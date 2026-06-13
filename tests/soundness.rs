use std::{
    ptr::NonNull,
    sync::atomic::{AtomicUsize, Ordering},
};

use jsony::{
    binary::Decoder, json::DecodeError, parser::Parser, BytesWriter, FromBinary, FromJson, ToBinary,
};

#[repr(align(16))]
#[allow(dead_code)]
struct WideFirst {
    marker: usize,
    padding: usize,
}

unsafe impl<'a> FromJson<'a> for WideFirst {
    unsafe fn emplace_from_json(
        dest: NonNull<()>,
        parser: &mut Parser<'a>,
    ) -> Result<(), &'static DecodeError> {
        parser.skip_value()?;
        dest.cast::<WideFirst>().write(WideFirst {
            marker: 0,
            padding: 0,
        });
        Ok(())
    }
}

struct TrackedDrop {
    initialized_at: *const TrackedDrop,
}

unsafe impl<'a> FromJson<'a> for TrackedDrop {
    unsafe fn emplace_from_json(
        dest: NonNull<()>,
        parser: &mut Parser<'a>,
    ) -> Result<(), &'static DecodeError> {
        parser.skip_value()?;
        let ptr = dest.cast::<TrackedDrop>().as_ptr();
        dest.cast::<TrackedDrop>().write(TrackedDrop {
            initialized_at: ptr,
        });
        Ok(())
    }
}

impl Drop for TrackedDrop {
    fn drop(&mut self) {
        assert_eq!(self.initialized_at, self as *const TrackedDrop);
    }
}

static LIVE_ZSTS: AtomicUsize = AtomicUsize::new(0);

struct ZstDrop;

unsafe impl<'a> FromJson<'a> for ZstDrop {
    unsafe fn emplace_from_json(
        dest: NonNull<()>,
        parser: &mut Parser<'a>,
    ) -> Result<(), &'static DecodeError> {
        parser.skip_value()?;
        LIVE_ZSTS.fetch_add(1, Ordering::SeqCst);
        dest.cast::<ZstDrop>().write(ZstDrop);
        Ok(())
    }
}

impl Drop for ZstDrop {
    fn drop(&mut self) {
        LIVE_ZSTS.fetch_sub(1, Ordering::SeqCst);
    }
}

#[test]
fn tuple2_error_drops_second_field_at_its_offset() {
    type Tuple = (WideFirst, TrackedDrop);
    assert_ne!(std::mem::offset_of!(Tuple, 1), 0);

    assert!(jsony::from_json::<Tuple>("[0, 0, 0]").is_err());
}

#[test]
fn tuple4_error_drops_later_field_at_its_offset() {
    type Tuple = (WideFirst, u8, u8, TrackedDrop);
    assert_ne!(std::mem::offset_of!(Tuple, 3), 0);

    assert!(jsony::from_json::<Tuple>("[0, 0, 0, 0, 0]").is_err());
}

#[test]
fn array_error_drops_initialized_zst_elements() {
    LIVE_ZSTS.store(0, Ordering::SeqCst);

    assert!(jsony::from_json::<[ZstDrop; 3]>("[0, 0]").is_err());

    assert_eq!(LIVE_ZSTS.load(Ordering::SeqCst), 0);
}

#[test]
fn short_binary_numeric_input_reports_eof() {
    assert!(jsony::from_binary::<u64>(&[1, 2, 3]).is_err());
}

#[test]
fn binary_pod_vec_length_overflow_reports_error() {
    let mut input = vec![255];
    input.extend_from_slice(&(1u64 << 63).to_le_bytes());

    assert!(jsony::from_binary::<Vec<u16>>(&input).is_err());
}

#[repr(align(8))]
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
struct PodZst;

unsafe impl ToBinary for PodZst {
    const POD: bool = true;

    fn encode_binary(&self, _encoder: &mut BytesWriter) {}
}

unsafe impl<'a> FromBinary<'a> for PodZst {
    const POD: bool = true;

    fn decode_binary(_decoder: &mut Decoder<'a>) -> Self {
        PodZst
    }
}

#[test]
fn borrowed_pod_zst_slice_uses_aligned_dangling_pointer() {
    let decoded = jsony::from_binary::<&[PodZst]>(&[3]).unwrap();

    assert_eq!(decoded.len(), 3);
    assert_eq!(decoded.as_ptr().addr() % align_of::<PodZst>(), 0);
}
