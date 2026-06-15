use std::{
    ptr::NonNull,
    sync::atomic::{AtomicUsize, Ordering},
};

use jsony::{
    BytesWriter, FromBinary, FromJson, Jsony, ToBinary, binary::Decoder, json::DecodeError,
    parser::Parser,
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

static LIVE_OVER_ALIGNED: AtomicUsize = AtomicUsize::new(0);

/// A zero-sized type with alignment greater than 1. Decoding `Vec<T>` for a ZST
/// goes through a type-erased path that only knows the element is zero-sized, so
/// it must still align the (otherwise dangling) data pointer to the element.
/// A misaligned pointer is UB the moment a `&mut T` is formed, e.g. on drop.
#[repr(align(8))]
struct OverAlignedZst;

unsafe impl<'a> FromJson<'a> for OverAlignedZst {
    unsafe fn emplace_from_json(
        dest: NonNull<()>,
        parser: &mut Parser<'a>,
    ) -> Result<(), &'static DecodeError> {
        parser.skip_value()?;
        LIVE_OVER_ALIGNED.fetch_add(1, Ordering::SeqCst);
        dest.cast::<OverAlignedZst>().write(OverAlignedZst);
        Ok(())
    }
}

impl Drop for OverAlignedZst {
    fn drop(&mut self) {
        LIVE_OVER_ALIGNED.fetch_sub(1, Ordering::SeqCst);
    }
}

#[test]
fn vec_of_over_aligned_zst_uses_aligned_pointer() {
    LIVE_OVER_ALIGNED.store(0, Ordering::SeqCst);
    {
        let v = jsony::from_json::<Vec<OverAlignedZst>>("[0, 0, 0]").unwrap();
        assert_eq!(v.len(), 3);
        assert_eq!(
            v.as_ptr().addr() % std::mem::align_of::<OverAlignedZst>(),
            0,
            "Vec data pointer is misaligned for the element type"
        );
    }
    assert_eq!(LIVE_OVER_ALIGNED.load(Ordering::SeqCst), 0);

    // The error path constructs and drops the partially built Vec, exercising
    // the same pointer.
    assert!(jsony::from_json::<Vec<OverAlignedZst>>("[0, 0, ").is_err());
    assert_eq!(LIVE_OVER_ALIGNED.load(Ordering::SeqCst), 0);
}

#[test]
fn byte_writer_clear_then_extended_slice_is_empty() {
    // Clearing a Vec-backed writer drops its length below the creation-time
    // length. The extended-slice length computation must not underflow and
    // build an `&[u8]` with a bogus, enormous length.
    let mut backing: Vec<u8> = vec![1, 2, 3, 4, 5, 6, 7, 8];
    let mut writer = BytesWriter::from(&mut backing);
    writer.push(b'x');
    writer.clear();
    let slice = writer.into_backed_with_extended_slice();
    assert!(slice.is_empty());
}

static LIVE_BOXED: AtomicUsize = AtomicUsize::new(0);

/// A heap-owning field whose live instances are counted. A value the decoder
/// fails to drop on an error path is observable two ways: `LIVE_BOXED` stays
/// non-zero, and the owned box leaks under miri/ASAN.
struct BoxedField(#[allow(dead_code)] Box<u32>);

unsafe impl<'a> FromJson<'a> for BoxedField {
    unsafe fn emplace_from_json(
        dest: NonNull<()>,
        parser: &mut Parser<'a>,
    ) -> Result<(), &'static DecodeError> {
        let value = u32::decode_json(parser)?;
        LIVE_BOXED.fetch_add(1, Ordering::SeqCst);
        dest.cast::<BoxedField>().write(BoxedField(Box::new(value)));
        Ok(())
    }
}

impl Drop for BoxedField {
    fn drop(&mut self) {
        LIVE_BOXED.fetch_sub(1, Ordering::SeqCst);
    }
}

/// When a container fails to parse after a heap-owning field is initialized, the
/// partially built value must be dropped. This covers every container shape: a
/// plain struct and the external, internal, and adjacent enum tag encodings.
/// Each input fully parses the inner field, then leaves the enclosing object
/// unterminated so the parse fails with the field already live.
#[test]
fn truncated_container_drops_partially_built_value() {
    #[derive(Jsony)]
    struct Plain {
        #[allow(dead_code)]
        a: BoxedField,
        #[allow(dead_code)]
        b: u32,
    }

    #[derive(Jsony)]
    #[allow(dead_code)]
    enum External {
        V0(BoxedField),
    }

    #[derive(Jsony)]
    #[jsony(tag = "kind")]
    enum Internal {
        V0 {
            #[allow(dead_code)]
            a: BoxedField,
        },
    }

    #[derive(Jsony)]
    #[jsony(tag = "kind", content = "data")]
    #[allow(dead_code)]
    enum Adjacent {
        V0(BoxedField),
    }

    LIVE_BOXED.store(0, Ordering::SeqCst);
    assert!(jsony::from_json::<Plain>(r#"{"a":1,"b":7"#).is_err());
    assert!(jsony::from_json::<External>(r#"{"V0":1"#).is_err());
    assert!(jsony::from_json::<Internal>(r#"{"kind":"V0","a":1"#).is_err());
    assert!(jsony::from_json::<Adjacent>(r#"{"kind":"V0","data":1"#).is_err());
    assert_eq!(
        LIVE_BOXED.load(Ordering::SeqCst),
        0,
        "decoder leaked a partially built value"
    );
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
