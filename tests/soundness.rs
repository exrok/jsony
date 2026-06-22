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
        unsafe {
            dest.cast::<WideFirst>().write(WideFirst {
                marker: 0,
                padding: 0,
            });
        }
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
        unsafe {
            dest.cast::<TrackedDrop>().write(TrackedDrop {
                initialized_at: ptr,
            });
        }
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
        unsafe {
            dest.cast::<ZstDrop>().write(ZstDrop);
        }
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
        unsafe {
            dest.cast::<OverAlignedZst>().write(OverAlignedZst);
        }
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

#[test]
fn vec_backed_byte_writer_preserves_prefix_view() {
    let mut backing = vec![1, 2];
    let mut writer = BytesWriter::from(&mut backing);
    assert_eq!(writer.buffer_slice(), &[1, 2]);
    writer.push(3);
    let slice = writer.into_backed_with_extended_slice();
    assert_eq!(slice, &[3]);
    assert_eq!(&backing[..], &[1, 2, 3]);
}

#[test]
fn vec_backed_text_writer_hides_non_utf8_prefix() {
    struct ObservesWriter;

    impl jsony::ToJson for ObservesWriter {
        type Kind = jsony::json::AnyValue;

        fn encode_json__jsony(&self, output: &mut jsony::TextWriter) -> Self::Kind {
            assert_eq!(output.as_str(), "");
            assert_eq!(output.buffer_slice(), "");
            output.push_str("null");
            jsony::json::AnyValue
        }
    }

    let mut output = Vec::with_capacity(1);
    output.push(0xff);

    let encoded = jsony::to_json_into(&ObservesWriter, &mut output);
    assert_eq!(encoded, "null");
    assert_eq!(&output[..], b"\xffnull");
}

#[test]
fn string_backed_text_writer_uses_suffix_view() {
    struct ObservesWriter;

    impl jsony::ToJson for ObservesWriter {
        type Kind = jsony::json::AnyValue;

        fn encode_json__jsony(&self, output: &mut jsony::TextWriter) -> Self::Kind {
            assert_eq!(output.as_str(), "");
            output.push_str("true");
            jsony::json::AnyValue
        }
    }

    let mut output = String::from("prefix:");
    let encoded = jsony::to_json_into(&ObservesWriter, &mut output);
    assert_eq!(encoded, "true");
    assert_eq!(output, "prefix:true");
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
        unsafe {
            dest.cast::<BoxedField>().write(BoxedField(Box::new(value)));
        }
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

/// A large, niche-bearing struct: the `Box` first field makes `BigNiche`
/// non-null, so `Option<BigNiche>` is niche-optimized to the same size, and the
/// trailing array makes the elided copy large. Parsing `Some` drives the niche
/// fast path in `Option<T>::emplace_from_json` (emplace `T` directly into
/// `dest`, then read-back-and-rewrap). The owned `Box` makes any leak or
/// double-free on that path observable under Miri/ASAN.
#[derive(Jsony, Debug, PartialEq)]
struct BigNiche {
    tag: Box<u32>,
    data: [u64; 8],
}

#[test]
fn option_niche_some_path_round_trips() {
    // Confirm the optimization's precondition actually holds, i.e. the test
    // exercises the fast path rather than the `MaybeUninit<T>` fallback.
    assert_eq!(size_of::<Option<BigNiche>>(), size_of::<BigNiche>());
    assert_eq!(size_of::<Option<Box<u32>>>(), size_of::<Box<u32>>());
    assert_eq!(size_of::<Option<&str>>(), size_of::<&str>());

    let big =
        jsony::from_json::<Option<BigNiche>>(r#"{"tag": 7, "data": [0, 1, 2, 3, 4, 5, 6, 7]}"#)
            .unwrap();
    assert_eq!(
        big,
        Some(BigNiche {
            tag: Box::new(7),
            data: [0, 1, 2, 3, 4, 5, 6, 7],
        })
    );
    assert_eq!(jsony::from_json::<Option<BigNiche>>("null").unwrap(), None);

    let boxed = jsony::from_json::<Option<Box<u32>>>("5").unwrap();
    assert_eq!(boxed, Some(Box::new(5)));
    assert_eq!(jsony::from_json::<Option<Box<u32>>>("null").unwrap(), None);

    let borrowed = jsony::from_json::<Option<&str>>(r#""hello""#).unwrap();
    assert_eq!(borrowed, Some("hello"));
    assert_eq!(jsony::from_json::<Option<&str>>("null").unwrap(), None);
}

/// The niche `Some` path must drop the inner heap allocation exactly once. A
/// successfully parsed `Option<BigNiche>` that is then dropped exercises the
/// move-out/write-back ownership of the read-back-and-rewrap step.
#[test]
fn option_niche_some_path_drops_exactly_once() {
    {
        let big = jsony::from_json::<Option<BigNiche>>(
            r#"{"tag": 99, "data": [9, 9, 9, 9, 9, 9, 9, 9]}"#,
        )
        .unwrap();
        assert_eq!(*big.as_ref().unwrap().tag, 99);
    }
    // Inner field that fails after the niche field is built must not leak.
    #[derive(Jsony)]
    struct Wrapper {
        #[allow(dead_code)]
        a: Option<BigNiche>,
        #[allow(dead_code)]
        b: u32,
    }
    assert!(jsony::from_json::<Wrapper>(r#"{"a":{"tag":1,"data":[0,0,0,0,0,0,0,0]},"b""#).is_err());
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
