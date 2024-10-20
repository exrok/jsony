use std::mem::MaybeUninit;
use std::ptr::NonNull;
#[doc(hidden)]
pub mod __internal;
pub mod binary;
mod byte_writer;
pub mod json;
mod lazy_parser;
pub mod parser;
pub mod strings;
pub mod text;
mod text_writer;
pub use byte_writer::BytesWriter;
use text::FromTextSequence;
use text::TextSequenceFieldNames;
pub use text_writer::TextWriter;
pub mod value;
use binary::{Decoder, FromBinaryError};
use json::DecodeError;
use json::JsonValueKind;
use parser::Parser;

pub use jsony_macros::{array, object, Jsony};
pub use text_writer::TextSink;

/// If `POD` is `false` then this trait is safe to implement
/// `POD` = true implies that `Self` is valid for all bit
/// patterns and has no padding.
pub unsafe trait FromBinary<'a>: Sized {
    const POD: bool = false;

    fn binary_decode(decoder: &mut Decoder<'a>) -> Self;

    #[doc(hidden)]
    #[cfg(not(target_endian = "little"))]
    fn endian_transform(&mut self) {}
}

pub unsafe trait ToBinary {
    const POD: bool = false;
    fn binary_encode(&self, encoder: &mut BytesWriter);

    #[doc(hidden)]
    #[cfg(not(target_endian = "little"))]
    fn endian_transform(&mut self) {}
}

pub unsafe trait FromJson<'a>: Sized + 'a {
    /// Safety to Call:
    /// - dest: must be an aligned pointer to self which is valid for writes.
    /// # Safety
    /// to implemented:
    /// - If `Ok(())` is returned then `dest` must be initilized to a valid Self
    #[inline]
    unsafe fn emplace_from_json(
        dest: NonNull<()>,
        parser: &mut Parser<'a>,
    ) -> Result<(), &'static DecodeError> {
        match Self::decode_json(parser) {
            Ok(value) => {
                dest.cast::<Self>().write(value);
                Ok(())
            }
            Err(err) => Err(err),
        }
    }

    // Allows users to relatively safety
    #[inline]
    fn decode_json(parser: &mut Parser<'a>) -> Result<Self, &'static DecodeError> {
        let mut value = std::mem::MaybeUninit::<Self>::uninit();
        if let Err(err) = unsafe {
            Self::emplace_from_json(NonNull::new_unchecked(value.as_mut_ptr()).cast(), parser)
        } {
            Err(err)
        } else {
            Ok(unsafe { value.assume_init() })
        }
    }
}

mod __private {
    pub trait Sealed {}
}

pub trait ToJson {
    type Kind: JsonValueKind;
    /// The Kind is returned to allow for trivial assertions of kind
    /// inside of the templating macros that also produce good error messages.
    fn jsonify_into(&self, output: &mut TextWriter) -> Self::Kind;
}

/// Lazy JSON parser
pub fn drill(input: &str) -> &lazy_parser::Value {
    lazy_parser::Value::new(input)
}

#[inline]
pub fn from_json<'a, T: FromJson<'a>>(json: &'a str) -> Result<T, &'static DecodeError> {
    unsafe fn inner_from_json<'a>(
        value: NonNull<()>,
        func: unsafe fn(NonNull<()>, &mut Parser<'a>) -> Result<(), &'static DecodeError>,
        json: &'a [u8],
    ) -> Result<(), &'static DecodeError> {
        let mut parser = Parser::new(json);
        match unsafe { func(value, &mut parser) } {
            Ok(()) => Ok(()),
            Err(err) => Err(err),
        }
    }
    let mut value = std::mem::MaybeUninit::<T>::uninit();
    match unsafe {
        inner_from_json(
            NonNull::new_unchecked(value.as_mut_ptr()).cast(),
            T::emplace_from_json,
            json.as_bytes(),
        )
    } {
        Ok(()) => Ok(unsafe { value.assume_init() }),
        Err(err) => Err(err),
    }
}

pub fn to_json<T: ?Sized + ToJson>(value: &T) -> String {
    let mut buf = TextWriter::new();
    value.jsonify_into(&mut buf);
    buf.into_string()
}

pub fn to_json_into<'a, T: ToJson, O: TextSink<'a>>(value: &T, output: O) -> O::Output {
    let mut buffer = O::buffer(output);
    value.jsonify_into(&mut buffer);
    O::finish(buffer)
}

pub fn from_binary<'a, T: FromBinary<'a>>(slice: &'a [u8]) -> Result<T, FromBinaryError> {
    let mut decoder = Decoder::new(slice);
    let value = Ok(T::binary_decode(&mut decoder));
    if let Some(error) = decoder.consume_error() {
        Err(error)
    } else {
        value
    }
}

pub fn to_binary<T: ToBinary + ?Sized>(value: &T) -> Vec<u8> {
    let mut encoder = BytesWriter::new();
    value.binary_encode(&mut encoder);
    encoder.into_vec()
}

pub fn from_form_urlencoded<'a, T: TextSequenceFieldNames + FromTextSequence<'a>>(
    form: &'a [u8],
) -> Result<T, &'static DecodeError> {
    let mut decoder = text::form_urlencoded::FormDecoder::new(form);
    let mut fields = MaybeUninit::<[Option<&str>; 32]>::uninit();
    let Ok(lines) = decoder.extract_named_fields(&mut fields, T::text_sequence_field_names())
    else {
        return Err(&DecodeError {
            message: "Expected larger",
        });
    };
    T::from_text_sequence(&mut decoder.ctx, lines)
}
