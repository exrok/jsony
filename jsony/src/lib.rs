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
use parser::JsonParentContext;
use parser::MISSING_REQUIRED_FIELDS;
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
    fn jsony_to_json_into(&self, output: &mut TextWriter) -> Self::Kind;
}

/// Lazy JSON parser
pub fn drill(input: &str) -> &lazy_parser::Value {
    lazy_parser::Value::new(input)
}

struct JsonErrorInner {
    error: &'static DecodeError,
    context: Option<String>,
    parent_context: JsonParentContext,
    index: usize,
    surrounding: [u8; 24],
}
impl JsonErrorInner {
    fn near_by_input(&self) -> &[u8] {
        &self.surrounding[0..self.surrounding[23] as usize]
    }
}

pub struct JsonError {
    inner: Box<JsonErrorInner>,
}

impl JsonError {
    pub fn index(&self) -> usize {
        self.inner.index
    }
    pub fn decoding_error(&self) -> &'static DecodeError {
        self.inner.error
    }
    fn extract(error: &'static DecodeError, parser: &mut Parser) -> JsonError {
        fn surrounding(at: usize, text: &[u8]) -> [u8; 24] {
            let mut s: [u8; 24] = [0; 24];
            let end = (at + 12).min(text.len());
            let start = end.saturating_sub(23);
            let ctx = &text[start..end];
            s[23] = ctx.len() as u8;
            s[0..ctx.len()].copy_from_slice(ctx);
            s
        }
        JsonError {
            inner: Box::new(JsonErrorInner {
                error,
                context: parser.ctx.error.take().map(|x| x.to_string()),
                parent_context: parser.parent_context,
                index: parser.index,
                surrounding: surrounding(parser.index, parser.ctx.data),
            }),
        }
    }
}
impl std::error::Error for JsonError {}

impl std::fmt::Debug for JsonError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        <JsonError as std::fmt::Display>::fmt(self, f)
    }
}

impl std::fmt::Display for JsonError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.write_str(&self.inner.error.message)?;
        if let Some(context) = &self.inner.context {
            f.write_str(": ")?;
            f.write_str(&context)?;
        }

        match &self.inner.parent_context {
            JsonParentContext::ObjectKey(key) => {
                write!(f, " @ key {:?}", key)?;
            }
            JsonParentContext::Schema { schema, mask } => {
                if std::ptr::eq(self.inner.error, &MISSING_REQUIRED_FIELDS) {
                    write!(f, ": ")?;
                    let mut first = true;
                    for (index, field) in schema.fields.iter().enumerate() {
                        if mask & (1 << index) != 0 {
                            if !first {
                                f.write_str(", ")?;
                            }
                            first = false;
                            write!(f, "{:?}", field.name)?;
                        }
                    }
                    return Ok(());
                }
            }
            _ => (),
        }
        write!(f, " near `{}`", self.inner.near_by_input().escape_ascii())
    }
}

#[inline]
pub fn from_json<'a, T: FromJson<'a>>(json: &'a str) -> Result<T, JsonError> {
    unsafe fn inner_from_json<'a>(
        value: NonNull<()>,
        func: unsafe fn(NonNull<()>, &mut Parser<'a>) -> Result<(), &'static DecodeError>,
        json: &'a [u8],
    ) -> Result<(), JsonError> {
        let mut parser = Parser::new(json);
        match unsafe { func(value, &mut parser) } {
            Ok(()) => Ok(()),
            Err(err) => Err(JsonError::extract(err, &mut parser)),
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
    value.jsony_to_json_into(&mut buf);
    buf.into_string()
}

pub fn to_json_into<'a, T: ToJson, O: TextSink<'a>>(value: &T, output: O) -> O::Output {
    let mut buffer = O::buffer(output);
    value.jsony_to_json_into(&mut buffer);
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
