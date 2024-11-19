//! # Jsony
//! An ergonomic and performant (at runtime & compile time) serialization framework
//! for the following formats:
//! - [JSON](json) (with optional extensions: trailing commas, comments, unquoted keys)
//! - [Custom Binary Encoding](crate::binary)
//! - [x-www-form-urlencoded](from_form_urlencoded) (in progress)
//!
//! ### Decoding/Encoding JSON with strongly typed data structures
//!
//! The Jsony derive macro can automatically implement the encoding and decoding
//! for a number of formats.
//! ```
//! use jsony::Jsony;
//!
//! #[derive(Jsony)]
//! #[jsony(Json)]
//! struct Player {
//!     name: String,
//!     health: u32,
//!     inventory: Vec<String>
//! }
//!
//! fn main() -> Result<(), jsony::JsonError> {
//!     let input = r#"
//!         {
//!             "name": "Jimmy",
//!             "health": 100,
//!             "inventory": [
//!                 "Rock",
//!                 "Helmet"
//!             ]
//!         }"#;
//!
//!     let mut player: Player = jsony::from_json(input)?;
//!     player.health -= 10;
//!
//!     let output: String = jsony::to_json(&player);
//!
//!     println!("generated json:\n {}", output);
//!
//!     Ok(())
//! }
//! ```
//! Here we choose to implement `ToJson` and `FromJson` automatically via the `Json` attribute.
//!
//! When deriving a format trait, all fields in the struct/enum need to implement the same
//! trait or have an implementation specified by a field attribute.
//!
//! #### Feature Documentation
//! - [Jsony derive macro](crate::Jsony): Declaratively specify how your Rust types map to various formats.
//! - [Flexible JSON decoder](crate::JsonParserConfig): Enable extensions for comments, trailing comments and more.
//! - [JSON template macros](crate::object): Flexibly generate a JSON string directly.
//! - [Binary format](crate::binary): Encode data in a fast, compact binary representation.
//! - [Lazy JSON Parsing](crate::drill): Dynamically parse only what you need.
//! - [Compact loosely typed Value](crate::value::JsonItem): Represent arbitrary JSON values in memory.
//! - [Flexible encode destination](crate::to_json_into): Encode to a file or stack-allocating buffer.

#![allow(
    clippy::question_mark,
    reason = "? introduces extra code bloat slowing down compile times"
)]
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

mod third_party;

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

/// Templating macro for creating JSON Arrays
///
/// See [object] template macro for more details.
///
/// Works that sames `object!{}` except produces an array.
#[cfg(feature = "macros")]
pub use jsony_macros::array;
#[cfg(feature = "macros")]
pub use jsony_macros::{object, Jsony};
pub use text_writer::IntoTextWriter;

/// A trait for parsing a value from a compact binary representation.
///
/// # Safety
///
/// This trait is safe to implement if you only override `binary_decode` and leave
/// other methods as default. However, setting `POD = true` requires additional
/// safety considerations.
///
/// # Usage
///
/// Given a decoder, `binary_decode` will attempt to decode `Self` from the prefix
/// of the decoder. This method always returns a value, even in case of an error.
/// When an error occurs, a default value is returned. This approach is primarily
/// an optimization but can also be used to extract partial values when parsing
/// as a whole has failed.
///
/// # Error Handling
///
/// Errors are stored in the decoder rather than being returned directly by
/// `binary_decode`.
///
/// # Plain Old Data (POD)
///
/// Setting `POD = true` indicates that this type can be directly memory-copied
/// from the input and maintains the same representation as if it was encoded
/// field-by-field in the binary decoder.
///
/// ## Requirements for `POD = true`:
/// - Every bit must be valid for the type and layout.
/// - There must be no padding between fields.
/// - Fields must be laid out in the same order as they are encoded and decoded
///   by `binary_decode`.
pub unsafe trait FromBinary<'a>: Sized {
    /// Indicates whether this type is Plain Old Data (POD).
    const POD: bool = false;

    /// Decodes `Self` from the given decoder.
    fn binary_decode(decoder: &mut Decoder<'a>) -> Self;

    /// Hidden method for endian transformation.
    ///
    /// This method is only defined for non-little-endian targets.
    #[doc(hidden)]
    #[cfg(not(target_endian = "little"))]
    fn endian_transform(&mut self) {}
}

/// A trait for converting a value to a compact binary representation.
///
/// This trait is used to append the binary encoding of a type to the end of a provided encoder.
/// For more details on the specific binary implementation, see the `binary` module.
///
/// # Safety
///
/// If you use the default implementation (i.e., `POD = false`), this trait is always safe to implement.
/// When `POD` is set to `true`, it indicates that this type can be directly memory-copied
/// to the output, maintaining the same representation as if it were encoded field-by-field.
///
/// ## Constraints for `POD = true`:
///
/// 1. Every bit pattern must be valid for the type and layout.
/// 2. There must be no padding between fields.
/// 3. Fields must be laid out in the same order as they would be encoded and decoded via the `binary_encode` function.
///
/// # Notes
///
/// - The `binary_encode` function should always write a value, even in error cases.
///   There is no error return;
pub unsafe trait ToBinary {
    /// Indicates whether the type is Plain Old Data (POD).
    ///
    /// When `true`, the type can be safely memory-copied.
    /// Default is `false`.
    const POD: bool = false;

    /// Encodes the type into its binary representation.
    ///
    /// This function should append the binary encoding of `self` to the provided `encoder`.
    fn binary_encode(&self, encoder: &mut BytesWriter);

    /// Hidden method for endian transformation.
    ///
    /// This method is only available on non-little-endian targets.
    #[doc(hidden)]
    #[cfg(not(target_endian = "little"))]
    fn endian_transform(&mut self) {}
}

/// A trait for types that can be parsed from JSON.
///
/// Either `emplace_for_json` or `json_decode` should be implemented.
///
/// # Safety
///
/// This trait is unsafe to implement. Implementors must ensure that the
/// `emplace_from_json` method properly initializes the memory at `dest`
/// when it returns `Ok(())`.
pub unsafe trait FromJson<'a>: Sized + 'a {
    /// Parses a JSON value and writes it directly to the given memory location.
    /// If Ok(()) is returned `dest` is guaranteed to be initialized
    ///
    /// # Safety
    ///
    /// dest, must be a pointer to Self although possilibly uninitialized
    /// and be valid for writes.
    ///
    /// If this method returns `Ok(())`, it must have properly initialized the
    /// memory at `dest` with a valid instance of `Self`. There is no such
    /// constraint if an error is returned.
    ///
    /// # Arguments
    ///
    /// * `dest` - A pointer to the memory where the parsed value should be written.
    /// * `parser` - The JSON parser to read from.
    #[inline]
    unsafe fn emplace_from_json(
        dest: NonNull<()>,
        parser: &mut Parser<'a>,
    ) -> Result<(), &'static DecodeError> {
        match Self::json_decode(parser) {
            Ok(value) => {
                dest.cast::<Self>().write(value);
                Ok(())
            }
            Err(err) => Err(err),
        }
    }

    /// Decodes a JSON value from the parser.
    ///
    /// This method reads JSON data from the current position of the parser,
    /// ignoring anything after the parsed value. It may add error context
    /// to the parser on failure.
    ///
    /// # Arguments
    ///
    /// * `parser` - The JSON parser to read from.
    ///
    /// # Returns
    ///
    /// The parsed value if successful, or an error if parsing failed.
    #[inline]
    fn json_decode(parser: &mut Parser<'a>) -> Result<Self, &'static DecodeError> {
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

/// A trait for converting a value into JSON.
pub trait ToJson {
    /// Represents the kind of JSON value that will be produced.
    ///
    /// This can be a string, object, array, or any value that could be
    /// one of those or a scalar (e.g., number).
    type Kind: JsonValueKind;

    /// Converts `self` to JSON and appends it to the given `TextWriter`.
    /// Note: this method is prefixed to avoid collisions in macros that
    /// that invoke it via method resolution.
    #[allow(non_snake_case)]
    fn json_encode__jsony(&self, output: &mut TextWriter) -> Self::Kind;
}

/// Lazy JSON Parsing that has been validated
#[repr(transparent)]
pub struct RawJson {
    pub(crate) raw: str,
}

impl RawJson {
    pub fn as_str(&self) -> &str {
        &self.raw
    }
}

impl RawJson {
    pub(crate) fn new_unchecked(raw: &str) -> &RawJson {
        if raw.is_empty() {
            unsafe { &*("null" as *const str as *const RawJson) }
        } else {
            unsafe { &*(raw as *const str as *const RawJson) }
        }
    }
    pub(crate) fn new_boxed_unchecked(raw: Box<str>) -> Box<RawJson> {
        if raw.is_empty() {
            Self::new_boxed_unchecked("null".into())
        } else {
            unsafe { Box::from_raw(Box::into_raw(raw) as *mut RawJson) }
        }
    }
}

impl std::fmt::Debug for RawJson {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        (**self).fmt(f)
    }
}

impl std::ops::Deref for RawJson {
    type Target = MaybeJson;

    fn deref(&self) -> &Self::Target {
        unsafe { &*(self as *const RawJson as *const MaybeJson) }
    }
}

/// Lazy JSON Parsing that may not be valid json
#[repr(transparent)]
pub struct MaybeJson {
    /// We'll parsing the head of the string is a next json value (possibly with additional whitespace)
    // Error are represented by empty string with pointer tagging
    // (why? to work around std:ops::Index only returning references)
    pub(crate) raw: str,
}

/// # Lazy JSON parser
///
/// Interpret the given a string a JSON value to lazily parse. Particular useful,
/// when you need to extract a single deeply nested value out of larger JSON object.
///
///
/// See [LazyValue] for more info.
///
/// ## Example
/// ```
/// let object = jsony::drill(stringify![{
///     "key": {"inner": [0, false]},
/// }]);
/// let value: bool = object["key"]["inner"][1].parse().unwrap();
/// assert_eq!(value, false);
/// ```
pub fn drill(input: &str) -> &MaybeJson {
    MaybeJson::new(input)
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

/// The error type for JSON decoding failure with context.
///
/// See [DecodeError] for the contextless errors used during decoding.
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
    fn trailing() -> JsonError {
        JsonError {
            inner: Box::new(JsonErrorInner {
                error: &DecodeError {
                    message: "Trailing characters",
                },
                context: None,
                parent_context: JsonParentContext::None,
                index: 0,
                surrounding: [0; 24],
            }),
        }
    }

    pub(crate) fn new(error: &'static DecodeError, context: Option<String>) -> JsonError {
        JsonError {
            inner: Box::new(JsonErrorInner {
                error,
                context,
                parent_context: JsonParentContext::None,
                index: 0,
                surrounding: [0; 24],
            }),
        }
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
                context: parser.at.ctx.error.take().map(|x| x.to_string()),
                parent_context: parser.parent_context,
                index: parser.at.index,
                surrounding: surrounding(parser.at.index, parser.at.ctx.data),
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
        f.write_str(self.inner.error.message)?;
        if let Some(context) = &self.inner.context {
            f.write_str(": ")?;
            f.write_str(context)?;
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

/// Configuration options for the JSON parser.
#[derive(Clone, Copy)]
#[repr(align(8))]
pub struct JsonParserConfig {
    /// Maximum depth of nested structures (objects and arrays) allowed during parsing.
    /// Each non-empty object or array counts as one depth layer. Default is 128.
    pub recursion_limit: i32,

    /// When enabled, allows trailing commas in arrays and objects.
    /// A trailing comma can appear before the closing brace or bracket.
    pub allow_trailing_commas: bool,

    /// When enabled, allows C-style single-line comments in the JSON.
    /// Comments start with two forward slashes (//) and continue to the end of the line.
    pub allow_comments: bool,

    /// When enabled, allows unquoted strings for object keys that correspond to fields
    /// of rust types.
    /// Unquoted keys must be comprised of characters matching the following [A-Za-z0-9_]
    /// and cannot start with a number.
    pub allow_unquoted_field_keys: bool,

    /// When enabled, allows extra data to appear after the outermost JSON structure.
    /// This is primarily relevant when using the `from_json` function rather than
    /// interacting with the parser directly.
    pub allow_trailing_data: bool,
}

impl Default for JsonParserConfig {
    fn default() -> Self {
        Self {
            recursion_limit: 128,
            allow_trailing_commas: Default::default(),
            allow_comments: Default::default(),
            allow_unquoted_field_keys: Default::default(),
            allow_trailing_data: Default::default(),
        }
    }
}

/// Parses a value implementing `FromJson` from a JSON string.
/// # Example
///
/// ```rust
/// assert_eq!(jsony::from_json::<u32>("123")?, 123);
/// # Ok::<(), jsony::JsonError>(())
/// ```
/// # Errors
///
/// This function can fail if:
/// - The input string is not valid JSON.
/// - The JSON structure doesn't match the expected type `T`.
/// - The values in the JSON don't meet the constraints of type `T`.
///
/// # Notes
///
/// - This function uses strict JSON parsing by default, without any extensions.
/// - Unquoted strings, comments, and trailing commas will cause parsing to fail.
/// - There is a default recursion limit for parsing.
///
/// For more flexible parsing options, see `from_json_with_config`.
#[inline]
pub fn from_json<'a, T: FromJson<'a>>(json: &'a str) -> Result<T, JsonError> {
    from_json_with_config(
        json,
        JsonParserConfig {
            recursion_limit: 128,
            allow_trailing_commas: false,
            allow_comments: false,
            allow_unquoted_field_keys: false,
            allow_trailing_data: false,
        },
    )
}

#[inline]
pub fn from_json_with_config<'a, T: FromJson<'a>>(
    json: &'a str,
    config: JsonParserConfig,
) -> Result<T, JsonError> {
    unsafe fn inner_from_json<'a>(
        value: NonNull<()>,
        func: unsafe fn(NonNull<()>, &mut Parser<'a>) -> Result<(), &'static DecodeError>,
        json: &'a str,
        config: JsonParserConfig,
    ) -> Result<bool, JsonError> {
        let mut parser = Parser::new(json, config);
        #[cfg(not(feature = "json_comments"))]
        if config.allow_comments {
            panic!("jsony: 'json_comments' feature is not enabled but is required for `allow_comments`.")
        }
        match unsafe { func(value, &mut parser) } {
            Ok(()) => {
                let _ = parser.at.peek();
                Ok(parser.at.ctx.data.len() == parser.at.index || config.allow_trailing_data)
            }
            Err(err) => Err(JsonError::extract(err, &mut parser)),
        }
    }
    let mut value = std::mem::MaybeUninit::<T>::uninit();
    match unsafe {
        inner_from_json(
            NonNull::new_unchecked(value.as_mut_ptr()).cast(),
            T::emplace_from_json,
            json,
            config,
        )
    } {
        Ok(true) => Ok(unsafe { value.assume_init() }),
        Ok(false) => {
            unsafe {
                value.assume_init_drop();
            }
            Err(JsonError::trailing())
        }
        Err(err) => Err(err),
    }
}

/// Converts the given value into a JSON string representation.
///
/// This function takes a reference to any type `T` that implements the `ToJson` trait
/// and converts it into a JSON-formatted string.
///
/// # Examples
///
/// ```
/// use jsony::{Jsony, to_json};
///
/// #[derive(Jsony)]
/// #[jsony(ToJson)]
/// struct Person {
///     name: String,
///     age: u32,
/// }
///
/// let person = Person {
///     name: "Alice".to_string(),
///     age: 30,
/// };
///
/// let json_string = to_json(&person);
/// assert_eq!(json_string, r#"{"name":"Alice","age":30}"#);
/// ```
pub fn to_json<T: ?Sized + ToJson>(value: &T) -> String {
    let mut buf = TextWriter::new();
    value.json_encode__jsony(&mut buf);
    buf.into_string()
}

/// Converts the given value into a JSON string appending it to the provided output.
///
/// Can be more efficient then `to_json` when used it avoid allocations or
/// extra copies.
///
/// ## Examples
/// **Appending to a String:**
/// ```
/// let mut output = String::new();
/// assert_eq!(jsony::to_json_into(&false, &mut output), "false");
/// assert_eq!(jsony::to_json_into(&42u32, &mut output), "42");
/// assert_eq!(jsony::to_json_into(&None::<u32>, &mut output), "null");
/// assert_eq!(output, "false42null");
/// ```
/// **Avoiding heap allocation via a stack allocated buffer:**
/// ```
/// let mut temp = [std::mem::MaybeUninit::<u8>::uninit(); 32];
/// assert_eq!(jsony::to_json_into(&false, &mut temp[..]), "false");
/// assert_eq!(jsony::to_json_into(&42u32, &mut temp[..]), "42");
/// assert_eq!(jsony::to_json_into(&None::<u32>, &mut temp[..]), "null");
/// ```
/// Note: When the temp buffers capacity is exceeded a heap allocation will
/// be used hence the return type of `Cow<'a, [u8]>`.
///
/// **Writing to `std::io::Writer`:**
/// ```
/// let writer: &mut (dyn std::io::Write + Send) = &mut std::io::stdout();
/// assert_eq!(jsony::to_json_into(&false, writer)?, 5, "returns number of bytes written");
/// # Ok::<_, std::io::Error>(())
/// ```
pub fn to_json_into<'a, T: ToJson, W: IntoTextWriter<'a>>(value: &T, output: W) -> W::Output {
    let mut buffer = W::into_text_writer(output);
    value.json_encode__jsony(&mut buffer);
    W::finish_writing(buffer)
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
    form: &'a str,
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
