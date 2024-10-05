#![allow(unused)]
use std::{
    borrow::Cow,
    collections::{HashMap, HashSet},
    ptr::NonNull,
    rc::Rc,
    sync::Arc,
};
#[doc(hidden)]
pub mod __internal;
pub mod binary;
pub mod json;
mod lazy_parser;
pub mod parser;
pub mod strings;
pub mod value;
use binary::{Decoder, FromBinaryError};
use json::DecodeError;
use parser::Parser;

pub use jsony_macros::{append_object, array, object, Jsony};

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
    fn binary_encode(&self, encoder: &mut Vec<u8>);

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

pub(crate) mod __private {
    pub trait Sealed {}
}
pub trait OutputBuffer: __private::Sealed {
    #[doc(hidden)]
    fn from_builder(builder: &mut json::RawBuf) -> &mut Self;
    #[doc(hidden)]
    fn terminate(&mut self);
}

pub trait ToJson {
    type Into: OutputBuffer;
    fn jsonify_into(&self, output: &mut Self::Into);
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
            Err(err) => {
                // todo add drop
                println!("Error At: {:?}", parser);
                Err(err)
            }
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
    let mut buf = json::RawBuf::new();
    buf.value(value);
    buf.string
}

pub fn append_json<T: ?Sized + ToJson>(value: &T, output: &mut String) {
    let mut buf = unsafe { &mut *(output as *mut _ as *mut json::RawBuf) };
    buf.value(value);
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
    let mut encoder = Vec::new();
    value.binary_encode(&mut encoder);
    encoder
}
