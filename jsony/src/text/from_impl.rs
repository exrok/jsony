use std::borrow::Cow;

use crate::json::DecodeError;

use super::{Ctx, FromText};

impl<'a> FromText<'a> for &'a str {
    fn from_text(ctx: &mut Ctx<'a>, text: &str) -> Result<Self, &'static DecodeError> {
        if let Some(text) = ctx.try_extend_lifetime(text) {
            Ok(text)
        } else {
            return Err(&DecodeError {
                message: "Text does not extend lifetime",
            });
        }
    }
}

static INVALID_NUMBER: DecodeError = DecodeError {
    message: "Invalid Number",
};

macro_rules! impl_from_text_from_str {
    ($($ty:ty),*) => {
        $(
        impl<'a> FromText<'a> for $ty {
            fn from_text(_ctx: &mut Ctx<'a>, text: &str) -> Result<Self, &'static DecodeError> {
                match text.parse::<$ty>() {
                    Ok(v) => Ok(v),
                    Err(_) => Err(&INVALID_NUMBER),
                }
            }
        })*
    };
}

impl_from_text_from_str! {
    u8,u16,u32,u64,u128,usize,
    i8,i16,i32,i64,i128,isize,
    f32,f64
}

impl<'a> FromText<'a> for char {
    fn from_text(_ctx: &mut Ctx<'a>, text: &str) -> Result<Self, &'static DecodeError> {
        match text.parse::<char>() {
            Ok(v) => Ok(v),
            Err(_) => Err(&DecodeError {
                message: "Invalid char",
            }),
        }
    }
}

impl<'a> FromText<'a> for Cow<'a, str> {
    fn from_text(ctx: &mut Ctx<'a>, text: &str) -> Result<Self, &'static DecodeError> {
        if let Some(text) = ctx.try_extend_lifetime(text) {
            Ok(Cow::Borrowed(text))
        } else {
            Ok(Cow::Owned(text.into()))
        }
    }
}

impl<'a> FromText<'a> for bool {
    fn from_text(_ctx: &mut Ctx<'a>, text: &str) -> Result<Self, &'static DecodeError> {
        if text.eq_ignore_ascii_case("true") || text == "1" {
            return Ok(true);
        } else if text.eq_ignore_ascii_case("false") || text == "0" {
            return Ok(false);
        } else {
            return Err(&DecodeError {
                message: "Invalid Boolean",
            });
        }
    }
}

impl<'a> FromText<'a> for Box<str> {
    fn from_text(_ctx: &mut Ctx<'a>, text: &str) -> Result<Self, &'static DecodeError> {
        Ok(text.into())
    }
}

impl<'a> FromText<'a> for String {
    fn from_text(_ctx: &mut Ctx<'a>, text: &str) -> Result<Self, &'static DecodeError> {
        Ok(text.into())
    }
}
