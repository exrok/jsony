use crate::json::DecodeError;
mod from_impl;

use core::str;
use std::borrow::Cow;

#[derive(Clone)]
pub struct Ctx<'a> {
    // This is always a str, but storing as &str and then converting
    // to `as_bytes()` reduces performance even in release mode.
    pub(crate) data: &'a [u8],
    pub(crate) error: Option<Cow<'a, str>>,
}
impl<'a> Ctx<'a> {
    #[inline(always)]
    pub fn as_bytes(&self) -> &'a [u8] {
        self.data
    }
    pub fn new(data: &'a str) -> Self {
        Self {
            data: data.as_bytes(),
            error: None,
        }
    }
    pub fn static_error(&mut self, err: &'static str) {
        if self.error.is_none() {
            self.error = Some(Cow::Borrowed(err));
        }
    }
    pub fn try_extend_lifetime(&self, text: &str) -> Option<&'a str> {
        if self.data.as_ptr_range().contains(&text.as_ptr()) {
            Some(unsafe { &*(text as *const str) })
        } else {
            None
        }
    }
}

pub trait FromText<'a>: Sized {
    fn from_text(ctx: &mut Ctx<'a>, text: &str) -> Result<Self, &'static DecodeError>;
}
