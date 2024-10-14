use crate::json::DecodeError;
// maximum number fields.
// currently 63 based on how we compute the bit masks.
pub const MAX_FIELDS: usize = 63;
pub(crate) mod form_urlencoded;
mod from_impl;

use std::borrow::Cow;

#[derive(Clone)]
pub struct Ctx<'a> {
    pub(crate) data: &'a [u8],
    pub(crate) error: Option<Cow<'a, str>>,
}
impl<'a> Ctx<'a> {
    pub fn new(data: &'a [u8]) -> Self {
        Self { data, error: None }
    }
    pub fn static_error(&mut self, err: &'static str) {
        if let None = self.error {
            self.error = Some(Cow::Borrowed(err));
        }
    }
    fn try_extend_lifetime(&self, text: &str) -> Option<&'a str> {
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

pub trait FromTextSequence<'a>: Sized {
    fn from_text_sequence(
        ctx: &mut Ctx<'a>,
        sequence: &[Option<&str>],
    ) -> Result<Self, &'static DecodeError>;
}

pub trait TextSequenceFieldNames: Sized {
    fn text_sequence_field_names() -> &'static [&'static str];
}

#[cfg(test)]
mod test {
    use form_urlencoded::FormDecoder;
    use std::mem::MaybeUninit;

    use super::*;
    #[test]
    fn form_decoder() {
        let mut fields = MaybeUninit::<[Option<&str>; 32]>::uninit();
        let mut decoder = FormDecoder::new(b"foo=hello&billy=nice");
        let lines = decoder
            .extract_named_fields(&mut fields, &["foo", "billy"])
            .unwrap();
        assert_eq!(lines, &[Some("hello"), Some("nice")]);
        //        let value = from_form::<Foo>(b"x=hello&value=nice").unwrap();
        //       assert_eq!(value.x, "hello");
        //      assert_eq!(value.value, "nice");
    }
}
