use crate::json::DecodeError;
mod from_impl;

use core::str;
use std::borrow::Cow;

/// A parsing context for UTF-8 input strings that can store error messages.
///
/// This context maintains a reference to the input data and optionally stores
/// error information during parsing operations.
#[derive(Clone)]
pub struct Ctx<'a> {
    // Note: This is always valid UTF-8, but stored as &[u8] for performance.
    // Converting from &str and using as_bytes() reduces performance even in release mode.
    pub(crate) input: &'a [u8],
    pub(crate) error: Option<Cow<'a, str>>,
}

impl<'a> Ctx<'a> {
    /// Creates a new parsing context from the given input string.
    pub fn new(input: &'a str) -> Self {
        Self {
            input: input.as_bytes(),
            error: None,
        }
    }

    /// Returns a byte slice containing the entire input.
    pub fn as_bytes(&self) -> &'a [u8] {
        self.input
    }

    /// Returns a string slice containing the entire input.
    #[inline]
    pub fn as_str(&self) -> &'a str {
        // Safety: input is guaranteed to be valid UTF-8 by construction
        unsafe { str::from_utf8_unchecked(self.input) }
    }

    /// Records an error message if no error is currently stored.
    pub fn static_error(&mut self, err: &'static str) {
        if self.error.is_none() {
            self.error = Some(Cow::Borrowed(err));
        }
    }

    /// Returns the current error message, if any.
    pub fn current_error(&self) -> Option<&str> {
        self.error.as_deref()
    }

    /// Attempts to extend the lifetime of a text fragment if it belongs to the input buffer.
    ///
    /// ### Example
    /// ```
    /// # use jsony::text::Ctx;
    /// let input = String::from("Hello, World!");
    /// let ctx = Ctx::new(&input);
    /// let slice = &input[0..5];
    ///
    /// if let Some(extended) = ctx.try_extend_lifetime(slice) {
    ///     assert_eq!(extended, "Hello");
    /// }
    /// ```
    pub fn try_extend_lifetime(&self, text: &str) -> Option<&'a str> {
        if self.input.as_ptr_range().contains(&text.as_ptr()) {
            Some(unsafe { &*(text as *const str) })
        } else {
            None
        }
    }
}

/// A trait for parsing values from string slices with lifetime-aware borrowing.
///
/// Similar to [`std::str::FromStr`] but with two key differences:
/// - Enables borrowing from the input string
/// - Uses a fixed error type
///
/// Implementations should maintain semantics similar to [`std::str::FromStr`] where possible.
pub trait FromText<'a>: Sized {
    /// Parses a string slice to create a value of this type.
    ///
    /// ### Errors
    /// On failure, returns a static reference to a [`DecodeError`]. Additional error
    /// information may be available through `ctx.current_error()`.
    fn from_text(ctx: &mut Ctx<'a>, text: &str) -> Result<Self, &'static DecodeError>;
}
