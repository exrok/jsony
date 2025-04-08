use crate::error::*;
use std::borrow::Cow;
use std::fmt;
use std::mem::MaybeUninit;
use std::ops::Range;
use std::ptr::NonNull;

use crate::__internal::ObjectSchemaInner;
use crate::json::DecodeError;
use crate::strings::{skip_json_string_and_eq, skip_json_string_and_validate};
use crate::text::Ctx;
use crate::{FromJson, JsonError, JsonParserConfig};

#[derive(Copy, Clone, PartialEq, Eq)]
pub struct Peek(u8);

#[allow(non_upper_case_globals)] // while testing
impl Peek {
    pub const Null: Self = Self(b'n');
    pub const True: Self = Self(b't');
    pub const False: Self = Self(b'f');
    pub const Minus: Self = Self(b'-');
    pub const Infinity: Self = Self(b'I');
    pub const NaN: Self = Self(b'N');
    pub const String: Self = Self(b'"');
    pub const Array: Self = Self(b'[');
    pub const Object: Self = Self(b'{');
}

impl fmt::Debug for Peek {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self.0 {
            b'n' => write!(f, "Null"),
            b't' => write!(f, "True"),
            b'f' => write!(f, "False"),
            b'-' => write!(f, "Minus"),
            b'I' => write!(f, "Infinity"),
            b'N' => write!(f, "NaN"),
            b'"' => write!(f, "String"),
            b'[' => write!(f, "Array"),
            b'{' => write!(f, "Object"),
            _ => write!(f, "Peek({:?})", self.0 as char),
        }
    }
}

impl Peek {
    pub const fn new(next: u8) -> Self {
        Self(next)
    }

    pub const fn is_num(self) -> bool {
        self.0.is_ascii_digit() || matches!(self, Self::Minus | Self::Infinity | Self::NaN)
    }

    pub const fn into_inner(self) -> u8 {
        self.0
    }
}

static TRUE_REST: [u8; 3] = [b'r', b'u', b'e'];
static FALSE_REST: [u8; 4] = [b'a', b'l', b's', b'e'];
static NULL_REST: [u8; 3] = [b'u', b'l', b'l'];

#[derive(Clone, Copy)]
pub(crate) enum JsonParentContext {
    None,
    ObjectKey(&'static str),
    Schema {
        schema: &'static ObjectSchemaInner,
        mask: u64,
    },
}
#[derive(Clone)]
pub struct InnerParser<'j> {
    pub ctx: Ctx<'j>,
    pub index: usize,
    pub config: JsonParserConfig,
}

#[derive(Clone)]
pub struct Parser<'j> {
    pub at: InnerParser<'j>,
    pub(crate) parent_context: JsonParentContext,
    pub scratch: Vec<u8>,
}

impl fmt::Debug for Parser<'_> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        writeln!(
            f,
            "Remaining: {}",
            &self.at.ctx.input[self.at.index..].escape_ascii()
        )
    }
}
type JsonResult<T> = Result<T, &'static DecodeError>;

static EOF_WHILE_PARSING_LIST_FIRST_ELEMENT: DecodeError = DecodeError {
    message: "EOF while parsing a list first element",
};
static EOF_WHILE_PARSING_LIST: DecodeError = DecodeError {
    message: "EOF while parsing a list",
};

static EXPECTED_LIST_COMMA_OR_END: DecodeError = DecodeError {
    message: "Expected list comma or end",
};

static KEY_MUST_BE_A_STRING: DecodeError = DecodeError {
    message: "Key must be a string",
};
static TODO_ERROR: DecodeError = DecodeError {
    message: "Todo Error",
};

static TRAILING_CHARACTERS: DecodeError = DecodeError {
    message: "Trailing characters",
};
pub static TRAILING_COMMA: DecodeError = DecodeError {
    message: "Trailing Comma",
};

static EXPECTED_STRING: DecodeError = DecodeError {
    message: "Expected string",
};

static EXPECTED_OBJECT_COMMA_OR_END: DecodeError = DecodeError {
    message: "Expected list comma or end",
};

pub(crate) static EOF_WHILE_PARSING_VALUE: DecodeError = DecodeError {
    message: "EOF while parsing value",
};
static EOF_WHILE_PARSING_OBJECT: DecodeError = DecodeError {
    message: "EOF while parsing object",
};

pub(crate) static EOF_WHILE_PARSING_STRING: DecodeError = DecodeError {
    message: "EOF while parsing string",
};
static EXPECTED_COLON: DecodeError = DecodeError {
    message: "Expected colon after key",
};

static EXPECTED_SOME_IDENT: DecodeError = DecodeError {
    message: "Expected some ident",
};

static EXPECTED_NUMERIC_KEY_TO_ONLY_CONTAIN_A_NUMBER: DecodeError = DecodeError {
    message: "Expected number key to only contain a number",
};
static EXPECTED_NUMBER_BUT_FOUND_STRING: DecodeError = DecodeError {
    message: "Expected a number but found a string",
};

static EXPECTED_OBJECT: DecodeError = DecodeError {
    message: "Expected an object",
};

fn extract_numeric_literal(
    bytes: &[u8],
    mut index: usize,
) -> Result<(&str, usize), &'static DecodeError> {
    while let Some(next) = bytes.get(index) {
        match next {
            b' ' | b'\r' | b'\t' | b'\n' => index += 1,
            ch => {
                if *ch == b'"' {
                    return Err(&EXPECTED_NUMBER_BUT_FOUND_STRING);
                } else {
                    break;
                }
            }
        }
    }

    let mark = index;
    while index < bytes.len() {
        match bytes[index] {
            b'0'..=b'9' | b'+' | b'-' | b'.' | b'e' | b'E' => {}
            _ => {
                return Ok((
                    unsafe { std::str::from_utf8_unchecked(&bytes[mark..index]) },
                    index,
                ))
            }
        }
        index += 1;
    }

    Ok((
        unsafe { std::str::from_utf8_unchecked(&bytes[mark..index]) },
        index,
    ))
}
fn is_escape(ch: u8) -> bool {
    ch == b'"' || ch == b'\\' || ch < 0x20
}

pub struct RetrySnapshot {
    index: usize,
    recursion_depth: i32,
}

impl<'j> InnerParser<'j> {
    pub fn enter_seen_object_key_eq(&mut self, value: &str) -> JsonResult<Option<bool>> {
        debug_assert_eq!(self.ctx.input[self.index], b'{');
        self.index += 1;
        if let Some(next) = self.eat_whitespace() {
            match next {
                b'"' => {
                    self.config.recursion_limit -= 1;
                    if self.config.recursion_limit < 0 {
                        return Err(&RECURSION_LIMIT_EXCEEDED);
                    }
                    self.discard_and_eq_seen_object_key(value).map(Some)
                }
                b'}' => {
                    self.index += 1;
                    Ok(None)
                }

                _ => {
                    if self.config.allow_unquoted_field_keys {
                        self.config.recursion_limit -= 1;
                        if self.config.recursion_limit < 0 {
                            return Err(&RECURSION_LIMIT_EXCEEDED);
                        }
                        let key = self.read_seen_unquoted_object_key()?;
                        Ok(Some(key == value))
                    } else {
                        Err(&KEY_MUST_BE_A_STRING)
                    }
                }
            }
        } else {
            Err(&EOF_WHILE_PARSING_OBJECT)
        }
    }
    pub fn enter_seen_object_discarding_key(&mut self) -> JsonResult<Option<()>> {
        debug_assert_eq!(self.ctx.input[self.index], b'{');
        self.index += 1;
        if let Some(next) = self.eat_whitespace() {
            match next {
                b'"' => {
                    self.config.recursion_limit -= 1;
                    if self.config.recursion_limit < 0 {
                        return Err(&RECURSION_LIMIT_EXCEEDED);
                    }
                    self.discard_seen_object_key().map(Some)
                }
                b'}' => {
                    self.index += 1;
                    Ok(None)
                }

                _ => {
                    if self.config.allow_unquoted_field_keys {
                        self.config.recursion_limit -= 1;
                        if self.config.recursion_limit < 0 {
                            return Err(&RECURSION_LIMIT_EXCEEDED);
                        }
                        self.read_seen_unquoted_object_key()?;
                        Ok(Some(()))
                    } else {
                        Err(&KEY_MUST_BE_A_STRING)
                    }
                }
            }
        } else {
            Err(&EOF_WHILE_PARSING_OBJECT)
        }
    }
    pub fn enter_seen_object<'k, 's: 'k>(
        &mut self,
        scratch: &'s mut Vec<u8>,
    ) -> JsonResult<Option<&'k str>>
    where
        'j: 'k,
    {
        debug_assert_eq!(self.ctx.input[self.index], b'{');
        self.index += 1;
        if let Some(next) = self.eat_whitespace() {
            match next {
                b'"' => {
                    self.config.recursion_limit -= 1;
                    if self.config.recursion_limit < 0 {
                        return Err(&RECURSION_LIMIT_EXCEEDED);
                    }
                    unsafe { self.read_seen_object_key(scratch).map(Some) }
                }
                b'}' => {
                    self.index += 1;
                    Ok(None)
                }

                _ => {
                    if self.config.allow_unquoted_field_keys {
                        self.config.recursion_limit -= 1;
                        if self.config.recursion_limit < 0 {
                            return Err(&RECURSION_LIMIT_EXCEEDED);
                        }
                        self.read_seen_unquoted_object_key().map(Some)
                    } else {
                        Err(&KEY_MUST_BE_A_STRING)
                    }
                }
            }
        } else {
            Err(&EOF_WHILE_PARSING_OBJECT)
        }
    }

    pub fn enter_object<'k, 's: 'k>(
        &mut self,
        scratch: &'s mut Vec<u8>,
    ) -> JsonResult<Option<&'k str>>
    where
        'j: 'k,
    {
        if self.peek()? != Peek::Object {
            return Err(&EOF_WHILE_PARSING_OBJECT);
        }
        self.enter_seen_object(scratch)
    }

    pub fn object_step_at_key(&mut self) -> JsonResult<Option<()>> {
        if let Some(next) = self.eat_whitespace() {
            match next {
                b',' => {
                    self.index += 1;
                    match self.eat_whitespace() {
                        Some(b'"') => Ok(Some(())),
                        Some(b'}') => {
                            if self.config.allow_trailing_commas {
                                self.index += 1;
                                self.config.recursion_limit += 1;
                                Ok(None)
                            } else {
                                Err(&TRAILING_COMMA)
                            }
                        }
                        Some(_) => Err(&KEY_MUST_BE_A_STRING),
                        None => Err(&EOF_WHILE_PARSING_VALUE),
                    }
                }
                b'}' => {
                    self.index += 1;
                    self.config.recursion_limit += 1;
                    Ok(None)
                }
                _ => Err(&EXPECTED_OBJECT_COMMA_OR_END),
            }
        } else {
            Err(&EOF_WHILE_PARSING_OBJECT)
        }
    }

    pub fn read_seen_unquoted_object_key(&mut self) -> JsonResult<&'j str> {
        let start = self.index;
        while let Some(ch) = self.ctx.input.get(self.index) {
            if matches!(*ch, b'a'..=b'z' | b'A'..=b'Z' | b'0'..=b'9' | b'_') {
                self.index += 1;
                continue;
            }
            break;
        }
        if self.index == start {
            return Err(&DecodeError {
                message: "Invalid unquoted key",
            });
        }
        let end = self.index;
        if let Some(next) = self.eat_whitespace() {
            if next == b':' {
                self.index += 1;
                Ok(unsafe { std::str::from_utf8_unchecked(&self.ctx.input[start..end]) })
            } else {
                Err(&EXPECTED_COLON)
            }
        } else {
            Err(&EOF_WHILE_PARSING_OBJECT)
        }
    }

    pub fn discard_and_eq_object_step(&mut self, value: &str) -> JsonResult<Option<bool>> {
        if let Some(next) = self.eat_whitespace() {
            match next {
                b',' => {
                    self.index += 1;
                    match self.eat_whitespace() {
                        Some(b'"') => self.discard_and_eq_seen_object_key(value).map(Some),
                        Some(b'}') => {
                            if self.config.allow_trailing_commas {
                                self.index += 1;
                                self.config.recursion_limit += 1;
                                Ok(None)
                            } else {
                                Err(&TRAILING_COMMA)
                            }
                        }
                        Some(_) => {
                            if self.config.allow_unquoted_field_keys {
                                let key = self.read_seen_unquoted_object_key()?;
                                Ok(Some(key == value))
                            } else {
                                Err(&KEY_MUST_BE_A_STRING)
                            }
                        }
                        None => Err(&EOF_WHILE_PARSING_VALUE),
                    }
                }
                b'}' => {
                    self.index += 1;
                    self.config.recursion_limit += 1;
                    Ok(None)
                }
                _ => Err(&EXPECTED_OBJECT_COMMA_OR_END),
            }
        } else {
            Err(&EOF_WHILE_PARSING_OBJECT)
        }
    }
    pub fn discard_object_step(&mut self) -> JsonResult<Option<()>> {
        if let Some(next) = self.eat_whitespace() {
            match next {
                b',' => {
                    self.index += 1;
                    match self.eat_whitespace() {
                        Some(b'"') => self.discard_seen_object_key().map(Some),
                        Some(b'}') => {
                            if self.config.allow_trailing_commas {
                                self.index += 1;
                                self.config.recursion_limit += 1;
                                Ok(None)
                            } else {
                                Err(&TRAILING_COMMA)
                            }
                        }
                        Some(_) => {
                            if self.config.allow_unquoted_field_keys {
                                self.read_seen_unquoted_object_key()?;
                                Ok(Some(()))
                            } else {
                                Err(&KEY_MUST_BE_A_STRING)
                            }
                        }
                        None => Err(&EOF_WHILE_PARSING_VALUE),
                    }
                }
                b'}' => {
                    self.index += 1;
                    self.config.recursion_limit += 1;
                    Ok(None)
                }
                _ => Err(&EXPECTED_OBJECT_COMMA_OR_END),
            }
        } else {
            Err(&EOF_WHILE_PARSING_OBJECT)
        }
    }
    pub fn object_step<'k, 's: 'k>(
        &mut self,
        scratch: &'s mut Vec<u8>,
    ) -> JsonResult<Option<&'k str>>
    where
        'j: 'k,
    {
        if let Some(next) = self.eat_whitespace() {
            match next {
                b',' => {
                    self.index += 1;
                    match self.eat_whitespace() {
                        Some(b'"') => unsafe { self.read_seen_object_key(scratch).map(Some) },
                        Some(b'}') => {
                            if self.config.allow_trailing_commas {
                                self.index += 1;
                                self.config.recursion_limit += 1;
                                Ok(None)
                            } else {
                                Err(&TRAILING_COMMA)
                            }
                        }
                        Some(_) => {
                            if self.config.allow_unquoted_field_keys {
                                self.read_seen_unquoted_object_key().map(Some)
                            } else {
                                Err(&KEY_MUST_BE_A_STRING)
                            }
                        }
                        None => Err(&EOF_WHILE_PARSING_VALUE),
                    }
                }
                b'}' => {
                    self.index += 1;
                    self.config.recursion_limit += 1;
                    Ok(None)
                }
                _ => Err(&EXPECTED_OBJECT_COMMA_OR_END),
            }
        } else {
            Err(&EOF_WHILE_PARSING_OBJECT)
        }
    }
    pub fn try_zerocopy(&self, text: &str) -> Option<&'j str> {
        // todo
        unsafe {
            if self.ctx.input.as_ptr_range().contains(&text.as_ptr()) {
                Some(&*(text as *const str))
            } else {
                None
            }
        }
    }

    fn skip_to_escape(&mut self) {
        // Immediately bail-out on empty strings and consecutive escapes (e.g. \u041b\u0435)
        if self.index == self.ctx.input.len() || is_escape(self.ctx.input[self.index]) {
            return;
        }
        self.index += 1;

        let rest = &self.ctx.input[self.index..];

        // #[cfg(fast_arithmetic = "64")]
        type Chunk = u64;
        // #[cfg(fast_arithmetic = "32")]
        // type Chunk = u32;

        const STEP: usize = size_of::<Chunk>();
        const ONE_BYTES: Chunk = Chunk::MAX / 255; // 0x0101...01

        for chunk in rest.chunks_exact(STEP) {
            let chars = Chunk::from_le_bytes(chunk.try_into().unwrap());
            let contains_ctrl = chars.wrapping_sub(ONE_BYTES * 0x20) & !chars;
            let chars_quote = chars ^ (ONE_BYTES * Chunk::from(b'"'));
            let contains_quote = chars_quote.wrapping_sub(ONE_BYTES) & !chars_quote;
            let chars_backslash = chars ^ (ONE_BYTES * Chunk::from(b'\\'));
            let contains_backslash = chars_backslash.wrapping_sub(ONE_BYTES) & !chars_backslash;
            let masked = (contains_ctrl | contains_quote | contains_backslash) & (ONE_BYTES << 7);
            if masked != 0 {
                // SAFETY: chunk is in-bounds for slice
                self.index = unsafe { chunk.as_ptr().offset_from(self.ctx.input.as_ptr()) }
                    as usize
                    + masked.trailing_zeros() as usize / 8;
                return;
            }
        }

        self.index += rest.len() / STEP * STEP;
        self.skip_to_escape_slow();
    }
    #[cold]
    #[inline(never)]
    fn skip_to_escape_slow(&mut self) {
        while self.index < self.ctx.input.len() && !is_escape(self.ctx.input[self.index]) {
            self.index += 1;
        }
    }

    fn discard_and_eq_seen_object_key(&mut self, value: &str) -> JsonResult<bool> {
        debug_assert_eq!(self.ctx.input[self.index], b'"');
        let eq = self.discard_and_eq_seen_string(value)?;

        if let Some(next) = self.eat_whitespace() {
            if next == b':' {
                self.index += 1;
                Ok(eq)
            } else {
                Err(&EXPECTED_COLON)
            }
        } else {
            Err(&EOF_WHILE_PARSING_OBJECT)
        }
    }

    fn discard_seen_object_key(&mut self) -> JsonResult<()> {
        debug_assert_eq!(self.ctx.input[self.index], b'"');
        self.discard_seen_string()?;

        if let Some(next) = self.eat_whitespace() {
            if next == b':' {
                self.index += 1;
                Ok(())
            } else {
                Err(&EXPECTED_COLON)
            }
        } else {
            Err(&EOF_WHILE_PARSING_OBJECT)
        }
    }

    unsafe fn read_seen_object_key<'k, 's: 'k>(
        &mut self,
        scratch: &'s mut Vec<u8>,
    ) -> JsonResult<&'k str>
    where
        'j: 'k,
    {
        let key = unsafe { self.read_seen_string(scratch)? };

        if let Some(next) = self.eat_whitespace() {
            if next == b':' {
                self.index += 1;

                // todo split buffer and parser such that this hack is unneeded
                Ok(key)
            } else {
                Err(&EXPECTED_COLON)
            }
        } else {
            Err(&EOF_WHILE_PARSING_OBJECT)
        }
    }

    pub fn discard_seen_string<'k, 's: 'k>(&mut self) -> JsonResult<()> {
        debug_assert_eq!(self.ctx.input[self.index], b'"');
        self.index = skip_json_string_and_validate(self.index + 1, self.ctx.input)?;
        Ok(())
    }

    pub fn discard_and_eq_seen_string(&mut self, value: &str) -> JsonResult<bool> {
        debug_assert_eq!(self.ctx.input[self.index], b'"');
        let (after_index, is_equal) =
            skip_json_string_and_eq(self.index + 1, self.ctx.input, value.as_bytes())?;
        self.index = after_index;
        Ok(is_equal)
    }

    /// # Safety
    /// Parser must be at a valid stream typically this enforced by
    /// calling the function immediately after `Parser::peak(..) == Peek::String`
    pub unsafe fn read_seen_string<'k, 's: 'k>(
        &mut self,
        scratch: &'s mut Vec<u8>,
    ) -> JsonResult<&'k str>
    where
        'j: 'k,
    {
        debug_assert_eq!(self.ctx.input[self.index], b'"');
        self.index += 1;
        let mut start = self.index;
        scratch.clear();
        loop {
            self.skip_to_escape();
            if self.index == self.ctx.input.len() {
                return Err(&EOF_WHILE_PARSING_STRING);
            }
            // match unsafe { self.ctx.data.get_unchecked(self.indexl } {
            match self.ctx.input[self.index] {
                b'"' => {
                    if scratch.is_empty() {
                        let output = unsafe {
                            std::str::from_utf8_unchecked(&self.ctx.input[start..self.index])
                        };
                        self.index += 1;
                        return Ok(output);
                    } else {
                        scratch.extend_from_slice(&self.ctx.input[start..self.index]);
                        self.index += 1;
                        return Ok(unsafe { std::str::from_utf8_unchecked(&*scratch) });
                    }
                }
                b'\\' => {
                    scratch.extend_from_slice(&self.ctx.input[start..self.index]);
                    self.index += 1;
                    match crate::strings::parse_escape(self.index, self.ctx.input, scratch) {
                        Ok(index) => {
                            self.index = index;
                            start = index;
                        }
                        Err(()) => {
                            return Err(&DecodeError {
                                message: "Invalid escape",
                            })
                        }
                    }
                    continue;
                }
                _ => {
                    return Err(&DecodeError {
                        message: "Control character detected in json",
                    })
                }
            }
        }
    }

    pub fn enter_seen_object_at_first_key(&mut self) -> JsonResult<Option<()>> {
        debug_assert_eq!(
            self.ctx.input[self.index],
            b'{',
            "From: `{}`",
            self.ctx.input[self.index - 5..(self.index + 20).min(self.ctx.input.len())]
                .escape_ascii()
        );

        self.index += 1;
        if let Some(next) = self.eat_whitespace() {
            match next {
                b'"' => {
                    self.config.recursion_limit -= 1;
                    if self.config.recursion_limit < 0 {
                        return Err(&RECURSION_LIMIT_EXCEEDED);
                    }
                    Ok(Some(()))
                }
                b'}' => {
                    self.index += 1;
                    Ok(None)
                }
                _ => Err(&KEY_MUST_BE_A_STRING),
            }
        } else {
            Err(&EOF_WHILE_PARSING_OBJECT)
        }
    }

    pub fn skip_value(&mut self) -> JsonResult<()> {
        match self.peek()? {
            Peek::Array => {
                if self.enter_seen_array()?.is_some() {
                    loop {
                        self.skip_value()?;
                        if self.array_step()?.is_none() {
                            break;
                        }
                    }
                }
                Ok(())
            }
            Peek::Object => {
                if self.enter_seen_object_discarding_key()?.is_some() {
                    loop {
                        self.skip_value()?;
                        if self.discard_object_step()?.is_none() {
                            break;
                        }
                    }
                }

                Ok(())
            }
            Peek::True => self.discard_seen_true(),
            Peek::False => self.discard_seen_false(),
            Peek::Null => self.discard_seen_null(),
            Peek::String => self.discard_seen_string(),
            Peek::Minus | Peek(b'0'..b':') => {
                self.consume_numeric_literal()?;
                Ok(())
            }
            _ => Err(&TODO_ERROR),
        }
    }

    pub fn enter_array(&mut self) -> JsonResult<Option<Peek>> {
        if self.peek()? != Peek::Array {
            return Err(&EOF_WHILE_PARSING_LIST_FIRST_ELEMENT);
        }
        self.enter_seen_array()
    }

    pub fn array_step_expecting_more(&mut self) -> JsonResult<()> {
        if let Some(next) = self.eat_whitespace() {
            match next {
                b',' => {
                    self.index += 1;
                    let next = self.array_peek()?;
                    if next.is_none() {
                        Err(&TRAILING_COMMA)
                    } else {
                        Ok(())
                    }
                }
                b']' => {
                    self.index += 1;
                    Err(&DecodeError {
                        message: "Array length too short",
                    })
                }
                _ => Err(&EXPECTED_LIST_COMMA_OR_END),
            }
        } else {
            Err(&EOF_WHILE_PARSING_LIST)
        }
    }

    pub fn enter_seen_array(&mut self) -> JsonResult<Option<Peek>> {
        debug_assert_eq!(self.ctx.input[self.index], b'[');
        self.index += 1;
        if let Some(next) = self.eat_whitespace() {
            if next == b']' {
                self.index += 1;
                Ok(None)
            } else {
                self.config.recursion_limit -= 1;
                if self.config.recursion_limit < 0 {
                    return Err(&RECURSION_LIMIT_EXCEEDED);
                }
                Ok(Some(Peek::new(next)))
            }
        } else {
            Err(&EOF_WHILE_PARSING_LIST_FIRST_ELEMENT)
        }
    }

    pub fn peek(&mut self) -> JsonResult<Peek> {
        if let Some(next) = self.eat_whitespace() {
            Ok(Peek::new(next))
        } else {
            Err(&EOF_WHILE_PARSING_VALUE)
        }
    }

    pub fn array_step(&mut self) -> JsonResult<Option<Peek>> {
        if let Some(next) = self.eat_whitespace() {
            match next {
                b',' => {
                    self.index += 1;
                    let next = self.array_peek()?;
                    if next.is_none() {
                        if self.config.allow_trailing_commas {
                            self.index += 1;
                            self.config.recursion_limit += 1;
                            Ok(None)
                        } else {
                            Err(&TRAILING_COMMA)
                        }
                    } else {
                        Ok(next)
                    }
                }
                b']' => {
                    self.index += 1;
                    self.config.recursion_limit += 1;
                    Ok(None)
                }
                _ => Err(&EXPECTED_LIST_COMMA_OR_END),
            }
        } else {
            Err(&EOF_WHILE_PARSING_LIST)
        }
    }

    pub fn eat_whitespace(&mut self) -> Option<u8> {
        let Some(mut next) = self.ctx.input.get(self.index).copied() else {
            return None;
        };
        #[cfg(feature = "json_comments")]
        if next > b'/' {
            return Some(next);
        }
        #[cfg(not(feature = "json_comments"))]
        if next > b' ' {
            return Some(next);
        }
        loop {
            match next {
                b' ' | b'\r' | b'\t' | b'\n' => self.index += 1,
                #[cfg(feature = "json_comments")]
                b'/' if self.config.allow_comments => self.eat_comment(),
                _ => return Some(next),
            }
            if let Some(next2) = self.ctx.input.get(self.index) {
                next = *next2;
            } else {
                return None;
            }
        }
    }

    #[cfg(feature = "json_comments")]
    #[cold]
    fn eat_comment(&mut self) {
        self.index += 1;
        let Some(ch) = self.ctx.input.get(self.index) else {
            return;
        };
        if *ch == b'/' {
            self.index += 1;
            while let Some(&ch) = self.ctx.input.get(self.index) {
                self.index += 1;
                if ch == b'\n' {
                    return;
                }
            }
        }
        if *ch == b'*' {
            let mut index = self.index + 2;
            while let Some(ch) = self.ctx.input.get(index) {
                if *ch == b'*' {
                    if let Some(next) = self.ctx.input.get(index + 1) {
                        if *next == b'/' {
                            self.index = index + 2;
                            return;
                        }
                    }
                }
                index += 1;
            }
            self.index = self.ctx.input.len();
        }
    }
    pub fn discard_seen_true(&mut self) -> JsonResult<()> {
        debug_assert_eq!(
            self.ctx.input[self.index], b't',
            "invalid FromJson impl: discard_seen_true() called when not seen Peek::True"
        );
        self.consume_ident(TRUE_REST)
    }

    pub fn discard_seen_false(&mut self) -> JsonResult<()> {
        debug_assert_eq!(
            self.ctx.input[self.index], b'f',
            "invalid FromJson impl: discard_seen_false() called when not seen Peek::False"
        );
        self.consume_ident(FALSE_REST)
    }

    pub fn discard_seen_null(&mut self) -> JsonResult<()> {
        debug_assert_eq!(
            self.ctx.input[self.index], b'n',
            "invalid FromJson impl: discard_seen_null() called when not seen Peek::Null"
        );
        self.consume_ident(NULL_REST)
    }
    fn consume_ident<const SIZE: usize>(&mut self, expected: [u8; SIZE]) -> JsonResult<()> {
        self.index = consume_ident(self.ctx.input, self.index, expected)?;
        Ok(())
    }
    fn array_peek(&mut self) -> JsonResult<Option<Peek>> {
        if let Some(next) = self.eat_whitespace() {
            match next {
                b']' => Ok(None),
                _ => Ok(Some(Peek::new(next))),
            }
        } else {
            Err(&EOF_WHILE_PARSING_VALUE)
        }
    }

    pub fn index_array(&mut self, mut index: usize) -> JsonResult<bool> {
        let mut res = self.enter_array();
        loop {
            match res {
                Ok(Some(_)) => {
                    if index == 0 {
                        return Ok(true);
                    }
                    index -= 1;
                }
                Ok(None) => return Ok(false),
                Err(err) => return Err(err),
            }
            self.skip_value()?;
            res = self.array_step();
        }
    }

    pub fn index_object(&mut self, key: &str) -> JsonResult<bool> {
        if self.peek()? != Peek::Object {
            return Err(&EOF_WHILE_PARSING_OBJECT);
        }
        let mut res = self.enter_seen_object_key_eq(key);
        loop {
            match res {
                Ok(Some(true)) => return Ok(true),
                Ok(Some(false)) => (),
                Ok(None) => return Ok(false),
                Err(err) => return Err(err),
            }
            self.skip_value()?;
            res = self.discard_and_eq_object_step(key);
        }
    }

    pub fn discard_colon(&mut self) -> JsonResult<()> {
        if let Some(next) = self.eat_whitespace() {
            if next == b':' {
                self.index += 1;
                Ok(())
            } else {
                Err(&EXPECTED_COLON)
            }
        } else {
            Err(&EOF_WHILE_PARSING_OBJECT)
        }
    }

    pub fn discard_remaining_object_fields(&mut self) -> JsonResult<()> {
        loop {
            match self.discard_object_step() {
                Ok(Some(_)) => self.skip_value()?,
                Ok(None) => {
                    return Ok(());
                }
                Err(err) => {
                    return Err(err);
                }
            }
        }
    }

    pub fn tag_query_at_content_next_object<'k, 's: 'k>(
        &mut self,
        tag: &str,
        content: &str,
        scratch: &'s mut Vec<u8>,
    ) -> JsonResult<(&'k str, bool)>
    where
        'j: 'k,
    {
        if self.peek()? != Peek::Object {
            return Err(&EOF_WHILE_PARSING_OBJECT);
        }
        let mut content_index = None;
        let mut current_field_name = match self.enter_seen_object(scratch) {
            Ok(Some(name)) => name,
            Ok(None) => {
                return Err(&DecodeError {
                    message: "Missing tag field",
                })
            }
            Err(err) => return Err(err),
        };
        loop {
            if current_field_name == tag {
                match unsafe { self.read_seen_string(scratch) } {
                    Ok(value) => {
                        if let Some(content_index) = content_index {
                            self.index = content_index;
                            return Ok((value, true));
                        }
                        loop {
                            match self.discard_and_eq_object_step(content) {
                                Ok(Some(matches)) => {
                                    if matches {
                                        return Ok((value, true));
                                    }
                                    self.skip_value()?
                                }
                                Ok(None) => return Ok((value, false)),
                                Err(err) => {
                                    return Err(err);
                                }
                            }
                        }
                    }
                    Err(err) => {
                        return Err(err);
                    }
                }
            } else if current_field_name == content {
                content_index = Some(self.index);
            }
            self.skip_value()?;
            match self.object_step(scratch) {
                Ok(Some(name)) => {
                    current_field_name = name;
                }
                Ok(None) => {
                    return Err(&DecodeError {
                        message: "Missing tag field",
                    });
                }
                Err(err) => {
                    return Err(err);
                }
            }
        }
    }

    pub fn take_string<'k, 's: 'k>(&mut self, scratch: &'s mut Vec<u8>) -> JsonResult<&'k str>
    where
        'j: 'k,
    {
        if self.peek()? != Peek::String {
            return Err(&EXPECTED_STRING);
        }
        unsafe { self.read_seen_string(scratch) }
    }
    pub fn tag_query_next_object<'k, 's: 'k>(
        &mut self,
        tag: &str,
        scratch: &'s mut Vec<u8>,
    ) -> JsonResult<&'k str>
    where
        'j: 'k,
    {
        if self.peek()? != Peek::Object {
            return Err(&EOF_WHILE_PARSING_OBJECT);
        }
        let initial_index = self.index;
        let initial_remaining_depth = self.config.recursion_limit;
        let mut current_field_name = match self.enter_seen_object(scratch) {
            Ok(Some(name)) => name,
            Ok(None) => {
                return Err(&DecodeError {
                    message: "Missing tag field",
                })
            }
            Err(err) => return Err(err),
        };
        loop {
            if current_field_name == tag {
                match self.take_string(scratch) {
                    Ok(value) => {
                        self.index = initial_index;
                        self.config.recursion_limit = initial_remaining_depth;
                        return Ok(value);
                    }
                    Err(err) => {
                        return Err(err);
                    }
                }
            }
            match self.skip_value() {
                Ok(()) => {}
                Err(err) => {
                    return Err(err);
                }
            }
            match self.object_step(scratch) {
                Ok(Some(name)) => {
                    current_field_name = name;
                }
                Ok(None) => {
                    return Err(&DecodeError {
                        message: "Missing tag field",
                    });
                }
                Err(err) => {
                    return Err(err);
                }
            }
        }
    }

    pub fn consume_numeric_literal(&mut self) -> JsonResult<&'j str> {
        let (output, index) = extract_numeric_literal(self.ctx.input, self.index)?;
        self.index = index;
        Ok(output)
    }
}

impl<'j> Parser<'j> {
    pub fn decode_array_sequence<K: FromJson<'j>>(
        &mut self,
        mut func: impl FnMut(K) -> Result<(), &'static DecodeError>,
    ) -> Result<(), &'static DecodeError> {
        match self.at.enter_array() {
            Ok(Some(_)) => (),
            Ok(None) => return Ok(()),
            Err(err) => return Err(err),
        };
        loop {
            match func(K::decode_json(self)?) {
                Ok(()) => (),
                Err(err) => return Err(err),
            }
            match self.at.array_step() {
                Ok(Some(_)) => (),
                Ok(None) => return Ok(()),
                Err(err) => return Err(err),
            }
        }
    }
    pub fn decode_object_sequence<K: FromJson<'j>, V: FromJson<'j>>(
        &mut self,
        mut func: impl FnMut(K, V) -> Result<(), &'static DecodeError>,
    ) -> Result<(), &'static DecodeError> {
        unsafe {
            let mut key_temp = MaybeUninit::<K>::uninit();
            let mut value_temp = MaybeUninit::<V>::uninit();
            let (drop_key, result) = self.type_erased_decode_object_sequence(
                NonNull::new_unchecked(key_temp.as_mut_ptr()).cast(),
                NonNull::new_unchecked(value_temp.as_mut_ptr()).cast(),
                K::emplace_from_json,
                V::emplace_from_json,
                &mut |key, value| func(key.cast::<K>().read(), value.cast::<V>().read()),
            );
            if drop_key {
                key_temp.assume_init_drop();
            }
            result
        }
    }

    unsafe fn type_erased_decode_object_sequence(
        &mut self,
        key_ptr: NonNull<()>,
        val_ptr: NonNull<()>,
        key_from_json: unsafe fn(NonNull<()>, &mut Parser<'j>) -> Result<(), &'static DecodeError>,
        val_from_json: unsafe fn(NonNull<()>, &mut Parser<'j>) -> Result<(), &'static DecodeError>,
        func: &mut dyn FnMut(NonNull<()>, NonNull<()>) -> Result<(), &'static DecodeError>,
    ) -> (bool, Result<(), &'static DecodeError>) {
        match self.at.peek() {
            Ok(Peek::Object) => (),
            Ok(_) => return (false, Err(&EXPECTED_OBJECT)),
            Err(err) => return (false, Err(err)),
        }
        match self.at.enter_seen_object_at_first_key() {
            Ok(Some(())) => (),
            Ok(None) => return (false, Ok(())),
            Err(err) => return (false, Err(err)),
        }
        loop {
            match key_from_json(key_ptr, self) {
                Ok(value) => value,
                Err(err) => {
                    if std::ptr::addr_eq(err, &EXPECTED_NUMBER_BUT_FOUND_STRING) {
                        debug_assert_eq!(self.at.ctx.input[self.at.index], b'"');
                        self.at.index += 1;
                        match key_from_json(key_ptr, self) {
                            Ok(value) => {
                                if self.at.ctx.input.get(self.at.index) != Some(&b'"') {
                                    return (
                                        true,
                                        Err(&EXPECTED_NUMERIC_KEY_TO_ONLY_CONTAIN_A_NUMBER),
                                    );
                                }
                                self.at.index += 1;
                                value
                            }
                            Err(err) => return (true, Err(err)),
                        }
                    } else {
                        return (true, Err(err));
                    }
                }
            };
            if let Err(err) = self.at.discard_colon() {
                return (true, Err(err));
            }
            match val_from_json(val_ptr, self) {
                Ok(value) => value,
                Err(err) => return (true, Err(err)),
            };
            match func(key_ptr, val_ptr) {
                Ok(()) => (),
                Err(err) => return (true, Err(err)),
            }
            match self.at.object_step_at_key() {
                Ok(Some(())) => (),
                Ok(None) => return (false, Ok(())),
                Err(err) => return (true, Err(err)),
            }
        }
    }
    // Todo use following when doing speed optimized build.
    // pub fn decode_object_sequence<K: FromJson<'j>, V: FromJson<'j>>(
    //     &mut self,
    //     mut func: impl FnMut(K, V) -> Result<(), &'static DecodeError>,
    // ) -> Result<(), &'static DecodeError> {
    //     if self.at.peek()? != Peek::Object {
    //         return Err(&EXPECTED_OBJECT);
    //     }
    //     match self.at.enter_seen_object_at_first_key() {
    //         Ok(Some(())) => (),
    //         Ok(None) => return Ok(()),
    //         Err(err) => return Err(err),
    //     }
    //     loop {
    //         let key = match K::decode_json(self) {
    //             Ok(value) => value,
    //             Err(err) => {
    //                 if std::ptr::addr_eq(err, &EXPECTED_NUMBER_BUT_FOUND_STRING) {
    //                     debug_assert_eq!(self.at.ctx.input[self.at.index], b'"');
    //                     self.at.index += 1;
    //                     match K::decode_json(self) {
    //                         Ok(value) => {
    //                             if self.at.ctx.input.get(self.at.index) != Some(&b'"') {
    //                                 return Err(&EXPECTED_NUMERIC_KEY_TO_ONLY_CONTAIN_A_NUMBER);
    //                             }
    //                             self.at.index += 1;
    //                             value
    //                         }
    //                         Err(err) => return Err(err),
    //                     }
    //                 } else {
    //                     return Err(err);
    //                 }
    //             }
    //         };
    //         self.at.discard_colon()?;
    //         let value = match V::decode_json(self) {
    //             Ok(value) => value,
    //             Err(err) => return Err(err),
    //         };
    //         match func(key, value) {
    //             Ok(()) => (),
    //             Err(err) => return Err(err),
    //         }
    //         match self.at.object_step_at_key() {
    //             Ok(Some(())) => (),
    //             Ok(None) => return Ok(()),
    //             Err(err) => return Err(err),
    //         }
    //     }
    // }
    pub fn snapshot(&self) -> RetrySnapshot {
        RetrySnapshot {
            index: self.at.index,
            recursion_depth: self.at.config.recursion_limit,
        }
    }

    pub fn restore_for_retry(&mut self, snapshot: &RetrySnapshot) {
        self.at.index = snapshot.index;
        self.at.config.recursion_limit = snapshot.recursion_depth;
        self.parent_context = JsonParentContext::None;
        self.at.ctx.error = None;
    }

    pub fn new(data: &'j str, config: JsonParserConfig) -> Self {
        Self {
            at: InnerParser {
                ctx: Ctx::new(data),
                index: 0,
                config,
            },
            parent_context: JsonParentContext::None,
            scratch: Vec::new(),
        }
    }
    pub fn report_static_error(&mut self, error: &'j str) {
        self.at.ctx.error = Some(Cow::Borrowed(error));
    }
    pub fn report_error(&mut self, error: String) {
        self.at.ctx.error = Some(Cow::Owned(error));
    }
    pub fn clear_error(&mut self) {
        self.at.ctx.error = None;
    }

    pub fn try_zerocopy(&self, text: &str) -> Option<&'j str> {
        unsafe {
            if self.at.ctx.input.as_ptr_range().contains(&text.as_ptr()) {
                Some(&*(text as *const str))
            } else {
                None
            }
        }
    }

    pub fn next_value<T: FromJson<'j>>(&mut self) -> Option<Result<T, JsonError>> {
        if self.at.eat_whitespace().is_none() {
            return None;
        }
        match T::decode_json(self) {
            Ok(value) => Some(Ok(value)),
            Err(err) => {
                let error = JsonError::extract(err, self);
                Some(Err(error))
            }
        }
    }
    pub fn slice(&self, range: Range<usize>) -> Option<&[u8]> {
        self.at.ctx.input.get(range)
    }

    pub fn finish(&mut self) -> JsonResult<()> {
        if self.at.eat_whitespace().is_none() {
            Ok(())
        } else {
            Err(&TRAILING_CHARACTERS)
        }
    }

    pub fn take_string(&mut self) -> JsonResult<&str> {
        self.at.take_string(&mut self.scratch)
    }
    pub fn skip_value(&mut self) -> JsonResult<()> {
        self.at.skip_value()
    }
    pub fn peek(&mut self) -> JsonResult<Peek> {
        self.at.peek()
    }
    /// # Safety
    /// Parser must be at a valid stream typically this enforced by
    /// calling the function immediately after `Parser::peak(..) == Peek::String`
    pub unsafe fn read_seen_string(&mut self) -> JsonResult<&str> {
        unsafe { self.at.read_seen_string(&mut self.scratch) }
    }

    pub fn tag_query_next_object<'a>(&'a mut self, tag: &str) -> JsonResult<&'a str> {
        self.at.tag_query_next_object(tag, &mut self.scratch)
    }

    pub fn tag_query_at_content_next_object<'a>(
        &'a mut self,
        tag: &str,
        content: &str,
    ) -> JsonResult<(&'a str, bool)> {
        self.at
            .tag_query_at_content_next_object(tag, content, &mut self.scratch)
    }
    pub fn discard_remaining_object_fields(&mut self) -> JsonResult<()> {
        self.at.discard_remaining_object_fields()
    }
    pub fn object_step(&mut self) -> JsonResult<Option<&str>> {
        self.at.object_step(&mut self.scratch)
    }
    pub fn enter_object(&mut self) -> JsonResult<Option<&str>> {
        self.at.enter_object(&mut self.scratch)
    }
    pub fn enter_seen_object(&mut self) -> JsonResult<Option<&str>> {
        self.at.enter_seen_object(&mut self.scratch)
    }

    pub fn take_cow_string(&mut self) -> JsonResult<Cow<'j, str>> {
        let value = self.at.take_string(&mut self.scratch)?;
        if let Some(borrowed) = self.at.ctx.try_extend_lifetime(value) {
            Ok(Cow::Borrowed(borrowed))
        } else {
            let owned = value.to_string();
            Ok(Cow::Owned(owned))
        }
    }

    pub fn take_borrowed_string(&mut self) -> JsonResult<&'j str> {
        let value = self.at.take_string(&mut self.scratch)?;
        if let Some(borrowed) = self.at.ctx.try_extend_lifetime(value) {
            Ok(borrowed)
        } else {
            Err(&DecodeError {
                message: "Unexpected escape in string",
            })
        }
    }
}

fn consume_ident<const SIZE: usize>(
    data: &[u8],
    mut index: usize,
    expected: [u8; SIZE],
) -> JsonResult<usize> {
    match data.get(index + 1..=index + SIZE) {
        Some(s) if s == expected => Ok(index + SIZE + 1),
        // TODO very sadly iterating over expected cause extra branches in the generated assembly
        //   and is significantly slower than just returning an error
        _ => {
            index += 1;
            for c in &expected {
                match data.get(index) {
                    Some(v) if v == c => index += 1,
                    Some(_) => return Err(&EXPECTED_SOME_IDENT),
                    _ => break,
                }
            }
            Err(&EOF_WHILE_PARSING_LIST)
        }
    }
}
