use std::borrow::Cow;
use std::fmt;
use std::ops::Range;

use crate::__internal::ObjectSchemaInner;
use crate::json::DecodeError;
use crate::text::Ctx;

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
pub struct Parser<'j> {
    pub ctx: Ctx<'j>,
    pub index: usize,
    pub(crate) parent_context: JsonParentContext,
    pub remaining_depth: i32,
    pub scratch: Vec<u8>,
}

impl<'j> fmt::Debug for Parser<'j> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        writeln!(
            f,
            "Remaining: {}",
            &self.ctx.data[self.index..].escape_ascii()
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
static TRAILING_COMMA: DecodeError = DecodeError {
    message: "Trailing Comma",
};

static EXPECTED_OBJECT_COMMA_OR_END: DecodeError = DecodeError {
    message: "Expected list comma or end",
};

static EOF_WHILE_PARSING_VALUE: DecodeError = DecodeError {
    message: "EOF while parsing value",
};
static EOF_WHILE_PARSING_OBJECT: DecodeError = DecodeError {
    message: "EOF while parsing object",
};

static EXPECTED_STRING: DecodeError = DecodeError {
    message: "Expected String",
};
static EOF_WHILE_PARSING_STRING: DecodeError = DecodeError {
    message: "EOF while parsing string",
};
static EXPECTED_COLON: DecodeError = DecodeError {
    message: "Expected colon after key",
};

static EXPECTED_SOME_IDENT: DecodeError = DecodeError {
    message: "Expected some ident",
};

pub static UNKNOWN_VARIANT: DecodeError = DecodeError {
    message: "Unknown enum variant",
};

pub static DUPLICATE_FIELD: DecodeError = DecodeError {
    message: "Duplicate field",
};
pub static RECURSION_LIMIT_EXCEEDED: DecodeError = DecodeError {
    message: "Recursion limit exceeded",
};

pub static MISSING_REQUIRED_FIELDS: DecodeError = DecodeError {
    message: "Missing required fields",
};

fn extract_numeric_literal(
    bytes: &[u8],
    mut index: usize,
) -> Result<(&str, usize), &'static DecodeError> {
    while let Some(next) = bytes.get(index) {
        match next {
            b' ' | b'\r' | b'\t' | b'\n' => index += 1,
            _ => break,
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

    return Ok((
        unsafe { std::str::from_utf8_unchecked(&bytes[mark..index]) },
        index,
    ));
}
fn is_escape(ch: u8) -> bool {
    ch == b'"' || ch == b'\\' || ch < 0x20
}

pub struct RetrySnapshot {
    index: usize,
    recursion_depth: i32,
}

const DEFAULT_DEPTH_LIMIT: i32 = 128;
/// consume_X, assumes we already know following thing must be type X
/// because we peeked and so the prefix. If you call `consume_X` with
/// peeking your code is likely buggy.
impl<'j> Parser<'j> {
    pub fn snapshot(&self) -> RetrySnapshot {
        RetrySnapshot {
            index: self.index,
            recursion_depth: self.remaining_depth,
        }
    }

    pub fn restore_for_retry(&mut self, snapshot: &RetrySnapshot) {
        self.index = snapshot.index;
        self.remaining_depth = snapshot.recursion_depth;
        self.parent_context = JsonParentContext::None;
        self.ctx.error = None;
    }

    pub fn new(data: &'j [u8]) -> Self {
        Self {
            ctx: Ctx::new(data),
            index: 0,
            parent_context: JsonParentContext::None,
            remaining_depth: DEFAULT_DEPTH_LIMIT,
            scratch: Vec::new(),
        }
    }
    pub fn report_static_error(&mut self, error: &'j str) {
        self.ctx.error = Some(Cow::Borrowed(error));
    }
    pub fn report_error(&mut self, error: String) {
        self.ctx.error = Some(Cow::Owned(error));
    }
    pub fn clear_error(&mut self) {
        self.ctx.error = None;
    }

    pub fn try_zerocopy(&self, text: &str) -> Option<&'j str> {
        // todo
        unsafe {
            if self.ctx.data.as_ptr_range().contains(&text.as_ptr()) {
                Some(&*(text as *const str))
            } else {
                None
            }
        }
    }
    #[allow(dead_code)]
    pub fn slice(&self, range: Range<usize>) -> Option<&[u8]> {
        self.ctx.data.get(range)
    }

    // pub fn current_position(&self) -> LinePosition {
    //     LinePosition::find(self.ctx.data, self.index)
    // }

    pub fn peek(&mut self) -> JsonResult<Peek> {
        if let Some(next) = self.eat_whitespace() {
            Ok(Peek::new(next))
        } else {
            Err(&EOF_WHILE_PARSING_VALUE)
        }
    }

    pub fn enter_array(&mut self) -> JsonResult<Option<Peek>> {
        if self.peek()? != Peek::Array {
            return Err(&EOF_WHILE_PARSING_LIST_FIRST_ELEMENT);
        }
        self.enter_seen_array()
    }

    pub fn enter_seen_array(&mut self) -> JsonResult<Option<Peek>> {
        debug_assert_eq!(self.ctx.data[self.index], b'[');
        self.index += 1;
        if let Some(next) = self.eat_whitespace() {
            if next == b']' {
                self.index += 1;
                Ok(None)
            } else {
                self.remaining_depth -= 1;
                if self.remaining_depth < 0 {
                    return Err(&RECURSION_LIMIT_EXCEEDED);
                }
                Ok(Some(Peek::new(next)))
            }
        } else {
            Err(&EOF_WHILE_PARSING_LIST_FIRST_ELEMENT)
        }
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
    pub fn array_step(&mut self) -> JsonResult<Option<Peek>> {
        if let Some(next) = self.eat_whitespace() {
            match next {
                b',' => {
                    self.index += 1;
                    let next = self.array_peek()?;
                    if next.is_none() {
                        Err(&TRAILING_COMMA)
                    } else {
                        Ok(next)
                    }
                }
                b']' => {
                    self.index += 1;
                    self.remaining_depth += 1;
                    Ok(None)
                }
                _ => Err(&EXPECTED_LIST_COMMA_OR_END),
            }
        } else {
            Err(&EOF_WHILE_PARSING_LIST)
        }
    }

    // todo should make this fuzzy and optimzied
    pub fn skip_value(&mut self) -> JsonResult<()> {
        match self.peek()? {
            Peek::Array => {
                if self.enter_seen_array()?.is_some() {
                    loop {
                        if let Err(err) = self.skip_value() {
                            return Err(err);
                        }
                        if self.array_step()?.is_none() {
                            break;
                        }
                    }
                }
                Ok(())
            }
            Peek::Object => {
                if self.enter_seen_object()?.is_some() {
                    loop {
                        if let Err(err) = self.skip_value() {
                            return Err(err);
                        }
                        if self.object_step()?.is_none() {
                            break;
                        }
                    }
                }

                Ok(())
            }
            Peek::True => {
                self.discard_seen_true()?;
                Ok(())
            }
            Peek::False => {
                self.discard_seen_false()?;
                Ok(())
            }
            Peek::Null => {
                self.discard_seen_null()?;
                Ok(())
            }
            Peek::String => {
                self.read_seen_string_unescaped()?;
                Ok(())
            }
            Peek::Minus | Peek(b'0'..b':') => {
                self.consume_numeric_literal()?;
                Ok(())
            }
            _ => Err(&TODO_ERROR),
        }
        // if let Some(value) = lazy_parser::expr_end(&self.ctx.data[self.index..]) {
        //     self.index += value;
        //     Ok(())
        // } else {
        //     Err(&TODO_ERROR)
        // }
    }
    // todo should make this fuzzy and optimzied
    pub fn index_into_next_object(&self, key: &str) -> JsonResult<Option<Parser<'j>>> {
        match crate::lazy_parser::object_index(&self.ctx.data[self.index..], key.as_bytes()) {
            Ok(value) => Ok(Some(Parser {
                parent_context: JsonParentContext::None,
                ctx: Ctx::new(self.ctx.data),
                index: self.index + (self.ctx.data.len() - value.len()),
                remaining_depth: self.remaining_depth,
                scratch: Vec::new(),
            })),
            Err(_) => Err(&TODO_ERROR),
        }
    }

    pub fn enter_seen_object_at_first_key(&mut self) -> JsonResult<Option<()>> {
        debug_assert_eq!(self.ctx.data[self.index], b'{');

        self.index += 1;
        if let Some(next) = self.eat_whitespace() {
            match next {
                b'"' => {
                    self.remaining_depth -= 1;
                    if self.remaining_depth < 0 {
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
            match self.object_step() {
                Ok(Some(_)) => {
                    if let Err(err) = self.skip_value() {
                        return Err(err);
                    }
                }
                Ok(None) => {
                    return Ok(());
                }
                Err(err) => {
                    return Err(err);
                }
            }
        }
    }

    pub fn tag_query_at_content_next_object(
        &mut self,
        tag: &str,
        content: &str,
    ) -> JsonResult<&str> {
        if self.peek()? != Peek::Object {
            return Err(&EOF_WHILE_PARSING_OBJECT);
        }
        let mut content_index = None;
        let mut current_field_name = match self.enter_seen_object() {
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
                match self.read_string_unescaped() {
                    Ok(value) => {
                        if let Some(content_index) = content_index {
                            self.index = content_index;
                            return Ok(value);
                        }
                        loop {
                            match self.object_step() {
                                Ok(Some(name)) => {
                                    if name == content {
                                        return Ok(value);
                                    }
                                    if let Err(err) = self.skip_value() {
                                        return Err(err);
                                    }
                                }
                                Ok(None) => {
                                    return Err(&DecodeError {
                                        message: "Missing content field",
                                    });
                                }
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
            if let Err(err) = self.skip_value() {
                return Err(err);
            }
            match self.object_step() {
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
    pub fn tag_query_next_object(&mut self, tag: &str) -> JsonResult<&'j str> {
        if self.peek()? != Peek::Object {
            return Err(&EOF_WHILE_PARSING_OBJECT);
        }
        let initial_index = self.index;
        let initial_remaining_depth = self.remaining_depth;
        let mut current_field_name = match self.enter_seen_object() {
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
                match self.read_string_unescaped() {
                    Ok(value) => {
                        self.index = initial_index;
                        self.remaining_depth = initial_remaining_depth;
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
            match self.object_step() {
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
    pub fn enter_object(&mut self) -> JsonResult<Option<&str>> {
        if self.peek()? != Peek::Object {
            return Err(&EOF_WHILE_PARSING_OBJECT);
        }
        self.enter_seen_object()
    }

    pub fn enter_seen_object(&mut self) -> JsonResult<Option<&str>> {
        debug_assert_eq!(self.ctx.data[self.index], b'{');
        self.index += 1;
        if let Some(next) = self.eat_whitespace() {
            match next {
                b'"' => {
                    self.remaining_depth -= 1;
                    if self.remaining_depth < 0 {
                        return Err(&RECURSION_LIMIT_EXCEEDED);
                    }
                    // self.read_seen_unescaped_object_key().map(Some)
                    self.read_seen_object_key().map(Some)
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

    pub fn object_step_at_key(&mut self) -> JsonResult<Option<()>> {
        if let Some(next) = self.eat_whitespace() {
            match next {
                b',' => {
                    self.index += 1;
                    match self.eat_whitespace() {
                        Some(b'"') => Ok(Some(())),
                        Some(b'}') => Err(&TRAILING_COMMA),
                        Some(_) => Err(&KEY_MUST_BE_A_STRING),
                        None => Err(&EOF_WHILE_PARSING_VALUE),
                    }
                }
                b'}' => {
                    self.index += 1;
                    self.remaining_depth += 1;
                    Ok(None)
                }
                _ => Err(&EXPECTED_OBJECT_COMMA_OR_END),
            }
        } else {
            Err(&EOF_WHILE_PARSING_OBJECT)
        }
    }

    pub fn object_step(&mut self) -> JsonResult<Option<&str>> {
        if let Some(next) = self.eat_whitespace() {
            match next {
                b',' => {
                    self.index += 1;
                    match self.eat_whitespace() {
                        Some(b'"') => self.read_seen_object_key().map(Some),
                        Some(b'}') => Err(&TRAILING_COMMA),
                        Some(_) => Err(&KEY_MUST_BE_A_STRING),
                        None => Err(&EOF_WHILE_PARSING_VALUE),
                    }
                }
                b'}' => {
                    self.index += 1;
                    self.remaining_depth += 1;
                    Ok(None)
                }
                _ => Err(&EXPECTED_OBJECT_COMMA_OR_END),
            }
        } else {
            Err(&EOF_WHILE_PARSING_OBJECT)
        }
    }

    pub fn finish(&mut self) -> JsonResult<()> {
        if self.eat_whitespace().is_none() {
            Ok(())
        } else {
            Err(&TRAILING_CHARACTERS)
        }
    }

    /// May only call if Peak::True
    pub fn discard_seen_true(&mut self) -> JsonResult<()> {
        debug_assert_eq!(self.ctx.data[self.index], b't');
        self.consume_ident(TRUE_REST)
    }

    /// May only call if Peak::False
    pub fn discard_seen_false(&mut self) -> JsonResult<()> {
        debug_assert_eq!(self.ctx.data[self.index], b'f');
        self.consume_ident(FALSE_REST)
    }

    pub fn discard_seen_null(&mut self) -> JsonResult<()> {
        debug_assert_eq!(self.ctx.data[self.index], b'n');
        self.consume_ident(NULL_REST)
    }

    fn skip_to_escape(&mut self) {
        // Immediately bail-out on empty strings and consecutive escapes (e.g. \u041b\u0435)
        if self.index == self.ctx.data.len() || is_escape(self.ctx.data[self.index]) {
            return;
        }
        self.index += 1;

        let rest = &self.ctx.data[self.index..];

        // if !forbid_control_characters {
        //     self.index += memchr::memchr2(b'"', b'\\', rest).unwrap_or(rest.len());
        //     return;
        // }

        // We wish to find the first byte in range 0x00..=0x1F or " or \. Ideally, we'd use
        // something akin to memchr3, but the memchr crate does not support this at the moment.
        // Therefore, we use a variation on Mycroft's algorithm [1] to provide performance better
        // than a naive loop. It runs faster than equivalent two-pass memchr2+SWAR code on
        // benchmarks and it's cross-platform, so probably the right fit.
        // [1]: https://groups.google.com/forum/#!original/comp.lang.c/2HtQXvg7iKc/xOJeipH6KLMJ

        // #[cfg(fast_arithmetic = "64")]
        type Chunk = u64;
        // #[cfg(fast_arithmetic = "32")]
        // type Chunk = u32;

        const STEP: usize = size_of::<Chunk>();
        const ONE_BYTES: Chunk = Chunk::MAX / 255; // 0x0101...01

        // unsafe {
        //     //saftey nned to check empty
        //     let mut head = self.ctx.data.as_ptr().add(self.index) as *const u64;
        //     let end = self.ctx.data.as_ptr().add(self.ctx.data.len()).sub(8) as *const u64;
        //     while head < end {
        //         let chars = head.read_unaligned();
        //         let contains_ctrl = chars.wrapping_sub(ONE_BYTES * 0x20) & !chars;
        //         let chars_quote = chars ^ (ONE_BYTES * Chunk::from(b'"'));
        //         let contains_quote = chars_quote.wrapping_sub(ONE_BYTES) & !chars_quote;
        //         let chars_backslash = chars ^ (ONE_BYTES * Chunk::from(b'\\'));
        //         let contains_backslash = chars_backslash.wrapping_sub(ONE_BYTES) & !chars_backslash;
        //         let masked =
        //             (contains_ctrl | contains_quote | contains_backslash) & (ONE_BYTES << 7);
        //         if masked != 0 {
        //             // SAFETY: chunk is in-bounds for slice
        //             self.index = unsafe { (head as *const u8).offset_from(self.ctx.data.as_ptr()) }
        //                 as usize
        //                 + masked.trailing_zeros() as usize / 8;
        //             return;
        //         }
        //         head = head.add(1);
        //     }
        // }
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
                self.index = unsafe { chunk.as_ptr().offset_from(self.ctx.data.as_ptr()) } as usize
                    + masked.trailing_zeros() as usize / 8;
                return;
            }
        }
        // unsafe {
        //     //saftey nned to check empty
        //     let mut head = self.ctx.data.as_ptr().add(self.index) as *const u64;
        //     let end = self.ctx.data.as_ptr().add(self.ctx.data.len()).sub(8) as *const u64;
        //     while head < end {
        // read unalign only works on LE
        //         let chars = head.read_unaligned();
        //         let contains_ctrl = chars.wrapping_sub(ONE_BYTES * 0x20) & !chars;
        //         let chars_quote = chars ^ (ONE_BYTES * Chunk::from(b'"'));
        //         let contains_quote = chars_quote.wrapping_sub(ONE_BYTES) & !chars_quote;
        //         let chars_backslash = chars ^ (ONE_BYTES * Chunk::from(b'\\'));
        //         let contains_backslash = chars_backslash.wrapping_sub(ONE_BYTES) & !chars_backslash;
        //         let masked =
        //             (contains_ctrl | contains_quote | contains_backslash) & (ONE_BYTES << 7);
        //         if masked != 0 {
        //             // SAFETY: chunk is in-bounds for slice
        //             self.index = unsafe { (head as *const u8).offset_from(self.ctx.data.as_ptr()) }
        //                 as usize
        //                 + masked.trailing_zeros() as usize / 8;
        //             return;
        //         }
        //         head = head.add(1);
        //     }
        // }

        self.index += rest.len() / STEP * STEP;
        self.skip_to_escape_slow();
    }
    #[cold]
    #[inline(never)]
    fn skip_to_escape_slow(&mut self) {
        while self.index < self.ctx.data.len() && !is_escape(self.ctx.data[self.index]) {
            self.index += 1;
        }
    }

    pub fn take_cow_string(&mut self) -> JsonResult<Cow<'j, str>> {
        if self.peek()? != Peek::String {
            return Err(&EOF_WHILE_PARSING_STRING);
        }
        let value: *const str = self.read_seen_string()?;
        let (value, ctx) = unsafe { self.unfreeze_with_context(value) };
        if let Some(borrowed) = ctx.try_extend_lifetime(value) {
            Ok(Cow::Borrowed(borrowed))
        } else {
            let owned = value.to_string();
            Ok(Cow::Owned(owned))
        }
    }

    /// Reads and returns the next string value from the JSON input.
    ///
    /// This function advances the parser's cursor to the next non-whitespace character,
    /// expecting it to be the start of a string. If a string is found, it is parsed
    /// and returned. The parser's cursor ends immediately after the parsed string.
    ///
    /// Depending on the presence of escapes the string maybe reference to underlying
    /// input or buffer.
    pub fn take_string(&mut self) -> JsonResult<&str> {
        if self.peek()? != Peek::String {
            return Err(&EOF_WHILE_PARSING_STRING);
        }
        self.read_seen_string()
    }

    pub fn read_seen_string(&mut self) -> JsonResult<&str> {
        debug_assert_eq!(self.ctx.data[self.index], b'"');
        self.index += 1;
        let mut start = self.index;
        self.scratch.clear();
        loop {
            self.skip_to_escape();
            if self.index == self.ctx.data.len() {
                return Err(&EOF_WHILE_PARSING_STRING);
            }
            // match unsafe { self.ctx.data.get_unchecked(self.indexl } {
            match self.ctx.data[self.index] {
                b'"' => {
                    if self.scratch.is_empty() {
                        let output = unsafe {
                            std::str::from_utf8_unchecked(&self.ctx.data[start..self.index])
                        };
                        self.index += 1;
                        return Ok(output);
                    } else {
                        self.scratch
                            .extend_from_slice(&self.ctx.data[start..self.index]);
                        self.index += 1;
                        return Ok(unsafe { std::str::from_utf8_unchecked(&self.scratch) });
                    }
                }
                b'\\' => {
                    self.scratch
                        .extend_from_slice(&self.ctx.data[start..self.index]);
                    self.index += 1;
                    match crate::strings::parse_escape(
                        self.index,
                        &self.ctx.data,
                        true,
                        &mut self.scratch,
                    ) {
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
    pub fn read_seen_string_unescaped(&mut self) -> JsonResult<&'j str> {
        debug_assert_eq!(self.ctx.data[self.index], b'"');
        self.index += 1;
        let start = self.index;
        loop {
            self.skip_to_escape();
            if self.index == self.ctx.data.len() {
                return Err(&EOF_WHILE_PARSING_STRING);
            }
            // match unsafe { self.ctx.data.get_unchecked(self.indexl } {
            match self.ctx.data[self.index] {
                b'"' => {
                    break;
                }
                b'\\' => {
                    self.index += 1;
                    continue;
                }
                _ => {
                    return Err(&DecodeError {
                        message: "Control character detected in json",
                    })
                }
            }
        }
        let output = unsafe { std::str::from_utf8_unchecked(&self.ctx.data[start..self.index]) };
        self.index += 1;
        Ok(output)
    }

    pub fn read_string_unescaped(&mut self) -> JsonResult<&'j str> {
        if self.eat_whitespace() != Some(b'"') {
            return Err(&EXPECTED_STRING);
        }
        self.read_seen_string_unescaped()
    }

    pub fn consume_numeric_literal(&mut self) -> JsonResult<&'j str> {
        let (output, index) = extract_numeric_literal(self.ctx.data, self.index)?;
        self.index = index;
        Ok(output)
    }

    pub(crate) unsafe fn unfreeze_with_context<'a>(
        &'a mut self,
        frozen: *const str,
    ) -> (&'a str, &'a mut Ctx<'j>) {
        (&*frozen, &mut self.ctx)
    }
    pub(crate) unsafe fn unfreeze<'a>(&'a mut self, frozen: *const str) -> &'a str {
        &*frozen
    }

    fn read_seen_object_key(&mut self) -> JsonResult<&'j str> {
        debug_assert_eq!(self.ctx.data[self.index], b'"');
        let key = self.read_seen_string()? as *const str;

        if let Some(next) = self.eat_whitespace() {
            if next == b':' {
                self.index += 1;

                // todo split buffer and parser such that this hack is unneeded
                Ok(unsafe { &*key })
            } else {
                Err(&EXPECTED_COLON)
            }
        } else {
            Err(&EOF_WHILE_PARSING_OBJECT)
        }
    }

    fn consume_ident<const SIZE: usize>(&mut self, expected: [u8; SIZE]) -> JsonResult<()> {
        self.index = consume_ident(self.ctx.data, self.index, expected)?;
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
    fn eat_whitespace(&mut self) -> Option<u8> {
        while let Some(&next) = self.ctx.data.get(self.index) {
            // optimize for common case.
            if next > b' ' {
                return Some(next);
            }
            match next {
                b' ' | b'\r' | b'\t' | b'\n' => self.index += 1,
                _ => return Some(next),
            }
        }
        None
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
