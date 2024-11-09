//! Lazy JSON parsing that allows access keys of objects and arrays
//! without parsing the entire string or intermediate objects.
//!
//! Ideal for when you have deeply nested json but only care about some
//! inner part. Further, all index access implement null coalescing meaning
//! when an
//!
//! Currently, should not be used to parse user provided json as currently
//! strict validation is not enough leading strange behaviour.
//!
//! Example:
//! ```ignore
//! let json = Value::new(r#"{"a": {"b": [{"c": "contents"}]}}"#);
//! let contents: String = json["a"]["b"][0]["c"].parse().unwrap();
//! assert_eq!(json["a"]["name_of_missing_key"][0]["c"].key_error(), Some("name_of_missing_key"));
//! ```

use crate::MaybeJson;
use memchr::memchr;

use crate::{from_json_with_config, FromJson, JsonError, JsonParserConfig};
// use serde::de::Error;

#[inline(always)]
pub fn index_of_string_end2(s: &[u8]) -> Option<usize> {
    unsafe {
        let start = s.as_ptr().add(1);
        let end = s.as_ptr().add(s.len() - 1);
        #[cfg(target_arch = "x86_64")]
        let searcher = memchr::arch::x86_64::sse2::memchr::One::new_unchecked(b'"');
        #[cfg(not(target_arch = "x86_64"))]
        let searcher = memchr::arch::all::memchr::One::new(b'"');
        'outer: while let Some(found) = searcher.find_raw(start, end) {
            if *found.sub(1) == b'\\' {
                let mut mx = found.sub(2);
                let mut escaped = true;
                loop {
                    if *mx != b'\\' {
                        if escaped {
                            continue 'outer;
                        } else {
                            break;
                        }
                    }
                    mx = mx.sub(1);
                    escaped ^= true;
                }
            }
            return Some((found.offset_from(start) + 1) as usize);
        }
        None
    }
}

fn index_after_value(s: &[u8]) -> Option<usize> {
    let mut depth = 1;
    let mut i = 0;
    while let Some(ch) = s.get(i) {
        i += 1;
        match ch {
            b'"' => i += index_of_string_end2(&s[i - 1..])?,
            b'{' | b'[' => depth += 1,
            b'}' | b']' => {
                depth -= 1;
                if depth == 0 {
                    return Some(i);
                }
            }
            _ => (),
        }
    }
    None
}

pub fn expr_end(s: &[u8]) -> Option<usize> {
    let mut i = 0;
    while s.get(i)?.is_ascii_whitespace() {
        i += 1;
    }
    let ch = s[i];
    i += 1;
    match ch {
        b'"' => Some(i + index_of_string_end2(&s[i - 1..])?),
        b'{' | b'[' => Some(i + index_after_value(&s[i..])?),
        _ => {
            let v = memchr::memchr3(b',', b']', b'}', &s[i..])?;
            Some(v + i)
        }
    }
}

fn index_of_next_value_in_array(s: &[u8]) -> Option<usize> {
    let mut i = 0;
    while s.get(i)?.is_ascii_whitespace() {
        i += 1;
    }
    let ch = s[i];
    i += 1;
    match ch {
        b'"' => i += index_of_string_end2(&s[i - 1..])?,
        b'{' | b'[' => i += index_after_value(&s[i..])?,
        _ => {}
    }
    i += memchr::memchr2(b',', b']', &s[i..])?;
    if s[i] == b']' {
        return None;
    }
    Some(i + 1)
}

// Return remaining bytes starting with value at the provided index if the bytes
// begins with a valid JSON array.
// If bytes is valid UTF-8 the returned bytes will be too.
fn array_index(bytes: &[u8], mut index: usize) -> Result<&[u8], ErrorCode> {
    let mut i = 0;
    while bytes.get(i).ok_or(ErrorCode::Eof)?.is_ascii_whitespace() {
        i += 1;
    }
    if bytes[i] != b'[' {
        return Err(ErrorCode::ExpectedArray);
    }
    i += 1;
    while index > 0 {
        i += index_of_next_value_in_array(&bytes[i..]).ok_or(ErrorCode::OutOfBoundsArrayAccess)?;
        index -= 1;
    }
    while bytes.get(i).ok_or(ErrorCode::Eof)?.is_ascii_whitespace() {
        i += 1;
    }
    if bytes[i] == b']' {
        return Err(ErrorCode::OutOfBoundsArrayAccess);
    }
    Ok(&bytes[i..])
}

// Return remaining bytes starting with value of the provided key.
// begins with a valid JSON object.
// If bytes is valid UTF-8 the returned bytes will be too.
pub(crate) fn object_index<'a>(bytes: &'a [u8], key: &[u8]) -> Result<&'a [u8], ErrorCode> {
    let mut i = 0;
    while bytes.get(i).ok_or(ErrorCode::Eof)?.is_ascii_whitespace() {
        i += 1;
    }
    if bytes[i] != b'{' {
        return Err(ErrorCode::ExpectedObject);
    }
    i += 1;
    while let Some(offset) = memchr(b'"', bytes.get(i..).ok_or(ErrorCode::Eof)?) {
        i += offset + 1;
        let key_start = i;
        i += index_of_string_end2(&bytes[key_start - 1..]).ok_or(ErrorCode::Eof)?;
        let key_stop = i - 1;
        i += memchr(b':', &bytes[i..]).ok_or(ErrorCode::Eof)? + 1;
        let expr_start = i;
        {
            let k = (key_start, key_stop);
            // let v = (expr_start);
            if key == &bytes[k.0..k.1] {
                return Ok(&bytes[expr_start..]);
            }
        }
        i += expr_end(&bytes[i..]).ok_or(ErrorCode::Eof)?;
    }
    Err(ErrorCode::MissingObjectKey)
}

impl std::ops::Index<&'static str> for MaybeJson {
    type Output = MaybeJson;

    /// If the current value is an object, get the value of the corresponding key
    fn index(&self, key: &'static str) -> &Self::Output {
        if self.is_error() {
            return self;
        }
        match object_index(self.raw.as_bytes(), key.as_bytes()) {
            // Safety: See contract of object_index, since self.raw was valid UTF-8 so is value
            Ok(value) => MaybeJson::new(unsafe { std::str::from_utf8_unchecked(value) }),
            Err(ErrorCode::MissingObjectKey) => MaybeJson::string_index_error(key),
            Err(err) => err.as_value(),
        }
    }
}

impl std::ops::Index<usize> for MaybeJson {
    type Output = MaybeJson;

    /// If the current value is an array, get the value of the corresponding index
    fn index(&self, index: usize) -> &Self::Output {
        if self.is_error() {
            return self;
        }
        match array_index(self.raw.as_bytes(), index) {
            Ok(value) => {
                // println!(
                //     "[{:?}] => {}",
                //     index,
                //     value[..value.len().min(50)].escape_ascii()
                // );
                MaybeJson::new(unsafe { std::str::from_utf8_unchecked(value) })
            }
            Err(err) => err.as_value(),
        }
    }
}

#[derive(Clone, Copy, PartialEq, Eq, Debug)]
#[repr(u8)]
pub enum ErrorCode {
    Eof = 1,
    MissingObjectKey = 2,
    OutOfBoundsArrayAccess = 3,
    ExpectedObject = 4,
    ExpectedArray = 5,
}

impl ErrorCode {
    fn as_value(self) -> &'static MaybeJson {
        // Safety:
        //   - Since ErrorCode is never 0, fake_pointer is never null
        //   - Since length is set to 0 no other restrictions exist
        //   - Since #[repr(transparent)] Value(str), cast is safe
        unsafe {
            let fake_pointer = self as usize as *const u8;
            &*(core::ptr::slice_from_raw_parts(fake_pointer, 0) as *const MaybeJson)
        }
    }
}

impl MaybeJson {
    pub fn parse<'a, T: FromJson<'a>>(&'a self) -> Result<T, JsonError> {
        if self.is_error() {
            todo!();
        }
        from_json_with_config::<'a, T>(
            &self.raw,
            JsonParserConfig {
                recursion_limit: 128,
                allow_trailing_commas: false,
                allow_comments: false,
                allow_unquoted_keys: false,
                allow_trailing_data: true,
            },
        )
    }
    pub fn error_code(&self) -> Option<ErrorCode> {
        if !self.is_error() {
            return None;
        }
        let tagged = self.raw.as_ptr() as usize;
        if (1..=5).contains(&tagged) {
            unsafe { Some(std::mem::transmute::<u8, ErrorCode>(tagged as u8)) }
        } else if tagged > 0xC000_0000_0000_0000 {
            Some(ErrorCode::MissingObjectKey)
        } else {
            None
        }
    }

    fn string_index_error(key: &'static str) -> &'static MaybeJson {
        #[cfg(target_pointer_width = "64")]
        // Safety:
        //   - key pointer must be non-null by definition of references
        //   - Thus tagged is non-null
        //   - Since length is set to 0 no other restrictions exist
        // Under strict provenance the int-2-pointer & back casts may be UB.
        // waiting for strict_provenance: https://github.com/rust-lang/rust/issues/95228
        unsafe {
            let pointer_bits = key.as_ptr() as usize;
            if pointer_bits > 0x1_ffff_ffff_ffff || key.len() > 0xfff {
                return ErrorCode::MissingObjectKey.as_value();
            }
            let tagged = pointer_bits | (key.len() << 49) | (0xC000_0000_0000_0000);
            &*(core::ptr::slice_from_raw_parts(tagged as *const u8, 0) as *const MaybeJson)
        }
        #[cfg(not(target_pointer_width = "64"))]
        {
            ErrorCode::MissingObjectKey.as_value();
        }
    }

    /// If the value is currently a MissingObjectKey get the key that causes
    /// error is possible.
    pub fn key_error(&self) -> Option<&'static str> {
        #[cfg(target_pointer_width = "64")]
        {
            if !self.is_error() {
                return None;
            }
            let tagged = self.raw.as_ptr() as usize;
            if tagged < 0xC000_0000_0000_0000 {
                return None;
            }
            let length = (tagged >> 49) & 0xfff;
            let pointer = tagged & 0x1_ffff_ffff_ffff;
            // Safety:
            //   - Assumes was created by [Value::string_index_error]
            unsafe {
                Some(std::str::from_utf8_unchecked(std::slice::from_raw_parts(
                    pointer as *const u8,
                    length,
                )))
            }
        }
        #[cfg(not(target_pointer_width = "64"))]
        {
            None
        }
    }

    #[cfg(not(target_pointer_width = "64"))]
    pub fn key_error(&self) -> Option<&'static str> {
        return None;
    }

    pub fn is_error(&self) -> bool {
        self.raw.is_empty()
    }

    pub fn null() -> &'static MaybeJson {
        MaybeJson::new("")
    }

    pub fn new(raw: &str) -> &MaybeJson {
        if raw.is_empty() {
            ErrorCode::Eof.as_value()
        } else {
            unsafe { &*(raw as *const str as *const MaybeJson) }
        }
    }

    // return a formatted error message if any.
    pub fn error(&self) -> Option<String> {
        if !self.is_error() {
            return None;
        }
        if let Some(key) = self.key_error() {
            return Some(format!("Object key not found: {key:?}"));
        }
        let Some(error_code) = self.error_code() else {
            // shouldn't happen ??
            return Some("Unknown JSON Parsing Error".into());
        };
        let message = match error_code {
            ErrorCode::Eof => "Unexpected EOF",
            ErrorCode::MissingObjectKey => "Key not found in object",
            ErrorCode::OutOfBoundsArrayAccess => "Index larger than array",
            ErrorCode::ExpectedObject => "Expected an object but found something else",
            ErrorCode::ExpectedArray => "Expected an array but found something else",
        };

        Some(message.into())
    }
}

#[cfg(test)]
mod test {
    use super::*;
    #[test]
    fn arrays() {
        let value = MaybeJson::new(stringify!([
            {
                "inner": [42, "nice"]
            },
            "wow",
            false,
            true,
            -123.456,
            null,
        ]));
        assert_eq!(42u64, value[0]["inner"][0].parse::<u64>().unwrap());
        assert_eq!("nice", value[0]["inner"][1].parse::<String>().unwrap());
        assert_eq!("wow", value[1].parse::<String>().unwrap());
        assert!(!value[2].parse::<bool>().unwrap());
        assert!(value[3].parse::<bool>().unwrap());
        assert_eq!(-123.456, value[4].parse::<f64>().unwrap());
        assert_eq!(None::<u32>, value[5].parse::<Option<u32>>().unwrap());
    }

    #[test]
    fn errors() {
        let value = MaybeJson::new(stringify!({
            "hello": 124,
            "nice": {"inner": [1,2,3]}
        }));
        assert_eq!(value["nice"]["inner"][0].parse::<u64>().unwrap(), 1);
        assert_eq!(
            MaybeJson::new("[]")[0].error_code(),
            Some(ErrorCode::OutOfBoundsArrayAccess)
        );
        assert_eq!(
            value["nice"]["inner"][5].error_code(),
            Some(ErrorCode::OutOfBoundsArrayAccess)
        );

        assert_eq!(value["nice"]["not_inner"].key_error(), Some("not_inner"));

        assert_eq!(
            value["nice"]["not_inner"].error_code(),
            Some(ErrorCode::MissingObjectKey)
        );

        assert_eq!(value[0].error_code(), Some(ErrorCode::ExpectedArray));

        assert_eq!(
            value["nice"]["inner"]["key_for_array"].error_code(),
            Some(ErrorCode::ExpectedObject)
        );
    }
}
