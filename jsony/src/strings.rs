use std::borrow::Cow;

use memchr::memchr;

use crate::json::DecodeError;

pub fn escape_to_str(input: &str) -> Result<&str, ()> {
    if let Some(_index) = memchr(b'\\', input.as_bytes()) {
        Err(())
    } else {
        Ok(input)
    }
}
pub fn escape_to_string(input: &str) -> Result<String, ()> {
    Ok(escape_to_cow(input).map_err(|_| ())?.into_owned())
}
pub fn escape_to_cow(input: &str) -> Result<Cow<'_, str>, &'static DecodeError> {
    let Some(mut index) = memchr(b'\\', input.as_bytes()) else {
        return Ok(Cow::Borrowed(input));
    };

    let bytes = input.as_bytes();
    let mut start = 0;
    let mut scratch = Vec::with_capacity(input.len() + 16);
    loop {
        scratch.extend_from_slice(&bytes[start..index]);
        index += 1;
        let nindex = match parse_escape(index, bytes, true, &mut scratch) {
            Ok(nindex) => nindex,
            Err(_err) => {
                return Err(&DecodeError {
                    message: "invalid escape",
                })
            }
        };
        index = nindex;
        start = index;
        let Some(index2) = memchr(b'\\', &bytes[start..]) else {
            scratch.extend_from_slice(&bytes[start..]);
            return Ok(Cow::Owned(unsafe { String::from_utf8_unchecked(scratch) }));
        };
        index = index2;
    }
}

fn parse_escape(
    mut index: usize,
    read: &[u8],
    validate: bool,
    scratch: &mut Vec<u8>,
) -> Result<usize, ()> {
    let Some(ch) = read.first() else {
        return Err(());
    };
    index += 1;

    match ch {
        b'"' => scratch.push(b'"'),
        b'\\' => scratch.push(b'\\'),
        b'/' => scratch.push(b'/'),
        b'b' => scratch.push(b'\x08'),
        b'f' => scratch.push(b'\x0c'),
        b'n' => scratch.push(b'\n'),
        b'r' => scratch.push(b'\r'),
        b't' => scratch.push(b'\t'),
        b'u' => return parse_unicode_escape(index, read, validate, scratch),
        // _ => return error(read, ErrorCode::InvalidEscape),
        _ => return Err(()),
    }

    Ok(index)
}
/// Parses a JSON \u escape and appends it into the scratch space. Assumes `\u`
/// has just been read.
#[cold]
fn parse_unicode_escape(
    mut index: usize,
    read: &[u8],
    validate: bool,
    scratch: &mut Vec<u8>,
) -> Result<usize, ()> {
    let n = match read[index..] {
        [a, b, c, d, ..] => {
            index += 4;
            match decode_four_hex_digits(a, b, c, d) {
                Some(val) => val,
                // None => error(self, ErrorCode::InvalidEscape),
                None => return Err(()),
            }
        }
        _ => {
            // index = self.slice.len();
            // error(self, ErrorCode::EofWhileParsingString)
            return Err(());
        }
    };

    // Non-BMP characters are encoded as a sequence of two hex escapes,
    // representing UTF-16 surrogates. If deserializing a utf-8 string the
    // surrogates are required to be paired, whereas deserializing a byte string
    // accepts lone surrogates.
    if validate && (0xDC00..=0xDFFF).contains(&n) {
        // XXX: This is actually a trailing surrogate.
        return Err(());
    }

    if !(0xD800..=0xDBFF).contains(&n) {
        // Every u16 outside of the surrogate ranges is guaranteed to be a
        // legal char.
        push_wtf8_codepoint(n as u32, scratch);
        return Ok(index);
    }

    // n is a leading surrogate, we now expect a trailing surrogate.
    let n1 = n;

    if read.get(index..index + 2) != Some(b"\\u") {
        return Err(());
    }
    index += 2;
    // if tri!(peek_or_eof(read)) == b'\\' {
    //     read.discard();
    // } else {
    //     return Err(());
    //     // return if validate {
    //     //     read.discard();
    //     //     return Err(());
    //     // } else {
    //     //     push_wtf8_codepoint(n1 as u32, scratch);
    //     //     Ok(index)
    //     // };
    // }

    // if tri!(peek_or_eof(read)) == b'u' {
    //     read.discard();
    // } else {
    //     return if validate {
    //         read.discard();
    //         error(read, ErrorCode::UnexpectedEndOfHexEscape)
    //     } else {
    //         push_wtf8_codepoint(n1 as u32, scratch);
    //         // The \ prior to this byte started an escape sequence, so we
    //         // need to parse that now. This recursive call does not blow the
    //         // stack on malicious input because the escape is not \u, so it
    //         // will be handled by one of the easy nonrecursive cases.
    //         parse_escape(read, validate, scratch)
    //     };
    // }

    let n2 = match read[index..] {
        [a, b, c, d, ..] => {
            index += 4;
            match decode_four_hex_digits(a, b, c, d) {
                Some(val) => val,
                // None => error(self, ErrorCode::InvalidEscape),
                None => return Err(()),
            }
        }
        _ => {
            // index = self.slice.len();
            // error(self, ErrorCode::EofWhileParsingString)
            return Err(());
        }
    };

    if !(0xDC00..=0xDFFF).contains(&n2) {
        return Err(());
    }

    // This value is in range U+10000..=U+10FFFF, which is always a valid
    // codepoint.
    let n = (((n1 - 0xD800) as u32) << 10 | (n2 - 0xDC00) as u32) + 0x1_0000;
    push_wtf8_codepoint(n, scratch);
    Ok(index)
}

/// Adds a WTF-8 codepoint to the end of the buffer. This is a more efficient
/// implementation of String::push. The codepoint may be a surrogate.
#[inline]
fn push_wtf8_codepoint(n: u32, scratch: &mut Vec<u8>) {
    if n < 0x80 {
        scratch.push(n as u8);
        return;
    }

    scratch.reserve(4);

    unsafe {
        let ptr = scratch.as_mut_ptr().add(scratch.len());

        let encoded_len = match n {
            0..=0x7F => unreachable!(),
            0x80..=0x7FF => {
                ptr.write((n >> 6 & 0b0001_1111) as u8 | 0b1100_0000);
                2
            }
            0x800..=0xFFFF => {
                ptr.write((n >> 12 & 0b0000_1111) as u8 | 0b1110_0000);
                ptr.add(1).write((n >> 6 & 0b0011_1111) as u8 | 0b1000_0000);
                3
            }
            0x1_0000..=0x10_FFFF => {
                ptr.write((n >> 18 & 0b0000_0111) as u8 | 0b1111_0000);
                ptr.add(1)
                    .write((n >> 12 & 0b0011_1111) as u8 | 0b1000_0000);
                ptr.add(2).write((n >> 6 & 0b0011_1111) as u8 | 0b1000_0000);
                4
            }
            0x11_0000.. => unreachable!(),
        };
        ptr.add(encoded_len - 1)
            .write((n & 0b0011_1111) as u8 | 0b1000_0000);

        scratch.set_len(scratch.len() + encoded_len);
    }
}

/// Parses a JSON escape sequence and discards the value. Assumes the previous
/// byte read was a backslash.
// fn ignore_escape<'de, R>(read: &mut R) -> Result<()>
// where
//     R: ?Sized + Read<'de>,
// {
//     let ch = tri!(next_or_eof(read));

//     match ch {
//         b'"' | b'\\' | b'/' | b'b' | b'f' | b'n' | b'r' | b't' => {}
//         b'u' => {
//             // At this point we don't care if the codepoint is valid. We just
//             // want to consume it. We don't actually know what is valid or not
//             // at this point, because that depends on if this string will
//             // ultimately be parsed into a string or a byte buffer in the "real"
//             // parse.

//             tri!(read.decode_hex_escape());
//         }
//         _ => {
//             return error(read, ErrorCode::InvalidEscape);
//         }
//     }

//     Ok(())
// }

const fn decode_hex_val_slow(val: u8) -> Option<u8> {
    match val {
        b'0'..=b'9' => Some(val - b'0'),
        b'A'..=b'F' => Some(val - b'A' + 10),
        b'a'..=b'f' => Some(val - b'a' + 10),
        _ => None,
    }
}

const fn build_hex_table(shift: usize) -> [i16; 256] {
    let mut table = [0; 256];
    let mut ch = 0;
    while ch < 256 {
        table[ch] = match decode_hex_val_slow(ch as u8) {
            Some(val) => (val as i16) << shift,
            None => -1,
        };
        ch += 1;
    }
    table
}

static HEX0: [i16; 256] = build_hex_table(0);
static HEX1: [i16; 256] = build_hex_table(4);

fn decode_four_hex_digits(a: u8, b: u8, c: u8, d: u8) -> Option<u16> {
    let a = HEX1[a as usize] as i32;
    let b = HEX0[b as usize] as i32;
    let c = HEX1[c as usize] as i32;
    let d = HEX0[d as usize] as i32;

    let codepoint = ((a | b) << 8) | c | d;

    // A single sign bit check.
    if codepoint >= 0 {
        Some(codepoint as u16)
    } else {
        None
    }
}
