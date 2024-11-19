use std::mem::MaybeUninit;

use crate::{json::DecodeError, parser::EOF_WHILE_PARSING_STRING};

fn is_escape(ch: u8) -> bool {
    ch == b'"' || ch == b'\\' || ch < 0x20
}

fn skip_to_escape(mut at: usize, data: &[u8]) -> usize {
    // Immediately bail-out on empty strings and consecutive escapes (e.g. \u041b\u0435)
    if at == data.len() || is_escape(data[at]) {
        return at;
    }
    at += 1;

    let rest = &data[at..];

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
            return unsafe { chunk.as_ptr().offset_from(data.as_ptr()) } as usize
                + masked.trailing_zeros() as usize / 8;
        }
    }

    at += rest.len() / STEP * STEP;
    skip_to_escape_slow(at, data)
}

#[cold]
#[inline(never)]
fn skip_to_escape_slow(mut at: usize, data: &[u8]) -> usize {
    while at < data.len() && !is_escape(data[at]) {
        at += 1;
    }
    at
}

pub(crate) fn parse_escape(index: usize, read: &[u8], scratch: &mut Vec<u8>) -> Result<usize, ()> {
    scratch.reserve(4);
    match parse_escape_inner(index, read, unsafe {
        &mut *(scratch.spare_capacity_mut().as_mut_ptr() as *mut [MaybeUninit<u8>; 4])
    }) {
        Ok((i, v)) => {
            let l = scratch.len() + v;
            unsafe {
                scratch.set_len(l);
            }
            Ok(i)
        }
        Err(_) => Err(()),
    }
}

pub(crate) fn skip_json_string_and_eq(
    mut at: usize,
    data: &[u8],
    value: &[u8],
) -> Result<(usize, bool), &'static DecodeError> {
    // at += 1;
    let mut read = 0;
    let mut head = at;
    loop {
        head = skip_to_escape(head, data);

        if head == data.len() {
            return Err(&EOF_WHILE_PARSING_STRING);
        }
        match data[head] {
            b'"' => {
                let segment = &data[at..head];
                if &value[read..] == segment {
                    return Ok((head + 1, true));
                } else {
                    return Ok((head + 1, false));
                }
            }
            b'\\' => {
                let mut scratch = [MaybeUninit::uninit(); 4];
                match parse_escape_inner(head + 1, data, &mut scratch) {
                    Ok((index, written)) => {
                        let segment = &data[at..head];
                        if value[read..].starts_with(segment) {
                            read += segment.len();
                            if value[read..].starts_with(unsafe {
                                std::slice::from_raw_parts(scratch.as_ptr() as *const u8, written)
                            }) {
                                read += written;
                                head = index;
                                at = index;
                                continue;
                            }
                        }
                        return Ok((skip_json_string_and_validate(index, data)?, false));
                    }
                    Err(()) => {
                        return Err(&DecodeError {
                            message: "Invalid escape sequence in string",
                        })
                    }
                }
            }
            _ => {
                return Err(&DecodeError {
                    message: "Control character detected in json",
                })
            }
        }
    }
}

pub(crate) fn skip_json_string_and_validate(
    mut at: usize,
    data: &[u8],
) -> Result<usize, &'static DecodeError> {
    // at += 1;

    loop {
        at = skip_to_escape(at, data);
        if at == data.len() {
            return Err(&EOF_WHILE_PARSING_STRING);
        }
        match data[at] {
            b'"' => return Ok(at + 1),
            b'\\' => {
                let mut scratch = [MaybeUninit::uninit(); 4];
                match parse_escape_inner(at + 1, data, &mut scratch) {
                    Ok((index, _written)) => {
                        at = index;
                        continue;
                    }
                    Err(()) => {
                        return Err(&DecodeError {
                            message: "Invalid escape sequence in string",
                        })
                    }
                }
            }
            _ => {
                return Err(&DecodeError {
                    message: "Control character detected in json",
                })
            }
        }
    }
}

pub(crate) fn parse_escape_inner(
    mut index: usize,
    read: &[u8],
    scratch: &mut [MaybeUninit<u8>; 4],
) -> Result<(usize, usize), ()> {
    let Some(ch) = read.get(index) else {
        return Err(());
    };
    index += 1;

    let unescaped = match ch {
        b'"' => b'"',
        b'\\' => b'\\',
        b'/' => b'/',
        b'b' => b'\x08',
        b'f' => b'\x0c',
        b'n' => b'\n',
        b'r' => b'\r',
        b't' => b'\t',
        // b'u' => {
        //     scratch.reserve(4);
        //     scratch.spare_capacity_mut().as_mut_ptr() as *mut [MaybeUninit<u8>;4]
        //     return parse_unicode_escape(index, read, scratch)
        // },
        b'u' => {
            return parse_unicode_escape(index, read, scratch);
        }
        _ => {
            return Err(());
        }
    };
    scratch[0].write(unescaped);

    Ok((index, 1))
}
/// Parses a JSON \u escape and appends it into the scratch space. Assumes `\u`
/// has just been read.
#[cold]
fn parse_unicode_escape(
    mut index: usize,
    read: &[u8],
    scratch: &mut [MaybeUninit<u8>; 4],
) -> Result<(usize, usize), ()> {
    let n = match read[index..] {
        [a, b, c, d, ..] => {
            index += 4;
            match decode_four_hex_digits(a, b, c, d) {
                Some(val) => val,
                // None => error(self, ErrorCode::InvalidEscape),
                None => {
                    return Err(());
                }
            }
        }
        _ => {
            // index = self.slice.len();
            // error(self, ErrorCode::EofWhileParsingString)
            return Err(());
        }
    };

    if !(0xD800..=0xDBFF).contains(&n) {
        // Every u16 outside of the surrogate ranges is guaranteed to be a
        // legal char.
        return Ok((index, push_wtf8_codepoint(n as u32, scratch)));
    }

    // n is a leading surrogate, we now expect a trailing surrogate.
    let n1 = n;

    if read.get(index..index + 2) != Some(b"\\u") {
        return Err(());
    }
    index += 2;

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
    Ok((index, push_wtf8_codepoint(n, scratch)))
}

/// Adds a WTF-8 codepoint to the end of the buffer. This is a more efficient
/// implementation of String::push. The codepoint may be a surrogate.
#[inline]
fn push_wtf8_codepoint(n: u32, scratch: &mut [MaybeUninit<u8>; 4]) -> usize {
    if n < 0x80 {
        scratch[0].write(n as u8);
        // scratch.push(n as u8);
        return 1;
    }

    unsafe {
        let ptr = scratch.as_mut_ptr() as *mut u8;

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

        encoded_len
    }
}

const fn decode_hex_val_slow(val: u8) -> Option<u8> {
    match val {
        b'0'..=b'9' => Some(val - b'0'),
        b'A'..=b'F' => Some(val - b'A' + 10),
        b'a'..=b'f' => Some(val - b'a' + 10),
        _ => None,
    }
}

const fn build_hex_table() -> [i8; 256] {
    let mut table = [0; 256];
    let mut ch = 0;
    while ch < 256 {
        table[ch] = match decode_hex_val_slow(ch as u8) {
            Some(val) => val as i8,
            None => -1,
        };
        ch += 1;
    }
    table
}

static HEX: [i8; 256] = build_hex_table();

fn decode_four_hex_digits(a: u8, b: u8, c: u8, d: u8) -> Option<u16> {
    let a = HEX[a as usize] as i32;
    let b = HEX[b as usize] as i32;
    let c = HEX[c as usize] as i32;
    let d = HEX[d as usize] as i32;

    let codepoint = (a << 12) | (b << 8) | (c << 4) | d;

    // A single sign bit check.
    if codepoint >= 0 {
        Some(codepoint as u16)
    } else {
        None
    }
}

// Likely useful in the future for things like form_urlencoded which has been
// temporarily removed.
// pub(crate) fn decode_two_hex_digits(a: u8, b: u8) -> Option<u8> {
//     let a = HEX[a as usize] as i16;
//     let b = HEX[b as usize] as i16;

//     let codepoint = (a << 4) | b;

//     // A single sign bit check.
//     if codepoint >= 0 {
//         Some(codepoint as u8)
//     } else {
//         None
//     }
// }

#[cfg(test)]
mod test {
    use super::*;
    #[test]
    fn escapes() {
        assert_eq!(
            decode_four_hex_digits(b'0', b'0', b'7', b'9').unwrap(),
            b'y' as u16
        );
        assert_eq!(
            decode_four_hex_digits(b'0', b'0', b'7', b'9').unwrap(),
            b'y' as u16
        );
    }
    #[test]
    fn json_skipping() {
        let input = br#""hell\no""#;
        let (offset, matches) = skip_json_string_and_eq(1, input, b"hell\no").unwrap();
        assert_eq!(offset, input.len());
        assert!(matches);
    }
}
