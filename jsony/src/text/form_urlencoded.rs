use std::mem::MaybeUninit;

use crate::json::DecodeError;

use super::Ctx;

pub(crate) struct FormDecoder<'a> {
    pub ctx: Ctx<'a>,
    buffer: Vec<u8>,
    at: usize,
}

#[repr(C)]
union PtrOrOffset {
    ptr: *const u8,
    offset: usize,
}
impl<'input> FormDecoder<'input> {
    pub fn extract_named_fields<'this, 'output_buffer, 'maybe_input: 'input + 'this>(
        &'this mut self,
        out: &'output_buffer mut MaybeUninit<[Option<&'maybe_input str>; 32]>,
        field_names: &[&str],
    ) -> Result<&'input [Option<&'maybe_input str>], &'static DecodeError> {
        self.buffer.clear();
        let ptrs = out.as_mut_ptr() as *mut (PtrOrOffset, isize);
        for i in 0..field_names.len() {
            unsafe {
                ptrs.add(i).write((PtrOrOffset { offset: usize::MAX }, 0));
            }
        }
        let found = unsafe {
            std::slice::from_raw_parts_mut(ptrs as *mut (PtrOrOffset, isize), field_names.len())
        };
        'outer: loop {
            if self.ctx.data.len() == self.at {
                break;
            }
            let key_start = self.at;
            let Some(key_len) = memchr::memchr(b'=', &self.ctx.data[self.at..]) else {
                return Err(&DecodeError {
                    message: "Expected =",
                });
            };
            let key_end = key_len + self.at;
            let value_end = match memchr::memchr(b'&', &self.ctx.data[key_end + 1..]) {
                Some(value_length) => {
                    self.at = value_length + key_end + 2; // 2: for '=' and '&'
                    value_length + key_end + 1
                }
                None => {
                    self.at = self.ctx.data.len();
                    self.ctx.data.len()
                }
            };
            let raw_key = &self.ctx.data[key_start..key_end];
            let seq_index = 'index: {
                let key = match self.form_decode(raw_key) {
                    Some((start, end)) => &self.buffer[start..][..end],
                    None => raw_key,
                };
                for (i, field_name) in field_names.iter().enumerate() {
                    if key == field_name.as_bytes() {
                        break 'index i;
                    }
                }
                continue 'outer;
            };

            let raw_value = &self.ctx.data[key_end + 1..value_end];
            match self.form_decode(raw_value) {
                Some((start, end)) => {
                    found[seq_index].0.offset = start;
                    found[seq_index].1 =
                        (end - start) as isize | (1 << ((std::mem::size_of::<usize>() * 8) - 1));
                }
                None => {
                    found[seq_index].0.ptr = raw_value.as_ptr();
                    found[seq_index].1 = raw_value.len() as isize;
                }
            }
        }
        for i in 0..field_names.len() {
            unsafe {
                let (ptr, size) = ptrs.add(i).read();
                if ptr.offset == usize::MAX {
                    ptrs.add(i).cast::<Option<&str>>().write(None);
                    continue;
                };
                let bytes = if size < 0 {
                    let size = size & !(1 << ((std::mem::size_of::<usize>() * 8) - 1));
                    &self.buffer[ptr.offset as usize..][..size as usize]
                } else {
                    std::slice::from_raw_parts(ptr.ptr, size as usize)
                };
                match std::str::from_utf8(bytes) {
                    Ok(value) => ptrs.add(i).cast::<Option<&str>>().write(Some(value)),
                    Err(_err) => {
                        return Err(&DecodeError {
                            message: "Non UTF8 value",
                        });
                    }
                }
            }
        }
        unsafe {
            Ok(std::slice::from_raw_parts(
                ptrs as *const Option<&str>,
                field_names.len(),
            ))
        }
    }
    pub fn new(form: &'input [u8]) -> Self {
        Self {
            buffer: Vec::new(),
            ctx: Ctx {
                data: form,
                error: None,
            },
            at: 0,
        }
    }
    pub fn form_decode(&mut self, value: &[u8]) -> Option<(usize, usize)> {
        let initial = self.buffer.len();
        let mut written = 0;
        let mut i = 0;
        while let Some(ch) = value.get(i) {
            i += 1;
            match *ch {
                b'%' => {
                    if let [a, b, ..] = &value[i..] {
                        if let Some(ch) = crate::strings::decode_two_hex_digits(*a, *b) {
                            self.buffer.extend_from_slice(&value[written..i - 1]);
                            self.buffer.push(ch);
                            written = i + 2;
                            i += 2;
                        }
                    }
                }
                b'+' => {
                    self.buffer.extend_from_slice(&value[written..i - 1]);
                    self.buffer.push(b' ');
                    written = i;
                }
                _ => continue,
            }
        }
        if written == 0 {
            return None;
        } else {
            self.buffer.extend_from_slice(&value[written..]);
            Some((initial, self.buffer.len() - initial))
        }
    }
}

#[cfg(test)]
mod test {
    use super::*;

    #[track_caller]
    fn assert_decodes(input: &[u8], names: &[&str], expected: &[Option<&str>]) {
        let mut fields = MaybeUninit::<[Option<&str>; 32]>::uninit();
        let mut decoder = FormDecoder::new(input);
        let result = decoder.extract_named_fields(&mut fields, names).unwrap();
        assert_eq!(result, expected);
    }

    #[test]
    fn form_decode() {
        assert_decodes(
            b"k1=val1&k2=val2", //
            &["k1", "k2"],
            &[Some("val1"), Some("val2")],
        );
        assert_decodes(
            b"k1=val1&k2=val2", //
            &["k2", "k1"],
            &[Some("val2"), Some("val1")],
        );
        assert_decodes(
            b"k1=val1&k2=val2", //
            &["k3", "k1"],
            &[None, Some("val1")],
        );
        assert_decodes(
            b"k1=val1&k2=val2", //
            &["k3"],
            &[None],
        );
        assert_decodes(
            b"k1=val1&k2=val2", //
            &["k3"],
            &[None],
        );
        assert_decodes(
            b"k1=va%20l+ue&k2=val2", //
            &["k1"],
            &[Some("va l ue")],
        );
        assert_decodes(
            b"k1=va%20l+ue&k+2%20=val2", //
            &["k1", "k 2 "],
            &[Some("va l ue"), Some("val2")],
        );
    }
}
