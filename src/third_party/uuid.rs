use uuid::{Uuid, fmt::Hyphenated};

use crate::{
    FromBinary, FromJson, ToBinary, ToJson,
    json::{AlwaysString, DecodeError},
    text::FromText,
};

impl<'a> FromText<'a> for Uuid {
    fn from_text(
        _: &mut crate::text::Ctx<'a>,
        text: &str,
    ) -> Result<Self, &'static crate::json::DecodeError> {
        match Uuid::parse_str(text) {
            Ok(value) => Ok(value),
            Err(_) => Err(&DecodeError {
                message: "Invalid UUID",
            }),
        }
    }
}

// SAFETY: on success `Uuid::parse_str` returns a valid `Uuid`, which is written
// exactly once into caller-provided storage. Parser or UUID errors return before
// initializing `dest`.
unsafe impl<'a> FromJson<'a> for Uuid {
    unsafe fn emplace_from_json(
        dest: std::ptr::NonNull<()>,
        parser: &mut crate::parser::Parser<'a>,
    ) -> Result<(), &'static DecodeError> {
        match Uuid::parse_str(parser.take_string()?) {
            Ok(value) => {
                // SAFETY: `FromJson::emplace_from_json` callers provide
                // writable storage for the target type.
                unsafe {
                    dest.cast::<Uuid>().write(value);
                }
                Ok(())
            }
            Err(_) => Err(&DecodeError {
                message: "Invalid UUID",
            }),
        }
    }
}

impl ToJson for Uuid {
    type Kind = AlwaysString;

    fn encode_json__jsony(&self, output: &mut crate::TextWriter) -> AlwaysString {
        let mut buffer = [0; Hyphenated::LENGTH];
        output.start_json_string();
        output.push_str(uuid::fmt::Hyphenated::from_uuid(*self).encode_lower(&mut buffer));
        output.end_json_string();
        AlwaysString
    }
}

// SAFETY: the locked `uuid` crate defines `Uuid` as `#[repr(transparent)]` over
// `uuid::Bytes`, and `Bytes` is `[u8; 16]`. Every 16-byte array is accepted by
// `Uuid::from_bytes`, so raw-copy POD decoding cannot create an invalid `Uuid`.
unsafe impl<'a> FromBinary<'a> for Uuid {
    const POD: bool = true;

    fn decode_binary(decoder: &mut crate::binary::Decoder<'a>) -> Self {
        Uuid::from_bytes(*decoder.byte_array::<16>())
    }
}

// SAFETY: the encoder writes the same 16 public bytes returned by
// `Uuid::as_bytes`, matching the `FromBinary` representation.
unsafe impl ToBinary for Uuid {
    const POD: bool = true;

    fn encode_binary(&self, encoder: &mut crate::BytesWriter) {
        encoder.push_bytes(self.as_bytes());
    }
}

#[cfg(test)]
mod test {
    use super::*;
    #[test]
    fn json() {
        let value = Uuid::from_u128(0xa1a2a3a4_b1b2_c1c2_d1d2_d3d4d5d6d7d8);
        let encoded = "\"a1a2a3a4-b1b2-c1c2-d1d2-d3d4d5d6d7d8\"";
        assert_eq!(crate::to_json(&value).as_str(), encoded);
        assert_eq!(crate::from_json::<Uuid>(encoded).unwrap(), value)
    }
    #[test]
    fn binary() {
        let encoded = 0xa1a2a3a4_b1b2_c1c2_d1d2_d3d4d5d6d7d8u128.to_be_bytes();
        let value = Uuid::from_bytes(encoded);
        assert_eq!(crate::to_binary(&value), encoded);
        assert_eq!(crate::from_binary::<Uuid>(&encoded).unwrap(), value)
    }
}
