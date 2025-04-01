/// With helper for decoding a owned cow: `#[jsony(From with = owned_cow)]`
///
/// Well FromJson and FromBinary are implemented for some parameters of `Cow<'_, T>` they will
/// always try to borrow.
pub mod owned_cow {
    use std::borrow::Cow;

    use crate::{
        binary::Decoder,
        json::{DecodeError, Parser},
        FromBinary, FromJson,
    };

    pub fn decode_json<'de, 'a, T>(
        decoder: &mut Parser<'de>,
    ) -> Result<Cow<'a, T>, &'static DecodeError>
    where
        T: ?Sized + ToOwned,
        T::Owned: FromJson<'de>,
    {
        Ok(Cow::Owned(T::Owned::decode_json(decoder)?))
    }

    pub fn decode_binary<'de, 'a, T>(decoder: &mut Decoder<'de>) -> Cow<'a, T>
    where
        T: ?Sized + ToOwned,
        T::Owned: FromBinary<'de>,
    {
        Cow::Owned(T::Owned::decode_binary(decoder))
    }
}

/// With helper for a value that been encoded to a string containing JSON: `#[jsony(with = json_string)]`
pub mod json_string {
    use std::any::type_name;

    use crate::{
        binary::Decoder,
        json::{AlwaysString, DecodeError, Parser},
        BytesWriter, FromBinary, FromJson, TextWriter, ToBinary, ToJson,
    };

    pub fn encode_json<T: ToJson>(value: &T, output: &mut TextWriter) -> AlwaysString {
        crate::to_json(value).encode_json__jsony(output)
    }

    // note the lifetime here it will never be possible to borrow, due to the
    // the escaping.
    pub fn decode_json<T: for<'a> FromJson<'a>>(
        decoder: &mut Parser<'_>,
    ) -> Result<T, &'static DecodeError> {
        match decoder.at.take_string(&mut decoder.scratch) {
            Ok(inner_string) => match crate::from_json::<T>(&inner_string) {
                Ok(value) => Ok(value),
                Err(err) => {
                    decoder.report_error(format!(
                        "Failed to decode nested inner JSON of string for type {}: {err:?}",
                        type_name::<T>(),
                    ));
                    Err(&DecodeError {
                        message: "Nesting JSON failed to parse",
                    })
                }
            },
            Err(err) => return Err(err),
        }
    }

    pub fn encode_binary<T: ToJson + ?Sized>(value: &T, output: &mut BytesWriter) {
        crate::to_json(value).encode_binary(output);
    }

    pub fn decode_binary<'a, T: Default + FromJson<'a>>(decoder: &mut Decoder<'a>) -> T {
        match crate::from_json::<T>(<&'a str>::decode_binary(decoder)) {
            Ok(value) => value,
            Err(err) => {
                decoder.report_error(format_args!(
                    "Failed to decode inner JSON of string for type {}: {err:?}",
                    type_name::<T>()
                ));
                Default::default()
            }
        }
    }
}
