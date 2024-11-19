use std::collections::BTreeMap;

use jsony::{FromBinary, Jsony, ToBinary};

macro_rules! flat_stringify_tt {
    ({$($tt:tt)*}) => {concat!("{", $(flat_stringify_tt!($tt),)* "}")};
    ([$($tt:tt)*]) => {concat!("[", $(flat_stringify_tt!($tt),)* "]")};
    (($literal:literal)) => {$literal};
    ($tt:tt) => {stringify!($tt)};
}

macro_rules! compact_stringify {
    ($($tt:tt)*) => {concat!($(flat_stringify_tt!{$tt}),*)};
}

macro_rules! assert_json_encode_eq {
    ($json:tt, $($input:tt)* ) => {
        assert_eq!(
            compact_stringify!($json),
            jsony::to_json(&$($input)*).as_str(),
            "JSON Encoding to match input"
        )
    };
}

macro_rules! assert_json_decode_eq {
    ($json:tt, $($input:tt)* ) => {
        assert_eq!(
            $($input)*,
            jsony::from_json(compact_stringify!($json)).unwrap(),
            "JSON Decoding to match input"
        )
    };
}

macro_rules! assert_json_decode_eq_typed {
    ($ty:ty, $json:tt, $($input:tt)* ) => {
        assert_eq!(
            $($input)*,
            jsony::from_json::<$ty>(compact_stringify!($json)).unwrap(),
            "JSON Decoding to match input"
        )
    };
}

macro_rules! assert_json_eq {
    ($json:tt, $($input:tt)* ) => {
        assert_json_decode_eq!($json, $($input)*);
        assert_json_encode_eq!($json, $($input)*);
    };
}

// macro_rules! assert_json_sym_eq {
//     ($json:tt, $($input:tt)* ) => {
//         assert_eq!(
//             jsony::to_json(&$($input)*).as_str(),
//             compact_stringify!({$($expected)*})
//         )
//     };
// }

// for testing single with that supports every option
mod bool_as_int {
    use jsony::{
        json::DecodeError, BytesWriter, FromBinary, FromJson, TextWriter, ToBinary, ToJson,
    };

    pub fn json_encode(value: &bool, output: &mut TextWriter) {
        (*value as u32).json_encode__jsony(output);
    }

    pub fn json_decode(
        parser: &mut jsony::parser::Parser<'_>,
    ) -> Result<bool, &'static DecodeError> {
        Ok(<u32>::json_decode(parser)? != 0)
    }

    pub fn binary_encode(value: &bool, output: &mut BytesWriter) {
        (*value as u32).binary_encode(output)
    }

    pub fn binary_decode(decoder: &mut jsony::binary::Decoder<'_>) -> bool {
        u32::binary_decode(decoder) != 0
    }
}

// for testing generic decode
mod from_str {
    use std::str::FromStr;

    use jsony::{json::DecodeError, FromBinary};

    pub fn json_decode<T: FromStr>(
        parser: &mut jsony::parser::Parser<'_>,
    ) -> Result<T, &'static DecodeError>
    where
        <T as FromStr>::Err: std::fmt::Display,
    {
        match T::from_str(parser.take_string()?) {
            Ok(value) => Ok(value),
            Err(err) => {
                parser.report_error(format!("FromStr failed: {err}"));
                Err(&DecodeError {
                    message: "FromStr failed",
                })
            }
        }
    }

    pub fn binary_decode<T: FromStr + Default>(decoder: &mut jsony::binary::Decoder<'_>) -> T
    where
        <T as FromStr>::Err: std::fmt::Display,
    {
        match T::from_str(<&str>::binary_decode(decoder)) {
            Ok(value) => value,
            Err(err) => {
                decoder.report_error(format_args!("FromStr failed: {err}"));
                Default::default()
            }
        }
    }
}

mod to_lossy_str {
    use jsony::{BytesWriter, TextWriter, ToBinary, ToJson};

    pub fn json_encode(value: &[u8], output: &mut TextWriter) {
        String::from_utf8_lossy(value).json_encode__jsony(output);
    }

    pub fn binary_encode(value: &[u8], output: &mut BytesWriter) {
        String::from_utf8_lossy(value).binary_encode(output);
    }
}
// for testing generic encode
mod to_string {
    use jsony::{BytesWriter, TextWriter, ToBinary, ToJson};

    pub fn json_encode<T: ToString>(value: &T, output: &mut TextWriter) {
        value.to_string().json_encode__jsony(output);
    }

    pub fn binary_encode<T: ToString>(value: &T, output: &mut BytesWriter) {
        value.to_string().binary_encode(output)
    }
}

// for testing with that borrows lifetimes
mod utf8_as_bytes {
    use jsony::{json::DecodeError, FromBinary, FromJson};

    pub fn json_decode<'a>(
        parser: &mut jsony::parser::Parser<'a>,
    ) -> Result<&'a [u8], &'static DecodeError> {
        <&'a str>::json_decode(parser).map(|s| s.as_bytes())
    }

    pub fn binary_decode<'a>(decoder: &mut jsony::binary::Decoder<'a>) -> &'a [u8] {
        <&'a str>::binary_decode(decoder).as_bytes()
    }
}

// #[track_caller]
// fn assert_bin_roundtrip<T: ToBinary + for<'a> FromBinary<'a> + PartialEq + std::fmt::Debug>(
//     value: &T,
// ) {
//     let mut writer = jsony::BytesWriter::new();
//     value.binary_encode(&mut writer);
//     {
//         let mut decoder = jsony::binary::Decoder::new(writer.buffer_slice());
//         let decoded = T::binary_decode(&mut decoder);
//         assert_eq!(value, &decoded);
//     }
// }

#[track_caller]
fn assert_bin_equiv<
    'a,
    T: ToBinary + FromBinary<'a> + PartialEq + std::fmt::Debug,
    F: ToBinary + FromBinary<'a> + std::fmt::Debug,
>(
    value: &T,
    reference: &F,
) {
    let mut writer = jsony::BytesWriter::new();
    value.binary_encode(&mut writer);
    let i = writer.buffer_slice().len();
    reference.binary_encode(&mut writer);
    let (encoded_value, encoded_reference) = writer.buffer_slice().split_at(i);
    assert_eq!(
        encoded_value, encoded_reference,
        "Value and reference have same encoding representation"
    );
    {
        // safety: will be safe for all tests we do
        let mut decoder =
            jsony::binary::Decoder::new(unsafe { &*(encoded_reference as *const [u8]) });
        let decoded = T::binary_decode(&mut decoder);
        assert_eq!(value, &decoded);
    }
}

#[test]
fn with_attribute_struct() {
    #[derive(Jsony, PartialEq, Eq, Debug)]
    #[jsony(Binary, Json)]
    struct SimpleStruct {
        #[jsony(with = bool_as_int)]
        value: bool,
    }
    assert_json_eq!( { "value": 1 }, SimpleStruct { value: true });
    assert_json_eq!( { "value": 0 }, SimpleStruct { value: false });
    assert_bin_equiv(&SimpleStruct { value: true }, &1u32);
    assert_bin_equiv(&SimpleStruct { value: false }, &0u32);

    #[derive(Jsony, PartialEq, Debug)]
    #[jsony(Json)]
    struct Asymmetric {
        #[jsony(FromJson with = from_str, ToJson with = bool_as_int)]
        value: bool,
    }
    assert_json_encode_eq!( { "value": 1 }, Asymmetric { value: true });
    assert_json_decode_eq!( { "value": "true" }, Asymmetric { value: true });
}

#[test]
fn with_attribute_enum() {
    #[derive(Jsony, PartialEq, Eq, Debug)]
    #[jsony(Binary, Json)]
    enum SimpleEnum<'a> {
        Slice {
            #[jsony(From with = utf8_as_bytes, To with = to_lossy_str)]
            value: &'a [u8],
        },
        Num(#[jsony(From with = from_str, To with = to_string)] u32),
    }

    assert_json_eq!( { "Slice": { "value": "hello" } }, SimpleEnum::Slice { value: b"hello" });
    assert_json_eq!( { "Num": "22" }, SimpleEnum::Num(22));
    assert_bin_equiv(&SimpleEnum::Slice { value: b"hello" }, &(0u8, "hello"));
    assert_bin_equiv(&SimpleEnum::Num(99), &(1u8, "99"));
}

#[test]
fn enum_variations() {
    #[derive(Jsony, Debug, PartialEq)]
    #[jsony(Json)]
    enum ExternallyTagged<'a> {
        Record {
            alpha: &'a str,
            beta: u32,
        },
        RecordFlatten {
            alpha: &'a str,
            #[jsony(flatten)]
            map: BTreeMap<&'a str, u32>,
        },
        Single(bool),
        Empty,
    }

    assert_json_eq!(
        { "Record": { "alpha": "abc", "beta": 7 } },
        ExternallyTagged::Record { alpha: "abc", beta: 7 }
    );

    assert_json_eq!(
        { "RecordFlatten": { "alpha": "abc" , "beta": 7, "delta": 90 } },
        ExternallyTagged::RecordFlatten { alpha: "abc", map: [
            ("beta", 7),
            ("delta", 90)
        ].iter().cloned().collect() }
    );

    assert_json_eq!(
        { "Single": true },
        ExternallyTagged::Single(true)
    );

    assert_json_eq!("Empty", ExternallyTagged::Empty);

    #[derive(Jsony, Debug, PartialEq)]
    #[jsony(Json, tag = "type")]
    enum InternallyTagged<'a> {
        Record {
            alpha: &'a str,
            beta: u32,
        },
        RecordFlatten {
            alpha: &'a str,
            #[jsony(flatten)]
            map: BTreeMap<&'a str, u32>,
        },
        // Single(bool), compile-time error
        Empty,
    }

    assert_json_eq!(
        { "type": "Record", "alpha": "abc" , "beta": 7 },
        InternallyTagged::Record { alpha: "abc", beta: 7 }
    );

    assert_json_eq!(
        { "type": "RecordFlatten", "alpha": "abc", "beta": 7, "delta": 90 },
        InternallyTagged::RecordFlatten { alpha: "abc", map: [
            ("beta", 7),
            ("delta", 90)
        ].iter().cloned().collect() }
    );

    assert_json_eq!(
        { "type": "Empty" },
        InternallyTagged::Empty
    );

    #[derive(Jsony, Debug, PartialEq)]
    #[jsony(Json, tag = "type", content = "data")]
    enum AdjacentlyTagged<'a> {
        Record {
            alpha: &'a str,
            beta: u32,
        },
        RecordFlatten {
            alpha: &'a str,
            #[jsony(flatten)]
            map: BTreeMap<&'a str, u32>,
        },
        Single(bool),
        Empty,
    }

    assert_json_eq!(
        { "type": "Record", "data": { "alpha": "abc" , "beta": 7 } },
        AdjacentlyTagged::Record { alpha: "abc", beta: 7 }
    );

    assert_json_eq!(
        { "type": "RecordFlatten", "data": { "alpha": "abc", "beta": 7, "delta": 90 } },
        AdjacentlyTagged::RecordFlatten { alpha: "abc", map: [
            ("beta", 7),
            ("delta", 90)
        ].iter().cloned().collect() }
    );

    assert_json_eq!(
        { "type": "Single", "data": true },
        AdjacentlyTagged::Single(true)
    );

    assert_json_eq!(
        { "type": "Empty" },
        AdjacentlyTagged::Empty
    );

    #[derive(Jsony, Debug, PartialEq)]
    #[jsony(Json, untagged)]
    enum Untagged<'a> {
        Record {
            alpha: &'a str,
            beta: u32,
        },
        RecordFlatten {
            beta: &'a str,
            #[jsony(flatten)]
            map: BTreeMap<&'a str, u32>,
        },
        Single(bool),
    }

    assert_json_eq!(
        { "alpha": "abc" , "beta": 7 },
        Untagged::Record { alpha: "abc", beta: 7 }
    );

    assert_json_eq!(
        { "beta": "abc", "delta": 90, "nitro": 30  },
        Untagged::RecordFlatten { beta: "abc", map: [
            ("delta", 90),
            ("nitro", 30)
        ].iter().cloned().collect() }
    );

    assert_json_eq!(true, Untagged::Single(true));
}

#[test]
fn skip() {
    #[derive(Jsony, PartialEq, Debug)]
    #[jsony(Json)]
    struct S1<'a> {
        #[jsony(skip_if = |v| v.len() == 2)]
        x: &'a str,
        #[jsony(skip)]
        y: &'a str,
        #[jsony(skip, default = "z_default")]
        z: &'a str,
        #[jsony(ToJson skip, default = "u_default")]
        u: &'a str,
        #[jsony(FromJson skip, default = "w_default")]
        w: &'a str,
    }
    assert_json_decode_eq!({
        "x": "ab",
        "y": 21, // since it is skipped it doesn't matter what type it is
        "z": "ignore me too",
        "u": "don't ignore me",
        "w": "ignore me as well"
    }, S1{
        x: "ab",              // don't skip_if as only applies to `To` traits
        y: "",                // skip with Default::default()
        z: "z_default",       // skip with provided default
        u: "don't ignore me", // don't skip as it was enabled only for `ToJson`
        w: "w_default",       // skip as it was enabled for `FromJson`
    });

    assert_json_encode_eq!({
        // "x": .., // was skipped since v.len() == 2
        // "y": .., // was skipped unconditionally
        // "z": .., // so was 'z'
        // "u": .., // skipped as well since it is enabled for `ToJson`
        "w": "don't skip me" // Not skipped since only enabled for `FromJson`
    }, S1{
        x: "ab",
        y: "skip me",
        z: "me as well",
        u: "and also me",
        w: "don't skip me",
    });

    assert_json_encode_eq!({
        "x": "abc", // was not skipped since v.len() != 2
        // "y": .., // was skipped unconditionally
        // "z": .., // so was 'z'
        // "u": .., // skipped as well since it is enabled for `ToJson`
        "w": "don't skip me" // Not skipped since only enabled for `FromJson`
    }, S1{
        x: "abc",
        y: "skip me",
        z: "me as well",
        u: "and also me",
        w: "don't skip me",
    });

    #[derive(Jsony, PartialEq, Debug)]
    #[jsony(Json)]
    enum E1<'a> {
        V1 {
            #[jsony(skip_if = |v| v.len() == 2)]
            x: &'a str,
            #[jsony(skip)]
            y: &'a str,
            #[jsony(skip, default = "z_default")]
            z: &'a str,
            #[jsony(ToJson skip, default = "u_default")]
            u: &'a str,
            #[jsony(FromJson skip, default = "w_default")]
            w: &'a str,
        },
    }
    assert_json_decode_eq!({
        "V1": {
            "x": "ab",
            "y": 21, // since it is skipped it doesn't matter what type it is
            "z": "ignore me too",
            "u": "don't ignore me",
            "w": "ignore me as well"
        }
    }, E1::V1{
        x: "ab",              // don't skip_if as only applies to `To` traits
        y: "",                // skip with Default::default()
        z: "z_default",       // skip with provided default
        u: "don't ignore me", // don't skip as it was enabled only for `ToJson`
        w: "w_default",       // skip as it was enabled for `FromJson`
    });

    assert_json_encode_eq!({
        "V1": {
            // "x": .., // was skipped since v.len() == 2
            // "y": .., // was skipped unconditionally
            // "z": .., // so was 'z'
            // "u": .., // skipped as well since it is enabled for `ToJson`
            "w": "don't skip me" // Not skipped since only enabled for `FromJson`
        }
    }, E1::V1{
        x: "ab",
        y: "skip me",
        z: "me as well",
        u: "and also me",
        w: "don't skip me",
    });

    assert_json_encode_eq!({
        "V1": {
            "x": "abc", // was not skipped since v.len() != 2
            // "y": .., // was skipped unconditionally
            // "z": .., // so was 'z'
            // "u": .., // skipped as well since it is enabled for `ToJson`
            "w": "don't skip me" // Not skipped since only enabled for `FromJson`
        }
    }, E1::V1{
        x: "abc",
        y: "skip me",
        z: "me as well",
        u: "and also me",
        w: "don't skip me",
    });

    #[derive(Jsony, PartialEq, Debug)]
    #[jsony(Binary)]
    struct Binary<'a> {
        #[jsony(skip)]
        a: &'a str,
        #[jsony(skip, default = "foo")]
        b: &'a str,
        c: u32,
    }

    let input = Binary {
        a: "test1",
        b: "test2",
        c: 32,
    };
    let encoded = jsony::to_binary(&input);
    let decoded: Binary = jsony::from_binary(&encoded).unwrap();
    assert_eq!(
        decoded,
        Binary {
            a: "",
            b: "foo",
            c: 32
        }
    )
}

#[test]
fn other_variant() {
    #[derive(Debug, Jsony, PartialEq, Eq)]
    enum Stringly {
        Alpha,
        #[jsony(other)]
        Other,
        Beta,
    }

    assert_json_decode_eq_typed!(
        Vec<Stringly>,
        ["Alpha", "Unknown", "Beta"],
        vec![Stringly::Alpha, Stringly::Other, Stringly::Beta]
    );

    #[derive(Debug, Jsony, PartialEq, Eq)]
    enum StringlyCapture<'a> {
        Alpha,
        #[jsony(other)]
        Other(&'a str),
        Beta,
    }

    assert_json_decode_eq_typed!(
        Vec<StringlyCapture>,
        ["Alpha", "Unknown", "Beta"],
        vec![
            StringlyCapture::Alpha,
            StringlyCapture::Other("Unknown"),
            StringlyCapture::Beta
        ]
    );

    #[derive(Debug, Jsony, PartialEq, Eq)]
    enum StringlyCaptureStruct<'a> {
        Alpha,
        #[jsony(other)]
        Other {
            field: &'a str,
        },
        Beta,
    }

    assert_json_decode_eq_typed!(
        Vec<StringlyCaptureStruct>,
        ["Alpha", "Unknown", "Beta"],
        vec![
            StringlyCaptureStruct::Alpha,
            StringlyCaptureStruct::Other { field: "Unknown" },
            StringlyCaptureStruct::Beta
        ]
    );

    #[derive(Debug, Jsony, PartialEq, Eq)]
    enum MixedCaptureStruct<'a> {
        Alpha {
            value: &'a str,
        },
        #[jsony(other)]
        Other {
            field: &'a str,
        },
        Beta,
    }

    assert_json_decode_eq_typed!(
        Vec<MixedCaptureStruct>,
        [{"Alpha": {"value": "Val"}}, "Unknown", "Beta"],
        vec![
            MixedCaptureStruct::Alpha{value: "Val"},
            MixedCaptureStruct::Other { field: "Unknown" },
            MixedCaptureStruct::Beta
        ]
    );
    #[derive(Debug, Jsony, PartialEq, Eq)]
    #[jsony(tag = "kind")]
    enum TaggedCapture<'a> {
        Alpha {
            value: &'a str,
        },
        Beta,
        #[jsony(other)]
        Other {
            field: &'a str,
        },
    }

    assert_json_decode_eq_typed!(
        Vec<TaggedCapture>,
        [
            {"kind": "Alpha", "value": "Val"},
            {"kind": "Unknown", "field_to_ignore": [{}, true]},
            {"field_to_ignore": [{}, true], "kind": "Unknown"},
            {"kind": "Beta"}
        ],
        vec![
            TaggedCapture::Alpha{value: "Val"},
            TaggedCapture::Other { field: "Unknown" },
            TaggedCapture::Other { field: "Unknown" },
            TaggedCapture::Beta
        ]
    );
    assert_json_decode_eq_typed!(
        Vec<TaggedCapture>,
        [
            {"kind": "Alpha", "value": "Val"},
            {"kind": "Unknown"},
            {"kind": "Beta"}
        ],
        vec![
            TaggedCapture::Alpha{value: "Val"},
            TaggedCapture::Other { field: "Unknown" },
            TaggedCapture::Beta
        ]
    );

    #[derive(Debug, Jsony, PartialEq, Eq)]
    #[jsony(tag = "kind", content = "data")]
    enum ContentCapture<'a> {
        Alpha {
            value: &'a str,
        },
        Beta,
        #[jsony(other)]
        Other {
            field: &'a str,
        },
    }
    assert_json_decode_eq_typed!(
        Vec<ContentCapture>,
        [
            {"kind": "Alpha", "data": {"value": "Val"}},
            {"kind": "Unknown1"},
            {"kind": "Unknown2", "data": false},
            {"kind": "Beta"}
        ],
        vec![
            ContentCapture::Alpha{value: "Val"},
            ContentCapture::Other { field: "Unknown1" },
            ContentCapture::Other { field: "Unknown2" },
            ContentCapture::Beta
        ]
    );
}
