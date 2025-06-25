use std::{
    borrow::Cow,
    collections::{BTreeMap, HashMap},
    mem::MaybeUninit,
};

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

macro_rules! assert_encode_json_eq {
    ($json:tt, $($input:tt)* ) => {
        assert_eq!(
            compact_stringify!($json),
            jsony::to_json(&$($input)*).as_str(),
            "JSON Encoding to match input"
        )
    };
}

macro_rules! assert_decode_failure {
    ($json:tt, $ty:ty, $error_name:ident ) => {
        assert_eq!(
            jsony::from_json::<$ty>(compact_stringify!($json))
                .err()
                .expect("decoding to fail")
                .decoding_error(),
            &jsony::error::$error_name,
            "JSON Decode Error to match"
        )
    };
}

macro_rules! assert_decode_json_eq {
    ($json:tt, $($input:tt)* ) => {
        assert_eq!(
            $($input)*,
            jsony::from_json(compact_stringify!($json)).unwrap(),
            "JSON Decoding to match input"
        )
    };
}

macro_rules! assert_decode_json_eq_typed {
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
        assert_decode_json_eq!($json, $($input)*);
        assert_encode_json_eq!($json, $($input)*);
    };
}

macro_rules! assert_binary_round_trip {
    ($($input:tt)*) => {
        {
            let value = $($input)*;
            assert_eq!(
                value,
                jsony::from_binary(&jsony::to_binary(&value)).expect("to decode")
            )
        }
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

    pub fn encode_json(value: &bool, output: &mut TextWriter) {
        (*value as u32).encode_json__jsony(output);
    }

    pub fn decode_json(
        parser: &mut jsony::parser::Parser<'_>,
    ) -> Result<bool, &'static DecodeError> {
        Ok(<u32>::decode_json(parser)? != 0)
    }

    pub fn encode_binary(value: &bool, output: &mut BytesWriter) {
        (*value as u32).encode_binary(output)
    }

    pub fn decode_binary(decoder: &mut jsony::binary::Decoder<'_>) -> bool {
        u32::decode_binary(decoder) != 0
    }
}

// for testing generic decode
mod from_str {
    use std::str::FromStr;

    use jsony::{json::DecodeError, FromBinary};

    pub fn decode_json<T: FromStr>(
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

    pub fn decode_binary<T: FromStr + Default>(decoder: &mut jsony::binary::Decoder<'_>) -> T
    where
        <T as FromStr>::Err: std::fmt::Display,
    {
        match T::from_str(<&str>::decode_binary(decoder)) {
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

    pub fn encode_json(value: &[u8], output: &mut TextWriter) {
        String::from_utf8_lossy(value).encode_json__jsony(output);
    }

    pub fn encode_binary(value: &[u8], output: &mut BytesWriter) {
        String::from_utf8_lossy(value).encode_binary(output);
    }
}
// for testing generic encode
mod to_string {
    use jsony::{BytesWriter, TextWriter, ToBinary, ToJson};

    pub fn encode_json<T: ToString>(value: &T, output: &mut TextWriter) {
        value.to_string().encode_json__jsony(output);
    }

    pub fn encode_binary<T: ToString>(value: &T, output: &mut BytesWriter) {
        value.to_string().encode_binary(output)
    }
}

// for testing with that borrows lifetimes
mod utf8_as_bytes {
    use jsony::{json::DecodeError, FromBinary, FromJson};

    pub fn decode_json<'a>(
        parser: &mut jsony::parser::Parser<'a>,
    ) -> Result<&'a [u8], &'static DecodeError> {
        <&'a str>::decode_json(parser).map(|s| s.as_bytes())
    }

    pub fn decode_binary<'a>(decoder: &mut jsony::binary::Decoder<'a>) -> &'a [u8] {
        <&'a str>::decode_binary(decoder).as_bytes()
    }
}

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
    value.encode_binary(&mut writer);
    let i = writer.buffer_slice().len();
    reference.encode_binary(&mut writer);
    let (encoded_value, encoded_reference) = writer.buffer_slice().split_at(i);
    assert_eq!(
        encoded_value, encoded_reference,
        "Value and reference have same encoding representation"
    );
    {
        // safety: will be safe for all tests we do
        let mut decoder =
            jsony::binary::Decoder::new(unsafe { &*(encoded_reference as *const [u8]) });
        let decoded = T::decode_binary(&mut decoder);
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
    assert_encode_json_eq!( { "value": 1 }, Asymmetric { value: true });
    assert_decode_json_eq!( { "value": "true" }, Asymmetric { value: true });
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
    assert_decode_json_eq!({
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

    assert_encode_json_eq!({
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

    assert_encode_json_eq!({
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
    assert_decode_json_eq!({
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

    assert_encode_json_eq!({
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

    assert_encode_json_eq!({
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

    assert_decode_json_eq_typed!(
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

    assert_decode_json_eq_typed!(
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

    assert_decode_json_eq_typed!(
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

    assert_decode_json_eq_typed!(
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

    assert_decode_json_eq_typed!(
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
    assert_decode_json_eq_typed!(
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
    assert_decode_json_eq_typed!(
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

#[test]
fn ignore_tag_adjacent_fields() {
    #[derive(Debug, Jsony, PartialEq, Eq)]
    enum Failing {
        Tuple(u32),
        Struct { value: u32 },
    }
    assert_decode_failure! {
        { },
        Failing,
        EMPTY_OBJECT_FOR_EXTERNALLY_TAGGED_ENUM
    }

    assert_decode_failure! {
        { "Tuple": 43, "Nice": 43 },
        Failing,
        MULTIPLE_FIELDS_FOR_EXTERNALLY_TAGGED_ENUM
    }

    assert_decode_failure! {
        { "Nice": 43, "Tuple": 43 },
        Failing,
        UNKNOWN_VARIANT
    }

    #[derive(Debug, Jsony, PartialEq, Eq)]
    #[jsony(ignore_tag_adjacent_fields)]
    enum Ignoring {
        Tuple(u32),
        Struct { value: u32 },
    }

    assert_decode_failure! {
        { },
        Ignoring,
        EMPTY_OBJECT_FOR_EXTERNALLY_TAGGED_ENUM
    }
    assert_decode_json_eq! {
        { "Tuple": 43, "Nice": 43 },
        Ignoring::Tuple(43)
    }

    assert_decode_json_eq! {
        { "Nice": 43, "Tuple": 43 },
        Ignoring::Tuple(43)
    }

    assert_decode_failure! {
        { "Foo": 43 },
        Ignoring,
        NO_FIELD_MATCHED_AN_ENUM_VARIANT
    }

    assert_decode_failure! {
        { "Foo": 43, "wow": {"Tuple": 55} },
        Ignoring,
        NO_FIELD_MATCHED_AN_ENUM_VARIANT
    }

    assert_decode_json_eq! {
        { "Nice": 43, "Struct": { "value": 99 }  },
        Ignoring::Struct{value: 99}
    }

    #[derive(Debug, Jsony, PartialEq, Eq)]
    #[jsony(ignore_tag_adjacent_fields)]
    enum IgnoringWithString {
        Tuple(u32),
        Struct { value: u32 },
        Stringly,
    }

    assert_decode_json_eq! {
        { "Nice": 43, "Struct": { "value": 99 }  },
        IgnoringWithString::Struct{value: 99}
    }

    assert_decode_json_eq! {
        "Stringly",
        IgnoringWithString::Stringly
    }
}

#[test]
fn struct_field_alias() {
    #[derive(Debug, Jsony, PartialEq, Eq)]
    struct Sys {
        #[jsony(alias = "james")]
        alpha: u32,
        #[jsony(alias = "alan", rename = "greg")]
        beta: bool,
    }

    assert_decode_json_eq! {
        { "alpha": 42, "alan": true },
        Sys{alpha: 42, beta: true}
    }

    assert_decode_json_eq! {
        { "james": 42, "greg": true },
        Sys{alpha: 42, beta: true}
    }
}

#[test]
fn with_owned_cow_helper() {
    use jsony::helper::owned_cow;

    #[derive(Debug, Jsony, PartialEq, Eq)]
    #[jsony(Binary, Json)]
    struct Sys<'a> {
        #[jsony(From with = owned_cow)]
        shared: Cow<'a, [&'a str]>,
    }

    assert_json_eq!(
        {"shared": ["hello", "nice"]},
        Sys{ shared: Cow::Borrowed(&["hello", "nice"])}
    );

    assert_binary_round_trip!(Sys {
        shared: Cow::Borrowed(&["hello", "nice"])
    })
}

#[test]
fn maps_with_numeric_keys() {
    assert_json_eq!(
        {"2": false},
        HashMap::<u32, bool>::from_iter([(2, false)])
    );
    assert_json_eq!(
        {"-1": true, "2": false, "3": true},
        BTreeMap::<i8, bool>::from_iter([(-1, true), (2, false), (3, true)])
    );
}

#[test]
fn with_json_string_helper() {
    use jsony::helper::json_string;
    #[derive(Debug, Jsony, PartialEq, Eq, Default)]
    #[jsony(Json)]
    struct Inner {
        a: i32,
        b: i32,
    }

    #[derive(Debug, Jsony, PartialEq, Eq)]
    #[jsony(Binary, Json)]
    struct Sys {
        #[jsony(with = json_string)]
        double_json: Inner,
    }

    assert_json_eq!(
        {"double_json":"{\"a\":10,\"b\":5}"},
        Sys {
            double_json: Inner{a: 10, b: 5}
        }
    );

    assert_binary_round_trip!(Sys {
        double_json: Inner { a: 10, b: 5 }
    })
}

#[test]
fn binary_pod() {
    #[derive(Debug, Jsony, PartialEq, Clone)]
    #[jsony(Binary, zerocopy)]
    #[repr(C)]
    struct Pod {
        alpha: u8,
        beta: u8,
        canary: u16,
        delta: f32,
        echo: [u8; 4],
    }

    #[derive(Debug, Jsony, PartialEq)]
    #[jsony(Binary, transparent)]
    #[repr(transparent)]
    struct TransparentPod(Pod);

    assert!(<TransparentPod as jsony::FromBinary>::POD);
    assert!(<TransparentPod as jsony::ToBinary>::POD);

    #[derive(Jsony)]
    #[jsony(Binary, transparent)]
    #[repr(transparent)]
    struct TransparentNotPod(String);
    assert!(!<TransparentNotPod as jsony::FromBinary>::POD);
    assert!(!<TransparentNotPod as jsony::ToBinary>::POD);

    let inputs: &[Pod] = &[
        Pod {
            alpha: 32,
            beta: 243,
            canary: 32,
            delta: 24.23,
            echo: [1, 2, 3, 4],
        },
        Pod {
            alpha: 12,
            beta: 58,
            canary: 12,
            delta: 989.23,
            echo: [255, 0, 1, 3],
        },
    ];
    assert_binary_round_trip!(inputs[0].clone());
    assert_binary_round_trip!(TransparentPod(inputs[0].clone()));
    let mut buffer = MaybeUninit::<[u32; 32]>::uninit();
    {
        // make suer the slice will be aligned.
        let aligned_buffer = unsafe {
            std::slice::from_raw_parts_mut(
                (buffer.as_mut_ptr() as *mut MaybeUninit<u8>).add(3),
                32 * size_of::<u32>() - 3,
            )
        };
        let aligned_encoded = jsony::to_binary_into(&inputs, aligned_buffer);
        assert_eq!(
            inputs,
            jsony::from_binary::<&[Pod]>(&aligned_encoded).expect("to decode since aligned")
        );
        let cowify = jsony::from_binary::<Cow<'_, [Pod]>>(&aligned_encoded)
            .expect("to decode since aligned");
        if let Cow::Owned(_) = cowify {
            panic!("Expected to borrow since aligned");
        }
        assert_eq!(inputs, &*cowify);
        assert_eq!(
            inputs,
            jsony::from_binary::<Vec<Pod>>(&aligned_encoded).expect("to decode since aligned")
        );
        assert_eq!(
            inputs,
            jsony::from_binary::<Vec<TransparentPod>>(&aligned_encoded)
                .expect("to decode since aligned")
                .into_iter()
                .map(|pod| { pod.0 })
                .collect::<Vec<_>>()
        );
    }

    {
        let unaligned_buffer = unsafe {
            std::slice::from_raw_parts_mut(
                buffer.as_mut_ptr() as *mut MaybeUninit<u8>,
                32 * size_of::<u32>(),
            )
        };
        let unaligned_encoded = jsony::to_binary_into(&inputs, unaligned_buffer);
        assert!(jsony::from_binary::<&[Pod]>(&unaligned_encoded).is_err());
        let cowify = jsony::from_binary::<Cow<'_, [Pod]>>(&unaligned_encoded)
            .expect("to decode since aligned");
        if let Cow::Borrowed(_) = cowify {
            panic!("Expected to owned since unaligned");
        }
        assert_eq!(inputs, &*cowify);
        assert_eq!(
            inputs,
            jsony::from_binary::<Vec<Pod>>(&unaligned_encoded).unwrap()
        );
    }
}

#[test]
fn binary_version_range() {
    use jsony::{from_binary, to_binary};
    #[derive(Jsony)]
    #[jsony(Binary, version)]
    struct V0(u32);

    #[derive(Jsony, PartialEq, Debug)]
    #[jsony(Binary, version = 1..)]
    struct V1(i32); // breaking version change. release

    #[derive(Jsony, PartialEq, Debug)]
    #[jsony(Binary, version = 1..)]
    struct V2(i32, #[jsony(version = 2, default = true)] bool);

    #[derive(Jsony, PartialEq, Debug)]
    #[jsony(Binary, version = 2..=3)]
    struct V3(i32, #[jsony(version = 2)] bool);

    let error_message = from_binary::<V1>(&to_binary(&V0(342)))
        .err()
        .expect("Expect decoding unknown version should fail")
        .to_string();
    assert!(
        error_message.contains("version"),
        "Expected error to be about version instead it was: {}",
        error_message
    );

    assert_eq!(
        from_binary::<V2>(&to_binary(&V1(123))).unwrap(),
        V2(123, true)
    );

    assert_eq!(
        from_binary::<V3>(&to_binary(&V2(123, true))).unwrap(),
        V3(123, true)
    );

    let error_message = from_binary::<V2>(&to_binary(&V3(123, true)))
        .err()
        .expect("Expect decoding unknown version should fail")
        .to_string();
    assert!(
        error_message.contains("version"),
        "Expected error to be about version instead it was: {}",
        error_message
    );
}

#[test]
fn binary_version() {
    use jsony::{from_binary, to_binary};
    #[derive(Jsony)]
    #[jsony(Binary, version)]
    struct V0 {
        alpha: u32,
    }

    #[derive(Jsony, PartialEq, Debug)]
    #[jsony(Binary, version = 1)]
    struct V1 {
        alpha: u32,
        #[jsony(version = 1)]
        beta: u32,
    }
    #[derive(Jsony, PartialEq, Debug)]
    #[jsony(Binary, version)]
    struct V2<'a> {
        alpha: u32,
        #[jsony(version = 1)]
        beta: u32,
        #[jsony(version = 2, default = "DEFAULT")]
        canary: &'a str,
        #[jsony(version = 2, default = 33)]
        delta: u32,
    }

    #[derive(Jsony, PartialEq, Debug)]
    #[jsony(Binary, version)]
    struct V3<'a>(
        u32,
        #[jsony(version = 1)] u32,
        #[jsony(version = 2, default = "DEFAULT")] &'a str,
        #[jsony(version = 2, default = 33)] u32,
        #[jsony(version = 3, default = true)] bool,
    );

    #[derive(Jsony, PartialEq, Debug)]
    #[jsony(Binary, version = 4..)]
    struct V4(i32); // breaking version change. release

    #[derive(Jsony, PartialEq, Debug)]
    #[jsony(Binary, version = 4..)]
    struct V5(i32, #[jsony(version = 5)] bool);

    #[derive(Jsony, PartialEq, Debug)]
    #[jsony(Binary, version = 4..=6)]
    struct V6(i32, #[jsony(version = 5)] bool);

    assert_eq!(
        from_binary::<V1>(&to_binary(&V0 { alpha: 32 })).unwrap(),
        V1 { alpha: 32, beta: 0 }
    );

    let error_message = from_binary::<V0>(&to_binary(&V1 { alpha: 32, beta: 0 }))
        .err()
        .expect("Expect decoding unknown version should fail")
        .to_string();
    assert!(
        error_message.contains("version"),
        "Expected error to be about version instead it was: {}",
        error_message
    );

    assert_eq!(
        from_binary::<V2>(&to_binary(&V0 { alpha: 32 })).unwrap(),
        V2 {
            alpha: 32,
            beta: 0,
            canary: "DEFAULT",
            delta: 33
        }
    );
    assert_eq!(
        from_binary::<V2>(&to_binary(&V1 {
            alpha: 32,
            beta: 342
        }))
        .unwrap(),
        V2 {
            alpha: 32,
            beta: 342,
            canary: "DEFAULT",
            delta: 33
        }
    );

    assert_eq!(
        from_binary::<Vec<V1>>(&to_binary(&[V0 { alpha: 250 }, V0 { alpha: 22 }][..])).unwrap(),
        vec![
            V1 {
                alpha: 250,
                beta: 0
            },
            V1 { alpha: 22, beta: 0 }
        ]
    );

    assert_eq!(
        from_binary::<V3>(&to_binary(&V2 {
            alpha: 32,
            beta: 342,
            canary: "Value",
            delta: 89383,
        }))
        .unwrap(),
        V3(32, 342, "Value", 89383, true)
    );
}

#[test]
fn object_as_vec_of_tuple_helper() {
    #[derive(Jsony, PartialEq, Eq, Debug)]
    #[jsony(Json)]
    struct Data<'a> {
        #[jsony(with = jsony::helper::object_as_vec_of_tuple)]
        pairs: Vec<(&'a str, u32)>,
    }

    assert_decode_json_eq! {
        { "pairs": {"hello": 32, "nice": 59} },
        Data{pairs: vec![("hello", 32), ("nice", 59)]}
    }

    assert_decode_json_eq! {
        { "pairs": {} },
        Data{pairs: vec![]}
    }

    #[derive(Jsony, PartialEq, Eq, Debug)]
    #[jsony(Json)]
    struct DataNum {
        #[jsony(with = jsony::helper::object_as_vec_of_tuple)]
        pairs: Vec<(u32, u32)>,
    }
    assert_decode_json_eq! {
        { "pairs": {"34": 234, "112": 452} },
        DataNum{pairs: vec![(34,234), (112,452)]}
    }
}

#[test]
fn flattenable() {
    #[derive(Jsony, PartialEq, Eq, Debug)]
    #[jsony(Json, Flattenable)]
    struct Data<'a> {
        #[jsony(default = "centauri")]
        alpha: &'a str,
        #[jsony(alias = "beta")]
        bravo: u32,
    }

    assert_json_eq! {
        { "alpha": "value", "bravo": 22 },
        Data{alpha: "value", bravo: 22}
    }
    assert_decode_json_eq! {
        { "alpha": "value", "beta": 22 },
        Data{alpha: "value", bravo: 22}
    }
    assert_decode_json_eq! {
        { "beta": 22 },
        Data{alpha: "centauri", bravo: 22}
    }

    #[derive(Jsony, PartialEq, Eq, Debug)]
    #[jsony(Json, Flattenable)]
    struct Wrapper<'a> {
        #[jsony(flatten)]
        data: Data<'a>,
        other: i32,
    }

    assert_json_eq! {
        {"alpha": "value", "bravo": 22, "other": 42},
        Wrapper{ data: Data{alpha: "value", bravo: 22}, other: 42}
    }
    assert_decode_json_eq! {
        { "alpha": "value", "beta": 22, "other": 42 },
        Wrapper{ data: Data{alpha: "value", bravo: 22}, other: 42}
    }
    assert_decode_json_eq! {
        { "alpha": "value", "beta": 22, "ignored": true, "other": 42 },
        Wrapper{ data: Data{alpha: "value", bravo: 22}, other: 42}
    }
    assert_decode_json_eq! {
        { "beta": 22, "other": 42 },
        Wrapper{ data: Data{alpha: "centauri", bravo: 22}, other: 42}
    }

    assert_decode_failure! {
        { "other": 42 },
        Wrapper,
        MISSING_REQUIRED_FIELDS /* Missing required beta/bravo field */
    }
}

#[test]
fn field_validate() {
    use jsony::require;
    #[derive(Jsony, PartialEq, Eq, Debug)]
    #[jsony(Json, Binary)]
    struct Data<'a> {
        #[jsony(default = "centauri".into(), validate = require!(|a| a.len() > 4, "alpha must be atleast 5 bytes long"))]
        alpha: String,
        #[jsony(alias = "beta", validate = require!(..10, "bravo must be less than 10"))]
        bravo: u32,
        #[jsony(validate = require!("bar" | "baz", "canary must be bar or baz"))]
        canary: &'a str,
    }

    macro_rules! data_binary_validation_failure {
        ($($error_fragment: literal {$($tt:tt)*}),* $(,)?) => {
            $({
                let data = Data { $($tt)* };
                let encoded = jsony::to_binary(&data);
                let data = jsony::from_binary::<Data>(&encoded);
                assert!(data.err().unwrap().message().contains($error_fragment));
            })*
        };
    }
    data_binary_validation_failure!(
        "alpha must be" { alpha: "asd".into(), bravo: 4, canary: "bar" },
        "bravo must be less than 10" { alpha: "asdasdf".into(), bravo: 12, canary: "bar" },
        "canary must be bar or baz" { alpha: "asdasdf".into(), bravo: 5, canary: "foo" },
    );

    assert_binary_round_trip!(Data {
        alpha: "value".into(),
        bravo: 2,
        canary: "bar"
    });

    assert_json_eq! {
        { "alpha": "value", "bravo": 2, "canary": "bar" },
        Data{alpha: "value".into(), bravo: 2, canary: "bar"}
    }
    assert_decode_failure! {
        { "alpha": "value", "bravo": 12, "canary": "bar" },
        Data,
        CUSTOM_FIELD_VALIDATION_ERROR
    }
    assert_decode_failure! {
        { "alpha": "value", "beta": 12, "canary": "bar" },
        Data,
        CUSTOM_FIELD_VALIDATION_ERROR
    }
    assert_decode_failure! {
        { "alpha": "value", "bravo": 2, "canary": "foo" },
        Data,
        CUSTOM_FIELD_VALIDATION_ERROR
    }

    #[derive(Jsony, PartialEq, Eq, Debug)]
    #[jsony(Json)]
    enum DataEnum {
        Foo(#[jsony(validate = require!(4..6))] u32),
    }

    assert_json_eq! {
        { "Foo": 4 },
        DataEnum::Foo(4)
    }
    assert_decode_failure! {
        { "Foo": 3 },
        DataEnum,
        CUSTOM_FIELD_VALIDATION_ERROR
    }
}
