use std::{
    borrow::Cow,
    collections::{BTreeMap, BTreeSet, HashMap, HashSet},
    ptr::NonNull,
    sync::atomic::{AtomicUsize, Ordering},
};

use jsony::{json::DecodeError, parser::Parser, FromJson, JsonParserConfig, Jsony};

#[test]
fn from_json_bytes_borrows_valid_utf8_and_reports_invalid_utf8() {
    let parsed = jsony::from_json_bytes::<&str>(br#""borrowed""#).unwrap();
    assert_eq!(parsed, "borrowed");

    let err = jsony::from_json_bytes::<String>(&[0xff]).unwrap_err();
    assert_eq!(err.decoding_error().message, "Invalid UTF-8");
    assert!(err.to_string().contains("invalid utf-8"));
}

static DROPPED_TRAILING_VALUE: AtomicUsize = AtomicUsize::new(0);

#[derive(Debug, Jsony)]
struct TrailingValue {
    value: String,
}

impl Drop for TrailingValue {
    fn drop(&mut self) {
        DROPPED_TRAILING_VALUE.fetch_add(1, Ordering::SeqCst);
    }
}

#[test]
fn trailing_data_error_drops_initialized_value_once() {
    DROPPED_TRAILING_VALUE.store(0, Ordering::SeqCst);

    let err = jsony::from_json_with_config::<TrailingValue>(
        r#"{"value":"kept alive only until trailing check"} trailing"#,
        JsonParserConfig::default(),
    )
    .unwrap_err();

    assert_eq!(err.decoding_error().message, "Trailing characters");
    assert_eq!(DROPPED_TRAILING_VALUE.load(Ordering::SeqCst), 1);

    DROPPED_TRAILING_VALUE.store(0, Ordering::SeqCst);
    let parsed = jsony::from_json_with_config::<TrailingValue>(
        r#"{"value":"ok"} trailing"#,
        JsonParserConfig {
            allow_trailing_data: true,
            ..Default::default()
        },
    )
    .unwrap();

    assert_eq!(parsed.value, "ok");
    assert_eq!(DROPPED_TRAILING_VALUE.load(Ordering::SeqCst), 0);
    drop(parsed);
    assert_eq!(DROPPED_TRAILING_VALUE.load(Ordering::SeqCst), 1);
}

#[derive(Debug, PartialEq, Eq)]
struct DecodeOnly(u8);

unsafe impl<'a> FromJson<'a> for DecodeOnly {
    fn decode_json(parser: &mut Parser<'a>) -> Result<Self, &'static DecodeError> {
        Ok(DecodeOnly(<u8 as FromJson>::decode_json(parser)?))
    }
}

#[test]
fn from_json_default_trait_methods_work_through_public_api() {
    assert_eq!(jsony::from_json::<DecodeOnly>("7").unwrap(), DecodeOnly(7));

    let boxed: Box<[u16]> = jsony::from_json("[1, 2, 3]").unwrap();
    assert_eq!(&*boxed, &[1, 2, 3]);
}

static LIVE_VEC_ITEMS: AtomicUsize = AtomicUsize::new(0);

#[derive(Debug)]
struct VecDropTracked;

unsafe impl<'a> FromJson<'a> for VecDropTracked {
    unsafe fn emplace_from_json(
        dest: NonNull<()>,
        parser: &mut Parser<'a>,
    ) -> Result<(), &'static DecodeError> {
        parser.skip_value()?;
        LIVE_VEC_ITEMS.fetch_add(1, Ordering::SeqCst);
        dest.cast::<VecDropTracked>().write(VecDropTracked);
        Ok(())
    }
}

impl Drop for VecDropTracked {
    fn drop(&mut self) {
        LIVE_VEC_ITEMS.fetch_sub(1, Ordering::SeqCst);
    }
}

#[test]
fn vec_json_error_drops_initialized_elements() {
    LIVE_VEC_ITEMS.store(0, Ordering::SeqCst);

    assert!(jsony::from_json::<Vec<VecDropTracked>>("[0, 1,").is_err());

    assert_eq!(LIVE_VEC_ITEMS.load(Ordering::SeqCst), 0);
}

#[test]
fn unquoted_keys_reject_digit_start_when_extension_is_enabled() {
    #[derive(Debug, PartialEq, Eq, Jsony)]
    struct Unquoted {
        _kind1: String,
        data: bool,
    }

    let config = JsonParserConfig {
        allow_unquoted_field_keys: true,
        ..Default::default()
    };

    assert_eq!(
        jsony::from_json_with_config::<Unquoted>(r#"{_kind1: "allowed", data: true}"#, config,)
            .unwrap(),
        Unquoted {
            _kind1: "allowed".to_owned(),
            data: true,
        }
    );

    let err = jsony::from_json_with_config::<Unquoted>(
        r#"{1_kind: "not an identifier", data: true}"#,
        config,
    )
    .unwrap_err();
    assert_eq!(err.decoding_error().message, "Invalid unquoted key");
}

#[test]
fn flattened_text_keys_can_borrow_or_own_as_needed() {
    #[derive(Debug, PartialEq, Jsony)]
    struct FlattenCow<'a> {
        #[jsony(flatten)]
        entries: Vec<(Cow<'a, str>, bool)>,
    }

    let parsed: FlattenCow<'_> =
        jsony::from_json(r#"{"plain":true,"esc\u0061ped":false}"#).unwrap();

    assert_eq!(parsed.entries.len(), 2);
    assert!(matches!(parsed.entries[0].0, Cow::Borrowed("plain")));
    assert!(matches!(&parsed.entries[1].0, Cow::Owned(value) if value == "escaped"));
    assert_eq!(parsed.entries[0].1, true);
    assert_eq!(parsed.entries[1].1, false);
}

#[test]
fn flattened_bool_keys_report_text_parse_errors() {
    #[derive(Debug, PartialEq, Eq, Jsony)]
    struct FlattenBool {
        #[jsony(flatten)]
        entries: Vec<(bool, u8)>,
    }

    assert_eq!(
        jsony::from_json::<FlattenBool>(r#"{"true":1,"0":2}"#)
            .unwrap()
            .entries,
        vec![(true, 1), (false, 2)]
    );

    let err = jsony::from_json::<FlattenBool>(r#"{"maybe":1}"#).unwrap_err();
    assert_eq!(err.decoding_error().message, "Invalid Boolean");
}

#[test]
fn maybe_json_lazy_errors_and_null_are_parseable() {
    let value = jsony::drill(r#"{"items":[{"name":"first"}]}"#);
    assert_eq!(value["items"][0]["name"].parse::<&str>().unwrap(), "first");

    let missing_key: &'static &'static str = &"missing";
    let missing = &value[missing_key];
    assert_eq!(missing.key_error(), Some("missing"));
    assert!(missing[0]
        .parse::<u8>()
        .unwrap_err()
        .to_string()
        .contains("Object key not found: \"missing\""));

    let dynamic_missing = &value["missing"];
    assert_eq!(dynamic_missing.key_error(), None);
    assert!(dynamic_missing
        .parse::<u8>()
        .unwrap_err()
        .to_string()
        .contains("Object Key Index Error"));

    assert!(value["items"][2]
        .parse::<u8>()
        .unwrap_err()
        .to_string()
        .contains("Array Index of 2"));

    assert_eq!(
        jsony::MaybeJson::null().parse::<Option<u8>>().unwrap(),
        None
    );
}

#[test]
fn json_error_display_preserves_field_context() {
    #[derive(Debug, Jsony)]
    struct Required {
        a: u8,
        b: bool,
    }

    let msg = jsony::from_json::<Required>("{}").unwrap_err().to_string();
    assert!(msg.contains("Missing required fields"), "{msg}");
    assert!(msg.contains("\"a\""), "{msg}");
    assert!(msg.contains("\"b\""), "{msg}");

    #[derive(Debug, Jsony)]
    struct HasFlag {
        flag: bool,
    }

    let msg = jsony::from_json::<HasFlag>(r#"{"flag":1}"#)
        .unwrap_err()
        .to_string();
    assert!(msg.contains("@ key \"flag\""), "{msg}");
}

#[test]
fn to_json_escapes_control_chars_and_non_finite_numbers() {
    let text = "\"\\\n\r\t\u{0008}\u{000c}\u{0001}";
    assert_eq!(jsony::to_json(text), r#""\"\\\n\r\t\b\f\u0001""#);
    assert_eq!(jsony::to_json(&f32::NAN), "null");
    assert_eq!(jsony::to_json(&f64::INFINITY), "null");
}

#[test]
fn binary_reports_invalid_utf8_and_invalid_char() {
    let err = jsony::from_binary::<String>(&[1, 0xff]).unwrap_err();
    assert!(err.message().contains("Invalid Utf-8"), "{err}");

    let err = jsony::from_binary::<char>(&0xd800u32.to_le_bytes()).unwrap_err();
    assert!(err.message().contains("Unicode scalar"), "{err}");
}

#[repr(align(2))]
struct AlignedToU16([u8; 5]);

#[test]
fn binary_cow_pod_slice_copies_unaligned_data() {
    let bytes = AlignedToU16([2, 0x34, 0x12, 0x78, 0x56]);

    let decoded = jsony::from_binary::<Cow<'_, [u16]>>(&bytes.0).unwrap();
    assert_eq!(&decoded[..], &[0x1234, 0x5678]);
    assert!(matches!(decoded, Cow::Owned(_)));

    let err = jsony::from_binary::<&[u16]>(&bytes.0).unwrap_err();
    assert!(err.message().contains("unaligned"), "{err}");
}

#[test]
fn binary_hash_collections_break_on_eof_lengths() {
    let mut impossible = vec![255];
    impossible.extend_from_slice(&u64::MAX.to_le_bytes());

    let err = jsony::from_binary::<HashSet<u8>>(&impossible).unwrap_err();
    assert_eq!(err.message(), "Unexpected EOF");

    let err = jsony::from_binary::<HashMap<u8, u16>>(&impossible).unwrap_err();
    assert_eq!(err.message(), "Unexpected EOF");

    let zero_sized = jsony::from_binary::<HashSet<[u8; 0]>>(&[1]).unwrap();
    assert_eq!(zero_sized.len(), 1);
}

#[test]
fn binary_btree_collections_roundtrip() {
    let map: BTreeMap<u32, String> =
        [(3, "c".to_owned()), (1, "a".to_owned()), (2, "b".to_owned())]
            .into_iter()
            .collect();
    let bytes = jsony::to_binary(&map);
    assert_eq!(jsony::from_binary::<BTreeMap<u32, String>>(&bytes).unwrap(), map);

    let set: BTreeSet<i64> = [5, -1, 3, -1].into_iter().collect();
    let bytes = jsony::to_binary(&set);
    assert_eq!(jsony::from_binary::<BTreeSet<i64>>(&bytes).unwrap(), set);

    // Length-prefixed entries match HashMap's shape: a truncated length errors.
    let mut impossible = vec![255];
    impossible.extend_from_slice(&u64::MAX.to_le_bytes());
    let err = jsony::from_binary::<BTreeSet<u8>>(&impossible).unwrap_err();
    assert_eq!(err.message(), "Unexpected EOF");
}
