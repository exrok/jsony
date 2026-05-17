use std::ptr::NonNull;

use jsony::{json::DecodeError, parser::Parser, FromJson, JsonParserConfig};

static KEY_ERROR: DecodeError = DecodeError {
    message: "key decode failed",
};
static CALLBACK_ERROR: DecodeError = DecodeError {
    message: "callback failed",
};

#[allow(dead_code)]
struct NeverInitializesKey(Box<u8>);

unsafe impl<'a> FromJson<'a> for NeverInitializesKey {
    unsafe fn emplace_from_json(
        _dest: NonNull<()>,
        _parser: &mut Parser<'a>,
    ) -> Result<(), &'static DecodeError> {
        Err(&KEY_ERROR)
    }
}

#[test]
fn object_key_error_before_initialization_is_not_dropped() {
    let mut parser = Parser::new(r#"{"owned":"value"}"#, JsonParserConfig::default());

    let err = parser
        .decode_object_sequence::<NeverInitializesKey, String>(|_, _| Ok(()))
        .unwrap_err();

    assert_eq!(err, &KEY_ERROR);
}

#[test]
fn object_callback_error_after_owned_pair_is_not_dropped_again() {
    let mut parser = Parser::new(r#"{"owned":"value"}"#, JsonParserConfig::default());

    let err = parser
        .decode_object_sequence::<String, String>(|key, value| {
            assert_eq!(key, "owned");
            assert_eq!(value, "value");
            Err(&CALLBACK_ERROR)
        })
        .unwrap_err();

    assert_eq!(err, &CALLBACK_ERROR);
}

#[test]
fn object_step_error_after_owned_pair_is_not_dropped_again() {
    let mut parser = Parser::new(r#"{"owned":"value" @"#, JsonParserConfig::default());

    let err = parser
        .decode_object_sequence::<String, String>(|key, value| {
            assert_eq!(key, "owned");
            assert_eq!(value, "value");
            Ok(())
        })
        .unwrap_err();

    assert_eq!(err.message, "Expected list comma or end");
}

#[test]
fn object_sequence_escaped_owned_keys_keep_scratch_buffer_ownership() {
    let mut parser = Parser::new(
        r#"{"a\u0062":"first","c\u0064":"second"}"#,
        JsonParserConfig::default(),
    );
    let mut pairs = Vec::new();

    parser
        .decode_object_sequence::<String, String>(|key, value| {
            pairs.push((key, value));
            Ok(())
        })
        .unwrap();

    assert_eq!(
        pairs,
        vec![
            ("ab".to_owned(), "first".to_owned()),
            ("cd".to_owned(), "second".to_owned()),
        ]
    );
}
