//! In-source tests for the derive code generator.
//!
//! Because the source crate is built with `proc_macro2`, the codegen can be
//! exercised directly here (`cargo test -p jsony_macros_source`) without going
//! through the generated proc-macro crate. The realistic surface is the public
//! [`codegen::derive`] entry point: it never panics, returning a
//! `compile_error!{..}` token stream on failure, so [`assert_ok`] asserts that
//! generation succeeded.

use crate::codegen;
use proc_macro2::TokenStream;
use std::str::FromStr;

/// Run the derive code generator on `src` and return the generated tokens as a
/// string.
fn derive_str(src: &str) -> String {
    codegen::derive(TokenStream::from_str(src).unwrap()).to_string()
}

#[track_caller]
fn assert_ok(out: &str) {
    assert!(
        !out.contains("compile_error"),
        "derive produced a compile error: {out}"
    );
}

#[test]
fn struct_from_json_smoke() {
    let out = derive_str("#[derive(Jsony)] #[jsony(FromJson)] struct Point { x: i32, y: i32 }");
    assert_ok(&out);
    assert!(out.contains("FromJson"));
    assert!(out.contains("Point"));
}

#[test]
fn struct_to_json_smoke() {
    let out = derive_str("#[derive(Jsony)] #[jsony(ToJson)] struct Point { x: i32, y: i32 }");
    assert_ok(&out);
    assert!(out.contains("ToJson"));
}

#[test]
fn enum_struct_variant_flatten_and_plain() {
    // Exercises the merged `enum_variant_from_json_struct` over both the
    // flatten and non-flatten struct-variant paths under an inline tag.
    let out = derive_str(
        "#[derive(Jsony)] #[jsony(FromJson, tag = \"kind\")] \
         enum E<'a> { \
             A { x: i32 }, \
             B { msg: std::borrow::Cow<'a, str>, #[jsony(flatten)] rest: Vec<(String, bool)> } \
         }",
    );
    assert_ok(&out);
    assert!(out.contains("FromJson"));
    assert!(
        out.contains("new_field_visitor"),
        "flatten path should emit a field visitor: {out}"
    );
}

#[test]
fn enum_to_json_and_binary() {
    // Exercises the shared `emit_variant_pattern` across tuple/struct/unit
    // variants for both the to_json and to_binary emitters.
    let out = derive_str(
        "#[derive(Jsony)] #[jsony(ToJson, ToBinary, FromBinary)] \
         enum E { Unit, Tuple(i32), Struct { a: i32, b: bool } }",
    );
    assert_ok(&out);
    assert!(out.contains("ToJson"));
    assert!(out.contains("ToBinary"));
}
