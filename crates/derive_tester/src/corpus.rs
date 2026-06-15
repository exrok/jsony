//! In-process macro-source coverage for branches the round-trip generator
//! cannot reach.
//!
//! **This is not the correctness oracle.** Whether the derive's output compiles
//! and round-trips is validated by the *generator* (`gen.rs`) feeding the
//! `run`/`sound`/`asan` tiers, which compile each type and round-trip real
//! values under AddressSanitizer. That pipeline is where soundness bugs are
//! found (e.g. generic types, now monomorphized and round-tripped — see
//! `gen::apply_generics`). Driving inputs here only asserts that the macro
//! *tokenizes and expands* (or rejects with the right message). A passing case
//! here does not mean the generated code is correct.
//!
//! What this exists for: the round-trip generator emits a deliberately narrow,
//! always-well-formed syntax subset, so large parts of the parser, the
//! throw/diagnostic code, and the `object!`/`array!` entry point are
//! structurally unreachable from it. This corpus drives exactly those:
//! - [`errors`]: malformed items that must expand *to* a `compile_error!` with an
//!   expected message — the only cheap in-process check of the parse/throw code
//!   in `ast.rs`/`case.rs`/`codegen.rs` and all of `context.rs`. Malformed input
//!   never compiles, so the round-trip tiers cannot exercise it. (Found the
//!   const-generic, where-clause, and empty-enum issues in docs/known-issues.md.)
//! - [`templates`]: `object!`/`array!` bodies — a separate proc-macro entry point
//!   the round-trip pipeline never touches (`template.rs`/`lit.rs`).
//! - [`valid`]: well-formed but syntactically odd items (generic *parameter
//!   lists* with bounds/defaults, unusual attribute and `repr` forms, parser
//!   edge shapes). These drive `ast.rs` parser branches the generator's bare
//!   `struct G<T>` output does not. Generic *codegen* itself is validated for
//!   real by the generator, not here.

use std::fmt::Write as _;

/// Which macro-source entry point drives a case.
#[derive(Clone, Copy, PartialEq, Eq)]
pub enum Driver {
    /// `#[derive(Jsony)]` — `expand_derive_str`.
    Derive,
    /// `object! { .. }` — `expand_object_str` (src is the brace body).
    Object,
    /// `array! [ .. ]` — `expand_array_str` (src is the bracket body).
    Array,
}

/// One expand-only case.
pub struct Case {
    pub name: String,
    pub driver: Driver,
    pub src: String,
    /// `None`: the expansion must contain no `compile_error!`.
    /// `Some(substr)`: the expansion must be a `compile_error!` whose rendered
    /// message contains `substr`.
    pub expect_error: Option<String>,
}

fn ok(name: &str, src: &str) -> Case {
    Case {
        name: name.to_string(),
        driver: Driver::Derive,
        src: src.to_string(),
        expect_error: None,
    }
}

fn err(name: &str, msg: &str, src: &str) -> Case {
    Case {
        name: name.to_string(),
        driver: Driver::Derive,
        src: src.to_string(),
        expect_error: Some(msg.to_string()),
    }
}

fn obj_ok(name: &str, src: &str) -> Case {
    Case {
        name: name.to_string(),
        driver: Driver::Object,
        src: src.to_string(),
        expect_error: None,
    }
}

fn obj_err(name: &str, msg: &str, src: &str) -> Case {
    Case {
        name: name.to_string(),
        driver: Driver::Object,
        src: src.to_string(),
        expect_error: Some(msg.to_string()),
    }
}

fn arr_ok(name: &str, src: &str) -> Case {
    Case {
        name: name.to_string(),
        driver: Driver::Array,
        src: src.to_string(),
        expect_error: None,
    }
}

fn arr_err(name: &str, msg: &str, src: &str) -> Case {
    Case {
        name: name.to_string(),
        driver: Driver::Array,
        src: src.to_string(),
        expect_error: Some(msg.to_string()),
    }
}

/// The eight `rename_all` rules with an explicit spelling, paired with a label.
const RENAME_RULES: &[(&str, &str)] = &[
    ("lower", "lowercase"),
    ("upper", "UPPERCASE"),
    ("pascal", "PascalCase"),
    ("camel", "camelCase"),
    ("snake", "snake_case"),
    ("screaming", "SCREAMING_SNAKE_CASE"),
    ("kebab", "kebab-case"),
    ("screamingkebab", "SCREAMING-KEBAB-CASE"),
];

// =====================================================================
// Bucket 1 — valid expand-only shapes
// =====================================================================

/// Valid derive items the generator never emits. Each must expand cleanly.
pub fn valid() -> Vec<Case> {
    let mut out = Vec::new();

    // --- Generics ---
    out.push(ok(
        "gen_type_json",
        "#[derive(jsony::Jsony)] #[jsony(Json)] struct G<T> { a: T, b: u32 }",
    ));
    out.push(ok(
        "gen_type_binary",
        "#[derive(jsony::Jsony)] #[jsony(Binary)] struct GB<T> { a: T, b: u32 }",
    ));
    out.push(ok(
        "gen_field_containers",
        "#[derive(jsony::Jsony)] #[jsony(Json, Binary)] struct GF<T> { a: Vec<T>, b: Option<T>, c: u32 }",
    ));
    out.push(ok(
        "gen_bound",
        "#[derive(jsony::Jsony)] #[jsony(Json)] struct B<T: Clone> { a: T }",
    ));
    out.push(ok(
        "gen_bound_multi",
        "#[derive(jsony::Jsony)] #[jsony(Json, Binary)] struct BM<T: Clone + Default> { a: T, b: u32 }",
    ));
    out.push(ok(
        "gen_lifetime_type",
        "#[derive(jsony::Jsony)] #[jsony(Json)] struct L<'a, T> { a: &'a str, b: T }",
    ));
    out.push(ok(
        "gen_str_enum",
        "#[derive(jsony::Jsony)] #[jsony(ToStr, FromStr)] enum SE<T> { A, B, #[allow(dead_code)] Phantom }",
    ));
    out.push(ok(
        "gen_flattenable",
        "#[derive(jsony::Jsony)] #[jsony(Json, Flattenable)] struct GFl<T> { a: T, b: u32 }",
    ));
    out.push(ok(
        "gen_default_field",
        "#[derive(jsony::Jsony)] #[jsony(FromJson)] struct GD<T: Default> { #[jsony(default)] a: T, b: u32 }",
    ));

    // --- generic-list parsing edge forms ---
    out.push(ok(
        "gen_two_type",
        "#[derive(jsony::Jsony)] #[jsony(Json)] struct G2<T, U> { a: T, b: U }",
    ));
    out.push(ok(
        "gen_default_param",
        "#[derive(jsony::Jsony)] #[jsony(Json)] struct GDp<T = u32> { a: T }",
    ));
    out.push(ok(
        "gen_default_then_param",
        "#[derive(jsony::Jsony)] #[jsony(Json)] struct GDT<T = u32, U> { a: U }",
    ));
    out.push(ok(
        "gen_two_bounded",
        "#[derive(jsony::Jsony)] #[jsony(Json)] struct G2B<T: Clone, U: Default> { a: T, b: U }",
    ));
    out.push(ok(
        "gen_bound_default",
        "#[derive(jsony::Jsony)] #[jsony(Json)] struct GBD<T: Clone = u32> { a: T }",
    ));
    out.push(ok(
        "gen_bound_generic",
        "#[derive(jsony::Jsony)] #[jsony(Json)] struct GBG<T: Into<u8>> { a: T }",
    ));
    out.push(ok(
        "gen_leading_comma",
        "#[derive(jsony::Jsony)] #[jsony(Json)] struct GLC<, T> { a: T }",
    ));
    out.push(ok(
        "gen_empty_brackets",
        "#[derive(jsony::Jsony)] #[jsony(Json)] struct GE<> { a: u32 }",
    ));
    // Garbage tokens between `<..>` and the body are silently consumed as
    // "where clauses" (the real `where` keyword is instead rejected — see
    // docs/known-issues.md). Drives the where-clause extraction branch.
    out.push(ok(
        "where_clause_quirk",
        "#[derive(jsony::Jsony)] #[jsony(FromJson)] struct WQ<T> Foo { a: T }",
    ));
    // Same quirk but a tuple body, so the where-extraction reports a TupleStruct.
    out.push(ok(
        "where_clause_quirk_tuple",
        "#[derive(jsony::Jsony)] #[jsony(Json)] struct WQT<T> Foo (u32)",
    ));

    // --- leading/odd attributes and repr forms ---
    out.push(ok(
        "pub_struct",
        "#[derive(jsony::Jsony)] #[jsony(FromJson)] pub(crate) struct PB { a: u32 }",
    ));
    out.push(ok(
        "attr_bare_path",
        "#[derive(jsony::Jsony)] #[foo] #[jsony(FromJson)] struct S { a: u32 }",
    ));
    out.push(ok(
        "attr_literal_first",
        "#[derive(jsony::Jsony)] #[\"x\"] #[jsony(FromJson)] struct S { a: u32 }",
    ));
    out.push(ok(
        "repr_unknown",
        "#[derive(jsony::Jsony)] #[jsony(FromJson)] #[repr(align(8))] struct S { a: u32 }",
    ));
    out.push(ok(
        "field_literal_attr",
        "#[derive(jsony::Jsony)] #[jsony(FromJson)] struct S { #[\"x\"] a: u32 }",
    ));
    out.push(ok(
        "field_bare_jsony",
        "#[derive(jsony::Jsony)] #[jsony(FromJson)] struct S { #[jsony] a: u32 }",
    ));
    out.push(ok(
        "field_non_jsony_attr",
        "#[derive(jsony::Jsony)] #[jsony(FromJson)] struct S { #[doc = \"x\"] a: u32 }",
    ));
    out.push(ok(
        "struct_leading_comma",
        "#[derive(jsony::Jsony)] #[jsony(FromJson)] struct S { , a: u32 }",
    ));

    // --- Flattenable field shapes ---
    out.push(ok(
        "flattenable_leading_default",
        "#[derive(jsony::Jsony)] #[jsony(Json, Flattenable)] struct FLD { #[jsony(default)] a: u32, b: u32 }",
    ));
    out.push(ok(
        "flattenable_alias",
        "#[derive(jsony::Jsony)] #[jsony(Json, Flattenable)] struct FLA { #[jsony(alias = \"x\")] a: u32, b: u32 }",
    ));

    // --- enum-variant field attrs (the `!on_self` codegen branches) ---
    out.push(ok(
        "enum_var_skip_if",
        "#[derive(jsony::Jsony)] #[jsony(ToJson)] enum EVS { V { #[jsony(skip_if = |x| *x > 10)] a: u32, b: u32 } }",
    ));
    out.push(ok(
        "enum_var_via_iter",
        "#[derive(jsony::Jsony)] #[jsony(ToJson)] enum EVI { V { #[jsony(flatten, via = Iterator)] m: Vec<(String, u32)>, b: u32 } }",
    ));
    // 11-field struct variant forces a two-digit scratch ident (`var(10)`).
    out.push(ok(
        "enum_var_eleven_fields",
        "#[derive(jsony::Jsony)] #[jsony(Json)] enum EVF { V { f0: u32, f1: u32, f2: u32, f3: u32, f4: u32, f5: u32, f6: u32, f7: u32, f8: u32, f9: u32, f10: u32 } }",
    ));

    // --- single-field tuple POD (handle_pod returns false, falls through) ---
    out.push(ok(
        "pod_single_tuple",
        "#[derive(jsony::Jsony)] #[jsony(Binary, zerocopy)] #[repr(C)] struct PST(u32);",
    ));

    // --- const generics ---
    // `<const N: usize>` (valid Rust) is REJECTED: the condition at
    // ast.rs `extract_derive_target` is inverted (`if ident_eq(ident,"const")
    // { throw }`), so `const N` throws while a nonsensical two-ident param is
    // accepted *as* a const generic. The rejection is driven in `errors()`; the
    // `<A B>` quirk below drives the `GenericKind::Const` codegen branch
    // (`fmt_generics`), which is otherwise unreachable from valid syntax. See
    // docs/known-issues.md.
    out.push(ok(
        "const_generic_quirk",
        "#[derive(jsony::Jsony)] #[jsony(Json)] struct CGQ<A B> { a: u32 }",
    ));

    // --- Empty tuple struct ToJson (FromJson rejects it; ToJson is fine) ---
    out.push(ok(
        "empty_tuple_tojson",
        "#[derive(jsony::Jsony)] #[jsony(ToJson)] struct ET();",
    ));

    // --- enum `other` variant WITH a field (FromText path) ---
    out.push(ok(
        "other_tuple_field",
        "#[derive(jsony::Jsony)] #[jsony(FromJson)] enum 01 { A, #[jsony(other)] Other(String) }",
    ));
    out.push(ok(
        "other_struct_field",
        "#[derive(jsony::Jsony)] #[jsony(FromJson)] enum O2 { A, #[jsony(other)] Other { name: String } }",
    ));
    out.push(ok(
        "other_unit",
        "#[derive(jsony::Jsony)] #[jsony(FromJson)] enum O3 { A, #[jsony(other)] Other }",
    ));

    // --- enum struct-variant flatten ---
    out.push(ok(
        "variant_struct_flatten",
        "#[derive(jsony::Jsony)] #[jsony(FromJson)] enum VF { V { a: u32, #[jsony(flatten)] rest: std::collections::HashMap<String, u32> } }",
    ));

    // --- tuple-variant field validate ---
    out.push(ok(
        "tuple_variant_validate",
        "#[derive(jsony::Jsony)] #[jsony(FromJson)] enum TV { V(#[jsony(validate = check_it)] u32) }",
    ));

    // --- bare closures (no arg type annotation) in skip_if ---
    out.push(ok(
        "bare_closure_skip_if",
        "#[derive(jsony::Jsony)] #[jsony(ToJson)] struct SC { #[jsony(skip_if = |x| *x > 10)] a: u32 }",
    ));
    out.push(ok(
        "bare_closure_skip_if_writer",
        "#[derive(jsony::Jsony)] #[jsony(ToJson)] struct SCW { #[jsony(skip_if = |x, w| *x > 10)] a: u32 }",
    ));

    // --- custom default expr on a skipped field ---
    out.push(ok(
        "skip_with_default_expr",
        "#[derive(jsony::Jsony)] #[jsony(Json)] struct SD { a: u32, #[jsony(skip, default = my_default())] b: u32 }",
    ));

    // --- POD trait subsets ---
    out.push(ok(
        "pod_frombinary_only",
        "#[derive(jsony::Jsony)] #[jsony(FromBinary, zerocopy)] #[repr(C)] struct PF { a: u32, b: u32 }",
    ));
    out.push(ok(
        "pod_tobinary_only",
        "#[derive(jsony::Jsony)] #[jsony(ToBinary, zerocopy)] #[repr(C)] struct PT { a: u32, b: u32 }",
    ));
    out.push(ok(
        "pod_single_field",
        "#[derive(jsony::Jsony)] #[jsony(Binary, zerocopy)] #[repr(C)] struct PS { a: u32 }",
    ));
    out.push(ok(
        "pod_tuple",
        "#[derive(jsony::Jsony)] #[jsony(Binary, zerocopy)] #[repr(C)] struct PTup(u32, u32);",
    ));
    out.push(ok(
        "pod_transparent",
        "#[derive(jsony::Jsony)] #[jsony(Binary, transparent)] #[repr(transparent)] struct PTr(u32);",
    ));

    // --- single-trait selectors ---
    out.push(ok(
        "sel_fromstr",
        "#[derive(jsony::Jsony)] #[jsony(FromStr)] enum FS { A, B }",
    ));
    out.push(ok(
        "sel_tostr",
        "#[derive(jsony::Jsony)] #[jsony(ToStr)] enum TS { A, B }",
    ));
    out.push(ok(
        "sel_str",
        "#[derive(jsony::Jsony)] #[jsony(Str)] enum ST { A, B }",
    ));
    out.push(ok(
        "sel_tobinary",
        "#[derive(jsony::Jsony)] #[jsony(ToBinary)] struct STB { a: u32 }",
    ));
    out.push(ok(
        "sel_frombinary",
        "#[derive(jsony::Jsony)] #[jsony(FromBinary)] struct SFB { a: u32 }",
    ));
    out.push(ok(
        "sel_fromjson",
        "#[derive(jsony::Jsony)] #[jsony(FromJson)] struct SFJ { a: u32 }",
    ));
    out.push(ok(
        "sel_default_fromjson",
        "#[derive(jsony::Jsony)] struct SDFJ { a: u32 }",
    ));

    // --- field trait-scoped attrs ---
    out.push(ok(
        "scoped_tojson_rename",
        "#[derive(jsony::Jsony)] #[jsony(Json)] struct SR { #[jsony(ToJson rename = \"x\")] a: u32, b: u32 }",
    ));
    out.push(ok(
        "scoped_fromjson_alias",
        "#[derive(jsony::Jsony)] #[jsony(Json)] struct SA { #[jsony(FromJson alias = \"y\")] a: u32, b: u32 }",
    ));
    out.push(ok(
        "scoped_to_rename",
        "#[derive(jsony::Jsony)] #[jsony(Json, Binary)] struct STo { #[jsony(To rename = \"x\")] a: u32, b: u32 }",
    ));
    out.push(ok(
        "scoped_from_alias",
        "#[derive(jsony::Jsony)] #[jsony(Json, Binary)] struct SFr { #[jsony(From alias = \"y\")] a: u32, b: u32 }",
    ));
    out.push(ok(
        "field_alias_plain",
        "#[derive(jsony::Jsony)] #[jsony(FromJson)] struct SAP { #[jsony(alias = \"alt\")] field: u32, b: u32 }",
    ));
    // A `ToJson`-scoped alias: `alias(FROM_JSON)` then returns None (the
    // not-found fallback), since the attr is enabled only for ToJson.
    out.push(ok(
        "scoped_tojson_alias",
        "#[derive(jsony::Jsony)] #[jsony(Json)] struct STA { #[jsony(ToJson alias = \"x\")] a: u32, b: u32 }",
    ));

    // --- enum discriminants ---
    out.push(ok(
        "enum_discriminants",
        "#[derive(jsony::Jsony)] #[jsony(FromJson)] enum ED { A = 1, B = 7 }",
    ));
    // Discriminant with a turbofish path drives the `::`/`<..>` depth scanner,
    // including a nested `<` (depth increment) and a `,` inside the generics.
    out.push(ok(
        "enum_discriminant_path",
        "#[derive(jsony::Jsony)] #[jsony(FromJson)] enum EDP { A = X::<Vec<u8>, u16>::Y, B = 2 }",
    ));

    // --- tuple-field visibility ---
    out.push(ok(
        "tuple_vis",
        "#[derive(jsony::Jsony)] #[jsony(Json)] struct TVi(pub u32, pub(crate) String);",
    ));

    // --- container version forms ---
    out.push(ok(
        "version_range",
        "#[derive(jsony::Jsony)] #[jsony(Binary, version = 1..=3)] struct VR { a: u32, #[jsony(version = 2)] b: u32 }",
    ));
    out.push(ok(
        "version_min_open",
        "#[derive(jsony::Jsony)] #[jsony(Binary, version = 2..)] struct VMO { a: u32 }",
    ));
    out.push(ok(
        "version_bare",
        "#[derive(jsony::Jsony)] #[jsony(Binary, version)] struct VB { a: u32, #[jsony(version = 1, default = 0)] b: u32 }",
    ));
    out.push(ok(
        "version_fixed",
        "#[derive(jsony::Jsony)] #[jsony(Binary, version = 3)] struct VFx { a: u32 }",
    ));

    // --- tag/content enum shapes ---
    out.push(ok(
        "internal_tag",
        "#[derive(jsony::Jsony)] #[jsony(Json, tag = \"kind\")] enum IT { A { x: u32 }, B { y: u32 } }",
    ));
    out.push(ok(
        "adjacent_tag_content",
        "#[derive(jsony::Jsony)] #[jsony(Json, tag = \"t\", content = \"c\")] enum AT { A(u32), B { y: u32 } }",
    ));
    out.push(ok(
        "untagged_enum",
        "#[derive(jsony::Jsony)] #[jsony(Json, untagged)] enum UT { A(u32), B { y: u32 }, C }",
    ));
    out.push(ok(
        "ignore_adjacent",
        "#[derive(jsony::Jsony)] #[jsony(FromJson, ignore_tag_adjacent_fields)] enum IA { A { x: u32 }, B }",
    ));
    out.push(ok(
        "internal_tag_unit",
        "#[derive(jsony::Jsony)] #[jsony(Json, tag = \"kind\")] enum ITU { A, B { y: u32 } }",
    ));

    // --- multi-word casing on variants and fields, per rule ---
    for (i, (label, rule)) in RENAME_RULES.iter().enumerate() {
        let var = format!(
            "#[derive(jsony::Jsony)] #[jsony(Json)] #[jsony(rename_all = \"{rule}\")] \
             enum CaseVar{i} {{ VeryTastyThing, AnotherGoodOne, A }}"
        );
        out.push(ok(&format!("case_variant_{label}"), &var));

        let field = format!(
            "#[derive(jsony::Jsony)] #[jsony(Json)] #[jsony(rename_all = \"{rule}\")] \
             struct CaseField{i} {{ very_tasty_field: u32, another_good_one: u32 }}"
        );
        out.push(ok(&format!("case_field_{label}"), &field));
    }

    // --- rename_all_fields + per-variant rename_all (multi-word) ---
    out.push(ok(
        "rename_all_fields",
        "#[derive(jsony::Jsony)] #[jsony(Json, rename_all_fields = \"snake_case\")] \
         enum RAF { V { VeryTastyField: u32, AnotherField: u32 } }",
    ));
    out.push(ok(
        "per_variant_rename_all",
        "#[derive(jsony::Jsony)] #[jsony(Json)] \
         enum PVR { #[jsony(rename_all = \"kebab-case\")] V { VeryTastyField: u32 }, B }",
    ));
    out.push(ok(
        "per_variant_rename",
        "#[derive(jsony::Jsony)] #[jsony(Json)] enum PVN { #[jsony(rename = \"renamed\")] LongVariantName, B }",
    ));

    // --- mixed strings-and-objects external enum ---
    out.push(ok(
        "mixed_unit_and_struct",
        "#[derive(jsony::Jsony)] #[jsony(Json)] enum MX { Unit, Data { x: u32 }, Tup(u32) }",
    ));

    out
}

// =====================================================================
// Bucket 2 — error-path shapes (malformed input)
// =====================================================================

/// Malformed derive items. Each must expand to a `compile_error!` whose message
/// contains the given substring. Drives the parse/throw code paths and, through
/// the panic/catch machinery, all of `context.rs`.
pub fn errors() -> Vec<Case> {
    let mut out = Vec::new();

    // --- container attribute parsing (ast.rs) ---
    out.push(err(
        "tag_and_untagged",
        "Cannot have a tag",
        "#[derive(jsony::Jsony)] #[jsony(untagged, tag = \"t\")] enum E { A }",
    ));
    out.push(err(
        "dup_untagged",
        "Duplicate tag",
        "#[derive(jsony::Jsony)] #[jsony(untagged, untagged)] enum E { A }",
    ));
    out.push(err(
        "dup_tag",
        "Duplicate tag",
        "#[derive(jsony::Jsony)] #[jsony(tag = \"a\", tag = \"b\")] enum E { A }",
    ));
    out.push(err(
        "tag_value_not_literal",
        "Expected a string literal",
        "#[derive(jsony::Jsony)] #[jsony(tag = true)] enum E { A }",
    ));
    out.push(err(
        "tag_bad_literal",
        "Expected a string literal",
        "#[derive(jsony::Jsony)] #[jsony(tag = 5.5)] enum E { A }",
    ));
    out.push(err(
        "tag_empty",
        "Expected a tag",
        "#[derive(jsony::Jsony)] #[jsony(tag =)] enum E { A }",
    ));
    out.push(err(
        "dup_content",
        "Duplicate content",
        "#[derive(jsony::Jsony)] #[jsony(tag = \"t\", content = \"c\", content = \"d\")] enum E { A }",
    ));
    out.push(err(
        "content_value_not_literal",
        "Expected the field of content",
        "#[derive(jsony::Jsony)] #[jsony(tag = \"t\", content = true)] enum E { A }",
    ));
    out.push(err(
        "content_bad_literal",
        "Expected a string literal",
        "#[derive(jsony::Jsony)] #[jsony(tag = \"t\", content = 5.5)] enum E { A }",
    ));
    out.push(err(
        "dup_version",
        "Duplicate version",
        "#[derive(jsony::Jsony)] #[jsony(Binary, version = 1, version = 2)] struct S { a: u32 }",
    ));
    out.push(err(
        "version_bad_shape",
        "Expected one of",
        "#[derive(jsony::Jsony)] #[jsony(Binary, version = bad)] struct S { a: u32 }",
    ));
    out.push(err(
        "version_min_too_big",
        "version number between",
        "#[derive(jsony::Jsony)] #[jsony(Binary, version = 99999999..)] struct S { a: u32 }",
    ));
    out.push(err(
        "version_cur_too_big",
        "version number between",
        "#[derive(jsony::Jsony)] #[jsony(Binary, version = 99999999)] struct S { a: u32 }",
    ));
    out.push(err(
        "dup_rename_all",
        "Duplicate rename_all",
        "#[derive(jsony::Jsony)] #[jsony(rename_all = \"snake_case\", rename_all = \"kebab-case\")] enum E { A }",
    ));
    out.push(err(
        "rename_all_not_literal",
        "Expected a literal",
        "#[derive(jsony::Jsony)] #[jsony(rename_all = bad)] enum E { A }",
    ));
    out.push(err(
        "rename_all_unknown",
        "unknown rename rule",
        "#[derive(jsony::Jsony)] #[jsony(rename_all = \"wat\")] enum E { A }",
    ));
    out.push(err(
        "dup_rename_all_fields",
        "Duplicate rename_all_fields",
        "#[derive(jsony::Jsony)] #[jsony(rename_all_fields = \"snake_case\", rename_all_fields = \"kebab-case\")] enum E { A }",
    ));
    out.push(err(
        "rename_all_fields_not_literal",
        "Expected a literal",
        "#[derive(jsony::Jsony)] #[jsony(rename_all_fields = bad)] enum E { A }",
    ));
    out.push(err(
        "unknown_container_attr",
        "Unknown attribute",
        "#[derive(jsony::Jsony)] #[jsony(bogus)] struct S { a: u32 }",
    ));
    out.push(err(
        "container_extra_value",
        "Extra value tokens",
        "#[derive(jsony::Jsony)] #[jsony(transparent = 5)] struct S { a: u32 }",
    ));
    out.push(err(
        "repr_packed",
        "repr(packed) not supported",
        "#[derive(jsony::Jsony)] #[jsony(Binary)] #[repr(packed)] struct S { a: u32 }",
    ));

    // --- field attribute parsing (ast.rs) ---
    out.push(err(
        "field_rename_not_literal",
        "Expected a literal",
        "#[derive(jsony::Jsony)] #[jsony(FromJson)] struct S { #[jsony(rename = bad)] a: u32 }",
    ));
    out.push(err(
        "field_rename_extra",
        "Unexpected a single literal",
        "#[derive(jsony::Jsony)] #[jsony(FromJson)] struct S { #[jsony(rename = \"x\" \"y\")] a: u32 }",
    ));
    out.push(err(
        "via_unknown",
        "Unknown via value",
        "#[derive(jsony::Jsony)] #[jsony(ToJson)] struct S { #[jsony(via = Nonsense)] a: Vec<u32> }",
    ));
    out.push(err(
        "via_not_ident",
        "Expected a value",
        "#[derive(jsony::Jsony)] #[jsony(ToJson)] struct S { #[jsony(via = \"x\")] a: u32 }",
    ));
    out.push(err(
        "with_then_validate",
        "validate",
        "#[derive(jsony::Jsony)] #[jsony(FromJson)] struct S { #[jsony(with = foo, validate = bar)] a: u32 }",
    ));
    out.push(err(
        "validate_then_with",
        "validate",
        "#[derive(jsony::Jsony)] #[jsony(FromJson)] struct S { #[jsony(validate = bar, with = foo)] a: u32 }",
    ));
    out.push(err(
        "flatten_with_args",
        "flatten doesn't take any arguments",
        "#[derive(jsony::Jsony)] #[jsony(FromJson)] struct S { #[jsony(flatten = x)] a: std::collections::HashMap<String, u32> }",
    ));
    out.push(err(
        "skip_with_args",
        "doesn't take any arguments",
        "#[derive(jsony::Jsony)] #[jsony(Json)] struct S { #[jsony(skip = x)] a: u32 }",
    ));
    out.push(err(
        "skip_if_empty",
        "skip criteria",
        "#[derive(jsony::Jsony)] #[jsony(ToJson)] struct S { #[jsony(skip_if)] a: u32 }",
    ));
    out.push(err(
        "other_with_args",
        "other doesn't take any arguments",
        "#[derive(jsony::Jsony)] #[jsony(FromJson)] enum E { A, #[jsony(other = x)] B }",
    ));
    out.push(err(
        "alias_not_literal",
        "Expected a literal",
        "#[derive(jsony::Jsony)] #[jsony(FromJson)] struct S { #[jsony(alias = bad)] a: u32 }",
    ));
    out.push(err(
        "field_version_not_number",
        "Expected a version number",
        "#[derive(jsony::Jsony)] #[jsony(Binary, version = 1)] struct S { a: u32, #[jsony(version = bad)] b: u32 }",
    ));
    out.push(err(
        "dup_field_attr",
        "Duplicate attribute",
        "#[derive(jsony::Jsony)] #[jsony(FromJson)] struct S { #[jsony(rename = \"x\", rename = \"y\")] a: u32 }",
    ));
    out.push(err(
        "unknown_field_attr",
        "Unknown attr field",
        "#[derive(jsony::Jsony)] #[jsony(FromJson)] struct S { #[jsony(nonsense)] a: u32 }",
    ));
    out.push(err(
        "attr_expected_ident",
        "Expected ident",
        "#[derive(jsony::Jsony)] #[jsony(FromJson)] struct S { #[jsony(\"x\")] a: u32 }",
    ));
    out.push(err(
        "attr_bad_separator",
        "Expected either",
        "#[derive(jsony::Jsony)] #[jsony(FromJson)] struct S { #[jsony(rename \"x\")] a: u32 }",
    ));
    out.push(err(
        "unknown_trait_alias",
        "Expected trait or alias",
        "#[derive(jsony::Jsony)] #[jsony(FromJson)] struct S { #[jsony(Nonsense rename = \"x\")] a: u32 }",
    ));

    // --- field-version validity (ast.rs scan_fields) ---
    out.push(err(
        "field_version_without_container",
        "field versions require",
        "#[derive(jsony::Jsony)] #[jsony(Binary)] struct S { a: u32, #[jsony(version = 1)] b: u32 }",
    ));
    out.push(err(
        "field_version_exceeds_container",
        "greater than container version",
        "#[derive(jsony::Jsony)] #[jsony(Binary, version = 1)] struct S { a: u32, #[jsony(version = 2)] b: u32 }",
    ));

    // --- structural parse (ast.rs extract_derive_target) ---
    // Valid const-generic syntax is rejected (inverted condition); drives the
    // `unexpected ident` throw. See docs/known-issues.md.
    out.push(err(
        "const_generic_rejected",
        "unexpected ident",
        "#[derive(jsony::Jsony)] #[jsony(Json)] struct CG<const N: usize> { a: u32 }",
    ));
    out.push(err(
        "empty_body",
        "Empty body",
        "#[derive(jsony::Jsony)] #[jsony(FromJson)] struct S",
    ));
    out.push(err(
        "bounded_lifetime",
        "Bounded lifetimes",
        "#[derive(jsony::Jsony)] #[jsony(FromJson)] struct S<'a: 'b> { a: u32 }",
    ));

    // --- codegen-level rejections (codegen.rs) ---
    out.push(err(
        "fromjson_empty_tuple",
        "Tuples without fields",
        "#[derive(jsony::Jsony)] #[jsony(FromJson)] struct E();",
    ));
    out.push(err(
        "two_flatten_struct",
        "Only one flatten",
        "#[derive(jsony::Jsony)] #[jsony(FromJson)] struct S { #[jsony(flatten)] a: std::collections::HashMap<String, u32>, #[jsony(flatten)] b: std::collections::HashMap<String, u32> }",
    ));
    out.push(err(
        "two_flatten_variant",
        "Only one flatten",
        "#[derive(jsony::Jsony)] #[jsony(FromJson)] enum E { V { #[jsony(flatten)] a: std::collections::HashMap<String, u32>, #[jsony(flatten)] b: std::collections::HashMap<String, u32> } }",
    ));
    out.push(err(
        "tuple_variant_multi_fromjson",
        "single field enum tuples",
        "#[derive(jsony::Jsony)] #[jsony(FromJson)] enum E { V(u8, u8) }",
    ));
    out.push(err(
        "tuple_variant_multi_tojson",
        "single field enum tuples",
        "#[derive(jsony::Jsony)] #[jsony(ToJson)] enum E { V(u8, u8) }",
    ));
    out.push(err(
        "two_other_variants",
        "Only one other variant",
        "#[derive(jsony::Jsony)] #[jsony(FromJson)] enum E { #[jsony(other)] A, #[jsony(other)] B }",
    ));
    out.push(err(
        "other_too_many_fields",
        "upto a single field",
        "#[derive(jsony::Jsony)] #[jsony(FromJson)] enum E { A, #[jsony(other)] B { x: u32, y: u32 } }",
    ));
    out.push(err(
        "tostr_data_variant",
        "must not have any tuple or struct",
        "#[derive(jsony::Jsony)] #[jsony(ToStr, FromStr)] enum E { A(u32), B }",
    ));
    out.push(err(
        "via_without_flatten",
        "only supported with flatten",
        "#[derive(jsony::Jsony)] #[jsony(ToJson)] struct S { #[jsony(via = Iterator)] pairs: Vec<(String, u32)> }",
    ));
    out.push(err(
        "pod_generics",
        "Pod derive doesn't support generics",
        "#[derive(jsony::Jsony)] #[jsony(Binary, zerocopy)] #[repr(C)] struct P<T> { a: T }",
    ));
    out.push(err(
        "pod_no_repr",
        "repr(transparent) or repr(C)",
        "#[derive(jsony::Jsony)] #[jsony(Binary, zerocopy)] struct P { a: u32, b: u32 }",
    ));
    out.push(err(
        "transparent_multi_fromjson",
        "single field",
        "#[derive(jsony::Jsony)] #[jsony(FromJson, transparent)] #[repr(transparent)] struct M { a: u8, b: u8 }",
    ));
    out.push(err(
        "transparent_multi_tojson",
        "single field",
        "#[derive(jsony::Jsony)] #[jsony(ToJson, transparent)] struct M { a: u8, b: u8 }",
    ));
    out.push(err(
        "transparent_no_repr",
        "requires #[repr(transparent)]",
        "#[derive(jsony::Jsony)] #[jsony(FromJson, transparent)] struct M { a: u8 }",
    ));
    out.push(err(
        "flatten_enum",
        "Flattening enums",
        "#[derive(jsony::Jsony)] #[jsony(FromJson, Flattenable)] enum E { A { x: u32 } }",
    ));
    out.push(err(
        "field_rename_numeric",
        "Invalid rename value",
        "#[derive(jsony::Jsony)] #[jsony(ToJson)] struct S { #[jsony(rename = 5)] a: u32 }",
    ));
    out.push(err(
        "variant_rename_numeric",
        "Invalid rename value",
        "#[derive(jsony::Jsony)] #[jsony(ToJson)] enum E { #[jsony(rename = 5)] A }",
    ));
    out.push(err(
        "flattenable_skip",
        "Flattenable does not yet support skipped",
        "#[derive(jsony::Jsony)] #[jsony(Json, Flattenable)] struct S { #[jsony(skip, default)] a: u32, b: u32 }",
    ));
    out.push(err(
        "tostr_only_data_variant",
        "must not have any tuple or struct",
        "#[derive(jsony::Jsony)] #[jsony(ToStr)] enum E { A(u32), B }",
    ));

    // --- structural-parse throws (ast.rs extract_derive_target) ---
    out.push(err(
        "struct_name_not_ident",
        "Expected a Ident",
        "#[derive(jsony::Jsony)] #[jsony(FromJson)] struct (u32);",
    ));
    out.push(err(
        "unhandled_after_name",
        "Unhandled feature",
        "#[derive(jsony::Jsony)] #[jsony(FromJson)] struct S = u8;",
    ));
    out.push(err(
        "struct_name_punct",
        "Expected a Ident",
        "#[derive(jsony::Jsony)] #[jsony(FromJson)] struct + ;",
    ));
    out.push(err(
        "struct_name_literal",
        "Expected a Ident",
        "#[derive(jsony::Jsony)] #[jsony(FromJson)] struct 5 ;",
    ));
    out.push(err(
        "where_no_body",
        "Expected body after where clauses",
        "#[derive(jsony::Jsony)] #[jsony(FromJson)] struct S<T> Foo ;",
    ));
    out.push(err(
        "after_generics_punct",
        "Expected either body or where clause",
        "#[derive(jsony::Jsony)] #[jsony(FromJson)] struct S<T> + { a: T }",
    ));
    // An empty enum throws the placeholder "Baddness" (ast.rs:1246) during
    // variant parsing — a poor diagnostic. See docs/known-issues.md.
    out.push(err(
        "empty_enum",
        "Baddness",
        "#[derive(jsony::Jsony)] #[jsony(FromJson)] enum E {}",
    ));

    // --- generic-list throws ---
    out.push(err(
        "generic_followed_by_group",
        "Unexpected group",
        "#[derive(jsony::Jsony)] #[jsony(FromJson)] struct S<T(u8)> { a: u32 }",
    ));
    out.push(err(
        "generic_unexpected_punct",
        "Unexpected token after first ident",
        "#[derive(jsony::Jsony)] #[jsony(FromJson)] struct S<T @> { a: u32 }",
    ));
    out.push(err(
        "generic_unexpected_token",
        "Unexpected token after first ident",
        "#[derive(jsony::Jsony)] #[jsony(FromJson)] struct S<T 5> { a: u32 }",
    ));
    // Note: the `'` not-followed-by-ident throw (ast.rs:667) is unreachable
    // from tokenizable input — a lone `'` is an unterminated lifetime/char and
    // fails to tokenize. Recorded as Bucket 4 in docs/known-issues.md.
    out.push(err(
        "generic_bad_punct",
        "Unexpected Punct",
        "#[derive(jsony::Jsony)] #[jsony(FromJson)] struct S<+> { a: u32 }",
    ));
    out.push(err(
        "generic_group_param",
        "Unhanlded",
        "#[derive(jsony::Jsony)] #[jsony(FromJson)] struct S<(x)> { a: u32 }",
    ));
    out.push(err(
        "generic_literal_param",
        "Unhanlded",
        "#[derive(jsony::Jsony)] #[jsony(FromJson)] struct S<5> { a: u32 }",
    ));
    out.push(err(
        "lifetime_unexpected_char",
        "unexpected char",
        "#[derive(jsony::Jsony)] #[jsony(FromJson)] struct S<'a +> { a: u32 }",
    ));
    out.push(err(
        "lifetime_unexpected_tok",
        "Unexpected tok",
        "#[derive(jsony::Jsony)] #[jsony(FromJson)] struct S<'a T> { a: u32 }",
    ));
    out.push(err(
        "where_clause_rejected",
        "Expected where clause",
        "#[derive(jsony::Jsony)] #[jsony(FromJson)] struct S<T> where T: Clone { a: T }",
    ));

    // --- field-attr remainder/value throws ---
    out.push(err(
        "via_extra_tokens",
        "Unexpected a single literal",
        "#[derive(jsony::Jsony)] #[jsony(ToJson)] struct S { #[jsony(via = extra Iterator)] a: Vec<u32> }",
    ));
    out.push(err(
        "alias_extra_tokens",
        "Unexpected a single literal",
        "#[derive(jsony::Jsony)] #[jsony(FromJson)] struct S { #[jsony(alias = \"x\" \"y\")] a: u32 }",
    ));
    out.push(err(
        "field_version_extra_tokens",
        "Unexpected a single number",
        "#[derive(jsony::Jsony)] #[jsony(Binary, version = 1)] struct S { a: u32, #[jsony(version = 1 2)] b: u32 }",
    ));

    // --- scan_fields version throws ---
    out.push(err(
        "field_version_too_big",
        "version number between",
        "#[derive(jsony::Jsony)] #[jsony(Binary, version = 1)] struct S { a: u32, #[jsony(version = 99999999)] b: u32 }",
    ));
    out.push(err(
        "version_min_exceeds_cur",
        "greater than container version",
        "#[derive(jsony::Jsony)] #[jsony(Binary, version = 5..=2)] struct S { a: u32 }",
    ));

    // --- enum-body parse throws ---
    out.push(err(
        "enum_hash_no_group",
        "Expected attr after",
        "#[derive(jsony::Jsony)] #[jsony(FromJson)] enum E { A, # }",
    ));
    out.push(err(
        "variant_rename_all_not_literal",
        "Expected a literal",
        "#[derive(jsony::Jsony)] #[jsony(FromJson)] enum E { #[jsony(rename_all = bad)] V { x: u32 } }",
    ));
    out.push(err(
        "discriminant_unclosed_generic",
        "Unexpected EOF while parsing type",
        "#[derive(jsony::Jsony)] #[jsony(FromJson)] enum E { A = X::<u8 }",
    ));
    out.push(err(
        "enum_unnamed_group",
        "Expected ident",
        "#[derive(jsony::Jsony)] #[jsony(FromJson)] enum E { (u32) }",
    ));
    out.push(err(
        "enum_literal_variant",
        "Expected either an ident or group",
        "#[derive(jsony::Jsony)] #[jsony(FromJson)] enum E { 5 }",
    ));
    out.push(err(
        "tuple_hash_no_group",
        "Expected attr after",
        "#[derive(jsony::Jsony)] #[jsony(Json)] struct T(u32, #);",
    ));
    out.push(err(
        "struct_hash_no_group",
        "Expected attr after",
        "#[derive(jsony::Jsony)] #[jsony(FromJson)] struct S { a: u32, # }",
    ));

    out
}

// =====================================================================
// Bucket 3 — template macro corpus (`object!` / `array!`)
// =====================================================================

/// `object!`/`array!` bodies exercising the template grammar and the literal
/// parser (`lit.rs`). Valid bodies must expand without a `compile_error!`;
/// malformed ones must expand to one.
pub fn templates() -> Vec<Case> {
    let mut out = Vec::new();

    // --- object: members, keys, nesting ---
    out.push(obj_ok("obj_empty", ""));
    out.push(obj_ok("obj_string_key", "\"a\": 1, \"b\": 2"));
    out.push(obj_ok("obj_ident_key", "a: 1, b: 2"));
    out.push(obj_ok("obj_ident_shorthand", "value"));
    out.push(obj_ok("obj_dyn_key", "[key_expr]: 1"));
    out.push(obj_ok("obj_nested", "outer: { inner: 1, deep: { x: 2 } }"));
    out.push(obj_ok("obj_array_value", "list: [1, 2, 3]"));
    out.push(obj_ok("obj_bool_null", "a: true, b: false, c: None"));
    out.push(obj_ok("obj_string_value", "a: \"hello\", b: \"world\""));
    out.push(obj_ok("obj_raw_number", "a: 42, b: 100"));
    out.push(obj_ok("obj_expr_value", "a: (1 + 2), b: some_var"));
    out.push(obj_ok("obj_conditional", "@[if cond] a: 1, b: 2"));
    out.push(obj_ok("obj_spread", "..base, a: 1"));
    out.push(obj_ok("obj_flatten_obj", "..other_obj"));
    out.push(obj_ok("obj_for", "for (k, v) in pairs; [k]: v"));
    out.push(obj_ok("obj_in_writer", "in writer; a: 1"));
    out.push(obj_ok(
        "obj_closure_value",
        "key: |b| { b.push_str(\"x\"); }",
    ));
    out.push(obj_ok("obj_match_value", "key: match x { _ => 1 }"));

    // --- array: elements, nesting, flatten, comprehensions ---
    out.push(arr_ok("arr_empty", ""));
    out.push(arr_ok("arr_numbers", "1, 2, 3"));
    out.push(arr_ok("arr_strings", "\"a\", \"b\", \"c\""));
    out.push(arr_ok("arr_mixed", "1, \"two\", true, None"));
    out.push(arr_ok("arr_nested", "[1, 2], [3, 4]"));
    out.push(arr_ok("arr_objects", "{ a: 1 }, { b: 2 }"));
    out.push(arr_ok("arr_expr", "(1 + 2), some_var"));
    out.push(arr_ok("arr_for", "for x in items; x"));
    out.push(arr_ok("arr_flatten", "..first, ..second"));
    out.push(arr_ok("arr_single", "only_one"));

    // --- literal grammar (lit.rs) via array elements ---
    out.push(arr_ok(
        "lit_escapes",
        r#""tab\tnewline\nquote\"backslash\\""#,
    ));
    out.push(arr_ok("lit_unicode", r#""\u{1F600} smile""#));
    out.push(arr_ok("lit_hex_escape", r#""\x41\x42""#));
    out.push(arr_ok("lit_raw_string", r##"r#"raw "quotes" inside"#"##));
    out.push(arr_ok("lit_control_escapes", r#""\r\0\'""#));
    out.push(arr_ok("lit_numbers", "0, 123, 4567890"));
    out.push(arr_ok("lit_underscore_unicode", r#""\u{1_F600}""#));
    out.push(arr_ok("lit_line_continuation", "\"a\\\n        b\""));
    out.push(arr_ok("lit_char_literal", "'a', 'b'"));
    out.push(arr_ok("lit_float_literal", "1.5, 2.0"));
    out.push(arr_ok("lit_byte_string", r#"b"bytes""#));
    // `\x` with hex-letter low nibbles (kept <= 0x7F so rustc accepts it).
    out.push(arr_ok("lit_hex_letters", r#""\x7a\x7A""#));
    // backspace + form-feed decode to bytes that re-escape via the \b / \f arms.
    out.push(arr_ok("lit_backspace_formfeed", r#""\u{8}\u{c}\u{1}""#));

    // --- spread/flatten of closures (the Object/Array writer branches) ---
    out.push(obj_ok("obj_flatten_closure", "..|w| { }"));
    out.push(arr_ok("arr_flatten_closure", "..|w| { }"));

    // --- object value variety (template.rs value paths) ---
    out.push(obj_ok("obj_flatten_object_literal", "..{ a: 1, b: 2 }"));
    out.push(obj_ok("obj_todo", "k: todo!()"));
    out.push(obj_ok("obj_multi_token_expr", "a: x + y"));
    out.push(arr_ok("arr_multi_token", "x + y"));
    out.push(obj_ok("obj_shorthand_multi", "x, y"));

    // --- template error paths (template.rs / lit.rs throws) ---
    out.push(obj_err("obj_missing_colon", "colon", "key value"));
    out.push(arr_err(
        "arr_object_flatten_into_array",
        "flatten",
        "..{ a: 1 }",
    ));
    out.push(obj_err(
        "obj_flatten_scalar",
        "Expected object to flatten",
        "..5",
    ));
    out.push(arr_err(
        "arr_flatten_scalar",
        "Expected array to flatten",
        "..5",
    ));
    out.push(obj_err(
        "obj_unexpected_key_punct",
        "Unexpected Punc",
        "+: 1",
    ));
    out.push(obj_err(
        "obj_trailing_after_object",
        "expected a comma",
        "k: { a: 1 } b",
    ));
    out.push(obj_err("obj_at_not_group", "Expected [] attr", "@x: 1"));
    out.push(obj_err(
        "obj_in_not_first",
        "in keyword only allowed",
        "a: 1, in w; b: 2",
    ));
    out.push(obj_err(
        "obj_for_not_first",
        "For comprehensions must be",
        "a: 1, for x in y; z",
    ));
    out.push(obj_err("obj_literal_then_token", "Unknown symbol", "5 x"));
    out.push(obj_err("obj_paren_key", "Expected [key]", "(x): 1"));
    out.push(obj_err("obj_numeric_key", "Unexpected key", "5: 1"));
    out.push(obj_err("obj_float_key", "Unexpected key", "1.5: 1"));
    out.push(obj_err("obj_literal_no_colon", "Expected colon", "5"));
    out.push(obj_err(
        "obj_in_no_semicolon",
        "Unexpected Eof",
        "in writer",
    ));
    out.push(obj_err(
        "obj_match_no_block",
        "Expected Blocked for Match",
        "k: match x",
    ));
    out.push(obj_err(
        "obj_match_paren",
        "Expected Blocked for Match",
        "k: match (x)",
    ));
    out.push(obj_err(
        "obj_empty_value",
        "Expected value after colon",
        "k:, b: 1",
    ));
    out.push(obj_err(
        "obj_array_then_key",
        "expected a comma",
        "k: [1, 2] more: 3",
    ));

    out
}

/// Everything, for a single combined coverage number.
pub fn all() -> Vec<Case> {
    let mut out = valid();
    out.extend(errors());
    out.extend(templates());
    out
}

/// Render the full source of a single case for inspection (`expand case-corpus`).
pub fn describe(case: &Case) -> String {
    let mut s = String::new();
    let driver = match case.driver {
        Driver::Derive => "derive",
        Driver::Object => "object!",
        Driver::Array => "array!",
    };
    let _ = writeln!(s, "// {} [{}]", case.name, driver);
    match &case.expect_error {
        Some(m) => {
            let _ = writeln!(s, "// expect compile_error containing: {m:?}");
        }
        None => {
            let _ = writeln!(s, "// expect clean expansion");
        }
    }
    let _ = writeln!(s, "{}", case.src);
    s
}
