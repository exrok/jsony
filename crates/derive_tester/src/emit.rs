//! Emit a self-checking batch source file from `Case`s.
//!
//! Each batch contains type definitions, one `dt_case_{idx}` oracle function per
//! case, and an integer-indexed dispatcher into them. JSON inputs are loaded at
//! runtime (file arg or stdin), one `idx<TAB>kind<TAB>json` record per line, so a
//! single compiled binary checks many input samples.

use std::fmt::Write;

use proc_macro2::TokenStream;

use crate::gen::{
    version_type_str, via_expected, Body, Case, DefaultSpec, FieldSpec, FlattenKind, FlattenSpec,
    HelperKind, PodFamily, SkipIfField, ValidateKind, VarKind, VariantSpec, VersionFamily,
    ViaFamily, DEFAULT_CONST, VALIDATE_SENTINEL,
};
use crate::schema::Type;

/// Record separator in the runtime input file: `idx<TAB>kind<TAB>json`.
pub(crate) const FIELD_SEP: char = '\t';

/// Input kinds in the record file. `OK` inputs must round-trip, `BAD` inputs
/// (truncated or malformed) must fail without panicking or leaking, `EQ` inputs
/// carry a second JSON in a fourth column and must decode equal to the first.
pub(crate) const KIND_OK: char = 'o';
pub(crate) const KIND_BAD: char = 'b';
pub(crate) const KIND_EQ: char = 'e';
/// `REJECT` inputs must fail to decode (a stricter `BAD`: the decode is required
/// to error, not merely to avoid panicking). Used for the rejecting-validator
/// inputs, where a decode that *accepts* the sentinel is itself the bug.
pub(crate) const KIND_REJECT: char = 'r';
/// `ENCODE` inputs carry a *full canonical* JSON object. The decoded value, when
/// re-encoded with `to_json`, must byte-equal the input — an absolute encode check
/// the self-consistent round trip cannot make. Only emitted for both-direction,
/// float-free cases (see `gen::sample_encode`).
pub(crate) const KIND_ENCODE: char = 'n';

/// Counting global allocator prepended to every batch. The oracle measures the
/// live-byte balance around each parse: it must return to its pre-parse value,
/// otherwise the generated `FromJson` leaked a (possibly partially initialized)
/// allocation. jsony's parser keeps no persistent scratch, so the only
/// allocations crossing a window are those owned by the parsed value or its
/// error, both of which the oracle drops inside the window.
const ALLOCATOR: &str = r#"use std::alloc::{GlobalAlloc, Layout, System};
use std::sync::atomic::{AtomicIsize, Ordering};
static LIVE: AtomicIsize = AtomicIsize::new(0);
struct CountingAlloc;
unsafe impl GlobalAlloc for CountingAlloc {
    unsafe fn alloc(&self, l: Layout) -> *mut u8 {
        let p = System.alloc(l);
        if !p.is_null() { LIVE.fetch_add(l.size() as isize, Ordering::Relaxed); }
        p
    }
    unsafe fn alloc_zeroed(&self, l: Layout) -> *mut u8 {
        let p = System.alloc_zeroed(l);
        if !p.is_null() { LIVE.fetch_add(l.size() as isize, Ordering::Relaxed); }
        p
    }
    unsafe fn dealloc(&self, p: *mut u8, l: Layout) {
        LIVE.fetch_sub(l.size() as isize, Ordering::Relaxed);
        System.dealloc(p, l);
    }
    unsafe fn realloc(&self, p: *mut u8, l: Layout, new_size: usize) -> *mut u8 {
        let np = System.realloc(p, l, new_size);
        if !np.is_null() { LIVE.fetch_add(new_size as isize - l.size() as isize, Ordering::Relaxed); }
        np
    }
}
#[global_allocator]
static GLOBAL: CountingAlloc = CountingAlloc;
"#;

/// The split decode/encode `with` modules referenced by the asymmetric-`with`
/// field (`FromJson with = dt_from_str`, `ToJson with = dt_to_string`). Generic
/// and unused unless a case wires them up, so they cost nothing otherwise. Mirror
/// the shipped `from_str`/`to_string` test helpers: decode parses a JSON string,
/// encode renders the value back as a JSON string.
const WITH_PAIR_HELPERS: &str = r#"mod dt_from_str {
    use std::str::FromStr;
    use jsony::json::DecodeError;
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
                Err(&DecodeError { message: "FromStr failed" })
            }
        }
    }
}
mod dt_to_string {
    use jsony::{TextWriter, ToJson};
    pub fn encode_json<T: ToString>(value: &T, output: &mut TextWriter) {
        value.to_string().encode_json__jsony(output);
    }
}
"#;

/// Whether any field type in the case borrows (needs a `<'a>` parameter). Only
/// the field-carrying shapes can borrow; the self-contained families manage their
/// own definitions and never use the borrowed pool.
fn case_has_lifetime(case: &Case) -> bool {
    let any = |fields: &[FieldSpec]| fields.iter().any(|f| f.ty.lifetimes() != 0);
    match &case.body {
        Body::Named(f) | Body::Tuple(f) => any(f),
        Body::Enum { variants, .. } => variants.iter().any(|v| any(&v.fields)),
        _ => false,
    }
}

/// Render a schema `Type` to its Rust source spelling (e.g. `Vec < u32 >`).
fn type_str(ty: Type) -> String {
    let mut tt = Vec::new();
    Type::gen(&ty, &mut tt);
    TokenStream::from_iter(tt).to_string()
}

/// The generic parameter list for the type *declaration*: `<T, U>` for a generic
/// case, `<'a>` for a borrowing case, else empty. The generator never combines
/// the two (generics are only added to borrow-free cases).
fn decl_generics(case: &Case) -> String {
    if !case.generics.is_empty() {
        let names: Vec<String> = case.generics.iter().map(|g| g.name.to_string()).collect();
        format!("<{}>", names.join(", "))
    } else if case_has_lifetime(case) {
        "<'a>".to_string()
    } else {
        String::new()
    }
}

/// The type reference the oracle decodes/encodes through: the monomorphization
/// `Name<u32, String>` for a generic case, `Name<'_>` for a borrowing case, else
/// the bare `Name`. Monomorphizing here is what makes the generic codegen flow
/// through the real compile/round-trip/ASAN pipeline.
fn type_ref(case: &Case) -> String {
    if !case.generics.is_empty() {
        let args: Vec<String> = case.generics.iter().map(|g| type_str(g.concrete)).collect();
        format!("{}<{}>", case.name, args.join(", "))
    } else if case_has_lifetime(case) {
        format!("{}<'_>", case.name)
    } else {
        case.name.clone()
    }
}

/// A field's declared type spelling in the definition: the generic parameter
/// name when the field is generic, otherwise its concrete type.
fn field_type_str(f: &FieldSpec) -> String {
    match f.generic {
        Some(c) => c.to_string(),
        None => type_str(f.ty),
    }
}

/// `#[jsony(...)]` attribute prefix for a named field (empty if none).
fn field_attr(f: &FieldSpec) -> String {
    if f.skip {
        return "#[jsony(skip)] ".to_string();
    }
    let mut parts = Vec::new();
    if let Some(r) = &f.rename {
        parts.push(format!("rename = {r:?}"));
    }
    if let Some(a) = &f.alias {
        parts.push(format!("alias = {a:?}"));
    }
    // `with` and `validate` are mutually exclusive (jsony rejects both at once).
    if f.with {
        parts.push("with = jsony::helper::json_string".to_string());
    } else {
        match f.validate {
            ValidateKind::None => {}
            ValidateKind::Ok => parts.push(format!("validate = {VALIDATOR_OK}")),
            ValidateKind::Reject => parts.push(format!("validate = {VALIDATOR_REJECT}")),
            ValidateKind::Require => parts.push(format!(
                "validate = jsony::require!(|v| *v != {VALIDATE_SENTINEL}, \"dt sentinel rejected\")"
            )),
        }
    }
    match f.default {
        DefaultSpec::None => {}
        DefaultSpec::Bare => parts.push("default".to_string()),
        DefaultSpec::Expr => parts.push("default = Default::default()".to_string()),
        DefaultSpec::Const => parts.push(format!("default = {DEFAULT_CONST}")),
    }
    if parts.is_empty() {
        String::new()
    } else {
        format!("#[jsony({})] ", parts.join(", "))
    }
}

/// `#[jsony(...)]` attribute prefix for an enum variant (empty if none). Combines
/// the per-variant `rename`, `rename_all`, and `other` markers into one attribute.
fn variant_attr(v: &VariantSpec) -> String {
    let mut parts = Vec::new();
    if let Some(r) = &v.rename {
        parts.push(format!("rename = {r:?}"));
    }
    if let Some(val) = v.rename_all.attr_value() {
        parts.push(format!("rename_all = {val:?}"));
    }
    if v.other {
        parts.push("other".to_string());
    }
    if parts.is_empty() {
        String::new()
    } else {
        format!("    #[jsony({})]\n", parts.join(", "))
    }
}

/// Name of the always-`Ok` validator prepended to every batch and referenced by
/// `#[jsony(validate = ...)]` fields with [`ValidateKind::Ok`].
const VALIDATOR_OK: &str = "dt_validate_ok";

/// Name of the rejecting validator referenced by [`ValidateKind::Reject`] fields.
const VALIDATOR_REJECT: &str = "dt_validate_reject";

/// The two validator definitions prepended to every batch:
///
/// - `dt_validate_ok` accepts any value, proving the validate codegen runs and
///   does not corrupt a well-formed decode.
/// - `dt_validate_reject` rejects the integer sentinel, proving the validator is
///   actually invoked and that its `Err` aborts the decode. `TryFrom<u8>` is
///   implemented for every integer type, and the sampler only attaches it to
///   integer fields, so the bound always holds.
fn validator_defs() -> String {
    format!(
        "fn dt_validate_ok<T>(_value: &T) -> ::std::result::Result<(), String> {{ Ok(()) }}\n\
         fn dt_validate_reject<T: ::std::cmp::PartialEq + ::std::convert::TryFrom<u8>>(v: &T) \
         -> ::std::result::Result<(), String> {{ \
         if let Ok(s) = <T as ::std::convert::TryFrom<u8>>::try_from({VALIDATE_SENTINEL}u8) {{ \
         if *v == s {{ return Err(::std::string::String::from(\"dt sentinel rejected\")); }} }} Ok(()) }}\n"
    )
}

/// The Rust type spelling of a flatten field: the map type, or the companion
/// struct name.
fn flatten_type(fl: &FlattenSpec) -> String {
    match &fl.kind {
        FlattenKind::Map(ty) => type_str(*ty),
        FlattenKind::Companion { name, .. } => name.clone(),
    }
}

/// The struct-body line for a `skip_if` field: `#[jsony(skip_if = <pred>,
/// default)] <name>: <ty>,`. The `default` (bare) supplies the trigger value on
/// decode when encode has omitted the field.
fn skip_if_field_line(sk: &SkipIfField) -> String {
    format!(
        "#[jsony(skip_if = {}, default)] {}: {},",
        sk.kind.predicate(),
        sk.name,
        sk.kind.type_str()
    )
}

/// Emit a companion `#[jsony(Json, Flattenable)]` struct for the companion
/// flatten flavor (a no-op for the map flavor). Its fields are plain scalars, so
/// their JSON keys are the declared names verbatim.
fn emit_flatten_companion(out: &mut String, fl: &FlattenSpec) {
    let FlattenKind::Companion { name, fields } = &fl.kind else {
        return;
    };
    let _ = writeln!(out, "#[derive(jsony::Jsony, PartialEq, Debug)]");
    let _ = writeln!(out, "#[jsony(Json, Flattenable)]");
    let _ = writeln!(out, "struct {name} {{");
    for f in fields {
        let _ = writeln!(out, "    {}: {},", f.name, type_str(f.ty));
    }
    let _ = writeln!(out, "}}");
}

fn emit_def(out: &mut String, case: &Case) {
    // A version family emits several `#[jsony(Binary, version ...)]` structs with
    // their own headers, so it bypasses the single-type definition below.
    if let Body::Version(fam) = &case.body {
        emit_version_defs(out, &case.name, fam);
        return;
    }
    // A POD family emits its own `#[jsony(Binary, zerocopy)]` struct plus wrappers.
    if let Body::Pod(fam) = &case.body {
        emit_pod_defs(out, &case.name, fam);
        return;
    }
    // A via=Iterator family emits its own `#[jsony(ToJson)]` struct.
    if let Body::ViaIter(_) = &case.body {
        emit_via_defs(out, &case.name);
        return;
    }
    // A with-helper / binary-rejection family emits its own struct.
    if let Body::Helper(kind) = &case.body {
        emit_helper_defs(out, &case.name, *kind);
        return;
    }
    let name = &case.name;
    // Declaration generics: `<'a>` when a field borrows, `<T, U>` when the case
    // is generic, else empty (see `decl_generics`).
    let lt = decl_generics(case);
    // A companion `Flattenable` struct (when the flatten field uses that flavor)
    // is defined first so the parent can reference it.
    if let Some(fl) = &case.flatten {
        emit_flatten_companion(out, fl);
    }
    let _ = writeln!(out, "#[derive(jsony::Jsony, PartialEq, Debug)]");
    let _ = writeln!(out, "#[jsony({})]", case.container_attr());
    // transparent FromJson requires repr(transparent).
    if case.transparent {
        let _ = writeln!(out, "#[repr(transparent)]");
    }
    match &case.body {
        Body::Named(fields) => {
            let _ = writeln!(out, "struct {name}{lt} {{");
            for f in fields {
                let _ = writeln!(
                    out,
                    "    {}{}: {},",
                    field_attr(f),
                    f.name,
                    field_type_str(f)
                );
            }
            // The flatten field is declared last, so its inlined keys follow the
            // regular fields in encode order.
            if let Some(fl) = &case.flatten {
                let _ = writeln!(
                    out,
                    "    #[jsony(flatten)] {}: {},",
                    fl.field,
                    flatten_type(fl)
                );
            }
            // The skip_if field (mutually exclusive with flatten) is likewise last.
            if let Some(sk) = &case.skip_if {
                let _ = writeln!(out, "    {}", skip_if_field_line(sk));
            }
            // The asymmetric-`with` field (split decode/encode modules), last too.
            if let Some(wp) = &case.with_pair {
                let _ = writeln!(
                    out,
                    "    #[jsony(FromJson with = dt_from_str, ToJson with = dt_to_string)] {}: u32,",
                    wp.name
                );
            }
            let _ = writeln!(out, "}}");
        }
        Body::Tuple(fields) => {
            let _ = write!(out, "struct {name}{lt}(");
            for f in fields {
                let _ = write!(out, "{}{}, ", field_attr(f), field_type_str(f));
            }
            let _ = writeln!(out, ");");
        }
        Body::Unit => {
            let _ = writeln!(out, "struct {name};");
        }
        Body::Enum { variants, .. } => {
            let _ = writeln!(out, "enum {name}{lt} {{");
            for v in variants {
                out.push_str(&variant_attr(v));
                match v.kind {
                    VarKind::Unit => {
                        let _ = writeln!(out, "    {},", v.name);
                    }
                    VarKind::Tuple => {
                        let _ = writeln!(
                            out,
                            "    {}({}{}),",
                            v.name,
                            field_attr(&v.fields[0]),
                            field_type_str(&v.fields[0])
                        );
                    }
                    VarKind::Struct => {
                        let _ = writeln!(out, "    {} {{", v.name);
                        for f in &v.fields {
                            let _ = writeln!(
                                out,
                                "        {}{}: {},",
                                field_attr(f),
                                f.name,
                                field_type_str(f)
                            );
                        }
                        let _ = writeln!(out, "    }},");
                    }
                }
            }
            let _ = writeln!(out, "}}");
        }
        Body::Str(variants) => {
            let _ = writeln!(out, "enum {name} {{");
            for v in variants {
                if let Some(r) = &v.rename {
                    let _ = writeln!(out, "    #[jsony(rename = {r:?})]");
                }
                let _ = writeln!(out, "    {},", v.name);
            }
            let _ = writeln!(out, "}}");
        }
        Body::Version(_) => unreachable!("version families are emitted above"),
        Body::Pod(_) => unreachable!("POD families are emitted above"),
        Body::ViaIter(_) => unreachable!("via=Iterator families are emitted above"),
        Body::Helper(_) => unreachable!("helper families are emitted above"),
    }
}

/// The unknown string the FromStr oracle asserts is rejected. No generated variant
/// (declared `V{i}`, renamed `ren_V{i}`, or recased) can produce it.
const STR_UNKNOWN: &str = "__dt_no_such_variant__";

/// Emit the signature line for a per-case oracle function `dt_case_{idx}`. Every
/// case body is its own function (kept small so LLVM's super-linear per-function
/// cost stays bounded) and the dispatcher selects it by integer index, not by a
/// string `match` over every case at once.
///
/// `#[inline(never)]` is load-bearing: at `opt-level >= 1` (the ASAN tier)
/// rustc's cross-CGU ThinLTO would otherwise re-inline every once-called case
/// back into the dispatcher, rebuilding the giant function and paying the import
/// cost on top, which more than doubles the compile. Pinning the functions apart
/// keeps the split win across both profiles.
fn emit_case_fn_header(out: &mut String, idx: usize) {
    let _ = writeln!(
        out,
        "#[inline(never)]\nfn dt_case_{idx}(kind: &str, input: &str, extra: &str) -> Result<(), String> {{"
    );
}

/// Emit the dedicated `ToStr`/`FromStr` oracle arm for a Str enum. It is
/// self-contained (no JSON, one trigger record): for every variant it asserts
/// `to_str` yields the expected string and that string `parse`s back to the same
/// variant, then asserts an unknown string fails to parse.
fn emit_str_arm(out: &mut String, idx: usize, case: &Case, variants: &[crate::gen::StrVariant]) {
    let name = &case.name;
    emit_case_fn_header(out, idx);
    let _ = writeln!(
        out,
        "            let before = LIVE.load(Ordering::Relaxed);"
    );
    // `{expected:?}` (a quoted string) is only ever placed in an *operand*
    // position (a `&str` literal); embedding it inside a message format string
    // would nest quotes and break the generated literal.
    for v in variants {
        let expected = v.expected_str(case);
        let vname = &v.name;
        let _ = writeln!(
            out,
            "            if {name}::{vname}.to_str() != {expected:?} {{ return Err(format!(\"to_str {name}::{vname} wrong: got {{:?}}\", {name}::{vname}.to_str())); }}"
        );
        let _ = writeln!(
            out,
            "            match {expected:?}.parse::<{name}>() {{ Ok({name}::{vname}) => {{}}, _ => return Err(format!(\"parse to {name}::{vname} failed\")) }}"
        );
    }
    let _ = writeln!(
        out,
        "            if {STR_UNKNOWN:?}.parse::<{name}>().is_ok() {{ return Err(format!(\"unknown string unexpectedly parsed\")); }}"
    );
    let _ = writeln!(out, "            let after = LIVE.load(Ordering::Relaxed);");
    let _ = writeln!(
        out,
        "            if after != before {{ return Err(format!(\"leak: {{}} bytes (str {name})\", after - before)); }}"
    );
    let _ = writeln!(out, "            Ok(())");
    let _ = writeln!(out, "        }}");
}

/// `#[jsony(version = ..)]` field-attribute prefix for one version field. Base
/// fields (`intro == 0`) carry no version attribute.
fn version_field_attr(f: &crate::gen::VersionField) -> String {
    if f.intro == 0 {
        return String::new();
    }
    match &f.default {
        Some(d) => format!("#[jsony(version = {}, default = {d})] ", f.intro),
        None => format!("#[jsony(version = {})] ", f.intro),
    }
}

/// Emit every schema struct of a version family: `{name}_S{idx}`, each deriving
/// `#[jsony(Binary, version ...)]` and containing the field-plan fields with
/// `intro <= schema.cur`. The min-windowed schema (`min > 0`) uses `version =
/// M..`; the rest use the auto `version` (which resolves to their max field
/// version, i.e. `schema.cur`).
fn emit_version_defs(out: &mut String, name: &str, fam: &VersionFamily) {
    for (idx, schema) in fam.schemas.iter().enumerate() {
        let sname = format!("{name}_S{idx}");
        let ver = if schema.min == 0 {
            "version".to_string()
        } else {
            format!("version = {}..", schema.min)
        };
        let _ = writeln!(out, "#[derive(jsony::Jsony, PartialEq, Debug)]");
        let _ = writeln!(out, "#[jsony(Binary, {ver})]");
        let included = fam
            .fields
            .iter()
            .enumerate()
            .filter(|(_, f)| f.intro <= schema.cur);
        if fam.tuple {
            let _ = write!(out, "struct {sname}(");
            for (_, f) in included {
                let _ = write!(out, "{}{}, ", version_field_attr(f), version_type_str(f.ty));
            }
            let _ = writeln!(out, ");");
        } else {
            let _ = writeln!(out, "struct {sname} {{");
            for (i, f) in included {
                let _ = writeln!(
                    out,
                    "    {}f{i}: {},",
                    version_field_attr(f),
                    version_type_str(f.ty)
                );
            }
            let _ = writeln!(out, "}}");
        }
    }
}

/// Build a literal for schema `sname`, including the field-plan fields with
/// `intro <= cur`, each rendered by `value`.
fn version_literal(
    fam: &VersionFamily,
    sname: &str,
    cur: u16,
    value: impl Fn(&crate::gen::VersionField, usize) -> String,
) -> String {
    let included = || {
        fam.fields
            .iter()
            .enumerate()
            .filter(|(_, f)| f.intro <= cur)
    };
    if fam.tuple {
        let mut s = format!("{sname}(");
        for (i, f) in included() {
            s.push_str(&value(f, i));
            s.push_str(", ");
        }
        s.push(')');
        s
    } else {
        let mut s = format!("{sname} {{ ");
        for (i, f) in included() {
            let _ = write!(s, "f{i}: {}, ", value(f, i));
        }
        s.push('}');
        s
    }
}

/// Emit the self-contained cross-version oracle for a version family. For every
/// ordered pair of schemas `(a, b)`, encode `S{a}`'s sampled value and decode it
/// as `S{b}`:
///
///   - When `a`'s version is inside `b`'s accepted window, the decode must
///     succeed and equal `S{b}` with `a`'s fields carried and `b`-only fields
///     filled from their declared defaults (an absolute default-fill check).
///   - Otherwise the decode must error with a "version" message (the
///     min/max-window rejection), not panic.
///
/// One trigger record fires the whole arm; no JSON inputs are consumed.
fn emit_version_arm(out: &mut String, idx: usize, name: &str, fam: &VersionFamily) {
    emit_case_fn_header(out, idx);
    let _ = writeln!(
        out,
        "            let before = LIVE.load(Ordering::Relaxed);"
    );
    for (a, sa) in fam.schemas.iter().enumerate() {
        let va = sa.cur;
        let enc = version_literal(fam, &format!("{name}_S{a}"), sa.cur, |f, _| f.value.clone());
        for (b, sb) in fam.schemas.iter().enumerate() {
            let _ = writeln!(out, "            {{");
            let _ = writeln!(out, "                let bytes = jsony::to_binary(&{enc});");
            if sb.min <= va && va <= sb.cur {
                let want = version_literal(fam, &format!("{name}_S{b}"), sb.cur, |f, _| {
                    if f.intro <= va {
                        f.value.clone()
                    } else {
                        f.fill().to_string()
                    }
                });
                let _ = writeln!(
                    out,
                    "                let got: {name}_S{b} = match jsony::from_binary(&bytes) {{ Ok(v) => v, Err(e) => return Err(format!(\"version S{a}->S{b}: {{e}}\")) }};"
                );
                // `want` may be a named struct literal; parenthesize it so it is
                // not misparsed as the `if` block in condition position.
                let _ = writeln!(
                    out,
                    "                if got != ({want}) {{ return Err(format!(\"version fill S{a}->S{b}: {{got:?}}\")); }}"
                );
            } else {
                let _ = writeln!(
                    out,
                    "                match jsony::from_binary::<{name}_S{b}>(&bytes) {{ Ok(v) => {{ drop(v); return Err(format!(\"version S{a}->S{b}: out-of-window accepted\")); }} Err(e) => {{ let m = format!(\"{{e}}\"); if !m.contains(\"version\") {{ return Err(format!(\"version S{a}->S{b}: wrong error: {{m}}\")); }} }} }}"
                );
            }
            let _ = writeln!(out, "            }}");
        }
    }
    // Soundness: truncated and garbage versioned bytes must error without
    // panicking or reading out of bounds (exercised under ASAN). The decode
    // result is dropped in-scope so the leak check covers any partial-init drop.
    let max_cur = fam.schemas.iter().map(|s| s.cur).max().unwrap_or(0);
    let big = max_cur as usize;
    let seed = version_literal(fam, &format!("{name}_S{big}"), max_cur, |f, _| {
        f.value.clone()
    });
    let _ = writeln!(out, "            {{");
    let _ = writeln!(out, "                let full = jsony::to_binary(&{seed});");
    let _ = writeln!(
        out,
        "                for cut in [0, full.len()/4, full.len()/2, full.len().saturating_sub(1)] {{ let _ = jsony::from_binary::<{name}_S{big}>(&full[..cut.min(full.len())]); }}"
    );
    let _ = writeln!(
        out,
        "                for raw in [&[][..], &[255u8][..], &[255u8,0,0,0,0,0,0,0,0][..]] {{ let _ = jsony::from_binary::<{name}_S{big}>(raw); }}"
    );
    let _ = writeln!(out, "            }}");
    let _ = writeln!(out, "            let after = LIVE.load(Ordering::Relaxed);");
    let _ = writeln!(
        out,
        "            if after != before {{ return Err(format!(\"leak: {{}} bytes (version {name})\", after - before)); }}"
    );
    let _ = writeln!(out, "            Ok(())");
    let _ = writeln!(out, "        }}");
}

/// Emit a POD family's type definitions: the `#[jsony(Binary, zerocopy)]
/// #[repr(C)]` struct, a `transparent` POD newtype over it, and a `transparent`
/// newtype over `String` (a non-POD, used to assert `POD` is `false` there).
fn emit_pod_defs(out: &mut String, name: &str, fam: &PodFamily) {
    let _ = writeln!(
        out,
        "#[derive(jsony::Jsony, PartialEq, Debug, Clone, Copy)]"
    );
    let _ = writeln!(out, "#[jsony(Binary, zerocopy)]");
    let _ = writeln!(out, "#[repr(C)]");
    let _ = writeln!(out, "struct {name} {{");
    for (i, f) in fam.fields.iter().enumerate() {
        let _ = writeln!(out, "    f{i}: {},", type_str(f.ty));
    }
    let _ = writeln!(out, "}}");
    let _ = writeln!(
        out,
        "#[derive(jsony::Jsony, PartialEq, Debug, Clone, Copy)]"
    );
    let _ = writeln!(out, "#[jsony(Binary, transparent)]");
    let _ = writeln!(out, "#[repr(transparent)]");
    let _ = writeln!(out, "struct {name}T({name});");
    let _ = writeln!(out, "#[derive(jsony::Jsony)]");
    let _ = writeln!(out, "#[jsony(Binary, transparent)]");
    let _ = writeln!(out, "#[repr(transparent)]");
    let _ = writeln!(out, "struct {name}N(String);");
}

/// A `{name} {{ f0: v0, .. }}` literal with each field set to `seed`-th sampled
/// value (so two distinct instances can be built for a slice).
fn pod_literal(name: &str, fam: &PodFamily, seed: usize) -> String {
    let mut s = format!("{name} {{ ");
    for (i, f) in fam.fields.iter().enumerate() {
        // Vary the second instance's values so the slice carries distinct rows.
        let v = if seed == 0 {
            f.value.clone()
        } else {
            crate::gen::pod_value(f.ty, seed * 7 + i + 1)
        };
        let _ = write!(s, "f{i}: {v}, ");
    }
    s.push('}');
    s
}

/// Emit the self-contained POD alignment oracle. Asserts the `POD` const-flags,
/// a scalar and transparent round-trip, then encodes a two-row slice into an
/// 8-aligned scratch at every byte offset `0..8` and decodes it as `&[T]`,
/// `Cow<[T]>`, and `Vec<T>`:
///
///   - `&[T]` borrows (decodes `Ok`) exactly when the rows land aligned, errors
///     otherwise; `Cow` is `Borrowed` iff aligned and `Owned` (a copy) otherwise;
///     both, and `Vec`, must equal the input rows.
///   - Across the offsets at least one aligned (borrow) placement must occur, and
///     for alignment `> 1` at least one unaligned (copy) placement too.
///
/// One trigger record fires the whole arm; no JSON inputs are consumed.
fn emit_pod_arm(out: &mut String, idx: usize, name: &str, fam: &PodFamily) {
    let one = pod_literal(name, fam, 0);
    let two = pod_literal(name, fam, 1);
    emit_case_fn_header(out, idx);
    let _ = writeln!(
        out,
        "            let before = LIVE.load(Ordering::Relaxed);"
    );
    // POD const-flags: the struct and its transparent wrapper are POD; the
    // transparent-over-String wrapper is not.
    for ty in [format!("{name}"), format!("{name}T")] {
        let _ = writeln!(
            out,
            "            if !<{ty} as jsony::FromBinary>::POD || !<{ty} as jsony::ToBinary>::POD {{ return Err(format!(\"{ty} POD flag false\")); }}"
        );
    }
    let _ = writeln!(
        out,
        "            if <{name}N as jsony::FromBinary>::POD || <{name}N as jsony::ToBinary>::POD {{ return Err(format!(\"{name}N (non-POD) reported POD\")); }}"
    );
    // Scalar + transparent round-trip.
    let _ = writeln!(out, "            let one = {one};");
    let _ = writeln!(out, "            {{");
    let _ = writeln!(out, "                let b = jsony::to_binary(&one);");
    let _ = writeln!(
        out,
        "                let d: {name} = match jsony::from_binary(&b) {{ Ok(v) => v, Err(e) => return Err(format!(\"pod self decode: {{e}}\")) }};"
    );
    let _ = writeln!(
        out,
        "                if d != one {{ return Err(format!(\"pod self mismatch\")); }}"
    );
    let _ = writeln!(
        out,
        "                let tb = jsony::to_binary(&{name}T(one));"
    );
    let _ = writeln!(
        out,
        "                let td: {name}T = match jsony::from_binary(&tb) {{ Ok(v) => v, Err(e) => return Err(format!(\"pod transparent decode: {{e}}\")) }};"
    );
    let _ = writeln!(out, "                if td != {name}T(one) {{ return Err(format!(\"pod transparent mismatch\")); }}");
    let _ = writeln!(out, "            }}");
    // Slice alignment oracle.
    let _ = writeln!(out, "            let inputs: &[{name}] = &[{one}, {two}];");
    let _ = writeln!(
        out,
        "            let align = ::std::mem::align_of::<{name}>();"
    );
    let _ = writeln!(
        out,
        "            let mut storage = ::std::mem::MaybeUninit::<[u64; 256]>::uninit();"
    );
    let _ = writeln!(
        out,
        "            let base = storage.as_mut_ptr() as *mut u8;"
    );
    let _ = writeln!(out, "            let mut saw_borrow = false;");
    let _ = writeln!(out, "            let mut saw_copy = false;");
    let _ = writeln!(out, "            for off in 0..8usize {{");
    let _ = writeln!(
        out,
        "                let region = unsafe {{ ::std::slice::from_raw_parts_mut(base.add(off) as *mut ::std::mem::MaybeUninit<u8>, 2048 - off) }};"
    );
    let _ = writeln!(
        out,
        "                let enc = jsony::to_binary_into(&inputs, region);"
    );
    let _ = writeln!(
        out,
        "                let slice_ok = match jsony::from_binary::<&[{name}]>(&enc) {{ Ok(s) => {{ if s != inputs {{ return Err(format!(\"pod aligned slice mismatch off={{off}}\")); }} true }} Err(_) => false }};"
    );
    let _ = writeln!(
        out,
        "                match jsony::from_binary::<::std::borrow::Cow<'_, [{name}]>>(&enc) {{ Ok(c) => {{ if &*c != inputs {{ return Err(format!(\"pod cow mismatch off={{off}}\")); }} let b = matches!(c, ::std::borrow::Cow::Borrowed(_)); if b != slice_ok {{ return Err(format!(\"pod cow/align disagree off={{off}}\")); }} }} Err(e) => return Err(format!(\"pod cow decode off={{off}}: {{e}}\")) }};"
    );
    let _ = writeln!(
        out,
        "                match jsony::from_binary::<Vec<{name}>>(&enc) {{ Ok(v) => {{ if v != inputs {{ return Err(format!(\"pod vec mismatch off={{off}}\")); }} }} Err(e) => return Err(format!(\"pod vec decode off={{off}}: {{e}}\")) }};"
    );
    let _ = writeln!(
        out,
        "                if slice_ok {{ saw_borrow = true; }} else {{ saw_copy = true; }}"
    );
    let _ = writeln!(out, "            }}");
    let _ = writeln!(out, "            if !saw_borrow {{ return Err(format!(\"pod never borrowed an aligned slice ({name})\")); }}");
    let _ = writeln!(out, "            if align > 1 && !saw_copy {{ return Err(format!(\"pod never hit the unaligned copy path ({name})\")); }}");
    let _ = writeln!(out, "            let after = LIVE.load(Ordering::Relaxed);");
    let _ = writeln!(
        out,
        "            if after != before {{ return Err(format!(\"leak: {{}} bytes (pod {name})\", after - before)); }}"
    );
    let _ = writeln!(out, "            Ok(())");
    let _ = writeln!(out, "        }}");
}

/// Emit a via=Iterator family's type definition: a `#[jsony(ToJson)]` struct with
/// a leading `a: u32` and a flattened `Vec<(String, i64)>` rendered via Iterator.
fn emit_via_defs(out: &mut String, name: &str) {
    let _ = writeln!(out, "#[derive(jsony::Jsony)]");
    let _ = writeln!(out, "#[jsony(ToJson)]");
    let _ = writeln!(out, "struct {name} {{");
    let _ = writeln!(out, "    a: u32,");
    let _ = writeln!(out, "    #[jsony(flatten, via = Iterator)]");
    let _ = writeln!(out, "    pairs: Vec<(String, i64)>,");
    let _ = writeln!(out, "}}");
}

/// Emit the self-contained via=Iterator encode oracle: construct the value with
/// its (possibly duplicate-keyed) pairs, encode it, and assert byte-equality with
/// the expected inline-object output. One trigger record fires the arm.
fn emit_via_arm(out: &mut String, idx: usize, name: &str, fam: &ViaFamily) {
    let expected = via_expected(fam);
    emit_case_fn_header(out, idx);
    let _ = writeln!(
        out,
        "            let before = LIVE.load(Ordering::Relaxed);"
    );
    // The value and its encoding own heap allocations (the pair strings), so they
    // are scoped to drop before the leak check below.
    let _ = writeln!(out, "            {{");
    let _ = write!(
        out,
        "                let v = {name} {{ a: {}u32, pairs: vec![",
        fam.lead
    );
    for (k, val) in &fam.pairs {
        let _ = write!(out, "({k:?}.to_string(), {val}i64), ");
    }
    let _ = writeln!(out, "] }};");
    let _ = writeln!(out, "                let s = jsony::to_json(&v);");
    // `expected` is placed as an operand (a `&str` literal), not inside a format
    // string, since it contains braces and quotes.
    let _ = writeln!(
        out,
        "                if s != {expected:?} {{ return Err(format!(\"via encode mismatch: got {{s}}\")); }}"
    );
    let _ = writeln!(out, "            }}");
    let _ = writeln!(out, "            let after = LIVE.load(Ordering::Relaxed);");
    let _ = writeln!(
        out,
        "            if after != before {{ return Err(format!(\"leak: {{}} bytes (via {name})\", after - before)); }}"
    );
    let _ = writeln!(out, "            Ok(())");
    let _ = writeln!(out, "        }}");
}

/// Emit the type definition for a with-helper / binary-rejection family.
fn emit_helper_defs(out: &mut String, name: &str, kind: HelperKind) {
    let _ = writeln!(out, "#[derive(jsony::Jsony, PartialEq, Debug)]");
    match kind {
        HelperKind::ObjVecString => {
            let _ = writeln!(out, "#[jsony(Json)]");
            let _ = writeln!(out, "struct {name} {{");
            let _ = writeln!(
                out,
                "    #[jsony(with = jsony::helper::object_as_vec_of_tuple)] pairs: Vec<(String, i64)>,"
            );
            let _ = writeln!(out, "}}");
        }
        HelperKind::ObjVecNum => {
            let _ = writeln!(out, "#[jsony(Json)]");
            let _ = writeln!(out, "struct {name} {{");
            let _ = writeln!(
                out,
                "    #[jsony(with = jsony::helper::object_as_vec_of_tuple)] pairs: Vec<(u32, u32)>,"
            );
            let _ = writeln!(out, "}}");
        }
        HelperKind::OwnedCow => {
            let _ = writeln!(out, "#[jsony(Binary, Json)]");
            let _ = writeln!(out, "struct {name}<'a> {{");
            let _ = writeln!(
                out,
                "    #[jsony(From with = jsony::helper::owned_cow)] shared: Cow<'a, [&'a str]>,"
            );
            let _ = writeln!(out, "}}");
        }
        HelperKind::BinReject => {
            let _ = writeln!(out, "#[jsony(Binary)]");
            let _ = writeln!(out, "struct {name} {{");
            let _ = writeln!(
                out,
                "    #[jsony(validate = jsony::require!(|v| *v != {VALIDATE_SENTINEL}, \"dt sentinel rejected\"))] x: u32,"
            );
            let _ = writeln!(out, "}}");
        }
    }
}

/// Emit the self-contained oracle for a with-helper / binary-rejection family.
/// All bindings live inside an inner block so heap allocations drop before the
/// leak check. One trigger record fires the whole arm.
fn emit_helper_arm(out: &mut String, idx: usize, name: &str, kind: HelperKind) {
    emit_case_fn_header(out, idx);
    let _ = writeln!(
        out,
        "            let before = LIVE.load(Ordering::Relaxed);"
    );
    let _ = writeln!(out, "            {{");
    match kind {
        HelperKind::ObjVecString => {
            let expected = "{\"pairs\":{\"a\":1,\"bb\":22}}";
            let _ = writeln!(
                out,
                "                let v = {name} {{ pairs: vec![(\"a\".to_string(), 1i64), (\"bb\".to_string(), 22i64)] }};"
            );
            let _ = writeln!(out, "                let s = jsony::to_json(&v);");
            let _ = writeln!(
                out,
                "                if s != {expected:?} {{ return Err(format!(\"obj_vec_str encode: got {{s}}\")); }}"
            );
            let _ = writeln!(
                out,
                "                let d: {name} = match jsony::from_json(&s) {{ Ok(v) => v, Err(e) => return Err(format!(\"obj_vec_str decode: {{e}}\")) }};"
            );
            let _ = writeln!(out, "                if d != v {{ return Err(format!(\"obj_vec_str roundtrip mismatch\")); }}");
        }
        HelperKind::ObjVecNum => {
            let expected = "{\"pairs\":{\"34\":234,\"112\":452}}";
            let _ = writeln!(
                out,
                "                let v = {name} {{ pairs: vec![(34u32, 234u32), (112u32, 452u32)] }};"
            );
            let _ = writeln!(out, "                let s = jsony::to_json(&v);");
            let _ = writeln!(
                out,
                "                if s != {expected:?} {{ return Err(format!(\"obj_vec_num encode: got {{s}}\")); }}"
            );
            let _ = writeln!(
                out,
                "                let d: {name} = match jsony::from_json({expected:?}) {{ Ok(v) => v, Err(e) => return Err(format!(\"obj_vec_num decode: {{e}}\")) }};"
            );
            let _ = writeln!(out, "                if d != v {{ return Err(format!(\"obj_vec_num roundtrip mismatch\")); }}");
        }
        HelperKind::OwnedCow => {
            let expected = "{\"shared\":[\"hello\",\"nice\"]}";
            let _ = writeln!(
                out,
                "                let v = {name} {{ shared: Cow::Borrowed(&[\"hello\", \"nice\"]) }};"
            );
            let _ = writeln!(out, "                let s = jsony::to_json(&v);");
            let _ = writeln!(
                out,
                "                if s != {expected:?} {{ return Err(format!(\"owned_cow encode: got {{s}}\")); }}"
            );
            let _ = writeln!(
                out,
                "                let d: {name}<'_> = match jsony::from_json(&s) {{ Ok(v) => v, Err(e) => return Err(format!(\"owned_cow json decode: {{e}}\")) }};"
            );
            let _ = writeln!(
                out,
                "                if d != v {{ return Err(format!(\"owned_cow json mismatch\")); }}"
            );
            let _ = writeln!(out, "                let b = jsony::to_binary(&v);");
            let _ = writeln!(
                out,
                "                let d2: {name}<'_> = match jsony::from_binary(&b) {{ Ok(v) => v, Err(_) => return Err(format!(\"owned_cow binary decode failed\")) }};"
            );
            let _ = writeln!(out, "                if d2 != v {{ return Err(format!(\"owned_cow binary mismatch\")); }}");
        }
        HelperKind::BinReject => {
            let _ = writeln!(
                out,
                "                let gb = jsony::to_binary(&{name} {{ x: 5u32 }});"
            );
            let _ = writeln!(
                out,
                "                match jsony::from_binary::<{name}>(&gb) {{ Ok(v) => drop(v), Err(e) => return Err(format!(\"binreject valid value rejected: {{e}}\")) }};"
            );
            let _ = writeln!(
                out,
                "                let bb = jsony::to_binary(&{name} {{ x: {VALIDATE_SENTINEL}u32 }});"
            );
            let _ = writeln!(
                out,
                "                match jsony::from_binary::<{name}>(&bb) {{ Ok(v) => {{ drop(v); return Err(format!(\"binreject sentinel accepted\")); }} Err(e) => drop(e) }};"
            );
        }
    }
    let _ = writeln!(out, "            }}");
    let _ = writeln!(out, "            let after = LIVE.load(Ordering::Relaxed);");
    let _ = writeln!(
        out,
        "            if after != before {{ return Err(format!(\"leak: {{}} bytes (helper {name})\", after - before)); }}"
    );
    let _ = writeln!(out, "            Ok(())");
    let _ = writeln!(out, "        }}");
}

fn emit_arm(out: &mut String, idx: usize, case: &Case) {
    if let Body::Str(variants) = &case.body {
        emit_str_arm(out, idx, case, variants);
        return;
    }
    if let Body::Version(fam) = &case.body {
        emit_version_arm(out, idx, &case.name, fam);
        return;
    }
    if let Body::Pod(fam) = &case.body {
        emit_pod_arm(out, idx, &case.name, fam);
        return;
    }
    if let Body::ViaIter(fam) = &case.body {
        emit_via_arm(out, idx, &case.name, fam);
        return;
    }
    if let Body::Helper(kind) = &case.body {
        emit_helper_arm(out, idx, &case.name, *kind);
        return;
    }
    // Type-position spelling: `T<'_>` when the type borrows, `T<u32, ..>` when it
    // is generic (the oracle monomorphization), else the bare name.
    let tyref = type_ref(case);
    let can_decode = case.traits.from_json;
    let can_encode = case.traits.to_json;
    let bin = case.traits.binary();
    emit_case_fn_header(out, idx);
    // Live-byte baseline. Every path below restores it before the leak check,
    // unless it early-returns on a correctness failure.
    let _ = writeln!(
        out,
        "            let before = LIVE.load(Ordering::Relaxed);"
    );
    let _ = writeln!(out, "            match kind {{");

    if can_decode {
        // Negative path: a truncated/malformed/duplicate-key input must fail
        // (or, rarely, parse) without panicking. The value or error drops in
        // this scope so the leak check catches a partially-initialized field the
        // generated decoder failed to drop on the error path.
        let _ = writeln!(
            out,
            "                \"{KIND_BAD}\" => {{ match jsony::from_json::<{tyref}>(input) {{ Ok(v) => drop(v), Err(e) => drop(e) }} }}"
        );
        // Reject path: the input must error (e.g. a rejecting validator firing on
        // its sentinel). A decode that *accepts* it is the failure. The value or
        // error drops in scope so the leak check still covers the partial-init
        // drop the rejection triggers.
        let _ = writeln!(
            out,
            "                \"{KIND_REJECT}\" => {{ match jsony::from_json::<{tyref}>(input) {{ Ok(v) => {{ drop(v); return Err(format!(\"reject input accepted: json={{input}}\")); }} Err(e) => drop(e) }} }}"
        );
        // Equivalence path: `input` and `extra` are two encodings of the same
        // value (key-permuted or whitespace-injected). Both must decode and
        // compare equal. This probes the decoder's insensitivity to key order
        // and insignificant whitespace independently of the round-trip oracle,
        // which cannot catch a decode that is self-consistently wrong.
        let _ = writeln!(out, "                \"{KIND_EQ}\" => {{");
        let _ = writeln!(
            out,
            "                    let a: {tyref} = match jsony::from_json(input) {{ Ok(v) => v, Err(e) => return Err(format!(\"equiv a from_json: {{e}} a={{input}}\")) }};"
        );
        let _ = writeln!(
            out,
            "                    let b: {tyref} = match jsony::from_json(extra) {{ Ok(v) => v, Err(e) => return Err(format!(\"equiv b from_json: {{e}} b={{extra}}\")) }};"
        );
        let _ = writeln!(
            out,
            "                    if a != b {{ return Err(format!(\"equivalence mismatch: {{a:?}} != {{b:?}} a={{input}} b={{extra}}\")); }}"
        );
        let _ = writeln!(out, "                }}");
        if can_encode {
            // Absolute encode oracle (two-column): decode `input` (col3),
            // re-encode, and compare to `expected` (col4, defaulting to `input`
            // when col4 is empty — the symmetric round-trip case). The asymmetric
            // form (col4 != col3) checks attributes whose decode and encode forms
            // differ: `skip_if` (a present trigger value re-encodes omitted),
            // `via`, and an asymmetric `with` pair. Catches encode bugs (wrong key
            // order/casing, dropped/extra fields, wrong tag placement) the round
            // trip misses.
            let _ = writeln!(out, "                \"{KIND_ENCODE}\" => {{");
            let _ = writeln!(
                out,
                "                    let v: {tyref} = match jsony::from_json(input) {{ Ok(v) => v, Err(e) => return Err(format!(\"encode-oracle decode: {{e}} json={{input}}\")) }};"
            );
            let _ = writeln!(out, "                    let s = jsony::to_json(&v);");
            let _ = writeln!(
                out,
                "                    let expected = if extra.is_empty() {{ input }} else {{ extra }};"
            );
            let _ = writeln!(
                out,
                "                    if s != expected {{ return Err(format!(\"encode mismatch: got {{s}} want {{expected}}\")); }}"
            );
            let _ = writeln!(out, "                }}");
        }
    }

    // Positive path. Each binding lives inside this arm's scope so it drops
    // before the leak check below.
    let _ = writeln!(out, "                _ => {{");
    if can_decode {
        let _ = writeln!(
            out,
            "                    let v1: {tyref} = match jsony::from_json(input) {{ Ok(v) => v, Err(e) => return Err(format!(\"from_json: {{e}} json={{input}}\")) }};"
        );
        if can_encode {
            let _ = writeln!(out, "                    let s = jsony::to_json(&v1);");
            let _ = writeln!(
                out,
                "                    let v2: {tyref} = match jsony::from_json(&s) {{ Ok(v) => v, Err(e) => return Err(format!(\"reparse: {{e}} json={{s}}\")) }};"
            );
            let _ = writeln!(
                out,
                "                    if v1 != v2 {{ return Err(format!(\"json mismatch: {{v1:?}} != {{v2:?}} json={{s}}\")); }}"
            );
        }
        if bin {
            let _ = writeln!(
                out,
                "                    let bytes = jsony::to_binary(&v1);"
            );
            let _ = writeln!(
                out,
                "                    let v3: {tyref} = match jsony::from_binary(&bytes) {{ Ok(v) => v, Err(_) => return Err(format!(\"from_binary failed (json={{input}})\")) }};"
            );
            let _ = writeln!(
                out,
                "                    if v1 != v3 {{ return Err(format!(\"binary mismatch: {{v1:?}} != {{v3:?}} (json={{input}})\")); }}"
            );
        }
    } else {
        // ToJson-only: no decoder. Reserved for the value-literal oracle; for
        // now no such case is generated, so the input is unused.
        let _ = writeln!(out, "                    let _ = (input, extra);");
    }
    let _ = writeln!(out, "                }}");
    let _ = writeln!(out, "            }}");

    // Leak check: live bytes must be back to baseline.
    let _ = writeln!(out, "            let after = LIVE.load(Ordering::Relaxed);");
    let _ = writeln!(
        out,
        "            if after != before {{ return Err(format!(\"leak: {{}} bytes (kind={{kind}} json={{input}})\", after - before)); }}"
    );
    let _ = writeln!(out, "            Ok(())");
    let _ = writeln!(out, "        }}");
}

/// The standalone type definition(s) for one case: exactly the items carrying
/// `#[derive(jsony::Jsony)]`, with none of the batch preamble (allocator,
/// validators, helper modules). A case can expand to several items (a flatten
/// companion, the schema structs of a version family, the three POD wrappers).
/// Used by `run::expand` to drive the derive in-process, so the source needs to
/// tokenize but never has to type-check or link.
pub(crate) fn case_source(case: &Case) -> String {
    let mut out = String::new();
    emit_def(&mut out, case);
    out
}

/// Build the full batch source for the given cases.
pub(crate) fn emit_batch(cases: &[Case]) -> String {
    let mut out = String::new();
    out.push_str("#![allow(dead_code, non_snake_case, non_camel_case_types, unused)]\n");
    out.push_str("use std::io::Read;\n");
    out.push_str("use std::collections::{HashMap, BTreeMap};\n");
    out.push_str("use std::borrow::Cow;\n");
    out.push_str(ALLOCATOR);
    out.push_str(&validator_defs());
    out.push_str(WITH_PAIR_HELPERS);
    out.push('\n');

    for case in cases {
        emit_def(&mut out, case);
        out.push('\n');
    }

    // Each case body is its own function. Splitting them keeps every function
    // small, so LLVM's super-linear per-function cost (present even at -O0) stays
    // bounded instead of compounding across one giant `run_case` body.
    for (idx, case) in cases.iter().enumerate() {
        emit_arm(&mut out, idx, case);
        out.push('\n');
    }

    // Index -> type name, only for the human-readable `FAIL` line. Keeping the
    // names out of the hot dispatch (and out of the input records) lets the
    // dispatcher select by integer.
    out.push_str("static DT_NAMES: &[&str] = &[\n");
    for case in cases {
        let _ = writeln!(out, "    {:?},", case.name);
    }
    out.push_str("];\n\n");

    // Integer dispatch: a jump table over the case index, far cheaper to compile
    // than a string `match` whose every arm carried a full case body.
    out.push_str(
        "fn run_case(idx: u32, kind: &str, input: &str, extra: &str) -> Result<(), String> {\n",
    );
    out.push_str("    match idx {\n");
    for idx in 0..cases.len() {
        let _ = writeln!(out, "        {idx} => dt_case_{idx}(kind, input, extra),");
    }
    out.push_str("        _ => Err(format!(\"unknown index {idx}\")),\n");
    out.push_str("    }\n");
    out.push_str("}\n\n");

    out.push_str("fn main() {\n");
    out.push_str("    let mut data = String::new();\n");
    out.push_str("    match std::env::args().nth(1) {\n");
    out.push_str(
        "        Some(path) => data = std::fs::read_to_string(&path).expect(\"read input file\"),\n",
    );
    out.push_str(
        "        None => { std::io::stdin().read_to_string(&mut data).expect(\"read stdin\"); }\n",
    );
    out.push_str("    }\n");
    out.push_str("    let mut total = 0u64;\n");
    out.push_str("    let mut fails = 0u64;\n");
    out.push_str("    for line in data.lines() {\n");
    out.push_str("        if line.is_empty() { continue; }\n");
    let _ = writeln!(
        out,
        "        let mut cols = line.splitn(4, '{}');",
        FIELD_SEP.escape_default()
    );
    out.push_str(
        "        let idx: u32 = cols.next().expect(\"idx\").parse().expect(\"idx parse\");\n",
    );
    out.push_str("        let kind = cols.next().expect(\"kind\");\n");
    out.push_str("        let input = cols.next().unwrap_or(\"\");\n");
    out.push_str("        let extra = cols.next().unwrap_or(\"\");\n");
    out.push_str("        total += 1;\n");
    out.push_str(
        "        if let Err(e) = run_case(idx, kind, input, extra) { let name = DT_NAMES.get(idx as usize).copied().unwrap_or(\"?\"); eprintln!(\"FAIL {name}: {e}\"); fails += 1; }\n",
    );
    out.push_str("    }\n");
    out.push_str(
        "    if fails > 0 { eprintln!(\"{fails}/{total} FAILURES\"); std::process::exit(1); }\n",
    );
    out.push_str("    println!(\"OK {total} cases\");\n");
    out.push_str("}\n");
    out
}
