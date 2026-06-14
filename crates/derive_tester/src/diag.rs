//! Diagnostics tier: deliberately-broken derive cases with span + message
//! assertions.
//!
//! This is `trybuild` in spirit, but the cases are programmatic and the compile
//! step reuses the harness's direct-`rustc` pipeline (`compile::Toolchain`), so
//! many cases check in parallel against one shared, pre-built dependency set
//! rather than one cargo invocation per case.
//!
//! Each [`DiagCase`] is a standalone source whose only defect is the mistake
//! under test. We type-check it with `--error-format=json`, parse the resulting
//! diagnostics (with jsony itself), and assert two things:
//!
//! 1. every [`Expect`] is matched by some error whose message contains the
//!    expected text and whose primary span points at the expected source token,
//! 2. no [`Forbid`] is matched, in particular the implicit rule that no error
//!    may blame the `#[derive(jsony::Jsony)]` entry point instead of the token
//!    the user actually got wrong.
//!
//! The expectations encode the diagnostic an end user *should* get, not the one
//! the current macro happens to produce. A case that fails is a real gap in the
//! diagnostics, not a broken test.

use std::fmt::Write as _;

use jsony::Jsony;
use rand::prelude::*;

use crate::compile::Toolchain;
use crate::gen::mix64;

// --- rustc JSON diagnostic model (only the fields we consume) ---
//
// rustc emits one JSON object per line on stderr under `--error-format=json`.
// jsony ignores unknown keys, so these declare the small subset we read out of
// the much larger diagnostic objects.

#[derive(Jsony)]
#[jsony(FromJson)]
struct RawSpan {
    byte_start: u32,
    byte_end: u32,
    line_start: u32,
    column_start: u32,
    is_primary: bool,
}

#[derive(Jsony)]
#[jsony(FromJson)]
struct RawCode {
    code: String,
}

#[derive(Jsony)]
#[jsony(FromJson)]
struct RawDiag {
    message: String,
    level: String,
    #[jsony(default)]
    code: Option<RawCode>,
    #[jsony(default)]
    spans: Vec<RawSpan>,
}

impl RawDiag {
    fn primary(&self) -> Option<&RawSpan> {
        self.spans.iter().find(|s| s.is_primary)
    }
    fn code(&self) -> &str {
        self.code.as_ref().map(|c| c.code.as_str()).unwrap_or("")
    }
}

/// Parse the error-level diagnostics out of a raw JSON stderr stream. Non-error
/// lines (warnings, the "aborting due to" summary, anything that fails to parse)
/// are dropped.
fn parse_errors(json: &str) -> Vec<RawDiag> {
    let mut out = Vec::new();
    for line in json.lines() {
        let line = line.trim();
        if !line.starts_with('{') {
            continue;
        }
        if let Ok(d) = jsony::from_json::<RawDiag>(line) {
            if d.level == "error" {
                out.push(d);
            }
        }
    }
    out
}

// --- Expectation model ---

/// How tightly a primary span must line up with the expected token text.
#[derive(Clone, Copy)]
pub enum SpanMode {
    /// The span's byte range must be contained in the needle's range. Tolerates
    /// the macro pointing at a sub-token (e.g. `NoImpl` inside `bad: NoImpl`).
    Within,
    /// The span's byte range must equal the needle's range exactly. Used where
    /// the macro itself sets the span (its own `compile_error!`), so an exact
    /// range is a meaningful regression guard.
    Exact,
}

/// One error the case must produce: a message substring and the source token its
/// primary span must point at.
pub struct Expect {
    /// Substring the error message must contain.
    pub message: String,
    /// Source text the primary span must point at. Pick a snippet unique in the
    /// source so it is unambiguous.
    pub needle: String,
    /// Which occurrence of `needle` to target (0-based; usually 0).
    pub nth: usize,
    pub mode: SpanMode,
}

impl Expect {
    pub fn within(message: &str, needle: &str) -> Expect {
        Expect {
            message: message.into(),
            needle: needle.into(),
            nth: 0,
            mode: SpanMode::Within,
        }
    }
    pub fn exact(message: &str, needle: &str) -> Expect {
        Expect {
            message: message.into(),
            needle: needle.into(),
            nth: 0,
            mode: SpanMode::Exact,
        }
    }
}

/// A diagnostic that must NOT occur: no error primary span may fall within the
/// `needle` text (optionally only when the message contains `message`).
pub struct Forbid {
    pub needle: String,
    pub nth: usize,
    pub message: String,
}

impl Forbid {
    /// The implicit per-case forbid: no error may blame the derive entry point.
    fn derive_path() -> Forbid {
        Forbid {
            needle: DERIVE_PATH.into(),
            nth: 0,
            message: String::new(),
        }
    }
}

/// The token every source uses for the derive entry point. The implicit
/// per-case forbid asserts no error blames it, which is the central promise of
/// the tier: a mistake on a field or attribute must point at that field or
/// attribute, never at the derive.
const DERIVE_PATH: &str = "jsony::Jsony";

pub struct DiagCase {
    pub name: String,
    pub source: String,
    pub expects: Vec<Expect>,
    pub forbid: Vec<Forbid>,
    /// When set, this case asserts an ideal we have assessed as not currently
    /// worth the implementation cost. A failing known-gap case is reported but
    /// does not fail the run, and a passing one is flagged so the marker can be
    /// removed. The string is the rationale.
    pub known_gap: Option<&'static str>,
}

impl DiagCase {
    /// Mark a case as a deferred-by-cost gap with the given rationale.
    fn known(mut self, reason: &'static str) -> DiagCase {
        self.known_gap = Some(reason);
        self
    }
}

/// Build a case, attaching the implicit "do not blame the derive" forbid.
fn case(name: impl Into<String>, source: String, expects: Vec<Expect>) -> DiagCase {
    DiagCase {
        name: name.into(),
        source,
        expects,
        forbid: vec![Forbid::derive_path()],
        known_gap: None,
    }
}

// --- Matching ---

/// Byte range of the `nth` occurrence of `needle` in `source`.
fn locate(source: &str, needle: &str, nth: usize) -> Option<(usize, usize)> {
    let mut start = 0;
    let mut count = 0;
    loop {
        let i = source[start..].find(needle)? + start;
        if count == nth {
            return Some((i, i + needle.len()));
        }
        count += 1;
        start = i + 1;
    }
}

fn span_satisfies(s: &RawSpan, lo: usize, hi: usize, mode: SpanMode) -> bool {
    let (sb, se) = (s.byte_start as usize, s.byte_end as usize);
    match mode {
        SpanMode::Within => sb >= lo && se <= hi,
        SpanMode::Exact => sb == lo && se == hi,
    }
}

/// The verdict for one case: which expectations went unmet and which forbidden
/// diagnostics occurred, plus the actual errors for debugging.
pub struct Outcome {
    pub name: String,
    pub unmet: Vec<String>,
    pub violations: Vec<String>,
    pub actual: Vec<String>,
}

impl Outcome {
    pub fn passed(&self) -> bool {
        self.unmet.is_empty() && self.violations.is_empty()
    }

    /// A multi-line failure report: each unmet expectation and violation, then
    /// the actual errors the compiler produced.
    pub fn render(&self) -> String {
        let mut s = format!("FAIL {}\n", self.name);
        for u in &self.unmet {
            let _ = writeln!(s, "    unmet: {u}");
        }
        for v in &self.violations {
            let _ = writeln!(s, "    violation: {v}");
        }
        if self.actual.is_empty() {
            let _ = writeln!(s, "    (no compiler errors produced)");
        } else {
            let _ = writeln!(s, "    actual errors:");
            for a in &self.actual {
                let _ = writeln!(s, "      - {a}");
            }
        }
        s
    }
}

/// One-line description of an actual error and where its primary span landed.
fn describe(source: &str, d: &RawDiag) -> String {
    let head = d.message.lines().next().unwrap_or("");
    match d.primary() {
        Some(s) => {
            let (sb, se) = (s.byte_start as usize, s.byte_end as usize);
            let snippet = source.get(sb..se).unwrap_or("?");
            format!(
                "[{}] {head} @ {}:{} {snippet:?}",
                d.code(),
                s.line_start,
                s.column_start
            )
        }
        None => format!("[{}] {head} @ (no primary span)", d.code()),
    }
}

fn evaluate(case: &DiagCase, diags: &[RawDiag]) -> Outcome {
    let src = &case.source;
    let mut unmet = Vec::new();
    for e in &case.expects {
        let Some((lo, hi)) = locate(src, &e.needle, e.nth) else {
            unmet.push(format!(
                "needle {:?} (#{}) not present in source",
                e.needle, e.nth
            ));
            continue;
        };
        let mut matched = false;
        for d in diags {
            if !d.message.contains(e.message.as_str()) {
                continue;
            }
            if let Some(s) = d.primary() {
                if span_satisfies(s, lo, hi, e.mode) {
                    matched = true;
                    break;
                }
            }
        }
        if !matched {
            unmet.push(format!(
                "no error with message ~{:?} and primary span on {:?}",
                e.message, e.needle
            ));
        }
    }

    let mut violations = Vec::new();
    for f in &case.forbid {
        let Some((lo, hi)) = locate(src, &f.needle, f.nth) else {
            continue;
        };
        for d in diags {
            if !f.message.is_empty() && !d.message.contains(f.message.as_str()) {
                continue;
            }
            if let Some(s) = d.primary() {
                let (sb, se) = (s.byte_start as usize, s.byte_end as usize);
                // Overlap, not containment: the blame need only touch the
                // forbidden region to be wrong.
                if sb < hi && se > lo {
                    violations.push(format!(
                        "error {:?} blames {:?}",
                        d.message.lines().next().unwrap_or(""),
                        f.needle
                    ));
                    break;
                }
            }
        }
    }

    let actual = diags.iter().map(|d| describe(src, d)).collect();
    Outcome {
        name: case.name.clone(),
        unmet,
        violations,
        actual,
    }
}

/// Compile one case and judge it. `tag` names the temp files; pass a value
/// unique among concurrently-running cases.
pub fn check_case(tc: &Toolchain, tag: &str, case: &DiagCase) -> Outcome {
    match tc.check_json(tag, &case.source) {
        Ok(json) => {
            let diags = parse_errors(&json);
            evaluate(case, &diags)
        }
        Err(e) => Outcome {
            name: case.name.clone(),
            unmet: vec![format!("compile invocation failed: {e}")],
            violations: Vec::new(),
            actual: Vec::new(),
        },
    }
}

// --- Curated catalog ---

/// Wrap a derive body in a minimal compilable crate. `header` carries any extra
/// item definitions (e.g. a type without jsony impls) placed before the derive.
fn src(header: &str, derive: &str) -> String {
    format!("#![allow(dead_code)]\n{header}{derive}\nfn main() {{}}\n")
}

/// Wrap a statement `body` inside `fn main`, with `header` items above it. Used
/// for the `object!`/`array!` template-macro cases, which appear in expression
/// position rather than as items.
fn src_in_main(header: &str, body: &str) -> String {
    format!("#![allow(dead_code)]\n{header}fn main() {{\n{body}\n}}\n")
}

/// A template-macro case carries no derive, so it gets no derive-path forbid.
fn tcase(name: impl Into<String>, source: String, expects: Vec<Expect>) -> DiagCase {
    DiagCase {
        name: name.into(),
        source,
        expects,
        forbid: Vec::new(),
        known_gap: None,
    }
}

/// The hand-written cases. Each targets one concrete mistake and asserts the
/// diagnostic an end user should receive.
pub fn catalog() -> Vec<DiagCase> {
    let mut out = Vec::new();

    // 1. Field type missing FromJson. The error must point at the field, not the
    //    derive. (Currently good.)
    out.push(case(
        "field_missing_fromjson",
        src(
            "struct NoFromJson;\n",
            "#[derive(jsony::Jsony)]\n#[jsony(FromJson)]\nstruct Probe { a: u32, bad: NoFromJson }",
        ),
        vec![Expect::within("FromJson", "bad: NoFromJson")],
    ));

    // 2. Field type missing ToJson. The primary error points at the field (the
    //    forbid would otherwise also pass), but the generated `<Ty as
    //    ToJson>::encode_json__jsony(..)` call makes rustc emit a *second*
    //    obligation that lands on the derive path. This second error is inherent
    //    to the qualified-path codegen: hand-written `<Ty as Trait>::m(..)`
    //    double-reports too. Removing it needs a per-field-type helper fn (added
    //    compile cost), so this is a deferred-by-cost gap rather than a fix.
    out.push(
        case(
            "field_missing_tojson",
            src(
                "struct NoToJson;\n",
                "#[derive(jsony::Jsony)]\n#[jsony(ToJson)]\nstruct Probe { a: u32, bad: NoToJson }",
            ),
            vec![Expect::within("ToJson", "bad: NoToJson")],
        )
        .known(
            "qualified-path ToJson codegen yields a second, derive-pointing \
             obligation; primary already points at the field; fix costs a \
             per-field-type helper fn",
        ),
    );

    // 3. Missing FromJson behind a container (`Vec<NoImpl>`). The blame should
    //    still reach the user's type.
    out.push(case(
        "field_missing_nested",
        src(
            "struct NoImpl;\n",
            "#[derive(jsony::Jsony)]\n#[jsony(FromJson)]\nstruct Probe { a: u32, bad: Vec<NoImpl> }",
        ),
        vec![Expect::within("FromJson", "Vec<NoImpl>")],
    ));

    // 4. Missing FromJson on an enum tuple-variant field.
    out.push(case(
        "enum_variant_field_missing",
        src(
            "struct NoImpl;\n",
            "#[derive(jsony::Jsony)]\n#[jsony(FromJson)]\nenum Probe { A(u32), B(NoImpl) }",
        ),
        vec![Expect::within("FromJson", "B(NoImpl)")],
    ));

    // 5. Unknown container attribute. The macro owns this span, so require it
    //    exactly on the offending token.
    out.push(case(
        "unknown_container_attr",
        src(
            "",
            "#[derive(jsony::Jsony)]\n#[jsony(FromJson, bogus_attr)]\nstruct Probe { a: u32 }",
        ),
        vec![Expect::exact("Unknown attribute", "bogus_attr")],
    ));

    // 6. Unknown field attribute.
    out.push(case(
        "unknown_field_attr",
        src(
            "",
            "#[derive(jsony::Jsony)]\n#[jsony(FromJson)]\nstruct Probe { #[jsony(nonsense)] a: u32 }",
        ),
        vec![Expect::exact("Unknown attr field", "nonsense")],
    ));

    // 7. Duplicate tag attribute on an enum.
    out.push(case(
        "duplicate_tag",
        src(
            "",
            "#[derive(jsony::Jsony)]\n#[jsony(FromJson, tag = \"a\", tag = \"b\")]\nenum Probe { A, B }",
        ),
        vec![Expect::within("Duplicate tag", "tag = \"b\"")],
    ));

    // 8. Non-string tag literal. The macro owns this span and points it exactly
    //    at the bad literal. (Currently good.)
    out.push(case(
        "tag_not_string",
        src(
            "",
            "#[derive(jsony::Jsony)]\n#[jsony(FromJson, tag = 5.5)]\nenum Probe { A, B }",
        ),
        vec![Expect::exact("Expected a string literal", "5.5")],
    ));

    // 8b. Tag value that is not a literal at all (`tag = true`). The ideal error
    //     points at the offending value `true`; the macro currently reports
    //     "Expected a tag" with the span on the `tag` keyword instead, so this
    //     case is expected to FAIL until the span is moved onto the value.
    out.push(case(
        "tag_value_not_literal",
        src(
            "",
            "#[derive(jsony::Jsony)]\n#[jsony(FromJson, tag = true)]\nenum Probe { A, B }",
        ),
        vec![Expect::within("string literal", "true")],
    ));

    // 9. `with` and `validate` on the same field.
    out.push(case(
        "with_validate_conflict",
        src(
            "",
            "#[derive(jsony::Jsony)]\n#[jsony(FromJson)]\nstruct Probe { #[jsony(with = thing, validate = check)] a: u32 }",
        ),
        vec![Expect::within("with", "validate = check")],
    ));

    // 10. A user closure in `skip_if` referencing an undefined variable. rustc
    //     owns this span; it should land on the user's token, proving the macro
    //     forwards closure-body spans rather than collapsing them to call_site.
    out.push(case(
        "skip_if_unknown_var",
        src(
            "",
            "#[derive(jsony::Jsony)]\n#[jsony(ToJson)]\nstruct Probe { #[jsony(skip_if = |v| undefined_var > 10)] a: u32 }",
        ),
        vec![Expect::within("cannot find value", "undefined_var")],
    ));

    // 11. `object!` value whose type lacks ToJson. The ideal error blames the
    //     value `NoToJson` and mentions the ToJson trait; the macro currently
    //     blames the whole `jsony::object! { ... }` invocation and surfaces the
    //     internal method name `encode_json__jsony`, so this is expected to FAIL
    //     until the value's span is forwarded.
    out.push(tcase(
        "object_value_missing_tojson",
        src_in_main(
            "struct NoToJson;\n",
            "    let _ = jsony::object! { \"a\": NoToJson };",
        ),
        vec![Expect::within("encode_json__jsony", ": NoToJson")],
    ));

    // 12. `array!` element whose type lacks ToJson. Same gap as 11.
    out.push(tcase(
        "array_element_missing_tojson",
        src_in_main(
            "struct NoToJson;\n",
            "    let _ = jsony::array! [ 1u32, NoToJson ];",
        ),
        vec![Expect::within("encode_json__jsony", ", NoToJson")],
    ));

    // 13. `object!` value referencing an undefined name. rustc resolves it in the
    //     user's context, so the span lands on the value token. (Currently good,
    //     and a useful contrast with 11: the macro forwards value-expression
    //     spans, it is specifically the trait-resolution span that collapses.)
    out.push(tcase(
        "object_unknown_value",
        src_in_main("", "    let _ = jsony::object! { \"a\": missing_value };"),
        vec![Expect::within("cannot find value", "missing_value")],
    ));

    out
}

// --- Generative missing-impl family ---
//
// Scales the central invariant ("a field whose type lacks the trait is blamed
// at the field, never at the derive") across many field counts and positions.
// Each case is a struct of well-formed fields plus one field of a unique type
// with no jsony impls, placed at a position determined by the id.

const GOOD_TYPES: &[&str] = &[
    "u8",
    "u16",
    "u32",
    "u64",
    "i32",
    "i64",
    "f32",
    "f64",
    "bool",
    "String",
    "Vec<u32>",
    "Option<String>",
];

/// Build the deterministic missing-impl case for `id`.
pub fn missing_impl_case(id: u64) -> DiagCase {
    let mut rng = StdRng::seed_from_u64(mix64(id ^ 0xD1A6_0000_0000_0001));
    let nfields = rng.gen_range(1..6);
    let bad_at = rng.gen_range(0..nfields);
    let bad_ty = format!("NoImpl{id}");
    let bad_field = format!("bad{id}");

    let mut fields = String::new();
    let mut needle = String::new();
    for i in 0..nfields {
        if i == bad_at {
            let frag = format!("{bad_field}: {bad_ty}");
            needle = frag.clone();
            let _ = writeln!(fields, "    {frag},");
        } else {
            let ty = GOOD_TYPES[rng.gen_range(0..GOOD_TYPES.len())];
            let _ = writeln!(fields, "    f{i}: {ty},");
        }
    }

    let header = format!("struct {bad_ty};\n");
    let derive =
        format!("#[derive(jsony::Jsony)]\n#[jsony(FromJson)]\nstruct Probe{id} {{\n{fields}}}");
    let source = src(&header, &derive);

    // `needle` is `bad{id}: NoImpl{id}`, unique in the source.
    DiagCase {
        name: format!("missing_impl#{id}"),
        source,
        expects: vec![Expect::within("FromJson", &needle)],
        forbid: vec![Forbid::derive_path()],
        known_gap: None,
    }
}

/// All cases for a run: the curated catalog plus `gen_count` generated
/// missing-impl cases starting at `gen_start`.
pub fn all_cases(gen_start: u64, gen_count: u64) -> Vec<DiagCase> {
    let mut cases = catalog();
    for id in gen_start..gen_start + gen_count {
        cases.push(missing_impl_case(id));
    }
    cases
}
