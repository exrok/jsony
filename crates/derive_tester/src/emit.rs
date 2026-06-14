//! Emit a self-checking batch source file from `Case`s.
//!
//! Each batch contains only type definitions + a `name -> oracle` dispatcher.
//! JSON inputs are loaded at runtime (file arg or stdin), one `name<TAB>json`
//! record per line, so a single compiled binary checks many input samples.

use std::fmt::Write;

use proc_macro2::TokenStream;

use crate::gen::{Body, Case, FieldSpec, VarKind};
use crate::schema::Type;

/// Record separator in the runtime input file: `name<TAB>kind<TAB>json`.
pub(crate) const FIELD_SEP: char = '\t';

/// Input kinds in the record file. `OK` inputs must round-trip, `BAD` inputs
/// (truncated or malformed) must fail without panicking or leaking, `EQ` inputs
/// carry a second JSON in a fourth column and must decode equal to the first.
pub(crate) const KIND_OK: char = 'o';
pub(crate) const KIND_BAD: char = 'b';
pub(crate) const KIND_EQ: char = 'e';

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

/// Render a schema `Type` to its Rust source spelling (e.g. `Vec < u32 >`).
fn type_str(ty: Type) -> String {
    let mut tt = Vec::new();
    Type::gen(&ty, &mut tt);
    TokenStream::from_iter(tt).to_string()
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
    if f.default {
        parts.push("default".to_string());
    }
    if parts.is_empty() {
        String::new()
    } else {
        format!("#[jsony({})] ", parts.join(", "))
    }
}

fn emit_def(out: &mut String, case: &Case) {
    let name = &case.name;
    let _ = writeln!(out, "#[derive(jsony::Jsony, PartialEq, Debug)]");
    let _ = writeln!(out, "#[jsony({})]", case.container_attr());
    // transparent FromJson requires repr(transparent).
    if case.transparent {
        let _ = writeln!(out, "#[repr(transparent)]");
    }
    match &case.body {
        Body::Named(fields) => {
            let _ = writeln!(out, "struct {name} {{");
            for f in fields {
                let _ = writeln!(out, "    {}{}: {},", field_attr(f), f.name, type_str(f.ty));
            }
            let _ = writeln!(out, "}}");
        }
        Body::Tuple(fields) => {
            let _ = write!(out, "struct {name}(");
            for f in fields {
                let _ = write!(out, "{}, ", type_str(f.ty));
            }
            let _ = writeln!(out, ");");
        }
        Body::Unit => {
            let _ = writeln!(out, "struct {name};");
        }
        Body::Enum { variants, .. } => {
            let _ = writeln!(out, "enum {name} {{");
            for v in variants {
                match v.kind {
                    VarKind::Unit => {
                        let _ = writeln!(out, "    {},", v.name);
                    }
                    VarKind::Tuple => {
                        let _ = writeln!(out, "    {}({}),", v.name, type_str(v.fields[0].ty));
                    }
                    VarKind::Struct => {
                        let _ = writeln!(out, "    {} {{", v.name);
                        for f in &v.fields {
                            let _ = writeln!(
                                out,
                                "        {}{}: {},",
                                field_attr(f),
                                f.name,
                                type_str(f.ty)
                            );
                        }
                        let _ = writeln!(out, "    }},");
                    }
                }
            }
            let _ = writeln!(out, "}}");
        }
    }
}

fn emit_arm(out: &mut String, case: &Case) {
    let name = &case.name;
    let can_decode = case.traits.from_json;
    let can_encode = case.traits.to_json;
    let bin = case.traits.binary();
    let _ = writeln!(out, "        {name:?} => {{");
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
            "                \"{KIND_BAD}\" => {{ match jsony::from_json::<{name}>(input) {{ Ok(v) => drop(v), Err(e) => drop(e) }} }}"
        );
        // Equivalence path: `input` and `extra` are two encodings of the same
        // value (key-permuted or whitespace-injected). Both must decode and
        // compare equal. This probes the decoder's insensitivity to key order
        // and insignificant whitespace independently of the round-trip oracle,
        // which cannot catch a decode that is self-consistently wrong.
        let _ = writeln!(out, "                \"{KIND_EQ}\" => {{");
        let _ = writeln!(
            out,
            "                    let a: {name} = match jsony::from_json(input) {{ Ok(v) => v, Err(e) => return Err(format!(\"equiv a from_json: {{e}} a={{input}}\")) }};"
        );
        let _ = writeln!(
            out,
            "                    let b: {name} = match jsony::from_json(extra) {{ Ok(v) => v, Err(e) => return Err(format!(\"equiv b from_json: {{e}} b={{extra}}\")) }};"
        );
        let _ = writeln!(
            out,
            "                    if a != b {{ return Err(format!(\"equivalence mismatch: {{a:?}} != {{b:?}} a={{input}} b={{extra}}\")); }}"
        );
        let _ = writeln!(out, "                }}");
    }

    // Positive path. Each binding lives inside this arm's scope so it drops
    // before the leak check below.
    let _ = writeln!(out, "                _ => {{");
    if can_decode {
        let _ = writeln!(
            out,
            "                    let v1: {name} = match jsony::from_json(input) {{ Ok(v) => v, Err(e) => return Err(format!(\"from_json: {{e}} json={{input}}\")) }};"
        );
        if can_encode {
            let _ = writeln!(out, "                    let s = jsony::to_json(&v1);");
            let _ = writeln!(
                out,
                "                    let v2: {name} = match jsony::from_json(&s) {{ Ok(v) => v, Err(e) => return Err(format!(\"reparse: {{e}} json={{s}}\")) }};"
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
                "                    let v3: {name} = match jsony::from_binary(&bytes) {{ Ok(v) => v, Err(_) => return Err(format!(\"from_binary failed (json={{input}})\")) }};"
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

/// Build the full batch source for the given cases.
pub(crate) fn emit_batch(cases: &[Case]) -> String {
    let mut out = String::new();
    out.push_str("#![allow(dead_code, non_snake_case, non_camel_case_types, unused)]\n");
    out.push_str("use std::io::Read;\n");
    out.push_str(ALLOCATOR);
    out.push('\n');

    for case in cases {
        emit_def(&mut out, case);
        out.push('\n');
    }

    out.push_str(
        "fn run_case(name: &str, kind: &str, input: &str, extra: &str) -> Result<(), String> {\n",
    );
    out.push_str("    match name {\n");
    for case in cases {
        emit_arm(&mut out, case);
    }
    out.push_str("        _ => Err(format!(\"unknown type {name}\")),\n");
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
    out.push_str("        let name = cols.next().expect(\"name\");\n");
    out.push_str("        let kind = cols.next().expect(\"kind\");\n");
    out.push_str("        let input = cols.next().unwrap_or(\"\");\n");
    out.push_str("        let extra = cols.next().unwrap_or(\"\");\n");
    out.push_str("        total += 1;\n");
    out.push_str(
        "        if let Err(e) = run_case(name, kind, input, extra) { eprintln!(\"FAIL {name}: {e}\"); fails += 1; }\n",
    );
    out.push_str("    }\n");
    out.push_str(
        "    if fails > 0 { eprintln!(\"{fails}/{total} FAILURES\"); std::process::exit(1); }\n",
    );
    out.push_str("    println!(\"OK {total} cases\");\n");
    out.push_str("}\n");
    out
}
