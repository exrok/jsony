//! Emit a self-checking batch source file from `Case`s.
//!
//! Each batch contains only type definitions + a `name -> oracle` dispatcher.
//! JSON inputs are loaded at runtime (file arg or stdin), one `name<TAB>json`
//! record per line, so a single compiled binary checks many input samples.

use std::fmt::Write;

use proc_macro2::TokenStream;

use crate::gen::{Body, Case, FieldSpec, VarKind};
use crate::schema::Type;

/// Record separator in the runtime input file: `name<TAB>json`.
pub(crate) const FIELD_SEP: char = '\t';

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
    let _ = writeln!(out, "        {name:?} => {{");
    let _ = writeln!(
        out,
        "            let v1: {name} = match jsony::from_json(input) {{ Ok(v) => v, Err(e) => return Err(format!(\"from_json: {{e}}\")) }};"
    );
    let _ = writeln!(out, "            let s = jsony::to_json(&v1);");
    let _ = writeln!(
        out,
        "            let v2: {name} = match jsony::from_json(&s) {{ Ok(v) => v, Err(e) => return Err(format!(\"reparse: {{e}} json={{s}}\")) }};"
    );
    let _ = writeln!(
        out,
        "            if v1 != v2 {{ return Err(format!(\"json mismatch: {{v1:?}} != {{v2:?}} json={{s}}\")); }}"
    );
    if case.traits.binary() {
        let _ = writeln!(out, "            let b = jsony::to_binary(&v1);");
        let _ = writeln!(
            out,
            "            let v3: {name} = match jsony::from_binary(&b) {{ Ok(v) => v, Err(_) => return Err(format!(\"from_binary failed (json={{s}})\")) }};"
        );
        let _ = writeln!(
            out,
            "            if v1 != v3 {{ return Err(format!(\"binary mismatch: {{v1:?}} != {{v3:?}} (json={{s}})\")); }}"
        );
    }
    let _ = writeln!(out, "            Ok(())");
    let _ = writeln!(out, "        }}");
}

/// Build the full batch source for the given cases.
pub(crate) fn emit_batch(cases: &[Case]) -> String {
    let mut out = String::new();
    out.push_str("#![allow(dead_code, non_snake_case, non_camel_case_types, unused)]\n");
    out.push_str("use std::io::Read;\n\n");

    for case in cases {
        emit_def(&mut out, case);
        out.push('\n');
    }

    out.push_str("fn run_case(name: &str, input: &str) -> Result<(), String> {\n");
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
        "        let (name, input) = line.split_once('{}').expect(\"name<SEP>json\");",
        FIELD_SEP.escape_default()
    );
    out.push_str("        total += 1;\n");
    out.push_str(
        "        if let Err(e) = run_case(name, input) { eprintln!(\"FAIL {name}: {e}\"); fails += 1; }\n",
    );
    out.push_str("    }\n");
    out.push_str(
        "    if fails > 0 { eprintln!(\"{fails}/{total} FAILURES\"); std::process::exit(1); }\n",
    );
    out.push_str("    println!(\"OK {total} cases\");\n");
    out.push_str("}\n");
    out
}
