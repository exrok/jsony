//! Orchestration: generate cases by id, emit batch sources + runtime input
//! files, then compile + run batches in parallel via direct `rustc`. Work is
//! streamed in waves so memory and on-disk files stay bounded regardless of the
//! total case count (scales to ~1M).

use std::path::PathBuf;

use anyhow::{Context, Result};
use rayon::prelude::*;

use crate::compile::{Toolchain, ToolchainSpec, ASAN, THROUGHPUT};
use crate::diag::{self, check_case};
use crate::emit::{emit_batch, FIELD_SEP, KIND_BAD, KIND_ENCODE, KIND_EQ, KIND_OK, KIND_REJECT};
use crate::gen::{case_from_id, sample, sample_json, Body, Case};

/// Cap on stored failure messages (all failures are still counted).
const MAX_FAILURE_SAMPLES: usize = 50;

#[derive(Default)]
pub struct Report {
    pub total_types: u64,
    pub total_inputs: u64,
    pub total_batches: u64,
    pub failed_batches: u64,
    pub failures: Vec<String>,
}

/// Absolute path to the jsony repo root (two levels up from this crate).
fn jsony_root() -> Result<String> {
    let manifest = PathBuf::from(env!("CARGO_MANIFEST_DIR"));
    let root = manifest
        .parent()
        .and_then(|p| p.parent())
        .context("locating jsony root")?;
    Ok(root.to_string_lossy().into_owned())
}

/// Build the runtime input file content for a batch. Per case: `samples`
/// well-formed inputs. For the first few samples it also emits equivalence
/// inputs (key-permuted and whitespace-injected, which must decode equal) and,
/// in the soundness tier, duplicate-key/truncated/malformed inputs that must
/// fail cleanly. Every record is deterministic per (id, index).
fn sample_inputs(cases: &[Case], samples: u32, bad: bool) -> (String, u64) {
    let mut content = String::new();
    let mut count = 0u64;
    let record = |content: &mut String, name: &str, kind: char, json: &str, extra: &str| {
        content.push_str(name);
        content.push(FIELD_SEP);
        content.push(kind);
        content.push(FIELD_SEP);
        content.push_str(json);
        if !extra.is_empty() {
            content.push(FIELD_SEP);
            content.push_str(extra);
        }
        content.push('\n');
    };
    // Equivalence inputs run in every tier (cheap, decode-only). The soundness
    // tier adds the duplicate-key/truncated/malformed inputs that drive the
    // partial-init drop paths.
    let eq_seeds = samples.min(4);
    let bad_seeds = if bad { samples.min(4) } else { 0 };
    for case in cases {
        // A Str enum has no JSON form. One trigger record fires its self-contained
        // `ToStr`/`FromStr` oracle arm, which checks every variant internally.
        if let Body::Str(_) = &case.body {
            record(&mut content, &case.name, KIND_OK, "str", "");
            count += 1;
            continue;
        }
        // A version family is binary-only and self-contained. One trigger record
        // fires its cross-version oracle arm.
        if let Body::Version(_) = &case.body {
            record(&mut content, &case.name, KIND_OK, "ver", "");
            count += 1;
            continue;
        }
        // A POD family is binary-only and self-contained. One trigger record fires
        // its alignment oracle arm.
        if let Body::Pod(_) = &case.body {
            record(&mut content, &case.name, KIND_OK, "pod", "");
            count += 1;
            continue;
        }
        // A via=Iterator family is ToJson-only and self-contained. One trigger
        // record fires its encode oracle arm.
        if let Body::ViaIter(_) = &case.body {
            record(&mut content, &case.name, KIND_OK, "via", "");
            count += 1;
            continue;
        }
        // A with-helper / binary-rejection family is self-contained. One trigger
        // record fires its fixed oracle arm.
        if let Body::Helper(_) = &case.body {
            record(&mut content, &case.name, KIND_OK, "helper", "");
            count += 1;
            continue;
        }
        // A ToJson-only case (none generated yet) has no decoder, so the derived
        // decode-oriented inputs would never run; emit only canonical inputs.
        let derive_inputs = case.traits.from_json;
        for k in 0..samples {
            let seed = case.id ^ (k as u64).wrapping_mul(0x9E3779B97F4A7C15);
            if derive_inputs && (k < eq_seeds || k < bad_seeds) {
                let s = sample(case, seed);
                record(&mut content, &case.name, KIND_OK, &s.canonical, "");
                count += 1;
                if k < eq_seeds {
                    // Absolute encode oracle (once per case): a full canonical
                    // input must re-encode to itself byte-for-byte.
                    if k == 0 {
                        if let Some(enc) = crate::gen::sample_encode(case, seed) {
                            record(&mut content, &case.name, KIND_ENCODE, &enc, "");
                            count += 1;
                        }
                        // Absolute `skip_if` encode checks (two-column): a present
                        // non-trigger value is kept, a trigger value re-encodes
                        // omitted. `expected` (col4) differs from `input` (col3) in
                        // the omitted case.
                        for (input, expected) in crate::gen::build_skip_if_encode(case, seed)
                            .into_iter()
                            .flatten()
                        {
                            record(&mut content, &case.name, KIND_ENCODE, &input, &expected);
                            count += 1;
                        }
                    }
                    // Skip records that are byte-identical to the canonical input
                    // (shapes with no reorderable object, e.g. newtypes).
                    if s.permuted != s.canonical {
                        record(&mut content, &case.name, KIND_EQ, &s.canonical, &s.permuted);
                        count += 1;
                    }
                    if s.spaced != s.canonical {
                        record(&mut content, &case.name, KIND_EQ, &s.canonical, &s.spaced);
                        count += 1;
                    }
                    // Absolute default-value check: omitting a `default = N` field
                    // must decode equal to setting it to N.
                    if let Some((present, omitted)) = &s.default_eq {
                        record(&mut content, &case.name, KIND_EQ, present, omitted);
                        count += 1;
                    }
                    // `ignore_tag_adjacent_fields`: with the flag, injected extra
                    // keys must be ignored, so the injected input decodes equal to
                    // the canonical one. (The no-flag negative is emitted below as
                    // a BAD input in the soundness tier.)
                    if case.ignore_tag_adjacent {
                        if let Some(adj) = &s.adjacent {
                            record(&mut content, &case.name, KIND_EQ, &s.canonical, adj);
                            count += 1;
                        }
                    }
                    // A rejecting validator must abort the decode when its field
                    // holds the sentinel. This is a strict must-error assertion
                    // (KIND_REJECT), run in every tier since it is decode-only.
                    if let Some(reject) = &s.validate_reject {
                        record(&mut content, &case.name, KIND_REJECT, reject, "");
                        count += 1;
                    }
                    // `other` catch-all: an unknown tag must decode to the same
                    // value as the other variant's own (known) tag.
                    if let Some((known, unknown)) = &s.other_eq {
                        record(&mut content, &case.name, KIND_EQ, known, unknown);
                        count += 1;
                    }
                }
                if k < bad_seeds {
                    if let Some(dup) = &s.dup {
                        record(&mut content, &case.name, KIND_BAD, dup, "");
                        count += 1;
                    }
                    // An externally-tagged enum without `ignore_tag_adjacent_fields`
                    // must reject an extra key adjacent to the tag. The error fires
                    // after the variant content (possibly heap-owning) is built, so
                    // it also drives the partial-init drop path for the leak check.
                    if !case.ignore_tag_adjacent {
                        if let Some(adj) = &s.adjacent {
                            record(&mut content, &case.name, KIND_BAD, adj, "");
                            count += 1;
                        }
                    }
                    for cut in truncations(&s.canonical) {
                        record(&mut content, &case.name, KIND_BAD, cut, "");
                        count += 1;
                    }
                    for bad in malformations(&s.canonical) {
                        record(&mut content, &case.name, KIND_BAD, &bad, "");
                        count += 1;
                    }
                }
            } else {
                let json = sample_json(case, seed);
                record(&mut content, &case.name, KIND_OK, &json, "");
                count += 1;
            }
        }
    }
    (content, count)
}

/// Char-boundary-safe prefixes of a JSON string, used as truncated (and thus
/// usually invalid) inputs. jsony escapes control bytes inside strings, so a
/// prefix never contains a raw separator and stays one record on one line.
fn truncations(json: &str) -> Vec<&str> {
    let n = json.len();
    if n < 2 {
        return Vec::new();
    }
    let mut cuts: Vec<&str> = Vec::new();
    for frac in [n / 4, n / 2, (3 * n) / 4, n - 1] {
        let mut i = frac;
        while i > 0 && !json.is_char_boundary(i) {
            i -= 1;
        }
        if i > 0 && i < n {
            cuts.push(&json[..i]);
        }
    }
    cuts.sort_unstable();
    cuts.dedup();
    cuts
}

/// Deterministic malformed variants of a well-formed JSON string, used as `b`
/// inputs: each must fail (or parse) without panicking or leaking. Unlike a
/// prefix, these keep a complete-looking head so earlier heap fields are live
/// when the parse diverges, exercising the partial-init drop paths from a
/// different angle than truncation. Covers the doc's mutation classes:
/// type-mismatch, structural break, trailing garbage, and deep nesting. None of
/// the transforms introduce a tab or newline, so each stays a single record.
fn malformations(json: &str) -> Vec<String> {
    let mut out = Vec::new();
    // Trailing garbage after a complete value, and a spurious extra closer.
    out.push(format!("{json} trailing_garbage"));
    out.push(format!("{json}]"));

    // Type-mismatch: turn a late digit into a letter so a numeric field fails to
    // parse after the fields before it are already initialized.
    if let Some(i) = late_index(json, |c| c.is_ascii_digit()) {
        let mut s = json.to_string();
        s.replace_range(i..i + 1, "x");
        out.push(s);
    }
    // Structural break: flip a late ':' to ',' (or ',' to ':'), corrupting the
    // object shape past the leading, already-decoded fields.
    if let Some(i) = late_index(json, |c| c == ':' || c == ',') {
        let mut s = json.to_string();
        let repl = if &json[i..i + 1] == ":" { "," } else { ":" };
        s.replace_range(i..i + 1, repl);
        out.push(s);
    }
    // A deeply nested prefix: the decoder enters the array then fails a few
    // levels in (the generated types are not deeply recursive), checking the
    // array-entry path stays panic-free on unexpected nesting.
    out.push(format!("{}{}", "[".repeat(300), json));

    out.retain(|s| !s.is_empty());
    out.sort_unstable();
    out.dedup();
    out
}

/// Byte index of the last ASCII char in the back three-quarters of `json`
/// matching `pred`. Restricting to the tail keeps leading fields decoded before
/// the corruption point. ASCII-only, so the index is always a char boundary.
fn late_index(json: &str, pred: impl Fn(char) -> bool) -> Option<usize> {
    let start = json.len() / 4;
    json.bytes()
        .enumerate()
        .skip(start)
        .rev()
        .find(|(_, b)| b.is_ascii() && pred(*b as char))
        .map(|(i, _)| i)
}

/// Compile and run one batch (given its source + input file content). `slot` is
/// reused across waves so the temp-file set stays bounded.
fn run_batch(
    tc: &Toolchain,
    work: &std::path::Path,
    slot: usize,
    src: &str,
    input: &str,
) -> Result<(), String> {
    let tag = format!("b{slot}");
    let input_path = work.join(format!("{tag}.input"));
    std::fs::write(&input_path, input).map_err(|e| format!("write {tag}.input: {e}"))?;
    let bin = tc.compile(&tag, src)?;
    let out = tc.run(&bin, &input_path)?;
    if out.status.success() {
        Ok(())
    } else {
        Err(format!(
            "[ids in {tag}] exit {:?}\n{}",
            out.status.code(),
            String::from_utf8_lossy(&out.stderr)
        ))
    }
}

/// Generate, compile, and run `ids` worth of cases in batches of `batch_size`,
/// `samples` JSON inputs per type. Batches are processed `wave_batches` at a
/// time across all cores.
pub fn run(
    ids: std::ops::Range<u64>,
    batch_size: u64,
    samples: u32,
    wave_batches: usize,
    bad: bool,
    spec: ToolchainSpec,
) -> Result<Report> {
    let work = PathBuf::from("/tmp/derive_tester_work");
    std::fs::create_dir_all(&work)?;
    let tc =
        Toolchain::prepare(&jsony_root()?, work.clone(), spec).context("preparing toolchain")?;

    let wave_types = batch_size * wave_batches as u64;
    let mut report = Report::default();
    let mut start = ids.start;

    while start < ids.end {
        let wave_end = (start + wave_types).min(ids.end);

        // Build this wave's batch sources + inputs single-threaded.
        let mut wave: Vec<(String, String)> = Vec::new();
        let mut id = start;
        while id < wave_end {
            let bend = (id + batch_size).min(wave_end);
            let cases: Vec<Case> = (id..bend).map(case_from_id).collect();
            let src = emit_batch(&cases);
            let (input, n) = sample_inputs(&cases, samples, bad);
            report.total_types += cases.len() as u64;
            report.total_inputs += n;
            wave.push((src, input));
            id = bend;
        }
        report.total_batches += wave.len() as u64;

        // Compile + run the wave in parallel (slot index == wave position).
        let outcomes: Vec<Result<(), String>> = wave
            .par_iter()
            .enumerate()
            .map(|(slot, (src, input))| run_batch(&tc, &work, slot, src, input))
            .collect();

        for r in outcomes {
            if let Err(e) = r {
                report.failed_batches += 1;
                if report.failures.len() < MAX_FAILURE_SAMPLES {
                    report.failures.push(e);
                }
            }
        }

        start = wave_end;
    }

    Ok(report)
}

/// Resolve the id band a sweep covers. `seed` is the band start: `Some(s)`
/// reproduces a prior run, `None` (the default) draws a fresh random band from
/// OS entropy so repeated runs explore new cases instead of re-testing the same
/// ids. The start is clamped so `start + count` cannot overflow. The chosen band
/// is announced, and any failure prints `T<id>`, reproducible with `case <id>`.
fn band(seed: Option<u64>, count: u64) -> std::ops::Range<u64> {
    let start = match seed {
        Some(s) => s,
        None => rand::random::<u64>() % (u64::MAX - count.max(1)),
    };
    eprintln!(
        "derive_tester band: ids {start}..{} ({count} types). Reproduce this band with \
         DT_SEED={start}; reproduce one failing case with `case <id>`.",
        start + count
    );
    start..start + count
}

/// Quick tier: a modest fixed round-trip smoke (well-formed + equivalence
/// inputs). Fixed band so it is a stable CI gate.
pub fn quick() -> Result<Report> {
    run(0..200, 50, 20, 16, false, THROUGHPUT)
}

/// Round-trip sweep on the throughput toolchain. `bad` adds the malformed inputs
/// and the leak check. `seed` pins the id band (see [`band`]).
pub fn sweep(count: u64, samples: u32, batch: u64, bad: bool, seed: Option<u64>) -> Result<Report> {
    run(band(seed, count), batch, samples, 16, bad, THROUGHPUT)
}

/// ASAN + LSAN tier on the sanitizer toolchain. It runs the full soundness check
/// set (round-trip, equivalence, duplicate-key, and the counting-allocator leak
/// check, since that allocator is compiled into every batch) with AddressSanitizer
/// and LeakSanitizer layered on top, so it is "sound + ASAN" in one pass. Smaller
/// waves since sanitized batches compile and run slower. `seed` pins the id band.
pub fn asan(count: u64, samples: u32, batch: u64, seed: Option<u64>) -> Result<Report> {
    run(band(seed, count), batch, samples, 8, true, ASAN)
}

// --- Diagnostics tier ---

/// Cap on rendered failure reports kept in a diagnostics run (all are counted).
const MAX_DIAG_REPORTS: usize = 200;

#[derive(Default)]
pub struct DiagReport {
    pub total: usize,
    pub passed: usize,
    /// Rendered reports for genuinely failing cases (capped at
    /// [`MAX_DIAG_REPORTS`]). Only these make the run exit nonzero.
    pub failures: Vec<String>,
    /// One-line notes for known-gap cases that still fail (expected, deferred).
    pub deferred: Vec<String>,
    /// Names of known-gap cases that now pass, so their marker can be removed.
    pub fixed: Vec<String>,
}

/// Diagnostics tier: compile the curated catalog plus `gen_count` generated
/// missing-impl cases, each with `--error-format=json`, and assert every error's
/// message and span. Cases compile independently and in parallel against the
/// shared pre-built dependency set. `seed` pins the generated id band the way
/// [`band`] does for the sweep.
pub fn diag(gen_count: u64, seed: Option<u64>) -> Result<DiagReport> {
    let work = PathBuf::from("/tmp/derive_tester_work");
    std::fs::create_dir_all(&work)?;
    let tc = Toolchain::prepare(&jsony_root()?, work, THROUGHPUT).context("preparing toolchain")?;

    let gen_start = match seed {
        Some(s) => s,
        None => rand::random::<u64>() % (u64::MAX - gen_count.max(1)),
    };
    if gen_count > 0 {
        eprintln!(
            "derive_tester diag band: generated ids {gen_start}..{} ({gen_count} cases). \
             Reproduce with DT_SEED={gen_start}.",
            gen_start + gen_count
        );
    }
    let cases = diag::all_cases(gen_start, gen_count);

    let outcomes: Vec<crate::diag::Outcome> = cases
        .par_iter()
        .enumerate()
        .map(|(i, c)| check_case(&tc, &format!("d{i}"), c))
        .collect();

    let mut report = DiagReport {
        total: outcomes.len(),
        ..Default::default()
    };
    for (c, o) in cases.iter().zip(&outcomes) {
        match (o.passed(), c.known_gap) {
            (true, None) => report.passed += 1,
            (true, Some(_)) => report.fixed.push(c.name.clone()),
            (false, Some(reason)) => report.deferred.push(format!("{}: {reason}", c.name)),
            (false, None) => {
                if report.failures.len() < MAX_DIAG_REPORTS {
                    report.failures.push(o.render());
                }
            }
        }
    }
    Ok(report)
}

// --- Direct macro-source driving (coverage) ---

#[derive(Default)]
pub struct ExpandReport {
    /// Cases generated (one per id in the band).
    pub types: u64,
    /// `#[derive(Jsony)]` items handed to the derive (a case can emit several).
    pub items: u64,
    /// Items that expanded to a `compile_error!`. Must be zero: a well-formed
    /// generated type the derive rejects is a bug in the harness or the codegen.
    pub errors: u64,
    /// Total byte length of the generated token streams, summed to keep the
    /// optimizer from eliding the expansion.
    pub output_bytes: u64,
}

/// Drive `jsony_macros_source`'s derive directly, in-process, over `count`
/// generated cases. No compiler subprocess: each case's type definition is built
/// as source, tokenized, and expanded by [`jsony_macros_source::expand_str`]. Run
/// this binary under `cargo +nightly llvm-cov` to measure which codegen paths the
/// generated types reach. `seed` pins the id band exactly like [`sweep`].
pub fn expand(count: u64, seed: Option<u64>) -> Result<ExpandReport> {
    let ids = band(seed, count);
    let (items, errors, output_bytes) = (ids.start..ids.end)
        .into_par_iter()
        .map(|id| {
            let case = case_from_id(id);
            let src = crate::emit::case_source(&case);
            let stats = jsony_macros_source::expand_str(&src);
            (stats.items, stats.errors, stats.output_bytes)
        })
        .reduce(
            || (0u64, 0u64, 0u64),
            |a, b| (a.0 + b.0, a.1 + b.1, a.2 + b.2),
        );
    Ok(ExpandReport {
        types: count,
        items,
        errors,
        output_bytes,
    })
}

/// Emit the standalone source for a single case id, plus its sampled inputs as a
/// trailing comment block, for inspection and reproduction. Includes the
/// truncated soundness inputs.
pub fn emit_case_source(id: u64, samples: u32) -> String {
    let case = case_from_id(id);
    let mut src = emit_batch(std::slice::from_ref(&case));
    let (inputs, _) = sample_inputs(std::slice::from_ref(&case), samples, true);
    src.push_str("\n/* sampled inputs (name<TAB>kind<TAB>json):\n");
    src.push_str(&inputs);
    src.push_str("*/\n");
    src
}
