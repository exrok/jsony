//! Orchestration: generate cases by id, emit batch sources + runtime input
//! files, then compile + run batches in parallel via direct `rustc`. Work is
//! streamed in waves so memory and on-disk files stay bounded regardless of the
//! total case count (scales to ~1M).

use std::path::PathBuf;

use anyhow::{Context, Result};
use rayon::prelude::*;

use crate::compile::{Toolchain, ToolchainSpec, ASAN, THROUGHPUT};
use crate::emit::{emit_batch, FIELD_SEP, KIND_BAD, KIND_OK};
use crate::gen::{case_from_id, sample_json, Case};

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
/// well-formed inputs, plus truncated copies of the first few as `BAD` inputs to
/// drive the partial-initialization drop paths. Deterministic per (id, index).
fn sample_inputs(cases: &[Case], samples: u32, bad: bool) -> (String, u64) {
    let mut content = String::new();
    let mut count = 0u64;
    let record = |content: &mut String, name: &str, kind: char, json: &str| {
        content.push_str(name);
        content.push(FIELD_SEP);
        content.push(kind);
        content.push(FIELD_SEP);
        content.push_str(json);
        content.push('\n');
    };
    // In the soundness tier the first few well-formed inputs per case are also
    // truncated into malformed records to drive the partial-init drop paths.
    let bad_seeds = if bad { samples.min(4) } else { 0 };
    for case in cases {
        for k in 0..samples {
            let seed = case.id ^ (k as u64).wrapping_mul(0x9E3779B97F4A7C15);
            let json = sample_json(case, seed);
            record(&mut content, &case.name, KIND_OK, &json);
            count += 1;
            if k < bad_seeds {
                for cut in truncations(&json) {
                    record(&mut content, &case.name, KIND_BAD, cut);
                    count += 1;
                }
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

/// Quick tier: a modest fixed round-trip smoke (well-formed inputs only).
pub fn quick() -> Result<Report> {
    run(0..200, 50, 20, 16, false, THROUGHPUT)
}

/// Round-trip sweep on the throughput toolchain. `bad` adds the malformed inputs.
pub fn sweep(count: u64, samples: u32, batch: u64, bad: bool) -> Result<Report> {
    run(0..count, batch, samples, 16, bad, THROUGHPUT)
}

/// ASAN + LSAN tier: malformed-heavy inputs on the sanitizer toolchain. Smaller
/// waves since sanitized batches compile and run slower.
pub fn asan(count: u64, samples: u32, batch: u64) -> Result<Report> {
    run(0..count, batch, samples, 8, true, ASAN)
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
