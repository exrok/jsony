//! Orchestration: generate cases by id, emit batch sources + runtime input
//! files, then compile + run batches in parallel via direct `rustc`. Work is
//! streamed in waves so memory and on-disk files stay bounded regardless of the
//! total case count (scales to ~1M).

use std::path::PathBuf;

use anyhow::{Context, Result};
use rayon::prelude::*;

use crate::compile::Toolchain;
use crate::emit::{emit_batch, FIELD_SEP};
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

/// Build the runtime input file content for a batch: `samples` JSON inputs per
/// case, each `name<SEP>json` on its own line. Deterministic per (id, index).
fn sample_inputs(cases: &[Case], samples: u32) -> (String, u64) {
    let mut content = String::new();
    let mut count = 0u64;
    for case in cases {
        for k in 0..samples {
            let seed = case.id ^ (k as u64).wrapping_mul(0x9E3779B97F4A7C15);
            let json = sample_json(case, seed);
            content.push_str(&case.name);
            content.push(FIELD_SEP);
            content.push_str(&json);
            content.push('\n');
            count += 1;
        }
    }
    (content, count)
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
) -> Result<Report> {
    let work = PathBuf::from("/tmp/derive_tester_work");
    std::fs::create_dir_all(&work)?;
    let tc = Toolchain::prepare(&jsony_root()?, work.clone()).context("preparing toolchain")?;

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
            let (input, n) = sample_inputs(&cases, samples);
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

/// Quick tier: a modest fixed run.
pub fn quick() -> Result<Report> {
    run(0..200, 50, 20, 16)
}
