use anyhow::{bail, Result};
use derive_tester::run::{self, CorpusReport, DiagReport, ExpandReport, Report};

fn report(r: &Report) {
    println!(
        "derive_tester: {} types, {} inputs, {} batches",
        r.total_types, r.total_inputs, r.total_batches
    );
    if r.failed_batches == 0 {
        println!("ALL PASS");
    } else {
        for f in &r.failures {
            eprintln!("{f}");
        }
        eprintln!(
            "{} batch failure(s){}",
            r.failed_batches,
            if r.failures.len() < r.failed_batches as usize {
                format!(" (showing first {})", r.failures.len())
            } else {
                String::new()
            }
        );
        std::process::exit(1);
    }
}

fn expand_report(r: &ExpandReport) {
    println!(
        "derive_tester expand: {} types -> {} derive items expanded, {} bytes generated",
        r.types, r.items, r.output_bytes
    );
    if r.errors > 0 {
        eprintln!(
            "{} item(s) expanded to compile_error! (harness/codegen bug)",
            r.errors
        );
        std::process::exit(1);
    }
}

fn corpus_report(label: &str, r: &CorpusReport) -> bool {
    println!(
        "derive_tester expand {label}: {}/{} pass, {} bytes generated",
        r.passed, r.total, r.output_bytes
    );
    for f in &r.failures {
        eprintln!("  FAIL {f}");
    }
    if !r.failures.is_empty() {
        eprintln!("{} {label} case failure(s)", r.failures.len());
    }
    r.failures.is_empty()
}

fn diag_report(r: &DiagReport) {
    println!(
        "derive_tester diag: {}/{} pass, {} deferred (known gaps), {} real failure(s)",
        r.passed,
        r.total,
        r.deferred.len(),
        r.failures.len()
    );
    for d in &r.deferred {
        println!("  deferred gap — {d}");
    }
    for name in &r.fixed {
        println!("  known gap now passes, remove its marker: {name}");
    }
    if r.failures.is_empty() {
        println!("ALL PASS (deferred gaps excluded)");
        return;
    }
    for f in &r.failures {
        eprint!("{f}");
    }
    eprintln!("{} real diagnostic failure(s)", r.failures.len());
    std::process::exit(1);
}

fn main() -> Result<()> {
    let args: Vec<String> = std::env::args().collect();
    let mode = args.get(1).map(String::as_str).unwrap_or("quick");

    // `case <id>` emits standalone source and exits before the run/report path.
    if mode == "case" {
        let id: u64 = args.get(2).map(|s| s.parse()).transpose()?.unwrap_or(0);
        let samples: u32 = args.get(3).map(|s| s.parse()).transpose()?.unwrap_or(4);
        print!("{}", run::emit_case_source(id, samples));
        return Ok(());
    }

    // `expand [count]` drives the macro source crate directly, in-process, over
    // `count` generated types. It compiles nothing: run it under
    // `cargo +nightly llvm-cov` to quantify which codegen paths the generated
    // types reach. Reports on its own path. `DT_SEED` pins the id band.
    if mode == "expand" {
        let seed: Option<u64> = std::env::var("DT_SEED")
            .ok()
            .map(|s| s.parse())
            .transpose()?;
        // `expand <count>` drives the generated sweep. `expand <sub>` drives an
        // expand-only corpus: `corpus` (valid shapes), `errors` (malformed),
        // `templates` (object!/array!), or `all` (every corpus + the sweep).
        let sub = args.get(2).map(String::as_str).unwrap_or("");
        match sub {
            "corpus" => {
                let ok = corpus_report("corpus", &run::expand_corpus());
                if !ok {
                    std::process::exit(1);
                }
                return Ok(());
            }
            "errors" => {
                let ok = corpus_report("errors", &run::expand_errors());
                if !ok {
                    std::process::exit(1);
                }
                return Ok(());
            }
            "templates" => {
                let ok = corpus_report("templates", &run::expand_templates());
                if !ok {
                    std::process::exit(1);
                }
                return Ok(());
            }
            "all" => {
                let count: u64 = args.get(3).map(|s| s.parse()).transpose()?.unwrap_or(1000);
                // One process so coverage accumulates across every entry point.
                let valid = corpus_report("valid", &run::expand_corpus());
                let errs = corpus_report("errors", &run::expand_errors());
                let tmpl = corpus_report("templates", &run::expand_templates());
                let generated = run::expand(count, seed)?;
                if !(valid && errs && tmpl) {
                    eprintln!("expand-only corpus failures present");
                    std::process::exit(1);
                }
                expand_report(&generated);
                return Ok(());
            }
            _ => {
                let count: u64 = args.get(2).map(|s| s.parse()).transpose()?.unwrap_or(1000);
                let r = run::expand(count, seed)?;
                expand_report(&r);
                return Ok(());
            }
        }
    }

    // `diag [gen_count]` runs the diagnostics tier (deliberately broken cases
    // with span + message assertions) and reports on its own path.
    if mode == "diag" {
        let gen_count: u64 = args.get(2).map(|s| s.parse()).transpose()?.unwrap_or(200);
        let seed: Option<u64> = std::env::var("DT_SEED")
            .ok()
            .map(|s| s.parse())
            .transpose()?;
        let r = run::diag(gen_count, seed)?;
        diag_report(&r);
        return Ok(());
    }

    // <count> [samples] [batch_size] argument triple shared by run/sound/asan.
    let count =
        || -> Result<u64> { Ok(args.get(2).map(|s| s.parse()).transpose()?.unwrap_or(1000)) };
    let samples =
        || -> Result<u32> { Ok(args.get(3).map(|s| s.parse()).transpose()?.unwrap_or(20)) };
    let batch = || -> Result<u64> { Ok(args.get(4).map(|s| s.parse()).transpose()?.unwrap_or(50)) };
    // `DT_SEED=<u64>` pins the id band so a prior run reproduces exactly. Unset
    // (the default) draws a fresh random band each run, so re-running explores
    // new cases rather than re-testing the same ids.
    let seed: Option<u64> = std::env::var("DT_SEED")
        .ok()
        .map(|s| s.parse())
        .transpose()?;

    let r = match mode {
        "quick" => run::quick()?,
        // round-trip + equivalence (well-formed inputs)
        "run" => run::sweep(count()?, samples()?, batch()?, false, seed)?,
        // adds duplicate-key/truncated/malformed inputs + the allocator leak check
        "sound" => run::sweep(count()?, samples()?, batch()?, true, seed)?,
        // the full soundness set under AddressSanitizer + LeakSanitizer (nightly,
        // build-std). Defaults to a smaller count since sanitized runs are slower.
        "asan" => run::asan(
            args.get(2).map(|s| s.parse()).transpose()?.unwrap_or(200),
            samples()?,
            batch()?,
            seed,
        )?,
        other => {
            bail!("unknown mode: {other} (expected `quick`, `run`/`sound`/`asan <count> [samples] [batch]`, `diag [gen_count]`, `expand [count]`, `expand corpus|errors|templates|all`, or `case <id>`)")
        }
    };
    report(&r);
    Ok(())
}
