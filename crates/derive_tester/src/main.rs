use anyhow::{bail, Result};
use derive_tester::run::{self, Report};

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

    // <count> [samples] [batch_size] argument triple shared by run/sound/asan.
    let count = || -> Result<u64> { Ok(args.get(2).map(|s| s.parse()).transpose()?.unwrap_or(1000)) };
    let samples = || -> Result<u32> { Ok(args.get(3).map(|s| s.parse()).transpose()?.unwrap_or(20)) };
    let batch = || -> Result<u64> { Ok(args.get(4).map(|s| s.parse()).transpose()?.unwrap_or(50)) };

    let r = match mode {
        "quick" => run::quick()?,
        // round-trip only (well-formed inputs)
        "run" => run::sweep(count()?, samples()?, batch()?, false)?,
        // adds truncated/malformed inputs + the allocator leak check
        "sound" => run::sweep(count()?, samples()?, batch()?, true)?,
        // malformed-heavy inputs under AddressSanitizer + LeakSanitizer (nightly,
        // build-std). Defaults to a smaller count since sanitized runs are slower.
        "asan" => run::asan(
            args.get(2).map(|s| s.parse()).transpose()?.unwrap_or(200),
            samples()?,
            batch()?,
        )?,
        other => {
            bail!("unknown mode: {other} (expected `quick`, `run`/`sound`/`asan <count> [samples] [batch]`, or `case <id>`)")
        }
    };
    report(&r);
    Ok(())
}
