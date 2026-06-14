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

    let r = match mode {
        "quick" => run::quick()?,
        // run <count> [samples] [batch_size]
        "run" => {
            let count: u64 = args.get(2).map(|s| s.parse()).transpose()?.unwrap_or(1000);
            let samples: u32 = args.get(3).map(|s| s.parse()).transpose()?.unwrap_or(20);
            let batch: u64 = args.get(4).map(|s| s.parse()).transpose()?.unwrap_or(50);
            run::run(0..count, batch, samples, 16)?
        }
        other => bail!("unknown mode: {other} (expected `quick` or `run <count> [samples] [batch]`)"),
    };
    report(&r);
    Ok(())
}
