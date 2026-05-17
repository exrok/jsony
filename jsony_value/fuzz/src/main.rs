use jsony_value_fuzz::{validate_serde_json_equivalence, ValidationOutcome};
use std::ffi::OsString;
use std::path::PathBuf;
use std::process::ExitCode;

const SERDE_JSON_EQUIVALENCE: &str = "serde_json_equivalence";
const TARGET_NAMES: &[&str] = &[SERDE_JSON_EQUIVALENCE];

fn main() -> ExitCode {
    let mut args = std::env::args_os().skip(1);
    let Some(target_name) = args.next() else {
        print_usage();
        return ExitCode::from(2);
    };

    if target_name == "--help" || target_name == "-h" {
        print_usage();
        return ExitCode::SUCCESS;
    }

    let Some(target_name) = target_name.to_str() else {
        eprintln!("fuzz target name must be valid UTF-8");
        return ExitCode::from(2);
    };

    let Some(target) = FuzzTarget::parse(target_name) else {
        eprintln!("unknown fuzz target: {target_name}");
        print_usage();
        return ExitCode::from(2);
    };

    let paths: Vec<_> = args.map(PathBuf::from).collect();
    if paths.is_empty() {
        print_usage();
        return ExitCode::from(2);
    }

    target.run(paths)
}

enum FuzzTarget {
    SerdeJsonEquivalence,
}

impl FuzzTarget {
    fn parse(name: &str) -> Option<Self> {
        match name {
            SERDE_JSON_EQUIVALENCE => Some(Self::SerdeJsonEquivalence),
            _ => None,
        }
    }

    fn run(&self, paths: Vec<PathBuf>) -> ExitCode {
        match self {
            Self::SerdeJsonEquivalence => run_serde_json_equivalence(paths),
        }
    }
}

fn run_serde_json_equivalence(paths: Vec<PathBuf>) -> ExitCode {
    let mut failed = 0usize;

    for path in paths {
        let data = match std::fs::read(&path) {
            Ok(data) => data,
            Err(error) => {
                failed += 1;
                eprintln!("FAIL {}", path.display());
                eprintln!("issue: could not read input: {error}");
                continue;
            }
        };

        match validate_serde_json_equivalence(&data) {
            Ok(ValidationOutcome::Equivalent) => {
                println!("ok {}", path.display());
            }
            Ok(ValidationOutcome::BothRejected { .. }) => {
                println!("ok {}: both parsers rejected input", path.display());
            }
            Err(error) => {
                failed += 1;
                eprintln!("FAIL {}", path.display());
                eprint!("{}", error.render_report_with_input(&data));
            }
        }
    }

    if failed == 0 {
        ExitCode::SUCCESS
    } else {
        ExitCode::FAILURE
    }
}

fn print_usage() {
    let program = std::env::args_os()
        .next()
        .unwrap_or_else(|| OsString::from("jsony_value-fuzz"));

    eprintln!(
        "usage: {} <target> <input> [input ...]",
        PathBuf::from(program).display()
    );
    eprintln!("targets:");
    for target in TARGET_NAMES {
        eprintln!("  {target}");
    }
}
