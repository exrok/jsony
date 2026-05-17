use jsony_value_fuzz::{validate_serde_json_equivalence, ValidationOutcome};
use std::ffi::OsString;
use std::path::PathBuf;
use std::process::ExitCode;

fn main() -> ExitCode {
    let mut paths = Vec::new();

    for arg in std::env::args_os().skip(1) {
        if arg == "--help" || arg == "-h" {
            print_usage();
            return ExitCode::SUCCESS;
        }

        paths.push(PathBuf::from(arg));
    }

    if paths.is_empty() {
        print_usage();
        return ExitCode::from(2);
    }

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
            Ok(ValidationOutcome::IgnoredInvalidSerdeJson { message }) => {
                println!(
                    "skip {}: serde_json rejected input: {message}",
                    path.display()
                );
            }
            Err(error) => {
                failed += 1;
                eprintln!("FAIL {}", path.display());
                eprint!("{}", error.render_report());
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
        .unwrap_or_else(|| OsString::from("serde_json_equivalence_check"));
    eprintln!(
        "usage: {} <input> [input ...]",
        PathBuf::from(program).display()
    );
}
