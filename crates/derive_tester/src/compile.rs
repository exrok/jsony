//! Direct-`rustc` batch compilation.
//!
//! We scaffold a throwaway crate that depends on the local jsony (and patches in
//! the locally generated `jsony_macros`), run `cargo build -vv` once to capture
//! the exact `rustc` invocation cargo uses (env prefix + flags + the precise
//! `--extern jsony=<rlib>` and linker flags), then compile every batch by
//! re-running that command directly with the input source and output path
//! substituted. No cargo, no metadata, no clean — just `rustc` per batch, which
//! parallelizes trivially.

use anyhow::{bail, Context, Result};
use std::path::{Path, PathBuf};
use std::process::Command;

pub struct Toolchain {
    /// Environment-variable prefix cargo used (e.g. `LD_LIBRARY_PATH=... CARGO_*=...`).
    envs: String,
    /// The rustc command, beginning with the absolute path to rustc, transformed
    /// for batch use: single `--emit=link`, no incremental, human error format.
    /// Still contains the placeholder input path `src/main.rs` to be substituted.
    base: String,
    work_dir: PathBuf,
}

impl Toolchain {
    /// Scaffold + build once, capturing the rustc command. `jsony_root` is the
    /// absolute path to the jsony crate (the repo root).
    pub fn prepare(jsony_root: &str, work_dir: PathBuf) -> Result<Toolchain> {
        let scaffold = work_dir.join("scaffold");
        std::fs::create_dir_all(scaffold.join("src"))?;
        std::fs::write(
            scaffold.join("Cargo.toml"),
            format!(
                r#"[package]
name = "dt_scaffold"
version = "0.1.0"
edition = "2021"

[dependencies]
jsony = {{ path = "{jsony_root}" }}

[patch.crates-io]
jsony_macros = {{ path = "{jsony_root}/crates/jsony_macros" }}
"#
            ),
        )?;
        std::fs::write(scaffold.join("src/main.rs"), "fn main() {}\n")?;

        let target = work_dir.join("target");
        // Force a fresh build of the scaffold bin so the verbose rustc line is emitted.
        let _ = std::fs::remove_file(scaffold.join("src/main.rs"));
        std::fs::write(scaffold.join("src/main.rs"), "fn main() {}\n")?;

        let out = Command::new("cargo")
            .arg("build")
            .arg("-vv")
            .arg("--color=never")
            .current_dir(&scaffold)
            .env("CARGO_TARGET_DIR", &target)
            .env("CARGO_TERM_COLOR", "never")
            .output()
            .context("spawning cargo build -vv")?;
        if !out.status.success() {
            bail!(
                "cargo build -vv failed:\n{}",
                String::from_utf8_lossy(&out.stderr)
            );
        }
        let combined = format!(
            "{}\n{}",
            String::from_utf8_lossy(&out.stderr),
            String::from_utf8_lossy(&out.stdout)
        );
        let (envs, raw_base) =
            extract_rustc(&combined, "dt_scaffold").context("no rustc command in cargo -vv")?;

        // Transform for batch use.
        let mut base = raw_base.replacen("--emit=dep-info,link", "--emit=link", 1);
        base = strip_flag_token(&base, "--error-format=");
        base = strip_flag_token(&base, "--json=");
        base = strip_flag_token(&base, "-C incremental=");

        Ok(Toolchain {
            envs,
            base,
            work_dir,
        })
    }

    /// Compile `source` to a fresh binary tagged by `tag`. Returns the binary
    /// path on success, or the compiler's stderr on failure.
    pub fn compile(&self, tag: &str, source: &str) -> Result<PathBuf, String> {
        let src = self.work_dir.join(format!("{tag}.rs"));
        let bin = self.work_dir.join(format!("{tag}.bin"));
        std::fs::write(&src, source).map_err(|e| format!("write {tag}.rs: {e}"))?;
        let cmd = self.base.replacen("src/main.rs", &src.to_string_lossy(), 1);
        let full = format!("{} {} -o {}", self.envs, cmd, bin.display());
        let out = Command::new("/bin/sh")
            .arg("-c")
            .arg(&full)
            .output()
            .map_err(|e| format!("spawn rustc: {e}"))?;
        if !out.status.success() {
            return Err(format!(
                "compile failed:\n{}",
                String::from_utf8_lossy(&out.stderr)
            ));
        }
        Ok(bin)
    }

    /// Run a compiled batch binary with the input file path as its argument,
    /// returning its captured output.
    pub fn run(&self, bin: &Path, input_path: &Path) -> Result<std::process::Output, String> {
        Command::new(bin)
            .arg(input_path)
            .output()
            .map_err(|e| format!("spawn {}: {e}", bin.display()))
    }
}

/// Locate the `Running \`...rustc --crate-name <name> ...\`` line in cargo's
/// verbose output and split it into (env prefix, rustc command). The rustc
/// command retains the absolute path to the rustc binary.
fn extract_rustc(output: &str, crate_name: &str) -> Option<(String, String)> {
    let needle = format!("rustc --crate-name {crate_name} ");
    for line in output.lines() {
        if !line.contains(&needle) {
            continue;
        }
        let mut line = line.trim();
        if let Some(rest) = line.strip_prefix("Running `") {
            line = rest;
        }
        line = line.strip_suffix('`').unwrap_or(line);
        let idx = line.find("rustc --crate-name")?;
        let before = &line[..idx]; // envs + " " + absolute path to .../bin/
        let split = before.rfind(' ').unwrap_or(0);
        let (envs, path_prefix) = before.split_at(split);
        let base = format!("{}{}", path_prefix.trim_start(), &line[idx..]);
        return Some((envs.trim().to_string(), base));
    }
    None
}

/// Remove `prefix` plus its value, where the value runs from the end of the
/// prefix to the next space. `prefix` may itself contain a space (e.g.
/// `-C incremental=`), so the terminator is searched for after the prefix.
fn strip_flag_token(cmd: &str, prefix: &str) -> String {
    let Some(start) = cmd.find(prefix) else {
        return cmd.to_string();
    };
    let from = start + prefix.len();
    let end = cmd[from..].find(' ').map(|i| from + i).unwrap_or(cmd.len());
    let mut out = String::with_capacity(cmd.len());
    out.push_str(&cmd[..start]);
    out.push_str(&cmd[end..]);
    out
}
