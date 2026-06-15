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
use std::hash::{Hash, Hasher};
use std::path::{Path, PathBuf};
use std::process::Command;
use std::time::SystemTime;

/// A batch-compilation toolchain configuration. The throughput spec is the
/// default fast tier. The ASAN spec rebuilds std with the address sanitizer for
/// the soundness tier.
#[derive(Clone, Copy)]
pub struct ToolchainSpec {
    /// Subdirectory + cache key, so distinct specs do not collide on disk.
    pub name: &'static str,
    /// rustup toolchain override (e.g. `nightly`), or None for the default.
    pub channel: Option<&'static str>,
    /// Extra `RUSTFLAGS` for the capture build, baked into the captured command.
    pub rustflags: &'static str,
    /// Explicit `--target` triple, required by the sanitizers.
    pub target: Option<&'static str>,
    /// Pass `-Zbuild-std` so std/core/alloc are rebuilt. The sanitizers need this
    /// to instrument allocations and reads inside std (LeakSanitizer in
    /// particular reports nothing without it).
    pub build_std: bool,
    /// `[profile.*]` overrides appended to the scaffold manifest.
    pub profile: &'static str,
    /// Optimization level forced on the *reused batch command* only, overriding
    /// the captured one. The dependencies and (under build-std) the standard
    /// library are built once by the capture `cargo build` at the manifest
    /// profile's opt-level and stay that way; only the throwaway batch binary's
    /// own codegen drops to this level, so it compiles fast while everything it
    /// links remains optimized. `None` leaves the captured opt-level untouched.
    pub batch_opt_level: Option<&'static str>,
    /// Environment applied when running a compiled batch binary.
    pub run_env: &'static [(&'static str, &'static str)],
}

/// Throughput profile (see docs/derive-tester.md). The batch crate is never
/// debugged and its runtime is already cheap, so strip debug info and skip
/// optimization for fast compiles. The dependencies are built once and reused
/// across every batch, so optimize them and strip their debug info.
pub const THROUGHPUT: ToolchainSpec = ToolchainSpec {
    name: "stable",
    channel: None,
    rustflags: "",
    target: None,
    build_std: false,
    profile: r#"[profile.dev]
debug = false
opt-level = 0

[profile.dev.package."*"]
debug = false
opt-level = 3
"#,
    batch_opt_level: None,
    run_env: &[],
};

/// AddressSanitizer + LeakSanitizer tier. Soundness profile keeps overflow and
/// debug-assertion checks on, and `-Zbuild-std` instruments std so LSAN reports
/// leaks. Runs over the malformed-heavy input set.
///
/// The dependencies and the instrumented std build at `opt-level = 2` (the
/// monomorphized jsony codecs the batch links run hot over the sample set), but
/// the batch binary itself is rewritten to `opt-level = 0` (`batch_opt_level`):
/// its own codegen is throwaway and the LLVM optimization of one big batch is
/// what dominated the compile, so dropping it roughly halves batch compile time
/// while the optimized std/jsony keep the sanitized run fast.
pub const ASAN: ToolchainSpec = ToolchainSpec {
    name: "asan",
    channel: Some("nightly"),
    rustflags: "-Zsanitizer=address",
    target: Some("x86_64-unknown-linux-gnu"),
    build_std: true,
    profile: r#"[profile.dev]
debug = true
opt-level = 2
overflow-checks = true
debug-assertions = true
"#,
    batch_opt_level: Some("0"),
    run_env: &[("ASAN_OPTIONS", "detect_leaks=1:abort_on_error=1")],
};

pub struct Toolchain {
    /// Environment-variable prefix cargo used (e.g. `LD_LIBRARY_PATH=... CARGO_*=...`).
    envs: String,
    /// The rustc command, beginning with the absolute path to rustc, transformed
    /// for batch use: single `--emit=link`, no incremental, human error format.
    /// Still contains the placeholder input path `src/main.rs` to be substituted.
    base: String,
    work_dir: PathBuf,
    /// Environment applied to each batch binary run (e.g. `ASAN_OPTIONS`).
    run_env: Vec<(String, String)>,
}

impl Toolchain {
    /// Scaffold + build once, capturing the rustc command. `jsony_root` is the
    /// absolute path to the jsony crate (the repo root).
    ///
    /// The capture is cached in `work_dir` and reused when neither the jsony
    /// sources, the generated macros, the rustc version, nor the scaffold
    /// manifest have changed since the last run. That skips the `cargo build -vv`
    /// bootstrap on every tight edit-test loop where only the harness changed.
    pub fn prepare(jsony_root: &str, work_dir: PathBuf, spec: ToolchainSpec) -> Result<Toolchain> {
        let base_dir = work_dir.join(spec.name);
        let scaffold = base_dir.join("scaffold");
        std::fs::create_dir_all(scaffold.join("src"))?;
        let cargo_toml = format!(
            r#"[package]
name = "dt_scaffold"
version = "0.1.0"
edition = "2021"

[dependencies]
jsony = {{ path = "{jsony_root}" }}

[patch.crates-io]
jsony_macros = {{ path = "{jsony_root}/crates/jsony_macros" }}

{}"#,
            spec.profile
        );
        std::fs::write(scaffold.join("Cargo.toml"), &cargo_toml)?;

        let run_env: Vec<(String, String)> = spec
            .run_env
            .iter()
            .map(|(k, v)| (k.to_string(), v.to_string()))
            .collect();

        let fingerprint = capture_fingerprint(jsony_root, &cargo_toml, spec);
        let cache_path = base_dir.join("toolchain.cache");
        if let Some(tc) = load_cache(&cache_path, &fingerprint, work_dir.clone(), &run_env) {
            return Ok(tc);
        }

        let target = base_dir.join("target");
        // Force a fresh build of the scaffold bin so the verbose rustc line is emitted.
        let _ = std::fs::remove_file(scaffold.join("src/main.rs"));
        std::fs::write(scaffold.join("src/main.rs"), "fn main() {}\n")?;

        let mut cmd = Command::new("cargo");
        if let Some(channel) = spec.channel {
            cmd.arg(format!("+{channel}"));
        }
        cmd.arg("build").arg("-vv").arg("--color=never");
        if spec.build_std {
            cmd.arg("-Zbuild-std");
        }
        if let Some(triple) = spec.target {
            cmd.arg("--target").arg(triple);
        }
        cmd.current_dir(&scaffold)
            .env("CARGO_TARGET_DIR", &target)
            .env("CARGO_TERM_COLOR", "never");
        if !spec.rustflags.is_empty() {
            cmd.env("RUSTFLAGS", spec.rustflags);
        }
        let out = cmd.output().context("spawning cargo build -vv")?;
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
        // Force the batch binary's own opt-level down without touching the
        // already-built (optimized) dependencies and std it links against.
        if let Some(level) = spec.batch_opt_level {
            base = set_opt_level(&base, level);
        }

        // Persist the capture: fingerprint, then envs and base on their own
        // lines (both are single-line, the rustc command has no embedded newline).
        let _ = std::fs::write(&cache_path, format!("{fingerprint}\n{envs}\n{base}\n"));

        Ok(Toolchain {
            envs,
            base,
            work_dir,
            run_env,
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

    /// Type-check `source` without codegen, capturing the compiler's structured
    /// diagnostics. Emits metadata only (no link) and `--error-format=json`, so
    /// errors and their spans come back as one JSON object per stderr line. The
    /// diagnostics tier parses these to assert error message and span. The exit
    /// status is ignored: a "broken" case is expected to fail, and the verdict
    /// comes from the parsed diagnostics, not the status.
    pub fn check_json(&self, tag: &str, source: &str) -> Result<String, String> {
        let src = self.work_dir.join(format!("{tag}.rs"));
        let meta = self.work_dir.join(format!("{tag}.rmeta"));
        std::fs::write(&src, source).map_err(|e| format!("write {tag}.rs: {e}"))?;
        let cmd = self
            .base
            .replacen("src/main.rs", &src.to_string_lossy(), 1)
            .replacen("--emit=link", "--emit=metadata", 1);
        let full = format!(
            "{} {} --error-format=json -o {}",
            self.envs,
            cmd,
            meta.display()
        );
        let out = Command::new("/bin/sh")
            .arg("-c")
            .arg(&full)
            .output()
            .map_err(|e| format!("spawn rustc: {e}"))?;
        Ok(String::from_utf8_lossy(&out.stderr).into_owned())
    }

    /// Run a compiled batch binary with the input file path as its argument,
    /// returning its captured output. Applies the spec's run environment (e.g.
    /// `ASAN_OPTIONS`).
    pub fn run(&self, bin: &Path, input_path: &Path) -> Result<std::process::Output, String> {
        let mut cmd = Command::new(bin);
        cmd.arg(input_path);
        for (k, v) in &self.run_env {
            cmd.env(k, v);
        }
        cmd.output()
            .map_err(|e| format!("spawn {}: {e}", bin.display()))
    }
}

/// Fingerprint of everything that would invalidate a cached rustc command: the
/// rustc version, the scaffold manifest (so a profile change is detected), and
/// the newest mtime across the jsony sources and the generated macros.
fn capture_fingerprint(jsony_root: &str, cargo_toml: &str, spec: ToolchainSpec) -> String {
    let mut h = std::collections::hash_map::DefaultHasher::new();
    if let Ok(out) = Command::new("rustc").arg("--version").output() {
        out.stdout.hash(&mut h);
    }
    cargo_toml.hash(&mut h);
    // Spec discriminators that are not reflected in the manifest.
    spec.channel.hash(&mut h);
    spec.rustflags.hash(&mut h);
    spec.target.hash(&mut h);
    spec.build_std.hash(&mut h);
    spec.batch_opt_level.hash(&mut h);
    let root = Path::new(jsony_root);
    let mut newest = SystemTime::UNIX_EPOCH;
    for rel in ["src", "Cargo.toml", "crates/jsony_macros/src"] {
        max_mtime(&root.join(rel), &mut newest);
    }
    if let Ok(d) = newest.duration_since(SystemTime::UNIX_EPOCH) {
        d.as_nanos().hash(&mut h);
    }
    format!("{:016x}", h.finish())
}

/// Recursively fold the newest modification time under `path` into `newest`.
fn max_mtime(path: &Path, newest: &mut SystemTime) {
    let Ok(meta) = std::fs::metadata(path) else {
        return;
    };
    if let Ok(m) = meta.modified() {
        if m > *newest {
            *newest = m;
        }
    }
    if meta.is_dir() {
        let Ok(entries) = std::fs::read_dir(path) else {
            return;
        };
        for entry in entries.flatten() {
            max_mtime(&entry.path(), newest);
        }
    }
}

/// Reload a cached capture when its fingerprint matches and the rlib it refers
/// to still exists (the target dir persists alongside the cache).
fn load_cache(
    cache_path: &Path,
    fingerprint: &str,
    work_dir: PathBuf,
    run_env: &[(String, String)],
) -> Option<Toolchain> {
    let body = std::fs::read_to_string(cache_path).ok()?;
    let mut lines = body.lines();
    if lines.next()? != fingerprint {
        return None;
    }
    let envs = lines.next()?.to_string();
    let base = lines.next()?.to_string();
    let rlib = extern_rlib(&base, "jsony")?;
    if !Path::new(rlib).exists() {
        return None;
    }
    Some(Toolchain {
        envs,
        base,
        work_dir,
        run_env: run_env.to_vec(),
    })
}

/// Extract the `--extern <crate>=<path>` rlib path from a captured command.
fn extern_rlib<'a>(base: &'a str, crate_name: &str) -> Option<&'a str> {
    let needle = format!("--extern {crate_name}=");
    let start = base.find(&needle)? + needle.len();
    let rest = &base[start..];
    let end = rest.find(' ').unwrap_or(rest.len());
    Some(&rest[..end])
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

/// Replace the value of the `-C opt-level=` flag in a captured rustc command.
/// Only the batch command is rewritten; the dependencies and std were already
/// built at the manifest profile's opt-level by the capture `cargo build`.
fn set_opt_level(cmd: &str, level: &str) -> String {
    let prefix = "-C opt-level=";
    let Some(start) = cmd.find(prefix) else {
        return cmd.to_string();
    };
    let from = start + prefix.len();
    let end = cmd[from..].find(' ').map(|i| from + i).unwrap_or(cmd.len());
    let mut out = String::with_capacity(cmd.len());
    out.push_str(&cmd[..from]);
    out.push_str(level);
    out.push_str(&cmd[end..]);
    out
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
