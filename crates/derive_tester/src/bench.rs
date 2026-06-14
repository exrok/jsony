use std::io::Write;
use std::path::{Path, PathBuf};
use std::process::Stdio;
use std::time::Duration;

use anyhow::{bail, Context};
use jsony::Jsony;
use proc_macro2::TokenTree;

use crate::library::Library;
use crate::token;

mod maybe_from_str {
    use std::str::FromStr;

    use jsony::{
        json::DecodeError,
        parser::{Parser, Peek},
        FromJson,
    };

    pub fn decode_json<'a, T: FromStr + FromJson<'a>>(
        parser: &mut Parser<'a>,
    ) -> Result<T, &'static DecodeError>
    where
        <T as FromStr>::Err: std::fmt::Display,
    {
        match parser.peek()? {
            Peek::String => parser.take_string()?.parse::<T>().map_err(|err| {
                parser.report_error(err.to_string());
                &DecodeError {
                    message: "Failed to parse value from string",
                }
            }),
            _ => T::decode_json(parser),
        }
    }
}

mod duration_internals {
    use jsony::FromJson;
    use std::time::Duration;

    pub fn decode_json(
        parser: &mut jsony::parser::Parser<'_>,
    ) -> Result<Duration, &'static jsony::json::DecodeError> {
        #[derive(jsony::Jsony)]
        struct InternalDuration {
            secs: u64,
            nanos: u32,
        }
        let i = InternalDuration::decode_json(parser)?;
        Ok(Duration::new(i.secs, i.nanos))
    }
}

#[derive(Jsony, Debug, Default)]
#[jsony(Json)]
pub struct Counter {
    #[jsony(rename = "counter-value", FromJson with = maybe_from_str)]
    pub value: f64,
    pub unit: String,
    pub event: String,
    #[jsony(default)]
    pub variance: Option<f64>,
}

impl Counter {
    pub fn normalize(&self, baseline: &Counter) -> Counter {
        Counter {
            value: self.value - baseline.value,
            unit: self.unit.clone(),
            event: self.event.clone(),
            variance: None,
        }
    }

    pub fn average(counters: &[&Counter]) -> Counter {
        let n = counters.len() as f64;
        let mean = counters.iter().map(|c| c.value).sum::<f64>() / n;
        let variance = if counters.len() > 1 {
            let var = counters
                .iter()
                .map(|c| (c.value - mean).powi(2))
                .sum::<f64>()
                / (n - 1.0);
            Some(var.sqrt() / mean * 100.0)
        } else {
            None
        };
        Counter {
            value: mean,
            unit: counters[0].unit.clone(),
            event: counters[0].event.clone(),
            variance,
        }
    }
}

#[derive(Debug, Jsony)]
#[jsony(Json)]
pub struct Perf {
    pub instructions: Counter,
    pub cycles: Counter,
    pub task_clock: Counter,
    pub duration: Counter,
    pub build_size: Option<u64>,
}

#[derive(Debug, Jsony)]
#[jsony(Json)]
pub struct LibResult {
    pub name: String,
    pub lib: Library,
    #[jsony(default)]
    pub version: String,
    pub versions: Vec<(String, String)>,
    pub benchmarks: Vec<(Scenario, Perf)>,
    pub dependencies: String,
}

impl LibResult {
    pub fn resolve_version(&mut self) {
        let target = self.lib.version_crate_name();
        if let Some((_, ver)) = self.versions.iter().find(|(name, _)| name == target) {
            self.version = ver.clone();
        }
    }
}

impl std::fmt::Display for Perf {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "{:>8.2} ms  {:>10.6} Bcycles {:>10.6} Binst {:>8.2} task-clock",
            self.duration.value / 1_000_000.0,
            self.cycles.value / 1_000_000_000.0,
            self.instructions.value / 1_000_000_000.0,
            self.task_clock.value,
        )?;

        if let Some(build_size) = self.build_size {
            write!(f, " {:>8.0} kb (stripped)", build_size as f64 / 1024.0)?;
        }

        Ok(())
    }
}

impl Perf {
    pub fn normalized(&self, baseline: &Perf) -> Perf {
        Perf {
            instructions: self.instructions.normalize(&baseline.instructions),
            cycles: self.cycles.normalize(&baseline.cycles),
            task_clock: self.task_clock.normalize(&baseline.task_clock),
            duration: self.duration.normalize(&baseline.duration),
            build_size: self
                .build_size
                .map(|size| size - baseline.build_size.unwrap_or(0)),
        }
    }

    pub fn average(perfs: &[Perf]) -> Perf {
        Perf {
            instructions: Counter::average(
                &perfs.iter().map(|p| &p.instructions).collect::<Vec<_>>(),
            ),
            cycles: Counter::average(&perfs.iter().map(|p| &p.cycles).collect::<Vec<_>>()),
            task_clock: Counter::average(&perfs.iter().map(|p| &p.task_clock).collect::<Vec<_>>()),
            duration: Counter::average(&perfs.iter().map(|p| &p.duration).collect::<Vec<_>>()),
            build_size: perfs.last().and_then(|p| p.build_size),
        }
    }
}

#[derive(Copy, Clone, Debug, Jsony, Hash, PartialEq, Eq)]
#[jsony(Json)]
pub enum Incremental {
    Disabled,
    Unchanged,
    Touch,
    Postfix,
    Prefix,
    TypeTransform,
}

impl Incremental {
    pub fn cli_name(&self) -> &'static str {
        match self {
            Incremental::Disabled => "disabled",
            Incremental::Unchanged => "unchanged",
            Incremental::Touch => "touch",
            Incremental::Postfix => "postfix",
            Incremental::Prefix => "prefix",
            Incremental::TypeTransform => "type-transform",
        }
    }
}

#[derive(Clone, Debug, Jsony, Hash, PartialEq, Eq)]
#[jsony(Json)]
pub enum Scenario {
    WarmBuild {
        incremental: Incremental,
        profile: BuildProfile,
    },
    WarmCheck {
        incremental: Incremental,
    },
    CleanBuild {
        profile: BuildProfile,
    },
    RuntimeBenchmark {
        profile: BuildProfile,
    },
}

impl Scenario {
    pub fn class_name(&self) -> &'static str {
        match self {
            Scenario::WarmBuild { .. } => "Warm Build",
            Scenario::WarmCheck { .. } => "Warm Check",
            Scenario::CleanBuild { .. } => "Clean Build",
            Scenario::RuntimeBenchmark { .. } => "Runtime Benchmark",
        }
    }

    #[rustfmt::skip]
    pub const ALL: &[Scenario] = &[
        Scenario::WarmBuild { incremental: Incremental::Disabled, profile: BuildProfile::Release },
        Scenario::WarmBuild { incremental: Incremental::Disabled, profile: BuildProfile::Debug },
        Scenario::WarmBuild { incremental: Incremental::Unchanged, profile: BuildProfile::Debug },
        Scenario::WarmBuild { incremental: Incremental::Postfix, profile: BuildProfile::Debug },
        Scenario::WarmBuild { incremental: Incremental::Prefix, profile: BuildProfile::Debug },
        Scenario::WarmBuild { incremental: Incremental::TypeTransform, profile: BuildProfile::Debug },

        Scenario::WarmCheck { incremental: Incremental::Disabled },
        Scenario::WarmCheck { incremental: Incremental::Unchanged },
        Scenario::WarmCheck { incremental: Incremental::Postfix },
        Scenario::WarmCheck { incremental: Incremental::Prefix },
        Scenario::WarmCheck { incremental: Incremental::TypeTransform },

        Scenario::RuntimeBenchmark { profile: BuildProfile::ReleaseLto },
        Scenario::RuntimeBenchmark { profile: BuildProfile::Release },
        Scenario::RuntimeBenchmark { profile: BuildProfile::Debug },
        Scenario::CleanBuild { profile: BuildProfile::Debug },
        Scenario::CleanBuild { profile: BuildProfile::Release },
    ];
}

#[derive(PartialEq, Eq, Clone, Copy)]
pub enum CargoAction {
    DebugBuild,
    ReleaseBuild,
    Check,
}

#[derive(Clone, Copy, Debug, Jsony, Hash, PartialEq, Eq)]
#[jsony(Json)]
pub enum BuildProfile {
    Debug,
    Release,
    ReleaseLto,
    ReleaseLtoNative,
}

impl BuildProfile {
    pub fn cli_name(&self) -> &'static str {
        match self {
            BuildProfile::Debug => "debug",
            BuildProfile::Release => "release",
            BuildProfile::ReleaseLto => "release-lto",
            BuildProfile::ReleaseLtoNative => "release-lto-native",
        }
    }

    pub fn dir_name(&self) -> &'static str {
        match self {
            BuildProfile::Debug => "debug",
            BuildProfile::Release => "release",
            BuildProfile::ReleaseLto | BuildProfile::ReleaseLtoNative => "release-lto",
        }
    }
}

#[derive(jsony::Jsony, Debug)]
struct ProfileEvent<'a> {
    label: &'a str,
    #[jsony(with = duration_internals)]
    time: Duration,
    #[jsony(with = duration_internals)]
    self_time: Duration,
    invocation_count: u64,
}

#[derive(jsony::Jsony, Debug)]
struct ProfileTotalTime {
    #[jsony(with = duration_internals)]
    total_time: Duration,
}

#[derive(Debug)]
pub struct SelfProfileArtifacts {
    pub profile_dir: PathBuf,
    pub profile_prefix: PathBuf,
    pub summary_json: PathBuf,
    pub chrome_trace: PathBuf,
}

#[derive(Debug)]
pub struct SelfProfileSelfTime {
    pub label: String,
    pub self_time: Duration,
}

pub fn base_dir() -> &'static Path {
    Path::new("/tmp/benchy/")
}

pub fn extract_lock_versions(_crate_dir: &Path) -> Vec<(String, String)> {
    // Dependency-version recording was benchmark-only; not needed for testing.
    Vec::new()
}

pub fn read_self_profile_self_times(
    summary_json: &Path,
) -> anyhow::Result<(Duration, Vec<SelfProfileSelfTime>)> {
    let profile = std::fs::read_to_string(summary_json)
        .with_context(|| format!("opening profile summary json: {}", summary_json.display()))?;
    let total_time = jsony::from_json::<ProfileTotalTime>(&profile)?.total_time;
    let events: Vec<ProfileEvent> = jsony::drill(&profile)["query_data"].parse()?;
    Ok((
        total_time,
        events
            .into_iter()
            .map(|event| SelfProfileSelfTime {
                label: event.label.to_string(),
                self_time: event.self_time,
            })
            .collect(),
    ))
}

fn extract_run(output: &str, name: &str) -> Option<String> {
    let mut build_command: Option<String> = None;

    for line in output.lines() {
        let Some(initial) = line.strip_prefix("     Running `") else {
            continue;
        };
        let Some((inner, _)) = initial.rsplit_once('`') else {
            continue;
        };
        let Some((_, rest)) = inner.split_once("--crate-name ") else {
            continue;
        };
        let Some((crate_name, _)) = rest.split_once(" ") else {
            continue;
        };
        if crate_name == name {
            build_command = Some(inner.to_string());
        }
    }
    build_command
}

#[derive(Debug)]
struct RustCommand {
    envs: String,
    incremental: String,
    base: String,
}

impl RustCommand {
    fn parse(command: &str) -> anyhow::Result<RustCommand> {
        let (before, _) = command
            .split_once("rustc --crate-name")
            .context("Couldn't find rustc")?;
        let mid = before.rsplit_once(' ').unwrap_or(("", "")).0.len();
        let (envs, raw_base) = command.split_at(mid);
        let raw_base = raw_base.replace("-Clink-arg=-fuse-ld=mold", "");
        if let Some((before, rest)) = raw_base.split_once("-C incremental=") {
            let (inc, after) = rest.split_once(' ').unwrap_or((rest, ""));
            Ok(RustCommand {
                envs: envs.to_string(),
                incremental: inc.to_string(),
                base: format!("{before}{after}"),
            })
        } else {
            Ok(RustCommand {
                envs: envs.to_string(),
                incremental: "".to_string(),
                base: raw_base.to_string(),
            })
        }
    }
}

pub struct Benchy {
    pub crate_directory: PathBuf,
    command_cache: Option<CargoAction>,
    command: RustCommand,
    pub name: String,
    pub modification_target: String,
    modification_counter: u32,
}

impl Benchy {
    pub fn open(name: &str, deps: &str) -> anyhow::Result<Benchy> {
        Self::open_in(base_dir(), name, deps)
    }

    pub fn open_in(base: &Path, name: &str, deps: &str) -> anyhow::Result<Benchy> {
        let root = base.join(name);
        std::fs::create_dir_all(&root)?;
        std::fs::write(
            root.join("Cargo.toml"),
            format!(
                r#"
[package]
name = "{name}"
version = "0.1.0"
edition = "2021"

[profile.release-lto]
inherits = "release"
lto = true
codegen-units = 1

[dependencies]
{deps}
"#
            ),
        )?;

        let _ = std::fs::create_dir(&root.join("src"));
        std::fs::write(root.join("src/main.rs"), "fn main() {}")?;

        Ok(Benchy {
            name: name.to_string(),
            crate_directory: root,
            command_cache: None,
            command: RustCommand {
                envs: String::new(),
                incremental: String::new(),
                base: String::new(),
            },
            modification_target: "src/main.rs".to_string(),
            modification_counter: 0,
        })
    }

    pub fn read_versions(&self) -> Vec<(String, String)> {
        extract_lock_versions(&self.crate_directory)
    }

    pub fn write_bytes(&self, path: &str, contents: &[u8]) -> anyhow::Result<()> {
        let full_path = self.crate_directory.join("./src/").join(path);
        if let Some(parent) = full_path.parent() {
            std::fs::create_dir_all(parent)?;
        }
        std::fs::write(&full_path, contents)?;
        Ok(())
    }

    pub fn write(&self, path: &str, contents: Vec<TokenTree>) -> anyhow::Result<()> {
        let contents = token::to_rust(contents.into_iter().collect());
        self.write_bytes(path, &contents)
    }

    pub fn rustc_from_cargo(&mut self, action: CargoAction) -> anyhow::Result<()> {
        self.rustc_from_cargo_toolchain(action, None)
    }

    pub fn rustc_from_cargo_toolchain(
        &mut self,
        action: CargoAction,
        toolchain: Option<&str>,
    ) -> anyhow::Result<()> {
        if self.command_cache == Some(action) && toolchain.is_none() {
            return Ok(());
        }
        let _ = std::fs::remove_dir_all(self.crate_directory.join("./target").as_os_str());
        let args: &[&str] = match action {
            CargoAction::DebugBuild => &["build"],
            CargoAction::ReleaseBuild => &["build", "--release"],
            CargoAction::Check => &["check"],
        };
        let main_path = self.crate_directory.join("src/main.rs");
        let original_main = std::fs::read(&main_path)?;
        std::fs::write(&main_path, "fn main() {}")?;
        let mut cmd = std::process::Command::new("cargo");
        if let Some(tc) = toolchain {
            cmd.arg(format!("+{tc}"));
        }
        let value = cmd
            .arg("-vv")
            .args(args)
            .env("RUSTFLAGS", "")
            .env(
                "CARGO_TARGET_DIR",
                self.crate_directory.join("./target").as_os_str(),
            )
            .current_dir(&self.crate_directory)
            .stdout(Stdio::piped())
            .stderr(Stdio::piped())
            .spawn()?;
        let output = value.wait_with_output()?;
        let stdout = String::from_utf8_lossy(&output.stdout);
        let stderr = String::from_utf8_lossy(&output.stderr);

        if !output.status.success() {
            bail!(
                "cargo {} -vv failed for {} (exit {}):\n{}\n{}",
                if let Some(tc) = toolchain {
                    format!("+{tc} {}", args.join(" "))
                } else {
                    args.join(" ")
                },
                self.name,
                output.status.code().unwrap_or(-1),
                stderr.trim(),
                stdout.trim(),
            );
        }

        let combined = format!("{stderr}\n{stdout}");
        let build_command = extract_run(
            &combined,
            self.crate_directory.file_name().unwrap().to_str().unwrap(),
        )
        .context("Did not find build command in cargo -vv output")?;
        self.command = RustCommand::parse(&build_command)?;

        std::fs::write(&main_path, original_main)?;
        Ok(())
    }

    pub fn self_profile_macro_expand_time(&mut self) -> anyhow::Result<Duration> {
        self.rustc_from_cargo(CargoAction::Check)?;
        let r = 5;
        let mut total: f64 = 0.0;
        'outer: for _ in 0..r {
            let envs = &self.command.envs;
            let command = &self.command.base;
            let inc = &self.command.incremental;
            let mut proc = std::process::Command::new("/bin/sh")
                .arg("-c")
                .arg(format!(
                    "{envs} {command} -Zself-profile -Zself-profile-events=default,args -C incremental={inc}"
                ))
                .current_dir(&self.crate_directory)
                .spawn()?;
            let pid = proc.id();
            if !proc.wait()?.success() {
                bail!("Process failed")
            }
            let mut profile_path = self
                .crate_directory
                .join(&format!("{}-{:07}", self.name, pid));

            let status = std::process::Command::new("rustc-summarize")
                .arg("summarize")
                .arg("--json")
                .arg(&profile_path)
                .current_dir(&self.crate_directory)
                .status()?;
            if !status.success() {
                bail!("Failed to summarize")
            }
            profile_path.set_extension("json");
            let profile =
                std::fs::read_to_string(&profile_path).context("opening profile summary json")?;

            let mut events: Vec<ProfileEvent> = jsony::drill(&profile)["query_data"].parse()?;
            events.sort_by_key(|evt| evt.time);
            for event in events.iter().rev().take(10) {
                println!(
                    "{: >40}: {: >14?} {: >14?}",
                    event.label, event.time, event.self_time
                );
            }
            println!("\n\n");

            events.sort_by_key(|evt| evt.self_time);
            for event in events.iter().rev().take(10) {
                println!(
                    "{: >40}: {: >14?} {: >14?}",
                    event.label, event.time, event.self_time
                );
            }

            for event in &events {
                if event.label == "macro_expand_crate" {
                    total += event.time.as_secs_f64();
                    println!("{:?}", event.time);
                    continue 'outer;
                }
            }
            panic!("Did not find macro_expand_crate event")
        }

        Ok(Duration::from_secs_f64(total / (r as f64)))
    }

    pub fn perf_clean_build(
        &mut self,
        profile: BuildProfile,
        samples: u32,
    ) -> anyhow::Result<Perf> {
        let args = match profile {
            BuildProfile::Debug => "build",
            BuildProfile::Release => "build --release",
            BuildProfile::ReleaseLto => "build --profile release-lto",
            BuildProfile::ReleaseLtoNative => "build --profile release-lto",
        };
        let output_path = self.crate_directory.join("perf.output.json");
        let o = output_path.to_string_lossy();
        let target_dir = self.crate_directory.join("./target");
        let rustflags = if profile == BuildProfile::ReleaseLtoNative {
            "-C target-cpu=native"
        } else {
            ""
        };
        let mut perfs = Vec::new();
        for _ in 0..samples {
            std::process::Command::new("cargo")
                .arg("clean")
                .env("CARGO_TARGET_DIR", target_dir.as_os_str())
                .current_dir(&self.crate_directory)
                .status()?;

            std::process::Command::new("/bin/sh")
                .arg("-c")
                .arg(format!(
                    "perf stat -j -e duration_time,instructions,cycles,task-clock -o \"{o}\" -- cargo {args}"
                ))
                .env("RUSTFLAGS", rustflags)
                .env("CARGO_TARGET_DIR", target_dir.as_os_str())
                .current_dir(&self.crate_directory)
                .status()?;

            let output = std::fs::read_to_string(&output_path)?;
            let mut perf = Perf {
                instructions: Default::default(),
                cycles: Default::default(),
                task_clock: Default::default(),
                duration: Default::default(),
                build_size: None,
            };
            for line in output.lines() {
                let counter = jsony::from_json::<Counter>(line)?;
                match counter.event.strip_suffix(":u").unwrap_or(&counter.event) {
                    "instructions" => perf.instructions = counter,
                    "cycles" => perf.cycles = counter,
                    "task-clock" => perf.task_clock = counter,
                    "duration_time" => perf.duration = counter,
                    _ => {}
                }
            }
            perfs.push(perf);
        }
        Ok(Perf::average(&perfs))
    }

    /// Build for the given profile, strip the binary, and return the exe path and stripped size.
    pub fn build_and_strip(&self, profile: BuildProfile) -> anyhow::Result<(PathBuf, u64)> {
        let args: &[&str] = match profile {
            BuildProfile::Debug => &["build"],
            BuildProfile::Release => &["build", "--release"],
            BuildProfile::ReleaseLto => &["build", "--profile", "release-lto"],
            BuildProfile::ReleaseLtoNative => &["build", "--profile", "release-lto"],
        };
        let status = std::process::Command::new("cargo")
            .args(args)
            .env(
                "RUSTFLAGS",
                if profile == BuildProfile::ReleaseLtoNative {
                    "-C target-cpu=native"
                } else {
                    ""
                },
            )
            .env(
                "CARGO_TARGET_DIR",
                self.crate_directory.join("./target").as_os_str(),
            )
            .stderr(Stdio::inherit())
            .stdout(Stdio::null())
            .current_dir(&self.crate_directory)
            .status()?;
        if !status.success() {
            bail!("Non-success exist code: {status:?}")
        }
        let exe =
            self.crate_directory
                .join(format!("./target/{}/{}", profile.dir_name(), self.name));

        std::process::Command::new("/bin/strip")
            .arg(&exe)
            .status()?;
        let exe_size = std::fs::metadata(&exe)?.len();
        println!(
            "{: <32} {} Binary Size: {} KB",
            self.name,
            profile.dir_name(),
            exe_size / 1024
        );
        Ok((exe, exe_size))
    }

    /// Run perf stat on an already-built executable, returning Perf with build_size set.
    pub fn perf_stat_runtime(
        &self,
        exe: &Path,
        build_size: u64,
        command: &str,
        input: &[u8],
    ) -> anyhow::Result<Perf> {
        let e = exe.to_string_lossy();
        let output = self.crate_directory.join("perf.output.json");
        let o = output.to_string_lossy();
        let mut proc = std::process::Command::new("/bin/sh")
            .arg("-c")
            .arg(format!(
                "perf stat -j -e duration_time,instructions,cycles,task-clock -o \"{o}\" -- {e} {command}"
            ))
            .stdin(Stdio::piped())
            .current_dir(&self.crate_directory)
            .spawn()?;
        {
            let mut stdin = proc.stdin.take().unwrap();
            stdin.write_all(input)?;
        }
        let status = proc.wait()?;
        if !status.success() {
            bail!("Runtime benchmark failed (exit code: {:?})", status.code());
        }

        let output = std::fs::read_to_string(output)?;
        let mut perf = Perf {
            instructions: Default::default(),
            cycles: Default::default(),
            task_clock: Default::default(),
            duration: Default::default(),
            build_size: Some(build_size),
        };
        for line in output.lines() {
            let Ok(counter) = jsony::from_json::<Counter>(line) else {
                continue;
            };
            match counter.event.strip_suffix(":u").unwrap_or(&counter.event) {
                "instructions" => perf.instructions = counter,
                "cycles" => perf.cycles = counter,
                "task-clock" => perf.task_clock = counter,
                "duration_time" => perf.duration = counter,
                _ => {}
            }
        }
        Ok(perf)
    }

    pub fn perf_run_time(
        &mut self,
        profile: BuildProfile,
        command: String,
        input: &[u8],
    ) -> anyhow::Result<Perf> {
        let (exe, exe_size) = self.build_and_strip(profile)?;
        if self.name.starts_with("baseline") {
            return Ok(Perf {
                instructions: Default::default(),
                cycles: Default::default(),
                task_clock: Default::default(),
                duration: Default::default(),
                build_size: Some(exe_size),
            });
        }
        self.perf_stat_runtime(&exe, exe_size, &command, input)
    }

    fn perf_stat_once(&self, command: &str) -> anyhow::Result<Perf> {
        let envs = &self.command.envs;
        let output_path = self.crate_directory.join("perf.output.json");
        let o = output_path.to_string_lossy();
        let status = std::process::Command::new("/bin/sh")
            .arg("-c")
            .arg(format!(
                "{envs} perf stat -j -e duration_time,instructions,cycles,task-clock -o \"{o}\" -- {command}"
            ))
            .current_dir(&self.crate_directory)
            .status()?;
        if !status.success() {
            bail!("Compilation failed (exit code: {:?})", status.code());
        }
        let output = std::fs::read_to_string(&output_path)?;
        let mut perf = Perf {
            instructions: Default::default(),
            cycles: Default::default(),
            task_clock: Default::default(),
            duration: Default::default(),
            build_size: None,
        };
        for line in output.lines() {
            let Ok(counter) = jsony::from_json::<Counter>(line) else {
                continue;
            };
            match counter.event.strip_suffix(":u").unwrap_or(&counter.event) {
                "instructions" => perf.instructions = counter,
                "cycles" => perf.cycles = counter,
                "task-clock" => perf.task_clock = counter,
                "duration_time" => perf.duration = counter,
                _ => {}
            }
        }
        Ok(perf)
    }

    fn apply_modification(&mut self, inc: Incremental) -> anyhow::Result<()> {
        let target = self.crate_directory.join(&self.modification_target);
        match inc {
            Incremental::Disabled => {}
            Incremental::Unchanged | Incremental::Touch => {
                let contents = std::fs::read(&target)?;
                std::fs::write(&target, contents)?;
            }
            Incremental::Postfix | Incremental::Prefix => {
                self.modification_counter += 1;
                let n = self.modification_counter;
                let new_code = format!(
                    "\n#[allow(dead_code)]\nstruct _Pad{n} {{ _a: i32, _b: Vec<String>, _c: Option<u64> }}\n"
                );
                if inc == Incremental::Postfix {
                    let mut file = std::fs::OpenOptions::new().append(true).open(&target)?;
                    file.write_all(new_code.as_bytes())?;
                } else {
                    let contents = std::fs::read_to_string(&target)?;
                    let bytes = contents.as_bytes();
                    // Insert after any leading #![...] inner attributes so they stay at the top.
                    // We can't just use line-based detection because the formatter may put
                    // #![...] and other code on the same line.
                    let mut insert_pos = 0;
                    loop {
                        let mut i = insert_pos;
                        while i < bytes.len() && bytes[i].is_ascii_whitespace() {
                            i += 1;
                        }
                        if i + 2 < bytes.len()
                            && bytes[i] == b'#'
                            && bytes[i + 1] == b'!'
                            && bytes[i + 2] == b'['
                        {
                            let mut depth = 0;
                            let mut j = i + 2;
                            while j < bytes.len() {
                                if bytes[j] == b'[' {
                                    depth += 1;
                                } else if bytes[j] == b']' {
                                    depth -= 1;
                                    if depth == 0 {
                                        insert_pos = j + 1;
                                        break;
                                    }
                                }
                                j += 1;
                            }
                            if depth != 0 {
                                break;
                            }
                        } else {
                            break;
                        }
                    }
                    let mut new_contents = Vec::new();
                    new_contents.extend_from_slice(&bytes[..insert_pos]);
                    new_contents.extend_from_slice(new_code.as_bytes());
                    new_contents.extend_from_slice(&bytes[insert_pos..]);
                    std::fs::write(&target, new_contents)?;
                }
            }
            Incremental::TypeTransform => {
                let src_dir = self.crate_directory.join("src");
                for entry in std::fs::read_dir(&src_dir)? {
                    let entry = entry?;
                    let path = entry.path();
                    if path.extension().and_then(|e| e.to_str()) != Some("rs") {
                        continue;
                    }
                    let contents = std::fs::read_to_string(&path)?;
                    if !contents.contains("u32") && !contents.contains("i32") {
                        continue;
                    }
                    let transformed = contents
                        .replace("u32", "__U32_SWAP__")
                        .replace("i32", "u32")
                        .replace("__U32_SWAP__", "i32");
                    std::fs::write(&path, transformed)?;
                }
            }
        }
        Ok(())
    }

    pub fn self_profile(
        &mut self,
        scenario: Scenario,
        output_dir: &Path,
    ) -> anyhow::Result<SelfProfileArtifacts> {
        let incremental = match scenario {
            Scenario::WarmBuild {
                incremental,
                profile,
            } => {
                self.rustc_from_cargo_toolchain(
                    match profile {
                        BuildProfile::Debug => CargoAction::DebugBuild,
                        BuildProfile::Release => CargoAction::ReleaseBuild,
                        BuildProfile::ReleaseLto => CargoAction::ReleaseBuild,
                        BuildProfile::ReleaseLtoNative => CargoAction::ReleaseBuild,
                    },
                    Some("nightly"),
                )?;
                incremental
            }
            Scenario::WarmCheck { incremental } => {
                self.rustc_from_cargo_toolchain(CargoAction::Check, Some("nightly"))?;
                incremental
            }
            _ => bail!("Self-profiling only supports warm-build and warm-check scenarios"),
        };

        // Do one warmup build first (without self-profile)
        let warmup_command = if incremental == Incremental::Disabled {
            self.command.base.clone()
        } else {
            format!(
                "{} -C incremental={}",
                self.command.base, self.command.incremental
            )
        };
        self.apply_modification(incremental)?;
        let envs = &self.command.envs;
        let output = std::process::Command::new("/bin/sh")
            .arg("-c")
            .arg(format!("{envs} {warmup_command}"))
            .stdout(Stdio::null())
            .stderr(Stdio::piped())
            .current_dir(&self.crate_directory)
            .output()?;
        if !output.status.success() {
            bail!(
                "Warmup compilation failed:\n{}{}",
                String::from_utf8_lossy(&output.stderr).trim(),
                String::from_utf8_lossy(&output.stdout),
            );
        }

        // Now the actual self-profile run
        self.apply_modification(incremental)?;
        let profile_dir = output_dir.join(&self.name);
        std::fs::create_dir_all(&profile_dir)?;

        let command = if incremental == Incremental::Disabled {
            self.command.base.clone()
        } else {
            format!(
                "{} -C incremental={}",
                self.command.base, self.command.incremental
            )
        };

        let envs = &self.command.envs;
        let profile_out_str = profile_dir.to_string_lossy();
        let proc = std::process::Command::new("/bin/sh")
            .arg("-c")
            .arg(format!(
                "{envs} {command} -Zself-profile={profile_out_str} -Zself-profile-events=default,args"
            ))
            .stdout(Stdio::null())
            .stderr(Stdio::piped())
            .current_dir(&self.crate_directory)
            .spawn()?;
        let pid = proc.id();
        let output = proc.wait_with_output()?;
        if !output.status.success() {
            bail!(
                "Self-profile compilation failed:\n{}{}",
                String::from_utf8_lossy(&output.stderr).trim(),
                String::from_utf8_lossy(&output.stdout),
            );
        }

        // Find the actual profile file: crate_name-{PID}.mm_profdata
        let profile_prefix = profile_dir.join(format!("{}-{pid:07}", self.name));
        let summary_json = profile_prefix.with_extension("json");

        // Run summarize
        let status = std::process::Command::new("rustc-summarize")
            .arg("summarize")
            .arg("--json")
            .arg(&profile_prefix)
            .current_dir(&self.crate_directory)
            .status()?;
        if !status.success() {
            bail!("rustc-summarize failed");
        }

        // Also run summarize to stdout for immediate viewing
        println!("\n=== Self-Profile Summary for {} ===", self.name);
        let _ = std::process::Command::new("rustc-summarize")
            .arg("summarize")
            .arg("-p")
            .arg("1.0")
            .arg(&profile_prefix)
            .current_dir(&self.crate_directory)
            .status();

        // Generate chrome trace
        let _ = std::process::Command::new("rustc-crox")
            .arg(&profile_prefix)
            .current_dir(&profile_dir)
            .status();

        Ok(SelfProfileArtifacts {
            chrome_trace: profile_dir.join("chrome_profiler.json"),
            profile_dir,
            profile_prefix,
            summary_json,
        })
    }

    pub fn bench(&mut self, r: u32, scenario: Scenario) -> Result<Perf, anyhow::Error> {
        let incremental = match scenario {
            Scenario::WarmBuild {
                incremental,
                profile,
            } => {
                self.rustc_from_cargo(match profile {
                    BuildProfile::Debug => CargoAction::DebugBuild,
                    BuildProfile::Release => CargoAction::ReleaseBuild,
                    BuildProfile::ReleaseLto => CargoAction::ReleaseBuild,
                    BuildProfile::ReleaseLtoNative => CargoAction::ReleaseBuild,
                })?;
                incremental
            }
            Scenario::WarmCheck { incremental } => {
                self.rustc_from_cargo(CargoAction::Check)?;
                incremental
            }
            Scenario::CleanBuild { profile } => return self.perf_clean_build(profile, r),
            _ => {
                panic!("Unsupported scenario: {scenario:?}");
            }
        };
        let command = if incremental == Incremental::Disabled {
            self.command.base.clone()
        } else {
            format!(
                "{} -C incremental={}",
                self.command.base, self.command.incremental
            )
        };
        let mut perfs = Vec::new();
        for _ in 0..r {
            self.apply_modification(incremental)?;
            perfs.push(self.perf_stat_once(&command)?);
        }
        Ok(Perf::average(&perfs))
    }
}
