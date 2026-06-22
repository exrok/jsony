use std::{
    collections::{BTreeMap, BTreeSet},
    fmt::Display,
    fs::File,
    hint::black_box,
    io::{self, Read, Write},
    path::{Path, PathBuf},
    time::{Duration, SystemTime, UNIX_EPOCH},
};

use jsony_macros::Jsony;
use perf_event::{Builder, Counter, Group, GroupData, events::Hardware};

pub type RouteId = String;

#[cfg(target_os = "linux")]
mod hygiene {
    use std::{ffi::CString, hint::black_box, time::Instant};

    use super::CPU_WARMUP_DURATION;

    /// Pin to a stable CPU, raise scheduling priority, lock memory pages, and
    /// drive the CPU to boost. If ASLR is enabled and the binary supports it,
    /// re-exec with `ADDR_NO_RANDOMIZE` so layout-dependent cache behaviour is
    /// reproducible between processes. Failures are silently ignored.
    pub fn prepare_process() {
        try_disable_aslr_and_reexec();
        pin_cpu();
        try_raise_priority();
        try_lock_memory();
        cpu_frequency_warmup();
    }

    fn try_disable_aslr_and_reexec() {
        if std::env::var_os("JSONY_BENCH_NO_REEXEC").is_some() {
            return;
        }

        let aslr_bit = libc::ADDR_NO_RANDOMIZE as libc::c_ulong;

        // SAFETY: personality(0xffffffff) reads the current personality, no
        // side effects.
        let current = unsafe { libc::personality(0xffff_ffff) };
        if current < 0 || (current as libc::c_ulong) & aslr_bit != 0 {
            return;
        }

        // SAFETY: personality with the new bit set affects only this process.
        let next = (current as libc::c_ulong) | aslr_bit;
        if unsafe { libc::personality(next) } < 0 {
            return;
        }

        // Re-exec /proc/self/exe with the current argv. Setting an env marker
        // prevents an infinite re-exec loop.
        let argv = std::env::args().collect::<Vec<_>>();
        let cstrings = argv
            .iter()
            .filter_map(|arg| CString::new(arg.as_str()).ok())
            .collect::<Vec<_>>();
        if cstrings.len() != argv.len() {
            return;
        }
        let mut ptrs = cstrings
            .iter()
            .map(|cstr| cstr.as_ptr())
            .collect::<Vec<_>>();
        ptrs.push(std::ptr::null());

        // SAFETY: setenv with valid strings.
        let key = CString::new("JSONY_BENCH_NO_REEXEC").unwrap();
        let value = CString::new("1").unwrap();
        unsafe {
            libc::setenv(key.as_ptr(), value.as_ptr(), 1);
        }

        let exe = match CString::new("/proc/self/exe") {
            Ok(value) => value,
            Err(_) => return,
        };

        // SAFETY: execv replaces the process image if it succeeds, otherwise
        // returns -1 and we fall through to continue without ASLR-off.
        unsafe {
            libc::execv(exe.as_ptr(), ptrs.as_ptr());
        }
    }

    fn pin_cpu() {
        let cpu = match std::env::var("JSONY_BENCH_CPU") {
            Ok(value) => value.parse::<i32>().ok(),
            Err(_) => None,
        }
        // SAFETY: sched_getcpu has no preconditions.
        .or_else(|| {
            let current = unsafe { libc::sched_getcpu() };
            if current >= 0 { Some(current) } else { None }
        });

        let Some(cpu) = cpu else {
            return;
        };
        if cpu < 0 {
            return;
        }

        // SAFETY: zero-initialize cpu_set_t then set the chosen CPU bit and
        // call sched_setaffinity on the calling thread.
        unsafe {
            let mut set: libc::cpu_set_t = std::mem::zeroed();
            libc::CPU_SET(cpu as usize, &mut set);
            libc::sched_setaffinity(0, std::mem::size_of::<libc::cpu_set_t>(), &set);
        }
    }

    fn try_raise_priority() {
        // SAFETY: setpriority on the calling process is safe; result is ignored.
        unsafe {
            libc::setpriority(libc::PRIO_PROCESS, 0, -10);
        }
    }

    fn try_lock_memory() {
        // SAFETY: mlockall has no preconditions. EPERM is silently ignored.
        unsafe {
            libc::mlockall(libc::MCL_CURRENT | libc::MCL_FUTURE);
        }
    }

    fn cpu_frequency_warmup() {
        let start = Instant::now();
        let mut acc = 0u64;
        while start.elapsed() < CPU_WARMUP_DURATION {
            for index in 0..16384u64 {
                acc = acc.wrapping_add(black_box(index).wrapping_mul(0x9E37_79B9_7F4A_7C15));
            }
            black_box(acc);
        }
    }
}

#[cfg(not(target_os = "linux"))]
mod hygiene {
    pub fn prepare_process() {}
}

const RUN_FILE_FORMAT: &str = "jsony_bench.run";
const RUN_FILE_VERSION: u32 = 1;
const RUN_PLAN_FORMAT: &str = "jsony_bench.plan";
const RUN_PLAN_VERSION: u32 = 1;
const DEFAULT_PROFILE_ITERATIONS: u64 = 1_000;

// Trimmed-log-mean trim fraction per tail.
const TRIM_FRACTION: f64 = 0.10;

// Number of contiguous blocks used by the block-SE estimator.
const SE_BLOCK_COUNT: usize = 8;

// Each block must have at least this many samples for blocking to apply.
const SE_MIN_SAMPLES_PER_BLOCK: usize = 8;

// Default classification threshold and required posterior confidence.
const DEFAULT_THRESHOLD_PCT: f64 = 1.0;
const POSTERIOR_CONFIDENCE: f64 = 0.99;
const GROUNDING_CONFIDENCE: f64 = 0.90;
const CI_95_Z: f64 = 1.959_963_984_540_054;

// Minimum samples required on each side for a comparison to be considered.
const COMPARE_MIN_SAMPLES: usize = 60;

// Number of adjustment-only samples taken before the measurement loop in the
// generator path.
const ADJUSTMENT_SAMPLES: u64 = 24;

// Samples discarded after calibration to flush branch predictor / cache / TLB
// state and let CPU frequency reach its steady boost.
const BURN_IN_SAMPLES: usize = 16;

// Calibration spends at most this many iterations doubling.
const CALIBRATION_MAX_PASSES: usize = 8;

// Process-startup CPU warmup spin duration before any sample is taken.
const CPU_WARMUP_DURATION: Duration = Duration::from_millis(500);

#[derive(Default)]
pub struct Router {
    routes: Vec<RouteEntry>,
}

impl Router {
    pub fn add<F>(&mut self, route: impl Into<String>, func: F) -> &mut Self
    where
        F: Fn(&mut Bench<'_>) + 'static,
    {
        let route = route.into();
        validate_route_segment(&route);
        assert!(
            self.routes.iter().all(|entry| entry.route != route),
            "duplicate benchmark route {route:?}"
        );
        self.routes.push(RouteEntry {
            route,
            func: Box::new(func),
        });
        self
    }

    pub fn eval_from_env(&self) -> ! {
        let outcome = self.eval(std::env::args().skip(1));
        if !outcome.stdout.is_empty() {
            print!("{}", outcome.stdout);
        }
        if !outcome.stderr.is_empty() {
            eprint!("{}", outcome.stderr);
        }
        std::process::exit(outcome.exit_code);
    }

    pub fn eval(&self, args: impl IntoIterator<Item = String>) -> EvalOutcome {
        self.eval_inner(args, BenchBackend::live)
    }

    fn eval_inner(
        &self,
        args: impl IntoIterator<Item = String>,
        backend: impl FnOnce() -> BenchBackend,
    ) -> EvalOutcome {
        let args = args.into_iter().collect::<Vec<_>>();
        let options = match CliOptions::parse(&args) {
            Ok(options) => options,
            Err(err) => {
                return EvalOutcome::error(2, format!("{err}\n\n{}", usage()));
            }
        };

        if options.help {
            return EvalOutcome {
                run_file: None,
                compare_report: None,
                stdout: usage(),
                stderr: String::new(),
                exit_code: 0,
            };
        }

        if let Some(route_filter) = &options.route_filter
            && !self.has_top_route(route_filter)
        {
            return EvalOutcome::error(
                2,
                format!(
                    "unknown benchmark route {route_filter:?}\nknown routes:\n{}",
                    self.known_routes()
                ),
            );
        }

        let backend = match options.mode {
            CliMode::Bench => backend(),
            CliMode::Profile => BenchBackend::profile(options.profile_iterations),
        };

        if options.mode == CliMode::Profile {
            let (_backend, records) = self.measure_records(&options, None, backend);
            let run_file = self.run_file_from_records(&args, &options, records, None);

            if run_file.records.is_empty() {
                return EvalOutcome::error(
                    2,
                    "no benchmarks matched the selected route and parameter filters\n".to_owned(),
                );
            }

            return EvalOutcome {
                run_file: Some(run_file),
                compare_report: None,
                stdout: String::new(),
                stderr: String::new(),
                exit_code: 0,
            };
        }

        let compare_run_plan = options
            .compare
            .as_ref()
            .map(|path| load_run_plan_for_compare(path));

        let (mut run_file, comparison_run_plan) = if options.save.is_some()
            && options.compare.is_none()
        {
            let (next_backend, prepass_records) = self.measure_records(&options, None, backend);
            let prepass_run = self.run_file_from_records(&args, &options, prepass_records, None);

            if prepass_run.records.is_empty() {
                return EvalOutcome::error(
                    2,
                    "no benchmarks matched the selected route and parameter filters\n".to_owned(),
                );
            }

            let phase1_plan = run_plan_from_run_file(&prepass_run);
            let (_backend, locked_records) =
                self.measure_records(&options, Some(phase1_plan), next_backend);
            let mut locked_run = self.run_file_from_records(&args, &options, locked_records, None);
            locked_run.plan = Some(run_plan_from_run_pair(&prepass_run, &locked_run));
            (locked_run, None)
        } else if let Some(compare_plan) = compare_run_plan.clone() {
            let (next_backend, prepass_records) =
                self.measure_records(&options, Some(compare_plan.clone()), backend);
            let prepass_run = self.run_file_from_records(&args, &options, prepass_records, None);

            if prepass_run.records.is_empty() {
                return EvalOutcome::error(
                    2,
                    "no benchmarks matched the selected route and parameter filters\n".to_owned(),
                );
            }

            let (_backend, records) =
                self.measure_records(&options, Some(compare_plan.clone()), next_backend);
            let mut run = self.run_file_from_records(&args, &options, records, None);
            let current_noise_plan = run_plan_from_run_pair(&prepass_run, &run);
            let comparison_plan = combine_run_plan_noise(&compare_plan, &current_noise_plan);
            run.plan = Some(current_noise_plan);
            (run, Some(comparison_plan))
        } else {
            let (_backend, records) =
                self.measure_records(&options, compare_run_plan.clone(), backend);
            (
                self.run_file_from_records(&args, &options, records, None),
                None,
            )
        };

        if run_file.records.is_empty() {
            return EvalOutcome::error(
                2,
                "no benchmarks matched the selected route and parameter filters\n".to_owned(),
            );
        }

        // Load the baseline only after the current run is measured. Run files
        // include all raw samples and can be large enough to perturb allocator
        // and cache state for allocation-heavy benchmarks.
        let compare_report = options.compare.as_ref().map(|path| {
            let baseline = load_run_file(path);
            compare_runs_with_threshold_and_plan(
                &baseline,
                &run_file,
                options.threshold_pct,
                comparison_run_plan.as_ref().or(compare_run_plan.as_ref()),
            )
        });

        let stdout = if let Some(compare_report) = &compare_report {
            render_compare_report(compare_report)
        } else {
            render_run_table(&run_file)
        };

        if let Some(path) = &options.save {
            if run_file.plan.is_none() {
                run_file.plan = Some(run_plan_from_run_file_with_noise_source(
                    &run_file,
                    compare_run_plan.as_ref(),
                ));
            }
            save_run_file(path, &run_file);
        }

        EvalOutcome {
            run_file: Some(run_file),
            compare_report,
            stdout,
            stderr: String::new(),
            exit_code: 0,
        }
    }

    fn has_top_route(&self, route_filter: &str) -> bool {
        let top = route_filter.split('/').next().unwrap_or_default();
        self.routes.iter().any(|entry| entry.route == top)
    }

    fn known_routes(&self) -> String {
        if self.routes.is_empty() {
            return "  <none>\n".to_owned();
        }

        let mut output = String::new();
        for entry in &self.routes {
            output.push_str("  ");
            output.push_str(&entry.route);
            output.push('\n');
        }
        output
    }

    fn measure_records(
        &self,
        options: &CliOptions,
        run_plan: Option<RunPlan>,
        backend: BenchBackend,
    ) -> (BenchBackend, Vec<RunRecord>) {
        let mut context = RunContext {
            backend,
            route_filter: options.route_filter.clone(),
            param_filters: options.param_filters.clone(),
            run_plan,
            progress: options.progress,
            records: Vec::new(),
            record_keys: BTreeSet::new(),
        };

        for entry in &self.routes {
            if options
                .route_filter
                .as_deref()
                .is_none_or(|filter| route_filter_targets_top(filter, &entry.route))
            {
                let mut bench = Bench::new(&mut context, vec![entry.route.clone()]);
                (entry.func)(&mut bench);
            }
        }

        (context.backend, context.records)
    }

    fn run_file_from_records(
        &self,
        args: &[String],
        options: &CliOptions,
        records: Vec<RunRecord>,
        plan: Option<RunPlan>,
    ) -> RunFile {
        RunFile {
            plan,
            format: RUN_FILE_FORMAT.to_owned(),
            version: RUN_FILE_VERSION,
            command: args.to_vec(),
            route_filter: options.route_filter.clone(),
            param_filters: options.param_filters.clone(),
            created_unix_ms: unix_now_ms(),
            records,
        }
    }
}

struct RouteEntry {
    route: String,
    func: Box<dyn Fn(&mut Bench<'_>)>,
}

pub struct Bench<'a> {
    context: &'a mut RunContext,
    path: Vec<String>,
    params: Vec<ParamValue>,
    bench_parameters: BenchParameters,
    profile_iterations: Option<u64>,
}

impl<'a> Bench<'a> {
    fn new(context: &'a mut RunContext, path: Vec<String>) -> Self {
        Self {
            context,
            path,
            params: Vec::new(),
            bench_parameters: DEFAULT_BENCH_PARAM,
            profile_iterations: None,
        }
    }

    pub fn func(&mut self, mut func: impl FnMut()) -> &mut Self {
        if self.context.should_measure(&self.path, &self.params) {
            let plan = self.context.measurement_plan(&self.path, &self.params);
            self.context.report_measure_start(&self.path, &self.params);
            let started = std::time::Instant::now();
            let report = self.context.backend.measure_func(
                &self.bench_parameters,
                plan,
                self.default_profile_iterations(),
                &mut func,
            );
            self.context.report_measure_finish(
                &self.path,
                &self.params,
                started.elapsed(),
                &report,
            );
            self.context.push_record(RunRecord {
                route: self.path_string(),
                params: self.params.clone(),
                report,
                bench_parameters: self.bench_parameters,
            });
        }
        self
    }

    /// Run a benchmark whose body is indexed by an iteration counter that
    /// restarts at zero for each sample. Use this for corpus-cycling
    /// benchmarks. The work per sample is deterministic for a given iteration
    /// count, so save and compare see identical input sequences.
    pub fn indexed<F>(&mut self, mut body: F) -> &mut Self
    where
        F: FnMut(u64),
    {
        self.indexed_with_iteration_multiple(1, &mut body)
    }

    /// Run an indexed benchmark where each sample is rounded to a multiple of
    /// `cycle_len`. Use this when the body cycles through a finite corpus with
    /// `i % cycle_len`, so every sample sees the same input distribution after
    /// independent calibration.
    pub fn indexed_cyclic<F>(&mut self, cycle_len: usize, mut body: F) -> &mut Self
    where
        F: FnMut(u64),
    {
        assert!(cycle_len > 0, "indexed_cyclic cycle_len must be non-zero");
        self.indexed_with_iteration_multiple(cycle_len as u64, &mut body)
    }

    fn indexed_with_iteration_multiple(
        &mut self,
        iteration_multiple: u64,
        body: &mut dyn FnMut(u64),
    ) -> &mut Self {
        if self.context.should_measure(&self.path, &self.params) {
            let plan = self.context.measurement_plan(&self.path, &self.params);
            self.context.report_measure_start(&self.path, &self.params);
            let started = std::time::Instant::now();
            let report = self.context.backend.measure_indexed(
                &self.bench_parameters,
                plan,
                self.default_profile_iterations(),
                iteration_multiple,
                body,
            );
            self.context.report_measure_finish(
                &self.path,
                &self.params,
                started.elapsed(),
                &report,
            );
            self.context.push_record(RunRecord {
                route: self.path_string(),
                params: self.params.clone(),
                report,
                bench_parameters: self.bench_parameters,
            });
        }
        self
    }

    pub fn func_with_generator<T>(
        &mut self,
        generator: impl FnMut() -> T,
        func: impl FnMut(T),
    ) -> &mut Self {
        if self.context.should_measure(&self.path, &self.params) {
            let plan = self.context.measurement_plan(&self.path, &self.params);
            self.context.report_measure_start(&self.path, &self.params);
            let started = std::time::Instant::now();
            let report = self.context.backend.measure_generated(
                &self.bench_parameters,
                plan,
                self.default_profile_iterations(),
                generator,
                func,
            );
            self.context.report_measure_finish(
                &self.path,
                &self.params,
                started.elapsed(),
                &report,
            );
            self.context.push_record(RunRecord {
                route: self.path_string(),
                params: self.params.clone(),
                report,
                bench_parameters: self.bench_parameters,
            });
        }
        self
    }

    pub fn generated<T>(&mut self, generator: impl FnMut() -> T, func: impl FnMut(T)) -> &mut Self {
        self.func_with_generator(generator, func)
    }

    pub fn sub(&mut self, route: impl Into<String>) -> Bench<'_> {
        self.child_with_segment(route.into())
    }

    pub fn named(&mut self, route: impl Into<String>) -> Bench<'_> {
        self.child_with_segment(route.into())
    }

    pub fn with_parameters(&mut self, bench_parameters: BenchParameters) -> Bench<'_> {
        Bench {
            context: self.context,
            path: self.path.clone(),
            params: self.params.clone(),
            bench_parameters,
            profile_iterations: self.profile_iterations,
        }
    }

    /// Set the default iteration count used by the `profile` subcommand for
    /// this route and descendants. A CLI `--iterations` value still overrides
    /// this route default.
    pub fn with_profile_iterations(&mut self, iterations: u64) -> Bench<'_> {
        assert!(
            iterations > 0,
            "profile iterations must be greater than zero"
        );
        Bench {
            context: self.context,
            path: self.path.clone(),
            params: self.params.clone(),
            bench_parameters: self.bench_parameters,
            profile_iterations: Some(iterations),
        }
    }

    pub fn param<I, T, F>(&mut self, name: impl Into<String>, values: I, mut func: F) -> &mut Self
    where
        I: IntoIterator<Item = T>,
        T: Display,
        F: FnMut(&mut Bench<'_>, T),
    {
        let name = name.into();
        validate_param_name(&name);
        assert!(
            self.params.iter().all(|param| param.name != name),
            "duplicate benchmark parameter {name:?} on route {:?}",
            self.path_string()
        );

        for value in values {
            let value_text = value.to_string();
            if self.context.param_value_possible(&name, &value_text, None) {
                let mut child = Bench {
                    context: self.context,
                    path: self.path.clone(),
                    params: pushed_param(&self.params, &name, &value_text),
                    bench_parameters: self.bench_parameters,
                    profile_iterations: self.profile_iterations,
                };
                func(&mut child, value);
            }
        }
        self
    }

    pub fn param_str<I, S, F>(
        &mut self,
        name: impl Into<String>,
        values: I,
        mut func: F,
    ) -> &mut Self
    where
        I: IntoIterator<Item = S>,
        S: AsRef<str>,
        F: FnMut(&mut Bench<'_>, String),
    {
        self.param(
            name,
            values.into_iter().map(|value| value.as_ref().to_owned()),
            |bench, value| {
                func(bench, value);
            },
        )
    }

    /// Like [`Bench::param_str`], but in profile mode, when the user did not
    /// pass a filter for this parameter, only the supplied representative values
    /// are expanded. Bench mode and explicit `--param name=value` filters still
    /// use the full value list.
    pub fn param_str_profile_defaults<I, S, D, P, F>(
        &mut self,
        name: impl Into<String>,
        values: I,
        profile_defaults: D,
        mut func: F,
    ) -> &mut Self
    where
        I: IntoIterator<Item = S>,
        S: AsRef<str>,
        D: IntoIterator<Item = P>,
        P: AsRef<str>,
        F: FnMut(&mut Bench<'_>, String),
    {
        let profile_defaults = profile_defaults
            .into_iter()
            .map(|value| value.as_ref().to_owned())
            .collect::<BTreeSet<_>>();
        assert!(
            !profile_defaults.is_empty(),
            "profile default parameter values must not be empty"
        );
        self.param_str_inner(name, values, Some(&profile_defaults), |bench, value| {
            func(bench, value);
        })
    }

    pub fn items<I, T, F>(&mut self, name: impl Into<String>, values: I, func: F) -> &mut Self
    where
        I: IntoIterator<Item = T>,
        T: Display,
        F: FnMut(&mut Bench<'_>, T),
    {
        self.param(name, values, func)
    }

    fn child_with_segment(&mut self, segment: String) -> Bench<'_> {
        validate_route_segment(&segment);
        let mut path = self.path.clone();
        path.push(segment);
        Bench {
            context: self.context,
            path,
            params: self.params.clone(),
            bench_parameters: self.bench_parameters,
            profile_iterations: self.profile_iterations,
        }
    }

    fn path_string(&self) -> String {
        self.path.join("/")
    }

    fn default_profile_iterations(&self) -> u64 {
        self.profile_iterations
            .unwrap_or(DEFAULT_PROFILE_ITERATIONS)
    }

    fn param_str_inner<I, S, F>(
        &mut self,
        name: impl Into<String>,
        values: I,
        profile_defaults: Option<&BTreeSet<String>>,
        mut func: F,
    ) -> &mut Self
    where
        I: IntoIterator<Item = S>,
        S: AsRef<str>,
        F: FnMut(&mut Bench<'_>, String),
    {
        let name = name.into();
        validate_param_name(&name);
        assert!(
            self.params.iter().all(|param| param.name != name),
            "duplicate benchmark parameter {name:?} on route {:?}",
            self.path_string()
        );

        for value in values {
            let value_text = value.as_ref().to_owned();
            if self
                .context
                .param_value_possible(&name, &value_text, profile_defaults)
            {
                let mut child = Bench {
                    context: self.context,
                    path: self.path.clone(),
                    params: pushed_param(&self.params, &name, &value_text),
                    bench_parameters: self.bench_parameters,
                    profile_iterations: self.profile_iterations,
                };
                func(&mut child, value_text);
            }
        }
        self
    }
}

#[derive(Debug)]
pub struct EvalOutcome {
    pub run_file: Option<RunFile>,
    pub compare_report: Option<CompareReport>,
    pub stdout: String,
    pub stderr: String,
    pub exit_code: i32,
}

impl EvalOutcome {
    fn error(exit_code: i32, stderr: String) -> Self {
        Self {
            run_file: None,
            compare_report: None,
            stdout: String::new(),
            stderr,
            exit_code,
        }
    }
}

#[must_use]
#[derive(Jsony, Debug, Clone, PartialEq, Eq)]
#[jsony(Json)]
pub struct ParamValue {
    pub name: String,
    pub value: String,
}

impl ParamValue {
    pub fn new(name: impl Into<String>, value: impl Into<String>) -> Self {
        let name = name.into();
        validate_param_name(&name);
        Self {
            name,
            value: value.into(),
        }
    }
}

#[must_use]
#[derive(Jsony, Debug, Clone, PartialEq)]
#[jsony(Json)]
pub struct RunRecord {
    pub route: RouteId,
    pub params: Vec<ParamValue>,
    pub report: BenchReport,
    pub bench_parameters: BenchParameters,
}

#[must_use]
#[derive(Jsony, Debug, Clone, PartialEq)]
#[jsony(Json)]
pub struct RunFile {
    #[jsony(default)]
    pub plan: Option<RunPlan>,
    pub format: String,
    pub version: u32,
    pub command: Vec<String>,
    pub route_filter: Option<String>,
    pub param_filters: Vec<ParamValue>,
    pub created_unix_ms: u64,
    pub records: Vec<RunRecord>,
}

#[must_use]
#[derive(Jsony, Debug, Clone, PartialEq)]
#[jsony(Json)]
pub struct RunPlan {
    pub format: String,
    pub version: u32,
    pub records: Vec<RunPlanRecord>,
}

#[must_use]
#[derive(Jsony, Debug, Clone, PartialEq)]
#[jsony(Json)]
pub struct RunPlanRecord {
    pub route: RouteId,
    pub params: Vec<ParamValue>,
    pub iterations_per_sample: u64,
    pub samples: usize,
    #[jsony(default)]
    pub cycles_noise_log: f64,
    #[jsony(default)]
    pub inst_noise_log: f64,
}

#[derive(Clone, Copy, Debug, PartialEq)]
struct MeasurementPlan {
    iterations_per_sample: u64,
    samples: Option<usize>,
    cycles_noise_log: f64,
    inst_noise_log: f64,
}

impl MeasurementPlan {
    fn iterations_only(iterations_per_sample: u64) -> Self {
        Self {
            iterations_per_sample,
            samples: None,
            cycles_noise_log: 0.0,
            inst_noise_log: 0.0,
        }
    }

    fn from_record(record: &RunPlanRecord) -> Self {
        Self {
            iterations_per_sample: record.iterations_per_sample,
            samples: Some(record.samples),
            cycles_noise_log: sanitize_noise_log(record.cycles_noise_log),
            inst_noise_log: sanitize_noise_log(record.inst_noise_log),
        }
    }

    fn locked_samples(self, param: &BenchParameters) -> Option<usize> {
        self.samples
            .map(|samples| samples.max(param.min_samples).min(param.max_samples))
    }
}

#[must_use]
#[derive(Jsony, Debug, Clone, PartialEq)]
#[jsony(Json)]
pub struct CompareReport {
    pub entries: Vec<CompareEntry>,
    pub missing_baseline: Vec<RunRecord>,
    pub missing_current: Vec<RunRecord>,
    pub regression_count: usize,
    pub improvement_count: usize,
}

#[must_use]
#[derive(Jsony, Debug, Clone, PartialEq)]
#[jsony(Json)]
pub struct CompareEntry {
    pub route: RouteId,
    pub params: Vec<ParamValue>,
    pub baseline: BenchReport,
    pub current: BenchReport,
    /// Cycles-per-iteration: posterior mean of delta in percent, plus the
    /// 95% credible interval and posterior probabilities.
    pub cycles_delta_percent: Option<f64>,
    pub cycles_ci_lo_percent: Option<f64>,
    pub cycles_ci_hi_percent: Option<f64>,
    pub cycles_p_regression: Option<f64>,
    pub cycles_p_improvement: Option<f64>,
    /// Instructions-per-iteration: same fields, used as a grounding signal.
    pub inst_delta_percent: Option<f64>,
    pub inst_p_regression: Option<f64>,
    pub inst_p_improvement: Option<f64>,
    /// Wall-time delta, reported for context only.
    pub time_delta_percent: Option<f64>,
    pub classification: String,
    pub classification_reason: String,
    /// Threshold (in percent) used to derive the classification.
    pub threshold_percent: f64,
    /// Per-row confidence required after suite-level multiple-comparison correction.
    pub required_confidence: f64,
}

pub struct Bencher {
    perf_group: Group,
    cycle_counter: Counter,
    instruction_counter: Counter,
    branch_counter: Counter,
    baseline: Stat,
}

#[must_use]
#[derive(Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash, Default, Jsony)]
#[jsony(Json, transparent)]
#[repr(transparent)]
pub struct Dec(u64);

impl std::fmt::Display for Dec {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{:.2}", f64::from(*self))
    }
}

impl std::fmt::Debug for Dec {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{:.2}", f64::from(*self))
    }
}

impl Dec {
    const SCALE: u64 = 256;

    #[inline]
    pub fn u64_floor(self) -> u64 {
        self.0 / Self::SCALE
    }

    #[inline]
    pub fn abs_diff(self, other: Self) -> Self {
        Self(self.0.abs_diff(other.0))
    }

    #[inline]
    pub fn raw_fixed_point(self) -> u64 {
        self.0
    }

    #[inline]
    fn from_raw_fixed_point(value: u64) -> Self {
        Self(value)
    }

    #[inline]
    fn average(a: Self, b: Self) -> Self {
        Self(((a.0 as u128 + b.0 as u128) / 2).min(u64::MAX as u128) as u64)
    }
}

impl From<u64> for Dec {
    #[inline]
    fn from(value: u64) -> Self {
        Dec(value.saturating_mul(Self::SCALE))
    }
}

impl From<Dec> for f64 {
    #[inline]
    fn from(value: Dec) -> Self {
        value.0 as f64 / Dec::SCALE as f64
    }
}

impl std::ops::Add<Dec> for Dec {
    type Output = Dec;

    #[inline]
    fn add(self, rhs: Dec) -> Self::Output {
        Dec(self.0.saturating_add(rhs.0))
    }
}

impl std::ops::AddAssign<Dec> for Dec {
    #[inline]
    fn add_assign(&mut self, rhs: Dec) {
        self.0 = self.0.saturating_add(rhs.0);
    }
}

impl std::ops::Sub<Dec> for Dec {
    type Output = Dec;

    #[inline]
    fn sub(self, rhs: Dec) -> Self::Output {
        Dec(self.0.saturating_sub(rhs.0))
    }
}

impl std::ops::Div<u64> for Dec {
    type Output = Dec;

    #[inline]
    fn div(self, rhs: u64) -> Self::Output {
        Dec(self.0 / rhs)
    }
}

#[must_use]
#[derive(Jsony, Default, Debug, Clone, Copy, PartialEq, Eq)]
#[jsony(Json)]
pub struct Stat {
    pub nanos: Dec,
    pub cycles: Dec,
    pub inst: Dec,
    pub branch: Dec,
}

/// Standard error of `ln(metric)` for each tracked counter.
///
/// Stored as f64 because it is small (typically 1e-4..1e-1) and used purely
/// for posterior computations, not for cross-run accumulation.
#[must_use]
#[derive(Jsony, Default, Debug, Clone, Copy, PartialEq)]
#[jsony(Json)]
pub struct StatLogSe {
    pub nanos: f64,
    pub cycles: f64,
    pub inst: f64,
    pub branch: f64,
}

fn f64_to_dec(value: f64) -> Dec {
    if !value.is_finite() || value <= 0.0 {
        return Dec::default();
    }
    let scaled = value * Dec::SCALE as f64;
    if scaled >= u64::MAX as f64 {
        return Dec::from_raw_fixed_point(u64::MAX);
    }
    Dec::from_raw_fixed_point(scaled.round() as u64)
}

fn trimmed_log_estimate_metric(stats: &[Stat], extract: fn(&Stat) -> f64) -> LogEstimate {
    let values = stats.iter().map(extract).collect::<Vec<_>>();
    trimmed_log_estimate(&values)
}

/// Block-based trimmed log mean. Samples are partitioned into contiguous
/// blocks in measurement order. A trimmed log mean is computed per block;
/// the reported estimate is the average of block estimates and the SE is the
/// standard error across blocks. Falls back to the naive per-sample estimator
/// when there are too few samples to form blocks.
fn block_trimmed_log_estimate(stats: &[Stat], extract: fn(&Stat) -> f64) -> LogEstimate {
    let block_count = SE_BLOCK_COUNT;
    let min_per_block = SE_MIN_SAMPLES_PER_BLOCK;
    if stats.len() < block_count * min_per_block {
        return trimmed_log_estimate_metric(stats, extract);
    }

    let block_size = stats.len() / block_count;
    let mut block_means = Vec::with_capacity(block_count);
    for index in 0..block_count {
        let lo = index * block_size;
        let hi = if index + 1 == block_count {
            stats.len()
        } else {
            lo + block_size
        };
        let values = stats[lo..hi].iter().map(extract).collect::<Vec<_>>();
        let estimate = trimmed_log_estimate(&values);
        if !estimate.log_mean.is_finite() {
            return trimmed_log_estimate_metric(stats, extract);
        }
        block_means.push(estimate.log_mean);
    }

    let k = block_means.len() as f64;
    let mean = block_means.iter().sum::<f64>() / k;
    let mut acc = 0.0;
    for value in &block_means {
        let delta = value - mean;
        acc += delta * delta;
    }
    let block_variance = acc / (k - 1.0);
    let block_se = (block_variance / k).sqrt();

    // Don't let the block SE be smaller than the within-block SE for the same
    // data, since the latter is the lower bound under the no-drift assumption.
    let within = trimmed_log_estimate_metric(stats, extract);
    let log_se = block_se.max(within.log_se);

    LogEstimate {
        log_mean: mean,
        log_se,
    }
}

impl std::ops::Div<u64> for Stat {
    type Output = Stat;

    #[inline]
    fn div(self, rhs: u64) -> Self::Output {
        Stat {
            nanos: self.nanos / rhs,
            cycles: self.cycles / rhs,
            inst: self.inst / rhs,
            branch: self.branch / rhs,
        }
    }
}

impl std::fmt::Display for Stat {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "time={}ns cycles={} instructions={} branches={}",
            self.nanos, self.cycles, self.inst, self.branch
        )
    }
}

impl Stat {
    #[inline]
    pub fn per_second(&self) -> f64 {
        if self.nanos.0 == 0 {
            return 0.0;
        }
        1e9 / f64::from(self.nanos)
    }

    /// Compute the trimmed-log-mean estimate (in raw units) and its standard
    /// error in log units, per metric.
    ///
    /// The standard error uses a block estimator: samples are split into
    /// contiguous blocks preserving their measurement order, a trimmed log
    /// mean is computed per block, and the reported SE is the cross-block
    /// standard error. This widens the posterior when the platform drifts
    /// during a benchmark, which is the dominant source of between-process
    /// noise in practice.
    pub fn trimmed_log_estimate(stats: &[Stat]) -> (Stat, StatLogSe) {
        let nanos = block_trimmed_log_estimate(stats, |stat| f64::from(stat.nanos));
        let cycles = block_trimmed_log_estimate(stats, |stat| f64::from(stat.cycles));
        let inst = block_trimmed_log_estimate(stats, |stat| f64::from(stat.inst));
        let branch = block_trimmed_log_estimate(stats, |stat| f64::from(stat.branch));

        let estimate = Stat {
            nanos: f64_to_dec(nanos.value()),
            cycles: f64_to_dec(cycles.value()),
            inst: f64_to_dec(inst.value()),
            branch: f64_to_dec(branch.value()),
        };
        let log_se = StatLogSe {
            nanos: nanos.log_se,
            cycles: cycles.log_se,
            inst: inst.log_se,
            branch: branch.log_se,
        };
        (estimate, log_se)
    }

    pub fn mean(stats: &[Stat]) -> Stat {
        if stats.is_empty() {
            return Stat::default();
        }

        let len = stats.len() as u128;
        let mut nanos = 0u128;
        let mut cycles = 0u128;
        let mut inst = 0u128;
        let mut branch = 0u128;

        for stat in stats {
            nanos += stat.nanos.0 as u128;
            cycles += stat.cycles.0 as u128;
            inst += stat.inst.0 as u128;
            branch += stat.branch.0 as u128;
        }

        Stat {
            nanos: Dec::from_raw_fixed_point((nanos / len).min(u64::MAX as u128) as u64),
            cycles: Dec::from_raw_fixed_point((cycles / len).min(u64::MAX as u128) as u64),
            inst: Dec::from_raw_fixed_point((inst / len).min(u64::MAX as u128) as u64),
            branch: Dec::from_raw_fixed_point((branch / len).min(u64::MAX as u128) as u64),
        }
    }

    pub fn median(stats: &[Stat]) -> Stat {
        if stats.is_empty() {
            return Stat::default();
        }

        Stat {
            nanos: median_dec(stats.iter().map(|stat| stat.nanos)),
            cycles: median_dec(stats.iter().map(|stat| stat.cycles)),
            inst: median_dec(stats.iter().map(|stat| stat.inst)),
            branch: median_dec(stats.iter().map(|stat| stat.branch)),
        }
    }

    pub fn min(stats: &[Stat]) -> Stat {
        if stats.is_empty() {
            return Stat::default();
        }

        let mut min = stats[0];
        for stat in &stats[1..] {
            min.nanos = min.nanos.min(stat.nanos);
            min.cycles = min.cycles.min(stat.cycles);
            min.inst = min.inst.min(stat.inst);
            min.branch = min.branch.min(stat.branch);
        }
        min
    }

    pub fn max(stats: &[Stat]) -> Stat {
        if stats.is_empty() {
            return Stat::default();
        }

        let mut max = stats[0];
        for stat in &stats[1..] {
            max.nanos = max.nanos.max(stat.nanos);
            max.cycles = max.cycles.max(stat.cycles);
            max.inst = max.inst.max(stat.inst);
            max.branch = max.branch.max(stat.branch);
        }
        max
    }

    pub fn median_abs_deviation(stats: &[Stat], center: Stat) -> Stat {
        if stats.is_empty() {
            return Stat::default();
        }

        Stat {
            nanos: median_dec(stats.iter().map(|stat| stat.nanos.abs_diff(center.nanos))),
            cycles: median_dec(stats.iter().map(|stat| stat.cycles.abs_diff(center.cycles))),
            inst: median_dec(stats.iter().map(|stat| stat.inst.abs_diff(center.inst))),
            branch: median_dec(stats.iter().map(|stat| stat.branch.abs_diff(center.branch))),
        }
    }

    fn normalize(
        self,
        fixed_baseline: Stat,
        per_iteration_baseline: Stat,
        iterations: u64,
    ) -> (Stat, StatUnderflow) {
        assert!(
            iterations > 0,
            "benchmark sample iterations must be non-zero"
        );

        let (nanos, nanos_underflow) = normalize_dec(
            self.nanos,
            fixed_baseline.nanos,
            per_iteration_baseline.nanos,
            iterations,
        );
        let (cycles, cycles_underflow) = normalize_dec(
            self.cycles,
            fixed_baseline.cycles,
            per_iteration_baseline.cycles,
            iterations,
        );
        let (inst, inst_underflow) = normalize_dec(
            self.inst,
            fixed_baseline.inst,
            per_iteration_baseline.inst,
            iterations,
        );
        let (branch, branch_underflow) = normalize_dec(
            self.branch,
            fixed_baseline.branch,
            per_iteration_baseline.branch,
            iterations,
        );

        (
            Stat {
                nanos,
                cycles,
                inst,
                branch,
            },
            StatUnderflow {
                nanos: nanos_underflow,
                cycles: cycles_underflow,
                inst: inst_underflow,
                branch: branch_underflow,
            },
        )
    }

    fn saturating_sub_with_underflow(self, rhs: Stat) -> (Stat, StatUnderflow) {
        let nanos_underflow = self.nanos.0 < rhs.nanos.0;
        let cycles_underflow = self.cycles.0 < rhs.cycles.0;
        let inst_underflow = self.inst.0 < rhs.inst.0;
        let branch_underflow = self.branch.0 < rhs.branch.0;

        (
            Stat {
                nanos: self.nanos - rhs.nanos,
                cycles: self.cycles - rhs.cycles,
                inst: self.inst - rhs.inst,
                branch: self.branch - rhs.branch,
            },
            StatUnderflow {
                nanos: nanos_underflow,
                cycles: cycles_underflow,
                inst: inst_underflow,
                branch: branch_underflow,
            },
        )
    }
}

#[must_use]
#[derive(Jsony, Default, Debug, Clone, Copy, PartialEq, Eq)]
#[jsony(Json)]
pub struct StatUnderflow {
    pub nanos: bool,
    pub cycles: bool,
    pub inst: bool,
    pub branch: bool,
}

impl StatUnderflow {
    #[inline]
    pub fn any(self) -> bool {
        self.nanos || self.cycles || self.inst || self.branch
    }
}

#[must_use]
#[derive(Jsony, Debug, Clone, PartialEq)]
#[jsony(Json)]
pub struct BenchReport {
    /// Trimmed-log-mean (geometric trimmed mean) per metric.
    pub estimate: Stat,
    /// Standard error of `ln(estimate)`, per metric.
    pub estimate_log_se: StatLogSe,
    /// Plain median, kept for diagnostics only.
    pub median: Stat,
    pub min: Stat,
    pub max: Stat,
    pub median_abs_deviation: Stat,
    pub adjustment: Stat,
    pub samples: Vec<Stat>,
    pub iterations_per_sample: u64,
    pub warmup_samples: usize,
    pub underflow_samples: usize,
}

impl BenchReport {
    pub fn from_samples(
        samples: Vec<Stat>,
        iterations_per_sample: u64,
        warmup_samples: usize,
        underflow_samples: usize,
        adjustment: Stat,
    ) -> Self {
        let median = Stat::median(&samples);
        let min = Stat::min(&samples);
        let max = Stat::max(&samples);
        let median_abs_deviation = Stat::median_abs_deviation(&samples, median);
        let (estimate, estimate_log_se) = Stat::trimmed_log_estimate(&samples);

        Self {
            estimate,
            estimate_log_se,
            median,
            min,
            max,
            median_abs_deviation,
            adjustment,
            samples,
            iterations_per_sample,
            warmup_samples,
            underflow_samples,
        }
    }

    #[inline]
    pub fn sample_count(&self) -> usize {
        self.samples.len()
    }
}

impl std::fmt::Display for BenchReport {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "{} samples x {} iterations: estimate=({}) median=({}) mad=({})",
            self.samples.len(),
            self.iterations_per_sample,
            self.estimate,
            self.median,
            self.median_abs_deviation
        )
    }
}

#[derive(Jsony, Debug, Clone, Copy, PartialEq, Eq)]
#[jsony(Json)]
pub struct BenchParameters {
    pub sample_target_duration_ns: u64,
    pub max_sample_iterations: u64,
    pub min_sample_iterations: u64,
    pub max_samples: usize,
    pub min_samples: usize,
    pub target_duration_ns: u64,
}

impl BenchParameters {
    pub const QUICK: Self = Self {
        sample_target_duration_ns: nanos(Duration::from_millis(2)),
        max_sample_iterations: 1_000_000,
        min_sample_iterations: 4,
        max_samples: 240,
        min_samples: 80,
        target_duration_ns: nanos(Duration::from_millis(200)),
    };

    pub fn validate(self) {
        assert!(
            self.min_sample_iterations > 0,
            "min_sample_iterations must be greater than zero"
        );
        assert!(
            self.max_sample_iterations >= self.min_sample_iterations,
            "max_sample_iterations must be greater than or equal to min_sample_iterations"
        );
        assert!(
            self.min_samples > 0,
            "min_samples must be greater than zero"
        );
        assert!(
            self.max_samples >= self.min_samples,
            "max_samples must be greater than or equal to min_samples"
        );
        assert!(
            self.sample_target_duration_ns > 0,
            "sample_target_duration_ns must be greater than zero"
        );
        assert!(
            self.target_duration_ns > 0,
            "target_duration_ns must be greater than zero"
        );
    }
}

pub const BIG_BENCH_PARAM: BenchParameters = BenchParameters {
    sample_target_duration_ns: nanos(Duration::from_millis(10)),
    max_sample_iterations: 2_000_000,
    min_sample_iterations: 4,
    max_samples: 400,
    min_samples: 150,
    target_duration_ns: nanos(Duration::from_millis(1000)),
};

pub const DEFAULT_BENCH_PARAM: BenchParameters = BenchParameters {
    sample_target_duration_ns: nanos(Duration::from_millis(5)),
    max_sample_iterations: 1_000_000,
    min_sample_iterations: 4,
    max_samples: 300,
    min_samples: 100,
    target_duration_ns: nanos(Duration::from_millis(500)),
};

impl Default for Bencher {
    fn default() -> Self {
        Self::new()
    }
}

impl Bencher {
    pub fn new() -> Bencher {
        hygiene::prepare_process();
        Self::try_uncalibrated().expect("failed to create perf counter group for jsony_bench")
    }

    pub fn try_new() -> io::Result<Bencher> {
        Self::try_uncalibrated()
    }

    pub fn uncalibrated() -> Bencher {
        Self::try_uncalibrated().expect("failed to create perf counter group for jsony_bench")
    }

    pub fn try_uncalibrated() -> io::Result<Bencher> {
        let mut group = Group::new()?;
        let cycle_counter = group.add(&Builder::new(Hardware::CPU_CYCLES))?;
        let instruction_counter = group.add(&Builder::new(Hardware::INSTRUCTIONS))?;
        let branch_counter = group.add(&Builder::new(Hardware::BRANCH_INSTRUCTIONS))?;

        Ok(Bencher {
            perf_group: group,
            cycle_counter,
            instruction_counter,
            branch_counter,
            baseline: Stat::default(),
        })
    }

    pub fn calibrate(&mut self) {
        self.try_calibrate()
            .expect("failed to calibrate perf counter group for jsony_bench");
    }

    pub fn try_calibrate(&mut self) -> io::Result<()> {
        let iterations = 1000;
        let mut samples = Vec::with_capacity(iterations);

        for _ in 0..iterations {
            self.perf_group.reset()?;
            let time = std::time::Instant::now();
            self.perf_group.enable()?;
            self.perf_group.disable()?;
            let counts = self.try_read_valid_counts()?;

            samples.push(Stat {
                nanos: nanos(time.elapsed()).into(),
                cycles: counts.cycles.into(),
                inst: counts.inst.into(),
                branch: counts.branch.into(),
            });
        }

        self.baseline = Stat::median(&samples);
        Ok(())
    }

    #[inline]
    pub fn baseline(&self) -> Stat {
        self.baseline
    }

    pub fn func_report(&mut self, func: impl FnMut()) -> BenchReport {
        self.func_with_parameters_report(&DEFAULT_BENCH_PARAM, None, func)
    }

    pub fn func_with_parameters_report(
        &mut self,
        param: &BenchParameters,
        locked_iterations: Option<u64>,
        func: impl FnMut(),
    ) -> BenchReport {
        self.func_with_plan_report(
            param,
            locked_iterations.map(MeasurementPlan::iterations_only),
            func,
        )
    }

    fn func_with_plan_report(
        &mut self,
        param: &BenchParameters,
        measurement_plan: Option<MeasurementPlan>,
        mut func: impl FnMut(),
    ) -> BenchReport {
        self.bench_internal(
            BenchFunc(&mut move |bencher: &mut Bencher, iterations: u64| {
                let time = std::time::Instant::now();
                bencher
                    .perf_group
                    .enable()
                    .expect("failed to enable perf counter group for benchmark sample");
                for _ in 0..iterations {
                    func();
                }
                bencher
                    .perf_group
                    .disable()
                    .expect("failed to disable perf counter group for benchmark sample");
                time.elapsed()
            }),
            param,
            measurement_plan,
            1,
        )
    }

    /// Indexed-iteration benchmark: the body closure is called with iteration
    /// indices `0..iterations` per sample, so input sequences are identical
    /// across samples and across processes. Avoids the generator-state
    /// confound that plagues `bench_with_generator_*`.
    pub fn indexed_with_parameters_report(
        &mut self,
        param: &BenchParameters,
        locked_iterations: Option<u64>,
        iteration_multiple: u64,
        body: &mut dyn FnMut(u64),
    ) -> BenchReport {
        self.indexed_with_plan_report(
            param,
            locked_iterations.map(MeasurementPlan::iterations_only),
            iteration_multiple,
            body,
        )
    }

    fn indexed_with_plan_report(
        &mut self,
        param: &BenchParameters,
        measurement_plan: Option<MeasurementPlan>,
        iteration_multiple: u64,
        body: &mut dyn FnMut(u64),
    ) -> BenchReport {
        self.bench_internal(
            BenchFunc(&mut move |bencher: &mut Bencher, iterations: u64| {
                let time = std::time::Instant::now();
                bencher
                    .perf_group
                    .enable()
                    .expect("failed to enable perf counter group for benchmark sample");
                for i in 0..iterations {
                    body(i);
                }
                bencher
                    .perf_group
                    .disable()
                    .expect("failed to disable perf counter group for benchmark sample");
                time.elapsed()
            }),
            param,
            measurement_plan,
            iteration_multiple,
        )
    }

    pub fn bench_with_generator_report<T>(
        &mut self,
        generator: impl FnMut() -> T,
        func: impl FnMut(T),
    ) -> BenchReport {
        self.bench_with_generator_with_parameters_report(&BIG_BENCH_PARAM, None, generator, func)
    }

    pub fn bench_with_generator_with_parameters_report<T>(
        &mut self,
        param: &BenchParameters,
        locked_iterations: Option<u64>,
        mut generator: impl FnMut() -> T,
        mut func: impl FnMut(T),
    ) -> BenchReport {
        let measurement_plan = locked_iterations.map(MeasurementPlan::iterations_only);
        self.bench_with_generator_plan_report(param, measurement_plan, &mut generator, &mut func)
    }

    fn bench_with_generator_plan_report<T>(
        &mut self,
        param: &BenchParameters,
        measurement_plan: Option<MeasurementPlan>,
        generator: &mut impl FnMut() -> T,
        func: &mut impl FnMut(T),
    ) -> BenchReport {
        param.validate();

        let iterations = match measurement_plan {
            Some(plan) => clamp_iterations(plan.iterations_per_sample, param),
            None => {
                let mut bench = |bencher: &mut Bencher, iter: u64| {
                    run_generated_loop(bencher, iter, generator, Some(&mut *func))
                };
                let mut bench_func = BenchFunc(&mut bench);
                self.calibrate_iterations(&mut bench_func, param)
            }
        };
        let target_samples = measurement_plan.and_then(|plan| plan.locked_samples(param));

        // Burn-in: warm CPU, branch predictor, cache and TLB. Samples are
        // taken with the full pipeline (generator + func) so the work mirrors
        // what we will measure next. Discarded.
        for _ in 0..BURN_IN_SAMPLES {
            let _ = sample_generated(self, iterations, generator, Some(&mut *func)).normalized;
        }

        // Adjustment batch: a single trimmed-log-mean estimate of generator
        // overhead, computed once and subtracted from each measured sample.
        let mut adjustment_raw = Vec::with_capacity(ADJUSTMENT_SAMPLES as usize);
        for _ in 0..ADJUSTMENT_SAMPLES {
            let adj = sample_generated(self, iterations, generator, None::<&mut fn(T)>).normalized;
            adjustment_raw.push(adj);
        }
        let (adjustment, _) = Stat::trimmed_log_estimate(&adjustment_raw);

        let time = std::time::Instant::now();
        let mut samples = Vec::with_capacity(param.max_samples);
        let mut underflow_samples = 0usize;

        while samples.len() < param.max_samples {
            let measured =
                sample_generated(self, iterations, generator, Some(&mut *func)).normalized;
            let (net, underflow) = measured.saturating_sub_with_underflow(adjustment);
            if underflow.any() {
                underflow_samples += 1;
            }
            samples.push(net);

            let elapsed = nanos(time.elapsed());
            if should_stop_sampling(samples.len(), elapsed, param, target_samples) {
                break;
            }
        }

        BenchReport::from_samples(
            samples,
            iterations,
            BURN_IN_SAMPLES,
            underflow_samples,
            adjustment,
        )
    }

    #[inline(never)]
    fn bench_internal(
        &mut self,
        mut func: BenchFunc<'_, '_>,
        param: &BenchParameters,
        measurement_plan: Option<MeasurementPlan>,
        iteration_multiple: u64,
    ) -> BenchReport {
        param.validate();
        let iterations = match measurement_plan {
            Some(plan) => {
                align_iterations_to_multiple(plan.iterations_per_sample, param, iteration_multiple)
            }
            None => self.calibrate_iterations(&mut func, param),
        };
        let iterations = align_iterations_to_multiple(iterations, param, iteration_multiple);
        let target_samples = measurement_plan.and_then(|plan| plan.locked_samples(param));

        for _ in 0..BURN_IN_SAMPLES {
            let _ = func.sample(self, iterations, Stat::default());
        }

        let time = std::time::Instant::now();
        let mut samples = Vec::with_capacity(param.max_samples);
        let mut underflow_samples = 0usize;

        while samples.len() < param.max_samples {
            let sample = func.sample(self, iterations, Stat::default());
            if sample.underflow.any() {
                underflow_samples += 1;
            }
            samples.push(sample.normalized);

            let elapsed = nanos(time.elapsed());
            if should_stop_sampling(samples.len(), elapsed, param, target_samples) {
                break;
            }
        }

        BenchReport::from_samples(
            samples,
            iterations,
            BURN_IN_SAMPLES,
            underflow_samples,
            Stat::default(),
        )
    }

    /// Pick an iteration count whose sample wall time clears
    /// `sample_target_duration_ns`. Doubles iterations each pass, gives up
    /// after `CALIBRATION_MAX_PASSES`.
    fn calibrate_iterations(
        &mut self,
        func: &mut BenchFunc<'_, '_>,
        param: &BenchParameters,
    ) -> u64 {
        let mut iterations = param.min_sample_iterations.max(1);

        for _ in 0..CALIBRATION_MAX_PASSES {
            let sample = func.sample(self, iterations, Stat::default());
            let raw_ns = nanos(sample.raw_elapsed);
            if raw_ns >= param.sample_target_duration_ns {
                break;
            }

            let nanos_per_iter = raw_ns.max(1) / iterations.max(1);
            let target = param.sample_target_duration_ns / nanos_per_iter.max(1);
            let next = target
                .max(iterations.saturating_mul(2))
                .min(param.max_sample_iterations);
            if next <= iterations {
                break;
            }
            iterations = next;
        }

        clamp_iterations(iterations, param)
    }

    fn read_valid_counts(&mut self) -> PerfCounts {
        self.try_read_valid_counts()
            .expect("failed to read perf counter group")
    }

    fn try_read_valid_counts(&mut self) -> io::Result<PerfCounts> {
        let counts = self.perf_group.read()?;
        validate_group_scheduled(&counts);

        Ok(PerfCounts {
            cycles: counts[&self.cycle_counter],
            inst: counts[&self.instruction_counter],
            branch: counts[&self.branch_counter],
        })
    }
}

struct RunContext {
    backend: BenchBackend,
    route_filter: Option<String>,
    param_filters: Vec<ParamValue>,
    run_plan: Option<RunPlan>,
    progress: bool,
    records: Vec<RunRecord>,
    record_keys: BTreeSet<String>,
}

impl RunContext {
    fn measurement_plan(&self, path: &[String], params: &[ParamValue]) -> Option<MeasurementPlan> {
        self.run_plan.as_ref().and_then(|plan| {
            plan.records
                .iter()
                .find(|record| {
                    route_path_eq(path, &record.route) && params_match_exact(params, &record.params)
                })
                .map(MeasurementPlan::from_record)
        })
    }

    fn should_measure(&self, path: &[String], params: &[ParamValue]) -> bool {
        let route = path.join("/");
        let route_matches = self
            .route_filter
            .as_deref()
            .is_none_or(|filter| route == filter || route.starts_with(&format!("{filter}/")));
        route_matches && self.params_match(params)
    }

    fn params_match(&self, params: &[ParamValue]) -> bool {
        self.param_filters.iter().all(|filter| {
            params
                .iter()
                .any(|param| param.name == filter.name && param.value == filter.value)
        })
    }

    fn param_value_possible(
        &self,
        name: &str,
        value: &str,
        profile_defaults: Option<&BTreeSet<String>>,
    ) -> bool {
        let cli_filter_matches = self
            .param_filters
            .iter()
            .all(|filter| filter.name != name || filter.value == value);
        if !cli_filter_matches {
            return false;
        }

        if self.backend.is_profile()
            && self.param_filters.iter().all(|filter| filter.name != name)
            && let Some(profile_defaults) = profile_defaults
        {
            return profile_defaults.contains(value);
        }

        true
    }

    fn report_measure_start(&self, path: &[String], params: &[ParamValue]) {
        if !self.progress {
            return;
        }
        eprintln!("running {} {}", path.join("/"), format_params(params));
        let _ = io::stderr().flush();
    }

    fn report_measure_finish(
        &self,
        path: &[String],
        params: &[ParamValue],
        elapsed: Duration,
        report: &BenchReport,
    ) {
        if !self.progress {
            return;
        }
        eprintln!(
            "finished {} {} in {:.3}s ({} samples x {} iterations)",
            path.join("/"),
            format_params(params),
            elapsed.as_secs_f64(),
            report.sample_count(),
            report.iterations_per_sample,
        );
        let _ = io::stderr().flush();
    }

    fn push_record(&mut self, record: RunRecord) {
        let key = record_key(&record.route, &record.params);
        assert!(
            self.record_keys.insert(key),
            "duplicate benchmark record route={:?} params={:?}",
            record.route,
            record.params
        );
        self.records.push(record);
    }
}

enum BenchBackend {
    Live(Bencher),
    Profile {
        cli_iterations: Option<u64>,
    },
    #[cfg(test)]
    Fixed {
        next: u64,
    },
}

impl BenchBackend {
    fn live() -> Self {
        Self::Live(Bencher::new())
    }

    fn profile(cli_iterations: Option<u64>) -> Self {
        Self::Profile { cli_iterations }
    }

    #[cfg(test)]
    fn fixed() -> Self {
        Self::Fixed { next: 1 }
    }

    fn measure_func(
        &mut self,
        parameters: &BenchParameters,
        measurement_plan: Option<MeasurementPlan>,
        default_profile_iterations: u64,
        func: &mut dyn FnMut(),
    ) -> BenchReport {
        match self {
            BenchBackend::Live(bencher) => {
                bencher.func_with_plan_report(parameters, measurement_plan, func)
            }
            BenchBackend::Profile { cli_iterations } => {
                let iterations =
                    resolve_profile_iterations(*cli_iterations, default_profile_iterations);
                let start = std::time::Instant::now();
                for _ in 0..iterations {
                    func();
                }
                profile_report(iterations, start.elapsed())
            }
            #[cfg(test)]
            BenchBackend::Fixed { next } => {
                func();
                let base = *next;
                *next += 1;
                BenchReport::from_samples(
                    fixed_samples(base, measurement_plan, parameters),
                    measurement_plan
                        .map(|plan| plan.iterations_per_sample)
                        .unwrap_or(parameters.min_sample_iterations),
                    0,
                    0,
                    Stat::default(),
                )
            }
        }
    }

    fn measure_indexed(
        &mut self,
        parameters: &BenchParameters,
        measurement_plan: Option<MeasurementPlan>,
        default_profile_iterations: u64,
        iteration_multiple: u64,
        body: &mut dyn FnMut(u64),
    ) -> BenchReport {
        match self {
            BenchBackend::Live(bencher) => bencher.indexed_with_plan_report(
                parameters,
                measurement_plan,
                iteration_multiple,
                body,
            ),
            BenchBackend::Profile { cli_iterations } => {
                let iterations = align_profile_iterations(
                    resolve_profile_iterations(*cli_iterations, default_profile_iterations),
                    iteration_multiple,
                );
                let start = std::time::Instant::now();
                for index in 0..iterations {
                    body(index);
                }
                profile_report(iterations, start.elapsed())
            }
            #[cfg(test)]
            BenchBackend::Fixed { next } => {
                body(0);
                let base = *next;
                *next += 1;
                BenchReport::from_samples(
                    fixed_samples(base, measurement_plan, parameters),
                    align_iterations_to_multiple(
                        measurement_plan
                            .map(|plan| plan.iterations_per_sample)
                            .unwrap_or(parameters.min_sample_iterations),
                        parameters,
                        iteration_multiple,
                    ),
                    0,
                    0,
                    Stat::default(),
                )
            }
        }
    }

    fn measure_generated<T>(
        &mut self,
        parameters: &BenchParameters,
        measurement_plan: Option<MeasurementPlan>,
        default_profile_iterations: u64,
        generator: impl FnMut() -> T,
        func: impl FnMut(T),
    ) -> BenchReport {
        match self {
            BenchBackend::Live(bencher) => {
                let mut generator = generator;
                let mut func = func;
                bencher.bench_with_generator_plan_report(
                    parameters,
                    measurement_plan,
                    &mut generator,
                    &mut func,
                )
            }
            BenchBackend::Profile { cli_iterations } => {
                let iterations =
                    resolve_profile_iterations(*cli_iterations, default_profile_iterations);
                let start = std::time::Instant::now();
                let mut generator = generator;
                let mut func = func;
                for _ in 0..iterations {
                    let mut input = generator();
                    black_box(&mut input);
                    func(input);
                }
                profile_report(iterations, start.elapsed())
            }
            #[cfg(test)]
            BenchBackend::Fixed { next } => {
                measure_generated_fixed(parameters, measurement_plan, next, generator, func)
            }
        }
    }

    fn is_profile(&self) -> bool {
        matches!(self, Self::Profile { .. })
    }
}

fn resolve_profile_iterations(cli_iterations: Option<u64>, default_profile_iterations: u64) -> u64 {
    cli_iterations.unwrap_or(default_profile_iterations).max(1)
}

fn align_profile_iterations(iterations: u64, iteration_multiple: u64) -> u64 {
    let multiple = iteration_multiple.max(1);
    let iterations = iterations.max(1);
    if multiple == 1 {
        return iterations;
    }

    iterations
        .saturating_add(multiple - 1)
        .saturating_div(multiple)
        .saturating_mul(multiple)
        .max(multiple)
}

fn profile_report(iterations: u64, elapsed: Duration) -> BenchReport {
    let iterations = iterations.max(1);
    let nanos_per_iter = nanos(elapsed).max(1) as f64 / iterations as f64;
    BenchReport::from_samples(
        vec![Stat {
            nanos: f64_to_dec(nanos_per_iter),
            cycles: Dec::default(),
            inst: Dec::default(),
            branch: Dec::default(),
        }],
        iterations,
        0,
        0,
        Stat::default(),
    )
}

#[cfg(test)]
fn fixed_samples(
    base: u64,
    measurement_plan: Option<MeasurementPlan>,
    parameters: &BenchParameters,
) -> Vec<Stat> {
    let samples = measurement_plan
        .and_then(|plan| plan.locked_samples(parameters))
        .unwrap_or(64);
    (0..samples)
        .map(|_| stat_for_testish(base * 10 + 1))
        .collect()
}

#[cfg(test)]
fn measure_generated_fixed<T>(
    parameters: &BenchParameters,
    measurement_plan: Option<MeasurementPlan>,
    next: &mut u64,
    mut generator: impl FnMut() -> T,
    mut func: impl FnMut(T),
) -> BenchReport {
    let input = generator();
    func(input);
    let base = *next;
    *next += 1;
    BenchReport::from_samples(
        fixed_samples(base, measurement_plan, parameters),
        measurement_plan
            .map(|plan| plan.iterations_per_sample)
            .unwrap_or(parameters.min_sample_iterations),
        0,
        0,
        Stat::default(),
    )
}

struct BenchFunc<'a, 'b>(&'a mut (dyn 'b + FnMut(&mut Bencher, u64) -> Duration));

impl<'a, 'b> BenchFunc<'a, 'b> {
    fn sample(
        &mut self,
        bencher: &mut Bencher,
        iterations: u64,
        per_iteration_baseline: Stat,
    ) -> BenchSample {
        assert!(
            iterations > 0,
            "benchmark sample iterations must be non-zero"
        );

        bencher
            .perf_group
            .reset()
            .expect("failed to reset perf counter group");
        let elapsed = self.0(bencher, iterations);
        let counts = bencher.read_valid_counts();
        let raw = Stat {
            nanos: nanos(elapsed).into(),
            cycles: counts.cycles.into(),
            inst: counts.inst.into(),
            branch: counts.branch.into(),
        };
        let (normalized, underflow) =
            raw.normalize(Stat::default(), per_iteration_baseline, iterations);

        BenchSample {
            raw_elapsed: elapsed,
            normalized,
            underflow,
        }
    }
}

fn clamp_iterations(iterations: u64, param: &BenchParameters) -> u64 {
    iterations
        .max(param.min_sample_iterations)
        .min(param.max_sample_iterations)
}

fn align_iterations_to_multiple(
    iterations: u64,
    param: &BenchParameters,
    iteration_multiple: u64,
) -> u64 {
    let multiple = iteration_multiple.max(1);
    let clamped = clamp_iterations(iterations, param);
    if multiple == 1 {
        return clamped;
    }

    let rounded_up = clamped
        .saturating_add(multiple - 1)
        .saturating_div(multiple)
        .saturating_mul(multiple);
    if rounded_up <= param.max_sample_iterations {
        return rounded_up.max(multiple);
    }

    let rounded_down = param.max_sample_iterations / multiple * multiple;
    if rounded_down >= param.min_sample_iterations && rounded_down > 0 {
        rounded_down
    } else {
        clamped
    }
}

fn should_stop_sampling(
    sample_count: usize,
    elapsed_ns: u64,
    param: &BenchParameters,
    locked_samples: Option<usize>,
) -> bool {
    if let Some(samples) = locked_samples {
        sample_count >= samples
    } else {
        elapsed_ns >= param.target_duration_ns && sample_count >= param.min_samples
    }
}

fn sample_generated<T, G, F>(
    bencher: &mut Bencher,
    iterations: u64,
    generator: &mut G,
    func: Option<&mut F>,
) -> BenchSample
where
    G: FnMut() -> T,
    F: FnMut(T),
{
    assert!(
        iterations > 0,
        "benchmark sample iterations must be non-zero"
    );

    bencher
        .perf_group
        .reset()
        .expect("failed to reset perf counter group");
    let elapsed = run_generated_loop(bencher, iterations, generator, func);
    let counts = bencher.read_valid_counts();
    let raw = Stat {
        nanos: nanos(elapsed).into(),
        cycles: counts.cycles.into(),
        inst: counts.inst.into(),
        branch: counts.branch.into(),
    };
    let (normalized, underflow) = raw.normalize(Stat::default(), Stat::default(), iterations);

    BenchSample {
        raw_elapsed: elapsed,
        normalized,
        underflow,
    }
}

fn run_generated_loop<T, G, F>(
    bencher: &mut Bencher,
    iterations: u64,
    generator: &mut G,
    func: Option<&mut F>,
) -> Duration
where
    G: FnMut() -> T,
    F: FnMut(T),
{
    let time = std::time::Instant::now();
    bencher
        .perf_group
        .enable()
        .expect("failed to enable perf counter group for benchmark sample");

    if let Some(func) = func {
        for _ in 0..iterations {
            let mut input = generator();
            black_box(&mut input);
            func(input);
        }
    } else {
        for _ in 0..iterations {
            let mut input = generator();
            black_box(&mut input);
        }
    }

    bencher
        .perf_group
        .disable()
        .expect("failed to disable perf counter group for benchmark sample");
    time.elapsed()
}

struct BenchSample {
    raw_elapsed: Duration,
    normalized: Stat,
    underflow: StatUnderflow,
}

struct PerfCounts {
    cycles: u64,
    inst: u64,
    branch: u64,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum CliMode {
    Bench,
    Profile,
}

#[derive(Debug, Clone)]
struct CliOptions {
    mode: CliMode,
    route_filter: Option<String>,
    save: Option<PathBuf>,
    compare: Option<PathBuf>,
    param_filters: Vec<ParamValue>,
    threshold_pct: f64,
    profile_iterations: Option<u64>,
    progress: bool,
    help: bool,
}

impl CliOptions {
    fn parse(args: &[String]) -> Result<Self, String> {
        let mut mode = CliMode::Bench;
        let mut route_filter = None;
        let mut save = None;
        let mut compare = None;
        let mut param_filters = Vec::new();
        let mut threshold_pct = DEFAULT_THRESHOLD_PCT;
        let mut profile_iterations = None;
        let mut progress = false;
        let mut help = false;

        let mut index = 0;
        if let Some(command) = args.first().map(String::as_str) {
            match command {
                "bench" => index = 1,
                "profile" => {
                    mode = CliMode::Profile;
                    index = 1;
                }
                _ => {}
            }
        }

        while index < args.len() {
            match args[index].as_str() {
                "--help" | "-h" => {
                    help = true;
                    index += 1;
                }
                "--progress" => {
                    progress = true;
                    index += 1;
                }
                "--save" => {
                    if mode == CliMode::Profile {
                        return Err("profile does not support --save".to_owned());
                    }
                    index += 1;
                    let value = args
                        .get(index)
                        .ok_or_else(|| "--save requires a path".to_owned())?;
                    save = Some(PathBuf::from(value));
                    index += 1;
                }
                "--compare" => {
                    if mode == CliMode::Profile {
                        return Err("profile does not support --compare".to_owned());
                    }
                    index += 1;
                    let value = args
                        .get(index)
                        .ok_or_else(|| "--compare requires a path".to_owned())?;
                    compare = Some(PathBuf::from(value));
                    index += 1;
                }
                "--param" | "-p" => {
                    index += 1;
                    let value = args
                        .get(index)
                        .ok_or_else(|| "--param requires key=value".to_owned())?;
                    param_filters.push(parse_param_filter(value)?);
                    index += 1;
                }
                "--threshold-pct" => {
                    index += 1;
                    let raw = args
                        .get(index)
                        .ok_or_else(|| "--threshold-pct requires a value".to_owned())?;
                    threshold_pct = raw
                        .parse::<f64>()
                        .map_err(|err| format!("--threshold-pct expects a number: {err}"))?;
                    if !(threshold_pct.is_finite() && threshold_pct >= 0.0) {
                        return Err(
                            "--threshold-pct must be a non-negative finite number".to_owned()
                        );
                    }
                    index += 1;
                }
                "--iterations" | "--iters" => {
                    if mode != CliMode::Profile {
                        return Err(format!(
                            "{} is only valid with the profile subcommand",
                            args[index]
                        ));
                    }
                    index += 1;
                    let raw = args.get(index).ok_or_else(|| {
                        format!("{} requires a positive integer", args[index - 1])
                    })?;
                    let iterations = raw.parse::<u64>().map_err(|err| {
                        format!("{} expects a positive integer: {err}", args[index - 1])
                    })?;
                    if iterations == 0 {
                        return Err(format!("{} must be greater than zero", args[index - 1]));
                    }
                    profile_iterations = Some(iterations);
                    index += 1;
                }
                other if other.starts_with('-') => {
                    return Err(format!("unknown option {other:?}"));
                }
                route => {
                    if route_filter.is_some() {
                        return Err(format!("unexpected extra route argument {route:?}"));
                    }
                    validate_route_path(route)?;
                    route_filter = Some(route.to_owned());
                    index += 1;
                }
            }
        }

        if mode == CliMode::Profile && !help && route_filter.is_none() {
            return Err(
                "profile requires a route, for example: benchmark profile opus/encode".to_owned(),
            );
        }

        Ok(Self {
            mode,
            route_filter,
            save,
            compare,
            param_filters,
            threshold_pct,
            profile_iterations,
            progress,
            help,
        })
    }
}

pub fn compare_runs(baseline: &RunFile, current: &RunFile) -> CompareReport {
    compare_runs_with_threshold(baseline, current, DEFAULT_THRESHOLD_PCT)
}

pub fn compare_runs_with_threshold(
    baseline: &RunFile,
    current: &RunFile,
    threshold_percent: f64,
) -> CompareReport {
    compare_runs_with_threshold_and_plan(baseline, current, threshold_percent, None)
}

fn compare_runs_with_threshold_and_plan(
    baseline: &RunFile,
    current: &RunFile,
    threshold_percent: f64,
    run_plan: Option<&RunPlan>,
) -> CompareReport {
    assert_run_file_supported(baseline);
    assert_run_file_supported(current);
    let run_plan = run_plan.or(baseline.plan.as_ref());
    if let Some(plan) = run_plan {
        assert_run_plan_supported(plan);
    }

    let mut baseline_by_key = BTreeMap::new();
    for record in &baseline.records {
        baseline_by_key.insert(record_key(&record.route, &record.params), record);
    }

    let mut seen = BTreeSet::new();
    let mut entries = Vec::new();
    let mut missing_baseline = Vec::new();
    let matched_count = current
        .records
        .iter()
        .filter(|record| baseline_by_key.contains_key(&record_key(&record.route, &record.params)))
        .count();
    let required_confidence = corrected_confidence(matched_count);

    for current_record in &current.records {
        let key = record_key(&current_record.route, &current_record.params);
        if let Some(baseline_record) = baseline_by_key.get(&key) {
            let noise = run_plan
                .and_then(|plan| {
                    plan.records
                        .iter()
                        .find(|record| record_key(&record.route, &record.params) == key)
                })
                .map(MetricNoise::from_plan_record)
                .unwrap_or_default();
            seen.insert(key);
            entries.push(compare_records(
                baseline_record,
                current_record,
                threshold_percent,
                required_confidence,
                noise,
            ));
        } else {
            missing_baseline.push(current_record.clone());
        }
    }

    let missing_current = baseline
        .records
        .iter()
        .filter(|record| !seen.contains(&record_key(&record.route, &record.params)))
        .cloned()
        .collect::<Vec<_>>();

    let regression_count = entries
        .iter()
        .filter(|entry| entry.classification == "regression")
        .count();
    let improvement_count = entries
        .iter()
        .filter(|entry| entry.classification == "improvement")
        .count();

    CompareReport {
        entries,
        missing_baseline,
        missing_current,
        regression_count,
        improvement_count,
    }
}

pub fn render_run_table(run_file: &RunFile) -> String {
    let mut output = String::new();
    output.push_str(
        "route | params | cycles/op | cycles 95% | cycles mad | inst/op | inst mad | ns/op | samples | underflow | adj/cycles\n",
    );
    output.push_str(
        "----- | ------ | --------- | ---------- | ---------- | ------- | -------- | ----- | ------- | --------- | ----------\n",
    );
    for record in &run_file.records {
        output.push_str(&format!(
            "{} | {} | {} | {} | {} | {} | {} | {} | {} | {} | {}\n",
            record.route,
            format_params(&record.params),
            record.report.estimate.cycles,
            format_metric_ci(
                record.report.estimate.cycles,
                record.report.estimate_log_se.cycles
            ),
            record.report.median_abs_deviation.cycles,
            record.report.estimate.inst,
            record.report.median_abs_deviation.inst,
            record.report.estimate.nanos,
            record.report.sample_count(),
            format_underflow(&record.report),
            format_adjustment_ratio(&record.report),
        ));
    }
    output
}

pub fn render_compare_report(report: &CompareReport) -> String {
    let mut output = String::new();
    let mut changed = report
        .entries
        .iter()
        .filter(|entry| entry.classification != "unchanged")
        .collect::<Vec<_>>();
    changed.sort_by(|left, right| {
        right
            .cycles_delta_percent
            .unwrap_or(0.0)
            .abs()
            .partial_cmp(&left.cycles_delta_percent.unwrap_or(0.0).abs())
            .unwrap_or(std::cmp::Ordering::Equal)
    });

    let threshold = report
        .entries
        .first()
        .map(|entry| entry.threshold_percent)
        .unwrap_or(DEFAULT_THRESHOLD_PCT);

    if changed.is_empty() {
        output.push_str(&format!(
            "compare: no significant changes ({} matched, threshold {:.2}%, corrected posterior >= {:.3}%)\n",
            report.entries.len(),
            threshold,
            report
                .entries
                .first()
                .map(|entry| entry.required_confidence)
                .unwrap_or(POSTERIOR_CONFIDENCE)
                * 100.0,
        ));
    } else {
        output.push_str(&format!(
            "compare: {} regressions, {} improvements (threshold {:.2}%, corrected posterior >= {:.3}%)\n",
            report.regression_count,
            report.improvement_count,
            threshold,
            report
                .entries
                .first()
                .map(|entry| entry.required_confidence)
                .unwrap_or(POSTERIOR_CONFIDENCE)
                * 100.0,
        ));
        output.push_str(
            "route | params | cycles % [lo, hi] | P(reg/imp) | inst % | baseline cycles | current cycles | class | reason\n",
        );
        output.push_str(
            "----- | ------ | ----------------- | ---------- | ------ | --------------- | -------------- | ----- | ------\n",
        );
        for entry in changed {
            output.push_str(&format!(
                "{} | {} | {} | {} | {} | {} | {} | {} | {}\n",
                entry.route,
                format_params(&entry.params),
                format_delta_ci(
                    entry.cycles_delta_percent,
                    entry.cycles_ci_lo_percent,
                    entry.cycles_ci_hi_percent
                ),
                format_p_pair(entry.cycles_p_regression, entry.cycles_p_improvement),
                format_percent(entry.inst_delta_percent),
                entry.baseline.estimate.cycles,
                entry.current.estimate.cycles,
                entry.classification,
                entry.classification_reason,
            ));
        }
    }

    if !report.missing_baseline.is_empty() {
        output.push_str(&format!(
            "missing baseline records: {}\n",
            report.missing_baseline.len()
        ));
    }
    if !report.missing_current.is_empty() {
        output.push_str(&format!(
            "missing current records: {}\n",
            report.missing_current.len()
        ));
    }
    output
}

fn format_delta_ci(delta: Option<f64>, lo: Option<f64>, hi: Option<f64>) -> String {
    match (delta, lo, hi) {
        (Some(delta), Some(lo), Some(hi)) => format!("{delta:+.2}% [{lo:+.2}, {hi:+.2}]"),
        (Some(delta), _, _) => format!("{delta:+.2}%"),
        _ => "n/a".to_owned(),
    }
}

fn format_metric_ci(estimate: Dec, log_se: f64) -> String {
    let estimate = f64::from(estimate);
    if estimate <= 0.0 || !estimate.is_finite() || !log_se.is_finite() {
        return "n/a".to_owned();
    }
    let log_estimate = estimate.ln();
    let lo = (log_estimate - CI_95_Z * log_se).exp();
    let hi = (log_estimate + CI_95_Z * log_se).exp();
    format!("[{lo:.2}, {hi:.2}]")
}

fn format_underflow(report: &BenchReport) -> String {
    if report.underflow_samples == 0 {
        "-".to_owned()
    } else {
        format!("{}/{}", report.underflow_samples, report.sample_count())
    }
}

fn format_adjustment_ratio(report: &BenchReport) -> String {
    let estimate = f64::from(report.estimate.cycles);
    let adjustment = f64::from(report.adjustment.cycles);
    if estimate <= 0.0 || adjustment <= 0.0 {
        "-".to_owned()
    } else {
        format!("{:.1}%", adjustment / estimate * 100.0)
    }
}

fn format_p_pair(p_reg: Option<f64>, p_imp: Option<f64>) -> String {
    let p_reg = p_reg.unwrap_or(f64::NAN);
    let p_imp = p_imp.unwrap_or(f64::NAN);
    if !p_reg.is_finite() && !p_imp.is_finite() {
        return "n/a".to_owned();
    }
    if p_reg >= p_imp {
        format!("reg={:.3}", p_reg)
    } else {
        format!("imp={:.3}", p_imp)
    }
}

fn corrected_confidence(comparisons: usize) -> f64 {
    if comparisons <= 1 {
        return POSTERIOR_CONFIDENCE;
    }
    let suite_alpha = 1.0 - POSTERIOR_CONFIDENCE;
    (1.0 - suite_alpha / comparisons as f64).min(1.0)
}

fn sanitize_noise_log(value: f64) -> f64 {
    if value.is_finite() && value > 0.0 {
        value
    } else {
        0.0
    }
}

/// Block-normal summary of `ln(current_estimate / baseline_estimate)` for one
/// metric. The mean and SE come from contiguous sample blocks so drift inside a
/// run widens the comparison.
#[derive(Clone, Copy, Debug, Default)]
struct DeltaPosterior {
    /// Posterior mean of `ln(ratio)`. Equal to `current_log_mean - baseline_log_mean`.
    log_mean: f64,
    /// Posterior standard deviation of `ln(ratio)`.
    log_se: f64,
    usable: bool,
}

#[derive(Clone, Copy, Debug, Default)]
struct MetricNoise {
    cycles_log_se: f64,
    inst_log_se: f64,
}

impl MetricNoise {
    fn from_plan_record(record: &RunPlanRecord) -> Self {
        Self {
            cycles_log_se: sanitize_noise_log(record.cycles_noise_log),
            inst_log_se: sanitize_noise_log(record.inst_noise_log),
        }
    }
}

impl DeltaPosterior {
    /// `P(true_delta_log > threshold_log)`. When `log_se` is zero the
    /// posterior collapses to a point and the probability is `{0, 0.5, 1}`.
    fn p_above(&self, threshold_log: f64) -> f64 {
        if !self.usable {
            return f64::NAN;
        }
        if self.log_se <= 0.0 {
            return point_probability(self.log_mean, threshold_log, true);
        }
        1.0 - std_normal_cdf((threshold_log - self.log_mean) / self.log_se)
    }

    /// `P(true_delta_log < -threshold_log)`.
    fn p_below(&self, threshold_log: f64) -> f64 {
        if !self.usable {
            return f64::NAN;
        }
        if self.log_se <= 0.0 {
            return point_probability(self.log_mean, -threshold_log, false);
        }
        std_normal_cdf((-threshold_log - self.log_mean) / self.log_se)
    }

    /// 95% credible interval bounds for the delta expressed as percent.
    fn ci_percent(&self) -> (f64, f64) {
        if !self.usable || !self.log_se.is_finite() {
            return (f64::NAN, f64::NAN);
        }
        let half_width = CI_95_Z * self.log_se;
        let lo = (self.log_mean - half_width).exp() - 1.0;
        let hi = (self.log_mean + half_width).exp() - 1.0;
        (lo * 100.0, hi * 100.0)
    }

    fn mean_percent(&self) -> f64 {
        if !self.usable {
            return f64::NAN;
        }
        (self.log_mean.exp() - 1.0) * 100.0
    }
}

/// Probability that `value > cutoff` when the posterior is a delta function at
/// `value`. Ties are treated as `0.5`.
fn point_probability(value: f64, cutoff: f64, strict_above: bool) -> f64 {
    if value > cutoff {
        if strict_above { 1.0 } else { 0.0 }
    } else if value < cutoff {
        if strict_above { 0.0 } else { 1.0 }
    } else {
        0.5
    }
}

fn metric_posterior_from_samples(
    baseline: &BenchReport,
    current: &BenchReport,
    extract: fn(&Stat) -> f64,
    extra_log_se: f64,
) -> DeltaPosterior {
    let baseline_estimate = block_trimmed_log_estimate(&baseline.samples, extract);
    let current_estimate = block_trimmed_log_estimate(&current.samples, extract);
    let extra_log_se = sanitize_noise_log(extra_log_se);
    let usable = baseline.samples.len().min(current.samples.len()) >= COMPARE_MIN_SAMPLES
        && baseline_estimate.log_mean.is_finite()
        && current_estimate.log_mean.is_finite()
        && baseline_estimate.log_se.is_finite()
        && current_estimate.log_se.is_finite();

    DeltaPosterior {
        log_mean: current_estimate.log_mean - baseline_estimate.log_mean,
        log_se: (baseline_estimate.log_se * baseline_estimate.log_se
            + current_estimate.log_se * current_estimate.log_se)
            .mul_add(1.0, extra_log_se * extra_log_se)
            .sqrt(),
        usable,
    }
}

fn compare_records(
    baseline: &RunRecord,
    current: &RunRecord,
    threshold_percent: f64,
    required_confidence: f64,
    noise: MetricNoise,
) -> CompareEntry {
    let baseline_report = &baseline.report;
    let current_report = &current.report;

    let cycles = metric_posterior_from_samples(
        baseline_report,
        current_report,
        |stat| f64::from(stat.cycles),
        noise.cycles_log_se,
    );

    let inst = metric_posterior_from_samples(
        baseline_report,
        current_report,
        |stat| f64::from(stat.inst),
        noise.inst_log_se,
    );

    let time = metric_posterior_from_samples(
        baseline_report,
        current_report,
        |stat| f64::from(stat.nanos),
        0.0,
    );

    let threshold_log = (1.0 + threshold_percent / 100.0).ln();
    let cycles_p_reg = finite_or_none(cycles.p_above(threshold_log));
    let cycles_p_imp = finite_or_none(cycles.p_below(threshold_log));
    let inst_p_reg = finite_or_none(inst.p_above(0.0));
    let inst_p_imp = finite_or_none(inst.p_below(0.0));

    let (cycles_lo, cycles_hi) = cycles.ci_percent();

    let (classification, classification_reason) = classify_change(
        baseline_report,
        current_report,
        cycles,
        inst,
        threshold_log,
        required_confidence,
    );

    CompareEntry {
        route: current.route.clone(),
        params: current.params.clone(),
        baseline: baseline_report.clone(),
        current: current_report.clone(),
        cycles_delta_percent: finite_or_none(cycles.mean_percent()),
        cycles_ci_lo_percent: finite_or_none(cycles_lo),
        cycles_ci_hi_percent: finite_or_none(cycles_hi),
        cycles_p_regression: cycles_p_reg,
        cycles_p_improvement: cycles_p_imp,
        inst_delta_percent: finite_or_none(inst.mean_percent()),
        inst_p_regression: inst_p_reg,
        inst_p_improvement: inst_p_imp,
        time_delta_percent: finite_or_none(time.mean_percent()),
        classification: classification.to_owned(),
        classification_reason: classification_reason.to_owned(),
        threshold_percent,
        required_confidence,
    }
}

fn classify_change(
    baseline: &BenchReport,
    current: &BenchReport,
    cycles: DeltaPosterior,
    inst: DeltaPosterior,
    threshold_log: f64,
    required_confidence: f64,
) -> (&'static str, &'static str) {
    if adjusted_generated(baseline) || adjusted_generated(current) {
        return ("unchanged", "setup-adjusted-diagnostic");
    }
    if baseline.underflow_samples > 0 || current.underflow_samples > 0 {
        return ("unchanged", "underflow");
    }
    if !cycles.usable {
        return ("unchanged", "insufficient-or-invalid-cycles");
    }

    let p_reg = cycles.p_above(threshold_log);
    let p_imp = cycles.p_below(threshold_log);

    let inst_opposes = |sign: f64| -> bool {
        if !inst.usable {
            return false;
        }
        if sign > 0.0 {
            inst.p_below(0.0) >= GROUNDING_CONFIDENCE
        } else {
            inst.p_above(0.0) >= GROUNDING_CONFIDENCE
        }
    };

    if p_reg >= required_confidence {
        if inst_opposes(1.0) {
            return ("unchanged", "instruction-opposite");
        }
        return ("regression", "cycles-regression");
    }
    if p_imp >= required_confidence {
        if inst_opposes(-1.0) {
            return ("unchanged", "instruction-opposite");
        }
        return ("improvement", "cycles-improvement");
    }
    if p_reg >= POSTERIOR_CONFIDENCE || p_imp >= POSTERIOR_CONFIDENCE {
        return ("unchanged", "suite-corrected-confidence");
    }
    ("unchanged", "below-threshold-confidence")
}

fn adjusted_generated(report: &BenchReport) -> bool {
    report.adjustment.nanos.raw_fixed_point() != 0
        || report.adjustment.cycles.raw_fixed_point() != 0
        || report.adjustment.inst.raw_fixed_point() != 0
        || report.adjustment.branch.raw_fixed_point() != 0
}

fn finite_or_none(value: f64) -> Option<f64> {
    if value.is_finite() { Some(value) } else { None }
}

fn save_run_file(path: &Path, run_file: &RunFile) {
    let mut json = jsony::to_json(run_file);
    json.push('\n');
    std::fs::write(path, json)
        .unwrap_or_else(|err| panic!("failed to save benchmark run to {}: {err}", path.display()));
}

fn load_run_plan_for_compare(run_path: &Path) -> RunPlan {
    let Some(plan) = read_embedded_run_plan(run_path).unwrap_or_else(|err| {
        panic!(
            "failed to read embedded benchmark run plan from {}: {err}",
            run_path.display()
        )
    }) else {
        panic!(
            "benchmark run {} does not contain an embedded plan before records; re-save the baseline with --save",
            run_path.display()
        );
    };
    assert_run_plan_supported(&plan);
    plan
}

fn run_plan_from_run_file(run_file: &RunFile) -> RunPlan {
    run_plan_from_run_file_with_noise_source(run_file, None)
}

fn run_plan_from_run_file_with_noise_source(
    run_file: &RunFile,
    noise_source: Option<&RunPlan>,
) -> RunPlan {
    let mut noise_by_key = BTreeMap::new();
    if let Some(plan) = noise_source {
        for record in &plan.records {
            noise_by_key.insert(
                record_key(&record.route, &record.params),
                (record.cycles_noise_log, record.inst_noise_log),
            );
        }
    }

    run_plan_from_run_file_with_noise_map(run_file, &noise_by_key)
}

fn run_plan_from_run_pair(prepass: &RunFile, locked: &RunFile) -> RunPlan {
    let mut prepass_by_key = BTreeMap::new();
    for record in &prepass.records {
        prepass_by_key.insert(record_key(&record.route, &record.params), record);
    }

    let mut noise_by_key = BTreeMap::new();
    for record in &locked.records {
        let key = record_key(&record.route, &record.params);
        if let Some(prepass_record) = prepass_by_key.get(&key) {
            noise_by_key.insert(
                key,
                (
                    report_metric_abs_log_delta(&prepass_record.report, &record.report, |stat| {
                        stat.cycles
                    }),
                    report_metric_abs_log_delta(&prepass_record.report, &record.report, |stat| {
                        stat.inst
                    }),
                ),
            );
        }
    }

    run_plan_from_run_file_with_noise_map(locked, &noise_by_key)
}

fn combine_run_plan_noise(baseline: &RunPlan, current: &RunPlan) -> RunPlan {
    let mut current_by_key = BTreeMap::new();
    for record in &current.records {
        current_by_key.insert(record_key(&record.route, &record.params), record);
    }

    RunPlan {
        format: RUN_PLAN_FORMAT.to_owned(),
        version: RUN_PLAN_VERSION,
        records: baseline
            .records
            .iter()
            .map(|record| {
                let current_record = current_by_key.get(&record_key(&record.route, &record.params));
                RunPlanRecord {
                    route: record.route.clone(),
                    params: record.params.clone(),
                    iterations_per_sample: record.iterations_per_sample,
                    samples: record.samples,
                    cycles_noise_log: combine_noise_log(
                        record.cycles_noise_log,
                        current_record
                            .map(|record| record.cycles_noise_log)
                            .unwrap_or_default(),
                    ),
                    inst_noise_log: combine_noise_log(
                        record.inst_noise_log,
                        current_record
                            .map(|record| record.inst_noise_log)
                            .unwrap_or_default(),
                    ),
                }
            })
            .collect(),
    }
}

fn combine_noise_log(left: f64, right: f64) -> f64 {
    let left = sanitize_noise_log(left);
    let right = sanitize_noise_log(right);
    (left * left + right * right).sqrt()
}

fn run_plan_from_run_file_with_noise_map(
    run_file: &RunFile,
    noise_by_key: &BTreeMap<String, (f64, f64)>,
) -> RunPlan {
    RunPlan {
        format: RUN_PLAN_FORMAT.to_owned(),
        version: RUN_PLAN_VERSION,
        records: run_file
            .records
            .iter()
            .map(|record| {
                let (cycles_noise_log, inst_noise_log) = noise_by_key
                    .get(&record_key(&record.route, &record.params))
                    .copied()
                    .unwrap_or((0.0, 0.0));
                RunPlanRecord {
                    route: record.route.clone(),
                    params: record.params.clone(),
                    iterations_per_sample: record.report.iterations_per_sample,
                    samples: record.report.sample_count(),
                    cycles_noise_log,
                    inst_noise_log,
                }
            })
            .collect(),
    }
}

fn report_metric_abs_log_delta(
    baseline: &BenchReport,
    current: &BenchReport,
    extract: fn(&Stat) -> Dec,
) -> f64 {
    let baseline = f64::from(extract(&baseline.estimate));
    let current = f64::from(extract(&current.estimate));
    if baseline > 0.0 && current > 0.0 && baseline.is_finite() && current.is_finite() {
        (current / baseline).ln().abs()
    } else {
        return 0.0;
    }
}

fn read_embedded_run_plan(path: &Path) -> io::Result<Option<RunPlan>> {
    const CHUNK_SIZE: usize = 8192;
    const MAX_PLAN_PREFIX_BYTES: usize = 64 * 1024 * 1024;

    let mut file = File::open(path)?;
    let mut buffer = Vec::with_capacity(CHUNK_SIZE);
    let mut chunk = [0u8; CHUNK_SIZE];
    let mut eof = false;

    loop {
        match std::str::from_utf8(&buffer) {
            Ok(contents) => {
                let plan_json = &jsony::drill(contents)["plan"];
                if plan_json.decode_error().is_none() {
                    match plan_json.parse::<Option<RunPlan>>() {
                        Ok(plan) => return Ok(plan),
                        Err(err) if eof => {
                            return Err(io::Error::new(
                                io::ErrorKind::InvalidData,
                                format!("failed to decode embedded benchmark run plan: {err:?}"),
                            ));
                        }
                        Err(_) => {}
                    }
                } else if eof {
                    return Ok(None);
                }
            }
            Err(err) if err.error_len().is_some() || eof => {
                return Err(io::Error::new(io::ErrorKind::InvalidData, err));
            }
            Err(_) => {}
        }

        if eof {
            return Ok(None);
        }
        if buffer.len() >= MAX_PLAN_PREFIX_BYTES {
            return Err(io::Error::new(
                io::ErrorKind::InvalidData,
                "embedded benchmark run plan was not found in the expected prefix",
            ));
        }

        let read = file.read(&mut chunk)?;
        if read == 0 {
            eof = true;
        } else {
            buffer.extend_from_slice(&chunk[..read]);
        }
    }
}

fn load_run_file(path: &Path) -> RunFile {
    let contents = std::fs::read_to_string(path).unwrap_or_else(|err| {
        panic!(
            "failed to read benchmark run from {}: {err}",
            path.display()
        )
    });
    let run_file = jsony::from_json::<RunFile>(&contents).unwrap_or_else(|err| {
        panic!(
            "failed to decode benchmark run from {}: {err:?}",
            path.display()
        )
    });
    assert_run_file_supported(&run_file);
    run_file
}

fn assert_run_file_supported(run_file: &RunFile) {
    assert!(
        run_file.format == RUN_FILE_FORMAT,
        "unsupported benchmark run file format {:?}",
        run_file.format
    );
    assert!(
        run_file.version == RUN_FILE_VERSION,
        "unsupported benchmark run file version {}",
        run_file.version
    );
    if let Some(plan) = &run_file.plan {
        assert_run_plan_supported(plan);
    }
}

fn assert_run_plan_supported(run_plan: &RunPlan) {
    assert!(
        run_plan.format == RUN_PLAN_FORMAT,
        "unsupported benchmark run plan format {:?}",
        run_plan.format
    );
    assert!(
        run_plan.version == RUN_PLAN_VERSION,
        "unsupported benchmark run plan version {}",
        run_plan.version
    );
}

fn validate_group_scheduled(counts: &GroupData) {
    let enabled = counts
        .time_enabled()
        .expect("perf group read did not include time_enabled");
    let running = counts
        .time_running()
        .expect("perf group read did not include time_running");

    assert!(
        enabled == running,
        "perf counter group was not scheduled for the full sample: time_enabled={}ns time_running={}ns",
        nanos(enabled),
        nanos(running)
    );
}

fn normalize_dec(
    raw: Dec,
    fixed_baseline: Dec,
    per_iteration_baseline: Dec,
    iterations: u64,
) -> (Dec, bool) {
    let fixed_underflow = raw.0 < fixed_baseline.0;
    let after_fixed = raw.0.saturating_sub(fixed_baseline.0) / iterations;
    let per_iter_underflow = after_fixed < per_iteration_baseline.0;

    (
        Dec::from_raw_fixed_point(after_fixed.saturating_sub(per_iteration_baseline.0)),
        fixed_underflow || per_iter_underflow,
    )
}

/// Standard normal cumulative distribution function.
///
/// Implemented via the Abramowitz & Stegun 7.1.26 polynomial approximation of
/// `erf`, accurate to roughly `7.5e-8`.
fn std_normal_cdf(x: f64) -> f64 {
    0.5 * (1.0 + erf(x / std::f64::consts::SQRT_2))
}

fn erf(x: f64) -> f64 {
    let sign = if x < 0.0 { -1.0 } else { 1.0 };
    let x = x.abs();
    let t = 1.0 / (1.0 + 0.3275911 * x);
    let poly = (((((1.061405429 * t) - 1.453152027) * t) + 1.421413741) * t - 0.284496736) * t
        + 0.254829592;
    sign * (1.0 - poly * t * (-x * x).exp())
}

/// Result of the trimmed-log-mean estimator on one metric.
#[derive(Clone, Copy, Debug)]
struct LogEstimate {
    /// Mean of `ln(value)` over the kept samples. NaN if no usable samples.
    log_mean: f64,
    /// Standard error of `log_mean`.
    log_se: f64,
}

impl Default for LogEstimate {
    fn default() -> Self {
        Self {
            log_mean: f64::NAN,
            log_se: f64::NAN,
        }
    }
}

impl LogEstimate {
    fn value(&self) -> f64 {
        self.log_mean.exp()
    }
}

/// Compute the trimmed-log-mean and its standard error for a slice of f64
/// observations.
///
/// Samples with non-positive values are discarded (log is undefined). The
/// remaining samples are sorted and the bottom and top `TRIM_FRACTION` are
/// dropped before computing the mean and sample variance of `ln(value)`.
fn trimmed_log_estimate(samples: &[f64]) -> LogEstimate {
    let mut logs = samples
        .iter()
        .copied()
        .filter(|value| *value > 0.0 && value.is_finite())
        .map(f64::ln)
        .collect::<Vec<_>>();
    logs.sort_by(|left, right| left.partial_cmp(right).unwrap_or(std::cmp::Ordering::Equal));

    let total = logs.len();
    if total == 0 {
        return LogEstimate::default();
    }

    let trim_each = ((total as f64) * TRIM_FRACTION).floor() as usize;
    let lo = trim_each;
    let hi = total.saturating_sub(trim_each);
    if hi <= lo {
        return LogEstimate::default();
    }

    let kept = &logs[lo..hi];
    let n = kept.len() as f64;
    let sum: f64 = kept.iter().sum();
    let mean = sum / n;

    let variance = if kept.len() >= 2 {
        let mut acc = 0.0;
        for value in kept {
            let delta = value - mean;
            acc += delta * delta;
        }
        acc / (n - 1.0)
    } else {
        0.0
    };

    LogEstimate {
        log_mean: mean,
        log_se: (variance / n).sqrt(),
    }
}

fn median_dec(values: impl Iterator<Item = Dec>) -> Dec {
    let mut values = values.collect::<Vec<_>>();
    if values.is_empty() {
        return Dec::default();
    }

    let mid = values.len() / 2;
    let even_len = values.len() % 2 == 0;
    let (lower, upper, _) = values.select_nth_unstable(mid);

    if even_len {
        let lower = lower
            .iter()
            .copied()
            .max()
            .expect("even-length median has a lower partition");
        Dec::average(lower, *upper)
    } else {
        *upper
    }
}

fn parse_param_filter(value: &str) -> Result<ParamValue, String> {
    let (name, value) = value
        .split_once('=')
        .ok_or_else(|| format!("parameter filter {value:?} must use key=value"))?;
    validate_param_name_result(name)?;
    Ok(ParamValue {
        name: name.to_owned(),
        value: value.to_owned(),
    })
}

fn validate_route_segment(segment: &str) {
    assert!(
        !segment.is_empty() && !segment.contains('/'),
        "benchmark route segment must be non-empty and must not contain '/': {segment:?}"
    );
}

fn validate_route_path(path: &str) -> Result<(), String> {
    if path.is_empty() {
        return Err("benchmark route path must not be empty".to_owned());
    }
    for segment in path.split('/') {
        if segment.is_empty() {
            return Err(format!(
                "benchmark route path {path:?} contains an empty segment"
            ));
        }
    }
    Ok(())
}

fn validate_param_name(name: &str) {
    validate_param_name_result(name).unwrap_or_else(|err| panic!("{err}"));
}

fn validate_param_name_result(name: &str) -> Result<(), String> {
    if name.is_empty() || name.contains('/') || name.contains('=') {
        return Err(format!(
            "benchmark parameter name must be non-empty and must not contain '/' or '=': {name:?}"
        ));
    }
    Ok(())
}

fn route_filter_targets_top(filter: &str, top_route: &str) -> bool {
    filter == top_route || filter.starts_with(&format!("{top_route}/"))
}

fn route_path_eq(path: &[String], route: &str) -> bool {
    let mut parts = route.split('/');
    for segment in path {
        if parts.next() != Some(segment.as_str()) {
            return false;
        }
    }
    parts.next().is_none()
}

fn params_match_exact(left: &[ParamValue], right: &[ParamValue]) -> bool {
    left.len() == right.len() && left.iter().all(|param| right.contains(param))
}

fn pushed_param(params: &[ParamValue], name: &str, value: &str) -> Vec<ParamValue> {
    let mut next = params.to_vec();
    next.push(ParamValue {
        name: name.to_owned(),
        value: value.to_owned(),
    });
    next
}

fn record_key(route: &str, params: &[ParamValue]) -> String {
    let mut params = params.to_vec();
    params.sort_by(|left, right| {
        left.name
            .cmp(&right.name)
            .then(left.value.cmp(&right.value))
    });

    let mut key = route.to_owned();
    for param in params {
        key.push('\u{1f}');
        key.push_str(&param.name);
        key.push('=');
        key.push_str(&param.value);
    }
    key
}

fn format_params(params: &[ParamValue]) -> String {
    if params.is_empty() {
        return "-".to_owned();
    }
    params
        .iter()
        .map(|param| format!("{}={}", param.name, param.value))
        .collect::<Vec<_>>()
        .join(",")
}

fn format_percent(value: Option<f64>) -> String {
    value
        .map(|value| format!("{value:+.2}%"))
        .unwrap_or_else(|| "n/a".to_owned())
}

fn usage() -> String {
    format!(
        "usage:\n  benchmark [bench] [route] [--param key=value] [--save path] [--compare path] [--threshold-pct N] [--progress]\n  benchmark profile <route> [--param key=value] [--iterations N] [--progress]\n\nprofile runs matched routes without perf counters using fixed iterations. Routes may define their own profile default; otherwise the default is {DEFAULT_PROFILE_ITERATIONS}. --iterations overrides route defaults. Profile runs are quiet unless --progress is passed.\n"
    )
}

fn unix_now_ms() -> u64 {
    let elapsed = SystemTime::now()
        .duration_since(UNIX_EPOCH)
        .unwrap_or_else(|_| Duration::from_secs(0));
    nanos(elapsed) / 1_000_000
}

#[cfg(test)]
fn stat_for_testish(value: u64) -> Stat {
    Stat {
        nanos: value.into(),
        cycles: (value * 3).into(),
        inst: (value * 5).into(),
        branch: (value * 2).into(),
    }
}

const fn nanos(duration: Duration) -> u64 {
    let nanos = duration.as_nanos();
    if nanos > u64::MAX as u128 {
        u64::MAX
    } else {
        nanos as u64
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    fn stat(nanos: u64, cycles: u64, inst: u64, branch: u64) -> Stat {
        Stat {
            nanos: nanos.into(),
            cycles: cycles.into(),
            inst: inst.into(),
            branch: branch.into(),
        }
    }

    fn test_run_file(records: Vec<RunRecord>) -> RunFile {
        RunFile {
            plan: None,
            format: RUN_FILE_FORMAT.to_owned(),
            version: RUN_FILE_VERSION,
            command: Vec::new(),
            route_filter: None,
            param_filters: Vec::new(),
            created_unix_ms: 123,
            records,
        }
    }

    fn test_record(route: &str, nanos: u64, params: Vec<ParamValue>) -> RunRecord {
        test_record_with_stat(route, stat(nanos, nanos * 2, nanos * 3, nanos), params)
    }

    fn test_record_with_stat(route: &str, stat: Stat, params: Vec<ParamValue>) -> RunRecord {
        test_record_with_report(
            route,
            BenchReport::from_samples((0..64).map(|_| stat).collect(), 1, 0, 0, Stat::default()),
            params,
        )
    }

    fn test_record_with_report(
        route: &str,
        report: BenchReport,
        params: Vec<ParamValue>,
    ) -> RunRecord {
        RunRecord {
            route: route.to_owned(),
            params,
            report,
            bench_parameters: BenchParameters::QUICK,
        }
    }

    fn eval_fixed(router: &Router, args: &[&str]) -> EvalOutcome {
        router.eval_inner(
            args.iter().map(|value| value.to_string()),
            BenchBackend::fixed,
        )
    }

    #[test]
    fn bench_subcommand_is_an_alias_for_default_mode() {
        let mut router = Router::default();
        router.add("foo", |bench| {
            bench.func(|| {});
        });

        let outcome = eval_fixed(&router, &["bench", "foo"]);

        assert_eq!(outcome.exit_code, 0);
        assert_eq!(outcome.run_file.unwrap().records[0].route, "foo");
    }

    #[test]
    fn profile_subcommand_runs_fixed_iterations() {
        use std::sync::{
            Arc,
            atomic::{AtomicUsize, Ordering},
        };

        let iterations = Arc::new(AtomicUsize::new(0));
        let observed = Arc::clone(&iterations);
        let mut router = Router::default();
        router.add("corpus", move |bench| {
            let observed = Arc::clone(&observed);
            bench.indexed_cyclic(4, move |_| {
                observed.fetch_add(1, Ordering::SeqCst);
            });
        });

        let outcome = eval_fixed(&router, &["profile", "corpus", "--iterations", "6"]);

        assert_eq!(outcome.exit_code, 0);
        assert!(outcome.stdout.is_empty());
        assert!(outcome.stderr.is_empty());
        let run = outcome.run_file.unwrap();
        assert_eq!(iterations.load(Ordering::SeqCst), 8);
        assert_eq!(run.records[0].report.iterations_per_sample, 8);
        assert_eq!(run.records[0].report.sample_count(), 1);
    }

    #[test]
    fn profile_subcommand_uses_route_default_iterations() {
        use std::sync::{
            Arc,
            atomic::{AtomicUsize, Ordering},
        };

        let iterations = Arc::new(AtomicUsize::new(0));
        let observed = Arc::clone(&iterations);
        let mut router = Router::default();
        router.add("corpus", move |bench| {
            let observed = Arc::clone(&observed);
            bench
                .with_profile_iterations(6)
                .indexed_cyclic(4, move |_| {
                    observed.fetch_add(1, Ordering::SeqCst);
                });
        });

        let outcome = eval_fixed(&router, &["profile", "corpus"]);

        assert_eq!(outcome.exit_code, 0);
        let run = outcome.run_file.unwrap();
        assert_eq!(iterations.load(Ordering::SeqCst), 8);
        assert_eq!(run.records[0].report.iterations_per_sample, 8);
    }

    #[test]
    fn profile_cli_iterations_override_route_default_iterations() {
        use std::sync::{
            Arc,
            atomic::{AtomicUsize, Ordering},
        };

        let iterations = Arc::new(AtomicUsize::new(0));
        let observed = Arc::clone(&iterations);
        let mut router = Router::default();
        router.add("corpus", move |bench| {
            let observed = Arc::clone(&observed);
            bench
                .with_profile_iterations(20)
                .indexed_cyclic(4, move |_| {
                    observed.fetch_add(1, Ordering::SeqCst);
                });
        });

        let outcome = eval_fixed(&router, &["profile", "corpus", "--iterations", "6"]);

        assert_eq!(outcome.exit_code, 0);
        let run = outcome.run_file.unwrap();
        assert_eq!(iterations.load(Ordering::SeqCst), 8);
        assert_eq!(run.records[0].report.iterations_per_sample, 8);
    }

    #[test]
    fn profile_parameter_defaults_limit_unfiltered_profile_runs() {
        let mut router = Router::default();
        router.add("matrix", |bench| {
            bench.param_str_profile_defaults(
                "shape",
                ["small", "large"],
                ["large"],
                |bench, _shape| {
                    bench.func(|| {});
                },
            );
        });

        let outcome = eval_fixed(&router, &["profile", "matrix"]);
        let run = outcome.run_file.unwrap();

        assert_eq!(run.records.len(), 1);
        assert_eq!(run.records[0].params[0], ParamValue::new("shape", "large"));
    }

    #[test]
    fn explicit_profile_parameter_filter_overrides_profile_defaults() {
        let mut router = Router::default();
        router.add("matrix", |bench| {
            bench.param_str_profile_defaults(
                "shape",
                ["small", "large"],
                ["large"],
                |bench, _shape| {
                    bench.func(|| {});
                },
            );
        });

        let outcome = eval_fixed(&router, &["profile", "matrix", "--param", "shape=small"]);
        let run = outcome.run_file.unwrap();

        assert_eq!(run.records.len(), 1);
        assert_eq!(run.records[0].params[0], ParamValue::new("shape", "small"));
    }

    #[test]
    fn bench_mode_ignores_profile_parameter_defaults() {
        let mut router = Router::default();
        router.add("matrix", |bench| {
            bench.param_str_profile_defaults(
                "shape",
                ["small", "large"],
                ["large"],
                |bench, _shape| {
                    bench.func(|| {});
                },
            );
        });

        let outcome = eval_fixed(&router, &["bench", "matrix"]);
        let run = outcome.run_file.unwrap();

        assert_eq!(run.records.len(), 2);
        assert_eq!(run.records[0].params[0], ParamValue::new("shape", "small"));
        assert_eq!(run.records[1].params[0], ParamValue::new("shape", "large"));
    }

    #[test]
    fn profile_subcommand_requires_a_route() {
        let router = Router::default();
        let outcome = eval_fixed(&router, &["profile"]);

        assert_eq!(outcome.exit_code, 2);
        assert!(outcome.stderr.contains("profile requires a route"));
    }

    #[test]
    fn median_averages_even_sample_counts() {
        let samples = [
            stat(10, 100, 1000, 20),
            stat(14, 104, 1004, 24),
            stat(12, 102, 1002, 22),
            stat(16, 106, 1006, 26),
        ];

        assert_eq!(Stat::median(&samples), stat(13, 103, 1003, 23));
    }

    #[test]
    fn report_estimate_is_trimmed_log_mean() {
        // Trim is 10% per tail; with 3 samples nothing is trimmed (floor(0.3)=0).
        // Log-mean of {ln 10, ln 12, ln 100} ~ 3.043, exp -> ~20.9.
        let samples = [
            stat(10, 20, 30, 40),
            stat(12, 24, 32, 42),
            stat(100, 200, 300, 400),
        ];
        let report = BenchReport::from_samples(samples.to_vec(), 7, 8, 1, stat(2, 3, 4, 5));

        let expected_nanos = ((10f64).ln() + (12f64).ln() + (100f64).ln()) / 3.0;
        assert!((f64::from(report.estimate.nanos) - expected_nanos.exp()).abs() < 0.5);

        assert_eq!(report.median, stat(12, 24, 32, 42));
        assert_eq!(report.median_abs_deviation, stat(2, 4, 2, 2));
        assert_eq!(report.iterations_per_sample, 7);
        assert_eq!(report.warmup_samples, 8);
        assert_eq!(report.underflow_samples, 1);
        assert_eq!(report.adjustment, stat(2, 3, 4, 5));
        assert!(report.estimate_log_se.cycles > 0.0);
    }

    #[test]
    fn normalize_reports_underflow_without_wrapping() {
        let raw = stat(10, 10, 10, 10);
        let fixed = stat(20, 1, 1, 1);
        let per_iter = stat(0, 10, 0, 10);

        let (normalized, underflow) = raw.normalize(fixed, per_iter, 3);

        assert_eq!(normalized.nanos, Dec::default());
        assert_eq!(normalized.cycles, Dec::default());
        assert_eq!(normalized.inst, Dec::from_raw_fixed_point(768));
        assert_eq!(normalized.branch, Dec::default());
        assert!(underflow.nanos);
        assert!(underflow.cycles);
        assert!(!underflow.inst);
        assert!(underflow.branch);
    }

    #[test]
    #[should_panic(expected = "max_samples must be greater than or equal to min_samples")]
    fn parameters_validate_sample_count_ordering() {
        BenchParameters {
            max_samples: 9,
            min_samples: 10,
            ..DEFAULT_BENCH_PARAM
        }
        .validate();
    }

    #[test]
    fn cyclic_indexed_iterations_are_aligned_to_cycle_len() {
        assert_eq!(
            align_iterations_to_multiple(9_732, &BenchParameters::QUICK, 32),
            9_760
        );

        let mut router = Router::default();
        router.add("corpus", |bench| {
            bench
                .with_parameters(BenchParameters::QUICK)
                .indexed_cyclic(32, |index| {
                    black_box(index);
                });
        });

        let outcome = eval_fixed(&router, &[]);
        let run = outcome.run_file.unwrap();

        assert_eq!(run.records[0].report.iterations_per_sample % 32, 0);
        assert!(run.records[0].report.iterations_per_sample >= 32);
    }

    fn appended_plan_path(path: &Path) -> PathBuf {
        let mut plan_path = path.as_os_str().to_os_string();
        plan_path.push(".plan");
        PathBuf::from(plan_path)
    }

    fn temp_run_path(name: &str) -> PathBuf {
        std::env::temp_dir().join(format!(
            "jsony_bench_{name}_{}_{}.json",
            std::process::id(),
            unix_now_ms()
        ))
    }

    #[test]
    fn compare_locks_measurement_from_embedded_run_plan() {
        let mut baseline = test_run_file(vec![test_record_with_report(
            "corpus",
            BenchReport::from_samples(vec![stat(10, 20, 30, 10); 120], 96, 0, 0, Stat::default()),
            Vec::new(),
        )]);
        baseline.plan = Some(run_plan_from_run_file(&baseline));
        let path = temp_run_path("iteration_plan");
        save_run_file(&path, &baseline);

        let plan = load_run_plan_for_compare(&path);
        assert_eq!(plan.records[0].iterations_per_sample, 96);
        assert_eq!(plan.records[0].samples, 120);

        let mut router = Router::default();
        router.add("corpus", |bench| {
            bench.indexed_cyclic(32, |index| {
                black_box(index);
            });
        });

        let outcome = router.eval_inner(
            vec![
                "corpus".to_owned(),
                "--compare".to_owned(),
                path.display().to_string(),
            ],
            BenchBackend::fixed,
        );

        let _ = std::fs::remove_file(&path);
        let run = outcome.run_file.unwrap();
        assert_eq!(run.records[0].report.iterations_per_sample, 96);
        assert_eq!(run.records[0].report.sample_count(), 120);
    }

    #[test]
    fn save_writes_one_file_with_embedded_plan_and_noise() {
        let mut router = Router::default();
        router.add("foo", |bench| {
            bench.func(|| {});
        });

        let path = temp_run_path("single_artifact");
        let plan_path = appended_plan_path(&path);
        let _ = std::fs::remove_file(&path);
        let _ = std::fs::remove_file(&plan_path);

        let outcome = router.eval_inner(
            vec!["--save".to_owned(), path.display().to_string()],
            BenchBackend::fixed,
        );

        let contents = std::fs::read_to_string(&path).unwrap();
        let saved = load_run_file(&path);
        let plan = saved.plan.as_ref().expect("saved run should embed a plan");

        let _ = std::fs::remove_file(&path);
        let _ = std::fs::remove_file(&plan_path);

        assert_eq!(outcome.exit_code, 0);
        assert!(contents.starts_with("{\"plan\":"));
        assert!(!plan_path.exists());
        assert_eq!(saved.records.len(), 1);
        assert_eq!(
            plan.records[0].iterations_per_sample,
            saved.records[0].report.iterations_per_sample
        );
        assert_eq!(
            plan.records[0].samples,
            saved.records[0].report.sample_count()
        );
        assert!(plan.records[0].cycles_noise_log > 0.0);
        assert!(plan.records[0].inst_noise_log > 0.0);
    }

    #[test]
    fn compare_loads_full_baseline_only_after_current_measurement() {
        use std::sync::{
            Arc,
            atomic::{AtomicUsize, Ordering},
        };

        let mut baseline = test_run_file(vec![test_record_with_report(
            "foo",
            BenchReport::from_samples(vec![stat(10, 20, 30, 10); 120], 96, 0, 0, Stat::default()),
            Vec::new(),
        )]);
        baseline.plan = Some(run_plan_from_run_file(&baseline));
        let plan_json = jsony::to_json(&baseline.plan);
        let path = temp_run_path("malformed_records");
        std::fs::write(
            &path,
            format!(
                "{{\"plan\":{plan_json},\"format\":\"{RUN_FILE_FORMAT}\",\"version\":{RUN_FILE_VERSION},\"records\":["
            ),
        )
        .unwrap();

        let measured = Arc::new(AtomicUsize::new(0));
        let measured_in_bench = Arc::clone(&measured);
        let mut router = Router::default();
        router.add("foo", move |bench| {
            let measured = Arc::clone(&measured_in_bench);
            bench.func(move || {
                measured.fetch_add(1, Ordering::SeqCst);
            });
        });

        let result = std::panic::catch_unwind(std::panic::AssertUnwindSafe(|| {
            router.eval_inner(
                vec![
                    "foo".to_owned(),
                    "--compare".to_owned(),
                    path.display().to_string(),
                ],
                BenchBackend::fixed,
            )
        }));

        let _ = std::fs::remove_file(&path);
        assert!(result.is_err());
        assert_eq!(measured.load(Ordering::SeqCst), 2);
    }

    #[test]
    #[should_panic(expected = "indexed_cyclic cycle_len must be non-zero")]
    fn cyclic_indexed_rejects_empty_cycle() {
        let mut router = Router::default();
        router.add("corpus", |bench| {
            bench.indexed_cyclic(0, |_| {});
        });

        let _ = eval_fixed(&router, &[]);
    }

    #[test]
    fn std_normal_cdf_matches_known_values() {
        let cases = [
            (-3.0f64, 0.001_350f64),
            (-1.96, 0.025),
            (-1.0, 0.158_655),
            (0.0, 0.5),
            (1.0, 0.841_345),
            (1.96, 0.975),
            (3.0, 0.998_650),
        ];
        for (x, expected) in cases {
            let actual = std_normal_cdf(x);
            assert!(
                (actual - expected).abs() < 5e-4,
                "std_normal_cdf({x}) = {actual}, expected ~{expected}"
            );
        }
    }

    #[test]
    fn trimmed_log_estimate_handles_outliers() {
        let mut samples = vec![100.0f64; 200];
        // Inject 10 extreme outliers; they should be trimmed away.
        for index in 0..10 {
            samples.push(1_000_000.0 + index as f64);
        }
        let estimate = trimmed_log_estimate(&samples);
        let recovered = estimate.log_mean.exp();
        assert!(
            (recovered - 100.0).abs() < 0.5,
            "outliers leaked into estimate: {recovered}"
        );
    }

    #[test]
    fn trimmed_log_estimate_rejects_all_zero_samples() {
        let estimate = trimmed_log_estimate(&[0.0, 0.0, 0.0]);
        assert!(!estimate.log_mean.is_finite());
        assert!(!estimate.log_se.is_finite());

        let report =
            BenchReport::from_samples(vec![stat(10, 0, 0, 1); 64], 1, 0, 0, Stat::default());
        assert_eq!(report.estimate.cycles, Dec::default());
        assert!(!report.estimate_log_se.cycles.is_finite());
    }

    #[test]
    fn posterior_classifies_zero_variance_as_decisive() {
        // Identical baseline/current with a 5% gap and no within-sample
        // variance: must classify deterministically.
        let baseline = test_run_file(vec![test_record_with_stat(
            "x",
            stat(10_000, 20_000, 30_000, 10_000),
            Vec::new(),
        )]);
        let current = test_run_file(vec![test_record_with_stat(
            "x",
            stat(10_500, 21_000, 31_500, 10_500),
            Vec::new(),
        )]);

        let report = compare_runs(&baseline, &current);
        assert_eq!(report.entries[0].classification, "regression");
        assert_eq!(
            report.entries[0].cycles_p_regression.unwrap_or(0.0),
            1.0,
            "deterministic regression should report P(reg) = 1.0"
        );
    }

    #[test]
    fn posterior_holds_unchanged_when_below_threshold() {
        // 0.3% difference, well under the 1% default threshold.
        let baseline = test_run_file(vec![test_record_with_stat(
            "x",
            stat(10_000, 20_000, 30_000, 10_000),
            Vec::new(),
        )]);
        let current = test_run_file(vec![test_record_with_stat(
            "x",
            stat(10_030, 20_060, 30_090, 10_030),
            Vec::new(),
        )]);

        let report = compare_runs(&baseline, &current);
        assert_eq!(report.entries[0].classification, "unchanged");
    }

    #[test]
    fn compare_refuses_invalid_cycle_estimates() {
        let baseline = test_run_file(vec![test_record_with_stat(
            "x",
            stat(10_000, 0, 10_000, 10_000),
            Vec::new(),
        )]);
        let current = test_run_file(vec![test_record_with_stat(
            "x",
            stat(10_000, 1_000, 10_000, 10_000),
            Vec::new(),
        )]);

        let report = compare_runs(&baseline, &current);
        assert_eq!(report.entries[0].classification, "unchanged");
        assert_eq!(
            report.entries[0].classification_reason,
            "insufficient-or-invalid-cycles"
        );
        assert!(report.entries[0].cycles_p_regression.is_none());
    }

    #[test]
    fn compare_refuses_underflowed_reports() {
        let baseline = test_run_file(vec![test_record_with_stat(
            "x",
            stat(10_000, 20_000, 10_000, 10_000),
            Vec::new(),
        )]);
        let underflowed = BenchReport::from_samples(
            vec![stat(12_000, 24_000, 12_000, 12_000); 64],
            1,
            0,
            1,
            Stat::default(),
        );
        let current = test_run_file(vec![test_record_with_report("x", underflowed, Vec::new())]);

        let report = compare_runs(&baseline, &current);
        assert_eq!(report.entries[0].classification, "unchanged");
        assert_eq!(report.entries[0].classification_reason, "underflow");
    }

    #[test]
    fn cycles_only_change_with_stable_instructions_classifies() {
        let baseline = test_run_file(vec![test_record_with_stat(
            "x",
            stat(10_000, 20_000, 30_000, 10_000),
            Vec::new(),
        )]);
        let current = test_run_file(vec![test_record_with_stat(
            "x",
            stat(10_000, 21_000, 30_000, 10_000),
            Vec::new(),
        )]);

        let report = compare_runs(&baseline, &current);
        assert_eq!(report.entries[0].classification, "regression");
        assert_eq!(report.entries[0].classification_reason, "cycles-regression");
    }

    #[test]
    fn empirical_plan_noise_floor_tempers_cycles_only_changes() {
        let mut baseline = test_run_file(vec![test_record_with_stat(
            "x",
            stat(10_000, 20_000, 30_000, 10_000),
            Vec::new(),
        )]);
        let current = test_run_file(vec![test_record_with_stat(
            "x",
            stat(10_000, 20_400, 30_000, 10_000),
            Vec::new(),
        )]);
        let noisy_plan = RunPlan {
            format: RUN_PLAN_FORMAT.to_owned(),
            version: RUN_PLAN_VERSION,
            records: vec![RunPlanRecord {
                route: "x".to_owned(),
                params: Vec::new(),
                iterations_per_sample: 1,
                samples: 64,
                cycles_noise_log: 1.03f64.ln(),
                inst_noise_log: 0.0,
            }],
        };

        let decisive = compare_runs(&baseline, &current);
        assert_eq!(decisive.entries[0].classification, "regression");

        let tempered = compare_runs_with_threshold_and_plan(
            &baseline,
            &current,
            DEFAULT_THRESHOLD_PCT,
            Some(&noisy_plan),
        );
        assert_eq!(tempered.entries[0].classification, "unchanged");

        baseline.plan = Some(noisy_plan);
        let embedded = compare_runs(&baseline, &current);
        assert_eq!(embedded.entries[0].classification, "unchanged");
    }

    #[test]
    fn run_plan_records_observed_phase_delta_noise() {
        let baseline = test_run_file(vec![test_record_with_stat(
            "x",
            stat(10_000, 20_000, 30_000, 10_000),
            Vec::new(),
        )]);
        let current = test_run_file(vec![test_record_with_stat(
            "x",
            stat(10_000, 22_000, 30_300, 10_000),
            Vec::new(),
        )]);
        let plan = run_plan_from_run_pair(&baseline, &current);

        assert!((plan.records[0].cycles_noise_log - 1.10f64.ln()).abs() < 1e-12);
        assert!((plan.records[0].inst_noise_log - 1.01f64.ln()).abs() < 1e-12);
    }

    #[test]
    fn clear_opposite_instruction_move_vetoes_cycles_change() {
        let baseline = test_run_file(vec![test_record_with_stat(
            "x",
            stat(10_000, 20_000, 30_000, 10_000),
            Vec::new(),
        )]);
        let current = test_run_file(vec![test_record_with_stat(
            "x",
            stat(10_000, 21_000, 27_000, 10_000),
            Vec::new(),
        )]);

        let report = compare_runs(&baseline, &current);
        assert_eq!(report.entries[0].classification, "unchanged");
        assert_eq!(
            report.entries[0].classification_reason,
            "instruction-opposite"
        );
    }

    #[test]
    fn adjusted_generated_reports_are_diagnostic_only() {
        let baseline = test_run_file(vec![test_record_with_stat(
            "x",
            stat(10_000, 20_000, 30_000, 10_000),
            Vec::new(),
        )]);
        let adjusted = BenchReport::from_samples(
            vec![stat(12_000, 24_000, 30_000, 10_000); 64],
            1,
            0,
            0,
            stat(1, 1, 1, 1),
        );
        let current = test_run_file(vec![test_record_with_report("x", adjusted, Vec::new())]);

        let report = compare_runs(&baseline, &current);
        assert_eq!(report.entries[0].classification, "unchanged");
        assert_eq!(
            report.entries[0].classification_reason,
            "setup-adjusted-diagnostic"
        );
    }

    #[test]
    fn confidence_is_corrected_for_suite_size() {
        assert_eq!(corrected_confidence(0), POSTERIOR_CONFIDENCE);
        assert_eq!(corrected_confidence(1), POSTERIOR_CONFIDENCE);
        assert!(corrected_confidence(100) > POSTERIOR_CONFIDENCE);
    }

    #[test]
    fn router_runs_all_routes_by_default() {
        let mut router = Router::default();
        router.add("foo", |bench| {
            bench.func(|| {});
            bench.sub("bar").func(|| {});
            let mut next = 0usize;
            bench.sub("generated").generated(
                || {
                    next += 1;
                    next
                },
                |value| {
                    black_box(value);
                },
            );
        });
        router.add("other", |bench| {
            bench.func(|| {});
        });

        let outcome = eval_fixed(&router, &[]);
        let run = outcome.run_file.unwrap();

        assert_eq!(outcome.exit_code, 0);
        assert_eq!(
            run.records
                .iter()
                .map(|record| record.route.as_str())
                .collect::<Vec<_>>(),
            vec!["foo", "foo/bar", "foo/generated", "other"]
        );
    }

    #[test]
    fn router_filters_to_nested_route() {
        let mut router = Router::default();
        router.add("foo", |bench| {
            bench.func(|| {});
            bench.sub("bar").func(|| {});
            bench.sub("baz").func(|| {});
        });

        let outcome = eval_fixed(&router, &["foo/bar"]);
        let run = outcome.run_file.unwrap();

        assert_eq!(run.records.len(), 1);
        assert_eq!(run.records[0].route, "foo/bar");
    }

    #[test]
    fn unknown_route_exits_two() {
        let mut router = Router::default();
        router.add("foo", |bench| {
            bench.func(|| {});
        });

        let outcome = eval_fixed(&router, &["bar"]);

        assert_eq!(outcome.exit_code, 2);
        assert!(outcome.stderr.contains("unknown benchmark route"));
    }

    #[test]
    #[should_panic(expected = "duplicate benchmark route")]
    fn duplicate_top_route_panics() {
        let mut router = Router::default();
        router.add("foo", |_| {});
        router.add("foo", |_| {});
    }

    #[test]
    #[should_panic(expected = "must not contain '/'")]
    fn invalid_route_segment_panics() {
        let mut router = Router::default();
        router.add("foo/bar", |_| {});
    }

    #[test]
    fn parameter_expansion_and_filtering() {
        let mut router = Router::default();
        router.add("foo", |bench| {
            bench.param("size", [64, 1024], |bench, size| {
                bench.param_str("mode", ["fast", "full"], |bench, mode| {
                    bench.named("encode").func(|| {
                        black_box((size, &mode));
                    });
                });
            });
        });

        let all = eval_fixed(&router, &["foo"]);
        assert_eq!(all.run_file.as_ref().unwrap().records.len(), 4);

        let filtered = eval_fixed(
            &router,
            &["foo", "--param", "size=1024", "--param", "mode=fast"],
        );
        let records = filtered.run_file.unwrap().records;
        assert_eq!(records.len(), 1);
        assert_eq!(records[0].params[0], ParamValue::new("size", "1024"));
        assert_eq!(records[0].params[1], ParamValue::new("mode", "fast"));
    }

    #[test]
    fn run_file_json_roundtrip() {
        let run = test_run_file(vec![test_record(
            "foo/bar",
            10,
            vec![ParamValue::new("size", "64")],
        )]);

        let json = jsony::to_json(&run);
        let decoded = jsony::from_json::<RunFile>(&json).unwrap();

        assert_eq!(decoded, run);
    }

    #[test]
    fn compare_classifies_regressions_and_improvements() {
        let baseline = test_run_file(vec![
            test_record("same", 10_000, Vec::new()),
            test_record("faster", 10_000, Vec::new()),
            test_record("slower", 10_000, Vec::new()),
            test_record("missing", 10_000, Vec::new()),
        ]);
        let current = test_run_file(vec![
            test_record("same", 10_000, Vec::new()),
            test_record("faster", 9_500, Vec::new()),
            test_record("slower", 10_500, Vec::new()),
            test_record("new", 10_000, Vec::new()),
        ]);

        let report = compare_runs(&baseline, &current);
        let classes = report
            .entries
            .iter()
            .map(|entry| (entry.route.as_str(), entry.classification.as_str()))
            .collect::<Vec<_>>();

        assert_eq!(
            classes,
            vec![
                ("same", "unchanged"),
                ("faster", "improvement"),
                ("slower", "regression"),
            ]
        );
        assert_eq!(report.regression_count, 1);
        assert_eq!(report.improvement_count, 1);
        assert_eq!(report.missing_baseline[0].route, "new");
        assert_eq!(report.missing_current[0].route, "missing");
    }

    #[test]
    fn compare_classifies_by_cycles_not_instruction_count() {
        let baseline = test_run_file(vec![test_record_with_stat(
            "inst_only",
            stat(100, 1_000, 10_000, 100),
            Vec::new(),
        )]);
        let current = test_run_file(vec![test_record_with_stat(
            "inst_only",
            stat(100, 1_000, 10_500, 100),
            Vec::new(),
        )]);

        let report = compare_runs(&baseline, &current);

        assert_eq!(report.entries[0].classification, "unchanged");
        assert_eq!(report.regression_count, 0);
    }

    #[test]
    fn reporting_smoke_test() {
        let run = test_run_file(vec![test_record(
            "foo/bar",
            10,
            vec![ParamValue::new("size", "64")],
        )]);

        let output = render_run_table(&run);

        assert!(output.contains("route | params | cycles/op"));
        assert!(output.contains("foo/bar"));
        assert!(output.contains("size=64"));
    }

    #[test]
    fn compare_render_omits_unchanged_rows() {
        let baseline = test_run_file(vec![test_record("same", 10_000, Vec::new())]);
        let current = test_run_file(vec![test_record("same", 10_000, Vec::new())]);

        let output = render_compare_report(&compare_runs(&baseline, &current));

        assert!(output.contains("no significant changes"));
        assert!(!output.contains("same |"));
    }

    #[test]
    fn compare_warns_only_from_eval() {
        let mut router = Router::default();
        router.add("foo", |bench| {
            bench.func(|| {});
        });

        let dir = std::env::temp_dir();
        let path = dir.join(format!("jsony_bench_test_{}.json", std::process::id()));
        let mut baseline = test_run_file(vec![test_record("foo", 1, Vec::new())]);
        baseline.plan = Some(run_plan_from_run_file(&baseline));
        std::fs::write(&path, jsony::to_json(&baseline)).unwrap();

        let outcome = router.eval_inner(
            vec![
                "foo".to_owned(),
                "--compare".to_owned(),
                path.display().to_string(),
            ],
            BenchBackend::fixed,
        );

        let _ = std::fs::remove_file(&path);
        assert_eq!(outcome.exit_code, 0);
        assert!(outcome.compare_report.is_some());
    }

    #[test]
    #[ignore = "requires Linux perf_event_open permissions"]
    fn live_bencher_smoke() {
        let mut router = Router::default();
        router.add("foo", |bench| {
            bench.with_parameters(BenchParameters::QUICK).func(|| {
                black_box(1u64.wrapping_mul(2));
            });
        });

        let outcome = router.eval(vec!["foo".to_owned()]);
        let run = outcome.run_file.unwrap();

        assert_eq!(outcome.exit_code, 0);
        assert_eq!(run.records.len(), 1);
        assert_eq!(run.records[0].route, "foo");
        assert!(run.records[0].report.sample_count() >= BenchParameters::QUICK.min_samples);
    }
}
