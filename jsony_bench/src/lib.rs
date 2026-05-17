use std::{
    cell::Cell,
    collections::{BTreeMap, BTreeSet},
    fmt::Display,
    hint::black_box,
    io,
    path::{Path, PathBuf},
    time::{Duration, SystemTime, UNIX_EPOCH},
};

use jsony_macros::Jsony;
use perf_event::{Builder, Counter, Group, GroupData, events::Hardware};

pub type RouteId = String;

const RUN_FILE_FORMAT: &str = "jsony_bench.run";
const RUN_FILE_VERSION: u32 = 1;
const COMPARE_THRESHOLD_PERCENT: f64 = 5.0;

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

        let mut context = RunContext {
            backend: backend(),
            route_filter: options.route_filter.clone(),
            param_filters: options.param_filters.clone(),
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

        let run_file = RunFile {
            format: RUN_FILE_FORMAT.to_owned(),
            version: RUN_FILE_VERSION,
            command: args,
            route_filter: options.route_filter.clone(),
            param_filters: options.param_filters.clone(),
            created_unix_ms: unix_now_ms(),
            records: context.records,
        };

        if run_file.records.is_empty() {
            return EvalOutcome::error(
                2,
                "no benchmarks matched the selected route and parameter filters\n".to_owned(),
            );
        }

        let compare_report = options.compare.as_ref().map(|path| {
            let baseline = load_run_file(path);
            compare_runs(&baseline, &run_file)
        });

        let mut stdout = render_run_table(&run_file);
        if let Some(compare_report) = &compare_report {
            stdout.push('\n');
            stdout.push_str(&render_compare_report(compare_report));
        }

        if let Some(path) = &options.save {
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
}

impl<'a> Bench<'a> {
    fn new(context: &'a mut RunContext, path: Vec<String>) -> Self {
        Self {
            context,
            path,
            params: Vec::new(),
            bench_parameters: DEFAULT_BENCH_PARAM,
        }
    }

    pub fn func(&mut self, mut func: impl FnMut()) -> &mut Self {
        if self.context.should_measure(&self.path, &self.params) {
            let report = self
                .context
                .backend
                .measure_func(&self.bench_parameters, &mut func);
            self.context.push_record(RunRecord {
                route: self.path_string(),
                params: self.params.clone(),
                report,
                bench_parameters: self.bench_parameters,
            });
        }
        self
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
            if self.context.param_value_possible(&name, &value_text) {
                let mut child = Bench {
                    context: self.context,
                    path: self.path.clone(),
                    params: pushed_param(&self.params, &name, &value_text),
                    bench_parameters: self.bench_parameters,
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
        }
    }

    fn path_string(&self) -> String {
        self.path.join("/")
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
#[derive(Jsony, Debug, Clone, PartialEq, Eq)]
#[jsony(Json)]
pub struct RunRecord {
    pub route: RouteId,
    pub params: Vec<ParamValue>,
    pub report: BenchReport,
    pub bench_parameters: BenchParameters,
}

#[must_use]
#[derive(Jsony, Debug, Clone, PartialEq, Eq)]
#[jsony(Json)]
pub struct RunFile {
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
    pub time_ratio: Option<f64>,
    pub cycles_ratio: Option<f64>,
    pub time_delta_percent: Option<f64>,
    pub cycles_delta_percent: Option<f64>,
    pub classification: String,
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
#[derive(Jsony, Debug, Clone, PartialEq, Eq)]
#[jsony(Json)]
pub struct BenchReport {
    pub estimate: Stat,
    pub median: Stat,
    pub mean: Stat,
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
        let mean = Stat::mean(&samples);
        let min = Stat::min(&samples);
        let max = Stat::max(&samples);
        let median_abs_deviation = Stat::median_abs_deviation(&samples, median);

        Self {
            estimate: median,
            median,
            mean,
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
            "{} samples x {} iterations: estimate=({}) mean=({}) mad=({})",
            self.samples.len(),
            self.iterations_per_sample,
            self.estimate,
            self.mean,
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
        sample_target_duration_ns: nanos(Duration::from_micros(250)),
        max_sample_iterations: 50_000,
        min_sample_iterations: 1,
        max_samples: 100,
        min_samples: 50,
        target_duration_ns: nanos(Duration::from_millis(10)),
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
    }
}

pub const BIG_BENCH_PARAM: BenchParameters = BenchParameters {
    sample_target_duration_ns: nanos(Duration::from_millis(3)) / 2,
    max_sample_iterations: 2_000_000,
    min_sample_iterations: 3,
    max_samples: 250,
    min_samples: 50,
    target_duration_ns: nanos(Duration::from_millis(250)),
};

pub const DEFAULT_BENCH_PARAM: BenchParameters = BenchParameters {
    sample_target_duration_ns: nanos(Duration::from_millis(1)),
    max_sample_iterations: 1_000_000,
    min_sample_iterations: 1,
    max_samples: 100,
    min_samples: 10,
    target_duration_ns: nanos(Duration::from_millis(150)),
};

impl Default for Bencher {
    fn default() -> Self {
        Self::new()
    }
}

impl Bencher {
    pub fn new() -> Bencher {
        let mut bencher =
            Self::try_uncalibrated().expect("failed to create perf counter group for jsony_bench");
        bencher.calibrate();
        bencher
    }

    pub fn try_new() -> io::Result<Bencher> {
        let mut bencher = Self::try_uncalibrated()?;
        bencher.try_calibrate()?;
        Ok(bencher)
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
        self.func_with_parameters_report(&DEFAULT_BENCH_PARAM, func)
    }

    pub fn func_with_parameters_report(
        &mut self,
        param: &BenchParameters,
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
        )
    }

    pub fn bench_with_generator_report<T>(
        &mut self,
        generator: impl FnMut() -> T,
        func: impl FnMut(T),
    ) -> BenchReport {
        self.bench_with_generator_with_parameters_report(&BIG_BENCH_PARAM, generator, func)
    }

    pub fn bench_with_generator_with_parameters_report<T>(
        &mut self,
        param: &BenchParameters,
        mut generator: impl FnMut() -> T,
        mut func: impl FnMut(T),
    ) -> BenchReport {
        param.validate();

        let run_target = Cell::new(false);
        let mut bench = |bencher: &mut Bencher, iterations: u64| {
            let time = std::time::Instant::now();
            bencher
                .perf_group
                .enable()
                .expect("failed to enable perf counter group for benchmark sample");
            for _ in 0..iterations {
                let mut input = generator();
                black_box(&mut input);
                if run_target.get() {
                    func(input);
                }
            }
            bencher
                .perf_group
                .disable()
                .expect("failed to disable perf counter group for benchmark sample");
            time.elapsed()
        };

        let mut bench = BenchFunc(&mut bench);
        run_target.set(true);
        let (iterations, warmup_samples) =
            self.stabilize_iterations(&mut bench, param, Stat::default());

        let time = std::time::Instant::now();
        let mut samples = Vec::with_capacity(param.max_samples);
        let mut adjustment_samples = Vec::with_capacity(param.max_samples);
        let mut underflow_samples = 0usize;

        while samples.len() < param.max_samples {
            run_target.set(false);
            let adjustment = bench.sample(self, iterations, Stat::default()).normalized;
            adjustment_samples.push(adjustment);

            run_target.set(true);
            let measured = bench.sample(self, iterations, Stat::default()).normalized;
            let (net, underflow) = measured.saturating_sub_with_underflow(adjustment);
            if underflow.any() {
                underflow_samples += 1;
            }
            samples.push(net);

            let elapsed = nanos(time.elapsed());
            if elapsed >= param.target_duration_ns && samples.len() >= param.min_samples {
                break;
            }
        }

        BenchReport::from_samples(
            samples,
            iterations,
            warmup_samples,
            underflow_samples,
            Stat::median(&adjustment_samples),
        )
    }

    #[inline(never)]
    fn bench_internal(
        &mut self,
        mut func: BenchFunc<'_, '_>,
        param: &BenchParameters,
    ) -> BenchReport {
        param.validate();
        let (iterations, warmup_samples) =
            self.stabilize_iterations(&mut func, param, Stat::default());

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
            if elapsed >= param.target_duration_ns && samples.len() >= param.min_samples {
                break;
            }
        }

        BenchReport::from_samples(
            samples,
            iterations,
            warmup_samples,
            underflow_samples,
            Stat::default(),
        )
    }

    fn stabilize_iterations(
        &mut self,
        func: &mut BenchFunc<'_, '_>,
        param: &BenchParameters,
        per_iteration_baseline: Stat,
    ) -> (u64, usize) {
        let mut iterations = param.min_sample_iterations;
        let warmup_samples = 8;

        for _ in 0..warmup_samples {
            let sample = func.sample(self, iterations, per_iteration_baseline);
            let raw_nanos_per_iter = nanos(sample.raw_elapsed) / iterations.max(1);
            let target = (param.sample_target_duration_ns / raw_nanos_per_iter.max(1))
                .clamp(param.min_sample_iterations, param.max_sample_iterations);
            iterations = target.min(
                iterations
                    .saturating_mul(8)
                    .max(param.min_sample_iterations),
            );
        }

        (iterations, warmup_samples)
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
    records: Vec<RunRecord>,
    record_keys: BTreeSet<String>,
}

impl RunContext {
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

    fn param_value_possible(&self, name: &str, value: &str) -> bool {
        self.param_filters
            .iter()
            .all(|filter| filter.name != name || filter.value == value)
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
    #[cfg(test)]
    Fixed {
        next: u64,
    },
}

impl BenchBackend {
    fn live() -> Self {
        Self::Live(Bencher::new())
    }

    #[cfg(test)]
    fn fixed() -> Self {
        Self::Fixed { next: 1 }
    }

    fn measure_func(
        &mut self,
        parameters: &BenchParameters,
        func: &mut dyn FnMut(),
    ) -> BenchReport {
        match self {
            BenchBackend::Live(bencher) => bencher.func_with_parameters_report(parameters, func),
            #[cfg(test)]
            BenchBackend::Fixed { next } => {
                func();
                let base = *next;
                *next += 1;
                BenchReport::from_samples(
                    vec![
                        stat_for_testish(base * 10),
                        stat_for_testish(base * 10 + 2),
                        stat_for_testish(base * 10 + 1),
                    ],
                    parameters.min_sample_iterations,
                    0,
                    0,
                    Stat::default(),
                )
            }
        }
    }
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
            raw.normalize(bencher.baseline, per_iteration_baseline, iterations);

        BenchSample {
            raw_elapsed: elapsed,
            normalized,
            underflow,
        }
    }
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

#[derive(Debug, Clone)]
struct CliOptions {
    route_filter: Option<String>,
    save: Option<PathBuf>,
    compare: Option<PathBuf>,
    param_filters: Vec<ParamValue>,
    help: bool,
}

impl CliOptions {
    fn parse(args: &[String]) -> Result<Self, String> {
        let mut route_filter = None;
        let mut save = None;
        let mut compare = None;
        let mut param_filters = Vec::new();
        let mut help = false;

        let mut index = 0;
        while index < args.len() {
            match args[index].as_str() {
                "--help" | "-h" => {
                    help = true;
                    index += 1;
                }
                "--save" => {
                    index += 1;
                    let value = args
                        .get(index)
                        .ok_or_else(|| "--save requires a path".to_owned())?;
                    save = Some(PathBuf::from(value));
                    index += 1;
                }
                "--compare" => {
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

        Ok(Self {
            route_filter,
            save,
            compare,
            param_filters,
            help,
        })
    }
}

pub fn compare_runs(baseline: &RunFile, current: &RunFile) -> CompareReport {
    assert_run_file_supported(baseline);
    assert_run_file_supported(current);

    let mut baseline_by_key = BTreeMap::new();
    for record in &baseline.records {
        baseline_by_key.insert(record_key(&record.route, &record.params), record);
    }

    let mut seen = BTreeSet::new();
    let mut entries = Vec::new();
    let mut missing_baseline = Vec::new();

    for current_record in &current.records {
        let key = record_key(&current_record.route, &current_record.params);
        if let Some(baseline_record) = baseline_by_key.get(&key) {
            seen.insert(key);
            entries.push(compare_records(baseline_record, current_record));
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
    output.push_str("route | params | ns/op | mad | cycles | inst | branch | /sec\n");
    output.push_str("----- | ------ | ----- | --- | ------ | ---- | ------ | ----\n");
    for record in &run_file.records {
        output.push_str(&format!(
            "{} | {} | {} | {} | {} | {} | {} | {:.2}\n",
            record.route,
            format_params(&record.params),
            record.report.estimate.nanos,
            record.report.median_abs_deviation.nanos,
            record.report.estimate.cycles,
            record.report.estimate.inst,
            record.report.estimate.branch,
            record.report.estimate.per_second(),
        ));
    }
    output
}

pub fn render_compare_report(report: &CompareReport) -> String {
    let mut output = String::new();
    output.push_str("compare | params | time delta | cycles delta | class\n");
    output.push_str("------- | ------ | ---------- | ------------ | -----\n");
    for entry in &report.entries {
        output.push_str(&format!(
            "{} | {} | {} | {} | {}\n",
            entry.route,
            format_params(&entry.params),
            format_percent(entry.time_delta_percent),
            format_percent(entry.cycles_delta_percent),
            entry.classification,
        ));
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

fn compare_records(baseline: &RunRecord, current: &RunRecord) -> CompareEntry {
    let time_ratio = ratio(
        current.report.estimate.nanos,
        baseline.report.estimate.nanos,
    );
    let cycles_ratio = ratio(
        current.report.estimate.cycles,
        baseline.report.estimate.cycles,
    );
    let time_delta_percent = time_ratio.map(|ratio| (ratio - 1.0) * 100.0);
    let cycles_delta_percent = cycles_ratio.map(|ratio| (ratio - 1.0) * 100.0);
    let classification = classify_change(time_delta_percent, cycles_delta_percent).to_owned();

    CompareEntry {
        route: current.route.clone(),
        params: current.params.clone(),
        baseline: baseline.report.clone(),
        current: current.report.clone(),
        time_ratio,
        cycles_ratio,
        time_delta_percent,
        cycles_delta_percent,
        classification,
    }
}

fn classify_change(time_delta: Option<f64>, cycles_delta: Option<f64>) -> &'static str {
    if time_delta.is_some_and(|delta| delta >= COMPARE_THRESHOLD_PERCENT)
        || cycles_delta.is_some_and(|delta| delta >= COMPARE_THRESHOLD_PERCENT)
    {
        "regression"
    } else if time_delta.is_some_and(|delta| delta <= -COMPARE_THRESHOLD_PERCENT)
        || cycles_delta.is_some_and(|delta| delta <= -COMPARE_THRESHOLD_PERCENT)
    {
        "improvement"
    } else {
        "unchanged"
    }
}

fn ratio(current: Dec, baseline: Dec) -> Option<f64> {
    if baseline.0 == 0 {
        return None;
    }
    Some(current.0 as f64 / baseline.0 as f64)
}

fn save_run_file(path: &Path, run_file: &RunFile) {
    let mut json = jsony::to_json(run_file);
    json.push('\n');
    std::fs::write(path, json)
        .unwrap_or_else(|err| panic!("failed to save benchmark run to {}: {err}", path.display()));
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
    "usage: benchmark [route] [--param key=value] [--save path] [--compare path]\n".to_owned()
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
        RunRecord {
            route: route.to_owned(),
            params,
            report: BenchReport::from_samples(
                vec![stat(nanos, nanos * 2, nanos * 3, nanos)],
                1,
                0,
                0,
                Stat::default(),
            ),
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
    fn report_estimate_is_median_and_tracks_mad() {
        let report = BenchReport::from_samples(
            vec![
                stat(10, 20, 30, 40),
                stat(12, 24, 32, 42),
                stat(100, 200, 300, 400),
            ],
            7,
            8,
            1,
            stat(2, 3, 4, 5),
        );

        assert_eq!(report.estimate, stat(12, 24, 32, 42));
        assert_eq!(report.median_abs_deviation, stat(2, 4, 2, 2));
        assert_eq!(report.iterations_per_sample, 7);
        assert_eq!(report.warmup_samples, 8);
        assert_eq!(report.underflow_samples, 1);
        assert_eq!(report.adjustment, stat(2, 3, 4, 5));
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
    fn router_runs_all_routes_by_default() {
        let mut router = Router::default();
        router.add("foo", |bench| {
            bench.func(|| {});
            bench.sub("bar").func(|| {});
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
            vec!["foo", "foo/bar", "other"]
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
            test_record("same", 100, Vec::new()),
            test_record("faster", 100, Vec::new()),
            test_record("slower", 100, Vec::new()),
            test_record("missing", 100, Vec::new()),
        ]);
        let current = test_run_file(vec![
            test_record("same", 102, Vec::new()),
            test_record("faster", 90, Vec::new()),
            test_record("slower", 106, Vec::new()),
            test_record("new", 100, Vec::new()),
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
    fn reporting_smoke_test() {
        let run = test_run_file(vec![test_record(
            "foo/bar",
            10,
            vec![ParamValue::new("size", "64")],
        )]);

        let output = render_run_table(&run);

        assert!(output.contains("route | params | ns/op"));
        assert!(output.contains("foo/bar"));
        assert!(output.contains("size=64"));
    }

    #[test]
    fn compare_warns_only_from_eval() {
        let mut router = Router::default();
        router.add("foo", |bench| {
            bench.func(|| {});
        });

        let dir = std::env::temp_dir();
        let path = dir.join(format!("jsony_bench_test_{}.json", std::process::id()));
        let baseline = test_run_file(vec![test_record("foo", 1, Vec::new())]);
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
