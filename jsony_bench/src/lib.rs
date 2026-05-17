use std::{cell::Cell, hint::black_box, io, time::Duration};

use jsony_macros::Jsony;
use perf_event::{Builder, Counter, Group, GroupData, events::Hardware};

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
#[derive(Jsony, Debug, Clone)]
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

#[derive(Debug, Clone, Copy)]
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

pub static BIG_BENCH_PARAM: BenchParameters = BenchParameters {
    sample_target_duration_ns: nanos(Duration::from_millis(3)) / 2,
    max_sample_iterations: 2_000_000,
    min_sample_iterations: 3,
    max_samples: 250,
    min_samples: 50,
    target_duration_ns: nanos(Duration::from_millis(250)),
};

pub static DEFAULT_BENCH_PARAM: BenchParameters = BenchParameters {
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

    pub fn func(&mut self, func: impl FnMut()) -> Stat {
        self.func_report(func).estimate
    }

    pub fn func_with_parameters(&mut self, param: &BenchParameters, func: impl FnMut()) -> Stat {
        self.func_with_parameters_report(param, func).estimate
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

    pub fn bench_with_generator<T>(
        &mut self,
        generator: impl FnMut() -> T,
        func: impl FnMut(T),
    ) -> Stat {
        self.bench_with_generator_report(generator, func).estimate
    }

    pub fn bench_with_generator_with_parameters<T>(
        &mut self,
        param: &BenchParameters,
        generator: impl FnMut() -> T,
        func: impl FnMut(T),
    ) -> Stat {
        self.bench_with_generator_with_parameters_report(param, generator, func)
            .estimate
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
    #[ignore = "requires Linux perf_event_open permissions"]
    fn live_bencher_smoke() {
        let mut bencher = Bencher::new();
        let report = bencher.func_with_parameters_report(&BenchParameters::QUICK, || {
            black_box(1u64.wrapping_mul(2));
        });

        assert!(report.sample_count() >= BenchParameters::QUICK.min_samples);
        assert!(report.iterations_per_sample >= BenchParameters::QUICK.min_sample_iterations);
    }
}
