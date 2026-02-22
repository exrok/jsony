use std::{cell::Cell, hint::black_box, time::Duration};

use perf_event::{Builder, Counter, Group, events::Hardware};

pub struct Bencher {
    perf_group: Group,
    cycle_counter: Counter,
    instruction_counter: Counter,
    branch_counter: Counter,
    baseline: Stat,
    per_iteration_baseline: Stat,
}

#[repr(transparent)]
#[derive(Clone, Copy, PartialEq, Eq, Hash, Default)]
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
    pub fn u64_floor(self) -> u64 {
        self.0 >> 8
    }
}

impl From<u64> for Dec {
    fn from(value: u64) -> Self {
        Dec(value << 8)
    }
}
impl From<Dec> for f64 {
    fn from(value: Dec) -> Self {
        value.0 as f64 / 256.0
    }
}

impl std::ops::Add<Dec> for Dec {
    type Output = Dec;

    fn add(self, rhs: Dec) -> Self::Output {
        Dec(self.0 + rhs.0)
    }
}
impl std::ops::AddAssign<Dec> for Dec {
    fn add_assign(&mut self, rhs: Dec) {
        self.0 += rhs.0;
    }
}
impl std::ops::Sub<Dec> for Dec {
    type Output = Dec;

    fn sub(self, rhs: Dec) -> Self::Output {
        Dec(self.0 - rhs.0)
    }
}
impl std::ops::Div<u64> for Dec {
    type Output = Dec;

    fn div(self, rhs: u64) -> Self::Output {
        Dec(self.0 / rhs)
    }
}

impl std::ops::Div<u64> for Stat {
    type Output = Stat;

    fn div(self, rhs: u64) -> Self::Output {
        Stat {
            nanos: self.nanos / rhs,
            cycles: self.cycles / rhs,
            inst: self.inst / rhs,
            branch: self.branch / rhs,
        }
    }
}

#[derive(Default, Debug, Clone)]
pub struct Stat {
    pub nanos: Dec,
    pub cycles: Dec,
    pub inst: Dec,
    pub branch: Dec,
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
    // Assuming that stat corresponds to single item, this gives
    // you how many items could be processed per seconds using the nano stat
    pub fn per_second(&self) -> f64 {
        let nanos = self.nanos.0;
        if nanos == 0 {
            return 0.0;
        }
        1e9 / (f64::from(self.nanos))
    }
    pub fn mean(stats: &[Stat]) -> Stat {
        if stats.is_empty() {
            return Stat::default();
        }
        let mut total = Stat::default();
        for stat in stats {
            total.nanos += stat.nanos;
            total.cycles += stat.cycles;
            total.inst += stat.inst;
            total.branch += stat.branch;
        }
        let len = stats.len() as u64;
        total.nanos = Dec(total.nanos.0 / len);
        total.cycles = Dec(total.cycles.0 / len);
        total.inst = Dec(total.inst.0 / len);
        total.branch = Dec(total.branch.0 / len);
        return total;
    }
    pub fn normalize(&mut self, b: &Bencher, iterations: u64) {
        self.nanos = Dec(
            (self.nanos.0.saturating_sub(b.baseline.nanos.0) / iterations)
                .saturating_sub(b.per_iteration_baseline.nanos.0),
        );
        self.cycles = Dec(
            (self.cycles.0.saturating_sub(b.baseline.cycles.0) / iterations)
                .saturating_sub(b.per_iteration_baseline.cycles.0),
        );
        self.inst = Dec((self.inst.0.saturating_sub(b.baseline.inst.0) / iterations)
            .saturating_sub(b.per_iteration_baseline.inst.0));
        self.branch = Dec(
            (self.branch.0.saturating_sub(b.baseline.branch.0) / iterations)
                .saturating_sub(b.per_iteration_baseline.branch.0),
        );
    }
}

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
        sample_target_duration_ns: Duration::from_micros(250).as_nanos() as u64,
        max_sample_iterations: 50_000,
        min_sample_iterations: 1,
        max_samples: 100,
        min_samples: 50,
        target_duration_ns: Duration::from_millis(10).as_nanos() as u64,
    };
}

pub static BIG_BENCH_PARAM: BenchParameters = BenchParameters {
    sample_target_duration_ns: Duration::from_millis(3).as_nanos() as u64 / 2,
    max_sample_iterations: 2_000_000,
    min_sample_iterations: 3,
    max_samples: 250,
    min_samples: 50,
    target_duration_ns: Duration::from_millis(250).as_nanos() as u64,
};

pub static DEFAULT_BENCH_PARAM: BenchParameters = BenchParameters {
    sample_target_duration_ns: Duration::from_millis(1).as_nanos() as u64,
    max_sample_iterations: 1_000_000,
    min_sample_iterations: 1,
    max_samples: 100,
    min_samples: 10,
    target_duration_ns: Duration::from_millis(150).as_nanos() as u64,
};

impl Bencher {
    pub fn new() -> Bencher {
        let mut group = Group::new().unwrap();
        let cycle_counter = group.add(&Builder::new(Hardware::CPU_CYCLES)).unwrap();
        let instruction_counter = group.add(&Builder::new(Hardware::INSTRUCTIONS)).unwrap();
        let branch_counter = group
            .add(&Builder::new(Hardware::BRANCH_INSTRUCTIONS))
            .unwrap();
        Bencher {
            perf_group: group,
            cycle_counter,
            instruction_counter,
            branch_counter,
            baseline: Stat::default(),
            per_iteration_baseline: Stat::default(),
        }
    }
    pub fn calibrate(&mut self) {
        let iterations = 1000;
        let mut times: Vec<u32> = Vec::with_capacity(iterations);
        let mut cycles: Vec<u32> = Vec::with_capacity(iterations);
        let mut min_inst = u64::MAX;
        let mut min_branch = u64::MAX;
        for _ in 0..iterations {
            self.perf_group.reset().unwrap();
            let time = std::time::Instant::now();
            let _ = self.perf_group.enable();
            let _ = self.perf_group.disable();
            let elapsed_time = time.elapsed();
            let counts = self.perf_group.read().unwrap();
            times.push(elapsed_time.as_nanos() as u32);
            cycles.push(counts[&self.cycle_counter] as u32);
            min_inst = counts[&self.instruction_counter].min(min_inst);
            min_branch = counts[&self.branch_counter].min(min_branch);
        }
        times.sort_unstable();
        cycles.sort_unstable();
        let median_time = times[times.len() / 2];
        let median_cycles = cycles[cycles.len() / 2];
        self.baseline = Stat {
            nanos: (median_time as u64).into(),
            cycles: (median_cycles as u64).into(),
            inst: min_inst.into(),
            branch: min_branch.into(),
        };
    }

    #[inline(never)]
    fn bench_internal(
        &mut self,
        mut func: BenchFunc<'_, '_>,
        param: &BenchParameters,
    ) -> Vec<Stat> {
        // Phase 1: Warmup — find stable iteration count.
        // Uses raw wall-clock time (not normalized stat) so that heavy
        // generators in bench_with_generator are correctly accounted for.
        // Growth is capped at 8x per step so a single timing anomaly
        // can never cause a catastrophically slow next sample.
        let mut iterations = param.min_sample_iterations;
        for _ in 0..8 {
            let (raw_elapsed, _) = func.sample(self, iterations);
            let raw_nanos_per_iter = raw_elapsed.as_nanos() as u64 / iterations.max(1);
            let target = (param.sample_target_duration_ns / raw_nanos_per_iter.max(1))
                .clamp(param.min_sample_iterations, param.max_sample_iterations);
            iterations = target.min(
                iterations
                    .saturating_mul(8)
                    .max(param.min_sample_iterations),
            );
        }

        // Phase 2: Measurement — collect samples at the stabilized iteration count.
        let time = std::time::Instant::now();
        let mut samples = Vec::new();
        loop {
            let (_raw, stat) = func.sample(self, iterations);
            samples.push(stat);
            let elapsed = time.elapsed().as_nanos() as u64;
            if (elapsed > param.target_duration_ns && samples.len() > param.min_samples)
                || samples.len() > param.max_samples
            {
                break;
            }
        }
        samples
    }

    pub fn func(&mut self, mut func: impl FnMut()) -> Stat {
        let samples = self.bench_internal(
            BenchFunc(&mut move |bencher: &mut Bencher, iterations: u64| {
                let time = std::time::Instant::now();
                let _ = bencher.perf_group.enable();
                for _ in 0..iterations {
                    func();
                }
                let _ = bencher.perf_group.disable();
                time.elapsed()
            }),
            &DEFAULT_BENCH_PARAM,
        );

        Stat::mean(&samples)
    }

    pub fn bench_with_generator<T>(
        &mut self,
        mut generator: impl FnMut() -> T,
        mut func: impl FnMut(T),
    ) -> Stat {
        let on = &Cell::new(false);
        let mut bench = move |bencher: &mut Bencher, iterations: u64| {
            let time = std::time::Instant::now();
            let _ = bencher.perf_group.enable();
            for _ in 0..iterations {
                let mut input = generator();
                black_box(&mut input);
                if on.get() {
                    func(input);
                }
            }
            let _ = bencher.perf_group.disable();
            time.elapsed()
        };
        {
            let samples = self.bench_internal(BenchFunc(&mut bench), &DEFAULT_BENCH_PARAM);
            self.per_iteration_baseline = Stat::mean(&samples);
        }
        on.set(true);
        let samples = self.bench_internal(BenchFunc(&mut bench), &BIG_BENCH_PARAM);
        self.per_iteration_baseline = Default::default();
        Stat::mean(&samples)
    }
}

struct BenchFunc<'a, 'b>(&'a mut (dyn 'b + FnMut(&mut Bencher, u64) -> Duration));

impl<'a, 'b> BenchFunc<'a, 'b> {
    /// Returns (raw_elapsed, normalized_stat).
    /// raw_elapsed is the wall-clock time for the entire sample (all iterations).
    fn sample(&mut self, bencher: &mut Bencher, iterations: u64) -> (Duration, Stat) {
        bencher.perf_group.reset().unwrap();
        let elapsed = self.0(bencher, iterations);
        let counts = bencher.perf_group.read().unwrap();
        let mut stat = Stat {
            nanos: (elapsed.as_nanos() as u64).into(),
            cycles: counts[&bencher.cycle_counter].into(),
            inst: counts[&bencher.instruction_counter].into(),
            branch: counts[&bencher.branch_counter].into(),
        };
        stat.normalize(&bencher, iterations);
        (elapsed, stat)
    }
}
