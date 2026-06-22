//! Benchmarks for `jsony` core deserialization paths.
//!
//! The `option` route targets the niche fast path in
//! `Option<T>::emplace_from_json`: when `Option<T>` is niche-optimized it
//! emplaces `T` directly into the destination instead of using a stack
//! `MaybeUninit<T>` plus a `size_of::<T>()` copy. The win scales with
//! `size_of::<T>()`, so the cases are reported separately:
//!
//! - `option/microbench/large_struct` isolates the elided copy with a large,
//!   niche-bearing `T`.
//! - `option/realistic/record` mixes niche `Option` fields as a real record
//!   type would.

use std::hint::black_box;

use jsony::Jsony;
use jsony_bench::{Bench, BenchParameters, Router};

fn main() {
    benchmark_router().eval_from_env();
}

fn benchmark_router() -> Router {
    let mut router = Router::default();
    router.add("option", bench_option);
    router
}

/// Large and niche-bearing: the `Box` first field is non-null, so
/// `Option<BigNiche>` is niche-optimized to `BigNiche`'s size, and the trailing
/// array makes the copy the fast path elides large (~136 bytes).
#[derive(Jsony)]
struct BigNiche {
    #[allow(dead_code)]
    tag: Box<u32>,
    #[allow(dead_code)]
    data: [u64; 16],
}

/// Same shape as `BigNiche` but much larger (~520 bytes), so the elided
/// `size_of::<T>()` copy dominates and the niche fast path's win is clearest.
#[derive(Jsony)]
struct HugeNiche {
    #[allow(dead_code)]
    tag: Box<u32>,
    #[allow(dead_code)]
    data: [u64; 64],
}

/// A record mixing several niche `Option` fields, the common real-world shape.
#[derive(Jsony)]
struct Record {
    #[allow(dead_code)]
    id: Option<Box<u64>>,
    #[allow(dead_code)]
    name: Option<String>,
    #[allow(dead_code)]
    tags: Option<Vec<String>>,
    #[allow(dead_code)]
    payload: Option<BigNiche>,
    #[allow(dead_code)]
    flag: bool,
}

const BIG_NICHE_JSON: &str = r#"{"tag":7,"data":[0,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15]}"#;

const RECORD_JSON: &str = r#"{"id":42,"name":"example record","tags":["alpha","beta","gamma"],"payload":{"tag":9,"data":[0,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15]},"flag":true}"#;

/// `{"tag":7,"data":[0,1,2,...,N-1]}` for a `[u64; N]` `data` field.
fn niche_json(n: usize) -> String {
    let mut json = String::from(r#"{"tag":7,"data":["#);
    for i in 0..n {
        if i != 0 {
            json.push(',');
        }
        json.push_str(&i.to_string());
    }
    json.push_str("]}");
    json
}

fn bench_option(bench: &mut Bench<'_>) {
    let mut bench = bench.with_parameters(BenchParameters::QUICK);

    // Isolates the elided stack temporary + `size_of::<BigNiche>()` copy.
    bench.named("microbench").named("large_struct").func(|| {
        let value: Option<BigNiche> = jsony::from_json(black_box(BIG_NICHE_JSON)).unwrap();
        black_box(value);
    });

    // Same path with a much larger `T`, where the elided copy dominates.
    let huge_json = niche_json(64);
    bench.named("microbench").named("huge_struct").func(|| {
        let value: Option<HugeNiche> = jsony::from_json(black_box(huge_json.as_str())).unwrap();
        black_box(value);
    });

    // Realistic record with a mix of niche `Option` fields.
    bench.named("realistic").named("record").func(|| {
        let value: Record = jsony::from_json(black_box(RECORD_JSON)).unwrap();
        black_box(value);
    });
}
