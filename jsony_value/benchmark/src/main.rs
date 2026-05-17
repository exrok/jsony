use std::hint::black_box;

use jsony_bench::{Bench, BenchParameters, Router};
use jsony_value::{
    Value, ValueList, ValueMap, ValueMapBuilder, ValueNumber, ValueRef, ValueString,
};

const SHORT_TEXT: &str = "short borrowed text";
const MEDIUM_TEXT: &str =
    "jsony_value benchmark medium text with punctuation, spaces, and digits 0123456789";
const ESCAPED_TEXT: &str = "line one\nline two\twith tab \"quote\" and slash \\";

fn main() {
    benchmark_router().eval_from_env();
}

fn benchmark_router() -> Router {
    let mut router = Router::default();
    router
        .add("parse", bench_parse)
        .add("own", bench_ownership)
        .add("format", bench_format)
        .add("binary", bench_binary)
        .add("map", bench_map)
        .add("list", bench_list)
        .add("string", bench_string)
        .add("number", bench_number)
        .add("traverse", bench_traverse);
    router
}

fn bench_parse(bench: &mut Bench<'_>) {
    let mut bench = bench.with_parameters(BenchParameters::QUICK);
    let cases = json_cases();
    let names = case_names(&cases);

    bench
        .named("value")
        .param_str("doc", &names, |bench, name| {
            let case = find_case(&cases, &name);
            bench.func(|| {
                let value: Value<'_> = jsony::from_json(black_box(case.json.as_str())).unwrap();
                black_box(value);
            });
        });

    let map_cases = select_cases(
        &cases,
        &[
            "small_object",
            "large_object",
            "duplicate_keys",
            "nested_document",
        ],
    );
    let names = case_names(&map_cases);
    bench.named("map").param_str("doc", &names, |bench, name| {
        let case = find_case(&map_cases, &name);
        bench.func(|| {
            let map: ValueMap<'_> = jsony::from_json(black_box(case.json.as_str())).unwrap();
            black_box(map);
        });
    });

    let list_cases = select_cases(&cases, &["mixed_array", "number_array", "string_array"]);
    let names = case_names(&list_cases);
    bench.named("list").param_str("doc", &names, |bench, name| {
        let case = find_case(&list_cases, &name);
        bench.func(|| {
            let list: ValueList<'_> = jsony::from_json(black_box(case.json.as_str())).unwrap();
            black_box(list);
        });
    });
}

fn bench_ownership(bench: &mut Bench<'_>) {
    let mut bench = bench.with_parameters(BenchParameters::QUICK);
    let cases = json_cases();
    let names = case_names(&cases);

    bench
        .named("to_owned_from_parse")
        .param_str("doc", &names, |bench, name| {
            let case = find_case(&cases, &name);
            bench.func(|| {
                let value: Value<'_> = jsony::from_json(black_box(case.json.as_str())).unwrap();
                black_box(value.to_owned());
            });
        });

    let values = value_cases();
    let names = value_case_names(&values);
    bench
        .named("clone_borrowed")
        .param_str("value", &names, |bench, name| {
            let case = find_value_case(&values, &name);
            bench.func(|| {
                black_box(case.value.clone());
            });
        });

    let owned = value_cases()
        .into_iter()
        .map(|case| ValueCase {
            name: case.name,
            value: case.value.to_owned(),
        })
        .collect::<Vec<_>>();
    let names = value_case_names(&owned);
    bench
        .named("clone_owned")
        .param_str("value", &names, |bench, name| {
            let case = find_value_case(&owned, &name);
            bench.func(|| {
                black_box(case.value.clone());
            });
        });
}

fn bench_format(bench: &mut Bench<'_>) {
    let mut bench = bench.with_parameters(BenchParameters::QUICK);
    let values = value_cases();
    let names = value_case_names(&values);

    bench
        .named("display")
        .param_str("value", &names, |bench, name| {
            let case = find_value_case(&values, &name);
            bench.func(|| {
                black_box(case.value.to_string());
            });
        });

    bench
        .named("debug")
        .param_str("value", &names, |bench, name| {
            let case = find_value_case(&values, &name);
            bench.func(|| {
                black_box(format!("{:?}", black_box(&case.value)));
            });
        });
}

fn bench_binary(bench: &mut Bench<'_>) {
    let mut bench = bench.with_parameters(BenchParameters::QUICK);
    let values = value_cases()
        .into_iter()
        .map(|case| ValueCase {
            name: case.name,
            value: case.value.to_owned(),
        })
        .collect::<Vec<_>>();
    let names = value_case_names(&values);

    bench
        .named("encode")
        .param_str("value", &names, |bench, name| {
            let case = find_value_case(&values, &name);
            bench.func(|| {
                black_box(jsony::to_binary(black_box(&case.value)));
            });
        });

    let encoded = values
        .iter()
        .map(|case| BinaryCase {
            name: case.name,
            bytes: jsony::to_binary(&case.value),
        })
        .collect::<Vec<_>>();
    let names = binary_case_names(&encoded);
    bench
        .named("decode")
        .param_str("value", &names, |bench, name| {
            let case = find_binary_case(&encoded, &name);
            bench.func(|| {
                let value: Value<'_> =
                    jsony::from_binary(black_box(case.bytes.as_slice())).unwrap();
                black_box(value);
            });
        });

    bench
        .named("roundtrip")
        .param_str("value", &names, |bench, name| {
            let case = find_value_case(&values, &name);
            bench.func(|| {
                let bytes = jsony::to_binary(black_box(&case.value));
                let value: Value<'_> = jsony::from_binary(black_box(bytes.as_slice())).unwrap();
                black_box(value);
            });
        });
}

fn bench_map(bench: &mut Bench<'_>) {
    let mut bench = bench.with_parameters(BenchParameters::QUICK);
    bench
        .named("build_insert")
        .items("size", map_sizes(), |bench, size| {
            bench.func(|| {
                black_box(make_map_with_insert(black_box(size)));
            });
        });

    bench
        .named("build_builder")
        .items("size", map_sizes(), |bench, size| {
            bench.func(|| {
                black_box(make_map_with_builder(black_box(size)));
            });
        });

    bench
        .named("build_from_iter")
        .items("size", map_sizes(), |bench, size| {
            bench.func(|| {
                black_box(make_map_with_from_iter(black_box(size)));
            });
        });

    let maps = map_cases();
    let names = map_case_names(&maps);
    bench
        .named("get_hit")
        .param_str("shape", &names, |bench, name| {
            let case = find_map_case(&maps, &name);
            bench.func(|| {
                black_box(case.map.get(black_box(case.hit_key)));
            });
        });

    bench
        .named("get_miss")
        .param_str("shape", &names, |bench, name| {
            let case = find_map_case(&maps, &name);
            bench.func(|| {
                black_box(case.map.get(black_box("missing_key")));
            });
        });

    bench
        .named("index_hit")
        .param_str("shape", &names, |bench, name| {
            let case = find_map_case(&maps, &name);
            bench.func(|| {
                black_box(&case.map[black_box(case.hit_key)]);
            });
        });

    bench
        .named("get_all")
        .items("duplicates", [1usize, 2, 8, 32], |bench, duplicates| {
            let map = make_duplicate_map(duplicates);
            bench.func(|| {
                let count = map.get_all(black_box("dupe")).fold(0usize, |count, value| {
                    black_box(value);
                    count + 1
                });
                black_box(count);
            });
        });

    bench
        .named("entry_occupied")
        .items("size", map_sizes(), |bench, size| {
            bench.func(|| {
                let mut map = make_map_with_builder(size);
                black_box(map.entry(format!("key{:04}", size / 2)).or_default());
                black_box(map);
            });
        });

    bench
        .named("entry_vacant")
        .items("size", map_sizes(), |bench, size| {
            bench.func(|| {
                let mut map = make_map_with_builder(size);
                black_box(map.entry("inserted_key").or_insert(Value::from(17u64)));
                black_box(map);
            });
        });

    bench
        .named("remove_ordered")
        .items("size", map_sizes(), |bench, size| {
            bench.func(|| {
                let mut map = make_map_with_builder(size);
                black_box(map.remove(black_box(&format!("key{:04}", size / 2))));
                black_box(map);
            });
        });

    bench
        .named("remove_swap")
        .items("size", map_sizes(), |bench, size| {
            bench.func(|| {
                let mut map = make_map_with_builder(size);
                black_box(map.swap_remove(black_box(&format!("key{:04}", size / 2))));
                black_box(map);
            });
        });

    bench
        .named("sort")
        .items("size", map_sizes(), |bench, size| {
            bench.func(|| {
                let mut map = make_reverse_map(size);
                map.sort();
                black_box(map);
            });
        });
}

fn bench_list(bench: &mut Bench<'_>) {
    let mut bench = bench.with_parameters(BenchParameters::QUICK);
    bench
        .named("from_iter")
        .items("size", list_sizes(), |bench, size| {
            bench.func(|| {
                black_box(make_list_from_iter(black_box(size)));
            });
        });

    bench
        .named("with_capacity_push")
        .items("size", list_sizes(), |bench, size| {
            bench.func(|| {
                black_box(make_list_with_push(black_box(size)));
            });
        });

    let lists = list_cases();
    let names = list_case_names(&lists);
    bench
        .named("clone")
        .param_str("shape", &names, |bench, name| {
            let case = find_list_case(&lists, &name);
            bench.func(|| {
                black_box(case.list.clone());
            });
        });

    bench
        .named("scan_numbers")
        .param_str("shape", &names, |bench, name| {
            let case = find_list_case(&lists, &name);
            bench.func(|| {
                let mut sum = 0i64;
                for value in case.list.as_slice() {
                    sum = sum.wrapping_add(value.as_i64().unwrap_or_default());
                }
                black_box(sum);
            });
        });

    bench
        .named("to_vec")
        .param_str("shape", &names, |bench, name| {
            let case = find_list_case(&lists, &name);
            bench.func(|| {
                black_box(case.list.clone().to_vec());
            });
        });
}

fn bench_string(bench: &mut Bench<'_>) {
    let mut bench = bench.with_parameters(BenchParameters::QUICK);
    bench
        .named("from_borrowed")
        .param_str("text", string_names(), |bench, name| {
            let text = string_by_name(&name);
            bench.func(|| {
                black_box(ValueString::from_borrowed(black_box(text)));
            });
        });

    bench
        .named("from_owned")
        .param_str("text", string_names(), |bench, name| {
            let text = string_by_name(&name);
            bench.func(|| {
                black_box(ValueString::from_owned(black_box(
                    text.to_owned().into_boxed_str(),
                )));
            });
        });

    let strings = string_cases();
    let names = value_string_case_names(&strings);
    bench
        .named("clone_borrowed")
        .param_str("text", &names, |bench, name| {
            let case = find_value_string_case(&strings, &name);
            bench.func(|| {
                black_box(case.value.clone());
            });
        });

    let owned = strings
        .iter()
        .map(|case| ValueStringCase {
            name: case.name,
            value: case.value.clone().to_owned(),
        })
        .collect::<Vec<_>>();
    let names = value_string_case_names(&owned);
    bench
        .named("clone_owned")
        .param_str("text", &names, |bench, name| {
            let case = find_value_string_case(&owned, &name);
            bench.func(|| {
                black_box(case.value.clone());
            });
        });

    bench
        .named("display")
        .param_str("text", &names, |bench, name| {
            let case = find_value_string_case(&owned, &name);
            bench.func(|| {
                black_box(case.value.to_string());
            });
        });
}

fn bench_number(bench: &mut Bench<'_>) {
    let mut bench = bench.with_parameters(BenchParameters::QUICK);
    bench
        .named("from_i64")
        .items("kind", number_inputs(), |bench, input| {
            bench.func(|| {
                black_box(Value::from(black_box(input.as_i64)));
            });
        });

    bench
        .named("from_u64")
        .items("kind", number_inputs(), |bench, input| {
            bench.func(|| {
                black_box(Value::from(black_box(input.as_u64)));
            });
        });

    bench
        .named("from_f64")
        .items("kind", number_inputs(), |bench, input| {
            bench.func(|| {
                black_box(Value::from(black_box(input.as_f64)));
            });
        });

    let numbers = number_values();
    let names = number_case_names(&numbers);
    bench
        .named("as_i64")
        .param_str("kind", &names, |bench, name| {
            let case = find_number_case(&numbers, &name);
            bench.func(|| {
                black_box(case.value.as_i64());
            });
        });

    bench
        .named("as_u64")
        .param_str("kind", &names, |bench, name| {
            let case = find_number_case(&numbers, &name);
            bench.func(|| {
                black_box(case.value.as_u64());
            });
        });

    bench
        .named("as_f64")
        .param_str("kind", &names, |bench, name| {
            let case = find_number_case(&numbers, &name);
            bench.func(|| {
                black_box(case.value.as_f64());
            });
        });
}

fn bench_traverse(bench: &mut Bench<'_>) {
    let mut bench = bench.with_parameters(BenchParameters::QUICK);
    let values = value_cases();
    let names = value_case_names(&values);

    bench
        .named("count_nodes")
        .param_str("value", &names, |bench, name| {
            let case = find_value_case(&values, &name);
            bench.func(|| {
                black_box(count_nodes(black_box(&case.value)));
            });
        });

    bench
        .named("sum_numbers")
        .param_str("value", &names, |bench, name| {
            let case = find_value_case(&values, &name);
            bench.func(|| {
                black_box(sum_numbers(black_box(&case.value)));
            });
        });

    bench
        .named("sort_all_objects")
        .param_str("value", &names, |bench, name| {
            let case = find_value_case(&values, &name);
            bench.func(|| {
                let mut value = case.value.clone();
                value.sort_all_objects();
                black_box(value);
            });
        });
}

#[derive(Clone)]
struct JsonCase {
    name: &'static str,
    json: String,
}

struct ValueCase {
    name: &'static str,
    value: Value<'static>,
}

struct BinaryCase {
    name: &'static str,
    bytes: Vec<u8>,
}

struct MapCase {
    name: &'static str,
    map: ValueMap<'static>,
    hit_key: &'static str,
}

struct ListCase {
    name: &'static str,
    list: ValueList<'static>,
}

struct ValueStringCase {
    name: &'static str,
    value: ValueString<'static>,
}

#[derive(Clone, Copy)]
struct NumberInput {
    name: &'static str,
    as_i64: i64,
    as_u64: u64,
    as_f64: f64,
}

impl std::fmt::Display for NumberInput {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.write_str(self.name)
    }
}

struct NumberCase {
    name: &'static str,
    value: Value<'static>,
}

fn json_cases() -> Vec<JsonCase> {
    vec![
        JsonCase {
            name: "scalar_null",
            json: "null".to_owned(),
        },
        JsonCase {
            name: "scalar_bool",
            json: "true".to_owned(),
        },
        JsonCase {
            name: "integer",
            json: "1844674407370955161".to_owned(),
        },
        JsonCase {
            name: "float",
            json: "-12345.6789e-4".to_owned(),
        },
        JsonCase {
            name: "borrowed_string",
            json: r#""plain borrowed text without escapes""#.to_owned(),
        },
        JsonCase {
            name: "escaped_string",
            json: r#""line one\nline two\twith unicode \u263a and quote\"""#.to_owned(),
        },
        JsonCase {
            name: "mixed_array",
            json: make_mixed_array_json(64),
        },
        JsonCase {
            name: "number_array",
            json: make_number_array_json(512),
        },
        JsonCase {
            name: "string_array",
            json: make_string_array_json(192),
        },
        JsonCase {
            name: "small_object",
            json: make_object_json(8, false),
        },
        JsonCase {
            name: "large_object",
            json: make_object_json(160, false),
        },
        JsonCase {
            name: "duplicate_keys",
            json: make_duplicate_object_json(96),
        },
        JsonCase {
            name: "nested_document",
            json: make_nested_document_json(24),
        },
    ]
}

fn value_cases() -> Vec<ValueCase> {
    vec![
        ValueCase {
            name: "scalar",
            value: Value::from(42u64),
        },
        ValueCase {
            name: "string_borrowed",
            value: Value::from(ValueString::from_static(MEDIUM_TEXT)),
        },
        ValueCase {
            name: "list_small",
            value: Value::from(make_list_from_iter(16)),
        },
        ValueCase {
            name: "list_large",
            value: Value::from(make_list_from_iter(512)),
        },
        ValueCase {
            name: "map_small",
            value: Value::from(make_map_with_builder(8)),
        },
        ValueCase {
            name: "map_large",
            value: Value::from(make_map_with_builder(160)),
        },
        ValueCase {
            name: "map_duplicates",
            value: Value::from(make_duplicate_map(48)),
        },
        ValueCase {
            name: "nested",
            value: parse_static_value(&make_nested_document_json(24)),
        },
    ]
}

fn map_cases() -> Vec<MapCase> {
    vec![
        MapCase {
            name: "small_linear",
            map: make_map_with_builder(8),
            hit_key: "key0004",
        },
        MapCase {
            name: "large_indexed",
            map: make_map_with_builder(160),
            hit_key: "key0080",
        },
        MapCase {
            name: "duplicates",
            map: make_duplicate_map(48),
            hit_key: "dupe",
        },
    ]
}

fn list_cases() -> Vec<ListCase> {
    vec![
        ListCase {
            name: "small",
            list: make_list_from_iter(16),
        },
        ListCase {
            name: "medium",
            list: make_list_from_iter(128),
        },
        ListCase {
            name: "large",
            list: make_list_from_iter(1024),
        },
    ]
}

fn string_cases() -> Vec<ValueStringCase> {
    vec![
        ValueStringCase {
            name: "short",
            value: ValueString::from_static(SHORT_TEXT),
        },
        ValueStringCase {
            name: "medium",
            value: ValueString::from_static(MEDIUM_TEXT),
        },
        ValueStringCase {
            name: "escaped",
            value: ValueString::from_static(ESCAPED_TEXT),
        },
        ValueStringCase {
            name: "long",
            value: ValueString::from_owned(make_text(4096).into_boxed_str()),
        },
    ]
}

fn number_inputs() -> [NumberInput; 3] {
    [
        NumberInput {
            name: "small",
            as_i64: 17,
            as_u64: 17,
            as_f64: 17.0,
        },
        NumberInput {
            name: "large",
            as_i64: i64::MAX / 3,
            as_u64: u64::MAX / 3,
            as_f64: 9_007_199_254_740_991.0,
        },
        NumberInput {
            name: "negative",
            as_i64: -9_876_543_210,
            as_u64: 9_876_543_210,
            as_f64: -987_654.321,
        },
    ]
}

fn number_values() -> Vec<NumberCase> {
    vec![
        NumberCase {
            name: "i64",
            value: Value::from(-9_876_543_210i64),
        },
        NumberCase {
            name: "u64",
            value: Value::from(9_876_543_210u64),
        },
        NumberCase {
            name: "f64",
            value: Value::from(987_654.321f64),
        },
        NumberCase {
            name: "other_integer",
            value: Value::from(ValueString::other_borrowed("18446744073709551616000")).to_owned(),
        },
        NumberCase {
            name: "other_float",
            value: Value::from(ValueString::other_borrowed("-123.456e78")).to_owned(),
        },
    ]
}

fn map_sizes() -> [usize; 5] {
    [0, 4, 8, 32, 160]
}

fn list_sizes() -> [usize; 5] {
    [0, 4, 32, 256, 1024]
}

fn string_names() -> [&'static str; 4] {
    ["short", "medium", "escaped", "long"]
}

fn string_by_name(name: &str) -> &'static str {
    match name {
        "short" => SHORT_TEXT,
        "medium" => MEDIUM_TEXT,
        "escaped" => ESCAPED_TEXT,
        "long" => LONG_TEXT,
        _ => panic!("unknown string case {name}"),
    }
}

const LONG_TEXT: &str = "jsony_value benchmark long static text: abcdefghijklmnopqrstuvwxyz0123456789 abcdefghijklmnopqrstuvwxyz0123456789 abcdefghijklmnopqrstuvwxyz0123456789 abcdefghijklmnopqrstuvwxyz0123456789 abcdefghijklmnopqrstuvwxyz0123456789 abcdefghijklmnopqrstuvwxyz0123456789";

fn make_map_with_insert(size: usize) -> ValueMap<'static> {
    let mut map = ValueMap::new();
    for i in 0..size {
        map.insert(
            ValueString::from_owned(format!("key{i:04}").into_boxed_str()),
            Value::from(i as u64),
        );
    }
    map
}

fn make_map_with_builder(size: usize) -> ValueMap<'static> {
    let mut builder = ValueMapBuilder::new();
    for i in 0..size {
        builder.insert(
            ValueString::from_owned(format!("key{i:04}").into_boxed_str()),
            make_mixed_value(i),
        );
    }
    builder.build()
}

fn make_map_with_from_iter(size: usize) -> ValueMap<'static> {
    (0..size)
        .map(|i| (format!("key{i:04}"), make_mixed_value(i)))
        .collect()
}

fn make_reverse_map(size: usize) -> ValueMap<'static> {
    let mut builder = ValueMapBuilder::new();
    for i in (0..size).rev() {
        builder.insert(
            ValueString::from_owned(format!("key{i:04}").into_boxed_str()),
            Value::from(i as u64),
        );
    }
    builder.build()
}

fn make_duplicate_map(duplicates: usize) -> ValueMap<'static> {
    let mut builder = ValueMapBuilder::new();
    for i in 0..duplicates {
        builder.insert(ValueString::from_static("dupe"), Value::from(i as u64));
        builder.insert(
            ValueString::from_owned(format!("side{i:04}").into_boxed_str()),
            Value::from((i * 3) as u64),
        );
    }
    builder.build()
}

fn make_list_from_iter(size: usize) -> ValueList<'static> {
    (0..size).map(make_mixed_value).collect()
}

fn make_list_with_push(size: usize) -> ValueList<'static> {
    let mut list = ValueList::with_capacity(size as u32);
    for i in 0..size {
        list.push(make_mixed_value(i));
    }
    list
}

fn make_mixed_value(index: usize) -> Value<'static> {
    match index % 6 {
        0 => Value::from(index as u64),
        1 => Value::from(-(index as i64)),
        2 => Value::from((index as f64) * 1.25),
        3 => Value::from(index % 2 == 0),
        4 => Value::from(ValueString::from_static("static-string")),
        _ => Value::NULL,
    }
}

fn parse_static_value(json: &str) -> Value<'static> {
    jsony::from_json::<Value<'_>>(json).unwrap().to_owned()
}

fn make_mixed_array_json(size: usize) -> String {
    let mut json = String::from("[");
    for i in 0..size {
        if i != 0 {
            json.push(',');
        }
        match i % 6 {
            0 => json.push_str(&(i * 11).to_string()),
            1 => json.push_str(if i % 2 == 0 { "true" } else { "false" }),
            2 => json.push_str("\"plain text\""),
            3 => json.push_str("null"),
            4 => json.push_str(&format!("{{\"id\":{i},\"name\":\"item{i}\"}}")),
            _ => json.push_str(&format!("[{i},{}]", i + 1)),
        }
    }
    json.push(']');
    json
}

fn make_number_array_json(size: usize) -> String {
    let mut json = String::from("[");
    for i in 0..size {
        if i != 0 {
            json.push(',');
        }
        match i % 4 {
            0 => json.push_str(&(i as u64 * 1_000_003).to_string()),
            1 => json.push_str(&format!("-{}", i as i64 * 97)),
            2 => json.push_str(&format!("{}.{}", i * 13, i % 100)),
            _ => json.push_str(&format!("{}e-3", i * 17)),
        }
    }
    json.push(']');
    json
}

fn make_string_array_json(size: usize) -> String {
    let mut json = String::from("[");
    for i in 0..size {
        if i != 0 {
            json.push(',');
        }
        if i % 7 == 0 {
            json.push_str(&format!(r#""escaped\nitem\t{i}""#));
        } else {
            json.push_str(&format!(r#""item-{i:04}-plain-text""#));
        }
    }
    json.push(']');
    json
}

fn make_object_json(size: usize, reverse: bool) -> String {
    let mut json = String::from("{");
    let iter: Box<dyn Iterator<Item = usize>> = if reverse {
        Box::new((0..size).rev())
    } else {
        Box::new(0..size)
    };
    for (position, i) in iter.enumerate() {
        if position != 0 {
            json.push(',');
        }
        json.push_str(&format!(r#""key{i:04}":"#));
        match i % 5 {
            0 => json.push_str(&(i * 17).to_string()),
            1 => json.push_str(&format!(r#""value-{i:04}""#)),
            2 => json.push_str(if i % 2 == 0 { "true" } else { "false" }),
            3 => json.push_str("null"),
            _ => json.push_str(&format!(r#"[{i},{},"x"]"#, i + 1)),
        }
    }
    json.push('}');
    json
}

fn make_duplicate_object_json(size: usize) -> String {
    let mut json = String::from("{");
    for i in 0..size {
        if i != 0 {
            json.push(',');
        }
        if i % 3 == 0 {
            json.push_str(&format!(r#""dupe":{i}"#));
        } else {
            json.push_str(&format!(r#""key{i:04}":{i}"#));
        }
    }
    json.push('}');
    json
}

fn make_nested_document_json(width: usize) -> String {
    let mut sections = String::new();
    for i in 0..width {
        if i != 0 {
            sections.push(',');
        }
        sections.push_str(&format!(
            r#"{{"id":{i},"name":"section-{i:04}","enabled":{},"values":[{},{},{}],"meta":{{"rank":{},"tag":"tag-{}"}}}}"#,
            if i % 2 == 0 { "true" } else { "false" },
            i,
            i + 1,
            i + 2,
            width - i,
            i % 5
        ));
    }
    format!(
        r#"{{"title":"benchmark document","version":3,"sections":[{sections}],"summary":{{"count":{width},"ok":true}}}}"#
    )
}

fn make_text(size: usize) -> String {
    const CHUNK: &str = "abcdefghijklmnopqrstuvwxyz0123456789";
    let mut output = String::with_capacity(size);
    while output.len() < size {
        output.push_str(CHUNK);
    }
    output.truncate(size);
    output
}

fn count_nodes(value: &Value<'_>) -> usize {
    match value.as_ref() {
        ValueRef::Null(_)
        | ValueRef::Boolean(_)
        | ValueRef::Number(_)
        | ValueRef::String(_)
        | ValueRef::Other(_) => 1,
        ValueRef::List(list) => 1 + list.as_slice().iter().map(count_nodes).sum::<usize>(),
        ValueRef::Map(map) => {
            1 + map
                .entries()
                .iter()
                .map(|(_, value)| count_nodes(value))
                .sum::<usize>()
        }
    }
}

fn sum_numbers(value: &Value<'_>) -> i64 {
    match value.as_ref() {
        ValueRef::Number(ValueNumber::I64(value)) => *value,
        ValueRef::Number(ValueNumber::U64(value)) => (*value).min(i64::MAX as u64) as i64,
        ValueRef::Number(ValueNumber::F64(value)) => *value as i64,
        ValueRef::List(list) => list
            .as_slice()
            .iter()
            .map(sum_numbers)
            .fold(0i64, i64::wrapping_add),
        ValueRef::Map(map) => map
            .entries()
            .iter()
            .map(|(_, value)| sum_numbers(value))
            .fold(0i64, i64::wrapping_add),
        _ => 0,
    }
}

fn case_names(cases: &[JsonCase]) -> Vec<&'static str> {
    cases.iter().map(|case| case.name).collect()
}

fn value_case_names(cases: &[ValueCase]) -> Vec<&'static str> {
    cases.iter().map(|case| case.name).collect()
}

fn binary_case_names(cases: &[BinaryCase]) -> Vec<&'static str> {
    cases.iter().map(|case| case.name).collect()
}

fn map_case_names(cases: &[MapCase]) -> Vec<&'static str> {
    cases.iter().map(|case| case.name).collect()
}

fn list_case_names(cases: &[ListCase]) -> Vec<&'static str> {
    cases.iter().map(|case| case.name).collect()
}

fn value_string_case_names(cases: &[ValueStringCase]) -> Vec<&'static str> {
    cases.iter().map(|case| case.name).collect()
}

fn number_case_names(cases: &[NumberCase]) -> Vec<&'static str> {
    cases.iter().map(|case| case.name).collect()
}

fn select_cases(cases: &[JsonCase], names: &[&str]) -> Vec<JsonCase> {
    names
        .iter()
        .map(|name| find_case(cases, name).clone())
        .collect()
}

fn find_case<'a>(cases: &'a [JsonCase], name: &str) -> &'a JsonCase {
    cases
        .iter()
        .find(|case| case.name == name)
        .unwrap_or_else(|| panic!("unknown json case {name}"))
}

fn find_value_case<'a>(cases: &'a [ValueCase], name: &str) -> &'a ValueCase {
    cases
        .iter()
        .find(|case| case.name == name)
        .unwrap_or_else(|| panic!("unknown value case {name}"))
}

fn find_binary_case<'a>(cases: &'a [BinaryCase], name: &str) -> &'a BinaryCase {
    cases
        .iter()
        .find(|case| case.name == name)
        .unwrap_or_else(|| panic!("unknown binary case {name}"))
}

fn find_map_case<'a>(cases: &'a [MapCase], name: &str) -> &'a MapCase {
    cases
        .iter()
        .find(|case| case.name == name)
        .unwrap_or_else(|| panic!("unknown map case {name}"))
}

fn find_list_case<'a>(cases: &'a [ListCase], name: &str) -> &'a ListCase {
    cases
        .iter()
        .find(|case| case.name == name)
        .unwrap_or_else(|| panic!("unknown list case {name}"))
}

fn find_value_string_case<'a>(cases: &'a [ValueStringCase], name: &str) -> &'a ValueStringCase {
    cases
        .iter()
        .find(|case| case.name == name)
        .unwrap_or_else(|| panic!("unknown string case {name}"))
}

fn find_number_case<'a>(cases: &'a [NumberCase], name: &str) -> &'a NumberCase {
    cases
        .iter()
        .find(|case| case.name == name)
        .unwrap_or_else(|| panic!("unknown number case {name}"))
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn generated_json_cases_parse() {
        for case in json_cases() {
            let parsed: Value<'_> = jsony::from_json(&case.json)
                .unwrap_or_else(|err| panic!("case {} failed to parse: {err:?}", case.name));
            assert!(count_nodes(&parsed) > 0);
        }
    }

    #[test]
    fn map_fixtures_cover_linear_and_indexed_paths() {
        let cases = map_cases();
        assert_eq!(cases[0].map.entries().len(), 8);
        assert!(cases[1].map.entries().len() > 8);
        assert!(cases[2].map.get_all("dupe").count() > 1);
    }

    #[test]
    fn traversal_fixtures_do_real_work() {
        let nested = parse_static_value(&make_nested_document_json(6));
        assert!(count_nodes(&nested) > 30);
        assert!(sum_numbers(&nested) > 0);
    }

    #[test]
    fn benchmark_routes_are_registered() {
        let output = benchmark_router().eval(["--help".to_owned()]).stdout;
        assert!(output.contains("usage: benchmark"));

        let unknown = benchmark_router().eval(["missing".to_owned()]);
        assert_eq!(unknown.exit_code, 2);
        assert!(unknown.stderr.contains("known routes"));
    }
}
