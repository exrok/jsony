# Jsony

An **experimental** fast compiling serialization and deserialization rust library for JSON like formats.

[![Crates.io](https://img.shields.io/crates/v/jsony?style=flat-square)](https://crates.io/crates/jsony)
[![Crates.io](https://img.shields.io/docsrs/jsony?style=flat-square)](https://docs.rs/jsony/latest/jsony/)
[![License](https://img.shields.io/badge/license-MIT-blue?style=flat-square)](LICENSE)

**WARNING:** Jsony is currently in early development and makes extensive use of unsafe.

## Features

- Fast compile times <!-- Todo put link to benchmarks -->
- Competitive runtime performance
- Featureful derive macros for implementing To/From for various data formats
- Infallible serialization guaranteed to succeed via the type system
- Data formats:
  - **JSON** (optional extension: trailing commas, comments, unquoted keys)
  - **Compact Binary Encoding** with zerocopy and versioning support.
- Lazy JSON parser for efficiently extracting small fragments.
- JSON templating macros
- Encode directly to a file or stack allocated to buffer.

## Example

```rust
#[derive(Jsony, Debug)]
#[jsony(Json, tag = "kind")]
enum Status<'a> {
    Online,
    Error {
        #[jsony(default = i64::MAX)]
        code: i64,
        message: Cow<'a, str>,
        #[jsony(flatten, via = Iterator)]
        properties: Vec<(String, Data)>,
    },
    Offline,
}

#[derive(Jsony, Debug)]
#[jsony(Json, untagged)]
enum Data {
    Text(String),
    Array(Vec<Data>),
}

fn main() -> Result<(), Box<dyn std::error::Error>> {
    let input: String = jsony::object! {
        kind: "Error",
        code: 300,
        message: "System Failure",
        value: ["alpha", ["beta", "bravo"]],
    };

    let data: String = jsony::drill(&input)["value"][1][0].parse()?;
    assert_eq!(data, "beta");

    let status: Status = jsony::from_json(&input)?;

    assert_eq!(input, jsony::to_json(&status));

    Ok(())
}
```

## Acknowledgements

The derive feature set is largely based of `serde` and `serde_with`. <br>
The json parser is heavily inspire by `jiter` and `serde_json`.
