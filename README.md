# Jsony

An **experimental** fast compiling serialization and deserialization rust library for JSON like formats.

[![Crates.io](https://img.shields.io/crates/v/jsony?style=flat-square)](https://crates.io/crates/jsony)
[![Crates.io](https://img.shields.io/docsrs/jsony?style=flat-square)](https://docs.rs/jsony/latest/jsony/)
[![License](https://img.shields.io/badge/license-MIT-blue?style=flat-square)](LICENSE)

**WARNING:** Jsony is currently only a prototype, makes extensive use of unsafe and lacks extensive testing.
It is not recommended to be used for external facing systems at this time.

## Features

- Fast compile times <!-- Todo put link to benchmarks -->
- Competitive runtime performance
- Featureful derive macros for implementing To/From for various data formats
- Encoding checked at compile time, infallible at runtime
- Data formats
  - JSON (optional extension: trailing commas, comments, unquoted keys)
  - Custom Binary Encoding
- Lazy JSON parser for efficiently extracting small fragments.
- JSON templating macros
- Encode directly to a file or stack allocated to buffer.

## Example

```rust
use jsony::Jsony;

#[derive(Jsony, Debug)]
#[jsony(FromJson, ToJson, tag = "kind")]
enum Status<'a> {
    Online,
    Error {
        #[jsony(default = i64::MAX)]
        code: i64,
        message: Cow<'a, str>,
        #[jsony(flatten, via = Iterator)]
        properties: Vec<(String, JsonItem<'a>)>,
    },
    Offline,
}

fn main() -> Result<(), Box<dyn std::error::Error>> {
    let input: String = jsony::object! {
        kind: "Error",
        code: 300,
        message: "System Failure",
        data: [1, 2, 3],
        value: {
          previous: {
            kind: "Offline",
          }
        }
    };

    let previous: Status = jsony::drill(&input)["value"]["previous"].parse()?;
    assert!(matches!(previous, Status::Offline));

    let status: Status = jsony::from_json(&input)?;

    assert_eq!(
      input,
      jsony::to_json(&status)
    );

    Ok(())
}
```

## Acknowledgements

The derive feature set is largely based of `serde` and `serde_with`. <br>
The json parser is heavily inspire by `jiter` and `serde_json`.
