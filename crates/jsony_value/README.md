# jsony_value: Memory-efficient JSON value types for jsony.

**WARNING**: This crate is still experimental and makes extensive usage of unsafe.

Values use a 16-byte tagged union representation, allowing
[`Option<Value>`] to also be 16 bytes through niche optimization.

See: [jsony](https://crates.io/crates/jsony) for full JSON parsing and serialization.

# Key Types

- [`Value`] - Tagged union representing any JSON value
- [`ValueString`] - Strings that can be borrowed or owned
- [`ValueNumber`] - Discriminated union of `U64`, `I64`, `F64`
- [`ValueList`] - Growable array of values
- [`ValueMap`] - JSON object support duplicate keys

# Examples

```rust
use jsony_value::{Value, ValueString, ValueMap, ValueList};

// Create values from Rust types
let num: Value = 42i64.into();
let text: Value = "hello".into();
let flag: Value = true.into();

// Build a JSON object
let map: Value = [
    ("name", Value::from("Alice")),
    ("age", Value::from(30i64)),
].into_iter().collect();
```

# Acknowledgements

This project uses large parts of the hashbrown crates internals to build it's multimap.
