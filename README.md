# Jsony

An experimental fast-compiling serialization and deserialization library for JSON-like formats, very much a work in progress.

In the coming weeks, I hope to make significant improvements to the following:
- Soundness (less unsafe code, fuzzing, and extensive Miri tests)
- Missing features:
   - Support for most Serde attributes
   - Derive `IntoJson`
- Fix bugs
- Finalize API for "0.1"
- Documentation

## Weird things about the codebase if you're reading it

`jsony_macros` is generated from `jsony_macros_source` via `macro_preprocessor` with the following command:
```sh
cd ./macro_preprocessor
cargo run -- ../jsony_macros_source ../jsony_macros
```
Why? For a couple of reasons:
- As a micro-optimization to reduce compile time by expanding all macros
- While working on `jsony_macros_source`, you don't have to worry about rust-analyzer constantly recompiling proc-macros








