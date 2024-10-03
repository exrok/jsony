# Jsony

An experimental fast compiling serialization and deserialization libary for json like formats, very work in progress.

In the coming, weeks I hope make significant improvements to following:
- Soundness (Less unsafe, fuzzing & extensive miri tests)
- Missing feature
   - Support for most serde attributes
   - Derive IntoJson
- Fix Bugs
- Finialize API for "0.1"
- Documentation

## Weird things about the codebase if your reading it

- `json_macros` is generated from `json_macros_source` via `macro_preprocessor` with the following command:
```sh
cd ./macro_preprocessor
cargo run -- ../jsony_macros_source ../jsony_macros
```
Why? For a couple reasons:
   - as a micro optimization to reduce compile time by expanding all macros
   - While working on `jsony_macros_source` you don't have to worry about rust-analyzer constantly recompiling proc-macros







