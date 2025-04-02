# CHANGELOG

# Unreleased

Enhancements:

- Added `owned_cow` and `json_string` for use in the `with` attribute.
- Added blanket ToJson/ToBinary impls for `Cow<'_, T>`.
- Introduced `to_binary_into` the binary version of `to_json_into`.
- `Vec<u8>` backed BytesWriter now commits the underlying buffer on drop
  if the `Vec<u8>` BytesWriter is leaked, the `Vec<u8>` will contain the
  same length, as it had when provided to the BytesWriter.
- More Compilation time reductions:
  - Removed ~200 functions calls in jsony_macros
  - Added specialization for Default::default() FromJson fields

# 0.1.1 (2025-04-01)

Enhancements:

- Relaxed Sized constraint on `jsony::to_json_into`.
- Added `#[jsony(alias = "...")]` struct field alias.
- Implemented ToBinary/FromBinary for more standard library types.

# 0.1.0 (2025-03-30)

The initial semi-stable release of jsony.
