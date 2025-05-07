# CHANGELOG

# 0.1.4 (2025-05-07)

Enhancements:

- Additional helpers where added.
- Enum variant
- More defensive use of pointer arthimetic to avoid possiblity of UB

# 0.1.3 (2025-04-24)

Enhancements:

- Add `#[jsony(version)]` for binary versioning in FromBinary and ToBinary.
- Add `#[jsony(zerocopy)]` for deriving zerocopy support in FromBinary and ToBinary.
- Add FromBinary/ToBinary impls for `Cow<'_, [T]>` and `[T]` for zerocopy types.
- Avoid type change in compiler format error in template macros. (better compiler error messages)

# 0.1.2 (2025-04-05)

Enhancements:

- `ToStr/FromStr` enum derive support.
- Added `owned_cow` and `json_string` for use in the `with` attribute.
- Added blanket ToJson/ToBinary impls for `Cow<'_, T>`.
- Introduced `to_binary_into` the binary version of `to_json_into`.
- `Vec<u8>` backed BytesWriter now commits the underlying buffer on drop
  if the `Vec<u8>` BytesWriter is leaked, the `Vec<u8>` will contain the
  same length, as it had when provided to the BytesWriter.
- More Compilation time reductions:
  - Removed ~200 functions calls in jsony_macros
  - Added specialization for Default::default() FromJson fields
  - Type erased inner functions for `object_decode` and `impl<T> FromJson for Vec<T>`

Changes:

- `Hashmap::<Number, _>` now implements `ToJson` and `FromJson` via a new trait
  `JsonKeyKind` which is implemented for both `AlwaysNumber` and `AlwaysString`.

# 0.1.1 (2025-04-01)

Enhancements:

- Relaxed Sized constraint on `jsony::to_json_into`.
- Added `#[jsony(alias = "...")]` struct field alias.
- Implemented ToBinary/FromBinary for more standard library types.

# 0.1.0 (2025-03-30)

The initial semi-stable release of jsony.
