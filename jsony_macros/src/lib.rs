extern crate proc_macro;
mod ast;
mod case;
mod codegen;
mod lit;
mod template;
mod util;
mod writer;

use proc_macro::{Delimiter, Group, Ident, Literal, Punct, Spacing, Span, TokenStream, TokenTree};

#[derive(Copy, Clone, PartialEq, Eq)]
pub(crate) enum Flatten {
    None,
    Object,
    Array,
}

#[derive(Debug)]
struct InnerError {
    span: Span,
    message: String,
}

#[derive(Debug)]
pub(crate) struct Error(InnerError);

impl Error {
    pub(crate) fn to_compiler_error(&self, wrap: bool) -> TokenStream {
        let mut group = TokenTree::Group(Group::new(
            Delimiter::Parenthesis,
            TokenStream::from_iter([TokenTree::Literal(Literal::string(&self.0.message))]),
        ));
        let mut punc = TokenTree::Punct(Punct::new('!', Spacing::Alone));
        punc.set_span(self.0.span);
        group.set_span(self.0.span);

        if wrap {
            TokenStream::from_iter([TokenTree::Group(Group::new(
                Delimiter::Brace,
                TokenStream::from_iter([
                    TokenTree::Ident(Ident::new("compile_error", self.0.span)),
                    punc,
                    group,
                    TokenTree::Punct(Punct::new(';', Spacing::Alone)),
                    TokenTree::Ident(Ident::new("String", self.0.span)),
                    TokenTree::Punct(Punct::new(':', Spacing::Joint)),
                    TokenTree::Punct(Punct::new(':', Spacing::Alone)),
                    TokenTree::Ident(Ident::new("new", self.0.span)),
                    TokenTree::Group(Group::new(Delimiter::Parenthesis, TokenStream::new())),
                ]),
            ))])
        } else {
            TokenStream::from_iter([
                TokenTree::Ident(Ident::new("compile_error", self.0.span)),
                punc,
                group,
                TokenTree::Punct(Punct::new(';', Spacing::Alone)),
            ])
        }
    }
    pub(crate) fn msg(message: &str) -> Error {
        Error(InnerError {
            span: Span::call_site(),
            message: message.to_string(),
        })
    }
    pub(crate) fn msg_ctx(message: &str, fmt: &dyn std::fmt::Display) -> Error {
        Error(InnerError {
            span: Span::call_site(),
            message: format!("{message}: {fmt}"),
        })
    }
    pub(crate) fn span_msg(message: &str, span: Span) -> Error {
        Error(InnerError {
            span,
            message: message.to_string(),
        })
    }
    pub(crate) fn span_msg_ctx(message: &str, fmt: &dyn std::fmt::Display, span: Span) -> Error {
        Error(InnerError {
            span,
            message: format!("{}: {}", fmt, message),
        })
    }
}

#[proc_macro]
pub fn array(input: TokenStream) -> TokenStream {
    template::array(input)
}

/// Templating macro for creating JSON objects
///
/// ```ignore
/// let _player: String = jsony::object!{
///     name: "Jimmy",
///     health: 100,
/// };
/// let array_example: String = jsony::array![
///     true,
///     // Flatten arrays and loops
///     ..[for i in 0..10; i]
///     // Match Projections
///     match 4 {
///         0 => true,
///         1 => {key: "value"},
///         other => other,
///     }
/// ];
///
/// println!("array_example:\n{array_example}")
///
/// let map: std::collections::BTreeMap::<&str, u32> = [
///     ("alpha", 1),
///     ("beta", 2),
/// ].into_iter().collect();
///
/// let value = Some("data");
/// let key = "hello";
/// let map: std::collections::BTreeMap<&str, u32> =
///     [("alpha", 1), ("beta", 2)].into_iter().collect();
///
/// let object_example: String = jsony::object! {
///     // flaten objects value into object
///     ..map,
///
///     [key]: "variable-key",
///
///     // Conditional field guards
///     @[if let Some(data) = value]
///     "data-len": data.len()
/// };
///
/// println!("object_example:\n{object_example}")
/// ```
#[proc_macro]
pub fn object(input: TokenStream) -> TokenStream {
    template::object(input)
}

/// Unified derive macro for implementing various conversion traits
///
/// Configured with attributes in the form `#[jsony(...)]` on containers,
/// variants, and fields.
///
/// The traits/methods to derive are specified via container attributes. The currently
/// supported traits/methods are:
/// - `ToJson`
/// - `FromJson`
/// - `ToBinary`
/// - `FromBinary`
/// - `FromStr`
/// - `ToStr` (inherent method)
///
/// The rest of the attributes are described in the following tables. Note that some
/// attributes apply only to certain traits.
///
/// ## Container Attributes
/// These are `jsony` attributes that appear above a `struct` or `enum`.
///
/// | Format | Supported Traits | Description |
/// |--------|------------------|-------------|
/// | `content = "..."` | `Json` | Field containing the data content of enum.
/// | `Flattenable` | `FromJson` | Allows type to use `#[jsony(flatten)]` in `FromJson`.
/// | `ignore_tag_adjacent_fields` | `Json` | Ignore extra fields in externally tagged enum object
/// | `rename_all = "..."` | All | Renames variants and fields not explicitly renamed.
/// | `tag = "..."` | `Json` | Field containing the enum variant.
/// | `transparent` | All | Traits delegate to the single inner type.
/// | `untagged` | `Json` | Only data content of an enum is stored.
/// | `version [= N]`   | `Binary` | Enable binary versioning. Sets current version (default 0 or largest field version). |
/// | `version = M..`   | `Binary` | Current version is `M` or largest field version,  error on attempt to decode versions less then M |
/// | `version = M..=N` | `Binary` | Current version is `N`, error on attempt to decode versions less then M|
/// | `zerocopy` | `Binary` | Enables zerocopy support and asserts requirements are met.
///
/// ## Enum Variant Attributes
/// These are `jsony` attributes that appear above a variant in an enum.
///
/// | Format | Supported Traits | Description |
/// |-------------------|--------|---------------------------------------------------------------------------------|
/// | `rename = "..."`  | `Json` | Use provided string as variant name.
/// | `other`           | `FromJson` | Variant to use if given an unknown variant
///
/// ## Field Attributes
/// These are `jsony` attributes that appear above a field inside a struct or enum variant.
///
/// | Format | Supported Traits    | Description |
/// |-------------------|----------|---------------------------------------------------------------------------------|
/// | `alias = "..."`   |`FromJson`| Use provided string as a alternative field name when decoding.
/// | `default [= ...]` | `From`   | Use `Default::default()` or provided expression if field is missing.
/// | `flatten`         | `Json`   | Flatten the contents of the field into the container it is defined in.
/// | `rename = "..."`  | `Json`   | Use provided string as field name.
/// | `version = N`     | `Binary` | Field added in version `N`. Needs `default` when reading older versions.        |
/// | `via = ...`       | All      | Implement conversion through provided trait.
/// | `skip`            | All      | Omit field while serializing, use default value when deserializing.
/// | `skip_if = ...`   | `ToJson`  | Omit field while serializing if provided function returns true.
/// | `with = ...`      | All      | Use methods from specified module instead of trait. [read more](#jsonywith---on-fields)
///
/// ## Format Aliases
/// In the container attributes to specify the traits to derive and on the prefix of
/// other attributes to specify which traits that attribute should apply to, you can use
/// the following aliases to specify multiple traits at once:
///
/// | Alias | Traits Included | Description |
/// |-------|-----------------|-------------|
/// | `To` | `ToJson`, `ToBinary` | All traits converting a Rust type to serialized format
/// | `From` | `FromJson`, `FromBinary` | All traits converting from a serialized format to a Rust type
/// | `Binary` | `ToBinary`, `FromBinary` | All Binary encoding traits
/// | `Json` | `ToJson`, `FromJson` | All JSON encoding traits
/// | `Str` | `ToStr`, `FromStr` | Enum between string slice converting function
///
/// As more formats are added, these aliases may expand. If an attribute only supports a subset
/// of traits specified by the set, then the rest are ignored. If the set specified is disjoint, an error
/// will be raised. For example:
///
/// `#[jsony(To flatten)]` is the same as `#[jsony(ToJson flatten)]` as `ToBinary` does not support `flatten`.
/// Whereas `#[jsony(Binary flatten)]` is a compile-time error since `flatten` is not supported by either `ToBinary` nor `FromBinary`.
///
/// ### Detailed Field Attributes Descriptions
///
/// #### `#[jsony(with = ...)]` on fields
///
/// Uses the functions from the provided module path when a encoding/decoding trait method would normally be used.
///
/// The function corresponds to the save methods of each trait (omitting the `__jsony` suffix if present).
///
/// | Trait | Function |
/// |-------|-----------------|
/// | `ToJson` | `fn encode_json(value: &bool, output: &mut TextWriter)`
/// | `FromJson` | `fn decode_json<'a>(parser: &mut jsony::parser::Parser<'a>) -> Result<bool, &'static DecodeError>`
/// | `ToBinary` | `fn encode_binary(value: &bool, output: &mut BytesWriter)`
/// | `FromBinary` | `fn decode_binary(decoder: &mut jsony::binary::Decoder<'_>) -> bool`
///
/// ##### Example of `with` attribute
/// ```ignore
/// mod bool_as_int {
///     use jsony::{
///         json::DecodeError, BytesWriter, FromBinary, FromJson, TextWriter, ToBinary, ToJson,
///     };
///     pub fn encode_json(value: &bool, output: &mut TextWriter) {
///         (*value as u32).encode_json__jsony(output);
///     }
///     pub fn decode_json(parser: &mut jsony::parser::Parser<'_>) -> Result<bool, &'static DecodeError> {
///         Ok(<u32>::decode_json(parser)? != 0)
///     }
///     pub fn encode_binary(value: &bool, output: &mut BytesWriter) {
///         (*value as u32).encode_binary(output)
///     }
///     pub fn decode_binary(decoder: &mut jsony::binary::Decoder<'_>) -> bool {
///         u32::decode_binary(decoder) != 0
///     }
/// }
///
/// #[derive(Jsony)]
/// #[jsony(Binary, Json)]
/// struct Example {
///     #[jsony(with = bool_as_int)]
///     value: bool
/// }
/// ```
/// Note: The functions in the with module can be generic.
///
/// #### `#[jsony(skip)]`
///
/// Omit the field while serializing and use the default value when deserializing. If no default value is specified with `default` then
/// `Default::default()` is used.
///
/// `skip` is useful when you need a field on the rust side that isn't present in the serialized data format.
///
/// ### Detailed Container Attributes Descriptions
/// #### `#[jsony(transparent)]`
///
/// Must be used on a struct containing a single field. If `#[repr(transparent)]` is also specified, a more
/// efficient implementation may be used that ensures delegations become zero-cost.
///
/// #### `#[jsony(rename_all = "...")]`
///
/// The possible values are "lowercase", "UPPERCASE", "PascalCase", "camelCase", "snake_case", "SCREAMING_SNAKE_CASE", "kebab-case", "SCREAMING-KEBAB-CASE".
///
/// #### Binary Versioning (`#[jsony(version = ...)]`)
///
/// The `version` attribute on containers (`struct`/`enum`) enables versioning for
/// `ToBinary` and `FromBinary`.
///
/// - **ToBinary:** The *current version* is written as a prefix and is either specified
///    explicitly via the container attribute or inferred via the largest field version.
/// - **FromBinary:**
///   - By default (`version = N`), all versions below the current version will attempted
///     to be decoded, using the attribute default or `Default::default()` for absent
///     fields of older versions.
///   - When using a range a minimum version that will reject small versions.
/// - **Field:** Use `#[jsony(version = N)]` on fields added in a specific
///
/// This system allows newer code to read older data by providing defaults for new fields,
/// and helps older code detect and reject data from newer, unknown versions. Breaking
/// changes can be managed by incrementing the minimum required version (e.g., changing
/// `version = 1..` to `version = 2..`).
///
/// ```ignore
/// # use jsony::{Jsony, FromBinary, ToBinary, from_binary, to_binary};
/// #[derive(Jsony, PartialEq, Debug)]
/// #[jsony(Binary, version)] // Current version 0
/// struct RecordV0 { value: u32 }
///
/// #[derive(Jsony, PartialEq, Debug)]
/// #[jsony(Binary, version)] // Current version 1
/// struct RecordV1<'a> {
///     value: u32,
///     #[jsony(version = 1, default = "N/A")] // Added in V1
///     name: &'a str,
/// }
///
/// let bin_v0 = to_binary(&RecordV0 { value: 10 });
/// assert_eq!(
///     from_binary::<RecordV1>(&bin_v0).unwrap(),
///     RecordV1 { value: 10, name: "N/A"}
/// );
/// ```
///
/// #### `#[jsony(zerocopy)]`
///
/// Enables zero-copy optimizations for `FromBinary` and `ToBinary` implementations.
/// This allows borrowing the data directly from the input byte slice, improving performance.
///
/// **Requirements:**
///
/// * The struct must be `#[repr(C)]` or `#[repr(transparent)]`.
/// * The struct must be Plain Old Data (POD) – having a defined layout with no
///     padding, suitable for direct memory interpretation.
///
/// **Safety:**
///
/// Using this attribute is safe. The necessary POD constraints are checked at compile time
/// via `const` assertions. If the requirements are not met, a compile-time error
/// occurs. This avoids the need for `unsafe` code often associated with manual
/// `FromBinary::POD = true` implementations.
///
/// **Alignment and Slices:**
///
/// Deserializing slices like `&[Self]` requires the input data to have the correct
/// alignment for `Self`. If alignment cannot be guaranteed, using `Cow<'_, [Self]>`
/// is recommended. `Cow` will borrow if possible, or create an owned copy if the
/// input alignment is insufficient.
///
/// **Limitations:**
///
/// * **Big Endian:** As of jsony v0.1, zero-copy support on Big Endian systems
///     is limited to types of `size_of::<T>() == 1`. Future versions may expand this.
///
/// #### `#[jsony(skip_if = ...)]`
///
/// Omit the field if the provided predicate function return true. The predicate function can be specified
/// by a path or inline using closure syntax. The predicate function will provided the current field value
/// via reference.
///
/// ```ignore
/// #[derive(Jsony)]
/// #[jsony(ToString)]
/// struct Example {
///     #[jsony(skip_if = str::is_empty)]
///     value: String,
///     #[jsony(skip_if = |s| s == u32::MAX)]
///     sentinel: u32
/// }
/// ```
///
///
/// #### Default enum representation for JSON
///
/// <table width="100%">
/// <tr><td width="47%">Enum</td><td>JSON for Example::Alpha</td><td>JSON for Example::Beta</td></tr><tr><td>
///
/// ```ignore
/// #[derive(Jsony)]
/// enum Example {
///     Alpha {
///         field: bool
///     },
///     Beta
/// }
/// ```
///
/// </td><td>
///
/// ```json
/// {
///     "Alpha": {
///         "field": true
///     }
/// }
/// ```
///
/// </td><td>
///
/// ```json
/// "Beta"
/// ```
///
/// </td></tr></table>
///
/// #### Enum with `#[jsony(tag = "...")]`
///
/// May not be used with `untagged`.
///
/// <table width="100%">
/// <tr><td width="47%">Enum</td><td>JSON for Example::Alpha</td><td>JSON for Example::Beta</td></tr><tr><td>
///
/// ```ignore
/// #[derive(Jsony)]
/// #[jsony(tag = "kind")]
/// enum Example {
///     Alpha {
///         field: bool
///     },
///     Beta
/// }
/// ```
///
/// </td><td>
///
/// ```json
/// {
///     "kind": "Alpha",
///     "field": true
/// }
/// ```
///
/// </td><td>
///
/// ```json
/// {
///     "kind": "Beta"
/// }
/// ```
///
/// </td></tr></table>
///
/// #### Enum with `#[jsony(tag = "...", content = "...")]`
///
/// May not be used with `untagged`. Note that the content attribute `content` must be
/// used with `tag`.
///
/// <table width="100%">
/// <tr><td width="47%">Enum</td><td>JSON for Example::Alpha</td><td>JSON for Example::Beta</td></tr><tr><td>
///
/// ```ignore
/// #[derive(Jsony)]
/// #[jsony(tag = "kind", content = "data")]
/// enum Example {
///     Alpha {
///         field: bool
///     },
///     Beta
/// }
/// ```
///
/// </td><td>
///
/// ```json
/// {
///     "tag": "Alpha",
///     "data": {
///         "field": true
///     }
/// }
/// ```
///
/// </td><td>
///
/// ```json
/// {
///     "tag": "Beta"
/// }
/// ```
///
/// </td></tr></table>
///
/// #### Enum with `#[jsony(untagged)]`
///
/// <table width="100%">
/// <tr><td width="47%">Enum</td><td>JSON for Example::Alpha</td><td>JSON for Example::Beta</td></tr><tr><td>
///
/// ```ignore
/// #[derive(Jsony)]
/// #[jsony(untagged)]
/// enum Example {
///     Variant {
///         field: bool
///     }
/// }
/// ```
///
/// </td><td>
///
/// ```json
/// {
///     "field": true
/// }
/// ```
///
/// </td><td>
///
/// Not Supported
///
/// </td></tr></table>
///
/// ## Enum Conversion Helpers
///
/// The Jsony derive can also automatically implement common conversions for
/// enums. Not only is the same proc-macro expansion innovcation and parsing
/// of the derive target shared, but the implementation can be as well. For
/// instance an Enum with `#[jsony(FromJson, FromStr)]` may have the FromJson
/// impl internally call FromStr.
///
/// Example:
///
/// ```ignore
/// #[derive(Jsony, PartialEq, Debug)]
/// #[jsony(ToStr, FromStr, rename_all = "kebab-case")]
/// enum Mode {
///     Slow,
///     Fast,
///     TurboMax,
/// }
///
/// assert_eq!(Mode::TurboMax.to_str(), "turbo-max");
/// assert_eq!("slow".parse::<Mode>(), Ok(Mode::Slow));
/// assert!("zzz".parse::<Mode>().is_err());
/// ```
///
/// - `FromStr` will implement [std::str::FromStr] (often invoked via `str::parse` as seen in the example).
/// - `ToStr` will implement a inherent method on the enum with the following
/// signature:
///
/// ```ignore
/// pub fn to_str(&self) -> &'static str;
/// ```
/// Rename attributes on variants will be respected for these conversion.
///
/// ## Why a single unified derive macro?
///
/// By convention, derive macros are typically named after the trait they implement.
/// Jsony breaks this convention to reduce compilation time as the unified approach:
///
/// 1. Avoids the overhead of multiple derive macro invocations.
/// 2. Needs to parse the input only once.
/// 3. Allows traits to share code, as the macro knows the full set being implemented.
///
/// These optimizations are particularly useful for Jsony because:
/// - The derive macros are already extremely efficient, such that invocation overhead is measurable.
/// - Jsony's approach of having specialized traits for different formats means users often want to implement
///   many traits.

#[proc_macro_derive(Jsony, attributes(jsony))]
pub fn derive_jsony(input: TokenStream) -> TokenStream {
    codegen::derive(input)
}
