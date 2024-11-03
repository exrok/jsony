#![allow(warnings)]
extern crate proc_macro;
mod ast;
mod case;
mod codegen;
mod lit;
mod template;
mod util;
mod writer;

use proc_macro::{Delimiter, Group, Ident, Literal, Punct, Spacing, Span, TokenStream, TokenTree};

pub(crate) fn default_call_tokens(span: Span) -> Vec<TokenTree> {
    vec![
        TokenTree::Ident(Ident::new("Default", span)),
        TokenTree::Punct(Punct::new(':', Spacing::Joint)),
        TokenTree::Punct(Punct::new(':', Spacing::Alone)),
        TokenTree::Ident(Ident::new("default", span)),
        TokenTree::Group(Group::new(Delimiter::Parenthesis, TokenStream::new())),
    ]
}

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
    pub(crate) fn to_compiler_error(&self) -> TokenStream {
        let mut group = TokenTree::Group(Group::new(
            Delimiter::Parenthesis,
            TokenStream::from_iter([TokenTree::Literal(Literal::string(&self.0.message))]),
        ));
        let mut punc = TokenTree::Punct(Punct::new('!', Spacing::Alone));
        punc.set_span(self.0.span);
        group.set_span(self.0.span);

        TokenStream::from_iter([
            TokenTree::Ident(Ident::new("compile_error", self.0.span)),
            punc,
            group,
            TokenTree::Punct(Punct::new(';', Spacing::Alone)),
        ])
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

#[proc_macro]
pub fn object(input: TokenStream) -> TokenStream {
    template::object(input)
}

/// Unified derive macro for implementing various conversion traits
///
/// Configured with attributes in the form `#[jsony(...)]` on containers,
/// variants, and fields.
///
/// The traits to derive are specified via container attributes. The currently
/// supported traits are:
/// - `ToJson`
/// - `FromJson`
/// - `ToBinary`
/// - `FromBinary`
///
/// The rest of the attributes are described in the following tables. Note that some
/// attributes apply only to certain traits.
///
/// ## Container Attributes
/// These are `jsony` attributes that appear above a `struct` or `enum`.
///
/// | Format | Supported Traits | Description |
/// |--------|------------------|-------------|
/// | `transparent` | All | Traits delegate to the single inner type.
/// | `rename_all = "..."` | All | Renames variants and fields not explicitly renamed.
/// | `tag = "..."` | `Json` | Field containing the enum variant.
/// | `content = "..."` | `Json` | Field containing the data content of enum.
/// | `untagged` | `Json` | Only data content of an enum is stored.
/// | `Flattenable` | `FromJson` | Allows type to use `#[jsony(flatten)]` in `FromJson`.
///
/// ## Field Attributes
/// These are `jsony` attributes that appear above a field inside a struct or enum variant.
///
/// | Format | Supported Traits | Description |
/// |--------|------------------|-------------|
/// | `default [= ...]` | `From` | Use `Default::default()` or provided expression if field is missing.
/// | `flatten` | `Json` | Flatten the contents of the field into the container it is defined in.
/// | `rename = "..."` | `Json` | Use provided string as field name.
/// | `via = ...` | All | Implement conversion through provided trait.
/// | `with = ...` | All | Use methods from specified module instead of trait. [read more](#jsonywith---on-fields)
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
/// The function corresponds to the save methods of each trait (ommiting the `__jsony` suffix if present).
///
/// | Trait | Function |
/// |-------|-----------------|
/// | `ToJson` | `fn json_encode(value: &bool, output: &mut TextWriter)`
/// | `FromJson` | `fn json_decode<'a>(parser: &mut jsony::parser::Parser<'a>) -> Result<bool, &'static DecodeError>`
/// | `ToBinary` | `fn binary_encode(value: &bool, output: &mut BytesWriter)`
/// | `FromBinary` | `fn binary_decode(decoder: &mut jsony::binary::Decoder<'_>) -> bool`
///
/// ##### Example of `with` attribute
/// ```ignore
/// mod bool_as_int {
///     use jsony::{
///         json::DecodeError, BytesWriter, FromBinary, FromJson, TextWriter, ToBinary, ToJson,
///     };
///     pub fn json_encode(value: &bool, output: &mut TextWriter) {
///         (*value as u32).json_encode__jsony(output);
///     }
///     pub fn json_decode(parser: &mut jsony::parser::Parser<'_>) -> Result<bool, &'static DecodeError> {
///         Ok(<u32>::json_decode(parser)? != 0)
///     }
///     pub fn binary_encode(value: &bool, output: &mut BytesWriter) {
///         (*value as u32).binary_encode(output)
///     }
///     pub fn binary_decode(decoder: &mut jsony::binary::Decoder<'_>) -> bool {
///         u32::binary_decode(decoder) != 0
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
/// #### Default enum representation for JSON
///
/// <table width="100%">
/// <tr><td width="47%">Enum</td><td>JSON for Example::Alpha</td><td>JSON for Example::Beta</td></tr><tr><td>
///
/// ```rust
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
/// ```rust
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
/// ```rust
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
/// ```rust
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
