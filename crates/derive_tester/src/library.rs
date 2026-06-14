use std::borrow::Cow;
use std::sync::OnceLock;

use jsony::Jsony;
use proc_macro2::{Delimiter, Group, Ident, Literal, TokenStream, TokenTree};

use crate::features::{FieldAnnotator, FieldFeatureDecl};
use crate::schema::{
    Enum, ItemKind, Struct, Type, FLAGS, IMPL_FROM_BINARY, IMPL_FROM_JSON, IMPL_FROM_TOML,
    IMPL_TO_BINARY, IMPL_TO_JSON, IMPL_TO_TOML,
};

pub type Capability = u32;
pub const CAP_TO_JSON: Capability = 1 << 0;
pub const CAP_FROM_JSON: Capability = 1 << 1;
pub const CAP_JSON: Capability = CAP_TO_JSON | CAP_FROM_JSON;
pub const CAP_TO_BINARY: Capability = 1 << 2;
pub const CAP_FROM_BINARY: Capability = 1 << 3;
pub const CAP_BINARY: Capability = CAP_TO_BINARY | CAP_FROM_BINARY;
pub const CAP_TO_TOML: Capability = 1 << 4;
pub const CAP_FROM_TOML: Capability = 1 << 5;
pub const CAP_TOML: Capability = CAP_TO_TOML | CAP_FROM_TOML;
pub const CAP_ENUM_VARIANTS: Capability = 1 << 6;

pub fn caps_to_impl_flags(caps: Capability) -> FLAGS {
    let mut flags: FLAGS = 0;
    if caps & CAP_TO_JSON != 0 {
        flags |= IMPL_TO_JSON;
    }
    if caps & CAP_FROM_JSON != 0 {
        flags |= IMPL_FROM_JSON;
    }
    if caps & CAP_TO_BINARY != 0 {
        flags |= IMPL_TO_BINARY;
    }
    if caps & CAP_FROM_BINARY != 0 {
        flags |= IMPL_FROM_BINARY;
    }
    if caps & CAP_TO_TOML != 0 {
        flags |= IMPL_TO_TOML;
    }
    if caps & CAP_FROM_TOML != 0 {
        flags |= IMPL_FROM_TOML;
    }
    flags
}

fn toml_spanner_path() -> Option<&'static str> {
    static PATH: OnceLock<Option<String>> = OnceLock::new();
    PATH.get_or_init(|| std::env::var("TOML_SPANNER_PATH").ok())
        .as_deref()
}

#[derive(Clone, Debug, Jsony)]
#[jsony(Json)]
pub enum Library {
    Jsony { path: Option<String> },
    Merde,
    Sonic,
    Nanoserde,
    Musli,
    Serde,
    Miniserde,
    Midiserde,
    Baseline,
    Facet,
    Toml,
    TomlSpanner,
    TomlSpannerMacros,
    TomlEdit,
    TomlSpan,
}

impl Library {
    pub fn from_name(name: &str) -> Library {
        match name {
            "jsony" => Library::Jsony { path: None },
            "jsony(local)" => Library::Jsony { path: None },
            "merde" => Library::Merde,
            "sonic-rs" => Library::Sonic,
            "nanoserde" => Library::Nanoserde,
            "musli" => Library::Musli,
            "serde" => Library::Serde,
            "miniserde" => Library::Miniserde,
            "midiserde" => Library::Midiserde,
            "baseline" => Library::Baseline,
            "facet" => Library::Facet,
            "toml" => Library::Toml,
            "toml-spanner" => Library::TomlSpanner,
            "toml_edit" | "toml-edit" => Library::TomlEdit,
            "toml-span" => Library::TomlSpan,
            _ => Library::Baseline,
        }
    }

    pub fn default() -> Vec<Library> {
        vec![
            Library::Jsony { path: None },
            Library::Miniserde,
            Library::Nanoserde,
            Library::Serde,
        ]
    }

    pub fn all() -> Vec<Library> {
        vec![
            Library::Jsony { path: None },
            Library::Miniserde,
            Library::Nanoserde,
            Library::Serde,
            Library::Merde,
            Library::Musli,
        ]
    }

    pub fn name(&self) -> &'static str {
        match self {
            Library::Jsony { path: Some(..) } => "jsony(local)",
            Library::Jsony { .. } => "jsony",
            Library::Merde => "merde",
            Library::Facet => "facet",
            Library::Sonic => "sonic-rs",
            Library::Nanoserde => "nanoserde",
            Library::Musli => "musli",
            Library::Serde => "serde",
            Library::Miniserde => "miniserde",
            Library::Midiserde => "midiserde",
            Library::Baseline => "baseline",
            Library::Toml => "toml",
            Library::TomlSpanner => "toml-spanner",
            Library::TomlSpannerMacros => "toml-spanner",
            Library::TomlEdit => "toml_edit",
            Library::TomlSpan => "toml-span",
        }
    }

    /// The primary crate name as it appears in Cargo.lock, used for version lookup.
    pub fn version_crate_name(&self) -> &'static str {
        match self {
            Library::Jsony { .. } => "jsony",
            Library::Merde => "merde",
            Library::Facet => "facet",
            Library::Sonic => "sonic-rs",
            Library::Nanoserde => "nanoserde",
            Library::Musli => "musli",
            Library::Serde => "serde",
            Library::Miniserde => "miniserde",
            Library::Midiserde => "midiserde",
            Library::Baseline => "baseline",
            Library::Toml => "toml",
            Library::TomlSpanner => "toml-spanner",
            Library::TomlSpannerMacros => "toml-spanner",
            Library::TomlEdit => "toml_edit",
            Library::TomlSpan => "toml-span",
        }
    }

    pub fn crate_prefix(&self) -> &'static str {
        match self {
            Library::Jsony { path: Some(_) } => "jsony_local",
            Library::Jsony { .. } => "jsony",
            Library::Merde => "merde",
            Library::Facet => "facet",
            Library::Sonic => "sonic",
            Library::Nanoserde => "nanoserde",
            Library::Musli => "musli",
            Library::Serde => "serde",
            Library::Miniserde => "miniserde",
            Library::Midiserde => "midiserde",
            Library::Baseline => "baseline",
            Library::Toml => "toml",
            Library::TomlSpanner => "toml_spanner",
            Library::TomlSpannerMacros => "toml_spanner_macros",
            Library::TomlEdit => "toml_edit",
            Library::TomlSpan => "toml_span",
        }
    }

    pub fn capabilities(&self) -> Capability {
        match self {
            Library::Jsony { .. } | Library::Nanoserde | Library::Serde | Library::Musli => {
                CAP_JSON | CAP_BINARY | CAP_ENUM_VARIANTS
            }
            Library::Sonic => CAP_JSON | CAP_ENUM_VARIANTS,
            Library::Facet => CAP_JSON | CAP_TOML | CAP_ENUM_VARIANTS,
            Library::Miniserde | Library::Midiserde | Library::Merde => CAP_JSON,
            Library::Baseline => 0,
            Library::Toml => CAP_TOML | CAP_ENUM_VARIANTS,
            Library::TomlSpanner => CAP_TOML,
            Library::TomlSpannerMacros => CAP_TOML,
            Library::TomlEdit => CAP_TOML | CAP_ENUM_VARIANTS,
            Library::TomlSpan => CAP_FROM_TOML,
        }
    }

    pub fn is_derive_toml(&self) -> bool {
        matches!(
            self,
            Library::Toml | Library::TomlSpannerMacros | Library::TomlEdit | Library::Facet
        )
    }

    pub fn all_sets() -> Vec<Library> {
        let mut sets = Library::all();
        sets.push(Library::Sonic);
        sets.push(Library::Midiserde);
        sets.push(Library::Facet);
        sets.push(Library::Baseline);
        sets.push(Library::Toml);
        sets.push(Library::TomlSpanner);
        sets.push(Library::TomlSpannerMacros);
        sets.push(Library::TomlEdit);
        sets.push(Library::TomlSpan);
        sets
    }

    pub fn dependencies(&self) -> Cow<'static, str> {
        Cow::Borrowed(match self {
            Library::Jsony { path: Some(path) } => {
                return Cow::Owned(format!(
                    r#"
                jsony = {{path = "{path}/jsony", default-features = false}}
                jsony_macros = {{path = "{path}/jsony_macros", default-features = false}}

                [profile.dev.package."jsony"]
                incremental = false
                [profile.dev.package."jsony_macros"]
                incremental = false
                "#
                ))
            }
            Library::Baseline => "",
            Library::Facet => {
                r#"
facet = "0.44"
facet-json = "0.44"
            "#
            }
            Library::Jsony { .. } => {
                r#"
                jsony = {version = "=0.1.9", default-features = false}
                jsony_macros = {version = "=0.1.8", default-features = false}
            "#
            }
            Library::Merde => {
                r#"merde = { version = "10", features = ["core", "json", "deserialize"] }"#
            }
            Library::Nanoserde => "nanoserde = \"0.2\"",
            Library::Musli => "musli = { version = \"0.0.149\", features = [\"json\"]}",
            Library::Serde => {
                r#"
            serde_derive = "1"
            serde = "1"
            serde_json = "1"
            "#
            }
            Library::Sonic => "sonic-rs = \"0.3.14\"\nserde = \"1\"",
            Library::Miniserde => "miniserde = \"0.1\"",
            Library::Midiserde => "midiserde = \"0.1\"\nminiserde = \"0.1\"",
            Library::Toml => {
                r#"
            serde_derive = "1"
            serde = "1"
            toml = "1"
            toml_parser = { version = "1", features = ["unsafe"] }
            "#
            }
            Library::TomlSpanner => {
                if let Some(path) = toml_spanner_path() {
                    return Cow::Owned(format!(
                        r#"toml-spanner = {{ path = "{path}", default-features = false, features = ["from-toml", "to-toml"] }}
[profile.dev.package."toml-spanner"]
incremental = false
"#
                    ));
                }
                r#"toml-spanner = { version = "1", default-features = false, features = ["from-toml", "to-toml"] }
"#
            }
            Library::TomlSpannerMacros => {
                if let Some(path) = toml_spanner_path() {
                    return Cow::Owned(format!(
                        r#"toml-spanner = {{ path = "{path}", default-features = false, features = ["from-toml", "to-toml"] }}
toml-spanner-macros = {{ path = "{path}/crates/toml-spanner-macros" }}
[profile.dev.package."toml-spanner"]
incremental = false
[profile.dev.package."toml-spanner-macros"]
incremental = false
"#
                    ));
                }
                r#"toml-spanner = { version = "1", default-features = false, features = ["from-toml", "to-toml"] }
toml-spanner-macros = "1"
"#
            }
            Library::TomlEdit => {
                r#"
            serde_derive = "1"
            serde = "1"
            toml_edit = { version = "0.25", features = ["serde"] }
            toml_parser = { version = "1", features = ["unsafe"] }
            "#
            }
            Library::TomlSpan => r#"toml-span = "0.7""#,
        })
    }

    pub fn dependencies_for(&self, caps: Capability) -> Cow<'static, str> {
        if caps & CAP_BINARY != 0 {
            self.dependencies_multi()
        } else if caps & CAP_TOML != 0 {
            if caps & CAP_TO_TOML != 0 {
                self.dependencies_toml()
            } else {
                self.dependencies_toml_deser()
            }
        } else {
            self.dependencies()
        }
    }

    fn dependencies_toml(&self) -> Cow<'static, str> {
        match self {
            Library::Facet => Cow::Borrowed(
                r#"
facet = "0.44"
facet-toml = { version = "0.44.1", features = ["serialize", "fast"] }
            "#,
            ),
            _ => self.dependencies(),
        }
    }

    fn dependencies_toml_deser(&self) -> Cow<'static, str> {
        Cow::Borrowed(match self {
            Library::Toml => {
                r#"
            serde_derive = "1"
            serde = "1"
            toml = { version = "1", default-features = false, features = ["parse" , "serde"] }
            toml_parser = { version = "1", default-features = false, features = ["unsafe"] }
            "#
            }
            Library::TomlSpanner => {
                if let Some(path) = toml_spanner_path() {
                    return Cow::Owned(format!(
                        r#"toml-spanner = {{ path = "{path}", default-features = false, features = ["from-toml"] }}
[profile.dev.package."toml-spanner"]
incremental = false"#
                    ));
                }
                r#"toml-spanner = { version = "1", default-features = false, features = ["from-toml"] }"#
            }
            Library::TomlSpannerMacros => {
                if let Some(path) = toml_spanner_path() {
                    return Cow::Owned(format!(
                        r#"toml-spanner = {{ path = "{path}", default-features = false, features = ["from-toml"] }}
toml-spanner-macros = {{ path = "{path}/crates/toml-spanner-macros" }}
[profile.dev.package."toml-spanner"]
incremental = false"#
                    ));
                }
                r#"toml-spanner = { version = "1", default-features = false, features = ["from-toml"] }
toml-spanner-macros = "1""#
            }
            Library::TomlEdit => {
                r#"
            serde_derive = "1"
            serde = "1"
            toml_edit = { version = "0.25", default-features = false, features = ["serde", "parse"] }
            toml_parser = { version = "1", default-features = false, features = ["unsafe"] }
            "#
            }
            Library::TomlSpan => r#"toml-span = "0.7""#,
            Library::Facet => {
                r#"
facet = "0.44"
facet-toml = { version = "0.44.1", features = ["fast"] }
            "#
            }
            _ => return self.dependencies(),
        })
    }

    fn dependencies_multi(&self) -> Cow<'static, str> {
        match self {
            Library::Serde => Cow::Borrowed(
                r#"
            serde_derive = "1"
            serde = "1"
            serde_json = "1"
            bincode = "1"
            "#,
            ),
            Library::Musli => {
                Cow::Borrowed(r#"musli = { version = "0.0.149", features = ["json", "storage"]}"#)
            }
            _ => self.dependencies(), // includes Toml
        }
    }

    pub fn gen_to_json_string(&self, out: &mut Vec<TokenTree>, func: &dyn Fn(&mut Vec<TokenTree>)) {
        match self {
            Library::Jsony { .. } => splat!(out; jsony::to_json([func(out)])),
            Library::Nanoserde => splat!(out; ([func(out)]).serialize_json()),
            Library::Serde => splat!(out; serde_json::to_string([func(out)]).unwrap()),
            Library::Sonic => splat!(out; sonic_rs::to_string([func(out)]).unwrap()),
            Library::Miniserde => splat!(out; miniserde::json::to_string([func(out)])),
            Library::Midiserde => splat!(out; midiserde::json::to_string([func(out)])),
            Library::Merde => splat!(out; merde::json::to_string([func(out)]).unwrap()),
            Library::Musli => splat!(out; musli::json::to_string([func(out)]).unwrap()),
            Library::Baseline => splat!(out; {let _ = [func(out)]; String::new()}),
            Library::Facet => splat!(out; facet_json::to_string([func(out)]).unwrap()),
            Library::Toml
            | Library::TomlSpanner
            | Library::TomlSpannerMacros
            | Library::TomlEdit
            | Library::TomlSpan => {
                panic!("gen_to_json_string not supported for {}", self.name())
            }
        }
    }

    pub fn gen_from_json_str(
        &self,
        out: &mut Vec<TokenTree>,
        ty: &Ident,
        func: &dyn Fn(&mut Vec<TokenTree>),
    ) {
        match self {
            Library::Jsony { .. } => splat!(out; jsony::from_json::<[#ty]>([func(out)])),
            Library::Nanoserde => splat!(out; [#ty]::deserialize_json([func(out)])),
            Library::Serde => splat!(out; serde_json::from_str::<[#ty]>([func(out)])),
            Library::Sonic => splat!(out; sonic_rs::from_str::<[#ty]>([func(out)])),
            Library::Miniserde => {
                splat!(out; miniserde::json::from_str::<[#ty]>([func(out)]))
            }
            Library::Midiserde => {
                splat!(out; midiserde::json::from_str::<[#ty]>([func(out)]))
            }
            Library::Merde => splat!(out; merde::json::from_str::<[#ty]>([func(out)])),
            Library::Musli => splat!(out; musli::json::from_str::<[#ty]>([func(out)])),
            Library::Baseline => splat!(out; Err::<[#ty], &str>([func(out)])),
            Library::Facet => splat!(out; facet_json::from_str::<[#ty]>([func(out)])),
            Library::Toml
            | Library::TomlSpanner
            | Library::TomlSpannerMacros
            | Library::TomlEdit
            | Library::TomlSpan => {
                panic!("gen_from_json_str not supported for {}", self.name())
            }
        }
    }

    pub fn gen_prelude(&self, out: &mut Vec<TokenTree>) {
        splat!(out; #![[allow(unused_imports, dead_code)]]);
        self.gen_imports(out);
    }

    pub fn gen_module_prelude_for(&self, out: &mut Vec<TokenTree>, caps: Capability) {
        if caps & CAP_BINARY != 0 {
            self.gen_imports_multi(out);
        } else if caps & CAP_TOML != 0 && caps & CAP_TO_TOML == 0 {
            self.gen_imports_toml_deser(out);
        } else {
            self.gen_imports(out);
        }
    }

    fn gen_imports_toml_deser(&self, out: &mut Vec<TokenTree>) {
        match self {
            Library::Toml | Library::TomlEdit => {
                splat!(out; use serde_derive::Deserialize;)
            }
            Library::Baseline | Library::TomlSpanner | Library::TomlSpan => (),
            Library::TomlSpannerMacros => splat!(out; use toml_spanner_macros::Toml;),
            Library::Facet => splat!(out; use facet::Facet;),
            other => panic!("gen_imports_toml_deser not supported for {}", other.name()),
        }
    }

    fn gen_imports_multi(&self, out: &mut Vec<TokenTree>) {
        match self {
            Library::Jsony { .. } => splat!(out; use jsony_macros::Jsony;),
            Library::Nanoserde => splat!(out; use nanoserde::{DeBin, SerBin, DeJson, SerJson};),
            Library::Serde => splat!(out; use serde_derive::{Deserialize, Serialize};),
            Library::Musli => splat!(out; use musli::{Encode, Decode};),
            Library::Baseline => (),
            other => panic!("gen_imports_multi not supported for {}", other.name()),
        }
    }

    fn gen_imports(&self, out: &mut Vec<TokenTree>) {
        match self {
            Library::Jsony { .. } => splat!(out; use jsony_macros::Jsony;),
            Library::Merde => splat!(out; ),
            Library::Nanoserde => splat!(out; use nanoserde::{DeJson, SerJson};),
            Library::Musli => splat!(out; use musli::{Encode, Decode};),
            Library::Serde | Library::Toml | Library::TomlEdit => {
                splat!(out; use serde_derive::{Deserialize, Serialize};)
            }
            Library::Sonic => splat!(out; use sonic_rs::{Deserialize, Serialize};),
            Library::Miniserde => splat!(out; use miniserde::{Serialize, Deserialize};),
            Library::Midiserde => splat!(out; use midiserde::{Serialize, Deserialize};),
            Library::Facet => splat!(out; use facet::Facet;),
            Library::Baseline | Library::TomlSpanner | Library::TomlSpan => (),
            Library::TomlSpannerMacros => splat!(out; use toml_spanner_macros::Toml;),
        }
    }

    pub fn gen_compat_json(&self, out: &mut Vec<TokenTree>) {
        match self {
            Library::Jsony { .. } => splat! { out;
                pub fn to_json(value: &impl jsony::ToJson) -> String { jsony::to_json(value) }
                pub fn from_json<T: for<~a> jsony::FromJson<~a> >(s: &str) -> T { jsony::from_json::<T>(s).unwrap() }
            },
            Library::Serde => splat! { out;
                pub fn to_json(value: &impl serde::Serialize) -> String { serde_json::to_string(value).unwrap() }
                pub fn from_json<~a, T: serde::de::DeserializeOwned>(s: &str) -> T { serde_json::from_str::<T>(s).unwrap() }
            },
            Library::Sonic => splat! { out;
                pub fn to_json(value: &impl serde::Serialize) -> String { sonic_rs::to_string(value).unwrap() }
                pub fn from_json<T: serde::de::DeserializeOwned>(s: &str) -> T { sonic_rs::from_str::<T>(s).unwrap() }
            },
            Library::Nanoserde => splat! { out;
                pub fn to_json(value: &impl nanoserde::SerJson) -> String { value.serialize_json() }
                pub fn from_json<T: nanoserde::DeJson>(s: &str) -> T { T::deserialize_json(s).unwrap() }
            },
            Library::Musli => splat! { out;
                pub fn to_json<T: musli::en::Encode<musli::mode::Text> >(value: &T) -> String { musli::json::to_string(value).unwrap() }
                pub fn from_json<T: for<~a> musli::de::Decode<~a, musli::mode::Text, musli::alloc::Global> >(s: &str) -> T { musli::json::from_str::<T>(s).unwrap() }
            },
            Library::Miniserde => splat! { out;
                pub fn to_json(value: &impl miniserde::Serialize) -> String { miniserde::json::to_string(value) }
                pub fn from_json<T: miniserde::Deserialize>(s: &str) -> T { miniserde::json::from_str::<T>(s).unwrap() }
            },
            Library::Midiserde => splat! { out;
                pub fn to_json(value: &impl midiserde::Serialize) -> String { midiserde::json::to_string(value) }
                pub fn from_json<T: midiserde::Deserialize>(s: &str) -> T { midiserde::json::from_str::<T>(s).unwrap() }
            },
            Library::Baseline => splat! { out;
                pub fn to_json<T>(_value: &T) -> String { String::new() }
                pub fn from_json<T>(_s: &str) -> T { unimplemented!("baseline does not support deserialization") }
            },
            Library::Facet => splat! { out;
                pub fn to_json<~a>(value: &(impl facet::Facet<~a> + ~a)) -> String { facet_json::to_string(value).unwrap() }
                pub fn from_json<T: for<~a> facet::Facet<~a> >(s: &str) -> T { facet_json::from_str::<T>(s).unwrap() }
            },
            Library::Merde
            | Library::Toml
            | Library::TomlSpanner
            | Library::TomlSpannerMacros
            | Library::TomlEdit
            | Library::TomlSpan => {
                panic!("gen_compat_json not supported for {:?}", self.name())
            }
        }
    }

    pub fn gen_compat_binary(&self, out: &mut Vec<TokenTree>) {
        match self {
            Library::Jsony { .. } => splat! { out;
                pub fn to_binary(value: &impl jsony::ToBinary) -> Vec<u8> { jsony::to_binary(value) }
                pub fn from_binary<T: for<~a> jsony::FromBinary<~a> >(bytes: &[[u8]]) -> T { jsony::from_binary::<T>(bytes).unwrap() }
            },
            Library::Nanoserde => splat! { out;
                pub fn to_binary(value: &impl nanoserde::SerBin) -> Vec<u8> { nanoserde::SerBin::serialize_bin(value) }
                pub fn from_binary<T: nanoserde::DeBin>(bytes: &[[u8]]) -> T { T::deserialize_bin(bytes).unwrap() }
            },
            Library::Serde => splat! { out;
                pub fn to_binary(value: &impl serde::Serialize) -> Vec<u8> { bincode::serialize(value).unwrap() }
                pub fn from_binary<T: serde::de::DeserializeOwned>(bytes: &[[u8]]) -> T { bincode::deserialize(bytes).unwrap() }
            },
            Library::Musli => splat! { out;
                pub fn to_binary<T: musli::en::Encode<musli::mode::Binary> >(value: &T) -> Vec<u8> { musli::storage::to_vec(value).unwrap() }
                pub fn from_binary<T: for<~a> musli::de::Decode<~a, musli::mode::Binary, musli::alloc::Global> >(bytes: &[[u8]]) -> T { musli::storage::from_slice(bytes).unwrap() }
            },
            Library::Baseline => splat! { out;
                pub fn to_binary<T>(_value: &T) -> Vec<u8> { Vec::new() }
                pub fn from_binary<T>(_bytes: &[[u8]]) -> T { unimplemented!("baseline does not support deserialization") }
            },
            other => panic!("gen_compat_binary not supported for {}", other.name()), // includes Toml
        }
    }

    pub fn compat_module_bytes_for(&self, caps: Capability) -> Vec<u8> {
        let mut out = Vec::new();
        if caps & CAP_JSON != 0 {
            self.gen_compat_json(&mut out);
        }
        if caps & CAP_BINARY != 0 {
            self.gen_compat_binary(&mut out);
        }
        crate::token::to_rust(out.into_iter().collect())
    }

    pub fn gen_toml_deser_main(&self, out: &mut Vec<TokenTree>) {
        splat! { out;
            #![[allow(unused_imports, dead_code)]]
            mod models;
            fn main() {
                let mut args = std::env::args();
                args.next();
                let repeat: u64 = args.next().unwrap_or("1".into()).parse().unwrap();
                let mut input = String::new();
                use std::io::Read;
                std::io::stdin().read_to_string(&mut input).unwrap();
                let mut last_len: usize = [#Literal::usize_unsuffixed(0)];
                for _ in [#Literal::u64_unsuffixed(0)]..repeat {
                    std::hint::black_box(&mut input);
                    [match self {
                        Library::Toml => splat!(out;
                            let parsed: models::CargoToml = toml::from_str(&input).unwrap();
                            last_len = std::mem::size_of_val(&parsed);
                        ),
                        Library::TomlEdit => splat!(out;
                            let parsed: models::CargoToml = toml_edit::de::from_str(&input).unwrap();
                            last_len = std::mem::size_of_val(&parsed);
                        ),
                        Library::TomlSpanner | Library::TomlSpannerMacros => splat!(out;
                            let parsed: models::CargoToml = toml_spanner::from_str(&input).unwrap();
                            last_len = std::mem::size_of_val(&parsed);
                        ),
                        Library::TomlSpan => splat!(out;
                            let mut value = toml_span::parse(&input).unwrap();
                            let parsed: models::CargoToml = toml_span::Deserialize::deserialize(&mut value).unwrap();
                            last_len = std::mem::size_of_val(&parsed);
                        ),
                        Library::Facet => splat!(out;
                            let parsed: models::CargoToml = facet_toml::from_str(&input).unwrap();
                            last_len = std::mem::size_of_val(&parsed);
                        ),
                        Library::Baseline => splat!(out;
                            last_len = input.len();
                        ),
                        other => panic!("gen_toml_deser_main not supported for {}", other.name()),
                    }]
                    std::hint::black_box(last_len);
                }
                print!("{}", last_len);
            }
        };
    }

    pub fn gen_toml_main(&self, out: &mut Vec<TokenTree>) {
        splat! { out;
            #![[allow(unused_imports, dead_code)]]
            mod models;
            fn main() {
                let mut args = std::env::args();
                args.next();
                let repeat: u64 = args.next().unwrap_or("1".into()).parse().unwrap();
                let mut input = String::new();
                use std::io::Read;
                std::io::stdin().read_to_string(&mut input).unwrap();
                let mut last_len: usize = [#Literal::usize_unsuffixed(0)];
                for _ in [#Literal::u64_unsuffixed(0)]..repeat {
                    std::hint::black_box(&mut input);
                    [match self {
                        Library::Toml => splat!(out;
                            let parsed: models::CargoToml = toml::from_str(&input).unwrap();
                            last_len = toml::to_string(&parsed).unwrap().len();
                        ),
                        Library::TomlEdit => splat!(out;
                            let parsed: models::CargoToml = toml_edit::de::from_str(&input).unwrap();
                            last_len = toml_edit::ser::to_string(&parsed).unwrap().len();
                        ),
                        Library::TomlSpanner | Library::TomlSpannerMacros => splat!(out;
                            let parsed: models::CargoToml = toml_spanner::from_str(&input).unwrap();
                            last_len = toml_spanner::to_string(&parsed).unwrap().len();
                        ),
                        Library::Facet => splat!(out;
                            let parsed: models::CargoToml = facet_toml::from_str(&input).unwrap();
                            last_len = facet_toml::to_string(&parsed).unwrap().len();
                        ),
                        Library::Baseline => splat!(out;
                            last_len = input.len();
                        ),
                        other => panic!("gen_toml_main not supported for {}", other.name()),
                    }]
                    std::hint::black_box(last_len);
                }
                print!("{}", last_len);
            }
        };
    }
}

fn emit_jsony_flags(out: &mut Vec<TokenTree>, flags: FLAGS) {
    let start = out.len();
    let to_json = flags & IMPL_TO_JSON != 0;
    let from_json = flags & IMPL_FROM_JSON != 0;
    let to_binary = flags & IMPL_TO_BINARY != 0;
    let from_binary = flags & IMPL_FROM_BINARY != 0;
    if to_binary && from_binary {
        splat!(out; Binary);
    } else {
        if from_binary {
            splat!(out; FromBinary);
        }
        if to_binary {
            if out.len() != start {
                splat!(out; ,);
            }
            splat!(out; ToBinary);
        }
    }
    if to_json && from_json {
        if out.len() != start {
            splat!(out; ,);
        }
        splat!(out; Json);
    } else {
        if from_json {
            if out.len() != start {
                splat!(out; ,);
            }
            splat!(out; FromJson);
        }
        if to_json {
            if out.len() != start {
                splat!(out; ,);
            }
            splat!(out; ToJson);
        }
    }
    let inner = TokenStream::from_iter(out.drain(start..));
    out.push(TokenTree::Group(Group::new(Delimiter::Parenthesis, inner)));
}

fn emit_serde_derive(out: &mut Vec<TokenTree>, flags: FLAGS) {
    let ser = flags & (IMPL_TO_JSON | IMPL_TO_BINARY | IMPL_TO_TOML) != 0;
    let de = flags & (IMPL_FROM_JSON | IMPL_FROM_BINARY | IMPL_FROM_TOML) != 0;
    let start = out.len();
    if de {
        splat!(out; Deserialize);
    }
    if ser {
        if out.len() != start {
            splat!(out; ,);
        }
        splat!(out; Serialize);
    }
    let inner = TokenStream::from_iter(out.drain(start..));
    out.push(TokenTree::Group(Group::new(Delimiter::Parenthesis, inner)));
}

fn emit_nanoserde_derive(out: &mut Vec<TokenTree>, flags: FLAGS) {
    let start = out.len();
    if flags & IMPL_FROM_BINARY != 0 {
        splat!(out; DeBin);
    }
    if flags & IMPL_TO_BINARY != 0 {
        if out.len() != start {
            splat!(out; ,);
        }
        splat!(out; SerBin);
    }
    if flags & IMPL_FROM_JSON != 0 {
        if out.len() != start {
            splat!(out; ,);
        }
        splat!(out; DeJson);
    }
    if flags & IMPL_TO_JSON != 0 {
        if out.len() != start {
            splat!(out; ,);
        }
        splat!(out; SerJson);
    }
    let inner = TokenStream::from_iter(out.drain(start..));
    out.push(TokenTree::Group(Group::new(Delimiter::Parenthesis, inner)));
}

fn emit_musli_derive(out: &mut Vec<TokenTree>, flags: FLAGS) {
    let encode = flags & (IMPL_TO_JSON | IMPL_TO_BINARY) != 0;
    let decode = flags & (IMPL_FROM_JSON | IMPL_FROM_BINARY) != 0;
    let start = out.len();
    if decode {
        splat!(out; Decode);
    }
    if encode {
        if out.len() != start {
            splat!(out; ,);
        }
        splat!(out; Encode);
    }
    let inner = TokenStream::from_iter(out.drain(start..));
    out.push(TokenTree::Group(Group::new(Delimiter::Parenthesis, inner)));
}

impl<'a> Struct<'a> {
    pub fn generate_def(&self, out: &mut Vec<TokenTree>, lib: &Library) {
        self.generate_def_flagged(out, lib, false, self.flags)
    }

    pub fn generate_def_public(&self, out: &mut Vec<TokenTree>, lib: &Library) {
        self.generate_def_flagged(out, lib, true, self.flags)
    }

    pub fn generate_def_with_flags(
        &self,
        out: &mut Vec<TokenTree>,
        lib: &Library,
        public: bool,
        flags: FLAGS,
    ) {
        self.generate_def_flagged(out, lib, public, flags)
    }

    fn generate_def_flagged(
        &self,
        out: &mut Vec<TokenTree>,
        lib: &Library,
        public: bool,
        flags: FLAGS,
    ) {
        match lib {
            Library::Jsony { .. } => self.generate_def_jsony(out, public, flags),
            Library::Merde => self.generate_def_merde(out),
            Library::Nanoserde => self.generate_def_nanoserde(out, public, flags),
            Library::Musli => self.generate_def_musli(out, public, flags),
            Library::Serde
            | Library::Sonic
            | Library::Miniserde
            | Library::Midiserde
            | Library::Toml
            | Library::TomlEdit => self.generate_def_serde(out, public, flags),
            Library::Facet => self.generate_def_facet(out, public, flags),
            Library::Baseline | Library::TomlSpanner | Library::TomlSpan => {
                self.generate_def_baseline(out, public)
            }
            Library::TomlSpannerMacros => self.generate_def_toml_spanner_macros(out, public, flags),
        }
    }

    fn generate_def_toml_spanner_macros(
        &self,
        out: &mut Vec<TokenTree>,
        public: bool,
        flags: FLAGS,
    ) {
        let from = flags & IMPL_FROM_TOML != 0;
        let to = flags & IMPL_TO_TOML != 0;
        splat! {
            out;
            #[[ derive(Toml) ]]
            [if from && to {
                splat!(out; #[[ toml(FromToml, ToToml) ]]);
            } else if from {
                splat!(out; #[[ toml(FromToml) ]]);
            } else if to {
                splat!(out; #[[ toml(ToToml) ]]);
            }]
            [?(public) pub] struct [#self.name] [?(self.lifetimes !=0) <~a>] {
                [for (field in self.fields) {
                    [if field.default && field.rename.is_some() {
                        splat!(out; #[[ toml(default, rename = [#Literal::string(field.rename.unwrap())]) ]]);
                    } else if field.default {
                        splat!(out; #[[ toml(default) ]]);
                    } else if let Some(rename) = field.rename {
                        splat!(out; #[[ toml(rename = [#Literal::string(rename)]) ]]);
                    }]
                    [?(public) pub] [#field.name]: [field.ty().gen(out)],
                }]
            }
        }
    }

    fn generate_def_naked(&self, out: &mut Vec<TokenTree>) {
        if self.kind == ItemKind::Tuple {
            splat! {
                out;
                struct [#self.name] (
                    [for (field in self.fields) {
                        [field.ty().gen(out)],
                    }]
                );
            }
            return;
        }
        splat! {
            out;
            struct [#self.name] [?(self.lifetimes !=0) <~a>] {
                [for (field in self.fields) {
                    [#field.name]: [field.ty().gen(out)],
                }]
            }
        }
    }

    fn generate_def_nanoserde(&self, out: &mut Vec<TokenTree>, public: bool, flags: FLAGS) {
        if self.kind == ItemKind::Tuple {
            splat! {
                out;
                #[[derive [emit_nanoserde_derive(out, flags)] ]]
                #[[nserde(transparent)]]
                [?(public) pub] struct [#self.name] (
                    [for (field in self.fields) {
                        [?(public) pub] [field.ty().gen(out)],
                    }]
                );
            }
            return;
        }
        splat! {
            out;
            #[[derive [emit_nanoserde_derive(out, flags)] ]]
            [?(public) pub] struct [#self.name] [?(self.lifetimes !=0) <~a>] {
                [for (field in self.fields) {
                    [if let Some(rename) = field.rename {
                        splat!(out; #[[ nserde(rename = [#Literal::string(rename)]) ]]);
                    }]
                    [?(public) pub] [#field.name]: [field.ty().gen(out)],
                }]
            }
        }
    }

    fn generate_def_merde(&self, out: &mut Vec<TokenTree>) {
        if self.kind == ItemKind::Tuple {
            splat! {
                out;
                struct [#self.name] (
                    [for (field in self.fields) {
                        [field.ty().gen(out)],
                    }]
                );
            }
            return;
        }
        let mut first = true;
        splat! {
            out;
            struct [#self.name] [?(self.lifetimes !=0) <~a>] {
                [for (field in self.fields) {
                    [#field.name]: [field.ty().gen(out)],
                }]
            }

            merde::derive! {
                impl (Serialize, Deserialize) for struct [#self.name] [?(self.lifetimes !=0) <~a>] {
                    [for field in self.fields {
                        if first {
                            first = false;
                        } else {
                            splat!(out; ,);
                        }
                        splat!(out; [#field.name]);
                    }]
                }
            }
        }
    }

    fn generate_def_baseline(&self, out: &mut Vec<TokenTree>, public: bool) {
        if self.kind == ItemKind::Tuple {
            splat! {
                out;
                [?(public) pub] struct [#self.name] (
                    [for (field in self.fields) {
                        [?(public) pub] [field.ty().gen(out)],
                    }]
                );
            }
            return;
        }
        splat! {
            out;
            [?(public) pub] struct [#self.name] [?(self.lifetimes !=0) <~a>] {
                [for (field in self.fields) {
                    [?(public) pub] [#field.name]: [field.ty().gen(out)],
                }]
            }
        }
    }

    fn generate_def_jsony(&self, out: &mut Vec<TokenTree>, public: bool, flags: FLAGS) {
        if self.kind == ItemKind::Tuple {
            splat! {
                out;
                #[[ derive(Jsony) ]]
                #[[ jsony [emit_jsony_flags(out, flags)] ]]
                [?(public) pub] struct [#self.name] (
                    [for (field in self.fields) {
                        [?(public) pub] [field.ty().gen(out)],
                    }]
                );
            }
            return;
        }
        splat! {
            out;
            #[[ derive(Jsony) ]]
            #[[ jsony [emit_jsony_flags(out, flags)] ]]
            [?(public) pub] struct [#self.name] [?(self.lifetimes !=0) <~a>] {
                [for (field in self.fields) {
                    [if let Some(rename) = field.rename {
                        splat!(out; #[[ jsony(rename = [#Literal::string(rename)]) ]]);
                    }]
                    [?(public) pub] [#field.name]: [field.ty().gen(out)],
                }]
            }
        }
    }

    pub fn generate_adv_def_jsony(&'a self, out: &mut Vec<TokenTree>) {
        let mut fields = FieldAnnotator::of_struct(self);
        splat! {
            out;
            #[[ derive(Jsony) ]]
            #[[ jsony [emit_jsony_flags(out, self.flags)] ]]
            struct [#self.name] [?(self.lifetimes !=0) <~a>] {
                [while let Some((field, mut features)) = fields.next() {
                    let attr_start = out.len();
                    while let Some(feature) = features.next() {
                        if attr_start != out.len() {
                            splat!(out; ,)
                        }
                        match feature {
                            FieldFeatureDecl::Skip => splat!(out; skip),
                            FieldFeatureDecl::TrivialDefault => splat!(out; default),
                            FieldFeatureDecl::Default{seed} => {
                                splat!(out; default = [field.ty().generate_random_default(out, seed)])
                            },
                            FieldFeatureDecl::Rename(value) => {
                                splat!(out; rename = [#Literal::string(&value)]);
                            },
                        }
                    }
                    if attr_start != out.len() {
                        let inner = TokenTree::Group(Group::new(Delimiter::Parenthesis,
                            TokenStream::from_iter(out.drain(attr_start..))
                        ));
                        splat!(out; #[[ jsony [@inner] ]])
                    }

                    splat!(out; [#field.name]: [field.ty().gen(out)],)
                }]
            }
        }
    }

    fn generate_def_facet(&self, out: &mut Vec<TokenTree>, public: bool, flags: FLAGS) {
        let skip_none = flags & IMPL_TO_TOML != 0;
        if self.kind == ItemKind::Tuple {
            splat! {
                out;
                #[[ derive(Facet) ]]
                #[[ facet(transparent) ]]
                [?(public) pub] struct [#self.name] (
                    [for (field in self.fields) {
                        [?(public) pub] [field.ty().gen(out)],
                    }]
                );
            }
            return;
        }
        splat! {
            out;
            #[[ derive(Facet) ]]
            [?(public) pub] struct [#self.name] [?(self.lifetimes !=0) <~a>] {
                [for (field in self.fields) {
                    [if skip_none && matches!(field.ty(), Type::Option(_)) {
                        if let Some(rename) = field.rename {
                            splat!(out; #[[ facet(rename = [#Literal::string(rename)], skip_serializing_if = Option::is_none) ]]);
                        } else {
                            splat!(out; #[[ facet(skip_serializing_if = Option::is_none) ]]);
                        }
                    } else if let Some(rename) = field.rename {
                        splat!(out; #[[ facet(rename = [#Literal::string(rename)]) ]]);
                    }]
                    [?(public) pub] [#field.name]: [field.ty().gen(out)],
                }]
            }
        }
    }

    fn generate_def_musli(&self, out: &mut Vec<TokenTree>, public: bool, flags: FLAGS) {
        if self.kind == ItemKind::Tuple {
            splat! {
                out;
                #[[ derive [emit_musli_derive(out, flags)] ]]
                #[[ musli(transparent) ]]
                [?(public) pub] struct [#self.name] (
                    [for (field in self.fields) {
                        [?(public) pub] [field.ty().gen(out)],
                    }]
                );
            }
            return;
        }
        splat! {
            out;
            #[[ derive [emit_musli_derive(out, flags)] ]]
            [?(public) pub] struct [#self.name] [?(self.lifetimes !=0) <~a>] {
                [for (field in self.fields) {
                    [?(field.ty().lifetimes() !=0) #[[serde(borrow)]]]
                    [if let Some(rename) = field.rename {
                        splat!(out; #[[ musli(Text, name = [#Literal::string(rename)]) ]]);
                    }]
                    [?(public) pub] [#field.name]: [field.ty().gen(out)],
                }]
            }
        }
    }

    fn generate_def_serde(&self, out: &mut Vec<TokenTree>, public: bool, flags: FLAGS) {
        if self.kind == ItemKind::Tuple {
            splat! {
                out;
                #[[ derive [emit_serde_derive(out, flags)] ]]
                [?(public) pub] struct [#self.name] (
                    [for (field in self.fields) {
                        [?(public) pub] [field.ty().gen(out)],
                    }]
                );
            }
            return;
        }
        splat! {
            out;
            #[[ derive [emit_serde_derive(out, flags)] ]]
            [?(public) pub] struct [#self.name] [?(self.lifetimes !=0) <~a>] {
                [for (field in self.fields) {
                    [?(field.ty().lifetimes() !=0) #[[serde(borrow)]]]
                    [if let Some(rename) = field.rename {
                        if field.default {
                            splat!(out; #[[ serde(rename = [#Literal::string(rename)], default) ]]);
                        } else {
                            splat!(out; #[[ serde(rename = [#Literal::string(rename)]) ]]);
                        }
                    } else if field.default {
                        splat!(out; #[[ serde(default) ]]);
                    }]
                    [?(public) pub] [#field.name]: [field.ty().gen(out)],
                }]
            }
        }
    }
}

impl<'a> Enum<'a> {
    pub fn generate_def(&self, out: &mut Vec<TokenTree>, lib: &Library) {
        self.generate_def_flagged(out, lib, false, self.flags)
    }

    pub fn generate_def_public(&self, out: &mut Vec<TokenTree>, lib: &Library) {
        self.generate_def_flagged(out, lib, true, self.flags)
    }

    pub fn generate_def_with_flags(
        &self,
        out: &mut Vec<TokenTree>,
        lib: &Library,
        public: bool,
        flags: FLAGS,
    ) {
        self.generate_def_flagged(out, lib, public, flags)
    }

    fn generate_def_flagged(
        &self,
        out: &mut Vec<TokenTree>,
        lib: &Library,
        public: bool,
        flags: FLAGS,
    ) {
        match lib {
            Library::Jsony { .. } => self.generate_def_jsony(out, public, flags),
            Library::Nanoserde => self.generate_def_nanoserde(out, public, flags),
            Library::Musli => self.generate_def_musli(out, public, flags),
            Library::Serde
            | Library::Sonic
            | Library::Miniserde
            | Library::Midiserde
            | Library::Toml
            | Library::TomlEdit => self.generate_def_serde(out, public, flags),
            Library::Facet => self.generate_def_facet(out, public),
            Library::Baseline | Library::TomlSpanner | Library::TomlSpan => {
                self.generate_def_baseline(out, public)
            }
            Library::TomlSpannerMacros => {
                panic!("enum generate_def not supported for {:?}", lib.name())
            }
            Library::Merde => {
                panic!("enum generate_def not supported for {:?}", lib.name())
            }
        }
    }

    fn emit_variant_fields_jsony(out: &mut Vec<TokenTree>, variant: &crate::schema::EnumVariant) {
        out.push(variant.name.clone().into());
        if variant.fields.is_empty() {
            splat!(out; ,);
        } else {
            let start = out.len();
            for field in variant.fields {
                if let Some(rename) = field.rename {
                    splat!(out; #[[ jsony(rename = [#Literal::string(rename)]) ]]);
                }
                splat!(out; [#field.name]: [field.ty().gen(out)],);
            }
            let inner = TokenStream::from_iter(out.drain(start..));
            out.push(TokenTree::Group(Group::new(Delimiter::Brace, inner)));
            splat!(out; ,);
        }
    }

    fn generate_def_jsony(&self, out: &mut Vec<TokenTree>, public: bool, flags: FLAGS) {
        splat! {
            out;
            #[[ derive(Jsony) ]]
            #[[ jsony [emit_jsony_flags(out, flags)] ]]
            [?(public) pub] enum [#self.name] {
                [for (variant in &self.variants) {
                    [Self::emit_variant_fields_jsony(out, variant)]
                }]
            }
        }
    }

    fn emit_variant_fields_serde(out: &mut Vec<TokenTree>, variant: &crate::schema::EnumVariant) {
        out.push(variant.name.clone().into());
        if variant.fields.is_empty() {
            splat!(out; ,);
        } else {
            let start = out.len();
            for field in variant.fields {
                if let Some(rename) = field.rename {
                    splat!(out; #[[ serde(rename = [#Literal::string(rename)]) ]]);
                }
                splat!(out; [#field.name]: [field.ty().gen(out)],);
            }
            let inner = TokenStream::from_iter(out.drain(start..));
            out.push(TokenTree::Group(Group::new(Delimiter::Brace, inner)));
            splat!(out; ,);
        }
    }

    fn generate_def_serde(&self, out: &mut Vec<TokenTree>, public: bool, flags: FLAGS) {
        splat! {
            out;
            #[[ derive [emit_serde_derive(out, flags)] ]]
            [?(public) pub] enum [#self.name] {
                [for (variant in &self.variants) {
                    [Self::emit_variant_fields_serde(out, variant)]
                }]
            }
        }
    }

    fn emit_variant_fields_nanoserde(
        out: &mut Vec<TokenTree>,
        variant: &crate::schema::EnumVariant,
    ) {
        out.push(variant.name.clone().into());
        if variant.fields.is_empty() {
            splat!(out; ,);
        } else {
            let start = out.len();
            for field in variant.fields {
                if let Some(rename) = field.rename {
                    splat!(out; #[[ nserde(rename = [#Literal::string(rename)]) ]]);
                }
                splat!(out; [#field.name]: [field.ty().gen(out)],);
            }
            let inner = TokenStream::from_iter(out.drain(start..));
            out.push(TokenTree::Group(Group::new(Delimiter::Brace, inner)));
            splat!(out; ,);
        }
    }

    fn generate_def_nanoserde(&self, out: &mut Vec<TokenTree>, public: bool, flags: FLAGS) {
        splat! {
            out;
            #[[ derive [emit_nanoserde_derive(out, flags)] ]]
            [?(public) pub] enum [#self.name] {
                [for (variant in &self.variants) {
                    [Self::emit_variant_fields_nanoserde(out, variant)]
                }]
            }
        }
    }

    fn emit_variant_fields_musli(out: &mut Vec<TokenTree>, variant: &crate::schema::EnumVariant) {
        out.push(variant.name.clone().into());
        if variant.fields.is_empty() {
            splat!(out; ,);
        } else {
            let start = out.len();
            for field in variant.fields {
                if let Some(rename) = field.rename {
                    splat!(out; #[[ musli(Text, name = [#Literal::string(rename)]) ]]);
                }
                splat!(out; [#field.name]: [field.ty().gen(out)],);
            }
            let inner = TokenStream::from_iter(out.drain(start..));
            out.push(TokenTree::Group(Group::new(Delimiter::Brace, inner)));
            splat!(out; ,);
        }
    }

    fn generate_def_musli(&self, out: &mut Vec<TokenTree>, public: bool, flags: FLAGS) {
        splat! {
            out;
            #[[ derive [emit_musli_derive(out, flags)] ]]
            [?(public) pub] enum [#self.name] {
                [for (variant in &self.variants) {
                    [Self::emit_variant_fields_musli(out, variant)]
                }]
            }
        }
    }

    fn emit_variant_fields_facet(out: &mut Vec<TokenTree>, variant: &crate::schema::EnumVariant) {
        out.push(variant.name.clone().into());
        if variant.fields.is_empty() {
            splat!(out; ,);
        } else {
            let start = out.len();
            for field in variant.fields {
                if let Some(rename) = field.rename {
                    splat!(out; #[[ facet(rename = [#Literal::string(rename)]) ]]);
                }
                splat!(out; [#field.name]: [field.ty().gen(out)],);
            }
            let inner = TokenStream::from_iter(out.drain(start..));
            out.push(TokenTree::Group(Group::new(Delimiter::Brace, inner)));
            splat!(out; ,);
        }
    }

    fn generate_def_facet(&self, out: &mut Vec<TokenTree>, public: bool) {
        splat! {
            out;
            #[[ derive(Facet) ]]
            #[[ repr(C) ]]
            [?(public) pub] enum [#self.name] {
                [for (variant in &self.variants) {
                    [Self::emit_variant_fields_facet(out, variant)]
                }]
            }
        }
    }

    fn emit_variant_fields_baseline(
        out: &mut Vec<TokenTree>,
        variant: &crate::schema::EnumVariant,
    ) {
        out.push(variant.name.clone().into());
        if variant.fields.is_empty() {
            splat!(out; ,);
        } else {
            let start = out.len();
            for field in variant.fields {
                splat!(out; [#field.name]: [field.ty().gen(out)],);
            }
            let inner = TokenStream::from_iter(out.drain(start..));
            out.push(TokenTree::Group(Group::new(Delimiter::Brace, inner)));
            splat!(out; ,);
        }
    }

    fn generate_def_baseline(&self, out: &mut Vec<TokenTree>, public: bool) {
        splat! {
            out;
            [?(public) pub] enum [#self.name] {
                [for (variant in &self.variants) {
                    [Self::emit_variant_fields_baseline(out, variant)]
                }]
            }
        }
    }
}
