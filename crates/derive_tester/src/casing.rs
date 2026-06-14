//! Port of jsony's case conversion (`crates/jsony_macros_source/src/case.rs`).
//!
//! Kept byte-for-byte equivalent to the derive's own algorithm so the sampler
//! derives the exact JSON keys the generated `ToJson`/`FromJson` expect. Any
//! divergence would show up as a false round-trip mismatch, indistinguishable
//! from a real jsony bug, so this is a deliberate copy rather than an
//! approximation.

use rand::prelude::*;

#[derive(Clone, Copy, PartialEq, Eq)]
pub enum RenameRule {
    None,
    LowerCase,
    UpperCase,
    PascalCase,
    CamelCase,
    SnakeCase,
    ScreamingSnakeCase,
    KebabCase,
    ScreamingKebabCase,
}

/// Every rule with an explicit `rename_all` spelling (excludes `None`).
const NAMED: &[RenameRule] = &[
    RenameRule::LowerCase,
    RenameRule::UpperCase,
    RenameRule::PascalCase,
    RenameRule::CamelCase,
    RenameRule::SnakeCase,
    RenameRule::ScreamingSnakeCase,
    RenameRule::KebabCase,
    RenameRule::ScreamingKebabCase,
];

impl RenameRule {
    /// Pick a named rule uniformly at random (never `None`).
    pub fn random(rng: &mut StdRng) -> RenameRule {
        *NAMED.choose(rng).unwrap()
    }

    /// The `rename_all = "..."` literal value, or `None` for the identity rule.
    pub fn attr_value(self) -> Option<&'static str> {
        Some(match self {
            RenameRule::None => return None,
            RenameRule::LowerCase => "lowercase",
            RenameRule::UpperCase => "UPPERCASE",
            RenameRule::PascalCase => "PascalCase",
            RenameRule::CamelCase => "camelCase",
            RenameRule::SnakeCase => "snake_case",
            RenameRule::ScreamingSnakeCase => "SCREAMING_SNAKE_CASE",
            RenameRule::KebabCase => "kebab-case",
            RenameRule::ScreamingKebabCase => "SCREAMING-KEBAB-CASE",
        })
    }

    /// Apply the rule to an enum variant name, matching jsony's output.
    pub fn apply_to_variant(self, variant: &str) -> String {
        use RenameRule::*;
        match self {
            None | PascalCase => variant.to_owned(),
            LowerCase => variant.to_ascii_lowercase(),
            UpperCase => variant.to_ascii_uppercase(),
            CamelCase => variant[..1].to_ascii_lowercase() + &variant[1..],
            SnakeCase => {
                let mut snake = String::new();
                for (i, ch) in variant.char_indices() {
                    if i > 0 && ch.is_uppercase() {
                        snake.push('_');
                    }
                    snake.push(ch.to_ascii_lowercase());
                }
                snake
            }
            ScreamingSnakeCase => SnakeCase.apply_to_variant(variant).to_ascii_uppercase(),
            KebabCase => SnakeCase.apply_to_variant(variant).replace('_', "-"),
            ScreamingKebabCase => ScreamingSnakeCase
                .apply_to_variant(variant)
                .replace('_', "-"),
        }
    }

    /// Apply the rule to a struct field name, matching jsony's output.
    pub fn apply_to_field(self, field: &str) -> String {
        use RenameRule::*;
        match self {
            None | LowerCase | SnakeCase => field.to_owned(),
            UpperCase => field.to_ascii_uppercase(),
            PascalCase => {
                let mut pascal = String::new();
                let mut capitalize = true;
                for ch in field.chars() {
                    if ch == '_' {
                        capitalize = true;
                    } else if capitalize {
                        pascal.push(ch.to_ascii_uppercase());
                        capitalize = false;
                    } else {
                        pascal.push(ch);
                    }
                }
                pascal
            }
            CamelCase => {
                let pascal = PascalCase.apply_to_field(field);
                pascal[..1].to_ascii_lowercase() + &pascal[1..]
            }
            ScreamingSnakeCase => field.to_ascii_uppercase(),
            KebabCase => field.replace('_', "-"),
            ScreamingKebabCase => ScreamingSnakeCase.apply_to_field(field).replace('_', "-"),
        }
    }
}
