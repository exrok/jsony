use jsony_value::{Value, ValueBoolean, ValueMap, ValueNumber, ValueRef};
use serde_json::Value as SerdeValue;
use std::collections::HashSet;
use std::fmt::Write;

pub enum ValidationOutcome {
    Equivalent,
    IgnoredInvalidSerdeJson { message: String },
}

pub struct ValidationError {
    path: JsonPath,
    issue: String,
    serde_json: Option<String>,
    jsony_value: Option<String>,
}

impl ValidationError {
    fn new(
        path: &JsonPath,
        issue: impl Into<String>,
        serde_value: Option<&SerdeValue>,
        jsony_value: Option<&Value<'_>>,
    ) -> Self {
        Self {
            path: path.clone(),
            issue: issue.into(),
            serde_json: serde_value.map(format_serde_value),
            jsony_value: jsony_value.map(format_jsony_value),
        }
    }

    fn parse_error(path: &JsonPath, issue: impl Into<String>, serde_value: &SerdeValue) -> Self {
        Self {
            path: path.clone(),
            issue: issue.into(),
            serde_json: Some(format_serde_value(serde_value)),
            jsony_value: None,
        }
    }

    pub fn path(&self) -> &JsonPath {
        &self.path
    }

    pub fn issue(&self) -> &str {
        &self.issue
    }

    pub fn render_report(&self) -> String {
        let mut output = String::new();
        self.push_report_body(&mut output);
        output
    }

    pub fn render_report_with_input(&self, input: &[u8]) -> String {
        let mut output = String::new();
        output.push_str("input (escaped):\n");
        push_escaped_input(&mut output, input);
        output.push('\n');
        self.push_report_body(&mut output);
        output
    }

    fn push_report_body(&self, output: &mut String) {
        let _ = writeln!(output, "path: {}", self.path);
        let _ = writeln!(output, "issue: {}", self.issue);

        if let (Some(serde_json), Some(jsony_value)) = (&self.serde_json, &self.jsony_value) {
            output.push_str("diff:\n--- serde_json\n+++ jsony_value\n");
            push_diff_lines(output, '-', serde_json);
            push_diff_lines(output, '+', jsony_value);
        } else if let Some(serde_json) = &self.serde_json {
            output.push_str("serde_json:\n");
            output.push_str(serde_json);
            output.push('\n');
        }
    }
}

impl std::fmt::Display for ValidationError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.write_str(&self.render_report())
    }
}

impl std::fmt::Debug for ValidationError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("ValidationError")
            .field("path", &self.path.to_string())
            .field("issue", &self.issue)
            .field("serde_json", &self.serde_json)
            .field("jsony_value", &self.jsony_value)
            .finish()
    }
}

impl std::error::Error for ValidationError {}

#[derive(Clone, Default)]
pub struct JsonPath {
    segments: Vec<PathSegment>,
}

impl JsonPath {
    fn push_key(&mut self, key: &str) {
        self.segments.push(PathSegment::Key(key.to_owned()));
    }

    fn push_index(&mut self, index: usize) {
        self.segments.push(PathSegment::Index(index));
    }

    fn pop(&mut self) {
        self.segments.pop();
    }
}

impl std::fmt::Display for JsonPath {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.write_str("$")?;
        for segment in &self.segments {
            match segment {
                PathSegment::Index(index) => write!(f, "[{index}]")?,
                PathSegment::Key(key) if is_simple_path_key(key) => {
                    f.write_char('.')?;
                    f.write_str(key)?;
                }
                PathSegment::Key(key) => {
                    f.write_char('[')?;
                    f.write_str(&serde_json::to_string(key).map_err(|_| std::fmt::Error)?)?;
                    f.write_char(']')?;
                }
            }
        }
        Ok(())
    }
}

#[derive(Clone)]
enum PathSegment {
    Key(String),
    Index(usize),
}

pub fn assert_serde_json_equivalence(data: &[u8]) {
    if is_serde_json_equivalent(data) {
        return;
    }

    match validate_serde_json_equivalence(data) {
        Ok(_) => panic!("serde_json and jsony_value equivalence check failed"),
        Err(error) => panic!("{}", error.render_report_with_input(data)),
    }
}

pub fn is_serde_json_equivalent(data: &[u8]) -> bool {
    let Ok(serde_value) = serde_json::from_slice::<SerdeValue>(data) else {
        return true;
    };

    let Ok(jsony_value) = jsony::from_json_bytes::<Value<'_>>(data) else {
        return false;
    };

    compare_values(&serde_value, &jsony_value)
}

pub fn validate_serde_json_equivalence(data: &[u8]) -> Result<ValidationOutcome, ValidationError> {
    let serde_value = match serde_json::from_slice::<SerdeValue>(data) {
        Ok(value) => value,
        Err(error) => {
            return Ok(ValidationOutcome::IgnoredInvalidSerdeJson {
                message: error.to_string(),
            });
        }
    };

    let path = JsonPath::default();
    let jsony_value = jsony::from_json_bytes::<Value<'_>>(data).map_err(|error| {
        ValidationError::parse_error(
            &path,
            format!("serde_json accepted input but jsony_value rejected it: {error:?}"),
            &serde_value,
        )
    })?;

    let mut path = JsonPath::default();
    compare_values_detailed(&serde_value, &jsony_value, &mut path)?;

    Ok(ValidationOutcome::Equivalent)
}

fn compare_values(serde_value: &SerdeValue, jsony_value: &Value<'_>) -> bool {
    match (serde_value, jsony_value.as_ref()) {
        (SerdeValue::Null, ValueRef::Null(_)) => true,
        (SerdeValue::Bool(serde_value), ValueRef::Boolean(jsony_value)) => {
            compare_bool(*serde_value, jsony_value)
        }
        (SerdeValue::Number(serde_value), ValueRef::Number(jsony_value)) => {
            compare_number(serde_value, jsony_value)
        }
        (SerdeValue::String(serde_value), ValueRef::String(jsony_value)) => {
            compare_str(serde_value, jsony_value.as_str())
        }
        (SerdeValue::Array(serde_values), ValueRef::List(jsony_values)) => {
            let jsony_values = jsony_values.as_slice();
            if serde_values.len() != jsony_values.len() {
                return false;
            }

            for (serde_value, jsony_value) in serde_values.iter().zip(jsony_values) {
                if !compare_values(serde_value, jsony_value) {
                    return false;
                }
            }

            true
        }
        (SerdeValue::Object(serde_values), ValueRef::Map(jsony_values)) => {
            compare_objects(serde_values, jsony_values)
        }
        _ => false,
    }
}

fn compare_bool(serde_value: bool, jsony_value: &ValueBoolean) -> bool {
    serde_value == (jsony_value.value != 0)
}

fn compare_number(serde_value: &serde_json::Number, jsony_value: &ValueNumber) -> bool {
    match jsony_value {
        ValueNumber::U64(jsony_value) => serde_value.as_u64() == Some(*jsony_value),
        ValueNumber::I64(jsony_value) => serde_value.as_i64() == Some(*jsony_value),
        ValueNumber::F64(jsony_value) => serde_value.as_f64() == Some(*jsony_value),
    }
}

fn compare_str(serde_value: &str, jsony_value: &str) -> bool {
    serde_value == jsony_value
}

fn compare_objects(
    serde_values: &serde_json::Map<String, SerdeValue>,
    jsony_values: &ValueMap<'_>,
) -> bool {
    let entries = jsony_values.entries();
    if serde_values.len() == entries.len() {
        for ((serde_key, serde_value), (jsony_key, jsony_value)) in serde_values.iter().zip(entries)
        {
            if serde_key != jsony_key.as_str() || !compare_values(serde_value, jsony_value) {
                return false;
            }
        }

        return true;
    }

    if serde_values.len() > entries.len() {
        return false;
    }

    let mut seen_keys = HashSet::with_capacity(serde_values.len());
    let mut serde_iter = serde_values.iter();

    for (jsony_key, _) in entries {
        let jsony_key = jsony_key.as_str();
        if !seen_keys.insert(jsony_key) {
            continue;
        }

        let Some((serde_key, _)) = serde_iter.next() else {
            return false;
        };

        if serde_key != jsony_key {
            return false;
        }
    }

    if serde_iter.next().is_some() {
        return false;
    }

    seen_keys.clear();
    let mut skipped = 0;

    for (jsony_key, jsony_value) in entries.iter().rev() {
        let jsony_key = jsony_key.as_str();
        if !seen_keys.insert(jsony_key) {
            skipped += 1;
            continue;
        }

        let Some(serde_value) = serde_values.get(jsony_key) else {
            return false;
        };

        if !compare_values(serde_value, jsony_value) {
            return false;
        }
    }

    skipped + serde_values.len() == entries.len() && seen_keys.len() == serde_values.len()
}

fn compare_values_detailed(
    serde_value: &SerdeValue,
    jsony_value: &Value<'_>,
    path: &mut JsonPath,
) -> Result<(), ValidationError> {
    match (serde_value, jsony_value.as_ref()) {
        (SerdeValue::Null, ValueRef::Null(_)) => Ok(()),
        (SerdeValue::Bool(serde_bool), ValueRef::Boolean(jsony_bool)) => {
            if compare_bool(*serde_bool, jsony_bool) {
                Ok(())
            } else {
                Err(ValidationError::new(
                    path,
                    "boolean values differ",
                    Some(serde_value),
                    Some(jsony_value),
                ))
            }
        }
        (SerdeValue::Number(serde_number), ValueRef::Number(jsony_number)) => {
            if compare_number(serde_number, jsony_number) {
                Ok(())
            } else {
                Err(ValidationError::new(
                    path,
                    "number values differ",
                    Some(serde_value),
                    Some(jsony_value),
                ))
            }
        }
        (SerdeValue::String(serde_text), ValueRef::String(jsony_text)) => {
            if compare_str(serde_text, jsony_text.as_str()) {
                Ok(())
            } else {
                Err(ValidationError::new(
                    path,
                    "string values differ",
                    Some(serde_value),
                    Some(jsony_value),
                ))
            }
        }
        (SerdeValue::Array(serde_values), ValueRef::List(jsony_values)) => {
            compare_arrays_detailed(serde_values, jsony_values.as_slice(), path)
        }
        (SerdeValue::Object(serde_values), ValueRef::Map(jsony_values)) => {
            compare_objects_detailed(serde_values, jsony_values, path)
        }
        _ => Err(ValidationError::new(
            path,
            format!(
                "type mismatch: serde_json produced {}, jsony_value produced {}",
                serde_type_name(serde_value),
                jsony_type_name(jsony_value)
            ),
            Some(serde_value),
            Some(jsony_value),
        )),
    }
}

fn compare_arrays_detailed(
    serde_values: &[SerdeValue],
    jsony_values: &[Value<'_>],
    path: &mut JsonPath,
) -> Result<(), ValidationError> {
    if serde_values.len() != jsony_values.len() {
        return Err(ValidationError::new(
            path,
            format!(
                "array length mismatch: serde_json has {}, jsony_value has {}",
                serde_values.len(),
                jsony_values.len()
            ),
            None,
            None,
        ));
    }

    for (index, (serde_value, jsony_value)) in serde_values.iter().zip(jsony_values).enumerate() {
        path.push_index(index);
        let result = compare_values_detailed(serde_value, jsony_value, path);
        path.pop();
        result?;
    }

    Ok(())
}

fn compare_objects_detailed(
    serde_values: &serde_json::Map<String, SerdeValue>,
    jsony_values: &ValueMap<'_>,
    path: &mut JsonPath,
) -> Result<(), ValidationError> {
    let entries = jsony_values.entries();
    if serde_values.len() == entries.len() {
        for ((serde_key, serde_value), (jsony_key, jsony_value)) in serde_values.iter().zip(entries)
        {
            if serde_key != jsony_key.as_str() {
                return Err(ValidationError::new(
                    path,
                    format!(
                        "object key order mismatch: serde_json key {serde_key:?}, jsony_value key {:?}",
                        jsony_key.as_str()
                    ),
                    None,
                    None,
                ));
            }

            path.push_key(serde_key);
            let result = compare_values_detailed(serde_value, jsony_value, path);
            path.pop();
            result?;
        }

        return Ok(());
    }

    if serde_values.len() > entries.len() {
        return Err(ValidationError::new(
            path,
            format!(
                "object length mismatch: serde_json has {}, jsony_value has {}",
                serde_values.len(),
                entries.len()
            ),
            None,
            None,
        ));
    }

    let mut seen_keys = HashSet::with_capacity(serde_values.len());
    let mut serde_iter = serde_values.iter();

    for (jsony_key, _) in entries {
        let jsony_key = jsony_key.as_str();
        if !seen_keys.insert(jsony_key) {
            continue;
        }

        let Some((serde_key, _)) = serde_iter.next() else {
            return Err(ValidationError::new(
                path,
                "jsony_value has object entries that serde_json did not retain",
                None,
                None,
            ));
        };

        if serde_key != jsony_key {
            return Err(ValidationError::new(
                path,
                format!(
                    "object first-key order mismatch: serde_json key {serde_key:?}, jsony_value key {jsony_key:?}"
                ),
                None,
                None,
            ));
        }
    }

    if let Some((serde_key, _)) = serde_iter.next() {
        return Err(ValidationError::new(
            path,
            format!("serde_json retained key {serde_key:?} that jsony_value did not produce"),
            None,
            None,
        ));
    }

    seen_keys.clear();
    let mut skipped = 0;

    for (jsony_key, jsony_value) in entries.iter().rev() {
        let jsony_key = jsony_key.as_str();
        if !seen_keys.insert(jsony_key) {
            skipped += 1;
            continue;
        }

        let Some(serde_value) = serde_values.get(jsony_key) else {
            return Err(ValidationError::new(
                path,
                format!("jsony_value produced key {jsony_key:?} that serde_json did not retain"),
                None,
                Some(jsony_value),
            ));
        };

        path.push_key(jsony_key);
        let result = compare_values_detailed(serde_value, jsony_value, path);
        path.pop();
        result?;
    }

    if skipped + serde_values.len() != entries.len() || seen_keys.len() != serde_values.len() {
        return Err(ValidationError::new(
            path,
            format!(
                "duplicate-key accounting mismatch: skipped {skipped}, serde_json has {}, jsony_value has {}",
                serde_values.len(),
                entries.len()
            ),
            None,
            None,
        ));
    }

    Ok(())
}

fn serde_type_name(value: &SerdeValue) -> &'static str {
    match value {
        SerdeValue::Null => "null",
        SerdeValue::Bool(_) => "boolean",
        SerdeValue::Number(_) => "number",
        SerdeValue::String(_) => "string",
        SerdeValue::Array(_) => "array",
        SerdeValue::Object(_) => "object",
    }
}

fn jsony_type_name(value: &Value<'_>) -> &'static str {
    match value.as_ref() {
        ValueRef::Null(_) => "null",
        ValueRef::Boolean(_) => "boolean",
        ValueRef::Number(_) => "number",
        ValueRef::String(_) => "string",
        ValueRef::Other(_) => "other",
        ValueRef::List(_) => "array",
        ValueRef::Map(_) => "object",
    }
}

fn format_serde_value(value: &SerdeValue) -> String {
    serde_json::to_string_pretty(value).unwrap_or_else(|_| value.to_string())
}

fn format_jsony_value(value: &Value<'_>) -> String {
    jsony::to_json(value)
}

fn push_diff_lines(output: &mut String, marker: char, value: &str) {
    for line in value.lines() {
        let _ = writeln!(output, "{marker} {line}");
    }
}

fn push_escaped_input(output: &mut String, input: &[u8]) {
    if input.is_empty() {
        output.push_str("<empty>");
        return;
    }

    for byte in input {
        for escaped in std::ascii::escape_default(*byte) {
            output.push(char::from(escaped));
        }
    }
}

fn is_simple_path_key(key: &str) -> bool {
    let mut chars = key.chars();
    let Some(first) = chars.next() else {
        return false;
    };

    if !(first == '_' || first.is_ascii_alphabetic()) {
        return false;
    }

    chars.all(|ch| ch == '_' || ch.is_ascii_alphanumeric())
}
