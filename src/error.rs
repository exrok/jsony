use crate::json::DecodeError;

/// Error used for FromStr enum implementation
#[derive(Clone, Copy, Debug)]
pub struct UnknownVariant;

impl std::fmt::Display for UnknownVariant {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.write_str("Unknown Variant")
    }
}

impl std::error::Error for UnknownVariant {}

pub static CUSTOM_FIELD_VALIDATION_ERROR: DecodeError = DecodeError {
    message: "Parsed value failed validation",
};

pub static EMPTY_OBJECT_FOR_EXTERNALLY_TAGGED_ENUM: DecodeError = DecodeError {
    message: "Expected an object containing a single field naming the enum variant but instead found an empty object",
};

pub static MULTIPLE_FIELDS_FOR_EXTERNALLY_TAGGED_ENUM: DecodeError = DecodeError {
    message: "Expected an object containing a single field naming the enum variant however the object contains additional fields",
};

pub static NO_FIELD_MATCHED_AN_ENUM_VARIANT: DecodeError = DecodeError {
    message: "No fields in the object matched any enum variant",
};

pub static UNKNOWN_VARIANT: DecodeError = DecodeError {
    message: "Unknown enum variant",
};

pub static DUPLICATE_FIELD: DecodeError = DecodeError {
    message: "Duplicate field",
};

pub static RECURSION_LIMIT_EXCEEDED: DecodeError = DecodeError {
    message: "Recursion limit exceeded",
};

pub static MISSING_REQUIRED_FIELDS: DecodeError = DecodeError {
    message: "Missing required fields",
};

pub static MISSING_CONTENT_TAG: DecodeError = DecodeError {
    message: "Missing content tag",
};
