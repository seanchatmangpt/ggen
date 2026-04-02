use std::fs;
use std::io::Read;

fn main() {
    let file_path = "crates/ggen-core/src/validation/input.rs";
    let mut content = fs::read_to_string(file_path).unwrap();

    // Remove all .into() calls from InputValidationError errors
    content = content.replace(
        "InputValidationError::LengthViolation {\n                    field: field_name.to_string(),\n                    actual: len,\n                    constraint: format!(\"min {}\", min),\n                }.into();",
        "Error::new(&format!(\"Length violation: {} has length {}, minimum required is {}\", field_name, len, min));"
    );

    content = content.replace(
        "InputValidationError::LengthViolation {\n                    field: field_name.to_string(),\n                    actual: len,\n                    constraint: format!(\"max {}\", max),\n                }.into();",
        "Error::new(&format!(\"Length violation: {} has length {}, maximum allowed is {}\", field_name, len, max));"
    );

    content = content.replace(
        "InputValidationError::LengthViolation {\n                    field: field_name.to_string(),\n                    actual: len,\n                    constraint: format!(\"{} to {}\", min, max),\n                }.into();",
        "Error::new(&format!(\"Length violation: {} has length {}, range is {} to {}\", field_name, len, min, max));"
    );

    content = content.replace(
        "InputValidationError::PatternViolation {\n                    field: field_name.to_string(),\n                    actual: value.clone(),\n                    constraint: format!(\"pattern {}\", pattern),\n                }.into();",
        "Error::new(&format!(\"Pattern violation: {} '{}' does not match pattern {}\", field_name, value, pattern));"
    );

    content = content.replace(
        "InputValidationError::UrlViolation {\n                    field: field_name.to_string(),\n                    actual: value.clone(),\n                    reason: \"invalid URL format\".to_string(),\n                }.into();",
        "Error::new(&format!(\"URL violation: {} '{}' is not a valid URL\", field_name, value));"
    );

    content = content.replace(
        "InputValidationError::UrlViolation {\n                    field: field_name.to_string(),\n                    actual: url_str.to_string(),\n                    reason: reason.clone(),\n                }.into();",
        "Error::new(&format!(\"URL violation: {} '{}': {}\", field_name, url_str, reason));"
    );

    content = content.replace(
        "InputValidationError::RangeViolation {\n                    field: field_name.to_string(),\n                    actual: input.to_string(),\n                    constraint: constraint.to_string(),\n                }.into();",
        "Error::new(&format!(\"Range violation: {} must be {} (got {})\", field_name, constraint, input));"
    );

    content = content.replace(
        "InputValidationError::FormatViolation {\n                    field: field_name.to_string(),\n                    reason: reason.clone(),\n                }.into();",
        "Error::new(&format!(\"Format violation: {}: {}\", field_name, reason));"
    );

    content = content.replace(
        "InputValidationError::CompositeViolation {\n                    reason: reason.to_string(),\n                }.into();",
        "Error::new(&format!(\"Composite violation: {}\", reason));"
    );

    content = content.replace(
        "InputValidationError::EmptyInput {\n                    field: field_name.to_string(),\n                }.into();",
        "Error::new(&format!(\"Empty input for field: {}\", field_name));"
    );

    content = content.replace(
        "InputValidationError::EmptyInput {\n                    field: \"input\".to_string(),\n                }.into();",
        "Error::new(\"Input validation failed: no input provided\");"
    );

    content = content.replace(
        "InputValidationError::General {\n                    message: message.to_string(),\n                }.into();",
        "Error::new(message);"
    );

    content = content.replace(
        "InputValidationError::General {\n                    message: format!(\"Path validation failed: {}\", e),\n                }.into();",
        "Error::new(&format!(\"Path validation failed: {}\", e));"
    );

    // Write back to file
    fs::write(file_path, content).unwrap();
    println!("Fixed validation errors in {}", file_path);
}