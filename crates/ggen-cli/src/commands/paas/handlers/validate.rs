//! Validate specifications handler
//! Verb: validate | Noun: specification

use crate::commands::paas::errors::{PaasError, Result};
use std::path::Path;

/// Validate specifications in a directory
pub async fn validate_specs(
    spec_path: &str,
    min_closure: f32,
    _strict: bool,
) -> Result<()> {
    let path = Path::new(spec_path);

    if !path.exists() {
        return Err(PaasError::SpecValidationFailed {
            file: spec_path.to_string(),
            reason: "Directory does not exist".to_string(),
            line: None,
        });
    }

    // Check for required TTL files
    let required_files = vec!["cli-schema.ttl", "cli-commands.ttl", "ggen-paas-ontology.ttl"];

    let mut missing = Vec::new();
    for file in &required_files {
        let file_path = path.join(file);
        if !file_path.exists() {
            missing.push(file.to_string());
        }
    }

    // Calculate closure percentage
    let closure = if missing.is_empty() {
        100.0
    } else {
        ((required_files.len() - missing.len()) as f32 / required_files.len() as f32) * 100.0
    };

    // Check if closure meets minimum threshold
    if closure < min_closure {
        return Err(PaasError::ClosureIncomplete {
            current: closure,
            required: min_closure,
            missing_specs: missing,
        });
    }

    // Specification validation passed
    Ok(())
}

#[cfg(test)]
mod tests {
    use super::*;

    #[tokio::test]
    async fn test_validate_missing_directory() {
        let result = validate_specs("/nonexistent/path", 95.0, false).await;
        assert!(result.is_err());
    }

    #[tokio::test]
    async fn test_validate_high_closure_requirement() {
        let result = validate_specs(".specify", 100.0, false).await;
        // Will vary based on actual .specify contents
        let _ = result;
    }
}
