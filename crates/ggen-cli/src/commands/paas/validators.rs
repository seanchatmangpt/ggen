//! Validators for PaaS operations
//! Validates specifications, artifacts, and environments before operations

use crate::commands::paas::errors::{PaasError, Result};
use std::path::Path;

/// Validate specification closure
pub async fn validate_closure(spec_dir: &str, min_percentage: f32) -> Result<()> {
    let path = Path::new(spec_dir);

    if spec_dir.is_empty() {
        return Ok(()); // Skip if no spec dir specified
    }

    if !path.exists() {
        return Err(PaasError::SpecValidationFailed {
            file: spec_dir.to_string(),
            reason: "Specification directory does not exist".to_string(),
            line: None,
        });
    }

    // Check for required TTL files
    let required_files = vec!["cli-schema.ttl", "cli-commands.ttl", "ggen-paas-ontology.ttl"];

    let mut found = 0;
    for file in &required_files {
        if path.join(file).exists() {
            found += 1;
        }
    }

    let closure_percentage = (found as f32 / required_files.len() as f32) * 100.0;

    if closure_percentage < min_percentage {
        let missing: Vec<String> = required_files
            .iter()
            .filter(|f| !path.join(*f).exists())
            .map(|s| s.to_string())
            .collect();

        return Err(PaasError::ClosureIncomplete {
            current: closure_percentage,
            required: min_percentage,
            missing_specs: missing,
        });
    }

    Ok(())
}

#[cfg(test)]
mod tests {
    use super::*;

    #[tokio::test]
    async fn test_validate_nonexistent_directory() {
        let result = validate_closure("/nonexistent", 95.0).await;
        assert!(result.is_err());
    }

    #[tokio::test]
    async fn test_validate_empty_spec_dir() {
        let result = validate_closure("", 95.0).await;
        assert!(result.is_ok()); // Empty spec dir is skipped
    }

    #[tokio::test]
    async fn test_validate_real_spec_dir() {
        let result = validate_closure(".specify", 95.0).await;
        // Result depends on actual .specify contents
        let _ = result;
    }
}
