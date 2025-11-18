//! Pack validation logic

use crate::packs::metadata::load_pack_metadata;
use ggen_utils::error::Result;
use serde::{Deserialize, Serialize};

/// Validation result
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct ValidationResult {
    pub pack_id: String,
    pub valid: bool,
    pub score: f64,
    pub errors: Vec<String>,
    pub warnings: Vec<String>,
    pub checks: Vec<ValidationCheck>,
}

/// Individual validation check
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct ValidationCheck {
    pub name: String,
    pub passed: bool,
    pub message: String,
}

/// Validate a pack
pub fn validate_pack(pack_id: &str) -> Result<ValidationResult> {
    let pack = load_pack_metadata(pack_id)?;

    let mut errors = Vec::new();
    let mut warnings = Vec::new();
    let mut checks = Vec::new();

    // Check: Pack has name
    let has_name = !pack.name.is_empty();
    checks.push(ValidationCheck {
        name: "has_name".to_string(),
        passed: has_name,
        message: if has_name {
            "Pack has a name".to_string()
        } else {
            errors.push("Pack name is empty".to_string());
            "Pack name is empty".to_string()
        },
    });

    // Check: Pack has description
    let has_description = !pack.description.is_empty();
    checks.push(ValidationCheck {
        name: "has_description".to_string(),
        passed: has_description,
        message: if has_description {
            "Pack has a description".to_string()
        } else {
            errors.push("Pack description is empty".to_string());
            "Pack description is empty".to_string()
        },
    });

    // Check: Pack has at least one package
    let has_packages = !pack.packages.is_empty();
    checks.push(ValidationCheck {
        name: "has_packages".to_string(),
        passed: has_packages,
        message: if has_packages {
            format!("Pack has {} package(s)", pack.packages.len())
        } else {
            warnings.push("Pack has no packages".to_string());
            "Pack has no packages".to_string()
        },
    });

    // Check: Pack version is valid
    let has_valid_version = is_valid_semver(&pack.version);
    checks.push(ValidationCheck {
        name: "valid_version".to_string(),
        passed: has_valid_version,
        message: if has_valid_version {
            format!("Valid version: {}", pack.version)
        } else {
            warnings.push(format!("Invalid semver version: {}", pack.version));
            format!("Invalid semver version: {}", pack.version)
        },
    });

    // Check: Pack has templates or packages
    let has_content = !pack.templates.is_empty() || !pack.packages.is_empty();
    checks.push(ValidationCheck {
        name: "has_content".to_string(),
        passed: has_content,
        message: if has_content {
            format!(
                "Pack has {} template(s) and {} package(s)",
                pack.templates.len(),
                pack.packages.len()
            )
        } else {
            errors.push("Pack has no templates or packages".to_string());
            "Pack has no templates or packages".to_string()
        },
    });

    // Calculate validation score
    let passed_checks = checks.iter().filter(|c| c.passed).count();
    let total_checks = checks.len();
    let score = if total_checks > 0 {
        (passed_checks as f64 / total_checks as f64) * 100.0
    } else {
        0.0
    };

    let valid = errors.is_empty();

    Ok(ValidationResult {
        pack_id: pack_id.to_string(),
        valid,
        score,
        errors,
        warnings,
        checks,
    })
}

/// Check if version string is valid semver
fn is_valid_semver(version: &str) -> bool {
    // Simple semver check: X.Y.Z format
    let parts: Vec<&str> = version.split('.').collect();
    if parts.len() != 3 {
        return false;
    }

    parts.iter().all(|p| p.parse::<u32>().is_ok())
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_is_valid_semver() {
        assert!(is_valid_semver("1.0.0"));
        assert!(is_valid_semver("2.5.10"));
        assert!(!is_valid_semver("1.0"));
        assert!(!is_valid_semver("1.0.0.0"));
        assert!(!is_valid_semver("abc"));
    }

    #[test]
    fn test_validate_pack_handles_missing_pack() {
        let result = validate_pack("nonexistent-pack");
        assert!(result.is_err());
    }
}
