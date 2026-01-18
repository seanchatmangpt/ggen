//! Path protection validation for domain vs generated code boundaries.
//!
//! This module provides glob-based path matching to enforce the trait boundary
//! separation pattern where:
//! - `protected_paths` (e.g., src/domain/**) MUST NEVER be overwritten
//! - `regenerate_paths` (e.g., src/generated/**) CAN be freely regenerated

use glob::Pattern;
use std::path::Path;
use thiserror::Error;

/// Path protection errors
#[derive(Debug, Error, Clone, PartialEq, Eq)]
pub enum PathProtectionError {
    #[error("Cannot write to protected path: {path}")]
    ProtectedPathViolation { path: String, pattern: String },

    #[error("Cannot write to existing file not in regenerate_paths: {path}")]
    ImplicitProtectionViolation { path: String },

    #[error("Path matches both protected and regenerate patterns: {path}")]
    PathOverlapError {
        path: String,
        protected_pattern: String,
        regenerate_pattern: String,
    },

    #[error("Invalid glob pattern: {pattern} - {reason}")]
    InvalidPattern { pattern: String, reason: String },
}

/// Compiled path protection configuration
#[derive(Debug, Clone)]
pub struct PathProtectionConfig {
    protected_patterns: Vec<CompiledPattern>,
    regenerate_patterns: Vec<CompiledPattern>,
}

#[derive(Debug, Clone)]
struct CompiledPattern {
    original: String,
    pattern: Pattern,
}

impl CompiledPattern {
    fn new(pattern: &str) -> Result<Self, PathProtectionError> {
        let glob_pattern =
            Pattern::new(pattern).map_err(|e| PathProtectionError::InvalidPattern {
                pattern: pattern.to_string(),
                reason: e.to_string(),
            })?;

        Ok(Self {
            original: pattern.to_string(),
            pattern: glob_pattern,
        })
    }

    fn matches(&self, path: &str) -> bool {
        self.pattern.matches(path)
    }
}

impl PathProtectionConfig {
    /// Create a new PathProtectionConfig from string patterns
    pub fn new(
        protected_paths: &[&str], regenerate_paths: &[&str],
    ) -> Result<Self, PathProtectionError> {
        let protected_patterns: Result<Vec<_>, _> = protected_paths
            .iter()
            .map(|p| CompiledPattern::new(p))
            .collect();

        let regenerate_patterns: Result<Vec<_>, _> = regenerate_paths
            .iter()
            .map(|p| CompiledPattern::new(p))
            .collect();

        let config = Self {
            protected_patterns: protected_patterns?,
            regenerate_patterns: regenerate_patterns?,
        };

        // Validate no overlaps on common test paths
        config.validate_no_overlaps()?;

        Ok(config)
    }

    /// Validate that no path can match both protected and regenerate patterns
    fn validate_no_overlaps(&self) -> Result<(), PathProtectionError> {
        let test_paths = [
            "src/domain/user.rs",
            "src/generated/user.rs",
            "src/main.rs",
            "Cargo.toml",
        ];

        for path in test_paths {
            let protected_match = self.protected_patterns.iter().find(|p| p.matches(path));
            let regenerate_match = self.regenerate_patterns.iter().find(|p| p.matches(path));

            if let (Some(prot), Some(regen)) = (protected_match, regenerate_match) {
                return Err(PathProtectionError::PathOverlapError {
                    path: path.to_string(),
                    protected_pattern: prot.original.clone(),
                    regenerate_pattern: regen.original.clone(),
                });
            }
        }

        Ok(())
    }

    /// Check if a path is protected (should never be modified)
    pub fn is_protected(&self, path: &str) -> bool {
        self.protected_patterns.iter().any(|p| p.matches(path))
    }

    /// Check if a path is regeneratable (can be safely overwritten)
    pub fn is_regeneratable(&self, path: &str) -> bool {
        self.regenerate_patterns.iter().any(|p| p.matches(path))
    }

    /// Validate that writing to a path is allowed
    pub fn validate_write(&self, path: &str, file_exists: bool) -> Result<(), PathProtectionError> {
        // Check protected first
        if let Some(pattern) = self.protected_patterns.iter().find(|p| p.matches(path)) {
            return Err(PathProtectionError::ProtectedPathViolation {
                path: path.to_string(),
                pattern: pattern.original.clone(),
            });
        }

        // Check if explicitly regeneratable
        if self.is_regeneratable(path) {
            return Ok(());
        }

        // Implicit protection: if file exists and not in regenerate_paths
        if file_exists {
            return Err(PathProtectionError::ImplicitProtectionViolation {
                path: path.to_string(),
            });
        }

        // New file, not in any protection list - allow
        Ok(())
    }

    /// Get the matching protected pattern for a path, if any
    pub fn protected_pattern_for(&self, path: &str) -> Option<&str> {
        self.protected_patterns
            .iter()
            .find(|p| p.matches(path))
            .map(|p| p.original.as_str())
    }

    /// Get the matching regenerate pattern for a path, if any
    pub fn regenerate_pattern_for(&self, path: &str) -> Option<&str> {
        self.regenerate_patterns
            .iter()
            .find(|p| p.matches(path))
            .map(|p| p.original.as_str())
    }
}

impl Default for PathProtectionConfig {
    fn default() -> Self {
        Self::new(
            &["src/domain/**", "src/main.rs", "Cargo.toml"],
            &["src/generated/**"],
        )
        .expect("Default patterns should be valid")
    }
}

/// Helper to check path protection from filesystem Path
pub fn validate_write_path(
    config: &PathProtectionConfig, path: &Path, base_dir: &Path,
) -> Result<(), PathProtectionError> {
    let relative = path
        .strip_prefix(base_dir)
        .unwrap_or(path)
        .to_string_lossy();

    let file_exists = path.exists();
    config.validate_write(&relative, file_exists)
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_path_protection_default() {
        let config = PathProtectionConfig::default();

        assert!(config.is_protected("src/domain/user.rs"));
        assert!(config.is_protected("src/main.rs"));
        assert!(!config.is_protected("src/generated/user.rs"));

        assert!(config.is_regeneratable("src/generated/user.rs"));
        assert!(!config.is_regeneratable("src/domain/user.rs"));
    }

    #[test]
    fn test_validate_write_protected_fails() {
        let config = PathProtectionConfig::default();

        let result = config.validate_write("src/domain/user.rs", false);
        assert!(result.is_err());

        match result.unwrap_err() {
            PathProtectionError::ProtectedPathViolation { path, .. } => {
                assert_eq!(path, "src/domain/user.rs");
            }
            _ => panic!("Expected ProtectedPathViolation"),
        }
    }

    #[test]
    fn test_validate_write_regeneratable_passes() {
        let config = PathProtectionConfig::default();

        let result = config.validate_write("src/generated/user.rs", true);
        assert!(result.is_ok());
    }

    #[test]
    fn test_validate_write_new_file_passes() {
        let config = PathProtectionConfig::default();

        // New file not in any list - allowed
        let result = config.validate_write("src/utils/helper.rs", false);
        assert!(result.is_ok());
    }

    #[test]
    fn test_validate_write_existing_untracked_fails() {
        let config = PathProtectionConfig::default();

        // Existing file not in regenerate_paths - implicit protection
        let result = config.validate_write("src/utils/helper.rs", true);
        assert!(result.is_err());

        match result.unwrap_err() {
            PathProtectionError::ImplicitProtectionViolation { path } => {
                assert_eq!(path, "src/utils/helper.rs");
            }
            _ => panic!("Expected ImplicitProtectionViolation"),
        }
    }

    #[test]
    fn test_invalid_pattern() {
        let result = PathProtectionConfig::new(
            &["[invalid"], // Invalid glob pattern
            &["src/generated/**"],
        );

        assert!(result.is_err());
        match result.unwrap_err() {
            PathProtectionError::InvalidPattern { pattern, .. } => {
                assert_eq!(pattern, "[invalid");
            }
            _ => panic!("Expected InvalidPattern"),
        }
    }

    #[test]
    fn test_pattern_lookup() {
        let config = PathProtectionConfig::default();

        assert_eq!(
            config.protected_pattern_for("src/domain/user.rs"),
            Some("src/domain/**")
        );

        assert_eq!(
            config.regenerate_pattern_for("src/generated/user.rs"),
            Some("src/generated/**")
        );

        assert_eq!(config.protected_pattern_for("src/generated/user.rs"), None);
    }
}
