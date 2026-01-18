//! Path protection validation for generation operations
//!
//! Implements `[generation].protected_paths` and `[generation].regenerate_paths`
//! behavior from ggen.toml to prevent accidental overwrites of domain logic.

use super::headers::GenerationSafetyConfig;
use ggen_core::types::PathProtectionConfig;
use ggen_utils::error::Result;
use std::path::Path;

/// Result of path protection validation
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum GenerationWriteResult {
    /// Path is safe to write (new file or in regenerate_paths)
    AllowWrite,
    /// Path is protected, cannot be overwritten
    BlockedProtected { path: String, pattern: String },
    /// Path exists but not in regenerate_paths (implicit protection)
    BlockedImplicit { path: String },
    /// Path is in regenerate_paths (safe to overwrite)
    AllowRegenerate { path: String, pattern: String },
}

/// Validator for path protection during generation
pub struct PathProtectionValidator {
    config: PathProtectionConfig,
}

impl PathProtectionValidator {
    /// Create a new validator from GenerationSafetyConfig
    pub fn from_config(config: &GenerationSafetyConfig) -> Result<Self> {
        let protected: Vec<&str> = config.protected_paths.iter().map(|s| s.as_str()).collect();
        let regenerate: Vec<&str> = config.regenerate_paths.iter().map(|s| s.as_str()).collect();

        let path_config = PathProtectionConfig::new(&protected, &regenerate).map_err(|e| {
            ggen_utils::error::Error::new(&format!("Invalid path protection config: {}", e))
        })?;

        Ok(Self {
            config: path_config,
        })
    }

    /// Create a validator with default protection patterns
    pub fn default_protection() -> Self {
        Self {
            config: PathProtectionConfig::default(),
        }
    }

    /// Validate if a write operation is allowed for the given path
    pub fn validate_write(&self, path: &str, file_exists: bool) -> GenerationWriteResult {
        // Check if path matches protected patterns
        if let Some(pattern) = self.config.protected_pattern_for(path) {
            return GenerationWriteResult::BlockedProtected {
                path: path.to_string(),
                pattern: pattern.to_string(),
            };
        }

        // Check if path matches regenerate patterns
        if let Some(pattern) = self.config.regenerate_pattern_for(path) {
            return GenerationWriteResult::AllowRegenerate {
                path: path.to_string(),
                pattern: pattern.to_string(),
            };
        }

        // If file exists but not in regenerate_paths, block (implicit protection)
        if file_exists {
            return GenerationWriteResult::BlockedImplicit {
                path: path.to_string(),
            };
        }

        // New file not in any protection list - allow
        GenerationWriteResult::AllowWrite
    }

    /// Check if a path is protected (convenience method)
    pub fn is_protected(&self, path: &str) -> bool {
        self.config.is_protected(path)
    }

    /// Check if a path is regeneratable (convenience method)
    pub fn is_regeneratable(&self, path: &str) -> bool {
        self.config.is_regeneratable(path)
    }
}

/// Validate a generation write operation against config
///
/// This is the main entry point for path protection validation.
/// Returns Ok(()) if write is allowed, Err if blocked.
pub fn validate_generation_write(
    config: &GenerationSafetyConfig, output_path: &Path, base_dir: &Path,
) -> Result<()> {
    // Skip validation if disabled
    if !config.enabled {
        return Ok(());
    }

    let validator = PathProtectionValidator::from_config(config)?;

    // Get relative path for pattern matching
    let relative_path = output_path
        .strip_prefix(base_dir)
        .unwrap_or(output_path)
        .to_string_lossy();

    let file_exists = output_path.exists();

    match validator.validate_write(&relative_path, file_exists) {
        GenerationWriteResult::AllowWrite => Ok(()),
        GenerationWriteResult::AllowRegenerate { .. } => Ok(()),
        GenerationWriteResult::BlockedProtected { path, pattern } => {
            Err(ggen_utils::error::Error::new(&format!(
                "Cannot write to protected path '{}' (matches pattern '{}'). \
                 Protected paths cannot be overwritten by generation. \
                 If this is intentional, add the path to [generation].regenerate_paths in ggen.toml.",
                path, pattern
            )))
        }
        GenerationWriteResult::BlockedImplicit { path } => {
            Err(ggen_utils::error::Error::new(&format!(
                "Cannot overwrite existing file '{}' (not in regenerate_paths). \
                 Files not in [generation].regenerate_paths are implicitly protected. \
                 To allow overwriting, add the pattern to regenerate_paths in ggen.toml, \
                 or use --force to override.",
                path
            )))
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    fn test_config() -> GenerationSafetyConfig {
        GenerationSafetyConfig {
            enabled: true,
            protected_paths: vec!["src/domain/**".to_string(), "Cargo.toml".to_string()],
            regenerate_paths: vec!["src/generated/**".to_string()],
            generated_header: Some("// DO NOT EDIT".to_string()),
            require_confirmation: false,
            backup_before_write: true,
            poka_yoke: None,
        }
    }

    #[test]
    fn test_protected_path_blocked() {
        let validator = PathProtectionValidator::from_config(&test_config()).unwrap();

        let result = validator.validate_write("src/domain/user.rs", false);
        match result {
            GenerationWriteResult::BlockedProtected { path, .. } => {
                assert_eq!(path, "src/domain/user.rs");
            }
            _ => panic!("Expected BlockedProtected, got {:?}", result),
        }
    }

    #[test]
    fn test_regeneratable_path_allowed() {
        let validator = PathProtectionValidator::from_config(&test_config()).unwrap();

        let result = validator.validate_write("src/generated/types.rs", true);
        match result {
            GenerationWriteResult::AllowRegenerate { path, .. } => {
                assert_eq!(path, "src/generated/types.rs");
            }
            _ => panic!("Expected AllowRegenerate, got {:?}", result),
        }
    }

    #[test]
    fn test_new_file_allowed() {
        let validator = PathProtectionValidator::from_config(&test_config()).unwrap();

        let result = validator.validate_write("src/utils/helper.rs", false);
        assert_eq!(result, GenerationWriteResult::AllowWrite);
    }

    #[test]
    fn test_existing_untracked_blocked() {
        let validator = PathProtectionValidator::from_config(&test_config()).unwrap();

        let result = validator.validate_write("src/utils/helper.rs", true);
        match result {
            GenerationWriteResult::BlockedImplicit { path } => {
                assert_eq!(path, "src/utils/helper.rs");
            }
            _ => panic!("Expected BlockedImplicit, got {:?}", result),
        }
    }

    #[test]
    fn test_disabled_validation_allows_all() {
        let mut config = test_config();
        config.enabled = false;

        let base_dir = std::path::Path::new("/tmp");
        let output = base_dir.join("src/domain/user.rs");

        let result = validate_generation_write(&config, &output, base_dir);
        assert!(result.is_ok());
    }
}
