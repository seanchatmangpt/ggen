//! CODEOWNERS generation from noun-level OWNERS files
//!
//! This module wraps ggen-core's CodeownersGenerator to provide domain-level
//! CODEOWNERS generation that integrates with ggen.toml configuration.
//!
//! Implements `[codeowners]` behavior from ggen.toml:
//! - `enabled` - Enable/disable CODEOWNERS generation
//! - `source_dirs` - Directories to scan for OWNERS files
//! - `base_dirs` - Base directories for pattern generation
//! - `output_path` - Custom output path (default: .github/CODEOWNERS)
//! - `auto_regenerate` - Regenerate on noun changes

use ggen_core::types::codeowners::CodeownersGenerator;
use ggen_utils::error::Result;
use serde::{Deserialize, Serialize};
use std::path::{Path, PathBuf};

/// CODEOWNERS generation configuration
///
/// Local definition to avoid cyclic dependency with ggen-config.
/// Mirrors [codeowners] from ggen.toml.
#[derive(Debug, Clone, Default, Serialize, Deserialize)]
pub struct CodeownersConfig {
    /// Enable CODEOWNERS generation
    #[serde(default)]
    pub enabled: bool,
    /// Source directories to scan for OWNERS files
    #[serde(default)]
    pub source_dirs: Vec<String>,
    /// Base directories to generate entries for
    #[serde(default)]
    pub base_dirs: Vec<String>,
    /// Output path (defaults to .github/CODEOWNERS)
    pub output_path: Option<String>,
    /// Auto-regenerate on noun changes
    #[serde(default)]
    pub auto_regenerate: bool,
}

/// Result of CODEOWNERS generation
#[derive(Debug, Clone)]
pub struct CodeownersResult {
    /// Path to generated CODEOWNERS file
    pub output_path: PathBuf,
    /// Number of OWNERS files processed
    pub owners_files_scanned: usize,
    /// Number of entries generated
    pub entries_generated: usize,
}

/// Generate CODEOWNERS from configuration
///
/// This function reads the CODEOWNERS configuration from ggen.toml and generates
/// a .github/CODEOWNERS file from all OWNERS files found in the configured source directories.
pub fn generate_codeowners(
    config: &CodeownersConfig, project_root: &Path,
) -> Result<CodeownersResult> {
    if !config.enabled {
        return Err(ggen_utils::error::Error::new(
            "CODEOWNERS generation is disabled in ggen.toml. Set [codeowners].enabled = true",
        ));
    }

    // Determine source directories
    let source_dirs = if config.source_dirs.is_empty() {
        vec!["ontology".to_string()] // Default to ontology directory
    } else {
        config.source_dirs.clone()
    };

    // Create generator with configured base dirs
    let mut generator = if config.base_dirs.is_empty() {
        CodeownersGenerator::new()
    } else {
        CodeownersGenerator::new().with_base_dirs(config.base_dirs.clone())
    };

    // Scan all source directories for OWNERS files
    for source_dir in &source_dirs {
        let dir_path = project_root.join(source_dir);
        if dir_path.exists() {
            generator.scan_owners_files(&dir_path).map_err(|e| {
                ggen_utils::error::Error::new(&format!(
                    "Failed to scan OWNERS files in {}: {}",
                    source_dir, e
                ))
            })?;
        }
    }

    // Generate and write CODEOWNERS
    let (output_path, entries_generated) = if let Some(ref custom_path) = config.output_path {
        let path = project_root.join(custom_path);
        // Ensure parent directory exists
        if let Some(parent) = path.parent() {
            std::fs::create_dir_all(parent).map_err(|e| {
                ggen_utils::error::Error::new(&format!("Failed to create output directory: {}", e))
            })?;
        }
        let content = generator.generate();
        let entries = content
            .lines()
            .filter(|l| !l.is_empty() && !l.starts_with('#'))
            .count();

        std::fs::write(&path, &content).map_err(|e| {
            ggen_utils::error::Error::new(&format!("Failed to write CODEOWNERS: {}", e))
        })?;
        (path, entries)
    } else {
        let path = generator.write_to_github(project_root).map_err(|e| {
            ggen_utils::error::Error::new(&format!("Failed to write CODEOWNERS: {}", e))
        })?;

        // Count entries from generated content
        let content = std::fs::read_to_string(&path).unwrap_or_default();
        let entries = content
            .lines()
            .filter(|l| !l.is_empty() && !l.starts_with('#'))
            .count();
        (path, entries)
    };

    Ok(CodeownersResult {
        output_path,
        owners_files_scanned: source_dirs.len(),
        entries_generated,
    })
}

/// Check if CODEOWNERS should be auto-regenerated
///
/// Returns true if auto_regenerate is enabled in config and there are
/// changes to noun/OWNERS files.
pub fn should_regenerate(config: &CodeownersConfig) -> bool {
    config.enabled && config.auto_regenerate
}

/// Generate CODEOWNERS with default configuration
///
/// Convenience function for simple use cases without ggen.toml.
pub fn generate_codeowners_default(
    ontology_dir: &Path, project_root: &Path,
) -> Result<CodeownersResult> {
    let config = CodeownersConfig {
        enabled: true,
        source_dirs: vec![ontology_dir
            .strip_prefix(project_root)
            .unwrap_or(ontology_dir)
            .to_string_lossy()
            .to_string()],
        base_dirs: vec![],
        output_path: None,
        auto_regenerate: false,
    };

    generate_codeowners(&config, project_root)
}

#[cfg(test)]
mod tests {
    use super::*;
    use std::fs;
    use tempfile::TempDir;

    fn test_config() -> CodeownersConfig {
        CodeownersConfig {
            enabled: true,
            source_dirs: vec!["ontology".to_string()],
            base_dirs: vec!["ontology".to_string(), "src/generated".to_string()],
            output_path: None,
            auto_regenerate: true,
        }
    }

    #[test]
    fn test_generate_codeowners_disabled() {
        let temp_dir = TempDir::new().unwrap();
        let config = CodeownersConfig {
            enabled: false,
            ..Default::default()
        };

        let result = generate_codeowners(&config, temp_dir.path());
        assert!(result.is_err());
        assert!(result.unwrap_err().to_string().contains("disabled"));
    }

    #[test]
    fn test_generate_codeowners_empty_project() {
        let temp_dir = TempDir::new().unwrap();
        let ontology_dir = temp_dir.path().join("ontology");
        fs::create_dir_all(&ontology_dir).unwrap();

        let config = test_config();
        let result = generate_codeowners(&config, temp_dir.path());

        // Should succeed even with no OWNERS files (generates empty CODEOWNERS)
        assert!(result.is_ok());
        let codeowners_path = temp_dir.path().join(".github/CODEOWNERS");
        assert!(codeowners_path.exists());
    }

    #[test]
    fn test_generate_codeowners_with_owners() {
        let temp_dir = TempDir::new().unwrap();
        let user_dir = temp_dir.path().join("ontology/user");
        fs::create_dir_all(&user_dir).unwrap();

        // Create OWNERS file
        fs::write(user_dir.join("OWNERS"), "@company/identity-team\n@john.doe").unwrap();

        let config = test_config();
        let result = generate_codeowners(&config, temp_dir.path()).unwrap();

        assert!(result.output_path.exists());
        assert!(result.entries_generated > 0);

        // Check content
        let content = fs::read_to_string(&result.output_path).unwrap();
        assert!(content.contains("Auto-generated by ggen"));
        assert!(content.contains("@company/identity-team"));
    }

    #[test]
    fn test_generate_codeowners_custom_output() {
        let temp_dir = TempDir::new().unwrap();
        let ontology_dir = temp_dir.path().join("ontology");
        fs::create_dir_all(&ontology_dir).unwrap();

        let config = CodeownersConfig {
            enabled: true,
            source_dirs: vec!["ontology".to_string()],
            base_dirs: vec![],
            output_path: Some("docs/CODEOWNERS".to_string()),
            auto_regenerate: false,
        };

        let result = generate_codeowners(&config, temp_dir.path()).unwrap();

        assert_eq!(result.output_path, temp_dir.path().join("docs/CODEOWNERS"));
        assert!(result.output_path.exists());
    }

    #[test]
    fn test_should_regenerate() {
        let enabled_config = CodeownersConfig {
            enabled: true,
            auto_regenerate: true,
            ..Default::default()
        };
        assert!(should_regenerate(&enabled_config));

        let disabled_config = CodeownersConfig {
            enabled: true,
            auto_regenerate: false,
            ..Default::default()
        };
        assert!(!should_regenerate(&disabled_config));

        let disabled_all = CodeownersConfig {
            enabled: false,
            auto_regenerate: true,
            ..Default::default()
        };
        assert!(!should_regenerate(&disabled_all));
    }

    #[test]
    fn test_generate_codeowners_default() {
        let temp_dir = TempDir::new().unwrap();
        let ontology_dir = temp_dir.path().join("ontology");
        let user_dir = ontology_dir.join("user");
        fs::create_dir_all(&user_dir).unwrap();

        fs::write(user_dir.join("OWNERS"), "@team").unwrap();

        let result = generate_codeowners_default(&ontology_dir, temp_dir.path()).unwrap();

        assert!(result.output_path.exists());
        let content = fs::read_to_string(&result.output_path).unwrap();
        assert!(content.contains("@team"));
    }
}
