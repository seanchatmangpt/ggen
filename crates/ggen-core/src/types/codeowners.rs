//! CODEOWNERS generation from noun-level OWNERS files.
//!
//! This module aggregates team ownership from distributed OWNERS files
//! into a single .github/CODEOWNERS file for GitHub PR approval enforcement.

use std::collections::BTreeMap;
use std::fs;
use std::path::{Path, PathBuf};

use thiserror::Error;

/// CODEOWNERS generation errors
#[derive(Debug, Error)]
pub enum CodeownersError {
    #[error("Failed to read OWNERS file: {path}")]
    ReadError {
        path: PathBuf,
        #[source]
        source: std::io::Error,
    },

    #[error("Failed to write CODEOWNERS: {path}")]
    WriteError {
        path: PathBuf,
        #[source]
        source: std::io::Error,
    },

    #[error("Invalid OWNERS format in {path}: {reason}")]
    InvalidFormat { path: PathBuf, reason: String },
}

/// A single ownership entry
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct OwnerEntry {
    /// Path pattern (relative to repo root)
    pub pattern: String,
    /// List of owners (GitHub usernames or team names)
    pub owners: Vec<String>,
    /// Optional comment
    pub comment: Option<String>,
}

impl OwnerEntry {
    pub fn new(pattern: impl Into<String>, owners: Vec<String>) -> Self {
        Self {
            pattern: pattern.into(),
            owners,
            comment: None,
        }
    }

    pub fn with_comment(mut self, comment: impl Into<String>) -> Self {
        self.comment = Some(comment.into());
        self
    }

    /// Format as CODEOWNERS line
    pub fn to_codeowners_line(&self) -> String {
        let owners_str = self.owners.join(" ");
        if let Some(ref comment) = self.comment {
            format!("{} {} # {}", self.pattern, owners_str, comment)
        } else {
            format!("{} {}", self.pattern, owners_str)
        }
    }
}

/// Parsed OWNERS file content
#[derive(Debug, Clone, Default)]
pub struct OwnersFile {
    /// The noun or directory this OWNERS file belongs to
    pub noun: String,
    /// Default owners for all files in this directory
    pub default_owners: Vec<String>,
    /// Pattern-specific overrides
    pub pattern_overrides: Vec<OwnerEntry>,
    /// Original file path
    pub source_path: PathBuf,
}

impl OwnersFile {
    /// Parse an OWNERS file from content
    pub fn parse(content: &str, noun: &str, source_path: PathBuf) -> Result<Self, CodeownersError> {
        let mut default_owners = Vec::new();
        let mut pattern_overrides = Vec::new();

        for line in content.lines() {
            let line = line.trim();

            // Skip empty lines and comments
            if line.is_empty() || line.starts_with('#') {
                continue;
            }

            // Default owner lines start with @
            if line.starts_with('@') {
                // Default owner: "@company/team" or "@username"
                // These may contain / for team names, but start with @
                default_owners.push(line.to_string());
            } else if line.contains('/') || line.contains('*') {
                // Pattern override: "*.breaking.ttl @company/platform-team"
                // Pattern lines do NOT start with @ - the pattern comes first
                let parts: Vec<&str> = line.split_whitespace().collect();
                if parts.len() >= 2 {
                    let pattern = parts[0].to_string();
                    let owners: Vec<String> = parts[1..]
                        .iter()
                        .filter(|s| s.starts_with('@'))
                        .map(|s| s.to_string())
                        .collect();

                    if !owners.is_empty() {
                        pattern_overrides.push(OwnerEntry::new(pattern, owners));
                    }
                }
            }
        }

        Ok(Self {
            noun: noun.to_string(),
            default_owners,
            pattern_overrides,
            source_path,
        })
    }
}

/// CODEOWNERS file generator
#[derive(Debug, Clone)]
pub struct CodeownersGenerator {
    /// Collected OWNERS files
    owners_files: Vec<OwnersFile>,
    /// Base directories to generate entries for
    base_dirs: Vec<String>,
}

impl CodeownersGenerator {
    pub fn new() -> Self {
        Self {
            owners_files: Vec::new(),
            base_dirs: vec![
                "ontology".into(),
                "src/generated".into(),
                "src/domain".into(),
            ],
        }
    }

    /// Add custom base directories
    pub fn with_base_dirs(mut self, dirs: Vec<String>) -> Self {
        self.base_dirs = dirs;
        self
    }

    /// Scan directory for OWNERS files
    pub fn scan_owners_files(&mut self, base_path: &Path) -> Result<(), CodeownersError> {
        self.scan_recursive(base_path, base_path)
    }

    fn scan_recursive(&mut self, current: &Path, base: &Path) -> Result<(), CodeownersError> {
        if !current.is_dir() {
            return Ok(());
        }

        // Check for OWNERS file in current directory
        let owners_path = current.join("OWNERS");
        if owners_path.exists() {
            let content =
                fs::read_to_string(&owners_path).map_err(|e| CodeownersError::ReadError {
                    path: owners_path.clone(),
                    source: e,
                })?;

            let noun = current
                .strip_prefix(base)
                .unwrap_or(current)
                .to_string_lossy()
                .to_string();

            let owners_file = OwnersFile::parse(&content, &noun, owners_path)?;
            self.owners_files.push(owners_file);
        }

        // Recurse into subdirectories
        if let Ok(entries) = fs::read_dir(current) {
            for entry in entries.flatten() {
                let path = entry.path();
                if path.is_dir() {
                    self.scan_recursive(&path, base)?;
                }
            }
        }

        Ok(())
    }

    /// Generate CODEOWNERS content
    pub fn generate(&self) -> String {
        let mut lines = Vec::new();

        // Header
        lines.push("# Auto-generated by ggen from noun OWNERS files".into());
        lines.push("# DO NOT EDIT - regenerate with: ggen generate --codeowners".into());
        lines.push(String::new());

        // Group entries by noun
        let mut entries_by_noun: BTreeMap<&str, Vec<OwnerEntry>> = BTreeMap::new();

        for owners_file in &self.owners_files {
            let mut entries = Vec::new();

            // Generate entries for each base dir + noun combination
            for base_dir in &self.base_dirs {
                let pattern = format!("/{}/{}/", base_dir, owners_file.noun);
                entries.push(
                    OwnerEntry::new(pattern, owners_file.default_owners.clone())
                        .with_comment(format!("{} noun", owners_file.noun)),
                );
            }

            // Add pattern-specific overrides
            for override_entry in &owners_file.pattern_overrides {
                entries.push(override_entry.clone());
            }

            entries_by_noun.insert(&owners_file.noun, entries);
        }

        // Output grouped by noun
        for (noun, entries) in entries_by_noun {
            lines.push(format!("# {} noun", noun));
            for entry in entries {
                lines.push(entry.to_codeowners_line());
            }
            lines.push(String::new());
        }

        lines.join("\n")
    }

    /// Write CODEOWNERS to .github/CODEOWNERS
    pub fn write_to_github(&self, repo_root: &Path) -> Result<PathBuf, CodeownersError> {
        let github_dir = repo_root.join(".github");
        fs::create_dir_all(&github_dir).map_err(|e| CodeownersError::WriteError {
            path: github_dir.clone(),
            source: e,
        })?;

        let codeowners_path = github_dir.join("CODEOWNERS");
        let content = self.generate();

        fs::write(&codeowners_path, content).map_err(|e| CodeownersError::WriteError {
            path: codeowners_path.clone(),
            source: e,
        })?;

        Ok(codeowners_path)
    }
}

impl Default for CodeownersGenerator {
    fn default() -> Self {
        Self::new()
    }
}

/// Convenience function to generate CODEOWNERS from an ontology directory
pub fn generate_codeowners(
    ontology_dir: &Path, repo_root: &Path,
) -> Result<PathBuf, CodeownersError> {
    let mut generator = CodeownersGenerator::new();
    generator.scan_owners_files(ontology_dir)?;
    generator.write_to_github(repo_root)
}

#[cfg(test)]
mod tests {
    use super::*;
    use tempfile::TempDir;

    #[test]
    fn test_parse_owners_file() {
        let content = r#"
# Identity Team owns this noun
@company/identity-team
@john.smith

# Breaking changes require Platform Team
*.breaking.ttl @company/platform-team
"#;

        let owners =
            OwnersFile::parse(content, "user", PathBuf::from("ontology/user/OWNERS")).unwrap();

        assert_eq!(owners.noun, "user");
        assert_eq!(owners.default_owners.len(), 2);
        assert!(owners
            .default_owners
            .contains(&"@company/identity-team".to_string()));
        assert!(owners.default_owners.contains(&"@john.smith".to_string()));
        assert_eq!(owners.pattern_overrides.len(), 1);
    }

    #[test]
    fn test_owner_entry_format() {
        let entry = OwnerEntry::new("/ontology/user/", vec!["@team".into()]);
        assert_eq!(entry.to_codeowners_line(), "/ontology/user/ @team");

        let entry_with_comment = entry.with_comment("User noun");
        assert_eq!(
            entry_with_comment.to_codeowners_line(),
            "/ontology/user/ @team # User noun"
        );
    }

    #[test]
    fn test_generate_codeowners() {
        let temp_dir = TempDir::new().unwrap();
        let ontology_dir = temp_dir.path().join("ontology");
        let user_dir = ontology_dir.join("user");
        fs::create_dir_all(&user_dir).unwrap();

        fs::write(user_dir.join("OWNERS"), "@company/identity-team\n@john.doe").unwrap();

        let mut generator = CodeownersGenerator::new();
        generator.scan_owners_files(&ontology_dir).unwrap();

        let content = generator.generate();
        assert!(content.contains("Auto-generated by ggen"));
        assert!(content.contains("@company/identity-team"));
    }

    #[test]
    fn test_write_codeowners() {
        let temp_dir = TempDir::new().unwrap();
        let ontology_dir = temp_dir.path().join("ontology");
        let user_dir = ontology_dir.join("user");
        fs::create_dir_all(&user_dir).unwrap();

        fs::write(user_dir.join("OWNERS"), "@team").unwrap();

        let result = generate_codeowners(&ontology_dir, temp_dir.path());
        assert!(result.is_ok());

        let codeowners_path = temp_dir.path().join(".github/CODEOWNERS");
        assert!(codeowners_path.exists());
    }
}
