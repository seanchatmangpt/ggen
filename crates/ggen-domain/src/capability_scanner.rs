//! Inferred Capability Scanner
//!
//! Detects capabilities in generated code that lack ontology-backed declarations.
//! Required for CISO `ForbidInferredCapabilities` policy enforcement.

use std::collections::HashSet;
use std::path::Path;

use ggen_utils::error::{Error, Result};

/// Scan result for a single file
#[derive(Debug, Clone)]
pub struct InferredCapability {
    /// The capability name/identifier found
    pub capability: String,
    /// File where it was found
    pub file: String,
    /// Line number (1-based)
    pub line: usize,
    /// Context around the detection
    pub context: String,
}

/// Scans generated code for capabilities without ontology backing.
pub struct CapabilityScanner {
    /// Known ontology-backed capability identifiers
    ontology_declarations: HashSet<String>,
}

impl CapabilityScanner {
    /// Create a new scanner with known ontology declarations
    #[must_use]
    pub fn new(ontology_declarations: HashSet<String>) -> Self {
        Self {
            ontology_declarations,
        }
    }

    /// Scan generated files for inferred (undeclared) capabilities
    ///
    /// Returns a list of capabilities found that are NOT in the ontology declarations.
    pub fn scan(&self, generated_dir: &Path) -> Result<Vec<InferredCapability>> {
        let mut findings = Vec::new();
        self.scan_directory(generated_dir, &mut findings)?;
        Ok(findings)
    }

    /// Check if any inferred capabilities were found
    pub fn has_inferred_capabilities(&self, generated_dir: &Path) -> Result<bool> {
        Ok(!self.scan(generated_dir)?.is_empty())
    }

    fn scan_directory(&self, dir: &Path, findings: &mut Vec<InferredCapability>) -> Result<()> {
        if !dir.exists() {
            return Ok(());
        }

        let entries = std::fs::read_dir(dir)
            .map_err(|e| Error::new(&format!("Cannot read directory {}: {}", dir.display(), e)))?;

        for entry in entries {
            let entry =
                entry.map_err(|e| Error::new(&format!("Cannot read directory entry: {}", e)))?;
            let path = entry.path();

            if path.is_dir() {
                self.scan_directory(&path, findings)?;
            } else if path.extension().and_then(|e| e.to_str()) == Some("rs") {
                self.scan_rust_file(&path, findings)?;
            }
        }
        Ok(())
    }

    fn scan_rust_file(&self, path: &Path, findings: &mut Vec<InferredCapability>) -> Result<()> {
        let content = std::fs::read_to_string(path)
            .map_err(|e| Error::new(&format!("Cannot read {}: {}", path.display(), e)))?;

        // Pattern: detect capability-like identifiers
        // Look for struct/function names that could be capabilities
        let capability_patterns = [
            "Capability",
            "capability",
            "Surface",
            "Projection",
            "Runtime",
            "Handler",
            "Provider",
            "Adapter",
        ];

        for (line_num, line) in content.lines().enumerate() {
            // Skip comments and doc comments
            let trimmed = line.trim();
            if trimmed.starts_with("//") || trimmed.starts_with("/*") || trimmed.starts_with('*') {
                continue;
            }

            for pattern in &capability_patterns {
                if line.contains(pattern) {
                    // Extract the identifier
                    if let Some(ident) = self.extract_capability_identifier(line, pattern) {
                        // Check if this capability is ontology-backed
                        let normalized = ident.to_lowercase();
                        if !self
                            .ontology_declarations
                            .iter()
                            .any(|d| d.to_lowercase().contains(&normalized))
                        {
                            findings.push(InferredCapability {
                                capability: ident,
                                file: path.display().to_string(),
                                line: line_num + 1,
                                context: trimmed.to_string(),
                            });
                        }
                    }
                }
            }
        }
        Ok(())
    }

    fn extract_capability_identifier(&self, line: &str, pattern: &str) -> Option<String> {
        // Simple extraction: find the word containing the pattern
        let idx = line.find(pattern)?;
        // Find word boundaries
        let start = line[..idx]
            .rfind(|c: char| !c.is_alphanumeric() && c != '_')
            .map(|i| i + 1)
            .unwrap_or(0);
        let end = line[idx..]
            .find(|c: char| !c.is_alphanumeric() && c != '_')
            .map(|i| idx + i)
            .unwrap_or(line.len());
        let ident = line[start..end].trim().to_string();
        if ident.len() > 2 && ident.contains(pattern) {
            Some(ident)
        } else {
            None
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use std::fs;

    #[test]
    fn scanner_finds_inferred_capabilities_in_real_files() {
        // Arrange: create a real temp directory with .rs files
        let temp_dir = tempfile::TempDir::new().expect("Failed to create temp dir");
        let src_file = temp_dir.path().join("generated.rs");
        fs::write(
            &src_file,
            r#"// generated code
pub struct AuthHandler { }
pub struct PaymentProvider { }
pub fn custom_adapter() { }
"#,
        )
        .expect("Failed to write test file");

        // Only "AuthHandler" is ontology-backed
        let mut ontology = HashSet::new();
        ontology.insert("AuthHandler".to_string());

        let scanner = CapabilityScanner::new(ontology);

        // Act
        let findings = scanner.scan(temp_dir.path()).expect("Scan failed");

        // Assert: PaymentProvider and custom_adapter should be flagged, not AuthHandler
        assert_eq!(findings.len(), 2);
        let caps: Vec<&str> = findings.iter().map(|f| f.capability.as_str()).collect();
        assert!(caps.contains(&"PaymentProvider"));
        assert!(caps.contains(&"custom_adapter"));
        assert!(!caps.contains(&"AuthHandler"));
    }

    #[test]
    fn scanner_skips_comments_and_doc_comments() {
        let temp_dir = tempfile::TempDir::new().expect("Failed to create temp dir");
        let src_file = temp_dir.path().join("lib.rs");
        fs::write(
            &src_file,
            r#"// This is a comment about someHandler
/// Doc comment mentioning a Provider
/* Block comment with Adapter */
pub struct RealCode { }
"#,
        )
        .expect("Failed to write test file");

        let scanner = CapabilityScanner::new(HashSet::new());
        let findings = scanner.scan(temp_dir.path()).expect("Scan failed");

        // RealCode does not match any pattern, so no findings expected
        assert!(findings.is_empty());
    }

    #[test]
    fn scanner_returns_empty_for_nonexistent_directory() {
        let scanner = CapabilityScanner::new(HashSet::new());
        let result = scanner.scan(Path::new("/nonexistent/path/to/generated"));
        assert!(result.is_ok());
        assert!(result.unwrap().is_empty());
    }

    #[test]
    fn scanner_non_rs_files_are_ignored() {
        let temp_dir = tempfile::TempDir::new().expect("Failed to create temp dir");
        fs::write(temp_dir.path().join("config.toml"), "Handler = true")
            .expect("Failed to write toml");
        fs::write(temp_dir.path().join("readme.md"), "# Handler docs").expect("Failed to write md");

        let scanner = CapabilityScanner::new(HashSet::new());
        let findings = scanner.scan(temp_dir.path()).expect("Scan failed");
        assert!(findings.is_empty());
    }

    #[test]
    fn has_inferred_capabilities_returns_false_when_clean() {
        let temp_dir = tempfile::TempDir::new().expect("Failed to create temp dir");
        fs::write(temp_dir.path().join("lib.rs"), "pub struct Foo { }").expect("Failed to write");

        let mut ontology = HashSet::new();
        ontology.insert("DataHandler".to_string());

        let scanner = CapabilityScanner::new(ontology);
        assert!(!scanner
            .has_inferred_capabilities(temp_dir.path())
            .expect("Scan failed"));
    }

    #[test]
    fn has_inferred_capabilities_returns_true_when_found() {
        let temp_dir = tempfile::TempDir::new().expect("Failed to create temp dir");
        fs::write(
            temp_dir.path().join("lib.rs"),
            "pub struct OrphanHandler { }",
        )
        .expect("Failed to write");

        let scanner = CapabilityScanner::new(HashSet::new());
        assert!(scanner
            .has_inferred_capabilities(temp_dir.path())
            .expect("Scan failed"));
    }
}
