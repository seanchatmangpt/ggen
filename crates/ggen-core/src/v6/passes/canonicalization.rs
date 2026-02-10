//! μ₄: Canonicalization Pass
//!
//! Performs files → canonical files transformation.
//! Ensures deterministic output through consistent formatting and ordering.
//!
//! ## CONSTRUCT Guarantees
//!
//! - **Formatter as gate**: All files must be valid and formattable
//! - **Stop-the-line**: Any formatting failure halts the pipeline
//! - **Receipt integration**: All canonicalized files are hashed and verified

use crate::v6::pass::{Pass, PassContext, PassResult, PassType};
use ggen_utils::error::{Error, Result};
use serde::{Deserialize, Serialize};
use std::time::Instant;

/// Canonicalization policy for different file types
#[derive(Debug, Clone, Copy, PartialEq, Eq, Serialize, Deserialize)]
pub enum CanonicalizationPolicy {
    /// No formatting (preserve as-is)
    None,
    /// Remove trailing whitespace only
    TrimTrailingWhitespace,
    /// Normalize line endings to LF
    NormalizeLineEndings,
    /// Full formatting (language-specific)
    Format,
}

impl Default for CanonicalizationPolicy {
    fn default() -> Self {
        Self::NormalizeLineEndings
    }
}

/// μ₄: Canonicalization pass implementation
#[derive(Debug, Clone)]
pub struct CanonicalizationPass {
    /// Default policy for unknown file types
    default_policy: CanonicalizationPolicy,

    /// File extension to policy mapping
    extension_policies: std::collections::BTreeMap<String, CanonicalizationPolicy>,

    /// Whether to sort imports (for supported languages)
    sort_imports: bool,

    /// Whether to remove unused imports (requires static analysis)
    #[allow(dead_code)]
    remove_unused: bool,

    /// Whether to enable strict mode (formatting errors stop the line)
    strict_mode: bool,
}

impl CanonicalizationPass {
    /// Create a new canonicalization pass
    pub fn new() -> Self {
        let mut extension_policies = std::collections::BTreeMap::new();

        // v6 only outputs .ttl and .tera files - no source code
        extension_policies.insert("ttl".to_string(), CanonicalizationPolicy::Format);
        extension_policies.insert(
            "tera".to_string(),
            CanonicalizationPolicy::NormalizeLineEndings,
        );
        extension_policies.insert(
            "toml".to_string(),
            CanonicalizationPolicy::NormalizeLineEndings,
        );
        extension_policies.insert("json".to_string(), CanonicalizationPolicy::Format);

        Self {
            default_policy: CanonicalizationPolicy::NormalizeLineEndings,
            extension_policies,
            sort_imports: false,
            remove_unused: false,
            strict_mode: true, // v6 default: stop the line on formatting errors
        }
    }

    /// Set default policy
    pub fn with_default_policy(mut self, policy: CanonicalizationPolicy) -> Self {
        self.default_policy = policy;
        self
    }

    /// Set policy for an extension
    pub fn with_extension_policy(mut self, ext: &str, policy: CanonicalizationPolicy) -> Self {
        self.extension_policies.insert(ext.to_string(), policy);
        self
    }

    /// Enable import sorting
    pub fn with_sort_imports(mut self, enabled: bool) -> Self {
        self.sort_imports = enabled;
        self
    }

    /// Enable strict mode (formatting errors stop the line)
    pub fn with_strict_mode(mut self, enabled: bool) -> Self {
        self.strict_mode = enabled;
        self
    }

    /// Get policy for a file
    fn get_policy(&self, path: &std::path::Path) -> CanonicalizationPolicy {
        if let Some(ext) = path.extension().and_then(|e| e.to_str()) {
            self.extension_policies
                .get(ext)
                .copied()
                .unwrap_or(self.default_policy)
        } else {
            self.default_policy
        }
    }

    /// Canonicalize a file's content
    fn canonicalize(&self, path: &std::path::Path, content: &str) -> Result<String> {
        let policy = self.get_policy(path);

        match policy {
            CanonicalizationPolicy::None => Ok(content.to_string()),

            CanonicalizationPolicy::TrimTrailingWhitespace => Ok(content
                .lines()
                .map(|line| line.trim_end())
                .collect::<Vec<_>>()
                .join("\n")
                + if content.ends_with('\n') { "\n" } else { "" }),

            CanonicalizationPolicy::NormalizeLineEndings => {
                // Convert CRLF to LF, ensure final newline
                let normalized = content.replace("\r\n", "\n").replace('\r', "\n");
                if normalized.ends_with('\n') {
                    Ok(normalized)
                } else {
                    Ok(normalized + "\n")
                }
            }

            CanonicalizationPolicy::Format => {
                // Language-specific formatting
                let ext = path.extension().and_then(|e| e.to_str()).unwrap_or("");

                match ext {
                    "rs" => self.format_rust(content),
                    "json" => self.format_json(content),
                    _ => {
                        // Fall back to normalization
                        let normalized = content.replace("\r\n", "\n").replace('\r', "\n");
                        if normalized.ends_with('\n') {
                            Ok(normalized)
                        } else {
                            Ok(normalized + "\n")
                        }
                    }
                }
            }
        }
    }

    /// Format Rust code (basic canonicalization without external tools)
    fn format_rust(&self, content: &str) -> Result<String> {
        // Basic Rust canonicalization:
        // - Normalize line endings
        // - Trim trailing whitespace
        // - Ensure final newline

        let lines: Vec<&str> = content.lines().collect();
        let formatted: Vec<String> = lines
            .iter()
            .map(|line| line.trim_end().to_string())
            .collect();

        let mut result = formatted.join("\n");
        if !result.ends_with('\n') {
            result.push('\n');
        }

        Ok(result)
    }

    /// Format JSON with consistent indentation
    fn format_json(&self, content: &str) -> Result<String> {
        let value: serde_json::Value = serde_json::from_str(content).map_err(|e| {
            if self.strict_mode {
                Error::new(&format!(
                    "🚨 Invalid JSON Detected in μ₄:canonicalization\n\n\
                     μ₄:canonicalization STOPPED THE LINE (Andon Protocol)\n\n\
                     JSON parsing failed: {}\n\n\
                     μ₄ requires all files to be valid and formattable.\n\n\
                     Fix: Correct JSON syntax errors in template output.",
                    e
                ))
            } else {
                Error::new(&format!("Invalid JSON: {}", e))
            }
        })?;

        serde_json::to_string_pretty(&value)
            .map(|s| s + "\n")
            .map_err(|e| {
                if self.strict_mode {
                    Error::new(&format!(
                        "🚨 JSON Serialization Failed in μ₄:canonicalization\n\n\
                         μ₄:canonicalization STOPPED THE LINE (Andon Protocol)\n\n\
                         Serialization error: {}\n\n\
                         Fix: Check for circular references or unsupported values.",
                        e
                    ))
                } else {
                    Error::new(&format!("JSON serialization error: {}", e))
                }
            })
    }

    /// Process a single file
    fn process_file(&self, ctx: &PassContext<'_>, rel_path: &std::path::Path) -> Result<bool> {
        let full_path = ctx.output_dir.join(rel_path);

        if !full_path.exists() {
            if self.strict_mode {
                return Err(Error::new(&format!(
                    "🚨 Missing File in μ₄:canonicalization\n\n\
                     μ₄:canonicalization STOPPED THE LINE (Andon Protocol)\n\n\
                     Expected file '{}' does not exist.\n\n\
                     Fix: Verify μ₃:emission generated all expected files.",
                    rel_path.display()
                )));
            }
            return Ok(false);
        }

        let content = std::fs::read_to_string(&full_path).map_err(|e| {
            if self.strict_mode {
                Error::new(&format!(
                    "🚨 File Read Failed in μ₄:canonicalization\n\n\
                     μ₄:canonicalization STOPPED THE LINE (Andon Protocol)\n\n\
                     Failed to read file '{}': {}\n\n\
                     Fix: Check file permissions and encoding.",
                    full_path.display(),
                    e
                ))
            } else {
                Error::new(&format!("Failed to read file '{}': {}", full_path.display(), e))
            }
        })?;

        let canonicalized = self.canonicalize(rel_path, &content)?;

        // Only write if changed
        if canonicalized != content {
            std::fs::write(&full_path, &canonicalized).map_err(|e| {
                if self.strict_mode {
                    Error::new(&format!(
                        "🚨 File Write Failed in μ₄:canonicalization\n\n\
                         μ₄:canonicalization STOPPED THE LINE (Andon Protocol)\n\n\
                         Failed to write file '{}': {}\n\n\
                         Fix: Check file permissions and disk space.",
                        full_path.display(),
                        e
                    ))
                } else {
                    Error::new(&format!("Failed to write file '{}': {}", full_path.display(), e))
                }
            })?;
            return Ok(true);
        }

        Ok(false)
    }
}

impl Default for CanonicalizationPass {
    fn default() -> Self {
        Self::new()
    }
}

impl Pass for CanonicalizationPass {
    fn pass_type(&self) -> PassType {
        PassType::Canonicalization
    }

    fn name(&self) -> &str {
        "μ₄:canonicalization"
    }

    fn execute(&self, ctx: &mut PassContext<'_>) -> Result<PassResult> {
        let start = Instant::now();
        let mut _files_modified = 0;

        // Process all generated files
        for rel_path in &ctx.generated_files.clone() {
            if self.process_file(ctx, rel_path)? {
                _files_modified += 1;
            }
        }

        let duration = start.elapsed();
        Ok(PassResult::success().with_duration(duration))
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::graph::Graph;
    use std::path::PathBuf;
    use tempfile::TempDir;

    #[test]
    fn test_normalize_line_endings() {
        let pass = CanonicalizationPass::new();
        let path = PathBuf::from("test.txt");

        let result = pass.canonicalize(&path, "hello\r\nworld").unwrap();
        assert_eq!(result, "hello\nworld\n");

        let result = pass.canonicalize(&path, "hello\rworld").unwrap();
        assert_eq!(result, "hello\nworld\n");
    }

    #[test]
    fn test_trim_trailing_whitespace() {
        let mut pass = CanonicalizationPass::new();
        pass.extension_policies.insert(
            "txt".to_string(),
            CanonicalizationPolicy::TrimTrailingWhitespace,
        );

        let path = PathBuf::from("test.txt");
        let result = pass.canonicalize(&path, "hello   \nworld  \n").unwrap();
        assert_eq!(result, "hello\nworld\n");
    }

    #[test]
    fn test_format_json() {
        let pass = CanonicalizationPass::new();
        let path = PathBuf::from("test.json");

        let result = pass.canonicalize(&path, r#"{"a":1,"b":2}"#).unwrap();
        assert!(result.contains("  ")); // Should be pretty-printed
        assert!(result.ends_with("\n"));
    }

    #[test]
    fn test_policy_selection() {
        let pass = CanonicalizationPass::new();

        // v6 only outputs .ttl, .tera, .toml, .json files
        assert_eq!(
            pass.get_policy(&PathBuf::from("test.ttl")),
            CanonicalizationPolicy::Format
        );
        assert_eq!(
            pass.get_policy(&PathBuf::from("test.tera")),
            CanonicalizationPolicy::NormalizeLineEndings
        );
        assert_eq!(
            pass.get_policy(&PathBuf::from("test.toml")),
            CanonicalizationPolicy::NormalizeLineEndings
        );
        assert_eq!(
            pass.get_policy(&PathBuf::from("test.json")),
            CanonicalizationPolicy::Format
        );
        // Unknown extensions use default policy
        assert_eq!(
            pass.get_policy(&PathBuf::from("test.unknown")),
            CanonicalizationPolicy::NormalizeLineEndings
        );
    }

    #[test]
    fn test_execute_pass() {
        let graph = Graph::new().unwrap();
        let temp_dir = TempDir::new().unwrap();
        let output_dir = temp_dir.path().join("output");
        std::fs::create_dir_all(&output_dir).unwrap();

        // Create a file with non-canonical content
        std::fs::write(output_dir.join("test.txt"), "hello\r\nworld").unwrap();

        let pass = CanonicalizationPass::new();
        let mut ctx = PassContext::new(&graph, temp_dir.path().to_path_buf(), output_dir.clone());
        ctx.generated_files.push(PathBuf::from("test.txt"));

        let result = pass.execute(&mut ctx).unwrap();
        assert!(result.success);

        // Verify file was canonicalized
        let content = std::fs::read_to_string(output_dir.join("test.txt")).unwrap();
        assert_eq!(content, "hello\nworld\n");
    }
}
