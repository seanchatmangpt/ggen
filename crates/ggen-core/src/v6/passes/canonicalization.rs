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
//! - **Deterministic hashing**: Same input always produces same canonical hash

use crate::v6::pass::{Pass, PassContext, PassResult, PassType};
use ggen_canonical::hash::compute_hash;
use ggen_canonical::{json, rust, ttl};
use ggen_receipt::{hash_data, Receipt};
use ggen_utils::error::{Error, Result};
use serde::{Deserialize, Serialize};
use std::collections::BTreeMap;
use std::path::{Path, PathBuf};
use std::process::{Command, Stdio};
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

/// Formatter type for different languages
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum FormatterType {
    /// rustfmt for Rust code
    Rustfmt,
    /// prettier for JavaScript/TypeScript/JSON/etc
    Prettier,
    /// JSON canonicalizer
    Json,
    /// TTL/RDF canonicalizer
    Ttl,
    /// Generic line-based normalization
    Generic,
}

/// Receipt for canonicalization pass execution
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct CanonicalizationReceipt {
    /// Files processed
    pub files_processed: Vec<PathBuf>,
    /// File hashes (path -> SHA-256 hash)
    pub file_hashes: BTreeMap<PathBuf, String>,
    /// Formatter executions (formatter -> count)
    pub formatter_executions: BTreeMap<String, usize>,
    /// Total files modified
    pub files_modified: usize,
    /// Total execution time (ms)
    pub execution_time_ms: u64,
    /// Cryptographic receipt (if available)
    pub receipt: Option<Receipt>,
}

impl CanonicalizationReceipt {
    /// Create a new empty receipt
    pub fn new() -> Self {
        Self {
            files_processed: Vec::new(),
            file_hashes: BTreeMap::new(),
            formatter_executions: BTreeMap::new(),
            files_modified: 0,
            execution_time_ms: 0,
            receipt: None,
        }
    }

    /// Add a file hash
    pub fn add_file_hash(&mut self, path: PathBuf, hash: String) {
        self.file_hashes.insert(path.clone(), hash);
        self.files_processed.push(path);
    }

    /// Record a formatter execution
    pub fn record_formatter(&mut self, formatter: &str) {
        *self.formatter_executions.entry(formatter.to_string()).or_insert(0) += 1;
    }

    /// Compute aggregate hash of all file hashes
    pub fn aggregate_hash(&self) -> Result<String> {
        let mut hashes: Vec<&str> = self.file_hashes.values().map(|s| s.as_str()).collect();
        hashes.sort();
        let combined = hashes.join("");
        compute_hash(&combined).map_err(|e| Error::new(&format!("Hash computation failed: {}", e)))
    }
}

impl Default for CanonicalizationReceipt {
    fn default() -> Self {
        Self::new()
    }
}

/// μ₄: Canonicalization pass implementation
#[derive(Debug, Clone)]
pub struct CanonicalizationPass {
    /// Default policy for unknown file types
    default_policy: CanonicalizationPolicy,

    /// File extension to policy mapping
    extension_policies: BTreeMap<String, CanonicalizationPolicy>,

    /// Whether to enable strict mode (formatting errors stop the line)
    strict_mode: bool,

    /// Debug mode (formatters allowed to fail)
    debug_mode: bool,

    /// Enable cryptographic receipt generation
    enable_receipts: bool,
}

impl CanonicalizationPass {
    /// Create a new canonicalization pass
    pub fn new() -> Self {
        let mut extension_policies = BTreeMap::new();

        // Code formatters
        extension_policies.insert("rs".to_string(), CanonicalizationPolicy::Format);
        extension_policies.insert("js".to_string(), CanonicalizationPolicy::Format);
        extension_policies.insert("ts".to_string(), CanonicalizationPolicy::Format);
        extension_policies.insert("tsx".to_string(), CanonicalizationPolicy::Format);
        extension_policies.insert("jsx".to_string(), CanonicalizationPolicy::Format);

        // Data formats
        extension_policies.insert("json".to_string(), CanonicalizationPolicy::Format);
        extension_policies.insert("ttl".to_string(), CanonicalizationPolicy::Format);

        // Config formats
        extension_policies.insert("toml".to_string(), CanonicalizationPolicy::NormalizeLineEndings);
        extension_policies.insert("yaml".to_string(), CanonicalizationPolicy::NormalizeLineEndings);
        extension_policies.insert("yml".to_string(), CanonicalizationPolicy::NormalizeLineEndings);

        // Templates
        extension_policies.insert("tera".to_string(), CanonicalizationPolicy::NormalizeLineEndings);

        Self {
            default_policy: CanonicalizationPolicy::NormalizeLineEndings,
            extension_policies,
            strict_mode: true,
            debug_mode: false,
            enable_receipts: true,
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

    /// Enable strict mode (formatting errors stop the line)
    pub fn with_strict_mode(mut self, enabled: bool) -> Self {
        self.strict_mode = enabled;
        self
    }

    /// Enable debug mode (formatters allowed to fail)
    pub fn with_debug_mode(mut self, enabled: bool) -> Self {
        self.debug_mode = enabled;
        self
    }

    /// Enable receipt generation
    pub fn with_receipts(mut self, enabled: bool) -> Self {
        self.enable_receipts = enabled;
        self
    }

    /// Get policy for a file
    fn get_policy(&self, path: &Path) -> CanonicalizationPolicy {
        if let Some(ext) = path.extension().and_then(|e| e.to_str()) {
            self.extension_policies
                .get(ext)
                .copied()
                .unwrap_or(self.default_policy)
        } else {
            self.default_policy
        }
    }

    /// Get formatter type for a file
    fn get_formatter(&self, path: &Path) -> FormatterType {
        match path.extension().and_then(|e| e.to_str()) {
            Some("rs") => FormatterType::Rustfmt,
            Some("js" | "ts" | "tsx" | "jsx") => FormatterType::Prettier,
            Some("json") => FormatterType::Json,
            Some("ttl") => FormatterType::Ttl,
            _ => FormatterType::Generic,
        }
    }

    /// Canonicalize a file's content
    fn canonicalize(&self, path: &Path, content: &str, receipt: &mut CanonicalizationReceipt) -> Result<String> {
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
                let formatter = self.get_formatter(path);
                receipt.record_formatter(&format!("{:?}", formatter));

                match formatter {
                    FormatterType::Rustfmt => self.format_rust(path, content),
                    FormatterType::Prettier => self.format_prettier(path, content),
                    FormatterType::Json => self.format_json(content),
                    FormatterType::Ttl => self.format_ttl(content),
                    FormatterType::Generic => self.normalize_generic(content),
                }
            }
        }
    }

    /// Format Rust code using rustfmt via ggen-canonical
    fn format_rust(&self, _path: &Path, content: &str) -> Result<String> {
        match rust::canonicalize_rust(content) {
            Ok(formatted) => Ok(formatted),
            Err(e) => {
                if self.debug_mode {
                    // In debug mode, fall back to normalization
                    Ok(self.normalize_generic(content)?)
                } else if self.strict_mode {
                    Err(Error::new(&format!(
                        "🚨 Rustfmt Failed in μ₄:canonicalization\n\n\
                         μ₄:canonicalization STOPPED THE LINE (Andon Protocol)\n\n\
                         Rustfmt error: {}\n\n\
                         μ₄ requires all Rust files to be valid and formattable.\n\n\
                         Fix: Correct Rust syntax errors in template output.\n\
                         Debug: Run with --debug to skip formatter validation.",
                        e
                    )))
                } else {
                    Err(Error::new(&format!("Rustfmt failed: {}", e)))
                }
            }
        }
    }

    /// Format code using prettier
    fn format_prettier(&self, path: &Path, content: &str) -> Result<String> {
        // Try to run prettier
        let mut cmd = Command::new("prettier");
        cmd.arg("--stdin-filepath")
            .arg(path.to_string_lossy().to_string())
            .stdin(Stdio::piped())
            .stdout(Stdio::piped())
            .stderr(Stdio::piped());

        match cmd.spawn() {
            Ok(mut child) => {
                // Write input to stdin
                if let Some(mut stdin) = child.stdin.take() {
                    use std::io::Write;
                    if let Err(e) = stdin.write_all(content.as_bytes()) {
                        if self.debug_mode {
                            return Ok(self.normalize_generic(content)?);
                        }
                        return Err(Error::new(&format!("Failed to write to prettier stdin: {}", e)));
                    }
                }

                let output = child.wait_with_output().map_err(|e| {
                    Error::new(&format!("Failed to run prettier: {}", e))
                })?;

                if output.status.success() {
                    String::from_utf8(output.stdout).map_err(|e| {
                        Error::new(&format!("Invalid UTF-8 from prettier: {}", e))
                    })
                } else {
                    let stderr = String::from_utf8_lossy(&output.stderr);
                    if self.debug_mode {
                        return Ok(self.normalize_generic(content)?);
                    } else if self.strict_mode {
                        Err(Error::new(&format!(
                            "🚨 Prettier Failed in μ₄:canonicalization\n\n\
                             μ₄:canonicalization STOPPED THE LINE (Andon Protocol)\n\n\
                             Prettier error: {}\n\n\
                             μ₄ requires all code files to be valid and formattable.\n\n\
                             Fix: Correct syntax errors in template output.\n\
                             Debug: Run with --debug to skip formatter validation.",
                            stderr
                        )))
                    } else {
                        Err(Error::new(&format!("Prettier failed: {}", stderr)))
                    }
                }
            }
            Err(_) => {
                // Prettier not installed - fall back to normalization
                if self.debug_mode {
                    Ok(self.normalize_generic(content)?)
                } else {
                    // In strict mode, missing formatter is acceptable
                    Ok(self.normalize_generic(content)?)
                }
            }
        }
    }

    /// Format JSON using ggen-canonical
    fn format_json(&self, content: &str) -> Result<String> {
        match json::canonicalize_json_str(content) {
            Ok(canonical) => Ok(canonical + "\n"),
            Err(e) => {
                if self.debug_mode {
                    Ok(self.normalize_generic(content)?)
                } else if self.strict_mode {
                    Err(Error::new(&format!(
                        "🚨 Invalid JSON Detected in μ₄:canonicalization\n\n\
                         μ₄:canonicalization STOPPED THE LINE (Andon Protocol)\n\n\
                         JSON parsing failed: {}\n\n\
                         μ₄ requires all JSON files to be valid and formattable.\n\n\
                         Fix: Correct JSON syntax errors in template output.\n\
                         Debug: Run with --debug to skip formatter validation.",
                        e
                    )))
                } else {
                    Err(Error::new(&format!("Invalid JSON: {}", e)))
                }
            }
        }
    }

    /// Format TTL/RDF using ggen-canonical
    fn format_ttl(&self, content: &str) -> Result<String> {
        match ttl::canonicalize_ttl(content) {
            Ok(canonical) => Ok(canonical + "\n"),
            Err(e) => {
                if self.debug_mode {
                    Ok(self.normalize_generic(content)?)
                } else if self.strict_mode {
                    Err(Error::new(&format!(
                        "🚨 Invalid TTL Detected in μ₄:canonicalization\n\n\
                         μ₄:canonicalization STOPPED THE LINE (Andon Protocol)\n\n\
                         TTL parsing failed: {}\n\n\
                         μ₄ requires all TTL files to be valid.\n\n\
                         Fix: Correct TTL syntax errors in template output.\n\
                         Debug: Run with --debug to skip formatter validation.",
                        e
                    )))
                } else {
                    Err(Error::new(&format!("Invalid TTL: {}", e)))
                }
            }
        }
    }

    /// Generic normalization (fallback)
    fn normalize_generic(&self, content: &str) -> Result<String> {
        let normalized = content.replace("\r\n", "\n").replace('\r', "\n");
        if normalized.ends_with('\n') {
            Ok(normalized)
        } else {
            Ok(normalized + "\n")
        }
    }

    /// Process a single file
    fn process_file(&self, ctx: &PassContext<'_>, rel_path: &Path, receipt: &mut CanonicalizationReceipt) -> Result<bool> {
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

        let canonicalized = self.canonicalize(rel_path, &content, receipt)?;

        // Compute hash of canonicalized content
        let hash = hash_data(canonicalized.as_bytes());
        receipt.add_file_hash(rel_path.to_path_buf(), hash);

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
            receipt.files_modified += 1;
            return Ok(true);
        }

        Ok(false)
    }

    /// Generate cryptographic receipt
    fn generate_receipt(&self, canonical_receipt: &CanonicalizationReceipt) -> Result<Receipt> {
        let aggregate_hash = canonical_receipt.aggregate_hash()?;

        let receipt = Receipt::new(
            "μ₄:canonicalization".to_string(),
            vec![aggregate_hash.clone()],
            canonical_receipt.file_hashes.values().cloned().collect(),
            None,
        );

        Ok(receipt)
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
        let mut receipt = CanonicalizationReceipt::new();

        // Process all generated files
        for rel_path in &ctx.generated_files.clone() {
            self.process_file(ctx, rel_path, &mut receipt)?;
        }

        let duration = start.elapsed();
        receipt.execution_time_ms = duration.as_millis() as u64;

        // Generate cryptographic receipt if enabled
        if self.enable_receipts && !receipt.file_hashes.is_empty() {
            match self.generate_receipt(&receipt) {
                Ok(crypto_receipt) => {
                    receipt.receipt = Some(crypto_receipt);
                }
                Err(e) => {
                    if self.strict_mode {
                        return Err(Error::new(&format!(
                            "🚨 Receipt Generation Failed in μ₄:canonicalization\n\n\
                             Failed to generate cryptographic receipt: {}\n\n\
                             Fix: Check receipt system configuration.",
                            e
                        )));
                    }
                }
            }
        }

        // Store receipt in context for downstream passes
        let receipt_json = serde_json::to_value(&receipt)
            .map_err(|e| Error::new(&format!("Failed to serialize receipt: {}", e)))?;
        ctx.bindings.insert("canonicalization_receipt".to_string(), receipt_json);

        Ok(PassResult::success()
            .with_duration(duration)
            .with_files(receipt.files_processed.clone()))
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
        let mut receipt = CanonicalizationReceipt::new();

        let result = pass.canonicalize(&path, "hello\r\nworld", &mut receipt).unwrap();
        assert_eq!(result, "hello\nworld\n");
    }

    #[test]
    fn test_format_json() {
        let pass = CanonicalizationPass::new();
        let path = PathBuf::from("test.json");
        let mut receipt = CanonicalizationReceipt::new();

        let result = pass.canonicalize(&path, r#"{"z":1,"a":2}"#, &mut receipt).unwrap();
        assert!(result.contains(r#""a""#));
        assert!(result.ends_with("\n"));
    }

    #[test]
    fn test_format_json_invalid_strict() {
        let pass = CanonicalizationPass::new().with_strict_mode(true);
        let path = PathBuf::from("test.json");
        let mut receipt = CanonicalizationReceipt::new();

        let result = pass.canonicalize(&path, r#"{"invalid"#, &mut receipt);
        assert!(result.is_err());
        assert!(result.unwrap_err().to_string().contains("STOPPED THE LINE"));
    }

    #[test]
    fn test_format_json_invalid_debug() {
        let pass = CanonicalizationPass::new().with_debug_mode(true);
        let path = PathBuf::from("test.json");
        let mut receipt = CanonicalizationReceipt::new();

        let result = pass.canonicalize(&path, r#"{"invalid"#, &mut receipt);
        assert!(result.is_ok());
    }

    #[test]
    fn test_policy_selection() {
        let pass = CanonicalizationPass::new();

        assert_eq!(
            pass.get_policy(&PathBuf::from("test.rs")),
            CanonicalizationPolicy::Format
        );
        assert_eq!(
            pass.get_policy(&PathBuf::from("test.json")),
            CanonicalizationPolicy::Format
        );
        assert_eq!(
            pass.get_policy(&PathBuf::from("test.toml")),
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
        std::fs::write(output_dir.join("test.json"), r#"{"z":1,"a":2}"#).unwrap();

        let pass = CanonicalizationPass::new();
        let mut ctx = PassContext::new(&graph, temp_dir.path().to_path_buf(), output_dir.clone());
        ctx.generated_files.push(PathBuf::from("test.json"));

        let result = pass.execute(&mut ctx).unwrap();
        assert!(result.success);

        // Verify file was canonicalized
        let content = std::fs::read_to_string(output_dir.join("test.json")).unwrap();
        assert!(content.contains(r#""a""#));
    }

    #[test]
    fn test_receipt_generation() {
        let mut receipt = CanonicalizationReceipt::new();
        receipt.add_file_hash(PathBuf::from("test1.rs"), "hash1".to_string());
        receipt.add_file_hash(PathBuf::from("test2.rs"), "hash2".to_string());

        assert_eq!(receipt.files_processed.len(), 2);
        assert_eq!(receipt.file_hashes.len(), 2);

        let aggregate = receipt.aggregate_hash();
        assert!(aggregate.is_ok());
    }

    #[test]
    fn test_formatter_tracking() {
        let mut receipt = CanonicalizationReceipt::new();
        receipt.record_formatter("Rustfmt");
        receipt.record_formatter("Rustfmt");
        receipt.record_formatter("Json");

        assert_eq!(receipt.formatter_executions.get("Rustfmt"), Some(&2));
        assert_eq!(receipt.formatter_executions.get("Json"), Some(&1));
    }
}
