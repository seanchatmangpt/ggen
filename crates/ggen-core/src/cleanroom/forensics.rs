//! Forensics - Failure investigation and reproducibility
//!
//! When a test fails, the forensics pack captures everything needed to:
//! 1. Reproduce the failure locally
//! 2. Investigate what went wrong
//! 3. Share the failure with teammates
//! 4. Store as regression test artifact
//!
//! A forensics pack includes:
//! - Redacted environment variables
//! - Stdout/stderr slices (with secrets removed)
//! - Scenario plan (what was supposed to happen)
//! - Image digests (for containers)
//! - SBOM (dependencies involved)
//! - Seccomp profile (security context)
//! - Coverage shards (what code was executed)
//! - Attestation (configuration used)
//!
//! ## Usage
//!
//! ```rust,no_run
//! use ggen_core::cleanroom::forensics::ForensicsPack;
//!
//! # fn main() -> Result<(), Box<dyn std::error::Error>> {
//! let mut pack = ForensicsPack::new("/tmp/test");
//!
//! // Capture output
//! pack.capture_stdout("Building project...\nBuild complete!");
//! pack.capture_stderr("Warning: deprecated function");
//!
//! // Add environment (auto-redacted)
//! pack.add_env("PATH", "/usr/bin:/usr/local/bin");
//! pack.add_env("SECRET_KEY", "sk-1234567890"); // Will be redacted
//!
//! // Generate the pack
//! pack.generate("/tmp/failure.crf", &attestation)?;
//! # Ok(())
//! # }
//! ```

use std::collections::HashMap;
use std::path::{Path, PathBuf};
use serde::{Serialize, Deserialize};
use std::fs;
use std::io::Write;

use super::attestation::Attestation;
use crate::error::Result;

/// Forensics pack - All artifacts needed to reproduce a test run
///
/// The `.crf` (Cleanroom Forensics) file format is a JSON bundle containing:
/// - Redacted environment
/// - Stdout/stderr (with secrets scrubbed)
/// - Scenario plan
/// - Image digests
/// - SBOM
/// - Seccomp profile
/// - Coverage data
/// - Attestation
///
/// Run `cleanroom replay file.crf` to reproduce locally.
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct ForensicsPack {
    /// Format version
    pub version: String,

    /// Project root (relative paths)
    pub project_root: PathBuf,

    /// Captured environment (redacted)
    pub env: HashMap<String, String>,

    /// Stdout capture (redacted)
    pub stdout: Vec<String>,

    /// Stderr capture (redacted)
    pub stderr: Vec<String>,

    /// Command that was run
    pub command: Vec<String>,

    /// Exit code
    pub exit_code: Option<i32>,

    /// Image digests
    pub images: HashMap<String, String>,

    /// Seccomp profile
    pub seccomp: Option<SeccompProfile>,

    /// Coverage shards
    pub coverage: Option<CoverageData>,

    /// Reproducer script
    pub reproducer: String,

    /// Custom metadata
    pub metadata: HashMap<String, serde_json::Value>,
}

/// Seccomp profile for security context
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct SeccompProfile {
    /// Default action
    pub default_action: String,

    /// Syscalls allowed
    pub syscalls: Vec<String>,

    /// Arch constraints
    pub architectures: Vec<String>,
}

/// Coverage data for executed code
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct CoverageData {
    /// Coverage files
    pub files: Vec<String>,

    /// Total lines
    pub total_lines: u64,

    /// Covered lines
    pub covered_lines: u64,

    /// Coverage percentage
    pub coverage_pct: f64,
}

impl ForensicsPack {
    /// Create new forensics pack
    pub fn new(project_root: impl Into<PathBuf>) -> Self {
        Self {
            version: "1.0".to_string(),
            project_root: project_root.into(),
            env: HashMap::new(),
            stdout: Vec::new(),
            stderr: Vec::new(),
            command: Vec::new(),
            exit_code: None,
            images: HashMap::new(),
            seccomp: None,
            coverage: None,
            reproducer: String::new(),
            metadata: HashMap::new(),
        }
    }

    /// Add environment variable (will be redacted if secret)
    pub fn add_env(&mut self, key: impl Into<String>, value: impl Into<String>) {
        let key = key.into();
        let value = value.into();

        // Redact secrets
        let redacted_value = if Self::is_secret(&key) {
            Self::redact(&value)
        } else {
            value
        };

        self.env.insert(key, redacted_value);
    }

    /// Capture stdout (will be redacted)
    pub fn capture_stdout(&mut self, output: impl AsRef<str>) {
        let lines = output.as_ref().lines();
        for line in lines {
            self.stdout.push(Self::redact_line(line));
        }
    }

    /// Capture stderr (will be redacted)
    pub fn capture_stderr(&mut self, output: impl AsRef<str>) {
        let lines = output.as_ref().lines();
        for line in lines {
            self.stderr.push(Self::redact_line(line));
        }
    }

    /// Set command that was run
    pub fn set_command(&mut self, command: Vec<String>) {
        self.command = command;
    }

    /// Set exit code
    pub fn set_exit_code(&mut self, code: i32) {
        self.exit_code = Some(code);
    }

    /// Add image digest
    pub fn add_image(&mut self, name: impl Into<String>, digest: impl Into<String>) {
        self.images.insert(name.into(), digest.into());
    }

    /// Set seccomp profile
    pub fn set_seccomp(&mut self, profile: SeccompProfile) {
        self.seccomp = Some(profile);
    }

    /// Set coverage data
    pub fn set_coverage(&mut self, coverage: CoverageData) {
        self.coverage = Some(coverage);
    }

    /// Add custom metadata
    pub fn add_metadata(&mut self, key: impl Into<String>, value: serde_json::Value) {
        self.metadata.insert(key.into(), value);
    }

    /// Generate forensics pack file (.crf)
    pub fn generate(&self, output_path: impl AsRef<Path>, attestation: &Attestation) -> Result<()> {
        // Create bundle with forensics + attestation
        let bundle = ForensicsBundle {
            forensics: self.clone(),
            attestation: attestation.clone(),
        };

        // Write to file
        let json = serde_json::to_string_pretty(&bundle)?;
        let mut file = fs::File::create(output_path.as_ref())?;
        file.write_all(json.as_bytes())?;

        Ok(())
    }

    /// Load forensics pack from file
    pub fn load(path: impl AsRef<Path>) -> Result<ForensicsBundle> {
        let json = fs::read_to_string(path)?;
        let bundle = serde_json::from_str(&json)?;
        Ok(bundle)
    }

    /// Generate reproducer script
    pub fn generate_reproducer(&mut self) {
        let mut script = String::new();

        script.push_str("#!/usr/bin/env bash\n");
        script.push_str("# Auto-generated reproducer script\n\n");

        script.push_str("set -e\n\n");

        script.push_str("# Environment (redacted secrets)\n");
        for (key, value) in &self.env {
            script.push_str(&format!("export {}='{}'\n", key, value));
        }

        script.push_str("\n# Change to project root\n");
        script.push_str(&format!("cd '{}'\n\n", self.project_root.display()));

        script.push_str("# Run command\n");
        script.push_str(&self.command.join(" "));
        script.push_str("\n");

        self.reproducer = script;
    }

    /// Check if environment variable is a secret
    fn is_secret(key: &str) -> bool {
        let key_lower = key.to_lowercase();
        key_lower.contains("secret")
            || key_lower.contains("password")
            || key_lower.contains("token")
            || key_lower.contains("key")
            || key_lower.contains("api_")
            || key_lower.contains("auth")
    }

    /// Redact a value
    fn redact(value: &str) -> String {
        if value.is_empty() {
            return String::new();
        }

        let len = value.len();
        if len <= 4 {
            return "*".repeat(len);
        }

        // Show first 2 and last 2 characters
        format!("{}...{}", &value[..2], &value[len - 2..])
    }

    /// Redact secrets in a line
    fn redact_line(line: &str) -> String {
        let mut redacted = line.to_string();

        // Common secret patterns
        let patterns = [
            (r"(sk-[a-zA-Z0-9]{48})", "sk-**REDACTED**"),
            (r"(ghp_[a-zA-Z0-9]{36})", "ghp_**REDACTED**"),
            (r"(AIza[a-zA-Z0-9]{35})", "AIza**REDACTED**"),
            (r"([a-zA-Z0-9]{32})", "**REDACTED**"), // Generic 32-char strings
        ];

        for (pattern, replacement) in &patterns {
            if let Ok(re) = regex::Regex::new(pattern) {
                redacted = re.replace_all(&redacted, *replacement).to_string();
            }
        }

        redacted
    }
}

/// Bundle containing forensics + attestation
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct ForensicsBundle {
    pub forensics: ForensicsPack,
    pub attestation: Attestation,
}

impl ForensicsBundle {
    /// Get reproducer script
    pub fn reproducer(&self) -> &str {
        &self.forensics.reproducer
    }

    /// Extract reproducer to file
    pub fn extract_reproducer(&self, path: impl AsRef<Path>) -> Result<()> {
        fs::write(path, &self.forensics.reproducer)?;

        // Make executable on Unix
        #[cfg(unix)]
        {
            use std::os::unix::fs::PermissionsExt;
            let metadata = fs::metadata(&path)?;
            let mut permissions = metadata.permissions();
            permissions.set_mode(0o755);
            fs::set_permissions(&path, permissions)?;
        }

        Ok(())
    }

    /// Print summary
    pub fn summary(&self) -> String {
        let mut summary = String::new();

        summary.push_str("# Forensics Pack Summary\n\n");

        summary.push_str(&format!("Policy: {}\n", self.attestation.policy));
        summary.push_str(&format!("Determinism: {:.2}\n", self.attestation.determinism_score));
        summary.push_str(&format!("Exit Code: {:?}\n", self.forensics.exit_code));
        summary.push_str(&format!("Command: {}\n\n", self.forensics.command.join(" ")));

        summary.push_str("## Stdout\n");
        for line in self.forensics.stdout.iter().take(10) {
            summary.push_str(&format!("  {}\n", line));
        }
        if self.forensics.stdout.len() > 10 {
            summary.push_str(&format!("  ... {} more lines\n", self.forensics.stdout.len() - 10));
        }

        summary.push_str("\n## Stderr\n");
        for line in self.forensics.stderr.iter().take(10) {
            summary.push_str(&format!("  {}\n", line));
        }
        if self.forensics.stderr.len() > 10 {
            summary.push_str(&format!("  ... {} more lines\n", self.forensics.stderr.len() - 10));
        }

        summary
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_secret_detection() {
        assert!(ForensicsPack::is_secret("SECRET_KEY"));
        assert!(ForensicsPack::is_secret("API_TOKEN"));
        assert!(ForensicsPack::is_secret("PASSWORD"));
        assert!(!ForensicsPack::is_secret("PATH"));
        assert!(!ForensicsPack::is_secret("HOME"));
    }

    #[test]
    fn test_secret_redaction() {
        assert_eq!(ForensicsPack::redact("sk-1234567890"), "sk...90");
        assert_eq!(ForensicsPack::redact("short"), "*****");
    }

    #[test]
    fn test_forensics_pack_creation() {
        let mut pack = ForensicsPack::new("/tmp/test");

        pack.add_env("PATH", "/usr/bin");
        pack.add_env("SECRET_KEY", "sk-1234567890");

        assert_eq!(pack.env.get("PATH").unwrap(), "/usr/bin");
        assert!(pack.env.get("SECRET_KEY").unwrap().contains("..."));
    }

    #[test]
    fn test_reproducer_generation() {
        let mut pack = ForensicsPack::new("/tmp/test");
        pack.set_command(vec!["cargo".to_string(), "test".to_string()]);
        pack.generate_reproducer();

        assert!(pack.reproducer.contains("#!/usr/bin/env bash"));
        assert!(pack.reproducer.contains("cargo test"));
    }
}
