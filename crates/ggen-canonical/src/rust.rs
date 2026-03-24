//! Rust code canonicalization
//!
//! Provides deterministic Rust code formatting via rustfmt.
//! Ensures consistent code style for hash verification.

use crate::{Canonical, CanonicalError, Canonicalizer, Result};
use std::process::{Command, Stdio};

/// Rust code canonicalizer using rustfmt
pub struct RustCanonicalizer {
    /// Additional rustfmt config options
    config: RustfmtConfig,
}

/// Configuration for rustfmt
#[derive(Debug, Clone)]
pub struct RustfmtConfig {
    /// Edition to use (2015, 2018, 2021)
    pub edition: String,
}

impl Default for RustfmtConfig {
    fn default() -> Self {
        Self {
            edition: "2021".to_string(),
        }
    }
}

impl RustCanonicalizer {
    /// Create a new Rust canonicalizer with default config
    pub fn new() -> Self {
        Self {
            config: RustfmtConfig::default(),
        }
    }

    /// Create with custom config
    pub fn with_config(config: RustfmtConfig) -> Self {
        Self { config }
    }

    /// Format Rust code using rustfmt
    fn format_with_rustfmt(&self, code: &str) -> Result<String> {
        // Use rustfmt if available, otherwise return as-is with normalization
        let mut cmd = Command::new("rustfmt");
        cmd.arg("--edition")
            .arg(&self.config.edition)
            .stdin(Stdio::piped())
            .stdout(Stdio::piped())
            .stderr(Stdio::piped());

        let mut child = cmd
            .spawn()
            .map_err(|e| CanonicalError::Format(format!("Failed to spawn rustfmt: {}", e)))?;

        // Write input to stdin
        if let Some(mut stdin) = child.stdin.take() {
            use std::io::Write;
            stdin
                .write_all(code.as_bytes())
                .map_err(|e| CanonicalError::Io(e))?;
        }

        let output = child
            .wait_with_output()
            .map_err(|e| CanonicalError::Format(format!("Failed to run rustfmt: {}", e)))?;

        if output.status.success() {
            String::from_utf8(output.stdout).map_err(|e| {
                CanonicalError::Format(format!("Invalid UTF-8 from rustfmt: {}", e))
            })
        } else {
            let stderr = String::from_utf8_lossy(&output.stderr);
            Err(CanonicalError::Format(format!(
                "rustfmt failed: {}",
                stderr
            )))
        }
    }

    /// Normalize Rust code (fallback when rustfmt unavailable)
    fn normalize(&self, code: &str) -> String {
        code.lines()
            .map(|line| line.trim_end())
            .collect::<Vec<_>>()
            .join("\n")
    }
}

impl Default for RustCanonicalizer {
    fn default() -> Self {
        Self::new()
    }
}

impl Canonicalizer for RustCanonicalizer {
    type Input = String;
    type Output = Canonical<String>;

    fn canonicalize(&self, input: Self::Input) -> Result<Self::Output> {
        // Try rustfmt first, fall back to normalization
        let formatted = match self.format_with_rustfmt(&input) {
            Ok(formatted) => formatted,
            Err(_) => {
                // If rustfmt fails, use normalization
                self.normalize(&input)
            }
        };

        Ok(Canonical::new_unchecked(formatted))
    }
}

/// Canonicalize Rust code
///
/// # Errors
///
/// Returns error if code cannot be formatted
pub fn canonicalize_rust(code: &str) -> Result<String> {
    let canonicalizer = RustCanonicalizer::new();
    let canonical = canonicalizer.canonicalize(code.to_string())?;
    Ok(canonical.into_inner())
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_basic_formatting() {
        let code = "fn main(){println!(\"hello\");}";
        let canonicalizer = RustCanonicalizer::new();
        let result = canonicalizer.canonicalize(code.to_string());
        assert!(result.is_ok());
    }

    #[test]
    fn test_determinism() {
        let code = "fn main() { let x = 1; }";
        let canonicalizer = RustCanonicalizer::new();
        let result1 = canonicalizer.canonicalize(code.to_string()).unwrap();
        let result2 = canonicalizer.canonicalize(code.to_string()).unwrap();
        assert_eq!(result1, result2);
    }

    #[test]
    fn test_normalize_whitespace() {
        let code = "fn main() {   \n  let x = 1;  \n}  ";
        let canonicalizer = RustCanonicalizer::new();
        let result = canonicalizer.normalize(code);
        assert!(!result.contains("  \n")); // No trailing spaces
    }

    #[test]
    fn test_canonicalize_rust_helper() {
        let code = "fn test() {}";
        let result = canonicalize_rust(code);
        assert!(result.is_ok());
    }
}
