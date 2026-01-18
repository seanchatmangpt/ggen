//! Pre-flight validation for ggen sync and init commands
//!
//! This module implements comprehensive pre-flight checks to fail early with clear,
//! actionable error messages before beginning expensive operations.
//!
//! ## Constitutional Requirements
//!
//! - Fail fast with helpful error messages
//! - No unwrap/expect in production code
//! - Result<T,E> with proper error context
//! - Clear next steps for user in error messages
//!
//! ## Pre-flight Checks
//!
//! 1. **Disk Space** - Ensure sufficient space for generation (at least 100MB free)
//! 2. **Permissions** - Verify write permissions to output directory
//! 3. **LLM Provider** - Check if Ollama/configured LLM is running and reachable
//! 4. **Manifest Validity** - Parse and validate ggen.toml syntax before execution
//! 5. **Template Syntax** - Quick validate Tera templates before rendering
//! 6. **Dependencies** - Check for required tools (git, etc.)
//!
//! ## Error Codes
//!
//! - E0020: Insufficient disk space
//! - E0021: Insufficient permissions
//! - E0022: LLM provider unreachable
//! - E0023: Manifest syntax error
//! - E0024: Template syntax error
//! - E0025: Missing dependency
//! - E0026: Invalid output directory
//! - E0027: Network connectivity issue
//! - E0028: File system error
//! - E0029: Pre-flight check timeout

use crate::manifest::GgenManifest;
use ggen_utils::error::{Error, Result};
use std::path::{Path, PathBuf};
use std::time::Duration;

// ============================================================================
// Constants
// ============================================================================

/// Minimum required disk space in bytes (100 MB)
const MIN_DISK_SPACE_BYTES: u64 = 100 * 1024 * 1024;

/// Timeout for LLM health checks (5 seconds)
const LLM_HEALTH_CHECK_TIMEOUT_SECS: u64 = 5;

/// Maximum time for all pre-flight checks (30 seconds)
const PREFLIGHT_TIMEOUT_SECS: u64 = 30;

// ============================================================================
// Pre-flight Validator
// ============================================================================

/// Pre-flight validator for ggen operations
///
/// Performs comprehensive checks before beginning sync or init operations
/// to ensure the environment is ready and all prerequisites are met.
pub struct PreFlightValidator {
    /// Base path for relative file operations
    base_path: PathBuf,

    /// Whether to check LLM connectivity (disabled for init, enabled for sync)
    check_llm: bool,

    /// Whether to check template syntax (only for sync with templates)
    check_templates: bool,

    /// Whether to check git (only if needed)
    check_git: bool,
}

impl PreFlightValidator {
    /// Create a new pre-flight validator for sync operations
    pub fn for_sync(base_path: impl Into<PathBuf>) -> Self {
        Self {
            base_path: base_path.into(),
            check_llm: true,
            check_templates: true,
            check_git: false,
        }
    }

    /// Create a new pre-flight validator for init operations
    pub fn for_init(base_path: impl Into<PathBuf>) -> Self {
        Self {
            base_path: base_path.into(),
            check_llm: false,
            check_templates: false,
            check_git: false,
        }
    }

    /// Enable or disable LLM checking
    pub fn with_llm_check(mut self, enabled: bool) -> Self {
        self.check_llm = enabled;
        self
    }

    /// Enable or disable template checking
    pub fn with_template_check(mut self, enabled: bool) -> Self {
        self.check_templates = enabled;
        self
    }

    /// Enable or disable git checking
    pub fn with_git_check(mut self, enabled: bool) -> Self {
        self.check_git = enabled;
        self
    }

    /// Run all pre-flight checks
    ///
    /// Returns Ok(()) if all checks pass, or an Error with a clear message
    /// and next steps if any check fails.
    pub fn validate(&self, manifest: Option<&GgenManifest>) -> Result<PreFlightResult> {
        let start = std::time::Instant::now();
        let mut result = PreFlightResult::default();

        // Check 1: Disk space
        if let Err(e) = self.check_disk_space() {
            result.failures.push(format!("Disk space: {}", e));
        } else {
            result.passed_checks.push("Disk space".to_string());
        }

        // Check 2: Permissions
        if let Err(e) = self.check_permissions() {
            result.failures.push(format!("Permissions: {}", e));
        } else {
            result.passed_checks.push("Permissions".to_string());
        }

        // Check 3: LLM Provider (if enabled)
        if self.check_llm {
            if let Err(e) = self.check_llm_provider() {
                result.warnings.push(format!("LLM provider: {}", e));
            } else {
                result.passed_checks.push("LLM provider".to_string());
            }
        }

        // Check 4: Manifest validity (if provided)
        if let Some(manifest) = manifest {
            if let Err(e) = self.check_manifest_validity(manifest) {
                result.failures.push(format!("Manifest: {}", e));
            } else {
                result.passed_checks.push("Manifest".to_string());
            }
        }

        // Check 5: Template syntax (if enabled and manifest provided)
        if self.check_templates {
            if let Some(manifest) = manifest {
                if let Err(e) = self.check_template_syntax(manifest) {
                    result.failures.push(format!("Templates: {}", e));
                } else {
                    result.passed_checks.push("Templates".to_string());
                }
            }
        }

        // Check 6: Dependencies (if enabled)
        if self.check_git {
            if let Err(e) = self.check_git_installed() {
                result.warnings.push(format!("Git: {}", e));
            } else {
                result.passed_checks.push("Git".to_string());
            }
        }

        // Check timeout
        let elapsed = start.elapsed();
        if elapsed > Duration::from_secs(PREFLIGHT_TIMEOUT_SECS) {
            return Err(Error::new(&format!(
                "error[E0029]: Pre-flight checks timeout\n  |\n  = Checks took {:.2}s (limit: {}s)\n  = help: Some checks may be hanging. Check network connectivity.",
                elapsed.as_secs_f64(),
                PREFLIGHT_TIMEOUT_SECS
            )));
        }

        result.duration_ms = elapsed.as_millis() as u64;

        // Fail if any critical checks failed
        if !result.failures.is_empty() {
            let error_msg = format!(
                "error[E0020]: Pre-flight validation failed\n  |\n  = {} check(s) failed:\n{}\n  = help: Fix the issues above before proceeding",
                result.failures.len(),
                result.failures
                    .iter()
                    .map(|f| format!("    - {}", f))
                    .collect::<Vec<_>>()
                    .join("\n")
            );
            return Err(Error::new(&error_msg));
        }

        Ok(result)
    }

    /// Check 1: Ensure sufficient disk space is available
    fn check_disk_space(&self) -> Result<()> {
        // Get available disk space for the base path
        let available_bytes = get_available_disk_space(&self.base_path)?;

        if available_bytes < MIN_DISK_SPACE_BYTES {
            return Err(Error::new(&format!(
                "error[E0020]: Insufficient disk space\n  --> {}\n  |\n  = Available: {:.2} MB\n  = Required: {:.2} MB\n  = help: Free up at least {:.2} MB of disk space",
                self.base_path.display(),
                available_bytes as f64 / (1024.0 * 1024.0),
                MIN_DISK_SPACE_BYTES as f64 / (1024.0 * 1024.0),
                (MIN_DISK_SPACE_BYTES - available_bytes) as f64 / (1024.0 * 1024.0)
            )));
        }

        Ok(())
    }

    /// Check 2: Verify write permissions to output directory
    fn check_permissions(&self) -> Result<()> {
        // Try to create a test file in the base path
        let test_file = self.base_path.join(".ggen_preflight_test");

        std::fs::write(&test_file, b"test").map_err(|e| {
            Error::new(&format!(
                "error[E0021]: Insufficient permissions\n  --> {}\n  |\n  = Cannot write to directory\n  = Error: {}\n  = help: Check directory permissions or run with appropriate privileges",
                self.base_path.display(),
                e
            ))
        })?;

        // Clean up test file
        let _ = std::fs::remove_file(&test_file);

        Ok(())
    }

    /// Check 3: Verify LLM provider is reachable
    fn check_llm_provider(&self) -> Result<()> {
        // Try to get LLM provider from environment or default to Ollama
        let provider = std::env::var("GGEN_LLM_PROVIDER").unwrap_or_else(|_| "ollama".to_string());

        match provider.as_str() {
            "ollama" => self.check_ollama_health(),
            "openai" => self.check_openai_configured(),
            "anthropic" => self.check_anthropic_configured(),
            "mock" => Ok(()), // Mock provider is always available
            _ => Err(Error::new(&format!(
                "error[E0022]: Unknown LLM provider '{}'\n  |\n  = Supported providers: ollama, openai, anthropic, mock\n  = help: Set GGEN_LLM_PROVIDER to a valid provider",
                provider
            ))),
        }
    }

    /// Check if Ollama is running and reachable
    fn check_ollama_health(&self) -> Result<()> {
        let ollama_url = std::env::var("OLLAMA_BASE_URL")
            .unwrap_or_else(|_| "http://localhost:11434".to_string());

        let health_endpoint = format!("{}/api/tags", ollama_url);

        // Try to connect to Ollama health endpoint
        let client = reqwest::blocking::Client::builder()
            .timeout(Duration::from_secs(LLM_HEALTH_CHECK_TIMEOUT_SECS))
            .build()
            .map_err(|e| {
                Error::new(&format!(
                    "error[E0027]: Network client error\n  |\n  = {}\n  = help: Check network connectivity",
                    e
                ))
            })?;

        client.get(&health_endpoint).send().map_err(|e| {
            Error::new(&format!(
                "error[E0022]: Ollama not reachable\n  --> {}\n  |\n  = Error: {}\n  = help: Start Ollama with 'ollama serve' or set OLLAMA_BASE_URL",
                health_endpoint,
                e
            ))
        })?;

        Ok(())
    }

    /// Check if OpenAI is configured
    fn check_openai_configured(&self) -> Result<()> {
        if std::env::var("OPENAI_API_KEY").is_err() {
            return Err(Error::new(
                "error[E0022]: OpenAI API key not configured\n  |\n  = Environment variable OPENAI_API_KEY is not set\n  = help: Set OPENAI_API_KEY or switch to ollama provider"
            ));
        }
        Ok(())
    }

    /// Check if Anthropic is configured
    fn check_anthropic_configured(&self) -> Result<()> {
        if std::env::var("ANTHROPIC_API_KEY").is_err() {
            return Err(Error::new(
                "error[E0022]: Anthropic API key not configured\n  |\n  = Environment variable ANTHROPIC_API_KEY is not set\n  = help: Set ANTHROPIC_API_KEY or switch to ollama provider"
            ));
        }
        Ok(())
    }

    /// Check 4: Validate manifest structure and required fields
    fn check_manifest_validity(&self, manifest: &GgenManifest) -> Result<()> {
        // Check project name is not empty
        if manifest.project.name.trim().is_empty() {
            return Err(Error::new(
                "error[E0023]: Invalid manifest: project.name cannot be empty\n  |\n  = help: Set a valid project name in ggen.toml"
            ));
        }

        // Check ontology source exists
        let ontology_path = self.base_path.join(&manifest.ontology.source);
        if !ontology_path.exists() {
            return Err(Error::new(&format!(
                "error[E0023]: Ontology file not found\n  --> {}\n  |\n  = Specified in manifest: ontology.source\n  = help: Create the ontology file or update the path in ggen.toml",
                ontology_path.display()
            )));
        }

        // Check generation rules exist
        if manifest.generation.rules.is_empty() {
            return Err(Error::new(
                "error[E0023]: No generation rules defined\n  |\n  = At least one generation rule is required\n  = help: Add a [[generation.rules]] section to ggen.toml"
            ));
        }

        Ok(())
    }

    /// Check 5: Validate template syntax before rendering
    fn check_template_syntax(&self, manifest: &GgenManifest) -> Result<()> {
        use tera::Tera;

        for rule in &manifest.generation.rules {
            // Check if rule uses a template file
            if let crate::manifest::types::TemplateSource::File { file: template_file } = &rule.template {
                let template_path = self.base_path.join(template_file);

                // Check if template file exists
                if !template_path.exists() {
                    return Err(Error::new(&format!(
                        "error[E0024]: Template file not found\n  --> {}\n  |\n  = Rule: {}\n  = help: Create the template file or update the path in ggen.toml",
                        template_path.display(),
                        rule.name
                    )));
                }

                // Try to load and parse template
                let template_content = std::fs::read_to_string(&template_path).map_err(|e| {
                    Error::new(&format!(
                        "error[E0024]: Cannot read template file\n  --> {}\n  |\n  = Rule: {}\n  = Error: {}\n  = help: Check file permissions",
                        template_path.display(),
                        rule.name,
                        e
                    ))
                })?;

                // Parse template syntax
                let mut tera = Tera::default();
                tera.add_raw_template(&rule.name, &template_content)
                    .map_err(|e| {
                        Error::new(&format!(
                            "error[E0024]: Template syntax error\n  --> {}\n  |\n  = Rule: {}\n  = Error: {}\n  = help: Fix template syntax or check Tera documentation",
                            template_path.display(),
                            rule.name,
                            e
                        ))
                    })?;
            }
        }

        Ok(())
    }

    /// Check 6: Verify git is installed (if needed)
    fn check_git_installed(&self) -> Result<()> {
        std::process::Command::new("git")
            .arg("--version")
            .output()
            .map_err(|e| {
                Error::new(&format!(
                    "error[E0025]: Git not found\n  |\n  = Error: {}\n  = help: Install git or ensure it's in PATH",
                    e
                ))
            })?;

        Ok(())
    }
}

// ============================================================================
// Pre-flight Result
// ============================================================================

/// Result of pre-flight validation checks
#[derive(Debug, Clone, Default)]
pub struct PreFlightResult {
    /// Checks that passed
    pub passed_checks: Vec<String>,

    /// Critical failures (will prevent operation)
    pub failures: Vec<String>,

    /// Warnings (won't prevent operation but user should know)
    pub warnings: Vec<String>,

    /// Total duration of checks in milliseconds
    pub duration_ms: u64,
}

impl PreFlightResult {
    /// Check if all validations passed (no failures)
    pub fn is_success(&self) -> bool {
        self.failures.is_empty()
    }

    /// Get total number of checks run
    pub fn total_checks(&self) -> usize {
        self.passed_checks.len() + self.failures.len() + self.warnings.len()
    }
}

// ============================================================================
// Helper Functions
// ============================================================================

/// Get available disk space for a given path
///
/// Returns the available space in bytes, or an error if the check fails.
#[cfg(target_family = "unix")]
fn get_available_disk_space(path: &Path) -> Result<u64> {
    // MetadataExt used for future extensions
    #[allow(unused_imports, dead_code)]
    use std::os::unix::fs::MetadataExt;

    // Get filesystem stats using statvfs
    let path_str = path.to_str().ok_or_else(|| {
        Error::new(&format!(
            "error[E0028]: Invalid path encoding\n  --> {}\n  |\n  = help: Use UTF-8 compatible paths",
            path.display()
        ))
    })?;

    let stats = nix::sys::statvfs::statvfs(path_str).map_err(|e| {
        Error::new(&format!(
            "error[E0028]: Cannot get filesystem stats\n  --> {}\n  |\n  = Error: {}\n  = help: Check if path exists and is accessible",
            path.display(),
            e
        ))
    })?;

    // Available space = block size * available blocks
    let available = stats.block_size() * stats.blocks_available();
    Ok(available)
}

/// Get available disk space for a given path (Windows implementation)
#[cfg(target_family = "windows")]
fn get_available_disk_space(path: &Path) -> Result<u64> {
    use std::ffi::OsStr;
    use std::os::windows::ffi::OsStrExt;

    let wide_path: Vec<u16> = OsStr::new(path)
        .encode_wide()
        .chain(Some(0).into_iter())
        .collect();

    let mut free_bytes: u64 = 0;
    let mut total_bytes: u64 = 0;
    let mut total_free_bytes: u64 = 0;

    let result = unsafe {
        windows::Win32::Storage::FileSystem::GetDiskFreeSpaceExW(
            windows::core::PCWSTR(wide_path.as_ptr()),
            Some(&mut free_bytes),
            Some(&mut total_bytes),
            Some(&mut total_free_bytes),
        )
    };

    if !result.as_bool() {
        return Err(Error::new(&format!(
            "error[E0028]: Cannot get disk space\n  --> {}\n  |\n  = help: Check if path exists and is accessible",
            path.display()
        )));
    }

    Ok(free_bytes)
}

/// Fallback for unsupported platforms - assume sufficient space
#[cfg(not(any(target_family = "unix", target_family = "windows")))]
fn get_available_disk_space(_path: &Path) -> Result<u64> {
    // On unsupported platforms, return a value above the minimum
    Ok(MIN_DISK_SPACE_BYTES * 2)
}

#[cfg(test)]
mod tests {
    use super::*;
    use std::env;

    #[test]
    fn test_validator_creation() {
        let validator = PreFlightValidator::for_sync(".");
        assert!(validator.check_llm);
        assert!(validator.check_templates);
        assert!(!validator.check_git);

        let validator = PreFlightValidator::for_init(".");
        assert!(!validator.check_llm);
        assert!(!validator.check_templates);
        assert!(!validator.check_git);
    }

    #[test]
    fn test_preflight_result_success() {
        let mut result = PreFlightResult::default();
        result.passed_checks.push("Test".to_string());
        assert!(result.is_success());

        result.failures.push("Failure".to_string());
        assert!(!result.is_success());
    }

    #[test]
    fn test_disk_space_check() {
        let validator = PreFlightValidator::for_init(env::temp_dir());
        // Should pass on temp directory (usually has space)
        assert!(validator.check_disk_space().is_ok());
    }

    #[test]
    fn test_permissions_check() {
        let validator = PreFlightValidator::for_init(env::temp_dir());
        // Should pass on temp directory (writable)
        assert!(validator.check_permissions().is_ok());
    }
}
