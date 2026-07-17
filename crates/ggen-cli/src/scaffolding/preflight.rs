//! Pre-flight validation for `ggen init`.
//!
//! Ported (as a focused equivalent, not a byte-for-byte copy) from
//! `ggen-core/src/validation/preflight.rs`'s `PreFlightValidator` during the ggen-core
//! retirement migration (`specs/014-ggen-core-replacement`, Phase 3 "non-colliding noun
//! re-points").
//!
//! ## Why this is a focused port, not a full port (documented per task instructions)
//!
//! The original `PreFlightValidator` is a dual-purpose type shared by `ggen sync`
//! (`PreFlightValidator::for_sync`) and `ggen init` (`PreFlightValidator::for_init`). Its
//! `validate()` gates six checks behind `self.check_llm` / `self.check_templates` /
//! `self.check_git` / `Option<&GgenManifest>`. `cmds/init.rs` calls only
//! `PreFlightValidator::for_init(base_path).validate(None)`, and `for_init` sets
//! `check_llm = false`, `check_templates = false`, `check_git = false` — so on the init path
//! four of the six checks (LLM provider reachability via `reqwest::blocking`, manifest
//! validity, Tera template syntax, git-installed) are permanently unreachable dead code, and
//! porting them would additionally require pulling in `reqwest`'s `blocking` feature and a
//! `GgenManifest` type for logic that never executes from `ggen init`. Per the task's
//! documented fallback ("extract just init's slice ... your judgment call, document which you
//! did and why"), this file ports only the two checks that are ever reachable from init:
//! disk-space and write-permission checks. Both are byte-for-byte behavior-identical to the
//! ggen-core originals (same error codes/messages, same `nix`/`windows` platform split for
//! available-disk-space).
//!
//! `sync.rs`/other ggen-core callers of the sync-oriented `PreFlightValidator::for_sync` are
//! untouched — ggen-core stays on disk and continues to serve them.

use crate::utils::error::{Error, Result};
use std::path::{Path, PathBuf};
use std::time::Duration;

/// Minimum required disk space in bytes (100 MB) — same threshold as the ggen-core original.
const MIN_DISK_SPACE_BYTES: u64 = 100 * 1024 * 1024;

/// Maximum time for all pre-flight checks (30 seconds) — same threshold as the ggen-core original.
const PREFLIGHT_TIMEOUT_SECS: u64 = 30;

/// Pre-flight validator for `ggen init`.
///
/// Runs the two checks that are reachable on the init path: disk space and write
/// permissions. See module docs for why this is a focused port of the ggen-core original
/// rather than a full one.
pub struct PreFlightValidator {
    /// Base path for relative file operations
    base_path: PathBuf,
}

impl PreFlightValidator {
    /// Create a new pre-flight validator for init operations
    pub fn for_init(base_path: impl Into<PathBuf>) -> Self {
        Self {
            base_path: base_path.into(),
        }
    }

    /// Run pre-flight checks (disk space + permissions).
    ///
    /// Returns `Ok(PreFlightResult)` if all checks pass, or an `Error` with a clear message
    /// and next steps if any check fails.
    pub fn validate(&self) -> Result<PreFlightResult> {
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
}

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

/// Get available disk space for a given path (Unix implementation)
#[cfg(target_family = "unix")]
fn get_available_disk_space(path: &Path) -> Result<u64> {
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
    let available = stats.block_size() * stats.blocks_available() as u64;
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
    Ok(MIN_DISK_SPACE_BYTES * 2)
}

#[cfg(test)]
mod tests {
    use super::*;
    use std::env;

    #[test]
    fn test_validator_creation() {
        let validator = PreFlightValidator::for_init(".");
        assert_eq!(validator.base_path, PathBuf::from("."));
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

    #[test]
    fn test_validate_on_writable_temp_dir() {
        let validator = PreFlightValidator::for_init(env::temp_dir());
        let result = validator.validate().unwrap();
        assert!(result.is_success());
        assert_eq!(result.total_checks(), 2);
    }

    #[test]
    fn test_validate_fails_on_missing_dir() {
        let missing = env::temp_dir().join("ggen_preflight_test_missing_dir_xyz_does_not_exist");
        let validator = PreFlightValidator::for_init(missing);
        // Missing directory: fs::write for the permission probe fails (no such directory).
        assert!(validator.validate().is_err());
    }
}
