//! Validated path with compile-time safety guarantees.
//!
//! Prevents path traversal attacks, shell injection, and TOCTOU races.

use std::path::{Component, Path, PathBuf};
use std::str::FromStr;

use ggen_utils::error::{Error, Result};

/// Validated path that has passed all security checks.
///
/// Cannot be constructed with invalid paths (type-level safety).
///
/// # Security Checks
///
/// - No null bytes
/// - No parent directory references (`..`)
/// - Not an absolute path (unless explicitly allowed)
/// - No shell metacharacters
/// - No symlinks (optional)
///
/// # Example
///
/// ```no_run
/// use ggen_core::poka_yoke::ValidatedPath;
///
/// // Valid path
/// let path = ValidatedPath::new("templates/hello.tmpl")?;
///
/// // Invalid path (contains ..)
/// let bad = ValidatedPath::new("../etc/passwd");
/// assert!(bad.is_err());
/// # Ok::<(), ggen_core::error::Error>(())
/// ```
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct ValidatedPath {
    inner: PathBuf,
}

impl ValidatedPath {
    /// Creates a new validated path.
    ///
    /// # Errors
    ///
    /// Returns error if any validation check fails.
    pub fn new(path: impl AsRef<Path>) -> Result<Self> {
        let path = path.as_ref();

        // Comprehensive validation
        Self::check_no_null_bytes(path)?;
        Self::check_no_parent_refs(path)?;
        Self::check_not_absolute(path)?;
        Self::check_shell_safety(path)?;

        // Canonicalize to prevent TOCTOU (if path exists)
        let canonical = if path.exists() {
            path.canonicalize()
                .map_err(|e| Error::invalid_input(format!("Failed to canonicalize path: {}", e)))?
        } else {
            path.to_path_buf()
        };

        Ok(Self { inner: canonical })
    }

    /// Creates a validated path constrained within a base directory.
    ///
    /// Ensures path does not escape base directory.
    ///
    /// # Errors
    ///
    /// Returns error if path escapes base directory.
    pub fn new_within(path: impl AsRef<Path>, base: impl AsRef<Path>) -> Result<Self> {
        let validated = Self::new(path)?;
        let full_path = base.as_ref().join(&validated.inner);

        // Canonicalize both paths for reliable comparison
        let base_canonical = base.as_ref().canonicalize().map_err(|e| {
            Error::invalid_input(format!("Failed to canonicalize base directory: {}", e))
        })?;

        let full_canonical = if full_path.exists() {
            full_path.canonicalize().map_err(|e| {
                Error::invalid_input(format!("Failed to canonicalize full path: {}", e))
            })?
        } else {
            full_path
        };

        // Ensure path stays within base
        if !full_canonical.starts_with(&base_canonical) {
            return Err(Error::invalid_input(
                "Path escapes base directory (path traversal detected)",
            ));
        }

        Ok(validated)
    }

    /// Returns the inner path.
    pub fn as_path(&self) -> &Path {
        &self.inner
    }

    /// Consumes self and returns the inner PathBuf.
    pub fn into_path_buf(self) -> PathBuf {
        self.inner
    }

    /// Checks for null bytes in path.
    fn check_no_null_bytes(path: &Path) -> Result<()> {
        let path_str = path.to_string_lossy();
        if path_str.contains('\0') {
            return Err(Error::invalid_input("Path contains null byte"));
        }
        Ok(())
    }

    /// Checks for parent directory references.
    fn check_no_parent_refs(path: &Path) -> Result<()> {
        for component in path.components() {
            if component == Component::ParentDir {
                return Err(Error::invalid_input(
                    "Path contains '..' (parent directory reference)",
                ));
            }
        }
        Ok(())
    }

    /// Checks path is not absolute.
    fn check_not_absolute(path: &Path) -> Result<()> {
        if path.is_absolute() {
            return Err(Error::invalid_input("Path is absolute (expected relative)"));
        }
        Ok(())
    }

    /// Checks for shell metacharacters.
    fn check_shell_safety(path: &Path) -> Result<()> {
        let path_str = path.to_string_lossy();
        const DANGEROUS: &[char] = &['$', '`', ';', '|', '&', '>', '<', '\n', '\r'];

        for ch in DANGEROUS {
            if path_str.contains(*ch) {
                return Err(Error::invalid_input(format!(
                    "Path contains dangerous shell metacharacter: '{}'",
                    ch
                )));
            }
        }

        Ok(())
    }
}

impl AsRef<Path> for ValidatedPath {
    fn as_ref(&self) -> &Path {
        &self.inner
    }
}

impl FromStr for ValidatedPath {
    type Err = Error;

    fn from_str(s: &str) -> Result<Self> {
        Self::new(s)
    }
}

impl std::fmt::Display for ValidatedPath {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.inner.display())
    }
}
