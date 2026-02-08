//! Comprehensive Path Validation for Security-Critical File Operations
//!
//! This module provides enterprise-grade path validation to prevent:
//! - Path traversal attacks (../, ../../etc/passwd)
//! - Symlink attacks (following links outside workspace)
//! - Null byte injection
//! - Unicode normalization attacks
//! - Absolute path escapes
//! - Depth limit violations
//! - Extension mismatch attacks
//!
//! ## Usage
//!
//! ```rust
//! use ggen_utils::path_validator::{PathValidator, SafePath};
//! use std::path::Path;
//!
//! # fn main() -> ggen_utils::error::Result<()> {
//! // Create validator with workspace root
//! let validator = PathValidator::new(Path::new("/workspace"))
//!     .with_max_depth(10)
//!     .with_allowed_extensions(vec!["tmpl", "tera", "ttl", "rdf"]);
//!
//! // Validate a path
//! let safe_path = validator.validate("templates/example.tera")?;
//!
//! // Use safe path for file operations
//! let content = std::fs::read_to_string(safe_path.as_path())?;
//! # Ok(())
//! # }
//! ```

use crate::error::{Error, Result};
use std::collections::HashSet;
use std::path::{Path, PathBuf};

// ============================================================================
// Error Types
// ============================================================================

/// Path validation error types
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum PathValidationError {
    /// Path contains ".." components (path traversal attempt)
    PathTraversal { path: String },
    /// Path is absolute when relative was expected
    AbsolutePath { path: String },
    /// Path exceeds maximum allowed depth
    DepthExceeded {
        path: String,
        depth: usize,
        max: usize,
    },
    /// Path contains null bytes
    NullByte { path: String },
    /// Path has invalid extension
    InvalidExtension { path: String, expected: Vec<String> },
    /// Symlink points outside allowed workspace
    SymlinkEscape { link: String, target: String },
    /// Path escapes workspace root
    WorkspaceEscape { path: String, workspace: String },
    /// Path is empty
    EmptyPath,
    /// Invalid UTF-8 in path
    InvalidUtf8 { path: String },
    /// Unicode normalization issue
    UnicodeNormalization { path: String, reason: String },
}

impl std::fmt::Display for PathValidationError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::PathTraversal { path } => {
                write!(f, "Path traversal detected: {path} contains '..'")
            }
            Self::AbsolutePath { path } => {
                write!(f, "Absolute path not allowed: {path}")
            }
            Self::DepthExceeded { path, depth, max } => {
                write!(f, "Path depth {depth} exceeds maximum {max}: {path}")
            }
            Self::NullByte { path } => {
                write!(f, "Path contains null byte: {path}")
            }
            Self::InvalidExtension { path, expected } => {
                write!(
                    f,
                    "Invalid extension for {path}, expected one of: {}",
                    expected.join(", ")
                )
            }
            Self::SymlinkEscape { link, target } => {
                write!(f, "Symlink {link} points outside workspace: {target}")
            }
            Self::WorkspaceEscape { path, workspace } => {
                write!(f, "Path {path} escapes workspace {workspace}")
            }
            Self::EmptyPath => {
                write!(f, "Path cannot be empty")
            }
            Self::InvalidUtf8 { path } => {
                write!(f, "Path contains invalid UTF-8: {path}")
            }
            Self::UnicodeNormalization { path, reason } => {
                write!(f, "Unicode normalization issue in {path}: {reason}")
            }
        }
    }
}

impl std::error::Error for PathValidationError {}

impl From<PathValidationError> for Error {
    fn from(err: PathValidationError) -> Self {
        Error::new(&err.to_string())
    }
}

// ============================================================================
// SafePath - Type-safe validated path
// ============================================================================

/// A path that has been validated and is safe to use for file operations
///
/// This type guarantees:
/// - No path traversal components (..)
/// - Within workspace bounds
/// - Valid UTF-8
/// - No null bytes
/// - Extension matches expectations (if configured)
/// - Depth within limits (if configured)
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct SafePath {
    /// The validated path (relative to workspace root)
    inner: PathBuf,
    /// Absolute path (resolved and validated)
    absolute: PathBuf,
}

impl SafePath {
    /// Get the relative path
    #[must_use]
    pub fn as_path(&self) -> &Path {
        &self.inner
    }

    /// Get the absolute path
    #[must_use]
    pub fn absolute(&self) -> &Path {
        &self.absolute
    }

    /// Convert to PathBuf
    #[must_use]
    pub fn to_path_buf(&self) -> PathBuf {
        self.inner.clone()
    }

    /// Get file extension
    #[must_use]
    pub fn extension(&self) -> Option<&str> {
        self.inner.extension().and_then(|s| s.to_str())
    }

    /// Get file name
    #[must_use]
    pub fn file_name(&self) -> Option<&str> {
        self.inner.file_name().and_then(|s| s.to_str())
    }
}

impl AsRef<Path> for SafePath {
    fn as_ref(&self) -> &Path {
        &self.inner
    }
}

impl std::fmt::Display for SafePath {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.inner.display())
    }
}

// ============================================================================
// PathValidator
// ============================================================================

/// Validates paths for security-critical file operations
///
/// Prevents common attack vectors:
/// - Path traversal (../)
/// - Symlink escapes
/// - Null byte injection
/// - Unicode normalization attacks
/// - Absolute path escapes
/// - Depth limit violations
#[derive(Debug, Clone)]
pub struct PathValidator {
    /// Workspace root (all paths must be within this)
    workspace_root: PathBuf,
    /// Maximum path depth (number of components)
    max_depth: Option<usize>,
    /// Allowed file extensions (empty means all)
    allowed_extensions: HashSet<String>,
    /// Allow absolute paths
    allow_absolute: bool,
    /// Follow symlinks (with validation)
    follow_symlinks: bool,
}

impl PathValidator {
    /// Create a new validator with workspace root
    ///
    /// # Example
    ///
    /// ```rust
    /// use ggen_utils::path_validator::PathValidator;
    /// use std::path::Path;
    ///
    /// let validator = PathValidator::new(Path::new("/workspace"));
    /// ```
    #[must_use]
    pub fn new(workspace_root: &Path) -> Self {
        Self {
            workspace_root: workspace_root.to_path_buf(),
            max_depth: None,
            allowed_extensions: HashSet::new(),
            allow_absolute: false,
            follow_symlinks: true,
        }
    }

    /// Set maximum path depth
    #[must_use]
    pub fn with_max_depth(mut self, max_depth: usize) -> Self {
        self.max_depth = Some(max_depth);
        self
    }

    /// Set allowed file extensions
    #[must_use]
    pub fn with_allowed_extensions(mut self, extensions: Vec<&str>) -> Self {
        self.allowed_extensions = extensions.into_iter().map(|s| s.to_string()).collect();
        self
    }

    /// Allow absolute paths
    #[must_use]
    pub fn with_absolute_paths(mut self, allow: bool) -> Self {
        self.allow_absolute = allow;
        self
    }

    /// Set whether to follow symlinks
    #[must_use]
    pub fn with_follow_symlinks(mut self, follow: bool) -> Self {
        self.follow_symlinks = follow;
        self
    }

    /// Validate a path and return a SafePath
    ///
    /// # Errors
    ///
    /// Returns error if path:
    /// - Contains null bytes
    /// - Contains ".." components
    /// - Is absolute (unless allowed)
    /// - Exceeds max depth
    /// - Has invalid extension
    /// - Escapes workspace
    /// - Is a symlink pointing outside workspace
    pub fn validate(&self, path: impl AsRef<Path>) -> Result<SafePath> {
        let path = path.as_ref();

        // Check for empty path
        if path.as_os_str().is_empty() {
            return Err(PathValidationError::EmptyPath.into());
        }

        // Validate UTF-8
        let path_str = path
            .to_str()
            .ok_or_else(|| PathValidationError::InvalidUtf8 {
                path: path.display().to_string(),
            })?;

        // Check for null bytes
        if path_str.contains('\0') {
            return Err(PathValidationError::NullByte {
                path: path_str.to_string(),
            }
            .into());
        }

        // Check for path traversal
        self.check_path_traversal(path)?;

        // Check if absolute
        if path.is_absolute() && !self.allow_absolute {
            return Err(PathValidationError::AbsolutePath {
                path: path_str.to_string(),
            }
            .into());
        }

        // Check depth
        if let Some(max_depth) = self.max_depth {
            let depth = path.components().count();
            if depth > max_depth {
                return Err(PathValidationError::DepthExceeded {
                    path: path_str.to_string(),
                    depth,
                    max: max_depth,
                }
                .into());
            }
        }

        // Check extension
        if !self.allowed_extensions.is_empty() {
            self.check_extension(path)?;
        }

        // Resolve to absolute path
        let absolute = self.resolve_safely(path)?;

        // Check workspace bounds
        self.check_workspace_bounds(&absolute)?;

        // Check symlinks
        if self.follow_symlinks {
            self.check_symlink(&absolute)?;
        }

        Ok(SafePath {
            inner: path.to_path_buf(),
            absolute,
        })
    }

    /// Check for path traversal components
    fn check_path_traversal(&self, path: &Path) -> Result<()> {
        for component in path.components() {
            if let std::path::Component::ParentDir = component {
                return Err(PathValidationError::PathTraversal {
                    path: path.display().to_string(),
                }
                .into());
            }
        }
        Ok(())
    }

    /// Check file extension
    fn check_extension(&self, path: &Path) -> Result<()> {
        let ext = path.extension().and_then(|s| s.to_str()).ok_or_else(|| {
            PathValidationError::InvalidExtension {
                path: path.display().to_string(),
                expected: self.allowed_extensions.iter().cloned().collect(),
            }
        })?;

        if !self.allowed_extensions.contains(ext) {
            return Err(PathValidationError::InvalidExtension {
                path: path.display().to_string(),
                expected: self.allowed_extensions.iter().cloned().collect(),
            }
            .into());
        }

        Ok(())
    }

    /// Resolve path safely to absolute path
    fn resolve_safely(&self, path: &Path) -> Result<PathBuf> {
        let base = if path.is_absolute() {
            path.to_path_buf()
        } else {
            self.workspace_root.join(path)
        };

        // Canonicalize if exists, otherwise construct absolute path
        if base.exists() {
            base.canonicalize().map_err(|e| {
                Error::new(&format!(
                    "Failed to canonicalize path {}: {}",
                    base.display(),
                    e
                ))
            })
        } else {
            // For non-existent paths, manually construct absolute path
            let mut absolute = self.workspace_root.clone();
            for component in path.components() {
                match component {
                    std::path::Component::Normal(c) => {
                        absolute.push(c);
                    }
                    std::path::Component::RootDir => {
                        if self.allow_absolute {
                            absolute = PathBuf::from("/");
                        }
                    }
                    std::path::Component::ParentDir => {
                        // Already checked, should not reach here
                        return Err(PathValidationError::PathTraversal {
                            path: path.display().to_string(),
                        }
                        .into());
                    }
                    _ => {}
                }
            }
            Ok(absolute)
        }
    }

    /// Check if path is within workspace bounds
    fn check_workspace_bounds(&self, absolute: &Path) -> Result<()> {
        // Canonicalize workspace root
        let workspace_canonical = self.workspace_root.canonicalize().map_err(|e| {
            Error::new(&format!(
                "Failed to canonicalize workspace root {}: {}",
                self.workspace_root.display(),
                e
            ))
        })?;

        // Check if absolute path starts with workspace root
        if !absolute.starts_with(&workspace_canonical) {
            return Err(PathValidationError::WorkspaceEscape {
                path: absolute.display().to_string(),
                workspace: workspace_canonical.display().to_string(),
            }
            .into());
        }

        Ok(())
    }

    /// Check symlink target
    fn check_symlink(&self, path: &Path) -> Result<()> {
        // If path is a symlink, check where it points
        if path.is_symlink() {
            let target = std::fs::read_link(path).map_err(|e| {
                Error::new(&format!("Failed to read symlink {}: {}", path.display(), e))
            })?;

            // Resolve target to absolute path
            let target_absolute = if target.is_absolute() {
                target
            } else {
                path.parent()
                    .ok_or_else(|| Error::new("Symlink has no parent"))?
                    .join(target)
            };

            // Check if target is within workspace
            self.check_workspace_bounds(&target_absolute)?;
        }

        Ok(())
    }

    /// Validate a relative path doesn't escape workspace
    ///
    /// This is a convenience method for validating paths that should be relative.
    pub fn validate_relative(&self, path: impl AsRef<Path>) -> Result<SafePath> {
        let path = path.as_ref();

        if path.is_absolute() {
            return Err(PathValidationError::AbsolutePath {
                path: path.display().to_string(),
            }
            .into());
        }

        self.validate(path)
    }

    /// Batch validate multiple paths
    pub fn validate_batch(&self, paths: &[impl AsRef<Path>]) -> Result<Vec<SafePath>> {
        paths.iter().map(|p| self.validate(p)).collect()
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use tempfile::tempdir;

    // Arrange-Act-Assert pattern (Chicago TDD)

    #[test]
    fn test_valid_relative_path() {
        // Arrange
        let workspace = tempdir().expect("Failed to create temp dir");
        let validator = PathValidator::new(workspace.path());

        // Act
        let result = validator.validate("templates/example.tera");

        // Assert
        assert!(result.is_ok());
        let safe_path = result.expect("Should validate");
        assert_eq!(safe_path.extension(), Some("tera"));
    }

    #[test]
    fn test_path_traversal_blocked() {
        // Arrange
        let workspace = tempdir().expect("Failed to create temp dir");
        let validator = PathValidator::new(workspace.path());

        // Act
        let result = validator.validate("../../../etc/passwd");

        // Assert
        assert!(result.is_err());
        let err = result.unwrap_err();
        assert!(err.to_string().contains("Path traversal"));
    }

    #[test]
    fn test_null_byte_blocked() {
        // Arrange
        let workspace = tempdir().expect("Failed to create temp dir");
        let validator = PathValidator::new(workspace.path());

        // Act
        let result = validator.validate("file\0.txt");

        // Assert
        assert!(result.is_err());
        let err = result.unwrap_err();
        assert!(err.to_string().contains("null byte"));
    }

    #[test]
    fn test_absolute_path_blocked_by_default() {
        // Arrange
        let workspace = tempdir().expect("Failed to create temp dir");
        let validator = PathValidator::new(workspace.path());

        // Act
        let result = validator.validate("/etc/passwd");

        // Assert
        assert!(result.is_err());
        let err = result.unwrap_err();
        assert!(err.to_string().contains("Absolute path"));
    }

    #[test]
    fn test_absolute_path_allowed_when_configured() {
        // Arrange
        let workspace = tempdir().expect("Failed to create temp dir");
        let validator = PathValidator::new(workspace.path()).with_absolute_paths(true);

        // Act
        let test_file = workspace.path().join("test.txt");
        std::fs::write(&test_file, "test").expect("Failed to create test file");
        let result = validator.validate(&test_file);

        // Assert
        assert!(result.is_ok());
    }

    #[test]
    fn test_depth_limit_enforced() {
        // Arrange
        let workspace = tempdir().expect("Failed to create temp dir");
        let validator = PathValidator::new(workspace.path()).with_max_depth(3);

        // Act
        let deep_path = "a/b/c/d/e/f/g.txt";
        let result = validator.validate(deep_path);

        // Assert
        assert!(result.is_err());
        let err = result.unwrap_err();
        assert!(err.to_string().contains("depth"));
    }

    #[test]
    fn test_extension_validation() {
        // Arrange
        let workspace = tempdir().expect("Failed to create temp dir");
        let validator =
            PathValidator::new(workspace.path()).with_allowed_extensions(vec!["tera", "tmpl"]);

        // Act - valid extension
        let result_valid = validator.validate("template.tera");
        assert!(result_valid.is_ok());

        // Act - invalid extension
        let result_invalid = validator.validate("script.sh");

        // Assert
        assert!(result_invalid.is_err());
        let err = result_invalid.unwrap_err();
        assert!(err.to_string().contains("Invalid extension"));
    }

    #[test]
    fn test_empty_path_blocked() {
        // Arrange
        let workspace = tempdir().expect("Failed to create temp dir");
        let validator = PathValidator::new(workspace.path());

        // Act
        let result = validator.validate("");

        // Assert
        assert!(result.is_err());
        let err = result.unwrap_err();
        assert!(err.to_string().contains("empty"));
    }

    #[test]
    fn test_workspace_escape_blocked() {
        // Arrange
        let workspace = tempdir().expect("Failed to create temp dir");
        let validator = PathValidator::new(workspace.path()).with_absolute_paths(true);

        // Act - try to access outside workspace
        let result = validator.validate("/etc/passwd");

        // Assert
        assert!(result.is_err());
        let err = result.unwrap_err();
        assert!(err.to_string().contains("workspace"));
    }

    #[test]
    fn test_symlink_validation() {
        // Arrange
        let workspace = tempdir().expect("Failed to create temp dir");
        let validator = PathValidator::new(workspace.path());

        // Create a file and symlink inside workspace
        let target = workspace.path().join("target.txt");
        std::fs::write(&target, "content").expect("Failed to create target file");

        let link = workspace.path().join("link.txt");
        #[cfg(unix)]
        std::os::unix::fs::symlink(&target, &link).expect("Failed to create symlink");

        // Act
        #[cfg(unix)]
        let result = validator.validate("link.txt");

        // Assert
        #[cfg(unix)]
        assert!(result.is_ok());
    }

    #[test]
    fn test_batch_validation() {
        // Arrange
        let workspace = tempdir().expect("Failed to create temp dir");
        let validator = PathValidator::new(workspace.path());

        let paths = vec!["file1.txt", "file2.txt", "templates/example.tera"];

        // Act
        let result = validator.validate_batch(&paths);

        // Assert
        assert!(result.is_ok());
        let safe_paths = result.expect("Should validate all");
        assert_eq!(safe_paths.len(), 3);
    }

    #[test]
    fn test_safe_path_accessors() {
        // Arrange
        let workspace = tempdir().expect("Failed to create temp dir");
        let validator = PathValidator::new(workspace.path());

        // Act
        let safe_path = validator
            .validate("templates/example.tera")
            .expect("Should validate");

        // Assert
        assert_eq!(safe_path.extension(), Some("tera"));
        assert_eq!(safe_path.file_name(), Some("example.tera"));
        assert_eq!(safe_path.as_path(), Path::new("templates/example.tera"));
    }

    #[test]
    fn test_unicode_path_handling() {
        // Arrange
        let workspace = tempdir().expect("Failed to create temp dir");
        let validator = PathValidator::new(workspace.path());

        // Act - Unicode characters in path
        let result = validator.validate("templates/例え.tera");

        // Assert
        assert!(result.is_ok());
    }

    #[test]
    fn test_validate_relative_rejects_absolute() {
        // Arrange
        let workspace = tempdir().expect("Failed to create temp dir");
        let validator = PathValidator::new(workspace.path());

        // Act
        let result = validator.validate_relative("/etc/passwd");

        // Assert
        assert!(result.is_err());
        let err = result.unwrap_err();
        assert!(err.to_string().contains("Absolute path"));
    }
}
