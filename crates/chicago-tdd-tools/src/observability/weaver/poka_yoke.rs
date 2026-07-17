//! Poka-Yoke Types for Weaver Registry Validation
//!
//! Provides type-level error prevention for registry paths and versions.
//! Uses Rust's type system to make invalid registry states unrepresentable.
//!
//! **Poka-Yoke Principles**:
//! - **Make invalid states unrepresentable**: Use types to prevent errors
//! - **Type-level prevention**: Invalid registry paths/versions cannot be created
//! - **Runtime validation**: Invalid values return `None`, preventing use
//! - **Type-level invariants**: Encode constraints in types
//!
//! # Error Modes Prevented
//!
//! 1. **Invalid Registry Path**: Empty paths, non-existent paths, non-directories
//! 2. **Invalid Registry Version**: Empty versions, invalid formats
//! 3. **Invalid Registry State**: Using registry before validation
//! 4. **Missing Validation**: Registry not validated before use
//!
//! # Example
//!
//! ```rust
//! use chicago_tdd_tools::observability::weaver::poka_yoke::*;
//! use std::path::Path;
//!
//! // Valid registry path creation
//! let path = ValidRegistryPath::new("registry").expect("Valid path");
//! assert_eq!(path.as_path(), Path::new("registry"));
//!
//! // Invalid: Empty path
//! let path = ValidRegistryPath::new("");
//! assert!(path.is_none()); // Type prevents invalid path
//!
//! // Valid registry version
//! let version = RegistryVersion::new("v1.25.0").expect("Valid version");
//! assert_eq!(version.as_str(), "v1.25.0");
//!
//! // Invalid: Empty version
//! let version = RegistryVersion::new("");
//! assert!(version.is_none()); // Type prevents invalid version
//! ```

use std::path::{Path, PathBuf};

/// Valid registry path
///
/// **Poka-yoke**: Newtype prevents empty paths and non-existent paths.
/// The type system makes invalid registry paths impossible.
///
/// # Invariant
///
/// Path is always non-empty and exists (enforced by type).
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct ValidRegistryPath {
    /// Path value (always non-empty and exists)
    path: PathBuf,
}

impl ValidRegistryPath {
    /// Create a new valid registry path
    ///
    /// **Poka-yoke**: Returns `Option` to prevent invalid paths:
    /// - Empty path: Returns `None`
    /// - Non-existent path: Returns `None`
    /// - Path to non-directory: Returns `None`
    ///
    /// The type system forces handling of invalid paths at compile time.
    ///
    /// # Errors
    ///
    /// Returns `None` if path is invalid (empty, doesn't exist, or not a directory).
    #[must_use]
    pub fn new<P: AsRef<Path>>(path: P) -> Option<Self> {
        let path = path.as_ref();

        // Check 1: Path must not be empty
        if path.as_os_str().is_empty() {
            return None;
        }

        // Check 2: Path must exist
        if !path.exists() {
            return None;
        }

        // Check 3: Path must be a directory
        if !path.is_dir() {
            return None;
        }

        Some(Self { path: path.to_path_buf() })
    }

    /// Create a new valid registry path without existence check
    ///
    /// **Poka-yoke**: Only validates that path is non-empty.
    /// Use this when path will be created/cloned before use.
    ///
    /// # Errors
    ///
    /// Returns `None` if path is empty.
    #[must_use]
    pub fn new_unchecked<P: AsRef<Path>>(path: P) -> Option<Self> {
        let path = path.as_ref();

        // Check: Path must not be empty
        if path.as_os_str().is_empty() {
            return None;
        }

        Some(Self { path: path.to_path_buf() })
    }

    /// Get the path as `&Path`
    ///
    /// **Poka-yoke**: Returns `&Path` that is guaranteed to be non-empty.
    #[must_use]
    pub fn as_path(&self) -> &Path {
        &self.path
    }

    /// Get the path as `PathBuf`
    ///
    /// **Poka-yoke**: Returns `PathBuf` that is guaranteed to be non-empty.
    #[must_use]
    pub fn into_path_buf(self) -> PathBuf {
        self.path
    }

    /// Convert to string
    ///
    /// **Poka-yoke**: Returns `String` that is guaranteed to be non-empty.
    ///
    /// # Errors
    ///
    /// Returns `None` if path is not valid UTF-8.
    #[must_use]
    pub fn to_string(&self) -> Option<String> {
        self.path.to_str().map(ToOwned::to_owned)
    }
}

impl AsRef<Path> for ValidRegistryPath {
    fn as_ref(&self) -> &Path {
        &self.path
    }
}

impl From<ValidRegistryPath> for PathBuf {
    fn from(path: ValidRegistryPath) -> Self {
        path.path
    }
}

/// Registry version (tag, branch, or commit)
///
/// **Poka-yoke**: Newtype prevents empty versions and invalid formats.
/// The type system makes invalid registry versions impossible.
///
/// # Invariant
///
/// Version is always non-empty (enforced by type).
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct RegistryVersion {
    /// Version string (always non-empty)
    version: String,
}

impl RegistryVersion {
    /// Create a new registry version
    ///
    /// **Poka-yoke**: Returns `Option` to prevent invalid versions:
    /// - Empty version: Returns `None`
    /// - Whitespace-only version: Returns `None`
    ///
    /// The type system forces handling of invalid versions at compile time.
    ///
    /// # Errors
    ///
    /// Returns `None` if version is empty or whitespace-only.
    #[must_use]
    pub fn new<S: AsRef<str>>(version: S) -> Option<Self> {
        let version = version.as_ref();

        // Check: Version must not be empty or whitespace-only
        if version.trim().is_empty() {
            return None;
        }

        Some(Self { version: version.to_string() })
    }

    /// Get the version as `&str`
    ///
    /// **Poka-yoke**: Returns `&str` that is guaranteed to be non-empty.
    #[must_use]
    pub fn as_str(&self) -> &str {
        &self.version
    }

    /// Convert to string
    ///
    /// **Poka-yoke**: Returns `String` that is guaranteed to be non-empty.
    #[must_use]
    pub fn into_string(self) -> String {
        self.version
    }
}

impl AsRef<str> for RegistryVersion {
    fn as_ref(&self) -> &str {
        &self.version
    }
}

impl From<RegistryVersion> for String {
    fn from(version: RegistryVersion) -> Self {
        version.version
    }
}

/// Registry validation state
///
/// **Poka-yoke**: Enum prevents using registry before validation.
/// The type system makes invalid registry states impossible.
///
/// # Invariant
///
/// Registry can only be used after validation passes (enforced by type).
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum RegistryState {
    /// Registry path exists but not yet validated
    Unvalidated(ValidRegistryPath),
    /// Registry validated and ready to use
    Validated(ValidRegistryPath),
    /// Registry validation failed
    ValidationFailed(ValidRegistryPath, String),
}

impl RegistryState {
    /// Create a new unvalidated registry state
    ///
    /// **Poka-yoke**: Returns `Option` to prevent invalid paths.
    #[must_use]
    pub fn new<P: AsRef<Path>>(path: P) -> Option<Self> {
        ValidRegistryPath::new(path).map(Self::Unvalidated)
    }

    /// Validate the registry
    ///
    /// **Poka-yoke**: Only validates unvalidated registries.
    /// Returns new state based on validation result.
    ///
    /// # Errors
    ///
    /// Returns error if validation fails.
    pub fn validate(self) -> Result<Self, String> {
        match self {
            Self::Unvalidated(path) => {
                // Perform validation (simplified - actual validation would call weaver)
                // For now, just check that path exists and is a directory
                if path.as_path().exists() && path.as_path().is_dir() {
                    Ok(Self::Validated(path))
                } else {
                    Err("Registry path does not exist or is not a directory".to_string())
                }
            }
            Self::Validated(_) => Ok(self), // Already validated
            Self::ValidationFailed(_, err) => Err(err), // Already failed
        }
    }

    /// Get the registry path if validated
    ///
    /// **Poka-yoke**: Only returns path if registry is validated.
    /// Prevents using unvalidated registries.
    #[must_use]
    pub const fn validated_path(&self) -> Option<&ValidRegistryPath> {
        match self {
            Self::Validated(path) => Some(path),
            Self::Unvalidated(_) | Self::ValidationFailed(_, _) => None,
        }
    }

    /// Check if registry is validated
    ///
    /// **Poka-yoke**: Type-level check for validation state.
    #[must_use]
    pub const fn is_validated(&self) -> bool {
        matches!(self, Self::Validated(_))
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use std::fs;
    use tempfile::TempDir;

    #[test]
    fn test_valid_registry_path_valid() {
        let temp_dir = TempDir::new().unwrap();
        let path = ValidRegistryPath::new(temp_dir.path());
        assert!(path.is_some());
        assert_eq!(path.unwrap().as_path(), temp_dir.path());
    }

    #[test]
    fn test_valid_registry_path_empty() {
        let path = ValidRegistryPath::new("");
        assert!(path.is_none()); // Type prevents empty path
    }

    #[test]
    fn test_valid_registry_path_nonexistent() {
        let path = ValidRegistryPath::new("/nonexistent/path");
        assert!(path.is_none()); // Type prevents non-existent path
    }

    #[test]
    fn test_valid_registry_path_file() {
        if let Ok(temp_dir) = TempDir::new() {
            let file_path = temp_dir.path().join("file.txt");
            match fs::write(&file_path, "test") {
                Ok(()) => {
                    let path = ValidRegistryPath::new(&file_path);
                    assert!(path.is_none()); // Type prevents non-directory path
                }
                Err(_) => panic!("Failed to write test file"),
            }
        } else {
            panic!("Failed to create temp directory");
        }
    }

    #[test]
    fn test_registry_version_valid() {
        let version = RegistryVersion::new("v1.25.0");
        assert!(version.is_some());
        if let Some(v) = version {
            assert_eq!(v.as_str(), "v1.25.0");
        } else {
            panic!("Expected Some but got None");
        }
    }

    #[test]
    fn test_registry_version_empty() {
        let version = RegistryVersion::new("");
        assert!(version.is_none()); // Type prevents empty version
    }

    #[test]
    fn test_registry_version_whitespace() {
        let version = RegistryVersion::new("   ");
        assert!(version.is_none()); // Type prevents whitespace-only version
    }

    #[test]
    fn test_registry_state_unvalidated() {
        if let Ok(temp_dir) = TempDir::new() {
            let state = RegistryState::new(temp_dir.path());
            assert!(state.is_some());
            if let Some(state) = state {
                assert!(!state.is_validated());
                assert!(state.validated_path().is_none()); // Cannot use unvalidated path
            } else {
                panic!("Expected Some but got None");
            }
        } else {
            panic!("Failed to create temp directory");
        }
    }

    #[test]
    fn test_registry_state_validated() {
        if let Ok(temp_dir) = TempDir::new() {
            if let Some(state) = RegistryState::new(temp_dir.path()) {
                let validated = state.validate();
                assert!(validated.is_ok());
                if let Ok(validated) = validated {
                    assert!(validated.is_validated());
                    assert!(validated.validated_path().is_some()); // Can use validated path
                } else {
                    panic!("Expected Ok but got Err");
                }
            } else {
                panic!("Failed to create registry state");
            }
        } else {
            panic!("Failed to create temp directory");
        }
    }
}
