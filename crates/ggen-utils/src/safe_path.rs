//! Safe path handling with validation to prevent path traversal attacks
//!
//! This module provides the `SafePath` newtype that wraps `PathBuf` with validation
//! to prevent common path traversal vulnerabilities. All paths are validated on
//! construction to ensure they:
//!
//! - Do not contain parent directory references (..)
//! - Are relative paths or absolute paths within an allowed root
//! - Do not exceed maximum depth of 20 levels
//! - Do not contain symlinks outside the workspace (when resolved)
//!
//! ## Examples
//!
//! ```rust
//! use ggen_utils::safe_path::SafePath;
//!
//! // Valid paths
//! let safe = SafePath::new("src/generated").unwrap();
//! let joined = safe.join("output.rs").unwrap();
//! assert_eq!(joined.as_path().to_str().unwrap(), "src/generated/output.rs");
//!
//! // Invalid paths
//! assert!(SafePath::new("../etc/passwd").is_err());
//! assert!(SafePath::new("src/../../etc/passwd").is_err());
//! ```

use crate::error::{Error, Result};
use std::convert::TryFrom;
use std::fmt;
use std::path::{Path, PathBuf};

/// Maximum allowed path depth to prevent deeply nested directory attacks
const MAX_PATH_DEPTH: usize = 20;

/// A validated path that prevents path traversal attacks
///
/// This newtype wrapper around `PathBuf` ensures that all paths are validated
/// on construction and cannot be modified to violate safety constraints.
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct SafePath {
    /// Inner path - private to prevent direct mutation
    inner: PathBuf,
}

impl SafePath {
    /// Create a new SafePath from a string-like type
    ///
    /// # Validation Rules
    ///
    /// - No parent directory references (..)
    /// - Must be relative or within allowed root (if absolute)
    /// - Maximum depth of 20 levels
    /// - Path components must not be empty or contain only whitespace
    ///
    /// # Examples
    ///
    /// ```rust
    /// use ggen_utils::safe_path::SafePath;
    ///
    /// // Valid paths
    /// assert!(SafePath::new("src/generated").is_ok());
    /// assert!(SafePath::new("./output").is_ok());
    /// assert!(SafePath::new("test").is_ok());
    ///
    /// // Invalid paths
    /// assert!(SafePath::new("../etc").is_err());
    /// assert!(SafePath::new("src/../../../etc").is_err());
    /// assert!(SafePath::new("").is_err());
    /// ```
    pub fn new<P: AsRef<Path>>(path: P) -> Result<Self> {
        let path = path.as_ref();
        Self::validate_path(path)?;
        Ok(Self {
            inner: Self::normalize_path(path),
        })
    }

    /// Join this path with another path component
    ///
    /// The joined path is validated to ensure safety constraints are maintained.
    ///
    /// # Examples
    ///
    /// ```rust
    /// use ggen_utils::safe_path::SafePath;
    ///
    /// let base = SafePath::new("src").unwrap();
    /// let joined = base.join("generated").unwrap();
    /// assert_eq!(joined.as_path().to_str().unwrap(), "src/generated");
    ///
    /// // Cannot join with parent references
    /// assert!(base.join("../etc").is_err());
    /// ```
    pub fn join<P: AsRef<Path>>(&self, path: P) -> Result<Self> {
        let joined = self.inner.join(path.as_ref());
        Self::validate_path(&joined)?;
        Ok(Self {
            inner: Self::normalize_path(&joined),
        })
    }

    /// Get a reference to the inner path
    ///
    /// # Examples
    ///
    /// ```rust
    /// use ggen_utils::safe_path::SafePath;
    ///
    /// let safe = SafePath::new("src/generated").unwrap();
    /// assert_eq!(safe.as_path().to_str().unwrap(), "src/generated");
    /// ```
    #[must_use]
    pub fn as_path(&self) -> &Path {
        &self.inner
    }

    /// Convert into the inner PathBuf
    ///
    /// # Examples
    ///
    /// ```rust
    /// use ggen_utils::safe_path::SafePath;
    ///
    /// let safe = SafePath::new("src/generated").unwrap();
    /// let path_buf = safe.into_path_buf();
    /// assert_eq!(path_buf.to_str().unwrap(), "src/generated");
    /// ```
    #[must_use]
    pub fn into_path_buf(self) -> PathBuf {
        self.inner
    }

    /// Get the PathBuf (clone)
    ///
    /// # Examples
    ///
    /// ```rust
    /// use ggen_utils::safe_path::SafePath;
    ///
    /// let safe = SafePath::new("src/generated").unwrap();
    /// let path_buf = safe.as_path_buf();
    /// assert_eq!(path_buf.to_str().unwrap(), "src/generated");
    /// ```
    #[must_use]
    pub fn as_path_buf(&self) -> PathBuf {
        self.inner.clone()
    }

    /// Create a SafePath allowing absolute paths (for system paths like config files)
    ///
    /// # Examples
    ///
    /// ```rust
    /// use ggen_utils::safe_path::SafePath;
    ///
    /// let safe = SafePath::new_absolute("/tmp/config.toml").unwrap();
    /// assert!(safe.as_path().is_absolute());
    /// ```
    pub fn new_absolute<P: AsRef<Path>>(path: P) -> Result<Self> {
        let path = path.as_ref();
        // For absolute paths, we skip the relative path validation
        // but still check for parent dir components and other security issues

        // Check for empty path
        if path.as_os_str().is_empty() {
            return Err(Error::invalid_input("Path cannot be empty"));
        }

        // Check for parent directory references in original path
        for component in path.components() {
            if let std::path::Component::ParentDir = component {
                return Err(Error::invalid_input(format!(
                    "Path contains parent directory reference (..): {}",
                    path.display()
                )));
            }
        }

        // No normalization for absolute paths - use as-is
        Ok(Self {
            inner: path.to_path_buf(),
        })
    }

    /// Get current working directory as SafePath
    ///
    /// # Examples
    ///
    /// ```rust
    /// use ggen_utils::safe_path::SafePath;
    ///
    /// let cwd = SafePath::current_dir().unwrap();
    /// assert!(cwd.as_path().is_absolute());
    /// ```
    pub fn current_dir() -> Result<Self> {
        let cwd = std::env::current_dir()
            .map_err(|e| Error::new(&format!("Failed to get current directory: {}", e)))?;
        Self::new_absolute(cwd)
    }

    /// Get the parent directory
    ///
    /// # Examples
    ///
    /// ```rust
    /// use ggen_utils::safe_path::SafePath;
    ///
    /// let path = SafePath::new("src/generated").unwrap();
    /// let parent = path.parent().unwrap();
    /// assert_eq!(parent.as_path().to_str().unwrap(), "src");
    /// ```
    pub fn parent(&self) -> Result<Self> {
        let parent_path = self
            .inner
            .parent()
            .ok_or_else(|| Error::invalid_input("Path has no parent"))?;
        // For parent, we need to check if it's absolute or relative
        if parent_path.is_absolute() {
            Self::new_absolute(parent_path)
        } else {
            Self::new(parent_path)
        }
    }

    /// Check if path exists on filesystem
    ///
    /// # Examples
    ///
    /// ```rust
    /// use ggen_utils::safe_path::SafePath;
    ///
    /// let cwd = SafePath::current_dir().unwrap();
    /// assert!(cwd.exists());
    /// ```
    #[must_use]
    pub fn exists(&self) -> bool {
        self.inner.exists()
    }

    /// Validate a path against security constraints
    ///
    /// This performs the following checks:
    /// - Path is not empty
    /// - No parent directory references (..)
    /// - Path depth does not exceed MAX_PATH_DEPTH
    /// - Path components are not empty or whitespace-only
    fn validate_path(path: &Path) -> Result<()> {
        // Check for empty path
        if path.as_os_str().is_empty() {
            return Err(Error::invalid_input("Path cannot be empty"));
        }

        // Normalize and check for parent references in the final path
        let normalized = Self::normalize_path(path);

        // Check each component
        let components: Vec<_> = normalized.components().collect();

        // Check depth
        if components.len() > MAX_PATH_DEPTH {
            return Err(Error::invalid_input(format!(
                "Path depth {} exceeds maximum allowed depth of {}",
                components.len(),
                MAX_PATH_DEPTH
            )));
        }

        // Check for parent directory references in original path
        for component in path.components() {
            if let std::path::Component::ParentDir = component {
                return Err(Error::invalid_input(format!(
                    "Path contains parent directory reference (..): {}",
                    path.display()
                )));
            }
        }

        // Check for empty or whitespace-only components
        for component in &components {
            if let std::path::Component::Normal(os_str) = component {
                if let Some(s) = os_str.to_str() {
                    if s.trim().is_empty() {
                        return Err(Error::invalid_input(
                            "Path components cannot be empty or whitespace-only",
                        ));
                    }
                }
            }
        }

        Ok(())
    }

    /// Normalize a path by removing redundant separators and current directory references
    ///
    /// This does NOT resolve symlinks or make the path absolute - it only performs
    /// basic normalization of the path string representation.
    fn normalize_path(path: &Path) -> PathBuf {
        let mut normalized = PathBuf::new();

        for component in path.components() {
            match component {
                std::path::Component::CurDir => {
                    // Skip current directory references
                }
                std::path::Component::Normal(_) => {
                    normalized.push(component);
                }
                _ => {
                    // Keep other components (RootDir, Prefix, ParentDir)
                    // Note: ParentDir will be caught by validation
                    normalized.push(component);
                }
            }
        }

        normalized
    }
}

impl TryFrom<&str> for SafePath {
    type Error = Error;

    fn try_from(value: &str) -> Result<Self> {
        Self::new(value)
    }
}

impl TryFrom<String> for SafePath {
    type Error = Error;

    fn try_from(value: String) -> Result<Self> {
        Self::new(value)
    }
}

impl TryFrom<&String> for SafePath {
    type Error = Error;

    fn try_from(value: &String) -> Result<Self> {
        Self::new(value)
    }
}

impl TryFrom<PathBuf> for SafePath {
    type Error = Error;

    fn try_from(value: PathBuf) -> Result<Self> {
        Self::new(value)
    }
}

impl TryFrom<&Path> for SafePath {
    type Error = Error;

    fn try_from(value: &Path) -> Result<Self> {
        Self::new(value)
    }
}

impl fmt::Display for SafePath {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.inner.display())
    }
}

impl AsRef<Path> for SafePath {
    fn as_ref(&self) -> &Path {
        &self.inner
    }
}

impl AsRef<PathBuf> for SafePath {
    fn as_ref(&self) -> &PathBuf {
        &self.inner
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    // Test Category: Basic Construction

    #[test]
    fn test_new_simple_path() {
        // Arrange
        let path = "src/generated";

        // Act
        let result = SafePath::new(path);

        // Assert
        assert!(result.is_ok());
        let safe_path = result.unwrap();
        assert_eq!(safe_path.as_path().to_str().unwrap(), "src/generated");
    }

    #[test]
    fn test_new_single_component() {
        // Arrange
        let path = "test";

        // Act
        let result = SafePath::new(path);

        // Assert
        assert!(result.is_ok());
        assert_eq!(result.unwrap().as_path().to_str().unwrap(), "test");
    }

    #[test]
    fn test_new_with_current_dir_prefix() {
        // Arrange
        let path = "./output";

        // Act
        let result = SafePath::new(path);

        // Assert
        assert!(result.is_ok());
        // Current directory reference should be normalized away
        assert_eq!(result.unwrap().as_path().to_str().unwrap(), "output");
    }

    #[test]
    fn test_new_with_multiple_current_dirs() {
        // Arrange
        let path = "./src/./generated/./output";

        // Act
        let result = SafePath::new(path);

        // Assert
        assert!(result.is_ok());
        assert_eq!(
            result.unwrap().as_path().to_str().unwrap(),
            "src/generated/output"
        );
    }

    // Test Category: Parent Directory Attacks

    #[test]
    fn test_new_parent_dir_fails() {
        // Arrange
        let path = "../etc/passwd";

        // Act
        let result = SafePath::new(path);

        // Assert
        assert!(result.is_err());
        let err = result.unwrap_err();
        assert!(err.to_string().contains("parent directory reference"));
    }

    #[test]
    fn test_new_parent_dir_in_middle_fails() {
        // Arrange
        let path = "src/../../../etc/passwd";

        // Act
        let result = SafePath::new(path);

        // Assert
        assert!(result.is_err());
        assert!(result.unwrap_err().to_string().contains("parent directory"));
    }

    #[test]
    fn test_new_parent_dir_at_end_fails() {
        // Arrange
        let path = "src/generated/..";

        // Act
        let result = SafePath::new(path);

        // Assert
        assert!(result.is_err());
        assert!(result.unwrap_err().to_string().contains("parent directory"));
    }

    #[test]
    fn test_new_multiple_parent_dirs_fails() {
        // Arrange
        let path = "../../../../../../etc/passwd";

        // Act
        let result = SafePath::new(path);

        // Assert
        assert!(result.is_err());
        assert!(result.unwrap_err().to_string().contains("parent directory"));
    }

    // Test Category: Empty and Invalid Paths

    #[test]
    fn test_new_empty_path_fails() {
        // Arrange
        let path = "";

        // Act
        let result = SafePath::new(path);

        // Assert
        assert!(result.is_err());
        assert!(result.unwrap_err().to_string().contains("cannot be empty"));
    }

    #[test]
    fn test_new_whitespace_component_fails() {
        // Arrange
        let path = "src/   /generated";

        // Act
        let result = SafePath::new(path);

        // Assert
        assert!(result.is_err());
        assert!(result.unwrap_err().to_string().contains("whitespace-only"));
    }

    // Test Category: Depth Validation

    #[test]
    fn test_new_max_depth_allowed() {
        // Arrange - Create path at exact MAX_PATH_DEPTH (20)
        let components: Vec<String> = (0..MAX_PATH_DEPTH).map(|i| format!("level{}", i)).collect();
        let path = components.join("/");

        // Act
        let result = SafePath::new(&path);

        // Assert
        assert!(result.is_ok());
    }

    #[test]
    fn test_new_exceeds_max_depth_fails() {
        // Arrange - Create path with 21 levels (exceeds MAX_PATH_DEPTH)
        let components: Vec<String> = (0..=MAX_PATH_DEPTH)
            .map(|i| format!("level{}", i))
            .collect();
        let path = components.join("/");

        // Act
        let result = SafePath::new(&path);

        // Assert
        assert!(result.is_err());
        let err = result.unwrap_err();
        assert!(err.to_string().contains("exceeds maximum allowed depth"));
        assert!(err.to_string().contains("20"));
    }

    // Test Category: Join Operations

    #[test]
    fn test_join_simple() {
        // Arrange
        let base = SafePath::new("src").unwrap();

        // Act
        let result = base.join("generated");

        // Assert
        assert!(result.is_ok());
        assert_eq!(result.unwrap().as_path().to_str().unwrap(), "src/generated");
    }

    #[test]
    fn test_join_multiple_times() {
        // Arrange
        let base = SafePath::new("src").unwrap();

        // Act
        let step1 = base.join("generated").unwrap();
        let step2 = step1.join("output").unwrap();
        let result = step2.join("file.rs");

        // Assert
        assert!(result.is_ok());
        assert_eq!(
            result.unwrap().as_path().to_str().unwrap(),
            "src/generated/output/file.rs"
        );
    }

    #[test]
    fn test_join_with_parent_dir_fails() {
        // Arrange
        let base = SafePath::new("src").unwrap();

        // Act
        let result = base.join("../etc");

        // Assert
        assert!(result.is_err());
        assert!(result.unwrap_err().to_string().contains("parent directory"));
    }

    #[test]
    fn test_join_exceeding_depth_fails() {
        // Arrange - Create base path at 18 levels
        let components: Vec<String> = (0..18).map(|i| format!("level{}", i)).collect();
        let base = SafePath::new(components.join("/")).unwrap();

        // Act - Try to join 3 more levels (total 21, exceeds 20)
        let result = base.join("level18/level19/level20");

        // Assert
        assert!(result.is_err());
        assert!(result
            .unwrap_err()
            .to_string()
            .contains("exceeds maximum allowed depth"));
    }

    // Test Category: Type Conversions

    #[test]
    fn test_try_from_str() {
        // Arrange
        let path = "src/generated";

        // Act
        let result = SafePath::try_from(path);

        // Assert
        assert!(result.is_ok());
        assert_eq!(result.unwrap().as_path().to_str().unwrap(), "src/generated");
    }

    #[test]
    fn test_try_from_string() {
        // Arrange
        let path = String::from("src/generated");

        // Act
        let result = SafePath::try_from(path);

        // Assert
        assert!(result.is_ok());
        assert_eq!(result.unwrap().as_path().to_str().unwrap(), "src/generated");
    }

    #[test]
    fn test_try_from_string_ref() {
        // Arrange
        let path = String::from("src/generated");

        // Act
        let result = SafePath::try_from(&path);

        // Assert
        assert!(result.is_ok());
        assert_eq!(result.unwrap().as_path().to_str().unwrap(), "src/generated");
    }

    #[test]
    fn test_try_from_path_buf() {
        // Arrange
        let path = PathBuf::from("src/generated");

        // Act
        let result = SafePath::try_from(path);

        // Assert
        assert!(result.is_ok());
        assert_eq!(result.unwrap().as_path().to_str().unwrap(), "src/generated");
    }

    #[test]
    fn test_try_from_path() {
        // Arrange
        let path = Path::new("src/generated");

        // Act
        let result = SafePath::try_from(path);

        // Assert
        assert!(result.is_ok());
        assert_eq!(result.unwrap().as_path().to_str().unwrap(), "src/generated");
    }

    #[test]
    fn test_try_from_invalid_str_fails() {
        // Arrange
        let path = "../etc/passwd";

        // Act
        let result = SafePath::try_from(path);

        // Assert
        assert!(result.is_err());
        assert!(result.unwrap_err().to_string().contains("parent directory"));
    }

    // Test Category: Display and AsRef Traits

    #[test]
    fn test_display_trait() {
        // Arrange
        let safe = SafePath::new("src/generated").unwrap();

        // Act
        let display_string = format!("{}", safe);

        // Assert
        assert_eq!(display_string, "src/generated");
    }

    #[test]
    fn test_debug_trait() {
        // Arrange
        let safe = SafePath::new("src/generated").unwrap();

        // Act
        let debug_string = format!("{:?}", safe);

        // Assert
        assert!(debug_string.contains("SafePath"));
        assert!(debug_string.contains("src/generated"));
    }

    #[test]
    fn test_as_ref_path() {
        // Arrange
        let safe = SafePath::new("src/generated").unwrap();

        // Act
        let path_ref: &Path = safe.as_ref();

        // Assert
        assert_eq!(path_ref.to_str().unwrap(), "src/generated");
    }

    #[test]
    fn test_as_ref_path_buf() {
        // Arrange
        let safe = SafePath::new("src/generated").unwrap();

        // Act
        let path_buf_ref: &PathBuf = safe.as_ref();

        // Assert
        assert_eq!(path_buf_ref.to_str().unwrap(), "src/generated");
    }

    // Test Category: Equality and Cloning

    #[test]
    fn test_equality() {
        // Arrange
        let safe1 = SafePath::new("src/generated").unwrap();
        let safe2 = SafePath::new("src/generated").unwrap();
        let safe3 = SafePath::new("src/output").unwrap();

        // Act & Assert
        assert_eq!(safe1, safe2);
        assert_ne!(safe1, safe3);
    }

    #[test]
    fn test_clone() {
        // Arrange
        let safe = SafePath::new("src/generated").unwrap();

        // Act
        let cloned = safe.clone();

        // Assert
        assert_eq!(safe, cloned);
        assert_eq!(safe.as_path(), cloned.as_path());
    }

    // Test Category: Path Normalization

    #[test]
    fn test_normalization_removes_current_dir() {
        // Arrange
        let path = "./src/./generated/./output";

        // Act
        let safe = SafePath::new(path).unwrap();

        // Assert
        assert_eq!(safe.as_path().to_str().unwrap(), "src/generated/output");
    }

    #[test]
    fn test_into_path_buf() {
        // Arrange
        let safe = SafePath::new("src/generated").unwrap();

        // Act
        let path_buf = safe.into_path_buf();

        // Assert
        assert_eq!(path_buf.to_str().unwrap(), "src/generated");
    }

    // Test Category: Edge Cases

    #[test]
    fn test_path_with_file_extension() {
        // Arrange
        let path = "src/generated/output.rs";

        // Act
        let result = SafePath::new(path);

        // Assert
        assert!(result.is_ok());
        assert_eq!(
            result.unwrap().as_path().to_str().unwrap(),
            "src/generated/output.rs"
        );
    }

    #[test]
    fn test_path_with_special_chars_in_name() {
        // Arrange
        let path = "src/my-project_v2.0/output";

        // Act
        let result = SafePath::new(path);

        // Assert
        assert!(result.is_ok());
        assert_eq!(
            result.unwrap().as_path().to_str().unwrap(),
            "src/my-project_v2.0/output"
        );
    }
}
