//! Poka-yoke (error prevention) types for marketplace
//!
//! This module provides type-level guarantees that prevent entire classes of errors
//! at compile time. Types make invalid states unrepresentable.

use ggen_utils::error::{Error, Result};
use serde::{Deserialize, Serialize};
use std::fmt;

/// Validated package name that prevents invalid names at compile time
///
/// **Poka-yoke**: Package names are validated at construction, making invalid names
/// impossible to represent. This prevents injection attacks, path traversal, and
/// invalid characters at the type level.
///
/// # Invariants
/// - Non-empty (length > 0)
/// - Max length 100 characters
/// - Only alphanumeric, hyphens, underscores, and slashes
/// - No path traversal sequences (..)
/// - No control characters
#[derive(Debug, Clone, PartialEq, Eq, Hash, Serialize, Deserialize)]
#[serde(try_from = "String", into = "String")]
pub struct ValidatedPackageName(String);

impl ValidatedPackageName {
    /// Create a validated package name
    ///
    /// # Errors
    /// Returns error if name is invalid (empty, too long, invalid characters, etc.)
    pub fn new(name: impl Into<String>) -> Result<Self> {
        let name = name.into();

        // Validate non-empty
        if name.is_empty() {
            return Err(Error::new("Package name cannot be empty"));
        }

        // Validate max length
        if name.len() > 100 {
            return Err(Error::new("Package name too long (max 100 chars)"));
        }

        // Validate characters: alphanumeric, hyphens, underscores, slashes
        if !name
            .chars()
            .all(|c| c.is_alphanumeric() || c == '-' || c == '_' || c == '/')
        {
            return Err(Error::new(
                "Package name must contain only alphanumeric characters, hyphens, underscores, and slashes",
            ));
        }

        // Validate no path traversal
        if name.contains("..") {
            return Err(Error::new(
                "Package name cannot contain path traversal sequences (..)",
            ));
        }

        // Validate no control characters
        if name.chars().any(|c| c.is_control()) {
            return Err(Error::new("Package name cannot contain control characters"));
        }

        Ok(Self(name))
    }

    /// Get the inner string (guaranteed valid)
    pub fn as_str(&self) -> &str {
        &self.0
    }
}

impl TryFrom<String> for ValidatedPackageName {
    type Error = Error;

    fn try_from(name: String) -> Result<Self> {
        Self::new(name)
    }
}

impl From<ValidatedPackageName> for String {
    fn from(name: ValidatedPackageName) -> Self {
        name.0
    }
}

impl fmt::Display for ValidatedPackageName {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.0)
    }
}

impl AsRef<str> for ValidatedPackageName {
    fn as_ref(&self) -> &str {
        &self.0
    }
}

/// Semantic version that enforces valid semver format
///
/// **Poka-yoke**: Versions are validated at construction, making invalid versions
/// impossible to represent. This prevents version parsing errors and invalid formats.
///
/// # Invariants
/// - Non-empty
/// - Max length 50 characters
/// - Format: X.Y.Z where X, Y, Z are numeric
/// - At least 3 components (major.minor.patch)
#[derive(Debug, Clone, PartialEq, Eq, Hash, Serialize, Deserialize)]
#[serde(try_from = "String", into = "String")]
pub struct SemanticVersion(String);

impl SemanticVersion {
    /// Create a validated semantic version
    ///
    /// # Errors
    /// Returns error if version is invalid (empty, wrong format, non-numeric components)
    pub fn new(version: impl Into<String>) -> Result<Self> {
        let version = version.into();

        // Validate non-empty
        if version.is_empty() {
            return Err(Error::new("Package version is required"));
        }

        // Validate max length
        if version.len() > 50 {
            return Err(Error::new("Package version must be 50 characters or less"));
        }

        // Validate semver format: X.Y.Z
        let parts: Vec<&str> = version.split('.').collect();
        if parts.len() < 3 {
            return Err(Error::new(&format!(
                "Package version '{}' must follow semantic versioning (e.g., 1.0.0)",
                version
            )));
        }

        // Validate each component is numeric
        for (idx, part) in parts.iter().enumerate() {
            if part.parse::<u32>().is_err() {
                return Err(Error::new(&format!(
                    "Invalid version component '{}' at position {}. Expected numeric value",
                    part, idx
                )));
            }
        }

        Ok(Self(version))
    }

    /// Get the inner string (guaranteed valid)
    pub fn as_str(&self) -> &str {
        &self.0
    }

    /// Get major version component
    pub fn major(&self) -> Result<u32> {
        self.0
            .split('.')
            .next()
            .and_then(|s| s.parse().ok())
            .ok_or_else(|| Error::new("Invalid major version"))
    }

    /// Get minor version component
    pub fn minor(&self) -> Result<u32> {
        self.0
            .split('.')
            .nth(1)
            .and_then(|s| s.parse().ok())
            .ok_or_else(|| Error::new("Invalid minor version"))
    }

    /// Get patch version component
    pub fn patch(&self) -> Result<u32> {
        self.0
            .split('.')
            .nth(2)
            .and_then(|s| s.parse().ok())
            .ok_or_else(|| Error::new("Invalid patch version"))
    }
}

impl TryFrom<String> for SemanticVersion {
    type Error = Error;

    fn try_from(version: String) -> Result<Self> {
        Self::new(version)
    }
}

impl From<SemanticVersion> for String {
    fn from(version: SemanticVersion) -> Self {
        version.0
    }
}

impl fmt::Display for SemanticVersion {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.0)
    }
}

impl AsRef<str> for SemanticVersion {
    fn as_ref(&self) -> &str {
        &self.0
    }
}

/// Non-empty search query that prevents empty queries
///
/// **Poka-yoke**: Queries are validated at construction, making empty queries
/// impossible to represent. This prevents performance issues from searching all packages.
///
/// # Invariants
/// - Non-empty after trimming whitespace
#[derive(Debug, Clone, PartialEq, Eq, Hash, Serialize, Deserialize)]
#[serde(try_from = "String", into = "String")]
pub struct NonEmptyQuery(String);

impl NonEmptyQuery {
    /// Create a validated non-empty query
    ///
    /// # Errors
    /// Returns error if query is empty or only whitespace
    pub fn new(query: impl Into<String>) -> Result<Self> {
        let query = query.into().trim().to_string();

        if query.is_empty() {
            return Err(Error::new(
                "❌ Search query cannot be empty. Please provide a search term.",
            ));
        }

        Ok(Self(query))
    }

    /// Get the inner string (guaranteed non-empty)
    pub fn as_str(&self) -> &str {
        &self.0
    }
}

impl TryFrom<String> for NonEmptyQuery {
    type Error = Error;

    fn try_from(query: String) -> Result<Self> {
        Self::new(query)
    }
}

impl From<NonEmptyQuery> for String {
    fn from(query: NonEmptyQuery) -> Self {
        query.0
    }
}

impl fmt::Display for NonEmptyQuery {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.0)
    }
}

impl AsRef<str> for NonEmptyQuery {
    fn as_ref(&self) -> &str {
        &self.0
    }
}

/// Validated SHA256 checksum
///
/// **Poka-yoke**: Checksums are validated at construction, making invalid checksums
/// impossible to represent. This prevents checksum verification errors.
///
/// # Invariants
/// - Non-empty
/// - 64 hexadecimal characters (SHA256 format)
#[derive(Debug, Clone, PartialEq, Eq, Hash, Serialize, Deserialize)]
#[serde(try_from = "String", into = "String")]
pub struct Checksum(String);

impl Checksum {
    /// Create a validated checksum
    ///
    /// # Errors
    /// Returns error if checksum is invalid (empty, wrong length, non-hex characters)
    pub fn new(checksum: impl Into<String>) -> Result<Self> {
        let checksum = checksum.into();

        // Validate non-empty
        if checksum.is_empty() {
            return Err(Error::new("Checksum cannot be empty"));
        }

        // Validate length (SHA256 is 64 hex characters)
        if checksum.len() != 64 {
            return Err(Error::new(&format!(
                "Checksum must be 64 hexadecimal characters (SHA256), got {}",
                checksum.len()
            )));
        }

        // Validate hexadecimal characters
        if !checksum.chars().all(|c| c.is_ascii_hexdigit()) {
            return Err(Error::new(
                "Checksum must contain only hexadecimal characters",
            ));
        }

        Ok(Self(checksum))
    }

    /// Get the inner string (guaranteed valid)
    pub fn as_str(&self) -> &str {
        &self.0
    }
}

impl TryFrom<String> for Checksum {
    type Error = Error;

    fn try_from(checksum: String) -> Result<Self> {
        Self::new(checksum)
    }
}

impl From<Checksum> for String {
    fn from(checksum: Checksum) -> Self {
        checksum.0
    }
}

impl fmt::Display for Checksum {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.0)
    }
}

impl AsRef<str> for Checksum {
    fn as_ref(&self) -> &str {
        &self.0
    }
}

#[cfg(test)]
#[path = "types_tests.rs"]
mod tests;
