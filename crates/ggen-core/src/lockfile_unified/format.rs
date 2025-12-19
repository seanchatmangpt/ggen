//! TOML and JSON format implementations for lockfiles
//!
//! This module provides format abstraction allowing lockfiles to be
//! serialized in either TOML or JSON format.

use ggen_utils::error::{Error, Result};
use serde::{de::DeserializeOwned, Serialize};
use std::path::Path;

use super::traits::{Lockfile, LockfileFormat};

/// TOML serialization format (default for ggen.lock)
#[derive(Debug, Clone, Copy, Default)]
pub struct TomlFormat;

impl LockfileFormat for TomlFormat {
    const EXTENSION: &'static str = "lock";
    const MIME_TYPE: &'static str = "application/toml";

    fn serialize<L: Lockfile>(lockfile: &L) -> Result<String> {
        toml::to_string_pretty(lockfile)
            .map_err(|e| Error::with_context("Failed to serialize TOML lockfile", &e.to_string()))
    }

    fn deserialize<L: Lockfile>(content: &str) -> Result<L> {
        toml::from_str(content)
            .map_err(|e| Error::with_context("Failed to parse TOML lockfile", &e.to_string()))
    }
}

/// JSON serialization format (for .ggen/packs.lock)
#[derive(Debug, Clone, Copy, Default)]
pub struct JsonFormat;

impl LockfileFormat for JsonFormat {
    const EXTENSION: &'static str = "json";
    const MIME_TYPE: &'static str = "application/json";

    fn serialize<L: Lockfile>(lockfile: &L) -> Result<String> {
        serde_json::to_string_pretty(lockfile)
            .map_err(|e| Error::with_context("Failed to serialize JSON lockfile", &e.to_string()))
    }

    fn deserialize<L: Lockfile>(content: &str) -> Result<L> {
        serde_json::from_str(content)
            .map_err(|e| Error::with_context("Failed to parse JSON lockfile", &e.to_string()))
    }
}

/// Format type enumeration for runtime selection
#[derive(Debug, Clone, Copy, PartialEq, Eq, Default)]
pub enum FormatType {
    /// TOML format (.lock, .toml)
    #[default]
    Toml,
    /// JSON format (.json)
    Json,
}

impl FormatType {
    /// Get file extension for this format
    pub fn extension(&self) -> &'static str {
        match self {
            FormatType::Toml => "lock",
            FormatType::Json => "json",
        }
    }

    /// Get MIME type for this format
    pub fn mime_type(&self) -> &'static str {
        match self {
            FormatType::Toml => "application/toml",
            FormatType::Json => "application/json",
        }
    }

    /// Serialize using this format (dynamic dispatch)
    pub fn serialize<T: Serialize>(&self, value: &T) -> Result<String> {
        match self {
            FormatType::Toml => toml::to_string_pretty(value)
                .map_err(|e| Error::with_context("Failed to serialize TOML", &e.to_string())),
            FormatType::Json => serde_json::to_string_pretty(value)
                .map_err(|e| Error::with_context("Failed to serialize JSON", &e.to_string())),
        }
    }

    /// Deserialize using this format (dynamic dispatch)
    pub fn deserialize<T: DeserializeOwned>(&self, content: &str) -> Result<T> {
        match self {
            FormatType::Toml => toml::from_str(content)
                .map_err(|e| Error::with_context("Failed to parse TOML", &e.to_string())),
            FormatType::Json => serde_json::from_str(content)
                .map_err(|e| Error::with_context("Failed to parse JSON", &e.to_string())),
        }
    }
}

/// Detect format type from file extension
///
/// Returns `FormatType::Toml` as default for unknown extensions.
///
/// # Examples
///
/// ```rust
/// use ggen_core::lockfile_unified::format::{detect_format, FormatType};
/// use std::path::Path;
///
/// assert_eq!(detect_format(Path::new("ggen.lock")), FormatType::Toml);
/// assert_eq!(detect_format(Path::new("packs.json")), FormatType::Json);
/// assert_eq!(detect_format(Path::new("Cargo.toml")), FormatType::Toml);
/// ```
pub fn detect_format(path: &Path) -> FormatType {
    match path.extension().and_then(|e| e.to_str()) {
        Some("json") => FormatType::Json,
        Some("lock") | Some("toml") | None => FormatType::Toml,
        Some(_) => FormatType::Toml, // Default to TOML for unknown
    }
}

/// Auto-format serializer that detects format from path
pub fn serialize_to_path<T: Serialize>(value: &T, path: &Path) -> Result<String> {
    detect_format(path).serialize(value)
}

/// Auto-format deserializer that detects format from path
pub fn deserialize_from_path<T: DeserializeOwned>(content: &str, path: &Path) -> Result<T> {
    detect_format(path).deserialize(content)
}

#[cfg(test)]
mod tests {
    use super::*;
    use serde::{Deserialize, Serialize};

    #[derive(Debug, Clone, Serialize, Deserialize, PartialEq)]
    struct TestData {
        name: String,
        value: i32,
    }

    #[test]
    fn test_detect_format() {
        assert_eq!(detect_format(Path::new("ggen.lock")), FormatType::Toml);
        assert_eq!(detect_format(Path::new("config.toml")), FormatType::Toml);
        assert_eq!(detect_format(Path::new("packs.json")), FormatType::Json);
        assert_eq!(detect_format(Path::new("unknown")), FormatType::Toml);
    }

    #[test]
    fn test_toml_roundtrip() {
        let data = TestData {
            name: "test".into(),
            value: 42,
        };

        let serialized = FormatType::Toml.serialize(&data).unwrap();
        let deserialized: TestData = FormatType::Toml.deserialize(&serialized).unwrap();

        assert_eq!(data, deserialized);
    }

    #[test]
    fn test_json_roundtrip() {
        let data = TestData {
            name: "test".into(),
            value: 42,
        };

        let serialized = FormatType::Json.serialize(&data).unwrap();
        let deserialized: TestData = FormatType::Json.deserialize(&serialized).unwrap();

        assert_eq!(data, deserialized);
    }
}
