//! Common type definitions for ggen
//!
//! This module provides shared type definitions used across the ggen codebase,
//! including log levels, configuration types, and other common enums and structs.
//!
//! ## Types
//!
//! - **LogLevel**: Logging level enumeration (Debug, Info, Warn, Error)
//! - **Serialization support**: All types support serde serialization/deserialization
//!
//! ## Examples
//!
//! ### Using LogLevel
//!
//! ```rust
//! use ggen_utils::types::LogLevel;
//! use std::str::FromStr;
//!
//! # fn main() -> Result<(), Box<dyn std::error::Error>> {
//! // Parse from string
//! let level = LogLevel::from_str("debug")?;
//! assert_eq!(level, LogLevel::Debug);
//!
//! // Display
//! assert_eq!(level.to_string(), "debug");
//!
//! // Serialize
//! let json = serde_json::to_string(&level)?;
//! assert_eq!(json, "\"debug\"");
//! # Ok(())
//! # }
//! ```

use serde::{Deserialize, Serialize};

use crate::error::Result;
use std::str::FromStr;

#[derive(Debug, Serialize, Deserialize, Clone, PartialEq, Eq)]
pub enum LogLevel {
    #[serde(rename = "debug")]
    Debug,
    #[serde(rename = "info")]
    Info,
    #[serde(rename = "warn")]
    Warn,
    #[serde(rename = "error")]
    Error,
}

impl std::fmt::Display for LogLevel {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        let s = match self {
            Self::Debug => "debug",
            Self::Info => "info",
            Self::Warn => "warn",
            Self::Error => "error",
        };
        write!(f, "{s}")
    }
}

impl FromStr for LogLevel {
    type Err = crate::error::Error;

    fn from_str(s: &str) -> Result<Self> {
        match s {
            "debug" => Ok(Self::Debug),
            "warn" => Ok(Self::Warn),
            "error" => Ok(Self::Error),
            _ => Ok(Self::Info),
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_log_level_display() {
        assert_eq!(LogLevel::Debug.to_string(), "debug");
        assert_eq!(LogLevel::Info.to_string(), "info");
        assert_eq!(LogLevel::Warn.to_string(), "warn");
        assert_eq!(LogLevel::Error.to_string(), "error");
    }

    #[test]
    fn test_log_level_from_str() {
        assert_eq!(LogLevel::from_str("debug").unwrap(), LogLevel::Debug);
        assert_eq!(LogLevel::from_str("info").unwrap(), LogLevel::Info);
        assert_eq!(LogLevel::from_str("warn").unwrap(), LogLevel::Warn);
        assert_eq!(LogLevel::from_str("error").unwrap(), LogLevel::Error);

        // Default case
        assert_eq!(LogLevel::from_str("invalid").unwrap(), LogLevel::Info);
        assert_eq!(LogLevel::from_str("").unwrap(), LogLevel::Info);
    }

    #[test]
    fn test_log_level_serialization() {
        // Test serialization
        let debug_json = serde_json::to_string(&LogLevel::Debug).unwrap();
        assert_eq!(debug_json, "\"debug\"");

        let info_json = serde_json::to_string(&LogLevel::Info).unwrap();
        assert_eq!(info_json, "\"info\"");
    }

    #[test]
    fn test_log_level_deserialization() {
        // Test deserialization
        let debug: LogLevel = serde_json::from_str("\"debug\"").unwrap();
        assert_eq!(debug, LogLevel::Debug);

        let info: LogLevel = serde_json::from_str("\"info\"").unwrap();
        assert_eq!(info, LogLevel::Info);
    }

    #[test]
    fn test_log_level_debug() {
        let debug = LogLevel::Debug;
        let debug_str = format!("{debug:?}");
        assert!(debug_str.contains("Debug"));
    }
}
