use serde::{Deserialize, Serialize};

use crate::error::Result;
use std::str::FromStr;

#[derive(Debug, Serialize, Deserialize, Clone, PartialEq)]
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
        let s = match *self {
            LogLevel::Debug => "debug",
            LogLevel::Info => "info",
            LogLevel::Warn => "warn",
            LogLevel::Error => "error",
        };
        write!(f, "{}", s)
    }
}

impl FromStr for LogLevel {
    type Err = crate::error::Error;

    fn from_str(s: &str) -> Result<Self> {
        match s {
            "debug" => Ok(LogLevel::Debug),
            "info" => Ok(LogLevel::Info),
            "warn" => Ok(LogLevel::Warn),
            "error" => Ok(LogLevel::Error),
            _ => Ok(LogLevel::Info),
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
        let debug_str = format!("{:?}", debug);
        assert!(debug_str.contains("Debug"));
    }
}
