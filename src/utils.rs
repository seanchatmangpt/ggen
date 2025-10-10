//! Utility functions and common types for ggen
//!
//! This module provides shared utilities and helper functions.

use serde::{Deserialize, Serialize};

/// Utility configuration
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct UtilsConfig {
    pub logging_level: String,
    pub cache_enabled: bool,
}

impl Default for UtilsConfig {
    fn default() -> Self {
        Self {
            logging_level: "info".to_string(),
            cache_enabled: true,
        }
    }
}

/// Helper function for string operations
pub fn truncate_string(s: &str, max_len: usize) -> String {
    if s.len() <= max_len {
        s.to_string()
    } else {
        format!("{}...", &s[..max_len - 3])
    }
}
