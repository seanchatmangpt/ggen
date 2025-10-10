//! Core functionality for ggen
//!
//! This module contains the core business logic and domain models.

use serde::{Deserialize, Serialize};

/// Core configuration
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct CoreConfig {
    pub version: String,
    pub features: Vec<String>,
}

impl Default for CoreConfig {
    fn default() -> Self {
        Self {
            version: "0.2.4".to_string(),
            features: vec!["marketplace".to_string(), "agents".to_string()],
        }
    }
}
