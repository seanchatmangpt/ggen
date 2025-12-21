//! Audit trail module for tracking ggen sync execution
//!
//! Provides deterministic execution logging with timestamps, rule tracking,
//! file metadata, and SHA256-based content hashing for reproducibility.

use std::collections::HashMap;
use std::time::SystemTime;
use serde::{Deserialize, Serialize};

/// Audit trail for a single ggen sync execution
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct AuditTrail {
    /// ISO 8601 timestamp of sync execution
    pub timestamp: String,
    /// Number of rules executed
    pub rules_executed: usize,
    /// Number of files written/modified
    pub files_changed: usize,
    /// SHA256 hashes of modified files for verification
    pub file_hashes: HashMap<String, String>,
    /// Execution metadata for reproducibility
    pub metadata: ExecutionMetadata,
}

/// Execution metadata for reproducibility and provenance
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct ExecutionMetadata {
    /// ggen version
    pub ggen_version: String,
    /// Manifest file path
    pub manifest_path: String,
    /// Ontology file path
    pub ontology_path: String,
    /// SHA256 hash of input specification
    pub spec_hash: String,
    /// Total execution time in milliseconds
    pub duration_ms: u64,
}

impl AuditTrail {
    /// Create a new audit trail with current timestamp
    pub fn new(ggen_version: &str, manifest_path: &str, ontology_path: &str) -> Self {
        let timestamp = chrono::Local::now().to_rfc3339();
        
        AuditTrail {
            timestamp,
            rules_executed: 0,
            files_changed: 0,
            file_hashes: HashMap::new(),
            metadata: ExecutionMetadata {
                ggen_version: ggen_version.to_string(),
                manifest_path: manifest_path.to_string(),
                ontology_path: ontology_path.to_string(),
                spec_hash: String::new(),
                duration_ms: 0,
            },
        }
    }

    /// Record that a rule was executed
    pub fn record_rule_executed(&mut self) {
        self.rules_executed += 1;
    }

    /// Record a file change with hash
    pub fn record_file_change(&mut self, path: String, hash: String) {
        self.files_changed += 1;
        self.file_hashes.insert(path, hash);
    }

    /// Serialize audit trail to JSON
    pub fn to_json(&self) -> Result<String, Box<dyn std::error::Error>> {
        Ok(serde_json::to_string_pretty(self)?)
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_audit_trail_creation() {
        let audit = AuditTrail::new("5.0.2", "test.toml", "test.ttl");
        assert_eq!(audit.rules_executed, 0);
        assert_eq!(audit.files_changed, 0);
    }

    #[test]
    fn test_record_rule_executed() {
        let mut audit = AuditTrail::new("5.0.2", "test.toml", "test.ttl");
        audit.record_rule_executed();
        assert_eq!(audit.rules_executed, 1);
    }

    #[test]
    fn test_to_json_serialization() {
        let audit = AuditTrail::new("5.0.2", "test.toml", "test.ttl");
        let json = audit.to_json().expect("Failed to serialize");
        assert!(json.contains("\"rules_executed\""));
        assert!(json.contains("\"files_changed\""));
    }
}

pub mod writer;
