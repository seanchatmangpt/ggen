//! Evidence collection and cryptographic receipt generation

use crate::error::Result;
use serde::{Deserialize, Serialize};
use sha2::{Digest, Sha256};

/// Validation receipt with cryptographic evidence
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct ValidationReceipt {
    /// Receipt ID
    pub receipt_id: String,
    /// Execution ID
    pub execution_id: String,
    /// Manifest SHA-256 hash
    pub manifest_hash: String,
    /// Ontology SHA-256 hash
    pub ontology_hash: String,
    /// Files generated with hashes
    pub files: Vec<FileHash>,
    /// Total duration (seconds)
    pub duration_secs: f64,
    /// Rules executed
    pub rules_executed: u32,
    /// Audit timestamp (ISO 8601)
    pub audit_timestamp: String,
}

impl ValidationReceipt {
    /// Create new receipt
    pub fn new() -> Self {
        Self {
            receipt_id: uuid::Uuid::new_v4().to_string(),
            execution_id: format!("exec-{}", chrono::Utc::now().timestamp()),
            manifest_hash: String::new(),
            ontology_hash: String::new(),
            files: Vec::new(),
            duration_secs: 0.0,
            rules_executed: 0,
            audit_timestamp: chrono::Utc::now().to_rfc3339(),
        }
    }

    /// Set manifest hash
    pub fn with_manifest_hash(mut self, hash: String) -> Self {
        self.manifest_hash = hash;
        self
    }

    /// Set ontology hash
    pub fn with_ontology_hash(mut self, hash: String) -> Self {
        self.ontology_hash = hash;
        self
    }

    /// Add file hash
    pub fn add_file(mut self, path: String, hash: String) -> Self {
        self.files.push(FileHash { path, hash });
        self
    }

    /// Set duration
    pub fn with_duration(mut self, secs: f64) -> Self {
        self.duration_secs = secs;
        self
    }

    /// Set rules executed
    pub fn with_rules_executed(mut self, count: u32) -> Self {
        self.rules_executed = count;
        self
    }

    /// Get receipt as JSON
    pub fn to_json(&self) -> Result<String> {
        Ok(serde_json::to_string_pretty(self)?)
    }
}

impl Default for ValidationReceipt {
    fn default() -> Self {
        Self::new()
    }
}

/// File with SHA-256 hash
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct FileHash {
    /// File path
    pub path: String,
    /// SHA-256 hash
    pub hash: String,
}

/// Evidence collector for validation operations
pub struct EvidenceCollector {
    /// Collected evidence items
    evidence: Vec<EvidenceItem>,
}

/// Single piece of evidence
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct EvidenceItem {
    /// Evidence type
    pub evidence_type: String,
    /// Evidence description
    pub description: String,
    /// Evidence value/data
    pub value: String,
    /// SHA-256 hash of evidence
    pub hash: String,
    /// Collection timestamp
    pub collected_at: String,
}

impl EvidenceCollector {
    /// Create new collector
    pub fn new() -> Self {
        Self {
            evidence: Vec::new(),
        }
    }

    /// Collect evidence item
    pub fn collect(&mut self, evidence_type: String, description: String, value: String) {
        let hash = Self::hash_value(&value);
        self.evidence.push(EvidenceItem {
            evidence_type,
            description,
            value,
            hash,
            collected_at: chrono::Utc::now().to_rfc3339(),
        });
    }

    /// Hash value using SHA-256
    pub fn hash_value(value: &str) -> String {
        let mut hasher = Sha256::new();
        hasher.update(value.as_bytes());
        hex::encode(hasher.finalize())
    }

    /// Get collected evidence
    pub fn evidence(&self) -> &[EvidenceItem] {
        &self.evidence
    }

    /// Generate audit trail
    pub fn generate_audit_trail(&self) -> Result<String> {
        Ok(serde_json::to_string_pretty(&self.evidence)?)
    }

    /// Get total evidence count
    pub fn count(&self) -> usize {
        self.evidence.len()
    }

    /// Clear all evidence
    pub fn clear(&mut self) {
        self.evidence.clear();
    }
}

impl Default for EvidenceCollector {
    fn default() -> Self {
        Self::new()
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_receipt_creation() {
        let receipt = ValidationReceipt::new();
        assert!(!receipt.receipt_id.is_empty());
        assert!(!receipt.execution_id.is_empty());
    }

    #[test]
    fn test_receipt_with_hashes() {
        let receipt = ValidationReceipt::new()
            .with_manifest_hash("abc123".to_string())
            .with_ontology_hash("def456".to_string());
        assert_eq!(receipt.manifest_hash, "abc123");
        assert_eq!(receipt.ontology_hash, "def456");
    }

    #[test]
    fn test_receipt_add_file() {
        let receipt = ValidationReceipt::new()
            .add_file("file1.rs".to_string(), "hash1".to_string())
            .add_file("file2.rs".to_string(), "hash2".to_string());
        assert_eq!(receipt.files.len(), 2);
    }

    #[test]
    fn test_evidence_collector() {
        let mut collector = EvidenceCollector::new();
        collector.collect(
            "log".to_string(),
            "Test log".to_string(),
            "test data".to_string(),
        );
        assert_eq!(collector.count(), 1);
    }

    #[test]
    fn test_hash_value() {
        let hash1 = EvidenceCollector::hash_value("test");
        let hash2 = EvidenceCollector::hash_value("test");
        assert_eq!(hash1, hash2);
    }

    #[test]
    fn test_receipt_to_json() {
        let receipt = ValidationReceipt::new();
        let json = receipt.to_json();
        assert!(json.is_ok());
        let json_str = json.unwrap();
        assert!(json_str.contains("receipt_id"));
    }
}
