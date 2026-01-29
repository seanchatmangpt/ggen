//! Receipt model

use anyhow::Result;
use serde::{Deserialize, Serialize};
use sha2::{Digest, Sha256};
use std::fs;
use std::path::{Path, PathBuf};

/// Cryptographic receipt for an action
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct Receipt {
    pub receipt_id: String,
    pub action: String,
    pub target: String,
    pub timestamp: chrono::DateTime<chrono::Utc>,
    pub operator: String,
    pub hostname: String,
    pub receipt_hash: String,
}

impl Receipt {
    /// Create a new receipt
    pub fn new(action: &str, target: &str) -> Self {
        let receipt_id = format!("rcpt-{}-{}", chrono::Utc::now().timestamp(), std::process::id());
        let timestamp = chrono::Utc::now();
        let operator = std::env::var("USER").unwrap_or_else(|_| "unknown".to_string());
        let hostname = std::env::var("HOSTNAME")
            .or_else(|_| std::env::var("HOST"))
            .unwrap_or_else(|_| "localhost".to_string());

        // Compute hash
        let hash_input = format!(
            "{}|{}|{}|{}",
            receipt_id, action, target, timestamp
        );
        let mut hasher = Sha256::new();
        hasher.update(hash_input.as_bytes());
        let receipt_hash = hex::encode(hasher.finalize());

        Self {
            receipt_id,
            action: action.to_string(),
            target: target.to_string(),
            timestamp,
            operator,
            hostname,
            receipt_hash,
        }
    }

    /// Save receipt to file
    pub fn save(&self, evidence_dir: &Path) -> Result<PathBuf> {
        let path = evidence_dir.join(format!("{}.json", self.receipt_id));
        let json = serde_json::to_string_pretty(self)?;
        fs::write(&path, json)?;
        Ok(path)
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use tempfile::TempDir;

    #[test]
    fn test_receipt_creation() {
        let receipt = Receipt::new("test_action", "test_target");

        assert!(receipt.receipt_id.starts_with("rcpt-"));
        assert_eq!(receipt.action, "test_action");
        assert_eq!(receipt.target, "test_target");
        assert_eq!(receipt.receipt_hash.len(), 64);
    }

    #[test]
    fn test_receipt_save() {
        let receipt = Receipt::new("test_action", "test_target");
        let tmp = TempDir::new().unwrap();

        let path = receipt.save(tmp.path()).unwrap();
        assert!(path.exists());

        let content = fs::read_to_string(&path).unwrap();
        assert!(content.contains("receipt_id"));
        assert!(content.contains(&receipt.receipt_hash));
    }

    #[test]
    fn test_receipt_hash_determinism() {
        // Two receipts with same inputs at different times should have different hashes
        // (because timestamp is included)
        let r1 = Receipt::new("action", "target");
        std::thread::sleep(std::time::Duration::from_millis(10));
        let r2 = Receipt::new("action", "target");

        // Different timestamps should produce different hashes
        assert_ne!(r1.receipt_hash, r2.receipt_hash);
    }
}
