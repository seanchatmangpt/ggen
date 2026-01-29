//! Receipt model
//!
//! Uses mcp_core for cryptographic operations.

use anyhow::Result;
use mcp_core::crypto::hash_sha256;
use serde::{Deserialize, Serialize};
use std::fs;
use std::path::{Path, PathBuf};

// Re-export mcp_core Receipt for contract execution use cases
pub use mcp_core::types::Receipt as CoreReceipt;
pub use mcp_core::types::ExecutionMetrics;

/// Cryptographic receipt for a CLI action
///
/// This is a simplified receipt for CLI operations (kill switch, drills, etc.)
/// For contract execution receipts, use [`CoreReceipt`].
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct CliReceipt {
    pub receipt_id: String,
    pub action: String,
    pub target: String,
    pub timestamp: chrono::DateTime<chrono::Utc>,
    pub operator: String,
    pub hostname: String,
    pub receipt_hash: String,
}

impl CliReceipt {
    /// Create a new CLI receipt
    pub fn new(action: &str, target: &str) -> Self {
        let receipt_id = format!("rcpt-{}-{}", chrono::Utc::now().timestamp(), std::process::id());
        let timestamp = chrono::Utc::now();
        let operator = std::env::var("USER").unwrap_or_else(|_| "unknown".to_string());
        let hostname = std::env::var("HOSTNAME")
            .or_else(|_| std::env::var("HOST"))
            .unwrap_or_else(|_| "localhost".to_string());

        // Use mcp_core crypto for hashing
        let hash_input = format!(
            "{}|{}|{}|{}",
            receipt_id, action, target, timestamp
        );
        let receipt_hash = hash_sha256(hash_input.as_bytes());

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

// Type alias for backwards compatibility
pub type Receipt = CliReceipt;

#[cfg(test)]
mod tests {
    use super::*;
    use tempfile::TempDir;

    #[test]
    fn test_receipt_creation() {
        let receipt = CliReceipt::new("test_action", "test_target");

        assert!(receipt.receipt_id.starts_with("rcpt-"));
        assert_eq!(receipt.action, "test_action");
        assert_eq!(receipt.target, "test_target");
        assert_eq!(receipt.receipt_hash.len(), 64);
    }

    #[test]
    fn test_receipt_save() {
        let receipt = CliReceipt::new("test_action", "test_target");
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
        let r1 = CliReceipt::new("action", "target");
        std::thread::sleep(std::time::Duration::from_millis(10));
        let r2 = CliReceipt::new("action", "target");

        // Different timestamps should produce different hashes
        assert_ne!(r1.receipt_hash, r2.receipt_hash);
    }

    #[test]
    fn test_core_receipt_available() {
        // Verify mcp_core Receipt is accessible through re-export
        let metrics = ExecutionMetrics::default();
        let core_receipt = CoreReceipt::new(
            1,
            mcp_core::GENESIS_HASH,
            "test",
            "contract",
            "in_hash",
            "out_hash",
            metrics,
        );
        assert!(core_receipt.receipt_id.starts_with("rcpt-"));
    }
}
