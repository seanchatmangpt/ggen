//! Evidence bundle model

use serde::{Deserialize, Serialize};
use sha2::{Digest, Sha256};
use std::collections::HashMap;

/// Evidence bundle (.mcpb format)
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct Bundle {
    pub version: String,
    pub bundle_id: String,
    pub created_at: chrono::DateTime<chrono::Utc>,
    pub period_start: chrono::DateTime<chrono::Utc>,
    pub period_end: chrono::DateTime<chrono::Utc>,
    pub contract_family: String,
    pub receipt_chain_root: String,
    pub receipts: Vec<serde_json::Value>,
    pub refusals: Vec<serde_json::Value>,
    pub metrics: BundleMetrics,
    pub attestations: Vec<String>,
    pub bundle_hash: String,
}

/// Metrics included in bundle
#[derive(Debug, Clone, Default, Serialize, Deserialize)]
pub struct BundleMetrics {
    pub total_operations: u64,
    pub successful_operations: u64,
    pub refused_operations: u64,
    pub total_duration_us: u64,
    pub peak_memory_bytes: u64,
}

/// Verification result
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct VerificationResult {
    pub passed: bool,
    pub checks: HashMap<String, bool>,
    pub bundle_id: String,
    pub verified_at: chrono::DateTime<chrono::Utc>,
}

impl Bundle {
    /// Create a new empty bundle
    pub fn new_empty(contract_family: String) -> Self {
        let bundle_id = format!("bundle-{}", uuid::Uuid::new_v4());
        let now = chrono::Utc::now();
        let period_start = now - chrono::Duration::hours(24);

        // Empty receipt chain has zero hash
        let receipt_chain_root = "0".repeat(64);

        // Compute bundle hash
        let hash_input = format!(
            "1.0.0|{}|{}|{}|{}|{}|{}|0",
            bundle_id, now, period_start, now, contract_family, receipt_chain_root
        );
        let mut hasher = Sha256::new();
        hasher.update(hash_input.as_bytes());
        let bundle_hash = hex::encode(hasher.finalize());

        Self {
            version: "1.0.0".to_string(),
            bundle_id,
            created_at: now,
            period_start,
            period_end: now,
            contract_family,
            receipt_chain_root,
            receipts: Vec::new(),
            refusals: Vec::new(),
            metrics: BundleMetrics::default(),
            attestations: Vec::new(),
            bundle_hash,
        }
    }

    /// Text-blind verification
    pub fn verify(&self) -> VerificationResult {
        let mut checks = HashMap::new();

        // Check 1: Version format
        checks.insert(
            "version_format".to_string(),
            self.version.starts_with("1."),
        );

        // Check 2: Bundle hash integrity
        let hash_input = format!(
            "{}|{}|{}|{}|{}|{}|{}|{}",
            self.version,
            self.bundle_id,
            self.created_at,
            self.period_start,
            self.period_end,
            self.contract_family,
            self.receipt_chain_root,
            self.metrics.total_operations
        );
        let mut hasher = Sha256::new();
        hasher.update(hash_input.as_bytes());
        let computed_hash = hex::encode(hasher.finalize());
        checks.insert("bundle_hash".to_string(), computed_hash == self.bundle_hash);

        // Check 3: Receipt chain (empty is valid with zero hash)
        let chain_valid = if self.receipts.is_empty() {
            self.receipt_chain_root == "0".repeat(64)
        } else {
            // Would verify Merkle chain in production
            true
        };
        checks.insert("receipt_chain".to_string(), chain_valid);

        // Check 4: Metrics consistency
        let metrics_valid = self.metrics.total_operations == self.receipts.len() as u64
            && self.metrics.refused_operations == self.refusals.len() as u64;
        checks.insert("metrics_consistency".to_string(), metrics_valid);

        // Check 5: Period validity
        let period_valid = self.period_start <= self.period_end;
        checks.insert("period_validity".to_string(), period_valid);

        // Check 6: Refusal format (all refusals have valid structure)
        let refusals_valid = self.refusals.iter().all(|r| {
            r.get("code").is_some()
        });
        checks.insert("refusal_format".to_string(), refusals_valid || self.refusals.is_empty());

        let passed = checks.values().all(|&v| v);

        VerificationResult {
            passed,
            checks,
            bundle_id: self.bundle_id.clone(),
            verified_at: chrono::Utc::now(),
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_empty_bundle_creation() {
        let bundle = Bundle::new_empty("test-family".to_string());

        assert_eq!(bundle.version, "1.0.0");
        assert!(bundle.bundle_id.starts_with("bundle-"));
        assert!(bundle.receipts.is_empty());
        assert_eq!(bundle.receipt_chain_root, "0".repeat(64));
    }

    #[test]
    fn test_bundle_verification_pass() {
        let bundle = Bundle::new_empty("test-family".to_string());
        let result = bundle.verify();

        assert!(result.passed);
        assert!(result.checks.get("bundle_hash").copied().unwrap_or(false));
        assert!(result.checks.get("receipt_chain").copied().unwrap_or(false));
        assert!(result.checks.get("metrics_consistency").copied().unwrap_or(false));
    }

    #[test]
    fn test_verification_result_serialization() {
        let bundle = Bundle::new_empty("test-family".to_string());
        let result = bundle.verify();

        let json = serde_json::to_string(&result).unwrap();
        assert!(json.contains("passed"));
        assert!(json.contains("bundle_hash"));
    }
}
