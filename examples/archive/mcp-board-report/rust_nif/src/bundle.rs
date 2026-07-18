//! Evidence Bundle Module
//!
//! Provides .mcpb evidence bundle generation and text-blind verification.
//! Bundles contain receipts, refusals, and metrics for audit verification.

use crate::crypto::hash_sha256;
use crate::receipt::{Receipt, ReceiptChain};
use crate::refusal::Refusal;
use crate::{McpError, McpResult};
use serde::{Deserialize, Serialize};
use std::collections::HashMap;

/// Metrics included in bundle
#[derive(Debug, Clone, Default, Serialize, Deserialize)]
pub struct BundleMetrics {
    pub total_operations: u64,
    pub successful_operations: u64,
    pub refused_operations: u64,
    pub total_duration_us: u64,
    pub peak_memory_bytes: u64,
}

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
    pub receipts: Vec<Receipt>,
    pub refusals: Vec<Refusal>,
    pub metrics: BundleMetrics,
    pub attestations: Vec<String>,
    pub bundle_hash: String,
}

impl Bundle {
    /// Create a new bundle
    pub fn new(
        contract_family: String,
        receipts: Vec<Receipt>,
        refusals: Vec<Refusal>,
        period_start: chrono::DateTime<chrono::Utc>,
        period_end: chrono::DateTime<chrono::Utc>,
    ) -> McpResult<Self> {
        let bundle_id = format!("bundle-{}", uuid::Uuid::new_v4());
        let created_at = chrono::Utc::now();

        // Compute receipt chain root
        let receipt_chain_root = if receipts.is_empty() {
            "0".repeat(64)
        } else {
            let chain = ReceiptChain::from_receipts(receipts.clone())?;
            chain.root_hash().to_string()
        };

        // Compute metrics
        let metrics = BundleMetrics {
            total_operations: receipts.len() as u64,
            successful_operations: receipts
                .iter()
                .filter(|r| matches!(r.outcome, crate::receipt::ReceiptOutcome::Success))
                .count() as u64,
            refused_operations: refusals.len() as u64,
            total_duration_us: receipts.iter().map(|r| r.duration_us).sum(),
            peak_memory_bytes: 0, // Would be tracked during execution
        };

        // Compute bundle hash
        let hash_input = format!(
            "{}|{}|{}|{}|{}|{}|{}|{}",
            "1.0.0",
            bundle_id,
            created_at,
            period_start,
            period_end,
            contract_family,
            receipt_chain_root,
            metrics.total_operations
        );
        let bundle_hash = hash_sha256(hash_input.as_bytes());

        Ok(Self {
            version: "1.0.0".to_string(),
            bundle_id,
            created_at,
            period_start,
            period_end,
            contract_family,
            receipt_chain_root,
            receipts,
            refusals,
            metrics,
            attestations: Vec::new(),
            bundle_hash,
        })
    }

    /// Text-blind verification
    /// Verifies bundle integrity without accessing payload content
    pub fn verify(&self) -> McpResult<VerificationResult> {
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
        let computed_hash = hash_sha256(hash_input.as_bytes());
        checks.insert("bundle_hash".to_string(), computed_hash == self.bundle_hash);

        // Check 3: Receipt chain integrity (if receipts present)
        let chain_valid = if self.receipts.is_empty() {
            self.receipt_chain_root == "0".repeat(64)
        } else {
            match ReceiptChain::from_receipts(self.receipts.clone()) {
                Ok(chain) => {
                    chain.verify().is_ok() && chain.root_hash() == self.receipt_chain_root
                }
                Err(_) => false,
            }
        };
        checks.insert("receipt_chain".to_string(), chain_valid);

        // Check 4: Metrics consistency
        let metrics_valid = self.metrics.total_operations == self.receipts.len() as u64
            && self.metrics.refused_operations == self.refusals.len() as u64;
        checks.insert("metrics_consistency".to_string(), metrics_valid);

        // Check 5: Period validity
        let period_valid = self.period_start <= self.period_end;
        checks.insert("period_validity".to_string(), period_valid);

        // Check 6: Refusal codes are valid format
        let refusals_valid = self.refusals.iter().all(|r| {
            let code = r.code.as_str();
            code.contains('-') && code.len() >= 8
        });
        checks.insert("refusal_format".to_string(), refusals_valid);

        // Compute overall result
        let passed = checks.values().all(|&v| v);

        Ok(VerificationResult {
            passed,
            checks,
            bundle_id: self.bundle_id.clone(),
            verified_at: chrono::Utc::now(),
        })
    }

    /// Serialize to JSON
    pub fn to_json(&self) -> McpResult<String> {
        serde_json::to_string_pretty(self)
            .map_err(|e| McpError::SerializationError(e.to_string()))
    }

    /// Deserialize from JSON
    pub fn from_json(json: &str) -> McpResult<Self> {
        serde_json::from_str(json)
            .map_err(|e| McpError::SerializationError(e.to_string()))
    }
}

/// Verification result for text-blind verification
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct VerificationResult {
    pub passed: bool,
    pub checks: HashMap<String, bool>,
    pub bundle_id: String,
    pub verified_at: chrono::DateTime<chrono::Utc>,
}

impl VerificationResult {
    pub fn to_json(&self) -> McpResult<String> {
        serde_json::to_string_pretty(self)
            .map_err(|e| McpError::SerializationError(e.to_string()))
    }
}

/// Builder for creating bundles
pub struct BundleBuilder {
    contract_family: String,
    receipts: Vec<Receipt>,
    refusals: Vec<Refusal>,
    period_start: Option<chrono::DateTime<chrono::Utc>>,
    period_end: Option<chrono::DateTime<chrono::Utc>>,
}

impl BundleBuilder {
    pub fn new(contract_family: String) -> Self {
        Self {
            contract_family,
            receipts: Vec::new(),
            refusals: Vec::new(),
            period_start: None,
            period_end: None,
        }
    }

    pub fn with_receipts(mut self, receipts: Vec<Receipt>) -> Self {
        self.receipts = receipts;
        self
    }

    pub fn with_refusals(mut self, refusals: Vec<Refusal>) -> Self {
        self.refusals = refusals;
        self
    }

    pub fn with_period(
        mut self,
        start: chrono::DateTime<chrono::Utc>,
        end: chrono::DateTime<chrono::Utc>,
    ) -> Self {
        self.period_start = Some(start);
        self.period_end = Some(end);
        self
    }

    pub fn build(self) -> McpResult<Bundle> {
        let now = chrono::Utc::now();
        let period_start = self.period_start.unwrap_or(now - chrono::Duration::hours(24));
        let period_end = self.period_end.unwrap_or(now);

        Bundle::new(
            self.contract_family,
            self.receipts,
            self.refusals,
            period_start,
            period_end,
        )
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::receipt::ReceiptOutcome;
    use crate::refusal::RefusalCode;

    #[test]
    fn test_empty_bundle_creation() {
        let bundle = BundleBuilder::new("test-family".to_string())
            .build()
            .unwrap();

        assert_eq!(bundle.version, "1.0.0");
        assert!(bundle.bundle_id.starts_with("bundle-"));
        assert_eq!(bundle.receipts.len(), 0);
        assert_eq!(bundle.receipt_chain_root, "0".repeat(64));
    }

    #[test]
    fn test_bundle_with_receipts() {
        let genesis = Receipt::genesis(
            "contract-123".to_string(),
            "env-456".to_string(),
            "manifest-hash".to_string(),
        );

        let receipt2 = Receipt::new(
            "contract-123".to_string(),
            "env-456".to_string(),
            "manifest-hash".to_string(),
            ReceiptOutcome::Success,
            genesis.receipt_hash.clone(),
            500,
        );

        let bundle = BundleBuilder::new("test-family".to_string())
            .with_receipts(vec![genesis, receipt2])
            .build()
            .unwrap();

        assert_eq!(bundle.receipts.len(), 2);
        assert_eq!(bundle.metrics.total_operations, 2);
        assert_eq!(bundle.metrics.successful_operations, 2);
    }

    #[test]
    fn test_bundle_verification_pass() {
        let genesis = Receipt::genesis(
            "contract-123".to_string(),
            "env-456".to_string(),
            "manifest-hash".to_string(),
        );

        let bundle = BundleBuilder::new("test-family".to_string())
            .with_receipts(vec![genesis])
            .build()
            .unwrap();

        let result = bundle.verify().unwrap();

        assert!(result.passed);
        assert!(result.checks.get("bundle_hash").copied().unwrap_or(false));
        assert!(result.checks.get("receipt_chain").copied().unwrap_or(false));
    }

    #[test]
    fn test_bundle_with_refusals() {
        let refusal = Refusal::new(
            RefusalCode::time_exceeded(),
            "Time limit exceeded".to_string(),
            "env-123".to_string(),
        );

        let bundle = BundleBuilder::new("test-family".to_string())
            .with_refusals(vec![refusal])
            .build()
            .unwrap();

        assert_eq!(bundle.refusals.len(), 1);
        assert_eq!(bundle.metrics.refused_operations, 1);
    }

    #[test]
    fn test_bundle_serialization() {
        let bundle = BundleBuilder::new("test-family".to_string())
            .build()
            .unwrap();

        let json = bundle.to_json().unwrap();
        let parsed = Bundle::from_json(&json).unwrap();

        assert_eq!(parsed.bundle_id, bundle.bundle_id);
        assert_eq!(parsed.bundle_hash, bundle.bundle_hash);
    }

    #[test]
    fn test_verification_result_serialization() {
        let bundle = BundleBuilder::new("test-family".to_string())
            .build()
            .unwrap();

        let result = bundle.verify().unwrap();
        let json = result.to_json().unwrap();

        assert!(json.contains("passed"));
        assert!(json.contains("bundle_hash"));
    }
}
