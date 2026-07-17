//! Enterprise Claims Processing Workflow
//!
//! Demonstrates the Chatman Equation applied to insurance claims.
//! Complete pipeline: Submission → Validation → Fraud Detection →
//! Entitlements → Settlement → Payment → Receipt
//!
//! Validates with 100+ synthetic claims, proving determinism,
//! guard effectiveness, and audit trail completeness.

use crate::sector_stacks::{OperationReceipt, OperationStatus, SectorOperation};
use hex;
use serde::{Deserialize, Serialize};
use sha2::{Digest, Sha256};

/// Claim submission
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct ClaimSubmission {
    /// Unique claim identifier
    pub claim_id: String,
    /// Claimant identifier
    pub claimant_id: String,
    /// Claimed amount
    pub claim_amount: f64,
    /// Claim submission date
    pub claim_date: String,
    /// Description of the incident
    pub incident_description: String,
}

/// Validation result
#[derive(Debug, Clone, Copy, PartialEq, Eq, Serialize, Deserialize)]
pub enum ValidationResult {
    /// Claim is valid
    Valid,
    /// Claim is invalid
    Invalid,
}

/// Fraud score (0-100)
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct FraudScore {
    /// Fraud score from 0 to 100
    pub score: u32,
    /// List of fraud indicators
    pub indicators: Vec<String>,
    /// Whether claim is fraudulent
    pub is_fraudulent: bool,
}

impl FraudScore {
    /// Determine fraud based on indicators
    #[must_use]
    pub fn new(indicators: Vec<String>) -> Self {
        #[allow(clippy::cast_possible_truncation)] // Score is clamped to 100, so truncation is safe
        let score = (indicators.len() * 20).min(100) as u32;
        // Any fraud indicator triggers fraud flag
        let is_fraudulent = !indicators.is_empty();

        Self { score, indicators, is_fraudulent }
    }
}

/// Entitlements check
#[derive(Debug, Clone, Copy, PartialEq, Eq, Serialize, Deserialize)]
pub enum EntitlementsResult {
    /// Claimant is entitled to settlement
    Entitled,
    /// Claimant is not entitled to settlement
    NotEntitled,
}

/// Settlement calculation
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct Settlement {
    /// Original claim amount
    pub amount: f64,
    /// Deductible amount
    pub deductible: f64,
    /// Policy limit
    pub policy_limit: f64,
    /// Final settlement amount after deductible and limit
    pub final_amount: f64,
}

impl Settlement {
    /// Calculate final settlement
    #[must_use]
    pub fn calculate(claim_amount: f64, deductible: f64, policy_limit: f64) -> Self {
        let final_amount = ((claim_amount - deductible).max(0.0)).min(policy_limit);

        Self { amount: claim_amount, deductible, policy_limit, final_amount }
    }
}

/// Claims processing operation
pub struct ClaimsOperation {
    claim: ClaimSubmission,
    validation: ValidationResult,
    fraud_score: FraudScore,
    entitlements: EntitlementsResult,
    settlement: Settlement,
}

impl ClaimsOperation {
    /// Create new claims operation
    #[must_use]
    pub fn new(claim: ClaimSubmission) -> Self {
        // Validation (deterministic)
        let validation = if !claim.claim_id.is_empty()
            && claim.claim_amount > 0.0
            && claim.incident_description.len() >= 10
        {
            ValidationResult::Valid
        } else {
            ValidationResult::Invalid
        };

        // Fraud detection (deterministic)
        let mut fraud_indicators = Vec::new();
        if claim.claim_amount > 100_000.0 {
            fraud_indicators.push("High amount".to_string());
        }
        if claim.claim_id.contains("duplicate") {
            fraud_indicators.push("Duplicate claim".to_string());
        }
        let fraud_score = FraudScore::new(fraud_indicators);

        // Entitlements (deterministic)
        let entitlements = if validation == ValidationResult::Valid && !fraud_score.is_fraudulent {
            EntitlementsResult::Entitled
        } else {
            EntitlementsResult::NotEntitled
        };

        // Settlement (deterministic)
        let settlement = Settlement::calculate(claim.claim_amount, 500.0, 50_000.0);

        Self { claim, validation, fraud_score, entitlements, settlement }
    }

    /// Get validation result
    #[must_use]
    pub const fn validation(&self) -> ValidationResult {
        self.validation
    }

    /// Get fraud score
    #[must_use]
    pub const fn fraud_score(&self) -> &FraudScore {
        &self.fraud_score
    }

    /// Get entitlements result
    #[must_use]
    pub const fn entitlements(&self) -> EntitlementsResult {
        self.entitlements
    }

    /// Get settlement
    #[must_use]
    pub const fn settlement(&self) -> &Settlement {
        &self.settlement
    }

    /// Check if claim should be approved
    #[must_use]
    pub fn should_approve(&self) -> bool {
        self.validation == ValidationResult::Valid
            && !self.fraud_score.is_fraudulent
            && self.entitlements == EntitlementsResult::Entitled
            && self.settlement.final_amount > 0.0
    }

    /// Generate settlement receipt
    #[must_use]
    pub fn generate_settlement_receipt(&self) -> OperationReceipt {
        let status =
            if self.should_approve() { OperationStatus::Success } else { OperationStatus::Failed };

        let mut hasher = Sha256::new();
        hasher.update(self.claim.claim_id.as_bytes());
        hasher.update(self.fraud_score().score.to_string().as_bytes());
        hasher.update(self.settlement().final_amount.to_string().as_bytes());
        let hash = hasher.finalize();

        OperationReceipt {
            id: format!("claims-{}", self.claim.claim_id),
            sector: "Enterprise Claims".to_string(),
            operation: "Settlement".to_string(),
            status,
            result: format!(
                "Settlement: ${:.2} (Validated: {}, Fraud Score: {}, Entitled: {})",
                self.settlement().final_amount,
                self.validation() == ValidationResult::Valid,
                self.fraud_score().score,
                self.entitlements() == EntitlementsResult::Entitled
            ),
            merkle_root: hex::encode(hash),
            timestamp: "2025-11-16T00:00:00Z".to_string(),
        }
    }
}

impl SectorOperation for ClaimsOperation {
    fn sector_name(&self) -> &'static str {
        "Enterprise Claims"
    }

    fn description(&self) -> &'static str {
        "Insurance claims processing workflow"
    }

    fn is_deterministic(&self) -> bool {
        true // All stages are deterministic given inputs
    }

    fn generate_receipt(&self, _status: OperationStatus) -> OperationReceipt {
        self.generate_settlement_receipt()
    }
}

/// Synthetic claims generator for testing
pub struct SyntheticClaimsGenerator;

impl SyntheticClaimsGenerator {
    /// Generate synthetic test claims
    #[must_use]
    pub fn generate(count: usize) -> Vec<ClaimSubmission> {
        let mut claims = Vec::new();

        for i in 0..count {
            #[allow(clippy::cast_precision_loss)]
            // Precision loss acceptable for test data generation
            let claim = ClaimSubmission {
                claim_id: format!("CLAIM-{i:06}"),
                claimant_id: format!("CLT-{:04}", i % 100),
                claim_amount: 1000.0 + (i as f64 * 100.0) % 50_000.0,
                claim_date: "2025-11-16".to_string(),
                incident_description: format!("Incident description for claim {i}"),
            };
            claims.push(claim);
        }

        claims
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_validation_valid_claim() {
        let claim = ClaimSubmission {
            claim_id: "CLAIM-001".to_string(),
            claimant_id: "CLT-0001".to_string(),
            claim_amount: 5000.0,
            claim_date: "2025-11-16".to_string(),
            incident_description: "Valid incident description here".to_string(),
        };

        let op = ClaimsOperation::new(claim);
        assert_eq!(op.validation(), ValidationResult::Valid);
    }

    #[test]
    fn test_validation_invalid_claim() {
        let claim = ClaimSubmission {
            claim_id: "".to_string(),
            claimant_id: "CLT-0001".to_string(),
            claim_amount: 5000.0,
            claim_date: "2025-11-16".to_string(),
            incident_description: "Short".to_string(),
        };

        let op = ClaimsOperation::new(claim);
        assert_eq!(op.validation(), ValidationResult::Invalid);
    }

    #[test]
    fn test_fraud_detection() {
        let claim = ClaimSubmission {
            claim_id: "CLAIM-002".to_string(),
            claimant_id: "CLT-0002".to_string(),
            claim_amount: 150_000.0, // High amount triggers fraud indicator
            claim_date: "2025-11-16".to_string(),
            incident_description: "Valid incident description here".to_string(),
        };

        let op = ClaimsOperation::new(claim);
        assert!(op.fraud_score().is_fraudulent);
        assert!(!op.should_approve());
    }

    #[test]
    fn test_settlement_calculation() {
        let settlement = Settlement::calculate(5000.0, 500.0, 50_000.0);
        assert_eq!(settlement.final_amount, 4500.0);
    }

    #[test]
    fn test_settlement_exceeds_policy_limit() {
        let settlement = Settlement::calculate(100_000.0, 500.0, 50_000.0);
        assert_eq!(settlement.final_amount, 50_000.0);
    }

    #[test]
    fn test_receipt_generation_approved() {
        let claim = ClaimSubmission {
            claim_id: "CLAIM-003".to_string(),
            claimant_id: "CLT-0003".to_string(),
            claim_amount: 5000.0,
            claim_date: "2025-11-16".to_string(),
            incident_description: "Valid incident description here".to_string(),
        };

        let op = ClaimsOperation::new(claim);
        let receipt = op.generate_receipt(OperationStatus::Success);

        assert_eq!(receipt.sector, "Enterprise Claims");
        assert_eq!(receipt.status, OperationStatus::Success);
    }

    #[test]
    fn test_determinism_100_claims() {
        let claims = SyntheticClaimsGenerator::generate(100);

        // Process all claims twice
        let results1: Vec<_> = claims
            .iter()
            .map(|c| {
                let op = ClaimsOperation::new(c.clone());
                (op.should_approve(), op.settlement().final_amount)
            })
            .collect();

        let results2: Vec<_> = claims
            .iter()
            .map(|c| {
                let op = ClaimsOperation::new(c.clone());
                (op.should_approve(), op.settlement().final_amount)
            })
            .collect();

        // Should be identical
        assert_eq!(results1, results2);
    }

    #[test]
    fn test_audit_trail_completeness() {
        let claims = SyntheticClaimsGenerator::generate(50);

        for claim in claims {
            let op = ClaimsOperation::new(claim);
            let receipt = op.generate_receipt(OperationStatus::Success);

            // Every claim must have a receipt
            assert!(!receipt.id.is_empty());
            assert!(!receipt.merkle_root.is_empty());
            assert_eq!(receipt.sector, "Enterprise Claims");
        }
    }

    #[test]
    fn test_receipt_reproducibility() {
        let claim = ClaimSubmission {
            claim_id: "CLAIM-004".to_string(),
            claimant_id: "CLT-0004".to_string(),
            claim_amount: 5000.0,
            claim_date: "2025-11-16".to_string(),
            incident_description: "Valid incident description here".to_string(),
        };

        let op1 = ClaimsOperation::new(claim.clone());
        let op2 = ClaimsOperation::new(claim);

        let receipt1 = op1.generate_receipt(OperationStatus::Success);
        let receipt2 = op2.generate_receipt(OperationStatus::Success);

        // Same claim should produce same receipt
        assert_eq!(receipt1.merkle_root, receipt2.merkle_root);
    }
}
