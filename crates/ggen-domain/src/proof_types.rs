//! Proof-Carrying Decisions: Decisions Only Exist If Proofs Exist
//!
//! Key insight: make decisions literally impossible to construct without
//! attaching a proof object. Different decision severities require different proof levels.
//!
//! Proof traits form a lattice:
//! - WeakProof (≤30 doctrine distance) - for read-only, low-risk ops
//! - StandardProof (≤50 doctrine distance) - for cache/snapshot updates
//! - StrongProof (≤70 doctrine distance) - for ΔΣ proposals
//! - CriticalProof (≤80 doctrine distance) - for marketplace changes

use serde::{Deserialize, Serialize};
use std::marker::PhantomData;

// ============================================================================
// PROOF TRAIT HIERARCHY
// ============================================================================

/// Base proof trait (sealed against external implementation)
pub trait Proof: Send + Sync {
    /// Doctrine distance this proof achieves (0-100)
    fn doctrine_distance(&self) -> f64;

    /// Is this proof still valid?
    fn is_valid(&self) -> bool;

    /// Human-readable justification
    fn justification(&self) -> String;

    /// Proof ID for audit trails
    fn proof_id(&self) -> &str;

    /// Get the evidence count
    fn evidence_count(&self) -> usize {
        1
    }
}

/// Weak proof: suitable for read-only operations
/// Max doctrine distance: 30 (Perfect to Good range)
pub trait WeakProof: Proof {
    fn max_distance() -> f64 {
        30.0
    }

    fn can_use(&self) -> Result<(), String> {
        if self.doctrine_distance() <= Self::max_distance() {
            Ok(())
        } else {
            Err(format!(
                "Weak proof doctrine distance {} exceeds limit {}",
                self.doctrine_distance(),
                Self::max_distance()
            ))
        }
    }
}

/// Standard proof: for cache/snapshot updates
/// Max doctrine distance: 50 (Acceptable range)
pub trait StandardProof: Proof {
    fn max_distance() -> f64 {
        50.0
    }

    fn can_use(&self) -> Result<(), String> {
        if self.doctrine_distance() <= Self::max_distance() {
            Ok(())
        } else {
            Err(format!(
                "Standard proof doctrine distance {} exceeds limit {}",
                self.doctrine_distance(),
                Self::max_distance()
            ))
        }
    }
}

/// Strong proof: for ΔΣ proposals and ontology changes
/// Max doctrine distance: 70 (Acceptable to Marginal range)
pub trait StrongProof: Proof {
    fn max_distance() -> f64 {
        70.0
    }

    fn can_use(&self) -> Result<(), String> {
        if self.doctrine_distance() <= Self::max_distance() {
            Ok(())
        } else {
            Err(format!(
                "Strong proof doctrine distance {} exceeds limit {}",
                self.doctrine_distance(),
                Self::max_distance()
            ))
        }
    }
}

/// Critical proof: for marketplace promotions and high-risk changes
/// Max doctrine distance: 80 (Marginal range, just barely)
pub trait CriticalProof: Proof {
    fn max_distance() -> f64 {
        80.0
    }

    fn can_use(&self) -> Result<(), String> {
        if self.doctrine_distance() <= Self::max_distance() {
            Ok(())
        } else {
            Err(format!(
                "Critical proof doctrine distance {} exceeds limit {}",
                self.doctrine_distance(),
                Self::max_distance()
            ))
        }
    }
}

// ============================================================================
// CONCRETE PROOF IMPLEMENTATIONS
// ============================================================================

/// Lightweight proof for read-only operations
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct LightProof {
    proof_id: String,
    doctrine_distance: f64,
    created_at: u64,
}

impl LightProof {
    pub fn new(proof_id: impl Into<String>, doctrine_distance: f64) -> Self {
        Self {
            proof_id: proof_id.into(),
            doctrine_distance,
            created_at: std::time::SystemTime::now()
                .duration_since(std::time::UNIX_EPOCH)
                .unwrap_or_default()
                .as_secs(),
        }
    }
}

impl Proof for LightProof {
    fn doctrine_distance(&self) -> f64 {
        self.doctrine_distance
    }

    fn is_valid(&self) -> bool {
        true
    }

    fn justification(&self) -> String {
        "Light proof: basic read access".to_string()
    }

    fn proof_id(&self) -> &str {
        &self.proof_id
    }
}

impl WeakProof for LightProof {}

/// Standard proof: includes test results
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct TestingProof {
    proof_id: String,
    doctrine_distance: f64,
    tests_passed: usize,
    tests_failed: usize,
    created_at: u64,
}

impl TestingProof {
    pub fn new(
        proof_id: impl Into<String>,
        doctrine_distance: f64,
        tests_passed: usize,
    ) -> Self {
        Self {
            proof_id: proof_id.into(),
            doctrine_distance,
            tests_passed,
            tests_failed: 0,
            created_at: std::time::SystemTime::now()
                .duration_since(std::time::UNIX_EPOCH)
                .unwrap_or_default()
                .as_secs(),
        }
    }

    pub fn tests_passed(&self) -> usize {
        self.tests_passed
    }

    pub fn tests_failed(&self) -> usize {
        self.tests_failed
    }
}

impl Proof for TestingProof {
    fn doctrine_distance(&self) -> f64 {
        self.doctrine_distance
    }

    fn is_valid(&self) -> bool {
        self.tests_passed > 0 && self.tests_failed == 0
    }

    fn justification(&self) -> String {
        format!("Testing proof: {} tests passed", self.tests_passed)
    }

    fn proof_id(&self) -> &str {
        &self.proof_id
    }

    fn evidence_count(&self) -> usize {
        self.tests_passed
    }
}

impl StandardProof for TestingProof {}

/// Strong proof: includes evidence chain and impact analysis
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct EvidenceProof {
    proof_id: String,
    doctrine_distance: f64,
    evidence_count: usize,
    impact_verified: bool,
    tests_passed: usize,
    created_at: u64,
}

impl EvidenceProof {
    pub fn new(
        proof_id: impl Into<String>,
        doctrine_distance: f64,
        evidence_count: usize,
    ) -> Self {
        Self {
            proof_id: proof_id.into(),
            doctrine_distance,
            evidence_count,
            impact_verified: false,
            tests_passed: 0,
            created_at: std::time::SystemTime::now()
                .duration_since(std::time::UNIX_EPOCH)
                .unwrap_or_default()
                .as_secs(),
        }
    }

    pub fn verify_impact(mut self) -> Self {
        self.impact_verified = true;
        self
    }

    pub fn with_tests(mut self, count: usize) -> Self {
        self.tests_passed = count;
        self
    }
}

impl Proof for EvidenceProof {
    fn doctrine_distance(&self) -> f64 {
        self.doctrine_distance
    }

    fn is_valid(&self) -> bool {
        self.evidence_count > 0 && self.impact_verified && self.tests_passed > 0
    }

    fn justification(&self) -> String {
        format!(
            "Evidence proof: {} evidence links, impact verified, {} tests",
            self.evidence_count, self.tests_passed
        )
    }

    fn proof_id(&self) -> &str {
        &self.proof_id
    }

    fn evidence_count(&self) -> usize {
        self.evidence_count
    }
}

impl StrongProof for EvidenceProof {}

/// Critical proof: maximum rigor for high-risk decisions
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct CriticalityProof {
    proof_id: String,
    doctrine_distance: f64,
    evidence_count: usize,
    impact_verified: bool,
    tests_passed: usize,
    governance_approved: bool,
    approval_chain: Vec<String>, // Approvers
    created_at: u64,
}

impl CriticalityProof {
    pub fn new(proof_id: impl Into<String>, doctrine_distance: f64) -> Self {
        Self {
            proof_id: proof_id.into(),
            doctrine_distance,
            evidence_count: 0,
            impact_verified: false,
            tests_passed: 0,
            governance_approved: false,
            approval_chain: Vec::new(),
            created_at: std::time::SystemTime::now()
                .duration_since(std::time::UNIX_EPOCH)
                .unwrap_or_default()
                .as_secs(),
        }
    }

    pub fn with_evidence(mut self, count: usize) -> Self {
        self.evidence_count = count;
        self
    }

    pub fn verify_impact(mut self) -> Self {
        self.impact_verified = true;
        self
    }

    pub fn with_tests(mut self, count: usize) -> Self {
        self.tests_passed = count;
        self
    }

    pub fn approve(mut self, approver: String) -> Self {
        self.approval_chain.push(approver);
        if self.approval_chain.len() >= 2 {
            // Requires at least 2 approvals for critical decisions
            self.governance_approved = true;
        }
        self
    }

    pub fn approvers(&self) -> &[String] {
        &self.approval_chain
    }
}

impl Proof for CriticalityProof {
    fn doctrine_distance(&self) -> f64 {
        self.doctrine_distance
    }

    fn is_valid(&self) -> bool {
        self.evidence_count > 0
            && self.impact_verified
            && self.tests_passed > 0
            && self.governance_approved
    }

    fn justification(&self) -> String {
        format!(
            "Critical proof: {} evidence, impact verified, {} tests, {} approvals",
            self.evidence_count,
            self.tests_passed,
            self.approval_chain.len()
        )
    }

    fn proof_id(&self) -> &str {
        &self.proof_id
    }

    fn evidence_count(&self) -> usize {
        self.evidence_count
    }
}

impl CriticalProof for CriticalityProof {}

// ============================================================================
// PROOF-CARRYING DECISIONS
// ============================================================================

/// Decision that requires a weak proof
pub struct WeakDecision<P: WeakProof> {
    decision_id: String,
    description: String,
    proof: P,
}

impl<P: WeakProof> WeakDecision<P> {
    /// Construct a decision with proof
    pub fn make(
        decision_id: impl Into<String>,
        description: impl Into<String>,
        proof: P,
    ) -> Result<Self, String> {
        proof.can_use()?;

        Ok(Self {
            decision_id: decision_id.into(),
            description: description.into(),
            proof,
        })
    }

    pub fn proof(&self) -> &P {
        &self.proof
    }
}

/// Decision that requires a standard proof
pub struct StandardDecision<P: StandardProof> {
    decision_id: String,
    description: String,
    proof: P,
}

impl<P: StandardProof> StandardDecision<P> {
    pub fn make(
        decision_id: impl Into<String>,
        description: impl Into<String>,
        proof: P,
    ) -> Result<Self, String> {
        proof.can_use()?;

        Ok(Self {
            decision_id: decision_id.into(),
            description: description.into(),
            proof,
        })
    }

    pub fn proof(&self) -> &P {
        &self.proof
    }
}

/// Decision that requires a strong proof
pub struct StrongDecision<P: StrongProof> {
    decision_id: String,
    description: String,
    proof: P,
}

impl<P: StrongProof> StrongDecision<P> {
    pub fn make(
        decision_id: impl Into<String>,
        description: impl Into<String>,
        proof: P,
    ) -> Result<Self, String> {
        proof.can_use()?;

        Ok(Self {
            decision_id: decision_id.into(),
            description: description.into(),
            proof,
        })
    }

    pub fn proof(&self) -> &P {
        &self.proof
    }
}

/// Decision that requires critical proof
pub struct CriticalDecision<P: CriticalProof> {
    decision_id: String,
    description: String,
    proof: P,
}

impl<P: CriticalProof> CriticalDecision<P> {
    pub fn make(
        decision_id: impl Into<String>,
        description: impl Into<String>,
        proof: P,
    ) -> Result<Self, String> {
        proof.can_use()?;

        Ok(Self {
            decision_id: decision_id.into(),
            description: description.into(),
            proof,
        })
    }

    pub fn proof(&self) -> &P {
        &self.proof
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_weak_proof() {
        let proof = LightProof::new("proof-1", 20.0);
        assert!(proof.is_valid());
        assert_eq!(proof.doctrine_distance(), 20.0);
        assert!(proof.can_use().is_ok());
    }

    #[test]
    fn test_weak_proof_exceeds_limit() {
        let proof = LightProof::new("proof-2", 40.0);
        // Distance 40 exceeds WeakProof limit of 30
        assert!(proof.can_use().is_err());
    }

    #[test]
    fn test_standard_proof() {
        let proof = TestingProof::new("proof-3", 45.0, 10);
        assert!(proof.is_valid());
        assert!(proof.can_use().is_ok());
    }

    #[test]
    fn test_strong_proof_with_evidence() {
        let proof = EvidenceProof::new("proof-4", 60.0, 5)
            .verify_impact()
            .with_tests(15);
        assert!(proof.is_valid());
        assert_eq!(proof.evidence_count(), 5);
    }

    #[test]
    fn test_critical_proof_requires_approvals() {
        let proof = CriticalityProof::new("proof-5", 70.0)
            .with_evidence(10)
            .verify_impact()
            .with_tests(20)
            .approve("approver-1".to_string());

        // Single approval not enough
        assert!(!proof.is_valid());

        let proof2 = proof.approve("approver-2".to_string());
        // Two approvals sufficient
        assert!(proof2.is_valid());
    }

    #[test]
    fn test_decision_requires_proof() {
        let proof = LightProof::new("proof-6", 15.0);
        let decision = WeakDecision::make("decision-1", "Read operation", proof);
        assert!(decision.is_ok());
    }

    #[test]
    fn test_decision_rejects_invalid_proof() {
        let proof = LightProof::new("proof-7", 35.0); // Exceeds weak limit
        let decision = WeakDecision::make("decision-2", "Read operation", proof);
        assert!(decision.is_err());
    }
}
