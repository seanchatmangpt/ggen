//! Proof Carrier for Overlays - Auditable Justification for ΔΣ Proposals
//!
//! A ΔΣ proposal is only actionable if it carries auditable proof:
//! - Evidence chain from Γ observations
//! - Test results validating the change
//! - Expected improvements with impact metrics
//! - Risk assessment and mitigation strategies
//! - Doctrine alignment verification

use super::ahi_contract::{AHIError, Proposal};
use super::ontology_proposal_engine::OntologySigmaProposal;
use serde::{Deserialize, Serialize};
use std::collections::HashMap;

/// A single test result for validating a proposal
#[derive(Debug, Clone, Serialize, Deserialize, PartialEq)]
pub struct TestResult {
    pub test_id: String,
    pub test_name: String,
    pub category: TestCategory,
    pub passed: bool,
    pub message: String,
    pub execution_time_ms: f64,
    pub assertions_total: usize,
    pub assertions_passed: usize,
}

/// Test categories for proposal validation
#[derive(Debug, Clone, Copy, PartialEq, Eq, Serialize, Deserialize)]
pub enum TestCategory {
    /// Unit tests for isolated logic
    Unit,
    /// Integration tests with other components
    Integration,
    /// Performance benchmarks
    Performance,
    /// Doctrine constraint validation
    DoctrineCompliance,
    /// Backward compatibility checks
    Compatibility,
    /// Regression prevention tests
    Regression,
}

impl std::fmt::Display for TestCategory {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            TestCategory::Unit => write!(f, "Unit"),
            TestCategory::Integration => write!(f, "Integration"),
            TestCategory::Performance => write!(f, "Performance"),
            TestCategory::DoctrineCompliance => write!(f, "DoctrineCompliance"),
            TestCategory::Compatibility => write!(f, "Compatibility"),
            TestCategory::Regression => write!(f, "Regression"),
        }
    }
}

/// Evidence link in the justification chain
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct EvidenceLink {
    /// Observation or finding ID from Γ
    pub source_id: String,
    /// Type of evidence (metric, anomaly, user_report, etc.)
    pub evidence_type: String,
    /// Extracted metric or property
    pub property: String,
    /// Value supporting the proposal
    pub value: f64,
    /// Weight in overall justification (0-1)
    pub weight: f64,
    /// Timestamp when evidence was observed
    pub timestamp: u64,
}

/// Impact projection for a proposal
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct ImpactProjection {
    /// Estimated coverage improvement (0-100%)
    pub coverage_improvement: f64,
    /// Expected performance delta (negative = improvement)
    pub performance_delta_ticks: f64,
    /// Adoption impact (estimated tenant adoption increase)
    pub adoption_impact: f64,
    /// Revenue impact (estimated % change)
    pub revenue_impact: f64,
    /// Guard compliance improvement (0-100%)
    pub compliance_improvement: f64,
    /// Confidence level in projections (0-1)
    pub confidence: f64,
}

/// Risk mitigation strategy
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct RiskMitigation {
    pub risk_id: String,
    pub risk_description: String,
    pub risk_probability: f64, // 0-1
    pub risk_impact: f64,      // 0-100
    pub mitigation_strategy: String,
    pub rollback_plan: String,
    pub monitoring_alerts: Vec<String>,
}

/// Proof Carrier - ΔΣ proposal with complete auditable justification
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct ProofCarrier {
    /// Unique ID for this proof carrier
    pub id: String,

    /// The actual proposal being justified
    pub proposal: OntologySigmaProposal,

    /// Complete evidence chain from Γ
    pub evidence_chain: Vec<EvidenceLink>,

    /// Test results validating the proposal
    pub validation_tests: Vec<TestResult>,

    /// Expected impact projections
    pub impact_projection: ImpactProjection,

    /// Risk analysis and mitigation
    pub risk_mitigations: Vec<RiskMitigation>,

    /// Overall risk score (0-100, 0=safe, 100=dangerous)
    pub total_risk_score: f64,

    /// Doctrine alignment verification details
    pub doctrine_checks: HashMap<String, bool>,

    /// Approval chain (who approved, when)
    pub approvals: Vec<Approval>,

    /// Proof status
    pub status: ProofStatus,

    /// Timestamp when proof was created
    pub created_at: u64,

    /// Expiry timestamp (proofs may become stale)
    pub expires_at: u64,
}

/// Approval entry in the chain
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct Approval {
    pub approver_id: String,
    pub approved_at: u64,
    pub reason: String,
}

/// Status of a proof carrier
#[derive(Debug, Clone, Copy, PartialEq, Eq, Serialize, Deserialize)]
pub enum ProofStatus {
    /// Generated but not yet fully tested
    Pending,
    /// All tests passed, ready for review
    TestsPassed,
    /// Approved by governance
    Approved,
    /// Executed (overlay applied)
    Executed,
    /// Rejected (failed tests or doctrine)
    Rejected,
    /// Reverted after execution
    Reverted,
    /// Expired (proof is too old)
    Expired,
}

impl std::fmt::Display for ProofStatus {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            ProofStatus::Pending => write!(f, "Pending"),
            ProofStatus::TestsPassed => write!(f, "TestsPassed"),
            ProofStatus::Approved => write!(f, "Approved"),
            ProofStatus::Executed => write!(f, "Executed"),
            ProofStatus::Rejected => write!(f, "Rejected"),
            ProofStatus::Reverted => write!(f, "Reverted"),
            ProofStatus::Expired => write!(f, "Expired"),
        }
    }
}

impl ProofCarrier {
    /// Create a new proof carrier for a proposal
    pub fn new(proposal: OntologySigmaProposal, expires_in_seconds: u64) -> Self {
        let now = std::time::SystemTime::now()
            .duration_since(std::time::UNIX_EPOCH)
            .unwrap_or_default()
            .as_secs();

        Self {
            id: format!("proof-{}-{}", proposal.id, now),
            proposal,
            evidence_chain: Vec::new(),
            validation_tests: Vec::new(),
            impact_projection: ImpactProjection {
                coverage_improvement: 0.0,
                performance_delta_ticks: 0.0,
                adoption_impact: 0.0,
                revenue_impact: 0.0,
                compliance_improvement: 0.0,
                confidence: 0.0,
            },
            risk_mitigations: Vec::new(),
            total_risk_score: 0.0,
            doctrine_checks: HashMap::new(),
            approvals: Vec::new(),
            status: ProofStatus::Pending,
            created_at: now,
            expires_at: now + expires_in_seconds,
        }
    }

    /// Add evidence from Γ observation
    pub fn add_evidence(&mut self, link: EvidenceLink) -> Result<(), AHIError> {
        if link.weight <= 0.0 || link.weight > 1.0 {
            return Err(AHIError::InvalidConfig(
                "Evidence weight must be between 0 and 1".to_string(),
            ));
        }
        self.evidence_chain.push(link);
        Ok(())
    }

    /// Add a validation test result
    pub fn add_test_result(&mut self, test: TestResult) {
        self.validation_tests.push(test);
    }

    /// Set impact projection
    pub fn set_impact(&mut self, impact: ImpactProjection) {
        self.impact_projection = impact;
    }

    /// Add a risk mitigation strategy
    pub fn add_risk_mitigation(&mut self, mitigation: RiskMitigation) {
        self.risk_mitigations.push(mitigation);
    }

    /// Verify doctrine compliance for the proposal
    pub fn verify_doctrine(&mut self, constraints: &[(String, bool)]) {
        self.doctrine_checks = constraints.iter().cloned().collect();
    }

    /// Calculate composite risk score
    pub fn calculate_risk_score(&mut self) -> f64 {
        if self.risk_mitigations.is_empty() {
            return self.proposal.risk_score;
        }

        let mut weighted_risk = self.proposal.risk_score * 0.5;

        for mitigation in &self.risk_mitigations {
            let risk_contribution = mitigation.risk_probability * mitigation.risk_impact;
            weighted_risk += risk_contribution * 0.25 / self.risk_mitigations.len() as f64;
        }

        let test_pass_rate = if self.validation_tests.is_empty() {
            0.0
        } else {
            let passed = self.validation_tests.iter().filter(|t| t.passed).count();
            (passed as f64 / self.validation_tests.len() as f64) * 0.25
        };

        self.total_risk_score = (weighted_risk - test_pass_rate).max(0.0).min(100.0);
        self.total_risk_score
    }

    /// Check if all tests passed
    pub fn all_tests_passed(&self) -> bool {
        if self.validation_tests.is_empty() {
            return false;
        }
        self.validation_tests.iter().all(|t| t.passed)
    }

    /// Check if doctrine aligned
    pub fn is_doctrine_aligned(&self) -> bool {
        if self.doctrine_checks.is_empty() {
            return self.proposal.doctrine_aligned;
        }
        self.doctrine_checks.values().all(|&v| v)
    }

    /// Check if proof is fresh (not expired)
    pub fn is_fresh(&self) -> bool {
        let now = std::time::SystemTime::now()
            .duration_since(std::time::UNIX_EPOCH)
            .unwrap_or_default()
            .as_secs();
        now <= self.expires_at
    }

    /// Mark as approved
    pub fn approve(&mut self, approver_id: &str, reason: &str) -> Result<(), AHIError> {
        if self.status != ProofStatus::TestsPassed {
            return Err(AHIError::InvalidConfig(
                "Can only approve from TestsPassed status".to_string(),
            ));
        }
        self.approvals.push(Approval {
            approver_id: approver_id.to_string(),
            approved_at: std::time::SystemTime::now()
                .duration_since(std::time::UNIX_EPOCH)
                .unwrap_or_default()
                .as_secs(),
            reason: reason.to_string(),
        });
        self.status = ProofStatus::Approved;
        Ok(())
    }

    /// Finalize validation tests - move to TestsPassed if all passed
    pub fn finalize_tests(&mut self) -> Result<(), AHIError> {
        if self.validation_tests.is_empty() {
            return Err(AHIError::InvalidConfig(
                "No test results recorded".to_string(),
            ));
        }

        if self.all_tests_passed() {
            self.status = ProofStatus::TestsPassed;
            Ok(())
        } else {
            self.status = ProofStatus::Rejected;
            let failed_tests: Vec<_> = self
                .validation_tests
                .iter()
                .filter(|t| !t.passed)
                .map(|t| t.test_name.clone())
                .collect();
            Err(AHIError::InsufficientJustification(format!(
                "Tests failed: {:?}",
                failed_tests
            )))
        }
    }

    /// Validate complete proof carrier for promotion
    pub fn validate_for_promotion(&mut self) -> Result<(), AHIError> {
        // Refresh freshness check
        if !self.is_fresh() {
            self.status = ProofStatus::Expired;
            return Err(AHIError::InvalidConfig("Proof carrier expired".to_string()));
        }

        // Validate evidence chain
        if self.evidence_chain.is_empty() {
            return Err(AHIError::InsufficientJustification(
                "No evidence chain for proposal".to_string(),
            ));
        }

        // Validate doctrine alignment
        if !self.is_doctrine_aligned() {
            return Err(AHIError::DoctrineViolation(
                "Proposal not doctrine-aligned".to_string(),
            ));
        }

        // Calculate final risk score
        self.calculate_risk_score();
        if self.total_risk_score > 75.0 {
            return Err(AHIError::InvalidConfig(format!(
                "Risk score too high: {}",
                self.total_risk_score
            )));
        }

        Ok(())
    }

    /// Get audit trail (evidence + approvals + tests)
    pub fn audit_trail(&self) -> Vec<String> {
        let mut trail = Vec::new();

        trail.push(format!("Proof ID: {} (Status: {})", self.id, self.status));
        trail.push(format!("Proposal: {}", self.proposal.what()));
        trail.push(format!(
            "Evidence chain: {} links (total weight: {})",
            self.evidence_chain.len(),
            self.evidence_chain.iter().map(|e| e.weight).sum::<f64>()
        ));
        trail.push(format!(
            "Tests: {} total, {} passed",
            self.validation_tests.len(),
            self.validation_tests.iter().filter(|t| t.passed).count()
        ));
        trail.push(format!(
            "Risk score: {} (base: {})",
            self.total_risk_score, self.proposal.risk_score
        ));
        trail.push(format!("Doctrine aligned: {}", self.is_doctrine_aligned()));

        for approval in &self.approvals {
            trail.push(format!(
                "Approved by {} at timestamp {}",
                approval.approver_id, approval.approved_at
            ));
        }

        trail
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::ontology_proposal_engine::{OntologySigmaProposal, SigmaChangeKind};

    #[test]
    fn test_proof_carrier_creation() {
        let proposal = OntologySigmaProposal {
            id: "prop-1".to_string(),
            change_kind: SigmaChangeKind::NewConcept,
            element_name: "test_concept".to_string(),
            element_type: "Concept".to_string(),
            current_definition: None,
            proposed_definition: "Test concept definition".to_string(),
            justification_evidence: vec!["obs-1".to_string()],
            estimated_coverage_improvement: 15.0,
            estimated_performance_delta: -50.0,
            risk_score: 35.0,
            affected_patterns: vec![],
            affected_guards: vec![],
            doctrine_aligned: true,
        };

        let carrier = ProofCarrier::new(proposal, 86400); // 1 day expiry
        assert_eq!(carrier.status, ProofStatus::Pending);
        assert!(carrier.is_fresh());
    }

    #[test]
    fn test_evidence_chain() {
        let proposal = OntologySigmaProposal {
            id: "prop-2".to_string(),
            change_kind: SigmaChangeKind::NewPattern,
            element_name: "pattern_optimizer".to_string(),
            element_type: "Pattern".to_string(),
            current_definition: None,
            proposed_definition: "Optimized pattern".to_string(),
            justification_evidence: vec!["obs-1".to_string(), "obs-2".to_string()],
            estimated_coverage_improvement: 20.0,
            estimated_performance_delta: -100.0,
            risk_score: 40.0,
            affected_patterns: vec!["transaction_processing".to_string()],
            affected_guards: vec![],
            doctrine_aligned: true,
        };

        let mut carrier = ProofCarrier::new(proposal, 86400);

        let evidence = EvidenceLink {
            source_id: "obs-1".to_string(),
            evidence_type: "metric".to_string(),
            property: "latency_p99".to_string(),
            value: 250.0,
            weight: 0.6,
            timestamp: 1000,
        };

        assert!(carrier.add_evidence(evidence).is_ok());
        assert_eq!(carrier.evidence_chain.len(), 1);
    }

    #[test]
    fn test_test_results_and_validation() {
        let proposal = OntologySigmaProposal {
            id: "prop-3".to_string(),
            change_kind: SigmaChangeKind::GuardAdjustment,
            element_name: "guard_threshold".to_string(),
            element_type: "Guard".to_string(),
            current_definition: Some("Legacy threshold".to_string()),
            proposed_definition: "Adaptive threshold".to_string(),
            justification_evidence: vec!["obs-1".to_string()],
            estimated_coverage_improvement: 10.0,
            estimated_performance_delta: 0.0,
            risk_score: 25.0,
            affected_patterns: vec![],
            affected_guards: vec!["error_threshold".to_string()],
            doctrine_aligned: true,
        };

        let mut carrier = ProofCarrier::new(proposal, 86400);

        let test1 = TestResult {
            test_id: "test-unit-1".to_string(),
            test_name: "threshold_calculation".to_string(),
            category: TestCategory::Unit,
            passed: true,
            message: "Passed".to_string(),
            execution_time_ms: 10.5,
            assertions_total: 5,
            assertions_passed: 5,
        };

        carrier.add_test_result(test1);
        assert!(carrier.all_tests_passed());
        assert!(carrier.finalize_tests().is_ok());
        assert_eq!(carrier.status, ProofStatus::TestsPassed);
    }

    #[test]
    fn test_risk_score_calculation() {
        let proposal = OntologySigmaProposal {
            id: "prop-4".to_string(),
            change_kind: SigmaChangeKind::NewConcept,
            element_name: "high_risk_concept".to_string(),
            element_type: "Concept".to_string(),
            current_definition: None,
            proposed_definition: "Risky concept".to_string(),
            justification_evidence: vec!["obs-1".to_string()],
            estimated_coverage_improvement: 30.0,
            estimated_performance_delta: 200.0,
            risk_score: 70.0, // High base risk
            affected_patterns: vec![],
            affected_guards: vec![],
            doctrine_aligned: true,
        };

        let mut carrier = ProofCarrier::new(proposal, 86400);

        let mitigation = RiskMitigation {
            risk_id: "risk-1".to_string(),
            risk_description: "Performance degradation".to_string(),
            risk_probability: 0.3,
            risk_impact: 50.0,
            mitigation_strategy: "Add monitoring and alerts".to_string(),
            rollback_plan: "Revert to previous version".to_string(),
            monitoring_alerts: vec!["perf_degradation_alert".to_string()],
        };

        carrier.add_risk_mitigation(mitigation);

        // Add test that passes
        let test = TestResult {
            test_id: "test-perf".to_string(),
            test_name: "performance_regression".to_string(),
            category: TestCategory::Performance,
            passed: true,
            message: "No regression detected".to_string(),
            execution_time_ms: 500.0,
            assertions_total: 10,
            assertions_passed: 10,
        };

        carrier.add_test_result(test);

        let risk_score = carrier.calculate_risk_score();
        assert!(risk_score < 70.0); // Should be reduced by mitigation + passing tests
    }

    #[test]
    fn test_approval_chain() {
        let proposal = OntologySigmaProposal {
            id: "prop-5".to_string(),
            change_kind: SigmaChangeKind::NewPattern,
            element_name: "approved_pattern".to_string(),
            element_type: "Pattern".to_string(),
            current_definition: None,
            proposed_definition: "Pattern to be approved".to_string(),
            justification_evidence: vec!["obs-1".to_string()],
            estimated_coverage_improvement: 15.0,
            estimated_performance_delta: -50.0,
            risk_score: 30.0,
            affected_patterns: vec![],
            affected_guards: vec![],
            doctrine_aligned: true,
        };

        let mut carrier = ProofCarrier::new(proposal, 86400);

        // Move to TestsPassed
        let test = TestResult {
            test_id: "test-1".to_string(),
            test_name: "basic_validation".to_string(),
            category: TestCategory::Unit,
            passed: true,
            message: "OK".to_string(),
            execution_time_ms: 5.0,
            assertions_total: 3,
            assertions_passed: 3,
        };
        carrier.add_test_result(test);
        assert!(carrier.finalize_tests().is_ok());

        // Add approval
        assert!(carrier.approve("reviewer-1", "Code review passed").is_ok());
        assert_eq!(carrier.status, ProofStatus::Approved);
        assert_eq!(carrier.approvals.len(), 1);
    }

    #[test]
    fn test_audit_trail() {
        let proposal = OntologySigmaProposal {
            id: "prop-6".to_string(),
            change_kind: SigmaChangeKind::NewConcept,
            element_name: "audited_concept".to_string(),
            element_type: "Concept".to_string(),
            current_definition: None,
            proposed_definition: "Concept with audit trail".to_string(),
            justification_evidence: vec!["obs-1".to_string()],
            estimated_coverage_improvement: 25.0,
            estimated_performance_delta: -75.0,
            risk_score: 35.0,
            affected_patterns: vec![],
            affected_guards: vec![],
            doctrine_aligned: true,
        };

        let mut carrier = ProofCarrier::new(proposal, 86400);

        let evidence = EvidenceLink {
            source_id: "obs-1".to_string(),
            evidence_type: "anomaly".to_string(),
            property: "error_spike".to_string(),
            value: 5000.0,
            weight: 0.8,
            timestamp: 1000,
        };

        assert!(carrier.add_evidence(evidence).is_ok());

        let trail = carrier.audit_trail();
        assert!(trail.len() > 0);
        assert!(trail.join(" ").contains("Evidence chain"));
    }
}
