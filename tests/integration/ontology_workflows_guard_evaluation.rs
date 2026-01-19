//! Guard Evaluation Integration Tests (12 Guards)
//!
//! Tests the complete guard evaluation system that validates ontology-based
//! proposals against 12 critical guards covering policy, security, infrastructure,
//! ownership, compliance, and operational concerns.
//!
//! **Guards Tested**:
//! 1. Policy Guard - Policy compliance
//! 2. Security Guard - Security requirements
//! 3. Infrastructure Guard - Infrastructure readiness
//! 4. Ownership Guard - Resource ownership verification
//! 5. Compliance Guard - Regulatory compliance
//! 6. Encryption Guard - Data encryption requirements
//! 7. Audit Trail Guard - Audit logging requirements
//! 8. Access Control Guard - RBAC implementation
//! 9. Data Residency Guard - Geographic data location
//! 10. Redundancy Guard - High availability setup
//! 11. Disaster Recovery Guard - Backup and recovery
//! 12. Incident Response Guard - Incident handling procedures

use serde::{Deserialize, Serialize};
use std::collections::BTreeMap;

/// Result of guard evaluation
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct GuardEvaluationResult {
    pub guard_id: usize,
    pub guard_name: String,
    pub guard_type: String,
    pub passed: bool,
    pub message: String,
    pub proof: String, // Evidence supporting the result
}

/// Ontology proposal to be evaluated
#[derive(Debug, Clone)]
pub struct OntologyProposal {
    pub name: String,
    pub security_posture: String, // "enterprise" or "basic"
    pub infrastructure_ready: bool,
    pub owner_verified: bool,
    pub policies: Vec<String>,
    pub encryption_enabled: bool,
    pub audit_logging_enabled: bool,
    pub rbac_configured: bool,
    pub data_region: String,
    pub redundancy_level: usize, // 1=none, 2=standby, 3+=active-active
    pub backup_enabled: bool,
    pub incident_plan: bool,
}

impl OntologyProposal {
    /// Create a fully compliant proposal (all guards pass)
    pub fn fully_compliant() -> Self {
        Self {
            name: "Compliant-Proposal".to_string(),
            security_posture: "enterprise".to_string(),
            infrastructure_ready: true,
            owner_verified: true,
            policies: vec![
                "SOC2-TypeII".to_string(),
                "ISO27001".to_string(),
                "HIPAA".to_string(),
            ],
            encryption_enabled: true,
            audit_logging_enabled: true,
            rbac_configured: true,
            data_region: "us-east-1".to_string(),
            redundancy_level: 3,
            backup_enabled: true,
            incident_plan: true,
        }
    }

    /// Create a proposal with missing encryption (guard 6 fails)
    pub fn no_encryption() -> Self {
        let mut proposal = Self::fully_compliant();
        proposal.encryption_enabled = false;
        proposal.name = "No-Encryption-Proposal".to_string();
        proposal
    }

    /// Create a proposal with no audit logging (guard 7 fails)
    pub fn no_audit() -> Self {
        let mut proposal = Self::fully_compliant();
        proposal.audit_logging_enabled = false;
        proposal.name = "No-Audit-Proposal".to_string();
        proposal
    }

    /// Create a proposal with no disaster recovery (guard 11 fails)
    pub fn no_dr() -> Self {
        let mut proposal = Self::fully_compliant();
        proposal.backup_enabled = false;
        proposal.redundancy_level = 1;
        proposal.name = "No-DR-Proposal".to_string();
        proposal
    }
}

/// Evaluate all 12 guards against a proposal
pub fn evaluate_all_guards(proposal: &OntologyProposal) -> Vec<GuardEvaluationResult> {
    vec![
        evaluate_guard_1_policy(proposal),
        evaluate_guard_2_security(proposal),
        evaluate_guard_3_infrastructure(proposal),
        evaluate_guard_4_ownership(proposal),
        evaluate_guard_5_compliance(proposal),
        evaluate_guard_6_encryption(proposal),
        evaluate_guard_7_audit_trail(proposal),
        evaluate_guard_8_access_control(proposal),
        evaluate_guard_9_data_residency(proposal),
        evaluate_guard_10_redundancy(proposal),
        evaluate_guard_11_disaster_recovery(proposal),
        evaluate_guard_12_incident_response(proposal),
    ]
}

/// Guard 1: Policy Compliance
fn evaluate_guard_1_policy(proposal: &OntologyProposal) -> GuardEvaluationResult {
    let passed = !proposal.policies.is_empty();
    GuardEvaluationResult {
        guard_id: 1,
        guard_name: "Policy Compliance".to_string(),
        guard_type: "policy".to_string(),
        passed,
        message: if passed {
            "At least one policy framework is specified".to_string()
        } else {
            "No policy frameworks specified".to_string()
        },
        proof: format!("Policies: {:?}", proposal.policies),
    }
}

/// Guard 2: Security Posture
fn evaluate_guard_2_security(proposal: &OntologyProposal) -> GuardEvaluationResult {
    let passed = proposal.security_posture == "enterprise";
    GuardEvaluationResult {
        guard_id: 2,
        guard_name: "Security Posture".to_string(),
        guard_type: "security".to_string(),
        passed,
        message: if passed {
            "Enterprise-grade security posture required".to_string()
        } else {
            "Security posture must be enterprise".to_string()
        },
        proof: format!("Security posture: {}", proposal.security_posture),
    }
}

/// Guard 3: Infrastructure Readiness
fn evaluate_guard_3_infrastructure(proposal: &OntologyProposal) -> GuardEvaluationResult {
    let passed = proposal.infrastructure_ready;
    GuardEvaluationResult {
        guard_id: 3,
        guard_name: "Infrastructure Readiness".to_string(),
        guard_type: "infrastructure".to_string(),
        passed,
        message: if passed {
            "Infrastructure is production-ready".to_string()
        } else {
            "Infrastructure must be production-ready".to_string()
        },
        proof: format!("Infrastructure ready: {}", proposal.infrastructure_ready),
    }
}

/// Guard 4: Ownership Verification
fn evaluate_guard_4_ownership(proposal: &OntologyProposal) -> GuardEvaluationResult {
    let passed = proposal.owner_verified;
    GuardEvaluationResult {
        guard_id: 4,
        guard_name: "Ownership Verification".to_string(),
        guard_type: "ownership".to_string(),
        passed,
        message: if passed {
            "Resource ownership is verified".to_string()
        } else {
            "Resource ownership must be verified".to_string()
        },
        proof: format!("Owner verified: {}", proposal.owner_verified),
    }
}

/// Guard 5: Compliance with Regulations
fn evaluate_guard_5_compliance(proposal: &OntologyProposal) -> GuardEvaluationResult {
    let passed = proposal.policies.contains(&"SOC2-TypeII".to_string())
        || proposal.policies.contains(&"ISO27001".to_string())
        || proposal.policies.contains(&"HIPAA".to_string());
    GuardEvaluationResult {
        guard_id: 5,
        guard_name: "Regulatory Compliance".to_string(),
        guard_type: "compliance".to_string(),
        passed,
        message: if passed {
            "At least one major compliance framework is specified".to_string()
        } else {
            "Must specify SOC2, ISO27001, or HIPAA".to_string()
        },
        proof: format!("Compliance frameworks: {:?}", proposal.policies),
    }
}

/// Guard 6: Encryption Requirements
fn evaluate_guard_6_encryption(proposal: &OntologyProposal) -> GuardEvaluationResult {
    let passed = proposal.encryption_enabled;
    GuardEvaluationResult {
        guard_id: 6,
        guard_name: "Data Encryption".to_string(),
        guard_type: "security".to_string(),
        passed,
        message: if passed {
            "Encryption is enabled for data at rest and in transit".to_string()
        } else {
            "Encryption must be enabled".to_string()
        },
        proof: format!("Encryption enabled: {}", proposal.encryption_enabled),
    }
}

/// Guard 7: Audit Trail Logging
fn evaluate_guard_7_audit_trail(proposal: &OntologyProposal) -> GuardEvaluationResult {
    let passed = proposal.audit_logging_enabled;
    GuardEvaluationResult {
        guard_id: 7,
        guard_name: "Audit Trail Logging".to_string(),
        guard_type: "security".to_string(),
        passed,
        message: if passed {
            "Comprehensive audit trail logging is enabled".to_string()
        } else {
            "Audit logging must be enabled".to_string()
        },
        proof: format!("Audit logging enabled: {}", proposal.audit_logging_enabled),
    }
}

/// Guard 8: Access Control (RBAC)
fn evaluate_guard_8_access_control(proposal: &OntologyProposal) -> GuardEvaluationResult {
    let passed = proposal.rbac_configured;
    GuardEvaluationResult {
        guard_id: 8,
        guard_name: "Access Control (RBAC)".to_string(),
        guard_type: "security".to_string(),
        passed,
        message: if passed {
            "Role-based access control is configured".to_string()
        } else {
            "RBAC must be configured".to_string()
        },
        proof: format!("RBAC configured: {}", proposal.rbac_configured),
    }
}

/// Guard 9: Data Residency Compliance
fn evaluate_guard_9_data_residency(proposal: &OntologyProposal) -> GuardEvaluationResult {
    let passed = proposal.data_region.starts_with("us-")
        || proposal.data_region.starts_with("eu-")
        || proposal.data_region.starts_with("ap-");
    GuardEvaluationResult {
        guard_id: 9,
        guard_name: "Data Residency".to_string(),
        guard_type: "compliance".to_string(),
        passed,
        message: if passed {
            "Data region is in a compliant geographic location".to_string()
        } else {
            "Data must reside in compliant regions".to_string()
        },
        proof: format!("Data region: {}", proposal.data_region),
    }
}

/// Guard 10: Redundancy Configuration
fn evaluate_guard_10_redundancy(proposal: &OntologyProposal) -> GuardEvaluationResult {
    let passed = proposal.redundancy_level >= 2;
    GuardEvaluationResult {
        guard_id: 10,
        guard_name: "High Availability Redundancy".to_string(),
        guard_type: "infrastructure".to_string(),
        passed,
        message: if passed {
            "High availability with redundancy is configured (level {})".to_string()
        } else {
            "Redundancy must be at least level 2 (standby)".to_string()
        },
        proof: format!("Redundancy level: {}", proposal.redundancy_level),
    }
}

/// Guard 11: Disaster Recovery Planning
fn evaluate_guard_11_disaster_recovery(proposal: &OntologyProposal) -> GuardEvaluationResult {
    let passed = proposal.backup_enabled && proposal.redundancy_level >= 2;
    GuardEvaluationResult {
        guard_id: 11,
        guard_name: "Disaster Recovery".to_string(),
        guard_type: "operations".to_string(),
        passed,
        message: if passed {
            "Backup enabled and redundancy configured for disaster recovery".to_string()
        } else {
            "Backup must be enabled with redundancy for DR".to_string()
        },
        proof: format!(
            "Backup: {}, Redundancy: {}",
            proposal.backup_enabled, proposal.redundancy_level
        ),
    }
}

/// Guard 12: Incident Response Plan
fn evaluate_guard_12_incident_response(proposal: &OntologyProposal) -> GuardEvaluationResult {
    let passed = proposal.incident_plan;
    GuardEvaluationResult {
        guard_id: 12,
        guard_name: "Incident Response Plan".to_string(),
        guard_type: "operations".to_string(),
        passed,
        message: if passed {
            "Formal incident response plan is in place".to_string()
        } else {
            "Incident response plan must be documented".to_string()
        },
        proof: format!("Incident plan documented: {}", proposal.incident_plan),
    }
}

// ============================================================================
// Integration Tests
// ============================================================================

#[test]
fn test_all_12_guards_pass_on_compliant_proposal() {
    // Arrange: Create fully compliant proposal
    let proposal = OntologyProposal::fully_compliant();

    // Act: Evaluate all guards
    let results = evaluate_all_guards(&proposal);

    // Assert: All 12 guards pass
    assert_eq!(results.len(), 12, "Should have exactly 12 guards");

    for result in &results {
        assert!(
            result.passed,
            "Guard {} ({}) should pass for compliant proposal. Message: {}",
            result.guard_id, result.guard_name, result.message
        );
    }

    // Assert: Each guard has proof
    for result in &results {
        assert!(
            !result.proof.is_empty(),
            "Guard {} should provide proof",
            result.guard_id
        );
    }
}

#[test]
fn test_guard_6_encryption_fails_when_disabled() {
    // Arrange: Create proposal without encryption
    let proposal = OntologyProposal::no_encryption();

    // Act: Evaluate all guards
    let results = evaluate_all_guards(&proposal);

    // Assert: Guard 6 (Encryption) fails
    let encryption_guard = results.iter().find(|r| r.guard_id == 6).unwrap();
    assert!(
        !encryption_guard.passed,
        "Guard 6 should fail without encryption"
    );

    // Assert: Other guards still pass
    for result in &results {
        if result.guard_id != 6 {
            assert!(result.passed, "Guard {} should still pass", result.guard_id);
        }
    }
}

#[test]
fn test_guard_7_audit_fails_when_disabled() {
    // Arrange: Create proposal without audit logging
    let proposal = OntologyProposal::no_audit();

    // Act: Evaluate all guards
    let results = evaluate_all_guards(&proposal);

    // Assert: Guard 7 (Audit Trail) fails
    let audit_guard = results.iter().find(|r| r.guard_id == 7).unwrap();
    assert!(
        !audit_guard.passed,
        "Guard 7 should fail without audit logging"
    );

    // Assert: Other guards still pass (except those dependent on audit)
    for result in &results {
        if result.guard_id != 7 {
            assert!(result.passed, "Guard {} should still pass", result.guard_id);
        }
    }
}

#[test]
fn test_guard_11_disaster_recovery_fails_without_backup() {
    // Arrange: Create proposal without disaster recovery
    let proposal = OntologyProposal::no_dr();

    // Act: Evaluate all guards
    let results = evaluate_all_guards(&proposal);

    // Assert: Guard 11 (Disaster Recovery) fails
    let dr_guard = results.iter().find(|r| r.guard_id == 11).unwrap();
    assert!(
        !dr_guard.passed,
        "Guard 11 should fail without backup and redundancy"
    );
}

#[test]
fn test_guard_evaluation_determinism() {
    // Arrange
    let proposal = OntologyProposal::fully_compliant();

    // Act: Evaluate guards 3 times
    let results1 = evaluate_all_guards(&proposal);
    let results2 = evaluate_all_guards(&proposal);
    let results3 = evaluate_all_guards(&proposal);

    // Assert: All evaluations produce identical results
    for i in 0..12 {
        assert_eq!(
            results1[i].passed, results2[i].passed,
            "Guard {} should have deterministic evaluation",
            results1[i].guard_id
        );
        assert_eq!(
            results2[i].passed, results3[i].passed,
            "Guard {} should have deterministic evaluation",
            results2[i].guard_id
        );
    }
}

#[test]
fn test_guard_evaluation_score_calculation() {
    // Arrange
    let proposal = OntologyProposal::fully_compliant();
    let results = evaluate_all_guards(&proposal);

    // Act: Calculate pass rate
    let passed_count = results.iter().filter(|r| r.passed).count();
    let total_count = results.len();
    let pass_percentage = (passed_count as f64 / total_count as f64) * 100.0;

    // Assert: All guards pass = 100%
    assert_eq!(
        pass_percentage, 100.0,
        "Fully compliant proposal should have 100% guard pass rate"
    );

    // Act: Calculate for no encryption proposal
    let no_encrypt_proposal = OntologyProposal::no_encryption();
    let no_encrypt_results = evaluate_all_guards(&no_encrypt_proposal);
    let no_encrypt_passed = no_encrypt_results.iter().filter(|r| r.passed).count();
    let no_encrypt_percentage = (no_encrypt_passed as f64 / total_count as f64) * 100.0;

    // Assert: 11 out of 12 pass = ~91.67%
    assert!(
        no_encrypt_percentage > 90.0 && no_encrypt_percentage < 92.0,
        "No encryption proposal should have ~91.67% pass rate, got {}%",
        no_encrypt_percentage
    );
}

#[test]
fn test_guard_categories_map_correctly() {
    // Arrange
    let proposal = OntologyProposal::fully_compliant();
    let results = evaluate_all_guards(&proposal);

    // Create mapping of guard types
    let mut guard_types: BTreeMap<String, usize> = BTreeMap::new();
    for result in &results {
        *guard_types.entry(result.guard_type.clone()).or_insert(0) += 1;
    }

    // Assert: Guards are properly categorized
    assert!(
        guard_types.contains_key("policy"),
        "Should have policy guards"
    );
    assert!(
        guard_types.contains_key("security"),
        "Should have security guards"
    );
    assert!(
        guard_types.contains_key("infrastructure"),
        "Should have infrastructure guards"
    );
    assert!(
        guard_types.contains_key("ownership"),
        "Should have ownership guards"
    );
    assert!(
        guard_types.contains_key("compliance"),
        "Should have compliance guards"
    );
    assert!(
        guard_types.contains_key("operations"),
        "Should have operations guards"
    );

    // Assert: Correct number of guards per category
    assert_eq!(guard_types.get("policy").unwrap_or(&0), &1);
    assert_eq!(guard_types.get("security").unwrap_or(&0), &4);
    assert_eq!(guard_types.get("infrastructure").unwrap_or(&0), &2);
    assert_eq!(guard_types.get("ownership").unwrap_or(&0), &1);
    assert_eq!(guard_types.get("compliance").unwrap_or(&0), &2);
    assert_eq!(guard_types.get("operations").unwrap_or(&0), &2);
}

#[test]
fn test_guard_proof_is_concrete_and_actionable() {
    // Arrange
    let proposal = OntologyProposal::fully_compliant();
    let results = evaluate_all_guards(&proposal);

    // Act & Assert: Each guard provides specific, actionable proof
    for result in &results {
        assert!(
            !result.proof.is_empty(),
            "Guard {} must provide evidence",
            result.guard_id
        );
        assert!(
            result.proof.len() > 5,
            "Guard {} proof should be substantive",
            result.guard_id
        );

        // Verify proof is specific (not generic)
        match result.guard_id {
            1 => assert!(
                result.proof.contains("Policies:"),
                "Guard 1 proof should reference policies"
            ),
            6 => assert!(
                result.proof.contains("Encryption"),
                "Guard 6 proof should reference encryption"
            ),
            7 => assert!(
                result.proof.contains("Audit"),
                "Guard 7 proof should reference audit"
            ),
            _ => {} // Other guards also have specific proofs
        }
    }
}
