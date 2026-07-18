//! Compliance & Audit Governor integration tests
//!
//! Chicago TDD: Tests verify state machine behavior with real state objects.
//! Tests cover:
//! - Complete compliance lifecycle
//! - Audit trail immutability and cryptographic verification
//! - Data residency enforcement
//! - Breach incident response workflow
//! - Multi-framework compliance tracking
//! - Concurrent audit handling
//! - Timeout triggers and deadline enforcement

use gcp_erlang_autonomics::marketplace::{
    ComplianceGovernor, ComplianceState, ComplianceEvent, ComplianceFramework,
    AuditResult, DataResidency, BreachPhase,
};
use chrono::{Duration, Utc};

/// Test 1: Initial state is Compliant
#[tokio::test]
async fn test_initial_state_is_compliant() {
    // Arrange & Act
    let gov = ComplianceGovernor::new(
        "customer-1".to_string(),
        vec![ComplianceFramework::GDPR],
    );

    // Assert
    assert_eq!(gov.current_state(), ComplianceState::Compliant);
    assert!(gov.violations.is_empty());
    assert!(gov.breach_incidents.is_empty());
    assert!(gov.remediation_deadline.is_none());
}

/// Test 2: Compliant → AuditPending on periodic audit scheduled
#[tokio::test]
async fn test_compliant_to_audit_pending() {
    // Arrange
    let mut gov = ComplianceGovernor::new(
        "customer-1".to_string(),
        vec![ComplianceFramework::GDPR, ComplianceFramework::HIPAA],
    );

    // Act
    let (new_state, entry) = gov
        .transition(ComplianceEvent::PeriodicAuditScheduled)
        .await
        .unwrap();

    // Assert: State transitioned
    assert_eq!(new_state, ComplianceState::AuditPending);
    assert_eq!(gov.current_state(), ComplianceState::AuditPending);

    // Assert: Audit trail updated
    assert_eq!(gov.audit_trail.len(), 1);
    assert_eq!(entry.event_type, "audit_scheduled");
}

/// Test 3: AuditPending → AuditInProgress on StartAudit
#[tokio::test]
async fn test_audit_pending_to_audit_in_progress() {
    // Arrange
    let mut gov = ComplianceGovernor::new(
        "customer-1".to_string(),
        vec![ComplianceFramework::GDPR],
    );
    gov.transition(ComplianceEvent::PeriodicAuditScheduled).await.unwrap();
    assert_eq!(gov.current_state(), ComplianceState::AuditPending);

    // Act
    let (new_state, _) = gov
        .transition(ComplianceEvent::StartAudit)
        .await
        .unwrap();

    // Assert
    assert_eq!(new_state, ComplianceState::AuditInProgress);
    assert_eq!(gov.audit_trail.len(), 2);
}

/// Test 4: AuditInProgress → Compliant when audit passes
#[tokio::test]
async fn test_audit_in_progress_passes_returns_to_compliant() {
    // Arrange
    let mut gov = ComplianceGovernor::new(
        "customer-1".to_string(),
        vec![ComplianceFramework::GDPR],
    );
    gov.transition(ComplianceEvent::PeriodicAuditScheduled).await.unwrap();
    gov.transition(ComplianceEvent::StartAudit).await.unwrap();

    // Act: Audit passes
    let result = AuditResult {
        framework: ComplianceFramework::GDPR,
        passed: true,
        violations: vec![],
        score: 98,
        report: "clean_audit.pdf".to_string(),
    };

    let (new_state, _) = gov
        .transition(ComplianceEvent::AuditCompleted(result))
        .await
        .unwrap();

    // Assert
    assert_eq!(new_state, ComplianceState::Compliant);
    assert!(gov.violations.is_empty());
    assert_eq!(gov.audit_trail.len(), 3);
}

/// Test 5: AuditInProgress → NonCompliant when audit fails with violations
#[tokio::test]
async fn test_audit_in_progress_fails_moves_to_non_compliant() {
    // Arrange
    let mut gov = ComplianceGovernor::new(
        "customer-1".to_string(),
        vec![ComplianceFramework::GDPR, ComplianceFramework::HIPAA],
    );
    gov.transition(ComplianceEvent::PeriodicAuditScheduled).await.unwrap();
    gov.transition(ComplianceEvent::StartAudit).await.unwrap();

    // Act: Audit finds violations
    let result = AuditResult {
        framework: ComplianceFramework::GDPR,
        passed: false,
        violations: vec![
            "Data not encrypted at rest".to_string(),
            "No access logging configured".to_string(),
            "Insufficient retention policy".to_string(),
        ],
        score: 35,
        report: "violations_detected.pdf".to_string(),
    };

    let (new_state, _) = gov
        .transition(ComplianceEvent::AuditCompleted(result))
        .await
        .unwrap();

    // Assert: Moved to NonCompliant
    assert_eq!(new_state, ComplianceState::NonCompliant);
    assert_eq!(gov.violations.len(), 3);
    assert_eq!(gov.current_state(), ComplianceState::NonCompliant);

    // Assert: Violations have deadlines
    for violation in &gov.violations {
        assert!(violation.remediation_deadline > Utc::now());
        assert!(violation.framework == ComplianceFramework::GDPR);
    }
}

/// Test 6: NonCompliant → RemediationInProgress when remediation approved
#[tokio::test]
async fn test_non_compliant_to_remediation_in_progress() {
    // Arrange
    let mut gov = ComplianceGovernor::new(
        "customer-1".to_string(),
        vec![ComplianceFramework::HIPAA],
    );
    gov.transition(ComplianceEvent::PeriodicAuditScheduled).await.unwrap();
    gov.transition(ComplianceEvent::StartAudit).await.unwrap();

    let result = AuditResult {
        framework: ComplianceFramework::HIPAA,
        passed: false,
        violations: vec!["Missing TLS 1.3 encryption".to_string()],
        score: 45,
        report: "hipaa_violations.pdf".to_string(),
    };
    gov.transition(ComplianceEvent::AuditCompleted(result)).await.unwrap();
    assert_eq!(gov.current_state(), ComplianceState::NonCompliant);

    // Act
    let (new_state, _) = gov
        .transition(ComplianceEvent::RemediationPlanApproved)
        .await
        .unwrap();

    // Assert
    assert_eq!(new_state, ComplianceState::RemediationInProgress);
    assert!(gov.remediation_deadline.is_some());

    // Assert: Deadline is 30 days out
    let deadline = gov.remediation_deadline.unwrap();
    let duration = deadline - Utc::now();
    assert!(duration.num_days() >= 29 && duration.num_days() <= 30);
}

/// Test 7: RemediationInProgress → Compliant when remediation completes
#[tokio::test]
async fn test_remediation_in_progress_completes_returns_to_compliant() {
    // Arrange: Get to RemediationInProgress
    let mut gov = ComplianceGovernor::new(
        "customer-1".to_string(),
        vec![ComplianceFramework::SOC2],
    );
    gov.transition(ComplianceEvent::PeriodicAuditScheduled).await.unwrap();
    gov.transition(ComplianceEvent::StartAudit).await.unwrap();

    let result = AuditResult {
        framework: ComplianceFramework::SOC2,
        passed: false,
        violations: vec!["Security controls not documented".to_string()],
        score: 50,
        report: "soc2_findings.pdf".to_string(),
    };
    gov.transition(ComplianceEvent::AuditCompleted(result)).await.unwrap();
    gov.transition(ComplianceEvent::RemediationPlanApproved).await.unwrap();

    assert_eq!(gov.current_state(), ComplianceState::RemediationInProgress);
    assert_eq!(gov.violations.len(), 1);

    // Act: Remediation completes
    let (new_state, _) = gov
        .transition(ComplianceEvent::RemediationComplete)
        .await
        .unwrap();

    // Assert: Back to Compliant
    assert_eq!(new_state, ComplianceState::Compliant);
    assert!(gov.violations.is_empty());
    assert!(gov.remediation_deadline.is_none());
}

/// Test 8: AuditPending → Compliant when audit cancelled
#[tokio::test]
async fn test_audit_cancelled_returns_to_compliant() {
    // Arrange
    let mut gov = ComplianceGovernor::new(
        "customer-1".to_string(),
        vec![ComplianceFramework::PCIDSS],
    );
    gov.transition(ComplianceEvent::PeriodicAuditScheduled).await.unwrap();
    assert_eq!(gov.current_state(), ComplianceState::AuditPending);

    // Act
    let (new_state, _) = gov
        .transition(ComplianceEvent::AuditCancelled)
        .await
        .unwrap();

    // Assert
    assert_eq!(new_state, ComplianceState::Compliant);
}

/// Test 9: Compliant → AuditInProgress on urgent audit (bypasses pending)
#[tokio::test]
async fn test_urgent_audit_bypasses_pending() {
    // Arrange
    let mut gov = ComplianceGovernor::new(
        "customer-1".to_string(),
        vec![ComplianceFramework::FedRAMP],
    );

    // Act: Urgent audit from Compliant goes directly to InProgress
    let (new_state, _) = gov
        .transition(ComplianceEvent::UrgentAudit)
        .await
        .unwrap();

    // Assert
    assert_eq!(new_state, ComplianceState::AuditInProgress);
    assert_eq!(gov.audit_trail.len(), 1);
}

/// Test 10: Breach detection creates incident record
#[tokio::test]
async fn test_breach_detection_creates_incident() {
    // Arrange
    let mut gov = ComplianceGovernor::new(
        "customer-1".to_string(),
        vec![ComplianceFramework::GDPR],
    );

    // Act
    let incident_id = gov
        .detect_breach(
            "Unauthorized API access detected",
            vec!["api-server-1".to_string(), "api-server-2".to_string()],
        )
        .await
        .unwrap();

    // Assert
    assert!(gov.breach_incidents.contains_key(&incident_id));
    let incident = &gov.breach_incidents[&incident_id];
    assert_eq!(incident.phase, BreachPhase::Detected);
    assert_eq!(incident.affected_systems.len(), 2);
    assert_eq!(incident.timeline.len(), 1);
}

/// Test 11: Breach phase progression (incident response workflow)
#[tokio::test]
async fn test_breach_phase_progression_workflow() {
    // Arrange
    let mut gov = ComplianceGovernor::new(
        "customer-1".to_string(),
        vec![ComplianceFramework::GDPR, ComplianceFramework::HIPAA],
    );
    let incident_id = gov
        .detect_breach("SQL injection detected", vec!["db-primary".to_string()])
        .await
        .unwrap();

    // Act & Assert: Progress through incident response phases
    // Phase 1: Detected → Contained
    gov.update_breach_phase(&incident_id, BreachPhase::Contained).await.unwrap();
    assert_eq!(gov.breach_incidents[&incident_id].phase, BreachPhase::Contained);
    assert_eq!(gov.breach_incidents[&incident_id].timeline.len(), 2);

    // Phase 2: Contained → NotifiedAuthorities (GDPR 72-hour rule)
    gov.update_breach_phase(&incident_id, BreachPhase::NotifiedAuthorities).await.unwrap();
    assert_eq!(gov.breach_incidents[&incident_id].phase, BreachPhase::NotifiedAuthorities);

    // Phase 3: NotifiedAuthorities → NotifiedCustomers
    gov.update_breach_phase(&incident_id, BreachPhase::NotifiedCustomers).await.unwrap();
    assert_eq!(gov.breach_incidents[&incident_id].phase, BreachPhase::NotifiedCustomers);

    // Phase 4: NotifiedCustomers → Investigating
    gov.update_breach_phase(&incident_id, BreachPhase::Investigating).await.unwrap();
    assert_eq!(gov.breach_incidents[&incident_id].phase, BreachPhase::Investigating);

    // Phase 5: Investigating → Remediated
    gov.update_breach_phase(&incident_id, BreachPhase::Remediated).await.unwrap();
    assert_eq!(gov.breach_incidents[&incident_id].phase, BreachPhase::Remediated);

    // Phase 6: Remediated → VerifiedFix
    gov.update_breach_phase(&incident_id, BreachPhase::VerifiedFix).await.unwrap();
    assert_eq!(gov.breach_incidents[&incident_id].phase, BreachPhase::VerifiedFix);

    // Assert: Full timeline recorded
    assert_eq!(gov.breach_incidents[&incident_id].timeline.len(), 7);
}

/// Test 12: Data residency enforcement (region locking for EU)
#[tokio::test]
async fn test_data_residency_eu_enforcement() {
    // Arrange
    let mut gov = ComplianceGovernor::new(
        "customer-eu".to_string(),
        vec![ComplianceFramework::GDPR],
    );
    let residency = DataResidency {
        customer_id: "customer-eu".to_string(),
        allowed_regions: vec!["europe-west1".to_string(), "europe-west4".to_string()],
        classification: "Sensitive".to_string(),
    };
    gov.set_data_residency(residency);

    // Act & Assert: Allowed regions
    assert!(gov.verify_residency("europe-west1").is_ok());
    assert!(gov.verify_residency("europe-west4").is_ok());

    // Act & Assert: Disallowed regions
    assert!(gov.verify_residency("us-central1").is_err());
    assert!(gov.verify_residency("asia-east1").is_err());
    assert!(gov.verify_residency("us-east1").is_err());
}

/// Test 13: Audit trail immutability and cryptographic verification
#[tokio::test]
async fn test_audit_trail_immutability() {
    // Arrange
    let mut gov = ComplianceGovernor::new(
        "customer-1".to_string(),
        vec![ComplianceFramework::GDPR],
    );

    // Act: Multiple transitions
    gov.transition(ComplianceEvent::PeriodicAuditScheduled).await.unwrap();
    gov.transition(ComplianceEvent::StartAudit).await.unwrap();
    gov.transition(ComplianceEvent::AuditMilestoneComplete).await.unwrap();

    // Assert: Trail is immutable (hash chain verified)
    assert_eq!(gov.audit_trail.len(), 3);
    assert!(gov.verify_audit_trail().is_ok());

    // Try to corrupt: Modify entry hash
    if let Some(entry) = gov.audit_trail.front_mut() {
        entry.hash = "tampered_hash".to_string();
    }

    // Assert: Corruption detected
    assert!(gov.verify_audit_trail().is_err());
}

/// Test 14: Audit trail query by time range
#[tokio::test]
async fn test_audit_trail_query_by_time_range() {
    // Arrange
    let mut gov = ComplianceGovernor::new(
        "customer-1".to_string(),
        vec![ComplianceFramework::GDPR],
    );

    let start_time = Utc::now();
    gov.transition(ComplianceEvent::PeriodicAuditScheduled).await.unwrap();

    // Small delay to create time gap
    tokio::time::sleep(std::time::Duration::from_millis(10)).await;

    gov.transition(ComplianceEvent::StartAudit).await.unwrap();

    let end_time = Utc::now();

    // Act: Query entries in time range
    let entries = gov.get_audit_entries(start_time - Duration::minutes(1), end_time + Duration::minutes(1));

    // Assert
    assert_eq!(entries.len(), 2);
}

/// Test 15: Multi-framework compliance tracking
#[tokio::test]
async fn test_multi_framework_compliance() {
    // Arrange: Customer with 5 frameworks
    let mut gov = ComplianceGovernor::new(
        "customer-enterprise".to_string(),
        vec![
            ComplianceFramework::GDPR,
            ComplianceFramework::HIPAA,
            ComplianceFramework::SOC2,
            ComplianceFramework::PCIDSS,
            ComplianceFramework::FedRAMP,
        ],
    );

    // Assert: All frameworks subscribed
    assert_eq!(gov.frameworks.len(), 5);

    // Verify framework controls
    for framework in &gov.frameworks {
        let controls = framework.required_controls();
        assert!(!controls.is_empty());
        println!("{:?}: {} controls", framework.as_str(), controls.len());
    }
}

/// Test 16: Concurrent audit handling (multi-tenant isolation)
#[tokio::test]
async fn test_multi_tenant_isolation() {
    // Arrange: Two independent governors
    let mut gov1 = ComplianceGovernor::new(
        "customer-1".to_string(),
        vec![ComplianceFramework::GDPR],
    );
    let mut gov2 = ComplianceGovernor::new(
        "customer-2".to_string(),
        vec![ComplianceFramework::HIPAA],
    );

    // Act: Different transitions
    gov1.transition(ComplianceEvent::PeriodicAuditScheduled).await.unwrap();
    gov2.transition(ComplianceEvent::UrgentAudit).await.unwrap();

    // Assert: Independent states
    assert_eq!(gov1.current_state(), ComplianceState::AuditPending);
    assert_eq!(gov2.current_state(), ComplianceState::AuditInProgress);
    assert_eq!(gov1.audit_trail.len(), 1);
    assert_eq!(gov2.audit_trail.len(), 1);
    assert_ne!(gov1.customer_id, gov2.customer_id);
}

/// Test 17: Remediation failed moves back to NonCompliant
#[tokio::test]
async fn test_remediation_failed_returns_to_non_compliant() {
    // Arrange: Get to RemediationInProgress
    let mut gov = ComplianceGovernor::new(
        "customer-1".to_string(),
        vec![ComplianceFramework::GDPR],
    );
    gov.transition(ComplianceEvent::PeriodicAuditScheduled).await.unwrap();
    gov.transition(ComplianceEvent::StartAudit).await.unwrap();

    let result = AuditResult {
        framework: ComplianceFramework::GDPR,
        passed: false,
        violations: vec!["Violation".to_string()],
        score: 40,
        report: "report.pdf".to_string(),
    };
    gov.transition(ComplianceEvent::AuditCompleted(result)).await.unwrap();
    gov.transition(ComplianceEvent::RemediationPlanApproved).await.unwrap();

    // Act: Remediation fails
    let (new_state, _) = gov
        .transition(ComplianceEvent::RemediationFailed)
        .await
        .unwrap();

    // Assert: Back to NonCompliant
    assert_eq!(new_state, ComplianceState::NonCompliant);
    assert!(!gov.violations.is_empty());
}

/// Test 18: Customer appeals in NonCompliant state
#[tokio::test]
async fn test_customer_appeals_in_non_compliant() {
    // Arrange: Get to NonCompliant
    let mut gov = ComplianceGovernor::new(
        "customer-1".to_string(),
        vec![ComplianceFramework::GDPR],
    );
    gov.transition(ComplianceEvent::PeriodicAuditScheduled).await.unwrap();
    gov.transition(ComplianceEvent::StartAudit).await.unwrap();

    let result = AuditResult {
        framework: ComplianceFramework::GDPR,
        passed: false,
        violations: vec!["Violation".to_string()],
        score: 40,
        report: "report.pdf".to_string(),
    };
    gov.transition(ComplianceEvent::AuditCompleted(result)).await.unwrap();
    assert_eq!(gov.current_state(), ComplianceState::NonCompliant);

    // Act: Customer appeals
    let (new_state, entry) = gov
        .transition(ComplianceEvent::CustomerAppeals)
        .await
        .unwrap();

    // Assert: Stays in NonCompliant but logs appeal
    assert_eq!(new_state, ComplianceState::NonCompliant);
    assert_eq!(entry.event_type, "customer_appealed");
}

/// Test 19: Escalate to enforcement action
#[tokio::test]
async fn test_escalate_to_enforcement() {
    // Arrange: Get to NonCompliant
    let mut gov = ComplianceGovernor::new(
        "customer-1".to_string(),
        vec![ComplianceFramework::GDPR],
    );
    gov.transition(ComplianceEvent::PeriodicAuditScheduled).await.unwrap();
    gov.transition(ComplianceEvent::StartAudit).await.unwrap();

    let result = AuditResult {
        framework: ComplianceFramework::GDPR,
        passed: false,
        violations: vec!["Violation".to_string()],
        score: 40,
        report: "report.pdf".to_string(),
    };
    gov.transition(ComplianceEvent::AuditCompleted(result)).await.unwrap();

    // Act: Escalate to enforcement
    let (new_state, entry) = gov
        .transition(ComplianceEvent::EscalateToEnforcement)
        .await
        .unwrap();

    // Assert
    assert_eq!(new_state, ComplianceState::NonCompliant);
    assert_eq!(entry.event_type, "escalated_enforcement");
}

/// Test 20: Invalid state transition returns error
#[tokio::test]
async fn test_invalid_state_transition() {
    // Arrange: In RemediationInProgress
    let mut gov = ComplianceGovernor::new(
        "customer-1".to_string(),
        vec![ComplianceFramework::GDPR],
    );
    gov.transition(ComplianceEvent::PeriodicAuditScheduled).await.unwrap();
    gov.transition(ComplianceEvent::StartAudit).await.unwrap();

    let result = AuditResult {
        framework: ComplianceFramework::GDPR,
        passed: false,
        violations: vec!["Violation".to_string()],
        score: 40,
        report: "report.pdf".to_string(),
    };
    gov.transition(ComplianceEvent::AuditCompleted(result)).await.unwrap();
    gov.transition(ComplianceEvent::RemediationPlanApproved).await.unwrap();

    // Act: Try invalid transition from RemediationInProgress
    let result = gov
        .transition(ComplianceEvent::StartAudit)
        .await;

    // Assert: Error
    assert!(result.is_err());
}

/// Test 21: Audit milestone tracking during in-progress audit
#[tokio::test]
async fn test_audit_milestone_tracking() {
    // Arrange
    let mut gov = ComplianceGovernor::new(
        "customer-1".to_string(),
        vec![ComplianceFramework::GDPR],
    );
    gov.transition(ComplianceEvent::PeriodicAuditScheduled).await.unwrap();
    gov.transition(ComplianceEvent::StartAudit).await.unwrap();

    // Act: Record milestone
    let (state, entry) = gov
        .transition(ComplianceEvent::AuditMilestoneComplete)
        .await
        .unwrap();

    // Assert: Still in AuditInProgress but milestone recorded
    assert_eq!(state, ComplianceState::AuditInProgress);
    assert_eq!(entry.event_type, "audit_milestone");
    assert_eq!(gov.audit_trail.len(), 3);
}

/// Test 22: Audit failure due to error
#[tokio::test]
async fn test_audit_failed_with_error() {
    // Arrange
    let mut gov = ComplianceGovernor::new(
        "customer-1".to_string(),
        vec![ComplianceFramework::GDPR],
    );
    gov.transition(ComplianceEvent::PeriodicAuditScheduled).await.unwrap();
    gov.transition(ComplianceEvent::StartAudit).await.unwrap();

    // Act: Audit fails with error
    let (new_state, _) = gov
        .transition(ComplianceEvent::AuditFailed("Timeout after 14 days".to_string()))
        .await
        .unwrap();

    // Assert: Moves to NonCompliant
    assert_eq!(new_state, ComplianceState::NonCompliant);
    assert_eq!(gov.audit_trail.len(), 3);
}

/// Test 23: Check invariant on empty customer_id
#[tokio::test]
async fn test_invariant_empty_customer_id() {
    // Arrange
    let mut gov = ComplianceGovernor::new(
        "customer-1".to_string(),
        vec![ComplianceFramework::GDPR],
    );
    gov.customer_id = String::new();

    // Act: Try transition with empty customer_id
    let result = gov
        .transition(ComplianceEvent::PeriodicAuditScheduled)
        .await;

    // Assert: Error due to invariant violation
    assert!(result.is_err());
}

/// Test 24: Audit trail maintains hash chain
#[tokio::test]
async fn test_audit_trail_hash_chain() {
    // Arrange
    let mut gov = ComplianceGovernor::new(
        "customer-1".to_string(),
        vec![ComplianceFramework::GDPR],
    );

    // Act: Multiple transitions to build hash chain
    gov.transition(ComplianceEvent::PeriodicAuditScheduled).await.unwrap();
    gov.transition(ComplianceEvent::StartAudit).await.unwrap();
    gov.transition(ComplianceEvent::AuditMilestoneComplete).await.unwrap();

    // Assert: Hash chain is valid
    assert_eq!(gov.audit_trail.len(), 3);
    if let Some(first) = gov.audit_trail.front() {
        assert_eq!(first.prev_hash, "genesis");
    }

    // Verify chain continuity
    let entries: Vec<_> = gov.audit_trail.iter().collect();
    for i in 1..entries.len() {
        assert_eq!(entries[i].prev_hash, entries[i - 1].hash);
    }
}

/// Test 25: Complete audit to remediation to compliant lifecycle
#[tokio::test]
async fn test_complete_audit_lifecycle() {
    // Arrange
    let mut gov = ComplianceGovernor::new(
        "customer-complete".to_string(),
        vec![ComplianceFramework::GDPR, ComplianceFramework::HIPAA],
    );

    // Phase 1: Schedule audit
    gov.transition(ComplianceEvent::PeriodicAuditScheduled).await.unwrap();
    assert_eq!(gov.current_state(), ComplianceState::AuditPending);

    // Phase 2: Start audit
    gov.transition(ComplianceEvent::StartAudit).await.unwrap();
    assert_eq!(gov.current_state(), ComplianceState::AuditInProgress);

    // Phase 3: Record milestone
    gov.transition(ComplianceEvent::AuditMilestoneComplete).await.unwrap();
    assert_eq!(gov.current_state(), ComplianceState::AuditInProgress);

    // Phase 4: Audit finds violations
    let result = AuditResult {
        framework: ComplianceFramework::GDPR,
        passed: false,
        violations: vec![
            "Data not encrypted".to_string(),
            "Missing access controls".to_string(),
        ],
        score: 40,
        report: "audit_report.pdf".to_string(),
    };
    gov.transition(ComplianceEvent::AuditCompleted(result)).await.unwrap();
    assert_eq!(gov.current_state(), ComplianceState::NonCompliant);
    assert_eq!(gov.violations.len(), 2);

    // Phase 5: Remediation approved
    gov.transition(ComplianceEvent::RemediationPlanApproved).await.unwrap();
    assert_eq!(gov.current_state(), ComplianceState::RemediationInProgress);
    assert!(gov.remediation_deadline.is_some());

    // Phase 6: Remediation completes
    gov.transition(ComplianceEvent::RemediationComplete).await.unwrap();
    assert_eq!(gov.current_state(), ComplianceState::Compliant);
    assert!(gov.violations.is_empty());
    assert!(gov.remediation_deadline.is_none());

    // Assert: Full audit trail recorded
    assert!(gov.audit_trail.len() >= 6);
    assert!(gov.verify_audit_trail().is_ok());
}
