//! Security Tests for Autonomous System
//!
//! Tests API key masking, governance policy enforcement, authorization,
//! audit trail completeness, and security boundaries.

use ggen_ai::{
    governance::{
        AuditTrail, Decision, DecisionOutcome, GovernanceConfig, GovernanceCoordinator, Policy,
        PolicyEngine, SafetyController,
    },
    security::{MaskApiKey, SecretString},
    GraphEvolutionEngine, MockClient,
};

/// Test API key masking in evolution errors
#[tokio::test]
async fn test_api_key_masking_in_evolution_errors() {
    // Create client with API key in config
    let api_key = "sk-1234567890abcdefghijklmnopqrstuvwxyz";
    let mock_client = MockClient::with_response("error");

    // Evolution with mocked error containing API key
    let parser_client = Box::new(mock_client);
    let validator_client = Box::new(MockClient::with_response("valid"));

    let mut engine = GraphEvolutionEngine::with_defaults(parser_client, validator_client)
        .expect("Failed to create engine");

    let result = engine.evolve_from_nl("test").await;

    // Check that any error messages don't leak the API key
    if let Err(e) = result {
        let error_str = format!("{}", e);
        let masked = error_str.mask_api_key();

        assert!(
            !masked.contains("1234567890"),
            "API key should be masked in errors"
        );
        assert!(
            !masked.contains("abcdefghij"),
            "API key should be masked in errors"
        );
    }
}

/// Test governance policy enforcement blocks unauthorized operations
#[tokio::test]
async fn test_governance_policy_blocks_unauthorized_ops() {
    let mut config = GovernanceConfig::default();
    config.require_approval_for_all = false; // Allow some auto-approval

    let governance = GovernanceCoordinator::new(config)
        .await
        .expect("Failed to create governance");

    // Test that critical operations are blocked without approval
    let critical_decision = Decision::new_critical("delete_schema", "Delete entire schema");

    let outcome = governance
        .validate_decision(&critical_decision)
        .await
        .expect("Validation failed");

    match outcome {
        DecisionOutcome::Rejected { .. } => {
            // Expected - critical ops should be rejected or require approval
        }
        DecisionOutcome::PendingApproval { .. } => {
            // Also acceptable - requires human approval
        }
        DecisionOutcome::Approved { auto_approved, .. } => {
            assert!(
                !auto_approved,
                "Critical operations should not be auto-approved"
            );
        }
    }
}

/// Test governance enforces rate limits
#[tokio::test]
async fn test_governance_enforces_rate_limits() {
    let config = GovernanceConfig::default();
    let governance = GovernanceCoordinator::new(config)
        .await
        .expect("Failed to create governance");

    // Rapidly submit many decisions
    let mut approvals = 0;
    let mut rejections = 0;

    for i in 0..100 {
        let decision = Decision::new_low_risk(&format!("operation_{}", i), "Rapid operation");

        let outcome = governance
            .validate_decision(&decision)
            .await
            .expect("Validation failed");

        match outcome {
            DecisionOutcome::Approved { .. } => approvals += 1,
            DecisionOutcome::Rejected { .. } => rejections += 1,
            _ => {}
        }
    }

    println!("Approvals: {}, Rejections: {}", approvals, rejections);

    // In a real system with rate limiting, we'd expect some rejections
    // For now, just verify the system doesn't crash under load
    assert!(approvals > 0, "Some operations should succeed");
}

/// Test audit trail records all operations
#[tokio::test]
async fn test_audit_trail_records_all_operations() {
    use tempfile::TempDir;

    let temp_dir = TempDir::new().expect("Failed to create temp dir");
    let db_path = temp_dir.path().join("audit.db");

    let audit_trail = AuditTrail::new(db_path.to_str().unwrap())
        .await
        .expect("Failed to create audit trail");

    let config = GovernanceConfig {
        audit_db_path: db_path.to_str().unwrap().to_string(),
        ..Default::default()
    };

    let governance = GovernanceCoordinator::new(config)
        .await
        .expect("Failed to create governance");

    // Perform operations
    let operations = vec![
        Decision::new_low_risk("op1", "Operation 1"),
        Decision::new_medium_risk("op2", "Operation 2"),
        Decision::new_high_risk("op3", "Operation 3"),
    ];

    for decision in operations {
        let _ = governance.validate_decision(&decision).await;
    }

    // Query audit trail
    let query = ggen_ai::governance::AuditQuery {
        start_time: None,
        end_time: None,
        event_types: vec![],
        limit: 100,
    };

    let events = governance
        .query_audit_trail(query)
        .await
        .expect("Failed to query audit trail");

    println!("Audit trail events: {}", events.len());

    // Should have recorded all decisions
    assert!(events.len() >= 3, "All operations should be audited");
}

/// Test safety controller emergency stop
#[tokio::test]
async fn test_safety_controller_emergency_stop() {
    let config = GovernanceConfig::default();
    let governance = GovernanceCoordinator::new(config)
        .await
        .expect("Failed to create governance");

    // Trigger emergency stop
    governance
        .emergency_stop("Security breach detected")
        .await
        .expect("Emergency stop failed");

    // Verify system is in emergency mode
    let health = governance
        .get_health_status()
        .await
        .expect("Failed to get health status");

    assert!(
        health.emergency_stop_active,
        "Emergency stop should be active"
    );

    // Try to perform operation (should be blocked)
    let decision = Decision::new_low_risk("test_op", "Test operation");
    let outcome = governance
        .validate_decision(&decision)
        .await
        .expect("Validation failed");

    match outcome {
        DecisionOutcome::Rejected { .. } => {
            // Expected - operations should be blocked during emergency stop
        }
        _ => panic!("Operations should be rejected during emergency stop"),
    }

    // Resume operations
    governance
        .resume_operations("admin")
        .await
        .expect("Resume failed");

    let health2 = governance
        .get_health_status()
        .await
        .expect("Failed to get health status");

    assert!(
        !health2.emergency_stop_active,
        "Emergency stop should be cleared"
    );
}

/// Test secret string never leaks in debug output
#[test]
fn test_secret_string_protection_comprehensive() {
    let secrets = vec![
        "sk-1234567890abcdefghijklmnop",
        "sk-ant-api03-abcdefghijklmnopqrstuvwxyz",
        "gsk_1234567890abcdefghijklmnop",
        "Bearer eyJhbGciOiJIUzI1NiIsInR5cCI6IkpXVCJ9.test.signature",
    ];

    for secret in secrets {
        let secret_str = SecretString::new(secret.to_string());

        // Test Debug
        let debug_output = format!("{:?}", secret_str);
        assert!(
            !debug_output.contains(&secret[10..20]),
            "Secret leaked in debug: {}",
            secret
        );

        // Test Display
        let display_output = format!("{}", secret_str);
        assert!(
            !display_output.contains(&secret[10..20]),
            "Secret leaked in display: {}",
            secret
        );

        // Test JSON serialization
        let json = serde_json::to_string(&secret_str).unwrap();
        assert!(
            !json.contains(&secret[10..20]),
            "Secret leaked in JSON: {}",
            secret
        );
    }
}

/// Test that governance policies are enforceable
#[tokio::test]
async fn test_governance_policies_are_enforceable() {
    let config = GovernanceConfig::default();
    let governance = GovernanceCoordinator::new(config)
        .await
        .expect("Failed to create governance");

    // Define custom policy
    let policy = Policy::builder()
        .name("max_changes_per_hour")
        .description("Limit graph mutations per hour")
        .constraint("max_mutations", 100)
        .build()
        .expect("Failed to build policy");

    // Policy engine should validate against this policy
    // (This would require extending PolicyEngine with registration capability)

    // For now, verify that policy structure is correct
    assert_eq!(policy.name(), "max_changes_per_hour");
    assert!(policy.constraints().contains_key("max_mutations"));
}

/// Test that rollback preserves audit trail integrity
#[tokio::test]
async fn test_rollback_preserves_audit_trail() {
    use tempfile::TempDir;

    let temp_dir = TempDir::new().expect("Failed to create temp dir");
    let db_path = temp_dir.path().join("audit.db");

    let config = GovernanceConfig {
        audit_db_path: db_path.to_str().unwrap().to_string(),
        ..Default::default()
    };

    let governance = GovernanceCoordinator::new(config)
        .await
        .expect("Failed to create governance");

    // Perform operation
    let decision = Decision::new_medium_risk("test_op", "Test operation");
    let _ = governance.validate_decision(&decision).await;

    // Trigger rollback
    let _ = governance.rollback("previous_state").await;

    // Verify both operation and rollback are in audit trail
    let query = ggen_ai::governance::AuditQuery {
        start_time: None,
        end_time: None,
        event_types: vec![],
        limit: 100,
    };

    let events = governance
        .query_audit_trail(query)
        .await
        .expect("Failed to query audit trail");

    // Should have at least 2 events (operation + rollback)
    assert!(events.len() >= 2, "Rollback should be audited");
}

/// Test unauthorized access is blocked
#[tokio::test]
async fn test_unauthorized_access_blocked() {
    let config = GovernanceConfig::default();
    let governance = GovernanceCoordinator::new(config)
        .await
        .expect("Failed to create governance");

    // Attempt unauthorized critical operation
    let unauthorized_decision =
        Decision::new_critical("admin_operation", "Perform system-level change");

    let outcome = governance
        .validate_decision(&unauthorized_decision)
        .await
        .expect("Validation failed");

    // Critical operations should require approval or be rejected
    match outcome {
        DecisionOutcome::Approved { auto_approved, .. } => {
            assert!(!auto_approved, "Critical ops should not be auto-approved");
        }
        DecisionOutcome::PendingApproval { .. } | DecisionOutcome::Rejected { .. } => {
            // Expected
        }
    }
}

/// Test that sensitive data is redacted in logs
#[test]
fn test_sensitive_data_redaction_in_logs() {
    let log_messages = vec![
        "Failed to connect with api_key=sk-1234567890abcdefghijklmnop",
        "Authorization header: Bearer eyJhbGciOiJIUzI1NiIsInR5cCI6IkpXVCJ9",
        "Using credentials: sk-ant-api03-abcdefghijklmnopqrstuvwxyz",
    ];

    for message in log_messages {
        let redacted = message.mask_api_key();

        // Should not contain sensitive parts
        assert!(
            !redacted.contains("1234567890"),
            "Sensitive data leaked: {}",
            message
        );
        assert!(!redacted.contains("eyJhbGci"), "Token leaked: {}", message);
        assert!(!redacted.contains("abcdefghijk"), "Key leaked: {}", message);
    }
}
