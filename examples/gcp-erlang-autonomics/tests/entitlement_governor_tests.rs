//! Integration tests for Marketplace Entitlement Governor
//!
//! Chicago TDD: Tests verify FSM behavior with real state objects
//! Uses state verification, not implementation details

use gcp_erlang_autonomics::marketplace::{
    EntitlementGovernor, EntitlementState, EntitlementEvent, EntitlementConfig, MarketplaceError,
};

#[test]
fn test_create_new_entitlement_initial_state() {
    // Arrange
    let gov = EntitlementGovernor::new_default();

    // Act
    let result = gov.activate_entitlement("starter-sku", "cust-001");

    // Assert: Created in PendingApproval state
    assert!(result.is_ok());
    let ent = result.unwrap();
    assert_eq!(ent.state, EntitlementState::PendingApproval);
    assert_eq!(ent.customer_id, "cust-001");
    assert_eq!(ent.sku_id, "starter-sku");
}

#[test]
fn test_fsm_pending_to_active_approval() {
    // Arrange
    let gov = EntitlementGovernor::new_default();
    let ent = gov.activate_entitlement("pro-sku", "cust-002").unwrap();

    // Act
    let result = gov.handle_event(
        &ent.id,
        EntitlementEvent::ApprovalGranted {
            actor: "gcp-admin".to_string(),
        },
    );

    // Assert: Transitioned to Active
    assert!(result.is_ok());
    let (state, action) = result.unwrap();
    assert_eq!(state, EntitlementState::Active);
    assert!(action.is_some());

    // Verify receipt recorded
    let receipts = gov.get_receipt_chain(&ent.id).unwrap();
    assert_eq!(receipts.len(), 1);
    assert_eq!(receipts[0].from_state, "pending_approval");
    assert_eq!(receipts[0].to_state, "active");
}

#[test]
fn test_fsm_pending_to_cancelled_rejection() {
    let gov = EntitlementGovernor::new_default();
    let ent = gov.activate_entitlement("pro-sku", "cust-003").unwrap();

    let result = gov.handle_event(
        &ent.id,
        EntitlementEvent::ApprovalRejected {
            reason: "Account flagged".to_string(),
        },
    );

    assert!(result.is_ok());
    let (state, _) = result.unwrap();
    assert_eq!(state, EntitlementState::Cancelled);
}

#[test]
fn test_fsm_active_to_suspended_payment_failure() {
    let gov = EntitlementGovernor::new_default();
    let ent = gov.activate_entitlement("pro-sku", "cust-005").unwrap();

    // Approve first
    gov.handle_event(
        &ent.id,
        EntitlementEvent::ApprovalGranted {
            actor: "admin".to_string(),
        },
    )
    .unwrap();

    // Payment fails
    let result = gov.handle_event(
        &ent.id,
        EntitlementEvent::PaymentFailed {
            reason: "Card declined".to_string(),
        },
    );

    assert!(result.is_ok());
    let (state, _) = result.unwrap();
    assert_eq!(state, EntitlementState::Suspended);
}

#[test]
fn test_fsm_suspended_to_active_payment_received() {
    let gov = EntitlementGovernor::new_default();
    let ent = gov.activate_entitlement("pro-sku", "cust-006").unwrap();

    // Setup: Activate then suspend
    gov.handle_event(
        &ent.id,
        EntitlementEvent::ApprovalGranted {
            actor: "admin".to_string(),
        },
    )
    .unwrap();
    gov.handle_event(
        &ent.id,
        EntitlementEvent::PaymentFailed {
            reason: "Card declined".to_string(),
        },
    )
    .unwrap();

    // Act: Payment received
    let result = gov.handle_event(&ent.id, EntitlementEvent::PaymentReceived);

    // Assert
    assert!(result.is_ok());
    let (state, _) = result.unwrap();
    assert_eq!(state, EntitlementState::Active);
}

#[test]
fn test_full_refund_lifecycle() {
    let gov = EntitlementGovernor::new_default();
    let ent = gov.activate_entitlement("pro-sku", "cust-008").unwrap();

    // Reject approval to move to Cancelled
    gov.handle_event(
        &ent.id,
        EntitlementEvent::ApprovalRejected {
            reason: "Test".to_string(),
        },
    )
    .unwrap();

    // Approve refund
    gov.handle_event(&ent.id, EntitlementEvent::RefundApproved { amount: 99.99 }).unwrap();

    // Complete refund
    let result = gov.handle_event(&ent.id, EntitlementEvent::RefundCompleted);

    assert!(result.is_ok());
    let (state, _) = result.unwrap();
    assert_eq!(state, EntitlementState::Archived);
}

#[test]
fn test_reinstatement_flow() {
    let gov = EntitlementGovernor::new_default();
    let ent = gov.activate_entitlement("pro-sku", "cust-010").unwrap();

    // Cancel
    gov.handle_event(
        &ent.id,
        EntitlementEvent::ApprovalRejected {
            reason: "Test".to_string(),
        },
    )
    .unwrap();

    // Request reinstatement
    let result = gov.handle_event(&ent.id, EntitlementEvent::ReinstatementRequested);
    assert!(result.is_ok());
    let (state, _) = result.unwrap();
    assert_eq!(state, EntitlementState::ReinstallPending);

    // Approve reinstatement
    let result = gov.handle_event(
        &ent.id,
        EntitlementEvent::ReinstatementApproved {
            actor: "admin".to_string(),
        },
    );
    assert!(result.is_ok());
    let (state, _) = result.unwrap();
    assert_eq!(state, EntitlementState::Active);
}

#[test]
fn test_multi_tenant_isolation() {
    let gov = EntitlementGovernor::new_default();

    let ent1 = gov.activate_entitlement("sku-1", "cust-a").unwrap();
    let ent2 = gov.activate_entitlement("sku-1", "cust-b").unwrap();

    // Approve ent1
    gov.handle_event(
        &ent1.id,
        EntitlementEvent::ApprovalGranted {
            actor: "admin".to_string(),
        },
    )
    .unwrap();

    // Reject ent2
    gov.handle_event(
        &ent2.id,
        EntitlementEvent::ApprovalRejected {
            reason: "Test".to_string(),
        },
    )
    .unwrap();

    // Verify separate states (isolation)
    let (state1, _) = gov.check_status(&ent1.id).unwrap();
    let (state2, _) = gov.check_status(&ent2.id).unwrap();

    assert_eq!(state1, EntitlementState::Active);
    assert_eq!(state2, EntitlementState::Cancelled);
}

#[test]
fn test_receipt_chain_immutability() {
    let gov = EntitlementGovernor::new_default();
    let ent = gov.activate_entitlement("pro-sku", "cust-012").unwrap();

    // First transition
    gov.handle_event(
        &ent.id,
        EntitlementEvent::ApprovalGranted {
            actor: "admin".to_string(),
        },
    )
    .unwrap();

    // Second transition
    gov.handle_event(
        &ent.id,
        EntitlementEvent::PaymentFailed {
            reason: "Test".to_string(),
        },
    )
    .unwrap();

    // Get receipts
    let receipts = gov.get_receipt_chain(&ent.id).unwrap();
    assert_eq!(receipts.len(), 2);
    assert_eq!(receipts[0].from_state, "pending_approval");
    assert_eq!(receipts[0].to_state, "active");
    assert_eq!(receipts[1].from_state, "active");
    assert_eq!(receipts[1].to_state, "suspended");
}

#[test]
fn test_invalid_entitlement_id_error() {
    let gov = EntitlementGovernor::new_default();
    let result = gov.check_status("nonexistent-id");
    assert!(matches!(
        result,
        Err(MarketplaceError::EntitlementNotFound { .. })
    ));
}

#[test]
fn test_archived_state_is_terminal() {
    let gov = EntitlementGovernor::new_default();
    let ent = gov.activate_entitlement("pro-sku", "cust-017").unwrap();

    // Manual archive
    gov.handle_event(
        &ent.id,
        EntitlementEvent::Archive {
            reason: "Testing".to_string(),
        },
    )
    .unwrap();

    // Verify archived
    let (state, _) = gov.check_status(&ent.id).unwrap();
    assert_eq!(state, EntitlementState::Archived);

    // Try to transition from Archived - should fail
    let result = gov.handle_event(
        &ent.id,
        EntitlementEvent::ApprovalGranted {
            actor: "admin".to_string(),
        },
    );

    assert!(result.is_err());
}
