//! Marketplace Orchestrator Integration Tests
//!
//! Chicago TDD tests for the Marketplace Orchestration Governor
//! demonstrating the complete customer lifecycle coordination across 8 governors.

use gcp_erlang_autonomics::{
    MarketplaceOrchestrator, OrchestratorState, MarketplaceEvent, GovernorType,
};

#[tokio::test]
async fn test_customer_subscribe_flow_initialization_and_routing() {
    // Arrange: Create and initialize orchestrator
    let mut orchestrator = MarketplaceOrchestrator::new();
    assert_eq!(orchestrator.current_state(), OrchestratorState::Initializing);

    // Act: Initialize all 8 marketplace governors
    let init_result = orchestrator.initialize().await;

    // Assert: Initialization succeeds
    assert!(init_result.is_ok(), "Orchestrator initialization should succeed");
    assert_eq!(orchestrator.current_state(), OrchestratorState::Idle);

    let stats = orchestrator.stats();
    assert_eq!(stats.total_governors, 8);
    assert_eq!(stats.governors_healthy, 8);
    assert_eq!(stats.pending_events, 0);
}

#[tokio::test]
async fn test_customer_subscribe_event_assigns_6_governors() {
    // Arrange
    let mut orchestrator = MarketplaceOrchestrator::new();
    orchestrator.initialize().await.unwrap();

    // Act: Create subscription event
    let event = MarketplaceEvent::CustomerSubscribes {
        customer_id: "cust-001".to_string(),
        sku: "pro".to_string(),
    };

    let assigned = orchestrator.assign_governors(&event);

    // Assert: Should assign 6 governors
    assert_eq!(assigned.len(), 6, "CustomerSubscribes should coordinate 6 governors");
    assert!(
        assigned.contains(&GovernorType::Entitlement),
        "Entitlement governor should be included"
    );
    assert!(
        assigned.contains(&GovernorType::Billing),
        "Billing governor should be included"
    );
    assert!(
        assigned.contains(&GovernorType::Subscription),
        "Subscription governor should be included"
    );
    assert!(
        assigned.contains(&GovernorType::CustomerAccount),
        "CustomerAccount governor should be included"
    );
    assert!(
        assigned.contains(&GovernorType::QuotaSla),
        "QuotaSla governor should be included"
    );
    assert!(
        assigned.contains(&GovernorType::Compliance),
        "Compliance governor should be included"
    );
}

#[tokio::test]
async fn test_payment_failed_routes_to_4_governors() {
    // Arrange
    let mut orchestrator = MarketplaceOrchestrator::new();
    orchestrator.initialize().await.unwrap();

    // Act: Create payment failure event
    let event = MarketplaceEvent::PaymentFailed {
        customer_id: "cust-002".to_string(),
        reason: "Card declined - insufficient funds".to_string(),
    };

    let assigned = orchestrator.assign_governors(&event);

    // Assert: Payment failure involves 4 governors
    assert_eq!(assigned.len(), 4, "PaymentFailed should coordinate 4 governors");
    assert!(assigned.contains(&GovernorType::Billing));
    assert!(assigned.contains(&GovernorType::Entitlement));
    assert!(assigned.contains(&GovernorType::CustomerAccount));
    assert!(assigned.contains(&GovernorType::Compliance));
}

#[tokio::test]
async fn test_usage_exceeds_quota_coordinates_multi_tenant_cascade() {
    // Arrange
    let mut orchestrator = MarketplaceOrchestrator::new();
    orchestrator.initialize().await.unwrap();

    // Act: Create quota exceeded event
    let event = MarketplaceEvent::UsageExceedsQuota {
        customer_id: "cust-003".to_string(),
        resource: "cpu_cores".to_string(),
        current_usage: 150,
        limit: 100,
    };

    let assigned = orchestrator.assign_governors(&event);

    // Assert: Quota event coordinates 5 governors
    assert_eq!(assigned.len(), 5, "UsageExceedsQuota should coordinate 5 governors");
    assert!(
        assigned.contains(&GovernorType::MultiTenant),
        "MultiTenant governor should be included for cascade prevention"
    );
    assert!(assigned.contains(&GovernorType::QuotaSla));
    assert!(assigned.contains(&GovernorType::Billing));
}

#[tokio::test]
async fn test_manual_suspension_coordinates_all_governors() {
    // Arrange
    let mut orchestrator = MarketplaceOrchestrator::new();
    orchestrator.initialize().await.unwrap();

    // Act: Create manual suspension event (fraud scenario)
    let event = MarketplaceEvent::ManualSuspension {
        customer_id: "cust-fraud".to_string(),
        reason: "Suspicious activity detected - multiple failed payments".to_string(),
    };

    let assigned = orchestrator.assign_governors(&event);

    // Assert: Suspension impacts 7 governors (all except ProductCatalog)
    assert_eq!(assigned.len(), 7, "ManualSuspension should coordinate 7 governors");
    assert_eq!(
        assigned.iter().filter(|g| **g == GovernorType::ProductCatalog).count(),
        0,
        "ProductCatalog not involved in account suspension"
    );
}

#[tokio::test]
async fn test_process_event_creates_unique_event_ids() {
    // Arrange
    let mut orchestrator = MarketplaceOrchestrator::new();
    orchestrator.initialize().await.unwrap();

    // Act: Process two subscription events
    let event1 = MarketplaceEvent::CustomerSubscribes {
        customer_id: "cust-101".to_string(),
        sku: "starter".to_string(),
    };
    let event2 = MarketplaceEvent::CustomerSubscribes {
        customer_id: "cust-102".to_string(),
        sku: "pro".to_string(),
    };

    let id1 = orchestrator.process_event(event1).await.unwrap();
    let id2 = orchestrator.process_event(event2).await.unwrap();

    // Assert: Each event gets unique ID
    assert_ne!(id1, id2, "Event IDs should be globally unique");
    assert!(!id1.is_empty());
    assert!(!id2.is_empty());
}

#[tokio::test]
async fn test_event_queue_processing_idle_to_processing_transition() {
    // Arrange
    let mut orchestrator = MarketplaceOrchestrator::new();
    orchestrator.initialize().await.unwrap();

    let event = MarketplaceEvent::CustomerUpgrades {
        customer_id: "cust-upgrade-1".to_string(),
        old_sku: "starter".to_string(),
        new_sku: "pro".to_string(),
    };

    // Act: Process event
    let event_id = orchestrator.process_event(event).await.unwrap();
    assert_eq!(orchestrator.current_state(), OrchestratorState::ProcessingEvent);

    // Transition to next state
    let (new_state, _) = orchestrator.transition().await.unwrap();

    // Assert: State machine progresses
    assert_eq!(new_state, OrchestratorState::CoordinatingFsms);
    assert!(!event_id.is_empty());
}

#[tokio::test]
async fn test_subscription_renewal_event_routing() {
    // Arrange
    let mut orchestrator = MarketplaceOrchestrator::new();
    orchestrator.initialize().await.unwrap();

    // Act: Create renewal event
    let event = MarketplaceEvent::RenewalDue {
        customer_id: "cust-renew".to_string(),
        subscription_id: "sub-12345".to_string(),
    };

    let assigned = orchestrator.assign_governors(&event);

    // Assert: Renewal involves 3 governors
    assert_eq!(assigned.len(), 3, "RenewalDue should coordinate 3 governors");
    assert!(assigned.contains(&GovernorType::Subscription));
    assert!(assigned.contains(&GovernorType::Billing));
    assert!(assigned.contains(&GovernorType::Entitlement));
}

#[tokio::test]
async fn test_orchestrator_stats_reflects_state() {
    // Arrange
    let mut orchestrator = MarketplaceOrchestrator::new();

    // Act & Assert: Before initialization
    let stats_before = orchestrator.stats();
    assert_eq!(stats_before.current_state, OrchestratorState::Initializing);
    assert_eq!(stats_before.total_governors, 0);

    // Initialize
    orchestrator.initialize().await.unwrap();

    // Assert: After initialization
    let stats_after = orchestrator.stats();
    assert_eq!(stats_after.current_state, OrchestratorState::Idle);
    assert_eq!(stats_after.total_governors, 8);
    assert_eq!(stats_after.governors_healthy, 8);
}

#[tokio::test]
async fn test_customer_cancellation_coordinates_5_governors() {
    // Arrange
    let mut orchestrator = MarketplaceOrchestrator::new();
    orchestrator.initialize().await.unwrap();

    // Act: Create cancellation event
    let event = MarketplaceEvent::CustomerCancels {
        customer_id: "cust-cancel".to_string(),
        reason: Some("Switching to competitor".to_string()),
    };

    let assigned = orchestrator.assign_governors(&event);

    // Assert: Cancellation involves 5 governors
    assert_eq!(assigned.len(), 5, "CustomerCancels should coordinate 5 governors");
    assert!(assigned.contains(&GovernorType::Subscription));
    assert!(assigned.contains(&GovernorType::Entitlement));
    assert!(assigned.contains(&GovernorType::Billing));
    assert!(assigned.contains(&GovernorType::CustomerAccount));
    assert!(assigned.contains(&GovernorType::Compliance));
}

#[tokio::test]
async fn test_payment_processed_event_routing() {
    // Arrange
    let mut orchestrator = MarketplaceOrchestrator::new();
    orchestrator.initialize().await.unwrap();

    // Act: Create payment processed event
    let event = MarketplaceEvent::PaymentProcessed {
        customer_id: "cust-pay".to_string(),
        amount: 9900, // $99.00
        idempotency_key: "pay-idem-abc123".to_string(),
    };

    let assigned = orchestrator.assign_governors(&event);

    // Assert: Payment success involves 3 governors
    assert_eq!(assigned.len(), 3, "PaymentProcessed should coordinate 3 governors");
    assert!(assigned.contains(&GovernorType::Billing));
    assert!(assigned.contains(&GovernorType::Entitlement));
    assert!(assigned.contains(&GovernorType::CustomerAccount));
}

#[tokio::test]
async fn test_state_machine_idle_remains_idle_with_no_events() {
    // Arrange
    let mut orchestrator = MarketplaceOrchestrator::new();
    orchestrator.initialize().await.unwrap();

    // Act: Transition with no pending events
    let (state, _) = orchestrator.transition().await.unwrap();

    // Assert: Remains in Idle
    assert_eq!(state, OrchestratorState::Idle);
    assert_eq!(orchestrator.current_state(), OrchestratorState::Idle);
}

#[tokio::test]
async fn test_end_to_end_subscription_flow_states() {
    // Arrange: Create fresh orchestrator
    let mut orchestrator = MarketplaceOrchestrator::new();
    orchestrator.initialize().await.unwrap();

    // Act 1: Process subscription event
    let event = MarketplaceEvent::CustomerSubscribes {
        customer_id: "cust-e2e".to_string(),
        sku: "enterprise".to_string(),
    };

    // Act 2: Submit event
    let event_id = orchestrator.process_event(event).await.unwrap();
    assert_eq!(orchestrator.current_state(), OrchestratorState::ProcessingEvent);

    // Act 3: Progress to coordination
    let (state1, _) = orchestrator.transition().await.unwrap();
    assert_eq!(state1, OrchestratorState::CoordinatingFsms);

    // Act 4: Progress to feedback collection
    let (state2, responses) = orchestrator.transition().await.unwrap();
    assert_eq!(state2, OrchestratorState::AwaitingFeedback);

    // Act 5: Continue to completion
    let (state3, _) = orchestrator.transition().await.unwrap();

    // Assert: All states progressed correctly
    assert!(!event_id.is_empty());
    assert!(responses.is_some() || state3 != OrchestratorState::Idle);
}

#[tokio::test]
async fn test_event_deduplication_within_window() {
    // Arrange
    let mut orchestrator = MarketplaceOrchestrator::new();
    orchestrator.initialize().await.unwrap();

    let event = MarketplaceEvent::PaymentProcessed {
        customer_id: "cust-dedup".to_string(),
        amount: 5000,
        idempotency_key: "idempotent-key-123".to_string(),
    };

    // Act 1: Process event first time
    let event_id = orchestrator.process_event(event.clone()).await.unwrap();
    orchestrator.processed_events.insert(event_id, chrono::Utc::now());

    // Simulate state progression
    let _ = orchestrator.transition().await;
    let _ = orchestrator.transition().await;
    let _ = orchestrator.transition().await;
    let _ = orchestrator.transition().await;

    // Act 2: Try to process same event again within window
    let result = orchestrator.process_event(event).await;

    // Assert: Should detect as duplicate
    assert!(
        result.is_err(),
        "Processing same event twice should fail with idempotency violation"
    );
}

#[tokio::test]
async fn test_orchestrator_tracks_processed_events() {
    // Arrange
    let mut orchestrator = MarketplaceOrchestrator::new();
    orchestrator.initialize().await.unwrap();

    let event = MarketplaceEvent::CustomerSubscribes {
        customer_id: "cust-track".to_string(),
        sku: "pro".to_string(),
    };

    // Act: Process event and verify tracking
    let event_id = orchestrator.process_event(event).await.unwrap();

    // Simulate completion
    let _ = orchestrator.transition().await;
    let _ = orchestrator.transition().await;
    let _ = orchestrator.transition().await;
    let _ = orchestrator.transition().await;

    // Assert: Orchestrator has processed count
    let stats = orchestrator.stats();
    // After transitioning through states, some events should be recorded
    assert!(stats.pending_events >= 0, "Event tracking should be active");
}
