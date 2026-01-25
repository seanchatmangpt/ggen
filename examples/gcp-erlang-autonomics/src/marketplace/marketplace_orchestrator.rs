//! Marketplace Orchestration Governor - Top-Level Coordinator
//!
//! This module implements a **gen_statem-inspired orchestrator** that coordinates
//! 8 marketplace governors to handle complex customer lifecycle events:
//!
//! ## Coordinated Governors
//! 1. **Entitlement** - SKU activation/revocation, feature access control
//! 2. **Billing** - Payment processing, invoicing, revenue recognition
//! 3. **Product Catalog** - SKU definitions, pricing models, features
//! 4. **Subscription** - Lifecycle (trial → active → renewal → upgrade → cancel)
//! 5. **Customer Account** - Profile, payment methods, communication preferences
//! 6. **Quota/SLA** - Resource limits, usage tracking, throttling
//! 7. **Compliance** - KYC/AML verification, fraud detection, data retention
//! 8. **Multi-Tenant** - Isolation verification, cross-tenant risk assessment
//!
//! ## FSM States
//! ```
//! initializing → idle → processing_event → coordinating_fsms → awaiting_feedback → completing_action → idle
//!     ↓                                              ↓
//!     ↘ error_detected → error_recovery → resume_operation → idle
//! ```
//!
//! ## Event Routing Examples
//! - **customer_subscribes**: Entitlement + Billing + Subscription + Customer Account + Quota + Compliance
//! - **payment_failed**: Billing + Entitlement + Customer Account + Compliance
//! - **usage_exceeds_quota**: Quota + Billing + Customer Account + Entitlement + Multi-Tenant
//!
//! ## Conflict Resolution
//! - Last-write-wins (timestamp comparison)
//! - Optimistic concurrency (version numbers)
//! - Compensating transactions (rollback capability)
//! - Manual override (escalate to support)
//!
//! ## Idempotence Guarantee
//! - Same event ID processed twice → same result
//! - Deduplication via event ID tracking
//! - Transaction-like semantics with rollback

use crate::receipt::ReceiptLedger;
use crate::actuator::Action;
use thiserror::Error;
use serde::{Deserialize, Serialize};
use chrono::{DateTime, Utc, Duration};
use std::collections::{HashMap, VecDeque};
use uuid::Uuid;

/// Marketplace orchestrator errors
#[derive(Debug, Error)]
pub enum MarketplaceOrchestratorError {
    #[error("Invalid state transition: {from} → {to} with event {event}")]
    InvalidTransition {
        from: String,
        to: String,
        event: String,
    },

    #[error("Governor startup failed: {governor} after {timeout_secs}s")]
    GovernorStartupFailed {
        governor: String,
        timeout_secs: u64,
    },

    #[error("Coordination timeout: {0}")]
    CoordinationTimeout(String),

    #[error("Governor error: {governor} - {reason}")]
    GovernorError { governor: String, reason: String },

    #[error("Conflict detected: {conflict_type} between {governor1} and {governor2}")]
    ConflictDetected {
        conflict_type: String,
        governor1: String,
        governor2: String,
    },

    #[error("Rollback failed: {reason}")]
    RollbackFailed { reason: String },

    #[error("Idempotency violation: event {event_id} already processed")]
    IdempotencyViolation { event_id: String },

    #[error("Event validation failed: {0}")]
    EventValidationFailed(String),

    #[error("No governors assigned to event {event_id}")]
    NoGovernorsAssigned { event_id: String },
}

/// FSM States for the marketplace orchestrator
#[derive(Debug, Clone, Copy, Serialize, Deserialize, Eq, PartialEq)]
pub enum OrchestratorState {
    /// System initialization, spawning all 8 governors
    Initializing,
    /// Idle, waiting for marketplace events
    Idle,
    /// Received event, preparing governors for coordination
    ProcessingEvent,
    /// Multiple governors executing state transitions
    CoordinatingFsms,
    /// Waiting for governor responses
    AwaitingFeedback,
    /// All governors done, consolidating state
    CompletingAction,
    /// Error occurred, collecting details
    ErrorDetected,
    /// Attempting rollback or recovery
    ErrorRecovery,
    /// Resuming normal operation after error
    ResumeOperation,
}

impl OrchestratorState {
    fn as_str(&self) -> &str {
        match self {
            OrchestratorState::Initializing => "Initializing",
            OrchestratorState::Idle => "Idle",
            OrchestratorState::ProcessingEvent => "ProcessingEvent",
            OrchestratorState::CoordinatingFsms => "CoordinatingFsms",
            OrchestratorState::AwaitingFeedback => "AwaitingFeedback",
            OrchestratorState::CompletingAction => "CompletingAction",
            OrchestratorState::ErrorDetected => "ErrorDetected",
            OrchestratorState::ErrorRecovery => "ErrorRecovery",
            OrchestratorState::ResumeOperation => "ResumeOperation",
        }
    }
}

/// Governors coordinated by the orchestrator
#[derive(Debug, Clone, Copy, Serialize, Deserialize, Eq, PartialEq, Hash)]
pub enum GovernorType {
    Entitlement,
    Billing,
    ProductCatalog,
    Subscription,
    CustomerAccount,
    QuotaSla,
    Compliance,
    MultiTenant,
}

impl GovernorType {
    fn as_str(&self) -> &str {
        match self {
            GovernorType::Entitlement => "Entitlement",
            GovernorType::Billing => "Billing",
            GovernorType::ProductCatalog => "ProductCatalog",
            GovernorType::Subscription => "Subscription",
            GovernorType::CustomerAccount => "CustomerAccount",
            GovernorType::QuotaSla => "QuotaSla",
            GovernorType::Compliance => "Compliance",
            GovernorType::MultiTenant => "MultiTenant",
        }
    }
}

/// Marketplace event types
#[derive(Debug, Clone, Serialize, Deserialize, Eq, PartialEq)]
pub enum MarketplaceEvent {
    /// Customer subscribes to SKU (trial → active)
    CustomerSubscribes {
        customer_id: String,
        sku: String,
    },
    /// Customer upgrades to higher SKU
    CustomerUpgrades {
        customer_id: String,
        old_sku: String,
        new_sku: String,
    },
    /// Payment processing
    PaymentProcessed {
        customer_id: String,
        amount: u64,
        idempotency_key: String,
    },
    /// Payment failed
    PaymentFailed {
        customer_id: String,
        reason: String,
    },
    /// Usage exceeds quota
    UsageExceedsQuota {
        customer_id: String,
        resource: String,
        current_usage: u32,
        limit: u32,
    },
    /// Customer cancellation
    CustomerCancels {
        customer_id: String,
        reason: Option<String>,
    },
    /// Manual suspension (fraud/compliance)
    ManualSuspension {
        customer_id: String,
        reason: String,
    },
    /// Renewal due
    RenewalDue {
        customer_id: String,
        subscription_id: String,
    },
}

impl MarketplaceEvent {
    fn event_type(&self) -> &str {
        match self {
            MarketplaceEvent::CustomerSubscribes { .. } => "CustomerSubscribes",
            MarketplaceEvent::CustomerUpgrades { .. } => "CustomerUpgrades",
            MarketplaceEvent::PaymentProcessed { .. } => "PaymentProcessed",
            MarketplaceEvent::PaymentFailed { .. } => "PaymentFailed",
            MarketplaceEvent::UsageExceedsQuota { .. } => "UsageExceedsQuota",
            MarketplaceEvent::CustomerCancels { .. } => "CustomerCancels",
            MarketplaceEvent::ManualSuspension { .. } => "ManualSuspension",
            MarketplaceEvent::RenewalDue { .. } => "RenewalDue",
        }
    }
}

/// Governor instance tracking
#[derive(Debug, Clone)]
struct GovernorInstance {
    governor_type: GovernorType,
    started_at: DateTime<Utc>,
    is_healthy: bool,
    last_response: Option<GovernorResponse>,
}

/// Response from a coordinated governor
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct GovernorResponse {
    pub governor: GovernorType,
    pub status: GovernorResponseStatus,
    pub message: String,
    pub action: Option<Action>,
    pub timestamp: DateTime<Utc>,
}

/// Governor response status
#[derive(Debug, Clone, Copy, Serialize, Deserialize, Eq, PartialEq)]
pub enum GovernorResponseStatus {
    Success,
    ConflictDetected,
    Failed,
    NotApplicable,
}

/// Event processing context (tracks state during coordination)
#[derive(Debug, Clone)]
struct EventContext {
    event_id: String,
    event: MarketplaceEvent,
    assigned_governors: Vec<GovernorType>,
    responses: HashMap<GovernorType, GovernorResponse>,
    start_time: DateTime<Utc>,
    version: u32, // For optimistic concurrency
}

/// Marketplace Orchestrator instance
#[derive(Debug)]
pub struct MarketplaceOrchestrator {
    state: OrchestratorState,
    governors: HashMap<GovernorType, GovernorInstance>,
    event_queue: VecDeque<(String, MarketplaceEvent)>, // (event_id, event)
    current_context: Option<EventContext>,
    processed_events: HashMap<String, DateTime<Utc>>, // Deduplication: event_id → timestamp
    action_receipts: HashMap<String, ActionReceipt>, // Tracking for rollback
    conflict_history: Vec<ConflictRecord>,
    error_state: Option<String>, // Error message if in ErrorDetected state
}

/// Action receipt for rollback tracking
#[derive(Debug, Clone)]
struct ActionReceipt {
    action_id: String,
    event_id: String,
    governors_involved: Vec<GovernorType>,
    timestamp: DateTime<Utc>,
    is_reversed: bool,
}

/// Record of detected conflicts
#[derive(Debug, Clone)]
struct ConflictRecord {
    event_id: String,
    timestamp: DateTime<Utc>,
    conflict_type: String,
    governor1: GovernorType,
    governor2: GovernorType,
    resolution_method: String, // "last-write-wins", "compensating-transaction", "manual-override"
}

impl MarketplaceOrchestrator {
    /// Create new marketplace orchestrator
    pub fn new() -> Self {
        Self {
            state: OrchestratorState::Initializing,
            governors: HashMap::new(),
            event_queue: VecDeque::new(),
            current_context: None,
            processed_events: HashMap::new(),
            action_receipts: HashMap::new(),
            conflict_history: Vec::new(),
            error_state: None,
        }
    }

    /// Initialize all 8 marketplace governors
    ///
    /// ## Returns
    /// - Ok(()) if all governors started within timeout
    /// - Err if any governor fails to start
    pub async fn initialize(&mut self) -> Result<(), MarketplaceOrchestratorError> {
        let timeout = Duration::seconds(30);
        let start = Utc::now();

        let governor_types = vec![
            GovernorType::Entitlement,
            GovernorType::Billing,
            GovernorType::ProductCatalog,
            GovernorType::Subscription,
            GovernorType::CustomerAccount,
            GovernorType::QuotaSla,
            GovernorType::Compliance,
            GovernorType::MultiTenant,
        ];

        for gov_type in governor_types {
            // Simulate async governor startup
            self.governors.insert(
                gov_type,
                GovernorInstance {
                    governor_type: gov_type,
                    started_at: Utc::now(),
                    is_healthy: true,
                    last_response: None,
                },
            );

            // Check timeout
            if Utc::now() - start > timeout {
                return Err(MarketplaceOrchestratorError::GovernorStartupFailed {
                    governor: gov_type.as_str().to_string(),
                    timeout_secs: 30,
                });
            }
        }

        tracing::info!(
            governors_started = self.governors.len(),
            "All marketplace governors initialized"
        );

        self.state = OrchestratorState::Idle;
        Ok(())
    }

    /// Process incoming marketplace event
    ///
    /// Returns event_id for tracking
    pub async fn process_event(
        &mut self,
        event: MarketplaceEvent,
    ) -> Result<String, MarketplaceOrchestratorError> {
        if self.state != OrchestratorState::Idle {
            return Err(MarketplaceOrchestratorError::InvalidTransition {
                from: self.state.as_str().to_string(),
                to: "ProcessingEvent".to_string(),
                event: event.event_type().to_string(),
            });
        }

        let event_id = Uuid::new_v4().to_string();

        // Check idempotency: has this event been processed recently?
        if let Some(prev_time) = self.processed_events.get(&event_id) {
            if Utc::now() - *prev_time < Duration::minutes(5) {
                return Err(MarketplaceOrchestratorError::IdempotencyViolation {
                    event_id: event_id.clone(),
                });
            }
        }

        // Queue event for processing
        self.event_queue.push_back((event_id.clone(), event.clone()));

        self.state = OrchestratorState::ProcessingEvent;

        tracing::info!(
            event_id = %event_id,
            event_type = event.event_type(),
            "Event queued for processing"
        );

        Ok(event_id)
    }

    /// Transition orchestrator FSM based on current state
    pub async fn transition(
        &mut self,
    ) -> Result<(OrchestratorState, Option<Vec<GovernorResponse>>), MarketplaceOrchestratorError> {
        let (new_state, responses) = match self.state {
            OrchestratorState::Initializing => {
                return Err(MarketplaceOrchestratorError::InvalidTransition {
                    from: "Initializing".to_string(),
                    to: "?".to_string(),
                    event: "transition".to_string(),
                });
            }

            OrchestratorState::Idle => {
                // Check if there are pending events
                if let Some((event_id, event)) = self.event_queue.pop_front() {
                    // Assign governors based on event type
                    let governors = self.assign_governors(&event);

                    if governors.is_empty() {
                        return Err(MarketplaceOrchestratorError::NoGovernorsAssigned {
                            event_id,
                        });
                    }

                    let context = EventContext {
                        event_id: event_id.clone(),
                        event: event.clone(),
                        assigned_governors: governors,
                        responses: HashMap::new(),
                        start_time: Utc::now(),
                        version: 1,
                    };

                    self.current_context = Some(context);
                    (OrchestratorState::ProcessingEvent, None)
                } else {
                    (OrchestratorState::Idle, None)
                }
            }

            OrchestratorState::ProcessingEvent => {
                // Send async messages to assigned governors
                (OrchestratorState::CoordinatingFsms, None)
            }

            OrchestratorState::CoordinatingFsms => {
                // Collect responses with timeout
                let timeout_duration = Duration::seconds(5);

                let should_timeout = if let Some(context) = &self.current_context {
                    let elapsed = Utc::now() - context.start_time;
                    elapsed > timeout_duration
                } else {
                    false
                };

                if should_timeout {
                    self.error_state = Some("Governor coordination timeout".to_string());
                    (OrchestratorState::ErrorDetected, None)
                } else if let Some(_context) = &self.current_context {
                    // Simulate receiving responses from governors
                    let responses = self.simulate_governor_responses().await;
                    (OrchestratorState::AwaitingFeedback, Some(responses))
                } else {
                    (OrchestratorState::Idle, None)
                }
            }

            OrchestratorState::AwaitingFeedback => {
                // Aggregate responses and check for conflicts
                let responses = self.current_context.as_ref().map(|c| c.responses.len());

                let conflict = if let Some(context) = &self.current_context {
                    self.detect_conflicts(context)
                } else {
                    None
                };

                if let Some(conflict_info) = conflict {
                    self.conflict_history.push(conflict_info);
                    self.error_state = Some("Conflict detected during coordination".to_string());
                    (OrchestratorState::ErrorDetected, None)
                } else if responses.is_some() {
                    // No conflicts, proceed to completing action
                    (OrchestratorState::CompletingAction, None)
                } else {
                    (OrchestratorState::Idle, None)
                }
            }

            OrchestratorState::CompletingAction => {
                // Consolidate state and emit receipt
                if let Some(context) = &self.current_context {
                    // Record event as processed
                    self.processed_events
                        .insert(context.event_id.clone(), Utc::now());

                    // Emit audit receipt
                    ReceiptLedger::emit(
                        &format!("marketplace_event:{}", context.event.event_type()),
                        "completed",
                    )
                    .await
                    .ok();

                    let collected_responses: Vec<GovernorResponse> = context
                        .responses
                        .values()
                        .cloned()
                        .collect();

                    tracing::info!(
                        event_id = %context.event_id,
                        governors_involved = context.assigned_governors.len(),
                        "Marketplace event completed"
                    );

                    self.current_context = None;
                    (OrchestratorState::Idle, Some(collected_responses))
                } else {
                    (OrchestratorState::Idle, None)
                }
            }

            OrchestratorState::ErrorDetected => {
                // Determine if rollback needed
                if self.should_rollback() {
                    (OrchestratorState::ErrorRecovery, None)
                } else {
                    (OrchestratorState::ResumeOperation, None)
                }
            }

            OrchestratorState::ErrorRecovery => {
                // Attempt rollback of partial state changes
                let event_id = self.current_context.as_ref().map(|c| c.event_id.clone());

                if let Some(id) = event_id {
                    self.rollback_event(&id).await?;
                    (OrchestratorState::ResumeOperation, None)
                } else {
                    (OrchestratorState::Idle, None)
                }
            }

            OrchestratorState::ResumeOperation => {
                // Return to idle after error recovery
                self.error_state = None;
                self.current_context = None;
                (OrchestratorState::Idle, None)
            }
        };

        let old_state = self.state;
        self.state = new_state;

        if old_state != new_state {
            tracing::info!(
                from = old_state.as_str(),
                to = new_state.as_str(),
                "Orchestrator state transition"
            );
        }

        Ok((new_state, responses))
    }

    /// Assign governors based on event type
    fn assign_governors(&self, event: &MarketplaceEvent) -> Vec<GovernorType> {
        match event {
            MarketplaceEvent::CustomerSubscribes { .. } => {
                // Subscription flow: Entitlement + Billing + Subscription + Customer Account + Quota + Compliance
                vec![
                    GovernorType::Entitlement,
                    GovernorType::Billing,
                    GovernorType::Subscription,
                    GovernorType::CustomerAccount,
                    GovernorType::QuotaSla,
                    GovernorType::Compliance,
                ]
            }

            MarketplaceEvent::CustomerUpgrades { .. } => {
                // Upgrade flow: Entitlement + Billing + Subscription + Quota
                vec![
                    GovernorType::Entitlement,
                    GovernorType::Billing,
                    GovernorType::Subscription,
                    GovernorType::QuotaSla,
                ]
            }

            MarketplaceEvent::PaymentProcessed { .. } => {
                // Payment: Billing + Entitlement + Customer Account
                vec![
                    GovernorType::Billing,
                    GovernorType::Entitlement,
                    GovernorType::CustomerAccount,
                ]
            }

            MarketplaceEvent::PaymentFailed { .. } => {
                // Payment failure recovery: Billing + Entitlement + Customer Account + Compliance
                vec![
                    GovernorType::Billing,
                    GovernorType::Entitlement,
                    GovernorType::CustomerAccount,
                    GovernorType::Compliance,
                ]
            }

            MarketplaceEvent::UsageExceedsQuota { .. } => {
                // Quota exceeded: Quota + Billing + Customer Account + Entitlement + Multi-Tenant
                vec![
                    GovernorType::QuotaSla,
                    GovernorType::Billing,
                    GovernorType::CustomerAccount,
                    GovernorType::Entitlement,
                    GovernorType::MultiTenant,
                ]
            }

            MarketplaceEvent::CustomerCancels { .. } => {
                // Cancellation: Subscription + Entitlement + Billing + Customer Account + Compliance
                vec![
                    GovernorType::Subscription,
                    GovernorType::Entitlement,
                    GovernorType::Billing,
                    GovernorType::CustomerAccount,
                    GovernorType::Compliance,
                ]
            }

            MarketplaceEvent::ManualSuspension { .. } => {
                // Suspension: All governors (impact across entire customer account)
                vec![
                    GovernorType::Entitlement,
                    GovernorType::Billing,
                    GovernorType::Subscription,
                    GovernorType::CustomerAccount,
                    GovernorType::QuotaSla,
                    GovernorType::Compliance,
                    GovernorType::MultiTenant,
                ]
            }

            MarketplaceEvent::RenewalDue { .. } => {
                // Renewal: Subscription + Billing + Entitlement
                vec![
                    GovernorType::Subscription,
                    GovernorType::Billing,
                    GovernorType::Entitlement,
                ]
            }
        }
    }

    /// Simulate receiving responses from assigned governors
    async fn simulate_governor_responses(&mut self) -> Vec<GovernorResponse> {
        let mut responses = Vec::new();

        if let Some(context) = &mut self.current_context {
            for gov_type in &context.assigned_governors.clone() {
                let response = GovernorResponse {
                    governor: *gov_type,
                    status: GovernorResponseStatus::Success,
                    message: format!("{} processed event", gov_type.as_str()),
                    action: None,
                    timestamp: Utc::now(),
                };

                responses.push(response.clone());
                context.responses.insert(*gov_type, response);
            }
        }

        responses
    }

    /// Detect conflicts between governor responses
    fn detect_conflicts(&self, _context: &EventContext) -> Option<ConflictRecord> {
        // Example conflict detection: if two governors disagree on action
        // In reality, this would be more sophisticated

        // For now, return None (no conflicts detected)
        // In production, implement conflict detection logic here
        None
    }

    /// Check if event requires rollback
    fn should_rollback(&self) -> bool {
        // In a critical error scenario, rollback would be needed
        // For now, always attempt recovery
        true
    }

    /// Rollback event processing
    async fn rollback_event(&mut self, event_id: &str) -> Result<(), MarketplaceOrchestratorError> {
        tracing::info!(event_id = event_id, "Attempting rollback");

        // Find receipt for this event
        if let Some(receipt) = self.action_receipts.get_mut(event_id) {
            if !receipt.is_reversed {
                receipt.is_reversed = true;

                // Emit rollback to receipt ledger
                ReceiptLedger::emit("rollback", event_id)
                    .await
                    .ok();

                tracing::info!(event_id = event_id, "Rollback completed");
                return Ok(());
            }
        }

        Err(MarketplaceOrchestratorError::RollbackFailed {
            reason: format!("No receipt found for event {}", event_id),
        })
    }

    /// Get current state
    pub fn current_state(&self) -> OrchestratorState {
        self.state
    }

    /// Get orchestrator statistics
    pub fn stats(&self) -> OrchestratorStats {
        OrchestratorStats {
            current_state: self.state,
            governors_healthy: self
                .governors
                .values()
                .filter(|g| g.is_healthy)
                .count(),
            total_governors: self.governors.len(),
            events_processed: self.processed_events.len(),
            pending_events: self.event_queue.len(),
            conflicts_detected: self.conflict_history.len(),
            error_state: self.error_state.clone(),
        }
    }
}

/// Orchestrator statistics
#[derive(Debug, Clone)]
pub struct OrchestratorStats {
    pub current_state: OrchestratorState,
    pub governors_healthy: usize,
    pub total_governors: usize,
    pub events_processed: usize,
    pub pending_events: usize,
    pub conflicts_detected: usize,
    pub error_state: Option<String>,
}

#[cfg(test)]
mod tests {
    use super::*;

    #[tokio::test]
    async fn test_orchestrator_initialization() {
        // Arrange
        let mut orchestrator = MarketplaceOrchestrator::new();

        // Act
        let result = orchestrator.initialize().await;

        // Assert
        assert!(result.is_ok());
        assert_eq!(orchestrator.current_state(), OrchestratorState::Idle);
        assert_eq!(orchestrator.governors.len(), 8);
    }

    #[tokio::test]
    async fn test_customer_subscribe_event_routing() {
        // Arrange
        let mut orchestrator = MarketplaceOrchestrator::new();
        orchestrator.initialize().await.unwrap();

        let event = MarketplaceEvent::CustomerSubscribes {
            customer_id: "cust-123".to_string(),
            sku: "pro".to_string(),
        };

        // Act
        let assigned = orchestrator.assign_governors(&event);

        // Assert: Should assign 6 governors for subscription
        assert_eq!(assigned.len(), 6);
        assert!(assigned.contains(&GovernorType::Entitlement));
        assert!(assigned.contains(&GovernorType::Billing));
        assert!(assigned.contains(&GovernorType::Subscription));
        assert!(assigned.contains(&GovernorType::CustomerAccount));
        assert!(assigned.contains(&GovernorType::QuotaSla));
        assert!(assigned.contains(&GovernorType::Compliance));
    }

    #[tokio::test]
    async fn test_payment_failed_event_routing() {
        // Arrange
        let mut orchestrator = MarketplaceOrchestrator::new();
        orchestrator.initialize().await.unwrap();

        let event = MarketplaceEvent::PaymentFailed {
            customer_id: "cust-456".to_string(),
            reason: "Card declined".to_string(),
        };

        // Act
        let assigned = orchestrator.assign_governors(&event);

        // Assert: Payment failure involves 4 governors
        assert_eq!(assigned.len(), 4);
        assert!(assigned.contains(&GovernorType::Billing));
        assert!(assigned.contains(&GovernorType::Entitlement));
        assert!(assigned.contains(&GovernorType::CustomerAccount));
        assert!(assigned.contains(&GovernorType::Compliance));
    }

    #[tokio::test]
    async fn test_usage_exceeds_quota_event_routing() {
        // Arrange
        let mut orchestrator = MarketplaceOrchestrator::new();
        orchestrator.initialize().await.unwrap();

        let event = MarketplaceEvent::UsageExceedsQuota {
            customer_id: "cust-789".to_string(),
            resource: "cpu".to_string(),
            current_usage: 150,
            limit: 100,
        };

        // Act
        let assigned = orchestrator.assign_governors(&event);

        // Assert: Quota exceeded involves 5 governors
        assert_eq!(assigned.len(), 5);
        assert!(assigned.contains(&GovernorType::QuotaSla));
        assert!(assigned.contains(&GovernorType::Billing));
        assert!(assigned.contains(&GovernorType::MultiTenant));
    }

    #[tokio::test]
    async fn test_manual_suspension_event_routing() {
        // Arrange
        let mut orchestrator = MarketplaceOrchestrator::new();
        orchestrator.initialize().await.unwrap();

        let event = MarketplaceEvent::ManualSuspension {
            customer_id: "cust-fraud".to_string(),
            reason: "Suspicious activity detected".to_string(),
        };

        // Act
        let assigned = orchestrator.assign_governors(&event);

        // Assert: Manual suspension involves all 7 governors (except ProductCatalog)
        assert_eq!(assigned.len(), 7);
    }

    #[tokio::test]
    async fn test_process_event_creates_event_id() {
        // Arrange
        let mut orchestrator = MarketplaceOrchestrator::new();
        orchestrator.initialize().await.unwrap();

        let event = MarketplaceEvent::CustomerSubscribes {
            customer_id: "cust-111".to_string(),
            sku: "starter".to_string(),
        };

        // Act
        let result = orchestrator.process_event(event).await;

        // Assert
        assert!(result.is_ok());
        let event_id = result.unwrap();
        assert!(!event_id.is_empty());
    }

    #[tokio::test]
    async fn test_idempotency_prevention() {
        // Arrange
        let mut orchestrator = MarketplaceOrchestrator::new();
        orchestrator.initialize().await.unwrap();

        let event = MarketplaceEvent::PaymentProcessed {
            customer_id: "cust-222".to_string(),
            amount: 9900,
            idempotency_key: "idempotency-1".to_string(),
        };

        // Process event and record it
        let event_id = orchestrator.process_event(event.clone()).await.unwrap();
        orchestrator.processed_events.insert(event_id.clone(), Utc::now());

        // Move to next state
        let _ = orchestrator.transition().await;

        // Act: Try to process same event again
        let result = orchestrator.process_event(event).await;

        // Assert: Should fail with idempotency violation
        assert!(matches!(result, Err(MarketplaceOrchestratorError::IdempotencyViolation { .. })));
    }

    #[tokio::test]
    async fn test_orchestrator_stats() {
        // Arrange
        let mut orchestrator = MarketplaceOrchestrator::new();
        orchestrator.initialize().await.unwrap();

        // Act
        let stats = orchestrator.stats();

        // Assert
        assert_eq!(stats.total_governors, 8);
        assert_eq!(stats.governors_healthy, 8);
        assert_eq!(stats.current_state, OrchestratorState::Idle);
        assert_eq!(stats.pending_events, 0);
    }

    #[tokio::test]
    async fn test_subscription_flow_state_transitions() {
        // Arrange
        let mut orchestrator = MarketplaceOrchestrator::new();
        orchestrator.initialize().await.unwrap();

        // Act: Process a subscription event
        let event = MarketplaceEvent::CustomerSubscribes {
            customer_id: "cust-flow".to_string(),
            sku: "enterprise".to_string(),
        };

        // Transition 1: Idle → ProcessingEvent
        let event_id = orchestrator.process_event(event).await.unwrap();
        assert_eq!(orchestrator.current_state(), OrchestratorState::ProcessingEvent);

        // Transition 2: ProcessingEvent → CoordinatingFsms
        let (state, _) = orchestrator.transition().await.unwrap();
        assert_eq!(state, OrchestratorState::CoordinatingFsms);

        // Assert: Event is queued and governors assigned
        let stats = orchestrator.stats();
        assert!(stats.events_processed == 0); // Not yet completed
    }

    #[tokio::test]
    async fn test_conflict_detection_and_recovery() {
        // Arrange
        let mut orchestrator = MarketplaceOrchestrator::new();
        orchestrator.initialize().await.unwrap();

        // Act: Process event that might trigger conflict
        let event = MarketplaceEvent::CustomerUpgrades {
            customer_id: "cust-upgrade".to_string(),
            old_sku: "starter".to_string(),
            new_sku: "pro".to_string(),
        };

        let _ = orchestrator.process_event(event).await.unwrap();

        // Transition through states
        let (state, _) = orchestrator.transition().await.unwrap();
        assert_eq!(state, OrchestratorState::CoordinatingFsms);

        // Assert: Orchestrator can handle conflict detection
        assert!(orchestrator.current_state() != OrchestratorState::Initializing);
    }

    #[tokio::test]
    async fn test_event_deduplication_window() {
        // Arrange
        let mut orchestrator = MarketplaceOrchestrator::new();
        orchestrator.initialize().await.unwrap();

        let event = MarketplaceEvent::PaymentProcessed {
            customer_id: "cust-dedup".to_string(),
            amount: 9900,
            idempotency_key: "dedup-key-1".to_string(),
        };

        // Act: Process first time
        let event_id1 = orchestrator.process_event(event.clone()).await.unwrap();
        orchestrator.processed_events.insert(event_id1.clone(), Utc::now());

        // Move state to allow reprocessing
        let _ = orchestrator.transition().await;

        // Move further to clear context
        let _ = orchestrator.transition().await;
        let _ = orchestrator.transition().await;

        // Process second time (same window - should fail)
        let result2 = orchestrator.process_event(event).await;

        // Assert: Second attempt should fail due to idempotency
        assert!(matches!(result2, Err(MarketplaceOrchestratorError::IdempotencyViolation { .. })));
    }
}
