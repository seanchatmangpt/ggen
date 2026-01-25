//! Marketplace Entitlement Governor - Production-grade gen_statem FSM
//!
//! Implements a type-safe, event-driven finite state machine for SaaS marketplace
//! entitlements with automatic state transitions, timeouts, and receipt-based audit trails.

use chrono::{DateTime, Duration, Utc};
use serde::{Deserialize, Serialize};
use std::collections::HashMap;
use std::sync::Arc;
use thiserror::Error;
use uuid::Uuid;

/// Marketplace entitlement errors
#[derive(Debug, Error, Clone)]
pub enum MarketplaceError {
    #[error("Invalid state transition: {from} → {to}")]
    InvalidTransition { from: String, to: String },

    #[error("Entitlement not found: {entitlement_id}")]
    EntitlementNotFound { entitlement_id: String },

    #[error("Timeout exceeded: {reason}")]
    TimeoutExceeded { reason: String },

    #[error("Invalid input: {0}")]
    InvalidInput(String),

    #[error("GCP Marketplace API error: {0}")]
    ApiError(String),

    #[error("Refund processing error: {0}")]
    RefundError(String),

    #[error("State invariant violated: {0}")]
    InvariantViolated(String),

    #[error("Operation not allowed in state: {state}")]
    OperationNotAllowed { state: String },
}

/// FSM Entitlement States - Impossible states are impossible through type system
#[derive(Debug, Clone, Copy, PartialEq, Eq, Serialize, Deserialize)]
pub enum EntitlementState {
    /// Initial: Waiting for GCP Marketplace approval (24h timeout)
    PendingApproval,
    /// Active: Normal operation (indefinite)
    Active,
    /// Suspended: Payment failed or compliance issue (72h timeout)
    Suspended,
    /// Expired: Subscription period ended (30d timeout)
    Expired,
    /// Cancelled: Customer or admin cancelled (14d timeout)
    Cancelled,
    /// RefundIssued: Refund in progress (14d timeout)
    RefundIssued,
    /// ReinstallPending: Awaiting reinstatement approval (7d timeout)
    ReinstallPending,
    /// Archived: Final state, immutable (terminal)
    Archived,
}

impl EntitlementState {
    pub fn as_str(&self) -> &str {
        match self {
            EntitlementState::PendingApproval => "pending_approval",
            EntitlementState::Active => "active",
            EntitlementState::Suspended => "suspended",
            EntitlementState::Expired => "expired",
            EntitlementState::Cancelled => "cancelled",
            EntitlementState::RefundIssued => "refund_issued",
            EntitlementState::ReinstallPending => "reinstate_pending",
            EntitlementState::Archived => "archived",
        }
    }

    /// Check if this is a terminal state (no further transitions)
    pub fn is_terminal(&self) -> bool {
        matches!(self, EntitlementState::Archived)
    }
}

/// Events that trigger FSM transitions
#[derive(Debug, Clone, Serialize, Deserialize)]
pub enum EntitlementEvent {
    /// GCP Marketplace approved the subscription
    ApprovalGranted { actor: String },
    /// GCP Marketplace rejected the subscription
    ApprovalRejected { reason: String },
    /// Approval timeout (24h) - auto-reject
    ApprovalTimeout,
    /// Payment processing failed
    PaymentFailed { reason: String },
    /// Payment received - resume service
    PaymentReceived,
    /// Admin suspended for compliance
    SuspendedByAdmin { reason: String },
    /// Compliance issue fixed - can resume
    ComplianceFixed,
    /// Escalate suspension to cancellation
    EscalateToCancellation { reason: String },
    /// Suspension timeout (72h) - auto-cancel
    SuspensionTimeout,
    /// Customer requests to cancel
    CustomerCancels { reason: String },
    /// Subscription period expired
    ExpirationApproaching,
    /// Subscription expired - 30d window before archive
    SubscriptionExpired,
    /// Expiration timeout (30d) - auto-archive
    ExpirationTimeout,
    /// Refund was approved and issued
    RefundApproved { amount: f64 },
    /// Refund completed
    RefundCompleted,
    /// Refund failed
    RefundFailed { reason: String },
    /// Refund dispute raised
    RefundDispute,
    /// Refund timeout (14d) - mark as completed
    RefundTimeout,
    /// Customer wants to reactivate
    ReinstatementRequested,
    /// Admin approved reinstatement
    ReinstatementApproved { actor: String },
    /// Admin rejected reinstatement
    ReinstatementRejected { reason: String },
    /// Reinstatement timeout (7d) - auto-reject
    ReinstatementTimeout,
    /// Archive the entitlement (move to final state)
    Archive { reason: String },
}

/// Configuration for entitlement behavior
#[derive(Debug, Clone)]
pub struct EntitlementConfig {
    /// Approval timeout in hours (default: 24)
    pub approval_timeout_hours: u32,
    /// Suspension timeout in hours (default: 72)
    pub suspension_timeout_hours: u32,
    /// Refund processing window in days (default: 14)
    pub refund_window_days: u32,
    /// Reinstatement approval window in days (default: 7)
    pub reinstatement_window_days: u32,
    /// Expiration archive window in days (default: 30)
    pub expiration_archive_days: u32,
}

impl Default for EntitlementConfig {
    fn default() -> Self {
        Self {
            approval_timeout_hours: 24,
            suspension_timeout_hours: 72,
            refund_window_days: 14,
            reinstatement_window_days: 7,
            expiration_archive_days: 30,
        }
    }
}

/// Receipt entry for audit trail
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct StateChangeReceipt {
    pub id: String,
    pub entitlement_id: String,
    pub timestamp: DateTime<Utc>,
    pub from_state: String,
    pub to_state: String,
    pub event: String,
    pub actor: String, // customer_id, admin_id, or "system"
    pub details: String,
}

impl StateChangeReceipt {
    pub fn new(
        entitlement_id: String,
        from_state: EntitlementState,
        to_state: EntitlementState,
        event: String,
        actor: String,
        details: String,
    ) -> Self {
        Self {
            id: Uuid::new_v4().to_string(),
            entitlement_id,
            timestamp: Utc::now(),
            from_state: from_state.as_str().to_string(),
            to_state: to_state.as_str().to_string(),
            event,
            actor,
            details,
        }
    }
}

/// Entitlement data
#[derive(Debug, Clone)]
pub struct Entitlement {
    pub id: String,
    pub customer_id: String,
    pub sku_id: String,
    pub state: EntitlementState,
    pub created_at: DateTime<Utc>,
    pub expires_at: Option<DateTime<Utc>>,
    pub last_state_change: DateTime<Utc>,
    pub timeout_at: Option<DateTime<Utc>>,
    pub refund_amount: Option<f64>,
    pub receipt_chain: Vec<StateChangeReceipt>,
}

impl Entitlement {
    pub fn new(customer_id: String, sku_id: String) -> Self {
        let now = Utc::now();
        Self {
            id: Uuid::new_v4().to_string(),
            customer_id,
            sku_id,
            state: EntitlementState::PendingApproval,
            created_at: now,
            expires_at: None,
            last_state_change: now,
            timeout_at: None,
            refund_amount: None,
            receipt_chain: Vec::new(),
        }
    }

    /// Check if timeout has been exceeded (if one is set)
    pub fn is_timeout_exceeded(&self) -> bool {
        if let Some(timeout_at) = self.timeout_at {
            Utc::now() > timeout_at
        } else {
            false
        }
    }

    /// Add receipt and record state change
    fn record_transition(
        &mut self,
        to_state: EntitlementState,
        event: String,
        actor: String,
        details: String,
        config: &EntitlementConfig,
    ) {
        let from_state = self.state;
        let receipt = StateChangeReceipt::new(
            self.id.clone(),
            from_state,
            to_state,
            event,
            actor,
            details,
        );

        self.receipt_chain.push(receipt);
        self.state = to_state;
        self.last_state_change = Utc::now();

        // Set timeout for states that have them
        self.timeout_at = match to_state {
            EntitlementState::PendingApproval => {
                Some(Utc::now() + Duration::hours(config.approval_timeout_hours as i64))
            }
            EntitlementState::Suspended => {
                Some(Utc::now() + Duration::hours(config.suspension_timeout_hours as i64))
            }
            EntitlementState::Cancelled => {
                Some(Utc::now() + Duration::days(config.refund_window_days as i64))
            }
            EntitlementState::RefundIssued => {
                Some(Utc::now() + Duration::days(config.refund_window_days as i64))
            }
            EntitlementState::ReinstallPending => {
                Some(Utc::now() + Duration::days(config.reinstatement_window_days as i64))
            }
            EntitlementState::Expired => {
                Some(Utc::now() + Duration::days(config.expiration_archive_days as i64))
            }
            _ => None,
        };
    }
}

/// The Marketplace Entitlement Governor - manages FSM for multi-tenant entitlements
pub struct EntitlementGovernor {
    config: EntitlementConfig,
    // In-memory store: entitlement_id -> Entitlement
    entitlements: Arc<parking_lot::RwLock<HashMap<String, Entitlement>>>,
}

impl EntitlementGovernor {
    /// Create a new governor with configuration
    pub fn new(config: EntitlementConfig) -> Self {
        Self {
            config,
            entitlements: Arc::new(parking_lot::RwLock::new(HashMap::new())),
        }
    }

    /// Create with default configuration
    pub fn new_default() -> Self {
        Self::new(EntitlementConfig::default())
    }

    /// Activate a new entitlement (SKU activation for customer)
    pub fn activate_entitlement(
        &self,
        sku_id: &str,
        customer_id: &str,
    ) -> Result<Entitlement, MarketplaceError> {
        if sku_id.is_empty() {
            return Err(MarketplaceError::InvalidInput(
                "sku_id cannot be empty".to_string(),
            ));
        }
        if customer_id.is_empty() {
            return Err(MarketplaceError::InvalidInput(
                "customer_id cannot be empty".to_string(),
            ));
        }

        let mut store = self.entitlements.write();
        let entitlement = Entitlement::new(customer_id.to_string(), sku_id.to_string());
        let result = entitlement.clone();
        store.insert(entitlement.id.clone(), entitlement);
        Ok(result)
    }

    /// Get current entitlement status
    pub fn check_status(&self, entitlement_id: &str) -> Result<(EntitlementState, DateTime<Utc>), MarketplaceError> {
        let store = self.entitlements.read();
        let ent = store.get(entitlement_id).ok_or(MarketplaceError::EntitlementNotFound {
            entitlement_id: entitlement_id.to_string(),
        })?;

        Ok((ent.state, ent.last_state_change))
    }

    /// Get receipt chain for audit trail
    pub fn get_receipt_chain(&self, entitlement_id: &str) -> Result<Vec<StateChangeReceipt>, MarketplaceError> {
        let store = self.entitlements.read();
        let ent = store.get(entitlement_id).ok_or(MarketplaceError::EntitlementNotFound {
            entitlement_id: entitlement_id.to_string(),
        })?;

        Ok(ent.receipt_chain.clone())
    }

    /// Process an event - transitions state according to FSM rules
    pub fn handle_event(
        &self,
        entitlement_id: &str,
        event: EntitlementEvent,
    ) -> Result<(EntitlementState, Option<String>), MarketplaceError> {
        let mut store = self.entitlements.write();
        let ent = store.get_mut(entitlement_id).ok_or(MarketplaceError::EntitlementNotFound {
            entitlement_id: entitlement_id.to_string(),
        })?;

        let from_state = ent.state;
        let (to_state, action, event_str, actor, details) = Self::process_event(from_state, event)?;

        // Perform state transition
        if from_state != to_state {
            ent.record_transition(to_state, event_str, actor, details, &self.config);
        }

        Ok((to_state, action))
    }

    /// Process timeout events (automatically triggered when timeout exceeded)
    pub fn check_and_process_timeouts(
        &self,
        entitlement_id: &str,
    ) -> Result<Option<(EntitlementState, String)>, MarketplaceError> {
        let mut store = self.entitlements.write();
        let ent = store.get_mut(entitlement_id).ok_or(MarketplaceError::EntitlementNotFound {
            entitlement_id: entitlement_id.to_string(),
        })?;

        // Check if timeout exceeded and handle it
        if ent.is_timeout_exceeded() {
            let timeout_event = match ent.state {
                EntitlementState::PendingApproval => EntitlementEvent::ApprovalTimeout,
                EntitlementState::Suspended => EntitlementEvent::SuspensionTimeout,
                EntitlementState::Cancelled => EntitlementEvent::RefundTimeout,
                EntitlementState::RefundIssued => EntitlementEvent::RefundTimeout,
                EntitlementState::ReinstallPending => EntitlementEvent::ReinstatementTimeout,
                EntitlementState::Expired => EntitlementEvent::ExpirationTimeout,
                _ => return Ok(None), // No timeout for Active, Archived
            };

            let from_state = ent.state;
            let (to_state, action, event_str, actor, details) = Self::process_event(from_state, timeout_event)?;

            if from_state != to_state {
                ent.record_transition(to_state, event_str, actor.clone(), details, &self.config);
                return Ok(Some((to_state, action.unwrap_or_else(|| "timeout processed".to_string()))));
            }
        }

        Ok(None)
    }

    /// State machine logic - determines valid transitions and actions
    fn process_event(
        from_state: EntitlementState,
        event: EntitlementEvent,
    ) -> Result<(EntitlementState, Option<String>, String, String, String), MarketplaceError> {
        let _event_str = format!("{:?}", event).to_string();

        match (from_state, event) {
            // pending_approval → active
            (EntitlementState::PendingApproval, EntitlementEvent::ApprovalGranted { actor }) => {
                Ok((
                    EntitlementState::Active,
                    Some("Begin charging, send welcome email".to_string()),
                    "approval_granted".to_string(),
                    actor,
                    "GCP Marketplace approved subscription".to_string(),
                ))
            }
            // pending_approval → cancelled
            (EntitlementState::PendingApproval, EntitlementEvent::ApprovalRejected { reason }) => {
                Ok((
                    EntitlementState::Cancelled,
                    Some("Send rejection notice, cleanup resources".to_string()),
                    "approval_rejected".to_string(),
                    "gcp-marketplace".to_string(),
                    format!("Approval rejected: {}", reason),
                ))
            }
            // pending_approval → cancelled (timeout)
            (EntitlementState::PendingApproval, EntitlementEvent::ApprovalTimeout) => {
                Ok((
                    EntitlementState::Cancelled,
                    Some("Auto-reject after 24h, notify customer".to_string()),
                    "approval_timeout".to_string(),
                    "system".to_string(),
                    "Auto-rejected after 24h approval window".to_string(),
                ))
            }
            // active → suspended
            (EntitlementState::Active, EntitlementEvent::PaymentFailed { reason }) => {
                Ok((
                    EntitlementState::Suspended,
                    Some("Block new operations, send payment reminder".to_string()),
                    "payment_failed".to_string(),
                    "system".to_string(),
                    format!("Payment failed: {}", reason),
                ))
            }
            (EntitlementState::Active, EntitlementEvent::SuspendedByAdmin { reason }) => {
                Ok((
                    EntitlementState::Suspended,
                    Some("Send admin suspension notice".to_string()),
                    "suspended_by_admin".to_string(),
                    "admin".to_string(),
                    format!("Admin suspension: {}", reason),
                ))
            }
            // active → expired
            (EntitlementState::Active, EntitlementEvent::SubscriptionExpired) => {
                Ok((
                    EntitlementState::Expired,
                    Some("Stop billing, preserve data, offer renewal".to_string()),
                    "subscription_expired".to_string(),
                    "system".to_string(),
                    "Subscription period ended".to_string(),
                ))
            }
            // active → cancelled
            (EntitlementState::Active, EntitlementEvent::CustomerCancels { reason }) => {
                Ok((
                    EntitlementState::Cancelled,
                    Some("Stop billing, initiate refund window".to_string()),
                    "customer_cancelled".to_string(),
                    "customer".to_string(),
                    format!("Customer cancellation: {}", reason),
                ))
            }
            // suspended → active (payment received)
            (EntitlementState::Suspended, EntitlementEvent::PaymentReceived) => {
                Ok((
                    EntitlementState::Active,
                    Some("Resume billing and operations".to_string()),
                    "payment_received".to_string(),
                    "system".to_string(),
                    "Payment processed, resuming service".to_string(),
                ))
            }
            // suspended → active (compliance fixed)
            (EntitlementState::Suspended, EntitlementEvent::ComplianceFixed) => {
                Ok((
                    EntitlementState::Active,
                    Some("Resume operations".to_string()),
                    "compliance_fixed".to_string(),
                    "admin".to_string(),
                    "Compliance issue resolved".to_string(),
                ))
            }
            // suspended → cancelled
            (EntitlementState::Suspended, EntitlementEvent::EscalateToCancellation { reason }) => {
                Ok((
                    EntitlementState::Cancelled,
                    Some("Cancel and initiate refund".to_string()),
                    "escalated_to_cancellation".to_string(),
                    "admin".to_string(),
                    format!("Escalated from suspension: {}", reason),
                ))
            }
            // suspended → cancelled (timeout)
            (EntitlementState::Suspended, EntitlementEvent::SuspensionTimeout) => {
                Ok((
                    EntitlementState::Cancelled,
                    Some("Auto-cancel after 72h, initiate refund".to_string()),
                    "suspension_timeout".to_string(),
                    "system".to_string(),
                    "Auto-cancelled after 72h suspension window".to_string(),
                ))
            }
            // expired → cancelled (renewal request)
            (EntitlementState::Expired, EntitlementEvent::CustomerCancels { reason }) => {
                Ok((
                    EntitlementState::Cancelled,
                    Some("Process refund request".to_string()),
                    "customer_cancelled".to_string(),
                    "customer".to_string(),
                    format!("Cancellation after expiration: {}", reason),
                ))
            }
            // expired → archived (timeout)
            (EntitlementState::Expired, EntitlementEvent::ExpirationTimeout) => {
                Ok((
                    EntitlementState::Archived,
                    Some("Archive data permanently".to_string()),
                    "expiration_timeout".to_string(),
                    "system".to_string(),
                    "Auto-archived after 30d expiration window".to_string(),
                ))
            }
            // cancelled → refund_issued
            (EntitlementState::Cancelled, EntitlementEvent::RefundApproved { amount }) => {
                Ok((
                    EntitlementState::RefundIssued,
                    Some(format!("Issue refund of ${:.2}", amount)),
                    "refund_approved".to_string(),
                    "admin".to_string(),
                    format!("Refund approved: ${:.2}", amount),
                ))
            }
            // refund_issued → archived (refund completed)
            (EntitlementState::RefundIssued, EntitlementEvent::RefundCompleted) => {
                Ok((
                    EntitlementState::Archived,
                    Some("Update ledger, cleanup".to_string()),
                    "refund_completed".to_string(),
                    "system".to_string(),
                    "Refund processing completed".to_string(),
                ))
            }
            // refund_issued → cancelled (refund failed)
            (EntitlementState::RefundIssued, EntitlementEvent::RefundFailed { reason }) => {
                Ok((
                    EntitlementState::Cancelled,
                    Some("Contact customer about refund failure".to_string()),
                    "refund_failed".to_string(),
                    "system".to_string(),
                    format!("Refund failed: {}", reason),
                ))
            }
            // refund_issued → refund_issued (dispute)
            (EntitlementState::RefundIssued, EntitlementEvent::RefundDispute) => {
                Ok((
                    EntitlementState::RefundIssued,
                    Some("Escalate dispute to specialist".to_string()),
                    "refund_dispute".to_string(),
                    "customer".to_string(),
                    "Refund dispute raised".to_string(),
                ))
            }
            // refund_issued → archived (timeout)
            (EntitlementState::RefundIssued, EntitlementEvent::RefundTimeout) => {
                Ok((
                    EntitlementState::Archived,
                    Some("Mark refund window as completed, archive".to_string()),
                    "refund_timeout".to_string(),
                    "system".to_string(),
                    "Auto-archived after 14d refund window".to_string(),
                ))
            }
            // cancelled → reinstate_pending
            (EntitlementState::Cancelled, EntitlementEvent::ReinstatementRequested) => {
                Ok((
                    EntitlementState::ReinstallPending,
                    Some("Send reinstatement request to admin".to_string()),
                    "reinstatement_requested".to_string(),
                    "customer".to_string(),
                    "Customer requested reinstatement".to_string(),
                ))
            }
            // reinstate_pending → active
            (EntitlementState::ReinstallPending, EntitlementEvent::ReinstatementApproved { actor }) => {
                Ok((
                    EntitlementState::Active,
                    Some("Restore entitlements, resume billing".to_string()),
                    "reinstatement_approved".to_string(),
                    actor,
                    "Reinstatement approved by admin".to_string(),
                ))
            }
            // reinstate_pending → cancelled
            (EntitlementState::ReinstallPending, EntitlementEvent::ReinstatementRejected { reason }) => {
                Ok((
                    EntitlementState::Cancelled,
                    Some("Notify customer of rejection".to_string()),
                    "reinstatement_rejected".to_string(),
                    "admin".to_string(),
                    format!("Reinstatement rejected: {}", reason),
                ))
            }
            // reinstate_pending → cancelled (timeout)
            (EntitlementState::ReinstallPending, EntitlementEvent::ReinstatementTimeout) => {
                Ok((
                    EntitlementState::Cancelled,
                    Some("Auto-reject reinstatement after 7d".to_string()),
                    "reinstatement_timeout".to_string(),
                    "system".to_string(),
                    "Auto-rejected after 7d reinstatement window".to_string(),
                ))
            }
            // Any state → archived
            (state, EntitlementEvent::Archive { reason }) => {
                if state != EntitlementState::Archived {
                    Ok((
                        EntitlementState::Archived,
                        Some("Archive all data permanently".to_string()),
                        "archived".to_string(),
                        "admin".to_string(),
                        format!("Manually archived: {}", reason),
                    ))
                } else {
                    Err(MarketplaceError::InvalidTransition {
                        from: from_state.as_str().to_string(),
                        to: "archived".to_string(),
                    })
                }
            }
            // Invalid transitions
            (from, _) => {
                Err(MarketplaceError::InvalidTransition {
                    from: from.as_str().to_string(),
                    to: "unknown".to_string(),
                })
            }
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    // Test 1: Create new entitlement in PendingApproval state
    #[test]
    fn test_activate_entitlement_initial_state() {
        // Arrange
        let gov = EntitlementGovernor::new_default();

        // Act
        let result = gov.activate_entitlement("starter-sku", "cust-001");

        // Assert
        assert!(result.is_ok());
        let ent = result.unwrap();
        assert_eq!(ent.state, EntitlementState::PendingApproval);
        assert_eq!(ent.customer_id, "cust-001");
        assert_eq!(ent.sku_id, "starter-sku");
    }

    // Test 2: Invalid inputs rejected
    #[test]
    fn test_activate_entitlement_empty_sku() {
        let gov = EntitlementGovernor::new_default();
        let result = gov.activate_entitlement("", "cust-001");
        assert!(matches!(result, Err(MarketplaceError::InvalidInput(_))));
    }

    #[test]
    fn test_activate_entitlement_empty_customer() {
        let gov = EntitlementGovernor::new_default();
        let result = gov.activate_entitlement("sku-001", "");
        assert!(matches!(result, Err(MarketplaceError::InvalidInput(_))));
    }

    // Test 3: PendingApproval → Active on approval
    #[test]
    fn test_transition_pending_to_active() {
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

        // Assert
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

    // Test 4: PendingApproval → Cancelled on rejection
    #[test]
    fn test_transition_pending_to_cancelled_on_rejection() {
        let gov = EntitlementGovernor::new_default();
        let ent = gov.activate_entitlement("pro-sku", "cust-003").unwrap();

        let result = gov.handle_event(
            &ent.id,
            EntitlementEvent::ApprovalRejected {
                reason: "Account flagged for review".to_string(),
            },
        );

        assert!(result.is_ok());
        let (state, _) = result.unwrap();
        assert_eq!(state, EntitlementState::Cancelled);
    }

    // Test 5: PendingApproval → Cancelled on timeout
    #[test]
    fn test_transition_pending_to_cancelled_on_timeout() {
        let config = EntitlementConfig {
            approval_timeout_hours: 1, // Set very short for testing
            ..Default::default()
        };
        let gov = EntitlementGovernor::new(config);
        let ent = gov.activate_entitlement("pro-sku", "cust-004").unwrap();

        // Manually check timeout (in real system, would be checked by scheduler)
        let result = gov.handle_event(&ent.id, EntitlementEvent::ApprovalTimeout);

        assert!(result.is_ok());
        let (state, _) = result.unwrap();
        assert_eq!(state, EntitlementState::Cancelled);
    }

    // Test 6: Active → Suspended on payment failure
    #[test]
    fn test_transition_active_to_suspended_payment_failed() {
        let gov = EntitlementGovernor::new_default();
        let ent = gov.activate_entitlement("pro-sku", "cust-005").unwrap();

        // Approve
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

    // Test 7: Suspended → Active on payment received
    #[test]
    fn test_transition_suspended_to_active_on_payment() {
        let gov = EntitlementGovernor::new_default();
        let ent = gov.activate_entitlement("pro-sku", "cust-006").unwrap();

        // Activate then suspend
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

        // Payment received
        let result = gov.handle_event(&ent.id, EntitlementEvent::PaymentReceived);

        assert!(result.is_ok());
        let (state, _) = result.unwrap();
        assert_eq!(state, EntitlementState::Active);
    }

    // Test 8: Active → Expired
    #[test]
    fn test_transition_active_to_expired() {
        let gov = EntitlementGovernor::new_default();
        let ent = gov.activate_entitlement("pro-sku", "cust-007").unwrap();

        gov.handle_event(
            &ent.id,
            EntitlementEvent::ApprovalGranted {
                actor: "admin".to_string(),
            },
        )
        .unwrap();

        let result = gov.handle_event(&ent.id, EntitlementEvent::SubscriptionExpired);

        assert!(result.is_ok());
        let (state, _) = result.unwrap();
        assert_eq!(state, EntitlementState::Expired);
    }

    // Test 9: Cancelled → RefundIssued
    #[test]
    fn test_transition_cancelled_to_refund_issued() {
        let gov = EntitlementGovernor::new_default();
        let ent = gov.activate_entitlement("pro-sku", "cust-008").unwrap();

        gov.handle_event(
            &ent.id,
            EntitlementEvent::ApprovalRejected {
                reason: "Test".to_string(),
            },
        )
        .unwrap();

        let result = gov.handle_event(
            &ent.id,
            EntitlementEvent::RefundApproved { amount: 99.99 },
        );

        assert!(result.is_ok());
        let (state, _) = result.unwrap();
        assert_eq!(state, EntitlementState::RefundIssued);
    }

    // Test 10: RefundIssued → Archived on completion
    #[test]
    fn test_transition_refund_issued_to_archived() {
        let gov = EntitlementGovernor::new_default();
        let ent = gov.activate_entitlement("pro-sku", "cust-009").unwrap();

        gov.handle_event(
            &ent.id,
            EntitlementEvent::ApprovalRejected {
                reason: "Test".to_string(),
            },
        )
        .unwrap();

        gov.handle_event(
            &ent.id,
            EntitlementEvent::RefundApproved { amount: 99.99 },
        )
        .unwrap();

        let result = gov.handle_event(&ent.id, EntitlementEvent::RefundCompleted);

        assert!(result.is_ok());
        let (state, _) = result.unwrap();
        assert_eq!(state, EntitlementState::Archived);
    }

    // Test 11: Cancelled → ReinstallPending → Active
    #[test]
    fn test_transition_reinstatement_flow() {
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

    // Test 12: Check status at each point
    #[test]
    fn test_check_status() {
        let gov = EntitlementGovernor::new_default();
        let ent = gov.activate_entitlement("pro-sku", "cust-011").unwrap();

        let (state, _) = gov.check_status(&ent.id).unwrap();
        assert_eq!(state, EntitlementState::PendingApproval);
    }

    // Test 13: Receipt chain records all transitions
    #[test]
    fn test_receipt_chain_records_transitions() {
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

        let receipts = gov.get_receipt_chain(&ent.id).unwrap();
        assert_eq!(receipts.len(), 2);
        assert_eq!(receipts[0].from_state, "pending_approval");
        assert_eq!(receipts[0].to_state, "active");
        assert_eq!(receipts[1].from_state, "active");
        assert_eq!(receipts[1].to_state, "suspended");
    }

    // Test 14: Non-existent entitlement returns error
    #[test]
    fn test_nonexistent_entitlement() {
        let gov = EntitlementGovernor::new_default();
        let result = gov.check_status("nonexistent-id");
        assert!(matches!(
            result,
            Err(MarketplaceError::EntitlementNotFound { .. })
        ));
    }

    // Test 15: Multi-tenant isolation
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

        // Verify separate states
        let (state1, _) = gov.check_status(&ent1.id).unwrap();
        let (state2, _) = gov.check_status(&ent2.id).unwrap();

        assert_eq!(state1, EntitlementState::Active);
        assert_eq!(state2, EntitlementState::Cancelled);
    }

    // Test 16: Suspended → Active via compliance fix
    #[test]
    fn test_suspend_resume_via_compliance_fix() {
        let gov = EntitlementGovernor::new_default();
        let ent = gov.activate_entitlement("pro-sku", "cust-016").unwrap();

        gov.handle_event(
            &ent.id,
            EntitlementEvent::ApprovalGranted {
                actor: "admin".to_string(),
            },
        )
        .unwrap();

        // Admin suspend
        gov.handle_event(
            &ent.id,
            EntitlementEvent::SuspendedByAdmin {
                reason: "Compliance review".to_string(),
            },
        )
        .unwrap();

        // Compliance fixed
        let result = gov.handle_event(&ent.id, EntitlementEvent::ComplianceFixed);
        assert!(result.is_ok());
        let (state, _) = result.unwrap();
        assert_eq!(state, EntitlementState::Active);
    }

    // Test 17: Archived is terminal
    #[test]
    fn test_archived_is_terminal() {
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

        // Try to transition from Archived - should fail
        let result = gov.handle_event(
            &ent.id,
            EntitlementEvent::ApprovalGranted {
                actor: "admin".to_string(),
            },
        );

        assert!(result.is_err());
    }
}
