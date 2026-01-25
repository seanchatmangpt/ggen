//! Subscription Lifecycle Governor (gen_statem inspired)
//!
//! This module implements a production-grade SaaS subscription state machine
//! following Erlang's gen_statem patterns. It manages the complete lifecycle of
//! customer subscriptions including trials, billing, renewals, upgrades/downgrades,
//! and churn recovery.
//!
//! ## State Machine Diagram
//!
//! ```
//! trial (14 days)
//!   ├─ trial_ending_soon (7 days before) → [send upgrade prompts]
//!   ├─ customer_purchases → active
//!   └─ auto_expire (14 days) → trial_ended
//!
//! trial_ended
//!   ├─ customer_purchases → active
//!   ├─ customer_declines → cancelled
//!   └─ auto_expire (3 days) → cancelled
//!
//! active
//!   ├─ renewal_date_approaching → awaiting_renewal
//!   ├─ customer_requests_upgrade → upgrade_approved → active_higher_tier
//!   ├─ customer_requests_downgrade → downgrade_approved → active_lower_tier
//!   ├─ customer_cancels → cancelled
//!   └─ payment_failed → awaiting_renewal
//!
//! awaiting_renewal
//!   ├─ renewal_payment_received → active
//!   ├─ renewal_payment_failed → renewal_grace
//!   └─ auto_expire (7 days) → renewal_grace
//!
//! renewal_grace
//!   ├─ renewal_payment_received → active
//!   ├─ customer_fixes_payment → active
//!   ├─ grace_period_expires → cancelled
//!   └─ customer_cancels → cancelled
//!
//! cancelled
//!   ├─ customer_reactivates (within 30 days) → active
//!   └─ data_retention_expires (30 days) → lapsed
//!
//! lapsed
//!   ├─ customer_wants_restore → archived
//!   └─ auto_expire (90 days) → archived
//!
//! archived (terminal - read-only)
//! ```

use thiserror::Error;
use serde::{Deserialize, Serialize};
use chrono::{DateTime, Utc, Duration};
use std::collections::{HashMap, VecDeque};

// ============================================================================
// Errors
// ============================================================================

/// Subscription governor errors
#[derive(Debug, Error)]
pub enum SubscriptionError {
    #[error("Invalid state transition: {from} → {to} with event {event}")]
    InvalidTransition {
        from: String,
        to: String,
        event: String,
    },

    #[error("Invariant violation: {0}")]
    InvariantViolation(String),

    #[error("Insufficient balance for upgrade: need ${required}, have ${current}")]
    InsufficientBalance { required: i32, current: i32 },

    #[error("Feature incompatibility: {feature} not available in {tier}")]
    FeatureNotAvailable { feature: String, tier: String },

    #[error("Proration calculation failed: {0}")]
    ProrableCalculationFailed(String),

    #[error("Payment processing failed: {0}")]
    PaymentFailed(String),

    #[error("Subscription not found: {id}")]
    SubscriptionNotFound { id: String },

    #[error("Cannot process upgrade: {0}")]
    UpgradeError(String),

    #[error("Cannot process downgrade: {0}")]
    DowngradeError(String),

    #[error("Reactivation window expired (max 30 days after cancellation)")]
    ReactivationWindowExpired,

    #[error("Invalid contract term: {0}")]
    InvalidContractTerm(String),

    #[error("Invalid account type: {0}")]
    InvalidAccountType(String),

    #[error("Usage tracking error: {0}")]
    UsageTrackingError(String),
}

// ============================================================================
// Core Types
// ============================================================================

/// Subscription lifecycle states
#[derive(Debug, Clone, Copy, Serialize, Deserialize, Eq, PartialEq, Hash)]
pub enum SubscriptionState {
    /// Free trial period (14 days)
    Trial,
    /// Trial expired, awaiting purchase decision
    TrialEnded,
    /// Active paid subscription
    Active,
    /// Awaiting renewal payment (7 days before expiration)
    AwaitingRenewal,
    /// Payment failed, grace period active (7 days)
    RenewalGrace,
    /// Customer cancelled
    Cancelled,
    /// Data retention period expired
    Lapsed,
    /// Terminal state, read-only
    Archived,
}

impl SubscriptionState {
    fn as_str(&self) -> &str {
        match self {
            SubscriptionState::Trial => "Trial",
            SubscriptionState::TrialEnded => "TrialEnded",
            SubscriptionState::Active => "Active",
            SubscriptionState::AwaitingRenewal => "AwaitingRenewal",
            SubscriptionState::RenewalGrace => "RenewalGrace",
            SubscriptionState::Cancelled => "Cancelled",
            SubscriptionState::Lapsed => "Lapsed",
            SubscriptionState::Archived => "Archived",
        }
    }
}

/// Billing cycle types
#[derive(Debug, Clone, Copy, Serialize, Deserialize, Eq, PartialEq)]
pub enum BillingCycle {
    Monthly,
    Annual,
    TwoYear,
    ThreeYear,
}

impl BillingCycle {
    /// Get days in billing cycle
    pub fn days(&self) -> u32 {
        match self {
            BillingCycle::Monthly => 30,
            BillingCycle::Annual => 365,
            BillingCycle::TwoYear => 730,
            BillingCycle::ThreeYear => 1095,
        }
    }

    /// Get discount percentage
    pub fn discount_percent(&self) -> f64 {
        match self {
            BillingCycle::Monthly => 0.0,
            BillingCycle::Annual => 0.10,
            BillingCycle::TwoYear => 0.20,
            BillingCycle::ThreeYear => 0.30,
        }
    }
}

/// Feature tier enumeration
#[derive(Debug, Clone, Serialize, Deserialize, Eq, PartialEq, Hash)]
pub enum FeatureTier {
    Free,
    Starter,
    Professional,
    Enterprise,
}

impl FeatureTier {
    fn as_str(&self) -> &str {
        match self {
            FeatureTier::Free => "Free",
            FeatureTier::Starter => "Starter",
            FeatureTier::Professional => "Professional",
            FeatureTier::Enterprise => "Enterprise",
        }
    }

    /// Get monthly base price in cents
    pub fn monthly_price_cents(&self) -> i32 {
        match self {
            FeatureTier::Free => 0,
            FeatureTier::Starter => 2999,      // $29.99
            FeatureTier::Professional => 9999, // $99.99
            FeatureTier::Enterprise => 0,      // Custom pricing
        }
    }

    /// Check if feature available in this tier
    pub fn has_feature(&self, feature: &str) -> bool {
        match self {
            FeatureTier::Free => matches!(
                feature,
                "basic_api" | "documentation" | "community_support"
            ),
            FeatureTier::Starter => matches!(
                feature,
                "basic_api"
                    | "documentation"
                    | "email_support"
                    | "advanced_analytics"
                    | "custom_branding"
            ),
            FeatureTier::Professional => matches!(
                feature,
                "basic_api"
                    | "documentation"
                    | "priority_support"
                    | "advanced_analytics"
                    | "custom_branding"
                    | "api_access"
                    | "webhooks"
                    | "sso"
            ),
            FeatureTier::Enterprise => true, // All features
        }
    }

    /// Get feature limits for this tier
    pub fn get_limits(&self, feature: &str) -> Option<u32> {
        match (self, feature) {
            (FeatureTier::Free, "api_calls_per_month") => Some(10000),
            (FeatureTier::Starter, "api_calls_per_month") => Some(1000000),
            (FeatureTier::Professional, "api_calls_per_month") => Some(10000000),

            (FeatureTier::Free, "team_members") => Some(1),
            (FeatureTier::Starter, "team_members") => Some(5),
            (FeatureTier::Professional, "team_members") => Some(50),

            (FeatureTier::Free, "data_retention_days") => Some(30),
            (FeatureTier::Starter, "data_retention_days") => Some(90),
            (FeatureTier::Professional, "data_retention_days") => Some(365),

            (FeatureTier::Enterprise, _) => Some(u32::MAX),

            _ => None,
        }
    }
}

/// Account type (individual or organization)
#[derive(Debug, Clone, Serialize, Deserialize, Eq, PartialEq)]
pub enum AccountType {
    Individual,
    Family { members: u32 },
    Team { seats: u32 },
}

/// Usage tracking for soft/hard limits
#[derive(Debug, Clone, Default, Serialize, Deserialize)]
pub struct UsageMetrics {
    pub api_calls_this_month: u32,
    pub team_members_added: u32,
    pub data_stored_gb: u32,
}

impl UsageMetrics {
    pub fn exceeds_limit(&self, tier: &FeatureTier, metric: &str) -> bool {
        match metric {
            "api_calls" => {
                if let Some(limit) = tier.get_limits("api_calls_per_month") {
                    self.api_calls_this_month > limit
                } else {
                    false
                }
            }
            "team_members" => {
                if let Some(limit) = tier.get_limits("team_members") {
                    self.team_members_added > limit
                } else {
                    false
                }
            }
            _ => false,
        }
    }
}

/// Proration details for mid-cycle changes
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct ProratedCharge {
    pub original_price_cents: i32,
    pub new_price_cents: i32,
    pub days_elapsed: u32,
    pub days_remaining: u32,
    pub proration_amount_cents: i32,
    pub refund_amount_cents: i32,
}

/// Events that drive FSM transitions
#[derive(Debug, Clone, Serialize, Deserialize)]
pub enum SubscriptionEvent {
    /// Trial started
    TrialStarted,
    /// Trial ending soon (7 days before)
    TrialEndingSoon,
    /// Customer purchases subscription after trial
    CustomerPurchases { tier: FeatureTier, cycle: BillingCycle },
    /// Customer declines purchase
    CustomerDeclines,
    /// Auto-expiration triggers
    AutoTrialExpiration,
    /// Renewal date approaching
    RenewalDateApproaching,
    /// Payment received during renewal
    RenewalPaymentReceived { amount_cents: i32 },
    /// Payment failed during renewal
    RenewalPaymentFailed { reason: String },
    /// Customer fixes failed payment
    CustomerFixesPayment { amount_cents: i32 },
    /// Grace period expires (payment not fixed)
    GracePeriodExpires,
    /// Customer requests upgrade
    CustomerRequestsUpgrade { new_tier: FeatureTier },
    /// Customer requests downgrade
    CustomerRequestsDowngrade { new_tier: FeatureTier },
    /// Upgrade approved
    UpgradeApproved,
    /// Downgrade approved
    DowngradeApproved,
    /// Customer cancels subscription
    CustomerCancels { reason: String },
    /// Customer requests reactivation (within 30 days)
    CustomerRequestsReactivation,
    /// Data retention period expires
    DataRetentionExpires,
    /// Archive for compliance
    ArchiveForCompliance,
    /// Manual reset (operator intervention)
    ManualReset,
}

impl SubscriptionEvent {
    fn as_str(&self) -> &str {
        match self {
            SubscriptionEvent::TrialStarted => "TrialStarted",
            SubscriptionEvent::TrialEndingSoon => "TrialEndingSoon",
            SubscriptionEvent::CustomerPurchases { .. } => "CustomerPurchases",
            SubscriptionEvent::CustomerDeclines => "CustomerDeclines",
            SubscriptionEvent::AutoTrialExpiration => "AutoTrialExpiration",
            SubscriptionEvent::RenewalDateApproaching => "RenewalDateApproaching",
            SubscriptionEvent::RenewalPaymentReceived { .. } => "RenewalPaymentReceived",
            SubscriptionEvent::RenewalPaymentFailed { .. } => "RenewalPaymentFailed",
            SubscriptionEvent::CustomerFixesPayment { .. } => "CustomerFixesPayment",
            SubscriptionEvent::GracePeriodExpires => "GracePeriodExpires",
            SubscriptionEvent::CustomerRequestsUpgrade { .. } => "CustomerRequestsUpgrade",
            SubscriptionEvent::CustomerRequestsDowngrade { .. } => "CustomerRequestsDowngrade",
            SubscriptionEvent::UpgradeApproved => "UpgradeApproved",
            SubscriptionEvent::DowngradeApproved => "DowngradeApproved",
            SubscriptionEvent::CustomerCancels { .. } => "CustomerCancels",
            SubscriptionEvent::CustomerRequestsReactivation => "CustomerRequestsReactivation",
            SubscriptionEvent::DataRetentionExpires => "DataRetentionExpires",
            SubscriptionEvent::ArchiveForCompliance => "ArchiveForCompliance",
            SubscriptionEvent::ManualReset => "ManualReset",
        }
    }
}

/// Subscription record
#[derive(Debug, Clone)]
pub struct Subscription {
    pub id: String,
    pub customer_id: String,
    pub state: SubscriptionState,
    pub current_tier: FeatureTier,
    pub billing_cycle: BillingCycle,
    pub account_type: AccountType,
    pub created_at: DateTime<Utc>,
    pub trial_ends_at: Option<DateTime<Utc>>,
    pub current_period_start: DateTime<Utc>,
    pub current_period_end: DateTime<Utc>,
    pub renewal_reminder_sent: bool,
    pub last_payment_attempt: Option<DateTime<Utc>>,
    pub last_state_change: DateTime<Utc>,
    pub cancellation_reason: Option<String>,
    pub cancelled_at: Option<DateTime<Utc>>,
    pub usage_metrics: UsageMetrics,
    pub pending_upgrade: Option<FeatureTier>,
    pub churn_discount_offered: bool,
    pub reactivation_deadline: Option<DateTime<Utc>>,
}

impl Subscription {
    /// Create new trial subscription
    pub fn new_trial(customer_id: String, account_type: AccountType) -> Self {
        let now = Utc::now();
        let trial_ends = now + Duration::days(14);

        Self {
            id: uuid::Uuid::new_v4().to_string(),
            customer_id,
            state: SubscriptionState::Trial,
            current_tier: FeatureTier::Free,
            billing_cycle: BillingCycle::Monthly,
            account_type,
            created_at: now,
            trial_ends_at: Some(trial_ends),
            current_period_start: now,
            current_period_end: trial_ends,
            renewal_reminder_sent: false,
            last_payment_attempt: None,
            last_state_change: now,
            cancellation_reason: None,
            cancelled_at: None,
            usage_metrics: UsageMetrics::default(),
            pending_upgrade: None,
            churn_discount_offered: false,
            reactivation_deadline: None,
        }
    }

    /// Days remaining in current period
    pub fn days_remaining(&self) -> i64 {
        (self.current_period_end - Utc::now()).num_days()
    }

    /// Days elapsed in current period
    pub fn days_elapsed(&self) -> i64 {
        (Utc::now() - self.current_period_start).num_days()
    }

    /// Check if subscription is trial expiring soon (7 days)
    pub fn is_trial_expiring_soon(&self) -> bool {
        if let Some(trial_ends) = self.trial_ends_at {
            let days_until_expiry = (trial_ends - Utc::now()).num_days();
            days_until_expiry <= 7 && days_until_expiry > 0
        } else {
            false
        }
    }

    /// Check if trial is expired
    pub fn is_trial_expired(&self) -> bool {
        if let Some(trial_ends) = self.trial_ends_at {
            Utc::now() >= trial_ends
        } else {
            false
        }
    }

    /// Check if renewal is approaching (7 days)
    pub fn is_renewal_approaching(&self) -> bool {
        let days_until_renewal = (self.current_period_end - Utc::now()).num_days();
        days_until_renewal <= 7 && days_until_renewal > 0
    }

    /// Check if reactivation window is still open
    pub fn can_reactivate(&self) -> bool {
        if let Some(deadline) = self.reactivation_deadline {
            Utc::now() <= deadline
        } else {
            false
        }
    }
}

/// Audit trail entry
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct AuditEntry {
    pub id: String,
    pub subscription_id: String,
    pub timestamp: DateTime<Utc>,
    pub event: String,
    pub from_state: String,
    pub to_state: String,
    pub details: String,
}

// ============================================================================
// Governor Implementation
// ============================================================================

/// Subscription Lifecycle Governor
pub struct SubscriptionGovernor {
    subscriptions: HashMap<String, Subscription>,
    audit_trail: VecDeque<AuditEntry>,
    payment_queue: VecDeque<(String, i32)>, // (subscription_id, amount_cents)
}

impl SubscriptionGovernor {
    /// Create new governor
    pub fn new() -> Self {
        Self {
            subscriptions: HashMap::new(),
            audit_trail: VecDeque::with_capacity(10000),
            payment_queue: VecDeque::new(),
        }
    }

    /// Create new trial subscription
    pub async fn create_trial(
        &mut self,
        customer_id: String,
        account_type: AccountType,
    ) -> Result<Subscription, SubscriptionError> {
        let subscription = Subscription::new_trial(customer_id, account_type);
        let id = subscription.id.clone();

        self.audit_event(
            id.clone(),
            "Trial created".to_string(),
            "None".to_string(),
            SubscriptionState::Trial.as_str().to_string(),
        );

        self.subscriptions.insert(id, subscription.clone());
        Ok(subscription)
    }

    /// Process event and compute state transition
    pub async fn transition(
        &mut self,
        subscription_id: &str,
        event: SubscriptionEvent,
    ) -> Result<(SubscriptionState, Option<String>), SubscriptionError> {
        let subscription = self
            .subscriptions
            .get_mut(subscription_id)
            .ok_or(SubscriptionError::SubscriptionNotFound {
                id: subscription_id.to_string(),
            })?;

        let (new_state, action_msg) = match (&subscription.state, &event) {
            // === TRIAL state ===
            (SubscriptionState::Trial, SubscriptionEvent::TrialEndingSoon) => {
                subscription.renewal_reminder_sent = true;
                (
                    SubscriptionState::Trial,
                    Some("Send upgrade prompts to customer".to_string()),
                )
            }

            (SubscriptionState::Trial, SubscriptionEvent::CustomerPurchases { tier, cycle }) => {
                subscription.current_tier = tier.clone();
                subscription.billing_cycle = *cycle;
                let now = Utc::now();
                let period_days = cycle.days() as i64;
                subscription.current_period_start = now;
                subscription.current_period_end = now + Duration::days(period_days);
                subscription.trial_ends_at = None;

                let charge = tier.monthly_price_cents() as f64
                    * (1.0 - cycle.discount_percent())
                    * (period_days as f64 / 30.0);

                self.payment_queue
                    .push_back((subscription_id.to_string(), charge as i32));

                (
                    SubscriptionState::Active,
                    Some(format!("Charge customer {} cents", charge as i32)),
                )
            }

            (SubscriptionState::Trial, SubscriptionEvent::CustomerDeclines) => {
                subscription.cancellation_reason = Some("Declined trial conversion".to_string());
                (
                    SubscriptionState::TrialEnded,
                    Some("Disable all features".to_string()),
                )
            }

            (SubscriptionState::Trial, SubscriptionEvent::AutoTrialExpiration) => {
                (
                    SubscriptionState::TrialEnded,
                    Some("Trial auto-expired, offer upgrade incentives".to_string()),
                )
            }

            // === TRIAL_ENDED state ===
            (SubscriptionState::TrialEnded, SubscriptionEvent::CustomerPurchases { tier, cycle }) => {
                subscription.current_tier = tier.clone();
                subscription.billing_cycle = *cycle;
                let now = Utc::now();
                let period_days = cycle.days() as i64;
                subscription.current_period_start = now;
                subscription.current_period_end = now + Duration::days(period_days);

                let charge = tier.monthly_price_cents() as f64
                    * (1.0 - cycle.discount_percent())
                    * (period_days as f64 / 30.0);

                self.payment_queue
                    .push_back((subscription_id.to_string(), charge as i32));

                (
                    SubscriptionState::Active,
                    Some(format!("Charge customer {} cents", charge as i32)),
                )
            }

            (SubscriptionState::TrialEnded, SubscriptionEvent::CustomerDeclines) => {
                subscription.cancellation_reason = Some("Declined conversion after trial".to_string());
                subscription.cancelled_at = Some(Utc::now());
                subscription.reactivation_deadline = Some(Utc::now() + Duration::days(30));
                (
                    SubscriptionState::Cancelled,
                    Some("Stop billing, preserve customer data (30 days)".to_string()),
                )
            }

            (SubscriptionState::TrialEnded, SubscriptionEvent::AutoTrialExpiration) => {
                subscription.cancellation_reason = Some("Trial auto-expired (3 day window)".to_string());
                subscription.cancelled_at = Some(Utc::now());
                subscription.reactivation_deadline = Some(Utc::now() + Duration::days(30));
                (
                    SubscriptionState::Cancelled,
                    Some("Cancel subscription automatically".to_string()),
                )
            }

            // === ACTIVE state ===
            (SubscriptionState::Active, SubscriptionEvent::RenewalDateApproaching) => {
                subscription.renewal_reminder_sent = true;
                (
                    SubscriptionState::AwaitingRenewal,
                    Some("Send renewal reminder email".to_string()),
                )
            }

            (SubscriptionState::Active, SubscriptionEvent::CustomerRequestsUpgrade { new_tier }) => {
                let current_tier = subscription.current_tier.clone();
                Self::validate_tier_upgrade(&current_tier, new_tier)?;
                subscription.pending_upgrade = Some(new_tier.clone());
                (
                    SubscriptionState::Active,
                    Some(format!("Process upgrade to {}", new_tier.as_str())),
                )
            }

            (SubscriptionState::Active, SubscriptionEvent::UpgradeApproved) => {
                if let Some(new_tier) = &subscription.pending_upgrade {
                    let proration = Self::calculate_proration(
                        &subscription.current_tier,
                        new_tier,
                        subscription.billing_cycle,
                        subscription.days_elapsed() as u32,
                        subscription.days_remaining() as u32,
                    )?;

                    self.payment_queue.push_back((
                        subscription_id.to_string(),
                        proration.proration_amount_cents,
                    ));

                    subscription.current_tier = new_tier.clone();
                    subscription.pending_upgrade = None;
                } else {
                    return Err(SubscriptionError::UpgradeError(
                        "No pending upgrade".to_string(),
                    ));
                }

                (
                    SubscriptionState::Active,
                    Some("Upgrade applied and confirmed to customer".to_string()),
                )
            }

            (SubscriptionState::Active, SubscriptionEvent::CustomerRequestsDowngrade { new_tier }) => {
                let current_tier = subscription.current_tier.clone();
                Self::validate_tier_downgrade(&current_tier, new_tier)?;
                subscription.pending_upgrade = Some(new_tier.clone());
                (
                    SubscriptionState::Active,
                    Some(format!("Process downgrade to {}", new_tier.as_str())),
                )
            }

            (SubscriptionState::Active, SubscriptionEvent::DowngradeApproved) => {
                if let Some(new_tier) = &subscription.pending_upgrade {
                    let proration = Self::calculate_proration(
                        &subscription.current_tier,
                        new_tier,
                        subscription.billing_cycle,
                        subscription.days_elapsed() as u32,
                        subscription.days_remaining() as u32,
                    )?;

                    if proration.refund_amount_cents > 0 {
                        // Queue refund (negative charge)
                        self.payment_queue.push_back((
                            subscription_id.to_string(),
                            -proration.refund_amount_cents,
                        ));
                    }

                    subscription.current_tier = new_tier.clone();
                    subscription.pending_upgrade = None;
                } else {
                    return Err(SubscriptionError::DowngradeError(
                        "No pending downgrade".to_string(),
                    ));
                }

                (
                    SubscriptionState::Active,
                    Some("Downgrade applied and refund issued".to_string()),
                )
            }

            (SubscriptionState::Active, SubscriptionEvent::CustomerCancels { reason }) => {
                subscription.cancellation_reason = Some(reason.clone());
                subscription.cancelled_at = Some(Utc::now());
                subscription.reactivation_deadline = Some(Utc::now() + Duration::days(30));
                subscription.churn_discount_offered = false;
                (
                    SubscriptionState::Cancelled,
                    Some("Stop billing, preserve data (30 days), offer churn discount".to_string()),
                )
            }

            // === AWAITING_RENEWAL state ===
            (SubscriptionState::AwaitingRenewal, SubscriptionEvent::RenewalPaymentReceived { amount_cents }) => {
                subscription.last_payment_attempt = Some(Utc::now());
                let period_days = subscription.billing_cycle.days() as i64;
                let now = Utc::now();
                subscription.current_period_start = now;
                subscription.current_period_end = now + Duration::days(period_days);

                (
                    SubscriptionState::Active,
                    Some(format!("Renewal successful, charged {} cents", amount_cents)),
                )
            }

            (SubscriptionState::AwaitingRenewal, SubscriptionEvent::RenewalPaymentFailed { reason }) => {
                subscription.last_payment_attempt = Some(Utc::now());
                (
                    SubscriptionState::RenewalGrace,
                    Some(format!("Payment failed: {}, send urgent reminder", reason)),
                )
            }

            (SubscriptionState::AwaitingRenewal, SubscriptionEvent::GracePeriodExpires) => {
                (
                    SubscriptionState::RenewalGrace,
                    Some("Auto-transition to grace period".to_string()),
                )
            }

            // === RENEWAL_GRACE state ===
            (SubscriptionState::RenewalGrace, SubscriptionEvent::CustomerFixesPayment { amount_cents }) => {
                subscription.last_payment_attempt = Some(Utc::now());
                let now = Utc::now();
                let period_days = subscription.billing_cycle.days() as i64;
                subscription.current_period_start = now;
                subscription.current_period_end = now + Duration::days(period_days);

                self.payment_queue
                    .push_back((subscription_id.to_string(), *amount_cents));

                (
                    SubscriptionState::Active,
                    Some(format!("Payment received during grace period: {} cents", amount_cents)),
                )
            }

            (SubscriptionState::RenewalGrace, SubscriptionEvent::GracePeriodExpires) => {
                subscription.cancellation_reason = Some("Payment failed during grace period".to_string());
                subscription.cancelled_at = Some(Utc::now());
                subscription.reactivation_deadline = Some(Utc::now() + Duration::days(30));
                (
                    SubscriptionState::Cancelled,
                    Some("Grace period expired, cancel subscription".to_string()),
                )
            }

            (SubscriptionState::RenewalGrace, SubscriptionEvent::CustomerCancels { reason }) => {
                subscription.cancellation_reason = Some(reason.clone());
                subscription.cancelled_at = Some(Utc::now());
                subscription.reactivation_deadline = Some(Utc::now() + Duration::days(30));
                (
                    SubscriptionState::Cancelled,
                    Some("Customer cancelled during grace period".to_string()),
                )
            }

            // === CANCELLED state ===
            (SubscriptionState::Cancelled, SubscriptionEvent::CustomerRequestsReactivation) => {
                if !subscription.can_reactivate() {
                    return Err(SubscriptionError::ReactivationWindowExpired);
                }

                let now = Utc::now();
                let period_days = subscription.billing_cycle.days() as i64;
                subscription.current_period_start = now;
                subscription.current_period_end = now + Duration::days(period_days);
                subscription.reactivation_deadline = None;
                subscription.cancellation_reason = None;

                let charge = subscription.current_tier.monthly_price_cents() as f64
                    * (1.0 - subscription.billing_cycle.discount_percent())
                    * (period_days as f64 / 30.0);

                self.payment_queue
                    .push_back((subscription_id.to_string(), charge as i32));

                (
                    SubscriptionState::Active,
                    Some(format!("Reactivation successful, charge {} cents", charge as i32)),
                )
            }

            (SubscriptionState::Cancelled, SubscriptionEvent::DataRetentionExpires) => {
                (
                    SubscriptionState::Lapsed,
                    Some("Archive data for compliance, prepare for deletion".to_string()),
                )
            }

            // === LAPSED state ===
            (SubscriptionState::Lapsed, SubscriptionEvent::ArchiveForCompliance) => {
                (
                    SubscriptionState::Archived,
                    Some("Final archive, historical data preserved".to_string()),
                )
            }

            // === ARCHIVED state (terminal) ===
            (SubscriptionState::Archived, SubscriptionEvent::ManualReset) => {
                // Only allow manual reset from archived
                subscription.cancelled_at = None;
                subscription.reactivation_deadline = None;
                subscription.cancellation_reason = None;

                (
                    SubscriptionState::Active,
                    Some("Manual reset from archived state".to_string()),
                )
            }

            // Default: invalid transition
            (current, event) => {
                return Err(SubscriptionError::InvalidTransition {
                    from: current.as_str().to_string(),
                    to: "?".to_string(),
                    event: event.as_str().to_string(),
                })
            }
        };

        let old_state = subscription.state;
        subscription.state = new_state;
        subscription.last_state_change = Utc::now();

        // Log transition to audit trail
        if old_state != new_state {
            self.audit_event(
                subscription_id.to_string(),
                event.as_str().to_string(),
                old_state.as_str().to_string(),
                new_state.as_str().to_string(),
            );

            tracing::info!(
                subscription_id = %subscription_id,
                from = %old_state.as_str(),
                to = %new_state.as_str(),
                event = %event.as_str(),
                "Subscription state transition"
            );
        }

        Ok((new_state, action_msg))
    }

    /// Calculate proration for mid-cycle changes
    fn calculate_proration(
        old_tier: &FeatureTier,
        new_tier: &FeatureTier,
        billing_cycle: BillingCycle,
        days_elapsed: u32,
        days_remaining: u32,
    ) -> Result<ProratedCharge, SubscriptionError> {
        let total_days = days_elapsed + days_remaining;
        if total_days == 0 {
            return Err(SubscriptionError::ProrableCalculationFailed(
                "Invalid billing period".to_string(),
            ));
        }

        let old_price = old_tier.monthly_price_cents() as f64
            * (1.0 - billing_cycle.discount_percent());
        let new_price = new_tier.monthly_price_cents() as f64
            * (1.0 - billing_cycle.discount_percent());

        let daily_old = old_price / 30.0;
        let daily_new = new_price / 30.0;

        let _used_amount = daily_old * days_elapsed as f64; // Used for future analytics
        let remaining_old = daily_old * days_remaining as f64;
        let remaining_new = daily_new * days_remaining as f64;

        let proration_amount = (remaining_new - remaining_old) as i32;

        Ok(ProratedCharge {
            original_price_cents: old_tier.monthly_price_cents(),
            new_price_cents: new_tier.monthly_price_cents(),
            days_elapsed,
            days_remaining,
            proration_amount_cents: proration_amount,
            refund_amount_cents: if proration_amount < 0 {
                -proration_amount
            } else {
                0
            },
        })
    }

    /// Validate upgrade is allowed
    fn validate_tier_upgrade(
        current_tier: &FeatureTier,
        new_tier: &FeatureTier,
    ) -> Result<(), SubscriptionError> {
        let current_rank = Self::tier_rank(current_tier);
        let new_rank = Self::tier_rank(new_tier);

        if new_rank <= current_rank {
            return Err(SubscriptionError::UpgradeError(
                "Target tier is not higher than current tier".to_string(),
            ));
        }

        Ok(())
    }

    /// Validate downgrade is allowed
    fn validate_tier_downgrade(
        current_tier: &FeatureTier,
        new_tier: &FeatureTier,
    ) -> Result<(), SubscriptionError> {
        let current_rank = Self::tier_rank(current_tier);
        let new_rank = Self::tier_rank(new_tier);

        if new_rank >= current_rank {
            return Err(SubscriptionError::DowngradeError(
                "Target tier is not lower than current tier".to_string(),
            ));
        }

        Ok(())
    }

    /// Get tier ranking (0=free, 3=enterprise)
    fn tier_rank(tier: &FeatureTier) -> u32 {
        match tier {
            FeatureTier::Free => 0,
            FeatureTier::Starter => 1,
            FeatureTier::Professional => 2,
            FeatureTier::Enterprise => 3,
        }
    }

    /// Get subscription by ID
    pub fn get_subscription(&self, subscription_id: &str) -> Option<Subscription> {
        self.subscriptions.get(subscription_id).cloned()
    }

    /// Record audit event
    fn audit_event(
        &mut self,
        subscription_id: String,
        event: String,
        from_state: String,
        to_state: String,
    ) {
        let entry = AuditEntry {
            id: uuid::Uuid::new_v4().to_string(),
            subscription_id,
            timestamp: Utc::now(),
            event,
            from_state,
            to_state,
            details: String::new(),
        };

        self.audit_trail.push_back(entry);

        // Keep audit trail bounded
        if self.audit_trail.len() > 10000 {
            self.audit_trail.pop_front();
        }
    }

    /// Get audit trail for subscription
    pub fn get_audit_trail(&self, subscription_id: &str) -> Vec<AuditEntry> {
        self.audit_trail
            .iter()
            .filter(|e| e.subscription_id == subscription_id)
            .cloned()
            .collect()
    }

    /// Process pending payments
    pub async fn process_payments(&mut self) -> Result<Vec<(String, i32, bool)>, SubscriptionError> {
        let mut results = Vec::new();

        while let Some((sub_id, amount)) = self.payment_queue.pop_front() {
            let success = self.process_payment(&sub_id, amount).await.is_ok();
            results.push((sub_id, amount, success));
        }

        Ok(results)
    }

    /// Process single payment
    async fn process_payment(
        &self,
        _subscription_id: &str,
        _amount_cents: i32,
    ) -> Result<(), SubscriptionError> {
        // In production, this would call payment processor (Stripe, etc.)
        // For now, simulate success
        Ok(())
    }
}

// ============================================================================
// Tests (Chicago TDD - AAA Pattern)
// ============================================================================

#[cfg(test)]
mod tests {
    use super::*;

    #[tokio::test]
    async fn test_trial_to_active_purchase() {
        // Arrange
        let mut governor = SubscriptionGovernor::new();
        let customer_id = "cust-001".to_string();
        let account_type = AccountType::Individual;

        let subscription = governor
            .create_trial(customer_id, account_type)
            .await
            .unwrap();

        assert_eq!(subscription.state, SubscriptionState::Trial);
        assert_eq!(subscription.current_tier, FeatureTier::Free);

        // Act
        let (new_state, action) = governor
            .transition(
                &subscription.id,
                SubscriptionEvent::CustomerPurchases {
                    tier: FeatureTier::Starter,
                    cycle: BillingCycle::Monthly,
                },
            )
            .await
            .unwrap();

        // Assert
        assert_eq!(new_state, SubscriptionState::Active);
        assert!(action.is_some());
        assert!(action.unwrap().contains("Charge customer"));

        let updated = governor.get_subscription(&subscription.id).unwrap();
        assert_eq!(updated.current_tier, FeatureTier::Starter);
        assert_eq!(updated.state, SubscriptionState::Active);
    }

    #[tokio::test]
    async fn test_trial_expiration_auto_transition() {
        // Arrange
        let mut governor = SubscriptionGovernor::new();
        let subscription = governor
            .create_trial("cust-002".to_string(), AccountType::Individual)
            .await
            .unwrap();

        // Act
        let (new_state, action) = governor
            .transition(&subscription.id, SubscriptionEvent::AutoTrialExpiration)
            .await
            .unwrap();

        // Assert
        assert_eq!(new_state, SubscriptionState::TrialEnded);
        assert!(action.is_some());

        let updated = governor.get_subscription(&subscription.id).unwrap();
        assert!(updated.cancellation_reason.is_some());
    }

    #[tokio::test]
    async fn test_active_to_renewal_flow() {
        // Arrange
        let mut governor = SubscriptionGovernor::new();
        let mut subscription = governor
            .create_trial("cust-003".to_string(), AccountType::Individual)
            .await
            .unwrap();

        // Purchase to go active
        governor
            .transition(
                &subscription.id,
                SubscriptionEvent::CustomerPurchases {
                    tier: FeatureTier::Starter,
                    cycle: BillingCycle::Monthly,
                },
            )
            .await
            .unwrap();

        subscription = governor.get_subscription(&subscription.id).unwrap();
        assert_eq!(subscription.state, SubscriptionState::Active);

        // Act: Trigger renewal
        let (new_state, action) = governor
            .transition(
                &subscription.id,
                SubscriptionEvent::RenewalDateApproaching,
            )
            .await
            .unwrap();

        // Assert
        assert_eq!(new_state, SubscriptionState::AwaitingRenewal);
        assert!(action.is_some());
        assert!(action.unwrap().contains("renewal reminder"));
    }

    #[tokio::test]
    async fn test_upgrade_with_proration_monthly() {
        // Arrange
        let mut governor = SubscriptionGovernor::new();
        let subscription = governor
            .create_trial("cust-004".to_string(), AccountType::Individual)
            .await
            .unwrap();

        governor
            .transition(
                &subscription.id,
                SubscriptionEvent::CustomerPurchases {
                    tier: FeatureTier::Starter,
                    cycle: BillingCycle::Monthly,
                },
            )
            .await
            .unwrap();

        // Simulate 10 days elapsed
        let mut sub = governor.get_subscription(&subscription.id).unwrap();
        sub.current_period_start = Utc::now() - Duration::days(10);
        sub.current_period_end = Utc::now() + Duration::days(20);
        governor.subscriptions.insert(subscription.id.clone(), sub);

        // Act: Request upgrade
        governor
            .transition(
                &subscription.id,
                SubscriptionEvent::CustomerRequestsUpgrade {
                    new_tier: FeatureTier::Professional,
                },
            )
            .await
            .unwrap();

        let (new_state, _) = governor
            .transition(
                &subscription.id,
                SubscriptionEvent::UpgradeApproved,
            )
            .await
            .unwrap();

        // Assert
        assert_eq!(new_state, SubscriptionState::Active);
        let updated = governor.get_subscription(&subscription.id).unwrap();
        assert_eq!(updated.current_tier, FeatureTier::Professional);
        assert!(updated.pending_upgrade.is_none());
    }

    #[tokio::test]
    async fn test_downgrade_with_refund() {
        // Arrange
        let mut governor = SubscriptionGovernor::new();
        let subscription = governor
            .create_trial("cust-005".to_string(), AccountType::Individual)
            .await
            .unwrap();

        governor
            .transition(
                &subscription.id,
                SubscriptionEvent::CustomerPurchases {
                    tier: FeatureTier::Professional,
                    cycle: BillingCycle::Monthly,
                },
            )
            .await
            .unwrap();

        // Act: Request downgrade
        governor
            .transition(
                &subscription.id,
                SubscriptionEvent::CustomerRequestsDowngrade {
                    new_tier: FeatureTier::Starter,
                },
            )
            .await
            .unwrap();

        let (new_state, _) = governor
            .transition(
                &subscription.id,
                SubscriptionEvent::DowngradeApproved,
            )
            .await
            .unwrap();

        // Assert
        assert_eq!(new_state, SubscriptionState::Active);
        let updated = governor.get_subscription(&subscription.id).unwrap();
        assert_eq!(updated.current_tier, FeatureTier::Starter);
    }

    #[tokio::test]
    async fn test_failed_renewal_grace_period_recovery() {
        // Arrange
        let mut governor = SubscriptionGovernor::new();
        let subscription = governor
            .create_trial("cust-006".to_string(), AccountType::Individual)
            .await
            .unwrap();

        governor
            .transition(
                &subscription.id,
                SubscriptionEvent::CustomerPurchases {
                    tier: FeatureTier::Starter,
                    cycle: BillingCycle::Monthly,
                },
            )
            .await
            .unwrap();

        governor
            .transition(
                &subscription.id,
                SubscriptionEvent::RenewalDateApproaching,
            )
            .await
            .unwrap();

        // Act: Payment fails
        governor
            .transition(
                &subscription.id,
                SubscriptionEvent::RenewalPaymentFailed {
                    reason: "Card declined".to_string(),
                },
            )
            .await
            .unwrap();

        let (state_grace, _) = governor.get_subscription(&subscription.id).unwrap();
        assert_eq!(
            governor.get_subscription(&subscription.id).unwrap().state,
            SubscriptionState::RenewalGrace
        );

        // Customer fixes payment
        let (new_state, action) = governor
            .transition(
                &subscription.id,
                SubscriptionEvent::CustomerFixesPayment {
                    amount_cents: 2999,
                },
            )
            .await
            .unwrap();

        // Assert
        assert_eq!(new_state, SubscriptionState::Active);
        assert!(action.is_some());

        let updated = governor.get_subscription(&subscription.id).unwrap();
        assert_eq!(updated.state, SubscriptionState::Active);
    }

    #[tokio::test]
    async fn test_cancellation_with_reactivation() {
        // Arrange
        let mut governor = SubscriptionGovernor::new();
        let subscription = governor
            .create_trial("cust-007".to_string(), AccountType::Individual)
            .await
            .unwrap();

        governor
            .transition(
                &subscription.id,
                SubscriptionEvent::CustomerPurchases {
                    tier: FeatureTier::Starter,
                    cycle: BillingCycle::Monthly,
                },
            )
            .await
            .unwrap();

        // Act: Cancel
        let (state_cancelled, _) = governor
            .transition(
                &subscription.id,
                SubscriptionEvent::CustomerCancels {
                    reason: "Too expensive".to_string(),
                },
            )
            .await
            .unwrap();

        assert_eq!(state_cancelled, SubscriptionState::Cancelled);

        let updated = governor.get_subscription(&subscription.id).unwrap();
        assert!(updated.can_reactivate());

        // Reactivate
        let (new_state, action) = governor
            .transition(
                &subscription.id,
                SubscriptionEvent::CustomerRequestsReactivation,
            )
            .await
            .unwrap();

        // Assert
        assert_eq!(new_state, SubscriptionState::Active);
        assert!(action.is_some());
    }

    #[tokio::test]
    async fn test_proration_calculation_monthly() {
        // 30-day month, 10 days elapsed, 20 days remaining
        // Starter: $29.99, Professional: $99.99

        let proration = SubscriptionGovernor::calculate_proration(
            &FeatureTier::Starter,
            &FeatureTier::Professional,
            BillingCycle::Monthly,
            10,
            20,
        )
        .unwrap();

        // Daily rates: Starter ~$1.00, Professional ~$3.33
        // Remaining for Starter: $20.00, Professional: $66.60
        // Proration: $66.60 - $20.00 = $46.60 ≈ 4660 cents

        assert!(proration.proration_amount_cents > 4500);
        assert!(proration.proration_amount_cents < 4800);
    }

    #[tokio::test]
    async fn test_invalid_upgrade_to_lower_tier() {
        // Arrange
        let mut governor = SubscriptionGovernor::new();
        let subscription = governor
            .create_trial("cust-008".to_string(), AccountType::Individual)
            .await
            .unwrap();

        governor
            .transition(
                &subscription.id,
                SubscriptionEvent::CustomerPurchases {
                    tier: FeatureTier::Professional,
                    cycle: BillingCycle::Monthly,
                },
            )
            .await
            .unwrap();

        // Act: Try to "upgrade" to lower tier
        let result = governor
            .transition(
                &subscription.id,
                SubscriptionEvent::CustomerRequestsUpgrade {
                    new_tier: FeatureTier::Starter,
                },
            )
            .await;

        // Assert
        assert!(result.is_err());
        assert!(matches!(result.unwrap_err(), SubscriptionError::UpgradeError(_)));
    }

    #[tokio::test]
    async fn test_audit_trail_tracks_all_transitions() {
        // Arrange
        let mut governor = SubscriptionGovernor::new();
        let subscription = governor
            .create_trial("cust-009".to_string(), AccountType::Individual)
            .await
            .unwrap();

        // Act
        governor
            .transition(
                &subscription.id,
                SubscriptionEvent::CustomerPurchases {
                    tier: FeatureTier::Starter,
                    cycle: BillingCycle::Monthly,
                },
            )
            .await
            .unwrap();

        governor
            .transition(
                &subscription.id,
                SubscriptionEvent::RenewalDateApproaching,
            )
            .await
            .unwrap();

        // Assert
        let audit = governor.get_audit_trail(&subscription.id);
        assert!(audit.len() >= 3); // Created + Purchase + Renewal
        assert_eq!(audit[0].to_state, "Trial");
        assert_eq!(audit[1].to_state, "Active");
        assert_eq!(audit[2].to_state, "AwaitingRenewal");
    }

    #[tokio::test]
    async fn test_feature_tier_limits() {
        // Free tier: 10k API calls
        // Starter tier: 1M API calls
        // Professional: 10M API calls

        assert_eq!(FeatureTier::Free.get_limits("api_calls_per_month"), Some(10000));
        assert_eq!(
            FeatureTier::Starter.get_limits("api_calls_per_month"),
            Some(1000000)
        );
        assert_eq!(
            FeatureTier::Professional.get_limits("api_calls_per_month"),
            Some(10000000)
        );
    }

    #[tokio::test]
    async fn test_feature_availability_per_tier() {
        // Free: basic features only
        assert!(FeatureTier::Free.has_feature("basic_api"));
        assert!(!FeatureTier::Free.has_feature("api_access"));

        // Professional: advanced features
        assert!(FeatureTier::Professional.has_feature("api_access"));
        assert!(FeatureTier::Professional.has_feature("sso"));

        // Enterprise: all features
        assert!(FeatureTier::Enterprise.has_feature("anything"));
    }

    #[tokio::test]
    async fn test_trial_ending_soon_signal() {
        // Arrange
        let mut governor = SubscriptionGovernor::new();
        let mut subscription = governor
            .create_trial("cust-010".to_string(), AccountType::Individual)
            .await
            .unwrap();

        let subscription_id = subscription.id.clone();

        // Simulate 8 days elapsed (7-day window before expiry)
        subscription.trial_ends_at = Some(Utc::now() + Duration::days(6));
        governor.subscriptions.insert(subscription_id.clone(), subscription);

        // Act
        let sub = governor.get_subscription(&subscription_id).unwrap();
        let is_expiring_soon = sub.is_trial_expiring_soon();

        // Assert
        assert!(is_expiring_soon);
    }

    #[tokio::test]
    async fn test_renewal_approaching_signal() {
        // Arrange
        let mut governor = SubscriptionGovernor::new();
        let subscription = governor
            .create_trial("cust-011".to_string(), AccountType::Individual)
            .await
            .unwrap();

        governor
            .transition(
                &subscription.id,
                SubscriptionEvent::CustomerPurchases {
                    tier: FeatureTier::Starter,
                    cycle: BillingCycle::Monthly,
                },
            )
            .await
            .unwrap();

        // Simulate 25 days into 30-day period (5 days remaining)
        let mut sub = governor.get_subscription(&subscription.id).unwrap();
        sub.current_period_start = Utc::now() - Duration::days(25);
        sub.current_period_end = Utc::now() + Duration::days(5);
        governor.subscriptions.insert(subscription.id.clone(), sub);

        // Act
        let updated = governor.get_subscription(&subscription.id).unwrap();
        let approaching = updated.is_renewal_approaching();

        // Assert
        assert!(approaching);
    }
}
