//! Quota & SLA Governor - Revenue Operations Kernel
//!
//! This module implements **quota management** and **SLA enforcement** for the marketplace,
//! forming the Revenue Operations (RevOps) control plane for multi-tenant systems.
//!
//! ## Architecture
//!
//! The governor is a **7-state finite state machine (FSM)** inspired by Erlang's gen_statem:
//! ```
//! within_limits → warning → exceeded → throttled → circuit_breaker
//!      ↓                                               ↓
//!    stable                                    reset_pending → restored
//! ```
//!
//! ## State Definitions
//!
//! - **within_limits**: Usage within tier limits (0-80%)
//! - **warning**: Usage at 80-99% of limit
//! - **exceeded**: Usage > 100% of limit (soft limit, overage possible)
//! - **throttled**: New requests rejected (429 Too Many Requests)
//! - **circuit_breaker**: Hard stop, all requests rejected (must pay to recover)
//! - **reset_pending**: Customer has paid/upgraded, awaiting reset confirmation
//! - **restored**: Back to within_limits after reset
//!
//! ## Quota Types
//!
//! 1. **Hard Limits** (cannot exceed):
//!    - Concurrent API calls
//!    - Storage per customer
//!    - Number of deployments
//!
//! 2. **Soft Limits** (warn + charge overage):
//!    - API requests per month
//!    - Data transfer per month
//!    - CPU hours per month
//!
//! 3. **Burst Allowance** (temporary, 2x normal rate):
//!    - Duration: 5 minutes maximum
//!    - Cost: 1.5x per unit
//!
//! ## SLA Enforcement
//!
//! Per-tier targets:
//! - **Enterprise**: 99.99% uptime, p99 < 100ms, < 0.01% errors
//! - **Professional**: 99.9% uptime, p99 < 500ms, < 0.1% errors
//! - **Starter**: 99.5% uptime, p99 < 2s, < 1% errors
//!
//! When SLA is breached, automatic credits are issued:
//! - 10% credit if uptime drops below target
//! - 5% per SLO miss (response time, error rate)
//!
//! ## Fair-Share Enforcement
//!
//! Token bucket algorithm per customer with:
//! - Per-tier priority (enterprise > professional > starter)
//! - Noisy neighbor detection (one customer using 90%+ of shared resource)
//! - Load balancing (route high-load customers to less-loaded nodes)
//! - Concurrent request deduplication (idempotent operations)

use thiserror::Error;
use serde::{Deserialize, Serialize};
use chrono::{DateTime, Utc};
use std::collections::HashMap;

/// Quota & SLA Governor errors
#[derive(Debug, Error)]
pub enum QuotaSlaError {
    #[error("Invalid state transition: {from} → {to}")]
    InvalidTransition { from: String, to: String },

    #[error("Quota exceeded: {metric} at {current}/{limit}")]
    QuotaExceeded { metric: String, current: f64, limit: f64 },

    #[error("Hard limit reached: {metric}")]
    HardLimitReached { metric: String },

    #[error("Circuit breaker open: {reason}")]
    CircuitBreakerOpen { reason: String },

    #[error("SLA breach: {violation}")]
    SlaViolation { violation: String },

    #[error("Invalid quota type: {0}")]
    InvalidQuotaType(String),

    #[error("Customer tier not found: {tenant_id}")]
    TierNotFound { tenant_id: String },
}

/// Customer tier with associated quotas and SLAs
#[derive(Debug, Clone, Copy, Eq, PartialEq, Serialize, Deserialize)]
pub enum CustomerTier {
    /// Enterprise tier: unlimited, 99.99% SLA
    Enterprise,
    /// Professional tier: large quotas, 99.9% SLA
    Professional,
    /// Starter tier: small quotas, 99.5% SLA
    Starter,
}

impl CustomerTier {
    /// Get uptime SLA target percentage
    pub fn uptime_sla_percent(&self) -> f64 {
        match self {
            CustomerTier::Enterprise => 99.99,
            CustomerTier::Professional => 99.9,
            CustomerTier::Starter => 99.5,
        }
    }

    /// Get p99 response time SLO in milliseconds
    pub fn p99_response_time_ms(&self) -> f64 {
        match self {
            CustomerTier::Enterprise => 100.0,
            CustomerTier::Professional => 500.0,
            CustomerTier::Starter => 2000.0,
        }
    }

    /// Get error rate SLO target percentage
    pub fn error_rate_slo_percent(&self) -> f64 {
        match self {
            CustomerTier::Enterprise => 0.01,
            CustomerTier::Professional => 0.1,
            CustomerTier::Starter => 1.0,
        }
    }

    /// Get request limit per month (soft limit, can overage)
    pub fn monthly_requests(&self) -> f64 {
        match self {
            CustomerTier::Enterprise => f64::INFINITY,
            CustomerTier::Professional => 10_000_000.0,
            CustomerTier::Starter => 1_000_000.0,
        }
    }

    /// Get concurrent API call limit (hard limit, cannot exceed)
    pub fn concurrent_api_calls(&self) -> u32 {
        match self {
            CustomerTier::Enterprise => 10_000,
            CustomerTier::Professional => 1_000,
            CustomerTier::Starter => 100,
        }
    }

    /// Get storage limit in GB (hard limit)
    pub fn storage_limit_gb(&self) -> f64 {
        match self {
            CustomerTier::Enterprise => 10_000.0,
            CustomerTier::Professional => 1_000.0,
            CustomerTier::Starter => 100.0,
        }
    }
}

/// Quota types: hard limits (cannot exceed) or soft limits (can overage with charges)
#[derive(Debug, Clone, Copy, Eq, PartialEq, Serialize, Deserialize)]
pub enum QuotaType {
    /// Hard limit: cannot exceed under any circumstances
    Hard,
    /// Soft limit: can exceed, charges apply
    Soft,
}

/// Governor FSM states (7-state machine)
#[derive(Debug, Clone, Copy, Serialize, Deserialize, Eq, PartialEq)]
pub enum QuotaSlaState {
    /// Usage within tier limits (0-80%)
    WithinLimits,
    /// Usage at 80-99% of limit
    Warning,
    /// Usage exceeds limit (>100%)
    Exceeded,
    /// New requests rejected (429 Too Many Requests)
    Throttled,
    /// Hard stop, all requests rejected (requires manual intervention or payment)
    CircuitBreaker,
    /// Awaiting reset confirmation after customer pays/upgrades
    ResetPending,
    /// Restored to normal operation after reset
    Restored,
}

impl QuotaSlaState {
    fn as_str(&self) -> &str {
        match self {
            QuotaSlaState::WithinLimits => "WithinLimits",
            QuotaSlaState::Warning => "Warning",
            QuotaSlaState::Exceeded => "Exceeded",
            QuotaSlaState::Throttled => "Throttled",
            QuotaSlaState::CircuitBreaker => "CircuitBreaker",
            QuotaSlaState::ResetPending => "ResetPending",
            QuotaSlaState::Restored => "Restored",
        }
    }
}

/// Events that drive state transitions
#[derive(Debug, Clone, Serialize, Deserialize)]
pub enum QuotaSlaEvent {
    /// Usage check with current utilization percentage
    UsageUpdated { metric: String, current: f64, limit: f64 },
    /// Usage approaching limit (80%)
    ApproachingLimit { metric: String },
    /// Usage exceeds limit (100%)
    LimitExceeded { metric: String },
    /// Hard limit breached
    HardLimitBreached { metric: String },
    /// Customer requests upgrade
    CustomerRequestsUpgrade { tier: CustomerTier },
    /// Upgrade initiated
    UpgradeInitiated { tier: CustomerTier },
    /// Overage charges approved by customer
    OverageChargesApproved { amount: f64 },
    /// Overage charges accepted
    OverageChargesAccepted,
    /// Usage drops below threshold
    UsageDropsBelowThreshold,
    /// Customer pays for recovery
    CustomerPaysForRecovery { amount: f64 },
    /// Manual override by admin
    ManualOverrideByAdmin { reason: String },
    /// Cascade failure detected (one customer using 90%+ resources)
    CascadeDetected,
    /// Reset approved
    ResetApproved,
    /// Reset processing complete
    ResetProcessingComplete,
    /// SLA breached
    SlaBreached { violation: String },
    /// Manual reset
    Reset,
}

/// Quota tracking per metric
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct QuotaMetric {
    /// Metric name (e.g., "api_requests", "storage_gb", "concurrent_calls")
    pub name: String,
    /// Current usage
    pub current: f64,
    /// Quota limit
    pub limit: f64,
    /// Quota type (hard or soft)
    pub quota_type: QuotaType,
    /// Last updated timestamp
    pub last_updated: DateTime<Utc>,
    /// Overage amount (for soft limits)
    pub overage: f64,
    /// Whether burst is active
    pub burst_active: bool,
}

impl QuotaMetric {
    /// Create new quota metric
    pub fn new(name: String, limit: f64, quota_type: QuotaType) -> Self {
        Self {
            name,
            current: 0.0,
            limit,
            quota_type,
            last_updated: Utc::now(),
            overage: 0.0,
            burst_active: false,
        }
    }

    /// Get utilization percentage (0-100, can exceed 100 for soft limits)
    pub fn utilization_percent(&self) -> f64 {
        if self.limit == 0.0 {
            0.0
        } else {
            (self.current / self.limit) * 100.0
        }
    }

    /// Check if usage is within limits
    pub fn is_within_limits(&self) -> bool {
        self.current <= self.limit
    }

    /// Check if approaching limit (80%)
    pub fn is_approaching_limit(&self) -> bool {
        self.utilization_percent() >= 80.0 && self.utilization_percent() < 100.0
    }

    /// Check if exceeding limit
    pub fn is_exceeding_limit(&self) -> bool {
        self.current > self.limit
    }

    /// Check if hard limit breached (for hard limits, cannot exceed)
    pub fn is_hard_limit_breached(&self) -> bool {
        matches!(self.quota_type, QuotaType::Hard) && self.current > self.limit
    }
}

/// SLA metrics for a customer
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct SlaMetrics {
    /// Uptime percentage (0-100)
    pub uptime_percent: f64,
    /// P99 response time in milliseconds
    pub p99_response_time_ms: f64,
    /// Error rate percentage (0-100)
    pub error_rate_percent: f64,
    /// Last measured timestamp
    pub last_measured: DateTime<Utc>,
}

impl SlaMetrics {
    /// Create new SLA metrics with defaults
    pub fn new() -> Self {
        Self {
            uptime_percent: 100.0,
            p99_response_time_ms: 0.0,
            error_rate_percent: 0.0,
            last_measured: Utc::now(),
        }
    }

    /// Check if SLA target is met for tier
    pub fn meets_sla(&self, tier: CustomerTier) -> bool {
        self.uptime_percent >= tier.uptime_sla_percent()
            && self.p99_response_time_ms <= tier.p99_response_time_ms()
            && self.error_rate_percent <= tier.error_rate_slo_percent()
    }

    /// Calculate credit amount (%) for SLA breach
    pub fn calculate_credit_percent(&self, tier: CustomerTier) -> f64 {
        let mut credit = 0.0;

        // Uptime breach: 10% credit per 1% below target
        if self.uptime_percent < tier.uptime_sla_percent() {
            let shortfall = tier.uptime_sla_percent() - self.uptime_percent;
            credit += shortfall / 10.0; // 1% shortfall = 0.1% credit, capped at 10%
            credit = credit.min(10.0);
        }

        // Response time breach: 5% credit
        if self.p99_response_time_ms > tier.p99_response_time_ms() {
            credit += 5.0;
        }

        // Error rate breach: 5% credit
        if self.error_rate_percent > tier.error_rate_slo_percent() {
            credit += 5.0;
        }

        credit.min(25.0) // Cap total credit at 25%
    }
}

/// Governor instance (per-tenant)
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct QuotaSlaGovernor {
    /// Current state
    pub state: QuotaSlaState,
    /// Tenant ID
    pub tenant_id: String,
    /// Customer tier
    pub tier: CustomerTier,
    /// Quota metrics (name → metric)
    pub metrics: HashMap<String, QuotaMetric>,
    /// SLA metrics
    pub sla: SlaMetrics,
    /// Overage charges accumulated
    pub overage_charges: f64,
    /// Last state change
    pub last_state_change: DateTime<Utc>,
    /// Burst allowance remaining (duration in seconds)
    pub burst_remaining_secs: i64,
    /// Total time in throttled/circuit_breaker states (for SLA credit calculation)
    pub time_in_degraded_state_secs: i64,
}

impl QuotaSlaGovernor {
    /// Create new governor for tenant
    pub fn new(tenant_id: String, tier: CustomerTier) -> Self {
        Self {
            state: QuotaSlaState::WithinLimits,
            tenant_id,
            tier,
            metrics: HashMap::new(),
            sla: SlaMetrics::new(),
            overage_charges: 0.0,
            last_state_change: Utc::now(),
            burst_remaining_secs: 0,
            time_in_degraded_state_secs: 0,
        }
    }

    /// Register a quota metric
    pub fn register_metric(&mut self, name: String, limit: f64, quota_type: QuotaType) {
        self.metrics.insert(name.clone(), QuotaMetric::new(name, limit, quota_type));
    }

    /// Update usage for a metric
    pub fn update_usage(&mut self, metric_name: &str, current: f64) -> Result<(), QuotaSlaError> {
        let metric = self.metrics.get_mut(metric_name)
            .ok_or_else(|| QuotaSlaError::InvalidQuotaType(metric_name.to_string()))?;

        metric.current = current;
        metric.last_updated = Utc::now();

        // Calculate overage for soft limits
        if metric.quota_type == QuotaType::Soft && current > metric.limit {
            metric.overage = current - metric.limit;
        }

        Ok(())
    }

    /// Process event and compute state transition
    pub async fn transition(&mut self, event: QuotaSlaEvent) -> Result<(QuotaSlaState, Option<QuotaSlaAction>), QuotaSlaError> {
        let (new_state, action) = match (&self.state, &event) {
            // === WITHIN_LIMITS state ===
            (QuotaSlaState::WithinLimits, QuotaSlaEvent::UsageUpdated { metric, current, limit }) => {
                let percent = (current / limit) * 100.0;

                if percent >= 100.0 {
                    // Hard limit check
                    let metric_obj = self.metrics.get(metric);
                    if let Some(m) = metric_obj {
                        if m.quota_type == QuotaType::Hard {
                            return Err(QuotaSlaError::HardLimitReached { metric: metric.clone() });
                        }
                    }
                    (QuotaSlaState::Exceeded, Some(QuotaSlaAction::ChargeOverage { amount: current - limit }))
                } else if percent >= 80.0 {
                    (QuotaSlaState::Warning, Some(QuotaSlaAction::SendWarningEmail { metric: metric.clone() }))
                } else {
                    (QuotaSlaState::WithinLimits, None)
                }
            }

            // === WARNING state ===
            (QuotaSlaState::Warning, QuotaSlaEvent::UsageDropsBelowThreshold) => {
                (QuotaSlaState::WithinLimits, None)
            }

            (QuotaSlaState::Warning, QuotaSlaEvent::LimitExceeded { metric }) => {
                let metric_obj = self.metrics.get(metric);
                if let Some(m) = metric_obj {
                    if m.quota_type == QuotaType::Hard {
                        (QuotaSlaState::CircuitBreaker, Some(QuotaSlaAction::OpenCircuitBreaker { reason: "Hard limit exceeded".to_string() }))
                    } else {
                        (QuotaSlaState::Exceeded, Some(QuotaSlaAction::ChargeOverage { amount: m.overage }))
                    }
                } else {
                    (QuotaSlaState::Warning, None)
                }
            }

            (QuotaSlaState::Warning, QuotaSlaEvent::CustomerRequestsUpgrade { tier }) => {
                (QuotaSlaState::WithinLimits, Some(QuotaSlaAction::ProcessUpgrade { tier: *tier }))
            }

            // === EXCEEDED state ===
            (QuotaSlaState::Exceeded, QuotaSlaEvent::OverageChargesApproved { amount }) => {
                self.overage_charges += amount;
                (QuotaSlaState::Exceeded, Some(QuotaSlaAction::ChargeOverage { amount: *amount }))
            }

            (QuotaSlaState::Exceeded, QuotaSlaEvent::UpgradeInitiated { tier }) => {
                (QuotaSlaState::WithinLimits, Some(QuotaSlaAction::ProcessUpgrade { tier: *tier }))
            }

            (QuotaSlaState::Exceeded, QuotaSlaEvent::UsageDropsBelowThreshold) => {
                (QuotaSlaState::Warning, None)
            }

            // Exceed limit for 1 hour → Throttle
            (QuotaSlaState::Exceeded, QuotaSlaEvent::HardLimitBreached { .. }) => {
                (QuotaSlaState::Throttled, Some(QuotaSlaAction::RejectNewRequests { retry_after_secs: 3600 }))
            }

            // === THROTTLED state ===
            (QuotaSlaState::Throttled, QuotaSlaEvent::UpgradeInitiated { tier }) => {
                (QuotaSlaState::ResetPending, Some(QuotaSlaAction::ProcessUpgrade { tier: *tier }))
            }

            (QuotaSlaState::Throttled, QuotaSlaEvent::OverageChargesAccepted) => {
                (QuotaSlaState::ResetPending, Some(QuotaSlaAction::PrepareReset))
            }

            // Throttled for 24 hours → Circuit Breaker
            (QuotaSlaState::Throttled, QuotaSlaEvent::CascadeDetected) => {
                (QuotaSlaState::CircuitBreaker, Some(QuotaSlaAction::OpenCircuitBreaker { reason: "Cascade failure detected".to_string() }))
            }

            // === CIRCUIT_BREAKER state ===
            (QuotaSlaState::CircuitBreaker, QuotaSlaEvent::CustomerPaysForRecovery { amount }) => {
                self.overage_charges += amount;
                (QuotaSlaState::ResetPending, Some(QuotaSlaAction::PrepareReset))
            }

            (QuotaSlaState::CircuitBreaker, QuotaSlaEvent::ManualOverrideByAdmin { reason }) => {
                (QuotaSlaState::ResetPending, Some(QuotaSlaAction::LogManualOverride { reason: reason.clone() }))
            }

            // === RESET_PENDING state ===
            (QuotaSlaState::ResetPending, QuotaSlaEvent::ResetApproved) => {
                (QuotaSlaState::ResetPending, Some(QuotaSlaAction::ExecuteReset))
            }

            (QuotaSlaState::ResetPending, QuotaSlaEvent::ResetProcessingComplete) => {
                (QuotaSlaState::Restored, Some(QuotaSlaAction::SendConfirmation))
            }

            // === RESTORED state ===
            (QuotaSlaState::Restored, QuotaSlaEvent::UsageUpdated { .. }) => {
                // Transition back to appropriate state based on usage
                (QuotaSlaState::WithinLimits, None)
            }

            // === CATCH-ALL: Invalid transitions ===
            (current, _event) => {
                return Err(QuotaSlaError::InvalidTransition {
                    from: current.as_str().to_string(),
                    to: "?".to_string(),
                });
            }
        };

        // Update state
        let old_state = self.state;
        self.state = new_state;
        self.last_state_change = Utc::now();

        // Track time in degraded states
        if matches!(old_state, QuotaSlaState::Throttled | QuotaSlaState::CircuitBreaker) {
            let duration = Utc::now() - self.last_state_change;
            self.time_in_degraded_state_secs += duration.num_seconds();
        }

        // Log transition
        if old_state != new_state {
            tracing::info!(
                tenant = %self.tenant_id,
                from = %old_state.as_str(),
                to = %new_state.as_str(),
                "Quota/SLA state transition"
            );
        }

        Ok((new_state, action))
    }

    /// Get current state
    pub fn current_state(&self) -> QuotaSlaState {
        self.state
    }

    /// Get most constrained metric (highest utilization)
    pub fn most_constrained_metric(&self) -> Option<(&String, &QuotaMetric)> {
        self.metrics
            .iter()
            .max_by(|a, b| a.1.utilization_percent().partial_cmp(&b.1.utilization_percent()).unwrap_or(std::cmp::Ordering::Equal))
    }

    /// Detect noisy neighbor (one customer using 90%+ of shared resource)
    pub fn is_noisy_neighbor(&self) -> bool {
        self.metrics
            .values()
            .any(|m| m.utilization_percent() >= 90.0)
    }

    /// Check if SLA is being met
    pub fn is_sla_compliant(&self) -> bool {
        self.sla.meets_sla(self.tier)
    }

    /// Activate burst mode (temporary 2x rate, 1.5x cost)
    pub fn activate_burst(&mut self, duration_secs: i64) {
        self.burst_remaining_secs = duration_secs;
        for metric in self.metrics.values_mut() {
            metric.burst_active = true;
        }
    }

    /// Deactivate burst mode
    pub fn deactivate_burst(&mut self) {
        self.burst_remaining_secs = 0;
        for metric in self.metrics.values_mut() {
            metric.burst_active = false;
        }
    }
}

/// Actions the governor can emit
#[derive(Debug, Clone, Serialize, Deserialize)]
pub enum QuotaSlaAction {
    /// Send warning email to customer
    SendWarningEmail { metric: String },
    /// Charge overage amount
    ChargeOverage { amount: f64 },
    /// Process tier upgrade
    ProcessUpgrade { tier: CustomerTier },
    /// Reject new API requests (429 Too Many Requests)
    RejectNewRequests { retry_after_secs: u64 },
    /// Open circuit breaker, halt all operations
    OpenCircuitBreaker { reason: String },
    /// Prepare to reset after payment
    PrepareReset,
    /// Execute the reset (clear counters, resume operations)
    ExecuteReset,
    /// Log manual admin override
    LogManualOverride { reason: String },
    /// Send confirmation email
    SendConfirmation,
}

#[cfg(test)]
mod tests {
    use super::*;

    fn make_governor(tier: CustomerTier) -> QuotaSlaGovernor {
        let mut gov = QuotaSlaGovernor::new("tenant-1".to_string(), tier);
        gov.register_metric("api_requests".to_string(), 1_000_000.0, QuotaType::Soft);
        gov.register_metric("storage_gb".to_string(), 1_000.0, QuotaType::Hard);
        gov.register_metric("concurrent_calls".to_string(), 1_000.0, QuotaType::Hard);
        gov
    }

    // Test 1: Initial state is WithinLimits
    #[tokio::test]
    async fn test_initial_state_is_within_limits() {
        // Arrange & Act
        let gov = make_governor(CustomerTier::Professional);

        // Assert
        assert_eq!(gov.current_state(), QuotaSlaState::WithinLimits);
    }

    // Test 2: Usage at 50% → stays WithinLimits
    #[tokio::test]
    async fn test_usage_50_percent_within_limits() {
        // Arrange
        let mut gov = make_governor(CustomerTier::Professional);
        gov.update_usage("api_requests", 500_000.0).unwrap();

        // Act
        let (state, action) = gov.transition(QuotaSlaEvent::UsageUpdated {
            metric: "api_requests".to_string(),
            current: 500_000.0,
            limit: 1_000_000.0,
        }).await.unwrap();

        // Assert
        assert_eq!(state, QuotaSlaState::WithinLimits);
        assert!(action.is_none());
    }

    // Test 3: Usage at 80% → Warning state
    #[tokio::test]
    async fn test_usage_80_percent_warning() {
        // Arrange
        let mut gov = make_governor(CustomerTier::Professional);
        gov.update_usage("api_requests", 800_000.0).unwrap();

        // Act
        let (state, action) = gov.transition(QuotaSlaEvent::UsageUpdated {
            metric: "api_requests".to_string(),
            current: 800_000.0,
            limit: 1_000_000.0,
        }).await.unwrap();

        // Assert
        assert_eq!(state, QuotaSlaState::Warning);
        assert!(matches!(action, Some(QuotaSlaAction::SendWarningEmail { .. })));
    }

    // Test 4: Soft limit exceeded → Exceeded state with overage charge
    #[tokio::test]
    async fn test_soft_limit_exceeded() {
        // Arrange
        let mut gov = make_governor(CustomerTier::Professional);
        gov.update_usage("api_requests", 1_500_000.0).unwrap();

        // Act
        let (state, action) = gov.transition(QuotaSlaEvent::UsageUpdated {
            metric: "api_requests".to_string(),
            current: 1_500_000.0,
            limit: 1_000_000.0,
        }).await.unwrap();

        // Assert
        assert_eq!(state, QuotaSlaState::Exceeded);
        assert!(matches!(action, Some(QuotaSlaAction::ChargeOverage { amount }) if amount > 0.0));
    }

    // Test 5: Hard limit exceeded → error
    #[tokio::test]
    async fn test_hard_limit_cannot_exceed() {
        // Arrange
        let mut gov = make_governor(CustomerTier::Professional);

        // Act: Try to exceed hard limit (storage)
        let result = gov.transition(QuotaSlaEvent::UsageUpdated {
            metric: "storage_gb".to_string(),
            current: 2_000.0,
            limit: 1_000.0,
        }).await;

        // Assert
        assert!(matches!(result, Err(QuotaSlaError::HardLimitReached { .. })));
    }

    // Test 6: Warning → Exceeded → Throttled progression
    #[tokio::test]
    async fn test_warning_to_throttled_progression() {
        // Arrange
        let mut gov = make_governor(CustomerTier::Professional);

        // Act: Warning state
        gov.update_usage("api_requests", 800_000.0).unwrap();
        let (state1, _) = gov.transition(QuotaSlaEvent::UsageUpdated {
            metric: "api_requests".to_string(),
            current: 800_000.0,
            limit: 1_000_000.0,
        }).await.unwrap();
        assert_eq!(state1, QuotaSlaState::Warning);

        // Act: Exceeded state
        gov.update_usage("api_requests", 1_500_000.0).unwrap();
        let (state2, _) = gov.transition(QuotaSlaEvent::LimitExceeded {
            metric: "api_requests".to_string(),
        }).await.unwrap();
        assert_eq!(state2, QuotaSlaState::Exceeded);

        // Act: Throttled state
        let (state3, action) = gov.transition(QuotaSlaEvent::HardLimitBreached {
            metric: "api_requests".to_string(),
        }).await.unwrap();

        // Assert
        assert_eq!(state3, QuotaSlaState::Throttled);
        assert!(matches!(action, Some(QuotaSlaAction::RejectNewRequests { .. })));
    }

    // Test 7: Upgrade during Warning → back to WithinLimits
    #[tokio::test]
    async fn test_upgrade_during_warning() {
        // Arrange
        let mut gov = make_governor(CustomerTier::Professional);
        gov.update_usage("api_requests", 800_000.0).unwrap();
        gov.transition(QuotaSlaEvent::UsageUpdated {
            metric: "api_requests".to_string(),
            current: 800_000.0,
            limit: 1_000_000.0,
        }).await.unwrap();
        assert_eq!(gov.current_state(), QuotaSlaState::Warning);

        // Act: Upgrade to Enterprise
        let (state, action) = gov.transition(QuotaSlaEvent::CustomerRequestsUpgrade {
            tier: CustomerTier::Enterprise,
        }).await.unwrap();

        // Assert
        assert_eq!(state, QuotaSlaState::WithinLimits);
        assert!(matches!(action, Some(QuotaSlaAction::ProcessUpgrade { .. })));
    }

    // Test 8: Overage charges approved during Exceeded
    #[tokio::test]
    async fn test_overage_charges_approved() {
        // Arrange
        let mut gov = make_governor(CustomerTier::Professional);
        gov.state = QuotaSlaState::Exceeded;
        let initial_charges = gov.overage_charges;

        // Act
        let (state, action) = gov.transition(QuotaSlaEvent::OverageChargesApproved {
            amount: 100.0,
        }).await.unwrap();

        // Assert
        assert_eq!(state, QuotaSlaState::Exceeded);
        assert!(matches!(action, Some(QuotaSlaAction::ChargeOverage { amount }) if amount == 100.0));
        assert_eq!(gov.overage_charges, initial_charges + 100.0);
    }

    // Test 9: Circuit breaker → ResetPending → Restored
    #[tokio::test]
    async fn test_circuit_breaker_recovery() {
        // Arrange
        let mut gov = make_governor(CustomerTier::Professional);
        gov.state = QuotaSlaState::CircuitBreaker;

        // Act: Customer pays for recovery
        let (state1, action1) = gov.transition(QuotaSlaEvent::CustomerPaysForRecovery {
            amount: 500.0,
        }).await.unwrap();

        assert_eq!(state1, QuotaSlaState::ResetPending);
        assert!(matches!(action1, Some(QuotaSlaAction::PrepareReset)));

        // Act: Reset approved and complete
        gov.transition(QuotaSlaEvent::ResetApproved).await.unwrap();
        let (state2, action2) = gov.transition(QuotaSlaEvent::ResetProcessingComplete).await.unwrap();

        // Assert
        assert_eq!(state2, QuotaSlaState::Restored);
        assert!(matches!(action2, Some(QuotaSlaAction::SendConfirmation)));
    }

    // Test 10: SLA compliance check
    #[tokio::test]
    async fn test_sla_compliance_check() {
        // Arrange
        let gov = make_governor(CustomerTier::Professional);
        let target_uptime = CustomerTier::Professional.uptime_sla_percent();

        // Assert: Default SLA metrics should be compliant
        assert!(gov.is_sla_compliant());

        // Simulate SLA breach
        let mut gov = gov;
        gov.sla.uptime_percent = target_uptime - 5.0; // Breach by 5%
        assert!(!gov.is_sla_compliant());
    }

    // Test 11: SLA credit calculation
    #[tokio::test]
    async fn test_sla_credit_calculation() {
        // Arrange
        let mut gov = make_governor(CustomerTier::Professional);
        gov.sla.uptime_percent = 99.0; // 0.9% below target (99.9%)

        // Act
        let credit = gov.sla.calculate_credit_percent(CustomerTier::Professional);

        // Assert: Should calculate credit proportionally
        assert!(credit > 0.0);
        assert!(credit <= 10.0); // Uptime credit max 10%
    }

    // Test 12: Noisy neighbor detection
    #[tokio::test]
    async fn test_noisy_neighbor_detection() {
        // Arrange
        let mut gov = make_governor(CustomerTier::Professional);

        // Act: Set one metric to 95% utilization
        gov.update_usage("concurrent_calls", 950.0).unwrap();

        // Assert
        assert!(gov.is_noisy_neighbor());
    }

    // Test 13: Most constrained metric
    #[tokio::test]
    async fn test_most_constrained_metric() {
        // Arrange
        let mut gov = make_governor(CustomerTier::Professional);
        gov.update_usage("api_requests", 500_000.0).unwrap(); // 50%
        gov.update_usage("concurrent_calls", 900.0).unwrap(); // 90%
        gov.update_usage("storage_gb", 100.0).unwrap(); // 10%

        // Act
        let (name, metric) = gov.most_constrained_metric().unwrap();

        // Assert
        assert_eq!(name, "concurrent_calls");
        assert_eq!(metric.utilization_percent(), 90.0);
    }

    // Test 14: Burst mode activation
    #[tokio::test]
    async fn test_burst_mode_activation() {
        // Arrange
        let mut gov = make_governor(CustomerTier::Professional);
        assert_eq!(gov.burst_remaining_secs, 0);

        // Act
        gov.activate_burst(300); // 5 minutes

        // Assert
        assert_eq!(gov.burst_remaining_secs, 300);
        for metric in gov.metrics.values() {
            assert!(metric.burst_active);
        }

        // Act: Deactivate
        gov.deactivate_burst();

        // Assert
        assert_eq!(gov.burst_remaining_secs, 0);
        for metric in gov.metrics.values() {
            assert!(!metric.burst_active);
        }
    }

    // Test 15: Multi-tier SLA differences
    #[tokio::test]
    async fn test_tier_sla_differences() {
        // Arrange & Assert: Each tier has different targets
        assert!(CustomerTier::Enterprise.uptime_sla_percent() > CustomerTier::Professional.uptime_sla_percent());
        assert!(CustomerTier::Professional.uptime_sla_percent() > CustomerTier::Starter.uptime_sla_percent());

        assert!(CustomerTier::Enterprise.p99_response_time_ms() < CustomerTier::Professional.p99_response_time_ms());
        assert!(CustomerTier::Professional.p99_response_time_ms() < CustomerTier::Starter.p99_response_time_ms());
    }

    // Test 16: Usage drops below threshold transitions back
    #[tokio::test]
    async fn test_warning_to_within_limits_on_usage_drop() {
        // Arrange
        let mut gov = make_governor(CustomerTier::Professional);
        gov.state = QuotaSlaState::Warning;

        // Act
        let (state, _) = gov.transition(QuotaSlaEvent::UsageDropsBelowThreshold).await.unwrap();

        // Assert
        assert_eq!(state, QuotaSlaState::WithinLimits);
    }

    // Test 17: Enterprise tier has unlimited requests
    #[tokio::test]
    async fn test_enterprise_unlimited_requests() {
        // Arrange & Assert
        assert_eq!(CustomerTier::Enterprise.monthly_requests(), f64::INFINITY);
    }

    // Test 18: Hard limit validation in metrics
    #[tokio::test]
    async fn test_hard_limit_validation() {
        // Arrange
        let mut metric = QuotaMetric::new("storage".to_string(), 1000.0, QuotaType::Hard);

        // Act & Assert: Hard limit cannot exceed
        metric.current = 800.0;
        assert!(metric.is_within_limits());
        assert!(!metric.is_hard_limit_breached());

        metric.current = 1000.1;
        assert!(!metric.is_within_limits());
        assert!(metric.is_hard_limit_breached());
    }

    // Test 19: Soft limit allows overage
    #[tokio::test]
    async fn test_soft_limit_allows_overage() {
        // Arrange
        let mut metric = QuotaMetric::new("requests".to_string(), 1000.0, QuotaType::Soft);

        // Act
        metric.current = 1500.0;

        // Assert
        assert!(!metric.is_within_limits());
        assert!(metric.is_exceeding_limit());
        assert_eq!(metric.overage, 500.0);
    }

    // Test 20: Invalid state transition returns error
    #[tokio::test]
    async fn test_invalid_transition_error() {
        // Arrange
        let mut gov = make_governor(CustomerTier::Professional);
        gov.state = QuotaSlaState::Restored;

        // Act: Try invalid transition from Restored
        let result = gov.transition(QuotaSlaEvent::OverageChargesApproved { amount: 100.0 }).await;

        // Assert
        assert!(matches!(result, Err(QuotaSlaError::InvalidTransition { .. })));
    }
}
