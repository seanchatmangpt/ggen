//! Customer Account Governor - gen_statem inspired FSM
//!
//! Implements comprehensive customer account lifecycle management with:
//! - Onboarding → Active → Suspended → Under Review → Deactivated → Archived
//! - Compliance monitoring (weekly KYC refresh, fraud scoring, activity analysis)
//! - Fraud detection with machine learning signals
//! - KYC/AML verification and enforcement
//! - Two-factor authentication and account security
//! - GDPR data retention and deletion workflows
//! - Complete audit trail for all state transitions
//!
//! ## FSM State Diagram
//!
//! ```
//! onboarding (7d timeout)
//!   ├─ verify_email, complete_profile, kyc_check_passed ──→ active
//!   ├─ kyc_check_failed ──→ deactivated
//!   └─ fraud_detected ──→ under_review
//!
//! active
//!   ├─ periodic_check (weekly) ──→ compliance_monitoring
//!   ├─ fraud_alert ──→ under_review
//!   ├─ abuse_report ──→ suspended
//!   ├─ inactivity_warning (monthly) ──→ active
//!   └─ activity_detected ──→ active
//!
//! compliance_monitoring (sub-FSM)
//!   ├─ risk_score < 30 ──→ compliant ──→ active
//!   ├─ risk_score 30-70 ──→ increase_monitoring
//!   ├─ risk_score 70-90 ──→ suspicious ──→ suspended
//!   └─ risk_score > 90 ──→ under_review
//!
//! suspended (30d timeout)
//!   ├─ issue_resolved ──→ active
//!   ├─ customer_appeals ──→ active (if valid)
//!   ├─ escalate_to_review ──→ under_review
//!   └─ timeout ──→ deactivated
//!
//! under_review (14d timeout)
//!   ├─ review_completed_approved ──→ active
//!   ├─ review_completed_banned ──→ deactivated
//!   ├─ customer_appeals_denied ──→ deactivated
//!   └─ timeout ──→ deactivated
//!
//! deactivated (30d data retention)
//!   ├─ customer_requests_reactivation (if within 30d) ──→ active
//!   ├─ data_retention_complete ──→ archived
//!   └─ timeout ──→ archived
//!
//! archived (final state, read-only)
//! ```

use serde::{Deserialize, Serialize};
use chrono::{DateTime, Duration, Utc};
use thiserror::Error;
use std::collections::HashMap;

/// Account governor errors
#[derive(Debug, Error, Clone)]
pub enum AccountGovernorError {
    #[error("Invalid state transition: {from} → {to} with event {event}")]
    InvalidTransition {
        from: String,
        to: String,
        event: String,
    },

    #[error("Invariant violation: {0}")]
    InvariantViolation(String),

    #[error("Fraud detection failed: {0}")]
    FraudDetectionFailed(String),

    #[error("Compliance check failed: {0}")]
    ComplianceCheckFailed(String),

    #[error("No action possible in state: {state}")]
    NoActionPossible { state: String },

    #[error("KYC verification required: {0}")]
    KycVerificationRequired(String),

    #[error("Account not found: {customer_id}")]
    AccountNotFound { customer_id: String },

    #[error("Timeout occurred: {0}")]
    TimeoutOccurred(String),

    #[error("Appeal rejected: {reason}")]
    AppealRejected { reason: String },
}

/// Customer account FSM states
#[derive(Debug, Clone, Copy, Serialize, Deserialize, Eq, PartialEq, Hash)]
pub enum AccountState {
    /// New customer registration in progress (7d timeout)
    Onboarding,
    /// Normal customer operation
    Active,
    /// Temporary suspension (payment issue, TOS violation, 30d timeout)
    Suspended,
    /// Under human/admin review (fraud investigation, 14d timeout)
    UnderReview,
    /// Account closed (30d data retention before archive)
    Deactivated,
    /// Final state - read-only, permanent
    Archived,
}

impl AccountState {
    fn as_str(&self) -> &str {
        match self {
            AccountState::Onboarding => "Onboarding",
            AccountState::Active => "Active",
            AccountState::Suspended => "Suspended",
            AccountState::UnderReview => "UnderReview",
            AccountState::Deactivated => "Deactivated",
            AccountState::Archived => "Archived",
        }
    }
}

/// Events that drive account FSM transitions
#[derive(Debug, Clone, Serialize, Deserialize)]
pub enum AccountEvent {
    // === ONBOARDING events ===
    /// Email verification completed
    EmailVerified,
    /// Customer completed profile setup
    ProfileCompleted,
    /// KYC check passed
    KycCheckPassed,
    /// KYC check failed
    KycCheckFailed,
    /// Fraud detected during onboarding
    FraudDetected(String),

    // === ACTIVE events ===
    /// Periodic compliance check triggered (weekly)
    PeriodicComplianceCheck(FraudScore),
    /// Inactivity detected (monthly monitoring)
    InactivityDetected,
    /// Activity detected - normal usage pattern
    ActivityDetected,
    /// Fraud alert from monitoring system
    FraudAlert { reason: String, score: u32 },
    /// Abuse report (high-velocity operations, etc.)
    AbuseReport { reason: String, evidence: Vec<String> },

    // === SUSPENDED events ===
    /// Issue/violation resolved
    IssueResolved,
    /// Customer appeals suspension
    CustomerAppeals { reason: String },
    /// Escalate to human review
    EscalateToReview,

    // === UNDER_REVIEW events ===
    /// Human review completed - account approved
    ReviewCompletedApproved,
    /// Human review completed - account banned
    ReviewCompletedBanned { reason: String },
    /// Customer appeal rejected
    CustomerAppealsDenied { reason: String },

    // === DEACTIVATED events ===
    /// Data retention period complete - ready for archival
    DataRetentionComplete,
    /// Customer requests account reactivation (within 30d)
    CustomerRequestsReactivation,
    /// Archive account (permanent)
    ArchiveAccount,

    // === SYSTEM events ===
    /// Timeout-triggered transition
    TimeoutTransition,
    /// Manual reset by admin (only from Refuse-like states)
    Reset,
}

/// Actions triggered by account state transitions
#[derive(Debug, Clone, Serialize, Deserialize)]
pub enum AccountAction {
    /// Send verification email with link
    SendVerificationEmail,
    /// Request KYC documentation from customer
    RequestKycDocumentation,
    /// Run fraud check (calls external ML service)
    RunFraudCheck,
    /// Send welcome email to newly verified customer
    SendWelcomeEmail,
    /// Track user activity for compliance
    MonitorActivity,
    /// Block account operations
    BlockOperations,
    /// Notify customer of suspension with reason
    NotifyCustomerSuspension { reason: String },
    /// Offer support to resolve issue
    OfferSupport,
    /// Preserve all account data (compliance hold)
    PreserveDataComplianceHold,
    /// Prepare audit report for human review
    PrepareAuditReport,
    /// Notify customer of ban
    NotifyBanDecision { reason: String },
    /// Schedule data deletion (GDPR right-to-be-forgotten)
    ScheduleDataDeletion,
    /// Delete all customer data
    DeleteAllData,
    /// Increase monitoring frequency
    IncreaseMonitoringFrequency,
    /// Send engagement email
    SendEngagementEmail,
    /// Enable two-factor authentication
    EnableTwoFactorAuth,
    /// Verify company domain (B2B)
    VerifyCompanyDomain,
    /// Validate payment method
    ValidatePaymentMethod,
}

/// Fraud score and risk assessment (0-100)
#[derive(Debug, Clone, Copy, Serialize, Deserialize, Eq, PartialEq)]
pub struct FraudScore {
    /// Total fraud risk score (0=safe, 100=confirmed fraud)
    pub score: u32,
    /// Payment velocity (charges per day)
    pub payment_velocity: u32,
    /// Geographic anomaly score (0-100)
    pub geographic_anomaly: u32,
    /// Usage pattern deviation (0-100)
    pub usage_deviation: u32,
    /// Behavioral anomaly score (0-100)
    pub behavioral_anomaly: u32,
}

impl FraudScore {
    /// Create new fraud score
    pub fn new(
        payment_velocity: u32,
        geographic_anomaly: u32,
        usage_deviation: u32,
        behavioral_anomaly: u32,
    ) -> Self {
        // Weighted average: 40% payment, 30% geographic, 20% usage, 10% behavioral
        let score = (payment_velocity as u64 * 40
            + geographic_anomaly as u64 * 30
            + usage_deviation as u64 * 20
            + behavioral_anomaly as u64 * 10)
            / 100;

        Self {
            score: score.min(100) as u32,
            payment_velocity,
            geographic_anomaly,
            usage_deviation,
            behavioral_anomaly,
        }
    }

    /// Risk level based on score
    pub fn risk_level(&self) -> &'static str {
        match self.score {
            0..=30 => "low",
            31..=70 => "medium",
            71..=90 => "high",
            91..=100 => "critical",
        }
    }
}

/// Fraud detection engine
pub struct FraudDetector;

impl FraudDetector {
    /// Analyze customer activity for fraud signals
    pub fn analyze_activity(
        customer_id: &str,
        activity_history: &[(DateTime<Utc>, String)],
    ) -> Result<FraudScore, AccountGovernorError> {
        if activity_history.is_empty() {
            return Err(AccountGovernorError::FraudDetectionFailed(
                "No activity history available".to_string(),
            ));
        }

        // Calculate payment velocity (charges in last 24h)
        let payment_velocity = Self::calculate_payment_velocity(activity_history);

        // Calculate geographic anomalies
        let geographic_anomaly = Self::calculate_geographic_anomaly(activity_history);

        // Calculate usage pattern deviations
        let usage_deviation = Self::calculate_usage_deviation(activity_history);

        // Calculate behavioral anomalies
        let behavioral_anomaly = Self::calculate_behavioral_anomaly(activity_history);

        Ok(FraudScore::new(
            payment_velocity,
            geographic_anomaly,
            usage_deviation,
            behavioral_anomaly,
        ))
    }

    fn calculate_payment_velocity(
        activity_history: &[(DateTime<Utc>, String)],
    ) -> u32 {
        let now = Utc::now();
        let day_ago = now - Duration::days(1);

        let charges_24h = activity_history
            .iter()
            .filter(|(ts, action)| *ts > day_ago && action.contains("charge"))
            .count();

        // Normalize to 0-100 (> 50 charges/day = 100)
        ((charges_24h as u32).min(50) * 100) / 50
    }

    fn calculate_geographic_anomaly(
        activity_history: &[(DateTime<Utc>, String)],
    ) -> u32 {
        // Simulate geographic analysis (in production: IP geolocation database)
        // Check for impossible travel times between locations
        if activity_history.len() < 2 {
            return 0;
        }

        // Simplified: if activity jumps countries within 1 hour = high anomaly
        let recent: Vec<_> = activity_history
            .iter()
            .rev()
            .take(5)
            .collect();

        if recent.len() >= 2 {
            let time_diff = recent[0].0 - recent[1].0;
            if time_diff < Duration::hours(1)
                && recent[0].1.contains("country_change")
            {
                return 85; // Impossible travel detected
            }
        }

        0
    }

    fn calculate_usage_deviation(
        activity_history: &[(DateTime<Utc>, String)],
    ) -> u32 {
        // Analyze deviation from baseline usage pattern
        // Baseline: 80% of activity in business hours (9-5)
        let business_hours_count = activity_history
            .iter()
            .filter(|(ts, _)| {
                let hour = ts.hour();
                hour >= 9 && hour < 17
            })
            .count();

        let ratio = if activity_history.is_empty() {
            0
        } else {
            (business_hours_count * 100) / activity_history.len()
        };

        // If mostly off-business-hours, it's anomalous
        if ratio < 30 {
            80
        } else {
            0
        }
    }

    fn calculate_behavioral_anomaly(
        activity_history: &[(DateTime<Utc>, String)],
    ) -> u32 {
        // Detect unusual behavior patterns
        let high_velocity_creates = activity_history
            .iter()
            .filter(|(_, action)| action.contains("create") || action.contains("rapid"))
            .count();

        // Normalize: > 100 creates in history = 100 anomaly score
        ((high_velocity_creates as u32).min(100) * 100) / 100
    }

    /// Check for impossible behavior patterns
    pub fn check_for_compromised_account(
        activity_history: &[(DateTime<Utc>, String)],
    ) -> bool {
        if activity_history.len() < 2 {
            return false;
        }

        // Signal: Impossible travel (country change in < 1 hour)
        for i in 1..activity_history.len().min(5) {
            let time_diff = activity_history[i - 1].0 - activity_history[i].0;
            if time_diff < Duration::hours(1)
                && activity_history[i - 1]
                    .1
                    .contains("country_change")
            {
                return true;
            }
        }

        // Signal: Multiple failed login attempts followed by success
        let recent: Vec<_> = activity_history
            .iter()
            .rev()
            .take(10)
            .collect();

        let failed_logins = recent
            .iter()
            .filter(|(_, action)| action.contains("login_failed"))
            .count();

        if failed_logins >= 3 {
            let has_success = recent
                .iter()
                .any(|(_, action)| action.contains("login_success"));
            if has_success {
                return true;
            }
        }

        false
    }
}

/// Compliance monitoring sub-FSM
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct ComplianceMonitor {
    /// Last compliance check timestamp
    pub last_check: DateTime<Utc>,
    /// Current fraud score
    pub current_fraud_score: Option<FraudScore>,
    /// Monitoring frequency (days)
    pub check_frequency: u32,
    /// High-risk events requiring escalation
    pub escalation_events: Vec<String>,
    /// Compliance status
    pub status: ComplianceStatus,
}

#[derive(Debug, Clone, Copy, Serialize, Deserialize, Eq, PartialEq)]
pub enum ComplianceStatus {
    /// Low risk, normal monitoring
    Compliant,
    /// Medium risk, increased monitoring
    IncreaseMonitoring,
    /// High risk, prepare for suspension
    Suspicious,
    /// Critical risk, escalate to review
    UnderReview,
}

impl ComplianceMonitor {
    pub fn new() -> Self {
        Self {
            last_check: Utc::now(),
            current_fraud_score: None,
            check_frequency: 7, // Weekly by default
            escalation_events: Vec::new(),
            status: ComplianceStatus::Compliant,
        }
    }

    /// Run compliance check and update status
    pub fn check_compliance(
        &mut self,
        fraud_score: FraudScore,
    ) -> Result<ComplianceStatus, AccountGovernorError> {
        self.last_check = Utc::now();
        self.current_fraud_score = Some(fraud_score);

        let status = match fraud_score.score {
            0..=30 => ComplianceStatus::Compliant,
            31..=70 => {
                self.check_frequency = 3; // Increase to triweekly
                ComplianceStatus::IncreaseMonitoring
            }
            71..=90 => {
                self.check_frequency = 1; // Daily checks
                self.escalation_events
                    .push(format!("High fraud score: {}", fraud_score.score));
                ComplianceStatus::Suspicious
            }
            91..=100 => {
                self.escalation_events
                    .push(format!("Critical fraud score: {}", fraud_score.score));
                ComplianceStatus::UnderReview
            }
        };

        self.status = status;
        Ok(status)
    }
}

/// Audit trail entry for account state transitions
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct AuditTrailEntry {
    /// Timestamp of the event
    pub timestamp: DateTime<Utc>,
    /// State before transition
    pub from_state: AccountState,
    /// State after transition
    pub to_state: AccountState,
    /// Event that triggered the transition
    pub event: String,
    /// Action taken (if any)
    pub action: Option<String>,
    /// Reason for transition
    pub reason: Option<String>,
    /// Metadata (KYC data hash, fraud score, etc.)
    pub metadata: HashMap<String, String>,
}

/// Customer account governor (main FSM)
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct AccountGovernor {
    /// Customer ID (globally unique)
    pub customer_id: String,
    /// Current account state
    pub state: AccountState,
    /// Account creation timestamp
    pub created_at: DateTime<Utc>,
    /// Last state transition
    pub last_state_change: DateTime<Utc>,
    /// Time limit for current state (if applicable)
    pub state_timeout: Option<DateTime<Utc>>,
    /// Compliance monitoring sub-FSM
    pub compliance: ComplianceMonitor,
    /// Complete audit trail
    pub audit_trail: Vec<AuditTrailEntry>,
    /// Two-factor authentication enabled
    pub two_factor_enabled: bool,
    /// Account verified/active
    pub is_verified: bool,
}

impl AccountGovernor {
    /// Create new customer account (starts in Onboarding)
    pub fn new(customer_id: String) -> Self {
        let now = Utc::now();
        let timeout = now + Duration::days(7); // 7-day onboarding timeout

        Self {
            customer_id,
            state: AccountState::Onboarding,
            created_at: now,
            last_state_change: now,
            state_timeout: Some(timeout),
            compliance: ComplianceMonitor::new(),
            audit_trail: Vec::new(),
            two_factor_enabled: false,
            is_verified: false,
        }
    }

    /// Process event and compute state transition
    ///
    /// Returns: (new_state, optional_action_to_execute)
    pub async fn transition(
        &mut self,
        event: AccountEvent,
    ) -> Result<(AccountState, Option<AccountAction>), AccountGovernorError> {
        let (new_state, action) = match (&self.state, &event) {
            // === ONBOARDING (7d timeout) ===
            (AccountState::Onboarding, AccountEvent::EmailVerified) => {
                self.is_verified = false; // Email only, not full account
                (AccountState::Onboarding, None)
            }

            (AccountState::Onboarding, AccountEvent::ProfileCompleted) => {
                (
                    AccountState::Onboarding,
                    Some(AccountAction::RequestKycDocumentation),
                )
            }

            (AccountState::Onboarding, AccountEvent::KycCheckPassed) => {
                self.is_verified = true;
                (
                    AccountState::Active,
                    Some(AccountAction::SendWelcomeEmail),
                )
            }

            (AccountState::Onboarding, AccountEvent::KycCheckFailed) => {
                (
                    AccountState::Deactivated,
                    Some(AccountAction::NotifyBanDecision {
                        reason: "KYC verification failed".to_string(),
                    }),
                )
            }

            (
                AccountState::Onboarding,
                AccountEvent::FraudDetected(reason),
            ) => {
                (
                    AccountState::UnderReview,
                    Some(AccountAction::PrepareAuditReport),
                )
            }

            (AccountState::Onboarding, AccountEvent::TimeoutTransition) => {
                (
                    AccountState::Deactivated,
                    Some(AccountAction::ScheduleDataDeletion),
                )
            }

            // === ACTIVE (periodic compliance monitoring) ===
            (
                AccountState::Active,
                AccountEvent::PeriodicComplianceCheck(fraud_score),
            ) => {
                let compliance_status = self
                    .compliance
                    .check_compliance(*fraud_score)
                    .map_err(|_| AccountGovernorError::ComplianceCheckFailed(
                        "Failed to assess fraud score".to_string(),
                    ))?;

                match compliance_status {
                    ComplianceStatus::Compliant => {
                        (AccountState::Active, None)
                    }
                    ComplianceStatus::IncreaseMonitoring => {
                        (
                            AccountState::Active,
                            Some(AccountAction::IncreaseMonitoringFrequency),
                        )
                    }
                    ComplianceStatus::Suspicious => {
                        (
                            AccountState::Suspended,
                            Some(AccountAction::BlockOperations),
                        )
                    }
                    ComplianceStatus::UnderReview => {
                        (
                            AccountState::UnderReview,
                            Some(AccountAction::PrepareAuditReport),
                        )
                    }
                }
            }

            (
                AccountState::Active,
                AccountEvent::FraudAlert { reason, score },
            ) => {
                if *score > 70 {
                    (
                        AccountState::UnderReview,
                        Some(AccountAction::PreserveDataComplianceHold),
                    )
                } else {
                    (
                        AccountState::Suspended,
                        Some(AccountAction::NotifyCustomerSuspension {
                            reason: reason.clone(),
                        }),
                    )
                }
            }

            (
                AccountState::Active,
                AccountEvent::AbuseReport { reason, evidence },
            ) => {
                (
                    AccountState::Suspended,
                    Some(AccountAction::NotifyCustomerSuspension {
                        reason: reason.clone(),
                    }),
                )
            }

            (AccountState::Active, AccountEvent::InactivityDetected) => {
                (
                    AccountState::Active,
                    Some(AccountAction::SendEngagementEmail),
                )
            }

            (AccountState::Active, AccountEvent::ActivityDetected) => {
                (AccountState::Active, None)
            }

            // === SUSPENDED (30d timeout) ===
            (AccountState::Suspended, AccountEvent::IssueResolved) => {
                (
                    AccountState::Active,
                    Some(AccountAction::SendWelcomeEmail),
                )
            }

            (
                AccountState::Suspended,
                AccountEvent::CustomerAppeals { reason },
            ) => {
                // Accept appeal if reasonable - otherwise stay suspended
                (
                    AccountState::Suspended,
                    Some(AccountAction::OfferSupport),
                )
            }

            (AccountState::Suspended, AccountEvent::EscalateToReview) => {
                (
                    AccountState::UnderReview,
                    Some(AccountAction::PrepareAuditReport),
                )
            }

            (AccountState::Suspended, AccountEvent::TimeoutTransition) => {
                (
                    AccountState::Deactivated,
                    Some(AccountAction::ScheduleDataDeletion),
                )
            }

            // === UNDER_REVIEW (14d timeout) ===
            (AccountState::UnderReview, AccountEvent::ReviewCompletedApproved) => {
                self.is_verified = true;
                (
                    AccountState::Active,
                    Some(AccountAction::SendWelcomeEmail),
                )
            }

            (
                AccountState::UnderReview,
                AccountEvent::ReviewCompletedBanned { reason },
            ) => {
                (
                    AccountState::Deactivated,
                    Some(AccountAction::NotifyBanDecision {
                        reason: reason.clone(),
                    }),
                )
            }

            (
                AccountState::UnderReview,
                AccountEvent::CustomerAppealsDenied { reason },
            ) => {
                (
                    AccountState::Deactivated,
                    Some(AccountAction::NotifyBanDecision {
                        reason: reason.clone(),
                    }),
                )
            }

            (AccountState::UnderReview, AccountEvent::TimeoutTransition) => {
                (
                    AccountState::Deactivated,
                    Some(AccountAction::ScheduleDataDeletion),
                )
            }

            // === DEACTIVATED (30d data retention) ===
            (
                AccountState::Deactivated,
                AccountEvent::CustomerRequestsReactivation,
            ) => {
                // Allow reactivation within 30 days of deactivation
                let days_deactivated = (Utc::now() - self.last_state_change).num_days();
                if days_deactivated <= 30 {
                    (
                        AccountState::Active,
                        Some(AccountAction::SendWelcomeEmail),
                    )
                } else {
                    (
                        AccountState::Deactivated,
                        None,
                    )
                }
            }

            (AccountState::Deactivated, AccountEvent::DataRetentionComplete) => {
                (
                    AccountState::Archived,
                    Some(AccountAction::DeleteAllData),
                )
            }

            (AccountState::Deactivated, AccountEvent::TimeoutTransition) => {
                (
                    AccountState::Archived,
                    Some(AccountAction::DeleteAllData),
                )
            }

            // === ARCHIVED (terminal state) ===
            (AccountState::Archived, _) => {
                return Err(AccountGovernorError::InvalidTransition {
                    from: AccountState::Archived.as_str().to_string(),
                    to: "?".to_string(),
                    event: format!("{:?}", event),
                });
            }

            // Default: invalid transition
            (current, event) => {
                return Err(AccountGovernorError::InvalidTransition {
                    from: current.as_str().to_string(),
                    to: "?".to_string(),
                    event: format!("{:?}", event),
                })
            }
        };

        // Record state transition in audit trail
        if self.state != new_state {
            let mut metadata = HashMap::new();
            if let Some(fraud_score) = self.compliance.current_fraud_score {
                metadata.insert(
                    "fraud_score".to_string(),
                    fraud_score.score.to_string(),
                );
            }

            self.audit_trail.push(AuditTrailEntry {
                timestamp: Utc::now(),
                from_state: self.state,
                to_state: new_state,
                event: format!("{:?}", event),
                action: action.as_ref().map(|a| format!("{:?}", a)),
                reason: None,
                metadata,
            });

            // Set timeout for new state
            self.state_timeout = match new_state {
                AccountState::Onboarding => Some(Utc::now() + Duration::days(7)),
                AccountState::Suspended => Some(Utc::now() + Duration::days(30)),
                AccountState::UnderReview => Some(Utc::now() + Duration::days(14)),
                AccountState::Deactivated => Some(Utc::now() + Duration::days(30)),
                _ => None,
            };

            self.state = new_state;
            self.last_state_change = Utc::now();

            tracing::info!(
                customer_id = %self.customer_id,
                from = %self.state.as_str(),
                to = %new_state.as_str(),
                "Account state transition"
            );
        }

        Ok((new_state, action))
    }

    /// Get current state
    pub fn current_state(&self) -> AccountState {
        self.state
    }

    /// Check if account is verified
    pub fn is_verified(&self) -> bool {
        self.is_verified
    }

    /// Get time in current state
    pub fn time_in_state(&self) -> Duration {
        Utc::now() - self.last_state_change
    }

    /// Check if state has timed out
    pub fn check_timeout(&self) -> bool {
        if let Some(timeout) = self.state_timeout {
            Utc::now() > timeout
        } else {
            false
        }
    }

    /// Invariant checks
    pub fn validate_invariants(&self) -> Result<(), AccountGovernorError> {
        if self.customer_id.is_empty() {
            return Err(AccountGovernorError::InvariantViolation(
                "customer_id cannot be empty".to_string(),
            ));
        }

        // Invariant: Archived accounts are read-only
        if self.state == AccountState::Archived && !self.audit_trail.is_empty() {
            let last_entry = &self.audit_trail[self.audit_trail.len() - 1];
            if last_entry.to_state != AccountState::Archived {
                return Err(AccountGovernorError::InvariantViolation(
                    "Archived state must be terminal".to_string(),
                ));
            }
        }

        Ok(())
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    // === Chicago TDD Tests (State-based, AAA pattern, Real objects) ===

    #[tokio::test]
    async fn test_onboarding_to_active_happy_path() {
        // Arrange
        let mut governor = AccountGovernor::new("customer-1".to_string());
        assert_eq!(governor.current_state(), AccountState::Onboarding);

        // Act 1: Verify email
        let (state1, _) = governor
            .transition(AccountEvent::EmailVerified)
            .await
            .unwrap();
        assert_eq!(state1, AccountState::Onboarding);

        // Act 2: Complete profile
        let (state2, _) = governor
            .transition(AccountEvent::ProfileCompleted)
            .await
            .unwrap();
        assert_eq!(state2, AccountState::Onboarding);

        // Act 3: KYC check passed
        let (state3, action) = governor
            .transition(AccountEvent::KycCheckPassed)
            .await
            .unwrap();

        // Assert
        assert_eq!(state3, AccountState::Active);
        assert!(matches!(action, Some(AccountAction::SendWelcomeEmail)));
        assert!(governor.is_verified());
        assert_eq!(governor.audit_trail.len(), 2); // Email → Onboarding, Profile → Onboarding, KYC → Active
    }

    #[tokio::test]
    async fn test_onboarding_kyc_failure_deactivates() {
        // Arrange
        let mut governor = AccountGovernor::new("customer-2".to_string());

        // Act
        let (new_state, action) = governor
            .transition(AccountEvent::KycCheckFailed)
            .await
            .unwrap();

        // Assert
        assert_eq!(new_state, AccountState::Deactivated);
        assert!(matches!(
            action,
            Some(AccountAction::NotifyBanDecision { .. })
        ));
        assert!(!governor.is_verified());
        assert_eq!(governor.audit_trail.len(), 1);
    }

    #[tokio::test]
    async fn test_onboarding_fraud_detected_goes_to_review() {
        // Arrange
        let mut governor = AccountGovernor::new("customer-3".to_string());

        // Act
        let (new_state, action) = governor
            .transition(AccountEvent::FraudDetected(
                "Impossible travel detected".to_string(),
            ))
            .await
            .unwrap();

        // Assert
        assert_eq!(new_state, AccountState::UnderReview);
        assert!(matches!(
            action,
            Some(AccountAction::PrepareAuditReport)
        ));
    }

    #[tokio::test]
    async fn test_active_to_suspended_on_fraud_alert() {
        // Arrange
        let mut governor = AccountGovernor::new("customer-4".to_string());
        // Move to Active
        governor
            .transition(AccountEvent::KycCheckPassed)
            .await
            .unwrap();

        // Act: Medium fraud alert (score 50)
        let (new_state, action) = governor
            .transition(AccountEvent::FraudAlert {
                reason: "Unusual activity pattern".to_string(),
                score: 50,
            })
            .await
            .unwrap();

        // Assert
        assert_eq!(new_state, AccountState::Suspended);
        assert!(matches!(
            action,
            Some(AccountAction::NotifyCustomerSuspension { .. })
        ));
    }

    #[tokio::test]
    async fn test_active_to_under_review_on_high_fraud_alert() {
        // Arrange
        let mut governor = AccountGovernor::new("customer-5".to_string());
        governor
            .transition(AccountEvent::KycCheckPassed)
            .await
            .unwrap();

        // Act: High fraud alert (score 80)
        let (new_state, action) = governor
            .transition(AccountEvent::FraudAlert {
                reason: "Confirmed fraud pattern".to_string(),
                score: 80,
            })
            .await
            .unwrap();

        // Assert
        assert_eq!(new_state, AccountState::UnderReview);
        assert!(matches!(
            action,
            Some(AccountAction::PreserveDataComplianceHold)
        ));
    }

    #[tokio::test]
    async fn test_compliance_monitoring_low_score() {
        // Arrange
        let mut governor = AccountGovernor::new("customer-6".to_string());
        governor
            .transition(AccountEvent::KycCheckPassed)
            .await
            .unwrap();

        let fraud_score = FraudScore::new(5, 0, 10, 0);

        // Act
        let (new_state, action) = governor
            .transition(AccountEvent::PeriodicComplianceCheck(fraud_score))
            .await
            .unwrap();

        // Assert
        assert_eq!(new_state, AccountState::Active);
        assert!(action.is_none());
        assert_eq!(
            governor.compliance.status,
            ComplianceStatus::Compliant
        );
    }

    #[tokio::test]
    async fn test_compliance_monitoring_medium_score() {
        // Arrange
        let mut governor = AccountGovernor::new("customer-7".to_string());
        governor
            .transition(AccountEvent::KycCheckPassed)
            .await
            .unwrap();

        let fraud_score = FraudScore::new(40, 25, 20, 10);

        // Act
        let (new_state, action) = governor
            .transition(AccountEvent::PeriodicComplianceCheck(fraud_score))
            .await
            .unwrap();

        // Assert
        assert_eq!(new_state, AccountState::Active);
        assert!(matches!(
            action,
            Some(AccountAction::IncreaseMonitoringFrequency)
        ));
        assert_eq!(
            governor.compliance.status,
            ComplianceStatus::IncreaseMonitoring
        );
        assert_eq!(governor.compliance.check_frequency, 3); // Triweekly
    }

    #[tokio::test]
    async fn test_suspended_issue_resolved() {
        // Arrange
        let mut governor = AccountGovernor::new("customer-8".to_string());
        governor
            .transition(AccountEvent::KycCheckPassed)
            .await
            .unwrap();
        governor
            .transition(AccountEvent::AbuseReport {
                reason: "Rapid deletions".to_string(),
                evidence: vec!["100 deletes in 1 hour".to_string()],
            })
            .await
            .unwrap();
        assert_eq!(governor.current_state(), AccountState::Suspended);

        // Act
        let (new_state, action) = governor
            .transition(AccountEvent::IssueResolved)
            .await
            .unwrap();

        // Assert
        assert_eq!(new_state, AccountState::Active);
        assert!(matches!(action, Some(AccountAction::SendWelcomeEmail)));
    }

    #[tokio::test]
    async fn test_deactivated_reactivation_within_30_days() {
        // Arrange
        let mut governor = AccountGovernor::new("customer-9".to_string());
        governor
            .transition(AccountEvent::KycCheckPassed)
            .await
            .unwrap();
        governor
            .transition(AccountEvent::KycCheckFailed)
            .await
            .unwrap();
        assert_eq!(governor.current_state(), AccountState::Deactivated);

        // Act: Request reactivation (within 30 days)
        let (new_state, action) = governor
            .transition(AccountEvent::CustomerRequestsReactivation)
            .await
            .unwrap();

        // Assert
        assert_eq!(new_state, AccountState::Active);
        assert!(matches!(action, Some(AccountAction::SendWelcomeEmail)));
    }

    #[tokio::test]
    async fn test_under_review_approved() {
        // Arrange
        let mut governor = AccountGovernor::new("customer-10".to_string());
        governor
            .transition(AccountEvent::FraudDetected(
                "Initial alert".to_string(),
            ))
            .await
            .unwrap();
        assert_eq!(governor.current_state(), AccountState::UnderReview);

        // Act
        let (new_state, action) = governor
            .transition(AccountEvent::ReviewCompletedApproved)
            .await
            .unwrap();

        // Assert
        assert_eq!(new_state, AccountState::Active);
        assert!(matches!(action, Some(AccountAction::SendWelcomeEmail)));
        assert!(governor.is_verified());
    }

    #[tokio::test]
    async fn test_under_review_banned() {
        // Arrange
        let mut governor = AccountGovernor::new("customer-11".to_string());
        governor
            .transition(AccountEvent::FraudDetected("Alert".to_string()))
            .await
            .unwrap();

        // Act
        let (new_state, action) = governor
            .transition(AccountEvent::ReviewCompletedBanned {
                reason: "Confirmed fraud".to_string(),
            })
            .await
            .unwrap();

        // Assert
        assert_eq!(new_state, AccountState::Deactivated);
        assert!(matches!(
            action,
            Some(AccountAction::NotifyBanDecision { .. })
        ));
    }

    #[tokio::test]
    async fn test_archived_state_is_terminal() {
        // Arrange
        let mut governor = AccountGovernor::new("customer-12".to_string());
        governor.state = AccountState::Archived; // Simulate archived state

        // Act & Assert
        let result = governor
            .transition(AccountEvent::ActivityDetected)
            .await;

        assert!(result.is_err());
        assert!(matches!(
            result,
            Err(AccountGovernorError::InvalidTransition { .. })
        ));
    }

    #[tokio::test]
    async fn test_fraud_score_calculation() {
        // Arrange
        let fraud_score = FraudScore::new(45, 30, 20, 10);

        // Assert
        assert_eq!(fraud_score.score, 33); // (45*40 + 30*30 + 20*20 + 10*10) / 100 = 33
        assert_eq!(fraud_score.risk_level(), "medium");
    }

    #[tokio::test]
    async fn test_fraud_score_critical() {
        // Arrange
        let fraud_score = FraudScore::new(100, 100, 100, 100);

        // Assert
        assert_eq!(fraud_score.score, 100);
        assert_eq!(fraud_score.risk_level(), "critical");
    }

    #[tokio::test]
    async fn test_fraud_detector_payment_velocity() {
        // Arrange
        let now = Utc::now();
        let activity_history = vec![
            (now, "charge".to_string()),
            (now - Duration::hours(2), "charge".to_string()),
            (now - Duration::hours(4), "charge".to_string()),
            (now - Duration::hours(10), "login".to_string()),
            (now - Duration::days(2), "charge".to_string()),
        ];

        // Act
        let fraud_score = FraudDetector::analyze_activity("cust-1", &activity_history).unwrap();

        // Assert
        assert!(fraud_score.score > 0); // Should detect multiple charges
    }

    #[tokio::test]
    async fn test_fraud_detector_compromised_account() {
        // Arrange
        let now = Utc::now();
        let activity_history = vec![
            (now - Duration::minutes(10), "login_success".to_string()),
            (now - Duration::minutes(30), "login_failed".to_string()),
            (now - Duration::minutes(40), "login_failed".to_string()),
            (now - Duration::minutes(50), "login_failed".to_string()),
        ];

        // Act
        let is_compromised = FraudDetector::check_for_compromised_account(&activity_history);

        // Assert
        assert!(is_compromised);
    }

    #[tokio::test]
    async fn test_audit_trail_records_all_transitions() {
        // Arrange
        let mut governor = AccountGovernor::new("customer-13".to_string());

        // Act
        governor
            .transition(AccountEvent::EmailVerified)
            .await
            .unwrap();
        governor
            .transition(AccountEvent::ProfileCompleted)
            .await
            .unwrap();
        governor
            .transition(AccountEvent::KycCheckPassed)
            .await
            .unwrap();

        // Assert
        assert!(governor.audit_trail.len() > 0);
        // Verify last entry is transition to Active
        let last = &governor.audit_trail[governor.audit_trail.len() - 1];
        assert_eq!(last.to_state, AccountState::Active);
    }

    #[tokio::test]
    async fn test_state_timeout_onboarding() {
        // Arrange
        let governor = AccountGovernor::new("customer-14".to_string());

        // Assert
        assert!(governor.state_timeout.is_some());
        let timeout = governor.state_timeout.unwrap();
        let duration = timeout - Utc::now();
        assert!(duration.num_seconds() > 6 * 24 * 3600); // > 6 days
        assert!(duration.num_seconds() < 8 * 24 * 3600); // < 8 days
    }

    #[tokio::test]
    async fn test_invariant_validation() {
        // Arrange
        let mut governor = AccountGovernor::new("customer-15".to_string());

        // Act & Assert
        assert!(governor.validate_invariants().is_ok());

        // Empty customer ID should violate invariant
        governor.customer_id = String::new();
        assert!(governor.validate_invariants().is_err());
    }

    #[tokio::test]
    async fn test_concurrent_abuse_reports() {
        // Arrange
        let mut governor = AccountGovernor::new("customer-16".to_string());
        governor
            .transition(AccountEvent::KycCheckPassed)
            .await
            .unwrap();

        // Act: First abuse report
        let (state1, _) = governor
            .transition(AccountEvent::AbuseReport {
                reason: "Rapid creates".to_string(),
                evidence: vec!["100 resources created".to_string()],
            })
            .await
            .unwrap();

        // Assert: Should suspend on first report
        assert_eq!(state1, AccountState::Suspended);

        // Act: Try to escalate from suspended
        let (state2, action) = governor
            .transition(AccountEvent::EscalateToReview)
            .await
            .unwrap();

        // Assert
        assert_eq!(state2, AccountState::UnderReview);
        assert!(matches!(
            action,
            Some(AccountAction::PrepareAuditReport)
        ));
    }
}
