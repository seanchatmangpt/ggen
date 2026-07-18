//! Billing & Payment Governor - Financial FSM orchestrator
//!
//! This module implements the **Plan** phase of MAPE-K for billing and payment management
//! with a comprehensive state machine handling:
//! - Monthly billing cycles and invoice generation
//! - Payment processing with retry logic
//! - Failed payment escalation to collections
//! - Multi-currency support with tax calculations
//! - Duplicate payment prevention (idempotency)
//! - GCP Billing API integration
//!
//! ## State Machine Diagram
//!
//! ```
//! awaiting_invoice
//!   └─ invoice_generation_time_reached ──→ invoice_issued
//!
//! invoice_issued
//!   └─ customer_received ──→ payment_pending
//!
//! payment_pending
//!   ├─ payment_received ──→ payment_received
//!   ├─ payment_failed ──→ payment_failed
//!   ├─ payment_dispute ──→ payment_disputed
//!   └─ [7 day timeout] ──→ payment_failed
//!
//! payment_received
//!   ├─ receipt_logged ──→ payment_received
//!   ├─ accounting_reconciled ──→ archived
//!   └─ (terminal after reconciliation)
//!
//! payment_failed
//!   ├─ retry_approved ──→ retry_1
//!   ├─ escalate_to_admin ──→ payment_disputed
//!   ├─ [2 day timeout] ──→ retry_1
//!   └─ customer_disputes ──→ payment_disputed
//!
//! retry_1 (1 day, backup payment method)
//!   ├─ payment_received ──→ payment_received
//!   ├─ payment_failed ──→ retry_2
//!   └─ [1 day timeout] ──→ retry_2
//!
//! retry_2 (3 days, secondary backup method)
//!   ├─ payment_received ──→ payment_received
//!   ├─ payment_failed ──→ retry_3
//!   └─ [3 day timeout] ──→ retry_3
//!
//! retry_3 (7 days, last attempt)
//!   ├─ payment_received ──→ payment_received
//!   ├─ payment_failed ──→ collection_agency
//!   └─ [7 day timeout] ──→ collection_agency
//!
//! collection_agency
//!   ├─ payment_recovered ──→ archived
//!   ├─ account_written_off ──→ archived
//!   ├─ settlement_agreed ──→ archived
//!   └─ (terminal, manual review required)
//!
//! payment_disputed
//!   ├─ dispute_resolved ──→ archived
//!   ├─ refund_issued ──→ archived
//!   └─ (terminal after resolution)
//! ```

use crate::receipt::ReceiptLedger;
use thiserror::Error;
use serde::{Deserialize, Serialize};
use chrono::{DateTime, Utc, Duration};
use sha2::{Sha256, Digest};

/// Billing governor errors
#[derive(Debug, Error)]
pub enum BillingGovernorError {
    #[error("Invalid state transition: {from} → {to} with event {event}")]
    InvalidTransition {
        from: String,
        to: String,
        event: String,
    },

    #[error("Invariant violation: {0}")]
    InvariantViolation(String),

    #[error("Payment processing failed: {0}")]
    PaymentProcessingFailed(String),

    #[error("GCP API error: {0}")]
    GcpApiError(String),

    #[error("Invoice generation failed: {0}")]
    InvoiceGenerationFailed(String),

    #[error("Duplicate payment detected: {idempotency_key}")]
    DuplicatePayment { idempotency_key: String },

    #[error("Payment amount mismatch: expected {expected}, got {actual}")]
    PaymentAmountMismatch { expected: f64, actual: f64 },

    #[error("Concurrent payment detected: invoice {invoice_id}")]
    ConcurrentPayment { invoice_id: String },

    #[error("Invalid currency: {0}")]
    InvalidCurrency(String),

    #[error("Tax calculation failed: {0}")]
    TaxCalculationFailed(String),

    #[error("Receipt generation failed: {0}")]
    ReceiptGenerationFailed(String),
}

/// Billing FSM states
#[derive(Debug, Clone, Copy, Serialize, Deserialize, Eq, PartialEq, Hash)]
pub enum BillingState {
    /// Monthly billing cycle begins, awaiting invoice generation
    AwaitingInvoice,
    /// Invoice created and sent to customer
    InvoiceIssued,
    /// Waiting for payment (7-day window)
    PaymentPending,
    /// Payment received and logged
    PaymentReceived,
    /// Initial payment attempt failed
    PaymentFailed,
    /// First retry attempt (1 day)
    Retry1,
    /// Second retry attempt (3 days)
    Retry2,
    /// Third retry attempt (7 days)
    Retry3,
    /// Escalated to collections agency
    CollectionAgency,
    /// Payment disputed, awaiting resolution
    PaymentDisputed,
    /// Final archived state
    Archived,
}

impl BillingState {
    fn as_str(&self) -> &str {
        match self {
            BillingState::AwaitingInvoice => "AwaitingInvoice",
            BillingState::InvoiceIssued => "InvoiceIssued",
            BillingState::PaymentPending => "PaymentPending",
            BillingState::PaymentReceived => "PaymentReceived",
            BillingState::PaymentFailed => "PaymentFailed",
            BillingState::Retry1 => "Retry1",
            BillingState::Retry2 => "Retry2",
            BillingState::Retry3 => "Retry3",
            BillingState::CollectionAgency => "CollectionAgency",
            BillingState::PaymentDisputed => "PaymentDisputed",
            BillingState::Archived => "Archived",
        }
    }

    fn is_terminal(&self) -> bool {
        matches!(self, BillingState::Archived)
    }
}

/// Payment methods
#[derive(Debug, Clone, Serialize, Deserialize, Eq, PartialEq, Hash)]
pub enum PaymentMethod {
    /// Credit card (last 4 digits only in logs)
    CreditCard(String),
    /// ACH bank transfer
    BankTransfer(String),
    /// Wire transfer
    WireTransfer(String),
    /// Backup payment method (used after initial failure)
    BackupMethod(String),
}

impl PaymentMethod {
    /// Get masked representation for logging
    pub fn masked(&self) -> String {
        match self {
            PaymentMethod::CreditCard(last4) => format!("CC:****{}", last4),
            PaymentMethod::BankTransfer(account) => format!("ACH:****{}", &account[account.len().saturating_sub(4)..]),
            PaymentMethod::WireTransfer(account) => format!("Wire:****{}", &account[account.len().saturating_sub(4)..]),
            PaymentMethod::BackupMethod(method) => format!("Backup:***{}", &method[method.len().saturating_sub(2)..]),
        }
    }
}

/// Events that drive FSM transitions
#[derive(Debug, Clone, Serialize, Deserialize)]
pub enum BillingEvent {
    /// Time to generate invoice (5th of month)
    InvoiceGenerationTimeReached,
    /// Invoice created and customer notified
    InvoiceCreated { invoice_id: String, amount: f64 },
    /// Customer received invoice confirmation
    CustomerReceived,
    /// Payment received from customer
    PaymentReceived {
        payment_id: String,
        amount: f64,
        method: PaymentMethod,
        idempotency_key: String,
    },
    /// Payment processing failed
    PaymentFailed { reason: String },
    /// Retry payment approved
    RetryApproved,
    /// Escalate to admin for review
    EscalateToAdmin,
    /// Payment dispute raised by customer
    PaymentDispute { dispute_id: String },
    /// Dispute resolved
    DisputeResolved { outcome: DisputeOutcome },
    /// Account reconciled with accounting
    AccountingReconciled,
    /// Payment recovered from collections
    PaymentRecovered,
    /// Account written off
    AccountWrittenOff,
    /// Settlement agreement reached
    SettlementAgreed { settlement_amount: f64 },
    /// Manual timeout transition
    TimeoutTransition,
    /// Duplicate payment attempt
    DuplicatePaymentAttempt { idempotency_key: String },
}

/// Payment dispute outcomes
#[derive(Debug, Clone, Serialize, Deserialize)]
pub enum DisputeOutcome {
    /// Customer wins, refund issued
    RefundIssued { amount: f64 },
    /// Merchant wins, payment valid
    PaymentValid,
    /// Partial refund
    PartialRefund { refund_amount: f64, retained_amount: f64 },
}

/// Payment information
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct PaymentInfo {
    pub customer_id: String,
    pub invoice_id: String,
    pub amount: f64,
    pub currency: String,
    pub tax_amount: f64,
    pub due_date: DateTime<Utc>,
    pub payment_method: Option<PaymentMethod>,
    pub last_payment_attempt: Option<DateTime<Utc>>,
    pub failed_attempts: u32,
    pub idempotency_keys: Vec<String>, // For duplicate detection
}

impl PaymentInfo {
    /// Check if payment was already processed (idempotency)
    pub fn is_duplicate(&self, key: &str) -> bool {
        self.idempotency_keys.contains(&key.to_string())
    }

    /// Add idempotency key (tracking)
    pub fn track_attempt(&mut self, key: String) {
        self.idempotency_keys.push(key);
    }
}

/// Billing governor instance (per-invoice coordination)
#[derive(Debug, Clone)]
pub struct BillingGovernor {
    state: BillingState,
    customer_id: String,
    invoice_id: String,
    payment_info: PaymentInfo,
    last_state_change: DateTime<Utc>,
    retry_count: u32,
    dispute_id: Option<String>,
    payment_received_at: Option<DateTime<Utc>>,
    /// Idempotency tracking for concurrent payments
    concurrent_payment_guard: bool,
}

impl BillingGovernor {
    /// Create new billing governor for invoice
    pub fn new(
        customer_id: String,
        invoice_id: String,
        amount: f64,
        currency: String,
    ) -> Result<Self, BillingGovernorError> {
        Self::validate_currency(&currency)?;

        Ok(Self {
            state: BillingState::AwaitingInvoice,
            customer_id: customer_id.clone(),
            invoice_id: invoice_id.clone(),
            payment_info: PaymentInfo {
                customer_id,
                invoice_id,
                amount,
                currency,
                tax_amount: 0.0,
                due_date: Utc::now() + Duration::days(30),
                payment_method: None,
                last_payment_attempt: None,
                failed_attempts: 0,
                idempotency_keys: Vec::new(),
            },
            last_state_change: Utc::now(),
            retry_count: 0,
            dispute_id: None,
            payment_received_at: None,
            concurrent_payment_guard: false,
        })
    }

    /// Validate currency code
    fn validate_currency(currency: &str) -> Result<(), BillingGovernorError> {
        // Simple validation: 3-letter ISO code
        if currency.len() != 3 || !currency.chars().all(|c| c.is_uppercase()) {
            return Err(BillingGovernorError::InvalidCurrency(currency.to_string()));
        }
        Ok(())
    }

    /// Calculate tax amount
    pub fn calculate_tax(&self, tax_rate: f64) -> Result<f64, BillingGovernorError> {
        if !(0.0..=1.0).contains(&tax_rate) {
            return Err(BillingGovernorError::TaxCalculationFailed(
                format!("Tax rate must be 0-1, got {}", tax_rate),
            ));
        }
        Ok(self.payment_info.amount * tax_rate)
    }

    /// Process state transition
    pub async fn transition(
        &mut self,
        event: BillingEvent,
    ) -> Result<(BillingState, Option<BillingAction>), BillingGovernorError> {
        // Prevent transitions from terminal states
        if self.state.is_terminal() && !matches!(event, BillingEvent::TimeoutTransition) {
            return Err(BillingGovernorError::InvalidTransition {
                from: self.state.as_str().to_string(),
                to: "?".to_string(),
                event: format!("{:?}", event),
            });
        }

        let (new_state, action) = match (&self.state, &event) {
            // === AWAITING_INVOICE state ===
            (BillingState::AwaitingInvoice, BillingEvent::InvoiceGenerationTimeReached) => {
                (BillingState::InvoiceIssued, Some(BillingAction::GenerateInvoice))
            }

            (BillingState::AwaitingInvoice, BillingEvent::InvoiceCreated { invoice_id: _, amount }) => {
                Self::check_invoice_invariant(&self.payment_info, *amount)?;
                (BillingState::InvoiceIssued, Some(BillingAction::SendInvoiceEmail))
            }

            // === INVOICE_ISSUED state ===
            (BillingState::InvoiceIssued, BillingEvent::CustomerReceived) => {
                (BillingState::PaymentPending, Some(BillingAction::StartPaymentTimer))
            }

            // === PAYMENT_PENDING state ===
            (BillingState::PaymentPending, BillingEvent::PaymentReceived {
                payment_id,
                amount,
                method,
                idempotency_key,
            }) => {
                // Duplicate payment detection
                if self.payment_info.is_duplicate(&idempotency_key) {
                    return Err(BillingGovernorError::DuplicatePayment {
                        idempotency_key: idempotency_key.clone(),
                    });
                }

                // Concurrent payment detection
                if self.concurrent_payment_guard {
                    return Err(BillingGovernorError::ConcurrentPayment {
                        invoice_id: self.invoice_id.clone(),
                    });
                }

                // Amount validation
                Self::check_payment_amount(&self.payment_info, *amount)?;

                self.concurrent_payment_guard = true;
                self.payment_info.track_attempt(idempotency_key.clone());
                self.payment_info.payment_method = Some(method.clone());
                self.payment_info.last_payment_attempt = Some(Utc::now());
                self.payment_received_at = Some(Utc::now());

                (
                    BillingState::PaymentReceived,
                    Some(BillingAction::ReconcileWithGcp {
                        payment_id: payment_id.clone(),
                        amount: *amount,
                    }),
                )
            }

            (BillingState::PaymentPending, BillingEvent::PaymentFailed { reason: _ }) => {
                self.payment_info.failed_attempts += 1;
                self.payment_info.last_payment_attempt = Some(Utc::now());
                self.retry_count = 0;
                (BillingState::PaymentFailed, Some(BillingAction::NotifyPaymentFailed))
            }

            (BillingState::PaymentPending, BillingEvent::PaymentDispute { dispute_id }) => {
                self.dispute_id = Some(dispute_id.clone());
                (BillingState::PaymentDisputed, Some(BillingAction::LogDisputeStart))
            }

            (BillingState::PaymentPending, BillingEvent::TimeoutTransition) => {
                // 7-day payment window expired
                (BillingState::PaymentFailed, Some(BillingAction::NotifyPaymentFailed))
            }

            // === PAYMENT_RECEIVED state ===
            (BillingState::PaymentReceived, BillingEvent::AccountingReconciled) => {
                self.concurrent_payment_guard = false;
                (BillingState::Archived, Some(BillingAction::LogPaymentComplete))
            }

            // === PAYMENT_FAILED state ===
            (BillingState::PaymentFailed, BillingEvent::RetryApproved) => {
                self.retry_count = 1;
                (BillingState::Retry1, Some(BillingAction::PrepareRetryPayment { attempt: 1 }))
            }

            (BillingState::PaymentFailed, BillingEvent::EscalateToAdmin) => {
                (BillingState::PaymentDisputed, Some(BillingAction::EscalateToSupport))
            }

            (BillingState::PaymentFailed, BillingEvent::PaymentDispute { dispute_id }) => {
                self.dispute_id = Some(dispute_id.clone());
                (BillingState::PaymentDisputed, Some(BillingAction::LogDisputeStart))
            }

            (BillingState::PaymentFailed, BillingEvent::TimeoutTransition) => {
                // 2-day auto-retry after initial failure
                self.retry_count = 1;
                (BillingState::Retry1, Some(BillingAction::PrepareRetryPayment { attempt: 1 }))
            }

            // === RETRY_1 state (1 day, backup method) ===
            (BillingState::Retry1, BillingEvent::PaymentReceived {
                payment_id,
                amount,
                method,
                idempotency_key,
            }) => {
                if self.payment_info.is_duplicate(&idempotency_key) {
                    return Err(BillingGovernorError::DuplicatePayment {
                        idempotency_key: idempotency_key.clone(),
                    });
                }
                Self::check_payment_amount(&self.payment_info, *amount)?;

                self.payment_info.track_attempt(idempotency_key.clone());
                self.payment_info.payment_method = Some(method.clone());
                self.payment_info.last_payment_attempt = Some(Utc::now());
                self.payment_received_at = Some(Utc::now());

                (
                    BillingState::PaymentReceived,
                    Some(BillingAction::ReconcileWithGcp {
                        payment_id: payment_id.clone(),
                        amount: *amount,
                    }),
                )
            }

            (BillingState::Retry1, BillingEvent::PaymentFailed { .. }) => {
                self.payment_info.failed_attempts += 1;
                self.payment_info.last_payment_attempt = Some(Utc::now());
                (BillingState::Retry2, Some(BillingAction::PrepareRetryPayment { attempt: 2 }))
            }

            (BillingState::Retry1, BillingEvent::TimeoutTransition) => {
                // 1-day timeout, advance to retry 2
                (BillingState::Retry2, Some(BillingAction::PrepareRetryPayment { attempt: 2 }))
            }

            // === RETRY_2 state (3 days, secondary backup) ===
            (BillingState::Retry2, BillingEvent::PaymentReceived {
                payment_id,
                amount,
                method,
                idempotency_key,
            }) => {
                if self.payment_info.is_duplicate(&idempotency_key) {
                    return Err(BillingGovernorError::DuplicatePayment {
                        idempotency_key: idempotency_key.clone(),
                    });
                }
                Self::check_payment_amount(&self.payment_info, *amount)?;

                self.payment_info.track_attempt(idempotency_key.clone());
                self.payment_info.payment_method = Some(method.clone());
                self.payment_info.last_payment_attempt = Some(Utc::now());
                self.payment_received_at = Some(Utc::now());

                (
                    BillingState::PaymentReceived,
                    Some(BillingAction::ReconcileWithGcp {
                        payment_id: payment_id.clone(),
                        amount: *amount,
                    }),
                )
            }

            (BillingState::Retry2, BillingEvent::PaymentFailed { .. }) => {
                self.payment_info.failed_attempts += 1;
                self.payment_info.last_payment_attempt = Some(Utc::now());
                (BillingState::Retry3, Some(BillingAction::PrepareRetryPayment { attempt: 3 }))
            }

            (BillingState::Retry2, BillingEvent::TimeoutTransition) => {
                // 3-day timeout, advance to retry 3
                (BillingState::Retry3, Some(BillingAction::PrepareRetryPayment { attempt: 3 }))
            }

            // === RETRY_3 state (7 days, final attempt) ===
            (BillingState::Retry3, BillingEvent::PaymentReceived {
                payment_id,
                amount,
                method,
                idempotency_key,
            }) => {
                if self.payment_info.is_duplicate(&idempotency_key) {
                    return Err(BillingGovernorError::DuplicatePayment {
                        idempotency_key: idempotency_key.clone(),
                    });
                }
                Self::check_payment_amount(&self.payment_info, *amount)?;

                self.payment_info.track_attempt(idempotency_key.clone());
                self.payment_info.payment_method = Some(method.clone());
                self.payment_info.last_payment_attempt = Some(Utc::now());
                self.payment_received_at = Some(Utc::now());

                (
                    BillingState::PaymentReceived,
                    Some(BillingAction::ReconcileWithGcp {
                        payment_id: payment_id.clone(),
                        amount: *amount,
                    }),
                )
            }

            (BillingState::Retry3, BillingEvent::PaymentFailed { .. }) => {
                self.payment_info.failed_attempts += 1;
                self.payment_info.last_payment_attempt = Some(Utc::now());
                (BillingState::CollectionAgency, Some(BillingAction::EscalateToCollections))
            }

            (BillingState::Retry3, BillingEvent::TimeoutTransition) => {
                // 7-day timeout exhausted, escalate to collections
                (BillingState::CollectionAgency, Some(BillingAction::EscalateToCollections))
            }

            // === COLLECTION_AGENCY state ===
            (BillingState::CollectionAgency, BillingEvent::PaymentRecovered) => {
                (BillingState::Archived, Some(BillingAction::LogCollectionSuccess))
            }

            (BillingState::CollectionAgency, BillingEvent::AccountWrittenOff) => {
                (BillingState::Archived, Some(BillingAction::LogAccountWriteOff))
            }

            (BillingState::CollectionAgency, BillingEvent::SettlementAgreed { settlement_amount }) => {
                (
                    BillingState::Archived,
                    Some(BillingAction::LogSettlement {
                        amount: *settlement_amount,
                    }),
                )
            }

            // === PAYMENT_DISPUTED state ===
            (BillingState::PaymentDisputed, BillingEvent::DisputeResolved { outcome }) => {
                let action = match outcome {
                    DisputeOutcome::RefundIssued { amount } => {
                        BillingAction::ProcessRefund { amount: *amount }
                    }
                    DisputeOutcome::PaymentValid => {
                        BillingAction::LogDisputeResolution {
                            outcome: "payment_valid".to_string(),
                        }
                    }
                    DisputeOutcome::PartialRefund { refund_amount, .. } => {
                        BillingAction::ProcessRefund {
                            amount: *refund_amount,
                        }
                    }
                };
                (BillingState::Archived, Some(action))
            }

            // Default: invalid transition
            (current, event) => {
                return Err(BillingGovernorError::InvalidTransition {
                    from: current.as_str().to_string(),
                    to: "?".to_string(),
                    event: format!("{:?}", event),
                })
            }
        };

        let old_state = self.state;
        self.state = new_state;
        self.last_state_change = Utc::now();

        // Log transition
        if old_state != new_state {
            tracing::info!(
                customer = %self.customer_id,
                invoice = %self.invoice_id,
                from = %old_state.as_str(),
                to = %new_state.as_str(),
                "Billing state transition"
            );

            // Emit receipt for audit trail
            let _ = ReceiptLedger::emit(
                &format!("BillingStateTransition:{}", self.invoice_id),
                &format!("{} → {}", old_state.as_str(), new_state.as_str()),
            )
            .await;
        }

        Ok((new_state, action))
    }

    /// Check invoice amount invariant
    fn check_invoice_invariant(
        payment_info: &PaymentInfo,
        amount: f64,
    ) -> Result<(), BillingGovernorError> {
        if (amount - payment_info.amount).abs() > f64::EPSILON {
            return Err(BillingGovernorError::InvariantViolation(
                format!("Invoice amount mismatch: {} vs {}", amount, payment_info.amount),
            ));
        }
        if amount <= 0.0 {
            return Err(BillingGovernorError::InvariantViolation(
                "Invoice amount must be positive".to_string(),
            ));
        }
        Ok(())
    }

    /// Check payment amount matches invoice
    fn check_payment_amount(payment_info: &PaymentInfo, amount: f64) -> Result<(), BillingGovernorError> {
        let tolerance = 0.01; // Allow 1 cent tolerance
        if (amount - payment_info.amount).abs() > tolerance {
            return Err(BillingGovernorError::PaymentAmountMismatch {
                expected: payment_info.amount,
                actual: amount,
            });
        }
        Ok(())
    }

    // Accessors
    pub fn current_state(&self) -> BillingState {
        self.state
    }

    pub fn customer_id(&self) -> &str {
        &self.customer_id
    }

    pub fn invoice_id(&self) -> &str {
        &self.invoice_id
    }

    pub fn payment_info(&self) -> &PaymentInfo {
        &self.payment_info
    }

    pub fn failed_attempts(&self) -> u32 {
        self.payment_info.failed_attempts
    }

    pub fn retry_count(&self) -> u32 {
        self.retry_count
    }

    pub fn time_in_state(&self) -> Duration {
        Utc::now() - self.last_state_change
    }

    pub fn payment_received_at(&self) -> Option<DateTime<Utc>> {
        self.payment_received_at
    }

    pub fn dispute_id(&self) -> Option<&str> {
        self.dispute_id.as_deref()
    }
}

/// Actions to be executed by the actuator
#[derive(Debug, Clone, Serialize, Deserialize)]
pub enum BillingAction {
    /// Generate invoice from GCP Billing API
    GenerateInvoice,
    /// Send invoice email to customer
    SendInvoiceEmail,
    /// Start payment pending timer (7 days)
    StartPaymentTimer,
    /// Reconcile payment with GCP Billing
    ReconcileWithGcp { payment_id: String, amount: f64 },
    /// Notify customer of payment failure
    NotifyPaymentFailed,
    /// Prepare payment retry (exponential backoff)
    PrepareRetryPayment { attempt: u32 },
    /// Escalate to support team
    EscalateToSupport,
    /// Log payment completed
    LogPaymentComplete,
    /// Start payment dispute investigation
    LogDisputeStart,
    /// Log dispute resolution outcome
    LogDisputeResolution { outcome: String },
    /// Process refund to customer
    ProcessRefund { amount: f64 },
    /// Escalate to collections agency
    EscalateToCollections,
    /// Log successful collection
    LogCollectionSuccess,
    /// Log account written off
    LogAccountWriteOff,
    /// Log settlement agreement
    LogSettlement { amount: f64 },
}

impl BillingAction {
    /// Validate action semantics
    pub fn validate(&self) -> Result<(), BillingGovernorError> {
        match self {
            BillingAction::ReconcileWithGcp { amount, .. } => {
                if *amount <= 0.0 {
                    return Err(BillingGovernorError::PaymentProcessingFailed(
                        "Payment amount must be positive".to_string(),
                    ));
                }
                Ok(())
            }
            BillingAction::ProcessRefund { amount } => {
                if *amount <= 0.0 {
                    return Err(BillingGovernorError::PaymentProcessingFailed(
                        "Refund amount must be positive".to_string(),
                    ));
                }
                Ok(())
            }
            BillingAction::LogSettlement { amount } => {
                if *amount <= 0.0 {
                    return Err(BillingGovernorError::PaymentProcessingFailed(
                        "Settlement amount must be positive".to_string(),
                    ));
                }
                Ok(())
            }
            BillingAction::PrepareRetryPayment { attempt } => {
                if *attempt < 1 || *attempt > 3 {
                    return Err(BillingGovernorError::PaymentProcessingFailed(
                        "Retry attempt must be 1-3".to_string(),
                    ));
                }
                Ok(())
            }
            _ => Ok(()),
        }
    }
}

/// Receipt formats
#[derive(Debug, Clone, Serialize, Deserialize)]
pub enum ReceiptFormat {
    Json,
    Pdf,
    Csv,
}

/// Generate idempotency key from payment details
pub fn generate_idempotency_key(
    customer_id: &str,
    invoice_id: &str,
    amount: f64,
    timestamp: DateTime<Utc>,
) -> String {
    let mut hasher = Sha256::new();
    hasher.update(customer_id.as_bytes());
    hasher.update(b"|");
    hasher.update(invoice_id.as_bytes());
    hasher.update(b"|");
    hasher.update(amount.to_le_bytes());
    hasher.update(b"|");
    hasher.update(timestamp.timestamp().to_string().as_bytes());

    let digest = hasher.finalize();
    hex::encode(digest)
}

#[cfg(test)]
mod tests {
    use super::*;

    fn make_governor(customer_id: &str, invoice_id: &str, amount: f64) -> BillingGovernor {
        BillingGovernor::new(
            customer_id.to_string(),
            invoice_id.to_string(),
            amount,
            "USD".to_string(),
        )
        .unwrap()
    }

    // ============================================================
    // Test: Happy path - Invoice → Payment Received
    // ============================================================

    #[tokio::test]
    async fn test_happy_path_invoice_to_payment_received() {
        // Arrange
        let mut governor = make_governor("cust-1", "inv-001", 100.0);

        // Act 1: Generate invoice
        let (state1, action1) = governor
            .transition(BillingEvent::InvoiceGenerationTimeReached)
            .await
            .unwrap();

        // Assert 1
        assert_eq!(state1, BillingState::InvoiceIssued);
        assert!(matches!(action1, Some(BillingAction::GenerateInvoice)));

        // Act 2: Customer received
        let (state2, action2) = governor
            .transition(BillingEvent::CustomerReceived)
            .await
            .unwrap();

        // Assert 2
        assert_eq!(state2, BillingState::PaymentPending);
        assert!(matches!(action2, Some(BillingAction::StartPaymentTimer)));

        // Act 3: Payment received
        let (state3, action3) = governor
            .transition(BillingEvent::PaymentReceived {
                payment_id: "pay-001".to_string(),
                amount: 100.0,
                method: PaymentMethod::CreditCard("1234".to_string()),
                idempotency_key: generate_idempotency_key("cust-1", "inv-001", 100.0, Utc::now()),
            })
            .await
            .unwrap();

        // Assert 3
        assert_eq!(state3, BillingState::PaymentReceived);
        assert!(matches!(
            action3,
            Some(BillingAction::ReconcileWithGcp { .. })
        ));

        // Act 4: Accounting reconciled
        let (state4, action4) = governor
            .transition(BillingEvent::AccountingReconciled)
            .await
            .unwrap();

        // Assert 4
        assert_eq!(state4, BillingState::Archived);
        assert!(matches!(action4, Some(BillingAction::LogPaymentComplete)));
    }

    // ============================================================
    // Test: Failed payment → Retry 1 → Retry 2 → Retry 3 → Collections
    // ============================================================

    #[tokio::test]
    async fn test_payment_failure_retry_flow() {
        // Arrange
        let mut governor = make_governor("cust-2", "inv-002", 150.0);

        // Setup: Move to PaymentPending
        governor
            .transition(BillingEvent::InvoiceGenerationTimeReached)
            .await
            .unwrap();
        governor
            .transition(BillingEvent::CustomerReceived)
            .await
            .unwrap();

        // Act 1: Payment failed
        let (state1, _) = governor
            .transition(BillingEvent::PaymentFailed {
                reason: "Card declined".to_string(),
            })
            .await
            .unwrap();

        // Assert 1
        assert_eq!(state1, BillingState::PaymentFailed);
        assert_eq!(governor.failed_attempts(), 1);

        // Act 2: Retry approved
        let (state2, action2) = governor
            .transition(BillingEvent::RetryApproved)
            .await
            .unwrap();

        // Assert 2
        assert_eq!(state2, BillingState::Retry1);
        assert!(matches!(
            action2,
            Some(BillingAction::PrepareRetryPayment { attempt: 1 })
        ));

        // Act 3: Retry 1 fails
        let (state3, action3) = governor
            .transition(BillingEvent::PaymentFailed {
                reason: "Card declined again".to_string(),
            })
            .await
            .unwrap();

        // Assert 3
        assert_eq!(state3, BillingState::Retry2);
        assert!(matches!(
            action3,
            Some(BillingAction::PrepareRetryPayment { attempt: 2 })
        ));
        assert_eq!(governor.failed_attempts(), 2);

        // Act 4: Retry 2 fails
        let (state4, action4) = governor
            .transition(BillingEvent::PaymentFailed {
                reason: "Insufficient funds".to_string(),
            })
            .await
            .unwrap();

        // Assert 4
        assert_eq!(state4, BillingState::Retry3);
        assert!(matches!(
            action4,
            Some(BillingAction::PrepareRetryPayment { attempt: 3 })
        ));
        assert_eq!(governor.failed_attempts(), 3);

        // Act 5: Retry 3 fails, escalate to collections
        let (state5, action5) = governor
            .transition(BillingEvent::PaymentFailed {
                reason: "Account closed".to_string(),
            })
            .await
            .unwrap();

        // Assert 5
        assert_eq!(state5, BillingState::CollectionAgency);
        assert!(matches!(action5, Some(BillingAction::EscalateToCollections)));
        assert_eq!(governor.failed_attempts(), 4);
    }

    // ============================================================
    // Test: Duplicate payment detection
    // ============================================================

    #[tokio::test]
    async fn test_duplicate_payment_detection() {
        // Arrange
        let mut governor = make_governor("cust-3", "inv-003", 75.0);

        // Setup: Move to PaymentPending
        governor
            .transition(BillingEvent::InvoiceGenerationTimeReached)
            .await
            .unwrap();
        governor
            .transition(BillingEvent::CustomerReceived)
            .await
            .unwrap();

        let idempotency_key = generate_idempotency_key("cust-3", "inv-003", 75.0, Utc::now());

        // Act 1: First payment
        let (state1, _) = governor
            .transition(BillingEvent::PaymentReceived {
                payment_id: "pay-001".to_string(),
                amount: 75.0,
                method: PaymentMethod::CreditCard("1234".to_string()),
                idempotency_key: idempotency_key.clone(),
            })
            .await
            .unwrap();

        // Assert 1
        assert_eq!(state1, BillingState::PaymentReceived);

        // Act 2: Duplicate payment attempt (same idempotency key)
        governor.state = BillingState::PaymentPending; // Reset to allow retry
        let result = governor
            .transition(BillingEvent::PaymentReceived {
                payment_id: "pay-002".to_string(),
                amount: 75.0,
                method: PaymentMethod::CreditCard("1234".to_string()),
                idempotency_key,
            })
            .await;

        // Assert 2: Should fail with duplicate error
        assert!(matches!(
            result,
            Err(BillingGovernorError::DuplicatePayment { .. })
        ));
    }

    // ============================================================
    // Test: Concurrent payment detection
    // ============================================================

    #[tokio::test]
    async fn test_concurrent_payment_detection() {
        // Arrange
        let mut governor = make_governor("cust-4", "inv-004", 200.0);

        // Setup
        governor
            .transition(BillingEvent::InvoiceGenerationTimeReached)
            .await
            .unwrap();
        governor
            .transition(BillingEvent::CustomerReceived)
            .await
            .unwrap();

        // Manually set guard to simulate concurrent payment attempt
        governor.concurrent_payment_guard = true;

        let idempotency_key1 = generate_idempotency_key("cust-4", "inv-004", 200.0, Utc::now());

        // Act: Try to process payment while guard is set
        let result = governor
            .transition(BillingEvent::PaymentReceived {
                payment_id: "pay-001".to_string(),
                amount: 200.0,
                method: PaymentMethod::CreditCard("1234".to_string()),
                idempotency_key: idempotency_key1,
            })
            .await;

        // Assert: Should fail
        assert!(matches!(
            result,
            Err(BillingGovernorError::ConcurrentPayment { .. })
        ));
    }

    // ============================================================
    // Test: Payment amount mismatch detection
    // ============================================================

    #[tokio::test]
    async fn test_payment_amount_mismatch() {
        // Arrange
        let mut governor = make_governor("cust-5", "inv-005", 100.0);

        // Setup
        governor
            .transition(BillingEvent::InvoiceGenerationTimeReached)
            .await
            .unwrap();
        governor
            .transition(BillingEvent::CustomerReceived)
            .await
            .unwrap();

        let idempotency_key = generate_idempotency_key("cust-5", "inv-005", 100.0, Utc::now());

        // Act: Pay wrong amount (50 instead of 100)
        let result = governor
            .transition(BillingEvent::PaymentReceived {
                payment_id: "pay-001".to_string(),
                amount: 50.0, // Wrong!
                method: PaymentMethod::CreditCard("1234".to_string()),
                idempotency_key,
            })
            .await;

        // Assert
        assert!(matches!(
            result,
            Err(BillingGovernorError::PaymentAmountMismatch { .. })
        ));
    }

    // ============================================================
    // Test: Payment dispute → Resolution
    // ============================================================

    #[tokio::test]
    async fn test_payment_dispute_refund_flow() {
        // Arrange
        let mut governor = make_governor("cust-6", "inv-006", 120.0);

        // Setup: Move to PaymentPending
        governor
            .transition(BillingEvent::InvoiceGenerationTimeReached)
            .await
            .unwrap();
        governor
            .transition(BillingEvent::CustomerReceived)
            .await
            .unwrap();

        // Act 1: Dispute raised
        let (state1, action1) = governor
            .transition(BillingEvent::PaymentDispute {
                dispute_id: "disp-001".to_string(),
            })
            .await
            .unwrap();

        // Assert 1
        assert_eq!(state1, BillingState::PaymentDisputed);
        assert!(matches!(action1, Some(BillingAction::LogDisputeStart)));
        assert_eq!(governor.dispute_id(), Some("disp-001"));

        // Act 2: Dispute resolved with refund
        let (state2, action2) = governor
            .transition(BillingEvent::DisputeResolved {
                outcome: DisputeOutcome::RefundIssued { amount: 120.0 },
            })
            .await
            .unwrap();

        // Assert 2
        assert_eq!(state2, BillingState::Archived);
        assert!(matches!(
            action2,
            Some(BillingAction::ProcessRefund { amount: 120.0 })
        ));
    }

    // ============================================================
    // Test: Invalid currency detection
    // ============================================================

    #[tokio::test]
    fn test_invalid_currency_detection() {
        // Act: Try to create governor with invalid currency
        let result = BillingGovernor::new("cust-7".to_string(), "inv-007".to_string(), 100.0, "INVALID".to_string());

        // Assert
        assert!(matches!(
            result,
            Err(BillingGovernorError::InvalidCurrency(_))
        ));
    }

    // ============================================================
    // Test: Tax calculation
    // ============================================================

    #[tokio::test]
    fn test_tax_calculation() {
        // Arrange
        let governor = make_governor("cust-8", "inv-008", 100.0);

        // Act: Calculate 10% tax
        let tax = governor.calculate_tax(0.10).unwrap();

        // Assert
        assert!((tax - 10.0).abs() < f64::EPSILON);
    }

    // ============================================================
    // Test: Payment method masking
    // ============================================================

    #[tokio::test]
    fn test_payment_method_masking() {
        // Arrange
        let cc_method = PaymentMethod::CreditCard("4242".to_string());

        // Act
        let masked = cc_method.masked();

        // Assert
        assert_eq!(masked, "CC:****4242");
    }

    // ============================================================
    // Test: Settlement agreement in collections
    // ============================================================

    #[tokio::test]
    async fn test_collection_settlement_agreement() {
        // Arrange
        let mut governor = make_governor("cust-9", "inv-009", 300.0);
        governor.state = BillingState::CollectionAgency;

        // Act: Settlement agreed
        let (state, action) = governor
            .transition(BillingEvent::SettlementAgreed {
                settlement_amount: 250.0, // Negotiated down
            })
            .await
            .unwrap();

        // Assert
        assert_eq!(state, BillingState::Archived);
        assert!(matches!(
            action,
            Some(BillingAction::LogSettlement { amount: 250.0 })
        ));
    }

    // ============================================================
    // Test: Terminal state prevents transitions
    // ============================================================

    #[tokio::test]
    async fn test_terminal_state_prevents_transitions() {
        // Arrange
        let mut governor = make_governor("cust-10", "inv-010", 100.0);
        governor.state = BillingState::Archived;

        // Act: Try to transition from terminal state
        let result = governor
            .transition(BillingEvent::PaymentReceived {
                payment_id: "pay-001".to_string(),
                amount: 100.0,
                method: PaymentMethod::CreditCard("1234".to_string()),
                idempotency_key: "test-key".to_string(),
            })
            .await;

        // Assert
        assert!(matches!(
            result,
            Err(BillingGovernorError::InvalidTransition { .. })
        ));
    }

    // ============================================================
    // Test: Timeout auto-transitions
    // ============================================================

    #[tokio::test]
    async fn test_payment_pending_timeout_to_failed() {
        // Arrange
        let mut governor = make_governor("cust-11", "inv-011", 100.0);
        governor.state = BillingState::PaymentPending;

        // Act: 7-day timeout
        let (state, action) = governor
            .transition(BillingEvent::TimeoutTransition)
            .await
            .unwrap();

        // Assert: Auto-transitions to PaymentFailed
        assert_eq!(state, BillingState::PaymentFailed);
        assert!(matches!(action, Some(BillingAction::NotifyPaymentFailed)));
    }

    // ============================================================
    // Test: Action validation
    // ============================================================

    #[tokio::test]
    fn test_action_validation() {
        // Arrange
        let invalid_action = BillingAction::ReconcileWithGcp {
            payment_id: "pay-001".to_string(),
            amount: -100.0, // Invalid!
        };

        // Act
        let result = invalid_action.validate();

        // Assert
        assert!(result.is_err());
    }
}
