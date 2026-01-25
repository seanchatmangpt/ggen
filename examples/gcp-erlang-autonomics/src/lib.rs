//! Erlang Autonomic System - GCP Integration
//!
//! This library implements a production-grade autonomic system inspired by Erlang's
//! fault-tolerance patterns and adapted for GCP-based microservices.
//!
//! ## Architecture
//!
//! The system follows the Autonomic Computing (AC) MAPE-K loop model:
//! - **M**onitor: Signal ingestion and normalization
//! - **A**nalyze: Entitlement validation and state checking
//! - **P**lan: Governor FSM coordination
//! - **E**xecute: Actuator safe action execution
//! - **K**nowledge: Receipt-based cryptographic ledger
//!
//! ## Core Modules
//!
//! - [`signal_ingest`]: Raw signal normalization and deduplication
//! - [`entitlement`]: RevOps kernel with marketplace lifecycle management
//! - [`governor`]: Gen_statem-inspired FSM orchestrator (CRITICAL)
//! - [`actuator`]: Safe action execution with rollback capability
//! - [`receipt`]: Cryptographic receipt ledger with hash-chain verification
//!
//! ## Example
//!
//! ```no_run
//! use gcp_erlang_autonomics::{
//!     signal_ingest::{SignalIngest, RawEvent},
//!     entitlement::EntitlementService,
//!     governor::Governor,
//!     actuator::Actuator,
//! };
//!
//! # async fn example() -> Result<(), Box<dyn std::error::Error>> {
//! // 1. Ingest raw event
//! let raw = RawEvent {
//!     tenant_id: "tenant-1".into(),
//!     metric: "cpu_utilization".into(),
//!     value: 85.0,
//!     timestamp_ms: 1000000,
//! };
//!
//! let signal = SignalIngest::normalize(raw).await?;
//!
//! // 2. Check entitlement
//! let entitlement = EntitlementService::get_active("tenant-1").await?;
//!
//! // 3. Coordinate via Governor FSM
//! let (next_state, action) = Governor::transition_with_signal(&signal).await?;
//!
//! // 4. Execute action (if needed)
//! if let Some(action) = action {
//!     let receipt = Actuator::execute(action).await?;
//!     println!("Action executed: {:?}", receipt.id);
//! }
//! # Ok(())
//! # }
//! ```

pub mod signal_ingest;
pub mod entitlement;
pub mod governor;
pub mod actuator;
pub mod receipt;
pub mod marketplace;

// Re-exports of common types for ergonomics
pub use signal_ingest::{SignalIngest, RawEvent, NormalizedSignal, SignalError};
pub use entitlement::{EntitlementService, Entitlement, EntitlementState, EntitlementError};
pub use governor::{Governor, GovernorState, GovernorEvent, GovernorError};
pub use actuator::{Actuator, Action, ActionReceipt, ActuatorError};
pub use receipt::{Receipt, ReceiptLedger, ReceiptError};
pub use marketplace::{
    // Marketplace orchestrator
    MarketplaceOrchestrator, OrchestratorState, MarketplaceEvent, GovernorType,
    GovernorResponse, GovernorResponseStatus, MarketplaceOrchestratorError, OrchestratorStats,
    // Multi-tenant governance
    MTGovernor, MTGovernorState, MTGovernorEvent, MTGovernorError, TenantMetrics, TenantTier,
    CascadeIndicator, LoadBalancingStrategy, AuditEvent,
    // Compliance & Audit governance
    ComplianceGovernor, ComplianceState, ComplianceEvent, ComplianceError,
    ComplianceFramework, AuditTrailEntry, AuditResult, Violation,
    DataResidency, BreachIncident, BreachPhase,
};
