#![deny(warnings)]
#![allow(missing_docs)]
#![deny(unsafe_code)]
#![doc = "Definition of Done: Type-safe, deterministic autonomous substrate"]

//! # ggen-dod: The Definition of Done
//!
//! This crate implements the complete Definition of Done specification for ggen,
//! ensuring it operates as a closed-world, autonomic substrate without human arbitration
//! in the critical path.
//!
//! ## Core Systems
//!
//! - **O (Observations)**: Type-safe observation model with schema validation
//! - **Σ (Contracts)**: Versioned ontology and contract system with invariant enforcement
//! - **Q (Invariants)**: Hard-blocking constraint checks
//! - **μ (Kernel)**: Deterministic decision kernel with timing guarantees (τ ≤ 8)
//! - **Γ (History)**: Immutable receipt and audit trail system
//! - **ΔΣ (Evolution)**: Doctrine-aligned schema changes with proofs
//! - **MAPE-K (Autonomy)**: Monitor-Analyze-Plan-Execute-Knowledge loop
//!
//! ## Guarantees
//!
//! - **Determinism**: μ(O) produces identical A across all executions for fixed O, Σ*, Γ
//! - **Idempotence**: μ ∘ μ = μ for all idempotent operations
//! - **Closed-world**: All decisions derivable from O, Σ, Q, Γ (proven by decision closure checker)
//! - **Provenance**: Every action has cryptographically signed receipt
//! - **Timing**: Performance guarantees (τ ≤ 8ms) enforced at compile and runtime

pub mod autonomic;
pub mod binding_completeness;
pub mod contract;
pub mod decision;
pub mod decision_closure;
pub mod doctrine;
pub mod error;
pub mod invariant;
pub mod kernel;
pub mod observation;
pub mod receipt;
pub mod replay;
pub mod tenant;
pub mod timing;

// Re-export commonly used types
pub use autonomic::mape_k::{
    AnalysisPhase, ExecutionPhase, MAPEKLoop, ObservationPhase, PlanningPhase,
};
pub use contract::{Contract, ContractId, ContractVersion, Ontology};
pub use decision::{Decision, DecisionId, DecisionStore};
pub use doctrine::DoctrineCompliance;
pub use error::{DoDError, DoDResult};
pub use invariant::{Invariant, InvariantChecker, InvariantId};
pub use kernel::{Kernel, KernelAction, KernelDecision};
pub use observation::{Observation, ObservationId, ObservationSchema, ObservationType};
pub use receipt::{Receipt, ReceiptId, ReceiptStore};
pub use tenant::{TenantContext, TenantId, TenantIsolation};
pub use timing::{TimingEnforcer, TimingGuarantee, TimingMeasurement};

/// Core constants for DoD
pub mod constants {
    /// Maximum kernel decision time (τ ≤ 8ms)
    pub const KERNEL_MAX_TIME_MS: u64 = 8;

    /// Maximum observation size (preventing DOS)
    pub const MAX_OBSERVATION_SIZE: usize = 1024 * 1024; // 1MB

    /// Maximum Σ* fragment depth
    pub const MAX_SCHEMA_DEPTH: usize = 256;

    /// Maximum fan-out per kernel tick
    pub const MAX_FANOUT: usize = 1024;

    /// Maximum ΔΣ promotions per time unit
    pub const MAX_PROMOTION_RATE_PER_HOUR: usize = 100;

    /// Minimum proof requirements for different decision classes
    pub mod proof_thresholds {
        /// Read-only operations (≤30% doctrine distance)
        pub const WEAK_PROOF: u8 = 30;
        /// Cache/snapshot updates (≤50%)
        pub const STANDARD_PROOF: u8 = 50;
        /// Schema changes (≤70%)
        pub const STRONG_PROOF: u8 = 70;
        /// Marketplace changes (≤80%)
        pub const CRITICAL_PROOF: u8 = 80;
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_constants_consistency() {
        // Verify proof threshold ordering
        assert!(
            constants::proof_thresholds::WEAK_PROOF < constants::proof_thresholds::STANDARD_PROOF
        );
        assert!(
            constants::proof_thresholds::STANDARD_PROOF < constants::proof_thresholds::STRONG_PROOF
        );
        assert!(
            constants::proof_thresholds::STRONG_PROOF < constants::proof_thresholds::CRITICAL_PROOF
        );
    }
}
