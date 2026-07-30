#![forbid(unsafe_code)]
//! Canonical, deterministic, IO-free architecture kernel for ggen.
//!
//! The generic ggen Building Block calculus is the semantic authority. The
//! existing Fortune 5 runtime law is preserved as a profile module. This crate
//! may manufacture bounded intents, but it cannot actuate external systems.

pub mod building_block;

#[path = "lib.rs"]
pub mod fortune5_kernel;

/// Architecture profiles layered over the generic building-block calculus.
pub mod profiles {
    /// Fortune 5 operational controls, policies, intents, and receipts.
    pub use crate::fortune5_kernel as fortune5;
}

pub use building_block::{
    ArchitectureFacet, Authority, BuildingBlock, BuildingBlockContract, BuildingBlockId,
    BuildingBlockRegistry, BuildingBlockViolation, CompositionReceipt, EvidenceKind,
    EvidenceObligation, EvidenceReceipt, LifecycleState, ObligationId, Port, PortDirection,
    PortId, PortKind, ProfileId, RealizationBinding, RealizationId, ResourceClaim,
    ResourceCeiling, Standing, SubstitutionAssessment,
};

/// Receipt schema retained by the Fortune 5 profile.
pub const RECEIPT_SCHEMA: &str = fortune5_kernel::RECEIPT_SCHEMA;
/// Intent schema retained by the Fortune 5 profile.
pub const INTENT_SCHEMA: &str = fortune5_kernel::INTENT_SCHEMA;
/// The only lawful downstream actuator address.
pub const REQUIRED_BROKER: &str = fortune5_kernel::REQUIRED_BROKER;
