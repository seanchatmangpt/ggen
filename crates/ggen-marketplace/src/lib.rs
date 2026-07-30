//! # ggen-marketplace - Package marketplace system
//!
//! Marketplace plugin providing package management, discovery, installation,
//! and Genesis-bearing interchangeable-part passports for ggen.

// Crate-level lint exceptions for marketplace implementation and test code:
// - expect_used: RwLock/Mutex expect() on poisoned locks is intentional (panics on invariant violation)
// - unwrap_used: test setup and iterator chaining in non-fallible contexts
// - panic: test assertions and unreachable code paths in tests
// - unwrap_err: test error-path assertions
// - into_iter: single-item iteration patterns
#![allow(clippy::expect_used, clippy::unwrap_used, clippy::panic)]

pub mod agent;
pub mod marketplace;
pub mod ontology_core;
pub mod packs;
pub mod packs_registry;
pub mod sync_profile;

pub use marketplace::{
    all_fortune5_contracts, fortune5_contract, CausalPolarity, ConformityMark,
    Fortune5Assessment, Fortune5AssessmentReceipt, Fortune5Capability,
    Fortune5CapabilityAssessment, Fortune5CapabilityContract, Fortune5Category,
    Fortune5Error, Fortune5EvidenceLedger, Fortune5EvidenceOutcome, Fortune5EvidenceRecord,
    Fortune5Proof, Fortune5ProofSurface, Fortune5Reference, Fortune5Standing, HostProfile,
    InputEnvelope, IsolationClass, LifecyclePolicy, LifecycleState, Manifest,
    NonInterferenceProfile, OutputContract, Package, PackageId, PartIdentity, PartPassport,
    PassportBinding, PassportValidationReport, ProtocolRange, QualityScore, RdfRegistry,
    ResourceEnvelope, RetirementPolicy, SparqlSearchEngine, SubstitutionReport,
    TemporalProfile, VerifierMark, VerifierStatus, ALL_FORTUNE5_CAPABILITIES,
    CURRENT_PASSPORT_SCHEMA, FORTUNE5_CONTRACT_VERSION, REQUIRED_PROOF_SURFACES,
};
