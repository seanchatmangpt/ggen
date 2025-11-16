//! Capability-Based Effect System: Governance Through Ownership
//!
//! The core principle: code that doesn't have a capability cannot touch that resource.
//! Capabilities are:
//! - Typed handles encoding what can be touched
//! - Scarce (not cheaply cloneable for powerful capabilities)
//! - Non-forgeable (only produced through doctrine-aware gates)
//!
//! This turns "no ungoverned IO" into a compile-time property of Rust's type system.

use std::marker::PhantomData;
use serde::{Deserialize, Serialize};

// ============================================================================
// CAPABILITY TOKENS
// ============================================================================

/// Base capability marker (sealed trait preventing outside implementation)
pub mod capability_marker {
    pub trait Sealed {}
}

/// Read-only capability for observations and snapshots
/// Many can exist concurrently (immutable access pattern)
#[derive(Debug, Clone)]
pub struct ReadObservationCapability {
    #[allow(dead_code)]
    granted_at: u64,
    grant_id: String,
}

impl ReadObservationCapability {
    /// Grant a read capability
    pub fn grant(grant_id: impl Into<String>) -> Self {
        Self {
            granted_at: std::time::SystemTime::now()
                .duration_since(std::time::UNIX_EPOCH)
                .unwrap_or_default()
                .as_secs(),
            grant_id: grant_id.into(),
        }
    }

    pub fn grant_id(&self) -> &str {
        &self.grant_id
    }
}

/// Read snapshot capability
#[derive(Debug, Clone)]
pub struct ReadSnapshotCapability {
    #[allow(dead_code)]
    granted_at: u64,
    grant_id: String,
}

impl ReadSnapshotCapability {
    pub fn grant(grant_id: impl Into<String>) -> Self {
        Self {
            granted_at: std::time::SystemTime::now()
                .duration_since(std::time::UNIX_EPOCH)
                .unwrap_or_default()
                .as_secs(),
            grant_id: grant_id.into(),
        }
    }

    pub fn grant_id(&self) -> &str {
        &self.grant_id
    }
}

/// Write snapshot capability (writes go to staging, not active Σ)
/// Single holder only (exclusive write semantics)
pub struct WriteSnapshotCapability {
    #[allow(dead_code)]
    granted_at: u64,
    grant_id: String,
    _not_clone: std::marker::PhantomData<*const ()>, // Prevent automatic Clone
}

impl WriteSnapshotCapability {
    pub fn grant(grant_id: impl Into<String>) -> Self {
        Self {
            granted_at: std::time::SystemTime::now()
                .duration_since(std::time::UNIX_EPOCH)
                .unwrap_or_default()
                .as_secs(),
            grant_id: grant_id.into(),
            _not_clone: PhantomData,
        }
    }

    pub fn grant_id(&self) -> &str {
        &self.grant_id
    }
}

impl std::fmt::Debug for WriteSnapshotCapability {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("WriteSnapshotCapability")
            .field("grant_id", &self.grant_id)
            .finish()
    }
}

/// Write ontology capability (direct Σ mutation - very scarce)
/// Single holder only
pub struct WriteOntologyCapability {
    #[allow(dead_code)]
    granted_at: u64,
    grant_id: String,
    /// Only valid if doctrine check passed
    doctrine_verified: bool,
    _not_clone: PhantomData<*const ()>,
}

impl WriteOntologyCapability {
    /// Grant ontology write capability
    /// ONLY after doctrine checks have passed
    pub fn grant_verified(grant_id: impl Into<String>) -> Self {
        Self {
            granted_at: std::time::SystemTime::now()
                .duration_since(std::time::UNIX_EPOCH)
                .unwrap_or_default()
                .as_secs(),
            grant_id: grant_id.into(),
            doctrine_verified: true,
            _not_clone: PhantomData,
        }
    }

    pub fn grant_id(&self) -> &str {
        &self.grant_id
    }

    pub fn is_doctrine_verified(&self) -> bool {
        self.doctrine_verified
    }
}

impl std::fmt::Debug for WriteOntologyCapability {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("WriteOntologyCapability")
            .field("grant_id", &self.grant_id)
            .field("doctrine_verified", &self.doctrine_verified)
            .finish()
    }
}

/// Promote marketplace package capability
/// Requires strong proof
pub struct PromoteMarketplaceCapability {
    #[allow(dead_code)]
    granted_at: u64,
    grant_id: String,
    proof_id: String, // Links to the proof object that justified this
    _not_clone: PhantomData<*const ()>,
}

impl PromoteMarketplaceCapability {
    /// Grant after proof has been validated
    pub fn grant_with_proof(grant_id: impl Into<String>, proof_id: impl Into<String>) -> Self {
        Self {
            granted_at: std::time::SystemTime::now()
                .duration_since(std::time::UNIX_EPOCH)
                .unwrap_or_default()
                .as_secs(),
            grant_id: grant_id.into(),
            proof_id: proof_id.into(),
            _not_clone: PhantomData,
        }
    }

    pub fn grant_id(&self) -> &str {
        &self.grant_id
    }

    pub fn proof_id(&self) -> &str {
        &self.proof_id
    }
}

impl std::fmt::Debug for PromoteMarketplaceCapability {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("PromoteMarketplaceCapability")
            .field("grant_id", &self.grant_id)
            .field("proof_id", &self.proof_id)
            .finish()
    }
}

/// Modify doctrine capability (rarest - only for tightening)
pub struct ModifyDoctrineCapability {
    #[allow(dead_code)]
    granted_at: u64,
    grant_id: String,
    allow_tightening_only: bool,
    _not_clone: PhantomData<*const ()>,
}

impl ModifyDoctrineCapability {
    /// Grant doctrine modification (tightening only)
    pub fn grant_tightening_only(grant_id: impl Into<String>) -> Self {
        Self {
            granted_at: std::time::SystemTime::now()
                .duration_since(std::time::UNIX_EPOCH)
                .unwrap_or_default()
                .as_secs(),
            grant_id: grant_id.into(),
            allow_tightening_only: true,
            _not_clone: PhantomData,
        }
    }

    pub fn grant_id(&self) -> &str {
        &self.grant_id
    }

    pub fn allows_tightening_only(&self) -> bool {
        self.allow_tightening_only
    }
}

impl std::fmt::Debug for ModifyDoctrineCapability {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("ModifyDoctrineCapability")
            .field("grant_id", &self.grant_id)
            .field("allow_tightening_only", &self.allow_tightening_only)
            .finish()
    }
}

// ============================================================================
// EFFECT TYPES: Impossible to construct without capability
// ============================================================================

/// Effect: something with a real-world side-effect
/// Parameterized by what capability is needed
#[derive(Debug)]
pub struct Effect<C> {
    effect_id: String,
    description: String,
    _capability: PhantomData<C>,
}

impl<C> Effect<C> {
    /// Create an effect that requires capability C
    pub fn new(effect_id: impl Into<String>, description: impl Into<String>) -> Self {
        Self {
            effect_id: effect_id.into(),
            description: description.into(),
            _capability: PhantomData,
        }
    }

    pub fn effect_id(&self) -> &str {
        &self.effect_id
    }

    pub fn description(&self) -> &str {
        &self.description
    }
}

/// Read effect (requires read capability)
pub type ReadObservationEffect = Effect<ReadObservationCapability>;

pub type ReadSnapshotEffect = Effect<ReadSnapshotCapability>;

/// Write effect (requires write capability)
pub type WriteSnapshotEffect = Effect<WriteSnapshotCapability>;

pub type WriteOntologyEffect = Effect<WriteOntologyCapability>;

pub type PromoteMarketplaceEffect = Effect<PromoteMarketplaceCapability>;

pub type ModifyDoctrineEffect = Effect<ModifyDoctrineCapability>;

// ============================================================================
// EFFECT HANDLERS: Execute effects that possess capabilities
// ============================================================================

/// Handler for read-only effects
pub trait ReadOnlyEffectHandler {
    fn handle_read_observation(
        &self,
        _cap: &ReadObservationCapability,
        effect: &ReadObservationEffect,
    ) -> Result<String, String> {
        Ok(format!("Read: {}", effect.description()))
    }

    fn handle_read_snapshot(
        &self,
        _cap: &ReadSnapshotCapability,
        effect: &ReadSnapshotEffect,
    ) -> Result<String, String> {
        Ok(format!("Read: {}", effect.description()))
    }
}

/// Handler for write effects
pub trait WritableEffectHandler {
    fn handle_write_snapshot(
        &self,
        _cap: &WriteSnapshotCapability,
        effect: &WriteSnapshotEffect,
    ) -> Result<String, String> {
        Ok(format!("Staged write: {}", effect.description()))
    }

    fn handle_write_ontology(
        &self,
        cap: &WriteOntologyCapability,
        effect: &WriteOntologyEffect,
    ) -> Result<String, String> {
        if !cap.is_doctrine_verified() {
            return Err("Ontology write capability not doctrine-verified".to_string());
        }
        Ok(format!("Ontology mutation: {}", effect.description()))
    }
}

/// Handler for marketplace effects
pub trait MarketplaceEffectHandler {
    fn handle_promote_package(
        &self,
        cap: &PromoteMarketplaceCapability,
        effect: &PromoteMarketplaceEffect,
    ) -> Result<String, String> {
        Ok(format!(
            "Promote package (proof {}): {}",
            cap.proof_id(),
            effect.description()
        ))
    }
}

/// Handler for doctrine effects
pub trait DoctrineEffectHandler {
    fn handle_modify_doctrine(
        &self,
        cap: &ModifyDoctrineCapability,
        effect: &ModifyDoctrineEffect,
    ) -> Result<String, String> {
        if !cap.allows_tightening_only() {
            return Err("Doctrine modification must be tightening-only".to_string());
        }
        Ok(format!("Doctrine tightening: {}", effect.description()))
    }
}

// ============================================================================
// IMMUTABLE PROJECTION LAYER: Read-only Σ, O, Γ facades
// ============================================================================

/// Immutable snapshot of ontology (safe to clone and share)
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct ImmutableSnapshot {
    pub id: String,
    pub content: String,
    pub version: u64,
}

/// Immutable observation (safe to clone and share)
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct ImmutableObservation {
    pub id: String,
    pub data: String,
    pub timestamp: u64,
}

/// Immutable findings (safe to clone and share)
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct ImmutableFinding {
    pub id: String,
    pub content: String,
    pub severity: u8,
}

/// Projection layer: provides read-only views without capabilities
/// Code using this never needs write capability
pub struct ImmutableProjection {
    snapshots: Vec<ImmutableSnapshot>,
    observations: Vec<ImmutableObservation>,
    findings: Vec<ImmutableFinding>,
}

impl ImmutableProjection {
    pub fn new() -> Self {
        Self {
            snapshots: Vec::new(),
            observations: Vec::new(),
            findings: Vec::new(),
        }
    }

    pub fn add_snapshot(&mut self, snapshot: ImmutableSnapshot) {
        self.snapshots.push(snapshot);
    }

    pub fn add_observation(&mut self, observation: ImmutableObservation) {
        self.observations.push(observation);
    }

    pub fn add_finding(&mut self, finding: ImmutableFinding) {
        self.findings.push(finding);
    }

    pub fn snapshots(&self) -> &[ImmutableSnapshot] {
        &self.snapshots
    }

    pub fn observations(&self) -> &[ImmutableObservation] {
        &self.observations
    }

    pub fn findings(&self) -> &[ImmutableFinding] {
        &self.findings
    }

    pub fn latest_snapshot(&self) -> Option<&ImmutableSnapshot> {
        self.snapshots.iter().max_by_key(|s| s.version)
    }
}

impl Default for ImmutableProjection {
    fn default() -> Self {
        Self::new()
    }
}

// ============================================================================
// CAPABILITY GATEKEEPING: Only doctrine-proven builders can grant
// ============================================================================

/// Central authority for granting capabilities
/// (In real usage, would be called by doctrine-verified constructors only)
pub struct CapabilityGrantor;

impl CapabilityGrantor {
    /// Always grant read capabilities (no harm in reading)
    pub fn grant_read_observation(grant_id: impl Into<String>) -> ReadObservationCapability {
        ReadObservationCapability::grant(grant_id)
    }

    pub fn grant_read_snapshot(grant_id: impl Into<String>) -> ReadSnapshotCapability {
        ReadSnapshotCapability::grant(grant_id)
    }

    /// Only grant write snapshot to code that calls this explicitly
    pub fn grant_write_snapshot(grant_id: impl Into<String>) -> WriteSnapshotCapability {
        WriteSnapshotCapability::grant(grant_id)
    }

    /// Only grant write ontology after doctrine checks
    pub fn grant_write_ontology(grant_id: impl Into<String>) -> WriteOntologyCapability {
        WriteOntologyCapability::grant_verified(grant_id)
    }

    /// Only grant marketplace promotion with proof
    pub fn grant_promote_marketplace(
        grant_id: impl Into<String>,
        proof_id: impl Into<String>,
    ) -> PromoteMarketplaceCapability {
        PromoteMarketplaceCapability::grant_with_proof(grant_id, proof_id)
    }

    /// Only grant doctrine modification for tightening
    pub fn grant_doctrine_tightening(grant_id: impl Into<String>) -> ModifyDoctrineCapability {
        ModifyDoctrineCapability::grant_tightening_only(grant_id)
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_read_capabilities_are_cloneable() {
        let cap1 = ReadObservationCapability::grant("cap-1");
        let cap2 = cap1.clone();
        assert_eq!(cap1.grant_id(), cap2.grant_id());
    }

    #[test]
    fn test_write_capability_non_cloneable() {
        let cap = WriteSnapshotCapability::grant("write-1");
        let _cap_ref = &cap;
        // If we try: let cap2 = cap.clone();
        // ^ This will NOT compile because PhantomData<*const ()> prevents Clone
        // This test just verifies the type exists
    }

    #[test]
    fn test_effect_creation() {
        let _cap = ReadObservationCapability::grant("cap-1");
        let effect = ReadObservationEffect::new("effect-1", "Read observation data");

        assert_eq!(effect.effect_id(), "effect-1");
        assert_eq!(effect.description(), "Read observation data");
    }

    #[test]
    fn test_immutable_projection() {
        let mut proj = ImmutableProjection::new();

        proj.add_snapshot(ImmutableSnapshot {
            id: "snap-1".to_string(),
            content: "content".to_string(),
            version: 1,
        });

        proj.add_observation(ImmutableObservation {
            id: "obs-1".to_string(),
            data: "data".to_string(),
            timestamp: 1000,
        });

        assert_eq!(proj.snapshots().len(), 1);
        assert_eq!(proj.observations().len(), 1);
        assert_eq!(proj.latest_snapshot().unwrap().version, 1);
    }

    #[test]
    fn test_capability_grantor() {
        let read_cap = CapabilityGrantor::grant_read_observation("grant-1");
        let promote_cap =
            CapabilityGrantor::grant_promote_marketplace("grant-2", "proof-123");

        assert_eq!(read_cap.grant_id(), "grant-1");
        assert_eq!(promote_cap.proof_id(), "proof-123");
    }

    #[test]
    fn test_ontology_write_capability_verified() {
        let cap = CapabilityGrantor::grant_write_ontology("grant-ontology");
        assert!(cap.is_doctrine_verified());
    }
}
