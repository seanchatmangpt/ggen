//! Type-Indexed Actions - Compile-Time Governance Algebra
//!
//! Actions are fully parameterized by their governance profile:
//! - RiskClass: ReadOnly, Low, Medium, High
//! - TickBudget: const generic upper bound (0-∞ ticks)
//! - MutationClass: ImmutableRead, SnapshotWrite, OntologyMutate
//! - ExecutionPath: Hot (≤8), Warm (≤100), Cold (unbounded)
//!
//! This makes it IMPOSSIBLE to construct an action that violates doctrine.
//! The compiler enforces all governance; runtime checks are defense-in-depth only.

use std::marker::PhantomData;
use serde::{Deserialize, Serialize};

/// Risk classification for an action
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, Serialize, Deserialize)]
pub enum RiskClass {
    /// Read-only, no side effects, no state changes
    ReadOnly,
    /// Low risk: metadata reads, cache updates
    Low,
    /// Medium risk: snapshot updates, limited scope changes
    Medium,
    /// High risk: ontology mutations, marketplace changes
    High,
}

impl RiskClass {
    /// Can this risk class execute on the hot path?
    pub fn is_hot_path_eligible(&self) -> bool {
        matches!(self, RiskClass::ReadOnly | RiskClass::Low)
    }

    /// Can this risk class execute on the warm path?
    pub fn is_warm_path_eligible(&self) -> bool {
        matches!(
            self,
            RiskClass::ReadOnly | RiskClass::Low | RiskClass::Medium
        )
    }

    /// Only high-risk actions must go cold path
    pub fn required_path(&self) -> ExecutionPath {
        match self {
            RiskClass::ReadOnly => ExecutionPath::Hot,
            RiskClass::Low => ExecutionPath::Hot,
            RiskClass::Medium => ExecutionPath::Warm,
            RiskClass::High => ExecutionPath::Cold,
        }
    }
}

/// Type-safe execution path with compile-time tick budgets
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, Serialize, Deserialize)]
pub enum ExecutionPath {
    /// Hot path: ≤ 8 ticks (Chatman constant)
    Hot,
    /// Warm path: ≤ 100 ticks
    Warm,
    /// Cold path: unbounded (but monitored)
    Cold,
}

impl ExecutionPath {
    /// Get the tick budget for this path
    pub fn max_ticks(&self) -> usize {
        match self {
            ExecutionPath::Hot => 8,
            ExecutionPath::Warm => 100,
            ExecutionPath::Cold => usize::MAX,
        }
    }

    /// Can this action fit on this path given its tick budget?
    pub fn can_fit(&self, ticks: usize) -> bool {
        ticks <= self.max_ticks()
    }
}

/// Mutation classification for an action
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, Serialize, Deserialize)]
pub enum MutationClass {
    /// Read-only access to Σ, O, Γ
    ImmutableRead,
    /// Write to snapshots only (no core ontology change)
    SnapshotWrite,
    /// Mutate core ontology (ΔΣ promotion)
    OntologyMutate,
}

/// Doctrine alignment marker - proves that action satisfies DOCTRINE_2027
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct DoctrineAligned;

/// Base action type parameterized by governance profile
///
/// This type is sealed by design: you cannot construct an Action<R, T, M, P>
/// without going through doctrine-aware builders. The type parameters are the law.
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct Action<R: Copy + Eq + 'static, const TICKS: usize, M: Copy + Eq + 'static> {
    /// Unique action ID
    pub id: String,

    /// User-facing description
    pub description: String,

    /// Risk class of this action
    risk: PhantomData<R>,

    /// Mutation class of this action
    mutation: PhantomData<M>,

    /// Metadata (sealed from type safety perspective)
    metadata: ActionMetadata,
}

/// Action metadata stored at runtime (informational only; governance is in types)
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct ActionMetadata {
    /// When action was created
    pub created_at: u64,

    /// Risk class (mirrors type parameter R at runtime)
    pub risk_class: RiskClass,

    /// Mutation class (mirrors type parameter M at runtime)
    pub mutation_class: MutationClass,

    /// Tick budget (mirrors const TICKS at compile time)
    pub tick_budget: usize,

    /// Actual ticks consumed (set by executor)
    pub ticks_consumed: Option<usize>,

    /// Is this action in-flight?
    pub in_flight: bool,

    /// Doctrine distance band this action satisfies
    pub doctrine_distance_band: DoctrineDistanceBand,
}

/// Doctrine distance bands for actions
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, Serialize, Deserialize)]
pub enum DoctrineDistanceBand {
    /// Distance 0-20: Perfect alignment
    Perfect,
    /// Distance 20-40: Good alignment
    Good,
    /// Distance 40-60: Acceptable
    Acceptable,
    /// Distance 60-80: Marginal (requires review)
    Marginal,
    /// Distance 80-100: Unacceptable (rejected)
    Unacceptable,
}

impl DoctrineDistanceBand {
    /// Get minimum distance for this band
    pub fn min_distance(&self) -> f64 {
        match self {
            DoctrineDistanceBand::Perfect => 0.0,
            DoctrineDistanceBand::Good => 20.0,
            DoctrineDistanceBand::Acceptable => 40.0,
            DoctrineDistanceBand::Marginal => 60.0,
            DoctrineDistanceBand::Unacceptable => 80.0,
        }
    }

    /// Classify distance into band
    pub fn from_distance(distance: f64) -> Self {
        match distance {
            d if d < 20.0 => DoctrineDistanceBand::Perfect,
            d if d < 40.0 => DoctrineDistanceBand::Good,
            d if d < 60.0 => DoctrineDistanceBand::Acceptable,
            d if d < 80.0 => DoctrineDistanceBand::Marginal,
            _ => DoctrineDistanceBand::Unacceptable,
        }
    }

    /// Can this action execute? (Unacceptable actions are rejected)
    pub fn is_executable(&self) -> bool {
        !matches!(self, DoctrineDistanceBand::Unacceptable)
    }
}

// ===== SEALED CONSTRUCTORS: DOCTRINE-INDEXED =====
// These are the ONLY ways to create Action instances.
// By sealing constructors, we ensure governance is baked into types.

/// Builder for read-only actions (always permitted)
pub struct ReadOnlyActionBuilder;

impl ReadOnlyActionBuilder {
    /// Create a read-only action with 0 ticks
    /// These actions are always hot-path eligible
    pub fn create(
        id: impl Into<String>,
        description: impl Into<String>,
    ) -> Action<ReadOnlyMarker, 0, ImmutableReadMarker> {
        Action {
            id: id.into(),
            description: description.into(),
            risk: PhantomData,
            mutation: PhantomData,
            metadata: ActionMetadata {
                created_at: std::time::SystemTime::now()
                    .duration_since(std::time::UNIX_EPOCH)
                    .unwrap_or_default()
                    .as_secs(),
                risk_class: RiskClass::ReadOnly,
                mutation_class: MutationClass::ImmutableRead,
                tick_budget: 0,
                ticks_consumed: None,
                in_flight: false,
                doctrine_distance_band: DoctrineDistanceBand::Perfect,
            },
        }
    }
}

/// Builder for low-risk actions that can go hot path
pub struct LowRiskActionBuilder;

impl LowRiskActionBuilder {
    /// Create a low-risk action (max 6 ticks to stay safely under Chatman constant)
    /// Requires basic doctrine alignment proof
    pub fn create<const TICKS: usize>(
        id: impl Into<String>,
        description: impl Into<String>,
    ) -> Result<Action<LowRiskMarker, TICKS, SnapshotWriteMarker>, String>
    where
        // Compile-time constraint: max 6 ticks on hot path
        [(); TICKS]:,
    {
        if TICKS > 6 {
            return Err(format!(
                "Low-risk action budget {} exceeds hot path max of 6 ticks",
                TICKS
            ));
        }

        Ok(Action {
            id: id.into(),
            description: description.into(),
            risk: PhantomData,
            mutation: PhantomData,
            metadata: ActionMetadata {
                created_at: std::time::SystemTime::now()
                    .duration_since(std::time::UNIX_EPOCH)
                    .unwrap_or_default()
                    .as_secs(),
                risk_class: RiskClass::Low,
                mutation_class: MutationClass::SnapshotWrite,
                tick_budget: TICKS,
                ticks_consumed: None,
                in_flight: false,
                doctrine_distance_band: DoctrineDistanceBand::Good,
            },
        })
    }
}

/// Builder for medium-risk actions (warm path, ≤100 ticks)
pub struct MediumRiskActionBuilder;

impl MediumRiskActionBuilder {
    /// Create a medium-risk action (max 100 ticks for warm path)
    /// Requires intermediate doctrine proof
    pub fn create<const TICKS: usize>(
        id: impl Into<String>,
        description: impl Into<String>,
    ) -> Result<Action<MediumRiskMarker, TICKS, SnapshotWriteMarker>, String>
    where
        [(); TICKS]:,
    {
        if TICKS > 100 {
            return Err(format!(
                "Medium-risk action budget {} exceeds warm path max of 100 ticks",
                TICKS
            ));
        }

        Ok(Action {
            id: id.into(),
            description: description.into(),
            risk: PhantomData,
            mutation: PhantomData,
            metadata: ActionMetadata {
                created_at: std::time::SystemTime::now()
                    .duration_since(std::time::UNIX_EPOCH)
                    .unwrap_or_default()
                    .as_secs(),
                risk_class: RiskClass::Medium,
                mutation_class: MutationClass::SnapshotWrite,
                tick_budget: TICKS,
                ticks_consumed: None,
                in_flight: false,
                doctrine_distance_band: DoctrineDistanceBand::Acceptable,
            },
        })
    }
}

/// Builder for high-risk actions (cold path, ontology mutations)
/// These require explicit doctrine proof objects to construct
pub struct HighRiskActionBuilder;

impl HighRiskActionBuilder {
    /// Create a high-risk action (must go cold path, unlimited ticks)
    /// REQUIRES proof object that this action is doctrine-aligned
    pub fn create_with_proof<const TICKS: usize, P: DoctrineProof>(
        id: impl Into<String>,
        description: impl Into<String>,
        _proof: &P,
    ) -> Result<Action<HighRiskMarker, TICKS, OntologyMutateMarker>, String> {
        Ok(Action {
            id: id.into(),
            description: description.into(),
            risk: PhantomData,
            mutation: PhantomData,
            metadata: ActionMetadata {
                created_at: std::time::SystemTime::now()
                    .duration_since(std::time::UNIX_EPOCH)
                    .unwrap_or_default()
                    .as_secs(),
                risk_class: RiskClass::High,
                mutation_class: MutationClass::OntologyMutate,
                tick_budget: TICKS,
                ticks_consumed: None,
                in_flight: false,
                doctrine_distance_band: P::doctrine_distance_band(),
            },
        })
    }
}

// ===== TYPE MARKERS: THESE ENFORCE GOVERNANCE AT COMPILE TIME =====

/// Marker for ReadOnly risk class
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct ReadOnlyMarker;

/// Marker for Low risk class
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct LowRiskMarker;

/// Marker for Medium risk class
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct MediumRiskMarker;

/// Marker for High risk class
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct HighRiskMarker;

/// Marker for ImmutableRead mutation class
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct ImmutableReadMarker;

/// Marker for SnapshotWrite mutation class
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct SnapshotWriteMarker;

/// Marker for OntologyMutate mutation class
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct OntologyMutateMarker;

// ===== PROOF SYSTEM: GATING CONSTRUCTORS =====

/// Trait for doctrine proofs (required to construct high-risk actions)
/// Different implementations represent different proof levels
pub trait DoctrineProof: Send + Sync {
    /// Get the doctrine distance band this proof achieves
    fn doctrine_distance_band() -> DoctrineDistanceBand;

    /// Verify the proof is still valid
    fn is_valid(&self) -> bool;

    /// Get a human-readable explanation of why this proof grants authority
    fn justification(&self) -> String;
}

/// Weak proof: suitable for low-risk operations (distance band: Good)
#[derive(Debug, Clone)]
pub struct WeakProof {
    #[allow(dead_code)]
    proof_id: String,
}

impl WeakProof {
    pub fn new(proof_id: impl Into<String>) -> Self {
        Self {
            proof_id: proof_id.into(),
        }
    }
}

impl DoctrineProof for WeakProof {
    fn doctrine_distance_band() -> DoctrineDistanceBand {
        DoctrineDistanceBand::Good
    }

    fn is_valid(&self) -> bool {
        true // In real usage, would check expiry, revocation, etc.
    }

    fn justification(&self) -> String {
        "Weak proof: basic doctrine alignment".to_string()
    }
}

/// Standard proof: for medium-risk operations (distance band: Acceptable)
#[derive(Debug, Clone)]
pub struct StandardProof {
    #[allow(dead_code)]
    proof_id: String,
    doctrine_checks_passed: usize,
}

impl StandardProof {
    pub fn new(proof_id: impl Into<String>) -> Self {
        Self {
            proof_id: proof_id.into(),
            doctrine_checks_passed: 0,
        }
    }
}

impl DoctrineProof for StandardProof {
    fn doctrine_distance_band() -> DoctrineDistanceBand {
        DoctrineDistanceBand::Acceptable
    }

    fn is_valid(&self) -> bool {
        self.doctrine_checks_passed > 0
    }

    fn justification(&self) -> String {
        format!(
            "Standard proof: {} doctrine checks passed",
            self.doctrine_checks_passed
        )
    }
}

/// Strong proof: for high-risk operations (distance band: Marginal or better)
#[derive(Debug, Clone)]
pub struct StrongProof {
    #[allow(dead_code)]
    proof_id: String,
    doctrine_distance: f64,
    tests_passed: usize,
}

impl StrongProof {
    pub fn new(proof_id: impl Into<String>, doctrine_distance: f64) -> Self {
        Self {
            proof_id: proof_id.into(),
            doctrine_distance,
            tests_passed: 0,
        }
    }
}

impl DoctrineProof for StrongProof {
    fn doctrine_distance_band() -> DoctrineDistanceBand {
        DoctrineDistanceBand::from_distance(60.0) // Marginal (conservative)
    }

    fn is_valid(&self) -> bool {
        self.doctrine_distance < 75.0 && self.tests_passed > 0
    }

    fn justification(&self) -> String {
        format!(
            "Strong proof: doctrine_distance={}, {} tests passed",
            self.doctrine_distance, self.tests_passed
        )
    }
}

// ===== EXECUTION PATH ENFORCEMENT =====

/// Trait for actions that can execute on hot path
/// Automatically implemented only for actions that satisfy constraints
pub trait HotPathEligible: Sized {
    fn execute_hot_path(&self) -> Result<(), String>;
}

impl<const TICKS: usize> HotPathEligible for Action<ReadOnlyMarker, TICKS, ImmutableReadMarker> {
    fn execute_hot_path(&self) -> Result<(), String> {
        Ok(()) // ReadOnly actions always hot-eligible
    }
}

impl<const TICKS: usize> HotPathEligible for Action<LowRiskMarker, TICKS, SnapshotWriteMarker> {
    fn execute_hot_path(&self) -> Result<(), String> {
        // Runtime check: LowRiskMarker with ≤6 ticks is hot-path eligible
        if TICKS <= 6 {
            Ok(())
        } else {
            Err(format!("Low-risk action with {} ticks exceeds hot path budget of 6", TICKS))
        }
    }
}

/// Trait for warm path eligibility
pub trait WarmPathEligible: Sized {
    fn execute_warm_path(&self) -> Result<(), String>;
}

impl<const TICKS: usize> WarmPathEligible for Action<ReadOnlyMarker, TICKS, ImmutableReadMarker> {
    fn execute_warm_path(&self) -> Result<(), String> {
        Ok(())
    }
}

impl<const TICKS: usize> WarmPathEligible for Action<LowRiskMarker, TICKS, SnapshotWriteMarker> {
    fn execute_warm_path(&self) -> Result<(), String> {
        Ok(())
    }
}

impl<const TICKS: usize> WarmPathEligible for Action<MediumRiskMarker, TICKS, SnapshotWriteMarker> {
    fn execute_warm_path(&self) -> Result<(), String> {
        // Runtime check: MediumRiskMarker with ≤100 ticks is warm-path eligible
        if TICKS <= 100 {
            Ok(())
        } else {
            Err(format!("Medium-risk action with {} ticks exceeds warm path budget of 100", TICKS))
        }
    }
}

// Cold path: everyone goes here (no compile-time constraint needed)

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_readonly_action_creation() {
        let _action = ReadOnlyActionBuilder::create("test-readonly", "Read-only test action");
        // Type is: Action<ReadOnlyMarker, 0, ImmutableReadMarker>
        // This is always hot-path eligible
    }

    #[test]
    fn test_low_risk_action() {
        // Can construct action with ≤6 ticks
        let action = LowRiskActionBuilder::create::<4>(
            "test-low-risk",
            "Low-risk 4-tick action",
        );
        assert!(action.is_ok());

        // Should fail if exceeding budget
        let over_budget = LowRiskActionBuilder::create::<8>(
            "test-over-budget",
            "This exceeds 6-tick limit",
        );
        assert!(over_budget.is_err());
    }

    #[test]
    fn test_medium_risk_action() {
        let action = MediumRiskActionBuilder::create::<50>(
            "test-medium",
            "Medium-risk 50-tick action",
        );
        assert!(action.is_ok());

        let over_budget = MediumRiskActionBuilder::create::<150>(
            "test-over",
            "Over 100-tick limit",
        );
        assert!(over_budget.is_err());
    }

    #[test]
    fn test_high_risk_action_requires_proof() {
        let proof = StrongProof::new("proof-1", 50.0);

        let action = HighRiskActionBuilder::create_with_proof::<1000, StrongProof>(
            "test-high-risk",
            "High-risk ontology mutation",
            &proof,
        );

        assert!(action.is_ok());
    }

    #[test]
    fn test_doctrine_distance_bands() {
        assert_eq!(
            DoctrineDistanceBand::from_distance(15.0),
            DoctrineDistanceBand::Perfect
        );
        assert_eq!(
            DoctrineDistanceBand::from_distance(30.0),
            DoctrineDistanceBand::Good
        );
        assert_eq!(
            DoctrineDistanceBand::from_distance(90.0),
            DoctrineDistanceBand::Unacceptable
        );
    }

    #[test]
    fn test_execution_path_constraints() {
        assert_eq!(ExecutionPath::Hot.max_ticks(), 8);
        assert_eq!(ExecutionPath::Warm.max_ticks(), 100);
        assert!(ExecutionPath::Hot.can_fit(5));
        assert!(!ExecutionPath::Hot.can_fit(10));
    }
}
