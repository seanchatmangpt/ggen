//! > 📚 Reference
//!
//! Fail-Fast Hardening Infrastructure
//!
//! Core type system for unrecoverable invariant violations.
//! Every invariant violation results in immediate test failure.
//! Zero tolerance, no graceful degradation.
//!
//! This module defines all 47 invariant violation types across the 12 phases
//! of the testing framework, plus validators for each phase.

use std::error::Error;
use std::fmt::{self, Display};

/// > 📚 Reference
///
/// The single enum for all internal framework invariant violations.
/// If any of these occurs, the framework cannot certify correctness;
/// the test fails immediately.
///
/// # Examples
///
/// ```rust
/// use chicago_tdd_tools::core::invariants::UnrecoverableInvariantViolation;
///
/// let violation = UnrecoverableInvariantViolation::ContractMalformed("empty ID".to_string());
/// assert!(format!("{}", violation).contains("Contract malformed"));
/// ```
#[derive(Debug, Clone)]
pub enum UnrecoverableInvariantViolation {
    /// Phase 1: Contract Definition is incomplete or malformed
    ContractMalformed(String),
    /// Phase 1: Two contracts registered with identical IDs
    DuplicateContractId(String),
    /// Phase 1: Phases defined out of order or with gaps
    InvalidPhaseSequence(String),
    /// Phase 1: Contract object reached a later phase without passing validation
    ContractEscapedValidation(String),
    /// Phase 1: Contract metadata corrupted (NaN bounds, negative timeouts)
    MalformedMetadata(String),

    /// Phase 2: τ measurement decreased (clock went backward)
    ClockBackward {
        /// Previous τ value
        prev: u64,
        /// Current τ value (less than previous)
        current: u64,
    },
    /// Phase 2: τ jumped by more than configured threshold
    ClockMonsterJump {
        /// Previous τ value
        prev: u64,
        /// Current τ value
        current: u64,
        /// Configured maximum jump threshold
        threshold: u64,
    },
    /// Phase 2: Cannot measure thermal (RDTSC unavailable or broken)
    ThermalCannotMeasure(String),
    /// Phase 2: Arithmetic error during τ calculation
    ThermalComputationCorrupted(String),

    /// Phase 3: Runtime observed an effect not declared in contract
    UnobservedEffect(String),
    /// Phase 3: Declared effect not observed (incomplete instrumentation)
    EffectLost(String),
    /// Phase 3: Effect set composition/union/restrict operation failed
    EffectCompositionError(String),
    /// Phase 3: Effect algebra law violation (associativity, idempotence, etc.)
    EffectAlgebraViolation(String),

    /// Phase 4: State transition not defined for this (state, event) pair
    UnhandledStateEvent {
        /// Current state
        state: String,
        /// Event that occurred
        event: String,
    },
    /// Phase 4: State unreachable from initial state (dead state detected)
    DeadState(String),
    /// Phase 4: Transition violates contract invariants or phase ordering
    InvalidStateTransition(String),
    /// Phase 4: State field mutated outside normal transition mechanism
    StateCorruption(String),
    /// Phase 4: Detected invalid cycle in state machine graph
    CyclicPathViolation(String),

    /// Phase 5: Test executed but no receipt was produced
    MissingReceipt(String),
    /// Phase 5: Receipt checksum/hash validation failed
    CorruptedReceipt(String),
    /// Phase 5: Receipt missing required fields (incomplete)
    PartialReceipt(String),
    /// Phase 5: Receipt schema version not supported
    ReceiptVersionMismatch {
        /// Expected schema version
        expected: u32,
        /// Version found in receipt
        found: u32,
    },
    /// Phase 5: Cannot persist receipt to storage
    ReceiptPersistenceFailure(String),

    /// Phase 6: Test was scheduled but never executed or timed out
    AbandonedTest(String),
    /// Phase 6: Same contract ID executed twice in same run
    DuplicateExecution(String),
    /// Phase 6: Orchestration queue corrupted (circular refs, lost items)
    OrchestrationQueueCorruption(String),

    /// Phase 7: Pipeline claims success but not all phases executed
    PartialPipelineSuccess(String),
    /// Phase 7: Attempted to use removed "relaxed" or invalid pipeline config
    PipelineConfigInvalid(String),
    /// Phase 7: Contract phase defined but pipeline skipped it
    PipelinePhaseSkipped(String),
    /// Phase 7: Pipeline state machine reached impossible intermediate state
    PipelineInternalStateCorruption(String),

    /// Phase 8: Learner math produced NaN/Inf, corrupting state
    LearnerMathCorrupted(String),
    /// Phase 8: Learner state is corrupted and cannot be recovered
    LearnerLockout(String),
    /// Phase 8: Learner ordering violates phase dependencies
    BadPredictionOrdering(String),

    /// Phase 9: Consensus vote failed signature verification
    InvalidConsensusVote(String),
    /// Phase 9: Vote claims invalid voter identity
    VoteIdentityCorruption(String),
    /// Phase 9: Node not in validator set attempted to vote
    UnauthorizedVoter(String),
    /// Phase 9: Voting round cannot progress (deadlock detected)
    ConsensusDeadlock(String),

    /// Phase 10: Snapshot from incompatible schema version
    SnapshotSchemaVersionMismatch {
        /// Expected schema version
        expected: u32,
        /// Version found in snapshot
        found: u32,
    },
    /// Phase 10: Replayed execution diverges from original result
    ReplayDiverges(String),
    /// Phase 10: Snapshot write failed (missing observability)
    SnapshotLost(String),
    /// Phase 10: Snapshot data corrupted (detected via checksum)
    SnapshotCorrupted(String),
    /// Phase 10: Same snapshot replayed twice with different results
    NonDeterministicReplay(String),

    /// Phase 11: Prophet self-check failed (negative ticks, invalid bounds)
    ProphetSelfCheckFailed(String),
    /// Phase 11: Attempted to override measured τ with prediction
    PredictionOverridesActual(String),
    /// Phase 11: Confidence calculation caused numeric overflow
    PredictionMathOverflow(String),
    /// Phase 11: Historical data used for training contains NaN/Inf
    ProphetTrainingDataInvalid(String),

    /// Phase 12: Dashboard metrics don't sum correctly
    DashboardInconsistency(String),
    /// Phase 12: Metrics computed via heuristic instead of exact receipts
    ApproximateMetrics(String),
    /// Phase 12: Cannot serialize dashboard to JSON/HTML
    DashboardRenderFailure(String),
    /// Phase 12: Dashboard references non-existent receipt
    MissingReceiptDataInDashboard(String),
    /// Phase 12: Aggregated metrics have invalid values (negative counts)
    CorruptedAggregations(String),

    /// Generic fallback for unforeseen violations
    Other(String),
}

impl Display for UnrecoverableInvariantViolation {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::ContractMalformed(msg) => write!(f, "Contract malformed: {msg}"),
            Self::DuplicateContractId(id) => write!(f, "Duplicate contract ID: {id}"),
            Self::InvalidPhaseSequence(msg) => write!(f, "Invalid phase sequence: {msg}"),
            Self::ContractEscapedValidation(msg) => write!(f, "Contract escaped validation: {msg}"),
            Self::MalformedMetadata(msg) => write!(f, "Malformed metadata: {msg}"),

            Self::ClockBackward { prev, current } => {
                write!(f, "Clock went backward: {prev} -> {current}")
            }
            Self::ClockMonsterJump { prev, current, threshold } => {
                write!(f, "Clock monster jump: {prev} -> {current} (threshold: {threshold})")
            }
            Self::ThermalCannotMeasure(msg) => write!(f, "Cannot measure thermal: {msg}"),
            Self::ThermalComputationCorrupted(msg) => {
                write!(f, "Thermal computation corrupted: {msg}")
            }

            Self::UnobservedEffect(msg) => write!(f, "Unobserved effect: {msg}"),
            Self::EffectLost(msg) => write!(f, "Effect lost: {msg}"),
            Self::EffectCompositionError(msg) => write!(f, "Effect composition error: {msg}"),
            Self::EffectAlgebraViolation(msg) => write!(f, "Effect algebra violation: {msg}"),

            Self::UnhandledStateEvent { state, event } => {
                write!(f, "Unhandled state event: {event} in state '{state}'")
            }
            Self::DeadState(state) => write!(f, "Dead state (unreachable): {state}"),
            Self::InvalidStateTransition(msg) => write!(f, "Invalid state transition: {msg}"),
            Self::StateCorruption(msg) => write!(f, "State corruption: {msg}"),
            Self::CyclicPathViolation(msg) => write!(f, "Cyclic path violation: {msg}"),

            Self::MissingReceipt(msg) => write!(f, "Missing receipt: {msg}"),
            Self::CorruptedReceipt(msg) => write!(f, "Corrupted receipt: {msg}"),
            Self::PartialReceipt(msg) => write!(f, "Partial receipt: {msg}"),
            Self::ReceiptVersionMismatch { expected, found } => {
                write!(f, "Receipt version mismatch: expected {expected}, found {found}")
            }
            Self::ReceiptPersistenceFailure(msg) => write!(f, "Receipt persistence failure: {msg}"),

            Self::AbandonedTest(msg) => write!(f, "Abandoned test: {msg}"),
            Self::DuplicateExecution(msg) => write!(f, "Duplicate execution: {msg}"),
            Self::OrchestrationQueueCorruption(msg) => write!(f, "Queue corruption: {msg}"),

            Self::PartialPipelineSuccess(msg) => write!(f, "Partial pipeline success: {msg}"),
            Self::PipelineConfigInvalid(msg) => write!(f, "Pipeline config invalid: {msg}"),
            Self::PipelinePhaseSkipped(msg) => write!(f, "Pipeline phase skipped: {msg}"),
            Self::PipelineInternalStateCorruption(msg) => {
                write!(f, "Pipeline state corruption: {msg}")
            }

            Self::LearnerMathCorrupted(msg) => write!(f, "Learner math corrupted: {msg}"),
            Self::LearnerLockout(msg) => write!(f, "Learner lockout: {msg}"),
            Self::BadPredictionOrdering(msg) => write!(f, "Bad prediction ordering: {msg}"),

            Self::InvalidConsensusVote(msg) => write!(f, "Invalid consensus vote: {msg}"),
            Self::VoteIdentityCorruption(msg) => write!(f, "Vote identity corruption: {msg}"),
            Self::UnauthorizedVoter(msg) => write!(f, "Unauthorized voter: {msg}"),
            Self::ConsensusDeadlock(msg) => write!(f, "Consensus deadlock: {msg}"),

            Self::SnapshotSchemaVersionMismatch { expected, found } => {
                write!(f, "Snapshot schema version mismatch: expected {expected}, found {found}")
            }
            Self::ReplayDiverges(msg) => write!(f, "Replay diverges: {msg}"),
            Self::SnapshotLost(msg) => write!(f, "Snapshot lost: {msg}"),
            Self::SnapshotCorrupted(msg) => write!(f, "Snapshot corrupted: {msg}"),
            Self::NonDeterministicReplay(msg) => write!(f, "Non-deterministic replay: {msg}"),

            Self::ProphetSelfCheckFailed(msg) => write!(f, "Prophet self-check failed: {msg}"),
            Self::PredictionOverridesActual(msg) => write!(f, "Prediction overrides actual: {msg}"),
            Self::PredictionMathOverflow(msg) => write!(f, "Prediction math overflow: {msg}"),
            Self::ProphetTrainingDataInvalid(msg) => {
                write!(f, "Prophet training data invalid: {msg}")
            }

            Self::DashboardInconsistency(msg) => write!(f, "Dashboard inconsistency: {msg}"),
            Self::ApproximateMetrics(msg) => write!(f, "Approximate metrics: {msg}"),
            Self::DashboardRenderFailure(msg) => write!(f, "Dashboard render failure: {msg}"),
            Self::MissingReceiptDataInDashboard(msg) => {
                write!(f, "Missing receipt data in dashboard: {msg}")
            }
            Self::CorruptedAggregations(msg) => write!(f, "Corrupted aggregations: {msg}"),

            Self::Other(msg) => write!(f, "Invariant violation: {msg}"),
        }
    }
}

impl Error for UnrecoverableInvariantViolation {}

/// Result type for operations that can violate framework invariants.
/// The `Err` variant always means test failure; no "degraded" state.
pub type InvariantResult<T> = Result<T, UnrecoverableInvariantViolation>;

/// Trait for types that enforce their own invariants.
/// Used by all major components to validate state transitions.
pub trait InvariantCheck: Sized {
    /// Verify this type maintains all internal invariants.
    /// Returns `Ok(())` if invariants hold, `Err(...)` otherwise.
    /// Must not panic; violations are reported via Result.
    ///
    /// # Errors
    ///
    /// Returns an error if any invariant is violated.
    fn check_invariants(&self) -> InvariantResult<()>;
}

/// Helper macro for concise invariant checking in phase implementations.
///
/// Usage:
/// ```ignore
/// ensure_invariant!(condition, InvariantViolation::SomeFailure("message".to_string()))
/// ```
#[macro_export]
macro_rules! ensure_invariant {
    ($cond:expr, $err:expr) => {
        if !($cond) {
            return Err($err);
        }
    };
}

/// Helper macro to convert a custom error into an `InvariantViolation`.
/// Ensures no unwrap/expect escape in invariant-critical paths.
#[macro_export]
macro_rules! invariant_context {
    ($result:expr, $fallback:expr) => {
        match $result {
            Ok(v) => v,
            Err(e) => return Err($fallback(e.to_string())),
        }
    };
}

// ============================================================================
// Phase-Specific Validators
// ============================================================================

/// > 📚 Reference
///
/// Validates contract construction invariants.
///
/// # Examples
///
/// ```rust
/// use chicago_tdd_tools::core::invariants::ContractValidator;
///
/// assert!(ContractValidator::validate("my_contract", 3).is_ok());
/// assert!(ContractValidator::validate("", 3).is_err());
/// ```
pub struct ContractValidator;

impl ContractValidator {
    /// Verify contract is completely specified and valid.
    ///
    /// # Errors
    ///
    /// Returns an error if the contract ID is empty or phase count is zero.
    pub fn validate(contract_id: &str, phase_count: usize) -> InvariantResult<()> {
        ensure_invariant!(
            !contract_id.is_empty(),
            UnrecoverableInvariantViolation::ContractMalformed("empty ID".to_string())
        );

        ensure_invariant!(
            phase_count > 0,
            UnrecoverableInvariantViolation::InvalidPhaseSequence("no phases defined".to_string())
        );

        Ok(())
    }
}

/// Validates thermal (τ) measurement invariants.
pub struct ThermalValidator {
    last_tau: Option<u64>,
    max_jump_threshold: u64,
}

impl ThermalValidator {
    /// Create a new thermal validator with configured jump threshold.
    #[must_use]
    pub const fn new(max_jump_threshold: u64) -> Self {
        Self { last_tau: None, max_jump_threshold }
    }

    /// Validate new τ measurement for monotonicity and bounds.
    ///
    /// # Errors
    ///
    /// Returns an error if the clock goes backward or jumps exceed the threshold.
    #[allow(clippy::missing_const_for_fn)] // Cannot be const: mutates self
    pub fn validate_tau(&mut self, current_tau: u64) -> InvariantResult<()> {
        if let Some(prev) = self.last_tau {
            ensure_invariant!(
                current_tau >= prev,
                UnrecoverableInvariantViolation::ClockBackward { prev, current: current_tau }
            );

            let jump = current_tau.saturating_sub(prev);
            ensure_invariant!(
                jump <= self.max_jump_threshold,
                UnrecoverableInvariantViolation::ClockMonsterJump {
                    prev,
                    current: current_tau,
                    threshold: self.max_jump_threshold,
                }
            );
        }

        self.last_tau = Some(current_tau);
        Ok(())
    }
}

/// Validates effect set invariants.
pub struct EffectValidator {
    declared_effects: std::collections::BTreeSet<String>,
}

impl EffectValidator {
    /// Create a new effect validator with declared effect set.
    ///
    /// # Errors
    ///
    /// Returns an error if the declared effect set is empty.
    pub fn new(declared: Vec<String>) -> InvariantResult<Self> {
        ensure_invariant!(
            !declared.is_empty(),
            UnrecoverableInvariantViolation::EffectCompositionError(
                "no effects declared".to_string()
            )
        );

        Ok(Self { declared_effects: declared.into_iter().collect() })
    }

    /// Verify observed effects are a subset of declared effects.
    ///
    /// # Errors
    ///
    /// Returns an error if any observed effect is not in the declared set.
    pub fn validate_observed(&self, observed: &[String]) -> InvariantResult<()> {
        for effect in observed {
            ensure_invariant!(
                self.declared_effects.contains(effect),
                UnrecoverableInvariantViolation::UnobservedEffect(format!(
                    "effect '{effect}' not declared"
                ))
            );
        }
        Ok(())
    }
}

/// Validates state machine transition invariants.
pub struct StateValidator {
    reachable_states: std::collections::BTreeSet<String>,
    current_state: String,
}

impl StateValidator {
    /// Create a new state validator with initial state and complete state set.
    ///
    /// # Errors
    ///
    /// Returns an error if the initial state is empty or not in the state set.
    pub fn new(initial_state: String, all_states: Vec<String>) -> InvariantResult<Self> {
        ensure_invariant!(
            !initial_state.is_empty(),
            UnrecoverableInvariantViolation::InvalidStateTransition(
                "empty initial state".to_string()
            )
        );

        let all_set: std::collections::BTreeSet<_> = all_states.into_iter().collect();
        ensure_invariant!(
            all_set.contains(&initial_state),
            UnrecoverableInvariantViolation::ContractMalformed(
                "initial state not in state set".to_string()
            )
        );

        Ok(Self {
            reachable_states: std::collections::BTreeSet::from([initial_state.clone()]),
            current_state: initial_state,
        })
    }

    /// Verify transition to next state is valid.
    ///
    /// # Errors
    ///
    /// Returns an error if the next state is empty.
    pub fn validate_transition(&mut self, next_state: &str) -> InvariantResult<()> {
        ensure_invariant!(
            !next_state.is_empty(),
            UnrecoverableInvariantViolation::InvalidStateTransition("empty next state".to_string())
        );

        self.current_state = next_state.to_string();
        self.reachable_states.insert(next_state.to_string());
        Ok(())
    }
}

/// Validates receipt structure and integrity.
pub struct ReceiptValidator {
    expected_version: u32,
}

impl ReceiptValidator {
    /// Create a new receipt validator expecting a specific schema version.
    #[must_use]
    pub const fn new(version: u32) -> Self {
        Self { expected_version: version }
    }

    /// Verify receipt has all required fields and valid version.
    ///
    /// # Errors
    ///
    /// Returns an error if the version doesn't match or checksums don't match.
    pub fn validate_receipt(
        &self,
        version: u32,
        checksum: u32,
        computed: u32,
    ) -> InvariantResult<()> {
        ensure_invariant!(
            version == self.expected_version,
            UnrecoverableInvariantViolation::ReceiptVersionMismatch {
                expected: self.expected_version,
                found: version,
            }
        );

        ensure_invariant!(
            checksum == computed,
            UnrecoverableInvariantViolation::CorruptedReceipt(format!(
                "checksum mismatch: {checksum} vs {computed}"
            ))
        );

        Ok(())
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_thermal_validator_monotonic() {
        let mut tv = ThermalValidator::new(1000);
        assert!(tv.validate_tau(100).is_ok());
        assert!(tv.validate_tau(200).is_ok());
        assert!(tv.validate_tau(150).is_err()); // Goes backward
    }

    #[test]
    fn test_thermal_validator_jump() {
        let mut tv = ThermalValidator::new(1000);
        assert!(tv.validate_tau(100).is_ok());
        assert!(tv.validate_tau(2000).is_err()); // Jump > threshold
    }

    #[test]
    fn test_effect_validator_observed_outside_declared() {
        let ev = EffectValidator::new(vec!["A".to_string(), "B".to_string()]).unwrap();
        assert!(ev.validate_observed(&["A".to_string(), "B".to_string()]).is_ok());
        assert!(ev.validate_observed(&["A".to_string(), "C".to_string()]).is_err());
    }

    #[test]
    fn test_state_validator_transition() {
        let mut sv = StateValidator::new(
            "Init".to_string(),
            vec!["Init".to_string(), "Running".to_string(), "Done".to_string()],
        )
        .unwrap();
        assert!(sv.validate_transition("Running").is_ok());
        assert!(sv.validate_transition("Done").is_ok());
    }

    #[test]
    fn test_receipt_validator_version_mismatch() {
        let rv = ReceiptValidator::new(1);
        assert!(rv.validate_receipt(2, 0x1234, 0x1234).is_err());
    }

    #[test]
    fn test_receipt_validator_checksum_fail() {
        let rv = ReceiptValidator::new(1);
        assert!(rv.validate_receipt(1, 0x1234, 0x5678).is_err());
    }
}
