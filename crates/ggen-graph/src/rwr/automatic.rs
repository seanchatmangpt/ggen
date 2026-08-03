//! Fully automatic and autonomic operating kernel over the RWR execution rail.
//!
//! The kernel preserves zero-unreceipted actuation. Triggers are admitted,
//! deterministically routed, converted into bounded intents, granted exact
//! authority, actuated atomically, re-observed, and bound into causal receipts.

use crate::rwr::execution::{
    Action, ActuationReceipt, ExecutionError, FilesystemActuator, FoundationMachine,
};
use crate::rwr::matrix::Dimension;
use serde::{Deserialize, Serialize};
use std::collections::{BTreeMap, BTreeSet};
use std::fs::File;
use std::io::Read;

/// Version of the automatic/autonomic operating contract.
pub const OPERATIONS_VERSION: &str = "ggen-automatic-autonomic/v1";

fn put_len_prefixed(hasher: &mut blake3::Hasher, value: &[u8]) {
    hasher.update(&(value.len() as u64).to_le_bytes());
    hasher.update(value);
}

fn safe_token(value: &str) -> bool {
    !value.is_empty()
        && value.len() <= 128
        && value
            .bytes()
            .all(|byte| byte.is_ascii_alphanumeric() || matches!(byte, b'-' | b'_' | b'.'))
}

fn safe_action_id(value: &str) -> bool {
    !value.is_empty()
        && value.len() <= 128
        && value
            .bytes()
            .all(|byte| byte.is_ascii_alphanumeric() || matches!(byte, b'-' | b'_'))
}

/// Capabilities required for a fully automatic and autonomic operating rail.
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash, Serialize, Deserialize)]
#[repr(u8)]
pub enum OperationsCapability {
    /// Partial events are admitted into bounded observations.
    TriggerAdmission = 1,
    /// An admitted trigger maps to exactly one versioned route.
    DeterministicRouting = 2,
    /// Routes manufacture deterministic intents rather than direct side effects.
    IntentManufacturing = 3,
    /// Authority is derived for exactly one action and payload.
    ExactGrantDerivation = 4,
    /// Artifacts and receipts become visible atomically.
    AtomicActuation = 5,
    /// Execution is re-observed and checked against an explicit postcondition.
    ConsequenceVerification = 6,
    /// One trigger identity cannot actuate twice.
    Idempotency = 7,
    /// Transient observation failures retry within a deterministic bound.
    BoundedRetry = 8,
    /// Repeated failures open a circuit and refuse further work.
    CircuitBreaking = 9,
    /// Risky work consumes a bounded error budget.
    ErrorBudgetGating = 10,
    /// Operational severity escalates monotonically and red stops execution.
    AndonStopLine = 11,
    /// Failed postconditions execute an admitted inverse when available.
    LawfulRollback = 12,
    /// A bounded MAPE-K loop repairs divergent state or refuses.
    BoundedAutonomicRepair = 13,
    /// Receipts form a causal, tamper-evident, replay-protected chain.
    CausalReceiptReplay = 14,
    /// Consequences emit non-actuating knowledge hooks for downstream routing.
    KnowledgeHookEmission = 15,
}

/// Stable ordered capability inventory.
pub const ALL_OPERATIONS_CAPABILITIES: [OperationsCapability; 15] = [
    OperationsCapability::TriggerAdmission,
    OperationsCapability::DeterministicRouting,
    OperationsCapability::IntentManufacturing,
    OperationsCapability::ExactGrantDerivation,
    OperationsCapability::AtomicActuation,
    OperationsCapability::ConsequenceVerification,
    OperationsCapability::Idempotency,
    OperationsCapability::BoundedRetry,
    OperationsCapability::CircuitBreaking,
    OperationsCapability::ErrorBudgetGating,
    OperationsCapability::AndonStopLine,
    OperationsCapability::LawfulRollback,
    OperationsCapability::BoundedAutonomicRepair,
    OperationsCapability::CausalReceiptReplay,
    OperationsCapability::KnowledgeHookEmission,
];

/// Independent proof surfaces required for every capability.
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash, Serialize, Deserialize)]
#[repr(u8)]
pub enum OperationsProofSurface {
    /// The capability crosses a real execution or observation boundary.
    PositiveExecution = 1,
    /// A named falsifier is mechanically refused.
    NegativeRefusal = 2,
    /// The resulting evidence verifies and duplicate replay is refused.
    ReceiptReplay = 3,
}

/// Stable proof-surface inventory.
pub const REQUIRED_OPERATIONS_PROOF_SURFACES: [OperationsProofSurface; 3] = [
    OperationsProofSurface::PositiveExecution,
    OperationsProofSurface::NegativeRefusal,
    OperationsProofSurface::ReceiptReplay,
];

/// One admitted external event.
#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
pub struct Trigger {
    /// Stable idempotency identity.
    pub id: String,
    /// Versioned event kind used by deterministic routing.
    pub kind: String,
    /// Monotonic source sequence.
    pub sequence: u64,
    /// Event payload.
    pub payload: Vec<u8>,
    /// Claimed BLAKE3 payload digest.
    pub expected_payload_digest: [u8; 32],
}

impl Trigger {
    /// Construct a trigger and bind its payload digest.
    #[must_use]
    pub fn new(
        id: impl Into<String>, kind: impl Into<String>, sequence: u64, payload: Vec<u8>,
    ) -> Self {
        let expected_payload_digest = blake3::hash(&payload).into();
        Self {
            id: id.into(),
            kind: kind.into(),
            sequence,
            payload,
            expected_payload_digest,
        }
    }

    /// Verify the trigger and return its canonical digest.
    pub fn digest(&self) -> Result<[u8; 32], OperationsError> {
        if !safe_action_id(&self.id) {
            return Err(OperationsError::InvalidTriggerId(self.id.clone()));
        }
        if !safe_token(&self.kind) {
            return Err(OperationsError::InvalidTriggerKind(self.kind.clone()));
        }
        if self.sequence == 0 {
            return Err(OperationsError::InvalidTriggerSequence);
        }
        let observed: [u8; 32] = blake3::hash(&self.payload).into();
        if observed != self.expected_payload_digest {
            return Err(OperationsError::TriggerPayloadDigestMismatch(
                self.id.clone(),
            ));
        }
        let mut hasher = blake3::Hasher::new();
        put_len_prefixed(&mut hasher, b"automatic-trigger/v1");
        put_len_prefixed(&mut hasher, self.id.as_bytes());
        put_len_prefixed(&mut hasher, self.kind.as_bytes());
        hasher.update(&self.sequence.to_le_bytes());
        hasher.update(&self.expected_payload_digest);
        Ok(hasher.finalize().into())
    }
}

/// Admission policy for partial external triggers.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct TriggerAdmissionPolicy {
    allowed_kinds: BTreeSet<String>,
    max_payload_bytes: usize,
    max_sequence: u64,
}

impl TriggerAdmissionPolicy {
    /// Construct a bounded trigger-admission policy.
    #[must_use]
    pub fn new<I, S>(allowed_kinds: I, max_payload_bytes: usize, max_sequence: u64) -> Self
    where
        I: IntoIterator<Item = S>,
        S: Into<String>,
    {
        Self {
            allowed_kinds: allowed_kinds.into_iter().map(Into::into).collect(),
            max_payload_bytes,
            max_sequence,
        }
    }

    /// Admit one trigger into bounded observation state.
    pub fn admit(&self, trigger: Trigger) -> Result<AdmittedTrigger, OperationsError> {
        let trigger_digest = trigger.digest()?;
        if !self.allowed_kinds.contains(&trigger.kind) {
            return Err(OperationsError::TriggerKindRefused(trigger.kind));
        }
        if trigger.payload.len() > self.max_payload_bytes {
            return Err(OperationsError::TriggerPayloadBoundExceeded {
                observed: trigger.payload.len(),
                maximum: self.max_payload_bytes,
            });
        }
        if trigger.sequence > self.max_sequence {
            return Err(OperationsError::TriggerSequenceBoundExceeded {
                observed: trigger.sequence,
                maximum: self.max_sequence,
            });
        }
        Ok(AdmittedTrigger {
            trigger,
            trigger_digest,
        })
    }
}

/// Trigger after admission and digest verification.
#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
pub struct AdmittedTrigger {
    /// Original trigger.
    pub trigger: Trigger,
    /// Canonical trigger digest.
    pub trigger_digest: [u8; 32],
}

/// Deterministic trigger route.
#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
pub struct Route {
    /// Trigger kind accepted by this route.
    pub trigger_kind: String,
    /// RWR dimension whose bounded machinery performs the action.
    pub dimension: Dimension,
    /// Stable intent name used to derive action identities.
    pub intent_name: String,
}

impl Route {
    /// Construct and validate a route.
    pub fn new(
        trigger_kind: impl Into<String>, dimension: Dimension, intent_name: impl Into<String>,
    ) -> Result<Self, OperationsError> {
        let route = Self {
            trigger_kind: trigger_kind.into(),
            dimension,
            intent_name: intent_name.into(),
        };
        route.digest()?;
        Ok(route)
    }

    /// Return the canonical route digest.
    pub fn digest(&self) -> Result<[u8; 32], OperationsError> {
        if !safe_token(&self.trigger_kind) || !safe_action_id(&self.intent_name) {
            return Err(OperationsError::InvalidRoute);
        }
        let mut hasher = blake3::Hasher::new();
        put_len_prefixed(&mut hasher, b"automatic-route/v1");
        put_len_prefixed(&mut hasher, self.trigger_kind.as_bytes());
        hasher.update(&[self.dimension as u8]);
        put_len_prefixed(&mut hasher, self.intent_name.as_bytes());
        Ok(hasher.finalize().into())
    }
}

/// Finite deterministic router. Duplicate kinds are refused.
#[derive(Debug, Clone, Default, PartialEq, Eq)]
pub struct TriggerRouter {
    routes: BTreeMap<String, Route>,
}

impl TriggerRouter {
    /// Construct an empty router.
    #[must_use]
    pub const fn new() -> Self {
        Self {
            routes: BTreeMap::new(),
        }
    }

    /// Add one unique route.
    pub fn insert(&mut self, route: Route) -> Result<(), OperationsError> {
        route.digest()?;
        if self.routes.contains_key(&route.trigger_kind) {
            return Err(OperationsError::DuplicateRoute(route.trigger_kind));
        }
        self.routes.insert(route.trigger_kind.clone(), route);
        Ok(())
    }

    /// Resolve exactly one route for an admitted trigger.
    pub fn resolve(&self, trigger: &AdmittedTrigger) -> Result<&Route, OperationsError> {
        self.routes
            .get(&trigger.trigger.kind)
            .ok_or_else(|| OperationsError::RouteNotFound(trigger.trigger.kind.clone()))
    }
}

/// Deterministically manufactured intent.
#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
pub struct ManufacturedIntent {
    /// Safe action identity.
    pub id: String,
    /// Cause admitted by the trigger boundary.
    pub trigger_digest: [u8; 32],
    /// Route contract used to manufacture the intent.
    pub route_digest: [u8; 32],
    /// Execution dimension.
    pub dimension: Dimension,
    /// Desired consequence.
    pub desired_state: Vec<u8>,
    /// Optional lawful inverse.
    pub inverse_state: Option<Vec<u8>>,
    /// Digest binding the complete intent.
    pub intent_digest: [u8; 32],
}

impl ManufacturedIntent {
    fn manufacture(
        trigger: &AdmittedTrigger, route: &Route, inverse_state: Option<Vec<u8>>,
    ) -> Result<Self, OperationsError> {
        let route_digest = route.digest()?;
        let id = format!(
            "{}-{}-{}",
            route.intent_name, trigger.trigger.id, trigger.trigger.sequence
        );
        if !safe_action_id(&id) {
            return Err(OperationsError::InvalidIntentId(id));
        }
        let intent_digest = intent_digest(
            &id,
            &trigger.trigger_digest,
            &route_digest,
            route.dimension,
            &trigger.trigger.payload,
            inverse_state.as_deref(),
        );
        Ok(Self {
            id,
            trigger_digest: trigger.trigger_digest,
            route_digest,
            dimension: route.dimension,
            desired_state: trigger.trigger.payload.clone(),
            inverse_state,
            intent_digest,
        })
    }
}

fn intent_digest(
    id: &str, trigger_digest: &[u8; 32], route_digest: &[u8; 32], dimension: Dimension,
    desired_state: &[u8], inverse_state: Option<&[u8]>,
) -> [u8; 32] {
    let mut hasher = blake3::Hasher::new();
    put_len_prefixed(&mut hasher, b"automatic-intent/v1");
    put_len_prefixed(&mut hasher, id.as_bytes());
    hasher.update(trigger_digest);
    hasher.update(route_digest);
    hasher.update(&[dimension as u8]);
    hasher.update(blake3::hash(desired_state).as_bytes());
    if let Some(inverse) = inverse_state {
        hasher.update(&[1]);
        hasher.update(blake3::hash(inverse).as_bytes());
    } else {
        hasher.update(&[0]);
    }
    hasher.finalize().into()
}

/// Monotonic operational severity.
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Serialize, Deserialize)]
#[repr(u8)]
pub enum AndonLevel {
    /// Normal execution.
    Green = 1,
    /// Degraded execution; work may continue within policy.
    Yellow = 2,
    /// Stop-the-line state; new execution is refused.
    Red = 3,
}

/// Monotonic Andon control.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Serialize, Deserialize)]
pub struct AndonSignal {
    level: AndonLevel,
}

impl Default for AndonSignal {
    fn default() -> Self {
        Self {
            level: AndonLevel::Green,
        }
    }
}

impl AndonSignal {
    /// Current severity.
    #[must_use]
    pub const fn level(self) -> AndonLevel {
        self.level
    }

    /// Escalate severity. De-escalation is refused.
    pub fn escalate(&mut self, next: AndonLevel) -> Result<(), OperationsError> {
        if next < self.level {
            return Err(OperationsError::AndonDeescalationRefused {
                current: self.level,
                requested: next,
            });
        }
        self.level = next;
        Ok(())
    }

    fn admit_execution(self) -> Result<(), OperationsError> {
        if self.level == AndonLevel::Red {
            return Err(OperationsError::AndonStopLine);
        }
        Ok(())
    }
}

/// Deterministic retry policy. Backoff is represented as logical ticks, not wall-clock sleep.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Serialize, Deserialize)]
pub struct RetryPolicy {
    /// Maximum observation attempts, including the first attempt.
    pub max_attempts: u8,
    /// Base logical backoff ticks.
    pub base_backoff_ticks: u64,
}

impl RetryPolicy {
    /// Construct a bounded retry policy.
    pub const fn new(max_attempts: u8, base_backoff_ticks: u64) -> Self {
        Self {
            max_attempts,
            base_backoff_ticks,
        }
    }

    fn backoff_ticks(self, attempt: u8) -> u64 {
        let exponent = u32::from(attempt.saturating_sub(1)).min(20);
        self.base_backoff_ticks.saturating_mul(1_u64 << exponent)
    }
}

/// Fail-closed circuit breaker.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Serialize, Deserialize)]
pub struct CircuitBreaker {
    failure_threshold: u32,
    consecutive_failures: u32,
    open: bool,
}

impl CircuitBreaker {
    /// Construct a circuit with a positive failure threshold.
    #[must_use]
    pub const fn new(failure_threshold: u32) -> Self {
        Self {
            failure_threshold,
            consecutive_failures: 0,
            open: failure_threshold == 0,
        }
    }

    fn admit_execution(self) -> Result<(), OperationsError> {
        if self.open {
            return Err(OperationsError::CircuitOpen);
        }
        Ok(())
    }

    fn record_success(&mut self) {
        self.consecutive_failures = 0;
    }

    fn record_failure(&mut self) {
        self.consecutive_failures = self.consecutive_failures.saturating_add(1);
        if self.consecutive_failures >= self.failure_threshold {
            self.open = true;
        }
    }

    /// Whether the circuit is open.
    #[must_use]
    pub const fn is_open(self) -> bool {
        self.open
    }
}

/// Consumable error budget for risky changes.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Serialize, Deserialize)]
pub struct ErrorBudget {
    remaining_units: u64,
    cost_per_operation: u64,
}

impl ErrorBudget {
    /// Construct a bounded budget.
    #[must_use]
    pub const fn new(remaining_units: u64, cost_per_operation: u64) -> Self {
        Self {
            remaining_units,
            cost_per_operation,
        }
    }

    fn admit_execution(self) -> Result<(), OperationsError> {
        if self.remaining_units < self.cost_per_operation {
            return Err(OperationsError::ErrorBudgetExhausted {
                remaining: self.remaining_units,
                required: self.cost_per_operation,
            });
        }
        Ok(())
    }

    fn consume(&mut self) {
        self.remaining_units = self.remaining_units.saturating_sub(self.cost_per_operation);
    }

    /// Remaining units.
    #[must_use]
    pub const fn remaining(self) -> u64 {
        self.remaining_units
    }
}

/// Runtime controls shared by automatic and autonomic execution.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Serialize, Deserialize)]
pub struct OperationsGovernor {
    /// Monotonic stop-the-line signal.
    pub andon: AndonSignal,
    /// Retry policy for observation failures.
    pub retry: RetryPolicy,
    /// Fail-closed breaker.
    pub circuit_breaker: CircuitBreaker,
    /// Consumable risk budget.
    pub error_budget: ErrorBudget,
}

impl OperationsGovernor {
    /// Construct explicit controls.
    #[must_use]
    pub const fn new(
        retry: RetryPolicy, circuit_breaker: CircuitBreaker, error_budget: ErrorBudget,
    ) -> Self {
        Self {
            andon: AndonSignal {
                level: AndonLevel::Green,
            },
            retry,
            circuit_breaker,
            error_budget,
        }
    }

    fn admit_execution(self) -> Result<(), OperationsError> {
        self.andon.admit_execution()?;
        self.circuit_breaker.admit_execution()?;
        self.error_budget.admit_execution()?;
        if self.retry.max_attempts == 0 {
            return Err(OperationsError::RetryBoundZero);
        }
        Ok(())
    }
}

/// Observation failure class.
#[derive(Debug, Clone, PartialEq, Eq, thiserror::Error)]
pub enum ObservationError {
    /// Transient observation failure eligible for bounded retry.
    #[error("transient consequence observation failure: {0}")]
    Transient(String),
    /// Permanent observation failure.
    #[error("permanent consequence observation failure: {0}")]
    Permanent(String),
}

/// Boundary used to re-observe committed consequences.
pub trait ConsequenceObserver {
    /// Observe the artifact referenced by an actuation receipt.
    fn observe(
        &mut self, actuator: &FilesystemActuator, receipt: &ActuationReceipt,
    ) -> Result<Vec<u8>, ObservationError>;
}

/// Real filesystem observer.
#[derive(Debug, Default, Clone, Copy)]
pub struct FilesystemConsequenceObserver;

impl ConsequenceObserver for FilesystemConsequenceObserver {
    fn observe(
        &mut self, actuator: &FilesystemActuator, receipt: &ActuationReceipt,
    ) -> Result<Vec<u8>, ObservationError> {
        read_committed_payload(actuator, &receipt.action_id)
            .map_err(|error| ObservationError::Transient(error.to_string()))
    }
}

/// Non-actuating knowledge hook derived from a verified consequence.
#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
pub struct KnowledgeHook {
    /// Hook schema.
    pub schema: String,
    /// Stable topic for downstream routing.
    pub topic: String,
    /// Actuation receipt that caused the hook.
    pub cause_receipt_digest: [u8; 32],
    /// Observed consequence digest.
    pub consequence_digest: [u8; 32],
    /// BLAKE3 binding the hook.
    pub hook_digest: [u8; 32],
}

impl KnowledgeHook {
    fn issue(topic: String, receipt: &ActuationReceipt, consequence_digest: [u8; 32]) -> Self {
        let hook_digest = hook_digest(&topic, &receipt.receipt_digest, &consequence_digest);
        Self {
            schema: "knowledge-hook/v1".to_string(),
            topic,
            cause_receipt_digest: receipt.receipt_digest,
            consequence_digest,
            hook_digest,
        }
    }

    /// Verify the non-actuating hook.
    pub fn verify(&self) -> Result<(), OperationsError> {
        if self.schema != "knowledge-hook/v1"
            || !safe_token(&self.topic)
            || self.hook_digest
                != hook_digest(
                    &self.topic,
                    &self.cause_receipt_digest,
                    &self.consequence_digest,
                )
        {
            return Err(OperationsError::KnowledgeHookMismatch);
        }
        Ok(())
    }
}

fn hook_digest(
    topic: &str, cause_receipt_digest: &[u8; 32], consequence_digest: &[u8; 32],
) -> [u8; 32] {
    let mut hasher = blake3::Hasher::new();
    put_len_prefixed(&mut hasher, b"knowledge-hook/v1");
    put_len_prefixed(&mut hasher, topic.as_bytes());
    hasher.update(cause_receipt_digest);
    hasher.update(consequence_digest);
    hasher.finalize().into()
}

/// Receipt for one complete trigger-to-consequence automatic operation.
#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
pub struct AutomaticOperationReceipt {
    /// Receipt schema.
    pub schema: String,
    /// Operating contract version.
    pub operations_version: String,
    /// Admitted trigger identity.
    pub trigger_digest: [u8; 32],
    /// Deterministic route identity.
    pub route_digest: [u8; 32],
    /// Manufactured intent identity.
    pub intent_digest: [u8; 32],
    /// Exact grant used for actuation.
    pub grant_digest: [u8; 32],
    /// Atomic actuation receipt.
    pub actuation_receipt: ActuationReceipt,
    /// Re-observed consequence digest.
    pub consequence_digest: [u8; 32],
    /// Number of observation attempts.
    pub observation_attempts: u8,
    /// Total logical backoff ticks consumed.
    pub logical_backoff_ticks: u64,
    /// Previous successful operation in the causal chain.
    pub predecessor_receipt_digest: Option<[u8; 32]>,
    /// Non-actuating downstream hook.
    pub knowledge_hook: KnowledgeHook,
    /// BLAKE3 binding the complete operation.
    pub receipt_digest: [u8; 32],
}

impl AutomaticOperationReceipt {
    /// Every parameter is a distinct, independently-observed proof input
    /// (admitted trigger, route, intent, grant, actuation receipt, consequence
    /// digest, attempt/backoff counters, causal predecessor); bundling them
    /// into a struct would just relocate the same nine fields behind one more
    /// name with no invariant gained. Matches the existing
    /// `#[allow(clippy::too_many_arguments)]` precedent on
    /// `AutomaticRuntime::refuse_postcondition` in this same file.
    #[allow(clippy::too_many_arguments)]
    fn issue(
        admitted: &AdmittedTrigger, route: &Route, intent: &ManufacturedIntent,
        grant_digest: [u8; 32], actuation_receipt: ActuationReceipt, consequence_digest: [u8; 32],
        observation_attempts: u8, logical_backoff_ticks: u64,
        predecessor_receipt_digest: Option<[u8; 32]>,
    ) -> Result<Self, OperationsError> {
        let knowledge_hook = KnowledgeHook::issue(
            format!("{}.verified", admitted.trigger.kind),
            &actuation_receipt,
            consequence_digest,
        );
        let route_digest = route.digest()?;
        let receipt_digest = automatic_receipt_digest(
            &admitted.trigger_digest,
            &route_digest,
            &intent.intent_digest,
            &grant_digest,
            &actuation_receipt.receipt_digest,
            &consequence_digest,
            observation_attempts,
            logical_backoff_ticks,
            predecessor_receipt_digest.as_ref(),
            &knowledge_hook.hook_digest,
        );
        Ok(Self {
            schema: "automatic-operation-receipt/v1".to_string(),
            operations_version: OPERATIONS_VERSION.to_string(),
            trigger_digest: admitted.trigger_digest,
            route_digest,
            intent_digest: intent.intent_digest,
            grant_digest,
            actuation_receipt,
            consequence_digest,
            observation_attempts,
            logical_backoff_ticks,
            predecessor_receipt_digest,
            knowledge_hook,
            receipt_digest,
        })
    }

    /// Verify the complete causal operation receipt.
    pub fn verify(&self) -> Result<(), OperationsError> {
        self.actuation_receipt.verify()?;
        self.knowledge_hook.verify()?;
        if self.schema != "automatic-operation-receipt/v1"
            || self.operations_version != OPERATIONS_VERSION
            || self.grant_digest != self.actuation_receipt.grant_digest
            || self.consequence_digest != self.actuation_receipt.payload_digest
            || self.knowledge_hook.cause_receipt_digest != self.actuation_receipt.receipt_digest
            || self.knowledge_hook.consequence_digest != self.consequence_digest
        {
            return Err(OperationsError::AutomaticReceiptMismatch);
        }
        let expected = automatic_receipt_digest(
            &self.trigger_digest,
            &self.route_digest,
            &self.intent_digest,
            &self.grant_digest,
            &self.actuation_receipt.receipt_digest,
            &self.consequence_digest,
            self.observation_attempts,
            self.logical_backoff_ticks,
            self.predecessor_receipt_digest.as_ref(),
            &self.knowledge_hook.hook_digest,
        );
        if self.receipt_digest != expected {
            return Err(OperationsError::AutomaticReceiptMismatch);
        }
        Ok(())
    }
}

#[allow(clippy::too_many_arguments)]
fn automatic_receipt_digest(
    trigger_digest: &[u8; 32], route_digest: &[u8; 32], intent_digest: &[u8; 32],
    grant_digest: &[u8; 32], actuation_receipt_digest: &[u8; 32], consequence_digest: &[u8; 32],
    observation_attempts: u8, logical_backoff_ticks: u64,
    predecessor_receipt_digest: Option<&[u8; 32]>, hook_digest: &[u8; 32],
) -> [u8; 32] {
    let mut hasher = blake3::Hasher::new();
    put_len_prefixed(&mut hasher, b"automatic-operation-receipt/v1");
    put_len_prefixed(&mut hasher, OPERATIONS_VERSION.as_bytes());
    hasher.update(trigger_digest);
    hasher.update(route_digest);
    hasher.update(intent_digest);
    hasher.update(grant_digest);
    hasher.update(actuation_receipt_digest);
    hasher.update(consequence_digest);
    hasher.update(&[observation_attempts]);
    hasher.update(&logical_backoff_ticks.to_le_bytes());
    if let Some(predecessor) = predecessor_receipt_digest {
        hasher.update(&[1]);
        hasher.update(predecessor);
    } else {
        hasher.update(&[0]);
    }
    hasher.update(hook_digest);
    hasher.finalize().into()
}

/// Failure evidence returned when a postcondition cannot be established.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct PostconditionFailure {
    /// Number of bounded observation attempts.
    pub attempts: u8,
    /// Total logical backoff consumed.
    pub logical_backoff_ticks: u64,
    /// Last observed digest, when observation returned bytes.
    pub last_observed_digest: Option<[u8; 32]>,
    /// Receipt for the lawful inverse, when one existed.
    pub rollback_receipt: Option<ActuationReceipt>,
}

/// Stateful automatic runtime. It owns idempotency, causal ordering, and controls.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct AutomaticRuntime {
    admission: TriggerAdmissionPolicy,
    router: TriggerRouter,
    governor: OperationsGovernor,
    seen_triggers: BTreeSet<[u8; 32]>,
    predecessor_receipt_digest: Option<[u8; 32]>,
}

impl AutomaticRuntime {
    /// Construct the automatic runtime from explicit contracts.
    #[must_use]
    pub fn new(
        admission: TriggerAdmissionPolicy, router: TriggerRouter, governor: OperationsGovernor,
    ) -> Self {
        Self {
            admission,
            router,
            governor,
            seen_triggers: BTreeSet::new(),
            predecessor_receipt_digest: None,
        }
    }

    /// Current controls.
    #[must_use]
    pub const fn governor(&self) -> &OperationsGovernor {
        &self.governor
    }

    /// Mutable controls for explicit operator or autonomic escalation.
    #[must_use]
    pub fn governor_mut(&mut self) -> &mut OperationsGovernor {
        &mut self.governor
    }

    /// Execute one admitted trigger without a rollback inverse.
    pub fn execute<O: ConsequenceObserver>(
        &mut self, trigger: Trigger, machine: &FoundationMachine, actuator: &FilesystemActuator,
        observer: &mut O,
    ) -> Result<AutomaticOperationReceipt, OperationsError> {
        self.execute_with_inverse(trigger, None, machine, actuator, observer)
    }

    /// Execute one admitted trigger with an optional lawful inverse.
    pub fn execute_with_inverse<O: ConsequenceObserver>(
        &mut self, trigger: Trigger, inverse_state: Option<Vec<u8>>, machine: &FoundationMachine,
        actuator: &FilesystemActuator, observer: &mut O,
    ) -> Result<AutomaticOperationReceipt, OperationsError> {
        self.governor.admit_execution()?;
        let admitted = self.admission.admit(trigger)?;
        if !self.seen_triggers.insert(admitted.trigger_digest) {
            return Err(OperationsError::DuplicateTriggerRefused);
        }
        let route = self.router.resolve(&admitted)?.clone();
        let intent = ManufacturedIntent::manufacture(&admitted, &route, inverse_state)?;
        let action = Action::new(
            intent.id.clone(),
            intent.dimension,
            intent.desired_state.clone(),
        );
        let grant = machine.derive_grant(&action)?;
        let actuation_receipt = actuator.actuate(&grant, &action)?;

        let expected_digest: [u8; 32] = blake3::hash(&intent.desired_state).into();
        let mut attempts = 0_u8;
        let mut logical_backoff_ticks = 0_u64;
        let mut last_observed_digest = None;
        while attempts < self.governor.retry.max_attempts {
            attempts = attempts.saturating_add(1);
            match observer.observe(actuator, &actuation_receipt) {
                Ok(bytes) => {
                    let observed_digest: [u8; 32] = blake3::hash(&bytes).into();
                    last_observed_digest = Some(observed_digest);
                    if observed_digest == expected_digest {
                        self.governor.circuit_breaker.record_success();
                        self.governor.error_budget.consume();
                        let receipt = AutomaticOperationReceipt::issue(
                            &admitted,
                            &route,
                            &intent,
                            grant.grant_digest,
                            actuation_receipt,
                            observed_digest,
                            attempts,
                            logical_backoff_ticks,
                            self.predecessor_receipt_digest,
                        )?;
                        receipt.verify()?;
                        self.predecessor_receipt_digest = Some(receipt.receipt_digest);
                        return Ok(receipt);
                    }
                }
                Err(ObservationError::Permanent(message)) => {
                    return self.refuse_postcondition(
                        &intent,
                        machine,
                        actuator,
                        attempts,
                        logical_backoff_ticks,
                        last_observed_digest,
                        Some(message),
                    );
                }
                Err(ObservationError::Transient(_)) => {}
            }
            if attempts < self.governor.retry.max_attempts {
                logical_backoff_ticks = logical_backoff_ticks
                    .saturating_add(self.governor.retry.backoff_ticks(attempts));
            }
        }

        self.refuse_postcondition(
            &intent,
            machine,
            actuator,
            attempts,
            logical_backoff_ticks,
            last_observed_digest,
            None,
        )
    }

    #[allow(clippy::too_many_arguments)]
    fn refuse_postcondition<T>(
        &mut self, intent: &ManufacturedIntent, machine: &FoundationMachine,
        actuator: &FilesystemActuator, attempts: u8, logical_backoff_ticks: u64,
        last_observed_digest: Option<[u8; 32]>, permanent_message: Option<String>,
    ) -> Result<T, OperationsError> {
        self.governor.circuit_breaker.record_failure();
        if self.governor.circuit_breaker.is_open() {
            self.governor.andon.escalate(AndonLevel::Red)?;
        } else {
            self.governor.andon.escalate(AndonLevel::Yellow)?;
        }
        let rollback_receipt = if let Some(inverse_state) = &intent.inverse_state {
            let rollback_action = Action::new(
                format!("{}-rollback", intent.id),
                intent.dimension,
                inverse_state.clone(),
            );
            let rollback_grant = machine.derive_grant(&rollback_action)?;
            let receipt = actuator.actuate(&rollback_grant, &rollback_action)?;
            actuator.verify_committed(&receipt)?;
            Some(receipt)
        } else {
            None
        };
        Err(OperationsError::PostconditionRefused {
            failure: Box::new(PostconditionFailure {
                attempts,
                logical_backoff_ticks,
                last_observed_digest,
                rollback_receipt,
            }),
            permanent_message,
        })
    }
}

/// Replay verifier for automatic operation receipts.
#[derive(Debug, Clone, Default, PartialEq, Eq)]
pub struct AutomaticReplayVerifier {
    seen: BTreeSet<[u8; 32]>,
}

impl AutomaticReplayVerifier {
    /// Verify exactly once.
    pub fn verify_once(
        &mut self, receipt: &AutomaticOperationReceipt,
    ) -> Result<(), OperationsError> {
        receipt.verify()?;
        if !self.seen.insert(receipt.receipt_digest) {
            return Err(OperationsError::AutomaticReceiptReplayRefused);
        }
        Ok(())
    }

    /// Verify a persisted/replayed sequence of receipts both individually and
    /// as a causally-linked chain.
    ///
    /// `AutomaticOperationReceipt::verify` (via `verify_once`) only proves a
    /// receipt is internally self-consistent -- its own `receipt_digest`
    /// covers its own fields, including whatever value it happens to carry in
    /// `predecessor_receipt_digest`. That alone cannot detect two
    /// independently-issued receipts spliced into one sequence, or an earlier
    /// receipt in a real chain being dropped before replay: both produce a
    /// slice where every element individually verifies. This method closes
    /// that gap by additionally asserting, for every position `i` in
    /// `receipts`, that `receipts[i].predecessor_receipt_digest` equals
    /// `Some(receipts[i - 1].receipt_digest)` (and equals `None` at `i == 0`),
    /// refusing with `OperationsError::ChainLinkageBroken` on the first
    /// mismatch found.
    pub fn verify_chain(
        &mut self, receipts: &[AutomaticOperationReceipt],
    ) -> Result<(), OperationsError> {
        let mut expected_predecessor: Option<[u8; 32]> = None;
        for (position, receipt) in receipts.iter().enumerate() {
            self.verify_once(receipt)?;
            if receipt.predecessor_receipt_digest != expected_predecessor {
                return Err(OperationsError::ChainLinkageBroken {
                    position,
                    expected_predecessor,
                    observed_predecessor: receipt.predecessor_receipt_digest,
                });
            }
            expected_predecessor = Some(receipt.receipt_digest);
        }
        Ok(())
    }
}

/// Resource governed by the bounded autonomic controller.
#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
pub struct AutonomicResource {
    /// Stable resource identity.
    pub id: String,
    /// Desired state.
    pub desired_state: Vec<u8>,
    /// Action currently representing the resource state.
    pub current_action_id: String,
    /// Trigger kind routed to an autonomic repair intent.
    pub repair_trigger_kind: String,
}

impl AutonomicResource {
    /// Construct a bounded managed resource.
    pub fn new(
        id: impl Into<String>, desired_state: Vec<u8>, current_action_id: impl Into<String>,
        repair_trigger_kind: impl Into<String>,
    ) -> Result<Self, OperationsError> {
        let resource = Self {
            id: id.into(),
            desired_state,
            current_action_id: current_action_id.into(),
            repair_trigger_kind: repair_trigger_kind.into(),
        };
        if !safe_action_id(&resource.id)
            || !safe_action_id(&resource.current_action_id)
            || !safe_token(&resource.repair_trigger_kind)
        {
            return Err(OperationsError::InvalidAutonomicResource);
        }
        Ok(resource)
    }
}

/// Receipt for a complete bounded autonomic convergence cycle.
#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
pub struct AutonomicOperationsReceipt {
    /// Receipt schema.
    pub schema: String,
    /// Managed resource.
    pub resource_id: String,
    /// Initial observed state.
    pub initial_state_digest: [u8; 32],
    /// Final observed state.
    pub final_state_digest: [u8; 32],
    /// Number of repair cycles executed.
    pub cycles: u8,
    /// Automatic operation receipts produced by repairs.
    pub operations: Vec<AutomaticOperationReceipt>,
    /// Knowledge digest accumulated from the cycle.
    pub knowledge_digest: [u8; 32],
    /// Whether desired state was reached.
    pub converged: bool,
    /// BLAKE3 binding the cycle.
    pub receipt_digest: [u8; 32],
}

impl AutonomicOperationsReceipt {
    fn issue(
        resource_id: String, initial_state_digest: [u8; 32], final_state_digest: [u8; 32],
        cycles: u8, operations: Vec<AutomaticOperationReceipt>, converged: bool,
    ) -> Self {
        let knowledge_digest = autonomic_knowledge_digest(&resource_id, &operations);
        let receipt_digest = autonomic_operations_digest(
            &resource_id,
            &initial_state_digest,
            &final_state_digest,
            cycles,
            &operations,
            &knowledge_digest,
            converged,
        );
        Self {
            schema: "autonomic-operations-receipt/v1".to_string(),
            resource_id,
            initial_state_digest,
            final_state_digest,
            cycles,
            operations,
            knowledge_digest,
            converged,
            receipt_digest,
        }
    }

    /// Verify the cycle and every nested automatic operation.
    pub fn verify(&self) -> Result<(), OperationsError> {
        if self.schema != "autonomic-operations-receipt/v1" || !safe_token(&self.resource_id) {
            return Err(OperationsError::AutonomicReceiptMismatch);
        }
        for operation in &self.operations {
            operation.verify()?;
        }
        if self.knowledge_digest != autonomic_knowledge_digest(&self.resource_id, &self.operations)
            || self.receipt_digest
                != autonomic_operations_digest(
                    &self.resource_id,
                    &self.initial_state_digest,
                    &self.final_state_digest,
                    self.cycles,
                    &self.operations,
                    &self.knowledge_digest,
                    self.converged,
                )
        {
            return Err(OperationsError::AutonomicReceiptMismatch);
        }
        Ok(())
    }
}

fn autonomic_knowledge_digest(
    resource_id: &str, operations: &[AutomaticOperationReceipt],
) -> [u8; 32] {
    let mut hasher = blake3::Hasher::new();
    put_len_prefixed(&mut hasher, b"autonomic-knowledge/v1");
    put_len_prefixed(&mut hasher, resource_id.as_bytes());
    for operation in operations {
        hasher.update(&operation.receipt_digest);
        hasher.update(&operation.knowledge_hook.hook_digest);
    }
    hasher.finalize().into()
}

fn autonomic_operations_digest(
    resource_id: &str, initial_state_digest: &[u8; 32], final_state_digest: &[u8; 32], cycles: u8,
    operations: &[AutomaticOperationReceipt], knowledge_digest: &[u8; 32], converged: bool,
) -> [u8; 32] {
    let mut hasher = blake3::Hasher::new();
    put_len_prefixed(&mut hasher, b"autonomic-operations-receipt/v1");
    put_len_prefixed(&mut hasher, resource_id.as_bytes());
    hasher.update(initial_state_digest);
    hasher.update(final_state_digest);
    hasher.update(&[cycles]);
    hasher.update(&[u8::from(converged)]);
    hasher.update(knowledge_digest);
    for operation in operations {
        hasher.update(&operation.receipt_digest);
    }
    hasher.finalize().into()
}

/// Fully governed bounded autonomic controller.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct AutonomicOperationsController {
    max_cycles: u8,
}

impl AutonomicOperationsController {
    /// Construct a controller with a hard repair bound.
    #[must_use]
    pub const fn new(max_cycles: u8) -> Self {
        Self { max_cycles }
    }

    /// Monitor, analyze, plan, execute, re-observe, and accumulate knowledge.
    pub fn converge<O: ConsequenceObserver>(
        &self, resource: &mut AutonomicResource, runtime: &mut AutomaticRuntime,
        machine: &FoundationMachine, actuator: &FilesystemActuator, observer: &mut O,
    ) -> Result<AutonomicOperationsReceipt, OperationsError> {
        if self.max_cycles == 0 {
            return Err(OperationsError::AutonomicCycleBoundZero);
        }
        let initial_state = read_committed_payload(actuator, &resource.current_action_id)?;
        let initial_state_digest: [u8; 32] = blake3::hash(&initial_state).into();
        let desired_digest: [u8; 32] = blake3::hash(&resource.desired_state).into();
        let mut current_state = initial_state;
        let mut operations = Vec::new();

        for cycle in 0..self.max_cycles {
            let current_digest: [u8; 32] = blake3::hash(&current_state).into();
            if current_digest == desired_digest {
                let receipt = AutonomicOperationsReceipt::issue(
                    resource.id.clone(),
                    initial_state_digest,
                    current_digest,
                    cycle,
                    operations,
                    true,
                );
                receipt.verify()?;
                return Ok(receipt);
            }

            let trigger = Trigger::new(
                format!("repair-{}-{}", resource.id, cycle.saturating_add(1)),
                resource.repair_trigger_kind.clone(),
                u64::from(cycle.saturating_add(1)),
                resource.desired_state.clone(),
            );
            let operation = runtime.execute_with_inverse(
                trigger,
                Some(current_state.clone()),
                machine,
                actuator,
                observer,
            )?;
            resource.current_action_id = operation.actuation_receipt.action_id.clone();
            current_state = read_committed_payload(actuator, &resource.current_action_id)?;
            operations.push(operation);
        }

        let final_state_digest: [u8; 32] = blake3::hash(&current_state).into();
        Err(OperationsError::AutonomicConvergenceRefused {
            observed: final_state_digest,
            desired: desired_digest,
            maximum_cycles: self.max_cycles,
        })
    }
}

/// Read one real committed artifact.
pub fn read_committed_payload(
    actuator: &FilesystemActuator, action_id: &str,
) -> Result<Vec<u8>, OperationsError> {
    if !safe_action_id(action_id) {
        return Err(OperationsError::InvalidCommittedActionId(
            action_id.to_string(),
        ));
    }
    let path = actuator
        .root()
        .join("committed")
        .join(action_id)
        .join("artifact.bin");
    let mut bytes = Vec::new();
    File::open(path)?.read_to_end(&mut bytes)?;
    Ok(bytes)
}

/// Typed automatic/autonomic failures. No failure collapses into truth.
#[derive(Debug, thiserror::Error)]
pub enum OperationsError {
    /// Trigger identity is unsafe.
    #[error("invalid trigger id: {0}")]
    InvalidTriggerId(String),
    /// Trigger kind is unsafe.
    #[error("invalid trigger kind: {0}")]
    InvalidTriggerKind(String),
    /// Trigger sequence must be positive.
    #[error("trigger sequence must be greater than zero")]
    InvalidTriggerSequence,
    /// Trigger bytes changed after construction.
    #[error("trigger payload digest mismatch: {0}")]
    TriggerPayloadDigestMismatch(String),
    /// Trigger kind is excluded by admission policy.
    #[error("trigger kind refused: {0}")]
    TriggerKindRefused(String),
    /// Trigger payload exceeds the admitted bound.
    #[error("trigger payload bound exceeded: observed {observed}, maximum {maximum}")]
    TriggerPayloadBoundExceeded {
        /// Observed bytes.
        observed: usize,
        /// Maximum bytes.
        maximum: usize,
    },
    /// Trigger sequence exceeds the admitted bound.
    #[error("trigger sequence bound exceeded: observed {observed}, maximum {maximum}")]
    TriggerSequenceBoundExceeded {
        /// Observed sequence.
        observed: u64,
        /// Maximum sequence.
        maximum: u64,
    },
    /// Route fields are invalid.
    #[error("invalid deterministic route")]
    InvalidRoute,
    /// A trigger kind has more than one route.
    #[error("duplicate route refused: {0}")]
    DuplicateRoute(String),
    /// No route exists for the admitted trigger kind.
    #[error("route not found: {0}")]
    RouteNotFound(String),
    /// Manufactured intent identity is unsafe.
    #[error("invalid manufactured intent id: {0}")]
    InvalidIntentId(String),
    /// The same admitted trigger attempted to execute twice.
    #[error("duplicate trigger actuation refused")]
    DuplicateTriggerRefused,
    /// Retry policy has no allowed attempt.
    #[error("retry bound must be greater than zero")]
    RetryBoundZero,
    /// Circuit breaker is open.
    #[error("circuit breaker is open")]
    CircuitOpen,
    /// Error budget cannot fund the requested operation.
    #[error("error budget exhausted: remaining {remaining}, required {required}")]
    ErrorBudgetExhausted {
        /// Remaining units.
        remaining: u64,
        /// Required units.
        required: u64,
    },
    /// Andon red refuses new execution.
    #[error("andon red: stop-the-line execution refusal")]
    AndonStopLine,
    /// Andon severity cannot decrease through the execution API.
    #[error("andon de-escalation refused: current {current:?}, requested {requested:?}")]
    AndonDeescalationRefused {
        /// Current severity.
        current: AndonLevel,
        /// Requested lower severity.
        requested: AndonLevel,
    },
    /// Postcondition was not established within policy.
    #[error("postcondition refused after bounded observation")]
    PostconditionRefused {
        /// Bounded failure evidence. Boxed: `PostconditionFailure` carries an
        /// `Option<ActuationReceipt>` (multiple `String`s and BLAKE3 digests),
        /// which otherwise makes this variant, and therefore every
        /// `Result<_, OperationsError>` in this module, oversized
        /// (clippy::result_large_err).
        failure: Box<PostconditionFailure>,
        /// Permanent observer message, when present.
        permanent_message: Option<String>,
    },
    /// Automatic receipt does not verify.
    #[error("automatic operation receipt mismatch")]
    AutomaticReceiptMismatch,
    /// Automatic receipt was replayed.
    #[error("automatic operation receipt replay refused")]
    AutomaticReceiptReplayRefused,
    /// A replayed/persisted sequence of receipts is not causally linked: some
    /// receipt's `predecessor_receipt_digest` does not equal the
    /// `receipt_digest` of the receipt immediately preceding it in the
    /// sequence (position 0 must carry no predecessor). Individually-valid
    /// receipts (each passes `AutomaticOperationReceipt::verify`) can still
    /// fail this check if they were spliced together out of order, drawn
    /// from unrelated chains, or had an earlier receipt dropped before
    /// replay.
    #[error(
        "receipt chain linkage broken at position {position}: expected predecessor {expected_predecessor:?}, observed {observed_predecessor:?}"
    )]
    ChainLinkageBroken {
        /// Zero-based position in the replayed sequence where linkage broke.
        position: usize,
        /// Predecessor digest implied by the sequence itself (`None` at position 0).
        expected_predecessor: Option<[u8; 32]>,
        /// Predecessor digest actually carried by the receipt at `position`.
        observed_predecessor: Option<[u8; 32]>,
    },
    /// Knowledge hook does not verify.
    #[error("knowledge hook mismatch")]
    KnowledgeHookMismatch,
    /// Managed-resource fields are invalid.
    #[error("invalid autonomic resource")]
    InvalidAutonomicResource,
    /// A controller with no cycle budget cannot operate.
    #[error("autonomic cycle bound must be greater than zero")]
    AutonomicCycleBoundZero,
    /// Bounded repair did not converge.
    #[error(
        "autonomic convergence refused after {maximum_cycles} cycles: observed {observed:?}, desired {desired:?}"
    )]
    AutonomicConvergenceRefused {
        /// Final observed state.
        observed: [u8; 32],
        /// Desired state.
        desired: [u8; 32],
        /// Maximum cycles.
        maximum_cycles: u8,
    },
    /// Autonomic receipt does not verify.
    #[error("autonomic operations receipt mismatch")]
    AutonomicReceiptMismatch,
    /// Committed action identity is unsafe.
    #[error("invalid committed action id: {0}")]
    InvalidCommittedActionId(String),
    /// Existing RWR execution machinery refused or failed.
    #[error(transparent)]
    Execution(#[from] ExecutionError),
    /// Real filesystem observation failed.
    #[error("automatic/autonomic filesystem boundary failed: {0}")]
    Io(#[from] std::io::Error),
}
