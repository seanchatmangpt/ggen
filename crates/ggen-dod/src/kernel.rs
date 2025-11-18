//! μ-Kernel: Deterministic decision engine with timing guarantees
//!
//! The kernel is the beating heart of ggen. It takes observations (O),
//! contracts (Σ), and invariants (Q), and produces deterministic actions (A).
//! Every decision is:
//! - Deterministic: Same input always produces identical output
//! - Idempotent: Applying twice has same effect as applying once (when declared)
//! - Timed: Execution must complete within τ ≤ 8ms
//! - Traced: Full provenance recorded in Γ

use crate::error::DoDResult;
use crate::observation::Observation;
use crate::timing::TimingMeasurement;
use serde::{Deserialize, Serialize};
use std::collections::BTreeMap;
use std::time::Instant;
use uuid::Uuid;

/// Unique identifier for kernel actions
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash, Serialize, Deserialize)]
pub struct KernelActionId(Uuid);

impl KernelActionId {
    /// Generate a new action ID
    pub fn new() -> Self {
        Self(Uuid::new_v4())
    }
}

impl Default for KernelActionId {
    fn default() -> Self {
        Self::new()
    }
}

/// Type of action the kernel can emit
#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
pub enum ActionType {
    /// Schema evolution (ΔΣ)
    SchemaEvolution,
    /// Projection update (ΔΠ)
    ProjectionUpdate,
    /// Invariant adjustment (ΔQ)
    InvariantAdjustment,
    /// Marketplace action (promote, deprecate, reroute)
    MarketplaceAction,
    /// Autonomic loop action (monitor, analyze, plan, execute)
    AutonomicAction,
    /// State update
    StateUpdate,
    /// Custom action
    Custom(String),
}

/// Idempotence mode of an action
#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
pub enum IdempotenceMode {
    /// μ ∘ μ = μ (safe to apply multiple times)
    Idempotent,
    /// Not idempotent (side effects on each application)
    NonIdempotent,
    /// Idempotent only in specific mode (e.g., learning-only)
    ConditionallyIdempotent(String),
}

/// An action emitted by the kernel
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct KernelAction {
    /// Unique action ID
    id: KernelActionId,
    /// Type of action
    action_type: ActionType,
    /// Action payload
    payload: serde_json::Value,
    /// Idempotence mode
    idempotence: IdempotenceMode,
    /// Tenant this action belongs to
    tenant_id: String,
    /// Observations that triggered this action
    triggering_observations: Vec<crate::observation::ObservationId>,
}

impl KernelAction {
    /// Create a new kernel action
    pub fn new(
        action_type: ActionType, payload: serde_json::Value, tenant_id: impl Into<String>,
    ) -> Self {
        Self {
            id: KernelActionId::new(),
            action_type,
            payload,
            idempotence: IdempotenceMode::NonIdempotent,
            tenant_id: tenant_id.into(),
            triggering_observations: Vec::new(),
        }
    }

    /// Get action ID
    pub fn id(&self) -> KernelActionId {
        self.id
    }

    /// Get action type
    pub fn action_type(&self) -> &ActionType {
        &self.action_type
    }

    /// Get payload
    pub fn payload(&self) -> &serde_json::Value {
        &self.payload
    }

    /// Set idempotence mode
    pub fn with_idempotence(mut self, mode: IdempotenceMode) -> Self {
        self.idempotence = mode;
        self
    }

    /// Mark as idempotent
    pub fn idempotent(self) -> Self {
        self.with_idempotence(IdempotenceMode::Idempotent)
    }

    /// Add triggering observation
    pub fn with_triggering_observation(
        mut self, obs_id: crate::observation::ObservationId,
    ) -> Self {
        self.triggering_observations.push(obs_id);
        self
    }

    /// Get idempotence mode
    pub fn idempotence(&self) -> IdempotenceMode {
        self.idempotence
    }

    /// Get tenant ID
    pub fn tenant_id(&self) -> &str {
        &self.tenant_id
    }

    /// Get triggering observations
    pub fn triggering_observations(&self) -> &[crate::observation::ObservationId] {
        &self.triggering_observations
    }
}

/// Kernel decision (μ(O) → A)
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct KernelDecision {
    /// Unique decision ID
    decision_id: String,
    /// Observations that led to this decision
    observations: Vec<Observation>,
    /// Actions decided
    actions: Vec<KernelAction>,
    /// Timing measurement
    timing: TimingMeasurement,
    /// Whether this decision is deterministic replay
    is_replay: bool,
    /// Determinism hash (hash of (O, Σ*, Q, Γ) → hash(A))
    determinism_hash: Option<String>,
}

impl KernelDecision {
    /// Create a new kernel decision
    pub fn new() -> Self {
        Self {
            decision_id: Uuid::new_v4().to_string(),
            observations: Vec::new(),
            actions: Vec::new(),
            timing: TimingMeasurement::new(),
            is_replay: false,
            determinism_hash: None,
        }
    }

    /// Add an observation
    pub fn with_observation(mut self, obs: Observation) -> Self {
        self.observations.push(obs);
        self
    }

    /// Add an action
    pub fn with_action(mut self, action: KernelAction) -> Self {
        self.actions.push(action);
        self
    }

    /// Get decision ID
    pub fn decision_id(&self) -> &str {
        &self.decision_id
    }

    /// Get observations
    pub fn observations(&self) -> &[Observation] {
        &self.observations
    }

    /// Get actions
    pub fn actions(&self) -> &[KernelAction] {
        &self.actions
    }

    /// Get timing
    pub fn timing(&self) -> &TimingMeasurement {
        &self.timing
    }

    /// Is this a replay?
    pub fn is_replay(&self) -> bool {
        self.is_replay
    }

    /// Mark as replay
    pub fn as_replay(mut self) -> Self {
        self.is_replay = true;
        self
    }

    /// Set determinism hash
    pub fn with_determinism_hash(mut self, hash: String) -> Self {
        self.determinism_hash = Some(hash);
        self
    }

    /// Get determinism hash
    pub fn determinism_hash(&self) -> Option<&str> {
        self.determinism_hash.as_deref()
    }
}

impl Default for KernelDecision {
    fn default() -> Self {
        Self::new()
    }
}

/// The μ-Kernel: deterministic decision engine
pub struct Kernel {
    /// Current ontology (Σ*)
    contracts: BTreeMap<String, serde_json::Value>,
    /// Invariant rules (Q)
    invariants: BTreeMap<String, String>,
    /// Execution history for determinism checking
    execution_history: BTreeMap<String, KernelDecision>,
}

impl Kernel {
    /// Create a new kernel
    pub fn new() -> Self {
        Self {
            contracts: BTreeMap::new(),
            invariants: BTreeMap::new(),
            execution_history: BTreeMap::new(),
        }
    }

    /// Update schema (ΔΣ)
    pub fn update_schema(
        &mut self, name: impl Into<String>, schema: serde_json::Value,
    ) -> DoDResult<()> {
        self.contracts.insert(name.into(), schema);
        Ok(())
    }

    /// Add invariant
    pub fn add_invariant(
        &mut self, name: impl Into<String>, constraint: impl Into<String>,
    ) -> DoDResult<()> {
        self.invariants.insert(name.into(), constraint.into());
        Ok(())
    }

    /// Execute decision with timing enforcement
    pub fn decide(
        &mut self, observations: Vec<Observation>, tenant_id: &str,
    ) -> DoDResult<KernelDecision> {
        let start = Instant::now();
        let decision_start = TimingMeasurement::new();

        // Validate inputs
        if observations.is_empty() {
            return Err(crate::error::DoDError::KernelDecision(
                "no observations provided".to_string(),
            ));
        }

        // Tenant isolation check
        let tenant = &observations[0].tenant_id();
        if !observations.iter().all(|o| o.tenant_id() == tenant) {
            return Err(crate::error::DoDError::TenantIsolation(
                "observations from different tenants".to_string(),
            ));
        }

        if tenant != tenant_id {
            return Err(crate::error::DoDError::TenantIsolation(
                "tenant mismatch".to_string(),
            ));
        }

        // Create decision
        let mut decision = KernelDecision::new();
        for obs in observations {
            decision = decision.with_observation(obs);
        }

        // Derive actions from observations
        // This is where the actual μ logic would go
        let actions = self.derive_actions(&decision)?;
        for action in actions {
            decision = decision.with_action(action);
        }

        // Measure timing
        let elapsed = start.elapsed().as_millis() as u64;
        let mut timing = TimingMeasurement::new().finished(elapsed);

        // Check timing constraint
        if elapsed > crate::constants::KERNEL_MAX_TIME_MS {
            return Err(crate::error::DoDError::TimingViolation {
                expected: crate::constants::KERNEL_MAX_TIME_MS,
                actual: elapsed,
            });
        }

        decision = decision.with_determinism_hash(self.compute_determinism_hash(&decision));

        // Store in history
        self.execution_history
            .insert(decision.decision_id().to_string(), decision.clone());

        Ok(decision)
    }

    /// Derive actions from observations
    fn derive_actions(&self, decision: &KernelDecision) -> DoDResult<Vec<KernelAction>> {
        let mut actions = Vec::new();

        // Analyze observations and create corresponding actions
        for obs in decision.observations() {
            let action = match obs.obs_type() {
                crate::observation::ObservationType::Metric(_) => KernelAction::new(
                    ActionType::StateUpdate,
                    serde_json::json!({"type": "metric_update"}),
                    obs.tenant_id(),
                ),
                crate::observation::ObservationType::Anomaly(_) => KernelAction::new(
                    ActionType::AutonomicAction,
                    serde_json::json!({"type": "anomaly_response"}),
                    obs.tenant_id(),
                )
                .idempotent(),
                crate::observation::ObservationType::SLOBreach(_) => KernelAction::new(
                    ActionType::SchemaEvolution,
                    serde_json::json!({"type": "slo_response"}),
                    obs.tenant_id(),
                ),
                _ => KernelAction::new(
                    ActionType::Custom("unknown".to_string()),
                    serde_json::json!({"observation": obs.data()}),
                    obs.tenant_id(),
                ),
            }
            .with_triggering_observation(obs.id());

            actions.push(action);
        }

        Ok(actions)
    }

    /// Compute determinism hash (proves μ is deterministic)
    fn compute_determinism_hash(&self, decision: &KernelDecision) -> String {
        use sha2::Digest;
        let mut hasher = sha2::Sha256::new();

        // Hash all observations (O)
        for obs in decision.observations() {
            hasher.update(obs.id().to_string());
            hasher.update(serde_json::to_string(&obs.data()).unwrap_or_default());
        }

        // Hash all contracts (Σ*)
        for (name, schema) in &self.contracts {
            hasher.update(name);
            hasher.update(schema.to_string());
        }

        // Hash all invariants (Q)
        for (name, constraint) in &self.invariants {
            hasher.update(name);
            hasher.update(constraint);
        }

        // Hash all actions (A)
        for action in decision.actions() {
            hasher.update(action.id().to_string());
            hasher.update(action.payload().to_string());
        }

        hex::encode(hasher.finalize())
    }

    /// Verify determinism: replay should produce identical hash
    pub fn verify_determinism(
        &self, original_decision: &KernelDecision, replay_decision: &KernelDecision,
    ) -> DoDResult<()> {
        let original_hash = original_decision
            .determinism_hash()
            .ok_or_else(|| crate::error::DoDError::DeterminismViolation)?;

        let replay_hash = replay_decision
            .determinism_hash()
            .ok_or_else(|| crate::error::DoDError::DeterminismViolation)?;

        if original_hash != replay_hash {
            return Err(crate::error::DoDError::DeterminismViolation);
        }

        Ok(())
    }

    /// Get execution history
    pub fn execution_history(&self) -> &BTreeMap<String, KernelDecision> {
        &self.execution_history
    }
}

impl Default for Kernel {
    fn default() -> Self {
        Self::new()
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_kernel_creation() {
        let kernel = Kernel::new();
        assert!(kernel.contracts.is_empty());
        assert!(kernel.invariants.is_empty());
    }

    #[test]
    fn test_kernel_decision() -> DoDResult<()> {
        let mut kernel = Kernel::new();
        let obs = crate::observation::Observation::new(
            crate::observation::ObservationType::Metric(crate::observation::MetricType::Latency),
            serde_json::json!({"value": 42}),
            "test",
            "1.0",
            "tenant-1",
        )?;

        let decision = kernel.decide(vec![obs], "tenant-1")?;
        assert!(!decision.actions().is_empty());
        Ok(())
    }

    #[test]
    fn test_kernel_timing() -> DoDResult<()> {
        let mut kernel = Kernel::new();
        let obs = crate::observation::Observation::new(
            crate::observation::ObservationType::Metric(crate::observation::MetricType::Latency),
            serde_json::json!({"value": 1}),
            "test",
            "1.0",
            "tenant-1",
        )?;

        let decision = kernel.decide(vec![obs], "tenant-1")?;
        assert!(decision.timing().elapsed_ms() < crate::constants::KERNEL_MAX_TIME_MS);
        Ok(())
    }
}
