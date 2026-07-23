//! Authenticated post-AGI production controller.
//!
//! The controller closes the reference system around four mathematical objects:
//!
//! - algebra: a typed production state vector;
//! - geometry: an admitted convex safety region;
//! - calculus: finite-difference rates over a typed observation window;
//! - cryptographic protocol: keyed capabilities, replay protection, and a receipt chain.
//!
//! No external implementation can impersonate the broker. The concrete broker
//! verifies authority, applies topology mutations, and manufactures the receipt.

use serde::Serialize;
use std::{collections::BTreeSet, fmt, num::NonZeroU64};

pub const PHASE_SHIFT_MULTIPLIER: u64 = 1_000;
pub const MAX_ACTIONS: usize = 5;

#[derive(Clone, Copy, Debug, PartialEq, Eq, PartialOrd, Ord, Serialize)]
pub struct Digest([u8; 32]);

impl Digest {
    #[must_use]
    pub fn from_bytes(bytes: &[u8]) -> Self {
        Self(*blake3::hash(bytes).as_bytes())
    }

    #[must_use]
    pub const fn zero() -> Self {
        Self([0; 32])
    }

    #[must_use]
    pub fn to_hex(self) -> String {
        self.0.iter().map(|byte| format!("{byte:02x}")).collect()
    }

    fn keyed(key: &[u8; 32], domain: &str, parts: &[&[u8]]) -> Self {
        let mut hasher = blake3::Hasher::new_keyed(key);
        frame(&mut hasher, domain.as_bytes());
        for part in parts {
            frame(&mut hasher, part);
        }
        Self(*hasher.finalize().as_bytes())
    }
}

impl fmt::Display for Digest {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.write_str(&self.to_hex())
    }
}

#[derive(Clone, Copy, Debug, PartialEq, Eq, Serialize)]
pub enum Unit {
    Changes,
    Builds,
    Deployments,
    Requests,
}

impl Unit {
    const fn tag(self) -> u8 {
        match self {
            Self::Changes => 1,
            Self::Builds => 2,
            Self::Deployments => 3,
            Self::Requests => 4,
        }
    }
}

#[derive(Clone, Copy, Debug, PartialEq, Eq, Serialize)]
pub struct Measurement {
    pub value: u64,
    pub unit: Unit,
    pub window_seconds: NonZeroU64,
}

impl Measurement {
    #[must_use]
    pub const fn new(value: u64, unit: Unit, window_seconds: NonZeroU64) -> Self {
        Self {
            value,
            unit,
            window_seconds,
        }
    }

    fn compatible(self, other: Self) -> bool {
        self.unit == other.unit && self.window_seconds == other.window_seconds
    }
}

#[derive(Clone, Copy, Debug, PartialEq, Eq, Serialize)]
pub struct StateVector {
    pub demand: Measurement,
    pub error_basis_points: u16,
    pub latency_micros: u64,
    pub receipt_gap: u32,
}

#[derive(Clone, Copy, Debug, PartialEq, Eq, Serialize)]
pub struct RateVector {
    pub demand_per_second_milli: i128,
    pub error_basis_points_per_second_milli: i128,
    pub latency_micros_per_second_milli: i128,
}

impl RateVector {
    pub fn between(previous: StateVector, current: StateVector, elapsed: NonZeroU64) -> Result<Self, Refusal> {
        if !previous.demand.compatible(current.demand) {
            return Err(Refusal::MeasurementMismatch);
        }
        let dt = i128::from(elapsed.get());
        Ok(Self {
            demand_per_second_milli: delta(previous.demand.value, current.demand.value) * 1_000 / dt,
            error_basis_points_per_second_milli:
                (i128::from(current.error_basis_points) - i128::from(previous.error_basis_points)) * 1_000 / dt,
            latency_micros_per_second_milli:
                delta(previous.latency_micros, current.latency_micros) * 1_000 / dt,
        })
    }
}

#[derive(Clone, Copy, Debug, PartialEq, Eq, Serialize)]
pub struct SafeRegion {
    pub max_error_basis_points: u16,
    pub max_latency_micros: u64,
    pub max_receipt_gap: u32,
    pub max_error_acceleration_milli: i128,
    pub max_latency_acceleration_milli: i128,
}

impl SafeRegion {
    #[must_use]
    pub fn contains(self, state: StateVector, rate: RateVector) -> bool {
        state.error_basis_points <= self.max_error_basis_points
            && state.latency_micros <= self.max_latency_micros
            && state.receipt_gap <= self.max_receipt_gap
            && rate.error_basis_points_per_second_milli <= self.max_error_acceleration_milli
            && rate.latency_micros_per_second_milli <= self.max_latency_acceleration_milli
    }
}

#[derive(Clone, Copy, Debug, PartialEq, Eq, Serialize)]
pub enum Regime {
    Continuous,
    PhaseShift1000x,
}

#[derive(Clone, Copy, Debug, PartialEq, Eq, PartialOrd, Ord, Serialize)]
pub enum Action {
    PartitionDemand,
    EnablePullShards,
    RequireIndependentOracle,
    RequireProofCarryingReceipts,
    StageCanaryRollback,
}

const PHASE_SHIFT_ACTIONS: [Action; MAX_ACTIONS] = [
    Action::PartitionDemand,
    Action::EnablePullShards,
    Action::RequireIndependentOracle,
    Action::RequireProofCarryingReceipts,
    Action::StageCanaryRollback,
];

#[derive(Clone, Debug, PartialEq, Eq, Serialize)]
pub struct Topology {
    pub generation: u64,
    pub shards: u32,
    pub pull_enabled: bool,
    pub independent_oracle_required: bool,
    pub proof_receipts_required: bool,
    pub canary_percent: u8,
    pub rollback_armed: bool,
}

impl Default for Topology {
    fn default() -> Self {
        Self {
            generation: 0,
            shards: 1,
            pull_enabled: false,
            independent_oracle_required: false,
            proof_receipts_required: false,
            canary_percent: 0,
            rollback_armed: false,
        }
    }
}

impl Topology {
    fn digest(&self) -> Digest {
        Digest::from_bytes(
            format!(
                "{}:{}:{}:{}:{}:{}:{}",
                self.generation,
                self.shards,
                self.pull_enabled,
                self.independent_oracle_required,
                self.proof_receipts_required,
                self.canary_percent,
                self.rollback_armed
            )
            .as_bytes(),
        )
    }
}

#[derive(Clone, Debug, PartialEq, Eq, Serialize)]
pub struct Plan {
    cycle_id: String,
    regime: Regime,
    actions: Vec<Action>,
    source_digest: Digest,
    knowledge_digest: Digest,
    previous_receipt: Digest,
    target_topology: Topology,
    plan_digest: Digest,
}

impl Plan {
    #[must_use]
    pub fn regime(&self) -> Regime {
        self.regime
    }

    #[must_use]
    pub fn actions(&self) -> &[Action] {
        &self.actions
    }

    #[must_use]
    pub fn target_topology(&self) -> &Topology {
        &self.target_topology
    }
}

#[derive(Clone, Debug)]
pub struct AuthorityKey {
    id: String,
    key: [u8; 32],
}

impl AuthorityKey {
    #[must_use]
    pub fn derive(id: impl Into<String>, secret: &[u8]) -> Self {
        let id = id.into();
        let mut hasher = blake3::Hasher::new_derive_key("tcps-authority-key/v1");
        frame(&mut hasher, id.as_bytes());
        frame(&mut hasher, secret);
        Self {
            id,
            key: *hasher.finalize().as_bytes(),
        }
    }

    #[must_use]
    pub fn id(&self) -> &str {
        &self.id
    }

    pub fn authorize(
        &self,
        plan: &Plan,
        selector: &str,
        executor: &str,
        nonce: u128,
        epoch: u64,
        expires_at_epoch: u64,
    ) -> Result<Capability, Refusal> {
        if self.id.is_empty() || selector.is_empty() || executor.is_empty() {
            return Err(Refusal::EmptyActor);
        }
        if self.id == selector || self.id == executor || selector == executor {
            return Err(Refusal::AuthorityCollapse);
        }
        if expires_at_epoch < epoch {
            return Err(Refusal::ExpiredCapability);
        }
        let mac = capability_mac(
            &self.key,
            plan.plan_digest,
            selector,
            &self.id,
            executor,
            nonce,
            epoch,
            expires_at_epoch,
        );
        Ok(Capability {
            plan_digest: plan.plan_digest,
            selector: selector.to_owned(),
            authorizer: self.id.clone(),
            executor: executor.to_owned(),
            nonce,
            epoch,
            expires_at_epoch,
            mac,
        })
    }
}

#[derive(Debug, PartialEq, Eq)]
pub struct Capability {
    plan_digest: Digest,
    selector: String,
    authorizer: String,
    executor: String,
    nonce: u128,
    epoch: u64,
    expires_at_epoch: u64,
    mac: Digest,
}

#[derive(Clone, Debug, PartialEq, Eq, Serialize)]
pub struct ProductionReceipt {
    pub cycle_id: String,
    pub plan_digest: Digest,
    pub previous_receipt: Digest,
    pub topology_before: Digest,
    pub topology_after: Digest,
    pub selector: String,
    pub authorizer: String,
    pub executor: String,
    pub nonce: u128,
    pub epoch: u64,
    pub receipt_digest: Digest,
}

#[derive(Debug, PartialEq, Eq)]
pub enum Refusal {
    ZeroBaseline,
    MeasurementMismatch,
    UnsafeGeometry,
    EmptyActor,
    AuthorityCollapse,
    ExpiredCapability,
    InvalidCapability,
    Replay,
    BrokenReceiptChain,
    WrongExecutor,
    InvalidPlan,
    EmptyCycle,
}

impl fmt::Display for Refusal {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.write_str(match self {
            Self::ZeroBaseline => "baseline must be non-zero",
            Self::MeasurementMismatch => "measurements must use the same unit and window",
            Self::UnsafeGeometry => "state vector or derivative lies outside the admitted safe region",
            Self::EmptyActor => "all authority identities must be non-empty",
            Self::AuthorityCollapse => "selection, authorization, and execution must be distinct",
            Self::ExpiredCapability => "capability is expired",
            Self::InvalidCapability => "capability authentication failed",
            Self::Replay => "capability nonce has already been consumed",
            Self::BrokenReceiptChain => "plan does not extend the current receipt chain",
            Self::WrongExecutor => "capability targets another executor",
            Self::InvalidPlan => "plan is not the canonical response for its regime",
            Self::EmptyCycle => "cycle identifier must be non-empty",
        })
    }
}

impl std::error::Error for Refusal {}

#[derive(Debug)]
pub struct ProductionBroker {
    id: String,
    authority_id: String,
    authority_key: [u8; 32],
    safe_region: SafeRegion,
    topology: Topology,
    consumed_nonces: BTreeSet<u128>,
    last_receipt: Digest,
}

impl ProductionBroker {
    pub fn new(
        id: impl Into<String>,
        authority: &AuthorityKey,
        safe_region: SafeRegion,
    ) -> Result<Self, Refusal> {
        let id = id.into();
        if id.is_empty() || authority.id.is_empty() || id == authority.id {
            return Err(Refusal::AuthorityCollapse);
        }
        Ok(Self {
            id,
            authority_id: authority.id.clone(),
            authority_key: authority.key,
            safe_region,
            topology: Topology::default(),
            consumed_nonces: BTreeSet::new(),
            last_receipt: Digest::zero(),
        })
    }

    #[must_use]
    pub fn topology(&self) -> &Topology {
        &self.topology
    }

    #[must_use]
    pub fn last_receipt(&self) -> Digest {
        self.last_receipt
    }

    pub fn plan(
        &self,
        cycle_id: impl Into<String>,
        baseline: StateVector,
        observed: StateVector,
        rate: RateVector,
        source_digest: Digest,
        knowledge_digest: Digest,
    ) -> Result<Plan, Refusal> {
        let cycle_id = cycle_id.into();
        if cycle_id.is_empty() {
            return Err(Refusal::EmptyCycle);
        }
        if baseline.demand.value == 0 {
            return Err(Refusal::ZeroBaseline);
        }
        if !baseline.demand.compatible(observed.demand) {
            return Err(Refusal::MeasurementMismatch);
        }
        if !self.safe_region.contains(observed, rate) {
            return Err(Refusal::UnsafeGeometry);
        }
        let regime = if observed.demand.value / baseline.demand.value >= PHASE_SHIFT_MULTIPLIER {
            Regime::PhaseShift1000x
        } else {
            Regime::Continuous
        };
        let actions = match regime {
            Regime::Continuous => Vec::new(),
            Regime::PhaseShift1000x => PHASE_SHIFT_ACTIONS.to_vec(),
        };
        let target_topology = project_topology(&self.topology, regime, observed.demand.value)?;
        let plan_digest = plan_digest(
            &cycle_id,
            regime,
            &actions,
            source_digest,
            knowledge_digest,
            self.last_receipt,
            target_topology.digest(),
        );
        Ok(Plan {
            cycle_id,
            regime,
            actions,
            source_digest,
            knowledge_digest,
            previous_receipt: self.last_receipt,
            target_topology,
            plan_digest,
        })
    }

    pub fn execute(
        &mut self,
        plan: Plan,
        capability: Capability,
        current_epoch: u64,
    ) -> Result<ProductionReceipt, Refusal> {
        if capability.executor != self.id {
            return Err(Refusal::WrongExecutor);
        }
        if capability.authorizer != self.authority_id {
            return Err(Refusal::InvalidCapability);
        }
        if capability.expires_at_epoch < current_epoch || capability.epoch > current_epoch {
            return Err(Refusal::ExpiredCapability);
        }
        if capability.plan_digest != plan.plan_digest {
            return Err(Refusal::InvalidCapability);
        }
        if capability.selector == capability.authorizer
            || capability.selector == capability.executor
            || capability.authorizer == capability.executor
        {
            return Err(Refusal::AuthorityCollapse);
        }
        let expected = capability_mac(
            &self.authority_key,
            capability.plan_digest,
            &capability.selector,
            &capability.authorizer,
            &capability.executor,
            capability.nonce,
            capability.epoch,
            capability.expires_at_epoch,
        );
        if expected != capability.mac {
            return Err(Refusal::InvalidCapability);
        }
        if self.consumed_nonces.contains(&capability.nonce) {
            return Err(Refusal::Replay);
        }
        if plan.previous_receipt != self.last_receipt {
            return Err(Refusal::BrokenReceiptChain);
        }
        validate_plan(&plan, &self.topology)?;

        let topology_before = self.topology.digest();
        self.topology = plan.target_topology.clone();
        self.consumed_nonces.insert(capability.nonce);
        let topology_after = self.topology.digest();
        let receipt_digest = Digest::keyed(
            &self.authority_key,
            "tcps-production-receipt/v1",
            &[
                plan.cycle_id.as_bytes(),
                &plan.plan_digest.0,
                &plan.previous_receipt.0,
                &topology_before.0,
                &topology_after.0,
                capability.selector.as_bytes(),
                capability.authorizer.as_bytes(),
                capability.executor.as_bytes(),
                &capability.nonce.to_le_bytes(),
                &capability.epoch.to_le_bytes(),
            ],
        );
        self.last_receipt = receipt_digest;
        Ok(ProductionReceipt {
            cycle_id: plan.cycle_id,
            plan_digest: plan.plan_digest,
            previous_receipt: plan.previous_receipt,
            topology_before,
            topology_after,
            selector: capability.selector,
            authorizer: capability.authorizer,
            executor: capability.executor,
            nonce: capability.nonce,
            epoch: capability.epoch,
            receipt_digest,
        })
    }
}

fn project_topology(current: &Topology, regime: Regime, observed_demand: u64) -> Result<Topology, Refusal> {
    match regime {
        Regime::Continuous => Ok(current.clone()),
        Regime::PhaseShift1000x => {
            let shards = observed_demand
                .div_ceil(PHASE_SHIFT_MULTIPLIER)
                .try_into()
                .map_err(|_| Refusal::InvalidPlan)?;
            Ok(Topology {
                generation: current.generation.saturating_add(1),
                shards: shards.max(2),
                pull_enabled: true,
                independent_oracle_required: true,
                proof_receipts_required: true,
                canary_percent: 5,
                rollback_armed: true,
            })
        }
    }
}

fn validate_plan(plan: &Plan, current: &Topology) -> Result<(), Refusal> {
    let canonical_actions: &[Action] = match plan.regime {
        Regime::Continuous => &[],
        Regime::PhaseShift1000x => &PHASE_SHIFT_ACTIONS,
    };
    if plan.actions.as_slice() != canonical_actions {
        return Err(Refusal::InvalidPlan);
    }
    if plan.regime == Regime::Continuous && &plan.target_topology != current {
        return Err(Refusal::InvalidPlan);
    }
    if plan.regime == Regime::PhaseShift1000x
        && (!plan.target_topology.pull_enabled
            || !plan.target_topology.independent_oracle_required
            || !plan.target_topology.proof_receipts_required
            || !plan.target_topology.rollback_armed
            || plan.target_topology.canary_percent == 0
            || plan.target_topology.shards < 2)
    {
        return Err(Refusal::InvalidPlan);
    }
    Ok(())
}

fn capability_mac(
    key: &[u8; 32],
    plan_digest: Digest,
    selector: &str,
    authorizer: &str,
    executor: &str,
    nonce: u128,
    epoch: u64,
    expires_at_epoch: u64,
) -> Digest {
    Digest::keyed(
        key,
        "tcps-capability/v1",
        &[
            &plan_digest.0,
            selector.as_bytes(),
            authorizer.as_bytes(),
            executor.as_bytes(),
            &nonce.to_le_bytes(),
            &epoch.to_le_bytes(),
            &expires_at_epoch.to_le_bytes(),
        ],
    )
}

fn plan_digest(
    cycle_id: &str,
    regime: Regime,
    actions: &[Action],
    source_digest: Digest,
    knowledge_digest: Digest,
    previous_receipt: Digest,
    target_topology: Digest,
) -> Digest {
    let mut bytes = Vec::new();
    bytes.extend_from_slice(cycle_id.as_bytes());
    bytes.push(match regime {
        Regime::Continuous => 0,
        Regime::PhaseShift1000x => 1,
    });
    for action in actions {
        bytes.push(*action as u8);
    }
    bytes.extend_from_slice(&source_digest.0);
    bytes.extend_from_slice(&knowledge_digest.0);
    bytes.extend_from_slice(&previous_receipt.0);
    bytes.extend_from_slice(&target_topology.0);
    Digest::from_bytes(&bytes)
}

fn delta(previous: u64, current: u64) -> i128 {
    i128::from(current) - i128::from(previous)
}

fn frame(hasher: &mut blake3::Hasher, bytes: &[u8]) {
    hasher.update(&(bytes.len() as u64).to_le_bytes());
    hasher.update(bytes);
}
