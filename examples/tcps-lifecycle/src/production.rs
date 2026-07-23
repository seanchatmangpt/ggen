//! Authenticated post-AGI production controller.
//!
//! Algebra supplies a typed state vector. Geometry supplies an admitted safe
//! region. Calculus supplies finite-difference rates. Ed25519 capabilities,
//! single-use nonces, target-bound atomic writes, and a signed receipt chain
//! supply authority, actuation, freshness, and replay resistance.

use ed25519_dalek::{Signature, Signer, SigningKey, Verifier, VerifyingKey};
use serde::Serialize;
use std::{
    collections::BTreeSet,
    fmt,
    fs::{self, OpenOptions},
    io::Write,
    num::NonZeroU64,
    path::{Path, PathBuf},
};

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
        encode_bytes(&self.0)
    }

    fn framed(domain: &str, parts: &[&[u8]]) -> Self {
        let mut hasher = blake3::Hasher::new();
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

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub struct PublicKey([u8; 32]);

impl PublicKey {
    #[must_use]
    pub fn to_hex(self) -> String {
        encode_bytes(&self.0)
    }

    fn verifying_key(self) -> Result<VerifyingKey, Refusal> {
        VerifyingKey::from_bytes(&self.0).map_err(|_| Refusal::InvalidPublicKey)
    }
}

#[derive(Clone, Copy, Debug, PartialEq, Eq, Serialize)]
#[repr(u8)]
pub enum Unit {
    Changes = 1,
    Builds = 2,
    Deployments = 3,
    Requests = 4,
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
    pub fn between(
        previous: StateVector,
        current: StateVector,
        elapsed: NonZeroU64,
    ) -> Result<Self, Refusal> {
        if !previous.demand.compatible(current.demand) {
            return Err(Refusal::MeasurementMismatch);
        }
        let dt = i128::from(elapsed.get());
        Ok(Self {
            demand_per_second_milli: delta(previous.demand.value, current.demand.value) * 1_000 / dt,
            error_basis_points_per_second_milli:
                (i128::from(current.error_basis_points)
                    - i128::from(previous.error_basis_points))
                    * 1_000
                    / dt,
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

    fn digest(self) -> Digest {
        Digest::framed(
            "tcps-safe-region/v1",
            &[
                &self.max_error_basis_points.to_le_bytes(),
                &self.max_latency_micros.to_le_bytes(),
                &self.max_receipt_gap.to_le_bytes(),
                &self.max_error_acceleration_milli.to_le_bytes(),
                &self.max_latency_acceleration_milli.to_le_bytes(),
            ],
        )
    }
}

#[derive(Clone, Copy, Debug, PartialEq, Eq, Serialize)]
pub enum Regime {
    Continuous,
    PhaseShift1000x,
}

#[derive(Clone, Copy, Debug, PartialEq, Eq, PartialOrd, Ord, Serialize)]
#[repr(u8)]
pub enum Action {
    PartitionDemand = 1,
    EnablePullShards = 2,
    RequireIndependentOracle = 3,
    RequireProofCarryingReceipts = 4,
    StageCanaryRollback = 5,
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
        Digest::framed(
            "tcps-topology/v1",
            &[
                &self.generation.to_le_bytes(),
                &self.shards.to_le_bytes(),
                &[self.pull_enabled as u8],
                &[self.independent_oracle_required as u8],
                &[self.proof_receipts_required as u8],
                &[self.canary_percent],
                &[self.rollback_armed as u8],
            ],
        )
    }

    fn canonical_bytes(&self) -> Vec<u8> {
        format!(
            "generation={}\nshards={}\npull_enabled={}\nindependent_oracle_required={}\nproof_receipts_required={}\ncanary_percent={}\nrollback_armed={}\n",
            self.generation,
            self.shards,
            self.pull_enabled,
            self.independent_oracle_required,
            self.proof_receipts_required,
            self.canary_percent,
            self.rollback_armed,
        )
        .into_bytes()
    }
}

#[derive(Debug, PartialEq, Eq, Serialize)]
pub struct Plan {
    cycle_id: String,
    target_uri: String,
    regime: Regime,
    actions: Vec<Action>,
    source_digest: Digest,
    knowledge_digest: Digest,
    observation_digest: Digest,
    safe_region_digest: Digest,
    previous_receipt: Digest,
    target_topology: Topology,
    plan_digest: Digest,
}

impl Plan {
    #[must_use]
    pub const fn regime(&self) -> Regime {
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

    #[must_use]
    pub const fn digest(&self) -> Digest {
        self.plan_digest
    }

    #[must_use]
    pub fn target_uri(&self) -> &str {
        &self.target_uri
    }
}

pub struct AuthorityKey {
    id: String,
    signing_key: SigningKey,
}

impl AuthorityKey {
    pub fn derive(id: impl Into<String>, secret: &[u8]) -> Result<Self, Refusal> {
        let id = id.into();
        if id.is_empty() {
            return Err(Refusal::EmptyActor);
        }
        if secret.is_empty() {
            return Err(Refusal::EmptySecret);
        }
        let mut hasher = blake3::Hasher::new_derive_key("tcps-authority-ed25519/v1");
        frame(&mut hasher, id.as_bytes());
        frame(&mut hasher, secret);
        Ok(Self {
            id,
            signing_key: SigningKey::from_bytes(hasher.finalize().as_bytes()),
        })
    }

    #[must_use]
    pub fn id(&self) -> &str {
        &self.id
    }

    #[must_use]
    pub fn public_key(&self) -> PublicKey {
        PublicKey(self.signing_key.verifying_key().to_bytes())
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
        if selector.is_empty() || executor.is_empty() {
            return Err(Refusal::EmptyActor);
        }
        if self.id == selector || self.id == executor || selector == executor {
            return Err(Refusal::AuthorityCollapse);
        }
        if expires_at_epoch < epoch {
            return Err(Refusal::ExpiredCapability);
        }
        let message = capability_message(
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
            signature: self.signing_key.sign(&message),
        })
    }
}

pub struct BrokerKey {
    id: String,
    signing_key: SigningKey,
}

impl BrokerKey {
    pub fn derive(id: impl Into<String>, secret: &[u8]) -> Result<Self, Refusal> {
        let id = id.into();
        if id.is_empty() {
            return Err(Refusal::EmptyActor);
        }
        if secret.is_empty() {
            return Err(Refusal::EmptySecret);
        }
        let mut hasher = blake3::Hasher::new_derive_key("tcps-broker-ed25519/v1");
        frame(&mut hasher, id.as_bytes());
        frame(&mut hasher, secret);
        Ok(Self {
            id,
            signing_key: SigningKey::from_bytes(hasher.finalize().as_bytes()),
        })
    }

    #[must_use]
    pub fn id(&self) -> &str {
        &self.id
    }

    #[must_use]
    pub fn public_key(&self) -> PublicKey {
        PublicKey(self.signing_key.verifying_key().to_bytes())
    }
}

pub struct Capability {
    plan_digest: Digest,
    selector: String,
    authorizer: String,
    executor: String,
    nonce: u128,
    epoch: u64,
    expires_at_epoch: u64,
    signature: Signature,
}

#[derive(Clone, Debug, PartialEq, Eq, Serialize)]
pub struct ProductionReceipt {
    pub cycle_id: String,
    pub target_uri: String,
    pub source_digest: Digest,
    pub knowledge_digest: Digest,
    pub observation_digest: Digest,
    pub safe_region_digest: Digest,
    pub plan_digest: Digest,
    pub previous_receipt: Digest,
    pub topology_before: Digest,
    pub topology_after: Digest,
    pub effect_digest: Digest,
    pub selector: String,
    pub authorizer: String,
    pub executor: String,
    pub nonce: u128,
    pub epoch: u64,
    pub expires_at_epoch: u64,
    pub authorization_signature_digest: Digest,
    pub broker_public_key: String,
    pub receipt_digest: Digest,
    pub signature_hex: String,
}

impl ProductionReceipt {
    pub fn verify(&self, broker_public_key: PublicKey) -> Result<(), Refusal> {
        if self.broker_public_key != broker_public_key.to_hex() {
            return Err(Refusal::InvalidReceiptSignature);
        }
        let expected = receipt_digest_from_fields(self, broker_public_key);
        if expected != self.receipt_digest {
            return Err(Refusal::InvalidReceiptDigest);
        }
        let signature_bytes = decode_signature(&self.signature_hex)?;
        let signature = Signature::from_bytes(&signature_bytes);
        broker_public_key
            .verifying_key()?
            .verify(&self.receipt_digest.0, &signature)
            .map_err(|_| Refusal::InvalidReceiptSignature)
    }
}

#[derive(Debug, PartialEq, Eq)]
pub enum Refusal {
    ZeroBaseline,
    MeasurementMismatch,
    UnsafeGeometry,
    MissingSource,
    MissingKnowledge,
    EmptyActor,
    EmptySecret,
    AuthorityCollapse,
    ExpiredCapability,
    InvalidCapability,
    InvalidPublicKey,
    InvalidReceiptDigest,
    InvalidReceiptSignature,
    Replay,
    BrokenReceiptChain,
    WrongExecutor,
    WrongTarget,
    InvalidPlan,
    EmptyCycle,
    EmptyTarget,
    ActuationFailed,
}

impl fmt::Display for Refusal {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.write_str(match self {
            Self::ZeroBaseline => "baseline must be non-zero",
            Self::MeasurementMismatch => "measurements must use the same unit and window",
            Self::UnsafeGeometry => {
                "state vector or derivative lies outside the admitted safe region"
            }
            Self::MissingSource => "source digest must be non-zero",
            Self::MissingKnowledge => "knowledge digest must be non-zero",
            Self::EmptyActor => "all authority identities must be non-empty",
            Self::EmptySecret => "authority secrets must be non-empty",
            Self::AuthorityCollapse => {
                "selection, authorization, and execution must be distinct"
            }
            Self::ExpiredCapability => "capability is expired",
            Self::InvalidCapability => "capability signature or binding failed",
            Self::InvalidPublicKey => "public key is invalid",
            Self::InvalidReceiptDigest => "receipt fields do not match its digest",
            Self::InvalidReceiptSignature => "broker receipt signature is invalid",
            Self::Replay => "capability nonce has already been consumed",
            Self::BrokenReceiptChain => "plan does not extend the current receipt chain",
            Self::WrongExecutor => "capability targets another executor",
            Self::WrongTarget => "actuation target does not match the authorized plan",
            Self::InvalidPlan => "plan is not the canonical response for its regime",
            Self::EmptyCycle => "cycle identifier must be non-empty",
            Self::EmptyTarget => "actuation target must be non-empty",
            Self::ActuationFailed => "desired-state actuation failed before receipt construction",
        })
    }
}

impl std::error::Error for Refusal {}

pub struct ProductionBroker {
    id: String,
    signing_key: SigningKey,
    authority_id: String,
    authority_public_key: VerifyingKey,
    safe_region: SafeRegion,
    topology: Topology,
    consumed_nonces: BTreeSet<u128>,
    last_receipt: Digest,
}

impl ProductionBroker {
    pub fn new(
        broker: &BrokerKey,
        authority_id: impl Into<String>,
        authority_public_key: PublicKey,
        safe_region: SafeRegion,
    ) -> Result<Self, Refusal> {
        let authority_id = authority_id.into();
        if authority_id.is_empty() || broker.id == authority_id {
            return Err(Refusal::AuthorityCollapse);
        }
        Ok(Self {
            id: broker.id.clone(),
            signing_key: SigningKey::from_bytes(&broker.signing_key.to_bytes()),
            authority_id,
            authority_public_key: authority_public_key.verifying_key()?,
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
    pub const fn last_receipt(&self) -> Digest {
        self.last_receipt
    }

    pub fn plan(
        &self,
        cycle_id: impl Into<String>,
        target_uri: impl Into<String>,
        baseline: StateVector,
        observed: StateVector,
        rate: RateVector,
        source_digest: Digest,
        knowledge_digest: Digest,
    ) -> Result<Plan, Refusal> {
        let cycle_id = cycle_id.into();
        let target_uri = target_uri.into();
        if cycle_id.is_empty() {
            return Err(Refusal::EmptyCycle);
        }
        if target_uri.is_empty() {
            return Err(Refusal::EmptyTarget);
        }
        if baseline.demand.value == 0 {
            return Err(Refusal::ZeroBaseline);
        }
        if source_digest == Digest::zero() {
            return Err(Refusal::MissingSource);
        }
        if knowledge_digest == Digest::zero() {
            return Err(Refusal::MissingKnowledge);
        }
        if !baseline.demand.compatible(observed.demand) {
            return Err(Refusal::MeasurementMismatch);
        }
        if !self.safe_region.contains(observed, rate) {
            return Err(Refusal::UnsafeGeometry);
        }
        let observation_digest = observation_digest(baseline, observed, rate);
        let safe_region_digest = self.safe_region.digest();
        let regime = if observed.demand.value / baseline.demand.value
            >= PHASE_SHIFT_MULTIPLIER
        {
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
            &target_uri,
            regime,
            &actions,
            source_digest,
            knowledge_digest,
            observation_digest,
            safe_region_digest,
            self.last_receipt,
            target_topology.digest(),
        );
        Ok(Plan {
            cycle_id,
            target_uri,
            regime,
            actions,
            source_digest,
            knowledge_digest,
            observation_digest,
            safe_region_digest,
            previous_receipt: self.last_receipt,
            target_topology,
            plan_digest,
        })
    }

    pub fn execute_to_path(
        &mut self,
        plan: Plan,
        capability: Capability,
        current_epoch: u64,
        desired_state_path: impl AsRef<Path>,
    ) -> Result<ProductionReceipt, Refusal> {
        self.verify_capability(&plan, &capability, current_epoch)?;
        if self.consumed_nonces.contains(&capability.nonce) {
            return Err(Refusal::Replay);
        }
        if plan.previous_receipt != self.last_receipt {
            return Err(Refusal::BrokenReceiptChain);
        }
        let actual_target = desired_state_path.as_ref().to_string_lossy();
        if actual_target != plan.target_uri {
            return Err(Refusal::WrongTarget);
        }
        validate_plan(&plan, &self.topology)?;

        let topology_before = self.topology.digest();
        let effect_bytes = plan.target_topology.canonical_bytes();
        let effect_digest = Digest::from_bytes(&effect_bytes);
        atomic_write(
            desired_state_path.as_ref(),
            capability.nonce,
            &effect_bytes,
        )?;

        self.topology = plan.target_topology.clone();
        self.consumed_nonces.insert(capability.nonce);
        let topology_after = self.topology.digest();
        let authorization_signature_digest =
            Digest::from_bytes(&capability.signature.to_bytes());
        let broker_public_key = self.signing_key.verifying_key().to_bytes();
        let receipt_digest = receipt_digest_parts(
            &plan.cycle_id,
            &plan.target_uri,
            plan.source_digest,
            plan.knowledge_digest,
            plan.observation_digest,
            plan.safe_region_digest,
            plan.plan_digest,
            plan.previous_receipt,
            topology_before,
            topology_after,
            effect_digest,
            &capability.selector,
            &capability.authorizer,
            &capability.executor,
            capability.nonce,
            capability.epoch,
            capability.expires_at_epoch,
            authorization_signature_digest,
            broker_public_key,
        );
        let signature = self.signing_key.sign(&receipt_digest.0);
        self.last_receipt = receipt_digest;

        Ok(ProductionReceipt {
            cycle_id: plan.cycle_id,
            target_uri: plan.target_uri,
            source_digest: plan.source_digest,
            knowledge_digest: plan.knowledge_digest,
            observation_digest: plan.observation_digest,
            safe_region_digest: plan.safe_region_digest,
            plan_digest: plan.plan_digest,
            previous_receipt: plan.previous_receipt,
            topology_before,
            topology_after,
            effect_digest,
            selector: capability.selector,
            authorizer: capability.authorizer,
            executor: capability.executor,
            nonce: capability.nonce,
            epoch: capability.epoch,
            expires_at_epoch: capability.expires_at_epoch,
            authorization_signature_digest,
            broker_public_key: PublicKey(broker_public_key).to_hex(),
            receipt_digest,
            signature_hex: encode_bytes(&signature.to_bytes()),
        })
    }

    fn verify_capability(
        &self,
        plan: &Plan,
        capability: &Capability,
        current_epoch: u64,
    ) -> Result<(), Refusal> {
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
        let message = capability_message(
            capability.plan_digest,
            &capability.selector,
            &capability.authorizer,
            &capability.executor,
            capability.nonce,
            capability.epoch,
            capability.expires_at_epoch,
        );
        self.authority_public_key
            .verify(&message, &capability.signature)
            .map_err(|_| Refusal::InvalidCapability)
    }
}

fn project_topology(
    current: &Topology,
    regime: Regime,
    observed_demand: u64,
) -> Result<Topology, Refusal> {
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

fn observation_digest(
    baseline: StateVector,
    observed: StateVector,
    rate: RateVector,
) -> Digest {
    Digest::framed(
        "tcps-observation-vector/v1",
        &[
            &baseline.demand.value.to_le_bytes(),
            &[baseline.demand.unit as u8],
            &baseline.demand.window_seconds.get().to_le_bytes(),
            &baseline.error_basis_points.to_le_bytes(),
            &baseline.latency_micros.to_le_bytes(),
            &baseline.receipt_gap.to_le_bytes(),
            &observed.demand.value.to_le_bytes(),
            &[observed.demand.unit as u8],
            &observed.demand.window_seconds.get().to_le_bytes(),
            &observed.error_basis_points.to_le_bytes(),
            &observed.latency_micros.to_le_bytes(),
            &observed.receipt_gap.to_le_bytes(),
            &rate.demand_per_second_milli.to_le_bytes(),
            &rate.error_basis_points_per_second_milli.to_le_bytes(),
            &rate.latency_micros_per_second_milli.to_le_bytes(),
        ],
    )
}

fn capability_message(
    plan_digest: Digest,
    selector: &str,
    authorizer: &str,
    executor: &str,
    nonce: u128,
    epoch: u64,
    expires_at_epoch: u64,
) -> Vec<u8> {
    framed_bytes(
        "tcps-capability/v3",
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

#[allow(clippy::too_many_arguments)]
fn plan_digest(
    cycle_id: &str,
    target_uri: &str,
    regime: Regime,
    actions: &[Action],
    source_digest: Digest,
    knowledge_digest: Digest,
    observation_digest: Digest,
    safe_region_digest: Digest,
    previous_receipt: Digest,
    target_topology: Digest,
) -> Digest {
    let regime_byte = [match regime {
        Regime::Continuous => 0,
        Regime::PhaseShift1000x => 1,
    }];
    let action_bytes = actions
        .iter()
        .map(|action| *action as u8)
        .collect::<Vec<_>>();
    Digest::framed(
        "tcps-production-plan/v3",
        &[
            cycle_id.as_bytes(),
            target_uri.as_bytes(),
            &regime_byte,
            &action_bytes,
            &source_digest.0,
            &knowledge_digest.0,
            &observation_digest.0,
            &safe_region_digest.0,
            &previous_receipt.0,
            &target_topology.0,
        ],
    )
}

fn receipt_digest_from_fields(
    receipt: &ProductionReceipt,
    broker_public_key: PublicKey,
) -> Digest {
    receipt_digest_parts(
        &receipt.cycle_id,
        &receipt.target_uri,
        receipt.source_digest,
        receipt.knowledge_digest,
        receipt.observation_digest,
        receipt.safe_region_digest,
        receipt.plan_digest,
        receipt.previous_receipt,
        receipt.topology_before,
        receipt.topology_after,
        receipt.effect_digest,
        &receipt.selector,
        &receipt.authorizer,
        &receipt.executor,
        receipt.nonce,
        receipt.epoch,
        receipt.expires_at_epoch,
        receipt.authorization_signature_digest,
        broker_public_key.0,
    )
}

#[allow(clippy::too_many_arguments)]
fn receipt_digest_parts(
    cycle_id: &str,
    target_uri: &str,
    source_digest: Digest,
    knowledge_digest: Digest,
    observation_digest: Digest,
    safe_region_digest: Digest,
    plan_digest: Digest,
    previous_receipt: Digest,
    topology_before: Digest,
    topology_after: Digest,
    effect_digest: Digest,
    selector: &str,
    authorizer: &str,
    executor: &str,
    nonce: u128,
    epoch: u64,
    expires_at_epoch: u64,
    authorization_signature_digest: Digest,
    broker_public_key: [u8; 32],
) -> Digest {
    Digest::framed(
        "tcps-production-receipt/v3",
        &[
            cycle_id.as_bytes(),
            target_uri.as_bytes(),
            &source_digest.0,
            &knowledge_digest.0,
            &observation_digest.0,
            &safe_region_digest.0,
            &plan_digest.0,
            &previous_receipt.0,
            &topology_before.0,
            &topology_after.0,
            &effect_digest.0,
            selector.as_bytes(),
            authorizer.as_bytes(),
            executor.as_bytes(),
            &nonce.to_le_bytes(),
            &epoch.to_le_bytes(),
            &expires_at_epoch.to_le_bytes(),
            &authorization_signature_digest.0,
            &broker_public_key,
        ],
    )
}

fn atomic_write(path: &Path, nonce: u128, bytes: &[u8]) -> Result<(), Refusal> {
    let parent = path.parent().unwrap_or_else(|| Path::new("."));
    fs::create_dir_all(parent).map_err(|_| Refusal::ActuationFailed)?;
    let temp_path = temporary_path(path, nonce);
    let mut file = OpenOptions::new()
        .create_new(true)
        .write(true)
        .open(&temp_path)
        .map_err(|_| Refusal::ActuationFailed)?;
    let result = (|| {
        file.write_all(bytes)
            .map_err(|_| Refusal::ActuationFailed)?;
        file.sync_all().map_err(|_| Refusal::ActuationFailed)?;
        drop(file);
        fs::rename(&temp_path, path).map_err(|_| Refusal::ActuationFailed)?;
        Ok(())
    })();
    if result.is_err() {
        let _ = fs::remove_file(&temp_path);
    }
    result
}

fn temporary_path(path: &Path, nonce: u128) -> PathBuf {
    let mut value = path.as_os_str().to_os_string();
    value.push(format!(".{nonce}.tmp"));
    PathBuf::from(value)
}

fn framed_bytes(domain: &str, parts: &[&[u8]]) -> Vec<u8> {
    let mut bytes = Vec::new();
    append_frame(&mut bytes, domain.as_bytes());
    for part in parts {
        append_frame(&mut bytes, part);
    }
    bytes
}

fn append_frame(target: &mut Vec<u8>, bytes: &[u8]) {
    target.extend_from_slice(&(bytes.len() as u64).to_le_bytes());
    target.extend_from_slice(bytes);
}

fn delta(previous: u64, current: u64) -> i128 {
    i128::from(current) - i128::from(previous)
}

fn frame(hasher: &mut blake3::Hasher, bytes: &[u8]) {
    hasher.update(&(bytes.len() as u64).to_le_bytes());
    hasher.update(bytes);
}

fn encode_bytes(bytes: &[u8]) -> String {
    bytes.iter().map(|byte| format!("{byte:02x}")).collect()
}

fn decode_signature(value: &str) -> Result<[u8; 64], Refusal> {
    if value.len() != 128 {
        return Err(Refusal::InvalidReceiptSignature);
    }
    let mut output = [0_u8; 64];
    for (index, slot) in output.iter_mut().enumerate() {
        let offset = index * 2;
        *slot = u8::from_str_radix(&value[offset..offset + 2], 16)
            .map_err(|_| Refusal::InvalidReceiptSignature)?;
    }
    Ok(output)
}
