#![forbid(unsafe_code)]
//! Deterministic, IO-free enterprise-architecture kernel for Fortune 5 operation.
//!
//! The kernel owns architecture assets, lifecycle and promotion law, dependency
//! closure, impact analysis, SLOs, capacity envelopes, multi-region replication,
//! SPIFFE/SPIRE identity policy, KMS controls, observability coverage, path
//! classification, autonomic intent generation, and deterministic receipts.
//!
//! It deliberately does **not** actuate. Autonomic evaluation manufactures
//! bounded `ArchitectureIntent` values addressed to BRCE. Network calls, process
//! execution, filesystem mutation, deployment, and infrastructure changes live
//! outside this crate.

use serde::{Deserialize, Serialize};
use serde_json::Value;
use std::collections::{BTreeMap, BTreeSet, VecDeque};
use thiserror::Error;

pub const RECEIPT_SCHEMA: &str = "ggen.architecture.receipt.v1";
pub const INTENT_SCHEMA: &str = "ggen.architecture.intent.v1";
pub const REQUIRED_BROKER: &str = "BRCE";

pub type AssetId = String;
pub type CapabilityId = String;
pub type RegionId = String;

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Serialize, Deserialize)]
#[serde(rename_all = "kebab-case")]
pub enum Criticality {
    Tier0,
    Tier1,
    Tier2,
    Tier3,
}

impl Criticality {
    pub const fn requires_full_controls(self) -> bool {
        matches!(self, Self::Tier0 | Self::Tier1)
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Serialize, Deserialize)]
#[serde(rename_all = "kebab-case")]
pub enum AssetKind {
    BusinessCapability,
    BusinessProcess,
    DataProduct,
    Application,
    Service,
    Platform,
    Infrastructure,
    Policy,
    Control,
    IdentityDomain,
    KeyRing,
    TelemetryPipeline,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Serialize, Deserialize)]
#[serde(rename_all = "kebab-case")]
pub enum LifecycleState {
    Proposed,
    Admitted,
    Active,
    Deprecated,
    Retired,
}

impl LifecycleState {
    pub const fn may_transition_to(self, next: Self) -> bool {
        matches!(
            (self, next),
            (Self::Proposed, Self::Admitted)
                | (Self::Admitted, Self::Active)
                | (Self::Active, Self::Deprecated)
                | (Self::Deprecated, Self::Retired)
        )
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
pub struct ArchitectureAsset {
    pub id: AssetId,
    pub name: String,
    pub kind: AssetKind,
    pub lifecycle: LifecycleState,
    pub owner: String,
    pub criticality: Criticality,
    #[serde(default)]
    pub dependencies: BTreeSet<AssetId>,
    #[serde(default)]
    pub capabilities: BTreeSet<CapabilityId>,
    #[serde(default)]
    pub regions: BTreeSet<RegionId>,
    #[serde(default)]
    pub evidence: BTreeSet<String>,
    #[serde(default)]
    pub tags: BTreeMap<String, String>,
}

#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
pub struct EnterpriseCapability {
    pub id: CapabilityId,
    pub name: String,
    pub owner: String,
    pub lifecycle: LifecycleState,
    #[serde(default)]
    pub realized_by: BTreeSet<AssetId>,
}

#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
pub struct Region {
    pub id: RegionId,
    pub provider: String,
    pub jurisdiction: String,
    pub fault_domain: String,
    pub active: bool,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Serialize, Deserialize)]
#[serde(rename_all = "kebab-case")]
pub enum ReplicationMode {
    Strong,
    Eventual,
    ActivePassive,
}

#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
pub struct ReplicationPolicy {
    pub asset_id: AssetId,
    pub mode: ReplicationMode,
    pub regions: BTreeSet<RegionId>,
    pub write_quorum: u16,
    pub read_quorum: u16,
    pub rpo_seconds: u64,
    pub rto_seconds: u64,
    #[serde(default)]
    pub allowed_jurisdictions: BTreeSet<String>,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Serialize, Deserialize)]
#[serde(rename_all = "kebab-case")]
pub enum SliKind {
    Availability,
    LatencyP99Millis,
    ErrorRate,
    Durability,
    FreshnessSeconds,
    ThroughputPerSecond,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Serialize, Deserialize)]
#[serde(rename_all = "kebab-case")]
pub enum ObjectiveDirection {
    AtLeast,
    AtMost,
}

#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub struct SloDefinition {
    pub id: String,
    pub asset_id: AssetId,
    pub sli: SliKind,
    pub direction: ObjectiveDirection,
    pub target: f64,
    pub window_seconds: u64,
    pub minimum_samples: u64,
    pub fast_burn_threshold: f64,
}

#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub struct SliMeasurement {
    pub slo_id: String,
    pub observed: f64,
    pub samples: u64,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Serialize, Deserialize)]
#[serde(rename_all = "kebab-case")]
pub enum SloStanding {
    Unknown,
    Green,
    Breached,
}

#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub struct SloEvaluation {
    pub slo_id: String,
    pub asset_id: AssetId,
    pub standing: SloStanding,
    pub observed: Option<f64>,
    pub target: f64,
    pub error_budget_delta: Option<f64>,
}

#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub struct CapacityEnvelope {
    pub asset_id: AssetId,
    pub min_replicas: u32,
    pub max_replicas: u32,
    pub current_replicas: u32,
    pub unit_capacity: f64,
    pub observed_load: f64,
    pub reserve_ratio: f64,
}

#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub enum CapacityStanding {
    WithinEnvelope,
    ScaleOut { required_replicas: u32 },
    AdmissionControl {
        required_replicas: u32,
        maximum_replicas: u32,
        unmet_capacity: f64,
    },
}

impl CapacityEnvelope {
    pub fn standing(&self) -> CapacityStanding {
        if self.unit_capacity <= 0.0 || self.observed_load <= 0.0 {
            return CapacityStanding::WithinEnvelope;
        }
        let usable_per_replica = self.unit_capacity * (1.0 - self.reserve_ratio);
        if usable_per_replica <= 0.0 {
            return CapacityStanding::AdmissionControl {
                required_replicas: u32::MAX,
                maximum_replicas: self.max_replicas,
                unmet_capacity: self.observed_load,
            };
        }
        let required = (self.observed_load / usable_per_replica).ceil() as u32;
        let required = required.max(self.min_replicas);
        if required <= self.current_replicas {
            CapacityStanding::WithinEnvelope
        } else if required <= self.max_replicas {
            CapacityStanding::ScaleOut {
                required_replicas: required,
            }
        } else {
            CapacityStanding::AdmissionControl {
                required_replicas: required,
                maximum_replicas: self.max_replicas,
                unmet_capacity: self.observed_load
                    - usable_per_replica * f64::from(self.max_replicas),
            }
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
pub struct IdentityPolicy {
    pub asset_id: AssetId,
    pub trust_domain: String,
    pub allowed_spiffe_ids: BTreeSet<String>,
    pub mtls_required: bool,
    pub max_svid_ttl_seconds: u64,
    pub workload_attestation_required: bool,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Serialize, Deserialize)]
#[serde(rename_all = "kebab-case")]
pub enum KmsProvider {
    AwsKms,
    AzureKeyVault,
    GoogleCloudKms,
    Vault,
    ExternalHsm,
}

#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
pub struct KmsPolicy {
    pub asset_id: AssetId,
    pub provider: KmsProvider,
    pub key_alias: String,
    pub rotation_days: u32,
    pub hsm_backed: bool,
    pub envelope_encryption: bool,
    pub decrypt_audit_required: bool,
    pub break_glass_dual_control: bool,
}

#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
pub struct ObservabilityPolicy {
    pub asset_id: AssetId,
    pub logs: bool,
    pub metrics: bool,
    pub traces: bool,
    pub profiles: bool,
    pub correlation_ids: bool,
    pub redaction_required: bool,
    pub retention_days: u32,
    pub otlp_endpoint_class: String,
}

#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
pub struct PromotionGate {
    pub id: String,
    pub asset_id: AssetId,
    pub from: LifecycleState,
    pub to: LifecycleState,
    #[serde(default)]
    pub required_evidence: BTreeSet<String>,
    pub minimum_approvals: u16,
    pub require_slo_green: bool,
    pub require_receipt_replay: bool,
    pub require_security_controls: bool,
}

#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
pub struct PromotionContext {
    #[serde(default)]
    pub available_evidence: BTreeSet<String>,
    pub approvals: u16,
    pub receipt_replay_green: bool,
    pub security_controls_green: bool,
}

#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
pub struct PromotionDecision {
    pub gate_id: String,
    pub asset_id: AssetId,
    pub allowed: bool,
    #[serde(default)]
    pub reasons: Vec<String>,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Serialize, Deserialize)]
#[serde(rename_all = "kebab-case")]
pub enum ExecutionPath {
    Hot,
    Warm,
    Cold,
}

#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
pub struct PathDecision {
    pub path: ExecutionPath,
    pub triples: u64,
    pub joins: u32,
    pub simple_predicates: bool,
    pub reason: String,
}

pub fn select_execution_path(triples: u64, joins: u32, simple_predicates: bool) -> PathDecision {
    let (path, reason) = if triples <= 8 && joins == 0 && simple_predicates {
        (
            ExecutionPath::Hot,
            "at most eight triples, no joins, simple predicates".to_string(),
        )
    } else if triples <= 1_000 && joins <= 4 && simple_predicates {
        (
            ExecutionPath::Warm,
            "at most one thousand triples with bounded simple joins".to_string(),
        )
    } else {
        (
            ExecutionPath::Cold,
            "complex or large graph requires full SPARQL/cold-path execution".to_string(),
        )
    };
    PathDecision {
        path,
        triples,
        joins,
        simple_predicates,
        reason,
    }
}

#[derive(Debug, Clone, PartialEq, Serialize, Deserialize, Default)]
pub struct EnterpriseObservation {
    #[serde(default)]
    pub measurements: BTreeMap<String, SliMeasurement>,
    #[serde(default)]
    pub unavailable_regions: BTreeSet<RegionId>,
    #[serde(default)]
    pub key_age_days: BTreeMap<AssetId, u32>,
    #[serde(default)]
    pub svid_ttl_remaining_seconds: BTreeMap<AssetId, u64>,
    #[serde(default)]
    pub telemetry_stale_assets: BTreeSet<AssetId>,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Serialize, Deserialize)]
#[serde(rename_all = "kebab-case")]
pub enum IntentPriority {
    Critical,
    High,
    Medium,
    Low,
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Serialize, Deserialize)]
#[serde(rename_all = "kebab-case")]
pub enum ArchitectureIntentKind {
    ScaleOut,
    ThrottleAdmission,
    FreezePromotion,
    InitiateFailover,
    RotateKey,
    RenewIdentity,
    RestoreTelemetry,
    EscalateErrorBudget,
}

#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
pub struct ArchitectureIntent {
    pub schema: String,
    pub intent_id: String,
    pub source_asset: AssetId,
    pub kind: ArchitectureIntentKind,
    pub priority: IntentPriority,
    pub broker: String,
    pub reason: String,
    #[serde(default)]
    pub constraints: BTreeMap<String, String>,
    #[serde(default)]
    pub evidence: BTreeSet<String>,
}

impl ArchitectureIntent {
    fn new(
        source_asset: AssetId,
        kind: ArchitectureIntentKind,
        priority: IntentPriority,
        reason: String,
        constraints: BTreeMap<String, String>,
        evidence: BTreeSet<String>,
    ) -> Self {
        let identity = canonical_digest(&(
            INTENT_SCHEMA,
            &source_asset,
            &kind,
            &priority,
            REQUIRED_BROKER,
            &reason,
            &constraints,
            &evidence,
        ))
        .expect("intent identity serialization cannot fail");
        Self {
            schema: INTENT_SCHEMA.to_string(),
            intent_id: format!("urn:blake3:{identity}"),
            source_asset,
            kind,
            priority,
            broker: REQUIRED_BROKER.to_string(),
            reason,
            constraints,
            evidence,
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
pub struct ImpactReport {
    pub changed_assets: BTreeSet<AssetId>,
    pub directly_affected: BTreeSet<AssetId>,
    pub transitively_affected: BTreeSet<AssetId>,
    pub affected_capabilities: BTreeSet<CapabilityId>,
    pub affected_regions: BTreeSet<RegionId>,
}

#[derive(Debug, Clone, PartialEq, Serialize, Deserialize, Default)]
pub struct ArchitectureModel {
    #[serde(default)]
    pub assets: BTreeMap<AssetId, ArchitectureAsset>,
    #[serde(default)]
    pub capabilities: BTreeMap<CapabilityId, EnterpriseCapability>,
    #[serde(default)]
    pub regions: BTreeMap<RegionId, Region>,
    #[serde(default)]
    pub slos: BTreeMap<String, SloDefinition>,
    #[serde(default)]
    pub capacities: BTreeMap<AssetId, CapacityEnvelope>,
    #[serde(default)]
    pub replication: BTreeMap<AssetId, ReplicationPolicy>,
    #[serde(default)]
    pub identities: BTreeMap<AssetId, IdentityPolicy>,
    #[serde(default)]
    pub kms: BTreeMap<AssetId, KmsPolicy>,
    #[serde(default)]
    pub observability: BTreeMap<AssetId, ObservabilityPolicy>,
    #[serde(default)]
    pub promotion_gates: BTreeMap<String, PromotionGate>,
}

#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
pub struct Violation {
    pub code: String,
    pub subject: String,
    pub message: String,
}

impl Violation {
    fn new(code: &str, subject: impl Into<String>, message: impl Into<String>) -> Self {
        Self {
            code: code.to_string(),
            subject: subject.into(),
            message: message.into(),
        }
    }
}

impl ArchitectureModel {
    pub fn validate(&self) -> Vec<Violation> {
        let mut violations = Vec::new();

        for (id, asset) in &self.assets {
            if id != &asset.id || id.trim().is_empty() {
                violations.push(Violation::new(
                    "ASSET_ID_INVALID",
                    id,
                    "map identity must equal a non-empty asset identity",
                ));
            }
            if asset.owner.trim().is_empty() {
                violations.push(Violation::new(
                    "ASSET_OWNER_MISSING",
                    id,
                    "every architecture asset requires accountable ownership",
                ));
            }
            for dependency in &asset.dependencies {
                if !self.assets.contains_key(dependency) {
                    violations.push(Violation::new(
                        "UNKNOWN_DEPENDENCY",
                        id,
                        format!("dependency {dependency} is not an admitted architecture asset"),
                    ));
                }
            }
            for capability in &asset.capabilities {
                if !self.capabilities.contains_key(capability) {
                    violations.push(Violation::new(
                        "UNKNOWN_CAPABILITY",
                        id,
                        format!("capability {capability} is not admitted"),
                    ));
                }
            }
            for region in &asset.regions {
                if !self.regions.contains_key(region) {
                    violations.push(Violation::new(
                        "UNKNOWN_REGION",
                        id,
                        format!("region {region} is not admitted"),
                    ));
                }
            }
            if asset.criticality.requires_full_controls() {
                for (present, code, message) in [
                    (
                        self.capacities.contains_key(id),
                        "CAPACITY_ENVELOPE_MISSING",
                        "Tier 0/1 assets require a capacity envelope",
                    ),
                    (
                        self.replication.contains_key(id),
                        "REPLICATION_POLICY_MISSING",
                        "Tier 0/1 assets require multi-region replication",
                    ),
                    (
                        self.identities.contains_key(id),
                        "IDENTITY_POLICY_MISSING",
                        "Tier 0/1 assets require SPIFFE/SPIRE workload identity",
                    ),
                    (
                        self.kms.contains_key(id),
                        "KMS_POLICY_MISSING",
                        "Tier 0/1 assets require KMS-backed encryption policy",
                    ),
                    (
                        self.observability.contains_key(id),
                        "OBSERVABILITY_POLICY_MISSING",
                        "Tier 0/1 assets require comprehensive observability",
                    ),
                    (
                        self.slos.values().any(|slo| &slo.asset_id == id),
                        "SLO_MISSING",
                        "Tier 0/1 assets require at least one measurable SLO",
                    ),
                ] {
                    if !present {
                        violations.push(Violation::new(code, id, message));
                    }
                }
            }
        }

        violations.extend(self.dependency_cycle_violations());

        for (id, capability) in &self.capabilities {
            if id != &capability.id || id.trim().is_empty() {
                violations.push(Violation::new(
                    "CAPABILITY_ID_INVALID",
                    id,
                    "map identity must equal a non-empty capability identity",
                ));
            }
            if capability.owner.trim().is_empty() {
                violations.push(Violation::new(
                    "CAPABILITY_OWNER_MISSING",
                    id,
                    "every enterprise capability requires accountable ownership",
                ));
            }
            if capability.realized_by.is_empty() {
                violations.push(Violation::new(
                    "CAPABILITY_UNREALIZED",
                    id,
                    "every admitted capability must be realized by at least one asset",
                ));
            }
            for asset in &capability.realized_by {
                if !self.assets.contains_key(asset) {
                    violations.push(Violation::new(
                        "CAPABILITY_ASSET_UNKNOWN",
                        id,
                        format!("realizing asset {asset} is not admitted"),
                    ));
                }
            }
        }

        for (id, slo) in &self.slos {
            if id != &slo.id || id.trim().is_empty() {
                violations.push(Violation::new(
                    "SLO_ID_INVALID",
                    id,
                    "map identity must equal a non-empty SLO identity",
                ));
            }
            if !self.assets.contains_key(&slo.asset_id) {
                violations.push(Violation::new(
                    "SLO_ASSET_UNKNOWN",
                    id,
                    format!("SLO asset {} is not admitted", slo.asset_id),
                ));
            }
            if !slo.target.is_finite()
                || slo.target < 0.0
                || slo.window_seconds == 0
                || slo.minimum_samples == 0
                || !slo.fast_burn_threshold.is_finite()
                || slo.fast_burn_threshold <= 0.0
            {
                violations.push(Violation::new(
                    "SLO_CONTRACT_INVALID",
                    id,
                    "target, window, sample floor, and burn threshold must be finite and positive",
                ));
            }
        }

        for (asset_id, envelope) in &self.capacities {
            if asset_id != &envelope.asset_id
                || envelope.min_replicas == 0
                || envelope.max_replicas < envelope.min_replicas
                || envelope.current_replicas < envelope.min_replicas
                || envelope.current_replicas > envelope.max_replicas
                || !envelope.unit_capacity.is_finite()
                || envelope.unit_capacity <= 0.0
                || !envelope.observed_load.is_finite()
                || envelope.observed_load < 0.0
                || !envelope.reserve_ratio.is_finite()
                || !(0.0..1.0).contains(&envelope.reserve_ratio)
            {
                violations.push(Violation::new(
                    "CAPACITY_ENVELOPE_INVALID",
                    asset_id,
                    "replica bounds, unit capacity, load, and reserve ratio are inconsistent",
                ));
            }
        }

        for (asset_id, policy) in &self.replication {
            if asset_id != &policy.asset_id || !self.assets.contains_key(asset_id) {
                violations.push(Violation::new(
                    "REPLICATION_ASSET_INVALID",
                    asset_id,
                    "replication policy must belong to an admitted asset",
                ));
            }
            if policy.regions.len() < 2
                || policy.write_quorum == 0
                || policy.read_quorum == 0
                || usize::from(policy.write_quorum) > policy.regions.len()
                || usize::from(policy.read_quorum) > policy.regions.len()
                || policy.rto_seconds == 0
            {
                violations.push(Violation::new(
                    "REPLICATION_POLICY_INVALID",
                    asset_id,
                    "multi-region policy requires at least two regions, bounded quorums, and non-zero RTO",
                ));
            }
            if policy.mode == ReplicationMode::Strong
                && usize::from(policy.write_quorum + policy.read_quorum) <= policy.regions.len()
            {
                violations.push(Violation::new(
                    "STRONG_QUORUM_UNSAFE",
                    asset_id,
                    "strong consistency requires read quorum plus write quorum greater than region count",
                ));
            }
            for region_id in &policy.regions {
                match self.regions.get(region_id) {
                    None => violations.push(Violation::new(
                        "REPLICATION_REGION_UNKNOWN",
                        asset_id,
                        format!("replication region {region_id} is not admitted"),
                    )),
                    Some(region)
                        if !policy.allowed_jurisdictions.is_empty()
                            && !policy.allowed_jurisdictions.contains(&region.jurisdiction) =>
                    {
                        violations.push(Violation::new(
                            "DATA_RESIDENCY_VIOLATION",
                            asset_id,
                            format!(
                                "region {region_id} jurisdiction {} is not allowed",
                                region.jurisdiction
                            ),
                        ));
                    }
                    Some(_) => {}
                }
            }
        }

        for (asset_id, policy) in &self.identities {
            let trust_prefix = format!("spiffe://{}/", policy.trust_domain);
            if asset_id != &policy.asset_id
                || policy.trust_domain.trim().is_empty()
                || !policy.mtls_required
                || !policy.workload_attestation_required
                || policy.max_svid_ttl_seconds == 0
                || policy.max_svid_ttl_seconds > 86_400
                || policy.allowed_spiffe_ids.is_empty()
                || policy
                    .allowed_spiffe_ids
                    .iter()
                    .any(|id| !id.starts_with(&trust_prefix))
            {
                violations.push(Violation::new(
                    "SPIFFE_POLICY_INVALID",
                    asset_id,
                    "identity policy requires mTLS, attestation, bounded SVID TTL, and trust-domain-local SPIFFE IDs",
                ));
            }
        }

        for (asset_id, policy) in &self.kms {
            let critical = self
                .assets
                .get(asset_id)
                .is_some_and(|asset| asset.criticality.requires_full_controls());
            if asset_id != &policy.asset_id
                || policy.key_alias.trim().is_empty()
                || policy.rotation_days == 0
                || policy.rotation_days > 365
                || !policy.envelope_encryption
                || !policy.decrypt_audit_required
                || (critical && (!policy.hsm_backed || !policy.break_glass_dual_control))
            {
                violations.push(Violation::new(
                    "KMS_POLICY_INVALID",
                    asset_id,
                    "KMS policy requires bounded rotation, envelope encryption, decrypt audit, and Tier 0/1 HSM dual control",
                ));
            }
        }

        for (asset_id, policy) in &self.observability {
            if asset_id != &policy.asset_id
                || !policy.logs
                || !policy.metrics
                || !policy.traces
                || !policy.correlation_ids
                || !policy.redaction_required
                || policy.retention_days == 0
                || policy.otlp_endpoint_class.trim().is_empty()
            {
                violations.push(Violation::new(
                    "OBSERVABILITY_POLICY_INVALID",
                    asset_id,
                    "comprehensive observability requires logs, metrics, traces, correlation, redaction, retention, and OTLP routing",
                ));
            }
        }

        for (gate_id, gate) in &self.promotion_gates {
            if gate_id != &gate.id
                || !self.assets.contains_key(&gate.asset_id)
                || !gate.from.may_transition_to(gate.to)
                || gate.minimum_approvals == 0
                || gate.required_evidence.is_empty()
            {
                violations.push(Violation::new(
                    "PROMOTION_GATE_INVALID",
                    gate_id,
                    "promotion gate requires a legal lifecycle edge, evidence, approvals, and an admitted asset",
                ));
            }
        }

        violations.sort_by(|left, right| {
            (&left.code, &left.subject, &left.message).cmp(&(
                &right.code,
                &right.subject,
                &right.message,
            ))
        });
        violations
    }

    pub fn is_valid(&self) -> bool {
        self.validate().is_empty()
    }

    fn dependency_cycle_violations(&self) -> Vec<Violation> {
        fn visit(
            id: &str,
            model: &ArchitectureModel,
            visiting: &mut BTreeSet<String>,
            visited: &mut BTreeSet<String>,
            violations: &mut Vec<Violation>,
        ) {
            if visited.contains(id) {
                return;
            }
            if !visiting.insert(id.to_string()) {
                violations.push(Violation::new(
                    "DEPENDENCY_CYCLE",
                    id,
                    "architecture dependency graph contains a cycle",
                ));
                return;
            }
            if let Some(asset) = model.assets.get(id) {
                for dependency in &asset.dependencies {
                    if model.assets.contains_key(dependency) {
                        visit(dependency, model, visiting, visited, violations);
                    }
                }
            }
            visiting.remove(id);
            visited.insert(id.to_string());
        }

        let mut visiting = BTreeSet::new();
        let mut visited = BTreeSet::new();
        let mut violations = Vec::new();
        for id in self.assets.keys() {
            visit(id, self, &mut visiting, &mut visited, &mut violations);
        }
        violations
    }

    pub fn dependency_closure(&self, root: &str) -> Result<BTreeSet<AssetId>, KernelError> {
        if !self.assets.contains_key(root) {
            return Err(KernelError::UnknownAsset(root.to_string()));
        }
        let mut closure = BTreeSet::new();
        let mut queue = VecDeque::from([root.to_string()]);
        while let Some(current) = queue.pop_front() {
            if let Some(asset) = self.assets.get(&current) {
                for dependency in &asset.dependencies {
                    if closure.insert(dependency.clone()) {
                        queue.push_back(dependency.clone());
                    }
                }
            }
        }
        Ok(closure)
    }

    pub fn topological_order(&self) -> Result<Vec<AssetId>, KernelError> {
        let cycle = self
            .dependency_cycle_violations()
            .into_iter()
            .find(|violation| violation.code == "DEPENDENCY_CYCLE");
        if let Some(violation) = cycle {
            return Err(KernelError::DependencyCycle(violation.subject));
        }

        let mut indegree: BTreeMap<String, usize> = self
            .assets
            .keys()
            .map(|id| (id.clone(), 0_usize))
            .collect();
        let mut reverse: BTreeMap<String, BTreeSet<String>> = BTreeMap::new();
        for (id, asset) in &self.assets {
            for dependency in &asset.dependencies {
                if self.assets.contains_key(dependency) {
                    *indegree.entry(id.clone()).or_default() += 1;
                    reverse
                        .entry(dependency.clone())
                        .or_default()
                        .insert(id.clone());
                }
            }
        }
        let mut ready: BTreeSet<String> = indegree
            .iter()
            .filter_map(|(id, degree)| (*degree == 0).then_some(id.clone()))
            .collect();
        let mut ordered = Vec::with_capacity(self.assets.len());
        while let Some(id) = ready.pop_first() {
            ordered.push(id.clone());
            if let Some(dependents) = reverse.get(&id) {
                for dependent in dependents {
                    let degree = indegree
                        .get_mut(dependent)
                        .expect("dependent indegree exists");
                    *degree -= 1;
                    if *degree == 0 {
                        ready.insert(dependent.clone());
                    }
                }
            }
        }
        if ordered.len() != self.assets.len() {
            return Err(KernelError::DependencyCycle(
                "topological-order".to_string(),
            ));
        }
        Ok(ordered)
    }

    pub fn impact_analysis<I, S>(&self, changed: I) -> Result<ImpactReport, KernelError>
    where
        I: IntoIterator<Item = S>,
        S: AsRef<str>,
    {
        let changed_assets: BTreeSet<String> = changed
            .into_iter()
            .map(|value| value.as_ref().to_string())
            .collect();
        for id in &changed_assets {
            if !self.assets.contains_key(id) {
                return Err(KernelError::UnknownAsset(id.clone()));
            }
        }

        let mut reverse: BTreeMap<String, BTreeSet<String>> = BTreeMap::new();
        for (id, asset) in &self.assets {
            for dependency in &asset.dependencies {
                reverse
                    .entry(dependency.clone())
                    .or_default()
                    .insert(id.clone());
            }
        }

        let directly_affected: BTreeSet<String> = changed_assets
            .iter()
            .flat_map(|id| reverse.get(id).into_iter().flatten().cloned())
            .collect();
        let mut transitively_affected = directly_affected.clone();
        let mut queue: VecDeque<String> = directly_affected.iter().cloned().collect();
        while let Some(current) = queue.pop_front() {
            for dependent in reverse.get(&current).into_iter().flatten() {
                if transitively_affected.insert(dependent.clone()) {
                    queue.push_back(dependent.clone());
                }
            }
        }

        let all_affected: BTreeSet<String> = changed_assets
            .union(&transitively_affected)
            .cloned()
            .collect();
        let affected_capabilities = all_affected
            .iter()
            .filter_map(|id| self.assets.get(id))
            .flat_map(|asset| asset.capabilities.iter().cloned())
            .collect();
        let affected_regions = all_affected
            .iter()
            .filter_map(|id| self.assets.get(id))
            .flat_map(|asset| asset.regions.iter().cloned())
            .collect();

        Ok(ImpactReport {
            changed_assets,
            directly_affected,
            transitively_affected,
            affected_capabilities,
            affected_regions,
        })
    }

    pub fn evaluate_slo(
        &self,
        slo_id: &str,
        measurement: Option<&SliMeasurement>,
    ) -> Result<SloEvaluation, KernelError> {
        let slo = self
            .slos
            .get(slo_id)
            .ok_or_else(|| KernelError::UnknownSlo(slo_id.to_string()))?;
        let Some(measurement) = measurement else {
            return Ok(SloEvaluation {
                slo_id: slo.id.clone(),
                asset_id: slo.asset_id.clone(),
                standing: SloStanding::Unknown,
                observed: None,
                target: slo.target,
                error_budget_delta: None,
            });
        };
        if measurement.slo_id != slo.id || measurement.samples < slo.minimum_samples {
            return Ok(SloEvaluation {
                slo_id: slo.id.clone(),
                asset_id: slo.asset_id.clone(),
                standing: SloStanding::Unknown,
                observed: Some(measurement.observed),
                target: slo.target,
                error_budget_delta: None,
            });
        }
        let green = match slo.direction {
            ObjectiveDirection::AtLeast => measurement.observed >= slo.target,
            ObjectiveDirection::AtMost => measurement.observed <= slo.target,
        };
        let delta = match slo.direction {
            ObjectiveDirection::AtLeast => measurement.observed - slo.target,
            ObjectiveDirection::AtMost => slo.target - measurement.observed,
        };
        Ok(SloEvaluation {
            slo_id: slo.id.clone(),
            asset_id: slo.asset_id.clone(),
            standing: if green {
                SloStanding::Green
            } else {
                SloStanding::Breached
            },
            observed: Some(measurement.observed),
            target: slo.target,
            error_budget_delta: Some(delta),
        })
    }

    pub fn evaluate_promotion(
        &self,
        gate_id: &str,
        context: &PromotionContext,
        observations: &EnterpriseObservation,
    ) -> Result<PromotionDecision, KernelError> {
        let gate = self
            .promotion_gates
            .get(gate_id)
            .ok_or_else(|| KernelError::UnknownPromotionGate(gate_id.to_string()))?;
        let asset = self
            .assets
            .get(&gate.asset_id)
            .ok_or_else(|| KernelError::UnknownAsset(gate.asset_id.clone()))?;
        let mut reasons = Vec::new();
        if asset.lifecycle != gate.from {
            reasons.push(format!(
                "asset lifecycle {:?} does not equal gate source {:?}",
                asset.lifecycle, gate.from
            ));
        }
        if !gate.from.may_transition_to(gate.to) {
            reasons.push("gate requests an illegal lifecycle transition".to_string());
        }
        let missing: Vec<_> = gate
            .required_evidence
            .difference(&context.available_evidence)
            .cloned()
            .collect();
        if !missing.is_empty() {
            reasons.push(format!("missing evidence: {}", missing.join(", ")));
        }
        if context.approvals < gate.minimum_approvals {
            reasons.push(format!(
                "{} approvals provided; {} required",
                context.approvals, gate.minimum_approvals
            ));
        }
        if gate.require_receipt_replay && !context.receipt_replay_green {
            reasons.push("deterministic receipt replay is not green".to_string());
        }
        if gate.require_security_controls && !context.security_controls_green {
            reasons.push("security controls are not green".to_string());
        }
        if gate.require_slo_green {
            let related: Vec<_> = self
                .slos
                .values()
                .filter(|slo| slo.asset_id == gate.asset_id)
                .collect();
            if related.is_empty() {
                reasons.push("promotion requires SLOs but none are declared".to_string());
            }
            for slo in related {
                let evaluation =
                    self.evaluate_slo(&slo.id, observations.measurements.get(&slo.id))?;
                if evaluation.standing != SloStanding::Green {
                    reasons.push(format!(
                        "SLO {} is {:?}",
                        evaluation.slo_id, evaluation.standing
                    ));
                }
            }
        }
        Ok(PromotionDecision {
            gate_id: gate.id.clone(),
            asset_id: gate.asset_id.clone(),
            allowed: reasons.is_empty(),
            reasons,
        })
    }

    /// Pure MAPE-K policy evaluation. Returns broker-addressed intents only.
    pub fn evaluate_autonomics(
        &self,
        observations: &EnterpriseObservation,
    ) -> Result<Vec<ArchitectureIntent>, KernelError> {
        let mut intents = Vec::new();

        for (slo_id, slo) in &self.slos {
            let evaluation = self.evaluate_slo(slo_id, observations.measurements.get(slo_id))?;
            if evaluation.standing == SloStanding::Breached {
                let mut constraints = BTreeMap::new();
                constraints.insert("slo-id".to_string(), slo_id.clone());
                constraints.insert("target".to_string(), slo.target.to_string());
                if let Some(observed) = evaluation.observed {
                    constraints.insert("observed".to_string(), observed.to_string());
                }
                let evidence = BTreeSet::from([format!("slo:{slo_id}:breached")]);
                intents.push(ArchitectureIntent::new(
                    slo.asset_id.clone(),
                    ArchitectureIntentKind::EscalateErrorBudget,
                    IntentPriority::Critical,
                    format!("SLO {slo_id} breached its admitted objective"),
                    constraints.clone(),
                    evidence.clone(),
                ));
                intents.push(ArchitectureIntent::new(
                    slo.asset_id.clone(),
                    ArchitectureIntentKind::FreezePromotion,
                    IntentPriority::Critical,
                    format!("promotion must stop while SLO {slo_id} is breached"),
                    constraints,
                    evidence,
                ));
            }
        }

        for (asset_id, capacity) in &self.capacities {
            match capacity.standing() {
                CapacityStanding::WithinEnvelope => {}
                CapacityStanding::ScaleOut { required_replicas } => {
                    let constraints = BTreeMap::from([
                        (
                            "current-replicas".to_string(),
                            capacity.current_replicas.to_string(),
                        ),
                        (
                            "required-replicas".to_string(),
                            required_replicas.to_string(),
                        ),
                        (
                            "maximum-replicas".to_string(),
                            capacity.max_replicas.to_string(),
                        ),
                    ]);
                    intents.push(ArchitectureIntent::new(
                        asset_id.clone(),
                        ArchitectureIntentKind::ScaleOut,
                        IntentPriority::High,
                        "observed load exceeds admitted reserve envelope".to_string(),
                        constraints,
                        BTreeSet::from([format!("capacity:{asset_id}:scale-out")]),
                    ));
                }
                CapacityStanding::AdmissionControl {
                    required_replicas,
                    maximum_replicas,
                    unmet_capacity,
                } => {
                    let constraints = BTreeMap::from([
                        (
                            "required-replicas".to_string(),
                            required_replicas.to_string(),
                        ),
                        (
                            "maximum-replicas".to_string(),
                            maximum_replicas.to_string(),
                        ),
                        (
                            "unmet-capacity".to_string(),
                            unmet_capacity.to_string(),
                        ),
                    ]);
                    intents.push(ArchitectureIntent::new(
                        asset_id.clone(),
                        ArchitectureIntentKind::ThrottleAdmission,
                        IntentPriority::Critical,
                        "load exceeds the maximum admitted capacity envelope".to_string(),
                        constraints,
                        BTreeSet::from([format!("capacity:{asset_id}:exhausted")]),
                    ));
                }
            }
        }

        for (asset_id, policy) in &self.replication {
            let available = policy
                .regions
                .difference(&observations.unavailable_regions)
                .count();
            if available < usize::from(policy.write_quorum) {
                let constraints = BTreeMap::from([
                    ("available-regions".to_string(), available.to_string()),
                    (
                        "required-write-quorum".to_string(),
                        policy.write_quorum.to_string(),
                    ),
                    ("rto-seconds".to_string(), policy.rto_seconds.to_string()),
                    ("rpo-seconds".to_string(), policy.rpo_seconds.to_string()),
                ]);
                intents.push(ArchitectureIntent::new(
                    asset_id.clone(),
                    ArchitectureIntentKind::InitiateFailover,
                    IntentPriority::Critical,
                    "available regions cannot satisfy the admitted write quorum".to_string(),
                    constraints,
                    observations
                        .unavailable_regions
                        .iter()
                        .map(|region| format!("region:{region}:unavailable"))
                        .collect(),
                ));
            }
        }

        for (asset_id, policy) in &self.kms {
            if observations
                .key_age_days
                .get(asset_id)
                .is_some_and(|age| *age >= policy.rotation_days)
            {
                let age = observations.key_age_days[asset_id];
                intents.push(ArchitectureIntent::new(
                    asset_id.clone(),
                    ArchitectureIntentKind::RotateKey,
                    IntentPriority::Critical,
                    "KMS key reached its admitted rotation boundary".to_string(),
                    BTreeMap::from([
                        ("key-age-days".to_string(), age.to_string()),
                        (
                            "rotation-days".to_string(),
                            policy.rotation_days.to_string(),
                        ),
                        ("key-alias".to_string(), policy.key_alias.clone()),
                    ]),
                    BTreeSet::from([format!("kms:{asset_id}:rotation-due")]),
                ));
            }
        }

        for asset_id in self.identities.keys() {
            if observations
                .svid_ttl_remaining_seconds
                .get(asset_id)
                .is_some_and(|ttl| *ttl <= 300)
            {
                let ttl = observations.svid_ttl_remaining_seconds[asset_id];
                intents.push(ArchitectureIntent::new(
                    asset_id.clone(),
                    ArchitectureIntentKind::RenewIdentity,
                    IntentPriority::Critical,
                    "SPIFFE SVID is inside the renewal safety boundary".to_string(),
                    BTreeMap::from([(
                        "ttl-remaining-seconds".to_string(),
                        ttl.to_string(),
                    )]),
                    BTreeSet::from([format!("spiffe:{asset_id}:renewal-due")]),
                ));
            }
        }

        for asset_id in &observations.telemetry_stale_assets {
            if self.observability.contains_key(asset_id) {
                intents.push(ArchitectureIntent::new(
                    asset_id.clone(),
                    ArchitectureIntentKind::RestoreTelemetry,
                    IntentPriority::High,
                    "telemetry freshness is outside the admitted observation boundary".to_string(),
                    BTreeMap::new(),
                    BTreeSet::from([format!("telemetry:{asset_id}:stale")]),
                ));
            }
        }

        intents.sort_by(|left, right| {
            (&left.priority, &left.source_asset, &left.kind, &left.intent_id).cmp(&(
                &right.priority,
                &right.source_asset,
                &right.kind,
                &right.intent_id,
            ))
        });
        Ok(intents)
    }

    pub fn snapshot_digest(&self) -> Result<String, KernelError> {
        canonical_digest(self)
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
pub struct ReceiptEnvelope {
    pub schema: String,
    pub operation: String,
    pub subject: String,
    pub revision: String,
    pub input_digest: String,
    pub output_digest: String,
    pub previous_digest: String,
    pub digest_algorithm: String,
    pub digest: String,
}

impl ReceiptEnvelope {
    pub fn issue<I: Serialize, O: Serialize>(
        operation: impl Into<String>,
        subject: impl Into<String>,
        revision: impl Into<String>,
        input: &I,
        output: &O,
        previous_digest: impl Into<String>,
    ) -> Result<Self, KernelError> {
        let mut receipt = Self {
            schema: RECEIPT_SCHEMA.to_string(),
            operation: operation.into(),
            subject: subject.into(),
            revision: revision.into(),
            input_digest: canonical_digest(input)?,
            output_digest: canonical_digest(output)?,
            previous_digest: previous_digest.into(),
            digest_algorithm: "blake3".to_string(),
            digest: String::new(),
        };
        receipt.digest = receipt.compute_digest()?;
        Ok(receipt)
    }

    pub fn verify(&self) -> Result<(), KernelError> {
        if self.schema != RECEIPT_SCHEMA || self.digest_algorithm != "blake3" {
            return Err(KernelError::ReceiptInvalid(
                "unsupported receipt schema or digest algorithm".to_string(),
            ));
        }
        let computed = self.compute_digest()?;
        if self.digest != computed {
            return Err(KernelError::ReceiptInvalid(
                "receipt digest mismatch".to_string(),
            ));
        }
        Ok(())
    }

    fn compute_digest(&self) -> Result<String, KernelError> {
        #[derive(Serialize)]
        struct ReceiptBody<'a> {
            schema: &'a str,
            operation: &'a str,
            subject: &'a str,
            revision: &'a str,
            input_digest: &'a str,
            output_digest: &'a str,
            previous_digest: &'a str,
            digest_algorithm: &'a str,
        }
        canonical_digest(&ReceiptBody {
            schema: &self.schema,
            operation: &self.operation,
            subject: &self.subject,
            revision: &self.revision,
            input_digest: &self.input_digest,
            output_digest: &self.output_digest,
            previous_digest: &self.previous_digest,
            digest_algorithm: &self.digest_algorithm,
        })
    }
}

#[derive(Debug, Clone, Default, PartialEq, Eq, Serialize, Deserialize)]
pub struct ReceiptLedger {
    #[serde(default)]
    pub receipts: Vec<ReceiptEnvelope>,
}

impl ReceiptLedger {
    pub fn append(&mut self, receipt: ReceiptEnvelope) -> Result<(), KernelError> {
        receipt.verify()?;
        let expected_previous = self
            .receipts
            .last()
            .map_or("GENESIS", |previous| previous.digest.as_str());
        if receipt.previous_digest != expected_previous {
            return Err(KernelError::ReceiptInvalid(format!(
                "expected predecessor {expected_previous}, observed {}",
                receipt.previous_digest
            )));
        }
        self.receipts.push(receipt);
        Ok(())
    }

    pub fn verify(&self) -> Result<(), KernelError> {
        let mut expected_previous = "GENESIS";
        for receipt in &self.receipts {
            receipt.verify()?;
            if receipt.previous_digest != expected_previous {
                return Err(KernelError::ReceiptInvalid(format!(
                    "broken predecessor link at {}",
                    receipt.subject
                )));
            }
            expected_previous = &receipt.digest;
        }
        Ok(())
    }

    pub fn head(&self) -> &str {
        self.receipts
            .last()
            .map_or("GENESIS", |receipt| receipt.digest.as_str())
    }
}

pub fn canonical_digest<T: Serialize>(value: &T) -> Result<String, KernelError> {
    let value = serde_json::to_value(value)?;
    let canonical = canonicalize(value);
    let bytes = serde_json::to_vec(&canonical)?;
    Ok(blake3::hash(&bytes).to_hex().to_string())
}

fn canonicalize(value: Value) -> Value {
    match value {
        Value::Array(values) => Value::Array(values.into_iter().map(canonicalize).collect()),
        Value::Object(values) => {
            let sorted: BTreeMap<String, Value> = values
                .into_iter()
                .map(|(key, value)| (key, canonicalize(value)))
                .collect();
            Value::Object(sorted.into_iter().collect())
        }
        scalar => scalar,
    }
}

#[derive(Debug, Error)]
pub enum KernelError {
    #[error("unknown architecture asset: {0}")]
    UnknownAsset(String),
    #[error("unknown SLO: {0}")]
    UnknownSlo(String),
    #[error("unknown promotion gate: {0}")]
    UnknownPromotionGate(String),
    #[error("architecture dependency cycle: {0}")]
    DependencyCycle(String),
    #[error("invalid receipt: {0}")]
    ReceiptInvalid(String),
    #[error("serialization failed: {0}")]
    Serialization(#[from] serde_json::Error),
}

#[cfg(test)]
mod tests {
    use super::*;

    fn fixture() -> ArchitectureModel {
        let regions = BTreeMap::from([
            (
                "us-west-2".to_string(),
                Region {
                    id: "us-west-2".to_string(),
                    provider: "aws".to_string(),
                    jurisdiction: "US".to_string(),
                    fault_domain: "aws-us-west".to_string(),
                    active: true,
                },
            ),
            (
                "us-east-1".to_string(),
                Region {
                    id: "us-east-1".to_string(),
                    provider: "aws".to_string(),
                    jurisdiction: "US".to_string(),
                    fault_domain: "aws-us-east".to_string(),
                    active: true,
                },
            ),
            (
                "us-central1".to_string(),
                Region {
                    id: "us-central1".to_string(),
                    provider: "gcp".to_string(),
                    jurisdiction: "US".to_string(),
                    fault_domain: "gcp-us-central".to_string(),
                    active: true,
                },
            ),
        ]);

        let assets = BTreeMap::from([
            (
                "identity".to_string(),
                ArchitectureAsset {
                    id: "identity".to_string(),
                    name: "Workload identity".to_string(),
                    kind: AssetKind::IdentityDomain,
                    lifecycle: LifecycleState::Active,
                    owner: "security-platform".to_string(),
                    criticality: Criticality::Tier2,
                    dependencies: BTreeSet::new(),
                    capabilities: BTreeSet::new(),
                    regions: BTreeSet::from([
                        "us-west-2".to_string(),
                        "us-east-1".to_string(),
                    ]),
                    evidence: BTreeSet::new(),
                    tags: BTreeMap::new(),
                },
            ),
            (
                "ledger".to_string(),
                ArchitectureAsset {
                    id: "ledger".to_string(),
                    name: "Receipt ledger".to_string(),
                    kind: AssetKind::DataProduct,
                    lifecycle: LifecycleState::Active,
                    owner: "platform".to_string(),
                    criticality: Criticality::Tier2,
                    dependencies: BTreeSet::new(),
                    capabilities: BTreeSet::new(),
                    regions: BTreeSet::from([
                        "us-west-2".to_string(),
                        "us-east-1".to_string(),
                    ]),
                    evidence: BTreeSet::new(),
                    tags: BTreeMap::new(),
                },
            ),
            (
                "orders".to_string(),
                ArchitectureAsset {
                    id: "orders".to_string(),
                    name: "Order execution service".to_string(),
                    kind: AssetKind::Service,
                    lifecycle: LifecycleState::Admitted,
                    owner: "commerce".to_string(),
                    criticality: Criticality::Tier0,
                    dependencies: BTreeSet::from([
                        "identity".to_string(),
                        "ledger".to_string(),
                    ]),
                    capabilities: BTreeSet::from(["execute-orders".to_string()]),
                    regions: BTreeSet::from([
                        "us-west-2".to_string(),
                        "us-east-1".to_string(),
                        "us-central1".to_string(),
                    ]),
                    evidence: BTreeSet::from(["threat-model".to_string()]),
                    tags: BTreeMap::from([("data-class".to_string(), "restricted".to_string())]),
                },
            ),
            (
                "checkout".to_string(),
                ArchitectureAsset {
                    id: "checkout".to_string(),
                    name: "Checkout application".to_string(),
                    kind: AssetKind::Application,
                    lifecycle: LifecycleState::Active,
                    owner: "commerce".to_string(),
                    criticality: Criticality::Tier2,
                    dependencies: BTreeSet::from(["orders".to_string()]),
                    capabilities: BTreeSet::from(["sell-products".to_string()]),
                    regions: BTreeSet::from(["us-west-2".to_string()]),
                    evidence: BTreeSet::new(),
                    tags: BTreeMap::new(),
                },
            ),
        ]);

        let capabilities = BTreeMap::from([
            (
                "execute-orders".to_string(),
                EnterpriseCapability {
                    id: "execute-orders".to_string(),
                    name: "Execute orders".to_string(),
                    owner: "commerce".to_string(),
                    lifecycle: LifecycleState::Active,
                    realized_by: BTreeSet::from(["orders".to_string()]),
                },
            ),
            (
                "sell-products".to_string(),
                EnterpriseCapability {
                    id: "sell-products".to_string(),
                    name: "Sell products".to_string(),
                    owner: "commerce".to_string(),
                    lifecycle: LifecycleState::Active,
                    realized_by: BTreeSet::from(["checkout".to_string()]),
                },
            ),
        ]);

        let slos = BTreeMap::from([(
            "orders-availability".to_string(),
            SloDefinition {
                id: "orders-availability".to_string(),
                asset_id: "orders".to_string(),
                sli: SliKind::Availability,
                direction: ObjectiveDirection::AtLeast,
                target: 99.99,
                window_seconds: 2_592_000,
                minimum_samples: 1_000,
                fast_burn_threshold: 14.4,
            },
        )]);

        let capacities = BTreeMap::from([(
            "orders".to_string(),
            CapacityEnvelope {
                asset_id: "orders".to_string(),
                min_replicas: 3,
                max_replicas: 12,
                current_replicas: 3,
                unit_capacity: 1_000.0,
                observed_load: 2_200.0,
                reserve_ratio: 0.20,
            },
        )]);

        let replication = BTreeMap::from([(
            "orders".to_string(),
            ReplicationPolicy {
                asset_id: "orders".to_string(),
                mode: ReplicationMode::Strong,
                regions: BTreeSet::from([
                    "us-west-2".to_string(),
                    "us-east-1".to_string(),
                    "us-central1".to_string(),
                ]),
                write_quorum: 2,
                read_quorum: 2,
                rpo_seconds: 0,
                rto_seconds: 60,
                allowed_jurisdictions: BTreeSet::from(["US".to_string()]),
            },
        )]);

        let identities = BTreeMap::from([(
            "orders".to_string(),
            IdentityPolicy {
                asset_id: "orders".to_string(),
                trust_domain: "corp.example".to_string(),
                allowed_spiffe_ids: BTreeSet::from([
                    "spiffe://corp.example/ns/commerce/sa/orders".to_string(),
                ]),
                mtls_required: true,
                max_svid_ttl_seconds: 3_600,
                workload_attestation_required: true,
            },
        )]);

        let kms = BTreeMap::from([(
            "orders".to_string(),
            KmsPolicy {
                asset_id: "orders".to_string(),
                provider: KmsProvider::AwsKms,
                key_alias: "alias/orders-prod".to_string(),
                rotation_days: 30,
                hsm_backed: true,
                envelope_encryption: true,
                decrypt_audit_required: true,
                break_glass_dual_control: true,
            },
        )]);

        let observability = BTreeMap::from([(
            "orders".to_string(),
            ObservabilityPolicy {
                asset_id: "orders".to_string(),
                logs: true,
                metrics: true,
                traces: true,
                profiles: true,
                correlation_ids: true,
                redaction_required: true,
                retention_days: 400,
                otlp_endpoint_class: "regional-collector".to_string(),
            },
        )]);

        let promotion_gates = BTreeMap::from([(
            "orders-production".to_string(),
            PromotionGate {
                id: "orders-production".to_string(),
                asset_id: "orders".to_string(),
                from: LifecycleState::Admitted,
                to: LifecycleState::Active,
                required_evidence: BTreeSet::from([
                    "threat-model".to_string(),
                    "load-test".to_string(),
                    "replay-receipt".to_string(),
                ]),
                minimum_approvals: 2,
                require_slo_green: true,
                require_receipt_replay: true,
                require_security_controls: true,
            },
        )]);

        ArchitectureModel {
            assets,
            capabilities,
            regions,
            slos,
            capacities,
            replication,
            identities,
            kms,
            observability,
            promotion_gates,
        }
    }

    #[test]
    fn fortune_five_fixture_is_valid() {
        let model = fixture();
        assert_eq!(model.validate(), Vec::<Violation>::new());
        assert_eq!(
            model.topological_order().expect("topological order"),
            vec!["identity", "ledger", "orders", "checkout"]
        );
    }

    #[test]
    fn lifecycle_skips_are_impossible() {
        assert!(LifecycleState::Proposed.may_transition_to(LifecycleState::Admitted));
        assert!(!LifecycleState::Proposed.may_transition_to(LifecycleState::Active));
        assert!(!LifecycleState::Active.may_transition_to(LifecycleState::Retired));
    }

    #[test]
    fn impact_analysis_closes_over_dependents() {
        let report = fixture()
            .impact_analysis(["identity"])
            .expect("impact analysis");
        assert_eq!(report.directly_affected, BTreeSet::from(["orders".to_string()]));
        assert_eq!(
            report.transitively_affected,
            BTreeSet::from(["checkout".to_string(), "orders".to_string()])
        );
        assert!(report.affected_capabilities.contains("sell-products"));
    }

    #[test]
    fn path_selection_is_deterministic_and_auditable() {
        assert_eq!(select_execution_path(8, 0, true).path, ExecutionPath::Hot);
        assert_eq!(
            select_execution_path(1_000, 4, true).path,
            ExecutionPath::Warm
        );
        assert_eq!(
            select_execution_path(1_001, 0, true).path,
            ExecutionPath::Cold
        );
        assert_eq!(select_execution_path(8, 1, true).path, ExecutionPath::Warm);
    }

    #[test]
    fn promotion_requires_evidence_approvals_security_replay_and_green_slos() {
        let model = fixture();
        let observations = EnterpriseObservation {
            measurements: BTreeMap::from([(
                "orders-availability".to_string(),
                SliMeasurement {
                    slo_id: "orders-availability".to_string(),
                    observed: 99.995,
                    samples: 10_000,
                },
            )]),
            ..Default::default()
        };
        let context = PromotionContext {
            available_evidence: BTreeSet::from([
                "threat-model".to_string(),
                "load-test".to_string(),
                "replay-receipt".to_string(),
            ]),
            approvals: 2,
            receipt_replay_green: true,
            security_controls_green: true,
        };
        let decision = model
            .evaluate_promotion("orders-production", &context, &observations)
            .expect("promotion decision");
        assert!(decision.allowed, "{:?}", decision.reasons);

        let denied = model
            .evaluate_promotion(
                "orders-production",
                &PromotionContext {
                    approvals: 1,
                    ..context
                },
                &observations,
            )
            .expect("denied decision");
        assert!(!denied.allowed);
        assert!(denied.reasons.iter().any(|reason| reason.contains("approvals")));
    }

    #[test]
    fn autonomics_emit_brce_intents_without_actuation() {
        let mut model = fixture();
        model.capacities.get_mut("orders").expect("capacity").observed_load = 4_000.0;
        let observations = EnterpriseObservation {
            measurements: BTreeMap::from([(
                "orders-availability".to_string(),
                SliMeasurement {
                    slo_id: "orders-availability".to_string(),
                    observed: 99.0,
                    samples: 10_000,
                },
            )]),
            unavailable_regions: BTreeSet::from([
                "us-east-1".to_string(),
                "us-central1".to_string(),
            ]),
            key_age_days: BTreeMap::from([("orders".to_string(), 30)]),
            svid_ttl_remaining_seconds: BTreeMap::from([("orders".to_string(), 120)]),
            telemetry_stale_assets: BTreeSet::from(["orders".to_string()]),
        };
        let intents = model
            .evaluate_autonomics(&observations)
            .expect("autonomic intents");
        let kinds: BTreeSet<_> = intents.iter().map(|intent| intent.kind.clone()).collect();
        for required in [
            ArchitectureIntentKind::EscalateErrorBudget,
            ArchitectureIntentKind::FreezePromotion,
            ArchitectureIntentKind::ScaleOut,
            ArchitectureIntentKind::InitiateFailover,
            ArchitectureIntentKind::RotateKey,
            ArchitectureIntentKind::RenewIdentity,
            ArchitectureIntentKind::RestoreTelemetry,
        ] {
            assert!(kinds.contains(&required), "missing {required:?}");
        }
        assert!(intents.iter().all(|intent| intent.broker == REQUIRED_BROKER));
        assert!(intents.iter().all(|intent| intent.schema == INTENT_SCHEMA));
    }

    #[test]
    fn capacity_exhaustion_manufactures_admission_control_intent() {
        let mut model = fixture();
        model.capacities.get_mut("orders").expect("capacity").observed_load = 100_000.0;
        let intents = model
            .evaluate_autonomics(&EnterpriseObservation::default())
            .expect("autonomics");
        assert!(intents
            .iter()
            .any(|intent| intent.kind == ArchitectureIntentKind::ThrottleAdmission));
    }

    #[test]
    fn critical_asset_without_controls_is_refused() {
        let mut model = fixture();
        model.kms.remove("orders");
        model.observability.remove("orders");
        let codes: BTreeSet<_> = model.validate().into_iter().map(|v| v.code).collect();
        assert!(codes.contains("KMS_POLICY_MISSING"));
        assert!(codes.contains("OBSERVABILITY_POLICY_MISSING"));
    }

    #[test]
    fn dependency_cycles_are_refused() {
        let mut model = fixture();
        model
            .assets
            .get_mut("identity")
            .expect("identity")
            .dependencies
            .insert("checkout".to_string());
        assert!(model
            .validate()
            .iter()
            .any(|violation| violation.code == "DEPENDENCY_CYCLE"));
        assert!(matches!(
            model.topological_order(),
            Err(KernelError::DependencyCycle(_))
        ));
    }

    #[test]
    fn receipts_are_deterministic_and_predecessor_linked() {
        let model = fixture();
        let digest = model.snapshot_digest().expect("snapshot digest");
        let first = ReceiptEnvelope::issue(
            "validate",
            "enterprise-architecture",
            "abc123",
            &model,
            &model.validate(),
            "GENESIS",
        )
        .expect("first receipt");
        let again = ReceiptEnvelope::issue(
            "validate",
            "enterprise-architecture",
            "abc123",
            &model,
            &model.validate(),
            "GENESIS",
        )
        .expect("repeat receipt");
        assert_eq!(first.digest, again.digest);
        assert_eq!(first.input_digest, digest);

        let second = ReceiptEnvelope::issue(
            "impact",
            "identity",
            "abc123",
            &BTreeSet::from(["identity"]),
            &model.impact_analysis(["identity"]).expect("impact"),
            &first.digest,
        )
        .expect("second receipt");
        let mut ledger = ReceiptLedger::default();
        ledger.append(first).expect("append first");
        ledger.append(second).expect("append second");
        ledger.verify().expect("verify ledger");
        assert_ne!(ledger.head(), "GENESIS");
    }

    #[test]
    fn receipt_tampering_is_refused() {
        let model = fixture();
        let mut receipt = ReceiptEnvelope::issue(
            "validate",
            "enterprise-architecture",
            "abc123",
            &model,
            &model.validate(),
            "GENESIS",
        )
        .expect("receipt");
        receipt.output_digest.push('0');
        assert!(matches!(
            receipt.verify(),
            Err(KernelError::ReceiptInvalid(_))
        ));
    }
}
