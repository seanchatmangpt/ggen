//! Generic ggen Building Block law.
//!
//! A building block binds an implementation-independent architecture facet to
//! zero or more passport-addressed realizations, an executable contract,
//! evidence obligations, lifecycle, standing, and deterministic composition.
//! This module is pure: it performs no filesystem, process, network, cloud, or
//! deployment actuation.

use std::collections::{BTreeMap, BTreeSet};

use serde::{Deserialize, Serialize};
use thiserror::Error;

/// Canonical schema for deterministic composition receipts.
pub const BUILDING_BLOCK_RECEIPT_SCHEMA: &str = "ggen.building-block.receipt.v1";

macro_rules! string_id {
    ($name:ident, $doc:literal) => {
        #[doc = $doc]
        #[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash, Serialize, Deserialize)]
        #[serde(transparent)]
        pub struct $name(pub String);

        impl $name {
            /// Create an identifier. Admission validation rejects an empty value.
            #[must_use]
            pub fn new(value: impl Into<String>) -> Self {
                Self(value.into())
            }

            /// Borrow the identifier text.
            #[must_use]
            pub fn as_str(&self) -> &str {
                &self.0
            }
        }

        impl From<&str> for $name {
            fn from(value: &str) -> Self {
                Self::new(value)
            }
        }
    };
}

string_id!(
    BuildingBlockId,
    "Stable public identity of a building block."
);
string_id!(
    RealizationId,
    "Stable identity of one concrete realization."
);
string_id!(PortId, "Stable identity of a typed building-block port.");
string_id!(
    ProfileId,
    "Stable identity of an applied architecture profile."
);
string_id!(ObligationId, "Stable identity of an evidence obligation.");
string_id!(Authority, "A bounded authority or capability token.");

/// Lifecycle of a governed building block.
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Serialize, Deserialize)]
#[serde(rename_all = "snake_case")]
pub enum LifecycleState {
    /// Candidate observed but not yet stably identified.
    Discovered,
    /// Identity, source, and version are known.
    Identified,
    /// Syntax, provenance, contract, and dependency checks passed.
    Qualified,
    /// Admitted into the governed architecture boundary.
    Admitted,
    /// Available for normal use in its bounded context.
    Active,
    /// Usable only under an explicit migration window.
    Deprecated,
    /// New use is refused.
    Retired,
    /// Preserved only for provenance, evidence, and replay.
    Archived,
}

impl LifecycleState {
    /// Return whether one direct lifecycle transition is lawful.
    #[must_use]
    pub const fn allows(self, target: Self) -> bool {
        if self as u8 == target as u8 {
            return true;
        }

        matches!(
            (self, target),
            (Self::Discovered, Self::Identified | Self::Retired)
                | (Self::Identified, Self::Qualified | Self::Retired)
                | (Self::Qualified, Self::Admitted | Self::Retired)
                | (
                    Self::Admitted,
                    Self::Active | Self::Deprecated | Self::Retired
                )
                | (Self::Active, Self::Deprecated)
                | (Self::Deprecated, Self::Active | Self::Retired)
                | (Self::Retired, Self::Archived)
        )
    }
}

/// Evidentiary standing. Retirement is deliberately not a standing.
#[derive(Debug, Clone, Copy, Default, PartialEq, Eq, PartialOrd, Ord, Serialize, Deserialize)]
#[serde(rename_all = "SCREAMING_SNAKE_CASE")]
pub enum Standing {
    /// No sufficient execution observation exists.
    #[default]
    Unknown,
    /// A bounded subset is demonstrated; wider closure remains incomplete.
    PartialAlive,
    /// The entire declared bounded capability is demonstrated and replayable.
    Alive,
    /// Progress is prevented by an external or constitutional dependency.
    Blocked,
    /// The admitted source does not currently build.
    BuildBroken,
    /// The requested capability is outside the supported boundary.
    Unsupported,
}

/// Direction of information or authority across a port.
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Serialize, Deserialize)]
#[serde(rename_all = "snake_case")]
pub enum PortDirection {
    /// Input consumed by the block.
    Input,
    /// Output promised by the block.
    Output,
}

/// Semantic role of a port.
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Serialize, Deserialize)]
#[serde(rename_all = "snake_case")]
pub enum PortKind {
    /// Domain or application data.
    Data,
    /// Control input that does not itself actuate.
    Control,
    /// Evidence or verifier output.
    Evidence,
    /// A typed intent addressed to a broker.
    ActuationIntent,
}

/// One typed interface port.
#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
pub struct Port {
    /// Stable port identity.
    pub id: PortId,
    /// Input or output direction.
    pub direction: PortDirection,
    /// Semantic role.
    pub kind: PortKind,
    /// Schema, ontology class, or protocol identifier.
    pub schema: String,
    /// Whether the contract requires this port.
    pub required: bool,
}

/// Implementation-independent architecture requirement facet.
#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
pub struct ArchitectureFacet {
    /// Capability the block must realize.
    pub capability: String,
    /// Governing requirement identities.
    #[serde(default)]
    pub requirements: BTreeSet<String>,
    /// Constraints that every realization must preserve.
    #[serde(default)]
    pub constraints: BTreeSet<String>,
    /// Required quality attributes.
    #[serde(default)]
    pub quality_attributes: BTreeSet<String>,
    /// Maximum authorities any realization may exercise.
    #[serde(default)]
    pub permitted_authorities: BTreeSet<Authority>,
}

/// Maximum resources admitted for a realization.
#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
pub struct ResourceCeiling {
    /// Maximum resident memory in bytes.
    pub memory_bytes: u64,
    /// Maximum CPU time per bounded operation in milliseconds.
    pub cpu_millis: u64,
    /// Maximum bytes emitted by one bounded operation.
    pub output_bytes: u64,
    /// Maximum broker-addressed external intents per bounded operation.
    pub broker_intents: u32,
}

/// Resource claim made by one realization.
#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
pub struct ResourceClaim {
    /// Claimed resident memory in bytes.
    pub memory_bytes: u64,
    /// Claimed CPU time per bounded operation in milliseconds.
    pub cpu_millis: u64,
    /// Claimed output bytes per bounded operation.
    pub output_bytes: u64,
    /// Claimed broker-addressed external intents per bounded operation.
    pub broker_intents: u32,
}

impl ResourceClaim {
    /// Whether this claim fits entirely within a contract ceiling.
    #[must_use]
    pub const fn fits_within(&self, ceiling: &ResourceCeiling) -> bool {
        self.memory_bytes <= ceiling.memory_bytes
            && self.cpu_millis <= ceiling.cpu_millis
            && self.output_bytes <= ceiling.output_bytes
            && self.broker_intents <= ceiling.broker_intents
    }
}

/// Executable contract shared by every realization of a block.
#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
pub struct BuildingBlockContract {
    /// Required behavior clauses.
    #[serde(default)]
    pub behavior: BTreeSet<String>,
    /// Input ports every realization must consume lawfully.
    #[serde(default)]
    pub required_inputs: BTreeSet<PortId>,
    /// Output ports every realization must provide.
    #[serde(default)]
    pub promised_outputs: BTreeSet<PortId>,
    /// Maximum admitted resource envelope.
    pub resource_ceiling: ResourceCeiling,
    /// Maximum authorities permitted to a realization.
    #[serde(default)]
    pub authority_ceiling: BTreeSet<Authority>,
}

/// One concrete implementation bound to an independently verifiable passport.
#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
pub struct RealizationBinding {
    /// Stable realization identity.
    pub id: RealizationId,
    /// Building block this realization claims to implement.
    pub realizes: BuildingBlockId,
    /// Stable Part Passport identity.
    pub passport_id: String,
    /// Digest binding the exact admitted passport version.
    pub passport_digest: String,
    /// Ports provided by this realization.
    #[serde(default)]
    pub provided_ports: BTreeSet<PortId>,
    /// Authorities exercised by this realization.
    #[serde(default)]
    pub authorities: BTreeSet<Authority>,
    /// Bounded resource claim.
    pub resources: ResourceClaim,
}

/// Required evidence surface for one proof obligation.
#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
pub struct EvidenceObligation {
    /// Stable obligation identity.
    pub id: ObligationId,
    /// Command or fixture proving the positive behavior.
    pub positive_witness: String,
    /// Command or fixture proving a material negative refusal.
    pub negative_falsifier: String,
    /// Independently owned verifier command or identity.
    pub independent_verifier: String,
    /// Receipt-verification command or identity.
    pub receipt_verifier: String,
    /// Deterministic replay command or identity.
    pub replay: String,
}

/// Kind of observed evidence receipt.
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Serialize, Deserialize)]
#[serde(rename_all = "snake_case")]
pub enum EvidenceKind {
    /// Positive execution witness.
    PositiveWitness,
    /// Negative falsifier execution.
    NegativeFalsifier,
    /// Independent verifier result.
    IndependentVerifier,
    /// Receipt-verifier result.
    ReceiptVerifier,
    /// Deterministic replay result.
    Replay,
}

/// Observed evidence for one obligation surface.
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Serialize, Deserialize)]
pub struct EvidenceReceipt {
    /// Obligation satisfied by this evidence.
    pub obligation_id: ObligationId,
    /// Evidence surface that executed.
    pub kind: EvidenceKind,
    /// Non-empty digest of the externalized result.
    pub digest: String,
}

/// Canonical governed building block.
#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
pub struct BuildingBlock {
    /// Stable public identity.
    pub id: BuildingBlockId,
    /// Semantic version or immutable release identity.
    pub version: String,
    /// Accountable owner or steward.
    pub owner: String,
    /// Asset lifecycle.
    pub lifecycle: LifecycleState,
    /// Current evidence standing.
    #[serde(default)]
    pub standing: Standing,
    /// Implementation-independent requirements.
    pub architecture: ArchitectureFacet,
    /// Executable contract.
    pub contract: BuildingBlockContract,
    /// Typed ports.
    #[serde(default)]
    pub ports: BTreeMap<PortId, Port>,
    /// Other building blocks required by this block.
    #[serde(default)]
    pub dependencies: BTreeSet<BuildingBlockId>,
    /// Candidate realizations.
    #[serde(default)]
    pub realizations: BTreeMap<RealizationId, RealizationBinding>,
    /// Explicitly selected realization for the bounded context.
    #[serde(default)]
    pub selected_realization: Option<RealizationId>,
    /// Profiles whose obligations govern this block.
    #[serde(default)]
    pub profiles: BTreeSet<ProfileId>,
    /// Profiles that cannot be composed with this block.
    #[serde(default)]
    pub incompatible_profiles: BTreeSet<ProfileId>,
    /// Required proof surfaces.
    #[serde(default)]
    pub obligations: BTreeMap<ObligationId, EvidenceObligation>,
    /// Explicit exclusions from the block's claim boundary.
    #[serde(default)]
    pub exclusions: BTreeSet<String>,
    /// Stable provenance identity or digest.
    pub provenance: String,
}

/// Stable validation finding.
#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
pub struct BuildingBlockViolation {
    /// Stable machine-readable refusal code.
    pub code: String,
    /// Primary subject identity.
    pub subject: String,
    /// Human-readable explanation.
    pub message: String,
}

impl BuildingBlockViolation {
    fn new(code: &str, subject: impl Into<String>, message: impl Into<String>) -> Self {
        Self {
            code: code.to_string(),
            subject: subject.into(),
            message: message.into(),
        }
    }
}

impl BuildingBlock {
    /// Validate local identity, contract, realization, and evidence declarations.
    #[must_use]
    pub fn validate(&self) -> Vec<BuildingBlockViolation> {
        let mut violations = Vec::new();
        let subject = self.id.as_str();

        for (present, code, message) in [
            (
                !subject.trim().is_empty(),
                "GBB-IDENTITY-MISSING",
                "building-block identity is required",
            ),
            (
                !self.version.trim().is_empty(),
                "GBB-VERSION-MISSING",
                "building-block version is required",
            ),
            (
                !self.owner.trim().is_empty(),
                "GBB-OWNER-MISSING",
                "accountable owner is required",
            ),
            (
                !self.architecture.capability.trim().is_empty(),
                "GBB-CAPABILITY-MISSING",
                "architecture capability is required",
            ),
            (
                !self.contract.behavior.is_empty(),
                "GBB-CONTRACT-BEHAVIOR-MISSING",
                "at least one behavior clause is required",
            ),
            (
                !self.provenance.trim().is_empty(),
                "GBB-PROVENANCE-MISSING",
                "stable provenance is required",
            ),
        ] {
            if !present {
                violations.push(BuildingBlockViolation::new(code, subject, message));
            }
        }

        if self.dependencies.contains(&self.id) {
            violations.push(BuildingBlockViolation::new(
                "GBB-SELF-DEPENDENCY",
                subject,
                "a building block cannot depend on itself",
            ));
        }

        for input in &self.contract.required_inputs {
            match self.ports.get(input) {
                Some(port) if port.direction == PortDirection::Input => {}
                Some(_) => violations.push(BuildingBlockViolation::new(
                    "GBB-INPUT-DIRECTION-INVALID",
                    input.as_str(),
                    "a required input must reference an input port",
                )),
                None => violations.push(BuildingBlockViolation::new(
                    "GBB-INPUT-PORT-MISSING",
                    input.as_str(),
                    "a required input references an unknown port",
                )),
            }
        }

        for output in &self.contract.promised_outputs {
            match self.ports.get(output) {
                Some(port) if port.direction == PortDirection::Output => {}
                Some(_) => violations.push(BuildingBlockViolation::new(
                    "GBB-OUTPUT-DIRECTION-INVALID",
                    output.as_str(),
                    "a promised output must reference an output port",
                )),
                None => violations.push(BuildingBlockViolation::new(
                    "GBB-OUTPUT-PORT-MISSING",
                    output.as_str(),
                    "a promised output references an unknown port",
                )),
            }
        }

        if let Some(selected) = &self.selected_realization {
            if !self.realizations.contains_key(selected) {
                violations.push(BuildingBlockViolation::new(
                    "GBB-SELECTED-REALIZATION-MISSING",
                    selected.as_str(),
                    "the selected realization is not registered",
                ));
            }
        }

        for (id, realization) in &self.realizations {
            if id != &realization.id || realization.realizes != self.id {
                violations.push(BuildingBlockViolation::new(
                    "GBB-REALIZATION-IDENTITY-MISMATCH",
                    id.as_str(),
                    "realization identity or realizes relation does not match its registry position",
                ));
            }
            if realization.passport_id.trim().is_empty()
                || realization.passport_digest.trim().is_empty()
            {
                violations.push(BuildingBlockViolation::new(
                    "GBB-PASSPORT-BINDING-MISSING",
                    id.as_str(),
                    "a realization requires a passport identity and digest",
                ));
            }
            if !realization
                .authorities
                .is_subset(&self.contract.authority_ceiling)
                || !realization
                    .authorities
                    .is_subset(&self.architecture.permitted_authorities)
            {
                violations.push(BuildingBlockViolation::new(
                    "GBB-AUTHORITY-EXPANSION",
                    id.as_str(),
                    "realization authority exceeds its architecture or contract ceiling",
                ));
            }
            if !realization
                .resources
                .fits_within(&self.contract.resource_ceiling)
            {
                violations.push(BuildingBlockViolation::new(
                    "GBB-RESOURCE-CEILING-EXCEEDED",
                    id.as_str(),
                    "realization resource claim exceeds its contract ceiling",
                ));
            }
        }

        for obligation in self.obligations.values() {
            for (present, code, message) in [
                (
                    !obligation.positive_witness.trim().is_empty(),
                    "GBB-WITNESS-MISSING",
                    "positive witness is required",
                ),
                (
                    !obligation.negative_falsifier.trim().is_empty(),
                    "GBB-FALSIFIER-MISSING",
                    "negative falsifier is required",
                ),
                (
                    !obligation.independent_verifier.trim().is_empty(),
                    "GBB-VERIFIER-MISSING",
                    "independent verifier is required",
                ),
                (
                    !obligation.receipt_verifier.trim().is_empty(),
                    "GBB-RECEIPT-VERIFIER-MISSING",
                    "receipt verifier is required",
                ),
                (
                    !obligation.replay.trim().is_empty(),
                    "GBB-REPLAY-MISSING",
                    "replay command is required",
                ),
            ] {
                if !present {
                    violations.push(BuildingBlockViolation::new(
                        code,
                        obligation.id.as_str(),
                        message,
                    ));
                }
            }
        }

        violations
    }

    /// Derive evidence standing without conflating retirement with proof.
    #[must_use]
    pub fn evidence_standing(&self, receipts: &BTreeSet<EvidenceReceipt>) -> Standing {
        if self.obligations.is_empty() {
            return Standing::Unknown;
        }

        let mut completed = 0usize;
        for obligation_id in self.obligations.keys() {
            let complete = [
                EvidenceKind::PositiveWitness,
                EvidenceKind::NegativeFalsifier,
                EvidenceKind::IndependentVerifier,
                EvidenceKind::ReceiptVerifier,
                EvidenceKind::Replay,
            ]
            .into_iter()
            .all(|kind| {
                receipts.iter().any(|receipt| {
                    &receipt.obligation_id == obligation_id
                        && receipt.kind == kind
                        && !receipt.digest.trim().is_empty()
                })
            });
            if complete {
                completed += 1;
            }
        }

        if completed == self.obligations.len() {
            Standing::Alive
        } else if receipts.is_empty() {
            Standing::Unknown
        } else {
            Standing::PartialAlive
        }
    }

    /// Assess whether one registered realization may lawfully replace another.
    pub fn assess_substitution(
        &self, from: &RealizationId, to: &RealizationId,
    ) -> Result<SubstitutionAssessment, BuildingBlockRefusal> {
        let source = self
            .realizations
            .get(from)
            .ok_or_else(|| BuildingBlockRefusal::UnknownRealization(from.clone()))?;
        let target = self
            .realizations
            .get(to)
            .ok_or_else(|| BuildingBlockRefusal::UnknownRealization(to.clone()))?;

        let mut reasons = Vec::new();
        if target.realizes != self.id {
            reasons.push("target realization belongs to a different building block".to_string());
        }
        if !self
            .contract
            .promised_outputs
            .is_subset(&target.provided_ports)
        {
            reasons
                .push("target realization does not preserve all promised output ports".to_string());
        }
        if !target.authorities.is_subset(&source.authorities)
            || !target
                .authorities
                .is_subset(&self.contract.authority_ceiling)
        {
            reasons.push("target realization expands authority".to_string());
        }
        if !target
            .resources
            .fits_within(&self.contract.resource_ceiling)
        {
            reasons.push("target realization exceeds the resource ceiling".to_string());
        }
        if target.passport_id.trim().is_empty() || target.passport_digest.trim().is_empty() {
            reasons
                .push("target realization is not bound to an admitted Part Passport".to_string());
        }

        Ok(SubstitutionAssessment {
            block_id: self.id.clone(),
            from: from.clone(),
            to: to.clone(),
            allowed: reasons.is_empty(),
            reasons,
        })
    }
}

/// Deterministic registry for building-block closure and composition.
#[derive(Debug, Clone, Default, PartialEq, Eq, Serialize, Deserialize)]
pub struct BuildingBlockRegistry {
    /// Registered building blocks by stable identity.
    #[serde(default)]
    pub blocks: BTreeMap<BuildingBlockId, BuildingBlock>,
}

impl BuildingBlockRegistry {
    /// Create an empty registry.
    #[must_use]
    pub const fn new() -> Self {
        Self {
            blocks: BTreeMap::new(),
        }
    }

    /// Register one block, refusing identity collision.
    pub fn register(&mut self, block: BuildingBlock) -> Result<(), BuildingBlockRefusal> {
        if self.blocks.contains_key(&block.id) {
            return Err(BuildingBlockRefusal::DuplicateBlock(block.id));
        }
        self.blocks.insert(block.id.clone(), block);
        Ok(())
    }

    /// Validate local law plus dependency and profile closure.
    #[must_use]
    pub fn validate(&self) -> Vec<BuildingBlockViolation> {
        let mut violations = Vec::new();
        for block in self.blocks.values() {
            violations.extend(block.validate());
            for dependency in &block.dependencies {
                if !self.blocks.contains_key(dependency) {
                    violations.push(BuildingBlockViolation::new(
                        "GBB-DEPENDENCY-MISSING",
                        block.id.as_str(),
                        format!("dependency {} is not registered", dependency.as_str()),
                    ));
                }
            }
            for profile in &block.profiles {
                if block.incompatible_profiles.contains(profile) {
                    violations.push(BuildingBlockViolation::new(
                        "GBB-PROFILE-CONFLICT",
                        block.id.as_str(),
                        format!("profile {} is explicitly incompatible", profile.as_str()),
                    ));
                }
            }
        }
        if let Err(BuildingBlockRefusal::DependencyCycle(cycle)) = self.dependency_order() {
            violations.push(BuildingBlockViolation::new(
                "GBB-DEPENDENCY-CYCLE",
                cycle.first().map_or("unknown", BuildingBlockId::as_str),
                cycle
                    .iter()
                    .map(BuildingBlockId::as_str)
                    .collect::<Vec<_>>()
                    .join(" -> "),
            ));
        }
        violations
    }

    /// Return all blocks in dependency-before-dependent order.
    pub fn dependency_order(&self) -> Result<Vec<BuildingBlockId>, BuildingBlockRefusal> {
        #[derive(Clone, Copy, PartialEq, Eq)]
        enum Visit {
            Visiting,
            Complete,
        }

        fn visit(
            id: &BuildingBlockId, registry: &BuildingBlockRegistry,
            marks: &mut BTreeMap<BuildingBlockId, Visit>, stack: &mut Vec<BuildingBlockId>,
            order: &mut Vec<BuildingBlockId>,
        ) -> Result<(), BuildingBlockRefusal> {
            match marks.get(id) {
                Some(Visit::Complete) => return Ok(()),
                Some(Visit::Visiting) => {
                    let start = stack.iter().position(|item| item == id).unwrap_or(0);
                    let mut cycle = stack[start..].to_vec();
                    cycle.push(id.clone());
                    return Err(BuildingBlockRefusal::DependencyCycle(cycle));
                }
                None => {}
            }

            let block = registry
                .blocks
                .get(id)
                .ok_or_else(|| BuildingBlockRefusal::UnknownBlock(id.clone()))?;
            marks.insert(id.clone(), Visit::Visiting);
            stack.push(id.clone());
            for dependency in &block.dependencies {
                if !registry.blocks.contains_key(dependency) {
                    return Err(BuildingBlockRefusal::UnknownDependency {
                        block: id.clone(),
                        dependency: dependency.clone(),
                    });
                }
                visit(dependency, registry, marks, stack, order)?;
            }
            stack.pop();
            marks.insert(id.clone(), Visit::Complete);
            order.push(id.clone());
            Ok(())
        }

        let mut marks = BTreeMap::new();
        let mut stack = Vec::new();
        let mut order = Vec::new();
        for id in self.blocks.keys() {
            visit(id, self, &mut marks, &mut stack, &mut order)?;
        }
        Ok(order)
    }

    /// Compose the dependency closure of selected roots and issue a deterministic receipt.
    pub fn compose(
        &self, roots: &BTreeSet<BuildingBlockId>,
    ) -> Result<CompositionReceipt, BuildingBlockRefusal> {
        let violations = self.validate();
        if !violations.is_empty() {
            return Err(BuildingBlockRefusal::RegistryInvalid(violations));
        }

        let mut selected = BTreeSet::new();
        let mut pending: Vec<BuildingBlockId> = roots.iter().cloned().collect();
        while let Some(id) = pending.pop() {
            let block = self
                .blocks
                .get(&id)
                .ok_or_else(|| BuildingBlockRefusal::UnknownBlock(id.clone()))?;
            if selected.insert(id) {
                pending.extend(block.dependencies.iter().cloned());
            }
        }

        let global_order = self.dependency_order()?;
        let order: Vec<BuildingBlockId> = global_order
            .into_iter()
            .filter(|id| selected.contains(id))
            .collect();
        let profiles: BTreeSet<ProfileId> = selected
            .iter()
            .filter_map(|id| self.blocks.get(id))
            .flat_map(|block| block.profiles.iter().cloned())
            .collect();

        for id in &selected {
            let block = self
                .blocks
                .get(id)
                .ok_or_else(|| BuildingBlockRefusal::UnknownBlock(id.clone()))?;
            for incompatible in &block.incompatible_profiles {
                if profiles.contains(incompatible) {
                    return Err(BuildingBlockRefusal::ProfileConflict {
                        block: id.clone(),
                        profile: incompatible.clone(),
                    });
                }
            }
        }

        #[derive(Serialize)]
        struct ReceiptPayload<'a> {
            schema: &'static str,
            roots: &'a BTreeSet<BuildingBlockId>,
            blocks: &'a BTreeSet<BuildingBlockId>,
            order: &'a [BuildingBlockId],
            profiles: &'a BTreeSet<ProfileId>,
        }

        let payload = ReceiptPayload {
            schema: BUILDING_BLOCK_RECEIPT_SCHEMA,
            roots,
            blocks: &selected,
            order: &order,
            profiles: &profiles,
        };
        let bytes = serde_json::to_vec(&payload)?;
        let digest = blake3::hash(&bytes).to_hex().to_string();

        Ok(CompositionReceipt {
            schema: BUILDING_BLOCK_RECEIPT_SCHEMA.to_string(),
            roots: roots.clone(),
            blocks: selected,
            order,
            profiles,
            digest: format!("urn:blake3:{digest}"),
        })
    }
}

/// Deterministic receipt for one qualified composition candidate.
#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
pub struct CompositionReceipt {
    /// Receipt schema identifier.
    pub schema: String,
    /// Requested root blocks.
    pub roots: BTreeSet<BuildingBlockId>,
    /// Complete dependency closure.
    pub blocks: BTreeSet<BuildingBlockId>,
    /// Dependency-before-dependent order.
    pub order: Vec<BuildingBlockId>,
    /// Applied profile closure.
    pub profiles: BTreeSet<ProfileId>,
    /// Canonical BLAKE3 identity.
    pub digest: String,
}

/// Result of comparing two realization bindings.
#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
pub struct SubstitutionAssessment {
    /// Governing building block.
    pub block_id: BuildingBlockId,
    /// Existing realization.
    pub from: RealizationId,
    /// Candidate replacement.
    pub to: RealizationId,
    /// Whether every checked contract dimension is preserved.
    pub allowed: bool,
    /// Stable human-readable refusal reasons.
    #[serde(default)]
    pub reasons: Vec<String>,
}

/// Typed fail-closed errors from registry, composition, and substitution law.
#[derive(Debug, Error)]
pub enum BuildingBlockRefusal {
    /// A block identity already exists.
    #[error("building block `{0:?}` is already registered")]
    DuplicateBlock(BuildingBlockId),
    /// A requested block is unknown.
    #[error("building block `{0:?}` is not registered")]
    UnknownBlock(BuildingBlockId),
    /// A dependency is unknown.
    #[error("building block `{block:?}` requires unknown dependency `{dependency:?}`")]
    UnknownDependency {
        /// Requiring block.
        block: BuildingBlockId,
        /// Missing dependency.
        dependency: BuildingBlockId,
    },
    /// Dependency closure contains a cycle.
    #[error("building-block dependency cycle: {0:?}")]
    DependencyCycle(Vec<BuildingBlockId>),
    /// A requested realization is unknown.
    #[error("realization `{0:?}` is not registered")]
    UnknownRealization(RealizationId),
    /// A profile conflicts with a selected block.
    #[error("block `{block:?}` conflicts with profile `{profile:?}")]
    ProfileConflict {
        /// Conflicting block.
        block: BuildingBlockId,
        /// Conflicting profile.
        profile: ProfileId,
    },
    /// The registry contains one or more admission violations.
    #[error("building-block registry is invalid")]
    RegistryInvalid(Vec<BuildingBlockViolation>),
    /// Canonical receipt serialization failed.
    #[error("building-block receipt serialization failed: {0}")]
    Serialization(#[from] serde_json::Error),
}

#[cfg(test)]
mod tests {
    use super::*;

    fn sample_block(id: &str) -> BuildingBlock {
        let input = PortId::from("input");
        let output = PortId::from("output");
        let authority = Authority::from("read:graph");
        let realization_id = RealizationId::from("realization-v1");
        let block_id = BuildingBlockId::from(id);
        let ports = BTreeMap::from([
            (
                input.clone(),
                Port {
                    id: input.clone(),
                    direction: PortDirection::Input,
                    kind: PortKind::Data,
                    schema: "urn:schema:input".to_string(),
                    required: true,
                },
            ),
            (
                output.clone(),
                Port {
                    id: output.clone(),
                    direction: PortDirection::Output,
                    kind: PortKind::Evidence,
                    schema: "urn:schema:output".to_string(),
                    required: true,
                },
            ),
        ]);
        let realization = RealizationBinding {
            id: realization_id.clone(),
            realizes: block_id.clone(),
            passport_id: "urn:passport:v1".to_string(),
            passport_digest: "urn:blake3:passport".to_string(),
            provided_ports: BTreeSet::from([output.clone()]),
            authorities: BTreeSet::from([authority.clone()]),
            resources: ResourceClaim {
                memory_bytes: 64,
                cpu_millis: 10,
                output_bytes: 1_024,
                broker_intents: 0,
            },
        };
        let obligation = EvidenceObligation {
            id: ObligationId::from("proof"),
            positive_witness: "cargo test witness".to_string(),
            negative_falsifier: "cargo test falsifier".to_string(),
            independent_verifier: "cargo test verifier".to_string(),
            receipt_verifier: "ggen receipt verify".to_string(),
            replay: "ggen block replay".to_string(),
        };
        BuildingBlock {
            id: block_id,
            version: "26.7.30".to_string(),
            owner: "architecture-board".to_string(),
            lifecycle: LifecycleState::Admitted,
            standing: Standing::Unknown,
            architecture: ArchitectureFacet {
                capability: "deterministic projection".to_string(),
                requirements: BTreeSet::from(["REQ-1".to_string()]),
                constraints: BTreeSet::new(),
                quality_attributes: BTreeSet::from(["deterministic".to_string()]),
                permitted_authorities: BTreeSet::from([authority.clone()]),
            },
            contract: BuildingBlockContract {
                behavior: BTreeSet::from(["project admitted graph".to_string()]),
                required_inputs: BTreeSet::from([input]),
                promised_outputs: BTreeSet::from([output]),
                resource_ceiling: ResourceCeiling {
                    memory_bytes: 128,
                    cpu_millis: 20,
                    output_bytes: 2_048,
                    broker_intents: 0,
                },
                authority_ceiling: BTreeSet::from([authority]),
            },
            ports,
            dependencies: BTreeSet::new(),
            realizations: BTreeMap::from([(realization_id.clone(), realization)]),
            selected_realization: Some(realization_id),
            profiles: BTreeSet::from([ProfileId::from("core")]),
            incompatible_profiles: BTreeSet::new(),
            obligations: BTreeMap::from([(obligation.id.clone(), obligation)]),
            exclusions: BTreeSet::from(["direct actuation".to_string()]),
            provenance: "urn:git:commit:test".to_string(),
        }
    }

    #[test]
    fn lifecycle_and_standing_are_orthogonal() {
        assert!(LifecycleState::Deprecated.allows(LifecycleState::Retired));
        let block = sample_block("block");
        assert_eq!(block.evidence_standing(&BTreeSet::new()), Standing::Unknown);
    }

    #[test]
    fn dependency_order_is_deterministic() {
        let mut dependency = sample_block("dependency");
        dependency.selected_realization = None;
        dependency.realizations.clear();
        let mut root = sample_block("root");
        root.dependencies.insert(dependency.id.clone());
        let mut registry = BuildingBlockRegistry::new();
        assert!(registry.register(root).is_ok());
        assert!(registry.register(dependency).is_ok());
        let order = registry.dependency_order();
        assert!(matches!(
            order,
            Ok(value)
                if value
                    == vec![
                        BuildingBlockId::from("dependency"),
                        BuildingBlockId::from("root"),
                    ]
        ));
    }

    #[test]
    fn unknown_dependency_is_refused() {
        let mut block = sample_block("root");
        block.dependencies.insert(BuildingBlockId::from("missing"));
        let mut registry = BuildingBlockRegistry::new();
        assert!(registry.register(block).is_ok());
        assert!(matches!(
            registry.dependency_order(),
            Err(BuildingBlockRefusal::UnknownDependency { .. })
        ));
    }

    #[test]
    fn alive_requires_all_five_evidence_surfaces() {
        let block = sample_block("block");
        let obligation = ObligationId::from("proof");
        let partial = BTreeSet::from([EvidenceReceipt {
            obligation_id: obligation.clone(),
            kind: EvidenceKind::PositiveWitness,
            digest: "urn:blake3:witness".to_string(),
        }]);
        assert_eq!(block.evidence_standing(&partial), Standing::PartialAlive);

        let complete: BTreeSet<EvidenceReceipt> = [
            EvidenceKind::PositiveWitness,
            EvidenceKind::NegativeFalsifier,
            EvidenceKind::IndependentVerifier,
            EvidenceKind::ReceiptVerifier,
            EvidenceKind::Replay,
        ]
        .into_iter()
        .map(|kind| EvidenceReceipt {
            obligation_id: obligation.clone(),
            kind,
            digest: format!("urn:blake3:{kind:?}"),
        })
        .collect();
        assert_eq!(block.evidence_standing(&complete), Standing::Alive);
    }

    #[test]
    fn substitution_refuses_authority_expansion() {
        let mut block = sample_block("block");
        let replacement_id = RealizationId::from("replacement");
        let mut replacement = match block.realizations.values().next() {
            Some(value) => value.clone(),
            None => return,
        };
        replacement.id = replacement_id.clone();
        replacement
            .authorities
            .insert(Authority::from("write:filesystem"));
        block
            .realizations
            .insert(replacement_id.clone(), replacement);

        let assessment =
            block.assess_substitution(&RealizationId::from("realization-v1"), &replacement_id);
        assert!(matches!(assessment, Ok(value) if !value.allowed));
    }

    #[test]
    fn composition_receipt_is_replay_stable() {
        let block = sample_block("root");
        let mut registry = BuildingBlockRegistry::new();
        assert!(registry.register(block).is_ok());
        let roots = BTreeSet::from([BuildingBlockId::from("root")]);
        let first = registry.compose(&roots);
        let second = registry.compose(&roots);
        assert!(matches!(
            (&first, &second),
            (Ok(left), Ok(right)) if left == right && left.digest.starts_with("urn:blake3:")
        ));
    }
}
