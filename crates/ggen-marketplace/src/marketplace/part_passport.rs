//! Genesis-bearing interchangeable-part passports.
//!
//! An AC adapter nameplate is a compact constitutional surface: it identifies
//! the manufacturer and model, declares the admissible input envelope, promises
//! a bounded output, exposes polarity and capacity, names conformity marks, and
//! carries lifecycle instructions. A ggen part needs the same information for
//! causality rather than electricity.
//!
//! This module defines a machine-readable passport, deterministic human
//! nameplate projection, validation law, and contextual substitution check.
//! It intentionally does not claim that a signature, hash, or self-declared
//! mark is sufficient proof. Every mark points to evidence and every stable
//! passport requires at least one passed independent verifier.

use semver::Version;
use serde::{Deserialize, Serialize};
use std::collections::BTreeSet;
use std::fmt::{self, Write};

use crate::marketplace::models::{Manifest, PackageId, PackageVersion};

/// Latest passport schema understood by this implementation.
pub const CURRENT_PASSPORT_SCHEMA: u16 = 1;

/// Complete causal rating plate for one interchangeable part.
#[derive(Clone, Debug, Eq, PartialEq, Serialize, Deserialize)]
pub struct PartPassport {
    /// Passport schema version, independent from the part version.
    pub schema_version: u16,
    /// Stable identity and provenance coordinates.
    pub identity: PartIdentity,
    /// Conditioned observations that may enter the part.
    pub input: InputEnvelope,
    /// Standing-bearing consequence promised by the part.
    pub output: OutputContract,
    /// Direction of causal flow and authority.
    pub polarity: CausalPolarity,
    /// Maximum resources the host must be willing to supply.
    pub resources: ResourceEnvelope,
    /// Isolation from ambient host authority.
    pub isolation: IsolationClass,
    /// Host profiles on which the part is admitted to run.
    pub host_profiles: BTreeSet<HostProfile>,
    /// Policy or jurisdiction profiles under which the part is admitted.
    #[serde(default)]
    pub jurisdiction_profiles: BTreeSet<String>,
    /// Self-declared or externally issued conformity statements.
    #[serde(default)]
    pub conformity: Vec<ConformityMark>,
    /// Independent verification statements.
    #[serde(default)]
    pub verifiers: Vec<VerifierMark>,
    /// Explicit side-effect and shared-environment boundary.
    pub noninterference: NonInterferenceProfile,
    /// Installation, deprecation, revocation, and retirement law.
    pub lifecycle: LifecyclePolicy,
}

/// Identity fields analogous to manufacturer, model, lot, and serial markings.
#[derive(Clone, Debug, Eq, PartialEq, Serialize, Deserialize)]
pub struct PartIdentity {
    /// Stable package or part identifier.
    pub part_id: String,
    /// Product family to which the part belongs.
    pub family: String,
    /// Semantic version of this part design.
    pub version: String,
    /// Entity responsible for the part declaration.
    pub manufacturer: String,
    /// Digest of the exact executable or packaged artifact.
    pub artifact_digest: String,
    /// Digest of the admitted graph used to manufacture the artifact.
    pub source_graph_digest: String,
    /// URI or content address of the manufacturing receipt.
    pub build_receipt: String,
    /// Optional batch or release-run coordinate.
    #[serde(default, skip_serializing_if = "Option::is_none")]
    pub batch_id: Option<String>,
}

/// The conditioned input rail seen by the deterministic kernel.
#[derive(Clone, Debug, Eq, PartialEq, Serialize, Deserialize)]
pub struct InputEnvelope {
    /// Media type or canonical representation.
    pub representation: String,
    /// Schema or ontology profile identifying input meaning.
    pub schema: String,
    /// Inclusive protocol range accepted by the part.
    pub protocol: ProtocolRange,
    /// Maximum canonical payload size.
    pub max_payload_bytes: u64,
    /// Event classes accepted by the part.
    pub accepted_event_types: BTreeSet<String>,
    /// Fields that must be present after admission and canonicalization.
    pub required_fields: BTreeSet<String>,
    /// Temporal assumptions analogous to an adapter's frequency rating.
    pub temporal: TemporalProfile,
}

/// Inclusive semantic-version protocol range.
#[derive(Clone, Debug, Eq, PartialEq, Serialize, Deserialize)]
pub struct ProtocolRange {
    pub minimum: String,
    pub maximum: String,
}

impl ProtocolRange {
    /// Whether this range accepts every protocol version accepted by `required`.
    #[must_use]
    pub fn covers(&self, required: &Self) -> bool {
        let Ok(self_min) = Version::parse(&self.minimum) else {
            return false;
        };
        let Ok(self_max) = Version::parse(&self.maximum) else {
            return false;
        };
        let Ok(required_min) = Version::parse(&required.minimum) else {
            return false;
        };
        let Ok(required_max) = Version::parse(&required.maximum) else {
            return false;
        };

        self_min <= required_min && self_max >= required_max
    }
}

/// Clock and cadence assumptions supplied by the OTP/AtomVM conditioner.
#[derive(Clone, Debug, Eq, PartialEq, Serialize, Deserialize)]
pub struct TemporalProfile {
    pub clock: ClockDiscipline,
    pub max_events_per_second: u64,
    pub timeout_semantics: TimeoutSemantics,
    /// Ambient wall-clock reads make replay depend on undeclared state.
    pub wall_clock_access: bool,
}

#[derive(Clone, Copy, Debug, Eq, PartialEq, Ord, PartialOrd, Hash, Serialize, Deserialize)]
#[serde(rename_all = "snake_case")]
pub enum ClockDiscipline {
    Logical,
    MonotonicObserved,
    EventSupplied,
}

#[derive(Clone, Copy, Debug, Eq, PartialEq, Ord, PartialOrd, Hash, Serialize, Deserialize)]
#[serde(rename_all = "snake_case")]
pub enum TimeoutSemantics {
    ExplicitEvent,
    HostDeadline,
    Unsupported,
}

/// Declared postcondition and bounded output capacity.
#[derive(Clone, Debug, Eq, PartialEq, Serialize, Deserialize)]
pub struct OutputContract {
    pub representation: String,
    pub schema: String,
    pub artifact_type: String,
    pub deterministic_serialization: bool,
    pub receipt_required: bool,
    pub max_artifacts: u32,
}

/// Causal orientation analogous to center-positive/center-negative polarity.
#[derive(Clone, Debug, Eq, PartialEq, Serialize, Deserialize)]
pub struct CausalPolarity {
    pub consumes: BTreeSet<String>,
    pub produces: BTreeSet<String>,
    #[serde(default)]
    pub requires_authorities: BTreeSet<String>,
    #[serde(default)]
    pub emits_intents: BTreeSet<String>,
}

/// Bounded power draw of the software part.
#[derive(Clone, Debug, Eq, PartialEq, Serialize, Deserialize)]
pub struct ResourceEnvelope {
    pub max_memory_pages: u32,
    pub max_fuel: u64,
    pub max_execution_ms: u64,
    pub max_queue_depth: u32,
    pub max_concurrency: u16,
    /// Whether the part may use undeclared host IO.
    pub ambient_io: bool,
}

/// Isolation strength. Ordering permits minimum-isolation comparisons.
#[derive(Clone, Copy, Debug, Eq, PartialEq, Ord, PartialOrd, Hash, Serialize, Deserialize)]
#[serde(rename_all = "snake_case")]
pub enum IsolationClass {
    Ambient,
    CapabilityIsolated,
    DoubleInsulated,
}

/// Named deployment profile such as `server`, `ci`, `browser`, or `atomvm-edge`.
#[derive(Clone, Debug, Eq, PartialEq, Ord, PartialOrd, Hash, Serialize, Deserialize)]
pub struct HostProfile(String);

impl HostProfile {
    #[must_use]
    pub fn new(value: impl Into<String>) -> Self {
        Self(value.into())
    }

    #[must_use]
    pub fn as_str(&self) -> &str {
        &self.0
    }
}

impl fmt::Display for HostProfile {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.write_str(&self.0)
    }
}

/// Conformity declaration. `self_declared` distinguishes CE-like declaration
/// from an independent laboratory or verifier mark.
#[derive(Clone, Debug, Eq, PartialEq, Serialize, Deserialize)]
pub struct ConformityMark {
    pub profile: String,
    pub issuer: String,
    pub evidence_uri: String,
    pub artifact_digest: String,
    pub self_declared: bool,
}

#[derive(Clone, Copy, Debug, Eq, PartialEq, Ord, PartialOrd, Hash, Serialize, Deserialize)]
#[serde(rename_all = "snake_case")]
pub enum VerifierStatus {
    Passed,
    Failed,
    Unknown,
    Revoked,
}

/// Independent verification statement tied to the exact artifact digest.
#[derive(Clone, Debug, Eq, PartialEq, Serialize, Deserialize)]
pub struct VerifierMark {
    pub verifier: String,
    pub property: String,
    pub evidence_uri: String,
    pub artifact_digest: String,
    pub status: VerifierStatus,
}

/// Shared-environment compatibility analogous to electromagnetic compatibility.
#[derive(Clone, Debug, Eq, PartialEq, Serialize, Deserialize)]
pub struct NonInterferenceProfile {
    #[serde(default)]
    pub may_read: BTreeSet<String>,
    #[serde(default)]
    pub may_write: BTreeSet<String>,
    #[serde(default)]
    pub may_emit: BTreeSet<String>,
    #[serde(default)]
    pub forbidden: BTreeSet<String>,
}

#[derive(Clone, Copy, Debug, Eq, PartialEq, Ord, PartialOrd, Hash, Serialize, Deserialize)]
#[serde(rename_all = "snake_case")]
pub enum LifecycleState {
    Experimental,
    Candidate,
    Stable,
    Deprecated,
    Revoked,
}

impl LifecycleState {
    #[must_use]
    pub fn allows_new_installation(self) -> bool {
        matches!(self, Self::Experimental | Self::Candidate | Self::Stable)
    }
}

/// Lifecycle and disposal instructions analogous to WEEE and date-code fields.
#[derive(Clone, Debug, Eq, PartialEq, Serialize, Deserialize)]
pub struct LifecyclePolicy {
    pub state: LifecycleState,
    pub policy_version: String,
    pub retirement: RetirementPolicy,
}

#[derive(Clone, Debug, Eq, PartialEq, Serialize, Deserialize)]
pub struct RetirementPolicy {
    pub revoke_credentials: bool,
    pub preserve_receipts: bool,
    #[serde(default, skip_serializing_if = "Option::is_none")]
    pub migration_target: Option<String>,
    pub disposal_instructions: String,
}

/// Binding between an existing marketplace manifest and its separate passport
/// sidecar or embedded custom section.
#[derive(Clone, Debug, Serialize, Deserialize)]
pub struct PassportBinding {
    pub package_id: PackageId,
    pub package_version: PackageVersion,
    pub passport: PartPassport,
}

impl PassportBinding {
    /// Bind a passport to an existing manifest without changing legacy manifest
    /// serialization or breaking downstream struct literals.
    #[must_use]
    pub fn from_manifest(manifest: &Manifest, passport: PartPassport) -> Self {
        Self {
            package_id: manifest.id.clone(),
            package_version: manifest.version.clone(),
            passport,
        }
    }

    /// Validate both the passport and its identity binding.
    #[must_use]
    pub fn validate(&self) -> PassportValidationReport {
        let mut report = self.passport.validate();

        if self.passport.identity.part_id != self.package_id.as_str() {
            report.push(PassportViolation::new(
                PassportViolationCode::IdentityMismatch,
                "identity.part_id",
                format!(
                    "passport part_id '{}' does not match manifest id '{}'",
                    self.passport.identity.part_id, self.package_id
                ),
            ));
        }

        if self.passport.identity.version != self.package_version.as_str() {
            report.push(PassportViolation::new(
                PassportViolationCode::IdentityMismatch,
                "identity.version",
                format!(
                    "passport version '{}' does not match manifest version '{}'",
                    self.passport.identity.version, self.package_version
                ),
            ));
        }

        report
    }
}

/// Compact symbols projected onto the human-readable nameplate.
#[derive(Clone, Copy, Debug, Eq, PartialEq, Ord, PartialOrd, Hash, Serialize, Deserialize)]
#[serde(rename_all = "snake_case")]
pub enum NameplateMark {
    ConditionedInput,
    DeterministicOutput,
    NoAmbientAuthority,
    DoubleInsulated,
    LimitedConsequence,
    IndependentVerification,
    NonInterference,
    SeparateRetirement,
}

impl fmt::Display for NameplateMark {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let symbol = match self {
            Self::ConditionedInput => "O*",
            Self::DeterministicOutput => "D",
            Self::NoAmbientAuthority => "NAA",
            Self::DoubleInsulated => "II",
            Self::LimitedConsequence => "LCS",
            Self::IndependentVerification => "IV",
            Self::NonInterference => "NI",
            Self::SeparateRetirement => "RET",
        };
        f.write_str(symbol)
    }
}

#[derive(Clone, Copy, Debug, Eq, PartialEq, Ord, PartialOrd, Hash, Serialize, Deserialize)]
#[serde(rename_all = "snake_case")]
pub enum PassportViolationCode {
    UnsupportedSchema,
    MissingField,
    InvalidVersion,
    InvalidDigest,
    InvalidBound,
    InvalidProtocolRange,
    AmbientAuthorityViolation,
    DigestMismatch,
    MissingStandingEvidence,
    FailedVerifier,
    NonInterferenceConflict,
    IdentityMismatch,
}

#[derive(Clone, Debug, Eq, PartialEq, Serialize, Deserialize)]
pub struct PassportViolation {
    pub code: PassportViolationCode,
    pub field: String,
    pub message: String,
}

impl PassportViolation {
    #[must_use]
    pub fn new(
        code: PassportViolationCode, field: impl Into<String>, message: impl Into<String>,
    ) -> Self {
        Self {
            code,
            field: field.into(),
            message: message.into(),
        }
    }
}

#[derive(Clone, Debug, Default, Eq, PartialEq, Serialize, Deserialize)]
pub struct PassportValidationReport {
    pub violations: Vec<PassportViolation>,
}

impl PassportValidationReport {
    #[must_use]
    pub fn is_valid(&self) -> bool {
        self.violations.is_empty()
    }

    fn push(&mut self, violation: PassportViolation) {
        self.violations.push(violation);
    }
}

#[derive(Clone, Copy, Debug, Eq, PartialEq, Ord, PartialOrd, Hash, Serialize, Deserialize)]
#[serde(rename_all = "snake_case")]
pub enum SubstitutionViolationCode {
    InvalidCandidate,
    InvalidRequiredProfile,
    InputRepresentation,
    InputSchema,
    ProtocolRange,
    InputCapacity,
    EventSurface,
    RequiredFields,
    OutputContract,
    OutputCapacity,
    CausalPolarity,
    ResourceEnvelope,
    AmbientAuthority,
    IsolationClass,
    HostProfile,
    JurisdictionProfile,
    ConformityProfile,
    NonInterference,
    Lifecycle,
}

#[derive(Clone, Debug, Eq, PartialEq, Serialize, Deserialize)]
pub struct SubstitutionViolation {
    pub code: SubstitutionViolationCode,
    pub message: String,
}

#[derive(Clone, Debug, Default, Eq, PartialEq, Serialize, Deserialize)]
pub struct SubstitutionReport {
    pub compatible: bool,
    pub violations: Vec<SubstitutionViolation>,
}

impl SubstitutionReport {
    fn reject(&mut self, code: SubstitutionViolationCode, message: impl Into<String>) {
        self.compatible = false;
        self.violations.push(SubstitutionViolation {
            code,
            message: message.into(),
        });
    }
}

impl PartPassport {
    /// Validate the rating plate without consulting narrative trust.
    #[must_use]
    pub fn validate(&self) -> PassportValidationReport {
        let mut report = PassportValidationReport::default();

        if self.schema_version != CURRENT_PASSPORT_SCHEMA {
            report.push(PassportViolation::new(
                PassportViolationCode::UnsupportedSchema,
                "schema_version",
                format!(
                    "passport schema {} is unsupported; expected {}",
                    self.schema_version, CURRENT_PASSPORT_SCHEMA
                ),
            ));
        }

        require_non_empty(&mut report, "identity.part_id", &self.identity.part_id);
        require_non_empty(&mut report, "identity.family", &self.identity.family);
        require_non_empty(
            &mut report,
            "identity.manufacturer",
            &self.identity.manufacturer,
        );
        require_non_empty(
            &mut report,
            "identity.build_receipt",
            &self.identity.build_receipt,
        );

        if Version::parse(&self.identity.version).is_err() {
            report.push(PassportViolation::new(
                PassportViolationCode::InvalidVersion,
                "identity.version",
                "part version must be valid semantic versioning",
            ));
        }

        validate_digest(
            &mut report,
            "identity.artifact_digest",
            &self.identity.artifact_digest,
        );
        validate_digest(
            &mut report,
            "identity.source_graph_digest",
            &self.identity.source_graph_digest,
        );

        require_non_empty(
            &mut report,
            "input.representation",
            &self.input.representation,
        );
        require_non_empty(&mut report, "input.schema", &self.input.schema);

        let minimum = Version::parse(&self.input.protocol.minimum);
        let maximum = Version::parse(&self.input.protocol.maximum);
        match (minimum, maximum) {
            (Ok(minimum), Ok(maximum)) if minimum <= maximum => {}
            (Ok(_), Ok(_)) => report.push(PassportViolation::new(
                PassportViolationCode::InvalidProtocolRange,
                "input.protocol",
                "minimum protocol version exceeds maximum",
            )),
            _ => report.push(PassportViolation::new(
                PassportViolationCode::InvalidVersion,
                "input.protocol",
                "protocol bounds must be semantic versions",
            )),
        }

        require_positive(
            &mut report,
            "input.max_payload_bytes",
            self.input.max_payload_bytes,
        );
        require_non_empty_set(
            &mut report,
            "input.accepted_event_types",
            &self.input.accepted_event_types,
        );
        require_non_empty_set(
            &mut report,
            "input.required_fields",
            &self.input.required_fields,
        );
        require_positive(
            &mut report,
            "input.temporal.max_events_per_second",
            self.input.temporal.max_events_per_second,
        );

        require_non_empty(
            &mut report,
            "output.representation",
            &self.output.representation,
        );
        require_non_empty(&mut report, "output.schema", &self.output.schema);
        require_non_empty(
            &mut report,
            "output.artifact_type",
            &self.output.artifact_type,
        );
        require_positive(
            &mut report,
            "output.max_artifacts",
            u64::from(self.output.max_artifacts),
        );

        require_non_empty_set(&mut report, "polarity.consumes", &self.polarity.consumes);
        require_non_empty_set(&mut report, "polarity.produces", &self.polarity.produces);

        require_positive(
            &mut report,
            "resources.max_memory_pages",
            u64::from(self.resources.max_memory_pages),
        );
        require_positive(&mut report, "resources.max_fuel", self.resources.max_fuel);
        require_positive(
            &mut report,
            "resources.max_execution_ms",
            self.resources.max_execution_ms,
        );
        require_positive(
            &mut report,
            "resources.max_queue_depth",
            u64::from(self.resources.max_queue_depth),
        );
        require_positive(
            &mut report,
            "resources.max_concurrency",
            u64::from(self.resources.max_concurrency),
        );

        if self.isolation == IsolationClass::DoubleInsulated && self.resources.ambient_io {
            report.push(PassportViolation::new(
                PassportViolationCode::AmbientAuthorityViolation,
                "resources.ambient_io",
                "double-insulated parts cannot use ambient host IO",
            ));
        }

        if self.host_profiles.is_empty() {
            report.push(PassportViolation::new(
                PassportViolationCode::MissingField,
                "host_profiles",
                "at least one admitted host profile is required",
            ));
        }
        for host in &self.host_profiles {
            require_non_empty(&mut report, "host_profiles[]", host.as_str());
        }

        for mark in &self.conformity {
            require_non_empty(&mut report, "conformity.profile", &mark.profile);
            require_non_empty(&mut report, "conformity.issuer", &mark.issuer);
            require_non_empty(&mut report, "conformity.evidence_uri", &mark.evidence_uri);
            validate_evidence_digest(
                &mut report,
                "conformity.artifact_digest",
                &mark.artifact_digest,
                &self.identity.artifact_digest,
            );
        }

        for verifier in &self.verifiers {
            require_non_empty(&mut report, "verifiers.verifier", &verifier.verifier);
            require_non_empty(&mut report, "verifiers.property", &verifier.property);
            require_non_empty(
                &mut report,
                "verifiers.evidence_uri",
                &verifier.evidence_uri,
            );
            validate_evidence_digest(
                &mut report,
                "verifiers.artifact_digest",
                &verifier.artifact_digest,
                &self.identity.artifact_digest,
            );

            if matches!(
                verifier.status,
                VerifierStatus::Failed | VerifierStatus::Revoked
            ) {
                report.push(PassportViolation::new(
                    PassportViolationCode::FailedVerifier,
                    "verifiers.status",
                    format!(
                        "verifier '{}' has status {:?}",
                        verifier.verifier, verifier.status
                    ),
                ));
            }
        }

        if self.lifecycle.state == LifecycleState::Stable {
            if !self.output.deterministic_serialization || !self.output.receipt_required {
                report.push(PassportViolation::new(
                    PassportViolationCode::MissingStandingEvidence,
                    "output",
                    "stable parts require deterministic serialization and receipts",
                ));
            }
            if self.conformity.is_empty() {
                report.push(PassportViolation::new(
                    PassportViolationCode::MissingStandingEvidence,
                    "conformity",
                    "stable parts require at least one conformity declaration",
                ));
            }
            if !self
                .verifiers
                .iter()
                .any(|mark| mark.status == VerifierStatus::Passed)
            {
                report.push(PassportViolation::new(
                    PassportViolationCode::MissingStandingEvidence,
                    "verifiers",
                    "stable parts require at least one passed independent verifier",
                ));
            }
        }

        validate_noninterference(&mut report, &self.noninterference);

        if Version::parse(&self.lifecycle.policy_version).is_err() {
            report.push(PassportViolation::new(
                PassportViolationCode::InvalidVersion,
                "lifecycle.policy_version",
                "lifecycle policy version must be valid semantic versioning",
            ));
        }
        require_non_empty(
            &mut report,
            "lifecycle.retirement.disposal_instructions",
            &self.lifecycle.retirement.disposal_instructions,
        );

        report
    }

    /// Derive compact symbols for a human nameplate. Marks are projections of
    /// machine-readable facts, not independently asserted booleans.
    #[must_use]
    pub fn nameplate_marks(&self) -> BTreeSet<NameplateMark> {
        let mut marks = BTreeSet::from([NameplateMark::ConditionedInput]);

        if self.output.deterministic_serialization {
            marks.insert(NameplateMark::DeterministicOutput);
        }
        if !self.resources.ambient_io {
            marks.insert(NameplateMark::NoAmbientAuthority);
        }
        if self.isolation == IsolationClass::DoubleInsulated {
            marks.insert(NameplateMark::DoubleInsulated);
        }
        if !self.noninterference.forbidden.is_empty()
            || !self.polarity.requires_authorities.is_empty()
        {
            marks.insert(NameplateMark::LimitedConsequence);
        }
        if self
            .verifiers
            .iter()
            .any(|mark| mark.status == VerifierStatus::Passed)
        {
            marks.insert(NameplateMark::IndependentVerification);
        }
        if validate_noninterference_conflicts(&self.noninterference).is_empty() {
            marks.insert(NameplateMark::NonInterference);
        }
        if self.lifecycle.retirement.preserve_receipts {
            marks.insert(NameplateMark::SeparateRetirement);
        }

        marks
    }

    /// Deterministic human-readable projection analogous to an adapter label.
    #[must_use]
    pub fn render_nameplate(&self) -> String {
        let marks = self
            .nameplate_marks()
            .into_iter()
            .map(|mark| mark.to_string())
            .collect::<Vec<_>>()
            .join(" ");
        let hosts = self
            .host_profiles
            .iter()
            .map(ToString::to_string)
            .collect::<Vec<_>>()
            .join(",");
        let conformity = self
            .conformity
            .iter()
            .map(|mark| mark.profile.as_str())
            .collect::<Vec<_>>()
            .join(",");
        let verifiers = self
            .verifiers
            .iter()
            .filter(|mark| mark.status == VerifierStatus::Passed)
            .map(|mark| mark.verifier.as_str())
            .collect::<Vec<_>>()
            .join(",");

        let mut label = String::new();
        writeln!(&mut label, "GGEN PART PASSPORT").expect("String writes cannot fail");
        writeln!(&mut label, "MODEL: {}", self.identity.part_id)
            .expect("String writes cannot fail");
        writeln!(&mut label, "FAMILY: {}", self.identity.family)
            .expect("String writes cannot fail");
        writeln!(&mut label, "VERSION: {}", self.identity.version)
            .expect("String writes cannot fail");
        writeln!(&mut label, "MFR: {}", self.identity.manufacturer)
            .expect("String writes cannot fail");
        writeln!(
            &mut label,
            "INPUT: {} {} {}..{} {}B MAX",
            self.input.representation,
            self.input.schema,
            self.input.protocol.minimum,
            self.input.protocol.maximum,
            self.input.max_payload_bytes
        )
        .expect("String writes cannot fail");
        writeln!(
            &mut label,
            "OUTPUT: {} {} {} x{} MAX",
            self.output.representation,
            self.output.schema,
            self.output.artifact_type,
            self.output.max_artifacts
        )
        .expect("String writes cannot fail");
        writeln!(
            &mut label,
            "RESOURCES: {} pages / {} fuel / {}ms / q{} / c{}",
            self.resources.max_memory_pages,
            self.resources.max_fuel,
            self.resources.max_execution_ms,
            self.resources.max_queue_depth,
            self.resources.max_concurrency
        )
        .expect("String writes cannot fail");
        writeln!(&mut label, "ISOLATION: {:?}", self.isolation).expect("String writes cannot fail");
        writeln!(&mut label, "HOSTS: {hosts}").expect("String writes cannot fail");
        writeln!(&mut label, "CONFORMITY: {conformity}").expect("String writes cannot fail");
        writeln!(&mut label, "VERIFIED: {verifiers}").expect("String writes cannot fail");
        writeln!(&mut label, "LIFECYCLE: {:?}", self.lifecycle.state)
            .expect("String writes cannot fail");
        writeln!(&mut label, "DIGEST: {}", self.identity.artifact_digest)
            .expect("String writes cannot fail");
        writeln!(&mut label, "MARKS: {marks}").expect("String writes cannot fail");
        label
    }

    /// Determine whether `self` can replace `required` without hand-fitting the
    /// host, meaning, authority, or evidence surface.
    #[must_use]
    pub fn can_substitute_for(&self, required: &Self) -> SubstitutionReport {
        let mut report = SubstitutionReport {
            compatible: true,
            violations: Vec::new(),
        };

        let candidate_validation = self.validate();
        if !candidate_validation.is_valid() {
            report.reject(
                SubstitutionViolationCode::InvalidCandidate,
                format!(
                    "candidate passport has {} validation violation(s)",
                    candidate_validation.violations.len()
                ),
            );
        }

        let required_validation = required.validate();
        if !required_validation.is_valid() {
            report.reject(
                SubstitutionViolationCode::InvalidRequiredProfile,
                format!(
                    "required passport has {} validation violation(s)",
                    required_validation.violations.len()
                ),
            );
        }

        compare_equal(
            &mut report,
            SubstitutionViolationCode::InputRepresentation,
            &self.input.representation,
            &required.input.representation,
            "input representation",
        );
        compare_equal(
            &mut report,
            SubstitutionViolationCode::InputSchema,
            &self.input.schema,
            &required.input.schema,
            "input schema",
        );

        if !self.input.protocol.covers(&required.input.protocol) {
            report.reject(
                SubstitutionViolationCode::ProtocolRange,
                "candidate protocol range does not cover required range",
            );
        }
        if self.input.max_payload_bytes < required.input.max_payload_bytes {
            report.reject(
                SubstitutionViolationCode::InputCapacity,
                "candidate input payload capacity is below required capacity",
            );
        }
        if !self
            .input
            .accepted_event_types
            .is_superset(&required.input.accepted_event_types)
        {
            report.reject(
                SubstitutionViolationCode::EventSurface,
                "candidate does not accept every required event type",
            );
        }
        if !self
            .input
            .required_fields
            .is_subset(&required.input.required_fields)
        {
            report.reject(
                SubstitutionViolationCode::RequiredFields,
                "candidate requires fields the existing socket does not guarantee",
            );
        }

        if self.output.representation != required.output.representation
            || self.output.schema != required.output.schema
            || self.output.artifact_type != required.output.artifact_type
            || (required.output.deterministic_serialization
                && !self.output.deterministic_serialization)
            || (required.output.receipt_required && !self.output.receipt_required)
        {
            report.reject(
                SubstitutionViolationCode::OutputContract,
                "candidate output contract does not preserve the required consequence",
            );
        }
        if self.output.max_artifacts < required.output.max_artifacts {
            report.reject(
                SubstitutionViolationCode::OutputCapacity,
                "candidate output capacity is below required capacity",
            );
        }
        if self.polarity != required.polarity {
            report.reject(
                SubstitutionViolationCode::CausalPolarity,
                "candidate reverses or changes causal polarity",
            );
        }

        if !resources_fit(&self.resources, &required.resources) {
            report.reject(
                SubstitutionViolationCode::ResourceEnvelope,
                "candidate exceeds one or more host resource ceilings",
            );
        }
        if self.resources.ambient_io && !required.resources.ambient_io {
            report.reject(
                SubstitutionViolationCode::AmbientAuthority,
                "candidate requires ambient IO that the socket does not grant",
            );
        }
        if self.isolation < required.isolation {
            report.reject(
                SubstitutionViolationCode::IsolationClass,
                "candidate isolation class is weaker than required",
            );
        }
        if !self.host_profiles.is_superset(&required.host_profiles) {
            report.reject(
                SubstitutionViolationCode::HostProfile,
                "candidate omits one or more required host profiles",
            );
        }
        if !self
            .jurisdiction_profiles
            .is_superset(&required.jurisdiction_profiles)
        {
            report.reject(
                SubstitutionViolationCode::JurisdictionProfile,
                "candidate omits one or more required jurisdiction profiles",
            );
        }

        let candidate_profiles = self
            .conformity
            .iter()
            .map(|mark| mark.profile.as_str())
            .collect::<BTreeSet<_>>();
        let required_profiles = required
            .conformity
            .iter()
            .map(|mark| mark.profile.as_str())
            .collect::<BTreeSet<_>>();
        if !candidate_profiles.is_superset(&required_profiles) {
            report.reject(
                SubstitutionViolationCode::ConformityProfile,
                "candidate lacks one or more required conformity profiles",
            );
        }

        if !noninterference_is_substitutable(&self.noninterference, &required.noninterference) {
            report.reject(
                SubstitutionViolationCode::NonInterference,
                "candidate widens the admitted side-effect surface",
            );
        }

        if !self.lifecycle.state.allows_new_installation() {
            report.reject(
                SubstitutionViolationCode::Lifecycle,
                "candidate lifecycle does not permit new installation",
            );
        }

        report
    }
}

fn require_non_empty(report: &mut PassportValidationReport, field: &str, value: &str) {
    if value.trim().is_empty() {
        report.push(PassportViolation::new(
            PassportViolationCode::MissingField,
            field,
            "value cannot be empty",
        ));
    }
}

fn require_non_empty_set(
    report: &mut PassportValidationReport, field: &str, value: &BTreeSet<String>,
) {
    if value.is_empty() {
        report.push(PassportViolation::new(
            PassportViolationCode::MissingField,
            field,
            "at least one value is required",
        ));
    } else if value.iter().any(|entry| entry.trim().is_empty()) {
        report.push(PassportViolation::new(
            PassportViolationCode::MissingField,
            field,
            "set entries cannot be empty",
        ));
    }
}

fn require_positive(report: &mut PassportValidationReport, field: &str, value: u64) {
    if value == 0 {
        report.push(PassportViolation::new(
            PassportViolationCode::InvalidBound,
            field,
            "bound must be greater than zero",
        ));
    }
}

fn validate_digest(report: &mut PassportValidationReport, field: &str, value: &str) {
    if !is_supported_digest(value) {
        report.push(PassportViolation::new(
            PassportViolationCode::InvalidDigest,
            field,
            "digest must be blake3:<64 lowercase hex> or sha256:<64 lowercase hex>",
        ));
    }
}

fn is_supported_digest(value: &str) -> bool {
    let Some((algorithm, digest)) = value.split_once(':') else {
        return false;
    };
    matches!(algorithm, "blake3" | "sha256")
        && digest.len() == 64
        && digest
            .bytes()
            .all(|byte| byte.is_ascii_digit() || (b'a'..=b'f').contains(&byte))
}

fn validate_evidence_digest(
    report: &mut PassportValidationReport, field: &str, evidence_digest: &str,
    artifact_digest: &str,
) {
    validate_digest(report, field, evidence_digest);
    if evidence_digest != artifact_digest {
        report.push(PassportViolation::new(
            PassportViolationCode::DigestMismatch,
            field,
            "evidence mark is not bound to the passport artifact digest",
        ));
    }
}

fn validate_noninterference(
    report: &mut PassportValidationReport, profile: &NonInterferenceProfile,
) {
    for conflict in validate_noninterference_conflicts(profile) {
        report.push(PassportViolation::new(
            PassportViolationCode::NonInterferenceConflict,
            "noninterference",
            conflict,
        ));
    }
}

fn validate_noninterference_conflicts(profile: &NonInterferenceProfile) -> Vec<String> {
    let mut conflicts = Vec::new();
    for resource in profile.may_write.intersection(&profile.forbidden) {
        conflicts.push(format!(
            "resource '{resource}' is both writable and forbidden"
        ));
    }
    for event in profile.may_emit.intersection(&profile.forbidden) {
        conflicts.push(format!("event '{event}' is both emitted and forbidden"));
    }
    conflicts
}

fn compare_equal<T: Eq + fmt::Debug>(
    report: &mut SubstitutionReport, code: SubstitutionViolationCode, candidate: &T, required: &T,
    label: &str,
) {
    if candidate != required {
        report.reject(
            code,
            format!(
                "candidate {label} {:?} differs from required {:?}",
                candidate, required
            ),
        );
    }
}

fn resources_fit(candidate: &ResourceEnvelope, required: &ResourceEnvelope) -> bool {
    candidate.max_memory_pages <= required.max_memory_pages
        && candidate.max_fuel <= required.max_fuel
        && candidate.max_execution_ms <= required.max_execution_ms
        && candidate.max_queue_depth <= required.max_queue_depth
        && candidate.max_concurrency <= required.max_concurrency
}

fn noninterference_is_substitutable(
    candidate: &NonInterferenceProfile, required: &NonInterferenceProfile,
) -> bool {
    candidate.may_read.is_subset(&required.may_read)
        && candidate.may_write.is_subset(&required.may_write)
        && candidate.may_emit.is_subset(&required.may_emit)
        && candidate.forbidden.is_superset(&required.forbidden)
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::marketplace::models::PackageMetadata;
    use indexmap::IndexMap;

    fn set(values: &[&str]) -> BTreeSet<String> {
        values.iter().map(|value| (*value).to_string()).collect()
    }

    fn digest() -> String {
        format!("blake3:{}", "a".repeat(64))
    }

    fn sample_passport() -> PartPassport {
        let artifact_digest = digest();
        PartPassport {
            schema_version: CURRENT_PASSPORT_SCHEMA,
            identity: PartIdentity {
                part_id: "crown-kernel".to_string(),
                family: "pc-powl2".to_string(),
                version: "26.7.57".to_string(),
                manufacturer: "ggen-foundry".to_string(),
                artifact_digest: artifact_digest.clone(),
                source_graph_digest: format!("sha256:{}", "b".repeat(64)),
                build_receipt: "urn:ggen:receipt:crown-kernel:26.7.57".to_string(),
                batch_id: Some("release-26.7.57".to_string()),
            },
            input: InputEnvelope {
                representation: "application/ld+json".to_string(),
                schema: "urn:ggen:schema:admitted-trace-event:v1".to_string(),
                protocol: ProtocolRange {
                    minimum: "1.0.0".to_string(),
                    maximum: "1.2.0".to_string(),
                },
                max_payload_bytes: 65_536,
                accepted_event_types: set(&["trace.observed", "trace.replay"]),
                required_fields: set(&[
                    "event_id",
                    "aggregate_id",
                    "sequence",
                    "authority_receipt",
                ]),
                temporal: TemporalProfile {
                    clock: ClockDiscipline::Logical,
                    max_events_per_second: 10_000,
                    timeout_semantics: TimeoutSemantics::ExplicitEvent,
                    wall_clock_access: false,
                },
            },
            output: OutputContract {
                representation: "application/json".to_string(),
                schema: "urn:ggen:schema:crown-receipt:v1".to_string(),
                artifact_type: "CrownReceipt".to_string(),
                deterministic_serialization: true,
                receipt_required: true,
                max_artifacts: 1,
            },
            polarity: CausalPolarity {
                consumes: set(&["AdmittedTraceEvent"]),
                produces: set(&["CrownReceipt"]),
                requires_authorities: set(&["read:admitted-state"]),
                emits_intents: set(&["emit:crown-receipt"]),
            },
            resources: ResourceEnvelope {
                max_memory_pages: 64,
                max_fuel: 5_000_000,
                max_execution_ms: 20,
                max_queue_depth: 1_024,
                max_concurrency: 1,
                ambient_io: false,
            },
            isolation: IsolationClass::DoubleInsulated,
            host_profiles: [HostProfile::new("server"), HostProfile::new("ci")]
                .into_iter()
                .collect(),
            jurisdiction_profiles: set(&["internal-production"]),
            conformity: vec![ConformityMark {
                profile: "mfw-core-v1".to_string(),
                issuer: "ggen-foundry".to_string(),
                evidence_uri: "urn:ggen:evidence:conformity:crown-kernel".to_string(),
                artifact_digest: artifact_digest.clone(),
                self_declared: true,
            }],
            verifiers: vec![VerifierMark {
                verifier: "lean-kernel".to_string(),
                property: "trace-kernel-equivalence".to_string(),
                evidence_uri: "urn:ggen:evidence:lean:crown-kernel".to_string(),
                artifact_digest,
                status: VerifierStatus::Passed,
            }],
            noninterference: NonInterferenceProfile {
                may_read: set(&["admitted-state"]),
                may_write: BTreeSet::new(),
                may_emit: set(&["crown-receipt"]),
                forbidden: set(&["filesystem", "network", "wall-clock"]),
            },
            lifecycle: LifecyclePolicy {
                state: LifecycleState::Stable,
                policy_version: "1.0.0".to_string(),
                retirement: RetirementPolicy {
                    revoke_credentials: true,
                    preserve_receipts: true,
                    migration_target: None,
                    disposal_instructions:
                        "revoke capability grants; preserve receipts and replay fixtures"
                            .to_string(),
                },
            },
        }
    }

    #[test]
    fn valid_stable_passport_has_standing() {
        let passport = sample_passport();
        let report = passport.validate();
        assert!(report.is_valid(), "{:#?}", report.violations);
    }

    #[test]
    fn double_insulation_refuses_ambient_io() {
        let mut passport = sample_passport();
        passport.resources.ambient_io = true;
        let report = passport.validate();
        assert!(report.violations.iter().any(|violation| {
            violation.code == PassportViolationCode::AmbientAuthorityViolation
        }));
    }

    #[test]
    fn lower_draw_candidate_is_substitutable() {
        let required = sample_passport();
        let mut candidate = required.clone();
        candidate.identity.artifact_digest = format!("blake3:{}", "c".repeat(64));
        for mark in &mut candidate.conformity {
            mark.artifact_digest
                .clone_from(&candidate.identity.artifact_digest);
        }
        for mark in &mut candidate.verifiers {
            mark.artifact_digest
                .clone_from(&candidate.identity.artifact_digest);
        }
        candidate.resources.max_memory_pages = 32;
        candidate.resources.max_fuel = 4_000_000;
        candidate.resources.max_execution_ms = 10;

        let report = candidate.can_substitute_for(&required);
        assert!(report.compatible, "{:#?}", report.violations);
    }

    #[test]
    fn changed_polarity_is_not_substitutable() {
        let required = sample_passport();
        let mut candidate = required.clone();
        candidate
            .polarity
            .emits_intents
            .insert("deploy:production".to_string());

        let report = candidate.can_substitute_for(&required);
        assert!(!report.compatible);
        assert!(report
            .violations
            .iter()
            .any(|violation| violation.code == SubstitutionViolationCode::CausalPolarity));
    }

    #[test]
    fn nameplate_is_derived_from_machine_facts() {
        let label = sample_passport().render_nameplate();
        assert!(label.contains("MODEL: crown-kernel"));
        assert!(label.contains("ISOLATION: DoubleInsulated"));
        assert!(label.contains("MARKS: O* D NAA II LCS IV NI RET"));
    }

    #[test]
    fn passport_round_trips_as_json() -> Result<(), Box<dyn std::error::Error>> {
        let passport = sample_passport();
        let json = serde_json::to_string_pretty(&passport)?;
        let decoded: PartPassport = serde_json::from_str(&json)?;
        assert_eq!(decoded, passport);
        Ok(())
    }

    #[test]
    fn binding_refuses_manifest_identity_mismatch() -> Result<(), Box<dyn std::error::Error>> {
        let id = PackageId::new("different-part")?;
        let version = PackageVersion::new("26.7.57")?;
        let mut metadata = PackageMetadata::new(
            id.clone(),
            "Different Part",
            "Fixture for passport binding",
            "MIT",
        );
        metadata.authors.push("ggen-foundry".to_string());
        let manifest = Manifest {
            id,
            version,
            metadata,
            dependencies: Vec::new(),
            features: IndexMap::new(),
        };

        let binding = PassportBinding::from_manifest(&manifest, sample_passport());
        let report = binding.validate();
        assert!(report
            .violations
            .iter()
            .any(|violation| violation.code == PassportViolationCode::IdentityMismatch));
        Ok(())
    }
}
