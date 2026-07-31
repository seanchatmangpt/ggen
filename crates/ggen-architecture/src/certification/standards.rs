use std::collections::BTreeSet;

use serde::{Deserialize, Serialize};
use thiserror::Error;

use super::{
    digest, testing_bblock_protocol, TestingBblockProtocol, TestingBblockRefusal,
};

/// Stable identity of the standards admitted from the seven-day ggen programme.
pub const GGEN_SEVEN_DAY_STANDARDS_ID: &str = "GGEN-STANDARDS-7D-2026-07-30";
/// Semantic version of the admitted standards profile.
pub const GGEN_SEVEN_DAY_STANDARDS_VERSION: &str = "26.7.31";
const EXPECTED_CHECKPOINTS: usize = 19;

/// Authority layer responsible for one standard.
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Serialize, Deserialize)]
#[serde(rename_all = "snake_case")]
pub enum StandardAuthority {
    Constitutional,
    Semantic,
    Manufacturing,
    Planning,
    Execution,
    Evidence,
    Verification,
    Delivery,
    Interaction,
}

/// Admission status of one standard in the bounded certification profile.
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Serialize, Deserialize)]
#[serde(rename_all = "SCREAMING_SNAKE_CASE")]
pub enum StandardStatus {
    /// Enforced by merged or exact-head executable repository law.
    Admitted,
    /// Preserved as an inherited executable rail with its own receipt boundary.
    Inherited,
    /// Explicit future Gall checkpoint; it cannot contribute to ALIVE standing.
    PendingCheckpoint,
}

/// One named constitutional, semantic, manufacturing, or verification checkpoint.
#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
pub struct StandardCheckpoint {
    pub id: String,
    pub authority: StandardAuthority,
    pub status: StandardStatus,
    pub law: String,
    pub falsifier: String,
}

/// Exact standards profile bound into every TAI certification receipt.
#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
pub struct StandardsProfile {
    pub id: String,
    pub version: String,
    pub observation_window: String,
    pub governing_equation: String,
    pub root_broker: String,
    pub standing_lattice: Vec<String>,
    pub testing_bblock: TestingBblockProtocol,
    pub checkpoints: Vec<StandardCheckpoint>,
    pub exclusions: Vec<String>,
}

impl StandardsProfile {
    /// Validate identity, cardinality, unique checkpoint ownership, and the
    /// non-promoting boundary for pending work.
    pub fn validate(&self) -> Result<(), StandardsProfileRefusal> {
        if self.id != GGEN_SEVEN_DAY_STANDARDS_ID {
            return Err(StandardsProfileRefusal::IdentityMismatch(self.id.clone()));
        }
        if self.version != GGEN_SEVEN_DAY_STANDARDS_VERSION {
            return Err(StandardsProfileRefusal::VersionMismatch(
                self.version.clone(),
            ));
        }
        if self.root_broker != "BRCE" {
            return Err(StandardsProfileRefusal::BrokerMismatch(
                self.root_broker.clone(),
            ));
        }
        self.testing_bblock.validate()?;
        if self.checkpoints.len() != EXPECTED_CHECKPOINTS {
            return Err(StandardsProfileRefusal::CardinalityMismatch {
                expected: EXPECTED_CHECKPOINTS,
                observed: self.checkpoints.len(),
            });
        }
        let mut identities = BTreeSet::new();
        for checkpoint in &self.checkpoints {
            if checkpoint.id.trim().is_empty()
                || checkpoint.law.trim().is_empty()
                || checkpoint.falsifier.trim().is_empty()
            {
                return Err(StandardsProfileRefusal::IncompleteCheckpoint(
                    checkpoint.id.clone(),
                ));
            }
            if !identities.insert(checkpoint.id.clone()) {
                return Err(StandardsProfileRefusal::DuplicateCheckpoint(
                    checkpoint.id.clone(),
                ));
            }
        }
        let required_standing = [
            "PARTIAL_ALIVE",
            "ALIVE",
            "BLOCKED",
            "BUILD_BROKEN",
            "UNKNOWN",
            "UNSUPPORTED",
        ];
        if self.standing_lattice != required_standing.map(str::to_string) {
            return Err(StandardsProfileRefusal::StandingLatticeMismatch);
        }
        Ok(())
    }

    /// Return a canonical BLAKE3 identity after validating the profile.
    pub fn digest(&self) -> Result<String, StandardsProfileRefusal> {
        self.validate()?;
        digest(self).map_err(|error| StandardsProfileRefusal::Serialization(error.to_string()))
    }

    /// Only admitted or inherited checkpoints may contribute to a bounded
    /// certification claim. Pending checkpoints remain visible but non-promoting.
    #[must_use]
    pub fn promoting_checkpoints(&self) -> Vec<&StandardCheckpoint> {
        self.checkpoints
            .iter()
            .filter(|checkpoint| checkpoint.status != StandardStatus::PendingCheckpoint)
            .collect()
    }
}

/// Return the canonical standards profile developed across the seven-day ggen
/// programme and consumed by the TAI certification case.
#[must_use]
pub fn seven_day_standards_profile() -> StandardsProfile {
    let rows = [
        (
            "STD-01-ADMITTED-OBSERVATION",
            StandardAuthority::Constitutional,
            StandardStatus::Admitted,
            "A = μ(O*); only admitted, aligned, complete, grounded, and bounded observation enters manufacture.",
            "Manufacture from partial, stale, unbounded, or unaudited observation.",
        ),
        (
            "STD-02-RECEIPTED-CONSEQUENCE",
            StandardAuthority::Constitutional,
            StandardStatus::Admitted,
            "R = receipt(A); zero unreceipted actuation and command success is not consequence success.",
            "Promote from exit status without re-observed consequence and receipt.",
        ),
        (
            "STD-03-STATE-AUTHORITY-SEPARATION",
            StandardAuthority::Constitutional,
            StandardStatus::Admitted,
            "candidate != verified != authorized != actuated; planning and cognition never grant their own authority.",
            "Collapse a proposal, model response, plan, or generated file into execution authority.",
        ),
        (
            "STD-04-BRCE-ONLY-DO",
            StandardAuthority::Execution,
            StandardStatus::Admitted,
            "BRCE is the sole lawful external DO path; kernels and hooks manufacture bounded intents only.",
            "Perform filesystem, process, network, cloud, deployment, or production actuation from a pure kernel.",
        ),
        (
            "STD-05-STANDING-LATTICE",
            StandardAuthority::Verification,
            StandardStatus::Admitted,
            "Standing is typed and evidence-derived; UNKNOWN is not admitted and retirement is lifecycle, not standing.",
            "Infer ALIVE from metadata, lifecycle, documentation, an external certificate, or an incomplete crown.",
        ),
        (
            "STD-06-ONTOLOGY-AUTHORITY",
            StandardAuthority::Semantic,
            StandardStatus::Admitted,
            "Public-ontology-first graphs own domain meaning; generated artifacts, prose, and trackers are projections or observations.",
            "Hand-maintain generated consequences or introduce a competing private semantic authority.",
        ),
        (
            "STD-07-GGEN-MANUFACTURE",
            StandardAuthority::Manufacturing,
            StandardStatus::Admitted,
            "ggen follows Resolve → Enrich → Extract → Render → Write → Receipt with exact output ownership and bounded projection cardinality.",
            "Write an undeclared target, exceed admitted resource ceilings, or allow duplicate canonical writers.",
        ),
        (
            "STD-08-DETERMINISM-REPLAY",
            StandardAuthority::Manufacturing,
            StandardStatus::Admitted,
            "Repeated manufacture from the same admitted graph is byte-identical and independently replayable.",
            "Accept drift between first and second sync or a receipt that cannot be recomputed independently.",
        ),
        (
            "STD-09-PART-PASSPORT",
            StandardAuthority::Semantic,
            StandardStatus::Inherited,
            "Every interchangeable realization carries a passport binding meaning, ports, authority, resources, isolation, evidence, support, replacement, and retirement.",
            "Approve substitution from ABI compatibility alone or permit authority/resource expansion.",
        ),
        (
            "STD-10-GALL-CHECKPOINTS",
            StandardAuthority::Delivery,
            StandardStatus::Admitted,
            "Complex systems advance through dependency-closed Gall checkpoints with explicit obligations, exclusions, falsifiers, receipts, and replay.",
            "Claim crown completeness from a partial checkpoint or delete a Chesterton fence without replacement evidence.",
        ),
        (
            "STD-11-EVIDENCE-FIVE-SURFACE",
            StandardAuthority::Evidence,
            StandardStatus::Admitted,
            "Every certification obligation requires a positive witness, negative falsifier, independent verifier, receipt verifier, and replay.",
            "Promote with any proof surface absent, empty, self-produced, or detached from the exact artifact.",
        ),
        (
            "STD-12-REAL-BOUNDARY-TESTING",
            StandardAuthority::Verification,
            StandardStatus::Admitted,
            "Chicago-style tests cross real process, filesystem, state, protocol, or telemetry boundaries and externalize causal evidence.",
            "Use mocks, stubs, fabricated telemetry, synthetic receipts, or hard-coded success as primary evidence.",
        ),
        (
            "STD-13-EXACT-HEAD-EXECUTION",
            StandardAuthority::Verification,
            StandardStatus::Admitted,
            "Execution claims bind the exact commit/tree, toolchain, policy, inputs, outputs, and verifier report.",
            "Reuse a receipt from an earlier head or classify an action_required shell as an executed verifier.",
        ),
        (
            "STD-14-PROCESS-EVIDENCE",
            StandardAuthority::Evidence,
            StandardStatus::Inherited,
            "Execution emits causally linked OCEL/process evidence, predecessor receipts, consequence observation, and deterministic replay.",
            "Hand-construct process evidence or infer causality from timestamps or final-state coincidence alone.",
        ),
        (
            "STD-15-POWL-MFW-BOUNDARY",
            StandardAuthority::Planning,
            StandardStatus::Inherited,
            "POWL v2 and MFW own admitted planning and lifecycle semantics; ggen projects them and BRCE alone actuates them.",
            "Let a planner, serializer, or generated Motion Packet self-authorize execution.",
        ),
        (
            "STD-16-FORMAL-PROOF-DISCIPLINE",
            StandardAuthority::Verification,
            StandardStatus::Inherited,
            "Proof-relevant Lean values inhabit Type and are definitions; theorem interfaces must be non-circular, non-vacuous, and observation-complete.",
            "Declare Type-valued manufacturers as theorems, assume the conclusion, admit an empty relation, or erase trajectory/timestamp identity.",
        ),
        (
            "STD-17-AUTOMATIC-AUTONOMIC-BOUNDS",
            StandardAuthority::Execution,
            StandardStatus::Admitted,
            "Automatic routing and MAPE-K repair are finite, idempotent, budgeted, circuit-broken, Andon-governed, and receipted through the same authority rail.",
            "Add an emergency repair, retry, rollback, or promotion path that bypasses admission, budgets, receipts, or consequence verification.",
        ),
        (
            "STD-18-CI-CONTROL-PLANE-G0",
            StandardAuthority::Delivery,
            StandardStatus::PendingCheckpoint,
            "CI G0 inventories every workflow, names one semantic owner per production output, measures trigger fan-out, and preserves retirement fences before consolidation.",
            "Delete or consolidate workflows before exact inventory, ownership, and missing-owner/duplicate-authority falsifiers execute.",
        ),
        (
            "STD-19-TESTING-BBLOCK",
            StandardAuthority::Verification,
            StandardStatus::PendingCheckpoint,
            "Testing is a canonical Building Block with ten distinct executable suites: protocol/unit, property/fuzz, stdio plus HTTP integration, black-box CLI E2E, security, chaos, stress, benchmark, replay, and machine-readable verifier report.",
            "Collapse the suites into one undifferentiated command or claim ALIVE while any declared suite remains a pending checkpoint.",
        ),
    ];
    StandardsProfile {
        id: GGEN_SEVEN_DAY_STANDARDS_ID.to_string(),
        version: GGEN_SEVEN_DAY_STANDARDS_VERSION.to_string(),
        observation_window: "2026-07-23/2026-07-30".to_string(),
        governing_equation: "O → O* → I → G → A → R → O′".to_string(),
        root_broker: "BRCE".to_string(),
        standing_lattice: [
            "PARTIAL_ALIVE",
            "ALIVE",
            "BLOCKED",
            "BUILD_BROKEN",
            "UNKNOWN",
            "UNSUPPORTED",
        ]
        .into_iter()
        .map(str::to_string)
        .collect(),
        testing_bblock: testing_bblock_protocol(),
        checkpoints: rows
            .into_iter()
            .map(|(id, authority, status, law, falsifier)| StandardCheckpoint {
                id: id.to_string(),
                authority,
                status,
                law: law.to_string(),
                falsifier: falsifier.to_string(),
            })
            .collect(),
        exclusions: [
            "no direct actuation outside BRCE",
            "no generated artifact above graph authority",
            "no synthetic evidence promoted as production standing",
            "no pending checkpoint contributing to ALIVE",
            "no weighted average hiding a missing conjunctive capability",
            "no collapsed testing suite hiding an unexecuted boundary",
        ]
        .into_iter()
        .map(str::to_string)
        .collect(),
    }
}

#[derive(Debug, Error)]
pub enum StandardsProfileRefusal {
    #[error("seven-day standards identity mismatch: `{0}`")]
    IdentityMismatch(String),
    #[error("seven-day standards version mismatch: `{0}`")]
    VersionMismatch(String),
    #[error("seven-day standards broker mismatch: `{0}`")]
    BrokerMismatch(String),
    #[error("seven-day standards cardinality mismatch: expected {expected}, observed {observed}")]
    CardinalityMismatch { expected: usize, observed: usize },
    #[error("seven-day standards checkpoint is incomplete: `{0}`")]
    IncompleteCheckpoint(String),
    #[error("seven-day standards checkpoint is duplicated: `{0}`")]
    DuplicateCheckpoint(String),
    #[error("seven-day standards standing lattice is not canonical")]
    StandingLatticeMismatch,
    #[error(transparent)]
    TestingBblock(#[from] TestingBblockRefusal),
    #[error("seven-day standards serialization failed: {0}")]
    Serialization(String),
}
