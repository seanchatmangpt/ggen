//! Evidence admission, full-matrix assessment, and assessment receipts.

use crate::rwr::matrix::{
    contract, Dimension, EvidenceSurface, MaturityLevel, ALL_DIMENSIONS, MATRIX_VERSION,
};
use serde::{Deserialize, Serialize};
use std::collections::{BTreeMap, BTreeSet, HashSet};

/// Gall standing emitted by the maturity assessor.
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash, Serialize, Deserialize)]
pub enum GallState {
    /// Some real execution exists, but the bounded crown is open.
    PartialAlive,
    /// Every required dimension has observed Level-5 execution.
    Alive,
    /// A falsifier or policy failure blocks promotion.
    Blocked,
    /// The executable verifier failed to build.
    BuildBroken,
    /// Required evidence is absent or inconclusive.
    Unknown,
    /// The capability is outside the admitted boundary.
    Unsupported,
}

/// Outcome observed on one evidence surface.
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash, Serialize, Deserialize)]
pub enum EvidenceOutcome {
    /// The real verifier passed.
    Pass,
    /// The real verifier falsified the claim.
    Fail,
    /// The verifier could not build.
    BuildFailed,
    /// No observation was obtained.
    Unknown,
    /// The source cannot prove the surface within its boundary.
    Unsupported,
}

/// One immutable admitted observation.
#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
pub struct EvidenceRecord {
    /// Stable evidence identity.
    pub id: String,
    /// Matrix dimension supported or falsified.
    pub dimension: Dimension,
    /// Highest maturity level directly exercised.
    pub level: MaturityLevel,
    /// Independent proof surface.
    pub surface: EvidenceSurface,
    /// Observed result.
    pub outcome: EvidenceOutcome,
    /// Human- and machine-locatable evidence source.
    pub source: String,
    /// Monotonic observation epoch.
    pub epoch: u64,
    /// BLAKE3 of the externalized evidence bytes.
    pub artifact_digest: [u8; 32],
    /// BLAKE3 binding every record field.
    pub observation_digest: [u8; 32],
}

impl EvidenceRecord {
    /// Construct a record from bytes actually observed at the evidence boundary.
    #[must_use]
    #[allow(clippy::too_many_arguments)]
    pub fn observed(
        id: impl Into<String>, dimension: Dimension, level: MaturityLevel,
        surface: EvidenceSurface, outcome: EvidenceOutcome, source: impl Into<String>, epoch: u64,
        artifact: &[u8],
    ) -> Self {
        let id = id.into();
        let source = source.into();
        let artifact_digest = blake3::hash(artifact).into();
        let observation_digest = observation_digest(
            &id,
            dimension,
            level,
            surface,
            outcome,
            &source,
            epoch,
            &artifact_digest,
        );
        Self {
            id,
            dimension,
            level,
            surface,
            outcome,
            source,
            epoch,
            artifact_digest,
            observation_digest,
        }
    }

    /// Verify the cryptographic binding of this record.
    #[must_use]
    pub fn verify(&self) -> bool {
        self.observation_digest
            == observation_digest(
                &self.id,
                self.dimension,
                self.level,
                self.surface,
                self.outcome,
                &self.source,
                self.epoch,
                &self.artifact_digest,
            )
    }
}

fn put(hasher: &mut blake3::Hasher, bytes: &[u8]) {
    hasher.update(&(bytes.len() as u64).to_le_bytes());
    hasher.update(bytes);
}

#[allow(clippy::too_many_arguments)]
fn observation_digest(
    id: &str, dimension: Dimension, level: MaturityLevel, surface: EvidenceSurface,
    outcome: EvidenceOutcome, source: &str, epoch: u64, artifact_digest: &[u8; 32],
) -> [u8; 32] {
    let mut hasher = blake3::Hasher::new();
    put(&mut hasher, b"rwr-evidence/v1");
    put(&mut hasher, id.as_bytes());
    hasher.update(&[dimension as u8, level as u8, surface as u8, outcome as u8]);
    put(&mut hasher, source.as_bytes());
    hasher.update(&epoch.to_le_bytes());
    hasher.update(artifact_digest);
    hasher.finalize().into()
}

/// Evidence-ledger and receipt failure.
#[derive(Debug, thiserror::Error, PartialEq, Eq)]
pub enum EvidenceError {
    /// Evidence identities are immutable and unique.
    #[error("duplicate evidence id refused: {0}")]
    DuplicateId(String),
    /// The supplied record does not verify.
    #[error("evidence record digest mismatch: {0}")]
    DigestMismatch(String),
    /// Assessment serialization failed.
    #[error("assessment serialization failed: {0}")]
    Serialization(String),
    /// Assessment receipt verification failed.
    #[error("assessment receipt digest mismatch")]
    ReceiptMismatch,
}

/// Append-only evidence history.
#[derive(Debug, Clone, Default, PartialEq, Eq, Serialize, Deserialize)]
pub struct EvidenceLedger {
    records: Vec<EvidenceRecord>,
}

impl EvidenceLedger {
    /// Create an empty ledger.
    #[must_use]
    pub const fn new() -> Self {
        Self {
            records: Vec::new(),
        }
    }

    /// Admit one immutable cryptographically valid observation.
    pub fn admit(&mut self, record: EvidenceRecord) -> Result<(), EvidenceError> {
        if !record.verify() {
            return Err(EvidenceError::DigestMismatch(record.id));
        }
        if self.records.iter().any(|existing| existing.id == record.id) {
            return Err(EvidenceError::DuplicateId(record.id));
        }
        self.records.push(record);
        Ok(())
    }

    /// Read the complete history.
    #[must_use]
    pub fn records(&self) -> &[EvidenceRecord] {
        &self.records
    }

    /// Return the next monotonic epoch.
    #[must_use]
    pub fn next_epoch(&self) -> u64 {
        self.records
            .iter()
            .map(|record| record.epoch)
            .max()
            .unwrap_or(0)
            .saturating_add(1)
    }

    /// Cryptographic root over the complete immutable history.
    #[must_use]
    pub fn root_digest(&self) -> [u8; 32] {
        let mut digests: Vec<[u8; 32]> = self
            .records
            .iter()
            .map(|record| record.observation_digest)
            .collect();
        digests.sort_unstable();
        let mut hasher = blake3::Hasher::new();
        put(&mut hasher, b"rwr-evidence-ledger/v1");
        for digest in digests {
            hasher.update(&digest);
        }
        hasher.finalize().into()
    }

    fn latest(&self, dimension: Dimension) -> BTreeMap<EvidenceSurface, &EvidenceRecord> {
        let mut latest: BTreeMap<EvidenceSurface, &EvidenceRecord> = BTreeMap::new();
        for record in self
            .records
            .iter()
            .filter(|record| record.dimension == dimension)
        {
            let replace = match latest.get(&record.surface) {
                Some(current) => {
                    record.epoch > current.epoch
                        || (record.epoch == current.epoch && record.id > current.id)
                }
                None => true,
            };
            if replace {
                latest.insert(record.surface, record);
            }
        }
        latest
    }

    /// Assess all dimensions against the full Level-5 contract.
    #[must_use]
    pub fn assess(&self) -> MaturityAssessment {
        let dimensions: Vec<DimensionAssessment> = ALL_DIMENSIONS
            .iter()
            .copied()
            .map(|dimension| self.assess_dimension(dimension))
            .collect();
        MaturityAssessment {
            matrix_version: MATRIX_VERSION.to_string(),
            evidence_root: self.root_digest(),
            standing: overall_standing(&dimensions),
            dimensions,
        }
    }

    fn assess_dimension(&self, dimension: Dimension) -> DimensionAssessment {
        let required = contract(dimension).required_surfaces;
        let latest = self.latest(dimension);
        let mut satisfied = Vec::new();
        let mut missing = Vec::new();
        let mut blocking = Vec::new();
        for surface in required {
            match latest.get(surface) {
                Some(record)
                    if record.outcome == EvidenceOutcome::Pass
                        && record.level >= MaturityLevel::DigitalEcosystem =>
                {
                    satisfied.push(*surface);
                }
                Some(record) => {
                    missing.push(*surface);
                    if record.outcome != EvidenceOutcome::Pass {
                        blocking.push(record.id.clone());
                    }
                }
                None => missing.push(*surface),
            }
        }
        let attained_level = attained_level(required, &latest);
        let standing = dimension_standing(required, &latest, attained_level);
        DimensionAssessment {
            dimension,
            attained_level,
            standing,
            satisfied_surfaces: satisfied,
            missing_surfaces: missing,
            blocking_evidence: blocking,
        }
    }
}

fn attained_level(
    required: &[EvidenceSurface], latest: &BTreeMap<EvidenceSurface, &EvidenceRecord>,
) -> Option<MaturityLevel> {
    const LEVELS: [MaturityLevel; 5] = [
        MaturityLevel::DigitalEcosystem,
        MaturityLevel::BusinessModularity,
        MaturityLevel::OptimizedCore,
        MaturityLevel::StandardizedTechnology,
        MaturityLevel::BusinessSilos,
    ];
    LEVELS.into_iter().find(|candidate| {
        required.iter().all(|surface| {
            latest.get(surface).is_some_and(|record| {
                record.outcome == EvidenceOutcome::Pass && record.level >= *candidate
            })
        })
    })
}

fn dimension_standing(
    required: &[EvidenceSurface], latest: &BTreeMap<EvidenceSurface, &EvidenceRecord>,
    attained_level: Option<MaturityLevel>,
) -> GallState {
    let outcomes: HashSet<EvidenceOutcome> = required
        .iter()
        .filter_map(|surface| latest.get(surface).map(|record| record.outcome))
        .collect();
    if outcomes.contains(&EvidenceOutcome::BuildFailed) {
        GallState::BuildBroken
    } else if outcomes.contains(&EvidenceOutcome::Fail) {
        GallState::Blocked
    } else if outcomes.contains(&EvidenceOutcome::Unsupported) {
        GallState::Unsupported
    } else if attained_level == Some(MaturityLevel::DigitalEcosystem) {
        GallState::Alive
    } else if outcomes.contains(&EvidenceOutcome::Pass) {
        GallState::PartialAlive
    } else {
        GallState::Unknown
    }
}

fn overall_standing(dimensions: &[DimensionAssessment]) -> GallState {
    if dimensions
        .iter()
        .all(|dimension| dimension.standing == GallState::Alive)
    {
        return GallState::Alive;
    }
    for candidate in [
        GallState::BuildBroken,
        GallState::Blocked,
        GallState::Unsupported,
        GallState::Unknown,
        GallState::PartialAlive,
    ] {
        if dimensions
            .iter()
            .any(|dimension| dimension.standing == candidate)
        {
            return candidate;
        }
    }
    GallState::Unknown
}

/// Current standing of one matrix dimension.
#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
pub struct DimensionAssessment {
    /// Assessed dimension.
    pub dimension: Dimension,
    /// Highest level with every required surface passing.
    pub attained_level: Option<MaturityLevel>,
    /// Gall standing for the dimension.
    pub standing: GallState,
    /// Level-5 surfaces currently proven.
    pub satisfied_surfaces: Vec<EvidenceSurface>,
    /// Required Level-5 surfaces not currently proven.
    pub missing_surfaces: Vec<EvidenceSurface>,
    /// Latest falsifying evidence identities.
    pub blocking_evidence: Vec<String>,
}

/// Full-matrix assessment.
#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
pub struct MaturityAssessment {
    /// Contract version used.
    pub matrix_version: String,
    /// Root of the immutable evidence history.
    pub evidence_root: [u8; 32],
    /// Conjunctive crown standing.
    pub standing: GallState,
    /// Stable per-dimension results.
    pub dimensions: Vec<DimensionAssessment>,
}

impl MaturityAssessment {
    /// Return every dimension/surface obligation still open.
    #[must_use]
    pub fn open_obligations(&self) -> Vec<(Dimension, EvidenceSurface)> {
        self.dimensions
            .iter()
            .flat_map(|assessment| {
                assessment
                    .missing_surfaces
                    .iter()
                    .copied()
                    .map(move |surface| (assessment.dimension, surface))
            })
            .collect()
    }

    /// Confirm all 21 dimensions are present exactly once.
    #[must_use]
    pub fn is_complete_matrix(&self) -> bool {
        let observed: BTreeSet<Dimension> = self
            .dimensions
            .iter()
            .map(|assessment| assessment.dimension)
            .collect();
        observed.len() == ALL_DIMENSIONS.len()
            && ALL_DIMENSIONS
                .iter()
                .all(|dimension| observed.contains(dimension))
    }
}

/// Cryptographic receipt binding an assessment to its evidence root.
#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
pub struct AssessmentReceipt {
    /// Receipt schema.
    pub schema: String,
    /// Complete assessment payload.
    pub assessment: MaturityAssessment,
    /// BLAKE3 over canonical JSON of the assessment.
    pub receipt_digest: [u8; 32],
}

impl AssessmentReceipt {
    /// Issue a receipt from an assessment.
    pub fn issue(assessment: MaturityAssessment) -> Result<Self, EvidenceError> {
        let receipt_digest = assessment_digest(&assessment)?;
        Ok(Self {
            schema: "rwr-assessment-receipt/v1".to_string(),
            assessment,
            receipt_digest,
        })
    }

    /// Verify this receipt.
    pub fn verify(&self) -> Result<(), EvidenceError> {
        if self.schema != "rwr-assessment-receipt/v1"
            || self.receipt_digest != assessment_digest(&self.assessment)?
        {
            return Err(EvidenceError::ReceiptMismatch);
        }
        Ok(())
    }
}

fn assessment_digest(assessment: &MaturityAssessment) -> Result<[u8; 32], EvidenceError> {
    let payload = serde_json::to_vec(assessment)
        .map_err(|error| EvidenceError::Serialization(error.to_string()))?;
    let mut hasher = blake3::Hasher::new();
    put(&mut hasher, b"rwr-assessment/v1");
    put(&mut hasher, &payload);
    Ok(hasher.finalize().into())
}
