//! Evidence admission, full-matrix assessment, and assessment receipts.

use crate::rwr::matrix::{contract, Dimension, EvidenceSurface, MaturityLevel, ALL_DIMENSIONS, MATRIX_VERSION};
use serde::{Deserialize, Serialize};
use std::collections::{BTreeMap, BTreeSet, HashSet};

/// Gall standing used by the RWR execution foundation.
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash, Serialize, Deserialize)]
pub enum GallState {
    /// Some real execution exists, but the bounded crown contract is not closed.
    PartialAlive,
    /// Every required dimension is proven by observed Level-5 execution.
    Alive,
    /// A policy, invariant, or evidence failure blocks promotion.
    Blocked,
    /// The executable verifier or generated consumer failed to build.
    BuildBroken,
    /// Required observation is missing or inconclusive.
    Unknown,
    /// The requested capability is outside the admitted boundary.
    Unsupported,
}

/// Outcome observed on one proof surface.
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash, Serialize, Deserialize)]
#[repr(u8)]
pub enum EvidenceOutcome {
    /// The observation passed its real verifier.
    Pass,
    /// The observation crossed the boundary and falsified the claim.
    Fail,
    /// The executable verifier could not build.
    BuildFailed,
    /// The observation was not obtained.
    Unknown,
    /// The source cannot prove this surface within its declared boundary.
    Unsupported,
}

/// One immutable admitted observation.
#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
pub struct EvidenceRecord {
    /// Stable evidence identity.
    pub id: String,
    /// Matrix dimension supported or falsified by this observation.
    pub dimension: Dimension,
    /// Highest maturity level directly exercised by this observation.
    pub level: MaturityLevel,
    /// Independent proof surface.
    pub surface: EvidenceSurface,
    /// Observed result.
    pub outcome: EvidenceOutcome,
    /// Human- and machine-locatable evidence source.
    pub source: String,
    /// Monotonic observation epoch. Later epochs supersede earlier observations
    /// for current-state assessment while preserving history in the ledger root.
    pub epoch: u64,
    /// BLAKE3 of the externalized evidence bytes.
    pub artifact_digest: [u8; 32],
    /// BLAKE3 binding all record fields.
    pub observation_digest: [u8; 32],
}

impl EvidenceRecord {
    /// Construct a record from bytes actually observed at the evidence boundary.
    #[must_use]
    pub fn observed(
        id: impl Into<String>,
        dimension: Dimension,
        level: MaturityLevel,
        surface: EvidenceSurface,
        outcome: EvidenceOutcome,
        source: impl Into<String>,
        epoch: u64,
        artifact: &[u8],
    ) -> Self {
        let id = id.into();
        let source = source.into();
        let artifact_digest: [u8; 32] = blake3::hash(artifact).into();
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

fn put_len_prefixed(hasher: &mut blake3::Hasher, value: &[u8]) {
    hasher.update(&(value.len() as u64).to_le_bytes());
    hasher.update(value);
}

#[allow(clippy::too_many_arguments)]
fn observation_digest(
    id: &str,
    dimension: Dimension,
    level: MaturityLevel,
    surface: EvidenceSurface,
    outcome: EvidenceOutcome,
    source: &str,
    epoch: u64,
    artifact_digest: &[u8; 32],
) -> [u8; 32] {
    let mut hasher = blake3::Hasher::new();
    put_len_prefixed(&mut hasher, b"rwr-evidence/v1");
    put_len_prefixed(&mut hasher, id.as_bytes());
    hasher.update(&[dimension as u8]);
    hasher.update(&[level as u8]);
    hasher.update(&[surface as u8]);
    hasher.update(&[outcome as u8]);
    put_len_prefixed(&mut hasher, source.as_bytes());
    hasher.update(&epoch.to_le_bytes());
    hasher.update(artifact_digest);
    hasher.finalize().into()
}

/// Evidence-ledger admission failure.
#[derive(Debug, thiserror::Error, PartialEq, Eq)]
pub enum EvidenceError {
    /// Evidence identities are immutable and unique.
    #[error("duplicate evidence id refused: {0}")]
    DuplicateId(String),
    /// The supplied record does not verify cryptographically.
    #[error("evidence record digest mismatch: {0}")]
    DigestMismatch(String),
    /// Assessment receipt serialization failed.
    #[error("assessment serialization failed: {0}")]
    Serialization(String),
    /// Assessment receipt hash does not match its payload.
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
        Self { records: Vec::new() }
    }

    /// Admit one immutable observation.
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

    /// Read the complete immutable history.
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

    /// Cryptographic root over the complete evidence history.
    #[must_use]
    pub fn root_digest(&self) -> [u8; 32] {
        let mut digests: Vec<[u8; 32]> = self
            .records
            .iter()
            .map(|record| record.observation_digest)
            .collect();
        digests.sort_unstable();
        let mut hasher = blake3::Hasher::new();
        put_len_prefixed(&mut hasher, b"rwr-evidence-ledger/v1");
        for digest in digests {
            hasher.update(&digest);
        }
        hasher.finalize().into()
    }

    /// Assess all dimensions against the full Level-5 contract.
    #[must_use]
    pub fn assess(&self) -> MaturityAssessment {
        let dimensions = ALL_DIMENSIONS
            .iter()
            .copied()
            .map(|dimension| self.assess_dimension(dimension))
            .collect::<Vec<_>>();
        let standing = overall_standing(&dimensions);
        MaturityAssessment {
            matrix_version: MATRIX_VERSION.to_string(),
            evidence_root: self.root_digest(),
            standing,
            dimensions,
        }
    }

    fn latest_for_dimension(
        &self,
        dimension: Dimension,
    ) -> BTreeMap<EvidenceSurface, &EvidenceRecord> {
        let mut latest = BTreeMap::new();
        for record in self.records.iter().filter(|record| record.dimension == dimension) {
            match latest.get(&record.surface) {
                Some(current)
                    if current.epoch > record.epoch
                        || (current.epoch == record.epoch && current.id >= record.id) => {}
                _ => {
                    latest.insert(record.surface, record);
                }
            }
        }
        latest
    }

    fn assess_dimension(&self, dimension: Dimension) -> DimensionAssessment {
        let dimension_contract = contract(dimension);
        let latest = self.latest_for_dimension(dimension);
        let mut satisfied_surfaces = Vec::new();
        let mut missing_surfaces = Vec::new();
        let mut blocking_evidence = Vec::new();

        for surface in dimension_contract.required_surfaces {
            match latest.get(surface) {
                Some(record)
                    if record.outcome == EvidenceOutcome::Pass
                        && record.level >= MaturityLevel::DigitalEcosystem =>
                {
                    satisfied_surfaces.push(*surface);
                }
                Some(record) => {
                    missing_surfaces.push(*surface);
                    if record.outcome != EvidenceOutcome::Pass {
                        blocking_evidence.push(record.id.clone());
                    }
                }
                None => missing_surfaces.push(*surface),
            }
        }

        let attained_level = attained_level(dimension_contract.required_surfaces, &latest);
        let standing = dimension_standing(
            dimension_contract.required_surfaces,
            &latest,
            attained_level,
        );

        DimensionAssessment {
            dimension,
            attained_level,
            standing,
            satisfied_surfaces,
            missing_surfaces,
            blocking_evidence,
        }
    }
}

fn attained_level(
    required_surfaces: &[EvidenceSurface],
    latest: &BTreeMap<EvidenceSurface, &EvidenceRecord>,
) -> Option<MaturityLevel> {
    const LEVELS: [MaturityLevel; 5] = [
        MaturityLevel::DigitalEcosystem,
        MaturityLevel::BusinessModularity,
        MaturityLevel::OptimizedCore,
        MaturityLevel::StandardizedTechnology,
        MaturityLevel::BusinessSilos,
    ];

    LEVELS.into_iter().find(|candidate| {
        required_surfaces.iter().all(|surface| {
            latest.get(surface).is_some_and(|record| {
                record.outcome == EvidenceOutcome::Pass && record.level >= *candidate
            })
        })
    })
}

fn dimension_standing(
    required_surfaces: &[EvidenceSurface],
    latest: &BTreeMap<EvidenceSurface, &EvidenceRecord>,
    attained_level: Option<MaturityLevel>,
) -> GallState {
    let outcomes = required_surfaces
        .iter()
        .filter_map(|surface| latest.get(surface).map(|record| record.outcome))
        .collect::<HashSet<_>>();

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
    if dimensions.iter().all(|dimension| dimension.standing == GallState::Alive) {
        return GallState::Alive;
    }
    for candidate in [
        GallState::BuildBroken,
        GallState::Blocked,
        GallState::Unsupported,
        GallState::Unknown,
        GallState::PartialAlive,
    ] {
        if dimensions.iter().any(|dimension| dimension.standing == candidate) {
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
    /// Highest level with all required surfaces passing.
    pub attained_level: Option<MaturityLevel>,
    /// Gall standing for the dimension.
    pub standing: GallState,
    /// Level-5 surfaces currently proven.
    pub satisfied_surfaces: Vec<EvidenceSurface>,
    /// Required Level-5 surfaces not currently proven.
    pub missing_surfaces: Vec<EvidenceSurface>,
    /// Latest falsifying or non-admitted evidence identities.
    pub blocking_evidence: Vec<String>,
}

/// Full-matrix assessment.
#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
pub struct MaturityAssessment {
    /// Contract version used to assess the ledger.
    pub matrix_version: String,
    /// Root of the immutable evidence history.
    pub evidence_root: [u8; 32],
    /// Crown standing. `Alive` requires every dimension to be `Alive`.
    pub standing: GallState,
    /// Stable ordered per-dimension results.
    pub dimensions: Vec<DimensionAssessment>,
}

impl MaturityAssessment {
    /// Return every dimension/surface obligation still open at Level 5.
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

    /// Confirm that all 21 dimensions are present exactly once.
    #[must_use]
    pub fn is_complete_matrix(&self) -> bool {
        let observed = self
            .dimensions
            .iter()
            .map(|assessment| assessment.dimension)
            .collect::<BTreeSet<_>>();
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
    /// BLAKE3 over the canonical JSON payload.
    pub receipt_digest: [u8; 32],
}

impl AssessmentReceipt {
    /// Create a receipt from an assessment.
    pub fn issue(assessment: MaturityAssessment) -> Result<Self, EvidenceError> {
        let receipt_digest = assessment_digest(&assessment)?;
        Ok(Self {
            schema: "rwr-assessment-receipt/v1".to_string(),
            assessment,
            receipt_digest,
        })
    }

    /// Verify the assessment receipt.
    pub fn verify(&self) -> Result<(), EvidenceError> {
        if self.schema != "rwr-assessment-receipt/v1" {
            return Err(EvidenceError::ReceiptMismatch);
        }
        let expected = assessment_digest(&self.assessment)?;
        if self.receipt_digest != expected {
            return Err(EvidenceError::ReceiptMismatch);
        }
        Ok(())
    }
}

fn assessment_digest(assessment: &MaturityAssessment) -> Result<[u8; 32], EvidenceError> {
    let payload = serde_json::to_vec(assessment)
        .map_err(|error| EvidenceError::Serialization(error.to_string()))?;
    let mut hasher = blake3::Hasher::new();
    put_len_prefixed(&mut hasher, b"rwr-assessment/v1");
    put_len_prefixed(&mut hasher, &payload);
    Ok(hasher.finalize().into())
}
