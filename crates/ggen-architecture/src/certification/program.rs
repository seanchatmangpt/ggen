use std::collections::BTreeMap;

use serde::{Deserialize, Serialize};

use super::{
    digest, CertificationAward, CertificationLevel, CertificationRefusal, CertificationRequirement,
    CertificationRequirementKind, RequirementReceipt, CERTIFICATION_AWARD_SCHEMA,
    GBB_CERTIFICATION_PROGRAM_ID, REBUILD_ROADMAP_SCHEMA, TAI_REBUILD_RECEIPT_SCHEMA,
};

/// Cumulative evidence-backed certification program.
#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
pub struct CertificationProgram {
    pub id: String,
    pub version: String,
    pub requirements: BTreeMap<String, CertificationRequirement>,
}

impl CertificationProgram {
    /// Construct the canonical five-level program.
    #[must_use]
    pub fn ggen_bblocks_v1() -> Self {
        let rows = [
            (
                "GBB-100-FENCE",
                CertificationLevel::Gbb100Foundation,
                CertificationRequirementKind::Fence,
                "Explain identity, lifecycle, standing, ports, ceilings, obligations, exclusions, and provenance without collapsing their boundaries.",
                "Treat lifecycle, external certification, or metadata as proof of ALIVE standing.",
                "ggen.building-block.boundary-analysis.v1",
            ),
            (
                "GBB-100-DIAGNOSE",
                CertificationLevel::Gbb100Foundation,
                CertificationRequirementKind::Diagnose,
                "Map malformed blocks to stable refusal codes and bounded repairs.",
                "Accept a missing passport, expanded authority, invalid port, or absent falsifier.",
                "ggen.building-block.diagnostic-report.v1",
            ),
            (
                "GBB-200-CONSTRUCT",
                CertificationLevel::Gbb200Builder,
                CertificationRequirementKind::Construct,
                "Manufacture a valid block with typed ports, ceilings, exclusions, provenance, and passport-bound realization.",
                "Admit a realization without exact passport digest, resources, or authority ceiling.",
                "ggen.building-block.v1",
            ),
            (
                "GBB-200-EVIDENCE",
                CertificationLevel::Gbb200Builder,
                CertificationRequirementKind::EvidenceClosure,
                "Close positive, negative, independent-verifier, receipt-verifier, and replay evidence for every obligation.",
                "Promote to ALIVE with any proof surface absent or digest empty.",
                "ggen.building-block.evidence-bundle.v1",
            ),
            (
                "GBB-300-COMPOSE",
                CertificationLevel::Gbb300Composer,
                CertificationRequirementKind::Compose,
                "Compose dependency closure and emit a replay-stable BLAKE3 receipt.",
                "Produce order-dependent or non-replayable composition output.",
                "ggen.building-block.receipt.v1",
            ),
            (
                "GBB-300-REFUSE",
                CertificationLevel::Gbb300Composer,
                CertificationRequirementKind::RefuseInvalidComposition,
                "Refuse missing dependencies, cycles, duplicate identities, and profile conflicts.",
                "Repair an invalid graph by silently deleting constraints or dependencies.",
                "ggen.building-block.composition-refusal.v1",
            ),
            (
                "GBB-400-SUBSTITUTE",
                CertificationLevel::Gbb400Governor,
                CertificationRequirementKind::Substitute,
                "Preserve promised ports, passport standing, authority, and resources during substitution.",
                "Approve authority expansion, output loss, or resource-ceiling breach.",
                "ggen.building-block.substitution-assessment.v1",
            ),
            (
                "GBB-400-GOVERN",
                CertificationLevel::Gbb400Governor,
                CertificationRequirementKind::Govern,
                "Govern lifecycle, evidence standing, and broker-addressed intents as orthogonal states.",
                "Treat RETIRED as standing or allow direct external actuation.",
                "ggen.building-block.governance-report.v1",
            ),
            (
                "GBB-500-ROADMAP",
                CertificationLevel::Gbb500TaiManufacturer,
                CertificationRequirementKind::GenerateRoadmap,
                "Generate exact lifecycle, realization, and evidence work from the selected GBB closure.",
                "Hand-write a roadmap that omits closure or repeats already satisfied work.",
                REBUILD_ROADMAP_SCHEMA,
            ),
            (
                "GBB-500-REBUILD",
                CertificationLevel::Gbb500TaiManufacturer,
                CertificationRequirementKind::RebuildTai,
                "Simulate rebuilding the complete TAI and bind composition and evidence digests.",
                "Issue a rebuild receipt while any roadmap step remains unresolved.",
                TAI_REBUILD_RECEIPT_SCHEMA,
            ),
            (
                "GBB-500-RECOVER",
                CertificationLevel::Gbb500TaiManufacturer,
                CertificationRequirementKind::RecoverFailure,
                "Inject and recover from one declared failure without bypassing law.",
                "Use an emergency path that skips verifier, receipt, or replay.",
                "ggen.building-block.recovery-receipt.v1",
            ),
            (
                "GBB-500-TRANSFER",
                CertificationLevel::Gbb500TaiManufacturer,
                CertificationRequirementKind::TeachTransfer,
                "Manufacture a verifier and falsifier fixture another candidate can replay.",
                "Award transfer standing from explanation without independent replay.",
                "ggen.building-block.transfer-verifier.v1",
            ),
        ];
        let requirements = rows
            .into_iter()
            .map(|(id, level, kind, outcome, falsifier, artifact_schema)| {
                let requirement = CertificationRequirement {
                    id: id.to_string(),
                    level,
                    kind,
                    outcome: outcome.to_string(),
                    falsifier: falsifier.to_string(),
                    artifact_schema: artifact_schema.to_string(),
                };
                (requirement.id.clone(), requirement)
            })
            .collect();
        Self {
            id: GBB_CERTIFICATION_PROGRAM_ID.to_string(),
            version: "26.7.30".to_string(),
            requirements,
        }
    }

    /// Requirements accumulated through the requested credential.
    #[must_use]
    pub fn requirements_through(
        &self, level: CertificationLevel,
    ) -> Vec<&CertificationRequirement> {
        self.requirements
            .values()
            .filter(|requirement| requirement.level <= level)
            .collect()
    }

    /// Assess a cumulative portfolio and issue a deterministic award receipt.
    pub fn assess(
        &self, candidate_id: &str, level: CertificationLevel,
        receipts: &BTreeMap<String, RequirementReceipt>,
    ) -> Result<CertificationAward, CertificationRefusal> {
        if candidate_id.trim().is_empty() {
            return Err(CertificationRefusal::CandidateMissing);
        }
        let mut requirement_ids = Vec::new();
        let mut receipt_digests = BTreeMap::new();
        for requirement in self.requirements_through(level) {
            let receipt = receipts.get(&requirement.id).ok_or_else(|| {
                CertificationRefusal::RequirementReceiptMissing(requirement.id.clone())
            })?;
            receipt.validate(candidate_id, &requirement.id)?;
            requirement_ids.push(requirement.id.clone());
            receipt_digests.insert(requirement.id.clone(), digest(receipt)?);
        }

        #[derive(Serialize)]
        struct AwardPayload<'a> {
            schema: &'static str,
            program_id: &'a str,
            program_version: &'a str,
            candidate_id: &'a str,
            level: CertificationLevel,
            requirement_ids: &'a [String],
            receipt_digests: &'a BTreeMap<String, String>,
        }
        let award_digest = digest(&AwardPayload {
            schema: CERTIFICATION_AWARD_SCHEMA,
            program_id: &self.id,
            program_version: &self.version,
            candidate_id,
            level,
            requirement_ids: &requirement_ids,
            receipt_digests: &receipt_digests,
        })?;
        Ok(CertificationAward {
            schema: CERTIFICATION_AWARD_SCHEMA.to_string(),
            program_id: self.id.clone(),
            program_version: self.version.clone(),
            candidate_id: candidate_id.to_string(),
            level,
            requirement_ids,
            receipt_digests,
            digest: award_digest,
        })
    }
}

impl RequirementReceipt {
    fn validate(
        &self, candidate_id: &str, requirement_id: &str,
    ) -> Result<(), CertificationRefusal> {
        if self.candidate_id != candidate_id {
            return Err(CertificationRefusal::ReceiptCandidateMismatch {
                expected: candidate_id.to_string(),
                observed: self.candidate_id.clone(),
            });
        }
        if self.requirement_id != requirement_id {
            return Err(CertificationRefusal::ReceiptRequirementMismatch {
                expected: requirement_id.to_string(),
                observed: self.requirement_id.clone(),
            });
        }
        for (surface, value) in [
            ("positive_witness", &self.positive_witness_digest),
            ("negative_falsifier", &self.negative_falsifier_digest),
            ("independent_verifier", &self.independent_verifier_digest),
            ("receipt_verifier", &self.receipt_verifier_digest),
            ("replay", &self.replay_digest),
            ("artifact", &self.artifact_digest),
        ] {
            if value.trim().is_empty() {
                return Err(CertificationRefusal::ReceiptSurfaceMissing {
                    requirement: requirement_id.to_string(),
                    surface: surface.to_string(),
                });
            }
        }
        Ok(())
    }
}
