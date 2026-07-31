use std::collections::BTreeMap;

use serde::{Deserialize, Serialize};

use super::{
    digest, CertificationAward, CertificationLevel, CertificationRefusal, CertificationRequirement,
    CertificationRequirementKind, RequirementReceipt, CERTIFICATION_AWARD_SCHEMA,
    GBB_CERTIFICATION_PROGRAM_ID, REBUILD_ROADMAP_SCHEMA, TAI_CASE_STUDY_ID,
    TAI_CASE_STUDY_VERSION, TAI_REBUILD_RECEIPT_SCHEMA,
};

#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
pub struct CertificationProgram {
    pub id: String,
    pub version: String,
    pub case_study_id: String,
    pub requirements: BTreeMap<String, CertificationRequirement>,
}

impl CertificationProgram {
    /// Construct the five-level certification around `TAI-REBUILD-001`.
    #[must_use]
    pub fn ggen_bblocks_v1() -> Self {
        let rows = [
            (
                "GBB-100-FENCE",
                CertificationLevel::Gbb100Foundation,
                CertificationRequirementKind::Fence,
                "Distinguish historical Technology Applications, Inc., the Automated Technical Capability Company target, and the exact Target Architecture Instance.",
                "Treat history, metadata, lifecycle, or an external certificate as ALIVE standing.",
                "ggen.building-block.boundary-analysis.v1",
            ),
            (
                "GBB-100-DIAGNOSE",
                CertificationLevel::Gbb100Foundation,
                CertificationRequirementKind::Diagnose,
                "Diagnose malformed TAI mission, contract, capability, certification, organization, technology, quality, evidence, and economics blocks.",
                "Accept a missing dependency, passport, port, falsifier, provenance, or bounded authority.",
                "ggen.building-block.diagnostic-report.v1",
            ),
            (
                "GBB-200-CONSTRUCT",
                CertificationLevel::Gbb200Builder,
                CertificationRequirementKind::Construct,
                "Manufacture one passport-bound TAI Building Block with typed ports, ceilings, exclusions, and lifecycle obligations.",
                "Admit a realization without an exact passport digest or resource and authority ceilings.",
                "ggen.building-block.v1",
            ),
            (
                "GBB-200-EVIDENCE",
                CertificationLevel::Gbb200Builder,
                CertificationRequirementKind::EvidenceClosure,
                "Close witness, falsifier, independent verifier, receipt verifier, and replay evidence for every obligation.",
                "Promote a TAI block to ALIVE with any proof surface missing or empty.",
                "ggen.building-block.evidence-bundle.v1",
            ),
            (
                "GBB-300-COMPOSE",
                CertificationLevel::Gbb300Composer,
                CertificationRequirementKind::Compose,
                "Compose TAI dependency closure and emit a replay-stable BLAKE3 receipt.",
                "Use a hand-maintained implementation sequence or order-dependent output.",
                "ggen.building-block.receipt.v1",
            ),
            (
                "GBB-300-REFUSE",
                CertificationLevel::Gbb300Composer,
                CertificationRequirementKind::RefuseInvalidComposition,
                "Refuse duplicate identities, missing dependencies, cycles, incomplete enterprise closure, and profile conflicts.",
                "Repair the graph by silently deleting mission, quality, evidence, lifecycle, or broker constraints.",
                "ggen.building-block.composition-refusal.v1",
            ),
            (
                "GBB-400-SUBSTITUTE",
                CertificationLevel::Gbb400Governor,
                CertificationRequirementKind::Substitute,
                "Preserve promised ports, passports, authority, resources, supportability, replacement, and retirement during substitution.",
                "Approve authority expansion, output loss, resource breach, or lifecycle-support regression.",
                "ggen.building-block.substitution-assessment.v1",
            ),
            (
                "GBB-400-GOVERN",
                CertificationLevel::Gbb400Governor,
                CertificationRequirementKind::Govern,
                "Govern concept-through-decommission lifecycle, evidence standing, and BRCE-addressed intents as orthogonal states.",
                "Treat RETIRED as standing or allow cognition to authorize direct external actuation.",
                "ggen.building-block.governance-report.v1",
            ),
            (
                "GBB-500-ROADMAP",
                CertificationLevel::Gbb500TaiManufacturer,
                CertificationRequirementKind::GenerateRoadmap,
                "Generate unresolved lifecycle, realization, evidence, profile, and dependency work from current TAI graph state.",
                "Hand-write a roadmap that omits closure or repeats satisfied work.",
                REBUILD_ROADMAP_SCHEMA,
            ),
            (
                "GBB-500-REBUILD",
                CertificationLevel::Gbb500TaiManufacturer,
                CertificationRequirementKind::RebuildTai,
                "Execute the ontology-authored TAI pack across all seven scenarios and bind the resulting composition and evidence receipts.",
                "Issue a rebuild receipt while generated roadmap work remains unresolved.",
                TAI_REBUILD_RECEIPT_SCHEMA,
            ),
            (
                "GBB-500-RECOVER",
                CertificationLevel::Gbb500TaiManufacturer,
                CertificationRequirementKind::RecoverFailure,
                "Recover a declared contract, certification, inspection, founder, dependency, evidence, or replay failure without bypassing law.",
                "Use an emergency path that skips admission, BRCE, verification, receipt, or replay.",
                "ggen.building-block.recovery-receipt.v1",
            ),
            (
                "GBB-500-TRANSFER",
                CertificationLevel::Gbb500TaiManufacturer,
                CertificationRequirementKind::TeachTransfer,
                "Manufacture a verifier and falsifier another candidate can replay independently against TAI-REBUILD-001.",
                "Award transfer standing from explanation, slides, or a certificate without independent replay.",
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
            version: TAI_CASE_STUDY_VERSION.to_string(),
            case_study_id: TAI_CASE_STUDY_ID.to_string(),
            requirements,
        }
    }

    #[must_use]
    pub fn requirements_through(
        &self, level: CertificationLevel,
    ) -> Vec<&CertificationRequirement> {
        self.requirements
            .values()
            .filter(|requirement| requirement.level <= level)
            .collect()
    }

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
            receipt.validate(candidate_id, &requirement.id, &self.case_study_id)?;
            requirement_ids.push(requirement.id.clone());
            receipt_digests.insert(requirement.id.clone(), digest(receipt)?);
        }

        #[derive(Serialize)]
        struct AwardPayload<'a> {
            schema: &'static str,
            program_id: &'a str,
            program_version: &'a str,
            case_study_id: &'a str,
            candidate_id: &'a str,
            level: CertificationLevel,
            requirement_ids: &'a [String],
            receipt_digests: &'a BTreeMap<String, String>,
        }
        let award_digest = digest(&AwardPayload {
            schema: CERTIFICATION_AWARD_SCHEMA,
            program_id: &self.id,
            program_version: &self.version,
            case_study_id: &self.case_study_id,
            candidate_id,
            level,
            requirement_ids: &requirement_ids,
            receipt_digests: &receipt_digests,
        })?;
        Ok(CertificationAward {
            schema: CERTIFICATION_AWARD_SCHEMA.to_string(),
            program_id: self.id.clone(),
            program_version: self.version.clone(),
            case_study_id: self.case_study_id.clone(),
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
        &self, candidate_id: &str, requirement_id: &str, case_study_id: &str,
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
        if self.case_study_id != case_study_id {
            return Err(CertificationRefusal::CaseStudyMismatch {
                expected: case_study_id.to_string(),
                observed: self.case_study_id.clone(),
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
