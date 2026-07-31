use std::collections::BTreeMap;

use serde::{Deserialize, Serialize};

use super::{
    digest, seven_day_standards_profile, CertificationAward, CertificationLevel,
    CertificationRefusal, CertificationRequirement, CertificationRequirementKind,
    RequirementReceipt, CERTIFICATION_AWARD_SCHEMA, GBB_CERTIFICATION_PROGRAM_ID,
    GGEN_SEVEN_DAY_STANDARDS_ID, REBUILD_ROADMAP_SCHEMA, TAI_CASE_STUDY_ID,
    TAI_CASE_STUDY_VERSION, TAI_REBUILD_RECEIPT_SCHEMA,
};

#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
pub struct CertificationProgram {
    pub id: String,
    pub version: String,
    pub case_study_id: String,
    pub standards_profile_id: String,
    pub requirements: BTreeMap<String, CertificationRequirement>,
}

impl CertificationProgram {
    /// Construct the five-level certification around `TAI-REBUILD-001` and the
    /// exact seven-day ggen standards profile.
    #[must_use]
    pub fn ggen_bblocks_v1() -> Self {
        let rows = [
            (
                "GBB-100-FENCE",
                CertificationLevel::Gbb100Foundation,
                CertificationRequirementKind::Fence,
                "Distinguish historical Technology Applications, Inc., the Automated Technical Capability Company target, the exact Target Architecture Instance, and admitted versus pending standards.",
                "Treat history, metadata, lifecycle, a pending checkpoint, or an external certificate as ALIVE standing.",
                "ggen.building-block.boundary-analysis.v2",
            ),
            (
                "GBB-100-DIAGNOSE",
                CertificationLevel::Gbb100Foundation,
                CertificationRequirementKind::Diagnose,
                "Diagnose malformed TAI mission, contract, capability, certification, organization, technology, quality, evidence, economics, and standards bindings.",
                "Accept a missing dependency, passport, port, falsifier, provenance, bounded authority, or standards digest.",
                "ggen.building-block.diagnostic-report.v2",
            ),
            (
                "GBB-200-CONSTRUCT",
                CertificationLevel::Gbb200Builder,
                CertificationRequirementKind::Construct,
                "Manufacture one passport-bound TAI Building Block with typed ports, ceilings, exclusions, lifecycle obligations, and exact standards-profile identity.",
                "Admit a realization without an exact passport digest, resource and authority ceilings, or standards binding.",
                "ggen.building-block.v2",
            ),
            (
                "GBB-200-EVIDENCE",
                CertificationLevel::Gbb200Builder,
                CertificationRequirementKind::EvidenceClosure,
                "Close witness, falsifier, independent verifier, receipt verifier, and replay evidence for every obligation on the exact standards profile.",
                "Promote a TAI block to ALIVE with any proof surface missing, empty, or bound to an earlier standards profile.",
                "ggen.building-block.evidence-bundle.v2",
            ),
            (
                "GBB-300-COMPOSE",
                CertificationLevel::Gbb300Composer,
                CertificationRequirementKind::Compose,
                "Compose TAI dependency closure under the admitted standards profile and emit a replay-stable BLAKE3 receipt.",
                "Use a hand-maintained implementation sequence, stale parent case, or order-dependent output.",
                "ggen.building-block.receipt.v2",
            ),
            (
                "GBB-300-REFUSE",
                CertificationLevel::Gbb300Composer,
                CertificationRequirementKind::RefuseInvalidComposition,
                "Refuse duplicate identities, missing dependencies, cycles, incomplete enterprise closure, profile conflicts, and standards drift.",
                "Repair the graph by silently deleting mission, quality, evidence, lifecycle, broker, or standards constraints.",
                "ggen.building-block.composition-refusal.v2",
            ),
            (
                "GBB-400-SUBSTITUTE",
                CertificationLevel::Gbb400Governor,
                CertificationRequirementKind::Substitute,
                "Preserve promised ports, passports, authority, resources, isolation, evidence, supportability, replacement, retirement, and standards identity during substitution.",
                "Approve authority expansion, output loss, resource breach, lifecycle-support regression, or passport detachment.",
                "ggen.building-block.substitution-assessment.v2",
            ),
            (
                "GBB-400-GOVERN",
                CertificationLevel::Gbb400Governor,
                CertificationRequirementKind::Govern,
                "Govern concept-through-decommission lifecycle, evidence standing, pending checkpoints, and BRCE-addressed intents as orthogonal states.",
                "Treat RETIRED as standing, a pending checkpoint as admitted, or cognition as direct external authority.",
                "ggen.building-block.governance-report.v2",
            ),
            (
                "GBB-500-ROADMAP",
                CertificationLevel::Gbb500TaiManufacturer,
                CertificationRequirementKind::GenerateRoadmap,
                "Generate unresolved lifecycle, realization, evidence, profile, dependency, and standards work from current TAI graph state.",
                "Hand-write a roadmap that omits closure, repeats satisfied work, or hides a pending standard.",
                REBUILD_ROADMAP_SCHEMA,
            ),
            (
                "GBB-500-REBUILD",
                CertificationLevel::Gbb500TaiManufacturer,
                CertificationRequirementKind::RebuildTai,
                "Execute the ontology-authored TAI pack across all seven scenarios and bind composition, evidence, case-study, and standards-profile receipts.",
                "Issue a rebuild receipt while generated roadmap work remains unresolved or standards identity is stale.",
                TAI_REBUILD_RECEIPT_SCHEMA,
            ),
            (
                "GBB-500-RECOVER",
                CertificationLevel::Gbb500TaiManufacturer,
                CertificationRequirementKind::RecoverFailure,
                "Recover a declared contract, certification, inspection, founder, dependency, evidence, replay, or standards failure without bypassing law.",
                "Use an emergency path that skips admission, BRCE, verification, receipt, replay, budgets, or consequence observation.",
                "ggen.building-block.recovery-receipt.v2",
            ),
            (
                "GBB-500-TRANSFER",
                CertificationLevel::Gbb500TaiManufacturer,
                CertificationRequirementKind::TeachTransfer,
                "Manufacture a verifier and falsifier another candidate can replay independently against TAI-REBUILD-001 and the exact seven-day standards profile.",
                "Award transfer standing from explanation, slides, documentation, or a certificate without exact-head independent replay.",
                "ggen.building-block.transfer-verifier.v2",
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
            standards_profile_id: GGEN_SEVEN_DAY_STANDARDS_ID.to_string(),
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
        let standards = seven_day_standards_profile();
        standards.validate()?;
        if self.standards_profile_id != standards.id {
            return Err(CertificationRefusal::StandardsProfileMismatch {
                expected: standards.id,
                observed: self.standards_profile_id.clone(),
            });
        }
        let standards_profile_digest = standards.digest()?;
        let mut requirement_ids = Vec::new();
        let mut receipt_digests = BTreeMap::new();
        for requirement in self.requirements_through(level) {
            let receipt = receipts.get(&requirement.id).ok_or_else(|| {
                CertificationRefusal::RequirementReceiptMissing(requirement.id.clone())
            })?;
            receipt.validate(
                candidate_id,
                &requirement.id,
                &self.case_study_id,
                &self.standards_profile_id,
                &standards_profile_digest,
            )?;
            requirement_ids.push(requirement.id.clone());
            receipt_digests.insert(requirement.id.clone(), digest(receipt)?);
        }

        #[derive(Serialize)]
        struct AwardPayload<'a> {
            schema: &'static str,
            program_id: &'a str,
            program_version: &'a str,
            case_study_id: &'a str,
            standards_profile_id: &'a str,
            standards_profile_digest: &'a str,
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
            standards_profile_id: &self.standards_profile_id,
            standards_profile_digest: &standards_profile_digest,
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
            standards_profile_id: self.standards_profile_id.clone(),
            standards_profile_digest,
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
        standards_profile_id: &str, standards_profile_digest: &str,
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
        if self.standards_profile_id != standards_profile_id {
            return Err(CertificationRefusal::StandardsProfileMismatch {
                expected: standards_profile_id.to_string(),
                observed: self.standards_profile_id.clone(),
            });
        }
        if self.standards_profile_digest != standards_profile_digest {
            return Err(CertificationRefusal::StandardsProfileDigestMismatch {
                expected: standards_profile_digest.to_string(),
                observed: self.standards_profile_digest.clone(),
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
