use std::collections::BTreeMap;

use serde::{Deserialize, Serialize};

use super::{
    digest, CertificationAward, CertificationLevel, CertificationRefusal, CertificationRequirement,
    CertificationRequirementKind, RequirementReceipt, CERTIFICATION_AWARD_SCHEMA,
    GBB_CERTIFICATION_PROGRAM_ID, REBUILD_ROADMAP_SCHEMA, TAI_CASE_STUDY_ID,
    TAI_REBUILD_RECEIPT_SCHEMA,
};

/// Cumulative evidence-backed certification program.
#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
pub struct CertificationProgram {
    pub id: String,
    pub version: String,
    pub case_study_id: String,
    pub requirements: BTreeMap<String, CertificationRequirement>,
}

impl CertificationProgram {
    /// Construct the canonical five-level program around the TAI case study.
    #[must_use]
    pub fn ggen_bblocks_v1() -> Self {
        let rows = [
            (
                "GBB-100-FENCE",
                CertificationLevel::Gbb100Foundation,
                CertificationRequirementKind::Fence,
                "Preserve the distinction between historical TAI observation, the Automated Technical Capability Company target, and the exact Target Architecture Instance reconstructed from Building Blocks.",
                "Collapse lifecycle, external certification, metadata, or historical narrative into ALIVE standing.",
                "ggen.building-block.boundary-analysis.v1",
            ),
            (
                "GBB-100-DIAGNOSE",
                CertificationLevel::Gbb100Foundation,
                CertificationRequirementKind::Diagnose,
                "Map malformed TAI Mission, Contract, Organization, Capability, Certification, Technology, Quality, Evidence, and Economics blocks to stable refusal codes and bounded repairs.",
                "Accept a missing passport, expanded authority, invalid port, absent falsifier, or detached TAI provenance.",
                "ggen.building-block.diagnostic-report.v1",
            ),
            (
                "GBB-200-CONSTRUCT",
                CertificationLevel::Gbb200Builder,
                CertificationRequirementKind::Construct,
                "Manufacture one valid TAI enterprise facet with typed ports, ceilings, exclusions, provenance, and a passport-bound realization.",
                "Admit a TAI realization without exact passport digest, resource claim, authority ceiling, or lifecycle support obligation.",
                "ggen.building-block.v1",
            ),
            (
                "GBB-200-EVIDENCE",
                CertificationLevel::Gbb200Builder,
                CertificationRequirementKind::EvidenceClosure,
                "Close positive, negative, independent-verifier, receipt-verifier, and replay evidence for every TAI obligation.",
                "Promote a TAI block to ALIVE with any proof surface absent or digest empty.",
                "ggen.building-block.evidence-bundle.v1",
            ),
            (
                "GBB-300-COMPOSE",
                CertificationLevel::Gbb300Composer,
                CertificationRequirementKind::Compose,
                "Compose the nine TAI enterprise facets and enterprise root in dependency-before-dependent order and emit a replay-stable BLAKE3 receipt.",
                "Produce a hand-authored implementation sequence or order-dependent TAI composition output.",
                "ggen.building-block.receipt.v1",
            ),
            (
                "GBB-300-REFUSE",
                CertificationLevel::Gbb300Composer,
                CertificationRequirementKind::RefuseInvalidComposition,
                "Refuse missing TAI dependencies, cycles, duplicate identities, incompatible profiles, and incomplete enterprise closure.",
                "Repair the TAI graph by silently deleting mission, quality, evidence, lifecycle, or broker constraints.",
                "ggen.building-block.composition-refusal.v1",
            ),
            (
                "GBB-400-SUBSTITUTE",
                CertificationLevel::Gbb400Governor,
                CertificationRequirementKind::Substitute,
                "Substitute a TAI realization while preserving promised ports, passport standing, authority, resources, supportability, replacement, and retirement boundaries.",
                "Approve authority expansion, output loss, resource-ceiling breach, or lifecycle-support regression.",
                "ggen.building-block.substitution-assessment.v1",
            ),
            (
                "GBB-400-GOVERN",
                CertificationLevel::Gbb400Governor,
                CertificationRequirementKind::Govern,
                "Govern TAI concept-through-decommission lifecycle, evidence standing, and BRCE-addressed intents as orthogonal states.",
                "Treat RETIRED as standing, cognition as authority, or allow direct external actuation outside BRCE.",
                "ggen.building-block.governance-report.v1",
            ),
            (
                "GBB-500-ROADMAP",
                CertificationLevel::Gbb500TaiManufacturer,
                CertificationRequirementKind::GenerateRoadmap,
                "Generate exact lifecycle, realization, evidence, profile, and dependency work from the admitted TAI Building Block closure.",
                "Hand-write a TAI roadmap that omits closure, repeats satisfied work, or begins from a projection instead of admitted graph state.",
                REBUILD_ROADMAP_SCHEMA,
            ),
            (
                "GBB-500-REBUILD",
                CertificationLevel::Gbb500TaiManufacturer,
                CertificationRequirementKind::RebuildTai,
                "Simulate rebuilding the complete Automated Technical Capability Company and bind composition, profile, lifecycle, and evidence digests.",
                "Issue a TAI rebuild receipt while any generated roadmap step remains unresolved.",
                TAI_REBUILD_RECEIPT_SCHEMA,
            ),
            (
                "GBB-500-RECOVER",
                CertificationLevel::Gbb500TaiManufacturer,
                CertificationRequirementKind::RecoverFailure,
                "Inject and recover from one TAI dependency, profile, realization, evidence, replay, or lifecycle failure without bypassing law.",
                "Use an emergency path that skips admission, BRCE, verifier, receipt, replay, or preserved failed observation.",
                "ggen.building-block.recovery-receipt.v1",
            ),
            (
                "GBB-500-TRANSFER",
                CertificationLevel::Gbb500TaiManufacturer,
                CertificationRequirementKind::TeachTransfer,
                "Manufacture a TAI verifier and falsifier fixture another candidate can replay to reconstruct the enterprise independently.",
                "Award transfer standing from explanation, slides, or a certificate without independent enterprise replay.",
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
            case_study_id: TAI_CASE_STUDY_ID.to_string(),
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
