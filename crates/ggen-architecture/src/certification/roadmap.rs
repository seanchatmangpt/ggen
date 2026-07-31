use std::collections::{BTreeMap, BTreeSet};

use serde::Serialize;

use crate::building_block::{
    BuildingBlockId, BuildingBlockRegistry, CompositionReceipt, EvidenceReceipt, LifecycleState,
    ProfileId, Standing,
};

use super::{
    digest, seven_day_standards_profile, CertificationRefusal, EvidenceLedger, RebuildRoadmap,
    RoadmapAction, RoadmapStep, TaiRebuildReceipt, TargetArchitectureInstance,
    REBUILD_ROADMAP_SCHEMA, TAI_CASE_STUDY_ID, TAI_REBUILD_RECEIPT_SCHEMA,
};

pub fn generate_rebuild_roadmap(
    registry: &BuildingBlockRegistry, target: &TargetArchitectureInstance,
    evidence: &EvidenceLedger,
) -> Result<RebuildRoadmap, CertificationRefusal> {
    let standards_profile_digest = validate_target_context(target)?;
    if target.roots.is_empty() {
        return Err(CertificationRefusal::TargetRootsMissing);
    }
    let composition = registry.compose(&target.roots)?;
    if let Some(expected) = &target.expected_composition_digest {
        if expected != &composition.digest {
            return Err(CertificationRefusal::CompositionDigestMismatch {
                expected: expected.clone(),
                observed: composition.digest.clone(),
            });
        }
    }
    for required in &target.required_profiles {
        if !composition.profiles.contains(required) {
            return Err(CertificationRefusal::RequiredProfileMissing(
                required.clone(),
            ));
        }
    }

    let mut steps = Vec::new();
    for block_id in &composition.order {
        let block = registry
            .blocks
            .get(block_id)
            .ok_or_else(|| CertificationRefusal::ComposedBlockMissing(block_id.clone()))?;
        let actions: &[RoadmapAction] = match block.lifecycle {
            LifecycleState::Discovered => &[
                RoadmapAction::Identify,
                RoadmapAction::Qualify,
                RoadmapAction::Admit,
                RoadmapAction::Activate,
            ],
            LifecycleState::Identified => &[
                RoadmapAction::Qualify,
                RoadmapAction::Admit,
                RoadmapAction::Activate,
            ],
            LifecycleState::Qualified => &[RoadmapAction::Admit, RoadmapAction::Activate],
            LifecycleState::Admitted => &[RoadmapAction::Activate],
            LifecycleState::Active => &[],
            LifecycleState::Deprecated => &[RoadmapAction::Reactivate],
            LifecycleState::Retired | LifecycleState::Archived => {
                return Err(CertificationRefusal::TargetContainsRetiredBlock {
                    block: block_id.clone(),
                    lifecycle: block.lifecycle,
                });
            }
        };
        for action in actions {
            push_step(
                &mut steps,
                block_id,
                *action,
                format!(
                    "{} is {:?}; TAI reconstruction requires an active lifecycle",
                    block_id.as_str(),
                    block.lifecycle
                ),
            );
        }
        if block.selected_realization.is_none() {
            push_step(
                &mut steps,
                block_id,
                RoadmapAction::SelectRealization,
                "TAI has no selected passport-bound realization".to_string(),
            );
        }
        let receipts = evidence.get(block_id).cloned().unwrap_or_default();
        if block.evidence_standing(&receipts) != Standing::Alive {
            push_step(
                &mut steps,
                block_id,
                RoadmapAction::CloseEvidence,
                "every TAI obligation requires witness, falsifier, independent verifier, receipt verification, and replay".to_string(),
            );
        }
    }

    #[derive(Serialize)]
    struct Payload<'a> {
        schema: &'static str,
        target_id: &'a str,
        case_study_id: &'a str,
        standards_profile_id: &'a str,
        standards_profile_digest: &'a str,
        composition: &'a CompositionReceipt,
        steps: &'a [RoadmapStep],
    }
    let roadmap_digest = digest(&Payload {
        schema: REBUILD_ROADMAP_SCHEMA,
        target_id: &target.id,
        case_study_id: &target.case_study_id,
        standards_profile_id: &target.standards_profile_id,
        standards_profile_digest: &standards_profile_digest,
        composition: &composition,
        steps: &steps,
    })?;
    Ok(RebuildRoadmap {
        schema: REBUILD_ROADMAP_SCHEMA.to_string(),
        target_id: target.id.clone(),
        case_study_id: target.case_study_id.clone(),
        standards_profile_id: target.standards_profile_id.clone(),
        standards_profile_digest,
        composition,
        steps,
        digest: roadmap_digest,
    })
}

fn validate_target_context(
    target: &TargetArchitectureInstance,
) -> Result<String, CertificationRefusal> {
    if target.id.trim().is_empty() {
        return Err(CertificationRefusal::TargetIdentityMissing);
    }
    if target.case_study_id != TAI_CASE_STUDY_ID {
        return Err(CertificationRefusal::CaseStudyMismatch {
            expected: TAI_CASE_STUDY_ID.to_string(),
            observed: target.case_study_id.clone(),
        });
    }
    let standards = seven_day_standards_profile();
    standards.validate()?;
    if target.standards_profile_id != standards.id {
        return Err(CertificationRefusal::StandardsProfileMismatch {
            expected: standards.id,
            observed: target.standards_profile_id.clone(),
        });
    }
    let expected_digest = standards.digest()?;
    if target.standards_profile_digest != expected_digest {
        return Err(CertificationRefusal::StandardsProfileDigestMismatch {
            expected: expected_digest,
            observed: target.standards_profile_digest.clone(),
        });
    }
    Ok(target.standards_profile_digest.clone())
}

fn push_step(
    steps: &mut Vec<RoadmapStep>, block_id: &BuildingBlockId, action: RoadmapAction,
    rationale: String,
) {
    steps.push(RoadmapStep {
        sequence: steps.len().saturating_add(1) as u32,
        block_id: block_id.clone(),
        action,
        rationale,
    });
}

pub fn simulate_tai_rebuild(
    registry: &BuildingBlockRegistry, target: &TargetArchitectureInstance, candidate_id: &str,
    evidence: &EvidenceLedger,
) -> Result<TaiRebuildReceipt, CertificationRefusal> {
    if candidate_id.trim().is_empty() {
        return Err(CertificationRefusal::CandidateMissing);
    }
    let roadmap = generate_rebuild_roadmap(registry, target, evidence)?;
    if !roadmap.steps.is_empty() {
        return Err(CertificationRefusal::RoadmapIncomplete(roadmap.steps.len()));
    }
    let evidence_digests = collect_evidence_digests(&roadmap.composition.order, evidence);

    #[derive(Serialize)]
    struct Payload<'a> {
        schema: &'static str,
        candidate_id: &'a str,
        target_id: &'a str,
        case_study_id: &'a str,
        standards_profile_id: &'a str,
        standards_profile_digest: &'a str,
        composition_digest: &'a str,
        order: &'a [BuildingBlockId],
        profiles: &'a BTreeSet<ProfileId>,
        evidence_digests: &'a BTreeMap<BuildingBlockId, BTreeSet<String>>,
        standing: Standing,
    }
    let rebuild_digest = digest(&Payload {
        schema: TAI_REBUILD_RECEIPT_SCHEMA,
        candidate_id,
        target_id: &target.id,
        case_study_id: &target.case_study_id,
        standards_profile_id: &roadmap.standards_profile_id,
        standards_profile_digest: &roadmap.standards_profile_digest,
        composition_digest: &roadmap.composition.digest,
        order: &roadmap.composition.order,
        profiles: &roadmap.composition.profiles,
        evidence_digests: &evidence_digests,
        standing: Standing::Alive,
    })?;
    Ok(TaiRebuildReceipt {
        schema: TAI_REBUILD_RECEIPT_SCHEMA.to_string(),
        candidate_id: candidate_id.to_string(),
        target_id: target.id.clone(),
        case_study_id: target.case_study_id.clone(),
        standards_profile_id: roadmap.standards_profile_id,
        standards_profile_digest: roadmap.standards_profile_digest,
        composition_digest: roadmap.composition.digest,
        order: roadmap.composition.order,
        profiles: roadmap.composition.profiles,
        evidence_digests,
        standing: Standing::Alive,
        digest: rebuild_digest,
    })
}

fn collect_evidence_digests(
    order: &[BuildingBlockId], evidence: &BTreeMap<BuildingBlockId, BTreeSet<EvidenceReceipt>>,
) -> BTreeMap<BuildingBlockId, BTreeSet<String>> {
    order
        .iter()
        .map(|block_id| {
            let digests = evidence
                .get(block_id)
                .into_iter()
                .flat_map(|receipts| receipts.iter())
                .map(|receipt| receipt.digest.clone())
                .collect();
            (block_id.clone(), digests)
        })
        .collect()
}
