//! Exact Fortune 5 crown law above the canonical 21/99/63 assessor.
//!
//! The assessor proves the conjunctive enterprise capability profile. The crown
//! additionally requires six release truths, five SLA governors, six operational
//! ingress controls, taxonomy/profile cardinality, receipt replay, segregation of
//! duties, and zero direct actuation. Synthetic evidence may prove the machinery
//! but can never authorize production promotion.

use std::collections::{BTreeMap, BTreeSet};

use serde::{Deserialize, Serialize};

use crate::{
    error::Result,
    fortune5::{Fortune5Assessment, Fortune5Catalog, ProofKind},
    model::{Severity, Standing},
    receipt::deterministic_hash,
};

/// Six release truths required before Fortune 5 production standing.
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Serialize, Deserialize)]
#[serde(rename_all = "snake_case")]
pub enum ReleaseTruth {
    DeterministicExecution,
    PerformanceGuarantees,
    CryptographicReceipts,
    InfinityGeneration,
    Fortune5Integration,
    DarkMatterEnergyElimination,
}

impl ReleaseTruth {
    /// Canonical release-truth universe.
    #[must_use]
    pub fn all() -> BTreeSet<Self> {
        BTreeSet::from([
            Self::DeterministicExecution,
            Self::PerformanceGuarantees,
            Self::CryptographicReceipts,
            Self::InfinityGeneration,
            Self::Fortune5Integration,
            Self::DarkMatterEnergyElimination,
        ])
    }
}

/// Five Fortune 5 SLA governors.
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Serialize, Deserialize)]
#[serde(rename_all = "snake_case")]
pub enum SlaGovernor {
    SloTracking,
    PromotionGates,
    MultiRegion,
    SpiffeSpire,
    KmsIntegration,
}

impl SlaGovernor {
    /// Canonical SLA-governor universe.
    #[must_use]
    pub fn all() -> BTreeSet<Self> {
        BTreeSet::from([
            Self::SloTracking,
            Self::PromotionGates,
            Self::MultiRegion,
            Self::SpiffeSpire,
            Self::KmsIntegration,
        ])
    }
}

/// Evidence attached to one release truth or SLA governor.
#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
pub struct CrownEvidence {
    pub evidence_id: String,
    pub producer: String,
    pub approver: String,
    pub verifier: String,
    pub digest: String,
    #[serde(default)]
    pub artifacts: BTreeSet<String>,
    pub standing: Standing,
}

impl CrownEvidence {
    fn findings(&self, subject: &str) -> Vec<CrownFinding> {
        let mut findings = Vec::new();
        if self.evidence_id.trim().is_empty()
            || self.producer.trim().is_empty()
            || self.approver.trim().is_empty()
            || self.verifier.trim().is_empty()
            || self.digest.trim().is_empty()
            || self.artifacts.is_empty()
        {
            findings.push(finding(
                "F5-CROWN-EVID-001",
                Severity::Error,
                subject,
                "crown evidence is missing identity, digest, or artifact bindings",
                "admit a complete evidence package with stable identity and artifacts",
            ));
        }
        let identities = BTreeSet::from([
            self.producer.as_str(),
            self.approver.as_str(),
            self.verifier.as_str(),
        ]);
        if identities.len() != 3 {
            findings.push(finding(
                "F5-CROWN-SOD-001",
                Severity::Critical,
                subject,
                "producer, approver, and verifier identities collapse",
                "segregate evidence production, approval, and verification authorities",
            ));
        }
        if self.standing != Standing::Alive {
            findings.push(finding(
                "F5-CROWN-EVID-002",
                Severity::Error,
                subject,
                "crown evidence does not have ALIVE standing",
                "repair the evidence package and independently reverify it",
            ));
        }
        findings
    }
}

/// Six ingress controls preserving bounded execution, budgets, chronology, and law.
#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
pub struct OperationalGuards {
    pub max_run_len: u8,
    pub budget_cap: u64,
    /// Rate limit in parts per million. Canonical 0.05 = 50,000 ppm.
    pub rate_limit_ppm: u32,
    pub chronology: bool,
    pub conservation: bool,
    /// Conservation tolerance in parts per million. Canonical 0.001 = 1,000 ppm.
    pub conservation_tolerance_ppm: u32,
    pub legality: bool,
    #[serde(default)]
    pub exclusion_regions: BTreeSet<String>,
}

impl Default for OperationalGuards {
    fn default() -> Self {
        Self {
            max_run_len: 8,
            budget_cap: 2_000_000_000,
            rate_limit_ppm: 50_000,
            chronology: true,
            conservation: true,
            conservation_tolerance_ppm: 1_000,
            legality: true,
            exclusion_regions: BTreeSet::new(),
        }
    }
}

impl OperationalGuards {
    fn findings(&self) -> Vec<CrownFinding> {
        let mut findings = Vec::new();
        if self.max_run_len == 0 || self.max_run_len > 8 {
            findings.push(finding(
                "F5-GUARD-001",
                Severity::Critical,
                "max_run_len",
                "max_run_len must remain inside the Chatman Constant boundary 1..=8",
                "set max_run_len to a positive value no greater than eight",
            ));
        }
        if self.budget_cap == 0 {
            findings.push(finding(
                "F5-GUARD-002",
                Severity::Critical,
                "budget_cap",
                "budget_cap must be positive",
                "admit an explicit non-zero budget ceiling",
            ));
        }
        if self.rate_limit_ppm == 0 || self.rate_limit_ppm > 1_000_000 {
            findings.push(finding(
                "F5-GUARD-003",
                Severity::Critical,
                "rate_limit",
                "rate_limit must be in the interval (0, 1]",
                "admit a positive rate limit no greater than one million ppm",
            ));
        }
        if !self.chronology {
            findings.push(finding(
                "F5-GUARD-004",
                Severity::Critical,
                "chronology",
                "chronology guard is disabled",
                "enable ordering and minimum decision-lag enforcement",
            ));
        }
        if !self.conservation || self.conservation_tolerance_ppm == 0 {
            findings.push(finding(
                "F5-GUARD-005",
                Severity::Critical,
                "conservation",
                "conservation is disabled or has zero tolerance configuration",
                "enable conservation and admit a positive tolerance",
            ));
        }
        if !self.legality {
            findings.push(finding(
                "F5-GUARD-006",
                Severity::Critical,
                "legality",
                "legality guard is disabled",
                "enable hard exclusion-region enforcement",
            ));
        }
        findings
    }
}

/// Crown evidence and policy above one Fortune 5 assessment.
#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
pub struct LevelFiveCrownProgram {
    pub name: String,
    pub release_truths: BTreeMap<ReleaseTruth, CrownEvidence>,
    pub sla_governors: BTreeMap<SlaGovernor, CrownEvidence>,
    pub operational_guards: OperationalGuards,
    pub receipt_replay_verified: bool,
    pub direct_actuation_performed: bool,
}

/// Exact taxonomy/profile closure derived from the canonical catalog.
#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
pub struct TaxonomyProfileClosure {
    pub profile: String,
    pub dimensions: usize,
    pub controls: usize,
    pub obligations: usize,
    pub design_obligations: usize,
    pub operation_obligations: usize,
    pub falsifier_obligations: usize,
    pub unique_dimension_ids: bool,
    pub unique_control_ids: bool,
    pub unique_obligation_ids: bool,
    pub catalog_findings: usize,
}

impl TaxonomyProfileClosure {
    /// Derive closure directly from the canonical catalog.
    #[must_use]
    pub fn canonical() -> Self {
        let catalog = Fortune5Catalog::canonical();
        let dimension_ids: BTreeSet<_> = catalog
            .dimensions
            .iter()
            .map(|dimension| dimension.id.as_str())
            .collect();
        let control_ids: Vec<_> = catalog
            .dimensions
            .iter()
            .flat_map(|dimension| dimension.required_controls.iter())
            .collect();
        let unique_controls: BTreeSet<_> = control_ids.iter().copied().collect();
        let obligations: Vec<_> = catalog.obligations().collect();
        let obligation_ids: BTreeSet<_> = obligations
            .iter()
            .map(|obligation| obligation.id.as_str())
            .collect();
        Self {
            profile: catalog.profile,
            dimensions: catalog.dimensions.len(),
            controls: control_ids.len(),
            obligations: obligations.len(),
            design_obligations: obligations
                .iter()
                .filter(|obligation| obligation.kind == ProofKind::Design)
                .count(),
            operation_obligations: obligations
                .iter()
                .filter(|obligation| obligation.kind == ProofKind::Operation)
                .count(),
            falsifier_obligations: obligations
                .iter()
                .filter(|obligation| obligation.kind == ProofKind::Falsifier)
                .count(),
            unique_dimension_ids: dimension_ids.len() == 21,
            unique_control_ids: unique_controls.len() == control_ids.len(),
            unique_obligation_ids: obligation_ids.len() == obligations.len(),
            catalog_findings: catalog.validate().len(),
        }
    }

    fn findings(&self) -> Vec<CrownFinding> {
        let exact = self.dimensions == 21
            && self.controls == 99
            && self.obligations == 63
            && self.design_obligations == 21
            && self.operation_obligations == 21
            && self.falsifier_obligations == 21
            && self.unique_dimension_ids
            && self.unique_control_ids
            && self.unique_obligation_ids
            && self.catalog_findings == 0;
        if exact {
            Vec::new()
        } else {
            vec![finding(
                "F5-TAXONOMY-001",
                Severity::Critical,
                &self.profile,
                "taxonomy/profile closure is not exactly 21 dimensions, 99 controls, and 63 Design/Operation/Falsifier obligations",
                "repair the canonical profile before admitting crown evidence",
            )]
        }
    }
}

/// One crown refusal or warning.
#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
pub struct CrownFinding {
    pub code: String,
    pub severity: Severity,
    pub subject: String,
    pub message: String,
    pub remediation: String,
}

/// Receipted assessment of the complete Fortune 5 crown contract.
#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
pub struct LevelFiveCrownAssessment {
    pub program: String,
    pub assessment_receipt: String,
    pub taxonomy: TaxonomyProfileClosure,
    pub release_truths_alive: usize,
    pub sla_governors_alive: usize,
    pub operational_controls_alive: usize,
    pub structurally_ready: bool,
    pub promotion_ready: bool,
    pub synthetic: bool,
    pub findings: Vec<CrownFinding>,
    pub receipt_hash: String,
}

#[derive(Serialize)]
struct CrownReceiptBody<'a> {
    program: &'a str,
    assessment_receipt: &'a str,
    taxonomy: &'a TaxonomyProfileClosure,
    release_truths_alive: usize,
    sla_governors_alive: usize,
    operational_controls_alive: usize,
    structurally_ready: bool,
    promotion_ready: bool,
    synthetic: bool,
    findings: &'a [CrownFinding],
}

impl LevelFiveCrownAssessment {
    /// Assess the exact Fortune 5 crown above a canonical profile assessment.
    pub fn assess(
        assessment: &Fortune5Assessment,
        crown: &LevelFiveCrownProgram,
    ) -> Result<Self> {
        let taxonomy = TaxonomyProfileClosure::canonical();
        let mut findings = taxonomy.findings();

        if assessment.alive_dimensions != 21
            || assessment.passed_obligations != 63
            || assessment.total_obligations != 63
            || !assessment.level_five_ready
            || assessment.standing != Standing::Alive
        {
            findings.push(finding(
                "F5-CROWN-ASSESS-001",
                Severity::Critical,
                &assessment.program,
                "the underlying conjunctive 21/99/63 assessment is not ALIVE",
                "close every design, operation, and falsifier obligation before crown",
            ));
        }

        for truth in ReleaseTruth::all() {
            match crown.release_truths.get(&truth) {
                Some(evidence) => findings.extend(evidence.findings(&format!("{truth:?}"))),
                None => findings.push(finding(
                    "F5-TRUTH-001",
                    Severity::Critical,
                    &format!("{truth:?}"),
                    "required release truth has no evidence package",
                    "attach independently produced, approved, and verified release evidence",
                )),
            }
        }
        for governor in SlaGovernor::all() {
            match crown.sla_governors.get(&governor) {
                Some(evidence) => findings.extend(evidence.findings(&format!("{governor:?}"))),
                None => findings.push(finding(
                    "F5-SLA-001",
                    Severity::Critical,
                    &format!("{governor:?}"),
                    "required SLA governor has no evidence package",
                    "attach independently verified governor evidence",
                )),
            }
        }
        findings.extend(crown.operational_guards.findings());

        if !crown.receipt_replay_verified {
            findings.push(finding(
                "F5-REPLAY-001",
                Severity::Critical,
                &crown.name,
                "receipt replay has not been independently verified",
                "replay the complete evidence chain from the exact revision",
            ));
        }
        if crown.direct_actuation_performed {
            findings.push(finding(
                "F5-ACTUATION-001",
                Severity::Critical,
                &crown.name,
                "the architecture crown performed direct actuation",
                "manufacture a bounded BRCE intent instead of actuating",
            ));
        }

        findings.sort_by(|left, right| {
            (&left.severity, &left.code, &left.subject).cmp(&(
                &right.severity,
                &right.code,
                &right.subject,
            ))
        });
        let structurally_ready = findings
            .iter()
            .all(|item| item.severity < Severity::Error);
        let promotion_ready = structurally_ready && !assessment.synthetic;
        if assessment.synthetic {
            findings.push(finding(
                "F5-SYNTHETIC-001",
                Severity::Warning,
                &assessment.program,
                "synthetic evidence proves machinery only and cannot authorize promotion",
                "replace synthetic evidence with observed production evidence and reverify",
            ));
        }

        let release_truths_alive = crown
            .release_truths
            .values()
            .filter(|evidence| evidence.findings("release-truth").is_empty())
            .count();
        let sla_governors_alive = crown
            .sla_governors
            .values()
            .filter(|evidence| evidence.findings("sla-governor").is_empty())
            .count();
        let operational_controls_alive = if crown.operational_guards.findings().is_empty() {
            6
        } else {
            0
        };

        let mut result = Self {
            program: crown.name.clone(),
            assessment_receipt: assessment.receipt_hash.clone(),
            taxonomy,
            release_truths_alive,
            sla_governors_alive,
            operational_controls_alive,
            structurally_ready,
            promotion_ready,
            synthetic: assessment.synthetic,
            findings,
            receipt_hash: String::new(),
        };
        result.receipt_hash = deterministic_hash(
            "fortune5_level5_crown",
            &CrownReceiptBody {
                program: &result.program,
                assessment_receipt: &result.assessment_receipt,
                taxonomy: &result.taxonomy,
                release_truths_alive: result.release_truths_alive,
                sla_governors_alive: result.sla_governors_alive,
                operational_controls_alive: result.operational_controls_alive,
                structurally_ready: result.structurally_ready,
                promotion_ready: result.promotion_ready,
                synthetic: result.synthetic,
                findings: &result.findings,
            },
        )?;
        Ok(result)
    }
}

fn finding(
    code: &str,
    severity: Severity,
    subject: &str,
    message: &str,
    remediation: &str,
) -> CrownFinding {
    CrownFinding {
        code: code.to_string(),
        severity,
        subject: subject.to_string(),
        message: message.to_string(),
        remediation: remediation.to_string(),
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::fortune5::{ControlEvidence, Fortune5Policy, Fortune5Program};

    fn evidence(id: &str) -> CrownEvidence {
        CrownEvidence {
            evidence_id: id.to_string(),
            producer: format!("producer-{id}"),
            approver: format!("approver-{id}"),
            verifier: format!("verifier-{id}"),
            digest: format!("blake3:{id}"),
            artifacts: BTreeSet::from([format!("evidence/{id}.json")]),
            standing: Standing::Alive,
        }
    }

    fn complete_assessment(synthetic: bool) -> Fortune5Assessment {
        let catalog = Fortune5Catalog::canonical();
        let program = Fortune5Program {
            name: "fortune5-crown-test".to_string(),
            policy: Fortune5Policy::default(),
            evidence: catalog
                .obligations()
                .map(|obligation| ControlEvidence {
                    obligation_id: obligation.id.clone(),
                    standing: Standing::Alive,
                    producer: format!("producer-{}", obligation.id),
                    approver: format!("approver-{}", obligation.id),
                    verifier: format!("verifier-{}", obligation.id),
                    observed_at: "fixture-sequence-1".to_string(),
                    digest: format!("blake3:{}", obligation.id),
                    artifacts: obligation.required_evidence.clone(),
                })
                .collect(),
            synthetic,
        };
        Fortune5Assessment::assess(&program).expect("complete assessment")
    }

    fn complete_crown() -> LevelFiveCrownProgram {
        LevelFiveCrownProgram {
            name: "fortune5-crown-test".to_string(),
            release_truths: ReleaseTruth::all()
                .into_iter()
                .map(|truth| (truth, evidence(&format!("truth-{truth:?}"))))
                .collect(),
            sla_governors: SlaGovernor::all()
                .into_iter()
                .map(|governor| (governor, evidence(&format!("sla-{governor:?}"))))
                .collect(),
            operational_guards: OperationalGuards::default(),
            receipt_replay_verified: true,
            direct_actuation_performed: false,
        }
    }

    #[test]
    fn taxonomy_is_exactly_21_99_63() {
        let closure = TaxonomyProfileClosure::canonical();
        assert_eq!(closure.dimensions, 21);
        assert_eq!(closure.controls, 99);
        assert_eq!(closure.obligations, 63);
        assert_eq!(closure.design_obligations, 21);
        assert_eq!(closure.operation_obligations, 21);
        assert_eq!(closure.falsifier_obligations, 21);
        assert!(closure.findings().is_empty());
    }

    #[test]
    fn synthetic_level_five_proves_structure_but_not_promotion() {
        let result = LevelFiveCrownAssessment::assess(&complete_assessment(true), &complete_crown())
            .expect("crown assessment");
        assert!(result.structurally_ready, "{:?}", result.findings);
        assert!(!result.promotion_ready);
        assert_eq!(result.release_truths_alive, 6);
        assert_eq!(result.sla_governors_alive, 5);
        assert_eq!(result.operational_controls_alive, 6);
    }

    #[test]
    fn production_level_five_can_crown() {
        let result =
            LevelFiveCrownAssessment::assess(&complete_assessment(false), &complete_crown())
                .expect("crown assessment");
        assert!(result.structurally_ready, "{:?}", result.findings);
        assert!(result.promotion_ready);
    }

    #[test]
    fn missing_release_truth_refuses_crown() {
        let mut crown = complete_crown();
        crown
            .release_truths
            .remove(&ReleaseTruth::CryptographicReceipts);
        let result = LevelFiveCrownAssessment::assess(&complete_assessment(false), &crown)
            .expect("crown assessment");
        assert!(!result.structurally_ready);
        assert!(result
            .findings
            .iter()
            .any(|item| item.code == "F5-TRUTH-001"));
    }

    #[test]
    fn operational_guard_bypass_refuses_crown() {
        let mut crown = complete_crown();
        crown.operational_guards.max_run_len = 9;
        crown.operational_guards.legality = false;
        let result = LevelFiveCrownAssessment::assess(&complete_assessment(false), &crown)
            .expect("crown assessment");
        assert!(!result.structurally_ready);
        assert!(result
            .findings
            .iter()
            .any(|item| item.code == "F5-GUARD-001"));
        assert!(result
            .findings
            .iter()
            .any(|item| item.code == "F5-GUARD-006"));
    }

    #[test]
    fn direct_actuation_and_missing_replay_refuse_crown() {
        let mut crown = complete_crown();
        crown.direct_actuation_performed = true;
        crown.receipt_replay_verified = false;
        let result = LevelFiveCrownAssessment::assess(&complete_assessment(false), &crown)
            .expect("crown assessment");
        assert!(!result.structurally_ready);
        assert!(result
            .findings
            .iter()
            .any(|item| item.code == "F5-ACTUATION-001"));
        assert!(result
            .findings
            .iter()
            .any(|item| item.code == "F5-REPLAY-001"));
    }
}
