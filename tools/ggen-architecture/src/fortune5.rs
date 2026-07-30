//! Fortune 5 operating-model, platform, governance, and proof obligations.
//!
//! The profile is deliberately conjunctive: Level 5 standing requires every
//! dimension and every proof obligation to pass. A synthetic fixture can prove
//! the assessment machinery, but it cannot establish production standing.

use std::collections::{BTreeMap, BTreeSet};

use serde::{Deserialize, Serialize};

use crate::{
    error::Result,
    model::{Severity, Standing},
    receipt::deterministic_hash,
};

/// Ross-Weill-Robertson and Chatman control domains used by the Fortune 5 profile.
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Serialize, Deserialize)]
#[serde(rename_all = "snake_case")]
pub enum Fortune5Domain {
    /// Enterprise operating-model choices and accountability.
    OperatingModel,
    /// Enterprise-architecture core-diagram content.
    CoreDiagram,
    /// Shared digitized-platform capabilities.
    DigitizedPlatform,
    /// Enterprise engagement and portfolio-governance mechanisms.
    EngagementModel,
    /// Realized enterprise value and adaptability.
    ValueRealization,
    /// Deterministic machinery, automation, autonomics, receipts, and replay.
    MachineryAutonomics,
}

/// Proof position required for every Fortune 5 dimension.
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Serialize, Deserialize)]
#[serde(rename_all = "snake_case")]
pub enum ProofKind {
    /// Authority, ownership, policy, architecture, and admitted design.
    Design,
    /// Runtime operation, service levels, evidence production, and control execution.
    Operation,
    /// Named negative fixture proving failure-closed refusal and replay.
    Falsifier,
}

/// One proof obligation inside a Fortune 5 dimension.
#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
pub struct ProofObligation {
    /// Stable obligation identifier.
    pub id: String,
    /// Proof position.
    pub kind: ProofKind,
    /// Required claim.
    pub description: String,
    /// Evidence artifact classes required from a conforming implementation.
    pub required_evidence: BTreeSet<String>,
}

/// One conjunctive Fortune 5 capability dimension.
#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
pub struct Fortune5Dimension {
    /// Stable dimension identifier.
    pub id: String,
    /// Parent control domain.
    pub domain: Fortune5Domain,
    /// Human-readable title.
    pub title: String,
    /// Required enterprise capability.
    pub capability: String,
    /// Machine-visible controls that must exist.
    pub required_controls: BTreeSet<String>,
    /// Design, operation, and falsifier obligations.
    pub obligations: Vec<ProofObligation>,
}

/// Canonical Level 5 profile.
#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
pub struct Fortune5Catalog {
    /// Catalog schema version.
    pub schema_version: u32,
    /// Stable profile name.
    pub profile: String,
    /// Exactly twenty-one conjunctive dimensions.
    pub dimensions: Vec<Fortune5Dimension>,
}

impl Fortune5Catalog {
    /// Manufacture the canonical twenty-one-dimension, sixty-three-obligation profile.
    #[must_use]
    pub fn canonical() -> Self {
        let dimensions = vec![
            dimension(
                "operating_model.standardization",
                Fortune5Domain::OperatingModel,
                "Enterprise standardization",
                "One governed operating grammar across products, platforms, and business units.",
                &[
                    "standards_registry",
                    "policy_engine",
                    "exception_authority",
                    "compatibility_contracts",
                ],
            ),
            dimension(
                "operating_model.integration",
                Fortune5Domain::OperatingModel,
                "Enterprise integration",
                "Shared identity, authoritative data, process, and event contracts across the enterprise.",
                &[
                    "enterprise_identity",
                    "authoritative_data_registry",
                    "integration_contracts",
                    "event_schema_governance",
                ],
            ),
            dimension(
                "operating_model.accountability",
                Fortune5Domain::OperatingModel,
                "Decision rights and accountability",
                "Explicit owners, decision rights, segregation of duties, and risk acceptance.",
                &[
                    "decision_rights_registry",
                    "segregation_of_duties",
                    "risk_acceptance",
                    "architecture_board",
                ],
            ),
            dimension(
                "core_diagram.capability_value_stream",
                Fortune5Domain::CoreDiagram,
                "Capability and value-stream core",
                "Capabilities, value streams, owners, products, and realization paths form one executable graph.",
                &[
                    "capability_map",
                    "value_stream_map",
                    "product_realization_graph",
                    "ownership_graph",
                ],
            ),
            dimension(
                "core_diagram.shared_data",
                Fortune5Domain::CoreDiagram,
                "Shared information core",
                "Canonical information domains, classifications, lineage, retention, and stewardship are explicit.",
                &[
                    "canonical_data_model",
                    "data_classification",
                    "lineage_registry",
                    "retention_policy",
                    "data_stewardship",
                ],
            ),
            dimension(
                "core_diagram.integration_fabric",
                Fortune5Domain::CoreDiagram,
                "Integration and automation fabric",
                "APIs, events, workflows, policies, and semantic contracts are versioned architecture assets.",
                &[
                    "api_catalog",
                    "event_catalog",
                    "workflow_registry",
                    "semantic_contract_registry",
                ],
            ),
            dimension(
                "core_diagram.external_channels",
                Fortune5Domain::CoreDiagram,
                "Customer and supplier channels",
                "External channels, trust boundaries, service commitments, and third-party dependencies are governed.",
                &[
                    "channel_catalog",
                    "trust_boundary_model",
                    "service_commitments",
                    "supplier_dependency_registry",
                ],
            ),
            dimension(
                "platform.shared_infrastructure",
                Fortune5Domain::DigitizedPlatform,
                "Shared infrastructure platform",
                "Compute, network, identity, secrets, observability, and policy enforcement are reusable governed services.",
                &[
                    "compute_platform",
                    "network_control_plane",
                    "identity_and_access",
                    "secrets_management",
                    "observability_platform",
                    "policy_enforcement_point",
                ],
            ),
            dimension(
                "platform.transaction_processing",
                Fortune5Domain::DigitizedPlatform,
                "Enterprise transaction systems",
                "Consequential transactions are atomic, idempotent, auditable, recoverable, and bounded by authorization.",
                &[
                    "transaction_boundary",
                    "idempotency",
                    "audit_log",
                    "authorization_boundary",
                    "compensation_and_rollback",
                ],
            ),
            dimension(
                "platform.authoritative_data",
                Fortune5Domain::DigitizedPlatform,
                "Authoritative enterprise data",
                "Systems of record, systems of evidence, lineage, quality, privacy, and retention are independently verifiable.",
                &[
                    "system_of_record_registry",
                    "system_of_evidence_registry",
                    "data_quality_controls",
                    "privacy_controls",
                    "retention_and_disposal",
                ],
            ),
            dimension(
                "platform.reusable_components",
                Fortune5Domain::DigitizedPlatform,
                "Reusable service and component platform",
                "Reusable components carry compatibility, ownership, lifecycle, vulnerability, and retirement contracts.",
                &[
                    "service_catalog",
                    "component_catalog",
                    "compatibility_policy",
                    "vulnerability_management",
                    "retirement_contracts",
                ],
            ),
            dimension(
                "engagement.enterprise_priorities",
                Fortune5Domain::EngagementModel,
                "Enterprise priorities",
                "Strategy, capability gaps, investment themes, and architecture outcomes are linked and ranked.",
                &[
                    "strategy_registry",
                    "capability_gap_portfolio",
                    "investment_prioritization",
                    "outcome_traceability",
                ],
            ),
            dimension(
                "engagement.portfolio_governance",
                Fortune5Domain::EngagementModel,
                "Portfolio and change governance",
                "Work enters through governed intake, architecture review, funding, sequencing, and promotion gates.",
                &[
                    "governed_intake",
                    "architecture_review",
                    "portfolio_sequencing",
                    "change_control",
                    "promotion_gate",
                ],
            ),
            dimension(
                "engagement.linking_mechanisms",
                Fortune5Domain::EngagementModel,
                "Linking mechanisms",
                "Architecture, product, security, data, reliability, finance, legal, and supplier decisions share typed contracts.",
                &[
                    "cross_function_decision_contracts",
                    "architecture_owners",
                    "security_owners",
                    "data_owners",
                    "reliability_owners",
                    "supplier_owners",
                ],
            ),
            dimension(
                "value.operational_excellence",
                Fortune5Domain::ValueRealization,
                "Operational excellence",
                "SLIs, SLOs, capacity, cost, quality, throughput, recovery, and control effectiveness are measured and governed.",
                &[
                    "sli_slo_registry",
                    "capacity_envelopes",
                    "cost_of_quality",
                    "recovery_objectives",
                    "control_effectiveness",
                ],
            ),
            dimension(
                "value.customer_intimacy",
                Fortune5Domain::ValueRealization,
                "Customer and stakeholder intimacy",
                "Customer outcomes, service commitments, consent, accessibility, and incident communication are traceable.",
                &[
                    "customer_outcome_registry",
                    "service_level_commitments",
                    "consent_and_privacy",
                    "accessibility_policy",
                    "incident_communications",
                ],
            ),
            dimension(
                "value.strategic_agility",
                Fortune5Domain::ValueRealization,
                "Strategic agility",
                "The enterprise can simulate, plan, migrate, rollback, and retire architecture without losing standing.",
                &[
                    "scenario_modeling",
                    "transition_planning",
                    "migration_factory",
                    "rollback_plans",
                    "retirement_factory",
                ],
            ),
            dimension(
                "machinery.deterministic_manufacture",
                Fortune5Domain::MachineryAutonomics,
                "Deterministic manufacturing machinery",
                "Admitted knowledge deterministically manufactures artifacts, tests, policies, plans, and evidence.",
                &[
                    "admitted_observation",
                    "deterministic_projection",
                    "content_addressed_identity",
                    "generated_artifact_law",
                ],
            ),
            dimension(
                "automation.policy_bounded",
                Fortune5Domain::MachineryAutonomics,
                "Policy-bounded automation",
                "Automation is capability-scoped, least-privilege, failure-closed, reversible, and separately authorized.",
                &[
                    "capability_tokens",
                    "least_privilege",
                    "failure_closed_automation",
                    "rollback_authority",
                    "kill_switch",
                ],
            ),
            dimension(
                "autonomics.convergent",
                Fortune5Domain::MachineryAutonomics,
                "Convergent autonomics",
                "Observe-analyze-plan loops have budgets, convergence criteria, escalation, and no direct actuation authority.",
                &[
                    "mape_k_controller",
                    "convergence_budget",
                    "oscillation_detection",
                    "human_escalation",
                    "zero_direct_actuation",
                ],
            ),
            dimension(
                "evidence.receipt_replay",
                Fortune5Domain::MachineryAutonomics,
                "Receipt, replay, and independent verification",
                "Every consequential promotion is receipted, replayable, independently verifiable, and retained through retirement.",
                &[
                    "append_only_evidence_ledger",
                    "receipt_verifier",
                    "replay_engine",
                    "evidence_retention",
                    "promotion_certificate",
                ],
            ),
        ];

        Self {
            schema_version: 1,
            profile: "fortune5-rwr-level5-v26.7.30".to_string(),
            dimensions,
        }
    }

    /// Return all proof obligations in deterministic catalog order.
    pub fn obligations(&self) -> impl Iterator<Item = &ProofObligation> {
        self.dimensions
            .iter()
            .flat_map(|dimension| dimension.obligations.iter())
    }

    /// Validate the fixed cardinality and identifier law of the canonical profile.
    #[must_use]
    pub fn validate(&self) -> Vec<Fortune5Finding> {
        let mut findings = Vec::new();
        if self.dimensions.len() != 21 {
            findings.push(finding(
                "F5-CAT-001",
                Severity::Critical,
                &self.profile,
                format!(
                    "Fortune 5 profile requires exactly 21 dimensions; found {}",
                    self.dimensions.len()
                ),
                "restore the canonical twenty-one-dimension profile",
            ));
        }

        let mut dimension_ids = BTreeSet::new();
        let mut obligation_ids = BTreeSet::new();
        for dimension in &self.dimensions {
            if !dimension_ids.insert(dimension.id.clone()) {
                findings.push(finding(
                    "F5-CAT-002",
                    Severity::Critical,
                    &dimension.id,
                    "duplicate Fortune 5 dimension identifier",
                    "assign one stable identifier to each dimension",
                ));
            }
            if dimension.required_controls.is_empty() {
                findings.push(finding(
                    "F5-CAT-003",
                    Severity::Error,
                    &dimension.id,
                    "dimension declares no required controls",
                    "bind the dimension to machine-visible enterprise controls",
                ));
            }
            if dimension.obligations.len() != 3 {
                findings.push(finding(
                    "F5-CAT-004",
                    Severity::Critical,
                    &dimension.id,
                    format!(
                        "dimension requires design, operation, and falsifier obligations; found {}",
                        dimension.obligations.len()
                    ),
                    "restore the three-position proof contract",
                ));
            }
            for obligation in &dimension.obligations {
                if !obligation_ids.insert(obligation.id.clone()) {
                    findings.push(finding(
                        "F5-CAT-005",
                        Severity::Critical,
                        &obligation.id,
                        "duplicate Fortune 5 proof obligation identifier",
                        "assign a globally unique proof obligation identifier",
                    ));
                }
                if obligation.required_evidence.is_empty() {
                    findings.push(finding(
                        "F5-CAT-006",
                        Severity::Error,
                        &obligation.id,
                        "proof obligation declares no required evidence classes",
                        "declare the evidence artifacts required to establish the claim",
                    ));
                }
            }
        }

        if self.obligations().count() != 63 {
            findings.push(finding(
                "F5-CAT-007",
                Severity::Critical,
                &self.profile,
                format!(
                    "Fortune 5 profile requires exactly 63 proof obligations; found {}",
                    self.obligations().count()
                ),
                "restore three proof obligations for all twenty-one dimensions",
            ));
        }

        sort_findings(&mut findings);
        findings
    }
}

/// Policy governing evidence admission for a Fortune 5 assessment.
#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
pub struct Fortune5Policy {
    /// Require a verifier distinct from the evidence producer.
    pub require_independent_verifier: bool,
    /// Require the operator and approver to be different principals.
    pub require_segregation_of_duties: bool,
    /// Require one or more attached artifacts for every proof record.
    pub require_artifacts: bool,
    /// Require all twenty-one dimensions for Level 5 standing.
    pub conjunctive_level_five: bool,
}

impl Default for Fortune5Policy {
    fn default() -> Self {
        Self {
            require_independent_verifier: true,
            require_segregation_of_duties: true,
            require_artifacts: true,
            conjunctive_level_five: true,
        }
    }
}

/// One admitted evidence record for one proof obligation.
#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
pub struct ControlEvidence {
    /// Proof obligation established by this record.
    pub obligation_id: String,
    /// Stable evidence identifier.
    pub evidence_id: String,
    /// Evidentiary standing.
    pub standing: Standing,
    /// Principal that produced the evidence.
    pub producer: String,
    /// Principal that approved the evidence for architecture standing.
    pub approver: String,
    /// Independent verifier identity.
    pub verifier: String,
    /// Deterministic observation timestamp or sequence label.
    pub observed_at: String,
    /// Content digest over the evidence package.
    pub digest: String,
    /// Attached evidence artifact identifiers.
    #[serde(default)]
    pub artifacts: BTreeSet<String>,
}

/// Input program for one Fortune 5 assessment.
#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
pub struct Fortune5Program {
    /// Program or enterprise identifier.
    pub name: String,
    /// Evidence-admission policy.
    #[serde(default)]
    pub policy: Fortune5Policy,
    /// Admitted proof records.
    #[serde(default)]
    pub evidence: Vec<ControlEvidence>,
    /// Explicit marker for test-only evidence.
    #[serde(default)]
    pub synthetic: bool,
}

/// One assessed Fortune 5 dimension.
#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
pub struct DimensionAssessment {
    /// Dimension identifier.
    pub dimension_id: String,
    /// Domain.
    pub domain: Fortune5Domain,
    /// Dimension standing.
    pub standing: Standing,
    /// Number of passing proof obligations.
    pub passed_obligations: usize,
    /// Total obligations for the dimension.
    pub total_obligations: usize,
    /// Obligations without admitted passing proof.
    pub unresolved_obligations: Vec<String>,
}

/// One Fortune 5 assessment finding.
#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
pub struct Fortune5Finding {
    /// Stable finding code.
    pub code: String,
    /// Severity.
    pub severity: Severity,
    /// Primary subject.
    pub subject: String,
    /// Explanation.
    pub message: String,
    /// Bounded remediation.
    pub remediation: String,
}

/// Receipted result of one Fortune 5 assessment.
#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
pub struct Fortune5Assessment {
    /// Program name.
    pub program: String,
    /// Catalog profile.
    pub profile: String,
    /// Calculated maturity level from zero through five.
    pub maturity_level: u8,
    /// Conjunctive Level 5 result.
    pub level_five_ready: bool,
    /// Aggregate evidentiary standing.
    pub standing: Standing,
    /// Number of dimensions with complete proof.
    pub alive_dimensions: usize,
    /// Number of passing proof obligations.
    pub passed_obligations: usize,
    /// Number of required proof obligations.
    pub total_obligations: usize,
    /// Per-dimension assessment.
    pub dimensions: Vec<DimensionAssessment>,
    /// Ordered findings.
    pub findings: Vec<Fortune5Finding>,
    /// Whether the admitted evidence is explicitly synthetic.
    pub synthetic: bool,
    /// BLAKE3 receipt over the complete assessment.
    pub receipt_hash: String,
}

#[derive(Serialize)]
struct AssessmentReceiptBody<'a> {
    program: &'a str,
    profile: &'a str,
    maturity_level: u8,
    level_five_ready: bool,
    standing: Standing,
    dimensions: &'a [DimensionAssessment],
    findings: &'a [Fortune5Finding],
    synthetic: bool,
}

impl Fortune5Assessment {
    /// Assess one evidence program against the canonical Level 5 profile.
    pub fn assess(program: &Fortune5Program) -> Result<Self> {
        let catalog = Fortune5Catalog::canonical();
        let mut findings = catalog.validate();
        let mut evidence_by_obligation = BTreeMap::<String, &ControlEvidence>::new();
        let known_obligations: BTreeSet<String> = catalog
            .obligations()
            .map(|obligation| obligation.id.clone())
            .collect();

        for evidence in &program.evidence {
            if !known_obligations.contains(&evidence.obligation_id) {
                findings.push(finding(
                    "F5-EVID-000",
                    Severity::Warning,
                    &evidence.obligation_id,
                    "evidence references an unknown Fortune 5 obligation",
                    "bind the record to a canonical proof obligation",
                ));
                continue;
            }
            if evidence_by_obligation
                .insert(evidence.obligation_id.clone(), evidence)
                .is_some()
            {
                findings.push(finding(
                    "F5-EVID-007",
                    Severity::Error,
                    &evidence.obligation_id,
                    "multiple evidence records claim the same proof obligation",
                    "admit one authoritative evidence package or model explicit supersession",
                ));
            }
        }

        let mut dimensions = Vec::with_capacity(catalog.dimensions.len());
        let mut passed_obligations = 0_usize;

        for dimension in &catalog.dimensions {
            let mut passed = 0_usize;
            let mut unresolved = Vec::new();
            let mut has_refusal = false;

            for obligation in &dimension.obligations {
                match evidence_by_obligation.get(&obligation.id) {
                    None => {
                        unresolved.push(obligation.id.clone());
                        findings.push(finding(
                            "F5-EVID-001",
                            Severity::Warning,
                            &obligation.id,
                            "required Fortune 5 proof is missing",
                            "run the Gall checkpoint and attach design, operation, or falsifier evidence",
                        ));
                    }
                    Some(evidence) => {
                        let evidence_findings =
                            validate_evidence(evidence, obligation, &program.policy);
                        if evidence_findings.is_empty() {
                            passed = passed.saturating_add(1);
                            passed_obligations = passed_obligations.saturating_add(1);
                        } else {
                            has_refusal = has_refusal
                                || evidence_findings
                                    .iter()
                                    .any(|item| item.severity >= Severity::Error);
                            unresolved.push(obligation.id.clone());
                            findings.extend(evidence_findings);
                        }
                    }
                }
            }

            let standing = if passed == dimension.obligations.len() {
                Standing::Alive
            } else if has_refusal {
                Standing::Blocked
            } else if passed > 0 {
                Standing::PartialAlive
            } else {
                Standing::Unknown
            };
            dimensions.push(DimensionAssessment {
                dimension_id: dimension.id.clone(),
                domain: dimension.domain,
                standing,
                passed_obligations: passed,
                total_obligations: dimension.obligations.len(),
                unresolved_obligations: unresolved,
            });
        }

        let alive_dimensions = dimensions
            .iter()
            .filter(|dimension| dimension.standing == Standing::Alive)
            .count();
        let maturity_level = maturity_level(alive_dimensions);
        let level_five_ready = if program.policy.conjunctive_level_five {
            alive_dimensions == catalog.dimensions.len()
        } else {
            maturity_level == 5
        };
        let standing = if level_five_ready {
            Standing::Alive
        } else if findings.iter().any(|item| item.severity >= Severity::Error) {
            Standing::Blocked
        } else if passed_obligations > 0 {
            Standing::PartialAlive
        } else {
            Standing::Unknown
        };

        sort_findings(&mut findings);
        let profile = catalog.profile;
        let receipt_hash = deterministic_hash(
            "fortune5_assessment",
            &AssessmentReceiptBody {
                program: &program.name,
                profile: &profile,
                maturity_level,
                level_five_ready,
                standing,
                dimensions: &dimensions,
                findings: &findings,
                synthetic: program.synthetic,
            },
        )?;

        Ok(Self {
            program: program.name.clone(),
            profile,
            maturity_level,
            level_five_ready,
            standing,
            alive_dimensions,
            passed_obligations,
            total_obligations: 63,
            dimensions,
            findings,
            synthetic: program.synthetic,
            receipt_hash,
        })
    }

    /// Render a stable operator-facing assessment.
    #[must_use]
    pub fn render_text(&self) -> String {
        format!(
            "Fortune 5 assessment: {:?}\nlevel: {}\nlevel_five_ready: {}\ndimensions: {}/21\nobligations: {}/{}\nsynthetic: {}\nreceipt: {}\n",
            self.standing,
            self.maturity_level,
            self.level_five_ready,
            self.alive_dimensions,
            self.passed_obligations,
            self.total_obligations,
            self.synthetic,
            self.receipt_hash
        )
    }
}

/// Bounded Fortune 5 autonomic intent kind.
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Serialize, Deserialize)]
#[serde(rename_all = "snake_case")]
pub enum Fortune5IntentKind {
    /// Prevent promotion until the Level 5 contract is satisfied.
    BlockPromotion,
    /// Manufacture missing design, operation, or falsifier evidence.
    ManufactureEvidence,
    /// Repair a segregation-of-duties violation.
    RepairSegregation,
    /// Re-run independent verification over a complete evidence program.
    Reverify,
    /// Submit a production evidence package to an admitted execution broker.
    SubmitPromotion,
}

/// Capability-bounded intent emitted from a Fortune 5 assessment.
#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
pub struct Fortune5Intent {
    /// Deterministic intent identity.
    pub intent_id: String,
    /// Requested bounded consequence.
    pub kind: Fortune5IntentKind,
    /// Primary assessment or control subject.
    pub subject: String,
    /// Preconditions required before downstream execution.
    pub preconditions: BTreeSet<String>,
    /// Capabilities required from an admitted actuator.
    pub required_capabilities: BTreeSet<String>,
    /// Evidence expected after lawful execution.
    pub expected_evidence: BTreeSet<String>,
    /// Stable parameters for the downstream broker.
    pub payload: BTreeMap<String, String>,
}

#[derive(Serialize)]
struct Fortune5IntentBody<'a> {
    kind: Fortune5IntentKind,
    subject: &'a str,
    preconditions: &'a BTreeSet<String>,
    required_capabilities: &'a BTreeSet<String>,
    expected_evidence: &'a BTreeSet<String>,
    payload: &'a BTreeMap<String, String>,
}

/// Receipted Fortune 5 autonomic plan that performs no direct actuation.
#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
pub struct Fortune5AutonomicPlan {
    /// Assessment receipt used as admitted observation.
    pub assessment_receipt: String,
    /// Capability-bounded intents.
    pub intents: Vec<Fortune5Intent>,
    /// Constitutional proof that planning performed no actuation.
    pub actuation_performed: bool,
    /// BLAKE3 receipt over the plan.
    pub receipt_hash: String,
}

#[derive(Serialize)]
struct Fortune5PlanReceiptBody<'a> {
    assessment_receipt: &'a str,
    intents: &'a [Fortune5Intent],
    actuation_performed: bool,
}

impl Fortune5AutonomicPlan {
    /// Plan bounded remediation or promotion intents from one assessment.
    pub fn plan(assessment: &Fortune5Assessment) -> Result<Self> {
        let mut intents = Vec::new();

        if assessment.level_five_ready && !assessment.synthetic {
            intents.push(build_intent(
                Fortune5IntentKind::SubmitPromotion,
                &assessment.program,
                &["fortune5 assessment receipt independently verified"],
                &["brce_admission", "promotion_authority"],
                &["execution_grant", "promotion_receipt"],
                &[
                    ("assessment_receipt", assessment.receipt_hash.as_str()),
                    ("maturity_level", "5"),
                ],
            )?);
        } else if assessment.level_five_ready {
            intents.push(build_intent(
                Fortune5IntentKind::Reverify,
                &assessment.program,
                &["synthetic proof is explicitly non-authoritative"],
                &["production_evidence_collector", "independent_verifier"],
                &["production_fortune5_assessment"],
                &[
                    ("assessment_receipt", assessment.receipt_hash.as_str()),
                    ("reason", "synthetic evidence cannot authorize promotion"),
                ],
            )?);
        } else {
            let maturity_level = assessment.maturity_level.to_string();
            let unresolved_obligations = assessment
                .total_obligations
                .saturating_sub(assessment.passed_obligations)
                .to_string();
            intents.push(build_intent(
                Fortune5IntentKind::BlockPromotion,
                &assessment.program,
                &["Fortune 5 assessment admitted"],
                &["promotion_gate"],
                &["promotion_refusal_receipt"],
                &[
                    ("assessment_receipt", assessment.receipt_hash.as_str()),
                    ("maturity_level", maturity_level.as_str()),
                ],
            )?);
            intents.push(build_intent(
                Fortune5IntentKind::ManufactureEvidence,
                &assessment.program,
                &["unresolved obligations enumerated"],
                &["gall_checkpoint_planner", "evidence_manufacturer"],
                &[
                    "design_evidence",
                    "operating_receipt",
                    "negative_fixture",
                    "independent_replay",
                ],
                &[
                    ("assessment_receipt", assessment.receipt_hash.as_str()),
                    ("unresolved_obligations", unresolved_obligations.as_str()),
                ],
            )?);
        }

        if assessment
            .findings
            .iter()
            .any(|finding| finding.code == "F5-SOD-001")
        {
            intents.push(build_intent(
                Fortune5IntentKind::RepairSegregation,
                &assessment.program,
                &["segregation-of-duties violation receipted"],
                &["identity_governance", "decision_rights_authority"],
                &["updated_role_binding", "segregation_verification"],
                &[("assessment_receipt", assessment.receipt_hash.as_str())],
            )?);
        }

        intents.sort_by(|left, right| {
            left.kind
                .cmp(&right.kind)
                .then(left.subject.cmp(&right.subject))
                .then(left.intent_id.cmp(&right.intent_id))
        });

        let receipt_hash = deterministic_hash(
            "fortune5_autonomic_plan",
            &Fortune5PlanReceiptBody {
                assessment_receipt: &assessment.receipt_hash,
                intents: &intents,
                actuation_performed: false,
            },
        )?;

        Ok(Self {
            assessment_receipt: assessment.receipt_hash.clone(),
            intents,
            actuation_performed: false,
            receipt_hash,
        })
    }
}

fn build_intent(
    kind: Fortune5IntentKind, subject: &str, preconditions: &[&str],
    required_capabilities: &[&str], expected_evidence: &[&str], payload: &[(&str, &str)],
) -> Result<Fortune5Intent> {
    let preconditions = preconditions
        .iter()
        .map(|item| (*item).to_string())
        .collect::<BTreeSet<_>>();
    let required_capabilities = required_capabilities
        .iter()
        .map(|item| (*item).to_string())
        .collect::<BTreeSet<_>>();
    let expected_evidence = expected_evidence
        .iter()
        .map(|item| (*item).to_string())
        .collect::<BTreeSet<_>>();
    let payload = payload
        .iter()
        .map(|(key, value)| ((*key).to_string(), (*value).to_string()))
        .collect::<BTreeMap<_, _>>();
    let intent_id = deterministic_hash(
        "fortune5_intent",
        &Fortune5IntentBody {
            kind,
            subject,
            preconditions: &preconditions,
            required_capabilities: &required_capabilities,
            expected_evidence: &expected_evidence,
            payload: &payload,
        },
    )?;

    Ok(Fortune5Intent {
        intent_id,
        kind,
        subject: subject.to_string(),
        preconditions,
        required_capabilities,
        expected_evidence,
        payload,
    })
}

fn dimension(
    id: &str, domain: Fortune5Domain, title: &str, capability: &str, controls: &[&str],
) -> Fortune5Dimension {
    let required_controls = controls
        .iter()
        .map(|control| (*control).to_string())
        .collect();
    let obligations = vec![
        obligation(
            id,
            ProofKind::Design,
            "Prove admitted authority, ownership, policy, architecture, interfaces, and lifecycle.",
            &[
                "architecture_descriptor",
                "owner_attestation",
                "policy_decision",
                "design_verification",
            ],
        ),
        obligation(
            id,
            ProofKind::Operation,
            "Prove bounded operation, service levels, segregation of duties, observability, and recovery.",
            &[
                "operating_receipt",
                "slo_evidence",
                "control_effectiveness",
                "recovery_evidence",
            ],
        ),
        obligation(
            id,
            ProofKind::Falsifier,
            "Prove a named negative fixture refuses the invalid path and replays the refusal.",
            &["negative_fixture", "refusal_receipt", "independent_replay"],
        ),
    ];

    Fortune5Dimension {
        id: id.to_string(),
        domain,
        title: title.to_string(),
        capability: capability.to_string(),
        required_controls,
        obligations,
    }
}

fn obligation(
    dimension_id: &str, kind: ProofKind, description: &str, evidence: &[&str],
) -> ProofObligation {
    let suffix = match kind {
        ProofKind::Design => "design",
        ProofKind::Operation => "operation",
        ProofKind::Falsifier => "falsifier",
    };
    ProofObligation {
        id: format!("{dimension_id}.{suffix}"),
        kind,
        description: description.to_string(),
        required_evidence: evidence.iter().map(|item| (*item).to_string()).collect(),
    }
}

fn validate_evidence(
    evidence: &ControlEvidence, obligation: &ProofObligation, policy: &Fortune5Policy,
) -> Vec<Fortune5Finding> {
    let mut findings = Vec::new();

    if evidence.evidence_id.trim().is_empty() {
        findings.push(finding(
            "F5-EVID-008",
            Severity::Error,
            &obligation.id,
            "evidence identifier is empty",
            "assign a stable evidence identity",
        ));
    }
    if evidence.standing != Standing::Alive {
        findings.push(finding(
            "F5-EVID-002",
            Severity::Error,
            &obligation.id,
            format!("evidence standing is {:?}", evidence.standing),
            "repair the proof package until the bounded claim is ALIVE",
        ));
    }
    if evidence.digest.trim().is_empty() {
        findings.push(finding(
            "F5-EVID-003",
            Severity::Error,
            &obligation.id,
            "evidence package has no content digest",
            "bind the evidence package to a recomputable content digest",
        ));
    }
    if evidence.observed_at.trim().is_empty() {
        findings.push(finding(
            "F5-EVID-006",
            Severity::Error,
            &obligation.id,
            "evidence has no deterministic observation label",
            "record a timestamp or monotonic sequence label",
        ));
    }
    if policy.require_artifacts && evidence.artifacts.is_empty() {
        findings.push(finding(
            "F5-EVID-004",
            Severity::Error,
            &obligation.id,
            "evidence package contains no attached artifacts",
            "attach the required proof objects, logs, fixtures, and receipts",
        ));
    }
    if policy.require_independent_verifier
        && (evidence.verifier.trim().is_empty() || evidence.verifier == evidence.producer)
    {
        findings.push(finding(
            "F5-EVID-005",
            Severity::Error,
            &obligation.id,
            "evidence lacks an independent verifier",
            "verify the package using a principal and implementation independent from the producer",
        ));
    }
    if policy.require_segregation_of_duties
        && (evidence.producer.trim().is_empty()
            || evidence.approver.trim().is_empty()
            || evidence.producer == evidence.approver)
    {
        findings.push(finding(
            "F5-SOD-001",
            Severity::Critical,
            &obligation.id,
            "evidence violates producer-approver segregation of duties",
            "assign distinct production and approval principals",
        ));
    }

    let missing_classes = obligation
        .required_evidence
        .difference(&evidence.artifacts)
        .cloned()
        .collect::<Vec<_>>();
    if !missing_classes.is_empty() {
        findings.push(finding(
            "F5-EVID-009",
            Severity::Error,
            &obligation.id,
            format!(
                "evidence package is missing required classes: {}",
                missing_classes.join(", ")
            ),
            "attach every evidence class declared by the proof obligation",
        ));
    }

    findings
}

const fn maturity_level(alive_dimensions: usize) -> u8 {
    match alive_dimensions {
        21 => 5,
        17..=20 => 4,
        13..=16 => 3,
        9..=12 => 2,
        1..=8 => 1,
        _ => 0,
    }
}

fn finding(
    code: &str, severity: Severity, subject: &str, message: impl Into<String>, remediation: &str,
) -> Fortune5Finding {
    Fortune5Finding {
        code: code.to_string(),
        severity,
        subject: subject.to_string(),
        message: message.into(),
        remediation: remediation.to_string(),
    }
}

fn sort_findings(findings: &mut [Fortune5Finding]) {
    findings.sort_by(|left, right| {
        right
            .severity
            .cmp(&left.severity)
            .then(left.code.cmp(&right.code))
            .then(left.subject.cmp(&right.subject))
    });
}
