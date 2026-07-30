//! MAPE-K-style autonomic observation, diagnosis, and bounded intent planning.
//!
//! No function in this module performs actuation. The terminal output is an
//! [`ArchitectureIntent`] that must be admitted by BRCE or another explicit
//! broker before any external consequence occurs.

use std::collections::{BTreeMap, BTreeSet};

use serde::{Deserialize, Serialize};

use crate::{
    capacity::{CapacityEnvelope, CapacityLevel, CapacitySample},
    error::{ArchitectureError, Result},
    model::{LifecycleState, Severity, Standing},
    receipt::deterministic_hash,
    state::ArchitectureState,
};

/// Boundary stimulus admitted for one autonomic cycle.
#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
#[serde(tag = "kind", rename_all = "snake_case")]
pub enum Stimulus {
    /// New measured capacity evidence.
    Capacity {
        /// Observed sample.
        sample: CapacitySample,
    },
    /// An architecture asset changed.
    AssetChanged {
        /// Changed asset identifier.
        asset_id: String,
    },
    /// Generated or deployed state no longer matches its expected digest.
    DriftDetected {
        /// Drifted asset.
        asset_id: String,
        /// Expected digest.
        expected_hash: String,
        /// Observed digest.
        observed_hash: String,
    },
    /// A required architecture dependency is unavailable.
    DependencyUnavailable {
        /// Dependent asset.
        asset_id: String,
        /// Unavailable dependency.
        dependency_id: String,
    },
    /// A lifecycle deadline or externally approved target state arrived.
    LifecycleDeadline {
        /// Subject asset.
        asset_id: String,
        /// Requested lifecycle state.
        target: LifecycleState,
    },
    /// Evidence standing changed.
    StandingChanged {
        /// Subject asset.
        asset_id: String,
        /// Newly observed standing.
        standing: Standing,
    },
}

/// Diagnosis produced by analysis of an admitted stimulus.
#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
pub struct Diagnosis {
    /// Stable diagnosis code.
    pub code: String,
    /// Severity.
    pub severity: Severity,
    /// Primary subject.
    pub subject: String,
    /// Evidence-grounded rationale.
    pub rationale: String,
    /// Identified affected architecture assets.
    #[serde(default)]
    pub affected_assets: Vec<String>,
}

/// Kinds of bounded architecture intent.
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Serialize, Deserialize)]
#[serde(rename_all = "snake_case")]
pub enum IntentKind {
    /// Surface an evidence-backed operator warning.
    Warn,
    /// Select or manufacture a smaller operating profile.
    Reprofile,
    /// Re-run architecture and implementation verification.
    Revalidate,
    /// Prevent promotion until evidence is repaired.
    BlockPromotion,
    /// Rebuild a deterministic generated projection.
    RebuildProjection,
    /// Construct a migration and rollback work package.
    CreateMigrationPlan,
    /// Recalculate a dependency-closed transition plan.
    ReplanTransition,
    /// Submit an otherwise-complete request to an admitted broker.
    SubmitToBroker,
}

/// Declarative, capability-bounded request produced by autonomic planning.
#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
pub struct ArchitectureIntent {
    /// Deterministic identity derived from the intent body.
    pub intent_id: String,
    /// Requested kind of consequence.
    pub kind: IntentKind,
    /// Primary subject.
    pub subject: String,
    /// Other affected architecture assets.
    #[serde(default)]
    pub affected_assets: Vec<String>,
    /// Preconditions the broker or downstream workflow must prove.
    #[serde(default)]
    pub preconditions: BTreeSet<String>,
    /// Capabilities required from an admitted actuator.
    #[serde(default)]
    pub required_capabilities: BTreeSet<String>,
    /// Evidence expected after lawful execution.
    #[serde(default)]
    pub expected_evidence: BTreeSet<String>,
    /// Stable parameters for downstream interpretation.
    #[serde(default)]
    pub payload: BTreeMap<String, String>,
}

#[derive(Serialize)]
struct IntentBody<'a> {
    kind: IntentKind,
    subject: &'a str,
    affected_assets: &'a [String],
    preconditions: &'a BTreeSet<String>,
    required_capabilities: &'a BTreeSet<String>,
    expected_evidence: &'a BTreeSet<String>,
    payload: &'a BTreeMap<String, String>,
}

impl ArchitectureIntent {
    fn build(
        kind: IntentKind, subject: String, mut affected_assets: Vec<String>,
        preconditions: BTreeSet<String>, required_capabilities: BTreeSet<String>,
        expected_evidence: BTreeSet<String>, payload: BTreeMap<String, String>,
    ) -> Result<Self> {
        affected_assets.sort();
        affected_assets.dedup();
        let intent_id = deterministic_hash(
            "architecture_intent",
            &IntentBody {
                kind,
                subject: &subject,
                affected_assets: &affected_assets,
                preconditions: &preconditions,
                required_capabilities: &required_capabilities,
                expected_evidence: &expected_evidence,
                payload: &payload,
            },
        )?;
        Ok(Self {
            intent_id,
            kind,
            subject,
            affected_assets,
            preconditions,
            required_capabilities,
            expected_evidence,
            payload,
        })
    }
}

/// Receipted result of one monitor-analyze-plan cycle.
#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub struct AutonomicCycle {
    /// Deterministic cycle identity.
    pub cycle_id: String,
    /// Caller-supplied observation timestamp or sequence label.
    pub observed_at: String,
    /// Admitted stimuli.
    pub stimuli: Vec<Stimulus>,
    /// Evidence-grounded diagnoses.
    pub diagnoses: Vec<Diagnosis>,
    /// Bounded intents, never direct effects.
    pub intents: Vec<ArchitectureIntent>,
    /// Constitutional proof that this cycle performed no actuation.
    pub actuation_performed: bool,
    /// BLAKE3 receipt over the complete cycle body.
    pub receipt_hash: String,
}

#[derive(Serialize)]
struct CycleReceiptBody<'a> {
    observed_at: &'a str,
    stimuli: &'a [Stimulus],
    diagnoses: &'a [Diagnosis],
    intents: &'a [ArchitectureIntent],
    actuation_performed: bool,
}

/// Read-only architecture controller implementing Monitor, Analyze, and Plan.
pub struct AutonomicController<'a> {
    state: &'a ArchitectureState,
}

impl<'a> AutonomicController<'a> {
    /// Bind the controller to one admitted architecture state.
    #[must_use]
    pub const fn new(state: &'a ArchitectureState) -> Self {
        Self { state }
    }

    /// Run one bounded autonomic cycle and emit intents only.
    pub fn run_cycle(
        &self, observed_at: impl Into<String>, stimuli: Vec<Stimulus>,
    ) -> Result<AutonomicCycle> {
        if self.state.autonomic_policy.direct_actuation_allowed {
            return Err(ArchitectureError::DirectActuationForbidden);
        }

        let observed_at = observed_at.into();
        let mut diagnoses = Vec::new();
        let mut intents = Vec::new();

        if self.state.autonomic_policy.enabled {
            for stimulus in &stimuli {
                self.analyze_stimulus(stimulus, &mut diagnoses, &mut intents)?;
            }
        } else {
            diagnoses.push(Diagnosis {
                code: "EA-AUTO-000".to_string(),
                severity: Severity::Info,
                subject: self.state.name.clone(),
                rationale: "autonomic cycles are disabled by policy".to_string(),
                affected_assets: Vec::new(),
            });
        }

        let limit = self.state.autonomic_policy.max_intents_per_cycle;
        if intents.len() > limit {
            diagnoses.push(Diagnosis {
                code: "EA-AUTO-009".to_string(),
                severity: Severity::Warning,
                subject: self.state.name.clone(),
                rationale: format!(
                    "{} candidate intents exceeded the bounded cycle limit of {}",
                    intents.len(),
                    limit
                ),
                affected_assets: Vec::new(),
            });
            intents.truncate(limit);
        }

        diagnoses.sort_by(|left, right| {
            right
                .severity
                .cmp(&left.severity)
                .then(left.code.cmp(&right.code))
                .then(left.subject.cmp(&right.subject))
        });
        intents.sort_by(|left, right| {
            left.kind
                .cmp(&right.kind)
                .then(left.subject.cmp(&right.subject))
                .then(left.intent_id.cmp(&right.intent_id))
        });

        let receipt_hash = deterministic_hash(
            "autonomic_cycle",
            &CycleReceiptBody {
                observed_at: &observed_at,
                stimuli: &stimuli,
                diagnoses: &diagnoses,
                intents: &intents,
                actuation_performed: false,
            },
        )?;

        Ok(AutonomicCycle {
            cycle_id: receipt_hash.clone(),
            observed_at,
            stimuli,
            diagnoses,
            intents,
            actuation_performed: false,
            receipt_hash,
        })
    }

    fn analyze_stimulus(
        &self, stimulus: &Stimulus, diagnoses: &mut Vec<Diagnosis>,
        intents: &mut Vec<ArchitectureIntent>,
    ) -> Result<()> {
        match stimulus {
            Stimulus::Capacity { sample } => {
                let mut samples = self.state.capacity_samples.clone();
                samples.push(sample.clone());
                let envelope = CapacityEnvelope::analyze(&samples, &self.state.capacity_policy);
                let level = self.state.capacity_policy.classify(sample);
                match level {
                    CapacityLevel::Healthy => diagnoses.push(Diagnosis {
                        code: "EA-AUTO-101".to_string(),
                        severity: Severity::Info,
                        subject: sample.label.clone(),
                        rationale: "capacity sample remains inside the admitted envelope"
                            .to_string(),
                        affected_assets: Vec::new(),
                    }),
                    CapacityLevel::Warning => {
                        diagnoses.push(Diagnosis {
                            code: "EA-AUTO-102".to_string(),
                            severity: Severity::Warning,
                            subject: sample.label.clone(),
                            rationale: format!(
                                "capacity warning at {} workload units; first knee: {}",
                                sample.workload.units(),
                                envelope.first_knee.as_deref().unwrap_or("not observed")
                            ),
                            affected_assets: Vec::new(),
                        });
                        intents.push(Self::capacity_intent(IntentKind::Reprofile, sample, level)?);
                    }
                    CapacityLevel::Refuse => {
                        diagnoses.push(Diagnosis {
                            code: "EA-AUTO-103".to_string(),
                            severity: Severity::Critical,
                            subject: sample.label.clone(),
                            rationale: "capacity refusal threshold crossed".to_string(),
                            affected_assets: Vec::new(),
                        });
                        intents.push(Self::capacity_intent(
                            IntentKind::BlockPromotion,
                            sample,
                            level,
                        )?);
                        intents.push(Self::capacity_intent(IntentKind::Reprofile, sample, level)?);
                    }
                }
            }
            Stimulus::AssetChanged { asset_id } => {
                let impact = self.state.registry.impact_report(asset_id)?;
                diagnoses.push(Diagnosis {
                    code: "EA-AUTO-201".to_string(),
                    severity: Severity::Warning,
                    subject: asset_id.clone(),
                    rationale: format!(
                        "architecture change affects {} registered assets",
                        impact.affected.len()
                    ),
                    affected_assets: impact.affected.clone(),
                });
                intents.push(ArchitectureIntent::build(
                    IntentKind::Revalidate,
                    asset_id.clone(),
                    impact.ordered_revalidation,
                    BTreeSet::from(["dependency impact report admitted".to_string()]),
                    BTreeSet::from(["verification_runner".to_string()]),
                    BTreeSet::from([
                        "validation_report".to_string(),
                        "revalidation_receipt".to_string(),
                    ]),
                    BTreeMap::new(),
                )?);
            }
            Stimulus::DriftDetected {
                asset_id,
                expected_hash,
                observed_hash,
            } => {
                self.state.registry.asset(asset_id)?;
                diagnoses.push(Diagnosis {
                    code: "EA-AUTO-301".to_string(),
                    severity: Severity::Critical,
                    subject: asset_id.clone(),
                    rationale: "observed state does not match the admitted digest".to_string(),
                    affected_assets: vec![asset_id.clone()],
                });
                let payload = BTreeMap::from([
                    ("expected_hash".to_string(), expected_hash.clone()),
                    ("observed_hash".to_string(), observed_hash.clone()),
                ]);
                intents.push(ArchitectureIntent::build(
                    IntentKind::BlockPromotion,
                    asset_id.clone(),
                    vec![asset_id.clone()],
                    BTreeSet::from(["drift evidence admitted".to_string()]),
                    BTreeSet::from(["promotion_gate".to_string()]),
                    BTreeSet::from(["promotion_refusal_receipt".to_string()]),
                    payload.clone(),
                )?);
                intents.push(ArchitectureIntent::build(
                    IntentKind::RebuildProjection,
                    asset_id.clone(),
                    vec![asset_id.clone()],
                    BTreeSet::from([
                        "authoritative source hash verified".to_string(),
                        "write scope admitted".to_string(),
                    ]),
                    BTreeSet::from(["generated_artifact_writer".to_string()]),
                    BTreeSet::from(["artifact_digest".to_string(), "rebuild_receipt".to_string()]),
                    payload,
                )?);
            }
            Stimulus::DependencyUnavailable {
                asset_id,
                dependency_id,
            } => {
                let asset = self.state.registry.asset(asset_id)?;
                if !asset.dependencies.contains(dependency_id) {
                    return Err(ArchitectureError::DanglingDependency {
                        asset_id: asset_id.clone(),
                        dependency_id: dependency_id.clone(),
                    });
                }
                diagnoses.push(Diagnosis {
                    code: "EA-AUTO-401".to_string(),
                    severity: Severity::Critical,
                    subject: asset_id.clone(),
                    rationale: format!("required dependency `{dependency_id}` is unavailable"),
                    affected_assets: vec![asset_id.clone(), dependency_id.clone()],
                });
                intents.push(ArchitectureIntent::build(
                    IntentKind::BlockPromotion,
                    asset_id.clone(),
                    vec![asset_id.clone()],
                    BTreeSet::from(["dependency outage evidence admitted".to_string()]),
                    BTreeSet::from(["promotion_gate".to_string()]),
                    BTreeSet::from(["promotion_refusal_receipt".to_string()]),
                    BTreeMap::from([("dependency_id".to_string(), dependency_id.clone())]),
                )?);
                intents.push(ArchitectureIntent::build(
                    IntentKind::ReplanTransition,
                    asset_id.clone(),
                    vec![asset_id.clone(), dependency_id.clone()],
                    BTreeSet::from(["alternative building blocks identified".to_string()]),
                    BTreeSet::from(["architecture_planner".to_string()]),
                    BTreeSet::from(["plan_certificate".to_string()]),
                    BTreeMap::from([("unavailable_dependency".to_string(), dependency_id.clone())]),
                )?);
            }
            Stimulus::LifecycleDeadline { asset_id, target } => {
                let asset = self.state.registry.asset(asset_id)?;
                if asset.lifecycle.allows(*target) {
                    diagnoses.push(Diagnosis {
                        code: "EA-AUTO-502".to_string(),
                        severity: Severity::Warning,
                        subject: asset_id.clone(),
                        rationale: format!(
                            "lifecycle deadline requests {} -> {}",
                            asset.lifecycle, target
                        ),
                        affected_assets: vec![asset_id.clone()],
                    });
                    intents.push(ArchitectureIntent::build(
                        IntentKind::CreateMigrationPlan,
                        asset_id.clone(),
                        self.state.registry.impact_report(asset_id)?.affected,
                        BTreeSet::from([
                            "successor identified".to_string(),
                            "rollback policy admitted".to_string(),
                        ]),
                        BTreeSet::from(["architecture_planner".to_string()]),
                        BTreeSet::from([
                            "migration_plan".to_string(),
                            "plan_certificate".to_string(),
                        ]),
                        BTreeMap::from([
                            ("from".to_string(), asset.lifecycle.to_string()),
                            ("to".to_string(), target.to_string()),
                        ]),
                    )?);
                } else {
                    diagnoses.push(Diagnosis {
                        code: "EA-AUTO-501".to_string(),
                        severity: Severity::Error,
                        subject: asset_id.clone(),
                        rationale: format!(
                            "requested lifecycle transition {} -> {} is unlawful",
                            asset.lifecycle, target
                        ),
                        affected_assets: vec![asset_id.clone()],
                    });
                    intents.push(ArchitectureIntent::build(
                        IntentKind::BlockPromotion,
                        asset_id.clone(),
                        vec![asset_id.clone()],
                        BTreeSet::from(["lifecycle law evaluated".to_string()]),
                        BTreeSet::from(["lifecycle_gate".to_string()]),
                        BTreeSet::from(["transition_refusal_receipt".to_string()]),
                        BTreeMap::from([
                            ("from".to_string(), asset.lifecycle.to_string()),
                            ("to".to_string(), target.to_string()),
                        ]),
                    )?);
                }
            }
            Stimulus::StandingChanged { asset_id, standing } => {
                self.state.registry.asset(asset_id)?;
                let severity = match standing {
                    Standing::Alive => Severity::Info,
                    Standing::PartialAlive | Standing::Unknown => Severity::Warning,
                    Standing::Blocked
                    | Standing::BuildBroken
                    | Standing::Unsupported
                    | Standing::Retired => Severity::Error,
                };
                diagnoses.push(Diagnosis {
                    code: "EA-AUTO-601".to_string(),
                    severity,
                    subject: asset_id.clone(),
                    rationale: format!("evidence standing changed to {standing:?}"),
                    affected_assets: vec![asset_id.clone()],
                });
                if severity >= Severity::Error {
                    intents.push(ArchitectureIntent::build(
                        IntentKind::BlockPromotion,
                        asset_id.clone(),
                        self.state.registry.impact_report(asset_id)?.affected,
                        BTreeSet::from(["standing observation admitted".to_string()]),
                        BTreeSet::from(["promotion_gate".to_string()]),
                        BTreeSet::from(["promotion_refusal_receipt".to_string()]),
                        BTreeMap::from([("standing".to_string(), format!("{standing:?}"))]),
                    )?);
                }
            }
        }
        Ok(())
    }

    fn capacity_intent(
        kind: IntentKind, sample: &CapacitySample, level: CapacityLevel,
    ) -> Result<ArchitectureIntent> {
        ArchitectureIntent::build(
            kind,
            sample.label.clone(),
            Vec::new(),
            BTreeSet::from(["capacity sample admitted".to_string()]),
            BTreeSet::from([match kind {
                IntentKind::BlockPromotion => "promotion_gate".to_string(),
                _ => "architecture_profiler".to_string(),
            }]),
            BTreeSet::from([
                "capacity_report".to_string(),
                "capacity_receipt".to_string(),
            ]),
            BTreeMap::from([
                ("capacity_level".to_string(), format!("{level:?}")),
                ("elapsed_ms".to_string(), sample.elapsed_ms.to_string()),
                (
                    "peak_memory_bytes".to_string(),
                    sample.peak_memory_bytes.to_string(),
                ),
                (
                    "workload_units".to_string(),
                    sample.workload.units().to_string(),
                ),
            ]),
        )
    }
}
