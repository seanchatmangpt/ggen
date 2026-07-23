//! Receipted 1000x phase-shift autonomics.
//!
//! The loop implements a bounded MAPE-K-style controller without allowing the
//! observer/executor to adjudicate its own evidence:
//!
//! `Sensed -> Classified -> Proposed -> Authorized -> Receipted`
//!
//! Cargo-cicd is the reference observer and executor. Praxis is the independent
//! judge. TCPS owns the phase law and constructs the final receipt atomically.

use serde::Serialize;
use std::{fmt, marker::PhantomData};

/// Exact qualitative-regime boundary. Below this value, ordinary standard work
/// remains admissible. At or above it, the production system must be recomposed.
pub const PHASE_SHIFT_MULTIPLIER: u64 = 1_000;
/// The phase-shift response is deliberately finite and reviewable.
pub const MAX_AUTONOMIC_ACTIONS: usize = 5;
/// Reference safety fence: more than one percent failed observations stops the loop.
pub const MAX_ERROR_BASIS_POINTS: u16 = 100;

#[derive(Clone, Copy, Debug, PartialEq, Eq, Serialize)]
pub enum PhaseRegime {
    Continuous,
    PhaseShift1000x,
}

impl PhaseRegime {
    const fn as_str(self) -> &'static str {
        match self {
            Self::Continuous => "continuous",
            Self::PhaseShift1000x => "phase-shift-1000x",
        }
    }
}

#[derive(Clone, Copy, Debug, PartialEq, Eq, Serialize)]
pub enum AutonomicAction {
    PreserveStandardWork,
    PartitionDemand,
    EnablePullShards,
    RequireIndependentOracle,
    RequireProofCarryingReceipts,
    StageCanaryRollback,
}

impl AutonomicAction {
    const fn as_str(self) -> &'static str {
        match self {
            Self::PreserveStandardWork => "preserve-standard-work",
            Self::PartitionDemand => "partition-demand",
            Self::EnablePullShards => "enable-pull-shards",
            Self::RequireIndependentOracle => "require-independent-oracle",
            Self::RequireProofCarryingReceipts => "require-proof-carrying-receipts",
            Self::StageCanaryRollback => "stage-canary-rollback",
        }
    }
}

const PHASE_SHIFT_ACTIONS: [AutonomicAction; MAX_AUTONOMIC_ACTIONS] = [
    AutonomicAction::PartitionDemand,
    AutonomicAction::EnablePullShards,
    AutonomicAction::RequireIndependentOracle,
    AutonomicAction::RequireProofCarryingReceipts,
    AutonomicAction::StageCanaryRollback,
];

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct AutonomicPolicy {
    threshold_multiplier: u64,
    max_error_basis_points: u16,
    max_actions: usize,
    knowledge_digest: String,
}

impl AutonomicPolicy {
    pub fn phase_shift_1000x(knowledge_digest: impl Into<String>) -> Result<Self, AutonomicRefusal> {
        let knowledge_digest = knowledge_digest.into();
        if knowledge_digest.is_empty() {
            return Err(AutonomicRefusal::MissingKnowledge);
        }
        Ok(Self {
            threshold_multiplier: PHASE_SHIFT_MULTIPLIER,
            max_error_basis_points: MAX_ERROR_BASIS_POINTS,
            max_actions: MAX_AUTONOMIC_ACTIONS,
            knowledge_digest,
        })
    }

    #[must_use]
    pub const fn threshold_multiplier(&self) -> u64 {
        self.threshold_multiplier
    }

    #[must_use]
    pub fn knowledge_digest(&self) -> &str {
        &self.knowledge_digest
    }
}

/// Opaque machine evidence. It cannot be cloned, copied, deserialized, or
/// directly constructed by a consumer.
#[derive(Debug, PartialEq, Eq, Serialize)]
pub struct AutonomicEvidence {
    cycle_id: String,
    baseline_units: u64,
    observed_units: u64,
    error_basis_points: u16,
    source_digest: String,
    observer: String,
    evidence_digest: String,
}

impl AutonomicEvidence {
    #[must_use]
    pub const fn baseline_units(&self) -> u64 {
        self.baseline_units
    }

    #[must_use]
    pub const fn observed_units(&self) -> u64 {
        self.observed_units
    }

    #[must_use]
    pub fn evidence_digest(&self) -> &str {
        &self.evidence_digest
    }
}

/// Opaque affine adaptation proposal.
#[derive(Debug, PartialEq, Eq, Serialize)]
pub struct AutonomicPlan {
    cycle_id: String,
    regime: PhaseRegime,
    actions: Vec<AutonomicAction>,
    observer: String,
    workcell: String,
    evidence_digest: String,
    knowledge_digest: String,
    plan_digest: String,
}

impl AutonomicPlan {
    #[must_use]
    pub const fn regime(&self) -> PhaseRegime {
        self.regime
    }

    #[must_use]
    pub fn actions(&self) -> &[AutonomicAction] {
        &self.actions
    }

    #[must_use]
    pub fn plan_digest(&self) -> &str {
        &self.plan_digest
    }
}

/// Opaque affine capability manufactured only by the Praxis reference judge.
#[derive(Debug, PartialEq, Eq)]
pub struct AutonomicAuthorization {
    plan_digest: String,
    knowledge_digest: String,
    judge: String,
    approver: String,
    authorization_digest: String,
}

#[derive(Clone, Debug, PartialEq, Eq, Serialize)]
pub struct AutonomicReceipt {
    cycle_id: String,
    regime: PhaseRegime,
    evidence_digest: String,
    plan_digest: String,
    authorization_digest: String,
    adaptation_digest: String,
    observer: String,
    judge: String,
    executor: String,
    receipt_digest: String,
}

impl AutonomicReceipt {
    #[must_use]
    pub const fn regime(&self) -> PhaseRegime {
        self.regime
    }

    #[must_use]
    pub fn receipt_digest(&self) -> &str {
        &self.receipt_digest
    }

    #[must_use]
    pub fn judge(&self) -> &str {
        &self.judge
    }
}

#[derive(Clone, Debug, PartialEq, Eq, Serialize)]
pub struct AutonomicAndon {
    cycle_id: String,
    reason: String,
}

impl AutonomicAndon {
    #[must_use]
    pub fn reason(&self) -> &str {
        &self.reason
    }
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub enum AutonomicRefusal {
    ZeroBaseline,
    InvalidErrorRate,
    UnsafeObservation,
    EmptySourceDigest,
    MissingKnowledge,
    TooManyActions,
    InvalidPlan,
    EmptyActor,
    SelfAdjudication,
    AuthorizationMismatch,
    WorkcellMismatch,
}

impl fmt::Display for AutonomicRefusal {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.write_str(match self {
            Self::ZeroBaseline => "phase-shift classification requires a non-zero baseline",
            Self::InvalidErrorRate => "error basis points must be at most 10000",
            Self::UnsafeObservation => "observation exceeds the autonomic safety fence",
            Self::EmptySourceDigest => "autonomic evidence requires a source digest",
            Self::MissingKnowledge => "autonomic policy requires an admitted knowledge digest",
            Self::TooManyActions => "autonomic plan exceeds its bounded action budget",
            Self::InvalidPlan => "autonomic plan does not match the classified regime",
            Self::EmptyActor => "autonomic actors and approvers must be named",
            Self::SelfAdjudication => "worker and judge must be distinct authorities",
            Self::AuthorizationMismatch => "autonomic authorization does not bind the plan",
            Self::WorkcellMismatch => "autonomic executor does not match the authorized workcell",
        })
    }
}

impl std::error::Error for AutonomicRefusal {}

#[derive(Debug)]
pub struct Sensed;
#[derive(Debug)]
pub struct Classified;
#[derive(Debug)]
pub struct Proposed;
#[derive(Debug)]
pub struct Authorized;
#[derive(Debug)]
pub struct Receipted;
#[derive(Debug)]
pub struct Stopped;

#[derive(Debug)]
pub struct AutonomicLoop<State> {
    evidence: AutonomicEvidence,
    regime: Option<PhaseRegime>,
    plan: Option<AutonomicPlan>,
    authorization: Option<AutonomicAuthorization>,
    receipt: Option<AutonomicReceipt>,
    andon: Option<AutonomicAndon>,
    state: PhantomData<State>,
}

#[derive(Clone, Debug)]
pub struct ReferenceAutonomicCargoCicd {
    actor: String,
    fail_execution: bool,
}

impl ReferenceAutonomicCargoCicd {
    #[must_use]
    pub fn green() -> Self {
        Self {
            actor: "cargo-cicd".into(),
            fail_execution: false,
        }
    }

    #[must_use]
    pub fn failing() -> Self {
        Self {
            actor: "cargo-cicd".into(),
            fail_execution: true,
        }
    }

    pub fn observe(
        &self,
        cycle_id: impl Into<String>,
        baseline_units: u64,
        observed_units: u64,
        error_basis_points: u16,
        source_digest: impl Into<String>,
    ) -> Result<AutonomicLoop<Sensed>, AutonomicRefusal> {
        if baseline_units == 0 {
            return Err(AutonomicRefusal::ZeroBaseline);
        }
        if error_basis_points > 10_000 {
            return Err(AutonomicRefusal::InvalidErrorRate);
        }
        let cycle_id = cycle_id.into();
        let source_digest = source_digest.into();
        if source_digest.is_empty() {
            return Err(AutonomicRefusal::EmptySourceDigest);
        }
        let evidence_digest = digest(&[
            &cycle_id,
            &baseline_units.to_string(),
            &observed_units.to_string(),
            &error_basis_points.to_string(),
            &source_digest,
            &self.actor,
            "autonomic-observation",
        ]);
        Ok(AutonomicLoop {
            evidence: AutonomicEvidence {
                cycle_id,
                baseline_units,
                observed_units,
                error_basis_points,
                source_digest,
                observer: self.actor.clone(),
                evidence_digest,
            },
            regime: None,
            plan: None,
            authorization: None,
            receipt: None,
            andon: None,
            state: PhantomData,
        })
    }

    fn apply(&mut self, plan: &AutonomicPlan) -> Result<String, AutonomicAndon> {
        if self.fail_execution {
            self.fail_execution = false;
            return Err(AutonomicAndon {
                cycle_id: plan.cycle_id.clone(),
                reason: "autonomic workcell abnormality: adaptation refused".into(),
            });
        }
        let actions = plan
            .actions
            .iter()
            .map(|action| action.as_str())
            .collect::<Vec<_>>()
            .join("\u{1f}");
        Ok(digest(&[
            &plan.plan_digest,
            &actions,
            &self.actor,
            "autonomic-adaptation",
        ]))
    }
}

#[derive(Clone, Debug)]
pub struct ReferenceAutonomicPraxis {
    judge: String,
}

impl ReferenceAutonomicPraxis {
    #[must_use]
    pub fn new(judge: impl Into<String>) -> Self {
        Self {
            judge: judge.into(),
        }
    }

    pub fn authorize(
        &self,
        proposed: &AutonomicLoop<Proposed>,
        approver: impl Into<String>,
    ) -> Result<AutonomicAuthorization, AutonomicRefusal> {
        let plan = proposed.plan.as_ref().expect("proposed state owns a plan");
        let approver = approver.into();
        if self.judge.is_empty() || approver.is_empty() {
            return Err(AutonomicRefusal::EmptyActor);
        }
        if self.judge == plan.observer || self.judge == plan.workcell {
            return Err(AutonomicRefusal::SelfAdjudication);
        }
        validate_actions(plan.regime, &plan.actions)?;
        let authorization_digest = digest(&[
            &plan.plan_digest,
            &plan.knowledge_digest,
            &self.judge,
            &approver,
            "autonomic-authorize",
        ]);
        Ok(AutonomicAuthorization {
            plan_digest: plan.plan_digest.clone(),
            knowledge_digest: plan.knowledge_digest.clone(),
            judge: self.judge.clone(),
            approver,
            authorization_digest,
        })
    }
}

impl AutonomicLoop<Sensed> {
    pub fn classify(
        self,
        policy: &AutonomicPolicy,
    ) -> Result<AutonomicLoop<Classified>, AutonomicRefusal> {
        if self.evidence.error_basis_points > policy.max_error_basis_points {
            return Err(AutonomicRefusal::UnsafeObservation);
        }
        let regime = if self.evidence.observed_units / self.evidence.baseline_units
            >= policy.threshold_multiplier
        {
            PhaseRegime::PhaseShift1000x
        } else {
            PhaseRegime::Continuous
        };
        Ok(AutonomicLoop {
            evidence: self.evidence,
            regime: Some(regime),
            plan: None,
            authorization: None,
            receipt: None,
            andon: None,
            state: PhantomData,
        })
    }
}

impl AutonomicLoop<Classified> {
    pub fn propose(
        self,
        policy: &AutonomicPolicy,
    ) -> Result<AutonomicLoop<Proposed>, AutonomicRefusal> {
        let regime = self.regime.expect("classified state owns a regime");
        let actions = match regime {
            PhaseRegime::Continuous => vec![AutonomicAction::PreserveStandardWork],
            PhaseRegime::PhaseShift1000x => PHASE_SHIFT_ACTIONS.to_vec(),
        };
        if actions.len() > policy.max_actions {
            return Err(AutonomicRefusal::TooManyActions);
        }
        validate_actions(regime, &actions)?;
        let encoded_actions = actions
            .iter()
            .map(|action| action.as_str())
            .collect::<Vec<_>>()
            .join("\u{1f}");
        let plan_digest = digest(&[
            &self.evidence.cycle_id,
            &self.evidence.evidence_digest,
            regime.as_str(),
            &encoded_actions,
            &policy.knowledge_digest,
            "autonomic-plan",
        ]);
        Ok(AutonomicLoop {
            plan: Some(AutonomicPlan {
                cycle_id: self.evidence.cycle_id.clone(),
                regime,
                actions,
                observer: self.evidence.observer.clone(),
                workcell: "cargo-cicd".into(),
                evidence_digest: self.evidence.evidence_digest.clone(),
                knowledge_digest: policy.knowledge_digest.clone(),
                plan_digest,
            }),
            evidence: self.evidence,
            regime: Some(regime),
            authorization: None,
            receipt: None,
            andon: None,
            state: PhantomData,
        })
    }
}

impl AutonomicLoop<Proposed> {
    #[must_use]
    pub fn plan(&self) -> &AutonomicPlan {
        self.plan.as_ref().expect("proposed state owns a plan")
    }

    pub fn authorize(
        self,
        authorization: AutonomicAuthorization,
    ) -> Result<AutonomicLoop<Authorized>, AutonomicRefusal> {
        let plan = self.plan.as_ref().expect("proposed state owns a plan");
        let expected = digest(&[
            &plan.plan_digest,
            &plan.knowledge_digest,
            &authorization.judge,
            &authorization.approver,
            "autonomic-authorize",
        ]);
        if authorization.plan_digest != plan.plan_digest
            || authorization.knowledge_digest != plan.knowledge_digest
            || authorization.authorization_digest != expected
        {
            return Err(AutonomicRefusal::AuthorizationMismatch);
        }
        Ok(AutonomicLoop {
            evidence: self.evidence,
            regime: self.regime,
            plan: self.plan,
            authorization: Some(authorization),
            receipt: None,
            andon: None,
            state: PhantomData,
        })
    }
}

impl AutonomicLoop<Authorized> {
    pub fn apply(
        self,
        executor: &mut ReferenceAutonomicCargoCicd,
    ) -> Result<AutonomicLoop<Receipted>, AutonomicLoop<Stopped>> {
        let plan = self.plan.as_ref().expect("authorized state owns a plan");
        if executor.actor.is_empty() || executor.actor != plan.workcell {
            return Err(self.stop(AutonomicRefusal::WorkcellMismatch.to_string()));
        }

        let execution = executor.apply(plan);
        match execution {
            Ok(adaptation_digest) if !adaptation_digest.is_empty() => {
                let authorization = self
                    .authorization
                    .as_ref()
                    .expect("authorized state owns authorization");
                let receipt = AutonomicReceipt {
                    cycle_id: plan.cycle_id.clone(),
                    regime: plan.regime,
                    evidence_digest: plan.evidence_digest.clone(),
                    plan_digest: plan.plan_digest.clone(),
                    authorization_digest: authorization.authorization_digest.clone(),
                    adaptation_digest,
                    observer: plan.observer.clone(),
                    judge: authorization.judge.clone(),
                    executor: executor.actor.clone(),
                    receipt_digest: String::new(),
                };
                let receipt_digest = digest(&[
                    &receipt.cycle_id,
                    &receipt.evidence_digest,
                    &receipt.plan_digest,
                    &receipt.authorization_digest,
                    &receipt.adaptation_digest,
                    &receipt.observer,
                    &receipt.judge,
                    &receipt.executor,
                    "autonomic-receipt",
                ]);
                let receipt = AutonomicReceipt {
                    receipt_digest,
                    ..receipt
                };
                Ok(AutonomicLoop {
                    evidence: self.evidence,
                    regime: self.regime,
                    plan: self.plan,
                    authorization: self.authorization,
                    receipt: Some(receipt),
                    andon: None,
                    state: PhantomData,
                })
            }
            Ok(_) => Err(self.stop("autonomic workcell returned empty adaptation evidence")),
            Err(andon) => Err(AutonomicLoop {
                evidence: self.evidence,
                regime: self.regime,
                plan: self.plan,
                authorization: self.authorization,
                receipt: None,
                andon: Some(andon),
                state: PhantomData,
            }),
        }
    }

    fn stop(self, reason: impl Into<String>) -> AutonomicLoop<Stopped> {
        AutonomicLoop {
            andon: Some(AutonomicAndon {
                cycle_id: self.evidence.cycle_id.clone(),
                reason: reason.into(),
            }),
            evidence: self.evidence,
            regime: self.regime,
            plan: self.plan,
            authorization: self.authorization,
            receipt: None,
            state: PhantomData,
        }
    }
}

impl AutonomicLoop<Receipted> {
    #[must_use]
    pub fn receipt(&self) -> &AutonomicReceipt {
        self.receipt.as_ref().expect("receipted state owns a receipt")
    }
}

impl AutonomicLoop<Stopped> {
    #[must_use]
    pub fn andon(&self) -> &AutonomicAndon {
        self.andon.as_ref().expect("stopped state owns an andon")
    }
}

fn validate_actions(
    regime: PhaseRegime,
    actions: &[AutonomicAction],
) -> Result<(), AutonomicRefusal> {
    let valid = match regime {
        PhaseRegime::Continuous => {
            actions == [AutonomicAction::PreserveStandardWork].as_slice()
        }
        PhaseRegime::PhaseShift1000x => actions == PHASE_SHIFT_ACTIONS.as_slice(),
    };
    if valid {
        Ok(())
    } else {
        Err(AutonomicRefusal::InvalidPlan)
    }
}

fn digest(parts: &[&str]) -> String {
    let mut hasher = blake3::Hasher::new();
    for part in parts {
        hasher.update(&(part.len() as u64).to_le_bytes());
        hasher.update(part.as_bytes());
    }
    hasher.finalize().to_hex().to_string()
}
