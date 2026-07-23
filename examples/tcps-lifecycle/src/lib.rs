#![forbid(unsafe_code)]
//! Executable TCPS lifecycle with affine capabilities and atomic receipts.
//!
//! ```compile_fail
//! use tcps_lifecycle::{ProductionLine, ReferenceCargoCicdExecutor};
//! let observed = ProductionLine::observe("change-1", 1, "standard-v1");
//! let mut executor = ReferenceCargoCicdExecutor::green();
//! observed.execute(&mut executor);
//! ```

pub use tcps_aeneas_kernel as aeneas_kernel;

use serde::Serialize;
use std::{fmt, marker::PhantomData};

#[derive(Debug)] pub struct Observed;
#[derive(Debug)] pub struct Admitted;
#[derive(Debug)] pub struct Planned;
#[derive(Debug)] pub struct Authorized;
#[derive(Debug)] pub struct Receipted;
#[derive(Debug)] pub struct Stopped;

#[derive(Clone, Debug, PartialEq, Eq, Serialize)]
pub struct Observation {
    id: String,
    downstream_demand: u32,
    standard_hash: String,
}

#[derive(Clone, Debug, PartialEq, Eq, Serialize)]
pub struct AdmissionReceipt {
    observation_id: String,
    standard_hash: String,
    digest: String,
}

#[derive(Debug, PartialEq, Eq, Serialize)]
pub struct CargoCicdEvidence {
    build_green: bool,
    tests_green: bool,
    trybuild_green: bool,
    lean_green: bool,
    changed_scope: Vec<String>,
    source_digest: String,
    evidence_digest: String,
}

impl CargoCicdEvidence {
    #[must_use]
    pub const fn is_green(&self) -> bool {
        self.build_green && self.tests_green && self.trybuild_green && self.lean_green
    }

    #[must_use] pub fn digest(&self) -> &str { &self.evidence_digest }
    #[must_use] pub fn source_digest(&self) -> &str { &self.source_digest }
    #[must_use] pub fn changed_scope(&self) -> &[String] { &self.changed_scope }
}

#[derive(Debug, PartialEq, Eq, Serialize)]
pub struct ProductionPlan {
    observation_id: String,
    workcell: String,
    evidence_digest: String,
    plan_digest: String,
}

impl ProductionPlan {
    #[must_use] pub fn evidence_digest(&self) -> &str { &self.evidence_digest }
}

/// Opaque affine capability. Only `ReferencePraxisGate::authorize` constructs it.
#[derive(Debug, PartialEq, Eq)]
pub struct Authorization {
    plan_digest: String,
    standard_hash: String,
    approver: String,
    authorization_digest: String,
}

impl Authorization {
    fn new(plan_digest: &str, standard_hash: &str, approver: impl Into<String>) -> Self {
        let approver = approver.into();
        Self {
            plan_digest: plan_digest.to_owned(),
            standard_hash: standard_hash.to_owned(),
            authorization_digest: digest(&[plan_digest, standard_hash, &approver, "authorize"]),
            approver,
        }
    }

    #[must_use] pub fn approver(&self) -> &str { &self.approver }
}

#[derive(Clone, Debug, PartialEq, Eq, Serialize)]
pub struct ExecutionReceipt {
    observation_id: String,
    plan_digest: String,
    authorization_digest: String,
    evidence_digest: String,
    executor: String,
    artifact_digest: String,
    receipt_digest: String,
}

impl ExecutionReceipt {
    #[must_use] pub fn executor(&self) -> &str { &self.executor }
    #[must_use] pub fn receipt_digest(&self) -> &str { &self.receipt_digest }
    #[must_use] pub fn artifact_digest(&self) -> &str { &self.artifact_digest }
    #[must_use] pub fn evidence_digest(&self) -> &str { &self.evidence_digest }
}

#[derive(Clone, Debug, PartialEq, Eq, Serialize)]
pub struct Andon {
    observation_id: String,
    reason: String,
}

impl Andon {
    #[must_use] pub fn reason(&self) -> &str { &self.reason }
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub enum Refusal {
    ZeroDemand,
    StandardNotAdmitted,
    InvalidAdmissionReceipt,
    EvidenceNotGreen,
    AuthorizationMismatch,
    StandardUnchanged,
    EmptyStandard,
}

impl fmt::Display for Refusal {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.write_str(match self {
            Self::ZeroDemand => "downstream demand must be positive",
            Self::StandardNotAdmitted => "observation standard is not admitted",
            Self::InvalidAdmissionReceipt => "admission receipt does not bind the observation",
            Self::EvidenceNotGreen => "cargo-cicd or Lean evidence is not green",
            Self::AuthorizationMismatch => "authorization does not match the plan and standard",
            Self::StandardUnchanged => "restart requires updated standard work",
            Self::EmptyStandard => "standard work hash must be non-empty",
        })
    }
}

impl std::error::Error for Refusal {}

pub trait AdmissionGate {
    fn admit(&self, observation: &Observation) -> Result<AdmissionReceipt, Refusal>;
}

/// The executor returns artifact evidence only. The framework manufactures the receipt.
pub trait WorkcellExecutor {
    fn executor_name(&self) -> &str;
    fn execute(
        &mut self,
        observation: &Observation,
        plan: &ProductionPlan,
        authorization: &Authorization,
    ) -> Result<String, Andon>;
}

#[derive(Clone, Debug)]
pub struct ReferencePraxisGate { admitted_standard_hash: String }

impl ReferencePraxisGate {
    #[must_use]
    pub fn new(admitted_standard_hash: impl Into<String>) -> Self {
        Self { admitted_standard_hash: admitted_standard_hash.into() }
    }

    pub fn authorize(
        &self,
        plan: &ProductionLine<Planned>,
        approver: impl Into<String>,
    ) -> Result<Authorization, Refusal> {
        if plan.observation.standard_hash != self.admitted_standard_hash {
            return Err(Refusal::StandardNotAdmitted);
        }
        Ok(Authorization::new(
            plan.plan_digest(),
            &self.admitted_standard_hash,
            approver,
        ))
    }
}

impl AdmissionGate for ReferencePraxisGate {
    fn admit(&self, observation: &Observation) -> Result<AdmissionReceipt, Refusal> {
        if observation.downstream_demand == 0 { return Err(Refusal::ZeroDemand); }
        if observation.standard_hash.is_empty() { return Err(Refusal::EmptyStandard); }
        if observation.standard_hash != self.admitted_standard_hash {
            return Err(Refusal::StandardNotAdmitted);
        }
        Ok(AdmissionReceipt {
            observation_id: observation.id.clone(),
            standard_hash: observation.standard_hash.clone(),
            digest: admission_digest(observation),
        })
    }
}

#[derive(Clone, Debug)]
pub struct ReferenceCargoCicdExecutor {
    fail_execution: bool,
    verification_green: bool,
    executor_name: String,
}

impl ReferenceCargoCicdExecutor {
    #[must_use]
    pub fn green() -> Self {
        Self { fail_execution: false, verification_green: true, executor_name: "cargo-cicd".into() }
    }

    #[must_use]
    pub fn failing() -> Self {
        Self { fail_execution: true, verification_green: true, executor_name: "cargo-cicd".into() }
    }

    #[must_use]
    pub fn verification_failing() -> Self {
        Self { fail_execution: false, verification_green: false, executor_name: "cargo-cicd".into() }
    }

    #[must_use]
    pub fn verify(
        &self,
        source_digest: impl Into<String>,
        mut changed_scope: Vec<String>,
    ) -> CargoCicdEvidence {
        let source_digest = source_digest.into();
        changed_scope.sort();
        changed_scope.dedup();
        let scope = changed_scope.join("\u{1f}");
        let evidence_digest = digest(&[
            &source_digest,
            &scope,
            if self.verification_green { "green" } else { "red" },
            &self.executor_name,
        ]);
        CargoCicdEvidence {
            build_green: self.verification_green,
            tests_green: self.verification_green,
            trybuild_green: self.verification_green,
            lean_green: self.verification_green,
            changed_scope,
            source_digest,
            evidence_digest,
        }
    }
}

impl WorkcellExecutor for ReferenceCargoCicdExecutor {
    fn executor_name(&self) -> &str { &self.executor_name }

    fn execute(
        &mut self,
        observation: &Observation,
        plan: &ProductionPlan,
        authorization: &Authorization,
    ) -> Result<String, Andon> {
        if self.fail_execution {
            self.fail_execution = false;
            return Err(Andon {
                observation_id: observation.id.clone(),
                reason: "workcell abnormality: executor refused the plan".into(),
            });
        }
        Ok(digest(&[
            &observation.id,
            &plan.plan_digest,
            &authorization.authorization_digest,
            "artifact",
        ]))
    }
}

#[derive(Debug)]
pub struct ProductionLine<State> {
    observation: Observation,
    admission: Option<AdmissionReceipt>,
    plan: Option<ProductionPlan>,
    authorization: Option<Authorization>,
    receipt: Option<ExecutionReceipt>,
    andon: Option<Andon>,
    state: PhantomData<State>,
}

impl ProductionLine<Observed> {
    #[must_use]
    pub fn observe(
        id: impl Into<String>,
        downstream_demand: u32,
        standard_hash: impl Into<String>,
    ) -> Self {
        Self {
            observation: Observation { id: id.into(), downstream_demand, standard_hash: standard_hash.into() },
            admission: None,
            plan: None,
            authorization: None,
            receipt: None,
            andon: None,
            state: PhantomData,
        }
    }

    pub fn admit(self, gate: &impl AdmissionGate) -> Result<ProductionLine<Admitted>, Refusal> {
        let admission = gate.admit(&self.observation)?;
        if admission.observation_id != self.observation.id
            || admission.standard_hash != self.observation.standard_hash
            || admission.digest != admission_digest(&self.observation)
        {
            return Err(Refusal::InvalidAdmissionReceipt);
        }
        Ok(ProductionLine {
            observation: self.observation,
            admission: Some(admission),
            plan: None,
            authorization: None,
            receipt: None,
            andon: None,
            state: PhantomData,
        })
    }
}

impl ProductionLine<Admitted> {
    pub fn plan(self, evidence: CargoCicdEvidence) -> Result<ProductionLine<Planned>, Refusal> {
        if !evidence.is_green() { return Err(Refusal::EvidenceNotGreen); }
        let admission = self.admission.expect("admitted state owns admission receipt");
        let plan_digest = digest(&[
            &self.observation.id,
            &admission.digest,
            evidence.digest(),
            "cargo-cicd-workcell",
        ]);
        let observation_id = self.observation.id.clone();
        Ok(ProductionLine {
            observation: self.observation,
            admission: Some(admission),
            plan: Some(ProductionPlan {
                observation_id,
                workcell: "cargo-cicd".into(),
                evidence_digest: evidence.evidence_digest,
                plan_digest,
            }),
            authorization: None,
            receipt: None,
            andon: None,
            state: PhantomData,
        })
    }
}

impl ProductionLine<Planned> {
    #[must_use]
    pub fn plan_digest(&self) -> &str {
        &self.plan.as_ref().expect("planned state owns plan").plan_digest
    }

    pub fn authorize(self, authorization: Authorization) -> Result<ProductionLine<Authorized>, Refusal> {
        let plan = self.plan.as_ref().expect("planned state owns plan");
        let expected = digest(&[
            &plan.plan_digest,
            &self.observation.standard_hash,
            &authorization.approver,
            "authorize",
        ]);
        if authorization.plan_digest != plan.plan_digest
            || authorization.standard_hash != self.observation.standard_hash
            || authorization.authorization_digest != expected
        {
            return Err(Refusal::AuthorizationMismatch);
        }
        Ok(ProductionLine {
            observation: self.observation,
            admission: self.admission,
            plan: self.plan,
            authorization: Some(authorization),
            receipt: None,
            andon: None,
            state: PhantomData,
        })
    }
}

impl ProductionLine<Authorized> {
    pub fn execute(
        self,
        executor: &mut impl WorkcellExecutor,
    ) -> Result<ProductionLine<Receipted>, ProductionLine<Stopped>> {
        let plan = self.plan.as_ref().expect("authorized state owns plan");
        let authorization = self.authorization.as_ref().expect("authorized state owns authorization");
        if executor.executor_name().is_empty() || executor.executor_name() != plan.workcell {
            return Err(self.stop("workcell abnormality: executor identity mismatch"));
        }
        match executor.execute(&self.observation, plan, authorization) {
            Ok(artifact_digest) if !artifact_digest.is_empty() => {
                let executor_name = executor.executor_name().to_owned();
                let receipt_digest = digest(&[
                    &self.observation.id,
                    &plan.plan_digest,
                    &authorization.authorization_digest,
                    &plan.evidence_digest,
                    &artifact_digest,
                    &executor_name,
                ]);
                let receipt = ExecutionReceipt {
                    observation_id: plan.observation_id.clone(),
                    plan_digest: plan.plan_digest.clone(),
                    authorization_digest: authorization.authorization_digest.clone(),
                    evidence_digest: plan.evidence_digest.clone(),
                    executor: executor_name,
                    artifact_digest,
                    receipt_digest,
                };
                Ok(ProductionLine {
                    observation: self.observation,
                    admission: self.admission,
                    plan: self.plan,
                    authorization: self.authorization,
                    receipt: Some(receipt),
                    andon: None,
                    state: PhantomData,
                })
            }
            Ok(_) => Err(self.stop("workcell abnormality: empty artifact evidence")),
            Err(andon) => Err(ProductionLine {
                observation: self.observation,
                admission: self.admission,
                plan: self.plan,
                authorization: self.authorization,
                receipt: None,
                andon: Some(andon),
                state: PhantomData,
            }),
        }
    }

    fn stop(self, reason: &str) -> ProductionLine<Stopped> {
        ProductionLine {
            andon: Some(Andon { observation_id: self.observation.id.clone(), reason: reason.into() }),
            observation: self.observation,
            admission: self.admission,
            plan: self.plan,
            authorization: self.authorization,
            receipt: None,
            state: PhantomData,
        }
    }
}

impl ProductionLine<Receipted> {
    #[must_use]
    pub fn receipt(&self) -> &ExecutionReceipt {
        self.receipt.as_ref().expect("receipted state owns receipt")
    }
}

impl ProductionLine<Stopped> {
    #[must_use]
    pub fn andon(&self) -> &Andon {
        self.andon.as_ref().expect("stopped state owns andon")
    }

    pub fn recover(
        self,
        updated_standard_hash: impl Into<String>,
    ) -> Result<ProductionLine<Observed>, Refusal> {
        let updated_standard_hash = updated_standard_hash.into();
        if updated_standard_hash.is_empty() { return Err(Refusal::EmptyStandard); }
        if updated_standard_hash == self.observation.standard_hash {
            return Err(Refusal::StandardUnchanged);
        }
        Ok(ProductionLine::observe(
            self.observation.id,
            self.observation.downstream_demand,
            updated_standard_hash,
        ))
    }
}

fn admission_digest(observation: &Observation) -> String {
    digest(&[&observation.id, &observation.standard_hash, "praxis-admit"])
}

fn digest(parts: &[&str]) -> String {
    let mut hasher = blake3::Hasher::new();
    for part in parts {
        hasher.update(&(part.len() as u64).to_le_bytes());
        hasher.update(part.as_bytes());
    }
    hasher.finalize().to_hex().to_string()
}
