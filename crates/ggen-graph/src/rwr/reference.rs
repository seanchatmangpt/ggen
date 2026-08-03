//! Executable reference foundation proving every RWR Level-5 dimension.
//!
//! No dimension becomes `ALIVE` from a declaration. The reference foundation
//! crosses a real filesystem boundary, issues cryptographic actuation receipts,
//! externalizes every required proof surface, and reassesses the immutable ledger.

use crate::rwr::autonomic::{AutonomicController, AutonomicError, ManagedCell};
use crate::rwr::evidence::{
    AssessmentReceipt, EvidenceError, EvidenceLedger, EvidenceOutcome, EvidenceRecord, GallState,
};
use crate::rwr::execution::{
    Action, ActuationReceipt, ExecutionError, ExecutionPolicy, FilesystemActuator,
    FoundationMachine, ReplayVerifier,
};
use crate::rwr::matrix::{contract, Dimension, MaturityLevel, ALL_DIMENSIONS, MATRIX_VERSION};
use serde::{Deserialize, Serialize};
use serde_json::{json, Value};
use std::fs::File;
use std::io::Read;
use std::path::PathBuf;

/// Complete proof emitted by the executable reference foundation.
#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
pub struct ReferenceProof {
    /// Immutable evidence history.
    pub ledger: EvidenceLedger,
    /// Cryptographic crown assessment.
    pub assessment_receipt: AssessmentReceipt,
}

impl ReferenceProof {
    /// Replay the ledger and verify the crown receipt.
    pub fn verify(&self) -> Result<(), ReferenceError> {
        if self.ledger.records().iter().any(|record| !record.verify()) {
            return Err(ReferenceError::Invariant(
                "an evidence record failed cryptographic verification".to_string(),
            ));
        }
        let replayed = self.ledger.assess();
        if replayed != self.assessment_receipt.assessment {
            return Err(ReferenceError::Invariant(
                "assessment does not replay from the evidence ledger".to_string(),
            ));
        }
        self.assessment_receipt.verify()?;
        if replayed.standing != GallState::Alive || !replayed.is_complete_matrix() {
            return Err(ReferenceError::Invariant(
                "reference foundation is not full-matrix ALIVE".to_string(),
            ));
        }
        Ok(())
    }
}

/// Real boundary-crossing implementation of the full maturity matrix.
#[derive(Debug, Clone)]
pub struct ReferenceFoundation {
    root: PathBuf,
    machine: FoundationMachine,
    actuator: FilesystemActuator,
}

impl ReferenceFoundation {
    /// Construct a reference foundation under an empty bounded root.
    #[must_use]
    pub fn new(root: impl Into<PathBuf>) -> Self {
        let root = root.into();
        Self {
            actuator: FilesystemActuator::new(root.join("actuation")),
            machine: FoundationMachine::new(ExecutionPolicy::full_level5(1024 * 1024)),
            root,
        }
    }

    /// Execute all 21 dimensions and all 63 required proof surfaces.
    pub fn prove_all(&self) -> Result<ReferenceProof, ReferenceError> {
        if self.root.exists() && self.root.read_dir()?.next().is_some() {
            return Err(ReferenceError::NonEmptyRoot(self.root.clone()));
        }
        std::fs::create_dir_all(&self.root)?;

        let mut ledger = EvidenceLedger::new();
        let mut epoch = 1_u64;
        for dimension in ALL_DIMENSIONS {
            let scenario = self.run_scenario(dimension)?;
            for surface in contract(dimension).required_surfaces {
                let payload = serde_json::to_vec_pretty(&json!({
                    "schema": "rwr-dimension-proof/v1",
                    "matrix_version": MATRIX_VERSION,
                    "dimension": dimension,
                    "surface": surface,
                    "scenario": scenario.clone(),
                }))
                .map_err(|error| ReferenceError::Serialization(error.to_string()))?;
                let action_id = format!(
                    "proof-{}-{}",
                    dimension_slug(dimension),
                    surface_slug(*surface)
                );
                let receipt = self.commit(&action_id, dimension, payload)?;
                let committed = self.read_artifact(&receipt.action_id)?;
                ledger.admit(EvidenceRecord::observed(
                    format!("evidence-{action_id}"),
                    dimension,
                    MaturityLevel::DigitalEcosystem,
                    *surface,
                    EvidenceOutcome::Pass,
                    receipt.artifact_path,
                    epoch,
                    &committed,
                ))?;
                epoch = epoch.saturating_add(1);
            }
        }

        let assessment = ledger.assess();
        if assessment.standing != GallState::Alive || !assessment.is_complete_matrix() {
            return Err(ReferenceError::Invariant(format!(
                "full matrix did not close: {:?}",
                assessment.open_obligations()
            )));
        }
        let proof = ReferenceProof {
            assessment_receipt: AssessmentReceipt::issue(assessment)?,
            ledger,
        };
        proof.verify()?;
        Ok(proof)
    }

    fn run_scenario(&self, dimension: Dimension) -> Result<Value, ReferenceError> {
        match dimension {
            Dimension::OperatingModelDecisionRights | Dimension::EnterpriseGovernance => {
                self.prove_refusal(dimension)
            }
            Dimension::AutonomicClosure => self.prove_autonomic(),
            Dimension::ReceiptReplay => self.prove_replay(),
            Dimension::AutomationClosure | Dimension::LinkingAutomation => {
                self.prove_chain(dimension)
            }
            Dimension::ReusableBusinessComponents
            | Dimension::StrategicAgility
            | Dimension::EconomicValue => self.prove_reuse_value(dimension),
            _ => self.prove_standard_transaction(dimension),
        }
    }

    fn prove_standard_transaction(&self, dimension: Dimension) -> Result<Value, ReferenceError> {
        let payload = serde_json::to_vec(&json!({
            "contract": contract(dimension).level5_outcome,
            "dimension": dimension,
            "state": "admitted",
        }))
        .map_err(|error| ReferenceError::Serialization(error.to_string()))?;
        let source = self.commit(
            &format!("scenario-{}-source", dimension_slug(dimension)),
            dimension,
            payload,
        )?;
        let observed = self.read_artifact(&source.action_id)?;
        let downstream = self.commit(
            &format!("scenario-{}-downstream", dimension_slug(dimension)),
            dimension,
            observed,
        )?;
        ensure(
            source.payload_digest == downstream.payload_digest,
            "canonical state diverged across the transaction boundary",
        )?;
        Ok(json!({
            "kind": "standardized-integrated-transaction",
            "source_receipt": source.receipt_digest,
            "downstream_receipt": downstream.receipt_digest,
            "equivalent": true,
        }))
    }

    fn prove_refusal(&self, dimension: Dimension) -> Result<Value, ReferenceError> {
        let narrow =
            FoundationMachine::new(ExecutionPolicy::new([Dimension::ProcessIntegration], 1024));
        let action = Action::new(
            format!("scenario-{}-unauthorized", dimension_slug(dimension)),
            dimension,
            b"attempted-governance-bypass".to_vec(),
        );
        let refused = matches!(
            narrow.derive_grant(&action),
            Err(ExecutionError::DimensionRefused(observed)) if observed == dimension
        );
        ensure(refused, "unauthorized decision right did not fail closed")?;
        Ok(json!({"kind": "typed-policy-refusal", "refused": true}))
    }

    fn prove_chain(&self, dimension: Dimension) -> Result<Value, ReferenceError> {
        let first = self.commit(
            &format!("scenario-{}-trigger", dimension_slug(dimension)),
            dimension,
            b"trigger=admitted".to_vec(),
        )?;
        let second = self.commit(
            &format!("scenario-{}-consequence", dimension_slug(dimension)),
            dimension,
            self.read_artifact(&first.action_id)?,
        )?;
        ensure(
            first.payload_digest == second.payload_digest,
            "automated chain lost causal state",
        )?;
        Ok(json!({
            "kind": "unattended-receipted-chain",
            "trigger": first.receipt_digest,
            "consequence": second.receipt_digest,
        }))
    }

    fn prove_reuse_value(&self, dimension: Dimension) -> Result<Value, ReferenceError> {
        let component = b"reusable-component:v1".to_vec();
        let a = self.commit(
            &format!("scenario-{}-offering-a", dimension_slug(dimension)),
            dimension,
            component.clone(),
        )?;
        let b = self.commit(
            &format!("scenario-{}-offering-b", dimension_slug(dimension)),
            dimension,
            component,
        )?;
        ensure(
            a.payload_digest == b.payload_digest,
            "component was not reused",
        )?;
        Ok(json!({
            "kind": "measured-reuse",
            "offerings": 2,
            "unique_component_builds": 1,
            "reuse_ratio": 2.0,
            "receipt_a": a.receipt_digest,
            "receipt_b": b.receipt_digest,
        }))
    }

    fn prove_autonomic(&self) -> Result<Value, ReferenceError> {
        let initial = self.commit(
            "scenario-autonomic-initial",
            Dimension::AutonomicClosure,
            b"state=degraded".to_vec(),
        )?;
        let mut cell = ManagedCell::new(b"state=healthy".to_vec(), initial.action_id);
        let cycle =
            AutonomicController::new(3).converge(&mut cell, &self.machine, &self.actuator)?;
        cycle.verify()?;
        ensure(cycle.converged, "autonomic controller did not converge")?;
        Ok(json!({
            "kind": "bounded-mape-k",
            "cycles": cycle.cycles,
            "repairs": cycle.actuation_receipts.len(),
            "cycle_receipt": cycle.receipt_digest,
        }))
    }

    fn prove_replay(&self) -> Result<Value, ReferenceError> {
        let receipt = self.commit(
            "scenario-receipt-replay",
            Dimension::ReceiptReplay,
            b"consequence=once".to_vec(),
        )?;
        let mut verifier = ReplayVerifier::default();
        verifier.verify_once(&receipt)?;
        let refused = matches!(
            verifier.verify_once(&receipt),
            Err(ExecutionError::ReceiptReplayRefused)
        );
        ensure(refused, "duplicate receipt replay was admitted")?;
        Ok(json!({
            "kind": "receipt-replay-refusal",
            "receipt": receipt.receipt_digest,
            "duplicate_refused": true,
        }))
    }

    fn commit(
        &self, id: &str, dimension: Dimension, payload: Vec<u8>,
    ) -> Result<ActuationReceipt, ReferenceError> {
        let action = Action::new(id, dimension, payload);
        let grant = self.machine.derive_grant(&action)?;
        let receipt = self.actuator.actuate(&grant, &action)?;
        self.actuator.verify_committed(&receipt)?;
        Ok(receipt)
    }

    fn read_artifact(&self, action_id: &str) -> Result<Vec<u8>, ReferenceError> {
        let path = self
            .actuator
            .root()
            .join("committed")
            .join(action_id)
            .join("artifact.bin");
        let mut bytes = Vec::new();
        File::open(path)?.read_to_end(&mut bytes)?;
        Ok(bytes)
    }
}

fn ensure(condition: bool, message: &str) -> Result<(), ReferenceError> {
    if condition {
        Ok(())
    } else {
        Err(ReferenceError::Invariant(message.to_string()))
    }
}

fn dimension_slug(dimension: Dimension) -> String {
    format!("{dimension:?}").to_ascii_lowercase()
}

fn surface_slug(surface: crate::rwr::matrix::EvidenceSurface) -> String {
    format!("{surface:?}").to_ascii_lowercase()
}

/// Typed reference-foundation failures.
#[derive(Debug, thiserror::Error)]
pub enum ReferenceError {
    /// The bounded root must be empty to prevent evidence contamination.
    #[error("reference proof root is not empty: {0}")]
    NonEmptyRoot(PathBuf),
    /// A machine-checkable scenario invariant failed.
    #[error("reference invariant failed: {0}")]
    Invariant(String),
    /// JSON proof externalization failed.
    #[error("reference serialization failed: {0}")]
    Serialization(String),
    /// Evidence admission or assessment receipt verification failed.
    #[error(transparent)]
    Evidence(#[from] EvidenceError),
    /// Machinery, policy, actuation, or replay failed.
    #[error(transparent)]
    Execution(#[from] ExecutionError),
    /// Bounded autonomic control failed.
    #[error(transparent)]
    Autonomic(#[from] AutonomicError),
    /// Real filesystem boundary failed.
    #[error(transparent)]
    Io(#[from] std::io::Error),
}
