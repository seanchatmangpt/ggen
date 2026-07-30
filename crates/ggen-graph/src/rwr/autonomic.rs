//! Bounded MAPE-K autonomic controller over the receipted filesystem actuator.

use crate::rwr::execution::{
    Action, ActuationReceipt, ExecutionError, FilesystemActuator, FoundationMachine,
};
use crate::rwr::matrix::Dimension;
use serde::{Deserialize, Serialize};
use std::fs::File;
use std::io::Read;

fn put_len_prefixed(hasher: &mut blake3::Hasher, value: &[u8]) {
    hasher.update(&(value.len() as u64).to_le_bytes());
    hasher.update(value);
}

/// Bounded resource whose current state is always a committed, receipted artifact.
#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
pub struct ManagedCell {
    desired_state: Vec<u8>,
    current_action_id: String,
}

impl ManagedCell {
    /// Create a managed cell from an already committed initial action.
    #[must_use]
    pub fn new(desired_state: Vec<u8>, current_action_id: impl Into<String>) -> Self {
        Self {
            desired_state,
            current_action_id: current_action_id.into(),
        }
    }

    /// Current committed action identity.
    #[must_use]
    pub fn current_action_id(&self) -> &str {
        &self.current_action_id
    }
}

/// Receipt for a complete monitor-analyze-plan-execute-knowledge cycle.
#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
pub struct AutonomicCycleReceipt {
    /// Receipt schema.
    pub schema: String,
    /// Digest observed by the first monitor step.
    pub initial_state_digest: [u8; 32],
    /// Digest observed after convergence.
    pub final_state_digest: [u8; 32],
    /// Number of repair actions executed.
    pub cycles: u8,
    /// Receipts produced by real repair actions.
    pub actuation_receipts: Vec<ActuationReceipt>,
    /// Whether the bounded controller reached desired state.
    pub converged: bool,
    /// BLAKE3 binding the complete cycle.
    pub receipt_digest: [u8; 32],
}

impl AutonomicCycleReceipt {
    fn issue(
        initial_state_digest: [u8; 32],
        final_state_digest: [u8; 32],
        cycles: u8,
        actuation_receipts: Vec<ActuationReceipt>,
        converged: bool,
    ) -> Self {
        let receipt_digest = cycle_digest(
            &initial_state_digest,
            &final_state_digest,
            cycles,
            &actuation_receipts,
            converged,
        );
        Self {
            schema: "rwr-autonomic-cycle/v1".to_string(),
            initial_state_digest,
            final_state_digest,
            cycles,
            actuation_receipts,
            converged,
            receipt_digest,
        }
    }

    /// Verify the complete autonomic cycle receipt and every nested actuation.
    pub fn verify(&self) -> Result<(), AutonomicError> {
        if self.schema != "rwr-autonomic-cycle/v1" {
            return Err(AutonomicError::CycleReceiptMismatch);
        }
        for receipt in &self.actuation_receipts {
            receipt.verify()?;
        }
        let expected = cycle_digest(
            &self.initial_state_digest,
            &self.final_state_digest,
            self.cycles,
            &self.actuation_receipts,
            self.converged,
        );
        if self.receipt_digest != expected {
            return Err(AutonomicError::CycleReceiptMismatch);
        }
        Ok(())
    }
}

fn cycle_digest(
    initial_state_digest: &[u8; 32],
    final_state_digest: &[u8; 32],
    cycles: u8,
    receipts: &[ActuationReceipt],
    converged: bool,
) -> [u8; 32] {
    let mut hasher = blake3::Hasher::new();
    put_len_prefixed(&mut hasher, b"rwr-autonomic-cycle/v1");
    hasher.update(initial_state_digest);
    hasher.update(final_state_digest);
    hasher.update(&[cycles]);
    hasher.update(&[u8::from(converged)]);
    for receipt in receipts {
        hasher.update(&receipt.receipt_digest);
    }
    hasher.finalize().into()
}

/// Deterministic, bounded MAPE-K controller.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct AutonomicController {
    max_cycles: u8,
}

impl AutonomicController {
    /// Construct a controller with a hard convergence bound.
    #[must_use]
    pub const fn new(max_cycles: u8) -> Self {
        Self { max_cycles }
    }

    /// Monitor, analyze, plan, execute, and re-monitor until converged or refused.
    pub fn converge(
        &self,
        cell: &mut ManagedCell,
        machine: &FoundationMachine,
        actuator: &FilesystemActuator,
    ) -> Result<AutonomicCycleReceipt, AutonomicError> {
        if self.max_cycles == 0 {
            return Err(AutonomicError::ConvergenceBoundZero);
        }

        let initial_state = read_committed_state(actuator, cell.current_action_id())?;
        let initial_state_digest: [u8; 32] = blake3::hash(&initial_state).into();
        let desired_digest: [u8; 32] = blake3::hash(&cell.desired_state).into();
        let mut current_state = initial_state;
        let mut receipts = Vec::new();

        for cycle in 0..self.max_cycles {
            let current_digest: [u8; 32] = blake3::hash(&current_state).into();
            if current_digest == desired_digest {
                let receipt = AutonomicCycleReceipt::issue(
                    initial_state_digest,
                    current_digest,
                    cycle,
                    receipts,
                    true,
                );
                receipt.verify()?;
                return Ok(receipt);
            }

            let action = Action::new(
                format!("autonomic-repair-{}", cycle.saturating_add(1)),
                Dimension::AutonomicClosure,
                cell.desired_state.clone(),
            );
            let grant = machine.derive_grant(&action)?;
            let receipt = actuator.actuate(&grant, &action)?;
            cell.current_action_id = receipt.action_id.clone();
            current_state = read_committed_state(actuator, cell.current_action_id())?;
            receipts.push(receipt);
        }

        let final_state_digest: [u8; 32] = blake3::hash(&current_state).into();
        Err(AutonomicError::ConvergenceRefused {
            observed: final_state_digest,
            desired: desired_digest,
            maximum_cycles: self.max_cycles,
        })
    }
}

fn read_committed_state(
    actuator: &FilesystemActuator,
    action_id: &str,
) -> Result<Vec<u8>, AutonomicError> {
    let path = actuator
        .root()
        .join("committed")
        .join(action_id)
        .join("artifact.bin");
    let mut bytes = Vec::new();
    File::open(path)?.read_to_end(&mut bytes)?;
    Ok(bytes)
}

/// Typed autonomic failures.
#[derive(Debug, thiserror::Error)]
pub enum AutonomicError {
    /// A controller with no allowed cycle cannot operate.
    #[error("autonomic convergence bound must be greater than zero")]
    ConvergenceBoundZero,
    /// The controller exhausted its bounded cycle budget.
    #[error(
        "autonomic convergence refused after {maximum_cycles} cycles: observed {observed:?}, desired {desired:?}"
    )]
    ConvergenceRefused {
        /// Final observed digest.
        observed: [u8; 32],
        /// Desired digest.
        desired: [u8; 32],
        /// Hard cycle bound.
        maximum_cycles: u8,
    },
    /// Cycle receipt does not verify.
    #[error("autonomic cycle receipt mismatch")]
    CycleReceiptMismatch,
    /// Underlying execution machinery refused or failed.
    #[error(transparent)]
    Execution(#[from] ExecutionError),
    /// Real filesystem observation failed.
    #[error("autonomic monitor boundary failed: {0}")]
    Io(#[from] std::io::Error),
}
