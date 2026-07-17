//! Wave Orchestration (CA-8.1)
//!
//! Implements N-phase sequential waves with M parallel tasks.
//! Provides wave state observability and failure classification.

use crate::core::governance::Severity;
use crate::swarm::task::{TaskReceipt, TaskRequest};
use serde::{Deserialize, Serialize};
use std::collections::HashMap;

/// Status of a wave execution.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Serialize, Deserialize)]
pub enum WaveStatus {
    /// Wave is queued and waiting for execution.
    Queued,
    /// Wave is currently executing phases.
    Executing,
    /// Wave completed all phases successfully.
    Completed,
    /// Wave failed during execution of a phase.
    Failed,
}

/// A classification of a failure that occurred during a wave.
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct ResidualClass {
    /// Unique identifier for the failure class.
    pub code: String,
    /// Detailed description of the failure.
    pub description: String,
    /// Severity of the failure.
    pub severity: Severity,
}

/// A phase within a wave, consisting of parallel tasks.
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct WavePhase {
    /// Name of the phase.
    pub name: String,
    /// Tasks to be executed in parallel during this phase.
    pub tasks: Vec<TaskRequest>,
}

/// A wave is a sequence of phases executed sequentially.
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct Wave {
    /// Unique wave identifier.
    pub id: String,
    /// Phases to be executed sequentially.
    pub phases: Vec<WavePhase>,
    /// Current execution status.
    pub status: WaveStatus,
    /// Metadata for the wave.
    pub metadata: HashMap<String, String>,
}

/// Proof of wave completion, composed of task receipts.
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct WaveReceipt {
    /// Wave identifier.
    pub wave_id: String,
    /// Receipts for each phase.
    pub phase_receipts: Vec<PhaseReceipt>,
    /// Final status of the wave.
    pub status: WaveStatus,
    /// Failure classifications if any.
    pub residual_classes: Vec<ResidualClass>,
    /// Total execution time in milliseconds.
    pub total_execution_time_ms: u64,
}

/// Proof of phase completion.
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct PhaseReceipt {
    /// Phase name.
    pub phase_name: String,
    /// Receipts for tasks executed in this phase.
    pub task_receipts: Vec<TaskReceipt>,
}

impl Wave {
    /// Create a new wave.
    #[must_use]
    pub fn new(id: String) -> Self {
        Self { id, phases: Vec::new(), status: WaveStatus::Queued, metadata: HashMap::new() }
    }

    /// Add a phase to the wave.
    pub fn add_phase(&mut self, name: String, tasks: Vec<TaskRequest>) {
        self.phases.push(WavePhase { name, tasks });
    }

    /// Execute the wave (mock implementation for orchestrator).
    /// In a real system, this would be handled by the `SwarmCoordinator`.
    pub async fn execute<F, Fut>(&mut self, mut executor: F) -> WaveReceipt
    where
        F: FnMut(TaskRequest) -> Fut,
        Fut: std::future::Future<Output = TaskReceipt>,
    {
        self.status = WaveStatus::Executing;
        let mut phase_receipts = Vec::new();
        let mut total_time = 0;
        let mut failed = false;
        let mut residual_classes = Vec::new();

        for phase in &self.phases {
            let mut task_receipts = Vec::new();

            // Execute tasks in parallel (M parallel tasks)
            let mut futures = Vec::new();
            for task in phase.tasks.clone() {
                futures.push(executor(task));
            }

            let results = futures::future::join_all(futures).await;

            for receipt in results {
                total_time += receipt.execution_time_ms;
                if !receipt.is_success() {
                    failed = true;
                    residual_classes.push(ResidualClass {
                        code: format!("TASK_FAIL_{}", receipt.task_id),
                        description: receipt.result.clone(),
                        severity: Severity::Andon,
                    });
                }
                task_receipts.push(receipt);
            }

            phase_receipts.push(PhaseReceipt { phase_name: phase.name.clone(), task_receipts });

            if failed {
                self.status = WaveStatus::Failed;
                break;
            }
        }

        if !failed {
            self.status = WaveStatus::Completed;
        }

        WaveReceipt {
            wave_id: self.id.clone(),
            phase_receipts,
            status: self.status,
            residual_classes,
            total_execution_time_ms: total_time,
        }
    }
}

impl WaveReceipt {
    /// Check if the wave was successful.
    #[must_use]
    pub fn is_success(&self) -> bool {
        self.status == WaveStatus::Completed
    }

    /// Returns the number of successful tasks.
    #[must_use]
    pub fn success_count(&self) -> usize {
        self.phase_receipts
            .iter()
            .flat_map(|p| &p.task_receipts)
            .filter(|t| t.is_success())
            .count()
    }

    /// Returns the total number of tasks.
    #[must_use]
    pub fn total_tasks(&self) -> usize {
        self.phase_receipts.iter().flat_map(|p| &p.task_receipts).count()
    }
}
