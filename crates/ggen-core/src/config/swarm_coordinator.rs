//! Swarm Coordinator
//!
//! Implements worker coordination, task distribution, and self-healing for the Hive Queen.

use super::swarm_intelligence::{
    AgentMessage, AgentStatus, CollectiveMemory, ConsensusVoting, MessageType, Proposal,
    WorkerState,
};
use chrono::{DateTime, Duration, Utc};
use ggen_utils::error::{Error, Result};
use serde::{Deserialize, Serialize};
use std::collections::{HashMap, VecDeque};
use tokio::sync::mpsc;

/// Swarm coordinator manages worker agents
pub struct SwarmCoordinator {
    /// Worker registry
    workers: HashMap<String, WorkerState>,

    /// Task queue
    task_queue: VecDeque<Task>,

    /// Completed tasks
    completed_tasks: Vec<Task>,

    /// Message channel
    message_tx: mpsc::UnboundedSender<AgentMessage>,
    #[allow(dead_code)]
    message_rx: mpsc::UnboundedReceiver<AgentMessage>,

    /// Collective memory reference
    #[allow(dead_code)]
    memory: CollectiveMemory,

    /// Active consensus votes
    active_votes: Vec<ConsensusVoting>,

    /// Failure recovery settings
    recovery_config: RecoveryConfig,
}

/// Task to be executed by workers
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct Task {
    /// Task ID
    pub id: String,

    /// Task description
    pub description: String,

    /// Task type
    pub task_type: TaskType,

    /// Priority (1-10, 10 = highest)
    pub priority: u8,

    /// Assigned worker
    pub assigned_to: Option<String>,

    /// Task status
    pub status: TaskStatus,

    /// Created timestamp
    pub created_at: DateTime<Utc>,

    /// Started timestamp
    pub started_at: Option<DateTime<Utc>>,

    /// Completed timestamp
    pub completed_at: Option<DateTime<Utc>>,

    /// Result data
    pub result: Option<String>,

    /// Retry count
    pub retries: u8,

    /// Dependencies (task IDs that must complete first)
    pub dependencies: Vec<String>,
}

/// Task types
#[derive(Debug, Clone, Copy, PartialEq, Eq, Serialize, Deserialize)]
pub enum TaskType {
    Analysis,
    VersionResolution,
    ConflictDetection,
    Validation,
    Optimization,
    Coordination,
}

/// Task status
#[derive(Debug, Clone, Copy, PartialEq, Eq, Serialize, Deserialize)]
pub enum TaskStatus {
    Pending,
    Assigned,
    InProgress,
    Completed,
    Failed,
    Cancelled,
}

/// Recovery configuration
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct RecoveryConfig {
    /// Max retry attempts
    pub max_retries: u8,

    /// Retry delay (seconds)
    pub retry_delay_secs: u64,

    /// Health check interval (seconds)
    pub health_check_interval_secs: u64,

    /// Worker timeout (seconds)
    pub worker_timeout_secs: u64,

    /// Enable auto-healing
    pub auto_heal: bool,

    /// Enable task redistribution
    pub auto_redistribute: bool,
}

impl Default for RecoveryConfig {
    fn default() -> Self {
        Self {
            max_retries: 3,
            retry_delay_secs: 5,
            health_check_interval_secs: 30,
            worker_timeout_secs: 300,
            auto_heal: true,
            auto_redistribute: true,
        }
    }
}

impl SwarmCoordinator {
    /// Create new swarm coordinator
    pub fn new(memory: CollectiveMemory) -> Self {
        let (tx, rx) = mpsc::unbounded_channel();

        Self {
            workers: HashMap::new(),
            task_queue: VecDeque::new(),
            completed_tasks: Vec::new(),
            message_tx: tx,
            message_rx: rx,
            memory,
            active_votes: Vec::new(),
            recovery_config: RecoveryConfig::default(),
        }
    }

    /// Register a worker
    pub fn register_worker(&mut self, worker_id: String) {
        let worker = WorkerState {
            worker_id: worker_id.clone(),
            current_task: None,
            status: AgentStatus::Idle,
            health: 1.0,
            last_heartbeat: Utc::now(),
            workload: Vec::new(),
            completed_tasks: 0,
            failed_tasks: 0,
        };

        self.workers.insert(worker_id, worker);
    }

    /// Submit a task
    pub fn submit_task(&mut self, task: Task) {
        self.task_queue.push_back(task);
    }

    /// Assign tasks to workers (intelligent load balancing)
    pub fn assign_tasks(&mut self) -> Result<Vec<String>> {
        let mut assigned = Vec::new();

        while let Some(task) = self.task_queue.pop_front() {
            // Check dependencies
            if !self.dependencies_met(&task) {
                // Re-queue for later
                self.task_queue.push_back(task);
                continue;
            }

            // Find best worker
            if let Some(worker_id) = self.select_best_worker(&task) {
                let mut task = task;
                task.assigned_to = Some(worker_id.clone());
                task.status = TaskStatus::Assigned;
                task.started_at = Some(Utc::now());

                // Update worker state
                if let Some(worker) = self.workers.get_mut(&worker_id) {
                    worker.current_task = Some(task.id.clone());
                    worker.status = AgentStatus::Working;
                    worker.workload.push(task.id.clone());
                }

                // Send message to worker
                let msg = AgentMessage {
                    id: format!("msg-{}", Utc::now().timestamp_nanos_opt().unwrap_or(0)),
                    from: "coordinator".to_string(),
                    to: Some(worker_id.clone()),
                    msg_type: MessageType::TaskAssignment,
                    content: serde_json::to_string(&task).unwrap_or_default(),
                    timestamp: Utc::now(),
                    priority: task.priority,
                    requires_response: true,
                };

                let _ = self.message_tx.send(msg);

                assigned.push(task.id.clone());

                // Store in completed list (will be updated when done)
                self.completed_tasks.push(task);
            } else {
                // No available workers, re-queue
                self.task_queue.push_back(task);
                break;
            }
        }

        Ok(assigned)
    }

    /// Select best worker for a task
    fn select_best_worker(&self, task: &Task) -> Option<String> {
        let mut best_worker: Option<(&String, f32)> = None;

        for (worker_id, worker) in &self.workers {
            if !worker.is_available() {
                continue;
            }

            // Calculate fitness score
            let score = self.calculate_worker_fitness(worker, task);

            if let Some((_, best_score)) = best_worker {
                if score > best_score {
                    best_worker = Some((worker_id, score));
                }
            } else {
                best_worker = Some((worker_id, score));
            }
        }

        best_worker.map(|(id, _)| id.clone())
    }

    /// Calculate worker fitness for a task
    fn calculate_worker_fitness(&self, worker: &WorkerState, _task: &Task) -> f32 {
        // Factors: health, current load, success rate
        let health_factor = worker.health;
        let load_factor = 1.0 - (worker.workload.len() as f32 / 10.0).min(1.0);

        let success_rate = if worker.completed_tasks + worker.failed_tasks > 0 {
            worker.completed_tasks as f32 / (worker.completed_tasks + worker.failed_tasks) as f32
        } else {
            1.0
        };

        // Weighted average
        (health_factor * 0.4) + (load_factor * 0.3) + (success_rate * 0.3)
    }

    /// Check if task dependencies are met
    fn dependencies_met(&self, task: &Task) -> bool {
        for dep_id in &task.dependencies {
            let dep_completed = self
                .completed_tasks
                .iter()
                .any(|t| t.id == *dep_id && t.status == TaskStatus::Completed);

            if !dep_completed {
                return false;
            }
        }

        true
    }

    /// Process worker heartbeat
    pub fn process_heartbeat(&mut self, worker_id: &str) {
        if let Some(worker) = self.workers.get_mut(worker_id) {
            worker.heartbeat();
        }
    }

    /// Complete a task
    pub fn complete_task(&mut self, worker_id: &str, task_id: &str, result: Option<String>) {
        if let Some(worker) = self.workers.get_mut(worker_id) {
            worker.complete_task(task_id);
        }

        // Update task status
        for task in &mut self.completed_tasks {
            if task.id == task_id {
                task.status = TaskStatus::Completed;
                task.completed_at = Some(Utc::now());
                task.result = result;
                break;
            }
        }
    }

    /// Fail a task
    pub fn fail_task(&mut self, worker_id: &str, task_id: &str) {
        if let Some(worker) = self.workers.get_mut(worker_id) {
            worker.fail_task();
        }

        // Find and retry task if possible
        for task in &mut self.completed_tasks {
            if task.id == task_id {
                task.retries += 1;

                if task.retries < self.recovery_config.max_retries {
                    // Re-queue for retry
                    task.status = TaskStatus::Pending;
                    task.assigned_to = None;
                    self.task_queue.push_back(task.clone());
                } else {
                    task.status = TaskStatus::Failed;
                }
                break;
            }
        }
    }

    /// Perform health checks on all workers
    pub fn health_check(&mut self) -> Vec<String> {
        let mut unhealthy = Vec::new();
        let timeout = Duration::seconds(self.recovery_config.worker_timeout_secs as i64);

        for (worker_id, worker) in &mut self.workers {
            // Check heartbeat timeout
            if Utc::now() - worker.last_heartbeat > timeout {
                worker.health = 0.0;
                worker.status = AgentStatus::Failed;
                unhealthy.push(worker_id.clone());
            }

            // Auto-heal if enabled
            if self.recovery_config.auto_heal
                && !worker.is_healthy()
                && worker.status == AgentStatus::Failed
            {
                worker.status = AgentStatus::Recovering;
                worker.health = 0.5;
            }
        }

        // Redistribute tasks from unhealthy workers
        if self.recovery_config.auto_redistribute && !unhealthy.is_empty() {
            self.redistribute_tasks(&unhealthy);
        }

        unhealthy
    }

    /// Redistribute tasks from failed workers
    fn redistribute_tasks(&mut self, failed_workers: &[String]) {
        for task in &mut self.completed_tasks {
            if task.status == TaskStatus::InProgress || task.status == TaskStatus::Assigned {
                if let Some(assigned) = &task.assigned_to {
                    if failed_workers.contains(assigned) {
                        // Reset and re-queue
                        task.status = TaskStatus::Pending;
                        task.assigned_to = None;
                        self.task_queue.push_back(task.clone());
                    }
                }
            }
        }
    }

    /// Initiate consensus voting
    pub fn start_consensus(&mut self, topic: String, proposals: Vec<Proposal>) -> String {
        let vote_id = format!("vote-{}", Utc::now().timestamp_nanos_opt().unwrap_or(0));
        let mut voting = ConsensusVoting::new(topic, 0.5, 0.66);

        for proposal in proposals {
            voting.add_proposal(proposal);
        }

        self.active_votes.push(voting);
        vote_id
    }

    /// Cast a vote in active consensus
    pub fn cast_vote(
        &mut self, vote_index: usize, agent_id: String, proposal_id: String,
    ) -> Result<()> {
        if let Some(voting) = self.active_votes.get_mut(vote_index) {
            voting
                .vote(agent_id, proposal_id)
                .map_err(|e| Error::new(&e))
        } else {
            Err(Error::new("Invalid vote index"))
        }
    }

    /// Check for consensus results
    pub fn check_consensus(&mut self) -> Vec<(String, Option<String>)> {
        let mut results = Vec::new();
        let total_agents = self.workers.len();

        for voting in &mut self.active_votes {
            if let Some(winner_id) = voting.check_consensus(total_agents) {
                results.push((voting.topic.clone(), Some(winner_id)));
            }
        }

        results
    }

    /// Get coordinator statistics
    pub fn stats(&self) -> CoordinatorStats {
        let total_tasks = self.completed_tasks.len();
        let completed = self
            .completed_tasks
            .iter()
            .filter(|t| t.status == TaskStatus::Completed)
            .count();
        let failed = self
            .completed_tasks
            .iter()
            .filter(|t| t.status == TaskStatus::Failed)
            .count();

        let healthy_workers = self.workers.values().filter(|w| w.is_healthy()).count();

        CoordinatorStats {
            total_workers: self.workers.len(),
            healthy_workers,
            total_tasks,
            completed_tasks: completed,
            failed_tasks: failed,
            pending_tasks: self.task_queue.len(),
            active_votes: self.active_votes.len(),
        }
    }
}

/// Coordinator statistics
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct CoordinatorStats {
    pub total_workers: usize,
    pub healthy_workers: usize,
    pub total_tasks: usize,
    pub completed_tasks: usize,
    pub failed_tasks: usize,
    pub pending_tasks: usize,
    pub active_votes: usize,
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_worker_registration() {
        let memory = CollectiveMemory::new();
        let mut coordinator = SwarmCoordinator::new(memory);

        coordinator.register_worker("worker-1".to_string());
        coordinator.register_worker("worker-2".to_string());

        assert_eq!(coordinator.workers.len(), 2);
    }

    #[test]
    fn test_task_assignment() {
        let memory = CollectiveMemory::new();
        let mut coordinator = SwarmCoordinator::new(memory);

        coordinator.register_worker("worker-1".to_string());

        let task = Task {
            id: "task-1".to_string(),
            description: "Test task".to_string(),
            task_type: TaskType::Analysis,
            priority: 5,
            assigned_to: None,
            status: TaskStatus::Pending,
            created_at: Utc::now(),
            started_at: None,
            completed_at: None,
            result: None,
            retries: 0,
            dependencies: Vec::new(),
        };

        coordinator.submit_task(task);
        let assigned = coordinator.assign_tasks().unwrap();

        assert_eq!(assigned.len(), 1);
    }

    #[test]
    fn test_health_check() {
        let memory = CollectiveMemory::new();
        let mut coordinator = SwarmCoordinator::new(memory);

        coordinator.register_worker("worker-1".to_string());

        // Simulate timeout by setting old heartbeat
        if let Some(worker) = coordinator.workers.get_mut("worker-1") {
            worker.last_heartbeat = Utc::now() - Duration::seconds(400);
        }

        let unhealthy = coordinator.health_check();
        assert_eq!(unhealthy.len(), 1);
    }
}
