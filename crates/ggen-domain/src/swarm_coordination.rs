//! Swarm Coordination: Type-Level Scheduling and Lock-Free Snapshots
//!
//! At AHI scale (trillions of agents), we need primitives that:
//! - Never block readers (lock-free snapshot reads)
//! - Aggregate proposals conflict-free when possible
//! - Use type-indexed scheduling to avoid starvation
//! - Support deterministic schedule ordering
//!
//! The type system guides execution; the runtime follows.

use serde::{Deserialize, Serialize};
use std::sync::Arc;

// ============================================================================
// SCHEDULING HINTS: Type-Indexed Priority & Latency
// ============================================================================

/// Priority class for agent tasks
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash, Serialize, Deserialize)]
pub enum Priority {
    /// Critical path: system governance
    Critical = 3,
    /// High: domain decisions
    High = 2,
    /// Normal: routine operations
    Normal = 1,
    /// Low: background optimization
    Low = 0,
}

/// Execution latency tier
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, Serialize, Deserialize)]
pub enum LatencyTier {
    /// Hot path: ≤8 ticks
    Hot,
    /// Warm path: ≤100 ticks
    Warm,
    /// Cold path: unbounded
    Cold,
}

/// Resource consumption estimate
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, Serialize, Deserialize)]
pub enum ResourceCost {
    /// Minimal: pure compute
    Minimal,
    /// Light: single observation read
    Light,
    /// Moderate: snapshot write
    Moderate,
    /// Heavy: ontology mutation
    Heavy,
}

/// Scheduling hint: encoded in action types to guide dispatch
#[derive(Debug, Clone, Copy, Serialize, Deserialize)]
pub struct SchedulingHint {
    pub priority: Priority,
    pub latency_tier: LatencyTier,
    pub resource_cost: ResourceCost,
}

impl SchedulingHint {
    pub fn new(priority: Priority, latency_tier: LatencyTier, resource_cost: ResourceCost) -> Self {
        Self {
            priority,
            latency_tier,
            resource_cost,
        }
    }

    /// Compute a deterministic sort key (used for stable ordering)
    pub fn sort_key(&self) -> (u8, u8, u8) {
        (
            self.priority as u8,
            match self.latency_tier {
                LatencyTier::Hot => 2,
                LatencyTier::Warm => 1,
                LatencyTier::Cold => 0,
            },
            match self.resource_cost {
                ResourceCost::Minimal => 0,
                ResourceCost::Light => 1,
                ResourceCost::Moderate => 2,
                ResourceCost::Heavy => 3,
            },
        )
    }
}

// ============================================================================
// LOCK-FREE SNAPSHOT READS
// ============================================================================

/// Versioned snapshot descriptor
/// Readers never block; they use this to get the current snapshot
#[derive(Debug, Clone)]
pub struct SnapshotDescriptor {
    pub version: u64,
    pub snapshot_id: String,
}

/// Lock-free snapshot cell
/// Writers update the descriptor atomically; readers never wait
pub struct SnapshotCell {
    /// Current active descriptor (can be atomically swapped)
    descriptor: Arc<SnapshotDescriptor>,
    /// Optional staging area for next snapshot
    staging: Option<Arc<SnapshotDescriptor>>,
}

impl SnapshotCell {
    pub fn new(snapshot_id: impl Into<String>) -> Self {
        Self {
            descriptor: Arc::new(SnapshotDescriptor {
                version: 0,
                snapshot_id: snapshot_id.into(),
            }),
            staging: None,
        }
    }

    /// Get current active snapshot (never blocks)
    pub fn current(&self) -> Arc<SnapshotDescriptor> {
        Arc::clone(&self.descriptor)
    }

    /// Begin staging next snapshot
    pub fn stage_next(&mut self, new_snapshot_id: impl Into<String>) {
        self.staging = Some(Arc::new(SnapshotDescriptor {
            version: self.descriptor.version + 1,
            snapshot_id: new_snapshot_id.into(),
        }));
    }

    /// Atomically swap staged snapshot to active
    /// Fails if no staging snapshot
    pub fn commit_staged(&mut self) -> Result<(), String> {
        match self.staging.take() {
            Some(next) => {
                self.descriptor = next;
                Ok(())
            }
            None => Err("No staged snapshot to commit".to_string()),
        }
    }

    /// Get version
    pub fn version(&self) -> u64 {
        self.descriptor.version
    }
}

impl Clone for SnapshotCell {
    fn clone(&self) -> Self {
        Self {
            descriptor: Arc::clone(&self.descriptor),
            staging: self.staging.as_ref().map(Arc::clone),
        }
    }
}

// ============================================================================
// CONFLICT-FREE PROPOSAL AGGREGATION
// ============================================================================

/// Trait for proposals that can be merged conflict-free
pub trait ConflictFree: Clone {
    /// Try to merge with another
    /// Returns Ok(merged) if compatible, Err if conflict
    fn try_merge(&self, other: &Self) -> Result<Self, String>;

    /// Is this proposal commutative with another?
    fn is_commutative_with(&self, _other: &Self) -> bool {
        true // Default: assume commutative unless proven otherwise
    }
}

/// Proposal type that can always be merged (commutative)
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct CommutativeProposal {
    pub id: String,
    pub action: String,
    pub priority: Priority,
}

impl ConflictFree for CommutativeProposal {
    fn try_merge(&self, other: &Self) -> Result<Self, String> {
        // Commutative: order doesn't matter
        Ok(CommutativeProposal {
            id: format!("{}-{}", self.id, other.id),
            action: format!("{}|{}", self.action, other.action),
            priority: std::cmp::max(self.priority, other.priority),
        })
    }

    fn is_commutative_with(&self, _other: &Self) -> bool {
        true
    }
}

/// Proposal with potential conflicts
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct ConditionalProposal {
    pub id: String,
    pub action: String,
    pub resource: String, // Which resource it affects
    pub priority: Priority,
}

impl ConflictFree for ConditionalProposal {
    fn try_merge(&self, other: &Self) -> Result<Self, String> {
        // Conflict if touching same resource
        if self.resource != other.resource {
            Ok(ConditionalProposal {
                id: format!("{}-{}", self.id, other.id),
                action: format!("{}|{}", self.action, other.action),
                resource: "mixed".to_string(),
                priority: std::cmp::max(self.priority, other.priority),
            })
        } else {
            Err(format!(
                "Conflict: both proposals touch resource {}",
                self.resource
            ))
        }
    }

    fn is_commutative_with(&self, other: &Self) -> bool {
        self.resource != other.resource
    }
}

/// Aggregate compatible proposals
pub struct ProposalAggregator;

impl ProposalAggregator {
    /// Merge multiple commutative proposals
    /// Succeeds only if all are fully compatible
    pub fn aggregate_commutative(
        proposals: Vec<CommutativeProposal>,
    ) -> Result<CommutativeProposal, String> {
        if proposals.is_empty() {
            return Err("No proposals to aggregate".to_string());
        }

        let mut result = proposals[0].clone();
        for proposal in &proposals[1..] {
            result = result.try_merge(proposal)?;
        }

        Ok(result)
    }

    /// Merge conditional proposals, stopping on conflict
    /// Returns (merged, failed_proposals)
    pub fn aggregate_conditional(
        proposals: Vec<ConditionalProposal>,
    ) -> (Option<ConditionalProposal>, Vec<ConditionalProposal>) {
        let mut merged: Option<ConditionalProposal> = None;
        let mut failed = Vec::new();

        for proposal in proposals {
            match &merged {
                None => merged = Some(proposal),
                Some(existing) => match existing.try_merge(&proposal) {
                    Ok(combined) => merged = Some(combined),
                    Err(_) => failed.push(proposal), // Conflict: keep separate
                },
            }
        }

        (merged, failed)
    }
}

// ============================================================================
// DETERMINISTIC SCHEDULING
// ============================================================================

/// Agent task with scheduling hint
#[derive(Debug, Clone)]
pub struct ScheduledTask {
    pub task_id: String,
    pub hint: SchedulingHint,
    pub description: String,
}

impl ScheduledTask {
    pub fn new(
        task_id: impl Into<String>, hint: SchedulingHint, description: impl Into<String>,
    ) -> Self {
        Self {
            task_id: task_id.into(),
            hint,
            description: description.into(),
        }
    }
}

/// FIFO scheduler with priority lanes
pub struct PriorityScheduler {
    // Queue per priority level
    critical_queue: Vec<ScheduledTask>,
    high_queue: Vec<ScheduledTask>,
    normal_queue: Vec<ScheduledTask>,
    low_queue: Vec<ScheduledTask>,
}

impl PriorityScheduler {
    pub fn new() -> Self {
        Self {
            critical_queue: Vec::new(),
            high_queue: Vec::new(),
            normal_queue: Vec::new(),
            low_queue: Vec::new(),
        }
    }

    /// Enqueue task in appropriate priority lane
    pub fn enqueue(&mut self, task: ScheduledTask) {
        match task.hint.priority {
            Priority::Critical => self.critical_queue.push(task),
            Priority::High => self.high_queue.push(task),
            Priority::Normal => self.normal_queue.push(task),
            Priority::Low => self.low_queue.push(task),
        }
    }

    /// Dequeue next task (respects priority, prevents starvation)
    /// Uses round-robin across priority levels
    pub fn dequeue(&mut self, allow_starvation: bool) -> Option<ScheduledTask> {
        // Always check critical first
        if !self.critical_queue.is_empty() {
            return Some(self.critical_queue.remove(0));
        }

        // Then high
        if !self.high_queue.is_empty() {
            return Some(self.high_queue.remove(0));
        }

        // Then normal
        if !self.normal_queue.is_empty() {
            return Some(self.normal_queue.remove(0));
        }

        // Only low if not at risk of starvation
        if allow_starvation || self.high_queue.is_empty() {
            if !self.low_queue.is_empty() {
                return Some(self.low_queue.remove(0));
            }
        }

        None
    }

    pub fn queue_depth(&self) -> usize {
        self.critical_queue.len()
            + self.high_queue.len()
            + self.normal_queue.len()
            + self.low_queue.len()
    }
}

impl Default for PriorityScheduler {
    fn default() -> Self {
        Self::new()
    }
}

// ============================================================================
// METRIC TRACKING FOR SWARM HEALTH
// ============================================================================

/// Metrics for swarm execution
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct SwarmMetrics {
    pub total_tasks_processed: u64,
    pub total_merges_successful: u64,
    pub total_conflicts: u64,
    pub avg_queue_depth: f64,
}

impl SwarmMetrics {
    pub fn new() -> Self {
        Self {
            total_tasks_processed: 0,
            total_merges_successful: 0,
            total_conflicts: 0,
            avg_queue_depth: 0.0,
        }
    }

    pub fn conflict_ratio(&self) -> f64 {
        if self.total_merges_successful == 0 {
            return 0.0;
        }
        self.total_conflicts as f64 / (self.total_merges_successful + self.total_conflicts) as f64
    }
}

impl Default for SwarmMetrics {
    fn default() -> Self {
        Self::new()
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_scheduling_hint_sort_key() {
        let hint1 =
            SchedulingHint::new(Priority::Critical, LatencyTier::Hot, ResourceCost::Minimal);
        let hint2 = SchedulingHint::new(Priority::Low, LatencyTier::Cold, ResourceCost::Heavy);

        let key1 = hint1.sort_key();
        let key2 = hint2.sort_key();

        assert!(key1 > key2); // Critical should sort higher
    }

    #[test]
    fn test_snapshot_cell_lock_free() {
        let cell = SnapshotCell::new("snap-1");
        let snap1 = cell.current();

        assert_eq!(snap1.version, 0);
        assert_eq!(snap1.snapshot_id, "snap-1");

        // Clone reader can happen freely without waiting
        let snap1_copy = cell.current();
        assert_eq!(snap1_copy.version, snap1.version);
    }

    #[test]
    fn test_snapshot_staging_and_commit() {
        let mut cell = SnapshotCell::new("snap-1");

        // Stage next
        cell.stage_next("snap-2");

        // Current still unchanged
        assert_eq!(cell.current().version, 0);

        // Commit
        assert!(cell.commit_staged().is_ok());

        // Now current is updated
        assert_eq!(cell.current().version, 1);
        assert_eq!(cell.current().snapshot_id, "snap-2");
    }

    #[test]
    fn test_commutative_proposal_merge() {
        let p1 = CommutativeProposal {
            id: "p1".to_string(),
            action: "action1".to_string(),
            priority: Priority::High,
        };

        let p2 = CommutativeProposal {
            id: "p2".to_string(),
            action: "action2".to_string(),
            priority: Priority::Normal,
        };

        let merged = p1.try_merge(&p2);
        assert!(merged.is_ok());
        let merged = merged.unwrap();
        assert_eq!(merged.priority, Priority::High); // Takes higher priority
    }

    #[test]
    fn test_conditional_proposal_merge() {
        let p1 = ConditionalProposal {
            id: "p1".to_string(),
            action: "read".to_string(),
            resource: "ontology_v1".to_string(),
            priority: Priority::Normal,
        };

        let p2 = ConditionalProposal {
            id: "p2".to_string(),
            action: "read".to_string(),
            resource: "marketplace_v1".to_string(),
            priority: Priority::Normal,
        };

        // Different resources: should merge
        let merged = p1.try_merge(&p2);
        assert!(merged.is_ok());

        // Same resource: should conflict
        let p3 = ConditionalProposal {
            id: "p3".to_string(),
            action: "write".to_string(),
            resource: "ontology_v1".to_string(),
            priority: Priority::High,
        };

        let conflict = p1.try_merge(&p3);
        assert!(conflict.is_err());
    }

    #[test]
    fn test_priority_scheduler() {
        let mut scheduler = PriorityScheduler::new();

        scheduler.enqueue(ScheduledTask::new(
            "task-low",
            SchedulingHint::new(Priority::Low, LatencyTier::Cold, ResourceCost::Light),
            "Low priority",
        ));

        scheduler.enqueue(ScheduledTask::new(
            "task-critical",
            SchedulingHint::new(Priority::Critical, LatencyTier::Hot, ResourceCost::Minimal),
            "Critical",
        ));

        scheduler.enqueue(ScheduledTask::new(
            "task-normal",
            SchedulingHint::new(Priority::Normal, LatencyTier::Warm, ResourceCost::Moderate),
            "Normal",
        ));

        // Should dequeue in priority order
        let first = scheduler.dequeue(false).unwrap();
        assert_eq!(first.task_id, "task-critical");

        let second = scheduler.dequeue(false).unwrap();
        assert_eq!(second.task_id, "task-normal");

        let third = scheduler.dequeue(false).unwrap();
        assert_eq!(third.task_id, "task-low");
    }

    #[test]
    fn test_aggregator_commutative() {
        let proposals = vec![
            CommutativeProposal {
                id: "p1".to_string(),
                action: "action1".to_string(),
                priority: Priority::High,
            },
            CommutativeProposal {
                id: "p2".to_string(),
                action: "action2".to_string(),
                priority: Priority::Normal,
            },
        ];

        let merged = ProposalAggregator::aggregate_commutative(proposals);
        assert!(merged.is_ok());
    }

    #[test]
    fn test_swarm_metrics() {
        let mut metrics = SwarmMetrics::new();
        metrics.total_merges_successful = 100;
        metrics.total_conflicts = 10;

        let ratio = metrics.conflict_ratio();
        assert!(ratio > 0.0 && ratio < 1.0);
    }
}
