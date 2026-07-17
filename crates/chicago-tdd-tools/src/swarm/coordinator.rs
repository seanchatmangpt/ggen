//! Swarm Coordinator: Central Orchestration Logic
//!
//! Coordinates swarm members, manages task distribution, and ensures
//! deterministic consensus across the swarm.

use super::member::SwarmMember;
use super::task::{TaskQueue, TaskReceipt, TaskRequest};
use serde::{Deserialize, Serialize};
use std::collections::HashMap;

/// Swarm membership (list of active members)
#[derive(Debug, Clone, Default, Serialize, Deserialize)]
pub struct SwarmMembership {
    /// Swarm name
    pub swarm_id: String,
    /// Active members
    members: HashMap<String, SwarmMember>,
}

impl SwarmMembership {
    /// Create a new swarm membership
    #[must_use]
    pub fn new() -> Self {
        Self { swarm_id: format!("swarm-{}", uuid::Uuid::new_v4()), members: HashMap::new() }
    }

    /// Add a member to the swarm
    pub fn add_member(&mut self, member: SwarmMember) {
        self.members.insert(member.id.clone(), member);
    }

    /// Remove a member from the swarm
    pub fn remove_member(&mut self, member_id: &str) {
        self.members.remove(member_id);
    }

    /// Get a member by ID
    #[must_use]
    pub fn get_member(&self, member_id: &str) -> Option<&SwarmMember> {
        self.members.get(member_id)
    }

    /// Get mutable reference to member
    pub fn get_member_mut(&mut self, member_id: &str) -> Option<&mut SwarmMember> {
        self.members.get_mut(member_id)
    }

    /// Get all members
    #[must_use]
    pub const fn members(&self) -> &HashMap<String, SwarmMember> {
        &self.members
    }

    /// Get members handling a specific sector
    #[must_use]
    pub fn members_for_sector(&self, sector: &str) -> Vec<&SwarmMember> {
        self.members.values().filter(|m| m.can_handle(sector)).collect()
    }

    /// Get member count
    #[must_use]
    pub fn member_count(&self) -> usize {
        self.members.len()
    }

    /// Get active member count
    #[must_use]
    pub fn active_members(&self) -> usize {
        self.members
            .values()
            .filter(|m| m.state == super::member::MemberState::Alive)
            .count()
    }

    /// Get total capacity
    #[must_use]
    pub fn total_capacity(&self) -> u32 {
        self.members.values().map(|m| m.capacity).sum()
    }

    /// Get total current tasks
    #[must_use]
    pub fn total_current_tasks(&self) -> u32 {
        self.members.values().map(|m| m.current_tasks).sum()
    }
}

/// Default consensus threshold: 66% of swarm members must agree.
const DEFAULT_CONSENSUS_THRESHOLD: f32 = 0.66;

/// Coordinates swarm operations and task distribution
pub struct SwarmCoordinator {
    /// Swarm membership
    pub membership: SwarmMembership,
    /// Task queue
    pub task_queue: TaskQueue,
    /// Task-to-member assignments
    task_assignments: HashMap<String, String>,
    /// Consensus threshold (% of members that must agree)
    consensus_threshold: f32,
}

impl SwarmCoordinator {
    /// Create a new swarm coordinator
    #[must_use]
    pub fn new() -> Self {
        Self {
            membership: SwarmMembership::new(),
            task_queue: TaskQueue::new(),
            task_assignments: HashMap::new(),
            consensus_threshold: DEFAULT_CONSENSUS_THRESHOLD,
        }
    }

    /// Register a member with the coordinator
    pub fn register_member(&mut self, member: SwarmMember) {
        self.membership.add_member(member);
    }

    /// Submit a task to the swarm
    pub fn submit_task(&mut self, task: TaskRequest) {
        self.task_queue.enqueue(task);
    }

    /// Assign next queued task to an available member
    ///
    /// # Errors
    ///
    /// Returns `Err(String)` if no tasks are queued or no available members can handle the task.
    pub fn distribute_next_task(&mut self) -> Result<(String, String), String> {
        let Some(task) = self.task_queue.dequeue() else {
            return Err("No tasks queued".to_string());
        };

        // Find best member for task
        let member_id = self.find_best_member(&task)?;

        // Assign task to member
        if let Some(member) = self.membership.get_member_mut(&member_id) {
            member.assign_task()?;
        }

        self.task_assignments.insert(task.id.clone(), member_id.clone());

        Ok((task.id, member_id))
    }

    /// Find the best member for a task
    fn find_best_member(&self, task: &TaskRequest) -> Result<String, String> {
        let mut candidates = vec![];

        // Find members that can handle all required sectors
        for sector in &task.sectors {
            let sector_members = self.membership.members_for_sector(sector);
            for member in sector_members {
                if member.has_capacity() {
                    candidates.push((member.id.clone(), member.reputation));
                }
            }
        }

        if candidates.is_empty() {
            return Err("No available members for task".to_string());
        }

        // Sort by reputation (highest first) and return best
        candidates.sort_by(|a, b| b.1.cmp(&a.1));
        Ok(candidates[0].0.clone())
    }

    /// Record task completion
    pub fn record_completion(&mut self, receipt: TaskReceipt) {
        // Update member state
        if let Some(member) = self.membership.get_member_mut(&receipt.agent_id) {
            member.complete_task();

            // Update reputation based on success
            if receipt.is_success() {
                member.update_reputation(5);
            } else {
                member.update_reputation(-10);
            }
        }

        self.task_queue.record_receipt(receipt);
    }

    /// Check swarm consensus on a result
    #[must_use]
    #[allow(clippy::cast_precision_loss)] // Precision loss acceptable for consensus calculation (usize to f32)
    pub fn check_consensus(&self, sector: &str) -> bool {
        let members = self.membership.members_for_sector(sector);
        let total = members.len() as f32;
        let active = members.iter().filter(|m| m.current_tasks == 0).count() as f32;

        if total == 0.0 {
            return false;
        }

        active / total >= self.consensus_threshold
    }

    /// Get swarm status
    #[must_use]
    pub fn status(&self) -> SwarmStatus {
        SwarmStatus {
            swarm_id: self.membership.swarm_id.clone(),
            total_members: self.membership.member_count(),
            active_members: self.membership.active_members(),
            total_capacity: self.membership.total_capacity(),
            current_tasks: self.membership.total_current_tasks(),
            queued_tasks: self.task_queue.task_count(),
            completed_tasks: self.task_queue.receipt_count(),
        }
    }
}

impl Default for SwarmCoordinator {
    fn default() -> Self {
        Self::new()
    }
}

/// Status snapshot of the swarm
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct SwarmStatus {
    /// Swarm identifier
    pub swarm_id: String,
    /// Total registered members
    pub total_members: usize,
    /// Currently active members
    pub active_members: usize,
    /// Total capacity across swarm
    pub total_capacity: u32,
    /// Current executing tasks
    pub current_tasks: u32,
    /// Queued tasks waiting
    pub queued_tasks: usize,
    /// Completed tasks
    pub completed_tasks: usize,
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_swarm_membership() {
        let mut membership = SwarmMembership::new();

        let member = SwarmMember::new("agent-1".to_string(), "Agent 1".to_string())
            .with_sector("Academic".to_string());

        membership.add_member(member);
        assert_eq!(membership.member_count(), 1);
    }

    #[test]
    fn test_swarm_coordinator_creation() {
        let coordinator = SwarmCoordinator::new();

        assert_eq!(coordinator.membership.member_count(), 0);
        assert_eq!(coordinator.task_queue.task_count(), 0);
    }

    #[test]
    fn test_register_member() {
        let mut coordinator = SwarmCoordinator::new();

        let member = SwarmMember::new("agent-1".to_string(), "Agent".to_string())
            .with_sector("Academic".to_string());

        coordinator.register_member(member);
        assert_eq!(coordinator.membership.member_count(), 1);
    }

    #[test]
    fn test_submit_task() {
        let mut coordinator = SwarmCoordinator::new();

        let task = TaskRequest::new(
            "task-1".to_string(),
            "Academic".to_string(),
            "desk-review".to_string(),
            "paper".to_string(),
        );

        coordinator.submit_task(task);
        assert_eq!(coordinator.task_queue.task_count(), 1);
    }

    #[test]
    fn test_distribute_task() {
        let mut coordinator = SwarmCoordinator::new();

        let member = SwarmMember::new("agent-1".to_string(), "Agent".to_string())
            .with_sector("Academic".to_string())
            .with_capacity(10);

        coordinator.register_member(member);

        let task = TaskRequest::new(
            "task-1".to_string(),
            "Academic".to_string(),
            "op".to_string(),
            "data".to_string(),
        );

        coordinator.submit_task(task);

        let result = coordinator.distribute_next_task();
        assert!(result.is_ok());

        let (task_id, member_id) = result.unwrap(); // Test code: unwrap is acceptable
        assert_eq!(task_id, "task-1");
        assert_eq!(member_id, "agent-1");
    }

    #[test]
    fn test_swarm_status() {
        let mut coordinator = SwarmCoordinator::new();

        let member = SwarmMember::new("agent-1".to_string(), "Agent".to_string()).with_capacity(5);

        coordinator.register_member(member);

        let status = coordinator.status();
        assert_eq!(status.total_members, 1);
        assert_eq!(status.total_capacity, 5);
    }

    #[test]
    fn test_members_for_sector() {
        let mut membership = SwarmMembership::new();

        membership.add_member(
            SwarmMember::new("a1".to_string(), "Agent 1".to_string())
                .with_sector("Academic".to_string()),
        );
        membership.add_member(
            SwarmMember::new("a2".to_string(), "Agent 2".to_string())
                .with_sector("Claims".to_string()),
        );

        let academic_members = membership.members_for_sector("Academic");
        assert_eq!(academic_members.len(), 1);
    }
}
