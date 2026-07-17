//! Swarm Member: Individual Agent in the Swarm
//!
//! Represents a single agent participating in the swarm. Each member has
//! capabilities (sectors it can handle), state, and communication with other members.

use serde::{Deserialize, Serialize};
use std::collections::HashMap;

/// Current state of a swarm member
#[derive(Debug, Clone, Copy, PartialEq, Eq, Serialize, Deserialize)]
pub enum MemberState {
    /// Member is alive and responsive
    Alive,
    /// Member is busy (executing a task)
    Busy,
    /// Member is temporarily offline
    Offline,
    /// Member has failed and is unavailable
    Failed,
}

impl std::fmt::Display for MemberState {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Alive => write!(f, "Alive"),
            Self::Busy => write!(f, "Busy"),
            Self::Offline => write!(f, "Offline"),
            Self::Failed => write!(f, "Failed"),
        }
    }
}

/// A member of the swarm
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct SwarmMember {
    /// Unique member identifier
    pub id: String,
    /// Human-readable name
    pub name: String,
    /// Sectors this member can handle
    pub sectors: Vec<String>,
    /// Current state
    pub state: MemberState,
    /// Task capacity (max concurrent tasks)
    pub capacity: u32,
    /// Current task count
    pub current_tasks: u32,
    /// Ontologies this member knows about
    pub ontologies: HashMap<String, bool>,
    /// Last heartbeat timestamp
    pub last_heartbeat: String,
    /// Reputation score (0-100)
    pub reputation: u32,
}

impl SwarmMember {
    /// Create a new swarm member
    #[must_use]
    pub fn new(id: String, name: String) -> Self {
        Self {
            id,
            name,
            sectors: Vec::new(),
            state: MemberState::Alive,
            capacity: 10,
            current_tasks: 0,
            ontologies: HashMap::new(),
            last_heartbeat: chrono::Utc::now().to_rfc3339(),
            reputation: 100,
        }
    }

    /// Add a sector capability
    #[must_use]
    pub fn with_sector(mut self, sector: String) -> Self {
        if !self.sectors.contains(&sector) {
            self.sectors.push(sector);
        }
        self
    }

    /// Add a sector capability (alias for `with_sector` for API consistency)
    #[must_use]
    pub fn add_sector(self, sector: String) -> Self {
        self.with_sector(sector)
    }

    /// Add multiple sectors
    #[must_use]
    pub fn with_sectors(mut self, sectors: Vec<String>) -> Self {
        for sector in sectors {
            if !self.sectors.contains(&sector) {
                self.sectors.push(sector);
            }
        }
        self
    }

    /// Set capacity
    #[must_use]
    #[allow(clippy::missing_const_for_fn)] // Mutating self is not const
    pub fn with_capacity(mut self, capacity: u32) -> Self {
        self.capacity = capacity;
        self
    }

    /// Check if member can handle a sector
    #[must_use]
    pub fn can_handle(&self, sector: &str) -> bool {
        self.sectors.iter().any(|s| s == sector)
    }

    /// Check if member has available capacity
    #[must_use]
    pub const fn has_capacity(&self) -> bool {
        self.current_tasks < self.capacity
    }

    /// Assign a task to this member
    ///
    /// # Errors
    ///
    /// Returns `Err(String)` if the member is at capacity or not in the Alive state.
    pub fn assign_task(&mut self) -> Result<(), String> {
        if !self.has_capacity() {
            return Err("Member at capacity".to_string());
        }
        if self.state != MemberState::Alive {
            return Err(format!("Member is {}", self.state));
        }
        self.current_tasks += 1;
        if self.current_tasks >= self.capacity {
            self.state = MemberState::Busy;
        }
        Ok(())
    }

    /// Complete a task
    pub fn complete_task(&mut self) {
        if self.current_tasks > 0 {
            self.current_tasks -= 1;
        }
        if self.current_tasks < self.capacity {
            self.state = MemberState::Alive;
        }
        self.last_heartbeat = chrono::Utc::now().to_rfc3339();
    }

    /// Update heartbeat
    pub fn heartbeat(&mut self) {
        self.last_heartbeat = chrono::Utc::now().to_rfc3339();
        if self.state == MemberState::Offline {
            self.state = MemberState::Alive;
        }
    }

    /// Register knowledge of an ontology
    pub fn register_ontology(&mut self, sector: String) {
        self.ontologies.insert(sector, true);
    }

    /// Update reputation
    #[allow(clippy::cast_possible_wrap, clippy::cast_sign_loss)] // Reputation is bounded 0-100, so conversions are safe
    pub fn update_reputation(&mut self, delta: i32) {
        let rep_as_i32 = self.reputation as i32;
        let new_rep = (rep_as_i32 + delta).clamp(0, 100);
        self.reputation = new_rep as u32;
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_member_creation() {
        let member = SwarmMember::new("agent-1".to_string(), "Academic Agent".to_string());

        assert_eq!(member.id, "agent-1");
        assert_eq!(member.name, "Academic Agent");
        assert_eq!(member.state, MemberState::Alive);
        assert_eq!(member.capacity, 10);
    }

    #[test]
    fn test_member_with_sectors() {
        let member = SwarmMember::new("agent-1".to_string(), "Multi-Agent".to_string())
            .with_sector("Academic".to_string())
            .with_sector("Claims".to_string());

        assert_eq!(member.sectors.len(), 2);
        assert!(member.can_handle("Academic"));
        assert!(member.can_handle("Claims"));
        assert!(!member.can_handle("Unknown"));
    }

    #[test]
    fn test_member_capacity() {
        let mut member =
            SwarmMember::new("agent-1".to_string(), "Agent".to_string()).with_capacity(2);

        assert!(member.has_capacity());
        assert!(member.assign_task().is_ok());
        assert!(member.has_capacity());
        assert!(member.assign_task().is_ok());
        assert!(!member.has_capacity());
        assert!(member.assign_task().is_err());
    }

    #[test]
    #[allow(clippy::unwrap_used)]
    fn test_member_state_transitions() {
        let mut member =
            SwarmMember::new("agent-1".to_string(), "Agent".to_string()).with_capacity(1);

        assert_eq!(member.state, MemberState::Alive);

        member.assign_task().unwrap(); // Test code: unwrap is acceptable
        assert_eq!(member.state, MemberState::Busy);

        member.complete_task();
        assert_eq!(member.state, MemberState::Alive);
    }

    #[test]
    fn test_member_reputation() {
        let mut member = SwarmMember::new("agent-1".to_string(), "Agent".to_string());

        assert_eq!(member.reputation, 100);

        member.update_reputation(-10);
        assert_eq!(member.reputation, 90);

        member.update_reputation(-200);
        assert_eq!(member.reputation, 0); // Min bound

        member.update_reputation(150);
        assert_eq!(member.reputation, 100); // Max bound
    }

    #[test]
    fn test_member_state_display() {
        assert_eq!(MemberState::Alive.to_string(), "Alive");
        assert_eq!(MemberState::Busy.to_string(), "Busy");
        assert_eq!(MemberState::Failed.to_string(), "Failed");
    }
}
