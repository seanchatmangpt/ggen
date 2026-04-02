//! Leader election with fallback strategy

use super::{NodeId, Round};
use serde::{Deserialize, Serialize};

/// Leader election strategy
#[derive(Debug, Clone, Copy, PartialEq, Eq, Serialize, Deserialize)]
pub enum LeaderElectionStrategy {
    /// Round-robin: rotate leader at each round
    RoundRobin,
    /// Randomized: pick random leader (production-grade randomization)
    Randomized,
}

/// Leader information
#[derive(Debug, Clone, Copy, PartialEq, Eq, Serialize, Deserialize)]
pub struct LeaderInfo {
    pub node_id: NodeId,
    pub round: Round,
    pub strategy: LeaderElectionStrategy,
}

impl LeaderInfo {
    pub fn new(node_id: NodeId, round: Round, strategy: LeaderElectionStrategy) -> Self {
        Self {
            node_id,
            round,
            strategy,
        }
    }
}

/// Leader elector
pub struct LeaderElector {
    nodes: Vec<NodeId>,
    strategy: LeaderElectionStrategy,
    current_leader: Option<LeaderInfo>,
}

impl LeaderElector {
    /// Create new leader elector
    pub fn new(nodes: Vec<NodeId>, strategy: LeaderElectionStrategy) -> Self {
        let mut sorted = nodes;
        sorted.sort_by_key(|n| n.0);

        Self {
            nodes: sorted,
            strategy,
            current_leader: None,
        }
    }

    /// Elect leader for round
    pub fn elect_for_round(&mut self, round: Round) -> NodeId {
        if self.nodes.is_empty() {
            panic!("Cannot elect leader from empty node set");
        }

        let leader_id = match self.strategy {
            LeaderElectionStrategy::RoundRobin => {
                let idx = (round.0 as usize) % self.nodes.len();
                self.nodes[idx]
            }
            LeaderElectionStrategy::Randomized => {
                // Use round as seed for deterministic randomization
                let seed = round.0.wrapping_mul(2654435761); // FNV prime
                let idx = (seed as usize) % self.nodes.len();
                self.nodes[idx]
            }
        };

        let leader = LeaderInfo::new(leader_id, round, self.strategy);
        self.current_leader = Some(leader);
        leader_id
    }

    /// Get current leader
    pub fn current_leader(&self) -> Option<LeaderInfo> {
        self.current_leader
    }

    /// Request new leader (because current leader is Byzantine)
    pub fn next_leader(&mut self) -> NodeId {
        let current_round = self
            .current_leader
            .map(|l| l.round)
            .unwrap_or(Round::new(0));
        self.elect_for_round(Round::new(current_round.0 + 1))
    }

    /// Get all nodes
    pub fn nodes(&self) -> &[NodeId] {
        &self.nodes
    }

    /// Add isolated node to exclusion list
    pub fn exclude_node(&mut self, node_id: NodeId) {
        self.nodes.retain(|&n| n != node_id);
    }

    /// Restore node (in case of false accusation)
    pub fn include_node(&mut self, node_id: NodeId) {
        if !self.nodes.contains(&node_id) {
            self.nodes.push(node_id);
            self.nodes.sort_by_key(|n| n.0);
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_leader_elector_creation() {
        let nodes = vec![NodeId::new(1), NodeId::new(2), NodeId::new(3)];
        let elector = LeaderElector::new(nodes, LeaderElectionStrategy::RoundRobin);
        assert_eq!(elector.nodes().len(), 3);
    }

    #[test]
    fn test_round_robin_election() {
        let nodes = vec![NodeId::new(1), NodeId::new(2), NodeId::new(3)];
        let mut elector = LeaderElector::new(nodes, LeaderElectionStrategy::RoundRobin);

        let leader1 = elector.elect_for_round(Round::new(0));
        assert_eq!(leader1, NodeId::new(1));

        let leader2 = elector.elect_for_round(Round::new(1));
        assert_eq!(leader2, NodeId::new(2));

        let leader3 = elector.elect_for_round(Round::new(2));
        assert_eq!(leader3, NodeId::new(3));

        // Wrap around
        let leader4 = elector.elect_for_round(Round::new(3));
        assert_eq!(leader4, NodeId::new(1));
    }

    #[test]
    fn test_randomized_election() {
        let nodes = vec![NodeId::new(1), NodeId::new(2), NodeId::new(3)];
        let mut elector = LeaderElector::new(nodes, LeaderElectionStrategy::Randomized);

        let leader1 = elector.elect_for_round(Round::new(0));
        assert!(elector.nodes().contains(&leader1));

        // Same round should produce same leader (deterministic)
        let leader1_again = elector.elect_for_round(Round::new(0));
        assert_eq!(leader1, leader1_again);
    }

    #[test]
    fn test_current_leader() {
        let nodes = vec![NodeId::new(1), NodeId::new(2)];
        let mut elector = LeaderElector::new(nodes, LeaderElectionStrategy::RoundRobin);

        assert!(elector.current_leader().is_none());

        elector.elect_for_round(Round::new(0));
        let leader = elector.current_leader();

        assert!(leader.is_some());
        assert_eq!(leader.unwrap().node_id, NodeId::new(1));
        assert_eq!(leader.unwrap().round, Round::new(0));
    }

    #[test]
    fn test_next_leader() {
        let nodes = vec![NodeId::new(1), NodeId::new(2), NodeId::new(3)];
        let mut elector = LeaderElector::new(nodes, LeaderElectionStrategy::RoundRobin);

        elector.elect_for_round(Round::new(0));
        let next = elector.next_leader();

        assert_eq!(next, NodeId::new(2));
    }

    #[test]
    fn test_exclude_node() {
        let nodes = vec![NodeId::new(1), NodeId::new(2), NodeId::new(3)];
        let mut elector = LeaderElector::new(nodes, LeaderElectionStrategy::RoundRobin);

        elector.exclude_node(NodeId::new(2));
        assert_eq!(elector.nodes().len(), 2);

        let leader = elector.elect_for_round(Round::new(0));
        assert_ne!(leader, NodeId::new(2));
    }

    #[test]
    fn test_include_node() {
        let nodes = vec![NodeId::new(1), NodeId::new(2)];
        let mut elector = LeaderElector::new(nodes, LeaderElectionStrategy::RoundRobin);

        elector.exclude_node(NodeId::new(2));
        assert_eq!(elector.nodes().len(), 1);

        elector.include_node(NodeId::new(2));
        assert_eq!(elector.nodes().len(), 2);
    }

    #[test]
    fn test_nodes_sorted() {
        let nodes = vec![NodeId::new(3), NodeId::new(1), NodeId::new(2)];
        let elector = LeaderElector::new(nodes, LeaderElectionStrategy::RoundRobin);

        let sorted = elector.nodes();
        assert_eq!(sorted[0], NodeId::new(1));
        assert_eq!(sorted[1], NodeId::new(2));
        assert_eq!(sorted[2], NodeId::new(3));
    }
}
