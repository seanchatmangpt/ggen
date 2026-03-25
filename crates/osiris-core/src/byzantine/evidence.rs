//! Evidence logging for Byzantine node behavior

use serde::{Deserialize, Serialize};
use std::collections::HashMap;
use uuid::Uuid;

use super::consensus::ProposalValue;
use super::NodeId;

/// Types of misbehavior evidence
#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
pub enum Misbehavior {
    /// Node sent conflicting votes for different values
    ConflictingVotes {
        value_a: ProposalValue,
        value_b: ProposalValue,
    },

    /// Node proposed multiple different values in same round
    DoubleProposal {
        round: u64,
        value_a: ProposalValue,
        value_b: ProposalValue,
    },

    /// Node failed to respond to proposal request
    Timeout { round: u64 },

    /// Node sent malformed or invalid message
    InvalidMessage { reason: String },

    /// Node equivocated (sent contradictory leader votes)
    Equivocation { details: String },
}

/// Evidence record for auditing
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct Evidence {
    pub id: String,
    pub accused_node: NodeId,
    pub misbehavior: Misbehavior,
    pub timestamp: u64,
    pub severity: EvidenceSeverity,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Serialize, Deserialize, PartialOrd, Ord)]
pub enum EvidenceSeverity {
    Low,
    Medium,
    High,
    Critical,
}

impl Evidence {
    pub fn new(accused_node: NodeId, misbehavior: Misbehavior) -> Self {
        let severity = match &misbehavior {
            Misbehavior::ConflictingVotes { .. } => EvidenceSeverity::Critical,
            Misbehavior::DoubleProposal { .. } => EvidenceSeverity::Critical,
            Misbehavior::Equivocation { .. } => EvidenceSeverity::High,
            Misbehavior::InvalidMessage { .. } => EvidenceSeverity::Medium,
            Misbehavior::Timeout { .. } => EvidenceSeverity::Low,
        };

        Self {
            id: Uuid::new_v4().to_string(),
            accused_node,
            misbehavior,
            timestamp: std::time::SystemTime::now()
                .duration_since(std::time::UNIX_EPOCH)
                .unwrap()
                .as_secs(),
            severity,
        }
    }

    pub fn with_severity(mut self, severity: EvidenceSeverity) -> Self {
        self.severity = severity;
        self
    }
}

/// Audit log for all evidence
#[derive(Debug, Clone)]
pub struct EvidenceLog {
    entries: Vec<Evidence>,
    node_misbehavior_count: HashMap<NodeId, usize>,
}

impl EvidenceLog {
    pub fn new() -> Self {
        Self {
            entries: Vec::new(),
            node_misbehavior_count: HashMap::new(),
        }
    }

    /// Record evidence
    pub fn add(&mut self, evidence: Evidence) {
        *self
            .node_misbehavior_count
            .entry(evidence.accused_node)
            .or_insert(0) += 1;
        self.entries.push(evidence);
    }

    /// Get all evidence
    pub fn get_all(&self) -> &[Evidence] {
        &self.entries
    }

    /// Get evidence for specific node
    pub fn get_for_node(&self, node_id: NodeId) -> Vec<&Evidence> {
        self.entries
            .iter()
            .filter(|e| e.accused_node == node_id)
            .collect()
    }

    /// Get misbehavior count for node
    pub fn misbehavior_count(&self, node_id: NodeId) -> usize {
        self.node_misbehavior_count
            .get(&node_id)
            .copied()
            .unwrap_or(0)
    }

    /// Get critical evidence (high conviction of Byzantine behavior)
    pub fn get_critical(&self) -> Vec<&Evidence> {
        self.entries
            .iter()
            .filter(|e| e.severity == EvidenceSeverity::Critical)
            .collect()
    }

    /// Should node be isolated?
    pub fn should_isolate(&self, node_id: NodeId) -> bool {
        let critical_count = self
            .get_for_node(node_id)
            .iter()
            .filter(|e| e.severity == EvidenceSeverity::Critical)
            .count();

        // Isolate after 2 critical violations
        critical_count >= 2
    }

    /// Get nodes that should be isolated
    pub fn get_isolated_nodes(&self) -> Vec<NodeId> {
        let mut nodes: Vec<_> = self
            .node_misbehavior_count
            .keys()
            .filter(|&&node_id| self.should_isolate(node_id))
            .copied()
            .collect();
        nodes.sort();
        nodes
    }
}

impl Default for EvidenceLog {
    fn default() -> Self {
        Self::new()
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_evidence_creation() {
        let node_id = NodeId::new(1);
        let value_a = ProposalValue::new("data_a".to_string());
        let value_b = ProposalValue::new("data_b".to_string());

        let evidence = Evidence::new(node_id, Misbehavior::ConflictingVotes { value_a, value_b });

        assert_eq!(evidence.accused_node, node_id);
        assert_eq!(evidence.severity, EvidenceSeverity::Critical);
    }

    #[test]
    fn test_evidence_severity_levels() {
        let node_id = NodeId::new(1);

        let conflicting = Evidence::new(
            node_id,
            Misbehavior::ConflictingVotes {
                value_a: ProposalValue::new("a".to_string()),
                value_b: ProposalValue::new("b".to_string()),
            },
        );
        assert_eq!(conflicting.severity, EvidenceSeverity::Critical);

        let timeout = Evidence::new(node_id, Misbehavior::Timeout { round: 1 });
        assert_eq!(timeout.severity, EvidenceSeverity::Low);

        let invalid = Evidence::new(
            node_id,
            Misbehavior::InvalidMessage {
                reason: "bad format".to_string(),
            },
        );
        assert_eq!(invalid.severity, EvidenceSeverity::Medium);
    }

    #[test]
    fn test_evidence_log_add() {
        let mut log = EvidenceLog::new();
        let node_id = NodeId::new(1);

        let evidence = Evidence::new(node_id, Misbehavior::Timeout { round: 1 });

        log.add(evidence);
        assert_eq!(log.misbehavior_count(node_id), 1);
        assert_eq!(log.get_all().len(), 1);
    }

    #[test]
    fn test_evidence_log_critical_evidence() {
        let mut log = EvidenceLog::new();
        let node_id = NodeId::new(1);
        let value_a = ProposalValue::new("a".to_string());
        let value_b = ProposalValue::new("b".to_string());

        // Add critical evidence
        log.add(Evidence::new(
            node_id,
            Misbehavior::ConflictingVotes {
                value_a: value_a.clone(),
                value_b: value_b.clone(),
            },
        ));

        let critical = log.get_critical();
        assert_eq!(critical.len(), 1);
    }

    #[test]
    fn test_evidence_log_isolation() {
        let mut log = EvidenceLog::new();
        let node_id = NodeId::new(1);
        let value_a = ProposalValue::new("a".to_string());
        let value_b = ProposalValue::new("b".to_string());

        // One critical evidence - not enough
        log.add(Evidence::new(
            node_id,
            Misbehavior::ConflictingVotes {
                value_a: value_a.clone(),
                value_b: value_b.clone(),
            },
        ));

        assert!(!log.should_isolate(node_id));

        // Second critical evidence - now isolate
        log.add(Evidence::new(
            node_id,
            Misbehavior::DoubleProposal {
                round: 1,
                value_a: value_a.clone(),
                value_b,
            },
        ));

        assert!(log.should_isolate(node_id));
    }

    #[test]
    fn test_get_isolated_nodes() {
        let mut log = EvidenceLog::new();
        let node1 = NodeId::new(1);
        let node2 = NodeId::new(2);
        let value_a = ProposalValue::new("a".to_string());
        let value_b = ProposalValue::new("b".to_string());

        // Isolate node 1
        for _ in 0..2 {
            log.add(Evidence::new(
                node1,
                Misbehavior::ConflictingVotes {
                    value_a: value_a.clone(),
                    value_b: value_b.clone(),
                },
            ));
        }

        // Add some evidence to node 2 but not enough to isolate
        log.add(Evidence::new(node2, Misbehavior::Timeout { round: 1 }));

        let isolated = log.get_isolated_nodes();
        assert_eq!(isolated.len(), 1);
        assert!(isolated.contains(&node1));
    }
}
