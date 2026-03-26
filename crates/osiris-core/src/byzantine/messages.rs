//! Message types for Byzantine consensus protocol

use super::consensus::ProposalValue;
use super::{NodeId, Round};
use serde::{Deserialize, Serialize};

/// Message types in BFT protocol
#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
pub enum MessageType {
    /// Leader proposes a value
    Propose { round: Round, value: ProposalValue },

    /// Node approves proposal
    Approve { round: Round, value: ProposalValue },

    /// Leader sends commit instruction
    Commit { round: Round, value: ProposalValue },

    /// Request new leader (current is Byzantine)
    LeaderChange { reason: String },

    /// Heartbeat/keep-alive from leader
    Heartbeat { round: Round },

    /// Acknowledge receipt of heartbeat
    Ack { round: Round },
}

/// Protocol message
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct Message {
    pub id: String,
    pub sender: NodeId,
    pub msg_type: MessageType,
    pub timestamp: u64,
}

impl Message {
    /// Create new message
    pub fn new(sender: NodeId, msg_type: MessageType) -> Self {
        Self {
            id: uuid::Uuid::new_v4().to_string(),
            sender,
            msg_type,
            timestamp: std::time::SystemTime::now()
                .duration_since(std::time::UNIX_EPOCH)
                .unwrap()
                .as_secs(),
        }
    }

    /// Create proposal message
    pub fn propose(sender: NodeId, round: Round, value: ProposalValue) -> Self {
        Self::new(sender, MessageType::Propose { round, value })
    }

    /// Create approval message
    pub fn approve(sender: NodeId, round: Round, value: ProposalValue) -> Self {
        Self::new(sender, MessageType::Approve { round, value })
    }

    /// Create commit message
    pub fn commit(sender: NodeId, round: Round, value: ProposalValue) -> Self {
        Self::new(sender, MessageType::Commit { round, value })
    }

    /// Create leader change request
    pub fn leader_change(sender: NodeId, reason: String) -> Self {
        Self::new(sender, MessageType::LeaderChange { reason })
    }

    /// Create heartbeat message
    pub fn heartbeat(sender: NodeId, round: Round) -> Self {
        Self::new(sender, MessageType::Heartbeat { round })
    }

    /// Create ack message
    pub fn ack(sender: NodeId, round: Round) -> Self {
        Self::new(sender, MessageType::Ack { round })
    }

    /// Get the round number if applicable
    pub fn get_round(&self) -> Option<Round> {
        match self.msg_type {
            MessageType::Propose { round, .. } => Some(round),
            MessageType::Approve { round, .. } => Some(round),
            MessageType::Commit { round, .. } => Some(round),
            MessageType::Heartbeat { round } => Some(round),
            MessageType::Ack { round } => Some(round),
            MessageType::LeaderChange { .. } => None,
        }
    }

    /// Get the value if applicable
    pub fn get_value(&self) -> Option<&ProposalValue> {
        match &self.msg_type {
            MessageType::Propose { value, .. } => Some(value),
            MessageType::Approve { value, .. } => Some(value),
            MessageType::Commit { value, .. } => Some(value),
            _ => None,
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_message_creation() {
        let node_id = NodeId::new(1);
        let round = Round::new(1);
        let value = ProposalValue::new("test".to_string());

        let msg = Message::propose(node_id, round, value.clone());

        assert_eq!(msg.sender, node_id);
        assert_eq!(msg.get_round(), Some(round));
        assert_eq!(msg.get_value(), Some(&value));
    }

    #[test]
    fn test_approve_message() {
        let node_id = NodeId::new(2);
        let round = Round::new(1);
        let value = ProposalValue::new("test".to_string());

        let msg = Message::approve(node_id, round, value.clone());

        assert_eq!(msg.sender, node_id);
        assert_eq!(msg.get_round(), Some(round));
        assert_eq!(msg.get_value(), Some(&value));

        match msg.msg_type {
            MessageType::Approve { .. } => {}
            _ => panic!("Expected Approve message"),
        }
    }

    #[test]
    fn test_commit_message() {
        let node_id = NodeId::new(1);
        let round = Round::new(2);
        let value = ProposalValue::new("committed".to_string());

        let msg = Message::commit(node_id, round, value.clone());

        assert_eq!(msg.get_round(), Some(round));
        assert_eq!(msg.get_value(), Some(&value));

        match msg.msg_type {
            MessageType::Commit { .. } => {}
            _ => panic!("Expected Commit message"),
        }
    }

    #[test]
    fn test_leader_change_message() {
        let node_id = NodeId::new(3);
        let reason = "Byzantine behavior detected".to_string();

        let msg = Message::leader_change(node_id, reason.clone());

        assert_eq!(msg.sender, node_id);
        assert_eq!(msg.get_round(), None);
        assert_eq!(msg.get_value(), None);

        match msg.msg_type {
            MessageType::LeaderChange { reason: r } => assert_eq!(r, reason),
            _ => panic!("Expected LeaderChange message"),
        }
    }

    #[test]
    fn test_heartbeat_message() {
        let node_id = NodeId::new(1);
        let round = Round::new(5);

        let msg = Message::heartbeat(node_id, round);

        assert_eq!(msg.get_round(), Some(round));
        assert_eq!(msg.get_value(), None);
    }

    #[test]
    fn test_ack_message() {
        let node_id = NodeId::new(2);
        let round = Round::new(5);

        let msg = Message::ack(node_id, round);

        assert_eq!(msg.get_round(), Some(round));
        assert_eq!(msg.sender, node_id);
    }

    #[test]
    fn test_message_serialization() {
        let value = ProposalValue::new("data".to_string());
        let msg = Message::propose(NodeId::new(1), Round::new(1), value.clone());

        let json = serde_json::to_string(&msg).unwrap();
        let deserialized: Message = serde_json::from_str(&json).unwrap();

        assert_eq!(msg.sender, deserialized.sender);
        assert_eq!(msg.get_round(), deserialized.get_round());
    }

    #[test]
    fn test_message_round_extraction() {
        let node_id = NodeId::new(1);
        let round = Round::new(42);

        let propose = Message::propose(node_id, round, ProposalValue::new("a".to_string()));
        assert_eq!(propose.get_round(), Some(Round::new(42)));

        let change = Message::leader_change(node_id, "reason".to_string());
        assert_eq!(change.get_round(), None);
    }
}
