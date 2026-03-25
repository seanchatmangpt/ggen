//! Message types for PBFT consensus protocol
use chrono::{DateTime, Utc};
use serde::{Deserialize, Serialize};
use std::fmt;

/// Types of messages in the PBFT protocol
#[derive(Debug, Clone, Serialize, Deserialize, PartialEq, Eq, Hash)]
pub enum MessageType {
    /// Primary proposes a value to all replicas
    PrePrepare,
    /// Replica acknowledges pre-prepare and prepares the value
    Prepare,
    /// Replica commits after seeing 2f+1 prepares
    Commit,
    /// View change request (triggered on primary timeout)
    ViewChange,
}

impl fmt::Display for MessageType {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            MessageType::PrePrepare => write!(f, "PRE_PREPARE"),
            MessageType::Prepare => write!(f, "PREPARE"),
            MessageType::Commit => write!(f, "COMMIT"),
            MessageType::ViewChange => write!(f, "VIEW_CHANGE"),
        }
    }
}

/// PBFT consensus message
#[derive(Debug, Clone, Serialize, Deserialize, PartialEq, Eq)]
pub struct Message {
    /// Message type (pre-prepare, prepare, commit, view-change)
    pub msg_type: MessageType,
    /// Sending node ID
    pub sender_id: u64,
    /// Current view/primary number
    pub view: u64,
    /// Consensus round number
    pub round: u64,
    /// Proposed/committed value
    pub value: Option<String>,
    /// Cryptographic digest/hash of the value
    pub digest: Option<String>,
    /// Timestamp when message was created
    pub timestamp: DateTime<Utc>,
}

impl Message {
    /// Create a new pre-prepare message
    pub fn pre_prepare(
        sender_id: u64, view: u64, round: u64, value: String, digest: String,
    ) -> Self {
        Self {
            msg_type: MessageType::PrePrepare,
            sender_id,
            view,
            round,
            value: Some(value),
            digest: Some(digest),
            timestamp: Utc::now(),
        }
    }

    /// Create a new prepare message
    pub fn prepare(sender_id: u64, view: u64, round: u64, digest: String) -> Self {
        Self {
            msg_type: MessageType::Prepare,
            sender_id,
            view,
            round,
            value: None,
            digest: Some(digest),
            timestamp: Utc::now(),
        }
    }

    /// Create a new commit message
    pub fn commit(sender_id: u64, view: u64, round: u64, digest: String) -> Self {
        Self {
            msg_type: MessageType::Commit,
            sender_id,
            view,
            round,
            value: None,
            digest: Some(digest),
            timestamp: Utc::now(),
        }
    }

    /// Create a view change message
    pub fn view_change(sender_id: u64, view: u64, round: u64) -> Self {
        Self {
            msg_type: MessageType::ViewChange,
            sender_id,
            view,
            round,
            value: None,
            digest: None,
            timestamp: Utc::now(),
        }
    }

    /// Verify message is valid for given parameters
    pub fn is_valid(&self, expected_view: u64, expected_round: u64) -> bool {
        // Must match current view and round
        self.view == expected_view && self.round == expected_round &&
        // Must not be from ourselves
        // (checked elsewhere as we don't have our node ID here)
        // Must have required fields
        match self.msg_type {
            MessageType::PrePrepare => {
                self.value.is_some() && self.digest.is_some()
            }
            MessageType::Prepare => self.digest.is_some(),
            MessageType::Commit => self.digest.is_some(),
            MessageType::ViewChange => true,
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_message_creation() {
        let msg = Message::pre_prepare(0, 0, 1, "value".to_string(), "hash".to_string());
        assert_eq!(msg.msg_type, MessageType::PrePrepare);
        assert_eq!(msg.sender_id, 0);
        assert_eq!(msg.value, Some("value".to_string()));
    }

    #[test]
    fn test_message_validity() {
        let msg = Message::pre_prepare(0, 0, 1, "value".to_string(), "hash".to_string());
        assert!(msg.is_valid(0, 1));
        assert!(!msg.is_valid(1, 1));
        assert!(!msg.is_valid(0, 2));
    }

    #[test]
    fn test_prepare_message() {
        let msg = Message::prepare(1, 0, 1, "hash".to_string());
        assert_eq!(msg.msg_type, MessageType::Prepare);
        assert_eq!(msg.sender_id, 1);
        assert!(msg.value.is_none());
        assert_eq!(msg.digest, Some("hash".to_string()));
    }

    #[test]
    fn test_message_type_display() {
        assert_eq!(MessageType::PrePrepare.to_string(), "PRE_PREPARE");
        assert_eq!(MessageType::Prepare.to_string(), "PREPARE");
        assert_eq!(MessageType::Commit.to_string(), "COMMIT");
        assert_eq!(MessageType::ViewChange.to_string(), "VIEW_CHANGE");
    }
}
