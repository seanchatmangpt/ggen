//! Individual consensus node implementation
use crate::messages::Message;
use crate::receipts::Receipt;
use crate::state::ConsensusState;
use dashmap::DashMap;
use std::sync::Arc;

/// A node participating in PBFT consensus
#[derive(Clone)]
pub struct Node {
    /// Unique node identifier
    pub id: u64,
    /// Total nodes in the system
    pub total_nodes: u64,
    /// Current consensus state
    state: Arc<tokio::sync::Mutex<ConsensusState>>,
    /// Received messages (msg_hash -> Message)
    messages: Arc<DashMap<String, Message>>,
    /// Finalized receipts from decided rounds
    receipts: Arc<DashMap<u64, Receipt>>,
    /// Message processing flags (to prevent duplicates)
    processed_messages: Arc<DashMap<String, bool>>,
}

impl Node {
    /// Create a new consensus node
    pub fn new(id: u64, total_nodes: u64) -> Self {
        Self {
            id,
            total_nodes,
            state: Arc::new(tokio::sync::Mutex::new(ConsensusState::new(0, 0))),
            messages: Arc::new(DashMap::new()),
            receipts: Arc::new(DashMap::new()),
            processed_messages: Arc::new(DashMap::new()),
        }
    }

    /// Get current state snapshot
    pub async fn get_state(&self) -> ConsensusState {
        self.state.lock().await.clone()
    }

    /// Set current state
    pub async fn set_state(&self, state: ConsensusState) {
        *self.state.lock().await = state;
    }

    /// Add a message to the node's message buffer
    pub fn add_message(&self, message: Message) -> Result<(), String> {
        let msg_key = format!("{}_{}_{}", message.msg_type, message.view, message.round);

        // Check for duplicate
        if self.processed_messages.contains_key(&msg_key) {
            return Err(format!("Duplicate message: {}", msg_key));
        }

        self.messages.insert(msg_key.clone(), message);
        self.processed_messages.insert(msg_key, true);
        Ok(())
    }

    /// Get a message if it exists
    pub fn get_message(&self, key: &str) -> Option<Message> {
        self.messages.get(key).map(|m| m.clone())
    }

    /// Count messages of a specific type for current round
    pub fn count_message_type(&self, msg_type: &str) -> usize {
        self.messages
            .iter()
            .filter(|entry| entry.key().starts_with(msg_type))
            .count()
    }

    /// Add a finalized receipt (idempotent - doesn't error if already exists)
    pub fn add_receipt(&self, receipt: Receipt) -> Result<(), String> {
        self.receipts.insert(receipt.round, receipt);
        Ok(())
    }

    /// Get a receipt if it exists
    pub fn get_receipt(&self, round: u64) -> Option<Receipt> {
        self.receipts.get(&round).map(|r| r.clone())
    }

    /// Clear messages for a new round
    pub async fn clear_messages(&self) {
        self.messages.clear();
        self.processed_messages.clear();
    }

    /// Calculate quorum size for this node count (2f+1 where f = (n-1)/3)
    pub fn quorum_size(&self) -> usize {
        (((self.total_nodes - 1) / 3) * 2 + 1) as usize
    }

    /// Determine which node is primary for current view
    pub fn primary_id(&self, view: u64) -> u64 {
        view % self.total_nodes
    }

    /// Check if this node is the primary
    pub async fn is_primary(&self) -> bool {
        let state = self.state.lock().await;
        self.id == self.primary_id(state.view)
    }

    /// Get all current messages
    pub fn get_all_messages(&self) -> Vec<Message> {
        self.messages
            .iter()
            .map(|entry| entry.value().clone())
            .collect()
    }

    /// Get all receipts
    pub fn get_all_receipts(&self) -> Vec<Receipt> {
        self.receipts
            .iter()
            .map(|entry| entry.value().clone())
            .collect()
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::messages::MessageType;

    #[tokio::test]
    async fn test_node_creation() {
        let node = Node::new(0, 4);
        assert_eq!(node.id, 0);
        assert_eq!(node.total_nodes, 4);

        let state = node.get_state().await;
        assert_eq!(state.round, 0);
    }

    #[tokio::test]
    async fn test_quorum_size_4_nodes() {
        let node = Node::new(0, 4);
        assert_eq!(node.quorum_size(), 3); // 2*1 + 1
    }

    #[tokio::test]
    async fn test_quorum_size_10_nodes() {
        let node = Node::new(0, 10);
        assert_eq!(node.quorum_size(), 7); // 2*3 + 1
    }

    #[tokio::test]
    async fn test_primary_id() {
        let node = Node::new(0, 4);
        assert_eq!(node.primary_id(0), 0);
        assert_eq!(node.primary_id(1), 1);
        assert_eq!(node.primary_id(4), 0); // Wraps around
    }

    #[tokio::test]
    async fn test_is_primary() {
        let node = Node::new(0, 4);
        let state = node.get_state().await;
        assert!(node.id == node.primary_id(state.view));
    }

    #[tokio::test]
    async fn test_add_message() {
        let node = Node::new(0, 4);
        let msg = Message::prepare(1, 0, 1, "hash".to_string());
        assert!(node.add_message(msg).is_ok());

        // Duplicate should fail
        let msg2 = Message::prepare(1, 0, 1, "hash".to_string());
        assert!(node.add_message(msg2).is_err());
    }

    #[tokio::test]
    async fn test_clear_messages() {
        let node = Node::new(0, 4);
        let msg = Message::prepare(1, 0, 1, "hash".to_string());
        node.add_message(msg).unwrap();
        assert_eq!(node.get_all_messages().len(), 1);

        node.clear_messages().await;
        assert_eq!(node.get_all_messages().len(), 0);
    }

    #[tokio::test]
    async fn test_add_receipt() {
        let node = Node::new(0, 4);
        let receipt = Receipt::new(1, 0, "value".to_string());
        assert!(node.add_receipt(receipt).is_ok());

        let retrieved = node.get_receipt(1);
        assert!(retrieved.is_some());
    }
}
