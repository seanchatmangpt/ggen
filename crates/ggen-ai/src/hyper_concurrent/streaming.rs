//! Async Streaming Coordinator for continuous agent output
//!
//! Enables streaming of agent results as they become available,
//! rather than waiting for all agents to complete.

use crate::error::Result;
use futures::stream::{Stream, StreamExt};
use parking_lot::RwLock;
use serde::{Deserialize, Serialize};
use std::collections::HashMap;
use std::pin::Pin;
use std::sync::atomic::{AtomicBool, AtomicU64, AtomicUsize, Ordering};
use std::sync::Arc;
use std::task::{Context, Poll};
use tokio::sync::mpsc::{self, Receiver, Sender};
use tracing::{debug, trace};

/// Stream item from agent execution
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct StreamItem<T> {
    /// Agent that produced this item
    pub agent_id: String,
    /// Item sequence number
    pub sequence: u64,
    /// The actual value
    pub value: T,
    /// Whether this is the final item from this agent
    pub is_final: bool,
    /// Timestamp
    pub timestamp: String,
}

impl<T> StreamItem<T> {
    /// Create a new stream item
    pub fn new(agent_id: String, sequence: u64, value: T, is_final: bool) -> Self {
        Self {
            agent_id,
            sequence,
            value,
            is_final,
            timestamp: chrono::Utc::now().to_rfc3339(),
        }
    }
}

/// Streaming coordinator for managing multiple agent streams
#[derive(Debug)]
pub struct AsyncStreamingCoordinator<T> {
    /// Per-agent senders
    senders: RwLock<HashMap<String, Sender<StreamItem<T>>>>,
    /// Central receiver for all items
    central_receiver: RwLock<Option<Receiver<StreamItem<T>>>>,
    /// Central sender
    central_sender: Sender<StreamItem<T>>,
    /// Agent sequence counters
    sequences: RwLock<HashMap<String, AtomicU64>>,
    /// Total items streamed
    total_items: AtomicU64,
    /// Active agents
    active_agents: AtomicUsize,
    /// Whether streaming is complete
    complete: AtomicBool,
    /// Channel capacity
    capacity: usize,
}

impl<T: Send + 'static> AsyncStreamingCoordinator<T> {
    /// Create a new streaming coordinator
    pub fn new(capacity: usize) -> Self {
        let (sender, receiver) = mpsc::channel(capacity);

        Self {
            senders: RwLock::new(HashMap::new()),
            central_receiver: RwLock::new(Some(receiver)),
            central_sender: sender,
            sequences: RwLock::new(HashMap::new()),
            total_items: AtomicU64::new(0),
            active_agents: AtomicUsize::new(0),
            complete: AtomicBool::new(false),
            capacity,
        }
    }

    /// Register an agent for streaming
    pub fn register_agent(&self, agent_id: &str) -> AgentStreamHandle<T>
    where
        T: Clone,
    {
        let (sender, receiver) = mpsc::channel(self.capacity);

        self.senders
            .write()
            .insert(agent_id.to_string(), sender.clone());
        self.sequences
            .write()
            .insert(agent_id.to_string(), AtomicU64::new(0));
        self.active_agents.fetch_add(1, Ordering::Relaxed);

        debug!("Registered agent for streaming: {}", agent_id);

        AgentStreamHandle {
            agent_id: agent_id.to_string(),
            sender,
            central_sender: self.central_sender.clone(),
            sequence: Arc::new(AtomicU64::new(0)),
        }
    }

    /// Mark an agent as complete
    pub fn complete_agent(&self, agent_id: &str) {
        self.senders.write().remove(agent_id);
        let remaining = self.active_agents.fetch_sub(1, Ordering::Relaxed) - 1;

        debug!("Agent {} completed streaming, {} remaining", agent_id, remaining);

        if remaining == 0 {
            self.complete.store(true, Ordering::Relaxed);
        }
    }

    /// Check if all agents are complete
    pub fn is_complete(&self) -> bool {
        self.complete.load(Ordering::Relaxed)
    }

    /// Get total items streamed
    pub fn total_items(&self) -> u64 {
        self.total_items.load(Ordering::Relaxed)
    }

    /// Get active agent count
    pub fn active_agents(&self) -> usize {
        self.active_agents.load(Ordering::Relaxed)
    }

    /// Take the receiver (can only be called once)
    pub fn take_receiver(&self) -> Option<Receiver<StreamItem<T>>> {
        self.central_receiver.write().take()
    }

    /// Create a stream from the coordinator
    pub fn into_stream(self: Arc<Self>) -> impl Stream<Item = StreamItem<T>>
    where
        T: Clone + Send + Sync + 'static,
    {
        let receiver = self.central_receiver.write().take();
        match receiver {
            Some(rx) => CoordinatorStream {
                receiver: rx,
                coordinator: self,
            }
            .boxed(),
            None => futures::stream::empty().boxed(),
        }
    }
}

/// Stream wrapper for coordinator
struct CoordinatorStream<T> {
    receiver: Receiver<StreamItem<T>>,
    coordinator: Arc<AsyncStreamingCoordinator<T>>,
}

impl<T: Clone + Send + 'static> Stream for CoordinatorStream<T> {
    type Item = StreamItem<T>;

    fn poll_next(mut self: Pin<&mut Self>, cx: &mut Context<'_>) -> Poll<Option<Self::Item>> {
        Pin::new(&mut self.receiver).poll_recv(cx)
    }
}

/// Handle for agents to send stream items
#[derive(Debug)]
pub struct AgentStreamHandle<T> {
    /// Agent ID
    agent_id: String,
    /// Local sender (for agent-specific consumers)
    sender: Sender<StreamItem<T>>,
    /// Central sender (for aggregated stream)
    central_sender: Sender<StreamItem<T>>,
    /// Sequence counter
    sequence: Arc<AtomicU64>,
}

impl<T: Clone + Send + 'static> AgentStreamHandle<T> {
    /// Send an item to the stream
    pub async fn send(&self, value: T) -> Result<()> {
        let seq = self.sequence.fetch_add(1, Ordering::Relaxed);
        let item = StreamItem::new(self.agent_id.clone(), seq, value, false);

        // Send to central stream
        self.central_sender
            .send(item.clone())
            .await
            .map_err(|_| crate::error::GgenAiError::internal("Stream closed"))?;

        // Send to local stream
        let _ = self.sender.send(item).await;

        trace!("Agent {} sent item {}", self.agent_id, seq);
        Ok(())
    }

    /// Send the final item and complete
    pub async fn send_final(&self, value: T) -> Result<()> {
        let seq = self.sequence.fetch_add(1, Ordering::Relaxed);
        let item = StreamItem::new(self.agent_id.clone(), seq, value, true);

        self.central_sender
            .send(item.clone())
            .await
            .map_err(|_| crate::error::GgenAiError::internal("Stream closed"))?;

        let _ = self.sender.send(item).await;

        debug!("Agent {} sent final item", self.agent_id);
        Ok(())
    }

    /// Get current sequence number
    pub fn sequence(&self) -> u64 {
        self.sequence.load(Ordering::Relaxed)
    }
}

/// Buffered stream that collects items until a batch is ready
#[derive(Debug)]
pub struct BufferedStreamCollector<T> {
    /// Buffer for items
    buffer: RwLock<Vec<StreamItem<T>>>,
    /// Batch size
    batch_size: usize,
    /// Notifier for batch ready
    notifier: tokio::sync::Notify,
    /// Total batches produced
    batches_produced: AtomicU64,
}

impl<T: Clone> BufferedStreamCollector<T> {
    /// Create a new buffered collector
    pub fn new(batch_size: usize) -> Self {
        Self {
            buffer: RwLock::new(Vec::with_capacity(batch_size)),
            batch_size,
            notifier: tokio::sync::Notify::new(),
            batches_produced: AtomicU64::new(0),
        }
    }

    /// Add an item to the buffer
    pub fn add(&self, item: StreamItem<T>) -> Option<Vec<StreamItem<T>>> {
        let mut buffer = self.buffer.write();
        buffer.push(item);

        if buffer.len() >= self.batch_size {
            let batch: Vec<_> = buffer.drain(..).collect();
            self.batches_produced.fetch_add(1, Ordering::Relaxed);
            self.notifier.notify_one();
            Some(batch)
        } else {
            None
        }
    }

    /// Flush remaining items
    pub fn flush(&self) -> Vec<StreamItem<T>> {
        let mut buffer = self.buffer.write();
        let batch: Vec<_> = buffer.drain(..).collect();
        if !batch.is_empty() {
            self.batches_produced.fetch_add(1, Ordering::Relaxed);
        }
        batch
    }

    /// Wait for a batch to be ready
    pub async fn wait_for_batch(&self) {
        self.notifier.notified().await;
    }

    /// Get batch count
    pub fn batches_produced(&self) -> u64 {
        self.batches_produced.load(Ordering::Relaxed)
    }
}

/// Stream merger that combines multiple streams with ordering
#[derive(Debug)]
pub struct StreamMerger<T> {
    /// Items buffer keyed by (agent_id, sequence)
    buffer: RwLock<HashMap<(String, u64), StreamItem<T>>>,
    /// Expected next sequence per agent
    expected_sequences: RwLock<HashMap<String, u64>>,
    /// Ready items queue
    ready: RwLock<Vec<StreamItem<T>>>,
}

impl<T: Clone> StreamMerger<T> {
    /// Create a new stream merger
    pub fn new() -> Self {
        Self {
            buffer: RwLock::new(HashMap::new()),
            expected_sequences: RwLock::new(HashMap::new()),
            ready: RwLock::new(Vec::new()),
        }
    }

    /// Add an item (may buffer if out of order)
    pub fn add(&self, item: StreamItem<T>) {
        let agent_id = &item.agent_id;
        let seq = item.sequence;

        let mut expected = self.expected_sequences.write();
        let current_expected = *expected.get(agent_id).unwrap_or(&0);

        if seq == current_expected {
            // In order, add to ready
            self.ready.write().push(item);
            expected.insert(agent_id.clone(), current_expected + 1);

            // Check for buffered items that are now ready
            self.flush_ready(agent_id, current_expected + 1);
        } else {
            // Out of order, buffer it
            self.buffer
                .write()
                .insert((agent_id.clone(), seq), item);
        }
    }

    /// Flush ready items from buffer
    fn flush_ready(&self, agent_id: &str, from_seq: u64) {
        let mut buffer = self.buffer.write();
        let mut expected = self.expected_sequences.write();
        let mut ready = self.ready.write();

        let mut seq = from_seq;
        while let Some(item) = buffer.remove(&(agent_id.to_string(), seq)) {
            ready.push(item);
            seq += 1;
        }
        expected.insert(agent_id.to_string(), seq);
    }

    /// Take ready items
    pub fn take_ready(&self) -> Vec<StreamItem<T>> {
        let mut ready = self.ready.write();
        std::mem::take(&mut *ready)
    }
}

impl<T: Clone> Default for StreamMerger<T> {
    fn default() -> Self {
        Self::new()
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[tokio::test]
    async fn test_coordinator_creation() {
        let coordinator: AsyncStreamingCoordinator<String> = AsyncStreamingCoordinator::new(100);
        assert_eq!(coordinator.active_agents(), 0);
        assert!(!coordinator.is_complete());
    }

    #[tokio::test]
    async fn test_agent_registration() {
        let coordinator: Arc<AsyncStreamingCoordinator<String>> =
            Arc::new(AsyncStreamingCoordinator::new(100));

        let _handle = coordinator.register_agent("agent-1");
        assert_eq!(coordinator.active_agents(), 1);

        coordinator.complete_agent("agent-1");
        assert_eq!(coordinator.active_agents(), 0);
        assert!(coordinator.is_complete());
    }

    #[tokio::test]
    async fn test_stream_item() {
        let item = StreamItem::new("agent-1".to_string(), 0, "test".to_string(), false);
        assert_eq!(item.agent_id, "agent-1");
        assert_eq!(item.sequence, 0);
        assert!(!item.is_final);
    }

    #[test]
    fn test_buffered_collector() {
        let collector: BufferedStreamCollector<String> = BufferedStreamCollector::new(3);

        let item1 = StreamItem::new("a".to_string(), 0, "1".to_string(), false);
        let item2 = StreamItem::new("a".to_string(), 1, "2".to_string(), false);
        let item3 = StreamItem::new("a".to_string(), 2, "3".to_string(), false);

        assert!(collector.add(item1).is_none());
        assert!(collector.add(item2).is_none());
        let batch = collector.add(item3);
        assert!(batch.is_some());
        assert_eq!(batch.unwrap().len(), 3);
    }

    #[test]
    fn test_stream_merger() {
        let merger: StreamMerger<String> = StreamMerger::new();

        // Add out of order
        let item2 = StreamItem::new("a".to_string(), 2, "2".to_string(), false);
        let item0 = StreamItem::new("a".to_string(), 0, "0".to_string(), false);
        let item1 = StreamItem::new("a".to_string(), 1, "1".to_string(), false);

        merger.add(item2);
        assert!(merger.take_ready().is_empty());

        merger.add(item0);
        assert_eq!(merger.take_ready().len(), 1);

        merger.add(item1);
        let ready = merger.take_ready();
        assert_eq!(ready.len(), 2); // item1 and buffered item2
    }
}
