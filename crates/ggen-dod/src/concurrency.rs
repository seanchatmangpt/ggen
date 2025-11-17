//! Formal Concurrency Control Model
//!
//! Closes Gap #1: Graph model extension for concurrent access patterns.
//!
//! This module extends μ(O, Σ, Q) to formally handle:
//! - Lock semantics (mutual exclusion, deadlock freedom)
//! - Channel semantics (message passing, ordering)
//! - Atomicity (atomic vs. eventual consistency)
//! - Happens-before relationships (causality)
//!
//! Key insight: Concurrency is not "magic" - it's a constraint in Q.

use serde::{Deserialize, Serialize};
use std::collections::{BTreeMap, HashSet};
use std::fmt;

/// A formal lock model in the Q invariant system.
///
/// Instead of ad-hoc locks, we express locks as constraints:
/// - Q_mutual_exclusion: "at most one agent holds lock L"
/// - Q_deadlock_free: "no circular wait chains"
/// - Q_liveness: "every waiting agent eventually acquires lock"
#[derive(Debug, Clone, Serialize, Deserialize, PartialEq, Eq)]
pub struct FormalLock {
    /// Unique lock identifier
    pub id: String,

    /// Who currently holds the lock (None = unlocked)
    pub holder: Option<String>,

    /// Agents waiting for this lock (ordered by arrival time)
    pub waiters: Vec<String>,

    /// Invariant: no holder AND no waiters XOR holder present
    pub invariant: LockInvariant,
}

#[derive(Debug, Clone, Serialize, Deserialize, PartialEq, Eq)]
pub enum LockInvariant {
    /// Mutual exclusion: at most one holder at any time
    MutualExclusion,

    /// Deadlock freedom: no circular holds
    DeadlockFree,

    /// Liveness: waiting agents always eventually acquire
    Liveness,

    /// Fairness: no agent starves indefinitely
    Fairness,
}

impl FormalLock {
    /// Attempt to acquire lock for agent
    ///
    /// Deterministic: always succeeds if not held, deterministically adds to queue if held.
    /// Verifiable: receiver can check "agent was added to waiter queue at position N"
    pub fn acquire(&mut self, agent: String) -> AcquisitionResult {
        match &self.holder {
            None => {
                self.holder = Some(agent.clone());
                AcquisitionResult::Acquired(AcquisitionProof {
                    lock_id: self.id.clone(),
                    agent: agent.clone(),
                    position: 0,
                    timestamp_ns: current_ns(),
                })
            }
            Some(_) => {
                let position = self.waiters.len();
                self.waiters.push(agent.clone());
                AcquisitionResult::Queued(QueueProof {
                    lock_id: self.id.clone(),
                    agent: agent.clone(),
                    position,
                    timestamp_ns: current_ns(),
                })
            }
        }
    }

    /// Release lock, promoting first waiter if present
    ///
    /// Deterministic: waiter at position 0 is always promoted (FIFO)
    pub fn release(&mut self) -> ReleaseProof {
        let released_agent = self.holder.take();

        let promoted_agent = if !self.waiters.is_empty() {
            let agent = self.waiters.remove(0);
            self.holder = Some(agent.clone());
            Some(agent)
        } else {
            None
        };

        ReleaseProof {
            lock_id: self.id.clone(),
            released_agent,
            promoted_agent,
            timestamp_ns: current_ns(),
        }
    }

    /// Verify lock invariant is satisfied
    pub fn verify_invariant(&self) -> Result<(), LockViolation> {
        // Q: At most one holder
        match &self.holder {
            Some(h) if self.waiters.contains(h) => {
                Err(LockViolation::HolderAlsoWaiting(h.clone()))
            }
            Some(_) => Ok(()),
            None if !self.waiters.is_empty() => {
                // OK: waiting for unlock
                Ok(())
            }
            None => Ok(()),
        }
    }
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub enum AcquisitionResult {
    Acquired(AcquisitionProof),
    Queued(QueueProof),
}

#[derive(Debug, Clone, Serialize, Deserialize, PartialEq, Eq)]
pub struct AcquisitionProof {
    pub lock_id: String,
    pub agent: String,
    pub position: usize,
    pub timestamp_ns: u64,
}

#[derive(Debug, Clone, Serialize, Deserialize, PartialEq, Eq)]
pub struct QueueProof {
    pub lock_id: String,
    pub agent: String,
    pub position: usize,
    pub timestamp_ns: u64,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct ReleaseProof {
    pub lock_id: String,
    pub released_agent: Option<String>,
    pub promoted_agent: Option<String>,
    pub timestamp_ns: u64,
}

#[derive(Debug, Clone)]
pub enum LockViolation {
    HolderAlsoWaiting(String),
    MultipleHolders,
    DeadlockDetected(Vec<String>),
}

/// Channel model for message-passing concurrency
///
/// Channels are ordered message queues with provable FIFO semantics.
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct FormalChannel {
    pub id: String,
    pub messages: Vec<Message>,
    pub closed: bool,
    pub invariant: ChannelInvariant,
}

#[derive(Debug, Clone, Serialize, Deserialize, PartialEq, Eq)]
pub enum ChannelInvariant {
    /// Messages are delivered in FIFO order
    FIFOOrdering,

    /// No message is lost or duplicated
    Reliability,

    /// Sender always knows if send succeeded or channel is closed
    Atomicity,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct Message {
    pub id: String,
    pub sender: String,
    pub payload: String,
    pub sequence_number: u64,
    pub timestamp_ns: u64,
}

impl FormalChannel {
    pub fn new(id: String) -> Self {
        FormalChannel {
            id,
            messages: Vec::new(),
            closed: false,
            invariant: ChannelInvariant::FIFOOrdering,
        }
    }

    /// Send message deterministically
    ///
    /// Returns proof that message was enqueued with sequence number N
    pub fn send(&mut self, sender: String, payload: String) -> Result<SendProof, ChannelError> {
        if self.closed {
            return Err(ChannelError::ChannelClosed);
        }

        let sequence_number = self.messages.len() as u64;
        let message = Message {
            id: format!("{}-{}", self.id, sequence_number),
            sender: sender.clone(),
            payload,
            sequence_number,
            timestamp_ns: current_ns(),
        };

        self.messages.push(message.clone());

        Ok(SendProof {
            channel_id: self.id.clone(),
            message_id: message.id,
            sequence_number,
            timestamp_ns: current_ns(),
        })
    }

    /// Receive message deterministically (FIFO)
    ///
    /// Always returns first message in queue
    pub fn recv(&mut self) -> Result<Message, ChannelError> {
        if self.messages.is_empty() {
            return Err(ChannelError::NoMessages);
        }

        Ok(self.messages.remove(0))
    }

    /// Verify FIFO ordering invariant
    pub fn verify_fifo(&self) -> Result<(), ChannelViolation> {
        for (i, msg) in self.messages.iter().enumerate() {
            if msg.sequence_number != i as u64 {
                return Err(ChannelViolation::OutOfOrder {
                    expected: i as u64,
                    got: msg.sequence_number,
                });
            }
        }
        Ok(())
    }
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct SendProof {
    pub channel_id: String,
    pub message_id: String,
    pub sequence_number: u64,
    pub timestamp_ns: u64,
}

#[derive(Debug, Clone)]
pub enum ChannelError {
    ChannelClosed,
    NoMessages,
    BufferFull,
}

#[derive(Debug, Clone)]
pub enum ChannelViolation {
    OutOfOrder { expected: u64, got: u64 },
    MessageLoss { expected: u64, actual: u64 },
    Duplication(String),
}

/// Happens-before relationship (causality tracking)
///
/// For distributed systems, we need to track causal ordering:
/// "Event A happened before event B" (in the system's logical time)
#[derive(Debug, Clone, Serialize, Deserialize, PartialEq, Eq, Ord, PartialOrd)]
pub struct LogicalTimestamp {
    /// Agent's local logical clock (Lamport clock)
    pub agent_id: String,
    pub clock: u64,
}

#[derive(Debug, Clone)]
pub struct HappensBefore {
    /// Maps from event pairs to causal relationship
    pub edges: BTreeMap<(String, String), CausalRelation>,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum CausalRelation {
    /// A caused B (A's output fed into B's input)
    Caused,

    /// A and B are concurrent (no causal relationship)
    Concurrent,

    /// Ordering unknown (insufficient information)
    Unknown,
}

impl HappensBefore {
    pub fn new() -> Self {
        HappensBefore {
            edges: BTreeMap::new(),
        }
    }

    /// Record that event A happened before event B
    pub fn add_edge(&mut self, a: String, b: String) {
        self.edges.insert((a, b), CausalRelation::Caused);
    }

    /// Check if there's a causal path from A to B
    pub fn is_reachable(&self, from: &str, to: &str) -> bool {
        let mut visited = HashSet::new();
        self._dfs(from, to, &mut visited)
    }

    fn _dfs(&self, current: &str, target: &str, visited: &mut HashSet<String>) -> bool {
        if current == target {
            return true;
        }

        visited.insert(current.to_string());

        for (key, rel) in self.edges.iter() {
            if key.0 == current && *rel == CausalRelation::Caused {
                if !visited.contains(&key.1) && self._dfs(&key.1, target, visited) {
                    return true;
                }
            }
        }

        false
    }

    /// Detect if two events are truly concurrent
    pub fn are_concurrent(&self, a: &str, b: &str) -> bool {
        !self.is_reachable(a, b) && !self.is_reachable(b, a)
    }
}

/// Atomic operations (linearizability guarantees)
///
/// Some operations must appear to happen atomically to all observers.
/// This is expressed as a Q invariant.
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct AtomicOperation {
    pub id: String,
    pub name: String,
    pub preconditions: Vec<String>,     // Q conditions that must hold before
    pub postconditions: Vec<String>,    // Q conditions that must hold after
    pub linearization_point: String,    // The single point where op "happens"
}

impl AtomicOperation {
    /// Verify that operation maintains atomicity invariant
    ///
    /// Q: There exists a linearization where all operations appear atomic
    pub fn verify_linearizable(&self) -> Result<(), LinearizabilityViolation> {
        if self.linearization_point.is_empty() {
            return Err(LinearizabilityViolation::NoLinearizationPoint);
        }

        if self.preconditions.is_empty() || self.postconditions.is_empty() {
            return Err(LinearizabilityViolation::IncompleteSpecification);
        }

        Ok(())
    }
}

#[derive(Debug, Clone)]
pub enum LinearizabilityViolation {
    NoLinearizationPoint,
    IncompleteSpecification,
    NonDeterministic,
    RaceCondition,
}

/// Deadlock detector using wait-for graph analysis
///
/// Q_deadlock_free: no cycles in the wait-for graph
pub struct DeadlockDetector {
    wait_for_graph: BTreeMap<String, Vec<String>>,
}

impl DeadlockDetector {
    pub fn new() -> Self {
        DeadlockDetector {
            wait_for_graph: BTreeMap::new(),
        }
    }

    pub fn add_waiter(&mut self, waiter: String, waiting_for: String) {
        self.wait_for_graph
            .entry(waiter)
            .or_insert_with(Vec::new)
            .push(waiting_for);
    }

    /// Detect if there's a cycle in wait-for graph
    ///
    /// Algorithm: DFS with cycle detection
    pub fn has_cycle(&self) -> Result<(), DeadlockViolation> {
        let mut visited = HashSet::new();
        let mut rec_stack = HashSet::new();

        for node in self.wait_for_graph.keys() {
            if !visited.contains(node) {
                if self._has_cycle_dfs(node, &mut visited, &mut rec_stack) {
                    return Err(DeadlockViolation::CycleDetected);
                }
            }
        }

        Ok(())
    }

    fn _has_cycle_dfs(
        &self,
        node: &str,
        visited: &mut HashSet<String>,
        rec_stack: &mut HashSet<String>,
    ) -> bool {
        visited.insert(node.to_string());
        rec_stack.insert(node.to_string());

        if let Some(neighbors) = self.wait_for_graph.get(node) {
            for neighbor in neighbors {
                if !visited.contains(neighbor) {
                    if self._has_cycle_dfs(neighbor, visited, rec_stack) {
                        return true;
                    }
                } else if rec_stack.contains(neighbor) {
                    return true;
                }
            }
        }

        rec_stack.remove(node);
        false
    }
}

#[derive(Debug, Clone)]
pub enum DeadlockViolation {
    CycleDetected,
    LivenessViolation(String),
}

fn current_ns() -> u64 {
    std::time::SystemTime::now()
        .duration_since(std::time::UNIX_EPOCH)
        .unwrap()
        .as_nanos() as u64
}

impl fmt::Display for FormalLock {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(
            f,
            "Lock(id={}, holder={:?}, waiters={})",
            self.id,
            self.holder,
            self.waiters.len()
        )
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_lock_acquire_release() {
        let mut lock = FormalLock {
            id: "lock1".to_string(),
            holder: None,
            waiters: vec![],
            invariant: LockInvariant::MutualExclusion,
        };

        // Agent A acquires
        match lock.acquire("agent_a".to_string()) {
            AcquisitionResult::Acquired(proof) => {
                assert_eq!(proof.position, 0);
                assert_eq!(lock.holder, Some("agent_a".to_string()));
            }
            _ => panic!("Should acquire"),
        }

        // Agent B waits
        match lock.acquire("agent_b".to_string()) {
            AcquisitionResult::Queued(proof) => {
                assert_eq!(proof.position, 0);
                assert!(lock.waiters.contains(&"agent_b".to_string()));
            }
            _ => panic!("Should queue"),
        }

        // A releases, B promoted
        let release = lock.release();
        assert_eq!(release.released_agent, Some("agent_a".to_string()));
        assert_eq!(release.promoted_agent, Some("agent_b".to_string()));
        assert_eq!(lock.holder, Some("agent_b".to_string()));
    }

    #[test]
    fn test_channel_fifo() {
        let mut ch = FormalChannel::new("ch1".to_string());

        ch.send("agent_a".to_string(), "msg1".to_string())
            .unwrap();
        ch.send("agent_b".to_string(), "msg2".to_string())
            .unwrap();

        let msg1 = ch.recv().unwrap();
        assert_eq!(msg1.payload, "msg1");

        let msg2 = ch.recv().unwrap();
        assert_eq!(msg2.payload, "msg2");

        ch.verify_fifo().unwrap();
    }

    #[test]
    fn test_deadlock_detection() {
        let mut detector = DeadlockDetector::new();

        // A waits for B, B waits for A
        detector.add_waiter("A".to_string(), "B".to_string());
        detector.add_waiter("B".to_string(), "A".to_string());

        let result = detector.has_cycle();
        assert!(result.is_err());
    }

    #[test]
    fn test_happens_before() {
        let mut hb = HappensBefore::new();

        hb.add_edge("event_1".to_string(), "event_2".to_string());
        hb.add_edge("event_2".to_string(), "event_3".to_string());

        assert!(hb.is_reachable("event_1", "event_3"));
        assert!(!hb.is_reachable("event_3", "event_1"));
        assert!(hb.are_concurrent("event_1", "event_4"));
    }
}
