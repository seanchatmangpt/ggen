//! Parts Execution Model
//!
//! Implements local execution of Genesis-bearing parts with evidence emission (OCEL/OTEL).
//! Each part executes with its own Genesis core, maintaining a vector clock for causality tracking.

use crate::utils::error::Result;
use chrono::{DateTime, Utc};
use serde::{Deserialize, Serialize};
use std::collections::HashMap;

/// Vector clock for distributed causality tracking
#[derive(Debug, Clone, Serialize, Deserialize, PartialEq, Eq)]
pub struct VectorClock {
    /// Map of node_id -> logical_time
    pub clock: HashMap<String, u64>,
}

impl VectorClock {
    /// Create a new vector clock for a given node
    pub fn new(node_id: String) -> Self {
        let mut clock = HashMap::new();
        clock.insert(node_id, 0);
        VectorClock { clock }
    }

    /// Increment this clock's own component
    pub fn tick(&mut self, node_id: &str) {
        let entry = self.clock.entry(node_id.to_string()).or_insert(0);
        *entry += 1;
    }

    /// Merge with a received clock (take max per component)
    pub fn merge(&mut self, other: &VectorClock) {
        for (node_id, time) in &other.clock {
            let entry = self.clock.entry(node_id.clone()).or_insert(0);
            *entry = (*entry).max(*time);
        }
    }

    /// Check if this clock happens-before another
    pub fn happens_before(&self, other: &VectorClock) -> bool {
        let mut less_somewhere = false;
        for (node_id, time) in &self.clock {
            let other_time = other.clock.get(node_id).copied().unwrap_or(0);
            if *time > other_time {
                return false; // Some component is greater
            }
            if *time < other_time {
                less_somewhere = true;
            }
        }
        less_somewhere
    }

    /// Serialize clock as a JSON string for OTEL attributes
    pub fn to_json_string(&self) -> String {
        serde_json::to_string(&self.clock).unwrap_or_else(|_| "{}".to_string())
    }
}

/// Execution status for a part invocation
#[derive(Debug, Clone, Copy, Serialize, Deserialize, PartialEq, Eq)]
pub enum ExecutionStatus {
    Success,
    Refused,
    Error,
}

impl std::fmt::Display for ExecutionStatus {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            ExecutionStatus::Success => write!(f, "Success"),
            ExecutionStatus::Refused => write!(f, "Refused"),
            ExecutionStatus::Error => write!(f, "Error"),
        }
    }
}

/// Single part execution packet with cryptographic evidence
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct ExecutionPacket {
    pub operation_id: String,
    pub part_id: String,
    pub input_hash: [u8; 32],
    pub output_hash: [u8; 32],
    pub vector_clock: VectorClock,
    pub signature: Option<String>, // Ed25519 base64
    pub status: ExecutionStatus,
    pub timestamp: DateTime<Utc>,
    pub duration_ms: u64,
}

impl ExecutionPacket {
    /// Create a new execution packet
    pub fn new(
        operation_id: String, part_id: String, input_hash: [u8; 32], output_hash: [u8; 32],
        vector_clock: VectorClock, status: ExecutionStatus, duration_ms: u64,
    ) -> Self {
        ExecutionPacket {
            operation_id,
            part_id,
            input_hash,
            output_hash,
            vector_clock,
            signature: None,
            status,
            timestamp: Utc::now(),
            duration_ms,
        }
    }

    /// Return the output size in bytes (placeholder, would come from actual execution)
    pub fn output_size_bytes(&self) -> u64 {
        1024 // Placeholder
    }
}

/// Local execution context for a part
pub struct LocalExecutionContext {
    pub part_id: String,
    pub vector_clock: VectorClock,
    pub event_log: Vec<serde_json::Value>, // OCEL events
}

impl LocalExecutionContext {
    /// Create a new execution context for a part
    pub fn new(part_id: String) -> Self {
        LocalExecutionContext {
            part_id: part_id.clone(),
            vector_clock: VectorClock::new(part_id),
            event_log: Vec::new(),
        }
    }

    /// Increment the vector clock and return the updated clock
    pub fn tick(&mut self) -> VectorClock {
        self.vector_clock.tick(&self.part_id);
        self.vector_clock.clone()
    }

    /// Merge an external clock into this context's clock
    pub fn merge_clock(&mut self, external_clock: &VectorClock) {
        self.vector_clock.merge(external_clock);
    }

    /// Add an OCEL event to the log
    pub fn emit_event(&mut self, event: serde_json::Value) {
        self.event_log.push(event);
    }

    /// Log an execution packet
    pub fn log_execution(&mut self, packet: ExecutionPacket) {
        self.event_log.push(serde_json::json!({
            "operation_id": packet.operation_id,
            "status": packet.status.to_string(),
        }));
    }

    /// Get all event logs
    pub fn packets(&self) -> &[serde_json::Value] {
        &self.event_log
    }
}

/// Trait for executing Genesis-bearing parts
#[async_trait::async_trait]
pub trait PartExecutor: Send + Sync {
    /// Execute a part with the given input
    /// Returns the output and an execution packet with evidence
    async fn execute(&self, input: Vec<u8>) -> Result<(Vec<u8>, ExecutionPacket)>;
}

/// Refusal evidence when a part execution is denied
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct RefusalEvidence {
    pub operation_id: String,
    pub part_id: String,
    pub refusal_code: String,
    pub reason: String,
    pub timestamp: DateTime<Utc>,
    pub vector_clock: VectorClock,
}

impl RefusalEvidence {
    pub fn new(
        operation_id: String, part_id: String, refusal_code: String, reason: String,
        vector_clock: VectorClock,
    ) -> Self {
        RefusalEvidence {
            operation_id,
            part_id,
            refusal_code,
            reason,
            timestamp: Utc::now(),
            vector_clock,
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_vector_clock_new() {
        let clock = VectorClock::new("part-1".to_string());
        assert_eq!(clock.clock.get("part-1"), Some(&0));
    }

    #[test]
    fn test_vector_clock_tick() {
        let mut clock = VectorClock::new("part-1".to_string());
        clock.tick("part-1");
        assert_eq!(clock.clock.get("part-1"), Some(&1));
    }

    #[test]
    fn test_execution_packet_creation() {
        let clock = VectorClock::new("part-1".to_string());
        let packet = ExecutionPacket::new(
            "op-1".to_string(),
            "part-1".to_string(),
            [0u8; 32],
            [1u8; 32],
            clock,
            ExecutionStatus::Success,
            100,
        );

        assert_eq!(packet.operation_id, "op-1");
        assert_eq!(packet.status, ExecutionStatus::Success);
    }
}
