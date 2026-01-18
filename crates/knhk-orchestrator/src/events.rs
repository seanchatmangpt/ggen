//! Event types for ETL-KGC-Workflow pipeline integration.
//!
//! This module defines the events that flow through the orchestrator:
//! - `EtlTripleEvent`: Output from knhk-etl with receipt
//! - `WorkflowTriggerEvent`: Input to knhk-workflow-engine
//! - `ProcessInstanceEvent`: State change feedback from workflow engine

use serde::{Deserialize, Serialize};
use std::collections::BTreeMap;
use uuid::Uuid;

/// RDF triple produced by ETL pipeline with temporal and causal metadata.
///
/// This event carries:
/// - The RDF triple (S, P, O) from ETL's Emit stage
/// - Merkle-linked receipt proving deterministic transformation
/// - Temporal coordinates for KGC-4D context injection
/// - OpenTelemetry span ID for distributed tracing
#[derive(Clone, Debug, Serialize, Deserialize)]
pub struct EtlTripleEvent {
    /// Unique transaction ID for trace correlation across ETL-KGC-Workflow pipeline
    pub transaction_id: String,

    /// Subject IRI of the RDF triple
    pub subject: String,

    /// Predicate IRI of the RDF triple
    pub predicate: String,

    /// Object value (IRI or literal)
    pub object: String,

    /// Nanosecond-precision timestamp when triple exited ETL Emit stage
    pub etl_timestamp: i64,

    /// OpenTelemetry span ID for distributed tracing
    pub kgc_span_id: String,

    /// Merkle-linked receipt from ETL proving deterministic transformation
    pub etl_receipt: EtlReceipt,

    /// Optional graph context (default: "default")
    #[serde(default = "default_graph")]
    pub graph: String,
}

fn default_graph() -> String {
    "default".to_string()
}

/// Receipt from ETL pipeline proving transformation correctness.
#[derive(Clone, Debug, Serialize, Deserialize)]
pub struct EtlReceipt {
    /// Merkle hash of the triple
    pub hash: String,

    /// Number of ticks consumed in Reflex stage
    pub ticks: u8,

    /// Previous receipt hash (for lockchain linking)
    pub prev_hash: Option<String>,
}

impl EtlTripleEvent {
    /// Create a new ETL triple event.
    #[must_use]
    pub fn new(
        subject: String,
        predicate: String,
        object: String,
        etl_timestamp: i64,
    ) -> Self {
        Self {
            transaction_id: Uuid::new_v4().to_string(),
            subject,
            predicate,
            object,
            etl_timestamp,
            kgc_span_id: Uuid::new_v4().to_string(),
            etl_receipt: EtlReceipt {
                hash: String::new(),
                ticks: 0,
                prev_hash: None,
            },
            graph: default_graph(),
        }
    }

    /// Set the ETL receipt for this event.
    pub fn with_receipt(mut self, receipt: EtlReceipt) -> Self {
        self.etl_receipt = receipt;
        self
    }

    /// Set the graph context.
    pub fn with_graph(mut self, graph: String) -> Self {
        self.graph = graph;
        self
    }
}

/// Command to start or continue a workflow instance with ETL-processed data.
///
/// This event is created by the orchestrator from aggregated ETL events and submitted
/// to knhk-workflow-engine for process execution.
#[derive(Clone, Debug, Serialize, Deserialize)]
pub struct WorkflowTriggerEvent {
    /// BPMN workflow identifier from workflow-engine
    pub workflow_id: String,

    /// Unique process instance ID (UUID)
    pub process_instance_id: String,

    /// Transaction ID linking back to ETL events (for end-to-end tracing)
    pub correlation_id: String,

    /// OpenTelemetry span ID for distributed tracing
    pub span_id: String,

    /// Key-value map of variables for workflow execution
    pub process_variables: BTreeMap<String, serde_json::Value>,

    /// Timestamp when trigger was created
    pub trigger_timestamp: i64,

    /// Number of ETL events aggregated into this trigger
    pub event_count: usize,
}

impl WorkflowTriggerEvent {
    /// Create a new workflow trigger event.
    #[must_use]
    pub fn new(workflow_id: String, correlation_id: String) -> Self {
        Self {
            workflow_id,
            process_instance_id: Uuid::new_v4().to_string(),
            correlation_id,
            span_id: Uuid::new_v4().to_string(),
            process_variables: BTreeMap::new(),
            trigger_timestamp: chrono::Utc::now().timestamp_nanos_opt().unwrap_or(0),
            event_count: 0,
        }
    }

    /// Add a process variable.
    pub fn with_variable(mut self, key: String, value: serde_json::Value) -> Self {
        self.process_variables.insert(key, value);
        self
    }

    /// Set all process variables at once.
    pub fn with_variables(mut self, variables: BTreeMap<String, serde_json::Value>) -> Self {
        self.process_variables = variables;
        self
    }

    /// Set the event count.
    pub fn with_event_count(mut self, count: usize) -> Self {
        self.event_count = count;
        self
    }
}

/// State of a workflow process instance.
#[derive(Clone, Copy, Debug, Serialize, Deserialize, PartialEq, Eq)]
#[serde(rename_all = "SCREAMING_SNAKE_CASE")]
pub enum ProcessInstanceState {
    /// Instance created but not yet started
    Created,
    /// Instance is currently executing
    Running,
    /// Instance has been paused
    Paused,
    /// Instance completed successfully
    Completed,
    /// Instance failed during execution
    Failed,
    /// Instance was explicitly cancelled
    Cancelled,
}

/// State change in a workflow instance emitted back to KGC-4D.
///
/// These events close the loop: workflow engine state changes are captured and
/// fedback to KGC-4D for temporal tracking and causality maintenance.
#[derive(Clone, Debug, Serialize, Deserialize)]
pub struct ProcessInstanceEvent {
    /// UUID of the process instance from workflow-engine
    pub process_instance_id: String,

    /// BPMN workflow identifier
    pub workflow_id: String,

    /// State of the workflow instance
    pub workflow_state: ProcessInstanceState,

    /// Complete state of all variables at this checkpoint
    pub state_snapshot: BTreeMap<String, serde_json::Value>,

    /// Timestamp of this state change
    pub state_timestamp: i64,

    /// OpenTelemetry span ID for correlation
    pub span_id: String,
}

impl ProcessInstanceEvent {
    /// Create a new process instance event.
    #[must_use]
    pub fn new(
        process_instance_id: String,
        workflow_id: String,
        workflow_state: ProcessInstanceState,
    ) -> Self {
        Self {
            process_instance_id,
            workflow_id,
            workflow_state,
            state_snapshot: BTreeMap::new(),
            state_timestamp: chrono::Utc::now().timestamp_nanos_opt().unwrap_or(0),
            span_id: Uuid::new_v4().to_string(),
        }
    }

    /// Add state variable to snapshot.
    pub fn with_state_var(mut self, key: String, value: serde_json::Value) -> Self {
        self.state_snapshot.insert(key, value);
        self
    }

    /// Set entire state snapshot.
    pub fn with_snapshot(mut self, snapshot: BTreeMap<String, serde_json::Value>) -> Self {
        self.state_snapshot = snapshot;
        self
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_etl_triple_event_creation() {
        let event = EtlTripleEvent::new(
            "http://example.com/subject".to_string(),
            "http://example.com/predicate".to_string(),
            "http://example.com/object".to_string(),
            123456789,
        );

        assert_eq!(event.subject, "http://example.com/subject");
        assert_eq!(event.predicate, "http://example.com/predicate");
        assert_eq!(event.object, "http://example.com/object");
        assert_eq!(event.etl_timestamp, 123456789);
        assert_eq!(event.graph, "default");
        assert!(!event.transaction_id.is_empty());
        assert!(!event.kgc_span_id.is_empty());
    }

    #[test]
    fn test_workflow_trigger_event_creation() {
        let trigger = WorkflowTriggerEvent::new(
            "workflow-123".to_string(),
            "txn-456".to_string(),
        );

        assert_eq!(trigger.workflow_id, "workflow-123");
        assert_eq!(trigger.correlation_id, "txn-456");
        assert!(!trigger.process_instance_id.is_empty());
        assert!(!trigger.span_id.is_empty());
        assert_eq!(trigger.event_count, 0);
        assert!(trigger.process_variables.is_empty());
    }

    #[test]
    fn test_workflow_trigger_with_variables() {
        let mut variables = BTreeMap::new();
        variables.insert("key1".to_string(), serde_json::json!("value1"));
        variables.insert("key2".to_string(), serde_json::json!(42));

        let trigger = WorkflowTriggerEvent::new(
            "workflow-123".to_string(),
            "txn-456".to_string(),
        )
        .with_variables(variables.clone())
        .with_event_count(2);

        assert_eq!(trigger.process_variables, variables);
        assert_eq!(trigger.event_count, 2);
    }

    #[test]
    fn test_process_instance_event_creation() {
        let event = ProcessInstanceEvent::new(
            "instance-789".to_string(),
            "workflow-123".to_string(),
            ProcessInstanceState::Running,
        );

        assert_eq!(event.process_instance_id, "instance-789");
        assert_eq!(event.workflow_id, "workflow-123");
        assert_eq!(event.workflow_state, ProcessInstanceState::Running);
        assert!(!event.span_id.is_empty());
        assert!(event.state_snapshot.is_empty());
    }

    #[test]
    fn test_serialization_roundtrip() {
        let event = EtlTripleEvent::new(
            "http://example.com/s".to_string(),
            "http://example.com/p".to_string(),
            "http://example.com/o".to_string(),
            123456789,
        );

        let json = serde_json::to_string(&event).unwrap();
        let deserialized: EtlTripleEvent = serde_json::from_str(&json).unwrap();

        assert_eq!(deserialized.subject, event.subject);
        assert_eq!(deserialized.predicate, event.predicate);
        assert_eq!(deserialized.object, event.object);
    }
}
