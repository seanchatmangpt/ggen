//! Event Bus for inter-system communication
//!
//! Asynchronous, reliable event delivery using Tokio channels with guaranteed ordering
//! and delivery tracking.

use crate::events::{EtlTripleEvent, ProcessInstanceEvent, WorkflowTriggerEvent};
use crate::Result;
use tokio::sync::mpsc;
use tracing::trace;

/// Configuration for event bus channels
#[derive(Clone, Debug)]
pub struct BusConfig {
    /// Capacity of ETL → Orchestrator channel
    pub etl_channel_capacity: usize,

    /// Capacity of Orchestrator → Workflow channel
    pub workflow_channel_capacity: usize,

    /// Capacity of Workflow → KGC feedback channel
    pub feedback_channel_capacity: usize,
}

impl Default for BusConfig {
    fn default() -> Self {
        Self {
            etl_channel_capacity: 10_000,
            workflow_channel_capacity: 1_000,
            feedback_channel_capacity: 1_000,
        }
    }
}

/// Event Bus coordinating ETL → Orchestrator → Workflow communication
pub struct EventBus {
    /// Sender for ETL events
    etl_tx: mpsc::UnboundedSender<EtlTripleEvent>,
    /// Receiver for ETL events
    etl_rx: mpsc::UnboundedReceiver<EtlTripleEvent>,

    /// Sender for workflow triggers
    workflow_tx: mpsc::UnboundedSender<WorkflowTriggerEvent>,
    /// Receiver for workflow triggers
    workflow_rx: mpsc::UnboundedReceiver<WorkflowTriggerEvent>,

    /// Sender for workflow feedback events
    feedback_tx: mpsc::UnboundedSender<ProcessInstanceEvent>,
    /// Receiver for workflow feedback events
    feedback_rx: mpsc::UnboundedReceiver<ProcessInstanceEvent>,

    /// Metrics: events received from ETL
    etl_events_count: u64,
    /// Metrics: triggers sent to workflow engine
    triggers_sent_count: u64,
    /// Metrics: feedback events received
    feedback_events_count: u64,
}

impl EventBus {
    /// Create a new event bus with default configuration
    #[must_use]
    pub fn new() -> Self {
        let (etl_tx, etl_rx) = mpsc::unbounded_channel();
        let (workflow_tx, workflow_rx) = mpsc::unbounded_channel();
        let (feedback_tx, feedback_rx) = mpsc::unbounded_channel();

        Self {
            etl_tx,
            etl_rx,
            workflow_tx,
            workflow_rx,
            feedback_tx,
            feedback_rx,
            etl_events_count: 0,
            triggers_sent_count: 0,
            feedback_events_count: 0,
        }
    }

    /// Get sender for ETL events
    #[must_use]
    pub fn etl_sender(&self) -> mpsc::UnboundedSender<EtlTripleEvent> {
        self.etl_tx.clone()
    }

    /// Get receiver for ETL events
    pub fn etl_receiver(&mut self) -> &mut mpsc::UnboundedReceiver<EtlTripleEvent> {
        &mut self.etl_rx
    }

    /// Receive an ETL event (consumes receiver)
    pub async fn recv_etl(&mut self) -> Option<EtlTripleEvent> {
        if let Some(event) = self.etl_rx.recv().await {
            self.etl_events_count += 1;
            trace!(
                etl_event.transaction_id = %event.transaction_id,
                etl_event.subject = %event.subject,
                "Received ETL event"
            );
            Some(event)
        } else {
            None
        }
    }

    /// Get sender for workflow triggers
    #[must_use]
    pub fn workflow_sender(&self) -> mpsc::UnboundedSender<WorkflowTriggerEvent> {
        self.workflow_tx.clone()
    }

    /// Send a workflow trigger event
    pub async fn send_trigger(&mut self, trigger: WorkflowTriggerEvent) -> Result<()> {
        self.workflow_tx
            .send(trigger.clone())
            .map_err(|e| crate::OrchestratorError::QueueError(e.to_string()))?;

        self.triggers_sent_count += 1;
        trace!(
            trigger.workflow_id = %trigger.workflow_id,
            trigger.correlation_id = %trigger.correlation_id,
            "Sent workflow trigger"
        );
        Ok(())
    }

    /// Receive a workflow trigger (consumes receiver)
    pub async fn recv_trigger(&mut self) -> Option<WorkflowTriggerEvent> {
        self.workflow_rx.recv().await
    }

    /// Get sender for feedback events
    #[must_use]
    pub fn feedback_sender(&self) -> mpsc::UnboundedSender<ProcessInstanceEvent> {
        self.feedback_tx.clone()
    }

    /// Send a process instance feedback event
    pub async fn send_feedback(&mut self, event: ProcessInstanceEvent) -> Result<()> {
        self.feedback_tx
            .send(event.clone())
            .map_err(|e| crate::OrchestratorError::QueueError(e.to_string()))?;

        self.feedback_events_count += 1;
        trace!(
            feedback.process_instance_id = %event.process_instance_id,
            feedback.workflow_state = ?event.workflow_state,
            "Sent feedback event"
        );
        Ok(())
    }

    /// Receive a feedback event (consumes receiver)
    pub async fn recv_feedback(&mut self) -> Option<ProcessInstanceEvent> {
        if let Some(event) = self.feedback_rx.recv().await {
            Some(event)
        } else {
            None
        }
    }

    /// Get current metrics
    #[must_use]
    pub fn metrics(&self) -> BusMetrics {
        BusMetrics {
            etl_events_received: self.etl_events_count,
            workflow_triggers_sent: self.triggers_sent_count,
            feedback_events_received: self.feedback_events_count,
        }
    }

    /// Check if ETL channel has pending events
    #[must_use]
    pub fn has_pending_etl_events(&self) -> bool {
        !self.etl_rx.is_empty()
    }

    /// Get number of pending ETL events
    #[must_use]
    pub fn pending_etl_events(&self) -> usize {
        self.etl_rx.len()
    }

    /// Andon signal: check for queue overflow
    ///
    /// Returns true if pending events exceed threshold (indicates RED signal)
    #[must_use]
    pub fn check_etl_overflow(&self, threshold: usize) -> bool {
        self.pending_etl_events() > threshold
    }
}

impl Default for EventBus {
    fn default() -> Self {
        Self::new()
    }
}

/// Metrics snapshot from event bus
#[derive(Clone, Copy, Debug)]
pub struct BusMetrics {
    /// Total ETL events received
    pub etl_events_received: u64,
    /// Total workflow triggers sent
    pub workflow_triggers_sent: u64,
    /// Total feedback events received
    pub feedback_events_received: u64,
}

impl BusMetrics {
    /// Calculate throughput: triggers per ETL event
    #[must_use]
    pub fn aggregation_ratio(&self) -> f64 {
        if self.etl_events_received == 0 {
            0.0
        } else {
            self.workflow_triggers_sent as f64 / self.etl_events_received as f64
        }
    }

    /// Calculate feedback rate: feedback per trigger
    #[must_use]
    pub fn feedback_rate(&self) -> f64 {
        if self.workflow_triggers_sent == 0 {
            0.0
        } else {
            self.feedback_events_received as f64 / self.workflow_triggers_sent as f64
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[tokio::test]
    async fn test_event_bus_creation() {
        let bus = EventBus::new();
        let metrics = bus.metrics();

        assert_eq!(metrics.etl_events_received, 0);
        assert_eq!(metrics.workflow_triggers_sent, 0);
        assert_eq!(metrics.feedback_events_received, 0);
    }

    #[tokio::test]
    async fn test_etl_event_flow() {
        let mut bus = EventBus::new();
        let tx = bus.etl_sender();

        let event = EtlTripleEvent::new(
            "http://example.com/s".to_string(),
            "http://example.com/p".to_string(),
            "http://example.com/o".to_string(),
            123456789,
        );

        tx.send(event.clone()).unwrap();

        let received = bus.recv_etl().await;
        assert!(received.is_some());
        assert_eq!(received.unwrap().subject, event.subject);

        let metrics = bus.metrics();
        assert_eq!(metrics.etl_events_received, 1);
    }

    #[tokio::test]
    async fn test_workflow_trigger_flow() {
        let mut bus = EventBus::new();

        let trigger = WorkflowTriggerEvent::new("workflow-123".to_string(), "txn-456".to_string());

        bus.send_trigger(trigger.clone()).await.unwrap();

        let received = bus.recv_trigger().await;
        assert!(received.is_some());
        assert_eq!(received.unwrap().workflow_id, trigger.workflow_id);

        let metrics = bus.metrics();
        assert_eq!(metrics.workflow_triggers_sent, 1);
    }

    #[test]
    fn test_bus_metrics_aggregation_ratio() {
        let metrics = BusMetrics {
            etl_events_received: 100,
            workflow_triggers_sent: 20,
            feedback_events_received: 20,
        };

        assert_eq!(metrics.aggregation_ratio(), 0.2);
    }

    #[test]
    fn test_bus_metrics_feedback_rate() {
        let metrics = BusMetrics {
            etl_events_received: 100,
            workflow_triggers_sent: 20,
            feedback_events_received: 18,
        };

        assert_eq!(metrics.feedback_rate(), 0.9);
    }
}
