//! Event publishing for YAWL workflow events
//!
//! Provides functionality to publish YAWL workflow events as A2A messages,
//! enabling event-driven communication between YAWL and A2A systems.

use crate::error::A2aMcpResult;
use a2a_generated::converged::message::{
    ConvergedMessage, ConvergedMessageType, MessageEnvelope, MessagePriority,
    ConvergedPayload, UnifiedContent, MessageRouting, MessageLifecycle,
    MessageState, QoSRequirements, ReliabilityLevel,
};
use chrono::Utc;
use std::collections::HashMap;

/// Event types for YAWL workflow events
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum YawlEventType {
    /// Task state has changed
    TaskStatusUpdate,

    /// Workflow has started
    WorkflowStarted,

    /// Workflow has completed
    WorkflowCompleted,

    /// Workflow has failed
    WorkflowFailed,

    /// Gateway has been activated (split/join)
    GatewayActivated,

    /// Custom event type
    Custom(&'static str),
}

impl YawlEventType {
    /// Get the event type as a string
    pub fn as_str(&self) -> &str {
        match self {
            YawlEventType::TaskStatusUpdate => "TaskStatusUpdate",
            YawlEventType::WorkflowStarted => "WorkflowStarted",
            YawlEventType::WorkflowCompleted => "WorkflowCompleted",
            YawlEventType::WorkflowFailed => "WorkflowFailed",
            YawlEventType::GatewayActivated => "GatewayActivated",
            YawlEventType::Custom(s) => s,
        }
    }
}

/// Publishes YAWL events as A2A messages
///
/// Creates A2A event messages from YAWL workflow state changes
/// and lifecycle events. In production, these would be published
/// via an A2A client to the message bus.
pub struct YawlEventPublisher {
    /// Optional source identifier for events
    source: String,
}

impl YawlEventPublisher {
    /// Create a new YawlEventPublisher
    pub fn new() -> Self {
        Self {
            source: "yawl-engine".to_string(),
        }
    }

    /// Create a new YawlEventPublisher with custom source
    pub fn with_source(source: String) -> Self {
        Self { source }
    }

    /// Publish a YAWL task state change as an A2A event
    ///
    /// Creates an Event message containing the task state transition.
    /// In production, this would be sent via the A2A client.
    pub async fn publish_task_event(
        &self,
        workflow_id: &str,
        task_id: &str,
        old_state: &str,
        new_state: &str,
    ) -> A2aMcpResult<ConvergedMessage> {
        let mut data = HashMap::new();
        data.insert("eventType".to_string(), serde_json::json!("TaskStatusUpdate"));
        data.insert("workflowId".to_string(), serde_json::json!(workflow_id));
        data.insert("taskId".to_string(), serde_json::json!(task_id));
        data.insert("oldState".to_string(), serde_json::json!(old_state));
        data.insert("newState".to_string(), serde_json::json!(new_state));
        data.insert("timestamp".to_string(), serde_json::json!(Utc::now().to_rfc3339()));

        Ok(ConvergedMessage {
            message_id: format!("evt-{}-{}-{}", workflow_id, task_id, Utc::now().timestamp()),
            source: format!("{}:{}", self.source, workflow_id),
            target: None,
            envelope: MessageEnvelope {
                message_type: ConvergedMessageType::Event,
                priority: MessagePriority::Normal,
                timestamp: Utc::now(),
                schema_version: "1.0".to_string(),
                content_type: "application/json".to_string(),
                correlation_id: Some(task_id.to_string()),
                causation_chain: Some(vec![workflow_id.to_string()]),
            },
            payload: ConvergedPayload {
                content: UnifiedContent::Data {
                    data: data.into_iter().map(|(k, v)| (k, serde_json::Value::from(v))).collect(),
                    schema: Some("YawlTaskEvent".to_string()),
                },
                context: None,
                hints: None,
                integrity: None,
            },
            routing: MessageRouting {
                path: vec![self.source.clone()],
                metadata: None,
                qos: QoSRequirements {
                    reliability: ReliabilityLevel::AtLeastOnce,
                    latency: None,
                    throughput: None,
                },
            },
            lifecycle: MessageLifecycle {
                state: MessageState::Created,
                history: Vec::new(),
                timeout: None,
            },
            extensions: None,
        })
    }

    /// Publish a workflow lifecycle event
    pub async fn publish_workflow_event(
        &self,
        workflow_id: &str,
        event_type: YawlEventType,
        metadata: Option<HashMap<String, serde_json::Value>>,
    ) -> A2aMcpResult<ConvergedMessage> {
        let mut data = HashMap::new();
        data.insert("eventType".to_string(), serde_json::json!(event_type.as_str()));
        data.insert("workflowId".to_string(), serde_json::json!(workflow_id));
        data.insert("timestamp".to_string(), serde_json::json!(Utc::now().to_rfc3339()));

        if let Some(meta) = metadata {
            for (key, value) in meta {
                data.insert(key, value);
            }
        }

        Ok(ConvergedMessage {
            message_id: format!("evt-wf-{}-{}", workflow_id, Utc::now().timestamp()),
            source: self.source.clone(),
            target: None,
            envelope: MessageEnvelope {
                message_type: ConvergedMessageType::Event,
                priority: match event_type {
                    YawlEventType::WorkflowFailed => MessagePriority::High,
                    _ => MessagePriority::Normal,
                },
                timestamp: Utc::now(),
                schema_version: "1.0".to_string(),
                content_type: "application/json".to_string(),
                correlation_id: Some(workflow_id.to_string()),
                causation_chain: None,
            },
            payload: ConvergedPayload {
                content: UnifiedContent::Data {
                    data: data.into_iter().map(|(k, v)| (k, serde_json::Value::from(v))).collect(),
                    schema: Some("YawlWorkflowEvent".to_string()),
                },
                context: None,
                hints: None,
                integrity: None,
            },
            routing: MessageRouting {
                path: vec![self.source.clone()],
                metadata: None,
                qos: QoSRequirements {
                    reliability: ReliabilityLevel::AtLeastOnce,
                    latency: None,
                    throughput: None,
                },
            },
            lifecycle: MessageLifecycle {
                state: MessageState::Created,
                history: Vec::new(),
                timeout: None,
            },
            extensions: None,
        })
    }

    /// Publish a gateway activation event
    ///
    /// Notifies when a YAWL gateway (split/join) is activated during workflow execution.
    pub async fn publish_gateway_event(
        &self,
        workflow_id: &str,
        gateway_id: &str,
        gateway_type: &str, // "XorSplit", "AndJoin", etc.
        active_paths: Vec<String>,
    ) -> A2aMcpResult<ConvergedMessage> {
        let mut data = HashMap::new();
        data.insert("eventType".to_string(), serde_json::json!("GatewayActivated"));
        data.insert("workflowId".to_string(), serde_json::json!(workflow_id));
        data.insert("gatewayId".to_string(), serde_json::json!(gateway_id));
        data.insert("gatewayType".to_string(), serde_json::json!(gateway_type));
        data.insert(
            "activePaths".to_string(),
            serde_json::json!(active_paths),
        );
        data.insert("timestamp".to_string(), serde_json::json!(Utc::now().to_rfc3339()));

        Ok(ConvergedMessage {
            message_id: format!("evt-gw-{}-{}", gateway_id, Utc::now().timestamp()),
            source: format!("{}:{}", self.source, workflow_id),
            target: None,
            envelope: MessageEnvelope {
                message_type: ConvergedMessageType::Event,
                priority: MessagePriority::Normal,
                timestamp: Utc::now(),
                schema_version: "1.0".to_string(),
                content_type: "application/json".to_string(),
                correlation_id: Some(gateway_id.to_string()),
                causation_chain: Some(vec![workflow_id.to_string()]),
            },
            payload: ConvergedPayload {
                content: UnifiedContent::Data {
                    data: data.into_iter().map(|(k, v)| (k, serde_json::Value::from(v))).collect(),
                    schema: Some("YawlGatewayEvent".to_string()),
                },
                context: None,
                hints: None,
                integrity: None,
            },
            routing: MessageRouting {
                path: vec![self.source.clone()],
                metadata: None,
                qos: QoSRequirements {
                    reliability: ReliabilityLevel::AtLeastOnce,
                    latency: None,
                    throughput: None,
                },
            },
            lifecycle: MessageLifecycle {
                state: MessageState::Created,
                history: Vec::new(),
                timeout: None,
            },
            extensions: None,
        })
    }

    /// Create a batch of events for multiple task state changes
    pub async fn publish_task_events_batch(
        &self,
        events: Vec<(String, String, String, String)>, // (workflow_id, task_id, old_state, new_state)
    ) -> A2aMcpResult<Vec<ConvergedMessage>> {
        let mut messages = Vec::new();
        for (workflow_id, task_id, old_state, new_state) in events {
            messages.push(
                self.publish_task_event(&workflow_id, &task_id, &old_state, &new_state)
                    .await?,
            );
        }
        Ok(messages)
    }
}

impl Default for YawlEventPublisher {
    fn default() -> Self {
        Self::new()
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[tokio::test]
    async fn test_task_event_publish() {
        let publisher = YawlEventPublisher::new();
        let message = publisher
            .publish_task_event("wf-123", "task-456", "NotStarted", "Executing")
            .await
            .unwrap();

        assert_eq!(message.envelope.message_type, ConvergedMessageType::Event);
        assert_eq!(message.source, "yawl-engine:wf-123");
        assert_eq!(message.envelope.correlation_id, Some("task-456".to_string()));
    }

    #[tokio::test]
    async fn test_workflow_event_publish() {
        let publisher = YawlEventPublisher::new();
        let message = publisher
            .publish_workflow_event("wf-789", YawlEventType::WorkflowStarted, None)
            .await
            .unwrap();

        assert_eq!(message.envelope.message_type, ConvergedMessageType::Event);
        assert_eq!(message.envelope.priority, MessagePriority::Normal);
    }

    #[tokio::test]
    async fn test_failed_workflow_priority() {
        let publisher = YawlEventPublisher::new();
        let message = publisher
            .publish_workflow_event("wf-999", YawlEventType::WorkflowFailed, None)
            .await
            .unwrap();

        assert_eq!(message.envelope.priority, MessagePriority::High);
    }

    #[tokio::test]
    async fn test_gateway_event_publish() {
        let publisher = YawlEventPublisher::new();
        let message = publisher
            .publish_gateway_event("wf-123", "gw-split-1", "AndSplit", vec!["A".to_string(), "B".to_string()])
            .await
            .unwrap();

        assert_eq!(message.envelope.message_type, ConvergedMessageType::Event);
        assert_eq!(message.envelope.correlation_id, Some("gw-split-1".to_string()));
    }

    #[tokio::test]
    async fn test_custom_source() {
        let publisher = YawlEventPublisher::with_source("custom-yawl".to_string());
        let message = publisher
            .publish_task_event("wf-123", "task-456", "Ready", "Executing")
            .await
            .unwrap();

        assert!(message.source.starts_with("custom-yawl"));
    }
}
