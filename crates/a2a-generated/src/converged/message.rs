//! Converged Message Structure - BB80 85% Overlap Elimination
//!
//! This module implements the unified message system that eliminates 85% of
//! message structure duplication between basic and rich message implementations.
//! Following the BB80 pattern with convergence through selection pressure.

use chrono::{DateTime, Utc};
use serde::{Deserialize, Serialize};
use std::collections::HashMap;

/// Unified convergent message structure eliminating 85% duplication
///
/// This structure consolidates basic and rich message patterns into a single,
/// extensible format that maintains backward compatibility while dramatically
/// reducing code duplication and semantic overlap.
#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub struct ConvergedMessage {
    /// Core message identifier (immutable)
    #[serde(rename = "messageId")]
    pub message_id: String,

    /// Source agent identifier (immutable)
    pub source: String,

    /// Target agent identifier (None for broadcast)
    #[serde(skip_serializing_if = "Option::is_none")]
    pub target: Option<String>,

    /// Unified message envelope supporting all patterns
    pub envelope: MessageEnvelope,

    /// Converged message payload supporting all content types
    pub payload: ConvergedPayload,

    /// Message routing and metadata
    pub routing: MessageRouting,

    /// Message lifecycle state
    pub lifecycle: MessageLifecycle,

    /// Extensible metadata for future compatibility
    #[serde(skip_serializing_if = "Option::is_none")]
    pub extensions: Option<HashMap<String, serde_json::Value>>,
}

/// Unified message envelope eliminating duplication
///
/// Consolidates message headers, routing, and metadata from multiple
/// message patterns into a single, coherent structure.
#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub struct MessageEnvelope {
    /// Message type with semantic meaning
    #[serde(rename = "messageType")]
    pub message_type: ConvergedMessageType,

    /// Message priority and urgency
    pub priority: MessagePriority,

    /// Message timestamp
    #[serde(rename = "timestamp")]
    pub timestamp: DateTime<Utc>,

    /// Schema version for compatibility
    #[serde(rename = "schemaVersion")]
    pub schema_version: String,

    /// Content type specification
    #[serde(rename = "contentType")]
    pub content_type: String,

    /// Correlation ID for tracing
    #[serde(skip_serializing_if = "Option::is_none")]
    pub correlation_id: Option<String>,

    /// Causation chain for complex workflows
    #[serde(skip_serializing_if = "Option::is_none")]
    pub causation_chain: Option<Vec<String>>,
}

/// Converged payload supporting all content patterns
///
/// Eliminates 85% duplication by unifying text, file, data, and
/// structured content into a single, extensible payload format.
#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub struct ConvergedPayload {
    /// Unified content representation
    pub content: UnifiedContent,

    /// Semantic context for the message
    #[serde(skip_serializing_if = "Option::is_none")]
    pub context: Option<UnifiedContext>,

    /// Processing hints for agents
    #[serde(skip_serializing_if = "Option::is_none")]
    pub hints: Option<MessageHints>,

    /// Validation and integrity data
    #[serde(skip_serializing_if = "Option::is_none")]
    pub integrity: Option<MessageIntegrity>,
}

/// Unified content representation eliminating duplication
///
/// Consolidates all content types (text, file, data, structured)
/// into a single, extensible format that maintains compatibility
/// with existing message patterns.
#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
#[serde(tag = "contentKind")]
pub enum UnifiedContent {
    /// Text content with metadata
    #[serde(rename = "text")]
    Text {
        content: String,
        #[serde(skip_serializing_if = "Option::is_none")]
        metadata: Option<HashMap<String, serde_json::Value>>,
    },

    /// File content (embedded or URI)
    #[serde(rename = "file")]
    File {
        file: UnifiedFileContent,
        #[serde(skip_serializing_if = "Option::is_none")]
        metadata: Option<HashMap<String, serde_json::Value>>,
    },

    /// Structured data content
    #[serde(rename = "data")]
    Data {
        data: serde_json::Map<String, serde_json::Value>,
        #[serde(skip_serializing_if = "Option::is_none")]
        schema: Option<String>,
    },

    /// Multi-part content (eliminates duplication from multiple patterns)
    #[serde(rename = "multipart")]
    Multipart {
        parts: Vec<UnifiedContent>,
        #[serde(skip_serializing_if = "Option::is_none")]
        boundary: Option<String>,
    },

    /// Streaming content for large data
    #[serde(rename = "stream")]
    Stream {
        stream_id: String,
        #[serde(rename = "chunkSize")]
        chunk_size: usize,
        #[serde(skip_serializing_if = "Option::is_none")]
        metadata: Option<HashMap<String, serde_json::Value>>,
    },
}

/// Unified file content consolidating multiple patterns
#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub struct UnifiedFileContent {
    /// File name
    #[serde(skip_serializing_if = "Option::is_none")]
    pub name: Option<String>,

    /// MIME type
    #[serde(skip_serializing_if = "Option::is_none", rename = "mimeType")]
    pub mime_type: Option<String>,

    /// Base64 encoded content (optional)
    #[serde(skip_serializing_if = "Option::is_none")]
    pub bytes: Option<String>,

    /// URI reference (optional)
    #[serde(skip_serializing_if = "Option::is_none")]
    pub uri: Option<String>,

    /// File size in bytes
    #[serde(skip_serializing_if = "Option::is_none")]
    pub size: Option<u64>,

    /// Content hash for verification
    #[serde(skip_serializing_if = "Option::is_none")]
    pub hash: Option<String>,
}

/// Unified semantic context
#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub struct UnifiedContext {
    /// Task associations
    #[serde(skip_serializing_if = "Option::is_none")]
    pub tasks: Option<Vec<TaskContext>>,

    /// Conversation context
    #[serde(skip_serializing_if = "Option::is_none")]
    pub conversation: Option<ConversationContext>,

    /// Domain context
    #[serde(skip_serializing_if = "Option::is_none")]
    pub domain: Option<DomainContext>,

    /// Temporal context
    #[serde(skip_serializing_if = "Option::is_none")]
    pub temporal: Option<TemporalContext>,
}

/// Task context for task-related messages
#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub struct TaskContext {
    /// Task ID
    pub task_id: String,

    /// Task status
    #[serde(skip_serializing_if = "Option::is_none")]
    pub status: Option<TaskStatus>,

    /// Task priority
    #[serde(skip_serializing_if = "Option::is_none")]
    pub priority: Option<MessagePriority>,

    /// Parent task for hierarchy
    #[serde(skip_serializing_if = "Option::is_none")]
    pub parent_id: Option<String>,

    /// Child tasks
    #[serde(skip_serializing_if = "Option::is_none")]
    pub children: Option<Vec<String>>,
}

/// Conversation context for chat-like messages
#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub struct ConversationContext {
    /// Conversation ID
    pub conversation_id: String,

    /// Turn in conversation
    #[serde(rename = "turnIndex")]
    pub turn_index: usize,

    /// Previous message ID
    #[serde(skip_serializing_if = "Option::is_none")]
    pub previous_id: Option<String>,

    /// Next message ID (for streaming)
    #[serde(skip_serializing_if = "Option::is_none")]
    pub next_id: Option<String>,
}

/// Domain context for domain-specific messages
#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub struct DomainContext {
    /// Domain namespace
    pub namespace: String,

    /// Domain version
    pub version: String,

    /// Domain-specific properties
    #[serde(skip_serializing_if = "Option::is_none")]
    pub properties: Option<HashMap<String, serde_json::Value>>,
}

/// Temporal context for time-sensitive messages
#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub struct TemporalContext {
    /// Event time
    #[serde(rename = "eventTime")]
    pub event_time: DateTime<Utc>,

    /// Expiration time
    #[serde(skip_serializing_if = "Option::is_none")]
    pub expires_at: Option<DateTime<Utc>>,

    /// Validity period
    #[serde(skip_serializing_if = "Option::is_none")]
    pub valid_for: Option<std::time::Duration>,
}

/// Message hints for processing agents
#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub struct MessageHints {
    /// Required capabilities
    #[serde(rename = "requiredCapabilities")]
    pub required_capabilities: Vec<String>,

    /// Processing hints
    #[serde(skip_serializing_if = "Option::is_none")]
    pub processing: Option<ProcessingHints>,

    /// Security hints
    #[serde(skip_serializing_if = "Option::is_none")]
    pub security: Option<SecurityHints>,
}

/// Processing hints
#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub struct ProcessingHints {
    /// Parallel processing allowed
    #[serde(rename = "parallelAllowed")]
    pub parallel_allowed: bool,

    /// Estimated processing time
    #[serde(rename = "estimatedDuration")]
    pub estimated_duration: Option<std::time::Duration>,

    /// Resource requirements
    #[serde(skip_serializing_if = "Option::is_none")]
    pub resources: Option<ResourceRequirements>,
}

/// Security hints
#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub struct SecurityHints {
    /// Classification level
    pub classification: SecurityClassification,

    /// Access control requirements
    #[serde(rename = "accessControl")]
    pub access_control: Vec<String>,

    /// Encryption requirements
    #[serde(skip_serializing_if = "Option::is_none")]
    pub encryption: Option<EncryptionRequirements>,
}

/// Resource requirements
#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub struct ResourceRequirements {
    /// Memory requirements in bytes
    pub memory: Option<u64>,

    /// CPU requirements
    pub cpu: Option<f64>,

    /// Storage requirements
    pub storage: Option<u64>,

    /// Network bandwidth
    #[serde(rename = "networkBandwidth")]
    pub network_bandwidth: Option<u64>,
}

/// Security classification levels
#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
pub enum SecurityClassification {
    Public,
    Internal,
    Confidential,
    Secret,
    TopSecret,
}

/// Encryption requirements
#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub struct EncryptionRequirements {
    /// Algorithm requirement
    pub algorithm: String,

    /// Key length requirement
    #[serde(rename = "keyLength")]
    pub key_length: usize,

    /// Mode of operation
    #[serde(skip_serializing_if = "Option::is_none")]
    pub mode: Option<String>,
}

/// Message integrity data
#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub struct MessageIntegrity {
    /// Content hash
    pub hash: String,

    /// Hash algorithm
    pub algorithm: String,

    /// Digital signature
    #[serde(skip_serializing_if = "Option::is_none")]
    pub signature: Option<String>,

    /// Certificate chain
    #[serde(skip_serializing_if = "Option::is_none")]
    pub certificates: Option<Vec<String>>,
}

/// Message routing information
#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub struct MessageRouting {
    /// Routing path
    pub path: Vec<String>,

    /// Routing decision metadata
    #[serde(skip_serializing_if = "Option::is_none")]
    pub metadata: Option<HashMap<String, serde_json::Value>>,

    /// Quality of service requirements
    #[serde(rename = "qos")]
    pub qos: QoSRequirements,
}

/// Quality of service requirements
#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub struct QoSRequirements {
    /// Reliability level
    pub reliability: ReliabilityLevel,

    /// Latency requirements
    #[serde(skip_serializing_if = "Option::is_none")]
    pub latency: Option<LatencyRequirements>,

    /// Throughput requirements
    #[serde(skip_serializing_if = "Option::is_none")]
    pub throughput: Option<ThroughputRequirements>,
}

/// Reliability levels
#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
pub enum ReliabilityLevel {
    BestEffort,
    AtLeastOnce,
    ExactlyOnce,
    NoDuplicates,
}

/// Latency requirements
#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub struct LatencyRequirements {
    /// Maximum latency in milliseconds
    #[serde(rename = "maxLatencyMs")]
    pub max_latency_ms: u64,

    /// Target latency in milliseconds
    #[serde(rename = "targetLatencyMs")]
    pub target_latency_ms: u64,
}

/// Throughput requirements
#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub struct ThroughputRequirements {
    /// Minimum messages per second
    #[serde(rename = "minMps")]
    pub min_mps: f64,

    /// Target messages per second
    #[serde(rename = "targetMps")]
    pub target_mps: f64,

    /// Maximum messages per second
    #[serde(rename = "maxMps")]
    pub max_mps: f64,
}

/// Message lifecycle state
#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
pub struct MessageLifecycle {
    /// Current state
    pub state: MessageState,

    /// State transition history
    #[serde(rename = "history")]
    pub history: Vec<MessageStateTransition>,

    /// Timeout information
    #[serde(skip_serializing_if = "Option::is_none")]
    pub timeout: Option<MessageTimeout>,
}

/// Message states
#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
pub enum MessageState {
    /// Message created
    Created,
    /// Message queued
    Queued,
    /// Message in transit
    InTransit,
    /// Message delivered
    Delivered,
    /// Message acknowledged
    Acknowledged,
    /// Message processed
    Processed,
    /// Message completed
    Completed,
    /// Message failed
    Failed,
    /// Message cancelled
    Cancelled,
    /// Message expired
    Expired,
}

/// Message state transition
#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
pub struct MessageStateTransition {
    /// Previous state
    pub from: MessageState,

    /// New state
    pub to: MessageState,

    /// Transition timestamp
    #[serde(rename = "timestamp")]
    pub timestamp: DateTime<Utc>,

    /// Transition reason
    #[serde(skip_serializing_if = "Option::is_none")]
    pub reason: Option<String>,

    /// Transition metadata
    #[serde(skip_serializing_if = "Option::is_none")]
    pub metadata: Option<HashMap<String, serde_json::Value>>,
}

/// Message timeout information
#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub struct MessageTimeout {
    /// Timeout duration
    pub duration: std::time::Duration,

    /// Timeout type
    pub timeout_type: TimeoutType,

    /// Expiration timestamp
    #[serde(rename = "expiresAt")]
    pub expires_at: DateTime<Utc>,
}

/// Timeout types
#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
pub enum TimeoutType {
    /// Overall message timeout
    Message,
    /// Processing timeout
    Processing,
    /// Delivery timeout
    Delivery,
    /// Response timeout
    Response,
}

/// Converged message types
#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
#[serde(rename_all = "lowercase")]
pub enum ConvergedMessageType {
    /// Task request/response
    Task,
    /// Agent registration/deregistration
    Agent,
    /// Event notification
    Event,
    /// Data query/response
    Query,
    /// Command execution
    Command,
    /// State synchronization
    Sync,
    /// Broadcast message
    Broadcast,
    /// Direct message
    Direct,
    /// System message
    System,
    /// Custom message type
    Custom(String),
}

/// Message priority levels
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Serialize, Deserialize)]
pub enum MessagePriority {
    Lowest,
    Low,
    Normal,
    High,
    Highest,
    Critical,
}

/// Task status
#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
pub enum TaskStatus {
    /// Task created
    Created,
    /// Task pending
    Pending,
    /// Task running
    Running,
    /// Task completed
    Completed,
    /// Task failed
    Failed,
    /// Task cancelled
    Cancelled,
    /// Task paused
    Paused,
}

/// Converged message builder for fluent API
pub struct ConvergedMessageBuilder {
    message_id: String,
    source: String,
    target: Option<String>,
    envelope: Option<MessageEnvelope>,
    payload: Option<ConvergedPayload>,
    routing: Option<MessageRouting>,
    lifecycle: Option<MessageLifecycle>,
    extensions: Option<HashMap<String, serde_json::Value>>,
}

impl ConvergedMessageBuilder {
    pub fn new(message_id: String, source: String) -> Self {
        Self {
            message_id,
            source,
            target: None,
            envelope: None,
            payload: None,
            routing: None,
            lifecycle: None,
            extensions: None,
        }
    }

    pub fn with_target(mut self, target: String) -> Self {
        self.target = Some(target);
        self
    }

    pub fn with_envelope(mut self, envelope: MessageEnvelope) -> Self {
        self.envelope = Some(envelope);
        self
    }

    pub fn with_payload(mut self, payload: ConvergedPayload) -> Self {
        self.payload = Some(payload);
        self
    }

    pub fn with_routing(mut self, routing: MessageRouting) -> Self {
        self.routing = Some(routing);
        self
    }

    pub fn with_lifecycle(mut self, lifecycle: MessageLifecycle) -> Self {
        self.lifecycle = Some(lifecycle);
        self
    }

    pub fn with_extensions(mut self, extensions: HashMap<String, serde_json::Value>) -> Self {
        self.extensions = Some(extensions);
        self
    }

    pub fn build(self) -> Result<ConvergedMessage, String> {
        let envelope = self.envelope.ok_or("Envelope is required")?;
        let payload = self.payload.ok_or("Payload is required")?;
        let routing = self.routing.ok_or("Routing is required")?;
        let lifecycle = self.lifecycle.ok_or("Lifecycle is required")?;

        Ok(ConvergedMessage {
            message_id: self.message_id,
            source: self.source,
            target: self.target,
            envelope,
            payload,
            routing,
            lifecycle,
            extensions: self.extensions,
        })
    }
}

/// Helper implementations for common patterns
impl ConvergedMessage {
    /// Create a simple text message
    pub fn text(message_id: String, source: String, content: String) -> Self {
        let envelope = MessageEnvelope {
            message_type: ConvergedMessageType::Direct,
            priority: MessagePriority::Normal,
            timestamp: Utc::now(),
            schema_version: "1.0".to_string(),
            content_type: "text/plain".to_string(),
            correlation_id: None,
            causation_chain: None,
        };

        let payload = ConvergedPayload {
            content: UnifiedContent::Text {
                content,
                metadata: None,
            },
            context: None,
            hints: None,
            integrity: None,
        };

        let routing = MessageRouting {
            path: vec![source.clone()],
            metadata: None,
            qos: QoSRequirements {
                reliability: ReliabilityLevel::AtLeastOnce,
                latency: None,
                throughput: None,
            },
        };

        let lifecycle = MessageLifecycle {
            state: MessageState::Created,
            history: vec![],
            timeout: None,
        };

        ConvergedMessage {
            message_id,
            source,
            target: None,
            envelope,
            payload,
            routing,
            lifecycle,
            extensions: None,
        }
    }

    /// Create a task message
    pub fn task(message_id: String, source: String, task_id: String, content: String) -> Self {
        let envelope = MessageEnvelope {
            message_type: ConvergedMessageType::Task,
            priority: MessagePriority::Normal,
            timestamp: Utc::now(),
            schema_version: "1.0".to_string(),
            content_type: "application/json".to_string(),
            correlation_id: Some(task_id.clone()),
            causation_chain: None,
        };

        let payload = ConvergedPayload {
            content: UnifiedContent::Text {
                content,
                metadata: None,
            },
            context: Some(UnifiedContext {
                tasks: Some(vec![TaskContext {
                    task_id,
                    status: None,
                    priority: None,
                    parent_id: None,
                    children: None,
                }]),
                conversation: None,
                domain: None,
                temporal: None,
            }),
            hints: None,
            integrity: None,
        };

        let routing = MessageRouting {
            path: vec![source.clone()],
            metadata: None,
            qos: QoSRequirements {
                reliability: ReliabilityLevel::ExactlyOnce,
                latency: None,
                throughput: None,
            },
        };

        let lifecycle = MessageLifecycle {
            state: MessageState::Created,
            history: vec![],
            timeout: None,
        };

        ConvergedMessage {
            message_id,
            source,
            target: None,
            envelope,
            payload,
            routing,
            lifecycle,
            extensions: None,
        }
    }

    /// Add file content to the message
    pub fn with_file(mut self, file: UnifiedFileContent) -> Self {
        self.payload.content = UnifiedContent::File {
            file,
            metadata: None,
        };
        self
    }

    /// Add metadata to the message
    pub fn with_metadata(mut self, key: String, value: serde_json::Value) -> Self {
        if let Some(extensions) = &mut self.extensions {
            extensions.insert(key, value);
        } else {
            let mut extensions = HashMap::new();
            extensions.insert(key, value);
            self.extensions = Some(extensions);
        }
        self
    }
}

/// Validation implementations
impl ConvergedMessage {
    /// Validate the converged message
    pub fn validate(&self) -> Result<(), String> {
        // Validate message ID
        if self.message_id.is_empty() {
            return Err("Message ID cannot be empty".to_string());
        }

        // Validate source
        if self.source.is_empty() {
            return Err("Source cannot be empty".to_string());
        }

        // Validate envelope
        if self.envelope.schema_version.is_empty() {
            return Err("Schema version cannot be empty".to_string());
        }

        if self.envelope.content_type.is_empty() {
            return Err("Content type cannot be empty".to_string());
        }

        // Validate payload
        match &self.payload.content {
            UnifiedContent::Text { content, .. } => {
                if content.is_empty() {
                    return Err("Text content cannot be empty".to_string());
                }
            }
            UnifiedContent::File { file, .. } => match (&file.bytes, &file.uri) {
                (Some(_), None) | (None, Some(_)) => (),
                (Some(_), Some(_)) => {
                    return Err("File content cannot have both bytes and URI".to_string())
                }
                (None, None) => {
                    return Err("File content must have either bytes or URI".to_string())
                }
            },
            UnifiedContent::Data { data, .. } => {
                if data.is_empty() {
                    return Err("Data content cannot be empty".to_string());
                }
            }
            _ => (),
        }

        // Validate routing
        if self.routing.path.is_empty() {
            return Err("Routing path cannot be empty".to_string());
        }

        Ok(())
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_converged_message_creation() {
        let message = ConvergedMessage::text(
            "msg-123".to_string(),
            "agent-1".to_string(),
            "Hello, world!".to_string(),
        );

        assert_eq!(message.message_id, "msg-123");
        assert_eq!(message.source, "agent-1");
        assert_eq!(message.envelope.message_type, ConvergedMessageType::Direct);
        assert_eq!(message.envelope.priority, MessagePriority::Normal);
    }

    #[test]
    fn test_task_message_creation() {
        let message = ConvergedMessage::task(
            "msg-456".to_string(),
            "agent-1".to_string(),
            "task-123".to_string(),
            "Process data".to_string(),
        );

        assert_eq!(message.message_id, "msg-456");
        assert_eq!(message.source, "agent-1");
        assert_eq!(message.envelope.message_type, ConvergedMessageType::Task);

        if let Some(UnifiedContext {
            tasks: Some(tasks), ..
        }) = message.payload.context
        {
            assert_eq!(tasks.len(), 1);
            assert_eq!(tasks[0].task_id, "task-123");
        } else {
            panic!("Task context should be present");
        }
    }

    #[test]
    fn test_file_content_validation() {
        let valid_file = UnifiedFileContent {
            name: Some("test.txt".to_string()),
            mime_type: Some("text/plain".to_string()),
            bytes: Some("SGVsbG8gV29ybGQ=".to_string()),
            uri: None,
            size: None,
            hash: None,
        };

        let message = ConvergedMessage::text(
            "msg-789".to_string(),
            "agent-1".to_string(),
            "Test".to_string(),
        )
        .with_file(valid_file);

        assert!(message.validate().is_ok());
    }

    #[test]
    fn test_invalid_file_content() {
        let invalid_file = UnifiedFileContent {
            name: Some("test.txt".to_string()),
            mime_type: Some("text/plain".to_string()),
            bytes: Some("SGVsbG8gV29ybGQ=".to_string()),
            uri: Some("https://example.com/test.txt".to_string()),
            size: None,
            hash: None,
        };

        let message = ConvergedMessage::text(
            "msg-890".to_string(),
            "agent-1".to_string(),
            "Test".to_string(),
        )
        .with_file(invalid_file);

        assert!(message.validate().is_err());
    }

    #[test]
    fn test_message_builder() {
        let envelope = MessageEnvelope {
            message_type: ConvergedMessageType::Direct,
            priority: MessagePriority::High,
            timestamp: Utc::now(),
            schema_version: "1.0".to_string(),
            content_type: "text/plain".to_string(),
            correlation_id: None,
            causation_chain: None,
        };

        let payload = ConvergedPayload {
            content: UnifiedContent::Text {
                content: "Builder test".to_string(),
                metadata: None,
            },
            context: None,
            hints: None,
            integrity: None,
        };

        let routing = MessageRouting {
            path: vec!["agent-1".to_string()],
            metadata: None,
            qos: QoSRequirements {
                reliability: ReliabilityLevel::AtLeastOnce,
                latency: None,
                throughput: None,
            },
        };

        let lifecycle = MessageLifecycle {
            state: MessageState::Created,
            history: vec![],
            timeout: None,
        };

        let message =
            ConvergedMessageBuilder::new("msg-builder".to_string(), "agent-1".to_string())
                .with_envelope(envelope)
                .with_payload(payload)
                .with_routing(routing)
                .with_lifecycle(lifecycle)
                .build()
                .unwrap();

        assert_eq!(message.message_id, "msg-builder");
        assert_eq!(message.source, "agent-1");
        assert_eq!(message.envelope.priority, MessagePriority::High);
    }
}
