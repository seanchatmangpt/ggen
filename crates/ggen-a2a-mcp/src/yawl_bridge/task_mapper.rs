//! Task mapping between YAWL and A2A
//!
//! Provides conversion between YAWL workflow task representations
//! and A2A protocol message formats.

use crate::error::{A2aMcpError, A2aMcpResult};
use a2a_generated::converged::message::{
    ConvergedMessage, ConvergedMessageType, ConvergedPayload, MessageEnvelope, MessageLifecycle,
    MessagePriority, MessageRouting, MessageState, QoSRequirements, ReliabilityLevel,
    UnifiedContent,
};
use chrono::Utc;

/// YAWL task representation
///
/// Captures the essential properties of a YAWL workflow task
/// for conversion to A2A message format.
#[derive(Debug, Clone)]
pub struct YawlTask {
    /// Unique task identifier
    pub id: String,

    /// Human-readable task name
    pub name: String,

    /// Type of YAWL task
    pub task_type: YawlTaskType,

    /// Split behavior (for composite tasks)
    pub split_type: Option<YawlSplitType>,

    /// Join behavior (for composite tasks)
    pub join_type: Option<YawlJoinType>,

    /// Task input parameters
    pub input_data: Option<serde_json::Value>,

    /// Parent workflow ID
    pub workflow_id: Option<String>,

    /// Parent task ID (for nested tasks)
    pub parent_id: Option<String>,
}

/// YAWL task types
///
/// Based on the YAWL workflow language specification.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum YawlTaskType {
    /// Simple atomic task (single execution)
    Atomic,

    /// Composite task (contains sub-workflow)
    Composite,

    /// Multiple instance task (parallel executions)
    MultipleInstance,
}

/// YAWL split types for workflow branching
///
/// Determines how multiple outgoing transitions are handled.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum YawlSplitType {
    /// XOR split: exactly one outgoing path is taken
    Xor,

    /// AND split: all outgoing paths are taken in parallel
    And,

    /// OR split: one or more outgoing paths are taken
    Or,
}

/// YAWL join types for workflow synchronization
///
/// Determines how multiple incoming transitions are synchronized.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum YawlJoinType {
    /// XOR join: wait for one incoming path
    Xor,

    /// AND join: wait for all incoming paths
    And,
}

/// Maps YAWL tasks to A2A messages
pub struct TaskMapper;

impl TaskMapper {
    /// Create a new TaskMapper
    pub fn new() -> Self {
        Self
    }

    /// Convert YAWL task to A2A ConvergedMessage
    ///
    /// Creates a Task-type message with the YAWL task data encoded
    /// in the payload. The message can be sent to A2A agents for execution.
    pub fn yawl_to_a2a_task(&self, yawl_task: &YawlTask) -> A2aMcpResult<ConvergedMessage> {
        let mut data = serde_json::Map::new();
        data.insert("taskId".to_string(), serde_json::json!(yawl_task.id));
        data.insert("taskName".to_string(), serde_json::json!(yawl_task.name));
        data.insert(
            "taskType".to_string(),
            serde_json::json!(format!("{:?}", yawl_task.task_type)),
        );

        if let Some(split) = yawl_task.split_type {
            data.insert(
                "splitType".to_string(),
                serde_json::json!(format!("{:?}", split)),
            );
        }
        if let Some(join) = yawl_task.join_type {
            data.insert(
                "joinType".to_string(),
                serde_json::json!(format!("{:?}", join)),
            );
        }
        if let Some(input) = &yawl_task.input_data {
            data.insert("inputData".to_string(), input.clone());
        }
        if let Some(workflow_id) = &yawl_task.workflow_id {
            data.insert("workflowId".to_string(), serde_json::json!(workflow_id));
        }
        if let Some(parent_id) = &yawl_task.parent_id {
            data.insert("parentId".to_string(), serde_json::json!(parent_id));
        }

        Ok(ConvergedMessage {
            message_id: format!("task-{}", yawl_task.id),
            source: "yawl-engine".to_string(),
            target: None,
            envelope: MessageEnvelope {
                message_type: ConvergedMessageType::Task,
                priority: MessagePriority::Normal,
                timestamp: Utc::now(),
                schema_version: "1.0".to_string(),
                content_type: "application/json".to_string(),
                correlation_id: Some(yawl_task.id.clone()),
                causation_chain: yawl_task.workflow_id.clone().map(|id| vec![id]),
            },
            payload: ConvergedPayload {
                content: UnifiedContent::Data {
                    data,
                    schema: Some("YawlTask".to_string()),
                },
                context: None,
                hints: None,
                integrity: None,
            },
            routing: MessageRouting {
                path: vec!["yawl-engine".to_string()],
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

    /// Convert YAWL task with specific target agent
    pub fn yawl_to_a2a_task_for_agent(
        &self, yawl_task: &YawlTask, target_agent: &str,
    ) -> A2aMcpResult<ConvergedMessage> {
        let mut message = self.yawl_to_a2a_task(yawl_task)?;
        message.target = Some(target_agent.to_string());
        message.routing.path.push(target_agent.to_string());
        Ok(message)
    }

    /// Convert YAWL task with custom priority
    pub fn yawl_to_a2a_task_with_priority(
        &self, yawl_task: &YawlTask, priority: MessagePriority,
    ) -> A2aMcpResult<ConvergedMessage> {
        let mut message = self.yawl_to_a2a_task(yawl_task)?;
        message.envelope.priority = priority;
        Ok(message)
    }

    /// Create YAWL task from A2A message
    ///
    /// Extracts YAWL task information from an A2A ConvergedMessage.
    /// Returns an error if the message is not a valid YAWL task message.
    pub fn a2a_to_yawl_task(message: &ConvergedMessage) -> A2aMcpResult<YawlTask> {
        if message.envelope.message_type != ConvergedMessageType::Task {
            return Err(A2aMcpError::Translation(
                "Message is not a Task type".to_string(),
            ));
        }

        let data = match &message.payload.content {
            UnifiedContent::Data { data, schema } => {
                if schema.as_ref().is_some_and(|s| s != "YawlTask") {
                    return Err(A2aMcpError::Translation(
                        "Message payload is not a YawlTask schema".to_string(),
                    ));
                }
                data
            }
            _ => {
                return Err(A2aMcpError::Translation(
                    "Message payload is not Data content".to_string(),
                ))
            }
        };

        let id = data
            .get("taskId")
            .and_then(|v| v.as_str())
            .ok_or_else(|| A2aMcpError::Translation("Missing taskId".to_string()))?
            .to_string();

        let name = data
            .get("taskName")
            .and_then(|v| v.as_str())
            .ok_or_else(|| A2aMcpError::Translation("Missing taskName".to_string()))?
            .to_string();

        let task_type_str = data
            .get("taskType")
            .and_then(|v| v.as_str())
            .ok_or_else(|| A2aMcpError::Translation("Missing taskType".to_string()))?;

        let task_type = match task_type_str {
            "Atomic" => YawlTaskType::Atomic,
            "Composite" => YawlTaskType::Composite,
            "MultipleInstance" => YawlTaskType::MultipleInstance,
            _ => {
                return Err(A2aMcpError::Translation(format!(
                    "Unknown task type: {}",
                    task_type_str
                )))
            }
        };

        let split_type = data
            .get("splitType")
            .and_then(|v| v.as_str())
            .and_then(|s| match s {
                "Xor" => Some(YawlSplitType::Xor),
                "And" => Some(YawlSplitType::And),
                "Or" => Some(YawlSplitType::Or),
                _ => None,
            });

        let join_type = data
            .get("joinType")
            .and_then(|v| v.as_str())
            .and_then(|s| match s {
                "Xor" => Some(YawlJoinType::Xor),
                "And" => Some(YawlJoinType::And),
                _ => None,
            });

        let input_data = data.get("inputData").cloned();
        let workflow_id = data
            .get("workflowId")
            .and_then(|v| v.as_str())
            .map(String::from);
        let parent_id = data
            .get("parentId")
            .and_then(|v| v.as_str())
            .map(String::from);

        Ok(YawlTask {
            id,
            name,
            task_type,
            split_type,
            join_type,
            input_data,
            workflow_id,
            parent_id,
        })
    }

    /// Batch convert multiple YAWL tasks to A2A messages
    pub fn yawl_to_a2a_batch(&self, tasks: &[YawlTask]) -> A2aMcpResult<Vec<ConvergedMessage>> {
        tasks.iter().map(|t| self.yawl_to_a2a_task(t)).collect()
    }
}

impl Default for TaskMapper {
    fn default() -> Self {
        Self::new()
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_yawl_to_a2a_conversion() {
        let mapper = TaskMapper::new();
        let yawl_task = YawlTask {
            id: "task-123".to_string(),
            name: "Test Task".to_string(),
            task_type: YawlTaskType::Atomic,
            split_type: None,
            join_type: None,
            input_data: Some(serde_json::json!({"key": "value"})),
            workflow_id: Some("workflow-456".to_string()),
            parent_id: None,
        };

        let result = mapper.yawl_to_a2a_task(&yawl_task).unwrap();
        assert_eq!(result.message_id, "task-task-123");
        assert_eq!(result.source, "yawl-engine");
        assert_eq!(result.envelope.message_type, ConvergedMessageType::Task);
    }

    #[test]
    fn test_a2a_to_yawl_conversion() {
        let mapper = TaskMapper::new();
        let yawl_task = YawlTask {
            id: "task-789".to_string(),
            name: "Round Trip".to_string(),
            task_type: YawlTaskType::Composite,
            split_type: Some(YawlSplitType::And),
            join_type: Some(YawlJoinType::Xor),
            input_data: None,
            workflow_id: None,
            parent_id: None,
        };

        let message = mapper.yawl_to_a2a_task(&yawl_task).unwrap();
        let converted = TaskMapper::a2a_to_yawl_task(&message).unwrap();

        assert_eq!(converted.id, yawl_task.id);
        assert_eq!(converted.name, yawl_task.name);
        assert_eq!(converted.task_type, yawl_task.task_type);
        assert_eq!(converted.split_type, yawl_task.split_type);
        assert_eq!(converted.join_type, yawl_task.join_type);
    }

    #[test]
    fn test_batch_conversion() {
        let mapper = TaskMapper::new();
        let tasks = vec![
            YawlTask {
                id: "task-1".to_string(),
                name: "Task 1".to_string(),
                task_type: YawlTaskType::Atomic,
                split_type: None,
                join_type: None,
                input_data: None,
                workflow_id: None,
                parent_id: None,
            },
            YawlTask {
                id: "task-2".to_string(),
                name: "Task 2".to_string(),
                task_type: YawlTaskType::MultipleInstance,
                split_type: None,
                join_type: None,
                input_data: None,
                workflow_id: None,
                parent_id: None,
            },
        ];

        let results = mapper.yawl_to_a2a_batch(&tasks).unwrap();
        assert_eq!(results.len(), 2);
    }
}
