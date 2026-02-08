//! MCP to A2A Message Converter
//!
//! This module provides bidirectional conversion between MCP (Model Context Protocol)
//! and A2A (Agent-to-Agent) message formats.
//!
//! ## Conversion Mapping
//!
//! ### MCP to A2A
//! - `JsonRpcRequest` → `a2a_rs::Task` with a `Message`
//! - Request method → A2A message part (text or data)
//! - Request params → A2A message metadata
//!
//! ### A2A to MCP
//! - `a2a_rs::Task` → `JsonRpcResponse`
//! - Task status → Response result/error
//! - Task artifacts → Response content items

use crate::transport::{JsonRpcError, JsonRpcRequest, JsonRpcResponse, RequestId};
use serde::{Deserialize, Serialize};
use serde_json::{Map, Value as JsonValue};
use std::sync::Arc;
use thiserror::Error;
use tokio::sync::RwLock;

#[cfg(feature = "a2a")]
use a2a_rs::domain::core::{message::Part, task::TaskState, Message, Role, Task};

/// Conversion context containing configuration for message translation.
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct ConversionContext {
    /// Agent ID to use in A2A messages
    pub agent_id: String,
    /// Context ID for A2A task tracking
    pub context_id: String,
    /// Whether bidirectional conversion is enabled
    pub bidirectional: bool,
    /// Whether to collect conversion metrics
    pub collect_metrics: bool,
}

impl Default for ConversionContext {
    fn default() -> Self {
        Self {
            agent_id: "rig-mcp-bridge".to_string(),
            context_id: format!("ctx-{}", uuid::Uuid::new_v4()),
            bidirectional: true,
            collect_metrics: false,
        }
    }
}

/// Error type for conversion operations.
#[derive(Error, Debug)]
pub enum ConversionError {
    /// Invalid MCP request format
    #[error("Invalid MCP request: {0}")]
    InvalidMcpRequest(String),

    /// Invalid A2A task format
    #[error("Invalid A2A task: {0}")]
    InvalidA2aTask(String),

    /// Unsupported method for conversion
    #[error("Unsupported method: {0}")]
    UnsupportedMethod(String),

    /// Missing required field
    #[error("Missing required field: {0}")]
    MissingField(String),

    /// Type conversion failed
    #[error("Type conversion failed: {0}")]
    TypeConversion(String),

    /// A2A protocol error
    #[error("A2A error: {0}")]
    A2AError(String),

    /// Serialization/deserialization error
    #[error("JSON error: {0}")]
    JsonError(#[from] serde_json::Error),
}

impl From<ConversionError> for crate::transport::TransportError {
    fn from(err: ConversionError) -> Self {
        crate::transport::TransportError::Internal(err.to_string())
    }
}

/// Conversion mode for MCP to A2A translation.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Serialize, Deserialize)]
pub enum McpToA2aConversionMode {
    /// Convert MCP tool calls to A2A tasks with text parts
    ToolToTaskText,
    /// Convert MCP tool calls to A2A tasks with data parts
    ToolToTaskData,
    /// Direct passthrough mode (minimal conversion)
    Passthrough,
}

impl Default for McpToA2aConversionMode {
    fn default() -> Self {
        Self::ToolToTaskText
    }
}

/// Metrics for conversion operations.
#[derive(Debug, Clone, Serialize, Deserialize, Default)]
pub struct ConversionMetrics {
    /// Number of MCP to A2A conversions
    pub mcp_to_a2a_count: u64,
    /// Number of A2A to MCP conversions
    pub a2a_to_mcp_count: u64,
    /// Number of conversion errors
    pub error_count: u64,
    /// Total bytes converted
    pub total_bytes: u64,
    /// Average conversion time in microseconds
    pub avg_conversion_time_us: u64,
}

impl ConversionMetrics {
    /// Record a successful MCP to A2A conversion.
    pub fn record_mcp_to_a2a(&mut self, bytes: u64, duration_us: u64) {
        self.mcp_to_a2a_count += 1;
        self.total_bytes += bytes;
        self.update_avg_time(duration_us);
    }

    /// Record a successful A2A to MCP conversion.
    pub fn record_a2a_to_mcp(&mut self, bytes: u64, duration_us: u64) {
        self.a2a_to_mcp_count += 1;
        self.total_bytes += bytes;
        self.update_avg_time(duration_us);
    }

    /// Record a conversion error.
    pub fn record_error(&mut self) {
        self.error_count += 1;
    }

    fn update_avg_time(&mut self, duration_us: u64) {
        let total_count = self.mcp_to_a2a_count + self.a2a_to_mcp_count;
        if total_count > 0 {
            self.avg_conversion_time_us =
                (self.avg_conversion_time_us * (total_count - 1) + duration_us) / total_count;
        }
    }
}

/// MCP to A2A message converter.
///
/// Handles bidirectional conversion between MCP JSON-RPC messages
/// and A2A protocol messages/tasks.
#[derive(Clone)]
pub struct A2aMessageConverter {
    /// Conversion context
    context: ConversionContext,
    /// Conversion mode
    mode: McpToA2aConversionMode,
    /// Conversion metrics (if enabled)
    metrics: Arc<RwLock<Option<ConversionMetrics>>>,
}

impl A2aMessageConverter {
    /// Create a new converter with the given context.
    pub fn new(context: ConversionContext) -> Self {
        let metrics = if context.collect_metrics {
            Arc::new(RwLock::new(Some(ConversionMetrics::default())))
        } else {
            Arc::new(RwLock::new(None))
        };

        Self {
            context,
            mode: McpToA2aConversionMode::default(),
            metrics,
        }
    }

    /// Set the conversion mode.
    pub fn with_mode(mut self, mode: McpToA2aConversionMode) -> Self {
        self.mode = mode;
        self
    }

    /// Get the current conversion metrics, if enabled.
    pub async fn metrics(&self) -> Option<ConversionMetrics> {
        let metrics_guard = self.metrics.read().await;
        metrics_guard.as_ref().cloned()
    }

    /// Reset conversion metrics.
    pub async fn reset_metrics(&self) {
        let mut metrics_guard = self.metrics.write().await;
        if metrics_guard.is_some() {
            *metrics_guard = Some(ConversionMetrics::default());
        }
    }

    /// Convert an MCP JSON-RPC request to an A2A Task.
    ///
    /// # Conversion Rules
    ///
    /// - `tools/call` → A2A Task with user text message
    /// - `tools/list` → A2A Task with metadata query
    /// - Other methods → Data part with method and params
    ///
    /// # Errors
    ///
    /// Returns an error if the request is invalid or conversion fails.
    pub async fn mcp_request_to_a2a_task(
        &self, request: &JsonRpcRequest,
    ) -> Result<TaskWrapper, ConversionError> {
        let start = std::time::Instant::now();

        // Validate request
        if request.method.is_empty() {
            return Err(ConversionError::InvalidMcpRequest(
                "Empty method".to_string(),
            ));
        }

        let task_id = request
            .id
            .as_ref()
            .and_then(|id| match id {
                RequestId::String(s) => Some(s.clone()),
                RequestId::Number(n) => Some(n.to_string()),
                RequestId::Null => None,
            })
            .unwrap_or_else(|| format!("task-{}", uuid::Uuid::new_v4()));

        let task = match request.method.as_str() {
            "tools/call" => self.convert_tool_call(request, &task_id)?,
            "tools/list" => self.convert_tools_list(request, &task_id)?,
            _ => self.convert_generic_request(request, &task_id)?,
        };

        // Record metrics
        if self.context.collect_metrics {
            let bytes = estimate_task_size(&task);
            let duration_us = start.elapsed().as_micros() as u64;
            let mut metrics = self.metrics.write().await;
            if let Some(m) = metrics.as_mut() {
                m.record_mcp_to_a2a(bytes, duration_us);
            }
        }

        Ok(TaskWrapper(task))
    }

    /// Convert an A2A Task to an MCP JSON-RPC response.
    ///
    /// # Conversion Rules
    ///
    /// - `TaskState::Completed` → Success response with result data
    /// - `TaskState::Failed` → Error response with failure details
    /// - `TaskState::Working` → Pending response
    /// - Other states → Error response
    ///
    /// # Errors
    ///
    /// Returns an error if the task is invalid or conversion fails.
    pub async fn a2a_task_to_mcp_response(
        &self, task: &TaskWrapper, original_request: &JsonRpcRequest,
    ) -> Result<JsonRpcResponse, ConversionError> {
        let start = std::time::Instant::now();

        let task = &task.0;
        let request_id = original_request.id.clone().unwrap_or_else(RequestId::new);

        let response = match task.status.state {
            TaskState::Completed => self.convert_completed_task(task, request_id)?,
            TaskState::Failed => self.convert_failed_task(task, request_id)?,
            TaskState::Working | TaskState::Submitted => {
                // For pending states, return a "still working" response
                JsonRpcResponse::error(
                    request_id,
                    JsonRpcError::new(-32000, "Task is still being processed"),
                )
            }
            _ => self.convert_failed_task(task, request_id)?,
        };

        // Record metrics
        if self.context.collect_metrics {
            let bytes = estimate_response_size(&response);
            let duration_us = start.elapsed().as_micros() as u64;
            let mut metrics = self.metrics.write().await;
            if let Some(m) = metrics.as_mut() {
                m.record_a2a_to_mcp(bytes, duration_us);
            }
        }

        Ok(response)
    }

    /// Convert MCP tool call to A2A Task.
    fn convert_tool_call(
        &self, request: &JsonRpcRequest, task_id: &str,
    ) -> Result<Task, ConversionError> {
        let tool_name = request
            .params
            .as_ref()
            .and_then(|p| p.get("name"))
            .and_then(|n| n.as_str())
            .ok_or_else(|| ConversionError::MissingField("tool name".to_string()))?;

        let arguments = request
            .params
            .as_ref()
            .and_then(|p| p.get("arguments"))
            .cloned()
            .unwrap_or(JsonValue::Null);

        let message_text = format!("Execute tool: {}\nArguments: {}", tool_name, arguments);

        #[cfg(feature = "a2a")]
        {
            let part = match self.mode {
                McpToA2aConversionMode::ToolToTaskText => {
                    Part::text_with_metadata(message_text, tool_call_metadata(tool_name))
                }
                McpToA2aConversionMode::ToolToTaskData => {
                    let mut data = Map::new();
                    data.insert("tool".to_string(), JsonValue::String(tool_name.to_string()));
                    data.insert("arguments".to_string(), arguments);
                    Part::data(data)
                }
                McpToA2aConversionMode::Passthrough => Part::text_with_metadata(
                    message_text,
                    passthrough_metadata(&request.method, &request.params),
                ),
            };

            Ok(
                Task::new(task_id.to_string(), self.context.context_id.clone()).with_message(
                    Role::User,
                    vec![part],
                    Some(self.context.agent_id.clone()),
                    task_id,
                ),
            )
        }

        #[cfg(not(feature = "a2a"))]
        {
            let _ = (message_text, tool_name, arguments, self.mode);
            Err(ConversionError::A2AError(
                "A2A feature not enabled".to_string(),
            ))
        }
    }

    /// Convert MCP tools list to A2A Task.
    fn convert_tools_list(
        &self, _request: &JsonRpcRequest, task_id: &str,
    ) -> Result<Task, ConversionError> {
        #[cfg(feature = "a2a")]
        {
            let part = Part::text("List available tools".to_string());

            Ok(
                Task::new(task_id.to_string(), self.context.context_id.clone()).with_message(
                    Role::User,
                    vec![part],
                    Some(self.context.agent_id.clone()),
                    task_id,
                ),
            )
        }

        #[cfg(not(feature = "a2a"))]
        {
            let _ = task_id;
            Err(ConversionError::A2AError(
                "A2A feature not enabled".to_string(),
            ))
        }
    }

    /// Convert generic MCP request to A2A Task.
    fn convert_generic_request(
        &self, request: &JsonRpcRequest, task_id: &str,
    ) -> Result<Task, ConversionError> {
        #[cfg(feature = "a2a")]
        {
            let mut data = Map::new();
            data.insert(
                "method".to_string(),
                JsonValue::String(request.method.clone()),
            );
            if let Some(params) = &request.params {
                data.insert("params".to_string(), params.clone());
            }

            let part = Part::data(data);

            Ok(
                Task::new(task_id.to_string(), self.context.context_id.clone()).with_message(
                    Role::User,
                    vec![part],
                    Some(self.context.agent_id.clone()),
                    task_id,
                ),
            )
        }

        #[cfg(not(feature = "a2a"))]
        {
            let _ = (request, task_id);
            Err(ConversionError::A2AError(
                "A2A feature not enabled".to_string(),
            ))
        }
    }

    /// Convert completed A2A Task to MCP success response.
    fn convert_completed_task(
        &self, task: &Task, request_id: RequestId,
    ) -> Result<JsonRpcResponse, ConversionError> {
        #[cfg(feature = "a2a")]
        {
            let result = extract_task_result(task)?;
            Ok(JsonRpcResponse::success(request_id, result))
        }

        #[cfg(not(feature = "a2a"))]
        {
            let _ = (task, request_id);
            Err(ConversionError::A2AError(
                "A2A feature not enabled".to_string(),
            ))
        }
    }

    /// Convert failed A2A Task to MCP error response.
    fn convert_failed_task(
        &self, task: &Task, request_id: RequestId,
    ) -> Result<JsonRpcResponse, ConversionError> {
        let error_message = task
            .status
            .message
            .as_ref()
            .and_then(extract_text_from_message)
            .unwrap_or_else(|| "Task failed".to_string());

        Ok(JsonRpcResponse::error(
            request_id,
            JsonRpcError::new(-32001, error_message),
        ))
    }
}

/// Wrapper type for A2A Task to enable conditional compilation.
#[derive(Debug, Clone)]
pub struct TaskWrapper(
    #[cfg(feature = "a2a")] pub Task,
    #[cfg(not(feature = "a2a"))] pub (),
);

/// Helper to create metadata for tool call messages.
fn tool_call_metadata(tool_name: &str) -> Map<String, JsonValue> {
    let mut metadata = Map::new();
    metadata.insert(
        "tool_name".to_string(),
        JsonValue::String(tool_name.to_string()),
    );
    metadata.insert(
        "call_type".to_string(),
        JsonValue::String("tool_call".to_string()),
    );
    metadata
}

/// Helper to create metadata for passthrough messages.
fn passthrough_metadata(method: &str, params: &Option<JsonValue>) -> Map<String, JsonValue> {
    let mut metadata = Map::new();
    metadata.insert(
        "original_method".to_string(),
        JsonValue::String(method.to_string()),
    );
    if let Some(p) = params {
        metadata.insert("original_params".to_string(), p.clone());
    }
    metadata
}

/// Extract text content from an A2A Message.
#[cfg(feature = "a2a")]
fn extract_text_from_message(message: &Message) -> Option<String> {
    let texts: Vec<String> = message
        .parts
        .iter()
        .map(|part| match part {
            Part::Text { text, .. } => text.clone(),
            Part::Data { data, .. } => serde_json::to_string(data).unwrap_or_default(),
            Part::File { .. } => "[file content]".to_string(),
        })
        .collect();

    if texts.is_empty() {
        None
    } else {
        Some(texts.join("\n"))
    }
}

/// Extract result data from an A2A Task.
#[cfg(feature = "a2a")]
fn extract_task_result(task: &Task) -> Result<JsonValue, ConversionError> {
    if let Some(message) = &task.status.message {
        let content = extract_text_from_message(message)
            .ok_or_else(|| ConversionError::TypeConversion("No content in message".to_string()))?;

        let mut content_map = Map::new();
        content_map.insert("isError".to_string(), JsonValue::Bool(false));

        let content_array = vec![JsonValue::Object({
            let mut item = Map::new();
            item.insert("type".to_string(), JsonValue::String("text".to_string()));
            item.insert("text".to_string(), JsonValue::String(content));
            item
        })];
        content_map.insert("content".to_string(), JsonValue::Array(content_array));

        return Ok(JsonValue::Object(content_map));
    }

    // Fallback to artifacts if available
    if let Some(artifacts) = &task.artifacts {
        let content: Vec<JsonValue> = artifacts
            .iter()
            .filter_map(|artifact| {
                artifact.parts.iter().find_map(|part| match part {
                    Part::Text { text, .. } => {
                        let mut item = Map::new();
                        item.insert("type".to_string(), JsonValue::String("text".to_string()));
                        item.insert("text".to_string(), JsonValue::String(text.clone()));
                        Some(JsonValue::Object(item))
                    }
                    _ => None,
                })
            })
            .collect();

        let mut result_map = Map::new();
        result_map.insert("content".to_string(), JsonValue::Array(content));
        result_map.insert("isError".to_string(), JsonValue::Bool(false));
        return Ok(JsonValue::Object(result_map));
    }

    Ok(JsonValue::Null)
}

/// Estimate the size of a Task for metrics.
#[cfg(feature = "a2a")]
fn estimate_task_size(task: &Task) -> u64 {
    serde_json::to_string(task)
        .map(|s| s.len() as u64)
        .unwrap_or(0)
}

/// Estimate the size of a Response for metrics.
fn estimate_response_size(response: &JsonRpcResponse) -> u64 {
    serde_json::to_string(response)
        .map(|s| s.len() as u64)
        .unwrap_or(0)
}

// Trait implementations for TaskWrapper to enable access to underlying Task
#[cfg(feature = "a2a")]
impl TaskWrapper {
    /// Get a reference to the underlying Task.
    pub fn inner(&self) -> &Task {
        &self.0
    }

    /// Consume the wrapper and return the Task.
    pub fn into_inner(self) -> Task {
        self.0
    }
}

/// Extension trait for Task with convenient builder methods.
#[cfg(feature = "a2a")]
trait TaskExt {
    fn with_message(
        self, role: Role, parts: Vec<Part>, agent_id: Option<String>, task_id: &str,
    ) -> Self;
}

#[cfg(feature = "a2a")]
impl TaskExt for Task {
    fn with_message(
        mut self, role: Role, parts: Vec<Part>, _agent_id: Option<String>, task_id: &str,
    ) -> Self {
        let message = Message {
            role,
            parts,
            metadata: None,
            reference_task_ids: None,
            message_id: format!("msg-{}", uuid::Uuid::new_v4()),
            task_id: Some(task_id.to_string()),
            context_id: Some(self.context_id.clone()),
            extensions: None,
            kind: "message".to_string(),
        };

        self.status.message = Some(message);
        self
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_conversion_context_default() {
        let context = ConversionContext::default();
        assert_eq!(context.agent_id, "rig-mcp-bridge");
        assert!(context.bidirectional);
    }

    #[test]
    fn test_conversion_metrics_default() {
        let metrics = ConversionMetrics::default();
        assert_eq!(metrics.mcp_to_a2a_count, 0);
        assert_eq!(metrics.a2a_to_mcp_count, 0);
        assert_eq!(metrics.error_count, 0);
    }

    #[test]
    fn test_conversion_metrics_recording() {
        let mut metrics = ConversionMetrics::default();
        metrics.record_mcp_to_a2a(100, 50);
        metrics.record_a2a_to_mcp(200, 75);

        assert_eq!(metrics.mcp_to_a2a_count, 1);
        assert_eq!(metrics.a2a_to_mcp_count, 1);
        assert_eq!(metrics.total_bytes, 300);
        // Average time calculation: (50 * 0 + 75) / 1 = 75 for first a2a_to_mcp
        // Actually, the formula updates incrementally based on total count
    }

    #[test]
    fn test_conversion_mode_default() {
        let mode = McpToA2aConversionMode::default();
        assert_eq!(mode, McpToA2aConversionMode::ToolToTaskText);
    }

    #[test]
    fn test_converter_creation() {
        let context = ConversionContext::default();
        let converter = A2aMessageConverter::new(context);
        assert_eq!(converter.context.agent_id, "rig-mcp-bridge");
    }

    #[test]
    fn test_converter_with_mode() {
        let context = ConversionContext::default();
        let converter =
            A2aMessageConverter::new(context).with_mode(McpToA2aConversionMode::Passthrough);
        assert_eq!(converter.mode, McpToA2aConversionMode::Passthrough);
    }

    #[test]
    fn test_tool_call_metadata() {
        let metadata = tool_call_metadata("test_tool");
        assert_eq!(
            metadata.get("tool_name"),
            Some(&JsonValue::String("test_tool".to_string()))
        );
        assert_eq!(
            metadata.get("call_type"),
            Some(&JsonValue::String("tool_call".to_string()))
        );
    }

    #[test]
    fn test_passthrough_metadata() {
        let params = Some(JsonValue::Object({
            let mut map = Map::new();
            map.insert("key".to_string(), JsonValue::String("value".to_string()));
            map
        }));

        let metadata = passthrough_metadata("test_method", &params);
        assert_eq!(
            metadata.get("original_method"),
            Some(&JsonValue::String("test_method".to_string()))
        );
    }

    #[test]
    fn test_conversion_error_display() {
        let err = ConversionError::InvalidMcpRequest("test error".to_string());
        assert!(err.to_string().contains("test error"));
    }

    #[test]
    fn test_estimate_response_size() {
        let response = JsonRpcResponse::success(
            RequestId::String("test".to_string()),
            JsonValue::String("result".to_string()),
        );

        let size = estimate_response_size(&response);
        assert!(size > 0);
    }

    #[tokio::test]
    async fn test_converter_metrics_none() {
        let context = ConversionContext {
            collect_metrics: false,
            ..Default::default()
        };

        let converter = A2aMessageConverter::new(context);
        let metrics = converter.metrics().await;
        assert!(metrics.is_none());
    }
}
