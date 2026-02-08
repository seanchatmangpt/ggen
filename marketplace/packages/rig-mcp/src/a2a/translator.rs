//! MCP to A2A Protocol Translation Layer
//!
//! This module provides bidirectional protocol translation between the Model Context Protocol (MCP)
//! and the Agent-to-Agent (A2A) protocol. It handles:
//!
//! - **Schema Generation**: Converting A2A AgentCards and AgentSkills to MCP Tool schemas
//! - **Message Translation**: Converting between MCP JSON-RPC messages and A2A Tasks/Messages
//! - **Tool Mapping**: Mapping A2A skills to MCP tools with proper input schema generation
//!
//! ## Architecture
//!
//! The translator sits between MCP clients and A2A agents, providing:
//!
//! ```text
//! MCP Client                Translator                A2A Agent
//!    │                          │                          │
//!    ├── tools/list  ──────────┼──► AgentCard.skills ─────┼──► Get capabilities
//!    │                          │                          │
//!    ├── tools/call  ──────────┼──► Task with Message ────┼──► Execute skill
//!    │                          │                          │
//!    ◄── ToolResult  ──────────┼──◄─ TaskStatus/Artifacts ┼──◄─ Return result
//!    │                          │                          │
//! ```
//!
//! ## Tool Schema Generation
//!
//! A2A skills are mapped to MCP tools with the following rules:
//!
//! - `AgentSkill.name` → `Tool.name` (prefixed with agent name if multiple agents)
//! - `AgentSkill.description` → `Tool.description`
//! - `AgentSkill.examples` → Documentation in schema
//! - Input/Output modes → JSON Schema constraints
//!
//! ## Example
//!
//! ```rust,no_run
//! use rig_mcp_integration::a2a::translator::{
//!     ProtocolTranslator, TranslatorConfig, ToolSchemaGenerator,
//! };
//! use rig_mcp_integration::transport::Tool;
//!
//! # async fn example() -> Result<(), Box<dyn std::error::Error>> {
//! // Create translator with default configuration
//! let translator = ProtocolTranslator::new(TranslatorConfig::default());
//!
//! // Generate MCP tools from an A2A AgentCard
//! # let agent_card = todo!();
//! let tools: Vec<Tool> = translator.generate_tools_from_card(&agent_card)?;
//!
//! // Convert MCP tool call to A2A Task
//! # let mcp_request = todo!();
//! let a2a_task = translator.tool_call_to_task(&mcp_request)?;
//!
//! // Convert A2A Task result to MCP response
//! # let a2a_result = todo!();
//! let mcp_response = translator.task_result_to_response(&a2a_result)?;
//! # Ok(())
//! # }
//! ```

use crate::transport::{JsonRpcError, JsonRpcRequest, JsonRpcResponse, RequestId, Tool};
use serde::{Deserialize, Serialize};
use serde_json::{Map, Value as JsonValue};
use thiserror::Error;

// A2A types from the vendor crate
#[cfg(feature = "a2a")]
use a2a_rs::{
    domain::core::{
        agent::{AgentCard, AgentSkill},
        message::{Artifact, Message, Part, Role},
        task::{Task, TaskState, TaskStatus},
    },
};

/// Configuration for the protocol translator.
///
/// Controls how translation between MCP and A2A protocols is performed,
/// including naming conventions, schema generation options, and error handling.
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct TranslatorConfig {
    /// Prefix to add to tool names for disambiguation (e.g., agent name)
    #[serde(skip_serializing_if = "Option::is_none")]
    pub tool_name_prefix: Option<String>,

    /// Whether to include examples in generated tool schemas
    #[serde(default = "default_include_examples")]
    pub include_examples: bool,

    /// Whether to include skill tags in tool metadata
    #[serde(default = "default_include_tags")]
    pub include_tags: bool,

    /// Whether to use strict schema validation (fails on unknown properties)
    #[serde(default = "default_strict_validation")]
    pub strict_validation: bool,

    /// Default timeout for tool execution in milliseconds
    #[serde(default = "default_timeout_ms")]
    pub default_timeout_ms: u64,
}

fn default_include_examples() -> bool {
    true
}

fn default_include_tags() -> bool {
    true
}

fn default_strict_validation() -> bool {
    false
}

fn default_timeout_ms() -> u64 {
    30000
}

impl Default for TranslatorConfig {
    fn default() -> Self {
        Self {
            tool_name_prefix: None,
            include_examples: default_include_examples(),
            include_tags: default_include_tags(),
            strict_validation: default_strict_validation(),
            default_timeout_ms: default_timeout_ms(),
        }
    }
}

impl TranslatorConfig {
    /// Create a new translator configuration.
    pub fn new() -> Self {
        Self::default()
    }

    /// Set a prefix for tool names (useful for multi-agent scenarios).
    pub fn with_tool_name_prefix(mut self, prefix: impl Into<String>) -> Self {
        self.tool_name_prefix = Some(prefix.into());
        self
    }

    /// Configure whether to include examples in generated schemas.
    pub fn with_include_examples(mut self, include: bool) -> Self {
        self.include_examples = include;
        self
    }

    /// Configure whether to include skill tags in tool metadata.
    pub fn with_include_tags(mut self, include: bool) -> Self {
        self.include_tags = include;
        self
    }

    /// Set the default timeout for tool execution.
    pub fn with_timeout(mut self, timeout_ms: u64) -> Self {
        self.default_timeout_ms = timeout_ms;
        self
    }
}

/// Error type for protocol translation operations.
#[derive(Error, Debug)]
pub enum TranslationError {
    /// Invalid MCP request format
    #[error("Invalid MCP request: {0}")]
    InvalidMcpRequest(String),

    /// Invalid A2A task format
    #[error("Invalid A2A task: {0}")]
    InvalidA2aTask(String),

    /// Invalid AgentCard
    #[error("Invalid AgentCard: {0}")]
    InvalidAgentCard(String),

    /// Schema generation error
    #[error("Schema generation error: {0}")]
    SchemaError(String),

    /// Tool not found in AgentCard
    #[error("Tool '{0}' not found in AgentCard skills")]
    ToolNotFound(String),

    /// Missing required field
    #[error("Missing required field: {0}")]
    MissingField(String),

    /// Type conversion failed
    #[error("Type conversion failed: {0}")]
    TypeConversion(String),

    /// A2A protocol error
    #[error("A2A protocol error: {0}")]
    A2AError(String),

    /// JSON serialization/deserialization error
    #[error("JSON error: {0}")]
    JsonError(#[from] serde_json::Error),

    /// Unsupported operation
    #[error("Unsupported operation: {0}")]
    Unsupported(String),
}

impl From<TranslationError> for crate::transport::TransportError {
    fn from(err: TranslationError) -> Self {
        crate::transport::TransportError::Internal(err.to_string())
    }
}

/// Result type for translation operations.
pub type TranslationResult<T> = Result<T, TranslationError>;

/// MCP to A2A Protocol Translator.
///
/// Handles bidirectional translation between MCP JSON-RPC protocol and
/// A2A protocol messages and schemas.
#[derive(Clone)]
pub struct ProtocolTranslator {
    /// Translator configuration
    config: TranslatorConfig,
    /// Cached AgentCard for tool schema generation
    #[cfg(feature = "a2a")]
    agent_card: Option<AgentCard>,
}

impl ProtocolTranslator {
    /// Create a new protocol translator with the given configuration.
    pub fn new(config: TranslatorConfig) -> Self {
        Self {
            config,
            #[cfg(feature = "a2a")]
            agent_card: None,
        }
    }

    /// Create a translator with default configuration.
    pub fn default_config() -> Self {
        Self::new(TranslatorConfig::default())
    }

    /// Set the AgentCard for schema generation and validation.
    #[cfg(feature = "a2a")]
    pub fn with_agent_card(mut self, card: AgentCard) -> Self {
        self.agent_card = Some(card);
        self
    }

    /// Update the cached AgentCard.
    #[cfg(feature = "a2a")]
    pub fn set_agent_card(&mut self, card: AgentCard) {
        self.agent_card = Some(card);
    }

    /// Get the current AgentCard if set.
    #[cfg(feature = "a2a")]
    pub fn agent_card(&self) -> Option<&AgentCard> {
        self.agent_card.as_ref()
    }

    /// Generate MCP Tool schemas from an A2A AgentCard.
    ///
    /// Each skill in the AgentCard is converted to an MCP Tool with:
    /// - Name: `{prefix?}{skill.id}` or `{prefix?}{skill.name}`
    /// - Description: skill description with examples appended
    /// - Input schema: JSON Schema derived from skill's input modes
    ///
    /// # Errors
    ///
    /// Returns an error if the AgentCard is invalid or schema generation fails.
    #[cfg(feature = "a2a")]
    pub fn generate_tools_from_card(&self, card: &AgentCard) -> TranslationResult<Vec<Tool>> {
        let mut tools = Vec::new();

        for skill in &card.skills {
            let tool = self.skill_to_tool(card, skill)?;
            tools.push(tool);
        }

        Ok(tools)
    }

    /// Generate MCP Tools from the currently configured AgentCard.
    ///
    /// # Errors
    ///
    /// Returns an error if no AgentCard is configured.
    #[cfg(feature = "a2a")]
    pub fn generate_tools(&self) -> TranslationResult<Vec<Tool>> {
        let card = self
            .agent_card
            .as_ref()
            .ok_or_else(|| TranslationError::InvalidAgentCard("No AgentCard configured".to_string()))?;

        self.generate_tools_from_card(card)
    }

    /// Convert a single A2A AgentSkill to an MCP Tool.
    ///
    /// # Tool Name Mapping
    ///
    /// - If `tool_name_prefix` is set: `{prefix}_{skill.id}`
    /// - Otherwise: `{skill.id}`
    ///
    /// # Input Schema Generation
    ///
    /// The input schema is generated based on:
    /// - `skill.input_modes`: Defines expected input types (text, data, file)
    /// - `skill.examples`: Added as schema documentation/examples
    #[cfg(feature = "a2a")]
    fn skill_to_tool(&self, card: &AgentCard, skill: &AgentSkill) -> TranslationResult<Tool> {
        // Generate tool name with optional prefix
        let tool_name = if let Some(prefix) = &self.config.tool_name_prefix {
            format!("{}_{}", prefix, skill.id)
        } else {
            skill.id.clone()
        };

        // Build description with examples if enabled
        let mut description = skill.description.clone();
        if self.config.include_examples {
            if let Some(examples) = &skill.examples {
                if !examples.is_empty() {
                    description.push_str("\n\nExamples:\n");
                    for example in examples {
                        description.push_str(&format!("- {}\n", example));
                    }
                }
            }
        }

        // Add skill tags to description if enabled
        if self.config.include_tags && !skill.tags.is_empty() {
            description.push_str(&format!("\n\nTags: {}", skill.tags.join(", ")));
        }

        // Generate input schema
        let input_schema = self.generate_input_schema(skill, card)?;

        Ok(Tool {
            name: tool_name,
            description,
            input_schema: Some(input_schema),
        })
    }

    /// Generate JSON Schema for a skill's input.
    ///
    /// The schema is generated based on the skill's input modes and examples.
    /// Basic schema structure:
    ///
    /// ```json
    /// {
    ///   "type": "object",
    ///   "properties": {
    ///     "prompt": { "type": "string", "description": "..." }
    ///   },
    ///   "required": ["prompt"]
    /// }
    /// ```
    #[cfg(feature = "a2a")]
    fn generate_input_schema(&self, skill: &AgentSkill, _card: &AgentCard) -> TranslationResult<JsonValue> {
        let mut properties = Map::new();

        // Add prompt property for text input modes
        let has_text_input = skill
            .input_modes
            .as_ref()
            .map(|modes| modes.iter().any(|m| m == "text"))
            .unwrap_or(true); // Default to text if not specified

        if has_text_input {
            let mut prompt_schema = Map::new();
            prompt_schema.insert("type".to_string(), JsonValue::String("string".to_string()));
            prompt_schema.insert(
                "description".to_string(),
                JsonValue::String(format!("Input prompt for the {} skill", skill.name)),
            );

            // Add examples to the prompt schema if available
            if self.config.include_examples {
                if let Some(examples) = &skill.examples {
                    if !examples.is_empty() {
                        prompt_schema.insert(
                            "examples".to_string(),
                            JsonValue::Array(
                                examples.iter().map(|e| JsonValue::String(e.clone())).collect(),
                            ),
                        );
                    }
                }
            }

            properties.insert("prompt".to_string(), JsonValue::Object(prompt_schema));
        }

        // Add data property for structured data input modes
        let has_data_input = skill
            .input_modes
            .as_ref()
            .map(|modes| modes.iter().any(|m| m == "data"))
            .unwrap_or(false);

        if has_data_input {
            let mut data_schema = Map::new();
            data_schema.insert("type".to_string(), JsonValue::String("object".to_string()));
            data_schema.insert(
                "description".to_string(),
                JsonValue::String("Structured data input for the skill".to_string()),
            );
            properties.insert("data".to_string(), JsonValue::Object(data_schema));
        }

        // Build the final schema
        let mut schema = Map::new();
        schema.insert("type".to_string(), JsonValue::String("object".to_string()));
        schema.insert("properties".to_string(), JsonValue::Object(properties));

        // Add required fields
        let required = if has_text_input {
            vec!["prompt".to_string()]
        } else {
            vec![]
        };
        schema.insert(
            "required".to_string(),
            JsonValue::Array(required.into_iter().map(JsonValue::String).collect()),
        );

        // Add additionalProperties setting based on strict_validation
        schema.insert(
            "additionalProperties".to_string(),
            JsonValue::Bool(!self.config.strict_validation),
        );

        Ok(JsonValue::Object(schema))
    }

    /// Convert an MCP tools/call request to an A2A Task.
    ///
    /// # Conversion Rules
    ///
    /// - Request ID → Task ID
    /// - Tool name → Maps to skill ID (with prefix stripped)
    /// - Arguments → Message Part based on input mode
    ///
    /// # Errors
    ///
    /// Returns an error if:
    /// - The request is not a tools/call
    /// - Tool name is missing
    /// - Arguments cannot be extracted
    #[cfg(feature = "a2a")]
    pub fn tool_call_to_task(&self, request: &JsonRpcRequest) -> TranslationResult<Task> {
        if request.method != "tools/call" {
            return Err(TranslationError::InvalidMcpRequest(
                "Expected tools/call method".to_string(),
            ));
        }

        // Extract tool name from params
        let params = request
            .params
            .as_ref()
            .ok_or_else(|| TranslationError::MissingField("params".to_string()))?;

        let raw_tool_name = params
            .get("name")
            .and_then(|v| v.as_str())
            .ok_or_else(|| TranslationError::MissingField("name".to_string()))?;

        // Strip prefix if configured
        let skill_id = if let Some(prefix) = &self.config.tool_name_prefix {
            raw_tool_name
                .strip_prefix(&format!("{}_", prefix))
                .unwrap_or(raw_tool_name)
        } else {
            raw_tool_name
        };

        // Extract arguments
        let arguments = params
            .get("arguments")
            .cloned()
            .unwrap_or(JsonValue::Null);

        // Generate task ID from request ID
        let task_id = request
            .id
            .as_ref()
            .and_then(|id| match id {
                RequestId::String(s) => Some(s.clone()),
                RequestId::Number(n) => Some(n.to_string()),
                RequestId::Null => None,
            })
            .unwrap_or_else(|| format!("task-{}", uuid::Uuid::new_v4()));

        // Generate context ID
        let context_id = format!("ctx-{}", uuid::Uuid::new_v4());

        // Build the message parts from arguments
        let parts = self.arguments_to_parts(&arguments, skill_id)?;

        // Create the message
        let message = Message {
            role: Role::User,
            parts,
            metadata: None,
            reference_task_ids: None,
            message_id: format!("msg-{}", uuid::Uuid::new_v4()),
            task_id: Some(task_id.clone()),
            context_id: Some(context_id.clone()),
            extensions: None,
            kind: "message".to_string(),
        };

        // Create the task
        let task = Task::builder()
            .id(task_id)
            .context_id(context_id)
            .status(TaskStatus {
                state: TaskState::Submitted,
                message: Some(message),
                timestamp: Some(chrono::Utc::now()),
            })
            .build();

        Ok(task)
    }

    /// Convert MCP tool call arguments to A2A Message Parts.
    ///
    /// Handles different argument structures:
    /// - `{"prompt": "text"}` → Text part
    /// - `{"data": {...}}` → Data part
    /// - Other structures → Data part with raw arguments
    #[cfg(feature = "a2a")]
    fn arguments_to_parts(&self, arguments: &JsonValue, _skill_id: &str) -> TranslationResult<Vec<Part>> {
        let mut parts = Vec::new();

        match arguments {
            JsonValue::Object(args) => {
                // Check for prompt field (text input)
                if let Some(prompt) = args.get("prompt").and_then(|v| v.as_str()) {
                    parts.push(Part::Text {
                        text: prompt.to_string(),
                        metadata: None,
                    });
                }

                // Check for data field (structured input)
                if let Some(data) = args.get("data") {
                    if let Some(data_map) = data.as_object() {
                        parts.push(Part::Data {
                            data: data_map.clone(),
                            metadata: None,
                        });
                    }
                }

                // If no recognized fields, treat entire object as data
                if parts.is_empty() {
                    parts.push(Part::Data {
                        data: args.clone(),
                        metadata: None,
                    });
                }
            }
            JsonValue::String(text) => {
                parts.push(Part::Text {
                    text: text.clone(),
                    metadata: None,
                });
            }
            _ => {
                // Fallback: convert to string as text part
                parts.push(Part::Text {
                    text: arguments.to_string(),
                    metadata: None,
                });
            }
        }

        Ok(parts)
    }

    /// Convert an A2A Task result to an MCP JSON-RPC response.
    ///
    /// # Conversion Rules
    ///
    /// - `TaskState::Completed` → Success response with content from artifacts
    /// - `TaskState::Failed` → Error response with failure message
    /// - Other states → Error response indicating pending state
    ///
    /// # Errors
    ///
    /// Returns an error if the task status cannot be converted.
    #[cfg(feature = "a2a")]
    pub fn task_result_to_response(
        &self,
        task: &Task,
        original_request: &JsonRpcRequest,
    ) -> TranslationResult<JsonRpcResponse> {
        let request_id = original_request
            .id
            .clone()
            .unwrap_or_else(RequestId::new);

        match &task.status.state {
            TaskState::Completed => {
                let content = self.extract_task_content(task)?;
                let result = self.build_tool_result(content)?;
                Ok(JsonRpcResponse::success(request_id, result))
            }
            TaskState::Failed => {
                let error_msg = task
                    .status
                    .message
                    .as_ref()
                    .and_then(|msg| self.extract_text_from_message(msg))
                    .unwrap_or_else(|| "Task execution failed".to_string());

                Ok(JsonRpcResponse::error(
                    request_id,
                    JsonRpcError::new(-32001, error_msg),
                ))
            }
            TaskState::Working | TaskState::Submitted => {
                Ok(JsonRpcResponse::error(
                    request_id,
                    JsonRpcError::new(-32000, "Task is still being processed"),
                ))
            }
            _ => {
                Ok(JsonRpcResponse::error(
                    request_id,
                    JsonRpcError::new(-32002, format!("Task in unexpected state: {:?}", task.status.state)),
                ))
            }
        }
    }

    /// Extract content from an A2A Task for the MCP response.
    ///
    /// Priority order:
    /// 1. Artifacts (if available)
    /// 2. Status message content
    /// 3. Empty content placeholder
    #[cfg(feature = "a2a")]
    fn extract_task_content(&self, task: &Task) -> TranslationResult<String> {
        // First check artifacts
        if let Some(artifacts) = &task.artifacts {
            if !artifacts.is_empty() {
                let content_parts: Vec<String> = artifacts
                    .iter()
                    .filter_map(|artifact| self.extract_text_from_artifact(artifact))
                    .collect();

                if !content_parts.is_empty() {
                    return Ok(content_parts.join("\n"));
                }
            }
        }

        // Then check status message
        if let Some(message) = &task.status.message {
            if let Some(text) = self.extract_text_from_message(message) {
                return Ok(text);
            }
        }

        // Default empty response
        Ok(String::new())
    }

    /// Extract text content from an A2A Artifact.
    #[cfg(feature = "a2a")]
    fn extract_text_from_artifact(&self, artifact: &Artifact) -> Option<String> {
        let texts: Vec<String> = artifact
            .parts
            .iter()
            .filter_map(|part| match part {
                Part::Text { text, .. } => Some(text.clone()),
                Part::Data { data, .. } => serde_json::to_string(data).ok(),
                Part::File { .. } => Some("[file content]".to_string()),
            })
            .collect();

        if texts.is_empty() {
            None
        } else {
            Some(texts.join("\n"))
        }
    }

    /// Extract text content from an A2A Message.
    #[cfg(feature = "a2a")]
    fn extract_text_from_message(&self, message: &Message) -> Option<String> {
        let texts: Vec<String> = message
            .parts
            .iter()
            .filter_map(|part| match part {
                Part::Text { text, .. } => Some(text.clone()),
                Part::Data { data, .. } => serde_json::to_string(data).ok(),
                Part::File { .. } => Some("[file content]".to_string()),
            })
            .collect();

        if texts.is_empty() {
            None
        } else {
            Some(texts.join("\n"))
        }
    }

    /// Build an MCP ToolResult from content.
    #[cfg(feature = "a2a")]
    fn build_tool_result(&self, content: String) -> TranslationResult<JsonValue> {
        let content_item = serde_json::json!({
            "type": "text",
            "text": content
        });

        let result = serde_json::json!({
            "content": [content_item],
            "isError": false
        });

        Ok(result)
    }

    /// Find a skill by tool name in the configured AgentCard.
    ///
    /// Handles prefix stripping if configured.
    #[cfg(feature = "a2a")]
    pub fn find_skill_by_tool_name(&self, tool_name: &str) -> TranslationResult<Option<&AgentSkill>> {
        let card = self
            .agent_card
            .as_ref()
            .ok_or_else(|| TranslationError::InvalidAgentCard("No AgentCard configured".to_string()))?;

        // Strip prefix if configured
        let skill_id = if let Some(prefix) = &self.config.tool_name_prefix {
            tool_name
                .strip_prefix(&format!("{}_", prefix))
                .unwrap_or(tool_name)
        } else {
            tool_name
        };

        Ok(card.skills.iter().find(|s| s.id == skill_id))
    }
}

/// Generator for MCP Tool schemas from A2A protocol definitions.
///
/// Provides static methods for schema generation without needing a full translator instance.
pub struct ToolSchemaGenerator;

impl ToolSchemaGenerator {
    /// Generate a basic MCP Tool schema from an AgentSkill.
    ///
    /// This is a simplified version that doesn't require a full translator instance.
    #[cfg(feature = "a2a")]
    pub fn skill_to_tool_schema(skill: &AgentSkill) -> TranslationResult<Tool> {
        let input_schema = Self::generate_basic_input_schema(skill)?;

        Ok(Tool {
            name: skill.id.clone(),
            description: skill.description.clone(),
            input_schema: Some(input_schema),
        })
    }

    /// Generate a basic input schema for a skill.
    #[cfg(feature = "a2a")]
    fn generate_basic_input_schema(skill: &AgentSkill) -> TranslationResult<JsonValue> {
        let mut properties = Map::new();

        // Always include a prompt property for text input
        let mut prompt_schema = Map::new();
        prompt_schema.insert("type".to_string(), JsonValue::String("string".to_string()));
        prompt_schema.insert(
            "description".to_string(),
            JsonValue::String(format!("Input for {}", skill.name)),
        );
        properties.insert("prompt".to_string(), JsonValue::Object(prompt_schema));

        let mut schema = Map::new();
        schema.insert("type".to_string(), JsonValue::String("object".to_string()));
        schema.insert("properties".to_string(), JsonValue::Object(properties));
        schema.insert(
            "required".to_string(),
            JsonValue::Array(vec![JsonValue::String("prompt".to_string())]),
        );

        Ok(JsonValue::Object(schema))
    }
}

// Default implementation for no-std or when a2a feature is disabled
#[cfg(not(feature = "a2a"))]
impl ProtocolTranslator {
    /// Generate MCP Tools (stub when a2a feature is disabled).
    pub fn generate_tools(&self) -> TranslationResult<Vec<Tool>> {
        Err(TranslationError::A2AError(
            "A2A feature not enabled".to_string(),
        ))
    }

    /// Generate tools from card (stub when a2a feature is disabled).
    pub fn generate_tools_from_card(&self, _card: &()) -> TranslationResult<Vec<Tool>> {
        Err(TranslationError::A2AError(
            "A2A feature not enabled".to_string(),
        ))
    }

    /// Convert tool call to task (stub when a2a feature is disabled).
    pub fn tool_call_to_task(&self, _request: &JsonRpcRequest) -> TranslationResult<()> {
        Err(TranslationError::A2AError(
            "A2A feature not enabled".to_string(),
        ))
    }

    /// Convert task result to response (stub when a2a feature is disabled).
    pub fn task_result_to_response(
        &self,
        _task: &(),
        _original_request: &JsonRpcRequest,
    ) -> TranslationResult<JsonRpcResponse> {
        Err(TranslationError::A2AError(
            "A2A feature not enabled".to_string(),
        ))
    }
}

#[cfg(not(feature = "a2a"))]
impl ToolSchemaGenerator {
    /// Generate skill schema (stub when a2a feature is disabled).
    pub fn skill_to_tool_schema(_skill: &()) -> TranslationResult<Tool> {
        Err(TranslationError::A2AError(
            "A2A feature not enabled".to_string(),
        ))
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_translator_config_default() {
        let config = TranslatorConfig::default();
        assert!(config.tool_name_prefix.is_none());
        assert!(config.include_examples);
        assert!(config.include_tags);
        assert_eq!(config.default_timeout_ms, 30000);
    }

    #[test]
    fn test_translator_config_builder() {
        let config = TranslatorConfig::new()
            .with_tool_name_prefix("test_agent")
            .with_include_examples(false)
            .with_include_tags(false)
            .with_timeout(5000);

        assert_eq!(config.tool_name_prefix, Some("test_agent".to_string()));
        assert!(!config.include_examples);
        assert!(!config.include_tags);
        assert_eq!(config.default_timeout_ms, 5000);
    }

    #[test]
    fn test_protocol_translator_creation() {
        let translator = ProtocolTranslator::default_config();
        assert!(translator.config.tool_name_prefix.is_none());
    }

    #[test]
    fn test_translation_error_display() {
        let err = TranslationError::InvalidMcpRequest("test error".to_string());
        assert!(err.to_string().contains("test error"));
    }

    #[test]
    fn test_translation_error_tool_not_found() {
        let err = TranslationError::ToolNotFound("my_tool".to_string());
        assert!(err.to_string().contains("my_tool"));
        assert!(err.to_string().contains("not found"));
    }

    #[test]
    fn test_tool_name_prefix_stripping() {
        let config = TranslatorConfig::new()
            .with_tool_name_prefix("agent");

        // Test stripping logic
        let prefixed = "agent_my_skill";
        let stripped = prefixed
            .strip_prefix(&format!("{}_", "agent"))
            .unwrap_or(prefixed);
        assert_eq!(stripped, "my_skill");

        // Test non-prefixed name
        let no_prefix = "my_skill";
        let not_stripped = no_prefix
            .strip_prefix(&format!("{}_", "agent"))
            .unwrap_or(no_prefix);
        assert_eq!(not_stripped, "my_skill");
    }

    #[test]
    fn test_translator_without_a2a_feature() {
        let translator = ProtocolTranslator::default_config();
        // Should return error when a2a feature is not enabled
        let result = translator.generate_tools();
        #[cfg(not(feature = "a2a"))]
        assert!(result.is_err());
        #[cfg(feature = "a2a")]
        assert!(result.is_err()); // No agent card configured
    }
}

#[cfg(test)]
#[cfg(feature = "a2a")]
mod integration_tests {
    use super::*;
    use a2a_rs::domain::core::agent::{AgentCapabilities, AgentCard};

    /// Helper to create a test AgentCard with skills.
    fn create_test_agent_card() -> AgentCard {
        AgentCard::builder()
            .name("Test Agent".to_string())
            .description("A test agent".to_string())
            .url("https://example.com".to_string())
            .version("1.0.0".to_string())
            .capabilities(AgentCapabilities::default())
            .default_input_modes(vec!["text".to_string()])
            .default_output_modes(vec!["text".to_string()])
            .skills(vec![])
            .build()
    }

    /// Helper to create a test AgentCard with a specific skill.
    fn create_agent_card_with_skill(skill: AgentSkill) -> AgentCard {
        AgentCard::builder()
            .name("Test Agent".to_string())
            .description("A test agent".to_string())
            .url("https://example.com".to_string())
            .version("1.0.0".to_string())
            .capabilities(AgentCapabilities::default())
            .default_input_modes(vec!["text".to_string()])
            .default_output_modes(vec!["text".to_string()])
            .skills(vec![skill])
            .build()
    }

    #[test]
    fn test_generate_tools_from_agent_card() {
        let skill = AgentSkill::new(
            "text_generation".to_string(),
            "Text Generation".to_string(),
            "Generate text based on prompts".to_string(),
            vec!["nlp".to_string(), "generation".to_string()],
        );

        let card = create_agent_card_with_skill(skill);
        let translator = ProtocolTranslator::default_config();
        let tools = translator.generate_tools_from_card(&card).unwrap();

        assert_eq!(tools.len(), 1);
        assert_eq!(tools[0].name, "text_generation");
        assert!(tools[0].description.contains("Generate text"));
        assert!(tools[0].input_schema.is_some());
    }

    #[test]
    fn test_generate_tools_with_prefix() {
        let skill = AgentSkill::new(
            "analyze".to_string(),
            "Analysis".to_string(),
            "Analyze data".to_string(),
            vec![],
        );

        let card = create_agent_card_with_skill(skill);

        let translator = ProtocolTranslator::new(
            TranslatorConfig::new().with_tool_name_prefix("my_agent"),
        );
        let tools = translator.generate_tools_from_card(&card).unwrap();

        assert_eq!(tools.len(), 1);
        assert_eq!(tools[0].name, "my_agent_analyze");
    }

    #[test]
    fn test_tool_call_to_task_conversion() {
        let translator = ProtocolTranslator::default_config();

        let request = JsonRpcRequest::new(
            "tools/call",
            Some(serde_json::json!({
                "name": "test_skill",
                "arguments": {
                    "prompt": "Hello, world!"
                }
            })),
        );

        let task = translator.tool_call_to_task(&request).unwrap();

        assert!(!task.id.is_empty());
        assert_eq!(task.status.state, TaskState::Submitted);
        assert!(task.status.message.is_some());

        let message = task.status.message.as_ref().unwrap();
        assert_eq!(message.role, Role::User);
        assert!(!message.parts.is_empty());
    }

    #[test]
    fn test_task_result_to_response_success() {
        let translator = ProtocolTranslator::default_config();

        // Create a completed task
        let message = Message::builder()
            .role(Role::Agent)
            .parts(vec![Part::Text {
                text: "Task completed successfully".to_string(),
                metadata: None,
            }])
            .message_id("msg-123".to_string())
            .build();

        let task = Task::builder()
            .id("task-123".to_string())
            .context_id("ctx-123".to_string())
            .status(TaskStatus {
                state: TaskState::Completed,
                message: Some(message),
                timestamp: Some(chrono::Utc::now()),
            })
            .build();

        let original_request = JsonRpcRequest::new("tools/call", None);
        let response = translator.task_result_to_response(&task, &original_request).unwrap();

        assert!(response.is_success());
        assert!(response.result.is_some());
    }

    #[test]
    fn test_task_result_to_response_failure() {
        let translator = ProtocolTranslator::default_config();

        // Create a failed task
        let message = Message::builder()
            .role(Role::Agent)
            .parts(vec![Part::Text {
                text: "Task failed: invalid input".to_string(),
                metadata: None,
            }])
            .message_id("msg-123".to_string())
            .build();

        let task = Task::builder()
            .id("task-123".to_string())
            .context_id("ctx-123".to_string())
            .status(TaskStatus {
                state: TaskState::Failed,
                message: Some(message),
                timestamp: Some(chrono::Utc::now()),
            })
            .build();

        let original_request = JsonRpcRequest::new("tools/call", None);
        let response = translator.task_result_to_response(&task, &original_request).unwrap();

        assert!(response.is_error());
        assert!(response.error.is_some());
        let error = response.error.unwrap();
        assert!(error.message.contains("invalid input"));
    }

    #[test]
    fn test_find_skill_by_tool_name() {
        let skill = AgentSkill::new(
            "my_skill".to_string(),
            "My Skill".to_string(),
            "A test skill".to_string(),
            vec![],
        );

        let card = create_agent_card_with_skill(skill);
        let mut translator = ProtocolTranslator::new(TranslatorConfig::new().with_tool_name_prefix("agent"));
        translator.set_agent_card(card);

        // Find with prefix
        let skill = translator.find_skill_by_tool_name("agent_my_skill").unwrap();
        assert!(skill.is_some());
        assert_eq!(skill.unwrap().id, "my_skill");

        // Find without prefix
        let skill = translator.find_skill_by_tool_name("my_skill").unwrap();
        assert!(skill.is_some());
    }

    #[test]
    fn test_input_schema_generation() {
        let skill = AgentSkill::new(
            "test_skill".to_string(),
            "Test Skill".to_string(),
            "A test skill".to_string(),
            vec!["test".to_string()],
        );

        let translator = ProtocolTranslator::default_config();
        let card = AgentCard::builder()
            .name("Test".to_string())
            .description("Test".to_string())
            .url("https://test.com".to_string())
            .version("1.0.0".to_string())
            .capabilities(AgentCapabilities::default())
            .default_input_modes(vec!["text".to_string()])
            .default_output_modes(vec!["text".to_string()])
            .skills(vec![])
            .build();

        let schema = translator.generate_input_schema(&skill, &card).unwrap();

        assert_eq!(schema["type"], "object");
        assert!(schema["properties"].is_object());
        assert!(schema["properties"]["prompt"].is_object());
        assert_eq!(schema["properties"]["prompt"]["type"], "string");
    }

    #[test]
    fn test_arguments_to_parts_text() {
        let translator = ProtocolTranslator::default_config();
        let arguments = serde_json::json!({"prompt": "Hello, world!"});

        let parts = translator.arguments_to_parts(&arguments, "test_skill").unwrap();

        assert_eq!(parts.len(), 1);
        match &parts[0] {
            Part::Text { text, .. } => assert_eq!(text, "Hello, world!"),
            _ => panic!("Expected Text part"),
        }
    }

    #[test]
    fn test_arguments_to_parts_data() {
        let translator = ProtocolTranslator::default_config();
        let arguments = serde_json::json!({
            "data": {"key": "value"}
        });

        let parts = translator.arguments_to_parts(&arguments, "test_skill").unwrap();

        assert_eq!(parts.len(), 1);
        match &parts[0] {
            Part::Data { data, .. } => assert_eq!(data.get("key").unwrap(), "value"),
            _ => panic!("Expected Data part"),
        }
    }

    #[test]
    fn test_tool_schema_generator() {
        let skill = AgentSkill::new(
            "gen_skill".to_string(),
            "Gen Skill".to_string(),
            "Generated skill".to_string(),
            vec![],
        );

        let tool = ToolSchemaGenerator::skill_to_tool_schema(&skill).unwrap();

        assert_eq!(tool.name, "gen_skill");
        assert_eq!(tool.description, "Generated skill");
        assert!(tool.input_schema.is_some());
    }

    #[test]
    fn test_extract_text_from_message() {
        let translator = ProtocolTranslator::default_config();

        let message = Message::builder()
            .role(Role::Agent)
            .parts(vec![
                Part::Text {
                    text: "Hello".to_string(),
                    metadata: None,
                },
                Part::Text {
                    text: "World".to_string(),
                    metadata: None,
                },
            ])
            .message_id("msg-1".to_string())
            .build();

        let text = translator.extract_text_from_message(&message).unwrap();
        assert_eq!(text, "Hello\nWorld");
    }
}
