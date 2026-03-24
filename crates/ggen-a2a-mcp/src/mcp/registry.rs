//! MCP tool registry for A2A skills
//!
//! This module provides a registry for mapping A2A (Agent-to-Agent) skills
//! to MCP (Model Context Protocol) tools. It handles the conversion between
//! A2A skill definitions and MCP tool definitions with proper JSON schema
//! generation.

use crate::error::{A2aMcpError, A2aMcpResult};
use serde::{Deserialize, Serialize};
use serde_json::json;
use std::collections::HashMap;
use std::sync::Arc;
use tokio::sync::RwLock;

/// MCP tool definition converted from A2A skill
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct McpToolDefinition {
    /// Tool name (must match MCP spec: ^[a-zA-Z0-9_]{1,64}$)
    pub name: String,

    /// Human-readable description
    pub description: String,

    /// JSON Schema for input validation
    pub input_schema: serde_json::Value,

    /// Whether the tool is deprecated
    #[serde(skip_serializing_if = "is_false")]
    pub deprecated: bool,
}

fn is_false(b: &bool) -> bool {
    !b
}

/// Metadata for mapping MCP tools back to A2A skills
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct A2aSkillMetadata {
    /// A2A agent ID that implements this skill
    pub agent_id: String,

    /// Skill/method name in the A2A protocol
    pub skill_name: String,

    /// Required execution mode
    pub execution_mode: ExecutionMode,
}

/// Execution mode for A2A skills
#[derive(Debug, Clone, Copy, Serialize, Deserialize, PartialEq, Eq)]
pub enum ExecutionMode {
    #[serde(rename = "sync")]
    Sync,
    #[serde(rename = "async")]
    Async,
    #[serde(rename = "stream")]
    Stream,
}

/// Trait for managing MCP tool registry
#[async_trait::async_trait]
pub trait McpToolRegistry: Send + Sync {
    /// Register an A2A skill as an MCP tool
    async fn register_skill(&mut self, skill: A2aSkill) -> Result<(), A2aMcpError>;

    /// Get tool definition by name
    async fn get_tool(&self, name: &str) -> Option<McpToolDefinition>;

    /// List all registered tools
    async fn list_tools(&self) -> Vec<McpToolDefinition>;
}

/// A2A skill definition
#[derive(Debug, Clone)]
pub struct A2aSkill {
    pub id: String,
    pub name: String,
    pub description: String,
    pub inputs: Vec<SkillParameter>,
    pub outputs: Vec<SkillParameter>,
}

/// Parameter definition for A2A skills
#[derive(Debug, Clone)]
pub struct SkillParameter {
    pub name: String,
    pub description: String,
    pub param_type: SkillParamType,
    pub required: bool,
}

/// Parameter type for A2A skills
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum SkillParamType {
    String,
    Number,
    Boolean,
    Object,
    Array,
}

/// In-memory MCP tool registry for A2A skills
///
/// This registry maintains a mapping between A2A skills and their MCP tool
/// representations. It provides thread-safe concurrent access for registration
/// and lookup operations.
pub struct InMemoryToolRegistry {
    /// Registered tools indexed by name
    tools: Arc<RwLock<HashMap<String, McpToolDefinition>>>,

    /// Mapping from skill ID to tool name
    skill_mapping: Arc<RwLock<HashMap<String, String>>>,

    /// Metadata for each tool (maps back to A2A skill)
    metadata: Arc<RwLock<HashMap<String, A2aSkillMetadata>>>,
}

impl InMemoryToolRegistry {
    /// Create a new in-memory tool registry
    pub fn new() -> Self {
        Self {
            tools: Arc::new(RwLock::new(HashMap::new())),
            skill_mapping: Arc::new(RwLock::new(HashMap::new())),
            metadata: Arc::new(RwLock::new(HashMap::new())),
        }
    }

    /// Convert skill to JSON schema for MCP tool
    fn skill_to_json_schema(&self, skill: &A2aSkill) -> A2aMcpResult<serde_json::Value> {
        let mut properties = serde_json::Map::new();
        let mut required = Vec::new();

        for param in &skill.inputs {
            let param_type = match param.param_type {
                SkillParamType::String => json!("string"),
                SkillParamType::Number => json!("number"),
                SkillParamType::Boolean => json!("boolean"),
                SkillParamType::Object => json!("object"),
                SkillParamType::Array => json!("array"),
            };

            let mut param_schema = json!({
                "type": param_type,
                "description": param.description
            });

            // Add additional constraints based on type
            if matches!(param.param_type, SkillParamType::Array) {
                // Default to string items for arrays
                param_schema["items"] = json!("string");
            }

            properties.insert(param.name.clone(), param_schema);

            if param.required {
                required.push(param.name.clone());
            }
        }

        Ok(json!({
            "type": "object",
            "properties": properties,
            "required": required
        }))
    }

    /// Sanitize tool name to match MCP spec
    ///
    /// MCP tool names must match: ^[a-zA-Z0-9_]{1,64}$
    fn sanitize_tool_name(&self, name: &str) -> String {
        name.chars()
            .map(|c| {
                if c.is_alphanumeric() || c == '_' {
                    c
                } else {
                    '_'
                }
            })
            .collect::<String>()
            .chars()
            .take(64)
            .collect()
    }

    /// Get metadata for a tool
    pub async fn get_metadata(&self, name: &str) -> Option<A2aSkillMetadata> {
        let metadata = self.metadata.read().await;
        metadata.get(name).cloned()
    }

    /// Get tool name by skill ID
    pub async fn get_tool_by_skill_id(&self, skill_id: &str) -> Option<String> {
        let mapping = self.skill_mapping.read().await;
        mapping.get(skill_id).cloned()
    }

    /// Register a skill with associated metadata
    pub async fn register_skill_with_metadata(
        &mut self, skill: A2aSkill, metadata: A2aSkillMetadata,
    ) -> Result<(), A2aMcpError> {
        let tool_name = self.sanitize_tool_name(&skill.name);
        let input_schema = self.skill_to_json_schema(&skill)?;

        let tool = McpToolDefinition {
            name: tool_name.clone(),
            description: skill.description.clone(),
            input_schema,
            deprecated: false,
        };

        let mut tools = self.tools.write().await;
        let mut mapping = self.skill_mapping.write().await;
        let mut meta = self.metadata.write().await;

        tools.insert(tool_name.clone(), tool);
        mapping.insert(skill.id.clone(), tool_name.clone());
        meta.insert(tool_name, metadata);

        Ok(())
    }
}

#[async_trait::async_trait]
impl McpToolRegistry for InMemoryToolRegistry {
    async fn register_skill(&mut self, skill: A2aSkill) -> Result<(), A2aMcpError> {
        let tool_name = self.sanitize_tool_name(&skill.name);
        let input_schema = self.skill_to_json_schema(&skill)?;

        let tool = McpToolDefinition {
            name: tool_name.clone(),
            description: skill.description.clone(),
            input_schema,
            deprecated: false,
        };

        let mut tools = self.tools.write().await;
        let mut mapping = self.skill_mapping.write().await;

        tools.insert(tool_name.clone(), tool);
        mapping.insert(skill.id, tool_name);

        Ok(())
    }

    async fn get_tool(&self, name: &str) -> Option<McpToolDefinition> {
        let tools = self.tools.read().await;
        tools.get(name).cloned()
    }

    async fn list_tools(&self) -> Vec<McpToolDefinition> {
        let tools = self.tools.read().await;
        tools.values().cloned().collect()
    }
}

impl Default for InMemoryToolRegistry {
    fn default() -> Self {
        Self::new()
    }
}

/// Type alias for convenience - use the struct directly to avoid confusion with the trait
pub type A2aSkillRegistry = InMemoryToolRegistry;

#[cfg(test)]
mod tests {
    use super::*;

    fn create_test_skill() -> A2aSkill {
        A2aSkill {
            id: "skill-1".to_string(),
            name: "search_files".to_string(),
            description: "Search for files in the project".to_string(),
            inputs: vec![
                SkillParameter {
                    name: "query".to_string(),
                    description: "Search query pattern".to_string(),
                    param_type: SkillParamType::String,
                    required: true,
                },
                SkillParameter {
                    name: "max_results".to_string(),
                    description: "Maximum number of results".to_string(),
                    param_type: SkillParamType::Number,
                    required: false,
                },
            ],
            outputs: vec![SkillParameter {
                name: "results".to_string(),
                description: "List of matching files".to_string(),
                param_type: SkillParamType::Array,
                required: true,
            }],
        }
    }

    #[test]
    fn test_sanitize_tool_name() {
        let registry = InMemoryToolRegistry::new();

        assert_eq!(registry.sanitize_tool_name("valid_name"), "valid_name");
        assert_eq!(
            registry.sanitize_tool_name("name-with-dashes"),
            "name_with_dashes"
        );
        assert_eq!(
            registry.sanitize_tool_name("name with spaces"),
            "name_with_spaces"
        );
        assert_eq!(
            registry.sanitize_tool_name("name@with!special"),
            "name_with_special"
        );

        // Test truncation to 64 chars
        let long_name = "a".repeat(100);
        assert_eq!(registry.sanitize_tool_name(&long_name).len(), 64);
    }

    #[test]
    fn test_skill_to_json_schema() {
        let registry = InMemoryToolRegistry::new();
        let skill = create_test_skill();

        let schema = registry.skill_to_json_schema(&skill).unwrap();

        assert_eq!(schema["type"], "object");
        assert!(schema["properties"].is_object());
        assert!(schema["properties"]["query"].is_object());
        assert_eq!(schema["properties"]["query"]["type"], "string");
        assert_eq!(schema["properties"]["max_results"]["type"], "number");

        let required = schema["required"].as_array().unwrap();
        assert_eq!(required.len(), 1);
        assert_eq!(required[0], "query");
    }

    #[tokio::test]
    async fn test_register_skill() {
        let mut registry = InMemoryToolRegistry::new();
        let skill = create_test_skill();

        registry.register_skill(skill).await.unwrap();

        let tools = registry.list_tools().await;
        assert_eq!(tools.len(), 1);
        assert_eq!(tools[0].name, "search_files");
    }

    #[tokio::test]
    async fn test_get_tool() {
        let mut registry = InMemoryToolRegistry::new();
        let skill = create_test_skill();

        registry.register_skill(skill).await.unwrap();

        let tool = registry.get_tool("search_files").await;
        assert!(tool.is_some());
        assert_eq!(tool.unwrap().description, "Search for files in the project");

        let missing = registry.get_tool("nonexistent").await;
        assert!(missing.is_none());
    }

    #[tokio::test]
    async fn test_get_tool_by_skill_id() {
        let mut registry = InMemoryToolRegistry::new();
        let skill = create_test_skill();

        registry.register_skill(skill).await.unwrap();

        let tool_name = registry.get_tool_by_skill_id("skill-1").await;
        assert_eq!(tool_name, Some("search_files".to_string()));

        let missing = registry.get_tool_by_skill_id("nonexistent").await;
        assert!(missing.is_none());
    }

    #[tokio::test]
    async fn test_register_skill_with_metadata() {
        let mut registry = InMemoryToolRegistry::new();
        let skill = create_test_skill();
        let metadata = A2aSkillMetadata {
            agent_id: "agent-1".to_string(),
            skill_name: "search_files_skill".to_string(),
            execution_mode: ExecutionMode::Async,
        };

        registry
            .register_skill_with_metadata(skill, metadata)
            .await
            .unwrap();

        let meta = registry.get_metadata("search_files").await;
        assert!(meta.is_some());
        let meta = meta.unwrap();
        assert_eq!(meta.agent_id, "agent-1");
        assert_eq!(meta.skill_name, "search_files_skill");
        assert_eq!(meta.execution_mode, ExecutionMode::Async);
    }

    #[tokio::test]
    async fn test_default_registry() {
        let registry = InMemoryToolRegistry::default();
        assert_eq!(registry.list_tools().await.len(), 0);
    }

    #[test]
    fn test_execution_mode_serialization() {
        let sync = ExecutionMode::Sync;
        let async_mode = ExecutionMode::Async;
        let stream = ExecutionMode::Stream;

        assert_eq!(serde_json::to_value(&sync).unwrap(), "sync");
        assert_eq!(serde_json::to_value(&async_mode).unwrap(), "async");
        assert_eq!(serde_json::to_value(&stream).unwrap(), "stream");
    }
}
