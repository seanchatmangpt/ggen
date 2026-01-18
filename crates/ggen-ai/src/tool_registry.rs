//! Tool Registry - Centralized discovery and management of agent tools
//!
//! Provides thread-safe registry for tools accessible by agents via multiple interfaces:
//! - Direct Rust API (crate::tool_registry::REGISTRY.read())
//! - HTTP endpoints (for GenAI agents)
//! - MCP tool definitions (for Model Context Protocol)
//! - JSON export (for tool discovery)
//!
//! # Thread Safety
//!
//! The registry uses `Arc<RwLock<>>` for thread-safe access:
//! - Multiple readers can access simultaneously
//! - Writers get exclusive access
//! - No deadlock risk if used correctly (always drop locks promptly)
//!
//! # Example
//!
//! ```ignore
//! use ggen_ai::tool_registry::{REGISTRY, Tool, ToolTag, Signature};
//! use ggen_ai::dspy::field::{InputField, OutputField};
//!
//! // Register a tool at startup
//! let sig = Signature::new("MyTool", "Does something")
//!     .with_input(InputField::new("text", "Input text", "String"))
//!     .with_output(OutputField::new("result", "Result", "String"));
//!
//! let tool = Tool::new("my_tool", "My Tool", "1.0.0", "Does something", sig)
//!     .with_tag(ToolTag::Generation);
//!
//! REGISTRY.register("my_tool", tool).ok();
//!
//! // Access registry
//! {
//!     let registry = REGISTRY.read().ok().unwrap();
//!     let tool = registry.get("my_tool").ok();
//! }
//! ```

use crate::error::{GgenAiError, Result};
use crate::tool::Tool;
use std::collections::HashMap;
use std::sync::{Arc, RwLock};

/// Global tool registry singleton
///
/// Thread-safe registry that can be accessed from multiple agents and threads.
/// Use `REGISTRY.register()` to add tools at startup or runtime.
pub static REGISTRY: once_cell::sync::Lazy<Arc<RwLock<ToolRegistry>>> =
    once_cell::sync::Lazy::new(|| Arc::new(RwLock::new(ToolRegistry::new())));

/// Centralized registry for tool discovery and management
///
/// Stores tools indexed by ID for O(1) lookup. Thread-safe via RwLock.
#[derive(Debug)]
pub struct ToolRegistry {
    /// Map of tool ID to Tool definition
    tools: HashMap<String, Tool>,
}

impl ToolRegistry {
    /// Create a new empty registry
    pub fn new() -> Self {
        Self {
            tools: HashMap::new(),
        }
    }

    /// Register a tool in the registry
    ///
    /// # Arguments
    /// * `id` - Unique tool identifier
    /// * `tool` - Tool definition to register
    ///
    /// # Errors
    /// Returns error if:
    /// - Tool ID already exists
    /// - Tool validation fails
    ///
    /// # Example
    /// ```ignore
    /// let tool = Tool::new("my_tool", "My Tool", "1.0.0", "Description", sig);
    /// REGISTRY.register("my_tool", tool)?;
    /// ```
    pub fn register(&mut self, id: &str, tool: Tool) -> Result<()> {
        // Validate tool first
        tool.validate()?;

        // Check if tool already exists
        if self.tools.contains_key(id) {
            return Err(GgenAiError::ValidationError {
                message: format!(
                    "Tool with ID '{}' already registered. Use update() to modify.",
                    id
                ),
            });
        }

        // Verify ID consistency
        if tool.id != id {
            return Err(GgenAiError::ValidationError {
                message: format!(
                    "Tool ID mismatch: registry key '{}' != tool.id '{}'",
                    id, tool.id
                ),
            });
        }

        self.tools.insert(id.to_string(), tool);
        Ok(())
    }

    /// Update an existing tool or insert if not present
    ///
    /// Unlike `register()`, this allows updating existing tools.
    /// Useful for hot-reloading tool configurations.
    pub fn upsert(&mut self, id: &str, tool: Tool) -> Result<()> {
        // Validate tool
        tool.validate()?;

        // Verify ID consistency
        if tool.id != id {
            return Err(GgenAiError::ValidationError {
                message: format!(
                    "Tool ID mismatch: registry key '{}' != tool.id '{}'",
                    id, tool.id
                ),
            });
        }

        self.tools.insert(id.to_string(), tool);
        Ok(())
    }

    /// Retrieve a tool by ID
    ///
    /// # Arguments
    /// * `id` - Tool identifier
    ///
    /// # Returns
    /// Reference to the Tool if found
    ///
    /// # Errors
    /// Returns error if tool not found
    pub fn get(&self, id: &str) -> Result<&Tool> {
        self.tools
            .get(id)
            .ok_or_else(|| GgenAiError::ValidationError {
                message: format!("Tool '{}' not found in registry", id),
            })
    }

    /// Check if tool is registered
    pub fn contains(&self, id: &str) -> bool {
        self.tools.contains_key(id)
    }

    /// List all registered tools with their metadata
    ///
    /// Returns a vector of (ID, Tool) tuples
    pub fn list(&self) -> Vec<(String, &Tool)> {
        self.tools
            .iter()
            .map(|(id, tool)| (id.clone(), tool))
            .collect()
    }

    /// Get all tools matching a specific tag
    ///
    /// # Arguments
    /// * `tag` - Tool tag to filter by
    ///
    /// # Returns
    /// Vector of tool IDs matching the tag
    pub fn list_by_tag(&self, tag: &crate::tool::ToolTag) -> Vec<&Tool> {
        self.tools
            .values()
            .filter(|tool| tool.tags.contains(tag))
            .collect()
    }

    /// Get all tools in a specific domain (via category tag)
    pub fn list_by_domain(&self, domain: &str) -> Vec<&Tool> {
        self.tools
            .values()
            .filter(|tool| tool.tags.iter().any(|tag| tag.to_string() == domain))
            .collect()
    }

    /// Validate a tool input against the registered tool's signature
    ///
    /// # Arguments
    /// * `tool_id` - ID of the tool
    /// * `input` - JSON input to validate
    ///
    /// # Returns
    /// Ok if input is valid, Err with validation message otherwise
    pub fn validate_input(&self, tool_id: &str, input: &serde_json::Value) -> Result<()> {
        let tool = self.get(tool_id)?;

        // Get the JSON schema for the tool's input
        let schema = tool.signature.as_json_schema();

        // Basic validation: check required fields
        if let Some(required_fields) = schema["required"].as_array() {
            for required_field in required_fields {
                if let Some(field_name) = required_field.as_str() {
                    if !input.get(field_name).is_some() {
                        return Err(GgenAiError::ValidationError {
                            message: format!(
                                "Tool '{}' requires input field '{}'",
                                tool_id, field_name
                            ),
                        });
                    }
                }
            }
        }

        Ok(())
    }

    /// Export all tools as OpenAPI-compatible tool definitions
    ///
    /// Returns a JSON array suitable for OpenAI function calls
    pub fn export_openapi(&self) -> serde_json::Value {
        let tools: Vec<_> = self.tools.values().map(|tool| tool.to_openapi()).collect();

        serde_json::json!(tools)
    }

    /// Export all tools as MCP tool definitions
    ///
    /// Returns a JSON array suitable for Model Context Protocol
    pub fn export_mcp_tools(&self) -> serde_json::Value {
        let tools: Vec<_> = self.tools.values().map(|tool| tool.to_mcp_tool()).collect();

        serde_json::json!(tools)
    }

    /// Export all tools as JSON with full metadata
    pub fn export_json(&self) -> Result<String> {
        serde_json::to_string_pretty(&self.tools).map_err(|e| GgenAiError::Json(e))
    }

    /// Get count of registered tools
    pub fn count(&self) -> usize {
        self.tools.len()
    }

    /// Clear all tools from registry (for testing)
    #[cfg(test)]
    pub fn clear(&mut self) {
        self.tools.clear();
    }

    /// Get all tool IDs
    pub fn tool_ids(&self) -> Vec<String> {
        self.tools.keys().cloned().collect()
    }
}

impl Default for ToolRegistry {
    fn default() -> Self {
        Self::new()
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::dspy::field::{InputField, OutputField};
    use crate::tool::{AuthScope, ToolSlo, ToolTag};

    fn create_test_tool(id: &str) -> Tool {
        let sig = crate::dspy::Signature::new("TestTool", "Test tool")
            .with_input(InputField::new("input", "Input text", "String"))
            .with_output(OutputField::new("output", "Output text", "String"));

        Tool::new(id, "Test Tool", "1.0.0", "A test tool", sig)
    }

    #[test]
    fn test_registry_creation() {
        let registry = ToolRegistry::new();
        assert_eq!(registry.count(), 0);
    }

    #[test]
    fn test_register_tool() {
        let mut registry = ToolRegistry::new();
        let tool = create_test_tool("test_tool");

        assert!(registry.register("test_tool", tool).is_ok());
        assert_eq!(registry.count(), 1);
    }

    #[test]
    fn test_register_duplicate_tool() {
        let mut registry = ToolRegistry::new();
        let tool1 = create_test_tool("test_tool");
        let tool2 = create_test_tool("test_tool");

        assert!(registry.register("test_tool", tool1).is_ok());
        assert!(registry.register("test_tool", tool2).is_err());
    }

    #[test]
    fn test_register_id_mismatch() {
        let mut registry = ToolRegistry::new();
        let tool = create_test_tool("tool_a");

        let result = registry.register("tool_b", tool);
        assert!(result.is_err());
    }

    #[test]
    fn test_get_existing_tool() {
        let mut registry = ToolRegistry::new();
        let tool = create_test_tool("test_tool");

        registry.register("test_tool", tool).ok();

        let retrieved = registry.get("test_tool");
        assert!(retrieved.is_ok());
        assert_eq!(retrieved.unwrap().name, "Test Tool");
    }

    #[test]
    fn test_get_nonexistent_tool() {
        let registry = ToolRegistry::new();
        let result = registry.get("nonexistent");

        assert!(result.is_err());
    }

    #[test]
    fn test_contains_tool() {
        let mut registry = ToolRegistry::new();
        let tool = create_test_tool("test_tool");

        registry.register("test_tool", tool).ok();

        assert!(registry.contains("test_tool"));
        assert!(!registry.contains("other_tool"));
    }

    #[test]
    fn test_list_tools() {
        let mut registry = ToolRegistry::new();

        for i in 1..=3 {
            let tool = create_test_tool(&format!("tool_{}", i));
            registry.register(&format!("tool_{}", i), tool).ok();
        }

        let tools = registry.list();
        assert_eq!(tools.len(), 3);
    }

    #[test]
    fn test_list_by_tag() {
        let mut registry = ToolRegistry::new();

        let tool1 = create_test_tool("tool_1")
            .with_tag(ToolTag::CodeGeneration)
            .with_tag(ToolTag::Financial);

        let tool2 = create_test_tool("tool_2").with_tag(ToolTag::Validation);

        let tool3 = create_test_tool("tool_3").with_tag(ToolTag::CodeGeneration);

        registry.register("tool_1", tool1).ok();
        registry.register("tool_2", tool2).ok();
        registry.register("tool_3", tool3).ok();

        let codegen_tools = registry.list_by_tag(&ToolTag::CodeGeneration);
        assert_eq!(codegen_tools.len(), 2);

        let financial_tools = registry.list_by_tag(&ToolTag::Financial);
        assert_eq!(financial_tools.len(), 1);

        let validation_tools = registry.list_by_tag(&ToolTag::Validation);
        assert_eq!(validation_tools.len(), 1);
    }

    #[test]
    fn test_validate_input_with_required_fields() {
        let mut registry = ToolRegistry::new();
        let tool = create_test_tool("test_tool");

        registry.register("test_tool", tool).ok();

        let valid_input = serde_json::json!({ "input": "test" });
        assert!(registry.validate_input("test_tool", &valid_input).is_ok());

        let invalid_input = serde_json::json!({ "other_field": "test" });
        assert!(registry
            .validate_input("test_tool", &invalid_input)
            .is_err());
    }

    #[test]
    fn test_validate_input_nonexistent_tool() {
        let registry = ToolRegistry::new();
        let input = serde_json::json!({ "test": "value" });

        let result = registry.validate_input("nonexistent", &input);
        assert!(result.is_err());
    }

    #[test]
    fn test_upsert_new_tool() {
        let mut registry = ToolRegistry::new();
        let tool = create_test_tool("test_tool");

        assert!(registry.upsert("test_tool", tool).is_ok());
        assert_eq!(registry.count(), 1);
    }

    #[test]
    fn test_upsert_existing_tool() {
        let mut registry = ToolRegistry::new();
        let tool1 = create_test_tool("test_tool");
        let tool2 = create_test_tool("test_tool").with_author("Updated Author");

        registry.register("test_tool", tool1).ok();
        assert_eq!(registry.get("test_tool").unwrap().author, None);

        registry.upsert("test_tool", tool2).ok();
        assert_eq!(
            registry.get("test_tool").unwrap().author,
            Some("Updated Author".to_string())
        );
    }

    #[test]
    fn test_export_openapi() {
        let mut registry = ToolRegistry::new();
        let tool = create_test_tool("test_tool");

        registry.register("test_tool", tool).ok();

        let openapi = registry.export_openapi();
        assert!(openapi.is_array());

        let tools_array = openapi.as_array().unwrap();
        assert_eq!(tools_array.len(), 1);
        assert_eq!(tools_array[0]["type"], "function");
    }

    #[test]
    fn test_export_mcp_tools() {
        let mut registry = ToolRegistry::new();
        let tool = create_test_tool("test_tool");

        registry.register("test_tool", tool).ok();

        let mcp = registry.export_mcp_tools();
        assert!(mcp.is_array());

        let tools_array = mcp.as_array().unwrap();
        assert_eq!(tools_array.len(), 1);
        assert_eq!(tools_array[0]["name"], "test_tool");
    }

    #[test]
    fn test_export_json() {
        let mut registry = ToolRegistry::new();
        let tool = create_test_tool("test_tool");

        registry.register("test_tool", tool).ok();

        let json_str = registry.export_json();
        assert!(json_str.is_ok());

        let json_str = json_str.unwrap();
        let parsed: serde_json::Value = serde_json::from_str(&json_str).unwrap();

        assert!(parsed.is_object());
        assert!(parsed.get("test_tool").is_some());
    }

    #[test]
    fn test_count() {
        let mut registry = ToolRegistry::new();

        assert_eq!(registry.count(), 0);

        for i in 1..=5 {
            let tool = create_test_tool(&format!("tool_{}", i));
            registry.register(&format!("tool_{}", i), tool).ok();
        }

        assert_eq!(registry.count(), 5);
    }

    #[test]
    fn test_tool_ids() {
        let mut registry = ToolRegistry::new();

        registry.register("tool_1", create_test_tool("tool_1")).ok();
        registry.register("tool_2", create_test_tool("tool_2")).ok();
        registry.register("tool_3", create_test_tool("tool_3")).ok();

        let ids = registry.tool_ids();
        assert_eq!(ids.len(), 3);
        assert!(ids.contains(&"tool_1".to_string()));
        assert!(ids.contains(&"tool_2".to_string()));
        assert!(ids.contains(&"tool_3".to_string()));
    }

    #[test]
    fn test_register_invalid_tool() {
        let mut registry = ToolRegistry::new();

        // Create a tool with missing output
        let sig = crate::dspy::Signature::new("InvalidTool", "Invalid tool")
            .with_input(InputField::new("input", "Input", "String"));

        let tool = Tool::new("invalid_tool", "Invalid Tool", "1.0.0", "Invalid", sig);

        let result = registry.register("invalid_tool", tool);
        assert!(result.is_err());
    }

    #[test]
    fn test_registry_with_multiple_domains() {
        let mut registry = ToolRegistry::new();

        let financial_tool = create_test_tool("financial_tool")
            .with_tag(ToolTag::Financial)
            .with_tag(ToolTag::Generation);

        let banking_tool = create_test_tool("banking_tool")
            .with_tag(ToolTag::Banking)
            .with_tag(ToolTag::Validation);

        let insurance_tool = create_test_tool("insurance_tool")
            .with_tag(ToolTag::Insurance)
            .with_tag(ToolTag::Analysis);

        registry.register("financial_tool", financial_tool).ok();
        registry.register("banking_tool", banking_tool).ok();
        registry.register("insurance_tool", insurance_tool).ok();

        let financial = registry.list_by_tag(&ToolTag::Financial);
        assert_eq!(financial.len(), 1);

        let banking = registry.list_by_tag(&ToolTag::Banking);
        assert_eq!(banking.len(), 1);

        let insurance = registry.list_by_tag(&ToolTag::Insurance);
        assert_eq!(insurance.len(), 1);

        assert_eq!(registry.count(), 3);
    }

    #[test]
    fn test_list_domain() {
        let mut registry = ToolRegistry::new();

        let tool1 = create_test_tool("tool_1").with_tag(ToolTag::Financial);

        let tool2 = create_test_tool("tool_2").with_tag(ToolTag::Financial);

        let tool3 = create_test_tool("tool_3").with_tag(ToolTag::Banking);

        registry.register("tool_1", tool1).ok();
        registry.register("tool_2", tool2).ok();
        registry.register("tool_3", tool3).ok();

        let financial_domain = registry.list_by_domain("financial");
        assert_eq!(financial_domain.len(), 2);

        let banking_domain = registry.list_by_domain("banking");
        assert_eq!(banking_domain.len(), 1);
    }

    #[test]
    fn test_tool_with_slo_requirements() {
        let mut registry = ToolRegistry::new();

        let slo = ToolSlo {
            timeout_ms: 5_000,
            max_retries: 2,
            cacheable: true,
        };

        let tool = create_test_tool("slo_tool").with_slo(slo);

        registry.register("slo_tool", tool).ok();

        let retrieved = registry.get("slo_tool").unwrap();
        assert_eq!(retrieved.slo.timeout_ms, 5_000);
        assert_eq!(retrieved.slo.max_retries, 2);
        assert!(retrieved.slo.cacheable);
    }

    #[test]
    fn test_tool_with_auth_scope() {
        let mut registry = ToolRegistry::new();

        let tool = create_test_tool("admin_tool").with_auth_scope(AuthScope::Admin);

        registry.register("admin_tool", tool).ok();

        let retrieved = registry.get("admin_tool").unwrap();
        assert_eq!(retrieved.auth_scope, AuthScope::Admin);
    }

    #[test]
    fn test_registry_thread_safety_via_global() {
        // Reset the global registry for this test
        {
            let mut reg = REGISTRY.write().unwrap();
            reg.clear();
        }

        let tool = create_test_tool("thread_test_tool");

        {
            let mut reg = REGISTRY.write().unwrap();
            reg.register("thread_test_tool", tool).ok();
        }

        {
            let reg = REGISTRY.read().unwrap();
            assert!(reg.contains("thread_test_tool"));
            assert_eq!(reg.count(), 1);
        }
    }

    #[test]
    fn test_global_registry_list() {
        {
            let mut reg = REGISTRY.write().unwrap();
            reg.clear();
        }

        let tool1 = create_test_tool("global_tool_1");
        let tool2 = create_test_tool("global_tool_2");

        {
            let mut reg = REGISTRY.write().unwrap();
            reg.register("global_tool_1", tool1).ok();
            reg.register("global_tool_2", tool2).ok();
        }

        {
            let reg = REGISTRY.read().unwrap();
            let list = reg.list();
            assert_eq!(list.len(), 2);
        }
    }

    #[test]
    fn test_global_registry_export() {
        {
            let mut reg = REGISTRY.write().unwrap();
            reg.clear();
        }

        let tool = create_test_tool("export_tool");

        {
            let mut reg = REGISTRY.write().unwrap();
            reg.register("export_tool", tool).ok();
        }

        {
            let reg = REGISTRY.read().unwrap();
            let openapi = reg.export_openapi();
            assert!(openapi.is_array());
        }
    }
}
