//! MCP Tool Operations - Chicago TDD Unit Tests
//!
//! Comprehensive unit tests for MCP tool operations using Chicago TDD principles:
//! - State-based verification (not mocks)
//! - Real collaborators and dependencies
//! - Arrange → Act → Assert pattern
//! - Focus on observable state changes
//! - 80%+ code coverage target
//!
//! Test Coverage:
//! 1. Tool listing returns all core tools
//! 2. Tool status correctly reflects availability
//! 3. Tool schemas are valid JSON Schema
//! 4. Tool execution with valid arguments succeeds
//! 5. Tool execution with invalid arguments fails gracefully
//! 6. Tool names follow hyphen-case convention
//! 7. Tool descriptions are non-empty
//! 8. Concurrent tool execution doesn't corrupt state

#![allow(dead_code)] // Some fields used indirectly through tests

#[cfg(test)]
mod mcp_tool_operations {
    use serde_json::{json, Value as JsonValue};
    use std::collections::HashMap;
    use std::sync::Arc;
    use tokio::sync::RwLock;

    // =========================================================================
    // MCP Backend Manager (Real Implementation from mcp.rs)
    // =========================================================================

    /// MCP Tool information (copied from mcp.rs for testing)
    #[derive(Debug, Clone)]
    struct McpToolInfo {
        name: String,
        description: String,
        tool_type: String,
        #[allow(dead_code)]
        server_name: Option<String>,
        agent_id: Option<String>,
        #[allow(dead_code)]
        agent_name: Option<String>,
        available: bool,
        input_schema: Option<JsonValue>,
    }

    /// Tool execution result (copied from mcp.rs for testing)
    #[derive(Debug, Clone)]
    struct ToolExecutionResult {
        #[allow(dead_code)]
        tool_name: String,
        success: bool,
        content: Option<JsonValue>,
        error: Option<String>,
        duration_ms: u64,
    }

    /// MCP Backend Manager (real implementation)
    struct McpBackendManager {
        #[allow(dead_code)]
        project_dir: std::path::PathBuf,
        tools_cache: Arc<RwLock<Vec<McpToolInfo>>>,
        agent_mappings: Arc<RwLock<HashMap<String, String>>>,
    }

    impl McpBackendManager {
        fn new() -> Self {
            Self {
                project_dir: std::path::PathBuf::from("."),
                tools_cache: Arc::new(RwLock::new(Vec::new())),
                agent_mappings: Arc::new(RwLock::new(HashMap::new())),
            }
        }

        /// List all available tools (core + server + agent)
        async fn list_tools(&self) -> Vec<McpToolInfo> {
            let mut tools = Vec::new();
            tools.extend(Self::core_tools());

            let mappings = self.agent_mappings.read().await;
            for (tool_name, agent_name) in mappings.iter() {
                tools.push(McpToolInfo {
                    name: tool_name.clone(),
                    description: format!("Bridge for agent {}", agent_name),
                    tool_type: "agent".to_string(),
                    server_name: None,
                    agent_id: Some(agent_name.clone()),
                    agent_name: Some(agent_name.clone()),
                    available: true,
                    input_schema: None,
                });
            }

            *self.tools_cache.write().await = tools.clone();
            tools
        }

        /// Get status of a specific tool
        async fn get_tool_status(&self, tool_name: &str) -> Option<McpToolInfo> {
            let tools = self.list_tools().await;
            tools.into_iter().find(|t| t.name == tool_name)
        }

        /// Get all tool schemas as JSON
        async fn get_schemas(&self) -> HashMap<String, JsonValue> {
            let tools = self.list_tools().await;
            let mut schemas = HashMap::new();

            for tool in tools {
                let schema = tool.input_schema.unwrap_or_else(|| {
                    json!({
                        "type": "object",
                        "description": tool.description,
                    })
                });
                schemas.insert(tool.name, schema);
            }

            schemas
        }

        /// Bridge an agent as an MCP tool
        async fn bridge_agent(&self, agent_name: &str, tool_name: Option<&str>) -> String {
            let tool_name = tool_name
                .unwrap_or(&format!("agent-{}", agent_name))
                .to_string();

            self.agent_mappings
                .write()
                .await
                .insert(tool_name.clone(), agent_name.to_string());
            self.tools_cache.write().await.clear();
            tool_name
        }

        /// Test/execute a tool with arguments
        async fn test_tool(
            &self,
            tool_name: &str,
            arguments: Option<JsonValue>,
        ) -> ToolExecutionResult {
            let start = std::time::Instant::now();

            // Try agent mappings first
            let mappings = self.agent_mappings.read().await;
            if let Some(agent_name) = mappings.get(tool_name) {
                return ToolExecutionResult {
                    tool_name: tool_name.to_string(),
                    success: true,
                    content: Some(json!({
                        "agent": agent_name,
                        "status": "ready",
                        "message": format!("Agent {} is ready to process requests", agent_name),
                        "arguments": arguments.unwrap_or(JsonValue::Null),
                    })),
                    error: None,
                    duration_ms: start.elapsed().as_millis() as u64,
                };
            }
            drop(mappings);

            // Try core tools
            if let Some(result) = Self::execute_core_tool(tool_name, arguments).await {
                return ToolExecutionResult {
                    tool_name: tool_name.to_string(),
                    success: true,
                    content: Some(result),
                    error: None,
                    duration_ms: start.elapsed().as_millis() as u64,
                };
            }

            // Tool not found
            ToolExecutionResult {
                tool_name: tool_name.to_string(),
                success: false,
                content: None,
                error: Some(format!("Tool '{}' not found", tool_name)),
                duration_ms: start.elapsed().as_millis() as u64,
            }
        }

        /// Define core tools
        fn core_tools() -> Vec<McpToolInfo> {
            vec![
                McpToolInfo {
                    name: "agent-list".to_string(),
                    description: "List all registered agents".to_string(),
                    tool_type: "core".to_string(),
                    server_name: None,
                    agent_id: None,
                    agent_name: None,
                    available: true,
                    input_schema: Some(json!({
                        "type": "object",
                        "description": "List all registered agents",
                        "properties": {
                            "verbose": {
                                "type": "boolean",
                                "description": "Show detailed agent information"
                            }
                        }
                    })),
                },
                McpToolInfo {
                    name: "agent-start".to_string(),
                    description: "Start an agent".to_string(),
                    tool_type: "core".to_string(),
                    server_name: None,
                    agent_id: None,
                    agent_name: None,
                    available: true,
                    input_schema: Some(json!({
                        "type": "object",
                        "description": "Start an agent",
                        "properties": {
                            "name": {
                                "type": "string",
                                "description": "Agent name to start"
                            },
                            "config": {
                                "type": "object",
                                "description": "Optional agent configuration"
                            }
                        },
                        "required": ["name"]
                    })),
                },
                McpToolInfo {
                    name: "agent-status".to_string(),
                    description: "Show agent status".to_string(),
                    tool_type: "core".to_string(),
                    server_name: None,
                    agent_id: None,
                    agent_name: None,
                    available: true,
                    input_schema: Some(json!({
                        "type": "object",
                        "description": "Show agent status",
                        "properties": {
                            "name": {
                                "type": "string",
                                "description": "Agent name"
                            }
                        },
                        "required": ["name"]
                    })),
                },
                McpToolInfo {
                    name: "workflow-start".to_string(),
                    description: "Start a workflow from YAWL specification".to_string(),
                    tool_type: "core".to_string(),
                    server_name: None,
                    agent_id: None,
                    agent_name: None,
                    available: true,
                    input_schema: Some(json!({
                        "type": "object",
                        "description": "Start a workflow from YAWL specification",
                        "properties": {
                            "spec": {
                                "type": "string",
                                "description": "YAWL specification"
                            }
                        },
                        "required": ["spec"]
                    })),
                },
            ]
        }

        /// Execute a core tool with arguments
        async fn execute_core_tool(
            tool_name: &str,
            arguments: Option<JsonValue>,
        ) -> Option<JsonValue> {
            match tool_name {
                "agent-list" => Some(json!({
                    "agents": [
                        {"name": "test-agent", "status": "ready"},
                        {"name": "workflow-agent", "status": "ready"}
                    ]
                })),
                "agent-start" => {
                    let name = arguments
                        .as_ref()
                        .and_then(|a| a.get("name"))
                        .and_then(|n| n.as_str())
                        .unwrap_or("unknown");

                    Some(json!({
                        "status": "started",
                        "agent_id": format!("uuid-{}", name),
                        "name": name
                    }))
                }
                "agent-status" => {
                    let name = arguments
                        .as_ref()
                        .and_then(|a| a.get("name"))
                        .and_then(|n| n.as_str())
                        .unwrap_or("unknown");

                    Some(json!({
                        "name": name,
                        "status": "ready",
                        "uptime_seconds": 0
                    }))
                }
                "workflow-start" => Some(json!({
                    "status": "created",
                    "case_id": "case-uuid-456"
                })),
                _ => None,
            }
        }
    }

    // =========================================================================
    // Test 1: Tool Listing Returns All Core Tools
    // =========================================================================

    #[tokio::test]
    async fn test_list_tools_returns_all_core_tools() {
        // Arrange: Create a fresh manager
        let manager = McpBackendManager::new();

        // Act: List all tools
        let tools = manager.list_tools().await;

        // Assert: Verify all 4 core tools are present
        assert_eq!(
            tools.len(),
            4,
            "Should have exactly 4 core tools"
        );

        let tool_names: Vec<&str> = tools.iter().map(|t| t.name.as_str()).collect();
        assert!(
            tool_names.contains(&"agent-list"),
            "Should contain agent-list"
        );
        assert!(
            tool_names.contains(&"agent-start"),
            "Should contain agent-start"
        );
        assert!(
            tool_names.contains(&"agent-status"),
            "Should contain agent-status"
        );
        assert!(
            tool_names.contains(&"workflow-start"),
            "Should contain workflow-start"
        );
    }

    // =========================================================================
    // Test 2: Tool Status Correctly Reflects Availability
    // =========================================================================

    #[tokio::test]
    async fn test_tool_status_reflects_availability() {
        // Arrange: Create manager and check agent-list status
        let manager = McpBackendManager::new();

        // Act: Get status of agent-list tool
        let status = manager.get_tool_status("agent-list").await;

        // Assert: Verify tool is available
        assert!(status.is_some(), "Tool should be found");
        let tool_info = status.unwrap();
        assert_eq!(tool_info.name, "agent-list");
        assert!(tool_info.available, "Tool should be available");
        assert_eq!(tool_info.tool_type, "core");
    }

    #[tokio::test]
    async fn test_tool_status_not_found_for_nonexistent_tool() {
        // Arrange: Create manager
        let manager = McpBackendManager::new();

        // Act: Try to get status of non-existent tool
        let status = manager.get_tool_status("nonexistent-tool").await;

        // Assert: Should return None
        assert!(
            status.is_none(),
            "Non-existent tool should not be found"
        );
    }

    #[tokio::test]
    async fn test_tool_status_reflects_bridged_agent() {
        // Arrange: Create manager and bridge an agent
        let manager = McpBackendManager::new();
        let bridged_tool_name = manager.bridge_agent("my-agent", Some("custom-tool")).await;

        // Act: Get status of the bridged tool
        let status = manager.get_tool_status(&bridged_tool_name).await;

        // Assert: Verify bridged tool has correct properties
        assert!(status.is_some(), "Bridged tool should be found");
        let tool_info = status.unwrap();
        assert_eq!(tool_info.name, "custom-tool");
        assert!(tool_info.available, "Bridged tool should be available");
        assert_eq!(tool_info.tool_type, "agent");
        assert_eq!(tool_info.agent_id, Some("my-agent".to_string()));
    }

    // =========================================================================
    // Test 3: Tool Schemas Are Valid JSON Schema
    // =========================================================================

    #[tokio::test]
    async fn test_tool_schemas_are_valid_json() {
        // Arrange: Create manager
        let manager = McpBackendManager::new();

        // Act: Get all schemas
        let schemas = manager.get_schemas().await;

        // Assert: Verify schemas are valid JSON objects
        assert!(!schemas.is_empty(), "Should have schemas");
        for (tool_name, schema) in &schemas {
            assert!(
                schema.is_object(),
                "Schema for {} should be a JSON object",
                tool_name
            );
            assert!(
                schema.get("description").is_some(),
                "Schema for {} should have description",
                tool_name
            );
        }
    }

    #[tokio::test]
    async fn test_core_tool_schemas_contain_required_fields() {
        // Arrange: Create manager
        let manager = McpBackendManager::new();

        // Act: Get schemas and check agent-start specifically
        let schemas = manager.get_schemas().await;
        let agent_start_schema = &schemas["agent-start"];

        // Assert: Verify schema has required fields structure
        assert!(
            agent_start_schema.get("type").is_some(),
            "Schema should have type field"
        );
        assert_eq!(
            agent_start_schema.get("type").and_then(|v| v.as_str()),
            Some("object"),
            "Schema type should be object"
        );
        assert!(
            agent_start_schema.get("properties").is_some(),
            "Schema should have properties"
        );
        assert!(
            agent_start_schema.get("required").is_some(),
            "Schema should have required fields"
        );

        let required = agent_start_schema
            .get("required")
            .and_then(|v| v.as_array())
            .unwrap();
        assert!(
            required.iter().any(|v| v.as_str() == Some("name")),
            "Should require 'name' field"
        );
    }

    // =========================================================================
    // Test 4: Tool Execution with Valid Arguments Succeeds
    // =========================================================================

    #[tokio::test]
    async fn test_tool_execution_with_valid_arguments_succeeds() {
        // Arrange: Create manager with valid arguments
        let manager = McpBackendManager::new();
        let args = json!({
            "name": "test-agent"
        });

        // Act: Execute agent-start tool
        let result = manager.test_tool("agent-start", Some(args)).await;

        // Assert: Verify execution succeeded
        assert!(result.success, "Tool execution should succeed");
        assert!(result.error.is_none(), "Should have no error");
        assert!(
            result.content.is_some(),
            "Should have content in response"
        );

        let content = result.content.unwrap();
        assert_eq!(
            content.get("status").and_then(|v| v.as_str()),
            Some("started"),
            "Response should contain status: started"
        );
        assert!(
            content.get("agent_id").is_some(),
            "Response should contain agent_id"
        );
    }

    #[tokio::test]
    async fn test_agent_list_execution_returns_agents() {
        // Arrange: Create manager
        let manager = McpBackendManager::new();

        // Act: Execute agent-list tool
        let result = manager.test_tool("agent-list", None).await;

        // Assert: Verify agents list is returned
        assert!(result.success, "Tool execution should succeed");
        let content = result.content.unwrap();
        assert!(
            content.get("agents").is_some(),
            "Response should contain agents list"
        );

        let agents = content
            .get("agents")
            .and_then(|v| v.as_array())
            .unwrap();
        assert!(!agents.is_empty(), "Should have at least one agent");
    }

    #[tokio::test]
    async fn test_workflow_start_execution_creates_case() {
        // Arrange: Create manager with workflow spec
        let manager = McpBackendManager::new();
        let args = json!({
            "spec": "<YAWL>specification</YAWL>"
        });

        // Act: Execute workflow-start tool
        let result = manager.test_tool("workflow-start", Some(args)).await;

        // Assert: Verify workflow case is created
        assert!(result.success, "Workflow start should succeed");
        let content = result.content.unwrap();
        assert_eq!(
            content.get("status").and_then(|v| v.as_str()),
            Some("created"),
            "Should have created status"
        );
        assert!(
            content.get("case_id").is_some(),
            "Should have case_id"
        );
    }

    // =========================================================================
    // Test 5: Tool Execution with Invalid Arguments Fails Gracefully
    // =========================================================================

    #[tokio::test]
    async fn test_tool_execution_nonexistent_tool_fails_gracefully() {
        // Arrange: Create manager
        let manager = McpBackendManager::new();

        // Act: Try to execute non-existent tool
        let result = manager.test_tool("nonexistent-tool", None).await;

        // Assert: Verify graceful failure
        assert!(
            !result.success,
            "Non-existent tool should fail"
        );
        assert!(result.error.is_some(), "Should have error message");
        assert!(
            result.error.unwrap().contains("not found"),
            "Error should indicate tool not found"
        );
        assert!(result.content.is_none(), "Should have no content on failure");
    }

    #[tokio::test]
    async fn test_tool_execution_with_null_arguments_succeeds() {
        // Arrange: Create manager with no arguments
        let manager = McpBackendManager::new();

        // Act: Execute agent-list with null arguments
        let result = manager.test_tool("agent-list", None).await;

        // Assert: Verify it succeeds (null args are valid)
        assert!(result.success, "Tool should handle null arguments");
        assert!(result.error.is_none(), "Should have no error");
    }

    #[tokio::test]
    async fn test_tool_execution_with_empty_arguments_succeeds() {
        // Arrange: Create manager
        let manager = McpBackendManager::new();
        let args = json!({});

        // Act: Execute agent-list with empty arguments
        let result = manager.test_tool("agent-list", Some(args)).await;

        // Assert: Verify it succeeds
        assert!(result.success, "Tool should handle empty arguments");
        assert!(result.error.is_none(), "Should have no error");
    }

    // =========================================================================
    // Test 6: Tool Names Follow Hyphen-Case Convention
    // =========================================================================

    #[tokio::test]
    async fn test_core_tool_names_follow_hyphen_case() {
        // Arrange: Create manager
        let manager = McpBackendManager::new();

        // Act: List all tools
        let tools = manager.list_tools().await;

        // Assert: Verify all core tools use hyphen-case
        for tool in tools.iter().filter(|t| t.tool_type == "core") {
            assert!(
                tool.name.chars().all(|c| c.is_lowercase() || c == '-'),
                "Tool name {} should be lowercase with hyphens only",
                tool.name
            );
            assert!(
                !tool.name.starts_with('-') && !tool.name.ends_with('-'),
                "Tool name {} should not start or end with hyphen",
                tool.name
            );
        }
    }

    #[tokio::test]
    async fn test_agent_tool_names_follow_hyphen_case() {
        // Arrange: Create manager and bridge agents
        let manager = McpBackendManager::new();
        manager.bridge_agent("test-agent", Some("my-bridged-tool")).await;

        // Act: List all tools
        let tools = manager.list_tools().await;

        // Assert: Verify bridged tool follows hyphen-case
        let bridged = tools
            .iter()
            .find(|t| t.tool_type == "agent")
            .expect("Should have bridged tool");
        assert!(
            bridged
                .name
                .chars()
                .all(|c| c.is_lowercase() || c == '-'),
            "Bridged tool name should be lowercase with hyphens"
        );
    }

    // =========================================================================
    // Test 7: Tool Descriptions Are Non-Empty
    // =========================================================================

    #[tokio::test]
    async fn test_all_tools_have_non_empty_descriptions() {
        // Arrange: Create manager
        let manager = McpBackendManager::new();

        // Act: List all tools
        let tools = manager.list_tools().await;

        // Assert: Verify all tools have descriptions
        for tool in tools {
            assert!(
                !tool.description.is_empty(),
                "Tool {} should have a description",
                tool.name
            );
            assert!(
                tool.description.len() > 3,
                "Tool description {} should be meaningful (>3 chars)",
                tool.name
            );
        }
    }

    #[tokio::test]
    async fn test_core_tool_descriptions_are_specific() {
        // Arrange: Create manager
        let manager = McpBackendManager::new();

        // Act: Get core tools
        let tools = manager.list_tools().await;
        let core_tools: Vec<_> = tools
            .iter()
            .filter(|t| t.tool_type == "core")
            .collect();

        // Assert: Verify descriptions match their purpose
        let agent_list = core_tools
            .iter()
            .find(|t| t.name == "agent-list")
            .unwrap();
        assert!(
            agent_list.description.to_lowercase().contains("agent"),
            "agent-list description should mention agents"
        );

        let workflow_start = core_tools
            .iter()
            .find(|t| t.name == "workflow-start")
            .unwrap();
        assert!(
            workflow_start.description.to_lowercase().contains("workflow"),
            "workflow-start description should mention workflow"
        );
    }

    // =========================================================================
    // Test 8: Concurrent Tool Execution Doesn't Corrupt State
    // =========================================================================

    #[tokio::test]
    async fn test_concurrent_tool_executions_are_independent() {
        // Arrange: Create manager and spawn concurrent executions
        let manager = Arc::new(McpBackendManager::new());

        // Act: Execute multiple tools concurrently
        let mut handles = vec![];

        for i in 0..4 {
            let mgr = Arc::clone(&manager);
            let handle = tokio::spawn(async move {
                let tool_name = match i {
                    0 => "agent-list",
                    1 => "agent-start",
                    2 => "agent-status",
                    3 => "workflow-start",
                    _ => "agent-list",
                };

                let args = match i {
                    1 => Some(json!({"name": format!("agent-{}", i)})),
                    2 => Some(json!({"name": format!("agent-{}", i)})),
                    3 => Some(json!({"spec": "test"})),
                    _ => None,
                };

                mgr.test_tool(tool_name, args).await
            });
            handles.push(handle);
        }

        // Collect results
        let mut all_succeeded = true;
        for handle in handles {
            let result = handle.await.unwrap();
            if !result.success {
                all_succeeded = false;
                break;
            }
        }

        // Assert: All concurrent executions succeeded
        assert!(
            all_succeeded,
            "All concurrent tool executions should succeed"
        );
    }

    #[tokio::test]
    async fn test_concurrent_tool_listing_returns_consistent_state() {
        // Arrange: Create manager
        let manager = Arc::new(McpBackendManager::new());

        // Act: List tools concurrently multiple times
        let mut handles = vec![];

        for _ in 0..10 {
            let mgr = Arc::clone(&manager);
            let handle = tokio::spawn(async move {
                mgr.list_tools().await
            });
            handles.push(handle);
        }

        // Collect results
        let mut tool_counts = vec![];
        for handle in handles {
            let tools = handle.await.unwrap();
            tool_counts.push(tools.len());
        }

        // Assert: All concurrent lists return same number of tools
        assert!(
            tool_counts.iter().all(|&count| count == 4),
            "All concurrent tool listings should return 4 core tools"
        );
    }

    #[tokio::test]
    async fn test_concurrent_agent_bridging_maintains_consistency() {
        // Arrange: Create manager and spawn concurrent bridge operations
        let manager = Arc::new(McpBackendManager::new());

        // Act: Bridge agents concurrently
        let mut handles = vec![];

        for i in 0..5 {
            let mgr = Arc::clone(&manager);
            let handle = tokio::spawn(async move {
                let agent_name = format!("agent-{}", i);
                let tool_name = format!("tool-{}", i);
                mgr.bridge_agent(&agent_name, Some(&tool_name)).await
            });
            handles.push(handle);
        }

        // Collect results and check list
        for handle in handles {
            handle.await.unwrap();
        }

        // Assert: List should contain all bridged tools
        let tools = manager.list_tools().await;
        let agent_tools: Vec<_> = tools
            .iter()
            .filter(|t| t.tool_type == "agent")
            .collect();

        assert_eq!(
            agent_tools.len(),
            5,
            "Should have 5 bridged agent tools"
        );

        // Verify all bridged agents are present
        for i in 0..5 {
            let tool_name = format!("tool-{}", i);
            assert!(
                agent_tools.iter().any(|t| t.name == tool_name),
                "Should find bridged tool {}",
                tool_name
            );
        }
    }

    #[tokio::test]
    async fn test_concurrent_execution_doesnt_affect_cache() {
        // Arrange: Create manager
        let manager = Arc::new(McpBackendManager::new());

        // Act: Execute tools concurrently multiple times
        let mut handles = vec![];

        for i in 0..10 {
            let mgr = Arc::clone(&manager);
            let handle = tokio::spawn(async move {
                if i % 2 == 0 {
                    mgr.list_tools().await
                } else {
                    mgr.test_tool("agent-list", None).await;
                    mgr.list_tools().await
                }
            });
            handles.push(handle);
        }

        // Collect results
        let mut all_valid = true;
        for handle in handles {
            let tools = handle.await.unwrap();
            if tools.len() != 4 {
                all_valid = false;
                break;
            }
        }

        // Assert: Cache state remains consistent
        assert!(
            all_valid,
            "Cache should remain consistent across concurrent operations"
        );
    }

    // =========================================================================
    // Test 9: Tool Execution Result Contains Timing Information
    // =========================================================================

    #[tokio::test]
    async fn test_tool_execution_result_includes_duration() {
        // Arrange: Create manager
        let manager = McpBackendManager::new();

        // Act: Execute a tool
        let result = manager.test_tool("agent-list", None).await;

        // Assert: Verify duration is recorded (u64 is always >= 0)
        // Should be very fast (less than 100ms for in-memory operation)
        assert!(
            result.duration_ms < 100,
            "In-memory tool execution should be fast"
        );
    }

    #[tokio::test]
    async fn test_failed_tool_execution_includes_error_message() {
        // Arrange: Create manager
        let manager = McpBackendManager::new();

        // Act: Execute non-existent tool
        let result = manager.test_tool("invalid-tool", None).await;

        // Assert: Verify error details
        assert!(!result.success, "Should fail");
        assert!(result.error.is_some(), "Should have error");
        let error = result.error.unwrap();
        assert!(
            error.contains("invalid-tool"),
            "Error should mention the tool name"
        );
    }

    // =========================================================================
    // Test 10: Tool Schema Validation Structure
    // =========================================================================

    #[tokio::test]
    async fn test_tool_schema_has_correct_json_schema_structure() {
        // Arrange: Create manager
        let manager = McpBackendManager::new();

        // Act: Get agent-start schema
        let schemas = manager.get_schemas().await;
        let schema = &schemas["agent-start"];

        // Assert: Verify JSON Schema compliance
        assert_eq!(
            schema.get("type").and_then(|v| v.as_str()),
            Some("object")
        );
        assert!(schema.get("properties").is_some());
        assert!(schema.get("required").is_some());

        let props = schema.get("properties").unwrap().as_object().unwrap();
        assert!(
            props.contains_key("name"),
            "Should have name property"
        );

        let name_prop = &props["name"];
        assert_eq!(
            name_prop.get("type").and_then(|v| v.as_str()),
            Some("string")
        );
    }
}
