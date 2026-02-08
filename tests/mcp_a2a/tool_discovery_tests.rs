//! Tool Discovery Integration Tests
//!
//! Comprehensive integration tests for MCP tool discovery through A2A protocol.
//! Tests cover:
//! - TD-001: Basic Tool Discovery
//! - TD-002: Dynamic Tool Registration
//! - TD-003: Tool Schema Validation
//! - TD-004: Discovery Error Handling
//! - TD-005: Cached Discovery
//!
//! Follows Chicago TDD AAA pattern (Arrange/Act/Assert) with state-based testing
//! and real collaborators (no mocks unless external I/O).

use std::collections::HashMap;
use std::sync::{Arc, Mutex};
use std::time::Duration;
use tokio::time::timeout;
use serde::{Deserialize, Serialize};
use serde_json::{json, Value};

// ============================================================================
// MOCK INFRASTRUCTURE
// ============================================================================

/// Mock A2A server for testing tool discovery
///
/// This mock simulates an A2A protocol server that exposes tools via MCP.
/// Following Chicago TDD: real collaborator pattern for in-memory testing.
#[derive(Clone)]
pub struct MockA2AServer {
    base_url: String,
    tools: Arc<Mutex<HashMap<String, ToolDefinition>>>,
    discovery_count: Arc<Mutex<usize>>,
    errors: Arc<Mutex<Vec<DiscoveryError>>>,
    is_running: Arc<Mutex<bool>>,
}

/// Tool definition as exposed through MCP protocol
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct ToolDefinition {
    pub name: String,
    pub description: String,
    pub category: ToolCategory,
    pub schema: Value,
    pub agent_id: Option<String>,
    pub requires_auth: bool,
    pub deprecated: bool,
}

#[derive(Debug, Clone, Serialize, Deserialize, PartialEq)]
pub enum ToolCategory {
    Core,
    Agent,
    Workflow,
    System,
    Custom(String),
}

/// Discovery error types
#[derive(Debug, Clone)]
pub struct DiscoveryError {
    pub error_type: ErrorType,
    pub message: String,
    pub timestamp: std::time::Instant,
}

#[derive(Debug, Clone, PartialEq)]
pub enum ErrorType {
    ConnectionRefused,
    Timeout,
    InvalidSchema,
    Unauthorized,
    NotFound,
    Internal,
}

impl MockA2AServer {
    /// Create a new mock A2A server
    pub fn new(base_url: String) -> Self {
        Self {
            base_url,
            tools: Arc::new(Mutex::new(HashMap::new())),
            discovery_count: Arc::new(Mutex::new(0)),
            errors: Arc::new(Mutex::new(Vec::new())),
            is_running: Arc::new(Mutex::new(true)),
        }
    }

    /// Register a tool with the server
    pub fn register_tool(&self, tool: ToolDefinition) -> Result<(), String> {
        if !*self.is_running.lock().unwrap() {
            return Err("Server is not running".to_string());
        }
        self.tools.lock().unwrap().insert(tool.name.clone(), tool);
        Ok(())
    }

    /// Discover all available tools
    pub fn discover_tools(&self) -> Result<Vec<ToolDefinition>, DiscoveryError> {
        // Increment discovery count
        *self.discovery_count.lock().unwrap() += 1;

        // Check server status
        if !*self.is_running.lock().unwrap() {
            return Err(DiscoveryError {
                error_type: ErrorType::ConnectionRefused,
                message: "Server is not running".to_string(),
                timestamp: std::time::Instant::now(),
            });
        }

        // Return all tools
        let tools = self.tools.lock().unwrap();
        Ok(tools.values().cloned().collect())
    }

    /// Discover tools by category
    pub fn discover_tools_by_category(&self, category: &ToolCategory) -> Result<Vec<ToolDefinition>, DiscoveryError> {
        if !*self.is_running.lock().unwrap() {
            return Err(DiscoveryError {
                error_type: ErrorType::ConnectionRefused,
                message: "Server is not running".to_string(),
                timestamp: std::time::Instant::now(),
            });
        }

        let tools = self.tools.lock().unwrap();
        let filtered: Vec<_> = tools.values()
            .filter(|t| &t.category == category)
            .cloned()
            .collect();

        Ok(filtered)
    }

    /// Get a specific tool by name
    pub fn get_tool(&self, name: &str) -> Result<ToolDefinition, DiscoveryError> {
        if !*self.is_running.lock().unwrap() {
            return Err(DiscoveryError {
                error_type: ErrorType::ConnectionRefused,
                message: "Server is not running".to_string(),
                timestamp: std::time::Instant::now(),
            });
        }

        let tools = self.tools.lock().unwrap();
        tools.get(name)
            .cloned()
            .ok_or_else(|| DiscoveryError {
                error_type: ErrorType::NotFound,
                message: format!("Tool '{}' not found", name),
                timestamp: std::time::Instant::now(),
            })
    }

    /// Simulate server stop
    pub fn stop(&self) {
        *self.is_running.lock().unwrap() = false;
    }

    /// Simulate server start
    pub fn start(&self) {
        *self.is_running.lock().unwrap() = true;
    }

    /// Check if server is running
    pub fn is_running(&self) -> bool {
        *self.is_running.lock().unwrap()
    }

    /// Get discovery count
    pub fn discovery_count(&self) -> usize {
        *self.discovery_count.lock().unwrap()
    }

    /// Record an error
    pub fn record_error(&self, error: DiscoveryError) {
        self.errors.lock().unwrap().push(error);
    }

    /// Get recorded errors
    pub fn errors(&self) -> Vec<DiscoveryError> {
        self.errors.lock().unwrap().clone()
    }

    /// Clear all errors
    pub fn clear_errors(&self) {
        self.errors.lock().unwrap().clear();
    }
}

/// Mock MCP client for tool discovery
///
/// This mock simulates an MCP client that discovers tools from A2A servers.
pub struct MockMCPClient {
    a2a_servers: Vec<MockA2AServer>,
    tool_cache: Arc<Mutex<HashMap<String, CachedTools>>>,
    cache_ttl: Duration,
}

/// Cached tools with expiration
#[derive(Clone)]
struct CachedTools {
    tools: Vec<ToolDefinition>,
    cached_at: std::time::Instant,
}

impl CachedTools {
    fn is_expired(&self, ttl: Duration) -> bool {
        self.cached_at.elapsed() > ttl
    }
}

impl MockMCPClient {
    /// Create a new MCP client with A2A server connections
    pub fn new(a2a_servers: Vec<MockA2AServer>, cache_ttl: Duration) -> Self {
        Self {
            a2a_servers,
            tool_cache: Arc::new(Mutex::new(HashMap::new())),
            cache_ttl,
        }
    }

    /// Discover all tools from all connected A2A servers
    pub async fn discover_all_tools(&self) -> Result<Vec<ToolDefinition>, String> {
        let mut all_tools = Vec::new();

        for server in &self.a2a_servers {
            match server.discover_tools() {
                Ok(tools) => all_tools.extend(tools),
                Err(e) => return Err(e.message),
            }
        }

        Ok(all_tools)
    }

    /// Discover tools with caching
    pub async fn discover_tools_cached(&self, cache_key: &str) -> Result<Vec<ToolDefinition>, String> {
        let mut cache = self.tool_cache.lock().unwrap();

        // Check cache
        if let Some(cached) = cache.get(cache_key) {
            if !cached.is_expired(self.cache_ttl) {
                return Ok(cached.tools.clone());
            }
        }

        // Cache miss or expired - fetch from servers
        drop(cache);
        let tools = self.discover_all_tools().await?;

        // Update cache
        let mut cache = self.tool_cache.lock().unwrap();
        cache.insert(cache_key.to_string(), CachedTools {
            tools: tools.clone(),
            cached_at: std::time::Instant::now(),
        });

        Ok(tools)
    }

    /// Invalidate cache for a specific key
    pub fn invalidate_cache(&self, cache_key: &str) {
        self.tool_cache.lock().unwrap().remove(cache_key);
    }

    /// Clear all cache
    pub fn clear_cache(&self) {
        self.tool_cache.lock().unwrap().clear();
    }

    /// Get tool by name from any server
    pub async fn get_tool_by_name(&self, name: &str) -> Result<ToolDefinition, String> {
        for server in &self.a2a_servers {
            if let Ok(tool) = server.get_tool(name) {
                return Ok(tool);
            }
        }
        Err(format!("Tool '{}' not found on any server", name))
    }
}

/// Tool Discovery Manager
///
/// High-level interface for tool discovery operations.
pub struct ToolDiscoveryManager {
    mcp_client: MockMCPClient,
}

impl ToolDiscoveryManager {
    /// Create a new tool discovery manager
    pub fn new(mcp_client: MockMCPClient) -> Self {
        Self { mcp_client }
    }

    /// Discover all available tools
    pub async fn discover_tools(&self) -> Result<DiscoveryResult, String> {
        let tools = self.mcp_client.discover_all_tools().await?;

        Ok(DiscoveryResult {
            tools: tools.clone(),
            total_count: tools.len(),
            core_tools: tools.iter().filter(|t| t.category == ToolCategory::Core).count(),
            agent_tools: tools.iter().filter(|t| t.category == ToolCategory::Agent).count(),
            workflow_tools: tools.iter().filter(|t| t.category == ToolCategory::Workflow).count(),
            custom_tools: tools.iter().filter(|t| matches!(&t.category, ToolCategory::Custom(_))).count(),
        })
    }

    /// Discover tools with caching
    pub async fn discover_tools_with_cache(&self, cache_key: &str) -> Result<DiscoveryResult, String> {
        let tools = self.mcp_client.discover_tools_cached(cache_key).await?;

        Ok(DiscoveryResult {
            tools: tools.clone(),
            total_count: tools.len(),
            core_tools: tools.iter().filter(|t| t.category == ToolCategory::Core).count(),
            agent_tools: tools.iter().filter(|t| t.category == ToolCategory::Agent).count(),
            workflow_tools: tools.iter().filter(|t| t.category == ToolCategory::Workflow).count(),
            custom_tools: tools.iter().filter(|t| matches!(&t.category, ToolCategory::Custom(_))).count(),
        })
    }

    /// Validate tool schema
    pub fn validate_tool_schema(&self, tool: &ToolDefinition) -> Result<SchemaValidationResult, String> {
        // Check required fields
        if tool.name.is_empty() {
            return Ok(SchemaValidationResult {
                is_valid: false,
                errors: vec!["Tool name cannot be empty".to_string()],
                warnings: Vec::new(),
            });
        }

        if tool.description.is_empty() {
            return Ok(SchemaValidationResult {
                is_valid: false,
                errors: vec!["Tool description cannot be empty".to_string()],
                warnings: Vec::new(),
            });
        }

        let mut warnings = Vec::new();

        // Check schema structure
        if !tool.schema.is_object() {
            return Ok(SchemaValidationResult {
                is_valid: false,
                errors: vec!["Tool schema must be a JSON object".to_string()],
                warnings: Vec::new(),
            });
        }

        // Check for deprecated status
        if tool.deprecated {
            warnings.push(format!("Tool '{}' is deprecated", tool.name));
        }

        Ok(SchemaValidationResult {
            is_valid: true,
            errors: Vec::new(),
            warnings,
        })
    }

    /// Invalidate tool cache
    pub fn invalidate_cache(&self, cache_key: &str) {
        self.mcp_client.invalidate_cache(cache_key);
    }
}

/// Result of tool discovery operation
#[derive(Debug, Clone)]
pub struct DiscoveryResult {
    pub tools: Vec<ToolDefinition>,
    pub total_count: usize,
    pub core_tools: usize,
    pub agent_tools: usize,
    pub workflow_tools: usize,
    pub custom_tools: usize,
}

/// Result of schema validation
#[derive(Debug, Clone)]
pub struct SchemaValidationResult {
    pub is_valid: bool,
    pub errors: Vec<String>,
    pub warnings: Vec<String>,
}

// ============================================================================
// TEST UTILITIES
// ============================================================================

/// Create a standard test tool
fn create_test_tool(name: &str, category: ToolCategory) -> ToolDefinition {
    ToolDefinition {
        name: name.to_string(),
        description: format!("Test tool for {}", name),
        category,
        schema: json!({
            "type": "object",
            "properties": {
                "input": {"type": "string"}
            },
            "required": ["input"]
        }),
        agent_id: None,
        requires_auth: false,
        deprecated: false,
    }
}

/// Create a core tool
fn create_core_tool(name: &str) -> ToolDefinition {
    create_test_tool(name, ToolCategory::Core)
}

/// Create an agent tool
fn create_agent_tool(name: &str, agent_id: &str) -> ToolDefinition {
    ToolDefinition {
        name: name.to_string(),
        description: format!("Agent tool for {}", name),
        category: ToolCategory::Agent,
        schema: json!({
            "type": "object",
            "properties": {
                "command": {"type": "string", "description": "Command to execute"}
            },
            "required": ["command"]
        }),
        agent_id: Some(agent_id.to_string()),
        requires_auth: true,
        deprecated: false,
    }
}

// ============================================================================
// TD-001: BASIC TOOL DISCOVERY TESTS
// ============================================================================

#[tokio::test]
async fn td_001_basic_tool_discovery_returns_all_registered_tools() {
    // ARRANGE: Create mock A2A server and register tools
    let server = MockA2AServer::new("http://localhost:8080".to_string());
    server.register_tool(create_core_tool("agent-list"));
    server.register_tool(create_core_tool("agent-start"));
    server.register_tool(create_agent_tool("text-generator", "agent-123"));

    let manager = ToolDiscoveryManager::new(MockMCPClient::new(
        vec![server.clone()],
        Duration::from_secs(60),
    ));

    // ACT: Discover all tools
    let result = manager.discover_tools().await;

    // ASSERT: Verify all tools are returned
    assert!(result.is_ok(), "Discovery should succeed");
    let discovery = result.unwrap();
    assert_eq!(discovery.total_count, 3, "Should discover 3 tools");
    assert_eq!(discovery.core_tools, 2, "Should have 2 core tools");
    assert_eq!(discovery.agent_tools, 1, "Should have 1 agent tool");
}

#[tokio::test]
async fn td_001_basic_tool_discovery_empty_server_returns_zero_tools() {
    // ARRANGE: Create mock A2A server with no tools
    let server = MockA2AServer::new("http://localhost:8080".to_string());

    let manager = ToolDiscoveryManager::new(MockMCPClient::new(
        vec![server.clone()],
        Duration::from_secs(60),
    ));

    // ACT: Discover all tools
    let result = manager.discover_tools().await;

    // ASSERT: Verify empty result
    assert!(result.is_ok(), "Discovery should succeed even with no tools");
    let discovery = result.unwrap();
    assert_eq!(discovery.total_count, 0, "Should discover 0 tools");
    assert!(discovery.tools.is_empty(), "Tools list should be empty");
}

#[tokio::test]
async fn td_001_basic_tool_discovery_multiple_servers_aggregates_tools() {
    // ARRANGE: Create multiple mock A2A servers
    let server1 = MockA2AServer::new("http://localhost:8080".to_string());
    server1.register_tool(create_core_tool("agent-list"));

    let server2 = MockA2AServer::new("http://localhost:8081".to_string());
    server2.register_tool(create_core_tool("agent-start"));
    server2.register_tool(create_agent_tool("workflow-executor", "agent-456"));

    let manager = ToolDiscoveryManager::new(MockMCPClient::new(
        vec![server1.clone(), server2.clone()],
        Duration::from_secs(60),
    ));

    // ACT: Discover all tools from all servers
    let result = manager.discover_tools().await;

    // ASSERT: Verify tools from all servers are aggregated
    assert!(result.is_ok(), "Discovery should succeed");
    let discovery = result.unwrap();
    assert_eq!(discovery.total_count, 3, "Should discover 3 tools from all servers");
    assert_eq!(discovery.core_tools, 2, "Should have 2 core tools");
    assert_eq!(discovery.agent_tools, 1, "Should have 1 agent tool");
}

// ============================================================================
// TD-002: DYNAMIC TOOL REGISTRATION TESTS
// ============================================================================

#[tokio::test]
async fn td_002_dynamic_tool_registration_new_tool_appears_in_discovery() {
    // ARRANGE: Create mock A2A server with initial tools
    let server = MockA2AServer::new("http://localhost:8080".to_string());
    server.register_tool(create_core_tool("agent-list"));

    let manager = ToolDiscoveryManager::new(MockMCPClient::new(
        vec![server.clone()],
        Duration::from_secs(60),
    ));

    // ACT: Register a new tool and discover
    server.register_tool(create_core_tool("agent-status"));
    let result = manager.discover_tools().await;

    // ASSERT: Verify new tool appears in discovery
    assert!(result.is_ok(), "Discovery should succeed");
    let discovery = result.unwrap();
    assert_eq!(discovery.total_count, 2, "Should discover 2 tools after registration");
    assert!(discovery.tools.iter().any(|t| t.name == "agent-status"),
            "Newly registered tool should appear in discovery");
}

#[tokio::test]
async fn td_002_dynamic_tool_registration_batch_registration_all_discoverable() {
    // ARRANGE: Create mock A2A server
    let server = MockA2AServer::new("http://localhost:8080".to_string());

    // ACT: Register multiple tools at once
    let tools_to_register = vec![
        create_core_tool("agent-list"),
        create_core_tool("agent-start"),
        create_core_tool("agent-status"),
        create_agent_tool("text-generator", "agent-1"),
        create_agent_tool("code-analyzer", "agent-2"),
    ];

    for tool in tools_to_register {
        server.register_tool(tool).expect("Registration should succeed");
    }

    let manager = ToolDiscoveryManager::new(MockMCPClient::new(
        vec![server.clone()],
        Duration::from_secs(60),
    ));
    let result = manager.discover_tools().await;

    // ASSERT: All registered tools are discoverable
    assert!(result.is_ok(), "Discovery should succeed");
    let discovery = result.unwrap();
    assert_eq!(discovery.total_count, 5, "Should discover all 5 registered tools");
}

#[tokio::test]
async fn td_002_dynamic_tool_registration_duplicate_tool_name_overwrites() {
    // ARRANGE: Create mock A2A server
    let server = MockA2AServer::new("http://localhost:8080".to_string());
    let original_tool = ToolDefinition {
        name: "agent-list".to_string(),
        description: "Original description".to_string(),
        category: ToolCategory::Core,
        schema: json!({"type": "object"}),
        agent_id: None,
        requires_auth: false,
        deprecated: false,
    };

    server.register_tool(original_tool);

    // ACT: Register tool with same name but different description
    let updated_tool = ToolDefinition {
        name: "agent-list".to_string(),
        description: "Updated description".to_string(),
        category: ToolCategory::Core,
        schema: json!({"type": "object"}),
        agent_id: None,
        requires_auth: false,
        deprecated: false,
    };

    server.register_tool(updated_tool);

    let manager = ToolDiscoveryManager::new(MockMCPClient::new(
        vec![server.clone()],
        Duration::from_secs(60),
    ));
    let result = manager.discover_tools().await;

    // ASSERT: Updated tool is returned
    assert!(result.is_ok(), "Discovery should succeed");
    let discovery = result.unwrap();
    assert_eq!(discovery.total_count, 1, "Should have 1 tool (not duplicate)");
    let discovered = &discovery.tools[0];
    assert_eq!(discovered.description, "Updated description",
               "Tool should have updated description");
}

#[tokio::test]
async fn td_002_dynamic_tool_registration_cannot_register_when_server_stopped() {
    // ARRANGE: Create and stop mock A2A server
    let server = MockA2AServer::new("http://localhost:8080".to_string());
    server.stop();

    // ACT: Try to register a tool
    let result = server.register_tool(create_core_tool("agent-list"));

    // ASSERT: Registration should fail
    assert!(result.is_err(), "Registration should fail when server is stopped");
    assert!(result.unwrap_err().contains("not running"),
            "Error message should indicate server is not running");
}

// ============================================================================
// TD-003: TOOL SCHEMA VALIDATION TESTS
// ============================================================================

#[tokio::test]
async fn td_003_tool_schema_validation_valid_tool_passes() {
    // ARRANGE: Create tool discovery manager
    let server = MockA2AServer::new("http://localhost:8080".to_string());
    let manager = ToolDiscoveryManager::new(MockMCPClient::new(
        vec![server.clone()],
        Duration::from_secs(60),
    ));

    let valid_tool = ToolDefinition {
        name: "valid-tool".to_string(),
        description: "A valid tool with proper schema".to_string(),
        category: ToolCategory::Core,
        schema: json!({
            "type": "object",
            "properties": {
                "input": {
                    "type": "string",
                    "description": "Input parameter"
                }
            },
            "required": ["input"]
        }),
        agent_id: None,
        requires_auth: false,
        deprecated: false,
    };

    // ACT: Validate tool schema
    let result = manager.validate_tool_schema(&valid_tool);

    // ASSERT: Validation should pass
    assert!(result.is_ok(), "Validation should succeed");
    let validation = result.unwrap();
    assert!(validation.is_valid, "Tool schema should be valid");
    assert!(validation.errors.is_empty(), "Should have no validation errors");
}

#[tokio::test]
async fn td_003_tool_schema_validation_empty_name_fails() {
    // ARRANGE: Create tool discovery manager
    let server = MockA2AServer::new("http://localhost:8080".to_string());
    let manager = ToolDiscoveryManager::new(MockMCPClient::new(
        vec![server.clone()],
        Duration::from_secs(60),
    ));

    let invalid_tool = ToolDefinition {
        name: "".to_string(),
        description: "Tool with empty name".to_string(),
        category: ToolCategory::Core,
        schema: json!({"type": "object"}),
        agent_id: None,
        requires_auth: false,
        deprecated: false,
    };

    // ACT: Validate tool schema
    let result = manager.validate_tool_schema(&invalid_tool);

    // ASSERT: Validation should fail with appropriate error
    assert!(result.is_ok(), "Validation should complete");
    let validation = result.unwrap();
    assert!(!validation.is_valid, "Tool schema should be invalid");
    assert!(validation.errors.iter().any(|e| e.contains("name")),
            "Should have error about empty name");
}

#[tokio::test]
async fn td_003_tool_schema_validation_empty_description_fails() {
    // ARRANGE: Create tool discovery manager
    let server = MockA2AServer::new("http://localhost:8080".to_string());
    let manager = ToolDiscoveryManager::new(MockMCPClient::new(
        vec![server.clone()],
        Duration::from_secs(60),
    ));

    let invalid_tool = ToolDefinition {
        name: "tool-without-description".to_string(),
        description: "".to_string(),
        category: ToolCategory::Core,
        schema: json!({"type": "object"}),
        agent_id: None,
        requires_auth: false,
        deprecated: false,
    };

    // ACT: Validate tool schema
    let result = manager.validate_tool_schema(&invalid_tool);

    // ASSERT: Validation should fail with appropriate error
    assert!(result.is_ok(), "Validation should complete");
    let validation = result.unwrap();
    assert!(!validation.is_valid, "Tool schema should be invalid");
    assert!(validation.errors.iter().any(|e| e.contains("description")),
            "Should have error about empty description");
}

#[tokio::test]
async fn td_003_tool_schema_validation_invalid_schema_type_fails() {
    // ARRANGE: Create tool discovery manager
    let server = MockA2AServer::new("http://localhost:8080".to_string());
    let manager = ToolDiscoveryManager::new(MockMCPClient::new(
        vec![server.clone()],
        Duration::from_secs(60),
    ));

    let invalid_tool = ToolDefinition {
        name: "invalid-schema-tool".to_string(),
        description: "Tool with invalid schema".to_string(),
        category: ToolCategory::Core,
        schema: json!("not-an-object"),
        agent_id: None,
        requires_auth: false,
        deprecated: false,
    };

    // ACT: Validate tool schema
    let result = manager.validate_tool_schema(&invalid_tool);

    // ASSERT: Validation should fail
    assert!(result.is_ok(), "Validation should complete");
    let validation = result.unwrap();
    assert!(!validation.is_valid, "Tool schema should be invalid");
    assert!(validation.errors.iter().any(|e| e.contains("object")),
            "Should have error about schema not being an object");
}

#[tokio::test]
async fn td_003_tool_schema_validation_deprecated_tool_generates_warning() {
    // ARRANGE: Create tool discovery manager
    let server = MockA2AServer::new("http://localhost:8080".to_string());
    let manager = ToolDiscoveryManager::new(MockMCPClient::new(
        vec![server.clone()],
        Duration::from_secs(60),
    ));

    let deprecated_tool = ToolDefinition {
        name: "old-tool".to_string(),
        description: "A deprecated tool".to_string(),
        category: ToolCategory::Core,
        schema: json!({"type": "object"}),
        agent_id: None,
        requires_auth: false,
        deprecated: true,
    };

    // ACT: Validate tool schema
    let result = manager.validate_tool_schema(&deprecated_tool);

    // ASSERT: Validation passes but with deprecation warning
    assert!(result.is_ok(), "Validation should complete");
    let validation = result.unwrap();
    assert!(validation.is_valid, "Deprecated tool schema should still be valid");
    assert!(!validation.warnings.is_empty(), "Should have deprecation warning");
    assert!(validation.warnings.iter().any(|w| w.contains("deprecated")),
            "Should have warning about deprecation");
}

// ============================================================================
// TD-004: DISCOVERY ERROR HANDLING TESTS
// ============================================================================

#[tokio::test]
async fn td_004_discovery_error_handling_server_not_running_returns_error() {
    // ARRANGE: Create and stop mock A2A server
    let server = MockA2AServer::new("http://localhost:8080".to_string());
    server.stop();

    let manager = ToolDiscoveryManager::new(MockMCPClient::new(
        vec![server.clone()],
        Duration::from_secs(60),
    ));

    // ACT: Try to discover tools
    let result = manager.discover_tools().await;

    // ASSERT: Should return error
    assert!(result.is_err(), "Discovery should fail when server is not running");
    assert!(result.unwrap_err().contains("not running") ||
            result.unwrap_err().contains("Server"),
            "Error should indicate server issue");
}

#[tokio::test]
async fn td_004_discovery_error_handling_partial_server_failure_returns_error() {
    // ARRANGE: Create multiple servers, one stopped
    let server1 = MockA2AServer::new("http://localhost:8080".to_string());
    server1.register_tool(create_core_tool("agent-list"));

    let server2 = MockA2AServer::new("http://localhost:8081".to_string());
    server2.stop(); // This server is stopped

    let manager = ToolDiscoveryManager::new(MockMCPClient::new(
        vec![server1.clone(), server2.clone()],
        Duration::from_secs(60),
    ));

    // ACT: Try to discover tools from all servers
    let result = manager.discover_tools().await;

    // ASSERT: Should return error due to failed server
    assert!(result.is_err(), "Discovery should fail when any server is unavailable");
}

#[tokio::test]
async fn td_004_discovery_error_handling_get_nonexistent_tool_returns_not_found() {
    // ARRANGE: Create mock A2A server
    let server = MockA2AServer::new("http://localhost:8080".to_string());

    // ACT: Try to get a tool that doesn't exist
    let result = server.get_tool("nonexistent-tool");

    // ASSERT: Should return NotFound error
    assert!(result.is_err(), "Getting nonexistent tool should fail");
    let error = result.unwrap_err();
    assert_eq!(error.error_type, ErrorType::NotFound,
               "Error type should be NotFound");
    assert!(error.message.contains("nonexistent-tool"),
            "Error message should mention the tool name");
}

#[tokio::test]
async fn td_004_discovery_error_handling_error_preserved_for_inspection() {
    // ARRANGE: Create mock A2A server and trigger an error
    let server = MockA2AServer::new("http://localhost:8080".to_string());
    server.record_error(DiscoveryError {
        error_type: ErrorType::Timeout,
        message: "Connection timed out".to_string(),
        timestamp: std::time::Instant::now(),
    });

    // ACT: Retrieve recorded errors
    let errors = server.errors();

    // ASSERT: Error should be preserved
    assert_eq!(errors.len(), 1, "Should have one recorded error");
    assert_eq!(errors[0].error_type, ErrorType::Timeout,
               "Error type should be Timeout");
    assert!(errors[0].message.contains("timed out"),
            "Error message should describe the issue");
}

#[tokio::test]
async fn td_004_discovery_error_handling_clear_errors_removes_all() {
    // ARRANGE: Create mock A2A server with recorded errors
    let server = MockA2AServer::new("http://localhost:8080".to_string());
    server.record_error(DiscoveryError {
        error_type: ErrorType::Timeout,
        message: "Error 1".to_string(),
        timestamp: std::time::Instant::now(),
    });
    server.record_error(DiscoveryError {
        error_type: ErrorType::ConnectionRefused,
        message: "Error 2".to_string(),
        timestamp: std::time::Instant::now(),
    });

    // Verify errors exist
    assert_eq!(server.errors().len(), 2, "Should have 2 errors before clearing");

    // ACT: Clear all errors
    server.clear_errors();

    // ASSERT: All errors should be removed
    assert_eq!(server.errors().len(), 0, "Should have no errors after clearing");
}

// ============================================================================
// TD-005: CACHED DISCOVERY TESTS
// ============================================================================

#[tokio::test]
async fn td_005_cached_discovery_first_call_fetches_from_server() {
    // ARRANGE: Create mock A2A server with tools
    let server = MockA2AServer::new("http://localhost:8080".to_string());
    server.register_tool(create_core_tool("agent-list"));

    let client = MockMCPClient::new(
        vec![server.clone()],
        Duration::from_secs(60),
    );
    let manager = ToolDiscoveryManager::new(client);

    // ACT: First discovery call
    let result = manager.discover_tools_with_cache("test-cache").await;

    // ASSERT: Should fetch from server
    assert!(result.is_ok(), "Discovery should succeed");
    assert_eq!(server.discovery_count(), 1, "Should have called server discovery once");
}

#[tokio::test]
async fn td_005_cached_discovery_second_call_uses_cache() {
    // ARRANGE: Create mock A2A server with tools
    let server = MockA2AServer::new("http://localhost:8080".to_string());
    server.register_tool(create_core_tool("agent-list"));

    let client = MockMCPClient::new(
        vec![server.clone()],
        Duration::from_secs(60),
    );
    let manager = ToolDiscoveryManager::new(client);

    // ACT: First discovery call (cache miss)
    let _result1 = manager.discover_tools_with_cache("test-cache").await;
    let first_count = server.discovery_count();

    // Second discovery call (cache hit)
    let _result2 = manager.discover_tools_with_cache("test-cache").await;
    let second_count = server.discovery_count();

    // ASSERT: Second call should use cache, not fetch from server
    assert_eq!(first_count, 1, "First call should fetch from server");
    assert_eq!(second_count, 1, "Second call should use cache (no additional server call)");
}

#[tokio::test]
async fn td_005_cached_discovery_different_cache_keys_fetch_separately() {
    // ARRANGE: Create mock A2A server with tools
    let server = MockA2AServer::new("http://localhost:8080".to_string());
    server.register_tool(create_core_tool("agent-list"));

    let client = MockMCPClient::new(
        vec![server.clone()],
        Duration::from_secs(60),
    );
    let manager = ToolDiscoveryManager::new(client);

    // ACT: Discovery with different cache keys
    let _result1 = manager.discover_tools_with_cache("cache-key-1").await;
    let _result2 = manager.discover_tools_with_cache("cache-key-2").await;

    // ASSERT: Both should fetch from server
    assert_eq!(server.discovery_count(), 2, "Both calls should fetch from server");
}

#[tokio::test]
async fn td_005_cached_discovery_expired_cache_refetches() {
    // ARRANGE: Create mock A2A server with tools and short TTL
    let server = MockA2AServer::new("http://localhost:8080".to_string());
    server.register_tool(create_core_tool("agent-list"));

    let client = MockMCPClient::new(
        vec![server.clone()],
        Duration::from_millis(100), // Very short TTL
    );
    let manager = ToolDiscoveryManager::new(client);

    // ACT: First discovery (cache miss)
    let _result1 = manager.discover_tools_with_cache("test-cache").await;
    let first_count = server.discovery_count();

    // Wait for cache to expire
    tokio::time::sleep(Duration::from_millis(150)).await;

    // Second discovery (cache expired)
    let _result2 = manager.discover_tools_with_cache("test-cache").await;
    let second_count = server.discovery_count();

    // ASSERT: Second call should refetch from server
    assert_eq!(first_count, 1, "First call should fetch from server");
    assert_eq!(second_count, 2, "Second call should refetch from server after cache expires");
}

#[tokio::test]
async fn td_005_cached_discovery_manual_invalidate_clears_cache() {
    // ARRANGE: Create mock A2A server and manager
    let server = MockA2AServer::new("http://localhost:8080".to_string());
    server.register_tool(create_core_tool("agent-list"));

    let client = MockMCPClient::new(
        vec![server.clone()],
        Duration::from_secs(60),
    );
    let manager = ToolDiscoveryManager::new(client);

    // ACT: First discovery, then invalidate cache
    let _result1 = manager.discover_tools_with_cache("test-cache").await;
    let first_count = server.discovery_count();

    manager.invalidate_cache("test-cache");

    let _result2 = manager.discover_tools_with_cache("test-cache").await;
    let second_count = server.discovery_count();

    // ASSERT: After invalidation, should refetch from server
    assert_eq!(first_count, 1, "First call should fetch from server");
    assert_eq!(second_count, 2, "After invalidation, should refetch from server");
}

#[tokio::test]
async fn td_005_cached_discovery_register_new_tool_invalidate_discover_new_tool() {
    // ARRANGE: Create mock A2A server with initial tool
    let server = MockA2AServer::new("http://localhost:8080".to_string());
    server.register_tool(create_core_tool("agent-list"));

    let client = MockMCPClient::new(
        vec![server.clone()],
        Duration::from_secs(60),
    );
    let manager = ToolDiscoveryManager::new(client);

    // ACT: Initial discovery, register new tool, invalidate, rediscover
    let result1 = manager.discover_tools_with_cache("test-cache").await;
    let initial_count = result1.unwrap().total_count;

    // Register new tool
    server.register_tool(create_core_tool("agent-start"));

    // Invalidate cache and rediscover
    manager.invalidate_cache("test-cache");
    let result2 = manager.discover_tools_with_cache("test-cache").await;
    let new_count = result2.unwrap().total_count;

    // ASSERT: New tool should be discovered after cache invalidation
    assert_eq!(initial_count, 1, "Initial discovery should find 1 tool");
    assert_eq!(new_count, 2, "After invalidation, should discover 2 tools");
}

// ============================================================================
// INTEGRATION TEST: END-TO-END SCENARIOS
// ============================================================================

#[tokio::test]
async fn integration_full_tool_discovery_workflow() {
    // ARRANGE: Setup complete multi-server environment
    let server1 = MockA2AServer::new("http://localhost:8080".to_string());
    server1.register_tool(create_core_tool("agent-list"));
    server1.register_tool(create_core_tool("agent-start"));

    let server2 = MockA2AServer::new("http://localhost:8081".to_string());
    server2.register_tool(create_agent_tool("text-generator", "agent-1"));
    server2.register_tool(create_agent_tool("code-analyzer", "agent-2"));

    let server3 = MockA2AServer::new("http://localhost:8082".to_string());
    server3.register_tool(create_test_tool("workflow-start", ToolCategory::Workflow));

    let manager = ToolDiscoveryManager::new(MockMCPClient::new(
        vec![server1, server2, server3],
        Duration::from_secs(60),
    ));

    // ACT: Perform full discovery workflow
    let discovery_result = manager.discover_tools_with_cache("full-discovery").await;

    // ASSERT: Verify complete discovery
    assert!(discovery_result.is_ok(), "Discovery should succeed");
    let result = discovery_result.unwrap();
    assert_eq!(result.total_count, 5, "Should discover all 5 tools");
    assert_eq!(result.core_tools, 2, "Should have 2 core tools");
    assert_eq!(result.agent_tools, 2, "Should have 2 agent tools");
    assert_eq!(result.workflow_tools, 1, "Should have 1 workflow tool");

    // Validate all discovered tools
    for tool in &result.tools {
        let validation = manager.validate_tool_schema(tool);
        assert!(validation.is_ok(), "Validation should complete");
        assert!(validation.unwrap().is_valid, "All discovered tools should have valid schemas");
    }
}

#[tokio::test]
async fn integration_tool_discovery_with_timeout() {
    // ARRANGE: Create server and manager
    let server = MockA2AServer::new("http://localhost:8080".to_string());
    server.register_tool(create_core_tool("agent-list"));

    let manager = ToolDiscoveryManager::new(MockMCPClient::new(
        vec![server.clone()],
        Duration::from_secs(60),
    ));

    // ACT: Perform discovery with timeout
    let result = timeout(Duration::from_millis(100), manager.discover_tools()).await;

    // ASSERT: Should complete within timeout
    assert!(result.is_ok(), "Discovery should complete within timeout");
    assert!(result.unwrap().is_ok(), "Discovery should succeed");
}

#[tokio::test]
async fn integration_concurrent_discoveries_handle_correctly() {
    // ARRANGE: Create server
    let server = MockA2AServer::new("http://localhost:8080".to_string());
    server.register_tool(create_core_tool("agent-list"));
    server.register_tool(create_core_tool("agent-start"));

    let manager = Arc::new(manager);
    let manager = ToolDiscoveryManager::new(MockMCPClient::new(
        vec![server.clone()],
        Duration::from_secs(60),
    ));

    // ACT: Launch concurrent discoveries
    let manager1 = manager.clone();
    let manager2 = manager.clone();
    let manager3 = manager.clone();

    let task1 = tokio::spawn(async move { manager1.discover_tools().await });
    let task2 = tokio::spawn(async move { manager2.discover_tools().await });
    let task3 = tokio::spawn(async move { manager3.discover_tools().await });

    let (result1, result2, result3) = tokio::join!(task1, task2, task3);

    // ASSERT: All discoveries should succeed
    assert!(result1.unwrap().is_ok(), "First discovery should succeed");
    assert!(result2.unwrap().is_ok(), "Second discovery should succeed");
    assert!(result3.unwrap().is_ok(), "Third discovery should succeed");
}
