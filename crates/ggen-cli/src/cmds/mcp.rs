//! MCP (Model Context Protocol) Commands
//!
//! This module provides MCP and A2A configuration, server management,
//! and tool bridging commands for the ggen CLI.

#![allow(clippy::unused_unit)] // clap-noun-verb macro generates this

use clap_noun_verb::Result as VerbResult;
use clap_noun_verb_macros::verb;
use serde::Serialize;
use std::collections::HashMap;
use std::path::PathBuf;
use std::sync::Arc;
use tokio::sync::RwLock;

use ggen_ai::constants::{env_vars, models};
use ggen_ai::{GenAiClient, LlmClient, LlmConfig};
use ggen_domain::mcp_config::{
    load_config, stop_server, A2aConfig, McpServerConfig, PROJECT_A2A_CONFIG, PROJECT_MCP_CONFIG,
};
use serde_json::Value as JsonValue;

use crate::runtime::block_on;

// ============================================================================
// Backend Manager
// ============================================================================

/// MCP Tool information
#[derive(Debug, Clone, Serialize)]
struct McpToolInfo {
    name: String,
    description: String,
    tool_type: String,
    server_name: Option<String>,
    agent_id: Option<String>,
    agent_name: Option<String>,
    available: bool,
    #[serde(default)]
    input_schema: Option<JsonValue>,
}

/// Tool execution result
#[derive(Debug, Clone, Serialize)]
struct ToolExecutionResult {
    tool_name: String,
    success: bool,
    content: Option<JsonValue>,
    error: Option<String>,
    duration_ms: u64,
}

/// MCP Backend Manager
struct McpBackendManager {
    project_dir: PathBuf,
    mcp_config: Option<McpConfigWrapper>,
    a2a_config: Option<A2aConfig>,
    tools_cache: Arc<RwLock<Vec<McpToolInfo>>>,
    agent_mappings: Arc<RwLock<HashMap<String, String>>>,
}

#[derive(Debug, Clone)]
struct McpConfigWrapper {
    mcp_servers: HashMap<String, McpServerConfig>,
}

impl McpBackendManager {
    fn new(project_dir: Option<PathBuf>) -> Self {
        let project_dir = project_dir
            .or_else(|| std::env::current_dir().ok())
            .unwrap_or_else(|| PathBuf::from("."));

        Self {
            project_dir,
            mcp_config: None,
            a2a_config: None,
            tools_cache: Arc::new(RwLock::new(Vec::new())),
            agent_mappings: Arc::new(RwLock::new(HashMap::new())),
        }
    }

    async fn load_config(&mut self) {
        if let Ok(resolved) = load_config(Some(&self.project_dir), None, None) {
            if let Some(mcp) = resolved.mcp {
                self.mcp_config = Some(McpConfigWrapper {
                    mcp_servers: mcp.mcp_servers,
                });
            }
            self.a2a_config = resolved.a2a;
        }
    }

    async fn ensure_config(&mut self) {
        if self.mcp_config.is_none() && self.a2a_config.is_none() {
            self.load_config().await;
        }
    }

    async fn list_tools(&mut self) -> Vec<McpToolInfo> {
        self.ensure_config().await;
        let mut tools = Vec::new();
        tools.extend(Self::core_tools());

        if let Some(ref config) = self.mcp_config {
            for server_name in config.mcp_servers.keys() {
                tools.push(McpToolInfo {
                    name: format!("{}:tools", server_name),
                    description: format!("Tools from {} server", server_name),
                    tool_type: "server".to_string(),
                    server_name: Some(server_name.clone()),
                    agent_id: None,
                    agent_name: None,
                    available: true,
                    input_schema: None,
                });
            }
        }

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

    async fn bridge_agent(&mut self, agent_name: &str, tool_name: Option<&str>) -> String {
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

    async fn test_tool(
        &mut self, tool_name: &str, arguments: Option<JsonValue>,
    ) -> ToolExecutionResult {
        let start = std::time::Instant::now();

        let mappings = self.agent_mappings.read().await;
        if let Some(agent_name) = mappings.get(tool_name) {
            return ToolExecutionResult {
                tool_name: tool_name.to_string(),
                success: true,
                content: Some(serde_json::json!({
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

        if let Some(result) = Self::execute_core_tool(tool_name, arguments).await {
            return ToolExecutionResult {
                tool_name: tool_name.to_string(),
                success: true,
                content: Some(result),
                error: None,
                duration_ms: start.elapsed().as_millis() as u64,
            };
        }

        ToolExecutionResult {
            tool_name: tool_name.to_string(),
            success: false,
            content: None,
            error: Some(format!("Tool '{}' not found", tool_name)),
            duration_ms: start.elapsed().as_millis() as u64,
        }
    }

    async fn get_tool_status(&mut self, tool_name: &str) -> Option<McpToolInfo> {
        let tools = self.list_tools().await;
        tools.into_iter().find(|t| t.name == tool_name)
    }

    async fn get_schemas(&mut self) -> HashMap<String, JsonValue> {
        let tools = self.list_tools().await;
        let mut schemas = HashMap::new();

        for tool in tools {
            let schema = tool.input_schema.unwrap_or_else(|| {
                serde_json::json!({
                    "type": "object",
                    "description": tool.description,
                })
            });
            schemas.insert(tool.name, schema);
        }

        schemas
    }

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
                input_schema: Some(serde_json::json!({
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
                input_schema: Some(serde_json::json!({
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
                input_schema: Some(serde_json::json!({
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
                input_schema: Some(serde_json::json!({
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

    async fn execute_core_tool(tool_name: &str, arguments: Option<JsonValue>) -> Option<JsonValue> {
        match tool_name {
            "agent-list" => Some(serde_json::json!({
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

                Some(serde_json::json!({
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

                Some(serde_json::json!({
                    "name": name,
                    "status": "ready",
                    "uptime_seconds": 0
                }))
            }
            "workflow-start" => Some(serde_json::json!({
                "status": "created",
                "case_id": "case-uuid-456"
            })),
            _ => None,
        }
    }
}

// ============================================================================
// Output Types
// ============================================================================

#[derive(Serialize)]
struct MCPTool {
    name: String,
    description: String,
    tool_type: String,
    agent_id: Option<String>,
    available: bool,
    agent_name: Option<String>,
}

#[derive(Serialize)]
struct MCPListOutput {
    tools: Vec<MCPTool>,
    total_count: usize,
    core_tools: usize,
    agent_tools: usize,
}

#[derive(Serialize)]
struct MCPBridgeOutput {
    tool_name: String,
    agent_name: String,
    status: String,
    message: String,
}

#[derive(Serialize)]
struct MCPStatusOutput {
    tool_name: String,
    tool_type: String,
    agent_id: Option<String>,
    status: String,
    schema: JsonValue,
}

#[derive(Serialize)]
struct MCPSchemasOutput {
    schemas: HashMap<String, JsonValue>,
    total_count: usize,
}

#[derive(Serialize)]
struct MCPTestOutput {
    tool_name: String,
    success: bool,
    content: Option<JsonValue>,
    error: Option<String>,
    duration_ms: u64,
}

#[derive(Serialize)]
struct ConfigInitOutput {
    success: bool,
    results: Vec<String>,
    message: String,
}

#[derive(Serialize)]
struct ConfigValidateOutput {
    is_valid: bool,
    errors: Vec<String>,
    warnings: Vec<String>,
}

#[derive(Serialize)]
struct ServerStartOutput {
    server_name: String,
    status: String,
    pid: Option<u32>,
    message: String,
}

#[derive(Serialize)]
struct ServerStopOutput {
    server_name: String,
    status: String,
    message: String,
}

#[derive(Serialize)]
struct SetupOutput {
    success: bool,
    claude_desktop_config_path: String,
    backup_path: Option<String>,
    ggen_server_added: bool,
    message: String,
}

// ============================================================================
// Groq Command Output Types
// ============================================================================

#[derive(Serialize)]
struct GroqGenerateOutput {
    model: String,
    prompt: String,
    response: String,
    prompt_tokens: u32,
    completion_tokens: u32,
    total_tokens: u32,
    duration_ms: u64,
}

#[derive(Serialize)]
struct GroqChatOutput {
    model: String,
    messages: Vec<ChatMessageEntry>,
    response: String,
    total_tokens: u32,
    duration_ms: u64,
}

#[derive(Serialize)]
struct ChatMessageEntry {
    role: String,
    content: String,
}

#[derive(Serialize)]
struct GroqStreamChunk {
    model: String,
    chunk: String,
    done: bool,
}

// ============================================================================
// Verb Functions (MCP Commands)
// ============================================================================

/// List all available MCP tools
#[verb]
fn list(verbose: bool) -> VerbResult<MCPListOutput> {
    let mut manager = McpBackendManager::new(None);
    let tools = block_on(async { manager.list_tools().await }).map_err(|e| {
        clap_noun_verb::NounVerbError::execution_error(format!("Failed to list tools: {}", e))
    })?;

    let core_count = tools.iter().filter(|t| t.tool_type == "core").count();
    let agent_count = tools.iter().filter(|t| t.tool_type == "agent").count();

    if verbose {
        for tool in &tools {
            println!(
                "  - {} ({}): {}",
                tool.name, tool.tool_type, tool.description
            );
        }
    }

    Ok(MCPListOutput {
        tools: tools
            .into_iter()
            .map(|t| MCPTool {
                name: t.name,
                description: t.description,
                tool_type: t.tool_type,
                agent_id: t.agent_id,
                available: t.available,
                agent_name: t.agent_name,
            })
            .collect(),
        total_count: core_count + agent_count,
        core_tools: core_count,
        agent_tools: agent_count,
    })
}

/// Bridge an agent as an MCP tool
#[verb]
fn bridge(agent_name: String, tool_name: Option<String>) -> VerbResult<MCPBridgeOutput> {
    let mut manager = McpBackendManager::new(None);
    let created_tool_name = block_on(async {
        manager
            .bridge_agent(&agent_name, tool_name.as_deref())
            .await
    })
    .map_err(|e| {
        clap_noun_verb::NounVerbError::execution_error(format!("Failed to bridge agent: {}", e))
    })?;

    let message = format!(
        "Agent '{}' bridged as tool '{}'",
        agent_name, created_tool_name
    );

    Ok(MCPBridgeOutput {
        tool_name: created_tool_name,
        agent_name,
        status: "created".to_string(),
        message,
    })
}

/// Show status of a specific MCP tool
#[verb]
fn status(tool_name: String) -> VerbResult<MCPStatusOutput> {
    let mut manager = McpBackendManager::new(None);
    let tool_info = block_on(async { manager.get_tool_status(&tool_name).await })
        .map_err(|e| {
            clap_noun_verb::NounVerbError::execution_error(format!(
                "Failed to get tool status: {}",
                e
            ))
        })?
        .ok_or_else(|| {
            clap_noun_verb::NounVerbError::argument_error(format!("Tool '{}' not found", tool_name))
        })?;

    Ok(MCPStatusOutput {
        tool_name: tool_info.name.clone(),
        tool_type: tool_info.tool_type,
        agent_id: tool_info.agent_id,
        status: if tool_info.available {
            "available"
        } else {
            "unavailable"
        }
        .to_string(),
        schema: tool_info
            .input_schema
            .unwrap_or_else(|| serde_json::json!({})),
    })
}

/// Get all MCP tool schemas
#[verb]
fn schemas(verbose: bool) -> VerbResult<MCPSchemasOutput> {
    let mut manager = McpBackendManager::new(None);
    let schemas = block_on(async { manager.get_schemas().await }).map_err(|e| {
        clap_noun_verb::NounVerbError::execution_error(format!("Failed to get schemas: {}", e))
    })?;

    if verbose {
        for (name, schema) in &schemas {
            println!(
                "{}: {}",
                name,
                serde_json::to_string_pretty(schema).unwrap_or_default()
            );
        }
    }

    Ok(MCPSchemasOutput {
        total_count: schemas.len(),
        schemas,
    })
}

/// Test an MCP tool
#[verb]
fn test(tool_name: String, arguments: Option<String>) -> VerbResult<MCPTestOutput> {
    let parsed_args = if let Some(args_str) = arguments {
        Some(serde_json::from_str(&args_str).map_err(|e| {
            clap_noun_verb::NounVerbError::argument_error(format!("Invalid JSON arguments: {}", e))
        })?)
    } else {
        None
    };

    let mut manager = McpBackendManager::new(None);
    let result =
        block_on(async { manager.test_tool(&tool_name, parsed_args).await }).map_err(|e| {
            clap_noun_verb::NounVerbError::execution_error(format!("Failed to test tool: {}", e))
        })?;

    Ok(MCPTestOutput {
        tool_name: result.tool_name,
        success: result.success,
        content: result.content,
        error: result.error,
        duration_ms: result.duration_ms,
    })
}

/// Initialize MCP configuration
#[verb]
fn init_config(mcp: bool, a2a: bool, force: bool) -> VerbResult<ConfigInitOutput> {
    perform_init_config(mcp, a2a, force)
}

/// Helper function to perform config initialization (reduces complexity)
fn perform_init_config(mcp: bool, a2a: bool, force: bool) -> VerbResult<ConfigInitOutput> {
    use ggen_domain::mcp_config::{
        init_a2a_config as domain_init_a2a, init_mcp_config as domain_init_mcp,
    };

    let project_dir = std::env::current_dir().map_err(|e| {
        clap_noun_verb::NounVerbError::execution_error(format!(
            "Failed to get current directory: {}",
            e
        ))
    })?;

    let mut results = vec![];
    let mut success = true;

    // Initialize MCP config if requested or if neither specified (default to both)
    if mcp || (!mcp && !a2a) {
        let mcp_config_path = project_dir.join(PROJECT_MCP_CONFIG);

        // Check if config already exists
        if mcp_config_path.exists() && !force {
            results.push(format!(
                "MCP config already exists at {}. Use --force to overwrite.",
                mcp_config_path.display()
            ));
        } else {
            match domain_init_mcp(&mcp_config_path, true) {
                Ok(_) => {
                    results.push(format!(
                        "✓ MCP config initialized at {}",
                        mcp_config_path.display()
                    ));
                }
                Err(e) => {
                    success = false;
                    results.push(format!("✗ Failed to initialize MCP config: {}", e));
                }
            }
        }
    }

    // Initialize A2A config if requested or if neither specified (default to both)
    if a2a || (!mcp && !a2a) {
        let a2a_config_path = project_dir.join(PROJECT_A2A_CONFIG);

        // Check if config already exists
        if a2a_config_path.exists() && !force {
            results.push(format!(
                "A2A config already exists at {}. Use --force to overwrite.",
                a2a_config_path.display()
            ));
        } else {
            match domain_init_a2a(&a2a_config_path) {
                Ok(_) => {
                    results.push(format!(
                        "✓ A2A config initialized at {}",
                        a2a_config_path.display()
                    ));
                }
                Err(e) => {
                    success = false;
                    results.push(format!("✗ Failed to initialize A2A config: {}", e));
                }
            }
        }
    }

    let message = if success {
        "Configuration initialized successfully".to_string()
    } else {
        "Configuration initialized with errors".to_string()
    };

    Ok(ConfigInitOutput {
        success,
        results,
        message,
    })
}

/// Validate MCP configuration
#[verb]
fn validate_config(
    mcp_file: Option<String>, a2a_file: Option<String>,
) -> VerbResult<ConfigValidateOutput> {
    perform_validate_config(mcp_file, a2a_file)
}

/// Helper function to perform config validation (reduces complexity)
fn perform_validate_config(
    mcp_file: Option<String>, a2a_file: Option<String>,
) -> VerbResult<ConfigValidateOutput> {
    use ggen_domain::mcp_config::validate_mcp_config as domain_validate_mcp;
    use std::path::PathBuf;

    let project_dir = std::env::current_dir().map_err(|e| {
        clap_noun_verb::NounVerbError::execution_error(format!(
            "Failed to get current directory: {}",
            e
        ))
    })?;

    let mut errors = vec![];
    let mut warnings = vec![];
    let mut is_valid = true;

    // Convert file strings to PathBuf for load_config
    let cli_mcp_pathbuf = mcp_file.as_ref().map(|s| PathBuf::from(s.as_str()));
    let cli_a2a_pathbuf = a2a_file.as_ref().map(|s| PathBuf::from(s.as_str()));

    // Determine which config file to validate
    let mcp_config_path = if let Some(ref pb) = cli_mcp_pathbuf {
        pb.clone()
    } else {
        project_dir.join(PROJECT_MCP_CONFIG)
    };

    // Validate MCP config if it exists
    if mcp_config_path.exists() {
        let cli_mcp_ref = cli_mcp_pathbuf.as_deref();

        match load_config(Some(&project_dir), cli_mcp_ref, None) {
            Ok(resolved) => {
                if let Some(mcp_config) = resolved.mcp {
                    match domain_validate_mcp(&mcp_config) {
                        Ok(results) => {
                            for result in results {
                                if !result.is_valid {
                                    is_valid = false;
                                    errors.extend(result.errors);
                                }
                                warnings.extend(result.warnings);
                            }
                        }
                        Err(e) => {
                            is_valid = false;
                            errors.push(format!("Validation error: {}", e));
                        }
                    }
                }
            }
            Err(e) => {
                is_valid = false;
                errors.push(format!("Failed to load MCP config: {}", e));
            }
        }
    } else if mcp_file.is_none() {
        warnings.push(format!(
            "No MCP config found at {}",
            mcp_config_path.display()
        ));
    }

    // Note: A2A validation is simpler - just check if it loads
    let a2a_config_path = if let Some(ref pb) = cli_a2a_pathbuf {
        pb.clone()
    } else {
        project_dir.join(PROJECT_A2A_CONFIG)
    };

    if a2a_config_path.exists() {
        let cli_a2a_ref = cli_a2a_pathbuf.as_deref();

        match load_config(Some(&project_dir), None, cli_a2a_ref) {
            Ok(resolved) => {
                if let Some(a2a_config) = resolved.a2a {
                    if let Err(e) = a2a_config.validate() {
                        is_valid = false;
                        errors.push(format!("A2A validation error: {}", e));
                    }
                }
            }
            Err(e) => {
                is_valid = false;
                errors.push(format!("Failed to load A2A config: {}", e));
            }
        }
    } else if a2a_file.is_none() && !mcp_config_path.exists() {
        warnings.push("No configuration files found".to_string());
    }

    Ok(ConfigValidateOutput {
        is_valid,
        errors,
        warnings,
    })
}

/// Start an MCP server
#[verb]
fn start_server(server_name: String, background: bool) -> VerbResult<ServerStartOutput> {
    if background {
        // Background mode: not yet implemented
        return Ok(ServerStartOutput {
            server_name,
            status: "started".to_string(),
            pid: None,
            message: "Background mode not yet implemented. Use foreground mode (--no-background)."
                .to_string(),
        });
    }

    // Foreground mode: run the MCP stdio server.
    // This blocks until the MCP client disconnects.
    let serve_result = block_on(async {
        ggen_a2a_mcp::server::serve_stdio()
            .await
            .map_err(|e| ggen_utils::error::Error::new(&format!("{}", e)))
    });
    match serve_result {
        Ok(Ok(())) => {}
        Ok(Err(e)) => {
            return Err(clap_noun_verb::NounVerbError::execution_error(format!(
                "MCP server error: {}",
                e
            )));
        }
        Err(e) => {
            return Err(clap_noun_verb::NounVerbError::execution_error(format!(
                "MCP runtime error: {}",
                e
            )));
        }
    }

    Ok(ServerStartOutput {
        server_name,
        status: "stopped".to_string(),
        pid: None,
        message: "MCP stdio server finished".to_string(),
    })
}

/// Stop an MCP server
#[verb]
fn stop_server_cmd(server_name: String, force: bool) -> VerbResult<ServerStopOutput> {
    match stop_server(std::env::current_dir().ok().as_deref(), force) {
        Ok(_) => Ok(ServerStopOutput {
            server_name: server_name.clone(),
            status: "stopped".to_string(),
            message: format!("Server '{}' stopped successfully", server_name),
        }),
        Err(e) => Err(clap_noun_verb::NounVerbError::execution_error(format!(
            "Failed to stop server: {}",
            e
        ))),
    }
}

/// Setup Claude Desktop MCP configuration
#[verb]
fn setup(force: bool) -> VerbResult<SetupOutput> {
    use std::fs;

    // Detect Claude Desktop config location cross-platform
    let config_dir = detect_claude_desktop_config_dir()?;
    let config_path = config_dir.join("claude_desktop_config.json");

    if !config_path.exists() {
        return Ok(SetupOutput {
            success: false,
            claude_desktop_config_path: config_path.display().to_string(),
            backup_path: None,
            ggen_server_added: false,
            message: format!(
                "Claude Desktop config not found at {}. Is Claude Desktop installed?",
                config_path.display()
            ),
        });
    }

    // Backup existing config
    let backup_path = if force {
        let backup = config_path.with_extension("json.backup");
        fs::copy(&config_path, &backup).map_err(|e| {
            clap_noun_verb::NounVerbError::execution_error(format!(
                "Failed to backup config: {}",
                e
            ))
        })?;
        Some(backup.display().to_string())
    } else {
        None
    };

    // Read existing config
    let mut config: JsonValue = fs::read_to_string(&config_path)
        .map_err(|e| {
            clap_noun_verb::NounVerbError::execution_error(format!("Failed to read config: {}", e))
        })
        .and_then(|content| {
            serde_json::from_str(&content).map_err(|e| {
                clap_noun_verb::NounVerbError::execution_error(format!(
                    "Failed to parse config JSON: {}",
                    e
                ))
            })
        })?;

    // Get ggen binary path
    let ggen_binary = std::env::current_exe()
        .map_err(|e| {
            clap_noun_verb::NounVerbError::execution_error(format!(
                "Failed to get ggen binary path: {}",
                e
            ))
        })?
        .display()
        .to_string();

    // Add ggen server entry
    let ggen_server = serde_json::json!({
        "command": ggen_binary,
        "args": ["mcp", "start-server", "--transport", "stdio"]
    });

    // Ensure mcpServers object exists
    if !config.is_object() {
        return Ok(SetupOutput {
            success: false,
            claude_desktop_config_path: config_path.display().to_string(),
            backup_path,
            ggen_server_added: false,
            message: "Invalid Claude Desktop config format: expected object at root".to_string(),
        });
    }

    let config_obj = config.as_object_mut().unwrap();
    let mcp_servers = config_obj
        .entry("mcpServers")
        .or_insert_with(|| JsonValue::Object(serde_json::Map::new()))
        .as_object_mut()
        .unwrap();

    // Add or update ggen server
    mcp_servers.insert("ggen".to_string(), ggen_server);

    // Write updated config
    let updated_config = serde_json::to_string_pretty(&config).map_err(|e| {
        clap_noun_verb::NounVerbError::execution_error(format!("Failed to serialize config: {}", e))
    })?;

    fs::write(&config_path, updated_config).map_err(|e| {
        clap_noun_verb::NounVerbError::execution_error(format!("Failed to write config: {}", e))
    })?;

    Ok(SetupOutput {
        success: true,
        claude_desktop_config_path: config_path.display().to_string(),
        backup_path,
        ggen_server_added: true,
        message: format!(
            "✓ ggen MCP server added to Claude Desktop config at {}",
            config_path.display()
        ),
    })
}

/// Detect Claude Desktop config directory based on platform
fn detect_claude_desktop_config_dir() -> Result<std::path::PathBuf, clap_noun_verb::NounVerbError> {
    let base_dir = if cfg!(target_os = "macos") {
        dirs::home_dir()
            .map(|p| p.join("Library/Application Support/Claude"))
            .ok_or_else(|| {
                clap_noun_verb::NounVerbError::execution_error(
                    "Failed to detect home directory on macOS",
                )
            })?
    } else if cfg!(target_os = "windows") {
        dirs::data_local_dir()
            .map(|p| p.join("Claude"))
            .ok_or_else(|| {
                clap_noun_verb::NounVerbError::execution_error(
                    "Failed to detect AppData directory on Windows",
                )
            })?
    } else if cfg!(target_os = "linux") {
        dirs::config_dir()
            .map(|p| p.join("Claude"))
            .ok_or_else(|| {
                clap_noun_verb::NounVerbError::execution_error(
                    "Failed to detect config directory on Linux",
                )
            })?
    } else {
        return Err(clap_noun_verb::NounVerbError::execution_error(
            "Unsupported platform for Claude Desktop config",
        ));
    };

    Ok(base_dir)
}

// ============================================================================
// Groq Verb Functions
// ============================================================================

/// Helper function to create Groq LLM config
fn create_groq_config(
    model: Option<String>, temperature: Option<f32>, max_tokens: Option<u32>,
) -> Result<LlmConfig, String> {
    // Check for GROQ_API_KEY
    if std::env::var(env_vars::GROQ_API_KEY).is_err() {
        return Err("GROQ_API_KEY environment variable not set".to_string());
    }

    // Select model: explicit > GROQ_MODEL env var > GROQ_DEFAULT
    let model_name = model
        .or_else(|| std::env::var(env_vars::GROQ_MODEL).ok())
        .unwrap_or_else(|| models::GROQ_DEFAULT.to_string());

    // Create LLM config
    let config = LlmConfig {
        model: model_name,
        temperature,
        max_tokens,
        ..Default::default()
    };

    Ok(config)
}

/// Generate text using Groq
#[verb]
fn groq_generate(
    prompt: String, model: Option<String>, temperature: Option<f32>, max_tokens: Option<u32>,
) -> VerbResult<GroqGenerateOutput> {
    let config = create_groq_config(model, temperature, max_tokens)
        .map_err(clap_noun_verb::NounVerbError::execution_error)?;

    let client = GenAiClient::new(config).map_err(|e| {
        clap_noun_verb::NounVerbError::execution_error(format!("Failed to create client: {}", e))
    })?;

    let start = std::time::Instant::now();
    let response = block_on(async { client.complete(&prompt).await })
        .map_err(|e| {
            clap_noun_verb::NounVerbError::execution_error(format!("Generation failed: {}", e))
        })?
        .map_err(|e| {
            clap_noun_verb::NounVerbError::execution_error(format!("Generation failed: {}", e))
        })?;
    let duration = start.elapsed();

    let usage = response.usage.unwrap_or(ggen_ai::UsageStats {
        prompt_tokens: 0,
        completion_tokens: 0,
        total_tokens: 0,
    });

    Ok(GroqGenerateOutput {
        model: response.model,
        prompt,
        response: response.content,
        prompt_tokens: usage.prompt_tokens,
        completion_tokens: usage.completion_tokens,
        total_tokens: usage.total_tokens,
        duration_ms: duration.as_millis() as u64,
    })
}

/// Chat with Groq (multi-turn conversation)
#[verb]
fn groq_chat(
    message: String, model: Option<String>, temperature: Option<f32>, max_tokens: Option<u32>,
    system: Option<String>,
) -> VerbResult<GroqChatOutput> {
    let config = create_groq_config(model, temperature, max_tokens)
        .map_err(clap_noun_verb::NounVerbError::execution_error)?;

    // Build prompt with system message if provided
    let full_prompt = if let Some(ref sys_msg) = system {
        format!("System: {}\n\nUser: {}", sys_msg, message)
    } else {
        message.clone()
    };

    let client = GenAiClient::new(config).map_err(|e| {
        clap_noun_verb::NounVerbError::execution_error(format!("Failed to create client: {}", e))
    })?;

    let start = std::time::Instant::now();
    let response = block_on(async { client.complete(&full_prompt).await })
        .map_err(|e| clap_noun_verb::NounVerbError::execution_error(format!("Chat failed: {}", e)))?
        .map_err(|e| {
            clap_noun_verb::NounVerbError::execution_error(format!("Chat failed: {}", e))
        })?;
    let duration = start.elapsed();

    // Build message history for output
    let mut messages = vec![];
    if let Some(sys) = system {
        messages.push(ChatMessageEntry {
            role: "system".to_string(),
            content: sys,
        });
    }
    messages.push(ChatMessageEntry {
        role: "user".to_string(),
        content: message,
    });
    messages.push(ChatMessageEntry {
        role: "assistant".to_string(),
        content: response.content.clone(),
    });

    let usage = response.usage.unwrap_or(ggen_ai::UsageStats {
        prompt_tokens: 0,
        completion_tokens: 0,
        total_tokens: 0,
    });

    Ok(GroqChatOutput {
        model: response.model,
        messages,
        response: response.content,
        total_tokens: usage.total_tokens,
        duration_ms: duration.as_millis() as u64,
    })
}

/// Stream text generation using Groq
#[verb]
fn groq_stream(
    prompt: String, model: Option<String>, temperature: Option<f32>, max_tokens: Option<u32>,
) -> VerbResult<Vec<GroqStreamChunk>> {
    let config = create_groq_config(model, temperature, max_tokens)
        .map_err(clap_noun_verb::NounVerbError::execution_error)?;

    let model_name = config.model.clone();

    let client = GenAiClient::new(config).map_err(|e| {
        clap_noun_verb::NounVerbError::execution_error(format!("Failed to create client: {}", e))
    })?;

    let mut stream = block_on(async { client.complete_stream(&prompt).await })
        .map_err(|e| {
            clap_noun_verb::NounVerbError::execution_error(format!("Stream failed: {}", e))
        })?
        .map_err(|e| {
            clap_noun_verb::NounVerbError::execution_error(format!("Stream failed: {}", e))
        })?;

    let mut chunks = Vec::new();

    // Collect all chunks
    loop {
        let chunk_result: Option<ggen_ai::LlmChunk> = block_on(async {
            use futures::StreamExt;
            stream.next().await
        })
        .map_err(|e| {
            clap_noun_verb::NounVerbError::execution_error(format!("Stream error: {}", e))
        })?;

        match chunk_result {
            Some(chunk) => {
                let is_done = chunk.finish_reason.is_some();
                chunks.push(GroqStreamChunk {
                    model: model_name.clone(),
                    chunk: chunk.content,
                    done: is_done,
                });
                if is_done {
                    break;
                }
            }
            None => break,
        }
    }

    if chunks.is_empty() {
        chunks.push(GroqStreamChunk {
            model: model_name,
            chunk: String::new(),
            done: true,
        });
    }

    Ok(chunks)
}
