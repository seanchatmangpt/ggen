//! MCP Tool Agent Example
//!
//! This example demonstrates how MCP (Model Context Protocol) tools can be used as agents,
//! providing bidirectional communication between MCP tools and the A2A (Agent-to-Agent) protocol.
//! Shows tool registration, execution, and monitoring.
//!
//! ## What This Demonstrates
//!
//! - MCP Tool definition with input/output schemas
//! - Agent system for tool execution
//! - Bidirectional protocol manager for A2A communication
//! - Performance metrics tracking
//! - Execution history management
//!
//! ## How to Run
//!
//! ```bash
//! cargo run --example mcp_tool_agent
//! ```
//!
//! ## Expected Output
//!
//! The example will:
//! 1. Create MCP tools (data_processor, model_trainer)
//! 2. Register them with different agents
//! 3. Execute tools via the bidirectional protocol
//! 4. Display performance metrics and execution history

use std::collections::HashMap;
use std::time::Duration;
use tokio::sync::mpsc;

/// MCP Tool definition with Clone support for registry operations
#[derive(Debug, Clone)]
pub struct MCPTool {
    pub name: String,
    pub description: String,
    pub input_schema: ToolSchema,
    pub output_schema: ToolSchema,
}

/// Tool schema definition with Clone support
#[derive(Debug, Clone)]
pub struct ToolSchema {
    pub properties: HashMap<String, SchemaProperty>,
    pub required: Vec<String>,
    pub type_: String,
}

/// Schema property definition with Clone support
#[derive(Debug, Clone)]
pub struct SchemaProperty {
    pub type_: String,
    pub description: String,
    pub default: Option<String>,
}

/// MCP Agent System
pub struct McpAgentSystem {
    agent_id: String,
    tools: HashMap<String, MCPTool>,
    communication_channel: mpsc::Sender<A2ACommand>,
}

/// A2A command types
#[derive(Debug, Clone)]
pub enum A2ACommand {
    ExecuteTool { tool_name: String, arguments: HashMap<String, String> },
    GetToolInfo { tool_name: String },
    ListTools {},
}

/// MCP Tool result with Clone support for execution history
#[derive(Debug, Clone)]
pub struct ToolResult {
    pub tool_name: String,
    pub output: String,
    pub artifacts: Vec<String>,
    pub execution_time_ms: u64,
    pub success: bool,
}

/// Bidirectional protocol manager
pub struct BidirectionalProtocol {
    mcp_tools: HashMap<String, MCPTool>,
    a2a_registry: HashMap<String, String>, // tool_name -> agent_id
    execution_history: Vec<ExecutionRecord>,
}

/// Execution record
pub struct ExecutionRecord {
    pub id: String,
    pub tool_name: String,
    pub agent_id: String,
    pub timestamp: std::time::Instant,
    pub duration: Duration,
    pub result: ToolResult,
}

impl McpAgentSystem {
    /// Create new MCP agent system
    pub fn new(agent_id: String) -> Self {
        Self {
            agent_id,
            tools: HashMap::new(),
            communication_channel: mpsc::channel(100).0,
        }
    }

    /// Register an MCP tool
    pub fn register_tool(&mut self, tool: MCPTool) {
        self.tools.insert(tool.name.clone(), tool);
        println!("Registered tool: {} for agent: {}", self.tools.len(), self.agent_id);
    }

    /// Execute tool via A2A protocol
    pub async fn execute_tool(
        &self,
        tool_name: &str,
        arguments: HashMap<String, String>,
    ) -> Result<ToolResult, Box<dyn std::error::Error>> {
        let tool = self.tools.get(tool_name)
            .ok_or_else(|| format!("Tool {} not found", tool_name))?;

        // Validate arguments
        self.validate_arguments(tool, &arguments)?;

        println!("Executing tool: {} with agent: {}", tool_name, self.agent_id);

        // Simulate tool execution
        let start_time = std::time::Instant::now();
        tokio::time::sleep(Duration::from_millis(500)).await;

        // Generate result based on tool type
        let result = match tool_name {
            "data_processor" => {
                ToolResult {
                    tool_name: tool_name.to_string(),
                    output: "Data processed successfully".to_string(),
                    artifacts: vec!["data_output.json".to_string()],
                    execution_time_ms: start_time.elapsed().as_millis() as u64,
                    success: true,
                }
            },
            "model_trainer" => {
                ToolResult {
                    tool_name: tool_name.to_string(),
                    output: "Model trained with 95% accuracy".to_string(),
                    artifacts: vec!["model.bin".to_string(), "metrics.json".to_string()],
                    execution_time_ms: start_time.elapsed().as_millis() as u64,
                    success: true,
                }
            },
            "file_generator" => {
                ToolResult {
                    tool_name: tool_name.to_string(),
                    output: "File generated successfully".to_string(),
                    artifacts: vec!["output.txt".to_string()],
                    execution_time_ms: start_time.elapsed().as_millis() as u64,
                    success: true,
                }
            },
            _ => {
                ToolResult {
                    tool_name: tool_name.to_string(),
                    output: "Tool executed successfully".to_string(),
                    artifacts: vec![],
                    execution_time_ms: start_time.elapsed().as_millis() as u64,
                    success: true,
                }
            }
        };

        println!("Tool execution completed in {}ms", result.execution_time_ms);
        Ok(result)
    }

    /// Validate tool arguments
    fn validate_arguments(&self, tool: &MCPTool, arguments: &HashMap<String, String>) -> Result<(), Box<dyn std::error::Error>> {
        for required_arg in &tool.input_schema.required {
            if !arguments.contains_key(required_arg) {
                return Err(format!("Missing required argument: {}", required_arg).into());
            }
        }
        Ok(())
    }

    /// Get tool info
    pub fn get_tool_info(&self, tool_name: &str) -> Result<&MCPTool, Box<dyn std::error::Error>> {
        self.tools.get(tool_name)
            .ok_or_else(|| format!("Tool {} not found", tool_name).into())
    }

    /// List all available tools
    pub fn list_tools(&self) -> Vec<&str> {
        self.tools.keys().map(|s| s.as_str()).collect()
    }
}

impl BidirectionalProtocol {
    /// Create new bidirectional protocol manager
    pub fn new() -> Self {
        Self {
            mcp_tools: HashMap::new(),
            a2a_registry: HashMap::new(),
            execution_history: Vec::new(),
        }
    }

    /// Register MCP tool with A2A mapping
    pub fn register_tool(&mut self, tool: MCPTool, agent_id: &str) -> Result<(), Box<dyn std::error::Error>> {
        let tool_name = tool.name.clone();
        self.mcp_tools.insert(tool_name.clone(), tool);
        self.a2a_registry.insert(tool_name.clone(), agent_id.to_string());
        println!("Registered MCP tool {} to agent {}", tool_name, agent_id);
        Ok(())
    }

    /// Execute tool via bidirectional protocol
    pub async fn execute_bidirectional(
        &mut self,
        tool_name: &str,
        arguments: HashMap<String, String>,
    ) -> Result<ToolResult, Box<dyn std::error::Error>> {
        let agent_id = self.a2a_registry.get(tool_name)
            .ok_or_else(|| format!("Tool {} not registered to any agent", tool_name))?.clone();

        println!("Executing {} via A2A protocol through agent: {}", tool_name, agent_id);

        // Create MCP agent system
        let mut agent_system = McpAgentSystem::new(agent_id.clone());

        // Register all tools for this agent
        for (name, tool) in &self.mcp_tools {
            if self.a2a_registry.get(name).map(|s| s.as_str()) == Some(agent_id.as_str()) {
                agent_system.register_tool(tool.clone());
            }
        }

        // Execute tool
        let result = agent_system.execute_tool(tool_name, arguments).await?;

        // Record execution
        let record = ExecutionRecord {
            id: uuid::Uuid::new_v4().to_string(),
            tool_name: tool_name.to_string(),
            agent_id: agent_id.clone(),
            timestamp: std::time::Instant::now(),
            duration: Duration::from_millis(result.execution_time_ms),
            result: result.clone(),
        };

        self.execution_history.push(record);

        Ok(result)
    }

    /// Get agent tool capabilities
    pub fn get_agent_capabilities(&self, agent_id: &str) -> Vec<&str> {
        self.mcp_tools.keys()
            .filter(|tool| self.a2a_registry.get(*tool).map(|s| s.as_str()) == Some(agent_id))
            .map(|s| s.as_str())
            .collect()
    }

    /// Get execution history
    pub fn get_execution_history(&self) -> &Vec<ExecutionRecord> {
        &self.execution_history
    }

    /// Get performance metrics
    pub fn get_performance_metrics(&self) -> PerformanceMetrics {
        if self.execution_history.is_empty() {
            return PerformanceMetrics {
                total_executions: 0,
                average_execution_time_ms: 0.0,
                success_rate: 0.0,
                most_used_tool: None,
            };
        }

        let total = self.execution_history.len() as u64;
        let total_time_ms: u64 = self.execution_history.iter()
            .map(|r| r.duration.as_millis() as u64)
            .sum();
        let average_time = total_time_ms as f64 / total as f64;

        let successful = self.execution_history.iter()
            .filter(|r| r.result.success)
            .count() as u64;
        let success_rate = successful as f64 / total as f64 * 100.0;

        // Count tool usage
        let mut tool_counts: HashMap<String, usize> = HashMap::new();
        for record in &self.execution_history {
            *tool_counts.entry(record.tool_name.clone()).or_insert(0) += 1;
        }
        let most_used_tool = tool_counts.into_iter()
            .max_by_key(|(_, count)| *count)
            .map(|(tool, _)| tool);

        PerformanceMetrics {
            total_executions: total,
            average_execution_time_ms: average_time,
            success_rate,
            most_used_tool,
        }
    }
}

/// Performance metrics
pub struct PerformanceMetrics {
    pub total_executions: u64,
    pub average_execution_time_ms: f64,
    pub success_rate: f64,
    pub most_used_tool: Option<String>,
}

#[tokio::main]
async fn main() -> Result<(), Box<dyn std::error::Error>> {
    println!("MCP Tool Agent Example");
    println!("========================");

    // Create bidirectional protocol
    let mut protocol = BidirectionalProtocol::new();

    // Define MCP tools
    let tools = vec![
        MCPTool {
            name: "data_processor".to_string(),
            description: "Process and transform data".to_string(),
            input_schema: ToolSchema {
                properties: {
                    let mut props = HashMap::new();
                    props.insert("input_file".to_string(), SchemaProperty {
                        type_: "string".to_string(),
                        description: "Input file path".to_string(),
                        default: None,
                    });
                    props.insert("operation".to_string(), SchemaProperty {
                        type_: "string".to_string(),
                        description: "Operation to perform".to_string(),
                        default: Some("transform".to_string()),
                    });
                    props
                },
                required: vec!["input_file".to_string()],
                type_: "object".to_string(),
            },
            output_schema: ToolSchema {
                properties: {
                    let mut props = HashMap::new();
                    props.insert("output_file".to_string(), SchemaProperty {
                        type_: "string".to_string(),
                        description: "Output file path".to_string(),
                        default: None,
                    });
                    props
                },
                required: vec!["output_file".to_string()],
                type_: "object".to_string(),
            },
        },
        MCPTool {
            name: "model_trainer".to_string(),
            description: "Train machine learning models".to_string(),
            input_schema: ToolSchema {
                properties: {
                    let mut props = HashMap::new();
                    props.insert("dataset".to_string(), SchemaProperty {
                        type_: "string".to_string(),
                        description: "Dataset path".to_string(),
                        default: None,
                    });
                    props.insert("algorithm".to_string(), SchemaProperty {
                        type_: "string".to_string(),
                        description: "ML algorithm".to_string(),
                        default: Some("random_forest".to_string()),
                    });
                    props
                },
                required: vec!["dataset".to_string()],
                type_: "object".to_string(),
            },
            output_schema: ToolSchema {
                properties: {
                    let mut props = HashMap::new();
                    props.insert("model_path".to_string(), SchemaProperty {
                        type_: "string".to_string(),
                        description: "Trained model path".to_string(),
                        default: None,
                    });
                    props.insert("accuracy".to_string(), SchemaProperty {
                        type_: "number".to_string(),
                        description: "Model accuracy".to_string(),
                        default: None,
                    });
                    props
                },
                required: vec!["model_path".to_string()],
                type_: "object".to_string(),
            },
        },
    ];

    // Register tools with different agents
    for tool in tools {
        let agent_id = match tool.name.as_str() {
            "data_processor" => "data-agent".to_string(),
            "model_trainer" => "ml-agent".to_string(),
            _ => "default-agent".to_string(),
        };
        protocol.register_tool(tool, &agent_id)?;
    }

    // Execute tools via bidirectional protocol
    println!("\nExecuting tools via bidirectional protocol...");

    // Execute data processor
    let data_args = vec![
        ("input_file".to_string(), "/data/input.csv".to_string()),
        ("operation".to_string(), "clean".to_string()),
    ].into_iter().collect();

    let data_result = protocol.execute_bidirectional("data_processor", data_args).await?;
    println!("Data processor result: {}", data_result.output);

    // Execute model trainer
    let model_args = vec![
        ("dataset".to_string(), "/data/cleaned.csv".to_string()),
        ("algorithm".to_string(), "xgboost".to_string()),
    ].into_iter().collect();

    let model_result = protocol.execute_bidirectional("model_trainer", model_args).await?;
    println!("Model trainer result: {}", model_result.output);

    // Get agent capabilities
    println!("\nAgent capabilities:");
    for (agent, tools) in [
        ("data-agent", vec!["data_processor"]),
        ("ml-agent", vec!["model_trainer"]),
    ] {
        println!("  {}: {}", agent, tools.join(", "));
    }

    // Get performance metrics
    let metrics = protocol.get_performance_metrics();
    println!("\nPerformance metrics:");
    println!("  Total executions: {}", metrics.total_executions);
    println!("  Average time: {:.2}ms", metrics.average_execution_time_ms);
    println!("  Success rate: {:.1}%", metrics.success_rate);

    // Show execution history
    println!("\nExecution history:");
    for record in protocol.get_execution_history() {
        println!("  {}: {} via {} in {}ms",
            record.tool_name,
            if record.result.success { "OK" } else { "FAIL" },
            record.agent_id,
            record.duration.as_millis());
    }

    println!("\nBidirectional protocol example completed successfully!");
    Ok(())
}
