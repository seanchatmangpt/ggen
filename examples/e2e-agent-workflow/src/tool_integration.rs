//! MCP tool discovery and execution

use serde::{Deserialize, Serialize};
use std::collections::HashMap;
use uuid::Uuid;

/// A tool available via MCP
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct ToolSpec {
    /// Tool ID
    pub id: String,
    /// Tool name
    pub name: String,
    /// Tool description
    pub description: String,
    /// Input parameters required
    pub parameters: Vec<String>,
    /// Return type
    pub return_type: String,
}

/// A call to a tool
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct ToolCall {
    /// Call ID
    pub id: Uuid,
    /// Tool being called
    pub tool: String,
    /// Arguments passed
    pub arguments: HashMap<String, String>,
    /// Called by agent
    pub caller_id: Uuid,
}

/// Result of tool execution
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct ToolResult {
    /// Call ID that produced this result
    pub call_id: Uuid,
    /// Tool that was called
    pub tool_name: String,
    /// Success or failure
    pub success: bool,
    /// Output value
    pub output: String,
    /// Error message if failed
    pub error: Option<String>,
}

/// Manages available tools and execution
pub struct ToolManager {
    tools: HashMap<String, ToolSpec>,
    call_history: Vec<ToolCall>,
    result_history: Vec<ToolResult>,
}

impl ToolManager {
    /// Create new tool manager
    pub fn new() -> Self {
        Self {
            tools: HashMap::new(),
            call_history: Vec::new(),
            result_history: Vec::new(),
        }
    }

    /// Register a tool
    pub fn register_tool(&mut self, tool: ToolSpec) {
        self.tools.insert(tool.name.clone(), tool);
    }

    /// Get all available tools
    pub fn available_tools(&self) -> Vec<&ToolSpec> {
        self.tools.values().collect()
    }

    /// Get tool by name
    pub fn get_tool(&self, name: &str) -> Option<&ToolSpec> {
        self.tools.get(name)
    }

    /// Record a tool call
    pub fn record_call(&mut self, call: ToolCall) {
        self.call_history.push(call);
    }

    /// Record a tool result
    pub fn record_result(&mut self, result: ToolResult) {
        self.result_history.push(result);
    }

    /// Get tool call history
    pub fn call_history(&self) -> &[ToolCall] {
        &self.call_history
    }

    /// Get result history
    pub fn result_history(&self) -> &[ToolResult] {
        &self.result_history
    }

    /// Get success rate for a tool
    pub fn tool_success_rate(&self, tool_name: &str) -> f64 {
        let results: Vec<_> = self
            .result_history
            .iter()
            .filter(|r| r.tool_name == tool_name)
            .collect();

        if results.is_empty() {
            return 0.0;
        }

        let successes = results.iter().filter(|r| r.success).count();
        (successes as f64 / results.len() as f64) * 100.0
    }

    /// Simulate tool execution
    pub fn execute_tool(&self, call: &ToolCall) -> ToolResult {
        match self.get_tool(&call.tool) {
            Some(tool) => {
                let success = !call.arguments.is_empty();
                ToolResult {
                    call_id: call.id,
                    tool_name: tool.name.clone(),
                    success,
                    output: if success {
                        format!("Tool {} executed successfully", tool.name)
                    } else {
                        String::new()
                    },
                    error: if !success {
                        Some("Missing required arguments".to_string())
                    } else {
                        None
                    },
                }
            }
            None => ToolResult {
                call_id: call.id,
                tool_name: call.tool.clone(),
                success: false,
                output: String::new(),
                error: Some("Tool not found".to_string()),
            },
        }
    }
}

impl Default for ToolManager {
    fn default() -> Self {
        Self::new()
    }
}

/// Initialize standard tools
pub fn initialize_standard_tools() -> ToolManager {
    let mut manager = ToolManager::new();

    manager.register_tool(ToolSpec {
        id: "search_web".to_string(),
        name: "search_web".to_string(),
        description: "Search the web for information".to_string(),
        parameters: vec!["query".to_string()],
        return_type: "SearchResults".to_string(),
    });

    manager.register_tool(ToolSpec {
        id: "get_time".to_string(),
        name: "get_time".to_string(),
        description: "Get current time".to_string(),
        parameters: vec![],
        return_type: "String".to_string(),
    });

    manager.register_tool(ToolSpec {
        id: "store_data".to_string(),
        name: "store_data".to_string(),
        description: "Store data in memory".to_string(),
        parameters: vec!["key".to_string(), "value".to_string()],
        return_type: "bool".to_string(),
    });

    manager.register_tool(ToolSpec {
        id: "retrieve_data".to_string(),
        name: "retrieve_data".to_string(),
        description: "Retrieve stored data".to_string(),
        parameters: vec!["key".to_string()],
        return_type: "String".to_string(),
    });

    manager
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_tool_registration() {
        let mut manager = ToolManager::new();
        let tool = ToolSpec {
            id: "test".to_string(),
            name: "test_tool".to_string(),
            description: "A test tool".to_string(),
            parameters: vec!["param1".to_string()],
            return_type: "String".to_string(),
        };
        manager.register_tool(tool);
        assert!(manager.get_tool("test_tool").is_some());
    }

    #[test]
    fn test_tool_call_recording() {
        let mut manager = ToolManager::new();
        let call = ToolCall {
            id: Uuid::new_v4(),
            tool: "test".to_string(),
            arguments: HashMap::new(),
            caller_id: Uuid::new_v4(),
        };
        manager.record_call(call.clone());
        assert_eq!(manager.call_history().len(), 1);
    }

    #[test]
    fn test_standard_tools_initialized() {
        let manager = initialize_standard_tools();
        assert!(manager.get_tool("search_web").is_some());
        assert!(manager.get_tool("get_time").is_some());
        assert!(manager.get_tool("store_data").is_some());
    }

    #[test]
    fn test_tool_execution() {
        let manager = initialize_standard_tools();
        let mut args = HashMap::new();
        args.insert("query".to_string(), "test".to_string());
        let call = ToolCall {
            id: Uuid::new_v4(),
            tool: "search_web".to_string(),
            arguments: args,
            caller_id: Uuid::new_v4(),
        };
        let result = manager.execute_tool(&call);
        assert!(result.success);
    }
}
