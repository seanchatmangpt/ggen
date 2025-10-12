//! Mock MCP server for testing without real servers or LLM APIs

use serde_json::json;

/// Mock MCP server that returns test tools
pub struct MockMcpServer;

impl MockMcpServer {
    /// Returns a mock tool list response
    pub fn mock_tools_list() -> serde_json::Value {
        json!({
            "tools": [
                {
                    "name": "read_file",
                    "description": "Read contents of a file",
                    "inputSchema": {
                        "type": "object",
                        "properties": {
                            "path": {
                                "type": "string",
                                "description": "Path to the file"
                            }
                        },
                        "required": ["path"]
                    }
                },
                {
                    "name": "write_file",
                    "description": "Write contents to a file",
                    "inputSchema": {
                        "type": "object",
                        "properties": {
                            "path": {
                                "type": "string",
                                "description": "Path to the file"
                            },
                            "content": {
                                "type": "string",
                                "description": "Content to write"
                            }
                        },
                        "required": ["path", "content"]
                    }
                },
                {
                    "name": "list_directory",
                    "description": "List files in a directory",
                    "inputSchema": {
                        "type": "object",
                        "properties": {
                            "path": {
                                "type": "string",
                                "description": "Directory path"
                            }
                        },
                        "required": ["path"]
                    }
                }
            ]
        })
    }

    /// Returns a mock tool call response
    pub fn mock_tool_call_response(tool_name: &str, args: serde_json::Value) -> serde_json::Value {
        match tool_name {
            "read_file" => json!({
                "content": [
                    {
                        "type": "text",
                        "text": "Mock file contents for path: {}",
                    }
                ]
            }),
            "write_file" => json!({
                "content": [
                    {
                        "type": "text",
                        "text": "Successfully wrote to file",
                    }
                ]
            }),
            "list_directory" => json!({
                "content": [
                    {
                        "type": "text",
                        "text": "file1.txt\nfile2.txt\nfile3.txt",
                    }
                ]
            }),
            _ => json!({
                "error": format!("Unknown tool: {}", tool_name)
            })
        }
    }

    /// Creates a mock stdio MCP server process
    /// This returns a command that just echoes the mock response
    pub fn mock_stdio_command() -> String {
        // On Unix, use echo to simulate MCP server
        if cfg!(unix) {
            "echo".to_string()
        } else {
            // On Windows, use cmd /c echo
            "cmd".to_string()
        }
    }

    pub fn mock_stdio_args() -> Vec<String> {
        if cfg!(unix) {
            vec![Self::mock_tools_list().to_string()]
        } else {
            vec!["/c".to_string(), "echo".to_string(), Self::mock_tools_list().to_string()]
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_mock_tools_list_has_three_tools() {
        let tools = MockMcpServer::mock_tools_list();
        let tools_array = tools["tools"].as_array().unwrap();
        assert_eq!(tools_array.len(), 3);
    }

    #[test]
    fn test_mock_tools_have_required_fields() {
        let tools = MockMcpServer::mock_tools_list();
        let first_tool = &tools["tools"][0];

        assert!(first_tool["name"].is_string());
        assert!(first_tool["description"].is_string());
        assert!(first_tool["inputSchema"].is_object());
    }

    #[test]
    fn test_mock_tool_call_returns_content() {
        let response = MockMcpServer::mock_tool_call_response(
            "read_file",
            json!({"path": "/test/file.txt"})
        );

        assert!(response["content"].is_array());
        assert!(response["content"][0]["type"] == "text");
    }

    #[test]
    fn test_mock_unknown_tool_returns_error() {
        let response = MockMcpServer::mock_tool_call_response(
            "unknown_tool",
            json!({})
        );

        assert!(response["error"].is_string());
    }
}
