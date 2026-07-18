//! MCP Tool Quickstart Example
//!
//! This example demonstrates a simple MCP tool that validates project paths.
//! It shows the core concepts of MCP without the complexity of full server setup.
//!
//! Run: cargo run --bin mcp_tool_example

use serde::{Deserialize, Serialize};
use std::path::Path;

/// Tool parameters
#[derive(Debug, Deserialize)]
struct ValidateProjectParams {
    project_path: String,
}

/// Tool result
#[derive(Debug, Serialize)]
struct ValidationResult {
    project_path: String,
    is_valid: bool,
    checks_passed: usize,
    total_checks: usize,
    details: Vec<String>,
}

/// Simple MCP Tool
struct McpTool {
    name: String,
    description: String,
}

impl McpTool {
    fn new(name: impl Into<String>, description: impl Into<String>) -> Self {
        McpTool {
            name: name.into(),
            description: description.into(),
        }
    }

    /// Get tool metadata (equivalent to list_tools in MCP)
    fn metadata(&self) -> ToolMetadata {
        ToolMetadata {
            name: self.name.clone(),
            description: self.description.clone(),
            parameters: vec!["project_path".to_string()],
        }
    }

    /// Execute the tool (equivalent to call_tool in MCP)
    fn execute(&self, params: ValidateProjectParams) -> anyhow::Result<ToolResult> {
        if self.name != "validate_project" {
            return Err(anyhow::anyhow!("Unknown tool: {}", self.name));
        }

        println!("🔍 Validating project: {}", params.project_path);

        // Simulate validation checks
        let checks = vec![
            ("Cargo.toml exists", Path::new(&params.project_path).join("Cargo.toml").exists()),
            ("src/ directory exists", Path::new(&params.project_path).join("src").exists()),
            (".git/ directory exists", Path::new(&params.project_path).join(".git").exists()),
        ];

        let checks_passed = checks.iter().filter(|(_, passed)| *passed).count();
        let total_checks = checks.len();
        let is_valid = checks_passed == total_checks;

        let details: Vec<String> = checks
            .into_iter()
            .map(|(name, passed)| format!("{} {}", if passed { "✓" } else { "✗" }, name))
            .collect();

        let validation_result = ValidationResult {
            project_path: params.project_path,
            is_valid,
            checks_passed,
            total_checks,
            details,
        };

        Ok(ToolResult::Validation(validation_result))
    }
}

/// Tool metadata (MCP schema)
#[derive(Debug, Clone)]
struct ToolMetadata {
    name: String,
    description: String,
    parameters: Vec<String>,
}

/// Tool execution result
#[derive(Debug, Serialize)]
#[serde(untagged)]
enum ToolResult {
    Validation(ValidationResult),
}

impl ToolResult {
    /// Format result for display
    fn display(&self) -> String {
        match self {
            ToolResult::Validation(result) => {
                format!(
                    "Project Validation Results:\n\
                     Path: {}\n\
                     Status: {}\n\
                     Checks: {}/{}\n\
                     Details:\n  {}",
                    result.project_path,
                    if result.is_valid { "PASS ✓" } else { "FAIL ✗" },
                    result.checks_passed,
                    result.total_checks,
                    result.details.join("\n  ")
                )
            }
        }
    }
}

fn main() -> anyhow::Result<()> {
    println!("🚀 MCP Tool Quickstart Example");
    println!("===============================\n");

    // Create a simple MCP tool
    let tool = McpTool::new(
        "validate_project",
        "Validate a project path by checking for Cargo.toml, src/, and .git/",
    );
    println!("✅ Created MCP tool: {}", tool.name);

    // Get tool metadata
    let metadata = tool.metadata();
    println!("✅ Tool metadata:");
    println!("  Name: {}", metadata.name);
    println!("  Description: {}", metadata.description);
    println!("  Parameters: {:?}\n", metadata.parameters);

    // Execute tool with current directory
    let project_path = std::env::current_dir()?
        .parent()
        .unwrap()
        .parent()
        .unwrap()
        .to_str()
        .unwrap()
        .to_string();

    println!("🔨 Executing tool...");
    println!("📂 Path: {}\n", project_path);

    let params = ValidateProjectParams { project_path };
    match tool.execute(params) {
        Ok(result) => {
            println!("━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━");
            println!("TOOL RESULT");
            println!("━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━\n");
            println!("{}", result.display());
            println!("━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━");
        }
        Err(e) => {
            println!("❌ Error: {}", e);
        }
    }

    println!("\n✅ Example complete!");

    println!("\n💡 Key Concepts:");
    println!("  - MCP Tools expose functionality with structured parameters");
    println!("  - Tools return structured results (not plain strings)");
    println!("  - Tools can be listed (metadata) and called (execute)");
    println!("  - In a real MCP server, tools are exposed via MCP protocol");

    Ok(())
}
