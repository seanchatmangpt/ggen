///! CLI command execution helper for ggen-mcp
///!
///! This module provides utilities for calling the ggen CLI from MCP tools.
///! All MCP tools should delegate to the CLI instead of reimplementing logic.
use crate::error::{GgenMcpError, Result};
use serde_json::Value;
use std::process::Command;
use tracing::{debug, error, info};

/// Execute a ggen CLI command and return the JSON output
pub async fn call_ggen_cli(args: &[&str]) -> Result<Value> {
    debug!("Executing ggen CLI with args: {:?}", args);
    info!("CLI Command: ggen {}", args.join(" "));

    let output = Command::new("ggen").args(args).output().map_err(|e| {
        error!("Failed to execute ggen CLI: {}", e);
        error!("This usually means 'ggen' is not in PATH or not installed");
        GgenMcpError::ExecutionFailed(format!(
            "Failed to execute ggen CLI: {}. Ensure 'ggen' is installed and in PATH",
            e
        ))
    })?;

    let exit_code = output.status.code();
    debug!("ggen CLI exit code: {:?}", exit_code);

    if !output.status.success() {
        let stderr = String::from_utf8_lossy(&output.stderr);
        let stdout = String::from_utf8_lossy(&output.stdout);
        error!("ggen CLI failed with exit code {:?}", exit_code);
        error!("stderr: {}", stderr);
        debug!("stdout: {}", stdout);

        return Err(GgenMcpError::ExecutionFailed(format!(
            "ggen CLI command failed (exit code {:?}): {}",
            exit_code,
            stderr.trim()
        )));
    }

    let stdout = String::from_utf8_lossy(&output.stdout);
    debug!("ggen CLI stdout length: {} bytes", stdout.len());

    // Try to parse as JSON first
    if let Ok(json) = serde_json::from_str::<Value>(&stdout) {
        info!("Successfully parsed JSON output from ggen CLI");
        debug!("Parsed JSON: {:?}", json);
        return Ok(json);
    }

    // If not JSON, wrap the text output in a JSON structure
    info!("ggen CLI returned non-JSON output, wrapping in JSON structure");
    let wrapped = serde_json::json!({
        "output": stdout.trim(),
        "success": true,
        "format": "text"
    });
    Ok(wrapped)
}

/// Execute a ggen CLI command with variable arguments
pub async fn call_ggen_with_vars(
    subcommands: &[&str], vars: &serde_json::Map<String, Value>,
) -> Result<Value> {
    debug!("Building CLI command with {} variables", vars.len());

    let mut args: Vec<String> = subcommands.iter().map(|s| s.to_string()).collect();

    // Add variable arguments
    for (key, value) in vars.iter() {
        args.push("--var".to_string());
        let val_str = match value {
            Value::String(s) => s.clone(),
            Value::Number(n) => n.to_string(),
            Value::Bool(b) => b.to_string(),
            _ => value.to_string(),
        };
        let var_arg = format!("{}={}", key, val_str);
        debug!("Adding variable: {}", var_arg);
        args.push(var_arg);
    }

    let arg_refs: Vec<&str> = args.iter().map(|s| s.as_str()).collect();
    call_ggen_cli(&arg_refs).await
}

#[cfg(test)]
mod tests {
    use super::*;

    #[tokio::test]
    async fn test_call_ggen_cli_help() {
        // Test that we can call ggen CLI
        let result = call_ggen_cli(&["--help"]).await;

        // Help command should succeed
        assert!(result.is_ok(), "ggen --help should succeed");

        let output = result.unwrap();
        assert!(output.is_object() || output.is_string());
    }

    #[tokio::test]
    async fn test_call_ggen_with_vars() {
        let mut vars = serde_json::Map::new();
        vars.insert("name".to_string(), Value::String("test".to_string()));
        vars.insert("version".to_string(), Value::String("1.0.0".to_string()));

        // This will fail if ggen CLI doesn't exist, but tests the logic
        let result = call_ggen_with_vars(&["project", "gen", "test-template"], &vars).await;

        // We expect this to fail because test-template doesn't exist,
        // but it should fail in the right way (CLI execution, not argument parsing)
        assert!(result.is_err());
        if let Err(GgenMcpError::ExecutionFailed(msg)) = result {
            assert!(
                msg.contains("ggen CLI"),
                "Error should mention CLI execution"
            );
        }
    }
}
