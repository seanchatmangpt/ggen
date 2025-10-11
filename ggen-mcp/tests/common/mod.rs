//! Common test utilities and helpers for ggen-mcp integration tests
//!
//! This module provides shared test infrastructure following Rust best practices:
//! - Server creation helpers
//! - Mock data builders
//! - Assertion utilities
//! - Workspace management

use ggen_mcp::server::GgenMcpServer;
use serde_json::{json, Value};
use std::path::PathBuf;
use tempfile::TempDir;

pub mod fixtures;

/// Creates a test MCP server instance with default configuration
///
/// # Example
/// ```no_run
/// use ggen_mcp_tests::common::create_test_server;
///
/// let server = create_test_server();
/// ```
pub fn create_test_server() -> GgenMcpServer {
    GgenMcpServer::new()
}

/// Creates a temporary workspace directory for test isolation
///
/// The directory is automatically cleaned up when the TempDir is dropped
///
/// # Example
/// ```no_run
/// use ggen_mcp_tests::common::create_temp_workspace;
///
/// let workspace = create_temp_workspace();
/// let path = workspace.path();
/// // Use path for test operations
/// ```
pub fn create_temp_workspace() -> TempDir {
    tempfile::tempdir().expect("Failed to create temporary workspace")
}

/// Creates a temporary workspace with a specific name pattern
pub fn create_named_workspace(pattern: &str) -> TempDir {
    tempfile::Builder::new()
        .prefix(pattern)
        .tempdir()
        .expect("Failed to create named workspace")
}

/// Mock template builder for testing
///
/// # Example
/// ```no_run
/// use ggen_mcp_tests::common::mock_template;
///
/// let template = mock_template();
/// assert_eq!(template["name"], "test-template");
/// ```
pub fn mock_template() -> Value {
    json!({
        "name": "test-template",
        "version": "1.0.0",
        "description": "Test template",
        "files": [],
        "variables": {}
    })
}

/// Creates a mock template with custom name
pub fn mock_template_named(name: &str) -> Value {
    json!({
        "name": name,
        "version": "1.0.0",
        "description": format!("Test template: {}", name),
        "files": [],
        "variables": {}
    })
}

/// Creates a mock template with files
pub fn mock_template_with_files(name: &str, files: Vec<&str>) -> Value {
    json!({
        "name": name,
        "version": "1.0.0",
        "description": format!("Test template: {}", name),
        "files": files.iter().map(|f| json!({
            "path": f,
            "content": format!("// Content for {}", f)
        })).collect::<Vec<_>>(),
        "variables": {}
    })
}

/// Mock tool request builder
pub fn mock_tool_request(tool_name: &str, args: Value) -> Value {
    json!({
        "jsonrpc": "2.0",
        "id": 1,
        "method": "tools/call",
        "params": {
            "name": tool_name,
            "arguments": args
        }
    })
}

/// Mock resource request builder
pub fn mock_resource_request(uri: &str) -> Value {
    json!({
        "jsonrpc": "2.0",
        "id": 1,
        "method": "resources/read",
        "params": {
            "uri": uri
        }
    })
}

/// Test assertion helper for JSON responses
pub fn assert_json_success(response: &Value) {
    assert!(
        response.get("result").is_some(),
        "Response should have result field"
    );
    assert!(
        response.get("error").is_none(),
        "Response should not have error field"
    );
}

/// Test assertion helper for error responses
pub fn assert_json_error(response: &Value, expected_code: Option<i64>) {
    let error = response
        .get("error")
        .expect("Response should have error field");
    if let Some(code) = expected_code {
        assert_eq!(
            error.get("code").and_then(|c| c.as_i64()),
            Some(code),
            "Error code mismatch"
        );
    }
}

/// Creates a test file in the workspace
pub fn create_test_file(workspace: &TempDir, path: &str, content: &str) -> PathBuf {
    let file_path = workspace.path().join(path);

    // Create parent directories if needed
    if let Some(parent) = file_path.parent() {
        std::fs::create_dir_all(parent).expect("Failed to create parent directories");
    }

    std::fs::write(&file_path, content).expect("Failed to write test file");
    file_path
}

/// Wait helper for async tests
#[cfg(feature = "tokio")]
pub async fn wait_for_condition<F>(mut condition: F, timeout_ms: u64) -> bool
where
    F: FnMut() -> bool,
{
    use tokio::time::{sleep, Duration};

    let start = std::time::Instant::now();
    let timeout = Duration::from_millis(timeout_ms);

    while start.elapsed() < timeout {
        if condition() {
            return true;
        }
        sleep(Duration::from_millis(10)).await;
    }

    false
}

/// Setup test logging for integration tests
pub fn setup_test_logging() {
    let _ = env_logger::builder()
        .is_test(true)
        .filter_level(log::LevelFilter::Debug)
        .try_init();
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_create_temp_workspace() {
        let workspace = create_temp_workspace();
        assert!(workspace.path().exists());
    }

    #[test]
    fn test_mock_template() {
        let template = mock_template();
        assert_eq!(template["name"], "test-template");
        assert!(template["files"].is_array());
    }

    #[test]
    fn test_mock_template_with_files() {
        let template = mock_template_with_files("test", vec!["file1.rs", "file2.rs"]);
        assert_eq!(template["files"].as_array().unwrap().len(), 2);
    }

    #[test]
    fn test_create_test_file() {
        let workspace = create_temp_workspace();
        let file_path = create_test_file(&workspace, "test.txt", "test content");

        assert!(file_path.exists());
        let content = std::fs::read_to_string(&file_path).unwrap();
        assert_eq!(content, "test content");
    }

    #[test]
    fn test_json_assertions() {
        let success = json!({"result": "ok"});
        assert_json_success(&success);

        let error = json!({"error": {"code": -32600, "message": "Invalid request"}});
        assert_json_error(&error, Some(-32600));
    }
}
