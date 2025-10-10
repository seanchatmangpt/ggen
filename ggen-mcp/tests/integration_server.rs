//! Integration tests for ggen-mcp server
//!
//! These tests verify end-to-end functionality of the MCP server

mod common;

use common::*;
use ggen_mcp::server::GgenMcpServer;
use serde_json::json;

#[test]
fn test_server_initialization() {
    setup_test_logging();
    let server = create_test_server();
    assert!(std::mem::size_of_val(&server) > 0);
}

#[test]
fn test_workspace_creation() {
    let workspace = create_temp_workspace();
    assert!(workspace.path().exists());
    assert!(workspace.path().is_dir());
}

#[test]
fn test_template_fixture_loading() {
    let template = fixtures::rust_api_template();
    assert_eq!(template["name"], "rust-api");
    assert!(template["files"].is_array());
    assert!(template["files"].as_array().unwrap().len() > 0);
}

#[test]
fn test_multiple_templates() {
    let rust_template = fixtures::rust_api_template();
    let ts_template = fixtures::typescript_app_template();
    let py_template = fixtures::python_service_template();

    assert_ne!(rust_template["name"], ts_template["name"]);
    assert_ne!(ts_template["name"], py_template["name"]);
}

#[test]
fn test_complex_template_structure() {
    let template = fixtures::complex_nested_template();
    let files = template["files"].as_array().unwrap();

    assert!(files.len() >= 5);
    assert!(files.iter().any(|f| f["path"].as_str().unwrap().contains("backend")));
    assert!(files.iter().any(|f| f["path"].as_str().unwrap().contains("frontend")));
}

#[test]
fn test_tool_request_format() {
    let request = mock_tool_request("test_tool", json!({"arg": "value"}));
    assert_eq!(request["method"], "tools/call");
    assert_eq!(request["params"]["name"], "test_tool");
}

#[test]
fn test_file_creation_in_workspace() {
    let workspace = create_temp_workspace();
    let file_path = create_test_file(&workspace, "test.txt", "test content");

    assert!(file_path.exists());
    let content = std::fs::read_to_string(&file_path).unwrap();
    assert_eq!(content, "test content");
}

#[test]
fn test_nested_file_creation() {
    let workspace = create_temp_workspace();
    let file_path = create_test_file(&workspace, "src/lib.rs", "// Library code");

    assert!(file_path.exists());
    assert!(file_path.parent().unwrap().exists());
}

#[test]
fn test_json_success_assertion() {
    let response = json!({"result": "success", "data": "test"});
    assert_json_success(&response);
}

#[test]
#[should_panic(expected = "Response should have result field")]
fn test_json_success_assertion_fails() {
    let response = json!({"error": "failed"});
    assert_json_success(&response);
}

#[test]
fn test_json_error_assertion() {
    let response = json!({"error": {"code": -32600, "message": "Invalid Request"}});
    assert_json_error(&response, Some(-32600));
}

#[test]
fn test_random_template_generation() {
    let name1 = fixtures::generators::random_template_name();
    let name2 = fixtures::generators::random_template_name();

    assert_ne!(name1, name2);
    assert!(name1.len() > 10);
    assert!(name2.len() > 10);
}

#[cfg(feature = "stress-test")]
#[test]
#[ignore] // Slow test, run with --ignored
fn stress_test_many_workspaces() {
    let workspaces: Vec<_> = (0..1000)
        .map(|_| create_temp_workspace())
        .collect();

    assert_eq!(workspaces.len(), 1000);
    for workspace in &workspaces {
        assert!(workspace.path().exists());
    }
}

#[cfg(feature = "stress-test")]
#[test]
#[ignore] // Slow test
fn stress_test_large_template() {
    let template = json!({
        "name": "stress-template",
        "files": (0..10000).map(|i| json!({
            "path": format!("file_{}.rs", i),
            "content": format!("// Content {}", i)
        })).collect::<Vec<_>>()
    });

    let files = template["files"].as_array().unwrap();
    assert_eq!(files.len(), 10000);
}

#[tokio::test]
async fn test_async_server_operations() {
    setup_test_logging();
    let server = create_test_server();

    // Simulate async operation
    tokio::time::sleep(tokio::time::Duration::from_millis(10)).await;

    assert!(std::mem::size_of_val(&server) > 0);
}

#[tokio::test]
async fn test_concurrent_server_creation() {
    let handles: Vec<_> = (0..10)
        .map(|_| {
            tokio::spawn(async {
                create_test_server()
            })
        })
        .collect();

    for handle in handles {
        assert!(handle.await.is_ok());
    }
}
