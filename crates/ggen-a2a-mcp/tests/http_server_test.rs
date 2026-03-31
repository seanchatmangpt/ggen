//! Integration tests for HTTP MCP server.

use std::time::Duration;
use tokio::time::sleep;

/// Test that HTTP server starts and binds to port.
#[tokio::test]
async fn test_http_server_starts() {
    // Use a random port to avoid conflicts
    let port = 18765 + (rand::random::<u16>() % 1000);
    let host = "127.0.0.1";

    // Spawn server in background
    let server_handle =
        tokio::spawn(async move { ggen_a2a_mcp::server::serve_http(host, port).await });

    // Give server time to start
    sleep(Duration::from_millis(100)).await;

    // Verify server is listening by attempting to connect
    let addr = format!("{}:{}", host, port);
    match tokio::net::TcpListener::bind(&addr).await {
        Ok(_) => {
            // If we can bind, server failed to start
            panic!("Server did not bind to {}", addr);
        }
        Err(e) if e.kind() == std::io::ErrorKind::AddrInUse => {
            // Address in use means server is running - success!
        }
        Err(e) => {
            panic!("Unexpected error: {}", e);
        }
    }

    // Clean up server
    server_handle.abort();
}

/// Test POST request with valid JSON-RPC.
#[tokio::test]
async fn test_post_valid_jsonrpc() {
    let port = 18766 + (rand::random::<u16>() % 1000);
    let host = "127.0.0.1";

    // Start server
    tokio::spawn(async move { ggen_a2a_mcp::server::serve_http(host, port).await });

    sleep(Duration::from_millis(100)).await;

    // Send POST request with valid JSON-RPC
    let client = reqwest::Client::new();
    let url = format!("http://{}:{}/", host, port);
    let json_req = serde_json::json!({
        "jsonrpc": "2.0",
        "method": "tools/list",
        "id": 1
    });

    let response = client
        .post(&url)
        .header("content-type", "application/json")
        .json(&json_req)
        .timeout(Duration::from_secs(5))
        .send()
        .await;

    assert!(response.is_ok(), "Failed to send request");
    let resp = response.unwrap();

    assert_eq!(resp.status(), reqwest::StatusCode::OK);
    assert!(resp
        .headers()
        .get("content-type")
        .unwrap()
        .to_str()
        .unwrap()
        .contains("application/json"));

    let body: serde_json::Value = resp.json().await.unwrap();
    assert_eq!(body["jsonrpc"], "2.0");
    assert!(body.get("result").is_some());
    assert_eq!(body["id"], 1);
}

/// Test invalid content-type returns 400.
#[tokio::test]
async fn test_invalid_content_type() {
    let port = 18767 + (rand::random::<u16>() % 1000);
    let host = "127.0.0.1";

    // Start server
    tokio::spawn(async move { ggen_a2a_mcp::server::serve_http(host, port).await });

    sleep(Duration::from_millis(100)).await;

    // Send POST request with invalid content-type
    let client = reqwest::Client::new();
    let url = format!("http://{}:{}/", host, port);
    let json_req = serde_json::json!({
        "jsonrpc": "2.0",
        "method": "tools/list",
        "id": 1
    });

    let response = client
        .post(&url)
        .header("content-type", "text/plain")
        .json(&json_req)
        .timeout(Duration::from_secs(5))
        .send()
        .await;

    assert!(response.is_ok());
    let resp = response.unwrap();
    assert_eq!(resp.status(), reqwest::StatusCode::BAD_REQUEST);
}

/// Test CORS headers are present.
#[tokio::test]
async fn test_cors_headers() {
    let port = 18768 + (rand::random::<u16>() % 1000);
    let host = "127.0.0.1";

    // Start server
    tokio::spawn(async move { ggen_a2a_mcp::server::serve_http(host, port).await });

    sleep(Duration::from_millis(100)).await;

    // Send POST request
    let client = reqwest::Client::new();
    let url = format!("http://{}:{}/", host, port);
    let json_req = serde_json::json!({
        "jsonrpc": "2.0",
        "method": "tools/list",
        "id": 1
    });

    let response = client
        .post(&url)
        .header("content-type", "application/json")
        .json(&json_req)
        .timeout(Duration::from_secs(5))
        .send()
        .await;

    assert!(response.is_ok());
    let resp = response.unwrap();

    // Check CORS headers
    assert!(resp.headers().get("access-control-allow-origin").is_some());
}

/// Test malformed JSON returns 400.
#[tokio::test]
async fn test_malformed_json() {
    let port = 18769 + (rand::random::<u16>() % 1000);
    let host = "127.0.0.1";

    // Start server
    tokio::spawn(async move { ggen_a2a_mcp::server::serve_http(host, port).await });

    sleep(Duration::from_millis(100)).await;

    // Send POST request with malformed JSON
    let client = reqwest::Client::new();
    let url = format!("http://{}:{}/", host, port);

    let response = client
        .post(&url)
        .header("content-type", "application/json")
        .body("{ invalid json }")
        .timeout(Duration::from_secs(5))
        .send()
        .await;

    assert!(response.is_ok());
    let resp = response.unwrap();
    assert_eq!(resp.status(), reqwest::StatusCode::BAD_REQUEST);
}

/// Test server returns list of available tools.
#[tokio::test]
async fn test_list_tools_response() {
    let port = 18770 + (rand::random::<u16>() % 1000);
    let host = "127.0.0.1";

    // Start server
    tokio::spawn(async move { ggen_a2a_mcp::server::serve_http(host, port).await });

    sleep(Duration::from_millis(100)).await;

    // Send request
    let client = reqwest::Client::new();
    let url = format!("http://{}:{}/", host, port);
    let json_req = serde_json::json!({
        "jsonrpc": "2.0",
        "method": "tools/list",
        "id": 42
    });

    let response = client
        .post(&url)
        .header("content-type", "application/json")
        .json(&json_req)
        .timeout(Duration::from_secs(5))
        .send()
        .await
        .unwrap();

    let body: serde_json::Value = response.json().await.unwrap();

    // Verify response structure
    assert_eq!(body["jsonrpc"], "2.0");
    assert_eq!(body["id"], 42);

    // Verify tools are listed (our simple implementation returns a status with tools array)
    let tools = body["result"]["tools"].as_array().unwrap();
    assert!(!tools.is_empty());

    // Verify expected tools are present
    let tool_names: Vec<&str> = tools.iter().filter_map(|t| t.as_str()).collect();

    assert!(tool_names.contains(&"generate"));
    assert!(tool_names.contains(&"validate"));
    assert!(tool_names.contains(&"sync"));
}
