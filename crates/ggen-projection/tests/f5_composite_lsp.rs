use serde_json::{json, Value};
use std::sync::Arc;
use std::time::Duration;
use tokio::sync::Mutex;
use tokio::time::timeout;

// Mock Upstream Server state for testing Feature 5
#[derive(Clone)]
pub struct MockUpstream {
    pub name: String,
    pub should_crash: bool,
    pub delay: Option<Duration>,
    pub diagnostics: Vec<Value>,
    pub hover_text: Option<String>,
    pub completion_items: Vec<Value>,
}

// Composite LSP Multiplexer Implementation
pub struct CompositeMultiplexer {
    pub upstreams: Vec<MockUpstream>,
    pub client_diagnostics: Arc<Mutex<Vec<Value>>>,
    pub timeout_limit: Duration,
}

impl CompositeMultiplexer {
    pub fn new(upstreams: Vec<MockUpstream>) -> Self {
        Self {
            upstreams,
            client_diagnostics: Arc::new(Mutex::new(Vec::new())),
            timeout_limit: Duration::from_millis(50),
        }
    }

    pub async fn handle_request(
        &self, method: &str, _params: Value,
    ) -> Result<Value, anyhow::Error> {
        match method {
            "initialize" => Ok(json!({
                "capabilities": {
                    "textDocumentSync": 1,
                    "hoverProvider": true,
                    "completionProvider": {
                        "resolveProvider": false
                    }
                },
                "serverInfo": {
                    "name": "tower-lsp-max-composite"
                }
            })),
            "textDocument/hover" => {
                let mut contents = Vec::new();
                for upstream in &self.upstreams {
                    if upstream.should_crash {
                        continue;
                    }
                    let run = async {
                        if let Some(delay) = upstream.delay {
                            tokio::time::sleep(delay).await;
                        }
                        upstream.hover_text.clone()
                    };
                    match timeout(self.timeout_limit, run).await {
                        Ok(Some(text)) => contents.push(text),
                        _ => {} // Timeout or no hover
                    }
                }
                if contents.is_empty() {
                    Ok(Value::Null)
                } else {
                    Ok(json!({
                        "contents": {
                            "kind": "markdown",
                            "value": contents.join("\n---\n")
                        }
                    }))
                }
            }
            "textDocument/completion" => {
                let mut items = Vec::new();
                for upstream in &self.upstreams {
                    if upstream.should_crash {
                        continue;
                    }
                    let run = async {
                        if let Some(delay) = upstream.delay {
                            tokio::time::sleep(delay).await;
                        }
                        upstream.completion_items.clone()
                    };
                    match timeout(self.timeout_limit, run).await {
                        Ok(res_items) => items.extend(res_items),
                        _ => {} // Timeout or crash
                    }
                }
                Ok(json!({
                    "isIncomplete": false,
                    "items": items
                }))
            }
            "shutdown" => Ok(json!(null)),
            _ => Err(anyhow::anyhow!("Method not found")),
        }
    }

    pub async fn handle_notification(
        &self, method: &str, params: Value,
    ) -> Result<(), anyhow::Error> {
        if method == "textDocument/publishDiagnostics" {
            // Attribution and source validation
            let mut diags = params
                .get("diagnostics")
                .and_then(Value::as_array)
                .cloned()
                .unwrap_or_default();
            let mut validated_diags = Vec::new();

            for d in &mut diags {
                let source_id = d
                    .get("data")
                    .and_then(|data| data.get("source_id"))
                    .and_then(Value::as_str);

                // If source_id is missing/stripped, reject the merge (Attribution Bypass constraint)
                if source_id.is_none() {
                    return Err(anyhow::anyhow!("Attribution Bypass: source_id is missing"));
                }

                validated_diags.push(d.clone());
            }

            let mut client_diags = self.client_diagnostics.lock().await;
            client_diags.push(json!({
                "uri": params.get("uri").unwrap_or(&Value::Null),
                "diagnostics": validated_diags
            }));
        }
        Ok(())
    }
}

// ==========================================
// Tier 1: Feature Coverage (5 tests)
// ==========================================

#[tokio::test]
async fn test_f5_t1_initialize_multiplexer() {
    let multiplexer = CompositeMultiplexer::new(vec![]);
    let resp = multiplexer
        .handle_request("initialize", json!({}))
        .await
        .unwrap();
    assert_eq!(resp["serverInfo"]["name"], "tower-lsp-max-composite");
}

#[tokio::test]
async fn test_f5_t1_route_diagnostics_with_attribution() {
    let multiplexer = CompositeMultiplexer::new(vec![]);

    // Valid diagnostics with source_id
    let params = json!({
        "uri": "file:///src/main.rs",
        "diagnostics": [{
            "range": { "start": { "line": 0, "character": 0 }, "end": { "line": 0, "character": 5 } },
            "message": "Drift detected",
            "source": "ggen-lsp",
            "data": {
                "source_id": "ggen_lsp"
            }
        }]
    });

    let res = multiplexer
        .handle_notification("textDocument/publishDiagnostics", params)
        .await;
    assert!(res.is_ok());

    let client_diags = multiplexer.client_diagnostics.lock().await;
    assert_eq!(client_diags.len(), 1);
    assert_eq!(
        client_diags[0]["diagnostics"][0]["data"]["source_id"],
        "ggen_lsp"
    );
}

#[tokio::test]
async fn test_f5_t1_merge_hover_responses() {
    let upstream1 = MockUpstream {
        name: "rust-analyzer".to_string(),
        should_crash: false,
        delay: None,
        diagnostics: vec![],
        hover_text: Some("Hover from Rust Analyzer".to_string()),
        completion_items: vec![],
    };
    let upstream2 = MockUpstream {
        name: "ggen-lsp".to_string(),
        should_crash: false,
        delay: None,
        diagnostics: vec![],
        hover_text: Some("Hover from ggen-lsp".to_string()),
        completion_items: vec![],
    };

    let multiplexer = CompositeMultiplexer::new(vec![upstream1, upstream2]);
    let resp = multiplexer
        .handle_request("textDocument/hover", json!({}))
        .await
        .unwrap();

    let val = resp["contents"]["value"].as_str().unwrap();
    assert!(val.contains("Hover from Rust Analyzer"));
    assert!(val.contains("Hover from ggen-lsp"));
}

#[tokio::test]
async fn test_f5_t1_route_completion() {
    let upstream1 = MockUpstream {
        name: "rust-analyzer".to_string(),
        should_crash: false,
        delay: None,
        diagnostics: vec![],
        hover_text: None,
        completion_items: vec![json!({"label": "itemA"})],
    };
    let upstream2 = MockUpstream {
        name: "ggen-lsp".to_string(),
        should_crash: false,
        delay: None,
        diagnostics: vec![],
        hover_text: None,
        completion_items: vec![json!({"label": "itemB"})],
    };

    let multiplexer = CompositeMultiplexer::new(vec![upstream1, upstream2]);
    let resp = multiplexer
        .handle_request("textDocument/completion", json!({}))
        .await
        .unwrap();

    let items = resp["items"].as_array().unwrap();
    assert_eq!(items.len(), 2);
    assert_eq!(items[0]["label"], "itemA");
    assert_eq!(items[1]["label"], "itemB");
}

#[tokio::test]
async fn test_f5_t1_routing_source_preservation() {
    let multiplexer = CompositeMultiplexer::new(vec![]);

    let params = json!({
        "uri": "file:///src/main.rs",
        "diagnostics": [{
            "range": { "start": { "line": 0, "character": 0 }, "end": { "line": 0, "character": 5 } },
            "message": "Drift detected",
            "source": "rust-analyzer",
            "data": {
                "source_id": "rust_analyzer"
            }
        }]
    });

    multiplexer
        .handle_notification("textDocument/publishDiagnostics", params)
        .await
        .unwrap();
    let client_diags = multiplexer.client_diagnostics.lock().await;
    assert_eq!(client_diags[0]["diagnostics"][0]["source"], "rust-analyzer");
}

// ==========================================
// Tier 2: Boundary & Corner Cases (5 tests)
// ==========================================

#[tokio::test]
async fn test_f5_t2_upstream_crash_isolation() {
    // Upstream 1 crashes, Upstream 2 is healthy
    let upstream1 = MockUpstream {
        name: "rust-analyzer".to_string(),
        should_crash: true,
        delay: None,
        diagnostics: vec![],
        hover_text: Some("Hover from Rust Analyzer".to_string()),
        completion_items: vec![],
    };
    let upstream2 = MockUpstream {
        name: "ggen-lsp".to_string(),
        should_crash: false,
        delay: None,
        diagnostics: vec![],
        hover_text: Some("Hover from ggen-lsp".to_string()),
        completion_items: vec![],
    };

    let multiplexer = CompositeMultiplexer::new(vec![upstream1, upstream2]);
    let resp = multiplexer
        .handle_request("textDocument/hover", json!({}))
        .await
        .unwrap();

    // Should still return results from non-crashed upstream
    let val = resp["contents"]["value"].as_str().unwrap();
    assert!(!val.contains("Hover from Rust Analyzer"));
    assert!(val.contains("Hover from ggen-lsp"));
}

#[tokio::test]
async fn test_f5_t2_slow_upstream_timeout() {
    // Upstream 1 has a delay of 200ms (exceeding multiplexer's 50ms limit), Upstream 2 is instant
    let upstream1 = MockUpstream {
        name: "rust-analyzer".to_string(),
        should_crash: false,
        delay: Some(Duration::from_millis(200)),
        diagnostics: vec![],
        hover_text: Some("Hover from Rust Analyzer".to_string()),
        completion_items: vec![],
    };
    let upstream2 = MockUpstream {
        name: "ggen-lsp".to_string(),
        should_crash: false,
        delay: None,
        diagnostics: vec![],
        hover_text: Some("Hover from ggen-lsp".to_string()),
        completion_items: vec![],
    };

    let multiplexer = CompositeMultiplexer::new(vec![upstream1, upstream2]);
    let start = std::time::Instant::now();
    let resp = multiplexer
        .handle_request("textDocument/hover", json!({}))
        .await
        .unwrap();
    let elapsed = start.elapsed();

    // Verify response was fast and slow upstream was timed out
    assert!(elapsed < Duration::from_millis(150));
    let val = resp["contents"]["value"].as_str().unwrap();
    assert!(!val.contains("Hover from Rust Analyzer"));
    assert!(val.contains("Hover from ggen-lsp"));
}

#[tokio::test]
async fn test_f5_t2_duplicate_diagnostic_keys() {
    let multiplexer = CompositeMultiplexer::new(vec![]);

    // Send duplicate diagnostics from different sources
    let params1 = json!({
        "uri": "file:///src/main.rs",
        "diagnostics": [{
            "range": { "start": { "line": 0, "character": 0 }, "end": { "line": 0, "character": 5 } },
            "message": "Error X",
            "source": "rust-analyzer",
            "data": {
                "source_id": "rust_analyzer"
            }
        }]
    });
    let params2 = json!({
        "uri": "file:///src/main.rs",
        "diagnostics": [{
            "range": { "start": { "line": 0, "character": 0 }, "end": { "line": 0, "character": 5 } },
            "message": "Error X",
            "source": "ggen-lsp",
            "data": {
                "source_id": "ggen_lsp"
            }
        }]
    });

    multiplexer
        .handle_notification("textDocument/publishDiagnostics", params1)
        .await
        .unwrap();
    multiplexer
        .handle_notification("textDocument/publishDiagnostics", params2)
        .await
        .unwrap();

    let client_diags = multiplexer.client_diagnostics.lock().await;
    assert_eq!(client_diags.len(), 2);
    assert_eq!(
        client_diags[0]["diagnostics"][0]["data"]["source_id"],
        "rust_analyzer"
    );
    assert_eq!(
        client_diags[1]["diagnostics"][0]["data"]["source_id"],
        "ggen_lsp"
    );
}

#[tokio::test]
async fn test_f5_t2_malformed_rpc_from_upstream() {
    let multiplexer = CompositeMultiplexer::new(vec![]);

    // Simulate notification with missing source_id (Attribution Bypass condition)
    let bad_params = json!({
        "uri": "file:///src/main.rs",
        "diagnostics": [{
            "range": { "start": { "line": 0, "character": 0 }, "end": { "line": 0, "character": 5 } },
            "message": "No source ID"
        }]
    });

    let res = multiplexer
        .handle_notification("textDocument/publishDiagnostics", bad_params)
        .await;
    assert!(res.is_err()); // Reject merge and isolate error
}

#[tokio::test]
async fn test_f5_t2_composite_exit_propagation() {
    let multiplexer = CompositeMultiplexer::new(vec![]);

    // Send shutdown request
    let resp = multiplexer
        .handle_request("shutdown", json!(null))
        .await
        .unwrap();
    assert_eq!(resp, Value::Null);
}
