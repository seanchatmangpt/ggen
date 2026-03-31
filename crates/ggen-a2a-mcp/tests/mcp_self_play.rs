//! MCP Self-Play Integration Tests
//!
//! Simulates an automated MCP client building a project end-to-end using the
//! full ggen MCP tool surface: discover -> scaffold -> validate -> query -> sync.
//!
//! Pattern: rmcp 1.3.0 in-process duplex transport (tokio::io::duplex).
//! AAA: Arrange / Act / Assert
//!
//! Run with: cargo test -p ggen-a2a-mcp --test mcp_self_play -- --test-threads=1 --nocapture

use ggen_a2a_mcp::ggen_server::GgenMcpServer;
use rmcp::{model::*, service::RunningService, ClientHandler, RoleClient, ServiceExt};

// ---------------------------------------------------------------------------
// Minimal no-op client handler
// ---------------------------------------------------------------------------

#[derive(Debug, Clone, Default)]
struct TestClientHandler;

impl ClientHandler for TestClientHandler {
    fn get_info(&self) -> ClientInfo {
        ClientInfo::default()
    }
}

// ---------------------------------------------------------------------------
// Helpers
// ---------------------------------------------------------------------------

/// Spin up GgenMcpServer over an in-process duplex transport (65536 buffer).
async fn start_server() -> anyhow::Result<RunningService<RoleClient, TestClientHandler>> {
    let (server_transport, client_transport) = tokio::io::duplex(65536);

    let server = GgenMcpServer::new();
    tokio::spawn(async move {
        let _ = server.serve(server_transport).await;
    });

    let client = TestClientHandler::default().serve(client_transport).await?;
    Ok(client)
}

/// Extract text content from a CallToolResult.
fn extract_text(result: &CallToolResult) -> Option<String> {
    result.content.iter().find_map(|c| {
        if let RawContent::Text(tc) = &c.raw {
            Some(tc.text.clone())
        } else {
            None
        }
    })
}

/// Build tool arguments from a JSON object literal.
fn args(json: serde_json::Value) -> serde_json::Map<String, serde_json::Value> {
    json.as_object().cloned().unwrap_or_default()
}

// ---------------------------------------------------------------------------
// Test 1: Discover examples and scaffold one into a tempdir
// ---------------------------------------------------------------------------

#[tokio::test]
async fn test_mcp_discover_and_scaffold() -> anyhow::Result<()> {
    // Arrange — point server at the real examples directory
    std::env::set_var("GGEN_EXAMPLES_DIR", "/Users/sac/ggen/examples");
    let client = start_server().await?;
    let tempdir = tempfile::tempdir()?;

    // Act — list examples
    let list_result = client
        .call_tool(CallToolRequestParams::new("list_examples"))
        .await?;
    assert_ne!(
        list_result.is_error,
        Some(true),
        "list_examples should not error"
    );

    let list_text = extract_text(&list_result).expect("list_examples must return text");
    let list_json: serde_json::Value = serde_json::from_str(&list_text)?;
    let examples = list_json["examples"]
        .as_array()
        .expect("examples should be a JSON array");
    assert!(!examples.is_empty(), "examples array must be non-empty");

    // Pick first example name
    let first_name = examples[0]["name"]
        .as_str()
        .expect("example must have a string name");

    // Get example details
    let get_result = client
        .call_tool(
            CallToolRequestParams::new("get_example").with_arguments(args(serde_json::json!({
                "name": first_name
            }))),
        )
        .await?;
    assert_ne!(
        get_result.is_error,
        Some(true),
        "get_example should not error"
    );
    let get_text = extract_text(&get_result).expect("get_example must return text");
    assert!(
        get_text.contains("@prefix"),
        "get_example response must contain TTL content with @prefix"
    );

    // Scaffold into tempdir
    let target_dir = tempdir.path().join("scaffolded");
    let scaffold_result = client
        .call_tool(
            CallToolRequestParams::new("scaffold_from_example").with_arguments(args(
                serde_json::json!({
                    "example_name": first_name,
                    "target_dir": target_dir.to_str().unwrap()
                }),
            )),
        )
        .await?;
    assert_ne!(
        scaffold_result.is_error,
        Some(true),
        "scaffold_from_example should not error: {:?}",
        extract_text(&scaffold_result)
    );

    // Assert — ggen.toml must exist in scaffolded output
    assert!(
        target_dir.join("ggen.toml").exists(),
        "scaffolded dir must contain ggen.toml"
    );

    client.cancel().await?;
    Ok(())
}

// ---------------------------------------------------------------------------
// Test 2: Validate TTL and run a SPARQL query
// ---------------------------------------------------------------------------

#[tokio::test]
async fn test_mcp_validate_and_query() -> anyhow::Result<()> {
    // Arrange
    let client = start_server().await?;
    let valid_ttl = concat!(
        "@prefix ex: <http://example.org/> .\n",
        "ex:Foo a ex:Bar .\n",
        "ex:Baz a ex:Bar .\n",
    );

    // Act — validate
    let validate_result = client
        .call_tool(
            CallToolRequestParams::new("validate").with_arguments(args(serde_json::json!({
                "ttl": valid_ttl
            }))),
        )
        .await?;

    // Assert — valid
    assert_ne!(
        validate_result.is_error,
        Some(true),
        "validate should succeed"
    );
    let validate_text = extract_text(&validate_result).expect("validate must return text");
    assert!(
        validate_text.contains("Valid"),
        "validate response must contain 'Valid', got: {}",
        validate_text
    );

    // Act — query ontology
    let sparql = "PREFIX ex: <http://example.org/> SELECT ?s WHERE { ?s a ex:Bar }";
    let query_result = client
        .call_tool(
            CallToolRequestParams::new("query_ontology").with_arguments(args(serde_json::json!({
                "ttl": valid_ttl,
                "sparql": sparql
            }))),
        )
        .await?;

    // Assert — at least 2 results (Foo and Baz)
    assert_ne!(
        query_result.is_error,
        Some(true),
        "query_ontology should succeed"
    );
    let query_text = extract_text(&query_result).expect("query_ontology must return text");
    let query_json: serde_json::Value = serde_json::from_str(&query_text)?;
    let count = query_json["count"]
        .as_u64()
        .expect("count must be a number");
    assert!(
        count >= 2,
        "expected at least 2 results for ?s a ex:Bar, got {}",
        count
    );

    client.cancel().await?;
    Ok(())
}

// ---------------------------------------------------------------------------
// Test 3: Full project build — discover, scaffold, validate, query, sync
// ---------------------------------------------------------------------------

#[tokio::test]
async fn test_mcp_full_project_build() -> anyhow::Result<()> {
    // Arrange
    std::env::set_var("GGEN_EXAMPLES_DIR", "/Users/sac/ggen/examples");
    let client = start_server().await?;
    let tempdir = tempfile::tempdir()?;
    let target_dir = tempdir.path().join("a2a-groq-agent-project");

    // Act 1 — list_examples and find a2a-groq-agent
    let list_result = client
        .call_tool(CallToolRequestParams::new("list_examples"))
        .await?;
    let list_text = extract_text(&list_result).expect("list_examples text");
    let list_json: serde_json::Value = serde_json::from_str(&list_text)?;
    let names: Vec<&str> = list_json["examples"]
        .as_array()
        .unwrap()
        .iter()
        .filter_map(|e| e["name"].as_str())
        .collect();
    assert!(
        names.contains(&"a2a-groq-agent"),
        "a2a-groq-agent must be in examples list"
    );

    // Act 2 — get_example
    let get_result = client
        .call_tool(
            CallToolRequestParams::new("get_example").with_arguments(args(serde_json::json!({
                "name": "a2a-groq-agent"
            }))),
        )
        .await?;
    assert_ne!(
        get_result.is_error,
        Some(true),
        "get_example should succeed"
    );
    let get_text = extract_text(&get_result).expect("get_example text");
    let get_json: serde_json::Value = serde_json::from_str(&get_text)?;
    assert_eq!(
        get_json["name"].as_str(),
        Some("a2a-groq-agent"),
        "get_example name must match"
    );

    // Act 3 — scaffold
    let scaffold_result = client
        .call_tool(
            CallToolRequestParams::new("scaffold_from_example").with_arguments(args(
                serde_json::json!({
                    "example_name": "a2a-groq-agent",
                    "target_dir": target_dir.to_str().unwrap()
                }),
            )),
        )
        .await?;
    assert_ne!(
        scaffold_result.is_error,
        Some(true),
        "scaffold should succeed: {:?}",
        extract_text(&scaffold_result)
    );

    // Act 4 — read scaffolded TTL and validate it
    let ttl_path = target_dir.join("ontology").join("a2a-groq-agent.ttl");
    assert!(ttl_path.exists(), "scaffolded TTL file must exist");
    let scaffolded_ttl = std::fs::read_to_string(&ttl_path)?;

    let validate_result = client
        .call_tool(
            CallToolRequestParams::new("validate").with_arguments(args(serde_json::json!({
                "ttl": scaffolded_ttl
            }))),
        )
        .await?;
    assert_ne!(
        validate_result.is_error,
        Some(true),
        "validate scaffolded TTL should succeed"
    );
    let validate_text = extract_text(&validate_result).expect("validate text");
    assert!(
        validate_text.contains("Valid"),
        "scaffolded TTL must be valid, got: {}",
        validate_text
    );

    // Act 5 — query for Skills in the ontology
    let sparql =
        "PREFIX agent: <https://ggen.io/examples/a2a-groq#> SELECT ?s WHERE { ?s a agent:Skill }";
    let query_result = client
        .call_tool(
            CallToolRequestParams::new("query_ontology").with_arguments(args(serde_json::json!({
                "ttl": scaffolded_ttl,
                "sparql": sparql
            }))),
        )
        .await?;
    assert_ne!(
        query_result.is_error,
        Some(true),
        "query_ontology should succeed"
    );
    let query_text = extract_text(&query_result).expect("query text");
    let query_json: serde_json::Value = serde_json::from_str(&query_text)?;
    let skill_count = query_json["count"]
        .as_u64()
        .expect("count must be a number");
    assert!(
        skill_count >= 4,
        "expected at least 4 Skills in a2a-groq-agent, got {}",
        skill_count
    );

    // Act 6 — sync with dry_run (create empty queries dir since sync expects it)
    let queries_dir = ttl_path.parent().unwrap().join("queries");
    std::fs::create_dir_all(&queries_dir)?;

    let sync_result = client
        .call_tool(
            CallToolRequestParams::new("sync").with_arguments(args(serde_json::json!({
                "ontology_path": ttl_path.to_str().unwrap(),
                "dry_run": true
            }))),
        )
        .await?;
    assert_ne!(
        sync_result.is_error,
        Some(true),
        "sync dry_run should succeed: {:?}",
        extract_text(&sync_result)
    );
    let sync_text = extract_text(&sync_result).expect("sync text");
    assert!(
        sync_text.contains("dry-run"),
        "sync result should indicate dry-run mode"
    );

    client.cancel().await?;
    Ok(())
}

// ---------------------------------------------------------------------------
// Test 4: Resource browsing
// ---------------------------------------------------------------------------

#[tokio::test]
async fn test_mcp_resource_browsing() -> anyhow::Result<()> {
    // Arrange
    std::env::set_var("GGEN_EXAMPLES_DIR", "/Users/sac/ggen/examples");
    let client = start_server().await?;

    // Act — list resources
    let list_result = client.list_resources(None).await?;
    assert!(
        !list_result.resources.is_empty(),
        "list_resources should return non-empty resource list"
    );

    // Act — read example summary
    let summary_result = client
        .read_resource(ReadResourceRequestParams::new(String::from(
            "ggen://example/a2a-groq-agent",
        )))
        .await?;
    let summary_text: String = summary_result
        .contents
        .iter()
        .find_map(|c| {
            if let ResourceContents::TextResourceContents { text, .. } = c {
                Some(text.clone())
            } else {
                None
            }
        })
        .expect("summary must have text content");
    assert!(
        summary_text.contains("a2a-groq-agent"),
        "resource summary must contain the example name"
    );

    // Act — read TTL content
    let ttl_result = client
        .read_resource(ReadResourceRequestParams::new(String::from(
            "ggen://example/a2a-groq-agent/ttl",
        )))
        .await?;
    let ttl_text: String = ttl_result
        .contents
        .iter()
        .find_map(|c| {
            if let ResourceContents::TextResourceContents { text, .. } = c {
                Some(text.clone())
            } else {
                None
            }
        })
        .expect("ttl resource must have text content");
    assert!(
        ttl_text.contains("@prefix"),
        "TTL resource must contain @prefix"
    );

    // Act — read config content
    let config_result = client
        .read_resource(ReadResourceRequestParams::new(String::from(
            "ggen://example/a2a-groq-agent/config",
        )))
        .await?;
    let config_text: String = config_result
        .contents
        .iter()
        .find_map(|c| {
            if let ResourceContents::TextResourceContents { text, .. } = c {
                Some(text.clone())
            } else {
                None
            }
        })
        .expect("config resource must have text content");
    assert!(
        config_text.contains("[project]"),
        "config resource must contain [project]"
    );

    client.cancel().await?;
    Ok(())
}

// ---------------------------------------------------------------------------
// Test 5: Prompt-assisted generation
// ---------------------------------------------------------------------------

#[tokio::test]
async fn test_mcp_prompt_assisted_generation() -> anyhow::Result<()> {
    // Arrange
    std::env::set_var("GGEN_EXAMPLES_DIR", "/Users/sac/ggen/examples");
    let client = start_server().await?;

    // Act — list prompts
    let list_result = client.list_prompts(None).await?;
    assert_eq!(
        list_result.prompts.len(),
        3,
        "expected exactly 3 prompts, got {}",
        list_result.prompts.len()
    );

    // Act — get generate-from-example prompt
    let prompt_result = client
        .get_prompt(
            GetPromptRequestParams::new("generate-from-example").with_arguments(args(
                serde_json::json!({
                    "example_name": "a2a-groq-agent",
                    "target_domain": "e-commerce"
                }),
            )),
        )
        .await?;

    // Assert
    assert!(
        !prompt_result.messages.is_empty(),
        "generate-from-example must return non-empty messages"
    );
    let message_text = match &prompt_result.messages[0].content {
        PromptMessageContent::Text { text } => text.clone(),
        other => panic!("expected Text content, got: {:?}", other),
    };
    assert!(
        message_text.contains("a2a-groq-agent"),
        "prompt must reference the example name"
    );
    assert!(
        message_text.contains("e-commerce"),
        "prompt must reference the target domain"
    );

    client.cancel().await?;
    Ok(())
}

// ---------------------------------------------------------------------------
// Test 6: Error recovery — invalid TTL then fix and query
// ---------------------------------------------------------------------------

#[tokio::test]
async fn test_mcp_error_recovery() -> anyhow::Result<()> {
    // Arrange
    let client = start_server().await?;

    // Act 1 — validate invalid TTL (should error)
    let invalid_result = client
        .call_tool(
            CallToolRequestParams::new("validate").with_arguments(args(serde_json::json!({
                "ttl": "THIS IS NOT TURTLE @@@INVALID"
            }))),
        )
        .await?;

    // Assert 1
    assert_eq!(
        invalid_result.is_error,
        Some(true),
        "invalid TTL must produce is_error=true"
    );
    let error_text = extract_text(&invalid_result).expect("error result must have text");
    assert!(
        error_text.contains("Invalid TTL"),
        "error text must mention 'Invalid TTL', got: {}",
        error_text
    );

    // Act 2 — validate fixed TTL (should succeed)
    let fixed_ttl = "@prefix ex: <http://example.org/> .\nex:Fixed a ex:Entity .";
    let fixed_result = client
        .call_tool(
            CallToolRequestParams::new("validate").with_arguments(args(serde_json::json!({
                "ttl": fixed_ttl
            }))),
        )
        .await?;

    // Assert 2
    assert_ne!(
        fixed_result.is_error,
        Some(true),
        "fixed TTL should be valid"
    );
    let fixed_text = extract_text(&fixed_result).expect("fixed result must have text");
    assert!(
        fixed_text.contains("Valid"),
        "fixed TTL response must contain 'Valid', got: {}",
        fixed_text
    );

    // Act 3 — query the fixed TTL for the entity
    let sparql = "PREFIX ex: <http://example.org/> SELECT ?s WHERE { ?s a ex:Entity }";
    let query_result = client
        .call_tool(
            CallToolRequestParams::new("query_ontology").with_arguments(args(serde_json::json!({
                "ttl": fixed_ttl,
                "sparql": sparql
            }))),
        )
        .await?;

    // Assert 3
    assert_ne!(
        query_result.is_error,
        Some(true),
        "query on fixed TTL should succeed"
    );
    let query_text = extract_text(&query_result).expect("query result must have text");
    let query_json: serde_json::Value = serde_json::from_str(&query_text)?;
    let count = query_json["count"]
        .as_u64()
        .expect("count must be a number");
    assert_eq!(count, 1, "expected exactly 1 Entity, got {}", count);

    client.cancel().await?;
    Ok(())
}
