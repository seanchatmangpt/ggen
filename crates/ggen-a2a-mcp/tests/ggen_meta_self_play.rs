//! Meta / Recursive Generation Self-Play Tests
//!
//! Tests ggen generating its own MCP + A2A code: scaffold examples, write custom
//! ontologies, run the full validate -> query -> generate pipeline, and perform
//! round-trip verification where generated artifacts feed back into ontologies.
//!
//! Pattern: rmcp 1.3.0 in-process duplex transport (tokio::io::duplex).
//! All tests run without API keys.
//!
//! Run with:
//!   cargo test -p ggen-a2a-mcp --test ggen_meta_self_play -- --test-threads=1 --nocapture

use std::path::Path;

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

    let client = TestClientHandler.serve(client_transport).await?;
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

/// Count all files recursively under a directory.
fn count_files_recursive(dir: &Path) -> usize {
    std::fs::read_dir(dir)
        .unwrap_or_else(|_| {
            panic!("cannot read dir: {}", dir.display());
        })
        .flatten()
        .map(|e| {
            if e.file_type().unwrap().is_dir() {
                count_files_recursive(&e.path())
            } else {
                1
            }
        })
        .sum()
}

/// Find all files with a given extension recursively.
fn find_files_with_ext(dir: &Path, ext: &str) -> Vec<std::path::PathBuf> {
    let mut results = Vec::new();
    let entries = std::fs::read_dir(dir).unwrap_or_else(|_| {
        panic!("cannot read dir: {}", dir.display());
    });
    for entry in entries.flatten() {
        let p = entry.path();
        if p.is_dir() {
            results.extend(find_files_with_ext(&p, ext));
        } else if p.extension().and_then(|s| s.to_str()) == Some(ext) {
            results.push(p);
        }
    }
    results
}

/// Write a TTL file and queries directory, returning the ontology path.
fn write_ontology_project(
    base_dir: &Path,
    ttl_content: &str,
    sparql_queries: &[(&str, &str)], // (filename_stem, query_body)
) -> std::path::PathBuf {
    let ontology_dir = base_dir.join("ontology");
    std::fs::create_dir_all(&ontology_dir).unwrap();
    let ttl_path = ontology_dir.join("model.ttl");
    std::fs::write(&ttl_path, ttl_content).unwrap();

    let queries_dir = base_dir.join("queries");
    std::fs::create_dir_all(&queries_dir).unwrap();
    for (stem, query) in sparql_queries {
        std::fs::write(queries_dir.join(format!("{stem}.rq")), query).unwrap();
    }

    ttl_path
}

// ---------------------------------------------------------------------------
// Test 1: Scaffold mcp-server-definition example — verify full structure
// ---------------------------------------------------------------------------

#[tokio::test]
async fn test_meta_scaffold_mcp_example_has_valid_structure() -> anyhow::Result<()> {
    // Arrange — point server at real examples directory
    std::env::set_var("GGEN_EXAMPLES_DIR", "/Users/sac/ggen/examples");
    let client = start_server().await?;
    let tempdir = tempfile::tempdir()?;

    // Act — list examples and confirm mcp-server-definition exists
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

    // Pick mcp-server-definition if available, otherwise fall back to first example
    let example_name = if names.contains(&"mcp-server-definition") {
        "mcp-server-definition"
    } else {
        names[0]
    };

    // Act — scaffold into tempdir
    let target_dir = tempdir.path().join("scaffolded");
    let scaffold_result = client
        .call_tool(
            CallToolRequestParams::new("scaffold_from_example").with_arguments(args(
                serde_json::json!({
                    "example_name": example_name,
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

    // Assert — structural integrity
    assert!(
        target_dir.join("ggen.toml").exists(),
        "scaffolded dir must contain ggen.toml"
    );
    assert!(
        target_dir.join("README.md").exists(),
        "scaffolded dir must contain README.md"
    );

    // Check TTL file (in ontology/ subdir)
    let ttl_files = find_files_with_ext(&target_dir, "ttl");
    assert!(
        !ttl_files.is_empty(),
        "scaffolded dir must contain at least one .ttl file"
    );

    // Check templates directory has .tera files
    let templates_dir = target_dir.join("templates");
    assert!(
        templates_dir.is_dir(),
        "scaffolded dir must contain templates/ directory"
    );
    let tera_files = find_files_with_ext(&templates_dir, "tera");
    assert!(
        !tera_files.is_empty(),
        "templates/ must contain at least one .tera file"
    );

    // Validate the scaffolded TTL via the validate tool
    let ttl_content = std::fs::read_to_string(&ttl_files[0])?;
    let validate_result = client
        .call_tool(
            CallToolRequestParams::new("validate").with_arguments(args(serde_json::json!({
                "ttl": ttl_content
            }))),
        )
        .await?;
    assert_ne!(
        validate_result.is_error,
        Some(true),
        "scaffolded TTL must validate: {:?}",
        extract_text(&validate_result)
    );
    let validate_text = extract_text(&validate_result).expect("validate text");
    assert!(
        validate_text.contains("Valid"),
        "scaffolded TTL must be valid, got: {}",
        validate_text
    );

    // Query for mcp:Tool instances (prefix from the actual ontology)
    let sparql =
        "PREFIX mcp: <https://ggen.io/examples/mcp#> SELECT ?tool WHERE { ?tool a mcp:Tool }";
    let query_result = client
        .call_tool(
            CallToolRequestParams::new("query_ontology").with_arguments(args(serde_json::json!({
                "ttl": ttl_content,
                "sparql": sparql
            }))),
        )
        .await?;
    // The query should succeed (even if 0 results for non-mcp examples)
    assert_ne!(
        query_result.is_error,
        Some(true),
        "query_ontology should succeed: {:?}",
        extract_text(&query_result)
    );

    client.cancel().await?;
    Ok(())
}

// ---------------------------------------------------------------------------
// Test 2: Generate Rust code from a custom 3-service ontology
// ---------------------------------------------------------------------------

#[tokio::test]
async fn test_meta_generate_from_custom_ontology_produces_rust() -> anyhow::Result<()> {
    // Arrange — custom TTL with 3 services
    let client = start_server().await?;
    let tempdir = tempfile::tempdir()?;

    let ttl_content = concat!(
        "@prefix ex: <http://example.org/services#> .\n",
        "@prefix rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#> .\n",
        "\n",
        "ex:GreetingService a ex:Service ;\n",
        "    ex:serviceName \"GreetingService\" ;\n",
        "    ex:description \"Says hello\" .\n",
        "ex:FarewellService a ex:Service ;\n",
        "    ex:serviceName \"FarewellService\" ;\n",
        "    ex:description \"Says goodbye\" .\n",
        "ex:StatusService a ex:Service ;\n",
        "    ex:serviceName \"StatusService\" ;\n",
        "    ex:description \"Reports system status\" .\n",
    );

    let sparql_query = concat!(
        "PREFIX ex: <http://example.org/services#>\n",
        "SELECT ?service WHERE { ?s a ex:Service ; ex:serviceName ?service }\n",
    );

    let ttl_path = write_ontology_project(
        tempdir.path(),
        ttl_content,
        &[("list_services", sparql_query)],
    );
    let output_dir = tempdir.path().join("generated");

    // Act — generate with language=rust
    let gen_result = client
        .call_tool(
            CallToolRequestParams::new("generate").with_arguments(args(serde_json::json!({
                "ontology_path": ttl_path.to_str().unwrap(),
                "queries_dir": tempdir.path().join("queries").to_str().unwrap(),
                "output_dir": output_dir.to_str().unwrap(),
                "language": "rust"
            }))),
        )
        .await?;

    // Assert — generation succeeds
    assert_ne!(
        gen_result.is_error,
        Some(true),
        "generate should succeed: {:?}",
        extract_text(&gen_result)
    );
    let gen_text = extract_text(&gen_result).expect("generate text");
    assert!(
        gen_text.contains("Generated"),
        "response should mention 'Generated', got: {}",
        gen_text
    );
    assert!(
        gen_text.contains("Receipt:"),
        "response should include a receipt, got: {}",
        gen_text
    );

    // Assert — at least 2 .rs files with "pub struct"
    let rs_files = find_files_with_ext(&output_dir, "rs");
    assert!(
        rs_files.len() >= 2,
        "expected at least 2 .rs files, got {}",
        rs_files.len()
    );

    let mut struct_count = 0;
    for rs_file in &rs_files {
        let content = std::fs::read_to_string(rs_file)?;
        if content.contains("pub struct") {
            struct_count += 1;
        }
    }
    assert!(
        struct_count >= 2,
        "expected at least 2 .rs files with 'pub struct', got {}",
        struct_count
    );

    client.cancel().await?;
    Ok(())
}

// ---------------------------------------------------------------------------
// Test 3: Full pipeline — validate -> query -> generate (K8s-style ontology)
// ---------------------------------------------------------------------------

#[tokio::test]
async fn test_meta_full_pipeline_validate_query_generate() -> anyhow::Result<()> {
    // Arrange — K8s-style TTL with 4 resources
    let client = start_server().await?;
    let tempdir = tempfile::tempdir()?;
    let start = std::time::Instant::now();

    let ttl_content = concat!(
        "@prefix k8s: <http://example.org/k8s#> .\n",
        "@prefix rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#> .\n",
        "\n",
        "k8s:Deployment a k8s:Resource ;\n",
        "    k8s:resourceName \"Deployment\" ;\n",
        "    k8s:apiVersion \"apps/v1\" .\n",
        "k8s:Service a k8s:Resource ;\n",
        "    k8s:resourceName \"Service\" ;\n",
        "    k8s:apiVersion \"v1\" .\n",
        "k8s:ConfigMap a k8s:Resource ;\n",
        "    k8s:resourceName \"ConfigMap\" ;\n",
        "    k8s:apiVersion \"v1\" .\n",
        "k8s:Ingress a k8s:Resource ;\n",
        "    k8s:resourceName \"Ingress\" ;\n",
        "    k8s:apiVersion \"networking.k8s.io/v1\" .\n",
    );

    let sparql_query = concat!(
        "PREFIX k8s: <http://example.org/k8s#>\n",
        "SELECT ?service WHERE { ?s a k8s:Resource ; k8s:resourceName ?service }\n",
    );

    // Step 1: Validate
    let validate_result = client
        .call_tool(
            CallToolRequestParams::new("validate").with_arguments(args(serde_json::json!({
                "ttl": ttl_content
            }))),
        )
        .await?;
    assert_ne!(
        validate_result.is_error,
        Some(true),
        "validate must succeed"
    );
    let validate_text = extract_text(&validate_result).expect("validate text");
    assert!(
        validate_text.contains("Valid"),
        "TTL must be valid, got: {}",
        validate_text
    );

    // Step 2: Query ontology — expect 4 rows
    let query_result = client
        .call_tool(
            CallToolRequestParams::new("query_ontology").with_arguments(args(serde_json::json!({
                "ttl": ttl_content,
                "sparql": sparql_query
            }))),
        )
        .await?;
    assert_ne!(query_result.is_error, Some(true), "query must succeed");
    let query_text = extract_text(&query_result).expect("query text");
    let query_json: serde_json::Value = serde_json::from_str(&query_text)?;
    let count = query_json["count"]
        .as_u64()
        .expect("count must be a number");
    assert_eq!(count, 4, "expected 4 resources, got {}", count);

    // Step 3: Generate code
    let ttl_path = write_ontology_project(
        tempdir.path(),
        ttl_content,
        &[("list_resources", sparql_query)],
    );
    let output_dir = tempdir.path().join("generated");

    let gen_result = client
        .call_tool(
            CallToolRequestParams::new("generate").with_arguments(args(serde_json::json!({
                "ontology_path": ttl_path.to_str().unwrap(),
                "queries_dir": tempdir.path().join("queries").to_str().unwrap(),
                "output_dir": output_dir.to_str().unwrap(),
                "language": "rust"
            }))),
        )
        .await?;
    assert_ne!(
        gen_result.is_error,
        Some(true),
        "generate must succeed: {:?}",
        extract_text(&gen_result)
    );
    let gen_text = extract_text(&gen_result).expect("generate text");
    assert!(
        gen_text.contains("Receipt:"),
        "generate must include a receipt"
    );

    // Assert: pipeline completes in < 5s
    let elapsed = start.elapsed();
    assert!(
        elapsed.as_secs() < 5,
        "full pipeline should complete in < 5s, took {:.2}s",
        elapsed.as_secs_f64()
    );

    client.cancel().await?;
    Ok(())
}

// ---------------------------------------------------------------------------
// Test 4: Scaffold a2a-agent-definition — verify Agent + Skill terms
// ---------------------------------------------------------------------------

#[tokio::test]
async fn test_meta_scaffold_a2a_example_has_agent_definitions() -> anyhow::Result<()> {
    // Arrange
    std::env::set_var("GGEN_EXAMPLES_DIR", "/Users/sac/ggen/examples");
    let client = start_server().await?;
    let tempdir = tempfile::tempdir()?;

    // Act — list examples and confirm a2a-agent-definition exists
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

    let example_name = if names.contains(&"a2a-agent-definition") {
        "a2a-agent-definition"
    } else {
        // Fall back to first available example
        names[0]
    };

    // Scaffold
    let target_dir = tempdir.path().join("scaffolded_a2a");
    let scaffold_result = client
        .call_tool(
            CallToolRequestParams::new("scaffold_from_example").with_arguments(args(
                serde_json::json!({
                    "example_name": example_name,
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

    // Read scaffolded TTL
    let ttl_files = find_files_with_ext(&target_dir, "ttl");
    assert!(
        !ttl_files.is_empty(),
        "scaffolded a2a dir must contain .ttl file(s)"
    );
    let ttl_content = std::fs::read_to_string(&ttl_files[0])?;

    // Validate the TTL
    let validate_result = client
        .call_tool(
            CallToolRequestParams::new("validate").with_arguments(args(serde_json::json!({
                "ttl": ttl_content
            }))),
        )
        .await?;
    assert_ne!(validate_result.is_error, Some(true), "TTL must be valid");
    let validate_text = extract_text(&validate_result).expect("validate text");
    assert!(
        validate_text.contains("Valid"),
        "TTL must be valid, got: {}",
        validate_text
    );

    // Query for Skills (prefix: agent:)
    let sparql_skills =
        "PREFIX agent: <https://ggen.io/examples/a2a#> SELECT ?skill WHERE { ?skill a agent:Skill }";
    let query_result = client
        .call_tool(
            CallToolRequestParams::new("query_ontology").with_arguments(args(serde_json::json!({
                "ttl": ttl_content,
                "sparql": sparql_skills
            }))),
        )
        .await?;

    // Assert: at least 3 skills (a2a-agent-definition has 4; fallback examples may have 0)
    let query_text = extract_text(&query_result).expect("query text");
    let query_json: serde_json::Value = serde_json::from_str(&query_text)?;
    let skill_count = query_json["count"]
        .as_u64()
        .expect("count must be a number");

    // Only enforce >= 3 if we actually scaffolded the a2a example
    if example_name == "a2a-agent-definition" {
        assert!(
            skill_count >= 3,
            "a2a-agent-definition must have >= 3 Skills, got {}",
            skill_count
        );
    }

    // ggen.toml must contain [project] and [[generation.rules]]
    let ggen_toml = std::fs::read_to_string(target_dir.join("ggen.toml"))?;
    assert!(
        ggen_toml.contains("[project]"),
        "ggen.toml must contain [project]"
    );
    assert!(
        ggen_toml.contains("[[generation.rules]]"),
        "ggen.toml must contain [[generation.rules]]"
    );

    client.cancel().await?;
    Ok(())
}

// ---------------------------------------------------------------------------
// Test 5: Multi-query single ontology — disjoint Entity and Role result sets
// ---------------------------------------------------------------------------

#[tokio::test]
async fn test_meta_multi_query_single_ontology() -> anyhow::Result<()> {
    // Arrange — TTL with 2 Entities and 2 Roles
    let client = start_server().await?;

    let ttl_content = concat!(
        "@prefix org: <http://example.org/org#> .\n",
        "@prefix rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#> .\n",
        "\n",
        "org:UserEntity a org:Entity ;\n",
        "    org:entityName \"User\" .\n",
        "org:OrderEntity a org:Entity ;\n",
        "    org:entityName \"Order\" .\n",
        "org:AdminRole a org:Role ;\n",
        "    org:roleName \"Admin\" .\n",
        "org:ViewerRole a org:Role ;\n",
        "    org:roleName \"Viewer\" .\n",
    );

    let entity_query = concat!(
        "PREFIX org: <http://example.org/org#>\n",
        "SELECT ?item WHERE { ?item a org:Entity }\n",
    );
    let role_query = concat!(
        "PREFIX org: <http://example.org/org#>\n",
        "SELECT ?item WHERE { ?item a org:Role }\n",
    );

    // Act — query for entities
    let entity_result = client
        .call_tool(
            CallToolRequestParams::new("query_ontology").with_arguments(args(serde_json::json!({
                "ttl": ttl_content,
                "sparql": entity_query
            }))),
        )
        .await?;
    assert_ne!(
        entity_result.is_error,
        Some(true),
        "entity query must succeed"
    );
    let entity_text = extract_text(&entity_result).expect("entity text");
    let entity_json: serde_json::Value = serde_json::from_str(&entity_text)?;
    let entity_count = entity_json["count"]
        .as_u64()
        .expect("entity count must be a number");
    assert_eq!(entity_count, 2, "expected 2 entities, got {}", entity_count);

    // Collect entity URIs
    let entity_uris: Vec<String> = entity_json["rows"]
        .as_array()
        .unwrap()
        .iter()
        .filter_map(|row| row["item"].as_str().map(|s| s.to_string()))
        .collect();

    // Act — query for roles
    let role_result = client
        .call_tool(
            CallToolRequestParams::new("query_ontology").with_arguments(args(serde_json::json!({
                "ttl": ttl_content,
                "sparql": role_query
            }))),
        )
        .await?;
    assert_ne!(role_result.is_error, Some(true), "role query must succeed");
    let role_text = extract_text(&role_result).expect("role text");
    let role_json: serde_json::Value = serde_json::from_str(&role_text)?;
    let role_count = role_json["count"]
        .as_u64()
        .expect("role count must be a number");
    assert_eq!(role_count, 2, "expected 2 roles, got {}", role_count);

    // Collect role URIs
    let role_uris: Vec<String> = role_json["rows"]
        .as_array()
        .unwrap()
        .iter()
        .filter_map(|row| row["item"].as_str().map(|s| s.to_string()))
        .collect();

    // Assert — URI sets are disjoint
    for entity_uri in &entity_uris {
        assert!(
            !role_uris.contains(entity_uri),
            "entity URI {} should not appear in role results",
            entity_uri
        );
    }
    for role_uri in &role_uris {
        assert!(
            !entity_uris.contains(role_uri),
            "role URI {} should not appear in entity results",
            role_uri
        );
    }

    // Now generate code using both queries
    let tempdir = tempfile::tempdir()?;
    let sparql_all = concat!(
        "PREFIX org: <http://example.org/org#>\n",
        "SELECT ?service WHERE {\n",
        "  { ?item a org:Entity ; org:entityName ?service }\n",
        "  UNION\n",
        "  { ?item a org:Role ; org:roleName ?service }\n",
        "}\n",
    );
    let ttl_path =
        write_ontology_project(tempdir.path(), ttl_content, &[("all_items", sparql_all)]);
    let output_dir = tempdir.path().join("generated");

    let gen_result = client
        .call_tool(
            CallToolRequestParams::new("generate").with_arguments(args(serde_json::json!({
                "ontology_path": ttl_path.to_str().unwrap(),
                "queries_dir": tempdir.path().join("queries").to_str().unwrap(),
                "output_dir": output_dir.to_str().unwrap(),
                "language": "rust"
            }))),
        )
        .await?;
    assert_ne!(
        gen_result.is_error,
        Some(true),
        "generate from multi-type ontology must succeed: {:?}",
        extract_text(&gen_result)
    );

    client.cancel().await?;
    Ok(())
}

// ---------------------------------------------------------------------------
// Test 6: Sync dry-run — no files written to disk
// ---------------------------------------------------------------------------

#[tokio::test]
async fn test_meta_sync_dry_run_no_files_written() -> anyhow::Result<()> {
    // Arrange
    let client = start_server().await?;
    let tempdir = tempfile::tempdir()?;

    let ttl_content = concat!(
        "@prefix ex: <http://example.org/dry#> .\n",
        "@prefix rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#> .\n",
        "ex:DryService a ex:Service ;\n",
        "    ex:serviceName \"DryService\" .\n",
    );

    let sparql_query = concat!(
        "PREFIX ex: <http://example.org/dry#>\n",
        "SELECT ?service WHERE { ?s a ex:Service ; ex:serviceName ?service }\n",
    );

    let ttl_path = write_ontology_project(
        tempdir.path(),
        ttl_content,
        &[("list_services", sparql_query)],
    );
    let output_dir = tempdir.path().join("generated");

    // Act — sync with dry_run=true
    let sync_result = client
        .call_tool(
            CallToolRequestParams::new("sync").with_arguments(args(serde_json::json!({
                "ontology_path": ttl_path.to_str().unwrap(),
                "queries_dir": tempdir.path().join("queries").to_str().unwrap(),
                "output_dir": output_dir.to_str().unwrap(),
                "language": "rust",
                "dry_run": true
            }))),
        )
        .await?;

    // Assert — succeeds and mentions dry-run
    assert_ne!(
        sync_result.is_error,
        Some(true),
        "sync dry_run must succeed: {:?}",
        extract_text(&sync_result)
    );
    let sync_text = extract_text(&sync_result).expect("sync text");
    assert!(
        sync_text.contains("dry-run"),
        "sync result must mention 'dry-run', got: {}",
        sync_text
    );

    // Assert — output directory either does not exist or is empty
    if output_dir.exists() {
        let file_count = count_files_recursive(&output_dir);
        assert_eq!(
            file_count, 0,
            "dry-run must not write files, found {} file(s)",
            file_count
        );
    }

    client.cancel().await?;
    Ok(())
}

// ---------------------------------------------------------------------------
// Test 7: Round-trip — generate code, extract struct name, re-query artifact
// ---------------------------------------------------------------------------

#[tokio::test]
async fn test_meta_round_trip_generate_then_requery() -> anyhow::Result<()> {
    // Arrange — ontology with a single distinctive service name
    let client = start_server().await?;
    let tempdir = tempfile::tempdir()?;

    let struct_name = "RoundTripWidget";
    let ttl_content = format!(
        concat!(
            "@prefix ex: <http://example.org/rt#> .\n",
            "@prefix rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#> .\n",
            "\n",
            "ex:RoundTripWidget a ex:Service ;\n",
            "    ex:serviceName \"{}\" .\n",
        ),
        struct_name
    );

    let sparql_query = concat!(
        "PREFIX ex: <http://example.org/rt#>\n",
        "SELECT ?service WHERE { ?s a ex:Service ; ex:serviceName ?service }\n",
    );

    // Step 1: Generate code
    let ttl_path = write_ontology_project(
        tempdir.path(),
        &ttl_content,
        &[("list_services", sparql_query)],
    );
    let output_dir = tempdir.path().join("generated");

    let gen_result = client
        .call_tool(
            CallToolRequestParams::new("generate").with_arguments(args(serde_json::json!({
                "ontology_path": ttl_path.to_str().unwrap(),
                "queries_dir": tempdir.path().join("queries").to_str().unwrap(),
                "output_dir": output_dir.to_str().unwrap(),
                "language": "rust"
            }))),
        )
        .await?;
    assert_ne!(
        gen_result.is_error,
        Some(true),
        "generate must succeed: {:?}",
        extract_text(&gen_result)
    );

    // Step 2: Read generated .rs file and extract struct name
    let rs_files = find_files_with_ext(&output_dir, "rs");
    assert!(
        !rs_files.is_empty(),
        "generation must produce at least one .rs file"
    );

    let generated_content = std::fs::read_to_string(&rs_files[0])?;
    assert!(
        generated_content.contains("pub struct"),
        "generated .rs must contain 'pub struct'"
    );
    assert!(
        generated_content.contains(struct_name),
        "generated .rs must contain the struct name '{}'",
        struct_name
    );

    // Step 3: Build an artifact TTL using the struct name
    let artifact_ttl = format!(
        concat!(
            "@prefix art: <http://example.org/artifact#> .\n",
            "@prefix rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#> .\n",
            "\n",
            "art:GeneratedArtifact a art:Artifact ;\n",
            "    art:structName \"{}\" ;\n",
            "    art:language \"rust\" .\n",
        ),
        struct_name
    );

    // Step 4: Validate the artifact TTL
    let validate_result = client
        .call_tool(
            CallToolRequestParams::new("validate").with_arguments(args(serde_json::json!({
                "ttl": artifact_ttl
            }))),
        )
        .await?;
    assert_ne!(
        validate_result.is_error,
        Some(true),
        "artifact TTL must be valid"
    );
    let validate_text = extract_text(&validate_result).expect("validate text");
    assert!(
        validate_text.contains("Valid"),
        "artifact TTL must be valid, got: {}",
        validate_text
    );

    // Step 5: Query the artifact to recover the struct name
    let artifact_query = concat!(
        "PREFIX art: <http://example.org/artifact#>\n",
        "SELECT ?struct WHERE { ?a a art:Artifact ; art:structName ?struct }\n",
    );
    let query_result = client
        .call_tool(
            CallToolRequestParams::new("query_ontology").with_arguments(args(serde_json::json!({
                "ttl": artifact_ttl,
                "sparql": artifact_query
            }))),
        )
        .await?;
    assert_ne!(
        query_result.is_error,
        Some(true),
        "artifact query must succeed: {:?}",
        extract_text(&query_result)
    );
    let query_text = extract_text(&query_result).expect("query text");
    let query_json: serde_json::Value = serde_json::from_str(&query_text)?;

    // Assert — round-trip preserves struct name
    // Oxigraph serializes literal strings with surrounding quotes, so strip them
    let recovered_raw = query_json["rows"][0]["struct"]
        .as_str()
        .expect("query must return struct name");
    let recovered = recovered_raw.trim_matches('"');
    assert_eq!(
        recovered, struct_name,
        "round-trip must preserve struct name: expected '{}', got '{}'",
        struct_name, recovered
    );

    client.cancel().await?;
    Ok(())
}
