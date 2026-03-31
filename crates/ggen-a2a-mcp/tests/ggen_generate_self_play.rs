//! MCP Self-Play Tests — generate tool success path
//!
//! 8 tests exercising ggen's `generate` tool end-to-end via rmcp 1.3.0
//! in-process duplex transport. Each test creates its own tempdir + server.
//!
//! Run with:
//!   cargo test -p ggen-a2a-mcp --test ggen_generate_self_play -- --test-threads=1 --nocapture

use std::path::{Path, PathBuf};

use ggen_a2a_mcp::ggen_server::GgenMcpServer;
use rmcp::{model::*, service::RunningService, ClientHandler, RoleClient, ServiceExt};

// ---------------------------------------------------------------------------
// Shared infrastructure
// ---------------------------------------------------------------------------

#[derive(Debug, Clone, Default)]
struct TestClientHandler;

impl ClientHandler for TestClientHandler {
    fn get_info(&self) -> ClientInfo {
        ClientInfo::default()
    }
}

async fn start_server() -> anyhow::Result<RunningService<RoleClient, TestClientHandler>> {
    let (server_transport, client_transport) = tokio::io::duplex(65536);

    let server = GgenMcpServer::new();
    tokio::spawn(async move {
        let _ = server.serve(server_transport).await;
    });

    let client = TestClientHandler.serve(client_transport).await?;
    Ok(client)
}

fn extract_text(result: &CallToolResult) -> Option<String> {
    result.content.iter().find_map(|c| {
        if let RawContent::Text(tc) = &c.raw {
            Some(tc.text.clone())
        } else {
            None
        }
    })
}

fn args(json: serde_json::Value) -> serde_json::Map<String, serde_json::Value> {
    json.as_object().cloned().unwrap_or_default()
}

// ---------------------------------------------------------------------------
// Ontology fixture — BookService + AuthorService, Book + Author entities
// ---------------------------------------------------------------------------

const BOOK_AUTHOR_TTL: &str = concat!(
    "@prefix ex: <http://example.org/books#> .\n",
    "@prefix rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#> .\n",
    "@prefix rdfs: <http://www.w3.org/2000/01/rdf-schema#> .\n",
    "@prefix owl: <http://www.w3.org/2002/07/owl#> .\n",
    "\n",
    "# Services\n",
    "ex:BookService a ex:Service ;\n",
    "    rdfs:label \"BookService\" ;\n",
    "    ex:version \"1.0.0\" .\n",
    "\n",
    "ex:AuthorService a ex:Service ;\n",
    "    rdfs:label \"AuthorService\" ;\n",
    "    ex:version \"1.0.0\" .\n",
    "\n",
    "# Entities\n",
    "ex:Book a owl:Class ;\n",
    "    rdfs:label \"Book\" ;\n",
    "    ex:field \"title\", \"isbn\", \"author_id\" .\n",
    "\n",
    "ex:Author a owl:Class ;\n",
    "    rdfs:label \"Author\" ;\n",
    "    ex:field \"name\", \"email\" .\n",
);

// ---------------------------------------------------------------------------
// SPARQL queries
// ---------------------------------------------------------------------------

const SERVICE_SELECT_SPARQL: &str = concat!(
    "PREFIX ex: <http://example.org/books#>\n",
    "PREFIX rdfs: <http://www.w3.org/2000/01/rdf-schema#>\n",
    "SELECT ?service ?name ?version WHERE {\n",
    "  ?service a ex:Service .\n",
    "  ?service rdfs:label ?name .\n",
    "  ?service ex:version ?version .\n",
    "}\n",
);

const ENTITY_SELECT_SPARQL: &str = concat!(
    "PREFIX ex: <http://example.org/books#>\n",
    "PREFIX rdfs: <http://www.w3.org/2000/01/rdf-schema#>\n",
    "PREFIX owl: <http://www.w3.org/2002/07/owl#>\n",
    "SELECT ?entity ?label ?field WHERE {\n",
    "  ?entity a owl:Class .\n",
    "  ?entity rdfs:label ?label .\n",
    "  ?entity ex:field ?field .\n",
    "}\n",
);

// ---------------------------------------------------------------------------
// Project setup helper
// ---------------------------------------------------------------------------

/// Write ontology TTL + SPARQL .rq files into tempdir.
/// Returns (ontology_path, queries_dir, output_dir).
fn setup_generate_project(
    dir: &Path, ttl_content: &str, sparql_queries: &[(&str, &str)],
) -> (PathBuf, PathBuf, PathBuf) {
    let queries_dir = dir.join("queries");
    let output_dir = dir.join("generated");
    std::fs::create_dir_all(&queries_dir).expect("create queries dir");
    std::fs::create_dir_all(&output_dir).expect("create output dir");

    // Write ontology
    let ontology_path = dir.join("ontology.ttl");
    std::fs::write(&ontology_path, ttl_content).expect("write ttl");

    // Write SPARQL query files
    for (filename, sparql) in sparql_queries {
        std::fs::write(queries_dir.join(filename), sparql).expect("write .rq file");
    }

    (ontology_path, queries_dir, output_dir)
}

// ---------------------------------------------------------------------------
// Test 1: generate produces valid code from real ontology
// ---------------------------------------------------------------------------

#[tokio::test]
async fn test_generate_produces_valid_code_from_real_ontology() -> anyhow::Result<()> {
    // Arrange
    let client = start_server().await?;
    let tempdir = tempfile::tempdir()?;
    let queries = [("01_services.rq", SERVICE_SELECT_SPARQL)];
    let (ontology_path, queries_dir, output_dir) =
        setup_generate_project(tempdir.path(), BOOK_AUTHOR_TTL, &queries);

    // Act — validate first, then generate
    let validate_result = client
        .call_tool(
            CallToolRequestParams::new("validate").with_arguments(args(serde_json::json!({
                "ttl": BOOK_AUTHOR_TTL
            }))),
        )
        .await?;
    assert_ne!(
        validate_result.is_error,
        Some(true),
        "validate must succeed before generate"
    );

    let generate_result = client
        .call_tool(
            CallToolRequestParams::new("generate").with_arguments(args(serde_json::json!({
                "ontology_path": ontology_path.to_str().unwrap(),
                "queries_dir": queries_dir.to_str().unwrap(),
                "output_dir": output_dir.to_str().unwrap(),
                "language": "rust"
            }))),
        )
        .await?;

    // Assert — generate succeeds
    assert_ne!(
        generate_result.is_error,
        Some(true),
        "generate should succeed: {:?}",
        extract_text(&generate_result)
    );
    let text = extract_text(&generate_result).expect("generate must return text");
    assert!(
        text.contains("Generated"),
        "response must contain 'Generated', got: {}",
        text
    );

    // Assert — SHA256 receipt present (64 hex chars)
    assert!(
        text.contains("Receipt: "),
        "response must contain receipt, got: {}",
        text
    );
    let receipt_start = text.find("Receipt: ").unwrap() + "Receipt: ".len();
    let receipt_line = text[receipt_start..].lines().next().unwrap_or("");
    assert!(
        receipt_line.len() >= 64,
        "receipt must be at least 64 hex chars, got {} chars: {}",
        receipt_line.len(),
        receipt_line
    );
    assert!(
        receipt_line.chars().all(|c| c.is_ascii_hexdigit()),
        "receipt must be hex, got: {}",
        receipt_line
    );

    // Assert — .rs files on disk
    let rs_files: Vec<PathBuf> = std::fs::read_dir(&output_dir)
        .expect("output dir must exist")
        .flatten()
        .filter(|e| e.path().extension().and_then(|ext| ext.to_str()) == Some("rs"))
        .map(|e| e.path())
        .collect();
    assert!(
        !rs_files.is_empty(),
        "output_dir must contain at least one .rs file"
    );

    client.cancel().await?;
    Ok(())
}

// ---------------------------------------------------------------------------
// Test 2: generate Rust struct code from RDF schema
// ---------------------------------------------------------------------------

#[tokio::test]
async fn test_generate_rust_struct_code_from_rdf_schema() -> anyhow::Result<()> {
    // Arrange
    let client = start_server().await?;
    let tempdir = tempfile::tempdir()?;
    let queries = [("01_services.rq", SERVICE_SELECT_SPARQL)];
    let (ontology_path, queries_dir, output_dir) =
        setup_generate_project(tempdir.path(), BOOK_AUTHOR_TTL, &queries);

    // Act
    let result = client
        .call_tool(
            CallToolRequestParams::new("generate").with_arguments(args(serde_json::json!({
                "ontology_path": ontology_path.to_str().unwrap(),
                "queries_dir": queries_dir.to_str().unwrap(),
                "output_dir": output_dir.to_str().unwrap(),
                "language": "rust"
            }))),
        )
        .await?;

    // Assert — success
    assert_ne!(result.is_error, Some(true), "generate rust should succeed");

    // Assert — .rs files exist and contain expected Rust patterns
    let rs_files: Vec<String> = std::fs::read_dir(&output_dir)
        .expect("output dir must exist")
        .flatten()
        .filter(|e| e.path().extension().and_then(|ext| ext.to_str()) == Some("rs"))
        .map(|e| std::fs::read_to_string(e.path()).expect("read .rs file"))
        .collect();

    assert!(!rs_files.is_empty(), "must generate at least one .rs file");

    let combined = rs_files.join("\n");
    assert!(
        combined.contains("struct"),
        "generated Rust code must contain 'struct'"
    );
    assert!(
        combined.contains("#[derive"),
        "generated Rust code must contain '#[derive'"
    );

    client.cancel().await?;
    Ok(())
}

// ---------------------------------------------------------------------------
// Test 3: generate Python dataclass from ontology
// ---------------------------------------------------------------------------

#[tokio::test]
async fn test_generate_python_dataclass_from_ontology() -> anyhow::Result<()> {
    // Arrange
    let client = start_server().await?;
    let tempdir = tempfile::tempdir()?;
    let queries = [("01_services.rq", SERVICE_SELECT_SPARQL)];
    let (ontology_path, queries_dir, output_dir) =
        setup_generate_project(tempdir.path(), BOOK_AUTHOR_TTL, &queries);

    // Act
    let result = client
        .call_tool(
            CallToolRequestParams::new("generate").with_arguments(args(serde_json::json!({
                "ontology_path": ontology_path.to_str().unwrap(),
                "queries_dir": queries_dir.to_str().unwrap(),
                "output_dir": output_dir.to_str().unwrap(),
                "language": "python"
            }))),
        )
        .await?;

    // Assert — success
    assert_ne!(
        result.is_error,
        Some(true),
        "generate python should succeed: {:?}",
        extract_text(&result)
    );

    // Assert — .py files exist and contain @dataclass and class
    let py_files: Vec<String> = std::fs::read_dir(&output_dir)
        .expect("output dir must exist")
        .flatten()
        .filter(|e| e.path().extension().and_then(|ext| ext.to_str()) == Some("py"))
        .map(|e| std::fs::read_to_string(e.path()).expect("read .py file"))
        .collect();

    assert!(!py_files.is_empty(), "must generate at least one .py file");

    let combined = py_files.join("\n");
    assert!(
        combined.contains("@dataclass"),
        "generated Python code must contain '@dataclass'"
    );
    assert!(
        combined.contains("class "),
        "generated Python code must contain 'class'"
    );

    client.cancel().await?;
    Ok(())
}

// ---------------------------------------------------------------------------
// Test 4: scaffold example then validate project structure
// ---------------------------------------------------------------------------

#[tokio::test]
async fn test_scaffold_mcp_example_has_valid_project_structure() -> anyhow::Result<()> {
    // Arrange — point at examples dir
    std::env::set_var("GGEN_EXAMPLES_DIR", "/Users/sac/ggen/examples");
    let client = start_server().await?;
    let tempdir = tempfile::tempdir()?;

    // Act — list_examples to find a real example
    let list_result = client
        .call_tool(CallToolRequestParams::new("list_examples"))
        .await?;
    assert_ne!(
        list_result.is_error,
        Some(true),
        "list_examples should not error"
    );
    let list_text = extract_text(&list_result).expect("list_examples text");
    let list_json: serde_json::Value = serde_json::from_str(&list_text)?;
    let examples = list_json["examples"]
        .as_array()
        .expect("examples should be an array");
    assert!(
        !examples.is_empty(),
        "need at least one example to scaffold"
    );

    let example_name = examples[0]["name"]
        .as_str()
        .expect("example must have a name");

    // Act — scaffold
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

    // Assert — ggen.toml exists
    assert!(
        target_dir.join("ggen.toml").exists(),
        "scaffolded dir must contain ggen.toml"
    );

    // Assert — .ttl files exist
    let ttl_files: Vec<PathBuf> = walk_dir_for_ext(&target_dir, "ttl");
    assert!(
        !ttl_files.is_empty(),
        "scaffolded dir must contain at least one .ttl file"
    );

    // Assert — file count >= 2 (ggen.toml + at least one other)
    let all_files: Vec<PathBuf> = walk_dir_all(&target_dir);
    assert!(
        all_files.len() >= 2,
        "scaffolded project must have >= 2 files, got {}",
        all_files.len()
    );

    // Assert — scaffolded TTL validates
    let scaffolded_ttl = find_any_ttl_content(&target_dir);
    assert!(
        !scaffolded_ttl.is_empty(),
        "scaffolded project must contain TTL content"
    );
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
        "scaffolded TTL must be valid"
    );
    let validate_text = extract_text(&validate_result).expect("validate text");
    assert!(
        validate_text.contains("Valid"),
        "scaffolded TTL must pass validation, got: {}",
        validate_text
    );

    client.cancel().await?;
    Ok(())
}

// ---------------------------------------------------------------------------
// Test 5: validate correct ontology with varying complexity
// ---------------------------------------------------------------------------

#[tokio::test]
async fn test_validate_correct_ontology_succeeds() -> anyhow::Result<()> {
    // Arrange
    let client = start_server().await?;

    // Simple ontology — 2 triples
    let simple_ttl = concat!(
        "@prefix ex: <http://example.org/> .\n",
        "ex:A a ex:Type .\n",
        "ex:B a ex:Type .\n",
    );

    // Complex ontology — 10+ triples
    let complex_ttl = concat!(
        "@prefix ex: <http://example.org/books#> .\n",
        "@prefix rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#> .\n",
        "@prefix rdfs: <http://www.w3.org/2000/01/rdf-schema#> .\n",
        "@prefix owl: <http://www.w3.org/2002/07/owl#> .\n",
        "ex:BookService a ex:Service .\n",
        "ex:AuthorService a ex:Service .\n",
        "ex:ReviewService a ex:Service .\n",
        "ex:Book a owl:Class ; rdfs:label \"Book\" .\n",
        "ex:Author a owl:Class ; rdfs:label \"Author\" .\n",
        "ex:Review a owl:Class ; rdfs:label \"Review\" .\n",
        "ex:BookService rdfs:label \"BookService\" .\n",
        "ex:AuthorService rdfs:label \"AuthorService\" .\n",
        "ex:ReviewService rdfs:label \"ReviewService\" .\n",
    );

    // Act + Assert — simple
    let simple_result = client
        .call_tool(
            CallToolRequestParams::new("validate").with_arguments(args(serde_json::json!({
                "ttl": simple_ttl
            }))),
        )
        .await?;
    assert_ne!(
        simple_result.is_error,
        Some(true),
        "simple TTL must validate"
    );
    let simple_text = extract_text(&simple_result).expect("simple validate text");
    assert!(
        simple_text.contains("Valid"),
        "simple TTL must be Valid, got: {}",
        simple_text
    );

    // Act + Assert — complex
    let complex_result = client
        .call_tool(
            CallToolRequestParams::new("validate").with_arguments(args(serde_json::json!({
                "ttl": complex_ttl
            }))),
        )
        .await?;
    assert_ne!(
        complex_result.is_error,
        Some(true),
        "complex TTL must validate"
    );
    let complex_text = extract_text(&complex_result).expect("complex validate text");
    assert!(
        complex_text.contains("Valid"),
        "complex TTL must be Valid, got: {}",
        complex_text
    );

    // Assert — complex has more triples than simple
    let simple_triples = extract_triple_count(&simple_text);
    let complex_triples = extract_triple_count(&complex_text);
    assert!(
        complex_triples > simple_triples,
        "complex ontology ({}) must have more triples than simple ({})",
        complex_triples,
        simple_triples
    );

    client.cancel().await?;
    Ok(())
}

// ---------------------------------------------------------------------------
// Test 6: query_ontology returns expected results
// ---------------------------------------------------------------------------

#[tokio::test]
async fn test_query_ontology_returns_expected_results() -> anyhow::Result<()> {
    // Arrange
    let client = start_server().await?;

    // Act — query services
    let result = client
        .call_tool(
            CallToolRequestParams::new("query_ontology").with_arguments(args(serde_json::json!({
                "ttl": BOOK_AUTHOR_TTL,
                "sparql": SERVICE_SELECT_SPARQL
            }))),
        )
        .await?;

    // Assert
    assert_ne!(result.is_error, Some(true), "query_ontology should succeed");
    let text = extract_text(&result).expect("query_ontology text");
    let json: serde_json::Value = serde_json::from_str(&text)?;

    // JSON has "rows" and "count" keys
    assert!(
        json.get("rows").is_some(),
        "result JSON must have 'rows' key"
    );
    assert!(
        json.get("count").is_some(),
        "result JSON must have 'count' key"
    );

    // count = 2 (BookService + AuthorService)
    let count = json["count"].as_u64().expect("count must be a number");
    assert_eq!(count, 2, "expected 2 services, got {}", count);

    // Rows have expected variable bindings
    let rows = json["rows"].as_array().expect("rows must be an array");
    assert_eq!(rows.len(), 2, "expected 2 rows");
    for row in rows {
        assert!(
            row.get("service").is_some(),
            "each row must have 'service' binding"
        );
        assert!(
            row.get("name").is_some(),
            "each row must have 'name' binding"
        );
        assert!(
            row.get("version").is_some(),
            "each row must have 'version' binding"
        );
    }

    // Verify one row contains BookService
    let all_text = serde_json::to_string(&rows)?;
    assert!(
        all_text.contains("BookService"),
        "results must contain BookService"
    );

    client.cancel().await?;
    Ok(())
}

// ---------------------------------------------------------------------------
// Test 7: generate with inference rules (pre-materialized triples)
// ---------------------------------------------------------------------------

#[tokio::test]
async fn test_generate_with_inference_rules() -> anyhow::Result<()> {
    // Arrange
    let client = start_server().await?;
    let tempdir = tempfile::tempdir()?;

    // Act 1 — CONSTRUCT query should be rejected (only SELECT supported)
    let construct_sparql = concat!(
        "PREFIX ex: <http://example.org/books#>\n",
        "CONSTRUCT { ?s a ex:InferredService } WHERE { ?s a ex:Service }\n",
    );
    let construct_result = client
        .call_tool(
            CallToolRequestParams::new("query_ontology").with_arguments(args(serde_json::json!({
                "ttl": BOOK_AUTHOR_TTL,
                "sparql": construct_sparql
            }))),
        )
        .await?;

    // Assert 1 — CONSTRUCT must produce an error
    assert_eq!(
        construct_result.is_error,
        Some(true),
        "CONSTRUCT query should return is_error=true"
    );

    // Arrange 2 — enriched TTL with pre-materialized inference triples
    let enriched_ttl = concat!(
        "@prefix ex: <http://example.org/books#> .\n",
        "@prefix rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#> .\n",
        "@prefix rdfs: <http://www.w3.org/2000/01/rdf-schema#> .\n",
        "@prefix owl: <http://www.w3.org/2002/07/owl#> .\n",
        "\n",
        "# Services\n",
        "ex:BookService a ex:Service ;\n",
        "    rdfs:label \"BookService\" ;\n",
        "    ex:version \"1.0.0\" .\n",
        "\n",
        "ex:AuthorService a ex:Service ;\n",
        "    rdfs:label \"AuthorService\" ;\n",
        "    ex:version \"1.0.0\" .\n",
        "\n",
        "# Entities\n",
        "ex:Book a owl:Class ;\n",
        "    rdfs:label \"Book\" ;\n",
        "    ex:field \"title\", \"isbn\", \"author_id\" .\n",
        "\n",
        "ex:Author a owl:Class ;\n",
        "    rdfs:label \"Author\" ;\n",
        "    ex:field \"name\", \"email\" .\n",
        "\n",
        "# Pre-materialized inference triples\n",
        "ex:BookService a ex:Microservice .\n",
        "ex:AuthorService a ex:Microservice .\n",
        "ex:BookService ex:dependsOn ex:AuthorService .\n",
    );

    let inference_sparql = concat!(
        "PREFIX ex: <http://example.org/books#>\n",
        "PREFIX rdfs: <http://www.w3.org/2000/01/rdf-schema#>\n",
        "SELECT ?service ?type WHERE {\n",
        "  ?service a ex:Microservice .\n",
        "  ?service rdfs:label ?type .\n",
        "}\n",
    );

    // Act 2 — query the enriched ontology for inferred types
    let query_result = client
        .call_tool(
            CallToolRequestParams::new("query_ontology").with_arguments(args(serde_json::json!({
                "ttl": enriched_ttl,
                "sparql": inference_sparql
            }))),
        )
        .await?;

    // Assert 2 — inference query succeeds
    assert_ne!(
        query_result.is_error,
        Some(true),
        "inference SELECT query should succeed"
    );
    let query_text = extract_text(&query_result).expect("inference query text");
    let query_json: serde_json::Value = serde_json::from_str(&query_text)?;
    let inferred_count = query_json["count"]
        .as_u64()
        .expect("count must be a number");
    assert!(
        inferred_count >= 2,
        "expected >= 2 inferred Microservices, got {}",
        inferred_count
    );

    // Act 3 — generate from the enriched ontology
    let queries = [
        ("01_services.rq", SERVICE_SELECT_SPARQL),
        ("02_inferred.rq", inference_sparql),
    ];
    let (ontology_path, queries_dir, output_dir) =
        setup_generate_project(tempdir.path(), enriched_ttl, &queries);

    let generate_result = client
        .call_tool(
            CallToolRequestParams::new("generate").with_arguments(args(serde_json::json!({
                "ontology_path": ontology_path.to_str().unwrap(),
                "queries_dir": queries_dir.to_str().unwrap(),
                "output_dir": output_dir.to_str().unwrap(),
                "language": "rust"
            }))),
        )
        .await?;

    // Assert 3 — generate from enriched ontology succeeds
    assert_ne!(
        generate_result.is_error,
        Some(true),
        "generate from enriched ontology should succeed: {:?}",
        extract_text(&generate_result)
    );
    let gen_text = extract_text(&generate_result).expect("generate text");
    assert!(
        gen_text.contains("Generated"),
        "enriched generate response must contain 'Generated'"
    );

    client.cancel().await?;
    Ok(())
}

// ---------------------------------------------------------------------------
// Test 8: generate output contains no template artifacts
// ---------------------------------------------------------------------------

#[tokio::test]
async fn test_generate_output_no_template_artifacts() -> anyhow::Result<()> {
    // Arrange
    let client = start_server().await?;
    let tempdir = tempfile::tempdir()?;
    let queries = [
        ("01_services.rq", SERVICE_SELECT_SPARQL),
        ("02_entities.rq", ENTITY_SELECT_SPARQL),
    ];
    let (ontology_path, queries_dir, output_dir) =
        setup_generate_project(tempdir.path(), BOOK_AUTHOR_TTL, &queries);

    // Act
    let result = client
        .call_tool(
            CallToolRequestParams::new("generate").with_arguments(args(serde_json::json!({
                "ontology_path": ontology_path.to_str().unwrap(),
                "queries_dir": queries_dir.to_str().unwrap(),
                "output_dir": output_dir.to_str().unwrap(),
                "language": "rust"
            }))),
        )
        .await?;

    // Assert — generate succeeds
    assert_ne!(
        result.is_error,
        Some(true),
        "generate should succeed: {:?}",
        extract_text(&result)
    );

    // Collect all generated file contents
    let all_contents: Vec<String> = std::fs::read_dir(&output_dir)
        .expect("output dir must exist")
        .flatten()
        .filter_map(|e| {
            let path = e.path();
            if path.is_file() {
                std::fs::read_to_string(&path).ok()
            } else {
                None
            }
        })
        .collect();

    assert!(!all_contents.is_empty(), "must generate at least one file");

    let combined = all_contents.join("\n");

    // Assert — NO Tera template artifacts
    assert!(
        !combined.contains("{{"),
        "generated code must NOT contain '{{' (Tera template artifact)"
    );
    assert!(
        !combined.contains("{%"),
        "generated code must NOT contain '{{%' (Tera template artifact)"
    );

    // Assert — expected Rust patterns ARE present
    assert!(
        combined.contains("//!"),
        "generated Rust code should contain module doc comment '//!'"
    );
    assert!(
        combined.contains("#[derive("),
        "generated Rust code should contain '#[derive('"
    );
    assert!(
        combined.contains("pub struct"),
        "generated Rust code should contain 'pub struct'"
    );

    client.cancel().await?;
    Ok(())
}

// ---------------------------------------------------------------------------
// File-system helpers
// ---------------------------------------------------------------------------

/// Walk a directory recursively, collecting files matching the given extension.
fn walk_dir_for_ext(root: &Path, ext: &str) -> Vec<PathBuf> {
    walk_dir_all(root)
        .into_iter()
        .filter(|p| p.extension().and_then(|e| e.to_str()) == Some(ext))
        .collect()
}

/// Walk a directory recursively, collecting all file paths.
fn walk_dir_all(root: &Path) -> Vec<PathBuf> {
    let mut files = Vec::new();
    if let Ok(entries) = std::fs::read_dir(root) {
        for entry in entries.flatten() {
            let path = entry.path();
            if path.is_dir() {
                files.extend(walk_dir_all(&path));
            } else {
                files.push(path);
            }
        }
    }
    files
}

/// Find the first .ttl file content under the directory.
fn find_any_ttl_content(dir: &Path) -> String {
    for subdir in &[dir.to_path_buf(), dir.join("ontology")] {
        if let Ok(entries) = std::fs::read_dir(subdir) {
            for entry in entries.flatten() {
                let path = entry.path();
                if path.extension().and_then(|e| e.to_str()) == Some("ttl") {
                    if let Ok(content) = std::fs::read_to_string(&path) {
                        return content;
                    }
                }
            }
        }
    }
    String::new()
}

/// Extract the triple count from a validate response like "Valid Turtle content (5 triple(s) parsed)".
fn extract_triple_count(text: &str) -> u64 {
    // Pattern: "N triple(s) parsed"
    text.split("triple")
        .next()
        .and_then(|prefix| {
            prefix
                .trim()
                .chars()
                .rev()
                .take_while(|c| c.is_ascii_digit())
                .collect::<String>()
                .chars()
                .rev()
                .collect::<String>()
                .parse::<u64>()
                .ok()
        })
        .unwrap_or(0)
}
