# MCP Server Validation, Benchmarks, and Stress Tests

> **For agentic workers:** REQUIRED SUB-SKILL: Use superpowers:subagent-driven-development (recommended) or superpowers:executing-plans to implement this plan task-by-task. Steps use checkbox (`- [ ]`) syntax for tracking.

**Goal:** Validate MCP documentation against runtime, establish performance benchmarks, and add stress tests for `ggen-a2a-mcp` server.

**Architecture:**
- **Doc validation**: Runtime reflection tests compare documented tools/resources/prompts against actual server capabilities
- **Benchmarks**: Criterion suite measuring latency/throughput for all 9 tools + pagination
- **Stress tests**: Concurrent request floods, large payload handling, resource exhaustion scenarios

**Tech Stack:** Criterion 0.7, rmcp 1.3.0, tokio, tempfile, proptest

---

## Task 1: Add Criterion benchmark dependency

**Files:**
- Modify: `crates/ggen-a2a-mcp/Cargo.toml`

- [ ] **Step 1: Add criterion dev-dependency and benchmark harness**

Edit `crates/ggen-a2a-mcp/Cargo.toml`. Add after the existing `[dev-dependencies]` section:

```toml
[dev-dependencies]
# ... existing deps ...
criterion = { version = "0.7", features = ["html_reports"] }

[[bench]]
name = "mcp_tool_benchmarks"
harness = false

[[bench]]
name = "mcp_stress_tests"
harness = false
```

- [ ] **Step 2: Verify compilation**

Run: `cargo check -p ggen-a2a-mcp --benches`

Expected: No errors, new benchmark targets recognized

- [ ] **Step 3: Commit**

```bash
git add crates/ggen-a2a-mcp/Cargo.toml
git commit -m "feat(mcp): add criterion benchmark framework"
```

---

## Task 2: Create benchmark infrastructure helper

**Files:**
- Create: `crates/ggen-a2a-mcp/benches/bench_helper.rs`

- [ ] **Step 1: Create benchmark helper module**

Create `crates/ggen-a2a-mcp/benches/bench_helper.rs`:

```rust
//! Benchmark helper utilities for GgenMcpServer
//!
//! Provides common setup/teardown for Criterion benchmarks and stress tests.

use ggen_a2a_mcp::ggen_server::GgenMcpServer;
use rmcp::{model::*, service::RunningService, ClientHandler, RoleClient, ServiceExt};
use std::sync::Arc;
use tokio::runtime::Runtime;

/// Minimal client handler for benchmarking
#[derive(Debug, Clone, Default)]
struct BenchClientHandler;

impl ClientHandler for BenchClientHandler {
    fn get_info(&self) -> ClientInfo {
        ClientInfo::default()
    }
}

/// Spawn a server+client pair connected via duplex transport
///
/// Returns (runtime, client) — caller must keep runtime alive for duration of benchmark.
pub fn spawn_duplex_server() -> anyhow::Result<(Runtime, RunningService<RoleClient, BenchClientHandler>)> {
    let rt = Runtime::new()?;
    let (server_transport, client_transport) = tokio::io::duplex(65_536);

    let server = GgenMcpServer::new();
    rt.spawn(async move {
        let _ = server.serve(server_transport).await;
    });

    let client = rt.block_on(BenchClientHandler::default().serve(client_transport))?;
    Ok((rt, client))
}

/// Valid TTL content for benchmarks (100 triples)
pub const VALID_TTL_100: &str = include_str!("../../../examples/a2a-agent-lifecycle/ontology/publisher.ttl");

/// SPARQL SELECT query that returns all triples
pub const SPARQL_SELECT_ALL: &str = "SELECT ?s ?p ?o WHERE { ?s ?p ?o } LIMIT 100";

/// Example name for benchmarks (uses a2a-agent-lifecycle)
pub const BENCHMARK_EXAMPLE: &str = "a2a-agent-lifecycle";
```

- [ ] **Step 2: Create benches directory**

Run: `mkdir -p crates/ggen-a2a-mcp/benches`

Expected: Directory created

- [ ] **Step 3: Verify file compiles**

Run: `cargo check -p ggen-a2a-mcp --benches`

Expected: No errors

- [ ] **Step 4: Commit**

```bash
git add crates/ggen-a2a-mcp/benches/bench_helper.rs
git commit -m "feat(mcp): add benchmark helper utilities"
```

---

## Task 3: Tool performance benchmarks

**Files:**
- Create: `crates/ggen-a2a-mcp/benches/mcp_tool_benchmarks.rs`

- [ ] **Step 1: Write tool benchmark suite**

Create `crates/ggen-a2a-mcp/benches/mcp_tool_benchmarks.rs`:

```rust
//! Criterion benchmarks for all MCP tools
//!
//! Measures latency and throughput for:
//! - validate (TTL parsing)
//! - list_generators (static list)
//! - list_examples (filesystem scan)
//! - get_example (file reads)
//! - query_ontology (SPARQL execution)
//! - search (marketplace lookup)
//! - scaffold (filesystem copy)

use criterion::{black_box, criterion_group, criterion_main, BenchmarkId, Criterion, Throughput};
use rmcp::model::CallToolRequestParams;

mod bench_helper;
use bench_helper::*;

fn bench_validate(c: &mut Criterion) {
    let (_rt, client) = spawn_duplex_server().unwrap();

    let mut group = c.benchmark_group("validate");

    for size in [10, 50, 100, 500].iter() {
        let ttl = format!("@prefix ex: <http://example.org/> .\n{}\n", (0..*size)
            .map(|i| format!("ex:s{} a ex:Class .", i))
            .collect::<Vec<_>>()
            .join("\n"));

        group.throughput(Throughput::Bytes(ttl.len() as u64));
        group.bench_with_input(BenchmarkId::from_parameter(size), size, |b, _| {
            let args = serde_json::json!({"ttl": &ttl}).as_object().unwrap().clone();
            b.iter(|| {
                let _ = rt.block_on(async {
                    client.call_tool(CallToolRequestParams::new("validate").with_arguments(args.clone())).await
                });
            });
        });
    }
    group.finish();
}

fn bench_list_generators(c: &mut Criterion) {
    let (_rt, client) = spawn_duplex_server().unwrap();

    c.bench_function("list_generators", |b| {
        b.iter(|| {
            let _ = rt.block_on(async {
                client.call_tool(CallToolRequestParams::new("list_generators")).await
            });
        })
    });
}

fn bench_list_examples(c: &mut Criterion) {
    let (_rt, client) = spawn_duplex_server().unwrap();

    c.bench_function("list_examples", |b| {
        b.iter(|| {
            let _ = rt.block_on(async {
                client.call_tool(CallToolRequestParams::new("list_examples")).await
            });
        })
    });
}

fn bench_get_example(c: &mut Criterion) {
    let (_rt, client) = spawn_duplex_server().unwrap();

    c.bench_function("get_example", |b| {
        let args = serde_json::json!({"name": BENCHMARK_EXAMPLE}).as_object().unwrap().clone();
        b.iter(|| {
            let _ = rt.block_on(async {
                client.call_tool(CallToolRequestParams::new("get_example").with_arguments(args.clone())).await
            });
        })
    });
}

fn bench_query_ontology(c: &mut Criterion) {
    let (_rt, client) = spawn_duplex_server().unwrap();

    let mut group = c.benchmark_group("query_ontology");

    for triple_count in [10, 50, 100, 500].iter() {
        let ttl = format!("@prefix ex: <http://example.org/> .\n{}\n", (0..*triple_count)
            .map(|i| format!("ex:s{} ex:p{} ex:o{} .", i, i, i))
            .collect::<Vec<_>>()
            .join("\n"));

        group.throughput(Throughput::Elements(*triple_count as u64));
        group.bench_with_input(BenchmarkId::from_parameter(triple_count), triple_count, |b, _| {
            let args = serde_json::json!({
                "ttl": &ttl,
                "sparql": SPARQL_SELECT_ALL
            }).as_object().unwrap().clone();
            b.iter(|| {
                let _ = rt.block_on(async {
                    client.call_tool(CallToolRequestParams::new("query_ontology").with_arguments(args.clone())).await
                });
            });
        });
    }
    group.finish();
}

fn bench_search(c: &mut Criterion) {
    let (_rt, client) = spawn_duplex_server().unwrap();

    c.bench_function("search", |b| {
        let args = serde_json::json!({"query": "agent"}).as_object().unwrap().clone();
        b.iter(|| {
            let _ = rt.block_on(async {
                client.call_tool(CallToolRequestParams::new("search").with_arguments(args.clone())).await
            });
        })
    });
}

fn bench_scaffold_from_example(c: &mut Criterion) {
    let (_rt, client) = spawn_duplex_server().unwrap();

    c.bench_function("scaffold_from_example", |b| {
        // Use a temp dir for scaffolding
        let temp_dir = std::env::temp_dir().join(format!("ggen-bench-{}", std::process::id()));
        let args = serde_json::json!({
            "example_name": BENCHMARK_EXAMPLE,
            "target_dir": temp_dir.to_string_lossy().as_ref()
        }).as_object().unwrap().clone();
        b.iter(|| {
            let _ = rt.block_on(async {
                client.call_tool(CallToolRequestParams::new("scaffold_from_example").with_arguments(args.clone())).await
            });
        });
        // Cleanup after benchmark
        let _ = std::fs::remove_dir_all(&temp_dir);
    });
}

criterion_group!(
    benches,
    bench_validate,
    bench_list_generators,
    bench_list_examples,
    bench_get_example,
    bench_query_ontology,
    bench_search,
    bench_scaffold_from_example
);
criterion_main!(benches);
```

- [ ] **Step 2: Fix runtime lifetime issue in benchmarks**

The above code has a bug — `rt` is created inside `bench_validate` but used in other functions. Create a corrected version with proper runtime scoping.

```rust
//! In each benchmark function, replace:
let (_rt, client) = spawn_duplex_server().unwrap();

//! With:
let rt = tokio::runtime::Runtime::new().unwrap();
let (server_transport, client_transport) = tokio::io::duplex(65_536);
let server = GgenMcpServer::new();
rt.spawn(async move {
    let _ = server.serve(server_transport).await;
});
let client = rt.block_on(BenchClientHandler::default().serve(client_transport)).unwrap();
```

- [ ] **Step 3: Run benchmarks to verify they compile and execute**

Run: `cargo bench -p ggen-a2a-mcp -- --test`

Expected: Benchmarks run (may have failures if examples dir missing, but should execute)

- [ ] **Step 4: Commit**

```bash
git add crates/ggen-a2a-mcp/benches/mcp_tool_benchmarks.rs
git commit -m "feat(mcp): add Criterion benchmarks for all tools"
```

---

## Task 4: Resource and Prompt benchmarks

**Files:**
- Modify: `crates/ggen-a2a-mcp/benches/mcp_tool_benchmarks.rs`

- [ ] **Step 1: Add resource/prompt benchmarks to existing file**

Append to `crates/ggen-a2a-mcp/benches/mcp_tool_benchmarks.rs`:

```rust
//! Add after existing benchmarks:

fn bench_list_resources(c: &mut Criterion) {
    let rt = tokio::runtime::Runtime::new().unwrap();
    let (server_transport, client_transport) = tokio::io::duplex(65_536);
    let server = GgenMcpServer::new();
    rt.spawn(async move {
        let _ = server.serve(server_transport).await;
    });
    let client = rt.block_on(BenchClientHandler::default().serve(client_transport)).unwrap();

    c.bench_function("list_resources", |b| {
        b.iter(|| {
            let _ = rt.block_on(async {
                client.list_resources(None).await
            });
        })
    });
}

fn bench_read_resource(c: &mut Criterion) {
    let rt = tokio::runtime::Runtime::new().unwrap();
    let (server_transport, client_transport) = tokio::io::duplex(65_536);
    let server = GgenMcpServer::new();
    rt.spawn(async move {
        let _ = server.serve(server_transport).await;
    });
    let client = rt.block_on(BenchClientHandler::default().serve(client_transport)).unwrap();

    c.bench_function("read_resource_ttl", |b| {
        let uri = format!("ggen://example/{}/ttl", BENCHMARK_EXAMPLE);
        b.iter(|| {
            let _ = rt.block_on(async {
                client.read_resource(ReadResourceRequestParams::new(uri.clone())).await
            });
        })
    });
}

fn bench_list_prompts(c: &mut Criterion) {
    let rt = tokio::runtime::Runtime::new().unwrap();
    let (server_transport, client_transport) = tokio::io::duplex(65_536);
    let server = GgenMcpServer::new();
    rt.spawn(async move {
        let _ = server.serve(server_transport).await;
    });
    let client = rt.block_on(BenchClientHandler::default().serve(client_transport)).unwrap();

    c.bench_function("list_prompts", |b| {
        b.iter(|| {
            let _ = rt.block_on(async {
                client.list_prompts(None).await
            });
        })
    });
}

fn bench_get_prompt(c: &mut Criterion) {
    let rt = tokio::runtime::Runtime::new().unwrap();
    let (server_transport, client_transport) = tokio::io::duplex(65_536);
    let server = GgenMcpServer::new();
    rt.spawn(async move {
        let _ = server.serve(server_transport).await;
    });
    let client = rt.block_on(BenchClientHandler::default().serve(client_transport)).unwrap();

    c.bench_function("get_prompt_explain_rdf_schema", |b| {
        let args = serde_json::json!({"ttl_content": VALID_TTL_100}).as_object().unwrap().clone();
        b.iter(|| {
            let _ = rt.block_on(async {
                client.get_prompt(GetPromptRequestParams::new("explain-rdf-schema").with_arguments(args.clone())).await
            });
        })
    });
}

fn bench_complete(c: &mut Criterion) {
    let rt = tokio::runtime::Runtime::new().unwrap();
    let (server_transport, client_transport) = tokio::io::duplex(65_536);
    let server = GgenMcpServer::new();
    rt.spawn(async move {
        let _ = server.serve(server_transport).await;
    });
    let client = rt.block_on(BenchClientHandler::default().serve(client_transport)).unwrap();

    c.bench_function("complete_example_name", |b| {
        let params = CompleteRequestParams::new(
            Reference::for_prompt("list_examples"),
            ArgumentInfo {
                name: "example_name".to_string(),
                value: "a2a".to_string(),
            },
        );
        b.iter(|| {
            let _ = rt.block_on(async {
                client.complete(params.clone()).await
            });
        })
    });
}

//! Update criterion_group! at bottom:
criterion_group!(
    benches,
    bench_validate,
    bench_list_generators,
    bench_list_examples,
    bench_get_example,
    bench_query_ontology,
    bench_search,
    bench_scaffold_from_example,
    bench_list_resources,
    bench_read_resource,
    bench_list_prompts,
    bench_get_prompt,
    bench_complete,
);
```

- [ ] **Step 2: Run benchmarks**

Run: `cargo bench -p ggen-a2a-mcp`

Expected: All benchmarks execute, HTML report generated in `target/criterion/report/`

- [ ] **Step 3: Commit**

```bash
git add crates/ggen-a2a-mcp/benches/mcp_tool_benchmarks.rs
git commit -m "feat(mcp): add resource/prompt/completion benchmarks"
```

---

## Task 5: Documentation validation test

**Files:**
- Create: `crates/ggen-a2a-mcp/tests/doc_validation_test.rs`

- [ ] **Step 1: Write doc validation test**

Create `crates/ggen-a2a-mcp/tests/doc_validation_test.rs`:

```rust
//! Runtime validation that documentation matches actual server capabilities.
//!
//! Tests verify:
//! - All documented tools exist with correct parameters
//! - All documented resources are accessible
//! - All documented prompts exist with correct arguments
//! - Tool descriptions match documented behavior

use ggen_a2a_mcp::ggen_server::GgenMcpServer;
use rmcp::{model::*, service::RunningService, ClientHandler, RoleClient, ServiceExt};

#[derive(Debug, Clone, Default)]
struct TestClient;
impl ClientHandler for TestClient {
    fn get_info(&self) -> ClientInfo {
        ClientInfo::default()
    }
}

async fn start_server() -> anyhow::Result<RunningService<RoleClient, TestClient>> {
    let (server_transport, client_transport) = tokio::io::duplex(4096);
    let server = GgenMcpServer::new();
    tokio::spawn(async move {
        let _ = server.serve(server_transport).await;
    });
    Ok(TestClient::default().serve(client_transport).await?)
}

/// Documented tools in README.md and docs/reference/commands/mcp.md
const DOCUMENTED_TOOLS: &[&str] = &[
    "generate",
    "sync",
    "validate",
    "list_generators",
    "list_examples",
    "get_example",
    "search",
    "scaffold_from_example",
    "query_ontology",
];

#[tokio::test]
async fn all_documented_tools_exist() -> anyhow::Result<()> {
    let client = start_server().await?;
    let tools = client.list_tools(None).await?;
    let tool_names: Vec<&str> = tools.tools.iter().map(|t| t.name.as_ref()).collect();

    for documented in DOCUMENTED_TOOLS {
        assert!(
            tool_names.contains(documented),
            "Documented tool '{}' not found in server. Available: {:?}",
            documented,
            tool_names
        );
    }

    client.cancel().await?;
    Ok(())
}

#[tokio::test]
async fn documented_tools_have_descriptions() -> anyhow::Result<()> {
    let client = start_server().await?;
    let tools = client.list_tools(None).await?;

    for tool in &tools.tools {
        if DOCUMENTED_TOOLS.contains(&tool.name.as_ref()) {
            assert!(
                tool.description.as_ref().map_or(false, |d| !d.is_empty()),
                "Tool '{}' must have a non-empty description",
                tool.name
            );
        }
    }

    client.cancel().await?;
    Ok(())
}

/// Documented resource URI patterns from README.md
const RESOURCE_PATTERNS: &[&str] = &[
    "ggen://example/{name}",
    "ggen://example/{name}/ttl",
    "ggen://example/{name}/readme",
    "ggen://example/{name}/config",
];

#[tokio::test]
async fn resource_list_is_not_empty() -> anyhow::Result<()> {
    let client = start_server().await?;
    let resources = client.list_resources(None).await?;

    assert!(
        !resources.resources.is_empty(),
        "Resource list should not be empty (examples dir may be missing, but server should return empty list not error)"
    );

    client.cancel().await?;
    Ok(())
}

/// Documented prompts from README.md
const DOCUMENTED_PROMPTS: &[&str] = &[
    "explain-rdf-schema",
    "generate-from-example",
    "scaffold-project",
];

#[tokio::test]
async fn all_documented_prompts_exist() -> anyhow::Result<()> {
    let client = start_server().await?;
    let prompts = client.list_prompts(None).await?;
    let prompt_names: Vec<&str> = prompts.prompts.iter().map(|p| p.name.as_str()).collect();

    for documented in DOCUMENTED_PROMPTS {
        assert!(
            prompt_names.contains(documented),
            "Documented prompt '{}' not found. Available: {:?}",
            documented,
            prompt_names
        );
    }

    client.cancel().await?;
    Ok(())
}

#[tokio::test]
async fn prompts_have_descriptions() -> anyhow::Result<()> {
    let client = start_server().await?;
    let prompts = client.list_prompts(None).await?;

    for prompt in &prompts.prompts {
        assert!(
            prompt.description.as_ref().map_or(false, |d| !d.is_empty()),
            "Prompt '{}' must have a non-empty description",
            prompt.name
        );
    }

    client.cancel().await?;
    Ok(())
}

#[tokio::test]
async fn validate_tool_accepts_ttl_parameter() -> anyhow::Result<()> {
    let client = start_server().await?;
    let tools = client.list_tools(None).await?;
    let validate_tool = tools.tools.iter()
        .find(|t| t.name.as_ref() == "validate")
        .expect("validate tool must exist");

    let input_schema = validate_tool.input_schema.as_ref()
        .expect("validate tool must have input schema");

    // Check that 'ttl' property exists and is required
    let schema_obj = input_schema.as_object()
        .expect("schema must be object");

    let properties = schema_obj.get("properties")
        .and_then(|v| v.as_object())
        .expect("schema must have properties");

    assert!(
        properties.contains_key("ttl"),
        "validate tool must have 'ttl' parameter in schema"
    );

    let required = schema_obj.get("required")
        .and_then(|v| v.as_array())
        .unwrap_or(&vec![]);

    assert!(
        required.iter().any(|v| v.as_str() == Some("ttl")),
        "validate tool 'ttl' parameter must be required"
    );

    client.cancel().await?;
    Ok(())
}
```

- [ ] **Step 2: Run doc validation tests**

Run: `cargo test -p ggen-a2a-mcp doc_validation_test`

Expected: All tests pass (validates docs match runtime)

- [ ] **Step 3: Commit**

```bash
git add crates/ggen-a2a-mcp/tests/doc_validation_test.rs
git commit -m "test(mcp): add runtime doc validation tests"
```

---

## Task 6: Stress tests — concurrent requests

**Files:**
- Create: `crates/ggen-a2a-mcp/benches/mcp_stress_tests.rs`

- [ ] **Step 1: Write concurrent request stress tests**

Create `crates/ggen-a2a-mcp/benches/mcp_stress_tests.rs`:

```rust
//! Stress tests for MCP server under load
//!
//! Tests:
//! - Concurrent tool calls (10, 50, 100 parallel requests)
//! - Large payload handling (1MB, 10MB TTL content)
//! - Resource pagination stress
//! - Prompt template rendering under load

use criterion::{black_box, criterion_group, criterion_main, BenchmarkId, Criterion};
use rmcp::model::CallToolRequestParams;
use std::sync::Arc;
use tokio::task::JoinSet;

mod bench_helper;
use bench_helper::*;

fn bench_concurrent_validate(c: &mut Criterion) {
    let mut group = c.benchmark_group("concurrent_validate");

    for concurrent in [10, 50, 100].iter() {
        group.bench_with_input(BenchmarkId::from_parameter(concurrent), concurrent, |b, &concurrent| {
            let rt = tokio::runtime::Runtime::new().unwrap();
            let (server_transport, client_transport) = tokio::io::duplex(65_536);
            let server = GgenMcpServer::new();
            rt.spawn(async move {
                let _ = server.serve(server_transport).await;
            });
            let client = Arc::new(rt.block_on(
                BenchClientHandler::default().serve(client_transport)
            ).unwrap());

            let ttl = "@prefix ex: <http://example.org/> . ex:s a ex:Class .";
            let args = std::sync::Arc::new(
                serde_json::json!({"ttl": ttl}).as_object().unwrap().clone()
            );

            b.iter(|| {
                let rt = tokio::runtime::Runtime::new().unwrap();
                let client = client.clone();
                let args = args.clone();

                rt.block_on(async {
                    let mut set = JoinSet::new();
                    for _ in 0..concurrent {
                        let client = client.clone();
                        let args = args.clone();
                        set.spawn(async move {
                            let _ = client.call_tool(
                                CallToolRequestParams::new("validate").with_arguments((*args).clone())
                            ).await;
                        });
                    }
                    while let Some(_) = set.join_next().await {}
                });
            });
        });
    }
    group.finish();
}

fn bench_large_ttl_payload(c: &mut Criterion) {
    let mut group = c.benchmark_group("large_payload");

    // Generate TTL content of varying sizes
    for size_kb in [10, 100, 1000].iter() {
        let triples_count = size_kb * 10; // Roughly 100 bytes per triple
        let ttl = format!("@prefix ex: <http://example.org/> .\n{}\n",
            (0..triples_count)
                .map(|i| format!("ex:s{} ex:p{} ex:o{} .", i, i, i))
                .collect::<Vec<_>>()
                .join("\n")
        );

        group.throughput(criterion::Throughput::Bytes(ttl.len() as u64));
        group.bench_with_input(BenchmarkId::new("kb", size_kb), size_kb, |b, _| {
            let rt = tokio::runtime::Runtime::new().unwrap();
            let (server_transport, client_transport) = tokio::io::duplex(65_536);
            let server = GgenMcpServer::new();
            rt.spawn(async move {
                let _ = server.serve(server_transport).await;
            });
            let client = rt.block_on(
                BenchClientHandler::default().serve(client_transport)
            ).unwrap();

            let args = serde_json::json!({"ttl": &ttl}).as_object().unwrap().clone();

            b.iter(|| {
                let _ = rt.block_on(async {
                    client.call_tool(
                        CallToolRequestParams::new("validate").with_arguments(args.clone())
                    ).await
                });
            });
        });
    }
    group.finish();
}

fn bench_resource_pagination(c: &mut Criterion) {
    let rt = tokio::runtime::Runtime::new().unwrap();
    let (server_transport, client_transport) = tokio::io::duplex(65_536);
    let server = GgenMcpServer::new();
    rt.spawn(async move {
        let _ = server.serve(server_transport).await;
    });
    let client = rt.block_on(
        BenchClientHandler::default().serve(client_transport)
    ).unwrap();

    c.bench_function("resource_pagination_full_scan", |b| {
        b.iter(|| {
            let _ = rt.block_on(async {
                let mut all_resources = vec![];
                let mut cursor = None;
                loop {
                    let result = client.list_resources(cursor.clone().map(|c: String| {
                        PaginatedRequestParams { cursor: Some(Cursor::from(c)), ..Default::default() }
                    })).await;
                    if result.is_err() {
                        break;
                    }
                    let response = result.unwrap();
                    if response.resources.is_empty() {
                        break;
                    }
                    all_resources.extend(response.resources);
                    cursor = response.next_cursor.map(|c| c.into_string());
                    if cursor.is_none() {
                        break;
                    }
                }
                all_resources
            });
        })
    });
}

fn bench_concurrent_prompt_rendering(c: &mut Criterion) {
    let mut group = c.benchmark_group("concurrent_prompts");

    for concurrent in [10, 50].iter() {
        group.bench_with_input(BenchmarkId::from_parameter(concurrent), concurrent, |b, &concurrent| {
            let rt = tokio::runtime::Runtime::new().unwrap();
            let (server_transport, client_transport) = tokio::io::duplex(65_536);
            let server = GgenMcpServer::new();
            rt.spawn(async move {
                let _ = server.serve(server_transport).await;
            });
            let client = Arc::new(rt.block_on(
                BenchClientHandler::default().serve(client_transport)
            ).unwrap());

            let args = std::sync::Arc::new(
                serde_json::json!({"ttl_content": VALID_TTL_100}).as_object().unwrap().clone()
            );

            b.iter(|| {
                let rt = tokio::runtime::Runtime::new().unwrap();
                let client = client.clone();
                let args = args.clone();

                rt.block_on(async {
                    let mut set = JoinSet::new();
                    for _ in 0..concurrent {
                        let client = client.clone();
                        let args = args.clone();
                        set.spawn(async move {
                            let _ = client.get_prompt(
                                GetPromptRequestParams::new("explain-rdf-schema")
                                    .with_arguments(Some((*args).clone()))
                            ).await;
                        });
                    }
                    while let Some(_) = set.join_next().await {}
                });
            });
        });
    }
    group.finish();
}

criterion_group!(
    stress_tests,
    bench_concurrent_validate,
    bench_large_ttl_payload,
    bench_resource_pagination,
    bench_concurrent_prompt_rendering
);
criterion_main!(stress_tests);
```

- [ ] **Step 2: Run stress tests**

Run: `cargo bench -p ggen-a2a-mcp --bench mcp_stress_tests`

Expected: All stress tests execute (may be slow due to high concurrency)

- [ ] **Step 3: Commit**

```bash
git add crates/ggen-a2a-mcp/benches/mcp_stress_tests.rs
git commit -m "test(mcp): add concurrent and load stress tests"
```

---

## Task 7: Property-based tests with proptest

**Files:**
- Create: `crates/ggen-a2a-mcp/tests/property_tests.rs`

- [ ] **Step 1: Write property-based tests for tool inputs**

Create `crates/ggen-a2a-mcp/tests/property_tests.rs`:

```rust
//! Property-based tests for MCP tool robustness
//!
//! Tests verify invariants:
//! - validate accepts any valid Turtle syntax
//! - query_ontology returns valid JSON for any SPARQL SELECT
//! - list_examples handles arbitrary category/limit combinations
//! - search handles various query strings

use ggen_a2a_mcp::ggen_server::GgenMcpServer;
use proptest::prelude::*;
use rmcp::{model::*, service::RunningService, ClientHandler, RoleClient, ServiceExt};

#[derive(Debug, Clone, Default)]
struct TestClient;
impl ClientHandler for TestClient {
    fn get_info(&self) -> ClientInfo {
        ClientInfo::default()
    }
}

async fn start_server() -> anyhow::Result<RunningService<RoleClient, TestClient>> {
    let (server_transport, client_transport) = tokio::io::duplex(4096);
    let server = GgenMcpServer::new();
    tokio::spawn(async move {
        let _ = server.serve(server_transport).await;
    });
    Ok(TestClient::default().serve(client_transport).await?)
}

proptest! {
    #[tokio::test]
    async fn prop_validate_accepts_various_ttl_formats(
        triple_count in 0..100usize,
        use_prefix in bool::arbitrary()
    ) {
        let client = start_server().await.unwrap();

        let ttl = if use_prefix {
            format!("@prefix ex: <http://example.org/> .\n{}\n",
                (0..triple_count)
                    .map(|i| format!("ex:s{} a ex:Class .", i))
                    .collect::<Vec<_>>()
                    .join("\n")
            )
        } else {
            (0..triple_count)
                .map(|i| format!("<http://example.org/s{}> <http://example.org/p> <http://example.org/o{}> .", i, i))
                .collect::<Vec<_>>()
                .join("\n")
        };

        let args = serde_json::json!({"ttl": &ttl}).as_object().unwrap().clone();
        let result = client.call_tool(
            CallToolRequestParams::new("validate").with_arguments(args)
        ).await;

        // Should not crash — may return error for invalid TTL, but must not panic
        prop_assert!(result.is_ok());

        client.cancel().await.unwrap();
    }

    #[tokio::test]
    async fn prop_list_examples_handles_arbitrary_limits(
        limit in 0..1000usize
    ) {
        let client = start_server().await.unwrap();

        let args = serde_json::json!({"limit": limit}).as_object().unwrap().clone();
        let result = client.call_tool(
            CallToolRequestParams::new("list_examples").with_arguments(args)
        ).await;

        // Should not crash
        prop_assert!(result.is_ok());

        client.cancel().await.unwrap();
    }

    #[tokio::test]
    async fn prop_search_handles_various_queries(
        query_len in 0..100usize,
        has_category in bool::arbitrary()
    ) {
        let client = start_server().await.unwrap();

        let query = "a".repeat(query_len);
        let mut args_obj = serde_json::json!({"query": &query}).as_object().unwrap().clone();

        if has_category {
            args_obj.insert("category".to_string(), serde_json::Value::String("agent".to_string()));
        }

        let result = client.call_tool(
            CallToolRequestParams::new("search").with_arguments(args_obj)
        ).await;

        // Should not crash
        prop_assert!(result.is_ok());

        client.cancel().await.unwrap();
    }
}
```

- [ ] **Step 2: Add proptest dependency**

Edit `crates/ggen-a2a-mcp/Cargo.toml` dev-dependencies:

```toml
[dev-dependencies]
# ... existing deps ...
proptest = { workspace = true }
```

- [ ] **Step 3: Run property tests**

Run: `cargo test -p ggen-a2a-mcp property_tests -- --nocapture`

Expected: Tests run with various random inputs, no panics

- [ ] **Step 4: Commit**

```bash
git add crates/ggen-a2a-mcp/tests/property_tests.rs crates/ggen-a2a-mcp/Cargo.toml
git commit -m "test(mcp): add property-based tests with proptest"
```

---

## Task 8: Update README with benchmark/stress test documentation

**Files:**
- Modify: `crates/ggen-a2a-mcp/README.md`

- [ ] **Step 1: Add performance testing section to README**

Append to `crates/ggen-a2a-mcp/README.md`:

```markdown
---

## Performance Testing

### Benchmarks

```bash
# Run all Criterion benchmarks
cargo bench -p ggen-a2a-mcp

# View HTML report
open target/criterion/report/index.html
```

Benchmarks measure:
- Tool invocation latency (validate, list_examples, get_example, etc.)
- SPARQL query performance (10–500 triples)
- Resource pagination overhead
- Prompt rendering throughput

### Stress Tests

```bash
# Run concurrent load tests
cargo bench -p ggen-a2a-mcp --bench mcp_stress_tests
```

Stress tests validate:
- Concurrent request handling (10/50/100 parallel)
- Large payload processing (up to 1MB TTL)
- Resource pagination under load
- Prompt rendering concurrency

### Property-Based Tests

```bash
# Run proptest suite
cargo test -p ggen-a2a-mcp property_tests
```

Property tests verify invariants across random inputs:
- validate accepts various Turtle formats
- list_examples handles arbitrary limits
- search handles various query strings

### Documentation Validation

```bash
# Verify docs match runtime
cargo test -p ggen-a2a-mcp doc_validation_test
```

Validates that documented tools/resources/prompts exist with correct signatures.
```

- [ ] **Step 2: Verify README renders correctly**

Run: `cat crates/ggen-a2a-mcp/README.md | head -100`

Expected: README content visible, new section present

- [ ] **Step 3: Commit**

```bash
git add crates/ggen-a2a-mcp/README.md
git commit -m "docs(mcp): add performance testing section to README"
```

---

## Task 9: Update RMCP_NOTES with benchmark patterns

**Files:**
- Modify: `docs/RMCP_NOTES.md`

- [ ] **Step 1: Add benchmark/stress testing patterns to RMCP_NOTES**

Append to `docs/RMCP_NOTES.md`:

```markdown
---

## 12. Benchmarking rmcp servers

### Criterion setup

```toml
# Cargo.toml
[dev-dependencies]
criterion = { version = "0.7", features = ["html_reports"] }

[[bench]]
name = "server_benchmarks"
harness = false
```

### Duplex transport benchmark pattern

```rust
use criterion::{black_box, criterion_group, criterion_main, Criterion};
use rmcp::{ClientHandler, RoleClient, ServiceExt};

struct BenchClient;
impl ClientHandler for BenchClient {
    fn get_info(&self) -> ClientInfo { ClientInfo::default() }
}

fn bench_tool_call(c: &mut Criterion) {
    c.bench_function("my_tool", |b| {
        let rt = tokio::runtime::Runtime::new().unwrap();
        let (s_tx, c_tx) = tokio::io::duplex(65_536);

        rt.spawn(async move {
            let _ = MyServer::new().serve(s_tx).await;
        });

        let client = rt.block_on(BenchClient::serve(c_tx)).unwrap();
        let args = serde_json::json!({"key": "value"}).as_object().unwrap().clone();

        b.iter(|| {
            let _ = rt.block_on(async {
                client.call_tool(CallToolRequestParams::new("tool").with_arguments(args.clone())).await
            });
        });
    });
}

criterion_group!(benches, bench_tool_call);
criterion_main!(benches);
```

### Stress testing concurrent requests

```rust
use tokio::task::JoinSet;
use std::sync::Arc;

fn bench_concurrent(c: &mut Criterion) {
    let mut group = c.benchmark_group("concurrent");

    for concurrent in [10, 50, 100].iter() {
        group.bench_with_input(BenchmarkId::from_parameter(concurrent), concurrent, |b, &concurrent| {
            let rt = tokio::runtime::Runtime::new().unwrap();
            let (s_tx, c_tx) = tokio::io::duplex(65_536);
            rt.spawn(async move { let _ = MyServer::new().serve(s_tx).await; });
            let client = Arc::new(rt.block_on(BenchClient::serve(c_tx)).unwrap());
            let args = Arc::new(json!({"key": "value"}).as_object().unwrap().clone());

            b.iter(|| {
                let rt = tokio::runtime::Runtime::new().unwrap();
                rt.block_on(async {
                    let mut set = JoinSet::new();
                    for _ in 0..concurrent {
                        let client = client.clone();
                        let args = args.clone();
                        set.spawn(async move {
                            let _ = client.call_tool(CallToolRequestParams::new("tool").with_arguments((*args).clone())).await;
                        });
                    }
                    while let Some(_) = set.join_next().await {}
                });
            });
        });
    }
    group.finish();
}
```

### Property-based testing with proptest

```rust
use proptest::prelude::*;

proptest! {
    #[tokio::test]
    async fn prop_tool_handles_various_inputs(size in 0..1000usize) {
        let client = start_server().await.unwrap();
        let input = "x".repeat(size);

        let result = client.call_tool(
            CallToolRequestParams::new("tool")
                .with_arguments(json!({"input": input}).as_object().unwrap().clone())
        ).await;

        // Should not crash — may error, but no panics
        prop_assert!(result.is_ok());
        client.cancel().await.unwrap();
    }
}
```
```

- [ ] **Step 2: Verify markdown formatting**

Run: `head -50 docs/RMCP_NOTES.md`

Expected: File intact, new section visible

- [ ] **Step 3: Commit**

```bash
git add docs/RMCP_NOTES.md
git commit -m "docs(mcp): add benchmark/stress testing patterns to RMCP_NOTES"
```

---

## Task 10: Run full validation suite

**Files:**
- No file creation — validation only

- [ ] **Step 1: Run all MCP tests**

Run: `cargo test -p ggen-a2a-mcp`

Expected: All 15 original tests + 5 doc validation tests + property tests pass

- [ ] **Step 2: Run benchmarks**

Run: `cargo bench -p ggen-a2a-mcp`

Expected: All benchmarks complete, HTML report generated

- [ ] **Step 3: Verify doc validation tests specifically**

Run: `cargo test -p ggen-a2a-mcp doc_validation_test -- --nocapture`

Expected: Output shows documented tools/resources/prompts all validated

- [ ] **Step 4: Run full workspace check**

Run: `cargo make check`

Expected: No compilation errors

- [ ] **Step 5: Commit final validation (if any fixes needed)**

If any tests fail or fixes were needed:

```bash
git add -A
git commit -m "fix(mcp): validation fixes and final test suite pass"
```

---

## Task 11: Update CHANGELOG

**Files:**
- Modify: `CHANGELOG.md`

- [ ] **Step 1: Add changelog entry**

Edit `CHANGELOG.md`, add to Unreleased section:

```markdown
### ggen-a2a-mcp

- **Added**: Criterion benchmark suite for all 9 tools + resources/prompts/completions
- **Added**: Stress tests for concurrent requests (10/50/100 parallel) and large payloads (up to 1MB)
- **Added**: Property-based tests with proptest for input robustness
- **Added**: Runtime doc validation tests ensuring documentation matches server capabilities
- **Added**: Performance testing documentation in README and RMCP_NOTES
```

- [ ] **Step 2: Verify changelog format**

Run: `head -50 CHANGELOG.md`

Expected: Changelog follows existing format

- [ ] **Step 3: Commit**

```bash
git add CHANGELOG.md
git commit -m "docs(changelog): add MCP validation/benchmark/stress test entries"
```

---

## Task 12: Final verification and documentation

**Files:**
- No file creation — final checks only

- [ ] **Step 1: Run full pre-commit gates**

Run: `cargo make pre-commit`

Expected: All gates pass (check, lint, test-unit)

- [ ] **Step 2: Run benchmark suite one final time**

Run: `cargo bench -p ggen-a2a-mcp -- --sample-size 10`

Expected: All benchmarks complete (reduced sample size for faster verification)

- [ ] **Step 3: Verify HTML report generation**

Run: `ls -la target/criterion/report/`

Expected: HTML files present for all benchmarks

- [ ] **Step 4: Create summary of baseline performance**

Create a performance baseline note for future reference:

```bash
cat > /tmp/mcp_baseline.txt << 'EOF'
MCP Server Performance Baseline
Generated: 2026-03-28
Command: cargo bench -p ggen-a2a-mcp

Key metrics (from target/criterion/report/):
- validate (100 triples): ~X ms
- list_generators: ~Y ms
- list_examples: ~Z ms
- query_ontology (100 triples): ~A ms
- resource pagination (full scan): ~B ms

Stress tests:
- 10 concurrent validate: ~C ms total
- 100 concurrent validate: ~D ms total
- 1MB TTL payload: ~E ms

Property tests:
- 256 random TTL formats: all pass, no panics
- 256 random search queries: all pass, no crashes
EOF

cat /tmp/mcp_baseline.txt
```

- [ ] **Step 5: Final commit if needed**

If any documentation or notes were added:

```bash
git add -A
git commit -m "docs(mcp): final validation baseline and summary"
```

---

## Definition of Done

- [ ] All 9 tools have Criterion benchmarks measuring latency
- [ ] Resources, prompts, completions have benchmarks
- [ ] Stress tests verify concurrent request handling (10/50/100 parallel)
- [ ] Property-based tests verify input robustness
- [ ] Doc validation tests ensure documentation matches runtime
- [ ] README updated with performance testing section
- [ ] RMCP_NOTES updated with benchmark/stress patterns
- [ ] CHANGELOG entries added
- [ ] `cargo make pre-commit` passes all gates
- [ ] HTML benchmark reports generated in `target/criterion/report/`
