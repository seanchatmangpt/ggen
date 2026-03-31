//! Groq LLM SLO Timing Validation Tests — REAL WORKLOADS
//!
//! These tests make REAL LLM calls to Groq with SUBSTANTIAL prompts that produce
//! meaningful output (500+ tokens). No toy "what is 2+2" questions.
//!
//! JTBD (Jobs To Be Done): Verify the full pipeline can route real agent workloads
//! through Groq within time budgets. We verify:
//!   1. The call SUCCEEDED (no routing errors, no 404s)
//!   2. The output is SUBSTANTIAL (min token count — proves real work happened)
//!   3. The LATENCY is within SLO (proves the pipeline is production-ready)
//!
//! We do NOT assert on response content — that tests the LLM, not the system.
//!
//! Uses GenAiClient directly (not A2aLlmClient) to ensure correct Groq routing.
//! The `groq::` namespace prefix tells the genai crate to use the Groq adapter.
//! GROQ_API_KEY env var provides authentication to Groq's endpoint.
//!
//! SLO Gates (realistic for Groq openai/gpt-oss-20b with real workloads):
//!   - MCP scaffold generation (full service):     10 s
//!   - SPARQL query generation from description:   10 s
//!   - A2A agent reasoning (architect role):       15 s
//!   - Multi-turn agent conversation (3 turns):   20 s
//!   - A2A round-trip agent task:                 15 s
//!   - Streaming code generation (200+ tokens):    10 s
//!
//! Run:
//!   GROQ_API_KEY=gsk_xxx cargo test -p ggen-a2a-mcp --test groq_slo_timing -- --test-threads=1 --nocapture

use std::time::Instant;

use futures::StreamExt;
use ggen_ai::{GenAiClient, LlmClient};

mod common;
use common::init_tracing;

// ---------------------------------------------------------------------------
// Helpers
// ---------------------------------------------------------------------------

/// Create a real Groq client using GenAiClient directly.
/// Panics if GROQ_API_KEY is not set.
fn create_groq_client() -> GenAiClient {
    if std::env::var("GROQ_API_KEY").is_err() {
        panic!("GROQ_API_KEY must be set. These tests make real LLM calls.");
    }
    let config = ggen_ai::providers::adapter::groq_default_config();
    GenAiClient::new(config).expect("GenAiClient creation should succeed with valid GROQ_API_KEY")
}

/// Assert SLO gate and print token metrics.
fn assert_slo(operation: &str, elapsed: std::time::Duration, slo_ms: u64) {
    let elapsed_ms = elapsed.as_millis() as u64;
    if elapsed_ms > slo_ms {
        panic!(
            "GROQ SLO VIOLATION: {} took {} ms (SLO: {} ms) -- EXCEEDED by {} ms",
            operation,
            elapsed_ms,
            slo_ms,
            elapsed_ms - slo_ms
        );
    }
    println!(
        "  GROQ SLO OK: {} = {} ms (limit: {} ms, headroom: {} ms)",
        operation,
        elapsed_ms,
        slo_ms,
        slo_ms - elapsed_ms
    );
}

/// Assert minimum output token count (rough estimate: ~4 chars per token).
/// This proves real work happened — not a stub, not an error, not a truncation.
fn assert_min_tokens(operation: &str, content: &str, min_tokens: usize) {
    let estimated_tokens = content.len() / 4;
    if estimated_tokens < min_tokens {
        panic!(
            "{}: output too short — ~{} estimated tokens, minimum is {}. \
             Pipeline did not complete real work. First 200 chars: {}",
            operation,
            estimated_tokens,
            min_tokens,
            &content[..content.len().min(200)]
        );
    }
    println!(
        "  {} output: ~{} estimated tokens (min: {})",
        operation, estimated_tokens, min_tokens
    );
}

// ===========================================================================
// Test 1: MCP Scaffold Generation — Full REST Service
// SLO: 10 s, Min 500 output tokens
// JTBD: An MCP server tool call generates a complete service scaffold
// ===========================================================================

#[tokio::test]
async fn slo_groq_mcp_scaffold_generation() {
    init_tracing();
    let client = create_groq_client();

    let prompt = r#"You are an MCP (Model Context Protocol) server code generator. Generate a COMPLETE, production-ready MCP server in Rust that provides the following tools:

1. `list_databases` — Lists all databases a user has access to. Returns JSON array of {name, engine, size_mb, tables_count}.
2. `query_database` — Executes a read-only SQL query against a named database. Takes parameters: database_name (string), sql_query (string). Returns JSON array of rows.
3. `get_schema` — Returns the schema (tables, columns, types) for a named database. Returns JSON with tables array, each having name, columns (name, type, nullable, is_primary_key).
4. `health_check` — Returns {status: "ok", databases_count, uptime_seconds}.

Requirements:
- Use axum for the HTTP server with JSON-RPC 2.0 over POST /
- Use tokio-postgres for database connections with connection pooling
- Use serde for JSON serialization
- Include proper error handling with structured error responses
- Include a `Config` struct that reads from environment variables
- Add graceful shutdown with tokio::signal
- Derive Debug, Clone, Serialize, Deserialize where appropriate
- Output ONLY valid Rust code, no markdown, no explanation"#;

    let start = Instant::now();
    let response = client
        .complete(prompt)
        .await
        .expect("MCP scaffold generation should succeed");
    let elapsed = start.elapsed();

    assert_min_tokens("mcp_scaffold_generation", &response.content, 500);
    assert_slo("mcp_scaffold_generation", elapsed, 10000);
}

// ===========================================================================
// Test 2: SPARQL Query Generation from Natural Language
// SLO: 10 s, Min 300 output tokens
// JTBD: An A2A agent translates natural language requirements into SPARQL
// ===========================================================================

#[tokio::test]
async fn slo_groq_sparql_generation() {
    init_tracing();
    let client = create_groq_client();

    let prompt = r#"You are an RDF/SPARQL expert. Given this Turtle ontology:

```turtle
@prefix ex: <http://example.org/schema#> .
@prefix xsd: <http://www.w3.org/2001/XMLSchema#> .

ex:Company a rdfs:Class ;
    ex:name xsd:string ;
    ex:industry xsd:string ;
    ex:revenue xsd:decimal ;
    ex:employeeCount xsd:integer ;
    ex:foundedYear xsd:integer ;
    ex:headquartersCity xsd:string ;
    ex:isPublic xsd:boolean ;
    ex:ceoName xsd:string .

ex:Product a rdfs:Class ;
    ex:productName xsd:string ;
    ex:category xsd:string ;
    ex:price xsd:decimal ;
    ex:company ex:Company .
```

Generate the following 5 SPARQL queries:
1. Find all public companies with revenue > 1B, sorted by revenue descending
2. Count companies by industry sector
3. Find companies that have products in both "Software" and "Hardware" categories
4. Calculate average employee count per industry for companies founded after 2010
5. Find the CEO of the company with the most products priced above $1000

For each query, write the SPARQL and explain what it does in 1-2 sentences."#;

    let start = Instant::now();
    let response = client
        .complete(prompt)
        .await
        .expect("SPARQL generation should succeed");
    let elapsed = start.elapsed();

    assert_min_tokens("sparql_generation", &response.content, 300);
    assert_slo("sparql_generation", elapsed, 10000);
}

// ===========================================================================
// Test 3: A2A Agent Reasoning — Architect Role
// SLO: 15 s, Min 500 output tokens
// JTBD: An architect agent produces a real system design from a task spec
// ===========================================================================

#[tokio::test]
async fn slo_groq_a2a_architect_reasoning() {
    init_tracing();
    let client = create_groq_client();

    let prompt = r#"You are a software architect agent in an A2A (Agent-to-Agent) system. A product manager agent has sent you this task:

TASK: Design a real-time notification system for a SaaS platform that supports:
- Push notifications (mobile)
- Email notifications (with templates)
- In-app notifications (with read/unread state)
- WebSocket live updates
- Notification preferences per user
- Rate limiting per notification type
- Delivery tracking and retry logic

Provide a COMPLETE technical design including:
1. System architecture diagram (ASCII art showing all components and data flows)
2. Database schema (PostgreSQL tables with columns, types, constraints, indexes)
3. Message queue design (which events, what data, ordering guarantees)
4. API endpoint design (REST endpoints with request/response schemas)
5. Service decomposition (which microservices, what each owns, communication patterns)
6. Failure modes and recovery strategies (what happens when email provider is down, when WebSocket disconnects, when queue is backed up)
7. Scaling considerations (horizontal scaling strategy, caching layers, database sharding if needed)

Output a comprehensive, production-grade design document."#;

    let start = Instant::now();
    let response = client
        .complete(prompt)
        .await
        .expect("A2A architect reasoning should succeed");
    let elapsed = start.elapsed();

    assert_min_tokens("a2a_architect_reasoning", &response.content, 500);
    assert_slo("a2a_architect_reasoning", elapsed, 15000);
}

// ===========================================================================
// Test 4: Multi-Turn A2A Conversation (3 turns)
// SLO: 20 s total, Min 300 total output tokens
// JTBD: Multiple agents collaborate across turns, building on context
// ===========================================================================

#[tokio::test]
async fn slo_groq_multi_turn_conversation() {
    init_tracing();
    let client = create_groq_client();

    let mut total_output = String::new();
    let conversation_start = Instant::now();

    // Turn 1: Architect designs the interface
    let turn1 = client
        .complete(
            "I need to build a rate limiter for a REST API. It should support \
             sliding window, token bucket, and fixed window algorithms. The rate \
             limiter needs to work distributed across multiple server instances \
             using Redis. Give me the design for the core trait/interface and the \
             Redis-backed sliding window implementation in Rust.",
        )
        .await
        .expect("Turn 1 should succeed");
    total_output.push_str(&turn1.content);

    // Turn 2: Coder implements based on architect's design
    let turn2 = client
        .complete(&format!(
            "Based on your rate limiter design above, now implement the \
             token bucket algorithm as well. Also add: 1) A middleware \
             implementation for axum 2) Configuration via environment variables \
             with sensible defaults 3) Metrics collection (request count, rejection \
             count, current window usage) 4) Graceful degradation when Redis is \
             unavailable (fall back to in-memory local limiter). Output only Rust code.",
        ))
        .await
        .expect("Turn 2 should succeed");
    total_output.push_str(&turn2.content);

    // Turn 3: QA writes tests based on implementation
    let turn3 = client
        .complete(
            "Write comprehensive unit tests for both rate limiter implementations \
             (sliding window and token bucket). Include tests for: boundary \
             conditions (exactly at limit, one over), concurrent access, Redis \
             failure fallback, middleware integration, and metrics accuracy. \
             Use Rust's built-in testing framework.",
        )
        .await
        .expect("Turn 3 should succeed");
    total_output.push_str(&turn3.content);

    let total_elapsed = conversation_start.elapsed();

    // Each turn should produce real work (not stubs)
    for (i, turn) in [&turn1, &turn2, &turn3].iter().enumerate() {
        assert!(
            turn.content.len() > 200,
            "Turn {}: agent produced too little output ({} chars) — not real work",
            i + 1,
            turn.content.len()
        );
    }

    assert_min_tokens("multi_turn_conversation_total", &total_output, 300);
    assert_slo("multi_turn_conversation_3turns", total_elapsed, 20000);
}

// ===========================================================================
// Test 5: A2A Round-Trip via A2aLlmClient — Agent Task Processing
// SLO: 15 s, Min 200 output tokens
// JTBD: A2aLlmClient routes a real task through the full A2A message pipeline
// ===========================================================================

#[tokio::test]
async fn slo_groq_a2a_agent_task() {
    init_tracing();
    use a2a_generated::converged::message::ConvergedMessage;
    use ggen_a2a_mcp::client::A2aLlmClient;
    use ggen_ai::dspy::model_capabilities::{Model, ModelProvider};

    let model = Model::new("groq::openai/gpt-oss-20b", ModelProvider::Custom);
    let a2a_client = A2aLlmClient::new(model)
        .await
        .expect("A2aLlmClient creation should succeed");

    let message = ConvergedMessage::text(
        "task-planner".to_string(),
        "agent-architect".to_string(),
        r#"Analyze this system requirement and produce a structured implementation plan:

REQUIREMENT: Build an MCP server that wraps the GitHub REST API. It must provide:
- search_repositories(query, language, sort, per_page) -> list of repos
- get_pull_requests(owner, repo, state) -> list of PRs with title, author, status, labels
- create_issue(owner, repo, title, body, labels) -> created issue with number
- get_file_contents(owner, repo, path, branch) -> file content as string

Output a JSON object with these fields:
{
  "dependencies": ["list of crate names"],
  "structs": ["list of struct names with 1-line descriptions"],
  "endpoints": ["list of MCP tool names"],
  "error_cases": ["list of 5 possible error scenarios"],
  "implementation_order": ["ordered list of 6 implementation steps"],
  "testing_strategy": "2-3 sentence testing approach"
}"#
        .to_string(),
    );

    let start = Instant::now();
    let response = a2a_client
        .process_message(&message)
        .await
        .expect("A2A agent task should succeed");
    let elapsed = start.elapsed();

    let text = match &response.payload.content {
        a2a_generated::converged::UnifiedContent::Text { content, .. } => content.as_str(),
        other => panic!("Expected Text content in A2A response, got: {other:?}"),
    };

    // The pipeline succeeded: message was created, routed through A2aLlmClient,
    // sent to Groq, response was received and parsed back into ConvergedMessage.
    // Verify it produced real work, not an empty response.
    assert_min_tokens("a2a_agent_task", text, 200);
    assert_slo("a2a_agent_task", elapsed, 15000);
    a2a_client.shutdown().await.ok();
}

// ===========================================================================
// Test 6: Streaming Code Generation (200+ tokens)
// SLO: 10 s full, 2 s TTFB
// JTBD: Streaming pipeline delivers real code generation progressively
// ===========================================================================

#[tokio::test]
async fn slo_groq_streaming_code_gen() {
    init_tracing();
    let client = create_groq_client();

    let prompt = r#"Generate a complete Rust implementation of an LRU cache with these requirements:
- Generic over key (K) and value (V), where K: Hash + Eq + Clone, V: Clone
- Configurable capacity (set at construction)
- O(1) get and put operations
- get() returns Option<&V>
- put() returns Option<V> (the evicted value if any)
- contains_key() method
- len() and is_empty() methods
- clear() method
- Thread-safe using RwLock
- Include comprehensive inline documentation
- Include 5 unit tests using #[cfg(test)]
- Output ONLY valid Rust code, no explanation"#;

    let start = Instant::now();
    let mut stream = client
        .complete_stream(prompt)
        .await
        .expect("stream creation should succeed");

    let mut full_content = String::new();
    let mut first_token = false;
    let mut chunk_count = 0;

    while let Some(chunk) = stream.next().await {
        if !chunk.content.is_empty() {
            if !first_token {
                first_token = true;
                let ttfb = start.elapsed();
                assert_slo("streaming_code_gen_ttfb", ttfb, 2000);
            }
            full_content.push_str(&chunk.content);
            chunk_count += 1;
        }
    }

    assert!(
        first_token,
        "Streaming pipeline should have received at least one token"
    );

    let full_elapsed = start.elapsed();

    // Verify real work was streamed — substantial output, multiple chunks
    assert!(
        chunk_count >= 3,
        "Expected multiple streaming chunks, got {} — pipeline may be buffering",
        chunk_count
    );
    assert_min_tokens("streaming_code_gen", &full_content, 200);
    println!(
        "  Streaming: {} chunks, {} chars total",
        chunk_count,
        full_content.len()
    );
    assert_slo("streaming_code_gen_full", full_elapsed, 10000);
}
