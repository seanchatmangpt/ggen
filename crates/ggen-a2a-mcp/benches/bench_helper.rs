//! Benchmark helper utilities for GgenMcpServer
//!
//! Provides common setup/teardown for Criterion benchmarks and stress tests.

use rmcp::{model::ClientInfo, ClientHandler};

/// Minimal client handler for benchmarking
#[derive(Debug, Clone, Default)]
#[allow(dead_code)]
pub struct BenchClientHandler;

impl ClientHandler for BenchClientHandler {
    fn get_info(&self) -> ClientInfo {
        ClientInfo::default()
    }
}

/// Example name for benchmarks (uses a2a-agent-lifecycle)
#[allow(dead_code)]
pub const BENCHMARK_EXAMPLE: &str = "a2a-agent-lifecycle";

/// SPARQL SELECT query that returns all triples
#[allow(dead_code)]
pub const SPARQL_SELECT_ALL: &str = "SELECT ?s ?p ?o WHERE { ?s ?p ?o } LIMIT 100";

/// Valid TTL content for benchmarks (100 triples, inline)
#[allow(dead_code)]
pub const VALID_TTL_100: &str = r#"@prefix ex: <http://example.org/> .
ex:s0 ex:p0 ex:o0 .
ex:s1 ex:p1 ex:o1 .
ex:s2 ex:p2 ex:o2 .
ex:s3 ex:p3 ex:o3 .
ex:s4 ex:p4 ex:o4 .
ex:s5 ex:p5 ex:o5 .
ex:s6 ex:p6 ex:o6 .
ex:s7 ex:p7 ex:o7 .
ex:s8 ex:p8 ex:o8 .
ex:s9 ex:p9 ex:o9 .
"#;
