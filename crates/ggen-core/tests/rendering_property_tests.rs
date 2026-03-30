/// Property-based tests for template rendering.
///
/// These tests verify invariants that must hold for ALL inputs, not just
/// specific examples. Uses proptest for automated input generation.

use ggen_core::graph::{CachedResult, Graph};
use ggen_core::register::register_all;
use proptest::prelude::*;
use serde_json::{Map, Value};
use tera::{Context, Tera};

fn workspace_root() -> std::path::PathBuf {
    let mut p = std::path::PathBuf::from(env!("CARGO_MANIFEST_DIR"));
    p.pop(); // crates/ggen-core -> crates
    p.pop(); // crates -> ggen root
    p
}

fn to_clean_json(result: &CachedResult) -> Value {
    match result {
        CachedResult::Solutions(rows) => {
            let arr: Vec<Value> = rows
                .iter()
                .map(|row| {
                    let mut obj = Map::new();
                    for (k, v) in row {
                        let clean = v.trim_start_matches('"').trim_end_matches('"');
                        obj.insert(k.clone(), Value::String(clean.to_string()));
                    }
                    Value::Object(obj)
                })
                .collect();
            Value::Array(arr)
        }
        CachedResult::Boolean(b) => Value::Bool(*b),
        CachedResult::Graph(triples) => {
            let arr: Vec<Value> = triples
                .iter()
                .map(|t| Value::String(t.clone()))
                .collect();
            Value::Array(arr)
        }
    }
}

/// Build a TTL ontology with a server of the given name and N tools.
fn build_mcp_ontology(server_name: &str, tool_names: &[String]) -> String {
    let mut ttl = format!(
        r#"
@prefix mcp: <https://ggen.dev/ontology/mcp#> .
@prefix rdfs: <http://www.w3.org/2000/01/rdf-schema#> .
@prefix ex:   <https://ggen.dev/examples/e2e#> .

ex:Transport a mcp:Transport ; rdfs:label "stdio" .
ex:Server a mcp:McpsServer ;
    mcp:serverName "{}" ;
    mcp:serverVersion "1.0.0" ;
    mcp:serverDescription "Test server" ;
    mcp:hasTransport ex:Transport .
"#,
        server_name
    );

    for (i, tool_name) in tool_names.iter().enumerate() {
        ttl.push_str(&format!(
            "ex:Tool{} a mcp:Tool ; mcp:toolName \"{}\" ; mcp:toolDescription \"Tool {}\" .\n",
            i, tool_name, i
        ));
    }

    ttl
}

const MCP_SPARQL: &str = r#"
    PREFIX mcp: <https://ggen.dev/ontology/mcp#>
    PREFIX rdfs: <http://www.w3.org/2000/01/rdf-schema#>
    SELECT ?server_name ?server_version ?server_description ?transport_type
           ?tool_name ?tool_description
    WHERE {
      ?server a mcp:McpsServer ;
        mcp:serverName ?server_name ; mcp:serverVersion ?server_version ;
        mcp:serverDescription ?server_description ; mcp:hasTransport ?transport .
      ?transport rdfs:label ?transport_type .
      ?tool a mcp:Tool ; mcp:toolName ?tool_name ; mcp:toolDescription ?tool_description .
    }
    ORDER BY ?tool_name
"#;

/// Minimal template that exercises the core rendering path.
const MINIMAL_TEMPLATE: &str = r#"
// Server: {{ server_name }}
// Tools: {{ sparql_results | length }}
{% for row in sparql_results %}
// - {{ row.tool_name }}: {{ row.tool_description | default(value="") }}
{% endfor %}
"#;

// ---------------------------------------------------------------------------
// Property 1: Rendering is deterministic — same input always produces same output
// ---------------------------------------------------------------------------

proptest! {
    #[test]
    fn prop_rendering_is_deterministic(
        server_name in "[a-z]{3,15}",
        num_tools in 0usize..5usize,
    ) {
        let tool_names: Vec<String> = (0..num_tools)
            .map(|i| format!("tool_{}", i))
            .collect();

        let ontology = build_mcp_ontology(&server_name, &tool_names);
        let graph = Graph::new().expect("Graph::new");
        graph.insert_turtle(&ontology).expect("insert_turtle");

        let result = graph.query_cached(MCP_SPARQL).expect("query_cached");
        let json1 = to_clean_json(&result);

        // Re-query same graph — must produce identical results
        let result2 = graph.query_cached(MCP_SPARQL).expect("query_cached 2");
        let json2 = to_clean_json(&result2);

        prop_assert_eq!(json1.clone(), json2.clone());

        // Render template with both — must produce identical output
        let mut tera = Tera::default();
        register_all(&mut tera);

        let mut ctx1 = Context::new();
        ctx1.insert("server_name", &server_name);
        ctx1.insert("sparql_results", &json1);
        let rendered1 = tera.render_str(MINIMAL_TEMPLATE, &ctx1).unwrap();

        let mut ctx2 = Context::new();
        ctx2.insert("server_name", &server_name);
        ctx2.insert("sparql_results", &json2);
        let rendered2 = tera.render_str(MINIMAL_TEMPLATE, &ctx2).unwrap();

        prop_assert_eq!(rendered1, rendered2);
    }
}

// ---------------------------------------------------------------------------
// Property 2: Output always contains the server name
// ---------------------------------------------------------------------------

proptest! {
    #[test]
    fn prop_output_always_contains_server_name(
        server_name in "[a-z]{3,15}",
        num_tools in 0usize..5usize,
    ) {
        let tool_names: Vec<String> = (0..num_tools)
            .map(|i| format!("tool_{}", i))
            .collect();

        let ontology = build_mcp_ontology(&server_name, &tool_names);
        let graph = Graph::new().expect("Graph::new");
        graph.insert_turtle(&ontology).expect("insert_turtle");

        let result = graph.query_cached(MCP_SPARQL).expect("query_cached");
        let json = to_clean_json(&result);

        let mut tera = Tera::default();
        register_all(&mut tera);

        let mut ctx = Context::new();
        ctx.insert("server_name", &server_name);
        ctx.insert("sparql_results", &json);

        let rendered = tera.render_str(MINIMAL_TEMPLATE, &ctx).unwrap();

        prop_assert!(rendered.contains(&server_name));
    }
}

// ---------------------------------------------------------------------------
// Property 3: Tool count in output matches SPARQL result count
// ---------------------------------------------------------------------------

proptest! {
    #[test]
    fn prop_tool_count_matches_sparql_results(
        num_tools in 0usize..5usize,
    ) {
        let tool_names: Vec<String> = (0..num_tools)
            .map(|i| format!("tool_{}", i))
            .collect();

        let ontology = build_mcp_ontology("test_server", &tool_names);
        let graph = Graph::new().expect("Graph::new");
        graph.insert_turtle(&ontology).expect("insert_turtle");

        let result = graph.query_cached(MCP_SPARQL).expect("query_cached");
        let json = to_clean_json(&result);

        let mut tera = Tera::default();
        register_all(&mut tera);

        let mut ctx = Context::new();
        ctx.insert("server_name", "test_server");
        ctx.insert("sparql_results", &json);

        let rendered = tera.render_str(MINIMAL_TEMPLATE, &ctx).unwrap();

        // Count tool lines (lines starting with "// - ")
        let tool_lines = rendered
            .lines()
            .filter(|l| l.trim().starts_with("// -"))
            .count();

        prop_assert_eq!(tool_lines, num_tools as usize);
    }
}

// ---------------------------------------------------------------------------
// Property 4: No Tera artifacts leak into rendered output
// ---------------------------------------------------------------------------

proptest! {
    #[test]
    fn prop_no_tera_artifacts_in_output(
        server_name in "[a-z]{3,15}",
        num_tools in 0usize..5usize,
    ) {
        let tool_names: Vec<String> = (0..num_tools)
            .map(|i| format!("tool_{}", i))
            .collect();

        let ontology = build_mcp_ontology(&server_name, &tool_names);
        let graph = Graph::new().expect("Graph::new");
        graph.insert_turtle(&ontology).expect("insert_turtle");

        let result = graph.query_cached(MCP_SPARQL).expect("query_cached");
        let json = to_clean_json(&result);

        let mut tera = Tera::default();
        register_all(&mut tera);

        let mut ctx = Context::new();
        ctx.insert("server_name", &server_name);
        ctx.insert("sparql_results", &json);

        let rendered = tera.render_str(MINIMAL_TEMPLATE, &ctx).unwrap();

        prop_assert!(!rendered.contains("{{"), "Output contains unclosed Tera expr");
        prop_assert!(!rendered.contains("}}"), "Output contains unclosed Tera expr");
        prop_assert!(!rendered.contains("{%"), "Output contains unclosed Tera tag");
        prop_assert!(!rendered.contains("%}"), "Output contains unclosed Tera tag");
    }
}

// ---------------------------------------------------------------------------
// Property 5: Empty ontology produces zero tools but valid output
// ---------------------------------------------------------------------------

#[test]
fn prop_empty_ontology_renders_without_error() {
    let ontology = build_mcp_ontology("empty_server", &[]);
    let graph = Graph::new().expect("Graph::new");
    graph.insert_turtle(&ontology).expect("insert_turtle");

    let result = graph.query_cached(MCP_SPARQL).expect("query_cached");
    let json = to_clean_json(&result);

    let mut tera = Tera::default();
    register_all(&mut tera);

    let mut ctx = Context::new();
    ctx.insert("server_name", "empty_server");
    ctx.insert("sparql_results", &json);

    let rendered = tera.render_str(MINIMAL_TEMPLATE, &ctx).unwrap();

    assert!(rendered.contains("empty_server"));
    assert!(rendered.contains("Tools: 0"));
}
