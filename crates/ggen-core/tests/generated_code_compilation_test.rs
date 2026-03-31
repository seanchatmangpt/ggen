//! Tests that rendered Tera templates produce syntactically valid code artifacts.
//!
//! These tests fill the gap between `pipeline_e2e_test.rs` (which validates rendering
//! succeeds) and actual compilation. Since `rustfmt`/`gofmt`/`javac`/`tsc` may not be
//! installed, we validate syntax patterns via string checks and simple brace-balancing.
//!
//! Pipeline: ontology (.ttl) -> SPARQL SELECT (.rq) -> Tera (.tera) -> code artifact
//! A = mu(O)

use ggen_core::graph::{CachedResult, Graph};
use ggen_core::register::register_all;
use serde_json::{Map, Value};
use tera::{Context, Tera};

// ---------------------------------------------------------------------------
// Helpers
// ---------------------------------------------------------------------------

fn workspace_root() -> std::path::PathBuf {
    let mut p = std::path::PathBuf::from(env!("CARGO_MANIFEST_DIR"));
    p.pop(); // crates/ggen-core -> crates
    p.pop(); // crates -> ggen root
    p
}

/// Convert CachedResult to clean JSON, stripping RDF literal quotes.
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
            let arr: Vec<Value> = triples.iter().map(|t| Value::String(t.clone())).collect();
            Value::Array(arr)
        }
    }
}

/// Check that braces are balanced in a string (counts `{` vs `}`).
fn braces_balanced(code: &str) -> bool {
    let mut depth: i32 = 0;
    for ch in code.chars() {
        match ch {
            '{' => depth += 1,
            '}' => depth -= 1,
            _ => {}
        }
        if depth < 0 {
            return false;
        }
    }
    depth == 0
}

/// Check that parentheses are balanced in a string.
fn parens_balanced(code: &str) -> bool {
    let mut depth: i32 = 0;
    for ch in code.chars() {
        match ch {
            '(' => depth += 1,
            ')' => depth -= 1,
            _ => {}
        }
        if depth < 0 {
            return false;
        }
    }
    depth == 0
}

/// Check that brackets are balanced in a string.
fn brackets_balanced(code: &str) -> bool {
    let mut depth: i32 = 0;
    for ch in code.chars() {
        match ch {
            '[' => depth += 1,
            ']' => depth -= 1,
            _ => {}
        }
        if depth < 0 {
            return false;
        }
    }
    depth == 0
}

/// Check that no unrendered Tera template expressions remain in the output.
///
/// This is more nuanced than a simple `contains("{{")` because:
/// - Go templates use `{% raw %}{{Type: "text"}}{% endraw %}` which produces
///   literal `{{` in the output (valid Go struct literal syntax).
/// - We only flag patterns that look like actual unrendered template expressions:
///   `{{ variable }}` (with spaces), `{% for ... %}`, `{% if ... %}`, etc.
fn no_tera_artifacts(rendered: &str) -> bool {
    // Check for unrendered `{%` block tags (for, if, else, endif, endfor, raw, etc.)
    let tera_block_patterns = ["{% ", "{%\n", " %}", "%\n"];
    for pat in &tera_block_patterns {
        if rendered.contains(pat) {
            return false;
        }
    }

    // Check for unrendered `{{ variable }}` expressions.
    // Tera expressions have `{{ ` (opening) or `{{` followed by a filter `|`.
    // Go struct literals like `{{Type: "text"}}` have `{{` immediately followed
    // by an uppercase letter (no space), so we distinguish by requiring a space
    // after `{{` or a pipe `|` (filter) for Tera expressions.
    let mut search_from = 0;
    while let Some(idx) = rendered[search_from..].find("{{") {
        let abs_idx = search_from + idx;
        let after = &rendered[abs_idx + 2..];
        let next_ch = after.chars().next();
        match next_ch {
            // `{{ ` (space) = Tera expression start
            Some(' ') => return false,
            // `{{|` or `{{ variable|` (filter) = Tera expression
            Some('|') => return false,
            // `{{\n` (newline) = Tera expression start
            Some('\n') | Some('\r') => return false,
            // `{{-` (whitespace trim) = Tera expression
            Some('-') => return false,
            // `{{` followed by uppercase letter = likely Go struct literal (e.g., {{Type:}})
            // or other non-Tera syntax -- skip
            _ => {}
        }
        search_from = abs_idx + 2;
    }

    true
}

// ---------------------------------------------------------------------------
// Render helper
// ---------------------------------------------------------------------------

/// Render a template from the workspace templates directory with graph-backed SPARQL data.
///
/// Takes a template filename (relative to `templates/`), an ontology TTL string,
/// a SPARQL query, and additional context key-value pairs (inserted into the Tera context).
/// Returns the rendered string on success, or panics on failure.
fn render_template_with_graph(
    template_filename: &str, ontology: &str, sparql: &str, extra_ctx: Vec<(&str, &str)>,
) -> String {
    let root = workspace_root();

    // 1. Load ontology and execute SPARQL
    let graph = Graph::new().unwrap_or_else(|e| panic!("Graph::new failed: {}", e));
    graph
        .insert_turtle(ontology)
        .unwrap_or_else(|e| panic!("insert_turtle failed: {}", e));

    let result = graph
        .query_cached(sparql)
        .unwrap_or_else(|e| panic!("query_cached failed: {}", e));
    let json = to_clean_json(&result);

    // 2. Load and parse the template
    let mut tera = Tera::default();
    register_all(&mut tera);

    let template_path = root.join("templates").join(template_filename);
    let template_content = std::fs::read_to_string(&template_path)
        .unwrap_or_else(|e| panic!("Cannot read {}: {}", template_path.display(), e));

    let template_name = template_filename.replace('/', "_").replace('.', "_");
    tera.add_raw_template(&template_name, &template_content)
        .unwrap_or_else(|e| panic!("Template parse error for {}: {}", template_filename, e));

    // 3. Build context with SPARQL results and any extra variables
    let mut ctx = Context::new();
    ctx.insert("sparql_results", &json);
    for (key, val) in &extra_ctx {
        ctx.insert(*key, *val);
    }

    // 4. Render
    tera.render(&template_name, &ctx)
        .unwrap_or_else(|e| panic!("Render failed for {}: {}", template_filename, e))
}

// ---------------------------------------------------------------------------
// Minimal test ontologies
// ---------------------------------------------------------------------------

const MINIMAL_MCP_ONTOLOGY: &str = r#"
@prefix mcp: <https://ggen.dev/ontology/mcp#> .
@prefix rdfs: <http://www.w3.org/2000/01/rdf-schema#> .
@prefix ex:   <https://ggen.dev/examples/e2e#> .

ex:StdioTransport a mcp:Transport ; rdfs:label "stdio" .
ex:TestServer a mcp:McpsServer ;
    mcp:serverName "e2e_test_server" ;
    mcp:serverVersion "0.1.0" ;
    mcp:serverDescription "End-to-end test MCP server" ;
    mcp:hasTransport ex:StdioTransport .
ex:GetWeather a mcp:Tool ;
    mcp:toolName "get_weather" ;
    mcp:toolDescription "Gets weather for a city" .
ex:GetTime a mcp:Tool ;
    mcp:toolName "get_time" ;
    mcp:toolDescription "Gets current time in a timezone" .
"#;

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

const MINIMAL_A2A_ONTOLOGY: &str = r#"
@prefix a2a: <https://ggen.dev/ontology/a2a#> .
@prefix ex:   <https://ggen.dev/examples/e2e#> .
ex:ReviewAgent a a2a:Agent ;
    a2a:agentName "code_reviewer" ;
    a2a:agentVersion "1.0.0" ;
    a2a:agentDescription "Reviews code for quality issues" ;
    a2a:agentUrl "http://localhost:8090" ;
    a2a:providerName "ggen" ;
    a2a:providerUrl "https://ggen.dev" .
ex:ReviewCode a a2a:Skill ;
    a2a:skillName "review_code" ;
    a2a:skillDescription "Reviews a code snippet for bugs" ;
    a2a:skillTags "[\"code\", \"review\"]" ;
    a2a:streaming false ; a2a:timeoutMs "5000" ; a2a:retryPolicy "none" .
ex:SuggestFixes a a2a:Skill ;
    a2a:skillName "suggest_fixes" ;
    a2a:skillDescription "Suggests fixes for identified issues" ;
    a2a:skillTags "[\"code\", \"suggestions\"]" ;
    a2a:streaming true ; a2a:timeoutMs "10000" ; a2a:retryPolicy "once" .
"#;

const A2A_SPARQL: &str = r#"
    PREFIX a2a: <https://ggen.dev/ontology/a2a#>
    SELECT ?agent_name ?agent_version ?agent_description ?agent_url
           ?provider_name ?provider_url ?skill_name ?skill_description
           ?skill_tags ?streaming ?timeout_ms ?retry_policy
    WHERE {
      ?agent a a2a:Agent ;
        a2a:agentName ?agent_name ; a2a:agentVersion ?agent_version ;
        a2a:agentDescription ?agent_description ; a2a:agentUrl ?agent_url ;
        a2a:providerName ?provider_name ; a2a:providerUrl ?provider_url .
      ?skill a a2a:Skill ;
        a2a:skillName ?skill_name ; a2a:skillDescription ?skill_description ;
        a2a:skillTags ?skill_tags ; a2a:streaming ?streaming ;
        a2a:timeoutMs ?timeout_ms ; a2a:retryPolicy ?retry_policy .
    }
    ORDER BY ?skill_name
"#;

// ===========================================================================
// Test 1: Rendered Rust code (mcp-rust.tera) has valid syntax patterns
// ===========================================================================

#[test]
fn test_rendered_rust_code_is_valid_syntax() {
    let rendered = render_template_with_graph(
        "mcp-rust.tera",
        MINIMAL_MCP_ONTOLOGY,
        MCP_SPARQL,
        vec![
            ("server_name", "e2e_test_server"),
            ("server_version", "0.1.0"),
            ("server_description", "End-to-end test MCP server"),
            ("transport_type", "stdio"),
        ],
    );

    // No Tera artifacts
    assert!(
        no_tera_artifacts(&rendered),
        "Rendered Rust code contains Tera delimiters"
    );

    // Balanced delimiters
    assert!(
        braces_balanced(&rendered),
        "Rendered Rust code has unbalanced braces"
    );
    assert!(
        parens_balanced(&rendered),
        "Rendered Rust code has unbalanced parentheses"
    );
    assert!(
        brackets_balanced(&rendered),
        "Rendered Rust code has unbalanced brackets"
    );

    // Contains Rust-specific constructs
    assert!(
        rendered.contains("pub struct"),
        "Should contain pub struct declarations"
    );
    assert!(rendered.contains("fn "), "Should contain fn declarations");
    assert!(rendered.contains("impl "), "Should contain impl blocks");
    assert!(
        rendered.contains("#[derive"),
        "Should contain derive attributes"
    );
    assert!(
        rendered.contains("async fn main"),
        "Should contain async fn main"
    );
    assert!(rendered.contains("use rmcp"), "Should import rmcp SDK");

    // Contains rendered tool names
    assert!(
        rendered.contains("get_time"),
        "Should contain get_time tool"
    );
    assert!(
        rendered.contains("get_weather"),
        "Should contain get_weather tool"
    );
    assert!(
        rendered.contains("GetTimeParams"),
        "Should contain GetTimeParams struct"
    );
    assert!(
        rendered.contains("GetWeatherParams"),
        "Should contain GetWeatherParams struct"
    );
}

// ===========================================================================
// Test 2: Rendered Go code (mcp-go.tera) has valid syntax patterns
// ===========================================================================

#[test]
fn test_rendered_go_code_is_valid_syntax() {
    let rendered = render_template_with_graph(
        "mcp-go.tera",
        MINIMAL_MCP_ONTOLOGY,
        MCP_SPARQL,
        vec![
            ("server_name", "e2e_test_server"),
            ("server_version", "0.1.0"),
            ("server_description", "End-to-end test MCP server"),
            ("transport_type", "stdio"),
        ],
    );

    // No Tera artifacts
    assert!(
        no_tera_artifacts(&rendered),
        "Rendered Go code contains Tera delimiters"
    );

    // Balanced delimiters
    assert!(
        braces_balanced(&rendered),
        "Rendered Go code has unbalanced braces"
    );
    assert!(
        parens_balanced(&rendered),
        "Rendered Go code has unbalanced parentheses"
    );

    // Contains Go-specific constructs
    assert!(
        rendered.contains("package main"),
        "Should have package main"
    );
    assert!(rendered.contains("func main()"), "Should have func main()");
    assert!(
        rendered.contains("func (s *MCPServer)"),
        "Should have method declarations"
    );
    assert!(rendered.contains("type "), "Should have type definitions");
    assert!(rendered.contains("import ("), "Should have import block");

    // Contains rendered tool names
    assert!(
        rendered.contains("get_time"),
        "Should contain get_time tool"
    );
    assert!(
        rendered.contains("get_weather"),
        "Should contain get_weather tool"
    );
    assert!(
        rendered.contains("GetTimeInput"),
        "Should contain GetTimeInput struct"
    );
    assert!(
        rendered.contains("GetWeatherInput"),
        "Should contain GetWeatherInput struct"
    );
}

// ===========================================================================
// Test 3: Rendered TypeScript code (mcp-typescript.tera) has valid syntax
// ===========================================================================

#[test]
fn test_rendered_typescript_code_is_valid_syntax() {
    let rendered = render_template_with_graph(
        "mcp-typescript.tera",
        MINIMAL_MCP_ONTOLOGY,
        MCP_SPARQL,
        vec![
            ("server_name", "e2e_test_server"),
            ("server_version", "0.1.0"),
            ("server_description", "End-to-end test MCP server"),
            ("transport_type", "stdio"),
        ],
    );

    // No Tera artifacts
    assert!(
        no_tera_artifacts(&rendered),
        "Rendered TypeScript code contains Tera delimiters"
    );

    // Balanced delimiters
    assert!(
        braces_balanced(&rendered),
        "Rendered TypeScript code has unbalanced braces"
    );
    assert!(
        parens_balanced(&rendered),
        "Rendered TypeScript code has unbalanced parentheses"
    );
    assert!(
        brackets_balanced(&rendered),
        "Rendered TypeScript code has unbalanced brackets"
    );

    // Contains TypeScript-specific constructs
    assert!(
        rendered.contains("import "),
        "Should have import statements"
    );
    assert!(
        rendered.contains("async function handle"),
        "Should have async function declarations"
    );
    assert!(
        rendered.contains("const "),
        "Should have const declarations"
    );
    assert!(
        rendered.contains(": Record<"),
        "Should have TypeScript type annotations"
    );
    assert!(
        rendered.contains("type "),
        "Should contain type alias declarations"
    );

    // Contains rendered tool names
    assert!(
        rendered.contains("get_time"),
        "Should contain get_time tool"
    );
    assert!(
        rendered.contains("get_weather"),
        "Should contain get_weather tool"
    );
}

// ===========================================================================
// Test 4: Rendered Java code (mcp-java.tera) has valid syntax patterns
// ===========================================================================

#[test]
fn test_rendered_java_code_is_valid_syntax() {
    let rendered = render_template_with_graph(
        "mcp-java.tera",
        MINIMAL_MCP_ONTOLOGY,
        MCP_SPARQL,
        vec![
            ("server_name", "e2e_test_server"),
            ("server_version", "0.1.0"),
            ("server_description", "End-to-end test MCP server"),
            ("transport_type", "stdio"),
        ],
    );

    // No Tera artifacts
    assert!(
        no_tera_artifacts(&rendered),
        "Rendered Java code contains Tera delimiters"
    );

    // Balanced delimiters
    assert!(
        braces_balanced(&rendered),
        "Rendered Java code has unbalanced braces"
    );
    assert!(
        parens_balanced(&rendered),
        "Rendered Java code has unbalanced parentheses"
    );

    // Contains Java-specific constructs
    assert!(
        rendered.contains("public class"),
        "Should contain public class declaration"
    );
    assert!(
        rendered.contains("public static void main"),
        "Should have main method"
    );
    assert!(
        rendered.contains("record "),
        "Should contain record declarations"
    );
    assert!(
        rendered.contains("package "),
        "Should have package declaration"
    );
    assert!(
        rendered.contains("import "),
        "Should have import statements"
    );

    // Contains rendered tool names
    assert!(
        rendered.contains("get_time"),
        "Should contain get_time tool"
    );
    assert!(
        rendered.contains("get_weather"),
        "Should contain get_weather tool"
    );
    assert!(
        rendered.contains("GetTimeInput"),
        "Should contain GetTimeInput record"
    );
    assert!(
        rendered.contains("GetWeatherInput"),
        "Should contain GetWeatherInput record"
    );
}

// ===========================================================================
// Test 5: ALL 10 templates produce output with zero Tera artifacts
// ===========================================================================

#[test]
fn test_no_tera_artifacts_in_any_rendered_output() {
    // MCP templates (use MCP ontology + SPARQL)
    let mcp_templates = [
        "mcp-rust.tera",
        "mcp-go.tera",
        "mcp-typescript.tera",
        "mcp-java.tera",
        "mcp-elixir.tera",
    ];

    let mcp_extra_ctx = vec![
        ("server_name", "e2e_test_server"),
        ("server_version", "0.1.0"),
        ("server_description", "End-to-end test MCP server"),
        ("transport_type", "stdio"),
    ];

    for template in &mcp_templates {
        let rendered = render_template_with_graph(
            template,
            MINIMAL_MCP_ONTOLOGY,
            MCP_SPARQL,
            mcp_extra_ctx.clone(),
        );
        assert!(
            no_tera_artifacts(&rendered),
            "{template}: rendered output contains Tera delimiters (double-brace or percent-brace)",
            template = template
        );
    }

    // A2A templates (use A2A ontology + SPARQL)
    let a2a_templates = [
        "a2a-rust.tera",
        "a2a-go.tera",
        "a2a-typescript.tera",
        "a2a-java.tera",
    ];

    let a2a_extra_ctx = vec![
        ("agent_name", "code_reviewer"),
        ("agent_version", "1.0.0"),
        ("agent_description", "Reviews code for quality issues"),
        ("agent_url", "http://localhost:8090"),
        ("provider_name", "ggen"),
        ("provider_url", "https://ggen.dev"),
    ];

    for template in &a2a_templates {
        let rendered = render_template_with_graph(
            template,
            MINIMAL_A2A_ONTOLOGY,
            A2A_SPARQL,
            a2a_extra_ctx.clone(),
        );
        assert!(
            no_tera_artifacts(&rendered),
            "{template}: rendered output contains Tera delimiters (double-brace or percent-brace)",
            template = template
        );
    }

    // A2A Elixir template uses `skills` variable, not `sparql_results`
    // We handle it separately with a custom render.
    let rendered = render_a2a_elixir_with_skills();
    assert!(
        no_tera_artifacts(&rendered),
        "a2a-elixir.tera: rendered output contains Tera delimiters (double-brace or percent-brace)"
    );
}

// ===========================================================================
// Test 6: Each rendered template contains expected language constructs
// ===========================================================================

#[test]
fn test_rendered_code_contains_expected_language_constructs() {
    // --- MCP Rust ---
    let rust = render_template_with_graph(
        "mcp-rust.tera",
        MINIMAL_MCP_ONTOLOGY,
        MCP_SPARQL,
        vec![
            ("server_name", "e2e_test_server"),
            ("server_version", "0.1.0"),
            ("server_description", "End-to-end test MCP server"),
            ("transport_type", "stdio"),
        ],
    );
    assert!(rust.contains("fn main"), "mcp-rust should contain fn main");
    assert!(rust.contains("use rmcp"), "mcp-rust should import rmcp");
    assert!(
        rust.contains("pub struct"),
        "mcp-rust should contain pub struct"
    );

    // --- MCP Go ---
    let go = render_template_with_graph(
        "mcp-go.tera",
        MINIMAL_MCP_ONTOLOGY,
        MCP_SPARQL,
        vec![
            ("server_name", "e2e_test_server"),
            ("server_version", "0.1.0"),
            ("server_description", "End-to-end test MCP server"),
            ("transport_type", "stdio"),
        ],
    );
    assert!(
        go.contains("func main()"),
        "mcp-go should contain func main()"
    );
    assert!(
        go.contains("package main"),
        "mcp-go should contain package main"
    );
    assert!(
        go.contains("type "),
        "mcp-go should contain type definitions"
    );

    // --- MCP TypeScript ---
    let ts = render_template_with_graph(
        "mcp-typescript.tera",
        MINIMAL_MCP_ONTOLOGY,
        MCP_SPARQL,
        vec![
            ("server_name", "e2e_test_server"),
            ("server_version", "0.1.0"),
            ("server_description", "End-to-end test MCP server"),
            ("transport_type", "stdio"),
        ],
    );
    assert!(
        ts.contains("type "),
        "mcp-typescript should contain type alias declarations"
    );
    assert!(
        ts.contains("async function"),
        "mcp-typescript should contain async function"
    );
    assert!(
        ts.contains("const "),
        "mcp-typescript should contain const declarations"
    );

    // --- MCP Java ---
    let java = render_template_with_graph(
        "mcp-java.tera",
        MINIMAL_MCP_ONTOLOGY,
        MCP_SPARQL,
        vec![
            ("server_name", "e2e_test_server"),
            ("server_version", "0.1.0"),
            ("server_description", "End-to-end test MCP server"),
            ("transport_type", "stdio"),
        ],
    );
    assert!(
        java.contains("public class"),
        "mcp-java should contain public class"
    );
    assert!(java.contains("record "), "mcp-java should contain record");
    assert!(
        java.contains("public static void main"),
        "mcp-java should contain main method"
    );

    // --- MCP Elixir ---
    let ex = render_template_with_graph(
        "mcp-elixir.tera",
        MINIMAL_MCP_ONTOLOGY,
        MCP_SPARQL,
        vec![
            ("server_name", "e2e_test_server"),
            ("server_version", "0.1.0"),
            ("server_description", "End-to-end test MCP server"),
            ("transport_type", "stdio"),
        ],
    );
    assert!(
        ex.contains("defmodule"),
        "mcp-elixir should contain defmodule"
    );
    assert!(
        ex.contains("use GenServer"),
        "mcp-elixir should use GenServer"
    );
    assert!(
        ex.contains("def start_link"),
        "mcp-elixir should contain start_link"
    );

    // --- A2A Rust ---
    let a2a_rust = render_template_with_graph(
        "a2a-rust.tera",
        MINIMAL_A2A_ONTOLOGY,
        A2A_SPARQL,
        vec![
            ("agent_name", "code_reviewer"),
            ("agent_version", "1.0.0"),
            ("agent_description", "Reviews code for quality issues"),
            ("agent_url", "http://localhost:8090"),
            ("provider_name", "ggen"),
            ("provider_url", "https://ggen.dev"),
        ],
    );
    assert!(
        a2a_rust.contains("fn main"),
        "a2a-rust should contain fn main"
    );
    assert!(
        a2a_rust.contains("pub struct"),
        "a2a-rust should contain pub struct"
    );
    assert!(
        a2a_rust.contains("Router::new"),
        "a2a-rust should use axum Router"
    );

    // --- A2A Go ---
    let a2a_go = render_template_with_graph(
        "a2a-go.tera",
        MINIMAL_A2A_ONTOLOGY,
        A2A_SPARQL,
        vec![
            ("agent_name", "code_reviewer"),
            ("agent_version", "1.0.0"),
            ("agent_description", "Reviews code for quality issues"),
            ("agent_url", "http://localhost:8090"),
            ("provider_name", "ggen"),
            ("provider_url", "https://ggen.dev"),
        ],
    );
    assert!(
        a2a_go.contains("func main()"),
        "a2a-go should contain func main()"
    );
    assert!(
        a2a_go.contains("package main"),
        "a2a-go should contain package main"
    );
    assert!(
        a2a_go.contains("agentCardHandler"),
        "a2a-go should contain agentCardHandler"
    );

    // --- A2A TypeScript ---
    let a2a_ts = render_template_with_graph(
        "a2a-typescript.tera",
        MINIMAL_A2A_ONTOLOGY,
        A2A_SPARQL,
        vec![
            ("agent_name", "code_reviewer"),
            ("agent_version", "1.0.0"),
            ("agent_description", "Reviews code for quality issues"),
            ("agent_url", "http://localhost:8090"),
            ("provider_name", "ggen"),
            ("provider_url", "https://ggen.dev"),
        ],
    );
    assert!(
        a2a_ts.contains("interface "),
        "a2a-typescript should contain interface"
    );
    assert!(
        a2a_ts.contains("async function"),
        "a2a-typescript should contain async function"
    );
    assert!(
        a2a_ts.contains("express()"),
        "a2a-typescript should use express"
    );

    // --- A2A Java ---
    let a2a_java = render_template_with_graph(
        "a2a-java.tera",
        MINIMAL_A2A_ONTOLOGY,
        A2A_SPARQL,
        vec![
            ("agent_name", "code_reviewer"),
            ("agent_version", "1.0.0"),
            ("agent_description", "Reviews code for quality issues"),
            ("agent_url", "http://localhost:8090"),
            ("provider_name", "ggen"),
            ("provider_url", "https://ggen.dev"),
        ],
    );
    assert!(
        a2a_java.contains("public class"),
        "a2a-java should contain public class"
    );
    assert!(
        a2a_java.contains("@RestController"),
        "a2a-java should contain @RestController"
    );
    assert!(
        a2a_java.contains("record "),
        "a2a-java should contain record"
    );

    // --- A2A Elixir (uses `skills` variable) ---
    let a2a_ex = render_a2a_elixir_with_skills();
    assert!(
        a2a_ex.contains("defmodule"),
        "a2a-elixir should contain defmodule"
    );
    assert!(
        a2a_ex.contains("use GenServer"),
        "a2a-elixir should use GenServer"
    );
    assert!(
        a2a_ex.contains("review_code"),
        "a2a-elixir should contain review_code skill"
    );
}

// ===========================================================================
// A2A Elixir helper (uses `skills` variable, not `sparql_results`)
// ===========================================================================

/// Render a2a-elixir.tera with a `skills` JSON array instead of `sparql_results`.
/// The a2a-elixir template iterates `{% for skill in skills %}`.
fn render_a2a_elixir_with_skills() -> String {
    let root = workspace_root();

    let mut tera = Tera::default();
    register_all(&mut tera);

    let template_path = root.join("templates/a2a-elixir.tera");
    let template_content = std::fs::read_to_string(&template_path)
        .unwrap_or_else(|e| panic!("Cannot read a2a-elixir.tera: {}", e));

    tera.add_raw_template("a2a_elixir", &template_content)
        .unwrap_or_else(|e| panic!("Template parse error for a2a-elixir.tera: {}", e));

    // Build skills array matching what the template expects
    let skills = serde_json::json!([
        {
            "name": "review_code",
            "description": "Reviews a code snippet for bugs",
            "streaming": "false",
            "timeout_ms": "5000",
            "retry_policy": "none"
        },
        {
            "name": "suggest_fixes",
            "description": "Suggests fixes for identified issues",
            "streaming": "true",
            "timeout_ms": "10000",
            "retry_policy": "once"
        }
    ]);

    let mut ctx = Context::new();
    ctx.insert("skills", &skills);
    ctx.insert("agent_name", "code_reviewer");
    ctx.insert("agent_version", "1.0.0");
    ctx.insert("agent_description", "Reviews code for quality issues");
    ctx.insert("agent_url", "http://localhost:8090");
    ctx.insert("provider_name", "ggen");
    ctx.insert("provider_url", "https://ggen.dev");

    tera.render("a2a_elixir", &ctx)
        .unwrap_or_else(|e| panic!("Render failed for a2a-elixir.tera: {}", e))
}
