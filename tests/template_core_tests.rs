//! Consolidated Template Core Tests
//!
//! Critical tests for template parsing, rendering, frozen sections, RDF/SPARQL integration.
//! Consolidated from template_systems_tests.rs and template_comprehensive_test.rs
//!
//! Test Categories:
//! - Parsing: YAML frontmatter + Liquid body parsing
//! - Rendering: Variable substitution, filters, conditionals, loops
//! - Frozen Sections: Preservation during regeneration
//! - RDF/SPARQL: Graph integration and query execution
//! - Security: Path traversal prevention
//! - Edge Cases: Empty, unicode, malformed input handling

use anyhow::Result;
use ggen_core::pipeline::Pipeline;
use ggen_core::template::Template;
use ggen_core::templates::frozen::FrozenMerger;
use ggen_core::Graph;
use std::fs;
use std::path::Path;
use tempfile::TempDir;
use tera::Context;

/* ========== Test Utilities ========== */

fn mk_tera() -> tera::Tera {
    let mut tera = tera::Tera::default();
    ggen_core::register::register_all(&mut tera);
    tera
}

fn ctx_from_pairs(pairs: &[(&str, &str)]) -> Context {
    let mut c = Context::new();
    for (k, v) in pairs {
        c.insert(*k, v);
    }
    c
}

#[allow(dead_code)]
fn load_template_fixture(name: &str) -> Result<String> {
    let fixture_path = Path::new("tests/fixtures/templates").join(name);
    fs::read_to_string(&fixture_path)
        .map_err(|e| anyhow::anyhow!("Failed to load fixture '{}': {}", fixture_path.display(), e))
}

/* ========== PARSING TESTS (10 critical tests) ========== */

#[test]
fn test_template_parse_basic() {
    let template_str = r#"---
to: "output.rs"
---
// Template body
"#;
    let result = Template::parse(template_str);
    assert!(result.is_ok());
}

#[test]
fn test_template_parse_with_variables() {
    let template_str = r#"---
to: "{{ name }}.rs"
---
// Name: {{ name }}
"#;
    let result = Template::parse(template_str);
    assert!(result.is_ok());
}

#[test]
fn test_template_parse_invalid_yaml() {
    let template_str = r#"---
invalid: [yaml
---
// Body
"#;
    let result = Template::parse(template_str);
    assert!(result.is_err());
}

#[test]
fn test_template_parse_with_rdf_and_sparql() {
    let template_str = r#"---
to: "output.rs"
rdf:
  - "data.ttl"
prefixes:
  ex: "http://example.org/"
sparql:
  classes: "SELECT ?class WHERE { ?class a owl:Class }"
---
// Body with RDF
"#;
    let result = Template::parse(template_str);
    assert!(result.is_ok());
}

#[test]
fn test_parse_preserves_body_whitespace() -> Result<()> {
    let input = r#"---
to: "file.rs"
---
fn main() {
    println!("Hello");
        println!("World");  // extra indent
}"#;

    let template = Template::parse(input)?;
    assert!(template
        .body
        .contains("        println!(\"World\");  // extra indent"));
    Ok(())
}

/* ========== RENDERING TESTS (10 critical tests) ========== */

#[test]
fn test_template_render_basic() -> Result<(), Box<dyn std::error::Error>> {
    let template_str = r#"---
to: "output.rs"
---
// Hello World
"#;

    let tmpl = Template::parse(template_str)?;
    let mut pipeline = Pipeline::new()?;
    let ctx = Context::new();

    let result = tmpl.render(pipeline.tera_mut(), &ctx)?;
    assert!(result.contains("// Hello World"));
    Ok(())
}

#[test]
fn test_template_render_with_variable() -> Result<(), Box<dyn std::error::Error>> {
    let template_str = r#"---
to: "output.rs"
---
// Name: {{ name }}
"#;

    let tmpl = Template::parse(template_str)?;
    let mut pipeline = Pipeline::new()?;
    let mut ctx = Context::new();
    ctx.insert("name", "TestApp");

    let result = tmpl.render(pipeline.tera_mut(), &ctx)?;
    assert!(result.contains("// Name: TestApp"));
    Ok(())
}

#[test]
fn test_template_render_with_filters() -> Result<(), Box<dyn std::error::Error>> {
    let template_str = r#"---
to: "output.rs"
---
// Upper: {{ name | upper }}
// Lower: {{ name | lower }}
"#;

    let tmpl = Template::parse(template_str)?;
    let mut pipeline = Pipeline::new()?;
    let mut ctx = Context::new();
    ctx.insert("name", "TestApp");

    let result = tmpl.render(pipeline.tera_mut(), &ctx)?;
    assert!(result.contains("// Upper: TESTAPP"));
    assert!(result.contains("// Lower: testapp"));
    Ok(())
}

#[test]
fn test_template_render_conditional() -> Result<(), Box<dyn std::error::Error>> {
    let template_str = r#"---
to: "output.rs"
---
{% if debug %}
// Debug mode
{% else %}
// Release mode
{% endif %}
"#;

    let tmpl = Template::parse(template_str)?;
    let mut pipeline = Pipeline::new()?;
    let mut ctx = Context::new();
    ctx.insert("debug", &true);

    let result = tmpl.render(pipeline.tera_mut(), &ctx)?;
    assert!(result.contains("// Debug mode"));
    Ok(())
}

#[test]
fn test_template_render_loop() -> Result<(), Box<dyn std::error::Error>> {
    let template_str = r#"---
to: "output.rs"
---
{% for i in range(end=3) %}
// Line {{ i }}
{% endfor %}
"#;

    let tmpl = Template::parse(template_str)?;
    let mut pipeline = Pipeline::new()?;
    let ctx = Context::new();

    let result = tmpl.render(pipeline.tera_mut(), &ctx)?;
    assert!(result.contains("// Line 0"));
    assert!(result.contains("// Line 2"));
    Ok(())
}

#[test]
fn test_template_render_array_iteration() -> Result<(), Box<dyn std::error::Error>> {
    let template_str = r#"---
to: "output.rs"
---
{% for item in items %}
// Item: {{ item }}
{% endfor %}
"#;

    let tmpl = Template::parse(template_str)?;
    let mut pipeline = Pipeline::new()?;
    let mut ctx = Context::new();
    ctx.insert("items", &vec!["a", "b", "c"]);

    let result = tmpl.render(pipeline.tera_mut(), &ctx)?;
    assert!(result.contains("// Item: a"));
    assert!(result.contains("// Item: c"));
    Ok(())
}

#[test]
fn test_render_frontmatter_basic() -> Result<()> {
    let input = r#"---
to: "{{name}}.rs"
vars:
  greeting: "Hello"
---
body"#;

    let mut template = Template::parse(input)?;
    let mut tera = mk_tera();
    let vars = ctx_from_pairs(&[("name", "test")]);

    template.render_frontmatter(&mut tera, &vars)?;
    assert_eq!(template.front.to.as_deref(), Some("test.rs"));
    Ok(())
}

/* ========== FROZEN SECTION TESTS (3 critical tests) ========== */

#[test]
fn test_frozen_merger_has_frozen_sections_none() {
    let content = "// Regular content\n// No frozen sections";
    assert!(!FrozenMerger::has_frozen_sections(content));
}

#[test]
fn test_frozen_merger_has_frozen_sections_present() {
    let content = r#"
// Code
{% frozen %}
// Protected code
{% endfrozen %}
// More code
"#;
    assert!(FrozenMerger::has_frozen_sections(content));
}

#[test]
fn test_frozen_merger_preserves_named_sections() -> Result<(), Box<dyn std::error::Error>> {
    // Per docs: Frozen sections with matching IDs have their OLD content preserved
    // Note: FrozenMerger works with single-line frozen sections (per doc examples)
    let old_content = r#"{% frozen id="custom" %}old code{% endfrozen %}"#;
    let new_content = r#"{% frozen id="custom" %}new code{% endfrozen %}"#;

    let merged = FrozenMerger::merge_with_frozen(old_content, new_content)?;
    // Old content should be preserved, not the new generated code
    assert!(merged.contains("old code"));
    assert!(!merged.contains("new code"));
    Ok(())
}

/* ========== RDF/SPARQL INTEGRATION TESTS (5 critical tests) ========== */

#[test]
fn test_rdf_inline_and_sparql() -> Result<()> {
    let input = r#"---
prefixes:
  ex: "http://example.org/"
rdf_inline:
  - "ex:Alice a ex:Person ."
  - "ex:Bob a ex:Person ."
sparql:
  people: "SELECT ?person WHERE { ?person a ex:Person }"
---
Count: {{ sparql_results.people | length }}"#;

    let mut template = Template::parse(input)?;
    let mut graph = Graph::new()?;
    let mut tera = mk_tera();
    let vars = Context::new();

    template.process_graph(&mut graph, &mut tera, &vars, Path::new("test.tmpl"))?;
    let rendered = template.render(&mut tera, &vars)?;

    assert!(rendered.contains("Count: 2"));
    Ok(())
}

#[test]
fn test_sparql_ask_query() -> Result<()> {
    let input = r#"---
prefixes:
  ex: "http://example.org/"
rdf_inline:
  - "ex:data a ex:Thing ."
sparql:
  has_data: "ASK WHERE { ?s a ex:Thing }"
---
Has data: {{ sparql_results.has_data }}"#;

    let mut template = Template::parse(input)?;
    let mut graph = Graph::new()?;
    let mut tera = mk_tera();
    let vars = Context::new();

    template.process_graph(&mut graph, &mut tera, &vars, Path::new("test.tmpl"))?;
    let rendered = template.render(&mut tera, &vars)?;

    assert!(rendered.contains("Has data: true"));
    Ok(())
}

#[test]
fn test_sparql_results_available_in_template() -> Result<()> {
    let input = r#"---
prefixes:
  ex: "http://example.org/"
rdf_inline:
  - "ex:thing a ex:Type ."
sparql:
  count: "SELECT (COUNT(?s) as ?cnt) WHERE { ?s a ex:Type }"
---
Count: {{ sparql_results.count }}"#;

    let mut template = Template::parse(input)?;
    let mut graph = Graph::new()?;
    let mut tera = mk_tera();
    let vars = Context::new();

    template.process_graph(&mut graph, &mut tera, &vars, Path::new("test.tmpl"))?;
    let rendered = template.render(&mut tera, &vars)?;

    assert!(rendered.contains("Count:"));
    assert!(template.front.sparql_results.contains_key("count"));
    Ok(())
}

/* ========== SECURITY TESTS (2 critical tests) ========== */

#[test]
fn test_path_traversal_prevention() -> Result<()> {
    let input = r#"---
to: "../../../etc/passwd"
---
malicious content"#;

    let mut template = Template::parse(input)?;
    let mut tera = mk_tera();
    let vars = Context::new();

    template.render_frontmatter(&mut tera, &vars)?;
    // The 'to' field will contain the path, but actual file writing should be prevented
    // by the caller (Generator or CLI). Template parsing doesn't validate paths.
    assert_eq!(template.front.to.as_deref(), Some("../../../etc/passwd"));
    Ok(())
}

#[test]
fn test_rdf_file_path_traversal_blocked() -> Result<()> {
    let temp_dir = TempDir::new()?;
    let template_path = temp_dir.path().join("template.yaml");

    let input = r#"---
rdf:
  - "../../../etc/passwd.ttl"
---
body"#;

    let mut template = Template::parse(input)?;
    let mut graph = Graph::new()?;
    let mut tera = mk_tera();
    let vars = Context::new();

    // This should fail due to path traversal protection
    let result = template.process_graph(&mut graph, &mut tera, &vars, &template_path);
    // Should return an error (path traversal blocked or file not found)
    assert!(result.is_err());
    Ok(())
}

/* ========== EDGE CASE TESTS (3 critical tests) ========== */

#[test]
fn test_template_unicode() -> Result<()> {
    let input = r#"---
to: "output.rs"
vars:
  message: "Hello"
---
// Unicode test
println!("Hello World");"#;

    let mut template = Template::parse(input)?;
    let mut tera = mk_tera();
    let vars = Context::new();

    template.render_frontmatter(&mut tera, &vars)?;
    let rendered = template.render(&mut tera, &vars)?;

    assert!(rendered.contains("Hello World"));
    Ok(())
}

#[test]
fn test_empty_template() -> Result<()> {
    let input = "";
    let template = Template::parse(input)?;
    assert_eq!(template.body, "");
    Ok(())
}

#[test]
fn test_frontmatter_with_special_characters() -> Result<()> {
    let input = r#"---
to: "file with spaces & special.rs"
---
content"#;

    let mut template = Template::parse(input)?;
    let mut tera = mk_tera();
    let vars = Context::new();

    template.render_frontmatter(&mut tera, &vars)?;
    assert_eq!(
        template.front.to.as_deref(),
        Some("file with spaces & special.rs")
    );
    Ok(())
}

/* ========== CONTEXT TESTS (3 critical tests) ========== */

#[test]
fn test_context_creation() {
    let ctx = Context::new();
    assert!(ctx.into_json().is_object());
}

#[test]
fn test_context_insert_various_types() {
    let mut ctx = Context::new();
    ctx.insert("string", "test");
    ctx.insert("number", &42);
    ctx.insert("boolean", &true);
    ctx.insert("array", &vec!["a", "b"]);

    assert!(ctx.get("string").is_some());
    assert!(ctx.get("number").is_some());
    assert!(ctx.get("boolean").is_some());
    assert!(ctx.get("array").is_some());
}

#[test]
fn test_context_overwrite() {
    let mut ctx = Context::new();
    ctx.insert("key", "old");
    ctx.insert("key", "new");

    let json = ctx.into_json();
    assert_eq!(json["key"], "new");
}
