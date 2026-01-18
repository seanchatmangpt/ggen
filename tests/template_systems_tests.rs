//! Comprehensive tests for Template Systems (50 tests)
//!
//! Tests cover:
//! - Template parsing (YAML frontmatter + Liquid body)
//! - Context creation and binding
//! - Variable substitution
//! - Error handling for missing variables
//! - Template inheritance and composition
//! - Frozen sections
//! - Format detection

use ggen_core::pipeline::Pipeline;
use ggen_core::template::Template;
use ggen_core::templates::frozen::FrozenMerger;
use ggen_utils::error::Result;
use tera::Context;

// =============================================================================
// TEMPLATE PARSING TESTS (15 tests)
// =============================================================================

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
fn test_template_parse_empty_frontmatter() {
    let template_str = r#"---
{}
---
// Body
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
fn test_template_parse_no_frontmatter() {
    let template_str = "// Just a body";

    let result = Template::parse(template_str);
    // Should fail without frontmatter
    assert!(result.is_err());
}

#[test]
fn test_template_parse_multiline_frontmatter() {
    let template_str = r#"---
to: "output.rs"
description: "Multi-line
  description
  here"
tags:
  - tag1
  - tag2
---
// Body
"#;

    let result = Template::parse(template_str);
    assert!(result.is_ok());
}

#[test]
fn test_template_parse_empty_body() {
    let template_str = r#"---
to: "output.rs"
---
"#;

    let result = Template::parse(template_str);
    assert!(result.is_ok());
}

#[test]
fn test_template_parse_with_rdf() {
    let template_str = r#"---
to: "output.rs"
rdf:
  - "data.ttl"
---
// Body with RDF
"#;

    let result = Template::parse(template_str);
    assert!(result.is_ok());
}

#[test]
fn test_template_parse_with_sparql() {
    let template_str = r#"---
to: "output.rs"
sparql:
  classes: "SELECT ?class WHERE { ?class a owl:Class }"
---
// Body
"#;

    let result = Template::parse(template_str);
    assert!(result.is_ok());
}

#[test]
fn test_template_parse_with_prefixes() {
    let template_str = r#"---
to: "output.rs"
prefixes:
  ex: "http://example.org/"
  rdf: "http://www.w3.org/1999/02/22-rdf-syntax-ns#"
---
// Body
"#;

    let result = Template::parse(template_str);
    assert!(result.is_ok());
}

#[test]
fn test_template_parse_complex() {
    let template_str = r#"---
to: "src/{{ module }}/{{ name }}.rs"
description: "Generated module"
rdf:
  - "schema.ttl"
prefixes:
  ex: "http://example.org/"
sparql:
  entities: "SELECT ?e WHERE { ?e a ex:Entity }"
---
{% for entity in entities %}
// Entity: {{ entity }}
{% endfor %}
"#;

    let result = Template::parse(template_str);
    assert!(result.is_ok());
}

#[test]
fn test_template_parse_liquid_syntax() {
    let template_str = r#"---
to: "output.rs"
---
{% if debug %}
// Debug mode
{% else %}
// Release mode
{% endif %}
"#;

    let result = Template::parse(template_str);
    assert!(result.is_ok());
}

#[test]
fn test_template_parse_liquid_loops() {
    let template_str = r#"---
to: "output.rs"
---
{% for i in range(end=5) %}
// Line {{ i }}
{% endfor %}
"#;

    let result = Template::parse(template_str);
    assert!(result.is_ok());
}

#[test]
fn test_template_parse_unicode() {
    let template_str = r#"---
to: "output.rs"
---
// 你好 🌍
// Привет мир
"#;

    let result = Template::parse(template_str);
    assert!(result.is_ok());
}

#[test]
fn test_template_parse_special_characters() {
    let template_str = r#"---
to: "output-file_2024.rs"
---
// Special chars: @#$%^&*()
"#;

    let result = Template::parse(template_str);
    assert!(result.is_ok());
}

// =============================================================================
// TEMPLATE RENDERING TESTS (15 tests)
// =============================================================================

#[test]
fn test_template_render_basic() -> Result<()> {
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
fn test_template_render_with_variable() -> Result<()> {
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
fn test_template_render_missing_variable() {
    let template_str = r#"---
to: "output.rs"
---
// Value: {{ missing }}
"#;

    let tmpl = Template::parse(template_str).unwrap();
    let mut pipeline = Pipeline::new().unwrap();
    let ctx = Context::new();

    let result = tmpl.render(pipeline.tera_mut(), &ctx);
    assert!(result.is_err());
}

#[test]
fn test_template_render_with_filter() -> Result<()> {
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
fn test_template_render_conditional() -> Result<()> {
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
fn test_template_render_loop() -> Result<()> {
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
fn test_template_render_multiple_variables() -> Result<()> {
    let template_str = r#"---
to: "output.rs"
---
// Name: {{ name }}
// Version: {{ version }}
// Author: {{ author }}
"#;

    let tmpl = Template::parse(template_str)?;
    let mut pipeline = Pipeline::new()?;
    let mut ctx = Context::new();
    ctx.insert("name", "App");
    ctx.insert("version", "1.0.0");
    ctx.insert("author", "Test");

    let result = tmpl.render(pipeline.tera_mut(), &ctx)?;
    assert!(result.contains("// Name: App"));
    assert!(result.contains("// Version: 1.0.0"));
    assert!(result.contains("// Author: Test"));

    Ok(())
}

#[test]
fn test_template_render_empty() -> Result<()> {
    let template_str = r#"---
to: "output.rs"
---
"#;

    let tmpl = Template::parse(template_str)?;
    let mut pipeline = Pipeline::new()?;
    let ctx = Context::new();

    let result = tmpl.render(pipeline.tera_mut(), &ctx)?;
    assert_eq!(result, "");

    Ok(())
}

#[test]
fn test_template_render_complex_expression() -> Result<()> {
    let template_str = r#"---
to: "output.rs"
---
// Length: {{ name | length }}
// Reversed: {{ name | reverse }}
"#;

    let tmpl = Template::parse(template_str)?;
    let mut pipeline = Pipeline::new()?;
    let mut ctx = Context::new();
    ctx.insert("name", "test");

    let result = tmpl.render(pipeline.tera_mut(), &ctx)?;
    assert!(result.contains("// Length: 4"));

    Ok(())
}

#[test]
fn test_template_render_nested_conditionals() -> Result<()> {
    let template_str = r#"---
to: "output.rs"
---
{% if outer %}
  {% if inner %}
    // Both true
  {% endif %}
{% endif %}
"#;

    let tmpl = Template::parse(template_str)?;
    let mut pipeline = Pipeline::new()?;
    let mut ctx = Context::new();
    ctx.insert("outer", &true);
    ctx.insert("inner", &true);

    let result = tmpl.render(pipeline.tera_mut(), &ctx)?;
    assert!(result.contains("// Both true"));

    Ok(())
}

#[test]
fn test_template_render_comments() -> Result<()> {
    let template_str = r#"---
to: "output.rs"
---
{# This is a comment #}
// Actual code
"#;

    let tmpl = Template::parse(template_str)?;
    let mut pipeline = Pipeline::new()?;
    let ctx = Context::new();

    let result = tmpl.render(pipeline.tera_mut(), &ctx)?;
    assert!(!result.contains("This is a comment"));
    assert!(result.contains("// Actual code"));

    Ok(())
}

#[test]
fn test_template_render_whitespace_control() -> Result<()> {
    let template_str = r#"---
to: "output.rs"
---
{%- for i in range(end=3) -%}
{{ i }}
{%- endfor -%}
"#;

    let tmpl = Template::parse(template_str)?;
    let mut pipeline = Pipeline::new()?;
    let ctx = Context::new();

    let result = tmpl.render(pipeline.tera_mut(), &ctx)?;
    assert!(result.contains("0"));
    assert!(result.contains("2"));

    Ok(())
}

#[test]
fn test_template_render_array_iteration() -> Result<()> {
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
fn test_template_render_macro() -> Result<()> {
    let template_str = r#"---
to: "output.rs"
---
{% macro greeting(name) %}
Hello {{ name }}!
{% endmacro greeting %}
{{ greeting(name="World") }}
"#;

    let tmpl = Template::parse(template_str)?;
    let mut pipeline = Pipeline::new()?;
    let ctx = Context::new();

    let result = tmpl.render(pipeline.tera_mut(), &ctx)?;
    assert!(result.contains("Hello World!"));

    Ok(())
}

#[test]
fn test_template_render_inheritance() -> Result<()> {
    // Note: Template inheritance typically requires multiple files
    // This is a simplified test
    let template_str = r#"---
to: "output.rs"
---
{% block content %}
Default content
{% endblock content %}
"#;

    let tmpl = Template::parse(template_str)?;
    let mut pipeline = Pipeline::new()?;
    let ctx = Context::new();

    let result = tmpl.render(pipeline.tera_mut(), &ctx)?;
    assert!(result.contains("Default content"));

    Ok(())
}

// =============================================================================
// FROZEN SECTION TESTS (10 tests)
// =============================================================================

#[test]
fn test_frozen_merger_has_frozen_sections_none() {
    let content = "// Regular content\n// No frozen sections";
    assert!(!FrozenMerger::has_frozen_sections(content));
}

#[test]
fn test_frozen_merger_has_frozen_sections_present() {
    let content = r#"
// Code
# frozen
// Protected code
# end frozen
// More code
"#;
    assert!(FrozenMerger::has_frozen_sections(content));
}

#[test]
fn test_frozen_merger_merge_no_frozen() -> Result<()> {
    let existing = "// Old content";
    let new_content = "// New content";

    let result = FrozenMerger::merge_with_frozen(existing, new_content)?;
    assert_eq!(result, new_content);

    Ok(())
}

#[test]
fn test_frozen_merger_merge_preserves_frozen() -> Result<()> {
    let existing = r#"// Header
# frozen
// Frozen content
# end frozen
// Footer"#;

    let new_content = r#"// New header
// New footer"#;

    let result = FrozenMerger::merge_with_frozen(existing, new_content)?;
    assert!(result.contains("// Frozen content"));

    Ok(())
}

#[test]
fn test_frozen_merger_multiple_sections() -> Result<()> {
    let existing = r#"
# frozen
// Section 1
# end frozen
// Middle
# frozen
// Section 2
# end frozen
"#;

    let new_content = "// All new";

    let result = FrozenMerger::merge_with_frozen(existing, new_content)?;
    assert!(result.contains("// Section 1"));
    assert!(result.contains("// Section 2"));

    Ok(())
}

#[test]
fn test_frozen_merger_empty_frozen_section() -> Result<()> {
    let existing = r#"
# frozen
# end frozen
"#;

    let new_content = "// New";

    let result = FrozenMerger::merge_with_frozen(existing, new_content)?;
    assert!(result.contains("// New"));

    Ok(())
}

#[test]
fn test_frozen_merger_unclosed_frozen() {
    let existing = r#"
# frozen
// Content without end
"#;

    let new_content = "// New";

    // Should handle gracefully
    let result = FrozenMerger::merge_with_frozen(existing, new_content);
    assert!(result.is_ok());
}

#[test]
fn test_frozen_merger_nested_markers() {
    // Nested frozen markers (shouldn't happen but test robustness)
    let content = r#"
# frozen
# frozen
// Content
# end frozen
# end frozen
"#;

    assert!(FrozenMerger::has_frozen_sections(content));
}

#[test]
fn test_frozen_merger_whitespace_handling() -> Result<()> {
    let existing = r#"
    # frozen
    // Indented content
    # end frozen
"#;

    let new_content = "// New";

    let result = FrozenMerger::merge_with_frozen(existing, new_content)?;
    assert!(result.contains("// Indented content"));

    Ok(())
}

#[test]
fn test_frozen_merger_line_endings() -> Result<()> {
    let existing = "# frozen\r\n// Windows line endings\r\n# end frozen";
    let new_content = "// New content";

    let result = FrozenMerger::merge_with_frozen(existing, new_content)?;
    assert!(result.contains("// Windows line endings"));

    Ok(())
}

// =============================================================================
// CONTEXT TESTS (10 tests)
// =============================================================================

#[test]
fn test_context_creation() {
    let ctx = Context::new();
    assert!(ctx.into_json().is_object());
}

#[test]
fn test_context_insert_string() {
    let mut ctx = Context::new();
    ctx.insert("name", "test");

    assert!(ctx.get("name").is_some());
}

#[test]
fn test_context_insert_number() {
    let mut ctx = Context::new();
    ctx.insert("count", &42);

    assert!(ctx.get("count").is_some());
}

#[test]
fn test_context_insert_boolean() {
    let mut ctx = Context::new();
    ctx.insert("active", &true);

    assert!(ctx.get("active").is_some());
}

#[test]
fn test_context_insert_array() {
    let mut ctx = Context::new();
    ctx.insert("items", &vec!["a", "b", "c"]);

    assert!(ctx.get("items").is_some());
}

#[test]
fn test_context_from_serialize() {
    use std::collections::HashMap;

    let mut map = HashMap::new();
    map.insert("key", "value");

    let result = Context::from_serialize(&map);
    assert!(result.is_ok());
}

#[test]
fn test_context_multiple_inserts() {
    let mut ctx = Context::new();
    ctx.insert("a", "1");
    ctx.insert("b", "2");
    ctx.insert("c", "3");

    assert!(ctx.get("a").is_some());
    assert!(ctx.get("c").is_some());
}

#[test]
fn test_context_overwrite() {
    let mut ctx = Context::new();
    ctx.insert("key", "old");
    ctx.insert("key", "new");

    // Later insert should overwrite
    let json = ctx.into_json();
    assert_eq!(json["key"], "new");
}

#[test]
fn test_context_nested_structure() {
    use serde_json::json;

    let mut ctx = Context::new();
    ctx.insert(
        "config",
        &json!({
            "name": "app",
            "version": "1.0.0"
        }),
    );

    assert!(ctx.get("config").is_some());
}

#[test]
fn test_context_empty() {
    let ctx = Context::new();
    let json = ctx.into_json();

    assert!(json.is_object());
    assert_eq!(json.as_object().unwrap().len(), 0);
}
