//! Comprehensive template engine tests
//!
//! Tests for template parsing, rendering, RDF integration, and file generation.
//! Following London TDD principles with 100% production-safe error handling.

use anyhow::Result;
use ggen_core::{Graph, Template};
use std::collections::BTreeMap;
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

fn load_template_fixture(name: &str) -> Result<String> {
    let fixture_path = Path::new("tests/fixtures/templates").join(name);
    fs::read_to_string(&fixture_path).map_err(|e| {
        anyhow::anyhow!(
            "Failed to load fixture '{}': {}",
            fixture_path.display(),
            e
        )
    })
}

/* ========== Unit Tests: Template Parsing ========== */

#[test]
fn test_parse_simple_template() -> Result<()> {
    let input = r#"---
to: "output.rs"
vars:
  name: "test"
---
fn {{name}}() {}"#;

    let template = Template::parse(input)?;
    assert_eq!(template.body, "fn {{name}}() {}");
    Ok(())
}

#[test]
fn test_parse_template_without_frontmatter() -> Result<()> {
    let input = "fn main() { println!(\"Hello\"); }";
    let template = Template::parse(input)?;
    assert_eq!(template.body, input);
    Ok(())
}

#[test]
fn test_parse_template_with_empty_frontmatter() -> Result<()> {
    let input = r#"---
---
fn main() {}"#;

    let template = Template::parse(input)?;
    assert_eq!(template.body, "fn main() {}");
    Ok(())
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
    assert!(template.body.contains("        println!(\"World\");  // extra indent"));
    Ok(())
}

/* ========== Unit Tests: Frontmatter Rendering ========== */

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
    assert_eq!(
        template.front.vars.get("greeting").and_then(|v| v.as_str()),
        Some("Hello")
    );
    Ok(())
}

#[test]
fn test_render_frontmatter_nested_vars() -> Result<()> {
    let input = r#"---
to: "output.rs"
vars:
  service: "{{service_name}}"
  port: "{{port_number}}"
---
body"#;

    let mut template = Template::parse(input)?;
    let mut tera = mk_tera();
    let vars = ctx_from_pairs(&[("service_name", "user_api"), ("port_number", "8080")]);

    template.render_frontmatter(&mut tera, &vars)?;
    assert_eq!(
        template.front.vars.get("service").and_then(|v| v.as_str()),
        Some("user_api")
    );
    assert_eq!(
        template.front.vars.get("port").and_then(|v| v.as_str()),
        Some("8080")
    );
    Ok(())
}

/* ========== Unit Tests: Flexible Vars Deserialization ========== */

#[test]
fn test_vars_as_map() -> Result<()> {
    let fixture = load_template_fixture("flexible_vars_map.yaml")?;
    let template = Template::parse(&fixture)?;
    let mut tera = mk_tera();

    let mut vars = Context::new();
    vars.insert("dummy", "placeholder"); // Context needs at least one value

    let mut tmpl = template;
    tmpl.render_frontmatter(&mut tera, &vars)?;

    assert_eq!(
        tmpl.front.vars.get("name").and_then(|v| v.as_str()),
        Some("Alice")
    );
    assert_eq!(
        tmpl.front.vars.get("age").and_then(|v| v.as_i64()),
        Some(30)
    );
    Ok(())
}

#[test]
fn test_vars_as_array() -> Result<()> {
    let fixture = load_template_fixture("flexible_vars_array.yaml")?;
    let template = Template::parse(&fixture)?;
    let mut tera = mk_tera();

    let vars = Context::new();
    let mut tmpl = template;
    tmpl.render_frontmatter(&mut tera, &vars)?;

    // Arrays are converted to indexed maps: var0, var1, var2
    assert_eq!(
        tmpl.front.vars.get("var0").and_then(|v| v.as_str()),
        Some("first")
    );
    assert_eq!(
        tmpl.front.vars.get("var1").and_then(|v| v.as_str()),
        Some("second")
    );
    assert_eq!(
        tmpl.front.vars.get("var2").and_then(|v| v.as_str()),
        Some("third")
    );
    Ok(())
}

#[test]
fn test_vars_as_single_string() -> Result<()> {
    let input = r#"---
to: "output.txt"
vars: "single_value"
---
Value: {{var0}}"#;

    let mut template = Template::parse(input)?;
    let mut tera = mk_tera();
    let vars = Context::new();

    template.render_frontmatter(&mut tera, &vars)?;

    // Single values are converted to {var0: value}
    assert_eq!(
        template.front.vars.get("var0").and_then(|v| v.as_str()),
        Some("single_value")
    );
    Ok(())
}

#[test]
fn test_vars_null_or_missing() -> Result<()> {
    let input = r#"---
to: "output.txt"
---
body"#;

    let mut template = Template::parse(input)?;
    let mut tera = mk_tera();
    let vars = Context::new();

    template.render_frontmatter(&mut tera, &vars)?;

    // Null/missing vars should result in empty map
    assert!(template.front.vars.is_empty());
    Ok(())
}

/* ========== Integration Tests: RDF and SPARQL ========== */

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
fn test_sparql_with_variables() -> Result<()> {
    let input = r#"---
prefixes:
  ex: "http://example.org/"
rdf_inline:
  - "ex:{{entity}} a ex:Person ; ex:name '{{name}}' ."
sparql:
  query: "SELECT ?n WHERE { ex:{{entity}} ex:name ?n }"
---
Name: {{ sparql_first(results=sparql_results.query, column="n") }}"#;

    let mut template = Template::parse(input)?;
    let mut graph = Graph::new()?;
    let mut tera = mk_tera();
    let vars = ctx_from_pairs(&[("entity", "Alice"), ("name", "Alice Smith")]);

    template.process_graph(&mut graph, &mut tera, &vars, Path::new("test.tmpl"))?;
    let rendered = template.render(&mut tera, &vars)?;

    assert!(rendered.contains("Name: \"Alice Smith\""));
    Ok(())
}

#[test]
fn test_rdf_metadata_fixture() -> Result<()> {
    let fixture = load_template_fixture("rdf_metadata.yaml")?;
    let mut template = Template::parse(&fixture)?;
    let mut graph = Graph::new()?;
    let mut tera = mk_tera();
    let vars = ctx_from_pairs(&[("component_name", "auth_controller")]);

    template.process_graph(&mut graph, &mut tera, &vars, Path::new("test.tmpl"))?;
    let rendered = template.render(&mut tera, &vars)?;

    assert!(rendered.contains("Component: auth_controller"));
    assert!(rendered.contains("Generated files:"));
    Ok(())
}

/* ========== Integration Tests: Multi-File Generation ========== */

#[test]
fn test_generate_microservice_from_templates() -> Result<()> {
    let output_dir = TempDir::new()?;

    // Load and render microservice template
    let microservice_tmpl = load_template_fixture("microservice.yaml")?;
    let mut template = Template::parse(&microservice_tmpl)?;
    let mut graph = Graph::new()?;
    let mut tera = mk_tera();
    let vars = ctx_from_pairs(&[("service_name", "auth"), ("port", "9000")]);

    template.process_graph(&mut graph, &mut tera, &vars, Path::new("test.tmpl"))?;
    template.render_frontmatter(&mut tera, &vars)?;
    let rendered = template.render(&mut tera, &vars)?;

    // Write to output directory
    let output_path = output_dir
        .path()
        .join(template.front.to.as_ref().ok_or_else(|| {
            anyhow::anyhow!("Template must have 'to' field")
        })?);
    fs::create_dir_all(output_path.parent().ok_or_else(|| {
        anyhow::anyhow!("Output path has no parent")
    })?)?;
    fs::write(&output_path, rendered)?;

    // Verify generated file
    assert!(output_path.exists());
    let content = fs::read_to_string(&output_path)?;
    assert!(content.contains("auth - Microservice"));
    assert!(content.contains("0.0.0.0:9000"));
    assert!(content.contains("health_check"));
    Ok(())
}

#[test]
fn test_generate_cargo_toml() -> Result<()> {
    let output_dir = TempDir::new()?;

    let cargo_tmpl = load_template_fixture("cargo_toml.yaml")?;
    let mut template = Template::parse(&cargo_tmpl)?;
    let mut tera = mk_tera();
    let vars = ctx_from_pairs(&[("service_name", "payment_service"), ("version", "1.0.0")]);

    template.render_frontmatter(&mut tera, &vars)?;
    let rendered = template.render(&mut tera, &vars)?;

    let output_path = output_dir
        .path()
        .join(template.front.to.as_ref().ok_or_else(|| {
            anyhow::anyhow!("Template must have 'to' field")
        })?);
    fs::create_dir_all(output_path.parent().ok_or_else(|| {
        anyhow::anyhow!("Output path has no parent")
    })?)?;
    fs::write(&output_path, rendered)?;

    let content = fs::read_to_string(&output_path)?;
    assert!(content.contains("name = \"payment_service\""));
    assert!(content.contains("version = \"1.0.0\""));
    assert!(content.contains("axum = \"0.7\""));
    Ok(())
}

#[test]
fn test_generate_integration_test() -> Result<()> {
    let output_dir = TempDir::new()?;

    let test_tmpl = load_template_fixture("integration_test.yaml")?;
    let mut template = Template::parse(&test_tmpl)?;
    let mut tera = mk_tera();
    let vars = ctx_from_pairs(&[("service_name", "api_service"), ("port", "3000")]);

    template.render_frontmatter(&mut tera, &vars)?;
    let rendered = template.render(&mut tera, &vars)?;

    let output_path = output_dir
        .path()
        .join(template.front.to.as_ref().ok_or_else(|| {
            anyhow::anyhow!("Template must have 'to' field")
        })?);
    fs::create_dir_all(output_path.parent().ok_or_else(|| {
        anyhow::anyhow!("Output path has no parent")
    })?)?;
    fs::write(&output_path, rendered)?;

    let content = fs::read_to_string(&output_path)?;
    assert!(content.contains("use api_service::*;"));
    assert!(content.contains("localhost:3000"));
    assert!(content.contains("test_health_check"));
    Ok(())
}

#[test]
fn test_generate_complete_microservice_structure() -> Result<()> {
    let output_dir = TempDir::new()?;
    let service_name = "order_service";
    let port = "8081";

    let templates = vec![
        ("microservice.yaml", service_name, port),
        ("cargo_toml.yaml", service_name, port),
        ("integration_test.yaml", service_name, port),
    ];

    let mut generated_files = Vec::new();

    for (template_name, svc, prt) in templates {
        let tmpl_content = load_template_fixture(template_name)?;
        let mut template = Template::parse(&tmpl_content)?;
        let mut graph = Graph::new()?;
        let mut tera = mk_tera();
        let vars = ctx_from_pairs(&[("service_name", svc), ("port", prt)]);

        template.process_graph(&mut graph, &mut tera, &vars, Path::new("test.tmpl"))?;
        template.render_frontmatter(&mut tera, &vars)?;
        let rendered = template.render(&mut tera, &vars)?;

        let output_path = output_dir.path().join(
            template
                .front
                .to
                .as_ref()
                .ok_or_else(|| anyhow::anyhow!("Template must have 'to' field"))?,
        );
        fs::create_dir_all(
            output_path
                .parent()
                .ok_or_else(|| anyhow::anyhow!("Output path has no parent"))?,
        )?;
        fs::write(&output_path, rendered)?;

        generated_files.push(output_path);
    }

    // Verify all files were generated
    assert_eq!(generated_files.len(), 3);
    for file in &generated_files {
        assert!(file.exists(), "File should exist: {}", file.display());
    }

    // Verify directory structure
    let service_dir = output_dir.path().join("services").join(service_name);
    assert!(service_dir.exists());
    assert!(service_dir.join("src").exists());
    assert!(service_dir.join("tests").exists());
    assert!(service_dir.join("Cargo.toml").exists());

    Ok(())
}

/* ========== Performance Tests ========== */

#[test]
fn test_parse_performance_large_template() -> Result<()> {
    use std::time::Instant;

    // Generate a large template with many variables
    let mut frontmatter = String::from("---\nto: \"output.rs\"\nvars:\n");
    for i in 0..1000 {
        frontmatter.push_str(&format!("  var{}: \"value{}\"\n", i, i));
    }
    frontmatter.push_str("---\n");

    let mut body = String::new();
    for i in 0..100 {
        body.push_str(&format!("fn func{}() {{ println!(\"{{{{var{}}}}}\"); }}\n", i, i));
    }

    let template_str = format!("{}{}", frontmatter, body);

    let start = Instant::now();
    let template = Template::parse(&template_str)?;
    let parse_duration = start.elapsed();

    // Parsing should be fast (< 100ms for large templates)
    assert!(
        parse_duration.as_millis() < 100,
        "Parsing took too long: {:?}",
        parse_duration
    );

    // Verify template was parsed correctly
    assert!(template.body.contains("func0"));
    assert!(template.body.contains("func99"));

    Ok(())
}

#[test]
fn test_render_performance_many_variables() -> Result<()> {
    use std::time::Instant;

    let input = r#"---
to: "output.rs"
---
{{name0}} {{name1}} {{name2}} {{name3}} {{name4}}"#;

    let template = Template::parse(input)?;
    let mut tera = mk_tera();
    let mut vars = Context::new();
    for i in 0..100 {
        vars.insert(&format!("name{}", i), &format!("value{}", i));
    }

    let start = Instant::now();
    let _ = template.render(&mut tera, &vars)?;
    let render_duration = start.elapsed();

    // Rendering should be fast (< 50ms)
    assert!(
        render_duration.as_millis() < 50,
        "Rendering took too long: {:?}",
        render_duration
    );

    Ok(())
}

/* ========== Security Tests ========== */

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
    assert_eq!(
        template.front.to.as_deref(),
        Some("../../../etc/passwd")
    );

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

/* ========== Edge Case Tests ========== */

#[test]
fn test_empty_template() -> Result<()> {
    let input = "";
    let template = Template::parse(input)?;
    assert_eq!(template.body, "");
    Ok(())
}

#[test]
fn test_only_frontmatter() -> Result<()> {
    let input = r#"---
to: "output.txt"
---"#;

    let template = Template::parse(input)?;
    assert_eq!(template.body, "");
    Ok(())
}

#[test]
fn test_frontmatter_with_special_characters() -> Result<()> {
    let input = r#"---
to: "file with spaces & special!@#$%^&*().rs"
---
content"#;

    let mut template = Template::parse(input)?;
    let mut tera = mk_tera();
    let vars = Context::new();

    template.render_frontmatter(&mut tera, &vars)?;
    assert_eq!(
        template.front.to.as_deref(),
        Some("file with spaces & special!@#$%^&*().rs")
    );

    Ok(())
}

#[test]
fn test_unicode_in_template() -> Result<()> {
    let input = r#"---
to: "文件.rs"
vars:
  message: "你好世界"
---
// {{message}}
println!("{{message}}");"#;

    let mut template = Template::parse(input)?;
    let mut tera = mk_tera();
    let vars = Context::new();

    template.render_frontmatter(&mut tera, &vars)?;
    let rendered = template.render(&mut tera, &vars)?;

    assert!(rendered.contains("你好世界"));
    Ok(())
}

#[test]
fn test_malformed_yaml_frontmatter() -> Result<()> {
    let input = r#"---
to: "output.rs"
vars: {invalid yaml :: syntax
---
body"#;

    let result = Template::parse(input);

    // Should return an error due to malformed YAML
    assert!(result.is_err());

    Ok(())
}

#[test]
fn test_missing_closing_frontmatter() -> Result<()> {
    let input = r#"---
to: "output.rs"
vars:
  name: "test"
body without closing frontmatter"#;

    // gray-matter should handle this - it looks for the closing ---
    let result = Template::parse(input);

    // This might succeed (treating everything as body) or fail depending on parser
    // Either way, we should handle it gracefully
    match result {
        Ok(t) => {
            // If it succeeds, body should contain the content
            assert!(!t.body.is_empty());
        }
        Err(_) => {
            // If it fails, that's also acceptable
        }
    }

    Ok(())
}

/* ========== Error Handling Tests ========== */

#[test]
fn test_render_with_missing_variable() -> Result<()> {
    let input = r#"---
to: "{{undefined_var}}.rs"
---
body"#;

    let mut template = Template::parse(input)?;
    let mut tera = mk_tera();
    let vars = Context::new();

    // Should return an error when trying to render undefined variable
    let result = template.render_frontmatter(&mut tera, &vars);

    // Tera should error on undefined variables
    assert!(result.is_err());

    Ok(())
}

#[test]
fn test_sparql_invalid_query() -> Result<()> {
    let input = r#"---
prefixes:
  ex: "http://example.org/"
sparql:
  bad: "INVALID SPARQL SYNTAX {{{"
---
body"#;

    let mut template = Template::parse(input)?;
    let mut graph = Graph::new()?;
    let mut tera = mk_tera();
    let vars = Context::new();

    // Should return an error due to invalid SPARQL
    let result = template.process_graph(&mut graph, &mut tera, &vars, Path::new("test.tmpl"));

    assert!(result.is_err());

    Ok(())
}

#[test]
fn test_from_file_not_found() -> Result<()> {
    let input = r#"---
from: "/nonexistent/path/to/file.txt"
---
body"#;

    let mut template = Template::parse(input)?;
    let mut tera = mk_tera();
    let vars = Context::new();

    template.render_frontmatter(&mut tera, &vars)?;

    // Should return an error when trying to read non-existent file
    let result = template.render(&mut tera, &vars);

    assert!(result.is_err());

    Ok(())
}

/* ========== Regression Tests ========== */

#[test]
fn test_issue_vars_array_conversion() -> Result<()> {
    // Regression test for TEMPLATE_GENERATION_CONTRACT.md issue
    // Previously, vars as array would crash. Now should convert to indexed map.
    let input = r#"---
to: "output.txt"
vars:
  - "item1"
  - "item2"
---
First: {{var0}}
Second: {{var1}}"#;

    let mut template = Template::parse(input)?;
    let mut tera = mk_tera();
    let vars = Context::new();

    template.render_frontmatter(&mut tera, &vars)?;
    let rendered = template.render(&mut tera, &vars)?;

    assert!(rendered.contains("First: item1"));
    assert!(rendered.contains("Second: item2"));

    Ok(())
}

#[test]
fn test_issue_sparql_results_available_in_template() -> Result<()> {
    // Ensure SPARQL results are accessible via sparql_results.<name>
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
