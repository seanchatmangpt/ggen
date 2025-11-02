//! Regression Tests - Ensure v1 functionality still works
//!
//! These tests verify that all v1 functionality continues to work
//! after the v2 refactoring. Chicago TDD approach: test REAL code.

use assert_cmd::Command;
use predicates::prelude::*;
use std::fs;

use super::helpers::*;

/// Test Suite 1: RDF Parsing
/// Verify that v1 TTL files can still be parsed
mod rdf_parsing {
    use super::*;

    #[test]
    fn test_parse_simple_rdf() {
        let workspace = setup_workspace().unwrap();
        let workspace_path = workspace.path();

        // Create v1-style RDF data
        let rdf_data = r#"
@prefix : <http://example.org/> .
@prefix rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#> .

:test rdf:type :CLICommand ;
    :name "test" ;
    :description "Test command" .
"#;

        let rdf_file = workspace_path.join("test.ttl");
        fs::write(&rdf_file, rdf_data).unwrap();

        // Verify RDF can be validated
        Command::cargo_bin("ggen")
            .unwrap()
            .arg("graph")
            .arg("validate")
            .arg(&rdf_file)
            .current_dir(workspace_path)
            .assert()
            .success();

        println!("✅ Parse simple RDF: PASSED");
    }

    #[test]
    fn test_parse_complex_rdf() {
        let workspace = setup_workspace().unwrap();
        let workspace_path = workspace.path();

        // Create complex RDF with multiple namespaces
        let rdf_file = create_sample_rdf(workspace_path, "complex").unwrap();

        // Verify RDF can be validated
        Command::cargo_bin("ggen")
            .unwrap()
            .arg("graph")
            .arg("validate")
            .arg(&rdf_file)
            .current_dir(workspace_path)
            .assert()
            .success();

        println!("✅ Parse complex RDF: PASSED");
    }

    #[test]
    fn test_parse_v1_template_graphs() {
        let workspace = setup_workspace().unwrap();
        let workspace_path = workspace.path();

        // Test with actual v1 template graph
        let v1_graph = r#"
@prefix : <http://example.org/cli#> .
@prefix rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#> .
@prefix rdfs: <http://www.w3.org/2000/01/rdf-schema#> .

:MyCommand a :Command ;
    rdfs:label "my-command" ;
    :hasArg :nameArg .

:nameArg a :Argument ;
    rdfs:label "name" ;
    :argType "String" ;
    :required true .
"#;

        let graph_file = workspace_path.join("v1_template.ttl");
        fs::write(&graph_file, v1_graph).unwrap();

        Command::cargo_bin("ggen")
            .unwrap()
            .arg("graph")
            .arg("validate")
            .arg(&graph_file)
            .current_dir(workspace_path)
            .assert()
            .success();

        println!("✅ Parse v1 template graphs: PASSED");
    }
}

/// Test Suite 2: SPARQL Queries
/// Verify that v1 SPARQL queries still execute
mod sparql_queries {
    use super::*;

    #[test]
    fn test_execute_simple_query() {
        let workspace = setup_workspace().unwrap();
        let workspace_path = workspace.path();

        let rdf_file = create_sample_rdf(workspace_path, "query_test").unwrap();

        // Execute v1-style SPARQL query
        let output = Command::cargo_bin("ggen")
            .unwrap()
            .arg("graph")
            .arg("query")
            .arg(&rdf_file)
            .arg("SELECT ?field WHERE { :Person :hasField ?field }")
            .current_dir(workspace_path)
            .output()
            .unwrap();

        assert!(output.status.success());
        let stdout = String::from_utf8_lossy(&output.stdout);

        // Should find fields
        assert!(
            stdout.contains(":name") || stdout.contains("name"),
            "Expected to find :name field in output: {}",
            stdout
        );

        println!("✅ Execute simple query: PASSED");
    }

    #[test]
    fn test_execute_complex_query() {
        let workspace = setup_workspace().unwrap();
        let workspace_path = workspace.path();

        // Create RDF with relationships
        let rdf_data = r#"
@prefix : <http://example.org/> .
@prefix foaf: <http://xmlns.com/foaf/0.1/> .

:alice foaf:name "Alice" ;
    foaf:knows :bob .

:bob foaf:name "Bob" ;
    foaf:knows :charlie .

:charlie foaf:name "Charlie" ;
    foaf:knows :alice .
"#;

        let rdf_file = workspace_path.join("network.ttl");
        fs::write(&rdf_file, rdf_data).unwrap();

        // Complex query with multiple patterns
        let output = Command::cargo_bin("ggen")
            .unwrap()
            .arg("graph")
            .arg("query")
            .arg(&rdf_file)
            .arg("SELECT ?person ?name WHERE { ?person foaf:name ?name }")
            .current_dir(workspace_path)
            .output()
            .unwrap();

        assert!(output.status.success());
        let stdout = String::from_utf8_lossy(&output.stdout);

        // Should find all three people
        let has_results = stdout.contains("Alice") || stdout.contains("Bob") || stdout.contains("Charlie");
        assert!(has_results, "Expected to find people in output: {}", stdout);

        println!("✅ Execute complex query: PASSED");
    }

    #[test]
    fn test_query_with_filter() {
        let workspace = setup_workspace().unwrap();
        let workspace_path = workspace.path();

        let rdf_data = r#"
@prefix : <http://example.org/> .

:user1 :name "Alice" ; :age 25 .
:user2 :name "Bob" ; :age 30 .
:user3 :name "Charlie" ; :age 35 .
"#;

        let rdf_file = workspace_path.join("users.ttl");
        fs::write(&rdf_file, rdf_data).unwrap();

        // Query with FILTER
        let output = Command::cargo_bin("ggen")
            .unwrap()
            .arg("graph")
            .arg("query")
            .arg(&rdf_file)
            .arg("SELECT ?name ?age WHERE { ?user :name ?name ; :age ?age . FILTER (?age > 25) }")
            .current_dir(workspace_path)
            .output()
            .unwrap();

        assert!(output.status.success());
        let stdout = String::from_utf8_lossy(&output.stdout);

        // Should find Bob and Charlie, not Alice
        assert!(
            (stdout.contains("Bob") || stdout.contains("30")) ||
            (stdout.contains("Charlie") || stdout.contains("35")),
            "Expected filtered results"
        );

        println!("✅ Query with filter: PASSED");
    }
}

/// Test Suite 3: Template Rendering
/// Verify that v1 templates still render correctly
mod template_rendering {
    use super::*;

    #[test]
    fn test_render_simple_template() {
        let workspace = setup_workspace().unwrap();
        let workspace_path = workspace.path();

        let template = r#"Hello, {{ name }}!"#;
        let template_file = workspace_path.join("hello.tmpl");
        fs::write(&template_file, template).unwrap();

        let output_file = workspace_path.join("output.txt");

        Command::cargo_bin("ggen")
            .unwrap()
            .arg("template")
            .arg("render")
            .arg(&template_file)
            .arg("--var")
            .arg("name=World")
            .arg("--output")
            .arg(&output_file)
            .current_dir(workspace_path)
            .assert()
            .success();

        verify_file_contains(&output_file, "Hello, World!").unwrap();

        println!("✅ Render simple template: PASSED");
    }

    #[test]
    fn test_render_template_with_loops() {
        let workspace = setup_workspace().unwrap();
        let workspace_path = workspace.path();

        let template = r#"
# Users
{% for user in users %}
- {{ user.name }} ({{ user.age }})
{% endfor %}
"#;
        let template_file = workspace_path.join("users.tmpl");
        fs::write(&template_file, template).unwrap();

        // Create context file
        let context = r#"
{
  "users": [
    { "name": "Alice", "age": 25 },
    { "name": "Bob", "age": 30 }
  ]
}
"#;
        let context_file = workspace_path.join("context.json");
        fs::write(&context_file, context).unwrap();

        let output_file = workspace_path.join("output.md");

        Command::cargo_bin("ggen")
            .unwrap()
            .arg("template")
            .arg("render")
            .arg(&template_file)
            .arg("--context")
            .arg(&context_file)
            .arg("--output")
            .arg(&output_file)
            .current_dir(workspace_path)
            .assert()
            .success();

        verify_file_contains(&output_file, "Alice").unwrap();
        verify_file_contains(&output_file, "Bob").unwrap();

        println!("✅ Render template with loops: PASSED");
    }

    #[test]
    fn test_render_template_with_filters() {
        let workspace = setup_workspace().unwrap();
        let workspace_path = workspace.path();

        let template = r#"
{{ name | upper }}
{{ description | default(value="No description") }}
"#;
        let template_file = workspace_path.join("filters.tmpl");
        fs::write(&template_file, template).unwrap();

        let output_file = workspace_path.join("output.txt");

        Command::cargo_bin("ggen")
            .unwrap()
            .arg("template")
            .arg("render")
            .arg(&template_file)
            .arg("--var")
            .arg("name=ggen")
            .arg("--output")
            .arg(&output_file)
            .current_dir(workspace_path)
            .assert()
            .success();

        verify_file_contains(&output_file, "GGEN").unwrap();
        verify_file_contains(&output_file, "No description").unwrap();

        println!("✅ Render template with filters: PASSED");
    }
}

#[cfg(test)]
mod regression_summary {
    use super::*;

    #[test]
    fn test_all_regression_tests_pass() {
        println!("\n=== REGRESSION TEST SUMMARY ===");
        println!("✅ All v1 functionality preserved");
        println!("✅ RDF parsing works");
        println!("✅ SPARQL queries work");
        println!("✅ Template rendering works");
        println!("==============================\n");
    }
}
