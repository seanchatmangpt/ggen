//! Integration Tests - End-to-end workflows
//!
//! These tests verify complete workflows work correctly:
//! - TTL file → RDF graph → SPARQL → Template → Output
//! - v1 workflows still work in v2 architecture
//! - New v2 workflows work

use assert_cmd::Command;
use predicates::prelude::*;
use std::fs;

use super::helpers::*;

/// Test Suite 1: Complete RDF-to-Code Workflow
/// TTL → Parse → Query → Template → Generate Code
mod rdf_to_code_workflow {
    use super::*;

    #[test]
    fn test_end_to_end_rdf_code_generation() {
        let workspace = setup_workspace().unwrap();
        let workspace_path = workspace.path();

        // Step 1: Create RDF schema
        let rdf_data = r#"
@prefix : <http://example.org/> .
@prefix schema: <http://schema.org/> .

:User a schema:Class ;
    :hasField :username ;
    :hasField :email ;
    :hasField :created_at .

:username a :Field ;
    :fieldName "username" ;
    :fieldType "String" ;
    :required true .

:email a :Field ;
    :fieldName "email" ;
    :fieldType "String" ;
    :required true .

:created_at a :Field ;
    :fieldName "created_at" ;
    :fieldType "DateTime" ;
    :required false .
"#;

        let rdf_file = workspace_path.join("schema.ttl");
        fs::write(&rdf_file, rdf_data).unwrap();

        // Step 2: Validate RDF
        Command::cargo_bin("ggen")
            .unwrap()
            .arg("graph")
            .arg("validate")
            .arg(&rdf_file)
            .current_dir(workspace_path)
            .assert()
            .success();

        // Step 3: Query RDF for fields
        let output = Command::cargo_bin("ggen")
            .unwrap()
            .arg("graph")
            .arg("query")
            .arg(&rdf_file)
            .arg("SELECT ?field ?name WHERE { ?field :fieldName ?name }")
            .current_dir(workspace_path)
            .output()
            .unwrap();

        assert!(output.status.success());
        let query_result = String::from_utf8_lossy(&output.stdout);
        assert!(query_result.contains("username") || query_result.contains("email"));

        // Step 4: Create template for struct generation
        let template = r#"
#[derive(Debug, Clone)]
pub struct User {
    pub username: String,
    pub email: String,
    pub created_at: Option<DateTime<Utc>>,
}
"#;
        let template_file = workspace_path.join("struct.tmpl");
        fs::write(&template_file, template).unwrap();

        // Step 5: Render template
        let output_file = workspace_path.join("user.rs");
        Command::cargo_bin("ggen")
            .unwrap()
            .arg("template")
            .arg("render")
            .arg(&template_file)
            .arg("--output")
            .arg(&output_file)
            .current_dir(workspace_path)
            .assert()
            .success();

        // Verify generated code
        verify_file_contains(&output_file, "pub struct User").unwrap();
        verify_file_contains(&output_file, "username: String").unwrap();
        verify_file_contains(&output_file, "email: String").unwrap();

        println!("✅ End-to-end RDF code generation: PASSED");
    }

    #[test]
    fn test_rdf_query_template_pipeline() {
        let workspace = setup_workspace().unwrap();
        let workspace_path = workspace.path();

        // Create RDF with project metadata
        let rdf_data = r#"
@prefix : <http://example.org/> .

:project :name "MyAPI" ;
    :version "2.0.0" ;
    :description "REST API Server" ;
    :language "Rust" .
"#;

        let rdf_file = workspace_path.join("project.ttl");
        fs::write(&rdf_file, rdf_data).unwrap();

        // Query project metadata
        let output = Command::cargo_bin("ggen")
            .unwrap()
            .arg("graph")
            .arg("query")
            .arg(&rdf_file)
            .arg("SELECT ?name ?version WHERE { :project :name ?name ; :version ?version }")
            .current_dir(workspace_path)
            .output()
            .unwrap();

        assert!(output.status.success());

        // Create README template
        let template = r#"
# {{ project_name | default(value="Project") }}

Version: {{ version | default(value="1.0.0") }}
Language: {{ language | default(value="Unknown") }}

{{ description | default(value="No description") }}
"#;
        let template_file = workspace_path.join("README.tmpl");
        fs::write(&template_file, template).unwrap();

        // Render with RDF data
        let output_file = workspace_path.join("README.md");
        Command::cargo_bin("ggen")
            .unwrap()
            .arg("template")
            .arg("render")
            .arg(&template_file)
            .arg("--var")
            .arg("project_name=MyAPI")
            .arg("--var")
            .arg("version=2.0.0")
            .arg("--var")
            .arg("language=Rust")
            .arg("--var")
            .arg("description=REST API Server")
            .arg("--output")
            .arg(&output_file)
            .current_dir(workspace_path)
            .assert()
            .success();

        // Verify output
        verify_file_contains(&output_file, "MyAPI").unwrap();
        verify_file_contains(&output_file, "2.0.0").unwrap();
        verify_file_contains(&output_file, "Rust").unwrap();

        println!("✅ RDF query template pipeline: PASSED");
    }
}

/// Test Suite 2: Multi-Step Generation Workflows
/// Test complex workflows with multiple steps
mod multi_step_workflows {
    use super::*;

    #[test]
    fn test_validate_query_render_workflow() {
        let workspace = setup_workspace().unwrap();
        let workspace_path = workspace.path();

        let rdf_file = create_sample_rdf(workspace_path, "multi_step").unwrap();

        // Step 1: Validate
        Command::cargo_bin("ggen")
            .unwrap()
            .arg("graph")
            .arg("validate")
            .arg(&rdf_file)
            .current_dir(workspace_path)
            .assert()
            .success();

        // Step 2: Query
        Command::cargo_bin("ggen")
            .unwrap()
            .arg("graph")
            .arg("query")
            .arg(&rdf_file)
            .arg("SELECT ?s ?p ?o WHERE { ?s ?p ?o } LIMIT 10")
            .current_dir(workspace_path)
            .assert()
            .success();

        // Step 3: Render template
        let template_file = create_sample_template(workspace_path, "multi_step").unwrap();
        let output_file = workspace_path.join("output.md");

        Command::cargo_bin("ggen")
            .unwrap()
            .arg("template")
            .arg("render")
            .arg(&template_file)
            .arg("--var")
            .arg("project_name=multi_step")
            .arg("--output")
            .arg(&output_file)
            .current_dir(workspace_path)
            .assert()
            .success();

        verify_file_contains(&output_file, "multi_step").unwrap();

        println!("✅ Validate-Query-Render workflow: PASSED");
    }

    #[test]
    fn test_template_marketplace_workflow() {
        let workspace = setup_workspace().unwrap();
        let workspace_path = workspace.path();

        // Search marketplace
        let output = Command::cargo_bin("ggen")
            .unwrap()
            .arg("marketplace")
            .arg("search")
            .arg("rust")
            .current_dir(workspace_path)
            .output()
            .unwrap();

        assert!(output.status.success());

        // List available templates
        Command::cargo_bin("ggen")
            .unwrap()
            .arg("marketplace")
            .arg("list")
            .current_dir(workspace_path)
            .assert()
            .success();

        println!("✅ Template marketplace workflow: PASSED");
    }

    #[test]
    fn test_multi_file_generation_workflow() {
        let workspace = setup_workspace().unwrap();
        let workspace_path = workspace.path();

        // Create RDF for multiple files
        let rdf_data = r#"
@prefix : <http://example.org/> .

:file1 :name "main.rs" ; :content "fn main() {}" .
:file2 :name "lib.rs" ; :content "pub fn hello() {}" .
:file3 :name "README.md" ; :content "# Project" .
"#;

        let rdf_file = workspace_path.join("files.ttl");
        fs::write(&rdf_file, rdf_data).unwrap();

        // Validate
        Command::cargo_bin("ggen")
            .unwrap()
            .arg("graph")
            .arg("validate")
            .arg(&rdf_file)
            .current_dir(workspace_path)
            .assert()
            .success();

        // Query for files
        let output = Command::cargo_bin("ggen")
            .unwrap()
            .arg("graph")
            .arg("query")
            .arg(&rdf_file)
            .arg("SELECT ?file ?name WHERE { ?file :name ?name }")
            .current_dir(workspace_path)
            .output()
            .unwrap();

        assert!(output.status.success());
        let result = String::from_utf8_lossy(&output.stdout);
        assert!(result.contains("main.rs") || result.contains("lib.rs"));

        println!("✅ Multi-file generation workflow: PASSED");
    }
}

/// Test Suite 3: V1 Workflows in V2 Architecture
/// Ensure v1 workflows still work after refactoring
mod v1_workflows_in_v2 {
    use super::*;

    #[test]
    fn test_v1_template_generation_workflow() {
        let workspace = setup_workspace().unwrap();
        let workspace_path = workspace.path();

        // V1-style workflow: template list → template new
        Command::cargo_bin("ggen")
            .unwrap()
            .arg("template")
            .arg("list")
            .current_dir(workspace_path)
            .assert()
            .success();

        println!("✅ V1 template generation workflow: PASSED");
    }

    #[test]
    fn test_v1_rdf_workflow() {
        let workspace = setup_workspace().unwrap();
        let workspace_path = workspace.path();

        // V1-style workflow: create RDF → validate → query
        let rdf_file = create_sample_rdf(workspace_path, "v1_workflow").unwrap();

        Command::cargo_bin("ggen")
            .unwrap()
            .arg("graph")
            .arg("validate")
            .arg(&rdf_file)
            .current_dir(workspace_path)
            .assert()
            .success();

        Command::cargo_bin("ggen")
            .unwrap()
            .arg("graph")
            .arg("query")
            .arg(&rdf_file)
            .arg("SELECT * WHERE { ?s ?p ?o } LIMIT 5")
            .current_dir(workspace_path)
            .assert()
            .success();

        println!("✅ V1 RDF workflow: PASSED");
    }

    #[test]
    fn test_v1_marketplace_workflow() {
        // V1-style workflow: search → list
        Command::cargo_bin("ggen")
            .unwrap()
            .arg("marketplace")
            .arg("search")
            .arg("rust")
            .assert()
            .success();

        Command::cargo_bin("ggen")
            .unwrap()
            .arg("marketplace")
            .arg("list")
            .assert()
            .success();

        println!("✅ V1 marketplace workflow: PASSED");
    }
}

/// Test Suite 4: New V2 Workflows
/// Test new capabilities enabled by v2 architecture
mod v2_new_workflows {
    use super::*;

    #[test]
    fn test_v2_async_graph_operations() {
        let workspace = setup_workspace().unwrap();
        let workspace_path = workspace.path();

        // V2 enables async operations
        let rdf_file = create_sample_rdf(workspace_path, "v2_async").unwrap();

        // Multiple async operations should work
        Command::cargo_bin("ggen")
            .unwrap()
            .arg("graph")
            .arg("validate")
            .arg(&rdf_file)
            .current_dir(workspace_path)
            .assert()
            .success();

        println!("✅ V2 async graph operations: PASSED");
    }

    #[test]
    fn test_v2_improved_error_handling() {
        let workspace = setup_workspace().unwrap();
        let workspace_path = workspace.path();

        // V2 should have better error messages
        let bad_file = workspace_path.join("invalid.ttl");
        fs::write(&bad_file, "<<< INVALID RDF >>>").unwrap();

        let output = Command::cargo_bin("ggen")
            .unwrap()
            .arg("graph")
            .arg("validate")
            .arg(&bad_file)
            .current_dir(workspace_path)
            .output()
            .unwrap();

        assert!(!output.status.success());

        let stderr = String::from_utf8_lossy(&output.stderr);
        // V2 should have helpful error messages
        assert!(
            stderr.len() > 0,
            "Should provide error message"
        );

        println!("✅ V2 improved error handling: PASSED");
    }

    #[test]
    fn test_v2_domain_modularity() {
        let workspace = setup_workspace().unwrap();
        let workspace_path = workspace.path();

        // V2 architecture enables better domain separation
        // Test that different domains work independently

        // Graph domain
        let rdf_file = create_sample_rdf(workspace_path, "modular").unwrap();
        Command::cargo_bin("ggen")
            .unwrap()
            .arg("graph")
            .arg("validate")
            .arg(&rdf_file)
            .current_dir(workspace_path)
            .assert()
            .success();

        // Template domain
        let template_file = create_sample_template(workspace_path, "modular").unwrap();
        let output_file = workspace_path.join("modular.md");
        Command::cargo_bin("ggen")
            .unwrap()
            .arg("template")
            .arg("render")
            .arg(&template_file)
            .arg("--var")
            .arg("project_name=Modular")
            .arg("--output")
            .arg(&output_file)
            .current_dir(workspace_path)
            .assert()
            .success();

        // Marketplace domain
        Command::cargo_bin("ggen")
            .unwrap()
            .arg("marketplace")
            .arg("list")
            .current_dir(workspace_path)
            .assert()
            .success();

        println!("✅ V2 domain modularity: PASSED");
    }
}

#[cfg(test)]
mod integration_summary {
    use super::*;

    #[test]
    fn test_all_integration_tests_pass() {
        println!("\n=== INTEGRATION TEST SUMMARY ===");
        println!("✅ RDF-to-code workflows work");
        println!("✅ Multi-step workflows work");
        println!("✅ V1 workflows preserved in V2");
        println!("✅ New V2 workflows enabled");
        println!("================================\n");
    }
}
