// Scenario 8: Deterministic output verification
// Chicago TDD: Verify REAL repeatability of code generation

use assert_cmd::Command;
use std::fs;

use super::test_helpers::*;

#[test]
fn test_deterministic_code_generation() {
    let workspace = setup_workspace().unwrap();
    let workspace_path = workspace.path();

    // Generate same project 3 times
    let mut generated_files = Vec::new();

    for i in 1..=3 {
        let output_dir = workspace_path.join(format!("output-{}", i));
        fs::create_dir_all(&output_dir).unwrap();

        Command::cargo_bin("ggen")
            .unwrap()
            .arg("project")
            .arg("new")
            .arg("test-app")
            .current_dir(&output_dir)
            .assert()
            .success();

        // Read generated main.rs
        let main_rs = output_dir.join("test-app/src/main.rs");
        assert!(main_rs.exists(), "Generated file should exist (run {})", i);

        #[allow(clippy::expect_used)]
        let content = fs::read_to_string(&main_rs).expect("Should be able to read generated file");
        generated_files.push(content);
    }

    // All 3 outputs must be identical
    assert_eq!(
        generated_files[0], generated_files[1],
        "First and second generation should be identical"
    );
    assert_eq!(
        generated_files[1], generated_files[2],
        "Second and third generation should be identical"
    );

    println!("✅ Deterministic output: PASSED");
    println!("   - 3 identical generations verified");
}

#[test]
fn test_template_rendering_determinism() {
    let workspace = setup_workspace().unwrap();
    let workspace_path = workspace.path();

    // Create a template with variables
    let template = r#"
// Generated code
pub struct {{ struct_name }} {
    pub field1: String,
    pub field2: i32,
}

impl {{ struct_name }} {
    pub fn new() -> Self {
        Self {
            field1: "{{ default_value }}".to_string(),
            field2: 42,
        }
    }
}
"#;

    let template_file = workspace_path.join("struct.tmpl");
    fs::write(&template_file, template).unwrap();

    // Render template 5 times with same variables
    let mut rendered_outputs = Vec::new();

    for i in 1..=5 {
        let output_file = workspace_path.join(format!("output-{}.rs", i));

        Command::cargo_bin("ggen")
            .unwrap()
            .arg("template")
            .arg("render")
            .arg(&template_file)
            .arg("--var")
            .arg("struct_name=Person")
            .arg("--var")
            .arg("default_value=John Doe")
            .arg("--output")
            .arg(&output_file)
            .current_dir(workspace_path)
            .assert()
            .success();

        let content = fs::read_to_string(&output_file).unwrap();
        rendered_outputs.push(content);
    }

    // All 5 renders must be identical
    for i in 1..5 {
        assert_eq!(
            rendered_outputs[0],
            rendered_outputs[i],
            "Render {} should match first render",
            i + 1
        );
    }

    println!("✅ Template rendering determinism: PASSED");
    println!("   - 5 identical renders verified");
}

#[test]
fn test_rdf_query_determinism() {
    let workspace = setup_workspace().unwrap();
    let workspace_path = workspace.path();

    // Create RDF data
    let rdf_data = r#"
@prefix : <http://example.org/> .
@prefix rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#> .

:Alice :knows :Bob .
:Bob :knows :Charlie .
:Charlie :knows :Alice .
"#;

    let rdf_file = workspace_path.join("data.ttl");
    fs::write(&rdf_file, rdf_data).unwrap();

    // Run same SPARQL query 3 times
    let mut query_results = Vec::new();

    for _ in 1..=3 {
        let output = Command::cargo_bin("ggen")
            .unwrap()
            .arg("rdf")
            .arg("query")
            .arg(&rdf_file)
            .arg("SELECT ?s ?o WHERE { ?s :knows ?o }")
            .current_dir(workspace_path)
            .output()
            .unwrap();

        assert!(output.status.success(), "Query should succeed");
        query_results.push(String::from_utf8_lossy(&output.stdout).to_string());
    }

    // All query results should be identical
    assert_eq!(query_results[0], query_results[1]);
    assert_eq!(query_results[1], query_results[2]);

    println!("✅ RDF query determinism: PASSED");
    println!("   - 3 identical query results verified");
}
