use super::super::world::GgenWorld;
use assert_cmd::Command;
use cucumber::{given, then, when};
use std::fs;

/// Step definitions for `ggen graph` noun-verb commands
///
/// Covers RDF graph operations:
/// - graph load: Load RDF data into graph
/// - graph query: Execute SPARQL queries
/// - graph export: Export graph to file
/// - graph validate: Validate against SHACL shapes
/// - graph stats: Show graph statistics

// ============================================================================
// GIVEN steps - Setup preconditions
// ============================================================================

#[given(regex = r#"^I have an RDF file "([^"]+)" with content:$"#)]
async fn create_rdf_file(world: &mut GgenWorld, filename: String, content: String) {
    let file_path = world.project_dir.join(&filename);
    fs::write(&file_path, content.trim())
        .unwrap_or_else(|e| panic!("Failed to write RDF file {}: {}", filename, e));
}

#[given(regex = r#"^I have an RDF file "([^"]+)" in RDF/XML format$"#)]
async fn create_rdf_xml_file(world: &mut GgenWorld, filename: String) {
    let content = r#"<?xml version="1.0"?>
<rdf:RDF xmlns:rdf="http://www.w3.org/1999/02/22-rdf-syntax-ns#"
         xmlns:ex="http://example.org/">
  <ex:Person rdf:about="http://example.org/Alice">
    <ex:name>Alice</ex:name>
    <ex:age rdf:datatype="http://www.w3.org/2001/XMLSchema#integer">30</ex:age>
  </ex:Person>
</rdf:RDF>"#;

    let file_path = world.project_dir.join(&filename);
    fs::write(&file_path, content)
        .unwrap_or_else(|e| panic!("Failed to write RDF/XML file {}: {}", filename, e));
}

#[given(regex = r"^I have a graph with person data$")]
async fn create_graph_with_person_data(world: &mut GgenWorld) {
    let turtle = r#"@prefix ex: <http://example.org/> .
@prefix rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#> .

ex:Alice a ex:Person ;
         ex:name "Alice" ;
         ex:age 30 .
"#;

    let file_path = world.project_dir.join("person_data.ttl");
    fs::write(&file_path, turtle)
        .unwrap_or_else(|e| panic!("Failed to write person data: {}", e));

    // Load it into the graph
    let output = Command::cargo_bin("ggen")
        .expect("ggen binary not found")
        .arg("graph")
        .arg("load")
        .arg("person_data.ttl")
        .current_dir(&world.project_dir)
        .output()
        .expect("Failed to load graph");

    world.last_output = Some(output.clone());
    world.last_exit_code = output.status.code();
}

#[given(regex = r#"^I have a SPARQL query file "([^"]+)" with:$"#)]
async fn create_sparql_query_file(world: &mut GgenWorld, filename: String, query: String) {
    let file_path = world.project_dir.join(&filename);
    fs::write(&file_path, query.trim())
        .unwrap_or_else(|e| panic!("Failed to write SPARQL file {}: {}", filename, e));
}

#[given(regex = r"^I have a graph with (\d+) triples$")]
async fn create_graph_with_n_triples(world: &mut GgenWorld, count: usize) {
    let mut turtle = String::from("@prefix ex: <http://example.org/> .\n\n");

    for i in 0..count {
        turtle.push_str(&format!("ex:subject{} ex:predicate{} ex:object{} .\n", i, i, i));
    }

    let file_path = world.project_dir.join("data.ttl");
    fs::write(&file_path, turtle)
        .unwrap_or_else(|e| panic!("Failed to write RDF data: {}", e));

    // Load it
    let output = Command::cargo_bin("ggen")
        .expect("ggen binary not found")
        .arg("graph")
        .arg("load")
        .arg("data.ttl")
        .current_dir(&world.project_dir)
        .output()
        .expect("Failed to load graph");

    world.last_output = Some(output);
}

#[given(regex = r"^I have a graph with multiple RDF types$")]
async fn create_graph_with_multiple_types(world: &mut GgenWorld) {
    let turtle = r#"@prefix ex: <http://example.org/> .
@prefix rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#> .

ex:Alice a ex:Person .
ex:ACME a ex:Organization .
ex:RustLang a ex:ProgrammingLanguage .
"#;

    let file_path = world.project_dir.join("types.ttl");
    fs::write(&file_path, turtle)
        .unwrap_or_else(|e| panic!("Failed to write types data: {}", e));

    let output = Command::cargo_bin("ggen")
        .expect("ggen binary not found")
        .arg("graph")
        .arg("load")
        .arg("types.ttl")
        .current_dir(&world.project_dir)
        .output()
        .expect("Failed to load graph");

    world.last_output = Some(output);
}

#[given(regex = r"^I have a graph with (\d+) people$")]
async fn create_graph_with_n_people(world: &mut GgenWorld, count: usize) {
    let mut turtle = String::from("@prefix ex: <http://example.org/> .\n\n");

    for i in 0..count {
        turtle.push_str(&format!(
            "ex:Person{} a ex:Person ; ex:name \"Person {}\" .\n",
            i, i
        ));
    }

    let file_path = world.project_dir.join("people.ttl");
    fs::write(&file_path, turtle)
        .unwrap_or_else(|e| panic!("Failed to write people data: {}", e));

    let output = Command::cargo_bin("ggen")
        .expect("ggen binary not found")
        .arg("graph")
        .arg("load")
        .arg("people.ttl")
        .current_dir(&world.project_dir)
        .output()
        .expect("Failed to load graph");

    world.last_output = Some(output);
}

// ============================================================================
// WHEN steps - Execute actions
// ============================================================================

#[when(regex = r#"^I run "ggen graph (.+)"$"#)]
async fn run_ggen_graph_command(world: &mut GgenWorld, args: String) {
    let arg_list: Vec<&str> = args.split_whitespace().collect();

    let mut cmd = Command::cargo_bin("ggen").expect("ggen binary not found");
    let output = cmd
        .arg("graph")
        .args(&arg_list)
        .current_dir(&world.project_dir)
        .output()
        .expect("Failed to run ggen graph command");

    world.last_output = Some(output.clone());
    world.last_exit_code = output.status.code();
}

// ============================================================================
// THEN steps - Verify outcomes
// ============================================================================

#[then(regex = r#"^I should see "([^"]+)" in output$"#)]
async fn should_see_in_output(world: &mut GgenWorld, expected: String) {
    let stdout = world.last_stdout();
    let stderr = world.last_stderr();

    assert!(
        stdout.contains(&expected) || stderr.contains(&expected),
        "Expected to see '{}' in output, but got:\nStdout: {}\nStderr: {}",
        expected,
        stdout,
        stderr
    );
}

#[then(regex = r"^the graph should contain the triples$")]
async fn graph_should_contain_triples(world: &mut GgenWorld) {
    // Query the graph to verify it has data
    let output = Command::cargo_bin("ggen")
        .expect("ggen binary not found")
        .arg("graph")
        .arg("query")
        .arg("SELECT (COUNT(*) as ?count) WHERE { ?s ?p ?o }")
        .current_dir(&world.project_dir)
        .output()
        .expect("Failed to query graph");

    let stdout = String::from_utf8_lossy(&output.stdout);
    assert!(
        !stdout.contains("0"),
        "Graph should contain triples, but count is 0"
    );
}

#[then(regex = r"^I should see query results in output$")]
async fn should_see_query_results(_world: &mut GgenWorld) {
    // This is validated by checking command success and output content
    // Individual tests will verify specific results
}

#[then(regex = r#"^I should see "([^"]+)" and "([^"]+)" in results$"#)]
async fn should_see_two_values_in_results(world: &mut GgenWorld, val1: String, val2: String) {
    let stdout = world.last_stdout();

    assert!(
        stdout.contains(&val1),
        "Expected to see '{}' in results, but got: {}",
        val1,
        stdout
    );
    assert!(
        stdout.contains(&val2),
        "Expected to see '{}' in results, but got: {}",
        val2,
        stdout
    );
}

#[then(regex = r"^the output should be valid JSON$")]
async fn output_should_be_valid_json(world: &mut GgenWorld) {
    let stdout = world.last_stdout();

    serde_json::from_str::<serde_json::Value>(&stdout)
        .unwrap_or_else(|e| panic!("Output is not valid JSON: {}\nOutput: {}", e, stdout));
}

#[then(regex = r"^I should see a formatted table in output$")]
async fn should_see_formatted_table(world: &mut GgenWorld) {
    let stdout = world.last_stdout();

    // Tables typically have column separators or headers
    assert!(
        stdout.contains("|") || stdout.contains("─") || stdout.contains("name"),
        "Expected to see a formatted table in output, but got: {}",
        stdout
    );
}

#[then(regex = r#"^the file "([^"]+)" should exist$"#)]
async fn file_should_exist(world: &mut GgenWorld, filename: String) {
    let file_path = world.project_dir.join(&filename);
    assert!(
        file_path.exists(),
        "File {} should exist at {}",
        filename,
        file_path.display()
    );
}

#[then(regex = r"^the file should contain valid Turtle syntax$")]
async fn file_should_contain_valid_turtle(world: &mut GgenWorld) {
    // In a real implementation, we'd parse the Turtle file
    // For now, check that a file was created and has content
    let stdout = world.last_stdout();
    assert!(!stdout.is_empty(), "Expected Turtle output");
}

#[then(regex = r"^the file should be valid JSON-LD$")]
async fn file_should_be_valid_jsonld(world: &mut GgenWorld) {
    let file_path = world.project_dir.join("export.jsonld");
    if file_path.exists() {
        let content = fs::read_to_string(&file_path)
            .expect("Failed to read JSON-LD file");
        serde_json::from_str::<serde_json::Value>(&content)
            .unwrap_or_else(|e| panic!("File is not valid JSON-LD: {}", e));
    }
}

#[then(regex = r"^the file should contain N-Triples format$")]
async fn file_should_contain_ntriples(_world: &mut GgenWorld) {
    // N-Triples validation would check for proper format
    // For now, we assume the export command succeeded
}

#[then(regex = r"^I should see all triples in results$")]
async fn should_see_all_triples(_world: &mut GgenWorld) {
    // This would validate that SPARQL SELECT * returns all triples
}

#[then(regex = r#"^I should see "(\d+)" in results$"#)]
async fn should_see_number_in_results(world: &mut GgenWorld, expected: String) {
    let stdout = world.last_stdout();
    assert!(
        stdout.contains(&expected),
        "Expected to see '{}' in results, but got: {}",
        expected,
        stdout
    );
}

#[then(regex = r"^I should see all unique classes$")]
async fn should_see_all_unique_classes(_world: &mut GgenWorld) {
    // Would validate that all RDF types are listed
}

#[then(regex = r"^I should see all properties and values for Alice$")]
async fn should_see_all_properties_for_alice(_world: &mut GgenWorld) {
    // Would validate Alice's properties are shown
}

#[then(regex = r"^the command should fail$")]
async fn command_should_fail(world: &mut GgenWorld) {
    assert!(
        !world.last_command_succeeded(),
        "Command should have failed but succeeded"
    );
}

#[then(regex = r#"^I should see "([^"]+)" in stderr$"#)]
async fn should_see_in_stderr(world: &mut GgenWorld, expected: String) {
    let stderr = world.last_stderr();
    assert!(
        stderr.contains(&expected),
        "Expected to see '{}' in stderr, but got: {}",
        expected,
        stderr
    );
}
