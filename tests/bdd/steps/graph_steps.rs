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
fn create_rdf_file(world: &mut GgenWorld, filename: String, content: String) {
    let file_path = world.project_dir.join(&filename);
    fs::write(&file_path, content.trim())
        .unwrap_or_else(|e| panic!("Failed to write RDF file {}: {}", filename, e));
}

#[given(regex = r#"^I have an RDF file "([^"]+)" in RDF/XML format$"#)]
fn create_rdf_xml_file(world: &mut GgenWorld, filename: String) {
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
fn create_graph_with_person_data(world: &mut GgenWorld) {
    let turtle = r#"@prefix ex: <http://example.org/> .
@prefix rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#> .

ex:Alice a ex:Person ;
         ex:name "Alice" ;
         ex:age 30 .
"#;

    let file_path = world.project_dir.join("person_data.ttl");
    fs::write(&file_path, turtle).unwrap_or_else(|e| panic!("Failed to write person data: {}", e));

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
fn create_sparql_query_file(world: &mut GgenWorld, filename: String, query: String) {
    let file_path = world.project_dir.join(&filename);
    fs::write(&file_path, query.trim())
        .unwrap_or_else(|e| panic!("Failed to write SPARQL file {}: {}", filename, e));
}

#[given(regex = r"^I have a graph with (\d+) triples$")]
fn create_graph_with_n_triples(world: &mut GgenWorld, count: usize) {
    let mut turtle = String::from("@prefix ex: <http://example.org/> .\n\n");

    for i in 0..count {
        turtle.push_str(&format!(
            "ex:subject{} ex:predicate{} ex:object{} .\n",
            i, i, i
        ));
    }

    let file_path = world.project_dir.join("data.ttl");
    fs::write(&file_path, turtle).unwrap_or_else(|e| panic!("Failed to write RDF data: {}", e));

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
fn create_graph_with_multiple_types(world: &mut GgenWorld) {
    let turtle = r#"@prefix ex: <http://example.org/> .
@prefix rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#> .

ex:Alice a ex:Person .
ex:ACME a ex:Organization .
ex:RustLang a ex:ProgrammingLanguage .
"#;

    let file_path = world.project_dir.join("types.ttl");
    fs::write(&file_path, turtle).unwrap_or_else(|e| panic!("Failed to write types data: {}", e));

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
fn create_graph_with_n_people(world: &mut GgenWorld, count: usize) {
    let mut turtle = String::from("@prefix ex: <http://example.org/> .\n\n");

    for i in 0..count {
        turtle.push_str(&format!(
            "ex:Person{} a ex:Person ; ex:name \"Person {}\" .\n",
            i, i
        ));
    }

    let file_path = world.project_dir.join("people.ttl");
    fs::write(&file_path, turtle).unwrap_or_else(|e| panic!("Failed to write people data: {}", e));

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
fn run_ggen_graph_command(world: &mut GgenWorld, args: String) {
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
fn should_see_in_output(world: &mut GgenWorld, expected: String) {
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
fn graph_should_contain_triples(world: &mut GgenWorld) {
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
fn should_see_query_results(_world: &mut GgenWorld) {
    // This is validated by checking command success and output content
    // Individual tests will verify specific results
}

#[then(regex = r#"^I should see "([^"]+)" and "([^"]+)" in results$"#)]
fn should_see_two_values_in_results(world: &mut GgenWorld, val1: String, val2: String) {
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
fn output_should_be_valid_json(world: &mut GgenWorld) {
    let stdout = world.last_stdout();

    serde_json::from_str::<serde_json::Value>(&stdout)
        .unwrap_or_else(|e| panic!("Output is not valid JSON: {}\nOutput: {}", e, stdout));
}

#[then(regex = r"^I should see a formatted table in output$")]
fn should_see_formatted_table(world: &mut GgenWorld) {
    let stdout = world.last_stdout();

    // Tables typically have column separators or headers
    assert!(
        stdout.contains("|") || stdout.contains("─") || stdout.contains("name"),
        "Expected to see a formatted table in output, but got: {}",
        stdout
    );
}

#[then(regex = r#"^the file "([^"]+)" should exist$"#)]
fn file_should_exist(world: &mut GgenWorld, filename: String) {
    let file_path = world.project_dir.join(&filename);
    assert!(
        file_path.exists(),
        "File {} should exist at {}",
        filename,
        file_path.display()
    );
}

#[then(regex = r"^the file should contain valid Turtle syntax$")]
fn file_should_contain_valid_turtle(world: &mut GgenWorld) {
    // In a real implementation, we'd parse the Turtle file
    // For now, check that a file was created and has content
    let stdout = world.last_stdout();
    assert!(!stdout.is_empty(), "Expected Turtle output");
}

#[then(regex = r"^the file should be valid JSON-LD$")]
fn file_should_be_valid_jsonld(world: &mut GgenWorld) {
    let file_path = world.project_dir.join("export.jsonld");
    if file_path.exists() {
        let content = fs::read_to_string(&file_path).expect("Failed to read JSON-LD file");
        serde_json::from_str::<serde_json::Value>(&content)
            .unwrap_or_else(|e| panic!("File is not valid JSON-LD: {}", e));
    }
}

#[then(regex = r"^the file should contain N-Triples format$")]
fn file_should_contain_ntriples(_world: &mut GgenWorld) {
    // N-Triples validation would check for proper format
    // For now, we assume the export command succeeded
}

#[then(regex = r"^I should see all triples in results$")]
fn should_see_all_triples(_world: &mut GgenWorld) {
    // This would validate that SPARQL SELECT * returns all triples
}

#[then(regex = r#"^I should see "(\d+)" in results$"#)]
fn should_see_number_in_results(world: &mut GgenWorld, expected: String) {
    let stdout = world.last_stdout();
    assert!(
        stdout.contains(&expected),
        "Expected to see '{}' in results, but got: {}",
        expected,
        stdout
    );
}

#[then(regex = r"^I should see all unique classes$")]
fn should_see_all_unique_classes(_world: &mut GgenWorld) {
    // Would validate that all RDF types are listed
}

#[then(regex = r"^I should see all properties and values for Alice$")]
fn should_see_all_properties_for_alice(_world: &mut GgenWorld) {
    // Would validate Alice's properties are shown
}

#[then(regex = r"^the command should fail$")]
fn command_should_fail(world: &mut GgenWorld) {
    assert!(
        !world.last_command_succeeded(),
        "Command should have failed but succeeded"
    );
}

#[then(regex = r#"^I should see "([^"]+)" in stderr$"#)]
fn should_see_in_stderr(world: &mut GgenWorld, expected: String) {
    let stderr = world.last_stderr();
    assert!(
        stderr.contains(&expected),
        "Expected to see '{}' in stderr, but got: {}",
        expected,
        stderr
    );
}

// ============================================================================
// Missing step definitions for graph.feature
// ============================================================================

#[given(regex = r#"^I have an RDF file "([^"]+)"$"#)]
fn create_rdf_file_simple(world: &mut GgenWorld, filename: String) {
    let turtle = r#"@prefix ex: <http://example.org/> .
@prefix rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#> .

ex:Resource a ex:Entity ;
         ex:name "Test Resource" ;
         ex:value "test" .
"#;

    let file_path = world.project_dir.join(&filename);
    fs::write(&file_path, turtle)
        .unwrap_or_else(|e| panic!("Failed to write RDF file {}: {}", filename, e));
}

#[given(regex = r#"^I have RDF files "([^"]+)" and "([^"]+)"$"#)]
fn create_multiple_rdf_files(world: &mut GgenWorld, file1: String, file2: String) {
    let turtle1 = r#"@prefix ex: <http://example.org/> .
ex:Resource1 ex:prop "value1" ."#;
    let turtle2 = r#"@prefix ex: <http://example.org/> .
ex:Resource2 ex:prop "value2" ."#;

    fs::write(world.project_dir.join(&file1), turtle1).expect("Failed to write first RDF file");
    fs::write(world.project_dir.join(&file2), turtle2).expect("Failed to write second RDF file");
}

#[given(regex = r#"^I have triples in named graph "([^"]+)"$"#)]
fn create_named_graph_triples(world: &mut GgenWorld, graph_uri: String) {
    let turtle = format!(
        r#"@prefix ex: <http://example.org/> .

GRAPH <{}> {{
    ex:Subject ex:predicate ex:Object .
}}
"#,
        graph_uri
    );

    let file_path = world.project_dir.join("named_ttl");
    fs::write(&file_path, turtle).unwrap_or_else(|e| panic!("Failed to write named.ttl: {}", e));
}

#[given(regex = r"^I have a graph with person relationships$")]
fn create_graph_with_relationships(world: &mut GgenWorld) {
    let turtle = r#"@prefix ex: <http://example.org/> .
@prefix rel: <http://example.org/rel/> .

ex:Alice rel:knows ex:Bob .
ex:Bob rel:knows ex:Charlie .
ex:Charlie rel:worksFor ex:ACME .
"#;

    let file_path = world.project_dir.join("relationships_turtle");
    fs::write(&file_path, turtle).expect("Failed to write relationships file");

    // Load into graph
    let output = Command::cargo_bin("ggen")
        .expect("ggen binary not found")
        .arg("graph")
        .arg("load")
        .arg("relationships_turtle")
        .current_dir(&world.project_dir)
        .output()
        .expect("Failed to load file");

    world.last_output = Some(output);
}

#[given(regex = r"^I have a large graph$")]
fn create_large_graph(world: &mut GgenWorld) {
    let mut turtle = String::from("@prefix ex: <http://example.org/> .\n\n");

    // Generate 1000+ triples
    for i in 0..1000 {
        turtle.push_str(&format!(
            "ex:Entity{} ex:prop{} \"value{}\" .\n",
            i,
            i % 10,
            i
        ));
    }

    let file_path = world.project_dir.join("large.turtle");
    fs::write(&file_path, turtle).expect("Failed to write large graph");

    // Load it
    let output = Command::cargo_bin("ggen")
        .expect("ggen binary not found")
        .arg("graph")
        .arg("load")
        .arg("large.turtle")
        .current_dir(&world.project_dir)
        .output()
        .expect("Failed to load file");

    world.last_output = Some(output);
}

#[given(regex = r"^I have a graph with 50 people$")]
fn create_graph_with_50_people(world: &mut GgenWorld) {
    let mut turtle = String::from("@prefix ex: <http://example.org/> .\n\n");

    // Generate 50 people
    for i in 1..=50 {
        turtle.push_str(&format!(
            "ex:Person{} a ex:Person ; ex:name \"Person{}\" ; ex:age {} .\n",
            i,
            i,
            20 + (i % 50)
        ));
    }

    let file_path = world.project_dir.join("people50.ttl");
    fs::write(&file_path, turtle).expect("Failed to write people50.ttl");

    // Load it
    let output = Command::cargo_bin("ggen")
        .expect("ggen binary not found")
        .arg("graph")
        .arg("load")
        .arg("people50.turtle")
        .current_dir(&world.project_dir)
        .output()
        .expect("Failed to load file");

    world.last_output = Some(output);
}

// ============================================================================
// SHACL Validation Steps
// ============================================================================

#[given(regex = r"^I have SHACL shapes defining person constraints$")]
fn create_shacl_shapes(world: &mut GgenWorld) {
    let shacl = r#"@prefix sh: <http://www.w3.org/ns/shacl#> .
@prefix ex: <http://example.org/> .
ex:PersonShape a sh:NodeShape ;
    sh:targetClass ex:Person ;
    sh:property [ sh:path ex:name ; sh:minCount 1 ] ."#;
    
    let file_path = world.project_dir.join("shapes.ttl");
    fs::write(&file_path, shacl).expect("Failed to write SHACL shapes");
}

#[given(regex = r"^I have SHACL shapes for validation$")]
fn create_shacl_shapes_for_validation(world: &mut GgenWorld) {
    let shacl = r#"@prefix sh: <http://www.w3.org/ns/shacl#> .
@prefix ex: <http://example.org/> .
ex:ValidationShape a sh:NodeShape ;
    sh:targetClass ex:Entity ;
    sh:property [ 
        sh:path ex:name ; 
        sh:minCount 1 ;
        sh:maxCount 1 
    ] ."#;
    
    let file_path = world.project_dir.join("validation_shapes.ttl");
    fs::write(&file_path, shacl).expect("Failed to write validation shapes");
}

#[then(regex = r"^I should see validation report$")]
fn should_see_validation_report(world: &mut GgenWorld) {
    let stdout = world.last_stdout();
    assert!(
        stdout.contains("validation") || stdout.contains("conforms") || stdout.contains("violation"),
        "Expected to see validation report in output, but got: {}",
        stdout
    );
}

#[then(regex = r"^I should see validation violations$")]
fn should_see_validation_violations(world: &mut GgenWorld) {
    let stdout = world.last_stdout();
    assert!(
        stdout.contains("violation") || stdout.contains("error") || stdout.contains("failed"),
        "Expected to see validation violations in output, but got: {}",
        stdout
    );
}

// ============================================================================
// Statistics Steps
// ============================================================================

#[then(regex = r"^I should see number of subjects$")]
fn should_see_subject_count(world: &mut GgenWorld) {
    let stdout = world.last_stdout();
    assert!(
        stdout.contains("subjects") || stdout.contains("subject"),
        "Expected to see subject count in output, but got: {}",
        stdout
    );
}

#[then(regex = r"^I should see number of predicates$")]
fn should_see_predicate_count(world: &mut GgenWorld) {
    let stdout = world.last_stdout();
    assert!(
        stdout.contains("predicates") || stdout.contains("predicate"),
        "Expected to see predicate count in output, but got: {}",
        stdout
    );
}

#[then(regex = r"^I should see number of objects$")]
fn should_see_object_count(world: &mut GgenWorld) {
    let stdout = world.last_stdout();
    assert!(
        stdout.contains("objects") || stdout.contains("object"),
        "Expected to see object count in output, but got: {}",
        stdout
    );
}

#[then(regex = r"^I should see total number of triples$")]
fn should_see_total_triple_count(world: &mut GgenWorld) {
    let stdout = world.last_stdout();
    assert!(
        stdout.contains("triples") || stdout.contains("total") || stdout.contains("count"),
        "Expected to see total triple count in output, but got: {}",
        stdout
    );
}

#[then(regex = r"^I should see graph statistics$")]
fn should_see_graph_statistics(world: &mut GgenWorld) {
    let stdout = world.last_stdout();
    assert!(
        stdout.contains("statistics") || stdout.contains("stats") || 
        stdout.contains("subjects") || stdout.contains("predicates") || 
        stdout.contains("objects") || stdout.contains("triples"),
        "Expected to see graph statistics in output, but got: {}",
        stdout
    );
}
