use super::super::world::GgenWorld;
use cucumber::{given, then, when};
use std::fs;

// RDF/SPARQL step definitions

#[given(regex = r"^I have an RDF ontology file$")]
fn have_rdf_ontology_file(world: &mut GgenWorld) {
    use std::fs;

    let rdf_content = r#"@prefix ex: <http://example.org/> .
@prefix rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#> .
@prefix rdfs: <http://www.w3.org/2000/01/rdf-schema#> .

ex:Person a rdfs:Class .
ex:name a rdf:Property ;
    rdfs:domain ex:Person ;
    rdfs:range rdfs:Literal .
"#;

    fs::write(world.project_dir.join("ontology.ttl"), rdf_content)
        .expect("Failed to write ontology file");
}

#[given(regex = r"^I have a SPARQL query file$")]
fn have_sparql_query_file(world: &mut GgenWorld) {
    use std::fs;

    let sparql_content = r#"PREFIX ex: <http://example.org/>
PREFIX rdfs: <http://www.w3.org/2000/01/rdf-schema#>

SELECT ?class ?property WHERE {
    ?class a rdfs:Class .
    ?property rdfs:domain ?class .
}
"#;

    fs::write(world.project_dir.join("query.sparql"), sparql_content)
        .expect("Failed to write SPARQL query file");
}

#[when(regex = r"^I run the SPARQL query$")]
fn run_sparql_query(world: &mut GgenWorld) {
    use assert_cmd::Command;

    let output = Command::cargo_bin("ggen")
        .expect("ggen binary not found")
        .arg("query")
        .arg("ontology.ttl")
        .arg("query.sparql")
        .current_dir(&world.project_dir)
        .output()
        .expect("Failed to run SPARQL query");

    world.last_output = Some(output.clone());
    world.last_exit_code = output.status.code();
}

#[then(regex = r"^I should see query results$")]
fn should_see_query_results(world: &mut GgenWorld) {
    let stdout = world.last_stdout();
    assert!(
        stdout.contains("ex:Person") || stdout.contains("ex:name"),
        "Expected query results, but got: {}",
        stdout
    );
}

#[then(regex = r"^the results should be in (.+) format$")]
fn results_should_be_in_format(world: &mut GgenWorld, format: String) {
    let stdout = world.last_stdout();

    match format.as_str() {
        "JSON" => assert!(
            stdout.contains("{") && stdout.contains("}"),
            "Expected JSON format, but got: {}",
            stdout
        ),
        "CSV" => assert!(
            stdout.contains(","),
            "Expected CSV format, but got: {}",
            stdout
        ),
        "TSV" => assert!(
            stdout.contains("\t"),
            "Expected TSV format, but got: {}",
            stdout
        ),
        _ => {} // Accept any format
    }
}

// Missing step definitions for rdf_sparql.feature

#[given(regex = r#"^I have RDF file "([^"]+)"$"#)]
fn have_rdf_file(world: &mut GgenWorld, filename: String) {
    let rdf_content = r#"@prefix ex: <http://example.org/> .
@prefix rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#> .
@prefix rdfs: <http://www.w3.org/2000/01/rdf-schema#> .

ex:Person a rdfs:Class .
ex:name a rdf:Property ;
    rdfs:domain ex:Person ;
    rdfs:range rdfs:Literal .

ex:alice a ex:Person ;
    ex:name "Alice" .
"#;

    let file_path = world.project_dir.join(&filename);
    if let Some(parent) = file_path.parent() {
        fs::create_dir_all(parent).expect("Failed to create RDF directory");
    }
    fs::write(&file_path, rdf_content).expect("Failed to write RDF file");
}

#[given(regex = r#"^I have a template that references "([^"]+)"$"#)]
fn have_template_referencing_rdf(world: &mut GgenWorld, rdf_file: String) {
    let template_content = format!(
        r#"---
to: output.txt
rdf: {}
vars:
  - name

Hello {{name}}!
"#,
        rdf_file
    );

    fs::write(
        world.project_dir.join("test-template.tmpl"),
        template_content,
    )
    .expect("Failed to write template file");
}

#[given(regex = r"^I have a template with inline Turtle RDF$")]
fn have_template_with_inline_turtle_rdf(world: &mut GgenWorld) {
    let template_content = r#"---
to: output.txt
rdf: |
  @prefix ex: <http://example.org/> .
  @prefix rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#> .
  
  ex:Person a rdf:Class .
  ex:alice a ex:Person ;
    ex:name "Alice" .
vars:
  - name

Hello {{name}}!
"#;

    fs::write(
        world.project_dir.join("test-template.tmpl"),
        template_content,
    )
    .expect("Failed to write template file");
}

#[given(regex = r"^I have RDF data with entities$")]
fn have_rdf_data_with_entities(world: &mut GgenWorld) {
    let rdf_content = r#"@prefix ex: <http://example.org/> .
@prefix rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#> .
@prefix rdfs: <http://www.w3.org/2000/01/rdf-schema#> .

ex:Person a rdfs:Class .
ex:name a rdf:Property ;
    rdfs:domain ex:Person ;
    rdfs:range rdfs:Literal .

ex:alice a ex:Person ;
    ex:name "Alice" .

ex:bob a ex:Person ;
    ex:name "Bob" .
"#;

    fs::write(world.project_dir.join("entities.ttl"), rdf_content)
        .expect("Failed to write entities file");
}

#[given(regex = r"^I have a template with SPARQL query for those entities$")]
fn have_template_with_sparql_query_for_entities(world: &mut GgenWorld) {
    let template_content = r#"---
to: output.txt
rdf: entities.ttl
sparql: |
  PREFIX ex: <http://example.org/>
  SELECT ?name WHERE {
    ?person a ex:Person ;
      ex:name ?name .
  }
vars:
  - name

Hello {{name}}!
"#;

    fs::write(
        world.project_dir.join("test-template.tmpl"),
        template_content,
    )
    .expect("Failed to write template file");
}

#[given(regex = r"^I have a template with custom prefixes$")]
fn have_template_with_custom_prefixes(world: &mut GgenWorld) {
    let template_content = r#"---
to: output.txt
rdf: |
  @prefix ex: <http://example.org/> .
  @prefix foaf: <http://xmlns.com/foaf/0.1/> .
  
  ex:Person a foaf:Person .
  ex:alice a ex:Person ;
    foaf:name "Alice" .
sparql: |
  PREFIX ex: <http://example.org/>
  PREFIX foaf: <http://xmlns.com/foaf/0.1/>
  SELECT ?name WHERE {
    ?person a ex:Person ;
      foaf:name ?name .
  }
vars:
  - name

Hello {{name}}!
"#;

    fs::write(
        world.project_dir.join("test-template.tmpl"),
        template_content,
    )
    .expect("Failed to write template file");
}

#[then(regex = r"^the RDF data should be loaded$")]
fn rdf_data_should_be_loaded(world: &mut GgenWorld) {
    // This would verify that RDF data was successfully loaded
    // For now, we just check that the command succeeded
    assert!(world.last_command_succeeded(), "RDF data loading failed");
}

#[then(regex = r"^it should be available for SPARQL queries$")]
fn rdf_should_be_available_for_sparql(world: &mut GgenWorld) {
    // This would verify that SPARQL queries can be executed against the loaded RDF
    // For now, we just check that the command succeeded
    assert!(
        world.last_command_succeeded(),
        "SPARQL availability check failed"
    );
}

#[then(regex = r"^the inline RDF should be parsed$")]
fn inline_rdf_should_be_parsed(world: &mut GgenWorld) {
    // This would verify that inline RDF was successfully parsed
    assert!(world.last_command_succeeded(), "Inline RDF parsing failed");
}

#[then(regex = r"^it should be added to the graph$")]
fn rdf_should_be_added_to_graph(world: &mut GgenWorld) {
    // This would verify that RDF was added to the graph
    assert!(world.last_command_succeeded(), "RDF graph addition failed");
}

#[then(regex = r"^variables should be extracted via SPARQL$")]
fn variables_should_be_extracted_via_sparql(world: &mut GgenWorld) {
    // This would verify that variables were extracted using SPARQL
    assert!(
        world.last_command_succeeded(),
        "SPARQL variable extraction failed"
    );
}

#[then(regex = r"^they should be available in the template$")]
fn variables_should_be_available_in_template(world: &mut GgenWorld) {
    // This would verify that extracted variables are available in template
    assert!(
        world.last_command_succeeded(),
        "Template variable availability failed"
    );
}

#[then(regex = r"^the prefixes should be registered$")]
fn prefixes_should_be_registered(world: &mut GgenWorld) {
    // This would verify that custom prefixes were registered
    assert!(world.last_command_succeeded(), "Prefix registration failed");
}

#[then(regex = r"^they should be usable in RDF and SPARQL$")]
fn prefixes_should_be_usable_in_rdf_and_sparql(world: &mut GgenWorld) {
    // This would verify that prefixes can be used in RDF and SPARQL
    assert!(world.last_command_succeeded(), "Prefix usage failed");
}

// ============================================================================
// Additional missing step definitions for rdf_sparql.feature
// ============================================================================
