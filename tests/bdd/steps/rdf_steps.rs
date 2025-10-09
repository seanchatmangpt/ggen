use super::super::world::RgenWorld;
use cucumber::{given, then, when};

// RDF/SPARQL step definitions

#[given(regex = r"^I have an RDF ontology file$")]
fn have_rdf_ontology_file(world: &mut RgenWorld) {
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
fn have_sparql_query_file(world: &mut RgenWorld) {
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
fn run_sparql_query(world: &mut RgenWorld) {
    use assert_cmd::Command;

    let output = Command::cargo_bin("rgen")
        .expect("rgen binary not found")
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
fn should_see_query_results(world: &mut RgenWorld) {
    let stdout = world.last_stdout();
    assert!(
        stdout.contains("ex:Person") || stdout.contains("ex:name"),
        "Expected query results, but got: {}",
        stdout
    );
}

#[then(regex = r"^the results should be in (.+) format$")]
fn results_should_be_in_format(world: &mut RgenWorld, format: String) {
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
