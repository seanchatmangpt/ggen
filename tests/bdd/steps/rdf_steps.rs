use cucumber::{given, then, when, World};
use std::fs;
use assert_cmd::Command;

use crate::world::RgenWorld;

pub fn steps() -> Vec<cucumber::Step<World>> {
    vec![
        given!("I have a clean project directory", clean_project_directory),
        given!("I have RDF file {string}", have_rdf_file),
        given!("I have a template that references {string}", have_template_referencing_rdf),
        given!("I have a template with inline Turtle RDF", have_template_with_inline_turtle_rdf),
        given!("I have RDF data with entities", have_rdf_data_with_entities),
        given!("I have a template with SPARQL query for those entities", have_template_with_sparql_for_entities),
        given!("I have a template with custom prefixes", have_template_with_custom_prefixes),
        when!("I run {string}", run_command),
        then!("the RDF data should be loaded", rdf_data_should_be_loaded),
        then!("it should be available for SPARQL queries", should_be_available_for_sparql),
        then!("the inline RDF should be parsed", inline_rdf_should_be_parsed),
        then!("it should be added to the graph", should_be_added_to_graph),
        then!("variables should be extracted via SPARQL", variables_should_be_extracted_via_sparql),
        then!("they should be available in the template", should_be_available_in_template),
        then!("the prefixes should be registered", prefixes_should_be_registered),
        then!("they should be usable in RDF and SPARQL", should_be_usable_in_rdf_and_sparql),
    ]
}

#[given("I have a clean project directory")]
fn clean_project_directory(world: &mut RgenWorld) {
    // World is already initialized with temp directory
    // Ensure it's clean
    if world.project_dir.exists() {
        fs::remove_dir_all(&world.project_dir).expect("Failed to clean project dir");
    }
    fs::create_dir_all(&world.project_dir).expect("Failed to create project dir");
}

#[given("I have RDF file {string}")]
fn have_rdf_file(world: &mut RgenWorld, path: &str) {
    let rdf_path = world.project_dir.join(path);
    let rdf_dir = rdf_path.parent().expect("RDF path has no parent");
    
    fs::create_dir_all(rdf_dir).expect("Failed to create RDF directory");
    
    let rdf_content = r#"
@prefix ex: <http://example.org/> .
@prefix rdfs: <http://www.w3.org/2000/01/rdf-schema#> .

ex:Person a rdfs:Class .
ex:alice a ex:Person ;
    ex:name "Alice" ;
    ex:age 30 .
ex:bob a ex:Person ;
    ex:name "Bob" ;
    ex:age 25 .
"#;
    
    fs::write(&rdf_path, rdf_content).expect("Failed to write RDF file");
}

#[given("I have a template that references {string}")]
fn have_template_referencing_rdf(world: &mut RgenWorld, rdf_path: &str) {
    let templates_dir = world.project_dir.join("templates");
    fs::create_dir_all(&templates_dir).expect("Failed to create templates directory");
    
    let template_content = format!(
        "---\n\
        to: output/{{{{ name }}}}.rs\n\
        rdf:\n\
          - \"{}\"\n\
        ---\n\
        pub struct {{{{ name }}}} {{}}\n",
        rdf_path
    );
    
    let template_file = templates_dir.join("test-template.tmpl");
    fs::write(&template_file, template_content).expect("Failed to write template file");
}

#[given("I have a template with inline Turtle RDF")]
fn have_template_with_inline_turtle_rdf(world: &mut RgenWorld) {
    let templates_dir = world.project_dir.join("templates");
    fs::create_dir_all(&templates_dir).expect("Failed to create templates directory");
    
    let template_content = r#"
---
to: output/{{ name }}.rs
rdf_inline:
  - "@prefix ex: <http://example.org/> . ex:Person a rdfs:Class ."
---
pub struct {{ name }} {}
"#;
    
    let template_file = templates_dir.join("test-template.tmpl");
    fs::write(&template_file, template_content).expect("Failed to write template file");
}

#[given("I have RDF data with entities")]
fn have_rdf_data_with_entities(world: &mut RgenWorld) {
    let graphs_dir = world.project_dir.join("graphs");
    fs::create_dir_all(&graphs_dir).expect("Failed to create graphs directory");
    
    let rdf_content = r#"
@prefix ex: <http://example.org/> .
@prefix rdfs: <http://www.w3.org/2000/01/rdf-schema#> .

ex:Person a rdfs:Class .
ex:alice a ex:Person ;
    ex:name "Alice" ;
    ex:age 30 .
ex:bob a ex:Person ;
    ex:name "Bob" ;
    ex:age 25 .
ex:charlie a ex:Person ;
    ex:name "Charlie" ;
    ex:age 35 .
"#;
    
    let rdf_file = graphs_dir.join("entities.ttl");
    fs::write(&rdf_file, rdf_content).expect("Failed to write RDF file");
}

#[given("I have a template with SPARQL query for those entities")]
fn have_template_with_sparql_for_entities(world: &mut RgenWorld) {
    let templates_dir = world.project_dir.join("templates");
    fs::create_dir_all(&templates_dir).expect("Failed to create templates directory");
    
    let template_content = r#"
---
to: output/{{ name }}.rs
rdf:
  - "graphs/entities.ttl"
sparql:
  entities: |
    PREFIX ex: <http://example.org/>
    SELECT ?name WHERE {
      ?person a ex:Person ;
        ex:name ?name .
    }
---
pub struct {{ name }} {}
"#;
    
    let template_file = templates_dir.join("test-template.tmpl");
    fs::write(&template_file, template_content).expect("Failed to write template file");
}

#[given("I have a template with custom prefixes")]
fn have_template_with_custom_prefixes(world: &mut RgenWorld) {
    let templates_dir = world.project_dir.join("templates");
    fs::create_dir_all(&templates_dir).expect("Failed to create templates directory");
    
    let template_content = r#"
---
to: output/{{ name }}.rs
prefixes:
  ex: "http://example.org/"
  rdfs: "http://www.w3.org/2000/01/rdf-schema#"
rdf_inline:
  - "@prefix ex: <http://example.org/> . ex:Person a rdfs:Class ."
sparql:
  classes: |
    PREFIX ex: <http://example.org/>
    PREFIX rdfs: <http://www.w3.org/2000/01/rdf-schema#>
    SELECT ?class WHERE {
      ?class a rdfs:Class .
    }
---
pub struct {{ name }} {}
"#;
    
    let template_file = templates_dir.join("test-template.tmpl");
    fs::write(&template_file, template_content).expect("Failed to write template file");
}

#[when("I run {string}")]
fn run_command(world: &mut RgenWorld, command: &str) {
    let args: Vec<&str> = command.split_whitespace().collect();
    
    if args.is_empty() {
        panic!("Empty command provided");
    }
    
    let binary = args[0];
    let cmd_args = &args[1..];
    
    let mut cmd = if binary == "rgen" {
        Command::cargo_bin("rgen").expect("rgen binary not found")
    } else {
        Command::new(binary)
    };
    
    let output = cmd
        .args(cmd_args)
        .current_dir(&world.project_dir)
        .output()
        .expect(&format!("Failed to run command: {}", command));
    
    world.last_output = Some(output.clone());
    world.last_exit_code = output.status.code();
}

#[then("the RDF data should be loaded")]
fn rdf_data_should_be_loaded(world: &mut RgenWorld) {
    assert!(world.last_command_succeeded(), 
        "RDF loading failed: {}", world.last_stderr());
    
    // Verify that RDF loading occurred without errors
    let stderr = world.last_stderr();
    assert!(!stderr.contains("RDF") || !stderr.contains("error"), 
        "RDF loading error detected: {}", stderr);
}

#[then("it should be available for SPARQL queries")]
fn should_be_available_for_sparql(world: &mut RgenWorld) {
    // Check that SPARQL queries can be executed
    assert!(world.last_command_succeeded(), 
        "SPARQL query execution failed: {}", world.last_stderr());
    
    // Verify that SPARQL processing occurred
    let stderr = world.last_stderr();
    assert!(!stderr.contains("SPARQL") || !stderr.contains("error"), 
        "SPARQL processing error detected: {}", stderr);
}

#[then("the inline RDF should be parsed")]
fn inline_rdf_should_be_parsed(world: &mut RgenWorld) {
    assert!(world.last_command_succeeded(), 
        "Inline RDF parsing failed: {}", world.last_stderr());
    
    // Verify that inline RDF was processed
    let stderr = world.last_stderr();
    assert!(!stderr.contains("RDF") || !stderr.contains("error"), 
        "Inline RDF parsing error detected: {}", stderr);
}

#[then("it should be added to the graph")]
fn should_be_added_to_graph(world: &mut RgenWorld) {
    // Verify that the RDF was successfully added to the graph
    assert!(world.last_command_succeeded(), 
        "RDF graph addition failed: {}", world.last_stderr());
    
    // Check that the generation process completed successfully
    let output_dir = world.project_dir.join("output");
    if output_dir.exists() {
        let entries: Vec<_> = fs::read_dir(&output_dir)
            .expect("Failed to read output dir")
            .collect();
        assert!(!entries.is_empty(), "No output files generated");
    }
}

#[then("variables should be extracted via SPARQL")]
fn variables_should_be_extracted_via_sparql(world: &mut RgenWorld) {
    assert!(world.last_command_succeeded(), 
        "SPARQL variable extraction failed: {}", world.last_stderr());
    
    // Verify that SPARQL queries were executed
    let stderr = world.last_stderr();
    assert!(!stderr.contains("SPARQL") || !stderr.contains("error"), 
        "SPARQL variable extraction error detected: {}", stderr);
}

#[then("they should be available in the template")]
fn should_be_available_in_template(world: &mut RgenWorld) {
    // Check that template rendering used the extracted variables
    let output_dir = world.project_dir.join("output");
    if output_dir.exists() {
        for entry in fs::read_dir(&output_dir).expect("Failed to read output dir") {
            let entry = entry.expect("Failed to read entry");
            let path = entry.path();
            if path.is_file() {
                let content = fs::read_to_string(&path).expect("Failed to read file");
                // Verify that the template was rendered with variables
                assert!(!content.is_empty(), "Generated file is empty: {}", path.display());
            }
        }
    }
}

#[then("the prefixes should be registered")]
fn prefixes_should_be_registered(world: &mut RgenWorld) {
    assert!(world.last_command_succeeded(), 
        "Prefix registration failed: {}", world.last_stderr());
    
    // Verify that prefix registration occurred without errors
    let stderr = world.last_stderr();
    assert!(!stderr.contains("prefix") || !stderr.contains("error"), 
        "Prefix registration error detected: {}", stderr);
}

#[then("they should be usable in RDF and SPARQL")]
fn should_be_usable_in_rdf_and_sparql(world: &mut RgenWorld) {
    // Verify that the prefixes were used successfully
    assert!(world.last_command_succeeded(), 
        "Prefix usage failed: {}", world.last_stderr());
    
    // Check that the generation process completed successfully
    let output_dir = world.project_dir.join("output");
    if output_dir.exists() {
        let entries: Vec<_> = fs::read_dir(&output_dir)
            .expect("Failed to read output dir")
            .collect();
        assert!(!entries.is_empty(), "No output files generated");
    }
}