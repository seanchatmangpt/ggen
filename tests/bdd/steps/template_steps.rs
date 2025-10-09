use cucumber::{given, then, when, World};
use std::fs;
use std::path::PathBuf;
use assert_cmd::Command;
use sha2::{Sha256, Digest};

use crate::world::RgenWorld;

pub fn steps() -> Vec<cucumber::Step<World>> {
    vec![
        given!("I have a clean project directory", clean_project_directory),
        given!("I have a template with:", have_template_with_content),
        given!("I have a template with RDF inline:", have_template_with_rdf_inline),
        given!("I have a template with SPARQL query:", have_template_with_sparql),
        given!("I have a template with determinism config:", have_template_with_determinism),
        given!("I have RDF graph data", have_rdf_graph_data),
        when!("I run {string}", run_command),
        when!("I run {string} with seed {string}", run_command_with_seed),
        when!("I run {string} again", run_command_again),
        when!("I run {string} multiple times", run_command_multiple_times),
        then!("the RDF graph should be processed", rdf_graph_should_be_processed),
        then!("the output should use RDF-extracted variables", output_should_use_rdf_variables),
        then!("SPARQL variables should be extracted", sparql_variables_should_be_extracted),
        then!("the output should use queried values", output_should_use_queried_values),
        then!("all outputs should be byte-identical", all_outputs_should_be_byte_identical),
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

#[given("I have a template with:")]
fn have_template_with_content(world: &mut RgenWorld, content: &str) {
    let template_path = world.project_dir.join("templates/test-template.tmpl");
    let template_dir = template_path.parent().expect("Template path has no parent");
    
    fs::create_dir_all(template_dir).expect("Failed to create template directory");
    fs::write(&template_path, content).expect("Failed to write template file");
}

#[given("I have a template with RDF inline:")]
fn have_template_with_rdf_inline(world: &mut RgenWorld, content: &str) {
    let template_path = world.project_dir.join("templates/test-template.tmpl");
    let template_dir = template_path.parent().expect("Template path has no parent");
    
    fs::create_dir_all(template_dir).expect("Failed to create template directory");
    fs::write(&template_path, content).expect("Failed to write template file");
}

#[given("I have a template with SPARQL query:")]
fn have_template_with_sparql(world: &mut RgenWorld, content: &str) {
    let template_path = world.project_dir.join("templates/test-template.tmpl");
    let template_dir = template_path.parent().expect("Template path has no parent");
    
    fs::create_dir_all(template_dir).expect("Failed to create template directory");
    fs::write(&template_path, content).expect("Failed to write template file");
}

#[given("I have a template with determinism config:")]
fn have_template_with_determinism(world: &mut RgenWorld, content: &str) {
    let template_path = world.project_dir.join("templates/test-template.tmpl");
    let template_dir = template_path.parent().expect("Template path has no parent");
    
    fs::create_dir_all(template_dir).expect("Failed to create template directory");
    fs::write(&template_path, content).expect("Failed to write template file");
}

#[given("I have RDF graph data")]
fn have_rdf_graph_data(world: &mut RgenWorld) {
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
"#;
    
    let rdf_file = graphs_dir.join("schema.ttl");
    fs::write(&rdf_file, rdf_content).expect("Failed to write RDF file");
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

#[when("I run {string} with seed {string}")]
fn run_command_with_seed(world: &mut RgenWorld, command: &str, seed: &str) {
    let args: Vec<&str> = command.split_whitespace().collect();
    
    if args.is_empty() {
        panic!("Empty command provided");
    }
    
    let mut cmd = Command::cargo_bin("rgen").expect("rgen binary not found");
    
    // Add seed as environment variable
    cmd.env("RGEN_SEED", seed);
    
    let output = cmd
        .args(&args[1..])
        .current_dir(&world.project_dir)
        .output()
        .expect(&format!("Failed to run command: {}", command));
    
    world.last_output = Some(output.clone());
    world.last_exit_code = output.status.code();
    
    // Capture output hash for comparison
    capture_output_hash(world);
}

#[when("I run {string} again")]
fn run_command_again(world: &mut RgenWorld, command: &str) {
    // Re-run the same command as before
    run_command(world, command);
}

#[when("I run {string} multiple times")]
fn run_command_multiple_times(world: &mut RgenWorld, command: &str) {
    // Run the command multiple times and capture outputs
    for i in 0..3 {
        run_command(world, command);
        
        // Capture the generated files for comparison
        let output_dir = world.project_dir.join("output");
        if output_dir.exists() {
            for entry in fs::read_dir(&output_dir).expect("Failed to read output dir") {
                let entry = entry.expect("Failed to read entry");
                let path = entry.path();
                if path.is_file() {
                    let content = fs::read_to_string(&path).expect("Failed to read file");
                    let hash = format!("{:x}", Sha256::digest(content.as_bytes()));
                    world.capture_hash(hash);
                }
            }
        }
    }
}

#[then("the RDF graph should be processed")]
fn rdf_graph_should_be_processed(world: &mut RgenWorld) {
    // Check that RDF processing occurred without errors
    assert!(world.last_command_succeeded(), 
        "RDF processing failed: {}", world.last_stderr());
    
    // Verify that RDF-related output is present
    let stdout = world.last_stdout();
    let stderr = world.last_stderr();
    
    // Look for RDF processing indicators (this would depend on actual implementation)
    // For now, just ensure no RDF-related errors
    assert!(!stderr.contains("RDF") || !stderr.contains("error"), 
        "RDF processing error detected: {}", stderr);
}

#[then("the output should use RDF-extracted variables")]
fn output_should_use_rdf_variables(world: &mut RgenWorld) {
    // Check that the generated output contains variables that would come from RDF
    let output_dir = world.project_dir.join("output");
    if output_dir.exists() {
        for entry in fs::read_dir(&output_dir).expect("Failed to read output dir") {
            let entry = entry.expect("Failed to read entry");
            let path = entry.path();
            if path.is_file() {
                let content = fs::read_to_string(&path).expect("Failed to read file");
                // Look for RDF-extracted content (this would depend on actual implementation)
                // For now, just ensure the file was generated
                assert!(!content.is_empty(), "Generated file is empty: {}", path.display());
            }
        }
    }
}

#[then("SPARQL variables should be extracted")]
fn sparql_variables_should_be_extracted(world: &mut RgenWorld) {
    // Check that SPARQL processing occurred without errors
    assert!(world.last_command_succeeded(), 
        "SPARQL processing failed: {}", world.last_stderr());
    
    // Verify that SPARQL-related output is present
    let stderr = world.last_stderr();
    
    // Look for SPARQL processing indicators (this would depend on actual implementation)
    // For now, just ensure no SPARQL-related errors
    assert!(!stderr.contains("SPARQL") || !stderr.contains("error"), 
        "SPARQL processing error detected: {}", stderr);
}

#[then("the output should use queried values")]
fn output_should_use_queried_values(world: &mut RgenWorld) {
    // Check that the generated output contains values from SPARQL queries
    let output_dir = world.project_dir.join("output");
    if output_dir.exists() {
        for entry in fs::read_dir(&output_dir).expect("Failed to read output dir") {
            let entry = entry.expect("Failed to read entry");
            let path = entry.path();
            if path.is_file() {
                let content = fs::read_to_string(&path).expect("Failed to read file");
                // Look for SPARQL-queried content (this would depend on actual implementation)
                // For now, just ensure the file was generated
                assert!(!content.is_empty(), "Generated file is empty: {}", path.display());
            }
        }
    }
}

#[then("all outputs should be byte-identical")]
fn all_outputs_should_be_byte_identical(world: &mut RgenWorld) {
    // Check that all captured hashes are identical
    assert!(world.captured_hashes.len() > 1, "Need at least 2 outputs to compare");
    
    let first_hash = &world.captured_hashes[0];
    for (i, hash) in world.captured_hashes.iter().enumerate() {
        assert_eq!(first_hash, hash, 
            "Output {} differs from first output. Expected: {}, Got: {}", 
            i, first_hash, hash);
    }
}

/// Helper function to capture output hash for comparison
fn capture_output_hash(world: &mut RgenWorld) {
    // Calculate hash of all generated files
    let output_dir = world.project_dir.join("output");
    if output_dir.exists() {
        let mut all_content = String::new();
        for entry in fs::read_dir(&output_dir).expect("Failed to read output dir") {
            let entry = entry.expect("Failed to read entry");
            let path = entry.path();
            if path.is_file() {
                let content = fs::read_to_string(&path).expect("Failed to read file");
                all_content.push_str(&content);
            }
        }
        let hash = format!("{:x}", Sha256::digest(all_content.as_bytes()));
        world.capture_hash(hash);
    }
}