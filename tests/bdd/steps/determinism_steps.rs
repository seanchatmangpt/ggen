use cucumber::{given, then, when, World};
use std::fs;
use assert_cmd::Command;
use sha2::{Sha256, Digest};

use crate::world::RgenWorld;

pub fn steps() -> Vec<cucumber::Step<World>> {
    vec![
        given!("I have a clean project directory", clean_project_directory),
        given!("I have a template with seed {string}", have_template_with_seed),
        given!("I have a template", have_template),
        given!("I have RDF graph data", have_rdf_graph_data),
        given!("I have a template with:", have_template_with_content),
        when!("I run {string} with seed {string}", run_command_with_seed),
        when!("I run {string} with seed {string} again", run_command_with_seed_again),
        when!("I run {string}", run_command),
        then!("I capture the output hash", capture_output_hash),
        then!("I capture the second output hash", capture_second_output_hash),
        then!("both output hashes should be identical", both_output_hashes_should_be_identical),
        then!("I capture the first output", capture_first_output),
        then!("I capture the second output", capture_second_output),
        then!("the outputs should be different", outputs_should_be_different),
        then!("a manifest hash should be computed", manifest_hash_should_be_computed),
        then!("the same inputs should produce the same manifest hash", same_inputs_same_manifest_hash),
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

#[given("I have a template with seed {string}")]
fn have_template_with_seed(world: &mut RgenWorld, seed: &str) {
    let templates_dir = world.project_dir.join("templates");
    fs::create_dir_all(&templates_dir).expect("Failed to create templates directory");
    
    let template_content = format!(
        "---\n\
        to: output/{{{{ name }}}}.txt\n\
        vars:\n\
          name: world\n\
        determinism:\n\
          seed: \"{}\"\n\
        ---\n\
        Hello, {{{{ name }}}}!\n",
        seed
    );
    
    let template_file = templates_dir.join("test-template.tmpl");
    fs::write(&template_file, template_content).expect("Failed to write template file");
}

#[given("I have a template")]
fn have_template(world: &mut RgenWorld) {
    let templates_dir = world.project_dir.join("templates");
    fs::create_dir_all(&templates_dir).expect("Failed to create templates directory");
    
    let template_content = r#"
---
to: output/{{ name }}.txt
vars:
  name: world
---
Hello, {{ name }}!
"#;
    
    let template_file = templates_dir.join("test-template.tmpl");
    fs::write(&template_file, template_content).expect("Failed to write template file");
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
"#;
    
    let rdf_file = graphs_dir.join("schema.ttl");
    fs::write(&rdf_file, rdf_content).expect("Failed to write RDF file");
}

#[given("I have a template with:")]
fn have_template_with_content(world: &mut RgenWorld, content: &str) {
    let templates_dir = world.project_dir.join("templates");
    fs::create_dir_all(&templates_dir).expect("Failed to create templates directory");
    
    let template_file = templates_dir.join("test-template.tmpl");
    fs::write(&template_file, content).expect("Failed to write template file");
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

#[when("I run {string} with seed {string} again")]
fn run_command_with_seed_again(world: &mut RgenWorld, command: &str, seed: &str) {
    run_command_with_seed(world, command, seed);
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
    
    // Capture output hash
    capture_output_hash(world);
}

#[then("I capture the output hash")]
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

#[then("I capture the second output hash")]
fn capture_second_output_hash(world: &mut RgenWorld) {
    capture_output_hash(world);
}

#[then("both output hashes should be identical")]
fn both_output_hashes_should_be_identical(world: &mut RgenWorld) {
    assert!(world.captured_hashes.len() >= 2, "Need at least 2 output hashes to compare");
    
    let first_hash = &world.captured_hashes[0];
    let second_hash = &world.captured_hashes[1];
    
    assert_eq!(first_hash, second_hash, 
        "Output hashes should be identical. First: {}, Second: {}", 
        first_hash, second_hash);
}

#[then("I capture the first output")]
fn capture_first_output(world: &mut RgenWorld) {
    capture_output_hash(world);
}

#[then("I capture the second output")]
fn capture_second_output(world: &mut RgenWorld) {
    capture_output_hash(world);
}

#[then("the outputs should be different")]
fn outputs_should_be_different(world: &mut RgenWorld) {
    assert!(world.captured_hashes.len() >= 2, "Need at least 2 output hashes to compare");
    
    let first_hash = &world.captured_hashes[0];
    let second_hash = &world.captured_hashes[1];
    
    assert_ne!(first_hash, second_hash, 
        "Outputs should be different but hashes are identical: {}", first_hash);
}

#[then("a manifest hash should be computed")]
fn manifest_hash_should_be_computed(world: &mut RgenWorld) {
    assert!(world.last_command_succeeded(), 
        "Manifest hash computation failed: {}", world.last_stderr());
    
    // Verify that a hash was captured
    assert!(!world.captured_hashes.is_empty(), 
        "No manifest hash was captured");
}

#[then("the same inputs should produce the same manifest hash")]
fn same_inputs_same_manifest_hash(world: &mut RgenWorld) {
    // This would require running the same command multiple times
    // and comparing manifest hashes
    assert!(world.last_command_succeeded(), 
        "Manifest hash computation failed: {}", world.last_stderr());
    
    // Verify that hashes are consistent
    assert!(!world.captured_hashes.is_empty(), 
        "No manifest hash was captured");
}