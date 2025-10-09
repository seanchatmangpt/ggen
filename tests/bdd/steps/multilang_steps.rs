use cucumber::{given, then, when, World};
use std::fs;
use assert_cmd::Command;

use crate::world::RgenWorld;

pub fn steps() -> Vec<cucumber::Step<World>> {
    vec![
        given!("I have a clean project directory", clean_project_directory),
        when!("I run {string}", run_command),
        then!("the file {string} should exist", file_should_exist),
        given!("I have templates for Rust, Python, and Bash", have_multilang_templates),
        then!("files should be generated for all languages", files_generated_for_all_languages),
        then!("all files should derive from the same ontology", all_files_derive_from_same_ontology),
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

#[then("the file {string} should exist")]
fn file_should_exist(world: &mut RgenWorld, path: &str) {
    let file_path = world.project_dir.join(path);
    assert!(file_path.exists(), 
        "File {} should exist at {}", path, file_path.display());
}

#[given("I have templates for Rust, Python, and Bash")]
fn have_multilang_templates(world: &mut RgenWorld) {
    let templates_dir = world.project_dir.join("templates").join("cli").join("subcommand");
    fs::create_dir_all(&templates_dir).expect("Failed to create templates directory");
    
    // Rust template
    let rust_template = r#"
---
to: src/cmds/{{ cmd }}.rs
vars:
  cmd: hello
---
pub fn {{ cmd }}(name: &str) {
    println!("Hello, {}!", name);
}
"#;
    fs::write(templates_dir.join("rust.tmpl"), rust_template)
        .expect("Failed to write Rust template");
    
    // Python template
    let python_template = r#"
---
to: commands/{{ cmd }}.py
vars:
  cmd: hello
---
def {{ cmd }}(name: str):
    print(f"Hello, {name}!")
"#;
    fs::write(templates_dir.join("python.tmpl"), python_template)
        .expect("Failed to write Python template");
    
    // Bash template
    let bash_template = r#"
---
to: commands/{{ cmd }}.sh
vars:
  cmd: hello
---
#!/bin/bash
{{ cmd }}() {
    echo "Hello, $1!"
}
"#;
    fs::write(templates_dir.join("bash.tmpl"), bash_template)
        .expect("Failed to write Bash template");
}

#[then("files should be generated for all languages")]
fn files_generated_for_all_languages(world: &mut RgenWorld) {
    // Check for Rust file
    let rust_file = world.project_dir.join("src").join("cmds").join("status.rs");
    assert!(rust_file.exists(), "Rust file should exist: {}", rust_file.display());
    
    // Check for Python file
    let python_file = world.project_dir.join("commands").join("status.py");
    assert!(python_file.exists(), "Python file should exist: {}", python_file.display());
    
    // Check for Bash file
    let bash_file = world.project_dir.join("commands").join("status.sh");
    assert!(bash_file.exists(), "Bash file should exist: {}", bash_file.display());
}

#[then("all files should derive from the same ontology")]
fn all_files_derive_from_same_ontology(world: &mut RgenWorld) {
    // Read all generated files and verify they contain the same base information
    let rust_file = world.project_dir.join("src").join("cmds").join("status.rs");
    let python_file = world.project_dir.join("commands").join("status.py");
    let bash_file = world.project_dir.join("commands").join("status.sh");
    
    let rust_content = fs::read_to_string(&rust_file).expect("Failed to read Rust file");
    let python_content = fs::read_to_string(&python_file).expect("Failed to read Python file");
    let bash_content = fs::read_to_string(&bash_file).expect("Failed to read Bash file");
    
    // All files should contain the same command name
    assert!(rust_content.contains("status"), "Rust file should contain 'status'");
    assert!(python_content.contains("status"), "Python file should contain 'status'");
    assert!(bash_content.contains("status"), "Bash file should contain 'status'");
    
    // All files should contain similar functionality (greeting)
    assert!(rust_content.contains("Hello") || rust_content.contains("hello"), 
        "Rust file should contain greeting");
    assert!(python_content.contains("Hello") || python_content.contains("hello"), 
        "Python file should contain greeting");
    assert!(bash_content.contains("Hello") || bash_content.contains("hello"), 
        "Bash file should contain greeting");
}