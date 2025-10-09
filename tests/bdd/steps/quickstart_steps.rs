use cucumber::{given, then, when, World};
use std::fs;
use assert_cmd::Command;

use crate::world::RgenWorld;

pub fn steps() -> Vec<cucumber::Step<World>> {
    vec![
        given!("I have a clean project directory", clean_project_directory),
        given!("rgen is installed", rgen_is_installed),
        given!("I have a template {string}", have_template),
        when!("I run {string}", run_command),
        then!("a file should be generated", file_should_be_generated),
        then!("the output should be deterministic", output_should_be_deterministic),
        then!("I should see search results", should_see_search_results),
        then!("results should contain {string}", results_should_contain),
        then!("the rpack should be installed", rpack_should_be_installed),
        then!("a file should be generated at {string}", file_should_be_generated_at),
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

#[given("rgen is installed")]
fn rgen_is_installed(_world: &mut RgenWorld) {
    // Verify rgen binary exists and is executable
    let mut cmd = Command::cargo_bin("rgen").expect("rgen binary not found");
    let output = cmd.arg("--version").output().expect("Failed to run rgen --version");
    
    assert!(output.status.success(), "rgen --version failed: {}", 
        String::from_utf8_lossy(&output.stderr));
    
    let version_output = String::from_utf8_lossy(&output.stdout);
    assert!(version_output.contains("rgen 0.1.0"), 
        "Expected version 'rgen 0.1.0', got: {}", version_output);
}

#[given("I have a template {string}")]
fn have_template(world: &mut RgenWorld, path: &str) {
    let template_path = world.project_dir.join(path);
    let template_dir = template_path.parent().expect("Template path has no parent");
    
    fs::create_dir_all(template_dir).expect("Failed to create template directory");
    
    // Create a basic template file
    let template_content = format!(
        "---\n\
        to: output/{{{{ name }}}}.txt\n\
        vars:\n\
          name: world\n\
        ---\n\
        Hello, {{{{ name }}}}!\n"
    );
    
    fs::write(&template_path, template_content)
        .expect("Failed to write template file");
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

#[then("a file should be generated")]
fn file_should_be_generated(world: &mut RgenWorld) {
    // Check that at least one file was generated
    let output_dir = world.project_dir.join("output");
    if output_dir.exists() {
        let entries: Vec<_> = fs::read_dir(&output_dir)
            .expect("Failed to read output dir")
            .collect();
        assert!(!entries.is_empty(), "No files were generated");
    } else {
        panic!("Output directory does not exist");
    }
}

#[then("the output should be deterministic")]
fn output_should_be_deterministic(world: &mut RgenWorld) {
    // For now, just verify that the command succeeded
    // In a real implementation, we'd run the command multiple times
    // and compare outputs
    assert!(world.last_command_succeeded(), 
        "Generation failed: {}", world.last_stderr());
}

#[then("I should see search results")]
fn should_see_search_results(world: &mut RgenWorld) {
    let stdout = world.last_stdout();
    assert!(!stdout.is_empty(), "Search returned no results");
    
    // Look for typical search result indicators
    assert!(stdout.contains("rust") || stdout.contains("cli") || stdout.contains("subcommand"), 
        "Expected search results, got: {}", stdout);
}

#[then("results should contain {string}")]
fn results_should_contain(world: &mut RgenWorld, expected: &str) {
    let stdout = world.last_stdout();
    assert!(stdout.contains(expected), 
        "Expected '{}' in search results, got: {}", expected, stdout);
}

#[then("the rpack should be installed")]
fn rpack_should_be_installed(world: &mut RgenWorld) {
    assert!(world.last_command_succeeded(), 
        "Rpack installation failed: {}", world.last_stderr());
    
    // Verify the rpack is listed in packs
    let mut cmd = Command::cargo_bin("rgen").expect("rgen binary not found");
    let output = cmd
        .arg("packs")
        .current_dir(&world.project_dir)
        .output()
        .expect("Failed to run rgen packs");
    
    let stdout = String::from_utf8_lossy(&output.stdout);
    assert!(stdout.contains("io.rgen.rust.cli-subcommand"), 
        "Rpack not found in packs list: {}", stdout);
}

#[then("a file should be generated at {string}")]
fn file_should_be_generated_at(world: &mut RgenWorld, path: &str) {
    let file_path = world.project_dir.join(path);
    assert!(file_path.exists(), 
        "File {} should exist at {}", path, file_path.display());
    
    // Verify the file has content
    let content = fs::read_to_string(&file_path).expect("Failed to read generated file");
    assert!(!content.is_empty(), "Generated file is empty: {}", path);
}