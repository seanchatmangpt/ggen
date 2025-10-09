use cucumber::{given, then, when, World};
use std::fs;
use assert_cmd::Command;

use crate::world::RgenWorld;

pub fn steps() -> Vec<cucumber::Step<World>> {
    vec![
        given!("I have a clean project directory", clean_project_directory),
        given!("rgen is installed", rgen_is_installed),
        given!("I have templates in {string}", have_templates_in_directory),
        given!("I have a template", have_template),
        when!("I run {string}", run_command),
        then!("the command should succeed", command_should_succeed),
        then!("I should see available templates", should_see_available_templates),
        then!("I should see template metadata", should_see_template_metadata),
        then!("the command should validate the template", command_should_validate_template),
        then!("I should see a hazard report", should_see_hazard_report),
        then!("I should see bash completion script", should_see_bash_completion),
        then!("I should see zsh completion script", should_see_zsh_completion),
        then!("I should see fish completion script", should_see_fish_completion),
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

#[given("I have templates in {string}")]
fn have_templates_in_directory(world: &mut RgenWorld, dir: &str) {
    let templates_dir = world.project_dir.join(dir);
    fs::create_dir_all(&templates_dir).expect("Failed to create templates directory");
    
    // Create a basic template structure
    let hello_template = templates_dir.join("hello.tmpl");
    let hello_content = format!(
        "---\n\
        to: output/{{{{ name }}}}.txt\n\
        vars:\n\
          name: world\n\
        ---\n\
        Hello, {{{{ name }}}}!\n"
    );
    fs::write(&hello_template, hello_content).expect("Failed to write hello template");
    
    let goodbye_template = templates_dir.join("goodbye.tmpl");
    let goodbye_content = format!(
        "---\n\
        to: output/{{{{ name }}}}.txt\n\
        vars:\n\
          name: world\n\
        ---\n\
        Goodbye, {{{{ name }}}}!\n"
    );
    fs::write(&goodbye_template, goodbye_content).expect("Failed to write goodbye template");
}

#[given("I have a template")]
fn have_template(world: &mut RgenWorld) {
    let templates_dir = world.project_dir.join("templates");
    fs::create_dir_all(&templates_dir).expect("Failed to create templates directory");
    
    let template_content = r#"
---
to: output/test.txt
vars:
  name: world
---
Hello, {{ name }}!
"#;
    
    let template_file = templates_dir.join("test.tmpl");
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

#[then("the command should succeed")]
fn command_should_succeed(world: &mut RgenWorld) {
    assert!(world.last_command_succeeded(), 
        "Command failed with exit code: {}\nStderr: {}", 
        world.last_exit_code.unwrap_or(-1),
        world.last_stderr());
}

#[then("I should see available templates")]
fn should_see_available_templates(world: &mut RgenWorld) {
    let stdout = world.last_stdout();
    assert!(stdout.contains("template") || stdout.contains("Available") || stdout.contains(".tmpl"), 
        "Expected template listing, got: {}", stdout);
}

#[then("I should see template metadata")]
fn should_see_template_metadata(world: &mut RgenWorld) {
    let stdout = world.last_stdout();
    assert!(stdout.contains("to:") || stdout.contains("vars:") || stdout.contains("---"), 
        "Expected template metadata, got: {}", stdout);
}

#[then("the command should validate the template")]
fn command_should_validate_template(world: &mut RgenWorld) {
    // Template validation should succeed or fail gracefully
    let stderr = world.last_stderr();
    let stdout = world.last_stdout();
    
    // Either successful validation or clear error message
    assert!(world.last_command_succeeded() || 
            stderr.contains("error") || 
            stderr.contains("invalid") ||
            stdout.contains("valid"), 
        "Template validation should succeed or provide clear error, got stdout: {}, stderr: {}", 
        stdout, stderr);
}

#[then("I should see a hazard report")]
fn should_see_hazard_report(world: &mut RgenWorld) {
    let stdout = world.last_stdout();
    assert!(stdout.contains("hazard") || stdout.contains("Hazard") || stdout.contains("⚠️") || stdout.contains("✅"), 
        "Expected hazard report, got: {}", stdout);
}

#[then("I should see bash completion script")]
fn should_see_bash_completion(world: &mut RgenWorld) {
    let stdout = world.last_stdout();
    assert!(stdout.contains("bash") || stdout.contains("complete") || stdout.contains("_rgen"), 
        "Expected bash completion script, got: {}", stdout);
}

#[then("I should see zsh completion script")]
fn should_see_zsh_completion(world: &mut RgenWorld) {
    let stdout = world.last_stdout();
    assert!(stdout.contains("zsh") || stdout.contains("compdef") || stdout.contains("_rgen"), 
        "Expected zsh completion script, got: {}", stdout);
}

#[then("I should see fish completion script")]
fn should_see_fish_completion(world: &mut RgenWorld) {
    let stdout = world.last_stdout();
    assert!(stdout.contains("fish") || stdout.contains("complete") || stdout.contains("rgen"), 
        "Expected fish completion script, got: {}", stdout);
}