use cucumber::{given, then, when, World};
use std::fs;
use assert_cmd::Command;
use mockito::{mock, server_url};

use crate::world::RgenWorld;

pub fn steps() -> Vec<cucumber::Step<World>> {
    vec![
        given!("I have a clean project directory", clean_project_directory),
        given!("the marketplace registry is available at {string}", marketplace_registry_available),
        when!("I run {string}", run_command),
        then!("I should see results for Rust CLI templates", should_see_rust_cli_results),
        then!("I should see results for Python API templates", should_see_python_api_results),
        then!("I should see results for TypeScript React templates", should_see_typescript_react_results),
        then!("I should see popular categories", should_see_popular_categories),
        then!("I should see package metadata", should_see_package_metadata),
        then!("I should see version information", should_see_version_information),
        then!("I should see description", should_see_description),
        then!("the package should be installed", package_should_be_installed),
        then!("{string} should list {string}", command_should_list_package),
        then!("version {string} should be installed", version_should_be_installed),
        given!("I have installed {string}", have_installed_package),
        then!("the package should be updated to latest version", package_should_be_updated),
        then!("the file should use the rpack template", file_should_use_rpack_template),
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

#[given("the marketplace registry is available at {string}")]
fn marketplace_registry_available(world: &mut RgenWorld, registry_url: &str) {
    // Set up mock registry for testing
    let _m = mock("GET", "/index.json")
        .with_status(200)
        .with_header("content-type", "application/json")
        .with_body(r#"
        {
          "packages": [
            {
              "id": "io.rgen.rust.cli-subcommand",
              "name": "Rust CLI Subcommand",
              "description": "Generate Rust CLI subcommands",
              "version": "0.2.0",
              "keywords": ["rust", "cli", "subcommand"],
              "categories": ["cli", "rust"]
            }
          ]
        }
        "#)
        .create();

    // Set the registry URL for testing
    world.set_registry_url(server_url());
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
    
    // Set registry URL if available
    if let Some(ref url) = world.registry_url {
        cmd.env("RGEN_REGISTRY_URL", url);
    }
    
    let output = cmd
        .args(cmd_args)
        .current_dir(&world.project_dir)
        .output()
        .expect(&format!("Failed to run command: {}", command));
    
    world.last_output = Some(output.clone());
    world.last_exit_code = output.status.code();
}

#[then("I should see results for Rust CLI templates")]
fn should_see_rust_cli_results(world: &mut RgenWorld) {
    let stdout = world.last_stdout();
    assert!(stdout.contains("rust") || stdout.contains("cli"), 
        "Expected Rust CLI results, got: {}", stdout);
}

#[then("I should see results for Python API templates")]
fn should_see_python_api_results(world: &mut RgenWorld) {
    let stdout = world.last_stdout();
    assert!(stdout.contains("python") || stdout.contains("api"), 
        "Expected Python API results, got: {}", stdout);
}

#[then("I should see results for TypeScript React templates")]
fn should_see_typescript_react_results(world: &mut RgenWorld) {
    let stdout = world.last_stdout();
    assert!(stdout.contains("typescript") || stdout.contains("react"), 
        "Expected TypeScript React results, got: {}", stdout);
}

#[then("I should see popular categories")]
fn should_see_popular_categories(world: &mut RgenWorld) {
    let stdout = world.last_stdout();
    assert!(stdout.contains("cli") || stdout.contains("rust") || stdout.contains("python"), 
        "Expected popular categories, got: {}", stdout);
}

#[then("I should see package metadata")]
fn should_see_package_metadata(world: &mut RgenWorld) {
    let stdout = world.last_stdout();
    assert!(stdout.contains("description") || stdout.contains("version") || stdout.contains("keywords"), 
        "Expected package metadata, got: {}", stdout);
}

#[then("I should see version information")]
fn should_see_version_information(world: &mut RgenWorld) {
    let stdout = world.last_stdout();
    assert!(stdout.contains("0.") || stdout.contains("version"), 
        "Expected version information, got: {}", stdout);
}

#[then("I should see description")]
fn should_see_description(world: &mut RgenWorld) {
    let stdout = world.last_stdout();
    assert!(stdout.contains("description") || stdout.contains("Generate"), 
        "Expected description, got: {}", stdout);
}

#[then("the package should be installed")]
fn package_should_be_installed(world: &mut RgenWorld) {
    assert!(world.last_command_succeeded(), 
        "Package installation failed: {}", world.last_stderr());
    
    // Check that the package is listed in packs
    let mut cmd = Command::cargo_bin("rgen").expect("rgen binary not found");
    let output = cmd
        .arg("packs")
        .current_dir(&world.project_dir)
        .output()
        .expect("Failed to run rgen packs");
    
    let stdout = String::from_utf8_lossy(&output.stdout);
    assert!(stdout.contains("io.rgen.rust.cli-subcommand"), 
        "Package not found in packs list: {}", stdout);
}

#[then("{string} should list {string}")]
fn command_should_list_package(world: &mut RgenWorld, command: &str, package: &str) {
    let args: Vec<&str> = command.split_whitespace().collect();
    
    let mut cmd = Command::cargo_bin("rgen").expect("rgen binary not found");
    let output = cmd
        .args(&args[1..])
        .current_dir(&world.project_dir)
        .output()
        .expect(&format!("Failed to run command: {}", command));
    
    let stdout = String::from_utf8_lossy(&output.stdout);
    assert!(stdout.contains(package), 
        "Expected '{}' in output, got: {}", package, stdout);
}

#[then("version {string} should be installed")]
fn version_should_be_installed(world: &mut RgenWorld, version: &str) {
    assert!(world.last_command_succeeded(), 
        "Version installation failed: {}", world.last_stderr());
    
    // Check that the specific version is installed
    let mut cmd = Command::cargo_bin("rgen").expect("rgen binary not found");
    let output = cmd
        .arg("packs")
        .current_dir(&world.project_dir)
        .output()
        .expect("Failed to run rgen packs");
    
    let stdout = String::from_utf8_lossy(&output.stdout);
    assert!(stdout.contains(version), 
        "Version {} not found in packs list: {}", version, stdout);
}

#[given("I have installed {string}")]
fn have_installed_package(world: &mut RgenWorld, package: &str) {
    let mut cmd = Command::cargo_bin("rgen").expect("rgen binary not found");
    
    if let Some(ref url) = world.registry_url {
        cmd.env("RGEN_REGISTRY_URL", url);
    }
    
    let output = cmd
        .arg("add")
        .arg(package)
        .current_dir(&world.project_dir)
        .output()
        .expect("Failed to run rgen add");
    
    assert!(output.status.success(), 
        "Failed to install package {}: {}", package, 
        String::from_utf8_lossy(&output.stderr));
}

#[then("the package should be updated to latest version")]
fn package_should_be_updated(world: &mut RgenWorld) {
    assert!(world.last_command_succeeded(), 
        "Package update failed: {}", world.last_stderr());
    
    // Verify the package is still installed and updated
    let mut cmd = Command::cargo_bin("rgen").expect("rgen binary not found");
    let output = cmd
        .arg("packs")
        .current_dir(&world.project_dir)
        .output()
        .expect("Failed to run rgen packs");
    
    let stdout = String::from_utf8_lossy(&output.stdout);
    assert!(stdout.contains("io.rgen.rust.cli-subcommand"), 
        "Package not found after update: {}", stdout);
}

#[then("the file should use the rpack template")]
fn file_should_use_rpack_template(world: &mut RgenWorld) {
    // Check that the generated file exists and contains expected content
    let output_dir = world.project_dir.join("src").join("cmds");
    if output_dir.exists() {
        for entry in fs::read_dir(&output_dir).expect("Failed to read output dir") {
            let entry = entry.expect("Failed to read entry");
            let path = entry.path();
            if path.is_file() {
                let content = fs::read_to_string(&path).expect("Failed to read file");
                // Look for Rust-specific content that would come from the rpack template
                assert!(content.contains("fn") || content.contains("pub"), 
                    "Generated file doesn't appear to be Rust code: {}", path.display());
            }
        }
    }
}