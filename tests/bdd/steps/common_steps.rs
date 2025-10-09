use cucumber::{given, then, when};
use std::fs;
use assert_cmd::Command;

use super::super::world::RgenWorld;

/// Common step definitions used across multiple features
/// 
/// These steps provide basic functionality for:
/// - Setting up clean project directories
/// - Running rgen commands
/// - Basic file operations
/// - Command output validation

#[given(regex = r"^I have a clean project directory$")]
fn clean_project_directory(world: &mut RgenWorld) {
    // World is already initialized with temp directory
    // Ensure it's clean
    if world.project_dir.exists() {
        fs::remove_dir_all(&world.project_dir).expect("Failed to clean project dir");
    }
    fs::create_dir_all(&world.project_dir).expect("Failed to create project dir");
}

#[given(regex = r"^rgen is installed$")]
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

#[when(regex = r"^I run (.+)$")]
fn run_command(world: &mut RgenWorld, command: String) {
    let args: Vec<&str> = command.split_whitespace().collect();
    
    if args.is_empty() {
        panic!("Empty command provided");
    }
    
    let binary = args[0];
    let cmd_args = &args[1..];
    
    let mut cmd = if binary == "rgen" {
        Command::cargo_bin("rgen").expect("rgen binary not found")
    } else if binary == "cargo" {
        Command::new("cargo")
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

#[then(regex = r"^the command should succeed$")]
fn command_should_succeed(world: &mut RgenWorld) {
    assert!(world.last_command_succeeded(), 
        "Command failed with exit code: {}\nStderr: {}", 
        world.last_exit_code.unwrap_or(-1),
        world.last_stderr());
}

#[then(regex = r"^I should see (.+)$")]
fn should_see_text(world: &mut RgenWorld, expected: String) {
    let stdout = world.last_stdout();
    let stderr = world.last_stderr();
    
    assert!(stdout.contains(&expected) || stderr.contains(&expected), 
        "Expected to see '{}' in output, but got:\nStdout: {}\nStderr: {}", 
        expected, stdout, stderr);
}

#[then(regex = r"^the file (.+) should exist$")]
fn file_should_exist(world: &mut RgenWorld, path: String) {
    let file_path = world.project_dir.join(&path);
    assert!(file_path.exists(), 
        "File {} should exist at {}", path, file_path.display());
}