use super::super::world::CleanroomWorld;
use assert_cmd::Command;
use cucumber::{given, then, when};
use std::fs;
use std::path::Path;

/// Common step definitions used across multiple features
///
/// These steps provide basic setup and validation functionality
/// that is shared between different cleanroom test scenarios.

// ============================================================================
// GIVEN steps - Setup preconditions
// ============================================================================

#[given(regex = r"^I have a clean test environment$")]
fn clean_test_environment(world: &mut CleanroomWorld) {
    // Ensure we're in a clean temporary directory
    assert!(world.project_dir.exists(), "Project directory should exist");
    
    // Clear any existing files
    if world.project_dir.exists() {
        for entry in fs::read_dir(&world.project_dir).unwrap() {
            let entry = entry.unwrap();
            if entry.file_type().unwrap().is_file() {
                fs::remove_file(entry.path()).unwrap();
            }
        }
    }
}

#[given(regex = r"^I have a file "([^"]+)" with content:$")]
fn create_file_with_content(world: &mut CleanroomWorld, filename: String, content: String) {
    let file_path = world.project_dir.join(&filename);
    
    // Create parent directories if needed
    if let Some(parent) = file_path.parent() {
        fs::create_dir_all(parent).expect("Failed to create parent directories");
    }
    
    fs::write(&file_path, content.trim())
        .unwrap_or_else(|e| panic!("Failed to write file {}: {}", filename, e));
    
    // Capture the file content for later verification
    world.capture_file(&filename, content.trim().to_string());
}

#[given(regex = r"^I have a binary file "([^"]+)" with (\d+) bytes$")]
fn create_binary_file(world: &mut CleanroomWorld, filename: String, size: usize) {
    let file_path = world.project_dir.join(&filename);
    
    // Create parent directories if needed
    if let Some(parent) = file_path.parent() {
        fs::create_dir_all(parent).expect("Failed to create parent directories");
    }
    
    // Generate random binary data
    let data: Vec<u8> = (0..size).map(|i| (i % 256) as u8).collect();
    
    fs::write(&file_path, &data)
        .unwrap_or_else(|e| panic!("Failed to write binary file {}: {}", filename, e));
    
    // Capture the binary artifact
    world.capture_artifact(&filename, data);
}

#[given(regex = r"^I have a directory "([^"]+)"$")]
fn create_directory(world: &mut CleanroomWorld, dirname: String) {
    let dir_path = world.project_dir.join(&dirname);
    fs::create_dir_all(&dir_path)
        .unwrap_or_else(|e| panic!("Failed to create directory {}: {}", dirname, e));
}

// ============================================================================
// WHEN steps - Execute actions
// ============================================================================

#[when(regex = r"^I run "([^"]+)"$")]
fn run_command(world: &mut CleanroomWorld, command: String) {
    let args: Vec<&str> = command.split_whitespace().collect();
    
    if args.is_empty() {
        panic!("Empty command provided");
    }
    
    let mut cmd = Command::new(args[0]);
    
    // Add arguments if any
    if args.len() > 1 {
        cmd.args(&args[1..]);
    }
    
    // Set working directory
    cmd.current_dir(&world.project_dir);
    
    // Execute command
    let output = cmd.output()
        .unwrap_or_else(|e| panic!("Failed to run command '{}': {}", command, e));
    
    // Store output for later verification
    world.last_output = Some(output.clone());
    world.last_exit_code = output.status.code();
}

#[when(regex = r"^I run cleanroom "([^"]+)"$")]
fn run_cleanroom_command(world: &mut CleanroomWorld, args: String) {
    // Parse arguments using shell-words for proper handling of quoted strings
    let arg_list = shell_words::split(&args)
        .unwrap_or_else(|e| panic!("Failed to parse arguments: {}", e));
    
    let mut cmd = Command::cargo_bin("cleanroom")
        .expect("cleanroom binary not found - run 'cargo make build' first");
    
    cmd.args(&arg_list)
        .current_dir(&world.project_dir);
    
    let output = cmd.output()
        .unwrap_or_else(|e| panic!("Failed to run cleanroom command: {}", e));
    
    world.last_output = Some(output.clone());
    world.last_exit_code = output.status.code();
}

// ============================================================================
// THEN steps - Verify outcomes
// ============================================================================

#[then(regex = r"^the command should succeed$")]
fn command_should_succeed(world: &mut CleanroomWorld) {
    if !world.last_command_succeeded() {
        // Debug information on failure
        eprintln!("=== COMMAND FAILED ===");
        eprintln!("Exit code: {}", world.last_exit_code.unwrap_or(-1));
        eprintln!("Stdout:\n{}", world.last_stdout());
        eprintln!("Stderr:\n{}", world.last_stderr());
        eprintln!("Working dir: {}", world.project_dir.display());
        eprintln!("Files present:");
        for entry in fs::read_dir(&world.project_dir).unwrap() {
            eprintln!("  - {}", entry.unwrap().path().display());
        }
        panic!("Command should have succeeded but failed");
    }
}

#[then(regex = r"^the command should fail$")]
fn command_should_fail(world: &mut CleanroomWorld) {
    if world.last_command_succeeded() {
        eprintln!("=== UNEXPECTED SUCCESS ===");
        eprintln!("Exit code: {}", world.last_exit_code.unwrap_or(0));
        eprintln!("Stdout:\n{}", world.last_stdout());
        eprintln!("Stderr:\n{}", world.last_stderr());
        panic!("Command should have failed but succeeded");
    }
}

#[then(regex = r"^I should see "([^"]+)" in output$")]
fn should_see_in_output(world: &mut CleanroomWorld, expected: String) {
    let stdout = world.last_stdout();
    let stderr = world.last_stderr();
    
    if !stdout.contains(&expected) && !stderr.contains(&expected) {
        eprintln!("=== EXPECTED TEXT NOT FOUND ===");
        eprintln!("Looking for: '{}'", expected);
        eprintln!("Stdout:\n{}", stdout);
        eprintln!("Stderr:\n{}", stderr);
        panic!("Expected to see '{}' in output", expected);
    }
}

#[then(regex = r"^I should not see "([^"]+)" in output$")]
fn should_not_see_in_output(world: &mut CleanroomWorld, unexpected: String) {
    let stdout = world.last_stdout();
    let stderr = world.last_stderr();
    
    if stdout.contains(&unexpected) || stderr.contains(&unexpected) {
        eprintln!("=== UNEXPECTED TEXT FOUND ===");
        eprintln!("Found: '{}'", unexpected);
        eprintln!("Stdout:\n{}", stdout);
        eprintln!("Stderr:\n{}", stderr);
        panic!("Should not see '{}' in output", unexpected);
    }
}

#[then(regex = r"^the file "([^"]+)" should exist$")]
fn file_should_exist(world: &mut CleanroomWorld, filename: String) {
    let file_path = world.project_dir.join(&filename);
    
    if !file_path.exists() {
        eprintln!("=== FILE NOT FOUND ===");
        eprintln!("Expected file: {}", file_path.display());
        eprintln!("Files in directory:");
        for entry in fs::read_dir(&world.project_dir).unwrap() {
            eprintln!("  - {}", entry.unwrap().path().display());
        }
        panic!("File '{}' should exist", filename);
    }
}

#[then(regex = r"^the file "([^"]+)" should not exist$")]
fn file_should_not_exist(world: &mut CleanroomWorld, filename: String) {
    let file_path = world.project_dir.join(&filename);
    
    if file_path.exists() {
        panic!("File '{}' should not exist", filename);
    }
}

#[then(regex = r"^the file "([^"]+)" should contain "([^"]+)"$")]
fn file_should_contain(world: &mut CleanroomWorld, filename: String, expected: String) {
    let file_path = world.project_dir.join(&filename);
    
    if !file_path.exists() {
        panic!("File '{}' does not exist", filename);
    }
    
    let content = fs::read_to_string(&file_path)
        .unwrap_or_else(|e| panic!("Failed to read file '{}': {}", filename, e));
    
    if !content.contains(&expected) {
        eprintln!("=== FILE CONTENT MISMATCH ===");
        eprintln!("File: {}", file_path.display());
        eprintln!("Expected to contain: '{}'", expected);
        eprintln!("Actual content:\n{}", content);
        panic!("File '{}' should contain '{}'", filename, expected);
    }
}

#[then(regex = r"^the directory "([^"]+)" should exist$")]
fn directory_should_exist(world: &mut CleanroomWorld, dirname: String) {
    let dir_path = world.project_dir.join(&dirname);
    
    if !dir_path.exists() || !dir_path.is_dir() {
        panic!("Directory '{}' should exist", dirname);
    }
}

#[then(regex = r"^the output should be valid JSON$")]
fn output_should_be_valid_json(world: &mut CleanroomWorld) {
    let stdout = world.last_stdout();
    
    // Try to parse as JSON
    match serde_json::from_str::<serde_json::Value>(&stdout) {
        Ok(_) => {
            // Valid JSON
        }
        Err(e) => {
            eprintln!("=== INVALID JSON ===");
            eprintln!("Error: {}", e);
            eprintln!("Output:\n{}", stdout);
            panic!("Output should be valid JSON");
        }
    }
}

#[then(regex = r"^the exit code should be (\d+)$")]
fn exit_code_should_be(world: &mut CleanroomWorld, expected_code: i32) {
    let actual_code = world.last_exit_code.unwrap_or(-1);
    
    if actual_code != expected_code {
        eprintln!("=== EXIT CODE MISMATCH ===");
        eprintln!("Expected: {}", expected_code);
        eprintln!("Actual: {}", actual_code);
        eprintln!("Stdout:\n{}", world.last_stdout());
        eprintln!("Stderr:\n{}", world.last_stderr());
        panic!("Expected exit code {}, got {}", expected_code, actual_code);
    }
}
