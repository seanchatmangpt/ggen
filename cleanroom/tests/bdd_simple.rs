use cucumber::{given, then, when, World};
use std::collections::HashMap;
use std::path::PathBuf;
use std::process::Output;
use tempfile::TempDir;

/// Simple world state for BDD tests
#[derive(Debug, Default, cucumber::World)]
pub struct SimpleWorld {
    pub temp_dir: Option<TempDir>,
    pub project_dir: PathBuf,
    pub last_output: Option<Output>,
    pub last_exit_code: Option<i32>,
    pub captured_files: HashMap<String, String>,
    pub environment: HashMap<String, String>,
}

impl SimpleWorld {
    pub fn new() -> Self {
        let temp_dir = TempDir::new().expect("Failed to create temp dir");
        let project_dir = temp_dir.path().to_path_buf();

        Self {
            temp_dir: Some(temp_dir),
            project_dir,
            ..Default::default()
        }
    }
}

// Step definitions
#[given("a cleanroom project")]
fn given_cleanroom_project(world: &mut SimpleWorld) {
    // Create a basic project structure
    std::fs::create_dir_all(&world.project_dir).expect("Failed to create project dir");
    
    // Create a simple config file
    let config_content = r#"
[cleanroom]
backend = "local"
timeout_ms = 5000
"#;
    std::fs::write(world.project_dir.join("cleanroom.toml"), config_content)
        .expect("Failed to write config");
}

#[when("I run cleanroom with args {string}")]
fn when_run_cleanroom(world: &mut SimpleWorld, args: String) {
    use assert_cmd::Command;
    
    // Parse arguments
    let arg_list: Vec<&str> = args.split_whitespace().collect();
    
    // Create command
    let mut cmd = Command::new("cargo");
    cmd.args(&["run", "--bin", "cleanroom", "--"]);
    cmd.args(&arg_list);
    cmd.current_dir(&world.project_dir);
    
    // Execute and capture output
    let output = cmd.output().expect("Failed to execute command");
    world.last_output = Some(output);
    world.last_exit_code = Some(world.last_output.as_ref().unwrap().status.code().unwrap_or(-1));
}

#[then("the exit code is {int}")]
fn then_exit_code(world: &mut SimpleWorld, expected_code: i32) {
    let actual_code = world.last_exit_code.unwrap_or(-1);
    assert_eq!(actual_code, expected_code, "Expected exit code {}, got {}", expected_code, actual_code);
}

#[then("stdout contains {string}")]
fn then_stdout_contains(world: &mut SimpleWorld, expected_text: String) {
    let output = world.last_output.as_ref().expect("No output captured");
    let stdout = String::from_utf8_lossy(&output.stdout);
    assert!(stdout.contains(&expected_text), "Expected stdout to contain '{}', got: {}", expected_text, stdout);
}

#[then("stderr is empty")]
fn then_stderr_empty(world: &mut SimpleWorld) {
    let output = world.last_output.as_ref().expect("No output captured");
    let stderr = String::from_utf8_lossy(&output.stderr);
    assert!(stderr.is_empty(), "Expected stderr to be empty, got: {}", stderr);
}

#[tokio::test]
async fn test_basic_cleanroom_features() {
    SimpleWorld::cucumber()
        .features(&["tests/bdd/features/01_unified_execution.feature"])
        .run_and_exit()
        .await;
}

#[tokio::test]
async fn test_all_bdd_features() {
    SimpleWorld::cucumber()
        .features(&[
            "tests/bdd/features/01_unified_execution.feature",
            "tests/bdd/features/02_backend_autodetect.feature",
            "tests/bdd/features/03_config_precedence.feature",
        ])
        .run_and_exit()
        .await;
}
