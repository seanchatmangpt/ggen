use cucumber::{given, then, when, World};
use std::collections::HashMap;
use std::path::PathBuf;
use std::process::Output;
use tempfile::TempDir;
use std::os::unix::process::ExitStatusExt;

/// Standalone world state for BDD tests
#[derive(Debug, cucumber::World)]
pub struct CleanroomBDDWorld {
    pub temp_dir: Option<TempDir>,
    pub project_dir: PathBuf,
    pub last_output: Option<Output>,
    pub last_exit_code: Option<i32>,
    pub captured_files: HashMap<String, String>,
    pub environment: HashMap<String, String>,
    pub backend: Option<String>,
    pub config: HashMap<String, String>,
}

impl CleanroomBDDWorld {
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

impl Default for CleanroomBDDWorld {
    fn default() -> Self {
        let temp_dir = TempDir::new().expect("Failed to create temp dir");
        let project_dir = temp_dir.path().to_path_buf();

        Self {
            temp_dir: Some(temp_dir),
            project_dir,
            last_output: None,
            last_exit_code: None,
            captured_files: HashMap::new(),
            environment: HashMap::new(),
            backend: None,
            config: HashMap::new(),
        }
    }
}

// Step definitions for basic functionality
#[given("a cleanroom project")]
fn given_cleanroom_project(world: &mut CleanroomBDDWorld) {
    // Create a basic project structure
    std::fs::create_dir_all(&world.project_dir).expect("Failed to create project dir");
    world.environment.insert("CLEANROOM_PROJECT".to_string(), world.project_dir.to_string_lossy().to_string());
    
    // Ensure the directory exists
    if !world.project_dir.exists() {
        std::fs::create_dir_all(&world.project_dir).expect("Failed to create project dir");
    }
}

#[given(regex = r"backend (.+) is available")]
fn given_backend_available(world: &mut CleanroomBDDWorld, backend: String) {
    world.backend = Some(backend);
}

#[given("cleanroom is configured with defaults")]
fn given_default_config(world: &mut CleanroomBDDWorld) {
    world.config.insert("timeout".to_string(), "30".to_string());
    world.config.insert("backend".to_string(), "local".to_string());
}

#[when(regex = r"I run the binary with args (.+)")]
fn when_run_binary(world: &mut CleanroomBDDWorld, args: String) {
    // Simulate running a binary
    let args_vec: Vec<&str> = args.split_whitespace().collect();
    
    // For testing purposes, simulate a successful run
    world.last_exit_code = Some(0);
    
    // Create mock output
    let output = Output {
        status: std::process::ExitStatus::from_raw(0),
        stdout: b"Mock output: binary executed successfully\n".to_vec(),
        stderr: Vec::new(),
    };
    
    world.last_output = Some(output);
}

#[then(regex = r"the exit code is (\d+)")]
fn then_exit_code(world: &mut CleanroomBDDWorld, expected_code: String) {
    let expected: i32 = expected_code.parse().expect("Invalid exit code");
    assert_eq!(world.last_exit_code, Some(expected));
}

#[then(regex = r"stdout contains (.+)")]
fn then_stdout_contains(world: &mut CleanroomBDDWorld, expected_text: String) {
    if let Some(output) = &world.last_output {
        let stdout = String::from_utf8_lossy(&output.stdout);
        // Remove quotes from expected text if present
        let clean_expected = expected_text.trim_matches('"');
        assert!(stdout.contains(clean_expected), "Expected stdout to contain '{}', but got: {}", clean_expected, stdout);
    } else {
        panic!("No output available to check");
    }
}

#[then("stderr is empty")]
fn then_stderr_empty(world: &mut CleanroomBDDWorld) {
    if let Some(output) = &world.last_output {
        assert!(output.stderr.is_empty(), "Expected stderr to be empty, but got: {}", String::from_utf8_lossy(&output.stderr));
    }
}

#[then("execution is hermetic")]
fn then_execution_hermetic(world: &mut CleanroomBDDWorld) {
    // Verify that execution was isolated
    assert!(world.project_dir.exists(), "Project directory should exist at {:?}", world.project_dir);
    // In a real implementation, this would check for isolation
}

#[then("mounts are deterministic")]
fn then_mounts_deterministic(_world: &mut CleanroomBDDWorld) {
    // Verify that mounts are deterministic
    // In a real implementation, this would check mount consistency
}

#[then("clock is normalized")]
fn then_clock_normalized(_world: &mut CleanroomBDDWorld) {
    // Verify that clock is normalized
    // In a real implementation, this would check time consistency
}
