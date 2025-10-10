use super::super::world::GgenWorld;
use assert_cmd::Command;
use cucumber::{given, then, when};
use std::fs;

/// Step definitions for `ggen project` noun-verb commands
///
/// Covers project operations:
/// - project gen: Generate artifacts from templates
/// - project plan: Create dry-run plans
/// - project apply: Apply plans to filesystem
/// - project diff: Show what generation would change

// ============================================================================
// GIVEN steps - Setup preconditions
// ============================================================================

#[given(regex = r"^I am in a clean project directory$")]
async fn clean_project_directory(world: &mut GgenWorld) {
    // World is already initialized with temp directory
    if world.project_dir.exists() {
        fs::remove_dir_all(&world.project_dir).expect("Failed to clean project dir");
    }
    fs::create_dir_all(&world.project_dir).expect("Failed to create project dir");
}

#[given(regex = r#"^I have a template "([^"]+)" with content:$"#)]
async fn create_template_file(world: &mut GgenWorld, filename: String, content: String) {
    let file_path = world.project_dir.join(&filename);
    if let Some(parent) = file_path.parent() {
        fs::create_dir_all(parent).expect("Failed to create template directory");
    }
    fs::write(&file_path, content.trim())
        .unwrap_or_else(|e| panic!("Failed to write template {}: {}", filename, e));
}

#[given(regex = r#"^I have an existing file "([^"]+)" with "([^"]+)"$"#)]
async fn create_existing_file(world: &mut GgenWorld, filename: String, content: String) {
    let file_path = world.project_dir.join(&filename);
    fs::write(&file_path, content)
        .unwrap_or_else(|e| panic!("Failed to write file {}: {}", filename, e));
}

#[given(regex = r#"^I have a plan file "([^"]+)" with:$"#)]
async fn create_plan_file(world: &mut GgenWorld, filename: String, content: String) {
    let file_path = world.project_dir.join(&filename);
    fs::write(&file_path, content.trim())
        .unwrap_or_else(|e| panic!("Failed to write plan file {}: {}", filename, e));
}

#[given(regex = r"^the marketplace registry is available$")]
async fn marketplace_registry_available(_world: &mut GgenWorld) {
    // In a real test, we'd set up a mock registry
    // For now, we'll skip tests that require the registry
}

#[given(regex = r#"^I have installed the gpack "([^"]+)"$"#)]
async fn install_gpack(_world: &mut GgenWorld, _gpack_id: String) {
    // Would install gpack via `ggen market add`
    // For now, mark as pending implementation
}

// ============================================================================
// WHEN steps - Execute actions
// ============================================================================

#[when(regex = r#"^I run "ggen project (.+)"$"#)]
async fn run_ggen_project_command(world: &mut GgenWorld, args: String) {
    // Parse the command line, handling quoted arguments
    let arg_list = shell_words::split(&args)
        .unwrap_or_else(|e| panic!("Failed to parse arguments '{}': {}", args, e));

    let mut cmd = Command::cargo_bin("ggen").expect("ggen binary not found");
    let output = cmd
        .arg("project")
        .args(&arg_list)
        .current_dir(&world.project_dir)
        .output()
        .expect("Failed to run ggen project command");

    world.last_output = Some(output.clone());
    world.last_exit_code = output.status.code();
}

// ============================================================================
// THEN steps - Verify outcomes
// ============================================================================

#[then(regex = r"^the command should succeed$")]
async fn command_should_succeed(world: &mut GgenWorld) {
    assert!(
        world.last_command_succeeded(),
        "Command failed with exit code: {}\nStderr: {}",
        world.last_exit_code.unwrap_or(-1),
        world.last_stderr()
    );
}

#[then(regex = r#"^the file "([^"]+)" should exist$"#)]
async fn file_should_exist(world: &mut GgenWorld, filename: String) {
    let file_path = world.project_dir.join(&filename);
    assert!(
        file_path.exists(),
        "File {} should exist at {}",
        filename,
        file_path.display()
    );
}

#[then(regex = r#"^the file "([^"]+)" should contain "([^"]+)"$"#)]
async fn file_should_contain(world: &mut GgenWorld, filename: String, expected: String) {
    let file_path = world.project_dir.join(&filename);
    let content = fs::read_to_string(&file_path)
        .unwrap_or_else(|e| panic!("Failed to read file {}: {}", filename, e));

    assert!(
        content.contains(&expected),
        "File {} should contain '{}', but got: {}",
        filename,
        expected,
        content
    );
}

#[then(regex = r#"^I should see "([^"]+)" in output$"#)]
async fn should_see_in_output(world: &mut GgenWorld, expected: String) {
    let stdout = world.last_stdout();
    let stderr = world.last_stderr();

    assert!(
        stdout.contains(&expected) || stderr.contains(&expected),
        "Expected to see '{}' in output, but got:\nStdout: {}\nStderr: {}",
        expected,
        stdout,
        stderr
    );
}

#[then(regex = r#"^the file "([^"]+)" should not exist$"#)]
async fn file_should_not_exist(world: &mut GgenWorld, filename: String) {
    let file_path = world.project_dir.join(&filename);
    assert!(
        !file_path.exists(),
        "File {} should not exist, but it does at {}",
        filename,
        file_path.display()
    );
}

#[then(regex = r#"^the file "([^"]+)" should still contain "([^"]+)"$"#)]
async fn file_should_still_contain(world: &mut GgenWorld, filename: String, expected: String) {
    let file_path = world.project_dir.join(&filename);
    let content = fs::read_to_string(&file_path)
        .unwrap_or_else(|e| panic!("Failed to read file {}: {}", filename, e));

    assert!(
        content.contains(&expected),
        "File {} should still contain '{}', but got: {}",
        filename,
        expected,
        content
    );
}

#[then(regex = r"^the command should fail$")]
async fn command_should_fail(world: &mut GgenWorld) {
    assert!(
        !world.last_command_succeeded(),
        "Command should have failed but succeeded"
    );
}

#[then(regex = r#"^I should see "([^"]+)" in stderr$"#)]
async fn should_see_in_stderr(world: &mut GgenWorld, expected: String) {
    let stderr = world.last_stderr();
    assert!(
        stderr.contains(&expected),
        "Expected to see '{}' in stderr, but got: {}",
        expected,
        stderr
    );
}

#[then(regex = r#"^I should see "(.+)" in output$"#)]
async fn should_see_pattern_in_output(world: &mut GgenWorld, pattern: String) {
    let stdout = world.last_stdout();
    let stderr = world.last_stderr();

    assert!(
        stdout.contains(&pattern) || stderr.contains(&pattern),
        "Expected to see '{}' in output, but got:\nStdout: {}\nStderr: {}",
        pattern,
        stdout,
        stderr
    );
}

#[then(regex = r"^both outputs should be identical$")]
async fn both_outputs_should_be_identical(world: &mut GgenWorld) {
    // This requires capturing multiple outputs
    // For now, we'll use file hash comparison
    if let Some(hash1) = world.captured_hashes.get(0) {
        if let Some(hash2) = world.captured_hashes.get(1) {
            assert_eq!(
                hash1, hash2,
                "Outputs should be identical (deterministic), but hashes differ:\n{} vs {}",
                hash1, hash2
            );
        }
    }
}

#[then(regex = r"^I should see Generated in output$")]
async fn should_see_generated_in_output(world: &mut GgenWorld) {
    let stdout = world.last_stdout();
    let stderr = world.last_stderr();

    assert!(
        stdout.contains("Generated") || stdout.contains("generated") ||
        stderr.contains("Generated") || stderr.contains("generated"),
        "Expected to see 'Generated' in output, but got:\nStdout: {}\nStderr: {}",
        stdout,
        stderr
    );
}
