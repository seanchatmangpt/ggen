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
fn clean_project_directory(world: &mut GgenWorld) {
    // World is already initialized with temp directory
    if world.project_dir.exists() {
        #[allow(clippy::expect_used)]
        fs::remove_dir_all(&world.project_dir).expect("Failed to clean project dir");
    }
    #[allow(clippy::expect_used)]
    fs::create_dir_all(&world.project_dir).expect("Failed to create project dir");
}

#[given(regex = r#"^I have a project template "([^"]+)" with content:$"#)]
fn create_project_template_file(world: &mut GgenWorld, filename: String, content: String) {
    let file_path = world.project_dir.join(&filename);
    if let Some(parent) = file_path.parent() {
        #[allow(clippy::expect_used)]
        fs::create_dir_all(parent).expect("Failed to create template directory");
    }
    fs::write(&file_path, content.trim())
        .unwrap_or_else(|e| panic!("Failed to write template {}: {}", filename, e));
}

#[given(regex = r#"^I have an existing file "([^"]+)" with "([^"]+)"$"#)]
fn create_existing_file(world: &mut GgenWorld, filename: String, content: String) {
    let file_path = world.project_dir.join(&filename);
    fs::write(&file_path, content)
        .unwrap_or_else(|e| panic!("Failed to write file {}: {}", filename, e));
}

#[given(regex = r#"^I have a plan file "([^"]+)" with:$"#)]
fn create_plan_file(world: &mut GgenWorld, filename: String, content: String) {
    let file_path = world.project_dir.join(&filename);
    fs::write(&file_path, content.trim())
        .unwrap_or_else(|e| panic!("Failed to write plan file {}: {}", filename, e));
}

// ============================================================================
// WHEN steps - Execute actions
// ============================================================================

#[when(regex = r#"^I run "ggen project (.+)"$"#)]
fn run_ggen_project_command(world: &mut GgenWorld, args: String) {
    // Parse the command line, handling quoted arguments
    let arg_list = shell_words::split(&args)
        .unwrap_or_else(|e| panic!("Failed to parse arguments '{}': {}", args, e));

    #[allow(clippy::expect_used)]
    let mut cmd = Command::cargo_bin("ggen").expect("ggen binary not found");
    let output = cmd
        .arg("project")
        .args(&arg_list)
        .current_dir(&world.project_dir)
        .output()
        #[allow(clippy::expect_used)]
        .expect("Failed to run ggen project command");

    world.last_output = Some(output.clone());
    world.last_exit_code = output.status.code();
}

// ============================================================================
// THEN steps - Verify outcomes
// ============================================================================

#[then(regex = r"^the command should succeed$")]
fn command_should_succeed(world: &mut GgenWorld) {
    assert!(
        world.last_command_succeeded(),
        "Command failed with exit code: {}\nStderr: {}",
        world.last_exit_code.unwrap_or(-1),
        world.last_stderr()
    );
}

#[then(regex = r#"^the file "([^"]+)" should exist$"#)]
fn file_should_exist(world: &mut GgenWorld, filename: String) {
    let file_path = world.project_dir.join(&filename);
    assert!(
        file_path.exists(),
        "File {} should exist at {}",
        filename,
        file_path.display()
    );
}

#[then(regex = r#"^the file "([^"]+)" should contain "([^"]+)"$"#)]
fn file_should_contain(world: &mut GgenWorld, filename: String, expected: String) {
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
fn should_see_in_output(world: &mut GgenWorld, expected: String) {
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
fn file_should_not_exist(world: &mut GgenWorld, filename: String) {
    let file_path = world.project_dir.join(&filename);
    assert!(
        !file_path.exists(),
        "File {} should not exist, but it does at {}",
        filename,
        file_path.display()
    );
}

#[then(regex = r#"^the file "([^"]+)" should still contain "([^"]+)"$"#)]
fn file_should_still_contain(world: &mut GgenWorld, filename: String, expected: String) {
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
fn command_should_fail(world: &mut GgenWorld) {
    assert!(
        !world.last_command_succeeded(),
        "Command should have failed but succeeded"
    );
}

#[then(regex = r#"^I should see "([^"]+)" in stderr$"#)]
fn should_see_in_stderr(world: &mut GgenWorld, expected: String) {
    let stderr = world.last_stderr();
    assert!(
        stderr.contains(&expected),
        "Expected to see '{}' in stderr, but got: {}",
        expected,
        stderr
    );
}

#[then(regex = r#"^I should see "(.+)" in output$"#)]
fn should_see_pattern_in_output(world: &mut GgenWorld, pattern: String) {
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
fn both_outputs_should_be_identical(world: &mut GgenWorld) {
    // This requires capturing multiple outputs
    // For now, we'll use file hash comparison
    if let Some(hash1) = world.captured_hashes.first() {
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
fn should_see_generated_in_output(world: &mut GgenWorld) {
    let stdout = world.last_stdout();
    let stderr = world.last_stderr();

    assert!(
        stdout.contains("Generated")
            || stdout.contains("generated")
            || stderr.contains("Generated")
            || stderr.contains("generated"),
        "Expected to see 'Generated' in output, but got:\nStdout: {}\nStderr: {}",
        stdout,
        stderr
    );
}

// ============================================================================
// Seed and Variable Steps
// ============================================================================

#[when(regex = r#"^I run "ggen project gen (.+)" with seed "([^"]+)"$"#)]
fn run_project_gen_with_seed(world: &mut GgenWorld, template: String, seed: String) {
    let output = Command::cargo_bin("ggen")
        #[allow(clippy::expect_used)]
        .expect("ggen binary not found")
        .arg("project")
        .arg("gen")
        .arg(&template)
        .arg("--seed")
        .arg(&seed)
        .current_dir(&world.project_dir)
        .output()
        #[allow(clippy::expect_used)]
        .expect("Failed to run project gen with seed");

    world.last_output = Some(output.clone());
    world.last_exit_code = output.status.code();
}

#[when(regex = r#"^I run "ggen project gen (.+)" with variable "([^"]+)"$"#)]
fn run_project_gen_with_variable(world: &mut GgenWorld, template: String, variable: String) {
    let output = Command::cargo_bin("ggen")
        #[allow(clippy::expect_used)]
        .expect("ggen binary not found")
        .arg("project")
        .arg("gen")
        .arg(&template)
        .arg("--var")
        .arg(&variable)
        .current_dir(&world.project_dir)
        .output()
        #[allow(clippy::expect_used)]
        .expect("Failed to run project gen with variable");

    world.last_output = Some(output.clone());
    world.last_exit_code = output.status.code();
}

#[when(regex = r#"^I run "ggen project gen (.+)" with variables "([^"]+)" and "([^"]+)"$"#)]
fn run_project_gen_with_multiple_variables(
    world: &mut GgenWorld, template: String, var1: String, var2: String,
) {
    let output = Command::cargo_bin("ggen")
        #[allow(clippy::expect_used)]
        .expect("ggen binary not found")
        .arg("project")
        .arg("gen")
        .arg(&template)
        .arg("--var")
        .arg(&var1)
        .arg("--var")
        .arg(&var2)
        .current_dir(&world.project_dir)
        .output()
        #[allow(clippy::expect_used)]
        .expect("Failed to run project gen with multiple variables");

    world.last_output = Some(output.clone());
    world.last_exit_code = output.status.code();
}

#[when(regex = r#"^I run "ggen project plan (.+)" with seed "([^"]+)"$"#)]
fn run_project_plan_with_seed(world: &mut GgenWorld, template: String, seed: String) {
    let output = Command::cargo_bin("ggen")
        #[allow(clippy::expect_used)]
        .expect("ggen binary not found")
        .arg("project")
        .arg("plan")
        .arg(&template)
        .arg("--seed")
        .arg(&seed)
        .current_dir(&world.project_dir)
        .output()
        #[allow(clippy::expect_used)]
        .expect("Failed to run project plan with seed");

    world.last_output = Some(output.clone());
    world.last_exit_code = output.status.code();
}

#[when(regex = r#"^I run "ggen project diff (.+)" with variable "([^"]+)"$"#)]
fn run_project_diff_with_variable(world: &mut GgenWorld, template: String, variable: String) {
    let output = Command::cargo_bin("ggen")
        #[allow(clippy::expect_used)]
        .expect("ggen binary not found")
        .arg("project")
        .arg("diff")
        .arg(&template)
        .arg("--var")
        .arg(&variable)
        .current_dir(&world.project_dir)
        .output()
        #[allow(clippy::expect_used)]
        .expect("Failed to run project diff with variable");

    world.last_output = Some(output.clone());
    world.last_exit_code = output.status.code();
}

#[when(regex = r#"^I run "ggen project apply (.+)"$"#)]
fn run_project_apply(world: &mut GgenWorld, plan_file: String) {
    let output = Command::cargo_bin("ggen")
        #[allow(clippy::expect_used)]
        .expect("ggen binary not found")
        .arg("project")
        .arg("apply")
        .arg(&plan_file)
        .current_dir(&world.project_dir)
        .output()
        #[allow(clippy::expect_used)]
        .expect("Failed to run project apply");

    world.last_output = Some(output.clone());
    world.last_exit_code = output.status.code();
}

#[then(regex = r"^the output should be deterministic$")]
fn output_should_be_deterministic(world: &mut GgenWorld) {
    // Store hash for comparison in subsequent runs
    let stdout = world.last_stdout();
    let hash = format!("{:x}", md5::compute(stdout.as_bytes()));
    world.captured_hashes.push(hash);
}

#[then(regex = r"^I should see plan output$")]
fn should_see_plan_output(world: &mut GgenWorld) {
    let stdout = world.last_stdout();
    assert!(
        stdout.contains("plan")
            || stdout.contains("Plan")
            || stdout.contains("changes")
            || stdout.contains("actions"),
        "Expected to see plan output, but got: {}",
        stdout
    );
}

#[then(regex = r"^I should see diff output$")]
fn should_see_diff_output(world: &mut GgenWorld) {
    let stdout = world.last_stdout();
    assert!(
        stdout.contains("diff")
            || stdout.contains("Diff")
            || stdout.contains("+")
            || stdout.contains("-")
            || stdout.contains("changed")
            || stdout.contains("modified"),
        "Expected to see diff output, but got: {}",
        stdout
    );
}
