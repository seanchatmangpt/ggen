use super::super::world::GgenWorld;
use assert_cmd::Command;
use cucumber::{given, then, when};
use std::fs;

/// Step definitions for `ggen ci` noun-verb commands
///
/// Covers CI/CD operations:
/// - ci pages: GitHub Pages deployment
/// - ci trigger: Trigger CI workflows
/// - ci workflow: Manage GitHub workflows

// ============================================================================
// GIVEN steps - Setup preconditions
// ============================================================================

#[given(regex = r"^I have a GitHub repository$")]
fn have_github_repository(world: &mut GgenWorld) {
    // Set up mock GitHub repository environment
    std::env::set_var("GITHUB_REPOSITORY", "test-org/test-repo");
    std::env::set_var("GITHUB_TOKEN", "mock-token");
    
    // Create mock .git directory
    let git_dir = world.project_dir.join(".git");
    fs::create_dir_all(&git_dir).expect("Failed to create .git directory");
}

#[given(regex = r"^I have GitHub Pages enabled$")]
fn have_github_pages_enabled(world: &mut GgenWorld) {
    // Create mock GitHub Pages configuration
    let pages_config = r#"{
        "source": {
            "branch": "gh-pages",
            "path": "/"
        }
    }"#;
    
    let config_path = world.project_dir.join(".github/pages.json");
    fs::create_dir_all(config_path.parent().unwrap()).expect("Failed to create .github directory");
    fs::write(&config_path, pages_config).expect("Failed to write pages config");
}

#[given(regex = r"^I have a GitHub workflow$")]
fn have_github_workflow(world: &mut GgenWorld) {
    let workflow_content = r#"name: CI
on: [push, pull_request]
jobs:
  test:
    runs-on: ubuntu-latest
    steps:
      - uses: actions/checkout@v3
      - name: Run tests
        run: cargo test"#;
    
    let workflow_path = world.project_dir.join(".github/workflows/ci.yml");
    fs::create_dir_all(workflow_path.parent().unwrap()).expect("Failed to create workflows directory");
    fs::write(&workflow_path, workflow_content).expect("Failed to write workflow");
}

#[given(regex = r"^I have documentation files$")]
fn have_documentation_files(world: &mut GgenWorld) {
    // Create mock documentation files
    let docs_dir = world.project_dir.join("docs");
    fs::create_dir_all(&docs_dir).expect("Failed to create docs directory");
    
    let index_content = r#"# Project Documentation
This is the main documentation page."#;
    fs::write(docs_dir.join("index.md"), index_content).expect("Failed to write index.md");
    
    let api_content = r#"# API Reference
API documentation goes here."#;
    fs::write(docs_dir.join("api.md"), api_content).expect("Failed to write api.md");
}

#[given(regex = r"^I have build artifacts$")]
fn have_build_artifacts(world: &mut GgenWorld) {
    // Create mock build artifacts
    let artifacts_dir = world.project_dir.join("target/release");
    fs::create_dir_all(&artifacts_dir).expect("Failed to create artifacts directory");
    
    let binary_content = b"mock binary content";
    fs::write(artifacts_dir.join("ggen"), binary_content).expect("Failed to write binary");
    
    let docs_content = r#"# Generated Documentation
This is generated documentation."#;
    fs::write(artifacts_dir.join("docs.html"), docs_content).expect("Failed to write docs.html");
}

// ============================================================================
// WHEN steps - Execute actions
// ============================================================================

#[when(regex = r#"^I run "ggen ci pages deploy"$"#)]
fn run_ci_pages_deploy(world: &mut GgenWorld) {
    let output = Command::cargo_bin("ggen")
        .expect("ggen binary not found")
        .arg("ci")
        .arg("pages")
        .arg("deploy")
        .current_dir(&world.project_dir)
        .output()
        .expect("Failed to run ci pages deploy");
    
    world.last_output = Some(output.clone());
    world.last_exit_code = output.status.code();
}

#[when(regex = r#"^I run "ggen ci pages setup"$"#)]
fn run_ci_pages_setup(world: &mut GgenWorld) {
    let output = Command::cargo_bin("ggen")
        .expect("ggen binary not found")
        .arg("ci")
        .arg("pages")
        .arg("setup")
        .current_dir(&world.project_dir)
        .output()
        .expect("Failed to run ci pages setup");
    
    world.last_output = Some(output.clone());
    world.last_exit_code = output.status.code();
}

#[when(regex = r#"^I run "ggen ci trigger (.+)"$"#)]
fn run_ci_trigger(world: &mut GgenWorld, workflow: String) {
    let output = Command::cargo_bin("ggen")
        .expect("ggen binary not found")
        .arg("ci")
        .arg("trigger")
        .arg(&workflow)
        .current_dir(&world.project_dir)
        .output()
        .expect("Failed to run ci trigger");
    
    world.last_output = Some(output.clone());
    world.last_exit_code = output.status.code();
}

#[when(regex = r#"^I run "ggen ci workflow (.+)"$"#)]
fn run_ci_workflow(world: &mut GgenWorld, action: String) {
    let output = Command::cargo_bin("ggen")
        .expect("ggen binary not found")
        .arg("ci")
        .arg("workflow")
        .arg(&action)
        .current_dir(&world.project_dir)
        .output()
        .expect("Failed to run ci workflow");
    
    world.last_output = Some(output.clone());
    world.last_exit_code = output.status.code();
}

#[when(regex = r#"^I run "ggen ci (.+)"$"#)]
fn run_ci_command(world: &mut GgenWorld, args: String) {
    let arg_list = shell_words::split(&args)
        .unwrap_or_else(|e| panic!("Failed to parse arguments '{}': {}", args, e));

    let mut cmd = Command::cargo_bin("ggen").expect("ggen binary not found");
    let output = cmd
        .arg("ci")
        .args(&arg_list)
        .current_dir(&world.project_dir)
        .output()
        .expect("Failed to run ci command");

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

#[then(regex = r"^the command should fail$")]
fn command_should_fail(world: &mut GgenWorld) {
    assert!(
        !world.last_command_succeeded(),
        "Command should have failed but succeeded"
    );
}

#[then(regex = r"^I should see deployment status$")]
fn should_see_deployment_status(world: &mut GgenWorld) {
    let stdout = world.last_stdout();
    assert!(
        stdout.contains("deploy") || stdout.contains("Deploy") ||
        stdout.contains("status") || stdout.contains("Status") ||
        stdout.contains("success") || stdout.contains("Success") ||
        stdout.contains("failed") || stdout.contains("Failed"),
        "Expected to see deployment status, but got: {}",
        stdout
    );
}

#[then(regex = r"^I should see GitHub Pages URL$")]
fn should_see_github_pages_url(world: &mut GgenWorld) {
    let stdout = world.last_stdout();
    assert!(
        stdout.contains("github.io") || stdout.contains("pages") ||
        stdout.contains("http") || stdout.contains("https"),
        "Expected to see GitHub Pages URL, but got: {}",
        stdout
    );
}

#[then(regex = r"^I should see workflow status$")]
fn should_see_workflow_status(world: &mut GgenWorld) {
    let stdout = world.last_stdout();
    assert!(
        stdout.contains("workflow") || stdout.contains("Workflow") ||
        stdout.contains("running") || stdout.contains("Running") ||
        stdout.contains("completed") || stdout.contains("Completed") ||
        stdout.contains("queued") || stdout.contains("Queued"),
        "Expected to see workflow status, but got: {}",
        stdout
    );
}

#[then(regex = r"^I should see build logs$")]
fn should_see_build_logs(world: &mut GgenWorld) {
    let stdout = world.last_stdout();
    assert!(
        stdout.contains("build") || stdout.contains("Build") ||
        stdout.contains("log") || stdout.contains("Log") ||
        stdout.contains("output") || stdout.contains("Output") ||
        stdout.contains("cargo") || stdout.contains("rust"),
        "Expected to see build logs, but got: {}",
        stdout
    );
}

#[then(regex = r"^I should see setup instructions$")]
fn should_see_setup_instructions(world: &mut GgenWorld) {
    let stdout = world.last_stdout();
    assert!(
        stdout.contains("setup") || stdout.contains("Setup") ||
        stdout.contains("configure") || stdout.contains("Configure") ||
        stdout.contains("install") || stdout.contains("Install") ||
        stdout.contains("enable") || stdout.contains("Enable"),
        "Expected to see setup instructions, but got: {}",
        stdout
    );
}

#[then(regex = r"^the workflow should be triggered$")]
fn workflow_should_be_triggered(world: &mut GgenWorld) {
    let stdout = world.last_stdout();
    assert!(
        stdout.contains("triggered") || stdout.contains("Triggered") ||
        stdout.contains("started") || stdout.contains("Started") ||
        stdout.contains("queued") || stdout.contains("Queued"),
        "Expected to see workflow triggered, but got: {}",
        stdout
    );
}

#[then(regex = r"^I should see GitHub API response$")]
fn should_see_github_api_response(world: &mut GgenWorld) {
    let stdout = world.last_stdout();
    assert!(
        stdout.contains("github.com") || stdout.contains("api.github.com") ||
        stdout.contains("status") || stdout.contains("response") ||
        stdout.contains("json") || stdout.contains("{"),
        "Expected to see GitHub API response, but got: {}",
        stdout
    );
}

#[then(regex = r"^the output should be valid JSON$")]
fn output_should_be_valid_json(world: &mut GgenWorld) {
    let stdout = world.last_stdout();
    serde_json::from_str::<serde_json::Value>(&stdout)
        .unwrap_or_else(|e| panic!("Output should be valid JSON: {}\nOutput: {}", e, stdout));
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
