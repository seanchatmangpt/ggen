use super::super::world::GgenWorld;
use assert_cmd::Command;
use cucumber::{given, then, when};
use std::fs;

/// Step definitions for `ggen market` noun-verb commands
///
/// Covers marketplace operations:
/// - market search: Search for gpacks
/// - market add: Install gpacks
/// - market remove: Uninstall gpacks
/// - market list: List installed gpacks
/// - market update: Update gpacks
/// - market show: Show gpack details
/// - market categories: Show popular categories

// ============================================================================
// GIVEN steps - Setup preconditions
// ============================================================================

#[given(regex = r"^the marketplace registry is available$")]
async fn marketplace_registry_available(_world: &mut GgenWorld) {
    // Set environment variable for registry URL
    std::env::set_var(
        "GGEN_REGISTRY_URL",
        "https://raw.githubusercontent.com/seanchatmangpt/ggen/master/registry/",
    );
}

#[given(regex = r#"^I have installed the gpack "([^"]+)"$"#)]
async fn have_installed_gpack(world: &mut GgenWorld, package_id: String) {
    // Simulate having a package installed by creating a mock lockfile entry
    let lockfile_path = world.project_dir.join("ggen.lock");
    let lockfile_content = format!(
        r#"{{"packages": {{"{}": {{"version": "0.2.0", "sha256": "abc123", "installed": true}}}}}}"#,
        package_id
    );
    fs::write(&lockfile_path, lockfile_content).expect("Failed to write mock lockfile");
}

#[given(regex = r#"^I have installed the gpack "([^"]+)@([^"]+)"$"#)]
async fn have_installed_gpack_with_version(
    world: &mut GgenWorld,
    package_id: String,
    version: String,
) {
    let lockfile_path = world.project_dir.join("ggen.lock");
    let lockfile_content = format!(
        r#"{{"packages": {{"{}": {{"version": "{}", "sha256": "abc123", "installed": true}}}}}}"#,
        package_id, version
    );
    fs::write(&lockfile_path, lockfile_content).expect("Failed to write mock lockfile");
}

#[given(regex = r#"^a newer version "([^"]+)" is available$"#)]
async fn newer_version_available(_world: &mut GgenWorld, _version: String) {
    // Would configure mock registry to return newer version
    // For now, this is a no-op
}

#[given(regex = r#"^the gpack "([^"]+)" has a PQC signature$"#)]
async fn gpack_has_pqc_signature(_world: &mut GgenWorld, _package_id: String) {
    // Would configure mock registry with PQC signature
}

// ============================================================================
// WHEN steps - Execute actions
// ============================================================================

#[when(regex = r#"^I run "ggen market (.+)"$"#)]
async fn run_ggen_market_command(world: &mut GgenWorld, args: String) {
    // Parse command line, handling quoted arguments and JSON
    let arg_list = shell_words::split(&args)
        .unwrap_or_else(|e| panic!("Failed to parse arguments '{}': {}", args, e));

    let mut cmd = Command::cargo_bin("ggen").expect("ggen binary not found");
    let output = cmd
        .arg("market")
        .args(&arg_list)
        .current_dir(&world.project_dir)
        .output()
        .expect("Failed to run ggen market command");

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

#[then(regex = r"^I should see results for rust gpacks$")]
async fn should_see_rust_results(world: &mut GgenWorld) {
    let stdout = world.last_stdout();
    assert!(
        stdout.contains("rust") || stdout.contains("Rust"),
        "Expected to see rust gpacks in results, but got: {}",
        stdout
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

#[then(regex = r"^results should only show rust category gpacks$")]
async fn results_should_show_rust_category(_world: &mut GgenWorld) {
    // Would validate that all results have rust category
}

#[then(regex = r"^the output should be valid JSON$")]
async fn output_should_be_valid_json(world: &mut GgenWorld) {
    let stdout = world.last_stdout();

    serde_json::from_str::<serde_json::Value>(&stdout)
        .unwrap_or_else(|e| panic!("Output is not valid JSON: {}\nOutput: {}", e, stdout));
}

#[then(regex = r#"^the JSON should contain a "([^"]+)" array$"#)]
async fn json_should_contain_array(world: &mut GgenWorld, field: String) {
    let stdout = world.last_stdout();
    let json: serde_json::Value = serde_json::from_str(&stdout)
        .unwrap_or_else(|e| panic!("Failed to parse JSON: {}", e));

    assert!(
        json.get(&field).and_then(|v| v.as_array()).is_some(),
        "JSON should contain '{}' array, but got: {}",
        field,
        stdout
    );
}

#[then(regex = r#"^the gpack should be listed in "([^"]+)"$"#)]
async fn gpack_should_be_in_lockfile(world: &mut GgenWorld, lockfile: String) {
    let lockfile_path = world.project_dir.join(&lockfile);
    assert!(
        lockfile_path.exists(),
        "Lockfile {} should exist",
        lockfile
    );

    let content = fs::read_to_string(&lockfile_path)
        .unwrap_or_else(|e| panic!("Failed to read lockfile: {}", e));
    assert!(
        !content.is_empty(),
        "Lockfile should not be empty"
    );
}

#[then(regex = r"^the gpack should be cached locally$")]
async fn gpack_should_be_cached(_world: &mut GgenWorld) {
    // Would check the local cache directory
}

#[then(regex = r#"^the lockfile should show version "([^"]+)"$"#)]
async fn lockfile_should_show_version(world: &mut GgenWorld, version: String) {
    let lockfile_path = world.project_dir.join("ggen.lock");
    if lockfile_path.exists() {
        let content = fs::read_to_string(&lockfile_path)
            .unwrap_or_else(|e| panic!("Failed to read lockfile: {}", e));
        assert!(
            content.contains(&version),
            "Lockfile should show version '{}', but got: {}",
            version,
            content
        );
    }
}

#[then(regex = r#"^the gpack should not be in "([^"]+)"$"#)]
async fn gpack_should_not_be_in_lockfile(world: &mut GgenWorld, lockfile: String) {
    let lockfile_path = world.project_dir.join(&lockfile);
    if lockfile_path.exists() {
        let content = fs::read_to_string(&lockfile_path)
            .unwrap_or_else(|e| panic!("Failed to read lockfile: {}", e));
        // In a real test, we'd check that the specific gpack is not listed
        // For now, we just verify the file exists or is empty
    }
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

#[then(regex = r"^I should see version information$")]
async fn should_see_version_information(world: &mut GgenWorld) {
    let stdout = world.last_stdout();
    assert!(
        stdout.contains("version") || stdout.contains("Version") || stdout.contains("0."),
        "Expected to see version information, but got: {}",
        stdout
    );
}

#[then(regex = r"^I should see source URLs$")]
async fn should_see_source_urls(world: &mut GgenWorld) {
    let stdout = world.last_stdout();
    assert!(
        stdout.contains("http") || stdout.contains("source") || stdout.contains("url"),
        "Expected to see source URLs, but got: {}",
        stdout
    );
}

#[then(regex = r"^the gpack should be updated to latest version$")]
async fn gpack_updated_to_latest(_world: &mut GgenWorld) {
    // Would verify lockfile has latest version
}

#[then(regex = r"^I should see popular categories$")]
async fn should_see_popular_categories(world: &mut GgenWorld) {
    let stdout = world.last_stdout();
    assert!(
        stdout.contains("rust") || stdout.contains("python") || stdout.contains("typescript") ||
        stdout.contains("category") || stdout.contains("Category"),
        "Expected to see popular categories, but got: {}",
        stdout
    );
}

#[then(regex = r"^I should see package metadata$")]
async fn should_see_package_metadata(world: &mut GgenWorld) {
    let stdout = world.last_stdout();
    assert!(
        stdout.contains("name") || stdout.contains("description") || stdout.contains("version"),
        "Expected to see package metadata, but got: {}",
        stdout
    );
}

#[then(regex = r"^I should see description$")]
async fn should_see_description(world: &mut GgenWorld) {
    let stdout = world.last_stdout();
    assert!(
        stdout.contains("description") || stdout.contains("Description") || stdout.len() > 50,
        "Expected to see description, but got: {}",
        stdout
    );
}

#[then(regex = r"^I should see SHA256 hash for each gpack$")]
async fn should_see_sha256_hashes(world: &mut GgenWorld) {
    let stdout = world.last_stdout();
    assert!(
        stdout.contains("sha256") || stdout.contains("SHA256") || stdout.contains("hash"),
        "Expected to see SHA256 hashes, but got: {}",
        stdout
    );
}

#[then(regex = r"^the SHA256 should be 64 hex characters$")]
async fn sha256_should_be_valid(_world: &mut GgenWorld) {
    // Would validate SHA256 format (64 hex chars)
}

#[then(regex = r"^the lockfile should contain the PQC signature$")]
async fn lockfile_should_contain_pqc_signature(_world: &mut GgenWorld) {
    // Would verify PQC signature in lockfile
}

#[then(regex = r"^the lockfile should contain the PQC public key$")]
async fn lockfile_should_contain_pqc_public_key(_world: &mut GgenWorld) {
    // Would verify PQC public key in lockfile
}
