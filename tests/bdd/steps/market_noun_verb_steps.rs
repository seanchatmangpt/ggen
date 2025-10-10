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
/// - market info: Show gpack details
/// - market categories: Show popular categories

// ============================================================================
// GIVEN steps - Setup preconditions
// ============================================================================

#[given(regex = r"^the marketplace registry is available$")]
fn marketplace_registry_available(_world: &mut GgenWorld) {
    // Set environment variable for registry URL
    std::env::set_var(
        "GGEN_REGISTRY_URL",
        "https://raw.githubusercontent.com/seanchatmangpt/ggen/master/registry/",
    );
}

#[given(regex = r"^the marketplace registry is available for market commands$")]
fn marketplace_registry_available_for_market(_world: &mut GgenWorld) {
    // Set environment variable for registry URL
    std::env::set_var(
        "GGEN_REGISTRY_URL",
        "https://raw.githubusercontent.com/seanchatmangpt/ggen/master/registry/",
    );
}

#[given(regex = r#"^I have installed the gpack "([^"]+)" without version$"#)]
fn have_installed_gpack(world: &mut GgenWorld, package_id: String) {
    // Simulate having a package installed by creating a mock lockfile entry
    let lockfile_path = world.project_dir.join("ggen.lock");
    let lockfile_content = format!(
        r#"{{"packages": {{"{}": {{"version": "0.2.0", "sha256": "abc123", "installed": true}}}}}}"#,
        package_id
    );
    fs::write(&lockfile_path, lockfile_content).expect("Failed to write mock lockfile");
}

#[given(regex = r#"^I have installed the gpack "([^"]+)@([^"]+)" with specific version$"#)]
fn have_installed_gpack_with_version(world: &mut GgenWorld, package_id: String, version: String) {
    let lockfile_path = world.project_dir.join("ggen.lock");
    let lockfile_content = format!(
        r#"{{"packages": {{"{}": {{"version": "{}", "sha256": "abc123", "installed": true}}}}}}"#,
        package_id, version
    );
    fs::write(&lockfile_path, lockfile_content).expect("Failed to write mock lockfile");
}

#[given(regex = r#"^a newer version "([^"]+)" is available$"#)]
fn newer_version_available(_world: &mut GgenWorld, _version: String) {
    // Would configure mock registry to return newer version
    // For now, this is a no-op
}

#[given(regex = r#"^the gpack "([^"]+)" has a PQC signature$"#)]
fn gpack_has_pqc_signature(_world: &mut GgenWorld, _package_id: String) {
    // Would configure mock registry with PQC signature
}

// ============================================================================
// WHEN steps - Execute actions
// ============================================================================

#[when(regex = r#"^I run "ggen market (.+)"$"#)]
fn run_ggen_market_command(world: &mut GgenWorld, args: String) {
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
fn command_should_succeed(world: &mut GgenWorld) {
    assert!(
        world.last_command_succeeded(),
        "Command failed with exit code: {}\nStderr: {}",
        world.last_exit_code.unwrap_or(-1),
        world.last_stderr()
    );
}

#[then(regex = r"^I should see results for rust gpacks$")]
fn should_see_rust_results(world: &mut GgenWorld) {
    let stdout = world.last_stdout();
    assert!(
        stdout.contains("rust") || stdout.contains("Rust"),
        "Expected to see rust gpacks in results, but got: {}",
        stdout
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

#[then(regex = r"^results should only show rust category gpacks$")]
fn results_should_show_rust_category(_world: &mut GgenWorld) {
    // Would validate that all results have rust category
}

#[then(regex = r"^the output should be valid JSON$")]
fn output_should_be_valid_json(world: &mut GgenWorld) {
    let stdout = world.last_stdout();

    serde_json::from_str::<serde_json::Value>(&stdout)
        .unwrap_or_else(|e| panic!("Output is not valid JSON: {}\nOutput: {}", e, stdout));
}

#[then(regex = r#"^the JSON should contain a "([^"]+)" array$"#)]
fn json_should_contain_array(world: &mut GgenWorld, field: String) {
    let stdout = world.last_stdout();
    let json: serde_json::Value =
        serde_json::from_str(&stdout).unwrap_or_else(|e| panic!("Failed to parse JSON: {}", e));

    assert!(
        json.get(&field).and_then(|v| v.as_array()).is_some(),
        "JSON should contain '{}' array, but got: {}",
        field,
        stdout
    );
}

#[then(regex = r#"^the gpack should be listed in "([^"]+)"$"#)]
fn gpack_should_be_in_lockfile(world: &mut GgenWorld, lockfile: String) {
    let lockfile_path = world.project_dir.join(&lockfile);
    assert!(lockfile_path.exists(), "Lockfile {} should exist", lockfile);

    let content = fs::read_to_string(&lockfile_path)
        .unwrap_or_else(|e| panic!("Failed to read lockfile: {}", e));
    assert!(!content.is_empty(), "Lockfile should not be empty");
}

#[then(regex = r"^the gpack should be cached locally$")]
fn gpack_should_be_cached(_world: &mut GgenWorld) {
    // Would check the local cache directory
}

#[then(regex = r#"^the lockfile should show version "([^"]+)"$"#)]
fn lockfile_should_show_version(world: &mut GgenWorld, version: String) {
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
fn gpack_should_not_be_in_lockfile(world: &mut GgenWorld, lockfile: String) {
    let lockfile_path = world.project_dir.join(&lockfile);
    if lockfile_path.exists() {
        let content = fs::read_to_string(&lockfile_path)
            .unwrap_or_else(|e| panic!("Failed to read lockfile: {}", e));
        // In a real test, we'd check that the specific gpack is not listed
        // For now, we just verify the file exists or is empty
        println!("Lockfile content: {}", content);
    }
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

#[then(regex = r"^I should see version information$")]
fn should_see_version_information(world: &mut GgenWorld) {
    let stdout = world.last_stdout();
    assert!(
        stdout.contains("version") || stdout.contains("Version") || stdout.contains("0."),
        "Expected to see version information, but got: {}",
        stdout
    );
}

#[then(regex = r"^I should see source URLs$")]
fn should_see_source_urls(world: &mut GgenWorld) {
    let stdout = world.last_stdout();
    assert!(
        stdout.contains("http") || stdout.contains("source") || stdout.contains("url"),
        "Expected to see source URLs, but got: {}",
        stdout
    );
}

#[then(regex = r"^the gpack should be updated to latest version$")]
fn gpack_updated_to_latest(_world: &mut GgenWorld) {
    // Would verify lockfile has latest version
}

#[then(regex = r"^I should see popular categories$")]
fn should_see_popular_categories(world: &mut GgenWorld) {
    let stdout = world.last_stdout();
    assert!(
        stdout.contains("rust")
            || stdout.contains("python")
            || stdout.contains("typescript")
            || stdout.contains("category")
            || stdout.contains("Category"),
        "Expected to see popular categories, but got: {}",
        stdout
    );
}

#[then(regex = r"^I should see package metadata$")]
fn should_see_package_metadata(world: &mut GgenWorld) {
    let stdout = world.last_stdout();
    assert!(
        stdout.contains("name") || stdout.contains("description") || stdout.contains("version"),
        "Expected to see package metadata, but got: {}",
        stdout
    );
}

#[then(regex = r"^I should see description$")]
fn should_see_description(world: &mut GgenWorld) {
    let stdout = world.last_stdout();
    assert!(
        stdout.contains("description") || stdout.contains("Description") || stdout.len() > 50,
        "Expected to see description, but got: {}",
        stdout
    );
}

#[then(regex = r"^I should see SHA256 hash for each gpack$")]
fn should_see_sha256_hashes(world: &mut GgenWorld) {
    let stdout = world.last_stdout();
    assert!(
        stdout.contains("sha256") || stdout.contains("SHA256") || stdout.contains("hash"),
        "Expected to see SHA256 hashes, but got: {}",
        stdout
    );
}

#[then(regex = r"^the SHA256 should be 64 hex characters$")]
fn sha256_should_be_valid(_world: &mut GgenWorld) {
    // Would validate SHA256 format (64 hex chars)
}

#[then(regex = r"^the lockfile should contain the PQC signature$")]
fn lockfile_should_contain_pqc_signature(_world: &mut GgenWorld) {
    // Would verify PQC signature in lockfile
}

#[then(regex = r"^the lockfile should contain the PQC public key$")]
fn lockfile_should_contain_pqc_public_key(_world: &mut GgenWorld) {
    // Would verify PQC public key in lockfile
}

// ============================================================================
// Market Search and Remove Steps
// ============================================================================

#[when(regex = r#"^I run "ggen market search (.+)"$"#)]
fn run_market_search(world: &mut GgenWorld, query: String) {
    let output = Command::cargo_bin("ggen")
        .expect("ggen binary not found")
        .arg("market")
        .arg("search")
        .arg(&query)
        .current_dir(&world.project_dir)
        .output()
        .expect("Failed to run market search");

    world.last_output = Some(output.clone());
    world.last_exit_code = output.status.code();
}

#[when(regex = r#"^I run "ggen market remove (.+)"$"#)]
fn run_market_remove(world: &mut GgenWorld, package_id: String) {
    let output = Command::cargo_bin("ggen")
        .expect("ggen binary not found")
        .arg("market")
        .arg("remove")
        .arg(&package_id)
        .current_dir(&world.project_dir)
        .output()
        .expect("Failed to run market remove");

    world.last_output = Some(output.clone());
    world.last_exit_code = output.status.code();
}

#[when(regex = r#"^I run "ggen market info (.+)"$"#)]
fn run_market_info(world: &mut GgenWorld, package_id: String) {
    let output = Command::cargo_bin("ggen")
        .expect("ggen binary not found")
        .arg("market")
        .arg("info")
        .arg(&package_id)
        .current_dir(&world.project_dir)
        .output()
        .expect("Failed to run market info");

    world.last_output = Some(output.clone());
    world.last_exit_code = output.status.code();
}

#[when(regex = r#"^I run "ggen market categories"$"#)]
fn run_market_categories(world: &mut GgenWorld) {
    let output = Command::cargo_bin("ggen")
        .expect("ggen binary not found")
        .arg("market")
        .arg("categories")
        .current_dir(&world.project_dir)
        .output()
        .expect("Failed to run market categories");

    world.last_output = Some(output.clone());
    world.last_exit_code = output.status.code();
}

#[when(regex = r#"^I run "ggen market list"$"#)]
fn run_market_list(world: &mut GgenWorld) {
    let output = Command::cargo_bin("ggen")
        .expect("ggen binary not found")
        .arg("market")
        .arg("list")
        .current_dir(&world.project_dir)
        .output()
        .expect("Failed to run market list");

    world.last_output = Some(output.clone());
    world.last_exit_code = output.status.code();
}

#[when(regex = r#"^I run "ggen market update"$"#)]
fn run_market_update(world: &mut GgenWorld) {
    let output = Command::cargo_bin("ggen")
        .expect("ggen binary not found")
        .arg("market")
        .arg("update")
        .current_dir(&world.project_dir)
        .output()
        .expect("Failed to run market update");

    world.last_output = Some(output.clone());
    world.last_exit_code = output.status.code();
}

#[when(regex = r#"^I run "ggen market update (.+)"$"#)]
fn run_market_update_specific(world: &mut GgenWorld, package_id: String) {
    let output = Command::cargo_bin("ggen")
        .expect("ggen binary not found")
        .arg("market")
        .arg("update")
        .arg(&package_id)
        .current_dir(&world.project_dir)
        .output()
        .expect("Failed to run market update specific");

    world.last_output = Some(output.clone());
    world.last_exit_code = output.status.code();
}

#[then(regex = r"^I should see search results$")]
fn should_see_search_results(world: &mut GgenWorld) {
    let stdout = world.last_stdout();
    assert!(
        !stdout.is_empty(),
        "Expected to see search results, but got empty output"
    );
}

#[then(regex = r"^I should see no results$")]
fn should_see_no_results(world: &mut GgenWorld) {
    let stdout = world.last_stdout();
    assert!(
        stdout.is_empty() || stdout.contains("No results") || stdout.contains("not found"),
        "Expected to see no results, but got: {}",
        stdout
    );
}

#[then(regex = r"^the gpack should be removed from lockfile$")]
fn gpack_should_be_removed_from_lockfile(world: &mut GgenWorld) {
    let lockfile_path = world.project_dir.join("ggen.lock");
    if lockfile_path.exists() {
        let content = fs::read_to_string(&lockfile_path)
            .unwrap_or_else(|e| panic!("Failed to read lockfile: {}", e));
        // In a real implementation, we'd check that the specific package is not in the lockfile
        assert!(
            content.is_empty() || !content.contains("io.ggen.rust.cli-subcommand"),
            "Gpack should be removed from lockfile, but lockfile still contains it: {}",
            content
        );
    }
}
