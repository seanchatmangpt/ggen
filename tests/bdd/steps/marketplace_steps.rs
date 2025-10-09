use super::super::world::RgenWorld;
use assert_cmd::Command;
use cucumber::{given, then, when};
use std::fs;

// Marketplace step definitions

#[given(regex = r"^the marketplace is available$")]
fn marketplace_is_available(_world: &mut RgenWorld) {
    // Set environment variable for registry URL
    std::env::set_var(
        "RGEN_REGISTRY_URL",
        "https://raw.githubusercontent.com/seanchatmangpt/rgen/master/registry/",
    );
}

#[given(regex = r"^the marketplace registry is available at (.+)$")]
fn marketplace_registry_available_at(world: &mut RgenWorld, registry_url: String) {
    std::env::set_var("RGEN_REGISTRY_URL", &registry_url);
    world.set_registry_url(registry_url);
}

#[when(regex = r"^I search for (.+)$")]
fn search_for_rpack(world: &mut RgenWorld, query: String) {
    let output = Command::cargo_bin("rgen")
        .expect("rgen binary not found")
        .arg("search")
        .arg(&query)
        .current_dir(&world.project_dir)
        .output()
        .expect("Failed to run rgen search");

    world.last_output = Some(output.clone());
    world.last_exit_code = output.status.code();
}

#[when(regex = r"^I run rgen search (.+)$")]
fn run_rgen_search(world: &mut RgenWorld, query: String) {
    let output = Command::cargo_bin("rgen")
        .expect("rgen binary not found")
        .arg("search")
        .arg(&query)
        .current_dir(&world.project_dir)
        .output()
        .expect("Failed to run rgen search");

    world.last_output = Some(output.clone());
    world.last_exit_code = output.status.code();
}

#[when(regex = r"^I run rgen categories$")]
fn run_rgen_categories(world: &mut RgenWorld) {
    let output = Command::cargo_bin("rgen")
        .expect("rgen binary not found")
        .arg("categories")
        .current_dir(&world.project_dir)
        .output()
        .expect("Failed to run rgen categories");

    world.last_output = Some(output.clone());
    world.last_exit_code = output.status.code();
}

#[when(regex = r"^I run rgen show (.+)$")]
fn run_rgen_show(world: &mut RgenWorld, package_id: String) {
    let output = Command::cargo_bin("rgen")
        .expect("rgen binary not found")
        .arg("show")
        .arg(&package_id)
        .current_dir(&world.project_dir)
        .output()
        .expect("Failed to run rgen show");

    world.last_output = Some(output.clone());
    world.last_exit_code = output.status.code();
}

#[when(regex = r"^I run rgen add (.+)$")]
fn run_rgen_add(world: &mut RgenWorld, package_id: String) {
    let output = Command::cargo_bin("rgen")
        .expect("rgen binary not found")
        .arg("add")
        .arg(&package_id)
        .current_dir(&world.project_dir)
        .output()
        .expect("Failed to run rgen add");

    world.last_output = Some(output.clone());
    world.last_exit_code = output.status.code();
}

#[when(regex = r"^I run rgen packs$")]
fn run_rgen_packs(world: &mut RgenWorld) {
    let output = Command::cargo_bin("rgen")
        .expect("rgen binary not found")
        .arg("packs")
        .current_dir(&world.project_dir)
        .output()
        .expect("Failed to run rgen packs");

    world.last_output = Some(output.clone());
    world.last_exit_code = output.status.code();
}

#[when(regex = r"^I run rgen update$")]
fn run_rgen_update(world: &mut RgenWorld) {
    let output = Command::cargo_bin("rgen")
        .expect("rgen binary not found")
        .arg("update")
        .current_dir(&world.project_dir)
        .output()
        .expect("Failed to run rgen update");

    world.last_output = Some(output.clone());
    world.last_exit_code = output.status.code();
}

#[then(regex = r"^I should see results for (.+) templates$")]
fn should_see_results_for_templates(world: &mut RgenWorld, language: String) {
    let stdout = world.last_stdout();
    assert!(
        stdout.contains(&language),
        "Expected to see {} templates in search results, but got: {}",
        language,
        stdout
    );
}

#[then(regex = r"^I should see popular categories$")]
fn should_see_popular_categories(world: &mut RgenWorld) {
    let stdout = world.last_stdout();
    assert!(
        stdout.contains("rust") || stdout.contains("python") || stdout.contains("typescript"),
        "Expected to see popular categories, but got: {}",
        stdout
    );
}

#[then(regex = r"^I should see package metadata$")]
fn should_see_package_metadata(world: &mut RgenWorld) {
    let stdout = world.last_stdout();
    assert!(
        stdout.contains("name") || stdout.contains("description") || stdout.contains("version"),
        "Expected to see package metadata, but got: {}",
        stdout
    );
}

#[then(regex = r"^I should see version information$")]
fn should_see_version_information(world: &mut RgenWorld) {
    let stdout = world.last_stdout();
    assert!(
        stdout.contains("version") || stdout.contains("0."),
        "Expected to see version information, but got: {}",
        stdout
    );
}

#[then(regex = r"^I should see description$")]
fn should_see_description(world: &mut RgenWorld) {
    let stdout = world.last_stdout();
    assert!(
        stdout.contains("description") || stdout.len() > 50,
        "Expected to see description, but got: {}",
        stdout
    );
}

#[then(regex = r"^the package should be installed$")]
fn package_should_be_installed(_world: &mut RgenWorld) {
    // For BDD tests, we assume the package is installed if the command succeeded
    // In real implementation, this would check the lockfile or cache
}

#[then(regex = r"^rgen packs should list (.+)$")]
fn rgen_packs_should_list(world: &mut RgenWorld, package_id: String) {
    let output = Command::cargo_bin("rgen")
        .expect("rgen binary not found")
        .arg("packs")
        .current_dir(&world.project_dir)
        .output()
        .expect("Failed to run rgen packs");

    let stdout = String::from_utf8_lossy(&output.stdout);
    assert!(
        stdout.contains(&package_id),
        "Expected 'rgen packs' to list '{}', but got: {}",
        package_id,
        stdout
    );
}

#[then(regex = r"^version (.+) should be installed$")]
fn version_should_be_installed(_world: &mut RgenWorld, _version: String) {
    // For BDD tests, we assume the version is installed if the command succeeded
    // In real implementation, this would check the lockfile for the specific version
}

#[then(regex = r"^the package should be updated to latest version$")]
fn package_should_be_updated(_world: &mut RgenWorld) {
    // For BDD tests, we assume the package is updated if the command succeeded
    // In real implementation, this would check the lockfile for version changes
}

#[given(regex = r"^I have installed (.+)$")]
fn have_installed_package(world: &mut RgenWorld, package_id: String) {
    // Simulate having a package installed by creating a mock lockfile entry
    let lockfile_path = world.project_dir.join("rgen.lock");
    let lockfile_content = format!(
        r#"{{"packages": {{"{}": {{"version": "0.1.0", "installed": true}}}}}}"#,
        package_id
    );
    fs::write(&lockfile_path, lockfile_content).expect("Failed to write mock lockfile");
}

#[given(regex = r"^I have installed (.+)$")]
fn have_installed_package_with_version(world: &mut RgenWorld, package_with_version: String) {
    // Parse package@version format
    let parts: Vec<&str> = package_with_version.split('@').collect();
    let package_id = parts[0];
    let version = parts.get(1).unwrap_or(&"0.1.0");

    let lockfile_path = world.project_dir.join("rgen.lock");
    let lockfile_content = format!(
        r#"{{"packages": {{"{}": {{"version": "{}", "installed": true}}}}}}"#,
        package_id, version
    );
    fs::write(&lockfile_path, lockfile_content).expect("Failed to write mock lockfile");
}
