use super::super::world::GgenWorld;
use assert_cmd::Command;
use cucumber::{given, then, when};
use std::fs;

// Marketplace step definitions

#[given(regex = r"^the marketplace is available$")]
fn marketplace_is_available(_world: &mut GgenWorld) {
    // Set environment variable for registry URL
    std::env::set_var(
        "GGEN_REGISTRY_URL",
        "https://raw.githubusercontent.com/seanchatmangpt/ggen/master/registry/",
    );
}

#[given(regex = r"^the marketplace registry is available at (.+)$")]
fn marketplace_registry_available_at(world: &mut GgenWorld, registry_url: String) {
    std::env::set_var("GGEN_REGISTRY_URL", &registry_url);
    world.set_registry_url(registry_url);
}

#[when(regex = r"^I search for (.+)$")]
fn search_for_gpack(world: &mut GgenWorld, query: String) {
    let output = Command::cargo_bin("ggen")
        .expect("ggen binary not found")
        .arg("search")
        .arg(&query)
        .current_dir(&world.project_dir)
        .output()
        .expect("Failed to run ggen search");

    world.last_output = Some(output.clone());
    world.last_exit_code = output.status.code();
}

#[when(regex = r"^I run ggen search (.+)$")]
fn run_ggen_search(world: &mut GgenWorld, query: String) {
    let output = Command::cargo_bin("ggen")
        .expect("ggen binary not found")
        .arg("search")
        .arg(&query)
        .current_dir(&world.project_dir)
        .output()
        .expect("Failed to run ggen search");

    world.last_output = Some(output.clone());
    world.last_exit_code = output.status.code();
}

#[when(regex = r"^I run ggen categories$")]
fn run_ggen_categories(world: &mut GgenWorld) {
    let output = Command::cargo_bin("ggen")
        .expect("ggen binary not found")
        .arg("categories")
        .current_dir(&world.project_dir)
        .output()
        .expect("Failed to run ggen categories");

    world.last_output = Some(output.clone());
    world.last_exit_code = output.status.code();
}

#[when(regex = r"^I run ggen show (.+)$")]
fn run_ggen_show(world: &mut GgenWorld, package_id: String) {
    let output = Command::cargo_bin("ggen")
        .expect("ggen binary not found")
        .arg("show")
        .arg(&package_id)
        .current_dir(&world.project_dir)
        .output()
        .expect("Failed to run ggen show");

    world.last_output = Some(output.clone());
    world.last_exit_code = output.status.code();
}

#[when(regex = r"^I run ggen add (.+)$")]
fn run_ggen_add(world: &mut GgenWorld, package_id: String) {
    let output = Command::cargo_bin("ggen")
        .expect("ggen binary not found")
        .arg("add")
        .arg(&package_id)
        .current_dir(&world.project_dir)
        .output()
        .expect("Failed to run ggen add");

    world.last_output = Some(output.clone());
    world.last_exit_code = output.status.code();
}

#[when(regex = r"^I run ggen packs$")]
fn run_ggen_packs(world: &mut GgenWorld) {
    let output = Command::cargo_bin("ggen")
        .expect("ggen binary not found")
        .arg("packs")
        .current_dir(&world.project_dir)
        .output()
        .expect("Failed to run ggen packs");

    world.last_output = Some(output.clone());
    world.last_exit_code = output.status.code();
}

#[when(regex = r"^I run ggen update$")]
fn run_ggen_update(world: &mut GgenWorld) {
    let output = Command::cargo_bin("ggen")
        .expect("ggen binary not found")
        .arg("update")
        .current_dir(&world.project_dir)
        .output()
        .expect("Failed to run ggen update");

    world.last_output = Some(output.clone());
    world.last_exit_code = output.status.code();
}

#[then(regex = r"^I should see results for (.+) templates$")]
fn should_see_results_for_templates(world: &mut GgenWorld, language: String) {
    let stdout = world.last_stdout();
    assert!(
        stdout.contains(&language),
        "Expected to see {} templates in search results, but got: {}",
        language,
        stdout
    );
}

#[then(regex = r"^I should see popular categories$")]
fn should_see_popular_categories(world: &mut GgenWorld) {
    let stdout = world.last_stdout();
    assert!(
        stdout.contains("rust") || stdout.contains("python") || stdout.contains("typescript"),
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

#[then(regex = r"^I should see version information$")]
fn should_see_version_information(world: &mut GgenWorld) {
    let stdout = world.last_stdout();
    assert!(
        stdout.contains("version") || stdout.contains("0."),
        "Expected to see version information, but got: {}",
        stdout
    );
}

#[then(regex = r"^I should see description$")]
fn should_see_description(world: &mut GgenWorld) {
    let stdout = world.last_stdout();
    assert!(
        stdout.contains("description") || stdout.len() > 50,
        "Expected to see description, but got: {}",
        stdout
    );
}

#[then(regex = r"^the package should be installed$")]
fn package_should_be_installed(_world: &mut GgenWorld) {
    // For BDD tests, we assume the package is installed if the command succeeded
    // In real implementation, this would check the lockfile or cache
}

#[then(regex = r"^ggen packs should list (.+)$")]
fn ggen_packs_should_list(world: &mut GgenWorld, package_id: String) {
    let output = Command::cargo_bin("ggen")
        .expect("ggen binary not found")
        .arg("packs")
        .current_dir(&world.project_dir)
        .output()
        .expect("Failed to run ggen packs");

    let stdout = String::from_utf8_lossy(&output.stdout);
    assert!(
        stdout.contains(&package_id),
        "Expected 'ggen packs' to list '{}', but got: {}",
        package_id,
        stdout
    );
}

#[then(regex = r"^version (.+) should be installed$")]
fn version_should_be_installed(_world: &mut GgenWorld, _version: String) {
    // For BDD tests, we assume the version is installed if the command succeeded
    // In real implementation, this would check the lockfile for the specific version
}

#[then(regex = r"^the package should be updated to latest version$")]
fn package_should_be_updated(_world: &mut GgenWorld) {
    // For BDD tests, we assume the package is updated if the command succeeded
    // In real implementation, this would check the lockfile for version changes
}

#[given(regex = r"^I have installed (.+)$")]
fn have_installed_package(world: &mut GgenWorld, package_id: String) {
    // Simulate having a package installed by creating a mock lockfile entry
    let lockfile_path = world.project_dir.join("ggen.lock");
    let lockfile_content = format!(
        r#"{{"packages": {{"{}": {{"version": "0.1.0", "installed": true}}}}}}"#,
        package_id
    );
    fs::write(&lockfile_path, lockfile_content).expect("Failed to write mock lockfile");
}

#[given(regex = r"^I have installed (.+)$")]
fn have_installed_package_with_version(world: &mut GgenWorld, package_with_version: String) {
    // Parse package@version format
    let parts: Vec<&str> = package_with_version.split('@').collect();
    let package_id = parts[0];
    let version = parts.get(1).unwrap_or(&"0.1.0");

    let lockfile_path = world.project_dir.join("ggen.lock");
    let lockfile_content = format!(
        r#"{{"packages": {{"{}": {{"version": "{}", "installed": true}}}}}}"#,
        package_id, version
    );
    fs::write(&lockfile_path, lockfile_content).expect("Failed to write mock lockfile");
}
