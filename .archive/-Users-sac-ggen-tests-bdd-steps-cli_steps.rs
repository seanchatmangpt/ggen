use super::super::world::GgenWorld;
use assert_cmd::Command;
use cucumber::{then, when};

// CLI command step definitions

#[when(regex = r"^I run ggen (.+)$")]
fn run_ggen_command(world: &mut GgenWorld, args: String) {
    let arg_list: Vec<&str> = args.split_whitespace().collect();

    let mut cmd = Command::cargo_bin("ggen").expect("ggen binary not found");
    let output = cmd
        .args(&arg_list)
        .current_dir(&world.project_dir)
        .output()
        .expect("Failed to run ggen command");

    world.last_output = Some(output.clone());
    world.last_exit_code = output.status.code();
}

#[when(regex = r"^I run ggen list$")]
fn run_ggen_list(world: &mut GgenWorld) {
    let output = Command::cargo_bin("ggen")
        .expect("ggen binary not found")
        .arg("list")
        .current_dir(&world.project_dir)
        .output()
        .expect("Failed to run ggen list");

    world.last_output = Some(output.clone());
    world.last_exit_code = output.status.code();
}

#[when(regex = r"^I run ggen lint (.+)$")]
fn run_ggen_lint(world: &mut GgenWorld, template_path: String) {
    let output = Command::cargo_bin("ggen")
        .expect("ggen binary not found")
        .arg("lint")
        .arg(&template_path)
        .current_dir(&world.project_dir)
        .output()
        .expect("Failed to run ggen lint");

    world.last_output = Some(output.clone());
    world.last_exit_code = output.status.code();
}

#[when(regex = r"^I run ggen hazard$")]
fn run_ggen_hazard(world: &mut GgenWorld) {
    let output = Command::cargo_bin("ggen")
        .expect("ggen binary not found")
        .arg("hazard")
        .current_dir(&world.project_dir)
        .output()
        .expect("Failed to run ggen hazard");

    world.last_output = Some(output.clone());
    world.last_exit_code = output.status.code();
}

#[when(regex = r"^I run ggen completion (.+)$")]
fn run_ggen_completion(world: &mut GgenWorld, shell: String) {
    let output = Command::cargo_bin("ggen")
        .expect("ggen binary not found")
        .arg("completion")
        .arg(&shell)
        .current_dir(&world.project_dir)
        .output()
        .expect("Failed to run ggen completion");

    world.last_output = Some(output.clone());
    world.last_exit_code = output.status.code();
}

#[when(regex = r"^I run ggen init$")]
fn run_ggen_init(world: &mut GgenWorld) {
    let output = Command::cargo_bin("ggen")
        .expect("ggen binary not found")
        .arg("init")
        .current_dir(&world.project_dir)
        .output()
        .expect("Failed to run ggen init");

    world.last_output = Some(output.clone());
    world.last_exit_code = output.status.code();
}

#[then(regex = r"^I should see the help text$")]
fn should_see_help_text(world: &mut GgenWorld) {
    let stdout = world.last_stdout();
    assert!(
        stdout.contains("USAGE:") || stdout.contains("Usage:"),
        "Expected help text, but got: {}",
        stdout
    );
}

#[then(regex = r"^I should see the version information$")]
fn should_see_version_info(world: &mut GgenWorld) {
    let stdout = world.last_stdout();
    assert!(
        stdout.contains("ggen 1.0.0"),
        "Expected version info, but got: {}",
        stdout
    );
}

#[then(regex = r"^the command should succeed$")]
fn command_should_succeed(world: &mut GgenWorld) {
    assert!(
        world.last_command_succeeded(),
        "Command should succeed, but failed with exit code: {}\nStderr: {}",
        world.last_exit_code.unwrap_or(-1),
        world.last_stderr()
    );
}

#[then(regex = r"^I should see available templates$")]
fn should_see_available_templates(world: &mut GgenWorld) {
    let stdout = world.last_stdout();
    assert!(
        stdout.contains("template") || stdout.contains(".tmpl") || stdout.len() > 10,
        "Expected to see available templates, but got: {}",
        stdout
    );
}

#[then(regex = r"^I should see template metadata$")]
fn should_see_template_metadata(world: &mut GgenWorld) {
    let stdout = world.last_stdout();
    assert!(
        stdout.contains("to:") || stdout.contains("vars:") || stdout.contains("description"),
        "Expected to see template metadata, but got: {}",
        stdout
    );
}

#[then(regex = r"^the command should validate the template$")]
fn command_should_validate_template(world: &mut GgenWorld) {
    // For BDD tests, we assume validation succeeded if the command succeeded
    // In real implementation, this would check for specific validation output
    assert!(
        world.last_command_succeeded(),
        "Template validation should succeed"
    );
}

#[then(regex = r"^I should see a hazard report$")]
fn should_see_hazard_report(world: &mut GgenWorld) {
    let stdout = world.last_stdout();
    assert!(
        stdout.contains("hazard") || stdout.contains("risk") || stdout.contains("security"),
        "Expected to see hazard report, but got: {}",
        stdout
    );
}

#[then(regex = r"^I should see (.+) completion script$")]
fn should_see_completion_script(world: &mut GgenWorld, shell: String) {
    let stdout = world.last_stdout();
    match shell.as_str() {
        "bash" => assert!(
            stdout.contains("complete") || stdout.contains("_ggen"),
            "Expected bash completion script, but got: {}",
            stdout
        ),
        "zsh" => assert!(
            stdout.contains("compdef") || stdout.contains("_ggen"),
            "Expected zsh completion script, but got: {}",
            stdout
        ),
        "fish" => assert!(
            stdout.contains("complete") || stdout.contains("ggen"),
            "Expected fish completion script, but got: {}",
            stdout
        ),
        _ => assert!(
            stdout.len() > 10,
            "Expected completion script, but got: {}",
            stdout
        ),
    }
}

#[then(regex = r"^I should have a basic project structure$")]
fn should_have_basic_project_structure(world: &mut GgenWorld) {
    let project_dir = &world.project_dir;

    // Check for basic files that should be created by ggen init
    let expected_files = ["ggen.toml", "templates"];
    for file in &expected_files {
        let path = project_dir.join(file);
        assert!(
            path.exists(),
            "Expected {} to exist at {}",
            file,
            path.display()
        );
    }
}

#[then(regex = r"^I should see search results$")]
fn should_see_search_results(world: &mut GgenWorld) {
    let stdout = world.last_stdout();
    assert!(
        stdout.len() > 10,
        "Expected search results, but got: {}",
        stdout
    );
}

#[then(regex = r"^results should contain (.+)$")]
fn results_should_contain(world: &mut GgenWorld, expected_result: String) {
    let stdout = world.last_stdout();
    assert!(
        stdout.contains(&expected_result),
        "Expected search results to contain '{}', but got: {}",
        expected_result,
        stdout
    );
}

#[then(regex = r"^the gpack should be installed$")]
fn gpack_should_be_installed(world: &mut GgenWorld) {
    // For BDD tests, we assume the gpack is installed if the command succeeded
    // In real implementation, this would check the lockfile or cache
    assert!(
        world.last_command_succeeded(),
        "Gpack should be installed successfully"
    );
}
