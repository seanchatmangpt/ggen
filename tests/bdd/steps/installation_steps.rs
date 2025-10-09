use super::super::world::RgenWorld;
use assert_cmd::Command;
use cucumber::{given, then, when};
use std::process::Command as StdCommand;

// Installation-specific step definitions

#[given(regex = r"^I install rgen via cargo$")]
fn install_rgen_via_cargo(world: &mut RgenWorld) {
    // For BDD tests, we assume rgen is already built and available
    // In real scenarios, this would run: cargo install rgen
    let output = StdCommand::new("cargo")
        .args(&["install", "--path", "."])
        .current_dir(&world.project_dir)
        .output()
        .expect("Failed to run cargo install");

    world.last_output = Some(output.clone());
    world.last_exit_code = output.status.code();
}

#[given(regex = r"^I install rgen via homebrew$")]
fn install_rgen_via_homebrew(world: &mut RgenWorld) {
    // For BDD tests, we simulate homebrew installation
    // In real scenarios, this would run: brew install rgen
    let output = StdCommand::new("brew")
        .args(&["install", "rgen"])
        .output()
        .expect("Failed to run brew install");

    world.last_output = Some(output.clone());
    world.last_exit_code = output.status.code();
}

#[given(regex = r"^I clone the repository$")]
fn clone_repository(world: &mut RgenWorld) {
    // For BDD tests, we're already in the repository
    // In real scenarios, this would run: git clone https://github.com/seanchatmangpt/rgen.git
    world.project_dir = std::env::current_dir().expect("Failed to get current directory");
}

#[given(regex = r"^I have built from source$")]
fn have_built_from_source(world: &mut RgenWorld) {
    // Build the project
    let output = StdCommand::new("cargo")
        .args(&["build"])
        .current_dir(&world.project_dir)
        .output()
        .expect("Failed to run cargo build");

    assert!(
        output.status.success(),
        "cargo build failed: {}",
        String::from_utf8_lossy(&output.stderr)
    );
}

#[when(regex = r"^I run cargo make build$")]
fn run_cargo_make_build(world: &mut RgenWorld) {
    let output = StdCommand::new("cargo")
        .args(&["make", "build"])
        .current_dir(&world.project_dir)
        .output()
        .expect("Failed to run cargo make build");

    world.last_output = Some(output.clone());
    world.last_exit_code = output.status.code();
}

#[when(regex = r"^I run cargo make install$")]
fn run_cargo_make_install(world: &mut RgenWorld) {
    let output = StdCommand::new("cargo")
        .args(&["make", "install"])
        .current_dir(&world.project_dir)
        .output()
        .expect("Failed to run cargo make install");

    world.last_output = Some(output.clone());
    world.last_exit_code = output.status.code();
}

#[then(regex = r"^rgen should be installed$")]
fn rgen_should_be_installed(_world: &mut RgenWorld) {
    // Verify rgen binary exists and is executable
    let mut cmd = Command::cargo_bin("rgen").expect("rgen binary not found");
    let output = cmd
        .arg("--version")
        .output()
        .expect("Failed to run rgen --version");

    assert!(
        output.status.success(),
        "rgen --version failed: {}",
        String::from_utf8_lossy(&output.stderr)
    );
}

#[then(regex = r"^the binary (.+) should exist$")]
fn binary_should_exist(world: &mut RgenWorld, binary_path: String) {
    let full_path = world.project_dir.join(&binary_path);
    assert!(
        full_path.exists(),
        "Binary {} should exist at {}",
        binary_path,
        full_path.display()
    );
}

#[then(regex = r"^(.+) should be in PATH$")]
fn should_be_in_path(_world: &mut RgenWorld, binary_name: String) {
    let output = StdCommand::new("which")
        .arg(&binary_name)
        .output()
        .expect("Failed to run which command");

    assert!(output.status.success(), "{} should be in PATH", binary_name);
}

#[then(regex = r"^(.+) should work$")]
fn command_should_work(_world: &mut RgenWorld, command: String) {
    let args: Vec<&str> = command.split_whitespace().collect();
    let output = StdCommand::new(args[0])
        .args(&args[1..])
        .output()
        .expect(&format!("Failed to run {}", command));

    assert!(output.status.success(), "Command '{}' should work", command);
}
