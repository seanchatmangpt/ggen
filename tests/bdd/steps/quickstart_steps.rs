use super::super::world::RgenWorld;
use cucumber::{given, then, when};

// Quickstart-specific step definitions

#[given(regex = r"^I have a new project directory$")]
fn have_new_project_directory(_world: &mut RgenWorld) {
    // Project directory is already set up in world
}

#[when(regex = r"^I initialize rgen$")]
fn initialize_rgen(world: &mut RgenWorld) {
    use assert_cmd::Command;
    let output = Command::cargo_bin("rgen")
        .expect("rgen binary not found")
        .arg("init")
        .current_dir(&world.project_dir)
        .output()
        .expect("Failed to run rgen init");

    world.last_output = Some(output.clone());
    world.last_exit_code = output.status.code();
}

#[then(regex = r"^I should have a basic project structure$")]
fn should_have_basic_project_structure(world: &mut RgenWorld) {
    let project_dir = &world.project_dir;

    // Check for basic files
    assert!(
        project_dir.join("rgen.toml").exists(),
        "rgen.toml should exist"
    );
    assert!(
        project_dir.join("templates").exists(),
        "templates directory should exist"
    );
}
