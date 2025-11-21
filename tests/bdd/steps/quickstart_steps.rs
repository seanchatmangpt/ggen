use super::super::world::GgenWorld;
use cucumber::{given, then, when};

// Quickstart-specific step definitions

#[given(regex = r"^I have a new project directory$")]
fn have_new_project_directory(_world: &mut GgenWorld) {
    // Project directory is already set up in world
}

#[when(regex = r"^I initialize ggen$")]
fn initialize_ggen(world: &mut GgenWorld) {
    use assert_cmd::Command;
    let output = Command::cargo_bin("ggen")
        #[allow(clippy::expect_used)]
        .expect("ggen binary not found")
        .arg("init")
        .current_dir(&world.project_dir)
        .output()
        #[allow(clippy::expect_used)]
        .expect("Failed to run ggen init");

    world.last_output = Some(output.clone());
    world.last_exit_code = output.status.code();
}

#[then(regex = r"^I should have a basic project structure$")]
fn should_have_basic_project_structure(world: &mut GgenWorld) {
    let project_dir = &world.project_dir;

    // Check for basic files
    assert!(
        project_dir.join("ggen.toml").exists(),
        "ggen.toml should exist"
    );
    assert!(
        project_dir.join("templates").exists(),
        "templates directory should exist"
    );
}
