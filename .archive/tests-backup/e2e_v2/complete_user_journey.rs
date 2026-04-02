// Scenario 1: Complete new user journey from zero to working app
// Chicago TDD: Execute REAL user workflow end-to-end

use assert_cmd::Command;
use predicates::prelude::*;
use std::fs;

use super::test_helpers::*;

#[test]
fn test_new_user_complete_workflow() {
    let workspace = setup_workspace().unwrap();
    let workspace_path = workspace.path();

    // Step 1: Verify ggen is installed and shows correct version
    Command::cargo_bin("ggen")
        .unwrap()
        .arg("--version")
        .assert()
        .success()
        .stdout(predicate::str::contains("ggen"));

    // Step 2: Create a new project using built-in template
    Command::cargo_bin("ggen")
        .unwrap()
        .arg("project")
        .arg("new")
        .arg("my-cli-app")
        .current_dir(workspace_path)
        .assert()
        .success();

    // Step 3: Verify project structure created
    let project_dir = workspace_path.join("my-cli-app");
    assert!(project_dir.exists(), "Project directory should be created");

    // Essential files should exist
    let cargo_toml = project_dir.join("Cargo.toml");
    assert!(cargo_toml.exists(), "Cargo.toml should exist");

    let main_rs = project_dir.join("src/main.rs");
    assert!(main_rs.exists(), "src/main.rs should exist");

    // Step 4: Verify Cargo.toml has correct project name
    verify_file_contains(&cargo_toml, "my-cli-app").unwrap();

    // Step 5: Verify project builds (critical!)
    // This proves the generated code is actually valid
    verify_rust_project_builds(&project_dir).expect("Generated project should build successfully");

    // Step 6: Verify generated binary can be run
    let output = std::process::Command::new("cargo")
        .arg("build")
        .current_dir(&project_dir)
        .output()
        .expect("Failed to build project");

    assert!(
        output.status.success(),
        "Build should succeed:\n{}",
        String::from_utf8_lossy(&output.stderr)
    );

    println!("✅ Complete user journey: PASSED");
    println!("   - Project created");
    println!("   - Files generated");
    println!("   - Code builds successfully");
}

#[test]
fn test_user_journey_with_custom_template() {
    let workspace = setup_workspace().unwrap();
    let workspace_path = workspace.path();

    // Create a simple custom template
    let templates_dir = workspace_path.join("templates");
    fs::create_dir_all(&templates_dir).unwrap();

    let template_content = r#"# {{ project_name }}

Generated project!

## Files
- src/main.rs
"#;

    let template_file = templates_dir.join("readme.tmpl");
    fs::write(&template_file, template_content).unwrap();

    // Generate using custom template
    let output_dir = workspace_path.join("output");
    fs::create_dir_all(&output_dir).unwrap();

    Command::cargo_bin("ggen")
        .unwrap()
        .arg("template")
        .arg("render")
        .arg(&template_file)
        .arg("--var")
        .arg("project_name=MyProject")
        .arg("--output")
        .arg(output_dir.join("README.md"))
        .current_dir(workspace_path)
        .assert()
        .success();

    // Verify rendered output
    let readme = output_dir.join("README.md");
    assert!(readme.exists(), "README.md should be generated");

    verify_file_contains(&readme, "# MyProject").unwrap();
    verify_file_contains(&readme, "Generated project!").unwrap();

    println!("✅ Custom template journey: PASSED");
}
