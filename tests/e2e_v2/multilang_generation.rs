// Scenario 5: Multi-language template generation
// Chicago TDD: Verify REAL multi-language support

use assert_cmd::Command;
use std::fs;

use super::test_helpers::*;

#[test]
fn test_rust_project_generation() {
    let workspace = setup_workspace().unwrap();

    Command::cargo_bin("ggen")
        .unwrap()
        .arg("project")
        .arg("new")
        .arg("rust-app")
        .current_dir(workspace.path())
        .assert()
        .success();

    // Verify Rust-specific files
    let cargo_toml = workspace.path().join("rust-app/Cargo.toml");
    assert!(cargo_toml.exists(), "Cargo.toml should exist");

    let main_rs = workspace.path().join("rust-app/src/main.rs");
    assert!(main_rs.exists(), "src/main.rs should exist");

    // Verify it's actually Rust code
    verify_file_contains(&cargo_toml, "[package]").unwrap();
    verify_file_contains(&main_rs, "fn main()").unwrap();

    // Verify project builds
    verify_rust_project_builds(&workspace.path().join("rust-app")).unwrap();

    println!("✅ Rust project generation: PASSED");
}

#[test]
fn test_python_project_structure() {
    let workspace = setup_workspace().unwrap();

    // Create a Python-style project template
    let templates_dir = workspace.path().join("templates");
    fs::create_dir_all(&templates_dir).unwrap();

    let pyproject_template = r#"[project]
name = "{{ project_name }}"
version = "0.1.0"
dependencies = []
"#;

    let template_file = templates_dir.join("pyproject.tmpl");
    fs::write(&template_file, pyproject_template).unwrap();

    let output_dir = workspace.path().join("python-app");
    fs::create_dir_all(&output_dir).unwrap();

    // Generate Python project config
    Command::cargo_bin("ggen")
        .unwrap()
        .arg("template")
        .arg("render")
        .arg(&template_file)
        .arg("--var")
        .arg("project_name=my-python-app")
        .arg("--output")
        .arg(output_dir.join("pyproject.toml"))
        .current_dir(workspace.path())
        .assert()
        .success();

    // Verify Python project structure
    let pyproject = output_dir.join("pyproject.toml");
    assert!(pyproject.exists(), "pyproject.toml should exist");

    verify_file_contains(&pyproject, "my-python-app").unwrap();
    verify_file_contains(&pyproject, "[project]").unwrap();

    println!("✅ Python project structure: PASSED");
}

#[test]
fn test_javascript_project_structure() {
    let workspace = setup_workspace().unwrap();

    // Create a JavaScript/Node project template
    let templates_dir = workspace.path().join("templates");
    fs::create_dir_all(&templates_dir).unwrap();

    let package_template = r#"{
  "name": "{{ project_name }}",
  "version": "1.0.0",
  "type": "module",
  "scripts": {
    "start": "node index.js"
  }
}
"#;

    let template_file = templates_dir.join("package.tmpl");
    fs::write(&template_file, package_template).unwrap();

    let output_dir = workspace.path().join("js-app");
    fs::create_dir_all(&output_dir).unwrap();

    // Generate package.json
    Command::cargo_bin("ggen")
        .unwrap()
        .arg("template")
        .arg("render")
        .arg(&template_file)
        .arg("--var")
        .arg("project_name=my-js-app")
        .arg("--output")
        .arg(output_dir.join("package.json"))
        .current_dir(workspace.path())
        .assert()
        .success();

    // Verify JavaScript project structure
    let package_json = output_dir.join("package.json");
    assert!(package_json.exists(), "package.json should exist");

    verify_file_contains(&package_json, "my-js-app").unwrap();
    verify_file_contains(&package_json, "\"type\": \"module\"").unwrap();

    println!("✅ JavaScript project structure: PASSED");
}

#[test]
fn test_multi_file_project_generation() {
    let workspace = setup_workspace().unwrap();

    // Generate a project (should create multiple files)
    Command::cargo_bin("ggen")
        .unwrap()
        .arg("project")
        .arg("new")
        .arg("multi-file-app")
        .current_dir(workspace.path())
        .assert()
        .success();

    let project_dir = workspace.path().join("multi-file-app");

    // Verify multiple files created
    assert!(project_dir.join("Cargo.toml").exists());
    assert!(project_dir.join("src").exists());
    assert!(project_dir.join("src/main.rs").exists());

    // Count files generated
    let file_count = std::fs::read_dir(&project_dir)
        .unwrap()
        .count();

    assert!(file_count >= 2, "Should generate multiple files");

    println!("✅ Multi-file project generation: PASSED ({} files)", file_count);
}
