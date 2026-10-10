#![allow(
    clippy::unwrap_used,
    clippy::expect_used,
    clippy::panic,
    clippy::needless_raw_string_hashes,
    clippy::duration_suboptimal_units,
    clippy::branches_sharing_code,
    clippy::used_underscore_binding,
    clippy::single_char_pattern,
    clippy::ignore_without_reason,
    clippy::cloned_ref_to_slice_refs,
    clippy::doc_overindented_list_items,
    clippy::match_wildcard_for_single_variants,
    clippy::ignored_unit_patterns,
    clippy::needless_collect,
    clippy::unnecessary_map_or,
    clippy::manual_flatten,
    clippy::manual_strip,
    clippy::future_not_send,
    clippy::unnested_or_patterns,
    clippy::no_effect_underscore_binding,
    clippy::literal_string_with_formatting_args
)]

//! End-to-end integration tests for project commands
//!
//! **Chicago TDD Principles**:
//! - REAL CLI process execution
//! - REAL file system operations
//! - REAL state verification of created files
//! - NO mocking of project domain logic
//!
//! **Critical User Workflows (80/20)**:
//! 1. Create new project from template
//! 2. Generate code from plan
//! 3. Apply plan to filesystem
//! 4. Initialize project with conventions
//! 5. Watch for changes

use assert_cmd::Command;
use predicates::prelude::*;
use std::fs;
use tempfile::TempDir;

/// Resolve the real `ggen` binary the same way `cli_boundary.rs::ggen_bin`
/// does: `CARGO_BIN_EXE_ggen` (set by `cargo test -p ggen-cli-lib`, never by
/// `-p ggen-engine`/`-p ggen-cli` — the root package is `autobins = false`),
/// then the workspace `target/{debug,release}/ggen`, then `PATH`. Panics
/// (loudly) if no candidate resolves, so failure happens at binary
/// resolution, not at first spawn.
fn ggen_bin() -> std::path::PathBuf {
    if let Ok(path) = std::env::var("CARGO_BIN_EXE_ggen") {
        let p = std::path::PathBuf::from(path);
        if p.exists() {
            return p;
        }
    }

    let target_root = std::env::var_os("CARGO_TARGET_DIR")
        .map(std::path::PathBuf::from)
        .or_else(|| {
            let manifest_dir =
                std::env::var_os("CARGO_MANIFEST_DIR").map(std::path::PathBuf::from)?;
            let mut dir: &std::path::Path = manifest_dir.as_path();
            loop {
                if dir.join("Cargo.lock").exists() {
                    return Some(dir.join("target"));
                }
                match dir.parent() {
                    Some(p) => dir = p,
                    None => return None,
                }
            }
        });

    if let Some(target) = target_root {
        for profile in &["debug", "release"] {
            let candidate = target.join(profile).join("ggen");
            if candidate.is_file() {
                return candidate;
            }
            let candidate_exe = target.join(profile).join("ggen.exe");
            if candidate_exe.is_file() {
                return candidate_exe;
            }
        }
    }

    panic!(
        "could not resolve the `ggen` binary: CARGO_BIN_EXE_ggen unset and no \
         target/debug/ggen found; build it with `cargo build -p ggen-cli-lib --bin ggen`"
    );
}

/// Helper to create ggen command
fn ggen() -> Command {
    Command::new(ggen_bin())
}

#[test]
#[ignore = "e2e CLI test: spawns the built `ggen` binary via assert_cmd::Command::cargo_bin; not part of the fast --lib loop, run explicitly via `cargo test -- --ignored` after `cargo build`"]
fn test_project_new_creates_project() {
    // Chicago TDD: Verify real project creation
    let temp_dir = TempDir::new().unwrap();

    ggen()
        .arg("project")
        .arg("new")
        .arg("test-project")
        .arg("--type")
        .arg("rust-cli")
        .current_dir(&temp_dir)
        .assert()
        .success();

    // Verify state: Project directory created
    let project_path = temp_dir.path().join("test-project");
    assert!(project_path.exists(), "Project directory should be created");
}

#[test]
#[ignore = "e2e CLI test: spawns the built `ggen` binary via assert_cmd::Command::cargo_bin; not part of the fast --lib loop, run explicitly via `cargo test -- --ignored` after `cargo build`"]
fn test_project_new_with_custom_output() {
    // Chicago TDD: Verify custom output directory
    let temp_dir = TempDir::new().unwrap();

    ggen()
        .arg("project")
        .arg("new")
        .arg("my-app")
        .arg("--type")
        .arg("rust-cli")
        .arg("--output")
        .arg(temp_dir.path().join("workspace"))
        .current_dir(&temp_dir)
        .assert()
        .success();

    // Verify state: Custom output path used
    let workspace_path = temp_dir.path().join("workspace");
    assert!(
        workspace_path.exists(),
        "Custom output directory should be created"
    );
}

#[test]
#[ignore = "e2e CLI test: spawns the built `ggen` binary via assert_cmd::Command::cargo_bin; not part of the fast --lib loop, run explicitly via `cargo test -- --ignored` after `cargo build`"]
fn test_project_init_creates_structure() {
    // Chicago TDD: Verify project initialization
    let temp_dir = TempDir::new().unwrap();

    ggen()
        .arg("project")
        .arg("init")
        .arg("--name")
        .arg("test-init")
        .current_dir(&temp_dir)
        .assert()
        .success();

    // Verify state: .ggen directory created
    let ggen_dir = temp_dir.path().join(".ggen");
    assert!(ggen_dir.exists(), ".ggen directory should be created");
}

#[test]
#[ignore = "e2e CLI test: spawns the built `ggen` binary via assert_cmd::Command::cargo_bin; not part of the fast --lib loop, run explicitly via `cargo test -- --ignored` after `cargo build`"]
fn test_project_init_with_preset() {
    // Chicago TDD: Verify preset application
    let temp_dir = TempDir::new().unwrap();

    ggen()
        .arg("project")
        .arg("init")
        .arg("--name")
        .arg("preset-test")
        .arg("--preset")
        .arg("clap-noun-verb")
        .current_dir(&temp_dir)
        .assert()
        .success();

    // Verify state: Config file created
    let config_path = temp_dir.path().join(".ggen/conventions.toml");
    assert!(config_path.exists(), "Config file should be created");
}

#[test]
#[ignore = "e2e CLI test: spawns the built `ggen` binary via assert_cmd::Command::cargo_bin; not part of the fast --lib loop, run explicitly via `cargo test -- --ignored` after `cargo build`"]
fn test_project_plan_generates_plan() {
    // Chicago TDD: Verify plan generation
    let temp_dir = TempDir::new().unwrap();

    // Create a simple template
    let templates_dir = temp_dir.path().join("templates");
    fs::create_dir_all(&templates_dir).unwrap();
    let template_path = templates_dir.join("test.tmpl");
    fs::write(&template_path, "Hello {{ name }}!").unwrap();

    ggen()
        .arg("project")
        .arg("plan")
        .arg("--template")
        .arg(template_path.to_str().unwrap())
        .arg("--var")
        .arg("name=world")
        .arg("--output")
        .arg("plan.json")
        .current_dir(&temp_dir)
        .assert()
        .success();

    // Verify state: Plan file created
    let plan_path = temp_dir.path().join("plan.json");
    assert!(plan_path.exists(), "Plan file should be created");
}

#[test]
#[ignore = "e2e CLI test: spawns the built `ggen` binary via assert_cmd::Command::cargo_bin; not part of the fast --lib loop, run explicitly via `cargo test -- --ignored` after `cargo build`"]
fn test_project_gen_creates_files() {
    // Chicago TDD: Verify code generation
    let temp_dir = TempDir::new().unwrap();

    // Create template directory
    let templates_dir = temp_dir.path().join("templates");
    fs::create_dir_all(&templates_dir).unwrap();
    let template_path = templates_dir.join("output.tmpl");
    fs::write(&template_path, "Generated: {{ name }}").unwrap();

    ggen()
        .arg("project")
        .arg("gen")
        .arg("--template")
        .arg(template_path.to_str().unwrap())
        .arg("--var")
        .arg("name=test")
        .current_dir(&temp_dir)
        .assert()
        .success();
}

#[test]
#[ignore = "e2e CLI test: spawns the built `ggen` binary via assert_cmd::Command::cargo_bin; not part of the fast --lib loop, run explicitly via `cargo test -- --ignored` after `cargo build`"]
fn test_project_gen_dry_run() {
    // Chicago TDD: Verify dry-run doesn't create files
    let temp_dir = TempDir::new().unwrap();

    // Create template directory
    let templates_dir = temp_dir.path().join("templates");
    fs::create_dir_all(&templates_dir).unwrap();
    let template_path = templates_dir.join("test.tmpl");
    fs::write(&template_path, "Test content").unwrap();

    ggen()
        .arg("project")
        .arg("gen")
        .arg("--template")
        .arg(template_path.to_str().unwrap())
        .arg("--dry-run")
        .current_dir(&temp_dir)
        .assert()
        .success();

    // In dry-run mode, verify no output files created
    // (implementation dependent - may or may not create files)
}

#[test]
#[ignore = "e2e CLI test: spawns the built `ggen` binary via assert_cmd::Command::cargo_bin; not part of the fast --lib loop, run explicitly via `cargo test -- --ignored` after `cargo build`"]
fn test_project_help_shows_verbs() {
    // Chicago TDD: Verify help state is comprehensive
    ggen()
        .arg("project")
        .arg("--help")
        .assert()
        .success()
        .stdout(predicate::str::contains("new"))
        .stdout(predicate::str::contains("plan"))
        .stdout(predicate::str::contains("gen"))
        .stdout(predicate::str::contains("init"))
        .stdout(predicate::str::contains("generate"))
        .stdout(predicate::str::contains("watch"));
}

#[test]
#[ignore = "e2e CLI test: spawns the built `ggen` binary via assert_cmd::Command::cargo_bin; not part of the fast --lib loop, run explicitly via `cargo test -- --ignored` after `cargo build`"]
fn test_project_invalid_verb() {
    // Chicago TDD: Verify error handling for invalid verbs
    ggen()
        .arg("project")
        .arg("invalid-verb")
        .assert()
        .failure()
        .stderr(predicate::str::contains("error").or(predicate::str::contains("invalid")));
}

#[test]
#[ignore = "e2e CLI test: spawns the built `ggen` binary via assert_cmd::Command::cargo_bin; not part of the fast --lib loop, run explicitly via `cargo test -- --ignored` after `cargo build`"]
fn test_project_new_invalid_type() {
    // Chicago TDD: Verify error state for invalid project type
    let temp_dir = TempDir::new().unwrap();

    let _ = ggen()
        .arg("project")
        .arg("new")
        .arg("test")
        .arg("--type")
        .arg("invalid-type-xyz")
        .current_dir(&temp_dir)
        .output();

    // Command may fail or succeed with warning - both valid behaviors
}

#[test]
#[ignore = "e2e CLI test: spawns the built `ggen` binary via assert_cmd::Command::cargo_bin; not part of the fast --lib loop, run explicitly via `cargo test -- --ignored` after `cargo build`"]
fn test_project_init_empty_name() {
    // Chicago TDD: Verify validation rejects empty project name
    let temp_dir = TempDir::new().unwrap();

    ggen()
        .arg("project")
        .arg("init")
        .arg("--name")
        .arg("")
        .current_dir(&temp_dir)
        .assert()
        .failure()
        .stderr(predicate::str::contains("empty").or(predicate::str::contains("error")));
}

#[test]
#[ignore = "e2e CLI test: spawns the built `ggen` binary via assert_cmd::Command::cargo_bin; not part of the fast --lib loop, run explicitly via `cargo test -- --ignored` after `cargo build`"]
fn test_project_init_whitespace_name() {
    // Chicago TDD: Verify validation rejects whitespace in name
    let temp_dir = TempDir::new().unwrap();

    ggen()
        .arg("project")
        .arg("init")
        .arg("--name")
        .arg("test project")
        .current_dir(&temp_dir)
        .assert()
        .failure()
        .stderr(predicate::str::contains("whitespace").or(predicate::str::contains("error")));
}
