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
//! End-to-end integration tests for AI commands
//!
//! **Chicago TDD Principles**:
//! - REAL CLI process execution
//! - REAL API calls (when available)
//! - REAL state verification
//! - NO mocking of AI responses
//!
//! **Critical User Workflows (80/20)**:
//! 1. Generate code with AI assistance
//! 2. Interactive AI chat session
//! 3. Analyze code and provide insights
//!
//! NOTE: AI commands are currently stubbed pending domain refactoring.
//! These tests verify CLI interface works correctly.

use assert_cmd::Command;
use predicates::prelude::*;
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
fn test_ai_generate_executes() {
    // Chicago TDD: Verify AI generate command runs (stubbed)
    let temp_dir = TempDir::new().unwrap();

    ggen()
        .arg("ai")
        .arg("generate")
        .arg("write a hello world function")
        .current_dir(&temp_dir)
        .assert()
        .success();

    // Stub returns success: false, but command should execute without error
}

#[test]
#[ignore = "e2e CLI test: spawns the built `ggen` binary via assert_cmd::Command::cargo_bin; not part of the fast --lib loop, run explicitly via `cargo test -- --ignored` after `cargo build`"]
fn test_ai_generate_with_language() {
    // Chicago TDD: Verify language parameter accepted
    let temp_dir = TempDir::new().unwrap();

    ggen()
        .arg("ai")
        .arg("generate")
        .arg("write a function")
        .arg("--language")
        .arg("rust")
        .current_dir(&temp_dir)
        .assert()
        .success();
}

#[test]
#[ignore = "e2e CLI test: spawns the built `ggen` binary via assert_cmd::Command::cargo_bin; not part of the fast --lib loop, run explicitly via `cargo test -- --ignored` after `cargo build`"]
fn test_ai_generate_with_model() {
    // Chicago TDD: Verify model parameter accepted
    let temp_dir = TempDir::new().unwrap();

    ggen()
        .arg("ai")
        .arg("generate")
        .arg("test prompt")
        .arg("--model")
        .arg("claude-3")
        .current_dir(&temp_dir)
        .assert()
        .success();
}

#[test]
#[ignore = "e2e CLI test: spawns the built `ggen` binary via assert_cmd::Command::cargo_bin; not part of the fast --lib loop, run explicitly via `cargo test -- --ignored` after `cargo build`"]
fn test_ai_chat_executes() {
    // Chicago TDD: Verify AI chat command runs (stubbed)
    let temp_dir = TempDir::new().unwrap();

    ggen()
        .arg("ai")
        .arg("chat")
        .arg("--message")
        .arg("hello")
        .current_dir(&temp_dir)
        .assert()
        .success();
}

#[test]
#[ignore = "e2e CLI test: spawns the built `ggen` binary via assert_cmd::Command::cargo_bin; not part of the fast --lib loop, run explicitly via `cargo test -- --ignored` after `cargo build`"]
fn test_ai_chat_interactive() {
    // Chicago TDD: Verify interactive flag accepted
    let temp_dir = TempDir::new().unwrap();

    ggen()
        .arg("ai")
        .arg("chat")
        .arg("--interactive")
        .current_dir(&temp_dir)
        .assert()
        .success();
}

#[test]
#[ignore = "e2e CLI test: spawns the built `ggen` binary via assert_cmd::Command::cargo_bin; not part of the fast --lib loop, run explicitly via `cargo test -- --ignored` after `cargo build`"]
fn test_ai_analyze_executes() {
    // Chicago TDD: Verify AI analyze command runs (stubbed)
    let temp_dir = TempDir::new().unwrap();

    // Create a test file to analyze
    let test_file = temp_dir.path().join("test.rs");
    std::fs::write(&test_file, "fn main() { println!(\"hello\"); }").unwrap();

    ggen()
        .arg("ai")
        .arg("analyze")
        .arg("--file")
        .arg(test_file.to_str().unwrap())
        .current_dir(&temp_dir)
        .assert()
        .success();
}

#[test]
#[ignore = "e2e CLI test: spawns the built `ggen` binary via assert_cmd::Command::cargo_bin; not part of the fast --lib loop, run explicitly via `cargo test -- --ignored` after `cargo build`"]
fn test_ai_analyze_with_code() {
    // Chicago TDD: Verify code parameter accepted
    let temp_dir = TempDir::new().unwrap();

    ggen()
        .arg("ai")
        .arg("analyze")
        .arg("--code")
        .arg("fn test() {}")
        .current_dir(&temp_dir)
        .assert()
        .success();
}

#[test]
#[ignore = "e2e CLI test: spawns the built `ggen` binary via assert_cmd::Command::cargo_bin; not part of the fast --lib loop, run explicitly via `cargo test -- --ignored` after `cargo build`"]
fn test_ai_help_shows_verbs() {
    // Chicago TDD: Verify help state is comprehensive
    ggen()
        .arg("ai")
        .arg("--help")
        .assert()
        .success()
        .stdout(predicate::str::contains("generate"))
        .stdout(predicate::str::contains("chat"))
        .stdout(predicate::str::contains("analyze"));
}

#[test]
#[ignore = "e2e CLI test: spawns the built `ggen` binary via assert_cmd::Command::cargo_bin; not part of the fast --lib loop, run explicitly via `cargo test -- --ignored` after `cargo build`"]
fn test_ai_generate_help() {
    // Chicago TDD: Verify verb-specific help
    ggen()
        .arg("ai")
        .arg("generate")
        .arg("--help")
        .assert()
        .success()
        .stdout(predicate::str::contains("prompt").or(predicate::str::contains("generate")));
}

#[test]
#[ignore = "e2e CLI test: spawns the built `ggen` binary via assert_cmd::Command::cargo_bin; not part of the fast --lib loop, run explicitly via `cargo test -- --ignored` after `cargo build`"]
fn test_ai_invalid_verb() {
    // Chicago TDD: Verify error handling for invalid verbs
    ggen()
        .arg("ai")
        .arg("invalid-verb")
        .assert()
        .failure()
        .stderr(predicate::str::contains("error").or(predicate::str::contains("invalid")));
}
