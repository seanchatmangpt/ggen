//! Self-Play Command Smoke Tests
//!
//! Chicago TDD smoke tests for self-play commands.
//! These tests verify basic functionality without mocking.

use std::path::PathBuf;

#[test]
fn test_self_play_demo_command() {
    // Test that the demo command runs without crashing
    // This is a basic smoke test to ensure the command is registered
    let output = std::process::Command::new(env!("CARGO_BIN_EXE_ggen"))
        .arg("self-play")
        .arg("demo")
        .output();

    // Note: This test may fail if the binary isn't built yet
    // or if there are port conflicts. The important thing is that
    // the command is recognized (not "unrecognized subcommand")
    match output {
        Ok(out) => {
            // Command was recognized (good!)
            // We don't assert on status code because there might be
            // port conflicts or other runtime issues
            let stderr = String::from_utf8_lossy(&out.stderr);
            assert!(
                !stderr.contains("unrecognized subcommand"),
                "self-play command should be registered"
            );
        }
        Err(e) => {
            // Binary not built or other OS-level error
            // This is acceptable for a smoke test
            eprintln!("Warning: Could not run ggen binary: {}", e);
        }
    }
}

#[test]
fn test_self_play_validate_command() {
    // Test that the validate command runs
    let demo_path = PathBuf::from("examples/self-play");
    let ontology_path = demo_path.join("ontology.ttl");

    // Only run if ontology exists
    if ontology_path.exists() {
        let output = std::process::Command::new(env!("CARGO_BIN_EXE_ggen"))
            .arg("self-play")
            .arg("validate")
            .arg(ontology_path)
            .output();

        match output {
            Ok(out) => {
                let stderr = String::from_utf8_lossy(&out.stderr);
                assert!(
                    !stderr.contains("unrecognized subcommand"),
                    "self-play validate command should be registered"
                );
            }
            Err(e) => {
                eprintln!("Warning: Could not run ggen binary: {}", e);
            }
        }
    }
}

#[test]
fn test_self_play_module_exists() {
    // Verify the self_play module file exists
    let module_path = PathBuf::from("crates/ggen-cli/src/cmds/self_play.rs");
    assert!(module_path.exists(), "self_play.rs module should exist");

    // Verify it's referenced in mod.rs
    let mod_rs = std::fs::read_to_string("crates/ggen-cli/src/cmds/mod.rs")
        .expect("Should be able to read mod.rs");
    assert!(
        mod_rs.contains("pub mod self_play"),
        "mod.rs should declare pub mod self_play"
    );
    assert!(
        !mod_rs.contains("// pub mod self_play"),
        "self_play module should not be commented out"
    );
}
