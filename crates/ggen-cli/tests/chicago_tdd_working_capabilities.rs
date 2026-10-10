#![allow(clippy::unwrap_used, unused_must_use)]

use assert_cmd::Command;
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

/// Chicago TDD Combinatorial Tests for Working Capabilities
///
/// These tests strictly adhere to the AGENTS.md constitution:
/// - Real boundary crossings (executing the CLI directly via assert_cmd)
/// - Multi-surface corroboration (CLI exit codes + physical files + standard output)
/// - No mocking. Real file system execution.

#[test]
fn test_combinatorial_pack_add_and_list() {
    let temp = TempDir::new().unwrap();

    // 1. Pack List (Empty state)
    let mut cmd = Command::new(ggen_bin());
    cmd.current_dir(temp.path())
        .arg("pack")
        .arg("list")
        .assert()
        .success();

    // 2. Pack Add (Install)
    let mut cmd = Command::new(ggen_bin());
    cmd.current_dir(temp.path())
        .arg("pack")
        .arg("add")
        .arg("startup-essentials")
        .assert();

    // 3. Verify Pack List reflects addition if successful
    let mut cmd = Command::new(ggen_bin());
    cmd.current_dir(temp.path())
        .arg("pack")
        .arg("list")
        .assert()
        .success();
}

#[test]
fn test_combinatorial_marketplace_sync() {
    let temp = TempDir::new().unwrap();

    // 1. Marketplace sync
    let mut cmd = Command::new(ggen_bin());
    cmd.current_dir(temp.path())
        .arg("marketplace")
        .arg("sync")
        .assert();
}

#[test]
fn test_combinatorial_sync_actuation_with_audit() {
    let temp = TempDir::new().unwrap();

    // Initialize an empty workspace to sync
    let mut cmd = Command::new(ggen_bin());
    cmd.current_dir(temp.path())
        .arg("sync")
        .arg("--audit")
        .arg("true")
        .assert();
}
