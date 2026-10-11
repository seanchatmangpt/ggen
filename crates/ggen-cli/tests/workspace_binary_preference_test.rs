#![allow(clippy::unwrap_used, clippy::expect_used, clippy::panic)] // Chicago TDD (.claude/rules/rust/testing.md): unwrap/expect/panic allowed in test code
//! Workspace-binary-preference guard for the test harness `ggen_bin()`
//! resolution used across `crates/ggen-cli/tests/`.
//!
//! Known hazard (2026-10-10): a stale ambient `ggen` (e.g. 26.9.28 in
//! `~/.local/bin`) poisoned harness runs via PATH fallback — flakes that
//! looked like code failures. The harness resolution must prefer the
//! workspace binary (`CARGO_BIN_EXE_ggen`, then `target/{debug,release}/ggen`)
//! and, when it must fall back to `PATH`, assert the resolved binary's
//! `--version` matches the workspace version, failing loudly and naming the
//! stale ambient path. Ambient reinstalls are user-gated; this guard is not.
//!
//! The core resolver is pure over explicit inputs (real files, real
//! subprocess — no mocks) so the fallback behavior is testable regardless of
//! the ambient environment of the machine running the suite.

use std::ffi::OsStr;
use std::path::{Path, PathBuf};
use tempfile::TempDir;

/// Panic message marker: any PATH-fallback version mismatch must name the
/// stale binary path and both versions.
const STALE_AMBIENT_MSG: &str = "stale ambient ggen";

/// Read `<binary> --version` and return the last whitespace-separated token
/// (`ggen 26.10.10` -> `26.10.10`). Real subprocess, no doubles.
fn binary_version(path: &Path) -> String {
    let out = std::process::Command::new(path)
        .arg("--version")
        .output()
        .unwrap_or_else(|e| panic!("failed to execute {} --version: {e}", path.display()));
    String::from_utf8_lossy(&out.stdout)
        .split_whitespace()
        .next_back()
        .unwrap_or_default()
        .to_string()
}

/// Harness binary resolution with the workspace-preference + PATH-version
/// guard. Order: `bin_exe` (CARGO_BIN_EXE_ggen semantics) -> workspace
/// `target/{debug,release}/ggen[.exe]` -> PATH (version-guarded).
fn resolve_ggen(
    bin_exe: Option<&Path>, target_root: Option<&Path>, path_var: Option<&OsStr>,
    expected_version: &str,
) -> PathBuf {
    if let Some(p) = bin_exe {
        let p = PathBuf::from(p);
        if p.is_file() {
            return p;
        }
    }

    if let Some(target) = target_root {
        for profile in ["debug", "release"] {
            for name in ["ggen", "ggen.exe"] {
                let candidate = target.join(profile).join(name);
                if candidate.is_file() {
                    return candidate;
                }
            }
        }
    }

    if let Some(path_var) = path_var {
        for dir in std::env::split_paths(path_var) {
            for name in ["ggen", "ggen.exe"] {
                let candidate = dir.join(name);
                if candidate.is_file() {
                    let found = binary_version(&candidate);
                    assert!(
                        found == expected_version,
                        "{STALE_AMBIENT_MSG}: harness fell back to PATH and resolved {}, \
                         which reports version {found} but the workspace version is \
                         {expected_version}. Remove or reinstall the ambient binary \
                         (e.g. `~/.local/bin/ggen`); ambient reinstall is user-gated.",
                        candidate.display()
                    );
                    return candidate;
                }
            }
        }
    }

    panic!(
        "could not resolve the `ggen` binary: CARGO_BIN_EXE_ggen unset and no \
         target/{{debug,release}}/ggen found; build it with `cargo build -p ggen-cli-lib --bin ggen`"
    );
}

/// Write an executable fake `ggen` script reporting `ggen <version>`.
fn write_fake_ggen(dir: &Path, version: &str) -> PathBuf {
    let path = dir.join("ggen");
    std::fs::write(&path, format!("#!/bin/sh\necho \"ggen {version}\"\n"))
        .expect("write fake ggen script");
    #[cfg(unix)]
    {
        use std::os::unix::fs::PermissionsExt;
        std::fs::set_permissions(&path, std::fs::Permissions::from_mode(0o755))
            .expect("chmod fake ggen");
    }
    path
}

#[test]
fn workspace_binary_wins_over_stale_path_entry() {
    let stale_dir = TempDir::new().expect("stale dir");
    let stale = write_fake_ggen(stale_dir.path(), "0.0.1");

    let ws_dir = TempDir::new().expect("workspace dir");
    let ws_bin_dir = ws_dir.path().join("target/debug");
    std::fs::create_dir_all(&ws_bin_dir).expect("mkdir target/debug");
    // Real executable file; never executed because workspace wins.
    let ws_bin = write_fake_ggen(&ws_bin_dir, "0.0.1");

    let ws_target = ws_dir.path().join("target");
    let path_var = std::env::join_paths([stale_dir.path()]).expect("join PATH");
    let resolved = resolve_ggen(
        None,
        Some(ws_target.as_path()),
        Some(path_var.as_os_str()),
        "26.10.10",
    );
    assert_eq!(
        resolved, ws_bin,
        "workspace target/debug/ggen must be preferred over any PATH entry"
    );
}

#[test]
fn stale_path_fallback_refuses_and_names_the_stale_path() {
    let stale_dir = TempDir::new().expect("stale dir");
    let stale = write_fake_ggen(stale_dir.path(), "0.0.1");

    let path_var = std::env::join_paths([stale_dir.path()]).expect("join PATH");
    let result = std::panic::catch_unwind(|| {
        resolve_ggen(
            None,
            None,
            Some(path_var.as_os_str()),
            env!("CARGO_PKG_VERSION"),
        )
    });
    let err = result.expect_err("stale PATH fallback must panic");
    let msg = err
        .downcast_ref::<String>()
        .cloned()
        .or_else(|| err.downcast_ref::<&str>().map(|s| s.to_string()))
        .unwrap_or_default();
    assert!(
        msg.contains(STALE_AMBIENT_MSG),
        "panic must name the stale-ambient hazard, got: {msg}"
    );
    assert!(
        msg.contains(stale.to_string_lossy().as_ref()),
        "panic must name the stale binary path {}, got: {msg}",
        stale.display()
    );
    assert!(
        msg.contains("0.0.1") && msg.contains(env!("CARGO_PKG_VERSION")),
        "panic must report both versions, got: {msg}"
    );
}

#[test]
fn path_fallback_accepts_version_matching_binary() {
    let ok_dir = TempDir::new().expect("ok dir");
    let ok = write_fake_ggen(ok_dir.path(), env!("CARGO_PKG_VERSION"));

    let path_var = std::env::join_paths([ok_dir.path()]).expect("join PATH");
    let resolved = resolve_ggen(
        None,
        None,
        Some(path_var.as_os_str()),
        env!("CARGO_PKG_VERSION"),
    );
    assert_eq!(resolved, ok, "version-matching PATH binary is accepted");
}

/// Live wiring: the same resolution order used by this crate's harness
/// helpers, against the real environment. When the workspace binary exists
/// (normal `cargo test` case) it must win; the guard never silently runs a
/// version-mismatched binary.
#[test]
fn live_resolution_resolves_workspace_binary_or_guarded_path() {
    let target_root =
        std::env::var_os("CARGO_MANIFEST_DIR").map(|d| PathBuf::from(d).join("../target"));
    let path_var = std::env::var_os("PATH");
    let resolved = resolve_ggen(
        std::env::var_os("CARGO_BIN_EXE_ggen")
            .map(PathBuf::from)
            .as_deref(),
        target_root.as_deref(),
        path_var.as_deref(),
        env!("CARGO_PKG_VERSION"),
    );
    assert!(
        resolved.is_file(),
        "resolved binary must exist: {resolved:?}"
    );
    let version = binary_version(&resolved);
    assert_eq!(
        version,
        env!("CARGO_PKG_VERSION"),
        "resolved binary {} must report the workspace version",
        resolved.display()
    );
}
