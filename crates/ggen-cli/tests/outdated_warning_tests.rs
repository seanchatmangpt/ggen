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

use std::fs;
use std::process::Command;
use std::time::{Duration, SystemTime};
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

#[test]
fn test_outdated_binary_warning_triggers() {
    // 1. Find the compiled target binary
    let current_bin = ggen_bin();
    assert!(
        current_bin.exists(),
        "Target binary must exist. Run cargo build first."
    );

    // 2. Set up a temporary directory
    let temp_dir = TempDir::new().expect("Failed to create temp dir");
    let cloned_bin_path = temp_dir
        .path()
        .join(if cfg!(windows) { "ggen.exe" } else { "ggen" });

    // Copy target binary to the temp directory
    fs::copy(&current_bin, &cloned_bin_path).expect("Failed to copy binary");

    // 3. Set modified time of the copy to 1 hour ago via file.set_modified()
    let file = fs::OpenOptions::new()
        .write(true)
        .open(&cloned_bin_path)
        .expect("Failed to open cloned binary");

    let past_time = SystemTime::now() - Duration::from_secs(3600);
    file.set_modified(past_time)
        .expect("Failed to set modified time");
    // A file open for write cannot be exec'd (ETXTBSY on Linux) -- must close
    // before Command::new(&cloned_bin_path) runs it below.
    drop(file);

    // Touch the target binary to ensure it has a newer mtime
    let target_file = fs::OpenOptions::new()
        .write(true)
        .open(&current_bin)
        .expect("Failed to open target binary");
    target_file
        .set_modified(SystemTime::now())
        .expect("Failed to set target modified time");
    // current_bin is the shared cargo-built `ggen` binary other tests in this
    // process may exec concurrently -- close the write handle immediately,
    // same ETXTBSY reasoning as above, not just for cloned_bin_path.
    drop(target_file);

    // 4. Run the outdated binary in the temp directory and check stderr
    let output = Command::new(&cloned_bin_path)
        .arg("--version")
        .env("GGEN_TEST_FORCE_TERMINAL", "1")
        .env_remove("GGEN_SKIP_OUTDATED_WARNING")
        .env_remove("CI")
        .env_remove("GITHUB_ACTIONS")
        .env_remove("TRAVIS")
        .env_remove("CIRCLECI")
        .env_remove("GITLAB_CI")
        .env_remove("JENKINS_URL")
        .output()
        .expect("Failed to run outdated binary");

    let stderr = String::from_utf8_lossy(&output.stderr);

    // Assert the warning is printed
    assert!(
        stderr.contains("warning: running an outdated 'ggen' binary"),
        "Warning was not found in stderr: {}",
        stderr
    );
    assert!(
        stderr.contains("current:"),
        "Current path was not found in stderr: {}",
        stderr
    );
    assert!(
        stderr.contains("latest:"),
        "Latest path was not found in stderr: {}",
        stderr
    );
    assert!(
        stderr.contains("info: compile the latest changes or update your installation with 'cargo install --path"),
        "Info suggestion was not found in stderr: {}", stderr
    );
}

#[test]
fn test_outdated_binary_warning_skips_when_configured() {
    // 1. Find the compiled target binary
    let current_bin = ggen_bin();
    assert!(
        current_bin.exists(),
        "Target binary must exist. Run cargo build first."
    );

    // 2. Set up a temporary directory
    let temp_dir = TempDir::new().expect("Failed to create temp dir");
    let cloned_bin_path = temp_dir
        .path()
        .join(if cfg!(windows) { "ggen.exe" } else { "ggen" });

    // Copy target binary to the temp directory
    fs::copy(&current_bin, &cloned_bin_path).expect("Failed to copy binary");

    // 3. Set modified time of the copy to 1 hour ago
    let file = fs::OpenOptions::new()
        .write(true)
        .open(&cloned_bin_path)
        .expect("Failed to open cloned binary");

    let past_time = SystemTime::now() - Duration::from_secs(3600);
    file.set_modified(past_time)
        .expect("Failed to set modified time");
    // A file open for write cannot be exec'd (ETXTBSY on Linux) -- must close
    // before Command::new(&cloned_bin_path) runs it below.
    drop(file);

    // 4. Run with bypass env var GGEN_SKIP_OUTDATED_WARNING=1
    let output = Command::new(&cloned_bin_path)
        .arg("--version")
        .env("GGEN_TEST_FORCE_TERMINAL", "1")
        .env("GGEN_SKIP_OUTDATED_WARNING", "1")
        .output()
        .expect("Failed to run bypassed binary");

    let stderr = String::from_utf8_lossy(&output.stderr);
    assert!(
        !stderr.contains("warning: running an outdated 'ggen' binary"),
        "Warning should not be displayed when bypassed: {}",
        stderr
    );
}

#[test]
fn test_up_to_date_binary_displays_no_warning() {
    // 1. Find the compiled target binary
    let current_bin = ggen_bin();
    assert!(
        current_bin.exists(),
        "Target binary must exist. Run cargo build first."
    );

    // 2. Run original target bin directly
    let output = Command::new(&current_bin)
        .arg("--version")
        .env("GGEN_TEST_FORCE_TERMINAL", "1")
        .env_remove("GGEN_SKIP_OUTDATED_WARNING")
        .env_remove("CI")
        .env_remove("GITHUB_ACTIONS")
        .env_remove("TRAVIS")
        .env_remove("CIRCLECI")
        .env_remove("GITLAB_CI")
        .env_remove("JENKINS_URL")
        .output()
        .expect("Failed to run original binary");

    let stderr = String::from_utf8_lossy(&output.stderr);
    assert!(
        !stderr.contains("warning: running an outdated 'ggen' binary"),
        "Warning should not be displayed for up-to-date binary: {}",
        stderr
    );
}
