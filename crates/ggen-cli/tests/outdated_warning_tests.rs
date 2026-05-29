use std::fs;
use std::process::Command;
use std::time::{Duration, SystemTime};
use tempfile::TempDir;

#[test]
fn test_outdated_binary_warning_triggers() {
    // 1. Find the compiled target binary
    let current_bin = assert_cmd::cargo::cargo_bin("ggen");
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

    // Touch the target binary to ensure it has a newer mtime
    let target_file = fs::OpenOptions::new()
        .write(true)
        .open(&current_bin)
        .expect("Failed to open target binary");
    target_file
        .set_modified(SystemTime::now())
        .expect("Failed to set target modified time");

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
    let current_bin = assert_cmd::cargo::cargo_bin("ggen");
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
    let current_bin = assert_cmd::cargo::cargo_bin("ggen");
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
