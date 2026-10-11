// Chicago TDD (.claude/rules/rust/testing.md): unwrap/expect/panic allowed in test code.
#![allow(clippy::unwrap_used, clippy::expect_used, clippy::panic)]

use std::path::Path;
use std::process::Command;

fn fixture_dir() -> std::path::PathBuf {
    let mut d = std::env::current_dir().unwrap();
    d.push("tests/fixtures");
    d
}

fn write_fixture(name: &str, body: &str) -> std::path::PathBuf {
    let dir = fixture_dir();
    std::fs::create_dir_all(&dir).unwrap();
    let path = dir.join(name);
    std::fs::write(&path, body).unwrap();
    #[cfg(unix)]
    {
        use std::os::unix::fs::PermissionsExt;
        std::fs::set_permissions(&path, std::fs::Permissions::from_mode(0o755)).unwrap();
    }
    path
}

fn bin_path() -> std::path::PathBuf {
    let mut p = std::env::current_exe().unwrap();
    p.pop(); // wrapper hash binary -> deps/
    p.pop(); // deps -> debug/
    p.pop(); // debug -> target/
    p.push("debug");
    p.push("pm4pytest");
    p
}

#[test]
fn propagates_exit_code_from_real_fixture() {
    let fixture = write_fixture("pass_fixture.sh", "#!/bin/sh\necho fixture-ok\nexit 7\n");
    let out = Command::new(bin_path())
        .env("PM4PYTEST_BIN", &fixture)
        .args(["--flag", "value"])
        .output()
        .unwrap();
    assert!(
        String::from_utf8_lossy(&out.stdout).contains("fixture-ok"),
        "fixture stdout missing: {:?}",
        String::from_utf8_lossy(&out.stdout)
    );
    assert_eq!(out.status.code(), Some(7));
}

#[test]
fn refusal_when_binary_not_found() {
    let out = Command::new(bin_path())
        .env_remove("PM4PYTEST_BIN")
        .env("PATH", "/nonexistent-pm4pytest-probe-path")
        .output()
        .unwrap();
    assert_eq!(out.status.code(), Some(2));
    assert!(
        String::from_utf8_lossy(&out.stderr).contains("REFUSED:PM4PYTEST_BINARY_NOT_FOUND"),
        "expected typed refusal on stderr"
    );
}

#[test]
fn uses_pm4pytest_bin_when_set_and_exists() {
    let fixture = write_fixture(
        "env_fixture.sh",
        "#!/bin/sh\necho from-env-fixture\nexit 0\n",
    );
    assert!(Path::new(&fixture).exists());
    let out = Command::new(bin_path())
        .env("PM4PYTEST_BIN", &fixture)
        .output()
        .unwrap();
    assert_eq!(out.status.code(), Some(0));
    assert!(String::from_utf8_lossy(&out.stdout).contains("from-env-fixture"));
}
