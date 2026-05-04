use assert_cmd::Command;
use std::fs;
use std::path::Path;
use std::thread;
use std::time::Duration;

#[test]
fn test_watch_mode_produces_receipt() {
    // Arrange: Use a temp workspace
    let temp = assert_fs::TempDir::new().unwrap();

    // Act: Run watch mode for a brief period
    let mut cmd = Command::cargo_bin("ggen").unwrap();
    cmd.arg("sync").arg("--watch").current_dir(temp.path());

    // Spawn and wait briefly
    let mut child = cmd.spawn().unwrap();
    thread::sleep(Duration::from_secs(2));
    child.kill().unwrap();

    // Assert: Check that a receipt was generated (proof of life)
    let receipt_path = temp.path().join(".ggen/receipts/latest.json");
    assert!(
        receipt_path.exists(),
        "Receipt should exist after watch sync"
    );
}
