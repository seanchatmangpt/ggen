//! G0 CI/CD inventory boundary proof.
//!
//! These tests execute the repository verifier as a real subprocess and inspect
//! its filesystem evidence. No workflow, parser, process, or receipt boundary is
//! mocked.

use std::fs;
use std::path::{Path, PathBuf};
use std::process::{Command, Output};

fn repository_root() -> PathBuf {
    Path::new(env!("CARGO_MANIFEST_DIR"))
        .parent()
        .and_then(Path::parent)
        .expect("ggen-engine must be located under <repo>/crates/ggen-engine")
        .to_path_buf()
}

fn execute(script: &str) -> Output {
    Command::new("python3")
        .arg(repository_root().join(script))
        .current_dir(repository_root())
        .output()
        .unwrap_or_else(|error| panic!("failed to execute {script}: {error}"))
}

#[test]
fn exact_repository_inventory_manufactures_partial_alive_evidence() {
    let output = execute("scripts/ci/verify-g0-workflow-inventory.py");
    assert!(
        output.status.success(),
        "G0 inventory refused the exact repository:\nstdout={}\nstderr={}",
        String::from_utf8_lossy(&output.stdout),
        String::from_utf8_lossy(&output.stderr)
    );

    let report_path = repository_root().join("target/ci-g0/workflow-inventory.json");
    let report = fs::read_to_string(&report_path)
        .unwrap_or_else(|error| panic!("missing {}: {error}", report_path.display()));
    assert!(report.contains("\"standing\": \"PARTIAL_ALIVE\""));
    assert!(report.contains("\"observed_workflow_count\": 48"));
    assert!(report.contains("\"state\": \"UNKNOWN\""));
    assert!(!report.contains("\"standing\": \"ALIVE\""));
}

#[test]
fn omission_and_duplicate_ownership_are_refused_by_execution() {
    let output = execute("scripts/ci/test-g0-workflow-inventory.py");
    assert!(
        output.status.success(),
        "G0 refusal fixtures failed:\nstdout={}\nstderr={}",
        String::from_utf8_lossy(&output.stdout),
        String::from_utf8_lossy(&output.stderr)
    );
    let stdout = String::from_utf8_lossy(&output.stdout);
    assert!(stdout.contains("REFUSED:CI-G0-INVENTORY-001"));
    assert!(stdout.contains("REFUSED:CI-G0-OWNERSHIP-001"));
}
