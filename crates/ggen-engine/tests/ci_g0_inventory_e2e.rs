//! G0 CI/CD inventory boundary proof.
//!
//! These tests execute the repository verifiers as real subprocesses and inspect
//! their filesystem evidence. No workflow, parser, process, or receipt boundary
//! is mocked.

#![allow(clippy::unwrap_used, clippy::expect_used, clippy::panic)]

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
    let inventory_output = execute("scripts/ci/verify-g0-workflow-inventory.py");
    assert!(
        inventory_output.status.success(),
        "G0 inventory refused the exact repository:\nstdout={}\nstderr={}",
        String::from_utf8_lossy(&inventory_output.stdout),
        String::from_utf8_lossy(&inventory_output.stderr)
    );

    let inventory_path = repository_root().join("target/ci-g0/workflow-inventory.json");
    let inventory = fs::read_to_string(&inventory_path)
        .unwrap_or_else(|error| panic!("missing {}: {error}", inventory_path.display()));
    assert!(inventory.contains("\"standing\": \"PARTIAL_ALIVE\""));
    // 48 -> 76 (2026-08-03, TECH-DEBT-003 fix): the real `.github/workflows/` count grew by
    // 28 real, separately-merged workflow files after the 2026-07-30 manifest snapshot (see
    // packs/github-actions-pack/observations/g0-workflow-inventory-v26.7.31.toml's own
    // 2026-08-03 comment for the drift evidence and the 28 newly-admitted entries). This was
    // real Contract Drift, not a test bug -- the manifest was stale, not this assertion's
    // intent, so the fix is admitting the real 76 here to match the now-current manifest.
    // 76 -> 74 (2026-08-18): further real, committed drift -- `docker.yml` and 4 of the 5
    // `foundry-{clean-room-semantic-replay,historical-lineage-clean-room}*.yml` snapshots were
    // superseded and removed from disk (their successors `docker-build-push.yml` and the
    // `-v2`/`-v5` files remain), while `agent-apply-ggen-engine-refactor.yml`,
    // `agent-source-capsule.yml`, `ggen-lsp-runtime-crown.yml`, and
    // `mmdio-semantic-crown-contract.yml` merged with no manifest entry. See the manifest's own
    // 2026-08-18 comment for the per-file evidence; net observed count is 74.
    assert!(inventory.contains("\"observed_workflow_count\": 74"));
    assert!(inventory.contains("\"state\": \"UNKNOWN\""));
    assert!(!inventory.contains("\"standing\": \"ALIVE\""));

    let topology_output = execute("scripts/ci/analyze-g0-workflow-topology.py");
    assert!(
        topology_output.status.success(),
        "G0 topology analysis refused the exact repository:\nstdout={}\nstderr={}",
        String::from_utf8_lossy(&topology_output.stdout),
        String::from_utf8_lossy(&topology_output.stderr)
    );

    let topology_path = repository_root().join("target/ci-g0/workflow-topology.json");
    let topology = fs::read_to_string(&topology_path)
        .unwrap_or_else(|error| panic!("missing {}: {error}", topology_path.display()));
    assert!(topology.contains("\"standing\": \"PARTIAL_ALIVE\""));
    assert!(topology.contains("\"workflow_count\": 74"));
    assert!(topology.contains("\"trigger_fanout\""));
    assert!(topology.contains("\"permission_ceiling\""));
    assert!(topology.contains("\"mutable_action_references\""));
    assert!(!topology.contains("\"standing\": \"ALIVE\""));
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
