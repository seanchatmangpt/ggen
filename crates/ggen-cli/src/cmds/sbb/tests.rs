use super::evaluation::{evaluate, GIT_ENV_LEAK_VARS};
use super::*;

fn run(root: &Path, args: &[&str]) -> String {
    let mut command = Command::new("git");
    command.arg("-C").arg(root).args(args);
    for var in GIT_ENV_LEAK_VARS {
        command.env_remove(var);
    }
    let output = command.output().expect("git");
    assert!(
        output.status.success(),
        "{}",
        String::from_utf8_lossy(&output.stderr)
    );
    String::from_utf8(output.stdout)
        .expect("utf8")
        .trim()
        .to_string()
}

fn fixture(root: &Path, duplicate: bool) -> PathBuf {
    run(root, &["init", "--quiet"]);
    run(root, &["config", "user.name", "ggen test"]);
    run(root, &["config", "user.email", "ggen-test@example.invalid"]);
    fs::write(root.join("evidence.txt"), b"standing evidence").expect("fixture");
    run(root, &["add", "evidence.txt"]);
    run(root, &["commit", "--quiet", "-m", "evidence"]);
    let commit = run(root, &["rev-parse", "HEAD"]);
    let evidence = Evidence {
        locator: "evidence.txt".to_string(),
        digest: format!("blake3:{}", digest_bytes(b"standing evidence")),
    };
    let chain = CHAIN
        .iter()
        .map(|stage| ((*stage).to_string(), evidence.clone()))
        .collect();
    let first = Delta {
        id: "capability-1".to_string(),
        commit,
        capability_iri: "urn:ggen:capability:one".to_string(),
        family: "projection".to_string(),
        summary: "Observed capability".to_string(),
        ontology_modules: vec!["urn:ggen:ontology:test".to_string()],
        textual_forms: vec!["rust".to_string()],
        chain,
        positive_witness: evidence.clone(),
        negative_fixture: evidence.clone(),
        adversarial_falsifier: evidence.clone(),
        verifier: evidence,
    };
    let mut deltas = vec![first.clone()];
    if duplicate {
        let mut second = first;
        second.id = "capability-2".to_string();
        second.capability_iri = "urn:ggen:capability:two".to_string();
        deltas.push(second);
    }
    let distribution = AXES
        .iter()
        .map(|axis| {
            let values = match *axis {
                "textual_forms" | "runtimes" => vec!["one".to_string(), "two".to_string()],
                _ => vec!["one".to_string()],
            };
            ((*axis).to_string(), values)
        })
        .collect();
    let manifest = Manifest {
        schema: MANIFEST_SCHEMA.to_string(),
        sbb: Sbb {
            id: "test-sbb".to_string(),
            version: "1.0.0".to_string(),
            architecture_contract: "urn:ggen:contract:test".to_string(),
            minimum_commit_equivalent_units: 1,
        },
        repository: Repository {
            root: ".".to_string(),
        },
        distribution,
        deltas,
    };
    let path = root.join("manifest.json");
    fs::write(&path, serde_json::to_vec_pretty(&manifest).expect("json")).expect("manifest");
    path
}

#[test]
fn unique_commit_counts_once() {
    let directory = tempfile::tempdir().expect("tempdir");
    let report = evaluate(&fixture(directory.path(), false)).expect("report");
    assert_eq!(report.commit_equivalent_units, 1);
    assert_eq!(report.distribution_contexts, "4");
    assert!(report.eligible_for_external_admission);
    assert_eq!(report.claim_ceiling, "PARTIAL_ALIVE");
}

#[test]
fn duplicate_commit_cannot_inflate_density() {
    let directory = tempfile::tempdir().expect("tempdir");
    let report = evaluate(&fixture(directory.path(), true)).expect("report");
    assert_eq!(report.commit_equivalent_units, 0);
    assert_eq!(report.duplicate_commit_collisions, 1);
}

#[test]
fn working_tree_drift_does_not_change_commit_evidence() {
    let directory = tempfile::tempdir().expect("tempdir");
    let path = fixture(directory.path(), false);
    fs::write(directory.path().join("evidence.txt"), b"uncommitted drift").expect("drift");
    assert_eq!(evaluate(&path).expect("report").commit_equivalent_units, 1);
}

#[test]
fn digest_mismatch_refuses_delta() {
    let directory = tempfile::tempdir().expect("tempdir");
    let path = fixture(directory.path(), false);
    let mut manifest: Value =
        serde_json::from_slice(&fs::read(&path).expect("manifest")).expect("json");
    manifest["deltas"][0]["chain"]["ontology"]["digest"] =
        Value::String(format!("blake3:{}", "0".repeat(64)));
    fs::write(&path, serde_json::to_vec_pretty(&manifest).expect("json")).expect("manifest");
    assert_eq!(evaluate(&path).expect("report").commit_equivalent_units, 0);
}

#[test]
fn receipts_replay_and_refuse_tampering() {
    let directory = tempfile::tempdir().expect("tempdir");
    let path = fixture(directory.path(), false);
    let output = directory.path().join("receipts");
    receipt(path.display().to_string(), output.display().to_string()).expect("receipt");
    let replayed =
        replay(path.display().to_string(), output.display().to_string()).expect("replay");
    assert_eq!(replayed["status"], "REPLAY_MATCH");
    let report_path = output.join("density-report.json");
    let mut stored: Value =
        serde_json::from_slice(&fs::read(&report_path).expect("report")).expect("json");
    stored["commit_equivalent_units"] = json!(999);
    fs::write(
        &report_path,
        serde_json::to_vec_pretty(&stored).expect("json"),
    )
    .expect("report");
    let replayed =
        replay(path.display().to_string(), output.display().to_string()).expect("replay");
    assert_eq!(replayed["status"], "REPLAY_DIVERGED");
}
