use ggen_pack_gall::{observe, resolve, verify, Catalog, OBSERVATION_SCHEMA, VERIFIER_SCHEMA};
use std::fs;
use std::path::Path;
use std::process::Command;
use tempfile::TempDir;

fn write(root: &Path, path: &str, content: &str) {
    let target = root.join(path);
    fs::create_dir_all(target.parent().unwrap()).unwrap();
    fs::write(target, content).unwrap();
}

fn fixture() -> TempDir {
    let dir = TempDir::new().unwrap();
    let root = dir.path();
    let contract = include_str!("../contracts/pack-gall.contract.json");
    write(
        root,
        "tools/pack-gall/contracts/pack-gall.contract.json",
        contract,
    );
    write(
        root,
        "tools/pack-gall/contracts/pack-kernel.schema.json",
        include_str!("../contracts/pack-kernel.schema.json"),
    );
    write(
        root,
        "tools/pack-gall/contracts/verifier-report.schema.json",
        include_str!("../contracts/verifier-report.schema.json"),
    );
    write(root, "AGENTS.md", "real constitution boundary");
    write(root, "justfile", "verify:\n    echo verified\n");
    write(
        root,
        "crates/ggen-cli/src/cmds/bblock.rs",
        "const A: &str = \"ggen.bblock.catalog.v1 ggen.bblock.plan.v1 ggen.bblock.receipt.v1 blake3\";\npub fn providers(){}\npub fn list(){}\npub fn inspect(){}\npub fn group(){}\npub fn plan(){}\npub fn enable(){}\npub fn validate(){}\n",
    );
    write(
        root,
        "crates/ggen-cli/src/cmds/pack.rs",
        "pub fn add(){}\npub fn remove(){}\npub fn list(){}\npub fn show(){}\npub fn search(){}\npub fn doctor(){}\n",
    );
    write(
        root,
        "crates/ggen-cli/src/cmds/packs.rs",
        "pub fn install(){}\npub fn list(){}\npub fn validate(){}\npub fn show(){}\n",
    );
    write(
        root,
        "crates/ggen-marketplace/src/packs/lockfile.rs",
        "pub struct PackLockfile; const INTEGRITY: &str = \"integrity\";\n",
    );
    write(
        root,
        "packs/fortune5-deployment-blocks-pack/ontology.ttl",
        "@prefix bb: <https://ggen.dev/bblock#> .\n",
    );
    write(
        root,
        "docs/marketplace/ATOMIC_PACKS.md",
        "Atomic packs are the canonical semantic unit\nBundles are aliases. Atomic packs are truth.\n",
    );
    write(
        root,
        "docs/architecture/BBLOCKS-PACKS-ARD-PRD-v26.7.31.md",
        "Packs are atoms.\n",
    );
    let mut groups = Vec::new();
    for index in 0..18 {
        let dependency = if index == 0 {
            vec![]
        } else {
            vec![format!("g{}", index - 1)]
        };
        groups.push(serde_json::json!({
            "id": if index == 17 { "fortune5-complete".to_string() } else { format!("g{index}") },
            "directory": format!("infrastructure/g{index}"),
            "dependencies": dependency,
            "common_packs": [format!("common-{index}")],
            "provider_packs": {
                "aws": [format!("aws-{index}")],
                "azure": [format!("azure-{index}")],
                "gcp": [format!("gcp-{index}")]
            }
        }));
    }
    let catalog = serde_json::json!({
        "schema": "ggen.bblock.catalog.v1",
        "version": "1.0.0",
        "providers": [
            {"id": "aws", "aliases": []},
            {"id": "azure", "aliases": []},
            {"id": "gcp", "aliases": ["gpc"]}
        ],
        "groups": groups
    });
    write(
        root,
        "packs/fortune5-deployment-blocks-pack/catalog/fortune5-bblocks.json",
        &serde_json::to_string_pretty(&catalog).unwrap(),
    );
    dir
}

#[test]
fn real_filesystem_observation_and_external_verification_are_deterministic() {
    let dir = fixture();
    let first = observe(dir.path()).unwrap();
    let second = observe(dir.path()).unwrap();
    assert_eq!(first.schema, OBSERVATION_SCHEMA);
    assert_eq!(first, second);
    let report = verify(dir.path(), &first).unwrap();
    assert_eq!(report.schema, VERIFIER_SCHEMA);
    assert_eq!(report.checkpoints.len(), 10);
    assert_eq!(report.standing, "PARTIAL_ALIVE");
    assert!(report.checkpoints.iter().all(|checkpoint| checkpoint.passed));
}

#[test]
fn tampered_command_surface_is_refused() {
    let dir = fixture();
    let observed = observe(dir.path()).unwrap();
    write(
        dir.path(),
        "crates/ggen-cli/src/cmds/packs.rs",
        "pub fn install(){}\n",
    );
    let report = verify(dir.path(), &observed).unwrap();
    assert_eq!(report.standing, "BUILD_BROKEN");
    assert!(report.checkpoints.iter().any(|checkpoint| !checkpoint.passed));
}

#[test]
fn resolver_is_deterministic_across_aliases_and_repeated_runs() {
    let dir = fixture();
    let bytes = fs::read(
        dir.path()
            .join("packs/fortune5-deployment-blocks-pack/catalog/fortune5-bblocks.json"),
    )
    .unwrap();
    let catalog: Catalog = serde_json::from_slice(&bytes).unwrap();
    let canonical = resolve(&catalog, "fortune5-complete", "gcp").unwrap();
    let alias = resolve(&catalog, "fortune5-complete", "gpc").unwrap();
    let repeated = resolve(&catalog, "fortune5-complete", "gcp").unwrap();
    assert_eq!(canonical, alias);
    assert_eq!(canonical, repeated);
    assert_eq!(canonical.plan_digest.len(), 64);
}

#[test]
fn observer_and_verifier_binaries_cross_process_and_filesystem_boundaries() {
    let dir = fixture();
    let evidence = dir.path().join(".ggen/pack-gall");
    let observation = evidence.join("equivalence.report.json");
    let report = evidence.join("verifier.report.json");
    let receipt = evidence.join("verification.receipt.json");

    let observe_status = Command::new(env!("CARGO_BIN_EXE_pack-gall-observe"))
        .args([
            "--root",
            dir.path().to_str().unwrap(),
            "--out",
            observation.to_str().unwrap(),
        ])
        .status()
        .unwrap();
    assert!(observe_status.success());
    assert!(observation.is_file());

    let verify_status = Command::new(env!("CARGO_BIN_EXE_pack-gall-verify"))
        .args([
            "--root",
            dir.path().to_str().unwrap(),
            "--observation",
            observation.to_str().unwrap(),
            "--report",
            report.to_str().unwrap(),
            "--receipt",
            receipt.to_str().unwrap(),
        ])
        .status()
        .unwrap();
    assert!(verify_status.success());
    assert!(report.is_file());
    assert!(receipt.is_file());
    let receipt_value: serde_json::Value =
        serde_json::from_slice(&fs::read(receipt).unwrap()).unwrap();
    assert_eq!(receipt_value["digest_algorithm"].as_str(), Some("blake3"));
    assert_eq!(receipt_value["digest"].as_str().unwrap().len(), 64);
}
