//! Chicago-TDD closure for the pack and field gaps surfaced by the book audit.
//!
//! These tests use real temporary filesystems, the real `ggen` binary, real
//! receipts, and real generated Rust crates. No pack loader, renderer, receipt,
//! or process collaborator is mocked.

#![allow(clippy::expect_used)]

use std::path::{Path, PathBuf};
use std::process::Command;

use chicago_tdd_tools::cli_proof::CliHarness;
use tempfile::TempDir;

/// Packs that were declared PARTIAL solely because the capability ledger did
/// not bind them to an executable composition consumer.
const GAP_PACKS: [&str; 6] = [
    "repo-as-found-pack",
    "repo-load-path-pack",
    "repo-intervention-pack",
    "repo-reconciliation-pack",
    "temporary-works-pack",
    "mfw-pcp-level5-pack",
];

fn packs_dir() -> PathBuf {
    Path::new(env!("CARGO_MANIFEST_DIR")).join("../../packs")
}

fn copy_tree(src: &Path, dst: &Path) {
    std::fs::create_dir_all(dst).expect("mkdir destination");
    for entry in std::fs::read_dir(src).expect("read source directory") {
        let entry = entry.expect("directory entry");
        let from = entry.path();
        let to = dst.join(entry.file_name());
        if from.is_dir() {
            copy_tree(&from, &to);
        } else {
            std::fs::copy(&from, &to).expect("copy file");
        }
    }
}

fn tree_digest(root: &Path) -> Vec<(String, String)> {
    fn walk(root: &Path, dir: &Path, out: &mut Vec<(String, String)>) {
        for entry in std::fs::read_dir(dir).expect("read directory") {
            let path = entry.expect("directory entry").path();
            let relative = path
                .strip_prefix(root)
                .expect("relative path")
                .to_string_lossy()
                .into_owned();
            if relative == ".ggen-v2" || relative.starts_with(".ggen-v2/") {
                continue;
            }
            if path.is_dir() {
                walk(root, &path, out);
            } else {
                out.push((
                    relative,
                    blake3::hash(&std::fs::read(&path).expect("read file"))
                        .to_hex()
                        .to_string(),
                ));
            }
        }
    }

    let mut result = Vec::new();
    walk(root, root, &mut result);
    result.sort();
    result
}

fn assert_cli_success(project: &Path, args: &[&str]) {
    let output = CliHarness::cargo_bin("ggen")
        .args(args)
        .current_dir(project)
        .run()
        .expect("run ggen");
    output.assert_success();
}

fn scaffold_gap_pack_consumer() -> (TempDir, PathBuf) {
    let directory = TempDir::new().expect("temporary directory");
    for pack in GAP_PACKS {
        copy_tree(&packs_dir().join(pack), &directory.path().join(pack));
    }

    let project = directory.path().join("consumer");
    std::fs::create_dir_all(project.join("templates")).expect("templates directory");
    std::fs::write(project.join("ontology.ttl"), "").expect("consumer ontology");

    let packs_table: String = GAP_PACKS
        .iter()
        .map(|pack| format!("{pack} = {{ path = \"../{pack}\" }}\n"))
        .collect();
    std::fs::write(
        project.join("ggen.toml"),
        format!(
            "[project]\nname = \"book-gap-closure\"\n\n\
             [ontology]\nsource = \"ontology.ttl\"\n\n\
             [packs]\n{packs_table}\n\
             [templates]\ndir = \"templates\"\n"
        ),
    )
    .expect("consumer manifest");

    (directory, project)
}

#[test]
fn six_gap_packs_resolve_compose_receipt_and_replay_idempotently() {
    let (_directory, project) = scaffold_gap_pack_consumer();

    assert_cli_success(&project, &["sync", "run"]);
    assert_cli_success(&project, &["receipt", "verify"]);

    let lock = std::fs::read_to_string(project.join("ggen.lock")).expect("ggen.lock");
    for pack in GAP_PACKS {
        assert!(
            lock.contains(&format!("[packs.{pack}]")),
            "lock does not admit {pack}"
        );
        assert!(
            lock.contains("content_hash = \"blake3:"),
            "lock must bind pack content hashes"
        );
    }

    let first = tree_digest(&project);
    assert_cli_success(&project, &["sync", "run"]);
    assert_cli_success(&project, &["receipt", "verify"]);
    let second = tree_digest(&project);
    assert_eq!(first, second, "second sync must be byte-identical");
}

fn run_cargo_test(project: &Path) {
    let output = Command::new("cargo")
        .args(["test", "--quiet"])
        .current_dir(project)
        .output()
        .expect("run generated cargo test");
    assert!(
        output.status.success(),
        "generated crate tests failed\nstdout:\n{}\nstderr:\n{}",
        String::from_utf8_lossy(&output.stdout),
        String::from_utf8_lossy(&output.stderr)
    );
}

#[test]
fn mfw_pcp_certificate_inventory_evolves_from_ontology_without_hand_repair() {
    let directory = TempDir::new().expect("temporary directory");
    let project = directory.path().join("mfw-pcp-level5-pack");
    copy_tree(&packs_dir().join("mfw-pcp-level5-pack"), &project);

    assert_cli_success(&project, &["sync", "run"]);
    assert_cli_success(&project, &["receipt", "verify"]);
    let generated = project.join("consumer/mfw-pcp-generated");
    run_cargo_test(&generated);

    let ontology_path = project.join("ontology.ttl");
    let mut ontology = std::fs::read_to_string(&ontology_path).expect("ontology");
    ontology.push_str(
        "\npcp:ck11 a pcp:CertificateKind ; pcp:order 11 ; \
         pcp:rustName \"Evolution\" ; pcp:label \"Evolution certificate\" .\n",
    );
    std::fs::write(&ontology_path, ontology).expect("mutated ontology");

    assert_cli_success(&project, &["sync", "run"]);
    assert_cli_success(&project, &["receipt", "verify"]);
    run_cargo_test(&generated);

    let certificates =
        std::fs::read_to_string(generated.join("src/certificates.rs")).expect("certificates");
    assert!(certificates.contains("Evolution"));
    assert!(certificates.contains("EXPECTED_COUNT: usize = 11"));

    let proof =
        std::fs::read_to_string(generated.join("tests/generated_proof.rs")).expect("proof");
    assert!(proof.contains("certificates::EXPECTED_COUNT"));
    assert!(!proof.contains("ALL.len(), 10"));

    let first = tree_digest(&generated);
    assert_cli_success(&project, &["sync", "run"]);
    assert_cli_success(&project, &["receipt", "verify"]);
    let second = tree_digest(&generated);
    assert_eq!(first, second, "evolved consumer must reach a fixed point");
}
