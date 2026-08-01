use ggen_architecture_foundry::{
    admit_workstream, digest_file, extract_components, initialize_corpus, load_program,
    replay_all_receipts, snapshot_repository, validate_program, ComponentMigration, EvidenceFile,
    MigrationManifest, WorkstreamReport, MIGRATION_SCHEMA, WORKSTREAM_REPORT_SCHEMA,
};
use std::fs;
use std::path::{Path, PathBuf};
use std::process::Command;
use tempfile::TempDir;

fn source_root() -> PathBuf {
    PathBuf::from(env!("CARGO_MANIFEST_DIR"))
        .join("../..")
        .canonicalize()
        .expect("source root")
}

fn program_path() -> PathBuf {
    source_root().join("docs/architecture-foundry/work-program.yaml")
}

fn run(repo: &Path, args: &[&str]) {
    let output = Command::new("git")
        .arg("-C")
        .arg(repo)
        .args(args)
        .output()
        .expect("git process");
    assert!(
        output.status.success(),
        "git {} failed: {}",
        args.join(" "),
        String::from_utf8_lossy(&output.stderr)
    );
}

fn init_repo() -> TempDir {
    let temp = tempfile::tempdir().expect("tempdir");
    run(temp.path(), &["init", "-b", "main"]);
    run(temp.path(), &["config", "user.name", "Foundry Test"]);
    run(
        temp.path(),
        &["config", "user.email", "foundry-test@example.invalid"],
    );
    fs::write(temp.path().join("README.md"), "# corpus\n").expect("README");
    run(temp.path(), &["add", "README.md"]);
    run(temp.path(), &["commit", "-m", "baseline"]);
    temp
}

fn commit_all(repo: &Path, message: &str) {
    run(repo, &["add", "-A"]);
    run(repo, &["commit", "-m", message]);
}

#[test]
fn validates_the_repository_work_program() {
    let program = load_program(&program_path()).expect("program");
    let report = validate_program(&program).expect("valid program");
    assert!(report.valid);
    assert_eq!(report.workstream_order.len(), 11);
    assert_eq!(
        report.workstream_order.first().map(String::as_str),
        Some("A")
    );
    assert_eq!(
        report.workstream_order.last().map(String::as_str),
        Some("K")
    );
}

#[test]
fn initializes_a_real_git_corpus_and_replays_its_receipt() {
    let source = source_root();
    let corpus = init_repo();
    let program = load_program(&program_path()).expect("program");

    let source_snapshot = snapshot_repository(&source).expect("source snapshot");
    assert!(source_snapshot.clean, "source checkout must be clean");

    let report = initialize_corpus(&program, &source, corpus.path()).expect("initialize");
    assert_eq!(report.generated_file_count, 10);
    assert!(corpus.path().join(&report.manifest_path).is_file());
    assert!(corpus.path().join(&report.receipt_path).is_file());
    assert_eq!(
        replay_all_receipts(&source, corpus.path()).expect("replay"),
        1
    );

    commit_all(corpus.path(), "initialize foundry");
    assert!(snapshot_repository(corpus.path()).expect("snapshot").clean);
    assert_eq!(
        replay_all_receipts(&source, corpus.path()).expect("replay"),
        1
    );
}

#[test]
fn extracts_a_real_file_with_cross_repository_lineage() {
    let source = source_root();
    let corpus = init_repo();
    let program = load_program(&program_path()).expect("program");
    initialize_corpus(&program, &source, corpus.path()).expect("initialize");
    commit_all(corpus.path(), "initialize foundry");

    let source_snapshot = snapshot_repository(&source).expect("source snapshot");
    let corpus_snapshot = snapshot_repository(corpus.path()).expect("corpus snapshot");
    let migration = MigrationManifest {
        schema_version: MIGRATION_SCHEMA.to_string(),
        batch_id: "architecture-foundry-doctrine".to_string(),
        source_head: source_snapshot.head,
        corpus_parent_head: corpus_snapshot.head,
        components: vec![ComponentMigration {
            id: "architecture-foundry-readme".to_string(),
            source_path: "docs/architecture-foundry/README.md".to_string(),
            destination_path:
                "foundry/corpus/repositories/ggen/docs/architecture-foundry/README.md".to_string(),
            disposition: "PRESERVED".to_string(),
            capability_ids: vec!["enterprise-architecture-foundry-doctrine".to_string()],
            replacement_owner: "ggen-legacy/foundry/corpus".to_string(),
            rationale: "Preserve the admitted source doctrine as a provenance-bound corpus witness"
                .to_string(),
        }],
    };

    let report = extract_components(&program, &source, corpus.path(), &migration)
        .expect("extract component");
    assert_eq!(report.component_count, 1);
    assert!(corpus.path().join(&report.receipt_path).is_file());
    assert!(corpus
        .path()
        .join("foundry/lineage/architecture-foundry-doctrine/architecture-foundry-readme.json")
        .is_file());
    assert_eq!(
        replay_all_receipts(&source, corpus.path()).expect("replay"),
        2
    );
}

#[test]
fn admits_workstream_a_only_with_exact_head_evidence() {
    let source = source_root();
    let corpus = init_repo();
    let program = load_program(&program_path()).expect("program");
    initialize_corpus(&program, &source, corpus.path()).expect("initialize");
    commit_all(corpus.path(), "initialize foundry");

    fs::create_dir_all(corpus.path().join("evidence")).expect("evidence dir");
    fs::write(
        corpus.path().join("evidence/exact-head-report.json"),
        "{\"external_head_agreement\":true}\n",
    )
    .expect("evidence");
    commit_all(corpus.path(), "add exact head evidence");

    let source_snapshot = snapshot_repository(&source).expect("source snapshot");
    let corpus_snapshot = snapshot_repository(corpus.path()).expect("corpus snapshot");
    let evidence_path = corpus.path().join("evidence/exact-head-report.json");
    let workstream = program
        .workstreams
        .iter()
        .find(|workstream| workstream.id == "A")
        .expect("workstream A");
    let report = WorkstreamReport {
        schema_version: WORKSTREAM_REPORT_SCHEMA.to_string(),
        workstream_id: "A".to_string(),
        source_head: source_snapshot.head,
        corpus_head: corpus_snapshot.head,
        verifier: "tests/real_git.rs::admits_workstream_a_only_with_exact_head_evidence"
            .to_string(),
        outputs: vec![EvidenceFile {
            repository: "corpus".to_string(),
            path: "evidence/exact-head-report.json".to_string(),
            blake3: digest_file(&evidence_path).expect("digest"),
        }],
        predicates: workstream.predicates.clone(),
    };

    let admission =
        admit_workstream(&program, &source, corpus.path(), &report).expect("workstream admission");
    assert_eq!(admission.status, "ADMITTED");
    assert!(corpus.path().join(&admission.receipt_path).is_file());
}
