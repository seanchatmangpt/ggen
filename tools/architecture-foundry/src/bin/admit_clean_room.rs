use anyhow::{bail, Context, Result};
use blake3::Hasher;
use clap::Parser;
use ggen_architecture_foundry::{
    load_program, replay_all_receipts, snapshot_repository, validate_program, verify_corpus,
    Receipt, VerificationReport, WorkstreamStateFile, RECEIPT_SCHEMA,
};
use serde::Serialize;
use serde_json::{json, Value as JsonValue};
use serde_yaml::Value as YamlValue;
use std::collections::BTreeMap;
use std::fs;
use std::path::{Path, PathBuf};
use std::process::Command;
use std::time::{SystemTime, UNIX_EPOCH};
use walkdir::WalkDir;

const CLEAN_ROOM_SCHEMA: &str = "ggen.enterprise-architecture-foundry.clean-room/1";
const VERIFIER_ID: &str = "ggen-foundry-clean-room-verifier/v1";

#[derive(Debug, Parser)]
#[command(
    name = "ggen-foundry-admit-clean-room",
    version,
    about = "Clone, build, verify, and replay the foundry twice from committed exact heads"
)]
struct Cli {
    #[arg(long)]
    program: PathBuf,
    #[arg(long)]
    source: PathBuf,
    #[arg(long)]
    corpus: PathBuf,
}

#[derive(Debug, Serialize)]
struct CleanRoomRun {
    run: u8,
    source_head: String,
    corpus_head: String,
    runtime_tests_passed: bool,
    receipts_replayed: usize,
    verification_valid: bool,
    verification_digest: String,
    foundry_tree_digest: String,
    clean_after_run: bool,
}

#[derive(Debug, Serialize)]
struct CleanRoomAdmissionReport {
    schema_version: String,
    workstream_id: String,
    verifier: String,
    source_head: String,
    corpus_head: String,
    runs: Vec<CleanRoomRun>,
    clean_room_build_success: bool,
    clean_room_verification_success: bool,
    replay_differences: usize,
    generated_drift: usize,
    second_run_semantic_result: String,
    predicates: BTreeMap<String, YamlValue>,
    metrics: BTreeMap<String, JsonValue>,
}

fn main() -> Result<()> {
    let cli = Cli::parse();
    let program = load_program(&cli.program)?;
    let validation = validate_program(&program)?;
    let source = snapshot_repository(&cli.source)?;
    let corpus = snapshot_repository(&cli.corpus)?;
    require_clean(&source, "SOURCE_WORKTREE_DIRTY")?;
    require_clean(&corpus, "CORPUS_WORKTREE_DIRTY")?;

    let workstream = program
        .workstreams
        .iter()
        .find(|candidate| candidate.id == "J")
        .context("WORKSTREAM_J_MISSING")?;
    if workstream.dependencies.len() != 1 || workstream.dependencies[0] != "I" {
        bail!("WORKSTREAM_J_DEPENDENCY_INVALID");
    }
    let foundry_root = cli.corpus.join("foundry");
    let state_path = foundry_root.join("workstreams/state.json");
    let mut state: WorkstreamStateFile = read_json(&state_path, "WORKSTREAM_STATE_INVALID")?;
    require_admitted(&state, "I")?;
    require_ready(&state, "J")?;
    replay_all_receipts(&cli.source, &cli.corpus)?;

    let run_root = std::env::temp_dir().join(format!(
        "ggen-foundry-clean-room-{}-{}",
        std::process::id(),
        SystemTime::now()
            .duration_since(UNIX_EPOCH)
            .context("system clock before epoch")?
            .as_nanos()
    ));
    fs::create_dir_all(&run_root).context("create clean-room root")?;

    let first = execute_clean_run(
        1,
        &run_root.join("run-1"),
        &cli,
        &program,
        &source.head,
        &corpus.head,
    )?;
    let second = execute_clean_run(
        2,
        &run_root.join("run-2"),
        &cli,
        &program,
        &source.head,
        &corpus.head,
    )?;
    let clean_room_build_success = first.runtime_tests_passed && second.runtime_tests_passed;
    let clean_room_verification_success = first.verification_valid && second.verification_valid;
    let replay_differences = usize::from(first.verification_digest != second.verification_digest);
    let generated_drift = usize::from(first.foundry_tree_digest != second.foundry_tree_digest);
    let second_run_semantic_result = if replay_differences == 0 && generated_drift == 0 {
        "NO_SEMANTIC_CHANGE"
    } else {
        "SEMANTIC_DRIFT"
    };
    if !clean_room_build_success
        || !clean_room_verification_success
        || replay_differences != 0
        || generated_drift != 0
    {
        bail!(
        "CLEAN_ROOM_REFUSED: build={clean_room_build_success}, verification={clean_room_verification_success}, replay={replay_differences}, drift={generated_drift}"
    );
    }

    let report = CleanRoomAdmissionReport {
        schema_version: CLEAN_ROOM_SCHEMA.to_string(),
        workstream_id: "J".to_string(),
        verifier: VERIFIER_ID.to_string(),
        source_head: source.head.clone(),
        corpus_head: corpus.head.clone(),
        runs: vec![first, second],
        clean_room_build_success,
        clean_room_verification_success,
        replay_differences,
        generated_drift,
        second_run_semantic_result: second_run_semantic_result.to_string(),
        predicates: workstream.predicates.clone(),
        metrics: BTreeMap::from([
            ("clean_clone_count".to_string(), json!(2)),
            ("runtime_test_runs".to_string(), json!(2)),
        ]),
    };
    let report_bytes = canonical_json(&report)?;
    let report_digest = digest_bytes(&report_bytes);
    let report_relative = "foundry/workstreams/J/admission-report.json";
    write_new(&cli.corpus.join(report_relative), &report_bytes)?;

    let receipt_relative = "foundry/receipts/workstream-J.json";
    {
        let current = state
            .workstreams
            .get_mut("J")
            .context("WORKSTREAM_J_STATE_MISSING")?;
        current.status = "ADMITTED".to_string();
        current.report_digest = Some(report_digest.clone());
        current.receipt_path = Some(receipt_relative.to_string());
    }
    if let Some(next) = state.workstreams.get_mut("K") {
        next.status = "READY".to_string();
    }
    let state_bytes = canonical_json(&state)?;
    let mut outputs = BTreeMap::new();
    outputs.insert(format!("corpus:{report_relative}"), report_digest);
    outputs.insert(
        "projection:foundry/workstreams/state.json".to_string(),
        digest_bytes(&state_bytes),
    );
    let mut inputs = BTreeMap::new();
    inputs.insert("work-program".to_string(), validation.program_digest);
    inputs.insert("source-tree".to_string(), source.tracked_tree_digest);
    inputs.insert("corpus-tree".to_string(), corpus.tracked_tree_digest);
    let subject_digest = digest_named_outputs(&outputs);
    let receipt = Receipt {
        schema_version: RECEIPT_SCHEMA.to_string(),
        receipt_type: "WORKSTREAM_ADMISSION".to_string(),
        subject: "J".to_string(),
        subject_digest: subject_digest.clone(),
        source_head: source.head,
        corpus_head: corpus.head,
        input_digests: inputs,
        output_digests: outputs,
        run_id: subject_digest.chars().take(20).collect(),
    };
    write_new(
        &cli.corpus.join(receipt_relative),
        &canonical_json(&receipt)?,
    )?;
    write_replace(&state_path, &state_bytes)?;
    let _ = fs::remove_dir_all(&run_root);
    println!("{}", serde_json::to_string_pretty(&report)?);
    Ok(())
}

fn execute_clean_run(
    run: u8, root: &Path, cli: &Cli, program: &ggen_architecture_foundry::WorkProgram,
    expected_source_head: &str, expected_corpus_head: &str,
) -> Result<CleanRoomRun> {
    fs::create_dir_all(root).with_context(|| format!("create {}", root.display()))?;
    let source_clone = root.join("ggen");
    let corpus_clone = root.join("ggen-legacy");
    git_clone(&cli.source, &source_clone)?;
    git_clone(&cli.corpus, &corpus_clone)?;
    git_checkout(&source_clone, expected_source_head)?;
    git_checkout(&corpus_clone, expected_corpus_head)?;

    let source_snapshot = snapshot_repository(&source_clone)?;
    let corpus_snapshot = snapshot_repository(&corpus_clone)?;
    if source_snapshot.head != expected_source_head || corpus_snapshot.head != expected_corpus_head
    {
        bail!("CLEAN_ROOM_HEAD_MISMATCH");
    }
    require_clean(&source_snapshot, "CLEAN_SOURCE_DIRTY")?;
    require_clean(&corpus_snapshot, "CLEAN_CORPUS_DIRTY")?;

    let target_dir = root.join("cargo-target");
    let test_status = Command::new("cargo")
        .args([
            "test",
            "--manifest-path",
            source_clone
                .join("tools/architecture-foundry/Cargo.toml")
                .to_string_lossy()
                .as_ref(),
            "--all-targets",
        ])
        .env("RUSTC_WRAPPER", "")
        .env("RUSTUP_TOOLCHAIN", "stable")
        .env("CARGO_TARGET_DIR", &target_dir)
        .status()
        .context("clean-room cargo test execution failed")?;
    let runtime_tests_passed = test_status.success();
    let receipts_replayed = replay_all_receipts(&source_clone, &corpus_clone)?;
    let verification = verify_corpus(program, &source_clone, &corpus_clone)?;
    let verification_valid = verification.program_valid
        && verification.manifest_valid
        && verification.invalid_lineage_records.is_empty()
        && verification.invalid_receipts.is_empty();
    let verification_digest =
        semantic_verification_digest(&verification, &source_clone, &corpus_clone)?;
    let foundry_tree_digest = digest_tree(&corpus_clone.join("foundry"))?;
    let clean_after_run = snapshot_repository(&corpus_clone)?.clean;
    if !runtime_tests_passed || !verification_valid || !clean_after_run {
        bail!(
            "CLEAN_ROOM_RUN_FAILED: run={run}, tests={runtime_tests_passed}, verification={verification_valid}, invalid_lineage={}, invalid_receipts={}, clean={clean_after_run}",
            verification.invalid_lineage_records.len(),
            verification.invalid_receipts.len(),
        );
    }
    Ok(CleanRoomRun {
        run,
        source_head: source_snapshot.head,
        corpus_head: corpus_snapshot.head,
        runtime_tests_passed,
        receipts_replayed,
        verification_valid,
        verification_digest,
        foundry_tree_digest,
        clean_after_run,
    })
}

fn semantic_verification_digest(
    report: &VerificationReport, source_root: &Path, corpus_root: &Path,
) -> Result<String> {
    let mut value = serde_json::to_value(report)?;
    normalize_verification_paths(&mut value, source_root, corpus_root);
    Ok(digest_bytes(&serde_json::to_vec(&value)?))
}

fn normalize_verification_paths(value: &mut JsonValue, source_root: &Path, corpus_root: &Path) {
    match value {
        JsonValue::String(text) => {
            let source = source_root.to_string_lossy();
            let corpus = corpus_root.to_string_lossy();
            *text = text.replace(source.as_ref(), "$SOURCE");
            *text = text.replace(corpus.as_ref(), "$CORPUS");
        }
        JsonValue::Array(values) => {
            for value in values {
                normalize_verification_paths(value, source_root, corpus_root);
            }
        }
        JsonValue::Object(values) => {
            for value in values.values_mut() {
                normalize_verification_paths(value, source_root, corpus_root);
            }
        }
        _ => {}
    }
}

fn git_clone(source: &Path, destination: &Path) -> Result<()> {
    let output = Command::new("git")
        .args([
            "clone",
            "--local",
            "--no-hardlinks",
            "--quiet",
            source.to_string_lossy().as_ref(),
            destination.to_string_lossy().as_ref(),
        ])
        .output()
        .context("git clone execution failed")?;
    if !output.status.success() {
        bail!(
            "CLEAN_ROOM_CLONE_FAILED: {}",
            String::from_utf8_lossy(&output.stderr).trim()
        );
    }
    Ok(())
}

fn git_checkout(repo: &Path, head: &str) -> Result<()> {
    let output = Command::new("git")
        .arg("-C")
        .arg(repo)
        .args(["checkout", "--detach", "--quiet", head])
        .output()
        .context("git checkout execution failed")?;
    if !output.status.success() {
        bail!(
            "CLEAN_ROOM_CHECKOUT_FAILED: {}",
            String::from_utf8_lossy(&output.stderr).trim()
        );
    }
    Ok(())
}

fn digest_tree(root: &Path) -> Result<String> {
    let mut paths = Vec::new();
    for entry in WalkDir::new(root) {
        let entry = entry.context("walk foundry tree")?;
        if entry.file_type().is_file() {
            paths.push(entry.path().to_path_buf());
        }
    }
    paths.sort();
    let mut hasher = Hasher::new();
    for path in paths {
        let relative = path
            .strip_prefix(root)
            .with_context(|| format!("tree path outside root: {}", path.display()))?;
        let bytes = fs::read(&path).with_context(|| format!("read {}", path.display()))?;
        hash_named_bytes(&mut hasher, &relative.to_string_lossy(), &bytes);
    }
    Ok(hasher.finalize().to_hex().to_string())
}

fn read_json<T: for<'de> serde::Deserialize<'de>>(path: &Path, code: &str) -> Result<T> {
    let bytes = fs::read(path).with_context(|| format!("{code}: {}", path.display()))?;
    serde_json::from_slice(&bytes).with_context(|| code.to_string())
}

fn require_clean(
    snapshot: &ggen_architecture_foundry::RepositorySnapshot, code: &str,
) -> Result<()> {
    if !snapshot.clean {
        bail!("{code}: {:?}", snapshot.dirty_entries);
    }
    Ok(())
}

fn require_admitted(state: &WorkstreamStateFile, id: &str) -> Result<()> {
    let observed = state
        .workstreams
        .get(id)
        .with_context(|| format!("WORKSTREAM_{id}_STATE_MISSING"))?;
    if observed.status != "ADMITTED" {
        bail!("WORKSTREAM_{id}_NOT_ADMITTED: {}", observed.status);
    }
    Ok(())
}

fn require_ready(state: &WorkstreamStateFile, id: &str) -> Result<()> {
    let observed = state
        .workstreams
        .get(id)
        .with_context(|| format!("WORKSTREAM_{id}_STATE_MISSING"))?;
    if observed.status != "READY" {
        bail!("WORKSTREAM_{id}_NOT_READY: {}", observed.status);
    }
    Ok(())
}

fn canonical_json<T: Serialize>(value: &T) -> Result<Vec<u8>> {
    let mut bytes = serde_json::to_vec_pretty(value)?;
    bytes.push(b'\n');
    Ok(bytes)
}

fn digest_bytes(bytes: &[u8]) -> String {
    blake3::hash(bytes).to_hex().to_string()
}

fn digest_named_outputs(outputs: &BTreeMap<String, String>) -> String {
    let mut hasher = Hasher::new();
    for (name, digest) in outputs {
        hash_named_bytes(&mut hasher, name, digest.as_bytes());
    }
    hasher.finalize().to_hex().to_string()
}

fn hash_named_bytes(hasher: &mut Hasher, name: &str, bytes: &[u8]) {
    hasher.update(&(name.len() as u64).to_le_bytes());
    hasher.update(name.as_bytes());
    hasher.update(&(bytes.len() as u64).to_le_bytes());
    hasher.update(bytes);
}

fn write_new(path: &Path, bytes: &[u8]) -> Result<()> {
    if path.exists() {
        bail!("EXISTING_OUTPUT_REFUSED: {}", path.display());
    }
    if let Some(parent) = path.parent() {
        fs::create_dir_all(parent)
            .with_context(|| format!("create directory {}", parent.display()))?;
    }
    fs::write(path, bytes).with_context(|| format!("write {}", path.display()))?;
    Ok(())
}

fn write_replace(path: &Path, bytes: &[u8]) -> Result<()> {
    if let Some(parent) = path.parent() {
        fs::create_dir_all(parent)
            .with_context(|| format!("create directory {}", parent.display()))?;
    }
    fs::write(path, bytes).with_context(|| format!("write {}", path.display()))?;
    Ok(())
}
