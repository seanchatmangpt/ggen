use anyhow::{bail, Context, Result};
use blake3::Hasher;
use clap::Parser;
use ggen_architecture_foundry::{
    digest_file, load_program, replay_all_receipts, snapshot_repository, validate_program,
    FoundryManifest, Receipt, WorkstreamStateFile, CORPUS_SCHEMA, RECEIPT_SCHEMA,
};
use serde::Serialize;
use serde_yaml::Value as YamlValue;
use std::collections::BTreeMap;
use std::fs;
use std::path::{Path, PathBuf};
use std::process::Command;

const BASELINE_ADMISSION_SCHEMA: &str = "ggen.enterprise-architecture-foundry.baseline-admission/1";
const VERIFIER_ID: &str = "ggen-foundry-admit-baseline/v1";

#[derive(Debug, Parser)]
#[command(
    name = "ggen-foundry-admit-baseline",
    version,
    about = "Independently recompute and admit Enterprise Architecture Foundry Workstream A"
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
struct BaselineAdmissionReport {
    schema_version: String,
    workstream_id: String,
    verifier: String,
    source_head: String,
    corpus_head: String,
    source_tree_digest: String,
    corpus_tree_digest: String,
    program_digest: String,
    foundry_manifest_digest: String,
    initialization_receipt_digest: String,
    initialization_parent_is_ancestor: bool,
    receipts_replayed: usize,
    predicates: BTreeMap<String, YamlValue>,
}

fn main() -> Result<()> {
    let cli = Cli::parse();
    let program = load_program(&cli.program)?;
    let validation = validate_program(&program)?;
    let source = snapshot_repository(&cli.source)?;
    let corpus = snapshot_repository(&cli.corpus)?;

    if !source.clean {
        bail!("SOURCE_WORKTREE_DIRTY: {:?}", source.dirty_entries);
    }
    if !corpus.clean {
        bail!("CORPUS_WORKTREE_DIRTY: {:?}", corpus.dirty_entries);
    }

    let workstream = program
        .workstreams
        .iter()
        .find(|workstream| workstream.id == "A")
        .context("WORKSTREAM_A_MISSING")?;
    if !workstream.dependencies.is_empty() {
        bail!("WORKSTREAM_A_DEPENDENCY_INVALID: expected no dependencies");
    }

    let foundry_root = cli.corpus.join("foundry");
    let manifest_path = foundry_root.join("foundry-manifest.json");
    let initialization_receipt_path = foundry_root.join("receipts/initialization.json");
    let state_path = foundry_root.join("workstreams/state.json");

    let manifest_bytes = fs::read(&manifest_path)
        .with_context(|| format!("DOCUMENT_EVIDENCE_MISSING: {}", manifest_path.display()))?;
    let manifest: FoundryManifest =
        serde_json::from_slice(&manifest_bytes).context("FOUNDRY_MANIFEST_SCHEMA_INVALID")?;
    if manifest.schema_version != CORPUS_SCHEMA {
        bail!(
            "FOUNDRY_MANIFEST_SCHEMA_INVALID: {}",
            manifest.schema_version
        );
    }
    if manifest.program_id != program.program_id
        || manifest.program_digest != validation.program_digest
        || manifest.source_repository != program.source_repository
        || manifest.corpus_repository != program.corpus_repository
    {
        bail!("FOUNDRY_MANIFEST_AUTHORITY_MISMATCH");
    }
    if manifest.source_head != source.head {
        bail!(
            "SOURCE_HEAD_STALE: manifest={}, current={}",
            manifest.source_head,
            source.head
        );
    }

    let initialization_parent_is_ancestor =
        git_is_ancestor(&cli.corpus, &manifest.corpus_parent_head, &corpus.head)?;
    if !initialization_parent_is_ancestor {
        bail!(
            "CORPUS_LINEAGE_BROKEN: parent {} is not an ancestor of {}",
            manifest.corpus_parent_head,
            corpus.head
        );
    }

    let initialization_receipt_bytes =
        fs::read(&initialization_receipt_path).with_context(|| {
            format!(
                "INITIALIZATION_RECEIPT_MISSING: {}",
                initialization_receipt_path.display()
            )
        })?;
    let initialization_receipt: Receipt = serde_json::from_slice(&initialization_receipt_bytes)
        .context("INITIALIZATION_RECEIPT_SCHEMA_INVALID")?;
    if initialization_receipt.schema_version != RECEIPT_SCHEMA
        || initialization_receipt.receipt_type != "CORPUS_INITIALIZATION"
        || initialization_receipt.source_head != source.head
        || initialization_receipt.corpus_head != manifest.corpus_parent_head
    {
        bail!("INITIALIZATION_RECEIPT_AUTHORITY_MISMATCH");
    }

    let receipts_replayed = replay_all_receipts(&cli.source, &cli.corpus)?;
    if receipts_replayed == 0 {
        bail!("INITIALIZATION_RECEIPT_NOT_REPLAYED");
    }

    let mut state: WorkstreamStateFile = serde_json::from_slice(
        &fs::read(&state_path)
            .with_context(|| format!("WORKSTREAM_STATE_MISSING: {}", state_path.display()))?,
    )
    .context("WORKSTREAM_STATE_SCHEMA_INVALID")?;
    let current_a = state
        .workstreams
        .get("A")
        .context("WORKSTREAM_A_STATE_MISSING")?;
    if current_a.status == "ADMITTED" {
        bail!("WORKSTREAM_A_ALREADY_ADMITTED");
    }
    if current_a.status != "READY" {
        bail!("WORKSTREAM_A_NOT_READY: {}", current_a.status);
    }

    let report = BaselineAdmissionReport {
        schema_version: BASELINE_ADMISSION_SCHEMA.to_string(),
        workstream_id: "A".to_string(),
        verifier: VERIFIER_ID.to_string(),
        source_head: source.head.clone(),
        corpus_head: corpus.head.clone(),
        source_tree_digest: source.tracked_tree_digest.clone(),
        corpus_tree_digest: corpus.tracked_tree_digest.clone(),
        program_digest: validation.program_digest.clone(),
        foundry_manifest_digest: digest_bytes(&manifest_bytes),
        initialization_receipt_digest: digest_bytes(&initialization_receipt_bytes),
        initialization_parent_is_ancestor,
        receipts_replayed,
        predicates: workstream.predicates.clone(),
    };

    let report_path = foundry_root.join("workstreams/A/admission-report.json");
    let report_bytes = canonical_json(&report)?;
    let report_digest = digest_bytes(&report_bytes);
    write_new(&report_path, &report_bytes)?;

    let receipt_relative = "foundry/receipts/workstream-A.json";
    {
        let state_a = state
            .workstreams
            .get_mut("A")
            .context("WORKSTREAM_A_STATE_MISSING")?;
        state_a.status = "ADMITTED".to_string();
        state_a.report_digest = Some(report_digest.clone());
        state_a.receipt_path = Some(receipt_relative.to_string());
    }
    if let Some(state_b) = state.workstreams.get_mut("B") {
        state_b.status = "READY".to_string();
    }

    let state_bytes = canonical_json(&state)?;
    let state_digest = digest_bytes(&state_bytes);

    let mut input_digests = BTreeMap::new();
    input_digests.insert("work-program".to_string(), validation.program_digest);
    input_digests.insert("foundry-manifest".to_string(), digest_file(&manifest_path)?);
    input_digests.insert(
        "initialization-receipt".to_string(),
        digest_file(&initialization_receipt_path)?,
    );
    input_digests.insert("source-tree".to_string(), source.tracked_tree_digest);
    input_digests.insert("corpus-tree".to_string(), corpus.tracked_tree_digest);

    let mut output_digests = BTreeMap::new();
    output_digests.insert(
        "corpus:foundry/workstreams/A/admission-report.json".to_string(),
        report_digest,
    );
    output_digests.insert(
        "projection:foundry/workstreams/state.json".to_string(),
        state_digest,
    );
    let subject_digest = digest_named_outputs(&output_digests);
    let receipt = Receipt {
        schema_version: RECEIPT_SCHEMA.to_string(),
        receipt_type: "WORKSTREAM_ADMISSION".to_string(),
        subject: "A".to_string(),
        subject_digest: subject_digest.clone(),
        source_head: source.head,
        corpus_head: corpus.head,
        input_digests,
        output_digests,
        run_id: subject_digest.chars().take(20).collect(),
    };
    let receipt_bytes = canonical_json(&receipt)?;
    write_new(&cli.corpus.join(receipt_relative), &receipt_bytes)?;
    write_replace(&state_path, &state_bytes)?;

    println!("{}", serde_json::to_string_pretty(&report)?);
    Ok(())
}

fn git_is_ancestor(repo: &Path, ancestor: &str, descendant: &str) -> Result<bool> {
    let status = Command::new("git")
        .arg("-C")
        .arg(repo)
        .args(["merge-base", "--is-ancestor", ancestor, descendant])
        .status()
        .context("git merge-base execution failed")?;
    match status.code() {
        Some(0) => Ok(true),
        Some(1) => Ok(false),
        code => bail!("git merge-base failed with status {code:?}"),
    }
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
