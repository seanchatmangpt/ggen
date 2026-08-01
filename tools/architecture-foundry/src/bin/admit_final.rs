use anyhow::{bail, Context, Result};
use blake3::Hasher;
use clap::Parser;
use ggen_architecture_foundry::{
    load_program, replay_all_receipts, snapshot_repository, validate_program, EvidenceFile,
    FinalEvidenceReport, Receipt, StandingRecord, WorkstreamStateFile, CORPUS_SCHEMA,
    FINAL_EVIDENCE_SCHEMA, RECEIPT_SCHEMA,
};
use serde::Deserialize;
use serde_json::{json, Value as JsonValue};
use std::collections::BTreeMap;
use std::fs;
use std::path::{Path, PathBuf};

const FINAL_VERIFIER_ID: &str = "ggen-foundry-final-admission-verifier/v1";

#[derive(Debug, Parser)]
#[command(
    name = "ggen-foundry-admit-final",
    version,
    about = "Recompute the terminal theorem and admit the foundry solution"
)]
struct Cli {
    #[arg(long)]
    program: PathBuf,
    #[arg(long)]
    source: PathBuf,
    #[arg(long)]
    corpus: PathBuf,
}

#[derive(Debug, Deserialize)]
struct Catalog<T> {
    entries: Vec<T>,
}

#[derive(Debug, Deserialize)]
struct CapabilityRecord {
    disposition: String,
}

#[derive(Debug, Deserialize)]
struct EquivalenceCase {
    positive_witness: bool,
    negative_falsifier: bool,
    verifier: String,
}

fn main() -> Result<()> {
    let cli = Cli::parse();
    let program = load_program(&cli.program)?;
    let validation = validate_program(&program)?;
    let source = snapshot_repository(&cli.source)?;
    let corpus = snapshot_repository(&cli.corpus)?;
    require_clean(&source, "SOURCE_WORKTREE_DIRTY")?;
    require_clean(&corpus, "CORPUS_WORKTREE_DIRTY")?;

    let foundry_root = cli.corpus.join("foundry");
    let state: WorkstreamStateFile = read_json(
        &foundry_root.join("workstreams/state.json"),
        "WORKSTREAM_STATE_INVALID",
    )?;
    let incomplete: Vec<String> = ('A'..='K')
        .filter(|id| {
            state
                .workstreams
                .get(&id.to_string())
                .map(|entry| entry.status != "ADMITTED")
                .unwrap_or(true)
        })
        .map(|id| id.to_string())
        .collect();
    if !incomplete.is_empty() {
        bail!("FINAL_WORKSTREAMS_INCOMPLETE: {incomplete:?}");
    }
    let receipts_replayed = replay_all_receipts(&cli.source, &cli.corpus)?;
    if receipts_replayed < 12 {
        bail!("FINAL_RECEIPT_PORTFOLIO_INCOMPLETE: {receipts_replayed}");
    }

    let capabilities: Catalog<CapabilityRecord> = read_json(
        &foundry_root.join("catalogs/capabilities.json"),
        "CAPABILITY_CATALOG_INVALID",
    )?;
    let equivalence: Catalog<EquivalenceCase> = read_json(
        &foundry_root.join("catalogs/equivalence.json"),
        "EQUIVALENCE_CATALOG_INVALID",
    )?;
    let subsystem_matrix: JsonValue = read_json(
        &foundry_root.join("catalogs/subsystem-evidence-matrix.json"),
        "SUBSYSTEM_MATRIX_INVALID",
    )?;
    let reference_report: JsonValue = read_json(
        &foundry_root.join("workstreams/K/admission-report.json"),
        "REFERENCE_REPORT_INVALID",
    )?;

    let unknown_dispositions = capabilities
        .entries
        .iter()
        .filter(|capability| capability.disposition == "UNKNOWN")
        .count();
    let equivalence_failures = equivalence
        .entries
        .iter()
        .filter(|case| !case.positive_witness || !case.negative_falsifier)
        .count();
    let unassigned_verifiers = equivalence
        .entries
        .iter()
        .filter(|case| case.verifier.trim().is_empty())
        .count();
    let subsystem_entries = subsystem_matrix
        .get("entries")
        .and_then(JsonValue::as_array)
        .context("SUBSYSTEM_MATRIX_ENTRIES_MISSING")?;
    let unknown_standings = subsystem_entries
        .iter()
        .filter(|entry| entry.get("standing").and_then(JsonValue::as_str) != Some("ALIVE"))
        .count();
    let reference_manufactured = reference_report
        .get("solution_admission")
        .and_then(JsonValue::as_bool)
        .unwrap_or(false)
        && reference_report
            .get("reference_build_success")
            .and_then(JsonValue::as_bool)
            .unwrap_or(false)
        && reference_report
            .get("replay_match")
            .and_then(JsonValue::as_bool)
            .unwrap_or(false);

    let expected_capabilities = capabilities.entries.len();
    let missing_equivalence_cases = expected_capabilities.saturating_sub(equivalence.entries.len());
    if expected_capabilities != 65
        || unknown_dispositions != 0
        || unknown_standings != 0
        || unassigned_verifiers != 0
        || missing_equivalence_cases != 0
        || equivalence_failures != 0
        || !reference_manufactured
    {
        bail!(
            "FINAL_THEOREM_REFUSED: capabilities={expected_capabilities}, unknown_dispositions={unknown_dispositions}, unknown_standings={unknown_standings}, unassigned={unassigned_verifiers}, missing_cases={missing_equivalence_cases}, failures={equivalence_failures}, reference={reference_manufactured}"
        );
    }

    let i_report_path = foundry_root.join("workstreams/I/admission-report.json");
    let k_report_path = foundry_root.join("workstreams/K/admission-report.json");
    let evidence = vec![
        EvidenceFile {
            repository: "corpus".to_string(),
            path: "foundry/workstreams/I/admission-report.json".to_string(),
            blake3: digest_file(&i_report_path)?,
        },
        EvidenceFile {
            repository: "corpus".to_string(),
            path: "foundry/workstreams/K/admission-report.json".to_string(),
            blake3: digest_file(&k_report_path)?,
        },
    ];
    let final_evidence = FinalEvidenceReport {
        schema_version: FINAL_EVIDENCE_SCHEMA.to_string(),
        source_head: source.head.clone(),
        corpus_head: corpus.head.clone(),
        verifier: FINAL_VERIFIER_ID.to_string(),
        predicates: program.final_predicates.clone(),
        evidence,
    };
    let evidence_bytes = canonical_json(&final_evidence)?;
    let evidence_relative = "foundry/evidence/final-evidence.json";
    write_new(&cli.corpus.join(evidence_relative), &evidence_bytes)?;

    let standing = StandingRecord {
        schema_version: CORPUS_SCHEMA.to_string(),
        program_id: program.program_id.clone(),
        standing: "ALIVE".to_string(),
        admitted: true,
        reasons: Vec::new(),
        source_head: source.head.clone(),
        corpus_head: corpus.head.clone(),
    };
    let standing_bytes = canonical_json(&standing)?;
    let standing_relative = "foundry/standing.json";
    write_replace(&cli.corpus.join(standing_relative), &standing_bytes)?;

    let theorem = json!({
        "schema_version": "ggen.enterprise-architecture-foundry.terminal-theorem/1",
        "program_id": program.program_id.clone(),
        "program_digest": validation.program_digest.clone(),
        "source_head": source.head.clone(),
        "corpus_parent_head": corpus.head.clone(),
        "workstreams_admitted": 11,
        "capabilities": expected_capabilities,
        "unknown_capabilities": 0,
        "unknown_dispositions": unknown_dispositions,
        "unknown_standings": unknown_standings,
        "unassigned_verifiers": unassigned_verifiers,
        "missing_equivalence_cases": missing_equivalence_cases,
        "equivalence_failures": equivalence_failures,
        "replay_differences": 0,
        "cross_repository_receipts_valid": true,
        "fortune_scale_reference_manufactured": reference_manufactured,
        "solution_admission": true,
        "standing": "ALIVE",
        "receipts_replayed": receipts_replayed,
        "verifier": FINAL_VERIFIER_ID,
    });
    let theorem_bytes = canonical_json(&theorem)?;
    let theorem_relative = "foundry/evidence/terminal-theorem.json";
    write_new(&cli.corpus.join(theorem_relative), &theorem_bytes)?;

    let mut outputs = BTreeMap::new();
    outputs.insert(
        format!("corpus:{evidence_relative}"),
        digest_bytes(&evidence_bytes),
    );
    outputs.insert(
        format!("corpus:{standing_relative}"),
        digest_bytes(&standing_bytes),
    );
    outputs.insert(
        format!("corpus:{theorem_relative}"),
        digest_bytes(&theorem_bytes),
    );
    let mut inputs = BTreeMap::new();
    inputs.insert(
        "work-program".to_string(),
        validation.program_digest.clone(),
    );
    inputs.insert(
        "source-tree".to_string(),
        source.tracked_tree_digest.clone(),
    );
    inputs.insert(
        "corpus-tree".to_string(),
        corpus.tracked_tree_digest.clone(),
    );
    inputs.insert(
        "subsystem-matrix".to_string(),
        digest_bytes(&serde_json::to_vec(&subsystem_matrix)?),
    );
    inputs.insert(
        "reference-report".to_string(),
        digest_bytes(&serde_json::to_vec(&reference_report)?),
    );
    let subject_digest = digest_named_outputs(&outputs);
    let receipt = Receipt {
        schema_version: RECEIPT_SCHEMA.to_string(),
        receipt_type: "SOLUTION_ADMISSION".to_string(),
        subject: program.program_name.clone(),
        subject_digest: subject_digest.clone(),
        source_head: source.head.clone(),
        corpus_head: corpus.head.clone(),
        input_digests: inputs,
        output_digests: outputs,
        run_id: subject_digest.chars().take(20).collect(),
    };
    write_new(
        &foundry_root.join("receipts/solution-admission.json"),
        &canonical_json(&receipt)?,
    )?;
    println!("{}", serde_json::to_string_pretty(&theorem)?);
    Ok(())
}

fn digest_file(path: &Path) -> Result<String> {
    Ok(digest_bytes(
        &fs::read(path).with_context(|| format!("read {}", path.display()))?,
    ))
}

fn read_json<T: for<'de> Deserialize<'de>>(path: &Path, code: &str) -> Result<T> {
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

fn canonical_json<T: serde::Serialize>(value: &T) -> Result<Vec<u8>> {
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
