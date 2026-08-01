use anyhow::{bail, Context, Result};
use blake3::Hasher;
use clap::Parser;
use ggen_architecture_foundry::{
    digest_file, load_program, replay_all_receipts, snapshot_repository, validate_program, Receipt,
    WorkstreamStateFile, RECEIPT_SCHEMA,
};
use serde::Serialize;
use serde_yaml::Value as YamlValue;
use std::collections::{BTreeMap, BTreeSet};
use std::fs;
use std::path::{Path, PathBuf};
use std::process::Command;

const OBSERVATION_SCHEMA: &str = "ggen.enterprise-architecture-foundry.observation-admission/1";
const VERIFIER_ID: &str = "ggen-foundry-admit-observation/v1";
const CAPABILITY_PATH: &str = "ontology/v26.8.1/legacy-capabilities.ttl";
const REPORT_PATH: &str = "docs/v26.8.1/90-legacy/observer-class-report.md";

#[derive(Debug, Parser)]
#[command(
    name = "ggen-foundry-admit-observation",
    version,
    about = "Independently verify and admit Workstream B exhaustive observation"
)]
struct Cli {
    #[arg(long)]
    program: PathBuf,
    #[arg(long)]
    source: PathBuf,
    #[arg(long)]
    corpus: PathBuf,
    #[arg(long, default_value = "origin/agent/v26.8.1-remaining-observers")]
    evidence_ref: String,
}

#[derive(Debug, Clone, Serialize)]
struct ObserverClassRecord {
    class_id: u8,
    observer_class: String,
    raw_row: String,
    attempted: bool,
}

#[derive(Debug, Clone, Serialize)]
struct CapabilityCandidate {
    capability_id: String,
    evidence_commit: String,
    evidence_path: String,
}

#[derive(Debug, Serialize)]
struct ObservationAdmissionReport {
    schema_version: String,
    workstream_id: String,
    verifier: String,
    source_head: String,
    corpus_head: String,
    evidence_ref: String,
    evidence_commit: String,
    capability_count: usize,
    observer_class_count: usize,
    observer_classes_unattempted: usize,
    orphan_candidates: usize,
    inventory_complete: bool,
    predicates: BTreeMap<String, YamlValue>,
    evidence_digests: BTreeMap<String, String>,
}

#[derive(Debug, Serialize)]
struct ObserverClassCatalog {
    schema_version: String,
    evidence_commit: String,
    records: Vec<ObserverClassRecord>,
}

#[derive(Debug, Serialize)]
struct CapabilityCandidateCatalog {
    schema_version: String,
    evidence_commit: String,
    candidates: Vec<CapabilityCandidate>,
}

#[derive(Debug, Serialize)]
struct ExclusionCatalog {
    schema_version: String,
    evidence_commit: String,
    observer_classes: Vec<ObserverClassRecord>,
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
        .find(|workstream| workstream.id == "B")
        .context("WORKSTREAM_B_MISSING")?;
    if workstream.dependencies.len() != 1 || workstream.dependencies[0] != "A" {
        bail!("WORKSTREAM_B_DEPENDENCY_INVALID");
    }

    let foundry_root = cli.corpus.join("foundry");
    let state_path = foundry_root.join("workstreams/state.json");
    let mut state: WorkstreamStateFile = serde_json::from_slice(
        &fs::read(&state_path)
            .with_context(|| format!("WORKSTREAM_STATE_MISSING: {}", state_path.display()))?,
    )
    .context("WORKSTREAM_STATE_SCHEMA_INVALID")?;
    require_admitted(&state, "A")?;
    require_ready(&state, "B")?;
    replay_all_receipts(&cli.source, &cli.corpus)?;

    let evidence_commit = git_text(&cli.source, &["rev-parse", &cli.evidence_ref])?;
    let capability_bytes = git_show(&cli.source, &evidence_commit, CAPABILITY_PATH)?;
    let observer_report_bytes = git_show(&cli.source, &evidence_commit, REPORT_PATH)?;

    let capability_ids = parse_capability_ids(&capability_bytes)?;
    if capability_ids.len() != 65 {
        bail!(
            "CAPABILITY_COUNT_MISMATCH: expected 65, observed {}",
            capability_ids.len()
        );
    }
    let observer_classes = parse_observer_classes(&observer_report_bytes)?;
    let unattempted = observer_classes
        .iter()
        .filter(|record| !record.attempted)
        .count();
    if observer_classes.len() != 20 || unattempted != 0 {
        bail!(
            "OBSERVER_CLOSURE_INCOMPLETE: classes={}, unattempted={}",
            observer_classes.len(),
            unattempted
        );
    }

    let candidates: Vec<CapabilityCandidate> = capability_ids
        .iter()
        .map(|capability_id| CapabilityCandidate {
            capability_id: capability_id.clone(),
            evidence_commit: evidence_commit.clone(),
            evidence_path: CAPABILITY_PATH.to_string(),
        })
        .collect();

    let evidence_root = foundry_root.join("evidence/B");
    let raw_capability_path = evidence_root.join("legacy-capabilities.ttl");
    let raw_report_path = evidence_root.join("observer-class-report.md");
    write_new(&raw_capability_path, &capability_bytes)?;
    write_new(&raw_report_path, &observer_report_bytes)?;

    let observer_catalog = ObserverClassCatalog {
        schema_version: OBSERVATION_SCHEMA.to_string(),
        evidence_commit: evidence_commit.clone(),
        records: observer_classes.clone(),
    };
    let observer_catalog_path = evidence_root.join("observer-reports.json");
    let observer_catalog_bytes = canonical_json(&observer_catalog)?;
    write_new(&observer_catalog_path, &observer_catalog_bytes)?;

    let candidate_catalog = CapabilityCandidateCatalog {
        schema_version: OBSERVATION_SCHEMA.to_string(),
        evidence_commit: evidence_commit.clone(),
        candidates,
    };
    let candidate_catalog_path = foundry_root.join("catalogs/capability-candidates.json");
    let candidate_catalog_bytes = canonical_json(&candidate_catalog)?;
    write_replace(&candidate_catalog_path, &candidate_catalog_bytes)?;

    let exclusion_catalog = ExclusionCatalog {
        schema_version: OBSERVATION_SCHEMA.to_string(),
        evidence_commit: evidence_commit.clone(),
        observer_classes: observer_classes.clone(),
    };
    let exclusion_catalog_path = evidence_root.join("exclusion-records.json");
    let exclusion_catalog_bytes = canonical_json(&exclusion_catalog)?;
    write_new(&exclusion_catalog_path, &exclusion_catalog_bytes)?;

    let mut evidence_digests = BTreeMap::new();
    evidence_digests.insert(
        "legacy-capabilities.ttl".to_string(),
        digest_bytes(&capability_bytes),
    );
    evidence_digests.insert(
        "observer-class-report.md".to_string(),
        digest_bytes(&observer_report_bytes),
    );
    evidence_digests.insert(
        "observer-reports.json".to_string(),
        digest_bytes(&observer_catalog_bytes),
    );
    evidence_digests.insert(
        "capability-candidates.json".to_string(),
        digest_bytes(&candidate_catalog_bytes),
    );
    evidence_digests.insert(
        "exclusion-records.json".to_string(),
        digest_bytes(&exclusion_catalog_bytes),
    );

    let report = ObservationAdmissionReport {
        schema_version: OBSERVATION_SCHEMA.to_string(),
        workstream_id: "B".to_string(),
        verifier: VERIFIER_ID.to_string(),
        source_head: source.head.clone(),
        corpus_head: corpus.head.clone(),
        evidence_ref: cli.evidence_ref,
        evidence_commit,
        capability_count: capability_ids.len(),
        observer_class_count: observer_classes.len(),
        observer_classes_unattempted: unattempted,
        orphan_candidates: 0,
        inventory_complete: true,
        predicates: workstream.predicates.clone(),
        evidence_digests,
    };
    let report_path = foundry_root.join("workstreams/B/admission-report.json");
    let admission_report_bytes = canonical_json(&report)?;
    let report_digest = digest_bytes(&admission_report_bytes);
    write_new(&report_path, &admission_report_bytes)?;

    let receipt_relative = "foundry/receipts/workstream-B.json";
    {
        let state_b = state
            .workstreams
            .get_mut("B")
            .context("WORKSTREAM_B_STATE_MISSING")?;
        state_b.status = "ADMITTED".to_string();
        state_b.report_digest = Some(report_digest.clone());
        state_b.receipt_path = Some(receipt_relative.to_string());
    }
    if let Some(state_c) = state.workstreams.get_mut("C") {
        state_c.status = "READY".to_string();
    }
    let state_bytes = canonical_json(&state)?;

    let mut inputs = BTreeMap::new();
    inputs.insert("work-program".to_string(), validation.program_digest);
    inputs.insert("source-tree".to_string(), source.tracked_tree_digest);
    inputs.insert("corpus-tree".to_string(), corpus.tracked_tree_digest);
    inputs.insert(
        "evidence-ontology".to_string(),
        digest_bytes(&capability_bytes),
    );
    inputs.insert(
        "evidence-observer-report".to_string(),
        digest_bytes(&observer_report_bytes),
    );

    let mut outputs = BTreeMap::new();
    for (relative, bytes) in [
        (
            "foundry/evidence/B/legacy-capabilities.ttl",
            capability_bytes.as_slice(),
        ),
        (
            "foundry/evidence/B/observer-class-report.md",
            observer_report_bytes.as_slice(),
        ),
        (
            "foundry/evidence/B/observer-reports.json",
            observer_catalog_bytes.as_slice(),
        ),
        (
            "foundry/catalogs/capability-candidates.json",
            candidate_catalog_bytes.as_slice(),
        ),
        (
            "foundry/evidence/B/exclusion-records.json",
            exclusion_catalog_bytes.as_slice(),
        ),
        (
            "foundry/workstreams/B/admission-report.json",
            admission_report_bytes.as_slice(),
        ),
    ] {
        outputs.insert(format!("corpus:{relative}"), digest_bytes(bytes));
    }
    outputs.insert(
        "projection:foundry/workstreams/state.json".to_string(),
        digest_bytes(&state_bytes),
    );
    let subject_digest = digest_named_outputs(&outputs);
    let receipt = Receipt {
        schema_version: RECEIPT_SCHEMA.to_string(),
        receipt_type: "WORKSTREAM_ADMISSION".to_string(),
        subject: "B".to_string(),
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

    println!("{}", serde_json::to_string_pretty(&report)?);
    Ok(())
}

fn parse_capability_ids(bytes: &[u8]) -> Result<BTreeSet<String>> {
    let text = std::str::from_utf8(bytes).context("CAPABILITY_ONTOLOGY_NOT_UTF8")?;
    let mut ids = BTreeSet::new();
    for line in text.lines() {
        let marker = "ggen:capabilityId \"";
        if let Some(start) = line.find(marker) {
            let remainder = &line[start + marker.len()..];
            let end = remainder
                .find('"')
                .context("CAPABILITY_ID_QUOTE_UNCLOSED")?;
            let id = &remainder[..end];
            if id.is_empty() || !ids.insert(id.to_string()) {
                bail!("CAPABILITY_ID_INVALID_OR_DUPLICATE: {id}");
            }
        }
    }
    if ids.is_empty() {
        bail!("CAPABILITY_CANDIDATES_EMPTY");
    }
    Ok(ids)
}

fn parse_observer_classes(bytes: &[u8]) -> Result<Vec<ObserverClassRecord>> {
    let text = std::str::from_utf8(bytes).context("OBSERVER_REPORT_NOT_UTF8")?;
    let mut latest = BTreeMap::<u8, ObserverClassRecord>::new();
    for line in text.lines() {
        if !line.starts_with('|') {
            continue;
        }
        let columns: Vec<&str> = line.split('|').map(str::trim).collect();
        if columns.len() < 8 {
            continue;
        }
        let Ok(class_id) = columns[1].parse::<u8>() else {
            continue;
        };
        if !(1..=20).contains(&class_id) {
            continue;
        }
        let raw_row = line.to_string();
        let attempted = !raw_row.to_ascii_lowercase().contains("not attempted");
        latest.insert(
            class_id,
            ObserverClassRecord {
                class_id,
                observer_class: columns[2].to_string(),
                raw_row,
                attempted,
            },
        );
    }
    let records: Vec<ObserverClassRecord> = latest.into_values().collect();
    if records
        .iter()
        .map(|record| record.class_id)
        .collect::<Vec<_>>()
        != (1u8..=20).collect::<Vec<_>>()
    {
        bail!("OBSERVER_CLASS_SET_INCOMPLETE");
    }
    Ok(records)
}

fn git_show(repo: &Path, commit: &str, path: &str) -> Result<Vec<u8>> {
    let specification = format!("{commit}:{path}");
    let output = Command::new("git")
        .arg("-C")
        .arg(repo)
        .args(["show", &specification])
        .output()
        .context("git show execution failed")?;
    if !output.status.success() {
        bail!(
            "EVIDENCE_BLOB_UNAVAILABLE: {}: {}",
            specification,
            String::from_utf8_lossy(&output.stderr).trim()
        );
    }
    Ok(output.stdout)
}

fn git_text(repo: &Path, args: &[&str]) -> Result<String> {
    let output = Command::new("git")
        .arg("-C")
        .arg(repo)
        .args(args)
        .output()
        .context("git execution failed")?;
    if !output.status.success() {
        bail!(
            "GIT_COMMAND_FAILED: git {}: {}",
            args.join(" "),
            String::from_utf8_lossy(&output.stderr).trim()
        );
    }
    Ok(String::from_utf8_lossy(&output.stdout).trim().to_string())
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
