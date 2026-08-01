use anyhow::{bail, Context, Result};
use blake3::Hasher;
use clap::Parser;
use ggen_architecture_foundry::{
    load_program, replay_all_receipts, snapshot_repository, validate_program, Receipt,
    WorkstreamStateFile, RECEIPT_SCHEMA,
};
use serde::{Deserialize, Serialize};
use serde_yaml::Value as YamlValue;
use std::collections::{BTreeMap, BTreeSet};
use std::fs;
use std::path::{Path, PathBuf};

const CLASSIFICATION_SCHEMA: &str =
    "ggen.enterprise-architecture-foundry.kernel-corpus-classification/1";
const VERIFIER_ID: &str = "ggen-foundry-admit-classification/v1";

#[derive(Debug, Parser)]
#[command(
    name = "ggen-foundry-admit-classification",
    version,
    about = "Classify every admitted capability into the ggen kernel or ggen-legacy corpus"
)]
struct Cli {
    #[arg(long)]
    program: PathBuf,
    #[arg(long)]
    source: PathBuf,
    #[arg(long)]
    corpus: PathBuf,
}

#[derive(Debug, Clone, Deserialize)]
struct CapabilityCatalog {
    entries: Vec<CapabilityRecord>,
}

#[derive(Debug, Clone, Deserialize)]
struct CapabilityRecord {
    capability_id: String,
    historical_source_commit: String,
    legacy_source_path: String,
    owning_subsystem: String,
    historical_semantic_owner: String,
    replacement_owner: String,
    admitted_owner: String,
    disposition: String,
    migration_path: String,
    rollback_path: String,
    archive_path: String,
    refusal_code: String,
    refusal_rationale: String,
}

#[derive(Debug, Clone, Serialize)]
struct ClassificationRecord {
    capability_id: String,
    classification: String,
    kernel_owner: String,
    corpus_destination: String,
    source_retirement_allowed: bool,
    classification_basis: String,
}

#[derive(Debug, Clone, Serialize)]
struct MigrationDependency {
    capability_id: String,
    predecessor_capabilities: Vec<String>,
    required_destination: String,
    destination_admission_required: bool,
    equivalence_required: bool,
    recovery_required: bool,
}

#[derive(Debug, Clone, Serialize)]
struct RecoveryPlan {
    capability_id: String,
    historical_source_commit: String,
    legacy_source_path: String,
    archive_path: String,
    rollback_path: String,
    recovery_command: String,
    recovery_evidence_required: bool,
}

#[derive(Debug, Serialize)]
struct Catalog<T> {
    schema_version: String,
    source_catalog_digest: String,
    entries: Vec<T>,
}

#[derive(Debug, Serialize)]
struct ClassificationAdmissionReport {
    schema_version: String,
    workstream_id: String,
    verifier: String,
    source_head: String,
    corpus_head: String,
    component_count: usize,
    unclassified_components: usize,
    classification_conflicts: usize,
    migration_dependencies_closed: bool,
    classification_counts: BTreeMap<String, usize>,
    predicates: BTreeMap<String, YamlValue>,
    source_catalog_digest: String,
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
        .find(|candidate| candidate.id == "D")
        .context("WORKSTREAM_D_MISSING")?;
    if workstream.dependencies.len() != 1 || workstream.dependencies[0] != "C" {
        bail!("WORKSTREAM_D_DEPENDENCY_INVALID");
    }

    let foundry_root = cli.corpus.join("foundry");
    let state_path = foundry_root.join("workstreams/state.json");
    let mut state: WorkstreamStateFile = serde_json::from_slice(
        &fs::read(&state_path)
            .with_context(|| format!("WORKSTREAM_STATE_MISSING: {}", state_path.display()))?,
    )
    .context("WORKSTREAM_STATE_SCHEMA_INVALID")?;
    require_admitted(&state, "C")?;
    require_ready(&state, "D")?;
    replay_all_receipts(&cli.source, &cli.corpus)?;

    let capability_path = foundry_root.join("catalogs/capabilities.json");
    let capability_bytes = fs::read(&capability_path)
        .with_context(|| format!("CAPABILITY_CATALOG_MISSING: {}", capability_path.display()))?;
    let source_catalog_digest = digest_bytes(&capability_bytes);
    let capability_catalog: CapabilityCatalog =
        serde_json::from_slice(&capability_bytes).context("CAPABILITY_CATALOG_INVALID")?;
    if capability_catalog.entries.len() != 65 {
        bail!(
            "CLASSIFICATION_INPUT_COUNT_MISMATCH: expected 65, observed {}",
            capability_catalog.entries.len()
        );
    }

    let mut ids = BTreeSet::new();
    let mut classifications = Vec::new();
    let mut dependencies = Vec::new();
    let mut recovery_plans = Vec::new();
    let mut classification_counts = BTreeMap::new();
    let mut unclassified_components = 0usize;
    let mut classification_conflicts = 0usize;

    for capability in &capability_catalog.entries {
        if !ids.insert(capability.capability_id.clone()) {
            classification_conflicts += 1;
            continue;
        }
        let (classification, kernel_owner, corpus_destination, basis) = classify(capability)?;
        if classification.is_empty() || corpus_destination.is_empty() {
            unclassified_components += 1;
        }
        *classification_counts
            .entry(classification.clone())
            .or_insert(0usize) += 1;

        classifications.push(ClassificationRecord {
            capability_id: capability.capability_id.clone(),
            classification: classification.clone(),
            kernel_owner,
            corpus_destination: corpus_destination.clone(),
            source_retirement_allowed: false,
            classification_basis: basis,
        });
        dependencies.push(MigrationDependency {
            capability_id: capability.capability_id.clone(),
            predecessor_capabilities: Vec::new(),
            required_destination: corpus_destination,
            destination_admission_required: true,
            equivalence_required: matches!(
                capability.disposition.as_str(),
                "PRESERVED" | "SUBSUMED" | "REPLACED"
            ),
            recovery_required: true,
        });
        recovery_plans.push(RecoveryPlan {
            capability_id: capability.capability_id.clone(),
            historical_source_commit: capability.historical_source_commit.clone(),
            legacy_source_path: capability.legacy_source_path.clone(),
            archive_path: capability.archive_path.clone(),
            rollback_path: capability.rollback_path.clone(),
            recovery_command: recovery_command(capability),
            recovery_evidence_required: true,
        });
    }

    let migration_dependencies_closed = dependencies.len() == capability_catalog.entries.len()
        && recovery_plans.len() == capability_catalog.entries.len();
    if unclassified_components != 0
        || classification_conflicts != 0
        || !migration_dependencies_closed
    {
        bail!(
            "CLASSIFICATION_REFUSED: unclassified={unclassified_components}, conflicts={classification_conflicts}, dependencies_closed={migration_dependencies_closed}"
        );
    }

    let classification_catalog = Catalog {
        schema_version: CLASSIFICATION_SCHEMA.to_string(),
        source_catalog_digest: source_catalog_digest.clone(),
        entries: classifications,
    };
    let dependency_catalog = Catalog {
        schema_version: CLASSIFICATION_SCHEMA.to_string(),
        source_catalog_digest: source_catalog_digest.clone(),
        entries: dependencies,
    };
    let recovery_catalog = Catalog {
        schema_version: CLASSIFICATION_SCHEMA.to_string(),
        source_catalog_digest: source_catalog_digest.clone(),
        entries: recovery_plans,
    };
    let classification_bytes = canonical_json(&classification_catalog)?;
    let dependency_bytes = canonical_json(&dependency_catalog)?;
    let recovery_bytes = canonical_json(&recovery_catalog)?;

    let classification_path = foundry_root.join("catalogs/component-classification.json");
    let dependency_path = foundry_root.join("catalogs/migration-dependency-graph.json");
    let recovery_path = foundry_root.join("catalogs/recovery-plans.json");
    write_new(&classification_path, &classification_bytes)?;
    write_new(&dependency_path, &dependency_bytes)?;
    write_new(&recovery_path, &recovery_bytes)?;

    let report = ClassificationAdmissionReport {
        schema_version: CLASSIFICATION_SCHEMA.to_string(),
        workstream_id: "D".to_string(),
        verifier: VERIFIER_ID.to_string(),
        source_head: source.head.clone(),
        corpus_head: corpus.head.clone(),
        component_count: 65,
        unclassified_components,
        classification_conflicts,
        migration_dependencies_closed,
        classification_counts,
        predicates: workstream.predicates.clone(),
        source_catalog_digest: source_catalog_digest.clone(),
    };
    let report_path = foundry_root.join("workstreams/D/admission-report.json");
    let report_bytes = canonical_json(&report)?;
    let report_digest = digest_bytes(&report_bytes);
    write_new(&report_path, &report_bytes)?;

    let receipt_relative = "foundry/receipts/workstream-D.json";
    {
        let state_d = state
            .workstreams
            .get_mut("D")
            .context("WORKSTREAM_D_STATE_MISSING")?;
        state_d.status = "ADMITTED".to_string();
        state_d.report_digest = Some(report_digest.clone());
        state_d.receipt_path = Some(receipt_relative.to_string());
    }
    if let Some(state_e) = state.workstreams.get_mut("E") {
        state_e.status = "READY".to_string();
    }
    let state_bytes = canonical_json(&state)?;

    let mut inputs = BTreeMap::new();
    inputs.insert("work-program".to_string(), validation.program_digest);
    inputs.insert("source-tree".to_string(), source.tracked_tree_digest);
    inputs.insert("corpus-tree".to_string(), corpus.tracked_tree_digest);
    inputs.insert("capability-catalog".to_string(), source_catalog_digest);

    let mut outputs = BTreeMap::new();
    for (relative, bytes) in [
        (
            "foundry/catalogs/component-classification.json",
            classification_bytes.as_slice(),
        ),
        (
            "foundry/catalogs/migration-dependency-graph.json",
            dependency_bytes.as_slice(),
        ),
        (
            "foundry/catalogs/recovery-plans.json",
            recovery_bytes.as_slice(),
        ),
        (
            "foundry/workstreams/D/admission-report.json",
            report_bytes.as_slice(),
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
        subject: "D".to_string(),
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

fn classify(capability: &CapabilityRecord) -> Result<(String, String, String, String)> {
    let subsystem = sanitize_segment(&capability.owning_subsystem);
    let id = sanitize_segment(&capability.capability_id);
    let destination = format!("foundry/corpus/components/{subsystem}/{id}");
    match capability.disposition.as_str() {
        "PRESERVED" => Ok((
            "KERNEL_RETAINED_WITH_CORPUS_WITNESS".to_string(),
            capability.admitted_owner.clone(),
            destination,
            "preserved behavior remains in the kernel while its historical witness enters the corpus"
                .to_string(),
        )),
        "REPLACED" | "SUBSUMED" => Ok((
            "CORPUS_HISTORICAL_IMPLEMENTATION".to_string(),
            capability.admitted_owner.clone(),
            destination,
            "historical implementation is retained as a foundry witness; replacement owner remains in ggen"
                .to_string(),
        )),
        "REFUSED" => Ok((
            "CORPUS_REFUSAL_WITNESS".to_string(),
            capability.admitted_owner.clone(),
            destination,
            format!(
                "refused capability retained with typed refusal {}: {}",
                capability.refusal_code, capability.refusal_rationale
            ),
        )),
        "ARCHIVED" => Ok((
            "CORPUS_ARCHIVE".to_string(),
            capability.admitted_owner.clone(),
            destination,
            "archived capability retained for provenance and recovery".to_string(),
        )),
        other => bail!("CLASSIFICATION_DISPOSITION_UNKNOWN: {other}"),
    }
}

fn recovery_command(capability: &CapabilityRecord) -> String {
    let commit = capability
        .historical_source_commit
        .split_whitespace()
        .next()
        .unwrap_or("<historical-commit>")
        .trim_matches(|character: char| !character.is_ascii_hexdigit());
    let path = capability
        .legacy_source_path
        .split(" (deleted")
        .next()
        .unwrap_or(&capability.legacy_source_path)
        .trim();
    if commit.is_empty() || path.is_empty() {
        format!(
            "resolve historical source from archive evidence for {}",
            capability.capability_id
        )
    } else {
        format!("git show {commit}^:{path}")
    }
}

fn sanitize_segment(value: &str) -> String {
    let sanitized: String = value
        .chars()
        .map(|character| {
            if character.is_ascii_alphanumeric() || matches!(character, '-' | '_' | '.') {
                character
            } else {
                '-'
            }
        })
        .collect();
    sanitized.trim_matches('-').to_string()
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
