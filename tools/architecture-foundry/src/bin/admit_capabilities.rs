use anyhow::{bail, Context, Result};
use blake3::Hasher;
use clap::Parser;
use ggen_architecture_foundry::{
    load_program, replay_all_receipts, snapshot_repository, validate_program, Receipt,
    WorkstreamStateFile, RECEIPT_SCHEMA,
};
use serde::Serialize;
use serde_yaml::Value as YamlValue;
use std::collections::{BTreeMap, BTreeSet};
use std::fs;
use std::path::{Path, PathBuf};

const CAPABILITY_ADMISSION_SCHEMA: &str =
    "ggen.enterprise-architecture-foundry.capability-admission/1";
const VERIFIER_ID: &str = "ggen-foundry-admit-capabilities/v1";

#[derive(Debug, Parser)]
#[command(
    name = "ggen-foundry-admit-capabilities",
    version,
    about = "Parse, validate, and admit Workstream C capabilities"
)]
struct Cli {
    #[arg(long)]
    program: PathBuf,
    #[arg(long)]
    source: PathBuf,
    #[arg(long)]
    corpus: PathBuf,
}

#[derive(Debug, Clone, Serialize)]
struct CapabilityRecord {
    capability_id: String,
    subject: String,
    historical_source_commit: String,
    legacy_source_path: String,
    owning_subsystem: String,
    historical_semantic_owner: String,
    replacement_owner: String,
    admitted_owner: String,
    disposition: String,
    input_contract: String,
    output_contract: String,
    error_contract: String,
    side_effects: String,
    ordering_requirements: String,
    default_behavior: String,
    configuration_dependencies: String,
    evidence_fixtures: String,
    migration_path: String,
    rollback_path: String,
    archive_path: String,
    refusal_code: String,
    refusal_rationale: String,
}

#[derive(Debug, Clone, Serialize)]
struct ProvenanceRecord {
    capability_id: String,
    historical_source_commit: String,
    legacy_source_path: String,
    evidence_path: String,
    evidence_digest: String,
}

#[derive(Debug, Clone, Serialize)]
struct OwnershipRecord {
    capability_id: String,
    subsystem: String,
    historical_owner: String,
    replacement_owner: String,
    admitted_owner: String,
    ownership_basis: String,
}

#[derive(Debug, Clone, Serialize)]
struct DispositionObligation {
    capability_id: String,
    disposition: String,
    destination_required: bool,
    equivalence_required: bool,
    refusal_evidence_required: bool,
    recovery_required: bool,
}

#[derive(Debug, Serialize)]
struct Catalog<T> {
    schema_version: String,
    evidence_digest: String,
    entries: Vec<T>,
}

#[derive(Debug, Serialize)]
struct CapabilityAdmissionReport {
    schema_version: String,
    workstream_id: String,
    verifier: String,
    source_head: String,
    corpus_head: String,
    capability_count: usize,
    provenance_count: usize,
    ownership_count: usize,
    obligation_count: usize,
    unknown_capabilities: usize,
    capabilities_without_provenance: usize,
    contradictory_owners: usize,
    disposition_counts: BTreeMap<String, usize>,
    subsystem_counts: BTreeMap<String, usize>,
    predicates: BTreeMap<String, YamlValue>,
    evidence_digest: String,
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
        .find(|workstream| workstream.id == "C")
        .context("WORKSTREAM_C_MISSING")?;
    if workstream.dependencies.len() != 1 || workstream.dependencies[0] != "B" {
        bail!("WORKSTREAM_C_DEPENDENCY_INVALID");
    }

    let foundry_root = cli.corpus.join("foundry");
    let state_path = foundry_root.join("workstreams/state.json");
    let mut state: WorkstreamStateFile = serde_json::from_slice(
        &fs::read(&state_path)
            .with_context(|| format!("WORKSTREAM_STATE_MISSING: {}", state_path.display()))?,
    )
    .context("WORKSTREAM_STATE_SCHEMA_INVALID")?;
    require_admitted(&state, "B")?;
    require_ready(&state, "C")?;
    replay_all_receipts(&cli.source, &cli.corpus)?;

    let evidence_path = foundry_root.join("evidence/B/legacy-capabilities.ttl");
    let evidence_bytes = fs::read(&evidence_path)
        .with_context(|| format!("CAPABILITY_EVIDENCE_MISSING: {}", evidence_path.display()))?;
    let evidence_digest = digest_bytes(&evidence_bytes);
    let capabilities = parse_capabilities(&evidence_bytes)?;
    if capabilities.len() != 65 {
        bail!(
            "CAPABILITY_COUNT_MISMATCH: expected 65, observed {}",
            capabilities.len()
        );
    }

    let mut seen_ids = BTreeSet::new();
    let mut provenance = Vec::new();
    let mut ownership = Vec::new();
    let mut obligations = Vec::new();
    let mut disposition_counts = BTreeMap::new();
    let mut subsystem_counts = BTreeMap::new();
    let mut unknown_capabilities = 0usize;
    let mut capabilities_without_provenance = 0usize;
    let mut contradictory_owners = 0usize;

    for capability in &capabilities {
        if capability.capability_id.is_empty() || !seen_ids.insert(capability.capability_id.clone())
        {
            unknown_capabilities += 1;
        }
        if capability.historical_source_commit.trim().is_empty()
            || capability.legacy_source_path.trim().is_empty()
        {
            capabilities_without_provenance += 1;
        }
        if capability.admitted_owner.trim().is_empty() {
            contradictory_owners += 1;
        }
        *disposition_counts
            .entry(capability.disposition.clone())
            .or_insert(0) += 1;
        *subsystem_counts
            .entry(capability.owning_subsystem.clone())
            .or_insert(0) += 1;

        provenance.push(ProvenanceRecord {
            capability_id: capability.capability_id.clone(),
            historical_source_commit: capability.historical_source_commit.clone(),
            legacy_source_path: capability.legacy_source_path.clone(),
            evidence_path: "foundry/evidence/B/legacy-capabilities.ttl".to_string(),
            evidence_digest: evidence_digest.clone(),
        });
        ownership.push(OwnershipRecord {
            capability_id: capability.capability_id.clone(),
            subsystem: capability.owning_subsystem.clone(),
            historical_owner: capability.historical_semantic_owner.clone(),
            replacement_owner: capability.replacement_owner.clone(),
            admitted_owner: capability.admitted_owner.clone(),
            ownership_basis: ownership_basis(capability),
        });
        obligations.push(DispositionObligation {
            capability_id: capability.capability_id.clone(),
            disposition: capability.disposition.clone(),
            destination_required: true,
            equivalence_required: matches!(
                capability.disposition.as_str(),
                "PRESERVED" | "SUBSUMED" | "REPLACED"
            ),
            refusal_evidence_required: capability.disposition == "REFUSED",
            recovery_required: true,
        });
    }

    if unknown_capabilities != 0
        || capabilities_without_provenance != 0
        || contradictory_owners != 0
    {
        bail!(
            "CAPABILITY_ADMISSION_REFUSED: unknown={unknown_capabilities}, provenance_missing={capabilities_without_provenance}, contradictory_owners={contradictory_owners}"
        );
    }

    let capability_catalog = Catalog {
        schema_version: CAPABILITY_ADMISSION_SCHEMA.to_string(),
        evidence_digest: evidence_digest.clone(),
        entries: capabilities,
    };
    let provenance_catalog = Catalog {
        schema_version: CAPABILITY_ADMISSION_SCHEMA.to_string(),
        evidence_digest: evidence_digest.clone(),
        entries: provenance,
    };
    let ownership_catalog = Catalog {
        schema_version: CAPABILITY_ADMISSION_SCHEMA.to_string(),
        evidence_digest: evidence_digest.clone(),
        entries: ownership,
    };
    let obligation_catalog = Catalog {
        schema_version: CAPABILITY_ADMISSION_SCHEMA.to_string(),
        evidence_digest: evidence_digest.clone(),
        entries: obligations,
    };

    let capability_bytes = canonical_json(&capability_catalog)?;
    let provenance_bytes = canonical_json(&provenance_catalog)?;
    let ownership_bytes = canonical_json(&ownership_catalog)?;
    let obligation_bytes = canonical_json(&obligation_catalog)?;

    let capability_path = foundry_root.join("catalogs/capabilities.json");
    let provenance_path = foundry_root.join("catalogs/provenance.json");
    let ownership_path = foundry_root.join("catalogs/subsystem-ownership.json");
    let obligation_path = foundry_root.join("catalogs/disposition-obligations.json");
    write_replace(&capability_path, &capability_bytes)?;
    write_new(&provenance_path, &provenance_bytes)?;
    write_new(&ownership_path, &ownership_bytes)?;
    write_new(&obligation_path, &obligation_bytes)?;

    let report = CapabilityAdmissionReport {
        schema_version: CAPABILITY_ADMISSION_SCHEMA.to_string(),
        workstream_id: "C".to_string(),
        verifier: VERIFIER_ID.to_string(),
        source_head: source.head.clone(),
        corpus_head: corpus.head.clone(),
        capability_count: 65,
        provenance_count: 65,
        ownership_count: 65,
        obligation_count: 65,
        unknown_capabilities,
        capabilities_without_provenance,
        contradictory_owners,
        disposition_counts,
        subsystem_counts,
        predicates: workstream.predicates.clone(),
        evidence_digest: evidence_digest.clone(),
    };
    let report_path = foundry_root.join("workstreams/C/admission-report.json");
    let report_bytes = canonical_json(&report)?;
    let report_digest = digest_bytes(&report_bytes);
    write_new(&report_path, &report_bytes)?;

    let receipt_relative = "foundry/receipts/workstream-C.json";
    {
        let state_c = state
            .workstreams
            .get_mut("C")
            .context("WORKSTREAM_C_STATE_MISSING")?;
        state_c.status = "ADMITTED".to_string();
        state_c.report_digest = Some(report_digest.clone());
        state_c.receipt_path = Some(receipt_relative.to_string());
    }
    if let Some(state_d) = state.workstreams.get_mut("D") {
        state_d.status = "READY".to_string();
    }
    let state_bytes = canonical_json(&state)?;

    let mut inputs = BTreeMap::new();
    inputs.insert("work-program".to_string(), validation.program_digest);
    inputs.insert("source-tree".to_string(), source.tracked_tree_digest);
    inputs.insert("corpus-tree".to_string(), corpus.tracked_tree_digest);
    inputs.insert("capability-evidence".to_string(), evidence_digest);

    let mut outputs = BTreeMap::new();
    for (relative, bytes) in [
        (
            "foundry/catalogs/capabilities.json",
            capability_bytes.as_slice(),
        ),
        (
            "foundry/catalogs/provenance.json",
            provenance_bytes.as_slice(),
        ),
        (
            "foundry/catalogs/subsystem-ownership.json",
            ownership_bytes.as_slice(),
        ),
        (
            "foundry/catalogs/disposition-obligations.json",
            obligation_bytes.as_slice(),
        ),
        (
            "foundry/workstreams/C/admission-report.json",
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
        subject: "C".to_string(),
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

fn parse_capabilities(bytes: &[u8]) -> Result<Vec<CapabilityRecord>> {
    let text = std::str::from_utf8(bytes).context("CAPABILITY_EVIDENCE_NOT_UTF8")?;
    let mut records = Vec::new();
    let mut subject = String::new();
    let mut properties = BTreeMap::<String, String>::new();

    for raw_line in text.lines() {
        let line = raw_line.trim();
        if line.starts_with("legacy:") && line.contains(" a ggen:LegacyCapability") {
            if !subject.is_empty() {
                records.push(build_record(&subject, &properties)?);
                properties.clear();
            }
            subject = line
                .split_whitespace()
                .next()
                .context("CAPABILITY_SUBJECT_MISSING")?
                .trim_end_matches(';')
                .to_string();
            continue;
        }
        if subject.is_empty() || !line.starts_with("ggen:") {
            continue;
        }
        let mut parts = line.splitn(2, char::is_whitespace);
        let predicate = parts
            .next()
            .context("CAPABILITY_PREDICATE_MISSING")?
            .trim_start_matches("ggen:")
            .to_string();
        let object = parts.next().unwrap_or("").trim();
        let value = if object.starts_with('"') {
            parse_turtle_string(object)?
        } else {
            object
                .trim_end_matches(';')
                .trim_end_matches('.')
                .trim()
                .trim_start_matches("ggen:")
                .to_string()
        };
        properties.insert(predicate, value);
    }
    if !subject.is_empty() {
        records.push(build_record(&subject, &properties)?);
    }
    Ok(records)
}

fn parse_turtle_string(object: &str) -> Result<String> {
    let mut value = String::new();
    let mut escaped = false;
    for character in object.chars().skip(1) {
        if escaped {
            match character {
                'n' => value.push('\n'),
                'r' => value.push('\r'),
                't' => value.push('\t'),
                '"' => value.push('"'),
                '\\' => value.push('\\'),
                other => {
                    value.push('\\');
                    value.push(other);
                }
            }
            escaped = false;
        } else if character == '\\' {
            escaped = true;
        } else if character == '"' {
            return Ok(value);
        } else {
            value.push(character);
        }
    }
    bail!("TURTLE_STRING_UNTERMINATED")
}

fn build_record(subject: &str, properties: &BTreeMap<String, String>) -> Result<CapabilityRecord> {
    let capability_id = required(properties, "capabilityId")?;
    let historical_source_commit = required(properties, "historicalSourceCommit")?;
    let legacy_source_path = required(properties, "legacySourcePath")?;
    let owning_subsystem = required(properties, "owningSubsystem")?;
    let historical_semantic_owner = required(properties, "historicalSemanticOwner")?;
    let disposition = required(properties, "hasDisposition")?;
    let replacement_owner = optional(properties, "replacementOwner");
    let admitted_owner = derive_owner(
        &disposition,
        &replacement_owner,
        &historical_semantic_owner,
        &owning_subsystem,
    )?;

    Ok(CapabilityRecord {
        capability_id,
        subject: subject.to_string(),
        historical_source_commit,
        legacy_source_path,
        owning_subsystem,
        historical_semantic_owner,
        replacement_owner,
        admitted_owner,
        disposition,
        input_contract: optional(properties, "inputContract"),
        output_contract: optional(properties, "outputContract"),
        error_contract: optional(properties, "errorContract"),
        side_effects: optional(properties, "sideEffects"),
        ordering_requirements: optional(properties, "orderingRequirements"),
        default_behavior: optional(properties, "defaultBehavior"),
        configuration_dependencies: optional(properties, "configurationDependencies"),
        evidence_fixtures: optional(properties, "evidenceFixtures"),
        migration_path: optional(properties, "migrationPath"),
        rollback_path: optional(properties, "rollbackPath"),
        archive_path: optional(properties, "archivePath"),
        refusal_code: optional(properties, "refusalCode"),
        refusal_rationale: optional(properties, "refusalRationale"),
    })
}

fn derive_owner(
    disposition: &str, replacement_owner: &str, historical_owner: &str, subsystem: &str,
) -> Result<String> {
    match disposition {
        "REFUSED" | "ARCHIVED" => Ok(format!("ggen-legacy/archive/{subsystem}")),
        "REPLACED" | "SUBSUMED" if !replacement_owner.trim().is_empty() => {
            Ok(replacement_owner.to_string())
        }
        "PRESERVED" if !replacement_owner.trim().is_empty() => Ok(replacement_owner.to_string()),
        "PRESERVED" if !historical_owner.trim().is_empty() => Ok(historical_owner.to_string()),
        "REPLACED" | "SUBSUMED" => {
            bail!("REPLACEMENT_OWNER_MISSING: disposition={disposition}, subsystem={subsystem}")
        }
        other => bail!("UNKNOWN_DISPOSITION: {other}"),
    }
}

fn ownership_basis(capability: &CapabilityRecord) -> String {
    match capability.disposition.as_str() {
        "REFUSED" | "ARCHIVED" => "disposition_requires_corpus_archive".to_string(),
        "REPLACED" | "SUBSUMED" => "declared_replacement_owner".to_string(),
        "PRESERVED" if !capability.replacement_owner.is_empty() => {
            "declared_current_owner".to_string()
        }
        "PRESERVED" => "historical_owner_remains_authoritative".to_string(),
        _ => "invalid".to_string(),
    }
}

fn required(properties: &BTreeMap<String, String>, key: &str) -> Result<String> {
    let value = properties
        .get(key)
        .with_context(|| format!("CAPABILITY_PROPERTY_MISSING: {key}"))?
        .trim()
        .to_string();
    if value.is_empty() {
        bail!("CAPABILITY_PROPERTY_EMPTY: {key}");
    }
    Ok(value)
}

fn optional(properties: &BTreeMap<String, String>, key: &str) -> String {
    properties.get(key).cloned().unwrap_or_default()
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
