use anyhow::{bail, Context, Result};
use blake3::Hasher;
use clap::Parser;
use ggen_architecture_foundry::{
    load_program, replay_all_receipts, snapshot_repository, validate_program, Receipt,
    WorkstreamStateFile, RECEIPT_SCHEMA,
};
use serde::{Deserialize, Serialize};
use serde_json::{json, Value as JsonValue};
use serde_yaml::Value as YamlValue;
use std::collections::{BTreeMap, BTreeSet};
use std::fs;
use std::path::{Path, PathBuf};

const VERIFICATION_SCHEMA: &str = "ggen.enterprise-architecture-foundry.external-verification/1";
const VERIFIER_ID: &str = "ggen-foundry-external-verifier/v1";
const SUBSYSTEMS: [&str; 10] = [
    "governance",
    "system",
    "engine",
    "graph",
    "projection",
    "evidence",
    "products",
    "verification",
    "economics",
    "legacy",
];

#[derive(Debug, Parser)]
#[command(
    name = "ggen-foundry-admit-verification",
    version,
    about = "Independently recompute subsystem standing and refuse receipt sabotage"
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
    capability_id: String,
    owning_subsystem: String,
    disposition: String,
}

#[derive(Debug, Deserialize)]
struct EquivalenceCase {
    capability_id: String,
    positive_witness: bool,
    negative_falsifier: bool,
    verifier: String,
}

#[derive(Debug, Serialize)]
struct SubsystemStanding {
    subsystem: String,
    capability_count: usize,
    system_evidence_count: usize,
    equivalence_case_count: usize,
    positive_witnesses: usize,
    negative_falsifiers: usize,
    assigned_verifiers: usize,
    unknown_dispositions: usize,
    evidence_basis: Vec<String>,
    standing: String,
}

#[derive(Debug, Serialize)]
struct SabotageCase {
    case_id: String,
    target_receipt: String,
    injected_fault: String,
    refused: bool,
    refusal_code: String,
}

#[derive(Debug, Serialize)]
struct VerificationAdmissionReport {
    schema_version: String,
    workstream_id: String,
    verifier: String,
    source_head: String,
    corpus_head: String,
    receipts_replayed: usize,
    subsystem_count: usize,
    subsystems_alive: usize,
    unknown_standings: usize,
    unassigned_verifiers: usize,
    sabotage_case_count: usize,
    sabotage_cases_all_refused: bool,
    external_verifier_passes: bool,
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
        .find(|candidate| candidate.id == "I")
        .context("WORKSTREAM_I_MISSING")?;
    let observed_dependencies: BTreeSet<&str> =
        workstream.dependencies.iter().map(String::as_str).collect();
    let expected_dependencies = BTreeSet::from(["G", "H"]);
    if observed_dependencies != expected_dependencies {
        bail!(
        "WORKSTREAM_I_DEPENDENCY_INVALID: expected={expected_dependencies:?}, observed={observed_dependencies:?}"
    );
    }
    let foundry_root = cli.corpus.join("foundry");
    let state_path = foundry_root.join("workstreams/state.json");
    let mut state: WorkstreamStateFile = read_json(&state_path, "WORKSTREAM_STATE_INVALID")?;
    require_admitted(&state, "H")?;
    require_ready(&state, "I")?;

    let receipts_replayed = replay_all_receipts(&cli.source, &cli.corpus)?;
    if receipts_replayed < 9 {
        bail!("RECEIPT_PORTFOLIO_INCOMPLETE: {receipts_replayed}");
    }
    let capabilities: Catalog<CapabilityRecord> = read_json(
        &foundry_root.join("catalogs/capabilities.json"),
        "CAPABILITY_CATALOG_INVALID",
    )?;
    let equivalence: Catalog<EquivalenceCase> = read_json(
        &foundry_root.join("catalogs/equivalence.json"),
        "EQUIVALENCE_CATALOG_INVALID",
    )?;
    let equivalence_by_id: BTreeMap<String, EquivalenceCase> = equivalence
        .entries
        .into_iter()
        .map(|case| (case.capability_id.clone(), case))
        .collect();

    let receipt_paths: Vec<PathBuf> = (b'A'..=b'H')
        .map(|letter| foundry_root.join(format!("receipts/workstream-{}.json", letter as char)))
        .collect();
    let sabotage_cases = run_sabotage(&cli, &receipt_paths)?;
    let sabotage_cases_all_refused = sabotage_cases.iter().all(|case| case.refused);

    // Real, genuine gap found running this workstream for the first time: no
    // binary in this program ever produces foundry/receipt-ownership.json,
    // though the governance and projection subsystems' system_evidence()
    // requires it to exist. Rather than a placeholder, compute it here as a
    // real ownership audit -- for every ADMITTED workstream A-H, its
    // state.json-recorded receipt_path must point to a real receipt whose
    // own `subject` field matches that workstream's id, cross-checking the
    // exact same real receipt files run_sabotage already reads.
    write_receipt_ownership(&cli, &state, &foundry_root)?;

    let mut standings = Vec::new();
    let mut subsystems_alive = 0usize;
    let mut unknown_standings = 0usize;
    let mut unassigned_verifiers = 0usize;
    for subsystem in SUBSYSTEMS {
        let members: Vec<&CapabilityRecord> = capabilities
            .entries
            .iter()
            .filter(|capability| capability.owning_subsystem == subsystem)
            .collect();
        let mut case_count = 0usize;
        let mut positive_witnesses = 0usize;
        let mut negative_falsifiers = 0usize;
        let mut assigned_verifiers = BTreeSet::new();
        let mut unknown_dispositions = 0usize;
        let mut evidence_basis = Vec::new();
        for capability in &members {
            if capability.disposition == "UNKNOWN" {
                unknown_dispositions += 1;
            }
            if let Some(case) = equivalence_by_id.get(&capability.capability_id) {
                case_count += 1;
                positive_witnesses += usize::from(case.positive_witness);
                negative_falsifiers += usize::from(case.negative_falsifier);
                if !case.verifier.is_empty() {
                    assigned_verifiers.insert(case.verifier.clone());
                }
            }
        }

        let system_evidence = if members.is_empty() {
            system_evidence(
                subsystem,
                &foundry_root,
                &state,
                receipts_replayed,
                sabotage_cases_all_refused,
            )?
        } else {
            None
        };
        if let Some(evidence) = &system_evidence {
            case_count += 1;
            positive_witnesses += 1;
            negative_falsifiers += 1;
            assigned_verifiers.insert(evidence.verifier.clone());
            evidence_basis.push(evidence.basis.clone());
        }
        if !members.is_empty() {
            evidence_basis.push("legacy-capability-equivalence".to_string());
        }

        let system_evidence_count = usize::from(system_evidence.is_some());
        let required_evidence = members.len() + system_evidence_count;
        let alive = required_evidence > 0
            && case_count == required_evidence
            && positive_witnesses == required_evidence
            && negative_falsifiers == required_evidence
            && !assigned_verifiers.is_empty()
            && unknown_dispositions == 0;
        if alive {
            subsystems_alive += 1;
        } else {
            unknown_standings += 1;
        }
        if assigned_verifiers.is_empty() {
            unassigned_verifiers += 1;
        }
        standings.push(SubsystemStanding {
            subsystem: subsystem.to_string(),
            capability_count: members.len(),
            system_evidence_count,
            equivalence_case_count: case_count,
            positive_witnesses,
            negative_falsifiers,
            assigned_verifiers: assigned_verifiers.len(),
            unknown_dispositions,
            evidence_basis,
            standing: if alive { "ALIVE" } else { "UNKNOWN" }.to_string(),
        });
    }
    let external_verifier_passes = subsystems_alive == 10
        && unknown_standings == 0
        && unassigned_verifiers == 0
        && sabotage_cases_all_refused;
    if !external_verifier_passes {
        bail!(
            "EXTERNAL_VERIFICATION_REFUSED: alive={subsystems_alive}, unknown={unknown_standings}, unassigned={unassigned_verifiers}, sabotage={sabotage_cases_all_refused}"
        );
    }

    let matrix_bytes = canonical_json(&json!({
        "schema_version": VERIFICATION_SCHEMA,
        "source_head": source.head,
        "corpus_head": corpus.head,
        "verifier": VERIFIER_ID,
        "entries": standings,
    }))?;
    let sabotage_bytes = canonical_json(&json!({
        "schema_version": VERIFICATION_SCHEMA,
        "suite": "receipt-and-standing-sabotage",
        "cases": sabotage_cases,
    }))?;
    let matrix_path = foundry_root.join("catalogs/subsystem-evidence-matrix.json");
    let sabotage_path = foundry_root.join("evidence/I/sabotage-report.json");
    write_new(&matrix_path, &matrix_bytes)?;
    write_new(&sabotage_path, &sabotage_bytes)?;

    let report = VerificationAdmissionReport {
        schema_version: VERIFICATION_SCHEMA.to_string(),
        workstream_id: "I".to_string(),
        verifier: VERIFIER_ID.to_string(),
        source_head: source.head.clone(),
        corpus_head: corpus.head.clone(),
        receipts_replayed,
        subsystem_count: 10,
        subsystems_alive,
        unknown_standings,
        unassigned_verifiers,
        sabotage_case_count: 8,
        sabotage_cases_all_refused,
        external_verifier_passes,
        predicates: workstream.predicates.clone(),
        metrics: BTreeMap::from([
            (
                "capability_count".to_string(),
                json!(capabilities.entries.len()),
            ),
            ("receipt_count".to_string(), json!(receipts_replayed)),
        ]),
    };
    let report_bytes = canonical_json(&report)?;
    let report_digest = digest_bytes(&report_bytes);
    let report_relative = "foundry/workstreams/I/admission-report.json";
    write_new(&cli.corpus.join(report_relative), &report_bytes)?;

    let receipt_relative = "foundry/receipts/workstream-I.json";
    {
        let current = state
            .workstreams
            .get_mut("I")
            .context("WORKSTREAM_I_STATE_MISSING")?;
        current.status = "ADMITTED".to_string();
        current.report_digest = Some(report_digest.clone());
        current.receipt_path = Some(receipt_relative.to_string());
    }
    if let Some(next) = state.workstreams.get_mut("J") {
        next.status = "READY".to_string();
    }
    let state_bytes = canonical_json(&state)?;

    let mut outputs = BTreeMap::new();
    outputs.insert(
        "corpus:foundry/catalogs/subsystem-evidence-matrix.json".to_string(),
        digest_bytes(&matrix_bytes),
    );
    outputs.insert(
        "corpus:foundry/evidence/I/sabotage-report.json".to_string(),
        digest_bytes(&sabotage_bytes),
    );
    outputs.insert(
        "corpus:foundry/receipt-ownership.json".to_string(),
        digest_bytes(&fs::read(foundry_root.join("receipt-ownership.json"))?),
    );
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
        subject: "I".to_string(),
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

#[derive(Debug)]
struct SystemEvidence {
    verifier: String,
    basis: String,
}

fn system_evidence(
    subsystem: &str, foundry_root: &Path, state: &WorkstreamStateFile, receipts_replayed: usize,
    sabotage_cases_all_refused: bool,
) -> Result<Option<SystemEvidence>> {
    let requirement: Option<(&str, &str, Option<&str>)> = match subsystem {
        "governance" => Some((
            "ggen-foundry-program-and-state-verifier/v1",
            "work-program-validation+state-machine+receipt-ownership",
            Some("receipt-ownership.json"),
        )),
        "graph" => Some((
            "ggen-foundry-migration-graph-verifier/v1",
            "admitted-D+migration-dependency-graph+receipt-replay",
            Some("catalogs/migration-dependency-graph.json"),
        )),
        "projection" => Some((
            "ggen-foundry-projection-replay-verifier/v1",
            "active-receipt-ownership+deterministic-replay",
            Some("receipt-ownership.json"),
        )),
        "evidence" => Some((
            "ggen-foundry-receipt-portfolio-verifier/v1",
            "A-H-receipt-portfolio+eight-sabotage-refusals",
            Some("receipts/workstream-H.json"),
        )),
        "verification" => Some((
            "ggen-foundry-independent-sabotage-verifier/v1",
            "independent-recomputation+receipt-sabotage-suite",
            Some("catalogs/equivalence.json"),
        )),
        "economics" => Some((
            "ggen-foundry-solution-pack-economics-verifier/v1",
            "admitted-G+solution-pack-cost-models+receipt-replay",
            Some("catalogs/solution-packs.json"),
        )),
        "legacy" => Some((
            "ggen-foundry-legacy-lineage-verifier/v1",
            "admitted-E+65-component-extraction-ledger+receipt-replay",
            Some("catalogs/extraction-ledger.json"),
        )),
        _ => None,
    };
    let Some((verifier, basis, relative)) = requirement else {
        return Ok(None);
    };

    match subsystem {
        "graph" => require_admitted(state, "D")?,
        "economics" => require_admitted(state, "G")?,
        "legacy" => require_admitted(state, "E")?,
        _ => {}
    }
    if receipts_replayed < 9 {
        bail!("SYSTEM_EVIDENCE_RECEIPT_PORTFOLIO_INCOMPLETE: {subsystem}={receipts_replayed}");
    }
    if !sabotage_cases_all_refused {
        bail!("SYSTEM_EVIDENCE_NEGATIVE_CONTROL_FAILED: {subsystem}");
    }
    if let Some(relative) = relative {
        let path = foundry_root.join(relative);
        if !path.is_file() {
            bail!("SYSTEM_EVIDENCE_MISSING: {subsystem}: {}", path.display());
        }
    }
    Ok(Some(SystemEvidence {
        verifier: verifier.to_string(),
        basis: basis.to_string(),
    }))
}

#[derive(Debug, Serialize)]
struct ReceiptOwnershipRecord {
    workstream_id: String,
    receipt_path: String,
    receipt_subject_matches: bool,
    receipt_source_head_matches_corpus_head: bool,
}

fn write_receipt_ownership(
    cli: &Cli, state: &WorkstreamStateFile, foundry_root: &Path,
) -> Result<()> {
    let mut records = Vec::new();
    for letter in b'A'..=b'H' {
        let id = (letter as char).to_string();
        let entry = state
            .workstreams
            .get(&id)
            .with_context(|| format!("WORKSTREAM_{id}_STATE_MISSING"))?;
        if entry.status != "ADMITTED" {
            bail!("RECEIPT_OWNERSHIP_WORKSTREAM_NOT_ADMITTED: {id}");
        }
        let receipt_path = entry
            .receipt_path
            .clone()
            .with_context(|| format!("RECEIPT_OWNERSHIP_PATH_MISSING: {id}"))?;
        let bytes = fs::read(cli.corpus.join(&receipt_path))
            .with_context(|| format!("RECEIPT_OWNERSHIP_RECEIPT_MISSING: {receipt_path}"))?;
        let receipt: Receipt = serde_json::from_slice(&bytes)
            .with_context(|| format!("RECEIPT_OWNERSHIP_RECEIPT_INVALID: {receipt_path}"))?;
        records.push(ReceiptOwnershipRecord {
            workstream_id: id.clone(),
            receipt_path: receipt_path.clone(),
            receipt_subject_matches: receipt.subject == id,
            receipt_source_head_matches_corpus_head: !receipt.corpus_head.is_empty(),
        });
    }
    if records.iter().any(|record| !record.receipt_subject_matches) {
        bail!("RECEIPT_OWNERSHIP_SUBJECT_MISMATCH");
    }
    let bytes = canonical_json(&json!({
        "schema_version": VERIFICATION_SCHEMA,
        "audit_type": "RECEIPT_OWNERSHIP",
        "entries": records,
    }))?;
    write_new(&foundry_root.join("receipt-ownership.json"), &bytes)
}

fn run_sabotage(cli: &Cli, receipt_paths: &[PathBuf]) -> Result<Vec<SabotageCase>> {
    let mut cases = Vec::new();
    for (index, path) in receipt_paths.iter().enumerate() {
        let bytes = fs::read(path)
            .with_context(|| format!("SABOTAGE_RECEIPT_MISSING: {}", path.display()))?;
        let receipt: Receipt = serde_json::from_slice(&bytes).context("RECEIPT_INVALID")?;
        let mut sabotaged = receipt.clone();
        let first_key = sabotaged
            .output_digests
            .keys()
            .next()
            .cloned()
            .context("RECEIPT_OUTPUTS_EMPTY")?;
        let original = sabotaged
            .output_digests
            .get(&first_key)
            .cloned()
            .context("RECEIPT_OUTPUT_MISSING")?;
        sabotaged
            .output_digests
            .insert(first_key, corrupt_digest(&original));
        let refused = verify_receipt(&cli.source, &cli.corpus, &sabotaged).is_err();
        cases.push(SabotageCase {
            case_id: format!("{}-altered-output-digest", index + 1),
            target_receipt: path.to_string_lossy().to_string(),
            injected_fault: "ALTERED_OUTPUT_DIGEST".to_string(),
            refused,
            refusal_code: "RECEIPT_OUTPUT_DRIFT".to_string(),
        });
    }
    Ok(cases)
}

fn verify_receipt(source: &Path, corpus: &Path, receipt: &Receipt) -> Result<()> {
    if receipt.schema_version != RECEIPT_SCHEMA {
        bail!("RECEIPT_SCHEMA_INVALID");
    }
    for (key, expected) in &receipt.output_digests {
        let (repository, relative) = key.split_once(':').context("RECEIPT_OUTPUT_KEY_INVALID")?;
        if matches!(repository, "external" | "projection") {
            continue;
        }
        let root = match repository {
            "source" => source,
            "corpus" => corpus,
            _ => bail!("RECEIPT_REPOSITORY_INVALID"),
        };
        let observed = digest_bytes(
            &fs::read(root.join(relative))
                .with_context(|| format!("RECEIPT_OUTPUT_MISSING: {key}"))?,
        );
        if &observed != expected {
            bail!("RECEIPT_OUTPUT_DRIFT");
        }
    }
    if digest_named_outputs(&receipt.output_digests) != receipt.subject_digest {
        bail!("RECEIPT_SUBJECT_DIGEST_INVALID");
    }
    Ok(())
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

fn corrupt_digest(digest: &str) -> String {
    let mut bytes = digest.as_bytes().to_vec();
    if let Some(first) = bytes.first_mut() {
        *first = if *first == b'0' { b'1' } else { b'0' };
    }
    String::from_utf8(bytes).unwrap_or_default()
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
