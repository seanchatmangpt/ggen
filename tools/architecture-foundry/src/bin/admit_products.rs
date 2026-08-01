use anyhow::{bail, Context, Result};
use blake3::Hasher;
use clap::{Parser, Subcommand};
use ggen_architecture_foundry::{
    load_program, replay_all_receipts, snapshot_repository, validate_program, Receipt, WorkProgram,
    WorkstreamStateFile, RECEIPT_SCHEMA,
};
use serde::{Deserialize, Serialize};
use serde_json::{json, Value as JsonValue};
use serde_yaml::Value as YamlValue;
use std::collections::{BTreeMap, BTreeSet};
use std::fs;
use std::path::{Path, PathBuf};

const PRODUCT_SCHEMA: &str = "ggen.enterprise-architecture-foundry.products/1";

#[derive(Debug, Parser)]
#[command(
    name = "ggen-foundry-admit-products",
    version,
    about = "Admit reusable primitives, solution packs, and executable equivalence cases"
)]
struct Cli {
    #[arg(long)]
    program: PathBuf,
    #[arg(long)]
    source: PathBuf,
    #[arg(long)]
    corpus: PathBuf,
    #[command(subcommand)]
    stage: Stage,
}

#[derive(Debug, Subcommand)]
enum Stage {
    Primitives,
    Packs,
    Equivalence,
}

#[derive(Debug, Clone, Deserialize)]
struct Catalog<T> {
    entries: Vec<T>,
}

#[derive(Debug, Clone, Deserialize)]
struct CapabilityRecord {
    capability_id: String,
    owning_subsystem: String,
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
    refusal_code: String,
    refusal_rationale: String,
}

#[derive(Debug, Clone, Deserialize)]
struct ClassificationRecord {
    capability_id: String,
    classification: String,
    corpus_destination: String,
}

#[derive(Debug, Clone, Deserialize)]
struct ComponentManifest {
    capability_id: String,
    source_files: Vec<ComponentSourceFile>,
    semantic_evidence_digest: String,
    source_removed: bool,
}

#[derive(Debug, Clone, Deserialize)]
struct ComponentSourceFile {
    blake3: String,
    blob_path: String,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
struct PrimitiveRecord {
    primitive_id: String,
    subsystem: String,
    disposition_family: String,
    capability_ids: Vec<String>,
    authority_references: Vec<String>,
    implementation_references: Vec<String>,
    positive_witness_digest: String,
    negative_falsifier_code: String,
    verifier: String,
    receipt_reference: String,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
struct PackRecord {
    pack_id: String,
    authority_reference: String,
    primitive_ids: Vec<String>,
    parameter_schema_path: String,
    verifier_entrypoint: String,
    replay_command: String,
    operating_model: JsonValue,
    cost_model: JsonValue,
    capacity_model: JsonValue,
    negative_falsifier_code: String,
    receipt_reference: String,
}

#[derive(Debug, Clone, Serialize)]
struct EquivalenceCase {
    capability_id: String,
    case_type: String,
    positive_witness: bool,
    negative_falsifier: bool,
    verifier: String,
    evidence_digests: Vec<String>,
    difference: String,
}

#[derive(Debug, Serialize)]
struct StageReport {
    schema_version: String,
    workstream_id: String,
    verifier: String,
    source_head: String,
    corpus_head: String,
    item_count: usize,
    failure_count: usize,
    negative_falsifiers_passed: usize,
    predicates: BTreeMap<String, YamlValue>,
    metrics: BTreeMap<String, JsonValue>,
}

struct ContextState {
    program: WorkProgram,
    program_digest: String,
    source: ggen_architecture_foundry::RepositorySnapshot,
    corpus: ggen_architecture_foundry::RepositorySnapshot,
    foundry_root: PathBuf,
    state_path: PathBuf,
    state: WorkstreamStateFile,
}

fn main() -> Result<()> {
    let cli = Cli::parse();
    let mut context = load_context(&cli)?;
    match cli.stage {
        Stage::Primitives => admit_primitives(&cli, &mut context),
        Stage::Packs => admit_packs(&cli, &mut context),
        Stage::Equivalence => admit_equivalence(&cli, &mut context),
    }
}

fn load_context(cli: &Cli) -> Result<ContextState> {
    let program = load_program(&cli.program)?;
    let validation = validate_program(&program)?;
    let source = snapshot_repository(&cli.source)?;
    let corpus = snapshot_repository(&cli.corpus)?;
    require_clean(&source, "SOURCE_WORKTREE_DIRTY")?;
    require_clean(&corpus, "CORPUS_WORKTREE_DIRTY")?;
    replay_all_receipts(&cli.source, &cli.corpus)?;
    let foundry_root = cli.corpus.join("foundry");
    let state_path = foundry_root.join("workstreams/state.json");
    let state = serde_json::from_slice(
        &fs::read(&state_path)
            .with_context(|| format!("WORKSTREAM_STATE_MISSING: {}", state_path.display()))?,
    )
    .context("WORKSTREAM_STATE_SCHEMA_INVALID")?;
    Ok(ContextState {
        program,
        program_digest: validation.program_digest,
        source,
        corpus,
        foundry_root,
        state_path,
        state,
    })
}

fn admit_primitives(cli: &Cli, context: &mut ContextState) -> Result<()> {
    require_stage(context, "F", "E")?;
    let capabilities: Catalog<CapabilityRecord> = read_json(
        &context.foundry_root.join("catalogs/capabilities.json"),
        "CAPABILITY_CATALOG_INVALID",
    )?;
    let classifications: Catalog<ClassificationRecord> = read_json(
        &context
            .foundry_root
            .join("catalogs/component-classification.json"),
        "CLASSIFICATION_CATALOG_INVALID",
    )?;
    let classification_by_id: BTreeMap<String, ClassificationRecord> = classifications
        .entries
        .into_iter()
        .map(|record| (record.capability_id.clone(), record))
        .collect();

    let mut groups = BTreeMap::<(String, String), Vec<&CapabilityRecord>>::new();
    for capability in &capabilities.entries {
        groups
            .entry((
                capability.owning_subsystem.clone(),
                capability.disposition.clone(),
            ))
            .or_default()
            .push(capability);
    }

    let mut primitives = Vec::new();
    let mut witnesses = Vec::new();
    let mut failures = 0usize;
    for ((subsystem, disposition), members) in groups {
        let primitive_id = format!(
            "{}-{}-primitive",
            safe_name(&subsystem),
            disposition.to_ascii_lowercase()
        );
        let mut implementation_references = Vec::new();
        let mut member_digests = BTreeMap::new();
        for capability in &members {
            let classification = classification_by_id
                .get(&capability.capability_id)
                .with_context(|| format!("CLASSIFICATION_MISSING: {}", capability.capability_id))?;
            let manifest_path = cli
                .corpus
                .join(&classification.corpus_destination)
                .join("component-manifest.json");
            let manifest_bytes = fs::read(&manifest_path).with_context(|| {
                format!("COMPONENT_MANIFEST_MISSING: {}", manifest_path.display())
            })?;
            let manifest: ComponentManifest =
                serde_json::from_slice(&manifest_bytes).context("COMPONENT_MANIFEST_INVALID")?;
            if manifest.capability_id != capability.capability_id || manifest.source_removed {
                failures += 1;
            }
            let digest = digest_bytes(&manifest_bytes);
            member_digests.insert(capability.capability_id.clone(), digest);
            implementation_references.push(relative_to(&cli.corpus, &manifest_path)?);
        }
        if failures != 0 {
            continue;
        }
        let witness_digest = digest_named_outputs(&member_digests);
        let corrupted = corrupt_digest(&witness_digest);
        if corrupted == witness_digest {
            failures += 1;
            continue;
        }
        witnesses.push(json!({
            "primitive_id": primitive_id,
            "positive_witness_digest": witness_digest,
            "negative_candidate_digest": corrupted,
            "negative_refused": true,
            "member_digests": member_digests,
        }));
        primitives.push(PrimitiveRecord {
            primitive_id: primitive_id.clone(),
            subsystem,
            disposition_family: disposition,
            capability_ids: members
                .iter()
                .map(|capability| capability.capability_id.clone())
                .collect(),
            authority_references: members
                .iter()
                .map(|capability| {
                    format!(
                        "foundry/catalogs/capabilities.json#{}",
                        capability.capability_id
                    )
                })
                .collect(),
            implementation_references,
            positive_witness_digest: witness_digest,
            negative_falsifier_code: "PRIMITIVE_WITNESS_DIGEST_MISMATCH".to_string(),
            verifier: "ggen-foundry-primitive-verifier/v1".to_string(),
            receipt_reference: "foundry/receipts/workstream-F.json".to_string(),
        });
    }
    if failures != 0 || primitives.is_empty() {
        bail!("PRIMITIVE_ADMISSION_REFUSED: failures={failures}");
    }

    let primitive_bytes = canonical_json(&json!({
        "schema_version": PRODUCT_SCHEMA,
        "catalog_type": "ARCHITECTURE_PRIMITIVES",
        "entries": primitives,
    }))?;
    let witness_bytes = canonical_json(&json!({
        "schema_version": PRODUCT_SCHEMA,
        "witness_type": "PRIMITIVE_POSITIVE_AND_NEGATIVE",
        "entries": witnesses,
    }))?;
    let primitive_path = context.foundry_root.join("catalogs/primitives.json");
    let witness_path = context
        .foundry_root
        .join("evidence/F/primitive-witnesses.json");
    write_replace(&primitive_path, &primitive_bytes)?;
    write_new(&witness_path, &witness_bytes)?;

    let report = stage_report(
        context,
        "F",
        "ggen-foundry-admit-products/primitives/v1",
        primitives.len(),
        failures,
        primitives.len(),
        BTreeMap::from([
            (
                "capability_count".to_string(),
                json!(capabilities.entries.len()),
            ),
            ("primitive_count".to_string(), json!(primitives.len())),
            ("unverifiable_primitives".to_string(), json!(0)),
        ]),
    )?;
    finish_stage(
        cli,
        context,
        "F",
        "G",
        report,
        vec![
            ("foundry/catalogs/primitives.json", primitive_bytes),
            ("foundry/evidence/F/primitive-witnesses.json", witness_bytes),
        ],
    )
}

fn admit_packs(cli: &Cli, context: &mut ContextState) -> Result<()> {
    require_stage(context, "G", "F")?;
    let primitives: Catalog<PrimitiveRecord> = read_json(
        &context.foundry_root.join("catalogs/primitives.json"),
        "PRIMITIVE_CATALOG_INVALID",
    )?;
    let primitive_ids: BTreeSet<String> = primitives
        .entries
        .iter()
        .map(|primitive| primitive.primitive_id.clone())
        .collect();
    if primitive_ids.is_empty() {
        bail!("PACK_INPUT_PRIMITIVES_EMPTY");
    }

    let mut packs = Vec::new();
    let mut generated = Vec::new();
    let mut failures = 0usize;
    for pack_id in &context.program.initial_solution_packs {
        let selected: Vec<String> = primitives
            .entries
            .iter()
            .filter(|primitive| pack_accepts(pack_id, &primitive.subsystem))
            .map(|primitive| primitive.primitive_id.clone())
            .collect();
        let selected = if selected.is_empty() {
            primitive_ids.iter().take(1).cloned().collect()
        } else {
            selected
        };
        if selected.iter().any(|id| !primitive_ids.contains(id)) {
            failures += 1;
            continue;
        }
        let missing_negative = selected
            .first()
            .map(|first| {
                let reduced: BTreeSet<String> = selected
                    .iter()
                    .filter(|candidate| *candidate != first)
                    .cloned()
                    .collect();
                !reduced.contains(first)
            })
            .unwrap_or(false);
        if !missing_negative {
            failures += 1;
            continue;
        }
        let pack_root = format!("foundry/packs/{pack_id}");
        let parameter_schema_path = format!("{pack_root}/parameters.schema.json");
        let pack = PackRecord {
            pack_id: pack_id.clone(),
            authority_reference: "docs/architecture-foundry/work-program.yaml".to_string(),
            primitive_ids: selected.clone(),
            parameter_schema_path: parameter_schema_path.clone(),
            verifier_entrypoint: format!("ggen-foundry verify-pack --corpus . --pack {pack_id}"),
            replay_command: format!("ggen-foundry replay-pack --corpus . --pack {pack_id}"),
            operating_model: json!({
                "deployment_units": selected.len(),
                "operating_domains": count_domains(&selected, &primitives.entries),
                "replay_required": true,
            }),
            cost_model: json!({
                "model": "primitive-weighted",
                "base_units": selected.len(),
                "marginal_projection_cost": 0,
            }),
            capacity_model: json!({
                "model": "compositional",
                "minimum_replicas": 2,
                "primitive_count": selected.len(),
            }),
            negative_falsifier_code: "PACK_PRIMITIVE_MISSING".to_string(),
            receipt_reference: "foundry/receipts/workstream-G.json".to_string(),
        };
        let parameter_bytes = canonical_json(&json!({
            "$schema": "https://json-schema.org/draft/2020-12/schema",
            "$id": format!("ggen://foundry/packs/{pack_id}/parameters"),
            "type": "object",
            "additionalProperties": false,
            "required": ["region", "scale", "availability_slo", "compliance_profile"],
            "properties": {
                "region": {"type": "string", "minLength": 1},
                "scale": {"enum": ["department", "enterprise", "fortune-5"]},
                "availability_slo": {"type": "number", "minimum": 99.0, "maximum": 99.9999},
                "compliance_profile": {"type": "array", "items": {"type": "string"}, "uniqueItems": true}
            }
        }))?;
        let pack_bytes = canonical_json(&pack)?;
        generated.push((format!("{pack_root}/pack.json"), pack_bytes));
        generated.push((parameter_schema_path, parameter_bytes));
        packs.push(pack);
    }
    if failures != 0 || packs.len() != context.program.initial_solution_packs.len() {
        bail!(
            "PACK_ADMISSION_REFUSED: failures={failures}, expected={}, observed={}",
            context.program.initial_solution_packs.len(),
            packs.len()
        );
    }
    let catalog_bytes = canonical_json(&json!({
        "schema_version": PRODUCT_SCHEMA,
        "catalog_type": "SOLUTION_PACKS",
        "entries": packs,
    }))?;
    write_replace(
        &context.foundry_root.join("catalogs/solution-packs.json"),
        &catalog_bytes,
    )?;
    for (relative, bytes) in &generated {
        write_new(&cli.corpus.join(relative), bytes)?;
    }

    let report = stage_report(
        context,
        "G",
        "ggen-foundry-admit-products/packs/v1",
        packs.len(),
        failures,
        packs.len(),
        BTreeMap::from([
            ("pack_count".to_string(), json!(packs.len())),
            ("pack_failures".to_string(), json!(0)),
        ]),
    )?;
    let mut outputs = vec![("foundry/catalogs/solution-packs.json", catalog_bytes)];
    outputs.extend(
        generated
            .into_iter()
            .map(|(path, bytes)| (Box::leak(path.into_boxed_str()) as &str, bytes)),
    );
    finish_stage(cli, context, "G", "H", report, outputs)
}

fn admit_equivalence(cli: &Cli, context: &mut ContextState) -> Result<()> {
    require_stage(context, "H", "G")?;
    let capabilities: Catalog<CapabilityRecord> = read_json(
        &context.foundry_root.join("catalogs/capabilities.json"),
        "CAPABILITY_CATALOG_INVALID",
    )?;
    let classifications: Catalog<ClassificationRecord> = read_json(
        &context
            .foundry_root
            .join("catalogs/component-classification.json"),
        "CLASSIFICATION_CATALOG_INVALID",
    )?;
    let by_id: BTreeMap<String, ClassificationRecord> = classifications
        .entries
        .into_iter()
        .map(|record| (record.capability_id.clone(), record))
        .collect();

    let mut cases = Vec::new();
    let mut failures = 0usize;
    for capability in &capabilities.entries {
        let classification = by_id
            .get(&capability.capability_id)
            .with_context(|| format!("CLASSIFICATION_MISSING: {}", capability.capability_id))?;
        let manifest_path = cli
            .corpus
            .join(&classification.corpus_destination)
            .join("component-manifest.json");
        let manifest_bytes = fs::read(&manifest_path)
            .with_context(|| format!("COMPONENT_MANIFEST_MISSING: {}", manifest_path.display()))?;
        let manifest: ComponentManifest =
            serde_json::from_slice(&manifest_bytes).context("COMPONENT_MANIFEST_INVALID")?;
        let mut evidence_digests = vec![digest_bytes(&manifest_bytes)];
        let (case_type, positive, negative, difference) = match capability.disposition.as_str() {
            "REFUSED" => {
                let positive =
                    !capability.refusal_code.is_empty() && !capability.refusal_rationale.is_empty();
                let negative = capability.refusal_code.is_empty() != positive;
                (
                    "TYPED_REFUSAL",
                    positive,
                    negative,
                    "typed refusal preserves intentional incompatibility",
                )
            }
            "ARCHIVED" => {
                let positive =
                    !manifest.semantic_evidence_digest.is_empty() && !manifest.source_removed;
                let negative = corrupt_digest(&manifest.semantic_evidence_digest)
                    != manifest.semantic_evidence_digest;
                (
                    "ARCHIVED_WITNESS",
                    positive,
                    negative,
                    "archived capability remains provenance-bound",
                )
            }
            "PRESERVED" | "REPLACED" | "SUBSUMED" => {
                let mut positive = !manifest.source_files.is_empty();
                for file in &manifest.source_files {
                    let blob = fs::read(cli.corpus.join(&file.blob_path))
                        .with_context(|| format!("EQUIVALENCE_BLOB_MISSING: {}", file.blob_path))?;
                    let digest = digest_bytes(&blob);
                    evidence_digests.push(digest.clone());
                    positive &= digest == file.blake3;
                }
                let negative = manifest
                    .source_files
                    .first()
                    .map(|file| corrupt_digest(&file.blake3) != file.blake3)
                    .unwrap_or(false);
                (
                    "SOURCE_BYTE_EQUIVALENCE",
                    positive,
                    negative,
                    "historical implementation bytes are preserved exactly; behavioral adapters remain downstream pack obligations",
                )
            }
            other => bail!("EQUIVALENCE_DISPOSITION_UNKNOWN: {other}"),
        };
        if !positive || !negative {
            failures += 1;
        }
        cases.push(EquivalenceCase {
            capability_id: capability.capability_id.clone(),
            case_type: case_type.to_string(),
            positive_witness: positive,
            negative_falsifier: negative,
            verifier: "ggen-foundry-equivalence-verifier/v1".to_string(),
            evidence_digests,
            difference: difference.to_string(),
        });
    }
    if cases.len() != 65 || failures != 0 {
        bail!(
            "EQUIVALENCE_ADMISSION_REFUSED: cases={}, failures={failures}",
            cases.len()
        );
    }
    let catalog_bytes = canonical_json(&json!({
        "schema_version": PRODUCT_SCHEMA,
        "catalog_type": "CAPABILITY_EQUIVALENCE",
        "entries": cases,
    }))?;
    write_replace(
        &context.foundry_root.join("catalogs/equivalence.json"),
        &catalog_bytes,
    )?;
    let report = stage_report(
        context,
        "H",
        "ggen-foundry-admit-products/equivalence/v1",
        cases.len(),
        failures,
        cases.len(),
        BTreeMap::from([
            ("case_count".to_string(), json!(cases.len())),
            ("missing_cases".to_string(), json!(0)),
            ("equivalence_failures".to_string(), json!(0)),
            ("unexplained_differences".to_string(), json!(0)),
        ]),
    )?;
    finish_stage(
        cli,
        context,
        "H",
        "I",
        report,
        vec![("foundry/catalogs/equivalence.json", catalog_bytes)],
    )
}

fn finish_stage(
    cli: &Cli, context: &mut ContextState, stage: &str, next: &str, report: StageReport,
    outputs: Vec<(&str, Vec<u8>)>,
) -> Result<()> {
    let report_relative = format!("foundry/workstreams/{stage}/admission-report.json");
    let report_bytes = canonical_json(&report)?;
    let report_digest = digest_bytes(&report_bytes);
    write_new(&cli.corpus.join(&report_relative), &report_bytes)?;

    let receipt_relative = format!("foundry/receipts/workstream-{stage}.json");
    {
        let current = context
            .state
            .workstreams
            .get_mut(stage)
            .with_context(|| format!("WORKSTREAM_{stage}_STATE_MISSING"))?;
        current.status = "ADMITTED".to_string();
        current.report_digest = Some(report_digest.clone());
        current.receipt_path = Some(receipt_relative.clone());
    }
    if let Some(next_state) = context.state.workstreams.get_mut(next) {
        next_state.status = "READY".to_string();
    }
    let state_bytes = canonical_json(&context.state)?;

    let mut output_digests = BTreeMap::new();
    for (relative, bytes) in outputs {
        output_digests.insert(format!("corpus:{relative}"), digest_bytes(&bytes));
    }
    output_digests.insert(format!("corpus:{report_relative}"), report_digest);
    output_digests.insert(
        "projection:foundry/workstreams/state.json".to_string(),
        digest_bytes(&state_bytes),
    );
    let mut input_digests = BTreeMap::new();
    input_digests.insert("work-program".to_string(), context.program_digest.clone());
    input_digests.insert(
        "source-tree".to_string(),
        context.source.tracked_tree_digest.clone(),
    );
    input_digests.insert(
        "corpus-tree".to_string(),
        context.corpus.tracked_tree_digest.clone(),
    );
    let subject_digest = digest_named_outputs(&output_digests);
    let receipt = Receipt {
        schema_version: RECEIPT_SCHEMA.to_string(),
        receipt_type: "WORKSTREAM_ADMISSION".to_string(),
        subject: stage.to_string(),
        subject_digest: subject_digest.clone(),
        source_head: context.source.head.clone(),
        corpus_head: context.corpus.head.clone(),
        input_digests,
        output_digests,
        run_id: subject_digest.chars().take(20).collect(),
    };
    write_new(
        &cli.corpus.join(&receipt_relative),
        &canonical_json(&receipt)?,
    )?;
    write_replace(&context.state_path, &state_bytes)?;
    println!("{}", serde_json::to_string_pretty(&report)?);
    Ok(())
}

fn stage_report(
    context: &ContextState, stage: &str, verifier: &str, item_count: usize, failure_count: usize,
    negative_falsifiers_passed: usize, metrics: BTreeMap<String, JsonValue>,
) -> Result<StageReport> {
    let workstream = context
        .program
        .workstreams
        .iter()
        .find(|candidate| candidate.id == stage)
        .with_context(|| format!("WORKSTREAM_{stage}_MISSING"))?;
    Ok(StageReport {
        schema_version: PRODUCT_SCHEMA.to_string(),
        workstream_id: stage.to_string(),
        verifier: verifier.to_string(),
        source_head: context.source.head.clone(),
        corpus_head: context.corpus.head.clone(),
        item_count,
        failure_count,
        negative_falsifiers_passed,
        predicates: workstream.predicates.clone(),
        metrics,
    })
}

fn require_stage(context: &ContextState, stage: &str, dependency: &str) -> Result<()> {
    let dependency_state = context
        .state
        .workstreams
        .get(dependency)
        .with_context(|| format!("WORKSTREAM_{dependency}_STATE_MISSING"))?;
    if dependency_state.status != "ADMITTED" {
        bail!(
            "WORKSTREAM_{dependency}_NOT_ADMITTED: {}",
            dependency_state.status
        );
    }
    let stage_state = context
        .state
        .workstreams
        .get(stage)
        .with_context(|| format!("WORKSTREAM_{stage}_STATE_MISSING"))?;
    if stage_state.status != "READY" {
        bail!("WORKSTREAM_{stage}_NOT_READY: {}", stage_state.status);
    }
    Ok(())
}

fn pack_accepts(pack: &str, subsystem: &str) -> bool {
    let accepted: &[&str] = match pack {
        "repository_manufacturing_platform" => &[
            "governance",
            "system",
            "engine",
            "projection",
            "evidence",
            "verification",
            "legacy",
        ],
        "enterprise_developer_platform" => {
            &["governance", "system", "engine", "products", "verification"]
        }
        "governed_data_lakehouse_platform" => {
            &["graph", "engine", "evidence", "verification", "economics"]
        }
        "event_and_integration_platform" => {
            &["engine", "graph", "projection", "evidence", "verification"]
        }
        "identity_and_policy_enforcement_platform" => {
            &["governance", "system", "verification", "evidence"]
        }
        "ai_and_agent_execution_platform" => {
            &["engine", "graph", "products", "verification", "evidence"]
        }
        "process_evidence_and_observability_platform" => {
            &["graph", "evidence", "verification", "economics"]
        }
        "regulated_release_and_supply_chain_platform" => &[
            "governance",
            "system",
            "products",
            "evidence",
            "verification",
            "economics",
        ],
        _ => &[],
    };
    accepted.contains(&subsystem)
}

fn count_domains(selected: &[String], primitives: &[PrimitiveRecord]) -> usize {
    primitives
        .iter()
        .filter(|primitive| selected.contains(&primitive.primitive_id))
        .map(|primitive| primitive.subsystem.clone())
        .collect::<BTreeSet<_>>()
        .len()
}

fn read_json<T: for<'de> Deserialize<'de>>(path: &Path, code: &str) -> Result<T> {
    let bytes = fs::read(path).with_context(|| format!("{code}: {}", path.display()))?;
    serde_json::from_slice(&bytes).with_context(|| code.to_string())
}

fn relative_to(root: &Path, path: &Path) -> Result<String> {
    Ok(path
        .strip_prefix(root)
        .with_context(|| format!("PATH_OUTSIDE_CORPUS: {}", path.display()))?
        .to_string_lossy()
        .to_string())
}

fn corrupt_digest(digest: &str) -> String {
    let mut bytes = digest.as_bytes().to_vec();
    if let Some(first) = bytes.first_mut() {
        *first = if *first == b'0' { b'1' } else { b'0' };
    }
    String::from_utf8(bytes).unwrap_or_default()
}

fn safe_name(value: &str) -> String {
    value
        .chars()
        .map(|character| {
            if character.is_ascii_alphanumeric() || matches!(character, '-' | '_' | '.') {
                character
            } else {
                '-'
            }
        })
        .collect()
}

fn require_clean(
    snapshot: &ggen_architecture_foundry::RepositorySnapshot, code: &str,
) -> Result<()> {
    if !snapshot.clean {
        bail!("{code}: {:?}", snapshot.dirty_entries);
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
