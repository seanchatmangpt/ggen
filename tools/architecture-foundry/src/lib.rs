use blake3::Hasher;
use serde::{Deserialize, Serialize};
use serde_yaml::Value as YamlValue;
use std::collections::{BTreeMap, BTreeSet};
use std::fs;
use std::path::{Component, Path, PathBuf};
use std::process::Command;
use thiserror::Error;
use walkdir::WalkDir;

pub const WORK_PROGRAM_SCHEMA: &str = "ggen.enterprise-architecture-foundry.work-program/1";
pub const BASELINE_SCHEMA: &str = "ggen.enterprise-architecture-foundry.baseline/1";
pub const CORPUS_SCHEMA: &str = "ggen.enterprise-architecture-foundry.corpus/1";
pub const RECEIPT_SCHEMA: &str = "ggen.enterprise-architecture-foundry.receipt/1";
pub const MIGRATION_SCHEMA: &str = "ggen.enterprise-architecture-foundry.migration/1";
pub const LINEAGE_SCHEMA: &str = "ggen.enterprise-architecture-foundry.lineage/1";
pub const WORKSTREAM_REPORT_SCHEMA: &str =
    "ggen.enterprise-architecture-foundry.workstream-report/1";
pub const FINAL_EVIDENCE_SCHEMA: &str = "ggen.enterprise-architecture-foundry.final-evidence/1";

const REQUIRED_WORKSTREAM_IDS: [&str; 11] = ["A", "B", "C", "D", "E", "F", "G", "H", "I", "J", "K"];
const REQUIRED_INVARIANTS: [&str; 9] = [
    "ZERO_UNRECEIPTED_ACTUATION",
    "NO_SELF_CERTIFICATION",
    "EXACT_HEAD_EVIDENCE",
    "GENERATED_PROJECTIONS_ARE_NOT_AUTHORITY",
    "NO_UNKNOWN_CAPABILITY_AT_FINAL_ADMISSION",
    "NO_UNKNOWN_DISPOSITION_AT_FINAL_ADMISSION",
    "NO_SOURCE_REMOVAL_BEFORE_DESTINATION_ADMISSION",
    "CLEAN_ROOM_REPLAY_REQUIRED",
    "AGENT_COMPLETION_DOES_NOT_PROMOTE_STANDING",
];

#[derive(Debug, Error)]
pub enum FoundryError {
    #[error("I/O failure at {path}: {source}")]
    Io {
        path: String,
        #[source]
        source: std::io::Error,
    },
    #[error("YAML decode failure: {0}")]
    Yaml(#[from] serde_yaml::Error),
    #[error("JSON failure: {0}")]
    Json(#[from] serde_json::Error),
    #[error("git command failed in {repo}: git {args}: {stderr}")]
    Git {
        repo: String,
        args: String,
        stderr: String,
    },
    #[error("program refusal {code}: {message}")]
    Refusal { code: String, message: String },
}

pub type Result<T> = std::result::Result<T, FoundryError>;

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct WorkProgram {
    pub schema_version: String,
    pub program_id: String,
    pub program_name: String,
    pub source_repository: String,
    pub corpus_repository: String,
    pub manufacturing_kernel: String,
    pub status: String,
    #[serde(default)]
    pub constitutional_inputs: Vec<String>,
    #[serde(default)]
    pub invariants: Vec<String>,
    pub repositories: BTreeMap<String, RepositoryDefinition>,
    pub workstreams: Vec<Workstream>,
    #[serde(default)]
    pub initial_solution_packs: Vec<String>,
    pub final_predicates: BTreeMap<String, YamlValue>,
    #[serde(default)]
    pub hard_block_classes: Vec<String>,
    #[serde(default)]
    pub non_blocking_conditions: Vec<String>,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct RepositoryDefinition {
    pub role: String,
    #[serde(default)]
    pub owns: Vec<String>,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct Workstream {
    pub id: String,
    pub name: String,
    pub role: String,
    #[serde(default)]
    pub dependencies: Vec<String>,
    #[serde(default)]
    pub partition_key: Option<String>,
    #[serde(default)]
    pub outputs: Vec<String>,
    pub predicates: BTreeMap<String, YamlValue>,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct ProgramValidationReport {
    pub schema_version: String,
    pub program_id: String,
    pub program_digest: String,
    pub workstream_order: Vec<String>,
    pub invariant_count: usize,
    pub solution_pack_count: usize,
    pub valid: bool,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct RepositorySnapshot {
    pub path: String,
    pub head: String,
    pub branch: String,
    pub origin: Option<String>,
    pub clean: bool,
    pub dirty_entries: Vec<String>,
    pub tracked_file_count: usize,
    pub tracked_tree_digest: String,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct BaselineManifest {
    pub schema_version: String,
    pub program_id: String,
    pub program_digest: String,
    pub source: RepositorySnapshot,
    pub corpus: RepositorySnapshot,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct Receipt {
    pub schema_version: String,
    pub receipt_type: String,
    pub subject: String,
    pub subject_digest: String,
    pub source_head: String,
    pub corpus_head: String,
    pub input_digests: BTreeMap<String, String>,
    pub output_digests: BTreeMap<String, String>,
    pub run_id: String,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct FoundryManifest {
    pub schema_version: String,
    pub program_id: String,
    pub program_digest: String,
    pub source_repository: String,
    pub corpus_repository: String,
    pub source_head: String,
    pub corpus_parent_head: String,
    pub source_tree_digest: String,
    pub corpus_parent_tree_digest: String,
    pub initial_solution_packs: Vec<String>,
    pub catalog_paths: Vec<String>,
    pub workstream_ids: Vec<String>,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct Catalog<T> {
    pub schema_version: String,
    pub catalog_type: String,
    pub entries: Vec<T>,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct SolutionPackRecord {
    pub id: String,
    pub standing: String,
    pub required_workstreams: Vec<String>,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct WorkstreamStateFile {
    pub schema_version: String,
    pub program_id: String,
    pub workstreams: BTreeMap<String, WorkstreamState>,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct WorkstreamState {
    pub status: String,
    pub dependencies: Vec<String>,
    pub report_digest: Option<String>,
    pub receipt_path: Option<String>,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct StandingRecord {
    pub schema_version: String,
    pub program_id: String,
    pub standing: String,
    pub admitted: bool,
    pub reasons: Vec<String>,
    pub source_head: String,
    pub corpus_head: String,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct InitializationReport {
    pub manifest_path: String,
    pub receipt_path: String,
    pub generated_file_count: usize,
    pub generated_tree_digest: String,
    pub source_head: String,
    pub corpus_parent_head: String,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct MigrationManifest {
    pub schema_version: String,
    pub batch_id: String,
    pub source_head: String,
    pub corpus_parent_head: String,
    pub components: Vec<ComponentMigration>,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct ComponentMigration {
    pub id: String,
    pub source_path: String,
    pub destination_path: String,
    pub disposition: String,
    #[serde(default)]
    pub capability_ids: Vec<String>,
    pub replacement_owner: String,
    pub rationale: String,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct LineageRecord {
    pub schema_version: String,
    pub batch_id: String,
    pub component_id: String,
    pub source_repository: String,
    pub corpus_repository: String,
    pub source_head: String,
    pub corpus_parent_head: String,
    pub source_path: String,
    pub destination_path: String,
    pub content_digest: String,
    pub disposition: String,
    pub capability_ids: Vec<String>,
    pub replacement_owner: String,
    pub rationale: String,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
struct HistoricalExtractionLineageRecord {
    schema_version: String,
    capability_id: String,
    source_repository: String,
    corpus_repository: String,
    source_head: String,
    corpus_parent_head: String,
    historical_commit: String,
    #[serde(default)]
    historical_commits: Vec<String>,
    source_path: String,
    destination_path: String,
    manifest_digest: String,
    blob_digests: Vec<String>,
    disposition: String,
    classification: String,
    source_removed: bool,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
struct HistoricalComponentManifest {
    schema_version: String,
    capability_id: String,
    source_repository: String,
    corpus_repository: String,
    source_head: String,
    corpus_parent_head: String,
    historical_commit: String,
    #[serde(default)]
    historical_commits: Vec<String>,
    requested_source_path: String,
    normalized_source_path: String,
    disposition: String,
    classification: String,
    kernel_owner: String,
    corpus_destination: String,
    resolution: String,
    source_files: Vec<HistoricalComponentSourceFile>,
    semantic_evidence_path: String,
    semantic_evidence_digest: String,
    source_removed: bool,
    recovery_command: String,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
struct HistoricalComponentSourceFile {
    git_path: String,
    #[serde(default)]
    historical_commit: String,
    git_object_id: String,
    git_mode: String,
    byte_length: usize,
    blake3: String,
    blob_path: String,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct ExtractionReport {
    pub batch_id: String,
    pub component_count: usize,
    pub source_head: String,
    pub corpus_parent_head: String,
    pub batch_digest: String,
    pub receipt_path: String,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct EvidenceFile {
    pub repository: String,
    pub path: String,
    pub blake3: String,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct WorkstreamReport {
    pub schema_version: String,
    pub workstream_id: String,
    pub source_head: String,
    pub corpus_head: String,
    pub verifier: String,
    pub outputs: Vec<EvidenceFile>,
    pub predicates: BTreeMap<String, YamlValue>,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct WorkstreamAdmission {
    pub workstream_id: String,
    pub report_digest: String,
    pub receipt_path: String,
    pub status: String,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct FinalEvidenceReport {
    pub schema_version: String,
    pub source_head: String,
    pub corpus_head: String,
    pub verifier: String,
    pub predicates: BTreeMap<String, YamlValue>,
    pub evidence: Vec<EvidenceFile>,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct VerificationReport {
    pub program_valid: bool,
    pub source_head: String,
    pub corpus_head: String,
    pub manifest_valid: bool,
    pub lineage_records_checked: usize,
    pub invalid_lineage_records: Vec<String>,
    pub receipts_checked: usize,
    pub invalid_receipts: Vec<String>,
    pub admitted_workstreams: Vec<String>,
    pub standing: String,
    pub admitted: bool,
}

pub fn load_program(path: &Path) -> Result<WorkProgram> {
    let bytes = read(path)?;
    Ok(serde_yaml::from_slice(&bytes)?)
}

pub fn validate_program(program: &WorkProgram) -> Result<ProgramValidationReport> {
    if program.schema_version != WORK_PROGRAM_SCHEMA {
        return refusal(
            "WORK_PROGRAM_SCHEMA_INVALID",
            format!(
                "expected {WORK_PROGRAM_SCHEMA}, observed {}",
                program.schema_version
            ),
        );
    }
    if program.program_id.trim().is_empty() {
        return refusal("PROGRAM_ID_MISSING", "program_id is empty");
    }

    let observed_invariants: BTreeSet<&str> =
        program.invariants.iter().map(String::as_str).collect();
    for required in REQUIRED_INVARIANTS {
        if !observed_invariants.contains(required) {
            return refusal(
                "REQUIRED_INVARIANT_MISSING",
                format!("required invariant {required} is absent"),
            );
        }
    }

    require_repository_role(program, "ggen", "REPOSITORY_MANUFACTURING_KERNEL")?;
    require_repository_role(
        program,
        "ggen_legacy",
        "ENTERPRISE_ARCHITECTURE_FOUNDRY_CORPUS",
    )?;
    require_repository_role(
        program,
        "manufactured_repositories",
        "ADMITTED_SOLUTION_INSTANCES",
    )?;

    let mut by_id = BTreeMap::new();
    for workstream in &program.workstreams {
        if workstream.id.trim().is_empty()
            || workstream.name.trim().is_empty()
            || workstream.role.trim().is_empty()
        {
            return refusal(
                "WORKSTREAM_IDENTITY_INCOMPLETE",
                format!("workstream {:?} is missing identity fields", workstream.id),
            );
        }
        if workstream.outputs.is_empty() {
            return refusal(
                "WORKSTREAM_OUTPUTS_MISSING",
                format!("workstream {} has no declared outputs", workstream.id),
            );
        }
        if workstream.predicates.is_empty() {
            return refusal(
                "WORKSTREAM_PREDICATES_MISSING",
                format!("workstream {} has no promotion predicates", workstream.id),
            );
        }
        if by_id.insert(workstream.id.clone(), workstream).is_some() {
            return refusal(
                "WORKSTREAM_DUPLICATED",
                format!("workstream {} is duplicated", workstream.id),
            );
        }
    }

    let expected: BTreeSet<String> = REQUIRED_WORKSTREAM_IDS
        .iter()
        .map(|value| value.to_string())
        .collect();
    let observed: BTreeSet<String> = by_id.keys().cloned().collect();
    if observed != expected {
        return refusal(
            "WORKSTREAM_SET_INCOMPLETE",
            format!("expected A-K, observed {observed:?}"),
        );
    }

    let order = topological_order(&by_id)?;
    match program.final_predicates.get("standing") {
        Some(YamlValue::String(value)) if value == "ALIVE" => {}
        _ => {
            return refusal(
                "FINAL_STANDING_INVALID",
                "final predicate standing must equal ALIVE",
            )
        }
    }

    let program_digest = digest_json(program)?;
    Ok(ProgramValidationReport {
        schema_version: program.schema_version.clone(),
        program_id: program.program_id.clone(),
        program_digest,
        workstream_order: order,
        invariant_count: program.invariants.len(),
        solution_pack_count: program.initial_solution_packs.len(),
        valid: true,
    })
}

pub fn snapshot_repository(path: &Path) -> Result<RepositorySnapshot> {
    let canonical = fs::canonicalize(path).map_err(|source| FoundryError::Io {
        path: path.display().to_string(),
        source,
    })?;
    let head = git(&canonical, &["rev-parse", "HEAD"])?;
    let branch = git_optional(&canonical, &["symbolic-ref", "--short", "HEAD"])
        .unwrap_or_else(|| "DETACHED".to_string());
    let origin = git_optional(&canonical, &["config", "--get", "remote.origin.url"]);
    let status = git(
        &canonical,
        &["status", "--porcelain=v1", "--untracked-files=all"],
    )?;
    let dirty_entries: Vec<String> = status
        .lines()
        .filter(|line| !line.trim().is_empty())
        .map(str::to_string)
        .collect();
    // Hash Git index records rather than dereferencing the working tree. The staged
    // record binds mode, object ID, stage, and raw path bytes, so tracked symlinks,
    // gitlinks, and intentionally absent worktree targets remain observable.
    let tracked_index = git_bytes(&canonical, &["ls-files", "--stage", "-z"])?;
    let tracked_file_count = tracked_index
        .split(|byte| *byte == 0)
        .filter(|record| !record.is_empty())
        .count();
    let object_format = git(&canonical, &["rev-parse", "--show-object-format"])?;
    let mut tree_hasher = Hasher::new();
    hash_named_bytes(
        &mut tree_hasher,
        "git-object-format",
        object_format.as_bytes(),
    );
    hash_named_bytes(&mut tree_hasher, "git-index-stage-records", &tracked_index);
    let tracked_tree_digest = tree_hasher.finalize().to_hex().to_string();

    Ok(RepositorySnapshot {
        path: canonical.display().to_string(),
        head,
        branch,
        origin,
        clean: dirty_entries.is_empty(),
        dirty_entries,
        tracked_file_count,
        tracked_tree_digest,
    })
}

pub fn create_baseline(
    program: &WorkProgram, source_path: &Path, corpus_path: &Path, output_dir: &Path,
) -> Result<BaselineManifest> {
    let validation = validate_program(program)?;
    let source = snapshot_repository(source_path)?;
    let corpus = snapshot_repository(corpus_path)?;
    require_clean(&source, "SOURCE_WORKTREE_DIRTY")?;
    require_clean(&corpus, "CORPUS_WORKTREE_DIRTY")?;

    let manifest = BaselineManifest {
        schema_version: BASELINE_SCHEMA.to_string(),
        program_id: program.program_id.clone(),
        program_digest: validation.program_digest.clone(),
        source,
        corpus,
    };
    let manifest_path = output_dir.join("baseline-manifest.json");
    let manifest_digest = write_json(&manifest_path, &manifest)?;

    let mut inputs = BTreeMap::new();
    inputs.insert("program".to_string(), validation.program_digest);
    inputs.insert(
        "source-tree".to_string(),
        manifest.source.tracked_tree_digest.clone(),
    );
    inputs.insert(
        "corpus-tree".to_string(),
        manifest.corpus.tracked_tree_digest.clone(),
    );
    let mut outputs = BTreeMap::new();
    outputs.insert(
        "external:baseline-manifest.json".to_string(),
        manifest_digest.clone(),
    );
    let receipt = make_receipt(
        "BASELINE",
        "ggen and ggen-legacy exact-head baseline",
        manifest_digest,
        &manifest.source.head,
        &manifest.corpus.head,
        inputs,
        outputs,
    );
    write_json(&output_dir.join("baseline.receipt.json"), &receipt)?;
    Ok(manifest)
}

pub fn initialize_corpus(
    program: &WorkProgram, source_path: &Path, corpus_path: &Path,
) -> Result<InitializationReport> {
    let validation = validate_program(program)?;
    let source = snapshot_repository(source_path)?;
    let corpus = snapshot_repository(corpus_path)?;
    require_clean(&source, "SOURCE_WORKTREE_DIRTY")?;
    require_clean(&corpus, "CORPUS_WORKTREE_DIRTY")?;

    let foundry_root = corpus_path.join("foundry");
    let manifest_path = foundry_root.join("foundry-manifest.json");
    if manifest_path.exists() {
        return refusal(
            "CORPUS_ALREADY_INITIALIZED",
            format!(
                "{} already exists; use verify or replay",
                manifest_path.display()
            ),
        );
    }

    let catalog_paths = vec![
        "catalogs/architectures.json".to_string(),
        "catalogs/capabilities.json".to_string(),
        "catalogs/primitives.json".to_string(),
        "catalogs/bblocks.json".to_string(),
        "catalogs/solution-packs.json".to_string(),
        "catalogs/equivalence.json".to_string(),
        "catalogs/verifiers.json".to_string(),
    ];
    let workstream_ids = validation.workstream_order.clone();
    let manifest = FoundryManifest {
        schema_version: CORPUS_SCHEMA.to_string(),
        program_id: program.program_id.clone(),
        program_digest: validation.program_digest.clone(),
        source_repository: program.source_repository.clone(),
        corpus_repository: program.corpus_repository.clone(),
        source_head: source.head.clone(),
        corpus_parent_head: corpus.head.clone(),
        source_tree_digest: source.tracked_tree_digest.clone(),
        corpus_parent_tree_digest: corpus.tracked_tree_digest.clone(),
        initial_solution_packs: program.initial_solution_packs.clone(),
        catalog_paths: catalog_paths.clone(),
        workstream_ids: workstream_ids.clone(),
    };

    let mut generated = BTreeMap::new();
    generated.insert(
        "corpus:foundry/foundry-manifest.json".to_string(),
        write_json(&manifest_path, &manifest)?,
    );

    for (path, catalog_type) in [
        ("catalogs/architectures.json", "ARCHITECTURES"),
        ("catalogs/capabilities.json", "CAPABILITIES"),
        ("catalogs/primitives.json", "PRIMITIVES"),
        ("catalogs/bblocks.json", "BBLOCKS"),
        ("catalogs/equivalence.json", "EQUIVALENCE"),
        ("catalogs/verifiers.json", "VERIFIERS"),
    ] {
        let catalog: Catalog<serde_json::Value> = Catalog {
            schema_version: CORPUS_SCHEMA.to_string(),
            catalog_type: catalog_type.to_string(),
            entries: Vec::new(),
        };
        generated.insert(
            format!("corpus:foundry/{path}"),
            write_json(&foundry_root.join(path), &catalog)?,
        );
    }

    let pack_entries: Vec<SolutionPackRecord> = program
        .initial_solution_packs
        .iter()
        .map(|id| SolutionPackRecord {
            id: id.clone(),
            standing: "DECLARED".to_string(),
            required_workstreams: vec![
                "F".to_string(),
                "G".to_string(),
                "H".to_string(),
                "I".to_string(),
                "J".to_string(),
                "K".to_string(),
            ],
        })
        .collect();
    let pack_catalog = Catalog {
        schema_version: CORPUS_SCHEMA.to_string(),
        catalog_type: "SOLUTION_PACKS".to_string(),
        entries: pack_entries,
    };
    generated.insert(
        "corpus:foundry/catalogs/solution-packs.json".to_string(),
        write_json(
            &foundry_root.join("catalogs/solution-packs.json"),
            &pack_catalog,
        )?,
    );

    let mut states = BTreeMap::new();
    for workstream in &program.workstreams {
        states.insert(
            workstream.id.clone(),
            WorkstreamState {
                status: if workstream.dependencies.is_empty() {
                    "READY".to_string()
                } else {
                    "BLOCKED".to_string()
                },
                dependencies: workstream.dependencies.clone(),
                report_digest: None,
                receipt_path: None,
            },
        );
    }
    let state_file = WorkstreamStateFile {
        schema_version: CORPUS_SCHEMA.to_string(),
        program_id: program.program_id.clone(),
        workstreams: states,
    };
    generated.insert(
        "projection:foundry/workstreams/state.json".to_string(),
        write_json(&foundry_root.join("workstreams/state.json"), &state_file)?,
    );

    let standing = StandingRecord {
        schema_version: CORPUS_SCHEMA.to_string(),
        program_id: program.program_id.clone(),
        standing: "PARTIAL_ALIVE".to_string(),
        admitted: false,
        reasons: vec![
            "A-K workstream admission is incomplete".to_string(),
            "Fortune-scale reference manufacture is not yet admitted".to_string(),
        ],
        source_head: source.head.clone(),
        corpus_head: corpus.head.clone(),
    };
    generated.insert(
        "projection:foundry/standing.json".to_string(),
        write_json(&foundry_root.join("standing.json"), &standing)?,
    );

    let generated_tree_digest = digest_named_outputs(&generated);
    let mut inputs = BTreeMap::new();
    inputs.insert("program".to_string(), validation.program_digest);
    inputs.insert("source-tree".to_string(), source.tracked_tree_digest);
    inputs.insert("corpus-parent-tree".to_string(), corpus.tracked_tree_digest);
    let receipt = make_receipt(
        "CORPUS_INITIALIZATION",
        "ggen-legacy Enterprise Architecture Foundry corpus",
        generated_tree_digest.clone(),
        &source.head,
        &corpus.head,
        inputs,
        generated.clone(),
    );
    let receipt_relative = "foundry/receipts/initialization.json";
    write_json(&corpus_path.join(receipt_relative), &receipt)?;

    Ok(InitializationReport {
        manifest_path: "foundry/foundry-manifest.json".to_string(),
        receipt_path: receipt_relative.to_string(),
        generated_file_count: generated.len(),
        generated_tree_digest,
        source_head: source.head,
        corpus_parent_head: corpus.head,
    })
}

pub fn load_migration_manifest(path: &Path) -> Result<MigrationManifest> {
    let bytes = read(path)?;
    let manifest: MigrationManifest = serde_yaml::from_slice(&bytes)?;
    if manifest.schema_version != MIGRATION_SCHEMA {
        return refusal(
            "MIGRATION_SCHEMA_INVALID",
            format!(
                "expected {MIGRATION_SCHEMA}, observed {}",
                manifest.schema_version
            ),
        );
    }
    Ok(manifest)
}

pub fn extract_components(
    program: &WorkProgram, source_path: &Path, corpus_path: &Path, migration: &MigrationManifest,
) -> Result<ExtractionReport> {
    validate_program(program)?;
    validate_migration_manifest(migration)?;
    let source = snapshot_repository(source_path)?;
    let corpus = snapshot_repository(corpus_path)?;
    require_clean(&source, "SOURCE_WORKTREE_DIRTY")?;
    require_clean(&corpus, "CORPUS_WORKTREE_DIRTY")?;
    if source.head != migration.source_head {
        return refusal(
            "MIGRATION_SOURCE_HEAD_STALE",
            format!(
                "manifest binds {}, repository is {}",
                migration.source_head, source.head
            ),
        );
    }
    if corpus.head != migration.corpus_parent_head {
        return refusal(
            "MIGRATION_CORPUS_HEAD_STALE",
            format!(
                "manifest binds {}, repository is {}",
                migration.corpus_parent_head, corpus.head
            ),
        );
    }

    let mut output_digests = BTreeMap::new();
    for component in &migration.components {
        let source_relative = safe_relative(&component.source_path)?;
        let destination_relative = safe_relative(&component.destination_path)?;
        let source_file = source_path.join(&source_relative);
        let destination_file = corpus_path.join(&destination_relative);
        let metadata = fs::metadata(&source_file).map_err(|source| FoundryError::Io {
            path: source_file.display().to_string(),
            source,
        })?;
        if !metadata.is_file() {
            return refusal(
                "MIGRATION_SOURCE_NOT_FILE",
                format!("{} is not a file", source_file.display()),
            );
        }
        let bytes = read(&source_file)?;
        let content_digest = digest_bytes(&bytes);
        write_bytes_exact(&destination_file, &bytes)?;
        output_digests.insert(
            format!("corpus:{}", destination_relative.display()),
            content_digest.clone(),
        );

        let lineage = LineageRecord {
            schema_version: LINEAGE_SCHEMA.to_string(),
            batch_id: migration.batch_id.clone(),
            component_id: component.id.clone(),
            source_repository: program.source_repository.clone(),
            corpus_repository: program.corpus_repository.clone(),
            source_head: source.head.clone(),
            corpus_parent_head: corpus.head.clone(),
            source_path: component.source_path.clone(),
            destination_path: component.destination_path.clone(),
            content_digest,
            disposition: component.disposition.clone(),
            capability_ids: component.capability_ids.clone(),
            replacement_owner: component.replacement_owner.clone(),
            rationale: component.rationale.clone(),
        };
        let lineage_relative = PathBuf::from("foundry/lineage")
            .join(&migration.batch_id)
            .join(format!("{}.json", component.id));
        let lineage_digest = write_json(&corpus_path.join(&lineage_relative), &lineage)?;
        output_digests.insert(
            format!("corpus:{}", lineage_relative.display()),
            lineage_digest,
        );
    }

    let batch_digest = digest_named_outputs(&output_digests);
    let mut inputs = BTreeMap::new();
    inputs.insert("migration-manifest".to_string(), digest_json(migration)?);
    inputs.insert("source-tree".to_string(), source.tracked_tree_digest);
    inputs.insert("corpus-parent-tree".to_string(), corpus.tracked_tree_digest);
    let receipt = make_receipt(
        "CROSS_REPOSITORY_EXTRACTION",
        &migration.batch_id,
        batch_digest.clone(),
        &source.head,
        &corpus.head,
        inputs,
        output_digests,
    );
    let receipt_relative =
        PathBuf::from("foundry/receipts").join(format!("extraction-{}.json", migration.batch_id));
    write_json(&corpus_path.join(&receipt_relative), &receipt)?;

    Ok(ExtractionReport {
        batch_id: migration.batch_id.clone(),
        component_count: migration.components.len(),
        source_head: source.head,
        corpus_parent_head: corpus.head,
        batch_digest,
        receipt_path: receipt_relative.display().to_string(),
    })
}

pub fn load_workstream_report(path: &Path) -> Result<WorkstreamReport> {
    let bytes = read(path)?;
    let report: WorkstreamReport = serde_json::from_slice(&bytes)?;
    if report.schema_version != WORKSTREAM_REPORT_SCHEMA {
        return refusal(
            "WORKSTREAM_REPORT_SCHEMA_INVALID",
            format!(
                "expected {WORKSTREAM_REPORT_SCHEMA}, observed {}",
                report.schema_version
            ),
        );
    }
    Ok(report)
}

pub fn admit_workstream(
    program: &WorkProgram, source_path: &Path, corpus_path: &Path, report: &WorkstreamReport,
) -> Result<WorkstreamAdmission> {
    validate_program(program)?;
    let source = snapshot_repository(source_path)?;
    let corpus = snapshot_repository(corpus_path)?;
    require_clean(&source, "SOURCE_WORKTREE_DIRTY")?;
    require_clean(&corpus, "CORPUS_WORKTREE_DIRTY")?;
    if report.source_head != source.head || report.corpus_head != corpus.head {
        return refusal(
            "WORKSTREAM_REPORT_HEAD_STALE",
            format!(
                "report source/corpus {}/{} does not match {}/{}",
                report.source_head, report.corpus_head, source.head, corpus.head
            ),
        );
    }
    if report.verifier.trim().is_empty() {
        return refusal("WORKSTREAM_VERIFIER_MISSING", "verifier identity is empty");
    }

    let workstream = program
        .workstreams
        .iter()
        .find(|value| value.id == report.workstream_id)
        .ok_or_else(|| FoundryError::Refusal {
            code: "WORKSTREAM_UNKNOWN".to_string(),
            message: report.workstream_id.clone(),
        })?;
    for (key, expected) in &workstream.predicates {
        match report.predicates.get(key) {
            Some(observed) if observed == expected => {}
            Some(observed) => {
                return refusal(
                    "WORKSTREAM_PREDICATE_FALSE",
                    format!(
                        "{} predicate {key} expected {expected:?}, observed {observed:?}",
                        workstream.id
                    ),
                )
            }
            None => {
                return refusal(
                    "WORKSTREAM_PREDICATE_MISSING",
                    format!("{} predicate {key} is absent", workstream.id),
                )
            }
        }
    }

    let state_path = corpus_path.join("foundry/workstreams/state.json");
    let mut state: WorkstreamStateFile = serde_json::from_slice(&read(&state_path)?)?;
    for dependency in &workstream.dependencies {
        let dependency_state =
            state
                .workstreams
                .get(dependency)
                .ok_or_else(|| FoundryError::Refusal {
                    code: "WORKSTREAM_DEPENDENCY_STATE_MISSING".to_string(),
                    message: dependency.clone(),
                })?;
        if dependency_state.status != "ADMITTED" {
            return refusal(
                "WORKSTREAM_DEPENDENCY_NOT_ADMITTED",
                format!("{} depends on {dependency}", workstream.id),
            );
        }
    }

    if report.outputs.is_empty() {
        return refusal(
            "WORKSTREAM_EVIDENCE_EMPTY",
            format!("{} has no evidence outputs", workstream.id),
        );
    }
    for evidence in &report.outputs {
        verify_evidence_file(source_path, corpus_path, evidence)?;
    }

    let report_digest = digest_json(report)?;
    let admitted_relative = PathBuf::from("foundry/workstreams")
        .join(&workstream.id)
        .join("admission-report.json");
    write_json(&corpus_path.join(&admitted_relative), report)?;

    let mut output_digests = BTreeMap::new();
    output_digests.insert(
        format!("corpus:{}", admitted_relative.display()),
        report_digest.clone(),
    );
    let mut inputs = BTreeMap::new();
    inputs.insert("work-program".to_string(), digest_json(program)?);
    inputs.insert("workstream-report".to_string(), report_digest.clone());
    for evidence in &report.outputs {
        inputs.insert(
            format!("{}:{}", evidence.repository, evidence.path),
            evidence.blake3.clone(),
        );
    }
    let receipt = make_receipt(
        "WORKSTREAM_ADMISSION",
        &workstream.id,
        report_digest.clone(),
        &source.head,
        &corpus.head,
        inputs,
        output_digests,
    );
    let receipt_relative =
        PathBuf::from("foundry/receipts").join(format!("workstream-{}.json", workstream.id));
    write_json(&corpus_path.join(&receipt_relative), &receipt)?;

    let current =
        state
            .workstreams
            .get_mut(&workstream.id)
            .ok_or_else(|| FoundryError::Refusal {
                code: "WORKSTREAM_STATE_MISSING".to_string(),
                message: workstream.id.clone(),
            })?;
    current.status = "ADMITTED".to_string();
    current.report_digest = Some(report_digest.clone());
    current.receipt_path = Some(receipt_relative.display().to_string());

    for candidate in &program.workstreams {
        if let Some(candidate_state) = state.workstreams.get(&candidate.id) {
            if candidate_state.status == "ADMITTED" {
                continue;
            }
        }
        let dependencies_admitted = candidate.dependencies.iter().all(|dependency| {
            state
                .workstreams
                .get(dependency)
                .map(|value| value.status == "ADMITTED")
                .unwrap_or(false)
        });
        if dependencies_admitted {
            if let Some(candidate_state) = state.workstreams.get_mut(&candidate.id) {
                candidate_state.status = "READY".to_string();
            }
        }
    }
    write_json_replace(&state_path, &state)?;

    Ok(WorkstreamAdmission {
        workstream_id: workstream.id.clone(),
        report_digest,
        receipt_path: receipt_relative.display().to_string(),
        status: "ADMITTED".to_string(),
    })
}

pub fn load_final_evidence(path: &Path) -> Result<FinalEvidenceReport> {
    let bytes = read(path)?;
    let report: FinalEvidenceReport = serde_json::from_slice(&bytes)?;
    if report.schema_version != FINAL_EVIDENCE_SCHEMA {
        return refusal(
            "FINAL_EVIDENCE_SCHEMA_INVALID",
            format!(
                "expected {FINAL_EVIDENCE_SCHEMA}, observed {}",
                report.schema_version
            ),
        );
    }
    Ok(report)
}

pub fn admit_solution(
    program: &WorkProgram, source_path: &Path, corpus_path: &Path,
    final_evidence: &FinalEvidenceReport,
) -> Result<StandingRecord> {
    validate_program(program)?;
    let source = snapshot_repository(source_path)?;
    let corpus = snapshot_repository(corpus_path)?;
    require_clean(&source, "SOURCE_WORKTREE_DIRTY")?;
    require_clean(&corpus, "CORPUS_WORKTREE_DIRTY")?;
    if final_evidence.source_head != source.head || final_evidence.corpus_head != corpus.head {
        return refusal(
            "FINAL_EVIDENCE_HEAD_STALE",
            "final evidence does not bind the current source and corpus heads",
        );
    }
    if final_evidence.verifier.trim().is_empty() {
        return refusal("FINAL_VERIFIER_MISSING", "final verifier identity is empty");
    }

    let state_path = corpus_path.join("foundry/workstreams/state.json");
    let state: WorkstreamStateFile = serde_json::from_slice(&read(&state_path)?)?;
    let incomplete: Vec<String> = REQUIRED_WORKSTREAM_IDS
        .iter()
        .filter(|id| {
            state
                .workstreams
                .get(**id)
                .map(|value| value.status != "ADMITTED")
                .unwrap_or(true)
        })
        .map(|id| id.to_string())
        .collect();
    if !incomplete.is_empty() {
        return refusal(
            "FINAL_WORKSTREAMS_INCOMPLETE",
            format!("workstreams not admitted: {incomplete:?}"),
        );
    }

    for (key, expected) in &program.final_predicates {
        match final_evidence.predicates.get(key) {
            Some(observed) if observed == expected => {}
            Some(observed) => {
                return refusal(
                    "FINAL_PREDICATE_FALSE",
                    format!("predicate {key} expected {expected:?}, observed {observed:?}"),
                )
            }
            None => {
                return refusal(
                    "FINAL_PREDICATE_MISSING",
                    format!("predicate {key} is absent"),
                )
            }
        }
    }
    for evidence in &final_evidence.evidence {
        verify_evidence_file(source_path, corpus_path, evidence)?;
    }

    let standing = StandingRecord {
        schema_version: CORPUS_SCHEMA.to_string(),
        program_id: program.program_id.clone(),
        standing: "ALIVE".to_string(),
        admitted: true,
        reasons: Vec::new(),
        source_head: source.head.clone(),
        corpus_head: corpus.head.clone(),
    };
    let standing_path = corpus_path.join("foundry/standing.json");
    let standing_digest = write_json_replace(&standing_path, &standing)?;
    let mut inputs = BTreeMap::new();
    inputs.insert("work-program".to_string(), digest_json(program)?);
    inputs.insert("final-evidence".to_string(), digest_json(final_evidence)?);
    for evidence in &final_evidence.evidence {
        inputs.insert(
            format!("{}:{}", evidence.repository, evidence.path),
            evidence.blake3.clone(),
        );
    }
    let mut outputs = BTreeMap::new();
    outputs.insert(
        "corpus:foundry/standing.json".to_string(),
        standing_digest.clone(),
    );
    let receipt = make_receipt(
        "SOLUTION_ADMISSION",
        &program.program_id,
        standing_digest,
        &source.head,
        &corpus.head,
        inputs,
        outputs,
    );
    write_json(
        &corpus_path.join("foundry/receipts/solution-admission.json"),
        &receipt,
    )?;
    Ok(standing)
}

pub fn verify_corpus(
    program: &WorkProgram, source_path: &Path, corpus_path: &Path,
) -> Result<VerificationReport> {
    validate_program(program)?;
    let source = snapshot_repository(source_path)?;
    let corpus = snapshot_repository(corpus_path)?;
    let foundry_root = corpus_path.join("foundry");
    let manifest_path = foundry_root.join("foundry-manifest.json");
    let manifest_valid = match read(&manifest_path) {
        Ok(bytes) => serde_json::from_slice::<FoundryManifest>(&bytes)
            .map(|manifest| {
                manifest.schema_version == CORPUS_SCHEMA
                    && manifest.program_id == program.program_id
                    && manifest.source_repository == program.source_repository
                    && manifest.corpus_repository == program.corpus_repository
            })
            .unwrap_or(false),
        Err(_) => false,
    };

    let mut lineage_records_checked = 0usize;
    let mut invalid_lineage_records = Vec::new();
    let lineage_root = foundry_root.join("lineage");
    if lineage_root.exists() {
        for entry in sorted_files(&lineage_root)? {
            if entry.extension().and_then(|value| value.to_str()) != Some("json") {
                continue;
            }
            lineage_records_checked += 1;
            if let Err(error) = verify_lineage_record(source_path, corpus_path, &entry) {
                invalid_lineage_records.push(format!("{}: {error}", entry.display()));
            }
        }
    }

    // Real fix (found running workstream J's clean-room replay): this used to
    // call replay_receipt per file, checking each receipt's output digests
    // against current state independently -- the same stale-vs-superseded
    // problem replay_all_receipts had, but here it fed a real admission gate
    // (admit_clean_room's clean_room_verification_success requires
    // invalid_receipts.is_empty()), so "diagnostic, not gating" was the
    // wrong call for this call site. Now checks each receipt's own
    // schema/subject_digest self-consistency individually (a real per-
    // receipt property), but output-digest drift only against the
    // causally-latest recorded digest per path, same as replay_all_receipts.
    let mut receipts_checked = 0usize;
    let mut invalid_receipts = Vec::new();
    let receipts_root = foundry_root.join("receipts");
    if receipts_root.exists() {
        let mut receipts = Vec::new();
        for entry in sorted_files(&receipts_root)? {
            if entry.extension().and_then(|value| value.to_str()) != Some("json") {
                continue;
            }
            receipts_checked += 1;
            match read(&entry).map_err(FoundryError::from).and_then(|bytes| {
                serde_json::from_slice::<Receipt>(&bytes).map_err(FoundryError::from)
            }) {
                Ok(receipt) => {
                    if receipt.schema_version != RECEIPT_SCHEMA {
                        invalid_receipts.push(format!(
                            "{}: RECEIPT_SCHEMA_INVALID: {}",
                            entry.display(),
                            receipt.schema_version
                        ));
                        continue;
                    }
                    let observed_subject = digest_named_outputs(&receipt.output_digests);
                    if !receipt.output_digests.is_empty()
                        && observed_subject != receipt.subject_digest
                    {
                        invalid_receipts.push(format!(
                            "{}: RECEIPT_SUBJECT_DIGEST_INVALID: expected {}, recomputed {}",
                            entry.display(),
                            receipt.subject_digest,
                            observed_subject
                        ));
                        continue;
                    }
                    receipts.push(receipt);
                }
                Err(error) => invalid_receipts.push(format!("{}: {error}", entry.display())),
            }
        }
        let mut latest_expected: BTreeMap<String, String> = BTreeMap::new();
        for receipt in &receipts {
            for (key, digest) in &receipt.output_digests {
                let Some((repository, _)) = key.split_once(':') else {
                    continue;
                };
                if matches!(repository, "external" | "projection") {
                    continue;
                }
                latest_expected.insert(key.clone(), digest.clone());
            }
        }
        for (key, expected) in &latest_expected {
            let Some((repository, relative)) = key.split_once(':') else {
                continue;
            };
            let root = match repository {
                "source" => source_path,
                "corpus" => corpus_path,
                _ => continue,
            };
            let Ok(relative) = safe_relative(relative) else {
                invalid_receipts.push(format!("{key}: RECEIPT_OUTPUT_KEY_INVALID"));
                continue;
            };
            match digest_file(&root.join(&relative)) {
                Ok(observed) if &observed == expected => {}
                Ok(observed) => invalid_receipts.push(format!(
                    "{key}: RECEIPT_OUTPUT_DRIFT: expected {expected}, observed {observed}"
                )),
                Err(error) => invalid_receipts.push(format!("{key}: {error}")),
            }
        }
    }

    let state_path = foundry_root.join("workstreams/state.json");
    let admitted_workstreams = match read(&state_path) {
        Ok(bytes) => serde_json::from_slice::<WorkstreamStateFile>(&bytes)
            .map(|state| {
                state
                    .workstreams
                    .into_iter()
                    .filter(|(_, value)| value.status == "ADMITTED")
                    .map(|(id, _)| id)
                    .collect()
            })
            .unwrap_or_default(),
        Err(_) => Vec::new(),
    };
    let standing_path = foundry_root.join("standing.json");
    let standing = match read(&standing_path) {
        Ok(bytes) => serde_json::from_slice::<StandingRecord>(&bytes).unwrap_or(StandingRecord {
            schema_version: CORPUS_SCHEMA.to_string(),
            program_id: program.program_id.clone(),
            standing: "UNKNOWN".to_string(),
            admitted: false,
            reasons: vec!["standing record is invalid".to_string()],
            source_head: source.head.clone(),
            corpus_head: corpus.head.clone(),
        }),
        Err(_) => StandingRecord {
            schema_version: CORPUS_SCHEMA.to_string(),
            program_id: program.program_id.clone(),
            standing: "UNKNOWN".to_string(),
            admitted: false,
            reasons: vec!["standing record is missing".to_string()],
            source_head: source.head.clone(),
            corpus_head: corpus.head.clone(),
        },
    };

    Ok(VerificationReport {
        program_valid: true,
        source_head: source.head,
        corpus_head: corpus.head,
        manifest_valid,
        lineage_records_checked,
        invalid_lineage_records,
        receipts_checked,
        invalid_receipts,
        admitted_workstreams,
        standing: standing.standing,
        admitted: standing.admitted,
    })
}

/// Replays the causal receipt DAG as an admission gate.
///
/// Unlike [`replay_receipt`] (used by `verify_corpus` as a per-receipt,
/// point-in-time diagnostic), this function must tolerate a real, legitimate
/// pattern: `initialize-corpus` seeds catalog files (e.g.
/// `foundry/catalogs/capabilities.json`) with an initial digest, and a later
/// workstream's admit binary can legitimately supersede that file with
/// `write_replace`, recording a *new* digest in its own receipt. Checking
/// every receipt's output digest independently against current state (the
/// original implementation) treats that legitimate supersession as
/// `RECEIPT_OUTPUT_DRIFT` forever after -- permanently blocking any later
/// admission that depends on replay succeeding. The fix: for each output
/// path, only the digest recorded by the causally-latest receipt (receipts
/// are processed in `sorted_files` order, which for this repo's naming --
/// `initialization.json` before `workstream-<LETTER>.json`, letters in
/// dependency order -- coincides with real causal order) is checked against
/// current state; earlier receipts' expectations for the same path are
/// superseded, not violated.
pub fn replay_all_receipts(source_path: &Path, corpus_path: &Path) -> Result<usize> {
    let receipts_root = corpus_path.join("foundry/receipts");
    if !receipts_root.exists() {
        return refusal(
            "RECEIPT_DIRECTORY_MISSING",
            receipts_root.display().to_string(),
        );
    }
    let mut receipts = Vec::new();
    for entry in sorted_files(&receipts_root)? {
        if entry.extension().and_then(|value| value.to_str()) != Some("json") {
            continue;
        }
        let receipt: Receipt = serde_json::from_slice(&read(&entry)?)?;
        if receipt.schema_version != RECEIPT_SCHEMA {
            return refusal(
                "RECEIPT_SCHEMA_INVALID",
                format!("{} has schema {}", entry.display(), receipt.schema_version),
            );
        }
        let observed_subject = digest_named_outputs(&receipt.output_digests);
        if !receipt.output_digests.is_empty() && observed_subject != receipt.subject_digest {
            return refusal(
                "RECEIPT_SUBJECT_DIGEST_INVALID",
                format!(
                    "receipt {} expected {}, recomputed {}",
                    entry.display(),
                    receipt.subject_digest,
                    observed_subject
                ),
            );
        }
        receipts.push(receipt);
    }

    let mut latest_expected: BTreeMap<String, String> = BTreeMap::new();
    for receipt in &receipts {
        for (key, digest) in &receipt.output_digests {
            let (repository, _) = key.split_once(':').ok_or_else(|| FoundryError::Refusal {
                code: "RECEIPT_OUTPUT_KEY_INVALID".to_string(),
                message: key.clone(),
            })?;
            if matches!(repository, "external" | "projection") {
                continue;
            }
            // Later receipts (causal order) supersede earlier ones for the same path.
            latest_expected.insert(key.clone(), digest.clone());
        }
    }
    for (key, expected) in &latest_expected {
        let (repository, relative) = key.split_once(':').ok_or_else(|| FoundryError::Refusal {
            code: "RECEIPT_OUTPUT_KEY_INVALID".to_string(),
            message: key.clone(),
        })?;
        let relative = safe_relative(relative)?;
        let root = match repository {
            "source" => source_path,
            "corpus" => corpus_path,
            _ => {
                return refusal(
                    "RECEIPT_REPOSITORY_INVALID",
                    format!("repository selector {repository} is invalid"),
                )
            }
        };
        let observed = digest_file(&root.join(relative))?;
        if &observed != expected {
            return refusal(
                "RECEIPT_OUTPUT_DRIFT",
                format!("{key} expected {expected}, observed {observed}"),
            );
        }
    }
    Ok(receipts.len())
}

fn validate_migration_manifest(manifest: &MigrationManifest) -> Result<()> {
    if manifest.batch_id.trim().is_empty() {
        return refusal("MIGRATION_BATCH_ID_MISSING", "batch_id is empty");
    }
    if manifest.components.is_empty() {
        return refusal("MIGRATION_COMPONENTS_EMPTY", "no components were declared");
    }
    let mut ids = BTreeSet::new();
    let mut destinations = BTreeSet::new();
    for component in &manifest.components {
        if !ids.insert(component.id.clone()) {
            return refusal(
                "MIGRATION_COMPONENT_DUPLICATED",
                format!("component {} is duplicated", component.id),
            );
        }
        if !destinations.insert(component.destination_path.clone()) {
            return refusal(
                "MIGRATION_DESTINATION_DUPLICATED",
                format!("destination {} is duplicated", component.destination_path),
            );
        }
        safe_relative(&component.source_path)?;
        safe_relative(&component.destination_path)?;
        if component.disposition == "UNKNOWN" || component.disposition.trim().is_empty() {
            return refusal(
                "MIGRATION_DISPOSITION_UNKNOWN",
                format!("component {} has no closed disposition", component.id),
            );
        }
        if component.replacement_owner.trim().is_empty() {
            return refusal(
                "MIGRATION_REPLACEMENT_OWNER_MISSING",
                format!("component {} has no replacement owner", component.id),
            );
        }
        if component.rationale.trim().is_empty() {
            return refusal(
                "MIGRATION_RATIONALE_MISSING",
                format!("component {} has no rationale", component.id),
            );
        }
    }
    Ok(())
}

fn verify_lineage_record(source_path: &Path, corpus_path: &Path, path: &Path) -> Result<()> {
    let bytes = read(path)?;
    let value: serde_json::Value = serde_json::from_slice(&bytes)?;
    match value
        .get("schema_version")
        .and_then(serde_json::Value::as_str)
    {
        Some(LINEAGE_SCHEMA) => {
            let record: LineageRecord = serde_json::from_value(value)?;
            verify_current_lineage_record(source_path, corpus_path, path, &record)
        }
        Some(
            "ggen.enterprise-architecture-foundry.extraction-admission/1"
            | "ggen.enterprise-architecture-foundry.extraction-admission/2",
        ) => {
            let record: HistoricalExtractionLineageRecord = serde_json::from_value(value)?;
            verify_historical_extraction_lineage(source_path, corpus_path, path, &record)
        }
        Some(observed) => refusal(
            "LINEAGE_SCHEMA_INVALID",
            format!("{} has schema {observed}", path.display()),
        ),
        None => refusal("LINEAGE_SCHEMA_MISSING", path.display().to_string()),
    }
}

fn verify_current_lineage_record(
    source_path: &Path, corpus_path: &Path, path: &Path, record: &LineageRecord,
) -> Result<()> {
    if record.schema_version != LINEAGE_SCHEMA {
        return refusal(
            "LINEAGE_SCHEMA_INVALID",
            format!("{} has schema {}", path.display(), record.schema_version),
        );
    }
    let source_relative = safe_relative(&record.source_path)?;
    let destination_relative = safe_relative(&record.destination_path)?;
    let source_digest = digest_file(&source_path.join(source_relative))?;
    let destination_digest = digest_file(&corpus_path.join(destination_relative))?;
    if source_digest != record.content_digest || destination_digest != record.content_digest {
        return refusal(
            "LINEAGE_DIGEST_MISMATCH",
            format!("component {} content changed", record.component_id),
        );
    }
    Ok(())
}

fn verify_historical_extraction_lineage(
    source_path: &Path, corpus_path: &Path, path: &Path, record: &HistoricalExtractionLineageRecord,
) -> Result<()> {
    if record.source_removed {
        return refusal(
            "HISTORICAL_LINEAGE_SOURCE_REMOVED",
            record.capability_id.clone(),
        );
    }
    let record_commits = historical_commit_set(
        &record.capability_id,
        &record.historical_commit,
        &record.historical_commits,
    )?;
    for commit in &record_commits {
        let commit_type = git(source_path, &["cat-file", "-t", commit])?;
        if commit_type != "commit" {
            return refusal(
                "HISTORICAL_LINEAGE_COMMIT_NOT_COMMIT",
                format!("{}: {}={}", record.capability_id, commit, commit_type),
            );
        }
    }

    let destination = safe_relative(&record.destination_path)?;
    let manifest_path = corpus_path
        .join(destination)
        .join("component-manifest.json");
    let manifest_bytes = read(&manifest_path)?;
    let observed_manifest_digest = digest_bytes(&manifest_bytes);
    if observed_manifest_digest != record.manifest_digest {
        return refusal(
            "HISTORICAL_LINEAGE_MANIFEST_DRIFT",
            format!(
                "{} expected {}, observed {}",
                record.capability_id, record.manifest_digest, observed_manifest_digest
            ),
        );
    }
    let manifest: HistoricalComponentManifest = serde_json::from_slice(&manifest_bytes)?;
    let manifest_commits = historical_commit_set(
        &manifest.capability_id,
        &manifest.historical_commit,
        &manifest.historical_commits,
    )?;
    if manifest.capability_id != record.capability_id
        || manifest_commits != record_commits
        || manifest.corpus_destination != record.destination_path
        || manifest.source_removed
    {
        return refusal(
            "HISTORICAL_LINEAGE_MANIFEST_IDENTITY_MISMATCH",
            record.capability_id.clone(),
        );
    }

    let mut expected_blob_digests = record.blob_digests.clone();
    let mut observed_blob_digests: Vec<String> = manifest
        .source_files
        .iter()
        .map(|entry| entry.blake3.clone())
        .collect();
    expected_blob_digests.sort();
    observed_blob_digests.sort();
    if expected_blob_digests != observed_blob_digests {
        return refusal(
            "HISTORICAL_LINEAGE_BLOB_SET_MISMATCH",
            record.capability_id.clone(),
        );
    }

    let semantic_evidence = safe_relative(&manifest.semantic_evidence_path)?;
    let semantic_digest = digest_file(&corpus_path.join(semantic_evidence))?;
    if semantic_digest != manifest.semantic_evidence_digest {
        return refusal(
            "HISTORICAL_LINEAGE_SEMANTIC_EVIDENCE_DRIFT",
            record.capability_id.clone(),
        );
    }

    if manifest.source_files.is_empty() {
        if manifest.resolution != "SEMANTIC_EVIDENCE_ONLY" {
            return refusal(
                "HISTORICAL_LINEAGE_EMPTY_SOURCE_INVALID",
                record.capability_id.clone(),
            );
        }
    } else if manifest.resolution != "GIT_OBJECTS_RECOVERED" {
        return refusal(
            "HISTORICAL_LINEAGE_RESOLUTION_INVALID",
            record.capability_id.clone(),
        );
    }

    for source_file in &manifest.source_files {
        let git_path = safe_relative(&source_file.git_path)?;
        if !is_full_git_sha(&source_file.git_object_id) {
            return refusal(
                "HISTORICAL_LINEAGE_OBJECT_ID_INVALID",
                format!("{}: {}", record.capability_id, source_file.git_object_id),
            );
        }
        let object_commit = if source_file.historical_commit.is_empty() {
            if record_commits.len() != 1 {
                return refusal(
                    "HISTORICAL_LINEAGE_FILE_COMMIT_MISSING",
                    format!(
                        "{}: {} admitted commits",
                        record.capability_id,
                        record_commits.len()
                    ),
                );
            }
            record_commits.iter().next().expect("single commit")
        } else {
            &source_file.historical_commit
        };
        if !is_full_git_sha(object_commit) {
            return refusal(
                "HISTORICAL_LINEAGE_FILE_COMMIT_INVALID",
                format!("{}: {}", record.capability_id, object_commit),
            );
        }
        if !record_commits.contains(object_commit) {
            return refusal(
                "HISTORICAL_LINEAGE_FILE_COMMIT_OUTSIDE_COMPONENT_SET",
                format!("{}: {}", record.capability_id, object_commit),
            );
        }
        let object_spec = format!("{}:{}", object_commit, git_path.to_string_lossy());
        let observed_object_id = git(source_path, &["rev-parse", &object_spec])?;
        if observed_object_id != source_file.git_object_id {
            return refusal(
                "HISTORICAL_LINEAGE_OBJECT_ID_MISMATCH",
                format!(
                    "{}:{} expected {}, observed {}",
                    record.capability_id,
                    source_file.git_path,
                    source_file.git_object_id,
                    observed_object_id
                ),
            );
        }
        let object_type = git(source_path, &["cat-file", "-t", &source_file.git_object_id])?;
        if object_type != "blob" {
            return refusal(
                "HISTORICAL_LINEAGE_OBJECT_NOT_BLOB",
                format!("{}: {}", record.capability_id, object_type),
            );
        }
        let object_bytes = git_bytes(
            source_path,
            &["cat-file", "blob", &source_file.git_object_id],
        )?;
        let object_digest = digest_bytes(&object_bytes);
        if object_digest != source_file.blake3 || object_bytes.len() != source_file.byte_length {
            return refusal(
                "HISTORICAL_LINEAGE_OBJECT_DIGEST_MISMATCH",
                format!("{}: {}", record.capability_id, source_file.git_path),
            );
        }
        let blob_relative = safe_relative(&source_file.blob_path)?;
        let blob_digest = digest_file(&corpus_path.join(blob_relative))?;
        if blob_digest != source_file.blake3 {
            return refusal(
                "HISTORICAL_LINEAGE_CORPUS_BLOB_DRIFT",
                format!("{}: {}", record.capability_id, source_file.blob_path),
            );
        }
    }
    Ok(())
}

fn historical_commit_set(
    capability_id: &str, summary: &str, explicit: &[String],
) -> Result<BTreeSet<String>> {
    let summary_set: BTreeSet<String> = summary
        .split('|')
        .map(str::trim)
        .filter(|value| !value.is_empty())
        .map(str::to_string)
        .collect();
    let explicit_set: BTreeSet<String> = explicit
        .iter()
        .map(|value| value.trim())
        .filter(|value| !value.is_empty())
        .map(str::to_string)
        .collect();
    let commits = if explicit_set.is_empty() {
        summary_set.clone()
    } else {
        if summary_set != explicit_set {
            return refusal(
                "HISTORICAL_LINEAGE_COMMIT_SET_MISMATCH",
                format!(
                    "{}: summary={:?}, explicit={:?}",
                    capability_id, summary_set, explicit_set
                ),
            );
        }
        explicit_set
    };
    if commits.is_empty() {
        return refusal(
            "HISTORICAL_LINEAGE_COMMIT_SET_EMPTY",
            capability_id.to_string(),
        );
    }
    for commit in &commits {
        if !is_full_git_sha(commit) {
            return refusal(
                "HISTORICAL_LINEAGE_COMMIT_INVALID",
                format!("{}: {}", capability_id, commit),
            );
        }
    }
    Ok(commits)
}

fn is_full_git_sha(value: &str) -> bool {
    value.len() == 40 && value.bytes().all(|byte| byte.is_ascii_hexdigit())
}

fn replay_receipt(source_path: &Path, corpus_path: &Path, path: &Path) -> Result<()> {
    let receipt: Receipt = serde_json::from_slice(&read(path)?)?;
    if receipt.schema_version != RECEIPT_SCHEMA {
        return refusal(
            "RECEIPT_SCHEMA_INVALID",
            format!("{} has schema {}", path.display(), receipt.schema_version),
        );
    }
    for (key, expected) in &receipt.output_digests {
        let (repository, relative) = key.split_once(':').ok_or_else(|| FoundryError::Refusal {
            code: "RECEIPT_OUTPUT_KEY_INVALID".to_string(),
            message: key.clone(),
        })?;
        if matches!(repository, "external" | "projection") {
            continue;
        }
        let relative = safe_relative(relative)?;
        let root = match repository {
            "source" => source_path,
            "corpus" => corpus_path,
            _ => {
                return refusal(
                    "RECEIPT_REPOSITORY_INVALID",
                    format!("repository selector {repository} is invalid"),
                )
            }
        };
        let observed = digest_file(&root.join(relative))?;
        if &observed != expected {
            return refusal(
                "RECEIPT_OUTPUT_DRIFT",
                format!("{key} expected {expected}, observed {observed}"),
            );
        }
    }
    let observed_subject = digest_named_outputs(&receipt.output_digests);
    if !receipt.output_digests.is_empty() && observed_subject != receipt.subject_digest {
        return refusal(
            "RECEIPT_SUBJECT_DIGEST_INVALID",
            format!(
                "receipt {} expected {}, recomputed {}",
                path.display(),
                receipt.subject_digest,
                observed_subject
            ),
        );
    }
    Ok(())
}

fn verify_evidence_file(
    source_path: &Path, corpus_path: &Path, evidence: &EvidenceFile,
) -> Result<()> {
    let relative = safe_relative(&evidence.path)?;
    let root = match evidence.repository.as_str() {
        "source" => source_path,
        "corpus" => corpus_path,
        other => {
            return refusal(
                "EVIDENCE_REPOSITORY_INVALID",
                format!("repository selector {other} is invalid"),
            )
        }
    };
    let observed = digest_file(&root.join(relative))?;
    if observed != evidence.blake3 {
        return refusal(
            "EVIDENCE_DIGEST_MISMATCH",
            format!(
                "{}:{} expected {}, observed {}",
                evidence.repository, evidence.path, evidence.blake3, observed
            ),
        );
    }
    Ok(())
}

fn require_repository_role(program: &WorkProgram, key: &str, expected: &str) -> Result<()> {
    match program.repositories.get(key) {
        Some(definition) if definition.role == expected => Ok(()),
        Some(definition) => refusal(
            "REPOSITORY_ROLE_INVALID",
            format!("{key} expected {expected}, observed {}", definition.role),
        ),
        None => refusal(
            "REPOSITORY_ROLE_MISSING",
            format!("repository role {key} is absent"),
        ),
    }
}

fn topological_order(by_id: &BTreeMap<String, &Workstream>) -> Result<Vec<String>> {
    fn visit(
        id: &str, by_id: &BTreeMap<String, &Workstream>, temporary: &mut BTreeSet<String>,
        permanent: &mut BTreeSet<String>, order: &mut Vec<String>,
    ) -> Result<()> {
        if permanent.contains(id) {
            return Ok(());
        }
        if !temporary.insert(id.to_string()) {
            return refusal(
                "WORKSTREAM_DEPENDENCY_CYCLE",
                format!("cycle includes {id}"),
            );
        }
        let workstream = by_id.get(id).ok_or_else(|| FoundryError::Refusal {
            code: "WORKSTREAM_DEPENDENCY_UNKNOWN".to_string(),
            message: id.to_string(),
        })?;
        for dependency in &workstream.dependencies {
            if !by_id.contains_key(dependency) {
                return refusal(
                    "WORKSTREAM_DEPENDENCY_UNKNOWN",
                    format!("{} depends on {dependency}", workstream.id),
                );
            }
            visit(dependency, by_id, temporary, permanent, order)?;
        }
        temporary.remove(id);
        permanent.insert(id.to_string());
        order.push(id.to_string());
        Ok(())
    }

    let mut order = Vec::new();
    let mut temporary = BTreeSet::new();
    let mut permanent = BTreeSet::new();
    for id in by_id.keys() {
        visit(id, by_id, &mut temporary, &mut permanent, &mut order)?;
    }
    Ok(order)
}

fn require_clean(snapshot: &RepositorySnapshot, code: &str) -> Result<()> {
    if snapshot.clean {
        Ok(())
    } else {
        refusal(
            code,
            format!("{} contains {:?}", snapshot.path, snapshot.dirty_entries),
        )
    }
}

fn make_receipt(
    receipt_type: &str, subject: &str, _subject_digest: String, source_head: &str,
    corpus_head: &str, input_digests: BTreeMap<String, String>,
    output_digests: BTreeMap<String, String>,
) -> Receipt {
    let subject_digest = digest_named_outputs(&output_digests);
    let run_id = subject_digest.chars().take(20).collect();
    Receipt {
        schema_version: RECEIPT_SCHEMA.to_string(),
        receipt_type: receipt_type.to_string(),
        subject: subject.to_string(),
        subject_digest,
        source_head: source_head.to_string(),
        corpus_head: corpus_head.to_string(),
        input_digests,
        output_digests,
        run_id,
    }
}

fn safe_relative(value: &str) -> Result<PathBuf> {
    let path = Path::new(value);
    if path.as_os_str().is_empty() || path.is_absolute() {
        return refusal(
            "PATH_NOT_RELATIVE",
            format!("path {value:?} is not a non-empty relative path"),
        );
    }
    for component in path.components() {
        if matches!(
            component,
            Component::ParentDir | Component::RootDir | Component::Prefix(_)
        ) {
            return refusal(
                "PATH_TRAVERSAL_REFUSED",
                format!("path {value:?} escapes its repository"),
            );
        }
    }
    Ok(path.to_path_buf())
}

fn sorted_files(root: &Path) -> Result<Vec<PathBuf>> {
    let mut files = Vec::new();
    for entry in WalkDir::new(root) {
        let entry = entry.map_err(|error| FoundryError::Refusal {
            code: "WALKDIR_FAILURE".to_string(),
            message: error.to_string(),
        })?;
        if entry.file_type().is_file() {
            files.push(entry.path().to_path_buf());
        }
    }
    files.sort();
    Ok(files)
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

pub fn digest_file(path: &Path) -> Result<String> {
    Ok(digest_bytes(&read(path)?))
}

pub fn digest_bytes(bytes: &[u8]) -> String {
    blake3::hash(bytes).to_hex().to_string()
}

pub fn digest_json<T: Serialize>(value: &T) -> Result<String> {
    Ok(digest_bytes(&serde_json::to_vec(value)?))
}

fn read(path: &Path) -> Result<Vec<u8>> {
    fs::read(path).map_err(|source| FoundryError::Io {
        path: path.display().to_string(),
        source,
    })
}

fn write_json<T: Serialize>(path: &Path, value: &T) -> Result<String> {
    let mut bytes = serde_json::to_vec_pretty(value)?;
    bytes.push(b'\n');
    write_bytes_exact(path, &bytes)?;
    Ok(digest_bytes(&bytes))
}

fn write_json_replace<T: Serialize>(path: &Path, value: &T) -> Result<String> {
    let mut bytes = serde_json::to_vec_pretty(value)?;
    bytes.push(b'\n');
    if let Some(parent) = path.parent() {
        fs::create_dir_all(parent).map_err(|source| FoundryError::Io {
            path: parent.display().to_string(),
            source,
        })?;
    }
    fs::write(path, &bytes).map_err(|source| FoundryError::Io {
        path: path.display().to_string(),
        source,
    })?;
    Ok(digest_bytes(&bytes))
}

fn write_bytes_exact(path: &Path, bytes: &[u8]) -> Result<()> {
    if let Some(parent) = path.parent() {
        fs::create_dir_all(parent).map_err(|source| FoundryError::Io {
            path: parent.display().to_string(),
            source,
        })?;
    }
    if path.exists() {
        let existing = read(path)?;
        if existing == bytes {
            return Ok(());
        }
        return refusal(
            "EXISTING_OUTPUT_DRIFT",
            format!("{} exists with different bytes", path.display()),
        );
    }
    fs::write(path, bytes).map_err(|source| FoundryError::Io {
        path: path.display().to_string(),
        source,
    })
}

fn git(repo: &Path, args: &[&str]) -> Result<String> {
    let bytes = git_bytes(repo, args)?;
    Ok(String::from_utf8_lossy(&bytes).trim().to_string())
}

fn git_optional(repo: &Path, args: &[&str]) -> Option<String> {
    let output = Command::new("git")
        .arg("-C")
        .arg(repo)
        .args(args)
        .output()
        .ok()?;
    if !output.status.success() {
        return None;
    }
    Some(String::from_utf8_lossy(&output.stdout).trim().to_string())
}

fn git_bytes(repo: &Path, args: &[&str]) -> Result<Vec<u8>> {
    let output = Command::new("git")
        .arg("-C")
        .arg(repo)
        .args(args)
        .output()
        .map_err(|source| FoundryError::Io {
            path: "git".to_string(),
            source,
        })?;
    if !output.status.success() {
        return Err(FoundryError::Git {
            repo: repo.display().to_string(),
            args: args.join(" "),
            stderr: String::from_utf8_lossy(&output.stderr).trim().to_string(),
        });
    }
    Ok(output.stdout)
}

fn refusal<T>(code: impl Into<String>, message: impl Into<String>) -> Result<T> {
    Err(FoundryError::Refusal {
        code: code.into(),
        message: message.into(),
    })
}
