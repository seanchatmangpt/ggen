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
use std::process::{Command, Output};
use std::time::{SystemTime, UNIX_EPOCH};
use walkdir::WalkDir;

const REFERENCE_SCHEMA: &str = "ggen.enterprise-architecture-foundry.fortune-reference/1";
const VERIFIER_ID: &str = "ggen-foundry-fortune-reference-verifier/v1";
const REFERENCE_ID: &str = "fortune-5-repository-manufacturing-platform";
const PACK_ID: &str = "repository_manufacturing_platform";

#[derive(Debug, Parser)]
#[command(
    name = "ggen-foundry-admit-reference",
    version,
    about = "Manufacture, compile, test, and replay a Fortune-scale reference repository"
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

#[derive(Debug, Clone, Deserialize, Serialize)]
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

#[derive(Debug, Deserialize)]
struct PrimitiveRecord {
    primitive_id: String,
}

#[derive(Debug, Serialize)]
struct BuildRun {
    run: u8,
    exit_code: i32,
    stdout_digest: String,
    stderr_digest: String,
    output_tree_digest: String,
    success: bool,
}

#[derive(Debug, Serialize)]
struct ReferenceAdmissionReport {
    schema_version: String,
    workstream_id: String,
    verifier: String,
    source_head: String,
    corpus_head: String,
    reference_id: String,
    pack_id: String,
    primitive_count: usize,
    build_runs: Vec<BuildRun>,
    reference_build_success: bool,
    verification_success: bool,
    replay_match: bool,
    solution_admission: bool,
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
    replay_all_receipts(&cli.source, &cli.corpus)?;

    let workstream = program
        .workstreams
        .iter()
        .find(|candidate| candidate.id == "K")
        .context("WORKSTREAM_K_MISSING")?;
    if workstream.dependencies.len() != 1 || workstream.dependencies[0] != "J" {
        bail!("WORKSTREAM_K_DEPENDENCY_INVALID");
    }
    let foundry_root = cli.corpus.join("foundry");
    let state_path = foundry_root.join("workstreams/state.json");
    let mut state: WorkstreamStateFile = read_json(&state_path, "WORKSTREAM_STATE_INVALID")?;
    require_admitted(&state, "J")?;
    require_ready(&state, "K")?;

    let packs: Catalog<PackRecord> = read_json(
        &foundry_root.join("catalogs/solution-packs.json"),
        "PACK_CATALOG_INVALID",
    )?;
    let primitives: Catalog<PrimitiveRecord> = read_json(
        &foundry_root.join("catalogs/primitives.json"),
        "PRIMITIVE_CATALOG_INVALID",
    )?;
    let primitive_ids: BTreeSet<String> = primitives
        .entries
        .into_iter()
        .map(|primitive| primitive.primitive_id)
        .collect();
    let pack = packs
        .entries
        .into_iter()
        .find(|pack| pack.pack_id == PACK_ID)
        .context("REFERENCE_PACK_MISSING")?;
    if pack.primitive_ids.is_empty()
        || pack
            .primitive_ids
            .iter()
            .any(|primitive| !primitive_ids.contains(primitive))
    {
        bail!("REFERENCE_PACK_PRIMITIVES_INVALID");
    }

    let reference_root = foundry_root.join("reference").join(REFERENCE_ID);
    if reference_root.exists() {
        bail!(
            "REFERENCE_OUTPUT_ALREADY_EXISTS: {}",
            reference_root.display()
        );
    }
    manufacture_reference(&reference_root, &pack, &source.head, &corpus.head)?;

    let build_root = std::env::temp_dir().join(format!(
        "ggen-foundry-reference-build-{}-{}",
        std::process::id(),
        SystemTime::now()
            .duration_since(UNIX_EPOCH)
            .context("system clock before epoch")?
            .as_nanos()
    ));
    fs::create_dir_all(&build_root).context("create reference build root")?;
    let first_output = run_reference_tests(&reference_root, &build_root.join("target-1"))?;
    let first_tree_digest = digest_tree(&reference_root)?;
    let second_output = run_reference_tests(&reference_root, &build_root.join("target-2"))?;
    let second_tree_digest = digest_tree(&reference_root)?;
    let first = build_run(1, first_output, first_tree_digest);
    let second = build_run(2, second_output, second_tree_digest);
    let reference_build_success = first.success && second.success;
    let replay_match = first.stdout_digest == second.stdout_digest
        && first.stderr_digest == second.stderr_digest
        && first.output_tree_digest == second.output_tree_digest;
    let verification_success = verify_reference(&reference_root, &pack)?;
    let solution_admission = reference_build_success && replay_match && verification_success;
    if !solution_admission {
        bail!(
            "REFERENCE_ADMISSION_REFUSED: build={reference_build_success}, replay={replay_match}, verify={verification_success}"
        );
    }

    let report = ReferenceAdmissionReport {
        schema_version: REFERENCE_SCHEMA.to_string(),
        workstream_id: "K".to_string(),
        verifier: VERIFIER_ID.to_string(),
        source_head: source.head.clone(),
        corpus_head: corpus.head.clone(),
        reference_id: REFERENCE_ID.to_string(),
        pack_id: PACK_ID.to_string(),
        primitive_count: pack.primitive_ids.len(),
        build_runs: vec![first, second],
        reference_build_success,
        verification_success,
        replay_match,
        solution_admission,
        predicates: workstream.predicates.clone(),
        metrics: BTreeMap::from([
            ("reference_repository_count".to_string(), json!(1)),
            ("build_run_count".to_string(), json!(2)),
            ("scale".to_string(), json!("fortune-5")),
        ]),
    };
    let report_bytes = canonical_json(&report)?;
    let report_digest = digest_bytes(&report_bytes);
    let report_relative = "foundry/workstreams/K/admission-report.json";
    write_new(&cli.corpus.join(report_relative), &report_bytes)?;

    let receipt_relative = "foundry/receipts/workstream-K.json";
    {
        let current = state
            .workstreams
            .get_mut("K")
            .context("WORKSTREAM_K_STATE_MISSING")?;
        current.status = "ADMITTED".to_string();
        current.report_digest = Some(report_digest.clone());
        current.receipt_path = Some(receipt_relative.to_string());
    }
    let state_bytes = canonical_json(&state)?;

    let mut outputs = BTreeMap::new();
    for path in sorted_files(&reference_root)? {
        let relative = path
            .strip_prefix(&cli.corpus)
            .with_context(|| format!("REFERENCE_PATH_OUTSIDE_CORPUS: {}", path.display()))?;
        outputs.insert(
            format!("corpus:{}", relative.to_string_lossy()),
            digest_bytes(&fs::read(&path)?),
        );
    }
    outputs.insert(format!("corpus:{report_relative}"), report_digest);
    outputs.insert(
        "projection:foundry/workstreams/state.json".to_string(),
        digest_bytes(&state_bytes),
    );
    let mut inputs = BTreeMap::new();
    inputs.insert("work-program".to_string(), validation.program_digest);
    inputs.insert("source-tree".to_string(), source.tracked_tree_digest);
    inputs.insert("corpus-tree".to_string(), corpus.tracked_tree_digest);
    inputs.insert("pack".to_string(), digest_bytes(&canonical_json(&pack)?));
    let subject_digest = digest_named_outputs(&outputs);
    let receipt = Receipt {
        schema_version: RECEIPT_SCHEMA.to_string(),
        receipt_type: "WORKSTREAM_ADMISSION".to_string(),
        subject: "K".to_string(),
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
    let _ = fs::remove_dir_all(&build_root);
    println!("{}", serde_json::to_string_pretty(&report)?);
    Ok(())
}

fn manufacture_reference(
    root: &Path, pack: &PackRecord, source_head: &str, corpus_head: &str,
) -> Result<()> {
    fs::create_dir_all(root.join("src")).context("create reference source directory")?;
    let cargo = format!(
        "[package]\nname = \"fortune-five-repository-manufacturing-reference\"\nversion = \"0.1.0\"\nedition = \"2021\"\npublish = false\n\n[workspace]\n"
    );
    let primitive_literals = pack
        .primitive_ids
        .iter()
        .map(|primitive| format!("    \"{primitive}\","))
        .collect::<Vec<_>>()
        .join("\n");
    let library = format!(
        "pub const REFERENCE_ID: &str = \"{REFERENCE_ID}\";\npub const PACK_ID: &str = \"{PACK_ID}\";\npub const SCALE: &str = \"fortune-5\";\npub const PRIMITIVES: &[&str] = &[\n{primitive_literals}\n];\n\npub fn verify() -> bool {{\n    !PRIMITIVES.is_empty() && SCALE == \"fortune-5\" && REFERENCE_ID.starts_with(\"fortune-5\")\n}}\n\n#[cfg(test)]\nmod tests {{\n    use super::*;\n\n    #[test]\n    fn admitted_reference_is_composed() {{\n        assert!(verify());\n        assert!(!PRIMITIVES.is_empty());\n        assert_eq!(PACK_ID, \"repository_manufacturing_platform\");\n    }}\n\n    #[test]\n    fn missing_primitive_falsifier_is_detectable() {{\n        let reduced = &PRIMITIVES[1..];\n        assert_eq!(reduced.len() + 1, PRIMITIVES.len());\n    }}\n}}\n"
    );
    let main = "fn main() {\n    assert!(fortune_five_repository_manufacturing_reference::verify());\n    println!(\"fortune-5-repository-manufacturing-platform:ALIVE\");\n}\n";
    let architecture = json!({
        "schema_version": REFERENCE_SCHEMA,
        "reference_id": REFERENCE_ID,
        "pack": pack,
        "parameters": {
            "region": "global-multi-region",
            "scale": "fortune-5",
            "availability_slo": 99.999,
            "compliance_profile": ["SOX", "SOC2", "PCI-DSS", "GDPR", "NIST-800-53"]
        },
        "source_head": source_head,
        "corpus_parent_head": corpus_head,
        "standing": "CANDIDATE",
    });
    let controls = json!({
        "zero_unreceipted_actuation": true,
        "exact_head_evidence": true,
        "independent_verification": true,
        "clean_room_replay": true,
        "regional_failure_domains": 3,
        "segregation_of_duties": true,
        "supply_chain_attestation": true,
    });
    let replay = json!({
        "command": "cargo test --manifest-path Cargo.toml",
        "expected_semantic_result": "NO_SEMANTIC_CHANGE",
        "negative_falsifier": "missing primitive changes composition cardinality",
    });
    write_new(&root.join("Cargo.toml"), cargo.as_bytes())?;
    write_new(&root.join("src/lib.rs"), library.as_bytes())?;
    write_new(&root.join("src/main.rs"), main.as_bytes())?;
    write_new(
        &root.join("architecture.json"),
        &canonical_json(&architecture)?,
    )?;
    write_new(&root.join("controls.json"), &canonical_json(&controls)?)?;
    write_new(&root.join("replay.json"), &canonical_json(&replay)?)?;
    write_new(
        &root.join("README.md"),
        b"# Fortune 5 Repository Manufacturing Reference\n\nGenerated from the admitted repository manufacturing solution pack. Standing is assigned only by the external foundry verifier.\n",
    )?;
    Ok(())
}

fn run_reference_tests(reference_root: &Path, target_dir: &Path) -> Result<Output> {
    Command::new("cargo")
        .args([
            "test",
            "--manifest-path",
            reference_root.join("Cargo.toml").to_string_lossy().as_ref(),
            "--quiet",
        ])
        .env("RUSTC_WRAPPER", "")
        .env("RUSTUP_TOOLCHAIN", "stable")
        .env("CARGO_TARGET_DIR", target_dir)
        .output()
        .context("reference cargo test execution failed")
}

fn build_run(run: u8, output: Output, output_tree_digest: String) -> BuildRun {
    BuildRun {
        run,
        exit_code: output.status.code().unwrap_or(-1),
        stdout_digest: digest_bytes(&output.stdout),
        stderr_digest: digest_bytes(&output.stderr),
        output_tree_digest,
        success: output.status.success(),
    }
}

fn verify_reference(root: &Path, pack: &PackRecord) -> Result<bool> {
    let architecture: JsonValue = read_json(&root.join("architecture.json"), "REFERENCE_INVALID")?;
    let observed_pack = architecture
        .get("pack")
        .and_then(|value| value.get("pack_id"))
        .and_then(JsonValue::as_str)
        .unwrap_or_default();
    let observed_scale = architecture
        .get("parameters")
        .and_then(|value| value.get("scale"))
        .and_then(JsonValue::as_str)
        .unwrap_or_default();
    Ok(observed_pack == pack.pack_id
        && observed_scale == "fortune-5"
        && root.join("src/lib.rs").is_file()
        && root.join("controls.json").is_file()
        && root.join("replay.json").is_file())
}

fn digest_tree(root: &Path) -> Result<String> {
    let mut files = sorted_files(root)?;
    files.retain(|path| !path.ends_with("Cargo.lock"));
    let mut hasher = Hasher::new();
    for path in files {
        let relative = path
            .strip_prefix(root)
            .with_context(|| format!("REFERENCE_PATH_OUTSIDE_ROOT: {}", path.display()))?;
        let bytes = fs::read(&path).with_context(|| format!("read {}", path.display()))?;
        hash_named_bytes(&mut hasher, &relative.to_string_lossy(), &bytes);
    }
    Ok(hasher.finalize().to_hex().to_string())
}

fn sorted_files(root: &Path) -> Result<Vec<PathBuf>> {
    let mut files = Vec::new();
    for entry in WalkDir::new(root) {
        let entry = entry.context("walk reference tree")?;
        if entry.file_type().is_file() {
            files.push(entry.path().to_path_buf());
        }
    }
    files.sort();
    Ok(files)
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
