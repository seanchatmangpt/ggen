use anyhow::{bail, Context, Result};
use serde::{Deserialize, Serialize};
use std::collections::BTreeSet;
use std::env;
use std::fs;
use std::path::{Path, PathBuf};
use std::process::Command;
use walkdir::WalkDir;

const VERSION: &str = "26.8.1";
const DOC_ROOT: &str = "docs/v26.8.1";
const MANIFEST_PATH: &str = "docs/v26.8.1/manifest.toml";
const COVERAGE_PATH: &str = "docs/v26.8.1/coverage-matrix.csv";
const EVIDENCE_ROOT: &str = ".ggen/v26.8.1";

#[derive(Debug, Deserialize)]
struct CorpusManifest {
    version: String,
    baseline_ref: String,
    required_document_count: usize,
    sunset_blocked_by_unknown: bool,
    validation: ValidationPolicy,
    standing: AllowedValues,
    disposition: AllowedValues,
    baseline: BaselineFacts,
}

#[derive(Debug, Deserialize)]
struct ValidationPolicy {
    minimum_documents: usize,
    require_unique_paths: bool,
    require_standing: bool,
    require_legacy_disposition: bool,
    require_verifier_assignment: bool,
    require_authority_mapping: bool,
    require_implementation_mapping: bool,
    require_zero_unknown_for_sunset: bool,
}

#[derive(Debug, Deserialize)]
struct AllowedValues {
    allowed: Vec<String>,
}

#[derive(Debug, Deserialize)]
struct BaselineFacts {
    repository: String,
    workspace_version: String,
    workspace_packages: usize,
    pipeline: Vec<String>,
}

#[derive(Debug, Deserialize, Serialize, Clone)]
struct CoverageRow {
    subsystem: String,
    authority_sources: String,
    implementation_sources: String,
    verifier: String,
    legacy_disposition: String,
    standing: String,
}

#[derive(Debug, Serialize)]
struct FileObservation {
    path: String,
    bytes: u64,
    blake3: String,
    classification: String,
}

#[derive(Debug, Serialize)]
struct WorkspaceObservation {
    workspace_version: String,
    package_count: usize,
    members: Vec<String>,
    command_files: Vec<String>,
    diagnostic_codes: Vec<String>,
    generated_surfaces: Vec<String>,
    legacy_references: Vec<String>,
}

#[derive(Debug, Serialize)]
struct Finding {
    code: String,
    severity: String,
    path: Option<String>,
    message: String,
}

#[derive(Debug, Serialize)]
struct GateResult {
    id: String,
    pass: bool,
    evidence: Vec<String>,
}

#[derive(Debug, Serialize)]
struct CrownReport {
    schema_version: String,
    release: String,
    source_head: String,
    baseline_ref: String,
    repository: String,
    standing: String,
    release_admitted: bool,
    sunset_admitted: bool,
    corpus_document_count: usize,
    coverage_row_count: usize,
    unknown_standing_count: usize,
    unknown_disposition_count: usize,
    observation_digest: String,
    gates: Vec<GateResult>,
    findings: Vec<Finding>,
    workspace: WorkspaceObservation,
    files: Vec<FileObservation>,
}

#[derive(Debug, Serialize)]
struct CrownReceipt {
    schema_version: String,
    release: String,
    source_head: String,
    report_path: String,
    report_blake3: String,
    observation_blake3: String,
    release_admitted: bool,
    sunset_admitted: bool,
}

fn main() {
    if let Err(error) = run() {
        eprintln!("v26.8.1 verifier refused: {error:#}");
        std::process::exit(2);
    }
}

fn run() -> Result<()> {
    let args: Vec<String> = env::args().skip(1).collect();
    let root = resolve_root(&args)?;
    let strict = !args.iter().any(|arg| arg == "--observe-only");

    let manifest = load_manifest(&root)?;
    let source_head = exact_head(&root);
    let documents = observe_documents(&root)?;
    let coverage = load_coverage(&root)?;
    let workspace = observe_workspace(&root)?;
    let files = observe_authority_files(&root)?;

    let mut findings = Vec::new();
    let mut gates = Vec::new();

    validate_manifest(&manifest, &mut findings);
    validate_documents(&manifest, &documents, &root, &mut findings, &mut gates)?;
    validate_coverage(&manifest, &coverage, &mut findings, &mut gates);
    validate_workspace(&manifest, &workspace, &mut findings, &mut gates);
    validate_authority_files(&files, &mut findings, &mut gates);

    let unknown_standing_count = coverage
        .iter()
        .filter(|row| row.standing.trim() == "UNKNOWN")
        .count();
    let unknown_disposition_count = coverage
        .iter()
        .filter(|row| row.legacy_disposition.trim() == "UNKNOWN")
        .count();
    let hard_failures = findings
        .iter()
        .filter(|finding| finding.severity == "ERROR")
        .count();
    let release_admitted = hard_failures == 0 && unknown_standing_count == 0;
    let zero_unknown_required =
        manifest.sunset_blocked_by_unknown || manifest.validation.require_zero_unknown_for_sunset;
    let sunset_admitted = release_admitted
        && unknown_disposition_count == 0
        && (!zero_unknown_required
            || (unknown_standing_count == 0 && unknown_disposition_count == 0));
    let standing = if sunset_admitted {
        "ALIVE"
    } else if hard_failures == 0 {
        "PARTIAL_ALIVE"
    } else {
        "BUILD_BROKEN"
    };

    let observation_bytes =
        serde_json::to_vec_pretty(&(documents.clone(), &coverage, &workspace, &files))?;
    let observation_digest = blake3::hash(&observation_bytes).to_hex().to_string();

    let report = CrownReport {
        schema_version: "ggen.v26.8.1.verifier-report/1".into(),
        release: VERSION.into(),
        source_head: source_head.clone(),
        baseline_ref: manifest.baseline_ref.clone(),
        repository: manifest.baseline.repository.clone(),
        standing: standing.into(),
        release_admitted,
        sunset_admitted,
        corpus_document_count: documents.len(),
        coverage_row_count: coverage.len(),
        unknown_standing_count,
        unknown_disposition_count,
        observation_digest: observation_digest.clone(),
        gates,
        findings,
        workspace,
        files,
    };

    let evidence_root = root.join(EVIDENCE_ROOT);
    fs::create_dir_all(&evidence_root)?;
    let report_path = evidence_root.join("verifier-report.json");
    let observation_path = evidence_root.join("observation.json");
    fs::write(&observation_path, observation_bytes)?;
    let report_bytes = serde_json::to_vec_pretty(&report)?;
    fs::write(&report_path, &report_bytes)?;

    let receipt = CrownReceipt {
        schema_version: "ggen.v26.8.1.crown-receipt/1".into(),
        release: VERSION.into(),
        source_head,
        report_path: relative(&root, &report_path),
        report_blake3: blake3::hash(&report_bytes).to_hex().to_string(),
        observation_blake3: observation_digest,
        release_admitted,
        sunset_admitted,
    };
    fs::write(
        evidence_root.join("receipt.json"),
        serde_json::to_vec_pretty(&receipt)?,
    )?;

    println!("standing={standing}");
    println!("release_admitted={release_admitted}");
    println!("sunset_admitted={sunset_admitted}");
    println!("report={}", relative(&root, &report_path));

    if strict && !release_admitted {
        bail!(
            "release admission refused; inspect {}",
            relative(&root, &report_path)
        );
    }
    Ok(())
}

fn resolve_root(args: &[String]) -> Result<PathBuf> {
    let explicit = args
        .windows(2)
        .find(|pair| pair[0] == "--root")
        .map(|pair| PathBuf::from(&pair[1]));
    let mut current = explicit.unwrap_or(env::current_dir()?);
    loop {
        if current.join("Cargo.toml").is_file() && current.join("AGENTS.md").is_file() {
            return current
                .canonicalize()
                .context("canonicalize repository root");
        }
        if !current.pop() {
            bail!("repository root not found; pass --root <path>");
        }
    }
}

fn load_manifest(root: &Path) -> Result<CorpusManifest> {
    let text = fs::read_to_string(root.join(MANIFEST_PATH))?;
    toml::from_str(&text).context("parse v26.8.1 manifest")
}

fn exact_head(root: &Path) -> String {
    Command::new("git")
        .args(["rev-parse", "HEAD"])
        .current_dir(root)
        .output()
        .ok()
        .filter(|output| output.status.success())
        .map(|output| String::from_utf8_lossy(&output.stdout).trim().to_owned())
        .unwrap_or_else(|| "UNKNOWN".into())
}

fn observe_documents(root: &Path) -> Result<Vec<String>> {
    let mut documents = Vec::new();
    for entry in WalkDir::new(root.join(DOC_ROOT)) {
        let entry = entry?;
        if !entry.file_type().is_file() {
            continue;
        }
        let path = entry.path();
        let name = path
            .file_name()
            .and_then(|value| value.to_str())
            .unwrap_or_default();
        if path.extension().and_then(|value| value.to_str()) == Some("md") && name != "README.md" {
            documents.push(relative(root, path));
        }
    }
    documents.sort();
    Ok(documents)
}

fn load_coverage(root: &Path) -> Result<Vec<CoverageRow>> {
    let mut reader = csv::Reader::from_path(root.join(COVERAGE_PATH))?;
    reader
        .deserialize()
        .collect::<std::result::Result<Vec<_>, _>>()
        .context("parse coverage matrix")
}

fn observe_workspace(root: &Path) -> Result<WorkspaceObservation> {
    let cargo_text = fs::read_to_string(root.join("Cargo.toml"))?;
    let cargo: toml::Value = toml::from_str(&cargo_text)?;
    let workspace = cargo.get("workspace");
    let workspace_version = workspace
        .and_then(|value| value.get("package"))
        .and_then(|value| value.get("version"))
        .and_then(toml::Value::as_str)
        .unwrap_or("UNKNOWN")
        .to_owned();
    let members = workspace
        .and_then(|value| value.get("members"))
        .and_then(toml::Value::as_array)
        .map(|items| {
            items
                .iter()
                .filter_map(toml::Value::as_str)
                .map(str::to_owned)
                .collect::<Vec<String>>()
        })
        .unwrap_or_default();

    let command_files = matching_paths(
        root,
        &["crates/ggen-cli/src/cmds", "crates/ggen-engine/src/verbs"],
        "rs",
    )?;
    let diagnostic_codes = scan_tokens(root, "crates/ggen-lsp/src", &["GGEN-", "LAW-", "PACK-"])?;
    let generated_surfaces =
        scan_paths_containing(root, &["GENERATED", "DO NOT EDIT", "@generated"])?;
    let legacy_references =
        scan_paths_containing(root, &["ggen-legacy", "ggen_core", "ggen-core"])?;

    Ok(WorkspaceObservation {
        workspace_version,
        package_count: members.len() + 1,
        members,
        command_files,
        diagnostic_codes,
        generated_surfaces,
        legacy_references,
    })
}

fn observe_authority_files(root: &Path) -> Result<Vec<FileObservation>> {
    let authority = [
        "AGENTS.md",
        "CLAUDE.md",
        "Cargo.toml",
        "Cargo.lock",
        "justfile",
        "rust-toolchain.toml",
        MANIFEST_PATH,
        COVERAGE_PATH,
    ];
    let mut observations = Vec::new();
    for path in authority {
        let full = root.join(path);
        if full.is_file() {
            let bytes = fs::read(&full)?;
            observations.push(FileObservation {
                path: path.into(),
                bytes: bytes.len() as u64,
                blake3: blake3::hash(&bytes).to_hex().to_string(),
                classification: "authority".into(),
            });
        }
    }
    Ok(observations)
}

fn validate_manifest(manifest: &CorpusManifest, findings: &mut Vec<Finding>) {
    if manifest.version != VERSION {
        error(
            findings,
            "MANIFEST_VERSION",
            Some(MANIFEST_PATH),
            format!("expected {VERSION}, found {}", manifest.version),
        );
    }
    if manifest.required_document_count < manifest.validation.minimum_documents {
        error(
            findings,
            "DOCUMENT_FLOOR",
            Some(MANIFEST_PATH),
            "required_document_count is below minimum_documents",
        );
    }
    let expected: Vec<String> = ["Resolve", "Enrich", "Extract", "Render", "Write", "Receipt"]
        .into_iter()
        .map(str::to_owned)
        .collect();
    if manifest.baseline.pipeline != expected {
        error(
            findings,
            "PIPELINE_ORDER",
            Some(MANIFEST_PATH),
            "canonical pipeline order changed",
        );
    }
}

fn validate_documents(
    manifest: &CorpusManifest,
    documents: &[String],
    root: &Path,
    findings: &mut Vec<Finding>,
    gates: &mut Vec<GateResult>,
) -> Result<()> {
    let unique: BTreeSet<_> = documents.iter().collect();
    let count_pass = documents.len() >= manifest.required_document_count
        && documents.len() >= manifest.validation.minimum_documents;
    if !count_pass {
        error(
            findings,
            "DOCUMENT_COUNT",
            Some(DOC_ROOT),
            format!(
                "found {}, require {}",
                documents.len(),
                manifest.required_document_count
            ),
        );
    }
    if manifest.validation.require_unique_paths && unique.len() != documents.len() {
        error(
            findings,
            "DUPLICATE_DOCUMENT_PATH",
            Some(DOC_ROOT),
            "document paths are not unique",
        );
    }

    let required_sections = ["authority", "implementation", "verification", "legacy"];
    let mut incomplete = Vec::new();
    for document in documents {
        let text = fs::read_to_string(root.join(document))?;
        let lowercase = text.to_ascii_lowercase();
        for section in required_sections {
            if !lowercase.contains(section) {
                incomplete.push(format!("{document}:{section}"));
            }
        }
        if text.contains("TODO") || text.contains("FIXME") {
            error(
                findings,
                "FORBIDDEN_PLACEHOLDER",
                Some(document),
                "document contains TODO or FIXME",
            );
        }
    }
    if !incomplete.is_empty() {
        error(
            findings,
            "DOCUMENT_SECTION_LOSS",
            Some(DOC_ROOT),
            format!("{} required section mappings absent", incomplete.len()),
        );
    }
    gates.push(GateResult {
        id: "corpus-structure".into(),
        pass: count_pass && incomplete.is_empty(),
        evidence: vec![
            format!("documents={}", documents.len()),
            format!("incomplete_sections={}", incomplete.len()),
        ],
    });
    Ok(())
}

fn validate_coverage(
    manifest: &CorpusManifest,
    coverage: &[CoverageRow],
    findings: &mut Vec<Finding>,
    gates: &mut Vec<GateResult>,
) {
    let allowed_standing: BTreeSet<_> = manifest
        .standing
        .allowed
        .iter()
        .map(String::as_str)
        .collect();
    let allowed_disposition: BTreeSet<_> = manifest
        .disposition
        .allowed
        .iter()
        .map(String::as_str)
        .collect();
    let mut invalid = 0usize;
    let mut unmapped = 0usize;
    for row in coverage {
        if row.subsystem.trim().is_empty() {
            invalid += 1;
        }
        if manifest.validation.require_authority_mapping && row.authority_sources.trim().is_empty()
        {
            unmapped += 1;
        }
        if manifest.validation.require_implementation_mapping
            && row.implementation_sources.trim().is_empty()
        {
            unmapped += 1;
        }
        if manifest.validation.require_verifier_assignment && row.verifier.trim().is_empty() {
            unmapped += 1;
        }
        if manifest.validation.require_standing && !allowed_standing.contains(row.standing.trim()) {
            invalid += 1;
        }
        if manifest.validation.require_legacy_disposition
            && !allowed_disposition.contains(row.legacy_disposition.trim())
        {
            invalid += 1;
        }
    }
    if invalid > 0 {
        error(
            findings,
            "INVALID_COVERAGE_VALUE",
            Some(COVERAGE_PATH),
            format!("{invalid} invalid coverage values"),
        );
    }
    if unmapped > 0 {
        error(
            findings,
            "UNMAPPED_COVERAGE",
            Some(COVERAGE_PATH),
            format!("{unmapped} required mappings absent"),
        );
    }
    gates.push(GateResult {
        id: "coverage-schema".into(),
        pass: invalid == 0 && unmapped == 0,
        evidence: vec![
            format!("rows={}", coverage.len()),
            format!("invalid={invalid}"),
            format!("unmapped={unmapped}"),
        ],
    });
}

fn validate_workspace(
    manifest: &CorpusManifest,
    workspace: &WorkspaceObservation,
    findings: &mut Vec<Finding>,
    gates: &mut Vec<GateResult>,
) {
    if workspace.workspace_version != manifest.baseline.workspace_version {
        warning(
            findings,
            "WORKSPACE_VERSION_DRIFT",
            Some("Cargo.toml"),
            format!(
                "manifest baseline={} observed={}",
                manifest.baseline.workspace_version, workspace.workspace_version
            ),
        );
    }
    if workspace.package_count != manifest.baseline.workspace_packages {
        warning(
            findings,
            "WORKSPACE_PACKAGE_DRIFT",
            Some("Cargo.toml"),
            format!(
                "manifest baseline={} observed={}",
                manifest.baseline.workspace_packages, workspace.package_count
            ),
        );
    }
    if workspace.command_files.is_empty() {
        error(
            findings,
            "COMMAND_SURFACE_ABSENT",
            Some("crates/ggen-cli/src"),
            "no command implementation files observed",
        );
    }
    gates.push(GateResult {
        id: "live-repository-observation".into(),
        pass: !workspace.command_files.is_empty(),
        evidence: vec![
            format!("packages={}", workspace.package_count),
            format!("command_files={}", workspace.command_files.len()),
            format!("diagnostic_codes={}", workspace.diagnostic_codes.len()),
            format!("legacy_references={}", workspace.legacy_references.len()),
        ],
    });
}

fn validate_authority_files(
    files: &[FileObservation],
    findings: &mut Vec<Finding>,
    gates: &mut Vec<GateResult>,
) {
    let required = [
        "AGENTS.md",
        "CLAUDE.md",
        "Cargo.toml",
        "Cargo.lock",
        "justfile",
        "rust-toolchain.toml",
    ];
    let observed: BTreeSet<_> = files.iter().map(|file| file.path.as_str()).collect();
    let missing: Vec<_> = required
        .iter()
        .filter(|path| !observed.contains(**path))
        .copied()
        .collect();
    if !missing.is_empty() {
        error(
            findings,
            "AUTHORITY_FILE_MISSING",
            None,
            format!("missing authority files: {}", missing.join(", ")),
        );
    }
    gates.push(GateResult {
        id: "authority-hash-inventory".into(),
        pass: missing.is_empty(),
        evidence: files
            .iter()
            .map(|file| format!("{}={}", file.path, file.blake3))
            .collect(),
    });
}

fn matching_paths(root: &Path, roots: &[&str], extension: &str) -> Result<Vec<String>> {
    let mut paths = Vec::new();
    for subroot in roots {
        let full = root.join(subroot);
        if !full.exists() {
            continue;
        }
        for entry in WalkDir::new(full) {
            let entry = entry?;
            if entry.file_type().is_file()
                && entry.path().extension().and_then(|value| value.to_str()) == Some(extension)
            {
                paths.push(relative(root, entry.path()));
            }
        }
    }
    paths.sort();
    Ok(paths)
}

fn scan_tokens(root: &Path, subroot: &str, prefixes: &[&str]) -> Result<Vec<String>> {
    let mut tokens = BTreeSet::new();
    let full = root.join(subroot);
    if !full.exists() {
        return Ok(Vec::new());
    }
    for entry in WalkDir::new(full) {
        let entry = entry?;
        if !entry.file_type().is_file() {
            continue;
        }
        let text = fs::read_to_string(entry.path()).unwrap_or_default();
        for word in text.split(|character: char| {
            !(character.is_ascii_alphanumeric() || character == '-' || character == '_')
        }) {
            if prefixes.iter().any(|prefix| word.starts_with(prefix)) && word.len() > 5 {
                tokens.insert(word.trim_matches('-').to_owned());
            }
        }
    }
    Ok(tokens.into_iter().collect())
}

fn scan_paths_containing(root: &Path, terms: &[&str]) -> Result<Vec<String>> {
    let mut paths = BTreeSet::new();
    for entry in WalkDir::new(root)
        .into_iter()
        .filter_entry(|entry| !ignored(entry.path()))
    {
        let entry = entry?;
        if !entry.file_type().is_file() || entry.metadata()?.len() > 2_000_000 {
            continue;
        }
        let text = fs::read_to_string(entry.path()).unwrap_or_default();
        if terms.iter().any(|term| text.contains(term)) {
            paths.insert(relative(root, entry.path()));
        }
    }
    Ok(paths.into_iter().collect())
}

fn ignored(path: &Path) -> bool {
    path.components().any(|component| {
        matches!(
            component.as_os_str().to_str(),
            Some(".git" | "target" | "node_modules" | ".ggen")
        )
    })
}

fn relative(root: &Path, path: &Path) -> String {
    path.strip_prefix(root)
        .unwrap_or(path)
        .to_string_lossy()
        .replace('\\', "/")
}

fn error(findings: &mut Vec<Finding>, code: &str, path: Option<&str>, message: impl Into<String>) {
    findings.push(Finding {
        code: code.into(),
        severity: "ERROR".into(),
        path: path.map(str::to_owned),
        message: message.into(),
    });
}

fn warning(
    findings: &mut Vec<Finding>, code: &str, path: Option<&str>, message: impl Into<String>,
) {
    findings.push(Finding {
        code: code.into(),
        severity: "WARNING".into(),
        path: path.map(str::to_owned),
        message: message.into(),
    });
}
