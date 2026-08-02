//! Solution Building Block capability-density commands (`ggen sbb <verb>`).
//!
//! One density unit is one unique Git commit with a complete, digest-bound
//! manufacturing and falsification evidence chain. This evaluator reports
//! evidence but never promotes its own output beyond `PARTIAL_ALIVE`.

use std::{
    collections::{BTreeMap, BTreeSet},
    fs,
    path::{Component, Path, PathBuf},
    process::Command,
};

use clap_noun_verb::{NounVerbError, Result};
use clap_noun_verb_macros::verb;
use serde::{Deserialize, Serialize};
use serde_json::{json, Value};

const MANIFEST_SCHEMA: &str = "ggen.sbb.capability-manifest.v1";
const REPORT_SCHEMA: &str = "ggen.sbb.capability-density-report.v1";
const RECEIPT_SCHEMA: &str = "ggen.sbb.capability-density-receipt.v1";
const AXES: [&str; 7] = [
    "ontology_modules",
    "textual_forms",
    "audiences",
    "languages",
    "jurisdictions",
    "organization_profiles",
    "runtimes",
];
const CHAIN: [&str; 10] = [
    "ontology",
    "shacl",
    "sparql",
    "typestate",
    "template",
    "artifact",
    "runtime_surface",
    "walkthrough",
    "receipt",
    "replay",
];

#[derive(Debug, Clone, Deserialize, Serialize)]
struct Manifest {
    schema: String,
    sbb: Sbb,
    repository: Repository,
    distribution: BTreeMap<String, Vec<String>>,
    deltas: Vec<Delta>,
}

#[derive(Debug, Clone, Deserialize, Serialize)]
struct Sbb {
    id: String,
    version: String,
    architecture_contract: String,
    minimum_commit_equivalent_units: u64,
}

#[derive(Debug, Clone, Deserialize, Serialize)]
struct Repository {
    root: String,
}

#[derive(Debug, Clone, Deserialize, Serialize)]
struct Delta {
    id: String,
    commit: String,
    capability_iri: String,
    family: String,
    summary: String,
    ontology_modules: Vec<String>,
    textual_forms: Vec<String>,
    chain: BTreeMap<String, Evidence>,
    positive_witness: Evidence,
    negative_fixture: Evidence,
    adversarial_falsifier: Evidence,
    verifier: Evidence,
}

#[derive(Debug, Clone, Deserialize, Serialize)]
struct Evidence {
    locator: String,
    digest: String,
}

#[derive(Debug, Clone, Deserialize, Serialize)]
struct DeltaReport {
    id: String,
    commit: String,
    observed: bool,
    violations: Vec<String>,
}

#[derive(Debug, Clone, Deserialize, Serialize)]
struct Report {
    schema: String,
    manifest_digest: String,
    sbb: Sbb,
    standing: String,
    claim_ceiling: String,
    target_met: bool,
    eligible_for_external_admission: bool,
    declared_deltas: usize,
    commit_equivalent_units: usize,
    duplicate_commit_collisions: usize,
    axes: BTreeMap<String, usize>,
    distribution_contexts: String,
    delivered_capability_instances: String,
    deltas: Vec<DeltaReport>,
    violations: Vec<String>,
    report_digest: String,
}

#[derive(Serialize)]
struct ReportBody<'a> {
    schema: &'static str,
    manifest_digest: &'a str,
    sbb: &'a Sbb,
    standing: &'a str,
    claim_ceiling: &'a str,
    target_met: bool,
    eligible_for_external_admission: bool,
    declared_deltas: usize,
    commit_equivalent_units: usize,
    duplicate_commit_collisions: usize,
    axes: &'a BTreeMap<String, usize>,
    distribution_contexts: &'a str,
    delivered_capability_instances: &'a str,
    deltas: &'a [DeltaReport],
    violations: &'a [String],
}

#[derive(Debug, Clone, Deserialize, Serialize)]
struct Receipt {
    schema: String,
    operation: String,
    manifest_digest: String,
    report_digest: String,
    previous_digest: String,
    artifacts: Vec<String>,
    digest_algorithm: String,
    digest: String,
}

#[derive(Serialize)]
struct ReceiptBody<'a> {
    schema: &'static str,
    operation: &'a str,
    manifest_digest: &'a str,
    report_digest: &'a str,
    previous_digest: &'a str,
    artifacts: &'a [String],
}

fn digest_json<T: Serialize>(value: &T) -> Result<String> {
    let bytes = serde_json::to_vec(value).map_err(|error| {
        NounVerbError::execution_error(format!("cannot serialize SBB evidence: {error}"))
    })?;
    Ok(blake3::hash(&bytes).to_hex().to_string())
}

fn digest_bytes(bytes: &[u8]) -> String {
    blake3::hash(bytes).to_hex().to_string()
}

fn report_digest(report: &Report) -> Result<String> {
    digest_json(&ReportBody {
        schema: REPORT_SCHEMA,
        manifest_digest: &report.manifest_digest,
        sbb: &report.sbb,
        standing: &report.standing,
        claim_ceiling: &report.claim_ceiling,
        target_met: report.target_met,
        eligible_for_external_admission: report.eligible_for_external_admission,
        declared_deltas: report.declared_deltas,
        commit_equivalent_units: report.commit_equivalent_units,
        duplicate_commit_collisions: report.duplicate_commit_collisions,
        axes: &report.axes,
        distribution_contexts: &report.distribution_contexts,
        delivered_capability_instances: &report.delivered_capability_instances,
        deltas: &report.deltas,
        violations: &report.violations,
    })
}

fn expected_digest(raw: &str) -> Option<&str> {
    raw.strip_prefix("blake3:")
        .or_else(|| raw.strip_prefix("blake3-"))
        .filter(|value| value.len() == 64 && value.bytes().all(|byte| byte.is_ascii_hexdigit()))
}

fn repository_root(manifest: &Path, raw: &str) -> PathBuf {
    let root = Path::new(raw);
    if root.is_absolute() {
        root.to_path_buf()
    } else {
        manifest
            .parent()
            .unwrap_or_else(|| Path::new("."))
            .join(root)
    }
}

fn git(repository: &Path, args: &[String]) -> Option<Vec<u8>> {
    let output = Command::new("git")
        .arg("-C")
        .arg(repository)
        .args(args)
        .output()
        .ok()?;
    output.status.success().then_some(output.stdout)
}

fn repository_observed(repository: &Path) -> bool {
    git(
        repository,
        &["rev-parse".into(), "--is-inside-work-tree".into()],
    )
    .is_some_and(|bytes| bytes.starts_with(b"true"))
}

fn resolve_commit(repository: &Path, commit: &str) -> Option<String> {
    if !(7..=64).contains(&commit.len()) || !commit.bytes().all(|byte| byte.is_ascii_hexdigit()) {
        return None;
    }
    let bytes = git(
        repository,
        &[
            "rev-parse".into(),
            "--verify".into(),
            format!("{commit}^{{commit}}"),
        ],
    )?;
    String::from_utf8(bytes)
        .ok()
        .map(|sha| sha.trim().to_ascii_lowercase())
}

fn safe_locator(locator: &str) -> bool {
    let path = Path::new(locator);
    !locator.trim().is_empty()
        && !locator.contains("://")
        && !path.is_absolute()
        && !path.components().any(|part| {
            matches!(
                part,
                Component::ParentDir | Component::RootDir | Component::Prefix(_)
            )
        })
}

fn evidence_observed(repository: &Path, commit: &str, evidence: &Evidence) -> bool {
    let Some(expected) = expected_digest(&evidence.digest) else {
        return false;
    };
    if !safe_locator(&evidence.locator) {
        return false;
    }
    let Some(bytes) = git(
        repository,
        &["show".into(), format!("{commit}:{}", evidence.locator)],
    ) else {
        return false;
    };
    expected.eq_ignore_ascii_case(&digest_bytes(&bytes))
}

fn unique_nonempty(values: &[String]) -> bool {
    !values.is_empty()
        && values.iter().all(|value| !value.trim().is_empty())
        && values.iter().collect::<BTreeSet<_>>().len() == values.len()
}

fn exact_keys<T>(map: &BTreeMap<String, T>, required: &[&str]) -> bool {
    map.keys().map(String::as_str).collect::<BTreeSet<_>>()
        == required.iter().copied().collect::<BTreeSet<_>>()
}

fn evaluate_delta(repository: &Path, delta: &Delta) -> DeltaReport {
    let mut violations = Vec::new();
    if delta.id.trim().is_empty()
        || delta.family.trim().is_empty()
        || delta.summary.trim().is_empty()
    {
        violations.push("id, family, and summary are required".to_string());
    }
    if !delta.capability_iri.contains(':') {
        violations.push("capability_iri must be absolute".to_string());
    }
    if !unique_nonempty(&delta.ontology_modules) || !unique_nonempty(&delta.textual_forms) {
        violations
            .push("ontology_modules and textual_forms must be non-empty and unique".to_string());
    }
    if !exact_keys(&delta.chain, &CHAIN) {
        violations
            .push("manufacturing chain must contain exactly the ten required stages".to_string());
    }
    let canonical_commit = resolve_commit(repository, &delta.commit);
    if canonical_commit.is_none() {
        violations.push("commit is not present in the admitted repository".to_string());
    }
    let commit = canonical_commit.unwrap_or_else(|| delta.commit.to_ascii_lowercase());
    let mut evidence: Vec<(&str, &Evidence)> = delta
        .chain
        .iter()
        .map(|(role, binding)| (role.as_str(), binding))
        .collect();
    evidence.extend([
        ("positive_witness", &delta.positive_witness),
        ("negative_fixture", &delta.negative_fixture),
        ("adversarial_falsifier", &delta.adversarial_falsifier),
        ("verifier", &delta.verifier),
    ]);
    for (role, binding) in evidence {
        if !evidence_observed(repository, &commit, binding) {
            violations.push(format!("{role} is absent, unsafe, or digest-divergent"));
        }
    }
    DeltaReport {
        id: delta.id.clone(),
        commit,
        observed: violations.is_empty(),
        violations,
    }
}

fn load(path: &Path) -> Result<(Manifest, Vec<u8>)> {
    let bytes = fs::read(path).map_err(|error| {
        NounVerbError::execution_error(format!("cannot read {}: {error}", path.display()))
    })?;
    let manifest = serde_json::from_slice(&bytes).map_err(|error| {
        NounVerbError::argument_error(format!("invalid {}: {error}", path.display()))
    })?;
    Ok((manifest, bytes))
}

fn evaluate(path: &Path) -> Result<Report> {
    let (manifest, bytes) = load(path)?;
    let repository = repository_root(path, &manifest.repository.root);
    let mut violations = Vec::new();
    if manifest.schema != MANIFEST_SCHEMA {
        violations.push(format!(
            "unsupported schema {}; expected {MANIFEST_SCHEMA}",
            manifest.schema
        ));
    }
    if manifest.sbb.id.trim().is_empty()
        || manifest.sbb.version.trim().is_empty()
        || !manifest.sbb.architecture_contract.contains(':')
        || manifest.sbb.minimum_commit_equivalent_units == 0
    {
        violations.push("incomplete SBB identity, contract, or density target".to_string());
    }
    if !repository_observed(&repository) {
        violations.push(format!(
            "{} is not an observed Git work tree",
            repository.display()
        ));
    }
    if manifest.deltas.is_empty() {
        violations.push("at least one capability delta is required".to_string());
    }
    if !exact_keys(&manifest.distribution, &AXES) {
        violations.push("distribution must contain exactly the seven required axes".to_string());
    }

    let mut axes = BTreeMap::new();
    for axis in AXES {
        let values = manifest
            .distribution
            .get(axis)
            .cloned()
            .unwrap_or_default();
        if !unique_nonempty(&values) {
            violations.push(format!("distribution axis {axis} is empty or duplicated"));
        }
        axes.insert(
            axis.to_string(),
            values.iter().collect::<BTreeSet<_>>().len(),
        );
    }

    let mut deltas: Vec<_> = manifest
        .deltas
        .iter()
        .map(|delta| evaluate_delta(&repository, delta))
        .collect();
    let mut ids = BTreeSet::new();
    let mut iris = BTreeSet::new();
    let mut commit_counts = BTreeMap::<String, usize>::new();
    for (source, report) in manifest.deltas.iter().zip(&mut deltas) {
        if !ids.insert(source.id.clone()) {
            report.violations.push("duplicate delta id".to_string());
        }
        if !iris.insert(source.capability_iri.clone()) {
            report
                .violations
                .push("duplicate capability IRI".to_string());
        }
        *commit_counts.entry(report.commit.clone()).or_default() += 1;
    }
    let duplicate_commit_collisions = commit_counts.values().filter(|count| **count > 1).count();
    for report in &mut deltas {
        if commit_counts
            .get(&report.commit)
            .copied()
            .unwrap_or_default()
            > 1
        {
            report.violations.push("duplicate commit claim".to_string());
        }
        report.observed = report.violations.is_empty();
    }
    violations.extend(deltas.iter().flat_map(|delta| {
        delta
            .violations
            .iter()
            .map(move |message| format!("{}: {message}", delta.id))
    }));

    let units = deltas.iter().filter(|delta| delta.observed).count();
    let contexts = axes
        .values()
        .try_fold(1_u128, |product, value| product.checked_mul(*value as u128));
    if contexts.is_none() {
        violations.push("distribution context product overflowed u128".to_string());
    }
    let contexts = contexts.unwrap_or_default();
    let delivered = contexts.checked_mul(units as u128);
    if delivered.is_none() {
        violations.push("delivered capability product overflowed u128".to_string());
    }
    let target_met = units as u64 >= manifest.sbb.minimum_commit_equivalent_units;
    let eligible = target_met && violations.is_empty() && units == manifest.deltas.len();
    let standing = if units == 0 {
        "UNKNOWN"
    } else {
        "PARTIAL_ALIVE"
    };
    let mut report = Report {
        schema: REPORT_SCHEMA.to_string(),
        manifest_digest: digest_bytes(&bytes),
        sbb: manifest.sbb,
        standing: standing.to_string(),
        claim_ceiling: "PARTIAL_ALIVE".to_string(),
        target_met,
        eligible_for_external_admission: eligible,
        declared_deltas: manifest.deltas.len(),
        commit_equivalent_units: units,
        duplicate_commit_collisions,
        axes,
        distribution_contexts: contexts.to_string(),
        delivered_capability_instances: delivered.unwrap_or_default().to_string(),
        deltas,
        violations,
        report_digest: String::new(),
    };
    report.report_digest = report_digest(&report)?;
    Ok(report)
}

impl Receipt {
    fn issue(
        operation: &str,
        report: &Report,
        previous: &str,
        artifacts: Vec<String>,
    ) -> Result<Self> {
        let body = ReceiptBody {
            schema: RECEIPT_SCHEMA,
            operation,
            manifest_digest: &report.manifest_digest,
            report_digest: &report.report_digest,
            previous_digest: previous,
            artifacts: &artifacts,
        };
        Ok(Self {
            schema: RECEIPT_SCHEMA.to_string(),
            operation: operation.to_string(),
            manifest_digest: report.manifest_digest.clone(),
            report_digest: report.report_digest.clone(),
            previous_digest: previous.to_string(),
            artifacts,
            digest_algorithm: "blake3".to_string(),
            digest: digest_json(&body)?,
        })
    }

    fn valid(&self) -> Result<bool> {
        Ok(self.schema == RECEIPT_SCHEMA
            && self.digest_algorithm == "blake3"
            && self.digest
                == digest_json(&ReceiptBody {
                    schema: RECEIPT_SCHEMA,
                    operation: &self.operation,
                    manifest_digest: &self.manifest_digest,
                    report_digest: &self.report_digest,
                    previous_digest: &self.previous_digest,
                    artifacts: &self.artifacts,
                })?)
    }
}

fn write_json<T: Serialize>(path: &Path, value: &T) -> Result<()> {
    if let Some(parent) = path.parent() {
        fs::create_dir_all(parent).map_err(|error| {
            NounVerbError::execution_error(format!("cannot create {}: {error}", parent.display()))
        })?;
    }
    let temporary = path.with_extension("tmp");
    let bytes = serde_json::to_vec_pretty(value).map_err(|error| {
        NounVerbError::execution_error(format!("cannot serialize {}: {error}", path.display()))
    })?;
    fs::write(&temporary, bytes).map_err(|error| {
        NounVerbError::execution_error(format!("cannot write {}: {error}", temporary.display()))
    })?;
    fs::rename(&temporary, path).map_err(|error| {
        NounVerbError::execution_error(format!("cannot replace {}: {error}", path.display()))
    })
}

fn receipt_paths(output: &Path) -> (PathBuf, PathBuf, PathBuf) {
    (
        output.join("density-report.json"),
        output.join("density-intent.json"),
        output.join("density-result.json"),
    )
}

fn previous_digest(path: &Path) -> Result<String> {
    if !path.is_file() {
        return Ok("GENESIS".to_string());
    }
    let receipt: Receipt = serde_json::from_slice(&fs::read(path).map_err(|error| {
        NounVerbError::execution_error(format!("cannot read {}: {error}", path.display()))
    })?)
    .map_err(|error| {
        NounVerbError::execution_error(format!("cannot parse {}: {error}", path.display()))
    })?;
    if !receipt.valid()? {
        return Err(NounVerbError::execution_error(format!(
            "existing receipt {} is invalid",
            path.display()
        )));
    }
    Ok(receipt.digest)
}

/// Return the machine-readable SBB density contract.
#[verb]
pub fn schema() -> Result<Value> {
    Ok(json!({
        "manifest_schema": MANIFEST_SCHEMA,
        "report_schema": REPORT_SCHEMA,
        "receipt_schema": RECEIPT_SCHEMA,
        "density_unit": "one unique Git commit with complete observed evidence",
        "required_axes": AXES,
        "required_chain": CHAIN,
        "required_claim_witnesses": [
            "positive_witness", "negative_fixture", "adversarial_falsifier", "verifier"
        ],
        "claim_ceiling": "PARTIAL_ALIVE",
        "external_witness_required_for_alive": true
    }))
}

/// Inspect the complete density report without mutation.
#[verb]
pub fn inspect(manifest: String) -> Result<Value> {
    serde_json::to_value(evaluate(Path::new(&manifest))?).map_err(|error| {
        NounVerbError::execution_error(format!("cannot encode density report: {error}"))
    })
}

/// Validate threshold attainment and external-admission eligibility.
#[verb]
pub fn validate(manifest: String) -> Result<Value> {
    let report = evaluate(Path::new(&manifest))?;
    Ok(json!({
        "standing": report.standing,
        "claim_ceiling": report.claim_ceiling,
        "target_met": report.target_met,
        "eligible_for_external_admission": report.eligible_for_external_admission,
        "commit_equivalent_units": report.commit_equivalent_units,
        "minimum_commit_equivalent_units": report.sbb.minimum_commit_equivalent_units,
        "violations": report.violations,
        "manifest_digest": report.manifest_digest,
        "report_digest": report.report_digest
    }))
}

/// Calculate the combinatorial distribution surface.
#[verb]
pub fn distribution(manifest: String) -> Result<Value> {
    let report = evaluate(Path::new(&manifest))?;
    Ok(json!({
        "sbb": report.sbb.id,
        "commit_equivalent_units": report.commit_equivalent_units,
        "canonical_maintenance_units": report.commit_equivalent_units,
        "axes": report.axes,
        "distribution_contexts": report.distribution_contexts,
        "distribution_multiplier": report.distribution_contexts,
        "delivered_capability_instances": report.delivered_capability_instances,
        "standing": report.standing,
        "report_digest": report.report_digest
    }))
}

/// Emit the deterministic report and chained intent/result receipts.
#[verb]
pub fn receipt(manifest: String, output: String) -> Result<Value> {
    let report = evaluate(Path::new(&manifest))?;
    let (report_path, intent_path, result_path) = receipt_paths(Path::new(&output));
    let intent = Receipt::issue(
        "density-evaluate-intent",
        &report,
        &previous_digest(&result_path)?,
        vec!["density-report.json".to_string()],
    )?;
    write_json(&intent_path, &intent)?;
    write_json(&report_path, &report)?;
    let result = Receipt::issue(
        "density-evaluate-result",
        &report,
        &intent.digest,
        vec![
            "density-report.json".to_string(),
            "density-intent.json".to_string(),
        ],
    )?;
    write_json(&result_path, &result)?;
    Ok(json!({
        "standing": report.standing,
        "claim_ceiling": report.claim_ceiling,
        "report": report_path,
        "intent_receipt": intent_path,
        "result_receipt": result_path,
        "receipt_digest": result.digest
    }))
}

/// Replay the report and receipt chain against exact manifest and Git evidence.
#[verb]
pub fn replay(manifest: String, output: String) -> Result<Value> {
    let report = evaluate(Path::new(&manifest))?;
    let (report_path, intent_path, result_path) = receipt_paths(Path::new(&output));
    let read = |path: &Path| -> Result<Vec<u8>> {
        fs::read(path).map_err(|error| {
            NounVerbError::execution_error(format!("cannot read {}: {error}", path.display()))
        })
    };
    let stored: Report = serde_json::from_slice(&read(&report_path)?).map_err(|error| {
        NounVerbError::execution_error(format!("cannot parse {}: {error}", report_path.display()))
    })?;
    let intent: Receipt = serde_json::from_slice(&read(&intent_path)?).map_err(|error| {
        NounVerbError::execution_error(format!("cannot parse {}: {error}", intent_path.display()))
    })?;
    let result: Receipt = serde_json::from_slice(&read(&result_path)?).map_err(|error| {
        NounVerbError::execution_error(format!("cannot parse {}: {error}", result_path.display()))
    })?;
    let matches = report_digest(&stored)? == stored.report_digest
        && intent.valid()?
        && result.valid()?
        && result.previous_digest == intent.digest
        && intent.manifest_digest == report.manifest_digest
        && intent.report_digest == report.report_digest
        && stored.manifest_digest == report.manifest_digest
        && stored.report_digest == report.report_digest
        && result.manifest_digest == report.manifest_digest
        && result.report_digest == report.report_digest;
    Ok(json!({
        "schema": "ggen.sbb.capability-density-replay.v1",
        "status": if matches { "REPLAY_MATCH" } else { "REPLAY_DIVERGED" },
        "matches": matches,
        "manifest_digest": report.manifest_digest,
        "report_digest": report.report_digest,
        "receipt_digest": result.digest
    }))
}

#[cfg(test)]
mod tests {
    use super::*;

    fn run(root: &Path, args: &[&str]) -> String {
        let output = Command::new("git")
            .arg("-C")
            .arg(root)
            .args(args)
            .output()
            .expect("git");
        assert!(
            output.status.success(),
            "{}",
            String::from_utf8_lossy(&output.stderr)
        );
        String::from_utf8(output.stdout)
            .expect("utf8")
            .trim()
            .to_string()
    }

    fn fixture(root: &Path, duplicate: bool) -> PathBuf {
        run(root, &["init", "--quiet"]);
        run(root, &["config", "user.name", "ggen test"]);
        run(
            root,
            &[
                "config",
                "user.email",
                "ggen-test@example.invalid",
            ],
        );
        fs::write(root.join("evidence.txt"), b"standing evidence").expect("fixture");
        run(root, &["add", "evidence.txt"]);
        run(root, &["commit", "--quiet", "-m", "evidence"]);
        let commit = run(root, &["rev-parse", "HEAD"]);
        let evidence = Evidence {
            locator: "evidence.txt".to_string(),
            digest: format!("blake3:{}", digest_bytes(b"standing evidence")),
        };
        let chain = CHAIN
            .iter()
            .map(|stage| ((*stage).to_string(), evidence.clone()))
            .collect();
        let first = Delta {
            id: "capability-1".to_string(),
            commit,
            capability_iri: "urn:ggen:capability:one".to_string(),
            family: "projection".to_string(),
            summary: "Observed capability".to_string(),
            ontology_modules: vec!["urn:ggen:ontology:test".to_string()],
            textual_forms: vec!["rust".to_string()],
            chain,
            positive_witness: evidence.clone(),
            negative_fixture: evidence.clone(),
            adversarial_falsifier: evidence.clone(),
            verifier: evidence,
        };
        let mut deltas = vec![first.clone()];
        if duplicate {
            let mut second = first;
            second.id = "capability-2".to_string();
            second.capability_iri = "urn:ggen:capability:two".to_string();
            deltas.push(second);
        }
        let distribution = AXES
            .iter()
            .map(|axis| {
                let values = match *axis {
                    "textual_forms" | "runtimes" => {
                        vec!["one".to_string(), "two".to_string()]
                    }
                    _ => vec!["one".to_string()],
                };
                ((*axis).to_string(), values)
            })
            .collect();
        let manifest = Manifest {
            schema: MANIFEST_SCHEMA.to_string(),
            sbb: Sbb {
                id: "test-sbb".to_string(),
                version: "1.0.0".to_string(),
                architecture_contract: "urn:ggen:contract:test".to_string(),
                minimum_commit_equivalent_units: 1,
            },
            repository: Repository {
                root: ".".to_string(),
            },
            distribution,
            deltas,
        };
        let path = root.join("manifest.json");
        fs::write(
            &path,
            serde_json::to_vec_pretty(&manifest).expect("json"),
        )
        .expect("manifest");
        path
    }

    #[test]
    fn unique_commit_counts_once() {
        let directory = tempfile::tempdir().expect("tempdir");
        let report = evaluate(&fixture(directory.path(), false)).expect("report");
        assert_eq!(report.commit_equivalent_units, 1);
        assert_eq!(report.distribution_contexts, "4");
        assert!(report.eligible_for_external_admission);
        assert_eq!(report.claim_ceiling, "PARTIAL_ALIVE");
    }

    #[test]
    fn duplicate_commit_cannot_inflate_density() {
        let directory = tempfile::tempdir().expect("tempdir");
        let report = evaluate(&fixture(directory.path(), true)).expect("report");
        assert_eq!(report.commit_equivalent_units, 0);
        assert_eq!(report.duplicate_commit_collisions, 1);
    }

    #[test]
    fn working_tree_drift_does_not_change_commit_evidence() {
        let directory = tempfile::tempdir().expect("tempdir");
        let path = fixture(directory.path(), false);
        fs::write(
            directory.path().join("evidence.txt"),
            b"uncommitted drift",
        )
        .expect("drift");
        assert_eq!(
            evaluate(&path).expect("report").commit_equivalent_units,
            1
        );
    }

    #[test]
    fn digest_mismatch_refuses_delta() {
        let directory = tempfile::tempdir().expect("tempdir");
        let path = fixture(directory.path(), false);
        let mut manifest: Value =
            serde_json::from_slice(&fs::read(&path).expect("manifest")).expect("json");
        manifest["deltas"][0]["chain"]["ontology"]["digest"] =
            Value::String(format!("blake3:{}", "0".repeat(64)));
        fs::write(
            &path,
            serde_json::to_vec_pretty(&manifest).expect("json"),
        )
        .expect("manifest");
        assert_eq!(
            evaluate(&path).expect("report").commit_equivalent_units,
            0
        );
    }

    #[test]
    fn receipts_replay_and_refuse_tampering() {
        let directory = tempfile::tempdir().expect("tempdir");
        let path = fixture(directory.path(), false);
        let output = directory.path().join("receipts");
        receipt(
            path.display().to_string(),
            output.display().to_string(),
        )
        .expect("receipt");
        let replayed = replay(
            path.display().to_string(),
            output.display().to_string(),
        )
        .expect("replay");
        assert_eq!(replayed["status"], "REPLAY_MATCH");
        let report_path = output.join("density-report.json");
        let mut stored: Value =
            serde_json::from_slice(&fs::read(&report_path).expect("report")).expect("json");
        stored["commit_equivalent_units"] = json!(999);
        fs::write(
            &report_path,
            serde_json::to_vec_pretty(&stored).expect("json"),
        )
        .expect("report");
        let replayed = replay(
            path.display().to_string(),
            output.display().to_string(),
        )
        .expect("replay");
        assert_eq!(replayed["status"], "REPLAY_DIVERGED");
    }
}
