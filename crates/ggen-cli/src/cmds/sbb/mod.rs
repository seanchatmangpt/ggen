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

mod evaluation;
mod receipts;

#[cfg(test)]
mod tests;

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
    serde_json::to_value(evaluation::evaluate(Path::new(&manifest))?).map_err(|error| {
        NounVerbError::execution_error(format!("cannot encode density report: {error}"))
    })
}

/// Validate threshold attainment and external-admission eligibility.
#[verb]
pub fn validate(manifest: String) -> Result<Value> {
    let report = evaluation::evaluate(Path::new(&manifest))?;
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
    let report = evaluation::evaluate(Path::new(&manifest))?;
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
    receipts::issue(Path::new(&manifest), Path::new(&output))
}

/// Replay the report and receipt chain against exact manifest and Git evidence.
#[verb]
pub fn replay(manifest: String, output: String) -> Result<Value> {
    receipts::replay(Path::new(&manifest), Path::new(&output))
}
