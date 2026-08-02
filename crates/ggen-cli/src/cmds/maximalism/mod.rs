//! Vision 2030 combinatorial-maximalism commands (`ggen maximalism <verb>`).
//!
//! This module admits capability realizations only from exact SBB evidence,
//! structured proof witnesses, passports, receipts, replay, and independent
//! acceptance. Catalog entries and generated artifacts do not count.

use std::{
    collections::{BTreeMap, BTreeSet},
    fs,
    path::{Component, Path, PathBuf},
};

use clap_noun_verb::{NounVerbError, Result};
use clap_noun_verb_macros::verb;
use serde::{Deserialize, Serialize};
use serde_json::{json, Value};

mod evaluation;
mod receipts;

#[cfg(test)]
mod tests;

const MANIFEST_SCHEMA: &str = "ggen.vision2030.maximalism.v1";
const REPORT_SCHEMA: &str = "ggen.vision2030.maximalism-report.v1";
const RECEIPT_SCHEMA: &str = "ggen.vision2030.maximalism-receipt.v1";
const SBB_REPORT_SCHEMA: &str = "ggen.sbb.capability-density-report.v1";
const SBB_RECEIPT_SCHEMA: &str = "ggen.sbb.capability-density-receipt.v1";
const SBB_REPLAY_SCHEMA: &str = "ggen.sbb.capability-density-replay.v1";
const WITNESS_SCHEMA: &str = "ggen.capability-witness.v1";
const VERIFIER_SCHEMA: &str = "ggen.capability-verifier.v1";
const PASSPORT_SCHEMA: &str = "ggen.capability-passport.v1";
const ACCEPTANCE_SCHEMA: &str = "ggen.external-admission.v1";
const EXECUTION_GRANT_SCHEMA: &str = "ggen.execution-grant.v1";

pub(super) const REQUIRED_DOMAINS: [&str; 19] = [
    "dx",
    "qol",
    "doctor",
    "wizard",
    "truthforge",
    "telco",
    "healthcare",
    "marketplace",
    "mcp-plus",
    "planning",
    "runtime",
    "coordination",
    "process-intelligence",
    "governance",
    "manufacturing",
    "security",
    "data",
    "observability",
    "economics",
];

pub(super) const OUTCOMES: [&str; 9] = [
    "YES", "NO", "AWAIT", "ASK", "RETRIEVE", "INSPECT", "ESCALATE", "IGNORE", "SETTLE",
];

const AUTHORITIES: [&str; 4] = ["observe", "recommend", "construct", "actuate"];
const HORIZONS: [u16; 5] = [2026, 2027, 2028, 2029, 2030];
const REQUIRED_EVIDENCE: [&str; 9] = [
    "sbb_report",
    "positive",
    "negative",
    "adversarial",
    "verifier",
    "passport",
    "receipt",
    "replay",
    "external_acceptance",
];

#[derive(Debug, Clone, Deserialize, Serialize)]
pub(super) struct Manifest {
    schema: String,
    program: Program,
    required_domains: Vec<String>,
    required_outcomes: Vec<String>,
    horizons: Vec<Horizon>,
    capabilities: Vec<Capability>,
}

#[derive(Debug, Clone, Deserialize, Serialize)]
pub(super) struct Program {
    id: String,
    version: String,
    target_year: u16,
    minimum_multiplier: u64,
}

#[derive(Debug, Clone, Deserialize, Serialize)]
pub(super) struct Horizon {
    year: u16,
    minimum_alive_capabilities: usize,
}

#[derive(Debug, Clone, Deserialize, Serialize)]
pub(super) struct Capability {
    id: String,
    iri: String,
    domain: String,
    horizon: u16,
    authority: String,
    surface: String,
    summary: String,
    outcomes: Vec<String>,
    dependencies: Vec<String>,
    evidence: BTreeMap<String, Evidence>,
}

#[derive(Debug, Clone, Deserialize, Serialize)]
pub(super) struct Evidence {
    locator: String,
    digest: String,
}

#[derive(Debug, Clone, Deserialize, Serialize)]
pub(super) struct SbbReport {
    schema: String,
    claim_ceiling: String,
    eligible_for_external_admission: bool,
    commit_equivalent_units: usize,
    axes: BTreeMap<String, usize>,
    distribution_contexts: String,
    delivered_capability_instances: String,
    report_digest: String,
}

#[derive(Debug, Clone, Deserialize, Serialize)]
pub(super) struct SbbReceipt {
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
struct SbbReceiptBody<'a> {
    schema: &'static str,
    operation: &'a str,
    manifest_digest: &'a str,
    report_digest: &'a str,
    previous_digest: &'a str,
    artifacts: &'a [String],
}

#[derive(Debug, Clone, Deserialize, Serialize)]
pub(super) struct ReplayWitness {
    schema: String,
    status: String,
    matches: bool,
    report_digest: String,
}

#[derive(Debug, Clone, Deserialize, Serialize)]
pub(super) struct ProofWitness {
    schema: String,
    kind: String,
    subject: String,
    result: String,
    report_digest: String,
}

#[derive(Debug, Clone, Deserialize, Serialize)]
pub(super) struct VerifierWitness {
    schema: String,
    subject: String,
    verifier: String,
    result: String,
    report_digest: String,
}

#[derive(Debug, Clone, Deserialize, Serialize)]
pub(super) struct Passport {
    schema: String,
    subject: String,
    report_digest: String,
    manifest: String,
    architecture_contract: String,
    route_model: String,
    interface_contract: String,
    schemas: Vec<String>,
    positive_fixtures: Vec<String>,
    negative_fixtures: Vec<String>,
    adversary_results: Vec<String>,
    provenance: Vec<String>,
    shacl_result: String,
    runtime_verdict: String,
    telemetry_verdict: String,
    deployment_hash: String,
    signature: String,
    bundle_digest: String,
}

#[derive(Debug, Clone, Deserialize, Serialize)]
pub(super) struct ExternalAcceptance {
    schema: String,
    subject: String,
    decision: String,
    issuer: String,
    report_digest: String,
}

#[derive(Debug, Clone, Deserialize, Serialize)]
pub(super) struct ExecutionGrant {
    schema: String,
    subject: String,
    broker: String,
    grant: String,
    report_digest: String,
}

#[derive(Debug, Clone, Deserialize, Serialize)]
pub(super) struct CapabilityReport {
    id: String,
    iri: String,
    domain: String,
    horizon: u16,
    authority: String,
    surface: String,
    outcomes: Vec<String>,
    standing: String,
    canonical_units: usize,
    delivered_instances: u128,
    ontology_modules: usize,
    textual_forms: usize,
    semantic_cells: u128,
    multiplier: String,
    dependencies_satisfied: bool,
    violations: Vec<String>,
}

#[derive(Debug, Clone, Deserialize, Serialize)]
pub(super) struct Coverage {
    declared: usize,
    alive: usize,
    closed: bool,
}

#[derive(Debug, Clone, Deserialize, Serialize)]
pub(super) struct HorizonReport {
    minimum_alive_capabilities: usize,
    alive: usize,
    closed: bool,
}

#[derive(Debug, Clone, Deserialize, Serialize)]
pub(super) struct Report {
    schema: String,
    manifest_digest: String,
    program: Program,
    standing: String,
    achieved: bool,
    measured_multiplier: String,
    canonical_units: usize,
    delivered_instances: String,
    semantic_cells: String,
    alive_domain_count: usize,
    domain_combination_space: String,
    all_capabilities_alive: bool,
    domains: BTreeMap<String, Coverage>,
    outcomes: BTreeMap<String, Coverage>,
    horizons: BTreeMap<String, HorizonReport>,
    capabilities: Vec<CapabilityReport>,
    violations: Vec<String>,
    report_digest: String,
}

#[derive(Serialize)]
struct ReportBody<'a> {
    schema: &'static str,
    manifest_digest: &'a str,
    program: &'a Program,
    standing: &'a str,
    achieved: bool,
    measured_multiplier: &'a str,
    canonical_units: usize,
    delivered_instances: &'a str,
    semantic_cells: &'a str,
    alive_domain_count: usize,
    domain_combination_space: &'a str,
    all_capabilities_alive: bool,
    domains: &'a BTreeMap<String, Coverage>,
    outcomes: &'a BTreeMap<String, Coverage>,
    horizons: &'a BTreeMap<String, HorizonReport>,
    capabilities: &'a [CapabilityReport],
    violations: &'a [String],
}

#[derive(Debug, Clone, Deserialize, Serialize)]
pub(super) struct ProgramReceipt {
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

pub(super) fn digest_json<T: Serialize>(value: &T) -> Result<String> {
    let bytes = serde_json::to_vec(value).map_err(|error| {
        NounVerbError::execution_error(format!("cannot serialize maximalism evidence: {error}"))
    })?;
    Ok(blake3::hash(&bytes).to_hex().to_string())
}

pub(super) fn digest_bytes(bytes: &[u8]) -> String {
    blake3::hash(bytes).to_hex().to_string()
}

pub(super) fn report_digest(report: &Report) -> Result<String> {
    digest_json(&ReportBody {
        schema: REPORT_SCHEMA,
        manifest_digest: &report.manifest_digest,
        program: &report.program,
        standing: &report.standing,
        achieved: report.achieved,
        measured_multiplier: &report.measured_multiplier,
        canonical_units: report.canonical_units,
        delivered_instances: &report.delivered_instances,
        semantic_cells: &report.semantic_cells,
        alive_domain_count: report.alive_domain_count,
        domain_combination_space: &report.domain_combination_space,
        all_capabilities_alive: report.all_capabilities_alive,
        domains: &report.domains,
        outcomes: &report.outcomes,
        horizons: &report.horizons,
        capabilities: &report.capabilities,
        violations: &report.violations,
    })
}

#[verb]
pub fn schema() -> Result<Value> {
    Ok(json!({
        "manifest_schema": MANIFEST_SCHEMA,
        "report_schema": REPORT_SCHEMA,
        "receipt_schema": RECEIPT_SCHEMA,
        "required_domains": REQUIRED_DOMAINS,
        "lawful_outcomes": OUTCOMES,
        "required_evidence": REQUIRED_EVIDENCE,
        "authority_classes": AUTHORITIES,
        "horizons": HORIZONS,
        "target_year": 2030,
        "minimum_multiplier": 1000,
        "category": "private-cloud admissible-work operating system",
        "llm_decides_standing": false,
        "zero_unreceipted_actuation": true
    }))
}

#[verb]
pub fn inspect(manifest: String) -> Result<Value> {
    evaluation::as_value(Path::new(&manifest))
}

#[verb]
pub fn validate(manifest: String) -> Result<Value> {
    evaluation::validation(Path::new(&manifest))
}

#[verb]
pub fn combinations(manifest: String) -> Result<Value> {
    evaluation::combinations(Path::new(&manifest))
}

#[verb]
pub fn outcomes(manifest: String) -> Result<Value> {
    evaluation::outcome_report(Path::new(&manifest))
}

#[verb]
pub fn receipt(manifest: String, output: String) -> Result<Value> {
    receipts::issue(Path::new(&manifest), Path::new(&output))
}

#[verb]
pub fn replay(manifest: String, output: String) -> Result<Value> {
    receipts::replay(Path::new(&manifest), Path::new(&output))
}

pub(super) fn doctor_report(path: &Path) -> Result<Value> {
    evaluation::doctor(path)
}

pub(super) fn doctor_domain(path: &Path) -> Result<Value> {
    evaluation::domain_lens(path, "doctor")
}

pub(super) fn wizard_plan(path: &Path, capability: &str) -> Result<Value> {
    evaluation::wizard(path, capability)
}

pub(super) fn wizard_domain(path: &Path) -> Result<Value> {
    evaluation::domain_lens(path, "wizard")
}

pub(super) fn telco_report(path: &Path) -> Result<Value> {
    evaluation::telco(path, None)
}

pub(super) fn telco_surface(path: &Path, surface: &str) -> Result<Value> {
    evaluation::telco(path, Some(surface))
}
