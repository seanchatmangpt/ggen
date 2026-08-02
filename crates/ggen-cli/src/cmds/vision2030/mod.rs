//! Vision 2030 phase-change commands (`ggen vision2030 <verb>`).
//!
//! The CLI is a thin adapter over an evidence-bound capability-program
//! evaluator. Catalog entries never count as achieved capabilities.

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

const MANIFEST_SCHEMA: &str = "ggen.vision2030.program.v1";
const REPORT_SCHEMA: &str = "ggen.vision2030.report.v1";
const RECEIPT_SCHEMA: &str = "ggen.vision2030.receipt.v1";
const SBB_REPORT_SCHEMA: &str = "ggen.sbb.capability-density-report.v1";
const SBB_RECEIPT_SCHEMA: &str = "ggen.sbb.capability-density-receipt.v1";
const EXTERNAL_ACCEPTANCE_SCHEMA: &str = "ggen.external-admission.v1";
const REPLAY_SCHEMA: &str = "ggen.sbb.capability-density-replay.v1";
const EXECUTION_GRANT_SCHEMA: &str = "ggen.execution-grant.v1";

const REQUIRED_DOMAINS: [&str; 12] = [
    "dx",
    "qol",
    "doctor",
    "healthcare",
    "marketplace",
    "mcp-plus",
    "planning",
    "runtime",
    "coordination",
    "process-intelligence",
    "governance",
    "manufacturing",
];
const REQUIRED_EVIDENCE: [&str; 7] = [
    "sbb_report",
    "positive",
    "negative",
    "verifier",
    "receipt",
    "replay",
    "external_acceptance",
];
const BLUE_OCEAN_MOVES: [&str; 4] = ["eliminate", "reduce", "raise", "create"];
const AUTHORITIES: [&str; 4] = ["observe", "recommend", "construct", "actuate"];
const HORIZONS: [u16; 5] = [2026, 2027, 2028, 2029, 2030];

#[derive(Debug, Clone, Deserialize, Serialize)]
struct Manifest {
    schema: String,
    program: Program,
    required_domains: Vec<String>,
    horizons: Vec<Horizon>,
    capabilities: Vec<Capability>,
}

#[derive(Debug, Clone, Deserialize, Serialize)]
struct Program {
    id: String,
    version: String,
    target_year: u16,
    phase_change_target: u64,
}

#[derive(Debug, Clone, Deserialize, Serialize)]
struct Horizon {
    year: u16,
    minimum_alive_capabilities: usize,
}

#[derive(Debug, Clone, Deserialize, Serialize)]
struct Capability {
    id: String,
    iri: String,
    domain: String,
    horizon: u16,
    blue_ocean_move: String,
    authority: String,
    summary: String,
    dependencies: Vec<String>,
    evidence: BTreeMap<String, Evidence>,
}

#[derive(Debug, Clone, Deserialize, Serialize)]
struct Evidence {
    locator: String,
    digest: String,
}

#[derive(Debug, Clone, Deserialize, Serialize)]
struct SbbReport {
    schema: String,
    claim_ceiling: String,
    eligible_for_external_admission: bool,
    commit_equivalent_units: usize,
    distribution_contexts: String,
    delivered_capability_instances: String,
    report_digest: String,
}

#[derive(Debug, Clone, Deserialize, Serialize)]
struct SbbReceipt {
    schema: String,
    operation: String,
    report_digest: String,
    digest_algorithm: String,
    digest: String,
}

#[derive(Debug, Clone, Deserialize, Serialize)]
struct ReplayWitness {
    schema: String,
    status: String,
    matches: bool,
    report_digest: String,
}

#[derive(Debug, Clone, Deserialize, Serialize)]
struct ExternalAcceptance {
    schema: String,
    subject: String,
    decision: String,
    issuer: String,
    report_digest: String,
}

#[derive(Debug, Clone, Deserialize, Serialize)]
struct ExecutionGrant {
    schema: String,
    subject: String,
    broker: String,
    grant: String,
    report_digest: String,
}

#[derive(Debug, Clone, Deserialize, Serialize)]
struct CapabilityReport {
    id: String,
    iri: String,
    domain: String,
    horizon: u16,
    blue_ocean_move: String,
    authority: String,
    standing: String,
    canonical_units: usize,
    delivered_instances: u128,
    multiplier: String,
    dependencies_satisfied: bool,
    violations: Vec<String>,
}

#[derive(Debug, Clone, Deserialize, Serialize)]
struct DomainReport {
    declared: usize,
    alive: usize,
    covered: bool,
}

#[derive(Debug, Clone, Deserialize, Serialize)]
struct HorizonReport {
    minimum_alive_capabilities: usize,
    alive: usize,
    met: bool,
}

#[derive(Debug, Clone, Deserialize, Serialize)]
struct Report {
    schema: String,
    manifest_digest: String,
    program: Program,
    standing: String,
    achieved: bool,
    phase_change_target: u64,
    phase_change_multiplier: String,
    canonical_units: usize,
    delivered_instances: String,
    all_capabilities_alive: bool,
    domains: BTreeMap<String, DomainReport>,
    horizons: BTreeMap<String, HorizonReport>,
    blue_ocean: BTreeMap<String, usize>,
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
    phase_change_target: u64,
    phase_change_multiplier: &'a str,
    canonical_units: usize,
    delivered_instances: &'a str,
    all_capabilities_alive: bool,
    domains: &'a BTreeMap<String, DomainReport>,
    horizons: &'a BTreeMap<String, HorizonReport>,
    blue_ocean: &'a BTreeMap<String, usize>,
    capabilities: &'a [CapabilityReport],
    violations: &'a [String],
}

#[derive(Debug, Clone, Deserialize, Serialize)]
struct ProgramReceipt {
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
        NounVerbError::execution_error(format!("cannot serialize Vision 2030 evidence: {error}"))
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
        program: &report.program,
        standing: &report.standing,
        achieved: report.achieved,
        phase_change_target: report.phase_change_target,
        phase_change_multiplier: &report.phase_change_multiplier,
        canonical_units: report.canonical_units,
        delivered_instances: &report.delivered_instances,
        all_capabilities_alive: report.all_capabilities_alive,
        domains: &report.domains,
        horizons: &report.horizons,
        blue_ocean: &report.blue_ocean,
        capabilities: &report.capabilities,
        violations: &report.violations,
    })
}

/// Return the executable Vision 2030 contract.
#[verb]
pub fn schema() -> Result<Value> {
    Ok(json!({
        "manifest_schema": MANIFEST_SCHEMA,
        "report_schema": REPORT_SCHEMA,
        "receipt_schema": RECEIPT_SCHEMA,
        "required_domains": REQUIRED_DOMAINS,
        "required_evidence": REQUIRED_EVIDENCE,
        "blue_ocean_moves": BLUE_OCEAN_MOVES,
        "authority_classes": AUTHORITIES,
        "horizons": HORIZONS,
        "target_year": 2030,
        "minimum_phase_change_target": 1000,
        "external_acceptance_required": true,
        "zero_unreceipted_actuation": true
    }))
}

/// Inspect the complete program report without mutation.
#[verb]
pub fn inspect(manifest: String) -> Result<Value> {
    evaluation::as_value(Path::new(&manifest))
}

/// Validate achievement, standing, coverage, and measured multiplier.
#[verb]
pub fn validate(manifest: String) -> Result<Value> {
    evaluation::validation(Path::new(&manifest))
}

/// Return horizon gates and missing accepted capability counts.
#[verb]
pub fn roadmap(manifest: String) -> Result<Value> {
    evaluation::roadmap(Path::new(&manifest))
}

/// Return Blue Ocean ERRC coverage.
#[verb]
pub fn blue_ocean(manifest: String) -> Result<Value> {
    evaluation::blue_ocean(Path::new(&manifest))
}

/// Diagnose the developer-experience capability surface.
#[verb]
pub fn dx(manifest: String) -> Result<Value> {
    evaluation::lens(Path::new(&manifest), "dx")
}

/// Diagnose the quality-of-life capability surface.
#[verb]
pub fn qol(manifest: String) -> Result<Value> {
    evaluation::lens(Path::new(&manifest), "qol")
}

/// Diagnose defects and deterministic remediations without actuation.
#[verb]
pub fn doctor(manifest: String) -> Result<Value> {
    evaluation::doctor(Path::new(&manifest))
}

/// Emit the deterministic report and chained receipts.
#[verb]
pub fn receipt(manifest: String, output: String) -> Result<Value> {
    receipts::issue(Path::new(&manifest), Path::new(&output))
}

/// Replay the report and receipt chain against exact evidence.
#[verb]
pub fn replay(manifest: String, output: String) -> Result<Value> {
    receipts::replay(Path::new(&manifest), Path::new(&output))
}
