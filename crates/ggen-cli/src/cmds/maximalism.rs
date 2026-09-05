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

/// The human-authored maximalist catalog shipped by
/// `packs/vision-2030-phase-change-pack/catalog/vision-2030-maximalist-capabilities.json`
/// (`ggen.vision2030.maximalist-catalog.v1`). It `extends` the base
/// `ggen.vision2030.catalog.v1` catalog by relative path; the projection follows
/// that link so the resulting manifest is the union the maximalism evaluator's
/// 19 required domains actually need (the 32 maximalist entries alone cover
/// only 7 of them).
const CATALOG_SCHEMA: &str = "ggen.vision2030.maximalist-catalog.v1";
const BASE_CATALOG_SCHEMA: &str = "ggen.vision2030.catalog.v1";

#[derive(Debug, Clone, Deserialize)]
struct MaximalistCatalog {
    schema: String,
    extends: Option<String>,
    required_domains: Vec<String>,
    required_outcomes: Vec<String>,
    capabilities: Vec<MaximalistEntry>,
}

#[derive(Debug, Clone, Deserialize)]
struct MaximalistEntry {
    id: String,
    iri: String,
    domain: String,
    horizon: u16,
    authority: String,
    surface: String,
    summary: String,
    #[serde(default)]
    outcomes: Vec<String>,
    #[serde(default)]
    depends_on: Vec<String>,
}

/// Base-catalog entry shape (the subset the projection needs). Base entries
/// carry a Blue Ocean move but no `surface`/`outcomes`, which the maximalism
/// evaluator requires -- see `project_catalog` for how that absence is kept
/// honest rather than papered over.
#[derive(Debug, Clone, Deserialize)]
struct BaseCatalog {
    schema: String,
    capabilities: Vec<BaseEntry>,
}

#[derive(Debug, Clone, Deserialize)]
struct BaseEntry {
    id: String,
    iri: String,
    domain: String,
    horizon: u16,
    authority: String,
    summary: String,
    #[serde(default)]
    depends_on: Vec<String>,
}

fn read_json<T: for<'de> Deserialize<'de>>(path: &Path, what: &str) -> Result<T> {
    let bytes = fs::read(path).map_err(|error| {
        NounVerbError::execution_error(format!(
            "maximalism project: cannot read {what} {}: {error}",
            path.display()
        ))
    })?;
    serde_json::from_slice(&bytes).map_err(|error| {
        NounVerbError::execution_error(format!(
            "maximalism project: {} is not a valid {what}: {error}",
            path.display()
        ))
    })
}

/// Project the maximalist catalog (plus the base catalog it `extends`) into a
/// `Manifest` the maximalism evaluator can consume.
///
/// Honesty rules, in order of what would be easiest to fake:
/// - No evidence is ever invented: every capability lands with an empty
///   evidence map, so the evaluator reports `DESIGNED`.
/// - No `surface`/`outcomes` are invented for base-catalog entries: they are
///   projected with an empty surface and empty outcome list, and the
///   evaluator's own "surface ... required" / "outcomes must be ... non-empty"
///   violations then name, per capability, exactly which of the 84 still need
///   a human (or a receipted proposal) to decide them. That is the true state
///   of the catalog, made legible -- not a manifest that looks more finished
///   than the catalog is.
/// - `required_domains`/`required_outcomes` come from the catalog itself, not
///   from this binary's constants, so a catalog that under-declares them is
///   caught by the evaluator's own `unique_nonempty`/coverage checks.
fn project_catalog(catalog_path: &Path) -> Result<Manifest> {
    // Check the schema id before the full parse so a wrong-kind document is
    // refused by name ("expected maximalist-catalog.v1"), not by whichever
    // field serde happens to miss first.
    #[derive(Deserialize)]
    struct SchemaOnly {
        schema: String,
    }
    let header: SchemaOnly = read_json(catalog_path, "catalog")?;
    if header.schema != CATALOG_SCHEMA {
        return Err(NounVerbError::execution_error(format!(
            "maximalism project: refusing catalog with schema {:?} (expected {CATALOG_SCHEMA:?})",
            header.schema
        )));
    }
    let catalog: MaximalistCatalog = read_json(catalog_path, "maximalist catalog")?;
    let mut capabilities: Vec<Capability> = Vec::new();
    if let Some(extends) = &catalog.extends {
        let base_path = catalog_path
            .parent()
            .unwrap_or_else(|| Path::new("."))
            .join(extends);
        let base: BaseCatalog = read_json(&base_path, "base catalog (`extends`)")?;
        if base.schema != BASE_CATALOG_SCHEMA {
            return Err(NounVerbError::execution_error(format!(
                "maximalism project: `extends` target {} has schema {:?} (expected {BASE_CATALOG_SCHEMA:?})",
                base_path.display(),
                base.schema
            )));
        }
        capabilities.extend(base.capabilities.into_iter().map(|entry| Capability {
            id: entry.id,
            iri: entry.iri,
            domain: entry.domain,
            horizon: entry.horizon,
            authority: entry.authority,
            // Honest absence -- the base catalog does not declare a surface.
            surface: String::new(),
            summary: entry.summary,
            outcomes: Vec::new(),
            dependencies: entry.depends_on,
            evidence: BTreeMap::new(),
        }));
    }
    capabilities.extend(catalog.capabilities.into_iter().map(|entry| Capability {
        id: entry.id,
        iri: entry.iri,
        domain: entry.domain,
        horizon: entry.horizon,
        authority: entry.authority,
        surface: entry.surface,
        summary: entry.summary,
        outcomes: entry.outcomes,
        dependencies: entry.depends_on,
        evidence: BTreeMap::new(),
    }));
    let mut seen = BTreeSet::new();
    for capability in &capabilities {
        if !seen.insert(capability.id.as_str()) {
            return Err(NounVerbError::execution_error(format!(
                "maximalism project: capability id {:?} appears in both the maximalist catalog and its `extends` base",
                capability.id
            )));
        }
    }
    Ok(Manifest {
        schema: MANIFEST_SCHEMA.to_string(),
        program: Program {
            id: "vision-2030-maximalism".to_string(),
            // Tracks the pack's pack.toml version; bump both together.
            version: "26.8.3".to_string(),
            target_year: 2030,
            minimum_multiplier: 1000,
        },
        required_domains: catalog.required_domains,
        required_outcomes: catalog.required_outcomes,
        horizons: HORIZONS
            .iter()
            .map(|year| Horizon {
                year: *year,
                minimum_alive_capabilities: 1,
            })
            .collect(),
        capabilities,
    })
}

/// Project the pack's maximalist catalog (following `extends`) into a manifest.
///
/// Writes exactly one file, `<output>/vision-2030-maximalism.manifest.json`;
/// the result is `DESIGNED` by construction. Pipe it into
/// `validate`/`combinations`/`outcomes` to see the real closure numbers stated
/// by the evaluator, not assumed here.
#[verb]
pub fn project(catalog: String, output: String) -> Result<Value> {
    let manifest = project_catalog(Path::new(&catalog))?;
    let output_dir = Path::new(&output);
    fs::create_dir_all(output_dir).map_err(|error| {
        NounVerbError::execution_error(format!(
            "maximalism project: cannot create {}: {error}",
            output_dir.display()
        ))
    })?;
    let path = output_dir.join("vision-2030-maximalism.manifest.json");
    let mut bytes = serde_json::to_vec_pretty(&manifest).map_err(|error| {
        NounVerbError::execution_error(format!(
            "maximalism project: cannot serialise manifest: {error}"
        ))
    })?;
    bytes.push(b'\n');
    fs::write(&path, &bytes).map_err(|error| {
        NounVerbError::execution_error(format!(
            "maximalism project: cannot write {}: {error}",
            path.display()
        ))
    })?;
    let without_surface = manifest
        .capabilities
        .iter()
        .filter(|c| c.surface.trim().is_empty())
        .count();
    Ok(json!({
        "schema": manifest.schema,
        "program": manifest.program.id,
        "capabilities": manifest.capabilities.len(),
        "capabilities_with_evidence": 0,
        "capabilities_without_surface": without_surface,
        "manifest": path,
        "manifest_digest": format!("blake3:{}", digest_bytes(&bytes)),
    }))
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
