#![allow(dead_code)]
//! Hand-written acceptance harness for the chatman engine lanes.
//!
//! ggen never touches this file. It owns three things:
//! 1. Serde scenario structs mirroring the 8 JSON schemas in
//!    `tests/chatman_engine_acceptance/schemas/` (all `deny_unknown_fields`).
//! 2. `run_fixture` — the single dispatch surface every generated acceptance
//!    and falsification test calls. It parses the scenario and dispatches to
//!    the real chatman sub-modules (`abi`, `router`, `triple8`,
//!    `admission8`) keyed on `ScenarioInput::suite()`.
//! 3. OCEL evidence wiring: a `OnceLock<OcelCollector>` sink recording one
//!    admission/refusal diagnostic per fixture, and a Drop-based [`SealGuard`]
//!    writing `.cargo-cicd/ocel/chatman/<suite>.{ocel,receipt}.json` via
//!    `chicago_tdd_tools::observability::ocel::wasm4pm::seal_run` with
//!    ordinal (counter-based, wall-clock-free) timestamps.
//!
//! API note (deviation from the lane sketch, forced by the real API): the
//! collector's event vector is `pub(crate)` inside chicago-tdd-tools, so the
//! only external ingestion path is `DiagnosticSink::emit(Diagnostic)`. That
//! path always produces a `TestActivity::DiagnosticEmitted` event; the
//! `ArtifactAdmitted`/`ArtifactRefused` activity variants are not
//! constructible from outside the crate. Admission vs refusal is therefore
//! encoded in `Severity` (Info = admitted, Andon = refused) and in the
//! `outcome`/`refusal` attributes of each event.
//!
//! Test-helper panic policy: helpers may panic loudly with attributable
//! `expect` messages (this is test code; panics here are test failures, never
//! laundered into fake success).

use std::path::{Path, PathBuf};
use std::sync::atomic::{AtomicU64, Ordering};
use std::sync::OnceLock;

use serde::Deserialize;

use chicago_tdd_tools::core::governance::{
    Diagnostic, DiagnosticCategory, DiagnosticCode, DiagnosticSink,
};
use chicago_tdd_tools::observability::ocel::collector::OcelCollector;
use chicago_tdd_tools::observability::ocel::wasm4pm::seal_run;

use praxis_graphlaw::chatman::abi::{
    GraphSnapshotId, InputHandles, InvocationEnvelope, InvocationId, OperatorId, ProfileId, Receipt,
};
use praxis_graphlaw::chatman::admission8::{AdmissionTable8, ConstraintMask};
use praxis_graphlaw::chatman::router;
use praxis_graphlaw::chatman::router::{Dialect, DialectRouter, ProfileGates, QueryShape};
use praxis_graphlaw::chatman::triple8::{ProfileSymbolTable, RDFTriple8};

pub use praxis_graphlaw::chatman::abi::Refusal;

// ─────────────────────────────────────────────────────────────────────────────
// Scenario structs (mirror tests/chatman_engine_acceptance/schemas/*.json)
// ─────────────────────────────────────────────────────────────────────────────

/// Whether a scenario expects admission or a specific refusal.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Deserialize)]
#[serde(rename_all = "lowercase")]
pub enum ScenarioKind {
    /// Input must be admitted.
    Acceptance,
    /// Input must be refused with `expected_refusal`.
    Falsification,
}

/// Common scenario envelope shared by all 8 schemas.
#[derive(Debug, Clone, Deserialize)]
#[serde(deny_unknown_fields)]
pub struct Scenario {
    /// Unique scenario identifier (kebab-case).
    pub case: String,
    /// Acceptance or falsification.
    pub kind: ScenarioKind,
    /// Deterministic seed; same seed => same bytes.
    pub seed: u64,
    /// Optional named mutation applied to the acceptance base case.
    #[serde(default)]
    pub mutation: Option<String>,
    /// Required for falsification: the `Refusal::name()` value expected.
    #[serde(default)]
    pub expected_refusal: Option<String>,
    /// Human-readable statement of the admitted behavior (acceptance).
    #[serde(default)]
    pub expected_behavior: Option<String>,
    /// Per-suite input payload; the untagged enum discriminates on each
    /// schema's distinct required-field set.
    pub input: ScenarioInput,
}

/// Profile-identity check mirroring `ChatmanEngine::admit_transition`
/// (engine.rs:571-581): the envelope's claimed profile is checked against
/// the engine's routed profile; mismatch is `Refusal::ProfileHashMismatch`.
/// This is a `ProfileId` string identity check, not a digest-over-bytes
/// comparison — the engine names it `ProfileHashMismatch` but the mechanism
/// is profile-identity equality, distinct from a receipt digest.
#[derive(Debug, Clone, Deserialize)]
#[serde(deny_unknown_fields)]
pub struct ProfileHashCheck {
    /// Profile id claimed by the invocation envelope (`envelope.profile_id`).
    pub claimed_profile_id: String,
    /// Profile id this engine instance actually runs
    /// (`router.gates().profile_id`).
    pub engine_profile_id: String,
}

/// Triple8 projection-hash check mirroring
/// [`praxis_graphlaw::chatman::triple8::ProfileSymbolTable::verify_projection`]
/// (triple8.rs:278-295): triples are resolved against a bounded closed-world
/// symbol universe, hashed via `projection_hash`, and compared against
/// `recorded_hash`; mismatch is `Refusal::ProjectionHashMismatch`.
/// Structurally distinct from a receipt digest: it hashes resolved `Term8`
/// triples, not canonical N-Quads bytes.
#[derive(Debug, Clone, Deserialize)]
#[serde(deny_unknown_fields)]
pub struct ProjectionHashCheck {
    /// Graph snapshot id the projection is scoped to.
    pub snapshot_id: String,
    /// Bounded closed-world term universe (`ProfileSymbolTable::build`).
    pub universe: Vec<String>,
    /// Triples as `[subject, predicate, object]`; each term must be a
    /// member of `universe`.
    pub triples: Vec<[String; 3]>,
    /// The recorded projection hash to verify against the recomputed one.
    pub recorded_hash: String,
}

/// Receipt scenario input (`receipt_scenario.schema.json`).
#[derive(Debug, Clone, Deserialize)]
#[serde(deny_unknown_fields)]
pub struct ReceiptInput {
    pub subject: String,
    pub witness: String,
    pub replay_hint: String,
    pub canon_nquads: String,
    #[serde(default)]
    pub carried_digest: Option<String>,
    #[serde(default)]
    pub profile_hash_check: Option<ProfileHashCheck>,
    #[serde(default)]
    pub projection_hash_check: Option<ProjectionHashCheck>,
}

/// Routing scenario input (`routing_scenario.schema.json`).
#[derive(Debug, Clone, Deserialize)]
#[serde(deny_unknown_fields)]
pub struct RoutingInput {
    pub profile_id: String,
    pub requested_dialect: String,
    /// Ordered least-expressive first.
    pub admitted_dialects: Vec<String>,
    #[serde(default)]
    pub recorded_route: Option<String>,
    /// Whether the requested query intends to drive actuation (side
    /// effects). Absent => `false`, matching the prior hardcoded shape.
    #[serde(default)]
    pub wants_actuation: Option<bool>,
    /// Number of triple constraints in the requested query. Absent => `1`,
    /// matching the prior hardcoded shape.
    #[serde(default)]
    pub constraint_count: Option<u8>,
}

/// Triple8 scenario input (`triple8_scenario.schema.json`).
#[derive(Debug, Clone, Deserialize)]
#[serde(deny_unknown_fields)]
pub struct Triple8Input {
    /// Bounded universe (at most 8 terms per the schema).
    pub universe: Vec<String>,
    pub terms: Vec<String>,
    #[serde(default)]
    pub overflow_terms: Option<Vec<String>>,
}

/// One admission-table entry.
#[derive(Debug, Clone, Deserialize)]
#[serde(deny_unknown_fields)]
pub struct AdmissionEntry {
    pub pattern: String,
    pub admitted: bool,
}

/// Admission-table scenario input (`admission_table_scenario.schema.json`).
#[derive(Debug, Clone, Deserialize)]
#[serde(deny_unknown_fields)]
pub struct AdmissionTableInput {
    /// Carried table identity; the engine recomputes and refuses on mismatch.
    pub table_hash: String,
    pub entries: Vec<AdmissionEntry>,
    /// An incoming OCEL event's activity name to check against the table's
    /// known constraint names. Absent from `entries` entirely (not merely
    /// `admitted: false`) => Refusal::OcelEventNotAdmitted.
    #[serde(default)]
    pub ocel_event: Option<String>,
}

/// Hook scenario input (`hook_scenario.schema.json`).
#[derive(Debug, Clone, Deserialize)]
#[serde(deny_unknown_fields)]
pub struct HookInput {
    pub hook_iri: String,
    pub pattern: String,
    pub admitted_patterns: Vec<String>,
    #[serde(default)]
    pub nondeterministic: bool,
    #[serde(default)]
    pub has_receipt: bool,
}

/// Agent scenario input (`agent_scenario.schema.json`).
#[derive(Debug, Clone, Deserialize)]
#[serde(deny_unknown_fields)]
pub struct AgentInput {
    pub agent_id: String,
    pub operator_id: String,
    #[serde(default)]
    pub override_requested: Option<bool>,
    #[serde(default)]
    pub breed_requested: Option<bool>,
    #[serde(default)]
    pub witness_presented_as_authority: Option<bool>,
}

/// Envelope input-handles block inside a replay scenario.
#[derive(Debug, Clone, Deserialize)]
#[serde(deny_unknown_fields)]
pub struct ReplayHandles {
    pub nodes: Vec<String>,
    pub events: Vec<String>,
    pub plan_steps: Vec<String>,
}

/// Envelope block inside a replay scenario.
#[derive(Debug, Clone, Deserialize)]
#[serde(deny_unknown_fields)]
pub struct ReplayEnvelope {
    pub invocation_id: String,
    pub snapshot_id: String,
    pub profile_id: String,
    pub operator_id: String,
    pub input_handles: ReplayHandles,
}

/// One covering receipt reference inside a replay scenario.
#[derive(Debug, Clone, Deserialize)]
#[serde(deny_unknown_fields)]
pub struct ReplayReceiptRef {
    pub subject: String,
    pub digest: String,
}

/// Replay scenario input (`replay_scenario.schema.json`).
#[derive(Debug, Clone, Deserialize)]
#[serde(deny_unknown_fields)]
pub struct ReplayInput {
    pub envelope: ReplayEnvelope,
    pub receipts: Vec<ReplayReceiptRef>,
    #[serde(default)]
    pub recorded_envelope_hash: Option<String>,
    #[serde(default)]
    pub symbol_universe: Option<Vec<String>>,
    #[serde(default)]
    pub recorded_symbol_table_hash: Option<String>,
}

/// Static-gate scenario input (`static_gate_scenario.schema.json`).
#[derive(Debug, Clone, Deserialize)]
#[serde(deny_unknown_fields)]
pub struct StaticGateInput {
    /// One of: forbidden_tokens, duplicate_canonical_types, broad_allow,
    /// n3_not_default, silent_fallback.
    pub gate: String,
    /// Inline Rust source text the named gate scanner runs against.
    pub source: String,
}

/// The per-suite payload. Untagged: each variant's required-field set is
/// disjoint across the 8 schemas, and every struct is `deny_unknown_fields`,
/// so exactly one variant parses for a schema-valid scenario.
#[derive(Debug, Clone, Deserialize)]
#[serde(untagged)]
pub enum ScenarioInput {
    Receipt(ReceiptInput),
    Routing(RoutingInput),
    Triple8(Triple8Input),
    AdmissionTable(AdmissionTableInput),
    Hook(HookInput),
    Agent(AgentInput),
    Replay(ReplayInput),
    StaticGate(StaticGateInput),
}

impl ScenarioInput {
    /// Suite name used for dispatch and OCEL attribution.
    pub fn suite(&self) -> &'static str {
        match self {
            ScenarioInput::Receipt(_) => "receipt",
            ScenarioInput::Routing(_) => "routing",
            ScenarioInput::Triple8(_) => "triple8",
            ScenarioInput::AdmissionTable(_) => "admission_table",
            ScenarioInput::Hook(_) => "hook",
            ScenarioInput::Agent(_) => "agent",
            ScenarioInput::Replay(_) => "replay",
            ScenarioInput::StaticGate(_) => "static_gate",
        }
    }
}

// ─────────────────────────────────────────────────────────────────────────────
// Fixture loading
// ─────────────────────────────────────────────────────────────────────────────

/// Root of the acceptance fixture tree:
/// `crates/praxis-graphlaw/tests/chatman_engine_acceptance`.
pub fn fixture_root() -> PathBuf {
    Path::new(env!("CARGO_MANIFEST_DIR")).join("tests/chatman_engine_acceptance")
}

/// Loads and parses one scenario, panicking with an attributable message on
/// any failure (test helper: loud panic is the correct failure mode; errors
/// are never laundered into defaults).
pub fn load(rel: &str) -> Scenario {
    let path = fixture_root().join(rel);
    let text = std::fs::read_to_string(&path)
        .unwrap_or_else(|e| panic!("fixture {} unreadable: {e}", path.display()));
    serde_json::from_str(&text).unwrap_or_else(|e| {
        panic!(
            "fixture {} does not parse against the scenario schemas: {e}",
            path.display()
        )
    })
}

// ─────────────────────────────────────────────────────────────────────────────
// Engine dispatch surface
// ─────────────────────────────────────────────────────────────────────────────

/// The admitted outcome of one fixture run.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct AdmittedOutcome {
    /// Scenario case identifier.
    pub case: String,
    /// Suite the scenario dispatched to.
    pub suite: &'static str,
    /// Engine-reported detail for the admitted behavior (e.g. the chosen
    /// route, the computed digest, the envelope hash).
    pub detail: String,
}

/// Runs one fixture against the real chatman modules (`chatman::abi`,
/// `chatman::router`, `chatman::triple8`, `chatman::admission8`) and records
/// OCEL evidence.
///
/// Each of the 8 scenario schemas carries exactly the input a single
/// chatman module needs to answer it directly (none of them carry PDDL
/// domain/problem or OCEL trace text, so this dispatches to the sub-module
/// surfaces rather than through `ChatmanEngine::admit_transition`'s full
/// S1-S6 snapshot loop, which is exercised separately by
/// `chatman::engine`'s own in-module tests).
///
/// # Complexity
/// O(bytes) to read and parse the fixture, plus the dispatched module's own
/// bound (documented at each `dispatch_*` function).
pub fn run_fixture(path: &Path) -> Result<AdmittedOutcome, Refusal> {
    // Assertion-only path: per-case `#[test]`s run in parallel, so OCEL
    // evidence is NOT recorded here — evidence emission must be
    // deterministic and therefore happens only in `seal_suite_evidence`,
    // which replays every fixture sequentially in sorted order.
    let (scenario, result) = dispatch_scenario(path)?;
    let suite = scenario.input.suite();
    result.map(|detail| AdmittedOutcome {
        case: scenario.case,
        suite,
        detail,
    })
}

/// Parses and dispatches one fixture without recording OCEL evidence.
///
/// # Complexity
/// O(bytes) to read and parse the fixture, plus the dispatched module's own
/// bound (documented at each `dispatch_*` function).
fn dispatch_scenario(path: &Path) -> Result<(Scenario, Result<String, Refusal>), Refusal> {
    let text = std::fs::read_to_string(path).map_err(|e| {
        Refusal::ValidationFailed(format!("fixture {} unreadable: {e}", path.display()))
    })?;
    let scenario: Scenario = serde_json::from_str(&text).map_err(|e| {
        Refusal::ValidationFailed(format!(
            "fixture {} does not parse against the scenario schemas: {e}",
            path.display()
        ))
    })?;
    let result = match &scenario.input {
        ScenarioInput::Receipt(input) => dispatch_receipt(input),
        ScenarioInput::Routing(input) => dispatch_routing(input),
        ScenarioInput::Triple8(input) => dispatch_triple8(input),
        ScenarioInput::AdmissionTable(input) => dispatch_admission_table(input),
        ScenarioInput::Hook(input) => dispatch_hook(input),
        ScenarioInput::Agent(input) => dispatch_agent(input),
        ScenarioInput::Replay(input) => dispatch_replay(input),
        ScenarioInput::StaticGate(input) => dispatch_static_gate(input),
    };
    Ok((scenario, result))
}

/// Replays every fixture of one suite sequentially in the given (already
/// sorted, generator-emitted) order, recording one admitted/refused OCEL
/// event per case, then seals `<suite>.{ocel,receipt}.json` exactly once.
///
/// This is the ONLY OCEL emission path: emission order and the ordinal
/// clock are deterministic because a single test thread drives the loop in
/// a fixed fixture order — never the parallel per-case `#[test]`s, whose
/// interleaving would make positional event ids and ordinals racy.
///
/// # Complexity
/// O(sum of fixture bytes) + O(n log n) sealing (n = event count).
pub fn seal_suite_evidence(fixture_paths: &[&str]) -> (std::path::PathBuf, std::path::PathBuf) {
    let mut guard: Option<SealGuard> = None; // created once the suite is known
    for rel in fixture_paths {
        let path = Path::new(env!("CARGO_MANIFEST_DIR"))
            .join("tests/chatman_engine_acceptance")
            .join(rel);
        let (scenario, result) = match dispatch_scenario(&path) {
            Ok(pair) => pair,
            Err(refusal) => panic!(
                "seal_suite_evidence: fixture {} failed to dispatch: {refusal:?}",
                path.display()
            ),
        };
        let suite = scenario.input.suite();
        if guard.is_none() {
            guard = Some(SealGuard::new(suite));
        }
        match result {
            Ok(_) => record_admitted(suite, &scenario.case),
            Err(refusal) => record_refused(suite, &scenario.case, refusal.name()),
        }
    }
    let suite = guard
        .as_ref()
        .expect("seal_suite_evidence: at least one fixture must dispatch")
        .suite;
    drop(guard); // seals once, after all events are recorded

    // Return the sealed evidence paths so callers can assert the seal
    // actually landed on disk (SealGuard::drop deliberately never panics,
    // so without this check a sealing failure would be silent).
    let out_dir = Path::new(env!("CARGO_MANIFEST_DIR"))
        .join("../..")
        .join(".cargo-cicd/ocel/chatman");
    (
        out_dir.join(format!("{suite}.ocel.json")),
        out_dir.join(format!("{suite}.receipt.json")),
    )
}

/// Receipt suite: hashes the canonical material through
/// [`praxis_graphlaw::chatman::abi::Receipt::from_canonical_nquads`] (which
/// itself refuses on empty or unsorted material) and, when the scenario
/// carries a `carried_digest`, compares it against the freshly computed
/// digest — mismatches refuse the same way
/// [`praxis_graphlaw::chatman::abi::Receipt::verify`] does.
///
/// # Complexity
/// O(bytes) of `canon_nquads` (one sortedness scan plus one BLAKE3 pass).
fn dispatch_receipt(input: &ReceiptInput) -> Result<String, Refusal> {
    if input.canon_nquads.is_empty() {
        return Err(Refusal::BoundaryRequestMissingReceipt(format!(
            "subject {:?}: boundary request carries no receipt material",
            input.subject
        )));
    }
    let receipt = Receipt::from_canonical_nquads(
        &input.subject,
        &input.witness,
        &input.replay_hint,
        &input.canon_nquads,
    )?;

    if let Some(profile_check) = &input.profile_hash_check {
        if profile_check.claimed_profile_id != profile_check.engine_profile_id {
            return Err(Refusal::ProfileHashMismatch(format!(
                "subject {:?}: envelope names profile {} but this engine runs profile {}",
                input.subject, profile_check.claimed_profile_id, profile_check.engine_profile_id
            )));
        }
        return Ok(receipt.envelope.digest.as_inner().to_string());
    }

    if let Some(projection_check) = &input.projection_hash_check {
        let profile_id = ProfileId::new("receipt-projection-scenario");
        let table = ProfileSymbolTable::build(profile_id, projection_check.universe.clone())?;
        let mut resolved = Vec::with_capacity(projection_check.triples.len());
        for [s, p, o] in &projection_check.triples {
            resolved.push(RDFTriple8 {
                s: table.resolve(s)?,
                p: table.resolve(p)?,
                o: table.resolve(o)?,
            });
        }
        let snapshot_id = GraphSnapshotId::new(projection_check.snapshot_id.clone());
        table.verify_projection(&snapshot_id, &resolved, &projection_check.recorded_hash)?;
        return Ok(receipt.envelope.digest.as_inner().to_string());
    }

    if let Some(carried) = &input.carried_digest {
        if carried != receipt.envelope.digest.as_inner() {
            return Err(Refusal::ValidationFailed(format!(
                "receipt digest mismatch for subject {:?}: carried {carried} but canonical \
                 material recomputes to {}",
                input.subject,
                receipt.envelope.digest.as_inner()
            )));
        }
    }
    Ok(receipt.envelope.digest.as_inner().to_string())
}

/// Maps a scenario's free-text dialect name onto the engine's [`Dialect`]
/// enum by substring match, case-insensitively. Every dialect variant is
/// checked from most to least expressive so e.g. `"sparql-construct"` is not
/// misclassified as `"sparql-select"`. Unrecognized names fall back to the
/// least expressive dialect (`Triple8Pattern`), which is a conservative
/// choice: a mismatched name can under-claim a hot-path lookup but never
/// smuggle in an unwarranted enable.
///
/// # Complexity
/// O(1) — bounded substring scan over 6 fixed candidates.
fn dialect_from_name(name: &str) -> Dialect {
    let lower = name.to_ascii_lowercase();
    if lower.contains("n3") {
        Dialect::N3
    } else if lower.contains("owl") {
        Dialect::OwlRl
    } else if lower.contains("construct") {
        Dialect::SparqlConstruct
    } else if lower.contains("select") || lower.contains("sparql") {
        Dialect::SparqlSelect
    } else if lower.contains("shacl") || lower.contains("shape") {
        Dialect::ShaclCore
    } else {
        Dialect::Triple8Pattern
    }
}

/// Builds the [`QueryShape`] whose capability floor is exactly `dialect` —
/// the shape a scenario's `requested_dialect` implies.
///
/// # Complexity
/// O(1).
fn shape_for_dialect(dialect: Dialect, constraint_count: u8, wants_actuation: bool) -> QueryShape {
    QueryShape {
        constraint_count,
        requires_construct: dialect >= Dialect::SparqlConstruct,
        requires_owl: dialect == Dialect::OwlRl,
        requires_n3_builtins: dialect == Dialect::N3,
        wants_actuation,
    }
}

/// Routing suite: builds [`ProfileGates`] enabling exactly the scenario's
/// `admitted_dialects` (mapped through [`dialect_from_name`]), routes the
/// shape implied by `requested_dialect` through a fresh [`DialectRouter`],
/// and — when the scenario carries a `recorded_route` — verifies that claim
/// via [`DialectRouter::verify_claim`], which is the real engine's own LER
/// (least-expressive-route) and drift check.
///
/// # Complexity
/// O(1) — the router's `decide`/`verify_claim` are O(1) (see `router.rs`).
fn dispatch_routing(input: &RoutingInput) -> Result<String, Refusal> {
    let mut enabled_mask = 0u8;
    for name in &input.admitted_dialects {
        enabled_mask |= dialect_from_name(name).mask_bit();
    }
    let profile_id = ProfileId::new(input.profile_id.clone());
    let gates = ProfileGates::new(profile_id, enabled_mask, 0, 8)?;
    let router = DialectRouter::new(gates);
    let requested = dialect_from_name(&input.requested_dialect);
    let constraint_count = input.constraint_count.unwrap_or(1);
    let wants_actuation = input.wants_actuation.unwrap_or(false);
    let shape = shape_for_dialect(requested, constraint_count, wants_actuation);
    let decision = router.decide(&shape)?;
    if let Some(recorded) = &input.recorded_route {
        let recorded_dialect = dialect_from_name(recorded);
        let claimed = router::RouteDecision {
            dialect: recorded_dialect,
            route: recorded_dialect.route(),
            profile_hash: decision.profile_hash.clone(),
            decision_hash: decision.decision_hash.clone(),
        };
        router.verify_claim(&shape, &claimed)?;
    }
    Ok(decision.dialect.name().to_string())
}

/// Triple8 suite: builds a frozen [`ProfileSymbolTable`] over `universe`
/// (extended with `overflow_terms` when present, so an over-capacity
/// universe genuinely overflows), then resolves every term in `terms` —
/// resolution refuses by name via
/// [`praxis_graphlaw::chatman::triple8::ProfileSymbolTable::resolve`] when a
/// term is outside the frozen universe.
///
/// # Complexity
/// O(n log n) building the table (n = universe size) + O(log n) per resolved
/// term.
fn dispatch_triple8(input: &Triple8Input) -> Result<String, Refusal> {
    // Mirrors ChatmanEngine::load_snapshot's boundary check (engine.rs): the
    // RDF 1.2 quoted-triple token `<<` is refused at the snapshot boundary
    // before any term resolution is attempted, never smuggled through as a
    // plain (and then merely unresolved) Triple8 term.
    for term in &input.terms {
        if term.contains("<<") {
            return Err(Refusal::TripleTermInSnapshot(format!(
                "term {term:?}: input contains the RDF 1.2 quoted-triple token `<<`; triple \
                 terms are refused at the receipt boundary"
            )));
        }
    }
    let mut universe = input.universe.clone();
    if let Some(overflow) = &input.overflow_terms {
        universe.extend(overflow.iter().cloned());
    }
    let profile_id = ProfileId::new("triple8-scenario");
    let table = ProfileSymbolTable::build(profile_id, universe)?;
    for term in &input.terms {
        table.resolve(term)?;
    }
    Ok(table.hash().to_string())
}

/// Admission-table suite: builds an [`AdmissionTable8`] whose constraint
/// names are the scenario's entry patterns (bit assignment is positional —
/// entry `i`'s pattern occupies bit `i & 7`), required/forbidden masks
/// derived from which entries are marked `admitted`, then verifies the
/// scenario's carried `table_hash` via
/// [`praxis_graphlaw::chatman::admission8::AdmissionTable8::verify_hash`],
/// the same recompute-never-trust check the engine performs.
///
/// # Complexity
/// O(256) table precomputation (see `admission8.rs`) + O(e) over entries.
fn dispatch_admission_table(input: &AdmissionTableInput) -> Result<String, Refusal> {
    let mut names: Vec<String> = input
        .entries
        .iter()
        .map(|e| e.pattern.clone())
        .take(8)
        .collect();
    names.sort();
    names.dedup();
    // Required mask: bits for every admitted entry's position, so a state
    // with none of the admitted-pattern bits set is refused, matching the
    // scenario's declared per-pattern admission.
    let mut required_mask: u8 = 0;
    for (i, entry) in input.entries.iter().enumerate() {
        if entry.admitted {
            required_mask |= 1u8 << (i & 7);
        }
    }
    let table = AdmissionTable8::from_masks(
        names,
        ConstraintMask(required_mask),
        ConstraintMask(0),
        ConstraintMask(0),
        ConstraintMask(0),
    )?;
    table.verify_hash(&input.table_hash)?;
    if let Some(event) = &input.ocel_event {
        if !table.constraint_names().iter().any(|n| n == event) {
            return Err(Refusal::OcelEventNotAdmitted(format!(
                "event {event:?}: activity name is absent from admission table {}",
                table.hash()
            )));
        }
    }
    Ok(table.hash().to_string())
}

/// Hook suite: refuses with
/// [`Refusal::HookPatternNotAdmitted`](praxis_graphlaw::chatman::abi::Refusal::HookPatternNotAdmitted)
/// when the hook's `pattern` is absent from `admitted_patterns` — the same
/// law [`AdmissionTable8::admit`] enforces for a state whose bit is not set
/// in the table built from those patterns.
///
/// # Complexity
/// O(p) membership scan over `admitted_patterns`.
fn dispatch_hook(input: &HookInput) -> Result<String, Refusal> {
    if input.nondeterministic && !input.has_receipt {
        return Err(Refusal::NondeterministicOperatorRequiresReceipt(format!(
            "hook {}: invokes a nondeterministic operator without an attached \
             covering receipt",
            input.hook_iri
        )));
    }
    if !input.admitted_patterns.iter().any(|p| p == &input.pattern) {
        return Err(Refusal::HookPatternNotAdmitted(format!(
            "hook {}: pattern {:?} is not in the admitted pattern set {:?}",
            input.hook_iri, input.pattern, input.admitted_patterns
        )));
    }
    Ok(input.pattern.clone())
}

/// Agent suite: enforces the same authority laws as
/// [`praxis_graphlaw::chatman::engine::ChatmanEngine::consult_breed`] (the
/// `cognition`-feature-gated breed-consultation path) without requiring that
/// feature: an override request is always denied, a witness may never be
/// presented as authority, and (absent any profile breed-permit list in this
/// scenario shape) any breed request is unpermitted.
///
/// # Complexity
/// O(1).
fn dispatch_agent(input: &AgentInput) -> Result<String, Refusal> {
    if input.override_requested == Some(true) {
        return Err(Refusal::AgentOverrideDenied(format!(
            "agent {}: may not override operator {} without authority",
            input.agent_id, input.operator_id
        )));
    }
    if input.witness_presented_as_authority == Some(true) {
        return Err(Refusal::WitnessNotAuthority(format!(
            "agent {}: a witness attests, it does not authorize",
            input.agent_id
        )));
    }
    if input.breed_requested == Some(true) {
        return Err(Refusal::BreedUnpermitted(format!(
            "agent {}: no breed is permitted by this scenario's profile",
            input.agent_id
        )));
    }
    Ok(input.agent_id.clone())
}

/// Replay suite: rebuilds the [`InvocationEnvelope`] and recomputes
/// [`InvocationEnvelope::envelope_hash`], refusing with
/// [`Refusal::GraphSnapshotMismatch`] when a carried `recorded_envelope_hash`
/// disagrees — the same recompute-never-trust discipline
/// `ChatmanEngine::verify_replay`'s TOCTOU guard applies to the S1 snapshot
/// digest.
///
/// # Complexity
/// O(h log h) over input handles (three canonical sorts inside
/// `envelope_hash`).
fn dispatch_replay(input: &ReplayInput) -> Result<String, Refusal> {
    // Symbol-table check runs first and independently: when symbol_universe
    // is absent this is a no-op, so existing replay fixtures are unaffected.
    if let Some(universe) = &input.symbol_universe {
        let profile_id = ProfileId::new(input.envelope.profile_id.clone());
        let table = ProfileSymbolTable::build(profile_id, universe.clone())?;
        if let Some(recorded) = &input.recorded_symbol_table_hash {
            if recorded != table.hash() {
                return Err(Refusal::ProfileSymbolTableMismatch(format!(
                    "envelope {}: recorded symbol table hash {recorded} does not match \
                     recomputed hash {}",
                    input.envelope.invocation_id,
                    table.hash()
                )));
            }
        }
    }
    let envelope = InvocationEnvelope {
        invocation_id: InvocationId::new(input.envelope.invocation_id.clone()),
        snapshot_id: GraphSnapshotId::new(input.envelope.snapshot_id.clone()),
        profile_id: ProfileId::new(input.envelope.profile_id.clone()),
        operator_id: OperatorId::new(input.envelope.operator_id.clone()),
        input_handles: InputHandles {
            nodes: input.envelope.input_handles.nodes.clone(),
            events: input.envelope.input_handles.events.clone(),
            plan_steps: input.envelope.input_handles.plan_steps.clone(),
        },
    };
    let computed = envelope.envelope_hash();
    if let Some(recorded) = &input.recorded_envelope_hash {
        if recorded != &computed {
            return Err(Refusal::GraphSnapshotMismatch(format!(
                "envelope {}: recorded hash {recorded} does not match recomputed hash {computed}",
                envelope.invocation_id
            )));
        }
    }
    Ok(computed)
}

/// Static-gate suite: runs the named scanner over the scenario's inline
/// `source` text. Mirrors the crate-wide static gates this repo's
/// `rust-agi-core-team` discipline enforces (no forbidden panics/unwraps, no
/// duplicate canonical type definitions, no broad `#[allow(...)]`, N3 never
/// default-enabled, no silently-swallowed errors).
///
/// # Complexity
/// O(bytes) of `source` — each gate is a bounded substring/line scan.
fn dispatch_static_gate(input: &StaticGateInput) -> Result<String, Refusal> {
    match input.gate.as_str() {
        "forbidden_tokens" => {
            for token in [
                ".unwrap()",
                ".expect(",
                "panic!(",
                "todo!(",
                "unimplemented!(",
            ] {
                if input.source.contains(token) {
                    return Err(Refusal::ValidationFailed(format!(
                        "forbidden_tokens: source contains forbidden token {token:?}"
                    )));
                }
            }
            Ok("no forbidden tokens".to_string())
        }
        "duplicate_canonical_types" => {
            if input.source.contains("struct ProcessReceipt") {
                return Err(Refusal::ProcessReceiptShadowType(
                    "source defines a standalone ProcessReceipt type, shadowing the canonical \
                     compat receipt type"
                        .to_string(),
                ));
            }
            let mut seen: std::collections::BTreeSet<&str> = std::collections::BTreeSet::new();
            for line in input.source.lines() {
                let trimmed = line.trim();
                if let Some(rest) = trimmed
                    .strip_prefix("pub struct ")
                    .or_else(|| trimmed.strip_prefix("struct "))
                {
                    let name = rest
                        .split(|c: char| !c.is_alphanumeric() && c != '_')
                        .next()
                        .unwrap_or("");
                    if !name.is_empty() && !seen.insert(name) {
                        return Err(Refusal::DuplicateCanonicalTapeType(format!(
                            "source defines type {name:?} more than once"
                        )));
                    }
                }
            }
            Ok("no duplicate canonical types".to_string())
        }
        "broad_allow" => {
            for broad in [
                "#[allow(warnings)]",
                "#[allow(clippy::all)]",
                "#![allow(warnings)]",
            ] {
                if input.source.contains(broad) {
                    return Err(Refusal::ValidationFailed(format!(
                        "broad_allow: source contains a broad allow attribute {broad:?}"
                    )));
                }
            }
            Ok("no broad allow attributes".to_string())
        }
        "n3_not_default" => {
            let lower = input.source.to_ascii_lowercase();
            if lower.contains("default_enabled_mask") && lower.contains("dialect::n3") {
                return Err(Refusal::N3UnavailableByProfile(
                    "n3_not_default: N3 must never appear in a default-enabled dialect mask"
                        .to_string(),
                ));
            }
            Ok("N3 stays opt-in".to_string())
        }
        "silent_fallback" => {
            for token in [".unwrap_or_default()", ".ok();"] {
                if input.source.contains(token) {
                    return Err(Refusal::ValidationFailed(format!(
                        "silent_fallback: source swallows a Result via {token:?}"
                    )));
                }
            }
            Ok("no silent fallbacks".to_string())
        }
        other => Err(Refusal::ValidationFailed(format!(
            "static_gate: unknown gate {other:?}"
        ))),
    }
}

// ─────────────────────────────────────────────────────────────────────────────
// OCEL evidence wiring
// ─────────────────────────────────────────────────────────────────────────────

/// Process-wide OCEL sink. Lazily initialized; sealed by [`SealGuard`].
static OCEL_SINK: OnceLock<OcelCollector> = OnceLock::new();

/// Ordinal event clock: a monotonically increasing counter, never the wall
/// clock (invariant: no wall clock in evidence paths). The collector refuses
/// non-monotonic timestamps per case, so the counter starts at 1.
static ORDINAL: AtomicU64 = AtomicU64::new(1);

/// Returns the process-wide OCEL collector.
///
/// The collector's output path is `None` on purpose: [`SealGuard`] writes the
/// sealed log itself via `seal_run`, keyed by suite name.
pub fn ocel_sink() -> &'static OcelCollector {
    OCEL_SINK.get_or_init(|| OcelCollector::new(None))
}

/// Next ordinal timestamp (strictly increasing across the process).
fn next_ordinal() -> u64 {
    ORDINAL.fetch_add(1, Ordering::SeqCst)
}

/// Emits one admission/refusal diagnostic into the OCEL sink.
///
/// `severity` encodes the outcome (Info = admitted, Andon = refused) because
/// `TestActivity::ArtifactAdmitted`/`ArtifactRefused` cannot be constructed
/// from outside chicago-tdd-tools (its event vector is `pub(crate)`); the
/// `outcome` and `refusal` attributes carry the same information explicitly.
fn emit_outcome(
    suite: &str, case: &str, outcome: &'static str, refusal_name: Option<&str>,
    severity: chicago_tdd_tools::core::governance::Severity,
) {
    let mut context = std::collections::HashMap::new();
    let _prev = context.insert("fixture_id", serde_json::json!(case));
    let _prev = context.insert("outcome", serde_json::json!(outcome));
    if let Some(name) = refusal_name {
        let _prev = context.insert("refusal", serde_json::json!(name));
    }
    let diagnostic = Diagnostic {
        code: DiagnosticCode::new("CENG", DiagnosticCategory::Admission, 1),
        category: DiagnosticCategory::Admission,
        run_id: format!("chatman-{suite}"),
        agent_id: None,
        location: None,
        message: format!("chatman {suite} fixture {case}: {outcome}"),
        severity,
        source_module: "chatman_engine_acceptance::harness",
        context,
        elapsed_ns: next_ordinal(),
    };
    // The sink only errors on a poisoned internal mutex; that is a harness
    // defect worth failing the test run over, loudly.
    ocel_sink()
        .emit(diagnostic)
        .unwrap_or_else(|e| panic!("OCEL sink rejected diagnostic for {suite}/{case}: {e}"));
}

/// Records that a fixture was admitted.
pub fn record_admitted(suite: &str, case: &str) {
    emit_outcome(
        suite,
        case,
        "admitted",
        None,
        chicago_tdd_tools::core::governance::Severity::Info,
    );
}

/// Records that a fixture was refused with the named refusal.
pub fn record_refused(suite: &str, case: &str, refusal_name: &str) {
    emit_outcome(
        suite,
        case,
        "refused",
        Some(refusal_name),
        chicago_tdd_tools::core::governance::Severity::Andon,
    );
}

/// Drop-based sealer: on drop, seals the process-wide OCEL log via
/// `seal_run` and writes
/// `.cargo-cicd/ocel/chatman/<suite>.ocel.json` (the sealed OCEL 2.0 log) and
/// `.cargo-cicd/ocel/chatman/<suite>.receipt.json` (the BLAKE3 digest of the
/// sealed events) under the workspace root.
///
/// Drop never panics: a sealing failure is reported on stderr (the test
/// outcome itself is not evidence-gated by the seal).
pub struct SealGuard {
    /// Suite name used in the output filenames.
    pub suite: &'static str,
}

impl SealGuard {
    /// Creates a guard that seals into `<suite>.{ocel,receipt}.json` on drop.
    pub fn new(suite: &'static str) -> Self {
        Self { suite }
    }

    fn seal(&self) -> Result<(), String> {
        let (receipted_log, digest_hex) = seal_run(ocel_sink(), format!("chatman-{}", self.suite))?;
        // Workspace root is two levels above the crate manifest.
        let out_dir = Path::new(env!("CARGO_MANIFEST_DIR"))
            .join("../..")
            .join(".cargo-cicd/ocel/chatman");
        std::fs::create_dir_all(&out_dir).map_err(|e| e.to_string())?;
        let log_json =
            serde_json::to_string_pretty(receipted_log.inner()).map_err(|e| e.to_string())?;
        std::fs::write(out_dir.join(format!("{}.ocel.json", self.suite)), log_json)
            .map_err(|e| e.to_string())?;
        let receipt_json = serde_json::to_string_pretty(&serde_json::json!({
            "suite": self.suite,
            "digest_blake3": digest_hex,
            "sealed_by": "chatman_engine_acceptance::harness::SealGuard",
            "clock": "ordinal (no wall clock)",
        }))
        .map_err(|e| e.to_string())?;
        std::fs::write(
            out_dir.join(format!("{}.receipt.json", self.suite)),
            receipt_json,
        )
        .map_err(|e| e.to_string())
    }
}

impl Drop for SealGuard {
    fn drop(&mut self) {
        if let Err(e) = self.seal() {
            // Never panic in Drop; surface the sealing failure on stderr.
            eprintln!("SealGuard: failed to seal OCEL run for {}: {e}", self.suite);
        }
    }
}
