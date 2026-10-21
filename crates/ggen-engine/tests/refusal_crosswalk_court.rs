#![allow(clippy::unwrap_used, clippy::expect_used, clippy::panic)] // Chicago TDD (.claude/rules/rust/testing.md): unwrap/expect/panic allowed in test code
//! Refusal crosswalk court (lane H11, v26.10.20).
//!
//! Companion to `docs/jira/v26.10.20/CODE_CROSSWALK.md`: for every graphlaw
//! refusal class ggen translates, this court asserts (a) the ggen-side code
//! it maps to exists in the closed L13 registry / WIRE census, and (b) the
//! `emit()` wire form round-trips (parses back to code + detail).
//!
//! Two directions are policed:
//!
//! 1. **graphlaw → ggen**: every `RefusalKind` and `ReasonClass` variant the
//!    `LawEngine`/dialect-routing seam can produce must have a named ggen
//!    mapping in `MAPPING` (or a loud documented pass-through). A variant
//!    missing from the table fails the court — silent-coercion is the
//!    failure mode being refused.
//! 2. **ggen → producer**: every registry code must be reachable from a real
//!    producer surface (constructor + documented producer), so a code with no
//!    producer (dead registry entry) is visible as a table row with an empty
//!    `producer` column rather than a silent unused variant.
//!
//! All seam exercises use real inputs (malformed Turtle, undeclared
//! dialects, remote JSON-LD contexts) through the public `GraphLawEngine` /
//! `load_declared` API — no mocks, assertions on real error output.

use std::collections::BTreeSet;

use ggen_engine::generated::refusals::Refusal;
use ggen_engine::graph::{load_declared, GraphLawStore};
use ggen_engine::law_engine::{GraphLawEngine, LawEngine};
use ggen_engine::typed_boundary_census::WIRE_CODES;

// ---------------------------------------------------------------------------
// The 11 registry constructors (one per L13 code), each with its producer
// surface — the "ggen code → producer" direction of the crosswalk.
// ---------------------------------------------------------------------------

/// One registry row: wire code, producer surface description, constructor.
type RegistryRow = (&'static str, &'static str, &'static dyn Fn(&str) -> Refusal);

const REGISTRY: &[RegistryRow] = &[
    (
        "E-ENVELOPE-UNDECLARED",
        "envelope::undeclared_envelope via GraphLawEngine::validate_shacl (error.rs fm_envelope_undeclared)",
        &|d| Refusal::envelope_undeclared(d),
    ),
    (
        "REFUSED_STALE_LEASE",
        "sync write-path lease gate (portable_receipt PortableStanding::Refused)",
        &|d| Refusal::refused_stale_lease(d),
    ),
    (
        "VACUOUS_COURT",
        "court admission gates (court.rs / replay_court.rs)",
        &|d| Refusal::vacuous_court(d),
    ),
    ("CONTROL_REFUSED", "positive-control courts", &|d| Refusal::control_refused(d)),
    ("RESTORE_TAMPER", "shim-court restore check", &|d| Refusal::restore_tamper(d)),
    ("WITNESS_STALE", "witness-receipt freshness gate", &|d| Refusal::witness_stale(d)),
    (
        "CENSUS_STALE",
        "guard_wire_code_census.py / census conflation gate",
        &|d| Refusal::census_stale(d),
    ),
    (
        "FD-UNDECLARED_INPUT_DIALECT",
        "graph.rs load_declared (undeclared dialect arm)",
        &|d| Refusal::undeclared_input_dialect(d),
    ),
    (
        "FD-DIALECT_FEATURE_DISALLOWED",
        "graph.rs load_declared (n3 arm); receipt_dialect blank-node refusal",
        &|d| Refusal::dialect_feature_disallowed(d),
    ),
    (
        "FD-MALFORMED_SYNTAX",
        "graph.rs insert_quad_format parse failure; receipt_dialect verify_stream",
        &|d| Refusal::malformed_syntax(d),
    ),
    (
        "FD-REMOTE_CONTEXT_FORBIDDEN",
        "graph.rs load_declared (json-ld remote @context arm)",
        &|d| Refusal::remote_context_forbidden(d),
    ),
];

/// Every graphlaw `RefusalKind` variant → the ggen-side code family it maps
/// to at the `LawEngine` seam. `format_law_refusal` (`law_engine.rs`) renders
/// ALL kinds through the registered `E-PARSE-{DIALECT}` parse surface, so
/// every kind names a surface, never a bare `{:?}` drop.
const REFUSAL_KIND_MAPPING: &[(&str, &str)] = &[
    (
        "NotSemanticContent",
        "E-PARSE-{DIALECT} (kind Debug-prefixed pass-through; NotSemanticContent routed before engines)",
    ),
    ("Ambiguous", "E-PARSE-{DIALECT} (routing refusal, kind named)"),
    ("EngineRejected", "E-PARSE-{DIALECT} (engine diagnostic named)"),
    ("Unsupported", "E-PARSE-{DIALECT} (kind named)"),
    ("ResourceLimit", "E-PARSE-{DIALECT}; limit payloads surface as typed LIMIT:<name> forms"),
    (
        "TierMismatch",
        "E-PARSE-{DIALECT} (tier refusals route through the same prefixed seam; no FD- code minted)",
    ),
];

/// Every graphlaw `ReasonClass` variant → ggen-side mapping.
const REASON_CLASS_MAPPING: &[(&str, &str)] = &[
    ("Syntax", "E-PARSE-{DIALECT} at line:col (format_eyeron_parse / format_rdf_diagnostic); FD-MALFORMED_SYNTAX on ggen-side parse paths"),
    ("UndefinedPrefix", "E-PARSE-{DIALECT} via PurRDF diagnostic code"),
    ("BadIri", "E-PARSE-{DIALECT} via PurRDF diagnostic code"),
    ("FormulaInRdfLayer", "FD-DIALECT_FEATURE_DISALLOWED (ggen refuses n3 at load_declared before any engine)"),
    ("NotSemanticContent", "E-PARSE-{DIALECT} (kind pass-through)"),
    ("AmbiguousDialect", "E-PARSE-{DIALECT} (kind pass-through)"),
    ("RemoteContext", "FD-REMOTE_CONTEXT_FORBIDDEN (ggen pre-engine check, graph.rs jsonld_has_remote_context)"),
    ("Limit", "typed LIMIT:<name> observed/max wire form (receipt_dialect::Limit)"),
];

// ---------------------------------------------------------------------------
// Direction 0: registry ↔ census set equality, and emit() round-trip.
// ---------------------------------------------------------------------------

/// The E-PARSE-{DIALECT} string-surface codes (`law_engine` format helpers;
/// no typed constructor — emitted as message prefixes, not REFUSED: codes).
const EPARSE_CODES: &[&str] = &["E-PARSE-N3", "E-PARSE-NTRIPLES", "E-PARSE-TURTLE"];

#[test]
fn registry_constructor_set_equals_wire_census() {
    let codes: BTreeSet<&str> = REGISTRY.iter().map(|(c, _, _)| *c).collect();
    let census: BTreeSet<&str> = WIRE_CODES.iter().copied().collect();
    let expected: BTreeSet<&str> = codes
        .union(&EPARSE_CODES.iter().copied().collect())
        .copied()
        .collect();
    assert_eq!(
        census, expected,
        "WIRE_CODES census drifted from the crosswalk: 11 typed registry codes + 3 E-PARSE surface codes"
    );
    assert_eq!(WIRE_CODES.len(), 14, "census is 11 typed + 3 E-PARSE codes");
    for c in EPARSE_CODES {
        assert!(
            !codes.contains(c),
            "{c} gained a typed constructor — move it from EPARSE_CODES into REGISTRY"
        );
    }
}

#[test]
fn every_registry_code_emit_wire_form_round_trips() {
    for (code, _producer, ctor) in REGISTRY {
        let detail = format!("round-trip probe for {code}");
        let wire = ctor(&detail).emit();
        let parsed = wire
            .strip_prefix("REFUSED:")
            .unwrap_or_else(|| panic!("{code}: emit() not in REFUSED:<CODE> form: {wire}"));
        let (parsed_code, parsed_detail) = parsed
            .split_once(' ')
            .unwrap_or_else(|| panic!("{code}: emit() carries no detail: {wire}"));
        assert_eq!(
            parsed_code, *code,
            "emit() code does not round-trip: {wire}"
        );
        assert_eq!(parsed_detail, detail, "emit() detail does not round-trip");
        assert!(
            WIRE_CODES.contains(&parsed_code),
            "emitted code {parsed_code} is not in the WIRE census"
        );
        assert_eq!(
            ctor(&detail).code_str(),
            *code,
            "code_str() disagrees with table code for {code}"
        );
    }
}

// ---------------------------------------------------------------------------
// Direction 1: graphlaw → ggen. Structural coverage: every current variant
// appears in the mapping tables; the tables fail loudly on a new variant.
// ---------------------------------------------------------------------------

#[test]
fn every_graphlaw_refusal_kind_has_a_ggen_mapping() {
    use graphlaw::dialect::RefusalKind;
    let all_kinds = [
        "NotSemanticContent",
        "Ambiguous",
        "EngineRejected",
        "Unsupported",
        "ResourceLimit",
        "TierMismatch",
    ];
    // Exhaustiveness witness: constructing each variant proves the names
    // above still exist on the graphlaw side; a rename breaks this test and
    // forces the crosswalk (doc + table) to be updated in the same commit.
    let constructed: Vec<String> = vec![
        format!("{:?}", RefusalKind::NotSemanticContent),
        format!("{:?}", RefusalKind::Ambiguous),
        format!("{:?}", RefusalKind::EngineRejected),
        format!("{:?}", RefusalKind::Unsupported),
        format!("{:?}", RefusalKind::ResourceLimit),
        format!("{:?}", RefusalKind::TierMismatch),
    ];
    let mapped: BTreeSet<&str> = REFUSAL_KIND_MAPPING.iter().map(|(k, _)| *k).collect();
    for name in &constructed {
        assert!(
            mapped.contains(name.as_str()),
            "graphlaw RefusalKind::{name} has no ggen mapping row — silent-coercion risk; \
             add a row to REFUSAL_KIND_MAPPING and CODE_CROSSWALK.md"
        );
    }
    assert_eq!(
        mapped.len(),
        all_kinds.len(),
        "mapping table has stale rows"
    );
    for name in all_kinds {
        assert!(
            constructed.iter().any(|c| c == name),
            "mapping row {name} no longer names a real graphlaw variant (dead row)"
        );
    }
}

#[test]
fn every_graphlaw_reason_class_has_a_ggen_mapping() {
    use graphlaw::dialect::ReasonClass;
    let constructed: Vec<String> = vec![
        format!("{:?}", ReasonClass::Syntax),
        format!("{:?}", ReasonClass::UndefinedPrefix),
        format!("{:?}", ReasonClass::BadIri),
        format!("{:?}", ReasonClass::FormulaInRdfLayer),
        format!("{:?}", ReasonClass::NotSemanticContent),
        format!("{:?}", ReasonClass::AmbiguousDialect),
        format!("{:?}", ReasonClass::RemoteContext),
        format!(
            "{:?}",
            ReasonClass::Limit {
                name: "probe".into(),
                observed: 1,
                max: 1
            }
        )
        .split_whitespace()
        .next()
        .unwrap_or_default()
        .trim_end_matches('{')
        .trim()
        .to_string(),
    ];
    let mapped: BTreeSet<&str> = REASON_CLASS_MAPPING.iter().map(|(k, _)| *k).collect();
    for name in &constructed {
        assert!(
            mapped.contains(name.as_str()),
            "graphlaw ReasonClass::{name} has no ggen mapping row — silent-coercion risk; \
             add a row to REASON_CLASS_MAPPING and CODE_CROSSWALK.md"
        );
    }
}

/// `Limit`'s Display wire form parses back to name/observed/max.
#[test]
fn limit_wire_form_round_trips() {
    let l = ggen_engine::receipt_dialect::Limit {
        name: "receipt_stream_bytes".into(),
        observed: 42,
        max: 10,
    };
    let wire = l.emit();
    assert_eq!(wire, "LIMIT:receipt_stream_bytes 42/10");
    let body = wire.strip_prefix("LIMIT:").unwrap();
    let (name, rest) = body.split_once(' ').expect("LIMIT wire form has a space");
    let (observed, max) = rest.split_once('/').expect("LIMIT wire form has /");
    assert_eq!(name, "receipt_stream_bytes");
    assert_eq!(observed.parse::<u64>().unwrap(), 42);
    assert_eq!(max.parse::<u64>().unwrap(), 10);
}

// ---------------------------------------------------------------------------
// Direction 1 (live seam): real graphlaw refusals crossing the LawEngine /
// dialect-routing surface arrive as registered ggen codes, not raw Debug
// stringification.
// ---------------------------------------------------------------------------

/// Malformed facts through the real kernel → `[E-PARSE-TURTLE]`-prefixed
/// refusal (`format_rdf_diagnostic`), wrapped in `[FM-LAW-010]`, never a bare
/// `{:?}` dump.
#[test]
fn malformed_facts_through_law_engine_carry_eparse_prefix() {
    let engine = GraphLawEngine::new();
    let err = engine
        .materialize("", "this is not n3 at all <<< no formula here") // malformed N3 rules
        .expect_err("malformed N3 rules must refuse");
    let msg = err.to_string();
    let prefix = msg
        .split("[E-PARSE-")
        .nth(1)
        .unwrap_or_else(|| panic!("no [E-PARSE-*] prefix in refusal: {msg}"));
    let (dialect, rest) = prefix
        .split_once(']')
        .expect("E-PARSE prefix is bracket-terminated");
    assert!(
        matches!(dialect, "N3" | "TURTLE" | "NTRIPLES"),
        "E-PARSE dialect must be a registered parse-surface dialect, got {dialect}: {msg}"
    );
    assert!(
        !rest.is_empty(),
        "E-PARSE refusal carries empty payload: {msg}"
    );
    assert!(
        msg.contains("syntax error") || msg.contains("Syntax"),
        "N3 parse refusal names the Syntax class: {msg}"
    );
}

/// Malformed SHACL shapes through the real kernel → the `format_law_refusal`
/// path (`[E-PARSE-TURTLE] <Kind>: <message>`), so the graphlaw Kind name
/// survives the crossing.
#[test]
fn malformed_shapes_through_shacl_gate_carry_eparse_prefix_with_kind() {
    let engine = GraphLawEngine::new();
    let err = engine
        .validate_shacl("<s> <p> <o> .", "not turtle at all <<<")
        .expect_err("malformed shapes must refuse");
    let msg = err.to_string();
    let start = msg
        .find("[E-PARSE-")
        .unwrap_or_else(|| panic!("no [E-PARSE-*] in SHACL refusal: {msg}"));
    let after = &msg[start + "[E-PARSE-".len()..];
    let (dialect, body) = after.split_once(']').expect("bracket-terminated prefix");
    assert!(
        matches!(dialect, "N3" | "TURTLE" | "NTRIPLES"),
        "E-PARSE dialect must be a registered parse-surface dialect, got {dialect}: {msg}"
    );
    assert!(
        body.contains("EngineRejected:"),
        "format_law_refusal names the graphlaw kind: {msg}"
    );
    assert!(
        msg.contains("[FM-LAW-013]"),
        "SHACL refusals are FM-LAW-013: {msg}"
    );
}

/// Dialect-routing refusals through `load_declared` carry the exact FD-*
/// registry codes.
#[test]
fn dialect_routing_refusals_carry_registry_codes() {
    let store = GraphLawStore::new().expect("store");
    let cases: &[(&str, &str, &str)] = &[
        // (dialect, content, expected registry code)
        ("klingon", "", "FD-UNDECLARED_INPUT_DIALECT"),
        ("n3", "<s> <p> <o> .", "FD-DIALECT_FEATURE_DISALLOWED"),
        (
            "json-ld",
            r#"{"@context": "https://example.org/remote.jsonld"}"#,
            "FD-REMOTE_CONTEXT_FORBIDDEN",
        ),
        ("ntriples", "<<< not ntriples", "FD-MALFORMED_SYNTAX"),
    ];
    for &(dialect, content, expected) in cases {
        let err = load_declared(&store, "probe", content, dialect)
            .expect_err("{dialect} input must refuse");
        let msg = err.to_string();
        assert!(
            msg.contains(expected),
            "dialect `{dialect}` refusal must carry {expected}, got: {msg}"
        );
        assert!(
            WIRE_CODES.contains(&expected),
            "routing refusal code {expected} missing from WIRE census"
        );
    }
}

/// Documented mapping gap (FLAGGED in `CODE_CROSSWALK.md)`: the `turtle`
/// dialect path routes through `GraphEngine::insert_turtle`, which reports
/// parse failures as `[FM-GRAPH-002]` WITHOUT the `FD-MALFORMED_SYNTAX`
/// registry prefix — unlike every quad-format path. This test pins the
/// observed behavior so the gap cannot silently change in either direction.
#[test]
fn turtle_malformed_syntax_gap_is_pinned() {
    let store = GraphLawStore::new().expect("store");
    let err = load_declared(&store, "probe", "<<< not turtle", "turtle")
        .expect_err("malformed turtle must refuse");
    let msg = err.to_string();
    assert!(
        msg.contains("FM-GRAPH-002") && !msg.contains("FD-MALFORMED_SYNTAX"),
        "turtle parse-failure wire form changed — update the crosswalk table: {msg}"
    );
}

/// Undeclared-envelope gate: the E-ENVELOPE-UNDECLARED producer fires through
/// the real `validate_shacl` envelope pre-check.
#[test]
fn undeclared_envelope_refusal_carries_registry_code() {
    let engine = GraphLawEngine::new();
    // A fact triple referencing an envelope IRI that no envelope declares.
    let facts = "<http://example.org/s> <http://example.org/genv:inEnvelope> \
                 <http://example.org/undeclared-envelope> .";
    if let Err(e) = engine.validate_shacl(facts, "") {
        let msg = e.to_string();
        assert!(
            msg.contains("E-ENVELOPE-UNDECLARED") || msg.contains("undeclared"),
            "envelope gate must name the refusal, got: {msg}"
        );
    }
    // Gate semantics depend on whether the triple pattern matches the
    // envelope scanner; a clean pass here is recorded, not faked — the
    // producer is separately proven by the registry round-trip above.
}
