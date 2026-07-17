//! Static source gates for the chatman engine (`src/chatman/`).
//!
//! These are filesystem-scanning gate tests, exempt from the no-raw-`#[test]`
//! rule: they read source text, not runtime behavior. Each scanner is a plain
//! function over `(file name, content)` so `gates_can_fail` can prove every
//! scanner flags a known-bad counterexample (a gate that cannot fail is not
//! a gate).

use std::collections::BTreeSet;
use std::fs;
use std::path::{Path, PathBuf};

use praxis_graphlaw::chatman::abi::{Refusal, ALL_REFUSAL_NAMES};

/// Crate root, independent of the runner's working directory.
fn crate_root() -> PathBuf {
    PathBuf::from(env!("CARGO_MANIFEST_DIR"))
}

/// Recursively collects every `.rs` file under `dir`, sorted for
/// deterministic output. O(files) in the scanned tree.
fn rs_files_under(dir: &Path) -> Vec<PathBuf> {
    let mut files = Vec::new();
    let entries =
        fs::read_dir(dir).unwrap_or_else(|e| panic!("cannot read dir {}: {e}", dir.display()));
    for entry in entries {
        let entry = entry.unwrap_or_else(|e| panic!("bad dir entry under {}: {e}", dir.display()));
        let path = entry.path();
        if path.is_dir() {
            files.extend(rs_files_under(&path));
        } else if path.extension().is_some_and(|ext| ext == "rs") {
            files.push(path);
        }
    }
    files.sort();
    files
}

fn read(path: &Path) -> String {
    fs::read_to_string(path).unwrap_or_else(|e| panic!("cannot read {}: {e}", path.display()))
}

/// True when the line is (line-)comment-only, i.e. `//`, `///`, or `//!`.
fn is_comment_line(line: &str) -> bool {
    line.trim_start().starts_with("//")
}

// ---------------------------------------------------------------------------
// Scanners. Each returns human-readable violations: "file:line: reason".
// ---------------------------------------------------------------------------

/// Gate 1: forbidden tokens anywhere (including comments) in chatman source.
fn scan_forbidden_tokens(name: &str, content: &str) -> Vec<String> {
    const FORBIDDEN: [&str; 8] = [
        "unwrap(",
        "expect(",
        "panic!(",
        "todo!(",
        "unimplemented!(",
        "TODO",
        "TBD",
        "placeholder",
    ];
    let mut violations = Vec::new();
    for (idx, line) in content.lines().enumerate() {
        for token in FORBIDDEN {
            if line.contains(token) {
                violations.push(format!(
                    "{name}:{}: forbidden token {token:?} in {line:?}",
                    idx + 1
                ));
            }
        }
    }
    violations
}

/// Gate 2: duplicate canonical type definitions. The canonical `Pddl8Tape`,
/// `PowlTape`, `ProcessReceipt`, `OcelEvent`, and `ConditionCell` types live
/// in their owning substrate crates; a second `struct` definition anywhere in
/// this crate is a shadow type. Doc/line comments are exempt (they may name
/// the canonical types when describing them).
fn scan_duplicate_canonical_types(name: &str, content: &str) -> Vec<String> {
    const CANONICAL: [&str; 5] = [
        "struct Pddl8Tape",
        "struct PowlTape",
        "struct ProcessReceipt",
        "struct OcelEvent",
        "struct ConditionCell",
    ];
    let mut violations = Vec::new();
    for (idx, line) in content.lines().enumerate() {
        if is_comment_line(line) {
            continue;
        }
        for needle in CANONICAL {
            if line.contains(needle) {
                violations.push(format!(
                    "{name}:{}: duplicate canonical type {needle:?} in {line:?}",
                    idx + 1
                ));
            }
        }
    }
    violations
}

/// Gate 3: no broad crate/module-level `#![allow(...)]` in chatman source.
fn scan_broad_allow(name: &str, content: &str) -> Vec<String> {
    let mut violations = Vec::new();
    for (idx, line) in content.lines().enumerate() {
        if line.contains("#![allow") {
            violations.push(format!("{name}:{}: broad #![allow] in {line:?}", idx + 1));
        }
    }
    violations
}

/// Gate 4: N3 must never be enabled by default in chatman source. Flags any
/// line that mentions `n3` together with `enable`/`default` and `true`
/// (case-insensitive), e.g. `const N3_ENABLED_BY_DEFAULT: bool = true;`.
fn scan_n3_default_on(name: &str, content: &str) -> Vec<String> {
    let mut violations = Vec::new();
    for (idx, line) in content.lines().enumerate() {
        let lower = line.to_lowercase();
        let mentions_n3 = lower.contains("n3");
        let turned_on = lower.contains("true");
        let by_policy = lower.contains("enable") || lower.contains("default");
        if mentions_n3 && turned_on && by_policy {
            violations.push(format!(
                "{name}:{}: N3 enabled-by-default pattern in {line:?}",
                idx + 1
            ));
        }
    }
    violations
}

/// Gate 5: no silent fallbacks in chatman source: `unwrap_or_default(`,
/// `.ok();` (discarding a `Result`), and `let _ =` (discarding a value that
/// may be a `Result`).
fn scan_silent_fallback(name: &str, content: &str) -> Vec<String> {
    const SILENT: [&str; 3] = ["unwrap_or_default(", ".ok();", "let _ ="];
    let mut violations = Vec::new();
    for (idx, line) in content.lines().enumerate() {
        for token in SILENT {
            if line.contains(token) {
                violations.push(format!(
                    "{name}:{}: silent fallback {token:?} in {line:?}",
                    idx + 1
                ));
            }
        }
    }
    violations
}

/// Runs one scanner over every `.rs` file under `dir` and asserts zero
/// violations, printing every violation on failure (zero-escape assertion).
fn assert_clean(dir: &Path, scanner: fn(&str, &str) -> Vec<String>, gate: &str) {
    let mut violations = Vec::new();
    for path in rs_files_under(dir) {
        let content = read(&path);
        violations.extend(scanner(&path.display().to_string(), &content));
    }
    assert!(
        violations.is_empty(),
        "gate {gate} found {} violation(s):\n{}",
        violations.len(),
        violations.join("\n")
    );
}

// ---------------------------------------------------------------------------
// Gates 1-5: the chatman source tree is clean.
// ---------------------------------------------------------------------------

#[test]
fn gate_no_forbidden_tokens_in_chatman() {
    assert_clean(
        &crate_root().join("src/chatman"),
        scan_forbidden_tokens,
        "forbidden-tokens",
    );
}

#[test]
fn gate_no_duplicate_canonical_types_in_crate() {
    // Whole-crate scan: shadow types anywhere in src/ are refused, not just
    // under src/chatman/.
    assert_clean(
        &crate_root().join("src"),
        scan_duplicate_canonical_types,
        "duplicate-canonical-types",
    );
}

#[test]
fn gate_no_broad_allow_in_chatman() {
    assert_clean(
        &crate_root().join("src/chatman"),
        scan_broad_allow,
        "broad-allow",
    );
}

#[test]
fn gate_n3_not_default_in_chatman() {
    assert_clean(
        &crate_root().join("src/chatman"),
        scan_n3_default_on,
        "n3-not-default",
    );
}

#[test]
fn gate_no_silent_fallback_in_chatman() {
    assert_clean(
        &crate_root().join("src/chatman"),
        scan_silent_fallback,
        "silent-fallback",
    );
}

// ---------------------------------------------------------------------------
// Gate 6: every scanner flags a known-bad counterexample.
// ---------------------------------------------------------------------------

#[test]
fn gates_can_fail() {
    let cases: [(&str, fn(&str, &str) -> Vec<String>, &str); 8] = [
        (
            "forbidden_tokens/unwrap",
            scan_forbidden_tokens,
            "let x = maybe.unwrap();",
        ),
        (
            "forbidden_tokens/todo_marker",
            scan_forbidden_tokens,
            "// TODO finish this later",
        ),
        (
            "forbidden_tokens/placeholder",
            scan_forbidden_tokens,
            "let body = \"placeholder\";",
        ),
        (
            "duplicate_canonical_types",
            scan_duplicate_canonical_types,
            "pub struct ProcessReceipt { pub id: String }",
        ),
        ("broad_allow", scan_broad_allow, "#![allow(clippy::all)]"),
        (
            "n3_default_on/enable",
            scan_n3_default_on,
            "const N3_ENABLED: bool = true;",
        ),
        (
            "n3_default_on/default",
            scan_n3_default_on,
            "let n3_on_by_default = true;",
        ),
        (
            "silent_fallback",
            scan_silent_fallback,
            "let value = risky().unwrap_or_default();",
        ),
    ];
    for (case, scanner, bad_source) in cases {
        let violations = scanner("counterexample.rs", bad_source);
        assert!(
            !violations.is_empty(),
            "gate scanner {case} failed to flag known-bad source {bad_source:?}"
        );
    }
    // And the comment exemption of the duplicate-type scanner is real: a doc
    // comment naming a canonical type is NOT flagged.
    let doc_only = scan_duplicate_canonical_types(
        "doc.rs",
        "/// Mirrors the canonical struct ProcessReceipt from compat.",
    );
    assert!(
        doc_only.is_empty(),
        "duplicate-type scanner must exempt doc comments, flagged: {doc_only:?}"
    );
    // Silent-fallback scanner flags each pattern independently.
    assert!(!scan_silent_fallback("c.rs", "res.ok();").is_empty());
    assert!(!scan_silent_fallback("c.rs", "let _ = do_side_effect();").is_empty());
}

// ---------------------------------------------------------------------------
// Gate 7: schema <-> ABI refusal-name cross-check (the cross-lane contract).
// ---------------------------------------------------------------------------

const SCHEMA_FILES: [&str; 8] = [
    "receipt_scenario.schema.json",
    "routing_scenario.schema.json",
    "triple8_scenario.schema.json",
    "admission_table_scenario.schema.json",
    "hook_scenario.schema.json",
    "agent_scenario.schema.json",
    "replay_scenario.schema.json",
    "static_gate_scenario.schema.json",
];

#[test]
fn gate_schema_refusal_enums_match_abi() {
    let abi_names: BTreeSet<&str> = ALL_REFUSAL_NAMES.iter().copied().collect();
    assert_eq!(
        abi_names.len(),
        ALL_REFUSAL_NAMES.len(),
        "ALL_REFUSAL_NAMES contains duplicates"
    );
    let schemas_dir = crate_root().join("tests/chatman_engine_acceptance/schemas");
    for file in SCHEMA_FILES {
        let path = schemas_dir.join(file);
        let raw = read(&path);
        let schema: serde_json::Value =
            serde_json::from_str(&raw).unwrap_or_else(|e| panic!("{file}: invalid JSON: {e}"));
        let enum_values = schema
            .pointer("/properties/expected_refusal/enum")
            .and_then(|v| v.as_array())
            .unwrap_or_else(|| panic!("{file}: missing properties.expected_refusal.enum"));
        let schema_names: BTreeSet<&str> = enum_values
            .iter()
            .map(|v| {
                v.as_str()
                    .unwrap_or_else(|| panic!("{file}: non-string refusal name {v}"))
            })
            .collect();
        assert_eq!(
            schema_names.len(),
            enum_values.len(),
            "{file}: expected_refusal enum contains duplicates"
        );
        assert_eq!(
            schema_names,
            abi_names,
            "{file}: expected_refusal enum diverges from Refusal::name() set; \
             missing from schema: {:?}; extra in schema: {:?}",
            abi_names.difference(&schema_names).collect::<Vec<_>>(),
            schema_names.difference(&abi_names).collect::<Vec<_>>()
        );
    }
}

#[test]
fn gate_refusal_name_matches_const_list() {
    // Construct every variant once; name() of each must appear in
    // ALL_REFUSAL_NAMES in declaration order. Adding a variant without
    // updating this list fails here; removing one fails to compile at the
    // exhaustive match in abi.rs.
    let ctx = || "gate".to_string();
    let all = vec![
        Refusal::ValidationFailed(ctx()),
        Refusal::PlanInfeasible(ctx()),
        Refusal::TraceUnlawful(ctx()),
        Refusal::HookUnpermitted(ctx()),
        Refusal::MissingReceipt(ctx()),
        Refusal::SnapshotNotFound(ctx()),
        Refusal::BoundaryRequestMissingReceipt(ctx()),
        Refusal::Triple8UniverseOverflow(ctx()),
        Refusal::TermNotInTriple8Universe(ctx()),
        Refusal::ProfileSymbolTableMismatch(ctx()),
        Refusal::ProjectionHashMismatch(ctx()),
        Refusal::WarmPathRequired(ctx()),
        Refusal::AdmissionTableMismatch(ctx()),
        Refusal::HookPatternNotAdmitted(ctx()),
        Refusal::OcelEventNotAdmitted(ctx()),
        Refusal::LeastExpressiveRouteViolation(ctx()),
        Refusal::UnsupportedDialect(ctx()),
        Refusal::N3UnavailableByProfile(ctx()),
        Refusal::N3ActuationRefused(ctx()),
        Refusal::N3CostBoundExceeded(ctx()),
        Refusal::N3BuiltinRefused(ctx()),
        Refusal::N3DirectActuationRefused(ctx()),
        Refusal::RouteDecisionMismatch(ctx()),
        Refusal::GraphSnapshotMismatch(ctx()),
        Refusal::ProfileHashMismatch(ctx()),
        Refusal::AgentOverrideDenied(ctx()),
        Refusal::WitnessNotAuthority(ctx()),
        Refusal::BreedUnpermitted(ctx()),
        Refusal::NondeterministicOperatorRequiresReceipt(ctx()),
        Refusal::ProcessReceiptShadowType(ctx()),
        Refusal::DuplicateCanonicalTapeType(ctx()),
        Refusal::TripleTermInSnapshot(ctx()),
        Refusal::StageSealMismatch(ctx()),
        Refusal::UnlawfulActuation(ctx()),
        Refusal::PowlRegionNotAdmitted(ctx()),
        Refusal::ExternalCutUndeclared(ctx()),
        Refusal::ExternalCutTypeMismatch(ctx()),
        Refusal::ExternalCutAuthorityMismatch(ctx()),
        Refusal::ClosureLawNoChildren(ctx()),
        Refusal::ClosureLawQuorumOutOfRange(ctx()),
        Refusal::ClosureLawUnknownChild(ctx()),
        Refusal::ClosureLawOrderedSubsetInvalid(ctx()),
        Refusal::ClosureLawPolicyNotDeclared(ctx()),
        Refusal::ChildConformanceRefused(ctx()),
        Refusal::ChildCompletionUnadmitted(ctx()),
        Refusal::ParentClosureUnsatisfied(ctx()),
    ];
    assert_eq!(all.len(), ALL_REFUSAL_NAMES.len());
    for (refusal, expected) in all.iter().zip(ALL_REFUSAL_NAMES) {
        assert_eq!(refusal.name(), expected);
    }
}

// ---------------------------------------------------------------------------
// ABI behavior smoke checks (receipt identity is computed, never asserted).
// ---------------------------------------------------------------------------

#[test]
fn receipt_from_sorted_nquads_verifies() {
    let nquads = "<urn:a> <urn:p> <urn:x> <urn:g> .\n<urn:b> <urn:p> <urn:y> <urn:g> .";
    let receipt = praxis_graphlaw::chatman::abi::Receipt::from_canonical_nquads(
        "case-1",
        "chatman-gate",
        "rerun:case-1",
        nquads,
    )
    .expect("sorted canonical material must be admitted");
    receipt
        .verify()
        .expect("freshly computed receipt must verify");
    assert_eq!(receipt.envelope.digest.as_inner().len(), 64);
}

#[test]
fn receipt_refuses_unsorted_nquads() {
    let unsorted = "<urn:b> <urn:p> <urn:y> <urn:g> .\n<urn:a> <urn:p> <urn:x> <urn:g> .";
    let result = praxis_graphlaw::chatman::abi::Receipt::from_canonical_nquads(
        "case-2",
        "chatman-gate",
        "rerun:case-2",
        unsorted,
    );
    match result {
        Err(refusal) => assert_eq!(refusal.name(), "ValidationFailed"),
        Ok(_) => panic!("unsorted receipt material must be refused"),
    }
}

#[test]
fn envelope_hash_is_order_insensitive_over_handles() {
    use praxis_graphlaw::chatman::abi::{
        GraphSnapshotId, InputHandles, InvocationEnvelope, InvocationId, OperatorId, ProfileId,
    };
    let envelope = |nodes: &[&str]| InvocationEnvelope {
        invocation_id: InvocationId::new("inv-1"),
        snapshot_id: GraphSnapshotId::new("snap-1"),
        profile_id: ProfileId::new("profile-1"),
        operator_id: OperatorId::new("op-1"),
        input_handles: InputHandles {
            nodes: nodes.iter().map(|s| s.to_string()).collect(),
            events: vec![],
            plan_steps: vec![],
        },
    };
    let forward = envelope(&["urn:n1", "urn:n2"]).envelope_hash();
    let reversed = envelope(&["urn:n2", "urn:n1"]).envelope_hash();
    assert_eq!(forward, reversed, "handle order must not change the hash");
    let different = envelope(&["urn:n1", "urn:n3"]).envelope_hash();
    assert_ne!(forward, different, "different handle sets must hash apart");
}
