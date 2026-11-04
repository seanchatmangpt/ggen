//! WO-07 hardening courts for the depgraph resolver (Chicago: real TOML documents,
//! real extract/gate calls, assertions on returned `SyncPlan`/`Refusal` state).

use ggen_abb_sbb::depgraph::{
    extract_consumer_edges, extract_pack_manifest, resolve_sync_order, PackManifest,
};
use std::collections::BTreeSet;

fn manifest(name: &str) -> PackManifest {
    PackManifest {
        name: name.into(),
        depends_on: BTreeSet::new(),
        provides: BTreeSet::new(),
        requires: BTreeSet::new(),
        artifacts: BTreeSet::new(),
    }
}

// ---------------------------------------------------------------------------
// Acceptance 1: port-normalization court. Non-canonical ports (case variant and
// whitespace variant, provider side and requirer side) refuse with
// NON_CANONICAL_PORT naming the pack and port; canonical manifests extract clean.
// ---------------------------------------------------------------------------

#[test]
fn case_variant_port_provider_side_refuses() {
    let src = r#"
[pack]
name = "provider-pack"

[graph]
provides = ["Hashing"]
"#;
    let err = extract_pack_manifest(src).unwrap_err();
    match err {
        ggen_abb_sbb::Refusal::NonCanonicalPort { pack, field, port } => {
            assert_eq!(pack, "provider-pack");
            assert_eq!(field, "provides");
            assert_eq!(port, "Hashing");
        }
        other => panic!("wrong refusal: {other:?}"),
    }
}

#[test]
fn whitespace_variant_port_requirer_side_refuses() {
    let src = r#"
[pack]
name = "requirer-pack"

[graph]
requires = [" hashing"]
"#;
    let err = extract_pack_manifest(src).unwrap_err();
    match err {
        ggen_abb_sbb::Refusal::NonCanonicalPort { pack, field, port } => {
            assert_eq!(pack, "requirer-pack");
            assert_eq!(field, "requires");
            assert_eq!(port, " hashing");
        }
        other => panic!("wrong refusal: {other:?}"),
    }
}

#[test]
fn whitespace_only_port_refuses_as_empty() {
    let src = r#"
[pack]
name = "p"

[graph]
provides = ["   "]
"#;
    let err = extract_pack_manifest(src).unwrap_err();
    assert!(
        matches!(err, ggen_abb_sbb::Refusal::NonCanonicalPort { .. }),
        "got: {err}"
    );
}

#[test]
fn canonical_manifest_extracts_clean() {
    let src = r#"
[pack]
name = "ok-pack"

[graph]
provides = ["receipt", "hashing-256"]
requires = ["ledger"]
"#;
    let m = extract_pack_manifest(src).unwrap();
    assert!(m.provides.contains("receipt"));
    assert!(m.provides.contains("hashing-256"));
    assert!(m.requires.contains("ledger"));
}

#[test]
fn legacy_pack_without_graph_table_stays_dependency_free() {
    let src = "[pack]\nname = \"legacy-pack\"\n";
    let m = extract_pack_manifest(src).unwrap();
    assert_eq!(m.name, "legacy-pack");
    assert!(m.provides.is_empty());
    assert!(m.requires.is_empty());
}

#[test]
fn non_string_port_entry_refuses_instead_of_silent_drop() {
    let src = r#"
[pack]
name = "p"

[graph]
provides = [42]
"#;
    let err = extract_pack_manifest(src).unwrap_err();
    assert!(err.to_string().contains("non-string entry"), "got: {err}");
}

// ---------------------------------------------------------------------------
// Acceptance 3: consumer `requires` court. A consumer ggen.toml with
// `[graph] requires = ["nope"]` and no provider refuses with the consumer's
// name; with requires absent the legacy path stays clean.
// ---------------------------------------------------------------------------

#[test]
fn consumer_requires_unsatisfied_refuses_naming_consumer() {
    let packs = vec![manifest("a")];
    let src = r#"
[graph]
name = "my-consumer"
depends_on = ["a"]
requires = ["nope"]
"#;
    let consumer = extract_consumer_edges(src).unwrap().expect("graph table");
    assert_eq!(consumer.requires, std::iter::once("nope".to_string()).collect::<BTreeSet<_>>());
    let err = resolve_sync_order(&packs, Some(&consumer)).unwrap_err();
    match err {
        ggen_abb_sbb::Refusal::UnboundPort { pack, port } => {
            assert_eq!(pack, "my-consumer");
            assert_eq!(port, "nope");
        }
        other => panic!("wrong refusal: {other:?}"),
    }
}

#[test]
fn consumer_requires_satisfied_by_any_pack_passes() {
    let packs = vec![
        manifest("far"),
        PackManifest {
            provides: std::iter::once("hashing".to_string()).collect(),
            ..manifest("provider")
        },
    ];
    let src = r#"
[graph]
name = "my-consumer"
depends_on = ["far"]
requires = ["hashing"]
"#;
    let consumer = extract_consumer_edges(src).unwrap().expect("graph table");
    // Consumer depends only on `far` but the port is bound by `provider`: the
    // consumer consumes every pack, so the check ranges over all provides.
    let plan = resolve_sync_order(&packs, Some(&consumer)).unwrap();
    assert_eq!(plan.order, vec!["far", "provider", "my-consumer"]);
}

#[test]
fn consumer_requires_canonical_port_law_applies_to_consumer_too() {
    let src = r#"
[graph]
name = "my-consumer"
requires = ["Nope"]
"#;
    let err = extract_consumer_edges(src).unwrap_err();
    match err {
        ggen_abb_sbb::Refusal::NonCanonicalPort { pack, field, port } => {
            assert_eq!(pack, "my-consumer");
            assert_eq!(field, "requires");
            assert_eq!(port, "Nope");
        }
        other => panic!("wrong refusal: {other:?}"),
    }
}

#[test]
fn legacy_consumer_ggen_toml_without_requires_parses_clean() {
    let packs = vec![manifest("a")];
    let src = "[graph]\nname = \"old-consumer\"\ndepends_on = [\"a\"]\n";
    let consumer = extract_consumer_edges(src).unwrap().expect("graph table");
    assert!(consumer.requires.is_empty());
    let plan = resolve_sync_order(&packs, Some(&consumer)).unwrap();
    assert_eq!(plan.order, vec!["a", "old-consumer"]);
}

// ---------------------------------------------------------------------------
// Acceptance 2: closure-bound court. With fixed edges the fixpoint over V
// nodes always converges in <= V passes, so no input graph can trip the
// production bound (2*|packs|^2, floor 16) — the guard is fired through the
// bounded `closure_fixpoint` seam with a tight bound on a real chain graph
// that genuinely needs more passes than the bound allows.
// ---------------------------------------------------------------------------

#[test]
fn closure_bound_guard_fires_on_chain_graph_with_tight_bound() {
    use ggen_abb_sbb::depgraph::closure_fixpoint;
    use std::collections::BTreeMap;

    // A chain of 6 packs a0 -> a1 -> ... -> a5 (a0 depends on a1, ...).
    // Forward chaining needs ~V passes to propagate a5's closure to a0, so a
    // bound of 3 defeats convergence for a0 (and any node far enough down the
    // chain) — this is how a runaway graph behaves under the same rules.
    let mut edges: BTreeMap<String, BTreeSet<String>> = BTreeMap::new();
    let v = 6;
    for i in 0..v {
        let name = format!("a{i}");
        let mut deps = BTreeSet::new();
        if i + 1 < v {
            deps.insert(format!("a{}", i + 1));
        }
        edges.insert(name, deps);
    }
    let err = closure_fixpoint(&edges, 3).unwrap_err();
    match err {
        ggen_abb_sbb::Refusal::ClosureBoundExceeded { max_iterations } => {
            assert_eq!(max_iterations, 3);
        }
        other => panic!("wrong refusal: {other:?}"),
    }
}

#[test]
fn same_chain_converges_under_production_bound_and_gate_passes() {
    // End to end: the same chain shape through the real gate at the production
    // bound converges (no false positive on honest graphs).
    let packs: Vec<PackManifest> = (0..6)
        .map(|i| {
            let mut deps = BTreeSet::new();
            if i + 1 < 6 {
                deps.insert(format!("a{}", i + 1));
            }
            PackManifest {
                depends_on: deps,
                ..manifest(&format!("a{i}"))
            }
        })
        .collect();
    let plan = resolve_sync_order(&packs, None).unwrap();
    assert_eq!(plan.order.len(), 6);
    assert!(plan.transitive_deps["a0"].contains("a5"));
}

// ---------------------------------------------------------------------------
// Gate-4 pack-side behavior unchanged (property complement): a pack requiring
// a port bound only through a transitive dep still passes.
// ---------------------------------------------------------------------------

#[test]
fn pack_side_transitive_port_binding_still_passes() {
    let packs = vec![
        PackManifest {
            provides: std::iter::once("hashing".to_string()).collect(),
            ..manifest("base")
        },
        PackManifest {
            depends_on: std::iter::once("base".to_string()).collect(),
            ..manifest("mid")
        },
        PackManifest {
            depends_on: std::iter::once("mid".to_string()).collect(),
            requires: std::iter::once("hashing".to_string()).collect(),
            ..manifest("top")
        },
    ];
    let plan = resolve_sync_order(&packs, None).unwrap();
    assert_eq!(plan.order, vec!["base", "mid", "top"]);
}

// ---------------------------------------------------------------------------
// Variant-flip courts: the two dedicated Refusal variants carry the canonical
// serde tags and round-trip through the enum's tagged representation.
// ---------------------------------------------------------------------------

#[test]
fn non_canonical_port_variant_serializes_with_canonical_tag() {
    let r = ggen_abb_sbb::Refusal::NonCanonicalPort {
        pack: "p".into(),
        field: "provides".into(),
        port: "Hashing".into(),
    };
    let v = serde_json::to_value(&r).unwrap();
    assert_eq!(v["refusal"], "NON_CANONICAL_PORT");
    assert_eq!(v["pack"], "p");
    assert_eq!(v["field"], "provides");
    assert_eq!(v["port"], "Hashing");
    let back: ggen_abb_sbb::Refusal = serde_json::from_value(v).unwrap();
    assert_eq!(back, r);
    // Display carries the wire code prefix + remediation.
    let msg = r.to_string();
    assert!(msg.starts_with("NON_CANONICAL_PORT:"), "got: {msg}");
    assert!(msg.contains("rename the port"), "got: {msg}");
}

#[test]
fn closure_bound_exceeded_variant_serializes_with_canonical_tag() {
    let r = ggen_abb_sbb::Refusal::ClosureBoundExceeded { max_iterations: 16 };
    let v = serde_json::to_value(&r).unwrap();
    assert_eq!(v["refusal"], "CLOSURE_BOUND_EXCEEDED");
    assert_eq!(v["max_iterations"], 16);
    let back: ggen_abb_sbb::Refusal = serde_json::from_value(v).unwrap();
    assert_eq!(back, r);
    let msg = r.to_string();
    assert!(msg.starts_with("CLOSURE_BOUND_EXCEEDED:"), "got: {msg}");
    assert!(msg.contains("cannot converge"), "got: {msg}");
    assert!(msg.contains("16"), "got: {msg}");
}
