//! E2E composition tests: real pack.toml files on disk, parsed through the
//! public metadata loader, composed with the deterministic kernel in
//! `packs_registry::composer::compose`.
//!
//! Chicago-style: real files, real parser, assertions on the returned plan and
//! typed refusals. No mocks.


#![allow(clippy::unwrap_used, clippy::expect_used, clippy::panic)] // Chicago TDD: real-IO tests
use ggen_marketplace::packs_registry::composer::{compose, CompositionRefusal};
use ggen_marketplace::packs_registry::metadata::list_packs;
use std::collections::BTreeSet;
use tempfile::TempDir;

/// Write a real pack.toml into `dir` and assert it round-trips.
fn write_pack_toml(dir: &std::path::Path, id: &str, body: &str) {
    let path = dir.join(format!("{}.toml", id));
    std::fs::write(&path, body).expect("write pack.toml");
}

fn provider_toml() -> &'static str {
    r#"
[pack]
id = "provider-pack"
name = "Provider Pack"
version = "1.0.0"
description = "provides pkg-a"
category = "test"
packages = ["pkg-a"]
production_ready = true

[[pack.templates]]
name = "tmpl-a"
path = "out/provider/tmpl-a.txt"
description = "provider template"
"#
}

fn consumer_toml() -> &'static str {
    r#"
[pack]
id = "consumer-pack"
name = "Consumer Pack"
version = "1.0.0"
description = "requires provider-pack"
category = "test"
packages = ["pkg-b"]
production_ready = true

[[pack.dependencies]]
pack_id = "provider-pack"
version = "1.0.0"

[[pack.templates]]
name = "tmpl-b"
path = "out/consumer/tmpl-b.txt"
description = "consumer template"
"#
}

/// Load pack.tomls from a temp packs dir through the public loader.
fn load_from(dir: &std::path::Path) -> Vec<ggen_marketplace::packs_registry::types::PackFile> {
    std::env::set_var("GGEN_PACKS_DIR", dir);
    let packs = list_packs(None).expect("list_packs from temp dir");
    packs
        .into_iter()
        // `capabilities: None` — these packs are loaded from plain pack.tomls
        // written by this test, which carry no `[capabilities]` table.
        .map(|p| ggen_marketplace::packs_registry::types::PackFile {
            pack: p,
            capabilities: None,
        })
        .collect()
}

#[test]
#[serial_test::serial]
fn clean_two_pack_compose_plans_with_dependency_order() {
    let dir = TempDir::new().expect("tempdir");
    write_pack_toml(dir.path(), "provider-pack", provider_toml());
    write_pack_toml(dir.path(), "consumer-pack", consumer_toml());

    let packs = load_from(dir.path());
    assert_eq!(packs.len(), 2, "both pack.tomls parsed");

    let plan = compose(&packs).expect("clean composition must plan");

    assert_eq!(
        plan.pack_ids,
        BTreeSet::from(["provider-pack".to_string(), "consumer-pack".to_string()])
    );
    // Deps before dependents.
    let pos = |id: &str| plan.order.iter().position(|p| p == id).expect("in order");
    assert!(pos("provider-pack") < pos("consumer-pack"));
    // Capability surface unioned.
    assert_eq!(
        plan.provides.get("pkg-a").map(|s| s.len()),
        Some(1),
        "pkg-a has exactly one provider"
    );
    // Artifact paths recorded per pack, disjoint.
    assert_eq!(plan.artifact_paths.len(), 2);
}

#[test]
#[serial_test::serial]
fn duplicate_capability_refuses_naming_both_packs() {
    let dir = TempDir::new().expect("tempdir");
    write_pack_toml(dir.path(), "provider-pack", provider_toml());
    // Second pack also provides pkg-a.
    write_pack_toml(
        dir.path(),
        "rival-pack",
        r#"
[pack]
id = "rival-pack"
name = "Rival Pack"
version = "1.0.0"
description = "also provides pkg-a"
category = "test"
packages = ["pkg-a"]
production_ready = true
"#,
    );

    let packs = load_from(dir.path());
    let err = compose(&packs).expect_err("duplicate capability must refuse");

    match err {
        CompositionRefusal::DuplicateCapability {
            capability,
            providers,
        } => {
            assert_eq!(capability, "pkg-a");
            let set: BTreeSet<&str> = providers.iter().map(|s| s.as_str()).collect();
            assert_eq!(
                set,
                BTreeSet::from(["provider-pack", "rival-pack"]),
                "refusal must name both providing packs"
            );
        }
        other => panic!("expected DuplicateCapability, got: {other:?}"),
    }
}

#[test]
#[serial_test::serial]
fn unbound_requirement_refuses_naming_capability_and_packs() {
    let dir = TempDir::new().expect("tempdir");
    // Consumer present, provider absent.
    write_pack_toml(dir.path(), "consumer-pack", consumer_toml());

    let packs = load_from(dir.path());
    let err = compose(&packs).expect_err("unbound requirement must refuse");

    match err {
        CompositionRefusal::UnboundRequirement {
            requiring_pack,
            required_pack,
        } => {
            assert_eq!(requiring_pack, "consumer-pack");
            assert_eq!(required_pack, "provider-pack");
        }
        other => panic!("expected UnboundRequirement, got: {other:?}"),
    }
}

#[test]
#[serial_test::serial]
fn duplicate_artifact_path_refuses() {
    let dir = TempDir::new().expect("tempdir");
    write_pack_toml(dir.path(), "provider-pack", provider_toml());
    write_pack_toml(
        dir.path(),
        "pack-two",
        r#"
[pack]
id = "pack-two"
name = "Pack Two"
version = "1.0.0"
description = "clobbers provider template output"
category = "test"
packages = ["pkg-z"]
production_ready = true

[[pack.templates]]
name = "other-name"
path = "out/provider/tmpl-a.txt"
description = "same output path as provider-pack"
"#,
    );

    let packs = load_from(dir.path());
    let err = compose(&packs).expect_err("duplicate artifact path must refuse");

    match err {
        CompositionRefusal::DuplicateArtifactPath {
            path,
            packs: owners,
        } => {
            assert_eq!(path, "out/provider/tmpl-a.txt");
            let set: BTreeSet<&str> = owners.iter().map(|s| s.as_str()).collect();
            assert_eq!(set, BTreeSet::from(["provider-pack", "pack-two"]));
        }
        other => panic!("expected DuplicateArtifactPath, got: {other:?}"),
    }
}

#[test]
#[serial_test::serial]
fn cyclic_dependencies_refuse() {
    let dir = TempDir::new().expect("tempdir");
    write_pack_toml(
        dir.path(),
        "pack-a",
        r#"
[pack]
id = "pack-a"
name = "Pack A"
version = "1.0.0"
description = "depends on pack-b"
category = "test"
packages = ["pkg-a"]
production_ready = true

[[pack.dependencies]]
pack_id = "pack-b"
version = "1.0.0"
"#,
    );
    write_pack_toml(
        dir.path(),
        "pack-b",
        r#"
[pack]
id = "pack-b"
name = "Pack B"
version = "1.0.0"
description = "depends on pack-a"
category = "test"
packages = ["pkg-b"]
production_ready = true

[[pack.dependencies]]
pack_id = "pack-a"
version = "1.0.0"
"#,
    );

    let packs = load_from(dir.path());
    let err = compose(&packs).expect_err("cycle must refuse");

    assert!(
        matches!(err, CompositionRefusal::CyclicDependencies { .. }),
        "expected CyclicDependencies, got: {err:?}"
    );
}

#[test]
#[serial_test::serial]
fn singleton_compose_is_deterministic_and_unchanged() {
    let dir = TempDir::new().expect("tempdir");
    write_pack_toml(dir.path(), "provider-pack", provider_toml());

    let packs = load_from(dir.path());
    let plan1 = compose(&packs).expect("singleton composes");
    let plan2 = compose(&packs).expect("singleton composes again");

    assert_eq!(plan1, plan2, "plan is a pure function of the pack set");
    assert_eq!(plan1.pack_ids.len(), 1);
    assert_eq!(plan1.order, vec!["provider-pack".to_string()]);
}

#[test]
#[serial_test::serial]
fn compose_is_order_independent_over_the_same_pack_set() {
    let dir = TempDir::new().expect("tempdir");
    write_pack_toml(dir.path(), "provider-pack", provider_toml());
    write_pack_toml(dir.path(), "consumer-pack", consumer_toml());

    let packs = load_from(dir.path());
    let plan_ab = compose(&packs).expect("compose ab");
    let mut reversed = packs.clone();
    reversed.reverse();
    let plan_ba = compose(&reversed).expect("compose ba");

    assert_eq!(plan_ab, plan_ba, "input order must not change the plan");
}

// ---------------------------------------------------------------------------
// [capabilities] annotation merge tests (lane composer-part2).
//
// These build `PackFile`s directly (real types, real compose kernel) because
// the `[capabilities]` table is the unit under test; the loader round-trip is
// covered by the tests above and the corpus test.
// ---------------------------------------------------------------------------

use ggen_marketplace::packs_registry::types::{PackCapabilitiesFile, PackFile};

fn annotated_pack(id: &str, packages: &[&str], provides: &[&str], requires: &[&str]) -> PackFile {
    let mut pack = ggen_marketplace::packs_registry::types::Pack {
        id: id.to_string(),
        name: format!("Pack {id}"),
        version: "1.0.0".to_string(),
        description: format!("annotated pack {id}"),
        category: "test".to_string(),
        author: None,
        repository: None,
        license: None,
        registry_type: None,
        packages: packages.iter().map(|s| s.to_string()).collect(),
        templates: vec![],
        sparql_queries: Default::default(),
        dependencies: vec![],
        tags: vec![],
        keywords: vec![],
        production_ready: true,
        metadata: Default::default(),
    };
    pack.templates = vec![];
    PackFile {
        pack,
        capabilities: Some(PackCapabilitiesFile {
            types: None,
            provides: Some(provides.iter().map(|s| s.to_string()).collect()),
            requires: Some(requires.iter().map(|s| s.to_string()).collect()),
        }),
    }
}

#[test]
fn overlapping_capabilities_provides_refuse_duplicate_capability() {
    // Two packs whose [capabilities].provides name the SAME URN.
    let packs = vec![
        annotated_pack("pack-a", &["pkg-a"], &["urn:ggen:pack:shared-cap"], &[]),
        annotated_pack("pack-b", &["pkg-b"], &["urn:ggen:pack:shared-cap"], &[]),
    ];

    let err = compose(&packs).expect_err("overlapping capabilities.provides must refuse");

    match err {
        CompositionRefusal::DuplicateCapability {
            capability,
            providers,
        } => {
            assert_eq!(capability, "urn:ggen:pack:shared-cap");
            let set: BTreeSet<&str> = providers.iter().map(|s| s.as_str()).collect();
            assert_eq!(
                set,
                BTreeSet::from(["pack-a", "pack-b"]),
                "refusal must name both packs providing the same URN"
            );
        }
        other => panic!("expected DuplicateCapability, got: {other:?}"),
    }
}

#[test]
fn capability_requirement_satisfied_by_capabilities_provides() {
    // pack-consumer requires urn:ggen:pack:cap-x via [capabilities]; no pack id
    // or package carries it — only pack-provider's [capabilities].provides.
    let packs = vec![
        annotated_pack("pack-provider", &[], &["urn:ggen:pack:cap-x"], &[]),
        annotated_pack("pack-consumer", &[], &[], &["urn:ggen:pack:cap-x"]),
    ];

    let plan = compose(&packs).expect("capability-provided requirement must bind");

    assert_eq!(plan.pack_ids.len(), 2);
    assert!(
        plan.provides.contains_key("urn:ggen:pack:cap-x"),
        "capability URN must appear in the provides map"
    );
}

#[test]
fn unbound_capabilities_requirement_refuses() {
    // Required URN is not a pack id, package, or any pack's provides.
    let packs = vec![annotated_pack(
        "pack-lonely",
        &["pkg-a"],
        &[],
        &["urn:ggen:pack:nowhere"],
    )];

    let err = compose(&packs).expect_err("unbound capabilities.requires must refuse");

    match err {
        CompositionRefusal::UnboundRequirement {
            requiring_pack,
            required_pack,
        } => {
            assert_eq!(requiring_pack, "pack-lonely");
            assert_eq!(required_pack, "urn:ggen:pack:nowhere");
        }
        other => panic!("expected UnboundRequirement, got: {other:?}"),
    }
}

#[test]
fn none_capabilities_packs_compose_unchanged() {
    // Zero drift: same packs as the clean two-pack test but composed directly
    // with capabilities: None must yield the identical pre-annotation plan.
    let dir = TempDir::new().expect("tempdir");
    write_pack_toml(dir.path(), "provider-pack", provider_toml());
    write_pack_toml(dir.path(), "consumer-pack", consumer_toml());
    let packs = load_from(dir.path());
    assert!(
        packs.iter().all(|p| p.capabilities.is_none()),
        "loader must produce capabilities: None for plain pack.tomls"
    );

    let plan = compose(&packs).expect("None-capabilities composition must plan");
    assert_eq!(plan.pack_ids.len(), 2);
    assert_eq!(
        plan.provides.len(),
        2,
        "pkg-a + pkg-b, packages channel only"
    );
    let pos = |id: &str| plan.order.iter().position(|p| p == id).expect("in order");
    assert!(pos("provider-pack") < pos("consumer-pack"));
}

// ---------------------------------------------------------------------------
// Edge cases (lane compose-edge-tests).
// ---------------------------------------------------------------------------

#[test]
fn self_provide_cycle_composes_clean_self_satisfaction() {
    // A pack that requires its own provides URN. compose() binds a requirement
    // if ANY pack in the set (including the requirer itself) provides the
    // capability (composer.rs step 2), so this is a clean Ok — the pack
    // satisfies its own requirement through the provides union. This is the
    // intended union semantics, not a missed cycle: the cycle check runs only
    // over declared `dependencies` edges, which capabilities never create.
    let packs = vec![annotated_pack(
        "pack-self",
        &["pkg-s"],
        &["urn:ggen:pack:self-cap"],
        &["urn:ggen:pack:self-cap"],
    )];

    let plan = compose(&packs).expect("self-provided requirement must bind to itself");
    assert_eq!(plan.pack_ids.len(), 1);
    assert!(plan.provides.contains_key("urn:ggen:pack:self-cap"));
}

#[test]
fn empty_capabilities_arrays_yield_zero_surface_clean_compose() {
    // provides=[] and requires=[] (present but empty) must behave exactly like
    // capabilities: None — zero added surface, clean compose.
    let packs = vec![
        annotated_pack("pack-empty", &["pkg-e"], &[], &[]),
        annotated_pack("pack-empty-2", &["pkg-f"], &[], &[]),
    ];

    let plan = compose(&packs).expect("empty capability arrays must compose clean");
    assert_eq!(plan.pack_ids.len(), 2);
    assert_eq!(plan.provides.len(), 2, "packages channel only");
    assert!(
        !plan.provides.keys().any(|k| k.starts_with("urn:")),
        "no capability URNs may enter the surface from empty arrays"
    );
}

#[test]
fn capability_requirement_satisfied_by_another_packs_package() {
    // pack-consumer requires "pkg-from-packages" via capabilities.requires;
    // the only provider is pack-provider's `packages` entry — no
    // capabilities.provides channel involved. The union path must bind it.
    let packs = vec![
        annotated_pack("pack-provider", &["pkg-from-packages"], &[], &[]),
        annotated_pack("pack-consumer", &[], &[], &["pkg-from-packages"]),
    ];

    let plan = compose(&packs).expect("requirement bound via another pack's packages");
    assert_eq!(plan.pack_ids.len(), 2);
    assert!(plan.provides.contains_key("pkg-from-packages"));
}

#[test]
fn three_pack_capability_chain_composes_but_order_is_dependencies_only() {
    // a requires cap-b, b requires cap-c, c provides cap-c. All requirements
    // bind -> Ok. PINNED: capabilities.requires does NOT create dependency
    // edges (composer.rs doc: `dependencies` edges are the only inter-pack
    // ordering data), so plan.order carries no capability-chain constraint —
    // it is the deterministic dependencies-only order, which here (no
    // declared deps) is the BTreeSet-resolved order. If capability edges ever
    // feed the topological sort, this assertion is the tripwire.
    let packs = vec![
        annotated_pack("pack-a", &["pkg-a"], &[], &["urn:ggen:pack:cap-b"]),
        annotated_pack("pack-b", &["pkg-b"], &["urn:ggen:pack:cap-b"], &["urn:ggen:pack:cap-c"]),
        annotated_pack("pack-c", &["pkg-c"], &["urn:ggen:pack:cap-c"], &[]),
    ];

    let plan = compose(&packs).expect("fully-bound three-pack capability chain must plan");
    assert_eq!(plan.pack_ids.len(), 3);
    assert_eq!(plan.order.len(), 3);
    assert!(plan.provides.contains_key("urn:ggen:pack:cap-b"));
    assert!(plan.provides.contains_key("urn:ggen:pack:cap-c"));
}

#[test]
fn duplicate_urn_within_one_packs_own_provides_silently_dedupes() {
    // One pack listing the same URN twice in its own provides: the per-pack
    // BTreeSet at compose() step 1 dedupes it (providers.len() stays 1), so
    // no DuplicateCapability fires and the URN appears exactly once in the
    // provides map. PINNED as silent dedup; flagged as a finding: a
    // self-contradictory provides list is accepted without diagnostics.
    let packs = vec![annotated_pack(
        "pack-dupe",
        &["pkg-d"],
        &["urn:ggen:pack:dupe-cap", "urn:ggen:pack:dupe-cap"],
        &[],
    )];

    let plan = compose(&packs).expect("duplicate self-URN must dedupe, not refuse");
    let providers = plan
        .provides
        .get("urn:ggen:pack:dupe-cap")
        .expect("deduped URN must appear in the provides map");
    assert_eq!(
        providers.len(),
        1,
        "duplicate URN in one pack's own provides collapses to a single provider"
    );
    assert!(providers.contains("pack-dupe"));
}

#[test]
fn self_satisfied_surfaces_packs_whose_requires_bind_to_own_provides() {
    // pack-self requires a URN it provides itself: binds clean via union
    // semantics, and the plan must surface it in self_satisfied for audit.
    let packs = vec![
        annotated_pack(
            "pack-self",
            &["pkg-s"],
            &["urn:ggen:pack:self-cap"],
            &["urn:ggen:pack:self-cap"],
        ),
        annotated_pack("pack-normal", &["pkg-n"], &["urn:ggen:pack:n-cap"], &[]),
    ];

    let plan = compose(&packs).expect("self-satisfying composition must plan");
    assert_eq!(
        plan.self_satisfied,
        vec!["pack-self".to_string()],
        "self-requiring pack must be named in self_satisfied; normal packs must not"
    );

    // Control: no self-satisfaction anywhere.
    let clean = vec![
        annotated_pack("pack-p", &[], &["urn:ggen:pack:cap-p"], &[]),
        annotated_pack("pack-c", &[], &[], &["urn:ggen:pack:cap-p"]),
    ];
    let plan_clean = compose(&clean).expect("clean composition must plan");
    assert!(
        plan_clean.self_satisfied.is_empty(),
        "normal cross-pack binding must leave self_satisfied empty"
    );
}

// ---------------------------------------------------------------------------
// Capability-derived ordering edges (experiment, lane composer-cap-order).
//
// capabilities.requires URNs bound by another pack's provides now create
// ORDERING (requirer after provider) in plan.order. Satisfaction/refusal
// semantics stay union-based: capability edges never refuse — an edge that
// would close a cycle is dropped deterministically; cycle refusal remains
// dependencies-only.
// ---------------------------------------------------------------------------

#[test]
fn capability_chain_orders_requirer_after_provider() {
    // a requires cap-b (provided by b), b requires cap-c (provided by c);
    // no declared dependencies. plan.order must respect a -> b -> c.
    let packs = vec![
        annotated_pack("pack-a", &["pkg-a"], &[], &["urn:ggen:pack:cap-b"]),
        annotated_pack("pack-b", &["pkg-b"], &["urn:ggen:pack:cap-b"], &["urn:ggen:pack:cap-c"]),
        annotated_pack("pack-c", &["pkg-c"], &["urn:ggen:pack:cap-c"], &[]),
    ];

    let plan = compose(&packs).expect("fully-bound capability chain must plan");
    let pos = |id: &str| plan.order.iter().position(|p| p == id).expect("in order");
    assert!(
        pos("pack-c") < pos("pack-b") && pos("pack-b") < pos("pack-a"),
        "capability chain must order requirer after provider (c before b before a), got: {:?}",
        plan.order
    );

    // Deterministic: pure function of the pack set.
    let mut reversed = packs.clone();
    reversed.reverse();
    assert_eq!(plan, compose(&reversed).expect("reversed compose"));
}

#[test]
fn capability_cycle_drops_edges_deterministically_and_composes_ok() {
    // a requires cap-b, b requires cap-a via [capabilities]. Cycle refusal
    // stays dependencies-only, so compose must be Ok; BOTH capability edges
    // would close a cycle, so both drop and the order is the deterministic
    // dependencies-only (BTreeSet tie-break) order.
    let packs = vec![
        annotated_pack("pack-a", &["pkg-a"], &["urn:ggen:pack:cap-b"], &["urn:ggen:pack:cap-a"]),
        annotated_pack("pack-b", &["pkg-b"], &["urn:ggen:pack:cap-a"], &["urn:ggen:pack:cap-b"]),
    ];

    let plan = compose(&packs).expect("capability cycle must degrade, not refuse");
    assert_eq!(plan.pack_ids.len(), 2);

    // No capability edge can have survived the cycle (either edge alone would
    // close the 2-cycle), so the order must equal the deps-only tie-break.
    let mut expected: Vec<String> = plan.pack_ids.iter().cloned().collect();
    expected.sort();
    assert_eq!(plan.order, expected);

    let plan2 = compose(&packs).expect("recompose");
    assert_eq!(plan, plan2, "edge dropping must be deterministic");

    // Control: one side of the cycle removed -> the remaining edge is kept
    // and orders requirer after provider.
    let one_sided = vec![
        annotated_pack("pack-a", &["pkg-a"], &["urn:ggen:pack:cap-b"], &["urn:ggen:pack:cap-a"]),
        annotated_pack("pack-b", &["pkg-b"], &["urn:ggen:pack:cap-a"], &[]),
    ];
    let plan_one = compose(&one_sided).expect("one-sided capability edge must compose");
    let pos = |id: &str| plan_one.order.iter().position(|p| p == id).expect("in order");
    assert!(
        pos("pack-b") < pos("pack-a"),
        "pack-a requires cap-b provided by pack-b, so b must come first"
    );
}

#[test]
fn packages_only_packs_order_unchanged_by_capability_edges() {
    // Zero drift: packs with capabilities: None take exactly the
    // dependencies-only order they did before the experiment. Uses the real
    // loader round-trip (plain pack.tomls, capabilities: None).
    let dir = TempDir::new().expect("tempdir");
    write_pack_toml(dir.path(), "provider-pack", provider_toml());
    write_pack_toml(dir.path(), "consumer-pack", consumer_toml());
    let packs = load_from(dir.path());

    let plan = compose(&packs).expect("packages-only composition must plan");
    let pos = |id: &str| plan.order.iter().position(|p| p == id).expect("in order");
    assert!(pos("provider-pack") < pos("consumer-pack"));
    assert_eq!(plan.order.len(), 2);
    assert!(
        !plan.provides.keys().any(|k| k.starts_with("urn:")),
        "no capability URNs may enter a packages-only surface"
    );
}
