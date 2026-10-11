//! Capability-aware composer kernel tests (lane composer-test).
//!
//! Targets the deterministic composition kernel in
//! `packs_registry/composer.rs`: `compose`, `PackCompositionPlan`,
//! `CompositionRefusal`. Real `pack.toml` fixtures in real TempDirs parsed by
//! the real `star_toml` parser (same call as `metadata.rs`). No mocks.

#![allow(clippy::unwrap_used, clippy::expect_used, clippy::panic)] // Chicago TDD: real-IO tests
use ggen_marketplace::packs_registry::composer::{compose, CompositionRefusal};
use ggen_marketplace::packs_registry::types::PackFile;
use std::fs;
use tempfile::TempDir;

/// Write a pack.toml fixture to a real TempDir and parse it with the real
/// parser (same `star_toml::from_str::<PackFile>` call as metadata.rs).
fn parse_fixture(toml_body: &str) -> PackFile {
    let dir = TempDir::new().expect("tempdir");
    let path = dir.path().join("pack.toml");
    fs::write(&path, toml_body).expect("write fixture");
    let content = fs::read_to_string(&path).expect("read fixture back");
    star_toml::from_str(&content).expect("fixture must parse")
}

fn pack_toml(
    id: &str, packages: &[&str], deps: &[(&str, bool)], provides: &[&str], requires: &[&str],
    template_paths: &[&str],
) -> String {
    let mut s = format!(
        "[pack]\nid = \"{id}\"\nname = \"{id}\"\nversion = \"1.0.0\"\n\
         description = \"fixture {id}\"\ncategory = \"test\"\npackages = [{}]\n",
        packages
            .iter()
            .map(|p| format!("\"{p}\""))
            .collect::<Vec<_>>()
            .join(", ")
    );
    if !deps.is_empty() {
        s.push_str("\n[[pack.dependencies]]\n");
        for (i, (dep, optional)) in deps.iter().enumerate() {
            if i > 0 {
                s.push_str("\n[[pack.dependencies]]\n");
            }
            s.push_str(&format!(
                "pack_id = \"{dep}\"\nversion = \"1.0.0\"\noptional = {optional}\n"
            ));
        }
    }
    if !template_paths.is_empty() {
        for (i, p) in template_paths.iter().enumerate() {
            s.push_str(&format!(
                "\n[[pack.templates]]\nname = \"t{i}\"\npath = \"{p}\"\n\
                 description = \"template {i}\"\n"
            ));
        }
    }
    if !provides.is_empty() || !requires.is_empty() {
        s.push_str("\n[capabilities]\n");
        if !provides.is_empty() {
            s.push_str(&format!(
                "provides = [{}]\n",
                provides
                    .iter()
                    .map(|p| format!("\"{p}\""))
                    .collect::<Vec<_>>()
                    .join(", ")
            ));
        }
        if !requires.is_empty() {
            s.push_str(&format!(
                "requires = [{}]\n",
                requires
                    .iter()
                    .map(|p| format!("\"{p}\""))
                    .collect::<Vec<_>>()
                    .join(", ")
            ));
        }
    }
    s
}

// (1) Surface union correctness.

#[test]
fn surface_union_package_only() {
    let a = parse_fixture(&pack_toml("a", &["pkg-a"], &[], &[], &[], &[]));
    let b = parse_fixture(&pack_toml("b", &[], &[("a", false)], &[], &[], &[]));
    let plan = compose(&[a, b]).expect("compose");
    assert!(plan.provides.contains_key("pkg-a"));
    assert_eq!(
        plan.provides.get("pkg-a").map(|s| s.len()),
        Some(1),
        "single provider, no DuplicateCapability"
    );
    assert!(plan.self_satisfied.is_empty());
}

#[test]
fn surface_union_capability_only() {
    // a provides only a capability URN; b requires it — bound via union.
    let a = parse_fixture(&pack_toml(
        "a",
        &[],
        &[],
        &["urn:ggen:pack:cap-x"],
        &[],
        &[],
    ));
    let b = parse_fixture(&pack_toml(
        "b",
        &[],
        &[],
        &[],
        &["urn:ggen:pack:cap-x"],
        &[],
    ));
    let plan = compose(&[a, b]).expect("capability-only binding composes");
    assert_eq!(
        plan.provides.get("urn:ggen:pack:cap-x").map(|s| s.len()),
        Some(1)
    );
    assert!(plan.self_satisfied.is_empty());
}

#[test]
fn surface_union_both_channels_merge_into_one_universe() {
    let a = parse_fixture(&pack_toml(
        "a",
        &["pkg-a"],
        &[],
        &["urn:ggen:pack:cap-a"],
        &[],
        &[],
    ));
    let plan = compose(&[a]).expect("compose");
    assert_eq!(
        plan.provides.len(),
        2,
        "package + capability URN both present"
    );
    assert!(plan.provides.contains_key("pkg-a"));
    assert!(plan.provides.contains_key("urn:ggen:pack:cap-a"));
}

// (2) self_satisfied: a pack providing its own require via capabilities.

#[test]
fn self_satisfied_through_own_provides() {
    let a = parse_fixture(&pack_toml(
        "a",
        &[],
        &[],
        &["urn:ggen:pack:self-cap"],
        &["urn:ggen:pack:self-cap"],
        &[],
    ));
    let plan = compose(&[a]).expect("self-satisfied composition is legitimate");
    assert_eq!(plan.self_satisfied, vec!["a".to_string()]);
    // Provider == requirer must not create a self ordering edge stall.
    assert_eq!(plan.order, vec!["a".to_string()]);
}

#[test]
fn not_self_satisfied_when_bound_by_other_pack() {
    let a = parse_fixture(&pack_toml("a", &[], &[], &["urn:ggen:pack:cap"], &[], &[]));
    let b = parse_fixture(&pack_toml("b", &[], &[], &[], &["urn:ggen:pack:cap"], &[]));
    let plan = compose(&[a, b]).expect("compose");
    assert!(plan.self_satisfied.is_empty());
    // Capability ordering edge: provider a before requirer b.
    assert_eq!(plan.order, vec!["a".to_string(), "b".to_string()]);
}

// (3) Each of the 4 refusals.

#[test]
fn refusal_duplicate_capability() {
    let a = parse_fixture(&pack_toml("a", &["shared-pkg"], &[], &[], &[], &[]));
    let b = parse_fixture(&pack_toml("b", &["shared-pkg"], &[], &[], &[], &[]));
    let err = compose(&[a, b]).expect_err("duplicate package name must refuse");
    match err {
        CompositionRefusal::DuplicateCapability {
            capability,
            providers,
        } => {
            assert_eq!(capability, "shared-pkg");
            assert_eq!(providers, vec!["a".to_string(), "b".to_string()]);
        }
        other => panic!("wrong refusal: {other:?}"),
    }
}

#[test]
fn refusal_duplicate_capability_via_capability_channel() {
    let a = parse_fixture(&pack_toml("a", &[], &[], &["urn:ggen:pack:dup"], &[], &[]));
    let b = parse_fixture(&pack_toml("b", &[], &[], &["urn:ggen:pack:dup"], &[], &[]));
    let err = compose(&[a, b]).expect_err("duplicate capability URN must refuse");
    assert!(matches!(
        err,
        CompositionRefusal::DuplicateCapability { .. }
    ));
}

#[test]
fn refusal_unbound_requirement_dependency_channel() {
    let a = parse_fixture(&pack_toml("a", &[], &[("ghost", false)], &[], &[], &[]));
    let err = compose(&[a]).expect_err("missing dependency pack must refuse");
    match err {
        CompositionRefusal::UnboundRequirement {
            requiring_pack,
            required_pack,
        } => {
            assert_eq!(requiring_pack, "a");
            assert_eq!(required_pack, "ghost");
        }
        other => panic!("wrong refusal: {other:?}"),
    }
}

#[test]
fn refusal_unbound_requirement_capability_channel() {
    let a = parse_fixture(&pack_toml(
        "a",
        &[],
        &[],
        &[],
        &["urn:ggen:pack:missing"],
        &[],
    ));
    let err = compose(&[a]).expect_err("unbound capability require must refuse");
    assert!(matches!(err, CompositionRefusal::UnboundRequirement { .. }));
}

#[test]
fn optional_dependency_does_not_refuse() {
    let a = parse_fixture(&pack_toml("a", &[], &[("ghost", true)], &[], &[], &[]));
    let plan = compose(&[a]).expect("optional dependency never refuses");
    assert_eq!(plan.order, vec!["a".to_string()]);
}

#[test]
fn refusal_duplicate_artifact_path() {
    let a = parse_fixture(&pack_toml("a", &[], &[], &[], &[], &["out/shared.txt"]));
    let b = parse_fixture(&pack_toml("b", &[], &[], &[], &[], &["out/shared.txt"]));
    let err = compose(&[a, b]).expect_err("clobbered output path must refuse");
    match err {
        CompositionRefusal::DuplicateArtifactPath { path, packs } => {
            assert_eq!(path, "out/shared.txt");
            assert_eq!(packs.len(), 2);
        }
        other => panic!("wrong refusal: {other:?}"),
    }
}

#[test]
fn refusal_cyclic_dependencies() {
    let a = parse_fixture(&pack_toml("a", &[], &[("b", false)], &[], &[], &[]));
    let b = parse_fixture(&pack_toml("b", &[], &[("a", false)], &[], &[], &[]));
    let err = compose(&[a, b]).expect_err("dependency cycle must refuse");
    assert!(matches!(err, CompositionRefusal::CyclicDependencies { .. }));
}

// (4) Capability cycle A<->B: no refusal, both cycle edges dropped,
// order deterministic. Repeat 10x — same result every time.

#[test]
fn capability_cycle_drops_edges_deterministically() {
    let expected = {
        let a = parse_fixture(&pack_toml(
            "a",
            &[],
            &[],
            &["urn:ggen:pack:cap-a"],
            &["urn:ggen:pack:cap-b"],
            &[],
        ));
        let b = parse_fixture(&pack_toml(
            "b",
            &[],
            &[],
            &["urn:ggen:pack:cap-b"],
            &["urn:ggen:pack:cap-a"],
            &[],
        ));
        compose(&[a, b]).expect("capability 2-cycle never refuses")
    };
    for i in 0..10 {
        let a = parse_fixture(&pack_toml(
            "a",
            &[],
            &[],
            &["urn:ggen:pack:cap-a"],
            &["urn:ggen:pack:cap-b"],
            &[],
        ));
        let b = parse_fixture(&pack_toml(
            "b",
            &[],
            &[],
            &["urn:ggen:pack:cap-b"],
            &["urn:ggen:pack:cap-a"],
            &[],
        ));
        let plan = compose(&[a, b]).expect("iteration must not refuse");
        assert_eq!(plan, expected, "iteration {i}: plan must be identical");
    }
    // A 2-cycle drops BOTH capability edges: order falls back to the
    // dependencies-only Kahn tie-break (lexicographic by pack id).
    assert_eq!(expected.order, vec!["a".to_string(), "b".to_string()]);
}

// (5) BTreeSet dedup + deny_unknown_fields through the real parser.

#[test]
fn capabilities_duplicate_urns_dedup_silently() {
    let a = parse_fixture(&pack_toml(
        "a",
        &[],
        &[],
        &["urn:ggen:pack:x", "urn:ggen:pack:x"],
        &[],
        &[],
    ));
    let plan = compose(&[a]).expect("deduped provides compose");
    assert_eq!(
        plan.provides.get("urn:ggen:pack:x").map(|s| s.len()),
        Some(1),
        "duplicate URN collapses to one provider entry (same pack anyway)"
    );
    // Same pack claiming a URN twice must not self-refuse as duplicate.
}

#[test]
fn capabilities_unknown_key_refused_by_parser() {
    let dir = TempDir::new().expect("tempdir");
    let path = dir.path().join("pack.toml");
    fs::write(
        &path,
        format!(
            "{}\n[capabilities]\nprovide = [\"urn:ggen:pack:typo\"]\n",
            pack_toml("a", &[], &[], &[], &[], &[])
        ),
    )
    .expect("write fixture");
    let content = fs::read_to_string(&path).expect("read fixture back");
    let result: Result<PackFile, _> = star_toml::from_str(&content);
    let err = result.expect_err("deny_unknown_fields must refuse typo'd key");
    let msg = format!("{err}").to_lowercase();
    assert!(
        msg.contains("unknown") || msg.contains("provide"),
        "refusal should name the unknown field, got: {err}"
    );
}

// (6) Order determinism over shuffled input sets.

#[test]
fn plan_is_invariant_over_input_permutation() {
    let mk = || {
        vec![
            parse_fixture(&pack_toml("c", &[], &[("a", false)], &[], &[], &[])),
            parse_fixture(&pack_toml(
                "b",
                &[],
                &[("a", false)],
                &["urn:ggen:pack:cb"],
                &[],
                &[],
            )),
            parse_fixture(&pack_toml("a", &[], &[], &["urn:ggen:pack:ab"], &[], &[])),
        ]
    };
    let baseline = compose(&mk()).expect("compose");
    // Dependency edges: c->a, b->a. Capability edges: a provides ab used by... none
    // except b? b provides cb (unused). Order must be a, b, c regardless of input order.
    assert_eq!(
        baseline.order,
        vec!["a".to_string(), "b".to_string(), "c".to_string()]
    );
    for i in 0..10 {
        let mut packs = mk();
        let k = i % packs.len();
        packs.rotate_left(k);
        let plan = compose(&packs).expect("compose");
        assert_eq!(plan, baseline, "rotation {i}: plan must be identical");
    }
}

#[test]
fn capability_order_edge_respected_with_dependency_tiebreak() {
    // No dependency edges; capability edge a -> b must still order a first,
    // and with a third free pack c the Kahn BTreeSet tie-break keeps it
    // lexicographically deterministic across runs.
    let a = parse_fixture(&pack_toml("a", &[], &[], &["urn:ggen:pack:x"], &[], &[]));
    let b = parse_fixture(&pack_toml("b", &[], &[], &[], &["urn:ggen:pack:x"], &[]));
    let c = parse_fixture(&pack_toml("c", &[], &[], &[], &[], &[]));
    let plan = compose(&[c, b, a]).expect("compose");
    assert_eq!(
        plan.order,
        vec!["a".to_string(), "b".to_string(), "c".to_string()]
    );
}
