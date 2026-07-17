//! PROJ-783 (PRD.md v26.7.11 sec.18): end-to-end proof that four of the
//! typed refusal catalog's codes actually fire through this crate's real,
//! `pub` admission-side API -- `praxis_graphlaw::chatman::powl_projection`
//! -- not a mocked or crate-internal-only check.
//!
//! Follows the capstone-test pattern established this session
//! (`crates/wasm4pm-arazzo/tests/end_to_end_lowering.rs`,
//! `crates/praxis-core/tests/rail_ab_external_cut_wiring.rs`): real
//! malformed/inadmissible input through the real pipeline function, the
//! specific typed refusal fires, exercised from outside the crate (proving
//! the function is genuinely part of the public admission surface).
//!
//! Codes covered: `POWL_REGION_NOT_ADMITTED`, `EXTERNAL_CUT_UNDECLARED`,
//! `EXTERNAL_CUT_TYPE_MISMATCH`, `EXTERNAL_CUT_AUTHORITY_MISMATCH`.

use std::collections::BTreeSet;

use powl2_decompose::{Powl, SocketPath};
use praxis_graphlaw::chatman::abi::Refusal;
use praxis_graphlaw::chatman::powl_projection::{powl_to_turtle, resolve_external_cut_at};

/// A two-step `PartialOrder` whose second child is a declared external cut
/// -- mirrors the fixture already established in
/// `powl_projection::tests::model_with_external_cut` and
/// `praxis_core::arazzo::tests::model_with_external_cut`.
fn model_with_external_cut() -> Powl {
    Powl::PartialOrder {
        children: vec![
            Powl::Leaf(Some("intake".to_string())),
            Powl::ExternalCut {
                region: Box::new(Powl::Leaf(Some("remote_settle".to_string()))),
                projection: "SELECT * WHERE { ?s ?p ?o }".to_string(),
                renderer: "arazzo_step.tera".to_string(),
            },
        ],
        order: BTreeSet::from([(0usize, 1usize)]),
    }
}

#[test]
fn external_cut_undeclared_fires_on_a_real_empty_projection() {
    let model = Powl::ExternalCut {
        region: Box::new(Powl::Leaf(Some("x".to_string()))),
        projection: String::new(),
        renderer: "t".to_string(),
    };
    let result = powl_to_turtle(&model, "urn:e2e:proj783:undeclared", None);
    assert_eq!(
        result.as_ref().err().map(Refusal::name),
        Some("ExternalCutUndeclared"),
        "got: {result:?}"
    );
}

#[test]
fn powl_region_not_admitted_fires_on_a_real_nested_cut() {
    let inner_cut = Powl::ExternalCut {
        region: Box::new(Powl::Leaf(Some("b".to_string()))),
        projection: "q".to_string(),
        renderer: "t".to_string(),
    };
    let outer_cut = Powl::ExternalCut {
        region: Box::new(inner_cut),
        projection: "q2".to_string(),
        renderer: "t2".to_string(),
    };
    let model = Powl::PartialOrder {
        children: vec![Powl::Leaf(Some("a".to_string())), outer_cut],
        order: BTreeSet::new(),
    };
    let result = powl_to_turtle(&model, "urn:e2e:proj783:nested-cut", None);
    assert_eq!(
        result.as_ref().err().map(Refusal::name),
        Some("PowlRegionNotAdmitted"),
        "got: {result:?}"
    );
}

#[test]
fn external_cut_type_mismatch_fires_on_a_real_socket_path_naming_a_leaf() {
    let model = model_with_external_cut();
    // Index 0 is the plain "intake" leaf, not the declared ExternalCut.
    let path = SocketPath::root().child(0);
    let result = resolve_external_cut_at(&model, &path);
    assert_eq!(
        result.as_ref().err().map(Refusal::name),
        Some("ExternalCutTypeMismatch"),
        "got: {result:?}"
    );
}

#[test]
fn external_cut_authority_mismatch_fires_on_a_real_cross_tenant_provenance_claim() {
    let model = model_with_external_cut();
    let result = powl_to_turtle(
        &model,
        "urn:tenant-a:region",
        Some("urn:tenant-b:other-snapshot"),
    );
    assert_eq!(
        result.as_ref().err().map(Refusal::name),
        Some("ExternalCutAuthorityMismatch"),
        "got: {result:?}"
    );
}

#[test]
fn a_well_formed_external_cut_is_admitted_end_to_end() -> Result<(), Refusal> {
    let model = model_with_external_cut();
    let path = SocketPath::root().child(1);
    resolve_external_cut_at(&model, &path)?;
    let turtle = powl_to_turtle(&model, "urn:e2e:proj783:ok", Some("urn:e2e:proj783:ok"))?;
    assert!(turtle.contains("powl2:ExternalCut"));
    Ok(())
}
