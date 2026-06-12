#![allow(
    clippy::unwrap_used,
    clippy::expect_used,
    clippy::panic,
    clippy::needless_raw_string_hashes,
    clippy::duration_suboptimal_units,
    clippy::branches_sharing_code,
    clippy::used_underscore_binding,
    clippy::single_char_pattern,
    clippy::ignore_without_reason,
    clippy::cloned_ref_to_slice_refs,
    clippy::doc_overindented_list_items,
    clippy::match_wildcard_for_single_variants,
    clippy::ignored_unit_patterns,
    clippy::needless_collect,
    clippy::unnecessary_map_or,
    clippy::manual_flatten,
    clippy::manual_strip,
    clippy::future_not_send,
    clippy::unnested_or_patterns,
    clippy::no_effect_underscore_binding,
    clippy::literal_string_with_formatting_args
)]
//! Adversarial DFG/conformance probes — interleaving, singletons, ties, isolation.
use chrono::{TimeZone, Utc};
use ggen_graph::ocel::{EvidenceProjector, OcelEvent, OcelLog, OcelObjectRef};
use ggen_graph::{check_lifecycle_order, discover_dfg, DeterministicGraph};
use std::collections::HashMap;

fn ev(id: &str, act: &str, secs: i64, case: &str) -> OcelEvent {
    OcelEvent {
        id: id.into(),
        activity: act.into(),
        timestamp: Utc.timestamp_opt(secs, 0).single().unwrap(),
        objects: vec![OcelObjectRef {
            id: case.into(),
            r#type: "diagnostic_code".into(),
            qualifier: Some("diag".into()),
        }],
        attributes: HashMap::new(),
    }
}
const Q: &str = "http://www.ocel-standard.org/ns#qualifier_diag";

#[test]
fn distinct_cases_do_not_create_cross_edges() {
    // Two cases interleaved in TIME but distinct case objects must NOT produce
    // A->X edges across cases.
    let log = OcelLog {
        objects: vec![],
        events: vec![
            ev("1", "A", 10, "c1"),
            ev("2", "X", 15, "c2"),
            ev("3", "B", 20, "c1"),
            ev("4", "Y", 25, "c2"),
        ],
    };
    let g = DeterministicGraph::new().unwrap();
    EvidenceProjector::project_ocel(&g, &log).unwrap();
    let edges = discover_dfg(&g, Q).unwrap();
    assert!(
        edges.iter().any(|e| e.source == "A" && e.target == "B"),
        "A->B within c1"
    );
    assert!(
        edges.iter().any(|e| e.source == "X" && e.target == "Y"),
        "X->Y within c2"
    );
    // The critical assertion: no edge crosses cases despite time interleaving.
    assert!(
        !edges
            .iter()
            .any(|e| (e.source == "A" && e.target == "X") || (e.source == "X" && e.target == "B")),
        "cross-case edges must not exist, got {edges:?}"
    );
}

#[test]
fn single_event_has_no_edges() {
    let log = OcelLog {
        objects: vec![],
        events: vec![ev("1", "Solo", 10, "c1")],
    };
    let g = DeterministicGraph::new().unwrap();
    EvidenceProjector::project_ocel(&g, &log).unwrap();
    assert!(
        discover_dfg(&g, Q).unwrap().is_empty(),
        "one event => no DFG edges"
    );
}

#[test]
fn empty_graph_yields_empty_dfg() {
    let g = DeterministicGraph::new().unwrap();
    assert!(discover_dfg(&g, Q).unwrap().is_empty());
}

#[test]
fn equal_timestamps_produce_no_direct_follow() {
    // Two events at the SAME timestamp: strict `<` means neither directly-follows
    // the other (no spurious edge, no panic).
    let log = OcelLog {
        objects: vec![],
        events: vec![ev("1", "A", 10, "c1"), ev("2", "B", 10, "c1")],
    };
    let g = DeterministicGraph::new().unwrap();
    EvidenceProjector::project_ocel(&g, &log).unwrap();
    let edges = discover_dfg(&g, Q).unwrap();
    assert!(
        !edges.iter().any(|e| e.source == "A" && e.target == "B"),
        "equal ts => no strict follow"
    );
}

#[test]
fn lifecycle_requires_all_three_in_order() {
    // Missing the middle step must fail conformance.
    let log = OcelLog {
        objects: vec![],
        events: vec![
            ev("1", "DiagnosticRaised", 10, "E1"),
            ev("3", "GatePassed", 30, "E1"),
        ],
    };
    let g = DeterministicGraph::new().unwrap();
    EvidenceProjector::project_ocel(&g, &log).unwrap();
    let ok =
        check_lifecycle_order(&g, Q, &["DiagnosticRaised", "RepairApplied", "GatePassed"]).unwrap();
    assert!(!ok, "missing RepairApplied must NOT conform");
}
