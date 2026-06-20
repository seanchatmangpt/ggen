use cpmp::ocel::{OcelEventLog, OcelReader};
use std::path::Path;

// ── helpers ──────────────────────────────────────────────────────────────────

/// Returns the path to the p2p fixture file (relative to the crate root at test time).
fn p2p_fixture_path() -> std::path::PathBuf {
    Path::new(env!("CARGO_MANIFEST_DIR"))
        .join("tests/fixtures/p2p.ocel.json")
}

/// Loads the purchase-to-pay fixture and fails the test on any error.
fn load_p2p() -> OcelEventLog {
    OcelReader::from_json_file(&p2p_fixture_path())
        .expect("p2p fixture must be valid OCEL 2.0 JSON")
}

// ── parsing ──────────────────────────────────────────────────────────────────

#[test]
fn from_json_parses_minimal_log() {
    let json = r#"{
      "objectTypes": [{"name": "order", "attributes": []}],
      "eventTypes":  [{"name": "Place Order", "attributes": []}],
      "objects": [{"id": "o1", "object_type": "order", "attributes": [], "relationships": []}],
      "events":  [{
          "id": "e1",
          "activity": "Place Order",
          "timestamp": "2026-01-01T00:00:00Z",
          "relationships": [{"object_id": "o1", "qualifier": "initiates"}],
          "attributes": []
      }]
    }"#;

    let log = OcelEventLog::from_json(json).expect("must parse");
    assert_eq!(log.event_count(), 1);
    assert_eq!(log.object_count(), 1);
}

#[test]
fn from_json_returns_parse_error_on_garbage_input() {
    let result = OcelEventLog::from_json("not json at all }{");
    assert!(result.is_err(), "garbage input must return Err");
}

#[test]
fn from_json_file_loads_p2p_fixture() {
    let log = load_p2p();
    assert_eq!(log.event_count(), 6);
    assert_eq!(log.object_count(), 3);
}

// ── schema declarations ───────────────────────────────────────────────────────

#[test]
fn object_type_names_returns_all_declared_types() {
    let log = load_p2p();
    let types = log.object_type_names();
    // sorted
    assert_eq!(types, vec!["item", "order"]);
}

#[test]
fn activity_types_returns_sorted_distinct_activities() {
    let log = load_p2p();
    let acts = log.activity_types();
    assert_eq!(
        acts,
        vec!["Pick Item", "Place Order", "Receive Payment", "Ship Order"]
    );
    // no duplicates — "Place Order" appears twice in events but once here
    assert_eq!(acts.len(), 4);
}

// ── temporal ordering ─────────────────────────────────────────────────────────

#[test]
fn sorted_events_returns_ascending_timestamp_order() {
    let log = load_p2p();
    let sorted = log.sorted_events();
    assert_eq!(sorted.len(), 6);
    for w in sorted.windows(2) {
        assert!(
            w[0].timestamp <= w[1].timestamp,
            "events out of order: {} > {}",
            w[0].timestamp,
            w[1].timestamp
        );
    }
}

#[test]
fn sorted_events_first_is_earliest_timestamp() {
    let log = load_p2p();
    let sorted = log.sorted_events();
    assert_eq!(sorted[0].id, "e1");
    assert_eq!(sorted[0].activity, "Place Order");
}

// ── per-object event filtering ────────────────────────────────────────────────

#[test]
fn events_for_object_returns_only_order1_events_in_temporal_order() {
    let log = load_p2p();
    let events = log.events_for_object("order1");
    // order1 participates in e1, e3, e4, e5
    assert_eq!(events.len(), 4);
    assert_eq!(events[0].activity, "Place Order");
    assert_eq!(events[1].activity, "Receive Payment");
    assert_eq!(events[2].activity, "Pick Item");
    assert_eq!(events[3].activity, "Ship Order");
}

#[test]
fn events_for_object_returns_two_events_for_order2() {
    let log = load_p2p();
    let events = log.events_for_object("order2");
    assert_eq!(events.len(), 2);
    assert_eq!(events[0].activity, "Place Order");
    assert_eq!(events[1].activity, "Receive Payment");
}

#[test]
fn events_for_nonexistent_object_returns_empty() {
    let log = load_p2p();
    let events = log.events_for_object("does-not-exist");
    assert!(events.is_empty());
}

// ── directly-follows relation ─────────────────────────────────────────────────

#[test]
fn directly_follows_for_order1_has_three_pairs() {
    let log = load_p2p();
    let pairs = log.directly_follows_for_object("order1");
    assert_eq!(pairs.len(), 3);
    assert_eq!(pairs[0], ("Place Order", "Receive Payment"));
    assert_eq!(pairs[1], ("Receive Payment", "Pick Item"));
    assert_eq!(pairs[2], ("Pick Item", "Ship Order"));
}

#[test]
fn directly_follows_for_item1_has_one_pair() {
    let log = load_p2p();
    let pairs = log.directly_follows_for_object("item1");
    assert_eq!(pairs.len(), 1);
    assert_eq!(pairs[0], ("Pick Item", "Ship Order"));
}

// ── DFG (graph-level) ─────────────────────────────────────────────────────────

#[test]
fn directly_follows_graph_has_three_distinct_edges() {
    let log = load_p2p();
    let dfg = log.directly_follows_graph();
    // ("Place Order", "Receive Payment"): order1 + order2 = 2
    // ("Receive Payment", "Pick Item"):   order1 only = 1
    // ("Pick Item", "Ship Order"):        order1 + item1 = 2
    assert_eq!(dfg.len(), 3);
    assert_eq!(
        dfg[&("Place Order".into(), "Receive Payment".into())],
        2,
        "Place Order → Receive Payment must have count 2"
    );
    assert_eq!(
        dfg[&("Receive Payment".into(), "Pick Item".into())],
        1
    );
    assert_eq!(
        dfg[&("Pick Item".into(), "Ship Order".into())],
        2
    );
}

// ── process variants ──────────────────────────────────────────────────────────

#[test]
fn variants_per_object_type_identifies_two_order_variants() {
    let log = load_p2p();
    let variants = log.variants_per_object_type();
    let order_variants = variants
        .get("order")
        .expect("order type must be present in variants");
    // order1: Place Order -> Receive Payment -> Pick Item -> Ship Order
    // order2: Place Order -> Receive Payment
    assert_eq!(order_variants.len(), 2);
    assert_eq!(
        order_variants
            ["Place Order -> Receive Payment -> Pick Item -> Ship Order"],
        1
    );
    assert_eq!(order_variants["Place Order -> Receive Payment"], 1);
}

#[test]
fn variants_per_object_type_item_type_has_one_variant() {
    let log = load_p2p();
    let variants = log.variants_per_object_type();
    let item_variants = variants
        .get("item")
        .expect("item type must be present in variants");
    assert_eq!(item_variants.len(), 1);
    assert_eq!(item_variants["Pick Item -> Ship Order"], 1);
}

// ── stats ─────────────────────────────────────────────────────────────────────

#[test]
fn stats_counts_are_correct_for_p2p_fixture() {
    let log = load_p2p();
    let s = log.stats();
    assert_eq!(s.event_count, 6);
    assert_eq!(s.object_count, 3);
    assert_eq!(s.activity_type_count, 4);
    assert_eq!(s.object_type_count, 2);
    assert_eq!(s.dfg_edge_count, 3);
    assert_eq!(s.variant_count, 3); // 2 order variants + 1 item variant
}

// ── event helpers ─────────────────────────────────────────────────────────────

#[test]
fn object_ids_returns_all_related_object_ids() {
    let log = load_p2p();
    // e4 relates to both order1 and item1
    let e4 = log.events.iter().find(|e| e.id == "e4").unwrap();
    let mut ids = e4.object_ids();
    ids.sort_unstable();
    assert_eq!(ids, vec!["item1", "order1"]);
}

#[test]
fn qualifiers_for_returns_correct_qualifier() {
    let log = load_p2p();
    let e4 = log.events.iter().find(|e| e.id == "e4").unwrap();
    let qs = e4.qualifiers_for("order1");
    assert_eq!(qs, vec!["fulfills"]);
    let qs_item = e4.qualifiers_for("item1");
    assert_eq!(qs_item, vec!["allocated"]);
}

#[test]
fn qualifiers_for_unknown_object_returns_empty() {
    let log = load_p2p();
    let e1 = log.events.iter().find(|e| e.id == "e1").unwrap();
    let qs = e1.qualifiers_for("no-such-object");
    assert!(qs.is_empty());
}

// ── o2o relationships ─────────────────────────────────────────────────────────

#[test]
fn order2_has_one_o2o_relationship_to_order1() {
    let log = load_p2p();
    let order2 = log
        .objects
        .iter()
        .find(|o| o.id == "order2")
        .expect("order2 must exist");
    assert_eq!(order2.relationships.len(), 1);
    assert_eq!(order2.relationships[0].target_object_id, "order1");
    assert_eq!(order2.relationships[0].qualifier, "referenced-by");
}
