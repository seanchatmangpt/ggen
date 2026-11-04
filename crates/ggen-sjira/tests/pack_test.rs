//! Pack integration surface (PG-2 Slice 5, ds-3): the real mini-pack fixture
//! under `tests/fixtures/pack/`, exercised through the real ggen-graph
//! SHACL validator and the real Oxigraph SPARQL engine — no mocks.
//!
//! The term-closure flip: the fixture ontology is missing `sj:term`, so the
//! gate returns 1 row AND the shape court refuses; writing the missing term
//! flips both to green. Same mutation, two courts.

use std::path::Path;

use ggen_sjira::pack_health::{self, HealthRefusal};
use ggen_sjira::shacl::{self, ShaclRefusal};
use ggen_sjira::target_pack::{self, TargetPackRefusal, TargetPackRow};

fn fixture_pack() -> std::path::PathBuf {
    std::path::Path::new(env!("CARGO_MANIFEST_DIR")).join("tests/fixtures/pack")
}

fn copy_pack_to(dir: &Path) -> std::path::PathBuf {
    let dst = dir.join("mini-pack");
    std::fs::create_dir_all(dst.join("gates")).unwrap();
    std::fs::create_dir_all(dst.join("shapes")).unwrap();
    for (rel, src) in [
        ("ontology.ttl", fixture_pack().join("ontology.ttl")),
        (
            "shapes/work-order.shacl.ttl",
            fixture_pack().join("shapes/work-order.shacl.ttl"),
        ),
        (
            "gates/term-closure.rq",
            fixture_pack().join("gates/term-closure.rq"),
        ),
    ] {
        std::fs::copy(src, dst.join(rel)).unwrap();
    }
    dst
}

#[test]
fn fixture_shacl_court_conforms() {
    let pack = fixture_pack();
    let violations = shacl::run(&pack, &pack.join("ontology.ttl")).unwrap();
    assert!(violations.is_empty(), "fixture must conform: {violations:?}");
}

#[test]
fn term_closure_violation_flips_the_shacl_court() {
    let dir = tempfile::tempdir().unwrap();
    let pack = copy_pack_to(dir.path());
    // Mutate: drop capabilityId from the work order -> shape violation.
    std::fs::write(
        pack.join("ontology.ttl"),
        r#"@prefix sj: <http://example.org/sj#> .
sj:wo1 a sj:WorkOrder ; sj:targetPack "mini-pack" .
"#,
    )
    .unwrap();
    let err = shacl::run(&pack, &pack.join("ontology.ttl")).unwrap_err();
    let ShaclRefusal::ShaclViolations(vs) = &err else {
        panic!("expected violations, got {err:?}");
    };
    assert_eq!(vs.len(), 1);
    assert_eq!(vs[0].path, "http://example.org/sj#capabilityId");
    assert!(err.to_string().starts_with("REFUSED:SHACL_VIOLATIONS"));
}

#[test]
fn term_closure_gate_counts_dangling_work_orders() {
    let pack = fixture_pack();
    let summary = pack_health::check(&pack).unwrap();
    let gate = summary
        .gates
        .iter()
        .find(|g| g.name == "term-closure")
        .expect("term-closure gate present");
    // The fixture work order has no sj:term yet -> the gate sees 1 dangling row.
    assert_eq!(gate.rows, 1);
}

#[test]
fn writing_the_term_flips_the_gate_green() {
    let dir = tempfile::tempdir().unwrap();
    let pack = copy_pack_to(dir.path());
    std::fs::write(
        pack.join("ontology.ttl"),
        r#"@prefix sj: <http://example.org/sj#> .
sj:wo1 a sj:WorkOrder ;
    sj:targetPack "mini-pack" ;
    sj:capabilityId "cap-alpha" ;
    sj:term sj:term-alpha .
"#,
    )
    .unwrap();
    let summary = pack_health::check(&pack).unwrap();
    let gate = summary.gates.iter().find(|g| g.name == "term-closure").unwrap();
    assert_eq!(gate.rows, 0);
    assert!(summary.unsupported.contains("render.rs"));
}

#[test]
fn broken_gate_is_attributed_to_the_gate() {
    let dir = tempfile::tempdir().unwrap();
    let pack = copy_pack_to(dir.path());
    std::fs::write(
        pack.join("gates").join("broken.rq"),
        "SELECT ?x WHERE { ?x ??? }",
    )
    .unwrap();
    let err = pack_health::check(&pack).unwrap_err();
    let HealthRefusal::Gate { name, .. } = &err else {
        panic!("got {err:?}");
    };
    assert_eq!(name, "broken");
}

#[test]
fn target_pack_enforcement_stamps_and_refuses() {
    // Stamp: fixture pack copies into a real packs root.
    let dir = tempfile::tempdir().unwrap();
    let root = dir.path().join("packs");
    std::fs::create_dir_all(&root).unwrap();
    let pack = copy_pack_to(&root);
    let pack_name = pack.file_name().unwrap().to_str().unwrap().to_string();
    let rows = vec![TargetPackRow {
        work_order: "WO-1".into(),
        target_pack: pack_name.clone(),
    }];
    let stamp = target_pack::enforce(&rows, &root).unwrap();
    assert!(stamp.contains_key(&pack_name));
    assert!(!stamp[&pack_name].is_empty());

    // Refuse: an unknown pack names the declaring work order.
    let rows = vec![TargetPackRow {
        work_order: "WO-2".into(),
        target_pack: "no-such-pack".into(),
    }];
    let err = target_pack::enforce(&rows, &root).unwrap_err();
    let TargetPackRefusal::TargetUnknown { pack, work_order } = &err else {
        panic!("got {err:?}");
    };
    assert_eq!(pack, "no-such-pack");
    assert_eq!(work_order, "WO-2");
    assert!(err.to_string().starts_with("REFUSED:TARGET_PACK_UNKNOWN"));
}
