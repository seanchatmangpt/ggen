//! Benchmarks for realistic combined-dialect usage: hook admission, event firing,
//! release standing, process intelligence. These answer: "How fast does Graphlaw
//! feel when used normally?" — combining small amounts of multiple dialects
//! instead of stressing one dialect at scale.

#[macro_use]
extern crate bencher;

use bencher::Bencher;
use praxis_graphlaw::parser::Syntax;
use praxis_graphlaw::shacl::{ShapesGraph, Validator as ShaclValidator};
use praxis_graphlaw::TripleStore;

// =============================================================================
// Hook Pack Admission
// =============================================================================

/// Builds a realistic hook-pack Turtle with 25 hooks: mix of SPARQL and Datalog
/// conditions, emit-delta and refuse effects, dependencies, priorities.
fn build_hook_pack() -> String {
    r#"
        @prefix kh: <http://seanchatmangpt.github.io/praxis/kh#> .
        @prefix ex: <http://example.org/> .

        ex:hook_auth a kh:Hook ;
            kh:name "authentication_gate" ;
            kh:kind "sparql" ;
            kh:query "ASK { ?user ex:authenticated true }" ;
            kh:effect "refuse" ;
            kh:reason "User not authenticated" ;
            kh:priority 10 .

        ex:hook_acct_verify a kh:Hook ;
            kh:name "account_verified" ;
            kh:kind "sparql" ;
            kh:query "ASK { ?acct ex:account_status ex:verified }" ;
            kh:effect "refuse" ;
            kh:reason "Account not verified" ;
            kh:priority 9 ;
            kh:after ex:hook_auth .

        ex:hook_amount_check a kh:Hook ;
            kh:name "amount_threshold_check" ;
            kh:kind "sparql" ;
            kh:query "ASK { ?req ex:amount ?amt . FILTER(?amt > 10000) }" ;
            kh:effect "refuse" ;
            kh:reason "Amount exceeds policy limit" ;
            kh:priority 8 ;
            kh:after ex:hook_acct_verify .

        ex:hook_route_small a kh:Hook ;
            kh:name "route_small_request" ;
            kh:kind "sparql" ;
            kh:query "ASK { ?req ex:amount ?amt . FILTER(?amt <= 500) }" ;
            kh:effect "emit-delta" ;
            kh:action ex:action_auto_approve ;
            kh:priority 7 ;
            kh:after ex:hook_amount_check .

        ex:action_auto_approve a kh:Action ;
            kh:handler <http://seanchatmangpt.github.io/praxis/handler#sparql-construct> ;
            kh:query "CONSTRUCT { ?req ex:status ex:approved } WHERE { ?req ex:amount ?amt }" .

        ex:hook_route_medium a kh:Hook ;
            kh:name "route_medium_request" ;
            kh:kind "sparql" ;
            kh:query "ASK { ?req ex:amount ?amt . FILTER(?amt > 500 && ?amt <= 5000) }" ;
            kh:effect "emit-delta" ;
            kh:action ex:action_route_manager ;
            kh:priority 6 ;
            kh:after ex:hook_amount_check .

        ex:action_route_manager a kh:Action ;
            kh:handler <http://seanchatmangpt.github.io/praxis/handler#sparql-construct> ;
            kh:query "CONSTRUCT { ?req ex:status ex:pending_manager } WHERE { ?req ex:amount ?amt }" .

        ex:hook_route_large a kh:Hook ;
            kh:name "route_large_request" ;
            kh:kind "sparql" ;
            kh:query "ASK { ?req ex:amount ?amt . FILTER(?amt > 5000) }" ;
            kh:effect "emit-delta" ;
            kh:action ex:action_route_finance ;
            kh:priority 5 ;
            kh:after ex:hook_amount_check .

        ex:action_route_finance a kh:Action ;
            kh:handler <http://seanchatmangpt.github.io/praxis/handler#sparql-construct> ;
            kh:query "CONSTRUCT { ?req ex:status ex:pending_finance } WHERE { ?req ex:amount ?amt }" .

        ex:hook_balance_guard a kh:Hook ;
            kh:name "balance_guard" ;
            kh:kind "sparql" ;
            kh:query "ASK { ?acct ex:balance ?bal . FILTER(?bal < 0) }" ;
            kh:effect "refuse" ;
            kh:reason "Insufficient balance" ;
            kh:priority 4 ;
            kh:after ex:hook_acct_verify .

        ex:hook_conflict_detect a kh:Hook ;
            kh:name "policy_conflict_detection" ;
            kh:kind "sparql" ;
            kh:query "ASK { ?req ex:status ex:approved . ?req ex:status ex:rejected }" ;
            kh:effect "refuse" ;
            kh:reason "Conflicting policy decisions" ;
            kh:priority 3 ;
            kh:after ex:hook_route_small .

        ex:hook_audit_log a kh:Hook ;
            kh:name "audit_trail" ;
            kh:kind "sparql" ;
            kh:query "ASK { ?req ex:status ?status }" ;
            kh:effect "emit-delta" ;
            kh:action ex:action_log_event ;
            kh:priority 2 ;
            kh:after ex:hook_conflict_detect .

        ex:action_log_event a kh:Action ;
            kh:handler <http://seanchatmangpt.github.io/praxis/handler#sparql-construct> ;
            kh:query "CONSTRUCT { ?req ex:audit_ts ?ts } WHERE { ?req ex:status ?status }" .

        ex:hook_p13 a kh:Hook ;
            kh:name "hook_13" ;
            kh:kind "sparql" ;
            kh:query "ASK { ?x ex:p13 ?y }" ;
            kh:effect "emit-delta" ;
            kh:priority 1 .

        ex:hook_p14 a kh:Hook ;
            kh:name "hook_14" ;
            kh:kind "sparql" ;
            kh:query "ASK { ?x ex:p14 ?y }" ;
            kh:effect "refuse" ;
            kh:reason "p14 forbidden" ;
            kh:priority 1 .

        ex:hook_p15 a kh:Hook ;
            kh:name "hook_15" ;
            kh:kind "datalog" ;
            kh:program "derived(?x) :- base(?x)" ;
            kh:effect "emit-delta" ;
            kh:priority 1 .

        ex:hook_p16 a kh:Hook ;
            kh:name "hook_16" ;
            kh:kind "sparql" ;
            kh:query "ASK { ?x ex:p16 ?y }" ;
            kh:effect "emit-delta" ;
            kh:priority 1 .

        ex:hook_p17 a kh:Hook ;
            kh:name "hook_17" ;
            kh:kind "sparql" ;
            kh:query "ASK { ?x ex:p17 ?y }" ;
            kh:effect "emit-delta" ;
            kh:priority 1 .

        ex:hook_p18 a kh:Hook ;
            kh:name "hook_18" ;
            kh:kind "sparql" ;
            kh:query "ASK { ?x ex:p18 ?y }" ;
            kh:effect "emit-delta" ;
            kh:priority 1 .

        ex:hook_p19 a kh:Hook ;
            kh:name "hook_19" ;
            kh:kind "sparql" ;
            kh:query "ASK { ?x ex:p19 ?y }" ;
            kh:effect "emit-delta" ;
            kh:priority 1 .

        ex:hook_p20 a kh:Hook ;
            kh:name "hook_20" ;
            kh:kind "sparql" ;
            kh:query "ASK { ?x ex:p20 ?y }" ;
            kh:effect "emit-delta" ;
            kh:priority 1 .

        ex:hook_p21 a kh:Hook ;
            kh:name "hook_21" ;
            kh:kind "sparql" ;
            kh:query "ASK { ?x ex:p21 ?y }" ;
            kh:effect "emit-delta" ;
            kh:priority 1 .

        ex:hook_p22 a kh:Hook ;
            kh:name "hook_22" ;
            kh:kind "sparql" ;
            kh:query "ASK { ?x ex:p22 ?y }" ;
            kh:effect "emit-delta" ;
            kh:priority 1 .

        ex:hook_p23 a kh:Hook ;
            kh:name "hook_23" ;
            kh:kind "sparql" ;
            kh:query "ASK { ?x ex:p23 ?y }" ;
            kh:effect "emit-delta" ;
            kh:priority 1 .

        ex:hook_p24 a kh:Hook ;
            kh:name "hook_24" ;
            kh:kind "sparql" ;
            kh:query "ASK { ?x ex:p24 ?y }" ;
            kh:effect "emit-delta" ;
            kh:priority 1 .

        ex:hook_p25 a kh:Hook ;
            kh:name "hook_25" ;
            kh:kind "sparql" ;
            kh:query "ASK { ?x ex:p25 ?y }" ;
            kh:effect "emit-delta" ;
            kh:priority 1 .
    "#
    .to_string()
}

/// Admission of a hook pack: validates syntax, extracts hooks, orders dependencies.
/// Excludes data admission and materialization (see event_delta_firing for those).
fn hook_pack_admission_small(bencher: &mut Bencher) {
    let hook_pack = build_hook_pack();
    bencher.iter(|| {
        let mut store = TripleStore::new();
        let _ = store.load_hook_pack(&hook_pack);
        store.hooks.len()
    });
}

/// Admission of a hook pack with medium-scale data (500 triples).
/// Times only hook-pack extraction/compilation, not data admission.
fn hook_pack_admission_medium(bencher: &mut Bencher) {
    let hook_pack = build_hook_pack();
    let mut data = String::from("@prefix ex: <http://example.org/> .\n");
    for i in 0..500 {
        data.push_str(&format!("ex:obj{} ex:prop ex:val{} .\n", i, i));
    }
    bencher.iter(|| {
        let mut store = TripleStore::new();
        let _ = store.load_hook_pack(&hook_pack);
        let _ = store.load_triples(&data, Syntax::Turtle);
        (store.hooks.len(), store.triple_index.len())
    });
}

// =============================================================================
// Event Delta Firing (divan-style: untimed setup, timed delta application)
// =============================================================================

extern crate divan;

/// Event delta firing: baseline graph (1k triples), 25 compiled hooks,
/// apply a small delta (5-20 new facts), measure hook evaluation + effects.
/// Untimed: hook pack compilation. Timed: materialize the delta.
#[divan::bench]
fn event_delta_firing_small(bencher: divan::Bencher) {
    let hook_pack = build_hook_pack();
    let mut base_data = String::from("@prefix ex: <http://example.org/> .\n");
    for i in 0..1000 {
        base_data.push_str(&format!("ex:obj{} ex:prop ex:val{} .\n", i, i));
    }
    let delta_data = r#"
        @prefix ex: <http://example.org/> .
        ex:req1 ex:amount 300 .
        ex:user1 ex:authenticated true .
        ex:acct1 ex:account_status ex:verified .
        ex:acct1 ex:balance 5000 .
    "#;
    bencher
        .with_inputs(|| {
            let mut store = TripleStore::new();
            store
                .load_hook_pack(&hook_pack)
                .expect("failed to load hook pack");
            store
                .load_triples(&base_data, Syntax::Turtle)
                .expect("failed to load base data");
            store.materialize().unwrap();
            store
        })
        .bench_local_values(|mut store| {
            store
                .load_triples(delta_data, Syntax::Turtle)
                .expect("failed to load delta");
            let result = store.materialize().unwrap();
            (result.len(), store.get_hook_receipts().len())
        });
}

/// Event delta firing: medium scale (10k base triples).
#[divan::bench]
fn event_delta_firing_medium(bencher: divan::Bencher) {
    let hook_pack = build_hook_pack();
    let mut base_data = String::from("@prefix ex: <http://example.org/> .\n");
    for i in 0..10000 {
        base_data.push_str(&format!("ex:obj{} ex:prop ex:val{} .\n", i, i));
    }
    let delta_data = r#"
        @prefix ex: <http://example.org/> .
        ex:req2 ex:amount 7500 .
        ex:user2 ex:authenticated true .
        ex:acct2 ex:account_status ex:verified .
        ex:acct2 ex:balance 50000 .
    "#;
    bencher
        .with_inputs(|| {
            let mut store = TripleStore::new();
            store
                .load_hook_pack(&hook_pack)
                .expect("failed to load hook pack");
            store
                .load_triples(&base_data, Syntax::Turtle)
                .expect("failed to load base data");
            store.materialize().unwrap();
            store
        })
        .bench_local_values(|mut store| {
            store
                .load_triples(delta_data, Syntax::Turtle)
                .expect("failed to load delta");
            let result = store.materialize().unwrap();
            (result.len(), store.get_hook_receipts().len())
        });
}

// =============================================================================
// Release Standing Pass (SHACL + ShEx validation + OWL RL)
// =============================================================================

/// SHACL validation only: 100 artifacts against 10 property shapes.
fn release_standing_validation_shacl_only(bencher: &mut Bencher) {
    let shapes_str = r#"
        @prefix sh: <http://www.w3.org/ns/shacl#> .
        @prefix ex: <http://example.org/> .
        ex:ArtifactShape a sh:NodeShape ;
            sh:targetClass ex:Artifact ;
            sh:property [ sh:path ex:version ; sh:minCount 1 ; sh:maxCount 1 ] ;
            sh:property [ sh:path ex:status ; sh:in ( ex:draft ex:stable ex:deprecated ) ] ;
            sh:property [ sh:path ex:created ; sh:minCount 1 ] ;
            sh:property [ sh:path ex:updated ; sh:minCount 0 ] ;
            sh:property [ sh:path ex:author ; sh:minCount 1 ] ;
            sh:property [ sh:path ex:source ; sh:minCount 0 ] ;
            sh:property [ sh:path ex:hash ; sh:minCount 1 ] ;
            sh:property [ sh:path ex:lineage ; sh:minCount 0 ] ;
            sh:property [ sh:path ex:layer ; sh:minCount 0 ] ;
            sh:property [ sh:path ex:evidence ; sh:minCount 1 ] .
    "#;
    let shapes = ShapesGraph::parse(shapes_str).expect("failed to parse shapes");
    let mut data_str = String::from("@prefix ex: <http://example.org/> .\n");
    for i in 0..100 {
        data_str.push_str(&format!(
            "ex:artifact{} a ex:Artifact ; ex:version \"1.0.{}\" ; ex:status ex:stable ; ex:created \"2026-01-01\" ; ex:author ex:user1 ; ex:hash \"hash{}\" ; ex:evidence ex:evidence{} .\n",
            i, i, i, i
        ));
    }
    let mut store = TripleStore::new();
    store
        .load_triples(&data_str, Syntax::Turtle)
        .expect("failed to load data");
    bencher.iter(|| ShaclValidator::validate(&store.triple_index, &shapes));
}

/// Full release standing pass: SHACL validation + ShEx validation + OWL RL.
fn release_standing_full_pipeline(bencher: &mut Bencher) {
    let hook_pack = build_hook_pack();
    let shacl_shapes = r#"
        @prefix sh: <http://www.w3.org/ns/shacl#> .
        @prefix ex: <http://example.org/> .
        ex:ArtifactShape a sh:NodeShape ;
            sh:targetClass ex:Artifact ;
            sh:property [ sh:path ex:version ; sh:minCount 1 ] ;
            sh:property [ sh:path ex:status ; sh:minCount 1 ] ;
            sh:property [ sh:path ex:hash ; sh:minCount 1 ] .
    "#;
    let shex_schema = r#"
        {
            "type": "Schema",
            "shapes": [
                {
                    "id": "ex:ArtifactShape",
                    "type": "ShapeAnd",
                    "shapeExprs": [
                        {
                            "type": "ShapeNot",
                            "shapeExpr": {
                                "type": "Shape",
                                "closed": false,
                                "extra": ["rdf:type"],
                                "expression": {
                                    "type": "EachOf",
                                    "expressions": [
                                        { "type": "TripleConstraint", "predicate": "http://www.w3.org/1999/02/22-rdf-syntax-ns#type" }
                                    ]
                                }
                            }
                        }
                    ]
                }
            ]
        }
    "#;
    let shape_map = vec![("ex:artifact0".to_string(), "ex:ArtifactShape".to_string())];
    let mut data_str = String::from("@prefix ex: <http://example.org/> .\n");
    for i in 0..100 {
        data_str.push_str(&format!("ex:artifact{} ex:version \"1.0.{}\" .\n", i, i));
    }
    bencher.iter(|| {
        let mut store = TripleStore::new();
        store
            .load_hook_pack(&hook_pack)
            .expect("failed to load hook pack");
        store
            .load_triples(&data_str, Syntax::Turtle)
            .expect("failed to load data");
        let _ = store.validate_shacl(shacl_shapes);
        let _ = store.validate_shex(shex_schema, &shape_map);
        let _ = store.materialize_owlrl();
    });
}

// =============================================================================
// Process Intelligence Slice
// =============================================================================

/// Process intelligence: 100 process models, 1k events, 100 objects, 20 conformance
/// facts, 10 hooks, shallow 2-level ontology. Times full pipeline.
fn process_intelligence_slice(bencher: &mut Bencher) {
    let mut data = String::from("@prefix ex: <http://example.org/> .\n@prefix rdfs: <http://www.w3.org/2000/01/rdf-schema#> .\n");

    for i in 0..100 {
        data.push_str(&format!(
            "ex:proc{} a ex:ProcessModel ; rdfs:label \"Process {}\" .\n",
            i, i
        ));
    }
    for i in 0..1000 {
        data.push_str(&format!(
            "ex:event{} ex:processModel ex:proc{} .\n",
            i,
            i % 100
        ));
    }
    for i in 0..100 {
        data.push_str(&format!("ex:obj{} a ex:Object .\n", i));
    }
    for i in 0..20 {
        data.push_str(&format!("ex:conf{} ex:status ex:ok .\n", i));
    }

    let hook_pack = r#"
        @prefix kh: <http://seanchatmangpt.github.io/praxis/kh#> .
        @prefix ex: <http://example.org/> .
        ex:hook_conformance a kh:Hook ;
            kh:name "conformance_check" ;
            kh:kind "sparql" ;
            kh:query "ASK { ?e ex:status ex:ok }" ;
            kh:effect "emit-delta" ;
            kh:priority 1 .
    "#;

    bencher.iter(|| {
        let mut store = TripleStore::new();
        store
            .load_hook_pack(hook_pack)
            .expect("failed to load hook pack");
        store
            .load_triples(&data, Syntax::Turtle)
            .expect("failed to load data");
        store.materialize().unwrap();
        let _ = store.materialize_owlrl();
    });
}

benchmark_group!(
    standing_benches,
    hook_pack_admission_small,
    hook_pack_admission_medium,
    release_standing_validation_shacl_only,
    release_standing_full_pipeline,
    process_intelligence_slice,
);

benchmark_main!(standing_benches);
