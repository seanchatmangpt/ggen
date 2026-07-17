//! Benchmarks for the four RDF reasoning dialects (SHACL, ShEx, N3, Datalog)
//! at meaningful scale, complementing `lib/tests/*_stress.rs`'s correctness
//! checks with actual measured throughput. See those stress tests for the
//! scaling characteristics discovered during development (e.g. N3's
//! non-semi-naive fixpoint is roughly cubic in chain depth).

#[macro_use]
extern crate bencher;

use bencher::Bencher;
use praxis_graphlaw::datalog::validate_rules;
use praxis_graphlaw::parser::{Parser, Syntax};
use praxis_graphlaw::shacl::{ShapesGraph, Validator as ShaclValidator};
use praxis_graphlaw::shex::validate_shex;
use praxis_graphlaw::tripleindex::TripleIndex;
use praxis_graphlaw::triples::{Aggregate, AggregateFunction, BodyLiteral, Rule, Triple};
use praxis_graphlaw::TripleStore;
use std::collections::HashMap;

fn build_data_index(data_str: &str) -> TripleIndex {
    let triples = Parser::parse_triples(data_str, Syntax::Turtle).unwrap();
    let mut index = TripleIndex::new();
    for t in triples {
        index.add(t);
    }
    index
}

// ---------------------------------------------------------------------
// SHACL: validate N focus nodes against a multi-constraint property shape.
// ---------------------------------------------------------------------

fn shacl_validate_n(bench: &mut Bencher, n: usize) {
    let shapes_str = r#"
        @prefix sh: <http://www.w3.org/ns/shacl#> .
        @prefix ex: <http://example.org/> .
        ex:PersonShape a sh:NodeShape ;
            sh:targetClass ex:Person ;
            sh:property [ sh:path ex:age ; sh:minInclusive 0 ; sh:maxInclusive 150 ; sh:minCount 1 ; sh:maxCount 1 ] .
    "#;
    let shapes = ShapesGraph::parse(shapes_str).unwrap();
    let mut data_str = String::from("@prefix ex: <http://example.org/> .\n");
    for i in 0..n {
        data_str.push_str(&format!("ex:p{} a ex:Person ; ex:age {} .\n", i, i % 120));
    }
    let data = build_data_index(&data_str);
    bench.iter(|| ShaclValidator::validate(&data, &shapes));
}

fn shacl_validate_100(bench: &mut Bencher) {
    shacl_validate_n(bench, 100);
}
fn shacl_validate_1000(bench: &mut Bencher) {
    shacl_validate_n(bench, 1000);
}
fn shacl_validate_5000(bench: &mut Bencher) {
    shacl_validate_n(bench, 5000);
}

fn shacl_validate_complex_n(bench: &mut Bencher, n: usize) {
    let shapes_str = r#"
        @prefix sh: <http://www.w3.org/ns/shacl#> .
        @prefix ex: <http://example.org/> .
        ex:CompanyShape a sh:NodeShape ;
            sh:targetClass ex:Company ;
            sh:property [
                sh:path ex:address ;
                sh:property [
                    sh:path ex:city ;
                    sh:property [
                        sh:path ex:country ;
                        sh:class ex:Country ;
                    ] ;
                ] ;
            ] .
    "#;
    let shapes = ShapesGraph::parse(shapes_str).unwrap();
    let mut data_str = String::from("@prefix ex: <http://example.org/> .\n");
    for i in 0..n {
        data_str.push_str(&format!(
            "ex:c{} a ex:Company ; ex:address ex:addr{} .\n\
             ex:addr{} ex:city ex:city{} .\n\
             ex:city{} ex:country ex:country{} .\n\
             ex:country{} a ex:Country .\n",
            i, i, i, i, i, i, i
        ));
    }
    let data = build_data_index(&data_str);
    bench.iter(|| ShaclValidator::validate(&data, &shapes));
}

fn shacl_validate_complex_100(bench: &mut Bencher) {
    shacl_validate_complex_n(bench, 100);
}

fn shacl_validate_complex_1000(bench: &mut Bencher) {
    shacl_validate_complex_n(bench, 1000);
}

// ---------------------------------------------------------------------
// ShEx: validate N focus nodes against a single-datatype-constraint shape.
// ---------------------------------------------------------------------

fn shex_validate_n(bench: &mut Bencher, n: usize) {
    let schema_json = r#"{
      "@context": "http://www.w3.org/ns/shex.jsonld",
      "type": "Schema",
      "shapes": [{
        "type": "ShapeDecl", "id": "http://example.org/AgeShape",
        "shapeExpr": { "type": "Shape", "expression": {
          "type": "TripleConstraint", "predicate": "http://example.org/age",
          "valueExpr": { "type": "NodeConstraint", "datatype": "http://www.w3.org/2001/XMLSchema#integer" }
        }}
      }]
    }"#;
    let mut data_str = String::new();
    let mut shape_map = Vec::with_capacity(n);
    for i in 0..n {
        let node = format!("http://example.org/p{}", i);
        data_str.push_str(&format!("<{}> <http://example.org/age> {} .\n", node, i));
        shape_map.push((node, "http://example.org/AgeShape".to_string()));
    }
    let data = build_data_index(&data_str);
    bench.iter(|| validate_shex(&data, schema_json, &shape_map).unwrap());
}

fn shex_validate_100(bench: &mut Bencher) {
    shex_validate_n(bench, 100);
}
fn shex_validate_1000(bench: &mut Bencher) {
    shex_validate_n(bench, 1000);
}
fn shex_validate_5000(bench: &mut Bencher) {
    shex_validate_n(bench, 5000);
}

fn shex_validate_complex_n(bench: &mut Bencher, n: usize) {
    let schema_json = r#"{
      "@context": "http://www.w3.org/ns/shex.jsonld",
      "type": "Schema",
      "shapes": [{
        "type": "ShapeDecl", "id": "http://example.org/AndShape",
        "shapeExpr": { "type": "ShapeAnd", "shapeExprs": [
          { "type": "Shape", "expression": { "type": "TripleConstraint", "predicate": "http://example.org/val",
            "valueExpr": { "type": "NodeConstraint", "minlength": 3 } } },
          { "type": "Shape", "expression": { "type": "TripleConstraint", "predicate": "http://example.org/val",
            "valueExpr": { "type": "NodeConstraint", "maxlength": 8 } } }
        ] }
      }]
    }"#;
    let mut data_str = String::new();
    let mut shape_map = Vec::with_capacity(n);
    for i in 0..n {
        let node = format!("http://example.org/p{}", i);
        data_str.push_str(&format!(
            "<{}> <http://example.org/val> \"abcde\" .\n",
            node
        ));
        shape_map.push((node, "http://example.org/AndShape".to_string()));
    }
    let data = build_data_index(&data_str);
    bench.iter(|| validate_shex(&data, schema_json, &shape_map).unwrap());
}

fn shex_validate_complex_100(bench: &mut Bencher) {
    shex_validate_complex_n(bench, 100);
}

fn shex_validate_complex_1000(bench: &mut Bencher) {
    shex_validate_complex_n(bench, 1000);
}

fn shexc_parse_benchmark(bench: &mut Bencher) {
    let schema_shexc = r#"
        PREFIX ex: <http://example.org/>
        PREFIX xsd: <http://www.w3.org/2001/XMLSchema#>
        ex:AgeShape {
            ex:age xsd:integer
        }
    "#;
    bench.iter(|| {
        praxis_graphlaw::shexc_parser::parse_shexc(schema_shexc).unwrap();
    });
}

// ---------------------------------------------------------------------
// N3: forward-chain a linear rdfs:subClassOf-style transitivity chain of
// depth N (see lib/tests/n3_stress.rs for the cubic-ish scaling analysis).
// ---------------------------------------------------------------------

fn build_chain(depth: usize) -> String {
    let mut doc = String::from(
        "@prefix rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#>.\n\
         @prefix rdfs: <http://www.w3.org/2000/01/rdf-schema#>.\n\
         @prefix : <http://example.org/deep#>.\n\n\
         :TestVariable rdf:type :N0.\n\n",
    );
    for i in 0..depth {
        doc.push_str(&format!(":N{} rdfs:subClassOf :N{}.\n", i, i + 1));
    }
    doc.push_str("\n{?X a ?D} <= {?C rdfs:subClassOf ?D. ?X a ?C}.\n");
    doc
}

fn n3_chain_n(bench: &mut Bencher, depth: usize) {
    let doc = build_chain(depth);
    bench.iter(|| {
        let mut store = TripleStore::from(&doc);
        store.materialize().unwrap();
    });
}

fn n3_chain_depth_50(bench: &mut Bencher) {
    n3_chain_n(bench, 50);
}
fn n3_chain_depth_150(bench: &mut Bencher) {
    n3_chain_n(bench, 150);
}
fn n3_chain_depth_400(bench: &mut Bencher) {
    n3_chain_n(bench, 400);
}

fn n3_parse_benchmark(bench: &mut Bencher) {
    let rule_str = "{?a <http://example.org/links> ?b . ?b <http://example.org/links> ?c} => {?a <http://example.org/reach> ?c} .";
    bench.iter(|| {
        Parser::parse_n3_document(rule_str).unwrap();
    });
}

// ---------------------------------------------------------------------
// Datalog: stratified negation-chain validation + materialization at depth N.
// ---------------------------------------------------------------------

fn build_negation_chain(layers: usize) -> Vec<Rule> {
    let pred = |name: &str| format!("http://example.org/{}", name);
    let mut rules = vec![Rule {
        head: Triple::from(
            "?x".to_string(),
            pred("P0"),
            "http://example.org/true".to_string(),
        ),
        body: vec![BodyLiteral {
            negated: false,
            pattern: Triple::from(
                "?x".to_string(),
                pred("Base"),
                "http://example.org/true".to_string(),
            ),
        }],
    }];
    for i in 1..layers {
        rules.push(Rule {
            head: Triple::from(
                "?x".to_string(),
                pred(&format!("P{}", i)),
                "http://example.org/true".to_string(),
            ),
            body: vec![
                BodyLiteral {
                    negated: false,
                    pattern: Triple::from(
                        "?x".to_string(),
                        pred("Base"),
                        "http://example.org/true".to_string(),
                    ),
                },
                BodyLiteral {
                    negated: true,
                    pattern: Triple::from(
                        "?x".to_string(),
                        pred(&format!("P{}", i - 1)),
                        "http://example.org/true".to_string(),
                    ),
                },
            ],
        });
    }
    rules
}

fn datalog_stratify_n(bench: &mut Bencher, layers: usize) {
    let rules = build_negation_chain(layers);
    bench.iter(|| validate_rules(&rules, &HashMap::new()).unwrap());
}

fn datalog_stratify_layers_20(bench: &mut Bencher) {
    datalog_stratify_n(bench, 20);
}
fn datalog_stratify_layers_50(bench: &mut Bencher) {
    datalog_stratify_n(bench, 50);
}
fn datalog_stratify_layers_200(bench: &mut Bencher) {
    datalog_stratify_n(bench, 200);
}

// ---------------------------------------------------------------------
// Datalog: grouped aggregation over N facts across N/20 groups.
// ---------------------------------------------------------------------

fn datalog_aggregate_n(bench: &mut Bencher, num_facts: usize) {
    let num_depts = (num_facts / 20).max(1);
    bench.iter(|| {
        let mut store = TripleStore::new();
        for d in 0..num_depts {
            for e in 0..(num_facts / num_depts) {
                store.add(Triple::from(
                    format!("http://example.org/dept{}", d),
                    "http://example.org/hasEmployee".to_string(),
                    format!("http://example.org/emp{}_{}", d, e),
                ));
            }
        }
        let rule = Rule {
            head: Triple::from(
                "?d".to_string(),
                "http://example.org/employeeCount".to_string(),
                "?count".to_string(),
            ),
            body: vec![BodyLiteral {
                negated: false,
                pattern: Triple::from(
                    "?d".to_string(),
                    "http://example.org/hasEmployee".to_string(),
                    "?e".to_string(),
                ),
            }],
        };
        let agg = Aggregate {
            function: AggregateFunction::Count,
            source_var: "?e".to_string(),
            target_var: "?count".to_string(),
            group_vars: vec!["?d".to_string()],
        };
        store.add_rule_with_aggregate(rule, agg).unwrap();
        store.materialize().unwrap();
    });
}

fn datalog_aggregate_facts_1000(bench: &mut Bencher) {
    datalog_aggregate_n(bench, 1000);
}

benchmark_group!(
    dialect_benches,
    shacl_validate_100,
    shacl_validate_1000,
    shacl_validate_5000,
    shacl_validate_complex_100,
    shacl_validate_complex_1000,
    shex_validate_100,
    shex_validate_1000,
    shex_validate_5000,
    shex_validate_complex_100,
    shex_validate_complex_1000,
    shexc_parse_benchmark,
    n3_chain_depth_50,
    n3_chain_depth_150,
    n3_chain_depth_400,
    n3_parse_benchmark,
    datalog_stratify_layers_20,
    datalog_stratify_layers_50,
    datalog_stratify_layers_200,
    datalog_aggregate_facts_1000,
);
benchmark_main!(dialect_benches);
