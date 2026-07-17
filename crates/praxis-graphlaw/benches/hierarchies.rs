#[macro_use]
extern crate bencher;

use bencher::Bencher;

use praxis_graphlaw::parser::{Parser, Syntax};
use praxis_graphlaw::shacl::{ShapesGraph, Validator as ShaclValidator};
use praxis_graphlaw::tripleindex::TripleIndex;
use praxis_graphlaw::TripleStore;

fn infer_hierarchy(max_depth: i32) {
    let mut data = ":a a :U0\n".to_owned();
    for i in 0..max_depth {
        data += format!("{{?a a :U{}}}=>{{?a a :U{}}}\n", i, i + 1).as_str();
        data += format!("{{?a a :U{}}}=>{{?a a :J{}}}\n", i, i + 1).as_str();
        data += format!("{{?a a :U{}}}=>{{?a a :Q{}}}\n", i, i + 1).as_str();
    }
    let mut store = TripleStore::from(data.as_str());
    store.materialize().unwrap();
}

fn infer_hierarchy_rdf_rule(max_depth: i32) {
    let mut data = ":a a :U0\n\
                        {?a :subClassOf ?b.?b :subClassOf ?c}=>{?a :subClassOf ?c}\n"
        .to_owned();
    for i in 0..max_depth {
        data += format!(":U{} :subClassOf :U{}.\n", i, i + 1).as_str();
        data += format!(":U{} :subClassOf :J{}.\n", i, i + 1).as_str();
        data += format!(":U{} :subClassOf :Q{}.\n", i, i + 1).as_str();
    }
    let mut store = TripleStore::from(data.as_str());
    store.materialize().unwrap();
}

fn shacl_hierarchy_validate(max_depth: i32) {
    let shapes_str = format!(
        r#"
        @prefix sh: <http://www.w3.org/ns/shacl#> .
        @prefix ex: <http://example.org/> .
        ex:ParentShape a sh:NodeShape ;
            sh:targetNode ex:focus ;
            sh:class ex:U{} .
        "#,
        max_depth
    );
    let shapes = ShapesGraph::parse(&shapes_str).unwrap();
    let mut data_str = "@prefix ex: <http://example.org/> .\n\
                        @prefix rdfs: <http://www.w3.org/2000/01/rdf-schema#> .\n\
                        ex:focus a ex:U0 .\n"
        .to_owned();
    for i in 0..max_depth {
        data_str += format!("ex:U{} rdfs:subClassOf ex:U{} .\n", i, i + 1).as_str();
    }
    let triples = Parser::parse_triples(&data_str, Syntax::Turtle).unwrap();
    let mut index = TripleIndex::new();
    for t in triples {
        index.add(t);
    }
    ShaclValidator::validate(&index, &shapes);
}

// Bounded hierarchy benchmarks that run successfully and measure subclass closure behavior

fn test_hierarchy_1000(bench: &mut Bencher) {
    bench.iter(|| {
        let max_depth = 1000;
        infer_hierarchy(max_depth);
    });
}

fn test_hierarchy_100(bench: &mut Bencher) {
    bench.iter(|| {
        let max_depth = 100;
        infer_hierarchy(max_depth);
    });
}

fn test_hierarchy_10(bench: &mut Bencher) {
    bench.iter(|| {
        let max_depth = 10;
        infer_hierarchy(max_depth);
    });
}

fn test_rdf_hierarchy_50(bench: &mut Bencher) {
    bench.iter(|| {
        let max_depth = 50;
        infer_hierarchy_rdf_rule(max_depth);
    });
}

fn test_rdf_hierarchy_100(bench: &mut Bencher) {
    bench.iter(|| {
        let max_depth = 100;
        infer_hierarchy_rdf_rule(max_depth);
    });
}

fn test_rdf_hierarchy_10(bench: &mut Bencher) {
    bench.iter(|| {
        let max_depth = 10;
        infer_hierarchy_rdf_rule(max_depth);
    });
}

fn test_shacl_hierarchy_1000(bench: &mut Bencher) {
    bench.iter(|| {
        shacl_hierarchy_validate(1000);
    });
}

fn test_shacl_hierarchy_100(bench: &mut Bencher) {
    bench.iter(|| {
        shacl_hierarchy_validate(100);
    });
}

fn test_shacl_hierarchy_10(bench: &mut Bencher) {
    bench.iter(|| {
        shacl_hierarchy_validate(10);
    });
}

benchmark_group!(
    benches,
    test_hierarchy_10,
    test_rdf_hierarchy_10,
    test_shacl_hierarchy_10,
    test_hierarchy_100,
    test_rdf_hierarchy_50,
    test_rdf_hierarchy_100,
    test_shacl_hierarchy_100,
    test_hierarchy_1000,
    test_shacl_hierarchy_1000
);
benchmark_main!(benches);
