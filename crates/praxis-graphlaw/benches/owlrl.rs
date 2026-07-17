use bencher::{benchmark_group, benchmark_main, Bencher};
use praxis_graphlaw::TripleStore;

fn owlrl_subclass_hierarchy(max_depth: usize) -> String {
    let mut ttl = String::from(
        r#"
@prefix ex: <http://example.org/> .
@prefix rdfs: <http://www.w3.org/2000/01/rdf-schema#> .
"#,
    );

    // Build a chain: U0 subClassOf U1 subClassOf ... U{max_depth}
    for i in 0..max_depth {
        ttl.push_str(&format!("ex:U{} rdfs:subClassOf ex:U{} .\n", i, i + 1));
    }

    // Add one individual of the top class
    ttl.push_str(&format!("ex:individual rdf:type ex:U0 .\n"));

    ttl
}

fn owlrl_subproperty_hierarchy(max_depth: usize) -> String {
    let mut ttl = String::from(
        r#"
@prefix ex: <http://example.org/> .
@prefix rdfs: <http://www.w3.org/2000/01/rdf-schema#> .
"#,
    );

    // Build a chain: P0 subPropertyOf P1 subPropertyOf ... P{max_depth}
    for i in 0..max_depth {
        ttl.push_str(&format!("ex:P{} rdfs:subPropertyOf ex:P{} .\n", i, i + 1));
    }

    // Add one fact using the top property
    ttl.push_str("ex:subject ex:P0 ex:object .\n");

    ttl
}

fn owlrl_transitive_property_chain(max_depth: usize) -> String {
    let mut ttl = String::from(
        r#"
@prefix ex: <http://example.org/> .
@prefix rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#> .
@prefix owl: <http://www.w3.org/2002/07/owl#> .
"#,
    );

    // Declare property as transitive
    ttl.push_str("ex:related rdf:type owl:TransitiveProperty .\n");

    // Build a chain: X0 related X1 related X2 ... related X{max_depth}
    for i in 0..max_depth {
        ttl.push_str(&format!("ex:X{} ex:related ex:X{} .\n", i, i + 1));
    }

    ttl
}

fn bench_owlrl_subclass_10(b: &mut Bencher) {
    let ttl = owlrl_subclass_hierarchy(10);
    b.iter(|| {
        let mut store = TripleStore::from(&ttl).expect("failed to load");
        store.materialize_owlrl().expect("materialize failed")
    });
}

fn bench_owlrl_subclass_100(b: &mut Bencher) {
    let ttl = owlrl_subclass_hierarchy(100);
    b.iter(|| {
        let mut store = TripleStore::from(&ttl).expect("failed to load");
        store.materialize_owlrl().expect("materialize failed")
    });
}

fn bench_owlrl_subclass_1000(b: &mut Bencher) {
    let ttl = owlrl_subclass_hierarchy(1000);
    b.iter(|| {
        let mut store = TripleStore::from(&ttl).expect("failed to load");
        store.materialize_owlrl().expect("materialize failed")
    });
}

fn bench_owlrl_subproperty_10(b: &mut Bencher) {
    let ttl = owlrl_subproperty_hierarchy(10);
    b.iter(|| {
        let mut store = TripleStore::from(&ttl).expect("failed to load");
        store.materialize_owlrl().expect("materialize failed")
    });
}

fn bench_owlrl_subproperty_100(b: &mut Bencher) {
    let ttl = owlrl_subproperty_hierarchy(100);
    b.iter(|| {
        let mut store = TripleStore::from(&ttl).expect("failed to load");
        store.materialize_owlrl().expect("materialize failed")
    });
}

fn bench_owlrl_subproperty_1000(b: &mut Bencher) {
    let ttl = owlrl_subproperty_hierarchy(1000);
    b.iter(|| {
        let mut store = TripleStore::from(&ttl).expect("failed to load");
        store.materialize_owlrl().expect("materialize failed")
    });
}

fn bench_owlrl_transitive_property_10(b: &mut Bencher) {
    let ttl = owlrl_transitive_property_chain(10);
    b.iter(|| {
        let mut store = TripleStore::from(&ttl).expect("failed to load");
        store.materialize_owlrl().expect("materialize failed")
    });
}

fn bench_owlrl_transitive_property_100(b: &mut Bencher) {
    let ttl = owlrl_transitive_property_chain(100);
    b.iter(|| {
        let mut store = TripleStore::from(&ttl).expect("failed to load");
        store.materialize_owlrl().expect("materialize failed")
    });
}

fn bench_owlrl_transitive_property_1000(b: &mut Bencher) {
    let ttl = owlrl_transitive_property_chain(1000);
    b.iter(|| {
        let mut store = TripleStore::from(&ttl).expect("failed to load");
        store.materialize_owlrl().expect("materialize failed")
    });
}

benchmark_group!(
    owlrl_benches,
    bench_owlrl_subclass_10,
    bench_owlrl_subclass_100,
    bench_owlrl_subclass_1000,
    bench_owlrl_subproperty_10,
    bench_owlrl_subproperty_100,
    bench_owlrl_subproperty_1000,
    bench_owlrl_transitive_property_10,
    bench_owlrl_transitive_property_100,
    bench_owlrl_transitive_property_1000,
);

benchmark_main!(owlrl_benches);
