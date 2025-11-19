//! Benchmarks for compile-time query validation

use criterion::{black_box, criterion_group, criterion_main, Criterion};
use ggen_ontology_advanced::query::{CompiledQuery, SelectQuery, QueryEngine};

fn bench_compiled_queries(c: &mut Criterion) {
    let mut group = c.benchmark_group("compiled_queries");

    group.bench_function("query_creation", |b| {
        b.iter(|| {
            CompiledQuery::<SelectQuery>::new(
                black_box("SELECT ?s ?p ?o WHERE { ?s ?p ?o }"),
                black_box(12345),
            )
        });
    });

    group.bench_function("query_hash_lookup", |b| {
        let query = CompiledQuery::<SelectQuery>::new(
            "SELECT ?s ?p ?o WHERE { ?s ?p ?o }",
            12345,
        );
        b.iter(|| black_box(query.query_hash()));
    });

    group.bench_function("query_cache_check", |b| {
        let engine = QueryEngine::new();
        let query = CompiledQuery::<SelectQuery>::new(
            "SELECT ?s ?p ?o WHERE { ?s ?p ?o }",
            12345,
        );
        engine.cache_query(query.query_hash(), query.query_str().to_string());

        b.iter(|| {
            black_box(engine.is_cached(query.query_hash()))
        });
    });

    group.finish();
}

fn bench_query_engine(c: &mut Criterion) {
    let mut group = c.benchmark_group("query_engine");
    let engine = QueryEngine::new();

    group.bench_function("cache_insert", |b| {
        let mut counter = 0u64;
        b.iter(|| {
            counter += 1;
            engine.cache_query(
                black_box(counter),
                black_box("SELECT * WHERE { ?s ?p ?o }".to_string()),
            );
        });
    });

    group.finish();
}

criterion_group!(benches, bench_compiled_queries, bench_query_engine);
criterion_main!(benches);
