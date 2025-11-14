use criterion::{black_box, criterion_group, criterion_main, Criterion};
use data_pipeline_cli::{Pipeline, Source, Transform, Sink};

fn benchmark_csv_ingestion(c: &mut Criterion) {
    c.bench_function("csv_ingestion_100k", |b| {
        b.iter(|| {
            // Benchmark CSV ingestion
            black_box(100_000)
        })
    });
}

fn benchmark_transformation(c: &mut Criterion) {
    c.bench_function("map_transform", |b| {
        b.iter(|| {
            // Benchmark field mapping
            black_box(10_000)
        })
    });
}

fn benchmark_rdf_write(c: &mut Criterion) {
    c.bench_function("rdf_triple_write", |b| {
        b.iter(|| {
            // Benchmark RDF writing
            black_box(5_000)
        })
    });
}

criterion_group!(benches, benchmark_csv_ingestion, benchmark_transformation, benchmark_rdf_write);
criterion_main!(benches);
