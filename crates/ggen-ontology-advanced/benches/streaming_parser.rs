//! Benchmarks for zero-copy streaming parser

use criterion::{black_box, criterion_group, criterion_main, Criterion, BenchmarkId};
use ggen_ontology_advanced::parser::{StreamingParser, ParserConfig};

fn generate_ntriples(n: usize) -> String {
    let mut result = String::new();
    for i in 0..n {
        result.push_str(&format!(
            "<http://example.org/subject{}> <http://example.org/predicate> <http://example.org/object{}> .\n",
            i, i
        ));
    }
    result
}

fn bench_streaming_parser(c: &mut Criterion) {
    let mut group = c.benchmark_group("streaming_parser");

    for size in [10, 100, 1000, 10000].iter() {
        let data = generate_ntriples(*size);

        group.bench_with_input(
            BenchmarkId::new("parse_stream", size),
            &data,
            |b, data| {
                let parser = StreamingParser::new();
                b.iter(|| {
                    let iter = parser.parse_str(black_box(data)).unwrap();
                    iter.count()
                });
            },
        );

        group.bench_with_input(
            BenchmarkId::new("parse_collect", size),
            &data,
            |b, data| {
                let parser = StreamingParser::new();
                b.iter(|| {
                    let iter = parser.parse_str(black_box(data)).unwrap();
                    iter.collect::<Result<Vec<_>, _>>()
                });
            },
        );
    }

    group.finish();
}

fn bench_zero_copy_vs_owned(c: &mut Criterion) {
    let mut group = c.benchmark_group("zero_copy");
    let data = generate_ntriples(1000);

    group.bench_function("zero_copy_iteration", |b| {
        let parser = StreamingParser::new();
        b.iter(|| {
            let iter = parser.parse_str(black_box(&data)).unwrap();
            // Iterate without converting to owned
            iter.count()
        });
    });

    group.bench_function("owned_conversion", |b| {
        let parser = StreamingParser::new();
        b.iter(|| {
            let iter = parser.parse_str(black_box(&data)).unwrap();
            // Convert all to owned triples
            iter.map(|r| r.map(|t| t.into_owned()))
                .collect::<Result<Vec<_>, _>>()
        });
    });

    group.finish();
}

fn bench_parser_configs(c: &mut Criterion) {
    let mut group = c.benchmark_group("parser_config");
    let data = generate_ntriples(1000);

    group.bench_function("default_config", |b| {
        let parser = StreamingParser::new();
        b.iter(|| {
            let iter = parser.parse_str(black_box(&data)).unwrap();
            iter.count()
        });
    });

    group.bench_function("strict_mode", |b| {
        let config = ParserConfig {
            strict_mode: true,
            ..Default::default()
        };
        let parser = StreamingParser::with_config(config);
        b.iter(|| {
            let iter = parser.parse_str(black_box(&data)).unwrap();
            iter.count()
        });
    });

    group.finish();
}

criterion_group!(
    benches,
    bench_streaming_parser,
    bench_zero_copy_vs_owned,
    bench_parser_configs
);
criterion_main!(benches);
