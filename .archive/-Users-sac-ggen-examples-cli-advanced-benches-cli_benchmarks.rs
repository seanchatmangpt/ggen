use criterion::{black_box, criterion_group, criterion_main, Criterion};

fn benchmark_analyze(c: &mut Criterion) {
    c.bench_function("analyze short string", |b| {
        b.iter(|| {
            let input = black_box("hello world");
            advanced_cli::processor::analyze(input)
        })
    });

    c.bench_function("analyze long string", |b| {
        let long_str = "hello ".repeat(1000);
        b.iter(|| {
            let input = black_box(&long_str);
            advanced_cli::processor::analyze(input)
        })
    });
}

criterion_group!(benches, benchmark_analyze);
criterion_main!(benches);
