//! Criterion benchmark for ABB/SBB admission + manufacture + replay.
//! `cargo bench --manifest-path crates/ggen-abb-sbb/Cargo.toml`
//! Recorded numbers and the committed regression bound: `bench/receipt.json`,
//! enforced by `tests/bench_bound.rs`.
use criterion::{black_box, criterion_group, criterion_main, BenchmarkId, Criterion};
use ggen_abb_sbb::*;

fn bench(c: &mut Criterion) {
    let gen = Generator {
        id: "ggen-abb-sbb".into(),
        version: "26.9.26".into(),
    };
    let mut group = c.benchmark_group("abb_sbb");
    for &(sbbs, arts) in &[(2usize, 3usize), (16, 16), (64, 64)] {
        let g = synthetic_graph(sbbs, arts);
        let last = format!("sbb:ingest-{:04}", sbbs - 1);
        let req = Request {
            abb: "abb:event-ingest".into(),
            sbb: last.clone(),
            requested_authority: Authority::Construct,
            expected_graph_digest: Some(g.digest()),
        };
        let label = format!("{sbbs}x{arts}");
        group.bench_with_input(BenchmarkId::new("graph_digest", &label), &g, |b, g| {
            b.iter(|| black_box(g.digest()))
        });
        group.bench_with_input(BenchmarkId::new("admit", &label), &g, |b, g| {
            b.iter(|| black_box(admit(g, &req).unwrap()))
        });
        group.bench_with_input(BenchmarkId::new("plan", &label), &g, |b, g| {
            b.iter(|| black_box(plan(g, "abb:event-ingest", Authority::Construct).unwrap()))
        });
        let ad = admit(&g, &req).unwrap();
        group.bench_with_input(BenchmarkId::new("manufacture", &label), &ad, |b, ad| {
            b.iter(|| black_box(manufacture(ad, &gen).unwrap()))
        });
        let m = manufacture(&ad, &gen).unwrap();
        group.bench_with_input(BenchmarkId::new("replay", &label), &g, |b, g| {
            b.iter(|| black_box(replay(&m.receipt, g, &gen).unwrap()))
        });
    }
    group.finish();
}

criterion_group!(benches, bench);
criterion_main!(benches);
