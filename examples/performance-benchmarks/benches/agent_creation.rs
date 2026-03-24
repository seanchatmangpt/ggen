//! Benchmark agent creation performance

use criterion::{black_box, criterion_group, criterion_main, Criterion};
use std::time::Instant;

fn agent_creation_benchmark(c: &mut Criterion) {
    c.bench_function("create_single_agent", |b| {
        b.iter(|| {
            let _start = Instant::now();
            // Simulate agent creation
            let _agent_id = uuid::Uuid::new_v4();
            let _name = String::from("TestAgent");
        })
    });

    c.bench_function("create_agent_pool_6", |b| {
        b.iter(|| {
            let _agents: Vec<_> = (0..6)
                .map(|i| {
                    (
                        uuid::Uuid::new_v4(),
                        format!("Agent-{}", i),
                    )
                })
                .collect();
        })
    });
}

criterion_group!(benches, agent_creation_benchmark);
criterion_main!(benches);
