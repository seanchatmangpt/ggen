//! Benchmarks for Elixir A2A template rendering.
//!
//! Validates that template rendering performance is acceptable for real-world use:
//! - Small projects (1-5 agents): < 10ms per template
//! - Medium projects (5-20 agents): < 50ms per template
//! - Large projects (20-100 agents): < 200ms per template

use criterion::{black_box, criterion_group, criterion_main, BenchmarkId, Criterion};
use ggen_core::register::register_all;
use serde_json::json;
use tera::Context;

fn render_elixir_agents(agent_count: usize) -> String {
    let agents: Vec<serde_json::Value> = (0..agent_count)
        .map(|i| {
            json!({
                "agentName": format!("agent-{}", i),
                "description": format!("Agent {}", i),
                "version": "1.0",
                "appModule": "MyApp",
                "path": format!("agent-{}", i),
                "skills": "test-skill"
            })
        })
        .collect();

    let mut ctx = Context::new();
    ctx.insert("agents", &agents);

    let template = std::fs::read_to_string("templates/elixir-a2a/agents.ex.tera")
        .expect("Failed to read template");
    let mut tera = tera::Tera::default();
    register_all(&mut tera);
    tera.render_str(&template, &ctx).expect("Failed to render")
}

fn bench_elixir_agents(c: &mut Criterion) {
    let mut group = c.benchmark_group("elixir_a2a_agents");

    for agent_count in [1, 5, 10, 20, 50, 100].iter() {
        group.bench_with_input(
            BenchmarkId::new("agents", agent_count),
            agent_count,
            |b, &count| {
                b.iter(|| render_elixir_agents(black_box(count)));
            },
        );
    }

    group.finish();
}

criterion_group!(elixir_a2a_benches, bench_elixir_agents);
criterion_main!(elixir_a2a_benches);
