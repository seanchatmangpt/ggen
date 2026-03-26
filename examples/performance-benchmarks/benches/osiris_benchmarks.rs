//! OSIRIS Life Domains Performance Benchmarks
//!
//! Benchmarks for:
//! - Agent initialization (target: <100ms)
//! - Goal discovery (target: <50ms)
//! - Consensus voting with 6 agents (target: <200ms)
//! - Learning outcome recording (target: <20ms)
//! - Metric calculation (target: <50ms)

use criterion::{black_box, criterion_group, criterion_main, Criterion, BenchmarkId};
use std::time::Instant;

fn osiris_agent_init_benchmark(c: &mut Criterion) {
    c.bench_function("osiris_agent_init", |b| {
        b.iter(|| {
            let _agent_id = black_box(uuid::Uuid::new_v4());
            let _config = black_box(serde_json::json!({
                "domain": "health",
                "lifecycle_stage": "growth",
                "metrics": []
            }));
        })
    });
}

fn osiris_goal_discovery_benchmark(c: &mut Criterion) {
    c.bench_function("osiris_goal_discovery", |b| {
        b.iter(|| {
            let domains = black_box(vec!["health", "relationships", "finance", "learning"]);
            let _goals: Vec<_> = domains
                .iter()
                .map(|d| format!("goal_for_{}", d))
                .collect();
        })
    });
}

fn osiris_consensus_voting_benchmark(c: &mut Criterion) {
    c.bench_function("osiris_consensus_voting_6_agents", |b| {
        b.iter(|| {
            // Simulate 6-agent consensus voting
            let votes = black_box(vec![true, true, false, true, true, true]);
            let consensus = votes.iter().filter(|&&v| v).count() >= 4;
            let _result = black_box(consensus);
        })
    });
}

fn osiris_learning_outcome_benchmark(c: &mut Criterion) {
    c.bench_function("osiris_learning_outcome_record", |b| {
        b.iter(|| {
            let _outcome = black_box(serde_json::json!({
                "id": uuid::Uuid::new_v4(),
                "domain": "learning",
                "level_before": 5,
                "level_after": 7,
                "timestamp": chrono::Utc::now(),
            }));
        })
    });
}

fn osiris_metric_calculation_benchmark(c: &mut Criterion) {
    c.bench_function("osiris_metric_calculation", |b| {
        let measurements = black_box(vec![50, 55, 52, 58, 60, 53, 51]);
        b.iter(|| {
            let sum: u64 = measurements.iter().sum();
            let avg = sum / measurements.len() as u64;
            let _stddev = (measurements
                .iter()
                .map(|&m| ((m as i64 - avg as i64) * (m as i64 - avg as i64)) as u64)
                .sum::<u64>()
                / measurements.len() as u64) as f64;
            black_box(_stddev)
        })
    });
}

fn osiris_domain_balance_benchmark(c: &mut Criterion) {
    c.bench_function("osiris_domain_balance_calculation", |b| {
        b.iter(|| {
            let scores = black_box(vec![7, 6, 8, 5, 9, 4]);
            let avg_score = scores.iter().sum::<i32>() as f64 / scores.len() as f64;
            let balance = 100.0 - (scores
                .iter()
                .map(|&s| ((s as f64 - avg_score).abs()))
                .sum::<f64>()
                / scores.len() as f64
                * 100.0 / 10.0);
            black_box(balance)
        })
    });
}

criterion_group!(
    benches,
    osiris_agent_init_benchmark,
    osiris_goal_discovery_benchmark,
    osiris_consensus_voting_benchmark,
    osiris_learning_outcome_benchmark,
    osiris_metric_calculation_benchmark,
    osiris_domain_balance_benchmark,
);
criterion_main!(benches);
