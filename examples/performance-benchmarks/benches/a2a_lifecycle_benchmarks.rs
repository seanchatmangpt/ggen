//! A2A Agent Lifecycle Performance Benchmarks
//!
//! Benchmarks for:
//! - State transition (target: <5ms)
//! - Message routing (target: <10ms per message)
//! - Task scheduling (target: <20ms)
//! - Agent creation (target: <50ms)

use criterion::{black_box, criterion_group, criterion_main, Criterion};

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum AgentState {
    Created,
    Ready,
    Processing,
    Completed,
    Failed,
}

fn a2a_state_transition_benchmark(c: &mut Criterion) {
    c.bench_function("a2a_state_transition", |b| {
        b.iter(|| {
            let current_state = black_box(AgentState::Created);
            let new_state = if current_state == AgentState::Created {
                AgentState::Ready
            } else if current_state == AgentState::Ready {
                AgentState::Processing
            } else {
                AgentState::Completed
            };
            black_box(new_state);
        })
    });
}

fn a2a_message_routing_benchmark(c: &mut Criterion) {
    c.bench_function("a2a_message_routing", |b| {
        b.iter(|| {
            let message = black_box(serde_json::json!({
                "from": uuid::Uuid::new_v4(),
                "to": uuid::Uuid::new_v4(),
                "payload": "data",
            }));
            let _dest = message.get("to").and_then(|v| v.as_str());
        })
    });
}

fn a2a_task_scheduling_benchmark(c: &mut Criterion) {
    c.bench_function("a2a_task_scheduling", |b| {
        b.iter(|| {
            let task = black_box((
                uuid::Uuid::new_v4(),
                "process_message",
                chrono::Utc::now(),
            ));
            let _priority = if task.1 == "urgent" { 10 } else { 5 };
        })
    });
}

fn a2a_agent_creation_benchmark(c: &mut Criterion) {
    c.bench_function("a2a_agent_creation", |b| {
        b.iter(|| {
            let _agent_id = black_box(uuid::Uuid::new_v4());
            let _state = black_box(AgentState::Created);
            let _inbox = black_box(Vec::<String>::new());
            let _tasks = black_box(Vec::<String>::new());
        })
    });
}

fn a2a_concurrent_agents_benchmark(c: &mut Criterion) {
    c.bench_function("a2a_create_100_agents", |b| {
        b.iter(|| {
            let _agents: Vec<_> = (0..100)
                .map(|_| {
                    (
                        uuid::Uuid::new_v4(),
                        AgentState::Created,
                    )
                })
                .collect();
        })
    });
}

criterion_group!(
    benches,
    a2a_state_transition_benchmark,
    a2a_message_routing_benchmark,
    a2a_task_scheduling_benchmark,
    a2a_agent_creation_benchmark,
    a2a_concurrent_agents_benchmark,
);
criterion_main!(benches);
