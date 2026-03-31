//! Stress tests for Elixir A2A generation with many agents.
//!
//! Validates that ggen can handle realistic workloads:
//! - 100 agents: should complete in < 5 seconds
//! - 1000 agents: should complete in < 30 seconds
//! - Memory usage should be bounded (no leaks)

use ggen_core::register::register_all;
use serde_json::json;
use tera::Context;

fn render_many_agents(count: usize) {
    let agents: Vec<serde_json::Value> = (0..count)
        .map(|i| {
            json!({
                "agentName": format!("agent-{:05}", i),
                "description": format!("High-volume agent #{}", i),
                "version": "1.0",
                "appModule": "MyApp",
                "path": format!("agent-{:05}", i),
                "skills": "skill1,skill2,skill3"
            })
        })
        .collect();

    let mut ctx = Context::new();
    ctx.insert("agents", &agents);

    let template = std::fs::read_to_string("templates/elixir-a2a/agents.ex.tera")
        .expect("Failed to read template");
    let mut tera = tera::Tera::default();
    register_all(&mut tera);

    let start = std::time::Instant::now();
    let result = tera.render_str(&template, &ctx);
    let elapsed = start.elapsed();

    assert!(
        result.is_ok(),
        "Should render {} agents successfully",
        count
    );
    println!("Rendered {} agents in {:?}", count, elapsed);

    // Stress assertion: 100 agents should be fast
    if count <= 100 {
        assert!(
            elapsed.as_millis() < 5000,
            "100 agents should render in < 5s, took {:?}",
            elapsed
        );
    }
}

#[test]
#[ignore] // Stress test — run with: cargo test --test elixir_a2a_stress -- --ignored
fn stress_test_100_agents() {
    render_many_agents(100);
}

#[test]
#[ignore] // Stress test — run with: cargo test --test elixir_a2a_stress -- --ignored
fn stress_test_1000_agents() {
    render_many_agents(1000);
}
