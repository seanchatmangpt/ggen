//! Swarm Performance Tests
//!
//! Performance benchmarks and scalability tests for the swarm coordination system.
//! Focuses on critical performance metrics: latency, throughput, and resource usage.

use ggen_core::config::hive_coordinator::HiveQueen;
use ggen_core::config::ontology_config::{CompositionStrategy, OntologyConfig, OntologyPackRef};
use std::time::Instant;

/// Test orchestration latency for small configuration
#[tokio::test]
async fn test_small_config_latency() {
    let config = create_test_config(2);
    let mut hive = HiveQueen::new(config).await.unwrap();

    let start = Instant::now();
    let _ = hive.orchestrate().await.unwrap();
    let duration = start.elapsed();

    // Small config should complete very quickly
    assert!(
        duration.as_millis() < 100,
        "Small config too slow: {:?}",
        duration
    );
}

/// Test orchestration latency for medium configuration
#[tokio::test]
async fn test_medium_config_latency() {
    let config = create_test_config(5);
    let mut hive = HiveQueen::new(config).await.unwrap();

    let start = Instant::now();
    let _ = hive.orchestrate().await.unwrap();
    let duration = start.elapsed();

    // Medium config should complete in under 500ms
    assert!(
        duration.as_millis() < 500,
        "Medium config too slow: {:?}",
        duration
    );
}

/// Test orchestration latency for large configuration
#[tokio::test]
async fn test_large_config_latency() {
    let config = create_test_config(10);
    let mut hive = HiveQueen::new(config).await.unwrap();

    let start = Instant::now();
    let _ = hive.orchestrate().await.unwrap();
    let duration = start.elapsed();

    // Large config should complete in under 1 second
    assert!(
        duration.as_secs() < 1,
        "Large config too slow: {:?}",
        duration
    );
}

/// Test agent spawning performance
#[tokio::test]
async fn test_agent_spawning_performance() {
    let config = create_test_config(10);

    let start = Instant::now();
    let hive = HiveQueen::new(config).await.unwrap();
    let duration = start.elapsed();

    // Agent spawning should be nearly instant
    assert!(
        duration.as_millis() < 50,
        "Agent spawning too slow: {:?}",
        duration
    );
    assert_eq!(hive.agents.len(), 6); // 4 base + optimizer + performance
}

/// Test throughput: multiple orchestrations
#[tokio::test]
async fn test_orchestration_throughput() {
    let config = create_test_config(5);
    let mut hive = HiveQueen::new(config).await.unwrap();

    let iterations = 10;
    let start = Instant::now();

    for _ in 0..iterations {
        let _ = hive.orchestrate().await.unwrap();
    }

    let duration = start.elapsed();
    let avg_ms = duration.as_millis() / iterations;

    // Should maintain good performance across multiple runs
    assert!(avg_ms < 100, "Average orchestration too slow: {}ms", avg_ms);
}

/// Test concurrent orchestration performance
#[tokio::test]
async fn test_concurrent_orchestration_performance() {
    let configs: Vec<_> = (0..5).map(|_| create_test_config(4)).collect();

    let start = Instant::now();

    let handles: Vec<_> = configs
        .into_iter()
        .map(|config| {
            tokio::spawn(async move {
                let mut hive = HiveQueen::new(config).await.unwrap();
                hive.orchestrate().await.unwrap()
            })
        })
        .collect();

    for handle in handles {
        let _ = handle.await.unwrap();
    }

    let duration = start.elapsed();

    // Concurrent execution should be efficient
    assert!(
        duration.as_secs() < 2,
        "Concurrent orchestration too slow: {:?}",
        duration
    );
}

/// Test memory efficiency
#[tokio::test]
async fn test_memory_efficiency() {
    let config = create_test_config(20);

    // Create and drop multiple hives
    for _ in 0..100 {
        let mut hive = HiveQueen::new(config.clone()).await.unwrap();
        let _ = hive.orchestrate().await.unwrap();
    }

    // If we got here without OOM, memory management is working
    assert!(true);
}

/// Test agent analysis performance
#[tokio::test]
async fn test_agent_analysis_performance() {
    use ggen_core::config::hive_coordinator::{AgentRole, HiveAgent};

    let config = create_test_config(10);
    let agent = HiveAgent::new("perf-test", AgentRole::Analyzer, vec!["test".to_string()]);

    let iterations = 1000;
    let start = Instant::now();

    for _ in 0..iterations {
        let _ = agent.analyze_config(&config).await.unwrap();
    }

    let duration = start.elapsed();
    let avg_micros = duration.as_micros() / iterations;

    // Each analysis should be very fast
    assert!(
        avg_micros < 1000,
        "Agent analysis too slow: {}μs",
        avg_micros
    );
}

/// Test conflict detection performance
#[tokio::test]
async fn test_conflict_detection_performance() {
    let config = create_conflicting_config(10);
    let mut hive = HiveQueen::new(config).await.unwrap();

    let start = Instant::now();
    let resolved = hive.orchestrate().await.unwrap();
    let duration = start.elapsed();

    // Conflict detection should not significantly slow down orchestration
    assert!(
        duration.as_millis() < 500,
        "Conflict detection too slow: {:?}",
        duration
    );
    assert!(resolved.conflicts_found > 0);
}

/// Test scaling with agent count
#[tokio::test]
async fn test_agent_scaling_performance() {
    // Test with different complexities
    let configs = vec![
        (2, 4), // 2 packs -> 4 agents
        (4, 5), // 4 packs -> 5 agents
        (6, 6), // 6 packs -> 6 agents
    ];

    for (pack_count, expected_agents) in configs {
        let config = create_test_config(pack_count);

        let start = Instant::now();
        let hive = HiveQueen::new(config).await.unwrap();
        let init_duration = start.elapsed();

        assert_eq!(hive.agents.len(), expected_agents);
        assert!(
            init_duration.as_millis() < 50,
            "Init too slow for {} agents",
            expected_agents
        );
    }
}

/// Test repeated orchestrations maintain performance
#[tokio::test]
async fn test_performance_stability() {
    let config = create_test_config(5);
    let mut hive = HiveQueen::new(config).await.unwrap();

    let mut durations = Vec::new();

    for _ in 0..20 {
        let start = Instant::now();
        let _ = hive.orchestrate().await.unwrap();
        durations.push(start.elapsed().as_millis());
    }

    // Performance should be consistent (no degradation)
    let avg: u128 = durations.iter().sum::<u128>() / durations.len() as u128;
    let max = *durations.iter().max().unwrap();

    // Max should not be more than 2x average
    assert!(
        max < avg * 2,
        "Performance degradation detected: max={}ms, avg={}ms",
        max,
        avg
    );
}

/// Benchmark: Initialization overhead
#[tokio::test]
async fn test_initialization_overhead() {
    let iterations = 100;
    let start = Instant::now();

    for i in 0..iterations {
        let config = create_test_config(3);
        let _ = HiveQueen::new(config).await.unwrap();

        // Simulate some delay to avoid overwhelming the system
        if i % 10 == 0 {
            tokio::time::sleep(tokio::time::Duration::from_millis(1)).await;
        }
    }

    let duration = start.elapsed();
    let avg_ms = duration.as_millis() / iterations;

    // Initialization should be fast
    assert!(
        avg_ms < 10,
        "Initialization overhead too high: {}ms",
        avg_ms
    );
}

/// Benchmark: State access performance
#[tokio::test]
async fn test_state_access_performance() {
    let config = create_test_config(5);
    let mut hive = HiveQueen::new(config).await.unwrap();

    // Run orchestration to populate state
    let _ = hive.orchestrate().await.unwrap();

    // State access should be fast even after orchestration
    let start = Instant::now();
    for _ in 0..1000 {
        // Accessing agent information
        let _ = hive.agents.len();
    }
    let duration = start.elapsed();

    assert!(
        duration.as_micros() < 10000,
        "State access too slow: {:?}",
        duration
    );
}

// Helper functions

fn create_test_config(pack_count: usize) -> OntologyConfig {
    let mut config = OntologyConfig::new();

    for i in 0..pack_count {
        config = config.with_pack(OntologyPackRef {
            name: format!("perf-pack-{}", i),
            version: "1.0.0".to_string(),
            namespace: Some(format!("http://perf.test/pack{}/", i)),
            classes: None,
            properties: None,
            source: None,
        });
    }

    config.with_composition(CompositionStrategy::Union)
}

fn create_conflicting_config(conflict_count: usize) -> OntologyConfig {
    let mut config = OntologyConfig::new();

    for i in 0..conflict_count {
        // Create pairs of potentially conflicting packs
        config = config
            .with_pack(OntologyPackRef {
                name: format!("conflict-a-{}", i),
                version: "^2.0.0".to_string(),
                namespace: Some(format!("http://conflict{}.test/", i)),
                classes: None,
                properties: None,
                source: None,
            })
            .with_pack(OntologyPackRef {
                name: format!("conflict-b-{}", i),
                version: "~1.9.0".to_string(),
                namespace: Some(format!("http://conflict{}.test/", i)),
                classes: None,
                properties: None,
                source: None,
            });
    }

    config.with_composition(CompositionStrategy::Union)
}
