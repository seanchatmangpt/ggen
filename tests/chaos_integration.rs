/// Chaos testing integration test
///
/// Demonstrates:
/// - Full chaos injection workflow
/// - Event sourcing for replay
/// - Correlation ID tracing
/// - Multiple failure scenarios

mod chaos {
    pub mod correlation;
    pub mod event_store;
    pub mod injection;
    pub mod scenarios;
}

#[cfg(test)]
mod chaos_integration_tests {
    use chaos::scenarios::{
        CascadingFailureScenario, ChaosScenario, ClockSkewScenario, NetworkChaosScenario,
        PanicInjectionScenario, RecoveryScenario,
    };

    /// Test all chaos scenarios and collect results
    #[tokio::test]
    async fn test_all_chaos_scenarios() {
        let scenarios: Vec<Box<dyn ChaosScenario>> = vec![
            Box::new(PanicInjectionScenario::new()),
            Box::new(NetworkChaosScenario::new()),
            Box::new(ClockSkewScenario::new()),
            Box::new(CascadingFailureScenario::new()),
            Box::new(RecoveryScenario::new()),
        ];

        for scenario in scenarios {
            println!("\n=== {} ===", scenario.name());
            println!("{}", scenario.description());

            let result = scenario.run().await;
            println!(
                "Result: {} ({}ms, {} events)",
                if result.success {
                    "✓ PASS"
                } else {
                    "✗ FAIL"
                },
                result.duration_ms,
                result.events_recorded
            );

            let verification = scenario.verify().await;
            for check in &verification.checks {
                println!("  {}", check);
            }
            for failed in &verification.failed_checks {
                println!("  {}", failed);
            }

            assert!(
                verification.passed,
                "Scenario {} verification failed",
                scenario.name()
            );
        }
    }

    /// Test event sourcing and replay capabilities
    #[tokio::test]
    async fn test_event_sourcing_and_replay() {
        use chaos::injection::ChaosEngine;

        let engine = ChaosEngine::default_engine();

        // Record a series of events
        let _: Result<(), _> = engine.inject_panic("component-a").await;
        let _: Result<(), _> = engine.kill_task("task-1", "executor").await;
        let _: Result<(), _> = engine
            .inject_lock_timeout("lock_manager", std::time::Duration::from_secs(5))
            .await;
        let _: Result<(), _> = engine.record_recovery("restart_all", "system", true).await;

        // Export and verify
        let events = engine.get_events().await;
        assert_eq!(events.len(), 4);

        // Verify replay-ability (JSON export)
        let json = engine.export_events_json().await;
        assert!(json.is_ok());
        let json_str = json.unwrap();
        assert!(json_str.contains("component-a"));
        assert!(json_str.contains("task-1"));
    }

    /// Test correlation ID tracing through complex operations
    #[tokio::test]
    async fn test_correlation_id_tracing() {
        use chaos::correlation::CorrelationContext;

        let ctx = CorrelationContext::new();
        let correlation_id = ctx.correlation_id().clone();

        // Simulate multi-step operation
        {
            let _span1 = ctx.start_span("api_gateway".to_string(), "receive_request".to_string());
        }

        {
            let _span2 = ctx.start_span("auth_service".to_string(), "verify_token".to_string());
        }

        {
            let _span3 = ctx.start_span("database".to_string(), "query_user".to_string());
        }

        {
            let _span4 = ctx.start_span("api_gateway".to_string(), "send_response".to_string());
        }

        // Verify trace chain
        let traces = ctx.get_traces();
        assert_eq!(traces.len(), 4);

        // All traces should have same correlation ID
        for trace in &traces {
            assert_eq!(trace.correlation_id, correlation_id);
        }

        // Verify latency calculation
        let total_latency = ctx.total_latency_ms();
        assert!(total_latency.is_some());
        println!("Total end-to-end latency: {} ms", total_latency.unwrap());
    }

    /// Test failure replay scenario
    #[tokio::test]
    async fn test_failure_replay() {
        use chaos::event_store::{EventStore, FailureEvent, InMemoryEventStore};
        use chaos::injection::ChaosEngine;

        let engine = ChaosEngine::default_engine();

        // Scenario 1: Record initial failures
        let _: Result<(), _> = engine.inject_panic("service-1").await;
        let _: Result<(), _> = engine.kill_task("task-x", "executor").await;

        // Get events for replay
        let events_for_replay = engine.get_events().await;
        assert_eq!(events_for_replay.len(), 2);

        // Scenario 2: Simulate replay in new engine
        let mut replay_store = InMemoryEventStore::new();
        for event in events_for_replay {
            let _: Result<(), _> = replay_store.record(event);
        }

        // Verify replayed events
        let replayed = replay_store.get_all();
        assert_eq!(replayed.len(), 2);

        // Verify event details are preserved
        let panic_event = replayed
            .iter()
            .find(|e| matches!(e, FailureEvent::PanicOccurred { .. }));
        assert!(panic_event.is_some());
    }

    /// Test component-level failure tracking
    #[tokio::test]
    async fn test_component_failure_tracking() {
        use chaos::injection::ChaosEngine;

        let engine = ChaosEngine::default_engine();

        // Multiple failures across components
        let components = vec!["cache", "queue", "database", "api"];
        for component in &components {
            let _: Result<(), _> = engine.inject_panic(component).await;
        }

        // Query failures per component
        for component in &components {
            let component_events = engine.get_component_events(component).await;
            assert_eq!(component_events.len(), 1);
            assert_eq!(component_events[0].component().unwrap(), *component);
        }

        // Total should be 4
        let all_events = engine.get_events().await;
        assert_eq!(all_events.len(), 4);
    }

    /// Test network chaos under load
    #[tokio::test]
    async fn test_network_chaos_under_load() {
        use chaos::injection::{ChaosConfig, ChaosEngine};
        use std::time::Duration;

        let config = ChaosConfig {
            enable_network: true,
            base_latency_ms: 100,
            max_jitter_ms: 50,
            failure_probability: 0.5,
            ..Default::default()
        };

        let engine = ChaosEngine::new(config);

        // Simulate multiple concurrent requests with latency
        for i in 0..10 {
            let _: Result<(), _> = engine.inject_network_delay().await;
            let _: Result<(), _> = engine
                .inject_network_partition(&format!("node-{}", i), Duration::from_millis(50))
                .await;
        }

        let events = engine.get_events().await;
        assert!(events.len() > 0);
        println!("Recorded {} network events under load", events.len());
    }

    /// Test cascading failure detection
    #[tokio::test]
    async fn test_cascading_failure_detection() {
        use chaos::event_store::FailureEvent;
        use chaos::injection::ChaosEngine;

        let engine = ChaosEngine::default_engine();

        // Kill primary component
        let _: Result<(), _> = engine.kill_task("primary-db", "database").await;

        // Start cascading failure
        let affected = vec![
            "api-1".to_string(),
            "api-2".to_string(),
            "worker-1".to_string(),
        ];
        let _: Result<(), _> = engine
            .start_cascading_failure("database_unavailable", affected.clone())
            .await;

        let events = engine.get_events().await;

        // Verify cascade event
        let cascade_event = events
            .iter()
            .find(|e| matches!(e, FailureEvent::CascadingFailureStart { .. }));

        assert!(cascade_event.is_some());

        if let Some(FailureEvent::CascadingFailureStart {
            affected_components,
            ..
        }) = cascade_event
        {
            assert_eq!(affected_components.len(), 3);
        }
    }

    /// Performance test: Event recording throughput
    #[tokio::test]
    async fn test_event_recording_throughput() {
        use chaos::injection::ChaosEngine;
        use std::time::Instant;

        let engine = ChaosEngine::default_engine();
        let start = Instant::now();

        // Record 1000 events
        for i in 0..1000 {
            let component = format!("component-{}", i % 10);
            let _: Result<(), _> = engine.inject_panic(&component).await;
        }

        let duration = start.elapsed();
        let events_per_sec = 1000.0 / duration.as_secs_f64();

        println!(
            "Event recording throughput: {:.0} events/sec",
            events_per_sec
        );

        let events = engine.get_events().await;
        assert_eq!(events.len(), 1000);
    }
}
