//! Supervisor Pattern Integration Tests
//!
//! Tests the complete supervisor pattern implementation with different
//! restart strategies and monitoring scenarios.

use osiris_core::{
    AndonSystem, BackoffStrategy, KaizenCycle, RestartStrategy, Restartable, SensorManager,
    SupervisionStrategy, SupervisorConfig, SupervisorTree,
};

#[tokio::test]
async fn test_supervisor_tree_creation() {
    let config = SupervisorConfig {
        strategy: SupervisionStrategy::OneForOne,
        intensity: 5,
        period: 60,
    };

    let supervisor = SupervisorTree::new(config);
    let health = supervisor.get_health().await;

    assert_eq!(health.total_children, 0);
    assert_eq!(health.dead_children, 0);
    assert!(health.is_healthy);
}

#[tokio::test]
async fn test_register_multiple_children() {
    let supervisor = SupervisorTree::default_tree();

    // Register SensorManager (Transient)
    let result = supervisor
        .register_child(
            "sensor_1".to_string(),
            RestartStrategy::Transient {
                max_retries: Some(3),
                backoff: BackoffStrategy::Fixed { delay_ms: 10 },
            },
        )
        .await;
    assert!(result.is_ok());

    // Register AndonSystem (Permanent)
    let result = supervisor
        .register_child(
            "andon_1".to_string(),
            RestartStrategy::Permanent {
                max_retries: Some(10),
                backoff: BackoffStrategy::Fixed { delay_ms: 10 },
            },
        )
        .await;
    assert!(result.is_ok());

    // Register KaizenCycle (Temporary)
    let result = supervisor
        .register_child(
            "kaizen_1".to_string(),
            RestartStrategy::Temporary {
                max_retries: 3,
                backoff: BackoffStrategy::Fixed { delay_ms: 10 },
            },
        )
        .await;
    assert!(result.is_ok());

    let health = supervisor.get_health().await;
    assert_eq!(health.total_children, 3);
    assert_eq!(health.dead_children, 0);
    assert!(health.is_healthy);
}

#[tokio::test]
async fn test_child_statistics_tracking() {
    let supervisor = SupervisorTree::default_tree();

    supervisor
        .register_child(
            "monitored_child".to_string(),
            RestartStrategy::Transient {
                max_retries: Some(5),
                backoff: BackoffStrategy::Fixed { delay_ms: 10 },
            },
        )
        .await
        .unwrap();

    let stats = supervisor.get_child_stats("monitored_child").await.unwrap();

    assert_eq!(stats.id, "monitored_child");
    assert_eq!(stats.restart_count, 0);
    assert!(stats.last_restart.is_none());
}

#[tokio::test]
async fn test_transient_strategy_configuration() {
    let strategy = RestartStrategy::Transient {
        max_retries: Some(3),
        backoff: BackoffStrategy::Exponential {
            initial_delay_ms: 100,
            multiplier: 2.0,
            max_delay_ms: 5000,
        },
    };

    match strategy {
        RestartStrategy::Transient {
            max_retries,
            backoff: _,
        } => {
            assert_eq!(max_retries, Some(3));
        }
        _ => panic!("Expected Transient strategy"),
    }
}

#[tokio::test]
async fn test_permanent_strategy_configuration() {
    let strategy = RestartStrategy::Permanent {
        max_retries: Some(10),
        backoff: BackoffStrategy::Fixed { delay_ms: 1000 },
    };

    match strategy {
        RestartStrategy::Permanent {
            max_retries,
            backoff: _,
        } => {
            assert_eq!(max_retries, Some(10));
        }
        _ => panic!("Expected Permanent strategy"),
    }
}

#[tokio::test]
async fn test_temporary_strategy_configuration() {
    let strategy = RestartStrategy::Temporary {
        max_retries: 3,
        backoff: BackoffStrategy::Fixed { delay_ms: 500 },
    };

    match strategy {
        RestartStrategy::Temporary {
            max_retries,
            backoff: _,
        } => {
            assert_eq!(max_retries, 3);
        }
        _ => panic!("Expected Temporary strategy"),
    }
}

#[tokio::test]
async fn test_sensor_manager_component() {
    let sensor = SensorManager::new("sensor_1".to_string());

    assert_eq!(sensor.id(), "sensor_1");
    assert_eq!(sensor.restart_count(), 0);
    assert!(!sensor.is_healthy().await);
}

#[tokio::test]
async fn test_andon_system_component() {
    let andon = AndonSystem::new("andon_1".to_string());

    assert_eq!(andon.id(), "andon_1");
    assert_eq!(andon.restart_count(), 0);
    assert!(!andon.is_healthy().await);
}

#[tokio::test]
async fn test_kaizen_cycle_component() {
    let kaizen = KaizenCycle::new("kaizen_1".to_string());

    assert_eq!(kaizen.id(), "kaizen_1");
    assert_eq!(kaizen.restart_count(), 0);
    assert!(!kaizen.is_healthy().await);
}

#[tokio::test]
async fn test_all_children_statistics() {
    let supervisor = SupervisorTree::default_tree();

    // Register multiple children with different strategies
    for i in 0..5 {
        supervisor
            .register_child(
                format!("child_{}", i),
                match i % 3 {
                    0 => RestartStrategy::Transient {
                        max_retries: Some(3),
                        backoff: BackoffStrategy::Fixed { delay_ms: 10 },
                    },
                    1 => RestartStrategy::Permanent {
                        max_retries: Some(5),
                        backoff: BackoffStrategy::Fixed { delay_ms: 10 },
                    },
                    _ => RestartStrategy::Temporary {
                        max_retries: 2,
                        backoff: BackoffStrategy::Fixed { delay_ms: 10 },
                    },
                },
            )
            .await
            .unwrap();
    }

    let all_stats = supervisor.get_all_children_stats().await;
    assert_eq!(all_stats.len(), 5);

    for stats in all_stats {
        assert_eq!(stats.restart_count, 0);
        assert!(stats.last_restart.is_none());
    }
}

#[tokio::test]
async fn test_supervisor_health_status() {
    let supervisor = SupervisorTree::default_tree();

    // Initially healthy
    let health = supervisor.get_health().await;
    assert!(health.is_healthy);
    assert_eq!(health.total_children, 0);
    assert_eq!(health.dead_children, 0);

    // Add some children
    for i in 0..3 {
        supervisor
            .register_child(
                format!("child_{}", i),
                RestartStrategy::Transient {
                    max_retries: Some(3),
                    backoff: BackoffStrategy::Fixed { delay_ms: 10 },
                },
            )
            .await
            .unwrap();
    }

    let health = supervisor.get_health().await;
    assert_eq!(health.total_children, 3);
    assert_eq!(health.dead_children, 0);
    assert!(health.is_healthy);
}

#[tokio::test]
async fn test_supervision_signal_creation() {
    let supervisor = SupervisorTree::default_tree();

    let signal = supervisor.create_signal("test_child", osiris_core::SignalLevel::Warning);

    assert_eq!(signal.signal_type, "supervision_event");
    assert_eq!(signal.source, Some("supervisor".to_string()));
    assert_eq!(signal.target, Some("test_child".to_string()));
    assert_eq!(signal.level, osiris_core::SignalLevel::Warning);
}

#[tokio::test]
async fn test_backoff_strategies() {
    // Test None backoff
    let none_strategy = BackoffStrategy::None;
    assert_eq!(none_strategy.delay_for_attempt(0).as_millis(), 0);
    assert_eq!(none_strategy.delay_for_attempt(5).as_millis(), 0);

    // Test Fixed backoff
    let fixed_strategy = BackoffStrategy::Fixed { delay_ms: 100 };
    assert_eq!(fixed_strategy.delay_for_attempt(0).as_millis(), 100);
    assert_eq!(fixed_strategy.delay_for_attempt(3).as_millis(), 100);

    // Test Exponential backoff
    let exp_strategy = BackoffStrategy::Exponential {
        initial_delay_ms: 100,
        multiplier: 2.0,
        max_delay_ms: 10000,
    };

    let delay_0 = exp_strategy.delay_for_attempt(0).as_millis();
    let delay_1 = exp_strategy.delay_for_attempt(1).as_millis();
    let delay_2 = exp_strategy.delay_for_attempt(2).as_millis();

    // Each should be approximately 2x the previous
    assert_eq!(delay_0, 100);
    assert_eq!(delay_1, 200);
    assert_eq!(delay_2, 400);
}

#[tokio::test]
async fn test_supervision_strategy_types() {
    let config_one_for_one = SupervisorConfig {
        strategy: SupervisionStrategy::OneForOne,
        intensity: 5,
        period: 60,
    };

    let config_one_for_all = SupervisorConfig {
        strategy: SupervisionStrategy::OneForAll,
        intensity: 5,
        period: 60,
    };

    let config_rest_for_one = SupervisorConfig {
        strategy: SupervisionStrategy::RestForOne,
        intensity: 5,
        period: 60,
    };

    let supervisor_1 = SupervisorTree::new(config_one_for_one);
    let supervisor_2 = SupervisorTree::new(config_one_for_all);
    let supervisor_3 = SupervisorTree::new(config_rest_for_one);

    // All should be healthy initially
    assert!(supervisor_1.get_health().await.is_healthy);
    assert!(supervisor_2.get_health().await.is_healthy);
    assert!(supervisor_3.get_health().await.is_healthy);
}

#[tokio::test]
async fn test_duplicate_child_registration_fails() {
    let supervisor = SupervisorTree::default_tree();

    let first_result = supervisor
        .register_child(
            "child_1".to_string(),
            RestartStrategy::Transient {
                max_retries: Some(3),
                backoff: BackoffStrategy::Fixed { delay_ms: 10 },
            },
        )
        .await;

    assert!(first_result.is_ok());

    let second_result = supervisor
        .register_child(
            "child_1".to_string(),
            RestartStrategy::Transient {
                max_retries: Some(3),
                backoff: BackoffStrategy::Fixed { delay_ms: 10 },
            },
        )
        .await;

    assert!(second_result.is_err());
}

#[tokio::test]
async fn test_get_nonexistent_child_stats() {
    let supervisor = SupervisorTree::default_tree();

    let result = supervisor.get_child_stats("nonexistent").await;

    assert!(result.is_err());
}
