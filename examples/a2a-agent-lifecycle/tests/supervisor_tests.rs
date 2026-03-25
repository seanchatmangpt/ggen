//! Supervisor tree and fault tolerance tests

use a2a_agent_lifecycle::{Agent, CrashReason, Supervisor, SupervisorState};

#[tokio::test]
async fn test_supervisor_idle_state() {
    let supervisor = Supervisor::new();
    assert_eq!(supervisor.get_state().await, SupervisorState::Idle);
    assert_eq!(supervisor.agent_count(), 0);
}

#[tokio::test]
async fn test_supervisor_start_transition() {
    let supervisor = Supervisor::new();
    assert!(supervisor.start().await.is_ok());
    assert_eq!(supervisor.get_state().await, SupervisorState::Supervising);
}

#[tokio::test]
async fn test_supervisor_double_start_fails() {
    let supervisor = Supervisor::new();
    assert!(supervisor.start().await.is_ok());
    assert!(supervisor.start().await.is_err());
}

#[test]
fn test_spawn_agent_without_state_check() {
    let supervisor = Supervisor::new();
    let agent = Agent::new("FailAgent");
    // spawn_agent_sync doesn't check state, just spawns
    assert!(supervisor.spawn_agent_sync(agent).is_ok());
}

#[tokio::test]
async fn test_spawn_single_agent() {
    let supervisor = Supervisor::new();
    let _ = supervisor.start().await;

    let agent = Agent::new("Agent1");
    let result = supervisor.spawn_agent_sync(agent);

    assert!(result.is_ok());
    assert_eq!(supervisor.agent_count(), 1);
    assert!(supervisor.has_agent(&result.unwrap()));
}

#[tokio::test]
async fn test_spawn_multiple_agents() {
    let supervisor = Supervisor::new();
    let _ = supervisor.start().await;

    let mut agent_ids = Vec::new();
    for i in 0..10 {
        let agent = Agent::new(format!("Agent{}", i));
        if let Ok(id) = supervisor.spawn_agent_sync(agent) {
            agent_ids.push(id);
        }
    }

    assert_eq!(supervisor.agent_count(), 10);
    for id in agent_ids {
        assert!(supervisor.has_agent(&id));
    }
}

#[tokio::test]
async fn test_agent_crash_first_restart() {
    let supervisor = Supervisor::new();
    let _ = supervisor.start().await;

    let agent = Agent::new("CrashAgent");
    let agent_id = supervisor.spawn_agent_sync(agent).unwrap();

    let restarted = supervisor
        .handle_crash(&agent_id, CrashReason::Panic("Test crash".to_string()))
        .await
        .unwrap();

    assert!(restarted);
    assert_eq!(supervisor.get_restart_count(&agent_id), Some(1));
}

#[tokio::test]
async fn test_agent_crash_multiple_restarts() {
    let supervisor = Supervisor::new();
    let _ = supervisor.start().await;

    let agent = Agent::new("MultiCrashAgent");
    let agent_id = supervisor.spawn_agent_sync(agent).unwrap();

    // First crash -> restart
    let result1 = supervisor
        .handle_crash(&agent_id, CrashReason::Timeout)
        .await
        .unwrap();
    assert!(result1);
    assert_eq!(supervisor.get_restart_count(&agent_id), Some(1));

    // Second crash -> restart
    let result2 = supervisor
        .handle_crash(&agent_id, CrashReason::HealthCheckFailed)
        .await
        .unwrap();
    assert!(result2);
    assert_eq!(supervisor.get_restart_count(&agent_id), Some(2));
}

#[tokio::test]
async fn test_exponential_backoff_first_attempt() {
    let supervisor = Supervisor::new();
    let _ = supervisor.start().await;

    let agent = Agent::new("BackoffAgent1");
    let agent_id = supervisor.spawn_agent_sync(agent).unwrap();

    let start = std::time::Instant::now();
    let _ = supervisor
        .handle_crash(&agent_id, CrashReason::Panic("Crash".to_string()))
        .await;
    let elapsed = start.elapsed();

    // First restart has 100ms backoff minimum
    assert!(elapsed.as_millis() >= 100);
}

#[tokio::test]
async fn test_exponential_backoff_grows() {
    let supervisor = Supervisor::new();
    let _ = supervisor.start().await;

    let agent = Agent::new("BackoffAgent2");
    let agent_id = supervisor.spawn_agent_sync(agent).unwrap();

    let start1 = std::time::Instant::now();
    let _ = supervisor
        .handle_crash(&agent_id, CrashReason::Panic("Crash 1".to_string()))
        .await;
    let backoff1 = start1.elapsed();

    let start2 = std::time::Instant::now();
    let _ = supervisor
        .handle_crash(&agent_id, CrashReason::Panic("Crash 2".to_string()))
        .await;
    let backoff2 = start2.elapsed();

    // Second backoff should be >= first (200ms >= 100ms)
    assert!(backoff2.as_millis() >= backoff1.as_millis());
}

#[tokio::test]
async fn test_max_restart_attempts_enforcement() {
    let supervisor = Supervisor::new();
    let _ = supervisor.start().await;

    let agent = Agent::new("MaxRestartAgent");
    let agent_id = supervisor.spawn_agent_sync(agent).unwrap();

    // Trigger 3 crashes (MAX_RESTART_ATTEMPTS)
    for i in 0..3 {
        let result = supervisor
            .handle_crash(&agent_id, CrashReason::Panic(format!("Crash {}", i)))
            .await
            .unwrap();

        if i < 2 {
            assert!(result); // Can still restart
        } else {
            assert!(!result); // No more restart attempts
        }
    }

    // Verify agent is terminated
    assert_eq!(supervisor.get_restart_count(&agent_id), Some(3));
}

#[tokio::test]
async fn test_agent_not_found_crash_error() {
    let supervisor = Supervisor::new();
    let _ = supervisor.start().await;

    let result = supervisor
        .handle_crash("nonexistent", CrashReason::Panic("Test".to_string()))
        .await;

    assert!(result.is_err());
}

#[tokio::test]
async fn test_dead_letter_queue_add_single() {
    let supervisor = Supervisor::new();
    let msg = a2a_agent_lifecycle::Message::new(
        a2a_agent_lifecycle::MessageType::TaskRequest,
        "sender",
        None,
        serde_json::json!({"data": "test"}),
    );

    let _ = supervisor
        .queue_dead_letter("agent-1".to_string(), msg)
        .await;

    let dlq = supervisor.get_dead_letters().await.unwrap();
    assert_eq!(dlq.len(), 1);
}

#[tokio::test]
async fn test_dead_letter_queue_add_multiple() {
    let supervisor = Supervisor::new();

    for i in 0..5 {
        let msg = a2a_agent_lifecycle::Message::new(
            a2a_agent_lifecycle::MessageType::TaskResult,
            "sender",
            None,
            serde_json::json!({"sequence": i}),
        );
        let _ = supervisor
            .queue_dead_letter(format!("agent-{}", i), msg)
            .await;
    }

    let dlq = supervisor.get_dead_letters().await.unwrap();
    assert_eq!(dlq.len(), 5);
}

#[tokio::test]
async fn test_dead_letter_queue_clear() {
    let supervisor = Supervisor::new();

    for i in 0..3 {
        let msg = a2a_agent_lifecycle::Message::new(
            a2a_agent_lifecycle::MessageType::Error,
            "sender",
            None,
            serde_json::json!({}),
        );
        let _ = supervisor
            .queue_dead_letter(format!("agent-{}", i), msg)
            .await;
    }

    let _ = supervisor.clear_dead_letters().await;

    let dlq = supervisor.get_dead_letters().await.unwrap();
    assert_eq!(dlq.len(), 0);
}

#[tokio::test]
async fn test_health_check_all_healthy() {
    let supervisor = Supervisor::new();
    let _ = supervisor.start().await;

    for i in 0..3 {
        let agent = Agent::new(format!("HealthAgent{}", i));
        let _ = supervisor.spawn_agent_sync(agent);
    }

    let unhealthy = supervisor.health_check().await.unwrap();
    assert_eq!(unhealthy.len(), 0);
}

#[tokio::test]
async fn test_graceful_shutdown_single_agent() {
    let supervisor = Supervisor::new();
    let _ = supervisor.start().await;

    let agent = Agent::new("ShutdownAgent");
    let _ = supervisor.spawn_agent_sync(agent);

    assert_eq!(supervisor.agent_count(), 1);
    let _ = supervisor.shutdown().await;
    assert_eq!(supervisor.get_state().await, SupervisorState::Idle);
}

#[tokio::test]
async fn test_graceful_shutdown_multiple_agents() {
    let supervisor = Supervisor::new();
    let _ = supervisor.start().await;

    for i in 0..5 {
        let agent = Agent::new(format!("ShutdownAgent{}", i));
        let _ = supervisor.spawn_agent_sync(agent);
    }

    assert_eq!(supervisor.agent_count(), 5);
    let _ = supervisor.shutdown().await;
    assert_eq!(supervisor.get_state().await, SupervisorState::Idle);
}

#[tokio::test]
async fn test_agent_health_metrics_new_agent() {
    let supervisor = Supervisor::new();
    let _ = supervisor.start().await;

    let agent = Agent::new("HealthMetricsAgent");
    let agent_id = supervisor.spawn_agent_sync(agent).unwrap();

    let health = supervisor.get_agent_health(&agent_id).await.unwrap();
    assert_eq!(health.agent_id, agent_id);
    assert_eq!(health.restart_count, 0);
    assert!(health.healthy);
}

#[tokio::test]
async fn test_agent_health_metrics_after_crash() {
    let supervisor = Supervisor::new();
    let _ = supervisor.start().await;

    let agent = Agent::new("CrashHealthAgent");
    let agent_id = supervisor.spawn_agent_sync(agent).unwrap();

    let _ = supervisor
        .handle_crash(&agent_id, CrashReason::Timeout)
        .await;

    let health = supervisor.get_agent_health(&agent_id).await.unwrap();
    assert_eq!(health.restart_count, 1);
    assert!(health.last_crash.is_some());
}

#[tokio::test]
async fn test_agent_not_found_health_error() {
    let supervisor = Supervisor::new();
    let result = supervisor.get_agent_health("nonexistent").await;
    assert!(result.is_err());
}

#[tokio::test]
async fn test_get_agents_list() {
    let supervisor = Supervisor::new();
    let _ = supervisor.start().await;

    let mut expected_ids = Vec::new();
    for i in 0..3 {
        let agent = Agent::new(format!("ListAgent{}", i));
        if let Ok(id) = supervisor.spawn_agent_sync(agent) {
            expected_ids.push(id);
        }
    }

    let agents = supervisor.get_agents();
    assert_eq!(agents.len(), 3);

    for id in expected_ids {
        assert!(agents.contains(&id));
    }
}
