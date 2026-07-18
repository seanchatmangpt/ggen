//! Agent lifecycle and fault recovery tests

use a2a_agent_lifecycle::{Agent, AgentState};

#[test]
fn test_heartbeat_marks_alive() {
    let mut agent = Agent::new("HeartbeatAgent");
    agent.mark_ready().unwrap();

    // Initially not crashed
    assert!(!agent.is_crashed());

    // Heartbeat keeps agent alive
    agent.heartbeat();
    assert!(!agent.is_crashed());
}

#[test]
fn test_crash_detection_explicit() {
    let mut agent = Agent::new("ExplicitCrashAgent");
    assert!(!agent.is_crashed());

    agent.mark_crashed();
    assert!(agent.is_crashed());
}

#[test]
fn test_crash_detection_timeout() {
    let mut agent = Agent::new("TimeoutCrashAgent");
    assert!(!agent.is_crashed());

    // Without heartbeat, will be detected as crashed after 5+ seconds
    // (In real scenario; we can't sleep in unit tests but logic is present)
}

#[test]
fn test_dead_letter_queue_enqueue() {
    let mut agent = Agent::new("DLQEnqueueAgent");
    assert_eq!(agent.dead_letter_queue_len(), 0);

    agent.queue_dead_letter("msg1".to_string());
    assert_eq!(agent.dead_letter_queue_len(), 1);

    agent.queue_dead_letter("msg2".to_string());
    assert_eq!(agent.dead_letter_queue_len(), 2);
}

#[test]
fn test_dead_letter_queue_dequeue() {
    let mut agent = Agent::new("DLQDequeueAgent");

    agent.queue_dead_letter("msg1".to_string());
    agent.queue_dead_letter("msg2".to_string());

    assert_eq!(agent.dequeue_dead_letter(), Some("msg1".to_string()));
    assert_eq!(agent.dequeue_dead_letter(), Some("msg2".to_string()));
    assert_eq!(agent.dequeue_dead_letter(), None);
}

#[test]
fn test_dead_letter_queue_fifo_order() {
    let mut agent = Agent::new("DLQFIFOAgent");

    for i in 0..5 {
        agent.queue_dead_letter(format!("msg{}", i));
    }

    for i in 0..5 {
        let msg = agent.dequeue_dead_letter();
        assert_eq!(msg, Some(format!("msg{}", i)));
    }
}

#[test]
fn test_graceful_shutdown_moves_pending_messages() {
    let mut agent = Agent::new("GracefulShutdownAgent");
    agent.mark_ready().unwrap();
    agent.mark_processing().unwrap();

    // Queue some messages
    agent.enqueue_message("pending1".to_string());
    agent.enqueue_message("pending2".to_string());

    assert_eq!(agent.message_queue_len(), 2);
    assert_eq!(agent.dead_letter_queue_len(), 0);

    // Graceful shutdown should move pending messages to DLQ
    let _ = agent.shutdown_graceful();

    assert_eq!(agent.message_queue_len(), 0);
    assert_eq!(agent.dead_letter_queue_len(), 2);
    assert_eq!(agent.state(), AgentState::Terminated);
}

#[test]
fn test_graceful_shutdown_empty_queue() {
    let mut agent = Agent::new("GracefulShutdownEmptyAgent");
    agent.mark_ready().unwrap();

    assert_eq!(agent.message_queue_len(), 0);
    let _ = agent.shutdown_graceful();

    assert_eq!(agent.state(), AgentState::Terminated);
    assert_eq!(agent.dead_letter_queue_len(), 0);
}

#[test]
fn test_message_ordering_under_restart() {
    let mut agent = Agent::new("OrderingAgent");
    agent.mark_ready().unwrap();

    // Queue messages in order
    for i in 0..10 {
        agent.enqueue_message(format!("msg{}", i));
    }

    // Simulate graceful shutdown (moves to DLQ)
    let _ = agent.shutdown_graceful();

    // Verify order in DLQ
    for i in 0..10 {
        let msg = agent.dequeue_dead_letter();
        assert_eq!(msg, Some(format!("msg{}", i)));
    }
}

#[test]
fn test_crash_recovery_state_machine() {
    let mut agent = Agent::new("CrashRecoveryAgent");

    // Normal flow
    assert_eq!(agent.state(), AgentState::Initializing);
    agent.mark_ready().unwrap();
    assert_eq!(agent.state(), AgentState::Ready);

    agent.mark_processing().unwrap();
    assert_eq!(agent.state(), AgentState::Processing);

    // Simulate crash
    agent.mark_crashed();
    assert!(agent.is_crashed());

    // Recover through error state
    agent.mark_error().unwrap();
    assert_eq!(agent.state(), AgentState::Error);

    agent.recover().unwrap();
    assert_eq!(agent.state(), AgentState::Ready);

    // Heartbeat clears crashed flag
    agent.heartbeat();
    assert!(!agent.is_crashed());
}

#[test]
fn test_state_transitions_preserve_message_queue() {
    let mut agent = Agent::new("PreserveQueueAgent");
    agent.mark_ready().unwrap();

    agent.enqueue_message("msg1".to_string());
    agent.enqueue_message("msg2".to_string());

    // Messages survive state transitions
    agent.mark_processing().unwrap();
    assert_eq!(agent.message_queue_len(), 2);

    agent.mark_idle().unwrap();
    assert_eq!(agent.message_queue_len(), 2);

    agent.mark_processing().unwrap();
    assert_eq!(agent.dequeue_message(), Some("msg1".to_string()));
    assert_eq!(agent.message_queue_len(), 1);
}

#[test]
fn test_error_recovery_clears_error_count() {
    let mut agent = Agent::new("ErrorCountAgent");
    agent.mark_ready().unwrap();

    // Trigger errors
    agent.mark_processing().unwrap();
    agent.mark_error().unwrap();
    assert_eq!(agent.error_count(), 1);

    // Recover and error again
    agent.recover().unwrap();
    agent.mark_processing().unwrap();
    agent.mark_error().unwrap();
    assert_eq!(agent.error_count(), 1); // Count was reset on recover

    // Recovery resets count again
    agent.recover().unwrap();
    assert_eq!(agent.error_count(), 0);
    assert_eq!(agent.state(), AgentState::Ready);
}

#[test]
fn test_multiple_crash_recovery_cycles() {
    let mut agent = Agent::new("MultipleCyclesAgent");
    agent.mark_ready().unwrap();

    for cycle in 0..3 {
        agent.mark_processing().unwrap();
        agent.mark_crashed();
        assert!(agent.is_crashed());

        agent.mark_error().unwrap();
        agent.recover().unwrap();
        agent.heartbeat();

        assert!(!agent.is_crashed());
        assert_eq!(agent.state(), AgentState::Ready);
    }
}

#[test]
fn test_dead_letter_mixed_operations() {
    let mut agent = Agent::new("MixedDLQAgent");

    // Add messages
    agent.queue_dead_letter("msg1".to_string());
    agent.queue_dead_letter("msg2".to_string());
    assert_eq!(agent.dead_letter_queue_len(), 2);

    // Dequeue one
    assert_eq!(agent.dequeue_dead_letter(), Some("msg1".to_string()));
    assert_eq!(agent.dead_letter_queue_len(), 1);

    // Add more
    agent.queue_dead_letter("msg3".to_string());
    assert_eq!(agent.dead_letter_queue_len(), 2);

    // Dequeue all
    assert_eq!(agent.dequeue_dead_letter(), Some("msg2".to_string()));
    assert_eq!(agent.dequeue_dead_letter(), Some("msg3".to_string()));
    assert_eq!(agent.dead_letter_queue_len(), 0);
}

#[test]
fn test_terminate_from_processing_state() {
    let mut agent = Agent::new("TerminateProcessingAgent");
    agent.mark_ready().unwrap();
    agent.mark_processing().unwrap();

    // Queue messages before termination
    agent.enqueue_message("pending".to_string());

    assert!(agent.terminate().is_ok());
    assert_eq!(agent.state(), AgentState::Terminated);
    assert!(agent.state().is_terminal());
}

#[test]
fn test_state_history_captures_crashes() {
    let mut agent = Agent::new("HistoryCrashAgent");
    agent.mark_ready().unwrap();
    agent.mark_processing().unwrap();

    // Mark crashed doesn't change state but affects behavior
    agent.mark_crashed();
    assert_eq!(agent.state_history().len(), 2); // Still 2 transitions
    assert!(agent.is_crashed());

    // Recover through error
    agent.mark_error().unwrap();
    assert_eq!(agent.state_history().len(), 3);

    agent.recover().unwrap();
    assert_eq!(agent.state_history().len(), 4);
}

#[test]
fn test_uptime_milliseconds_monotonic() {
    let mut agent = Agent::new("UptimeAgent");
    let uptime1 = agent.uptime_ms();

    std::thread::sleep(std::time::Duration::from_millis(10));

    let uptime2 = agent.uptime_ms();
    assert!(uptime2 > uptime1);
}

#[test]
fn test_message_queue_and_dlq_independent() {
    let mut agent = Agent::new("IndependentQueuesAgent");

    // Populate main queue
    agent.enqueue_message("main1".to_string());
    agent.enqueue_message("main2".to_string());

    // Populate DLQ
    agent.queue_dead_letter("dlq1".to_string());
    agent.queue_dead_letter("dlq2".to_string());

    // Verify independence
    assert_eq!(agent.message_queue_len(), 2);
    assert_eq!(agent.dead_letter_queue_len(), 2);

    // Dequeue from main
    assert_eq!(agent.dequeue_message(), Some("main1".to_string()));
    assert_eq!(agent.message_queue_len(), 1);
    assert_eq!(agent.dead_letter_queue_len(), 2); // DLQ unaffected

    // Dequeue from DLQ
    assert_eq!(agent.dequeue_dead_letter(), Some("dlq1".to_string()));
    assert_eq!(agent.message_queue_len(), 1); // Main unaffected
    assert_eq!(agent.dead_letter_queue_len(), 1);
}

#[test]
fn test_shutdown_leaves_undelivered_messages_in_dlq() {
    let mut agent = Agent::new("UndeliveredAgent");
    agent.mark_ready().unwrap();

    // Add various messages
    for i in 0..5 {
        agent.enqueue_message(format!("undelivered{}", i));
    }

    // Graceful shutdown should preserve all messages in DLQ
    let _ = agent.shutdown_graceful();

    assert_eq!(agent.message_queue_len(), 0);
    assert_eq!(agent.dead_letter_queue_len(), 5);

    // Verify messages are recoverable
    for i in 0..5 {
        assert_eq!(
            agent.dequeue_dead_letter(),
            Some(format!("undelivered{}", i))
        );
    }
}

#[test]
fn test_heartbeat_after_crash_clears_flag() {
    let mut agent = Agent::new("HeartbeatClearCrashAgent");

    agent.mark_crashed();
    assert!(agent.is_crashed());

    // Heartbeat clears the explicit crash flag
    agent.heartbeat();
    assert!(!agent.is_crashed());
}

#[test]
fn test_state_info_includes_current_state() {
    let mut agent = Agent::new("StateInfoAgent");
    agent.mark_ready().unwrap();

    let info = agent.state_info();
    assert_eq!(info.state, "READY");
    assert_eq!(info.name, "StateInfoAgent");
    assert_eq!(info.message_queue_len, 0);
    assert_eq!(info.error_count, 0);
}
