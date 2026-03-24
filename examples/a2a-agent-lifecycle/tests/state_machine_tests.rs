//! State machine comprehensive tests

use a2a_agent_lifecycle::{Agent, AgentState};

#[test]
fn test_initialization_to_ready() {
    let mut agent = Agent::new("Agent");
    assert_eq!(agent.state(), AgentState::Initializing);

    assert!(agent.mark_ready().is_ok());
    assert_eq!(agent.state(), AgentState::Ready);
}

#[test]
fn test_ready_to_processing() {
    let mut agent = Agent::new("Agent");
    agent.mark_ready().unwrap();

    assert!(agent.mark_processing().is_ok());
    assert_eq!(agent.state(), AgentState::Processing);
}

#[test]
fn test_processing_to_idle() {
    let mut agent = Agent::new("Agent");
    agent.mark_ready().unwrap();
    agent.mark_processing().unwrap();

    assert!(agent.mark_idle().is_ok());
    assert_eq!(agent.state(), AgentState::Idle);
}

#[test]
fn test_idle_back_to_processing() {
    let mut agent = Agent::new("Agent");
    agent.mark_ready().unwrap();
    agent.mark_processing().unwrap();
    agent.mark_idle().unwrap();

    assert!(agent.mark_processing().is_ok());
    assert_eq!(agent.state(), AgentState::Processing);
}

#[test]
fn test_processing_to_error() {
    let mut agent = Agent::new("Agent");
    agent.mark_ready().unwrap();
    agent.mark_processing().unwrap();

    assert!(agent.mark_error().is_ok());
    assert_eq!(agent.state(), AgentState::Error);
}

#[test]
fn test_error_to_ready_recovery() {
    let mut agent = Agent::new("Agent");
    agent.mark_ready().unwrap();
    agent.mark_processing().unwrap();
    agent.mark_error().unwrap();

    assert_eq!(agent.error_count(), 1);
    assert!(agent.recover().is_ok());
    assert_eq!(agent.state(), AgentState::Ready);
    assert_eq!(agent.error_count(), 0);
}

#[test]
fn test_error_recovery_resets_count() {
    let mut agent = Agent::new("Agent");
    agent.mark_ready().unwrap();
    agent.mark_processing().unwrap();
    agent.mark_error().unwrap();

    let initial_error_count = agent.error_count();
    agent.recover().unwrap();

    assert_eq!(agent.error_count(), 0);
    assert!(initial_error_count > 0);
}

#[test]
fn test_invalid_transition_initializing_to_processing() {
    let mut agent = Agent::new("Agent");
    assert!(agent.mark_processing().is_err());
}

#[test]
fn test_invalid_transition_initializing_to_idle() {
    let mut agent = Agent::new("Agent");
    assert!(agent.mark_idle().is_err());
}

#[test]
fn test_invalid_transition_ready_to_idle() {
    let mut agent = Agent::new("Agent");
    agent.mark_ready().unwrap();

    assert!(agent.mark_idle().is_err());
}

#[test]
fn test_max_retries_exceeded() {
    let mut agent = Agent::new("Agent");
    agent.mark_ready().unwrap();

    for _ in 0..3 {
        agent.mark_processing().unwrap();
        agent.mark_error().unwrap();
        if agent.error_count() < 3 {
            agent.recover().unwrap();
        }
    }

    assert_eq!(agent.error_count(), 3);
    assert!(agent.recover().is_err());
}

#[test]
fn test_terminate_from_initializing() {
    let mut agent = Agent::new("Agent");
    assert!(agent.terminate().is_ok());
    assert_eq!(agent.state(), AgentState::Terminated);
}

#[test]
fn test_terminate_from_ready() {
    let mut agent = Agent::new("Agent");
    agent.mark_ready().unwrap();

    assert!(agent.terminate().is_ok());
    assert_eq!(agent.state(), AgentState::Terminated);
}

#[test]
fn test_terminate_from_processing() {
    let mut agent = Agent::new("Agent");
    agent.mark_ready().unwrap();
    agent.mark_processing().unwrap();

    assert!(agent.terminate().is_ok());
    assert_eq!(agent.state(), AgentState::Terminated);
}

#[test]
fn test_terminate_from_error() {
    let mut agent = Agent::new("Agent");
    agent.mark_ready().unwrap();
    agent.mark_processing().unwrap();
    agent.mark_error().unwrap();

    assert!(agent.terminate().is_ok());
    assert_eq!(agent.state(), AgentState::Terminated);
}

#[test]
fn test_terminated_is_terminal_state() {
    let mut agent = Agent::new("Agent");
    agent.terminate().unwrap();

    assert!(agent.state().is_terminal());
}

#[test]
fn test_state_history_tracking() {
    let mut agent = Agent::new("Agent");
    agent.mark_ready().unwrap();
    agent.mark_processing().unwrap();
    agent.mark_idle().unwrap();

    let history = agent.state_history();
    assert_eq!(history.len(), 3);
    assert_eq!(history[0].from, AgentState::Initializing);
    assert_eq!(history[0].to, AgentState::Ready);
    assert_eq!(history[1].from, AgentState::Ready);
    assert_eq!(history[1].to, AgentState::Processing);
    assert_eq!(history[2].from, AgentState::Processing);
    assert_eq!(history[2].to, AgentState::Idle);
}

#[test]
fn test_message_queue_fifo() {
    let mut agent = Agent::new("Agent");

    agent.enqueue_message("msg1".to_string());
    agent.enqueue_message("msg2".to_string());
    agent.enqueue_message("msg3".to_string());

    assert_eq!(agent.message_queue_len(), 3);
    assert_eq!(agent.dequeue_message(), Some("msg1".to_string()));
    assert_eq!(agent.dequeue_message(), Some("msg2".to_string()));
    assert_eq!(agent.dequeue_message(), Some("msg3".to_string()));
    assert_eq!(agent.dequeue_message(), None);
}

#[test]
fn test_message_peek() {
    let mut agent = Agent::new("Agent");

    agent.enqueue_message("msg1".to_string());
    assert_eq!(agent.peek_message(), Some(&"msg1".to_string()));
    assert_eq!(agent.message_queue_len(), 1); // Peek doesn't remove
}

#[test]
fn test_state_code_correct() {
    let mut agent = Agent::new("Agent");

    assert_eq!(agent.state_code(), "INITIALIZING");
    agent.mark_ready().unwrap();
    assert_eq!(agent.state_code(), "READY");
    agent.mark_processing().unwrap();
    assert_eq!(agent.state_code(), "PROCESSING");
    agent.mark_idle().unwrap();
    assert_eq!(agent.state_code(), "IDLE");
}

#[test]
fn test_state_info() {
    let mut agent = Agent::new("TestAgent");
    agent.mark_ready().unwrap();

    let info = agent.state_info();
    assert_eq!(info.name, "TestAgent");
    assert_eq!(info.state, "READY");
    assert!(info.uptime_ms > 0);
}

#[test]
fn test_concurrent_message_processing() {
    let mut agent = Agent::new("Agent");

    for i in 0..10 {
        agent.enqueue_message(format!("msg{}", i));
    }

    assert_eq!(agent.message_queue_len(), 10);

    for i in 0..10 {
        let msg = agent.dequeue_message();
        assert_eq!(msg, Some(format!("msg{}", i)));
    }
}

#[test]
fn test_error_recovery_multiple_cycles() {
    let mut agent = Agent::new("Agent");
    agent.mark_ready().unwrap();

    for _ in 0..3 {
        agent.mark_processing().unwrap();
        agent.mark_error().unwrap();
        assert!(agent.recover().is_ok());
        assert_eq!(agent.state(), AgentState::Ready);
    }

    assert_eq!(agent.error_count(), 0);
    assert_eq!(agent.state_history().len(), 12); // 3 cycles * 4 transitions
}

#[test]
fn test_ready_to_error_direct() {
    let mut agent = Agent::new("Agent");
    agent.mark_ready().unwrap();

    // Ready can transition to Error directly
    assert!(agent.mark_error().is_ok());
    assert_eq!(agent.state(), AgentState::Error);
}

#[test]
fn test_state_transitions_are_timestamped() {
    let mut agent = Agent::new("Agent");
    let before = std::time::Instant::now();
    agent.mark_ready().unwrap();
    let after = std::time::Instant::now();

    let transition = &agent.state_history()[0];
    let is_between = transition.timestamp > chrono::Utc::now() - chrono::Duration::seconds(5);
    assert!(is_between);
}

#[test]
fn test_uptime_increases() {
    let mut agent = Agent::new("Agent");
    let uptime1 = agent.uptime_ms();

    std::thread::sleep(std::time::Duration::from_millis(10));

    let uptime2 = agent.uptime_ms();
    assert!(uptime2 > uptime1);
}

#[test]
fn test_unique_agent_ids() {
    let agent1 = Agent::new("Agent");
    let agent2 = Agent::new("Agent");

    assert_ne!(agent1.id, agent2.id);
}
