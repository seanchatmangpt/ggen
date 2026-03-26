//! Message routing and delivery tests

use a2a_agent_lifecycle::{Message, MessageRouter, MessageType};

#[test]
fn test_direct_message_delivery() {
    let mut router = MessageRouter::new();
    router.register_agent("agent-1");
    router.register_agent("agent-2");

    let msg = Message::new(
        MessageType::TaskRequest,
        "agent-1",
        Some("agent-2".to_string()),
        serde_json::json!({"task": "test"}),
    );

    assert!(router.send(msg).is_ok());
    assert_eq!(router.queue_len("agent-2").unwrap(), 1);
    assert_eq!(router.queue_len("agent-1").unwrap(), 0);
}

#[test]
fn test_broadcast_delivery() {
    let mut router = MessageRouter::new();
    router.register_agent("sender");
    router.register_agent("receiver-1");
    router.register_agent("receiver-2");
    router.register_agent("receiver-3");

    let msg = Message::new(
        MessageType::StateTransition,
        "sender",
        None, // Broadcast
        serde_json::json!({}),
    );

    router.send(msg).unwrap();

    assert_eq!(router.queue_len("receiver-1").unwrap(), 1);
    assert_eq!(router.queue_len("receiver-2").unwrap(), 1);
    assert_eq!(router.queue_len("receiver-3").unwrap(), 1);
    assert_eq!(router.queue_len("sender").unwrap(), 0); // Sender doesn't receive
}

#[test]
fn test_fifo_ordering() {
    let mut router = MessageRouter::new();
    router.register_agent("agent-1");
    router.register_agent("agent-2");

    for i in 0..10 {
        let msg = Message::new(
            MessageType::TaskResult,
            "agent-1",
            Some("agent-2".to_string()),
            serde_json::json!({"sequence": i}),
        );
        router.send(msg).unwrap();
    }

    for i in 0..10 {
        let msg = router.receive("agent-2").unwrap().unwrap();
        let seq = msg.payload.get("sequence").unwrap().as_i64().unwrap();
        assert_eq!(seq, i as i64);
    }
}

#[test]
fn test_message_ack_tracking() {
    let mut router = MessageRouter::new();
    router.register_agent("agent-1");
    router.register_agent("agent-2");

    let msg = Message::new(
        MessageType::TaskRequest,
        "agent-1",
        Some("agent-2".to_string()),
        serde_json::json!({}),
    );
    let msg_id = msg.id.clone();

    router.send(msg).unwrap();
    assert!(router.receive("agent-2").is_ok());

    // Acknowledge message
    assert!(router.ack("agent-2", &msg_id).is_ok());
}

#[test]
fn test_multiple_destinations() {
    let mut router = MessageRouter::new();
    router.register_agent("sender");
    router.register_agent("dest-1");
    router.register_agent("dest-2");
    router.register_agent("dest-3");

    // Send to dest-1
    let msg1 = Message::new(
        MessageType::TaskRequest,
        "sender",
        Some("dest-1".to_string()),
        serde_json::json!({"id": 1}),
    );
    router.send(msg1).unwrap();

    // Send to dest-2
    let msg2 = Message::new(
        MessageType::TaskRequest,
        "sender",
        Some("dest-2".to_string()),
        serde_json::json!({"id": 2}),
    );
    router.send(msg2).unwrap();

    // Send to dest-3
    let msg3 = Message::new(
        MessageType::TaskRequest,
        "sender",
        Some("dest-3".to_string()),
        serde_json::json!({"id": 3}),
    );
    router.send(msg3).unwrap();

    assert_eq!(router.queue_len("dest-1").unwrap(), 1);
    assert_eq!(router.queue_len("dest-2").unwrap(), 1);
    assert_eq!(router.queue_len("dest-3").unwrap(), 1);
}

#[test]
fn test_send_to_unregistered_agent() {
    let mut router = MessageRouter::new();
    router.register_agent("agent-1");

    let msg = Message::new(
        MessageType::TaskRequest,
        "agent-1",
        Some("nonexistent".to_string()),
        serde_json::json!({}),
    );

    assert!(router.send(msg).is_err());
}

#[test]
fn test_message_log() {
    let mut router = MessageRouter::new();
    router.register_agent("agent-1");
    router.register_agent("agent-2");

    for _ in 0..5 {
        let msg = Message::new(
            MessageType::TaskRequest,
            "agent-1",
            Some("agent-2".to_string()),
            serde_json::json!({}),
        );
        router.send(msg).unwrap();
    }

    assert_eq!(router.message_log().len(), 5);
}

#[test]
fn test_router_stats() {
    let mut router = MessageRouter::new();
    router.register_agent("agent-1");
    router.register_agent("agent-2");

    for _ in 0..3 {
        let msg = Message::new(
            MessageType::TaskRequest,
            "agent-1",
            Some("agent-2".to_string()),
            serde_json::json!({}),
        );
        router.send(msg).unwrap();
    }

    let stats = router.stats();
    assert_eq!(stats.total_messages, 3);
    assert_eq!(stats.pending_messages, 3);
}

#[test]
fn test_message_type_requires_ack() {
    let task_request = Message::new(
        MessageType::TaskRequest,
        "agent-1",
        Some("agent-2".to_string()),
        serde_json::json!({}),
    );
    assert!(task_request.requires_ack);

    let state_transition = Message::new(
        MessageType::StateTransition,
        "agent-1",
        None,
        serde_json::json!({}),
    );
    assert!(!state_transition.requires_ack);
}

#[test]
fn test_mixed_message_types() {
    let mut router = MessageRouter::new();
    router.register_agent("coordinator");
    router.register_agent("worker");

    let task_req = Message::new(
        MessageType::TaskRequest,
        "coordinator",
        Some("worker".to_string()),
        serde_json::json!({}),
    );
    let state_trans = Message::new(
        MessageType::StateTransition,
        "coordinator",
        None,
        serde_json::json!({}),
    );
    let error_msg = Message::new(
        MessageType::Error,
        "worker",
        Some("coordinator".to_string()),
        serde_json::json!({}),
    );

    router.send(task_req).unwrap();
    router.send(state_trans).unwrap();
    router.send(error_msg).unwrap();

    assert_eq!(router.message_log().len(), 3);
}

#[test]
fn test_message_delivery_latency() {
    let mut msg = Message::new(
        MessageType::TaskRequest,
        "agent-1",
        Some("agent-2".to_string()),
        serde_json::json!({}),
    );

    assert!(msg.delivery_latency_ms().is_none());

    msg.mark_delivered();
    assert!(msg.delivery_latency_ms().is_some());
}

#[test]
fn test_peek_without_dequeue() {
    let mut router = MessageRouter::new();
    router.register_agent("agent-1");
    router.register_agent("agent-2");

    let msg = Message::new(
        MessageType::TaskRequest,
        "agent-1",
        Some("agent-2".to_string()),
        serde_json::json!({"id": 1}),
    );
    router.send(msg).unwrap();

    let peeked = router.peek("agent-2").unwrap();
    assert!(peeked.is_some());

    // Queue length unchanged
    assert_eq!(router.queue_len("agent-2").unwrap(), 1);

    // Can still receive
    let received = router.receive("agent-2").unwrap();
    assert!(received.is_some());
    assert_eq!(router.queue_len("agent-2").unwrap(), 0);
}

#[test]
fn test_large_broadcast() {
    let mut router = MessageRouter::new();

    for i in 0..20 {
        router.register_agent(&format!("agent-{}", i));
    }

    let msg = Message::new(
        MessageType::StateTransition,
        "agent-0",
        None,
        serde_json::json!({}),
    );
    router.send(msg).unwrap();

    // All except sender should have the message
    for i in 1..20 {
        assert_eq!(router.queue_len(&format!("agent-{}", i)).unwrap(), 1);
    }
}

#[test]
fn test_message_ordering_with_broadcast() {
    let mut router = MessageRouter::new();
    router.register_agent("sender");
    router.register_agent("receiver");

    // Send both direct and broadcast
    for i in 0..5 {
        let msg = Message::new(
            MessageType::TaskRequest,
            "sender",
            Some("receiver".to_string()),
            serde_json::json!({"seq": i}),
        );
        router.send(msg).unwrap();
    }

    // Verify FIFO order
    for i in 0..5 {
        let msg = router.receive("receiver").unwrap().unwrap();
        let seq = msg.payload.get("seq").unwrap().as_i64().unwrap();
        assert_eq!(seq, i as i64);
    }
}

#[test]
fn test_concurrent_sends() {
    let mut router = MessageRouter::new();
    router.register_agent("agent-1");
    router.register_agent("agent-2");

    // Send from agent-1 to agent-2 and vice versa
    for i in 0..3 {
        let msg1 = Message::new(
            MessageType::TaskRequest,
            "agent-1",
            Some("agent-2".to_string()),
            serde_json::json!({"from": "1", "seq": i}),
        );
        let msg2 = Message::new(
            MessageType::TaskResult,
            "agent-2",
            Some("agent-1".to_string()),
            serde_json::json!({"from": "2", "seq": i}),
        );

        router.send(msg1).unwrap();
        router.send(msg2).unwrap();
    }

    // Check final state
    let stats = router.stats();
    assert_eq!(stats.total_messages, 6);
    assert_eq!(stats.pending_messages, 6);
}

#[test]
fn test_message_payload_preservation() {
    let mut router = MessageRouter::new();
    router.register_agent("agent-1");
    router.register_agent("agent-2");

    let original_payload = serde_json::json!({
        "task_id": "123",
        "description": "important task",
        "priority": "high",
        "data": [1, 2, 3, 4, 5]
    });

    let msg = Message::new(
        MessageType::TaskRequest,
        "agent-1",
        Some("agent-2".to_string()),
        original_payload.clone(),
    );
    router.send(msg).unwrap();

    let received = router.receive("agent-2").unwrap().unwrap();
    assert_eq!(received.payload, original_payload);
}
