//! A2A Self-Play Integration Tests
//!
//! Exercises A2A message routing, batch processing, task lifecycle,
//! YAWL bridging, and (optionally) live Groq calls. All tests except
//! the live Groq test run without any API keys.
//!
//! ## Test inventory
//!
//! 1. `test_a2a_message_converter_roundtrips`  -- A2A <-> LLM conversion
//! 2. `test_a2a_message_router_dispatches`     -- MessageRouter dispatching
//! 3. `test_a2a_batch_processor`               -- BatchProcessor with 5 messages
//! 4. `test_a2a_conversation_thread`           -- Correlation ID threading
//! 5. `test_a2a_state_lifecycle`               -- YAWL state transition rules
//! 6. `test_a2a_yawl_workflow`                 -- TaskMapper + YawlEventPublisher
//! 7. `test_a2a_groq_live_client`              -- Live Groq call (gated behind feature)

use std::sync::Arc;

use a2a_generated::converged::message::{
    ConvergedMessage, ConvergedMessageType, ConvergedPayload, MessageEnvelope, MessageLifecycle,
    MessagePriority, MessageRouting, MessageState, QoSRequirements, ReliabilityLevel,
    UnifiedContent,
};
use chrono::Utc;
use ggen_a2a_mcp::{
    A2aMessageConverter, BatchProcessor, LlmResponse, MessageRouter, TaskMapper,
    YawlEventPublisher, YawlStateMapper, YawlTask, YawlTaskType,
};

mod common;
use common::init_tracing;

// ---------------------------------------------------------------------------
// Helpers
// ---------------------------------------------------------------------------

/// Build a minimal `MessageRouting` for tests.
fn test_routing() -> MessageRouting {
    MessageRouting {
        path: vec!["test-agent".to_string()],
        metadata: None,
        qos: QoSRequirements {
            reliability: ReliabilityLevel::AtLeastOnce,
            latency: None,
            throughput: None,
        },
    }
}

/// Build a minimal `MessageLifecycle` for tests.
fn test_lifecycle() -> MessageLifecycle {
    MessageLifecycle {
        state: MessageState::Created,
        history: Vec::new(),
        timeout: None,
    }
}

/// Create a text `ConvergedMessage` with an optional correlation ID.
fn make_text_message(
    id: &str, source: &str, content: &str, correlation_id: Option<&str>,
) -> ConvergedMessage {
    let mut msg = ConvergedMessage::text(id.to_string(), source.to_string(), content.to_string());
    if let Some(cid) = correlation_id {
        msg.envelope.correlation_id = Some(cid.to_string());
    }
    msg
}

/// Create a data (Query-type) `ConvergedMessage`.
fn make_data_message(id: &str, source: &str) -> ConvergedMessage {
    let mut data = serde_json::Map::new();
    data.insert(
        "key".to_string(),
        serde_json::Value::String("value".to_string()),
    );
    data.insert("count".to_string(), serde_json::Value::Number(42.into()));

    ConvergedMessage {
        message_id: id.to_string(),
        source: source.to_string(),
        target: None,
        envelope: MessageEnvelope {
            message_type: ConvergedMessageType::Query,
            priority: MessagePriority::Normal,
            timestamp: Utc::now(),
            schema_version: "1.0".to_string(),
            content_type: "application/json".to_string(),
            correlation_id: None,
            causation_chain: None,
        },
        payload: ConvergedPayload {
            content: UnifiedContent::Data {
                data,
                schema: Some("test-schema".to_string()),
            },
            context: None,
            hints: None,
            integrity: None,
        },
        routing: test_routing(),
        lifecycle: test_lifecycle(),
        extensions: None,
    }
}

// ===========================================================================
// Test 1: A2A Message Converter Round-Trip
// ===========================================================================

#[tokio::test]
async fn test_a2a_message_converter_roundtrips() {
    init_tracing();
    // Build converter with a custom system prompt
    let converter = A2aMessageConverter::new().with_system_prompt("test-system-prompt");

    // Create an incoming ConvergedMessage
    let msg = ConvergedMessage::text(
        "m1".to_string(),
        "architect".to_string(),
        "Design API".to_string(),
    );

    // --- Forward: A2A -> LLM Request ---
    let llm_req = converter
        .a2a_to_llm_request(&msg)
        .expect("a2a_to_llm_request should succeed");

    assert_eq!(
        llm_req.system_prompt, "test-system-prompt",
        "system prompt should be preserved"
    );
    assert_eq!(
        llm_req.user_content, "Design API",
        "user content should match original text"
    );
    assert_eq!(llm_req.message_id, "m1", "message_id should propagate");

    // --- Reverse: LLM Response -> A2A Message ---
    let llm_response = LlmResponse {
        content: "API design...".to_string(),
        model: "mock".to_string(),
        usage: None,
    };

    let response_msg = converter
        .llm_response_to_a2a(&llm_response, &msg)
        .expect("llm_response_to_a2a should succeed");

    // Source of the response must be the original source (architect)
    assert_eq!(
        response_msg.source, "architect",
        "response source should be preserved from original message"
    );
    // The response content must contain the LLM output
    match &response_msg.payload.content {
        UnifiedContent::Text { content, .. } => {
            assert!(
                content.contains("API design..."),
                "response body should contain LLM output, got: {content}"
            );
        }
        other => panic!("expected Text content in response, got: {other:?}"),
    }
}

// ===========================================================================
// Test 2: Message Router Dispatches
// ===========================================================================

#[tokio::test]
async fn test_a2a_message_router_dispatches() {
    init_tracing();
    // Use with_defaults() which registers TextContentHandler + DataContentHandler + ...
    let router = MessageRouter::with_defaults();

    // --- Dispatch a text message ---
    let text_msg = make_text_message("t1", "agent-a", "Hello router", None);
    let text_result = router
        .route(text_msg)
        .await
        .expect("text route should succeed");
    match &text_result.payload.content {
        UnifiedContent::Text { content, .. } => {
            assert!(
                content.contains("Processed"),
                "text handler should prefix with 'Processed', got: {content}"
            );
        }
        other => panic!("expected Text response for text input, got: {other:?}"),
    }

    // --- Dispatch a data (Command-type) message ---
    // Use Command type because only DataContentHandler handles Command
    // (TextContentHandler handles Direct/Task/Query, causing a collision on Query).
    let mut data_msg = make_data_message("d1", "agent-b");
    data_msg.envelope.message_type = ConvergedMessageType::Command;
    let data_result = router
        .route(data_msg)
        .await
        .expect("data route should succeed");
    match &data_result.payload.content {
        UnifiedContent::Text { content, .. } => {
            assert!(
                content.contains("processed"),
                "data handler should return 'processed' status, got: {content}"
            );
        }
        other => panic!("expected Text response for data input, got: {other:?}"),
    }
}

// ===========================================================================
// Test 3: Batch Processor
// ===========================================================================

#[tokio::test]
async fn test_a2a_batch_processor() {
    init_tracing();
    let router = Arc::new(MessageRouter::with_defaults());
    let processor = BatchProcessor::new(router).with_max_concurrent(5);

    // Create 5 text messages (all handled by TextContentHandler via Direct type)
    let messages: Vec<ConvergedMessage> = (0..5)
        .map(|i| {
            make_text_message(
                &format!("batch-{}", i),
                "agent",
                &format!("msg {}", i),
                None,
            )
        })
        .collect();

    let batch_result = processor.process_batch(messages).await;

    assert_eq!(
        batch_result.total, 5,
        "should have processed 5 messages total"
    );
    assert_eq!(batch_result.successful, 5, "all 5 messages should succeed");
    assert_eq!(batch_result.failed, 0, "no messages should fail");

    // Verify individual results
    for individual in &batch_result.results {
        assert!(
            individual.success,
            "message {} should have succeeded",
            individual.message_id
        );
        assert!(
            individual.error.is_none(),
            "message {} should have no error",
            individual.message_id
        );
    }
}

// ===========================================================================
// Test 4: Conversation Thread (Correlation ID)
// ===========================================================================

#[tokio::test]
async fn test_a2a_conversation_thread() {
    init_tracing();
    let router = MessageRouter::with_defaults();
    let correlation_id = "conv-thread-42";

    // Create 3 messages in the same conversation
    let messages: Vec<ConvergedMessage> = (0..3)
        .map(|i| {
            make_text_message(
                &format!("thread-msg-{}", i),
                "architect",
                &format!("Turn {} of conversation", i),
                Some(correlation_id),
            )
        })
        .collect();

    let mut responses: Vec<ConvergedMessage> = Vec::new();
    for msg in messages {
        let resp = router.route(msg).await.expect("routing should succeed");
        responses.push(resp);
    }

    // All 3 responses must exist
    assert_eq!(responses.len(), 3, "should have 3 responses");

    // Each response's message_id should contain the original message_id
    // (the handler creates "{original_id}-response")
    for (idx, resp) in responses.iter().enumerate() {
        assert!(
            resp.message_id.contains(&format!("thread-msg-{}", idx)),
            "response {} should reference original message id",
            idx
        );
    }

    // The original messages all carried the correlation_id; verify the
    // router processed them (the response is a new message but the handler
    // copies source from the original). Verify source is preserved.
    for resp in &responses {
        assert_eq!(
            resp.source, "architect",
            "response source should be preserved"
        );
    }
}

// ===========================================================================
// Test 5: YAWL State Lifecycle
// ===========================================================================

#[test]
fn test_a2a_state_lifecycle() {
    init_tracing();
    // --- Valid forward transitions ---
    assert!(
        YawlStateMapper::is_valid_transition("NotStarted", "Ready"),
        "NotStarted -> Ready should be valid"
    );
    assert!(
        YawlStateMapper::is_valid_transition("Ready", "Executing"),
        "Ready -> Executing should be valid"
    );
    assert!(
        YawlStateMapper::is_valid_transition("Executing", "Completed"),
        "Executing -> Completed should be valid"
    );

    // --- Terminal state: Completed cannot transition ---
    assert!(
        !YawlStateMapper::is_valid_transition("Completed", "Executing"),
        "Completed -> Executing should be INVALID (terminal state)"
    );
    assert!(
        !YawlStateMapper::is_valid_transition("Completed", "Ready"),
        "Completed -> Ready should be INVALID"
    );

    // --- Error transitions from non-terminal ---
    assert!(
        YawlStateMapper::is_valid_transition("Executing", "Failed"),
        "Executing -> Failed should be valid (error transition)"
    );
    assert!(
        YawlStateMapper::is_valid_transition("NotStarted", "Cancelled"),
        "NotStarted -> Cancelled should be valid"
    );

    // --- Suspend / resume cycle ---
    assert!(
        YawlStateMapper::is_valid_transition("Executing", "Suspended"),
        "Executing -> Suspended should be valid"
    );
    assert!(
        YawlStateMapper::is_valid_transition("Suspended", "Executing"),
        "Suspended -> Executing should be valid (resume)"
    );

    // --- Terminal state detection ---
    assert!(YawlStateMapper::is_terminal("Completed"));
    assert!(YawlStateMapper::is_terminal("Failed"));
    assert!(YawlStateMapper::is_terminal("Cancelled"));
    assert!(!YawlStateMapper::is_terminal("NotStarted"));
    assert!(!YawlStateMapper::is_terminal("Ready"));
    assert!(!YawlStateMapper::is_terminal("Executing"));

    // --- Valid next states from "Ready" ---
    let next = YawlStateMapper::valid_next_states("Ready");
    assert!(
        next.contains(&"Ready"),
        "same-state should be in next states"
    );
    assert!(next.contains(&"Executing"));
    assert!(next.contains(&"Failed"));
    assert!(next.contains(&"Cancelled"));
}

// ===========================================================================
// Test 6: YAWL Workflow (TaskMapper + EventPublisher)
// ===========================================================================

#[tokio::test]
async fn test_a2a_yawl_workflow() {
    init_tracing();
    // --- TaskMapper: batch convert 3 YAWL tasks to A2A messages ---
    let mapper = TaskMapper::new();

    let tasks = vec![
        YawlTask {
            id: "t1".to_string(),
            name: "Authenticate".to_string(),
            task_type: YawlTaskType::Atomic,
            split_type: None,
            join_type: None,
            input_data: Some(serde_json::json!({"user": "alice"})),
            workflow_id: Some("wf-1".to_string()),
            parent_id: None,
        },
        YawlTask {
            id: "t2".to_string(),
            name: "Validate Input".to_string(),
            task_type: YawlTaskType::Atomic,
            split_type: None,
            join_type: None,
            input_data: None,
            workflow_id: Some("wf-1".to_string()),
            parent_id: None,
        },
        YawlTask {
            id: "t3".to_string(),
            name: "Process Order".to_string(),
            task_type: YawlTaskType::Atomic,
            split_type: None,
            join_type: None,
            input_data: Some(serde_json::json!({"order_id": 99})),
            workflow_id: Some("wf-1".to_string()),
            parent_id: None,
        },
    ];

    let messages = mapper
        .yawl_to_a2a_batch(&tasks)
        .expect("batch conversion should succeed");
    assert_eq!(messages.len(), 3, "should produce 3 A2A messages");

    // Verify message metadata
    for (msg, task) in messages.iter().zip(tasks.iter()) {
        assert_eq!(
            msg.message_id,
            format!("task-{}", task.id),
            "message_id should be task-{{id}}"
        );
        assert_eq!(msg.source, "yawl-engine", "source should be yawl-engine");
        assert_eq!(
            msg.envelope.message_type,
            ConvergedMessageType::Task,
            "message type should be Task"
        );
    }

    // --- TaskMapper round-trip: YAWL -> A2A -> YAWL ---
    let roundtrip_task = ggen_a2a_mcp::TaskMapper::a2a_to_yawl_task(&messages[0])
        .expect("round-trip should succeed");
    assert_eq!(roundtrip_task.id, "t1");
    assert_eq!(roundtrip_task.name, "Authenticate");
    assert_eq!(roundtrip_task.task_type, YawlTaskType::Atomic);

    // --- YawlEventPublisher: publish task state change ---
    let publisher = YawlEventPublisher::new();
    let event_msg = publisher
        .publish_task_event("wf-1", "t1", "NotStarted", "Ready")
        .await
        .expect("publish_task_event should succeed");

    assert_eq!(
        event_msg.envelope.message_type,
        ConvergedMessageType::Event,
        "event message type should be Event"
    );
    assert_eq!(
        event_msg.envelope.correlation_id,
        Some("t1".to_string()),
        "correlation_id should be the task id"
    );
    assert!(
        event_msg.source.contains("wf-1"),
        "source should contain workflow id, got: {}",
        event_msg.source
    );

    // Verify the event payload contains the state transition
    match &event_msg.payload.content {
        UnifiedContent::Data { data, schema } => {
            assert_eq!(
                schema.as_deref(),
                Some("YawlTaskEvent"),
                "schema should be YawlTaskEvent"
            );
            assert_eq!(
                data.get("oldState").and_then(|v| v.as_str()),
                Some("NotStarted")
            );
            assert_eq!(data.get("newState").and_then(|v| v.as_str()), Some("Ready"));
        }
        other => panic!("expected Data content in event, got: {other:?}"),
    }
}

// ===========================================================================
// Test 7: Live Groq Client (requires GROQ_API_KEY)
// ===========================================================================

#[tokio::test]
async fn test_a2a_groq_live_client() {
    init_tracing();
    // Gate: skip if GROQ_API_KEY is not set
    if std::env::var("GROQ_API_KEY").is_err() {
        eprintln!("skipping: GROQ_API_KEY not set");
        return;
    }

    use ggen_a2a_mcp::client::A2aLlmClient;
    use ggen_ai::dspy::model_capabilities::Model;

    let model = Model::from_name("groq::openai/gpt-oss-20b");
    let client: A2aLlmClient = A2aLlmClient::new(model)
        .await
        .expect("A2aLlmClient creation should succeed");

    let message = ConvergedMessage::text(
        "groq-test-1".to_string(),
        "self-play".to_string(),
        "What is 2+2? Reply with just the number.".to_string(),
    );

    let response: ConvergedMessage = client
        .process_message(&message)
        .await
        .expect("process_message should succeed");

    // Extract text from response
    let response_text = match &response.payload.content {
        UnifiedContent::Text { content, .. } => content.as_str(),
        other => panic!("expected Text content in Groq response, got: {other:?}"),
    };

    assert!(
        response_text.contains('4'),
        "Groq response should contain '4', got: {response_text}"
    );

    // Clean up
    client.shutdown().await.expect("shutdown should succeed");
}
