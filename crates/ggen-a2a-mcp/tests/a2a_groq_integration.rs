//! A2A + Groq Integration Tests — 8 tests bridging message routing with LLM calls
//!
//! Tests 1-4: Unit/integration (no API key needed)
//!
//! Run no-key tests:
//!   cargo test -p ggen-a2a-mcp --test a2a_groq_integration -- --test-threads=1 --nocapture
//!
//! Run live Groq tests (with tracing visible):
//!   RUST_LOG=info GROQ_API_KEY=gsk_xxx cargo test -p ggen-a2a-mcp --test a2a_groq_integration \
//!     -- --test-threads=1 --nocapture

use std::sync::Arc;
use std::time::Instant;

mod common;
use common::init_tracing;

use a2a_generated::converged::message::{
    ConvergedMessage, ConvergedMessageType, ConvergedPayload, MessageEnvelope, MessageLifecycle,
    MessagePriority, MessageRouting, MessageState, QoSRequirements, ReliabilityLevel,
    UnifiedContent,
};
use chrono::Utc;
use futures::StreamExt;
use ggen_a2a_mcp::{
    A2aMessageConverter, BatchProcessor, MessageRouter, TaskMapper, YawlEventPublisher,
    YawlJoinType, YawlSplitType, YawlStateMapper, YawlTask, YawlTaskType,
};

// ---------------------------------------------------------------------------
// Helpers
// ---------------------------------------------------------------------------

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

fn test_lifecycle() -> MessageLifecycle {
    MessageLifecycle {
        state: MessageState::Created,
        history: Vec::new(),
        timeout: None,
    }
}

/// Create a text ConvergedMessage with full envelope fields set.
fn make_text_message(
    id: &str, source: &str, content: &str, correlation_id: Option<&str>,
) -> ConvergedMessage {
    let msg = ConvergedMessage {
        message_id: id.to_string(),
        source: source.to_string(),
        target: Some("target-agent".to_string()),
        envelope: MessageEnvelope {
            message_type: ConvergedMessageType::Task,
            priority: MessagePriority::Normal,
            timestamp: Utc::now(),
            schema_version: "1.0".to_string(),
            content_type: "text/plain".to_string(),
            correlation_id: correlation_id.map(|s| s.to_string()),
            causation_chain: Some(vec!["origin-msg".to_string()]),
        },
        payload: ConvergedPayload {
            content: UnifiedContent::Text {
                content: content.to_string(),
                metadata: None,
            },
            context: None,
            hints: None,
            integrity: None,
        },
        routing: test_routing(),
        lifecycle: test_lifecycle(),
        extensions: None,
    };
    msg
}

/// Create a data (Command-type) ConvergedMessage.
fn make_data_message(id: &str, source: &str) -> ConvergedMessage {
    let mut data = serde_json::Map::new();
    data.insert(
        "action".to_string(),
        serde_json::Value::String("deploy".to_string()),
    );
    data.insert(
        "target_env".to_string(),
        serde_json::Value::String("production".to_string()),
    );

    ConvergedMessage {
        message_id: id.to_string(),
        source: source.to_string(),
        target: None,
        envelope: MessageEnvelope {
            message_type: ConvergedMessageType::Command,
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
                schema: Some("deploy-cmd".to_string()),
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

/// Estimate token count from character length (~4 chars per token).
fn estimate_tokens(content: &str) -> usize {
    content.len() / 4
}

// ===========================================================================
// Test 1: A2A Router Processes Task to Structured Output
//
// Create a ConvergedMessage with Task type, target, correlation_id, and
// causation_chain. Route through MessageRouter::with_defaults(). Assert:
//   - source preserved in response
//   - message_id contains original message_id
//   - routing.path is non-empty
//   - content is Text with "Processed"
// ===========================================================================

#[tokio::test]
async fn test_a2a_router_processes_task_to_structured_output() {
    let router = MessageRouter::with_defaults();

    let message = make_text_message(
        "task-migrate-db-001",
        "pm-agent",
        "Design the PostgreSQL to CockroachDB migration plan for the orders table",
        Some("migration-correlation-42"),
    );

    let response = router.route(message).await.expect("routing should succeed");

    // Source preserved from original message
    assert_eq!(
        response.source, "pm-agent",
        "response source should be preserved from original message"
    );

    // message_id contains the original message_id
    assert!(
        response.message_id.contains("task-migrate-db-001"),
        "response message_id should reference original: got {}",
        response.message_id
    );

    // routing.path is non-empty
    assert!(
        !response.routing.path.is_empty(),
        "routing.path should be non-empty in response"
    );

    // Content is Text with "Processed"
    match &response.payload.content {
        UnifiedContent::Text { content, .. } => {
            assert!(
                content.contains("Processed"),
                "text handler should prefix with 'Processed', got: {content}"
            );
        }
        other => panic!("expected Text content in response, got: {other:?}"),
    }
}

// ===========================================================================
// Test 2: A2A Mixed Batch — Text and Data Messages
//
// Create BatchProcessor with max_concurrent(5). Build 5 messages:
// 3 text + 2 data (Command type). Process batch. Assert:
//   - total = 5, successful = 5
//   - all individual results have success = true
// ===========================================================================

#[tokio::test]
async fn test_a2a_mixed_batch_text_and_data() {
    let router = Arc::new(MessageRouter::with_defaults());
    let processor = BatchProcessor::new(router).with_max_concurrent(5);

    // 3 text messages (Direct type — handled by TextContentHandler)
    let text_messages: Vec<ConvergedMessage> = (0..3)
        .map(|i| {
            let mut msg = ConvergedMessage::text(
                format!("text-batch-{}", i),
                "text-source".to_string(),
                format!("Text message number {}", i),
            );
            msg.envelope.message_type = ConvergedMessageType::Direct;
            msg
        })
        .collect();

    // 2 data messages (Command type — handled by DataContentHandler)
    let data_messages: Vec<ConvergedMessage> = (0..2)
        .map(|i| make_data_message(&format!("data-batch-{}", i), "data-source"))
        .collect();

    let mut all_messages = text_messages;
    all_messages.extend(data_messages);

    assert_eq!(all_messages.len(), 5, "should have 5 messages total");

    let batch_result = processor.process_batch(all_messages).await;

    assert_eq!(
        batch_result.total, 5,
        "should have processed 5 messages total"
    );
    assert_eq!(batch_result.successful, 5, "all 5 messages should succeed");
    assert_eq!(batch_result.failed, 0, "no messages should fail");

    // Verify all individual results
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
// Test 3: A2A Multi-Step Workflow Lifecycle
//
// Use YawlStateMapper + YawlEventPublisher. Simulate full workflow:
// NotStarted -> Ready -> Executing -> Suspended -> Executing -> Completed.
// At each step: assert is_valid_transition, publish event, assert Event type.
// Then assert terminal states reject transitions.
// ===========================================================================

#[tokio::test]
async fn test_a2a_multi_step_workflow_lifecycle() {
    let publisher = YawlEventPublisher::new();
    let workflow_id = "wf-lifecycle-001";
    let task_id = "task-lifecycle-001";

    // Define the workflow transitions
    let transitions = [
        ("NotStarted", "Ready"),
        ("Ready", "Executing"),
        ("Executing", "Suspended"),
        ("Suspended", "Executing"),
        ("Executing", "Completed"),
    ];

    for (old_state, new_state) in &transitions {
        // Assert transition is valid
        assert!(
            YawlStateMapper::is_valid_transition(old_state, new_state),
            "transition {old_state} -> {new_state} should be valid"
        );

        // Publish event
        let event_msg = publisher
            .publish_task_event(workflow_id, task_id, old_state, new_state)
            .await
            .expect(&format!(
                "publish_task_event({old_state} -> {new_state}) should succeed"
            ));

        // Assert event type is Event
        assert_eq!(
            event_msg.envelope.message_type,
            ConvergedMessageType::Event,
            "event message type should be Event for {old_state} -> {new_state}"
        );

        // Assert the payload contains the state transition
        match &event_msg.payload.content {
            UnifiedContent::Data { data, schema } => {
                assert_eq!(
                    schema.as_deref(),
                    Some("YawlTaskEvent"),
                    "schema should be YawlTaskEvent"
                );
                assert_eq!(
                    data.get("oldState").and_then(|v| v.as_str()),
                    Some(*old_state),
                    "oldState should match"
                );
                assert_eq!(
                    data.get("newState").and_then(|v| v.as_str()),
                    Some(*new_state),
                    "newState should match"
                );
            }
            other => panic!("expected Data content in event, got: {other:?}"),
        }
    }

    // Assert terminal states reject transitions
    let terminal_states = ["Completed", "Failed", "Cancelled"];
    for terminal in &terminal_states {
        assert!(
            YawlStateMapper::is_terminal(terminal),
            "{terminal} should be a terminal state"
        );
        assert!(
            !YawlStateMapper::is_valid_transition(terminal, "Ready"),
            "{terminal} -> Ready should be INVALID (terminal state)"
        );
        assert!(
            !YawlStateMapper::is_valid_transition(terminal, "Executing"),
            "{terminal} -> Executing should be INVALID (terminal state)"
        );
        assert!(
            !YawlStateMapper::is_valid_transition(terminal, "NotStarted"),
            "{terminal} -> NotStarted should be INVALID (terminal state)"
        );
    }
}

// ===========================================================================
// Test 4: A2A Correlation Context Across Turns
//
// Create 3 messages from different sources with the same correlation_id
// and a growing causation_chain. Route each. Assert:
//   - 3 responses
//   - each preserves source
//   - each message_id references original
// ===========================================================================

#[tokio::test]
async fn test_a2a_correlation_context_across_turns() {
    let router = MessageRouter::with_defaults();
    let correlation_id = "cross-agent-correlation-99";

    let sources = ["pm-agent", "architect-agent", "dev-agent"];
    let mut messages: Vec<ConvergedMessage> = Vec::new();
    let mut chain: Vec<String> = Vec::new();

    for (i, source) in sources.iter().enumerate() {
        chain.push(format!("msg-turn-{}", i));
        let mut msg = make_text_message(
            &format!("turn-{}", i),
            source,
            &format!("Turn {} contribution from {}", i, source),
            Some(correlation_id),
        );
        msg.envelope.causation_chain = Some(chain.clone());
        messages.push(msg);
    }

    let mut responses: Vec<ConvergedMessage> = Vec::new();
    for msg in messages {
        let resp = router.route(msg).await.expect("routing should succeed");
        responses.push(resp);
    }

    // 3 responses
    assert_eq!(responses.len(), 3, "should have 3 responses");

    // Each preserves source
    for (i, resp) in responses.iter().enumerate() {
        assert_eq!(
            resp.source, sources[i],
            "response {} should preserve source '{}'",
            i, sources[i]
        );
    }

    // Each message_id references original
    for (i, resp) in responses.iter().enumerate() {
        assert!(
            resp.message_id.contains(&format!("turn-{}", i)),
            "response {} should reference original message id 'turn-{}', got: {}",
            i,
            i,
            resp.message_id
        );
    }
}

// ===========================================================================
// Test 5: Groq A2A Architect Substantial Output (requires GROQ_API_KEY)
//
// A2aLlmClient with Groq model. Substantial architect prompt (database
// migration strategy, 7 items). Assert:
//   - process_message returns Ok
//   - Text content
//   - estimated tokens >= 500
//   - elapsed <= 15000ms
// ===========================================================================

#[tokio::test]
async fn test_groq_a2a_architect_substantial_output() {
    init_tracing();

    // Early return if GROQ_API_KEY not set
    if std::env::var("GROQ_API_KEY").is_err() {
        eprintln!("skipping test_groq_a2a_architect_substantial_output: GROQ_API_KEY not set");
        return;
    }

    use ggen_a2a_mcp::client::A2aLlmClient;
    use ggen_ai::dspy::model_capabilities::Model;

    let model = Model::from_name("groq::openai/gpt-oss-20b");
    let client = A2aLlmClient::new(model)
        .await
        .expect("A2aLlmClient creation should succeed");

    let prompt = r#"You are a database architect agent. Design a comprehensive PostgreSQL to CockroachDB migration strategy for a multi-tenant SaaS platform. Address ALL 7 items:

1. Schema compatibility analysis — which PostgreSQL features need changes (enums, sequences, custom types, partial indexes, exclusion constraints)
2. Data migration approach — online vs offline, chunking strategy, zero-downtime cutover using dual-write pattern
3. Connection pooling architecture — PgBouncer replacement with CockroachDB-compatible pooler, connection limits per tenant
4. Query compatibility audit — identify queries using PostgreSQL-specific syntax (CTEs with data-modifying statements, LATERAL joins, window functions with DISTINCT)
5. Transaction model changes — adapt from READ COMMITTED to SERIALIZABLE isolation, handle retry logic for serialization conflicts
6. Monitoring and observability — key metrics to track during and after migration (query latency P99, transaction retry rate, cluster health)
7. Rollback strategy — how to revert if migration fails at each stage (schema, data, application, DNS)

Output a detailed technical document with specific SQL examples where applicable."#;

    let message = ConvergedMessage::text(
        "architect-migration-001".to_string(),
        "migration-team".to_string(),
        prompt.to_string(),
    );

    let start = Instant::now();
    let response = client
        .process_message(&message)
        .await
        .expect("process_message should return Ok");
    let elapsed = start.elapsed();

    // Assert Text content
    let text = match &response.payload.content {
        UnifiedContent::Text { content, .. } => content.as_str(),
        other => panic!("expected Text content in Groq response, got: {other:?}"),
    };

    let tokens = estimate_tokens(text);
    assert!(
        tokens >= 500,
        "architect output too short — ~{} estimated tokens, minimum 500. First 200 chars: {}",
        tokens,
        &text[..text.len().min(200)]
    );

    assert!(
        elapsed.as_millis() <= 15000,
        "architect response took {} ms, SLO is 15000 ms",
        elapsed.as_millis()
    );

    println!(
        "  architect output: ~{} estimated tokens in {} ms",
        tokens,
        elapsed.as_millis()
    );

    client.shutdown().await.ok();
}

// ===========================================================================
// Test 6: Groq Streaming Through A2A Pipeline (requires GROQ_API_KEY)
//
// A2aLlmClient with Groq. Code generation prompt (connection pool).
// Use stream_response with StreamExt::next(). Assert:
//   - TTFB <= 2000ms
//   - >= 3 chunks
//   - >= 200 tokens
// ===========================================================================

#[tokio::test]
async fn test_groq_streaming_through_a2a_pipeline() {
    init_tracing();

    // Early return if GROQ_API_KEY not set
    if std::env::var("GROQ_API_KEY").is_err() {
        eprintln!("skipping test_groq_streaming_through_a2a_pipeline: GROQ_API_KEY not set");
        return;
    }

    use ggen_a2a_mcp::client::A2aLlmClient;
    use ggen_ai::dspy::model_capabilities::Model;

    let model = Model::from_name("groq::openai/gpt-oss-20b");
    let client = A2aLlmClient::new(model)
        .await
        .expect("A2aLlmClient creation should succeed");

    let prompt = r#"Generate a complete Rust implementation of a generic connection pool with these requirements:
- Generic over Connection type (C: Clone + Send + Sync)
- Configurable min/max connections, idle timeout, max lifetime
- Async get() returns PooledConnection wrapper with Drop that returns to pool
- Background task that evicts idle connections and checks max lifetime
- Health check before returning a connection (ping/validate)
- Builder pattern for construction
- Include comprehensive inline documentation
- Output ONLY valid Rust code, no markdown, no explanation"#;

    let start = Instant::now();
    let mut stream = client
        .stream_response(prompt)
        .await
        .expect("stream_response should succeed");

    let mut full_content = String::new();
    let mut ttfb_measured = false;
    let mut chunk_count = 0;

    while let Some(chunk) = stream.next().await {
        if !chunk.content.is_empty() {
            if !ttfb_measured {
                ttfb_measured = true;
                let ttfb = start.elapsed();
                assert!(
                    ttfb.as_millis() <= 2000,
                    "TTFB was {} ms, SLO is 2000 ms",
                    ttfb.as_millis()
                );
                println!("  streaming TTFB: {} ms", ttfb.as_millis());
            }
            full_content.push_str(&chunk.content);
            chunk_count += 1;
        }
    }

    assert!(ttfb_measured, "should have received at least one chunk");

    let tokens = estimate_tokens(&full_content);
    assert!(
        chunk_count >= 3,
        "expected >= 3 streaming chunks, got {chunk_count} — pipeline may be buffering"
    );
    assert!(
        tokens >= 200,
        "streaming output too short — ~{tokens} estimated tokens, minimum 200"
    );

    println!(
        "  streaming: {} chunks, ~{} estimated tokens",
        chunk_count, tokens
    );

    client.shutdown().await.ok();
}

// ===========================================================================
// Test 7: A2A Task Mapper Bidirectional Composite (no API key)
//
// Create 3 YawlTasks: Atomic with input_data, Composite with split/join (And),
// MultipleInstance with parent_id. YAWL -> A2A -> YAWL round-trip.
// Assert all fields preserved.
// ===========================================================================

#[test]
fn test_a2a_task_mapper_bidirectional_composite() {
    let mapper = TaskMapper::new();

    // Task 1: Atomic with input_data
    let atomic_task = YawlTask {
        id: "atomic-001".to_string(),
        name: "Validate Input".to_string(),
        task_type: YawlTaskType::Atomic,
        split_type: None,
        join_type: None,
        input_data: Some(serde_json::json!({
            "schema": "user-registration",
            "fields": ["email", "name", "age"]
        })),
        workflow_id: Some("wf-order-processing".to_string()),
        parent_id: None,
    };

    // Task 2: Composite with split/join (And)
    let composite_task = YawlTask {
        id: "composite-001".to_string(),
        name: "Fulfill Order".to_string(),
        task_type: YawlTaskType::Composite,
        split_type: Some(YawlSplitType::And),
        join_type: Some(YawlJoinType::And),
        input_data: Some(serde_json::json!({
            "order_id": "ORD-9942",
            "items": ["widget-a", "widget-b"]
        })),
        workflow_id: Some("wf-order-processing".to_string()),
        parent_id: None,
    };

    // Task 3: MultipleInstance with parent_id
    let mi_task = YawlTask {
        id: "mi-notify-customers".to_string(),
        name: "Send Notifications".to_string(),
        task_type: YawlTaskType::MultipleInstance,
        split_type: None,
        join_type: None,
        input_data: None,
        workflow_id: Some("wf-order-processing".to_string()),
        parent_id: Some("composite-001".to_string()),
    };

    let original_tasks = vec![atomic_task, composite_task, mi_task];

    // YAWL -> A2A -> YAWL round-trip for each task
    for (i, original) in original_tasks.iter().enumerate() {
        let a2a_msg = mapper
            .yawl_to_a2a_task(original)
            .expect(&format!("yawl_to_a2a_task should succeed for task {}", i));

        let roundtrip = TaskMapper::a2a_to_yawl_task(&a2a_msg)
            .expect(&format!("a2a_to_yawl_task should succeed for task {}", i));

        // Assert all fields preserved
        assert_eq!(
            roundtrip.id, original.id,
            "task {}: id should be preserved",
            i
        );
        assert_eq!(
            roundtrip.name, original.name,
            "task {}: name should be preserved",
            i
        );
        assert_eq!(
            roundtrip.task_type, original.task_type,
            "task {}: task_type should be preserved",
            i
        );
        assert_eq!(
            roundtrip.split_type, original.split_type,
            "task {}: split_type should be preserved",
            i
        );
        assert_eq!(
            roundtrip.join_type, original.join_type,
            "task {}: join_type should be preserved",
            i
        );
        assert_eq!(
            roundtrip.workflow_id, original.workflow_id,
            "task {}: workflow_id should be preserved",
            i
        );
        assert_eq!(
            roundtrip.parent_id, original.parent_id,
            "task {}: parent_id should be preserved",
            i
        );

        // input_data JSON equality (both Some)
        match (&roundtrip.input_data, &original.input_data) {
            (Some(rt), Some(orig)) => {
                assert_eq!(rt, orig, "task {}: input_data should be preserved", i);
            }
            (None, None) => {} // both None — OK
            (rt, orig) => {
                panic!(
                    "task {}: input_data mismatch — roundtrip: {:?}, original: {:?}",
                    i, rt, orig
                );
            }
        }
    }
}

// ===========================================================================
// Test 8: Groq A2A Error Recovery (requires GROQ_API_KEY)
//
// A2aLlmClient with Groq. Send empty content message -> assert Ok (no panic).
// Send 15000-char content -> assert Ok. Use A2aMessageConverter with Data
// content -> assert a2a_to_llm_request succeeds.
// ===========================================================================

#[tokio::test]
async fn test_groq_a2a_error_recovery() {
    init_tracing();

    // Early return if GROQ_API_KEY not set
    if std::env::var("GROQ_API_KEY").is_err() {
        eprintln!("skipping test_groq_a2a_error_recovery: GROQ_API_KEY not set");
        return;
    }

    use ggen_a2a_mcp::client::A2aLlmClient;
    use ggen_ai::dspy::model_capabilities::Model;

    let model = Model::from_name("groq::openai/gpt-oss-20b");
    let client = A2aLlmClient::new(model)
        .await
        .expect("A2aLlmClient creation should succeed");

    // --- Sub-test A: Empty content message should not panic ---
    let empty_msg = ConvergedMessage::text(
        "error-recovery-empty".to_string(),
        "stress-test".to_string(),
        "".to_string(),
    );

    let result_empty = client.process_message(&empty_msg).await;
    assert!(
        result_empty.is_ok(),
        "empty content message should return Ok, got error: {:?}",
        result_empty.err()
    );
    println!("  empty content: Ok (no panic)");

    // --- Sub-test B: 15000-char content should succeed ---
    let long_content: String = "A".repeat(15000);
    let long_msg = ConvergedMessage::text(
        "error-recovery-long".to_string(),
        "stress-test".to_string(),
        long_content,
    );

    let result_long = client.process_message(&long_msg).await;
    assert!(
        result_long.is_ok(),
        "15000-char content message should return Ok, got error: {:?}",
        result_long.err()
    );
    println!("  15000-char content: Ok");

    // --- Sub-test C: A2aMessageConverter with Data content ---
    let converter = A2aMessageConverter::new();

    let mut data = serde_json::Map::new();
    data.insert(
        "command".to_string(),
        serde_json::Value::String("analyze".to_string()),
    );
    data.insert("depth".to_string(), serde_json::Value::Number(3.into()));
    data.insert(
        "targets".to_string(),
        serde_json::json!(["module-a", "module-b", "module-c"]),
    );

    let data_message = ConvergedMessage {
        message_id: "converter-data-test".to_string(),
        source: "data-agent".to_string(),
        target: Some("llm-bridge".to_string()),
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
                schema: Some("analysis-request".to_string()),
            },
            context: None,
            hints: None,
            integrity: None,
        },
        routing: test_routing(),
        lifecycle: test_lifecycle(),
        extensions: None,
    };

    let llm_request = converter
        .a2a_to_llm_request(&data_message)
        .expect("a2a_to_llm_request with Data content should succeed");

    assert!(
        llm_request.user_content.contains("command"),
        "converted Data content should contain 'command' key"
    );
    assert!(
        llm_request.user_content.contains("analyze"),
        "converted Data content should contain 'analyze' value"
    );
    println!("  Data content conversion: Ok");

    client.shutdown().await.ok();
}
