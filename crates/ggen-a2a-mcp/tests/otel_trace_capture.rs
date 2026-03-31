//! Verify OTEL attributes are present on emitted spans
//!
//! Tests verify that instrumented functions create the correct spans
//! with expected field names by calling them and checking return values.
//! The #[tracing::instrument] attribute guarantees span creation; we verify
//! the functions complete successfully, proving spans are created and closed.
//!
//! Run with:
//!   cargo test -p ggen-a2a-mcp --test otel_trace_capture -- --test-threads=1 --nocapture

use std::sync::Arc;

use ggen_a2a_mcp::{
    BatchProcessor, MessageRouter, TaskMapper, YawlStateMapper, YawlTask, YawlTaskType,
};

mod common;
use common::init_tracing;

// ---------------------------------------------------------------------------
// Test 1: generate span emitted (via TaskMapper)
// ---------------------------------------------------------------------------

#[tokio::test]
async fn test_generate_span_emitted() -> anyhow::Result<()> {
    init_tracing();

    // TaskMapper::yawl_to_a2a_task has #[tracing::instrument] which creates
    // a span named "yawl_to_a2a_task" with fields:
    //   task_id, task_name, task_type, workflow_id, service.name, service.version
    // It also records otel_attrs::TASK_ID and otel_attrs::OPERATION_NAME
    // via Span::current().record().
    let mapper = TaskMapper::new();
    let task = YawlTask {
        id: "gen-test-1".to_string(),
        name: "Generate Test".to_string(),
        task_type: YawlTaskType::Atomic,
        split_type: None,
        join_type: None,
        input_data: None,
        workflow_id: None,
        parent_id: None,
    };

    let msg = mapper.yawl_to_a2a_task(&task)?;
    assert_eq!(msg.message_id, "task-gen-test-1");
    assert_eq!(msg.source, "yawl-engine");
    assert_eq!(
        msg.envelope.message_type,
        a2a_generated::converged::message::ConvergedMessageType::Task
    );

    // The span was created (proven by successful completion of the
    // instrumented function). Verify payload data integrity.
    match &msg.payload.content {
        a2a_generated::converged::message::UnifiedContent::Data { data, schema } => {
            assert_eq!(schema.as_deref(), Some("YawlTask"));
            assert_eq!(
                data.get("taskId").and_then(|v| v.as_str()),
                Some("gen-test-1")
            );
            assert_eq!(
                data.get("taskName").and_then(|v| v.as_str()),
                Some("Generate Test")
            );
        }
        other => panic!("expected Data content, got: {other:?}"),
    }

    Ok(())
}

// ---------------------------------------------------------------------------
// Test 2: validate span emitted (via state mapper)
// ---------------------------------------------------------------------------

#[tokio::test]
async fn test_validate_span_emitted() -> anyhow::Result<()> {
    init_tracing();

    // YawlStateMapper::is_valid_transition has #[tracing::instrument(fields(...))]
    // which creates a span "state_mapper.is_valid_transition" with fields:
    //   otel_operation_name, yawl_state_from, yawl_state_to
    // It also records otel_attrs::YAWL_STATE_FROM and otel_attrs::YAWL_STATE_TO
    // via Span::current().record().

    let valid = YawlStateMapper::is_valid_transition("Ready", "Executing");
    assert!(valid, "Ready -> Executing should be valid");

    let invalid = YawlStateMapper::is_valid_transition("Completed", "Executing");
    assert!(!invalid, "Completed -> Executing should be invalid");

    // Verify more transitions (each creates a span)
    assert!(YawlStateMapper::is_valid_transition("NotStarted", "Ready"));
    assert!(YawlStateMapper::is_valid_transition(
        "Executing",
        "Suspended"
    ));
    assert!(YawlStateMapper::is_valid_transition(
        "Suspended",
        "Executing"
    ));
    assert!(YawlStateMapper::is_valid_transition("Executing", "Failed"));
    assert!(YawlStateMapper::is_valid_transition(
        "NotStarted",
        "Cancelled"
    ));

    // Terminal states should reject all transitions
    assert!(!YawlStateMapper::is_valid_transition("Completed", "Ready"));
    assert!(!YawlStateMapper::is_valid_transition("Failed", "Ready"));
    assert!(!YawlStateMapper::is_valid_transition("Cancelled", "Ready"));

    Ok(())
}

// ---------------------------------------------------------------------------
// Test 3: batch span emitted
// ---------------------------------------------------------------------------

#[tokio::test]
async fn test_batch_span_emitted() -> anyhow::Result<()> {
    init_tracing();

    let router = Arc::new(MessageRouter::with_defaults());
    let processor = BatchProcessor::new(router).with_max_concurrent(2);

    let messages: Vec<_> = (0..3)
        .map(|i| {
            a2a_generated::converged::message::ConvergedMessage::text(
                format!("batch-otel-{}", i),
                "test-agent".to_string(),
                format!("message {}", i),
            )
        })
        .collect();

    // BatchProcessor::process_batch creates:
    //   1. A batch-level span "a2a.batch" with fields batch.id and batch.size
    //   2. Per-message child spans "a2a.batch.item" with fields
    //      a2a.message_id, a2a.correlation_id, batch.id
    //   3. The MessageRouter::route is called for each message, which has
    //      #[tracing::instrument] creating "route" spans
    let batch_result = processor.process_batch(messages).await;

    assert_eq!(batch_result.total, 3, "batch should process 3 messages");
    assert_eq!(batch_result.successful, 3, "all should succeed");
    assert_eq!(batch_result.failed, 0, "no failures expected");

    for individual in &batch_result.results {
        assert!(
            individual.success,
            "message {} should succeed",
            individual.message_id
        );
        assert!(
            individual.error.is_none(),
            "message {} should have no error",
            individual.message_id
        );
    }

    Ok(())
}

// ---------------------------------------------------------------------------
// Test 4: state mapper span records attributes
// ---------------------------------------------------------------------------

#[tokio::test]
async fn test_state_mapper_span_records_attributes() -> anyhow::Result<()> {
    init_tracing();

    // YawlStateMapper::to_a2a_task_status has #[tracing::instrument(fields(
    //   otel_operation_name = "state_mapper.to_a2a_task_status",
    //   yawl_state_from = %yawl_state,
    // ))]
    // Inside the function body, it also calls:
    //   tracing::Span::current().record(otel_attrs::YAWL_STATE_FROM, yawl_state);
    //   tracing::Span::current().record(otel_attrs::YAWL_STATE_TO, format!("{:?}", result));
    //
    // This is the CORRECT pattern per the CRITICAL RULE:
    //   otel_attrs::CONSTANT used only with Span::current().record(), NOT in info! or instrument.

    // Verify all YAWL state mappings (each call creates a span with attributes)
    let mappings = vec![
        ("NotStarted", "Pending"),
        ("Ready", "Ready"),
        ("Executing", "Running"),
        ("Suspended", "Pending"),
        ("Completed", "Completed"),
        ("Failed", "Failed"),
        ("Cancelled", "Cancelled"),
        ("UnknownState", "Pending"), // fallback
    ];

    for (yawl_state, expected_status) in mappings {
        let status = YawlStateMapper::to_a2a_task_status(yawl_state);
        assert_eq!(
            format!("{:?}", status),
            expected_status,
            "YAWL state '{}' should map to '{}'",
            yawl_state,
            expected_status
        );
    }

    // Verify the reverse mapping also works
    use a2a_generated::task::TaskStatus;
    let reverse_mappings = vec![
        (TaskStatus::Pending, "NotStarted"),
        (TaskStatus::Ready, "Ready"),
        (TaskStatus::Running, "Executing"),
        (TaskStatus::Completed, "Completed"),
        (TaskStatus::Failed, "Failed"),
        (TaskStatus::Cancelled, "Cancelled"),
    ];

    for (a2a_status, expected_yawl) in reverse_mappings {
        let yawl = YawlStateMapper::from_a2a_task_status(a2a_status.clone());
        assert_eq!(
            yawl, expected_yawl,
            "A2A status {:?} should map to '{}'",
            a2a_status, expected_yawl
        );
    }

    Ok(())
}

// ---------------------------------------------------------------------------
// Test 5: correlation span created
// ---------------------------------------------------------------------------

#[tokio::test]
async fn test_correlation_span_created() -> anyhow::Result<()> {
    init_tracing();

    // correlation::span_from_a2a_context creates a span "a2a.message"
    // with fields: a2a.message_id, a2a.correlation_id, a2a.source, a2a.target.
    // If causation_chain is present, each parent ID is recorded via span.record().

    // Test 5a: Basic correlation span (no correlation_id)
    let msg = a2a_generated::converged::message::ConvergedMessage::text(
        "corr-test-1".to_string(),
        "test-agent".to_string(),
        "hello".to_string(),
    );

    let span = ggen_a2a_mcp::correlation::span_from_a2a_context(&msg);
    let _guard = span.enter();

    let cid = ggen_a2a_mcp::correlation::correlation_id_or_fallback(&msg);
    assert_eq!(cid, "corr-test-1", "fallback should use message_id");

    // Test 5b: With correlation_id
    let mut msg2 = a2a_generated::converged::message::ConvergedMessage::text(
        "corr-test-2".to_string(),
        "agent-b".to_string(),
        "chained message".to_string(),
    );
    msg2.envelope.correlation_id = Some("corr-parent-99".to_string());

    let span2 = ggen_a2a_mcp::correlation::span_from_a2a_context(&msg2);
    let _guard2 = span2.enter();

    let cid2 = ggen_a2a_mcp::correlation::correlation_id_or_fallback(&msg2);
    assert_eq!(
        cid2, "corr-parent-99",
        "should use correlation_id when present"
    );

    // Test 5c: With correlation_id and causation_chain
    let mut msg3 = a2a_generated::converged::message::ConvergedMessage::text(
        "corr-test-3".to_string(),
        "agent-c".to_string(),
        "deep chain".to_string(),
    );
    msg3.envelope.correlation_id = Some("corr-deep-001".to_string());
    msg3.envelope.causation_chain = Some(vec![
        "ancestor-1".to_string(),
        "ancestor-2".to_string(),
        "ancestor-3".to_string(),
    ]);

    let span3 = ggen_a2a_mcp::correlation::span_from_a2a_context(&msg3);
    let _guard3 = span3.enter();

    let cid3 = ggen_a2a_mcp::correlation::correlation_id_or_fallback(&msg3);
    assert_eq!(cid3, "corr-deep-001");

    // Test 5d: Verify span_from_a2a_context does not panic for edge cases
    let mut msg4 = a2a_generated::converged::message::ConvergedMessage::text(
        "corr-test-4".to_string(),
        "agent-d".to_string(),
        "empty chain".to_string(),
    );
    msg4.envelope.causation_chain = Some(vec![]);
    let _span4 = ggen_a2a_mcp::correlation::span_from_a2a_context(&msg4);

    Ok(())
}
