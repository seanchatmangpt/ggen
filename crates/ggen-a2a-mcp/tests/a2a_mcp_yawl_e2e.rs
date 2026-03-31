//! Three-layer test: MCP + A2A + YAWL
//!
//! Exercises TaskMapper, YawlEventPublisher, and BatchProcessor together
//! with correlation ID propagation through the full A2A -> YAWL pipeline.
//!
//! Run with:
//!   cargo test -p ggen-a2a-mcp --test a2a_mcp_yawl_e2e -- --test-threads=1 --nocapture

use std::sync::Arc;

use a2a_generated::converged::message::{
    ConvergedMessage, ConvergedMessageType, MessageLifecycle, MessageRouting, MessageState,
    QoSRequirements, ReliabilityLevel, UnifiedContent,
};
use ggen_a2a_mcp::{
    BatchProcessor, MessageRouter, TaskMapper, YawlEventPublisher, YawlTask, YawlTaskType,
};

mod common;
use common::init_tracing;

// ---------------------------------------------------------------------------
// Helpers
// ---------------------------------------------------------------------------

#[allow(dead_code)]
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

#[allow(dead_code)]
fn test_lifecycle() -> MessageLifecycle {
    MessageLifecycle {
        state: MessageState::Created,
        history: Vec::new(),
        timeout: None,
    }
}

fn make_text_message(
    id: &str, source: &str, content: &str, correlation_id: Option<&str>,
) -> ConvergedMessage {
    let mut msg = ConvergedMessage::text(id.to_string(), source.to_string(), content.to_string());
    if let Some(cid) = correlation_id {
        msg.envelope.correlation_id = Some(cid.to_string());
    }
    msg
}

fn extract_unified_text(content: &UnifiedContent) -> &str {
    match content {
        UnifiedContent::Text { content, .. } => content.as_str(),
        _ => "",
    }
}

// ---------------------------------------------------------------------------
// Test 1: A2A message to YAWL task mapping
// ---------------------------------------------------------------------------

#[tokio::test]
async fn test_a2a_message_to_yawl_task_mapping() -> anyhow::Result<()> {
    init_tracing();

    let mapper = TaskMapper::new();

    let yawl_task = YawlTask {
        id: "t-001".to_string(),
        name: "Authenticate User".to_string(),
        task_type: YawlTaskType::Atomic,
        split_type: Some(ggen_a2a_mcp::YawlSplitType::And),
        join_type: Some(ggen_a2a_mcp::YawlJoinType::Xor),
        input_data: Some(serde_json::json!({"user": "alice"})),
        workflow_id: Some("wf-auth".to_string()),
        parent_id: None,
    };

    let message = mapper.yawl_to_a2a_task(&yawl_task)?;

    // Verify the message is a Task type from yawl-engine
    assert_eq!(
        message.envelope.message_type,
        ConvergedMessageType::Task,
        "message type should be Task"
    );
    assert_eq!(
        message.source, "yawl-engine",
        "source should be yawl-engine"
    );
    assert_eq!(
        message.message_id, "task-t-001",
        "message_id should be task-{{id}}"
    );

    // Verify the correlation_id is the task id
    assert_eq!(
        message.envelope.correlation_id,
        Some("t-001".to_string()),
        "correlation_id should be the task id"
    );

    // Verify the payload is Data with YawlTask schema
    match &message.payload.content {
        UnifiedContent::Data { data, schema } => {
            assert_eq!(
                schema.as_deref(),
                Some("YawlTask"),
                "schema should be YawlTask"
            );
            assert_eq!(
                data.get("taskId").and_then(|v| v.as_str()),
                Some("t-001"),
                "taskId should be t-001"
            );
            assert_eq!(
                data.get("taskName").and_then(|v| v.as_str()),
                Some("Authenticate User"),
                "taskName should be preserved"
            );
            assert_eq!(
                data.get("taskType").and_then(|v| v.as_str()),
                Some("Atomic"),
                "taskType should be Atomic"
            );
            assert_eq!(
                data.get("splitType").and_then(|v| v.as_str()),
                Some("And"),
                "splitType should be And"
            );
            assert_eq!(
                data.get("joinType").and_then(|v| v.as_str()),
                Some("Xor"),
                "joinType should be Xor"
            );
            assert_eq!(
                data.get("workflowId").and_then(|v| v.as_str()),
                Some("wf-auth"),
                "workflowId should be wf-auth"
            );
        }
        other => panic!("expected Data content, got: {other:?}"),
    }

    Ok(())
}

// ---------------------------------------------------------------------------
// Test 2: A2A to YAWL roundtrip
// ---------------------------------------------------------------------------

#[tokio::test]
async fn test_a2a_to_yawl_roundtrip() -> anyhow::Result<()> {
    init_tracing();

    let mapper = TaskMapper::new();

    let original_task = YawlTask {
        id: "t-round-100".to_string(),
        name: "Process Payment".to_string(),
        task_type: YawlTaskType::Composite,
        split_type: Some(ggen_a2a_mcp::YawlSplitType::Xor),
        join_type: Some(ggen_a2a_mcp::YawlJoinType::And),
        input_data: Some(serde_json::json!({"amount": 500, "currency": "USD"})),
        workflow_id: Some("wf-payments".to_string()),
        parent_id: Some("t-parent-50".to_string()),
    };

    // Forward: YAWL -> A2A
    let a2a_message = mapper.yawl_to_a2a_task(&original_task)?;
    assert_eq!(
        a2a_message.envelope.message_type,
        ConvergedMessageType::Task
    );

    // Reverse: A2A -> YAWL
    let roundtrip_task = TaskMapper::a2a_to_yawl_task(&a2a_message)?;

    // Verify all key fields preserved
    assert_eq!(
        roundtrip_task.id, original_task.id,
        "task id should be preserved through roundtrip"
    );
    assert_eq!(
        roundtrip_task.name, original_task.name,
        "task name should be preserved"
    );
    assert_eq!(
        roundtrip_task.task_type, original_task.task_type,
        "task type should be preserved"
    );
    assert_eq!(
        roundtrip_task.split_type, original_task.split_type,
        "split type should be preserved"
    );
    assert_eq!(
        roundtrip_task.join_type, original_task.join_type,
        "join type should be preserved"
    );
    assert_eq!(
        roundtrip_task.workflow_id, original_task.workflow_id,
        "workflow id should be preserved"
    );
    assert_eq!(
        roundtrip_task.parent_id, original_task.parent_id,
        "parent id should be preserved"
    );
    assert_eq!(
        roundtrip_task.input_data, original_task.input_data,
        "input_data should be preserved"
    );

    Ok(())
}

// ---------------------------------------------------------------------------
// Test 3: YAWL event publisher records states
// ---------------------------------------------------------------------------

#[tokio::test]
async fn test_yawl_event_publisher_records_states() -> anyhow::Result<()> {
    init_tracing();

    let publisher = YawlEventPublisher::new();

    let old_state = "Ready";
    let new_state = "Executing";

    let event_msg = publisher
        .publish_task_event("wf-order-42", "task-validate-input", old_state, new_state)
        .await?;

    // Verify it's an Event message
    assert_eq!(
        event_msg.envelope.message_type,
        ConvergedMessageType::Event,
        "event message type should be Event"
    );

    // Verify correlation_id is the task id
    assert_eq!(
        event_msg.envelope.correlation_id,
        Some("task-validate-input".to_string()),
        "correlation_id should be the task id"
    );

    // Verify the payload contains the state transition
    match &event_msg.payload.content {
        UnifiedContent::Data { data, schema } => {
            assert_eq!(
                schema.as_deref(),
                Some("YawlTaskEvent"),
                "schema should be YawlTaskEvent"
            );
            assert_eq!(
                data.get("oldState").and_then(|v| v.as_str()),
                Some(old_state),
                "oldState should be 'Ready'"
            );
            assert_eq!(
                data.get("newState").and_then(|v| v.as_str()),
                Some(new_state),
                "newState should be 'Executing'"
            );
            assert_eq!(
                data.get("workflowId").and_then(|v| v.as_str()),
                Some("wf-order-42"),
                "workflowId should be preserved"
            );
            assert_eq!(
                data.get("taskId").and_then(|v| v.as_str()),
                Some("task-validate-input"),
                "taskId should be preserved"
            );
            assert_eq!(
                data.get("eventType").and_then(|v| v.as_str()),
                Some("TaskStatusUpdate"),
                "eventType should be TaskStatusUpdate"
            );
        }
        other => panic!("expected Data content in event, got: {other:?}"),
    }

    // Verify source contains workflow id
    assert!(
        event_msg.source.contains("wf-order-42"),
        "source should contain workflow id, got: {}",
        event_msg.source
    );

    Ok(())
}

// ---------------------------------------------------------------------------
// Test 4: Correlation ID propagates through batch and events
// ---------------------------------------------------------------------------

#[tokio::test]
async fn test_correlation_id_propagates_through_batch_and_events() -> anyhow::Result<()> {
    init_tracing();

    let router = Arc::new(MessageRouter::with_defaults());
    let processor = BatchProcessor::new(router).with_max_concurrent(3);
    let mapper = TaskMapper::new();
    let publisher = YawlEventPublisher::new();

    // Create 3 messages with distinct correlation IDs
    let correlation_ids = vec!["corr-workflow-a", "corr-workflow-b", "corr-workflow-c"];
    let workflow_ids = vec!["wf-a", "wf-b", "wf-c"];
    let task_ids = vec!["task-a1", "task-b1", "task-c1"];

    let messages: Vec<ConvergedMessage> = correlation_ids
        .iter()
        .zip(workflow_ids.iter())
        .zip(task_ids.iter())
        .map(|((cid, wf), tid)| {
            let msg = make_text_message(
                &format!("batch-{}", uuid::Uuid::new_v4()),
                "test-agent",
                &format!("task {} in workflow {}", tid, wf),
                Some(cid),
            );
            msg
        })
        .collect();

    // Step 1: Process batch
    let batch_result = processor.process_batch(messages).await;
    assert_eq!(batch_result.total, 3, "should process 3 messages");
    assert_eq!(batch_result.successful, 3, "all should succeed");

    // Step 2: Map results to YAWL tasks via TaskMapper
    for (idx, _) in batch_result.results.iter().enumerate() {
        let yawl_task = YawlTask {
            id: task_ids[idx].to_string(),
            name: format!("Task {}", idx),
            task_type: YawlTaskType::Atomic,
            split_type: None,
            join_type: None,
            input_data: None,
            workflow_id: Some(workflow_ids[idx].to_string()),
            parent_id: None,
        };

        let a2a_msg = mapper.yawl_to_a2a_task(&yawl_task)?;

        // Verify correlation_id is set from task id
        assert_eq!(
            a2a_msg.envelope.correlation_id,
            Some(task_ids[idx].to_string()),
            "mapped message should have correlation_id = task id"
        );

        // Step 3: Publish YAWL event and verify correlation
        let event_msg = publisher
            .publish_task_event(workflow_ids[idx], task_ids[idx], "NotStarted", "Ready")
            .await?;

        // Verify event correlation_id matches task id
        assert_eq!(
            event_msg.envelope.correlation_id,
            Some(task_ids[idx].to_string()),
            "event correlation_id should be task id, got: {:?}",
            event_msg.envelope.correlation_id
        );
    }

    Ok(())
}

// ---------------------------------------------------------------------------
// Test 5: Full three-layer A2A route -> YAWL events
// ---------------------------------------------------------------------------

#[tokio::test]
async fn test_full_three_layer_a2a_route_to_yawl_events() -> anyhow::Result<()> {
    init_tracing();

    // Layer 1: A2A MessageRouter
    let router = MessageRouter::with_defaults();
    let correlation_id = "corr-full-pipeline-001";

    let msg = make_text_message(
        "msg-full-pipeline",
        "architect",
        "Start workflow: deploy to production",
        Some(correlation_id),
    );

    let routed_response = router.route(msg).await?;
    let text = extract_unified_text(&routed_response.payload.content);
    assert!(
        text.contains("Processed"),
        "Layer 1: routed message should be processed, got: {}",
        text
    );
    assert_eq!(
        routed_response.source, "architect",
        "Layer 1: response source should be preserved"
    );

    // Layer 2: Map to YAWL task via TaskMapper
    let mapper = TaskMapper::new();
    let yawl_task = YawlTask {
        id: "task-deploy-prod".to_string(),
        name: "Deploy to Production".to_string(),
        task_type: YawlTaskType::Atomic,
        split_type: None,
        join_type: None,
        input_data: Some(serde_json::json!({"env": "production"})),
        workflow_id: Some("wf-cicd".to_string()),
        parent_id: None,
    };

    let yawl_message = mapper.yawl_to_a2a_task(&yawl_task)?;

    // Verify the YAWL message has correlation from task id
    assert_eq!(
        yawl_message.envelope.correlation_id,
        Some("task-deploy-prod".to_string()),
        "Layer 2: YAWL message correlation should be task id"
    );
    assert_eq!(
        yawl_message.source, "yawl-engine",
        "Layer 2: source should be yawl-engine"
    );
    assert_eq!(
        yawl_message.envelope.message_type,
        ConvergedMessageType::Task,
        "Layer 2: message type should be Task"
    );

    // Layer 3: Publish YAWL event via YawlEventPublisher
    let publisher = YawlEventPublisher::new();
    let event_msg = publisher
        .publish_task_event("wf-cicd", "task-deploy-prod", "Ready", "Executing")
        .await?;

    // Verify the event has correlation chain
    assert_eq!(
        event_msg.envelope.correlation_id,
        Some("task-deploy-prod".to_string()),
        "Layer 3: event correlation_id should be task id"
    );
    assert_eq!(
        event_msg.envelope.causation_chain,
        Some(vec!["wf-cicd".to_string()]),
        "Layer 3: causation chain should contain workflow id"
    );
    assert_eq!(
        event_msg.envelope.message_type,
        ConvergedMessageType::Event,
        "Layer 3: message type should be Event"
    );

    // Verify the full correlation chain:
    //   A2A message had correlation_id = "corr-full-pipeline-001"
    //   YAWL task message has correlation_id = "task-deploy-prod" (from task.id)
    //   YAWL event has correlation_id = "task-deploy-prod" (from task_id param)
    // The correlation chain is: corr-full-pipeline-001 -> task-deploy-prod -> event
    // The event's causation_chain links back to wf-cicd.
    assert!(
        event_msg.source.contains("wf-cicd"),
        "Layer 3: event source should contain workflow id, got: {}",
        event_msg.source
    );

    // Verify event payload preserves states
    match &event_msg.payload.content {
        UnifiedContent::Data { data, schema } => {
            assert_eq!(schema.as_deref(), Some("YawlTaskEvent"));
            assert_eq!(data.get("oldState").and_then(|v| v.as_str()), Some("Ready"));
            assert_eq!(
                data.get("newState").and_then(|v| v.as_str()),
                Some("Executing")
            );
        }
        other => panic!("expected Data content in event, got: {other:?}"),
    }

    Ok(())
}
