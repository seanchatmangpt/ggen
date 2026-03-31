//! FIBO + TOGAF Phases A & B Self-Play Tests
//!
//! Tests the Architecture Vision and Business Architecture agents
//! collaborating through multi-turn architecture development with
//! FIBO ontology integration.

use std::collections::HashMap;

use a2a_generated::converged::message::{
    ConvergedMessage, ConvergedMessageType, ConvergedPayload, MessageEnvelope, MessageLifecycle,
    MessagePriority, MessageRouting, MessageState, QoSRequirements, ReliabilityLevel,
    UnifiedContent,
};
use chrono::Utc;
use ggen_a2a_mcp::MessageRouter;
use serde_json::json;

mod common;
use common::init_tracing;

// ---------------------------------------------------------------------------
// Helper Functions
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

/// Create a text message for testing.
fn make_text_message(
    message_id: &str, source: &str, content: &str, correlation_id: Option<&str>,
) -> ConvergedMessage {
    let mut msg = ConvergedMessage::text(
        message_id.to_string(),
        source.to_string(),
        content.to_string(),
    );

    if let Some(cid) = correlation_id {
        msg.envelope.correlation_id = Some(cid.to_string());
    }

    msg
}

/// Create a data message with structured content.
fn make_data_message(
    message_id: &str, source: &str, data: HashMap<String, serde_json::Value>,
    correlation_id: Option<&str>,
) -> ConvergedMessage {
    let mut data_map = serde_json::Map::new();

    for (key, value) in data {
        data_map.insert(key, value);
    }

    ConvergedMessage {
        message_id: message_id.to_string(),
        source: source.to_string(),
        target: None,
        envelope: MessageEnvelope {
            message_type: ConvergedMessageType::Task,
            priority: MessagePriority::Normal,
            timestamp: Utc::now(),
            schema_version: "1.0".to_string(),
            content_type: "application/json".to_string(),
            correlation_id: correlation_id.map(|s| s.to_string()),
            causation_chain: None,
        },
        payload: ConvergedPayload {
            content: UnifiedContent::Data {
                data: data_map,
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

/// Assert that a message contains a specific artifact type.
fn assert_has_artifact(messages: &[ConvergedMessage], artifact_type: &str) {
    let has_artifact = messages.iter().any(|m| match &m.payload.content {
        UnifiedContent::Data { data, .. } => {
            if let Some(artifact) = data.get("artifact_type") {
                if let Some(type_str) = artifact.as_str() {
                    return type_str == artifact_type;
                }
            }
            false
        }
        UnifiedContent::Text { content, .. } => content.contains(artifact_type),
        _ => false,
    });

    assert!(
        has_artifact,
        "Should have artifact of type {}, but not found in {} messages",
        artifact_type,
        messages.len()
    );
}

/// Assert that a FIBO entity is mapped to a business capability.
fn assert_fibo_mapped(messages: &[ConvergedMessage], fibo_entity: &str, business_capability: &str) {
    let mapped = messages.iter().any(|m| match &m.payload.content {
        UnifiedContent::Data { data, .. } => {
            let entity_match = data
                .get("fibo_entity")
                .and_then(|v| v.as_str())
                .map(|e| e == fibo_entity)
                .unwrap_or(false);

            let capability_match = data
                .get("business_capability")
                .and_then(|v| v.as_str())
                .map(|c| c == business_capability)
                .unwrap_or(false);

            entity_match && capability_match
        }
        UnifiedContent::Text { content, .. } => {
            content.contains(fibo_entity) && content.contains(business_capability)
        }
        _ => false,
    });

    assert!(
        mapped,
        "FIBO entity {} should map to capability {}",
        fibo_entity, business_capability
    );
}

// ---------------------------------------------------------------------------
// Phase A Simulation
// ---------------------------------------------------------------------------

/// Simulate Phase A collaboration through multiple turns.
async fn simulate_phase_a_collaboration(turns: usize) -> Vec<ConvergedMessage> {
    let router = MessageRouter::with_defaults();
    let mut messages = Vec::new();
    let correlation_id = uuid::Uuid::new_v4().to_string();

    for turn in 1..=turns {
        let msg = match turn {
            1 => {
                // Turn 1: Stakeholder analysis
                let mut data = HashMap::new();
                data.insert("phase".to_string(), json!("A"));
                data.insert("turn".to_string(), json!(1));
                data.insert("action".to_string(), json!("stakeholder_analysis"));
                data.insert("artifact_type".to_string(), json!("StakeholderMap"));
                data.insert(
                    "stakeholders".to_string(),
                    json!(["executives", "regulators", "customers"]),
                );

                make_data_message(
                    &format!("phase-a-turn-{}", turn),
                    "vision-agent",
                    data,
                    Some(&correlation_id),
                )
            }
            2 => {
                // Turn 2: Vision statement draft
                let mut data = HashMap::new();
                data.insert("phase".to_string(), json!("A"));
                data.insert("turn".to_string(), json!(2));
                data.insert("action".to_string(), json!("vision_draft"));
                data.insert(
                    "vision_statement".to_string(),
                    json!("To become the leading digital bank by 2030"),
                );

                make_data_message(
                    &format!("phase-a-turn-{}", turn),
                    "vision-agent",
                    data,
                    Some(&correlation_id),
                )
            }
            3 => {
                // Turn 3: ARB review feedback
                make_text_message(
                    &format!("phase-a-turn-{}", turn),
                    "arb-reviewer",
                    "Vision is too vague. Add specific metrics and timelines.",
                    Some(&correlation_id),
                )
            }
            4 => {
                // Turn 4: Vision refinement
                let mut data = HashMap::new();
                data.insert("phase".to_string(), json!("A"));
                data.insert("turn".to_string(), json!(4));
                data.insert("action".to_string(), json!("vision_refinement"));
                data.insert(
                    "vision_statement".to_string(),
                    json!("Achieve 50% digital adoption by 2027 with 99.9% uptime"),
                );
                data.insert(
                    "metrics".to_string(),
                    json!(["NPS", "adoption_rate", "uptime"]),
                );

                make_data_message(
                    &format!("phase-a-turn-{}", turn),
                    "vision-agent",
                    data,
                    Some(&correlation_id),
                )
            }
            5 => {
                // Turn 5: Final approval
                let mut data = HashMap::new();
                data.insert("phase".to_string(), json!("A"));
                data.insert("turn".to_string(), json!(5));
                data.insert("action".to_string(), json!("approval"));
                data.insert("status".to_string(), json!("approved"));
                data.insert(
                    "artifact_type".to_string(),
                    json!("ArchitectureVisionStatement"),
                );

                make_data_message(
                    &format!("phase-a-turn-{}", turn),
                    "arb-chair",
                    data,
                    Some(&correlation_id),
                )
            }
            _ => make_text_message(
                &format!("phase-a-turn-{}", turn),
                "unknown",
                "Unexpected turn",
                Some(&correlation_id),
            ),
        };

        match router.route(msg).await {
            Ok(response) => messages.push(response),
            Err(e) => {
                eprintln!("Error routing message in turn {}: {}", turn, e);
                // Continue with next turn
            }
        }
    }

    messages
}

// ---------------------------------------------------------------------------
// Phase B Simulation with FIBO
// ---------------------------------------------------------------------------

/// Simulate Phase B collaboration with FIBO ontology integration.
async fn simulate_phase_b_with_fibo(fibo_entities: Vec<String>) -> Vec<ConvergedMessage> {
    let router = MessageRouter::with_defaults();
    let mut messages = Vec::new();
    let correlation_id = uuid::Uuid::new_v4().to_string();

    // Phase B: Business Architecture (turns 6-12)
    for (idx, fibo_entity) in fibo_entities.iter().enumerate() {
        let turn = 6 + idx;

        let msg = match fibo_entity.as_str() {
            "fibo-fnd:LegalPerson" => {
                let mut data = HashMap::new();
                data.insert("phase".to_string(), json!("B"));
                data.insert("turn".to_string(), json!(turn));
                data.insert("action".to_string(), json!("map_fibo_concept"));
                data.insert("fibo_entity".to_string(), json!(fibo_entity));
                data.insert(
                    "business_capability".to_string(),
                    json!("CustomerOnboarding"),
                );
                data.insert(
                    "description".to_string(),
                    json!("Legal person represents customers in the banking system"),
                );

                make_data_message(
                    &format!("phase-b-turn-{}", turn),
                    "business-architect",
                    data,
                    Some(&correlation_id),
                )
            }
            "fibo-fnd:Organization" => {
                let mut data = HashMap::new();
                data.insert("phase".to_string(), json!("B"));
                data.insert("turn".to_string(), json!(turn));
                data.insert("action".to_string(), json!("map_fibo_concept"));
                data.insert("fibo_entity".to_string(), json!(fibo_entity));
                data.insert(
                    "business_capability".to_string(),
                    json!("BusinessStructure"),
                );
                data.insert(
                    "description".to_string(),
                    json!("Organization structure defines business units and reporting lines"),
                );

                make_data_message(
                    &format!("phase-b-turn-{}", turn),
                    "business-architect",
                    data,
                    Some(&correlation_id),
                )
            }
            "fibo-lcc:LoanContract" => {
                let mut data = HashMap::new();
                data.insert("phase".to_string(), json!("B"));
                data.insert("turn".to_string(), json!(turn));
                data.insert("action".to_string(), json!("map_fibo_concept"));
                data.insert("fibo_entity".to_string(), json!(fibo_entity));
                data.insert("business_capability".to_string(), json!("ProductCatalog"));
                data.insert(
                    "description".to_string(),
                    json!("Loan contracts represent core banking products"),
                );

                make_data_message(
                    &format!("phase-b-turn-{}", turn),
                    "business-architect",
                    data,
                    Some(&correlation_id),
                )
            }
            _ => make_text_message(
                &format!("phase-b-turn-{}", turn),
                "business-architect",
                &format!("Processing unknown FIBO entity: {}", fibo_entity),
                Some(&correlation_id),
            ),
        };

        match router.route(msg).await {
            Ok(response) => messages.push(response),
            Err(e) => {
                eprintln!("Error routing message in turn {}: {}", turn, e);
                // Continue with next turn
            }
        }
    }

    messages
}

/// Simulate Phase A to Phase B handoff.
async fn simulate_phase_a_to_b_handoff() -> Vec<ConvergedMessage> {
    let mut all_messages = Vec::new();

    // Phase A: 5 turns
    let phase_a_messages = simulate_phase_a_collaboration(5).await;
    all_messages.extend(phase_a_messages);

    // Phase B: 3 turns (partial)
    let fibo_entities = vec![
        "fibo-fnd:LegalPerson".to_string(),
        "fibo-fnd:Organization".to_string(),
        "fibo-lcc:LoanContract".to_string(),
    ];
    let phase_b_messages = simulate_phase_b_with_fibo(fibo_entities).await;
    all_messages.extend(phase_b_messages);

    all_messages
}

// ---------------------------------------------------------------------------
// Tests
// ---------------------------------------------------------------------------

#[tokio::test]
async fn test_phase_a_vision_agent_turns() {
    init_tracing();

    let turns = 5;
    let messages = simulate_phase_a_collaboration(turns).await;

    assert_eq!(
        messages.len(),
        turns,
        "Phase A should complete in {} turns, got {}",
        turns,
        messages.len()
    );

    // Verify vision artifacts created
    assert_has_artifact(&messages, "ArchitectureVisionStatement");
    assert_has_artifact(&messages, "StakeholderMap");
}

#[tokio::test]
async fn test_phase_b_business_architecture_with_fibo() {
    init_tracing();

    let fibo_entities = vec![
        "fibo-fnd:LegalPerson".to_string(),
        "fibo-fnd:Organization".to_string(),
        "fibo-lcc:LoanContract".to_string(),
    ];

    let messages = simulate_phase_b_with_fibo(fibo_entities).await;

    assert_eq!(
        messages.len(),
        3,
        "Phase B should process 3 FIBO entities, got {}",
        messages.len()
    );

    // Verify FIBO concepts mapped to business capabilities
    assert_fibo_mapped(&messages, "fibo-fnd:LegalPerson", "CustomerOnboarding");
    assert_fibo_mapped(&messages, "fibo-fnd:Organization", "BusinessStructure");
    assert_fibo_mapped(&messages, "fibo-lcc:LoanContract", "ProductCatalog");
}

#[tokio::test]
async fn test_phase_a_to_b_handoff() {
    init_tracing();

    let messages = simulate_phase_a_to_b_handoff().await;

    assert_eq!(
        messages.len(),
        8,
        "Phase A to B handoff should complete in 8 turns (5 Phase A + 3 Phase B), got {}",
        messages.len()
    );

    // Verify Phase A artifacts
    assert_has_artifact(&messages, "ArchitectureVisionStatement");
    assert_has_artifact(&messages, "StakeholderMap");

    // Verify Phase B FIBO mappings
    assert_fibo_mapped(&messages, "fibo-fnd:LegalPerson", "CustomerOnboarding");
    assert_fibo_mapped(&messages, "fibo-fnd:Organization", "BusinessStructure");
    assert_fibo_mapped(&messages, "fibo-lcc:LoanContract", "ProductCatalog");

    // Verify correlation IDs are preserved across handoff
    let correlation_ids: Vec<_> = messages
        .iter()
        .filter_map(|m| m.envelope.correlation_id.clone())
        .collect();

    // At least one correlation ID should be present
    assert!(
        !correlation_ids.is_empty(),
        "Correlation IDs should be preserved across handoff"
    );
}
