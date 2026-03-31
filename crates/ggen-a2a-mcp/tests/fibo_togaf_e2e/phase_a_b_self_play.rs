//! FIBO + TOGAF Phases A & B Self-Play Tests
//!
//! Tests the Architecture Vision and Business Architecture agents
//! collaborating through 8-12 turns of architecture development.
//!
//! ## Test Inventory
//!
//! 1. `test_phase_a_vision_agent_turns`        -- Phase A multi-turn collaboration (5 turns)
//! 2. `test_phase_b_business_architecture_with_fibo` -- Phase B with FIBO concepts (7 turns)
//! 3. `test_phase_a_to_b_handoff`              -- Phase A → B transition (8 turns)

use std::collections::HashMap;

use a2a_generated::converged::message::{
    ConvergedMessage, ConvergedMessageType, MessageEnvelope, MessageLifecycle, MessagePriority,
    MessageRouting, MessageState, QoSRequirements, ReliabilityLevel, UnifiedContent,
};
use chrono::Utc;
use ggen_a2a_mcp::{MessageRouter, A2aMessageConverter};

mod common;
use common::init_tracing;

// ---------------------------------------------------------------------------
// Helpers
// ---------------------------------------------------------------------------

/// Build a minimal `MessageRouting` for tests.
fn test_routing() -> MessageRouting {
    MessageRouting {
        path: vec!["togaf-agent".to_string()],
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

/// Create a TOGAF artifact message with metadata.
fn create_artifact_message(
    id: &str,
    source: &str,
    artifact_type: &str,
    content: &str,
    correlation_id: Option<&str>,
) -> ConvergedMessage {
    let mut data = HashMap::new();
    data.insert("artifact_type".to_string(), serde_json::Value::String(artifact_type.to_string()));
    data.insert("content".to_string(), serde_json::Value::String(content.to_string()));
    data.insert("phase".to_string(), serde_json::Value::String(extract_phase(artifact_type)));

    ConvergedMessage {
        message_id: id.to_string(),
        source: source.to_string(),
        target: Some("architecture-board".to_string()),
        envelope: MessageEnvelope {
            message_type: ConvergedMessageType::Task,
            priority: MessagePriority::Normal,
            timestamp: Utc::now(),
            schema_version: "1.0".to_string(),
            content_type: "application/json".to_string(),
            correlation_id: correlation_id.map(|s| s.to_string()),
            causation_chain: None,
        },
        payload: a2a_generated::converged::message::ConvergedPayload {
            content: UnifiedContent::Data {
                data,
                schema: Some("TOGAFArtifact".to_string()),
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

/// Create a FIBO mapping message.
fn create_fibo_mapping_message(
    id: &str,
    source: &str,
    fibo_entity: &str,
    business_capability: &str,
    correlation_id: Option<&str>,
) -> ConvergedMessage {
    let mut data = HashMap::new();
    data.insert(
        "fibo_entity".to_string(),
        serde_json::Value::String(fibo_entity.to_string()),
    );
    data.insert(
        "business_capability".to_string(),
        serde_json::Value::String(business_capability.to_string()),
    );
    data.insert(
        "mapping_type".to_string(),
        serde_json::Value::String("semantic".to_string()),
    );

    ConvergedMessage {
        message_id: id.to_string(),
        source: source.to_string(),
        target: Some("fibo-bridge".to_string()),
        envelope: MessageEnvelope {
            message_type: ConvergedMessageType::Query,
            priority: MessagePriority::Normal,
            timestamp: Utc::now(),
            schema_version: "1.0".to_string(),
            content_type: "application/json".to_string(),
            correlation_id: correlation_id.map(|s| s.to_string()),
            causation_chain: None,
        },
        payload: a2a_generated::converged::message::ConvergedPayload {
            content: UnifiedContent::Data {
                data,
                schema: Some("FIBOMapping".to_string()),
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

/// Extract TOGAF phase from artifact type.
fn extract_phase(artifact_type: &str) -> String {
    if artifact_type.contains("Vision") || artifact_type.contains("Stakeholder") {
        "PhaseA".to_string()
    } else if artifact_type.contains("Business") || artifact_type.contains("Capability") {
        "PhaseB".to_string()
    } else {
        "Unknown".to_string()
    }
}

/// Assert that an artifact exists in the message list.
fn assert_has_artifact(messages: &[ConvergedMessage], artifact_type: &str) {
    let has_artifact = messages.iter().any(|m| {
        match &m.payload.content {
            UnifiedContent::Data { data, .. } => data
                .get("artifact_type")
                .and_then(|v| v.as_str())
                .map(|t| t == artifact_type)
                .unwrap_or(false),
            _ => false,
        }
    });

    assert!(
        has_artifact,
        "Should have artifact of type {}, but found: {:?}",
        artifact_type,
        messages
            .iter()
            .filter_map(|m| match &m.payload.content {
                UnifiedContent::Data { data, .. } => {
                    data.get("artifact_type").and_then(|v| v.as_str()).map(|s| s.to_string())
                }
                _ => None,
            })
            .collect::<Vec<_>>()
    );
}

/// Assert that a FIBO entity is mapped to a business capability.
fn assert_fibo_mapped(messages: &[ConvergedMessage], fibo_entity: &str, business_capability: &str) {
    let mapped = messages.iter().any(|m| {
        match &m.payload.content {
            UnifiedContent::Data { data, .. } => {
                if let Some(entity) = data.get("fibo_entity") {
                    if let Some(capability) = data.get("business_capability") {
                        return entity.as_str() == Some(fibo_entity)
                            && capability.as_str() == Some(business_capability);
                    }
                }
                false
            }
            _ => false,
        }
    });

    assert!(
        mapped,
        "FIBO entity {} should map to capability {}",
        fibo_entity, business_capability
    );
}

// ===========================================================================
// Test 1: Phase A (Architecture Vision) Multi-Turn Collaboration
// ===========================================================================

/// Simulate Phase A collaboration through multiple turns.
async fn simulate_phase_a_collaboration(turns: usize) -> Vec<ConvergedMessage> {
    let router = MessageRouter::with_defaults();
    let mut messages = Vec::new();
    let correlation_id = Some("phase-a-vision-42".to_string());

    for turn in 1..=turns {
        let msg = match turn {
            1 => create_artifact_message(
                &format!("phase-a-turn-{}", turn),
                "stakeholder-analyst",
                "StakeholderMap",
                "Identified 15 key stakeholders including regulators, executives, and customers",
                correlation_id.as_deref(),
            ),
            2 => create_artifact_message(
                &format!("phase-a-turn-{}", turn),
                "vision-agent",
                "ArchitectureVisionStatement",
                "To enable trusted financial data exchange through semantic interoperability",
                correlation_id.as_deref(),
            ),
            3 => create_artifact_message(
                &format!("phase-a-turn-{}", turn),
                "arb-reviewer",
                "ARBReviewFeedback",
                "Vision is aligned with strategic goals but needs more detail on governance",
                correlation_id.as_deref(),
            ),
            4 => create_artifact_message(
                &format!("phase-a-turn-{}", turn),
                "vision-agent",
                "ArchitectureVisionStatement",
                "To enable trusted financial data exchange through semantic interoperability with robust governance framework",
                correlation_id.as_deref(),
            ),
            5 => create_artifact_message(
                &format!("phase-a-turn-{}", turn),
                "arb-approver",
                "ApprovalRecord",
                "Architecture Vision approved for Phase B transition",
                correlation_id.as_deref(),
            ),
            _ => unreachable!(),
        };

        let response = router.route(msg).await.expect("routing should succeed");
        messages.push(response);
    }

    messages
}

#[tokio::test]
async fn test_phase_a_vision_agent_turns() {
    init_tracing();

    let turns = 5;
    let messages = simulate_phase_a_collaboration(turns).await;

    assert_eq!(
        messages.len(),
        turns,
        "Phase A should complete in {} turns",
        turns
    );

    // Verify vision artifacts created
    assert_has_artifact(&messages, "ArchitectureVisionStatement");
    assert_has_artifact(&messages, "StakeholderMap");
    assert_has_artifact(&messages, "ARBReviewFeedback");
    assert_has_artifact(&messages, "ApprovalRecord");
}

// ===========================================================================
// Test 2: Phase B (Business Architecture) with FIBO Concepts
// ===========================================================================

/// Simulate Phase B with FIBO entity mappings.
async fn simulate_phase_b_with_fibo(fibo_entities: Vec<(&str, &str)>) -> Vec<ConvergedMessage> {
    let router = MessageRouter::with_defaults();
    let mut messages = Vec::new();
    let correlation_id = Some("phase-b-fibo-99".to_string());

    for (idx, (fibo_entity, business_capability)) in fibo_entities.iter().enumerate() {
        // Turn 6: Create business capability analysis
        let cap_msg = create_artifact_message(
            &format!("phase-b-turn-{}", idx * 2 + 6),
            "business-architect",
            &format!("BusinessCapability-{}", business_capability),
            &format!("Analyzed capability: {}", business_capability),
            correlation_id.as_deref(),
        );
        let cap_response = router.route(cap_msg).await.expect("routing should succeed");
        messages.push(cap_response);

        // Turn 7: Create FIBO mapping
        let fibo_msg = create_fibo_mapping_message(
            &format!("phase-b-turn-{}", idx * 2 + 7),
            "fibo-bridge",
            fibo_entity,
            business_capability,
            correlation_id.as_deref(),
        );
        let fibo_response = router.route(fibo_msg).await.expect("routing should succeed");
        messages.push(fibo_response);
    }

    messages
}

#[tokio::test]
async fn test_phase_b_business_architecture_with_fibo() {
    init_tracing();

    let fibo_entities = vec![
        ("fibo-fnd:LegalPerson", "CustomerOnboarding"),
        ("fibo-fnd:Organization", "BusinessStructure"),
        ("fibo-lcc:LoanContract", "ProductCatalog"),
    ];

    let messages = simulate_phase_b_with_fibo(fibo_entities).await;

    // Should have 6 messages (2 per FIBO entity: capability + mapping)
    assert_eq!(
        messages.len(),
        6,
        "Phase B should generate 6 messages for 3 FIBO entities"
    );

    // Verify FIBO concepts mapped to business capabilities
    assert_fibo_mapped(&messages, "fibo-fnd:LegalPerson", "CustomerOnboarding");
    assert_fibo_mapped(&messages, "fibo-fnd:Organization", "BusinessStructure");
    assert_fibo_mapped(&messages, "fibo-lcc:LoanContract", "ProductCatalog");

    // Verify business capability artifacts created
    assert_has_artifact(&messages, "BusinessCapability-CustomerOnboarding");
    assert_has_artifact(&messages, "BusinessCapability-BusinessStructure");
    assert_has_artifact(&messages, "BusinessCapability-ProductCatalog");
}

// ===========================================================================
// Test 3: Phase A → B Handoff
// ===========================================================================

/// Simulate complete Phase A to B handoff workflow.
async fn simulate_phase_a_to_b_handoff() -> Vec<ConvergedMessage> {
    let router = MessageRouter::with_defaults();
    let mut messages = Vec::new();
    let correlation_id = Some("phase-handoff-001".to_string());

    // === Phase A: Turns 1-5 (vision development) ===
    let phase_a_turns = 5;
    for turn in 1..=phase_a_turns {
        let msg = match turn {
            1 => create_artifact_message(
                &format!("handoff-turn-{}", turn),
                "stakeholder-analyst",
                "StakeholderMap",
                "Identified 15 key stakeholders",
                correlation_id.as_deref(),
            ),
            2 => create_artifact_message(
                &format!("handoff-turn-{}", turn),
                "vision-agent",
                "ArchitectureVisionStatement",
                "To enable trusted financial data exchange",
                correlation_id.as_deref(),
            ),
            3 => create_artifact_message(
                &format!("handoff-turn-{}", turn),
                "arb-reviewer",
                "ARBReviewFeedback",
                "Vision approved with minor suggestions",
                correlation_id.as_deref(),
            ),
            4 => create_artifact_message(
                &format!("handoff-turn-{}", turn),
                "vision-agent",
                "ArchitectureVisionStatement",
                "To enable trusted financial data exchange through semantic interoperability",
                correlation_id.as_deref(),
            ),
            5 => create_artifact_message(
                &format!("handoff-turn-{}", turn),
                "arb-approver",
                "ApprovalRecord",
                "Architecture Vision approved - proceed to Phase B",
                correlation_id.as_deref(),
            ),
            _ => unreachable!(),
        };

        let response = router.route(msg).await.expect("routing should succeed");
        messages.push(response);
    }

    // === Handoff Message ===
    let handoff_msg = create_artifact_message(
        "handoff-turn-6",
        "phase-coordinator",
        "PhaseHandoff",
        "Transitioning from Phase A (Vision) to Phase B (Business Architecture)",
        correlation_id.as_deref(),
    );
    let handoff_response = router.route(handoff_msg).await.expect("routing should succeed");
    messages.push(handoff_response);

    // === Phase B: Turns 7-8 (business architecture kickoff) ===
    for turn in 7..=8 {
        let msg = match turn {
            7 => create_artifact_message(
                &format!("handoff-turn-{}", turn),
                "business-architect",
                "BusinessArchitectureKickoff",
                "Received vision artifacts, initiating business capability analysis",
                correlation_id.as_deref(),
            ),
            8 => create_fibo_mapping_message(
                &format!("handoff-turn-{}", turn),
                "fibo-bridge",
                "fibo-fnd:LegalPerson",
                "CustomerOnboarding",
                correlation_id.as_deref(),
            ),
            _ => unreachable!(),
        };

        let response = router.route(msg).await.expect("routing should succeed");
        messages.push(response);
    }

    messages
}

#[tokio::test]
async fn test_phase_a_to_b_handoff() {
    init_tracing();

    let messages = simulate_phase_a_to_b_handoff().await;

    // Should have 8 messages total (5 Phase A + 1 handoff + 2 Phase B)
    assert_eq!(
        messages.len(),
        8,
        "Phase A→B handoff should complete in 8 turns"
    );

    // Verify Phase A artifacts
    assert_has_artifact(&messages, "StakeholderMap");
    assert_has_artifact(&messages, "ArchitectureVisionStatement");
    assert_has_artifact(&messages, "ApprovalRecord");

    // Verify handoff artifact
    assert_has_artifact(&messages, "PhaseHandoff");

    // Verify Phase B artifacts
    assert_has_artifact(&messages, "BusinessArchitectureKickoff");

    // Verify FIBO mapping preserved across handoff
    assert_fibo_mapped(&messages, "fibo-fnd:LegalPerson", "CustomerOnboarding");

    // Verify all messages share the same correlation ID (no message loss)
    let correlation_ids: Vec<_> = messages
        .iter()
        .filter_map(|m| m.envelope.correlation_id.as_deref())
        .collect();
    assert!(
        correlation_ids.iter().all(|id| *id == "phase-handoff-001"),
        "All messages should share the same correlation ID"
    );
}
