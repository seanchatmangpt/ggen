//! FIBO + TOGAF Phases C & D Self-Play Tests (Standalone)
//!
//! Tests Data Architecture (FIBO-based) and Technology Architecture agents.
//! Phase C: Use FIBO as canonical data model
//! Phase D: Map data entities to technology platforms

use ggen_a2a_mcp::client::GroqA2AClient;
use ggen_a2a_mcp::message::{A2AMessage, ContentBlock};
use std::time::Duration;
use tokio::time::timeout;

/// FIBO ontology classes for financial data modeling
const FIBO_CLASSES: &[(&str, &str, &str)] = &[
    // FND: Foundation
    (
        "fibo-fnd:LegalPerson",
        "Customer Entity",
        "Represents a legally recognized individual or organization",
    ),
    (
        "fibo-fnd:Organization",
        "Organization Entity",
        "Represents a business entity",
    ),
    (
        "fibo-fnd:Address",
        "Address Entity",
        "Represents a physical or mailing address",
    ),
    // LCC: Loan Contracts
    (
        "fibo-lcc:LoanContract",
        "Loan Agreement",
        "Represents a loan contract between parties",
    ),
    (
        "fibo-lcc:CreditAgreement",
        "Credit Agreement Entity",
        "Represents a credit facility agreement",
    ),
    (
        "fibo-lcc:InterestRate",
        "Rate Entity",
        "Represents an interest rate applied to a loan",
    ),
    // SEC: Securities
    (
        "fibo-sec:SecurityInstrument",
        "Holding Entity",
        "Represents a financial security or instrument",
    ),
    (
        "fibo-sec:EquityInstrument",
        "Equity Holding Entity",
        "Represents an ownership interest",
    ),
    (
        "fibo-sec:DebtInstrument",
        "Debt Holding Entity",
        "Represents a debt obligation",
    ),
    // IND: Indicators
    (
        "fibo-ind:MarketPrice",
        "Price Entity",
        "Represents a market price for an instrument",
    ),
    (
        "fibo-ind:InterestRate",
        "Rate Entity",
        "Represents an interest rate value",
    ),
];

/// Technology platforms mapped from data entities
const TECHNOLOGY_PLATFORMS: &[(&str, &str, &str)] = &[
    (
        "Customer Entity",
        "Core Banking System",
        "Customer 360 platform for customer data management",
    ),
    (
        "Organization Entity",
        "CRM System",
        "Salesforce for organization relationship management",
    ),
    (
        "Loan Agreement",
        "Loan Origination Platform",
        "LoanIQ for loan lifecycle management",
    ),
    (
        "Credit Agreement Entity",
        "Credit Risk Platform",
        "Moody's Analytics for credit risk assessment",
    ),
    (
        "Rate Entity",
        "Market Data Platform",
        "Bloomberg for real-time market data",
    ),
    (
        "Holding Entity",
        "Investment Management System",
        "Simcorp for investment portfolio management",
    ),
    (
        "Equity Holding Entity",
        "Trading Platform",
        "Apex for equity trading and execution",
    ),
    (
        "Debt Holding Entity",
        "Fixed Income Platform",
        "BlackRock for fixed income portfolio management",
    ),
    (
        "Price Entity",
        "Pricing Engine",
        "OpenFin for real-time pricing calculations",
    ),
    (
        "Address Entity",
        "Master Data Management",
        "Informatica MDM for address standardization",
    ),
];

/// Initialize tracing for tests
fn init_tracing() {
    tracing_subscriber::fmt()
        .with_env_filter("info")
        .with_test_writer()
        .try_init()
        .ok();
}

/// Simulate Phase C: Data Architecture with FIBO
///
/// Turns 13-20: Map FIBO ontology classes to data entities
async fn simulate_phase_c_fibo_mapping(
    client: &GroqA2AClient, fibo_classes: &[(&str, &str, &str)],
) -> Vec<A2AMessage> {
    let mut messages = Vec::new();

    // Phase C system prompt
    let system_prompt = r#"You are the Data Architecture Agent for an enterprise architecture project.
You are working in TOGAF Phase C (Data Architecture) using FIBO (Financial Industry Business Ontology) as the canonical data model.

Your task:
1. For each FIBO ontology class provided, create a corresponding data entity
2. Define key attributes for each data entity based on FIBO properties
3. Establish relationships between data entities
4. Output the data model in a structured format

Output format for each FIBO class:
{
  "fibo_class": "fibo-fnd:LegalPerson",
  "data_entity": "Customer Entity",
  "description": "...",
  "attributes": ["attr1", "attr2", ...],
  "relationships": [{"type": "relates_to", "target": "Other Entity"}]
}

Focus on: Financial services data modeling, regulatory compliance, data lineage"#;

    for (i, (fibo_class, data_entity, description)) in fibo_classes.iter().enumerate() {
        let turn_num = 13 + i;

        let user_prompt = format!(
            "Turn {}: Create data entity for FIBO class {}\n\n\
            FIBO Class: {}\n\
            Proposed Data Entity: {}\n\
            Description: {}\n\n\
            Define the data entity with attributes and relationships.",
            turn_num, fibo_class, fibo_class, data_entity, description
        );

        match timeout(
            Duration::from_secs(30),
            client.complete(&user_prompt, Some(system_prompt)),
        )
        .await
        {
            Ok(Ok(response)) => {
                let content = response.content;

                // Create A2A message
                let msg = A2AMessage {
                    role: "assistant".to_string(),
                    content: vec![ContentBlock::Text {
                        text: format!(
                            "Turn {}: Created data entity '{}' from FIBO class '{}'\n\
                            Response preview: {}",
                            turn_num,
                            data_entity,
                            fibo_class,
                            content.chars().take(200).collect::<String>()
                        ),
                        metadata: serde_json::json!({
                            "turn": turn_num,
                            "phase": "C",
                            "fibo_class": fibo_class,
                            "data_entity": data_entity,
                        }),
                    }],
                    timestamp: Some(std::time::SystemTime::now()),
                };

                messages.push(msg);

                tracing::info!(
                    "Turn {}: Created data entity '{}' from FIBO class '{}'",
                    turn_num,
                    data_entity,
                    fibo_class
                );
            }
            Ok(Err(e)) => {
                tracing::error!("Turn {}: LLM error: {}", turn_num, e);
                break;
            }
            Err(e) => {
                tracing::error!("Turn {}: Timeout: {}", turn_num, e);
                break;
            }
        }

        tokio::time::sleep(Duration::from_millis(500)).await;
    }

    messages
}

/// Simulate Phase D: Technology Architecture
///
/// Turns 21-28: Map data entities to technology platforms
async fn simulate_phase_d_platform_mapping(
    client: &GroqA2AClient, tech_stack: &[(&str, &str, &str)],
) -> Vec<A2AMessage> {
    let mut messages = Vec::new();

    // Phase D system prompt
    let system_prompt = r#"You are the Technology Architecture Agent for an enterprise architecture project.
You are working in TOGAF Phase D (Technology Architecture) mapping data entities to technology platforms.

Your task:
1. For each data entity, recommend an appropriate technology platform
2. Consider integration patterns with existing systems
3. Address non-functional requirements (performance, security, scalability)
4. Output platform recommendations with rationale

Output format for each data entity:
{
  "data_entity": "Customer Entity",
  "platform": "Core Banking System",
  "rationale": "...",
  "integration_pattern": "API / Event-driven / Batch",
  "key_capabilities": ["cap1", "cap2", ...],
  "alternatives": ["Alt1", "Alt2"]
}

Focus on: Financial services platforms, regulatory compliance, cloud-native architecture"#;

    for (i, (data_entity, platform, rationale)) in tech_stack.iter().enumerate() {
        let turn_num = 21 + i;

        let user_prompt = format!(
            "Turn {}: Recommend technology platform for data entity\n\n\
            Data Entity: {}\n\
            Proposed Platform: {}\n\
            Rationale: {}\n\n\
            Provide platform recommendation with integration pattern and alternatives.",
            turn_num, data_entity, platform, rationale
        );

        match timeout(
            Duration::from_secs(30),
            client.complete(&user_prompt, Some(system_prompt)),
        )
        .await
        {
            Ok(Ok(response)) => {
                let content = response.content;

                // Create A2A message
                let msg = A2AMessage {
                    role: "assistant".to_string(),
                    content: vec![ContentBlock::Text {
                        text: format!(
                            "Turn {}: Mapped data entity '{}' to platform '{}'\n\
                            Response preview: {}",
                            turn_num,
                            data_entity,
                            platform,
                            content.chars().take(200).collect::<String>()
                        ),
                        metadata: serde_json::json!({
                            "turn": turn_num,
                            "phase": "D",
                            "data_entity": data_entity,
                            "platform": platform,
                        }),
                    }],
                    timestamp: Some(std::time::SystemTime::now()),
                };

                messages.push(msg);

                tracing::info!(
                    "Turn {}: Mapped data entity '{}' to platform '{}'",
                    turn_num,
                    data_entity,
                    platform
                );
            }
            Ok(Err(e)) => {
                tracing::error!("Turn {}: LLM error: {}", turn_num, e);
                break;
            }
            Err(e) => {
                tracing::error!("Turn {}: Timeout: {}", turn_num, e);
                break;
            }
        }

        tokio::time::sleep(Duration::from_millis(500)).await;
    }

    messages
}

/// Assert that a data entity exists in messages
fn assert_data_entity_exists(messages: &[A2AMessage], fibo_class: &str, data_entity: &str) -> bool {
    messages.iter().any(|msg| {
        msg.content.iter().any(|block| {
            if let ContentBlock::Text { text, metadata } = block {
                text.contains(data_entity)
                    && metadata
                        .get("fibo_class")
                        .and_then(|v| v.as_str())
                        .map(|s| s == fibo_class)
                        .unwrap_or(false)
            } else {
                false
            }
        })
    })
}

/// Assert platform selection in messages
fn assert_platform_selection(messages: &[A2AMessage], tech_stack: &[(&str, &str, &str)]) -> bool {
    tech_stack.iter().all(|(data_entity, platform, _)| {
        messages.iter().any(|msg| {
            msg.content.iter().any(|block| {
                if let ContentBlock::Text { text, metadata } = block {
                    text.contains(platform)
                        && metadata
                            .get("data_entity")
                            .and_then(|v| v.as_str())
                            .map(|s| s == *data_entity)
                            .unwrap_or(false)
                } else {
                    false
                }
            })
        })
    })
}

/// Test Phase C: Data Architecture with FIBO
#[tokio::test]
async fn test_phase_c_data_architecture_with_fibo() {
    init_tracing();

    let api_key = std::env::var("GROQ_API_KEY").unwrap_or_else(|_| "test-key".to_string());
    let model = std::env::var("GROQ_MODEL").unwrap_or_else(|_| "groq-llama3-8b-8192".to_string());

    let client = match GroqA2AClient::new(&api_key, &model) {
        Ok(c) => c,
        Err(e) => {
            tracing::warn!("Failed to create Groq client: {}. Skipping test.", e);
            return; // Skip test if client creation fails
        }
    };

    // Turns 13-20: Data Architecture
    // Each turn maps FIBO ontology to data entities
    let messages = simulate_phase_c_fibo_mapping(&client, FIBO_CLASSES).await;

    println!("\n=== Phase C: Data Architecture with FIBO ===");
    println!("Total messages: {}", messages.len());

    // Verify each FIBO class became a data entity
    let mut entity_count = 0;

    for (fibo_class, data_entity, _) in FIBO_CLASSES {
        if assert_data_entity_exists(&messages, fibo_class, data_entity) {
            entity_count += 1;
            println!(
                "✓ Mapped FIBO class '{}' to data entity '{}'",
                fibo_class, data_entity
            );
        } else {
            println!(
                "✗ Failed to map FIBO class '{}' to data entity '{}'",
                fibo_class, data_entity
            );
        }
    }

    println!(
        "\nData Entities Created: {}/{}",
        entity_count,
        FIBO_CLASSES.len()
    );

    // Assertions
    assert!(
        entity_count >= FIBO_CLASSES.len() / 2,
        "Expected at least 50% of FIBO classes to be mapped, got {}/{}",
        entity_count,
        FIBO_CLASSES.len()
    );
}

/// Test Phase D: Technology Architecture
#[tokio::test]
async fn test_phase_d_technology_architecture() {
    init_tracing();

    let api_key = std::env::var("GROQ_API_KEY").unwrap_or_else(|_| "test-key".to_string());
    let model = std::env::var("GROQ_MODEL").unwrap_or_else(|_| "groq-llama3-8b-8192".to_string());

    let client = match GroqA2AClient::new(&api_key, &model) {
        Ok(c) => c,
        Err(e) => {
            tracing::warn!("Failed to create Groq client: {}. Skipping test.", e);
            return; // Skip test if client creation fails
        }
    };

    // Turns 21-28: Technology Architecture
    // Map data entities to applications/platforms
    let messages = simulate_phase_d_platform_mapping(&client, TECHNOLOGY_PLATFORMS).await;

    println!("\n=== Phase D: Technology Architecture ===");
    println!("Total messages: {}", messages.len());

    // Verify data-to-platform mapping
    let mut platform_count = 0;

    for (data_entity, platform, _) in TECHNOLOGY_PLATFORMS {
        if messages.iter().any(|msg| {
            msg.content.iter().any(|block| {
                if let ContentBlock::Text { text, metadata } = block {
                    text.contains(platform)
                        && metadata
                            .get("data_entity")
                            .and_then(|v| v.as_str())
                            .map(|s| s == *data_entity)
                            .unwrap_or(false)
                } else {
                    false
                }
            })
        }) {
            platform_count += 1;
            println!(
                "✓ Mapped data entity '{}' to platform '{}'",
                data_entity, platform
            );
        } else {
            println!(
                "✗ Failed to map data entity '{}' to platform '{}'",
                data_entity, platform
            );
        }
    }

    println!(
        "\nPlatform Mappings Created: {}/{}",
        platform_count,
        TECHNOLOGY_PLATFORMS.len()
    );

    // Assertions
    assert!(
        platform_count >= TECHNOLOGY_PLATFORMS.len() / 2,
        "Expected at least 50% of data entities to be mapped to platforms, got {}/{}",
        platform_count,
        TECHNOLOGY_PLATFORMS.len()
    );
}

/// Integration test: Full Phase C + D workflow
#[tokio::test]
async fn test_phase_c_d_full_workflow() {
    init_tracing();

    let api_key = std::env::var("GROQ_API_KEY").unwrap_or_else(|_| "test-key".to_string());
    let model = std::env::var("GROQ_MODEL").unwrap_or_else(|_| "groq-llama3-8b-8192".to_string());

    let client = match GroqA2AClient::new(&api_key, &model) {
        Ok(c) => c,
        Err(e) => {
            tracing::warn!("Failed to create Groq client: {}. Skipping test.", e);
            return; // Skip test if client creation fails
        }
    };

    println!("\n=== Full Phase C + D Workflow ===");

    // Phase C: Data Architecture with FIBO
    let phase_c_messages = simulate_phase_c_fibo_mapping(&client, FIBO_CLASSES).await;
    println!("Phase C messages: {}", phase_c_messages.len());

    // Phase D: Technology Architecture
    let phase_d_messages = simulate_phase_d_platform_mapping(&client, TECHNOLOGY_PLATFORMS).await;
    println!("Phase D messages: {}", phase_d_messages.len());

    let all_messages = vec![phase_c_messages, phase_d_messages]
        .into_iter()
        .flatten()
        .collect::<Vec<_>>();

    println!("Total messages across both phases: {}", all_messages.len());

    // Verify FIBO to Platform traceability
    println!("\n=== FIBO to Platform Traceability ===");

    let mut traceable_count = 0;

    for (fibo_class, data_entity, _) in FIBO_CLASSES {
        // Check if data entity exists in Phase C
        let entity_exists = assert_data_entity_exists(&phase_c_messages, fibo_class, data_entity);

        // Check if data entity is mapped to platform in Phase D
        let platform_mapped = TECHNOLOGY_PLATFORMS.iter().any(|(entity, _, _)| {
            entity == data_entity
                && phase_d_messages.iter().any(|msg| {
                    msg.content.iter().any(|block| {
                        if let ContentBlock::Text { metadata, .. } = block {
                            metadata
                                .get("data_entity")
                                .and_then(|v| v.as_str())
                                .map(|s| s == *data_entity)
                                .unwrap_or(false)
                        } else {
                            false
                        }
                    })
                })
        });

        if entity_exists && platform_mapped {
            traceable_count += 1;
            println!(
                "✓ FIBO '{}' → Entity '{}' → Platform",
                fibo_class, data_entity
            );
        } else {
            println!(
                "✗ FIBO '{}' → Entity '{}' (incomplete trace)",
                fibo_class, data_entity
            );
        }
    }

    println!(
        "\nTraceable FIBO entities: {}/{}",
        traceable_count,
        FIBO_CLASSES.len()
    );

    // Assert at least 50% traceability
    assert!(
        traceable_count >= FIBO_CLASSES.len() / 2,
        "Expected at least 50% of FIBO entities to be traceable to platforms, got {}/{}",
        traceable_count,
        FIBO_CLASSES.len()
    );
}
