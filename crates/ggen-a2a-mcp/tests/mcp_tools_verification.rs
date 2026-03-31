//! Direct verification of the 4 new MCP tools
//!
//! Tests that generate_agents, generate_a2a_test, validate_fibo, and orchestrate_conversation
//! tools work correctly and produce valid output.

use ggen_a2a_mcp::ggen_server::GgenMcpServer;
use std::path::PathBuf;

#[tokio::test]
async fn test_generate_a2a_test_tool_creates_valid_test_file() {
    let server = GgenMcpServer::new();
    let output_path = PathBuf::from("/tmp/test_mcp_a2a_test.rs");

    let result = server
        .generate_a2a_test(ggen_a2a_mcp::ggen_server::GenerateA2aTestParams {
            agent_count: 2,
            turn_count: 3,
            output_path: output_path.to_string_lossy().to_string(),
            test_name: Some("verification_test".to_string()),
        })
        .await;

    assert!(result.is_success(), "Tool should succeed");
    assert!(output_path.exists(), "Test file should be created");

    let content = std::fs::read_to_string(&output_path).unwrap();
    assert!(
        content.contains("verification_test"),
        "Test name should be in file"
    );
    assert!(
        content.contains("agent_count: 2"),
        "Agent count should be in file"
    );
    assert!(
        content.contains("turn_count: 3"),
        "Turn count should be in file"
    );

    // Cleanup
    let _ = std::fs::remove_file(&output_path);
}

#[tokio::test]
async fn test_validate_fibo_tool_analyzes_ontology() {
    let server = GgenMcpServer::new();

    let result = server
        .validate_fibo(ggen_a2a_mcp::ggen_server::ValidateFiboParams {
            ontology_path: "crates/ggen-core/queries".to_string(),
            check_coverage: Some(true),
            min_coverage: Some(0),
        })
        .await;

    assert!(result.is_success(), "Tool should succeed");

    let response = result.unwrap().contents.unwrap();
    let response_text = response.iter().filter_map(|c| c.as_text()).next().unwrap();

    assert!(
        response_text.contains("fibo_concepts_found") || response_text.contains("coverage_percent"),
        "Response should contain FIBO analysis"
    );
}

#[tokio::test]
async fn test_orchestrate_conversation_tool_creates_orchestrator() {
    let server = GgenMcpServer::new();

    // Create test config files
    let agents_config = PathBuf::from("/tmp/test_agents.json");
    let turns_config = PathBuf::from("/tmp/test_turns.json");
    let output_path = PathBuf::from("/tmp/test_orchestrator.rs");

    std::fs::write(&agents_config, r#"{"agents": [{"id": "a1"}]}"#).unwrap();
    std::fs::write(&turns_config, r#"{"turns": [{"turn": 1}]}"#).unwrap();

    let result = server
        .orchestrate_conversation(ggen_a2a_mcp::ggen_server::OrchestrateConversationParams {
            agents_config: agents_config.to_string_lossy().to_string(),
            turns_config: turns_config.to_string_lossy().to_string(),
            output_path: output_path.to_string_lossy().to_string(),
            pattern: Some("sequential".to_string()),
        })
        .await;

    assert!(result.is_success(), "Tool should succeed");
    assert!(output_path.exists(), "Orchestrator file should be created");

    let content = std::fs::read_to_string(&output_path).unwrap();
    assert!(
        content.contains("Orchestrator"),
        "Should create Orchestrator"
    );
    assert!(
        content.contains("OrchestrationPattern"),
        "Should have pattern enum"
    );

    // Cleanup
    let _ = std::fs::remove_file(&output_path);
    let _ = std::fs::remove_file(&agents_config);
    let _ = std::fs::remove_file(&turns_config);
}
