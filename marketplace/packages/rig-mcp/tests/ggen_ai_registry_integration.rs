//! Integration tests for ggen-ai core registry connection
//!
//! Tests the ToolRegistrationManager's ability to register discovered MCP tools
//! to the ggen-ai REGISTRY singleton, making them available to all agents.

use rig_mcp_integration::{
    registration::{ToolRegistrationManager, RegistrationConfig, RegistrationError},
    discovery::ToolSchema,
};

/// Test that discovered tools are registered to the ggen-ai REGISTRY
#[cfg(feature = "ggen-ai")]
#[tokio::test]
async fn test_register_a2a_agents_to_core_registry() {
    use ggen_ai::tool_registry::REGISTRY;

    // Create a registration manager with unique prefix to avoid conflicts
    let config = RegistrationConfig::default()
        .with_id_prefix("test1_registry")
        .with_source_tag("integration-test");
    let manager = ToolRegistrationManager::new(config);

    // Create a test tool schema
    let schema = ToolSchema::new("test_tool_registry", "A test tool for integration")
        .with_tag("validation")
        .with_tag("code_generation");

    let source_url = "http://test-server-registry-1:3000";

    // First, register the tool locally
    let tool_id = manager
        .register_tool(source_url, schema)
        .await
        .expect("Tool registration should succeed");

    // Verify tool is in local store
    assert!(manager.is_registered(&tool_id).await);
    assert_eq!(manager.tools_from_source(source_url).await.len(), 1);

    // Register A2A agents to core registry
    let agent_count = manager
        .register_a2a_agents(source_url)
        .await
        .expect("A2A agent registration should succeed");

    // Verify one agent was registered
    assert_eq!(agent_count, 1);

    // Verify statistics updated
    let stats = manager.statistics().await;
    assert_eq!(stats.total_registered, 1);
    assert_eq!(stats.agents_registered, 1);

    // Verify tool is in core registry
    let registry = REGISTRY.read().unwrap();
    assert!(registry.contains(&tool_id));
    let core_tool = registry.get(&tool_id).expect("Tool should be in core registry");
    assert_eq!(core_tool.name, "test_tool_registry");
    assert_eq!(core_tool.id, tool_id);

    // Verify tags were converted
    use ggen_ai::ToolTag;
    assert!(core_tool.tags.contains(&ToolTag::Validation));
    assert!(core_tool.tags.contains(&ToolTag::CodeGeneration));
}

/// Test that multiple tools are registered to core registry
#[cfg(feature = "ggen-ai")]
#[tokio::test]
async fn test_register_multiple_tools_to_core_registry() {
    use ggen_ai::tool_registry::REGISTRY;

    let config = RegistrationConfig::default()
        .with_id_prefix("test2_multi")
        .with_source_tag("integration-test");
    let manager = ToolRegistrationManager::new(config);

    let source_url = "http://multi-server-multi-2:3000";

    // Register multiple tools
    let schemas = vec![
        ToolSchema::new("tool_one_multi", "First tool").with_tag("validation"),
        ToolSchema::new("tool_two_multi", "Second tool").with_tag("generation"),
        ToolSchema::new("tool_three_multi", "Third tool").with_tag("analysis"),
    ];

    for schema in schemas {
        manager
            .register_tool(source_url, schema)
            .await
            .expect("Tool registration should succeed");
    }

    // Get initial registry count
    let initial_count = {
        let registry = REGISTRY.read().unwrap();
        registry.count()
    };

    // Register all to core registry
    let agent_count = manager
        .register_a2a_agents(source_url)
        .await
        .expect("A2A agent registration should succeed");

    assert_eq!(agent_count, 3);

    // Verify new tools are in core registry (at least 3 more than before)
    let registry = REGISTRY.read().unwrap();
    assert!(registry.count() >= initial_count + 3);
}

/// Test that unregistration cleans up tracking
#[cfg(feature = "ggen-ai")]
#[tokio::test]
async fn test_unregister_a2a_agents_from_source() {
    let config = RegistrationConfig::default()
        .with_id_prefix("test3")
        .with_source_tag("integration-test");
    let manager = ToolRegistrationManager::new(config);

    let source_url = "http://cleanup-server-3:3000";

    // Register tools
    let schema = ToolSchema::new("cleanup_tool", "Tool for cleanup test");
    manager
        .register_tool(source_url, schema)
        .await
        .expect("Tool registration should succeed");

    // Register to core
    let registered = manager
        .register_a2a_agents(source_url)
        .await
        .expect("A2A agent registration should succeed");
    assert_eq!(registered, 1);

    // Unregister
    let unregistered = manager
        .unregister_a2a_agents(source_url)
        .await
        .expect("A2A agent unregistration should succeed");
    assert_eq!(unregistered, 1);

    // Verify statistics updated
    let stats = manager.statistics().await;
    assert_eq!(stats.agents_registered, 0);
}

/// Test that source tracking works correctly
#[cfg(feature = "ggen-ai")]
#[tokio::test]
async fn test_source_tracking_for_core_registry() {
    let config = RegistrationConfig::default()
        .with_id_prefix("test4")
        .with_source_tag("integration-test");
    let manager = ToolRegistrationManager::new(config);

    let source1 = "http://source-one-4:3000";
    let source2 = "http://source-two-4:3000";

    // Register tools from different sources
    let schema1 = ToolSchema::new("source1_tool", "Tool from source 1");
    manager
        .register_tool(source1, schema1)
        .await
        .expect("Tool registration should succeed");

    let schema2 = ToolSchema::new("source2_tool", "Tool from source 2");
    manager
        .register_tool(source2, schema2)
        .await
        .expect("Tool registration should succeed");

    // Register both to core
    let count1 = manager
        .register_a2a_agents(source1)
        .await
        .expect("A2A agent registration should succeed");
    assert_eq!(count1, 1);

    let count2 = manager
        .register_a2a_agents(source2)
        .await
        .expect("A2A agent registration should succeed");
    assert_eq!(count2, 1);

    // Verify source info
    let source1_info = manager.source_info(source1).await;
    assert!(source1_info.is_some());
    assert_eq!(source1_info.unwrap().tool_count, 1);

    let source2_info = manager.source_info(source2).await;
    assert!(source2_info.is_some());
    assert_eq!(source2_info.unwrap().tool_count, 1);
}

/// Test error handling when no tools are registered for source
#[tokio::test]
async fn test_register_a2a_agents_empty_source_error() {
    let config = RegistrationConfig::default()
        .with_id_prefix("test5")
        .with_source_tag("integration-test");
    let manager = ToolRegistrationManager::new(config);

    let source_url = "http://empty-server-5:3000";

    // Try to register from a source with no tools
    let result = manager.register_a2a_agents(source_url).await;

    assert!(result.is_err());
    match result {
        Err(RegistrationError::A2aRegistration(msg)) => {
            assert!(msg.contains("No tools found"));
        }
        _ => panic!("Expected A2aRegistration error"),
    }
}

/// Test that duplicate detection works
#[tokio::test]
async fn test_duplicate_tool_detection_in_core_registry() {
    let config = RegistrationConfig::default()
        .with_id_prefix("test6")
        .with_source_tag("integration-test");
    let manager = ToolRegistrationManager::new(config);

    let source_url = "http://duplicate-server-6:3000";

    // Register a tool
    let schema = ToolSchema::new("unique_tool", "Unique tool");
    manager
        .register_tool(source_url, schema.clone())
        .await
        .expect("Tool registration should succeed");

    // Try to register again with same source (should update, not error)
    manager
        .register_tool(source_url, schema)
        .await
        .expect("Re-registration with same source should succeed");

    // Verify only one tool in local store
    assert_eq!(manager.tool_count().await, 1);

    // Register to core registry
    #[cfg(feature = "ggen-ai")]
    let count = manager
        .register_a2a_agents(source_url)
        .await
        .expect("A2A agent registration should succeed");

    // Only one unique tool should be registered
    #[cfg(feature = "ggen-ai")]
    assert_eq!(count, 1);
}

/// Test that registration works without ggen-ai feature
#[tokio::test]
async fn test_registration_standalone_without_ggen_ai() {
    let config = RegistrationConfig::default()
        .with_id_prefix("test7")
        .with_source_tag("standalone-test");
    let manager = ToolRegistrationManager::new(config);

    let schema = ToolSchema::new("standalone_tool", "Tool without ggen-ai");

    let result = manager.register_tool("http://standalone-7:3000", schema).await;

    assert!(result.is_ok());

    // Verify tool is in local store
    let tool_id = result.unwrap();
    assert!(manager.is_registered(&tool_id).await);
    assert_eq!(manager.tool_count().await, 1);
}

/// Test that statistics are tracked correctly
#[tokio::test]
async fn test_registration_statistics_tracking() {
    let config = RegistrationConfig::default()
        .with_id_prefix("test8")
        .with_source_tag("stats-test");
    let manager = ToolRegistrationManager::new(config);

    let source_url = "http://stats-server-8:3000";

    // Register a few tools
    for i in 1..=3 {
        let schema = ToolSchema::new(&format!("tool_{}", i), "Tool for stats");
        manager
            .register_tool(source_url, schema)
            .await
            .expect("Tool registration should succeed");
    }

    // Verify local statistics
    let stats = manager.statistics().await;
    assert_eq!(stats.total_registered, 3);
    assert_eq!(stats.total_unregistered, 0);
    assert!(stats.first_registration.is_some());
    assert!(stats.last_registration.is_some());
}
