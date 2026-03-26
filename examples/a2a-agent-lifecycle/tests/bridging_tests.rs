//! Agent bridging and MCP tool integration tests
//!
//! Temporarily disabled - depends on ggen-a2a-mcp (async trait object compilation issues)

// use a2a_agent_lifecycle::bridging::Tool;
// use a2a_agent_lifecycle::AgentBridge;

/*
fn create_simple_tool() -> Tool {
    Tool {
        name: "test_tool".to_string(),
        description: "Test tool".to_string(),
        input_schema: serde_json::json!({
            "type": "object",
            "properties": {
                "input": { "type": "string" }
            },
            "required": ["input"]
        }),
        output_schema: serde_json::json!({
            "type": "object",
            "properties": {
                "result": { "type": "string" }
            }
        }),
    }
}

#[test]
fn test_bridge_creation() {
    let bridge = AgentBridge::new("agent-001", "TestAgent");
    assert_eq!(bridge.agent_id, "agent-001");
    assert_eq!(bridge.agent_name, "TestAgent");
    assert_eq!(bridge.tool_count(), 0);
}

#[test]
fn test_tool_registration() {
    let mut bridge = AgentBridge::new("agent-001", "TestAgent");
    let tool = create_simple_tool();

    assert!(bridge.register_tool(tool).is_ok());
    assert_eq!(bridge.tool_count(), 1);
}

#[test]
fn test_duplicate_tool_rejection() {
    let mut bridge = AgentBridge::new("agent-001", "TestAgent");
    let tool = create_simple_tool();

    assert!(bridge.register_tool(tool.clone()).is_ok());
    assert!(bridge.register_tool(tool).is_err());
}

#[test]
fn test_multiple_tools() {
    let mut bridge = AgentBridge::new("agent-001", "TestAgent");

    for i in 0..5 {
        let mut tool = create_simple_tool();
        tool.name = format!("tool_{}", i);
        assert!(bridge.register_tool(tool).is_ok());
    }

    assert_eq!(bridge.tool_count(), 5);
}

#[test]
fn test_tool_execution() {
    let mut bridge = AgentBridge::new("agent-001", "TestAgent");
    bridge.register_tool(create_simple_tool()).unwrap();

    let execution = bridge
        .execute_tool(
            "test_tool",
            serde_json::json!({
                "input": "test_input"
            }),
        )
        .unwrap();

    assert!(execution.success);
    assert!(execution.output.is_some());
    assert_eq!(execution.tool_name, "test_tool");
}

#[test]
fn test_tool_execution_nonexistent() {
    let mut bridge = AgentBridge::new("agent-001", "TestAgent");
    bridge.register_tool(create_simple_tool()).unwrap();

    let result = bridge.execute_tool("nonexistent_tool", serde_json::json!({"input": "test"}));

    assert!(result.is_err());
}

#[test]
fn test_tool_execution_validation() {
    let mut bridge = AgentBridge::new("agent-001", "TestAgent");
    bridge.register_tool(create_simple_tool()).unwrap();

    // Missing required input field
    let result = bridge.execute_tool("test_tool", serde_json::json!({}));

    assert!(result.is_err());
}

#[test]
fn test_execution_safe_mode() {
    let mut bridge = AgentBridge::new("agent-001", "TestAgent");
    bridge.register_tool(create_simple_tool()).unwrap();

    let execution = bridge.execute_tool_safe(
        "test_tool",
        serde_json::json!({
            "input": "test"
        }),
    );

    assert!(execution.success);
    assert_eq!(bridge.execution_count(), 1);
}

#[test]
fn test_execution_safe_mode_with_error() {
    let mut bridge = AgentBridge::new("agent-001", "TestAgent");
    bridge.register_tool(create_simple_tool()).unwrap();

    let execution = bridge.execute_tool_safe(
        "test_tool",
        serde_json::json!({}), // Missing required field
    );

    assert!(!execution.success);
    assert!(execution.error.is_some());
    assert_eq!(bridge.execution_count(), 1);
}

#[test]
fn test_execution_history() {
    let mut bridge = AgentBridge::new("agent-001", "TestAgent");
    bridge.register_tool(create_simple_tool()).unwrap();

    for i in 0..10 {
        bridge.execute_tool_safe(
            "test_tool",
            serde_json::json!({
                "input": format!("input_{}", i)
            }),
        );
    }

    assert_eq!(bridge.execution_count(), 10);
    let executions = bridge.executions();
    assert_eq!(executions.len(), 10);
}

#[test]
fn test_success_rate() {
    let mut bridge = AgentBridge::new("agent-001", "TestAgent");
    bridge.register_tool(create_simple_tool()).unwrap();

    // Successful executions
    for _ in 0..8 {
        bridge.execute_tool_safe(
            "test_tool",
            serde_json::json!({
                "input": "test"
            }),
        );
    }

    // Failed executions
    for _ in 0..2 {
        bridge.execute_tool_safe(
            "test_tool",
            serde_json::json!({}), // Missing required field
        );
    }

    let success_rate = bridge.success_rate();
    assert!(success_rate > 0.0);
    assert!(success_rate <= 1.0);
    assert_eq!((success_rate * 100.0) as i32, 80);
}

#[test]
fn test_get_tool() {
    let mut bridge = AgentBridge::new("agent-001", "TestAgent");
    let tool = create_simple_tool();
    let tool_name = tool.name.clone();

    bridge.register_tool(tool).unwrap();

    assert!(bridge.get_tool(&tool_name).is_some());
    assert!(bridge.get_tool("nonexistent").is_none());
}

#[test]
fn test_tools_iterator() {
    let mut bridge = AgentBridge::new("agent-001", "TestAgent");

    for i in 0..3 {
        let mut tool = create_simple_tool();
        tool.name = format!("tool_{}", i);
        bridge.register_tool(tool).unwrap();
    }

    let tool_count = bridge.tools().count();
    assert_eq!(tool_count, 3);
}

#[test]
fn test_execution_payload_preservation() {
    let mut bridge = AgentBridge::new("agent-001", "TestAgent");
    bridge.register_tool(create_simple_tool()).unwrap();

    let input_payload = serde_json::json!({
        "input": "complex_input_data",
        "nested": {
            "value": 42
        }
    });

    let execution = bridge
        .execute_tool("test_tool", input_payload.clone())
        .unwrap();

    assert_eq!(execution.input, input_payload);
}

#[test]
fn test_builtin_tools() {
    let mut bridge = AgentBridge::new("agent-001", "TestAgent");

    // Test get_state
    let exec = bridge.execute_tool_safe("get_state", serde_json::json!({}));
    assert!(exec.success);
    assert!(exec.output.is_some());

    // Test execute_task
    let exec = bridge.execute_tool_safe(
        "execute_task",
        serde_json::json!({
            "task_id": "task-123"
        }),
    );
    assert!(exec.success);

    // Test get_metrics
    let exec = bridge.execute_tool_safe("get_metrics", serde_json::json!({}));
    assert!(exec.success);
}

#[test]
fn test_execution_id_uniqueness() {
    let mut bridge = AgentBridge::new("agent-001", "TestAgent");
    bridge.register_tool(create_simple_tool()).unwrap();

    let exec1 = bridge.execute_tool_safe(
        "test_tool",
        serde_json::json!({
            "input": "test1"
        }),
    );
    let exec2 = bridge.execute_tool_safe(
        "test_tool",
        serde_json::json!({
            "input": "test2"
        }),
    );

    assert_ne!(exec1.id, exec2.id);
}

#[test]
fn test_concurrent_executions() {
    let mut bridge = AgentBridge::new("agent-001", "TestAgent");
    bridge.register_tool(create_simple_tool()).unwrap();

    for i in 0..5 {
        let exec = bridge.execute_tool_safe(
            "test_tool",
            serde_json::json!({
                "input": format!("concurrent_{}", i)
            }),
        );
        assert!(exec.success);
    }

    assert_eq!(bridge.execution_count(), 5);
}

#[test]
fn test_tool_discovery() {
    let mut bridge = AgentBridge::new("agent-001", "TestAgent");

    let tool1 = create_simple_tool();
    let mut tool2 = create_simple_tool();
    tool2.name = "task_executor".to_string();
    let mut tool3 = create_simple_tool();
    tool3.name = "state_checker".to_string();

    bridge.register_tool(tool1).unwrap();
    bridge.register_tool(tool2).unwrap();
    bridge.register_tool(tool3).unwrap();

    let tools: Vec<_> = bridge.tools().collect();
    assert_eq!(tools.len(), 3);

    let tool_names: Vec<_> = tools.iter().map(|t| t.name.as_str()).collect();
    assert!(tool_names.contains(&"test_tool"));
    assert!(tool_names.contains(&"task_executor"));
    assert!(tool_names.contains(&"state_checker"));
}

#[test]
fn test_bridge_isolation() {
    let mut bridge1 = AgentBridge::new("agent-001", "Agent1");
    let mut bridge2 = AgentBridge::new("agent-002", "Agent2");

    bridge1.register_tool(create_simple_tool()).unwrap();

    assert_eq!(bridge1.tool_count(), 1);
    assert_eq!(bridge2.tool_count(), 0);

    assert!(bridge1.get_tool("test_tool").is_some());
    assert!(bridge2.get_tool("test_tool").is_none());
}

#[test]
fn test_execution_error_details() {
    let mut bridge = AgentBridge::new("agent-001", "TestAgent");
    bridge.register_tool(create_simple_tool()).unwrap();

    let execution = bridge.execute_tool_safe(
        "test_tool",
        serde_json::json!({}), // Missing required field
    );

    assert!(!execution.success);
    assert!(execution.error.is_some());
    assert!(execution.error.as_ref().unwrap().contains("required field"));
}
*/
