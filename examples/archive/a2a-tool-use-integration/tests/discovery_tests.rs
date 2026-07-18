//! Tool discovery tests
//!
//! Tests for tool discovery, schema parsing, and tool ranking.

use a2a_tool_use_integration::tool_discovery::{Tool, ToolCategory, ToolDiscovery, ToolRegistry};

#[test]
fn test_tool_discovery_by_pattern() {
    let discovery = ToolDiscovery::new("code.*".to_string());
    let tools = vec![
        Tool::new("code-generator".to_string(), "generates code".to_string(), ToolCategory::CodeGeneration),
        Tool::new("code-validator".to_string(), "validates code".to_string(), ToolCategory::Validation),
        Tool::new("data-analyzer".to_string(), "analyzes data".to_string(), ToolCategory::Analysis),
    ];

    let found = discovery.discover(&tools);
    assert_eq!(found.len(), 2);
    assert!(found.iter().any(|t| t.name == "code-generator"));
    assert!(found.iter().any(|t| t.name == "code-validator"));
}

#[test]
fn test_tool_discovery_case_insensitive() {
    let discovery = ToolDiscovery::new("(?i)code.*".to_string());
    let tools = vec![
        Tool::new("Code-Generator".to_string(), "test".to_string(), ToolCategory::CodeGeneration),
        Tool::new("CODE_FORMATTER".to_string(), "test".to_string(), ToolCategory::Transformation),
    ];

    let found = discovery.discover(&tools);
    assert!(found.len() >= 1);
}

#[test]
fn test_tool_ranking() {
    let mut discovery = ToolDiscovery::new(".*".to_string());
    discovery.ranking_criteria.insert("success_rate".to_string(), 1.0);

    let mut tool1 = Tool::new("t1".to_string(), "test".to_string(), ToolCategory::CodeGeneration);
    tool1.success_rate = 0.95;

    let mut tool2 = Tool::new("t2".to_string(), "test".to_string(), ToolCategory::CodeGeneration);
    tool2.success_rate = 0.80;

    let ranked = discovery.rank_tools(&[tool1, tool2]);
    assert_eq!(ranked[0].0.name, "t1");
    assert_eq!(ranked[1].0.name, "t2");
}

#[test]
fn test_tool_registry() {
    let mut registry = ToolRegistry::new();
    let tool = Tool::new("gen".to_string(), "test".to_string(), ToolCategory::CodeGeneration);
    registry.register(tool);

    assert!(registry.get("gen").is_some());
    assert_eq!(registry.list_tools().len(), 1);
}

#[test]
fn test_tools_by_category() {
    let mut registry = ToolRegistry::new();
    registry.register(Tool::new("gen".to_string(), "test".to_string(), ToolCategory::CodeGeneration));
    registry.register(Tool::new("val".to_string(), "test".to_string(), ToolCategory::Validation));

    let code_tools = registry.by_category(&ToolCategory::CodeGeneration);
    assert_eq!(code_tools.len(), 1);
    assert_eq!(code_tools[0].name, "gen");
}

#[test]
fn test_find_tools_by_multiple_categories() {
    let mut registry = ToolRegistry::new();
    registry.register(Tool::new("gen".to_string(), "test".to_string(), ToolCategory::CodeGeneration));
    registry.register(Tool::new("val".to_string(), "test".to_string(), ToolCategory::Validation));
    registry.register(Tool::new("ana".to_string(), "test".to_string(), ToolCategory::Analysis));

    let tools = registry.find_by_categories(&[ToolCategory::CodeGeneration, ToolCategory::Validation]);
    assert_eq!(tools.len(), 2);
}

#[test]
fn test_tool_schema_configuration() {
    let tool = Tool::new("gen".to_string(), "test".to_string(), ToolCategory::CodeGeneration)
        .with_input_schema(serde_json::json!({
            "type": "object",
            "properties": {
                "description": { "type": "string" }
            }
        }))
        .with_output_schema(serde_json::json!({
            "type": "object",
            "properties": {
                "code": { "type": "string" }
            }
        }))
        .with_required_params(vec!["description".to_string()])
        .with_success_rate(0.92);

    assert_eq!(tool.required_params.len(), 1);
    assert_eq!(tool.success_rate, 0.92);
}

#[test]
fn test_tool_execution_time_ranking() {
    let mut discovery = ToolDiscovery::new(".*".to_string());
    discovery.ranking_criteria.insert("execution_time".to_string(), 1.0);

    let mut tool1 = Tool::new("fast".to_string(), "test".to_string(), ToolCategory::CodeGeneration);
    tool1.avg_execution_time_ms = 100;

    let mut tool2 = Tool::new("slow".to_string(), "test".to_string(), ToolCategory::CodeGeneration);
    tool2.avg_execution_time_ms = 5000;

    let ranked = discovery.rank_tools(&[tool1, tool2]);
    assert_eq!(ranked[0].0.name, "fast");
}
