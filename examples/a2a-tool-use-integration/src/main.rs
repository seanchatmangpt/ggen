//! A2A Tool Use Integration Example
//!
//! Demonstrates autonomous agent tool discovery and use with multiple agents
//! discovering tools, planning tool sequences, executing them, and learning
//! from results.

use a2a_tool_use_integration::{
    agents::{AgentBase, AgentConfig, CodeAgent, DataAgent, ResearchAgent},
    agents::base::AgentType,
    Agent,
    execution::MockExecutor,
    tool_discovery::{Tool, ToolCategory, ToolRegistry},
};
use std::sync::Arc;

#[tokio::main]
async fn main() -> Result<(), Box<dyn std::error::Error>> {
    println!("=== A2A Tool Use Integration Example ===\n");

    // Create tool registry with common tools
    let mut registry = ToolRegistry::new();
    setup_tools(&mut registry);

    // Create a mock executor for demonstration
    let executor = Arc::new(
        MockExecutor::new()
            .set_response("code-generate".to_string(), Ok("pub fn main() {}".to_string()))
            .set_response("code-validate".to_string(), Ok("✓ Valid Rust syntax".to_string()))
            .set_response("code-format".to_string(), Ok("pub fn main() {}\n".to_string()))
            .set_response("research-search".to_string(), Ok("Research findings: ...".to_string()))
            .set_response("data-analyze".to_string(), Ok("Analysis complete".to_string())),
    );

    // Example 1: Code Agent
    println!("--- Code Agent Example ---");
    demonstrate_code_agent(executor.clone(), &registry).await;

    // Example 2: Research Agent
    println!("\n--- Research Agent Example ---");
    demonstrate_research_agent(executor.clone(), &registry).await;

    // Example 3: Data Agent
    println!("\n--- Data Agent Example ---");
    demonstrate_data_agent(executor.clone(), &registry).await;

    Ok(())
}

fn setup_tools(registry: &mut ToolRegistry) {
    // Code generation tools
    registry.register(
        Tool::new(
            "code-generate".to_string(),
            "Generates code from description".to_string(),
            ToolCategory::CodeGeneration,
        )
        .with_success_rate(0.95),
    );

    registry.register(
        Tool::new(
            "code-validate".to_string(),
            "Validates code syntax".to_string(),
            ToolCategory::Validation,
        )
        .with_success_rate(0.98),
    );

    registry.register(
        Tool::new(
            "code-format".to_string(),
            "Formats code".to_string(),
            ToolCategory::Transformation,
        )
        .with_success_rate(0.99),
    );

    // Research tools
    registry.register(
        Tool::new(
            "research-search".to_string(),
            "Searches for information".to_string(),
            ToolCategory::Research,
        )
        .with_success_rate(0.85),
    );

    // Data analysis tools
    registry.register(
        Tool::new(
            "data-analyze".to_string(),
            "Analyzes data".to_string(),
            ToolCategory::Analysis,
        )
        .with_success_rate(0.90),
    );

    println!("  Registered {} tools", registry.list_tools().len());
}

async fn demonstrate_code_agent(
    executor: Arc<MockExecutor>,
    registry: &ToolRegistry,
) {
    let config = AgentConfig::new("CodeAgent".to_string(), AgentType::Code);
    let mut agent = AgentBase::new(config, executor);

    // Register tools
    for tool_name in ["code-generate", "code-validate", "code-format"] {
        if let Some(tool) = registry.get(tool_name) {
            agent.register_tool(tool.clone());
        }
    }

    let code_agent = CodeAgent::new(agent);

    println!("  Agent: {}", code_agent.config().name);
    println!("  Type: {}", code_agent.config().agent_type);

    let result = code_agent
        .generate_code("Create a hello world function", "rust")
        .await;

    println!("  Goal: Generate Rust code");
    println!("  Success: {}", result.success);
    println!("  Quality Score: {}/100", result.quality_score());
    if let Some(code) = result.generated_code {
        println!("  Generated: {}", code);
    }
}

async fn demonstrate_research_agent(
    executor: Arc<MockExecutor>,
    registry: &ToolRegistry,
) {
    let config = AgentConfig::new("ResearchAgent".to_string(), AgentType::Research);
    let mut agent = AgentBase::new(config, executor);

    // Register tools
    if let Some(tool) = registry.get("research-search") {
        agent.register_tool(tool.clone());
    }

    let research_agent = ResearchAgent::new(agent);

    println!("  Agent: {}", research_agent.config().name);
    println!("  Type: {}", research_agent.config().agent_type);

    let result = research_agent.investigate("Rust async/await patterns").await;

    println!("  Topic: {}", result.topic);
    println!("  Success: {}", result.success);
    println!("  Sources Consulted: {}", result.sources_consulted);
    if let Some(findings) = result.findings {
        println!("  Findings: {}", findings);
    }
}

async fn demonstrate_data_agent(executor: Arc<MockExecutor>, registry: &ToolRegistry) {
    let config = AgentConfig::new("DataAgent".to_string(), AgentType::Data);
    let mut agent = AgentBase::new(config, executor);

    // Register tools
    if let Some(tool) = registry.get("data-analyze") {
        agent.register_tool(tool.clone());
    }

    let data_agent = DataAgent::new(agent);

    println!("  Agent: {}", data_agent.config().name);
    println!("  Type: {}", data_agent.config().agent_type);

    let result = data_agent
        .analyze("sample data values", "statistical")
        .await;

    println!("  Analysis Type: {}", result.analysis_type);
    println!("  Success: {}", result.success);
    println!("  Steps Executed: {}", result.steps_executed);
    if let Some(insights) = result.insights {
        println!("  Insights: {}", insights);
    }
}
