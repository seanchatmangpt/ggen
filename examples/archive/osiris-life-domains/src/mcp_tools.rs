//! MCP Tool Integration
//!
//! Provides tool definitions and execution for autonomous agents

use serde::{Deserialize, Serialize};
use std::collections::HashMap;
use std::sync::Arc;
use tokio::sync::RwLock;
use tracing::info;

/// MCP Tool definition
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct MCPTool {
    pub name: String,
    pub domain: String,
    pub description: String,
    pub inputs: Vec<String>,
    pub outputs: Vec<String>,
}

impl MCPTool {
    /// Create a new MCP tool
    pub fn new(
        name: &str,
        domain: &str,
        description: &str,
        inputs: Vec<String>,
        outputs: Vec<String>,
    ) -> Self {
        Self {
            name: name.to_string(),
            domain: domain.to_string(),
            description: description.to_string(),
            inputs,
            outputs,
        }
    }
}

/// MCP Tool Registry - agents discover and use tools
pub struct MCPToolRegistry {
    tools: Arc<RwLock<HashMap<String, MCPTool>>>,
}

impl MCPToolRegistry {
    /// Create new tool registry
    pub fn new() -> Self {
        let mut tools = HashMap::new();

        // Health tools
        tools.insert(
            "recommend_workout".to_string(),
            MCPTool::new(
                "recommend_workout",
                "health",
                "Recommend workout based on fitness level",
                vec!["fitness_level".to_string(), "duration".to_string()],
                vec!["workout_plan".to_string()],
            ),
        );
        tools.insert(
            "plan_meals".to_string(),
            MCPTool::new(
                "plan_meals",
                "health",
                "Plan healthy meals",
                vec!["dietary_preferences".to_string()],
                vec!["meal_plan".to_string()],
            ),
        );

        // Career tools
        tools.insert(
            "search_jobs".to_string(),
            MCPTool::new(
                "search_jobs",
                "career",
                "Search for job opportunities",
                vec!["skills".to_string(), "location".to_string()],
                vec!["job_listings".to_string()],
            ),
        );
        tools.insert(
            "assess_skills".to_string(),
            MCPTool::new(
                "assess_skills",
                "career",
                "Assess current skills",
                vec!["domain".to_string()],
                vec!["skill_assessment".to_string()],
            ),
        );

        // Finance tools
        tools.insert(
            "create_budget".to_string(),
            MCPTool::new(
                "create_budget",
                "finance",
                "Create monthly budget",
                vec!["income".to_string(), "expenses".to_string()],
                vec!["budget_plan".to_string()],
            ),
        );
        tools.insert(
            "analyze_investments".to_string(),
            MCPTool::new(
                "analyze_investments",
                "finance",
                "Analyze investment portfolio",
                vec!["portfolio".to_string()],
                vec!["analysis".to_string()],
            ),
        );

        // Learning tools
        tools.insert(
            "recommend_courses".to_string(),
            MCPTool::new(
                "recommend_courses",
                "learning",
                "Recommend online courses",
                vec!["skill_gap".to_string()],
                vec!["course_list".to_string()],
            ),
        );
        tools.insert(
            "generate_reading_list".to_string(),
            MCPTool::new(
                "generate_reading_list",
                "learning",
                "Generate reading list",
                vec!["topic".to_string()],
                vec!["reading_list".to_string()],
            ),
        );

        // Spirituality tools
        tools.insert(
            "guide_meditation".to_string(),
            MCPTool::new(
                "guide_meditation",
                "spirituality",
                "Guide meditation session",
                vec!["duration".to_string(), "style".to_string()],
                vec!["guided_session".to_string()],
            ),
        );
        tools.insert(
            "reflection_prompts".to_string(),
            MCPTool::new(
                "reflection_prompts",
                "spirituality",
                "Generate reflection prompts",
                vec!["topic".to_string()],
                vec!["prompts".to_string()],
            ),
        );

        Self {
            tools: Arc::new(RwLock::new(tools)),
        }
    }

    /// Get tools available for a domain
    pub async fn get_domain_tools(&self, domain: &str) -> Vec<MCPTool> {
        let tools = self.tools.read().await;
        tools
            .values()
            .filter(|t| t.domain == domain)
            .cloned()
            .collect()
    }

    /// Discover tools available
    pub async fn discover_tools(&self) -> Vec<MCPTool> {
        let tools = self.tools.read().await;
        tools.values().cloned().collect()
    }

    /// Execute a tool action
    pub async fn execute_action(&self, action: &str) -> anyhow::Result<String> {
        info!("Executing MCP tool action: {}", action);
        
        // Simulate tool execution
        let result = match action {
            "workout" => "30-minute cardio workout completed".to_string(),
            "sleep_8hrs" => "8 hours of quality sleep achieved".to_string(),
            "meditation" => "20-minute guided meditation completed".to_string(),
            "learn_skill" => "New programming skill acquired".to_string(),
            "complete_course" => "Online course completed successfully".to_string(),
            "quality_time" => "Quality time with family scheduled".to_string(),
            "increase_savings" => "Monthly savings increased by 10%".to_string(),
            "invest" => "Portfolio rebalanced and optimized".to_string(),
            "meditate_20min" => "20-minute mindfulness meditation completed".to_string(),
            "reflection" => "Personal reflection session completed".to_string(),
            _ => format!("Action {} executed", action),
        };
        
        Ok(result)
    }

    /// Get tool by name
    pub async fn get_tool(&self, name: &str) -> Option<MCPTool> {
        let tools = self.tools.read().await;
        tools.get(name).cloned()
    }

    /// Register custom tool
    pub async fn register_tool(&self, tool: MCPTool) -> anyhow::Result<()> {
        let mut tools = self.tools.write().await;
        tools.insert(tool.name.clone(), tool);
        Ok(())
    }
}

impl Default for MCPToolRegistry {
    fn default() -> Self {
        Self::new()
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[tokio::test]
    async fn test_tool_registry_creation() {
        let registry = MCPToolRegistry::new();
        let tools = registry.discover_tools().await;
        assert!(!tools.is_empty());
    }

    #[tokio::test]
    async fn test_get_domain_tools() {
        let registry = MCPToolRegistry::new();
        let health_tools = registry.get_domain_tools("health").await;
        assert!(!health_tools.is_empty());
        assert!(health_tools.iter().all(|t| t.domain == "health"));
    }

    #[tokio::test]
    async fn test_execute_action() {
        let registry = MCPToolRegistry::new();
        let result = registry.execute_action("workout").await;
        assert!(result.is_ok());
        assert!(result.unwrap().contains("workout"));
    }

    #[tokio::test]
    async fn test_get_tool() {
        let registry = MCPToolRegistry::new();
        let tool = registry.get_tool("recommend_workout").await;
        assert!(tool.is_some());
        assert_eq!(tool.unwrap().domain, "health");
    }
}
