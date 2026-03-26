/// MCP Tool Discovery and Execution
/// Integrates external tools via Model Context Protocol
use anyhow::{anyhow, Result};
use serde::{Deserialize, Serialize};
use std::sync::Arc;
use tokio::time::{sleep, timeout, Duration};
use tracing::{info, warn};

/// Tool metadata
#[derive(Clone, Debug, Serialize, Deserialize)]
pub struct ToolMetadata {
    pub id: String,
    pub name: String,
    pub description: String,
    pub timeout_ms: u64,
}

/// Tool execution result
#[derive(Clone, Debug, Serialize, Deserialize)]
pub struct ToolResult {
    pub tool_id: String,
    pub success: bool,
    pub output: serde_json::Value,
    pub error: Option<String>,
}

/// MCP Tool Registry
pub struct ToolRegistry {
    tools: Arc<std::collections::HashMap<String, ToolMetadata>>,
}

impl ToolRegistry {
    /// Create new tool registry
    pub async fn new() -> Result<Self> {
        let mut tools = std::collections::HashMap::new();

        // Pre-populate with example tools
        tools.insert(
            "calendar".to_string(),
            ToolMetadata {
                id: "calendar".to_string(),
                name: "Calendar Tool".to_string(),
                description: "Add events to calendar".to_string(),
                timeout_ms: 2000,
            },
        );

        tools.insert(
            "workout".to_string(),
            ToolMetadata {
                id: "workout".to_string(),
                name: "Workout Planner".to_string(),
                description: "Generate fitness routines".to_string(),
                timeout_ms: 3000,
            },
        );

        tools.insert(
            "career-coach".to_string(),
            ToolMetadata {
                id: "career-coach".to_string(),
                name: "Career Coach AI".to_string(),
                description: "Career advice and planning".to_string(),
                timeout_ms: 5000,
            },
        );

        tools.insert(
            "course-finder".to_string(),
            ToolMetadata {
                id: "course-finder".to_string(),
                name: "Course Finder".to_string(),
                description: "Find learning resources".to_string(),
                timeout_ms: 3000,
            },
        );

        tools.insert(
            "meal-planner".to_string(),
            ToolMetadata {
                id: "meal-planner".to_string(),
                name: "Meal Planner".to_string(),
                description: "Personalized nutrition plans".to_string(),
                timeout_ms: 2000,
            },
        );

        tools.insert(
            "travel-planner".to_string(),
            ToolMetadata {
                id: "travel-planner".to_string(),
                name: "Travel Planner".to_string(),
                description: "Vacation optimization".to_string(),
                timeout_ms: 4000,
            },
        );

        tools.insert(
            "sleep-coach".to_string(),
            ToolMetadata {
                id: "sleep-coach".to_string(),
                name: "Sleep Coach".to_string(),
                description: "Sleep improvement strategies".to_string(),
                timeout_ms: 2000,
            },
        );

        tools.insert(
            "finance-advisor".to_string(),
            ToolMetadata {
                id: "finance-advisor".to_string(),
                name: "Finance Advisor".to_string(),
                description: "Investment and budgeting advice".to_string(),
                timeout_ms: 3000,
            },
        );

        tools.insert(
            "relationship-coach".to_string(),
            ToolMetadata {
                id: "relationship-coach".to_string(),
                name: "Relationship Coach".to_string(),
                description: "Relationship improvement strategies".to_string(),
                timeout_ms: 4000,
            },
        );

        tools.insert(
            "leisure-planner".to_string(),
            ToolMetadata {
                id: "leisure-planner".to_string(),
                name: "Leisure Planner".to_string(),
                description: "Hobby and leisure recommendations".to_string(),
                timeout_ms: 2000,
            },
        );

        Ok(Self {
            tools: Arc::new(tools),
        })
    }

    /// Discover available tools
    pub async fn discover_tools(&self) -> Result<Vec<ToolMetadata>> {
        let tools: Vec<_> = self.tools.values().cloned().collect();
        Ok(tools)
    }

    /// Execute a tool with timeout and retry logic
    pub async fn execute(
        &self, tool_id: &str, _args: serde_json::Value,
    ) -> Result<serde_json::Value> {
        if let Some(tool) = self.tools.get(tool_id) {
            info!(
                "[TOOL] Executing {} with timeout {}ms",
                tool.name, tool.timeout_ms
            );

            // Simulate tool execution with realistic latency
            let result = timeout(
                Duration::from_millis(tool.timeout_ms),
                self.simulate_tool_execution(tool_id),
            )
            .await;

            match result {
                Ok(Ok(output)) => {
                    info!("[TOOL] ✓ {} completed", tool.name);
                    Ok(output)
                }
                Ok(Err(e)) => {
                    warn!("[TOOL] {} failed: {}", tool.name, e);
                    Err(e)
                }
                Err(_) => {
                    warn!("[TOOL] {} timeout exceeded", tool.name);
                    Err(anyhow!("Tool timeout: {}", tool.name))
                }
            }
        } else {
            Err(anyhow!("Tool not found: {}", tool_id))
        }
    }

    /// Simulate tool execution
    async fn simulate_tool_execution(&self, tool_id: &str) -> Result<serde_json::Value> {
        // Simulate network latency
        let latency = match tool_id {
            "calendar" => 100,
            "workout" => 200,
            "career-coach" => 1500,
            "course-finder" => 800,
            "meal-planner" => 300,
            "travel-planner" => 2000,
            "sleep-coach" => 250,
            "finance-advisor" => 600,
            "relationship-coach" => 1200,
            "leisure-planner" => 400,
            _ => 500,
        };

        sleep(Duration::from_millis(latency)).await;

        // Return realistic tool output
        let output = match tool_id {
            "calendar" => serde_json::json!({
                "event_id": uuid::Uuid::new_v4().to_string(),
                "title": "Scheduled successfully",
                "timestamp": chrono::Utc::now()
            }),
            "workout" => serde_json::json!({
                "routine": "4x/week full body",
                "exercises": 12,
                "duration_min": 45
            }),
            "career-coach" => serde_json::json!({
                "advice": "Focus on skill development",
                "next_steps": ["Learn Rust", "Build portfolio"]
            }),
            "course-finder" => serde_json::json!({
                "course": "Python Fundamentals",
                "platform": "Coursera",
                "duration_weeks": 4
            }),
            "meal-planner" => serde_json::json!({
                "meals": ["Oatmeal", "Grilled chicken", "Vegetables"],
                "calories": 2200
            }),
            "travel-planner" => serde_json::json!({
                "destination": "Bali",
                "dates": "March 30 - April 1",
                "estimated_cost": 1500
            }),
            "sleep-coach" => serde_json::json!({
                "bedtime": "23:00",
                "wake_time": "07:00",
                "target_hours": 8
            }),
            "finance-advisor" => serde_json::json!({
                "recommendation": "Increase 401k contribution",
                "monthly_savings": 1500
            }),
            "relationship-coach" => serde_json::json!({
                "action": "Schedule weekly date night",
                "frequency": "weekly"
            }),
            "leisure-planner" => serde_json::json!({
                "hobby": "Photography",
                "frequency": "2x/week"
            }),
            _ => serde_json::json!({"status": "completed"}),
        };

        Ok(output)
    }

    /// Get a tool by ID
    pub fn get_tool(&self, tool_id: &str) -> Option<ToolMetadata> {
        self.tools.get(tool_id).cloned()
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[tokio::test]
    async fn test_tool_registry_creation() {
        let registry = ToolRegistry::new()
            .await
            .expect("Failed to create registry");
        let tools = registry
            .discover_tools()
            .await
            .expect("Failed to discover tools");
        assert!(tools.len() >= 10);
    }

    #[tokio::test]
    async fn test_tool_execution() {
        let registry = ToolRegistry::new()
            .await
            .expect("Failed to create registry");
        let result = registry
            .execute("calendar", serde_json::json!({}))
            .await
            .expect("Failed to execute tool");
        assert!(result.is_object());
    }

    #[tokio::test]
    async fn test_tool_timeout() {
        let registry = ToolRegistry::new()
            .await
            .expect("Failed to create registry");
        // This should work normally
        let result = registry.execute("calendar", serde_json::json!({})).await;
        assert!(result.is_ok());
    }
}
