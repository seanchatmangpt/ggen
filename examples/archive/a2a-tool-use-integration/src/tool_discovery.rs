//! Tool discovery from MCP servers
//!
//! Discovers available tools and parses their schemas,
//! then matches tools to agent goals.

use serde::{Deserialize, Serialize};
use std::collections::BTreeMap;
use uuid::Uuid;

#[derive(Debug, Clone, PartialEq, Eq, Hash, Serialize, Deserialize)]
pub enum ToolCategory {
    CodeGeneration,
    DataProcessing,
    Validation,
    Research,
    Analysis,
    Transformation,
}

impl std::fmt::Display for ToolCategory {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::CodeGeneration => write!(f, "CodeGeneration"),
            Self::DataProcessing => write!(f, "DataProcessing"),
            Self::Validation => write!(f, "Validation"),
            Self::Research => write!(f, "Research"),
            Self::Analysis => write!(f, "Analysis"),
            Self::Transformation => write!(f, "Transformation"),
        }
    }
}

/// Represents an available tool from MCP
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct Tool {
    pub id: String,
    pub name: String,
    pub description: String,
    pub category: ToolCategory,
    pub input_schema: serde_json::Value,
    pub output_schema: serde_json::Value,
    pub required_params: Vec<String>,
    pub optional_params: Vec<String>,
    pub success_rate: f32,
    pub avg_execution_time_ms: u64,
}

impl Tool {
    /// Create a new tool definition
    pub fn new(
        name: String,
        description: String,
        category: ToolCategory,
    ) -> Self {
        Self {
            id: Uuid::new_v4().to_string(),
            name,
            description,
            category,
            input_schema: serde_json::json!({}),
            output_schema: serde_json::json!({}),
            required_params: Vec::new(),
            optional_params: Vec::new(),
            success_rate: 0.85,
            avg_execution_time_ms: 1000,
        }
    }

    /// Set input schema
    pub fn with_input_schema(mut self, schema: serde_json::Value) -> Self {
        self.input_schema = schema;
        self
    }

    /// Set output schema
    pub fn with_output_schema(mut self, schema: serde_json::Value) -> Self {
        self.output_schema = schema;
        self
    }

    /// Set required parameters
    pub fn with_required_params(mut self, params: Vec<String>) -> Self {
        self.required_params = params;
        self
    }

    /// Set optional parameters
    pub fn with_optional_params(mut self, params: Vec<String>) -> Self {
        self.optional_params = params;
        self
    }

    /// Set success rate
    pub fn with_success_rate(mut self, rate: f32) -> Self {
        self.success_rate = (rate).clamp(0.0, 1.0);
        self
    }

    /// Set average execution time
    pub fn with_execution_time_ms(mut self, time_ms: u64) -> Self {
        self.avg_execution_time_ms = time_ms;
        self
    }
}

/// Tool discovery mechanism
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct ToolDiscovery {
    pub pattern: String,
    pub ranking_criteria: BTreeMap<String, f32>,
}

impl ToolDiscovery {
    /// Create a new tool discovery pattern
    pub fn new(pattern: String) -> Self {
        Self {
            pattern,
            ranking_criteria: BTreeMap::new(),
        }
    }

    /// Discover tools matching the pattern
    pub fn discover(&self, all_tools: &[Tool]) -> Vec<Tool> {
        let regex = match regex::Regex::new(&self.pattern) {
            Ok(r) => r,
            Err(_) => {
                // Fallback to simple string matching if regex fails
                return all_tools
                    .iter()
                    .filter(|t| {
                        t.name.contains(&self.pattern) || t.description.contains(&self.pattern)
                    })
                    .cloned()
                    .collect();
            }
        };

        all_tools
            .iter()
            .filter(|t| regex.is_match(&t.name) || regex.is_match(&t.description))
            .cloned()
            .collect()
    }

    /// Rank tools by relevance criteria
    pub fn rank_tools(&self, tools: &[Tool]) -> Vec<(Tool, f32)> {
        let mut ranked = tools
            .iter()
            .map(|tool| {
                let mut score = tool.success_rate;
                
                // Apply ranking criteria weights
                if let Some(&weight) = self.ranking_criteria.get("success_rate") {
                    score = tool.success_rate * weight;
                }
                if let Some(&weight) = self.ranking_criteria.get("execution_time") {
                    // Lower execution time = higher score
                    let time_score = 1.0 - (tool.avg_execution_time_ms as f32 / 10000.0).min(1.0);
                    score += time_score * weight;
                }

                (tool.clone(), score)
            })
            .collect::<Vec<_>>();

        ranked.sort_by(|a, b| {
            b.1.partial_cmp(&a.1).unwrap_or(std::cmp::Ordering::Equal)
        });

        ranked
    }
}

/// Tool registry for managing available tools
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct ToolRegistry {
    pub tools: BTreeMap<String, Tool>,
}

impl ToolRegistry {
    /// Create a new empty registry
    pub fn new() -> Self {
        Self {
            tools: BTreeMap::new(),
        }
    }

    /// Register a tool
    pub fn register(&mut self, tool: Tool) {
        self.tools.insert(tool.name.clone(), tool);
    }

    /// Get tool by name
    pub fn get(&self, name: &str) -> Option<&Tool> {
        self.tools.get(name)
    }

    /// Get all tools in a category
    pub fn by_category(&self, category: &ToolCategory) -> Vec<&Tool> {
        self.tools
            .values()
            .filter(|t| &t.category == category)
            .collect()
    }

    /// List all available tool names
    pub fn list_tools(&self) -> Vec<String> {
        self.tools.keys().cloned().collect()
    }

    /// Find tools compatible with multiple categories
    pub fn find_by_categories(&self, categories: &[ToolCategory]) -> Vec<&Tool> {
        self.tools
            .values()
            .filter(|t| categories.contains(&t.category))
            .collect()
    }
}

impl Default for ToolRegistry {
    fn default() -> Self {
        Self::new()
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_tool_creation() {
        let tool = Tool::new(
            "code-generator".to_string(),
            "Generates code from descriptions".to_string(),
            ToolCategory::CodeGeneration,
        );
        assert_eq!(tool.name, "code-generator");
        assert_eq!(tool.category, ToolCategory::CodeGeneration);
    }

    #[test]
    fn test_tool_builder() {
        let tool = Tool::new(
            "code-validator".to_string(),
            "Validates code".to_string(),
            ToolCategory::Validation,
        )
        .with_success_rate(0.95)
        .with_execution_time_ms(500)
        .with_required_params(vec!["code".to_string(), "language".to_string()]);

        assert_eq!(tool.success_rate, 0.95);
        assert_eq!(tool.avg_execution_time_ms, 500);
        assert_eq!(tool.required_params.len(), 2);
    }

    #[test]
    fn test_tool_discovery() {
        let discovery = ToolDiscovery::new("code.*".to_string());
        let tools = vec![
            Tool::new("code-gen".to_string(), "test".to_string(), ToolCategory::CodeGeneration),
            Tool::new("code-val".to_string(), "test".to_string(), ToolCategory::Validation),
            Tool::new("data-proc".to_string(), "test".to_string(), ToolCategory::DataProcessing),
        ];

        let found = discovery.discover(&tools);
        assert_eq!(found.len(), 2);
    }

    #[test]
    fn test_tool_registry() {
        let mut registry = ToolRegistry::new();
        let tool = Tool::new("code-gen".to_string(), "test".to_string(), ToolCategory::CodeGeneration);
        registry.register(tool.clone());

        assert!(registry.get("code-gen").is_some());
        assert_eq!(registry.list_tools().len(), 1);
    }

    #[test]
    fn test_tool_ranking() {
        let mut discovery = ToolDiscovery::new(".*".to_string());
        discovery.ranking_criteria.insert("success_rate".to_string(), 0.7);
        discovery.ranking_criteria.insert("execution_time".to_string(), 0.3);

        let mut tool1 = Tool::new("t1".to_string(), "test".to_string(), ToolCategory::CodeGeneration);
        tool1.success_rate = 0.9;
        tool1.avg_execution_time_ms = 1000;

        let mut tool2 = Tool::new("t2".to_string(), "test".to_string(), ToolCategory::CodeGeneration);
        tool2.success_rate = 0.8;
        tool2.avg_execution_time_ms = 500;

        let ranked = discovery.rank_tools(&[tool1, tool2]);
        assert!(!ranked.is_empty());
    }
}
