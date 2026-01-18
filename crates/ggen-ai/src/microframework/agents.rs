//! Pre-built agent types for common tasks
//!
//! These agents integrate with ggen's existing code generation infrastructure.

use super::tasks::{Task, TaskResult, TaskType};
use crate::error::Result;
use async_trait::async_trait;
use serde::{Deserialize, Serialize};
use std::sync::Arc;

/// Agent role classification
#[derive(Debug, Clone, PartialEq, Eq, Hash, Serialize, Deserialize)]
pub enum AgentRole {
    /// Code generation agent
    CodeGenerator,
    /// Testing agent
    Tester,
    /// Code review agent
    Reviewer,
    /// Validation agent
    Validator,
    /// RDF processing agent
    RdfProcessor,
    /// Template generation agent
    TemplateGenerator,
    /// Custom agent
    Custom(String),
}

/// Trait for microframework agents
#[async_trait]
pub trait MicroAgent: Send + Sync + std::fmt::Debug {
    /// Get agent name
    fn name(&self) -> &str;

    /// Get agent role
    fn role(&self) -> AgentRole;

    /// Get supported task types
    fn supported_tasks(&self) -> Vec<TaskType>;

    /// Execute a task
    async fn execute(&self, task: &Task) -> Result<TaskResult>;

    /// Check if agent can handle a task
    fn can_handle(&self, task: &Task) -> bool {
        self.supported_tasks().contains(&task.task_type)
    }
}

/// Code generation agent
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct CodeGenAgent {
    /// Agent name
    pub name: String,
    /// Language targets
    pub languages: Vec<String>,
    /// Configuration
    pub config: CodeGenConfig,
}

/// Code generation configuration
#[derive(Debug, Clone, Default, Serialize, Deserialize)]
pub struct CodeGenConfig {
    /// Include documentation
    pub include_docs: bool,
    /// Include tests
    pub include_tests: bool,
    /// Style guide to follow
    pub style_guide: Option<String>,
}

impl CodeGenAgent {
    /// Create a new code generation agent
    pub fn new(name: &str) -> Self {
        Self {
            name: name.to_string(),
            languages: vec!["rust".to_string()],
            config: CodeGenConfig::default(),
        }
    }

    /// Add supported language
    pub fn with_language(mut self, lang: &str) -> Self {
        self.languages.push(lang.to_string());
        self
    }

    /// Enable documentation generation
    pub fn with_docs(mut self) -> Self {
        self.config.include_docs = true;
        self
    }

    /// Enable test generation
    pub fn with_tests(mut self) -> Self {
        self.config.include_tests = true;
        self
    }
}

impl Default for CodeGenAgent {
    fn default() -> Self {
        Self::new("code-gen")
    }
}

#[async_trait]
impl MicroAgent for CodeGenAgent {
    fn name(&self) -> &str {
        &self.name
    }

    fn role(&self) -> AgentRole {
        AgentRole::CodeGenerator
    }

    fn supported_tasks(&self) -> Vec<TaskType> {
        vec![TaskType::CodeGen, TaskType::TemplateGen]
    }

    async fn execute(&self, task: &Task) -> Result<TaskResult> {
        let start = std::time::Instant::now();

        // Simulate code generation
        let output = serde_json::json!({
            "agent": self.name,
            "task_type": "code_gen",
            "description": task.description,
            "languages": self.languages,
            "config": self.config,
        });

        Ok(TaskResult::success(
            task.id.clone(),
            output,
            start.elapsed().as_millis() as u64,
        ))
    }
}

/// Testing agent
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct TesterAgent {
    /// Agent name
    pub name: String,
    /// Test frameworks
    pub frameworks: Vec<String>,
    /// Configuration
    pub config: TesterConfig,
}

/// Tester configuration
#[derive(Debug, Clone, Default, Serialize, Deserialize)]
pub struct TesterConfig {
    /// Run in parallel
    pub parallel: bool,
    /// Coverage threshold
    pub coverage_threshold: Option<f64>,
    /// Test timeout in seconds
    pub test_timeout_secs: u64,
}

impl TesterAgent {
    /// Create a new testing agent
    pub fn new(name: &str) -> Self {
        Self {
            name: name.to_string(),
            frameworks: vec!["cargo-test".to_string()],
            config: TesterConfig {
                parallel: true,
                coverage_threshold: Some(80.0),
                test_timeout_secs: 60,
            },
        }
    }

    /// Add test framework
    pub fn with_framework(mut self, framework: &str) -> Self {
        self.frameworks.push(framework.to_string());
        self
    }

    /// Set coverage threshold
    pub fn with_coverage(mut self, threshold: f64) -> Self {
        self.config.coverage_threshold = Some(threshold);
        self
    }
}

impl Default for TesterAgent {
    fn default() -> Self {
        Self::new("tester")
    }
}

#[async_trait]
impl MicroAgent for TesterAgent {
    fn name(&self) -> &str {
        &self.name
    }

    fn role(&self) -> AgentRole {
        AgentRole::Tester
    }

    fn supported_tasks(&self) -> Vec<TaskType> {
        vec![TaskType::Test, TaskType::Validate]
    }

    async fn execute(&self, task: &Task) -> Result<TaskResult> {
        let start = std::time::Instant::now();

        let output = serde_json::json!({
            "agent": self.name,
            "task_type": "test",
            "description": task.description,
            "frameworks": self.frameworks,
            "config": self.config,
        });

        Ok(TaskResult::success(
            task.id.clone(),
            output,
            start.elapsed().as_millis() as u64,
        ))
    }
}

/// Code review agent
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct ReviewerAgent {
    /// Agent name
    pub name: String,
    /// Review criteria
    pub criteria: Vec<String>,
    /// Configuration
    pub config: ReviewerConfig,
}

/// Reviewer configuration
#[derive(Debug, Clone, Default, Serialize, Deserialize)]
pub struct ReviewerConfig {
    /// Check for security issues
    pub security_check: bool,
    /// Check for performance issues
    pub performance_check: bool,
    /// Check for style issues
    pub style_check: bool,
}

impl ReviewerAgent {
    /// Create a new review agent
    pub fn new(name: &str) -> Self {
        Self {
            name: name.to_string(),
            criteria: vec![
                "correctness".to_string(),
                "security".to_string(),
                "performance".to_string(),
            ],
            config: ReviewerConfig {
                security_check: true,
                performance_check: true,
                style_check: true,
            },
        }
    }
}

impl Default for ReviewerAgent {
    fn default() -> Self {
        Self::new("reviewer")
    }
}

#[async_trait]
impl MicroAgent for ReviewerAgent {
    fn name(&self) -> &str {
        &self.name
    }

    fn role(&self) -> AgentRole {
        AgentRole::Reviewer
    }

    fn supported_tasks(&self) -> Vec<TaskType> {
        vec![TaskType::Review]
    }

    async fn execute(&self, task: &Task) -> Result<TaskResult> {
        let start = std::time::Instant::now();

        let output = serde_json::json!({
            "agent": self.name,
            "task_type": "review",
            "description": task.description,
            "criteria": self.criteria,
            "config": self.config,
        });

        Ok(TaskResult::success(
            task.id.clone(),
            output,
            start.elapsed().as_millis() as u64,
        ))
    }
}

/// Validation agent
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct ValidatorAgent {
    /// Agent name
    pub name: String,
    /// Validation rules
    pub rules: Vec<String>,
}

impl ValidatorAgent {
    /// Create a new validation agent
    pub fn new(name: &str) -> Self {
        Self {
            name: name.to_string(),
            rules: vec!["syntax".to_string(), "types".to_string(), "lint".to_string()],
        }
    }
}

impl Default for ValidatorAgent {
    fn default() -> Self {
        Self::new("validator")
    }
}

#[async_trait]
impl MicroAgent for ValidatorAgent {
    fn name(&self) -> &str {
        &self.name
    }

    fn role(&self) -> AgentRole {
        AgentRole::Validator
    }

    fn supported_tasks(&self) -> Vec<TaskType> {
        vec![TaskType::Validate]
    }

    async fn execute(&self, task: &Task) -> Result<TaskResult> {
        let start = std::time::Instant::now();

        let output = serde_json::json!({
            "agent": self.name,
            "task_type": "validate",
            "description": task.description,
            "rules": self.rules,
        });

        Ok(TaskResult::success(
            task.id.clone(),
            output,
            start.elapsed().as_millis() as u64,
        ))
    }
}

/// RDF processing agent
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct RdfProcessorAgent {
    /// Agent name
    pub name: String,
    /// Ontology namespaces
    pub namespaces: Vec<String>,
}

impl RdfProcessorAgent {
    /// Create a new RDF processor agent
    pub fn new(name: &str) -> Self {
        Self {
            name: name.to_string(),
            namespaces: vec![
                "http://www.w3.org/2000/01/rdf-schema#".to_string(),
                "http://www.w3.org/2002/07/owl#".to_string(),
            ],
        }
    }
}

impl Default for RdfProcessorAgent {
    fn default() -> Self {
        Self::new("rdf-processor")
    }
}

#[async_trait]
impl MicroAgent for RdfProcessorAgent {
    fn name(&self) -> &str {
        &self.name
    }

    fn role(&self) -> AgentRole {
        AgentRole::RdfProcessor
    }

    fn supported_tasks(&self) -> Vec<TaskType> {
        vec![TaskType::RdfProcess]
    }

    async fn execute(&self, task: &Task) -> Result<TaskResult> {
        let start = std::time::Instant::now();

        let output = serde_json::json!({
            "agent": self.name,
            "task_type": "rdf_process",
            "description": task.description,
            "namespaces": self.namespaces,
        });

        Ok(TaskResult::success(
            task.id.clone(),
            output,
            start.elapsed().as_millis() as u64,
        ))
    }
}

/// Template generation agent
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct TemplateGenAgent {
    /// Agent name
    pub name: String,
    /// Template engine
    pub engine: String,
}

impl TemplateGenAgent {
    /// Create a new template generation agent
    pub fn new(name: &str) -> Self {
        Self {
            name: name.to_string(),
            engine: "tera".to_string(),
        }
    }
}

impl Default for TemplateGenAgent {
    fn default() -> Self {
        Self::new("template-gen")
    }
}

#[async_trait]
impl MicroAgent for TemplateGenAgent {
    fn name(&self) -> &str {
        &self.name
    }

    fn role(&self) -> AgentRole {
        AgentRole::TemplateGenerator
    }

    fn supported_tasks(&self) -> Vec<TaskType> {
        vec![TaskType::TemplateGen]
    }

    async fn execute(&self, task: &Task) -> Result<TaskResult> {
        let start = std::time::Instant::now();

        let output = serde_json::json!({
            "agent": self.name,
            "task_type": "template_gen",
            "description": task.description,
            "engine": self.engine,
        });

        Ok(TaskResult::success(
            task.id.clone(),
            output,
            start.elapsed().as_millis() as u64,
        ))
    }
}

/// Custom agent for user-defined behavior
pub struct CustomAgent {
    /// Agent name
    pub name: String,
    /// Agent role name
    pub role_name: String,
    /// Supported task types
    pub task_types: Vec<TaskType>,
    /// Custom handler
    handler: Arc<dyn Fn(&Task) -> serde_json::Value + Send + Sync>,
}

impl std::fmt::Debug for CustomAgent {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("CustomAgent")
            .field("name", &self.name)
            .field("role_name", &self.role_name)
            .field("task_types", &self.task_types)
            .field("handler", &"<function>")
            .finish()
    }
}

impl CustomAgent {
    /// Create a new custom agent
    pub fn new<F>(name: &str, role_name: &str, handler: F) -> Self
    where
        F: Fn(&Task) -> serde_json::Value + Send + Sync + 'static,
    {
        Self {
            name: name.to_string(),
            role_name: role_name.to_string(),
            task_types: vec![TaskType::Custom(role_name.to_string())],
            handler: Arc::new(handler),
        }
    }
}

#[async_trait]
impl MicroAgent for CustomAgent {
    fn name(&self) -> &str {
        &self.name
    }

    fn role(&self) -> AgentRole {
        AgentRole::Custom(self.role_name.clone())
    }

    fn supported_tasks(&self) -> Vec<TaskType> {
        self.task_types.clone()
    }

    async fn execute(&self, task: &Task) -> Result<TaskResult> {
        let start = std::time::Instant::now();
        let output = (self.handler)(task);

        Ok(TaskResult::success(
            task.id.clone(),
            output,
            start.elapsed().as_millis() as u64,
        ))
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[tokio::test]
    async fn test_code_gen_agent() {
        let agent = CodeGenAgent::new("coder").with_docs().with_tests();
        assert_eq!(agent.role(), AgentRole::CodeGenerator);

        let task = Task::code_gen("Generate a struct");
        assert!(agent.can_handle(&task));
    }

    #[tokio::test]
    async fn test_tester_agent() {
        let agent = TesterAgent::new("tester").with_coverage(90.0);
        assert_eq!(agent.role(), AgentRole::Tester);

        let task = Task::test("Run unit tests");
        let result = agent.execute(&task).await.unwrap();
        assert!(result.is_success());
    }

    #[tokio::test]
    async fn test_custom_agent() {
        let agent = CustomAgent::new("custom", "analyzer", |task| {
            serde_json::json!({"analyzed": task.description})
        });

        let task = Task::custom("analyzer", "Analyze code");
        let result = agent.execute(&task).await.unwrap();
        assert!(result.is_success());
    }
}
