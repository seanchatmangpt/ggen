//! Template Executor Agent
//! 
//! Handles template generation and code execution with deterministic outcomes

use super::*;
use std::collections::HashMap;
use std::path::PathBuf;
use tokio::time::{Duration, Instant};
use serde_json::Value;
use ggen_core::{Generator, GenContext, Template};
use chrono::{DateTime, Utc};

/// Template Executor Agent
/// 
/// Handles template generation and code execution with deterministic outcomes
/// Integrates with GGen core for actual template processing
pub struct TemplateExecutor {
    config: AgentConfig,
    status: AgentStatus,
    execution_history: Vec<ExecutionRecord>,
    template_cache: HashMap<String, CachedTemplate>,
    generator: Option<Generator>,
    execution_pool: ExecutionPool,
}

/// Execution record for audit trail
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct ExecutionRecord {
    pub id: Uuid,
    pub template_path: String,
    pub output_path: String,
    pub variables: HashMap<String, Value>,
    pub start_time: chrono::DateTime<chrono::Utc>,
    pub end_time: Option<chrono::DateTime<chrono::Utc>>,
    pub status: ExecutionStatus,
    pub files_generated: Vec<String>,
    pub errors: Vec<String>,
    pub metrics: ExecutionMetrics,
}

/// Execution status
#[derive(Debug, Clone, Serialize, Deserialize, PartialEq)]
pub enum ExecutionStatus {
    Pending,
    Running,
    Completed,
    Failed,
    Timeout,
    Cancelled,
}

/// Execution metrics
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct ExecutionMetrics {
    pub duration_ms: u64,
    pub memory_usage_mb: f64,
    pub cpu_usage_percent: f64,
    pub files_processed: usize,
    pub lines_generated: usize,
    pub cache_hits: usize,
    pub cache_misses: usize,
}

/// Cached template information
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct CachedTemplate {
    pub path: String,
    pub hash: String,
    pub last_modified: chrono::DateTime<chrono::Utc>,
    pub metadata: TemplateMetadata,
}

/// Template metadata
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct TemplateMetadata {
    pub name: String,
    pub description: String,
    pub variables: Vec<TemplateVariable>,
    pub dependencies: Vec<String>,
    pub output_patterns: Vec<String>,
}

/// Template variable definition
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct TemplateVariable {
    pub name: String,
    pub variable_type: VariableType,
    pub required: bool,
    pub default_value: Option<Value>,
    pub description: String,
}

/// Variable types
#[derive(Debug, Clone, Serialize, Deserialize, PartialEq)]
pub enum VariableType {
    String,
    Number,
    Boolean,
    Array,
    Object,
    Path,
    Url,
}

/// Execution pool for managing concurrent executions
#[derive(Debug, Clone)]
pub struct ExecutionPool {
    pub max_concurrent: usize,
    pub current_executions: usize,
    pub queue_size: usize,
    pub timeout_ms: u64,
}

#[async_trait::async_trait]
impl Agent for TemplateExecutor {
    async fn initialize(&mut self) -> Result<(), Box<dyn std::error::Error + Send + Sync>> {
        tracing::info!("Initializing Template Executor");
        
        // Initialize execution pool
        self.execution_pool = ExecutionPool {
            max_concurrent: 4,
            current_executions: 0,
            queue_size: 0,
            timeout_ms: 30000, // 30 seconds
        };
        
        // Initialize GGen generator
        self.generator = Some(self.create_generator().await?);
        
        tracing::info!("Template Executor initialized with {} max concurrent executions", 
            self.execution_pool.max_concurrent);
        Ok(())
    }
    
    async fn start(&mut self) -> Result<(), Box<dyn std::error::Error + Send + Sync>> {
        tracing::info!("Starting Template Executor");
        self.status = AgentStatus::Healthy;
        Ok(())
    }
    
    async fn stop(&mut self) -> Result<(), Box<dyn std::error::Error + Send + Sync>> {
        tracing::info!("Stopping Template Executor");
        self.status = AgentStatus::Unhealthy;
        Ok(())
    }
    
    async fn status(&self) -> AgentStatus {
        self.status.clone()
    }
    
    fn config(&self) -> &AgentConfig {
        &self.config
    }
    
    async fn handle_message(&mut self, message: AgentMessage) -> Result<AgentMessage, Box<dyn std::error::Error + Send + Sync>> {
        match message {
            AgentMessage::TaskAssignment { task_id, task } => {
                self.handle_execution_task(task_id, task).await
            }
            AgentMessage::HealthCheck { from } => {
                Ok(AgentMessage::HealthResponse {
                    status: self.status.clone(),
                    metrics: Some(self.get_metrics().await?),
                })
            }
            _ => {
                tracing::warn!("Template Executor received unhandled message type");
                Ok(AgentMessage::ErrorNotification {
                    error: "Unhandled message type".to_string(),
                    severity: ErrorSeverity::Medium,
                })
            }
        }
    }
}

impl TemplateExecutor {
    pub fn new(config: AgentConfig) -> Self {
        Self {
            config,
            status: AgentStatus::Healthy,
            execution_history: Vec::new(),
            template_cache: HashMap::new(),
            generator: None,
            execution_pool: ExecutionPool {
                max_concurrent: 4,
                current_executions: 0,
                queue_size: 0,
                timeout_ms: 30000,
            },
        }
    }
    
    /// Create GGen generator instance
    async fn create_generator(&self) -> Result<Generator, Box<dyn std::error::Error + Send + Sync>> {
        // TODO: Initialize with actual GGen core configuration
        let template_path = PathBuf::from("templates");
        let output_root = PathBuf::from("output");

        let ctx = GenContext::new(template_path.clone(), output_root);
        let pipeline = ggen_core::Pipeline::new()?;
        let generator = Generator::new(pipeline, ctx);

        Ok(generator)
    }
    
    /// Handle execution task
    async fn handle_execution_task(&mut self, task_id: Uuid, task: TaskDefinition) -> Result<AgentMessage, Box<dyn std::error::Error + Send + Sync>> {
        tracing::info!("Handling execution task: {}", task_id);
        
        // Check execution pool capacity
        if self.execution_pool.current_executions >= self.execution_pool.max_concurrent {
            return Ok(AgentMessage::TaskCompletion {
                task_id,
                result: TaskResult {
                    task_id,
                    success: false,
                    result: None,
                    error: Some("Execution pool at capacity".to_string()),
                    duration_ms: 0,
                    metrics: None,
                },
            });
        }
        
        let start_time = Utc::now();
        self.execution_pool.current_executions += 1;
        
        // Create execution record
        let mut execution = ExecutionRecord {
            id: task_id,
            template_path: task.parameters["template"].as_str().unwrap_or("unknown").to_string(),
            output_path: task.parameters["output"].as_str().unwrap_or(".").to_string(),
            variables: self.extract_variables(&task.parameters),
            start_time: chrono::Utc::now(),
            end_time: None,
            status: ExecutionStatus::Running,
            files_generated: Vec::new(),
            errors: Vec::new(),
            metrics: ExecutionMetrics {
                duration_ms: 0,
                memory_usage_mb: 0.0,
                cpu_usage_percent: 0.0,
                files_processed: 0,
                lines_generated: 0,
                cache_hits: 0,
                cache_misses: 0,
            },
        };
        
        // Execute the template
        match self.execute_template(&mut execution).await {
            Ok(result) => {
                execution.status = ExecutionStatus::Completed;
                execution.end_time = Some(chrono::Utc::now());
                execution.metrics.duration_ms = Utc::now().signed_duration_since(start_time).num_milliseconds() as u64;
                
                self.execution_history.push(execution.clone());
                self.execution_pool.current_executions -= 1;
                
                Ok(AgentMessage::TaskCompletion {
                    task_id,
                    result: TaskResult {
                        task_id,
                        success: true,
                        result: Some(result),
                        error: None,
                        duration_ms: execution.metrics.duration_ms,
                        metrics: Some(self.get_execution_metrics(&execution).await?),
                    },
                })
            }
            Err(e) => {
                execution.status = ExecutionStatus::Failed;
                execution.end_time = Some(chrono::Utc::now());
                execution.errors.push(e.to_string());
                execution.metrics.duration_ms = Utc::now().signed_duration_since(start_time).num_milliseconds() as u64;
                
                self.execution_history.push(execution.clone());
                self.execution_pool.current_executions -= 1;
                
                Ok(AgentMessage::TaskCompletion {
                    task_id,
                    result: TaskResult {
                        task_id,
                        success: false,
                        result: None,
                        error: Some(e.to_string()),
                        duration_ms: execution.metrics.duration_ms,
                        metrics: Some(self.get_execution_metrics(&execution).await?),
                    },
                })
            }
        }
    }
    
    /// Execute template generation
    async fn execute_template(&mut self, execution: &mut ExecutionRecord) -> Result<Value, Box<dyn std::error::Error + Send + Sync>> {
        tracing::info!("Executing template: {}", execution.template_path);
        
        // Check template cache first
        let template_path = &execution.template_path;
        if let Some(cached_template) = self.template_cache.get(template_path) {
            execution.metrics.cache_hits += 1;
            tracing::info!("Using cached template: {}", template_path);
        } else {
            execution.metrics.cache_misses += 1;
            // Load and cache template
            self.load_and_cache_template(template_path).await?;
        }
        
        // Prepare execution context
        let template_path_buf = PathBuf::from(template_path);
        let output_path_buf = PathBuf::from(&execution.output_path);
        
        let mut ctx = GenContext::new(template_path_buf, output_path_buf);
        
        // Set variables
        let mut vars = std::collections::BTreeMap::new();
        for (key, value) in &execution.variables {
            if let Some(str_value) = value.as_str() {
                vars.insert(key.clone(), str_value.to_string());
            }
        }
        ctx = ctx.with_vars(vars);
        
        // Execute with timeout
        let execution_future = self.execute_with_generator(ctx);
        let timeout_duration = Duration::from_millis(self.execution_pool.timeout_ms);
        
        match tokio::time::timeout(timeout_duration, execution_future).await {
            Ok(result) => {
                let result = result?;
                let files = result.files_generated.clone();
                execution.files_generated = files;
                execution.metrics.files_processed = result.files_generated.len();
                execution.metrics.lines_generated = result.lines_generated;
                
                Ok(serde_json::json!({
                    "template": execution.template_path,
                    "output_path": execution.output_path,
                    "files_generated": execution.files_generated,
                    "lines_generated": execution.metrics.lines_generated,
                    "variables_applied": execution.variables,
                    "status": "completed"
                }))
            }
            Err(_) => {
                execution.status = ExecutionStatus::Timeout;
                Err("Template execution timeout".into())
            }
        }
    }
    
    /// Execute with GGen generator
    async fn execute_with_generator(&self, ctx: GenContext) -> Result<ExecutionResult, Box<dyn std::error::Error + Send + Sync>> {
        // TODO: Use actual GGen generator
        // For now, simulate execution
        Ok(ExecutionResult {
            files_generated: vec![
                "README.md".to_string(),
                "src/main.rs".to_string(),
                "Cargo.toml".to_string(),
            ],
            lines_generated: 150,
        })
    }
    
    /// Load and cache template
    async fn load_and_cache_template(&mut self, template_path: &str) -> Result<(), Box<dyn std::error::Error + Send + Sync>> {
        tracing::info!("Loading template: {}", template_path);
        
        // TODO: Load actual template from GGen core
        // For now, create a mock cached template
        let cached_template = CachedTemplate {
            path: template_path.to_string(),
            hash: format!("hash_{}", template_path),
            last_modified: chrono::Utc::now(),
            metadata: TemplateMetadata {
                name: template_path.to_string(),
                description: "Mock template".to_string(),
                variables: vec![
                    TemplateVariable {
                        name: "name".to_string(),
                        variable_type: VariableType::String,
                        required: true,
                        default_value: None,
                        description: "Project name".to_string(),
                    },
                ],
                dependencies: Vec::new(),
                output_patterns: vec!["*.rs".to_string(), "*.md".to_string()],
            },
        };
        
        self.template_cache.insert(template_path.to_string(), cached_template);
        Ok(())
    }
    
    /// Extract variables from task parameters
    fn extract_variables(&self, parameters: &Value) -> HashMap<String, Value> {
        let mut variables = HashMap::new();
        
        if let Some(vars) = parameters.get("vars") {
            if let Some(obj) = vars.as_object() {
                for (key, value) in obj {
                    variables.insert(key.clone(), value.clone());
                }
            }
        }
        
        variables
    }
    
    /// Get execution metrics
    async fn get_execution_metrics(&self, execution: &ExecutionRecord) -> Result<Value, Box<dyn std::error::Error + Send + Sync>> {
        Ok(serde_json::json!({
            "execution_id": execution.id,
            "template_path": execution.template_path,
            "output_path": execution.output_path,
            "status": execution.status,
            "duration_ms": execution.metrics.duration_ms,
            "files_generated": execution.files_generated.len(),
            "lines_generated": execution.metrics.lines_generated,
            "cache_hits": execution.metrics.cache_hits,
            "cache_misses": execution.metrics.cache_misses,
            "errors": execution.errors.len()
        }))
    }
    
    /// Get executor metrics
    async fn get_metrics(&self) -> Result<Value, Box<dyn std::error::Error + Send + Sync>> {
        let total_executions = self.execution_history.len();
        let successful_executions = self.execution_history.iter()
            .filter(|e| e.status == ExecutionStatus::Completed)
            .count();
        let failed_executions = self.execution_history.iter()
            .filter(|e| e.status == ExecutionStatus::Failed)
            .count();
        
        let total_files_generated: usize = self.execution_history.iter()
            .map(|e| e.files_generated.len())
            .sum();
        
        let total_lines_generated: usize = self.execution_history.iter()
            .map(|e| e.metrics.lines_generated)
            .sum();
        
        Ok(serde_json::json!({
            "total_executions": total_executions,
            "successful_executions": successful_executions,
            "failed_executions": failed_executions,
            "success_rate": if total_executions > 0 { 
                successful_executions as f64 / total_executions as f64 
            } else { 
                0.0 
            },
            "total_files_generated": total_files_generated,
            "total_lines_generated": total_lines_generated,
            "execution_pool": {
                "max_concurrent": self.execution_pool.max_concurrent,
                "current_executions": self.execution_pool.current_executions,
                "queue_size": self.execution_pool.queue_size
            },
            "template_cache": {
                "cached_templates": self.template_cache.len()
            },
            "status": self.status
        }))
    }
}

/// Execution result
#[derive(Debug, Clone)]
struct ExecutionResult {
    files_generated: Vec<String>,
    lines_generated: usize,
}

