//! Pipeline orchestration for multi-stage code generation
//!
//! Enables defining and executing multi-stage pipelines with automatic
//! data flow between stages.

use super::agents::MicroAgent;
use super::orchestrator::AgentOrchestrator;
use super::tasks::{Task, TaskResult, TaskStatus, TaskType};
use crate::error::{GgenAiError, Result};
use serde::{Deserialize, Serialize};
use std::collections::HashMap;
use std::sync::Arc;

/// A pipeline stage definition
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct PipelineStage {
    /// Stage name
    pub name: String,
    /// Stage description
    pub description: String,
    /// Task type for this stage
    pub task_type: TaskType,
    /// Whether this stage can run in parallel with others
    pub parallel: bool,
    /// Stage timeout in seconds
    pub timeout_secs: u64,
    /// Retry count
    pub retry_count: u32,
    /// Input mappings from previous stages
    pub input_mappings: HashMap<String, String>,
    /// Tags for this stage
    pub tags: Vec<String>,
}

impl PipelineStage {
    /// Create a new pipeline stage
    pub fn new(name: &str, task_type: TaskType) -> Self {
        Self {
            name: name.to_string(),
            description: String::new(),
            task_type,
            parallel: false,
            timeout_secs: 60,
            retry_count: 2,
            input_mappings: HashMap::new(),
            tags: Vec::new(),
        }
    }

    /// Set description
    pub fn with_description(mut self, desc: &str) -> Self {
        self.description = desc.to_string();
        self
    }

    /// Enable parallel execution
    pub fn parallel(mut self) -> Self {
        self.parallel = true;
        self
    }

    /// Set timeout
    pub fn with_timeout(mut self, secs: u64) -> Self {
        self.timeout_secs = secs;
        self
    }

    /// Set retry count
    pub fn with_retries(mut self, count: u32) -> Self {
        self.retry_count = count;
        self
    }

    /// Map input from previous stage
    pub fn map_input(mut self, source: &str, target: &str) -> Self {
        self.input_mappings
            .insert(source.to_string(), target.to_string());
        self
    }

    /// Add tag
    pub fn with_tag(mut self, tag: &str) -> Self {
        self.tags.push(tag.to_string());
        self
    }
}

/// Pipeline definition
#[derive(Debug, Clone)]
pub struct Pipeline {
    /// Pipeline name
    pub name: String,
    /// Pipeline stages
    stages: Vec<PipelineStage>,
    /// Stage results
    results: HashMap<String, TaskResult>,
    /// Pipeline configuration
    config: PipelineConfig,
}

/// Pipeline configuration
#[derive(Debug, Clone, Default, Serialize, Deserialize)]
pub struct PipelineConfig {
    /// Stop on first failure
    pub stop_on_failure: bool,
    /// Maximum parallel stages
    pub max_parallel_stages: usize,
    /// Enable stage caching
    pub enable_caching: bool,
}

impl Pipeline {
    /// Create a new pipeline
    pub fn new(name: &str) -> Self {
        Self {
            name: name.to_string(),
            stages: Vec::new(),
            results: HashMap::new(),
            config: PipelineConfig::default(),
        }
    }

    /// Create a builder
    pub fn builder(name: &str) -> PipelineBuilder {
        PipelineBuilder::new(name)
    }

    /// Add a stage
    pub fn add_stage(&mut self, stage: PipelineStage) {
        self.stages.push(stage);
    }

    /// Get stage count
    pub fn stage_count(&self) -> usize {
        self.stages.len()
    }

    /// Execute the pipeline
    pub async fn execute(&mut self, orchestrator: &AgentOrchestrator) -> Result<PipelineResult> {
        let mut stage_results = Vec::new();
        let mut context = serde_json::Map::new();

        for stage in &self.stages {
            // Create task for this stage
            let mut task = Task::new(stage.task_type.clone(), &stage.description);
            task.config.timeout = std::time::Duration::from_secs(stage.timeout_secs);
            task.config.retry_count = stage.retry_count;

            // Map inputs from previous stages
            for (source, target) in &stage.input_mappings {
                if let Some(value) = context.get(source) {
                    task.config
                        .params
                        .insert(target.clone(), value.clone());
                }
            }

            // Execute stage
            let result = orchestrator.execute(task).await?;

            // Store result in context
            if result.is_success() {
                if let serde_json::Value::Object(obj) = &result.output {
                    for (k, v) in obj {
                        context.insert(format!("{}.{}", stage.name, k), v.clone());
                    }
                }
                context.insert(
                    format!("{}.output", stage.name),
                    result.output.clone(),
                );
            }

            let stage_success = result.is_success();
            stage_results.push(StageResult {
                stage_name: stage.name.clone(),
                task_result: result,
            });

            // Check stop on failure
            if !stage_success && self.config.stop_on_failure {
                return Ok(PipelineResult {
                    pipeline_name: self.name.clone(),
                    success: false,
                    stage_results,
                    final_output: serde_json::Value::Object(context),
                    error: Some("Pipeline stopped on failure".to_string()),
                });
            }
        }

        let success = stage_results.iter().all(|r| r.task_result.is_success());

        Ok(PipelineResult {
            pipeline_name: self.name.clone(),
            success,
            stage_results,
            final_output: serde_json::Value::Object(context),
            error: None,
        })
    }
}

/// Builder for pipelines
pub struct PipelineBuilder {
    pipeline: Pipeline,
}

impl PipelineBuilder {
    /// Create a new builder
    pub fn new(name: &str) -> Self {
        Self {
            pipeline: Pipeline::new(name),
        }
    }

    /// Add a stage
    pub fn stage(mut self, stage: PipelineStage) -> Self {
        self.pipeline.stages.push(stage);
        self
    }

    /// Add code generation stage
    pub fn code_gen(self, name: &str, description: &str) -> Self {
        self.stage(
            PipelineStage::new(name, TaskType::CodeGen).with_description(description),
        )
    }

    /// Add test stage
    pub fn test(self, name: &str, description: &str) -> Self {
        self.stage(PipelineStage::new(name, TaskType::Test).with_description(description))
    }

    /// Add review stage
    pub fn review(self, name: &str, description: &str) -> Self {
        self.stage(
            PipelineStage::new(name, TaskType::Review).with_description(description),
        )
    }

    /// Add validation stage
    pub fn validate(self, name: &str, description: &str) -> Self {
        self.stage(
            PipelineStage::new(name, TaskType::Validate).with_description(description),
        )
    }

    /// Add RDF processing stage
    pub fn rdf_process(self, name: &str, description: &str) -> Self {
        self.stage(
            PipelineStage::new(name, TaskType::RdfProcess).with_description(description),
        )
    }

    /// Stop on first failure
    pub fn stop_on_failure(mut self) -> Self {
        self.pipeline.config.stop_on_failure = true;
        self
    }

    /// Enable caching
    pub fn enable_caching(mut self) -> Self {
        self.pipeline.config.enable_caching = true;
        self
    }

    /// Build the pipeline
    pub fn build(self) -> Pipeline {
        self.pipeline
    }
}

/// Result from a single stage
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct StageResult {
    /// Stage name
    pub stage_name: String,
    /// Task result
    pub task_result: TaskResult,
}

/// Result from pipeline execution
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct PipelineResult {
    /// Pipeline name
    pub pipeline_name: String,
    /// Overall success
    pub success: bool,
    /// Per-stage results
    pub stage_results: Vec<StageResult>,
    /// Final output
    pub final_output: serde_json::Value,
    /// Error message if failed
    pub error: Option<String>,
}

impl PipelineResult {
    /// Get result for a specific stage
    pub fn stage(&self, name: &str) -> Option<&StageResult> {
        self.stage_results
            .iter()
            .find(|r| r.stage_name == name)
    }

    /// Get all successful stages
    pub fn successful_stages(&self) -> Vec<&StageResult> {
        self.stage_results
            .iter()
            .filter(|r| r.task_result.is_success())
            .collect()
    }

    /// Get all failed stages
    pub fn failed_stages(&self) -> Vec<&StageResult> {
        self.stage_results
            .iter()
            .filter(|r| !r.task_result.is_success())
            .collect()
    }
}

/// Pre-built pipelines for common workflows
impl Pipeline {
    /// Code generation pipeline (generate -> validate -> test)
    pub fn code_gen_pipeline() -> Pipeline {
        PipelineBuilder::new("code-gen")
            .code_gen("generate", "Generate code from specification")
            .validate("validate", "Validate generated code")
            .test("test", "Run tests on generated code")
            .stop_on_failure()
            .build()
    }

    /// Full review pipeline (generate -> review -> validate -> test)
    pub fn full_review_pipeline() -> Pipeline {
        PipelineBuilder::new("full-review")
            .code_gen("generate", "Generate code")
            .review("review", "Review generated code")
            .validate("validate", "Validate code")
            .test("test", "Run tests")
            .stop_on_failure()
            .build()
    }

    /// RDF processing pipeline
    pub fn rdf_pipeline() -> Pipeline {
        PipelineBuilder::new("rdf-processing")
            .rdf_process("parse", "Parse RDF ontology")
            .validate("validate", "Validate RDF structure")
            .code_gen("generate", "Generate code from RDF")
            .test("test", "Test generated code")
            .build()
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::microframework::agents::CodeGenAgent;

    #[test]
    fn test_stage_creation() {
        let stage = PipelineStage::new("generate", TaskType::CodeGen)
            .with_description("Generate code")
            .with_timeout(120)
            .with_retries(3);

        assert_eq!(stage.name, "generate");
        assert_eq!(stage.timeout_secs, 120);
        assert_eq!(stage.retry_count, 3);
    }

    #[test]
    fn test_pipeline_builder() {
        let pipeline = PipelineBuilder::new("test-pipeline")
            .code_gen("gen", "Generate code")
            .test("test", "Test code")
            .stop_on_failure()
            .build();

        assert_eq!(pipeline.stage_count(), 2);
        assert!(pipeline.config.stop_on_failure);
    }

    #[test]
    fn test_prebuilt_pipelines() {
        let code_gen = Pipeline::code_gen_pipeline();
        assert_eq!(code_gen.stage_count(), 3);

        let full_review = Pipeline::full_review_pipeline();
        assert_eq!(full_review.stage_count(), 4);

        let rdf = Pipeline::rdf_pipeline();
        assert_eq!(rdf.stage_count(), 4);
    }

    #[tokio::test]
    async fn test_pipeline_execution() {
        let orchestrator = AgentOrchestrator::new();
        orchestrator.register_agent(CodeGenAgent::new("coder"));

        let mut pipeline = PipelineBuilder::new("test")
            .code_gen("gen", "Generate code")
            .build();

        let result = pipeline.execute(&orchestrator).await.unwrap();
        assert!(result.success);
        assert_eq!(result.stage_results.len(), 1);
    }
}
