//! PSO-based Template Parameter Optimization Agent
//!
//! This agent uses Particle Swarm Optimization to tune template parameters for optimal code generation.

use crate::error::{GgenAiError, Result};
use crate::swarm::{
    AgentConfig, AgentHealth, AgentInput, AgentOutput, BaseAgent, HealthStatus,
    PerformanceThresholds, SwarmAgent, SwarmContext,
};
use crate::swarm::algorithms::pso::{
    DefaultTemplateFitness, FitnessFunction, ParameterType, PsoConfig,
    TemplateParameter, TemplateParameterOptimizer, TemplateQuality,
};
use async_trait::async_trait;
use serde_json::json;
use std::collections::HashMap;
use tracing::{debug, info};

/// PSO-based template parameter optimization agent
#[derive(Debug)]
pub struct PsoTemplateAgent {
    /// Base agent functionality
    base: BaseAgent,
    /// PSO configuration
    pso_config: PsoConfig,
    /// Template parameters
    template_parameters: Vec<TemplateParameter>,
    /// Optimization history
    optimization_history: Vec<OptimizationRecord>,
}

/// Record of a parameter optimization
#[derive(Debug, Clone)]
struct OptimizationRecord {
    /// Template name
    template_name: String,
    /// Optimized parameters
    parameters: HashMap<String, f64>,
    /// Quality score achieved
    quality: f64,
    /// Iterations to converge
    iterations: usize,
}

impl PsoTemplateAgent {
    /// Create a new PSO template agent
    pub fn new(name: String) -> Self {
        let config = AgentConfig {
            timeout_seconds: 120,
            retry_attempts: 3,
            verbose_logging: true,
            performance_thresholds: PerformanceThresholds {
                max_execution_time_ms: 60000,
                max_memory_usage_mb: 300,
                min_quality_score: 0.75,
            },
        };

        let capabilities = vec![
            "template_optimization".to_string(),
            "parameter_tuning".to_string(),
            "pso_optimization".to_string(),
            "quality_improvement".to_string(),
        ];

        // Default template parameters
        let template_parameters = vec![
            TemplateParameter {
                name: "complexity".to_string(),
                min: 0.0,
                max: 1.0,
                discrete: false,
                param_type: ParameterType::Complexity,
            },
            TemplateParameter {
                name: "verbosity".to_string(),
                min: 0.0,
                max: 1.0,
                discrete: false,
                param_type: ParameterType::Verbosity,
            },
            TemplateParameter {
                name: "optimization_level".to_string(),
                min: 0.0,
                max: 3.0,
                discrete: true,
                param_type: ParameterType::Performance,
            },
            TemplateParameter {
                name: "documentation_density".to_string(),
                min: 0.0,
                max: 1.0,
                discrete: false,
                param_type: ParameterType::Style,
            },
        ];

        Self {
            base: BaseAgent::new(&name, capabilities, config),
            pso_config: PsoConfig::default(),
            template_parameters,
            optimization_history: Vec::new(),
        }
    }

    /// Optimize template parameters using PSO
    async fn optimize_template_parameters(
        &mut self,
        template_name: &str,
        requirements: &TemplateRequirements,
    ) -> Result<AgentOutput> {
        info!("Optimizing template parameters using PSO for: {}", template_name);

        // Create fitness function based on requirements
        let fitness_fn = self.create_fitness_function(requirements);

        // Create optimizer
        let optimizer = TemplateParameterOptimizer::new(
            self.pso_config.clone(),
            self.template_parameters.clone(),
        );

        // Run optimization
        let solution = optimizer.optimize(&fitness_fn).await
            .map_err(|e| GgenAiError::internal(&format!("PSO optimization failed: {}", e)))?;

        info!(
            "PSO converged in {} iterations with quality: {:.4}",
            solution.iterations, solution.quality
        );

        // Record optimization
        self.optimization_history.push(OptimizationRecord {
            template_name: template_name.to_string(),
            parameters: solution.parameters.clone(),
            quality: solution.quality,
            iterations: solution.iterations,
        });

        // Create output
        let mut metadata = HashMap::new();
        metadata.insert("template_name".to_string(), template_name.to_string());
        metadata.insert("iterations".to_string(), solution.iterations.to_string());
        metadata.insert("quality_score".to_string(), solution.quality.to_string());

        Ok(AgentOutput {
            data: json!({
                "template_name": template_name,
                "optimized_parameters": solution.parameters,
                "quality": solution.quality,
                "iterations": solution.iterations
            }),
            output_type: "template_optimization".to_string(),
            target_agents: vec!["template_generator".to_string()],
            metadata,
        })
    }

    /// Create fitness function from requirements
    fn create_fitness_function(&self, requirements: &TemplateRequirements) -> DefaultTemplateFitness {
        let target_complexity = requirements.target_complexity;
        let target_readability = requirements.target_readability;
        let target_performance = requirements.target_performance;

        DefaultTemplateFitness::new(move |params| {
            let complexity = params.get("complexity").copied().unwrap_or(0.5);
            let verbosity = params.get("verbosity").copied().unwrap_or(0.5);
            let optimization_level = params.get("optimization_level").copied().unwrap_or(1.0);
            let documentation = params.get("documentation_density").copied().unwrap_or(0.5);

            // Calculate quality metrics
            let correctness = 0.95; // Would be calculated from actual validation

            let readability = (1.0 - (verbosity - target_readability).abs()) * 0.5
                + (documentation * 0.5);

            let performance = (1.0 - (optimization_level / 3.0 - target_performance).abs());

            let maintainability = (1.0 - (complexity - target_complexity).abs()) * 0.5
                + (verbosity * 0.3)
                + (documentation * 0.2);

            let test_coverage = 0.8; // Would be calculated from actual tests

            TemplateQuality {
                correctness,
                readability,
                performance,
                maintainability,
                test_coverage,
            }
        })
    }

    /// Get optimization statistics
    pub fn get_optimization_stats(&self) -> HashMap<String, serde_json::Value> {
        let mut stats = HashMap::new();

        stats.insert("total_optimizations".to_string(), json!(self.optimization_history.len()));

        if !self.optimization_history.is_empty() {
            let avg_quality = self.optimization_history
                .iter()
                .map(|r| r.quality)
                .sum::<f64>()
                / self.optimization_history.len() as f64;

            let avg_iterations = self.optimization_history
                .iter()
                .map(|r| r.iterations)
                .sum::<usize>()
                / self.optimization_history.len();

            stats.insert("average_quality".to_string(), json!(avg_quality));
            stats.insert("average_iterations".to_string(), json!(avg_iterations));
        }

        stats
    }
}

/// Template optimization requirements
#[derive(Debug, Clone)]
pub struct TemplateRequirements {
    /// Target complexity (0.0 to 1.0)
    pub target_complexity: f64,
    /// Target readability (0.0 to 1.0)
    pub target_readability: f64,
    /// Target performance (0.0 to 1.0)
    pub target_performance: f64,
    /// Minimum quality threshold
    pub min_quality: f64,
}

impl Default for TemplateRequirements {
    fn default() -> Self {
        Self {
            target_complexity: 0.5,
            target_readability: 0.8,
            target_performance: 0.7,
            min_quality: 0.75,
        }
    }
}

#[async_trait]
impl SwarmAgent for PsoTemplateAgent {
    fn name(&self) -> &str {
        self.base.name()
    }

    fn capabilities(&self) -> Vec<String> {
        self.base.capabilities()
    }

    async fn execute(&self, _context: &SwarmContext, input: AgentInput) -> Result<AgentOutput> {
        debug!("PsoTemplateAgent executing with input type: {}", input.input_type);

        match input.input_type.as_str() {
            "template_optimization" | "optimize_template" => {
                // Extract template name from input
                let template_name = input.data.get("template_name")
                    .and_then(|v| v.as_str())
                    .ok_or_else(|| GgenAiError::internal("No template name provided in input"))?;

                // Extract requirements (use defaults if not provided)
                let requirements = if let Some(req_data) = input.data.get("requirements") {
                    serde_json::from_value(req_data.clone())
                        .unwrap_or_default()
                } else {
                    TemplateRequirements::default()
                };

                // Clone self to get mutable access (in real implementation, would use interior mutability)
                let mut agent_clone = PsoTemplateAgent::new(self.name().to_string());
                agent_clone.pso_config = self.pso_config.clone();
                agent_clone.template_parameters = self.template_parameters.clone();

                agent_clone.optimize_template_parameters(template_name, &requirements).await
            }
            _ => {
                Err(GgenAiError::internal(&format!(
                    "Unsupported input type: {}",
                    input.input_type
                )))
            }
        }
    }

    async fn validate(&self) -> Result<bool> {
        // Validate PSO configuration
        if self.pso_config.num_particles == 0 {
            return Ok(false);
        }

        if self.pso_config.max_iterations == 0 {
            return Ok(false);
        }

        if self.template_parameters.is_empty() {
            return Ok(false);
        }

        Ok(true)
    }

    async fn health_check(&self) -> AgentHealth {
        let mut issues = Vec::new();

        // Check configuration
        if self.pso_config.num_particles < 10 {
            issues.push("PSO particle count is low".to_string());
        }

        if self.template_parameters.is_empty() {
            issues.push("No template parameters defined".to_string());
        }

        let status = if issues.is_empty() {
            HealthStatus::Healthy
        } else {
            HealthStatus::Degraded
        };

        let score = if issues.is_empty() { 1.0 } else { 0.7 };

        AgentHealth {
            status,
            score,
            last_check: chrono::Utc::now().to_rfc3339(),
            issues,
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[tokio::test]
    async fn test_pso_template_agent_creation() {
        let agent = PsoTemplateAgent::new("pso-template-agent".to_string());

        assert_eq!(agent.name(), "pso-template-agent");
        assert!(agent.capabilities().contains(&"template_optimization".to_string()));
    }

    #[tokio::test]
    async fn test_pso_template_agent_validation() {
        let agent = PsoTemplateAgent::new("pso-template-agent".to_string());

        let is_valid = agent.validate().await.unwrap();
        assert!(is_valid);
    }

    #[tokio::test]
    async fn test_pso_template_agent_health_check() {
        let agent = PsoTemplateAgent::new("pso-template-agent".to_string());

        let health = agent.health_check().await;
        assert!(matches!(health.status, HealthStatus::Healthy));
        assert!(health.score > 0.5);
    }
}
