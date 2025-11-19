//! ACO-based SPARQL Query Optimization Agent
//!
//! This agent uses Ant Colony Optimization to find optimal SPARQL query execution paths.

use crate::error::{GgenAiError, Result};
use crate::swarm::{
    AgentConfig, AgentHealth, AgentInput, AgentOutput, BaseAgent, HealthStatus,
    PerformanceThresholds, SwarmAgent, SwarmContext,
};
use crate::swarm::algorithms::aco::{AcoConfig, QueryNode, SparqlAcoOptimizer, parse_sparql_to_nodes};
use async_trait::async_trait;
use serde_json::json;
use std::collections::HashMap;
use tracing::{debug, info};

/// ACO-based SPARQL query optimization agent
#[derive(Debug)]
pub struct AcoSparqlAgent {
    /// Base agent functionality
    base: BaseAgent,
    /// ACO configuration
    aco_config: AcoConfig,
    /// Query optimization history
    optimization_history: Vec<OptimizationRecord>,
}

/// Record of a query optimization
#[derive(Debug, Clone)]
struct OptimizationRecord {
    /// Original query
    query: String,
    /// Optimized query path
    optimized_path: Vec<QueryNode>,
    /// Execution time (ms)
    execution_time_ms: u64,
    /// Quality score
    quality: f64,
}

impl AcoSparqlAgent {
    /// Create a new ACO SPARQL agent
    pub fn new(name: String) -> Self {
        let config = AgentConfig {
            timeout_seconds: 60,
            retry_attempts: 3,
            verbose_logging: true,
            performance_thresholds: PerformanceThresholds {
                max_execution_time_ms: 30000,
                max_memory_usage_mb: 200,
                min_quality_score: 0.7,
            },
        };

        let capabilities = vec![
            "sparql_optimization".to_string(),
            "query_path_finding".to_string(),
            "aco_optimization".to_string(),
            "performance_tuning".to_string(),
        ];

        Self {
            base: BaseAgent::new(&name, capabilities, config),
            aco_config: AcoConfig::default(),
            optimization_history: Vec::new(),
        }
    }

    /// Optimize a SPARQL query using ACO
    async fn optimize_sparql_query(&mut self, query: &str) -> Result<AgentOutput> {
        info!("Optimizing SPARQL query using ACO");

        // Parse SPARQL query to nodes
        let nodes = parse_sparql_to_nodes(query)
            .map_err(|e| GgenAiError::internal(&format!("Failed to parse SPARQL query: {}", e)))?;

        if nodes.is_empty() {
            return Err(GgenAiError::internal("No query nodes found"));
        }

        debug!("Parsed {} query nodes from SPARQL", nodes.len());

        // Create ACO optimizer
        let optimizer = SparqlAcoOptimizer::new(self.aco_config.clone(), nodes);

        // Run optimization
        let best_path = optimizer.optimize().await
            .map_err(|e| GgenAiError::internal(&format!("ACO optimization failed: {}", e)))?;

        info!("ACO found optimal path with cost: {:.4}", best_path.cost);

        // Generate optimized query from path
        let optimized_query = self.generate_optimized_query(&best_path.nodes);

        // Record optimization
        self.optimization_history.push(OptimizationRecord {
            query: query.to_string(),
            optimized_path: best_path.nodes.clone(),
            execution_time_ms: 0, // Would be filled by actual execution
            quality: best_path.quality,
        });

        // Create output
        let mut metadata = HashMap::new();
        metadata.insert("original_query".to_string(), query.to_string());
        metadata.insert("optimization_cost".to_string(), best_path.cost.to_string());
        metadata.insert("quality_score".to_string(), best_path.quality.to_string());
        metadata.insert("path_length".to_string(), best_path.nodes.len().to_string());

        Ok(AgentOutput {
            data: json!({
                "optimized_query": optimized_query,
                "path": best_path.nodes,
                "cost": best_path.cost,
                "quality": best_path.quality
            }),
            output_type: "sparql_optimization".to_string(),
            target_agents: vec!["query_executor".to_string()],
            metadata,
        })
    }

    /// Generate optimized SPARQL query from node path
    fn generate_optimized_query(&self, nodes: &[QueryNode]) -> String {
        let mut query = "SELECT * WHERE {\n".to_string();

        for (i, node) in nodes.iter().enumerate() {
            query.push_str(&format!(
                "  {} {} {} .",
                node.subject, node.predicate, node.object
            ));

            if i < nodes.len() - 1 {
                query.push('\n');
            }
        }

        query.push_str("\n}");
        query
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

            stats.insert("average_quality".to_string(), json!(avg_quality));
        }

        stats
    }
}

#[async_trait]
impl SwarmAgent for AcoSparqlAgent {
    fn name(&self) -> &str {
        self.base.name()
    }

    fn capabilities(&self) -> Vec<String> {
        self.base.capabilities()
    }

    async fn execute(&self, _context: &SwarmContext, input: AgentInput) -> Result<AgentOutput> {
        debug!("AcoSparqlAgent executing with input type: {}", input.input_type);

        match input.input_type.as_str() {
            "sparql_optimization" | "optimize_query" => {
                // Extract SPARQL query from input
                let query = input.data.get("query")
                    .and_then(|v| v.as_str())
                    .ok_or_else(|| GgenAiError::internal("No query provided in input"))?;

                // Clone self to get mutable access (in real implementation, would use interior mutability)
                let mut agent_clone = AcoSparqlAgent::new(self.name().to_string());
                agent_clone.aco_config = self.aco_config.clone();

                agent_clone.optimize_sparql_query(query).await
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
        // Validate ACO configuration
        if self.aco_config.num_ants == 0 {
            return Ok(false);
        }

        if self.aco_config.max_iterations == 0 {
            return Ok(false);
        }

        Ok(true)
    }

    async fn health_check(&self) -> AgentHealth {
        let mut issues = Vec::new();

        // Check configuration
        if self.aco_config.num_ants < 10 {
            issues.push("ACO ant count is low".to_string());
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
    async fn test_aco_sparql_agent_creation() {
        let agent = AcoSparqlAgent::new("aco-sparql-agent".to_string());

        assert_eq!(agent.name(), "aco-sparql-agent");
        assert!(agent.capabilities().contains(&"sparql_optimization".to_string()));
    }

    #[tokio::test]
    async fn test_aco_sparql_agent_validation() {
        let agent = AcoSparqlAgent::new("aco-sparql-agent".to_string());

        let is_valid = agent.validate().await.unwrap();
        assert!(is_valid);
    }

    #[tokio::test]
    async fn test_aco_sparql_agent_health_check() {
        let agent = AcoSparqlAgent::new("aco-sparql-agent".to_string());

        let health = agent.health_check().await;
        assert!(matches!(health.status, HealthStatus::Healthy));
        assert!(health.score > 0.5);
    }
}
