//! Mock agent for testing swarm functionality

use crate::error::Result;
use crate::swarm::{
    AgentHealth, HealthStatus, SwarmAgent, SwarmContext, AgentInput, AgentOutput,
    AgentConfig, PerformanceThresholds
};
use async_trait::async_trait;
use serde_json::Value;
use std::collections::HashMap;

/// Mock agent for testing swarm functionality
#[derive(Debug)]
pub struct MockAgent {
    name: String,
    capabilities: Vec<String>,
    config: AgentConfig,
    execution_count: std::sync::atomic::AtomicU64,
}

impl MockAgent {
    /// Create a new mock agent
    pub fn new(name: &str) -> Self {
        Self {
            name: name.to_string(),
            capabilities: vec![
                "mock_execution".to_string(),
                "test_validation".to_string(),
            ],
            config: AgentConfig {
                timeout_seconds: 30,
                retry_attempts: 3,
                verbose_logging: false,
                performance_thresholds: PerformanceThresholds {
                    max_execution_time_ms: 5000,
                    max_memory_usage_mb: 100,
                    min_quality_score: 0.8,
                },
            },
            execution_count: std::sync::atomic::AtomicU64::new(0),
        }
    }

    /// Get execution count
    pub fn execution_count(&self) -> u64 {
        self.execution_count.load(std::sync::atomic::Ordering::Relaxed)
    }
}

impl SwarmAgent for MockAgent {
    fn name(&self) -> &str {
        &self.name
    }

    fn capabilities(&self) -> Vec<String> {
        self.capabilities.clone()
    }

    async fn execute(&self, _context: &SwarmContext, input: AgentInput) -> Result<AgentOutput> {
        // Simulate some processing time
        tokio::time::sleep(std::time::Duration::from_millis(10)).await;

        self.execution_count.fetch_add(1, std::sync::atomic::Ordering::Relaxed);

        // Echo back the input data
        Ok(AgentOutput {
            data: input.data,
            output_type: "mock_output".to_string(),
            target_agents: vec!["validator".to_string(), "quality_assurance".to_string()],
            metadata: {
                let mut metadata = HashMap::new();
                metadata.insert("execution_count".to_string(), self.execution_count().to_string());
                metadata.insert("processing_time_ms".to_string(), "10".to_string());
                metadata
            },
        })
    }

    async fn validate(&self) -> Result<bool> {
        // Always return true for mock agent
        Ok(true)
    }

    async fn health_check(&self) -> AgentHealth {
        AgentHealth {
            status: HealthStatus::Healthy,
            score: 1.0,
            last_check: chrono::Utc::now().to_rfc3339(),
            issues: vec![],
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::swarm::{SwarmConfig, SwarmContext, PerformanceThresholds};

    #[tokio::test]
    async fn test_mock_agent_creation() {
        let agent = MockAgent::new("test-mock");
        assert_eq!(agent.name(), "test-mock");
        assert_eq!(agent.capabilities().len(), 2);
        assert!(agent.capabilities().contains(&"mock_execution".to_string()));
    }

    #[tokio::test]
    async fn test_mock_agent_execution() {
        let agent = MockAgent::new("test-agent");
        let context = SwarmContext {
            graph_state: "test_graph".to_string(),
            active_agents: vec![],
            metrics: Default::default(),
            config: SwarmConfig {
                max_concurrent_agents: 5,
                agent_timeout_seconds: 30,
                learning_enabled: true,
                autonomous_mode: false,
                performance_thresholds: PerformanceThresholds {
                    max_execution_time_ms: 5000,
                    max_memory_usage_mb: 100,
                    min_success_rate: 0.95,
                },
            },
        };

        let input = AgentInput {
            data: Value::String("test_input".to_string()),
            input_type: "test".to_string(),
            source_agent: None,
            context: HashMap::new(),
        };

        let output = agent.execute(&context, input).await.unwrap();
        assert_eq!(output.output_type, "mock_output");
        assert_eq!(agent.execution_count(), 1);
    }

    #[tokio::test]
    async fn test_mock_agent_health() {
        let agent = MockAgent::new("healthy-agent");
        let health = agent.health_check().await;
        assert!(matches!(health.status, HealthStatus::Healthy));
        assert_eq!(health.score, 1.0);
    }
}
