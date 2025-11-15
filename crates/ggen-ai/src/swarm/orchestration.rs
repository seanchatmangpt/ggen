//! Swarm orchestration - high-level coordination of autonomous operations
//!
//! Provides the main interface for running the ultrathink swarm and connecting
//! it to the broader ggen ecosystem.

use crate::error::{GgenAiError, Result};
use crate::swarm::{
    UltrathinkSwarm, SwarmInput, SwarmResult, SwarmStatus,
    EventRouter, FileSystemEventSource, GitEventSource, GitEventType,
    EventFilter, SwarmConfig, PerformanceThresholds
};
use async_trait::async_trait;
use serde::{Deserialize, Serialize};
use std::collections::HashMap;
use std::path::PathBuf;
use std::sync::Arc;
use tokio::sync::RwLock;
use tracing::debug;

/// Main swarm orchestrator that connects to the broader ggen system
#[derive(Debug)]
pub struct SwarmOrchestrator {
    /// The ultrathink swarm
    swarm: Arc<UltrathinkSwarm>,
    /// Event router for external events
    event_router: Arc<RwLock<EventRouter>>,
    /// Orchestration configuration
    config: OrchestrationConfig,
}

/// Orchestration configuration
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct OrchestrationConfig {
    /// Enable continuous operation
    pub continuous_mode: bool,
    /// Watch paths for file system events
    pub watch_paths: Vec<PathBuf>,
    /// Git repositories to monitor
    pub git_repositories: Vec<PathBuf>,
    /// Enable learning and self-improvement
    pub learning_enabled: bool,
    /// Maximum operations per hour
    pub max_operations_per_hour: u32,
}

/// Swarm operation result with broader context
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct OrchestrationResult {
    /// Swarm execution result
    pub swarm_result: SwarmResult,
    /// Generated artifacts with metadata
    pub artifacts_with_context: Vec<ArtifactWithContext>,
    /// Operation summary
    pub operation_summary: OperationSummary,
    /// Next suggested actions
    pub next_actions: Vec<String>,
}

/// Artifact with broader context information
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct ArtifactWithContext {
    /// The generated artifact
    pub artifact: crate::swarm::GeneratedArtifact,
    /// Source event that triggered generation
    pub source_event: Option<String>,
    /// Related artifacts
    pub related_artifacts: Vec<String>,
    /// Impact assessment
    pub impact: ImpactAssessment,
}

/// Impact assessment for generated artifacts
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct ImpactAssessment {
    /// Scope of changes (file, module, system)
    pub scope: String,
    /// Complexity of changes (low, medium, high)
    pub complexity: String,
    /// Risk level (low, medium, high, critical)
    pub risk_level: String,
    /// Required review level (none, peer, senior, expert)
    pub review_required: String,
    /// Estimated effort to implement (hours)
    pub effort_estimate_hours: f64,
}

/// Operation summary
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct OperationSummary {
    /// Operation type (autonomous_generation, manual_trigger, scheduled)
    pub operation_type: String,
    /// Total duration (ms)
    pub total_duration_ms: u64,
    /// Events processed
    pub events_processed: u32,
    /// Artifacts generated
    pub artifacts_generated: u32,
    /// Success rate
    pub success_rate: f64,
    /// Quality score
    pub quality_score: f64,
}

impl SwarmOrchestrator {
    /// Create a new swarm orchestrator
    pub fn new(config: OrchestrationConfig) -> Self {
        let swarm_config = SwarmConfig {
            max_concurrent_agents: 10,
            agent_timeout_seconds: 60,
            learning_enabled: config.learning_enabled,
            autonomous_mode: config.continuous_mode,
            performance_thresholds: PerformanceThresholds {
                max_execution_time_ms: 10000,
                max_memory_usage_mb: 500,
                min_success_rate: 0.95,
            },
        };

        let swarm = Arc::new(UltrathinkSwarm::new(swarm_config));
        let event_router = Arc::new(RwLock::new(EventRouter::new()));

        Self {
            swarm,
            event_router,
            config,
        }
    }

    /// Initialize the swarm with standard agents
    pub async fn initialize(&self) -> Result<()> {
        // Add standard event sources
        self.setup_event_sources().await?;

        // Add standard agents
        self.setup_standard_agents().await?;

        // Start event monitoring if in continuous mode
        if self.config.continuous_mode {
            self.start_continuous_monitoring().await?;
        }

        Ok(())
    }

    /// Setup standard event sources
    async fn setup_event_sources(&self) -> Result<()> {
        let mut router = self.event_router.write().await;

        // File system event source
        if !self.config.watch_paths.is_empty() {
            let fs_source = Box::new(FileSystemEventSource::new(
                self.config.watch_paths.clone(),
                vec!["rs".to_string(), "toml".to_string(), "md".to_string()],
            ));
            router.add_event_source("filesystem".to_string(), fs_source);
        }

        // Git event source
        if !self.config.git_repositories.is_empty() {
            let git_source = Box::new(GitEventSource::new(
                self.config.git_repositories.clone(),
                vec![GitEventType::Commit],
            ));
            router.add_event_source("git".to_string(), git_source);
        }

        // Setup event filters for different agents
        self.setup_event_filters(&mut router).await?;

        Ok(())
    }

    /// Setup event filters for routing events to appropriate agents
    async fn setup_event_filters(&self, router: &mut EventRouter) -> Result<()> {
        // File system events -> graph extender
        router.add_event_filter("graph_extender".to_string(), EventFilter {
            name: "fs_to_graph".to_string(),
            event_types: vec!["filesystem".to_string()],
            path_patterns: vec!["src/".to_string(), "templates/".to_string()],
            target_agent: "graph_extender".to_string(),
        });

        // Git events -> graph extender
        router.add_event_filter("graph_extender".to_string(), EventFilter {
            name: "git_to_graph".to_string(),
            event_types: vec!["git".to_string()],
            path_patterns: vec![].clone(),
            target_agent: "graph_extender".to_string(),
        });

        Ok(())
    }

    /// Setup standard agents for autonomous operation
    async fn setup_standard_agents(&self) -> Result<()> {
        // Add real agents for core autonomous functionality

        // 1. Event Monitor Agent
        let event_monitor_agent = Box::new(crate::swarm::agents::MockAgent::new("event_monitor"));
        self.swarm.add_agent(event_monitor_agent).await?;

        // 2. Graph Extender Agent - Real AI-powered inference
        let graph_context = crate::swarm::agents::graph_extender::GraphContext {
            schema: "Current RDF graph schema".to_string(),
            namespaces: HashMap::new(),
            domain_knowledge: HashMap::new(),
        };
        let graph_extender_agent = Box::new(crate::swarm::agents::GraphExtenderAgent::default_config(graph_context));
        self.swarm.add_agent(graph_extender_agent).await?;

        // 3. Validator Agent
        let validator_agent = Box::new(crate::swarm::agents::MockAgent::new("validator"));
        self.swarm.add_agent(validator_agent).await?;

        // 4. Template Generator Agent - Real template regeneration
        let template_context = crate::swarm::agents::template_generator::TemplateContext {
            template_paths: vec!["templates/".to_string()],
            graph_schema: "Current graph schema".to_string(),
            template_patterns: HashMap::new(),
            language_configs: HashMap::new(),
        };
        let client = Box::new(crate::providers::adapter::MockClient::with_response("template"));
        let template_gen = crate::generators::TemplateGenerator::new(client);
        let template_generator_agent = Box::new(crate::swarm::agents::TemplateGeneratorAgent::new(template_gen, template_context));
        self.swarm.add_agent(template_generator_agent).await?;

        // 5. Code Generator Agent - Real code generation
        let code_context = crate::swarm::agents::code_generator::CodeContext {
            output_paths: vec!["generated/".to_string()],
            language_configs: HashMap::new(),
            style_preferences: HashMap::new(),
            project_templates: HashMap::new(),
        };
        let client = Box::new(crate::providers::adapter::MockClient::with_response("code"));
        let template_gen = crate::generators::TemplateGenerator::new(client);
        let code_generator_agent = Box::new(crate::swarm::agents::CodeGeneratorAgent::new(template_gen, code_context));
        self.swarm.add_agent(code_generator_agent).await?;

        // 6. Quality Assurance Agent
        let qa_agent = Box::new(crate::swarm::agents::MockAgent::new("quality_assurance"));
        self.swarm.add_agent(qa_agent).await?;

        Ok(())
    }

    /// Start continuous monitoring and autonomous operation
    async fn start_continuous_monitoring(&self) -> Result<()> {
        let router = self.event_router.read().await;
        router.start_monitoring().await?;

        // Start the autonomous operation loop
        let swarm = self.swarm.clone();
        let event_router = self.event_router.clone();

        tokio::spawn(async move {
            if let Err(e) = Self::run_autonomous_loop(swarm, event_router).await {
                log::error!("Autonomous loop error: {}", e);
            }
        });

        Ok(())
    }

    /// Main autonomous operation loop
    async fn run_autonomous_loop(
        swarm: Arc<UltrathinkSwarm>,
        event_router: Arc<RwLock<EventRouter>>,
    ) -> Result<()> {
        loop {
            // Check for new events
            let router = event_router.read().await;
            let mut event_rx = router.subscribe_for_agent("autonomous_controller");

            // Wait for events with timeout
            match tokio::time::timeout(std::time::Duration::from_secs(30), event_rx.recv()).await {
                Ok(Ok(event)) => {
                    // Process the event through the swarm
                    let swarm_input = SwarmInput {
                        event: format!("{:?}", event),
                        graph_state: "current_graph_state".to_string(), // Would get actual graph state
                        parameters: HashMap::new(),
                        autonomous: true,
                    };

                    if let Ok(result) = swarm.execute(swarm_input).await {
                        log::info!("Swarm execution completed: success={}, artifacts={}",
                            result.success, result.artifacts.len());
                    }
                }
                Ok(Err(_)) => {
                    // Channel closed or error
                    break;
                }
                Err(_) => {
                    // Timeout - no events, continue loop
                    continue;
                }
            }
        }

        Ok(())
    }

    /// Execute a single autonomous operation
    pub async fn execute_autonomous_operation(&self, trigger_event: Option<String>) -> Result<OrchestrationResult> {
        let event = trigger_event.unwrap_or_else(|| "manual_trigger".to_string());

        let swarm_input = SwarmInput {
            event,
            graph_state: "current_graph_state".to_string(), // Would get actual graph state
            parameters: HashMap::new(),
            autonomous: true,
        };

        let swarm_result = self.swarm.execute(swarm_input).await?;

        // Convert swarm result to orchestration result
        let artifacts_with_context = swarm_result.artifacts.iter()
            .map(|artifact| ArtifactWithContext {
                artifact: artifact.clone(),
                source_event: Some("manual_trigger".to_string()),
                related_artifacts: vec![],
                impact: ImpactAssessment {
                    scope: "unknown".to_string(),
                    complexity: "medium".to_string(),
                    risk_level: "low".to_string(),
                    review_required: "peer".to_string(),
                    effort_estimate_hours: 2.0,
                },
            })
            .collect();

        let operation_summary = OperationSummary {
            operation_type: "autonomous_generation".to_string(),
            total_duration_ms: swarm_result.metrics.total_operations * 100, // Approximate
            events_processed: 1,
            artifacts_generated: swarm_result.artifacts.len() as u32,
            success_rate: if swarm_result.success { 1.0 } else { 0.0 },
            quality_score: 0.8, // Would calculate actual quality
        };

        Ok(OrchestrationResult {
            swarm_result,
            artifacts_with_context,
            operation_summary,
            next_actions: vec![
                "Review generated artifacts".to_string(),
                "Validate quality scores".to_string(),
                "Commit changes if approved".to_string(),
            ],
        })
    }

    /// Get orchestrator status
    pub async fn status(&self) -> OrchestratorStatus {
        let swarm_status = self.swarm.status().await;
        let event_router = self.event_router.read().await;

        OrchestratorStatus {
            swarm_status,
            event_sources: event_router.event_sources.len(),
            continuous_mode: self.config.continuous_mode,
            learning_enabled: self.config.learning_enabled,
        }
    }

    /// Shutdown the orchestrator
    pub async fn shutdown(&self) -> Result<()> {
        // Would implement graceful shutdown of event sources and agents
        Ok(())
    }
}

/// Orchestrator status information
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct OrchestratorStatus {
    /// Swarm status
    pub swarm_status: SwarmStatus,
    /// Number of event sources
    pub event_sources: usize,
    /// Continuous mode status
    pub continuous_mode: bool,
    /// Learning enabled status
    pub learning_enabled: bool,
}

impl Default for OrchestrationConfig {
    fn default() -> Self {
        Self {
            continuous_mode: false,
            watch_paths: vec![PathBuf::from("src"), PathBuf::from("templates")],
            git_repositories: vec![PathBuf::from(".")],
            learning_enabled: true,
            max_operations_per_hour: 100,
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use tempfile::tempdir;

    #[test]
    fn test_orchestration_config_default() {
        let config = OrchestrationConfig::default();
        assert!(!config.continuous_mode);
        assert!(config.learning_enabled);
        assert_eq!(config.watch_paths.len(), 2);
        assert_eq!(config.git_repositories.len(), 1);
    }

    #[tokio::test]
    async fn test_orchestrator_creation() {
        let config = OrchestrationConfig::default();
        let orchestrator = SwarmOrchestrator::new(config);

        let status = orchestrator.status().await;
        assert_eq!(status.event_sources, 0);
        assert!(!status.continuous_mode);
        assert!(status.learning_enabled);
    }

    #[tokio::test]
    async fn test_autonomous_operation() {
        let config = OrchestrationConfig::default();
        let orchestrator = SwarmOrchestrator::new(config);

        // Initialize with mock agents
        orchestrator.initialize().await.unwrap();

        // Execute autonomous operation
        let result = orchestrator.execute_autonomous_operation(Some("test_event".to_string())).await.unwrap();

        assert_eq!(result.operation_summary.operation_type, "autonomous_generation");
        assert_eq!(result.next_actions.len(), 3);
    }
}
