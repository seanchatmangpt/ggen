//! # Agent Implementations
//!
//! Contains implementations of the 12 hyper-advanced agents in the ultrathink system.
//! Each agent specializes in a specific domain of software development and quality assurance.

pub mod byzantene;
pub mod london_bdd;
pub mod swarm_agent;

// Re-export agent implementations for easy access
pub use byzantene::ByzanteneAgent;
pub use london_bdd::LondonBddAgent;
pub use swarm_agent::{SwarmAgent, SwarmAgentType, Trigger, WorkflowResult};

/// Create all agents for the ultrathink system
pub async fn create_all_agents() -> Vec<Box<dyn crate::core::Agent>> {
    vec![
        Box::new(LondonBddAgent::new()),
        Box::new(ByzanteneAgent::new()),
        // Swarm agents for autonomous workflows
        Box::new(SwarmAgent::new(
            "autonomous-coordinator".to_string(),
            SwarmAgentType::AutonomousCoordinator,
            vec![
                "autonomous_workflow".to_string(),
                "requirements_analysis".to_string(),
                "graph_evolution".to_string(),
                "code_regeneration".to_string(),
            ],
        )),
        Box::new(SwarmAgent::new(
            "trigger-monitor".to_string(),
            SwarmAgentType::TriggerMonitor,
            vec![
                "event_monitoring".to_string(),
                "change_detection".to_string(),
                "trigger_generation".to_string(),
            ],
        )),
        Box::new(SwarmAgent::new(
            "knowledge-evolver".to_string(),
            SwarmAgentType::KnowledgeEvolver,
            vec![
                "graph_extension".to_string(),
                "pattern_extraction".to_string(),
                "knowledge_validation".to_string(),
            ],
        )),
        // TODO: Add remaining specialized agents
        // Box::new(TestOracleAgent::new()),
        // Box::new(MockMasterAgent::new()),
        // Box::new(SecuritySentinelAgent::new()),
        // Box::new(AuditArchitectAgent::new()),
        // Box::new(PerformanceProfilerAgent::new()),
        // Box::new(PatternPhilosopherAgent::new()),
        // Box::new(DocsDynamoAgent::new()),
        // Box::new(CookbookCompilerAgent::new()),
        // Box::new(ApiArtisanAgent::new()),
        // Box::new(KnowledgeWeaverAgent::new()),
    ]
}

/// Get agent by name
pub fn get_agent_by_name(name: &str) -> Option<Box<dyn crate::core::Agent>> {
    match name {
        "london-bdd" => Some(Box::new(LondonBddAgent::new())),
        "byzantene" => Some(Box::new(ByzanteneAgent::new())),
        "autonomous-coordinator" => Some(Box::new(SwarmAgent::new(
            "autonomous-coordinator".to_string(),
            SwarmAgentType::AutonomousCoordinator,
            vec![
                "autonomous_workflow".to_string(),
                "requirements_analysis".to_string(),
                "graph_evolution".to_string(),
                "code_regeneration".to_string(),
            ],
        ))),
        "trigger-monitor" => Some(Box::new(SwarmAgent::new(
            "trigger-monitor".to_string(),
            SwarmAgentType::TriggerMonitor,
            vec![
                "event_monitoring".to_string(),
                "change_detection".to_string(),
                "trigger_generation".to_string(),
            ],
        ))),
        "knowledge-evolver" => Some(Box::new(SwarmAgent::new(
            "knowledge-evolver".to_string(),
            SwarmAgentType::KnowledgeEvolver,
            vec![
                "graph_extension".to_string(),
                "pattern_extraction".to_string(),
                "knowledge_validation".to_string(),
            ],
        ))),
        _ => None,
    }
}

/// List all available agent names
pub fn list_agent_names() -> Vec<&'static str> {
    vec![
        "london-bdd",
        "byzantene",
        "autonomous-coordinator",
        "trigger-monitor",
        "knowledge-evolver",
        // TODO: Add remaining agent names
        // "test-oracle",
        // "mock-master",
        // "security-sentinel",
        // "audit-architect",
        // "performance-profiler",
        // "pattern-philosopher",
        // "docs-dynamo",
        // "cookbook-compiler",
        // "api-artisan",
        // "knowledge-weaver",
    ]
}
