//! # Agent Implementations
//!
//! Contains implementations of the 12 hyper-advanced agents in the ultrathink system.
//! Each agent specializes in a specific domain of software development and quality assurance.

pub mod london_bdd;
pub mod byzantene;

// Re-export agent implementations for easy access
pub use london_bdd::LondonBddAgent;
pub use byzantene::ByzanteneAgent;

/// Create all agents for the ultrathink system
pub async fn create_all_agents() -> Vec<Box<dyn crate::core::Agent>> {
    vec![
        Box::new(LondonBddAgent::new()),
        Box::new(ByzanteneAgent::new()),
        // TODO: Add remaining 10 agents
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
        _ => None,
    }
}

/// List all available agent names
pub fn list_agent_names() -> Vec<&'static str> {
    vec![
        "london-bdd",
        "byzantene",
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


