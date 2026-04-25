//! Individual validation agent implementation
//!
//! Each agent validates one quality dimension independently.
//! Agents are supervised (Armstrong principles) and emit OTEL spans.

use serde::{Deserialize, Serialize};
use std::collections::HashMap;

/// A validation agent
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct ValidationAgent {
    /// Unique agent ID (e.g., "agent-1-compiler")
    pub id: String,
    /// Human-readable name (e.g., "Compiler Gate")
    pub name: String,
    /// ggen crate this agent validates (e.g., "ggen-core")
    pub crate_name: String,
    /// Agent capabilities (e.g., ["compilation", "syntax"])
    pub capabilities: Vec<String>,
    /// Current health status
    #[serde(default = "default_health")]
    pub health: AgentHealth,
    /// Last heartbeat timestamp
    #[serde(default)]
    pub last_heartbeat: Option<chrono::DateTime<chrono::Utc>>,
}

fn default_health() -> AgentHealth {
    AgentHealth::Unknown
}

impl ValidationAgent {
    /// Create a new validation agent
    pub fn new(
        id: impl Into<String>,
        name: impl Into<String>,
        crate_name: impl Into<String>,
        capabilities: Vec<String>,
    ) -> Self {
        Self {
            id: id.into(),
            name: name.into(),
            crate_name: crate_name.into(),
            capabilities,
            health: AgentHealth::Unknown,
            last_heartbeat: None,
        }
    }

    /// Check if agent is healthy (can accept validation tasks)
    pub fn is_healthy(&self) -> bool {
        matches!(self.health, AgentHealth::Healthy)
    }

    /// Check if agent has a specific capability
    pub fn has_capability(&self, capability: &str) -> bool {
        self.capabilities.iter().any(|c| c == capability)
    }
}

/// Agent health status
#[derive(Debug, Clone, Copy, PartialEq, Eq, Serialize, Deserialize)]
pub enum AgentHealth {
    /// Health status unknown (not yet checked)
    Unknown,
    /// Agent is healthy and accepting tasks
    Healthy,
    /// Agent is unhealthy (should not receive tasks)
    Unhealthy,
    /// Agent has failed (supervisor should restart)
    Failed,
}

/// Agent validation result
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct AgentResult {
    /// Agent ID
    pub agent_id: String,
    /// Validation signal (Green/Yellow/Red)
    pub signal: super::gates::AndonSignal,
    /// Human-readable result message
    pub message: String,
    /// Validation timestamp
    pub timestamp: chrono::DateTime<chrono::Utc>,
    /// Additional metadata (violations, warnings, etc.)
    #[serde(default)]
    pub metadata: HashMap<String, String>,
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_agent_creation() {
        let agent = ValidationAgent::new(
            "agent-1-compiler",
            "Compiler Gate",
            "ggen-core",
            vec!["compilation".into()],
        );

        assert_eq!(agent.id, "agent-1-compiler");
        assert_eq!(agent.name, "Compiler Gate");
        assert!(agent.has_capability("compilation"));
        assert!(!agent.has_capability("testing"));
    }

    #[test]
    fn test_agent_health() {
        let mut agent = ValidationAgent::new(
            "agent-1-compiler",
            "Compiler Gate",
            "ggen-core",
            vec![],
        );

        // Unknown by default
        assert!(!agent.is_healthy());

        // Mark as healthy
        agent.health = AgentHealth::Healthy;
        assert!(agent.is_healthy());
    }
}
