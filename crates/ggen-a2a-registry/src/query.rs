//! Agent discovery query types.

use crate::types::HealthStatus;

/// A filter used to discover agents in the registry.
///
/// All fields are optional; unset fields act as wildcards.
#[derive(Debug, Clone, Default)]
pub struct AgentQuery {
    /// Filter by exact agent type.
    pub agent_type: Option<String>,
    /// Filter by a required capability (substring match).
    pub capability: Option<String>,
    /// Filter by health status.
    pub health: Option<HealthStatus>,
    /// Limit the number of results (0 = no limit).
    pub limit: usize,
}

impl AgentQuery {
    /// Create a new empty query.
    #[must_use]
    pub fn new() -> Self {
        Self::default()
    }

    /// Filter by agent type.
    #[must_use]
    pub fn with_agent_type(mut self, agent_type: impl Into<String>) -> Self {
        self.agent_type = Some(agent_type.into());
        self
    }

    /// Filter by capability.
    #[must_use]
    pub fn with_capability(mut self, capability: impl Into<String>) -> Self {
        self.capability = Some(capability.into());
        self
    }

    /// Filter by health status.
    #[must_use]
    pub const fn with_health(mut self, health: HealthStatus) -> Self {
        self.health = Some(health);
        self
    }

    /// Limit the number of results.
    #[must_use]
    pub const fn with_limit(mut self, limit: usize) -> Self {
        self.limit = limit;
        self
    }
}

/// Check whether an `AgentEntry` matches a given query.
#[must_use]
pub fn matches_query(entry: &crate::types::AgentEntry, query: &AgentQuery) -> bool {
    if let Some(ref agent_type) = query.agent_type {
        if entry.agent_type != *agent_type {
            return false;
        }
    }
    if let Some(ref capability) = query.capability {
        if !entry.capabilities.iter().any(|c| c == capability) {
            return false;
        }
    }
    if let Some(ref health) = query.health {
        if entry.health != *health {
            return false;
        }
    }
    true
}
