//! Core types for the A2A registry: `AgentEntry`, `HealthStatus`, `Registration`.

use chrono::{DateTime, Utc};
use serde::{Deserialize, Serialize};

/// Health status of a registered agent.
///
/// This is the registry's own view of agent health, kept simple for
/// fast lookups.  The richer `a2a_generated::converged::HealthStatus`
/// is used at the protocol level; this type maps to/from it where needed.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Serialize, Deserialize)]
pub enum HealthStatus {
    /// Health has not yet been checked.
    Unknown,
    /// The agent responded to its last health check.
    Healthy,
    /// The agent is responding but exhibiting degraded behavior.
    Degraded,
    /// The agent failed its last health check.
    Unhealthy,
    /// The agent has been explicitly marked offline or could not be reached
    /// for several consecutive intervals.
    Offline,
}

impl std::fmt::Display for HealthStatus {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Unknown => write!(f, "unknown"),
            Self::Healthy => write!(f, "healthy"),
            Self::Degraded => write!(f, "degraded"),
            Self::Unhealthy => write!(f, "unhealthy"),
            Self::Offline => write!(f, "offline"),
        }
    }
}

/// A lightweight record stored in the registry for each registered agent.
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct AgentEntry {
    /// Unique identifier (e.g. UUID).
    pub id: String,
    /// Human-readable name.
    pub name: String,
    /// Agent type classification (e.g. "code-generator", "validator").
    pub agent_type: String,
    /// URL where the agent can be reached.
    pub endpoint_url: String,
    /// Capabilities the agent advertises (e.g. `"sparql"`, `"code-gen"`).
    pub capabilities: Vec<String>,
    /// Current health status.
    pub health: HealthStatus,
    /// When the agent was first registered.
    pub registered_at: DateTime<Utc>,
    /// Timestamp of the last successful heartbeat.
    pub last_heartbeat: DateTime<Utc>,
}

/// Returned after a successful registration.
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct Registration {
    /// The agent ID that was registered.
    pub agent_id: String,
    /// Timestamp of the registration event.
    pub registered_at: DateTime<Utc>,
}

/// Configuration for the background health monitor.
#[derive(Debug, Clone)]
pub struct HealthConfig {
    /// How often to ping each agent.
    pub check_interval: std::time::Duration,
    /// Per-ping HTTP timeout.
    pub ping_timeout: std::time::Duration,
    /// Number of consecutive failures before marking an agent `Offline`.
    pub offline_threshold: u32,
}

impl Default for HealthConfig {
    fn default() -> Self {
        Self {
            check_interval: std::time::Duration::from_secs(60),
            ping_timeout: std::time::Duration::from_secs(10),
            offline_threshold: 3,
        }
    }
}
