//! Serializable architecture state consumed by doctor and autonomic cycles.

use serde::{Deserialize, Serialize};

use crate::{
    capacity::{CapacityPolicy, CapacitySample},
    registry::ArchitectureRegistry,
};

/// Constitutional limits for autonomic planning.
#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
pub struct AutonomicPolicy {
    /// Whether observation and planning cycles are enabled.
    pub enabled: bool,
    /// Maximum bounded intents emitted by one cycle.
    pub max_intents_per_cycle: usize,
    /// Must remain false: the controller may produce intents, never actuate them.
    pub direct_actuation_allowed: bool,
}

impl Default for AutonomicPolicy {
    fn default() -> Self {
        Self {
            enabled: true,
            max_intents_per_cycle: 64,
            direct_actuation_allowed: false,
        }
    }
}

/// Complete admitted input to architecture diagnostics and autonomic planning.
#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub struct ArchitectureState {
    /// State schema version.
    #[serde(default = "default_schema_version")]
    pub schema_version: u32,
    /// Human-readable enterprise or ecosystem name.
    pub name: String,
    /// Governed asset graph.
    pub registry: ArchitectureRegistry,
    /// Observed capacity evidence.
    #[serde(default)]
    pub capacity_samples: Vec<CapacitySample>,
    /// Capacity warning and refusal budgets.
    #[serde(default)]
    pub capacity_policy: CapacityPolicy,
    /// Autonomic loop constraints.
    #[serde(default)]
    pub autonomic_policy: AutonomicPolicy,
}

const fn default_schema_version() -> u32 {
    1
}
