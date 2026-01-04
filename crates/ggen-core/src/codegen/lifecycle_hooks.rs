use std::collections::BTreeMap;

/// Phase configuration structure for sync lifecycle
#[derive(Debug, Clone)]
pub struct Phase {
    pub name: String,
    pub hooks: Option<Vec<String>>,
}

pub struct SyncLifecycleHooks {
    phases: BTreeMap<String, Phase>,
}

impl SyncLifecycleHooks {
    /// Create new lifecycle hooks manager
    pub fn new() -> Self {
        Self {
            phases: BTreeMap::new(),
        }
    }

    /// Register a phase with its hooks
    pub fn register_phase(&mut self, phase_name: String, hooks: Vec<String>) {
        self.phases.insert(
            phase_name.clone(),
            Phase {
                name: phase_name,
                hooks: if hooks.is_empty() { None } else { Some(hooks) },
            },
        );
    }

    /// Get hooks for a specific phase
    pub fn get_phase_hooks(&self, phase_name: &str) -> Option<Vec<String>> {
        self.phases.get(phase_name).and_then(|p| p.hooks.clone())
    }

    /// Check if a phase exists
    pub fn has_phase(&self, phase_name: &str) -> bool {
        self.phases.contains_key(phase_name)
    }
}

pub struct SyncPhaseConfig {
    pub before_sync: Vec<String>,
    pub after_sync: Vec<String>,
    pub before_generation: Vec<String>,
    pub after_generation: Vec<String>,
    pub parallel_rules: bool,
    pub max_parallelism: Option<usize>,
}

impl Default for SyncPhaseConfig {
    fn default() -> Self {
        Self {
            before_sync: vec![],
            after_sync: vec![],
            before_generation: vec![],
            after_generation: vec![],
            parallel_rules: true,
            max_parallelism: None,
        }
    }
}
