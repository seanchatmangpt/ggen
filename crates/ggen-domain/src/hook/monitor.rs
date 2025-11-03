//! Hook monitoring - domain logic
//!
//! Pure business logic for monitoring hooks.

use ggen_utils::error::Result;
use serde::{Deserialize, Serialize};

/// Arguments for monitoring hooks
#[derive(Debug, Clone, Default, Serialize, Deserialize)]
pub struct MonitorInput {
    /// Graph to monitor
    pub graph: String,

    /// Monitoring interval in seconds
    pub interval: u64,

    /// Run once and exit (don't watch continuously)
    pub once: bool,
}

/// Execute monitor hooks with input (pure domain function)
pub async fn execute_monitor(input: MonitorInput) -> Result<MonitorResult> {
    use crate::hook::list::execute_list;
    use crate::hook::ListInput;

    // Load all hooks that match the graph
    let hooks = execute_list(ListInput {
        verbose: false,
        filter: Some(input.graph.clone()),
    }).await?;

    // For now, return basic monitoring status
    // Real implementation would watch file system and trigger hooks
    Ok(MonitorResult {
        active_hooks: hooks.len(),
        watching: 0, // Would be file count in real implementation
        hooks,
    })
}

/// Monitor result
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct MonitorResult {
    pub active_hooks: usize,
    pub watching: usize,
    pub hooks: Vec<crate::hook::list::HookInfo>,
}
