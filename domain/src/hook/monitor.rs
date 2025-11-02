use serde::{Deserialize, Serialize};

#[derive(Debug, Serialize, Deserialize)]
pub struct HookMonitorOutput {
    pub graph: String,
    pub watching: Vec<String>,
    pub active_hooks: usize,
}

pub async fn monitor(
    graph: String,
    interval: Option<u64>,
) -> Result<HookMonitorOutput, String> {
    let _interval_secs = interval.unwrap_or(5);

    // TODO: Implement file watching and hook triggering
    // For now, return monitoring status

    Ok(HookMonitorOutput {
        graph,
        watching: vec![],
        active_hooks: 0,
    })
}
