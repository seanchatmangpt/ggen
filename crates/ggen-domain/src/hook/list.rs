//! Hook listing - domain logic
//!
//! Pure business logic for listing hooks.

use ggen_utils::error::Result;
use serde::{Deserialize, Serialize};

/// Arguments for listing hooks
#[derive(Debug, Clone, Default, Serialize, Deserialize)]
pub struct ListInput {
    /// Show detailed hook information
    pub verbose: bool,

    /// Filter by trigger pattern
    pub filter: Option<String>,
}

/// Execute list hooks with input (pure domain function)
pub async fn execute_list(input: ListInput) -> Result<Vec<HookInfo>> {
    use dirs::home_dir;
    use glob::glob;
    use std::fs;
    use std::path::PathBuf;

    let hooks_dir = home_dir()
        .ok_or_else(|| ggen_utils::error::Error::new("Home directory not found"))?
        .join(".ggen")
        .join("hooks");

    if !hooks_dir.exists() {
        return Ok(vec![]);
    }

    let mut hooks = Vec::new();
    let pattern = hooks_dir.join("*.json").to_string_lossy().to_string();

    for entry in glob(&pattern)
        .map_err(|e| ggen_utils::error::Error::new(&format!("Invalid glob pattern: {}", e)))?
    {
        let entry = entry.map_err(|e| {
            ggen_utils::error::Error::new(&format!("Error reading directory: {}", e))
        })?;

        if let Ok(content) = fs::read_to_string(&entry) {
            if let Ok(hook_data) = serde_json::from_str::<serde_json::Value>(&content) {
                let trigger = hook_data
                    .get("trigger")
                    .and_then(|v| v.as_str())
                    .unwrap_or("")
                    .to_string();

                // Apply filter if specified
                if let Some(ref filter) = input.filter {
                    if !trigger.contains(filter) {
                        continue;
                    }
                }

                hooks.push(HookInfo {
                    id: hook_data
                        .get("id")
                        .and_then(|v| v.as_str())
                        .unwrap_or("")
                        .to_string(),
                    trigger,
                    action: hook_data
                        .get("action")
                        .and_then(|v| v.as_str())
                        .unwrap_or("")
                        .to_string(),
                    created_at: hook_data
                        .get("created_at")
                        .and_then(|v| v.as_str())
                        .unwrap_or("")
                        .to_string(),
                });
            }
        }
    }

    Ok(hooks)
}

/// Hook information
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct HookInfo {
    pub id: String,
    pub trigger: String,
    pub action: String,
    pub created_at: String,
}
