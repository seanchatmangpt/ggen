//! Hook creation - domain logic
//!
//! Pure business logic for creating hooks.

use ggen_utils::error::Result;
use serde::{Deserialize, Serialize};

/// Arguments for creating a hook
#[derive(Debug, Clone, Default, Serialize, Deserialize)]
pub struct CreateInput {
    /// Hook trigger event (e.g., "file:change", "template:render")
    pub trigger: String,

    /// Action to execute (shell command or script path)
    pub action: String,

    /// Optional hook name
    pub name: Option<String>,
}

/// Execute create hook with input (pure domain function)
pub async fn execute_create(input: CreateInput) -> Result<HookResult> {
    use dirs::home_dir;
    use std::fs;

    // Generate hook ID
    let hook_id = input.name.clone().unwrap_or_else(|| {
        // SystemTime::now() is always after UNIX_EPOCH, so this is safe
        #[allow(clippy::expect_used)]
        let timestamp = std::time::SystemTime::now()
            .duration_since(std::time::UNIX_EPOCH)
            .expect("SystemTime::now() should always be after UNIX_EPOCH")
            .as_secs();
        format!(
            "hook_{}_{}",
            input.trigger.replace([':', ' '], "_"),
            timestamp
        )
    });

    // Create hooks directory
    let hooks_dir = home_dir()
        .ok_or_else(|| ggen_utils::error::Error::new("Home directory not found"))?
        .join(".ggen")
        .join("hooks");

    fs::create_dir_all(&hooks_dir).map_err(|e| {
        ggen_utils::error::Error::new(&format!("Failed to create hooks directory: {}", e))
    })?;

    // Create hook file
    let hook_file = hooks_dir.join(format!("{}.json", hook_id));
    let hook_data = serde_json::json!({
        "id": hook_id.clone(),
        "trigger": input.trigger,
        "action": input.action,
        "created_at": chrono::Utc::now().to_rfc3339(),
    });

    fs::write(&hook_file, serde_json::to_string_pretty(&hook_data)?)
        .map_err(|e| ggen_utils::error::Error::new(&format!("Failed to write hook file: {}", e)))?;

    Ok(HookResult {
        hook_id,
        status: HookStatus::Created,
    })
}

/// Hook execution result
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct HookResult {
    pub hook_id: String,
    pub status: HookStatus,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub enum HookStatus {
    Created,
    Removed,
    ListFound,
}
