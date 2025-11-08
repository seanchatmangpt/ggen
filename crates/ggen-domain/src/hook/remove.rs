//! Hook removal - domain logic
//!
//! Pure business logic for removing hooks.

use ggen_utils::error::Result;
use serde::{Deserialize, Serialize};

use super::{HookResult, HookStatus};

/// Arguments for removing a hook
#[derive(Debug, Clone, Default, Serialize, Deserialize)]
pub struct RemoveInput {
    /// Hook ID to remove
    pub hook_id: String,

    /// Force removal without confirmation
    pub force: bool,
}

/// Execute remove hook with input (pure domain function)
pub async fn execute_remove(input: RemoveInput) -> Result<HookResult> {
    use std::fs;
    use std::path::PathBuf;
    use dirs::home_dir;

    if !input.force {
        return Err(ggen_utils::error::Error::new("Use force flag to confirm removal"));
    }

    let hooks_dir = home_dir()
        .ok_or_else(|| ggen_utils::error::Error::new("Home directory not found"))?
        .join(".ggen")
        .join("hooks");

    let hook_file = hooks_dir.join(format!("{}.json", input.hook_id));

    if !hook_file.exists() {
        return Err(ggen_utils::error::Error::new(&format!("Hook not found: {}", input.hook_id)));
    }

    fs::remove_file(&hook_file)
        .map_err(|e| ggen_utils::error::Error::new(&format!("Failed to remove hook: {}", e)))?;

    Ok(HookResult {
        hook_id: input.hook_id,
        status: HookStatus::Removed,
    })
}
