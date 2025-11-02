use serde::{Deserialize, Serialize};

#[derive(Debug, Serialize, Deserialize)]
pub struct HookRemoveOutput {
    pub hook_id: String,
    pub removed: bool,
}

pub async fn remove(hook_id: String) -> Result<HookRemoveOutput, String> {
    // TODO: Remove hook from persistent storage
    // For now, return success

    Ok(HookRemoveOutput {
        hook_id,
        removed: true,
    })
}
