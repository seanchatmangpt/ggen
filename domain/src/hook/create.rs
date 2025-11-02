use serde::{Deserialize, Serialize};

#[derive(Debug, Serialize, Deserialize)]
pub struct HookCreateOutput {
    pub hook_id: String,
    pub trigger: String,
    pub action: String,
    pub success: bool,
}

pub async fn create(
    trigger: String,
    action: String,
    name: Option<String>,
) -> Result<HookCreateOutput, String> {
    // Generate hook ID
    let hook_id = name.clone().unwrap_or_else(|| {
        format!(
            "hook_{}_{}",
            trigger.replace([':', ' '], "_"),
            std::time::SystemTime::now()
                .duration_since(std::time::UNIX_EPOCH)
                .unwrap()
                .as_secs()
        )
    });

    // TODO: Implement hook persistence and registration
    // For now, return success response

    Ok(HookCreateOutput {
        hook_id,
        trigger,
        action,
        success: true,
    })
}
