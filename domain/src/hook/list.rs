use serde::{Deserialize, Serialize};

#[derive(Debug, Serialize, Deserialize)]
pub struct HookInfo {
    pub id: String,
    pub name: Option<String>,
    pub trigger: String,
    pub action: String,
    pub enabled: bool,
}

#[derive(Debug, Serialize, Deserialize)]
pub struct HookListOutput {
    pub hooks: Vec<HookInfo>,
    pub count: usize,
}

pub async fn list() -> Result<HookListOutput, String> {
    // TODO: Load hooks from persistent storage
    // For now, return empty list

    Ok(HookListOutput {
        hooks: vec![],
        count: 0,
    })
}
