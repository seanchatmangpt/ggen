use serde_json::{json, Value};
use crate::error::{Result, get_string_param, get_optional_string_param, success_response};
use chrono::{DateTime, Utc};

/// Register lifecycle hook
pub async fn register(params: Value) -> Result<Value> {
    let event = get_string_param(&params, "event")?;
    let command = get_string_param(&params, "command")?;
    let name = get_optional_string_param(&params, "name")
        .unwrap_or_else(|| format!("hook_{}", chrono::Utc::now().timestamp()));

    tracing::info!("Registering hook: {} for event: {}", name, event);

    // TODO: Replace with actual hook registration logic
    let result = json!({
        "name": name,
        "event": event,
        "command": command,
        "status": "registered",
        "hook_id": format!("hook_{}", uuid::Uuid::new_v4())
    });

    Ok(success_response(result))
}

#[cfg(test)]
mod tests {
    use super::*;
    use serde_json::json;

    #[tokio::test]
    async fn test_register_requires_event_and_command() {
        let params = json!({
            "event": "pre_gen"
        });
        let result = register(params).await;
        assert!(result.is_err());

        let params = json!({
            "command": "echo test"
        });
        let result = register(params).await;
        assert!(result.is_err());
    }
}
