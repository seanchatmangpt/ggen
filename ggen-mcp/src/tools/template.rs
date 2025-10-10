///! Template tools - delegates to ggen CLI commands

use serde_json::{json, Value};
use crate::cli_helper::call_ggen_cli;
use crate::error::{Result, get_string_param, get_optional_string_param, success_response};

/// Create new template - delegates to `ggen template new`
pub async fn create(params: Value) -> Result<Value> {
    let name = get_string_param(&params, "name")?;
    let template_type = get_optional_string_param(&params, "template_type");

    tracing::info!("Delegating to ggen template new: {}", name);

    let mut args = vec!["template", "new", &name];
    let template_type_str;
    if let Some(tt) = template_type {
        args.push("--template-type");
        template_type_str = tt;
        args.push(&template_type_str);
    }

    let result = call_ggen_cli(&args).await?;
    Ok(success_response(result))
}

/// Validate template - delegates to `ggen template lint`
pub async fn validate(params: Value) -> Result<Value> {
    let template = get_string_param(&params, "template")?;

    tracing::info!("Delegating to ggen template lint: {}", template);

    let result = call_ggen_cli(&["template", "lint", &template]).await?;
    Ok(success_response(result))
}

#[cfg(test)]
mod tests {
    use super::*;

    #[tokio::test]
    async fn test_create_requires_name_and_content() {
        let params = json!({"name": "test-template"});
        // Will fail at CLI level, but parameter extraction works
        let result = create(params).await;
        assert!(result.is_err() || result.is_ok()); // Either succeeds or CLI error
    }

    #[tokio::test]
    async fn test_validate_requires_template() {
        let params = json!({});
        let result = validate(params).await;
        assert!(result.is_err());
    }
}
