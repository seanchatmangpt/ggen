use crate::cli_helper::{call_ggen_cli, call_ggen_with_vars};
use crate::error::{
    get_bool_param, get_optional_object_param, get_string_param, success_response, Result,
};
///! Project tools - delegates to ggen CLI commands
use serde_json::Value;

/// Generate project from template - delegates to `ggen project gen`
pub async fn gen(params: Value) -> Result<Value> {
    let template = get_string_param(&params, "template")?;
    let vars = get_optional_object_param(&params, "vars").unwrap_or_default();
    let dry_run = get_bool_param(&params, "dry_run", false);
    let force = get_bool_param(&params, "force", false);

    tracing::info!("Delegating to ggen project gen: {}", template);

    let mut args = vec!["project", "gen", &template];
    if dry_run {
        args.push("--dry-run");
    }
    if force {
        args.push("--force");
    }

    let result = call_ggen_with_vars(&args, &vars).await?;
    Ok(success_response(result))
}

/// Plan project generation - delegates to `ggen project plan`
pub async fn plan(params: Value) -> Result<Value> {
    let template = get_string_param(&params, "template")?;
    let vars = get_optional_object_param(&params, "vars").unwrap_or_default();

    tracing::info!("Delegating to ggen project plan: {}", template);

    let result = call_ggen_with_vars(&["project", "plan", &template], &vars).await?;
    Ok(success_response(result))
}

/// Apply execution plan - delegates to `ggen project apply`
pub async fn apply(params: Value) -> Result<Value> {
    let plan = get_string_param(&params, "plan")?;

    tracing::info!("Delegating to ggen project apply");

    let result = call_ggen_cli(&["project", "apply", &plan]).await?;
    Ok(success_response(result))
}

/// Show diff - delegates to `ggen project diff`
pub async fn diff(params: Value) -> Result<Value> {
    let template = get_string_param(&params, "template")?;
    let vars = get_optional_object_param(&params, "vars").unwrap_or_default();

    tracing::info!("Delegating to ggen project diff: {}", template);

    let result = call_ggen_with_vars(&["project", "diff", &template], &vars).await?;
    Ok(success_response(result))
}

#[cfg(test)]
mod tests {
    use super::*;
    use serde_json::json;

    #[tokio::test]
    async fn test_gen_requires_template() {
        let params = json!({});
        let result = gen(params).await;
        assert!(result.is_err());
    }

    #[tokio::test]
    async fn test_plan_requires_template() {
        let params = json!({});
        let result = plan(params).await;
        assert!(result.is_err());
    }

    #[tokio::test]
    async fn test_apply_requires_plan() {
        let params = json!({});
        let result = apply(params).await;
        assert!(result.is_err());
    }
}
