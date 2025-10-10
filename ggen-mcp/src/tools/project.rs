use serde_json::{json, Value};
use crate::error::{Result, get_string_param, get_optional_string_param, success_response};

/// Generate project from template
pub async fn gen(params: Value) -> Result<Value> {
    let template = get_string_param(&params, "template")?;
    let vars = params.get("vars").cloned();
    let output = get_optional_string_param(&params, "output");

    tracing::info!("Generating project from template: {}", template);

    // TODO: Replace with actual ggen-core API call
    // For now, simulate the response
    let result = json!({
        "template": template,
        "output_dir": output.unwrap_or_else(|| ".".to_string()),
        "files_generated": [
            "README.md",
            "src/main.rs",
            "Cargo.toml"
        ],
        "variables_applied": vars,
        "status": "completed"
    });

    Ok(success_response(result))
}

/// Create execution plan without applying
pub async fn plan(params: Value) -> Result<Value> {
    let template = get_string_param(&params, "template")?;
    let vars = params.get("vars").cloned();

    tracing::info!("Creating execution plan for template: {}", template);

    // TODO: Replace with actual ggen-core API call
    let result = json!({
        "template": template,
        "plan_id": format!("plan_{}", chrono::Utc::now().timestamp()),
        "actions": [
            {
                "type": "create_file",
                "path": "README.md",
                "content_preview": "# Generated Project..."
            },
            {
                "type": "create_file",
                "path": "src/main.rs",
                "content_preview": "fn main() { ... }"
            }
        ],
        "variables": vars,
        "estimated_files": 3
    });

    Ok(success_response(result))
}

/// Apply execution plan
pub async fn apply(params: Value) -> Result<Value> {
    let plan = get_string_param(&params, "plan")?;

    tracing::info!("Applying execution plan");

    // TODO: Replace with actual ggen-core API call
    let result = json!({
        "plan_applied": plan,
        "files_created": 3,
        "files_modified": 0,
        "status": "success"
    });

    Ok(success_response(result))
}

/// Show diff between template and existing files
pub async fn diff(params: Value) -> Result<Value> {
    let template = get_string_param(&params, "template")?;
    let target = get_optional_string_param(&params, "target").unwrap_or_else(|| ".".to_string());
    let vars = params.get("vars").cloned();

    tracing::info!("Computing diff for template: {} against target: {}", template, target);

    // TODO: Replace with actual ggen-core API call
    let result = json!({
        "template": template,
        "target": target,
        "variables": vars,
        "changes": [
            {
                "file": "README.md",
                "status": "modified",
                "additions": 5,
                "deletions": 2
            },
            {
                "file": "src/new_feature.rs",
                "status": "new",
                "additions": 42,
                "deletions": 0
            }
        ],
        "summary": {
            "files_changed": 2,
            "total_additions": 47,
            "total_deletions": 2
        }
    });

    Ok(success_response(result))
}

#[cfg(test)]
mod tests {
    use super::*;
    use serde_json::json;

    #[tokio::test]
    async fn test_gen_basic() {
        let params = json!({
            "template": "rust-lib"
        });

        let result = gen(params).await;
        assert!(result.is_ok());
    }

    #[tokio::test]
    async fn test_gen_missing_template() {
        let params = json!({});

        let result = gen(params).await;
        assert!(result.is_err());
    }
}
