use serde_json::{json, Value};
use crate::error::{Result, get_string_param, get_optional_string_param, success_response};

/// Create new template
pub async fn create(params: Value) -> Result<Value> {
    let name = get_string_param(&params, "name")?;
    let content = get_string_param(&params, "content")?;
    let description = get_optional_string_param(&params, "description");
    let tags = params.get("tags")
        .and_then(|v| v.as_array())
        .map(|arr| {
            arr.iter()
                .filter_map(|v| v.as_str())
                .map(String::from)
                .collect::<Vec<_>>()
        })
        .unwrap_or_default();

    tracing::info!("Creating template: {}", name);

    // TODO: Replace with actual template creation logic
    let result = json!({
        "name": name,
        "path": format!("~/.ggen/templates/{}.tmpl", name),
        "description": description,
        "tags": tags,
        "size_bytes": content.len(),
        "status": "created"
    });

    Ok(success_response(result))
}

/// Validate template syntax
pub async fn validate(params: Value) -> Result<Value> {
    let template = get_string_param(&params, "template")?;

    tracing::info!("Validating template: {}", template);

    // TODO: Replace with actual validation logic
    let result = json!({
        "template": template,
        "valid": true,
        "syntax_errors": [],
        "warnings": [],
        "variables_detected": ["name", "version", "author"],
        "partials_detected": ["header", "footer"]
    });

    Ok(success_response(result))
}

#[cfg(test)]
mod tests {
    use super::*;
    use serde_json::json;

    #[tokio::test]
    async fn test_create_requires_name_and_content() {
        let params = json!({
            "name": "test-template"
        });
        let result = create(params).await;
        assert!(result.is_err());

        let params = json!({
            "content": "test content"
        });
        let result = create(params).await;
        assert!(result.is_err());
    }

    #[tokio::test]
    async fn test_validate_requires_template() {
        let params = json!({});
        let result = validate(params).await;
        assert!(result.is_err());
    }
}
