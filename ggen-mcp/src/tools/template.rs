use serde_json::{json, Value};
use std::fs;
use dirs;
use crate::error::{Result, GgenMcpError, get_string_param, get_optional_string_param, success_response};

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

    // Create templates directory
    let templates_dir = dirs::home_dir()
        .ok_or_else(|| GgenMcpError::ExecutionFailed("Could not find home directory".to_string()))?
        .join(".ggen")
        .join("templates");

    fs::create_dir_all(&templates_dir)
        .map_err(|e| GgenMcpError::ExecutionFailed(format!("Failed to create templates directory: {}", e)))?;

    // Build frontmatter + body
    let mut frontmatter = serde_yaml::Mapping::new();
    if let Some(desc) = &description {
        frontmatter.insert(
            serde_yaml::Value::String("description".to_string()),
            serde_yaml::Value::String(desc.clone()),
        );
    }
    if !tags.is_empty() {
        frontmatter.insert(
            serde_yaml::Value::String("tags".to_string()),
            serde_yaml::Value::Sequence(tags.iter().map(|s| serde_yaml::Value::String(s.clone())).collect()),
        );
    }

    let yaml_str = serde_yaml::to_string(&frontmatter)
        .map_err(|e| GgenMcpError::SerializationError(format!("Failed to serialize frontmatter: {}", e)))?;

    let full_template = format!("---\n{}---\n{}", yaml_str, content);

    // Write template file
    let template_path = templates_dir.join(format!("{}.tmpl", name));
    fs::write(&template_path, &full_template)
        .map_err(|e| GgenMcpError::ExecutionFailed(format!("Failed to write template: {}", e)))?;

    let result = json!({
        "name": name,
        "path": template_path.display().to_string(),
        "description": description,
        "tags": tags,
        "size_bytes": full_template.len(),
        "status": "created"
    });

    Ok(success_response(result))
}

/// Validate template syntax
pub async fn validate(params: Value) -> Result<Value> {
    let template = get_string_param(&params, "template")?;

    tracing::info!("Validating template: {}", template);

    // Locate template file
    let templates_dir = dirs::home_dir()
        .ok_or_else(|| GgenMcpError::ExecutionFailed("Could not find home directory".to_string()))?
        .join(".ggen")
        .join("templates");

    let template_path = templates_dir.join(format!("{}.tmpl", template));

    if !template_path.exists() {
        return Err(GgenMcpError::InvalidParameter(format!("Template not found: {}", template)));
    }

    // Read template
    let content = fs::read_to_string(&template_path)
        .map_err(|e| GgenMcpError::ExecutionFailed(format!("Failed to read template: {}", e)))?;

    // Parse with Tera
    let mut tera = tera::Tera::default();
    tera.autoescape_on(vec![]);

    let mut syntax_errors = vec![];
    let mut warnings = vec![];
    let mut variables_detected = vec![];

    // Try to parse template
    match tera.add_raw_template(&template, &content) {
        Ok(_) => {},
        Err(e) => {
            syntax_errors.push(e.to_string());
        }
    }

    // Parse frontmatter to find variables
    if content.starts_with("---") {
        use gray_matter::{Matter, engine::YAML};
        let matter = Matter::<YAML>::new();
        if let Ok(parsed) = matter.parse(&content) {
            if let Some(data) = parsed.data {
                if let Some(vars) = data.get("vars").and_then(|v| v.as_mapping()) {
                    variables_detected = vars.keys()
                        .filter_map(|k| k.as_str().map(String::from))
                        .collect();
                }
            }
        }
    }

    let result = json!({
        "template": template,
        "valid": syntax_errors.is_empty(),
        "syntax_errors": syntax_errors,
        "warnings": warnings,
        "variables_detected": variables_detected,
        "partials_detected": []
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
