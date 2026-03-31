//! Integration tests for the validate_templates MCP tool.

use serde_json::json;

#[tokio::test]
async fn test_validate_templates_valid() {
    // Test with a valid template
    let valid_template = r#"
Hello {{ name }}!

You have {{ count }} items.

{% for item in items %}
- {{ item }}
{% endfor %}
"#;

    // This would normally go through the MCP server
    // For now, we'll test the underlying function directly
    let result = ggen_core::template::validate_template(valid_template);
    assert!(result.is_ok());
    let validation = result.unwrap();
    assert!(validation.is_valid);
    assert!(validation.issues.is_empty());
}

#[tokio::test]
async fn test_validate_templates_invalid_syntax() {
    // Test with an invalid template (unclosed braces)
    let invalid_template = r#"
Hello {{ name }!

Unclosed brace: {{ unclosed

{% for item in items %}
- {{ item }}
{% endfor %}
"#;

    let result = ggen_core::template::validate_template(invalid_template);
    assert!(result.is_ok());
    let validation = result.unwrap();
    assert!(!validation.is_valid);
    assert!(!validation.issues.is_empty());
}

#[tokio::test]
async fn test_extract_template_variables() {
    let template = r#"
Hello {{ name }}!
Count: {{ count }}
Items: {{ items | length }}
"#;

    let vars = ggen_core::template::extract_template_variables(template);
    assert!(vars.contains("name"));
    assert!(vars.contains("count"));
    assert!(vars.contains("items")); // Without filter
}

#[tokio::test]
async fn test_extract_sparql_variables() {
    // Create a temporary SPARQL results file
    let temp_dir = tempfile::tempdir().unwrap();
    let results_path = temp_dir.path().join("results.json");

    let results_json = json!({
        "head": {
            "vars": ["name", "count", "items"]
        },
        "results": {
            "bindings": [
                {
                    "name": {"type": "literal", "value": "Alice"},
                    "count": {"type": "literal", "value": "5"},
                    "items": {"type": "literal", "value": "item1"}
                }
            ]
        }
    });

    std::fs::write(
        &results_path,
        serde_json::to_string_pretty(&results_json).unwrap(),
    )
    .unwrap();

    let vars =
        ggen_core::template::extract_variables_from_sparql_results(results_path.to_str().unwrap())
            .unwrap();

    assert!(vars.contains("name"));
    assert!(vars.contains("count"));
    assert!(vars.contains("items"));
}
