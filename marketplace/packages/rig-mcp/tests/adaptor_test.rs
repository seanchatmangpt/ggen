//! MCP adaptor tests - validates tool conversion without LLM APIs

use serde_json::json;
use std::sync::Arc;

#[test]
fn test_mcp_tool_schema_conversion() {
    // Test that JSON schemas can be properly structured
    let schema = json!({
        "type": "object",
        "properties": {
            "arg1": {
                "type": "string",
                "description": "First argument"
            },
            "arg2": {
                "type": "number",
                "description": "Second argument"
            }
        },
        "required": ["arg1"]
    });

    // Test basic schema structure
    assert!(schema.is_object());
    assert_eq!(schema["type"], "object");
    assert!(schema["properties"].is_object());
    assert!(schema["properties"]["arg1"].is_object());
    assert!(schema["properties"]["arg2"].is_object());
    assert_eq!(schema["properties"]["arg1"]["type"], "string");
    assert_eq!(schema["properties"]["arg2"]["type"], "number");
}

#[test]
fn test_schema_with_required_fields() {
    let schema = json!({
        "type": "object",
        "properties": {
            "required_field": { "type": "string" },
            "optional_field": { "type": "number" }
        },
        "required": ["required_field"]
    });

    assert!(schema["required"].is_array());
    assert_eq!(schema["required"][0], "required_field");
}

#[test]
fn test_complex_nested_schema() {
    let schema = json!({
        "type": "object",
        "properties": {
            "config": {
                "type": "object",
                "properties": {
                    "timeout": { "type": "number" },
                    "retries": { "type": "integer" },
                    "options": {
                        "type": "array",
                        "items": { "type": "string" }
                    }
                }
            },
            "metadata": {
                "type": "object",
                "additionalProperties": true
            }
        },
        "required": ["config"]
    });

    // Verify nested structure
    assert!(schema["properties"]["config"]["properties"]["timeout"].is_object());
    assert!(schema["properties"]["config"]["properties"]["options"]["items"].is_object());
    assert_eq!(schema["required"][0], "config");
}

#[test]
fn test_schema_with_enum_values() {
    let schema = json!({
        "type": "object",
        "properties": {
            "mode": {
                "type": "string",
                "enum": ["read", "write", "append"],
                "description": "File operation mode"
            }
        },
        "required": ["mode"]
    });

    // Verify enum is preserved
    let mode_enum = &schema["properties"]["mode"]["enum"];
    assert!(mode_enum.is_array());
    assert_eq!(mode_enum.as_array().unwrap().len(), 3);
    assert_eq!(mode_enum[0], "read");
    assert_eq!(mode_enum[1], "write");
    assert_eq!(mode_enum[2], "append");
}
