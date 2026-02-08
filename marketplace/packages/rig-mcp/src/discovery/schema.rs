//! Schema extraction and validation for MCP tools
//!
//! Provides functionality to extract, validate, and analyze tool schemas
//! from MCP server responses.

use crate::discovery::{DiscoveryError, Result};
use serde::{Deserialize, Serialize};
use serde_json::Value as JsonValue;
use std::collections::HashMap;

/// Schema validator for MCP tool definitions
#[derive(Debug, Clone)]
pub struct SchemaExtractor {
    /// Strict mode for validation
    pub strict: bool,
    /// Maximum depth for nested schema validation
    max_depth: usize,
}

impl Default for SchemaExtractor {
    fn default() -> Self {
        Self {
            strict: false,
            max_depth: 10,
        }
    }
}

impl SchemaExtractor {
    /// Create a new schema extractor
    pub fn new() -> Self {
        Self::default()
    }

    /// Enable strict validation mode
    pub fn with_strict(mut self, strict: bool) -> Self {
        self.strict = strict;
        self
    }

    /// Set maximum validation depth
    pub fn with_max_depth(mut self, depth: usize) -> Self {
        self.max_depth = depth;
        self
    }

    /// Extract tool schemas from a tools/list response
    pub fn extract_tools(&self, response: &JsonValue) -> Result<Vec<ToolSchema>> {
        let tools_array = response
            .get("tools")
            .and_then(|v| v.as_array())
            .ok_or_else(|| {
                DiscoveryError::InvalidResponse("Missing or invalid 'tools' array".to_string())
            })?;

        let mut schemas = Vec::with_capacity(tools_array.len());

        for (index, tool_value) in tools_array.iter().enumerate() {
            match self.extract_single_tool(tool_value) {
                Ok(schema) => schemas.push(schema),
                Err(e) => {
                    if self.strict {
                        return Err(DiscoveryError::SchemaValidation(format!(
                            "Tool at index {}: {}",
                            index, e
                        )));
                    }
                    // In non-strict mode, log and continue
                    tracing::warn!("Failed to extract tool at index {}: {}", index, e);
                }
            }
        }

        Ok(schemas)
    }

    /// Extract a single tool schema
    fn extract_single_tool(&self, value: &JsonValue) -> std::result::Result<ToolSchema, String> {
        let name = value
            .get("name")
            .and_then(|v| v.as_str())
            .ok_or("Missing 'name' field")?
            .to_string();

        let description = value
            .get("description")
            .and_then(|v| v.as_str())
            .ok_or("Missing 'description' field")?
            .to_string();

        let parameters = value
            .get("inputSchema")
            .or_else(|| value.get("input_schema"))
            .cloned();

        let parameters = if let Some(params) = parameters {
            Some(self.parse_parameter_schema(&params)?)
        } else {
            None
        };

        // Extract tags if present
        let tags = value
            .get("tags")
            .and_then(|v| v.as_array())
            .map(|arr| {
                arr.iter()
                    .filter_map(|v| v.as_str().map(String::from))
                    .collect()
            })
            .unwrap_or_default();

        Ok(ToolSchema {
            name,
            description,
            parameters,
            tags,
        })
    }

    /// Parse parameter schema from JSON Schema format
    fn parse_parameter_schema(
        &self, value: &JsonValue,
    ) -> std::result::Result<ParameterSchema, String> {
        let schema_type = value
            .get("type")
            .and_then(|v| v.as_str())
            .ok_or("Missing 'type' in parameter schema")?
            .to_string();

        let description = value
            .get("description")
            .and_then(|v| v.as_str())
            .map(String::from);

        let properties = value
            .get("properties")
            .and_then(|v| v.as_object())
            .map(|obj| {
                obj.iter()
                    .filter_map(|(k, v)| {
                        self.parse_parameter_schema(v)
                            .ok()
                            .map(|schema| (k.clone(), schema))
                    })
                    .collect()
            });

        let required = value.get("required").and_then(|v| v.as_array()).map(|arr| {
            arr.iter()
                .filter_map(|v| v.as_str().map(String::from))
                .collect()
        });

        let items = value
            .get("items")
            .and_then(|v| self.parse_parameter_schema(v).ok());

        // Extract pattern (regex)
        let pattern = value
            .get("pattern")
            .and_then(|v| v.as_str())
            .map(String::from);

        // Extract minLength/maxLength for strings or minItems/maxItems for arrays
        let min_length = value
            .get("minLength")
            .or_else(|| value.get("minItems"))
            .and_then(|v| v.as_u64());

        let max_length = value
            .get("maxLength")
            .or_else(|| value.get("maxItems"))
            .and_then(|v| v.as_u64());

        Ok(ParameterSchema {
            schema_type,
            description,
            properties,
            required,
            items: Box::new(items),
            default: value.get("default").cloned(),
            enum_values: value
                .get("enum")
                .and_then(|v| v.as_array())
                .map(|arr| arr.iter().cloned().collect()),
            pattern,
            min_length,
            max_length,
        })
    }

    /// Validate a tool schema
    pub fn validate(
        &self, schema: &ToolSchema,
    ) -> std::result::Result<(), ToolSchemaValidationError> {
        // Name validation
        if schema.name.is_empty() {
            return Err(ToolSchemaValidationError::EmptyName);
        }

        if !schema
            .name
            .chars()
            .all(|c| c.is_alphanumeric() || c == '_' || c == '-')
        {
            return Err(ToolSchemaValidationError::InvalidName(schema.name.clone()));
        }

        // Description validation
        if schema.description.is_empty() {
            return Err(ToolSchemaValidationError::EmptyDescription(
                schema.name.clone(),
            ));
        }

        // Parameter validation
        if let Some(params) = &schema.parameters {
            self.validate_parameters(params, &schema.name, 0)?;
        }

        Ok(())
    }

    /// Validate parameter schema recursively
    fn validate_parameters(
        &self, params: &ParameterSchema, tool_name: &str, depth: usize,
    ) -> std::result::Result<(), ToolSchemaValidationError> {
        if depth > self.max_depth {
            return Err(ToolSchemaValidationError::MaxDepthExceeded {
                tool_name: tool_name.to_string(),
                max_depth: self.max_depth,
            });
        }

        // Validate type
        match params.schema_type.as_str() {
            "object" | "array" | "string" | "number" | "integer" | "boolean" | "null" => {}
            t => {
                return Err(ToolSchemaValidationError::InvalidType {
                    tool_name: tool_name.to_string(),
                    type_name: t.to_string(),
                })
            }
        }

        // Validate properties if object type
        if params.schema_type == "object" {
            if let Some(props) = &params.properties {
                for (name, prop_schema) in props {
                    if name.is_empty() {
                        return Err(ToolSchemaValidationError::EmptyPropertyName {
                            tool_name: tool_name.to_string(),
                        });
                    }
                    self.validate_parameters(prop_schema, tool_name, depth + 1)?;
                }
            }
        }

        // Validate items if array type
        if params.schema_type == "array" {
            if let Some(items) = params.items.as_ref() {
                self.validate_parameters(items, tool_name, depth + 1)?;
            }
        }

        Ok(())
    }

    /// Calculate the complexity score of a tool schema
    pub fn calculate_complexity(&self, schema: &ToolSchema) -> u8 {
        let mut score: u8 = 0;

        // Base score for having parameters
        if schema.parameters.is_some() {
            score += 1;
        }

        if let Some(params) = &schema.parameters {
            score += self.calculate_parameter_complexity(params, 0);
        }

        // Tag count contributes slightly
        score += schema.tags.len() as u8;

        // Length of description
        score += (schema.description.len() as u8 / 50).min(3);

        score.min(10) // Cap at 10
    }

    /// Calculate complexity of a parameter schema
    fn calculate_parameter_complexity(&self, params: &ParameterSchema, depth: usize) -> u8 {
        if depth > self.max_depth {
            return 0;
        }

        let mut score: u8 = 1;

        match params.schema_type.as_str() {
            "object" => {
                if let Some(props) = &params.properties {
                    score += props.len() as u8;
                    for prop in props.values() {
                        score += self.calculate_parameter_complexity(prop, depth + 1);
                    }
                }
            }
            "array" => {
                if let Some(items) = params.items.as_ref() {
                    score += self.calculate_parameter_complexity(items, depth + 1);
                }
            }
            _ => {}
        }

        if params.required.is_some() {
            score += 1;
        }

        if params.enum_values.is_some() {
            score += 1;
        }

        score.min(5)
    }
}

/// Tool schema extracted from an MCP server
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct ToolSchema {
    /// Tool name (unique identifier)
    pub name: String,
    /// Human-readable description
    pub description: String,
    /// Input parameter schema (JSON Schema format)
    pub parameters: Option<ParameterSchema>,
    /// Tags for categorization
    #[serde(default)]
    pub tags: Vec<String>,
}

impl ToolSchema {
    /// Calculate the complexity score (0-10)
    pub fn complexity(&self) -> u8 {
        let extractor = SchemaExtractor::new();
        extractor.calculate_complexity(self)
    }

    /// Check if this tool requires authentication
    pub fn requires_auth(&self) -> bool {
        self.tags
            .iter()
            .any(|t| t == "auth" || t == "authenticated")
    }

    /// Get a JSON representation of this schema
    pub fn to_json(&self) -> Result<JsonValue> {
        serde_json::to_value(self).map_err(|e| DiscoveryError::JsonParse(e.to_string()))
    }

    /// Create a minimal tool schema
    pub fn new(name: impl Into<String>, description: impl Into<String>) -> Self {
        Self {
            name: name.into(),
            description: description.into(),
            parameters: None,
            tags: Vec::new(),
        }
    }

    /// Add parameters
    pub fn with_parameters(mut self, params: ParameterSchema) -> Self {
        self.parameters = Some(params);
        self
    }

    /// Add a tag
    pub fn with_tag(mut self, tag: impl Into<String>) -> Self {
        self.tags.push(tag.into());
        self
    }
}

/// Parameter schema following JSON Schema format
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct ParameterSchema {
    /// Schema type (object, array, string, number, etc.)
    #[serde(rename = "type")]
    pub schema_type: String,
    /// Human-readable description
    pub description: Option<String>,
    /// Properties for object type
    pub properties: Option<HashMap<String, ParameterSchema>>,
    /// Required property names
    pub required: Option<Vec<String>>,
    /// Item schema for array type
    #[serde(rename = "items")]
    pub items: Box<Option<ParameterSchema>>,
    /// Default value
    pub default: Option<JsonValue>,
    /// Enum values for constrained strings
    #[serde(rename = "enum")]
    pub enum_values: Option<Vec<JsonValue>>,
    /// Pattern for string validation (regex)
    #[serde(skip_serializing_if = "Option::is_none")]
    pub pattern: Option<String>,
    /// Minimum length for strings/min items for arrays
    #[serde(skip_serializing_if = "Option::is_none")]
    pub min_length: Option<u64>,
    /// Maximum length for strings/max items for arrays
    #[serde(skip_serializing_if = "Option::is_none")]
    pub max_length: Option<u64>,
}

impl ParameterSchema {
    /// Create a new parameter schema
    pub fn new(schema_type: impl Into<String>) -> Self {
        Self {
            schema_type: schema_type.into(),
            description: None,
            properties: None,
            required: None,
            items: Box::new(None),
            default: None,
            enum_values: None,
            pattern: None,
            min_length: None,
            max_length: None,
        }
    }

    /// Add description
    pub fn with_description(mut self, desc: impl Into<String>) -> Self {
        self.description = Some(desc.into());
        self
    }

    /// Add a property
    pub fn with_property(mut self, name: impl Into<String>, schema: ParameterSchema) -> Self {
        self.properties
            .get_or_insert_with(HashMap::new)
            .insert(name.into(), schema);
        self
    }

    /// Mark properties as required
    pub fn with_required(mut self, names: Vec<String>) -> Self {
        self.required = Some(names);
        self
    }

    /// Set items for array type
    pub fn with_items(mut self, items: ParameterSchema) -> Self {
        self.items = Box::new(Some(items));
        self
    }

    /// Check if a property is required
    pub fn is_required(&self, property_name: &str) -> bool {
        self.required
            .as_ref()
            .map(|req| req.contains(&property_name.to_string()))
            .unwrap_or(false)
    }
}

/// Schema validation errors
#[derive(Debug, thiserror::Error)]
pub enum ToolSchemaValidationError {
    /// Tool name is empty
    #[error("tool name cannot be empty")]
    EmptyName,

    /// Tool name contains invalid characters
    #[error("tool name '{0}' contains invalid characters (only alphanumeric, '_', '-' allowed)")]
    InvalidName(String),

    /// Description is empty
    #[error("tool '{0}' has empty description")]
    EmptyDescription(String),

    /// Maximum validation depth exceeded
    #[error("tool '{tool_name}' exceeds maximum validation depth of {max_depth}")]
    MaxDepthExceeded { tool_name: String, max_depth: usize },

    /// Invalid schema type
    #[error("tool '{tool_name}' has invalid type '{type_name}'")]
    InvalidType {
        tool_name: String,
        type_name: String,
    },

    /// Empty property name
    #[error("tool '{tool_name}' has empty property name")]
    EmptyPropertyName { tool_name: String },
}

/// Convenience function to extract schemas from a response
pub fn extract_schemas(response: &JsonValue) -> Result<Vec<ToolSchema>> {
    let extractor = SchemaExtractor::new();
    extractor.extract_tools(response)
}

/// Convenience function to validate a schema
pub fn validate_schema(schema: &ToolSchema) -> std::result::Result<(), ToolSchemaValidationError> {
    let extractor = SchemaExtractor::new();
    extractor.validate(schema)
}

#[cfg(test)]
mod tests {
    use super::*;
    use serde_json::json;

    #[test]
    fn test_schema_extractor_default() {
        let extractor = SchemaExtractor::new();
        assert!(!extractor.strict);
        assert_eq!(extractor.max_depth, 10);
    }

    #[test]
    fn test_schema_extractor_builder() {
        let extractor = SchemaExtractor::new().with_strict(true).with_max_depth(20);

        assert!(extractor.strict);
        assert_eq!(extractor.max_depth, 20);
    }

    #[test]
    fn test_tool_schema_builder() {
        let schema = ToolSchema::new("test_tool", "A test tool")
            .with_tag("test")
            .with_tag("demo");

        assert_eq!(schema.name, "test_tool");
        assert_eq!(schema.description, "A test tool");
        assert_eq!(schema.tags, vec!["test", "demo"]);
        assert!(schema.parameters.is_none());
    }

    #[test]
    fn test_parameter_schema_builder() {
        let schema = ParameterSchema::new("object")
            .with_description("An object parameter")
            .with_property("name", ParameterSchema::new("string"))
            .with_property("age", ParameterSchema::new("integer"))
            .with_required(vec!["name".to_string()]);

        assert_eq!(schema.schema_type, "object");
        assert_eq!(schema.description, Some("An object parameter".to_string()));
        assert!(schema.properties.is_some());
        assert!(schema.is_required("name"));
        assert!(!schema.is_required("age"));
    }

    #[test]
    fn test_tool_schema_complexity() {
        let simple_schema = ToolSchema::new("simple", "Simple tool");
        assert_eq!(simple_schema.complexity(), 0);

        let complex_schema = ToolSchema::new("complex", "A very complex tool with many features")
            .with_parameters(
                ParameterSchema::new("object")
                    .with_property("field1", ParameterSchema::new("string"))
                    .with_property("field2", ParameterSchema::new("integer"))
                    .with_property("field3", ParameterSchema::new("boolean")),
            )
            .with_tag("complex")
            .with_tag("advanced");

        // Complexity should be higher for the complex schema
        assert!(complex_schema.complexity() > simple_schema.complexity());
    }

    #[test]
    fn test_schema_validation() {
        let extractor = SchemaExtractor::new();

        // Valid schema
        let valid_schema = ToolSchema::new("valid_tool", "Valid tool description");
        assert!(extractor.validate(&valid_schema).is_ok());

        // Invalid name
        let invalid_name = ToolSchema::new("", "Description");
        assert!(matches!(
            extractor.validate(&invalid_name),
            Err(ToolSchemaValidationError::EmptyName)
        ));

        // Empty description
        let empty_desc = ToolSchema::new("tool", "");
        assert!(matches!(
            extractor.validate(&empty_desc),
            Err(ToolSchemaValidationError::EmptyDescription(_))
        ));
    }

    #[test]
    fn test_extract_from_response() {
        let response = json!({
            "tools": [
                {
                    "name": "test_tool",
                    "description": "A test tool",
                    "inputSchema": {
                        "type": "object",
                        "properties": {
                            "param1": {
                                "type": "string",
                                "description": "A string parameter"
                            }
                        },
                        "required": ["param1"]
                    },
                    "tags": ["test", "demo"]
                }
            ]
        });

        let extractor = SchemaExtractor::new();
        let schemas = extractor.extract_tools(&response).unwrap();

        assert_eq!(schemas.len(), 1);
        assert_eq!(schemas[0].name, "test_tool");
        assert_eq!(schemas[0].tags, vec!["test", "demo"]);
        assert!(schemas[0].parameters.is_some());
    }

    #[test]
    fn test_requires_auth() {
        let auth_tool = ToolSchema::new("secure_tool", "Needs auth").with_tag("authenticated");
        assert!(auth_tool.requires_auth());

        let public_tool = ToolSchema::new("public_tool", "No auth needed").with_tag("public");
        assert!(!public_tool.requires_auth());
    }
}
