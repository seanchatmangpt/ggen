//! Schema Converter - MCP Tool Schema to RegisteredTool and ggen_ai::Tool
//!
//! Converts MCP tool schemas (ToolSchema) to RegisteredTool format,
//! handling JSON Schema parsing, tag mapping, and SLO configuration.
//!
//! With the `ggen-ai` feature enabled, also provides conversion to ggen_ai::Tool.

use crate::discovery::{ParameterSchema, ToolSchema};
use crate::registration::{
    AuthScope, RegisteredTool, ToolInputField, ToolSignature, ToolSlo, ToolTag,
};
use std::collections::HashMap;
use thiserror::Error;

#[cfg(feature = "ggen-ai")]
pub use ggen_ai;

/// Errors that can occur during schema conversion
#[derive(Debug, Error)]
pub enum ConversionError {
    /// Invalid parameter schema
    #[error("invalid parameter schema: {0}")]
    InvalidParameter(String),

    /// Missing required field
    #[error("missing required field: {0}")]
    MissingField(String),

    /// Invalid JSON Schema
    #[error("invalid JSON Schema: {0}")]
    InvalidJsonSchema(String),

    /// Tag parsing failed
    #[error("tag parsing failed: {0}")]
    TagParse(String),
}

/// Configuration for schema conversion
#[derive(Debug, Clone)]
pub struct ConversionConfig {
    /// Default timeout for tools (ms)
    pub default_timeout_ms: u64,
    /// Default max retries
    pub default_max_retries: u32,
    /// Default cacheable flag
    pub default_cacheable: bool,
    /// Default auth scope
    pub default_auth_scope: AuthScope,
    /// Tool ID prefix
    pub id_prefix: Option<String>,
}

impl Default for ConversionConfig {
    fn default() -> Self {
        Self {
            default_timeout_ms: 30_000,
            default_max_retries: 3,
            default_cacheable: true,
            default_auth_scope: AuthScope::Public,
            id_prefix: Some("mcp".to_string()),
        }
    }
}

/// Schema Converter
///
/// Converts MCP ToolSchema to RegisteredTool format.
pub struct SchemaConverter {
    /// Conversion configuration
    config: ConversionConfig,
}

impl SchemaConverter {
    /// Create a new converter
    pub fn new(config: ConversionConfig) -> Self {
        Self { config }
    }

    /// Create with default configuration
    pub fn default_converter() -> Self {
        Self::new(ConversionConfig::default())
    }

    /// Convert a ToolSchema to a RegisteredTool
    ///
    /// # Arguments
    /// * `schema` - The MCP tool schema
    ///
    /// # Returns
    /// A converted RegisteredTool
    pub fn convert(&self, schema: ToolSchema) -> Result<RegisteredTool, ConversionError> {
        // Build signature from input schema
        let signature = self.build_signature(&schema)?;

        // Parse tags
        let tags = self.parse_tags(&schema);

        // Build SLO configuration
        let slo = self.build_slo(&schema);

        // Determine auth scope from tags
        let auth_scope = self.determine_auth_scope(&schema, &tags);

        // Create metadata
        let mut metadata = HashMap::new();
        metadata.insert("mcp_tool".to_string(), "true".to_string());
        if let Some(params) = &schema.parameters {
            if let Some(complexity) = self.estimate_complexity(params) {
                metadata.insert("complexity".to_string(), complexity.to_string());
            }
        }

        // Create the tool
        let tool = RegisteredTool::new(
            schema.name.clone(),
            schema.name.clone(), // Use name as display name
            "1.0.0",             // Default version
            schema.description.clone(),
            signature,
        )
        .with_tags(tags)
        .with_slo(slo)
        .with_auth_scope(auth_scope);

        // Add metadata to the tool
        let tool_with_metadata = metadata
            .into_iter()
            .fold(tool, |t, (k, v)| t.with_metadata(k, v));

        Ok(tool_with_metadata)
    }

    /// Convert multiple schemas
    pub fn convert_all(
        &self, schemas: Vec<ToolSchema>,
    ) -> Vec<Result<RegisteredTool, ConversionError>> {
        schemas.into_iter().map(|s| self.convert(s)).collect()
    }

    /// Build a ToolSignature from the tool schema
    fn build_signature(&self, schema: &ToolSchema) -> Result<ToolSignature, ConversionError> {
        let mut sig = ToolSignature::new(&schema.name, &schema.description);

        // Add input fields from parameter schema
        if let Some(ref params) = schema.parameters {
            if params.schema_type == "object" {
                if let Some(ref properties) = params.properties {
                    for (param_name, param_schema) in properties {
                        let input_field =
                            self.parameter_to_input_field(param_name, param_schema, params)?;
                        sig = sig.with_input(input_field);
                    }
                }
            } else {
                // Single parameter at root level
                let input_field = self.parameter_to_input_field("input", params, params)?;
                sig = sig.with_input(input_field);
            }
        } else {
            // No parameters - add a dummy input for the signature
            sig = sig.with_input(ToolInputField::new("_input", "Empty input", "String"));
        }

        // Add a default output field
        // MCP tools don't define outputs, so we use a generic result
        sig = sig.with_output("String");

        Ok(sig)
    }

    /// Convert a parameter schema to a ToolInputField
    fn parameter_to_input_field(
        &self, name: &str, param: &ParameterSchema, parent: &ParameterSchema,
    ) -> Result<ToolInputField, ConversionError> {
        // Determine Rust type from JSON Schema type
        let rust_type = self.json_type_to_rust(&param.schema_type, param);

        // Get description
        let description = param
            .description
            .clone()
            .unwrap_or_else(|| format!("Parameter: {}", name));

        // Check if required
        let is_required = parent
            .required
            .as_ref()
            .map(|req| req.contains(&name.to_string()))
            .unwrap_or(false);

        // Create the field
        let mut field = ToolInputField::new(name, description, rust_type);

        // Apply constraints based on schema
        if let Some(min_length) = self.extract_min_length(param) {
            field = field.with_min_length(min_length);
        }

        if let Some(max_length) = self.extract_max_length(param) {
            field = field.with_max_length(max_length);
        }

        if let Some(ref pattern) = param.pattern {
            field = field.with_pattern(pattern);
        }

        if let Some(ref enum_values) = param.enum_values {
            let string_values: Vec<String> = enum_values
                .iter()
                .filter_map(|v| v.as_str().map(String::from))
                .collect();
            if !string_values.is_empty() {
                field = field.with_enum_values(string_values);
            }
        }

        // Set required flag
        if is_required {
            field = field.with_required(true);
        }

        Ok(field)
    }

    /// Map JSON Schema type to Rust type annotation
    fn json_type_to_rust(&self, json_type: &str, param: &ParameterSchema) -> String {
        match json_type {
            "string" => "String".to_string(),
            "integer" => "i64".to_string(),
            "number" => "f64".to_string(),
            "boolean" => "bool".to_string(),
            "array" => {
                if let Some(ref items) = *param.items {
                    let inner_type = self.json_type_to_rust(&items.schema_type, items);
                    format!("Vec<{}>", inner_type)
                } else {
                    "Vec<serde_json::Value>".to_string()
                }
            }
            "object" => {
                if let Some(ref properties) = param.properties {
                    if properties.is_empty() {
                        "HashMap<String, serde_json::Value>".to_string()
                    } else {
                        "serde_json::Value".to_string()
                    }
                } else {
                    "HashMap<String, serde_json::Value>".to_string()
                }
            }
            _ => "String".to_string(), // Default to String for unknown types
        }
    }

    /// Extract minimum length constraint
    fn extract_min_length(&self, param: &ParameterSchema) -> Option<usize> {
        param.min_length.map(|l| l as usize)
    }

    /// Extract maximum length constraint
    fn extract_max_length(&self, param: &ParameterSchema) -> Option<usize> {
        param.max_length.map(|l| l as usize)
    }

    /// Parse tags from schema
    fn parse_tags(&self, schema: &ToolSchema) -> Vec<ToolTag> {
        let mut tags = Vec::new();

        // Add tags from schema
        for tag_str in &schema.tags {
            match self.parse_single_tag(tag_str) {
                Ok(tag) => tags.push(tag),
                Err(_) => {
                    // Unknown tag - store as Custom
                    tags.push(ToolTag::Custom(tag_str.clone()));
                }
            }
        }

        // Always add MCP tag for identification
        tags.push(ToolTag::Custom("mcp".to_string()));

        tags
    }

    /// Parse a single tag string to ToolTag
    fn parse_single_tag(&self, tag_str: &str) -> Result<ToolTag, ConversionError> {
        match tag_str.to_lowercase().as_str() {
            "code_generation" | "code-generation" | "codegen" => Ok(ToolTag::CodeGeneration),
            "validation" | "validate" => Ok(ToolTag::Validation),
            "query" | "query_generation" | "query-generation" => Ok(ToolTag::QueryGeneration),
            "ontology" | "rdf" => Ok(ToolTag::Ontology),
            "template" | "templating" => Ok(ToolTag::Template),
            "cache" | "caching" => Ok(ToolTag::Caching),
            "summarize" | "summarization" => Ok(ToolTag::Summarization),
            "analyze" | "analysis" => Ok(ToolTag::Analysis),
            "generate" | "generation" => Ok(ToolTag::Generation),
            "translate" | "translation" => Ok(ToolTag::Translation),
            "financial" | "finance" => Ok(ToolTag::Financial),
            "banking" => Ok(ToolTag::Banking),
            "insurance" => Ok(ToolTag::Insurance),
            _ => Err(ConversionError::TagParse(format!(
                "Unknown tag: {}",
                tag_str
            ))),
        }
    }

    /// Build SLO configuration from schema
    fn build_slo(&self, schema: &ToolSchema) -> ToolSlo {
        // Check for SLO hints in tags
        let is_cacheable = !schema
            .tags
            .iter()
            .any(|t| t.to_lowercase().contains("nocache") || t.to_lowercase().contains("no_cache"));

        // Check for timeout hints in metadata
        let timeout_ms = self.config.default_timeout_ms;

        // Check for retry hints
        let max_retries = if schema
            .tags
            .iter()
            .any(|t| t.to_lowercase().contains("idempotent"))
        {
            self.config.default_max_retries
        } else {
            0 // Don't retry non-idempotent operations
        };

        ToolSlo {
            timeout_ms,
            max_retries,
            cacheable: is_cacheable && self.config.default_cacheable,
        }
    }

    /// Determine auth scope from schema
    fn determine_auth_scope(&self, schema: &ToolSchema, tags: &[ToolTag]) -> AuthScope {
        // Check tags for auth hints
        for tag in tags {
            if matches!(tag, ToolTag::Custom(s) if s == "authenticated" || s == "auth") {
                return AuthScope::Authenticated;
            }
            if matches!(tag, ToolTag::Custom(s) if s == "admin" || s == "admin_only") {
                return AuthScope::Admin;
            }
        }

        // Check schema tags
        for tag_str in &schema.tags {
            let lower = tag_str.to_lowercase();
            if lower.contains("auth") || lower.contains("login") || lower.contains("token") {
                return AuthScope::Authenticated;
            }
            if lower.contains("admin") {
                return AuthScope::Admin;
            }
        }

        self.config.default_auth_scope.clone()
    }

    /// Estimate tool complexity based on parameters
    fn estimate_complexity(&self, params: &ParameterSchema) -> Option<u8> {
        let mut score = 0u8;

        // Base score for having parameters
        score += 1;

        // Score based on type complexity
        match params.schema_type.as_str() {
            "object" => {
                if let Some(ref properties) = params.properties {
                    score += properties.len() as u8;
                }
            }
            "array" => {
                score += 2;
                if let Some(ref items) = *params.items {
                    score += self.estimate_complexity(items).unwrap_or(0);
                }
            }
            _ => {}
        }

        // Constraints add complexity
        if params.required.is_some() {
            score += 1;
        }
        if params.enum_values.is_some() {
            score += 1;
        }
        if params.pattern.is_some() {
            score += 1;
        }

        Some(score.min(10))
    }

    /// Convert a ToolSchema directly to ggen_ai::Tool for core registry integration
    ///
    /// This method is only available with the `ggen-ai` feature enabled.
    /// It converts the MCP schema to a ggen_ai::Tool that can be registered
    /// in the global ggen-ai tool registry.
    ///
    /// # Arguments
    /// * `schema` - The MCP tool schema
    /// * `tool_id` - The unique tool ID to use
    ///
    /// # Returns
    /// A ggen_ai::Tool ready for registration
    #[cfg(feature = "ggen-ai")]
    pub fn convert_to_core_tool(
        &self, schema: &ToolSchema, tool_id: &str,
    ) -> Result<ggen_ai::Tool, ConversionError> {
        use ggen_ai::{
            dspy::{
                field::{InputField, OutputField},
                Signature,
            },
            Tool,
        };

        // Build signature
        let mut sig = Signature::new(&schema.name, &schema.description);

        // Add input fields from parameter schema
        if let Some(ref params) = schema.parameters {
            if params.schema_type == "object" {
                if let Some(ref properties) = params.properties {
                    for (param_name, param_schema) in properties {
                        let rust_type =
                            self.json_type_to_rust(&param_schema.schema_type, param_schema);
                        let description = param_schema
                            .description
                            .clone()
                            .unwrap_or_else(|| format!("Parameter: {}", param_name));
                        let input_field = InputField::new(param_name, &description, &rust_type);
                        sig = sig.with_input(input_field);
                    }
                }
            } else {
                // Single parameter at root level
                let rust_type = self.json_type_to_rust(&params.schema_type, params);
                let description = params
                    .description
                    .clone()
                    .unwrap_or_else(|| "Input".to_string());
                let input_field = InputField::new("input", &description, &rust_type);
                sig = sig.with_input(input_field);
            }
        } else {
            // No parameters - add a dummy input
            sig = sig.with_input(InputField::new("_input", "Empty input", "String"));
        }

        // Add output field
        sig = sig.with_output(OutputField::new(
            "result",
            "Tool execution result",
            "String",
        ));

        // Parse tags
        let tags = self.parse_tags_for_core(schema);

        // Build SLO
        let slo = ggen_ai::ToolSlo {
            timeout_ms: self.config.default_timeout_ms,
            max_retries: self.config.default_max_retries,
            cacheable: self.config.default_cacheable,
        };

        // Determine auth scope
        let auth_scope = self.determine_auth_scope_for_core(schema, &tags);

        // Create the tool
        let tool = Tool::new(tool_id, &schema.name, "1.0.0", &schema.description, sig)
            .with_tags(tags)
            .with_slo(slo)
            .with_auth_scope(auth_scope);

        Ok(tool)
    }

    /// Parse tags for ggen_ai::Tool format
    #[cfg(feature = "ggen-ai")]
    fn parse_tags_for_core(&self, schema: &ToolSchema) -> Vec<ggen_ai::ToolTag> {
        let mut tags = Vec::new();

        for tag_str in &schema.tags {
            match self.parse_single_tag_for_core(tag_str) {
                Ok(tag) => tags.push(tag),
                Err(_) => {
                    // Unknown tag - store as Custom
                    tags.push(ggen_ai::ToolTag::Custom(tag_str.clone()));
                }
            }
        }

        // Always add MCP tag for identification
        tags.push(ggen_ai::ToolTag::Custom("mcp".to_string()));

        tags
    }

    /// Parse a single tag string to ggen_ai::ToolTag
    #[cfg(feature = "ggen-ai")]
    fn parse_single_tag_for_core(
        &self, tag_str: &str,
    ) -> Result<ggen_ai::ToolTag, ConversionError> {
        match tag_str.to_lowercase().as_str() {
            "code_generation" | "code-generation" | "codegen" => {
                Ok(ggen_ai::ToolTag::CodeGeneration)
            }
            "validation" | "validate" => Ok(ggen_ai::ToolTag::Validation),
            "query" | "query_generation" | "query-generation" => {
                Ok(ggen_ai::ToolTag::QueryGeneration)
            }
            "ontology" | "rdf" => Ok(ggen_ai::ToolTag::Ontology),
            "template" | "templating" => Ok(ggen_ai::ToolTag::Template),
            "cache" | "caching" => Ok(ggen_ai::ToolTag::Caching),
            "summarize" | "summarization" => Ok(ggen_ai::ToolTag::Summarization),
            "analyze" | "analysis" => Ok(ggen_ai::ToolTag::Analysis),
            "generate" | "generation" => Ok(ggen_ai::ToolTag::Generation),
            "translate" | "translation" => Ok(ggen_ai::ToolTag::Translation),
            "financial" | "finance" => Ok(ggen_ai::ToolTag::Financial),
            "banking" => Ok(ggen_ai::ToolTag::Banking),
            "insurance" => Ok(ggen_ai::ToolTag::Insurance),
            _ => Err(ConversionError::TagParse(format!(
                "Unknown tag: {}",
                tag_str
            ))),
        }
    }

    /// Determine auth scope for ggen_ai::Tool
    #[cfg(feature = "ggen-ai")]
    fn determine_auth_scope_for_core(
        &self, schema: &ToolSchema, tags: &[ggen_ai::ToolTag],
    ) -> ggen_ai::AuthScope {
        // Check tags for auth hints
        for tag in tags {
            if matches!(tag, ggen_ai::ToolTag::Custom(s) if s == "authenticated" || s == "auth") {
                return ggen_ai::AuthScope::Authenticated;
            }
            if matches!(tag, ggen_ai::ToolTag::Custom(s) if s == "admin" || s == "admin_only") {
                return ggen_ai::AuthScope::Admin;
            }
        }

        // Check schema tags
        for tag_str in &schema.tags {
            let lower = tag_str.to_lowercase();
            if lower.contains("auth") || lower.contains("login") || lower.contains("token") {
                return ggen_ai::AuthScope::Authenticated;
            }
            if lower.contains("admin") {
                return ggen_ai::AuthScope::Admin;
            }
        }

        ggen_ai::AuthScope::Public
    }
}

impl Default for SchemaConverter {
    fn default() -> Self {
        Self::default_converter()
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_conversion_config_default() {
        let config = ConversionConfig::default();
        assert_eq!(config.default_timeout_ms, 30_000);
        assert_eq!(config.default_max_retries, 3);
        assert!(config.default_cacheable);
        assert_eq!(config.id_prefix, Some("mcp".to_string()));
    }

    #[test]
    fn test_converter_creation() {
        let converter = SchemaConverter::default_converter();
        assert_eq!(converter.config.id_prefix, Some("mcp".to_string()));
    }

    #[test]
    fn test_json_type_to_rust_string() {
        let converter = SchemaConverter::default_converter();
        let param = ParameterSchema::new("string");

        assert_eq!(converter.json_type_to_rust("string", &param), "String");
    }

    #[test]
    fn test_json_type_to_rust_integer() {
        let converter = SchemaConverter::default_converter();
        let param = ParameterSchema::new("integer");

        assert_eq!(converter.json_type_to_rust("integer", &param), "i64");
    }

    #[test]
    fn test_json_type_to_rust_number() {
        let converter = SchemaConverter::default_converter();
        let param = ParameterSchema::new("number");

        assert_eq!(converter.json_type_to_rust("number", &param), "f64");
    }

    #[test]
    fn test_json_type_to_rust_boolean() {
        let converter = SchemaConverter::default_converter();
        let param = ParameterSchema::new("boolean");

        assert_eq!(converter.json_type_to_rust("boolean", &param), "bool");
    }

    #[test]
    fn test_json_type_to_rust_array() {
        let converter = SchemaConverter::default_converter();
        let param = ParameterSchema::new("array").with_items(ParameterSchema::new("string"));

        assert_eq!(converter.json_type_to_rust("array", &param), "Vec<String>");
    }

    #[test]
    fn test_json_type_to_rust_unknown() {
        let converter = SchemaConverter::default_converter();
        let param = ParameterSchema::new("unknown");

        assert_eq!(converter.json_type_to_rust("unknown", &param), "String");
    }

    #[test]
    fn test_parse_single_tag_known() {
        let converter = SchemaConverter::default_converter();

        assert!(matches!(
            converter.parse_single_tag("code_generation"),
            Ok(ToolTag::CodeGeneration)
        ));
        assert!(matches!(
            converter.parse_single_tag("validation"),
            Ok(ToolTag::Validation)
        ));
        assert!(matches!(
            converter.parse_single_tag("financial"),
            Ok(ToolTag::Financial)
        ));
    }

    #[test]
    fn test_parse_tag_case_insensitive() {
        let converter = SchemaConverter::default_converter();

        assert!(matches!(
            converter.parse_single_tag("CODEGEN"),
            Ok(ToolTag::CodeGeneration)
        ));
        assert!(matches!(
            converter.parse_single_tag("Query"),
            Ok(ToolTag::QueryGeneration)
        ));
    }

    #[test]
    fn test_parse_single_tag_unknown() {
        let converter = SchemaConverter::default_converter();
        assert!(converter.parse_single_tag("unknown_tag").is_err());
    }

    #[test]
    fn test_build_slo_default() {
        let converter = SchemaConverter::default_converter();
        let schema = ToolSchema::new("test", "Test tool");

        let slo = converter.build_slo(&schema);
        assert_eq!(slo.timeout_ms, 30_000);
        assert!(slo.cacheable);
        assert_eq!(slo.max_retries, 0); // No retries by default for non-idempotent
    }

    #[test]
    fn test_build_slo_idempotent() {
        let converter = SchemaConverter::default_converter();
        let schema = ToolSchema::new("test", "Test tool").with_tag("idempotent");

        let slo = converter.build_slo(&schema);
        assert_eq!(slo.max_retries, 3);
    }

    #[test]
    fn test_build_slo_no_cache() {
        let converter = SchemaConverter::default_converter();
        let schema = ToolSchema::new("test", "Test tool").with_tag("nocache");

        let slo = converter.build_slo(&schema);
        assert!(!slo.cacheable);
    }

    #[test]
    fn test_determine_auth_scope_public() {
        let converter = SchemaConverter::default_converter();
        let schema = ToolSchema::new("test", "Test tool");
        let tags = vec![];

        let scope = converter.determine_auth_scope(&schema, &tags);
        assert_eq!(scope, AuthScope::Public);
    }

    #[test]
    fn test_determine_auth_scope_authenticated() {
        let converter = SchemaConverter::default_converter();
        let schema = ToolSchema::new("test", "Test tool").with_tag("authenticated");
        let tags = vec![];

        let scope = converter.determine_auth_scope(&schema, &tags);
        assert_eq!(scope, AuthScope::Authenticated);
    }

    #[test]
    fn test_determine_auth_scope_admin() {
        let converter = SchemaConverter::default_converter();
        let schema = ToolSchema::new("test", "Test tool").with_tag("admin");
        let tags = vec![];

        let scope = converter.determine_auth_scope(&schema, &tags);
        assert_eq!(scope, AuthScope::Admin);
    }

    #[test]
    fn test_estimate_complexity_simple() {
        let converter = SchemaConverter::default_converter();
        let param = ParameterSchema::new("string");

        let complexity = converter.estimate_complexity(&param);
        assert_eq!(complexity, Some(2)); // 1 base + 1 for parameter
    }

    #[test]
    fn test_estimate_complexity_object() {
        let converter = SchemaConverter::default_converter();
        let param = ParameterSchema::new("object")
            .with_property("field1", ParameterSchema::new("string"))
            .with_property("field2", ParameterSchema::new("integer"))
            .with_required(vec!["field1".to_string()]);

        let complexity = converter.estimate_complexity(&param);
        // 1 base + 2 properties + 1 for required
        assert!(complexity.unwrap() >= 4);
    }

    #[test]
    fn test_convert_simple_tool() {
        let converter = SchemaConverter::default_converter();
        let schema = ToolSchema::new("simple_tool", "A simple tool");

        let result = converter.convert(schema);
        assert!(result.is_ok());

        let tool = result.unwrap();
        assert_eq!(tool.id, "simple_tool");
        assert_eq!(tool.name, "simple_tool");
    }

    #[test]
    fn test_convert_tool_with_parameters() {
        let converter = SchemaConverter::default_converter();
        let param = ParameterSchema::new("object")
            .with_description("Input parameters")
            .with_property("text", ParameterSchema::new("string"))
            .with_property("count", ParameterSchema::new("integer"))
            .with_required(vec!["text".to_string()]);

        let schema =
            ToolSchema::new("tool_with_params", "Tool with parameters").with_parameters(param);

        let result = converter.convert(schema);
        assert!(result.is_ok());

        let tool = result.unwrap();
        assert_eq!(tool.signature.inputs.len(), 2);
    }

    #[test]
    fn test_convert_tool_with_tags() {
        let converter = SchemaConverter::default_converter();
        let schema = ToolSchema::new("tagged_tool", "Tool with tags")
            .with_tag("code_generation")
            .with_tag("financial");

        let result = converter.convert(schema);
        assert!(result.is_ok());

        let tool = result.unwrap();
        assert!(tool.tags.contains(&ToolTag::CodeGeneration));
        assert!(tool.tags.contains(&ToolTag::Financial));
        // Should also have MCP tag
        assert!(tool
            .tags
            .iter()
            .any(|t| matches!(t, ToolTag::Custom(s) if s == "mcp")));
    }

    #[test]
    fn test_convert_all() {
        let converter = SchemaConverter::default_converter();
        let schemas = vec![
            ToolSchema::new("tool1", "First tool"),
            ToolSchema::new("tool2", "Second tool"),
        ];

        let results = converter.convert_all(schemas);
        assert_eq!(results.len(), 2);
        assert!(results[0].is_ok());
        assert!(results[1].is_ok());
    }
}
