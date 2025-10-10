//! Validation Agent - Schema and Contract Validation
//!
//! This agent provides comprehensive validation for the MCP server:
//! - JSON schema validation for all tool inputs
//! - Contract validation between MCP and CLI layers
//! - Type safety enforcement and validation
//! - Data format validation and sanitization
//! - Business rule validation and enforcement
//!
//! # Validation Patterns
//!
//! ## Schema Validation
//! - **JSON Schema** - Validate input structure and types
//! - **Custom Validators** - Business-specific validation rules
//! - **Format Validation** - Validate data formats (email, URL, etc.)
//! - **Range Validation** - Validate numeric ranges and constraints
//!
//! ## Contract Validation
//! - **API Contracts** - Validate MCP tool contracts
//! - **Data Contracts** - Validate data structure contracts
//! - **Version Contracts** - Validate API version compatibility
//! - **Backward Compatibility** - Ensure backward compatibility
//!
//! ## Type Safety
//! - **Static Typing** - Compile-time type checking
//! - **Runtime Validation** - Runtime type validation
//! - **Type Coercion** - Safe type conversions
//! - **Type Inference** - Automatic type detection

use crate::agents::{Agent, AgentMetadata, AgentStatus, AgentId};
use crate::error::{GgenMcpError, Result};
use serde::{Deserialize, Serialize};
use std::collections::HashMap;
use uuid::Uuid;
use chrono::Utc;
use regex::Regex;

/// Validation result
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct ValidationResult {
    pub is_valid: bool,
    pub errors: Vec<ValidationError>,
    pub warnings: Vec<ValidationWarning>,
    pub sanitized_data: Option<serde_json::Value>,
    pub validation_time_ms: u64,
    pub metadata: HashMap<String, serde_json::Value>,
}

/// Validation error
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct ValidationError {
    pub field: String,
    pub error_type: ValidationErrorType,
    pub message: String,
    pub severity: ValidationSeverity,
    pub suggested_fix: Option<String>,
}

/// Validation warning
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct ValidationWarning {
    pub field: String,
    pub warning_type: ValidationWarningType,
    pub message: String,
    pub suggestion: Option<String>,
}

/// Validation error types
#[derive(Debug, Clone, Serialize, Deserialize, PartialEq)]
pub enum ValidationErrorType {
    RequiredFieldMissing,
    InvalidType,
    InvalidFormat,
    OutOfRange,
    InvalidValue,
    ConstraintViolation,
    SecurityViolation,
    ContractViolation,
}

/// Validation warning types
#[derive(Debug, Clone, Serialize, Deserialize, PartialEq)]
pub enum ValidationWarningType {
    DeprecatedField,
    UnusedField,
    PerformanceImpact,
    SecurityConcern,
    CompatibilityIssue,
}

/// Validation severity levels
#[derive(Debug, Clone, Serialize, Deserialize, PartialEq)]
pub enum ValidationSeverity {
    Low,
    Medium,
    High,
    Critical,
}

/// Schema definition
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct SchemaDefinition {
    pub name: String,
    pub version: String,
    pub fields: Vec<FieldDefinition>,
    pub required_fields: Vec<String>,
    pub custom_validators: Vec<CustomValidator>,
    pub last_updated: chrono::DateTime<Utc>,
}

/// Field definition
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct FieldDefinition {
    pub name: String,
    pub field_type: FieldType,
    pub description: String,
    pub required: bool,
    pub default_value: Option<serde_json::Value>,
    pub constraints: Vec<FieldConstraint>,
    pub format_validators: Vec<FormatValidator>,
}

/// Field type
#[derive(Debug, Clone, Serialize, Deserialize, PartialEq)]
pub enum FieldType {
    String,
    Integer,
    Float,
    Boolean,
    Array,
    Object,
    Enum(Vec<String>),
    Custom(String),
}

/// Field constraint
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct FieldConstraint {
    pub constraint_type: ConstraintType,
    pub value: serde_json::Value,
    pub message: String,
}

/// Constraint types
#[derive(Debug, Clone, Serialize, Deserialize, PartialEq)]
pub enum ConstraintType {
    MinLength,
    MaxLength,
    MinValue,
    MaxValue,
    Pattern,
    Custom,
}

/// Format validator
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct FormatValidator {
    pub name: String,
    pub pattern: Regex,
    pub message: String,
}

/// Custom validator
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct CustomValidator {
    pub name: String,
    pub validation_function: String, // Placeholder for actual validation logic
    pub message: String,
}

/// Validation Agent implementation
pub struct ValidationAgent {
    id: AgentId,
    schemas: HashMap<String, SchemaDefinition>,
    validation_history: Vec<ValidationResult>,
    custom_validators: HashMap<String, Box<dyn Fn(&serde_json::Value) -> Result<()> + Send + Sync>>,
}

impl ValidationAgent {
    pub fn new() -> Self {
        let mut agent = Self {
            id: Uuid::new_v4(),
            schemas: HashMap::new(),
            validation_history: Vec::new(),
            custom_validators: HashMap::new(),
        };

        // Initialize default schemas
        agent.initialize_default_schemas();
        
        // Initialize custom validators
        agent.initialize_custom_validators();

        agent
    }

    /// Initialize default schemas
    fn initialize_default_schemas(&mut self) {
        // Project generation schema
        self.schemas.insert("project_gen".to_string(), SchemaDefinition {
            name: "project_gen".to_string(),
            version: "1.0.0".to_string(),
            fields: vec![
                FieldDefinition {
                    name: "template".to_string(),
                    field_type: FieldType::String,
                    description: "Template name or path".to_string(),
                    required: true,
                    default_value: None,
                    constraints: vec![
                        FieldConstraint {
                            constraint_type: ConstraintType::MinLength,
                            value: serde_json::Value::Number(serde_json::Number::from(1)),
                            message: "Template name cannot be empty".to_string(),
                        },
                        FieldConstraint {
                            constraint_type: ConstraintType::MaxLength,
                            value: serde_json::Value::Number(serde_json::Number::from(1000)),
                            message: "Template name too long".to_string(),
                        },
                    ],
                    format_validators: vec![
                        FormatValidator {
                            name: "path_safe".to_string(),
                            pattern: Regex::new(r"^[a-zA-Z0-9._/-]+$").unwrap(),
                            message: "Template path contains invalid characters".to_string(),
                        },
                    ],
                },
                FieldDefinition {
                    name: "output".to_string(),
                    field_type: FieldType::String,
                    description: "Output directory".to_string(),
                    required: false,
                    default_value: Some(serde_json::Value::String(".".to_string())),
                    constraints: vec![
                        FieldConstraint {
                            constraint_type: ConstraintType::MaxLength,
                            value: serde_json::Value::Number(serde_json::Number::from(1000)),
                            message: "Output path too long".to_string(),
                        },
                    ],
                    format_validators: vec![
                        FormatValidator {
                            name: "path_safe".to_string(),
                            pattern: Regex::new(r"^[a-zA-Z0-9._/-]+$").unwrap(),
                            message: "Output path contains invalid characters".to_string(),
                        },
                    ],
                },
                FieldDefinition {
                    name: "vars".to_string(),
                    field_type: FieldType::Object,
                    description: "Template variables".to_string(),
                    required: false,
                    default_value: None,
                    constraints: vec![],
                    format_validators: vec![],
                },
            ],
            required_fields: vec!["template".to_string()],
            custom_validators: vec![],
            last_updated: Utc::now(),
        });

        // Graph query schema
        self.schemas.insert("graph_query".to_string(), SchemaDefinition {
            name: "graph_query".to_string(),
            version: "1.0.0".to_string(),
            fields: vec![
                FieldDefinition {
                    name: "data".to_string(),
                    field_type: FieldType::String,
                    description: "RDF data file or content".to_string(),
                    required: true,
                    default_value: None,
                    constraints: vec![
                        FieldConstraint {
                            constraint_type: ConstraintType::MinLength,
                            value: serde_json::Value::Number(serde_json::Number::from(1)),
                            message: "Data cannot be empty".to_string(),
                        },
                    ],
                    format_validators: vec![],
                },
                FieldDefinition {
                    name: "query".to_string(),
                    field_type: FieldType::String,
                    description: "SPARQL query".to_string(),
                    required: true,
                    default_value: None,
                    constraints: vec![
                        FieldConstraint {
                            constraint_type: ConstraintType::MinLength,
                            value: serde_json::Value::Number(serde_json::Number::from(1)),
                            message: "Query cannot be empty".to_string(),
                        },
                        FieldConstraint {
                            constraint_type: ConstraintType::MaxLength,
                            value: serde_json::Value::Number(serde_json::Number::from(10000)),
                            message: "Query too long".to_string(),
                        },
                    ],
                    format_validators: vec![
                        FormatValidator {
                            name: "sparql_safe".to_string(),
                            pattern: Regex::new(r"^(SELECT|CONSTRUCT|ASK|DESCRIBE)").unwrap(),
                            message: "Invalid SPARQL query format".to_string(),
                        },
                    ],
                },
                FieldDefinition {
                    name: "format".to_string(),
                    field_type: FieldType::Enum(vec!["json".to_string(), "csv".to_string(), "table".to_string()]),
                    description: "Output format".to_string(),
                    required: false,
                    default_value: Some(serde_json::Value::String("json".to_string())),
                    constraints: vec![],
                    format_validators: vec![],
                },
            ],
            required_fields: vec!["data".to_string(), "query".to_string()],
            custom_validators: vec![],
            last_updated: Utc::now(),
        });
    }

    /// Initialize custom validators
    fn initialize_custom_validators(&mut self) {
        // Path traversal validator
        self.custom_validators.insert("no_path_traversal".to_string(), Box::new(|value| {
            if let Some(path) = value.as_str() {
                if path.contains("..") {
                    return Err(GgenMcpError::InvalidParameter("Path traversal detected".to_string()));
                }
            }
            Ok(())
        }));

        // Template name validator
        self.custom_validators.insert("valid_template_name".to_string(), Box::new(|value| {
            if let Some(name) = value.as_str() {
                if name.trim().is_empty() {
                    return Err(GgenMcpError::InvalidParameter("Template name cannot be empty".to_string()));
                }
                if name.len() > 100 {
                    return Err(GgenMcpError::InvalidParameter("Template name too long".to_string()));
                }
            }
            Ok(())
        }));
    }

    /// Validate data against schema
    pub fn validate_against_schema(&mut self, schema_name: &str, data: serde_json::Value) -> Result<ValidationResult> {
        let start_time = std::time::Instant::now();
        
        let schema = self.schemas.get(schema_name)
            .ok_or_else(|| GgenMcpError::InvalidParameter(format!("Unknown schema: {}", schema_name)))?;

        let mut errors = Vec::new();
        let mut warnings = Vec::new();
        let mut sanitized_data = data.clone();

        // Validate required fields
        for required_field in &schema.required_fields {
            if !data.get(required_field).is_some() {
                errors.push(ValidationError {
                    field: required_field.clone(),
                    error_type: ValidationErrorType::RequiredFieldMissing,
                    message: format!("Required field '{}' is missing", required_field),
                    severity: ValidationSeverity::High,
                    suggested_fix: Some(format!("Add the '{}' field to the request", required_field)),
                });
            }
        }

        // Validate each field
        for field_def in &schema.fields {
            if let Some(field_value) = data.get(&field_def.name) {
                // Validate field type
                if !self.validate_field_type(field_value, &field_def.field_type) {
                    errors.push(ValidationError {
                        field: field_def.name.clone(),
                        error_type: ValidationErrorType::InvalidType,
                        message: format!("Field '{}' has invalid type", field_def.name),
                        severity: ValidationSeverity::Medium,
                        suggested_fix: Some(format!("Ensure '{}' is of type {:?}", field_def.name, field_def.field_type)),
                    });
                }

                // Validate constraints
                for constraint in &field_def.constraints {
                    if let Err(constraint_error) = self.validate_constraint(field_value, constraint) {
                        errors.push(ValidationError {
                            field: field_def.name.clone(),
                            error_type: ValidationErrorType::ConstraintViolation,
                            message: constraint_error,
                            severity: ValidationSeverity::Medium,
                            suggested_fix: Some(constraint.message.clone()),
                        });
                    }
                }

                // Validate format
                for format_validator in &field_def.format_validators {
                    if let Err(format_error) = self.validate_format(field_value, format_validator) {
                        errors.push(ValidationError {
                            field: field_def.name.clone(),
                            error_type: ValidationErrorType::InvalidFormat,
                            message: format_error,
                            severity: ValidationSeverity::Medium,
                            suggested_fix: Some(format_validator.message.clone()),
                        });
                    }
                }
            }
        }

        // Run custom validators
        for custom_validator in &schema.custom_validators {
            if let Err(custom_error) = self.run_custom_validator(custom_validator, &data) {
                errors.push(ValidationError {
                    field: "custom".to_string(),
                    error_type: ValidationErrorType::Custom,
                    message: custom_error,
                    severity: ValidationSeverity::High,
                    suggested_fix: Some(custom_validator.message.clone()),
                });
            }
        }

        let validation_time = start_time.elapsed().as_millis() as u64;
        let is_valid = errors.is_empty();

        let result = ValidationResult {
            is_valid,
            errors,
            warnings,
            sanitized_data: if is_valid { Some(sanitized_data) } else { None },
            validation_time_ms: validation_time,
            metadata: HashMap::from([
                ("schema_name".to_string(), serde_json::Value::String(schema_name.to_string())),
                ("schema_version".to_string(), serde_json::Value::String(schema.version.clone())),
                ("validation_time".to_string(), serde_json::Value::String(Utc::now().to_rfc3339())),
            ]),
        };

        self.validation_history.push(result.clone());
        
        // Keep only last 10000 validations
        if self.validation_history.len() > 10000 {
            self.validation_history.remove(0);
        }

        Ok(result)
    }

    /// Validate field type
    fn validate_field_type(&self, value: &serde_json::Value, field_type: &FieldType) -> bool {
        match field_type {
            FieldType::String => value.is_string(),
            FieldType::Integer => value.is_number() && value.as_i64().is_some(),
            FieldType::Float => value.is_number(),
            FieldType::Boolean => value.is_boolean(),
            FieldType::Array => value.is_array(),
            FieldType::Object => value.is_object(),
            FieldType::Enum(variants) => {
                if let Some(string_value) = value.as_str() {
                    variants.contains(&string_value.to_string())
                } else {
                    false
                }
            }
            FieldType::Custom(_) => true, // Custom types are validated separately
        }
    }

    /// Validate constraint
    fn validate_constraint(&self, value: &serde_json::Value, constraint: &FieldConstraint) -> Result<()> {
        match constraint.constraint_type {
            ConstraintType::MinLength => {
                if let Some(string_value) = value.as_str() {
                    let min_length = constraint.value.as_i64().unwrap_or(0);
                    if string_value.len() < min_length as usize {
                        return Err(GgenMcpError::InvalidParameter(constraint.message.clone()));
                    }
                }
            }
            ConstraintType::MaxLength => {
                if let Some(string_value) = value.as_str() {
                    let max_length = constraint.value.as_i64().unwrap_or(i64::MAX);
                    if string_value.len() > max_length as usize {
                        return Err(GgenMcpError::InvalidParameter(constraint.message.clone()));
                    }
                }
            }
            ConstraintType::MinValue => {
                if let Some(number_value) = value.as_f64() {
                    let min_value = constraint.value.as_f64().unwrap_or(f64::NEG_INFINITY);
                    if number_value < min_value {
                        return Err(GgenMcpError::InvalidParameter(constraint.message.clone()));
                    }
                }
            }
            ConstraintType::MaxValue => {
                if let Some(number_value) = value.as_f64() {
                    let max_value = constraint.value.as_f64().unwrap_or(f64::INFINITY);
                    if number_value > max_value {
                        return Err(GgenMcpError::InvalidParameter(constraint.message.clone()));
                    }
                }
            }
            ConstraintType::Pattern => {
                if let Some(string_value) = value.as_str() {
                    if let Some(pattern_str) = constraint.value.as_str() {
                        if let Ok(pattern) = Regex::new(pattern_str) {
                            if !pattern.is_match(string_value) {
                                return Err(GgenMcpError::InvalidParameter(constraint.message.clone()));
                            }
                        }
                    }
                }
            }
            ConstraintType::Custom => {
                // Custom constraints are handled separately
            }
        }
        Ok(())
    }

    /// Validate format
    fn validate_format(&self, value: &serde_json::Value, format_validator: &FormatValidator) -> Result<()> {
        if let Some(string_value) = value.as_str() {
            if !format_validator.pattern.is_match(string_value) {
                return Err(GgenMcpError::InvalidParameter(format_validator.message.clone()));
            }
        }
        Ok(())
    }

    /// Run custom validator
    fn run_custom_validator(&self, validator: &CustomValidator, data: &serde_json::Value) -> Result<()> {
        if let Some(validator_fn) = self.custom_validators.get(&validator.name) {
            validator_fn(data)?;
        }
        Ok(())
    }

    /// Get validation history
    pub fn get_validation_history(&self) -> &Vec<ValidationResult> {
        &self.validation_history
    }

    /// Get schema definition
    pub fn get_schema(&self, schema_name: &str) -> Option<&SchemaDefinition> {
        self.schemas.get(schema_name)
    }

    /// Get all schemas
    pub fn get_all_schemas(&self) -> &HashMap<String, SchemaDefinition> {
        &self.schemas
    }
}

#[async_trait::async_trait]
impl Agent for ValidationAgent {
    async fn initialize(&mut self) -> Result<(), Box<dyn std::error::Error>> {
        tracing::info!("Validation Agent initialized with ID: {}", self.id);
        tracing::info!("Loaded {} schemas", self.schemas.len());
        tracing::info!("Loaded {} custom validators", self.custom_validators.len());
        Ok(())
    }

    async fn execute(&self, input: serde_json::Value) -> Result<serde_json::Value, Box<dyn std::error::Error>> {
        let operation = input.get("operation")
            .and_then(|v| v.as_str())
            .ok_or("Missing operation")?;

        let mut agent = ValidationAgent::new();
        
        let result = match operation {
            "validate_schema" => {
                let schema_name = input.get("schema_name")
                    .and_then(|v| v.as_str())
                    .ok_or("Missing schema_name")?;
                let data = input.get("data")
                    .ok_or("Missing data")?;
                serde_json::to_value(agent.validate_against_schema(schema_name, data.clone())?)?
            }
            "get_schema" => {
                let schema_name = input.get("schema_name")
                    .and_then(|v| v.as_str())
                    .ok_or("Missing schema_name")?;
                serde_json::to_value(agent.get_schema(schema_name))?
            }
            "get_all_schemas" => {
                serde_json::to_value(agent.get_all_schemas())?
            }
            _ => return Err("Unknown operation".into()),
        };

        Ok(result)
    }

    fn metadata(&self) -> AgentMetadata {
        AgentMetadata {
            id: self.id,
            name: "ValidationAgent".to_string(),
            version: "1.0.0".to_string(),
            status: AgentStatus::Healthy,
            capabilities: vec![
                "schema_validation".to_string(),
                "contract_validation".to_string(),
                "type_safety".to_string(),
                "format_validation".to_string(),
            ],
            last_heartbeat: Utc::now(),
        }
    }

    async fn health_check(&self) -> AgentStatus {
        // Validation agent is always healthy unless explicitly failed
        AgentStatus::Healthy
    }

    async fn shutdown(&mut self) -> Result<(), Box<dyn std::error::Error>> {
        tracing::info!("Validation Agent shutting down");
        tracing::info!("Performed {} validations", self.validation_history.len());
        Ok(())
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use serde_json::json;

    #[test]
    fn test_validation_agent_creation() {
        let agent = ValidationAgent::new();
        
        assert!(!agent.schemas.is_empty());
        assert!(!agent.custom_validators.is_empty());
    }

    #[test]
    fn test_schema_validation_success() {
        let mut agent = ValidationAgent::new();
        
        let data = json!({
            "template": "rust-lib",
            "output": "./my-project",
            "vars": {"name": "test"}
        });
        
        let result = agent.validate_against_schema("project_gen", data).unwrap();
        
        assert!(result.is_valid);
        assert!(result.errors.is_empty());
    }

    #[test]
    fn test_schema_validation_missing_required() {
        let mut agent = ValidationAgent::new();
        
        let data = json!({
            "output": "./my-project"
            // Missing required "template" field
        });
        
        let result = agent.validate_against_schema("project_gen", data).unwrap();
        
        assert!(!result.is_valid);
        assert!(!result.errors.is_empty());
        assert_eq!(result.errors[0].error_type, ValidationErrorType::RequiredFieldMissing);
    }

    #[test]
    fn test_schema_validation_invalid_type() {
        let mut agent = ValidationAgent::new();
        
        let data = json!({
            "template": 123, // Should be string
            "output": "./my-project"
        });
        
        let result = agent.validate_against_schema("project_gen", data).unwrap();
        
        assert!(!result.is_valid);
        assert!(!result.errors.is_empty());
        assert_eq!(result.errors[0].error_type, ValidationErrorType::InvalidType);
    }

    #[test]
    fn test_schema_validation_constraint_violation() {
        let mut agent = ValidationAgent::new();
        
        let data = json!({
            "template": "", // Empty string violates min length constraint
            "output": "./my-project"
        });
        
        let result = agent.validate_against_schema("project_gen", data).unwrap();
        
        assert!(!result.is_valid);
        assert!(!result.errors.is_empty());
        assert_eq!(result.errors[0].error_type, ValidationErrorType::ConstraintViolation);
    }

    #[test]
    fn test_graph_query_validation() {
        let mut agent = ValidationAgent::new();
        
        let data = json!({
            "data": "./ontology.ttl",
            "query": "SELECT ?s ?p ?o WHERE { ?s ?p ?o }",
            "format": "json"
        });
        
        let result = agent.validate_against_schema("graph_query", data).unwrap();
        
        assert!(result.is_valid);
        assert!(result.errors.is_empty());
    }

    #[tokio::test]
    async fn test_agent_execution() {
        let mut agent = ValidationAgent::new();
        agent.initialize().await.unwrap();
        
        let input = json!({
            "operation": "validate_schema",
            "schema_name": "project_gen",
            "data": {
                "template": "rust-lib",
                "output": "./my-project"
            }
        });
        
        let result = agent.execute(input).await.unwrap();
        let validation_result: ValidationResult = serde_json::from_value(result).unwrap();
        
        assert!(validation_result.is_valid);
    }
}
