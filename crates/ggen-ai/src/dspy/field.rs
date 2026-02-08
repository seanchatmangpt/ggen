//! DSPy Field Types - InputField and OutputField

use regex::Regex;
use serde::{Deserialize, Serialize};
use serde_json::Value;
use std::collections::HashMap;

use crate::error::{GgenAiError, Result};

/// Metadata about a field (input or output)
#[derive(Debug, Clone, Serialize, Deserialize, PartialEq, Eq)]
pub struct FieldMetadata {
    /// Field name (must be valid Rust identifier)
    pub name: String,

    /// Human-readable description
    pub desc: String,

    /// Rust type annotation as string (e.g., "String", "i32", "Vec<String>")
    pub type_annotation: String,

    /// Optional prefix for context setting
    pub prefix: Option<String>,
}

impl FieldMetadata {
    /// Create new field metadata
    pub fn new(
        name: impl Into<String>, desc: impl Into<String>, type_annotation: impl Into<String>,
    ) -> Self {
        Self {
            name: name.into(),
            desc: desc.into(),
            type_annotation: type_annotation.into(),
            prefix: None,
        }
    }

    /// Add a prefix (e.g., for output fields with examples)
    pub fn with_prefix(mut self, prefix: impl Into<String>) -> Self {
        self.prefix = Some(prefix.into());
        self
    }
}

/// Constraints that can be applied to a field
///
/// These constraints are used to validate values and express domain requirements.
/// All constraints are optional and default to None (no constraint).
#[derive(Debug, Clone, Serialize, Deserialize, Default, PartialEq, Eq)]
pub struct FieldConstraints {
    /// Whether the field is required (not null/empty)
    pub required: bool,

    /// Minimum number of items (for arrays/collections)
    pub min_items: Option<usize>,

    /// Maximum number of items (for arrays/collections)
    pub max_items: Option<usize>,

    /// Minimum length of string value
    pub min_length: Option<usize>,

    /// Maximum length of string value
    pub max_length: Option<usize>,

    /// Regular expression pattern (as string) that value must match
    pub pattern: Option<String>,

    /// Enumerated allowed values
    pub enum_values: Option<Vec<String>>,

    /// Semantic type from RDF vocabulary (e.g., "fibo:Product", "schema:Person")
    pub semantic_type: Option<String>,

    /// XSD datatype or Rust type constraint (e.g., "xsd:string", "xsd:integer")
    pub datatype: Option<String>,
}

impl FieldConstraints {
    /// Create a new set of constraints with all fields defaulting to None
    pub fn new() -> Self {
        Self::default()
    }

    /// Set the required flag
    pub fn required(mut self, required: bool) -> Self {
        self.required = required;
        self
    }

    /// Set minimum items constraint
    pub fn min_items(mut self, min: usize) -> Self {
        self.min_items = Some(min);
        self
    }

    /// Set maximum items constraint
    pub fn max_items(mut self, max: usize) -> Self {
        self.max_items = Some(max);
        self
    }

    /// Set minimum length constraint
    pub fn min_length(mut self, min: usize) -> Self {
        self.min_length = Some(min);
        self
    }

    /// Set maximum length constraint
    pub fn max_length(mut self, max: usize) -> Self {
        self.max_length = Some(max);
        self
    }

    /// Set regex pattern constraint
    pub fn pattern(mut self, pattern: impl Into<String>) -> Self {
        self.pattern = Some(pattern.into());
        self
    }

    /// Set enumerated values constraint
    pub fn enum_values(mut self, values: Vec<String>) -> Self {
        self.enum_values = Some(values);
        self
    }

    /// Set semantic type constraint
    pub fn semantic_type(mut self, semantic_type: impl Into<String>) -> Self {
        self.semantic_type = Some(semantic_type.into());
        self
    }

    /// Set datatype constraint
    pub fn datatype(mut self, datatype: impl Into<String>) -> Self {
        self.datatype = Some(datatype.into());
        self
    }

    /// Validate that a value satisfies all constraints
    ///
    /// # Returns
    /// - `Ok(())` if all constraints are satisfied
    /// - `Err(GgenAiError)` with descriptive message if any constraint fails
    pub fn is_satisfied(&self, value: &Value) -> Result<()> {
        // Check required constraint
        if self.required && value.is_null() {
            return Err(GgenAiError::validation(
                "Field is required but value is null".to_string(),
            ));
        }

        // Skip further checks if value is null and not required
        if value.is_null() {
            return Ok(());
        }

        // Check enum constraint
        if let Some(ref enum_values) = self.enum_values {
            let value_str = match value {
                Value::String(s) => s.clone(),
                Value::Number(n) => n.to_string(),
                Value::Bool(b) => b.to_string(),
                _ => {
                    return Err(GgenAiError::validation(format!(
                        "Enum constraint applied to non-scalar value"
                    )))
                }
            };

            if !enum_values.contains(&value_str) {
                return Err(GgenAiError::validation(format!(
                    "Value '{}' not in allowed enum values: {:?}",
                    value_str, enum_values
                )));
            }
        }

        // String-specific constraints
        if let Some(s) = value.as_str() {
            if let Some(min) = self.min_length {
                if s.len() < min {
                    return Err(GgenAiError::validation(format!(
                        "String length {} is less than minimum {}",
                        s.len(),
                        min
                    )));
                }
            }

            if let Some(max) = self.max_length {
                if s.len() > max {
                    return Err(GgenAiError::validation(format!(
                        "String length {} exceeds maximum {}",
                        s.len(),
                        max
                    )));
                }
            }

            if let Some(ref pattern) = self.pattern {
                match Regex::new(pattern) {
                    Ok(regex) => {
                        if !regex.is_match(s) {
                            return Err(GgenAiError::validation(format!(
                                "Value '{}' does not match pattern '{}'",
                                s, pattern
                            )));
                        }
                    }
                    Err(e) => {
                        return Err(GgenAiError::validation(format!(
                            "Invalid regex pattern '{}': {}",
                            pattern, e
                        )));
                    }
                }
            }
        }

        // Array-specific constraints
        if let Some(arr) = value.as_array() {
            if let Some(min) = self.min_items {
                if arr.len() < min {
                    return Err(GgenAiError::validation(format!(
                        "Array length {} is less than minimum {}",
                        arr.len(),
                        min
                    )));
                }
            }

            if let Some(max) = self.max_items {
                if arr.len() > max {
                    return Err(GgenAiError::validation(format!(
                        "Array length {} exceeds maximum {}",
                        arr.len(),
                        max
                    )));
                }
            }
        }

        Ok(())
    }

    /// Check if any constraints are defined
    pub fn has_constraints(&self) -> bool {
        self.required
            || self.min_items.is_some()
            || self.max_items.is_some()
            || self.min_length.is_some()
            || self.max_length.is_some()
            || self.pattern.is_some()
            || self.enum_values.is_some()
            || self.semantic_type.is_some()
            || self.datatype.is_some()
    }
}

/// SHACL (Shapes Constraint Language) constraint representation
///
/// Maps RDF SHACL properties to field constraints. SHACL is a W3C specification
/// for validating RDF graphs. This struct captures essential SHACL properties
/// that map to DSPy field constraints.
#[derive(Debug, Clone, Serialize, Deserialize, PartialEq, Eq)]
pub struct SHACLConstraint {
    /// Shape node URI or identifier
    pub shape_id: String,

    /// Targeted class (sh:targetClass)
    pub target_class: Option<String>,

    /// Property being constrained (sh:path)
    pub property: Option<String>,

    /// Datatype constraint (sh:datatype)
    pub datatype: Option<String>,

    /// Cardinality: minimum value (sh:minCount)
    pub min_count: Option<usize>,

    /// Cardinality: maximum value (sh:maxCount)
    pub max_count: Option<usize>,

    /// Min length constraint (sh:minLength)
    pub min_length: Option<usize>,

    /// Max length constraint (sh:maxLength)
    pub max_length: Option<usize>,

    /// Pattern constraint (sh:pattern) - regex
    pub pattern: Option<String>,

    /// In constraint: allowed values (sh:in)
    pub in_values: Option<Vec<String>>,

    /// Node kind constraint (sh:nodeKind: IRI, Literal, BlankNode, etc.)
    pub node_kind: Option<String>,

    /// Custom properties (other SHACL properties not explicitly mapped)
    pub custom_properties: HashMap<String, String>,
}

impl SHACLConstraint {
    /// Create a new SHACL constraint with minimal required information
    pub fn new(shape_id: impl Into<String>) -> Self {
        Self {
            shape_id: shape_id.into(),
            target_class: None,
            property: None,
            datatype: None,
            min_count: None,
            max_count: None,
            min_length: None,
            max_length: None,
            pattern: None,
            in_values: None,
            node_kind: None,
            custom_properties: HashMap::new(),
        }
    }

    /// Convert SHACL constraint to FieldConstraints
    pub fn to_field_constraints(&self) -> FieldConstraints {
        FieldConstraints {
            required: self.min_count.map_or(false, |c| c > 0),
            min_items: self.min_count,
            max_items: self.max_count,
            min_length: self.min_length,
            max_length: self.max_length,
            pattern: self.pattern.clone(),
            enum_values: self.in_values.clone(),
            semantic_type: self.target_class.clone(),
            datatype: self.datatype.clone(),
        }
    }
}

/// Input field to a module - carries user-provided data
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct InputField {
    pub metadata: FieldMetadata,

    /// Constraints on the input field (optional, defaults to empty constraints)
    #[serde(default)]
    pub constraints: FieldConstraints,
}

impl InputField {
    /// Create new input field
    pub fn new(
        name: impl Into<String>, desc: impl Into<String>, type_annotation: impl Into<String>,
    ) -> Self {
        Self {
            metadata: FieldMetadata::new(name, desc, type_annotation),
            constraints: FieldConstraints::default(),
        }
    }

    /// Create with prefix
    pub fn with_prefix(
        name: impl Into<String>, desc: impl Into<String>, type_annotation: impl Into<String>,
        prefix: impl Into<String>,
    ) -> Self {
        Self {
            metadata: FieldMetadata::new(name, desc, type_annotation).with_prefix(prefix),
            constraints: FieldConstraints::default(),
        }
    }

    /// Create with constraints
    pub fn with_constraints(
        name: impl Into<String>, desc: impl Into<String>, type_annotation: impl Into<String>,
        constraints: FieldConstraints,
    ) -> Self {
        Self {
            metadata: FieldMetadata::new(name, desc, type_annotation),
            constraints,
        }
    }

    /// Get field name
    pub fn name(&self) -> &str {
        &self.metadata.name
    }

    /// Get field description
    pub fn desc(&self) -> &str {
        &self.metadata.desc
    }

    /// Get type annotation
    pub fn type_annotation(&self) -> &str {
        &self.metadata.type_annotation
    }

    /// Add constraints to this field
    pub fn add_constraints(mut self, constraints: FieldConstraints) -> Self {
        self.constraints = constraints;
        self
    }

    /// Validate a value against field constraints
    pub fn validate(&self, value: &Value) -> Result<()> {
        self.constraints.is_satisfied(value)
    }

    /// Set minimum items constraint (for collections)
    pub fn with_min_items(mut self, min: usize) -> Self {
        self.constraints.min_items = Some(min);
        self
    }

    /// Set maximum items constraint (for collections)
    pub fn with_max_items(mut self, max: usize) -> Self {
        self.constraints.max_items = Some(max);
        self
    }

    /// Set minimum length constraint (for strings)
    pub fn with_min_length(mut self, min: usize) -> Self {
        self.constraints.min_length = Some(min);
        self
    }

    /// Set maximum length constraint (for strings)
    pub fn with_max_length(mut self, max: usize) -> Self {
        self.constraints.max_length = Some(max);
        self
    }

    /// Set regex pattern constraint (for strings)
    pub fn with_pattern(mut self, pattern: impl Into<String>) -> Self {
        self.constraints.pattern = Some(pattern.into());
        self
    }

    /// Set enumerated allowed values
    pub fn with_enum_values(mut self, values: Vec<String>) -> Self {
        self.constraints.enum_values = Some(values);
        self
    }

    /// Set semantic type from RDF vocabulary
    pub fn with_semantic_type(mut self, semantic_type: impl Into<String>) -> Self {
        self.constraints.semantic_type = Some(semantic_type.into());
        self
    }

    /// Set the required flag
    pub fn required(mut self, required: bool) -> Self {
        self.constraints.required = required;
        self
    }

    /// Create from a SHACL constraint
    pub fn from_shacl(
        name: impl Into<String>, desc: impl Into<String>, type_annotation: impl Into<String>,
        shacl: &SHACLConstraint,
    ) -> Self {
        Self {
            metadata: FieldMetadata::new(name, desc, type_annotation),
            constraints: shacl.to_field_constraints(),
        }
    }
}

/// Output field from a module - model-generated data
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct OutputField {
    pub metadata: FieldMetadata,

    /// Constraints on the output field (optional, defaults to empty constraints)
    #[serde(default)]
    pub constraints: FieldConstraints,
}

impl OutputField {
    /// Create new output field
    pub fn new(
        name: impl Into<String>, desc: impl Into<String>, type_annotation: impl Into<String>,
    ) -> Self {
        Self {
            metadata: FieldMetadata::new(name, desc, type_annotation),
            constraints: FieldConstraints::default(),
        }
    }

    /// Create with prefix
    pub fn with_prefix(
        name: impl Into<String>, desc: impl Into<String>, type_annotation: impl Into<String>,
        prefix: impl Into<String>,
    ) -> Self {
        Self {
            metadata: FieldMetadata::new(name, desc, type_annotation).with_prefix(prefix),
            constraints: FieldConstraints::default(),
        }
    }

    /// Create with constraints
    pub fn with_constraints(
        name: impl Into<String>, desc: impl Into<String>, type_annotation: impl Into<String>,
        constraints: FieldConstraints,
    ) -> Self {
        Self {
            metadata: FieldMetadata::new(name, desc, type_annotation),
            constraints,
        }
    }

    /// Get field name
    pub fn name(&self) -> &str {
        &self.metadata.name
    }

    /// Get field description
    pub fn desc(&self) -> &str {
        &self.metadata.desc
    }

    /// Get type annotation
    pub fn type_annotation(&self) -> &str {
        &self.metadata.type_annotation
    }

    /// Add constraints to this field
    pub fn add_constraints(mut self, constraints: FieldConstraints) -> Self {
        self.constraints = constraints;
        self
    }

    /// Validate a value against field constraints
    pub fn validate(&self, value: &Value) -> Result<()> {
        self.constraints.is_satisfied(value)
    }

    /// Set minimum items constraint (for collections)
    pub fn with_min_items(mut self, min: usize) -> Self {
        self.constraints.min_items = Some(min);
        self
    }

    /// Set maximum items constraint (for collections)
    pub fn with_max_items(mut self, max: usize) -> Self {
        self.constraints.max_items = Some(max);
        self
    }

    /// Set minimum length constraint (for strings)
    pub fn with_min_length(mut self, min: usize) -> Self {
        self.constraints.min_length = Some(min);
        self
    }

    /// Set maximum length constraint (for strings)
    pub fn with_max_length(mut self, max: usize) -> Self {
        self.constraints.max_length = Some(max);
        self
    }

    /// Set regex pattern constraint (for strings)
    pub fn with_pattern(mut self, pattern: impl Into<String>) -> Self {
        self.constraints.pattern = Some(pattern.into());
        self
    }

    /// Set enumerated allowed values
    pub fn with_enum_values(mut self, values: Vec<String>) -> Self {
        self.constraints.enum_values = Some(values);
        self
    }

    /// Set semantic type from RDF vocabulary
    pub fn with_semantic_type(mut self, semantic_type: impl Into<String>) -> Self {
        self.constraints.semantic_type = Some(semantic_type.into());
        self
    }

    /// Set the required flag
    pub fn required(mut self, required: bool) -> Self {
        self.constraints.required = required;
        self
    }

    /// Create from a SHACL constraint
    pub fn from_shacl(
        name: impl Into<String>, desc: impl Into<String>, type_annotation: impl Into<String>,
        shacl: &SHACLConstraint,
    ) -> Self {
        Self {
            metadata: FieldMetadata::new(name, desc, type_annotation),
            constraints: shacl.to_field_constraints(),
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use serde_json::json;

    // ===== Field Creation Tests (Backward Compatibility) =====

    #[test]
    fn test_input_field_creation() {
        let field = InputField::new("query", "Search query", "String");
        assert_eq!(field.name(), "query");
        assert_eq!(field.desc(), "Search query");
        assert_eq!(field.type_annotation(), "String");
        assert!(!field.constraints.has_constraints());
    }

    #[test]
    fn test_output_field_creation() {
        let field = OutputField::new("answer", "Generated answer", "String");
        assert_eq!(field.name(), "answer");
        assert_eq!(field.desc(), "Generated answer");
        assert_eq!(field.type_annotation(), "String");
        assert!(!field.constraints.has_constraints());
    }

    #[test]
    fn test_field_with_prefix() {
        let field =
            InputField::with_prefix("question", "Question to answer", "String", "Question: ");
        assert_eq!(field.metadata.prefix, Some("Question: ".to_string()));
        assert!(!field.constraints.has_constraints());
    }

    // ===== FieldConstraints Builder Tests =====

    #[test]
    fn test_constraints_default() {
        let constraints = FieldConstraints::default();
        assert!(!constraints.required);
        assert!(constraints.min_items.is_none());
        assert!(constraints.max_items.is_none());
        assert!(constraints.min_length.is_none());
        assert!(constraints.max_length.is_none());
        assert!(constraints.pattern.is_none());
        assert!(constraints.enum_values.is_none());
        assert!(constraints.semantic_type.is_none());
        assert!(constraints.datatype.is_none());
    }

    #[test]
    fn test_constraints_builder() {
        let constraints = FieldConstraints::new()
            .required(true)
            .min_length(5)
            .max_length(50)
            .semantic_type("schema:Text")
            .datatype("xsd:string");

        assert!(constraints.required);
        assert_eq!(constraints.min_length, Some(5));
        assert_eq!(constraints.max_length, Some(50));
        assert_eq!(constraints.semantic_type, Some("schema:Text".to_string()));
        assert_eq!(constraints.datatype, Some("xsd:string".to_string()));
    }

    #[test]
    fn test_constraints_has_constraints() {
        let empty = FieldConstraints::default();
        assert!(!empty.has_constraints());

        let with_required = FieldConstraints::new().required(true);
        assert!(with_required.has_constraints());

        let with_min_items = FieldConstraints::new().min_items(1);
        assert!(with_min_items.has_constraints());

        let with_semantic = FieldConstraints::new().semantic_type("fibo:Product");
        assert!(with_semantic.has_constraints());
    }

    // ===== Required Constraint Tests =====

    #[test]
    fn test_required_constraint_passes() {
        let constraints = FieldConstraints::new().required(true);
        assert!(constraints.is_satisfied(&json!("value")).is_ok());
        assert!(constraints.is_satisfied(&json!(123)).is_ok());
        assert!(constraints.is_satisfied(&json!([])).is_ok());
    }

    #[test]
    fn test_required_constraint_fails() {
        let constraints = FieldConstraints::new().required(true);
        let result = constraints.is_satisfied(&json!(null));
        assert!(result.is_err());
        assert!(result.unwrap_err().to_string().contains("required"));
    }

    #[test]
    fn test_not_required_allows_null() {
        let constraints = FieldConstraints::new();
        assert!(constraints.is_satisfied(&json!(null)).is_ok());
    }

    // ===== String Length Constraint Tests =====

    #[test]
    fn test_min_length_passes() {
        let constraints = FieldConstraints::new().min_length(3);
        assert!(constraints.is_satisfied(&json!("hello")).is_ok());
        assert!(constraints.is_satisfied(&json!("abc")).is_ok());
    }

    #[test]
    fn test_min_length_fails() {
        let constraints = FieldConstraints::new().min_length(5);
        let result = constraints.is_satisfied(&json!("hi"));
        assert!(result.is_err());
        assert!(result.unwrap_err().to_string().contains("minimum"));
    }

    #[test]
    fn test_max_length_passes() {
        let constraints = FieldConstraints::new().max_length(10);
        assert!(constraints.is_satisfied(&json!("hello")).is_ok());
        assert!(constraints.is_satisfied(&json!("1234567890")).is_ok());
    }

    #[test]
    fn test_max_length_fails() {
        let constraints = FieldConstraints::new().max_length(5);
        let result = constraints.is_satisfied(&json!("hello world"));
        assert!(result.is_err());
        assert!(result.unwrap_err().to_string().contains("exceeds"));
    }

    #[test]
    fn test_string_length_both_bounds() {
        let constraints = FieldConstraints::new().min_length(5).max_length(10);

        assert!(constraints.is_satisfied(&json!("hello")).is_ok());
        assert!(constraints.is_satisfied(&json!("1234567890")).is_ok());
        assert!(constraints.is_satisfied(&json!("hi")).is_err());
        assert!(constraints
            .is_satisfied(&json!("this is too long"))
            .is_err());
    }

    // ===== Array Length Constraint Tests =====

    #[test]
    fn test_min_items_passes() {
        let constraints = FieldConstraints::new().min_items(2);
        assert!(constraints.is_satisfied(&json!([1, 2, 3])).is_ok());
        assert!(constraints.is_satisfied(&json!([1, 2])).is_ok());
    }

    #[test]
    fn test_min_items_fails() {
        let constraints = FieldConstraints::new().min_items(3);
        let result = constraints.is_satisfied(&json!([1, 2]));
        assert!(result.is_err());
        assert!(result.unwrap_err().to_string().contains("minimum"));
    }

    #[test]
    fn test_max_items_passes() {
        let constraints = FieldConstraints::new().max_items(5);
        assert!(constraints.is_satisfied(&json!([1, 2, 3])).is_ok());
    }

    #[test]
    fn test_max_items_fails() {
        let constraints = FieldConstraints::new().max_items(2);
        let result = constraints.is_satisfied(&json!([1, 2, 3]));
        assert!(result.is_err());
        assert!(result.unwrap_err().to_string().contains("exceeds"));
    }

    // ===== Enum Constraint Tests =====

    #[test]
    fn test_enum_constraint_passes() {
        let constraints = FieldConstraints::new().enum_values(vec![
            "red".to_string(),
            "green".to_string(),
            "blue".to_string(),
        ]);

        assert!(constraints.is_satisfied(&json!("red")).is_ok());
        assert!(constraints.is_satisfied(&json!("green")).is_ok());
    }

    #[test]
    fn test_enum_constraint_fails() {
        let constraints =
            FieldConstraints::new().enum_values(vec!["red".to_string(), "green".to_string()]);

        let result = constraints.is_satisfied(&json!("yellow"));
        assert!(result.is_err());
        assert!(result.unwrap_err().to_string().contains("enum"));
    }

    #[test]
    fn test_enum_with_numbers() {
        let constraints =
            FieldConstraints::new().enum_values(vec!["1".to_string(), "2".to_string()]);

        assert!(constraints.is_satisfied(&json!(1)).is_ok());
        assert!(constraints.is_satisfied(&json!(2)).is_ok());
        let result = constraints.is_satisfied(&json!(3));
        assert!(result.is_err());
    }

    // ===== Regex Pattern Constraint Tests =====

    #[test]
    fn test_pattern_constraint_passes() {
        let constraints = FieldConstraints::new().pattern(r"^[a-z]+@[a-z]+\.[a-z]+$");

        assert!(constraints.is_satisfied(&json!("user@example.com")).is_ok());
    }

    #[test]
    fn test_pattern_constraint_fails() {
        let constraints = FieldConstraints::new().pattern(r"^[a-z]+@[a-z]+\.[a-z]+$");

        let result = constraints.is_satisfied(&json!("not-an-email"));
        assert!(result.is_err());
        assert!(result.unwrap_err().to_string().contains("pattern"));
    }

    #[test]
    fn test_pattern_with_digits() {
        let constraints = FieldConstraints::new().pattern(r"^\d{3}-\d{3}-\d{4}$");

        assert!(constraints.is_satisfied(&json!("123-456-7890")).is_ok());
        assert!(constraints.is_satisfied(&json!("123-456")).is_err());
    }

    #[test]
    fn test_invalid_regex_pattern() {
        let constraints = FieldConstraints::new().pattern("[invalid(pattern");

        let result = constraints.is_satisfied(&json!("any"));
        assert!(result.is_err());
        assert!(result.unwrap_err().to_string().contains("Invalid regex"));
    }

    // ===== Field-Level Validation Tests =====

    #[test]
    fn test_input_field_validation() {
        let field = InputField::new("email", "User email", "String").add_constraints(
            FieldConstraints::new()
                .required(true)
                .pattern(r"^[a-z]+@[a-z]+\.[a-z]+$"),
        );

        assert!(field.validate(&json!("user@example.com")).is_ok());
        assert!(field.validate(&json!("invalid")).is_err());
        assert!(field.validate(&json!(null)).is_err());
    }

    #[test]
    fn test_output_field_validation() {
        let field = OutputField::new("tags", "Output tags", "Vec<String>")
            .add_constraints(FieldConstraints::new().min_items(1).max_items(5));

        assert!(field.validate(&json!(["tag1"])).is_ok());
        assert!(field.validate(&json!(["a", "b", "c"])).is_ok());
        assert!(field.validate(&json!([])).is_err());
        assert!(field
            .validate(&json!(["a", "b", "c", "d", "e", "f"]))
            .is_err());
    }

    #[test]
    fn test_input_field_with_constraints_constructor() {
        let constraints = FieldConstraints::new().required(true).min_length(5);

        let field = InputField::with_constraints("query", "Search query", "String", constraints);

        assert_eq!(field.name(), "query");
        assert!(field.constraints.required);
        assert_eq!(field.constraints.min_length, Some(5));
    }

    // ===== Combination Constraint Tests =====

    #[test]
    fn test_multiple_constraints_all_satisfied() {
        let constraints = FieldConstraints::new()
            .required(true)
            .min_length(3)
            .max_length(20)
            .pattern(r"^[a-zA-Z0-9_]+$")
            .semantic_type("schema:Identifier");

        assert!(constraints.is_satisfied(&json!("valid_name")).is_ok());
    }

    #[test]
    fn test_multiple_constraints_one_fails() {
        let constraints = FieldConstraints::new()
            .min_length(3)
            .max_length(20)
            .pattern(r"^[a-zA-Z0-9_]+$");

        // Fails max length
        assert!(constraints
            .is_satisfied(&json!("this_is_a_very_long_string_that_exceeds_max"))
            .is_err());
    }

    // ===== Serialization Tests =====

    #[test]
    fn test_field_constraints_serialization() {
        let constraints = FieldConstraints::new()
            .required(true)
            .min_length(5)
            .max_length(50)
            .enum_values(vec!["option1".to_string(), "option2".to_string()]);

        let json = serde_json::to_value(&constraints).expect("serialization failed");
        let deserialized: FieldConstraints =
            serde_json::from_value(json).expect("deserialization failed");

        assert_eq!(deserialized.required, true);
        assert_eq!(deserialized.min_length, Some(5));
        assert_eq!(deserialized.max_length, Some(50));
        assert_eq!(deserialized.enum_values.unwrap().len(), 2);
    }

    #[test]
    fn test_input_field_with_constraints_serialization() {
        let field = InputField::new("test", "Test field", "String")
            .add_constraints(FieldConstraints::new().required(true).min_length(3));

        let json = serde_json::to_value(&field).expect("serialization failed");
        let deserialized: InputField =
            serde_json::from_value(json).expect("deserialization failed");

        assert_eq!(deserialized.name(), "test");
        assert!(deserialized.constraints.required);
    }

    #[test]
    fn test_output_field_with_constraints_serialization() {
        let field = OutputField::new("result", "Result field", "String")
            .add_constraints(FieldConstraints::new().max_length(100));

        let json = serde_json::to_value(&field).expect("serialization failed");
        let deserialized: OutputField =
            serde_json::from_value(json).expect("deserialization failed");

        assert_eq!(deserialized.name(), "result");
        assert_eq!(deserialized.constraints.max_length, Some(100));
    }

    // ===== Edge Cases =====

    #[test]
    fn test_zero_length_string_with_min_constraint() {
        let constraints = FieldConstraints::new().min_length(1);
        assert!(constraints.is_satisfied(&json!("")).is_err());
    }

    #[test]
    fn test_empty_array_with_min_items() {
        let constraints = FieldConstraints::new().min_items(1);
        assert!(constraints.is_satisfied(&json!([])).is_err());
    }

    #[test]
    fn test_constraints_on_non_string_value() {
        let constraints = FieldConstraints::new().min_length(5);
        // min_length constraint only applies to strings, so numbers pass
        assert!(constraints.is_satisfied(&json!(12345)).is_ok());
    }

    #[test]
    fn test_enum_constraint_requires_scalar() {
        let constraints = FieldConstraints::new().enum_values(vec!["a".to_string()]);

        // Objects and arrays should fail enum constraint
        assert!(constraints.is_satisfied(&json!({})).is_err());
        assert!(constraints.is_satisfied(&json!([])).is_err());
    }

    // ===== InputField Builder Methods Tests =====

    #[test]
    fn test_input_field_with_min_items() {
        let field = InputField::new("items", "List of items", "Vec<String>").with_min_items(1);
        assert_eq!(field.constraints.min_items, Some(1));
    }

    #[test]
    fn test_input_field_with_max_items() {
        let field = InputField::new("items", "List of items", "Vec<String>").with_max_items(10);
        assert_eq!(field.constraints.max_items, Some(10));
    }

    #[test]
    fn test_input_field_with_min_length() {
        let field = InputField::new("name", "User name", "String").with_min_length(3);
        assert_eq!(field.constraints.min_length, Some(3));
    }

    #[test]
    fn test_input_field_with_max_length() {
        let field = InputField::new("name", "User name", "String").with_max_length(100);
        assert_eq!(field.constraints.max_length, Some(100));
    }

    #[test]
    fn test_input_field_with_pattern() {
        let field = InputField::new("email", "Email address", "String")
            .with_pattern(r"^[a-z0-9._%+-]+@[a-z0-9.-]+\.[a-z]{2,}$");
        assert_eq!(
            field.constraints.pattern,
            Some(r"^[a-z0-9._%+-]+@[a-z0-9.-]+\.[a-z]{2,}$".to_string())
        );
    }

    #[test]
    fn test_input_field_with_enum_values() {
        let values = vec![
            "active".to_string(),
            "inactive".to_string(),
            "pending".to_string(),
        ];
        let field = InputField::new("status", "Status", "String").with_enum_values(values.clone());
        assert_eq!(field.constraints.enum_values, Some(values));
    }

    #[test]
    fn test_input_field_with_semantic_type() {
        let field = InputField::new("person", "Person object", "Person")
            .with_semantic_type("schema:Person");
        assert_eq!(
            field.constraints.semantic_type,
            Some("schema:Person".to_string())
        );
    }

    #[test]
    fn test_input_field_required() {
        let field = InputField::new("id", "User ID", "u64").required(true);
        assert!(field.constraints.required);
    }

    #[test]
    fn test_input_field_builder_chaining() {
        let field = InputField::new("username", "Username", "String")
            .with_min_length(3)
            .with_max_length(50)
            .with_pattern(r"^[a-zA-Z0-9_]+$")
            .required(true);

        assert_eq!(field.constraints.min_length, Some(3));
        assert_eq!(field.constraints.max_length, Some(50));
        assert_eq!(
            field.constraints.pattern,
            Some(r"^[a-zA-Z0-9_]+$".to_string())
        );
        assert!(field.constraints.required);
    }

    // ===== OutputField Builder Methods Tests =====

    #[test]
    fn test_output_field_with_min_items() {
        let field = OutputField::new("results", "List of results", "Vec<String>").with_min_items(1);
        assert_eq!(field.constraints.min_items, Some(1));
    }

    #[test]
    fn test_output_field_with_max_items() {
        let field =
            OutputField::new("results", "List of results", "Vec<String>").with_max_items(100);
        assert_eq!(field.constraints.max_items, Some(100));
    }

    #[test]
    fn test_output_field_builder_chaining() {
        let field = OutputField::new("response", "Generated response", "String")
            .with_min_length(10)
            .with_max_length(1000)
            .required(true);

        assert_eq!(field.constraints.min_length, Some(10));
        assert_eq!(field.constraints.max_length, Some(1000));
        assert!(field.constraints.required);
    }

    // ===== SHACL Constraint Tests =====

    #[test]
    fn test_shacl_constraint_creation() {
        let shacl = SHACLConstraint::new("ex:PersonShape");
        assert_eq!(shacl.shape_id, "ex:PersonShape");
        assert!(shacl.target_class.is_none());
        assert!(shacl.property.is_none());
    }

    #[test]
    fn test_shacl_constraint_to_field_constraints() {
        let shacl = SHACLConstraint {
            shape_id: "ex:EmailShape".to_string(),
            target_class: Some("schema:Person".to_string()),
            property: Some("schema:email".to_string()),
            datatype: Some("xsd:string".to_string()),
            min_count: Some(1),
            max_count: Some(1),
            min_length: Some(5),
            max_length: Some(254),
            pattern: Some(r"^[^\s@]+@[^\s@]+\.[^\s@]+$".to_string()),
            in_values: None,
            node_kind: Some("Literal".to_string()),
            custom_properties: HashMap::new(),
        };

        let constraints = shacl.to_field_constraints();
        assert!(constraints.required);
        assert_eq!(constraints.semantic_type, Some("schema:Person".to_string()));
        assert_eq!(constraints.datatype, Some("xsd:string".to_string()));
        assert_eq!(constraints.min_length, Some(5));
        assert_eq!(constraints.max_length, Some(254));
        assert!(constraints.pattern.is_some());
    }

    #[test]
    fn test_shacl_constraint_with_enum() {
        let shacl = SHACLConstraint {
            shape_id: "ex:StatusShape".to_string(),
            target_class: None,
            property: Some("ex:status".to_string()),
            datatype: None,
            min_count: Some(1),
            max_count: Some(1),
            min_length: None,
            max_length: None,
            pattern: None,
            in_values: Some(vec![
                "active".to_string(),
                "inactive".to_string(),
                "archived".to_string(),
            ]),
            node_kind: Some("Literal".to_string()),
            custom_properties: HashMap::new(),
        };

        let constraints = shacl.to_field_constraints();
        assert!(constraints.required);
        assert_eq!(
            constraints.enum_values,
            Some(vec![
                "active".to_string(),
                "inactive".to_string(),
                "archived".to_string(),
            ])
        );
    }

    #[test]
    fn test_input_field_from_shacl() {
        let shacl = SHACLConstraint {
            shape_id: "ex:PersonShape".to_string(),
            target_class: Some("schema:Person".to_string()),
            property: Some("schema:name".to_string()),
            datatype: Some("xsd:string".to_string()),
            min_count: Some(1),
            max_count: None,
            min_length: Some(1),
            max_length: Some(100),
            pattern: None,
            in_values: None,
            node_kind: Some("Literal".to_string()),
            custom_properties: HashMap::new(),
        };

        let field = InputField::from_shacl("name", "Person's name", "String", &shacl);

        assert_eq!(field.name(), "name");
        assert_eq!(field.desc(), "Person's name");
        assert_eq!(field.type_annotation(), "String");
        assert!(field.constraints.required);
        assert_eq!(
            field.constraints.semantic_type,
            Some("schema:Person".to_string())
        );
        assert_eq!(field.constraints.min_length, Some(1));
        assert_eq!(field.constraints.max_length, Some(100));
    }

    #[test]
    fn test_output_field_from_shacl() {
        let shacl = SHACLConstraint {
            shape_id: "ex:ResultShape".to_string(),
            target_class: Some("schema:SearchAction".to_string()),
            property: Some("schema:result".to_string()),
            datatype: None,
            min_count: Some(1),
            max_count: Some(10),
            min_length: None,
            max_length: None,
            pattern: None,
            in_values: None,
            node_kind: Some("IRI".to_string()),
            custom_properties: HashMap::new(),
        };

        let field = OutputField::from_shacl("results", "Search results", "Vec<String>", &shacl);

        assert_eq!(field.name(), "results");
        assert!(field.constraints.required);
        assert_eq!(field.constraints.min_items, Some(1));
        assert_eq!(field.constraints.max_items, Some(10));
    }

    #[test]
    fn test_shacl_constraint_custom_properties() {
        let mut custom_props = HashMap::new();
        custom_props.insert("sh:message".to_string(), "Invalid person name".to_string());
        custom_props.insert("sh:severity".to_string(), "sh:Warning".to_string());

        let shacl = SHACLConstraint {
            shape_id: "ex:PersonShape".to_string(),
            target_class: None,
            property: None,
            datatype: None,
            min_count: None,
            max_count: None,
            min_length: None,
            max_length: None,
            pattern: None,
            in_values: None,
            node_kind: None,
            custom_properties: custom_props,
        };

        assert_eq!(shacl.custom_properties.len(), 2);
        assert_eq!(
            shacl.custom_properties.get("sh:message"),
            Some(&"Invalid person name".to_string())
        );
    }

    #[test]
    fn test_shacl_conversion_with_no_requirements() {
        let shacl = SHACLConstraint::new("ex:OptionalShape");
        let constraints = shacl.to_field_constraints();

        assert!(!constraints.required);
        assert!(constraints.min_items.is_none());
        assert!(constraints.max_items.is_none());
    }
}
