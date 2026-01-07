//! DSPy Field Types - InputField and OutputField

use serde::{Deserialize, Serialize};

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
    pub fn new(name: impl Into<String>, desc: impl Into<String>, type_annotation: impl Into<String>) -> Self {
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

/// Input field to a module - carries user-provided data
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct InputField {
    pub metadata: FieldMetadata,
}

impl InputField {
    /// Create new input field
    pub fn new(name: impl Into<String>, desc: impl Into<String>, type_annotation: impl Into<String>) -> Self {
        Self {
            metadata: FieldMetadata::new(name, desc, type_annotation),
        }
    }

    /// Create with prefix
    pub fn with_prefix(name: impl Into<String>, desc: impl Into<String>, type_annotation: impl Into<String>, prefix: impl Into<String>) -> Self {
        Self {
            metadata: FieldMetadata::new(name, desc, type_annotation).with_prefix(prefix),
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
}

/// Output field from a module - model-generated data
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct OutputField {
    pub metadata: FieldMetadata,
}

impl OutputField {
    /// Create new output field
    pub fn new(name: impl Into<String>, desc: impl Into<String>, type_annotation: impl Into<String>) -> Self {
        Self {
            metadata: FieldMetadata::new(name, desc, type_annotation),
        }
    }

    /// Create with prefix
    pub fn with_prefix(name: impl Into<String>, desc: impl Into<String>, type_annotation: impl Into<String>, prefix: impl Into<String>) -> Self {
        Self {
            metadata: FieldMetadata::new(name, desc, type_annotation).with_prefix(prefix),
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
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_input_field_creation() {
        let field = InputField::new("query", "Search query", "String");
        assert_eq!(field.name(), "query");
        assert_eq!(field.desc(), "Search query");
        assert_eq!(field.type_annotation(), "String");
    }

    #[test]
    fn test_output_field_creation() {
        let field = OutputField::new("answer", "Generated answer", "String");
        assert_eq!(field.name(), "answer");
        assert_eq!(field.desc(), "Generated answer");
        assert_eq!(field.type_annotation(), "String");
    }

    #[test]
    fn test_field_with_prefix() {
        let field = InputField::with_prefix(
            "question",
            "Question to answer",
            "String",
            "Question: "
        );
        assert_eq!(field.metadata.prefix, Some("Question: ".to_string()));
    }
}
