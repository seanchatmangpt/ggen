// RDF Specification Reader for Code Generation
// Demonstrates: RDF parsing, SPARQL queries, specification-driven generation
// ggen Pattern: A = μ(O) where O is RDF ontology, A is generated artifacts

use serde::{Deserialize, Serialize};
use std::collections::HashMap;
use thiserror::Error;

#[derive(Debug, Error)]
pub enum RdfSpecError {
    #[error("RDF parsing error: {0}")]
    ParseError(String),

    #[error("Invalid specification: {0}")]
    InvalidSpec(String),

    #[error("Entity not found: {0}")]
    EntityNotFound(String),
}

pub type RdfSpecResult<T> = Result<T, RdfSpecError>;

// ============================================================================
// RDF SPECIFICATION TYPES
// ============================================================================

/// Represents a code generation specification parsed from RDF
#[derive(Debug, Clone, Serialize, Deserialize, PartialEq)]
pub struct CodeGenSpec {
    pub id: String,
    pub name: String,
    pub description: String,
    pub input_language: String,
    pub output_language: String,
    pub framework: Option<String>,
    pub complexity_level: ComplexityLevel,
    pub validation_steps: Vec<ValidationRule>,
    pub metadata: HashMap<String, String>,
}

#[derive(Debug, Clone, Serialize, Deserialize, PartialEq)]
pub enum ComplexityLevel {
    Simple,
    Intermediate,
    Advanced,
}

/// Validation rule extracted from RDF specification
#[derive(Debug, Clone, Serialize, Deserialize, PartialEq)]
pub struct ValidationRule {
    pub rule_type: ValidationType,
    pub enabled: bool,
    pub config: HashMap<String, String>,
}

#[derive(Debug, Clone, Serialize, Deserialize, PartialEq)]
pub enum ValidationType {
    Syntax,
    Format,
    Compile,
    Test,
    Security,
}

// ============================================================================
// RDF SPECIFICATION PARSER
// ============================================================================

pub struct RdfSpecParser {
    triples: Vec<(String, String, String)>,
}

impl RdfSpecParser {
    pub fn new() -> Self {
        Self {
            triples: Vec::new(),
        }
    }

    /// Add a triple (subject, predicate, object) to the parser
    pub fn add_triple(&mut self, subject: impl Into<String>, predicate: impl Into<String>, object: impl Into<String>) {
        self.triples.push((
            subject.into(),
            predicate.into(),
            object.into(),
        ));
    }

    /// Parse RDF triples and extract CodeGenSpec
    pub fn parse_spec(&self, spec_id: &str) -> RdfSpecResult<CodeGenSpec> {
        // Extract basic properties
        let name = self.extract_property(spec_id, "name")?;
        let description = self.extract_property(spec_id, "description").unwrap_or_default();
        let input_language = self.extract_property(spec_id, "inputLanguage")?;
        let output_language = self.extract_property(spec_id, "outputLanguage")?;
        let framework = self.extract_property(spec_id, "framework").ok();

        let complexity_str = self.extract_property(spec_id, "complexityLevel").unwrap_or_else(|_| "Simple".to_string());
        let complexity_level = match complexity_str.as_str() {
            "Intermediate" => ComplexityLevel::Intermediate,
            "Advanced" => ComplexityLevel::Advanced,
            _ => ComplexityLevel::Simple,
        };

        // Extract validation steps
        let validation_steps = self.extract_validation_steps(spec_id)?;

        let mut metadata = HashMap::new();
        for (subj, pred, obj) in &self.triples {
            if subj == spec_id && pred.contains("meta") {
                metadata.insert(pred.clone(), obj.clone());
            }
        }

        Ok(CodeGenSpec {
            id: spec_id.to_string(),
            name,
            description,
            input_language,
            output_language,
            framework,
            complexity_level,
            validation_steps,
            metadata,
        })
    }

    fn extract_property(&self, subject: &str, predicate: &str) -> RdfSpecResult<String> {
        self.triples
            .iter()
            .find(|(subj, pred, _)| subj == subject && pred.contains(predicate))
            .map(|(_, _, obj)| obj.clone())
            .ok_or_else(|| RdfSpecError::EntityNotFound(format!("{} for {}", predicate, subject)))
    }

    fn extract_validation_steps(&self, spec_id: &str) -> RdfSpecResult<Vec<ValidationRule>> {
        let mut steps = Vec::new();

        // Look for validation rules
        let validation_subjects: Vec<String> = self.triples
            .iter()
            .filter(|(subj, pred, _)| subj == spec_id && pred.contains("validationStep"))
            .map(|(_, _, obj)| obj.clone())
            .collect();

        for val_id in validation_subjects {
            if let Ok(rule_type_str) = self.extract_property(&val_id, "type") {
                let rule_type = match rule_type_str.as_str() {
                    "Syntax" => ValidationType::Syntax,
                    "Format" => ValidationType::Format,
                    "Compile" => ValidationType::Compile,
                    "Test" => ValidationType::Test,
                    "Security" => ValidationType::Security,
                    _ => ValidationType::Syntax,
                };

                let enabled = self.extract_property(&val_id, "enabled")
                    .ok()
                    .map(|v| v.to_lowercase() == "true")
                    .unwrap_or(true);

                steps.push(ValidationRule {
                    rule_type,
                    enabled,
                    config: HashMap::new(),
                });
            }
        }

        Ok(steps)
    }
}

impl Default for RdfSpecParser {
    fn default() -> Self {
        Self::new()
    }
}

// ============================================================================
// TESTS
// ============================================================================

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_rdf_spec_parser_creation() {
        let parser = RdfSpecParser::new();
        assert_eq!(parser.triples.len(), 0);
    }

    #[test]
    fn test_add_triple() {
        let mut parser = RdfSpecParser::new();
        parser.add_triple("http://example.com/spec1", "http://example.com/name", "MySpec");
        assert_eq!(parser.triples.len(), 1);
    }

    #[test]
    fn test_parse_spec_basic() {
        let mut parser = RdfSpecParser::new();
        let spec_id = "http://example.com/spec1";

        parser.add_triple(spec_id, "name", "RustWebAPI");
        parser.add_triple(spec_id, "description", "A web API specification");
        parser.add_triple(spec_id, "inputLanguage", "RDF");
        parser.add_triple(spec_id, "outputLanguage", "Rust");
        parser.add_triple(spec_id, "framework", "Actix");
        parser.add_triple(spec_id, "complexityLevel", "Intermediate");

        let spec = parser.parse_spec(spec_id).unwrap();
        assert_eq!(spec.name, "RustWebAPI");
        assert_eq!(spec.input_language, "RDF");
        assert_eq!(spec.output_language, "Rust");
        assert_eq!(spec.framework, Some("Actix".to_string()));
        assert_eq!(spec.complexity_level, ComplexityLevel::Intermediate);
    }

    #[test]
    fn test_parse_spec_missing_required_field() {
        let parser = RdfSpecParser::new();
        let result = parser.parse_spec("http://example.com/missing");
        assert!(result.is_err());
    }

    #[test]
    fn test_complexity_level_variants() {
        assert_eq!(format!("{:?}", ComplexityLevel::Simple), "Simple");
        assert_eq!(format!("{:?}", ComplexityLevel::Intermediate), "Intermediate");
        assert_eq!(format!("{:?}", ComplexityLevel::Advanced), "Advanced");
    }

    #[test]
    fn test_validation_rule_extraction() {
        let mut parser = RdfSpecParser::new();
        let spec_id = "http://example.com/spec1";
        let val_id = "http://example.com/val1";

        parser.add_triple(spec_id, "name", "TestSpec");
        parser.add_triple(spec_id, "inputLanguage", "RDF");
        parser.add_triple(spec_id, "outputLanguage", "Rust");
        parser.add_triple(spec_id, "validationStep", val_id);
        parser.add_triple(val_id, "type", "Syntax");
        parser.add_triple(val_id, "enabled", "true");

        let spec = parser.parse_spec(spec_id).unwrap();
        assert!(!spec.validation_steps.is_empty());
        assert_eq!(spec.validation_steps[0].rule_type, ValidationType::Syntax);
    }

    #[test]
    fn test_default_complexity_level() {
        let mut parser = RdfSpecParser::new();
        let spec_id = "http://example.com/spec1";

        parser.add_triple(spec_id, "name", "SimpleSpec");
        parser.add_triple(spec_id, "inputLanguage", "RDF");
        parser.add_triple(spec_id, "outputLanguage", "Python");

        let spec = parser.parse_spec(spec_id).unwrap();
        assert_eq!(spec.complexity_level, ComplexityLevel::Simple);
    }

    #[test]
    fn test_spec_with_metadata() {
        let mut parser = RdfSpecParser::new();
        let spec_id = "http://example.com/spec1";

        parser.add_triple(spec_id, "name", "MetaSpec");
        parser.add_triple(spec_id, "inputLanguage", "RDF");
        parser.add_triple(spec_id, "outputLanguage", "Rust");
        parser.add_triple(spec_id, "metadata:author", "Alice");
        parser.add_triple(spec_id, "metadata:version", "1.0.0");

        let spec = parser.parse_spec(spec_id).unwrap();
        assert_eq!(spec.metadata.len(), 2);
    }

    #[test]
    fn test_validation_type_variants() {
        let types = vec![
            ValidationType::Syntax,
            ValidationType::Format,
            ValidationType::Compile,
            ValidationType::Test,
            ValidationType::Security,
        ];
        assert_eq!(types.len(), 5);
    }

    #[test]
    fn test_extract_property_not_found() {
        let parser = RdfSpecParser::new();
        let result = parser.extract_property("http://example.com/missing", "name");
        assert!(result.is_err());
    }

    #[test]
    fn test_spec_serialization() {
        let spec = CodeGenSpec {
            id: "spec1".to_string(),
            name: "TestSpec".to_string(),
            description: "A test specification".to_string(),
            input_language: "RDF".to_string(),
            output_language: "Rust".to_string(),
            framework: Some("Actix".to_string()),
            complexity_level: ComplexityLevel::Intermediate,
            validation_steps: vec![],
            metadata: HashMap::new(),
        };

        let json = serde_json::to_string(&spec).unwrap();
        assert!(json.contains("TestSpec"));
        assert!(json.contains("Rust"));
    }

    #[test]
    fn test_multiple_validation_steps() {
        let mut parser = RdfSpecParser::new();
        let spec_id = "http://example.com/spec1";

        parser.add_triple(spec_id, "name", "ComplexSpec");
        parser.add_triple(spec_id, "inputLanguage", "RDF");
        parser.add_triple(spec_id, "outputLanguage", "Rust");
        parser.add_triple(spec_id, "validationStep", "http://example.com/val1");
        parser.add_triple(spec_id, "validationStep", "http://example.com/val2");
        parser.add_triple("http://example.com/val1", "type", "Syntax");
        parser.add_triple("http://example.com/val2", "type", "Compile");

        let spec = parser.parse_spec(spec_id).unwrap();
        assert_eq!(spec.validation_steps.len(), 2);
    }

    #[test]
    fn test_validation_rule_serialization() {
        let rule = ValidationRule {
            rule_type: ValidationType::Compile,
            enabled: true,
            config: HashMap::new(),
        };

        let json = serde_json::to_string(&rule).unwrap();
        assert!(json.contains("Compile"));
        assert!(json.contains("true"));
    }

    #[test]
    fn test_extract_validation_steps_empty() {
        let mut parser = RdfSpecParser::new();
        let spec_id = "http://example.com/spec1";

        parser.add_triple(spec_id, "name", "NoValidation");
        parser.add_triple(spec_id, "inputLanguage", "RDF");
        parser.add_triple(spec_id, "outputLanguage", "Python");

        let spec = parser.parse_spec(spec_id).unwrap();
        assert!(spec.validation_steps.is_empty());
    }
}
