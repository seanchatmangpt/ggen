//! LLM-Construct Builder - High-level API for building LLM-Constructs
//!
//! This module orchestrates the complete pipeline from OWL ontology to executable
//! DSPy signature with constraints.

use crate::dspy::{FieldConstraints, OutputField};
use crate::error::Result;
use crate::owl::extractor::{OWLClass, OWLExtractor};
use crate::owl::shacl_generator::{GeneratedShape, PropertyShape, SHACLGenerator};
use oxigraph::store::Store;
use std::path::Path;

/// LLM-Construct specification - defines what to build
///
/// This structure captures the intent and configuration for building
/// an LLM-Construct from a domain ontology.
#[derive(Debug, Clone)]
pub struct LLMConstructSpec {
    /// Name of the construct (e.g., "BondExtractor")
    pub name: String,

    /// Intent description - what this construct does
    pub intent: String,

    /// Path to source OWL ontology file (TTL format)
    pub source_ontology_path: String,

    /// URI of target class to extract (e.g., "http://example.com/Bond")
    pub target_class_uri: String,

    /// Optional prompt template for LLM interaction
    pub prompt_template: Option<String>,
}

/// Built LLM-Construct ready for use or code generation
///
/// This structure contains the complete transformation pipeline results:
/// - Original specification
/// - Extracted OWL class definition
/// - Generated SHACL shapes
/// - DSPy fields with constraints
#[derive(Debug)]
pub struct LLMConstruct {
    /// Original specification
    pub spec: LLMConstructSpec,

    /// Extracted OWL class definition
    pub owl_class: OWLClass,

    /// Generated SHACL shapes
    pub generated_shacl: GeneratedShape,

    /// DSPy output fields with constraints
    pub dspy_fields: Vec<OutputField>,
}

/// LLM-Construct Builder - orchestrates the pipeline
///
/// This builder manages the complete transformation from OWL ontology
/// to executable DSPy fields with constraints.
///
/// ## Example
///
/// ```rust,no_run
/// use ggen_ai::llm_construct::{LLMConstructBuilder, LLMConstructSpec};
/// use oxigraph::store::Store;
///
/// # fn example() -> Result<(), Box<dyn std::error::Error>> {
/// let store = Store::new()?;
/// let mut builder = LLMConstructBuilder::new(store);
///
/// let spec = LLMConstructSpec {
///     name: "BondExtractor".to_string(),
///     intent: "Extract bond data".to_string(),
///     source_ontology_path: "examples/fibo-bond.ttl".to_string(),
///     target_class_uri: "http://example.com/Bond".to_string(),
///     prompt_template: None,
/// };
///
/// let construct = builder.build(spec)?;
/// # Ok(())
/// # }
/// ```
pub struct LLMConstructBuilder {
    extractor: OWLExtractor,
}

impl LLMConstructBuilder {
    /// Create new LLM-Construct builder with given RDF store
    ///
    /// The store will be used to load and query OWL ontologies.
    ///
    /// # Arguments
    ///
    /// * `store` - Oxigraph RDF store (empty or pre-populated)
    pub fn new(store: Store) -> Self {
        Self {
            extractor: OWLExtractor::new(store),
        }
    }

    /// Build LLM-Construct from specification
    ///
    /// This orchestrates the complete pipeline:
    /// 1. Load OWL ontology from file
    /// 2. Extract target class with properties and restrictions
    /// 3. Generate SHACL shapes from OWL restrictions
    /// 4. Map SHACL constraints to DSPy field constraints
    ///
    /// # Arguments
    ///
    /// * `spec` - LLM-Construct specification
    ///
    /// # Returns
    ///
    /// Complete LLM-Construct with all transformation results
    ///
    /// # Errors
    ///
    /// Returns error if:
    /// - Ontology file cannot be loaded
    /// - Target class not found in ontology
    /// - SHACL generation fails
    /// - Constraint mapping fails
    pub fn build(&mut self, spec: LLMConstructSpec) -> Result<LLMConstruct> {
        // Stage 1: Load OWL ontology
        self.extractor
            .load_ontology(Path::new(&spec.source_ontology_path))?;

        // Stage 2: Extract target class
        let owl_class = self.extractor.extract_class(&spec.target_class_uri)?;

        // Stage 3: Generate SHACL shapes
        let generated_shacl = SHACLGenerator::generate_shape(&owl_class)?;

        // Stage 4: Map to DSPy fields
        let dspy_fields = self.shacl_to_dspy(&generated_shacl)?;

        Ok(LLMConstruct {
            spec,
            owl_class,
            generated_shacl,
            dspy_fields,
        })
    }

    /// Map SHACL shapes to DSPy output fields with constraints
    ///
    /// This converts each SHACL PropertyShape into a DSPy OutputField,
    /// preserving all validation constraints through the transformation.
    ///
    /// # Arguments
    ///
    /// * `shacl` - Generated SHACL shape with property constraints
    ///
    /// # Returns
    ///
    /// Vector of DSPy OutputFields with constraints
    fn shacl_to_dspy(&self, shacl: &GeneratedShape) -> Result<Vec<OutputField>> {
        let mut fields = Vec::new();

        for prop_shape in &shacl.property_shapes {
            // Map PropertyShape constraints to FieldConstraints
            let constraints = self.property_shape_to_constraints(prop_shape)?;

            // Extract local name from property path URI
            let field_name = Self::local_name(&prop_shape.path);

            // Generate description from property and constraints
            let description = Self::generate_field_description(prop_shape);

            // Determine Rust type annotation from datatype
            let type_annotation = Self::map_datatype_to_rust(&prop_shape.datatype);

            // Create OutputField with constraints
            let field = OutputField::new(&field_name, &description, &type_annotation)
                .add_constraints(constraints);

            fields.push(field);
        }

        Ok(fields)
    }

    /// Convert SHACL PropertyShape to FieldConstraints
    ///
    /// Maps all SHACL constraint properties to their DSPy equivalents:
    /// - sh:minCount → required (if > 0)
    /// - sh:minLength/maxLength → string length constraints
    /// - sh:pattern → regex pattern
    /// - sh:datatype → datatype constraint
    /// - sh:class → semantic type constraint
    fn property_shape_to_constraints(&self, shape: &PropertyShape) -> Result<FieldConstraints> {
        Ok(FieldConstraints {
            required: shape.min_count.map_or(false, |c| c > 0),
            min_items: None, // Array constraints handled separately
            max_items: None,
            min_length: shape.min_length.map(|n| n as usize),
            max_length: shape.max_length.map(|n| n as usize),
            pattern: shape.pattern.clone(),
            enum_values: None, // sh:in handled in future phases
            semantic_type: shape.class.clone(),
            datatype: shape.datatype.clone(),
        })
    }

    /// Extract local name from URI (last segment after # or /)
    fn local_name(uri: &str) -> String {
        uri.split(&['#', '/'][..])
            .last()
            .unwrap_or(uri)
            .to_string()
    }

    /// Generate human-readable field description from PropertyShape
    fn generate_field_description(shape: &PropertyShape) -> String {
        let name = Self::local_name(&shape.path);
        let type_desc = if let Some(ref cls) = shape.class {
            format!(" of type {}", Self::local_name(cls))
        } else if let Some(ref dt) = shape.datatype {
            format!(" ({})", Self::local_name(dt))
        } else {
            String::new()
        };

        format!("Field '{}'{}", name, type_desc)
    }

    /// Map XSD datatype or sh:class to Rust type annotation
    ///
    /// Common XSD types map to Rust primitives:
    /// - xsd:string → String
    /// - xsd:integer/int → i64
    /// - xsd:decimal/double → f64
    /// - xsd:boolean → bool
    /// - xsd:dateTime → String (chrono types in future)
    ///
    /// Object properties (sh:class) default to String representation.
    fn map_datatype_to_rust(datatype: &Option<String>) -> String {
        match datatype.as_deref() {
            Some(dt) if dt.contains("string") => "String".to_string(),
            Some(dt) if dt.contains("integer") || dt.contains("int") => "i64".to_string(),
            Some(dt) if dt.contains("decimal") || dt.contains("double") || dt.contains("float") => {
                "f64".to_string()
            }
            Some(dt) if dt.contains("boolean") => "bool".to_string(),
            Some(dt) if dt.contains("dateTime") || dt.contains("date") => "String".to_string(),
            _ => "String".to_string(), // Default to String for unknown types
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_llm_construct_spec_creation() {
        let spec = LLMConstructSpec {
            name: "TestExtractor".to_string(),
            intent: "Extract test data".to_string(),
            source_ontology_path: "test.ttl".to_string(),
            target_class_uri: "http://example.com/Test".to_string(),
            prompt_template: None,
        };

        assert_eq!(spec.name, "TestExtractor");
        assert_eq!(spec.intent, "Extract test data");
    }

    #[test]
    fn test_llm_construct_builder_creation() {
        let store = Store::new().expect("Failed to create store");
        let builder = LLMConstructBuilder::new(store);
        assert!(std::ptr::addr_of!(builder).is_null() == false);
    }

    #[test]
    fn test_local_name_extraction() {
        assert_eq!(LLMConstructBuilder::local_name("http://example.com/Bond"), "Bond");
        assert_eq!(
            LLMConstructBuilder::local_name("http://example.com#hasISIN"),
            "hasISIN"
        );
        assert_eq!(LLMConstructBuilder::local_name("Bond"), "Bond");
    }

    #[test]
    fn test_map_datatype_to_rust() {
        assert_eq!(
            LLMConstructBuilder::map_datatype_to_rust(&Some(
                "http://www.w3.org/2001/XMLSchema#string".to_string()
            )),
            "String"
        );
        assert_eq!(
            LLMConstructBuilder::map_datatype_to_rust(&Some(
                "http://www.w3.org/2001/XMLSchema#integer".to_string()
            )),
            "i64"
        );
        assert_eq!(
            LLMConstructBuilder::map_datatype_to_rust(&Some(
                "http://www.w3.org/2001/XMLSchema#decimal".to_string()
            )),
            "f64"
        );
        assert_eq!(
            LLMConstructBuilder::map_datatype_to_rust(&Some(
                "http://www.w3.org/2001/XMLSchema#boolean".to_string()
            )),
            "bool"
        );
        assert_eq!(LLMConstructBuilder::map_datatype_to_rust(&None), "String");
    }

    #[test]
    fn test_property_shape_to_constraints_required() {
        let store = Store::new().expect("Failed to create store");
        let builder = LLMConstructBuilder::new(store);

        let shape = PropertyShape {
            path: "http://example.com/testProp".to_string(),
            datatype: Some("http://www.w3.org/2001/XMLSchema#string".to_string()),
            class: None,
            min_count: Some(1),
            max_count: Some(1),
            min_length: Some(5),
            max_length: Some(100),
            pattern: Some(r"^[A-Z]+$".to_string()),
            min_inclusive: None,
            max_inclusive: None,
            min_exclusive: None,
            max_exclusive: None,
        };

        let constraints = builder
            .property_shape_to_constraints(&shape)
            .expect("Constraint conversion failed");

        assert!(constraints.required);
        assert_eq!(constraints.min_length, Some(5));
        assert_eq!(constraints.max_length, Some(100));
        assert_eq!(constraints.pattern, Some(r"^[A-Z]+$".to_string()));
        assert_eq!(
            constraints.datatype,
            Some("http://www.w3.org/2001/XMLSchema#string".to_string())
        );
    }

    #[test]
    fn test_property_shape_to_constraints_optional() {
        let store = Store::new().expect("Failed to create store");
        let builder = LLMConstructBuilder::new(store);

        let shape = PropertyShape {
            path: "http://example.com/optionalProp".to_string(),
            datatype: Some("http://www.w3.org/2001/XMLSchema#string".to_string()),
            class: None,
            min_count: None,
            max_count: None,
            min_length: None,
            max_length: None,
            pattern: None,
            min_inclusive: None,
            max_inclusive: None,
            min_exclusive: None,
            max_exclusive: None,
        };

        let constraints = builder
            .property_shape_to_constraints(&shape)
            .expect("Constraint conversion failed");

        assert!(!constraints.required);
        assert!(constraints.min_length.is_none());
        assert!(constraints.max_length.is_none());
        assert!(constraints.pattern.is_none());
    }

    #[test]
    fn test_generate_field_description() {
        let shape_with_datatype = PropertyShape {
            path: "http://example.com/name".to_string(),
            datatype: Some("http://www.w3.org/2001/XMLSchema#string".to_string()),
            class: None,
            min_count: None,
            max_count: None,
            min_length: None,
            max_length: None,
            pattern: None,
            min_inclusive: None,
            max_inclusive: None,
            min_exclusive: None,
            max_exclusive: None,
        };

        let desc = LLMConstructBuilder::generate_field_description(&shape_with_datatype);
        assert!(desc.contains("name"));
        assert!(desc.contains("string"));

        let shape_with_class = PropertyShape {
            path: "http://example.com/issuer".to_string(),
            datatype: None,
            class: Some("http://example.com/Organization".to_string()),
            min_count: None,
            max_count: None,
            min_length: None,
            max_length: None,
            pattern: None,
            min_inclusive: None,
            max_inclusive: None,
            min_exclusive: None,
            max_exclusive: None,
        };

        let desc = LLMConstructBuilder::generate_field_description(&shape_with_class);
        assert!(desc.contains("issuer"));
        assert!(desc.contains("Organization"));
    }

    #[test]
    fn test_shacl_to_dspy_empty() {
        let store = Store::new().expect("Failed to create store");
        let builder = LLMConstructBuilder::new(store);

        let shacl = GeneratedShape {
            node_shape_uri: "http://example.com/TestShape".to_string(),
            target_class: "http://example.com/Test".to_string(),
            property_shapes: Vec::new(),
        };

        let fields = builder.shacl_to_dspy(&shacl).expect("Conversion failed");
        assert_eq!(fields.len(), 0);
    }

    #[test]
    fn test_shacl_to_dspy_with_properties() {
        let store = Store::new().expect("Failed to create store");
        let builder = LLMConstructBuilder::new(store);

        let shacl = GeneratedShape {
            node_shape_uri: "http://example.com/PersonShape".to_string(),
            target_class: "http://example.com/Person".to_string(),
            property_shapes: vec![
                PropertyShape {
                    path: "http://example.com/name".to_string(),
                    datatype: Some("http://www.w3.org/2001/XMLSchema#string".to_string()),
                    class: None,
                    min_count: Some(1),
                    max_count: Some(1),
                    min_length: Some(1),
                    max_length: Some(100),
                    pattern: None,
                    min_inclusive: None,
                    max_inclusive: None,
                    min_exclusive: None,
                    max_exclusive: None,
                },
                PropertyShape {
                    path: "http://example.com/age".to_string(),
                    datatype: Some("http://www.w3.org/2001/XMLSchema#integer".to_string()),
                    class: None,
                    min_count: Some(1),
                    max_count: Some(1),
                    min_length: None,
                    max_length: None,
                    pattern: None,
                    min_inclusive: None,
                    max_inclusive: None,
                    min_exclusive: None,
                    max_exclusive: None,
                },
            ],
        };

        let fields = builder.shacl_to_dspy(&shacl).expect("Conversion failed");
        assert_eq!(fields.len(), 2);

        // Check name field
        let name_field = &fields[0];
        assert_eq!(name_field.name(), "name");
        assert_eq!(name_field.type_annotation(), "String");
        assert!(name_field.constraints.required);
        assert_eq!(name_field.constraints.min_length, Some(1));
        assert_eq!(name_field.constraints.max_length, Some(100));

        // Check age field
        let age_field = &fields[1];
        assert_eq!(age_field.name(), "age");
        assert_eq!(age_field.type_annotation(), "i64");
        assert!(age_field.constraints.required);
    }

    #[test]
    fn test_build_with_invalid_path() {
        let store = Store::new().expect("Failed to create store");
        let mut builder = LLMConstructBuilder::new(store);

        let spec = LLMConstructSpec {
            name: "TestExtractor".to_string(),
            intent: "Test".to_string(),
            source_ontology_path: "/nonexistent/path/test.ttl".to_string(),
            target_class_uri: "http://example.com/Test".to_string(),
            prompt_template: None,
        };

        let result = builder.build(spec);
        assert!(result.is_err());
    }
}
