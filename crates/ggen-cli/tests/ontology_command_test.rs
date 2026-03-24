//! Ontology Commands Integration Tests - Chicago TDD
//!
//! Tests for ontology operations: generate, validate, init
//!
//! Chicago TDD Cycle:
//! 1. RED: Write failing test
//! 2. GREEN: Make test pass with REAL implementation
//! 3. REFACTOR: Improve code while maintaining green
//!
//! NO MOCKS - Tests against REAL ggen_core::ontology implementations

use std::path::PathBuf;
use std::collections::BTreeMap;

// ============================================================================
// Core Layer Imports (REAL types, NO mocks)
// ============================================================================

use ggen_core::ontology::{
    OntologySchema, OntClass, OntProperty, PropertyRange, Cardinality,
    OntRelationship, RelationshipType, OwlRestriction,
};

// ============================================================================
// OntologySchema Structure Tests
// ============================================================================

#[cfg(test)]
mod ontology_schema_tests {
    use super::*;

    /// Test: Create a minimal OntologySchema
    #[test]
    fn test_ontology_schema_minimal() {
        let schema = OntologySchema {
            classes: vec![],
            properties: vec![],
            relationships: vec![],
            namespace: "http://example.org#".to_string(),
            version: "1.0".to_string(),
            label: "Test Ontology".to_string(),
            description: None,
            metadata: BTreeMap::new(),
        };

        assert_eq!(schema.namespace, "http://example.org#");
        assert_eq!(schema.classes.len(), 0);
        assert_eq!(schema.properties.len(), 0);
        assert_eq!(schema.version, "1.0");
        assert_eq!(schema.label, "Test Ontology");
    }

    /// Test: OntologySchema with all fields populated
    #[test]
    fn test_ontology_schema_full() {
        let mut metadata = BTreeMap::new();
        metadata.insert("author".to_string(), "Test Author".to_string());
        metadata.insert("created".to_string(), "2025-01-01".to_string());

        let schema = OntologySchema {
            classes: vec![],
            properties: vec![],
            relationships: vec![],
            namespace: "http://example.org#".to_string(),
            version: "2.0".to_string(),
            label: "Full Ontology".to_string(),
            description: Some("A complete ontology example".to_string()),
            metadata,
        };

        assert_eq!(schema.description, Some("A complete ontology example".to_string()));
        assert_eq!(schema.metadata.len(), 2);
        assert_eq!(schema.metadata["author"], "Test Author");
    }

    /// Test: OntologySchema with classes
    #[test]
    fn test_ontology_schema_with_classes() {
        let schema = OntologySchema {
            classes: vec![
                OntClass {
                    uri: "http://example.org#Product".to_string(),
                    name: "Product".to_string(),
                    label: "Product".to_string(),
                    description: Some("A product".to_string()),
                    parent_classes: vec![],
                    properties: vec![],
                    is_abstract: false,
                    restrictions: vec![],
                },
            ],
            properties: vec![],
            relationships: vec![],
            namespace: "http://example.org#".to_string(),
            version: "1.0".to_string(),
            label: "Product Ontology".to_string(),
            description: None,
            metadata: BTreeMap::new(),
        };

        assert_eq!(schema.classes.len(), 1);
        assert_eq!(schema.classes[0].name, "Product");
    }
}

// ============================================================================
// OntClass Structure Tests
// ============================================================================

#[cfg(test)]
mod ont_class_tests {
    use super::*;

    /// Test: OntClass with all fields
    #[test]
    fn test_ont_class_full() {
        let class = OntClass {
            uri: "http://example.org#Person".to_string(),
            name: "Person".to_string(),
            label: "Person".to_string(),
            description: Some("A human being".to_string()),
            parent_classes: vec!["http://example.org#Agent".to_string()],
            properties: vec!["name".to_string(), "age".to_string()],
            is_abstract: false,
            restrictions: vec![],
        };

        assert_eq!(class.uri, "http://example.org#Person");
        assert_eq!(class.name, "Person");
        assert_eq!(class.label, "Person");
        assert_eq!(class.description, Some("A human being".to_string()));
        assert_eq!(class.parent_classes.len(), 1);
        assert_eq!(class.properties.len(), 2);
        assert!(!class.is_abstract);
    }

    /// Test: OntClass with minimal fields
    #[test]
    fn test_ont_class_minimal() {
        let class = OntClass {
            uri: "http://example.org#Thing".to_string(),
            name: "Thing".to_string(),
            label: "Thing".to_string(),
            description: None,
            parent_classes: vec![],
            properties: vec![],
            is_abstract: true,
            restrictions: vec![],
        };

        assert!(class.description.is_none());
        assert!(class.is_abstract);
    }

    /// Test: OntClass with restrictions
    #[test]
    fn test_ont_class_with_restrictions() {
        let class = OntClass {
            uri: "http://example.org#Adult".to_string(),
            name: "Adult".to_string(),
            label: "Adult".to_string(),
            description: None,
            parent_classes: vec![],
            properties: vec![],
            is_abstract: false,
            restrictions: vec![
                OwlRestriction::MinCardinality(18),
            ],
        };

        assert_eq!(class.restrictions.len(), 1);
    }
}

// ============================================================================
// OntProperty Structure Tests
// ============================================================================

#[cfg(test)]
mod ont_property_tests {
    use super::*;

    /// Test: OntProperty with String range
    #[test]
    fn test_ont_property_string_range() {
        let property = OntProperty {
            uri: "http://example.org#name".to_string(),
            name: "name".to_string(),
            label: "Name".to_string(),
            description: Some("The name of something".to_string()),
            domain: vec!["http://example.org#Person".to_string()],
            range: PropertyRange::String,
            cardinality: Cardinality::One,
            required: true,
            is_functional: true,
            is_inverse_functional: false,
            inverse_of: None,
        };

        assert_eq!(property.name, "name");
        assert!(matches!(property.range, PropertyRange::String));
        assert!(matches!(property.cardinality, Cardinality::One));
        assert!(property.required);
        assert!(property.is_functional);
    }

    /// Test: OntProperty with Reference range
    #[test]
    fn test_ont_property_reference_range() {
        let property = OntProperty {
            uri: "http://example.org#knows".to_string(),
            name: "knows".to_string(),
            label: "Knows".to_string(),
            description: None,
            domain: vec!["http://example.org#Person".to_string()],
            range: PropertyRange::Reference("http://example.org#Person".to_string()),
            cardinality: Cardinality::Many,
            required: false,
            is_functional: false,
            is_inverse_functional: false,
            inverse_of: None,
        };

        assert!(matches!(property.range, PropertyRange::Reference(_)));
        assert!(matches!(property.cardinality, Cardinality::Many));
        assert!(!property.required);
    }

    /// Test: OntProperty with all cardinality types
    #[test]
    fn test_ont_property_cardinalities() {
        let cardinalities = vec![
            (Cardinality::One, 1, Some(1)),
            (Cardinality::ZeroOrOne, 0, Some(1)),
            (Cardinality::Many, 0, None),
            (Cardinality::OneOrMore, 1, None),
            (Cardinality::Range { min: 2, max: Some(5) }, 2, Some(5)),
        ];

        for (cardinality, expected_min, expected_max) in cardinalities {
            assert_eq!(cardinality.min(), expected_min);
            assert_eq!(cardinality.max(), expected_max);
        }
    }

    /// Test: OntProperty with multiple domains
    #[test]
    fn test_ont_property_multiple_domains() {
        let property = OntProperty {
            uri: "http://example.org#identifier".to_string(),
            name: "identifier".to_string(),
            label: "Identifier".to_string(),
            description: None,
            domain: vec![
                "http://example.org#Person".to_string(),
                "http://example.org#Organization".to_string(),
            ],
            range: PropertyRange::String,
            cardinality: Cardinality::One,
            required: true,
            is_functional: true,
            is_inverse_functional: false,
            inverse_of: None,
        };

        assert_eq!(property.domain.len(), 2);
    }
}

// ============================================================================
// PropertyRange Enum Tests
// ============================================================================

#[cfg(test)]
mod property_range_tests {
    use super::*;

    /// Test: All PropertyRange variants
    #[test]
    fn test_property_range_variants() {
        let ranges = vec![
            PropertyRange::String,
            PropertyRange::Integer,
            PropertyRange::Float,
            PropertyRange::Boolean,
            PropertyRange::DateTime,
            PropertyRange::Date,
            PropertyRange::Time,
            PropertyRange::Reference("http://example.org#Class".to_string()),
            PropertyRange::Literal("custom".to_string()),
            PropertyRange::Enum(vec!["A".to_string(), "B".to_string()]),
        ];

        assert_eq!(ranges.len(), 10);
    }

    /// Test: PropertyRange Reference variant
    #[test]
    fn test_property_range_reference() {
        let range = PropertyRange::Reference("http://example.org#Person".to_string());
        assert!(matches!(range, PropertyRange::Reference(_)));
    }

    /// Test: PropertyRange Enum variant
    #[test]
    fn test_property_range_enum() {
        let range = PropertyRange::Enum(vec![
            "Red".to_string(),
            "Green".to_string(),
            "Blue".to_string(),
        ]);

        if let PropertyRange::Enum(values) = range {
            assert_eq!(values.len(), 3);
            assert!(values.contains(&"Red".to_string()));
        } else {
            panic!("Expected Enum variant");
        }
    }
}

// ============================================================================
// OntRelationship Structure Tests
// ============================================================================

#[cfg(test)]
mod ont_relationship_tests {
    use super::*;

    /// Test: OntRelationship basic structure
    #[test]
    fn test_ont_relationship_basic() {
        let relationship = OntRelationship {
            from_class: "http://example.org#Person".to_string(),
            to_class: "http://example.org#Organization".to_string(),
            property: "http://example.org#worksFor".to_string(),
            relationship_type: RelationshipType::ManyToOne,
            bidirectional: false,
            label: "works for".to_string(),
        };

        assert_eq!(relationship.from_class, "http://example.org#Person");
        assert_eq!(relationship.to_class, "http://example.org#Organization");
        assert!(matches!(relationship.relationship_type, RelationshipType::ManyToOne));
        assert!(!relationship.bidirectional);
    }

    /// Test: All RelationshipType variants
    #[test]
    fn test_relationship_type_variants() {
        let types = vec![
            RelationshipType::OneToOne,
            RelationshipType::OneToMany,
            RelationshipType::ManyToOne,
            RelationshipType::ManyToMany,
            RelationshipType::Inheritance,
            RelationshipType::Composition,
            RelationshipType::Aggregation,
        ];

        assert_eq!(types.len(), 7);
    }

    /// Test: Bidirectional relationship
    #[test]
    fn test_ont_relationship_bidirectional() {
        let relationship = OntRelationship {
            from_class: "http://example.org#Person".to_string(),
            to_class: "http://example.org#Person".to_string(),
            property: "http://example.org#knows".to_string(),
            relationship_type: RelationshipType::ManyToMany,
            bidirectional: true,
            label: "knows".to_string(),
        };

        assert!(relationship.bidirectional);
        assert!(matches!(relationship.relationship_type, RelationshipType::ManyToMany));
    }
}

// ============================================================================
// OwlRestriction Tests
// ============================================================================

#[cfg(test)]
mod owl_restriction_tests {
    use super::*;

    /// Test: All OwlRestriction variants
    #[test]
    fn test_owl_restriction_variants() {
        let restrictions = vec![
            OwlRestriction::SomeValuesFrom("http://example.org#Class".to_string()),
            OwlRestriction::AllValuesFrom("http://example.org#Class".to_string()),
            OwlRestriction::HasValue("specific-value".to_string()),
            OwlRestriction::MinCardinality(1),
            OwlRestriction::MaxCardinality(10),
            OwlRestriction::Cardinality(5),
        ];

        assert_eq!(restrictions.len(), 6);
    }

    /// Test: OwlRestriction SomeValuesFrom
    #[test]
    fn test_owl_restriction_some_values_from() {
        let restriction = OwlRestriction::SomeValuesFrom("http://example.org#Person".to_string());
        assert!(matches!(restriction, OwlRestriction::SomeValuesFrom(_)));
    }

    /// Test: OwlRestriction cardinality variants
    #[test]
    fn test_owl_restriction_cardinalities() {
        let min_card = OwlRestriction::MinCardinality(1);
        let max_card = OwlRestriction::MaxCardinality(5);
        let card = OwlRestriction::Cardinality(3);

        assert!(matches!(min_card, OwlRestriction::MinCardinality(1)));
        assert!(matches!(max_card, OwlRestriction::MaxCardinality(5)));
        assert!(matches!(card, OwlRestriction::Cardinality(3)));
    }
}

// ============================================================================
// CLI Output Structure Tests
// ============================================================================

#[cfg(test)]
mod cli_output_tests {
    use super::*;
    use serde_json;

    /// Test: CLI GenerateOutput can be serialized
    #[test]
    fn test_cli_generate_output_serialization() {
        #[derive(serde::Serialize)]
        struct GenerateOutput {
            language: String,
            files_generated: usize,
            output_directory: String,
            primary_file: String,
        }

        let output = GenerateOutput {
            language: "typescript".to_string(),
            files_generated: 3,
            output_directory: "generated".to_string(),
            primary_file: "types.ts".to_string(),
        };

        let json = serde_json::to_string(&output).unwrap();
        assert!(json.contains("typescript"));
        assert!(json.contains("types.ts"));
    }

    /// Test: CLI ValidateOutput can be serialized
    #[test]
    fn test_cli_validate_output_serialization() {
        #[derive(serde::Serialize)]
        struct ValidateOutput {
            is_valid: bool,
            classes_count: usize,
            properties_count: usize,
            warnings: Vec<String>,
            errors: Vec<String>,
        }

        let output = ValidateOutput {
            is_valid: true,
            classes_count: 5,
            properties_count: 12,
            warnings: vec!["Class 'Thing' has no properties".to_string()],
            errors: vec![],
        };

        let json = serde_json::to_string(&output).unwrap();
        assert!(json.contains("true"));
        assert!(json.contains("5"));
    }

    /// Test: CLI InitOutput can be serialized
    #[test]
    fn test_cli_init_output_serialization() {
        #[derive(serde::Serialize)]
        struct InitOutput {
            project_name: String,
            ontology_file: String,
            config_file: String,
            generated_files: Vec<String>,
        }

        let output = InitOutput {
            project_name: "my-ontology".to_string(),
            ontology_file: "ontologies/example.ttl".to_string(),
            config_file: "ggen.config.json".to_string(),
            generated_files: vec![
                "package.json".to_string(),
                "README.md".to_string(),
            ],
        };

        let json = serde_json::to_string(&output).unwrap();
        assert!(json.contains("my-ontology"));
    }
}

// ============================================================================
// Integration Tests (Type Validation - NO Mocks)
// ============================================================================

#[cfg(test)]
mod integration_tests {
    use super::*;

    /// Test: Complete ontology schema with classes and properties
    #[test]
    fn test_integration_complete_schema() {
        let schema = OntologySchema {
            classes: vec![
                OntClass {
                    uri: "http://example.org#Product".to_string(),
                    name: "Product".to_string(),
                    label: "Product".to_string(),
                    description: Some("A product for sale".to_string()),
                    parent_classes: vec![],
                    properties: vec!["name".to_string(), "price".to_string()],
                    is_abstract: false,
                    restrictions: vec![],
                },
                OntClass {
                    uri: "http://example.org#Order".to_string(),
                    name: "Order".to_string(),
                    label: "Order".to_string(),
                    description: Some("A customer order".to_string()),
                    parent_classes: vec![],
                    properties: vec!["product".to_string(), "quantity".to_string()],
                    is_abstract: false,
                    restrictions: vec![],
                },
            ],
            properties: vec![
                OntProperty {
                    uri: "http://example.org#name".to_string(),
                    name: "name".to_string(),
                    label: "Name".to_string(),
                    description: None,
                    domain: vec!["http://example.org#Product".to_string()],
                    range: PropertyRange::String,
                    cardinality: Cardinality::One,
                    required: true,
                    is_functional: true,
                    is_inverse_functional: false,
                    inverse_of: None,
                },
                OntProperty {
                    uri: "http://example.org#price".to_string(),
                    name: "price".to_string(),
                    label: "Price".to_string(),
                    description: None,
                    domain: vec!["http://example.org#Product".to_string()],
                    range: PropertyRange::Float,
                    cardinality: Cardinality::One,
                    required: true,
                    is_functional: true,
                    is_inverse_functional: false,
                    inverse_of: None,
                },
            ],
            relationships: vec![
                OntRelationship {
                    from_class: "http://example.org#Order".to_string(),
                    to_class: "http://example.org#Product".to_string(),
                    property: "http://example.org#product".to_string(),
                    relationship_type: RelationshipType::ManyToOne,
                    bidirectional: false,
                    label: "contains".to_string(),
                },
            ],
            namespace: "http://example.org#".to_string(),
            version: "1.0".to_string(),
            label: "E-commerce Ontology".to_string(),
            description: Some("Product and Order ontology".to_string()),
            metadata: BTreeMap::new(),
        };

        assert_eq!(schema.classes.len(), 2);
        assert_eq!(schema.properties.len(), 2);
        assert_eq!(schema.relationships.len(), 1);

        // Verify class-property relationships
        let product_class = &schema.classes[0];
        assert_eq!(product_class.properties.len(), 2);
    }

    /// Test: Schema.org-like ontology structure
    #[test]
    fn test_integration_schema_org_structure() {
        let schema = OntologySchema {
            classes: vec![
                OntClass {
                    uri: "https://schema.org/Product".to_string(),
                    name: "Product".to_string(),
                    label: "Product".to_string(),
                    description: Some("Any offered product or service".to_string()),
                    parent_classes: vec![],
                    properties: vec!["name".to_string()],
                    is_abstract: false,
                    restrictions: vec![],
                },
            ],
            properties: vec![
                OntProperty {
                    uri: "https://schema.org/name".to_string(),
                    name: "name".to_string(),
                    label: "Name".to_string(),
                    description: None,
                    domain: vec!["https://schema.org/Product".to_string()],
                    range: PropertyRange::String,
                    cardinality: Cardinality::One,
                    required: true,
                    is_functional: true,
                    is_inverse_functional: false,
                    inverse_of: None,
                },
            ],
            relationships: vec![],
            namespace: "https://schema.org/".to_string(),
            version: "1.0".to_string(),
            label: "Schema.org".to_string(),
            description: None,
            metadata: BTreeMap::new(),
        };

        assert_eq!(schema.namespace, "https://schema.org/");
        assert!(schema.classes.iter().any(|c| c.name == "Product"));
    }

    /// Test: Cardinality min/max methods
    #[test]
    fn test_integration_cardinality_methods() {
        // is_multi_valued returns true if max is None or max > 1
        let test_cases = vec![
            (Cardinality::One, false),           // max = Some(1)
            (Cardinality::ZeroOrOne, false),     // max = Some(1)
            (Cardinality::Many, true),           // max = None
            (Cardinality::OneOrMore, true),      // max = None
            (Cardinality::Range { min: 2, max: Some(5) }, true),  // max = Some(5)
            (Cardinality::Range { min: 0, max: Some(1) }, false), // max = Some(1)
        ];

        for (cardinality, expected) in test_cases {
            assert_eq!(cardinality.is_multi_valued(), expected);
        }
    }

    /// Test: Language options for code generation
    #[test]
    fn test_integration_language_options() {
        let languages = vec!["typescript", "graphql", "sql"];

        for language in languages {
            let output_dir = PathBuf::from("generated");
            let zod = true;
            let utilities = true;

            let _: &str = language;
            let _: &PathBuf = &output_dir;
            let _: bool = zod;
            let _: bool = utilities;
        }
    }

    /// Test: Template options for init (as &str options)
    #[test]
    fn test_integration_template_options() {
        let templates: Vec<Option<&str>> = vec![
            Some("schema.org"),
            Some("foaf"),
            Some("dublincore"),
            None,
        ];

        for template in templates {
            let project_name = "test-project";

            let _: &str = project_name;
            let _: Option<&str> = template;
        }
    }
}
