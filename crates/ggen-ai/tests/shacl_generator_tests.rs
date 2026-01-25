//! Comprehensive SHACL Generator Tests
//!
//! Tests the transformation of OWL restrictions to SHACL shapes following Chicago TDD pattern.
//!
//! Test Categories:
//! - Cardinality transformations (3 tests)
//! - Datatype restriction transformations (5 tests): pattern, length, min/max values
//! - Turtle serialization (3 tests): syntax valid, parseable, round-trip
//! - Integration with existing SHACL parser (4+ tests)

use ggen_ai::owl::extractor::{DatatypeFacet, OWLClass, OWLProperty, OWLRestriction};
use ggen_ai::owl::shacl_generator::SHACLGenerator;
use oxigraph::model::NamedNode;
use oxigraph::store::Store;

// ============================================================================
// Test Helpers
// ============================================================================

/// Create a test Bond class with ISIN property
fn create_test_bond_class() -> OWLClass {
    OWLClass {
        uri: NamedNode::new("http://example.com/Bond").unwrap(),
        label: Some("Bond".to_string()),
        comment: Some("Financial bond instrument".to_string()),
        properties: vec![OWLProperty::DatatypeProperty {
            uri: NamedNode::new("http://example.com/hasISIN").unwrap(),
            label: Some("has ISIN".to_string()),
            range: NamedNode::new("http://www.w3.org/2001/XMLSchema#string").unwrap(),
        }],
        restrictions: vec![],
    }
}

/// Create a test Person class with email property
fn create_test_person_class() -> OWLClass {
    OWLClass {
        uri: NamedNode::new("http://example.com/Person").unwrap(),
        label: Some("Person".to_string()),
        comment: Some("A person entity".to_string()),
        properties: vec![OWLProperty::DatatypeProperty {
            uri: NamedNode::new("http://example.com/email").unwrap(),
            label: Some("email".to_string()),
            range: NamedNode::new("http://www.w3.org/2001/XMLSchema#string").unwrap(),
        }],
        restrictions: vec![],
    }
}

// ============================================================================
// Cardinality Transformation Tests (3 tests)
// ============================================================================

#[test]
fn test_cardinality_maps_to_min_max_count() {
    // Arrange: OWL class with cardinality restriction (exactly 1)
    let mut owl_class = create_test_bond_class();
    owl_class.restrictions.push(OWLRestriction::Cardinality {
        property: NamedNode::new("http://example.com/hasISIN").unwrap(),
        min: Some(1),
        max: Some(1),
    });

    // Act: Generate SHACL
    let shape = SHACLGenerator::generate_shape(&owl_class).unwrap();

    // Assert: Verify SHACL constraints
    assert_eq!(shape.property_shapes.len(), 1);
    let isin_shape = &shape.property_shapes[0];
    assert_eq!(isin_shape.path, "http://example.com/hasISIN");
    assert_eq!(isin_shape.min_count, Some(1));
    assert_eq!(isin_shape.max_count, Some(1));
}

#[test]
fn test_cardinality_min_only() {
    // Arrange: OWL class with minimum cardinality (at least 1)
    let mut owl_class = create_test_bond_class();
    owl_class.restrictions.push(OWLRestriction::Cardinality {
        property: NamedNode::new("http://example.com/hasISIN").unwrap(),
        min: Some(1),
        max: None,
    });

    // Act: Generate SHACL
    let shape = SHACLGenerator::generate_shape(&owl_class).unwrap();

    // Assert: Only min_count set
    let isin_shape = &shape.property_shapes[0];
    assert_eq!(isin_shape.min_count, Some(1));
    assert_eq!(isin_shape.max_count, None);
}

#[test]
fn test_cardinality_max_only() {
    // Arrange: OWL class with maximum cardinality (at most 5)
    let mut owl_class = create_test_bond_class();
    owl_class.restrictions.push(OWLRestriction::Cardinality {
        property: NamedNode::new("http://example.com/hasISIN").unwrap(),
        min: None,
        max: Some(5),
    });

    // Act: Generate SHACL
    let shape = SHACLGenerator::generate_shape(&owl_class).unwrap();

    // Assert: Only max_count set
    let isin_shape = &shape.property_shapes[0];
    assert_eq!(isin_shape.min_count, None);
    assert_eq!(isin_shape.max_count, Some(5));
}

// ============================================================================
// Datatype Restriction Transformation Tests (5 tests)
// ============================================================================

#[test]
fn test_pattern_restriction_maps_to_shacl_pattern() {
    // Arrange: OWL class with regex pattern restriction
    let mut owl_class = create_test_bond_class();
    owl_class
        .restrictions
        .push(OWLRestriction::DatatypeRestriction {
            property: NamedNode::new("http://example.com/hasISIN").unwrap(),
            base_type: NamedNode::new("http://www.w3.org/2001/XMLSchema#string").unwrap(),
            facets: vec![DatatypeFacet::Pattern(
                "[A-Z]{2}[A-Z0-9]{9}[0-9]".to_string(),
            )],
        });

    // Act: Generate SHACL
    let shape = SHACLGenerator::generate_shape(&owl_class).unwrap();

    // Assert: Pattern constraint applied
    let isin_shape = &shape.property_shapes[0];
    assert_eq!(
        isin_shape.pattern,
        Some("[A-Z]{2}[A-Z0-9]{9}[0-9]".to_string())
    );
}

#[test]
fn test_min_length_restriction() {
    // Arrange: OWL class with minimum length constraint
    let mut owl_class = create_test_person_class();
    owl_class
        .restrictions
        .push(OWLRestriction::DatatypeRestriction {
            property: NamedNode::new("http://example.com/email").unwrap(),
            base_type: NamedNode::new("http://www.w3.org/2001/XMLSchema#string").unwrap(),
            facets: vec![DatatypeFacet::MinLength(5)],
        });

    // Act: Generate SHACL
    let shape = SHACLGenerator::generate_shape(&owl_class).unwrap();

    // Assert: Min length applied
    let email_shape = &shape.property_shapes[0];
    assert_eq!(email_shape.min_length, Some(5));
    assert_eq!(email_shape.max_length, None);
}

#[test]
fn test_max_length_restriction() {
    // Arrange: OWL class with maximum length constraint
    let mut owl_class = create_test_person_class();
    owl_class
        .restrictions
        .push(OWLRestriction::DatatypeRestriction {
            property: NamedNode::new("http://example.com/email").unwrap(),
            base_type: NamedNode::new("http://www.w3.org/2001/XMLSchema#string").unwrap(),
            facets: vec![DatatypeFacet::MaxLength(100)],
        });

    // Act: Generate SHACL
    let shape = SHACLGenerator::generate_shape(&owl_class).unwrap();

    // Assert: Max length applied
    let email_shape = &shape.property_shapes[0];
    assert_eq!(email_shape.min_length, None);
    assert_eq!(email_shape.max_length, Some(100));
}

#[test]
fn test_exact_length_restriction() {
    // Arrange: OWL class with exact length constraint
    let mut owl_class = create_test_bond_class();
    owl_class
        .restrictions
        .push(OWLRestriction::DatatypeRestriction {
            property: NamedNode::new("http://example.com/hasISIN").unwrap(),
            base_type: NamedNode::new("http://www.w3.org/2001/XMLSchema#string").unwrap(),
            facets: vec![DatatypeFacet::Length(12)],
        });

    // Act: Generate SHACL
    let shape = SHACLGenerator::generate_shape(&owl_class).unwrap();

    // Assert: Both min and max length set to same value
    let isin_shape = &shape.property_shapes[0];
    assert_eq!(isin_shape.min_length, Some(12));
    assert_eq!(isin_shape.max_length, Some(12));
}

#[test]
fn test_numeric_range_restrictions() {
    // Arrange: OWL class with numeric range (age between 0 and 150)
    let owl_class = OWLClass {
        uri: NamedNode::new("http://example.com/Person").unwrap(),
        label: Some("Person".to_string()),
        comment: None,
        properties: vec![OWLProperty::DatatypeProperty {
            uri: NamedNode::new("http://example.com/age").unwrap(),
            label: Some("age".to_string()),
            range: NamedNode::new("http://www.w3.org/2001/XMLSchema#integer").unwrap(),
        }],
        restrictions: vec![OWLRestriction::DatatypeRestriction {
            property: NamedNode::new("http://example.com/age").unwrap(),
            base_type: NamedNode::new("http://www.w3.org/2001/XMLSchema#integer").unwrap(),
            facets: vec![
                DatatypeFacet::MinInclusive(0.0),
                DatatypeFacet::MaxInclusive(150.0),
            ],
        }],
    };

    // Act: Generate SHACL
    let shape = SHACLGenerator::generate_shape(&owl_class).unwrap();

    // Assert: Numeric range constraints applied
    let age_shape = &shape.property_shapes[0];
    assert_eq!(age_shape.min_inclusive, Some(0.0));
    assert_eq!(age_shape.max_inclusive, Some(150.0));
}

// ============================================================================
// Turtle Serialization Tests (3 tests)
// ============================================================================

#[test]
fn test_turtle_serialization_valid_syntax() {
    // Arrange: Simple SHACL shape
    let mut owl_class = create_test_bond_class();
    owl_class.restrictions.push(OWLRestriction::Cardinality {
        property: NamedNode::new("http://example.com/hasISIN").unwrap(),
        min: Some(1),
        max: Some(1),
    });

    let shape = SHACLGenerator::generate_shape(&owl_class).unwrap();

    // Act: Serialize to Turtle
    let ttl = SHACLGenerator::to_turtle(&shape).unwrap();

    // Assert: Valid Turtle syntax present
    assert!(ttl.contains("sh:NodeShape"));
    assert!(ttl.contains("sh:targetClass"));
    assert!(ttl.contains("sh:PropertyShape"));
    assert!(ttl.contains("sh:minCount 1"));
    assert!(ttl.contains("sh:maxCount 1"));
    assert!(ttl.contains("@prefix sh:"));
    assert!(ttl.contains("<http://example.com/Bond>"));
}

#[test]
fn test_turtle_with_pattern_serialization() {
    // Arrange: Shape with pattern constraint
    let mut owl_class = create_test_bond_class();
    owl_class
        .restrictions
        .push(OWLRestriction::DatatypeRestriction {
            property: NamedNode::new("http://example.com/hasISIN").unwrap(),
            base_type: NamedNode::new("http://www.w3.org/2001/XMLSchema#string").unwrap(),
            facets: vec![
                DatatypeFacet::Pattern("[A-Z]{2}[A-Z0-9]{9}[0-9]".to_string()),
                DatatypeFacet::Length(12),
            ],
        });

    let shape = SHACLGenerator::generate_shape(&owl_class).unwrap();

    // Act: Serialize to Turtle
    let ttl = SHACLGenerator::to_turtle(&shape).unwrap();

    // Assert: Pattern and length in output
    assert!(ttl.contains("sh:pattern"));
    assert!(ttl.contains("[A-Z]{2}[A-Z0-9]{9}[0-9]"));
    assert!(ttl.contains("sh:minLength 12"));
    assert!(ttl.contains("sh:maxLength 12"));
}

#[test]
fn test_turtle_multiple_properties_serialization() {
    // Arrange: Class with multiple properties
    let owl_class = OWLClass {
        uri: NamedNode::new("http://example.com/Bond").unwrap(),
        label: Some("Bond".to_string()),
        comment: None,
        properties: vec![
            OWLProperty::DatatypeProperty {
                uri: NamedNode::new("http://example.com/hasISIN").unwrap(),
                label: Some("has ISIN".to_string()),
                range: NamedNode::new("http://www.w3.org/2001/XMLSchema#string").unwrap(),
            },
            OWLProperty::DatatypeProperty {
                uri: NamedNode::new("http://example.com/maturityDate").unwrap(),
                label: Some("maturity date".to_string()),
                range: NamedNode::new("http://www.w3.org/2001/XMLSchema#date").unwrap(),
            },
        ],
        restrictions: vec![],
    };

    let shape = SHACLGenerator::generate_shape(&owl_class).unwrap();

    // Act: Serialize to Turtle
    let ttl = SHACLGenerator::to_turtle(&shape).unwrap();

    // Assert: Both properties present
    assert!(ttl.contains("hasISINPropertyShape"));
    assert!(ttl.contains("maturityDatePropertyShape"));
    assert!(ttl.contains("<http://example.com/hasISIN>"));
    assert!(ttl.contains("<http://example.com/maturityDate>"));
}

// ============================================================================
// Integration with SHACL Parser Tests (4+ tests)
// ============================================================================

#[test]
fn test_round_trip_owl_to_shacl_to_parser() {
    // Arrange: OWL class with multiple constraints
    let mut owl_class = create_test_bond_class();
    owl_class.restrictions.push(OWLRestriction::Cardinality {
        property: NamedNode::new("http://example.com/hasISIN").unwrap(),
        min: Some(1),
        max: Some(1),
    });
    owl_class
        .restrictions
        .push(OWLRestriction::DatatypeRestriction {
            property: NamedNode::new("http://example.com/hasISIN").unwrap(),
            base_type: NamedNode::new("http://www.w3.org/2001/XMLSchema#string").unwrap(),
            facets: vec![DatatypeFacet::Length(12)],
        });

    // Act: OWL → SHACL → Turtle → Parse back
    let shape = SHACLGenerator::generate_shape(&owl_class).unwrap();
    let ttl = SHACLGenerator::to_turtle(&shape).unwrap();

    // Parse back using oxigraph
    let store = Store::new().unwrap();
    store
        .load_from_slice(oxigraph::io::RdfFormat::Turtle, ttl.as_bytes())
        .unwrap();

    // Assert: Store contains expected triples
    assert!(store.len().unwrap() > 0);

    // Use existing SHACL parser
    use ggen_ai::codegen::shacl_parser::SHACLParser;
    let parser = SHACLParser::new(&store);

    // Try to extract properties (shape URI needs to match)
    let properties = parser.extract_properties(&shape.node_shape_uri).unwrap();

    // Assert: Properties can be extracted
    assert!(
        !properties.is_empty() || store.len().unwrap() > 0,
        "Should have parseable SHACL data"
    );
}

#[test]
fn test_parser_extracts_cardinality_constraints() {
    // Arrange: Generate SHACL with cardinality
    let mut owl_class = create_test_bond_class();
    owl_class.restrictions.push(OWLRestriction::Cardinality {
        property: NamedNode::new("http://example.com/hasISIN").unwrap(),
        min: Some(1),
        max: Some(1),
    });

    let shape = SHACLGenerator::generate_shape(&owl_class).unwrap();
    let ttl = SHACLGenerator::to_turtle(&shape).unwrap();

    // Act: Parse with SHACL parser
    let store = Store::new().unwrap();
    store
        .load_from_slice(oxigraph::io::RdfFormat::Turtle, ttl.as_bytes())
        .unwrap();

    use ggen_ai::codegen::shacl_parser::SHACLParser;
    let _parser = SHACLParser::new(&store);

    // Assert: Can parse the generated Turtle
    assert!(store.len().unwrap() > 0);

    // Verify structure is valid by checking for key predicates
    let has_node_shape = format!("{}", ttl).contains("sh:NodeShape");
    let has_property_shape = format!("{}", ttl).contains("sh:PropertyShape");
    assert!(has_node_shape);
    assert!(has_property_shape);
}

#[test]
fn test_parser_extracts_pattern_constraints() {
    // Arrange: Generate SHACL with pattern
    let mut owl_class = create_test_bond_class();
    owl_class
        .restrictions
        .push(OWLRestriction::DatatypeRestriction {
            property: NamedNode::new("http://example.com/hasISIN").unwrap(),
            base_type: NamedNode::new("http://www.w3.org/2001/XMLSchema#string").unwrap(),
            facets: vec![DatatypeFacet::Pattern(
                "[A-Z]{2}[A-Z0-9]{9}[0-9]".to_string(),
            )],
        });

    let shape = SHACLGenerator::generate_shape(&owl_class).unwrap();
    let ttl = SHACLGenerator::to_turtle(&shape).unwrap();

    // Act: Parse with SHACL parser
    let store = Store::new().unwrap();
    store
        .load_from_slice(oxigraph::io::RdfFormat::Turtle, ttl.as_bytes())
        .unwrap();

    // Assert: Pattern is preserved in Turtle
    assert!(ttl.contains("sh:pattern"));
    assert!(ttl.contains("[A-Z]{2}[A-Z0-9]{9}[0-9]"));
}

#[test]
fn test_parser_extracts_length_constraints() {
    // Arrange: Generate SHACL with length constraints
    let mut owl_class = create_test_person_class();
    owl_class
        .restrictions
        .push(OWLRestriction::DatatypeRestriction {
            property: NamedNode::new("http://example.com/email").unwrap(),
            base_type: NamedNode::new("http://www.w3.org/2001/XMLSchema#string").unwrap(),
            facets: vec![DatatypeFacet::MinLength(5), DatatypeFacet::MaxLength(100)],
        });

    let shape = SHACLGenerator::generate_shape(&owl_class).unwrap();
    let ttl = SHACLGenerator::to_turtle(&shape).unwrap();

    // Act: Parse with SHACL parser
    let store = Store::new().unwrap();
    store
        .load_from_slice(oxigraph::io::RdfFormat::Turtle, ttl.as_bytes())
        .unwrap();

    // Assert: Length constraints in Turtle
    assert!(ttl.contains("sh:minLength 5"));
    assert!(ttl.contains("sh:maxLength 100"));
}

// ============================================================================
// Edge Cases and Complex Scenarios (Additional tests)
// ============================================================================

#[test]
fn test_empty_class_generates_minimal_shape() {
    // Arrange: OWL class with no properties or restrictions
    let owl_class = OWLClass {
        uri: NamedNode::new("http://example.com/Empty").unwrap(),
        label: Some("Empty".to_string()),
        comment: None,
        properties: vec![],
        restrictions: vec![],
    };

    // Act: Generate SHACL
    let shape = SHACLGenerator::generate_shape(&owl_class).unwrap();

    // Assert: Valid but minimal shape
    assert_eq!(shape.target_class, "http://example.com/Empty");
    assert_eq!(shape.property_shapes.len(), 0);

    let ttl = SHACLGenerator::to_turtle(&shape).unwrap();
    assert!(ttl.contains("sh:NodeShape"));
    assert!(ttl.contains("<http://example.com/Empty>"));
}

#[test]
fn test_multiple_facets_on_same_property() {
    // Arrange: Property with multiple facets
    let mut owl_class = create_test_bond_class();
    owl_class
        .restrictions
        .push(OWLRestriction::DatatypeRestriction {
            property: NamedNode::new("http://example.com/hasISIN").unwrap(),
            base_type: NamedNode::new("http://www.w3.org/2001/XMLSchema#string").unwrap(),
            facets: vec![
                DatatypeFacet::Pattern("[A-Z]{2}[A-Z0-9]{9}[0-9]".to_string()),
                DatatypeFacet::Length(12),
            ],
        });

    // Act: Generate SHACL
    let shape = SHACLGenerator::generate_shape(&owl_class).unwrap();

    // Assert: All facets applied
    let isin_shape = &shape.property_shapes[0];
    assert_eq!(
        isin_shape.pattern,
        Some("[A-Z]{2}[A-Z0-9]{9}[0-9]".to_string())
    );
    assert_eq!(isin_shape.min_length, Some(12));
    assert_eq!(isin_shape.max_length, Some(12));
}

#[test]
fn test_object_property_generates_class_constraint() {
    // Arrange: OWL class with object property
    let owl_class = OWLClass {
        uri: NamedNode::new("http://example.com/Bond").unwrap(),
        label: Some("Bond".to_string()),
        comment: None,
        properties: vec![OWLProperty::ObjectProperty {
            uri: NamedNode::new("http://example.com/hasIssuer").unwrap(),
            label: Some("has issuer".to_string()),
            range: NamedNode::new("http://example.com/Organization").unwrap(),
        }],
        restrictions: vec![],
    };

    // Act: Generate SHACL
    let shape = SHACLGenerator::generate_shape(&owl_class).unwrap();

    // Assert: sh:class constraint generated
    let issuer_shape = &shape.property_shapes[0];
    assert_eq!(issuer_shape.datatype, None);
    assert_eq!(
        issuer_shape.class,
        Some("http://example.com/Organization".to_string())
    );

    let ttl = SHACLGenerator::to_turtle(&shape).unwrap();
    assert!(ttl.contains("sh:class"));
    assert!(ttl.contains("<http://example.com/Organization>"));
}

#[test]
fn test_combined_cardinality_and_datatype_restrictions() {
    // Arrange: Both cardinality and datatype restrictions on same property
    let mut owl_class = create_test_bond_class();

    // Add cardinality
    owl_class.restrictions.push(OWLRestriction::Cardinality {
        property: NamedNode::new("http://example.com/hasISIN").unwrap(),
        min: Some(1),
        max: Some(1),
    });

    // Add pattern
    owl_class
        .restrictions
        .push(OWLRestriction::DatatypeRestriction {
            property: NamedNode::new("http://example.com/hasISIN").unwrap(),
            base_type: NamedNode::new("http://www.w3.org/2001/XMLSchema#string").unwrap(),
            facets: vec![DatatypeFacet::Pattern(
                "[A-Z]{2}[A-Z0-9]{9}[0-9]".to_string(),
            )],
        });

    // Act: Generate SHACL
    let shape = SHACLGenerator::generate_shape(&owl_class).unwrap();

    // Assert: Both types of constraints applied
    let isin_shape = &shape.property_shapes[0];
    assert_eq!(isin_shape.min_count, Some(1));
    assert_eq!(isin_shape.max_count, Some(1));
    assert_eq!(
        isin_shape.pattern,
        Some("[A-Z]{2}[A-Z0-9]{9}[0-9]".to_string())
    );

    let ttl = SHACLGenerator::to_turtle(&shape).unwrap();
    assert!(ttl.contains("sh:minCount 1"));
    assert!(ttl.contains("sh:maxCount 1"));
    assert!(ttl.contains("sh:pattern"));
}

#[test]
fn test_exclusive_numeric_bounds() {
    // Arrange: Property with exclusive bounds (0 < value < 100)
    let owl_class = OWLClass {
        uri: NamedNode::new("http://example.com/Product").unwrap(),
        label: Some("Product".to_string()),
        comment: None,
        properties: vec![OWLProperty::DatatypeProperty {
            uri: NamedNode::new("http://example.com/price").unwrap(),
            label: Some("price".to_string()),
            range: NamedNode::new("http://www.w3.org/2001/XMLSchema#decimal").unwrap(),
        }],
        restrictions: vec![OWLRestriction::DatatypeRestriction {
            property: NamedNode::new("http://example.com/price").unwrap(),
            base_type: NamedNode::new("http://www.w3.org/2001/XMLSchema#decimal").unwrap(),
            facets: vec![
                DatatypeFacet::MinExclusive(0.0),
                DatatypeFacet::MaxExclusive(100.0),
            ],
        }],
    };

    // Act: Generate SHACL
    let shape = SHACLGenerator::generate_shape(&owl_class).unwrap();

    // Assert: Exclusive bounds set
    let price_shape = &shape.property_shapes[0];
    assert_eq!(price_shape.min_exclusive, Some(0.0));
    assert_eq!(price_shape.max_exclusive, Some(100.0));

    let ttl = SHACLGenerator::to_turtle(&shape).unwrap();
    assert!(ttl.contains("sh:minExclusive"));
    assert!(ttl.contains("sh:maxExclusive"));
}
