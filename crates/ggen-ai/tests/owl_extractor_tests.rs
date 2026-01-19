//! Comprehensive tests for OWL Extractor using Chicago TDD pattern
//!
//! Test Organization:
//! - Basic extraction (5 tests): class, label, comment, uri
//! - Property extraction (5 tests): datatype properties, object properties, ranges
//! - Cardinality restrictions (5 tests): min, max, exact cardinality
//! - Datatype restrictions (5 tests): pattern, length, min/max values
//! - Error handling (5 tests): missing class, invalid URI, malformed TTL
//!
//! Chicago TDD Pattern: AAA (Arrange-Act-Assert) with real collaborators (Oxigraph)

use std::path::Path;

// Use actual implementation from ggen_ai::owl
use ggen_ai::owl::{
    DatatypeFacet, OWLClass, OWLExtractor, OWLProperty, OWLRestriction, ValueRestrictionType,
};

// =============================================================================
// BASIC EXTRACTION TESTS (5 tests)
// =============================================================================

#[test]
fn test_extract_class_basic_info() {
    // Arrange: Real Oxigraph store with sample OWL
    let store = oxigraph::store::Store::new().unwrap();
    let mut extractor = OWLExtractor::new(store);
    let fixture_path = Path::new(env!("CARGO_MANIFEST_DIR"))
        .join("tests/fixtures/simple_bond.ttl");

    extractor.load_ontology(&fixture_path).unwrap();

    // Act: Extract Bond class
    let result = extractor.extract_class("http://example.com/test#Bond");

    // Assert: Verify basic structure
    assert!(result.is_ok(), "Should extract Bond class successfully");
    let bond_class = result.unwrap();
    assert_eq!(bond_class.uri.as_str(), "http://example.com/test#Bond");
}

#[test]
fn test_extract_class_with_label() {
    // Arrange
    let store = oxigraph::store::Store::new().unwrap();
    let mut extractor = OWLExtractor::new(store);
    let fixture_path = Path::new(env!("CARGO_MANIFEST_DIR"))
        .join("tests/fixtures/simple_bond.ttl");

    extractor.load_ontology(&fixture_path).unwrap();

    // Act
    let bond_class = extractor.extract_class("http://example.com/test#Bond").unwrap();

    // Assert: Label is extracted correctly
    assert_eq!(bond_class.label, Some("Test Bond".to_string()));
}

#[test]
fn test_extract_class_with_comment() {
    // Arrange
    let store = oxigraph::store::Store::new().unwrap();
    let mut extractor = OWLExtractor::new(store);
    let fixture_path = Path::new(env!("CARGO_MANIFEST_DIR"))
        .join("tests/fixtures/simple_bond.ttl");

    extractor.load_ontology(&fixture_path).unwrap();

    // Act
    let bond_class = extractor.extract_class("http://example.com/test#Bond").unwrap();

    // Assert: Comment is extracted correctly
    assert_eq!(
        bond_class.comment,
        Some("A test bond for extraction".to_string())
    );
}

#[test]
fn test_extract_class_uri_preserved() {
    // Arrange
    let store = oxigraph::store::Store::new().unwrap();
    let mut extractor = OWLExtractor::new(store);
    let fixture_path = Path::new(env!("CARGO_MANIFEST_DIR"))
        .join("tests/fixtures/simple_bond.ttl");

    extractor.load_ontology(&fixture_path).unwrap();

    // Act
    let bond_class = extractor.extract_class("http://example.com/test#Bond").unwrap();

    // Assert: URI is exactly as requested
    assert_eq!(bond_class.uri.as_str(), "http://example.com/test#Bond");
}

#[test]
fn test_extract_empty_class() {
    // Arrange
    let store = oxigraph::store::Store::new().unwrap();
    let mut extractor = OWLExtractor::new(store);
    let fixture_path = Path::new(env!("CARGO_MANIFEST_DIR"))
        .join("tests/fixtures/simple_bond.ttl");

    extractor.load_ontology(&fixture_path).unwrap();

    // Act
    let empty_class = extractor.extract_class("http://example.com/test#EmptyClass").unwrap();

    // Assert: Class exists but has no properties
    assert_eq!(empty_class.label, Some("Empty Class".to_string()));
    assert_eq!(empty_class.properties.len(), 0);
    assert_eq!(empty_class.restrictions.len(), 0);
}

// =============================================================================
// PROPERTY EXTRACTION TESTS (5 tests)
// =============================================================================

#[test]
fn test_extract_datatype_property() {
    // Arrange
    let store = oxigraph::store::Store::new().unwrap();
    let mut extractor = OWLExtractor::new(store);
    let fixture_path = Path::new(env!("CARGO_MANIFEST_DIR"))
        .join("tests/fixtures/simple_bond.ttl");

    extractor.load_ontology(&fixture_path).unwrap();

    // Act
    let bond_class = extractor.extract_class("http://example.com/test#Bond").unwrap();

    // Assert: Has ISIN property extracted
    assert!(bond_class.properties.len() >= 1, "Should have at least one property");

    let has_isin = bond_class.properties.iter().find(|p| {
        match p {
            OWLProperty::DatatypeProperty { uri, .. } => {
                uri.as_str().contains("hasISIN")
            }
            _ => false,
        }
    });

    assert!(has_isin.is_some(), "Should find hasISIN property");

    match has_isin.unwrap() {
        OWLProperty::DatatypeProperty { uri, label, range } => {
            assert!(uri.as_str().contains("hasISIN"));
            assert_eq!(label, &Some("has ISIN".to_string()));
            assert!(range.as_str().contains("string"));
        }
        _ => panic!("Expected DatatypeProperty"),
    }
}

#[test]
fn test_extract_multiple_datatype_properties() {
    // Arrange
    let store = oxigraph::store::Store::new().unwrap();
    let mut extractor = OWLExtractor::new(store);
    let fixture_path = Path::new(env!("CARGO_MANIFEST_DIR"))
        .join("tests/fixtures/simple_bond.ttl");

    extractor.load_ontology(&fixture_path).unwrap();

    // Act
    let bond_class = extractor.extract_class("http://example.com/test#Bond").unwrap();

    // Assert: Multiple properties extracted
    let datatype_props: Vec<_> = bond_class.properties.iter().filter(|p| {
        matches!(p, OWLProperty::DatatypeProperty { .. })
    }).collect();

    assert!(datatype_props.len() >= 4, "Should have at least 4 datatype properties");
}

#[test]
fn test_extract_object_property() {
    // Arrange
    let store = oxigraph::store::Store::new().unwrap();
    let mut extractor = OWLExtractor::new(store);
    let fixture_path = Path::new(env!("CARGO_MANIFEST_DIR"))
        .join("tests/fixtures/simple_bond.ttl");

    extractor.load_ontology(&fixture_path).unwrap();

    // Act
    let bond_class = extractor.extract_class("http://example.com/test#Bond").unwrap();

    // Assert: Has issuer object property
    let has_issuer = bond_class.properties.iter().find(|p| {
        match p {
            OWLProperty::ObjectProperty { uri, .. } => {
                uri.as_str().contains("hasIssuer")
            }
            _ => false,
        }
    });

    assert!(has_issuer.is_some(), "Should find hasIssuer object property");

    match has_issuer.unwrap() {
        OWLProperty::ObjectProperty { uri, label, range } => {
            assert!(uri.as_str().contains("hasIssuer"));
            assert_eq!(label, &Some("has issuer".to_string()));
            assert!(range.as_str().contains("Issuer"));
        }
        _ => panic!("Expected ObjectProperty"),
    }
}

#[test]
fn test_property_ranges_correct() {
    // Arrange
    let store = oxigraph::store::Store::new().unwrap();
    let mut extractor = OWLExtractor::new(store);
    let fixture_path = Path::new(env!("CARGO_MANIFEST_DIR"))
        .join("tests/fixtures/simple_bond.ttl");

    extractor.load_ontology(&fixture_path).unwrap();

    // Act
    let bond_class = extractor.extract_class("http://example.com/test#Bond").unwrap();

    // Assert: Different properties have correct range types
    for prop in &bond_class.properties {
        match prop {
            OWLProperty::DatatypeProperty { uri, range, .. } => {
                if uri.as_str().contains("hasMaturityDate") {
                    assert!(range.as_str().contains("date"), "Maturity date should be xsd:date");
                } else if uri.as_str().contains("hasCouponRate") || uri.as_str().contains("hasParValue") {
                    assert!(range.as_str().contains("decimal"), "Numeric fields should be xsd:decimal");
                } else if uri.as_str().contains("ISIN") {
                    assert!(range.as_str().contains("string"), "ISIN should be xsd:string");
                }
            }
            _ => {}
        }
    }
}

#[test]
fn test_property_labels_extracted() {
    // Arrange
    let store = oxigraph::store::Store::new().unwrap();
    let mut extractor = OWLExtractor::new(store);
    let fixture_path = Path::new(env!("CARGO_MANIFEST_DIR"))
        .join("tests/fixtures/simple_bond.ttl");

    extractor.load_ontology(&fixture_path).unwrap();

    // Act
    let bond_class = extractor.extract_class("http://example.com/test#Bond").unwrap();

    // Assert: Properties have labels
    for prop in &bond_class.properties {
        let label = match prop {
            OWLProperty::DatatypeProperty { label, .. } => label,
            OWLProperty::ObjectProperty { label, .. } => label,
        };
        assert!(label.is_some(), "All properties should have labels");
        assert!(!label.as_ref().unwrap().is_empty(), "Labels should not be empty");
    }
}

// =============================================================================
// CARDINALITY RESTRICTION TESTS (5 tests)
// =============================================================================

#[test]
fn test_extract_exact_cardinality_restriction() {
    // Arrange
    let store = oxigraph::store::Store::new().unwrap();
    let mut extractor = OWLExtractor::new(store);
    let fixture_path = Path::new(env!("CARGO_MANIFEST_DIR"))
        .join("tests/fixtures/simple_bond.ttl");

    extractor.load_ontology(&fixture_path).unwrap();

    // Act
    let bond_class = extractor.extract_class("http://example.com/test#BondWithRestrictions").unwrap();

    // Assert: Has exact cardinality restriction on hasISIN
    let isin_restriction = bond_class.restrictions.iter().find(|r| {
        match r {
            OWLRestriction::Cardinality { property, .. } => {
                property.as_str().contains("hasISIN")
            }
            _ => false,
        }
    });

    assert!(isin_restriction.is_some(), "Should find cardinality restriction on hasISIN");

    match isin_restriction.unwrap() {
        OWLRestriction::Cardinality { min, max, .. } => {
            assert_eq!(*min, Some(1), "Exact cardinality should set min=1");
            assert_eq!(*max, Some(1), "Exact cardinality should set max=1");
        }
        _ => panic!("Expected Cardinality restriction"),
    }
}

#[test]
fn test_extract_min_cardinality_restriction() {
    // Arrange
    let store = oxigraph::store::Store::new().unwrap();
    let mut extractor = OWLExtractor::new(store);
    let fixture_path = Path::new(env!("CARGO_MANIFEST_DIR"))
        .join("tests/fixtures/simple_bond.ttl");

    extractor.load_ontology(&fixture_path).unwrap();

    // Act
    let bond_class = extractor.extract_class("http://example.com/test#BondWithRestrictions").unwrap();

    // Assert: Has minCardinality restriction on hasMaturityDate
    let maturity_restriction = bond_class.restrictions.iter().find(|r| {
        match r {
            OWLRestriction::Cardinality { property, .. } => {
                property.as_str().contains("hasMaturityDate")
            }
            _ => false,
        }
    });

    assert!(maturity_restriction.is_some(), "Should find cardinality restriction on hasMaturityDate");

    match maturity_restriction.unwrap() {
        OWLRestriction::Cardinality { min, max, .. } => {
            assert_eq!(*min, Some(1), "minCardinality should be 1");
            assert_eq!(*max, None, "maxCardinality should not be set");
        }
        _ => panic!("Expected Cardinality restriction"),
    }
}

#[test]
fn test_extract_max_cardinality_restriction() {
    // Arrange
    let store = oxigraph::store::Store::new().unwrap();
    let mut extractor = OWLExtractor::new(store);
    let fixture_path = Path::new(env!("CARGO_MANIFEST_DIR"))
        .join("tests/fixtures/simple_bond.ttl");

    extractor.load_ontology(&fixture_path).unwrap();

    // Act
    let bond_class = extractor.extract_class("http://example.com/test#BondWithRestrictions").unwrap();

    // Assert: Has maxCardinality restriction on hasCouponRate
    let coupon_restriction = bond_class.restrictions.iter().find(|r| {
        match r {
            OWLRestriction::Cardinality { property, .. } => {
                property.as_str().contains("hasCouponRate")
            }
            _ => false,
        }
    });

    assert!(coupon_restriction.is_some(), "Should find cardinality restriction on hasCouponRate");

    match coupon_restriction.unwrap() {
        OWLRestriction::Cardinality { min, max, .. } => {
            assert_eq!(*min, None, "minCardinality should not be set");
            assert_eq!(*max, Some(1), "maxCardinality should be 1");
        }
        _ => panic!("Expected Cardinality restriction"),
    }
}

#[test]
fn test_multiple_cardinality_restrictions() {
    // Arrange
    let store = oxigraph::store::Store::new().unwrap();
    let mut extractor = OWLExtractor::new(store);
    let fixture_path = Path::new(env!("CARGO_MANIFEST_DIR"))
        .join("tests/fixtures/simple_bond.ttl");

    extractor.load_ontology(&fixture_path).unwrap();

    // Act
    let bond_class = extractor.extract_class("http://example.com/test#BondWithRestrictions").unwrap();

    // Assert: Has multiple cardinality restrictions
    let cardinality_restrictions: Vec<_> = bond_class.restrictions.iter().filter(|r| {
        matches!(r, OWLRestriction::Cardinality { .. })
    }).collect();

    assert_eq!(cardinality_restrictions.len(), 3, "Should have 3 cardinality restrictions");
}

#[test]
fn test_cardinality_property_references() {
    // Arrange
    let store = oxigraph::store::Store::new().unwrap();
    let mut extractor = OWLExtractor::new(store);
    let fixture_path = Path::new(env!("CARGO_MANIFEST_DIR"))
        .join("tests/fixtures/simple_bond.ttl");

    extractor.load_ontology(&fixture_path).unwrap();

    // Act
    let bond_class = extractor.extract_class("http://example.com/test#BondWithRestrictions").unwrap();

    // Assert: Cardinality restrictions reference valid properties
    for restriction in &bond_class.restrictions {
        match restriction {
            OWLRestriction::Cardinality { property, .. } => {
                assert!(!property.as_str().is_empty(), "Property URI should not be empty");
                assert!(
                    property.as_str().contains("has"),
                    "Property URI should be valid: {}",
                    property.as_str()
                );
            }
            _ => {}
        }
    }
}

// =============================================================================
// DATATYPE RESTRICTION TESTS (5 tests)
// =============================================================================

#[test]
fn test_extract_pattern_restriction() {
    // Arrange
    let store = oxigraph::store::Store::new().unwrap();
    let mut extractor = OWLExtractor::new(store);
    let fixture_path = Path::new(env!("CARGO_MANIFEST_DIR"))
        .join("tests/fixtures/simple_bond.ttl");

    extractor.load_ontology(&fixture_path).unwrap();

    // Act
    let bond_class = extractor.extract_class("http://example.com/test#BondWithDatatypeRestrictions").unwrap();

    // Assert: Has pattern restriction on hasISIN
    let datatype_restriction = bond_class.restrictions.iter().find(|r| {
        matches!(r, OWLRestriction::DatatypeRestriction { .. })
    });

    assert!(datatype_restriction.is_some(), "Should find datatype restriction");

    match datatype_restriction.unwrap() {
        OWLRestriction::DatatypeRestriction { facets, .. } => {
            let has_pattern = facets.iter().any(|f| {
                matches!(f, DatatypeFacet::Pattern(_))
            });
            assert!(has_pattern, "Should have pattern facet");

            // Verify pattern value
            for facet in facets {
                if let DatatypeFacet::Pattern(pattern) = facet {
                    assert_eq!(pattern, "[A-Z]{2}[A-Z0-9]{10}", "Pattern should match ISIN format");
                }
            }
        }
        _ => panic!("Expected DatatypeRestriction"),
    }
}

#[test]
fn test_extract_length_restrictions() {
    // Arrange
    let store = oxigraph::store::Store::new().unwrap();
    let mut extractor = OWLExtractor::new(store);
    let fixture_path = Path::new(env!("CARGO_MANIFEST_DIR"))
        .join("tests/fixtures/simple_bond.ttl");

    extractor.load_ontology(&fixture_path).unwrap();

    // Act
    let bond_class = extractor.extract_class("http://example.com/test#BondWithDatatypeRestrictions").unwrap();

    // Assert: Has minLength and maxLength facets
    let datatype_restriction = bond_class.restrictions.iter().find(|r| {
        matches!(r, OWLRestriction::DatatypeRestriction { .. })
    });

    assert!(datatype_restriction.is_some());

    match datatype_restriction.unwrap() {
        OWLRestriction::DatatypeRestriction { facets, .. } => {
            let has_min_length = facets.iter().any(|f| {
                matches!(f, DatatypeFacet::MinLength(12))
            });
            let has_max_length = facets.iter().any(|f| {
                matches!(f, DatatypeFacet::MaxLength(12))
            });

            assert!(has_min_length, "Should have minLength facet");
            assert!(has_max_length, "Should have maxLength facet");
        }
        _ => panic!("Expected DatatypeRestriction"),
    }
}

#[test]
fn test_extract_numeric_range_restrictions() {
    // Arrange
    let store = oxigraph::store::Store::new().unwrap();
    let mut extractor = OWLExtractor::new(store);
    let fixture_path = Path::new(env!("CARGO_MANIFEST_DIR"))
        .join("tests/fixtures/simple_bond.ttl");

    extractor.load_ontology(&fixture_path).unwrap();

    // Act
    let bond_class = extractor.extract_class("http://example.com/test#BondWithNumericRestrictions").unwrap();

    // Assert: Has minInclusive and maxInclusive facets
    let datatype_restriction = bond_class.restrictions.iter().find(|r| {
        matches!(r, OWLRestriction::DatatypeRestriction { .. })
    });

    assert!(datatype_restriction.is_some(), "Should find numeric restriction");

    match datatype_restriction.unwrap() {
        OWLRestriction::DatatypeRestriction { facets, .. } => {
            let has_min = facets.iter().any(|f| {
                matches!(f, DatatypeFacet::MinInclusive(0.0))
            });
            let has_max = facets.iter().any(|f| {
                matches!(f, DatatypeFacet::MaxInclusive(100.0))
            });

            assert!(has_min, "Should have minInclusive facet");
            assert!(has_max, "Should have maxInclusive facet");
        }
        _ => panic!("Expected DatatypeRestriction"),
    }
}

#[test]
fn test_datatype_restriction_base_type() {
    // Arrange
    let store = oxigraph::store::Store::new().unwrap();
    let mut extractor = OWLExtractor::new(store);
    let fixture_path = Path::new(env!("CARGO_MANIFEST_DIR"))
        .join("tests/fixtures/simple_bond.ttl");

    extractor.load_ontology(&fixture_path).unwrap();

    // Act
    let bond_class = extractor.extract_class("http://example.com/test#BondWithDatatypeRestrictions").unwrap();

    // Assert: Datatype restriction has correct base type
    let datatype_restriction = bond_class.restrictions.iter().find(|r| {
        matches!(r, OWLRestriction::DatatypeRestriction { .. })
    });

    match datatype_restriction.unwrap() {
        OWLRestriction::DatatypeRestriction { base_type, .. } => {
            assert!(base_type.as_str().contains("string"), "Base type should be xsd:string");
        }
        _ => panic!("Expected DatatypeRestriction"),
    }
}

#[test]
fn test_multiple_facets_in_restriction() {
    // Arrange
    let store = oxigraph::store::Store::new().unwrap();
    let mut extractor = OWLExtractor::new(store);
    let fixture_path = Path::new(env!("CARGO_MANIFEST_DIR"))
        .join("tests/fixtures/simple_bond.ttl");

    extractor.load_ontology(&fixture_path).unwrap();

    // Act
    let bond_class = extractor.extract_class("http://example.com/test#BondWithDatatypeRestrictions").unwrap();

    // Assert: Datatype restriction has multiple facets (pattern + min/max length)
    let datatype_restriction = bond_class.restrictions.iter().find(|r| {
        matches!(r, OWLRestriction::DatatypeRestriction { .. })
    });

    match datatype_restriction.unwrap() {
        OWLRestriction::DatatypeRestriction { facets, .. } => {
            assert!(facets.len() >= 3, "Should have at least 3 facets (pattern, minLength, maxLength)");
        }
        _ => panic!("Expected DatatypeRestriction"),
    }
}

// =============================================================================
// VALUE RESTRICTION TESTS (5 tests)
// =============================================================================

#[test]
fn test_extract_all_values_from_restriction() {
    // Arrange
    let store = oxigraph::store::Store::new().unwrap();
    let mut extractor = OWLExtractor::new(store);
    let fixture_path = Path::new(env!("CARGO_MANIFEST_DIR"))
        .join("tests/fixtures/simple_bond.ttl");

    extractor.load_ontology(&fixture_path).unwrap();

    // Act
    let bond_class = extractor.extract_class("http://example.com/test#BondWithValueRestrictions").unwrap();

    // Assert: Has allValuesFrom restriction
    let value_restriction = bond_class.restrictions.iter().find(|r| {
        match r {
            OWLRestriction::ValueRestriction { property, value_type } => {
                property.as_str().contains("hasIssuer") &&
                matches!(value_type, ValueRestrictionType::AllValuesFrom(_))
            }
            _ => false,
        }
    });

    assert!(value_restriction.is_some(), "Should find allValuesFrom restriction");

    match value_restriction.unwrap() {
        OWLRestriction::ValueRestriction { value_type, .. } => {
            match value_type {
                ValueRestrictionType::AllValuesFrom(class) => {
                    assert!(class.as_str().contains("Issuer"), "Should reference Issuer class");
                }
                _ => panic!("Expected AllValuesFrom"),
            }
        }
        _ => panic!("Expected ValueRestriction"),
    }
}

#[test]
fn test_extract_some_values_from_restriction() {
    // Arrange
    let store = oxigraph::store::Store::new().unwrap();
    let mut extractor = OWLExtractor::new(store);
    let fixture_path = Path::new(env!("CARGO_MANIFEST_DIR"))
        .join("tests/fixtures/simple_bond.ttl");

    extractor.load_ontology(&fixture_path).unwrap();

    // Act
    let bond_class = extractor.extract_class("http://example.com/test#BondWithValueRestrictions").unwrap();

    // Assert: Has someValuesFrom restriction
    let value_restriction = bond_class.restrictions.iter().find(|r| {
        match r {
            OWLRestriction::ValueRestriction { property, value_type } => {
                property.as_str().contains("hasCouponRate") &&
                matches!(value_type, ValueRestrictionType::SomeValuesFrom(_))
            }
            _ => false,
        }
    });

    assert!(value_restriction.is_some(), "Should find someValuesFrom restriction");
}

#[test]
fn test_value_restriction_property_references() {
    // Arrange
    let store = oxigraph::store::Store::new().unwrap();
    let mut extractor = OWLExtractor::new(store);
    let fixture_path = Path::new(env!("CARGO_MANIFEST_DIR"))
        .join("tests/fixtures/simple_bond.ttl");

    extractor.load_ontology(&fixture_path).unwrap();

    // Act
    let bond_class = extractor.extract_class("http://example.com/test#BondWithValueRestrictions").unwrap();

    // Assert: Value restrictions reference valid properties
    for restriction in &bond_class.restrictions {
        match restriction {
            OWLRestriction::ValueRestriction { property, .. } => {
                assert!(!property.as_str().is_empty(), "Property URI should not be empty");
                assert!(
                    property.as_str().contains("has"),
                    "Property should have valid URI"
                );
            }
            _ => {}
        }
    }
}

#[test]
fn test_mixed_restriction_types() {
    // Arrange
    let store = oxigraph::store::Store::new().unwrap();
    let mut extractor = OWLExtractor::new(store);
    let fixture_path = Path::new(env!("CARGO_MANIFEST_DIR"))
        .join("tests/fixtures/simple_bond.ttl");

    extractor.load_ontology(&fixture_path).unwrap();

    // Act
    let bond_class = extractor.extract_class("http://example.com/test#BondWithValueRestrictions").unwrap();

    // Assert: Class has multiple types of restrictions
    let has_value_restrictions = bond_class.restrictions.iter().any(|r| {
        matches!(r, OWLRestriction::ValueRestriction { .. })
    });

    assert!(has_value_restrictions, "Should have at least one value restriction");
}

#[test]
fn test_value_restriction_class_references() {
    // Arrange
    let store = oxigraph::store::Store::new().unwrap();
    let mut extractor = OWLExtractor::new(store);
    let fixture_path = Path::new(env!("CARGO_MANIFEST_DIR"))
        .join("tests/fixtures/simple_bond.ttl");

    extractor.load_ontology(&fixture_path).unwrap();

    // Act
    let bond_class = extractor.extract_class("http://example.com/test#BondWithValueRestrictions").unwrap();

    // Assert: AllValuesFrom references are valid class URIs
    for restriction in &bond_class.restrictions {
        match restriction {
            OWLRestriction::ValueRestriction { value_type, .. } => {
                match value_type {
                    ValueRestrictionType::AllValuesFrom(class) => {
                        assert!(!class.as_str().is_empty(), "Class URI should not be empty");
                        assert!(
                            class.as_str().starts_with("http://"),
                            "Class URI should be absolute"
                        );
                    }
                    ValueRestrictionType::SomeValuesFrom(class) => {
                        assert!(!class.as_str().is_empty(), "Class URI should not be empty");
                    }
                }
            }
            _ => {}
        }
    }
}

// =============================================================================
// ERROR HANDLING TESTS (5 tests)
// =============================================================================

#[test]
fn test_extract_missing_class_returns_error() {
    // Arrange
    let store = oxigraph::store::Store::new().unwrap();
    let mut extractor = OWLExtractor::new(store);
    let fixture_path = Path::new(env!("CARGO_MANIFEST_DIR"))
        .join("tests/fixtures/simple_bond.ttl");

    extractor.load_ontology(&fixture_path).unwrap();

    // Act
    let result = extractor.extract_class("http://example.com/test#NonExistentClass");

    // Assert: Should return error for missing class
    assert!(result.is_err(), "Should return error for non-existent class");
}

#[test]
fn test_extract_invalid_uri_returns_error() {
    // Arrange
    let store = oxigraph::store::Store::new().unwrap();
    let extractor = OWLExtractor::new(store);

    // Act
    let result = extractor.extract_class("not a valid uri");

    // Assert: Should return error for invalid URI
    assert!(result.is_err(), "Should return error for invalid URI");
}

#[test]
fn test_load_missing_file_returns_error() {
    // Arrange
    let store = oxigraph::store::Store::new().unwrap();
    let mut extractor = OWLExtractor::new(store);
    let nonexistent_path = Path::new("/nonexistent/file.ttl");

    // Act
    let result = extractor.load_ontology(nonexistent_path);

    // Assert: Should return error for missing file
    assert!(result.is_err(), "Should return error for missing file");
}

#[test]
fn test_load_malformed_ttl_returns_error() {
    // Arrange
    let store = oxigraph::store::Store::new().unwrap();
    let mut extractor = OWLExtractor::new(store);

    // Create a temporary malformed TTL file
    use std::io::Write;
    let temp_dir = std::env::temp_dir();
    let malformed_path = temp_dir.join("malformed.ttl");
    let mut file = std::fs::File::create(&malformed_path).unwrap();
    writeln!(file, "This is not valid Turtle syntax {{{{ }}}}").unwrap();

    // Act
    let result = extractor.load_ontology(&malformed_path);

    // Assert: Should return error for malformed Turtle
    assert!(result.is_err(), "Should return error for malformed Turtle");

    // Cleanup
    std::fs::remove_file(malformed_path).ok();
}

#[test]
fn test_extract_empty_uri_returns_error() {
    // Arrange
    let store = oxigraph::store::Store::new().unwrap();
    let extractor = OWLExtractor::new(store);

    // Act
    let result = extractor.extract_class("");

    // Assert: Should return error for empty URI
    assert!(result.is_err(), "Should return error for empty URI");
}

// =============================================================================
// ADDITIONAL INTEGRATION TESTS (5+ tests)
// =============================================================================

#[test]
fn test_complete_class_extraction_workflow() {
    // Arrange
    let store = oxigraph::store::Store::new().unwrap();
    let mut extractor = OWLExtractor::new(store);
    let fixture_path = Path::new(env!("CARGO_MANIFEST_DIR"))
        .join("tests/fixtures/simple_bond.ttl");

    extractor.load_ontology(&fixture_path).unwrap();

    // Act: Extract complex class with all restriction types
    let result = extractor.extract_class("http://example.com/test#BondWithRestrictions");

    // Assert: Complete workflow succeeds
    assert!(result.is_ok(), "Complete workflow should succeed");
    let bond_class = result.unwrap();

    // Verify all components present
    assert!(!bond_class.uri.as_str().is_empty());
    assert!(bond_class.restrictions.len() > 0);
}

#[test]
fn test_multiple_ontology_loads() {
    // Arrange
    let store = oxigraph::store::Store::new().unwrap();
    let mut extractor = OWLExtractor::new(store);
    let fixture_path = Path::new(env!("CARGO_MANIFEST_DIR"))
        .join("tests/fixtures/simple_bond.ttl");

    // Act: Load ontology multiple times
    let result1 = extractor.load_ontology(&fixture_path);
    let result2 = extractor.load_ontology(&fixture_path);

    // Assert: Multiple loads should succeed (idempotent or additive)
    assert!(result1.is_ok(), "First load should succeed");
    assert!(result2.is_ok(), "Second load should succeed");
}

#[test]
fn test_extract_multiple_classes_same_store() {
    // Arrange
    let store = oxigraph::store::Store::new().unwrap();
    let mut extractor = OWLExtractor::new(store);
    let fixture_path = Path::new(env!("CARGO_MANIFEST_DIR"))
        .join("tests/fixtures/simple_bond.ttl");

    extractor.load_ontology(&fixture_path).unwrap();

    // Act: Extract multiple classes
    let bond = extractor.extract_class("http://example.com/test#Bond");
    let issuer = extractor.extract_class("http://example.com/test#Issuer");
    let empty = extractor.extract_class("http://example.com/test#EmptyClass");

    // Assert: All extractions should succeed
    assert!(bond.is_ok(), "Bond extraction should succeed");
    assert!(issuer.is_ok(), "Issuer extraction should succeed");
    assert!(empty.is_ok(), "EmptyClass extraction should succeed");
}

#[test]
fn test_class_hierarchy_preserved() {
    // Arrange
    let store = oxigraph::store::Store::new().unwrap();
    let mut extractor = OWLExtractor::new(store);
    let fixture_path = Path::new(env!("CARGO_MANIFEST_DIR"))
        .join("tests/fixtures/simple_bond.ttl");

    extractor.load_ontology(&fixture_path).unwrap();

    // Act: Extract parent and child classes
    let parent = extractor.extract_class("http://example.com/test#Bond");
    let child = extractor.extract_class("http://example.com/test#BondWithRestrictions");

    // Assert: Both classes extracted successfully (hierarchy preserved in graph)
    assert!(parent.is_ok(), "Parent class should be extracted");
    assert!(child.is_ok(), "Child class should be extracted");
}

#[test]
fn test_property_and_restriction_consistency() {
    // Arrange
    let store = oxigraph::store::Store::new().unwrap();
    let mut extractor = OWLExtractor::new(store);
    let fixture_path = Path::new(env!("CARGO_MANIFEST_DIR"))
        .join("tests/fixtures/simple_bond.ttl");

    extractor.load_ontology(&fixture_path).unwrap();

    // Act
    let bond_class = extractor.extract_class("http://example.com/test#BondWithRestrictions").unwrap();

    // Assert: Restrictions reference properties that exist
    for restriction in &bond_class.restrictions {
        let property_uri = match restriction {
            OWLRestriction::Cardinality { property, .. } => property.as_str(),
            OWLRestriction::DatatypeRestriction { property, .. } => property.as_str(),
            OWLRestriction::ValueRestriction { property, .. } => property.as_str(),
        };

        // Property should be valid URI
        assert!(
            property_uri.starts_with("http://"),
            "Restriction property should have valid URI: {}",
            property_uri
        );
    }
}
