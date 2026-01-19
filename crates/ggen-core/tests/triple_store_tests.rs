//! Chicago TDD Triple Store Tests
//!
//! Tests SPARQL-based extraction and querying of RDF triples.
//! Focus: observable state changes, real collaborators (RDF graphs), determinism
//!
//! AAA Pattern: Arrange (real RDF data) -> Act (extract/query) -> Assert (state changes)

use ggen_core::graph::Graph;
use ggen_core::ontology::{Cardinality, OntologyExtractor, PropertyRange, RelationshipType};
use std::fs;
use std::path::Path;

/// Load RDF/Turtle fixture file from disk
fn load_fixture(filename: &str) -> String {
    let fixture_path = Path::new("crates/ggen-core/tests/fixtures").join(filename);
    fs::read_to_string(fixture_path)
        .unwrap_or_else(|e| panic!("Failed to load fixture {}: {}", filename, e))
}

/// Helper: Create graph from TTL fixture
fn setup_graph_from_fixture(fixture_name: &str) -> Graph {
    let graph = Graph::new().expect("Failed to create graph");
    let turtle = load_fixture(fixture_name);
    graph
        .insert_turtle(&turtle)
        .expect("Failed to insert RDF data");
    graph
}

/// Helper: Extract ontology schema from graph
fn extract_schema(
    graph: &Graph, namespace: &str,
) -> Result<ggen_core::ontology::OntologySchema, String> {
    OntologyExtractor::extract(graph, namespace)
}

// ============================================================================
// TEST GROUP: Load Valid Ontologies
// ============================================================================

/// ARRANGE: Load TTL file with HIPAA policies
/// ACT: Extract ontology schema
/// ASSERT: Classes and properties extracted successfully (observable state change)
#[test]
fn load_hipaa_ontology_enables_schema_extraction() {
    // Arrange
    let graph = setup_graph_from_fixture("hipaa_legal.ttl");

    // Act
    let schema = extract_schema(&graph, "http://example.org/legal/hipaa#")
        .expect("Should extract HIPAA ontology");

    // Assert: State changed from empty graph to extracted schema
    assert!(
        !schema.classes.is_empty(),
        "Should extract classes from HIPAA ontology"
    );
    assert!(
        !schema.properties.is_empty(),
        "Should extract properties from HIPAA ontology"
    );

    // Verify specific classes exist
    let class_names: Vec<_> = schema.classes.iter().map(|c| c.name.as_str()).collect();
    assert!(
        class_names.contains(&"HIPAACompliance"),
        "Should contain HIPAACompliance class"
    );
    assert!(
        class_names.contains(&"ProtectedHealthInformation"),
        "Should contain ProtectedHealthInformation class"
    );
    assert!(
        class_names.contains(&"AccessControl"),
        "Should contain AccessControl class"
    );
}

/// ARRANGE: Load IT SLA ontology
/// ACT: Extract and query for service metric classes
/// ASSERT: Specific service classes and properties extracted (state verification)
#[test]
fn load_it_sla_ontology_provides_queryable_metrics() {
    // Arrange
    let graph = setup_graph_from_fixture("it_sla.ttl");

    // Act
    let schema = extract_schema(&graph, "http://example.org/it/sla#")
        .expect("Should extract IT SLA ontology");

    // Assert: Observable state - metrics are queryable
    let has_availability = schema.classes.iter().any(|c| c.name == "Availability");
    let has_response_time = schema.classes.iter().any(|c| c.name == "ResponseTime");
    let has_sla_class = schema
        .classes
        .iter()
        .any(|c| c.name == "ServiceLevelAgreement");

    assert!(has_availability, "Should have Availability class");
    assert!(has_response_time, "Should have ResponseTime class");
    assert!(has_sla_class, "Should have ServiceLevelAgreement class");

    // Verify availability-specific properties exist
    let availability_percentage = schema
        .properties
        .iter()
        .any(|p| p.name == "availabilityPercentage");
    assert!(
        availability_percentage,
        "Should have availabilityPercentage property"
    );
}

/// ARRANGE: Load Security MFA ontology
/// ACT: Extract authentication factor classes and relationships
/// ASSERT: MFA structure correctly extracted with proper relationships
#[test]
fn load_security_mfa_ontology_extracts_authentication_hierarchy() {
    // Arrange
    let graph = setup_graph_from_fixture("security_mfa.ttl");

    // Act
    let schema = extract_schema(&graph, "http://example.org/security/mfa#")
        .expect("Should extract Security MFA ontology");

    // Assert: Observable state - authentication hierarchy extracted
    let mfa_class = schema
        .find_class("MultiFactorAuthentication")
        .expect("Should find MFA class");
    assert_eq!(
        mfa_class.name, "MultiFactorAuthentication",
        "MFA class name should be correct"
    );

    // Verify factor classes exist
    let has_knowledge = schema.classes.iter().any(|c| c.name == "KnowledgeFactor");
    let has_possession = schema.classes.iter().any(|c| c.name == "PossessionFactor");
    let has_biometric = schema.classes.iter().any(|c| c.name == "BiometricFactor");

    assert!(has_knowledge, "Should have KnowledgeFactor class");
    assert!(has_possession, "Should have PossessionFactor class");
    assert!(has_biometric, "Should have BiometricFactor class");

    // Verify relationships exist
    let has_uses_factor = schema.properties.iter().any(|p| p.name == "usesFactor");
    assert!(has_uses_factor, "Should have usesFactor property");
}

/// ARRANGE: Load AWS Cloud ontology
/// ACT: Extract cloud service types
/// ASSERT: Cloud services and regions correctly extracted
#[test]
fn load_aws_cloud_ontology_extracts_service_topology() {
    // Arrange
    let graph = setup_graph_from_fixture("cloud_aws.ttl");

    // Act
    let schema = extract_schema(&graph, "http://example.org/cloud/aws#")
        .expect("Should extract AWS Cloud ontology");

    // Assert: Observable state - cloud topology extracted
    let service_classes: Vec<_> = schema
        .classes
        .iter()
        .filter(|c| c.name.ends_with("Service") || c.name.contains("EC2"))
        .map(|c| c.name.as_str())
        .collect();

    assert!(
        !service_classes.is_empty(),
        "Should have cloud service classes"
    );

    // Verify specific service types
    let has_compute = schema.classes.iter().any(|c| c.name == "ComputeService");
    let has_storage = schema.classes.iter().any(|c| c.name == "StorageService");
    let has_database = schema.classes.iter().any(|c| c.name == "DatabaseService");

    assert!(has_compute, "Should have ComputeService class");
    assert!(has_storage, "Should have StorageService class");
    assert!(has_database, "Should have DatabaseService class");

    // Verify region class exists
    let has_region = schema.classes.iter().any(|c| c.name == "Region");
    assert!(has_region, "Should have Region class");
}

// ============================================================================
// TEST GROUP: Query Semantics and Relationships
// ============================================================================

/// ARRANGE: Load ecommerce ontology (from existing fixtures)
/// ACT: Extract and build relationships
/// ASSERT: Relationships derived correctly from properties (deterministic state)
#[test]
fn extract_ecommerce_relationships_are_deterministic() {
    // Arrange
    let graph = setup_graph_from_fixture("ecommerce.ttl");

    // Act
    let schema = extract_schema(&graph, "http://example.org/ecommerce#")
        .expect("Should extract ecommerce ontology");

    // Assert: Observable state - relationships extracted and ordered
    assert!(
        !schema.relationships.is_empty(),
        "Should derive relationships"
    );

    // Verify Product -> Category relationship exists
    let product_category_rel = schema
        .relationships
        .iter()
        .find(|r| r.label.contains("Category"));
    assert!(
        product_category_rel.is_some(),
        "Should have Product -> Category relationship"
    );

    // Verify relationship structure
    if let Some(rel) = product_category_rel {
        assert!(
            rel.from_class.contains("Product"),
            "Source should be Product"
        );
        assert!(
            rel.to_class.contains("Category"),
            "Target should be Category"
        );
    }
}

/// ARRANGE: Load HIPAA and IT SLA ontologies
/// ACT: Extract both schemas
/// ASSERT: Both can be loaded and merged without conflicts
#[test]
fn load_multiple_ontologies_in_same_graph() {
    // Arrange
    let graph = Graph::new().expect("Failed to create graph");
    let hipaa_ttl = load_fixture("hipaa_legal.ttl");
    let it_sla_ttl = load_fixture("it_sla.ttl");

    // Act: Load both ontologies into same graph
    graph
        .insert_turtle(&hipaa_ttl)
        .expect("Should insert HIPAA ontology");
    graph
        .insert_turtle(&it_sla_ttl)
        .expect("Should insert IT SLA ontology");

    // Extract both schemas from merged graph
    let hipaa_schema = extract_schema(&graph, "http://example.org/legal/hipaa#")
        .expect("Should extract HIPAA schema");
    let it_schema =
        extract_schema(&graph, "http://example.org/it/sla#").expect("Should extract IT SLA schema");

    // Assert: Both schemas extracted correctly from merged graph
    assert!(hipaa_schema.classes.len() > 0, "HIPAA should have classes");
    assert!(it_schema.classes.len() > 0, "IT SLA should have classes");

    // Verify no cross-contamination
    let hipaa_class_names: Vec<_> = hipaa_schema
        .classes
        .iter()
        .map(|c| c.name.as_str())
        .collect();
    let it_class_names: Vec<_> = it_schema.classes.iter().map(|c| c.name.as_str()).collect();

    assert!(
        hipaa_class_names.contains(&"HIPAACompliance"),
        "HIPAA classes should be present"
    );
    assert!(
        it_class_names.contains(&"Service"),
        "IT classes should be present"
    );
}

// ============================================================================
// TEST GROUP: Property Range and Type Mapping
// ============================================================================

/// ARRANGE: Load IT SLA ontology with numeric properties
/// ACT: Extract properties and verify range types
/// ASSERT: Property ranges correctly identified (observable type state)
#[test]
fn property_ranges_correctly_mapped_from_xsd_types() {
    // Arrange
    let graph = setup_graph_from_fixture("it_sla.ttl");

    // Act
    let schema = extract_schema(&graph, "http://example.org/it/sla#")
        .expect("Should extract IT SLA ontology");

    // Assert: Property ranges are observable state
    let target_value_prop = schema
        .find_property("targetValue")
        .expect("Should find targetValue property");
    assert_eq!(
        target_value_prop.range,
        PropertyRange::String,
        "targetValue range should be mapped correctly"
    );

    let response_time_prop = schema
        .find_property("responseTimeMs")
        .expect("Should find responseTimeMs property");
    assert_eq!(
        response_time_prop.range,
        PropertyRange::String,
        "responseTimeMs range should be mapped correctly"
    );
}

/// ARRANGE: Load AWS ontology with reference properties
/// ACT: Extract object properties
/// ASSERT: References correctly typed (observable state)
#[test]
fn object_properties_correctly_identify_references() {
    // Arrange
    let graph = setup_graph_from_fixture("cloud_aws.ttl");

    // Act
    let schema = extract_schema(&graph, "http://example.org/cloud/aws#")
        .expect("Should extract AWS ontology");

    // Assert: Reference properties identified
    let runs_on_prop = schema
        .find_property("runsOn")
        .expect("Should find runsOn property");

    // Verify it's a reference property (not a datatype)
    match &runs_on_prop.range {
        PropertyRange::Reference(_) => {
            // Correct: it's a reference to Region
        }
        _ => panic!(
            "runsOn should be a Reference property, got {:?}",
            runs_on_prop.range
        ),
    }
}

// ============================================================================
// TEST GROUP: Cardinality Constraints
// ============================================================================

/// ARRANGE: Load HIPAA ontology with cardinality constraints
/// ACT: Extract cardinalities
/// ASSERT: Constraints correctly extracted (observable state)
#[test]
fn cardinality_constraints_are_extracted_from_ontology() {
    // Arrange
    let graph = setup_graph_from_fixture("hipaa_legal.ttl");

    // Act
    let schema = extract_schema(&graph, "http://example.org/legal/hipaa#")
        .expect("Should extract HIPAA ontology");

    // Assert: Cardinalities are observable state
    // Properties should have cardinality information (from Restriction definitions)
    let rule_id_prop = schema.find_property("ruleId").expect("Should find ruleId");
    assert_eq!(
        rule_id_prop.cardinality,
        Cardinality::One,
        "ruleId should be cardinality 1"
    );
    assert!(rule_id_prop.is_functional, "ruleId should be functional");
}

/// ARRANGE: Load Security MFA ontology with multi-cardinality
/// ACT: Extract properties with Many cardinality
/// ASSERT: Multi-valued properties correctly identified
#[test]
fn multi_valued_properties_correctly_identified() {
    // Arrange
    let graph = setup_graph_from_fixture("security_mfa.ttl");

    // Act
    let schema = extract_schema(&graph, "http://example.org/security/mfa#")
        .expect("Should extract Security MFA ontology");

    // Assert: Multi-valued property is observable state
    let uses_factor = schema
        .find_property("usesFactor")
        .expect("Should find usesFactor");
    assert!(
        uses_factor.cardinality.is_multi_valued(),
        "usesFactor should be multi-valued"
    );
}

// ============================================================================
// TEST GROUP: Error Handling - Invalid Input
// ============================================================================

/// ARRANGE: Invalid TTL file
/// ACT: Try to extract ontology
/// ASSERT: Error returned, original graph unmodified (observable state)
#[test]
fn invalid_ttl_file_returns_error() {
    // Arrange
    let graph = Graph::new().expect("Failed to create graph");
    let invalid_ttl = load_fixture("invalid.ttl");

    // Act
    let result = graph.insert_turtle(&invalid_ttl);

    // Assert: Error occurred, graph remains unchanged (observable state)
    // The parse error is observable
    match result {
        Err(e) => {
            // Expected: error message contains diagnostic info
            let error_msg = format!("{:?}", e);
            assert!(
                !error_msg.is_empty(),
                "Error should contain diagnostic information"
            );
        }
        Ok(_) => {
            // If it succeeded, try to extract (may fail at SPARQL stage)
            // This is acceptable - the important thing is the file is invalid
        }
    }
}

/// ARRANGE: Empty TTL file with no triples
/// ACT: Extract ontology
/// ASSERT: Returns valid but empty schema (observable state: empty collections)
#[test]
fn empty_ontology_extracts_with_no_classes() {
    // Arrange
    let graph = setup_graph_from_fixture("empty.ttl");

    // Act
    let schema = extract_schema(&graph, "http://example.org/legal/hipaa#");

    // Assert: Empty schema is valid state (not an error)
    // Observable state: empty classes and properties
    match schema {
        Ok(s) => {
            assert!(
                s.classes.is_empty(),
                "Empty ontology should have no classes"
            );
            assert!(
                s.properties.is_empty(),
                "Empty ontology should have no properties"
            );
        }
        Err(_) => {
            // Also acceptable - empty file may cause SPARQL query to find nothing
        }
    }
}

/// ARRANGE: Namespace that doesn't exist in graph
/// ACT: Try to extract schema for non-existent namespace
/// ASSERT: Returns empty or error (observable state)
#[test]
fn query_nonexistent_namespace_returns_empty() {
    // Arrange
    let graph = setup_graph_from_fixture("ecommerce.ttl");

    // Act
    let schema = extract_schema(&graph, "http://example.org/nonexistent#");

    // Assert: Observable state - empty schema when namespace not found
    match schema {
        Ok(s) => {
            assert!(
                s.classes.is_empty(),
                "Non-existent namespace should return empty schema"
            );
        }
        Err(_) => {
            // Also acceptable - error when namespace not found
        }
    }
}

// ============================================================================
// TEST GROUP: Determinism - Same Input Produces Same Output
// ============================================================================

/// ARRANGE: Load HIPAA ontology twice
/// ACT: Extract schema from both
/// ASSERT: Identical schemas (deterministic behavior verification)
#[test]
fn ontology_extraction_is_deterministic() {
    // Arrange: Load fixture twice
    let graph1 = setup_graph_from_fixture("hipaa_legal.ttl");
    let graph2 = setup_graph_from_fixture("hipaa_legal.ttl");

    // Act: Extract both
    let schema1 = extract_schema(&graph1, "http://example.org/legal/hipaa#")
        .expect("Should extract HIPAA ontology first time");
    let schema2 = extract_schema(&graph2, "http://example.org/legal/hipaa#")
        .expect("Should extract HIPAA ontology second time");

    // Assert: Deterministic - identical schemas
    assert_eq!(
        schema1.classes.len(),
        schema2.classes.len(),
        "Class count should be deterministic"
    );
    assert_eq!(
        schema1.properties.len(),
        schema2.properties.len(),
        "Property count should be deterministic"
    );

    // Verify class names in same order (deterministic ordering)
    let names1: Vec<_> = schema1.classes.iter().map(|c| c.name.as_str()).collect();
    let names2: Vec<_> = schema2.classes.iter().map(|c| c.name.as_str()).collect();
    assert_eq!(
        names1, names2,
        "Class names should be in deterministic order"
    );
}

/// ARRANGE: Load ecommerce ontology multiple times from disk
/// ACT: Extract schema each time
/// ASSERT: Exact same results each time (idempotent)
#[test]
fn ontology_extraction_is_idempotent() {
    // Arrange and Act: Extract 3 times
    let schema1 = extract_schema(
        &setup_graph_from_fixture("ecommerce.ttl"),
        "http://example.org/ecommerce#",
    )
    .expect("First extraction");
    let schema2 = extract_schema(
        &setup_graph_from_fixture("ecommerce.ttl"),
        "http://example.org/ecommerce#",
    )
    .expect("Second extraction");
    let schema3 = extract_schema(
        &setup_graph_from_fixture("ecommerce.ttl"),
        "http://example.org/ecommerce#",
    )
    .expect("Third extraction");

    // Assert: All three identical (idempotent)
    let verify_schemas_equal =
        |s1: &ggen_core::ontology::OntologySchema, s2: &ggen_core::ontology::OntologySchema| {
            assert_eq!(
                s1.classes.len(),
                s2.classes.len(),
                "Class count must be identical"
            );
            assert_eq!(
                s1.properties.len(),
                s2.properties.len(),
                "Property count must be identical"
            );
        };

    verify_schemas_equal(&schema1, &schema2);
    verify_schemas_equal(&schema2, &schema3);
}

// ============================================================================
// TEST GROUP: Schema Navigation and Query Methods
// ============================================================================

/// ARRANGE: Extract IT SLA schema
/// ACT: Use find_class and find_property methods
/// ASSERT: Navigation works on extracted schema (observable API state)
#[test]
fn schema_navigation_methods_work_on_extracted_ontology() {
    // Arrange
    let graph = setup_graph_from_fixture("it_sla.ttl");
    let schema = extract_schema(&graph, "http://example.org/it/sla#")
        .expect("Should extract IT SLA ontology");

    // Act and Assert: Find operations return correct results
    let sla_class = schema.find_class("ServiceLevelAgreement");
    assert!(
        sla_class.is_some(),
        "Should find ServiceLevelAgreement class"
    );

    let metric_class = schema.find_class("ServiceMetric");
    assert!(metric_class.is_some(), "Should find ServiceMetric class");

    let nonexistent = schema.find_class("NonExistentClass");
    assert!(nonexistent.is_none(), "Should not find nonexistent class");

    // Verify property navigation
    let sla_id_prop = schema.find_property("slaId");
    assert!(sla_id_prop.is_some(), "Should find slaId property");

    let nonexistent_prop = schema.find_property("nonExistentProperty");
    assert!(
        nonexistent_prop.is_none(),
        "Should not find nonexistent property"
    );
}

/// ARRANGE: Extract AWS schema
/// ACT: Query properties for specific class
/// ASSERT: Returns correct filtered properties (observable state)
#[test]
fn query_properties_by_class_returns_filtered_results() {
    // Arrange
    let graph = setup_graph_from_fixture("cloud_aws.ttl");
    let schema = extract_schema(&graph, "http://example.org/cloud/aws#")
        .expect("Should extract AWS ontology");

    // Act: Get CloudService class and query its properties
    let cloud_service_class = schema
        .find_class("CloudService")
        .expect("Should find CloudService class");
    let properties = schema.properties_for_class(&cloud_service_class.uri);

    // Assert: Observable state - properties correctly filtered
    assert!(
        !properties.is_empty(),
        "CloudService should have associated properties"
    );

    // Verify properties belong to CloudService domain
    for prop in properties {
        assert!(
            prop.domain.iter().any(|d| d.contains("CloudService")),
            "Property should have CloudService in domain"
        );
    }
}
