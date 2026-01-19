//! Chicago TDD Ontology Integration Tests
//!
//! Tests end-to-end ontology workflows combining multiple components.
//! Focus: Observable state changes across complete scenarios, real RDF data
//!
//! AAA Pattern: Arrange (load multiple ontologies) -> Act (query/extract/combine)
//!             -> Assert (unified state achieved)

use ggen_core::graph::Graph;
use ggen_core::ontology::OntologyExtractor;
use std::fs;
use std::path::Path;

/// Load RDF/Turtle fixture file
fn load_fixture(filename: &str) -> String {
    let fixture_path = Path::new("crates/ggen-core/tests/fixtures").join(filename);
    fs::read_to_string(fixture_path)
        .unwrap_or_else(|e| panic!("Failed to load fixture {}: {}", filename, e))
}

// ============================================================================
// SCENARIO 1: HIPAA Compliance Integration
// ============================================================================

/// ARRANGE: Load HIPAA legal ontology
/// ACT: Extract compliance framework and query regulations
/// ASSERT: Complete HIPAA compliance model extracted (observable state)
#[test]
fn scenario_load_hipaa_ontology_provides_compliance_model() {
    // Arrange: Load HIPAA ontology from fixture
    let graph = Graph::new().expect("Create graph");
    let hipaa_ttl = load_fixture("hipaa_legal.ttl");
    graph
        .insert_turtle(&hipaa_ttl)
        .expect("Insert HIPAA ontology");

    // Act: Extract HIPAA compliance schema
    let schema = OntologyExtractor::extract(&graph, "http://example.org/legal/hipaa#")
        .expect("Extract HIPAA schema");

    // Assert: Observable state - complete compliance model available
    assert!(
        !schema.classes.is_empty(),
        "HIPAA classes should be extracted"
    );
    assert!(
        !schema.properties.is_empty(),
        "HIPAA properties should be extracted"
    );

    // Verify core compliance classes exist
    let has_hipaa_compliance = schema.classes.iter().any(|c| c.name == "HIPAACompliance");
    let has_regulation = schema.classes.iter().any(|c| c.name == "RegulationRule");
    let has_phi = schema
        .classes
        .iter()
        .any(|c| c.name == "ProtectedHealthInformation");
    let has_access_control = schema.classes.iter().any(|c| c.name == "AccessControl");
    let has_encryption = schema.classes.iter().any(|c| c.name == "Encryption");
    let has_audit = schema.classes.iter().any(|c| c.name == "Audit");

    assert!(has_hipaa_compliance, "Must have HIPAACompliance class");
    assert!(has_regulation, "Must have RegulationRule class");
    assert!(has_phi, "Must have ProtectedHealthInformation class");
    assert!(has_access_control, "Must have AccessControl class");
    assert!(has_encryption, "Must have Encryption class");
    assert!(has_audit, "Must have Audit class");

    // Verify compliance relationships exist
    let has_regulation_rel = schema.properties.iter().any(|p| p.name == "hasRegulation");
    let has_access_control_rel = schema
        .properties
        .iter()
        .any(|p| p.name == "requiresAccessControl");
    let has_encryption_rel = schema
        .properties
        .iter()
        .any(|p| p.name == "requiresEncryption");

    assert!(has_regulation_rel, "Should have hasRegulation property");
    assert!(
        has_access_control_rel,
        "Should have requiresAccessControl property"
    );
    assert!(
        has_encryption_rel,
        "Should have requiresEncryption property"
    );
}

// ============================================================================
// SCENARIO 2: IT SLA Integration
// ============================================================================

/// ARRANGE: Load IT SLA ontology
/// ACT: Extract service metrics and thresholds
/// ASSERT: Complete SLA model with metrics extracted (observable state)
#[test]
fn scenario_load_it_sla_ontology_provides_service_metrics() {
    // Arrange: Load IT SLA ontology
    let graph = Graph::new().expect("Create graph");
    let sla_ttl = load_fixture("it_sla.ttl");
    graph
        .insert_turtle(&sla_ttl)
        .expect("Insert IT SLA ontology");

    // Act: Extract IT SLA schema
    let schema = OntologyExtractor::extract(&graph, "http://example.org/it/sla#")
        .expect("Extract IT SLA schema");

    // Assert: Observable state - SLA metrics model available
    assert!(
        !schema.classes.is_empty(),
        "IT SLA classes should be extracted"
    );

    // Verify SLA structure
    let has_sla = schema
        .classes
        .iter()
        .any(|c| c.name == "ServiceLevelAgreement");
    let has_service = schema.classes.iter().any(|c| c.name == "Service");
    let has_availability = schema.classes.iter().any(|c| c.name == "Availability");
    let has_response_time = schema.classes.iter().any(|c| c.name == "ResponseTime");
    let has_threshold = schema.classes.iter().any(|c| c.name == "Threshold");

    assert!(has_sla, "Must have ServiceLevelAgreement class");
    assert!(has_service, "Must have Service class");
    assert!(has_availability, "Must have Availability class");
    assert!(has_response_time, "Must have ResponseTime class");
    assert!(has_threshold, "Must have Threshold class");

    // Verify metric properties
    let has_availability_percentage = schema
        .properties
        .iter()
        .any(|p| p.name == "availabilityPercentage");
    let has_response_time_ms = schema.properties.iter().any(|p| p.name == "responseTimeMs");

    assert!(
        has_availability_percentage,
        "Should have availabilityPercentage property"
    );
    assert!(has_response_time_ms, "Should have responseTimeMs property");

    // Verify relationships
    let has_covers_service = schema.properties.iter().any(|p| p.name == "coversService");
    let has_defines_metric = schema.properties.iter().any(|p| p.name == "definesMetric");

    assert!(has_covers_service, "Should have coversService property");
    assert!(has_defines_metric, "Should have definesMetric property");
}

// ============================================================================
// SCENARIO 3: Security MFA Integration
// ============================================================================

/// ARRANGE: Load Security MFA ontology
/// ACT: Extract authentication hierarchy and factors
/// ASSERT: Complete MFA model with factors extracted (observable state)
#[test]
fn scenario_load_security_mfa_ontology_provides_auth_model() {
    // Arrange: Load Security MFA ontology
    let graph = Graph::new().expect("Create graph");
    let mfa_ttl = load_fixture("security_mfa.ttl");
    graph
        .insert_turtle(&mfa_ttl)
        .expect("Insert Security MFA ontology");

    // Act: Extract security schema
    let schema = OntologyExtractor::extract(&graph, "http://example.org/security/mfa#")
        .expect("Extract security schema");

    // Assert: Observable state - complete authentication model
    assert!(
        !schema.classes.is_empty(),
        "Security classes should be extracted"
    );

    // Verify authentication hierarchy
    let has_security_control = schema.classes.iter().any(|c| c.name == "SecurityControl");
    let has_auth_control = schema
        .classes
        .iter()
        .any(|c| c.name == "AuthenticationControl");
    let has_mfa = schema
        .classes
        .iter()
        .any(|c| c.name == "MultiFactorAuthentication");

    assert!(has_security_control, "Must have SecurityControl base class");
    assert!(has_auth_control, "Must have AuthenticationControl class");
    assert!(has_mfa, "Must have MultiFactorAuthentication class");

    // Verify factor types
    let has_knowledge = schema.classes.iter().any(|c| c.name == "KnowledgeFactor");
    let has_possession = schema.classes.iter().any(|c| c.name == "PossessionFactor");
    let has_biometric = schema.classes.iter().any(|c| c.name == "BiometricFactor");

    assert!(has_knowledge, "Must have KnowledgeFactor class");
    assert!(has_possession, "Must have PossessionFactor class");
    assert!(has_biometric, "Must have BiometricFactor class");

    // Verify role class
    let has_role = schema.classes.iter().any(|c| c.name == "Role");
    assert!(has_role, "Must have Role class");

    // Verify MFA-specific properties
    let has_uses_factor = schema.properties.iter().any(|p| p.name == "usesFactor");
    let has_requires_mfa = schema.properties.iter().any(|p| p.name == "requiresMFA");
    let has_required_factor_count = schema
        .properties
        .iter()
        .any(|p| p.name == "requiredFactorCount");

    assert!(has_uses_factor, "Should have usesFactor property");
    assert!(has_requires_mfa, "Should have requiresMFA property");
    assert!(
        has_required_factor_count,
        "Should have requiredFactorCount property"
    );
}

// ============================================================================
// SCENARIO 4: Cloud AWS Integration
// ============================================================================

/// ARRANGE: Load AWS Cloud ontology
/// ACT: Extract cloud services and regional topology
/// ASSERT: Complete cloud infrastructure model extracted (observable state)
#[test]
fn scenario_load_aws_cloud_ontology_provides_infrastructure_model() {
    // Arrange: Load AWS Cloud ontology
    let graph = Graph::new().expect("Create graph");
    let aws_ttl = load_fixture("cloud_aws.ttl");
    graph
        .insert_turtle(&aws_ttl)
        .expect("Insert AWS Cloud ontology");

    // Act: Extract AWS schema
    let schema = OntologyExtractor::extract(&graph, "http://example.org/cloud/aws#")
        .expect("Extract AWS schema");

    // Assert: Observable state - complete cloud infrastructure model
    assert!(
        !schema.classes.is_empty(),
        "Cloud classes should be extracted"
    );

    // Verify cloud service base class
    let has_cloud_service = schema.classes.iter().any(|c| c.name == "CloudService");
    assert!(has_cloud_service, "Must have CloudService base class");

    // Verify compute services
    let has_compute = schema.classes.iter().any(|c| c.name == "ComputeService");
    let has_ec2 = schema.classes.iter().any(|c| c.name == "EC2Instance");
    assert!(has_compute, "Must have ComputeService class");
    assert!(has_ec2, "Must have EC2Instance class");

    // Verify storage services
    let has_storage = schema.classes.iter().any(|c| c.name == "StorageService");
    let has_s3 = schema.classes.iter().any(|c| c.name == "S3Bucket");
    assert!(has_storage, "Must have StorageService class");
    assert!(has_s3, "Must have S3Bucket class");

    // Verify database services
    let has_database = schema.classes.iter().any(|c| c.name == "DatabaseService");
    let has_rds = schema.classes.iter().any(|c| c.name == "RDSDatabase");
    assert!(has_database, "Must have DatabaseService class");
    assert!(has_rds, "Must have RDSDatabase class");

    // Verify security services
    let has_security = schema.classes.iter().any(|c| c.name == "SecurityService");
    let has_iam = schema.classes.iter().any(|c| c.name == "IAMRole");
    assert!(has_security, "Must have SecurityService class");
    assert!(has_iam, "Must have IAMRole class");

    // Verify regional structure
    let has_region = schema.classes.iter().any(|c| c.name == "Region");
    assert!(has_region, "Must have Region class");

    // Verify cloud relationships
    let has_runs_on = schema.properties.iter().any(|p| p.name == "runsOn");
    let has_uses_security = schema.properties.iter().any(|p| p.name == "usesSecurity");
    let has_assumes_role = schema.properties.iter().any(|p| p.name == "assumesRole");

    assert!(has_runs_on, "Should have runsOn property");
    assert!(has_uses_security, "Should have usesSecurity property");
    assert!(has_assumes_role, "Should have assumesRole property");
}

// ============================================================================
// SCENARIO 5: Multi-Ontology Integration (HIPAA + IT SLA + Security + Cloud)
// ============================================================================

/// ARRANGE: Load all 4 ontologies (HIPAA, IT SLA, Security MFA, AWS Cloud)
/// ACT: Extract all schemas and verify they coexist in merged graph
/// ASSERT: All 4 ontologies loadable, extractable, without cross-contamination
#[test]
fn scenario_load_all_ontologies_enables_unified_compliance_framework() {
    // Arrange: Create graph and load all ontologies
    let graph = Graph::new().expect("Create graph");

    let hipaa_ttl = load_fixture("hipaa_legal.ttl");
    let sla_ttl = load_fixture("it_sla.ttl");
    let security_ttl = load_fixture("security_mfa.ttl");
    let aws_ttl = load_fixture("cloud_aws.ttl");
    let ecommerce_ttl = load_fixture("ecommerce.ttl");

    // Insert all ontologies into merged graph
    graph
        .insert_turtle(&hipaa_ttl)
        .expect("Insert HIPAA ontology");
    graph
        .insert_turtle(&sla_ttl)
        .expect("Insert IT SLA ontology");
    graph
        .insert_turtle(&security_ttl)
        .expect("Insert Security MFA ontology");
    graph
        .insert_turtle(&aws_ttl)
        .expect("Insert AWS Cloud ontology");
    graph
        .insert_turtle(&ecommerce_ttl)
        .expect("Insert E-commerce ontology");

    // Act: Extract all schemas from merged graph
    let hipaa_schema = OntologyExtractor::extract(&graph, "http://example.org/legal/hipaa#")
        .expect("Extract HIPAA schema");
    let sla_schema = OntologyExtractor::extract(&graph, "http://example.org/it/sla#")
        .expect("Extract IT SLA schema");
    let security_schema = OntologyExtractor::extract(&graph, "http://example.org/security/mfa#")
        .expect("Extract Security schema");
    let aws_schema = OntologyExtractor::extract(&graph, "http://example.org/cloud/aws#")
        .expect("Extract AWS schema");
    let ecommerce_schema = OntologyExtractor::extract(&graph, "http://example.org/ecommerce#")
        .expect("Extract E-commerce schema");

    // Assert: Observable state - all schemas extracted correctly
    assert!(
        !hipaa_schema.classes.is_empty(),
        "HIPAA schema should have classes"
    );
    assert!(
        !sla_schema.classes.is_empty(),
        "IT SLA schema should have classes"
    );
    assert!(
        !security_schema.classes.is_empty(),
        "Security schema should have classes"
    );
    assert!(
        !aws_schema.classes.is_empty(),
        "AWS schema should have classes"
    );
    assert!(
        !ecommerce_schema.classes.is_empty(),
        "E-commerce schema should have classes"
    );

    // Verify no cross-contamination between ontologies
    let hipaa_class_names: Vec<_> = hipaa_schema
        .classes
        .iter()
        .map(|c| c.name.as_str())
        .collect();
    let sla_class_names: Vec<_> = sla_schema.classes.iter().map(|c| c.name.as_str()).collect();
    let security_class_names: Vec<_> = security_schema
        .classes
        .iter()
        .map(|c| c.name.as_str())
        .collect();
    let aws_class_names: Vec<_> = aws_schema.classes.iter().map(|c| c.name.as_str()).collect();

    // HIPAA-specific classes should only appear in HIPAA schema
    assert!(hipaa_class_names.contains(&"HIPAACompliance"));
    assert!(
        !sla_class_names.contains(&"HIPAACompliance"),
        "SLA should not have HIPAA classes"
    );
    assert!(
        !security_class_names.contains(&"HIPAACompliance"),
        "Security should not have HIPAA classes"
    );
    assert!(
        !aws_class_names.contains(&"HIPAACompliance"),
        "AWS should not have HIPAA classes"
    );

    // SLA-specific classes should only appear in SLA schema
    assert!(sla_class_names.contains(&"Availability"));
    assert!(
        !hipaa_class_names.contains(&"Availability"),
        "HIPAA should not have SLA classes"
    );
    assert!(
        !aws_class_names.contains(&"Availability"),
        "AWS should not have SLA classes"
    );

    // Security-specific classes should only appear in Security schema
    assert!(security_class_names.contains(&"MultiFactorAuthentication"));
    assert!(
        !hipaa_class_names.contains(&"MultiFactorAuthentication"),
        "HIPAA should not have Security classes"
    );
    assert!(
        !sla_class_names.contains(&"MultiFactorAuthentication"),
        "SLA should not have Security classes"
    );

    // AWS-specific classes should only appear in AWS schema
    assert!(aws_class_names.contains(&"EC2Instance"));
    assert!(
        !hipaa_class_names.contains(&"EC2Instance"),
        "HIPAA should not have AWS classes"
    );
    assert!(
        !sla_class_names.contains(&"EC2Instance"),
        "SLA should not have AWS classes"
    );
    assert!(
        !security_class_names.contains(&"EC2Instance"),
        "Security should not have AWS classes"
    );
}

// ============================================================================
// SCENARIO 6: Complete HIPAA+IT+Security+Cloud Compliance Mapping
// ============================================================================

/// ARRANGE: Load all 4 domain ontologies
/// ACT: Verify cross-domain relationships can be traced
/// ASSERT: Unified compliance mapping achievable (observable end-to-end)
#[test]
fn scenario_unified_compliance_framework_connects_all_domains() {
    // Arrange: Load all ontologies
    let graph = Graph::new().expect("Create graph");

    graph
        .insert_turtle(&load_fixture("hipaa_legal.ttl"))
        .expect("Insert HIPAA");
    graph
        .insert_turtle(&load_fixture("it_sla.ttl"))
        .expect("Insert IT SLA");
    graph
        .insert_turtle(&load_fixture("security_mfa.ttl"))
        .expect("Insert Security");
    graph
        .insert_turtle(&load_fixture("cloud_aws.ttl"))
        .expect("Insert AWS");

    // Act: Extract all schemas
    let hipaa_schema = OntologyExtractor::extract(&graph, "http://example.org/legal/hipaa#")
        .expect("Extract HIPAA");
    let sla_schema =
        OntologyExtractor::extract(&graph, "http://example.org/it/sla#").expect("Extract IT SLA");
    let security_schema = OntologyExtractor::extract(&graph, "http://example.org/security/mfa#")
        .expect("Extract Security");
    let aws_schema =
        OntologyExtractor::extract(&graph, "http://example.org/cloud/aws#").expect("Extract AWS");

    // Assert: Observable end-to-end mapping structure
    // Verify HIPAA -> IT SLA connection (HIPAA policy requires specific SLAs)
    let hipaa_has_regulation = hipaa_schema
        .properties
        .iter()
        .any(|p| p.name == "hasRegulation");
    let sla_covers_service = sla_schema
        .properties
        .iter()
        .any(|p| p.name == "coversService");
    assert!(
        hipaa_has_regulation && sla_covers_service,
        "Should be able to map HIPAA regulations to IT SLAs"
    );

    // Verify IT SLA -> Security connection (SLAs require security controls)
    let sla_defines_metric = sla_schema
        .properties
        .iter()
        .any(|p| p.name == "definesMetric");
    let security_requires_mfa = security_schema
        .properties
        .iter()
        .any(|p| p.name == "requiresMFA");
    assert!(
        sla_defines_metric && security_requires_mfa,
        "Should be able to map IT metrics to security controls"
    );

    // Verify Security -> Cloud connection (Security roles run on cloud)
    let security_applies_to_role = security_schema
        .properties
        .iter()
        .any(|p| p.name == "appliesTo");
    let aws_uses_security = aws_schema
        .properties
        .iter()
        .any(|p| p.name == "usesSecurity");
    assert!(
        security_applies_to_role && aws_uses_security,
        "Should be able to map security roles to AWS resources"
    );

    // Verify complete chain: HIPAA -> IT SLA -> Security -> Cloud
    // Each domain has the necessary properties to link to next domain
    let has_complete_chain = hipaa_has_regulation &&     // HIPAA defines regulations
        sla_covers_service &&        // IT SLA covers services
        security_requires_mfa &&     // Security defines controls
        aws_uses_security; // Cloud implements controls

    assert!(
        has_complete_chain,
        "Complete compliance chain should be traceable"
    );
}

// ============================================================================
// SCENARIO 7: Determinism and Repeatability
// ============================================================================

/// ARRANGE: Load same ontologies multiple times
/// ACT: Extract schemas each time
/// ASSERT: Identical results every time (deterministic extraction)
#[test]
fn scenario_ontology_extraction_is_deterministic_across_multiple_loads() {
    // Arrange and Act: Load and extract 3 times
    let extract_hipaa = |i| {
        let graph = Graph::new().expect("Create graph");
        let ttl = load_fixture("hipaa_legal.ttl");
        graph.insert_turtle(&ttl).expect("Insert TTL");
        OntologyExtractor::extract(&graph, "http://example.org/legal/hipaa#")
            .expect(&format!("Extract HIPAA ontology iteration {}", i))
    };

    let schema1 = extract_hipaa(1);
    let schema2 = extract_hipaa(2);
    let schema3 = extract_hipaa(3);

    // Assert: Deterministic - all extractions identical
    assert_eq!(
        schema1.classes.len(),
        schema2.classes.len(),
        "Class count should be identical across extractions"
    );
    assert_eq!(
        schema2.classes.len(),
        schema3.classes.len(),
        "Class count should be identical across extractions"
    );

    assert_eq!(
        schema1.properties.len(),
        schema2.properties.len(),
        "Property count should be identical across extractions"
    );
    assert_eq!(
        schema2.properties.len(),
        schema3.properties.len(),
        "Property count should be identical across extractions"
    );

    // Verify class ordering is deterministic
    let names1: Vec<_> = schema1.classes.iter().map(|c| c.name.as_str()).collect();
    let names2: Vec<_> = schema2.classes.iter().map(|c| c.name.as_str()).collect();
    let names3: Vec<_> = schema3.classes.iter().map(|c| c.name.as_str()).collect();

    assert_eq!(names1, names2, "Class names should be in same order");
    assert_eq!(names2, names3, "Class names should be in same order");
}

// ============================================================================
// SCENARIO 8: Error Resilience
// ============================================================================

/// ARRANGE: Load valid ontologies and attempt invalid namespace
/// ACT: Try to extract from non-existent namespace
/// ASSERT: Returns empty or error without corrupting graph (resilience)
#[test]
fn scenario_invalid_namespace_does_not_corrupt_graph_state() {
    // Arrange: Load real ontology
    let graph = Graph::new().expect("Create graph");
    let hipaa_ttl = load_fixture("hipaa_legal.ttl");
    graph
        .insert_turtle(&hipaa_ttl)
        .expect("Insert HIPAA ontology");

    // Act: Try to extract from non-existent namespace
    let invalid_result = OntologyExtractor::extract(&graph, "http://example.org/nonexistent#");

    // Assert: Graph is still functional
    match invalid_result {
        Ok(schema) => {
            // If it succeeds with empty schema, that's OK
            assert!(
                schema.classes.is_empty(),
                "Non-existent namespace should have no classes"
            );
        }
        Err(_) => {
            // Error is also acceptable
        }
    }

    // Verify graph is still usable - extract real namespace after error
    let hipaa_schema = OntologyExtractor::extract(&graph, "http://example.org/legal/hipaa#")
        .expect("Should still extract real namespace after failed extraction");

    assert!(
        !hipaa_schema.classes.is_empty(),
        "Graph should still be valid after failed extraction attempt"
    );
}
