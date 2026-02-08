//! Integration tests for ontology handling layer
//!
//! Tests the complete flow of loading ontologies, querying them,
//! and mapping entities to standard ontology classes.

use ggen_ontology_core::{
    entity_mapper::EntityMapper, sparql_generator::SparqlGenerator, triple_store::TripleStore,
    validators::validate_turtle,
};
use std::io::Write;
use tempfile::NamedTempFile;

/// Sample Turtle ontology for testing
fn create_sample_ontology() -> NamedTempFile {
    let mut file = NamedTempFile::new().unwrap();
    let content = r#"
@prefix ex: <http://example.com/> .
@prefix rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#> .
@prefix rdfs: <http://www.w3.org/2000/01/rdf-schema#> .

# Policy examples
ex:Privacy-Policy-2024
    rdf:type ex:PrivacyPolicy ;
    rdfs:label "Privacy Policy 2024" ;
    rdfs:comment "Main privacy policy for data protection" ;
    ex:hasJurisdiction ex:Jurisdiction-US .

ex:Security-Policy-2024
    rdf:type ex:SecurityPolicy ;
    rdfs:label "Security Policy 2024" ;
    ex:hasJurisdiction ex:Jurisdiction-US .

# Jurisdiction
ex:Jurisdiction-US
    rdf:type ex:Jurisdiction ;
    ex:code "US" .

# Data classifications
ex:Public-Data
    rdf:type ex:DataClassification ;
    rdfs:label "Public" ;
    ex:classificationLevel 1 .

ex:Confidential-Data
    rdf:type ex:DataClassification ;
    rdfs:label "Confidential" ;
    ex:classificationLevel 3 .

# Services
ex:Service-API
    rdf:type ex:ServiceLevelAgreement ;
    rdfs:label "API Service" ;
    ex:availabilityPercentage 99.95 .

# Security controls
ex:MFA-Control
    rdf:type ex:SecurityControl ;
    rdfs:label "Multi-Factor Authentication" ;
    ex:controlType "Authentication" .

# Compute services
ex:Kubernetes-Cluster-1
    rdf:type ex:ComputeService ;
    rdfs:label "Production K8s Cluster" ;
    ex:computeType "Kubernetes" ;
    ex:provider "AWS" ;
    ex:deploymentRegion "us-east-1" .
"#;
    file.write_all(content.as_bytes()).unwrap();
    file.flush().unwrap();
    file
}

#[test]
fn test_load_and_query_ontology() {
    let ontology = create_sample_ontology();

    // Load ontology
    let store = TripleStore::new().expect("Failed to create triple store");
    store
        .load_turtle(ontology.path())
        .expect("Failed to load turtle");

    // Verify store has triples
    let count = store.triple_count().expect("Failed to get triple count");
    assert!(count > 0, "Triple store should have loaded triples");
}

#[test]
fn test_sparql_query_determinism() {
    let query1 = SparqlGenerator::find_policies_by_jurisdiction("US");
    let query2 = SparqlGenerator::find_policies_by_jurisdiction("US");

    assert_eq!(
        query1, query2,
        "Same parameters should produce identical queries"
    );
}

#[test]
fn test_sparql_queries_different_for_different_input() {
    let query_us = SparqlGenerator::find_policies_by_jurisdiction("US");
    let query_eu = SparqlGenerator::find_policies_by_jurisdiction("EU");

    assert_ne!(
        query_us, query_eu,
        "Different parameters should produce different queries"
    );
    assert!(query_us.contains("US"), "US query should contain US");
    assert!(query_eu.contains("EU"), "EU query should contain EU");
}

#[test]
fn test_entity_mapper_policy_determinism() {
    let result1 = EntityMapper::match_policy("Privacy Policy").expect("Failed to match policy");
    let result2 = EntityMapper::match_policy("Privacy Policy").expect("Failed to match policy");

    assert_eq!(
        result1, result2,
        "Same policy should produce identical matches"
    );
}

#[test]
fn test_entity_mapper_data_classification() {
    let public_matches = EntityMapper::match_data_classification("Public")
        .expect("Failed to match public classification");

    assert!(!public_matches.is_empty(), "Should have matches for Public");
    assert_eq!(
        public_matches[0].score, 1.0,
        "Exact match should have score 1.0"
    );

    let confidential_matches = EntityMapper::match_data_classification("Confidential")
        .expect("Failed to match confidential");

    assert!(
        !confidential_matches.is_empty(),
        "Should have matches for Confidential"
    );
    assert!(
        confidential_matches[0].score >= 0.9,
        "Confident match expected"
    );
}

#[test]
fn test_entity_mapper_service_levels() {
    // Critical SLA
    let critical = EntityMapper::match_service_level(99.99).expect("Failed to match critical SLA");
    assert_eq!(critical[0].class, ":CriticalService");

    // High availability
    let high = EntityMapper::match_service_level(99.9).expect("Failed to match high availability");
    assert_eq!(high[0].class, ":HighAvailabilityService");

    // Standard
    let standard = EntityMapper::match_service_level(99.0).expect("Failed to match standard");
    assert_eq!(standard[0].class, ":StandardService");
}

#[test]
fn test_entity_mapper_security_controls() {
    let mfa_matches = EntityMapper::match_security_control("MFA").expect("Failed to match MFA");

    assert!(!mfa_matches.is_empty(), "Should have matches for MFA");
    assert_eq!(mfa_matches[0].class, ":MultiFactorAuthentication");
    assert_eq!(
        mfa_matches[0].score, 0.98,
        "MFA should have high confidence"
    );
}

#[test]
fn test_entity_mapper_compute_services() {
    // VM
    let vm_matches =
        EntityMapper::match_compute_service("Virtual Machine").expect("Failed to match VM");
    assert_eq!(vm_matches[0].class, ":VirtualMachine");

    // Container
    let container_matches =
        EntityMapper::match_compute_service("Docker").expect("Failed to match container");
    assert!(!container_matches.is_empty());

    // Kubernetes
    let k8s_matches =
        EntityMapper::match_compute_service("Kubernetes").expect("Failed to match K8s");
    assert_eq!(k8s_matches[0].class, ":KubernetesCluster");
}

#[test]
fn test_validate_turtle_file() {
    let ontology = create_sample_ontology();

    let report = validate_turtle(ontology.path()).expect("Failed to validate");
    assert!(report.is_valid, "Sample ontology should be valid");
    assert!(
        report.errors.is_empty(),
        "Valid ontology should have no errors"
    );
}

#[test]
fn test_invalid_turtle_validation() {
    let mut file = NamedTempFile::new().unwrap();
    let content = "this is not valid turtle !!!";
    file.write_all(content.as_bytes()).unwrap();
    file.flush().unwrap();

    let report = validate_turtle(file.path()).expect("Failed to validate");
    assert!(!report.is_valid, "Invalid turtle should fail validation");
    assert!(!report.errors.is_empty(), "Should have error messages");
}

#[test]
fn test_sparql_generator_filters() {
    let filters = vec![
        ("availability".to_string(), "?avail > 99".to_string()),
        ("cost".to_string(), "?cost < 1000".to_string()),
    ];

    let query1 = SparqlGenerator::select_with_filters(&["?label"], "Service", &filters);
    let query2 = SparqlGenerator::select_with_filters(&["?label"], "Service", &filters);

    assert_eq!(query1, query2, "Filter queries should be deterministic");
    assert!(query1.contains("SELECT"), "Should have SELECT clause");
}

#[test]
fn test_triple_store_is_empty() {
    let store = TripleStore::new().expect("Failed to create store");

    let is_empty = store.is_empty().expect("Failed to check if empty");
    assert!(is_empty, "New store should be empty");
}

#[test]
fn test_triple_store_state_verification() {
    let ontology = create_sample_ontology();

    let store = TripleStore::new().expect("Failed to create store");
    let initial_empty = store.is_empty().expect("Failed to check initial state");
    assert!(initial_empty, "Store should start empty");

    // Load ontology
    store.load_turtle(ontology.path()).expect("Failed to load");

    let after_load_empty = store.is_empty().expect("Failed to check after load");
    assert!(!after_load_empty, "Store should have triples after load");

    let count = store.triple_count().expect("Failed to get count");
    assert!(count > 0, "Count should be positive");
}

#[test]
fn test_entity_mapper_scores_sorted() {
    let matches = EntityMapper::match_policy("Privacy GDPR Encryption").expect("Failed to match");

    // Verify sorted by score descending
    for i in 0..matches.len() - 1 {
        assert!(
            matches[i].score >= matches[i + 1].score,
            "Matches should be sorted by score descending"
        );
    }
}

#[test]
fn test_entity_mapper_multiple_keywords() {
    let policy = "Privacy Policy with Encryption and Security Controls";
    let matches = EntityMapper::match_policy(policy).expect("Failed to match");

    // Should have multiple matches for a comprehensive policy description
    assert!(matches.len() > 0, "Should match privacy keywords");

    // Should be sorted by score
    for i in 0..matches.len() - 1 {
        assert!(matches[i].score >= matches[i + 1].score);
    }
}
