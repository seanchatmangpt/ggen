//! Security tests for SPARQL injection prevention
//!
//! This test suite verifies that the validation module prevents SPARQL injection attacks
//! by testing various attack patterns against the URI and property name validators.

use ggen_ai::codegen::validation::{validate_rdf_uri, validate_property_name, validate_prefixed_iri};

#[test]
fn test_valid_http_uri() {
    assert!(validate_rdf_uri("http://example.com/ontology#MyClass").is_ok());
}

#[test]
fn test_valid_https_uri() {
    assert!(validate_rdf_uri("https://www.w3.org/2000/01/rdf-schema#Class").is_ok());
}

#[test]
fn test_valid_urn_uri() {
    assert!(validate_rdf_uri("urn:uuid:550e8400-e29b-41d4-a716-446655440000").is_ok());
}

// SQL/SPARQL injection attack tests
#[test]
fn test_uri_blocks_sql_injection_with_semicolon() {
    let result = validate_rdf_uri("http://example.com/'; DROP TABLE properties--");
    assert!(result.is_err(), "Should reject SQL injection pattern with semicolon");
}

#[test]
fn test_uri_blocks_sparql_comment_injection() {
    let result = validate_rdf_uri("http://example.com/ontology#Class--\nUNION SELECT");
    assert!(result.is_err(), "Should reject SPARQL comment injection pattern");
}

#[test]
fn test_uri_blocks_javascript_xss() {
    let result = validate_rdf_uri("javascript:alert('xss')");
    assert!(result.is_err(), "Should reject javascript: scheme");
}

#[test]
fn test_uri_blocks_control_characters() {
    let result = validate_rdf_uri("http://example.com/ontology#Bad\x00Name");
    assert!(result.is_err(), "Should reject control characters");
}

#[test]
fn test_uri_rejects_empty_string() {
    let result = validate_rdf_uri("");
    assert!(result.is_err(), "Should reject empty URI");
}

#[test]
fn test_uri_rejects_no_scheme() {
    let result = validate_rdf_uri("example.com/ontology");
    assert!(result.is_err(), "Should reject URI without scheme");
}

#[test]
fn test_uri_rejects_oversized_input() {
    let long_uri = format!("http://example.com/{}", "x".repeat(5000));
    let result = validate_rdf_uri(&long_uri);
    assert!(result.is_err(), "Should reject excessively long URIs");
}

// Property name injection tests
#[test]
fn test_property_valid_simple() {
    assert!(validate_property_name("outputField").is_ok());
}

#[test]
fn test_property_valid_with_prefix() {
    assert!(validate_property_name("sh:path").is_ok());
}

#[test]
fn test_property_valid_full_iri() {
    assert!(validate_property_name("http://www.w3.org/ns/shacl#path").is_ok());
}

#[test]
fn test_property_blocks_sparql_union_injection() {
    let result = validate_property_name("prop UNION SELECT * WHERE");
    assert!(result.is_err(), "Should reject SPARQL UNION injection");
}

#[test]
fn test_property_blocks_optional_manipulation() {
    let result = validate_property_name("prop OPTIONAL { ?s ?p ?o }");
    assert!(result.is_err(), "Should reject SPARQL OPTIONAL manipulation");
}

#[test]
fn test_property_blocks_quote_escape() {
    let result = validate_property_name("prop\"; DROP TABLE--");
    assert!(result.is_err(), "Should reject quote-based attacks");
}

#[test]
fn test_property_blocks_sql_comment() {
    let result = validate_property_name("prop--COMMENT");
    assert!(result.is_err(), "Should reject SQL comment patterns");
}

#[test]
fn test_property_rejects_empty() {
    let result = validate_property_name("");
    assert!(result.is_err(), "Should reject empty property");
}

#[test]
fn test_property_rejects_oversized() {
    let long_prop = "a".repeat(300);
    let result = validate_property_name(&long_prop);
    assert!(result.is_err(), "Should reject excessively long properties");
}

#[test]
fn test_property_rejects_control_characters() {
    let result = validate_property_name("prop\x00name");
    assert!(result.is_err(), "Should reject control characters");
}

// Prefixed IRI tests
#[test]
fn test_prefixed_iri_valid() {
    assert!(validate_prefixed_iri("sh:path").is_ok());
}

#[test]
fn test_prefixed_iri_valid_complex() {
    assert!(validate_prefixed_iri("rdf:type").is_ok());
}

#[test]
fn test_prefixed_iri_rejects_no_separator() {
    let result = validate_prefixed_iri("shpath");
    assert!(result.is_err(), "Should reject prefixed IRI without separator");
}

#[test]
fn test_prefixed_iri_rejects_empty_prefix() {
    let result = validate_prefixed_iri(":path");
    assert!(result.is_err(), "Should reject prefixed IRI with empty prefix");
}

#[test]
fn test_prefixed_iri_rejects_empty_local() {
    let result = validate_prefixed_iri("sh:");
    assert!(result.is_err(), "Should reject prefixed IRI with empty local name");
}

// Defense-in-depth tests
#[test]
fn test_injection_variants_blocked() {
    let injection_attempts = vec![
        "http://example.com/'; DELETE FROM--",
        "http://example.com/\"; DROP TABLE--",
        "http://example.com/' UNION SELECT--",
        "http://example.com/; EXEC sp_",
        "http://example.com/\" OR \"1\"=\"1",
    ];

    for attempt in injection_attempts {
        let result = validate_rdf_uri(attempt);
        assert!(
            result.is_err(),
            "Should reject injection attempt: {}",
            attempt
        );
    }
}

#[test]
fn test_valid_uris_pass() {
    let valid_uris = vec![
        "http://example.com/ontology#MyClass",
        "https://www.w3.org/ns/shacl#NodeShape",
        "urn:uuid:550e8400-e29b-41d4-a716-446655440000",
        "http://purl.org/dc/terms/title",
        "ftp://example.com/resource",
    ];

    for uri in valid_uris {
        let result = validate_rdf_uri(uri);
        assert!(result.is_ok(), "Should accept valid URI: {}", uri);
    }
}

#[test]
fn test_security_errors_are_descriptive() {
    let result = validate_rdf_uri("http://example.com/'; DROP TABLE--");
    match result {
        Err(e) => {
            let error_msg = e.to_string();
            assert!(
                error_msg.contains("injection") || error_msg.contains("forbidden"),
                "Error message should indicate security concern: {}",
                error_msg
            );
        }
        Ok(_) => panic!("Should have rejected injection attempt"),
    }
}
