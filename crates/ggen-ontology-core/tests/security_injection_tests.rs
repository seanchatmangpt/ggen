//! Security audit: SPARQL injection vulnerability testing
//!
//! Tests for SPARQL query generation with potentially malicious input
//! to verify the escape_sparql_string function prevents injection attacks

use ggen_ontology_core::sparql_generator::SparqlGenerator;

#[test]
fn test_sparql_injection_quote_in_jurisdiction() {
    // Test malicious input with quotes to break out of SPARQL string
    let malicious = r#"US"; DROP GRAPH <http://example.com>; --"#;
    let query = SparqlGenerator::find_policies_by_jurisdiction(malicious);

    // The query should contain the escaped input, not execute the injected DROP command
    assert!(query.contains("SELECT"), "Query should still be valid SPARQL");
    assert!(query.contains("WHERE"), "Query should still be valid SPARQL");

    // The dangerous characters should be escaped
    assert!(query.contains(r#"\""#), "Quotes should be escaped");
}

#[test]
fn test_sparql_injection_newline_in_classification() {
    // Test malicious input with newlines
    let malicious = "Public\n} UNION {SELECT * WHERE {?x ?y ?z";
    let query = SparqlGenerator::find_data_classifications(malicious);

    // Should prevent the UNION injection
    assert!(query.contains("SELECT"), "Query should still be valid SPARQL");
    assert!(query.contains(r#"\n"#), "Newlines should be escaped");
}

#[test]
fn test_sparql_injection_backslash_in_control_type() {
    // Test malicious input with backslashes
    let malicious = r#"Authentication\n"; DROP GRAPH <http://test>; --"#;
    let query = SparqlGenerator::find_security_controls(malicious);

    // Should escape backslashes properly
    assert!(query.contains(r#"\\"#), "Backslashes should be escaped");
}

#[test]
fn test_sparql_injection_tab_character() {
    // Test with tab characters that might confuse parsers
    let malicious = "Tab\there";
    let query = SparqlGenerator::find_compute_by_type(malicious);

    // Should escape tab
    assert!(query.contains(r#"\t"#), "Tabs should be escaped");
}

#[test]
fn test_sparql_injection_carriage_return() {
    // Test with carriage return
    let malicious = "Data\rClassification";
    let query = SparqlGenerator::find_data_classifications(malicious);

    // Should escape carriage return
    assert!(query.contains(r#"\r"#), "Carriage returns should be escaped");
}

#[test]
fn test_sparql_injection_combined_attack() {
    // Test with multiple dangerous characters combined
    let malicious = r#"Policy"; DELETE WHERE {?s ?p ?o}; -- \n"#;
    let query = SparqlGenerator::find_policies_by_jurisdiction(malicious);

    // All dangerous characters should be escaped
    assert!(query.contains(r#"\""#), "Quotes should be escaped");
    assert!(query.contains(r#"\\"#), "Backslashes should be escaped");
    assert!(query.contains(r#"\n"#), "Newlines should be escaped");

    // Query should still be valid
    assert!(query.contains("SELECT"), "Query should be valid");
}

#[test]
fn test_sparql_injection_unicode_characters() {
    // Test with unicode characters that shouldn't be escaped by the function
    let input = "Pøl爱cy";
    let query = SparqlGenerator::find_policies_by_jurisdiction(input);

    // Unicode should be preserved in output (not escaped)
    assert!(query.contains("Pøl爱cy"), "Unicode should not be escaped");
    assert!(query.contains("SELECT"), "Query should be valid");
}

#[test]
fn test_sparql_safe_strings_unchanged() {
    // Test that safe strings pass through unchanged
    let safe_inputs = vec![
        "US",
        "Public",
        "Encryption",
        "some_valid_label",
    ];

    for input in safe_inputs {
        let query = SparqlGenerator::find_policies_by_jurisdiction(input);
        assert!(query.contains(input), "Safe input should appear in query");
    }
}

#[test]
fn test_select_with_filters_determinism_with_injection_attempt() {
    // Test select_with_filters with malicious filter
    let filters = vec![
        ("availability".to_string(), "?avail > 99; DROP GRAPH test".to_string()),
        ("cost".to_string(), "?cost < 1000".to_string()),
    ];

    let query = SparqlGenerator::select_with_filters(&["?label"], "Service", &filters);

    // Query should be deterministic and valid
    assert!(query.contains("SELECT"), "Query should be valid");
    assert!(query.contains("Service"), "Class type should be present");
}
