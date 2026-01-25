//! Chicago TDD tests for SPARQL query builder
//!
//! These tests follow the AAA (Arrange-Act-Assert) pattern and verify
//! observable behavior with real collaborators (no mocks).

use ggen_core::rdf::query_builder::{Iri, Literal, SparqlQueryBuilder, Variable};

#[test]
fn test_injection_prevention_in_iri() {
    // Arrange
    let malicious_iri = "http://example.com/>; DROP TABLE users; <http://evil.com/";

    // Act
    let result = Iri::new(malicious_iri);

    // Assert - Should reject injection attempt
    assert!(result.is_err());
    let error_msg = result.unwrap_err().to_string();
    assert!(
        error_msg.contains("Invalid IRI format"),
        "Expected validation error for malicious IRI, got: {}",
        error_msg
    );
}

#[test]
fn test_injection_prevention_in_variable() {
    // Arrange
    let malicious_var = "x; INSERT DATA { <hack> <hack> <hack> }";

    // Act
    let result = Variable::new(malicious_var);

    // Assert - Should reject injection attempt
    assert!(result.is_err());
    let error_msg = result.unwrap_err().to_string();
    assert!(
        error_msg.contains("Invalid variable"),
        "Expected validation error for malicious variable, got: {}",
        error_msg
    );
}

#[test]
fn test_literal_escapes_quotes() {
    // Arrange
    let input = "Value with \"quotes\" and 'apostrophes'";

    // Act
    let literal = Literal::new(input);

    // Assert - Quotes should be escaped
    let escaped = literal.as_str();
    assert!(escaped.contains("\\\"quotes\\\""));
    assert!(!escaped.contains("\"quotes\""));
}

#[test]
fn test_literal_escapes_newlines() {
    // Arrange
    let input = "Line 1\nLine 2\rLine 3\tTabbed";

    // Act
    let literal = Literal::new(input);

    // Assert - Control characters should be escaped
    let escaped = literal.as_str();
    assert!(escaped.contains("\\n"));
    assert!(escaped.contains("\\r"));
    assert!(escaped.contains("\\t"));
}

#[test]
fn test_select_query_complete_workflow() {
    // Arrange
    let subject = Variable::new("subject").unwrap();
    let predicate = Variable::new("predicate").unwrap();
    let object = Variable::new("object").unwrap();
    let prefix = Iri::new("https://example.com/").unwrap();

    // Act
    let query = SparqlQueryBuilder::select()
        .prefix("ex", prefix)
        .var(subject)
        .var(predicate)
        .var(object)
        .where_pattern("?subject ?predicate ?object")
        .filter("?predicate = <https://example.com/name>")
        .limit(100)
        .build()
        .unwrap();

    // Assert - Query should be well-formed and safe
    assert!(query.contains("PREFIX ex: <https://example.com/>"));
    assert!(query.contains("SELECT ?subject ?predicate ?object"));
    assert!(query.contains("WHERE {"));
    assert!(query.contains("?subject ?predicate ?object"));
    assert!(query.contains("FILTER (?predicate = <https://example.com/name>)"));
    assert!(query.contains("LIMIT 100"));

    // Verify no injection artifacts
    assert!(!query.contains("DROP"));
    assert!(!query.contains("INSERT"));
    assert!(!query.contains("DELETE"));
}

#[test]
fn test_construct_query_safe_pattern_building() {
    // Arrange
    let prefix = Iri::new("http://example.com/").unwrap();

    // Act
    let query = SparqlQueryBuilder::construct()
        .prefix("ex", prefix)
        .construct_pattern("?s ex:newProperty ?o")
        .where_pattern("?s ex:oldProperty ?o")
        .filter("BOUND(?o)")
        .build()
        .unwrap();

    // Assert - CONSTRUCT query should be properly formatted
    assert!(query.contains("CONSTRUCT {"));
    assert!(query.contains("?s ex:newProperty ?o"));
    assert!(query.contains("WHERE {"));
    assert!(query.contains("?s ex:oldProperty ?o"));
    assert!(query.contains("FILTER (BOUND(?o))"));
}

#[test]
fn test_ask_query_boolean_check() {
    // Arrange
    let prefix = Iri::new("http://schema.org/").unwrap();

    // Act
    let query = SparqlQueryBuilder::ask()
        .prefix("schema", prefix)
        .where_pattern("?person a schema:Person")
        .where_pattern("?person schema:name ?name")
        .filter("?name = \"Alice\"")
        .build()
        .unwrap();

    // Assert - ASK query should check for pattern existence
    assert!(query.contains("ASK {"));
    assert!(query.contains("?person a schema:Person"));
    assert!(query.contains("?person schema:name ?name"));
    assert!(query.contains("FILTER (?name = \"Alice\")"));
}

#[test]
fn test_describe_query_with_resource() {
    // Arrange
    let resource = Iri::new("http://example.com/alice").unwrap();
    let prefix = Iri::new("http://example.com/").unwrap();

    // Act
    let query = SparqlQueryBuilder::describe()
        .prefix("ex", prefix)
        .resource(resource)
        .build()
        .unwrap();

    // Assert - DESCRIBE query should target specific resource
    assert!(query.contains("PREFIX ex: <http://example.com/>"));
    assert!(query.contains("DESCRIBE <http://example.com/alice>"));
}

#[test]
fn test_describe_query_with_where_clause() {
    // Arrange
    let var = Variable::new("person").unwrap();

    // Act
    let query = SparqlQueryBuilder::describe()
        .var(var)
        .where_pattern("?person a <http://schema.org/Person>")
        .where_pattern("?person <http://schema.org/age> ?age")
        .build()
        .unwrap();

    // Assert - DESCRIBE with WHERE should filter results
    assert!(query.contains("DESCRIBE ?person"));
    assert!(query.contains("WHERE {"));
    assert!(query.contains("?person a <http://schema.org/Person>"));
    assert!(query.contains("?person <http://schema.org/age> ?age"));
}

#[test]
fn test_multiple_where_patterns() {
    // Arrange
    let var = Variable::new("entity").unwrap();

    // Act
    let query = SparqlQueryBuilder::select()
        .var(var)
        .where_pattern("?entity <http://example.com/type> ?type")
        .where_pattern("?entity <http://example.com/name> ?name")
        .where_pattern("?entity <http://example.com/value> ?value")
        .build()
        .unwrap();

    // Assert - All WHERE patterns should be present
    assert!(query.contains("?entity <http://example.com/type> ?type"));
    assert!(query.contains("?entity <http://example.com/name> ?name"));
    assert!(query.contains("?entity <http://example.com/value> ?value"));
}

#[test]
fn test_multiple_filters() {
    // Arrange
    let var = Variable::new("x").unwrap();

    // Act
    let query = SparqlQueryBuilder::select()
        .var(var)
        .where_pattern("?x <http://example.com/value> ?val")
        .filter("?val > 100")
        .filter("?val < 1000")
        .build()
        .unwrap();

    // Assert - All FILTER clauses should be present
    assert!(query.contains("FILTER (?val > 100)"));
    assert!(query.contains("FILTER (?val < 1000)"));
}

#[test]
fn test_distinct_modifier() {
    // Arrange
    let var = Variable::new("category").unwrap();

    // Act
    let query = SparqlQueryBuilder::select()
        .distinct()
        .var(var)
        .where_pattern("?item <http://example.com/category> ?category")
        .build()
        .unwrap();

    // Assert - DISTINCT should be applied
    assert!(query.contains("SELECT DISTINCT ?category"));
}

#[test]
fn test_order_by_clause() {
    // Arrange
    let name_var = Variable::new("name").unwrap();
    let age_var = Variable::new("age").unwrap();
    let person_var = Variable::new("person").unwrap();

    // Act
    let query = SparqlQueryBuilder::select()
        .var(person_var)
        .var(name_var.clone())
        .where_pattern("?person <http://schema.org/name> ?name")
        .where_pattern("?person <http://schema.org/age> ?age")
        .order_by(age_var)
        .build()
        .unwrap();

    // Assert - ORDER BY should be present
    assert!(query.contains("ORDER BY ?age"));
}

#[test]
fn test_offset_clause() {
    // Arrange
    let var = Variable::new("item").unwrap();

    // Act
    let query = SparqlQueryBuilder::select()
        .var(var)
        .where_pattern("?item ?p ?o")
        .limit(10)
        .offset(20)
        .build()
        .unwrap();

    // Assert - Both LIMIT and OFFSET should be present
    assert!(query.contains("LIMIT 10"));
    assert!(query.contains("OFFSET 20"));
}

#[test]
fn test_select_all_variables() {
    // Arrange & Act
    let query = SparqlQueryBuilder::select()
        .all_vars()
        .where_pattern("?s ?p ?o")
        .build()
        .unwrap();

    // Assert - Should select all variables with *
    assert!(query.contains("SELECT *"));
}

#[test]
fn test_multiple_prefixes() {
    // Arrange
    let ex_prefix = Iri::new("http://example.com/").unwrap();
    let schema_prefix = Iri::new("http://schema.org/").unwrap();
    let var = Variable::new("person").unwrap();

    // Act
    let query = SparqlQueryBuilder::select()
        .prefix("ex", ex_prefix)
        .prefix("schema", schema_prefix)
        .var(var)
        .where_pattern("?person a schema:Person")
        .where_pattern("?person ex:customProperty ?val")
        .build()
        .unwrap();

    // Assert - All prefixes should be declared
    assert!(query.contains("PREFIX ex: <http://example.com/>"));
    assert!(query.contains("PREFIX schema: <http://schema.org/>"));
}

#[test]
fn test_variable_name_without_question_mark() {
    // Arrange & Act
    let var = Variable::new("myvar").unwrap();

    // Assert - Should add question mark automatically
    assert_eq!(var.as_str(), "?myvar");
    assert_eq!(var.name(), "myvar");
}

#[test]
fn test_variable_name_with_question_mark() {
    // Arrange & Act
    let var = Variable::new("?myvar").unwrap();

    // Assert - Should handle existing question mark correctly
    assert_eq!(var.as_str(), "?myvar");
    assert_eq!(var.name(), "myvar");
}

#[test]
fn test_empty_variable_name_rejected() {
    // Arrange & Act
    let result = Variable::new("");

    // Assert - Empty names should be rejected
    assert!(result.is_err());
    assert!(result.unwrap_err().to_string().contains("cannot be empty"));
}

#[test]
fn test_iri_special_characters_rejected() {
    // Arrange
    let test_cases = vec![
        "http://example.com/<script>",
        "http://example.com/{malicious}",
        "http://example.com/|pipe",
        "http://example.com/^caret",
        "http://example.com/`backtick",
        "http://example.com/\\backslash",
    ];

    for test_case in test_cases {
        // Act
        let result = Iri::new(test_case);

        // Assert - All should be rejected
        assert!(
            result.is_err(),
            "Expected IRI '{}' to be rejected",
            test_case
        );
    }
}

#[test]
fn test_complex_query_with_all_features() {
    // Arrange
    let ex_prefix = Iri::new("http://example.com/").unwrap();
    let schema_prefix = Iri::new("http://schema.org/").unwrap();
    let person = Variable::new("person").unwrap();
    let name = Variable::new("name").unwrap();
    let age = Variable::new("age").unwrap();

    // Act
    let query = SparqlQueryBuilder::select()
        .prefix("ex", ex_prefix)
        .prefix("schema", schema_prefix)
        .distinct()
        .var(person)
        .var(name.clone())
        .var(age.clone())
        .where_pattern("?person a schema:Person")
        .where_pattern("?person schema:name ?name")
        .where_pattern("?person ex:age ?age")
        .filter("?age >= 18")
        .filter("?age <= 65")
        .order_by(age)
        .limit(50)
        .offset(10)
        .build()
        .unwrap();

    // Assert - All components should be present and correct
    assert!(query.contains("PREFIX ex: <http://example.com/>"));
    assert!(query.contains("PREFIX schema: <http://schema.org/>"));
    assert!(query.contains("SELECT DISTINCT ?person ?name ?age"));
    assert!(query.contains("WHERE {"));
    assert!(query.contains("?person a schema:Person"));
    assert!(query.contains("?person schema:name ?name"));
    assert!(query.contains("?person ex:age ?age"));
    assert!(query.contains("FILTER (?age >= 18)"));
    assert!(query.contains("FILTER (?age <= 65)"));
    assert!(query.contains("ORDER BY ?age"));
    assert!(query.contains("LIMIT 50"));
    assert!(query.contains("OFFSET 10"));
}

#[test]
fn test_construct_multiple_patterns() {
    // Arrange
    let prefix = Iri::new("http://example.com/").unwrap();

    // Act
    let query = SparqlQueryBuilder::construct()
        .prefix("ex", prefix)
        .construct_pattern("?s ex:derivedProp1 ?o1")
        .construct_pattern("?s ex:derivedProp2 ?o2")
        .construct_pattern("?s ex:derivedProp3 ?o3")
        .where_pattern("?s ex:sourceProp1 ?o1")
        .where_pattern("?s ex:sourceProp2 ?o2")
        .where_pattern("?s ex:sourceProp3 ?o3")
        .build()
        .unwrap();

    // Assert - All CONSTRUCT and WHERE patterns should be present
    assert!(query.contains("?s ex:derivedProp1 ?o1"));
    assert!(query.contains("?s ex:derivedProp2 ?o2"));
    assert!(query.contains("?s ex:derivedProp3 ?o3"));
    assert!(query.contains("?s ex:sourceProp1 ?o1"));
    assert!(query.contains("?s ex:sourceProp2 ?o2"));
    assert!(query.contains("?s ex:sourceProp3 ?o3"));
}
