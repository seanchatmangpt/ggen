//! Tests for RDF literal type handling across ggen's SPARQL pipeline.
//!
//! ggen's pipeline converts SPARQL results into Tera template variables via
//! `Graph::query_cached()`, which materializes oxigraph `Term::to_string()` output
//! into `CachedResult::Solutions(Vec<BTreeMap<String, String>>)`.
//!
//! The key issue: oxigraph serializes RDF terms with artifacts that are unsuitable
//! for direct template rendering:
//!
//! | RDF Term Type        | oxigraph `Term::to_string()` output          |
//! |----------------------|-----------------------------------------------|
//! | Plain string literal | `"Alice"` (with surrounding quotes)            |
//! | Typed literal        | `"42"^^<http://www.w3.org/2001/XMLSchema#integer>` |
//! | Language-tagged      | `"hello"@en`                                  |
//! | IRI                  | `<http://example.org/Alice>` (angle brackets) |
//!
//! Templates expect clean values like `Alice`, `42`, `hello`, `http://example.org/Alice`.
//!
//! These tests DOCUMENT the current (dirty) serialization behavior so that any future
//! cleanup of the pipeline is caught by regressions.

use ggen_core::graph::{CachedResult, Graph};
use serde_json::{Map, Value};

// ---------------------------------------------------------------------------
// Shared prefixes
// ---------------------------------------------------------------------------

const PREFIXES: &str = r#"
@prefix ex:  <http://example.org/> .
@prefix xsd: <http://www.w3.org/2001/XMLSchema#> .
@prefix rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#> .
"#;

// ---------------------------------------------------------------------------
// Helpers
// ---------------------------------------------------------------------------

/// Create a fresh graph loaded with the given Turtle fragment (prefixes auto-prepended).
fn graph_with(ttl_body: &str) -> Graph {
    let graph = Graph::new().expect("Graph::new should succeed");
    graph
        .insert_turtle(&format!("{}{}", PREFIXES, ttl_body))
        .expect("insert_turtle should succeed");
    graph
}

/// Execute a SPARQL SELECT and return the raw solution rows.
fn query_rows(graph: &Graph, sparql: &str) -> Vec<std::collections::BTreeMap<String, String>> {
    let result = graph
        .query_cached(sparql)
        .expect("query_cached should succeed");
    match result {
        CachedResult::Solutions(rows) => rows,
        other => panic!("Expected Solutions, got {:?}", other),
    }
}

/// Strip RDF serialization artifacts from a raw oxigraph term value.
///
/// Handles:
/// - IRI brackets: `<http://...>` -> `http://...`
/// - Typed literal: `"42"^^<http://...#integer>` -> `42`
/// - Language-tagged: `"hello"@en` -> `hello`
/// - Plain string literal: `"Alice"` -> `Alice`
fn to_clean_json(result: &CachedResult) -> Value {
    match result {
        CachedResult::Solutions(rows) => {
            let arr: Vec<Value> = rows
                .iter()
                .map(|row| {
                    let mut obj = Map::new();
                    for (k, v) in row {
                        obj.insert(k.clone(), Value::String(clean_rdf_term(v)));
                    }
                    Value::Object(obj)
                })
                .collect();
            Value::Array(arr)
        }
        CachedResult::Boolean(b) => Value::Bool(*b),
        CachedResult::Graph(triples) => {
            let arr: Vec<Value> = triples.iter().map(|t| Value::String(t.clone())).collect();
            Value::Array(arr)
        }
    }
}

/// Strip RDF serialization artifacts from a single term string.
fn clean_rdf_term(raw: &str) -> String {
    let s = raw.trim();

    // IRI: <http://example.org/foo> -> http://example.org/foo
    if s.starts_with('<') && s.ends_with('>') {
        return s[1..s.len() - 1].to_string();
    }

    // Typed literal: "42"^^<http://...#integer> -> 42
    // Language-tagged: "hello"@en -> hello
    // Plain string: "Alice" -> Alice
    if s.starts_with('"') {
        // Find the closing quote
        if let Some(close_quote) = s[1..].find('"') {
            let inner = &s[1..=close_quote];
            // Check if there is a ^^ or @ suffix after the closing quote
            let rest = &s[close_quote + 2..];
            if rest.starts_with("^^") || rest.starts_with('@') {
                // Typed or language-tagged: return just the inner value
                return inner.to_string();
            }
            return inner.to_string();
        }
    }

    // Fallback: return as-is
    s.to_string()
}

// ===========================================================================
// Test 1: String literals have quotes in raw SPARQL results
// ===========================================================================

#[test]
fn test_string_literals_have_quotes_in_raw_results() {
    let graph = graph_with(
        r#"
        ex:alice ex:name "Alice" .
    "#,
    );

    let rows = query_rows(
        &graph,
        r#"
        PREFIX ex: <http://example.org/>
        SELECT ?name WHERE { ex:alice ex:name ?name }
    "#,
    );

    assert_eq!(rows.len(), 1, "Should return exactly one row");
    let name = rows[0].get("name").expect("name binding should exist");
    // oxigraph serializes plain string literals with surrounding double quotes
    assert_eq!(
        name, "\"Alice\"",
        "String literal should have surrounding quotes in raw output"
    );
}

// ===========================================================================
// Test 2: Typed integer literals include datatype URI
// ===========================================================================

#[test]
fn test_typed_integer_literals_include_datatype_uri() {
    let graph = graph_with(
        r#"
        ex:alice ex:age "42"^^xsd:integer .
    "#,
    );

    let rows = query_rows(
        &graph,
        r#"
        PREFIX ex: <http://example.org/>
        SELECT ?age WHERE { ex:alice ex:age ?age }
    "#,
    );

    assert_eq!(rows.len(), 1);
    let age = rows[0].get("age").expect("age binding should exist");
    // oxigraph serializes typed literals as "value"^^<datatype-uri>
    assert!(
        age.contains("42"),
        "Typed integer should contain the numeric value, got: {}",
        age
    );
    assert!(
        age.contains("integer"),
        "Typed integer should reference xsd:integer in raw output, got: {}",
        age
    );
}

// ===========================================================================
// Test 3: Typed boolean literals include datatype URI
// ===========================================================================

#[test]
fn test_typed_boolean_literals_include_datatype_uri() {
    let graph = graph_with(
        r#"
        ex:alice ex:active "true"^^xsd:boolean .
    "#,
    );

    let rows = query_rows(
        &graph,
        r#"
        PREFIX ex: <http://example.org/>
        SELECT ?active WHERE { ex:alice ex:active ?active }
    "#,
    );

    assert_eq!(rows.len(), 1);
    let active = rows[0].get("active").expect("active binding should exist");
    assert!(
        active.contains("true"),
        "Typed boolean should contain 'true', got: {}",
        active
    );
    assert!(
        active.contains("boolean"),
        "Typed boolean should reference xsd:boolean in raw output, got: {}",
        active
    );
}

// ===========================================================================
// Test 4: Language-tagged literals include language tag
// ===========================================================================

#[test]
fn test_language_tagged_literals_include_language_tag() {
    let graph = graph_with(
        r#"
        ex:alice ex:greeting "Hello"@en .
    "#,
    );

    let rows = query_rows(
        &graph,
        r#"
        PREFIX ex: <http://example.org/>
        SELECT ?greeting WHERE { ex:alice ex:greeting ?greeting }
    "#,
    );

    assert_eq!(rows.len(), 1);
    let greeting = rows[0]
        .get("greeting")
        .expect("greeting binding should exist");
    assert!(
        greeting.contains("Hello"),
        "Language-tagged literal should contain the value, got: {}",
        greeting
    );
    assert!(
        greeting.contains("@en"),
        "Language-tagged literal should include @en tag in raw output, got: {}",
        greeting
    );
}

// ===========================================================================
// Test 5: IRI values include angle brackets
// ===========================================================================

#[test]
fn test_iri_values_include_angle_brackets() {
    let graph = graph_with(
        r#"
        ex:alice ex:friend ex:bob .
        ex:bob a ex:Person .
    "#,
    );

    let rows = query_rows(
        &graph,
        r#"
        PREFIX ex: <http://example.org/>
        SELECT ?friend WHERE { ex:alice ex:friend ?friend }
    "#,
    );

    assert_eq!(rows.len(), 1);
    let friend = rows[0].get("friend").expect("friend binding should exist");
    assert!(
        friend.starts_with('<') && friend.ends_with('>'),
        "IRI values should have angle brackets in raw output, got: {}",
        friend
    );
    assert!(
        friend.contains("http://example.org/bob"),
        "IRI should contain full URI, got: {}",
        friend
    );
}

// ===========================================================================
// Test 6: to_clean_json strips quotes from string literals
// ===========================================================================

#[test]
fn test_to_clean_json_strips_quotes_from_string_literals() {
    let graph = graph_with(
        r#"
        ex:alice ex:name "Alice" .
    "#,
    );

    let result = graph
        .query_cached(
            r#"
            PREFIX ex: <http://example.org/>
            SELECT ?name WHERE { ex:alice ex:name ?name }
        "#,
        )
        .expect("query_cached should succeed");

    let json = to_clean_json(&result);
    let arr = json.as_array().expect("Should be a JSON array");
    assert_eq!(arr.len(), 1);

    let clean_name = arr[0]
        .get("name")
        .expect("name should exist")
        .as_str()
        .expect("name should be a string");
    assert_eq!(
        clean_name, "Alice",
        "to_clean_json should strip surrounding quotes from string literals"
    );
}

// ===========================================================================
// Test 7: Mixed literal types in single query result
// ===========================================================================

#[test]
fn test_mixed_literal_types_in_single_query() {
    let graph = graph_with(
        r#"
        ex:alice ex:name "Alice" ;
                  ex:age "42"^^xsd:integer ;
                  ex:active "true"^^xsd:boolean ;
                  ex:greeting "Hello"@en .
    "#,
    );

    let rows = query_rows(
        &graph,
        r#"
        PREFIX ex: <http://example.org/>
        SELECT ?name ?age ?active ?greeting
        WHERE {
            ex:alice ex:name ?name ;
                      ex:age ?age ;
                      ex:active ?active ;
                      ex:greeting ?greeting .
        }
    "#,
    );

    assert_eq!(
        rows.len(),
        1,
        "Should return one row with all four bindings"
    );

    let row = &rows[0];
    let name = row.get("name").expect("name binding");
    let age = row.get("age").expect("age binding");
    let active = row.get("active").expect("active binding");
    let greeting = row.get("greeting").expect("greeting binding");

    // All four raw values should be present (even with RDF artifacts)
    assert!(
        name.contains("Alice"),
        "name should contain Alice, got: {}",
        name
    );
    assert!(age.contains("42"), "age should contain 42, got: {}", age);
    assert!(
        active.contains("true"),
        "active should contain true, got: {}",
        active
    );
    assert!(
        greeting.contains("Hello"),
        "greeting should contain Hello, got: {}",
        greeting
    );

    // Verify each type carries its distinctive artifact
    assert!(
        name.starts_with('"') && name.ends_with('"'),
        "String literal should have quotes, got: {}",
        name
    );
    assert!(
        age.contains("integer"),
        "Integer should have datatype, got: {}",
        age
    );
    assert!(
        active.contains("boolean"),
        "Boolean should have datatype, got: {}",
        active
    );
    assert!(
        greeting.contains("@en"),
        "Language-tagged should have @ tag, got: {}",
        greeting
    );
}

// ===========================================================================
// Test 8: Blank nodes serialize consistently
// ===========================================================================

#[test]
fn test_blank_nodes_serialize_consistently() {
    let graph = graph_with(
        r#"
        ex:alice ex:knows [ ex:name "Bob" ] .
    "#,
    );

    let rows = query_rows(
        &graph,
        r#"
        PREFIX ex: <http://example.org/>
        SELECT ?friendName WHERE {
            ex:alice ex:knows ?_blank .
            ?_blank ex:name ?friendName .
        }
    "#,
    );

    assert_eq!(
        rows.len(),
        1,
        "Should resolve blank node and return one row"
    );
    let friend_name = rows[0]
        .get("friendName")
        .expect("friendName binding should exist");

    // The value should be the string literal "Bob" (with RDF quotes)
    assert!(
        friend_name.contains("Bob"),
        "Blank node property should resolve to Bob, got: {}",
        friend_name
    );

    // Re-query to verify determinism
    let rows2 = query_rows(
        &graph,
        r#"
        PREFIX ex: <http://example.org/>
        SELECT ?friendName WHERE {
            ex:alice ex:knows ?_blank .
            ?_blank ex:name ?friendName .
        }
    "#,
    );
    assert_eq!(
        rows[0].get("friendName"),
        rows2[0].get("friendName"),
        "Blank node serialization should be deterministic across queries"
    );
}

// ===========================================================================
// Test 9: URI/IRI values used as objects are serialized
// ===========================================================================

#[test]
fn test_iri_object_values_are_serialized() {
    let graph = graph_with(
        r#"
        ex:alice rdf:type ex:Person .
    "#,
    );

    let rows = query_rows(
        &graph,
        r#"
        PREFIX rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#>
        PREFIX ex: <http://example.org/>
        SELECT ?type WHERE { ex:alice rdf:type ?type }
    "#,
    );

    assert_eq!(rows.len(), 1);
    let type_val = rows[0].get("type").expect("type binding should exist");
    // IRI objects are serialized with angle brackets
    assert!(
        type_val.contains("Person"),
        "IRI object should contain 'Person', got: {}",
        type_val
    );
    assert!(
        type_val.contains("http://example.org/Person"),
        "IRI object should contain full URI, got: {}",
        type_val
    );
}

// ===========================================================================
// Test 10: Empty string literal vs missing binding
// ===========================================================================

#[test]
fn test_empty_string_literal_vs_missing_binding() {
    let graph = graph_with(
        r#"
        ex:alice ex:note "" .
        ex:bob ex:label "Bob" .
    "#,
    );

    let rows = query_rows(
        &graph,
        r#"
        PREFIX ex: <http://example.org/>
        SELECT ?entity ?note ?label
        WHERE {
            VALUES ?entity { ex:alice ex:bob }
            OPTIONAL { ?entity ex:note ?note }
            OPTIONAL { ?entity ex:label ?label }
        }
        ORDER BY ?entity
    "#,
    );

    assert_eq!(rows.len(), 2, "Should return rows for both entities");

    // First row: alice has empty string for note, missing for label
    let alice_row = &rows[0];
    let alice_note = alice_row
        .get("note")
        .expect("alice note binding should exist (empty string)");
    assert_eq!(
        alice_note, "\"\"",
        "Empty string literal should serialize as two double quotes"
    );
    // label is OPTIONAL and absent for alice — no binding
    assert!(
        alice_row.get("label").is_none(),
        "Missing OPTIONAL property should not produce a binding"
    );

    // Second row: bob has label, no note
    let bob_row = &rows[1];
    assert!(
        bob_row.get("note").is_none(),
        "Missing OPTIONAL property should not produce a binding for bob"
    );
    let bob_label = bob_row
        .get("label")
        .expect("bob label binding should exist");
    assert!(
        bob_label.contains("Bob"),
        "bob label should contain 'Bob', got: {}",
        bob_label
    );
}

// ===========================================================================
// Bonus: to_clean_json handles all literal types
// ===========================================================================

#[test]
fn test_to_clean_json_handles_typed_literal() {
    let graph = graph_with(
        r#"
        ex:alice ex:age "42"^^xsd:integer .
    "#,
    );

    let result = graph
        .query_cached(
            r#"
            PREFIX ex: <http://example.org/>
            SELECT ?age WHERE { ex:alice ex:age ?age }
        "#,
        )
        .expect("query_cached should succeed");

    let json = to_clean_json(&result);
    let clean_age = json.as_array().unwrap()[0]
        .get("age")
        .unwrap()
        .as_str()
        .unwrap();
    assert_eq!(
        clean_age, "42",
        "to_clean_json should strip datatype URI from typed literals"
    );
}

#[test]
fn test_to_clean_json_handles_language_tagged_literal() {
    let graph = graph_with(
        r#"
        ex:alice ex:greeting "Hello"@en .
    "#,
    );

    let result = graph
        .query_cached(
            r#"
            PREFIX ex: <http://example.org/>
            SELECT ?greeting WHERE { ex:alice ex:greeting ?greeting }
        "#,
        )
        .expect("query_cached should succeed");

    let json = to_clean_json(&result);
    let clean_greeting = json.as_array().unwrap()[0]
        .get("greeting")
        .unwrap()
        .as_str()
        .unwrap();
    assert_eq!(
        clean_greeting, "Hello",
        "to_clean_json should strip language tag from literals"
    );
}

#[test]
fn test_to_clean_json_handles_iri() {
    let graph = graph_with(
        r#"
        ex:alice ex:friend ex:bob .
    "#,
    );

    let result = graph
        .query_cached(
            r#"
            PREFIX ex: <http://example.org/>
            SELECT ?friend WHERE { ex:alice ex:friend ?friend }
        "#,
        )
        .expect("query_cached should succeed");

    let json = to_clean_json(&result);
    let clean_friend = json.as_array().unwrap()[0]
        .get("friend")
        .unwrap()
        .as_str()
        .unwrap();
    assert_eq!(
        clean_friend, "http://example.org/bob",
        "to_clean_json should strip angle brackets from IRIs"
    );
}
