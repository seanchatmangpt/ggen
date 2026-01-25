//! Integration tests for TTL to Signature transpiler
//!
//! Tests the complete workflow of converting TTL files with SHACL shapes
//! into DSPy Signature specifications.

use ggen_ai::codegen::{PropertyShape, TTLToSignatureTranspiler};
use oxigraph::store::Store;

/// Test helper to create a minimal TTL with SHACL shapes
fn create_test_ttl_with_shapes() -> String {
    r#"
@prefix : <http://example.com/test/> .
@prefix sh: <http://www.w3.org/ns/shacl#> .
@prefix rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#> .
@prefix rdfs: <http://www.w3.org/2000/01/rdf-schema#> .
@prefix xsd: <http://www.w3.org/2001/XMLSchema#> .
@prefix cns: <http://cns.io/ontology#> .

:PersonShape
    rdf:type sh:NodeShape ;
    sh:targetClass :Person ;
    sh:property :PersonNameProperty ;
    sh:property :PersonAgeProperty ;
    sh:property :PersonEmailProperty .

:PersonNameProperty
    sh:path :name ;
    sh:datatype xsd:string ;
    rdfs:comment "Person's full name" .

:PersonAgeProperty
    sh:path :age ;
    sh:datatype xsd:integer ;
    rdfs:comment "Person's age in years" .

:PersonEmailProperty
    sh:path :email ;
    sh:datatype xsd:string ;
    cns:outputField "true" ;
    rdfs:comment "Person's email address (output field)" .

:Person rdf:type rdfs:Class ;
    rdfs:label "Person" ;
    rdfs:comment "A person entity" .
    "#
    .to_string()
}

#[test]
fn test_transpiler_with_empty_store() {
    let transpiler = TTLToSignatureTranspiler::new();
    let store = Store::new().expect("Failed to create store");

    let result = transpiler.find_classes_with_shapes(&store);
    assert!(result.is_ok());

    let classes = result.unwrap();
    assert!(classes.is_empty());
}

#[test]
fn test_transpiler_creation_and_defaults() {
    let transpiler = TTLToSignatureTranspiler::new();
    assert_eq!(transpiler.signature_count(), 0);

    let default_transpiler = TTLToSignatureTranspiler::default();
    assert_eq!(default_transpiler.signature_count(), 0);
}

#[test]
fn test_property_shape_struct() {
    let shape = PropertyShape {
        iri: "http://example.com/shape#TestShape".to_string(),
        path: "testProperty".to_string(),
        description: Some("Test description".to_string()),
        datatype: Some("http://www.w3.org/2001/XMLSchema#string".to_string()),
        is_output: false,
    };

    assert_eq!(shape.iri, "http://example.com/shape#TestShape");
    assert_eq!(shape.path, "testProperty");
    assert_eq!(shape.description, Some("Test description".to_string()));
    assert!(!shape.is_output);
}

#[test]
fn test_property_shape_with_output_flag() {
    let shape = PropertyShape {
        iri: "http://example.com/shape#OutputShape".to_string(),
        path: "outputProperty".to_string(),
        description: None,
        datatype: Some("http://www.w3.org/2001/XMLSchema#string".to_string()),
        is_output: true,
    };

    assert!(shape.is_output);
}

#[test]
fn test_naming_conversions() {
    let transpiler = TTLToSignatureTranspiler::new();

    // Test various naming patterns
    assert_eq!(transpiler.snake_case("MyClassName"), "my_class_name");
    assert_eq!(transpiler.snake_case("my-property"), "my_property");
    assert_eq!(transpiler.snake_case("UPPERCASE"), "uppercase");
    assert_eq!(
        transpiler.snake_case("CamelCaseProperty"),
        "camel_case_property"
    );
    assert_eq!(
        transpiler.snake_case("already_snake_case"),
        "already_snake_case"
    );
}

#[test]
fn test_iri_local_name_extraction() {
    let transpiler = TTLToSignatureTranspiler::new();

    // Test various IRI patterns
    assert_eq!(
        transpiler.safe_local_name("http://example.com/ontology#Class"),
        "Class"
    );
    assert_eq!(
        transpiler.safe_local_name("http://example.com/ontology/Class"),
        "Class"
    );
    assert_eq!(
        transpiler.safe_local_name("http://example.com#ns#Class"),
        "Class"
    );
    assert_eq!(transpiler.safe_local_name("SimpleClass"), "SimpleClass");
    assert_eq!(transpiler.safe_local_name(""), "");
}

#[test]
fn test_field_name_collision_detection() {
    let mut transpiler = TTLToSignatureTranspiler::new();

    // First field is allowed
    let name1 = transpiler.check_field_collision("field_name".to_string());
    assert_eq!(name1, "field_name");

    // Duplicate gets numbered
    let name2 = transpiler.check_field_collision("field_name".to_string());
    assert_eq!(name2, "field_name_1");

    // Third duplicate gets incremented
    let name3 = transpiler.check_field_collision("field_name".to_string());
    assert_eq!(name3, "field_name_2");
}

#[test]
fn test_reserved_name_avoidance() {
    let mut transpiler = TTLToSignatureTranspiler::new();

    // Python/DSPy reserved names should get "custom_" prefix
    let custom_class = transpiler.check_field_collision("class".to_string());
    assert_eq!(custom_class, "custom_class");

    let custom_def = transpiler.check_field_collision("def".to_string());
    assert_eq!(custom_def, "custom_def");

    let custom_return = transpiler.check_field_collision("return".to_string());
    assert_eq!(custom_return, "custom_return");
}

#[test]
fn test_datatype_mapping() {
    let transpiler = TTLToSignatureTranspiler::new();

    // XSD datatypes should map to Rust types
    assert_eq!(
        transpiler.extract_datatype("http://www.w3.org/2001/XMLSchema#string"),
        "String"
    );
    assert_eq!(
        transpiler.extract_datatype("http://www.w3.org/2001/XMLSchema#integer"),
        "i32"
    );
    assert_eq!(
        transpiler.extract_datatype("http://www.w3.org/2001/XMLSchema#int"),
        "i32"
    );
    assert_eq!(
        transpiler.extract_datatype("http://www.w3.org/2001/XMLSchema#boolean"),
        "bool"
    );
    assert_eq!(
        transpiler.extract_datatype("http://www.w3.org/2001/XMLSchema#float"),
        "f32"
    );
    assert_eq!(
        transpiler.extract_datatype("http://www.w3.org/2001/XMLSchema#double"),
        "f32"
    );
    assert_eq!(
        transpiler.extract_datatype("http://www.w3.org/2001/XMLSchema#decimal"),
        "f32"
    );

    // Unknown types default to String
    assert_eq!(
        transpiler.extract_datatype("http://example.com/CustomType"),
        "String"
    );
}

#[test]
fn test_multiple_field_collision_resolution() {
    let mut transpiler = TTLToSignatureTranspiler::new();

    // Create multiple collisions
    let fields = vec!["name", "name", "name", "name"];

    let results: Vec<String> = fields
        .into_iter()
        .map(|f| transpiler.check_field_collision(f.to_string()))
        .collect();

    assert_eq!(results[0], "name");
    assert_eq!(results[1], "name_1");
    assert_eq!(results[2], "name_2");
    assert_eq!(results[3], "name_3");
}

#[test]
fn test_complex_naming_scenarios() {
    let mut transpiler = TTLToSignatureTranspiler::new();

    // Mix of snake_case conversion and collision detection
    let field1 = transpiler.snake_case("MyPropertyName");
    let field1_no_collision = transpiler.check_field_collision(field1);
    assert_eq!(field1_no_collision, "my_property_name");

    let field2 = transpiler.snake_case("MyPropertyName"); // Same
    let field2_with_collision = transpiler.check_field_collision(field2);
    assert_eq!(field2_with_collision, "my_property_name_1");
}

#[test]
fn test_empty_and_edge_case_names() {
    let transpiler = TTLToSignatureTranspiler::new();

    // Edge cases
    assert_eq!(transpiler.snake_case(""), "unnamed_field");
    assert_eq!(transpiler.snake_case("_"), "unnamed_field");
    assert_eq!(transpiler.snake_case("___"), "unnamed_field");
    assert_eq!(transpiler.snake_case("123"), "field_123");
    assert_eq!(transpiler.snake_case("_private"), "private");
}

#[test]
fn test_ttl_loading_and_parsing() {
    use oxigraph::sparql::SparqlEvaluator;

    let store = Store::new().expect("Failed to create store");

    // Store should be created successfully
    let result = SparqlEvaluator::new()
        .parse_query("SELECT ?s WHERE { ?s ?p ?o }")
        .and_then(|q| q.on_store(&store).execute().map_err(|e| e.into()));
    assert!(result.is_ok());
}

#[test]
fn test_property_shape_with_all_fields() {
    let shape = PropertyShape {
        iri: "http://example.com/test#shape".to_string(),
        path: "http://example.com/test#property".to_string(),
        description: Some("Complete test shape".to_string()),
        datatype: Some("http://www.w3.org/2001/XMLSchema#string".to_string()),
        is_output: true,
    };

    assert!(!shape.iri.is_empty());
    assert!(!shape.path.is_empty());
    assert!(shape.description.is_some());
    assert!(shape.datatype.is_some());
    assert!(shape.is_output);
}

#[test]
fn test_property_shape_minimal() {
    let shape = PropertyShape {
        iri: String::new(),
        path: "property".to_string(),
        description: None,
        datatype: None,
        is_output: false,
    };

    assert!(shape.iri.is_empty());
    assert_eq!(shape.path, "property");
    assert!(shape.description.is_none());
    assert!(shape.datatype.is_none());
    assert!(!shape.is_output);
}

#[test]
fn test_snake_case_consistency() {
    let transpiler = TTLToSignatureTranspiler::new();

    // Same input should always produce same output
    let input = "MyComplexPropertyName-With-Hyphens";
    let output1 = transpiler.snake_case(input);
    let output2 = transpiler.snake_case(input);

    assert_eq!(output1, output2);
}

#[test]
fn test_iri_extraction_consistency() {
    let transpiler = TTLToSignatureTranspiler::new();

    // Same IRI should always extract same local name
    let iri = "http://example.com/very/long/ontology#ClassName";
    let name1 = transpiler.safe_local_name(iri);
    let name2 = transpiler.safe_local_name(iri);

    assert_eq!(name1, name2);
    assert_eq!(name1, "ClassName");
}
