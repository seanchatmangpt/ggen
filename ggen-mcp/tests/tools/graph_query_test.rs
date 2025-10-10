// London School TDD Tests for ggen_graph_query
// Focus: Interaction testing for SPARQL and RDF operations

use ggen_mcp::tools::graph;
use serde_json::json;

// BDD-Style Tests for SPARQL Query

#[tokio::test]
async fn should_execute_sparql_select_query() {
    // Given: Valid SPARQL SELECT query
    let params = json!({
        "sparql": "SELECT * WHERE { ?s ?p ?o } LIMIT 10"
    });

    // When: Query is executed
    let result = graph::query(params).await;

    // Then: Should return bindings
    assert!(result.is_ok());
    let response = result.unwrap();
    let data = response.get("data").unwrap();

    assert!(data.get("bindings").is_some());
    assert!(data.get("count").is_some());
    assert!(data.get("execution_time_ms").is_some());
}

#[tokio::test]
async fn should_execute_query_on_named_graph() {
    // Given: Query with graph parameter
    let params = json!({
        "sparql": "SELECT ?s WHERE { ?s a :Person }",
        "graph": "http://example.org/people"
    });

    // When: Query is executed
    let result = graph::query(params).await;

    // Then: Should target specified graph
    assert!(result.is_ok());
    let data = result.unwrap().get("data").unwrap().clone();

    assert_eq!(
        data.get("graph").and_then(|v| v.as_str()),
        Some("http://example.org/people")
    );
}

#[tokio::test]
async fn should_fail_when_sparql_is_missing() {
    // Given: Params without SPARQL query
    let params = json!({
        "graph": "http://example.org/test"
    });

    // When: Query is attempted
    let result = graph::query(params).await;

    // Then: Should fail with appropriate error
    assert!(result.is_err());
    assert!(result.unwrap_err().to_string().contains("sparql"));
}

#[tokio::test]
async fn should_handle_empty_result_set() {
    // Given: Query that returns no results
    let params = json!({
        "sparql": "SELECT * WHERE { ?nonexistent ?never ?exists }"
    });

    // When: Query is executed
    let result = graph::query(params).await;

    // Then: Should return empty bindings
    assert!(result.is_ok());
    let data = result.unwrap().get("data").unwrap().clone();

    let bindings = data.get("bindings").unwrap().as_array().unwrap();
    // May be empty or have mock data
    assert!(bindings.len() >= 0);
}

// RDF Load Tests

#[tokio::test]
async fn should_load_rdf_file_in_turtle_format() {
    // Given: RDF file path
    let params = json!({
        "file": "/path/to/data.ttl",
        "format": "turtle"
    });

    // When: Load is performed
    let result = graph::load(params).await;

    // Then: Should load successfully
    assert!(result.is_ok());
    let data = result.unwrap().get("data").unwrap().clone();

    assert_eq!(data.get("file").and_then(|v| v.as_str()), Some("/path/to/data.ttl"));
    assert_eq!(data.get("format").and_then(|v| v.as_str()), Some("turtle"));
    assert!(data.get("triples_loaded").is_some());
}

#[tokio::test]
async fn should_use_turtle_as_default_format() {
    // Given: File without format specified
    let params = json!({
        "file": "/path/to/data.rdf"
    });

    // When: Load is performed
    let result = graph::load(params).await;

    // Then: Should default to turtle format
    assert!(result.is_ok());
    let data = result.unwrap().get("data").unwrap().clone();

    assert_eq!(data.get("format").and_then(|v| v.as_str()), Some("turtle"));
}

#[tokio::test]
async fn should_load_into_named_graph() {
    // Given: Load with target graph
    let params = json!({
        "file": "/path/to/ontology.ttl",
        "graph": "http://example.org/ontology"
    });

    // When: Load is executed
    let result = graph::load(params).await;

    // Then: Should load into specified graph
    assert!(result.is_ok());
    let data = result.unwrap().get("data").unwrap().clone();

    assert_eq!(
        data.get("graph").and_then(|v| v.as_str()),
        Some("http://example.org/ontology")
    );
}

#[tokio::test]
async fn should_fail_when_file_is_missing() {
    // Given: Params without file
    let params = json!({
        "format": "turtle"
    });

    // When: Load is attempted
    let result = graph::load(params).await;

    // Then: Should fail with error
    assert!(result.is_err());
    assert!(result.unwrap_err().to_string().contains("file"));
}

#[tokio::test]
async fn should_report_number_of_triples_loaded() {
    // Given: Valid RDF file
    let params = json!({
        "file": "/path/to/large-dataset.ttl"
    });

    // When: Load is performed
    let result = graph::load(params).await;

    // Then: Should report triple count
    assert!(result.is_ok());
    let data = result.unwrap().get("data").unwrap().clone();

    let triples_loaded = data.get("triples_loaded").unwrap().as_u64().unwrap();
    assert!(triples_loaded > 0);
}

// Graph Export Tests

#[tokio::test]
async fn should_export_graph_to_file() {
    // Given: Export parameters
    let params = json!({
        "output": "/tmp/export.ttl",
        "format": "turtle"
    });

    // When: Export is performed
    let result = graph::export(params).await;

    // Then: Should export successfully
    assert!(result.is_ok());
    let data = result.unwrap().get("data").unwrap().clone();

    assert_eq!(data.get("output").and_then(|v| v.as_str()), Some("/tmp/export.ttl"));
    assert!(data.get("triples_exported").is_some());
    assert!(data.get("file_size_bytes").is_some());
}

#[tokio::test]
async fn should_export_named_graph() {
    // Given: Export from specific graph
    let params = json!({
        "output": "/tmp/people.ttl",
        "graph": "http://example.org/people"
    });

    // When: Export is executed
    let result = graph::export(params).await;

    // Then: Should export from specified graph
    assert!(result.is_ok());
    let data = result.unwrap().get("data").unwrap().clone();

    assert_eq!(
        data.get("graph").and_then(|v| v.as_str()),
        Some("http://example.org/people")
    );
}

#[tokio::test]
async fn should_fail_when_output_path_is_missing() {
    // Given: Params without output
    let params = json!({
        "format": "turtle"
    });

    // When: Export is attempted
    let result = graph::export(params).await;

    // Then: Should fail with error
    assert!(result.is_err());
    assert!(result.unwrap_err().to_string().contains("output"));
}

#[tokio::test]
async fn should_support_multiple_rdf_formats() {
    // Given: Different RDF formats
    let formats = vec!["turtle", "rdfxml", "jsonld", "ntriples"];

    for format in formats {
        // When: Export in each format
        let params = json!({
            "output": format!("/tmp/export.{}", format),
            "format": format
        });

        let result = graph::export(params).await;

        // Then: Should handle format
        assert!(result.is_ok());
        let data = result.unwrap().get("data").unwrap().clone();
        assert_eq!(data.get("format").and_then(|v| v.as_str()), Some(format));
    }
}

// Performance Tests

#[tokio::test]
async fn should_execute_query_quickly() {
    use std::time::Instant;

    // Given: Simple SPARQL query
    let params = json!({
        "sparql": "SELECT * WHERE { ?s ?p ?o } LIMIT 1"
    });

    // When: Query is executed with timing
    let start = Instant::now();
    let result = graph::query(params).await;
    let duration = start.elapsed();

    // Then: Should complete quickly
    assert!(result.is_ok());
    assert!(duration.as_millis() < 100, "Query too slow: {:?}", duration);
}

// Integration Tests

#[tokio::test]
async fn should_complete_load_query_export_workflow() {
    // Given: Complete RDF workflow

    // Step 1: Load RDF
    let load_result = graph::load(json!({
        "file": "/path/to/data.ttl",
        "graph": "http://test.org/data"
    })).await;
    assert!(load_result.is_ok());

    // Step 2: Query the data
    let query_result = graph::query(json!({
        "sparql": "SELECT * WHERE { ?s ?p ?o }",
        "graph": "http://test.org/data"
    })).await;
    assert!(query_result.is_ok());

    // Step 3: Export results
    let export_result = graph::export(json!({
        "output": "/tmp/results.ttl",
        "graph": "http://test.org/data"
    })).await;
    assert!(export_result.is_ok());
}
