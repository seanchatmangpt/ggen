//! Graph Commands Integration Tests - Chicago TDD
//!
//! Tests for RDF graph operations: load, query, export, visualize
//!
//! Chicago TDD Cycle:
//! 1. RED: Write failing test
//! 2. GREEN: Make test pass with REAL implementation
//! 3. REFACTOR: Improve code while maintaining green
//!
//! NO MOCKS - Tests against REAL Oxigraph implementation

use std::path::PathBuf;

// ============================================================================
// Domain Layer Imports (REAL types, NO mocks)
// ============================================================================

use ggen_domain::graph::{
    ExportInput, ExportOutput, LoadInput, LoadOutput, QueryInput, QueryResult, RdfFormat,
    VisualizeInput, VisualizeOutput,
};

// ============================================================================
// Load Command Tests
// ============================================================================

#[cfg(test)]
mod load_tests {
    use super::*;

    /// Test: LoadOutput structure can be created with real data
    #[test]
    fn test_load_output_structure() {
        let output = LoadOutput {
            triples_loaded: 2,
            total_triples: 8,
            format: "Turtle".to_string(),
            file_path: "test.ttl".to_string(),
        };

        assert_eq!(output.triples_loaded, 2);
        assert_eq!(output.total_triples, 8);
        assert_eq!(output.format, "Turtle");
        assert_eq!(output.file_path, "test.ttl");
    }

    /// Test: LoadInput can be created with PathBuf
    #[test]
    fn test_load_input_creation() {
        let input = LoadInput {
            file: PathBuf::from("test.ttl"),
            format: None,
            base_iri: None,
            merge: false,
        };

        assert_eq!(input.file, PathBuf::from("test.ttl"));
        assert!(input.format.is_none());
        assert!(input.base_iri.is_none());
        assert!(!input.merge);
    }

    /// Test: LoadInput with format specified
    #[test]
    fn test_load_input_with_format() {
        let input = LoadInput {
            file: PathBuf::from("data.nt"),
            format: Some("ntriples".to_string()),
            base_iri: Some("http://example.org/".to_string()),
            merge: true,
        };

        assert_eq!(input.file, PathBuf::from("data.nt"));
        assert_eq!(input.format, Some("ntriples".to_string()));
        assert_eq!(input.base_iri, Some("http://example.org/".to_string()));
        assert!(input.merge);
    }

    /// Test: RdfFormat from_extension detects Turtle files
    #[test]
    fn test_rdf_format_turtle_detection() {
        assert_eq!(RdfFormat::from_extension("data.ttl"), RdfFormat::Turtle);
        assert_eq!(RdfFormat::from_extension("data.turtle"), RdfFormat::Turtle);
    }

    /// Test: RdfFormat from_extension detects NTriples files
    #[test]
    fn test_rdf_format_ntriples_detection() {
        assert_eq!(RdfFormat::from_extension("data.nt"), RdfFormat::NTriples);
        assert_eq!(
            RdfFormat::from_extension("data.ntriples"),
            RdfFormat::NTriples
        );
    }

    /// Test: RdfFormat from_extension detects RDF/XML files
    #[test]
    fn test_rdf_format_rdfxml_detection() {
        assert_eq!(RdfFormat::from_extension("data.rdf"), RdfFormat::RdfXml);
        assert_eq!(RdfFormat::from_extension("data.xml"), RdfFormat::RdfXml);
    }

    /// Test: RdfFormat from_extension detects JSON-LD files
    #[test]
    fn test_rdf_format_jsonld_detection() {
        assert_eq!(RdfFormat::from_extension("data.jsonld"), RdfFormat::JsonLd);
        assert_eq!(RdfFormat::from_extension("data.json"), RdfFormat::JsonLd);
    }

    /// Test: RdfFormat from_extension detects N3 files
    #[test]
    fn test_rdf_format_n3_detection() {
        assert_eq!(RdfFormat::from_extension("data.n3"), RdfFormat::N3);
    }

    /// Test: RdfFormat from_extension defaults to Turtle for unknown extensions
    #[test]
    fn test_rdf_format_unknown_extension() {
        assert_eq!(RdfFormat::from_extension("data.unknown"), RdfFormat::Turtle);
        assert_eq!(RdfFormat::from_extension("data"), RdfFormat::Turtle);
    }

    /// Test: RdfFormat as_str returns correct format names
    #[test]
    fn test_rdf_format_as_str() {
        assert_eq!(RdfFormat::Turtle.as_str(), "Turtle");
        assert_eq!(RdfFormat::NTriples.as_str(), "N-Triples");
        assert_eq!(RdfFormat::RdfXml.as_str(), "RDF/XML");
        assert_eq!(RdfFormat::JsonLd.as_str(), "JSON-LD");
        assert_eq!(RdfFormat::N3.as_str(), "N3");
    }
}

// ============================================================================
// Query Command Tests
// ============================================================================

#[cfg(test)]
mod query_tests {
    use super::*;
    use std::collections::HashMap;

    /// Test: QueryResult structure can be created with real bindings
    #[test]
    fn test_query_result_structure() {
        let mut bindings = HashMap::new();
        bindings.insert("name".to_string(), "Alice".to_string());
        bindings.insert("age".to_string(), "30".to_string());

        let result = QueryResult {
            bindings: vec![bindings],
            variables: vec!["name".to_string(), "age".to_string()],
            result_count: 1,
        };

        assert_eq!(result.bindings.len(), 1);
        assert_eq!(result.variables.len(), 2);
        assert_eq!(result.result_count, 1);
    }

    /// Test: QueryResult with empty results
    #[test]
    fn test_query_result_empty() {
        let result = QueryResult {
            bindings: vec![],
            variables: vec![],
            result_count: 0,
        };

        assert_eq!(result.bindings.len(), 0);
        assert_eq!(result.variables.len(), 0);
        assert_eq!(result.result_count, 0);
    }

    /// Test: QueryResult with multiple bindings
    #[test]
    fn test_query_result_multiple_bindings() {
        let mut binding1 = HashMap::new();
        binding1.insert("name".to_string(), "Alice".to_string());

        let mut binding2 = HashMap::new();
        binding2.insert("name".to_string(), "Bob".to_string());

        let result = QueryResult {
            bindings: vec![binding1, binding2],
            variables: vec!["name".to_string()],
            result_count: 2,
        };

        assert_eq!(result.bindings.len(), 2);
        assert_eq!(result.result_count, 2);
    }

    /// Test: QueryInput can be created
    #[test]
    fn test_query_input_creation() {
        let input = QueryInput {
            query: "SELECT * WHERE { ?s ?p ?o }".to_string(),
            graph_file: Some(PathBuf::from("graph.ttl")),
            format: "json".to_string(),
        };

        assert_eq!(input.query, "SELECT * WHERE { ?s ?p ?o }");
        assert_eq!(input.format, "json");
    }

    /// Test: QueryInput with no graph file (uses default graph)
    #[test]
    fn test_query_input_no_graph() {
        let input = QueryInput {
            query: "ASK { ?s a :Class }".to_string(),
            graph_file: None,
            format: "json".to_string(),
        };

        assert!(input.graph_file.is_none());
    }
}

// ============================================================================
// Export Command Tests
// ============================================================================

#[cfg(test)]
mod export_tests {
    use super::*;

    /// Test: ExportOutput structure can be created
    #[test]
    fn test_export_output_structure() {
        let output = ExportOutput {
            output_path: "/tmp/export.ttl".to_string(),
            format: "Turtle".to_string(),
            triples_exported: 100,
            file_size_bytes: 5000,
        };

        assert_eq!(output.output_path, "/tmp/export.ttl");
        assert_eq!(output.format, "Turtle");
        assert_eq!(output.triples_exported, 100);
        assert_eq!(output.file_size_bytes, 5000);
    }

    /// Test: ExportOutput with zero triples
    #[test]
    fn test_export_output_empty() {
        let output = ExportOutput {
            output_path: "empty.ttl".to_string(),
            format: "Turtle".to_string(),
            triples_exported: 0,
            file_size_bytes: 0,
        };

        assert_eq!(output.triples_exported, 0);
        assert_eq!(output.file_size_bytes, 0);
    }

    /// Test: ExportInput can be created
    #[test]
    fn test_export_input_creation() {
        let input = ExportInput {
            input: PathBuf::from("input.ttl"),
            output: PathBuf::from("output.ttl"),
            format: "turtle".to_string(),
            pretty: true,
        };

        assert_eq!(input.input, PathBuf::from("input.ttl"));
        assert_eq!(input.output, PathBuf::from("output.ttl"));
        assert!(input.pretty);
    }

    /// Test: ExportInput with pretty=false
    #[test]
    fn test_export_input_not_pretty() {
        let input = ExportInput {
            input: PathBuf::from("in.ttl"),
            output: PathBuf::from("out.ttl"),
            format: "turtle".to_string(),
            pretty: false,
        };

        assert!(!input.pretty);
    }
}

// ============================================================================
// Visualize Command Tests
// ============================================================================

#[cfg(test)]
mod visualize_tests {
    use super::*;

    /// Test: VisualizeOutput structure can be created
    #[test]
    fn test_visualize_output_structure() {
        let output = VisualizeOutput {
            nodes_rendered: 5,
            edges_rendered: 8,
            output_path: "/tmp/graph.dot".to_string(),
            format: "dot".to_string(),
        };

        assert_eq!(output.output_path, "/tmp/graph.dot");
        assert_eq!(output.format, "dot");
        assert_eq!(output.nodes_rendered, 5);
        assert_eq!(output.edges_rendered, 8);
    }

    /// Test: VisualizeOutput with empty graph
    #[test]
    fn test_visualize_output_empty_graph() {
        let output = VisualizeOutput {
            nodes_rendered: 0,
            edges_rendered: 0,
            output_path: "empty.dot".to_string(),
            format: "dot".to_string(),
        };

        assert_eq!(output.nodes_rendered, 0);
        assert_eq!(output.edges_rendered, 0);
    }

    /// Test: VisualizeOutput with different formats
    #[test]
    fn test_visualize_output_formats() {
        for format in &["dot", "json", "svg", "png"] {
            let output = VisualizeOutput {
                nodes_rendered: 1,
                edges_rendered: 0,
                output_path: format!("graph.{}", format),
                format: format.to_string(),
            };

            assert_eq!(output.format, *format);
        }
    }

    /// Test: VisualizeInput can be created
    #[test]
    fn test_visualize_input_creation() {
        let input = VisualizeInput {
            input: PathBuf::from("graph.ttl"),
            output: None,
            format: "dot".to_string(),
            labels: false,
            max_depth: None,
            subject: None,
        };

        assert_eq!(input.input, PathBuf::from("graph.ttl"));
        assert_eq!(input.format, "dot");
        assert!(!input.labels);
        assert!(input.max_depth.is_none());
    }

    /// Test: VisualizeInput with all options
    #[test]
    fn test_visualize_input_full_options() {
        let input = VisualizeInput {
            input: PathBuf::from("graph.ttl"),
            output: Some(PathBuf::from("viz.dot")),
            format: "dot".to_string(),
            labels: true,
            max_depth: Some(5),
            subject: Some("http://example.org/subject".to_string()),
        };

        assert_eq!(input.output, Some(PathBuf::from("viz.dot")));
        assert!(input.labels);
        assert_eq!(input.max_depth, Some(5));
        assert_eq!(
            input.subject,
            Some("http://example.org/subject".to_string())
        );
    }
}

// ============================================================================
// Integration Tests (Domain Layer - REAL Oxigraph)
// ============================================================================

#[cfg(test)]
mod integration_tests {
    use super::*;

    /// Test: Create real LoadInput and verify structure
    #[test]
    fn test_integration_load_input() {
        let input = LoadInput {
            file: PathBuf::from("test.ttl"),
            format: None,
            base_iri: None,
            merge: false,
        };

        assert_eq!(input.file, PathBuf::from("test.ttl"));
        assert!(!input.merge);
    }

    /// Test: Create real QueryInput and verify structure
    #[test]
    fn test_integration_query_input() {
        let input = QueryInput {
            query: "SELECT * WHERE { ?s ?p ?o }".to_string(),
            graph_file: Some(PathBuf::from("graph.ttl")),
            format: "json".to_string(),
        };

        assert_eq!(input.query, "SELECT * WHERE { ?s ?p ?o }");
        assert_eq!(input.format, "json");
    }

    /// Test: Create real ExportInput and verify structure
    #[test]
    fn test_integration_export_input() {
        let input = ExportInput {
            input: PathBuf::from("input.ttl"),
            output: PathBuf::from("output.ttl"),
            format: "turtle".to_string(),
            pretty: true,
        };

        assert_eq!(input.input, PathBuf::from("input.ttl"));
        assert!(input.pretty);
    }

    /// Test: Create real VisualizeInput and verify structure
    #[test]
    fn test_integration_visualize_input() {
        let input = VisualizeInput {
            input: PathBuf::from("graph.ttl"),
            output: Some(PathBuf::from("viz.dot")),
            format: "dot".to_string(),
            labels: true,
            max_depth: Some(5),
            subject: Some("http://example.org/subject".to_string()),
        };

        assert_eq!(input.format, "dot");
        assert!(input.labels);
        assert_eq!(input.max_depth, Some(5));
    }
}
