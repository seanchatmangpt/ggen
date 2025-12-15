//! Graph export operations integration tests
//!
//! Tests RDF export functionality in various formats (Turtle, N-Triples, JSON-LD).

use ggen_core::graph::{Graph, RdfFormat};
use anyhow::Result;
use std::fs;
use tempfile::TempDir;

#[cfg(test)]
mod rdf_export {
    use super::*;

    #[test]
    fn test_export_turtle_format() -> Result<()> {
        // Arrange
        let mut graph = Graph::new()?;
        let n1 = graph.add_node("node1")?;
        let n2 = graph.add_node("node2")?;
        graph.add_edge(&n1, &n2, "connects")?;

        // Act
        let turtle = graph.export_to_string(RdfFormat::Turtle)?;

        // Assert
        assert!(turtle.contains("node1"));
        assert!(turtle.contains("node2"));
        assert!(turtle.contains("connects"));
        assert!(turtle.starts_with("@prefix") || turtle.starts_with("<"));

        Ok(())
    }

    #[test]
    fn test_export_ntriples_format() -> Result<()> {
        // Arrange
        let mut graph = Graph::new()?;
        let n1 = graph.add_node("node1")?;
        let n2 = graph.add_node("node2")?;
        graph.add_edge(&n1, &n2, "connects")?;

        // Act
        let ntriples = graph.export_to_string(RdfFormat::NTriples)?;

        // Assert
        assert!(ntriples.contains("<"));
        assert!(ntriples.contains(">"));
        assert!(ntriples.lines().all(|line| line.ends_with(" .") || line.is_empty()));

        Ok(())
    }

    #[test]
    fn test_export_jsonld_format() -> Result<()> {
        // Arrange
        let mut graph = Graph::new()?;
        let n1 = graph.add_node("node1")?;
        let n2 = graph.add_node("node2")?;
        graph.add_edge(&n1, &n2, "connects")?;

        // Act
        let jsonld = graph.export_to_string(RdfFormat::JsonLd)?;

        // Assert
        assert!(jsonld.starts_with("{") || jsonld.starts_with("["));
        // Validate it's proper JSON
        let _: serde_json::Value = serde_json::from_str(&jsonld)?;

        Ok(())
    }

    #[test]
    fn test_export_empty_graph() -> Result<()> {
        // Arrange
        let graph = Graph::new()?;

        // Act
        let turtle = graph.export_to_string(RdfFormat::Turtle)?;

        // Assert - should produce valid but minimal RDF
        assert!(!turtle.is_empty());

        Ok(())
    }

    #[test]
    fn test_export_to_file() -> Result<()> {
        // Arrange
        let mut graph = Graph::new()?;
        graph.add_node("test")?;
        let temp_dir = TempDir::new()?;
        let file_path = temp_dir.path().join("graph.ttl");

        // Act
        graph.export_to_file(&file_path, RdfFormat::Turtle)?;

        // Assert
        assert!(file_path.exists());
        let content = fs::read_to_string(&file_path)?;
        assert!(!content.is_empty());
        assert!(content.contains("test"));

        Ok(())
    }
}

#[cfg(test)]
mod incremental_export {
    use super::*;

    #[test]
    fn test_incremental_export_new_nodes() -> Result<()> {
        // Arrange
        let mut graph = Graph::new()?;
        let n1 = graph.add_node("node1")?;
        let initial_export = graph.export_to_string(RdfFormat::Turtle)?;

        // Act - add more nodes
        let n2 = graph.add_node("node2")?;
        let incremental = graph.export_delta_to_string(RdfFormat::Turtle)?;

        // Assert
        assert!(!incremental.contains("node1")); // Already exported
        assert!(incremental.contains("node2")); // New node

        Ok(())
    }

    #[test]
    fn test_incremental_export_new_edges() -> Result<()> {
        // Arrange
        let mut graph = Graph::new()?;
        let n1 = graph.add_node("node1")?;
        let n2 = graph.add_node("node2")?;
        let _initial = graph.export_to_string(RdfFormat::Turtle)?;

        // Act
        graph.add_edge(&n1, &n2, "connects")?;
        let incremental = graph.export_delta_to_string(RdfFormat::Turtle)?;

        // Assert
        assert!(incremental.contains("connects"));

        Ok(())
    }

    #[test]
    fn test_reset_export_tracking() -> Result<()> {
        // Arrange
        let mut graph = Graph::new()?;
        graph.add_node("node1")?;
        let _first = graph.export_to_string(RdfFormat::Turtle)?;

        // Act
        graph.reset_export_tracking();
        let second = graph.export_delta_to_string(RdfFormat::Turtle)?;

        // Assert - should re-export everything
        assert!(second.contains("node1"));

        Ok(())
    }
}

#[cfg(test)]
mod large_graph_export {
    use super::*;

    #[test]
    fn test_export_1000_nodes() -> Result<()> {
        // Arrange
        let mut graph = Graph::new()?;
        for i in 0..1000 {
            graph.add_node(&format!("node{}", i))?;
        }

        // Act
        let start = std::time::Instant::now();
        let turtle = graph.export_to_string(RdfFormat::Turtle)?;
        let duration = start.elapsed();

        // Assert
        assert!(duration.as_millis() < 1000); // Should complete in < 1 second
        assert!(!turtle.is_empty());

        Ok(())
    }

    #[test]
    fn test_export_complex_graph() -> Result<()> {
        // Arrange - create a graph with 100 nodes and 500 edges
        let mut graph = Graph::new()?;
        let mut nodes = Vec::new();
        for i in 0..100 {
            nodes.push(graph.add_node(&format!("node{}", i))?);
        }

        for i in 0..500 {
            let source = &nodes[i % 100];
            let target = &nodes[(i + 1) % 100];
            graph.add_edge(source, target, &format!("edge{}", i))?;
        }

        // Act
        let start = std::time::Instant::now();
        let turtle = graph.export_to_string(RdfFormat::Turtle)?;
        let duration = start.elapsed();

        // Assert
        assert!(duration.as_millis() < 500); // Complex graph should export quickly
        assert!(!turtle.is_empty());
        assert_eq!(graph.node_count(), 100);
        assert_eq!(graph.edge_count(), 500);

        Ok(())
    }

    #[test]
    fn test_export_all_formats_consistency() -> Result<()> {
        // Arrange
        let mut graph = Graph::new()?;
        let n1 = graph.add_node("node1")?;
        let n2 = graph.add_node("node2")?;
        graph.add_edge(&n1, &n2, "connects")?;

        // Act - export in all formats
        let turtle = graph.export_to_string(RdfFormat::Turtle)?;
        let ntriples = graph.export_to_string(RdfFormat::NTriples)?;
        let jsonld = graph.export_to_string(RdfFormat::JsonLd)?;

        // Assert - all should contain the same information
        assert!(turtle.contains("node1"));
        assert!(ntriples.contains("node1"));
        assert!(jsonld.contains("node1"));

        Ok(())
    }
}
