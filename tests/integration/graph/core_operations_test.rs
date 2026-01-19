//! Core graph operations integration tests
//!
//! Tests the fundamental graph operations (node/edge creation, retrieval, validation)
//! focusing on the 80/20 critical paths.

use ggen_core::graph::{Graph, Node, Edge, NodeId, EdgeId, GraphError};
use anyhow::Result;

#[cfg(test)]
mod node_operations {
    use super::*;

    #[test]
    fn test_node_creation_basic() -> Result<()> {
        // Arrange
        let mut graph = Graph::new()?;

        // Act
        let node_id = graph.add_node("test-node")?;

        // Assert
        assert!(graph.contains_node(&node_id));
        assert_eq!(graph.node_count(), 1);

        Ok(())
    }

    #[test]
    fn test_node_retrieval_existing() -> Result<()> {
        // Arrange
        let mut graph = Graph::new()?;
        let node_id = graph.add_node("test-node")?;

        // Act
        let node = graph.get_node(&node_id)?;

        // Assert
        assert_eq!(node.id(), &node_id);

        Ok(())
    }

    #[test]
    fn test_node_retrieval_nonexistent() {
        // Arrange
        let graph = Graph::new().unwrap();
        let fake_id = NodeId::new("nonexistent");

        // Act
        let result = graph.get_node(&fake_id);

        // Assert
        assert!(result.is_err());
        assert!(matches!(result, Err(GraphError::NodeNotFound(_))));
    }

    #[test]
    fn test_node_removal() -> Result<()> {
        // Arrange
        let mut graph = Graph::new()?;
        let node_id = graph.add_node("test-node")?;
        assert_eq!(graph.node_count(), 1);

        // Act
        graph.remove_node(&node_id)?;

        // Assert
        assert_eq!(graph.node_count(), 0);
        assert!(!graph.contains_node(&node_id));

        Ok(())
    }

    #[test]
    fn test_node_metadata() -> Result<()> {
        // Arrange
        let mut graph = Graph::new()?;
        let node_id = graph.add_node("test-node")?;

        // Act
        graph.set_node_metadata(&node_id, "key", "value")?;
        let value = graph.get_node_metadata(&node_id, "key")?;

        // Assert
        assert_eq!(value, Some("value".to_string()));

        Ok(())
    }
}

#[cfg(test)]
mod edge_operations {
    use super::*;

    #[test]
    fn test_edge_creation_basic() -> Result<()> {
        // Arrange
        let mut graph = Graph::new()?;
        let source = graph.add_node("source")?;
        let target = graph.add_node("target")?;

        // Act
        let edge_id = graph.add_edge(&source, &target, "connects")?;

        // Assert
        assert!(graph.contains_edge(&edge_id));
        assert_eq!(graph.edge_count(), 1);

        Ok(())
    }

    #[test]
    fn test_edge_retrieval() -> Result<()> {
        // Arrange
        let mut graph = Graph::new()?;
        let source = graph.add_node("source")?;
        let target = graph.add_node("target")?;
        let edge_id = graph.add_edge(&source, &target, "connects")?;

        // Act
        let edge = graph.get_edge(&edge_id)?;

        // Assert
        assert_eq!(edge.source(), &source);
        assert_eq!(edge.target(), &target);

        Ok(())
    }

    #[test]
    fn test_edge_to_nonexistent_node() {
        // Arrange
        let mut graph = Graph::new().unwrap();
        let source = graph.add_node("source").unwrap();
        let fake_target = NodeId::new("nonexistent");

        // Act
        let result = graph.add_edge(&source, &fake_target, "connects");

        // Assert
        assert!(result.is_err());
        assert!(matches!(result, Err(GraphError::NodeNotFound(_))));
    }

    #[test]
    fn test_edge_traversal_forward() -> Result<()> {
        // Arrange
        let mut graph = Graph::new()?;
        let n1 = graph.add_node("node1")?;
        let n2 = graph.add_node("node2")?;
        let n3 = graph.add_node("node3")?;
        graph.add_edge(&n1, &n2, "next")?;
        graph.add_edge(&n2, &n3, "next")?;

        // Act
        let outgoing = graph.outgoing_edges(&n1)?;

        // Assert
        assert_eq!(outgoing.len(), 1);

        Ok(())
    }

    #[test]
    fn test_edge_traversal_backward() -> Result<()> {
        // Arrange
        let mut graph = Graph::new()?;
        let n1 = graph.add_node("node1")?;
        let n2 = graph.add_node("node2")?;
        graph.add_edge(&n1, &n2, "next")?;

        // Act
        let incoming = graph.incoming_edges(&n2)?;

        // Assert
        assert_eq!(incoming.len(), 1);

        Ok(())
    }
}

#[cfg(test)]
mod graph_validation {
    use super::*;

    #[test]
    fn test_graph_invariants_empty() -> Result<()> {
        // Arrange
        let graph = Graph::new()?;

        // Act & Assert
        assert_eq!(graph.node_count(), 0);
        assert_eq!(graph.edge_count(), 0);
        assert!(graph.validate_invariants().is_ok());

        Ok(())
    }

    #[test]
    fn test_graph_invariants_with_orphaned_edges() {
        // This test verifies that removing a node also removes connected edges
        // (testing referential integrity)

        // Arrange
        let mut graph = Graph::new().unwrap();
        let n1 = graph.add_node("node1").unwrap();
        let n2 = graph.add_node("node2").unwrap();
        graph.add_edge(&n1, &n2, "connects").unwrap();

        // Act
        graph.remove_node(&n1).unwrap();

        // Assert - edge should be removed automatically
        assert_eq!(graph.edge_count(), 0);
        assert!(graph.validate_invariants().is_ok());
    }

    #[test]
    fn test_graph_cycle_detection() -> Result<()> {
        // Arrange
        let mut graph = Graph::new()?;
        let n1 = graph.add_node("node1")?;
        let n2 = graph.add_node("node2")?;
        let n3 = graph.add_node("node3")?;

        // Create a cycle: n1 -> n2 -> n3 -> n1
        graph.add_edge(&n1, &n2, "next")?;
        graph.add_edge(&n2, &n3, "next")?;
        graph.add_edge(&n3, &n1, "next")?;

        // Act
        let has_cycle = graph.detect_cycles()?;

        // Assert
        assert!(has_cycle);

        Ok(())
    }

    #[test]
    fn test_graph_acyclic() -> Result<()> {
        // Arrange
        let mut graph = Graph::new()?;
        let n1 = graph.add_node("node1")?;
        let n2 = graph.add_node("node2")?;
        let n3 = graph.add_node("node3")?;

        // Create a DAG: n1 -> n2 -> n3
        graph.add_edge(&n1, &n2, "next")?;
        graph.add_edge(&n2, &n3, "next")?;

        // Act
        let has_cycle = graph.detect_cycles()?;

        // Assert
        assert!(!has_cycle);

        Ok(())
    }

    #[test]
    fn test_graph_topological_sort() -> Result<()> {
        // Arrange
        let mut graph = Graph::new()?;
        let n1 = graph.add_node("node1")?;
        let n2 = graph.add_node("node2")?;
        let n3 = graph.add_node("node3")?;

        // Create dependencies: n3 depends on n2, n2 depends on n1
        graph.add_edge(&n1, &n2, "depends")?;
        graph.add_edge(&n2, &n3, "depends")?;

        // Act
        let sorted = graph.topological_sort()?;

        // Assert
        assert_eq!(sorted.len(), 3);
        // n1 should come before n2, and n2 before n3
        let pos1 = sorted.iter().position(|n| n == &n1).unwrap();
        let pos2 = sorted.iter().position(|n| n == &n2).unwrap();
        let pos3 = sorted.iter().position(|n| n == &n3).unwrap();
        assert!(pos1 < pos2);
        assert!(pos2 < pos3);

        Ok(())
    }
}

#[cfg(test)]
mod concurrent_access {
    use super::*;
    use std::sync::Arc;
    use std::thread;

    #[test]
    fn test_concurrent_read_access() -> Result<()> {
        // Arrange
        let mut graph = Graph::new()?;
        let node_id = graph.add_node("test")?;
        let graph = Arc::new(graph);

        // Act - spawn multiple readers
        let handles: Vec<_> = (0..10)
            .map(|_| {
                let g = Arc::clone(&graph);
                let nid = node_id.clone();
                thread::spawn(move || {
                    g.get_node(&nid).unwrap();
                })
            })
            .collect();

        // Assert - all threads complete successfully
        for handle in handles {
            handle.join().unwrap();
        }

        Ok(())
    }

    // Note: Concurrent write tests would require Arc<RwLock<Graph>> or similar
    // This is a design decision for the Graph implementation
}
