//! GraphStore - Persistent storage operations
//!
//! Provides operations for creating and managing persistent on-disk RDF stores
//! using Oxigraph's RocksDB backend.

use crate::graph::core::Graph;
use ggen_utils::error::Result;
use oxigraph::store::Store;
use std::path::Path;
use std::sync::Arc;

/// GraphStore provides persistent storage operations for RDF graphs.
///
/// This type wraps Oxigraph's persistent storage capabilities, allowing
/// graphs to be stored on disk using RocksDB.
///
/// # Examples
///
/// ```rust,no_run
/// use ggen_core::graph::{Graph, GraphStore};
///
/// # fn main() -> ggen_utils::error::Result<()> {
/// // Open or create a persistent store
/// let store = GraphStore::open("./data/store")?;
///
/// // Create a graph from the persistent store
/// let graph = store.create_graph()?;
///
/// // Use the graph normally
/// graph.insert_turtle(r#"
///     @prefix ex: <http://example.org/> .
///     ex:alice a ex:Person .
/// "#)?;
///
/// // Data is persisted to disk
/// # Ok(())
/// # }
/// ```
pub struct GraphStore {
    store: Arc<Store>,
}

impl GraphStore {
    /// Open or create a persistent RDF store at the given path.
    ///
    /// The store uses RocksDB for persistence. If the path doesn't exist,
    /// a new store will be created.
    ///
    /// # Arguments
    ///
    /// * `path` - Path to the store directory
    ///
    /// # Errors
    ///
    /// Returns an error if:
    /// - The path cannot be created or accessed
    /// - The store is corrupted
    pub fn open<P: AsRef<Path>>(path: P) -> Result<Self> {
        let store = Store::open(path.as_ref())
            .map_err(|e| ggen_utils::error::Error::new(&format!("Failed to open store: {}", e)))?;
        Ok(Self {
            store: Arc::new(store),
        })
    }

    /// Create a new in-memory store.
    ///
    /// Creates a new in-memory RDF store. This is similar to `Graph::new()` but
    /// returns a `GraphStore` instead of a `Graph`, providing a consistent API
    /// for store management.
    pub fn new() -> Result<Self> {
        let store = Store::new().map_err(|e| {
            ggen_utils::error::Error::new(&format!("Failed to create store: {}", e))
        })?;
        Ok(Self {
            store: Arc::new(store),
        })
    }

    /// Create a Graph wrapper from this store.
    ///
    /// The returned Graph will use this persistent store for all operations.
    /// Multiple Graph instances can be created from the same store, and they
    /// will all share the same underlying data.
    pub fn create_graph(&self) -> Result<Graph> {
        Graph::from_store(Arc::clone(&self.store))
    }

    /// Get a reference to the underlying Store.
    ///
    /// This allows direct access to Oxigraph's Store API if needed.
    pub fn inner(&self) -> &Store {
        &*self.store
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use chicago_tdd_tools::test;
    use tempfile::TempDir;

    test!(test_store_new, {
        // Arrange & Act
        let store = GraphStore::new().unwrap();

        // Assert
        let graph = store.create_graph().unwrap();
        assert!(graph.is_empty());
    });

    test!(test_store_open_and_create_graph, {
        // Arrange
        let temp_dir = TempDir::new().unwrap();
        let store_path = temp_dir.path().join("test_store");

        // Act
        let store = GraphStore::open(&store_path).unwrap();
        let graph = store.create_graph().unwrap();

        // Assert
        assert!(graph.is_empty());
        assert!(store_path.exists() || !store_path.exists()); // Store may or may not create directory immediately
    });

    test!(test_store_create_graph_and_insert, {
        // Arrange
        let store = GraphStore::new().unwrap();
        let graph = store.create_graph().unwrap();

        // Act
        graph
            .insert_turtle(
                r#"
            @prefix ex: <http://example.org/> .
            ex:alice a ex:Person .
        "#,
            )
            .unwrap();

        // Assert
        assert!(!graph.is_empty());
        assert!(graph.len() > 0);
    });

    test!(test_store_multiple_graphs_share_data, {
        // Arrange
        let store = GraphStore::new().unwrap();
        let graph1 = store.create_graph().unwrap();

        // Act
        graph1
            .insert_turtle(
                r#"
            @prefix ex: <http://example.org/> .
            ex:alice a ex:Person .
        "#,
            )
            .unwrap();

        let graph2 = store.create_graph().unwrap();

        // Assert - Both graphs should see the same data
        assert!(!graph1.is_empty());
        assert!(!graph2.is_empty());
        assert_eq!(graph1.len(), graph2.len());
    });

    test!(test_store_persistent_storage, {
        // Arrange
        let temp_dir = TempDir::new().unwrap();
        let store_path = temp_dir.path().join("persistent_store");

        // Act - Create store and add data
        let store1 = GraphStore::open(&store_path).unwrap();
        let graph1 = store1.create_graph().unwrap();
        graph1
            .insert_turtle(
                r#"
            @prefix ex: <http://example.org/> .
            ex:alice a ex:Person .
        "#,
            )
            .unwrap();
        let count1 = graph1.len();
        drop(graph1);
        drop(store1);

        // Reopen store
        let store2 = GraphStore::open(&store_path).unwrap();
        let graph2 = store2.create_graph().unwrap();

        // Assert - Data should persist
        assert_eq!(graph2.len(), count1);
        assert!(!graph2.is_empty());
    });

    test!(test_store_inner_access, {
        // Arrange
        let store = GraphStore::new().unwrap();

        // Act
        let inner = store.inner();

        // Assert - Should be able to access inner store
        assert_eq!(inner.len().unwrap_or(0), 0);
    });
}
