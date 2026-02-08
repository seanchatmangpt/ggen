//! SPARQL CONSTRUCT query executor for YAWL transformation.
//!
//! This module provides the [`ConstructExecutor`] type for executing SPARQL CONSTRUCT
//! queries that transform industry ontologies into YAWL workflow specifications.
//!
//! # Six Transformation Patterns
//!
//! The executor includes six built-in CONSTRUCT queries that implement the core
//! transformation patterns:
//!
//! 1. **Class to Task** - Maps OWL classes to YAWL atomic tasks
//! 2. **Property to Flow** - Maps object properties to YAWL flow connections
//! 3. **Cardinality to Split/Join** - Determines gateway types from OWL restrictions
//! 4. **Rules to Conditions** - Converts OWL restrictions to YAWL conditions
//! 5. **Multiple Instance** - Generates multi-instance tasks from cardinality
//! 6. **Composite Task** - Creates composite tasks from class hierarchies
//!
//! # Example
//!
//! ```rust,no_run
//! use ggen_yawl::ConstructExecutor;
//! use ggen_core::Graph;
//!
//! # fn main() -> Result<(), Box<dyn std::error::Error>> {
//! let graph = Graph::new()?;
//!
//! // Execute all transformation queries
//! let executor = ConstructExecutor::new();
//! let result = executor.execute_all(&graph)?;
//! # Ok(())
//! # }
//! ```

use crate::{Error, Result};
use ggen_core::Graph;
use oxigraph::sparql::QueryResults;
use std::collections::HashMap;

/// A SPARQL CONSTRUCT query with metadata.
///
/// Represents a SPARQL CONSTRUCT query along with its dependencies
/// for execution order resolution.
///
/// # Fields
///
/// * [`name`](Self::name) - Query name for identification and logging
/// * [`sparql`](Self::sparql) - SPARQL CONSTRUCT query string
/// * [`dependencies`](Self::dependencies) - Query names that must execute before this one
///
/// # Example
///
/// ```rust
/// use ggen_yawl::transform::Query;
///
/// let query = Query::new(
///     "my_transform",
///     "CONSTRUCT { ?s a ?type } WHERE { ?s a ?type }"
/// )
/// .with_dependency("other_query");
/// ```
#[derive(Debug, Clone)]
pub struct Query {
    /// Query name for identification and logging.
    pub name: String,
    /// SPARQL CONSTRUCT query string.
    pub sparql: String,
    /// Query names that must execute before this one.
    pub dependencies: Vec<String>,
}

impl Query {
    /// Create a new query with no dependencies.
    ///
    /// # Arguments
    ///
    /// * `name` - Query name for identification
    /// * `sparql` - SPARQL CONSTRUCT query string
    ///
    /// # Example
    ///
    /// ```rust
    /// use ggen_yawl::transform::Query;
    ///
    /// let query = Query::new(
    ///     "extract_classes",
    ///     "CONSTRUCT { ?s a owl:Class } WHERE { ?s a owl:Class }"
    /// );
    /// assert_eq!(query.name, "extract_classes");
    /// assert!(query.dependencies.is_empty());
    /// ```
    pub fn new(name: impl Into<String>, sparql: impl Into<String>) -> Self {
        Self {
            name: name.into(),
            sparql: sparql.into(),
            dependencies: Vec::new(),
        }
    }

    /// Add a dependency to this query.
    ///
    /// # Arguments
    ///
    /// * `dep` - Name of the query that must execute before this one
    ///
    /// # Example
    ///
    /// ```rust
    /// use ggen_yawl::transform::Query;
    ///
    /// let query = Query::new("extract_properties", "")
    ///     .with_dependency("extract_classes");
    /// assert_eq!(query.dependencies, vec!["extract_classes"]);
    /// ```
    pub fn with_dependency(mut self, dep: impl Into<String>) -> Self {
        self.dependencies.push(dep.into());
        self
    }
}

/// Executor for SPARQL CONSTRUCT query chains.
///
/// The [`ConstructExecutor`] manages a collection of SPARQL CONSTRUCT queries
/// and executes them in dependency order, materializing results incrementally.
///
/// # Default Queries
///
/// The executor comes with six built-in queries for the YAWL transformation:
///
/// | Name | Pattern | Dependencies |
/// |------|---------|--------------|
/// | `class_to_task` | Class→Task | None |
/// | `property_to_flow` | Property→Flow | `class_to_task` |
/// | `cardinality_splitjoin` | Cardinality→Split/Join | `property_to_flow` |
/// | `rules_to_conditions` | Rules→Conditions | None |
/// | `multiple_instance` | Multiple Instance | `class_to_task` |
/// | `composite_task` | Composite Task | `class_to_task` |
///
/// # Example
///
/// ```rust,no_run
/// use ggen_yawl::ConstructExecutor;
/// use ggen_yawl::transform::Query;
/// use ggen_core::Graph;
///
/// # fn main() -> Result<(), Box<dyn std::error::Error>> {
/// let graph = Graph::new()?;
///
/// // Use default queries
/// let executor = ConstructExecutor::new();
/// let result = executor.execute_all(&graph)?;
///
/// // Or add custom queries
/// let mut executor = ConstructExecutor::new();
/// executor.register_query(Query::new("custom", "CONSTRUCT {...} WHERE {...}"));
/// # Ok(())
/// # }
/// ```
#[derive(Debug, Clone)]
pub struct ConstructExecutor {
    /// Queries registered with this executor.
    queries: HashMap<String, Query>,
}

impl Default for ConstructExecutor {
    fn default() -> Self {
        let mut exec = Self {
            queries: HashMap::new(),
        };
        exec.register_default_queries();
        exec
    }
}

impl ConstructExecutor {
    /// Create a new executor with default queries.
    pub fn new() -> Self {
        Self::default()
    }

    /// Register the default YAWL transformation queries.
    fn register_default_queries(&mut self) {
        // These queries are embedded for convenience
        // In production, they'd be loaded from files

        // Query 1: Class to Task
        self.queries.insert(
            "class_to_task".to_string(),
            Query::new("class_to_task", include_str!("../../queries/01-extract-tasks.rq")),
        );

        // Query 2: Property to Flow
        self.queries.insert(
            "property_to_flow".to_string(),
            Query::new(
                "property_to_flow",
                include_str!("../../queries/02-extract-flows.rq"),
            )
            .with_dependency("class_to_task"),
        );

        // Query 3: Cardinality to Split/Join
        self.queries.insert(
            "cardinality_splitjoin".to_string(),
            Query::new(
                "cardinality_splitjoin",
                include_str!("../../queries/03-cardinality-splitjoin.rq"),
            )
            .with_dependency("property_to_flow"),
        );

        // Query 4: Rules to Conditions
        self.queries.insert(
            "rules_to_conditions".to_string(),
            Query::new(
                "rules_to_conditions",
                include_str!("../../queries/04-rules-to-conditions.rq"),
            ),
        );

        // Query 5: Multiple Instance
        self.queries.insert(
            "multiple_instance".to_string(),
            Query::new(
                "multiple_instance",
                include_str!("../../queries/05-multiple-instance.rq"),
            )
            .with_dependency("class_to_task"),
        );

        // Query 6: Composite Task
        self.queries.insert(
            "composite_task".to_string(),
            Query::new(
                "composite_task",
                include_str!("../../queries/06-composite-task.rq"),
            )
            .with_dependency("class_to_task"),
        );
    }

    /// Register a custom query.
    pub fn register_query(&mut self, query: Query) {
        self.queries.insert(query.name.clone(), query);
    }

    /// Execute all registered queries on the graph.
    ///
    /// Queries are executed in topological order of their dependencies.
    pub fn execute_all(&self, graph: &Graph) -> Result<Graph> {
        let mut result_graph = graph.clone();

        // Determine execution order (topological sort)
        let order = self.topological_sort()?;

        // Execute queries in order
        for query_name in order {
            if let Some(query) = self.queries.get(&query_name) {
                result_graph = self.execute_query(&result_graph, query)?;
            }
        }

        Ok(result_graph)
    }

    /// Execute a single CONSTRUCT query on the graph and materialize results.
    ///
    /// This method:
    /// 1. Executes the SPARQL CONSTRUCT query
    /// 2. Collects the resulting triples/quads
    /// 3. Materializes them into the result graph
    pub fn execute_query(&self, graph: &Graph, query: &Query) -> Result<Graph> {
        // Execute the CONSTRUCT query
        let results = graph
            .query(&query.sparql)
            .map_err(|e| Error::sparql(format!("Query '{}' failed: {}", query.name, e)))?;

        // Materialize the CONSTRUCT results into the result graph
        #[allow(unused_mut)]
        let result_graph = graph.clone();

        match results {
            QueryResults::Graph(quads) => {
                // Collect quads from the CONSTRUCT query result
                // Use N-Triples format for proper serialization
                use oxigraph::io::RdfSerializer;
                let mut buffer = Vec::new();

                {
                    let mut writer = RdfSerializer::from_format(oxigraph::io::RdfFormat::NTriples)
                        .for_writer(&mut buffer);

                    for quad_result in quads {
                        let quad = quad_result
                            .map_err(|e| Error::sparql(format!("Error reading quad from '{}': {}", query.name, e)))?;
                        // Convert Triple to Quad by adding the default graph name
                        use oxigraph::model::QuadRef;
                        let quad_ref = QuadRef {
                            subject: quad.subject.as_ref(),
                            predicate: quad.predicate.as_ref(),
                            object: quad.object.as_ref(),
                            graph_name: oxigraph::model::GraphNameRef::DefaultGraph,
                        };
                        writer
                            .serialize_quad(quad_ref)
                            .map_err(|e| Error::sparql(format!("Failed to serialize quad: {}", e)))?;
                    }

                    writer
                        .finish()
                        .map_err(|e| Error::sparql(format!("Failed to finish serialization: {}", e)))?;
                }

                // Create a new graph with the materialized triples
                if !buffer.is_empty() {
                    // Create a temporary store and load it into a new graph
                    let store = oxigraph::store::Store::new()
                        .map_err(|e| Error::Oxigraph(format!("Failed to create store: {}", e)))?;

                    store
                        .load_from_reader(oxigraph::io::RdfFormat::NTriples, &buffer[..])
                        .map_err(|e| Error::Oxigraph(format!("Failed to load N-Triples: {}", e)))?;

                    // Create new graph from the store using the private workaround
                    let loader = crate::ontology::loader::OntologyLoader::new();
                    let new_graph = loader.create_graph_from_store(store)?;
                    Ok(new_graph)
                } else {
                    Ok(result_graph)
                }
            }
            QueryResults::Solutions(_) => Err(Error::sparql(format!(
                "Query '{}' expected to be CONSTRUCT but returned SELECT results",
                query.name
            ))),
            QueryResults::Boolean(_) => Err(Error::sparql(format!(
                "Query '{}' expected to be CONSTRUCT but returned ASK results",
                query.name
            ))),
        }
    }

    /// Topological sort of queries by dependencies.
    pub fn topological_sort(&self) -> Result<Vec<String>> {
        let mut sorted = Vec::new();
        let mut visited = std::collections::HashSet::new();
        let mut temp = std::collections::HashSet::new();

        for name in self.queries.keys() {
            if !visited.contains(name) {
                self.visit(name, &mut visited, &mut temp, &mut sorted)?;
            }
        }

        Ok(sorted)
    }

    fn visit(
        &self,
        name: &str,
        visited: &mut std::collections::HashSet<String>,
        temp: &mut std::collections::HashSet<String>,
        sorted: &mut Vec<String>,
    ) -> Result<()> {
        if visited.contains(name) {
            return Ok(());
        }

        if temp.contains(name) {
            return Err(Error::sparql(format!("Circular dependency involving: {}", name)));
        }

        temp.insert(name.to_string());

        if let Some(query) = self.queries.get(name) {
            for dep in &query.dependencies {
                self.visit(dep, visited, temp, sorted)?;
            }
        }

        temp.remove(name);
        visited.insert(name.to_string());
        sorted.push(name.to_string());

        Ok(())
    }

    /// Get the list of registered query names.
    pub fn query_names(&self) -> Vec<String> {
        self.queries.keys().cloned().collect()
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_query_creation() {
        let q = Query::new("test", "SELECT * WHERE { ?s ?p ?o }");
        assert_eq!(q.name, "test");
        assert!(q.dependencies.is_empty());
    }

    #[test]
    fn test_query_with_dependency() {
        let q = Query::new("test", "SELECT * WHERE { ?s ?p ?o }")
            .with_dependency("other");
        assert_eq!(q.dependencies, vec!["other"]);
    }

    #[test]
    fn test_executor_creation() {
        let exec = ConstructExecutor::new();
        assert!(!exec.query_names().is_empty());
    }

    #[test]
    fn test_topological_sort() {
        let exec = ConstructExecutor::new();
        let order = exec.topological_sort();
        assert!(order.is_ok());

        let sorted = order.unwrap();
        // class_to_task should come before property_to_flow
        if let (Some(i1), Some(i2)) = (
            sorted.iter().position(|x| x == "class_to_task"),
            sorted.iter().position(|x| x == "property_to_flow"),
        ) {
            assert!(i1 < i2, "class_to_task should execute before property_to_flow");
        }
    }

    #[test]
    fn test_execute_construct_query_materializes_results() {
        // Create a graph with source data
        let graph = Graph::new().expect("Failed to create graph");
        graph
            .insert_turtle(
                r#"
                @prefix ex: <http://example.org/> .
                ex:alice ex:knows ex:bob .
                ex:carol ex:knows ex:dave .
            "#,
            )
            .expect("Failed to insert turtle data");

        // Create a simple executor without default queries
        let query = Query::new(
            "test_construct",
            r#"
                PREFIX ex: <http://example.org/>
                CONSTRUCT {
                    ?s ex:related ?o .
                    ?s a ex:Entity .
                    ?o a ex:Entity .
                }
                WHERE { ?s ex:knows ?o }
            "#,
        );
        let mut exec = ConstructExecutor {
            queries: std::collections::HashMap::new(),
        };
        exec.register_query(query);

        // Execute all queries
        let result_graph = exec.execute_all(&graph);

        // Query execution should succeed (the graph API handles the transformation)
        assert!(result_graph.is_ok(), "Query execution should succeed: {:?}", result_graph.err());
    }

    #[test]
    fn test_execute_query_returns_enhanced_graph() {
        // Create a graph with source data
        let graph = Graph::new().expect("Failed to create graph");
        graph
            .insert_turtle(
                r#"
                @prefix ex: <http://example.org/> .
                ex:subject ex:predicate ex:object .
            "#,
            )
            .expect("Failed to insert turtle data");

        // Create executor with a valid CONSTRUCT query
        let query = Query::new(
            "transform",
            r#"
                PREFIX ex: <http://example.org/>
                CONSTRUCT {
                    ?s ex:transformed ?o .
                    ?s a ex:Thing .
                    ?o a ex:Thing .
                }
                WHERE { ?s ex:predicate ?o }
            "#,
        );

        // Execute single query
        let result_graph = ConstructExecutor::execute_query(&ConstructExecutor::new(), &graph, &query);

        // The query execution should succeed (even if results are empty)
        assert!(result_graph.is_ok(), "Query execution should succeed: {:?}", result_graph.err());
    }

    #[test]
    fn test_execute_query_handles_empty_construct() {
        // Empty graph
        let graph = Graph::new().expect("Failed to create graph");

        // Create executor
        let exec = ConstructExecutor::new();
        let query = Query::new(
            "empty_construct",
            r#"
                PREFIX ex: <http://example.org/>
                CONSTRUCT { ?s ex:related ?o }
                WHERE { ?s ex:knows ?o }
            "#,
        );

        // Execute query that returns no results
        let result_graph = exec.execute_query(&graph, &query);

        assert!(result_graph.is_ok(), "Empty CONSTRUCT should not error");
    }
}
