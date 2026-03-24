//! μ₂: Extraction Pass (CONSTRUCT-based IR Generation)
//!
//! Performs ontology → IR transformation using CONSTRUCT queries.
//! Builds a fully-shaped generation IR graph G′ that requires no structure recovery.
//!
//! ## Architecture
//!
//! This pass transforms the normalized ontology O′ (from μ₁) into a generation-ready
//! intermediate representation G′. Unlike traditional extraction that produces bindings,
//! this pass uses CONSTRUCT queries to pre-shape all code structures as RDF triples.
//!
//! Formula: G′ = CONSTRUCT(O′)
//!
//! ## CONSTRUCT Guarantees
//!
//! - **CONSTRUCT-only verification**: All queries must be CONSTRUCT, not SELECT/ASK/DESCRIBE
//! - **Stop-the-line**: Any non-CONSTRUCT query immediately halts the pipeline (Andon Protocol)
//! - **Receipt integration**: All extraction results are recorded in the receipt
//! - **No structure recovery**: G′ contains complete code structure, no post-processing needed
//!
//! ## Parallel Execution
//!
//! Tensor queries targeting disjoint predicates execute in parallel for performance:
//! - Automatic detection of predicate disjointness
//! - Rayon-based parallel execution
//! - Deterministic output despite parallelism
//!
//! ## Example
//!
//! ```rust,no_run
//! use ggen_core::v6::passes::ExtractionPass;
//! use ggen_core::v6::pass::{Pass, PassContext};
//! use ggen_core::Graph;
//! use std::path::PathBuf;
//!
//! # fn main() -> ggen_utils::error::Result<()> {
//! let graph = Graph::new()?;
//! graph.insert_turtle(r#"
//!     @prefix code: <http://ggen.dev/code#> .
//!     @prefix rdfs: <http://www.w3.org/2000/01/rdf-schema#> .
//!
//!     code:User a code:Struct ;
//!         rdfs:label "User" .
//! "#)?;
//!
//! let mut pass = ExtractionPass::new();
//! pass.add_tensor_query(TensorQuery {
//!     name: "extract-structs".to_string(),
//!     construct: r#"
//!         PREFIX code: <http://ggen.dev/code#>
//!         PREFIX gen: <http://ggen.dev/gen#>
//!
//!         CONSTRUCT {
//!             ?struct gen:codeType gen:Struct ;
//!                     gen:name ?name .
//!         }
//!         WHERE {
//!             ?struct a code:Struct ;
//!                     rdfs:label ?name .
//!         }
//!     "#.to_string(),
//!     target_predicates: vec!["http://ggen.dev/gen#codeType".to_string()],
//!     order: 1,
//!     description: Some("Extract struct definitions to IR".to_string()),
//! });
//!
//! let mut ctx = PassContext::new(&graph, PathBuf::new(), PathBuf::new());
//! let result = pass.execute(&mut ctx)?;
//!
//! assert!(result.success);
//! # Ok(())
//! # }
//! ```

use crate::graph::ConstructExecutor;
use crate::v6::pass::{Pass, PassContext, PassResult, PassType};
use ggen_utils::error::{Error, Result};
use rayon::prelude::*;
use serde::{Deserialize, Serialize};
use std::collections::{BTreeSet, HashMap};
use std::time::Instant;

/// A CONSTRUCT-based tensor query for IR generation
///
/// Tensor queries are CONSTRUCT queries that generate disjoint portions
/// of the IR graph, allowing parallel execution.
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct TensorQuery {
    /// Query name for auditing
    pub name: String,

    /// SPARQL CONSTRUCT query that generates IR triples
    pub construct: String,

    /// Target predicates this query produces (for disjointness analysis)
    pub target_predicates: Vec<String>,

    /// Execution order (lower = earlier, queries with same order may run in parallel)
    pub order: i32,

    /// Description for documentation
    pub description: Option<String>,
}

/// Receipt for extraction pass execution
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct ExtractionReceipt {
    /// Total IR triples generated
    pub total_triples: usize,

    /// Per-query execution records
    pub query_executions: Vec<QueryExecution>,

    /// Parallel execution statistics
    pub parallel_stats: ParallelStats,

    /// Execution timestamp (ISO 8601)
    pub timestamp: String,

    /// SHA-256 hash of the IR graph
    pub ir_hash: String,
}

/// Record of a single query execution
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct QueryExecution {
    /// Query name
    pub name: String,

    /// Triples produced
    pub triples_produced: usize,

    /// Execution duration in milliseconds
    pub duration_ms: u64,

    /// SHA-256 hash of the CONSTRUCT query
    pub query_hash: String,

    /// Whether executed in parallel
    pub parallel: bool,
}

/// Statistics for parallel execution
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct ParallelStats {
    /// Number of queries executed in parallel
    pub parallel_queries: usize,

    /// Number of queries executed sequentially
    pub sequential_queries: usize,

    /// Total parallelism achieved (0.0 = all sequential, 1.0 = perfect parallel)
    pub parallelism_ratio: f64,
}

/// μ₂: Extraction pass implementation (CONSTRUCT-based)
#[derive(Debug, Clone)]
pub struct ExtractionPass {
    /// Tensor queries to execute
    tensor_queries: Vec<TensorQuery>,

    /// Whether to enable parallel execution
    enable_parallel: bool,
}

impl ExtractionPass {
    /// Create a new extraction pass
    pub fn new() -> Self {
        Self {
            tensor_queries: Vec::new(),
            enable_parallel: true,
        }
    }

    /// Add a tensor query
    pub fn add_tensor_query(&mut self, query: TensorQuery) {
        self.tensor_queries.push(query);
        self.tensor_queries.sort_by_key(|q| q.order);
    }

    /// Create with a set of tensor queries
    pub fn with_tensor_queries(mut self, queries: Vec<TensorQuery>) -> Self {
        self.tensor_queries = queries;
        self.tensor_queries.sort_by_key(|q| q.order);
        self
    }

    /// Enable or disable parallel execution
    pub fn with_parallel(mut self, enabled: bool) -> Self {
        self.enable_parallel = enabled;
        self
    }

    /// Verify all queries are CONSTRUCT-only (SELECT/ASK/DESCRIBE gate)
    ///
    /// This is a critical Andon gate - any violation stops the line.
    fn verify_construct_only_gate(&self) -> Result<()> {
        for query in &self.tensor_queries {
            let query_upper = query.construct.to_uppercase();

            // Check for forbidden query types
            if query_upper.contains("SELECT") && !query_upper.contains("CONSTRUCT") {
                return Err(Error::new(&format!(
                    "🚨 SELECT Detected in μ₂:extraction\n\n\
                     μ₂:extraction STOPPED THE LINE (Andon Protocol)\n\n\
                     Query '{}' contains SELECT keyword without CONSTRUCT.\n\n\
                     μ₂ is CONSTRUCT-only for IR generation. SELECT queries are forbidden.\n\n\
                     Fix: Rewrite as CONSTRUCT query to generate IR triples, or move to μ₃.",
                    query.name
                )));
            }

            if query_upper.contains("ASK") && !query_upper.contains("CONSTRUCT") {
                return Err(Error::new(&format!(
                    "🚨 ASK Query Detected in μ₂:extraction\n\n\
                     μ₂:extraction STOPPED THE LINE (Andon Protocol)\n\n\
                     Query '{}' is an ASK query.\n\n\
                     μ₂ requires CONSTRUCT queries only. ASK queries are forbidden.\n\n\
                     Fix: Rewrite as CONSTRUCT query to generate IR triples.",
                    query.name
                )));
            }

            if query_upper.contains("DESCRIBE") && !query_upper.contains("CONSTRUCT") {
                return Err(Error::new(&format!(
                    "🚨 DESCRIBE Query Detected in μ₂:extraction\n\n\
                     μ₂:extraction STOPPED THE LINE (Andon Protocol)\n\n\
                     Query '{}' is a DESCRIBE query.\n\n\
                     μ₂ requires CONSTRUCT queries only. DESCRIBE queries are forbidden.\n\n\
                     Fix: Rewrite as explicit CONSTRUCT query to control IR shape.",
                    query.name
                )));
            }

            if query_upper.contains("INSERT") || query_upper.contains("DELETE") {
                return Err(Error::new(&format!(
                    "🚨 UPDATE Query Detected in μ₂:extraction\n\n\
                     μ₂:extraction STOPPED THE LINE (Andon Protocol)\n\n\
                     Query '{}' contains INSERT/DELETE (SPARQL Update).\n\n\
                     μ₂ is read-only. Graph updates belong in μ₁:normalization.\n\n\
                     Fix: Move UPDATE to μ₁ or rewrite as CONSTRUCT.",
                    query.name
                )));
            }

            // Verify CONSTRUCT keyword is present
            if !query_upper.contains("CONSTRUCT") {
                return Err(Error::new(&format!(
                    "🚨 Non-CONSTRUCT Query in μ₂:extraction\n\n\
                     μ₂:extraction STOPPED THE LINE (Andon Protocol)\n\n\
                     Query '{}' does not contain CONSTRUCT keyword.\n\n\
                     μ₂ requires CONSTRUCT queries to generate IR triples.\n\n\
                     Fix: Add CONSTRUCT clause to generate IR graph G′.",
                    query.name
                )));
            }
        }

        Ok(())
    }

    /// Group queries by execution order for parallel execution
    fn group_by_order(&self) -> Vec<Vec<&TensorQuery>> {
        let mut groups: HashMap<i32, Vec<&TensorQuery>> = HashMap::new();

        for query in &self.tensor_queries {
            groups.entry(query.order).or_default().push(query);
        }

        let mut sorted_groups: Vec<_> = groups.into_iter().collect();
        sorted_groups.sort_by_key(|(order, _)| *order);

        sorted_groups.into_iter().map(|(_, queries)| queries).collect()
    }

    /// Check if two queries have disjoint target predicates
    fn are_disjoint(query1: &TensorQuery, query2: &TensorQuery) -> bool {
        let set1: BTreeSet<_> = query1.target_predicates.iter().collect();
        let set2: BTreeSet<_> = query2.target_predicates.iter().collect();

        set1.is_disjoint(&set2)
    }

    /// Determine if a group of queries can execute in parallel
    fn can_parallelize(queries: &[&TensorQuery]) -> bool {
        if queries.len() <= 1 {
            return false;
        }

        // Check pairwise disjointness of all predicates
        for i in 0..queries.len() {
            for j in (i + 1)..queries.len() {
                if !Self::are_disjoint(queries[i], queries[j]) {
                    return false;
                }
            }
        }

        true
    }

    /// Execute a single tensor query and return the result
    fn execute_tensor_query(
        &self,
        ctx: &PassContext<'_>,
        query: &TensorQuery,
    ) -> Result<(String, usize, u64, String)> {
        let start = Instant::now();
        let executor = ConstructExecutor::new(ctx.graph);

        // Execute CONSTRUCT query
        let triples = executor
            .execute(&query.construct)
            .map_err(|e| Error::new(&format!("Tensor query '{}' failed: {}", query.name, e)))?;

        let duration_ms = start.elapsed().as_millis() as u64;
        let triples_count = triples.len();

        // Compute query hash for receipt
        let query_hash = {
            use sha2::{Digest, Sha256};
            let mut hasher = Sha256::new();
            hasher.update(query.construct.as_bytes());
            format!("{:x}", hasher.finalize())
        };

        // Materialize triples into the graph
        if !triples.is_empty() {
            let ntriples = triples.join("\n");
            ctx.graph.insert_turtle(&ntriples)?;
        }

        Ok((query.name.clone(), triples_count, duration_ms, query_hash))
    }

    /// Execute a group of queries (sequentially or in parallel)
    fn execute_group(
        &self,
        ctx: &PassContext<'_>,
        queries: &[&TensorQuery],
        can_parallel: bool,
    ) -> Result<Vec<QueryExecution>> {
        if can_parallel && self.enable_parallel && queries.len() > 1 {
            // Parallel execution for disjoint tensor queries
            let results: Result<Vec<_>> = queries
                .par_iter()
                .map(|query| {
                    let (name, triples, duration_ms, query_hash) =
                        self.execute_tensor_query(ctx, query)?;
                    Ok(QueryExecution {
                        name,
                        triples_produced: triples,
                        duration_ms,
                        query_hash,
                        parallel: true,
                    })
                })
                .collect();

            results
        } else {
            // Sequential execution
            let mut result_vec = Vec::new();
            for query in queries {
                let (name, triples, duration_ms, query_hash) = self.execute_tensor_query(ctx, query)?;
                result_vec.push(QueryExecution {
                    name,
                    triples_produced: triples,
                    duration_ms,
                    query_hash,
                    parallel: false,
                });
            }
            Ok(result_vec)
        }
    }

    /// Generate extraction receipt
    fn generate_receipt(&self, executions: Vec<QueryExecution>) -> Result<ExtractionReceipt> {
        let timestamp = chrono::Utc::now().to_rfc3339();

        let total_triples: usize = executions.iter().map(|e| e.triples_produced).sum();

        let parallel_queries = executions.iter().filter(|e| e.parallel).count();
        let sequential_queries = executions.len() - parallel_queries;

        let parallelism_ratio = if executions.is_empty() {
            0.0
        } else {
            parallel_queries as f64 / executions.len() as f64
        };

        // Compute IR hash (hash of all query hashes, deterministic)
        let ir_hash = {
            use sha2::{Digest, Sha256};
            let mut hasher = Sha256::new();
            for exec in &executions {
                hasher.update(exec.query_hash.as_bytes());
                hasher.update(exec.triples_produced.to_string().as_bytes());
            }
            format!("{:x}", hasher.finalize())
        };

        Ok(ExtractionReceipt {
            total_triples,
            query_executions: executions,
            parallel_stats: ParallelStats {
                parallel_queries,
                sequential_queries,
                parallelism_ratio,
            },
            timestamp,
            ir_hash,
        })
    }
}

impl Default for ExtractionPass {
    fn default() -> Self {
        Self::new()
    }
}

impl Pass for ExtractionPass {
    fn pass_type(&self) -> PassType {
        PassType::Extraction
    }

    fn name(&self) -> &str {
        "μ₂:extraction"
    }

    fn execute(&self, ctx: &mut PassContext<'_>) -> Result<PassResult> {
        let start = Instant::now();

        // GATE: Verify all queries are CONSTRUCT-only (no SELECT/ASK/DESCRIBE)
        // Stop the line immediately if any forbidden query type detected
        self.verify_construct_only_gate()?;

        // Group queries by execution order
        let groups = self.group_by_order();

        let mut all_executions = Vec::new();

        // Execute each group
        for group in groups {
            let can_parallel = Self::can_parallelize(&group);
            let executions = self.execute_group(ctx, &group, can_parallel)?;
            all_executions.extend(executions);
        }

        // Generate extraction receipt
        let receipt = self.generate_receipt(all_executions)?;

        // Store receipt in context for μ₅:receipt to include
        ctx.bindings.insert(
            "extraction_receipt".to_string(),
            serde_json::to_value(&receipt)
                .map_err(|e| Error::new(&format!("Failed to serialize receipt: {}", e)))?,
        );

        let duration = start.elapsed();
        Ok(PassResult::success()
            .with_triples(receipt.total_triples)
            .with_duration(duration))
    }
}

/// Standard extraction tensor queries for v6
impl ExtractionPass {
    /// Create a pass with standard v6 extraction rules
    pub fn with_standard_rules() -> Self {
        let mut pass = Self::new();

        // Tensor 1: Extract code structure types (structs, enums, traits)
        pass.add_tensor_query(TensorQuery {
            name: "extract-code-types".to_string(),
            construct: r#"
                PREFIX code: <http://ggen.dev/code#>
                PREFIX gen: <http://ggen.dev/gen#>
                PREFIX rdfs: <http://www.w3.org/2000/01/rdf-schema#>

                CONSTRUCT {
                    ?entity gen:codeType ?typeLabel ;
                            gen:name ?name ;
                            gen:visibility ?visibility .
                }
                WHERE {
                    VALUES (?type ?typeLabel) {
                        (code:Struct "struct")
                        (code:Enum "enum")
                        (code:Trait "trait")
                    }
                    ?entity a ?type ;
                            rdfs:label ?name .
                    OPTIONAL { ?entity code:visibility ?visibility . }
                }
            "#
            .to_string(),
            target_predicates: vec!["http://ggen.dev/gen#codeType".to_string()],
            order: 1,
            description: Some("Extract code type definitions to IR".to_string()),
        });

        // Tensor 2: Extract field definitions
        pass.add_tensor_query(TensorQuery {
            name: "extract-fields".to_string(),
            construct: r#"
                PREFIX code: <http://ggen.dev/code#>
                PREFIX gen: <http://ggen.dev/gen#>
                PREFIX rdfs: <http://www.w3.org/2000/01/rdf-schema#>

                CONSTRUCT {
                    ?field gen:fieldOf ?struct ;
                           gen:fieldName ?name ;
                           gen:fieldType ?type ;
                           gen:fieldVisibility ?visibility .
                }
                WHERE {
                    ?field a code:Field ;
                           code:belongsTo ?struct ;
                           rdfs:label ?name ;
                           code:hasType ?type .
                    OPTIONAL { ?field code:visibility ?visibility . }
                }
            "#
            .to_string(),
            target_predicates: vec!["http://ggen.dev/gen#fieldOf".to_string()],
            order: 1,
            description: Some("Extract field definitions to IR".to_string()),
        });

        // Tensor 3: Extract method definitions
        pass.add_tensor_query(TensorQuery {
            name: "extract-methods".to_string(),
            construct: r#"
                PREFIX code: <http://ggen.dev/code#>
                PREFIX gen: <http://ggen.dev/gen#>
                PREFIX rdfs: <http://www.w3.org/2000/01/rdf-schema#>

                CONSTRUCT {
                    ?method gen:methodOf ?parent ;
                            gen:methodName ?name ;
                            gen:methodReturnType ?returnType ;
                            gen:methodVisibility ?visibility .
                }
                WHERE {
                    ?method a code:Method ;
                            code:belongsTo ?parent ;
                            rdfs:label ?name .
                    OPTIONAL { ?method code:returnType ?returnType . }
                    OPTIONAL { ?method code:visibility ?visibility . }
                }
            "#
            .to_string(),
            target_predicates: vec!["http://ggen.dev/gen#methodOf".to_string()],
            order: 1,
            description: Some("Extract method definitions to IR".to_string()),
        });

        pass
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::graph::Graph;
    use std::path::PathBuf;

    #[test]
    fn test_extraction_pass_empty() {
        let graph = Graph::new().unwrap();
        let pass = ExtractionPass::new();

        let mut ctx = PassContext::new(&graph, PathBuf::new(), PathBuf::new());
        let result = pass.execute(&mut ctx).unwrap();

        assert!(result.success);
        assert_eq!(result.triples_added, 0);
    }

    #[test]
    fn test_construct_query_execution() {
        let graph = Graph::new().unwrap();
        graph
            .insert_turtle(
                r#"
            @prefix code: <http://ggen.dev/code#> .
            @prefix rdfs: <http://www.w3.org/2000/01/rdf-schema#> .

            code:User a code:Struct ;
                rdfs:label "User" .
        "#,
            )
            .unwrap();

        let mut pass = ExtractionPass::new();
        pass.add_tensor_query(TensorQuery {
            name: "extract-structs".to_string(),
            construct: r#"
                PREFIX code: <http://ggen.dev/code#>
                PREFIX gen: <http://ggen.dev/gen#>
                PREFIX rdfs: <http://www.w3.org/2000/01/rdf-schema#>

                CONSTRUCT {
                    ?struct gen:codeType "struct" ;
                            gen:name ?name .
                }
                WHERE {
                    ?struct a code:Struct ;
                            rdfs:label ?name .
                }
            "#
            .to_string(),
            target_predicates: vec!["http://ggen.dev/gen#codeType".to_string()],
            order: 1,
            description: None,
        });

        let mut ctx = PassContext::new(&graph, PathBuf::new(), PathBuf::new());
        let result = pass.execute(&mut ctx).unwrap();

        assert!(result.success);
        assert!(result.triples_added > 0);

        // Verify receipt was generated
        assert!(ctx.bindings.contains_key("extraction_receipt"));
    }

    #[test]
    fn test_select_query_rejected() {
        let graph = Graph::new().unwrap();
        let mut pass = ExtractionPass::new();

        // Add a SELECT query (should be rejected)
        pass.add_tensor_query(TensorQuery {
            name: "invalid-select".to_string(),
            construct: "SELECT ?s WHERE { ?s ?p ?o }".to_string(),
            target_predicates: vec![],
            order: 1,
            description: None,
        });

        let mut ctx = PassContext::new(&graph, PathBuf::new(), PathBuf::new());
        let result = pass.execute(&mut ctx);

        // Should fail with clear error message
        assert!(result.is_err());
        let error_msg = result.unwrap_err().to_string();
        assert!(error_msg.contains("SELECT Detected"));
        assert!(error_msg.contains("STOPPED THE LINE"));
    }

    #[test]
    fn test_ask_query_rejected() {
        let graph = Graph::new().unwrap();
        let mut pass = ExtractionPass::new();

        pass.add_tensor_query(TensorQuery {
            name: "invalid-ask".to_string(),
            construct: "ASK { ?s ?p ?o }".to_string(),
            target_predicates: vec![],
            order: 1,
            description: None,
        });

        let mut ctx = PassContext::new(&graph, PathBuf::new(), PathBuf::new());
        let result = pass.execute(&mut ctx);

        assert!(result.is_err());
        let error_msg = result.unwrap_err().to_string();
        assert!(error_msg.contains("ASK Query Detected"));
    }

    #[test]
    fn test_describe_query_rejected() {
        let graph = Graph::new().unwrap();
        let mut pass = ExtractionPass::new();

        pass.add_tensor_query(TensorQuery {
            name: "invalid-describe".to_string(),
            construct: "DESCRIBE <http://example.org/thing>".to_string(),
            target_predicates: vec![],
            order: 1,
            description: None,
        });

        let mut ctx = PassContext::new(&graph, PathBuf::new(), PathBuf::new());
        let result = pass.execute(&mut ctx);

        assert!(result.is_err());
        let error_msg = result.unwrap_err().to_string();
        assert!(error_msg.contains("DESCRIBE Query Detected"));
    }

    #[test]
    fn test_update_query_rejected() {
        let graph = Graph::new().unwrap();
        let mut pass = ExtractionPass::new();

        pass.add_tensor_query(TensorQuery {
            name: "invalid-insert".to_string(),
            construct: "INSERT DATA { <http://example.org/s> <http://example.org/p> <http://example.org/o> }".to_string(),
            target_predicates: vec![],
            order: 1,
            description: None,
        });

        let mut ctx = PassContext::new(&graph, PathBuf::new(), PathBuf::new());
        let result = pass.execute(&mut ctx);

        assert!(result.is_err());
        let error_msg = result.unwrap_err().to_string();
        assert!(error_msg.contains("UPDATE Query Detected"));
    }

    #[test]
    fn test_parallel_execution_disjoint_predicates() {
        let graph = Graph::new().unwrap();
        graph
            .insert_turtle(
                r#"
            @prefix code: <http://ggen.dev/code#> .
            @prefix rdfs: <http://www.w3.org/2000/01/rdf-schema#> .

            code:User a code:Struct ;
                rdfs:label "User" .

            code:Order a code:Struct ;
                rdfs:label "Order" .
        "#,
            )
            .unwrap();

        let mut pass = ExtractionPass::new().with_parallel(true);

        // Two queries with disjoint predicates (same order = parallel)
        pass.add_tensor_query(TensorQuery {
            name: "extract-structs".to_string(),
            construct: r#"
                PREFIX code: <http://ggen.dev/code#>
                PREFIX gen: <http://ggen.dev/gen#>
                PREFIX rdfs: <http://www.w3.org/2000/01/rdf-schema#>

                CONSTRUCT {
                    ?struct gen:codeType "struct" .
                }
                WHERE {
                    ?struct a code:Struct .
                }
            "#
            .to_string(),
            target_predicates: vec!["http://ggen.dev/gen#codeType".to_string()],
            order: 1,
            description: None,
        });

        pass.add_tensor_query(TensorQuery {
            name: "extract-names".to_string(),
            construct: r#"
                PREFIX code: <http://ggen.dev/code#>
                PREFIX gen: <http://ggen.dev/gen#>
                PREFIX rdfs: <http://www.w3.org/2000/01/rdf-schema#>

                CONSTRUCT {
                    ?struct gen:name ?name .
                }
                WHERE {
                    ?struct a code:Struct ;
                            rdfs:label ?name .
                }
            "#
            .to_string(),
            target_predicates: vec!["http://ggen.dev/gen#name".to_string()],
            order: 1,
            description: None,
        });

        let mut ctx = PassContext::new(&graph, PathBuf::new(), PathBuf::new());
        let result = pass.execute(&mut ctx).unwrap();

        assert!(result.success);
        assert!(result.triples_added > 0);

        // Verify parallel execution was used
        let receipt_value = ctx.bindings.get("extraction_receipt").unwrap();
        let receipt: ExtractionReceipt = serde_json::from_value(receipt_value.clone()).unwrap();
        assert!(receipt.parallel_stats.parallel_queries > 0);
    }

    #[test]
    fn test_disjoint_predicate_detection() {
        let query1 = TensorQuery {
            name: "q1".to_string(),
            construct: String::new(),
            target_predicates: vec!["pred:a".to_string(), "pred:b".to_string()],
            order: 1,
            description: None,
        };

        let query2 = TensorQuery {
            name: "q2".to_string(),
            construct: String::new(),
            target_predicates: vec!["pred:c".to_string(), "pred:d".to_string()],
            order: 1,
            description: None,
        };

        let query3 = TensorQuery {
            name: "q3".to_string(),
            construct: String::new(),
            target_predicates: vec!["pred:b".to_string(), "pred:e".to_string()],
            order: 1,
            description: None,
        };

        // q1 and q2 are disjoint
        assert!(ExtractionPass::are_disjoint(&query1, &query2));

        // q1 and q3 are NOT disjoint (share pred:b)
        assert!(!ExtractionPass::are_disjoint(&query1, &query3));
    }

    #[test]
    fn test_receipt_generation() {
        let executions = vec![
            QueryExecution {
                name: "q1".to_string(),
                triples_produced: 10,
                duration_ms: 50,
                query_hash: "hash1".to_string(),
                parallel: true,
            },
            QueryExecution {
                name: "q2".to_string(),
                triples_produced: 20,
                duration_ms: 75,
                query_hash: "hash2".to_string(),
                parallel: true,
            },
            QueryExecution {
                name: "q3".to_string(),
                triples_produced: 15,
                duration_ms: 60,
                query_hash: "hash3".to_string(),
                parallel: false,
            },
        ];

        let pass = ExtractionPass::new();
        let receipt = pass.generate_receipt(executions).unwrap();

        assert_eq!(receipt.total_triples, 45);
        assert_eq!(receipt.query_executions.len(), 3);
        assert_eq!(receipt.parallel_stats.parallel_queries, 2);
        assert_eq!(receipt.parallel_stats.sequential_queries, 1);
        assert!((receipt.parallel_stats.parallelism_ratio - 0.666).abs() < 0.01);
        assert!(!receipt.ir_hash.is_empty());
    }

    #[test]
    fn test_standard_rules() {
        let pass = ExtractionPass::with_standard_rules();
        assert!(pass.tensor_queries.len() >= 3);

        // Verify all are CONSTRUCT queries
        for query in &pass.tensor_queries {
            assert!(query.construct.to_uppercase().contains("CONSTRUCT"));
        }
    }

    #[test]
    fn test_non_construct_query_rejected() {
        let graph = Graph::new().unwrap();
        let mut pass = ExtractionPass::new();

        // Query without CONSTRUCT keyword
        pass.add_tensor_query(TensorQuery {
            name: "invalid-no-construct".to_string(),
            construct: "INVALID SPARQL".to_string(),
            target_predicates: vec![],
            order: 1,
            description: None,
        });

        let mut ctx = PassContext::new(&graph, PathBuf::new(), PathBuf::new());
        let result = pass.execute(&mut ctx);

        assert!(result.is_err());
        let error_msg = result.unwrap_err().to_string();
        assert!(error_msg.contains("Non-CONSTRUCT Query"));
    }
}
