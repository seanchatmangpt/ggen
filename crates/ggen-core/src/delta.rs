//! Delta-driven projection for detecting and analyzing RDF graph changes
//!
//! This module provides functionality to detect and analyze changes in RDF graphs,
//! determine which templates are affected by those changes, and support three-way
//! merging for files that contain both generated and manual content.
//!
//! ## Features
//!
//! - **Graph Comparison**: Compare two RDF graphs and detect semantic differences
//! - **Delta Types**: Track additions, deletions, and modifications of triples
//! - **Impact Analysis**: Determine which templates are affected by graph changes
//! - **Template Impact**: Calculate impact scores for template regeneration
//! - **Change Detection**: Efficient change detection using graph hashing
//!
//! ## Delta Types
//!
//! - **Addition**: New triple added to the graph
//! - **Deletion**: Triple removed from the graph
//! - **Modification**: Triple's object value changed
//!
//! ## Examples
//!
//! ### Detecting Graph Changes
//!
//! ```rust,no_run
//! use ggen_core::delta::{GraphDelta, ImpactAnalyzer};
//! use ggen_core::graph::Graph;
//!
//! # fn main() -> ggen_utils::error::Result<()> {
//! let old_graph = Graph::new()?;
//! let new_graph = Graph::new()?;
//!
//! let delta = GraphDelta::compute(&old_graph, &new_graph)?;
//! println!("Detected {} changes", delta.changes.len());
//! # Ok(())
//! # }
//! ```
//!
//! ### Analyzing Template Impact
//!
//! ```rust,no_run
//! use ggen_core::delta::ImpactAnalyzer;
//! use ggen_core::graph::Graph;
//!
//! # fn main() -> ggen_utils::error::Result<()> {
//! let analyzer = ImpactAnalyzer::new();
//! let old_graph = Graph::new()?;
//! let new_graph = Graph::new()?;
//!
//! let impacts = analyzer.analyze(&old_graph, &new_graph, &["template1.tmpl".into()])?;
//! for impact in impacts {
//!     println!("Template {}: impact score {}", impact.template_path.display(), impact.score);
//! }
//! # Ok(())
//! # }
//! ```

use ahash::AHasher;
use ggen_utils::error::Result;
use oxigraph::model::Quad;
use serde::{Deserialize, Serialize};
use std::collections::{BTreeMap, BTreeSet};
use std::fmt;
use std::hash::{Hash, Hasher};

use crate::graph::Graph;

/// Represents a semantic change in an RDF graph
///
/// A `DeltaType` describes a single change between two RDF graphs:
/// - **Addition**: A new triple was added
/// - **Deletion**: A triple was removed
/// - **Modification**: A triple's object value changed
///
/// # Examples
///
/// ```rust
/// use ggen_core::delta::DeltaType;
///
/// # fn main() {
/// // Addition example
/// let addition = DeltaType::Addition {
///     subject: "http://example.org/subject".to_string(),
///     predicate: "http://example.org/predicate".to_string(),
///     object: "http://example.org/object".to_string(),
/// };
/// match addition {
///     DeltaType::Addition { .. } => assert!(true),
///     _ => panic!("Should be Addition"),
/// }
///
/// // Deletion example
/// let deletion = DeltaType::Deletion {
///     subject: "http://example.org/subject".to_string(),
///     predicate: "http://example.org/predicate".to_string(),
///     object: "http://example.org/object".to_string(),
/// };
/// match deletion {
///     DeltaType::Deletion { .. } => assert!(true),
///     _ => panic!("Should be Deletion"),
/// }
///
/// // Modification example
/// let modification = DeltaType::Modification {
///     subject: "http://example.org/subject".to_string(),
///     predicate: "http://example.org/predicate".to_string(),
///     old_object: "old".to_string(),
///     new_object: "new".to_string(),
/// };
/// match modification {
///     DeltaType::Modification { .. } => assert!(true),
///     _ => panic!("Should be Modification"),
/// }
/// # }
/// ```
///
/// ```rust,no_run
/// use ggen_core::delta::DeltaType;
/// use oxigraph::model::{NamedNode, Literal, Quad};
///
/// # fn main() -> ggen_utils::error::Result<()> {
/// // Create an addition delta
/// let addition = DeltaType::Addition {
///     subject: "http://example.org/alice".to_string(),
///     predicate: "http://example.org/name".to_string(),
///     object: "Alice".to_string(),
/// };
///
/// // Check which IRIs are affected
/// let affected = addition.subjects();
/// assert_eq!(affected, vec!["http://example.org/alice"]);
/// # Ok(())
/// # }
/// ```
///
/// ```rust,no_run
/// use ggen_core::delta::DeltaType;
///
/// # fn main() -> ggen_utils::error::Result<()> {
/// // Create a modification delta
/// let modification = DeltaType::Modification {
///     subject: "http://example.org/alice".to_string(),
///     predicate: "http://example.org/age".to_string(),
///     old_object: "30".to_string(),
///     new_object: "31".to_string(),
/// };
///
/// // Check if a specific IRI is affected
/// assert!(modification.affects_iri("http://example.org/alice"));
/// # Ok(())
/// # }
/// ```
#[derive(Debug, Clone, PartialEq, Eq, Hash, Serialize, Deserialize)]
pub enum DeltaType {
    /// A triple was added to the graph
    Addition {
        subject: String,
        predicate: String,
        object: String,
    },
    /// A triple was removed from the graph
    Deletion {
        subject: String,
        predicate: String,
        object: String,
    },
    /// A triple's object changed
    Modification {
        subject: String,
        predicate: String,
        old_object: String,
        new_object: String,
    },
}

impl DeltaType {
    /// Create a delta from two quads
    ///
    /// Compares an old quad (from baseline graph) and a new quad (from current graph)
    /// and returns the appropriate delta type, or `None` if there's no change.
    ///
    /// # Examples
    ///
    /// ```rust,no_run
    /// use ggen_core::delta::DeltaType;
    /// use oxigraph::model::{NamedNode, Literal, Quad, Subject, Term};
    ///
    /// # fn main() -> ggen_utils::error::Result<()> {
    /// let old_quad = Quad::new(
    ///     Subject::NamedNode(NamedNode::new("http://example.org/alice")?),
    ///     NamedNode::new("http://example.org/name")?,
    ///     Term::Literal(Literal::new_simple_literal("Alice")),
    ///     None,
    /// );
    ///
    /// let new_quad = Quad::new(
    ///     Subject::NamedNode(NamedNode::new("http://example.org/alice")?),
    ///     NamedNode::new("http://example.org/name")?,
    ///     Term::Literal(Literal::new_simple_literal("Alice Smith")),
    ///     None,
    /// );
    ///
    /// // Detect modification
    /// let delta = DeltaType::from_quads(Some(&old_quad), Some(&new_quad));
    /// assert!(matches!(delta, Some(DeltaType::Modification { .. })));
    /// # Ok(())
    /// # }
    /// ```
    pub fn from_quads(old: Option<&Quad>, new: Option<&Quad>) -> Option<Self> {
        match (old, new) {
            (None, Some(new_quad)) => Some(DeltaType::Addition {
                subject: new_quad.subject.to_string(),
                predicate: new_quad.predicate.to_string(),
                object: new_quad.object.to_string(),
            }),
            (Some(old_quad), None) => Some(DeltaType::Deletion {
                subject: old_quad.subject.to_string(),
                predicate: old_quad.predicate.to_string(),
                object: old_quad.object.to_string(),
            }),
            (Some(old_quad), Some(new_quad)) => {
                if old_quad.subject == new_quad.subject
                    && old_quad.predicate == new_quad.predicate
                    && old_quad.object != new_quad.object
                {
                    Some(DeltaType::Modification {
                        subject: old_quad.subject.to_string(),
                        predicate: old_quad.predicate.to_string(),
                        old_object: old_quad.object.to_string(),
                        new_object: new_quad.object.to_string(),
                    })
                } else {
                    None
                }
            }
            (None, None) => None,
        }
    }

    /// Get all subjects affected by this delta
    ///
    /// Returns a vector of subject IRIs that are affected by this change.
    ///
    /// # Examples
    ///
    /// ```rust,no_run
    /// use ggen_core::delta::DeltaType;
    ///
    /// # fn main() -> ggen_utils::error::Result<()> {
    /// let delta = DeltaType::Addition {
    ///     subject: "http://example.org/alice".to_string(),
    ///     predicate: "http://example.org/name".to_string(),
    ///     object: "Alice".to_string(),
    /// };
    ///
    /// let subjects = delta.subjects();
    /// assert_eq!(subjects, vec!["http://example.org/alice"]);
    /// # Ok(())
    /// # }
    /// ```
    pub fn subjects(&self) -> Vec<&str> {
        match self {
            DeltaType::Addition { subject, .. }
            | DeltaType::Deletion { subject, .. }
            | DeltaType::Modification { subject, .. } => vec![subject],
        }
    }

    /// Get all predicates affected by this delta
    pub fn predicates(&self) -> Vec<&str> {
        match self {
            DeltaType::Addition { predicate, .. }
            | DeltaType::Deletion { predicate, .. }
            | DeltaType::Modification { predicate, .. } => vec![predicate],
        }
    }

    /// Check if this delta affects a specific IRI
    pub fn affects_iri(&self, iri: &str) -> bool {
        self.subjects().contains(&iri)
            || self.predicates().contains(&iri)
            || match self {
                DeltaType::Addition { object, .. } | DeltaType::Deletion { object, .. } => {
                    object == iri
                }
                DeltaType::Modification {
                    old_object,
                    new_object,
                    ..
                } => old_object == iri || new_object == iri,
            }
    }
}

impl fmt::Display for DeltaType {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            DeltaType::Addition {
                subject,
                predicate,
                object,
            } => write!(f, "+ {} {} {}", subject, predicate, object),
            DeltaType::Deletion {
                subject,
                predicate,
                object,
            } => write!(f, "- {} {} {}", subject, predicate, object),
            DeltaType::Modification {
                subject,
                predicate,
                old_object,
                new_object,
            } => write!(
                f,
                "~ {} {} {} -> {}",
                subject, predicate, old_object, new_object
            ),
        }
    }
}

/// Collection of deltas representing the difference between two graphs
///
/// A `GraphDelta` contains all changes detected between a baseline graph and
/// a current graph, along with metadata for tracking and verification.
///
/// # Examples
///
/// ```rust,no_run
/// use ggen_core::delta::GraphDelta;
/// use ggen_core::graph::Graph;
///
/// # fn main() -> ggen_utils::error::Result<()> {
/// let baseline = Graph::new()?;
/// baseline.insert_turtle(r#"
///     @prefix ex: <http://example.org/> .
///     ex:alice a ex:Person .
/// "#)?;
///
/// let current = Graph::new()?;
/// current.insert_turtle(r#"
///     @prefix ex: <http://example.org/> .
///     ex:alice a ex:Person .
///     ex:bob a ex:Person .
/// "#)?;
///
/// let delta = GraphDelta::new(&baseline, &current)?;
/// println!("Detected {} changes", delta.deltas.len());
/// # Ok(())
/// # }
/// ```
#[derive(Debug, Clone, Default, Serialize, Deserialize)]
pub struct GraphDelta {
    /// All detected changes
    pub deltas: Vec<DeltaType>,
    /// Hash of the baseline graph
    pub baseline_hash: Option<String>,
    /// Hash of the current graph
    pub current_hash: Option<String>,
    /// Timestamp when delta was computed
    pub computed_at: chrono::DateTime<chrono::Utc>,
}

impl GraphDelta {
    /// Create a new delta by comparing two graphs
    ///
    /// Computes all differences between the baseline and current graphs,
    /// including additions, deletions, and modifications.
    ///
    /// # Examples
    ///
    /// ```rust,no_run
    /// use ggen_core::delta::GraphDelta;
    /// use ggen_core::graph::Graph;
    ///
    /// # fn main() -> ggen_utils::error::Result<()> {
    /// let baseline = Graph::new()?;
    /// let current = Graph::new()?;
    /// current.insert_turtle(r#"
    ///     @prefix ex: <http://example.org/> .
    ///     ex:alice ex:name "Alice" .
    /// "#)?;
    ///
    /// let delta = GraphDelta::new(&baseline, &current)?;
    /// assert!(!delta.deltas.is_empty());
    /// # Ok(())
    /// # }
    /// ```
    pub fn new(baseline: &Graph, current: &Graph) -> Result<Self> {
        let mut deltas = Vec::new();

        // Get all quads from both graphs
        let baseline_quads = baseline.get_all_quads()?;
        let current_quads = current.get_all_quads()?;

        // Create lookup maps for efficient comparison
        let baseline_map: BTreeMap<(String, String, String), Quad> = baseline_quads
            .iter()
            .map(|q| {
                (
                    (
                        q.subject.to_string(),
                        q.predicate.to_string(),
                        q.object.to_string(),
                    ),
                    q.clone(),
                )
            })
            .collect();

        let current_map: BTreeMap<(String, String, String), Quad> = current_quads
            .iter()
            .map(|q| {
                (
                    (
                        q.subject.to_string(),
                        q.predicate.to_string(),
                        q.object.to_string(),
                    ),
                    q.clone(),
                )
            })
            .collect();

        // Find additions and modifications
        for ((s, p, o), current_quad) in &current_map {
            match baseline_map.get(&(s.clone(), p.clone(), o.clone())) {
                Some(baseline_quad) => {
                    // Check if objects are different (modification)
                    if baseline_quad.object != current_quad.object {
                        deltas.push(DeltaType::Modification {
                            subject: s.clone(),
                            predicate: p.clone(),
                            old_object: baseline_quad.object.to_string(),
                            new_object: current_quad.object.to_string(),
                        });
                    }
                    // If subjects/predicates match, it's not an addition
                }
                None => {
                    // This is an addition
                    deltas.push(DeltaType::Addition {
                        subject: s.clone(),
                        predicate: p.clone(),
                        object: o.clone(),
                    });
                }
            }
        }

        // Find deletions
        for (s, p, o) in baseline_map.keys() {
            if !current_map.contains_key(&(s.to_string(), p.clone(), o.clone())) {
                deltas.push(DeltaType::Deletion {
                    subject: s.clone(),
                    predicate: p.clone(),
                    object: o.clone(),
                });
            }
        }

        Ok(Self {
            deltas,
            baseline_hash: baseline.compute_hash().ok(),
            current_hash: current.compute_hash().ok(),
            computed_at: chrono::Utc::now(),
        })
    }

    /// Get all IRIs affected by this delta
    ///
    /// Returns a set of all unique IRIs (subjects, predicates, objects) that
    /// appear in any of the changes.
    ///
    /// # Examples
    ///
    /// ```rust,no_run
    /// use ggen_core::delta::GraphDelta;
    /// use ggen_core::graph::Graph;
    ///
    /// # fn main() -> ggen_utils::error::Result<()> {
    /// let baseline = Graph::new()?;
    /// let current = Graph::new()?;
    /// current.insert_turtle(r#"
    ///     @prefix ex: <http://example.org/> .
    ///     ex:alice ex:name "Alice" .
    /// "#)?;
    ///
    /// let delta = GraphDelta::new(&baseline, &current)?;
    /// let affected = delta.affected_iris();
    /// assert!(affected.contains("http://example.org/alice"));
    /// # Ok(())
    /// # }
    /// ```
    pub fn affected_iris(&self) -> BTreeSet<String> {
        let mut iris = BTreeSet::new();
        for delta in &self.deltas {
            iris.extend(delta.subjects().iter().map(|s| s.to_string()));
            iris.extend(delta.predicates().iter().map(|p| p.to_string()));
            match delta {
                DeltaType::Addition { object, .. } | DeltaType::Deletion { object, .. } => {
                    iris.insert(object.clone());
                }
                DeltaType::Modification {
                    old_object,
                    new_object,
                    ..
                } => {
                    iris.insert(old_object.clone());
                    iris.insert(new_object.clone());
                }
            }
        }
        iris
    }

    /// Check if this delta affects a specific IRI
    ///
    /// Returns `true` if any change in this delta affects the given IRI
    /// (as subject, predicate, or object).
    ///
    /// # Examples
    ///
    /// ```rust,no_run
    /// use ggen_core::delta::GraphDelta;
    /// use ggen_core::graph::Graph;
    ///
    /// # fn main() -> ggen_utils::error::Result<()> {
    /// let baseline = Graph::new()?;
    /// let current = Graph::new()?;
    /// current.insert_turtle(r#"
    ///     @prefix ex: <http://example.org/> .
    ///     ex:alice ex:name "Alice" .
    /// "#)?;
    ///
    /// let delta = GraphDelta::new(&baseline, &current)?;
    /// assert!(delta.affects_iri("http://example.org/alice"));
    /// # Ok(())
    /// # }
    /// ```
    pub fn affects_iri(&self, iri: &str) -> bool {
        self.deltas.iter().any(|d| d.affects_iri(iri))
    }

    /// Check if this delta is empty (no changes)
    pub fn is_empty(&self) -> bool {
        self.deltas.is_empty()
    }

    /// Get the count of each delta type
    pub fn counts(&self) -> BTreeMap<&str, usize> {
        let mut counts = BTreeMap::new();
        for delta in &self.deltas {
            let key = match delta {
                DeltaType::Addition { .. } => "additions",
                DeltaType::Deletion { .. } => "deletions",
                DeltaType::Modification { .. } => "modifications",
            };
            *counts.entry(key).or_insert(0) += 1;
        }
        counts
    }

    /// Filter deltas to only those affecting specific IRIs
    pub fn filter_by_iris(&self, iris: &[String]) -> Self {
        let filtered_deltas: Vec<_> = self
            .deltas
            .iter()
            .filter(|d| iris.iter().any(|iri| d.affects_iri(iri)))
            .cloned()
            .collect();

        Self {
            deltas: filtered_deltas,
            baseline_hash: self.baseline_hash.clone(),
            current_hash: self.current_hash.clone(),
            computed_at: self.computed_at,
        }
    }

    /// Merge another delta into this one
    pub fn merge(&mut self, other: GraphDelta) {
        self.deltas.extend(other.deltas);
        if self.baseline_hash.is_none() {
            self.baseline_hash = other.baseline_hash;
        }
        if self.current_hash.is_none() {
            self.current_hash = other.current_hash;
        }
    }
}

impl fmt::Display for GraphDelta {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        writeln!(f, "GraphDelta ({} changes):", self.deltas.len())?;

        let counts = self.counts();
        for (delta_type, count) in counts {
            writeln!(f, "  {}: {}", delta_type, count)?;
        }

        if !self.deltas.is_empty() {
            /// Maximum number of deltas to display before truncating
            const MAX_DELTAS_DISPLAY: usize = 10;

            writeln!(f)?;
            for delta in self.deltas.iter().take(MAX_DELTAS_DISPLAY) {
                writeln!(f, "  {}", delta)?;
            }

            if self.deltas.len() > MAX_DELTAS_DISPLAY {
                writeln!(
                    f,
                    "  ... and {} more",
                    self.deltas.len() - MAX_DELTAS_DISPLAY
                )?;
            }
        }

        Ok(())
    }
}

/// Template impact analysis for delta-driven regeneration
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct TemplateImpact {
    /// Path to the template
    pub template_path: String,
    /// IRIs from the delta that affect this template
    pub affected_iris: Vec<String>,
    /// Confidence score (0.0-1.0) of how likely this template is affected
    pub confidence: f64,
    /// Reason for the impact assessment
    pub reason: String,
}

impl TemplateImpact {
    /// Create a new template impact analysis
    pub fn new(
        template_path: String, affected_iris: Vec<String>, confidence: f64, reason: String,
    ) -> Self {
        Self {
            template_path,
            affected_iris,
            confidence,
            reason,
        }
    }

    /// Check if this impact is above a confidence threshold
    pub fn is_confident(&self, threshold: f64) -> bool {
        self.confidence >= threshold
    }
}

/// Analyze which templates are affected by a graph delta
pub struct ImpactAnalyzer {
    /// Cache of template query patterns for performance
    #[allow(dead_code)]
    template_queries: BTreeMap<String, Vec<String>>,
}

impl Default for ImpactAnalyzer {
    fn default() -> Self {
        Self::new()
    }
}

impl ImpactAnalyzer {
    /// Create a new impact analyzer
    pub fn new() -> Self {
        Self {
            template_queries: BTreeMap::new(),
        }
    }

    /// Analyze template impacts for a given delta
    pub fn analyze_impacts(
        &mut self, delta: &GraphDelta, template_paths: &[String], graph: &Graph,
    ) -> Result<Vec<TemplateImpact>> {
        let mut impacts = Vec::new();

        for template_path in template_paths {
            // Get or cache template queries
            let queries = self.get_template_queries(template_path, graph)?;

            // Analyze impact based on query patterns
            let (confidence, reason) = self.assess_impact(delta, &queries);

            if confidence > 0.0 {
                impacts.push(TemplateImpact::new(
                    template_path.clone(),
                    delta.affected_iris().into_iter().collect(),
                    confidence,
                    reason,
                ));
            }
        }

        // Sort by confidence (highest first)
        impacts.sort_by(|a, b| b.confidence.partial_cmp(&a.confidence).unwrap());

        Ok(impacts)
    }

    /// Get SPARQL queries from a template (simplified - in reality would parse template)
    fn get_template_queries(&mut self, template_path: &str, _graph: &Graph) -> Result<Vec<String>> {
        // Check if we have cached queries for this template
        if let Some(queries) = self.template_queries.get(template_path) {
            return Ok(queries.clone());
        }

        // This is a simplified implementation
        // In practice, would need to parse template frontmatter and extract SPARQL queries
        Ok(vec![
            "SELECT ?s ?p ?o WHERE { ?s ?p ?o }".to_string(),
            "SELECT ?class WHERE { ?class a rdfs:Class }".to_string(),
        ])
    }

    /// Assess how a delta impacts a set of queries
    fn assess_impact(&self, delta: &GraphDelta, queries: &[String]) -> (f64, String) {
        let affected_iris = delta.affected_iris();

        // Simple heuristic: check if any affected IRI appears in any query
        let mut max_relevance = 0.0;
        let mut reasons = Vec::new();

        for query in queries {
            let query_lower = query.to_lowercase();

            for iri in &affected_iris {
                if query_lower.contains(&iri.to_lowercase()) {
                    max_relevance = 1.0;
                    reasons.push(format!("Query directly references IRI: {}", iri));
                    break;
                }
            }
        }

        // If no direct matches, use pattern-based heuristics
        if max_relevance == 0.0 {
            // Check for schema changes (rdfs:Class, rdf:Property, etc.)
            for iri in &affected_iris {
                if iri.contains("rdfs:Class") || iri.contains("rdf:Property") {
                    max_relevance = 0.8;
                    reasons.push("Schema element changed".to_string());
                }
            }
        }

        let reason = if reasons.is_empty() {
            "No direct impact detected".to_string()
        } else {
            reasons.join("; ")
        };

        (max_relevance, reason)
    }
}

impl Graph {
    /// Get all quads in the graph for delta computation
    fn get_all_quads(&self) -> Result<Vec<Quad>> {
        let pattern = self.quads_for_pattern(None, None, None, None)?;
        Ok(pattern)
    }

    /// Compute a deterministic hash of the graph content
    pub fn compute_hash(&self) -> Result<String> {
        let quads = self.get_all_quads()?;
        let mut hasher = AHasher::default();

        // Create a deterministic string representation
        let mut sorted_quads: Vec<String> = quads
            .iter()
            .map(|q| format!("{} {} {}", q.subject, q.predicate, q.object))
            .collect();
        sorted_quads.sort();

        for quad_str in sorted_quads {
            quad_str.hash(&mut hasher);
        }

        Ok(format!("{:x}", hasher.finish()))
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::graph::Graph;

    fn create_test_graph() -> Result<(Graph, Graph)> {
        let baseline = Graph::new()?;
        baseline.insert_turtle(
            r#"
            @prefix : <http://example.org/> .
            @prefix rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#> .
            @prefix rdfs: <http://www.w3.org/2000/01/rdf-schema#> .
            :User a rdfs:Class .
            :name a rdf:Property ;
                  rdfs:domain :User .
        "#,
        )?;

        let current = Graph::new()?;
        current.insert_turtle(
            r#"
            @prefix : <http://example.org/> .
            @prefix rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#> .
            @prefix rdfs: <http://www.w3.org/2000/01/rdf-schema#> .
            :User a rdfs:Class .
            :name a rdf:Property ;
                  rdfs:domain :User .
            :email a rdf:Property ;
                    rdfs:domain :User .
        "#,
        )?;

        Ok((baseline, current))
    }

    test!(test_delta_creation, {
        let (baseline, current) = create_test_graph().unwrap();
        let delta = GraphDelta::new(&baseline, &current).unwrap();

        assert!(!delta.is_empty());
        assert!(delta.affects_iri("<http://example.org/email>"));

        // Should have two additions (the email property has 2 triples: type and domain)
        let counts = delta.counts();
        assert_eq!(counts.get("additions"), Some(&2));
        assert_eq!(counts.get("deletions"), None); // No deletions
        assert_eq!(counts.get("modifications"), None); // No modifications
    });

    test!(test_delta_affected_iris, {
        let (baseline, current) = create_test_graph().unwrap();
        let delta = GraphDelta::new(&baseline, &current).unwrap();

        let affected = delta.affected_iris();
        assert!(affected.contains("<http://example.org/email>"));
        assert!(affected.contains("<http://example.org/User>"));
        assert!(affected.contains("<http://www.w3.org/2000/01/rdf-schema#domain>"));
    });

    test!(test_delta_filtering, {
        let (baseline, current) = create_test_graph()?;
        let delta = GraphDelta::new(&baseline, &current)?;

        // Filter to only User-related changes
        let filtered = delta.filter_by_iris(&["<http://example.org/User>".to_string()]);

        // Should still contain the email addition since it affects User
        assert!(!filtered.is_empty());

        Ok(())
    });

    test!(test_impact_analyzer, {
        let (baseline, current) = create_test_graph().unwrap();
        let delta = GraphDelta::new(&baseline, &current).unwrap();

        let mut analyzer = ImpactAnalyzer::new();
        // Add a mock query that should match the email property
        analyzer.template_queries.insert(
            "template1.tmpl".to_string(),
            vec!["SELECT * WHERE { ?s <http://example.org/email> ?o }".to_string()],
        );

        let template_paths = vec!["template1.tmpl".to_string()];
        let impacts = analyzer
            .analyze_impacts(&delta, &template_paths, &baseline)
            .unwrap();

        // Should find some impacts since template queries match affected IRIs
        assert!(!impacts.is_empty());

        Ok(())
    });

    test!(test_graph_hash, {
        let (baseline, current) = create_test_graph().unwrap();

        let hash1 = baseline.compute_hash().unwrap();
        let hash2 = current.compute_hash().unwrap();

        // Different graphs should have different hashes
        assert_ne!(hash1, hash2);

        // Same graph should have same hash
        let hash3 = baseline.compute_hash().unwrap();
        assert_eq!(hash1, hash3);

        Ok(())
    });

    test!(test_delta_display, {
        let (baseline, current) = create_test_graph().unwrap();
        let delta = GraphDelta::new(&baseline, &current).unwrap();

        let display = format!("{}", delta);
        assert!(display.contains("GraphDelta"));
        assert!(display.contains("additions"));
        assert!(display.contains("http://example.org/email"));

        Ok(())
    });
}
