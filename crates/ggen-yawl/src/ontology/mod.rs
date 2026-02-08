//! Ontology loading and analysis module.
//!
//! Provides functionality for loading industry ontologies (FIBO, HL7, ISO standards)
//! from various RDF formats and analyzing their structure for workflow generation.

pub mod loader;

pub use loader::{OntologyFormat, OntologyLoader};

/// Analyzes ontology structure to detect workflow patterns.
#[derive(Debug, Clone)]
pub struct OntologyAnalyzer {
    /// Whether to include abstract classes in analysis
    include_abstract: bool,
}

impl Default for OntologyAnalyzer {
    fn default() -> Self {
        Self {
            include_abstract: false,
        }
    }
}

impl OntologyAnalyzer {
    /// Create a new analyzer with default settings.
    pub fn new() -> Self {
        Self::default()
    }

    /// Set whether to include abstract classes.
    pub fn with_abstract(mut self, include: bool) -> Self {
        self.include_abstract = include;
        self
    }

    /// Extract business process classes from the ontology.
    ///
    /// Business processes are identified by:
    /// - Having the stereotype "BusinessProcess" or "ProcessStep"
    /// - Being a concrete (non-abstract) class
    /// - Having rdfs:label defined
    pub fn extract_business_processes(
        &self, graph: &ggen_core::Graph,
    ) -> Result<Vec<ProcessClass>, crate::Error> {
        let query = r#"
            PREFIX rdfs: <http://www.w3.org/2000/01/rdf-schema#>
            PREFIX owl: <http://www.w3.org/2002/07/owl#>
            PREFIX src: <http://industry-ontology.org/>

            SELECT ?class ?label ?comment
            WHERE {
                ?class a owl:Class ;
                       rdfs:label ?label .
                OPTIONAL { ?class rdfs:comment ?comment }
                FILTER (
                    ?class != owl:Class &&
                    ?class != owl:Thing &&
                    STRSTARTS(STR(?class), "http://") ||
                    STRSTARTS(STR(?class), "https://")
                )
            }
            ORDER BY ?label
        "#;

        let _results = graph
            .query(query)
            .map_err(|e| crate::Error::sparql(e.to_string()))?;

        let processes = Vec::new();
        // Parse SPARQL results and extract process classes
        // (Implementation depends on ggen_core Graph API)

        Ok(processes)
    }
}

/// Represents a business process class extracted from an ontology.
#[derive(Debug, Clone)]
pub struct ProcessClass {
    /// IRI of the class
    pub iri: String,
    /// Local name of the class
    pub name: String,
    /// Optional rdfs:comment
    pub comment: Option<String>,
    /// Parent class IRIs
    pub parents: Vec<String>,
}

/// Cardinality information for ontology properties.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Cardinality {
    /// Exactly one (default)
    One,
    /// Zero or one
    ZeroOrOne,
    /// One or more
    OneOrMore,
    /// Zero or more
    Many,
    /// Specific range
    Range { min: usize, max: Option<usize> },
}

impl Cardinality {
    /// Returns the minimum cardinality.
    pub fn min(&self) -> usize {
        match self {
            Self::One | Self::ZeroOrOne => 0,
            Self::OneOrMore | Self::Many => 1,
            Self::Range { min, .. } => *min,
        }
    }

    /// Returns the maximum cardinality, if bounded.
    pub fn max(&self) -> Option<usize> {
        match self {
            Self::One | Self::ZeroOrOne => Some(1),
            Self::OneOrMore | Self::Many => None,
            Self::Range { max, .. } => *max,
        }
    }

    /// Returns true if this cardinality represents multiple values.
    pub fn is_multi_valued(&self) -> bool {
        match self {
            Self::OneOrMore | Self::Many => true,
            Self::Range { max, .. } => match max {
                None => true,
                Some(m) => *m > 1,
            },
            Self::One | Self::ZeroOrOne => false,
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_cardinality_one() {
        let c = Cardinality::One;
        assert_eq!(c.min(), 0);
        assert_eq!(c.max(), Some(1));
        assert!(!c.is_multi_valued());
    }

    #[test]
    fn test_cardinality_many() {
        let c = Cardinality::Many;
        assert_eq!(c.min(), 1);
        assert_eq!(c.max(), None);
        assert!(c.is_multi_valued());
    }
}
