//! 4D ontology extensions for RDF graphs
//!
//! Extends RDF graphs with temporal dimensions, treating time as a first-class
//! entity in the ontology. This enables temporal reasoning over knowledge graphs.
//!
//! ## Temporal RDF Vocabulary
//!
//! ```turtle
//! @prefix time: <http://www.w3.org/2006/time#> .
//! @prefix ggen-temporal: <http://ggen.dev/ontology/temporal#> .
//! @prefix xsd: <http://www.w3.org/2001/XMLSchema#> .
//!
//! :Entity_v1 a :MyClass ;
//!     ggen-temporal:validFrom "2025-01-01T00:00:00Z"^^xsd:dateTime ;
//!     ggen-temporal:validUntil "2025-06-01T00:00:00Z"^^xsd:dateTime ;
//!     ggen-temporal:eventId "evt_123" ;
//!     ggen-temporal:vectorTime "{node-1:5, node-2:3}" ;
//!     ggen-temporal:version "1.0.0" ;
//!     :property "value" .
//! ```
//!
//! ## Example Usage
//!
//! ```rust
//! use ggen_temporal::ontology_4d::*;
//! use chrono::Utc;
//!
//! # fn example() -> Result<(), Box<dyn std::error::Error>> {
//! let mut graph = TemporalGraph::new()?;
//!
//! // Add a temporal triple
//! let triple = TemporalTriple {
//!     subject: "ex:alice".to_string(),
//!     predicate: "ex:worksAt".to_string(),
//!     object: "ex:CompanyA".to_string(),
//!     valid_time: ValidTime::TimeRange(TimeRange {
//!         start: Utc::now(),
//!         end: None,
//!     }),
//!     event_id: Some("evt_123".to_string()),
//!     vector_time: None,
//! };
//!
//! graph.insert_temporal_triple(triple)?;
//!
//! // Query at a specific time
//! let results = graph.query_at_time(
//!     "SELECT ?o WHERE { ex:alice ex:worksAt ?o }",
//!     Utc::now()
//! )?;
//! # Ok(())
//! # }
//! ```

use crate::event_sourcing::EventId;
use crate::vector_clock::VectorTime;
use crate::{Result, TemporalError};
use chrono::{DateTime, Utc};
use ggen_core::Graph;
use serde::{Deserialize, Serialize};
use std::collections::HashMap;

/// Temporal namespace for RDF
pub const TEMPORAL_NAMESPACE: &str = "http://ggen.dev/ontology/temporal#";

/// Valid time specification for temporal triples
#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub enum ValidTime {
    /// Single point in time
    Instant(DateTime<Utc>),
    /// Time range with optional end
    TimeRange(TimeRange),
    /// Always valid
    Always,
}

impl ValidTime {
    /// Check if a time point is within this valid time
    #[must_use]
    pub fn contains(&self, time: DateTime<Utc>) -> bool {
        match self {
            Self::Instant(t) => *t == time,
            Self::TimeRange(range) => range.contains(time),
            Self::Always => true,
        }
    }

    /// Check if this valid time overlaps with another
    #[must_use]
    pub fn overlaps(&self, other: &ValidTime) -> bool {
        match (self, other) {
            (Self::Always, _) | (_, Self::Always) => true,
            (Self::Instant(t1), Self::Instant(t2)) => t1 == t2,
            (Self::Instant(t), Self::TimeRange(range))
            | (Self::TimeRange(range), Self::Instant(t)) => range.contains(*t),
            (Self::TimeRange(r1), Self::TimeRange(r2)) => r1.overlaps(r2),
        }
    }
}

/// Time range with start and optional end
#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub struct TimeRange {
    pub start: DateTime<Utc>,
    pub end: Option<DateTime<Utc>>,
}

impl TimeRange {
    #[must_use]
    pub const fn new(start: DateTime<Utc>, end: Option<DateTime<Utc>>) -> Self {
        Self { start, end }
    }

    /// Check if a time point is within this range
    #[must_use]
    pub fn contains(&self, time: DateTime<Utc>) -> bool {
        time >= self.start && self.end.map_or(true, |end| time <= end)
    }

    /// Check if this range overlaps with another
    #[must_use]
    pub fn overlaps(&self, other: &TimeRange) -> bool {
        // Check if ranges overlap
        let this_end = self.end.unwrap_or(DateTime::<Utc>::MAX_UTC);
        let other_end = other.end.unwrap_or(DateTime::<Utc>::MAX_UTC);

        self.start <= other_end && other.start <= this_end
    }

    /// Get the duration of this time range in seconds
    #[must_use]
    pub fn duration_secs(&self) -> Option<i64> {
        self.end.map(|end| (end - self.start).num_seconds())
    }
}

/// A temporal RDF triple with valid time and causality information
#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub struct TemporalTriple {
    /// Subject (entity)
    pub subject: String,
    /// Predicate (relationship)
    pub predicate: String,
    /// Object (value or entity)
    pub object: String,
    /// Valid time for this triple
    pub valid_time: ValidTime,
    /// Event that created this triple
    pub event_id: Option<EventId>,
    /// Vector time for causal consistency
    pub vector_time: Option<VectorTime>,
}

impl TemporalTriple {
    /// Create a new temporal triple
    #[must_use]
    pub fn new(
        subject: String,
        predicate: String,
        object: String,
        valid_time: ValidTime,
    ) -> Self {
        Self {
            subject,
            predicate,
            object,
            valid_time,
            event_id: None,
            vector_time: None,
        }
    }

    /// Set the event ID
    #[must_use]
    pub fn with_event_id(mut self, event_id: EventId) -> Self {
        self.event_id = Some(event_id);
        self
    }

    /// Set the vector time
    #[must_use]
    pub fn with_vector_time(mut self, vector_time: VectorTime) -> Self {
        self.vector_time = Some(vector_time);
        self
    }

    /// Convert to RDF turtle string with temporal metadata
    #[must_use]
    pub fn to_turtle(&self) -> String {
        let base_triple = format!("{} {} {} .", self.subject, self.predicate, self.object);

        let mut metadata = Vec::new();

        // Add temporal metadata
        match &self.valid_time {
            ValidTime::Instant(t) => {
                metadata.push(format!(
                    "    ggen-temporal:validAt \"{}\"^^xsd:dateTime",
                    t.to_rfc3339()
                ));
            }
            ValidTime::TimeRange(range) => {
                metadata.push(format!(
                    "    ggen-temporal:validFrom \"{}\"^^xsd:dateTime",
                    range.start.to_rfc3339()
                ));
                if let Some(end) = range.end {
                    metadata.push(format!(
                        "    ggen-temporal:validUntil \"{}\"^^xsd:dateTime",
                        end.to_rfc3339()
                    ));
                }
            }
            ValidTime::Always => {
                metadata.push("    ggen-temporal:validAlways true".to_string());
            }
        }

        if let Some(ref event_id) = self.event_id {
            metadata.push(format!("    ggen-temporal:eventId \"{event_id}\""));
        }

        if let Some(ref vector_time) = self.vector_time {
            metadata.push(format!(
                "    ggen-temporal:vectorTime \"{}\"",
                vector_time
            ));
        }

        if metadata.is_empty() {
            base_triple
        } else {
            format!("{}\n{} .", base_triple, metadata.join(" ;\n"))
        }
    }
}

/// Temporal query specification
#[derive(Debug, Clone)]
pub struct TemporalQuery {
    /// SPARQL query string
    pub sparql: String,
    /// Time point for the query
    pub time: Option<DateTime<Utc>>,
    /// Time range for the query
    pub time_range: Option<TimeRange>,
    /// Vector time constraints
    pub vector_time_constraint: Option<VectorTime>,
}

impl TemporalQuery {
    #[must_use]
    pub fn new(sparql: String) -> Self {
        Self {
            sparql,
            time: None,
            time_range: None,
            vector_time_constraint: None,
        }
    }

    #[must_use]
    pub fn at_time(mut self, time: DateTime<Utc>) -> Self {
        self.time = Some(time);
        self
    }

    #[must_use]
    pub fn in_range(mut self, range: TimeRange) -> Self {
        self.time_range = Some(range);
        self
    }

    #[must_use]
    pub fn before_vector_time(mut self, vector_time: VectorTime) -> Self {
        self.vector_time_constraint = Some(vector_time);
        self
    }
}

/// Temporal RDF graph with 4D ontology support
pub struct TemporalGraph {
    /// Underlying RDF graph
    base_graph: Graph,
    /// Temporal triples indexed by subject
    temporal_triples: HashMap<String, Vec<TemporalTriple>>,
}

impl TemporalGraph {
    /// Create a new temporal graph
    pub fn new() -> Result<Self> {
        let base_graph = Graph::new().map_err(|e| TemporalError::GraphError(e.to_string()))?;

        Ok(Self {
            base_graph,
            temporal_triples: HashMap::new(),
        })
    }

    /// Insert a temporal triple
    pub fn insert_temporal_triple(&mut self, triple: TemporalTriple) -> Result<()> {
        // Store in temporal index
        self.temporal_triples
            .entry(triple.subject.clone())
            .or_insert_with(Vec::new)
            .push(triple.clone());

        // Convert to RDF and insert into base graph
        let turtle = triple.to_turtle();
        self.base_graph
            .insert_turtle(&turtle)
            .map_err(|e| TemporalError::GraphError(e.to_string()))?;

        Ok(())
    }

    /// Query triples valid at a specific time
    pub fn query_at_time(
        &self,
        sparql: &str,
        time: DateTime<Utc>,
    ) -> Result<Vec<HashMap<String, String>>> {
        // Filter temporal triples by time
        let valid_triples: Vec<&TemporalTriple> = self
            .temporal_triples
            .values()
            .flatten()
            .filter(|t| t.valid_time.contains(time))
            .collect();

        // Create a temporary graph with only valid triples
        let mut temp_graph = Graph::new().map_err(|e| TemporalError::GraphError(e.to_string()))?;

        for triple in valid_triples {
            let simple_turtle = format!("{} {} {} .", triple.subject, triple.predicate, triple.object);
            temp_graph
                .insert_turtle(&simple_turtle)
                .map_err(|e| TemporalError::GraphError(e.to_string()))?;
        }

        // Execute query on filtered graph
        temp_graph
            .query(sparql)
            .map_err(|e| TemporalError::GraphError(e.to_string()))
    }

    /// Query triples valid in a time range
    pub fn query_in_range(
        &self,
        sparql: &str,
        range: &TimeRange,
    ) -> Result<Vec<HashMap<String, String>>> {
        // Filter temporal triples by time range
        let valid_triples: Vec<&TemporalTriple> = self
            .temporal_triples
            .values()
            .flatten()
            .filter(|t| match &t.valid_time {
                ValidTime::Always => true,
                ValidTime::Instant(inst) => range.contains(*inst),
                ValidTime::TimeRange(tr) => range.overlaps(tr),
            })
            .collect();

        // Create a temporary graph with valid triples
        let mut temp_graph = Graph::new().map_err(|e| TemporalError::GraphError(e.to_string()))?;

        for triple in valid_triples {
            let simple_turtle = format!("{} {} {} .", triple.subject, triple.predicate, triple.object);
            temp_graph
                .insert_turtle(&simple_turtle)
                .map_err(|e| TemporalError::GraphError(e.to_string()))?;
        }

        temp_graph
            .query(sparql)
            .map_err(|e| TemporalError::GraphError(e.to_string()))
    }

    /// Get all triples for a subject across all times
    #[must_use]
    pub fn get_temporal_history(&self, subject: &str) -> Vec<&TemporalTriple> {
        self.temporal_triples
            .get(subject)
            .map(|triples| triples.iter().collect())
            .unwrap_or_default()
    }

    /// Get triples causally before a vector time
    #[must_use]
    pub fn get_triples_before(&self, vector_time: &VectorTime) -> Vec<&TemporalTriple> {
        self.temporal_triples
            .values()
            .flatten()
            .filter(|t| {
                t.vector_time
                    .as_ref()
                    .map_or(false, |vt| vt.happened_before(vector_time))
            })
            .collect()
    }

    /// Execute a general temporal query
    pub fn execute_temporal_query(&self, query: &TemporalQuery) -> Result<Vec<HashMap<String, String>>> {
        if let Some(time) = query.time {
            self.query_at_time(&query.sparql, time)
        } else if let Some(ref range) = query.time_range {
            self.query_in_range(&query.sparql, range)
        } else {
            // No temporal constraint - query full graph
            self.base_graph
                .query(&query.sparql)
                .map_err(|e| TemporalError::GraphError(e.to_string()))
        }
    }

    /// Get the underlying base graph
    #[must_use]
    pub const fn base_graph(&self) -> &Graph {
        &self.base_graph
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_time_range_contains() {
        let start = Utc::now();
        let end = start + chrono::Duration::hours(1);
        let range = TimeRange::new(start, Some(end));

        let mid = start + chrono::Duration::minutes(30);
        assert!(range.contains(mid));

        let before = start - chrono::Duration::minutes(1);
        assert!(!range.contains(before));

        let after = end + chrono::Duration::minutes(1);
        assert!(!range.contains(after));
    }

    #[test]
    fn test_time_range_overlaps() {
        let start1 = Utc::now();
        let end1 = start1 + chrono::Duration::hours(2);
        let range1 = TimeRange::new(start1, Some(end1));

        let start2 = start1 + chrono::Duration::hours(1);
        let end2 = start2 + chrono::Duration::hours(2);
        let range2 = TimeRange::new(start2, Some(end2));

        assert!(range1.overlaps(&range2));
        assert!(range2.overlaps(&range1));
    }

    #[test]
    fn test_valid_time_contains() {
        let time = Utc::now();
        let valid = ValidTime::Instant(time);
        assert!(valid.contains(time));

        let range = TimeRange::new(
            time - chrono::Duration::hours(1),
            Some(time + chrono::Duration::hours(1)),
        );
        let valid_range = ValidTime::TimeRange(range);
        assert!(valid_range.contains(time));

        assert!(ValidTime::Always.contains(time));
    }

    #[test]
    fn test_temporal_triple_creation() {
        let triple = TemporalTriple::new(
            "ex:alice".to_string(),
            "ex:worksAt".to_string(),
            "ex:CompanyA".to_string(),
            ValidTime::Always,
        )
        .with_event_id("evt_123".to_string());

        assert_eq!(triple.subject, "ex:alice");
        assert_eq!(triple.event_id, Some("evt_123".to_string()));
    }

    #[test]
    fn test_temporal_triple_to_turtle() {
        let time = Utc::now();
        let triple = TemporalTriple::new(
            "ex:alice".to_string(),
            "ex:knows".to_string(),
            "ex:bob".to_string(),
            ValidTime::Instant(time),
        );

        let turtle = triple.to_turtle();
        assert!(turtle.contains("ex:alice"));
        assert!(turtle.contains("ex:knows"));
        assert!(turtle.contains("ggen-temporal:validAt"));
    }

    #[test]
    fn test_temporal_graph() -> Result<()> {
        let mut graph = TemporalGraph::new()?;

        let triple = TemporalTriple::new(
            "<http://example.org/alice>".to_string(),
            "<http://example.org/name>".to_string(),
            "\"Alice\"".to_string(),
            ValidTime::Always,
        );

        graph.insert_temporal_triple(triple)?;

        let history = graph.get_temporal_history("<http://example.org/alice>");
        assert_eq!(history.len(), 1);

        Ok(())
    }

    #[test]
    fn test_temporal_query() {
        let query = TemporalQuery::new("SELECT ?s WHERE { ?s ?p ?o }".to_string())
            .at_time(Utc::now());

        assert!(query.time.is_some());
        assert!(query.time_range.is_none());
    }
}
