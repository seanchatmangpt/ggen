//! # ggen-temporal - Temporal Logic Reasoning Engine for 4D Ontologies
//!
//! This crate provides a comprehensive temporal logic reasoning system with:
//!
//! - **4D Ontology Support**: Treat time as a first-class dimension in RDF graphs
//! - **Event Sourcing**: Immutable event log with chrono-semantic versioning
//! - **Bidirectional Time-Travel**: Navigate code generation history forward and backward
//! - **Vector Clocks**: Distributed causal consistency tracking
//! - **Temporal Logic Operators**: Reason about temporal constraints (Always, Eventually, Until, etc.)
//! - **Semantic Projections**: Distributed views with causal guarantees
//!
//! ## Architecture
//!
//! The temporal engine is built on several core concepts:
//!
//! ### Event Sourcing
//! All changes are captured as immutable events in an append-only log. Each event has:
//! - Temporal coordinates (timestamp + vector clock)
//! - Semantic version (major.minor.patch + semantic delta)
//! - Causal dependencies
//! - Graph deltas (RDF triples added/removed)
//!
//! ### 4D Ontology
//! Extends RDF with temporal dimensions:
//! ```turtle
//! @prefix time: <http://www.w3.org/2006/time#> .
//! @prefix ggen-temporal: <http://ggen.dev/ontology/temporal#> .
//!
//! :entity_v1 a :MyClass ;
//!     ggen-temporal:validFrom "2025-01-01T00:00:00Z"^^xsd:dateTime ;
//!     ggen-temporal:validUntil "2025-06-01T00:00:00Z"^^xsd:dateTime ;
//!     ggen-temporal:eventId "evt_123" ;
//!     :property "value" .
//! ```
//!
//! ### Vector Clocks
//! Track causality in distributed code generation:
//! ```rust
//! use ggen_temporal::VectorClock;
//!
//! let mut clock = VectorClock::new("node-1".to_string());
//! clock.tick(); // Increment local clock
//! let timestamp = clock.timestamp(); // Get current vector time
//! ```
//!
//! ### Temporal Logic
//! Express temporal constraints and queries:
//! ```rust
//! use ggen_temporal::temporal_logic::*;
//!
//! // "Always property X holds"
//! let formula = TemporalFormula::Always(Box::new(
//!     TemporalFormula::Predicate("has_tests".to_string())
//! ));
//!
//! // "Eventually property Y becomes true"
//! let formula2 = TemporalFormula::Eventually(Box::new(
//!     TemporalFormula::Predicate("is_production_ready".to_string())
//! ));
//! ```
//!
//! ### Time Travel Debugging
//! Navigate through code generation history:
//! ```rust
//! use ggen_temporal::TimeTravelDebugger;
//!
//! let debugger = TimeTravelDebugger::new(event_store);
//!
//! // Go back to a specific event
//! debugger.rewind_to_event("evt_123").await?;
//!
//! // Step forward one event
//! debugger.step_forward().await?;
//!
//! // Jump to a semantic version
//! debugger.jump_to_version("2.1.0").await?;
//! ```

#![deny(warnings)]
#![allow(dead_code)] // Allow during initial development

pub mod event_sourcing;
pub mod ontology_4d;
pub mod semantic_projection;
pub mod temporal_logic;
pub mod time_travel;
pub mod vector_clock;

// Re-export core types
pub use event_sourcing::{
    ChronoSemanticVersion, Event, EventId, EventStore, EventStream, SemanticDelta,
};
pub use ontology_4d::{TemporalGraph, TemporalQuery, TemporalTriple, TimeRange, ValidTime};
pub use semantic_projection::{
    CausalConsistency, ProjectionNode, ProjectionView, SemanticProjection,
};
pub use temporal_logic::{
    TemporalFormula, TemporalOperator, TemporalReasoner, TemporalState, TruthValue,
};
pub use time_travel::{Checkpoint, TimeMachine, TimePoint, TimeTravelDebugger, TraversalMode};
pub use vector_clock::{NodeId, VectorClock, VectorTime};

use thiserror::Error;

/// Errors that can occur in the temporal reasoning engine
#[derive(Error, Debug)]
pub enum TemporalError {
    #[error("Event not found: {0}")]
    EventNotFound(String),

    #[error("Causal consistency violation: {0}")]
    CausalityViolation(String),

    #[error("Invalid temporal query: {0}")]
    InvalidQuery(String),

    #[error("Time travel failed: {0}")]
    TimeTravelFailed(String),

    #[error("Version conflict: {0}")]
    VersionConflict(String),

    #[error("Graph error: {0}")]
    GraphError(String),

    #[error("Serialization error: {0}")]
    SerializationError(#[from] serde_json::Error),

    #[error("I/O error: {0}")]
    IoError(#[from] std::io::Error),

    #[error(transparent)]
    Other(#[from] anyhow::Error),
}

pub type Result<T> = std::result::Result<T, TemporalError>;

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_temporal_error_display() {
        let err = TemporalError::EventNotFound("evt_123".to_string());
        assert_eq!(err.to_string(), "Event not found: evt_123");
    }
}
