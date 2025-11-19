//! # GGen Kaizen - Continuous Improvement Engine for Ontologies
//!
//! This crate implements Kaizen (continuous improvement) principles for ontology development,
//! featuring:
//!
//! - **PDCA Cycles**: Automated Plan-Do-Check-Act feedback loops for semantic refinement
//! - **Pain Point Mining**: Suggestion system that analyzes developer feedback
//! - **Standard Work**: Auto-generated documentation from best practices
//! - **5S Organization**: Systematic RDF namespace management (Sort, Set in order, Shine, Standardize, Sustain)
//! - **Incremental Refinement**: Small semantic improvements that compound over time
//!
//! ## Architecture
//!
//! ```text
//! ┌─────────────────────────────────────────────────────────────┐
//! │                  Kaizen Orchestrator                        │
//! │              (Continuous Improvement Loop)                  │
//! └─────────────────────────────────────────────────────────────┘
//!          │                                           ▲
//!          │ observes                                  │ feedback
//!          ▼                                           │
//! ┌──────────────────┐    ┌──────────────────┐    ┌──────────────────┐
//! │   Observation    │───▶│    Analysis      │───▶│ Recommendation   │
//! │   (Telemetry)    │    │  (Pattern Det.)  │    │  (Suggestions)   │
//! └──────────────────┘    └──────────────────┘    └──────────────────┘
//!                                                          │
//!                                                          │ proposes
//!                                                          ▼
//!                                                  ┌──────────────────┐
//!                                                  │    Evolution     │
//!                                                  │  (Auto-apply)    │
//!                                                  └──────────────────┘
//! ```

pub mod pdca;
pub mod suggestion;
pub mod refinement;
pub mod namespace;
pub mod documentation;
pub mod orchestrator;

pub use orchestrator::{KaizenOrchestrator, KaizenConfig};
pub use pdca::{PDCACycle, CyclePhase, ImprovementTarget};
pub use suggestion::{SuggestionEngine, PainPoint, Suggestion, PainPointCategory, Severity};
pub use refinement::{SemanticRefinement, RefinementTracker};
pub use namespace::{NamespaceOrganizer, FiveSPrinciple};
pub use documentation::{StandardWorkGenerator, BestPractice};

use thiserror::Error;

#[derive(Error, Debug)]
pub enum KaizenError {
    #[error("PDCA cycle error: {0}")]
    PDCAError(String),

    #[error("Suggestion generation failed: {0}")]
    SuggestionError(String),

    #[error("Semantic refinement error: {0}")]
    RefinementError(String),

    #[error("Namespace organization error: {0}")]
    NamespaceError(String),

    #[error("Documentation generation error: {0}")]
    DocumentationError(String),

    #[error("IO error: {0}")]
    IoError(#[from] std::io::Error),

    #[error("Serialization error: {0}")]
    SerializationError(#[from] serde_json::Error),
}

pub type Result<T> = std::result::Result<T, KaizenError>;
