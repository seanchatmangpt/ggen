//! Autonomous RDF Graph Evolution System
//!
//! This module provides intelligent, self-managing graph evolution capabilities:
//! - Natural language to RDF conversion with AI inference
//! - Graph delta detection and change tracking
//! - Self-validation with SPARQL constraint checking
//! - Atomic commit/rollback operations
//! - Evolution history tracking

pub mod delta_detector;
pub mod deployment;
pub mod events;
pub mod graph_evolution;
pub mod nl_parser;
pub mod orchestrator;
pub mod regeneration;
pub mod telemetry;
pub mod validator;

pub use delta_detector::{DeltaDetector, DeltaOperation, GraphDelta};
pub use deployment::{DeploymentAutomation, DeploymentConfig, DeploymentResult};
pub use events::{ChangeEvent, ChangeType, EventSubscriber, GraphChangeNotifier};
pub use graph_evolution::{EvolutionConfig, EvolutionResult, GraphEvolutionEngine};
pub use nl_parser::{InferredRelation, NaturalLanguageParser, ParsedTriples};
pub use orchestrator::{OrchestratorConfig, ParallelExecution, RegenerationOrchestrator};
pub use regeneration::{AffectedArtifact, DeltaChange, RegenerationConfig, RegenerationEngine};
pub use telemetry::{
    FeedbackLoop, PerformanceMetrics, TelemetryCollector, TelemetryConfig, TelemetryEvent,
    TelemetryEventType,
};
pub use validator::{ConstraintViolation, SelfValidator, ValidationResult};

/// Re-export commonly used types
pub use crate::error::{GgenAiError, Result};
