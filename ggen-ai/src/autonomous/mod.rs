//! Autonomous RDF Graph Evolution System
//!
//! This module provides intelligent, self-managing graph evolution capabilities:
//! - Natural language to RDF conversion with AI inference
//! - Graph delta detection and change tracking
//! - Self-validation with SPARQL constraint checking
//! - Atomic commit/rollback operations
//! - Evolution history tracking

pub mod graph_evolution;
pub mod nl_parser;
pub mod validator;
pub mod delta_detector;
pub mod regeneration;
pub mod orchestrator;
pub mod telemetry;
pub mod deployment;
pub mod events;

pub use graph_evolution::{GraphEvolutionEngine, EvolutionConfig, EvolutionResult};
pub use nl_parser::{NaturalLanguageParser, ParsedTriples, InferredRelation};
pub use validator::{SelfValidator, ValidationResult, ConstraintViolation};
pub use delta_detector::{DeltaDetector, GraphDelta, DeltaOperation};
pub use regeneration::{RegenerationEngine, RegenerationConfig, AffectedArtifact, DeltaChange};
pub use orchestrator::{RegenerationOrchestrator, OrchestratorConfig, ParallelExecution};
pub use telemetry::{TelemetryCollector, TelemetryConfig, TelemetryEvent, TelemetryEventType, PerformanceMetrics, FeedbackLoop};
pub use deployment::{DeploymentAutomation, DeploymentConfig, DeploymentResult};
pub use events::{GraphChangeNotifier, ChangeEvent, ChangeType, EventSubscriber};

/// Re-export commonly used types
pub use crate::error::{GgenAiError, Result};
