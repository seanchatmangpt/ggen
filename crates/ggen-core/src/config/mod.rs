//! Configuration management for ggen
//!
//! This module provides configuration structures and utilities for various ggen
//! subsystems, including template generation options, marketplace settings, and
//! system-wide defaults.
//!
//! ## Features
//!
//! - **Template Configuration**: Search paths, default variables, cache settings
//! - **Generation Options**: Output directories, formatting, hooks, validation
//! - **Marketplace Settings**: Registry URLs, authentication, update policies
//!
//! ## Examples
//!
//! ### Loading Template Configuration
//!
//! ```rust,no_run
//! use ggen_core::config::TemplateConfig;
//! use std::path::PathBuf;
//!
//! # fn main() -> ggen_utils::error::Result<()> {
//! let config = TemplateConfig {
//!     search_paths: vec![PathBuf::from("templates")],
//!     default_variables: std::collections::HashMap::new(),
//!     metadata_store: PathBuf::from(".ggen/metadata"),
//!     cache_dir: Some(PathBuf::from(".ggen/cache")),
//!     generation: ggen_core::config::GenerationOptions::default(),
//!     marketplace: ggen_core::config::MarketplaceSettings::default(),
//! };
//! # Ok(())
//! # }
//! ```

pub mod andon_gemba;
pub mod hive_coordinator;
pub mod lock_manager;
pub mod ontology_config;
pub mod qa_cli;
pub mod quality_assurance;
pub mod swarm_coordinator;
pub mod swarm_intelligence;
pub mod system_health;
pub mod template_config;

#[cfg(test)]
mod ontology_integration_test;

#[cfg(test)]
mod qa_integration_test;

pub use andon_gemba::{
    ActionStatus, AlertStatus, Andon, AndonAlert, AndonHandler, AndonRule, AndonSeverity,
    GembaObservation, GembaWalk, GembaWalkSession, ImprovementAction, ObservationFrequency,
    ObservationImpact, ProblemArea, ProblemSeverity, ProblemStatus,
};
pub use hive_coordinator::{
    AgentRole, HiveAgent, HiveQueen, ResolutionSuggestion, ResolvedConfiguration,
};
pub use lock_manager::{CompositionMetadata, LockedPackage, LockfileManager, OntologyLockfile};
pub use ontology_config::{
    CompositionStrategy, ConflictResolution, GenerationHooks, LockConfig, OntologyConfig,
    OntologyPackRef, TargetConfig,
};
pub use qa_cli::{
    AndonStatusOutput, FmeaAnalysisOutput, GembaWalkOutput, HealthCheckOutput, QaCliError,
    QaCliManager, QaCliResult, QualityMetricsExport,
};
pub use quality_assurance::{
    DetectionMechanism, DifficultyLevel, FailureMode, MuraViolation, OptimizationOpportunity,
    PokaYoke, PreventionRule, PreventionType, SeverityLevel, Standard, ViolationSeverity,
    WasteItem, WasteType, FMEA, MUDA, MURA,
};
pub use swarm_coordinator::{
    CoordinatorStats, RecoveryConfig, SwarmCoordinator, Task, TaskStatus, TaskType,
};
pub use swarm_intelligence::{
    AgentMessage, AgentPerformance, AgentStatus, CollectiveMemory, ConsensusVoting, LearnedPattern,
    MemoryEntry, MessageType, Proposal, VotingStatus, WorkerState,
};
pub use system_health::{
    AlertThresholds, CheckResult, HealthMetrics, HealthMonitor, HealthStatus, HealthTrend,
    PreventionRegistry, QualityMetrics, QualityStatus, SystemHealthReport, ValidationRule,
};
pub use template_config::{GenerationOptions, MarketplaceSettings, TemplateConfig};
