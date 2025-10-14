//! Universal lifecycle system for ggen (80/20 Implementation)
//!
//! This module implements the lifecycle orchestration system that enables
//! cross-language project management through make.toml
//!
//! # 80/20 Philosophy
//!
//! This implementation focuses on the 20% of features that provide 80% of value:
//! - Simple phase execution (init, setup, build, test, deploy)
//! - Sequential execution with optional workspace parallelism
//! - File-based state persistence
//! - Deterministic caching
//! - Before/after hooks with recursion detection
//!
//! # Excluded Complexity (YAGNI until proven needed)
//!
//! - DAG-based dependency resolution (hooks provide enough ordering)
//! - Complex execution modes and visualization (keep output simple)
//! - Error recovery strategies (fail fast, report clearly)
//! - Advanced caching strategies (SHA256 is sufficient)

pub mod cache;
pub mod error;
pub mod exec;
pub mod loader;
pub mod model;
pub mod state;

// Production readiness tracking (80/20 rule implementation)
pub mod production;
pub mod validation;

// Performance optimization for <60s deployment target
pub mod optimization;

// Future enhancements (available but not in public API):
#[allow(dead_code)]
mod dag; // DAG resolution - use direct hooks for now
#[allow(dead_code)]
mod dx; // Developer experience helpers - keep output simple for now

#[cfg(test)]
mod integration_test;

#[cfg(test)]
mod behavior_tests;

// Public API (minimal and focused)
pub use cache::cache_key;
pub use error::{LifecycleError, Result};
pub use exec::{run_phase, run_pipeline, Context};
pub use loader::load_make;
pub use model::{Hooks, Make, Phase, Project, Workspace};
pub use state::{load_state, save_state, LifecycleState};

// Production readiness exports
pub use production::{Placeholder, PlaceholderProcessor, PlaceholderRegistry};
pub use production::{
    ReadinessCategory, ReadinessReport, ReadinessRequirement, ReadinessStatus, ReadinessTracker,
};
pub use validation::{ReadinessValidator, ValidationIssue, ValidationResult, ValidationSeverity};

// Performance optimization exports
pub use optimization::{
    run_fast_validation, run_optimized_pipeline, ContainerPool, DependencyCache,
    ParallelOrchestrator, PerformanceTargets, PipelineProfiler, StageMetrics,
};
