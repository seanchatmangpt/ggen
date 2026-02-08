//! Code generation module
//!
//! This module provides generators for different target languages and frameworks,
//! converting OntologySchema into working code for TypeScript, GraphQL, SQL, and more.

pub mod audit;
pub mod code_graph;
pub mod concurrent;
pub mod dependency_validation;
pub mod execution_lifecycle;
pub mod execution_proof;
pub mod executor;
pub mod incremental; // Phase 4: Incremental generation with SHA256 JIT
pub mod incremental_cache;
#[allow(dead_code)]
pub mod lifecycle_hooks;
pub mod marketplace_integration;
pub mod merge;
pub mod pipeline;
pub mod proof_archive;
pub mod swarm_execution;
pub mod swarm_executor_bridge;
pub mod transaction; // Atomic file operations with rollback
pub mod typescript;
pub mod ux; // UX utilities: progress indicators and formatting
pub mod watch;
pub mod watch_cache_integration;
pub mod watch_mode;

// Re-export key types
pub use audit::{AuditOutput, AuditStep, AuditTrail, AuditTrailBuilder};
pub use code_graph::{
    CodeEnum, CodeField, CodeGraphBuilder, CodeImpl, CodeImport, CodeItem, CodeMethod, CodeModule,
    CodeParam, CodeStruct, CodeTrait, CodeVariant,
};
pub use concurrent::ConcurrentRuleExecutor;
pub use dependency_validation::{DependencyCheck, DependencyValidationReport, DependencyValidator};
pub use execution_lifecycle::{ExecutionLifecycle, PostSyncContext, PreSyncContext};
pub use execution_proof::{ExecutionProof, ProofCarrier, RuleExecution};
pub use executor::{
    OutputFormat, SyncExecutor, SyncOptions, SyncResult, SyncedFileInfo, ValidationCheck,
};
pub use incremental_cache::{CacheInvalidation, IncrementalCache};
pub use marketplace_integration::{MarketplaceValidator, PackageValidation, PreFlightReport};
pub use merge::{merge_sections, parse_merge_markers, MergeMarkers, MergedSections};
pub use pipeline::{
    ExecutedRule, GeneratedFile, GenerationPipeline, PipelineState, RuleType, ValidationResult,
    ValidationSeverity,
};
pub use proof_archive::{ChainVerification, ProofArchive};
pub use swarm_execution::{Agent, SwarmCoordinator, SwarmSummary};
pub use swarm_executor_bridge::{ExecutionStrategy, SwarmExecutorBridge};
pub use transaction::{FileTransaction, TransactionReceipt};
pub use typescript::TypeScriptGenerator;
