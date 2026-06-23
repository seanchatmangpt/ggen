//! # ggen-core - Core graph-aware code generation engine
//!
//! This crate provides the core functionality for RDF-based code generation,
//! including template processing, RDF handling, and deterministic output generation.
//!
//! ## Overview
//!
//! `ggen-core` is the foundational crate for the ggen code generation system. It provides:
//!
//! - **Template Processing**: Parse, render, and generate code from templates with RDF integration
//! - **RDF Graph Management**: Load, query, and manipulate RDF data using SPARQL
//! - **Project Generation**: Scaffold complete projects from templates
//! - **Registry Integration**: Discover and install template packs from the registry
//! - **Lifecycle Management**: Orchestrate build, test, and deployment phases
//! - **Deterministic Output**: Ensure reproducible code generation
//!
//! ## Key Modules
//!
//! ### Template System
//! - [`template`] - Core template parsing and rendering
//! - [`templates`] - File tree generation from templates
//! - [`pipeline`] - Template processing pipeline
//! - [`generator`] - High-level generation engine
//!
//! ### RDF Integration
//! - [`graph`] - RDF graph management with SPARQL caching
//! - [`rdf`] - Template metadata and validation
//! - [`delta`] - Delta-driven projection for graph changes
//!
//! ### Project Management
//! - [`project_generator`] - Scaffold new projects (Rust, Next.js, etc.)
//! - [`cli_generator`] - Generate CLI projects from ontologies
//! - [`lifecycle`] - Universal lifecycle system for cross-language projects
//!
//! ### Registry and Packs
//! - [`registry`] - Registry client for pack discovery
//! - [`cache`] - Local cache manager for downloaded packs
//! - [`lockfile`] - Dependency lockfile management
//! - [`resolver`] - Template resolution from packs
//! - [`gpack`] - Gpack manifest structure and file discovery
//!
//! ### Utilities
//! - [`inject`] - File injection utilities
//! - [`merge`] - Three-way merge for delta-driven projection
//! - [`snapshot`] - Snapshot management for baselines
//! - [`preprocessor`] - Template preprocessor pipeline
//! - [`register`] - Tera filter and function registration
//! - [`tera_env`] - Tera template engine environment utilities
//!
//! ### Security (Week 4 Hardening)
//! - [`security`] - Security hardening module (command injection prevention, input validation)
//!
//! ## Quick Start
//!
//! ### Basic Template Generation
//!
//! ```rust,no_run
//! use crate::{Generator, GenContext, Pipeline};
//! use std::collections::BTreeMap;
//! use std::path::PathBuf;
//!
//! # fn main() -> crate::utils::error::Result<()> {
//! let pipeline = Pipeline::new()?;
//! let mut vars = BTreeMap::new();
//! vars.insert("name".to_string(), "MyApp".to_string());
//!
//! let ctx = GenContext::new(
//!     PathBuf::from("template.tmpl"),
//!     PathBuf::from("output")
//! ).with_vars(vars);
//!
//! let mut generator = Generator::new(pipeline, ctx);
//! let output_path = generator.generate()?;
//! println!("Generated: {:?}", output_path);
//! # Ok(())
//! # }
//! ```
//!
//! ### Using RDF Graph
//!
//! ```rust
//! use ggen_core::Graph;
//! use oxigraph::sparql::QueryResults;
//!
//! # fn main() -> ggen_core::utils::error::Result<()> {
//! let graph = Graph::new()?;
//! graph.insert_turtle(r#"
//!     @prefix ex: <http://example.org/> .
//!     ex:alice a ex:Person ;
//!              ex:name "Alice" .
//! "#)?;
//!
//! let results = graph.query("SELECT ?s ?o WHERE { ?s ex:name ?o }")?;
//! if let QueryResults::Solutions(solutions) = results {
//!     assert!(solutions.count() > 0);
//! }
//! # Ok(())
//! # }
//! ```
//!
//! ### Creating a New Project
//!
//! ```rust,no_run
//! use crate::project_generator::{ProjectConfig, ProjectType, create_new_project};
//! use std::path::PathBuf;
//!
//! # async fn example() -> crate::utils::error::Result<()> {
//! let config = ProjectConfig {
//!     name: "my-cli".to_string(),
//!     project_type: ProjectType::RustCli,
//!     framework: None,
//!     path: PathBuf::from("."),
//! };
//!
//! create_new_project(&config).await?;
//! # Ok(())
//! # }
//! ```
#![deny(warnings)]
#![allow(unexpected_cfgs)]
#![allow(unused_imports)]
#![allow(dead_code)] // Poka-Yoke: Prevent warnings at compile time - compiler enforces correctness
#![allow(
    clippy::manual_checked_ops,
    clippy::unnecessary_sort_by,
    clippy::collapsible_match,
    clippy::explicit_counter_loop,
    clippy::match_result_ok,
    clippy::collapsible_if,
    clippy::new_without_default,
    clippy::if_same_then_else,
    clippy::for_kv_map,
    clippy::expect_used,
    clippy::unwrap_used,
    clippy::panic,
    clippy::needless_raw_string_hashes,
    clippy::ignore_without_reason,
    clippy::no_effect_underscore_binding,
    clippy::unnested_or_patterns,
    clippy::iter_on_single_items,
    clippy::used_underscore_binding,
    clippy::future_not_send,
    clippy::format_collect,
    clippy::suspicious_doc_comments,
    clippy::ignored_unit_patterns
)]
pub mod agent;
pub mod audit;
pub mod cache;
pub mod canonical;
pub mod cli_generator;
pub mod codegen;
pub mod codegen_lib;
pub mod delta;
pub mod dflss;
pub mod domain;
pub mod e2e_tests;
pub mod generator;
pub mod genesis;
pub mod github;
pub mod gpack;
pub mod graph;
pub mod inject;
pub mod lean_six_sigma;
pub mod lifecycle;
pub mod lockfile;
pub mod manifest;
pub mod membrane;
pub mod merge;
pub mod ontology;
pub mod ontology_core;
pub mod ontology_pack;
pub mod parallel_generator;
pub mod parts_execution;
pub mod parts_foundry;
pub mod pipeline;
pub mod pipeline_engine;
pub mod pki;
pub mod poc;
pub mod poka_yoke;
pub mod pqc;
pub mod preprocessor;
pub mod project_generator;
pub mod prompt_mfg;
pub mod rdf;
pub mod register;
pub mod registry;
pub mod resolver;
pub mod schema;
pub mod semantic_bit;
pub mod simple_tracing;
pub mod snapshot;
pub mod stewardship;
pub mod stpnt;
pub mod streaming_generator;
pub mod telemetry;
pub mod template;
pub mod template_cache;
pub mod template_types;
pub mod templates;
pub mod tera_env;
pub mod tracing;
pub mod transport;
pub mod types;
pub mod utils;
pub mod validation;

// Schema parser and code generators for A2A communication
pub mod drift; // Drift detection for ontology changes
#[cfg(test)]
pub mod manufacturing; // DMAIC quality gates for Lean Six Sigma
pub mod metrics; // Quality metrics system (Code, Process, Six Sigma, TPS, Flow, OEE, Kaizen)
                 // Ontology system - re-enabled after oxigraph API compatibility fixes
pub mod pack_resolver; // μ₀: Pack resolution stage
pub mod packs; // Pack installation system - Phase 1
pub mod security; // Week 4 Security Hardening
pub mod sync; // Sync orchestrator: load_ontology → run_sparql → generate_code → validate → write_files
              // v26_5_19: Fully-Rendered Libraries via Ontology-First Compilation (A = μ(O))

// Re-export template types
pub use template_types::{Frontmatter, Template};

// Re-export production readiness types from lifecycle module
pub use lifecycle::{
    Placeholder, PlaceholderProcessor, PlaceholderRegistry, ReadinessCategory, ReadinessReport,
    ReadinessRequirement, ReadinessStatus, ReadinessTracker,
};

// Re-export quality metrics types
pub use metrics::{
    CodeMetrics, DefectMetrics, FlowMetrics, KaizenMetrics, MetricsCollector, MetricsReport,
    OEEMetrics, ProcessMetrics, WasteMetrics, WasteType,
};

// Re-export poka-yoke (error prevention) types
pub use lifecycle::{
    Closed, Counter, EmptyPathError, EmptyStringError, FileHandle, NonEmptyPath, NonEmptyString,
    Open,
};

// Re-export commonly used types for convenience
pub use cache::{CacheManager, CachedPack};
// Re-export A2A types from ggen-a2a-mcp crate for backward compatibility
pub use delta::{DeltaType, GraphDelta, ImpactAnalyzer, TemplateImpact};
pub use drift::{ChangeType, DriftChange, DriftDetector, DriftStatus, FileHashState, SyncState};
pub use generator::{GenContext, Generator};
// pub use ggen_a2a_mcp::{a2a, a2a_generated, a2a_registry};
pub use ggen_config::config::LockfileManager;
pub use ggen_config::{config, config_lib, ConfigError, ConfigLoader, GgenConfig, Result};
// Re-export marketplace types from ggen-marketplace crate for backward compatibility
pub use ggen_marketplace::{
    marketplace, Manifest, Package, PackageId, QualityScore, RdfRegistry, SparqlSearchEngine,
};
// Re-export receipt types from ggen-config crate for backward compatibility
pub use ggen_config::{
    chain, create_chained_receipt, envelope, error, generate_keypair, hash_data, payload_hash,
    receipt_impl, EnvelopeChain, EnvelopeChainLink, EnvelopeSignature, PayloadRef, Producer,
    Receipt, ReceiptChain, ReceiptEnvelope, ReceiptError, ENVELOPE_SCHEMA, HASH_PREFIX,
    SIGNATURE_ALGORITHM,
};
pub mod receipt {
    pub use ggen_config::*;
}
pub use github::{GitHubClient, PagesConfig, RepoInfo, WorkflowRun, WorkflowRunsResponse};
pub use gpack::GpackManifest;
pub use graph::Graph;
pub use lockfile::Lockfile;
pub use merge::{
    ConflictType, MergeConflict, MergeResult, MergeStrategy, RegionAwareMerger, RegionUtils,
    ThreeWayMerger,
};
// Re-export commonly used types for convenience
pub use packs::{LockedPack, PackLockfile, PackSource};
pub use pipeline::{Pipeline, PipelineBuilder};
pub use pki::{verify_ed25519, KeyPurpose, PkiManager, TrustedKeyEntry, TrustedKeysConfig};
pub use pqc::{calculate_sha256, calculate_sha256_file, PqcSigner, PqcVerifier};
pub use rdf::{
    GgenOntology, Iri, Literal, SparqlQueryBuilder, TemplateMetadata, TemplateMetadataStore,
    TemplateRelationship, TemplateVariable, ValidationReport, ValidationResult, Validator,
    Variable, GGEN_NAMESPACE,
};
pub use registry::{RegistryClient, RegistryIndex, ResolvedPack, SearchResult};
pub use resolver::{TemplateResolver, TemplateSearchResult, TemplateSource};
pub use snapshot::{
    FileSnapshot, GraphSnapshot, Region, RegionType, Snapshot, SnapshotManager, TemplateSnapshot,
};
// Re-export template-to-file-tree generation types
pub use templates::{
    generate_file_tree, FileTreeGenerator, FileTreeNode, FileTreeTemplate, GenerationResult,
    NodeType, TemplateContext, TemplateFormat, TemplateParser,
};

// Re-export ontology pack types
pub use ontology_pack::{
    Cardinality, CodeGenTarget, OntologyClass, OntologyConfig, OntologyDefinition, OntologyFormat,
    OntologyPackMetadata, OntologyProperty, OntologyRelationship, OntologySchema, PropertyRange,
    RelationshipType,
};

// Ontology system re-exports
// Re-enabled after oxigraph API compatibility fixes (Statement::From<Quad> and as_dataset)
pub use ontology::{
    AtomicPromotionCheck,
    // Promotion
    AtomicSnapshotPromoter,
    // Control loop
    AutonomousControlLoop,
    CompositeValidator,
    Constitution,
    ConstitutionValidation,
    ControlLoopConfig,
    // Delta proposer
    DeltaSigmaProposal,
    DeltaSigmaProposer,
    DynamicValidator,
    GuardSoundnessCheck,
    ImmutabilityCheck,
    Invariant,
    // Constitution
    InvariantCheck,
    InvariantResult,
    IterationTelemetry,
    LoopState,
    MinerConfig,
    MockDynamicValidator,
    MockLLMProposer,
    MockPerformanceValidator,
    MockStaticValidator,
    NoRetrocausationCheck,
    Observation,
    ObservationSource,
    OntClass,
    OntProperty,
    OntologyError,
    OntologyExtractor,
    OntologyResult,
    OntologyStats,
    // Pattern mining
    Pattern,
    PatternMiner,
    PatternType,
    PerformanceMetrics,
    PerformanceValidator,
    ProjectionDeterminismCheck,
    PromotionMetrics,
    PromotionResult,
    ProposedChange,
    ProposerConfig,
    RealLLMProposer,
    SLOPreservationCheck,
    SigmaOverlay,
    SigmaReceipt,
    SigmaRuntime,
    SigmaSnapshot,
    // Sigma runtime
    SigmaSnapshotId,
    SnapshotGuard,
    SnapshotMetadata,
    StaticValidator,
    TestResult,
    TypeSoundnessCheck,
    ValidationContext,
    // Validators
    ValidationEvidence,
    ValidatorResult,
};

// Note: Cardinality, OntologySchema, PropertyRange are exported from ontology_pack module above
// to avoid conflicts with ontology module exports
