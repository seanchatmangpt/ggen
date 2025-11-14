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
//! ## Quick Start
//!
//! ### Basic Template Generation
//!
//! ```rust,no_run
//! use ggen_core::{Generator, GenContext, Pipeline};
//! use std::collections::BTreeMap;
//! use std::path::PathBuf;
//!
//! # fn main() -> ggen_utils::error::Result<()> {
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
//!
//! # fn main() -> ggen_utils::error::Result<()> {
//! let graph = Graph::new()?;
//! graph.insert_turtle(r#"
//!     @prefix ex: <http://example.org/> .
//!     ex:alice a ex:Person ;
//!              ex:name "Alice" .
//! "#)?;
//!
//! let results = graph.query("SELECT ?s ?o WHERE { ?s ex:name ?o }")?;
//! assert!(!results.is_empty());
//! # Ok(())
//! # }
//! ```
//!
//! ### Creating a New Project
//!
//! ```rust,no_run
//! use ggen_core::project_generator::{ProjectConfig, ProjectType, create_new_project};
//! use std::path::PathBuf;
//!
//! # async fn example() -> anyhow::Result<()> {
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

#![deny(warnings)] // Poka-Yoke: Prevent warnings at compile time - compiler enforces correctness

pub mod cache;
pub mod cli_generator;
pub mod config;
pub mod delta;
#[cfg(test)]
pub mod e2e_tests;
pub mod generator;
pub mod github;
pub mod gpack;
pub mod graph;
pub mod inject;
pub mod lifecycle;
pub mod lockfile;
pub mod merge;
pub mod pipeline;
pub mod poc;
pub mod pqc;
pub mod preprocessor;
pub mod project_generator;
pub mod rdf;
pub mod register;
pub mod registry;
pub mod resolver;
pub mod snapshot;
pub mod streaming_generator;
pub mod telemetry;
pub mod template;
pub mod template_cache;
pub mod templates;
pub mod tera_env;
// pub mod tracing; // Temporarily disabled due to missing tracing_subscriber dependency
pub mod simple_tracing;

// Re-export production readiness types from lifecycle module
pub use lifecycle::{
    Placeholder, PlaceholderProcessor, PlaceholderRegistry, ReadinessCategory, ReadinessReport,
    ReadinessRequirement, ReadinessStatus, ReadinessTracker,
};

// Re-export poka-yoke (error prevention) types
pub use lifecycle::{
    Closed, Counter, EmptyPathError, EmptyStringError, FileHandle, NonEmptyPath, NonEmptyString,
    Open,
};

// Re-export commonly used types for convenience
pub use cache::{CacheManager, CachedPack};
pub use delta::{DeltaType, GraphDelta, ImpactAnalyzer, TemplateImpact};
pub use generator::{GenContext, Generator};
pub use github::{GitHubClient, PagesConfig, RepoInfo, WorkflowRun, WorkflowRunsResponse};
pub use gpack::GpackManifest;
pub use graph::Graph;
pub use lockfile::{LockEntry, Lockfile, LockfileManager};
pub use merge::{
    ConflictType, MergeConflict, MergeResult, MergeStrategy, RegionAwareMerger, RegionUtils,
    ThreeWayMerger,
};
pub use pipeline::{Pipeline, PipelineBuilder};
pub use pqc::{calculate_sha256, calculate_sha256_file, PqcSigner, PqcVerifier};
pub use rdf::{
    GgenOntology, TemplateMetadata, TemplateMetadataStore, TemplateRelationship, TemplateVariable,
    ValidationReport, ValidationResult, Validator, GGEN_NAMESPACE,
};
pub use registry::{RegistryClient, RegistryIndex, ResolvedPack, SearchResult};
pub use resolver::{TemplateResolver, TemplateSearchResult, TemplateSource};
pub use snapshot::{
    FileSnapshot, GraphSnapshot, Region, RegionType, Snapshot, SnapshotManager, TemplateSnapshot,
};
pub use template::Template;

// Re-export template-to-file-tree generation types
pub use templates::{
    generate_file_tree, FileTreeGenerator, FileTreeNode, FileTreeTemplate, GenerationResult,
    NodeType, TemplateContext, TemplateFormat, TemplateParser,
};
