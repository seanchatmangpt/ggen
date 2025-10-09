//! Core graph-aware code generation engine
//!
//! This crate provides the core functionality for RDF-based code generation,
//! including template processing, RDF handling, and deterministic output generation.

pub mod cache;
pub mod config;
#[cfg(test)]
pub mod e2e_tests;
pub mod generator;
pub mod github;
pub mod graph;
pub mod inject;
pub mod lockfile;
pub mod pipeline;
pub mod poc;
pub mod register;
pub mod registry;
pub mod resolver;
pub mod rpack;
pub mod template;
pub mod tera_env;
// pub mod tracing; // Temporarily disabled due to missing tracing_subscriber dependency
pub mod simple_tracing;

// Re-export commonly used types for convenience
pub use cache::{CacheManager, CachedPack};
pub use generator::{GenContext, Generator};
pub use github::{GitHubClient, PagesConfig, RepoInfo, WorkflowRun, WorkflowRunsResponse};
pub use graph::Graph;
pub use lockfile::{LockEntry, Lockfile, LockfileManager};
pub use pipeline::{Pipeline, PipelineBuilder};
pub use registry::{RegistryClient, RegistryIndex, ResolvedPack, SearchResult};
pub use resolver::{TemplateResolver, TemplateSearchResult, TemplateSource};
pub use rpack::RpackManifest;
pub use template::Template;
