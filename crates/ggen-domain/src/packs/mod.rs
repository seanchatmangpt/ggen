//! Packs Domain - Curated Package Collections
//!
//! This module provides domain logic for managing and using packs,
//! which are curated collections of packages for specific workflows.
//!
//! ## Architecture
//! - Packs are defined in marketplace/packs/*.toml
//! - Each pack contains metadata, packages list, and composition rules
//! - Packs support multi-pack composition for complex projects
//! - Integration with marketplace for package resolution and installation

pub mod compose;
pub mod generator;
pub mod install;
pub mod metadata;
pub mod score;
pub mod types;
pub mod validate;

// Phase 2-3 modules
pub mod advanced_resolver;
pub mod cloud_distribution;
pub mod dependency_graph;
pub mod installer;
pub mod registry;
pub mod repository;
pub mod sparql_executor;
pub mod template_generator;

// Re-export core types
pub use compose::{compose_packs, ComposePacksInput, ComposePacksOutput};
pub use generator::{generate_from_pack, GenerateInput, GenerateOutput};
pub use install::{install_pack, InstallInput, InstallOutput};
pub use metadata::{list_packs, load_pack_metadata, show_pack};
pub use score::{score_pack, PackScore};
pub use types::{CompositionStrategy, Pack, PackDependency, PackTemplate};
pub use validate::{validate_pack, ValidationResult};

// Re-export Phase 2-3 types
pub use advanced_resolver::{AdvancedResolver, ConflictResolution, ResolvedDependencies};
pub use cloud_distribution::{CacheInfo, CacheStats, CloudDistribution, InMemoryCDN};
pub use dependency_graph::DependencyGraph;
pub use installer::{InstallOptions, InstallReport, PackInstaller};
pub use registry::{InMemoryRegistry, PackRegistry, PublishMetadata, PublishReceipt, SearchQuery};
pub use repository::{FileSystemRepository, PackRepository};
pub use sparql_executor::{SparqlExecutor, SparqlResult};
pub use template_generator::{GenerationReport, TemplateGenerator};
