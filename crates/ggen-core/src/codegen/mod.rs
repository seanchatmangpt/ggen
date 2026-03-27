//! Code generation module
//!
//! This module provides generators for different target languages and frameworks,
//! converting OntologySchema into working code for TypeScript, GraphQL, SQL, and more.
//!
//! ## Unified Dispatch
//!
//! Use [`generate_service`] for a single entry point across all supported languages:
//!
//! ```rust,no_run
//! use ggen_core::codegen::{GeneratorLanguage, generate_service};
//!
//! let code = generate_service(GeneratorLanguage::Go, "UserService", 8080).unwrap();
//! assert!(code.contains("UserService"));
//! ```

pub mod audit;
pub mod canonicalize; // μ₄: Code canonicalization for A2A-RS integration
pub mod code_graph;
pub mod concurrent;
pub mod dependency_validation;
pub mod docker_kubernetes; // Docker & Kubernetes manifest generation
pub mod elixir; // Elixir GenServer microservice generation
pub mod execution_lifecycle;
pub mod execution_proof;
pub mod executor;
pub mod go; // Go microservice code generation
pub mod incremental; // Phase 4: Incremental generation with SHA256 JIT
pub mod incremental_cache;
#[allow(dead_code)]
pub mod lifecycle_hooks;
pub mod marketplace_integration;
pub mod merge;
pub mod pipeline;
pub mod python; // Python microservice code generation (FastAPI / Pydantic v2 / SQLAlchemy 2.0)
pub mod proof_archive;
pub mod swarm_execution;
pub mod swarm_executor_bridge;
pub mod terraform; // Terraform IaC generation
pub mod transaction; // Atomic file operations with rollback
pub mod typescript;
pub mod ux; // UX utilities: progress indicators and formatting
pub mod watch;
pub mod watch_cache_integration;
pub mod watch_mode;
pub mod watch_mode_enhanced; // Enhanced watch mode with cache and graceful shutdown

// Re-export key types
pub use audit::{AuditOutput, AuditStep, AuditTrail, AuditTrailBuilder};
pub use canonicalize::{
    canonicalize, canonicalize_a2a, canonicalize_file, canonicalize_rust_quick, CanonicalizeOptions,
};
pub use code_graph::{
    CodeEnum, CodeField, CodeGraphBuilder, CodeImpl, CodeImport, CodeItem, CodeMethod, CodeModule,
    CodeParam, CodeStruct, CodeTrait, CodeVariant,
};
pub use concurrent::ConcurrentRuleExecutor;
pub use dependency_validation::{DependencyCheck, DependencyValidationReport, DependencyValidator};
pub use docker_kubernetes::{
    DockerKubernetesGenerator, Language, ServiceSpec,
};
pub use elixir::ElixirGenerator;
pub use execution_lifecycle::{ExecutionLifecycle, PostSyncContext, PreSyncContext};
pub use execution_proof::{ExecutionProof, ProofCarrier, RuleExecution};
pub use executor::{
    OutputFormat, SyncExecutor, SyncOptions, SyncResult, SyncedFileInfo, ValidationCheck,
};
pub use go::GoCodeGenerator;
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
pub use python::{Endpoint as PythonEndpoint, Field as PythonField, PythonGenerator};
pub use terraform::TerraformGenerator;
pub use transaction::{FileTransaction, TransactionReceipt};
pub use typescript::TypeScriptGenerator;
pub use watch_mode_enhanced::EnhancedWatchMode;

/// Target language for the unified [`generate_service`] dispatch.
///
/// This enum covers all first-class generators in ggen. For Docker/Kubernetes manifests,
/// use [`docker_kubernetes::DockerKubernetesGenerator`] directly.
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum GeneratorLanguage {
    /// Go microservice (struct + HTTP handler skeleton)
    Go,
    /// Elixir GenServer module
    Elixir,
    /// Rust crate skeleton
    Rust,
    /// TypeScript/Node service (uses Docker builder image as hint)
    TypeScript,
    /// Python FastAPI service
    Python,
    /// Terraform AWS provider block
    Terraform,
}

/// Error returned by [`generate_service`].
#[derive(Debug)]
pub struct CodeGenError(pub String);

impl std::fmt::Display for CodeGenError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "CodeGenError: {}", self.0)
    }
}

impl std::error::Error for CodeGenError {}

/// Unified language dispatch: generate a service skeleton for `language`.
///
/// # Arguments
/// * `language` – Target [`GeneratorLanguage`]
/// * `service_name` – Name of the service (e.g. `"UserService"`)
/// * `port` – Default HTTP port the service should listen on
///
/// # Returns
/// `Ok(String)` containing the generated source text, or `Err(CodeGenError)`.
///
/// # Example
/// ```
/// use ggen_core::codegen::{GeneratorLanguage, generate_service};
///
/// let go_code = generate_service(GeneratorLanguage::Go, "OrderService", 8080).unwrap();
/// assert!(go_code.contains("OrderService"));
///
/// let tf_code = generate_service(GeneratorLanguage::Terraform, "infra", 0).unwrap();
/// assert!(tf_code.contains("hashicorp/aws"));
/// ```
pub fn generate_service(
    language: GeneratorLanguage,
    service_name: &str,
    port: u16,
) -> Result<String, CodeGenError> {
    match language {
        GeneratorLanguage::Go => {
            GoCodeGenerator::generate_service_struct(service_name, &[])
                .map_err(CodeGenError)
        }
        GeneratorLanguage::Elixir => {
            let spec = elixir::ServiceSpec {
                name: service_name.to_string(),
                module_name: service_name.to_string(),
                description: Some(format!("{} microservice (port {})", service_name, port)),
                supervisor: None,
                config: None,
            };
            ElixirGenerator::generate_module(&spec).map_err(CodeGenError)
        }
        GeneratorLanguage::Rust => {
            // Emit a minimal Cargo-style crate skeleton
            Ok(format!(
                "// Generated by ggen — Rust service: {name}\n//\n// Default port: {port}\n\npub struct {name};\n\nimpl {name} {{\n    pub fn port() -> u16 {{ {port} }}\n}}\n",
                name = service_name,
                port = port,
            ))
        }
        GeneratorLanguage::TypeScript => {
            // Emit a minimal TypeScript module stub
            Ok(format!(
                "// Generated by ggen — TypeScript service: {name}\n\
                 // Default port: {port}\n\
                 \n\
                 export const SERVICE_NAME = \"{name}\";\n\
                 export const SERVICE_PORT = {port};\n",
                name = service_name,
                port = port,
            ))
        }
        GeneratorLanguage::Python => {
            PythonGenerator::generate_main(service_name, port).map_err(CodeGenError)
        }
        GeneratorLanguage::Terraform => {
            TerraformGenerator::generate_provider_block().map_err(CodeGenError)
        }
    }
}
