//! PaaS submodule command handler
//!
//! Implements semantic noun-verb CLI using clap-noun-verb architecture.
//! Supports 8 verbs across 4 noun types for submodule management.
//!
//! Architecture:
//! - Verbs: init, update, validate, sync, deploy, status, logs, describe, explain
//! - Nouns: submodule, artifact, specification, environment
//! - Routing: PaasCommand dispatches noun-verb pairs to handlers

pub mod errors;
pub mod handlers;
pub mod validators;

use clap::{Args, Subcommand};
use errors::Result;
use std::path::PathBuf;

/// PaaS submodule management command
///
/// Manages ggen-spec-kit and clap-noun-verb submodules with semantic noun-verb interface.
#[derive(Debug, Args)]
pub struct PaasCommand {
    /// Verbose output
    #[arg(short, long, global = true)]
    pub verbose: bool,

    /// Specification directory (default: .specify)
    #[arg(long, global = true, env = "GGEN_SPEC_DIR")]
    pub spec_dir: Option<PathBuf>,

    /// Output directory (default: ./generated)
    #[arg(long, global = true, env = "GGEN_OUTPUT_DIR")]
    pub output_dir: Option<PathBuf>,

    #[command(subcommand)]
    pub action: PaasAction,
}

/// PaaS subcommands (noun-verb semantic routing)
#[derive(Debug, Subcommand)]
pub enum PaasAction {
    /// Initialize submodule
    Init {
        /// Submodule name (ggen-spec-kit, clap-noun-verb)
        #[arg(value_name = "NAME")]
        name: String,

        /// Initialize recursively with dependencies
        #[arg(long)]
        recursive: bool,

        /// Use shallow clone for large submodules
        #[arg(long)]
        shallow: bool,
    },

    /// Update submodule(s) to latest
    Update {
        /// Submodule name (optional, updates all if not specified)
        #[arg(value_name = "NAME")]
        name: Option<String>,

        /// Update recursively
        #[arg(long)]
        recursive: bool,

        /// Checkout specific ref/tag
        #[arg(long)]
        checkout: Option<String>,
    },

    /// Validate specification closure
    Validate {
        /// Specification file or directory to validate
        #[arg(long, value_name = "PATH")]
        spec: Option<PathBuf>,

        /// Require closure ≥ this percentage
        #[arg(long, default_value = "95")]
        min_closure: f32,

        /// Strict mode: fail on warnings
        #[arg(long)]
        strict: bool,
    },

    /// Synchronize specifications with generated code
    Sync {
        /// Source specification directory
        #[arg(long, value_name = "PATH")]
        source: Option<PathBuf>,

        /// Target code directory
        #[arg(long, value_name = "PATH")]
        target: Option<PathBuf>,

        /// Dry run: show changes without applying
        #[arg(long)]
        dry_run: bool,
    },

    /// Deploy artifacts to environment
    Deploy {
        /// Target environment (development, staging, production)
        #[arg(long, value_name = "ENV")]
        environment: String,

        /// Deployment target (docker, kubernetes, terraform)
        #[arg(long, value_name = "TARGET")]
        target: Option<String>,

        /// Dry run: validate without deploying
        #[arg(long)]
        dry_run: bool,

        /// Force deployment (skip safety checks)
        #[arg(long)]
        force: bool,
    },

    /// Show deployment status
    Status {
        /// Show status for specific environment
        #[arg(long, value_name = "ENV")]
        environment: Option<String>,

        /// Show detailed status
        #[arg(short, long)]
        detailed: bool,
    },

    /// Stream operation logs
    Logs {
        /// Number of lines to show
        #[arg(short, long, default_value = "50")]
        lines: usize,

        /// Filter by deployment/environment
        #[arg(long, value_name = "NAME")]
        deployment: Option<String>,

        /// Follow (stream) logs
        #[arg(short, long)]
        follow: bool,

        /// Filter by log level (error, warn, info, debug)
        #[arg(long)]
        level: Option<String>,
    },

    /// Describe artifact or specification
    Describe {
        /// Artifact or resource name to describe
        #[arg(value_name = "NAME")]
        name: String,

        /// Show full details
        #[arg(long)]
        detailed: bool,

        /// Output format (table, json, yaml)
        #[arg(long, default_value = "table")]
        format: String,
    },

    /// Explain generated artifact origin
    Explain {
        /// Artifact or code to explain
        #[arg(value_name = "PATH")]
        path: PathBuf,

        /// Include RDF triples that generated this artifact
        #[arg(long)]
        show_spec: bool,

        /// Include transformation pipeline details
        #[arg(long)]
        show_pipeline: bool,
    },
}

impl PaasCommand {
    /// Execute PaaS command with noun-verb routing
    pub async fn execute(self) -> Result<()> {
        // Validate specification closure before any operation
        if let Some(spec_dir) = self.spec_dir.as_ref() {
            if let Some(spec_str) = spec_dir.to_str() {
                validators::validate_closure(spec_str, 95.0).await?;
            }
        }

        // Route noun-verb pair to appropriate handler
        match self.action {
            PaasAction::Init {
                name,
                recursive,
                shallow,
            } => {
                handlers::init::init_submodule(&name, recursive, shallow).await?;
            }
            PaasAction::Update {
                name,
                recursive,
                checkout,
            } => {
                handlers::update::update_submodule(name.as_deref(), recursive, checkout.as_deref())
                    .await?;
            }
            PaasAction::Validate {
                spec,
                min_closure,
                strict,
            } => {
                let spec_path = spec
                    .as_ref()
                    .and_then(|p| p.to_str())
                    .unwrap_or(".specify");
                handlers::validate::validate_specs(spec_path, min_closure, strict).await?;
            }
            PaasAction::Sync {
                source,
                target,
                dry_run,
            } => {
                let src = source
                    .as_ref()
                    .and_then(|p| p.to_str())
                    .unwrap_or(".specify");
                let tgt = target
                    .as_ref()
                    .and_then(|p| p.to_str())
                    .unwrap_or("./generated");
                handlers::sync::sync_specs(src, tgt, dry_run).await?;
            }
            PaasAction::Deploy {
                environment,
                target,
                dry_run,
                force,
            } => {
                handlers::deploy::deploy_artifacts(&environment, target.as_deref(), dry_run, force)
                    .await?;
            }
            PaasAction::Status {
                environment,
                detailed,
            } => {
                handlers::status::show_status(environment.as_deref(), detailed).await?;
            }
            PaasAction::Logs {
                lines,
                deployment,
                follow,
                level,
            } => {
                handlers::logs::stream_logs(
                    lines,
                    deployment.as_deref(),
                    follow,
                    level.as_deref(),
                )
                .await?;
            }
            PaasAction::Describe { name, detailed, format } => {
                handlers::describe::describe_resource(&name, detailed, &format).await?;
            }
            PaasAction::Explain {
                path,
                show_spec,
                show_pipeline,
            } => {
                handlers::explain::explain_artifact(&path, show_spec, show_pipeline).await?;
            }
        }

        Ok(())
    }
}
