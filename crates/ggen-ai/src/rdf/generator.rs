//! Main CLI generator for RDF-to-CLI generation pipeline
//!
//! This module provides the main entry point for generating CLI projects from RDF
//! ontologies. It implements 2026 best practices including workspace structure with
//! separate CLI and domain crates, domain function references for stable contracts,
//! and clap-noun-verb v3.3.0 integration.
//!
//! ## Generation Pipeline
//!
//! The generator orchestrates a complete pipeline:
//! 1. Parse RDF (load schema + user TTL)
//! 2. Execute SPARQL queries to extract project structure
//! 3. Validate the extracted project data
//! 4. Generate workspace structure with CLI and domain crates
//! 5. Run post-generation hooks (cargo fmt, cargo check)
//!
//! ## Features
//!
//! - **Workspace Structure**: Separate CLI and domain crates
//! - **Domain Function References**: Stable contracts between CLI and domain
//! - **clap-noun-verb Integration**: Modern CLI argument parsing
//! - **Hyper-Advanced DX**: Live preview, enhanced errors, IDE hints
//!
//! ## Examples
//!
//! ### Generating a CLI Project
//!
//! ```rust,no_run
//! use ggen_ai::rdf::CliGenerator;
//! use std::path::Path;
//!
//! # fn main() -> anyhow::Result<()> {
//! let generator = CliGenerator::new("templates".into());
//! generator.generate_from_ttl(
//!     Path::new("project.ttl"),
//!     Path::new("output")
//! )?;
//! # Ok(())
//! # }
//! ```

use ggen_utils::error::{bail, Context, Result};
use std::path::{Path, PathBuf};

use crate::rdf::{QueryExecutor, RdfParser};

/// Main entry point for RDF-to-CLI generation
///
/// This generator uses the new 2026 best practices:
/// - Workspace structure (crates/{cli-crate}, crates/{domain-crate})
/// - clap-noun-verb v3.3.0 with domain function references
/// - Separation of CLI and domain logic
pub struct CliGenerator {
    template_dir: PathBuf,
}

impl CliGenerator {
    /// Create a new CLI generator with the specified template directory
    pub fn new(template_dir: PathBuf) -> Self {
        Self { template_dir }
    }

    /// Generate a complete CLI project from a TTL file using 2026 best practices
    ///
    /// This is the main pipeline that orchestrates all phases:
    /// 1. Parse RDF (load schema + user TTL)
    /// 2. Execute SPARQL queries to extract project structure
    /// 3. Validate the extracted project data
    /// 4. Generate workspace structure with CLI and domain crates
    /// 5. Run post-generation hooks (cargo fmt, cargo check)
    ///
    /// # Hyper-Advanced DX Features
    /// - Live template preview before generation
    /// - Enhanced error messages with fix suggestions
    /// - IDE-friendly code hints
    /// - Progressive disclosure (beginner → advanced)
    pub fn generate_from_ttl(&self, ttl_path: &Path, output_dir: &Path) -> Result<()> {
        use ggen_core::cli_generator::dx::{
            ErrorContext, ErrorEnhancer, ProgressiveDisclosure, TemplatePreview,
        };

        ggen_utils::alert_info!(
            "🚀 Generating CLI project from {} (2026 best practices + Hyper DX)",
            ttl_path.display()
        );

        // Step 1: Parse RDF
        ggen_utils::alert_info!("  [1/6] Parsing RDF...");
        let mut parser = RdfParser::new()?;
        parser.load_schema()?;
        parser.load_ttl(ttl_path)?;

        // Step 2: Execute SPARQL queries
        ggen_utils::alert_info!("  [2/6] Extracting project structure...");
        let executor = QueryExecutor::new(parser.get_store());
        let mut project = executor.extract_project()?;
        project.nouns = executor.extract_nouns()?;
        project.dependencies = executor.extract_dependencies()?;

        // Ensure default crate names if not specified
        if project.cli_crate.is_none() {
            project.cli_crate = Some(format!("{}-cli", project.name));
        }
        if project.domain_crate.is_none() {
            project.domain_crate = Some(format!("{}-core", project.name));
        }

        // Step 3: Validate project
        ggen_utils::alert_info!("  [3/7] Validating project...");
        if let Err(e) = validate_project(&project) {
            let enhanced = ErrorEnhancer::enhance_error(&e, &ErrorContext::WorkspaceStructure);
            ggen_utils::alert_critical!("{}", &enhanced);
            return Err(e);
        }

        // Step 3.5: Show live preview (Hyper DX)
        ggen_utils::alert_info!("  [4/7] 📋 Live Preview...");
        let cli_project = convert_project(&project)?;
        ggen_utils::alert_info!(
            "{}",
            TemplatePreview::preview_workspace_structure(&cli_project)
        );

        // Show beginner-friendly info
        ggen_utils::alert_info!("\n{}", ProgressiveDisclosure::beginner_info(&cli_project));

        // Step 4: Convert to ggen-core types and generate workspace
        ggen_utils::alert_info!("\n  [5/7] Generating workspace structure...");
        std::fs::create_dir_all(output_dir)?;
        if let Err(e) = self.generate_workspace(&project, output_dir) {
            let enhanced = ErrorEnhancer::enhance_error(&e, &ErrorContext::TemplateGeneration);
            ggen_utils::alert_critical!("{}", &enhanced);
            return Err(e);
        }

        // Step 5: Post-generation
        ggen_utils::alert_info!("  [6/7] Running post-generation hooks...");
        run_post_generation(output_dir)?;

        // Step 6: Show completion summary with advanced info
        ggen_utils::alert_info!("  [7/7] ✅ Generation Complete!\n");
        ggen_utils::alert_success!("CLI project generated at {}", output_dir.display());
        let cli_crate = project.cli_crate.as_ref().ok_or_else(|| {
            ggen_utils::error::Error::new("CLI crate name is required but was not provided")
        })?;
        let domain_crate = project.domain_crate.as_ref().ok_or_else(|| {
            ggen_utils::error::Error::new("Domain crate name is required but was not provided")
        })?;
        ggen_utils::alert_info!(
            "📁 Workspace: crates/{}, crates/{}",
            cli_crate,
            domain_crate
        );

        // Show advanced info for power users
        ggen_utils::alert_info!("\n{}", ProgressiveDisclosure::advanced_info(&cli_project));

        // Show next steps
        ggen_utils::alert_info!("\n🎯 Next Steps:");
        ggen_utils::alert_info!("  1. cd {}", project.name);
        ggen_utils::alert_info!("  2. cargo build");
        ggen_utils::alert_info!("  3. cargo run -- --help");
        ggen_utils::alert_info!(
            "  4. cargo run -- {} --help",
            project
                .nouns
                .first()
                .map(|n| n.name.as_str())
                .unwrap_or("noun")
        );

        Ok(())
    }

    /// Generate workspace structure using ggen-core CLI generator
    fn generate_workspace(
        &self, project: &crate::rdf::types::CliProject, output_dir: &Path,
    ) -> Result<()> {
        use ggen_core::cli_generator::{
            CliLayerGenerator, DomainLayerGenerator, WorkspaceGenerator,
        };

        // Convert project to ggen-core types
        let cli_project = convert_project(project)?;

        // Generate workspace
        let workspace_gen = WorkspaceGenerator::new(&self.template_dir)?;
        workspace_gen
            .generate(&cli_project, output_dir)
            .context("Failed to generate workspace structure")?;

        // Generate CLI layer
        let cli_gen = CliLayerGenerator::new(&self.template_dir)?;
        cli_gen
            .generate(&cli_project, output_dir)
            .context("Failed to generate CLI layer")?;

        // Generate domain layer
        let domain_gen = DomainLayerGenerator::new(&self.template_dir)?;
        domain_gen
            .generate(&cli_project, output_dir)
            .context("Failed to generate domain layer")?;

        Ok(())
    }
}

/// Convert ggen-ai::rdf::types::CliProject to ggen-core::cli_generator::types::CliProject
fn convert_project(
    project: &crate::rdf::types::CliProject,
) -> Result<ggen_core::cli_generator::types::CliProject> {
    use ggen_core::cli_generator::types::{
        Argument, ArgumentType, CliProject, Dependency, Noun, Validation, Verb,
    };

    let nouns: Vec<Noun> = project
        .nouns
        .iter()
        .map(|n| {
            let verbs: Vec<Verb> = n
                .verbs
                .iter()
                .map(|v| {
                    let arguments: Vec<Argument> = v
                        .arguments
                        .iter()
                        .map(|a| Argument {
                            name: a.name.clone(),
                            long: a.long.clone(),
                            short: a.short,
                            help: a.help.clone(),
                            required: a.required,
                            default: a.default.clone(),
                            value_name: a.value_name.clone(),
                            position: a.position,
                            arg_type: ArgumentType {
                                name: a.arg_type.name.clone(),
                                parser: a.arg_type.parser.clone(),
                            },
                        })
                        .collect();

                    let validations: Vec<Validation> = v
                        .validations
                        .iter()
                        .map(|val| Validation {
                            rule: val.rule.clone(),
                            pattern: val.pattern.clone(),
                            message: val.message.clone(),
                            arg_name: val.arg_name.clone(),
                        })
                        .collect();

                    Verb {
                        name: v.name.clone(),
                        description: v.description.clone(),
                        alias: v.alias.clone(),
                        arguments,
                        validations,
                        execution_logic: v.execution_logic.clone(),
                        domain_function: v.domain_function.clone(),
                        domain_module: v.domain_module.clone(),
                    }
                })
                .collect();

            Noun {
                name: n.name.clone(),
                description: n.description.clone(),
                module_path: n.module_path.clone(),
                verbs,
            }
        })
        .collect();

    let dependencies: Vec<Dependency> = project
        .dependencies
        .iter()
        .map(|d| Dependency {
            name: d.name.clone(),
            version: d.version.clone(),
            features: d.features.clone(),
            optional: d.optional,
        })
        .collect();

    Ok(CliProject {
        name: project.name.clone(),
        version: project.version.clone(),
        description: project.description.clone(),
        authors: project.authors.clone(),
        edition: project.edition.clone(),
        license: project.license.clone(),
        nouns,
        dependencies,
        cli_crate: project.cli_crate.clone(),
        domain_crate: project.domain_crate.clone(),
        resolver: project.resolver.clone(),
    })
}

/// Validate the extracted project structure
///
/// Ensures:
/// - At least one noun exists
/// - Each noun has at least one verb
fn validate_project(project: &crate::rdf::types::CliProject) -> Result<()> {
    if project.nouns.is_empty() {
        bail!("Project must have at least one noun");
    }

    for noun in &project.nouns {
        if noun.verbs.is_empty() {
            bail!("Noun '{}' must have at least one verb", noun.name);
        }
    }

    Ok(())
}

/// Run post-generation hooks
///
/// Executes:
/// - cargo fmt: Format generated code
/// - cargo check: Verify project compiles
fn run_post_generation(output_dir: &Path) -> Result<()> {
    use std::process::Command;

    // Format code
    let fmt_status = Command::new("cargo")
        .args(["fmt"])
        .current_dir(output_dir)
        .status()?;

    if !fmt_status.success() {
        bail!("cargo fmt failed");
    }

    // Check compilation
    let check_status = Command::new("cargo")
        .args(["check"])
        .current_dir(output_dir)
        .status()?;

    if !check_status.success() {
        bail!("cargo check failed");
    }

    Ok(())
}
