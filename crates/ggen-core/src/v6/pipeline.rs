//! Staged Pipeline Orchestrator
//!
//! Orchestrates the complete v6 projection pipeline: A = μ(O)
//!
//! The pipeline runs passes in order:
//! 0. μ₀: Pack Resolution (bundle expansion, dependency resolution, ontology merge)
//! 1. μ₁: Normalization (CONSTRUCT)
//! 2. μ₂: Extraction (SELECT)
//! 3. μ₃: Emission (Tera)
//! 4. μ₄: Canonicalization (formatting)
//! 5. μ₅: Receipt (provenance)

use crate::graph::{Graph, GraphExport};
use crate::pack_resolver::{PackResolver, ResolvedPacks};
use crate::v6::epoch::Epoch;
use crate::v6::guard::GuardSet;
use crate::v6::pass::{Pass, PassContext, PassExecution, PassResult};
use crate::v6::passes::{
    CanonicalizationPass, EmissionPass, ExtractionPass, NormalizationPass, ReceiptGenerationPass,
};
use crate::v6::receipt::{
    BuildReceipt, BundleExpansionRef, OutputFile, PackProvenance, ReceiptPolicies,
};
use crate::v6::vocabulary::VocabularyRegistry;
use ggen_marketplace::trust::TrustTier;
use ggen_utils::error::{Error, Result};
use oxigraph::io::RdfFormat;
use serde::{Deserialize, Serialize};
use sha2::{Digest, Sha256};
use std::path::{Path, PathBuf};
use std::time::Instant;

/// Stage pack templates into `<project>/.ggen/pack-stage/` for μ₃ rules or inspection.
#[allow(dead_code)]
fn stage_pack_templates(
    base_path: &Path, templates: &[crate::pack_resolver::TemplateDef],
) -> Result<Vec<String>> {
    let stage = base_path.join(".ggen").join("pack-stage");
    let mut names = Vec::new();
    for t in templates {
        let dest = stage.join(&t.path);
        if let Some(p) = dest.parent() {
            std::fs::create_dir_all(p)
                .map_err(|e| Error::new(&format!("Failed to create pack-stage dir: {}", e)))?;
        }
        std::fs::write(&dest, &t.content)
            .map_err(|e| Error::new(&format!("Failed to write staged template: {}", e)))?;
        names.push(t.path.display().to_string());
    }
    Ok(names)
}

/// Verification mode for the pipeline
#[derive(Debug, Clone, Copy, PartialEq, Eq, Serialize, Deserialize, Default)]
pub enum VerifyMode {
    /// Don't verify, just generate
    #[default]
    None,
    /// Verify inputs match epoch before running
    VerifyInputs,
    /// Verify outputs match receipt after running
    VerifyOutputs,
    /// Verify both inputs and outputs
    Full,
}

/// Pipeline configuration
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct PipelineConfig {
    /// Project name
    pub project_name: String,

    /// Project version
    pub project_version: String,

    /// Base path for the project
    pub base_path: PathBuf,

    /// Ontology source files
    pub ontology_sources: Vec<PathBuf>,

    /// Output directory for generated files
    pub output_dir: PathBuf,

    /// Path to write receipt
    pub receipt_path: Option<PathBuf>,

    /// Verification mode
    pub verify_mode: VerifyMode,

    /// Previous receipt for verification
    pub previous_receipt: Option<PathBuf>,

    /// ggen version
    pub toolchain_version: String,

    /// Enable μ₀ pack resolution stage
    #[serde(default)]
    pub enable_pack_resolution: bool,
}

impl PipelineConfig {
    /// Create a new pipeline configuration
    pub fn new(project_name: impl Into<String>, project_version: impl Into<String>) -> Self {
        Self {
            project_name: project_name.into(),
            project_version: project_version.into(),
            base_path: PathBuf::from("."),
            ontology_sources: Vec::new(),
            output_dir: PathBuf::from("."),
            receipt_path: Some(PathBuf::from(".ggen/receipt.json")),
            verify_mode: VerifyMode::None,
            previous_receipt: None,
            toolchain_version: env!("CARGO_PKG_VERSION").to_string(),
            enable_pack_resolution: true,
        }
    }

    /// Set base path
    pub fn with_base_path(mut self, path: impl Into<PathBuf>) -> Self {
        self.base_path = path.into();
        self
    }

    /// Add an ontology source
    pub fn with_ontology(mut self, path: impl Into<PathBuf>) -> Self {
        self.ontology_sources.push(path.into());
        self
    }

    /// Add multiple ontology sources
    pub fn with_ontologies(mut self, paths: Vec<PathBuf>) -> Self {
        self.ontology_sources.extend(paths);
        self
    }

    /// Set output directory
    pub fn with_output_dir(mut self, path: impl Into<PathBuf>) -> Self {
        self.output_dir = path.into();
        self
    }

    /// Set receipt path
    pub fn with_receipt_path(mut self, path: impl Into<PathBuf>) -> Self {
        self.receipt_path = Some(path.into());
        self
    }

    /// Set verification mode
    pub fn with_verify_mode(mut self, mode: VerifyMode) -> Self {
        self.verify_mode = mode;
        self
    }

    /// Set previous receipt for verification
    pub fn with_previous_receipt(mut self, path: impl Into<PathBuf>) -> Self {
        self.previous_receipt = Some(path.into());
        self
    }
}

/// The staged compilation pipeline
pub struct StagedPipeline {
    /// Configuration
    config: PipelineConfig,

    /// Loaded RDF graph
    graph: Graph,

    /// Input epoch
    epoch: Option<Epoch>,

    /// Executed passes
    executed_passes: Vec<PassExecution>,

    /// Generated files
    generated_files: Vec<PathBuf>,

    /// Vocabulary registry for governance
    vocabulary_registry: VocabularyRegistry,

    /// Guard set for output validation
    guard_set: GuardSet,

    /// μ₀: Pack resolver
    pack_resolver: Option<PackResolver>,

    /// μ₀: Resolved packs (after bundle expansion and dependency resolution)
    resolved_packs: Option<ResolvedPacks>,

    /// Normalization pass
    normalization: NormalizationPass,

    /// Extraction pass
    extraction: ExtractionPass,

    /// Emission pass
    emission: EmissionPass,

    /// Canonicalization pass
    canonicalization: CanonicalizationPass,

    /// Receipt generation pass
    #[allow(dead_code)]
    receipt_gen: ReceiptGenerationPass,
}

impl StagedPipeline {
    /// Create a new staged pipeline
    pub fn new(config: PipelineConfig) -> Result<Self> {
        let graph = Graph::new()?;

        // Initialize pack resolver if lockfile exists
        let pack_resolver = PackResolver::new(&config.base_path).ok();

        Ok(Self {
            config: config.clone(),
            graph,
            epoch: None,
            executed_passes: Vec::new(),
            generated_files: Vec::new(),
            vocabulary_registry: VocabularyRegistry::with_standard_vocabularies(),
            guard_set: GuardSet::default_v6(),
            pack_resolver,
            resolved_packs: None,
            normalization: NormalizationPass::with_standard_rules(),
            extraction: ExtractionPass::with_standard_rules(),
            emission: EmissionPass::new(),
            canonicalization: CanonicalizationPass::new(),
            receipt_gen: ReceiptGenerationPass::new(&config.toolchain_version)
                .with_receipt_path(config.receipt_path.clone().unwrap_or_default()),
        })
    }

    /// Set custom normalization pass
    pub fn with_normalization(mut self, pass: NormalizationPass) -> Self {
        self.normalization = pass;
        self
    }

    /// Set custom extraction pass
    pub fn with_extraction(mut self, pass: ExtractionPass) -> Self {
        self.extraction = pass;
        self
    }

    /// Set custom emission pass
    pub fn with_emission(mut self, pass: EmissionPass) -> Self {
        self.emission = pass;
        self
    }

    /// Set vocabulary registry
    pub fn with_vocabulary_registry(mut self, registry: VocabularyRegistry) -> Self {
        self.vocabulary_registry = registry;
        self
    }

    /// Set guard set
    pub fn with_guards(mut self, guards: GuardSet) -> Self {
        self.guard_set = guards;
        self
    }

    /// Load ontology sources and create epoch
    pub fn load_ontologies(&mut self) -> Result<&Epoch> {
        // Create epoch from ontology sources
        self.epoch = Some(Epoch::create(
            &self.config.base_path,
            &self.config.ontology_sources,
        )?);

        // Load each ontology file into the graph
        for source in &self.config.ontology_sources {
            let full_path = self.config.base_path.join(source);
            let content = std::fs::read_to_string(&full_path).map_err(|e| {
                Error::new(&format!(
                    "Failed to read ontology '{}': {}",
                    full_path.display(),
                    e
                ))
            })?;

            // Validate vocabulary governance
            let namespaces = VocabularyRegistry::extract_namespaces(&content);
            self.vocabulary_registry.validate_namespaces(&namespaces)?;

            // Load into graph
            self.graph.insert_turtle(&content)?;
        }

        Ok(self.epoch.as_ref().unwrap())
    }

    /// Get resolved packs from μ₀ stage
    pub fn resolved_packs(&self) -> Option<&ResolvedPacks> {
        self.resolved_packs.as_ref()
    }

    /// Verify inputs match a previous epoch
    pub fn verify_inputs(&self, previous_epoch: &Epoch) -> Result<bool> {
        if let Some(ref epoch) = self.epoch {
            Ok(epoch.id == previous_epoch.id)
        } else {
            Err(Error::new("No epoch loaded. Call load_ontologies() first."))
        }
    }

    /// Run a single pass and record execution
    #[allow(dead_code)]
    fn run_pass<P: Pass>(&mut self, pass: &P, ctx: &mut PassContext<'_>) -> Result<PassResult> {
        let start = Instant::now();
        let result = pass.execute(ctx)?;
        let duration = start.elapsed();

        // Record execution
        let mut execution = pass.create_execution_record(&result);
        execution.duration_ms = duration.as_millis() as u64;
        self.executed_passes.push(execution);

        Ok(result)
    }

    /// Run the complete pipeline
    ///
    /// # Returns
    /// * `Ok(BuildReceipt)` - Pipeline completed successfully
    /// * `Err(Error)` - Pipeline failed at some stage
    pub fn run(&mut self) -> Result<BuildReceipt> {
        let _pipeline_start = Instant::now();

        // μ₀: Pack Resolution (if lockfile exists)
        if let Some(ref resolver) = self.pack_resolver {
            match resolver.resolve() {
                Ok(resolved) => {
                    // Use merged ontology from resolved packs
                    self.graph = resolved.merged_ontology.clone();
                    self.resolved_packs = Some(resolved);
                }
                Err(e) if e.to_string().contains("Lockfile not found") => {
                    // No lockfile - skip pack resolution, continue with project ontologies
                    tracing::debug!("No lockfile found, skipping pack resolution");
                }
                Err(e) => {
                    // Other errors should propagate
                    return Err(e);
                }
            }
        }

        // Synthetic epoch when only pack substrate is present (no project TTL epoch yet)
        if self.epoch.is_none() && self.resolved_packs.is_some() {
            let turtle = GraphExport::new(&self.graph)
                .write_to_string(RdfFormat::Turtle)
                .map_err(|e| Error::new(&format!("Pack graph export failed: {}", e)))?;
            let digest = format!("{:x}", Sha256::digest(turtle.as_bytes()));
            let triple_count = self.graph.len();
            self.epoch = Some(Epoch::from_pack_merged_substrate(digest, triple_count));
        }

        // Load ontologies if not already loaded (skip if μ₀ provided merged ontology)
        if self.epoch.is_none() && self.resolved_packs.is_none() {
            self.load_ontologies()?;
        }

        // Verify inputs if requested
        if matches!(
            self.config.verify_mode,
            VerifyMode::VerifyInputs | VerifyMode::Full
        ) {
            if let Some(ref receipt_path) = self.config.previous_receipt {
                let previous_receipt = BuildReceipt::read_from_file(receipt_path)?;
                // Check if epoch matches
                if self.epoch.as_ref().map(|e| &e.id) != Some(&previous_receipt.epoch_id) {
                    return Err(Error::new(
                        "Input epoch does not match previous receipt. Inputs have changed.",
                    ));
                }
            }
        }

        // Create output directory
        let output_dir = self.config.base_path.join(&self.config.output_dir);
        std::fs::create_dir_all(&output_dir).map_err(|e| {
            Error::new(&format!(
                "Failed to create output directory '{}': {}",
                output_dir.display(),
                e
            ))
        })?;

        // Clone passes to avoid borrow issues
        let normalization = self.normalization.clone();
        let mut extraction = self.extraction.clone();
        let mut emission = self.emission.clone();

        // Load pack queries into μ₂ and pack templates into μ₃
        if let Some(ref rp) = self.resolved_packs {
            // Load pack queries into μ₂ (CONSTRUCT-only queries from packs)
            extraction.extend_with_pack_construct_queries(&rp.queries)?;
            // Load pack templates into μ₃ (Tera templates from packs)
            emission.extend_with_pack_templates(&rp.templates)?;
        }
        let canonicalization = self.canonicalization.clone();

        // Create pass context
        let mut ctx = PassContext::new(&self.graph, self.config.base_path.clone(), output_dir)
            .with_project(
                self.config.project_name.clone(),
                self.config.project_version.clone(),
            );

        // μ₁: Normalization (pipeline.load — load RDF ontology into normalized graph)
        let start = Instant::now();
        let span = tracing::info_span!(
            "pipeline.load",
            "operation.name" = "pipeline.load",
            "operation.type" = "pipeline",
            "pipeline.stage" = "mu1",
        );
        let _guard = span.enter();
        let norm_result = normalization.execute(&mut ctx)?;
        let mut execution = normalization.create_execution_record(&norm_result);
        execution.duration_ms = start.elapsed().as_millis() as u64;
        self.executed_passes.push(execution);
        if !norm_result.success {
            return Err(Error::new(&format!(
                "Normalization failed: {:?}",
                norm_result.error
            )));
        }
        let elapsed = start.elapsed();
        tracing::Span::current().record("pipeline.duration_ms", elapsed.as_millis() as u64);
        tracing::info!(
            stage = "mu1",
            elapsed_ms = elapsed.as_millis() as u64,
            "Pipeline stage completed"
        );
        drop(_guard);

        // μ₂: Extraction (pipeline.extract — extract skill definitions via SELECT)
        let start = Instant::now();
        let span = tracing::info_span!(
            "pipeline.extract",
            "operation.name" = "pipeline.extract",
            "operation.type" = "pipeline",
            "pipeline.stage" = "mu2",
        );
        let _guard = span.enter();
        let extract_result = extraction.execute(&mut ctx)?;
        let mut execution = extraction.create_execution_record(&extract_result);
        execution.duration_ms = start.elapsed().as_millis() as u64;
        self.executed_passes.push(execution);
        if !extract_result.success {
            return Err(Error::new(&format!(
                "Extraction failed: {:?}",
                extract_result.error
            )));
        }
        let elapsed = start.elapsed();
        tracing::Span::current().record("pipeline.duration_ms", elapsed.as_millis() as u64);
        tracing::info!(
            stage = "mu2",
            elapsed_ms = elapsed.as_millis() as u64,
            "Pipeline stage completed"
        );
        drop(_guard);

        // Stage pack templates under .ggen/pack-stage/ (μ₃ rules may reference these paths)
        if let Some(ref rp) = self.resolved_packs {
            let _staged = stage_pack_templates(&self.config.base_path, &rp.templates)?;
        }

        // μ₃: Emission (pipeline.generate — generate code via Tera templates)
        let start = Instant::now();
        let span = tracing::info_span!(
            "pipeline.generate",
            "operation.name" = "pipeline.generate",
            "operation.type" = "pipeline",
            "pipeline.stage" = "mu3",
        );
        let _guard = span.enter();

        let emission_result = emission.execute(&mut ctx)?;
        let mut execution = emission.create_execution_record(&emission_result);
        execution.duration_ms = start.elapsed().as_millis() as u64;
        self.executed_passes.push(execution);
        if !emission_result.success {
            return Err(Error::new(&format!(
                "Emission failed: {:?}",
                emission_result.error
            )));
        }
        let elapsed = start.elapsed();
        tracing::Span::current().record("pipeline.duration_ms", elapsed.as_millis() as u64);
        tracing::info!(
            stage = "mu3",
            elapsed_ms = elapsed.as_millis() as u64,
            "Pipeline stage completed"
        );
        drop(_guard);

        // μ₄: Canonicalization (pipeline.validate — quality gate validation)
        let start = Instant::now();
        let span = tracing::info_span!(
            "pipeline.validate",
            "operation.name" = "pipeline.validate",
            "operation.type" = "pipeline",
            "pipeline.stage" = "mu4",
        );
        let _guard = span.enter();
        let canon_result = canonicalization.execute(&mut ctx)?;
        let mut execution = canonicalization.create_execution_record(&canon_result);
        execution.duration_ms = start.elapsed().as_millis() as u64;
        self.executed_passes.push(execution);
        if !canon_result.success {
            return Err(Error::new(&format!(
                "Canonicalization failed: {:?}",
                canon_result.error
            )));
        }
        let elapsed = start.elapsed();
        tracing::Span::current().record("pipeline.duration_ms", elapsed.as_millis() as u64);
        tracing::info!(
            stage = "mu4",
            elapsed_ms = elapsed.as_millis() as u64,
            "Pipeline stage completed"
        );
        drop(_guard);

        // Collect generated files
        self.generated_files = ctx.generated_files.clone();

        // Create output file records
        let output_records = self.create_output_records(&ctx)?;

        // μ₅: Receipt generation (pipeline.emit — write generated files + provenance receipt)
        let start = Instant::now();
        let span = tracing::info_span!(
            "pipeline.emit",
            "operation.name" = "pipeline.emit",
            "operation.type" = "pipeline",
            "pipeline.stage" = "mu5",
        );
        let _guard = span.enter();

        let epoch = self.epoch.as_ref().unwrap();
        let mut receipt = BuildReceipt::new(
            epoch,
            self.executed_passes.clone(),
            output_records,
            &self.config.toolchain_version,
        )
        .with_policies(ReceiptPolicies {
            blank_node_policy: "canonicalize".to_string(),
            ordering_policy: "deterministic".to_string(),
            formatting_policy: "language-specific".to_string(),
            active_guards: vec!["path-guard".to_string(), "secret-guard".to_string()],
        });

        // Add pack provenance to receipt (if μ₀ resolved packs)
        if let Some(ref resolved) = self.resolved_packs {
            // Add bundle expansions
            for expansion in &resolved.bundle_expansions {
                receipt.add_bundle_expansion(BundleExpansionRef {
                    bundle_id: expansion.bundle_id.clone(),
                    expanded_to: expansion.expanded_to.clone(),
                });
            }

            // Add pack provenance
            for pack_id in &resolved.atomic_packs {
                let version = resolved
                    .pack_versions
                    .get(&pack_id.to_string())
                    .cloned()
                    .unwrap_or_else(|| "unknown".to_string());

                receipt.add_pack(PackProvenance {
                    pack_id: pack_id.to_string(),
                    version,
                    signature: "local:unsigned".to_string(),
                    digest: resolved.digest_for_pack(pack_id),
                    templates_contributed: resolved.template_paths_for_pack(pack_id),
                    queries_contributed: resolved.query_names_for_pack(pack_id),
                    files_generated: vec![],
                });
            }

            receipt.set_profile(crate::v6::receipt::ProfileRef {
                profile_id: resolved.profile.clone(),
                runtime_constraints: vec![],
                trust_requirement: TrustTier::Experimental,
            });
        }

        // Write receipt if path specified
        if let Some(ref receipt_path) = self.config.receipt_path {
            let full_receipt_path = self.config.base_path.join(receipt_path);
            if let Some(parent) = full_receipt_path.parent() {
                std::fs::create_dir_all(parent)?;
            }
            receipt.write_to_file(&full_receipt_path)?;
        }

        // Verify outputs if requested
        if matches!(
            self.config.verify_mode,
            VerifyMode::VerifyOutputs | VerifyMode::Full
        ) {
            let output_dir = self.config.base_path.join(&self.config.output_dir);
            if !receipt.verify_outputs(&output_dir)? {
                return Err(Error::new(
                    "Output verification failed. hash(A) ≠ hash(μ(O))",
                ));
            }
        }

        let elapsed = start.elapsed();
        tracing::Span::current().record("pipeline.duration_ms", elapsed.as_millis() as u64);
        tracing::info!(
            stage = "mu5",
            elapsed_ms = elapsed.as_millis() as u64,
            "Pipeline stage completed"
        );
        drop(_guard);

        Ok(receipt)
    }

    /// Create output file records from generated files
    fn create_output_records(&self, ctx: &PassContext<'_>) -> Result<Vec<OutputFile>> {
        let mut outputs = Vec::new();

        for rel_path in &ctx.generated_files {
            let full_path = ctx.output_dir.join(rel_path);

            if !full_path.exists() {
                continue;
            }

            outputs.push(OutputFile::from_path(
                &full_path,
                rel_path.clone(),
                "μ₃:emission",
            )?);
        }

        Ok(outputs)
    }

    /// Get the loaded epoch
    pub fn epoch(&self) -> Option<&Epoch> {
        self.epoch.as_ref()
    }

    /// Get executed passes
    pub fn executed_passes(&self) -> &[PassExecution] {
        &self.executed_passes
    }

    /// Get generated files
    pub fn generated_files(&self) -> &[PathBuf] {
        &self.generated_files
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::v6::vocabulary::AllowedVocabulary;
    use tempfile::TempDir;

    #[test]
    fn test_pipeline_config_builder() {
        let config = PipelineConfig::new("test", "1.0.0")
            .with_ontology("ontology/domain.ttl")
            .with_output_dir(".")
            .with_verify_mode(VerifyMode::Full);

        assert_eq!(config.project_name, "test");
        assert_eq!(config.project_version, "1.0.0");
        assert_eq!(config.ontology_sources.len(), 1);
        assert_eq!(config.verify_mode, VerifyMode::Full);
    }

    #[test]
    fn test_pipeline_creation() {
        let config = PipelineConfig::new("test", "1.0.0");
        let pipeline = StagedPipeline::new(config);
        assert!(pipeline.is_ok());
    }

    #[test]
    fn test_pipeline_empty_run() {
        let temp_dir = TempDir::new().unwrap();

        // Create a minimal ontology
        let ontology_dir = temp_dir.path().join("ontology");
        std::fs::create_dir_all(&ontology_dir).unwrap();
        std::fs::write(
            ontology_dir.join("domain.ttl"),
            r#"
            @prefix ex: <http://example.org/> .
            @prefix rdfs: <http://www.w3.org/2000/01/rdf-schema#> .

            ex:Person a rdfs:Class ;
                rdfs:label "Person" .
            "#,
        )
        .unwrap();

        let config = PipelineConfig::new("test", "1.0.0")
            .with_base_path(temp_dir.path())
            .with_ontology(PathBuf::from("ontology/domain.ttl"))
            .with_output_dir("output");

        let mut pipeline = StagedPipeline::new(config).unwrap();

        // Add example.org to allowed vocabularies for testing
        let mut registry = VocabularyRegistry::with_standard_vocabularies();
        registry.add_allowed(
            AllowedVocabulary::new("http://example.org/", "ex")
                .with_description("Example namespace for testing"),
        );
        pipeline = pipeline.with_vocabulary_registry(registry);

        let receipt = pipeline.run().unwrap();

        assert!(receipt.is_valid);
        assert_eq!(receipt.toolchain_version, env!("CARGO_PKG_VERSION"));
    }
}
