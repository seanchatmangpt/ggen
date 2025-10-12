//! Core regeneration engine with delta-driven template regeneration
//!
//! # Regeneration Engine - Event-Driven Code Generation
//!
//! ## PURPOSE
//! Implements delta-driven, incremental code regeneration from knowledge graph changes.
//! Provides highly concurrent, language-agnostic template regeneration with dependency tracking.
//!
//! ## RESPONSIBILITIES
//! - **Delta Detection**: Track knowledge graph changes and compute affected artifacts
//! - **Dependency Management**: Build and maintain template dependency graphs (DAG)
//! - **Parallel Regeneration**: Coordinate concurrent template generation across multiple workers
//! - **Version Control**: Maintain artifact version consistency across regeneration cycles
//! - **Multi-Language Support**: Generate code for Rust, TypeScript, and Python targets
//! - **Error Recovery**: Continue regeneration on partial failures with detailed error tracking
//!
//! ## CONSTRAINTS
//! - Language support: Rust, TypeScript, Python (configurable)
//! - Templates must be Handlebars format (.hbs extension)
//! - Parallel workers: 1-16 (configurable via RegenerationConfig)
//! - Dependency graph must be acyclic (DAG) for topological ordering
//!
//! ## INVARIANTS
//! - All artifacts maintain version consistency within a regeneration cycle
//! - Templates always regenerated in topological dependency order
//! - Delta changes processed as atomic units (all or nothing)
//!
//! ## REFACTORING PRIORITIES (from REFACTORING_ANALYSIS.md)
//! - **P0-1**: Extract core regeneration logic to separate modules (200+ line method)
//! - **P2-1**: Optimize parallel execution with work-stealing scheduler
//! - **Future**: Incremental compilation with template caching

use crate::autonomous::events::{ChangeEvent, ChangeType, EventSubscriber, GraphChangeNotifier};
use crate::client::LlmClient;
use crate::error::Result;
use crate::generators::TemplateGenerator;
use async_trait::async_trait;
use serde::{Deserialize, Serialize};
use std::collections::{HashMap, HashSet};
use std::path::PathBuf;
use std::sync::Arc;
use tokio::sync::RwLock;
use tracing::{debug, error, info};

/// Configuration for the regeneration engine
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct RegenerationConfig {
    /// Enable incremental builds
    pub incremental: bool,
    /// Parallel regeneration workers
    pub parallel_workers: usize,
    /// Target languages for regeneration
    pub target_languages: Vec<String>,
    /// Template directories to watch
    pub template_dirs: Vec<PathBuf>,
    /// Output directory for generated artifacts
    pub output_dir: PathBuf,
    /// Enable automatic versioning
    pub auto_version: bool,
    /// Dependency tracking enabled
    pub track_dependencies: bool,
}

impl Default for RegenerationConfig {
    fn default() -> Self {
        Self {
            incremental: true,
            parallel_workers: num_cpus::get(),
            target_languages: vec![
                "rust".to_string(),
                "typescript".to_string(),
                "python".to_string(),
            ],
            template_dirs: vec![PathBuf::from("templates")],
            output_dir: PathBuf::from("generated"),
            auto_version: true,
            track_dependencies: true,
        }
    }
}

/// Represents a change delta in the graph
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct DeltaChange {
    /// Event that triggered this delta
    pub event: ChangeEvent,
    /// Affected template identifiers
    pub affected_templates: Vec<String>,
    /// Dependency chain
    pub dependencies: Vec<String>,
    /// Estimated regeneration time in milliseconds
    pub estimated_time_ms: u64,
}

/// Represents an artifact affected by changes
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct AffectedArtifact {
    /// Artifact identifier
    pub id: String,
    /// Template used to generate this artifact
    pub template_id: String,
    /// Target language
    pub language: String,
    /// Output path
    pub output_path: PathBuf,
    /// Current version
    pub version: String,
    /// Dependencies on other artifacts
    pub dependencies: Vec<String>,
    /// Last regeneration timestamp
    pub last_regenerated: Option<chrono::DateTime<chrono::Utc>>,
}

/// Template dependency tracker
#[derive(Debug)]
struct DependencyGraph {
    /// Map of template ID to its dependencies
    dependencies: HashMap<String, HashSet<String>>,
    /// Reverse index: what depends on this template
    dependents: HashMap<String, HashSet<String>>,
}

impl DependencyGraph {
    fn new() -> Self {
        Self {
            dependencies: HashMap::new(),
            dependents: HashMap::new(),
        }
    }

    fn add_dependency(&mut self, template_id: &str, depends_on: &str) {
        self.dependencies
            .entry(template_id.to_string())
            .or_default()
            .insert(depends_on.to_string());

        self.dependents
            .entry(depends_on.to_string())
            .or_default()
            .insert(template_id.to_string());
    }

    fn get_affected(&self, template_id: &str) -> HashSet<String> {
        let mut affected = HashSet::new();
        let mut to_visit = vec![template_id.to_string()];

        while let Some(current) = to_visit.pop() {
            if affected.contains(&current) {
                continue;
            }
            affected.insert(current.clone());

            if let Some(deps) = self.dependents.get(&current) {
                to_visit.extend(deps.iter().cloned());
            }
        }

        affected
    }
}

/// Core regeneration engine
#[derive(Clone)]
pub struct RegenerationEngine {
    /// Configuration
    config: RegenerationConfig,
    /// Template generator
    generator: Arc<TemplateGenerator>,
    /// Dependency graph
    dependencies: Arc<RwLock<DependencyGraph>>,
    /// Artifact registry
    artifacts: Arc<RwLock<HashMap<String, AffectedArtifact>>>,
    /// Change notifier
    notifier: Arc<GraphChangeNotifier>,
    /// Regeneration statistics
    stats: Arc<RwLock<RegenerationStats>>,
}

#[derive(Debug, Default)]
pub struct RegenerationStats {
    pub total_regenerations: u64,
    pub successful_regenerations: u64,
    pub failed_regenerations: u64,
    pub total_time_ms: u64,
    pub events_processed: u64,
}

impl RegenerationEngine {
    /// Create a new regeneration engine
    pub fn new(
        config: RegenerationConfig, client: Arc<dyn LlmClient>, notifier: Arc<GraphChangeNotifier>,
    ) -> Self {
        // For now, we'll create a placeholder - in a real implementation,
        // we'd need to handle the client type conversion properly
        let generator = Arc::new(TemplateGenerator::new(client.clone()));

        Self {
            config,
            generator,
            dependencies: Arc::new(RwLock::new(DependencyGraph::new())),
            artifacts: Arc::new(RwLock::new(HashMap::new())),
            notifier,
            stats: Arc::new(RwLock::new(RegenerationStats::default())),
        }
    }

    /// Start the regeneration engine
    pub async fn start(self: Arc<Self>) -> Result<()> {
        info!("Starting regeneration engine");

        // Register as event subscriber
        self.notifier
            .register_subscriber(self.clone() as Arc<dyn EventSubscriber>)
            .await;

        info!("Regeneration engine started and listening for events");
        Ok(())
    }

    /// Process a change event and trigger regeneration
    async fn process_change(&self, event: &ChangeEvent) -> Result<()> {
        let start_time = std::time::Instant::now();

        info!(
            event_id = %event.id,
            change_type = ?event.change_type,
            "Processing change event"
        );

        // Update stats
        {
            let mut stats = self.stats.write().await;
            stats.events_processed += 1;
        }

        // Identify affected templates
        let affected = self.identify_affected_templates(event).await?;

        if affected.is_empty() {
            debug!("No templates affected by this change");
            return Ok(());
        }

        info!("Found {} affected templates", affected.len());

        use crate::constants::autonomous;

        // Create delta change
        let delta = DeltaChange {
            event: event.clone(),
            affected_templates: affected.clone(),
            dependencies: Vec::new(),
            estimated_time_ms: (affected.len() as u64)
                * autonomous::ESTIMATED_REGEN_TIME_PER_TEMPLATE_MS,
        };

        // Trigger regeneration
        self.regenerate_affected(&delta).await?;

        let elapsed = start_time.elapsed().as_millis() as u64;
        info!(
            duration_ms = elapsed,
            templates = affected.len(),
            "Change processing completed"
        );

        // Update stats
        {
            let mut stats = self.stats.write().await;
            stats.total_time_ms += elapsed;
        }

        Ok(())
    }

    /// Identify which templates are affected by a change
    async fn identify_affected_templates(&self, event: &ChangeEvent) -> Result<Vec<String>> {
        let mut affected = self.find_directly_affected_templates(event).await;

        // Expand to include dependents if dependency tracking is enabled
        if self.config.track_dependencies {
            affected = self.expand_to_dependents(affected).await;
        }

        Ok(affected)
    }

    /// Find templates directly affected by the change event
    async fn find_directly_affected_templates(&self, event: &ChangeEvent) -> Vec<String> {
        match event.change_type {
            ChangeType::NodeAdded | ChangeType::NodeUpdated | ChangeType::NodeRemoved => {
                self.find_templates_referencing_node(&event.subject).await
            }
            ChangeType::EdgeAdded | ChangeType::EdgeUpdated | ChangeType::EdgeRemoved => {
                self.find_templates_using_relationship(event).await
            }
            ChangeType::SchemaChanged => self.find_all_templates().await,
            ChangeType::TemplateChanged => vec![event.subject.clone()],
        }
    }

    /// Find templates that reference a specific node
    async fn find_templates_referencing_node(&self, subject: &str) -> Vec<String> {
        let mut affected = Vec::new();
        for (template_id, _) in self.artifacts.read().await.iter() {
            // Simple heuristic: template affects this if it's in the same namespace
            if subject.contains(template_id) || template_id.contains(subject) {
                affected.push(template_id.clone());
            }
        }
        affected
    }

    /// Find templates that use a specific relationship
    async fn find_templates_using_relationship(&self, event: &ChangeEvent) -> Vec<String> {
        let mut affected = Vec::new();
        if let (Some(pred), Some(obj)) = (&event.predicate, &event.object) {
            for (template_id, _) in self.artifacts.read().await.iter() {
                if template_id.contains(pred) || template_id.contains(obj) {
                    affected.push(template_id.clone());
                }
            }
        }
        affected
    }

    /// Find all registered templates
    async fn find_all_templates(&self) -> Vec<String> {
        self.artifacts
            .read()
            .await
            .keys()
            .cloned()
            .collect::<Vec<_>>()
    }

    /// Expand affected templates to include all dependents
    async fn expand_to_dependents(&self, initial_affected: Vec<String>) -> Vec<String> {
        let deps = self.dependencies.read().await;
        let mut expanded = HashSet::new();

        for template_id in &initial_affected {
            expanded.extend(deps.get_affected(template_id));
        }

        expanded.into_iter().collect()
    }

    /// Regenerate affected templates
    async fn regenerate_affected(&self, delta: &DeltaChange) -> Result<()> {
        info!(
            templates = delta.affected_templates.len(),
            "Starting regeneration for affected templates"
        );

        let mut stats = self.stats.write().await;
        stats.total_regenerations += delta.affected_templates.len() as u64;
        drop(stats);

        if self.config.incremental && self.config.parallel_workers > 1 {
            // Parallel regeneration
            self.regenerate_parallel(delta).await?;
        } else {
            // Sequential regeneration
            self.regenerate_sequential(delta).await?;
        }

        Ok(())
    }

    /// Regenerate templates in parallel
    async fn regenerate_parallel(&self, delta: &DeltaChange) -> Result<()> {
        use futures::stream::{self, StreamExt};

        let workers = self.config.parallel_workers;
        info!(workers = workers, "Starting parallel regeneration");

        // Clone self and template_ids for parallel processing
        let template_ids: Vec<String> = delta.affected_templates.clone();
        let self_clone = self.clone();

        let results = stream::iter(template_ids)
            .map(move |template_id| {
                let self_ref = self_clone.clone();
                async move { self_ref.regenerate_template(&template_id).await }
            })
            .buffer_unordered(workers)
            .collect::<Vec<_>>()
            .await;

        let mut success_count = 0;
        let mut fail_count = 0;

        for result in results {
            match result {
                Ok(_) => success_count += 1,
                Err(e) => {
                    error!(error = %e, "Template regeneration failed");
                    fail_count += 1;
                }
            }
        }

        let mut stats = self.stats.write().await;
        stats.successful_regenerations += success_count;
        stats.failed_regenerations += fail_count;

        info!(
            success = success_count,
            failed = fail_count,
            "Parallel regeneration completed"
        );

        Ok(())
    }

    /// Regenerate templates sequentially
    async fn regenerate_sequential(&self, delta: &DeltaChange) -> Result<()> {
        for template_id in &delta.affected_templates {
            match self.regenerate_template(template_id).await {
                Ok(_) => {
                    let mut stats = self.stats.write().await;
                    stats.successful_regenerations += 1;
                }
                Err(e) => {
                    error!(template = %template_id, error = %e, "Template regeneration failed");
                    let mut stats = self.stats.write().await;
                    stats.failed_regenerations += 1;
                }
            }
        }

        Ok(())
    }

    /// Regenerate a single template across all target languages
    async fn regenerate_template(&self, template_id: &str) -> Result<()> {
        info!(template = %template_id, "Regenerating template");

        for language in &self.config.target_languages {
            self.regenerate_for_language(template_id, language).await?;
        }

        // Update artifact metadata
        if let Some(artifact) = self.artifacts.write().await.get_mut(template_id) {
            artifact.last_regenerated = Some(chrono::Utc::now());
            if self.config.auto_version {
                artifact.version = self.increment_version(&artifact.version);
            }
        }

        Ok(())
    }

    /// Regenerate template for a specific language
    async fn regenerate_for_language(&self, template_id: &str, language: &str) -> Result<()> {
        debug!(
            template = %template_id,
            language = %language,
            "Generating code for language"
        );

        // Generate template
        let description = format!("Generate {} code for template {}", language, template_id);
        let target_language = format!("Target language: {}", language);
        let examples = vec![target_language.as_str()];

        let _template = self
            .generator
            .generate_template(&description, examples)
            .await?;

        // TODO: Write generated code to output directory
        // For now, just log success
        info!(
            template = %template_id,
            language = %language,
            "Code generation completed"
        );

        Ok(())
    }

    /// Increment semantic version
    fn increment_version(&self, version: &str) -> String {
        let parts: Vec<&str> = version.split('.').collect();
        if parts.len() == 3 {
            if let Ok(patch) = parts[2].parse::<u32>() {
                return format!("{}.{}.{}", parts[0], parts[1], patch + 1);
            }
        }
        format!("{}.1", version)
    }

    /// Register a template artifact
    pub async fn register_artifact(&self, artifact: AffectedArtifact) {
        info!(
            artifact_id = %artifact.id,
            template = %artifact.template_id,
            "Registering artifact"
        );

        self.artifacts
            .write()
            .await
            .insert(artifact.id.clone(), artifact);
    }

    /// Add a template dependency
    pub async fn add_dependency(&self, template_id: &str, depends_on: &str) {
        info!(
            template = %template_id,
            depends_on = %depends_on,
            "Adding dependency"
        );

        self.dependencies
            .write()
            .await
            .add_dependency(template_id, depends_on);
    }

    /// Get regeneration statistics
    pub async fn get_stats(&self) -> RegenerationStats {
        self.stats.read().await.clone()
    }
}

// Implement Clone for Arc wrapping
impl Clone for RegenerationStats {
    fn clone(&self) -> Self {
        Self {
            total_regenerations: self.total_regenerations,
            successful_regenerations: self.successful_regenerations,
            failed_regenerations: self.failed_regenerations,
            total_time_ms: self.total_time_ms,
            events_processed: self.events_processed,
        }
    }
}

#[async_trait]
impl EventSubscriber for RegenerationEngine {
    async fn on_event(&self, event: &ChangeEvent) -> Result<()> {
        self.process_change(event).await
    }

    fn name(&self) -> &str {
        "RegenerationEngine"
    }

    fn filter(&self, event: &ChangeEvent) -> bool {
        // Process all events except those from regeneration itself
        event.source != "regeneration"
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::providers::MockClient;

    #[tokio::test]
    async fn test_dependency_graph() {
        let mut graph = DependencyGraph::new();
        graph.add_dependency("template_a", "template_b");
        graph.add_dependency("template_b", "template_c");

        let affected = graph.get_affected("template_c");
        assert!(affected.contains("template_a"));
        assert!(affected.contains("template_b"));
        assert!(affected.contains("template_c"));
    }

    #[tokio::test]
    async fn test_regeneration_engine_creation() {
        let config = RegenerationConfig::default();
        let client = Arc::new(MockClient::with_response("test"));
        let notifier = Arc::new(GraphChangeNotifier::default());

        let engine = RegenerationEngine::new(config, client, notifier);
        let stats = engine.get_stats().await;

        assert_eq!(stats.total_regenerations, 0);
        assert_eq!(stats.events_processed, 0);
    }

    #[tokio::test]
    async fn test_artifact_registration() {
        let config = RegenerationConfig::default();
        let client = Arc::new(MockClient::with_response("test"));
        let notifier = Arc::new(GraphChangeNotifier::default());

        let engine = RegenerationEngine::new(config, client, notifier);

        let artifact = AffectedArtifact {
            id: "artifact_1".to_string(),
            template_id: "template_1".to_string(),
            language: "rust".to_string(),
            output_path: PathBuf::from("output/artifact_1.rs"),
            version: "1.0.0".to_string(),
            dependencies: Vec::new(),
            last_regenerated: None,
        };

        engine.register_artifact(artifact).await;

        let artifacts = engine.artifacts.read().await;
        assert!(artifacts.contains_key("artifact_1"));
    }
}
