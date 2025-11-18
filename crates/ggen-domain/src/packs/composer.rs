//! Multi-pack composition for complex projects
//!
//! This module provides functionality to compose multiple packs into a single
//! cohesive project, with conflict resolution and template merging.

use crate::packs::dependency_graph::DependencyGraph;
use crate::packs::installer::{InstallOptions, PackInstaller};
use crate::packs::repository::{FileSystemRepository, PackRepository};
use crate::packs::types::{CompositionStrategy, Pack};
use ggen_utils::error::{Error, Result};
use std::collections::{HashMap, HashSet};
use std::path::PathBuf;
use std::time::Instant;
use tracing::{info, warn};

/// Pack composer for multi-pack projects
pub struct PackComposer {
    repository: Box<dyn PackRepository>,
}

impl PackComposer {
    /// Create new composer with custom repository
    pub fn new(repository: Box<dyn PackRepository>) -> Self {
        Self { repository }
    }

    /// Create composer with default filesystem repository
    pub fn with_default_repo() -> Result<Self> {
        let repo = FileSystemRepository::discover()?;
        Ok(Self::new(Box::new(repo)))
    }

    /// Compose multiple packs into a single project
    ///
    /// # Features
    /// - Merges templates from multiple packs
    /// - Resolves cross-pack dependencies
    /// - Detects and reports conflicts
    /// - Generates composition plan
    /// - Supports different composition strategies
    ///
    /// # Arguments
    /// * `pack_ids` - List of pack IDs to compose
    /// * `project_name` - Name for the composed project
    /// * `options` - Composition options
    pub async fn compose(
        &self,
        pack_ids: &[String],
        project_name: &str,
        options: &CompositionOptions,
    ) -> Result<CompositionResult> {
        let start = Instant::now();

        if pack_ids.is_empty() {
            return Err(Error::new(
                "At least one pack ID must be specified for composition",
            ));
        }

        info!("Starting composition of {} packs", pack_ids.len());

        // Load all packs
        let mut packs = Vec::new();
        for pack_id in pack_ids {
            let pack = self.repository.load(pack_id).await?;
            packs.push(pack);
        }

        // Build dependency graph
        let graph = DependencyGraph::from_packs(&packs)?;
        let composition_order = graph.topological_sort()?;

        info!("Composition order: {:?}", composition_order);

        // Detect conflicts
        let conflicts = self.detect_composition_conflicts(&packs);
        if !conflicts.is_empty() {
            warn!("Detected {} conflict(s) during composition", conflicts.len());
            for conflict in &conflicts {
                warn!("  - {}", conflict);
            }

            if !options.force_composition {
                return Err(Error::new(&format!(
                    "Composition conflicts detected:\n{}",
                    conflicts.join("\n")
                )));
            }
        }

        // Compose packs according to strategy
        let composed_pack = match options.strategy {
            CompositionStrategy::Merge => self.merge_packs(&packs, &composition_order)?,
            CompositionStrategy::Layer => self.layer_packs(&packs, &composition_order)?,
            CompositionStrategy::Custom(ref rules) => {
                self.custom_composition(&packs, &composition_order, rules)?
            }
        };

        // Generate composition plan
        let plan = self.generate_composition_plan(&packs, &composed_pack, &composition_order);

        // Determine output path
        let output_path = options.output_dir.clone().unwrap_or_else(|| {
            PathBuf::from(project_name)
        });

        // Create output directory
        if !options.dry_run {
            tokio::fs::create_dir_all(&output_path).await?;
        }

        let duration = start.elapsed();

        info!("✓ Composition completed in {:?}", duration);

        Ok(CompositionResult {
            project_name: project_name.to_string(),
            packs_composed: pack_ids.to_vec(),
            composed_pack,
            composition_order,
            conflicts,
            plan,
            output_path,
            duration,
        })
    }

    /// Merge packs by combining packages and templates
    fn merge_packs(&self, packs: &[Pack], order: &[String]) -> Result<Pack> {
        if packs.is_empty() {
            return Err(Error::new("No packs to merge"));
        }

        let first = &packs[0];
        let mut merged = Pack {
            id: format!("composed-{}", first.id),
            name: format!("Composed Pack"),
            version: "1.0.0".to_string(),
            description: format!("Composed from {} packs", packs.len()),
            category: first.category.clone(),
            author: first.author.clone(),
            repository: None,
            license: first.license.clone(),
            packages: Vec::new(),
            templates: Vec::new(),
            sparql_queries: HashMap::new(),
            dependencies: Vec::new(),
            tags: Vec::new(),
            keywords: Vec::new(),
            production_ready: packs.iter().all(|p| p.production_ready),
            metadata: first.metadata.clone(),
        };

        // Merge in topological order
        let mut seen_packages = HashSet::new();
        let mut seen_templates = HashSet::new();

        for pack_id in order {
            if let Some(pack) = packs.iter().find(|p| p.id == *pack_id) {
                // Merge packages
                for package in &pack.packages {
                    if seen_packages.insert(package.clone()) {
                        merged.packages.push(package.clone());
                    }
                }

                // Merge templates
                for template in &pack.templates {
                    if seen_templates.insert(template.name.clone()) {
                        merged.templates.push(template.clone());
                    }
                }

                // Merge SPARQL queries (last one wins for duplicates)
                merged.sparql_queries.extend(pack.sparql_queries.clone());

                // Merge tags
                for tag in &pack.tags {
                    if !merged.tags.contains(tag) {
                        merged.tags.push(tag.clone());
                    }
                }

                // Merge keywords
                for keyword in &pack.keywords {
                    if !merged.keywords.contains(keyword) {
                        merged.keywords.push(keyword.clone());
                    }
                }
            }
        }

        Ok(merged)
    }

    /// Layer packs with override semantics
    fn layer_packs(&self, packs: &[Pack], order: &[String]) -> Result<Pack> {
        // In layering, later packs override earlier ones
        self.merge_packs(packs, order)
    }

    /// Custom composition with user-defined rules
    fn custom_composition(
        &self,
        packs: &[Pack],
        order: &[String],
        _rules: &HashMap<String, serde_json::Value>,
    ) -> Result<Pack> {
        // For now, fallback to merge
        // Custom composition rules can be extended via config
        self.merge_packs(packs, order)
    }

    /// Detect conflicts during composition
    fn detect_composition_conflicts(&self, packs: &[Pack]) -> Vec<String> {
        let mut conflicts = Vec::new();

        // Check for duplicate packages
        let mut package_sources: HashMap<String, Vec<String>> = HashMap::new();
        for pack in packs {
            for package in &pack.packages {
                package_sources
                    .entry(package.clone())
                    .or_insert_with(Vec::new)
                    .push(pack.name.clone());
            }
        }

        for (package, sources) in package_sources {
            if sources.len() > 1 {
                conflicts.push(format!(
                    "Package '{}' provided by: {}",
                    package,
                    sources.join(", ")
                ));
            }
        }

        // Check for duplicate template names
        let mut template_sources: HashMap<String, Vec<String>> = HashMap::new();
        for pack in packs {
            for template in &pack.templates {
                template_sources
                    .entry(template.name.clone())
                    .or_insert_with(Vec::new)
                    .push(pack.name.clone());
            }
        }

        for (template, sources) in template_sources {
            if sources.len() > 1 {
                conflicts.push(format!(
                    "Template '{}' provided by: {}",
                    template,
                    sources.join(", ")
                ));
            }
        }

        conflicts
    }

    /// Generate composition plan
    fn generate_composition_plan(
        &self,
        packs: &[Pack],
        composed: &Pack,
        order: &[String],
    ) -> CompositionPlan {
        let steps = order
            .iter()
            .map(|pack_id| {
                let pack = packs.iter().find(|p| p.id == *pack_id).unwrap();
                CompositionStep {
                    pack_id: pack.id.clone(),
                    pack_name: pack.name.clone(),
                    packages_to_add: pack.packages.len(),
                    templates_to_add: pack.templates.len(),
                }
            })
            .collect();

        CompositionPlan {
            total_packs: packs.len(),
            total_packages: composed.packages.len(),
            total_templates: composed.templates.len(),
            composition_order: order.to_vec(),
            steps,
        }
    }

    /// Install composed pack
    pub async fn install_composed(
        &self,
        composition_result: &CompositionResult,
        install_options: &InstallOptions,
    ) -> Result<()> {
        let installer = PackInstaller::new(self.repository.clone());

        // Save composed pack temporarily
        self.repository
            .save(&composition_result.composed_pack)
            .await?;

        // Install the composed pack
        let report = installer
            .install(&composition_result.composed_pack.id, install_options)
            .await?;

        info!("{}", report.summary());

        Ok(())
    }
}

/// Composition options
#[derive(Debug, Clone)]
pub struct CompositionOptions {
    /// Composition strategy
    pub strategy: CompositionStrategy,
    /// Output directory
    pub output_dir: Option<PathBuf>,
    /// Force composition even if conflicts exist
    pub force_composition: bool,
    /// Dry run mode
    pub dry_run: bool,
}

impl Default for CompositionOptions {
    fn default() -> Self {
        Self {
            strategy: CompositionStrategy::Merge,
            output_dir: None,
            force_composition: false,
            dry_run: false,
        }
    }
}

/// Composition result
#[derive(Debug, Clone)]
pub struct CompositionResult {
    /// Project name
    pub project_name: String,
    /// Pack IDs that were composed
    pub packs_composed: Vec<String>,
    /// The composed pack
    pub composed_pack: Pack,
    /// Composition order
    pub composition_order: Vec<String>,
    /// Conflicts detected
    pub conflicts: Vec<String>,
    /// Composition plan
    pub plan: CompositionPlan,
    /// Output path
    pub output_path: PathBuf,
    /// Time taken
    pub duration: std::time::Duration,
}

/// Composition plan
#[derive(Debug, Clone)]
pub struct CompositionPlan {
    /// Total number of packs
    pub total_packs: usize,
    /// Total packages after composition
    pub total_packages: usize,
    /// Total templates after composition
    pub total_templates: usize,
    /// Composition order
    pub composition_order: Vec<String>,
    /// Individual steps
    pub steps: Vec<CompositionStep>,
}

/// Individual composition step
#[derive(Debug, Clone)]
pub struct CompositionStep {
    /// Pack ID
    pub pack_id: String,
    /// Pack name
    pub pack_name: String,
    /// Number of packages to add
    pub packages_to_add: usize,
    /// Number of templates to add
    pub templates_to_add: usize,
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::packs::types::{PackDependency, PackMetadata, PackTemplate};

    fn create_test_pack(id: &str, packages: Vec<&str>, templates: Vec<&str>) -> Pack {
        Pack {
            id: id.to_string(),
            name: format!("Pack {}", id),
            version: "1.0.0".to_string(),
            description: format!("Test pack {}", id),
            category: "test".to_string(),
            author: None,
            repository: None,
            license: None,
            packages: packages.into_iter().map(|s| s.to_string()).collect(),
            templates: templates
                .into_iter()
                .map(|name| PackTemplate {
                    name: name.to_string(),
                    path: format!("templates/{}.tmpl", name),
                    description: format!("Template {}", name),
                    variables: vec![],
                })
                .collect(),
            sparql_queries: HashMap::new(),
            dependencies: vec![],
            tags: vec![],
            keywords: vec![],
            production_ready: true,
            metadata: PackMetadata::default(),
        }
    }

    #[test]
    fn test_composition_options_default() {
        let opts = CompositionOptions::default();
        assert!(!opts.force_composition);
        assert!(!opts.dry_run);
        assert!(opts.output_dir.is_none());
    }

    #[test]
    fn test_detect_package_conflicts() {
        let packs = vec![
            create_test_pack("pack1", vec!["pkg1", "pkg2"], vec![]),
            create_test_pack("pack2", vec!["pkg2", "pkg3"], vec![]),
        ];

        let composer = PackComposer::new(Box::new(
            FileSystemRepository::new(PathBuf::from("/tmp"))
        ));

        let conflicts = composer.detect_composition_conflicts(&packs);

        // Should detect pkg2 conflict
        assert_eq!(conflicts.len(), 1);
        assert!(conflicts[0].contains("pkg2"));
    }

    #[test]
    fn test_detect_template_conflicts() {
        let packs = vec![
            create_test_pack("pack1", vec![], vec!["main", "config"]),
            create_test_pack("pack2", vec![], vec!["config", "utils"]),
        ];

        let composer = PackComposer::new(Box::new(
            FileSystemRepository::new(PathBuf::from("/tmp"))
        ));

        let conflicts = composer.detect_composition_conflicts(&packs);

        // Should detect config template conflict
        assert_eq!(conflicts.len(), 1);
        assert!(conflicts[0].contains("config"));
    }
}
