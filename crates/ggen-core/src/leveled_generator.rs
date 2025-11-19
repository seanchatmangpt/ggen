//! Leveled Streaming Generator
//!
//! Enhanced template generator with Mura leveling and Heijunka scheduling integration.
//! Provides smooth workload distribution, predictable cadence, and variability reduction
//! for polyglot template generation across multiple language targets.
//!
//! ## Features
//!
//! - **Mura Leveling**: Balanced workload distribution across targets
//! - **Heijunka Scheduling**: Predictable batch processing with fixed cadence
//! - **RDF Batch Processing**: Standardized ontology transformation sequences
//! - **Variability Reduction**: Consistent generation latency
//! - **Comprehensive Metrics**: Real-time tracking and reporting

use crate::lifecycle::{
    BatchMetrics, HeijunkaConfig, HeijunkaScheduler, MuraConfig, ScheduledBatch,
    StandardWorkSequence, TargetWorkload, WorkStep, WorkloadLeveler,
};
use crate::pipeline::Pipeline;
use crate::streaming_generator::{GenerationResult, StreamingGenerator};
use crate::template_cache::TemplateCache;
use ggen_utils::error::{Error, Result};
use std::collections::HashMap;
use std::path::{Path, PathBuf};
use std::time::{Duration, Instant};
use tera::Context;
use walkdir::WalkDir;

/// Configuration for leveled generation
#[derive(Debug, Clone)]
pub struct LeveledGeneratorConfig {
    /// Mura leveling configuration
    pub mura: MuraConfig,
    /// Heijunka scheduling configuration
    pub heijunka: HeijunkaConfig,
    /// Enable RDF batch processing
    pub enable_rdf_batching: bool,
    /// RDF batch size
    pub rdf_batch_size: usize,
    /// Enable pre-warming caches
    pub enable_cache_prewarming: bool,
}

impl Default for LeveledGeneratorConfig {
    fn default() -> Self {
        Self {
            mura: MuraConfig::default(),
            heijunka: HeijunkaConfig::default(),
            enable_rdf_batching: true,
            rdf_batch_size: 10,
            enable_cache_prewarming: true,
        }
    }
}

impl LeveledGeneratorConfig {
    /// Configuration optimized for CI/CD pipelines
    pub fn fast_cicd() -> Self {
        Self {
            mura: MuraConfig {
                target_batch_size: 15,
                target_cv: 0.2,
                ..Default::default()
            },
            heijunka: HeijunkaConfig::fast_cicd(),
            enable_rdf_batching: true,
            rdf_batch_size: 20,
            enable_cache_prewarming: true,
        }
    }

    /// Configuration optimized for heavy ontology processing
    pub fn heavy_ontology() -> Self {
        Self {
            mura: MuraConfig {
                target_batch_size: 5,
                target_cv: 0.4,
                ..Default::default()
            },
            heijunka: HeijunkaConfig::heavy_ontology(),
            enable_rdf_batching: true,
            rdf_batch_size: 50,
            enable_cache_prewarming: true,
        }
    }
}

/// Template item with metadata for leveling
#[derive(Debug, Clone)]
pub struct TemplateItem {
    pub path: PathBuf,
    pub target: String,
    pub has_rdf: bool,
    pub has_sparql: bool,
}

/// Leveled streaming generator
pub struct LeveledGenerator {
    config: LeveledGeneratorConfig,
    template_dir: PathBuf,
    output_dir: PathBuf,
    cache: TemplateCache,
    pipeline: Pipeline,
    workload_leveler: WorkloadLeveler,
}

impl LeveledGenerator {
    pub fn new(
        config: LeveledGeneratorConfig, template_dir: PathBuf, output_dir: PathBuf,
    ) -> Result<Self> {
        Ok(Self {
            workload_leveler: WorkloadLeveler::new(config.mura.clone()),
            config,
            template_dir,
            output_dir,
            cache: TemplateCache::default(),
            pipeline: Pipeline::new()?,
        })
    }

    /// Generate all templates with Mura leveling and Heijunka scheduling
    pub fn generate_all(&mut self, vars: &Context) -> Result<LeveledGenerationResult> {
        self.workload_leveler.start();
        let total_start = Instant::now();

        tracing::info!("🚀 Starting leveled generation with Mura + Heijunka");

        // Step 1: Discover and classify templates
        let template_items = self.discover_templates()?;
        tracing::info!(
            total_templates = template_items.len(),
            "📝 Templates discovered"
        );

        // Step 2: Distribute templates across targets for balanced workload (Mura)
        let targets = self.identify_targets(&template_items);
        let distribution = self
            .workload_leveler
            .distribute_templates(template_items, &targets);

        tracing::info!(
            targets = targets.len(),
            "🎯 Templates distributed across targets"
        );

        // Step 3: Process each target with Heijunka scheduling
        let mut target_results = HashMap::new();
        let mut total_files_created = 0;

        for (target, items) in &distribution {
            let target_start = Instant::now();

            tracing::info!(
                target = %target,
                templates = items.len(),
                "▶️  Processing target"
            );

            let result = self.process_target_with_heijunka(target, items, vars)?;

            total_files_created += result.files_created;

            // Record target workload
            let workload = TargetWorkload {
                name: target.clone(),
                template_count: items.len(),
                total_duration: target_start.elapsed(),
                avg_duration: target_start.elapsed() / items.len() as u32,
                peak_duration: result.peak_duration,
                std_deviation: result.std_deviation,
                rdf_operations: result.rdf_operations,
                sparql_queries: result.sparql_queries,
            };

            self.workload_leveler.record_target_workload(workload);
            target_results.insert(target.clone(), result);
        }

        let total_duration = total_start.elapsed();

        Ok(LeveledGenerationResult {
            total_duration,
            total_templates: distribution.values().map(|v| v.len()).sum(),
            total_files_created,
            targets: target_results,
            workload_imbalance: self.workload_leveler.calculate_imbalance(),
            is_balanced: self.workload_leveler.is_balanced(),
        })
    }

    /// Discover all templates in template directory
    fn discover_templates(&self) -> Result<Vec<TemplateItem>> {
        let mut items = Vec::new();

        for entry in WalkDir::new(&self.template_dir) {
            let entry = entry
                .map_err(|e| Error::new(&format!("Failed to read directory entry: {}", e)))?;

            if !entry.file_type().is_file() {
                continue;
            }

            let path = entry.path();
            if path.extension() != Some(std::ffi::OsStr::new("tmpl")) {
                continue;
            }

            // Determine target from path or template content
            let target = self.determine_target(path)?;

            // Quick check for RDF/SPARQL (could be improved with actual parsing)
            let content = std::fs::read_to_string(path).unwrap_or_default();
            let has_rdf = content.contains("rdf:") || content.contains("rdf_inline:");
            let has_sparql = content.contains("sparql:");

            items.push(TemplateItem {
                path: path.to_path_buf(),
                target,
                has_rdf,
                has_sparql,
            });
        }

        Ok(items)
    }

    /// Determine target language from template path
    fn determine_target(&self, path: &Path) -> Result<String> {
        // Check path components for target indicators
        let path_str = path.to_string_lossy().to_lowercase();

        if path_str.contains("rust") || path_str.contains(".rs") {
            Ok("rust".to_string())
        } else if path_str.contains("typescript") || path_str.contains(".ts") {
            Ok("typescript".to_string())
        } else if path_str.contains("javascript") || path_str.contains(".js") {
            Ok("javascript".to_string())
        } else if path_str.contains("python") || path_str.contains(".py") {
            Ok("python".to_string())
        } else if path_str.contains("go") || path_str.contains(".go") {
            Ok("go".to_string())
        } else {
            Ok("generic".to_string())
        }
    }

    /// Identify unique targets from template items
    fn identify_targets(&self, items: &[TemplateItem]) -> Vec<String> {
        let mut targets: Vec<String> = items
            .iter()
            .map(|item| item.target.clone())
            .collect::<std::collections::HashSet<_>>()
            .into_iter()
            .collect();

        targets.sort();
        targets
    }

    /// Process a target with Heijunka scheduling
    fn process_target_with_heijunka(
        &mut self, target: &str, items: &[TemplateItem], vars: &Context,
    ) -> Result<TargetResult> {
        let mut scheduler = HeijunkaScheduler::new(self.config.heijunka.clone());

        // Clone items for scheduling (we need owned values)
        let items_vec: Vec<TemplateItem> = items.to_vec();

        // Schedule batches with Heijunka
        scheduler.schedule_batches(items_vec);

        let mut files_created = 0;
        let mut rdf_operations = 0;
        let mut sparql_queries = 0;
        let mut durations = Vec::new();

        // Process each batch
        while let Some(mut batch) = scheduler.next_batch() {
            let batch_start = Instant::now();

            // Process templates in batch
            for item in &batch.items {
                let template = self.cache.get_or_parse(&item.path)?;
                let mut template = (*template).clone();

                // Render frontmatter
                template.render_frontmatter(&mut self.pipeline.tera, vars)?;

                // Process RDF if present
                if item.has_rdf || item.has_sparql {
                    template.process_graph(
                        &mut self.pipeline.graph,
                        &mut self.pipeline.tera,
                        vars,
                        &item.path,
                    )?;
                    rdf_operations += 1;
                }

                if item.has_sparql {
                    sparql_queries += template.front.sparql.len();
                }

                // Render template
                let rendered = template.render(&mut self.pipeline.tera, vars)?;

                // Write output
                let output_path = self.determine_output_path(&item.path, &template)?;
                if let Some(parent) = output_path.parent() {
                    std::fs::create_dir_all(parent)?;
                }
                std::fs::write(&output_path, rendered)?;

                files_created += 1;
            }

            let batch_duration = batch_start.elapsed();
            durations.push(batch_duration);

            // Update batch metrics
            batch.rdf_operations = rdf_operations;
            batch.sparql_queries = sparql_queries;

            scheduler.complete_batch(batch);

            // Record batch metrics for Mura leveling
            let metrics = BatchMetrics::new(batch.id, batch.items.len())
                .with_duration(batch_duration)
                .with_rdf_operations(rdf_operations)
                .with_sparql_queries(sparql_queries);

            self.workload_leveler.record_batch(metrics);
        }

        // Calculate statistics
        let avg_duration = if !durations.is_empty() {
            durations.iter().sum::<Duration>() / durations.len() as u32
        } else {
            Duration::ZERO
        };

        let peak_duration = durations.iter().max().copied().unwrap_or(Duration::ZERO);

        let std_deviation = if durations.len() > 1 {
            let mean = avg_duration.as_secs_f64();
            let variance: f64 = durations
                .iter()
                .map(|d| {
                    let diff = d.as_secs_f64() - mean;
                    diff * diff
                })
                .sum::<f64>()
                / durations.len() as f64;
            variance.sqrt()
        } else {
            0.0
        };

        Ok(TargetResult {
            files_created,
            rdf_operations,
            sparql_queries,
            peak_duration,
            std_deviation,
            schedule_adherence: scheduler.schedule_adherence(),
        })
    }

    /// Determine output path for a template
    fn determine_output_path(
        &self, template_path: &Path, template: &crate::template::Template,
    ) -> Result<PathBuf> {
        if let Some(to_path) = &template.front.to {
            let rendered_to = self.pipeline.tera.render_str(
                to_path,
                &Context::new(),
            )?;
            Ok(self.output_dir.join(rendered_to))
        } else {
            let filename = template_path
                .file_stem()
                .ok_or_else(|| Error::new("Template has no filename"))?
                .to_string_lossy();
            Ok(self.output_dir.join(format!("{}.out", filename)))
        }
    }

    /// Generate comprehensive report
    pub fn report(&self) {
        self.workload_leveler.report();
    }

    /// Get workload leveler for additional metrics
    pub fn workload_leveler(&self) -> &WorkloadLeveler {
        &self.workload_leveler
    }
}

/// Result for a single target
#[derive(Debug, Clone)]
pub struct TargetResult {
    pub files_created: usize,
    pub rdf_operations: usize,
    pub sparql_queries: usize,
    pub peak_duration: Duration,
    pub std_deviation: f64,
    pub schedule_adherence: f64,
}

/// Overall leveled generation result
#[derive(Debug, Clone)]
pub struct LeveledGenerationResult {
    pub total_duration: Duration,
    pub total_templates: usize,
    pub total_files_created: usize,
    pub targets: HashMap<String, TargetResult>,
    pub workload_imbalance: f64,
    pub is_balanced: bool,
}

impl LeveledGenerationResult {
    /// Get throughput (templates per second)
    pub fn throughput(&self) -> f64 {
        if self.total_duration.as_secs_f64() == 0.0 {
            return 0.0;
        }
        self.total_templates as f64 / self.total_duration.as_secs_f64()
    }

    /// Generate summary report
    pub fn report(&self) {
        println!("\n🎉 Leveled Generation Summary");
        println!("═══════════════════════════════════════════════════════════");
        println!("  Total Templates:       {}", self.total_templates);
        println!("  Total Files Created:   {}", self.total_files_created);
        println!("  Total Duration:        {:.2}s", self.total_duration.as_secs_f64());
        println!("  Throughput:            {:.2} templates/sec", self.throughput());
        println!("  Workload Imbalance:    {:.3} (CV)", self.workload_imbalance);
        println!(
            "  Balance Status:        {}",
            if self.is_balanced {
                "✅ Balanced"
            } else {
                "⚠️  Unbalanced"
            }
        );

        println!("\n🎯 Target Summary:");
        for (target, result) in &self.targets {
            println!(
                "  {:12} │ Files: {:4} │ RDF Ops: {:3} │ SPARQL: {:3} │ Adherence: {:.3}",
                target,
                result.files_created,
                result.rdf_operations,
                result.sparql_queries,
                result.schedule_adherence
            );
        }

        println!("═══════════════════════════════════════════════════════════\n");
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use chicago_tdd_tools::{async_test, test};
    use tempfile::TempDir;

    test!(test_leveled_generator_config_defaults, {
        let config = LeveledGeneratorConfig::default();
        assert!(config.enable_rdf_batching);
        assert!(config.enable_cache_prewarming);
        assert_eq!(config.rdf_batch_size, 10);
    });

    test!(test_leveled_generator_config_presets, {
        let fast = LeveledGeneratorConfig::fast_cicd();
        assert!(fast.heijunka.strict_timeboxing);

        let heavy = LeveledGeneratorConfig::heavy_ontology();
        assert!(heavy.heijunka.takt_time > Duration::from_secs(10));
    });

    test!(test_template_item, {
        let item = TemplateItem {
            path: PathBuf::from("test.tmpl"),
            target: "rust".to_string(),
            has_rdf: true,
            has_sparql: true,
        };

        assert_eq!(item.target, "rust");
        assert!(item.has_rdf);
        assert!(item.has_sparql);
    });

    test!(test_leveled_generation_result, {
        let result = LeveledGenerationResult {
            total_duration: Duration::from_secs(10),
            total_templates: 100,
            total_files_created: 100,
            targets: HashMap::new(),
            workload_imbalance: 0.2,
            is_balanced: true,
        };

        assert_eq!(result.throughput(), 10.0);
        assert!(result.is_balanced);
    });

    test!(test_determine_target, {
        let temp_dir = TempDir::new()?;
        let config = LeveledGeneratorConfig::default();
        let generator = LeveledGenerator::new(
            config,
            temp_dir.path().to_path_buf(),
            temp_dir.path().to_path_buf(),
        )?;

        let rust_path = PathBuf::from("templates/rust/main.rs.tmpl");
        assert_eq!(generator.determine_target(&rust_path)?, "rust");

        let ts_path = PathBuf::from("templates/typescript/index.ts.tmpl");
        assert_eq!(generator.determine_target(&ts_path)?, "typescript");

        Ok(())
    });
}
