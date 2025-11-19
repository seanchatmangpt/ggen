//! Mura (Unevenness) Leveling Module
//!
//! Implements production smoothing and workload leveling strategies inspired by
//! Toyota Production System's Mura reduction principles. Provides predictable,
//! balanced workload distribution across polyglot template generation targets.
//!
//! ## Core Principles
//!
//! - **Mura Reduction**: Eliminate unevenness in workload distribution
//! - **Smooth Batching**: Even distribution of work across time and resources
//! - **Predictable Cadence**: Consistent rhythm in generation pipelines
//! - **Variability Tracking**: Monitor and reduce latency fluctuations
//!
//! ## Features
//!
//! - Workload balancing across language targets (Rust, TypeScript, JavaScript, etc.)
//! - Batch size leveling for RDF/ontology operations
//! - SPARQL query distribution smoothing
//! - Generation latency variability reduction
//! - Real-time workload metrics and monitoring

use super::{Context, Result};
use std::collections::HashMap;
use std::time::{Duration, Instant};

/// Workload metrics for a single target
#[derive(Debug, Clone, Default)]
pub struct TargetWorkload {
    /// Target name (e.g., "rust", "typescript", "javascript")
    pub name: String,
    /// Number of templates assigned to this target
    pub template_count: usize,
    /// Total processing time
    pub total_duration: Duration,
    /// Average processing time per template
    pub avg_duration: Duration,
    /// Peak processing time for a single template
    pub peak_duration: Duration,
    /// Standard deviation of processing times
    pub std_deviation: f64,
    /// Number of RDF operations
    pub rdf_operations: usize,
    /// Number of SPARQL queries
    pub sparql_queries: usize,
}

impl TargetWorkload {
    pub fn new(name: impl Into<String>) -> Self {
        Self {
            name: name.into(),
            ..Default::default()
        }
    }

    /// Calculate throughput (templates per second)
    pub fn throughput(&self) -> f64 {
        if self.total_duration.as_secs_f64() == 0.0 {
            return 0.0;
        }
        self.template_count as f64 / self.total_duration.as_secs_f64()
    }

    /// Calculate coefficient of variation (CV) - measure of relative variability
    pub fn coefficient_of_variation(&self) -> f64 {
        if self.avg_duration.as_secs_f64() == 0.0 {
            return 0.0;
        }
        self.std_deviation / self.avg_duration.as_secs_f64()
    }

    /// Check if workload is balanced (low variability)
    pub fn is_balanced(&self) -> bool {
        // CV < 0.5 indicates good balance (less than 50% relative variation)
        self.coefficient_of_variation() < 0.5
    }
}

/// Mura leveling configuration
#[derive(Debug, Clone)]
pub struct MuraConfig {
    /// Target batch size for template processing
    pub target_batch_size: usize,
    /// Maximum batch size variance (percentage)
    pub max_batch_variance: f64,
    /// Target coefficient of variation for balanced workload
    pub target_cv: f64,
    /// Enable automatic batch size adjustment
    pub auto_adjust_batches: bool,
    /// Minimum templates per target for balancing
    pub min_templates_per_target: usize,
}

impl Default for MuraConfig {
    fn default() -> Self {
        Self {
            target_batch_size: 10,
            max_batch_variance: 0.2, // 20% variance allowed
            target_cv: 0.3,          // Target CV of 0.3 (30% relative variation)
            auto_adjust_batches: true,
            min_templates_per_target: 3,
        }
    }
}

/// Workload leveling system for smooth distribution
pub struct WorkloadLeveler {
    config: MuraConfig,
    target_workloads: HashMap<String, TargetWorkload>,
    batch_metrics: Vec<BatchMetrics>,
    total_start: Option<Instant>,
}

impl WorkloadLeveler {
    pub fn new(config: MuraConfig) -> Self {
        Self {
            config,
            target_workloads: HashMap::new(),
            batch_metrics: Vec::new(),
            total_start: None,
        }
    }

    /// Start tracking overall workload
    pub fn start(&mut self) {
        self.total_start = Some(Instant::now());
    }

    /// Record workload for a target
    pub fn record_target_workload(&mut self, workload: TargetWorkload) {
        tracing::info!(
            target = %workload.name,
            templates = workload.template_count,
            duration_ms = workload.total_duration.as_millis(),
            throughput = %format!("{:.2}", workload.throughput()),
            cv = %format!("{:.3}", workload.coefficient_of_variation()),
            balanced = workload.is_balanced(),
            "📊 Target workload recorded"
        );

        self.target_workloads
            .insert(workload.name.clone(), workload);
    }

    /// Record batch processing metrics
    pub fn record_batch(&mut self, metrics: BatchMetrics) {
        tracing::debug!(
            batch_id = metrics.batch_id,
            size = metrics.size,
            duration_ms = metrics.duration.as_millis(),
            "Batch processed"
        );

        self.batch_metrics.push(metrics);
    }

    /// Calculate optimal batch sizes for leveling
    pub fn calculate_optimal_batch_sizes(&self, total_items: usize) -> Vec<usize> {
        if total_items == 0 {
            return vec![];
        }

        let target_size = self.config.target_batch_size;
        let num_batches = (total_items + target_size - 1) / target_size;

        // Level the batch sizes to minimize variance
        let base_size = total_items / num_batches;
        let remainder = total_items % num_batches;

        let mut batch_sizes = vec![base_size; num_batches];

        // Distribute remainder evenly across batches (Heijunka principle)
        for i in 0..remainder {
            batch_sizes[i] += 1;
        }

        tracing::info!(
            total_items = total_items,
            num_batches = num_batches,
            base_size = base_size,
            variance = %format!("{:.2}%", self.calculate_batch_variance(&batch_sizes) * 100.0),
            "📦 Calculated optimal batch sizes"
        );

        batch_sizes
    }

    /// Calculate batch size variance
    fn calculate_batch_variance(&self, batch_sizes: &[usize]) -> f64 {
        if batch_sizes.is_empty() {
            return 0.0;
        }

        let mean = batch_sizes.iter().sum::<usize>() as f64 / batch_sizes.len() as f64;
        let variance: f64 = batch_sizes
            .iter()
            .map(|&size| {
                let diff = size as f64 - mean;
                diff * diff
            })
            .sum::<f64>()
            / batch_sizes.len() as f64;

        variance.sqrt() / mean // Coefficient of variation
    }

    /// Distribute templates across targets for balanced workload
    pub fn distribute_templates<T: Clone>(
        &self, items: Vec<T>, targets: &[String],
    ) -> HashMap<String, Vec<T>> {
        if items.is_empty() || targets.is_empty() {
            return HashMap::new();
        }

        let mut distribution: HashMap<String, Vec<T>> = HashMap::new();
        for target in targets {
            distribution.insert(target.clone(), Vec::new());
        }

        // Round-robin distribution for even workload (Heijunka leveling)
        for (idx, item) in items.into_iter().enumerate() {
            let target_idx = idx % targets.len();
            let target = &targets[target_idx];
            distribution.get_mut(target).unwrap().push(item);
        }

        // Log distribution
        for (target, items) in &distribution {
            tracing::debug!(
                target = %target,
                count = items.len(),
                "Template distribution"
            );
        }

        distribution
    }

    /// Get workload imbalance score (0.0 = perfect balance, 1.0+ = high imbalance)
    pub fn calculate_imbalance(&self) -> f64 {
        if self.target_workloads.is_empty() {
            return 0.0;
        }

        let workloads: Vec<f64> = self
            .target_workloads
            .values()
            .map(|w| w.template_count as f64)
            .collect();

        let mean = workloads.iter().sum::<f64>() / workloads.len() as f64;
        if mean == 0.0 {
            return 0.0;
        }

        let variance: f64 = workloads
            .iter()
            .map(|&count| {
                let diff = count - mean;
                diff * diff
            })
            .sum::<f64>()
            / workloads.len() as f64;

        variance.sqrt() / mean // Coefficient of variation
    }

    /// Check if workload is balanced across targets
    pub fn is_balanced(&self) -> bool {
        self.calculate_imbalance() <= self.config.target_cv
    }

    /// Generate leveling report
    pub fn report(&self) {
        println!("\n📊 Mura Leveling Report");
        println!("═══════════════════════════════════════════════════════════");

        // Overall metrics
        let total_templates: usize = self
            .target_workloads
            .values()
            .map(|w| w.template_count)
            .sum();
        let total_rdf_ops: usize = self
            .target_workloads
            .values()
            .map(|w| w.rdf_operations)
            .sum();
        let total_sparql: usize = self
            .target_workloads
            .values()
            .map(|w| w.sparql_queries)
            .sum();

        let imbalance = self.calculate_imbalance();
        let balanced = self.is_balanced();

        println!("\n📈 Overall Metrics:");
        println!("  Total Templates:       {}", total_templates);
        println!("  Total RDF Operations:  {}", total_rdf_ops);
        println!("  Total SPARQL Queries:  {}", total_sparql);
        println!("  Workload Imbalance:    {:.3} (CV)", imbalance);
        println!(
            "  Status:                {}",
            if balanced {
                "✅ Balanced"
            } else {
                "⚠️  Unbalanced"
            }
        );

        if let Some(start) = self.total_start {
            let total_duration = start.elapsed();
            let throughput = total_templates as f64 / total_duration.as_secs_f64();
            println!("  Total Duration:        {:.2}s", total_duration.as_secs_f64());
            println!("  Overall Throughput:    {:.2} templates/sec", throughput);
        }

        // Target-specific metrics
        println!("\n🎯 Target Distribution:");
        let mut targets: Vec<_> = self.target_workloads.values().collect();
        targets.sort_by(|a, b| b.template_count.cmp(&a.template_count));

        for workload in targets {
            let balance_indicator = if workload.is_balanced() {
                "✅"
            } else {
                "⚠️ "
            };
            println!("  {} {:12} │ Templates: {:4} │ Avg: {:6.2}ms │ Peak: {:6.2}ms │ CV: {:.3} │ Throughput: {:.2}/s",
                balance_indicator,
                workload.name,
                workload.template_count,
                workload.avg_duration.as_secs_f64() * 1000.0,
                workload.peak_duration.as_secs_f64() * 1000.0,
                workload.coefficient_of_variation(),
                workload.throughput()
            );
        }

        // Batch metrics
        if !self.batch_metrics.is_empty() {
            println!("\n📦 Batch Processing:");
            let avg_batch_size = self.batch_metrics.iter().map(|b| b.size).sum::<usize>() as f64
                / self.batch_metrics.len() as f64;
            let avg_batch_duration: Duration = self
                .batch_metrics
                .iter()
                .map(|b| b.duration)
                .sum::<Duration>()
                / self.batch_metrics.len() as u32;

            println!("  Total Batches:         {}", self.batch_metrics.len());
            println!("  Avg Batch Size:        {:.1}", avg_batch_size);
            println!(
                "  Avg Batch Duration:    {:.2}ms",
                avg_batch_duration.as_secs_f64() * 1000.0
            );

            let batch_sizes: Vec<usize> = self.batch_metrics.iter().map(|b| b.size).collect();
            let batch_variance = self.calculate_batch_variance(&batch_sizes);
            println!("  Batch Size Variance:   {:.2}%", batch_variance * 100.0);

            if batch_variance <= self.config.max_batch_variance {
                println!("  Batch Balance:         ✅ Leveled");
            } else {
                println!("  Batch Balance:         ⚠️  Uneven");
            }
        }

        println!("\n💡 Recommendations:");
        if !balanced {
            println!("  • Consider redistributing templates for better balance");
            println!(
                "  • Target CV: {:.3}, Current: {:.3}",
                self.config.target_cv, imbalance
            );
        }

        let unbalanced_targets: Vec<_> = self
            .target_workloads
            .values()
            .filter(|w| !w.is_balanced())
            .collect();

        if !unbalanced_targets.is_empty() {
            println!("  • High variability detected in:");
            for target in unbalanced_targets {
                println!(
                    "    - {} (CV: {:.3})",
                    target.name,
                    target.coefficient_of_variation()
                );
            }
        }

        if self.batch_metrics.is_empty() {
            println!("  • Consider enabling batch processing for RDF operations");
        }

        println!("═══════════════════════════════════════════════════════════\n");
    }

    /// Get current configuration
    pub fn config(&self) -> &MuraConfig {
        &self.config
    }

    /// Get all target workloads
    pub fn workloads(&self) -> &HashMap<String, TargetWorkload> {
        &self.target_workloads
    }
}

/// Metrics for a single batch
#[derive(Debug, Clone)]
pub struct BatchMetrics {
    pub batch_id: usize,
    pub size: usize,
    pub duration: Duration,
    pub rdf_operations: usize,
    pub sparql_queries: usize,
}

impl BatchMetrics {
    pub fn new(batch_id: usize, size: usize) -> Self {
        Self {
            batch_id,
            size,
            duration: Duration::default(),
            rdf_operations: 0,
            sparql_queries: 0,
        }
    }

    pub fn with_duration(mut self, duration: Duration) -> Self {
        self.duration = duration;
        self
    }

    pub fn with_rdf_operations(mut self, count: usize) -> Self {
        self.rdf_operations = count;
        self
    }

    pub fn with_sparql_queries(mut self, count: usize) -> Self {
        self.sparql_queries = count;
        self
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use chicago_tdd_tools::{async_test, test};

    test!(test_target_workload, {
        let mut workload = TargetWorkload::new("rust");
        workload.template_count = 10;
        workload.total_duration = Duration::from_secs(5);
        workload.avg_duration = Duration::from_millis(500);
        workload.std_deviation = 0.1;

        assert_eq!(workload.throughput(), 2.0); // 10 templates / 5 seconds
        assert!(workload.coefficient_of_variation() < 0.3);
        assert!(workload.is_balanced());
    });

    test!(test_mura_config_defaults, {
        let config = MuraConfig::default();
        assert_eq!(config.target_batch_size, 10);
        assert_eq!(config.max_batch_variance, 0.2);
        assert_eq!(config.target_cv, 0.3);
        assert!(config.auto_adjust_batches);
    });

    test!(test_calculate_optimal_batch_sizes, {
        let config = MuraConfig {
            target_batch_size: 5,
            ..Default::default()
        };
        let leveler = WorkloadLeveler::new(config);

        // 23 items with target batch size of 5 -> 5 batches
        let batch_sizes = leveler.calculate_optimal_batch_sizes(23);
        assert_eq!(batch_sizes.len(), 5);
        assert_eq!(batch_sizes.iter().sum::<usize>(), 23);

        // Should be leveled: [5, 5, 5, 4, 4] or similar
        let max = *batch_sizes.iter().max().unwrap();
        let min = *batch_sizes.iter().min().unwrap();
        assert!(max - min <= 1); // Maximum difference of 1
    });

    test!(test_distribute_templates, {
        let leveler = WorkloadLeveler::new(MuraConfig::default());
        let items = vec![1, 2, 3, 4, 5, 6, 7, 8, 9, 10];
        let targets = vec!["rust".to_string(), "typescript".to_string(), "javascript".to_string()];

        let distribution = leveler.distribute_templates(items, &targets);

        assert_eq!(distribution.len(), 3);
        // Round-robin should give: rust=4, typescript=3, javascript=3 (or similar)
        for (target, items) in &distribution {
            assert!(items.len() >= 3 && items.len() <= 4, "Target {} has {} items", target, items.len());
        }
    });

    test!(test_workload_imbalance, {
        let mut leveler = WorkloadLeveler::new(MuraConfig::default());

        // Balanced workload
        let mut w1 = TargetWorkload::new("rust");
        w1.template_count = 10;
        leveler.record_target_workload(w1);

        let mut w2 = TargetWorkload::new("typescript");
        w2.template_count = 10;
        leveler.record_target_workload(w2);

        let mut w3 = TargetWorkload::new("javascript");
        w3.template_count = 10;
        leveler.record_target_workload(w3);

        let imbalance = leveler.calculate_imbalance();
        assert!(imbalance < 0.01); // Perfect balance
        assert!(leveler.is_balanced());
    });

    test!(test_workload_imbalance_unbalanced, {
        let mut leveler = WorkloadLeveler::new(MuraConfig::default());

        let mut w1 = TargetWorkload::new("rust");
        w1.template_count = 20;
        leveler.record_target_workload(w1);

        let mut w2 = TargetWorkload::new("typescript");
        w2.template_count = 5;
        leveler.record_target_workload(w2);

        let imbalance = leveler.calculate_imbalance();
        assert!(imbalance > 0.3); // Significant imbalance
    });

    test!(test_batch_metrics, {
        let batch = BatchMetrics::new(1, 10)
            .with_duration(Duration::from_secs(2))
            .with_rdf_operations(5)
            .with_sparql_queries(3);

        assert_eq!(batch.batch_id, 1);
        assert_eq!(batch.size, 10);
        assert_eq!(batch.duration, Duration::from_secs(2));
        assert_eq!(batch.rdf_operations, 5);
        assert_eq!(batch.sparql_queries, 3);
    });
}
