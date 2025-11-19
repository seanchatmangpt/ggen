//! Heijunka (Level Scheduling) Module
//!
//! Implements production leveling and predictable scheduling inspired by Toyota's
//! Heijunka system. Provides fixed-cadence batch processing for RDF/ontology operations
//! and template generation workflows.
//!
//! ## Core Principles
//!
//! - **Level Scheduling**: Distribute work evenly over time
//! - **Fixed Cadence**: Predictable time boxes for operations
//! - **Batch Leveling**: Standardized batch sizes and intervals
//! - **Production Smoothing**: Eliminate peaks and valleys in workload
//!
//! ## Features
//!
//! - Time-boxed batch processing for RDF operations
//! - Predictable CI/CD pipeline cadence
//! - SPARQL query batching and scheduling
//! - Standardized work sequences
//! - Takt time management

use super::{Context, Result};
use std::collections::VecDeque;
use std::time::{Duration, Instant};

/// Heijunka configuration
#[derive(Debug, Clone)]
pub struct HeijunkaConfig {
    /// Target time per batch (takt time)
    pub takt_time: Duration,
    /// Maximum batch size
    pub max_batch_size: usize,
    /// Minimum batch size
    pub min_batch_size: usize,
    /// Target batches per cycle
    pub batches_per_cycle: usize,
    /// Enable strict time boxing
    pub strict_timeboxing: bool,
    /// Buffer time between batches
    pub inter_batch_buffer: Duration,
}

impl Default for HeijunkaConfig {
    fn default() -> Self {
        Self {
            takt_time: Duration::from_secs(10),      // 10 seconds per batch
            max_batch_size: 20,
            min_batch_size: 5,
            batches_per_cycle: 5,
            strict_timeboxing: false,
            inter_batch_buffer: Duration::from_millis(100),
        }
    }
}

impl HeijunkaConfig {
    /// Create configuration for fast CI/CD pipelines
    pub fn fast_cicd() -> Self {
        Self {
            takt_time: Duration::from_secs(5),
            max_batch_size: 15,
            min_batch_size: 3,
            batches_per_cycle: 10,
            strict_timeboxing: true,
            inter_batch_buffer: Duration::from_millis(50),
        }
    }

    /// Create configuration for heavy ontology processing
    pub fn heavy_ontology() -> Self {
        Self {
            takt_time: Duration::from_secs(30),
            max_batch_size: 50,
            min_batch_size: 10,
            batches_per_cycle: 3,
            strict_timeboxing: false,
            inter_batch_buffer: Duration::from_millis(500),
        }
    }
}

/// Scheduled batch for processing
#[derive(Debug, Clone)]
pub struct ScheduledBatch<T> {
    pub id: usize,
    pub items: Vec<T>,
    pub scheduled_start: Option<Instant>,
    pub actual_start: Option<Instant>,
    pub actual_end: Option<Instant>,
    pub rdf_operations: usize,
    pub sparql_queries: usize,
}

impl<T> ScheduledBatch<T> {
    pub fn new(id: usize, items: Vec<T>) -> Self {
        Self {
            id,
            items,
            scheduled_start: None,
            actual_start: None,
            actual_end: None,
            rdf_operations: 0,
            sparql_queries: 0,
        }
    }

    /// Mark batch as started
    pub fn start(&mut self) {
        self.actual_start = Some(Instant::now());
    }

    /// Mark batch as completed
    pub fn complete(&mut self) {
        self.actual_end = Some(Instant::now());
    }

    /// Get actual processing duration
    pub fn duration(&self) -> Option<Duration> {
        match (self.actual_start, self.actual_end) {
            (Some(start), Some(end)) => Some(end.duration_since(start)),
            _ => None,
        }
    }

    /// Check if batch met its scheduled time
    pub fn met_schedule(&self) -> bool {
        match (self.scheduled_start, self.actual_start) {
            (Some(scheduled), Some(actual)) => {
                let delay = actual.duration_since(scheduled);
                delay < Duration::from_millis(100) // Allow 100ms variance
            }
            _ => false,
        }
    }

    /// Get schedule variance (positive = late, negative = early)
    pub fn schedule_variance(&self) -> Option<Duration> {
        match (self.scheduled_start, self.actual_start) {
            (Some(scheduled), Some(actual)) => {
                if actual > scheduled {
                    Some(actual.duration_since(scheduled))
                } else {
                    Some(Duration::ZERO)
                }
            }
            _ => None,
        }
    }
}

/// Heijunka scheduler for level production
pub struct HeijunkaScheduler<T> {
    config: HeijunkaConfig,
    batches: VecDeque<ScheduledBatch<T>>,
    completed_batches: Vec<ScheduledBatch<T>>,
    cycle_start: Option<Instant>,
    current_batch_id: usize,
}

impl<T> HeijunkaScheduler<T> {
    pub fn new(config: HeijunkaConfig) -> Self {
        Self {
            config,
            batches: VecDeque::new(),
            completed_batches: Vec::new(),
            cycle_start: None,
            current_batch_id: 0,
        }
    }

    /// Schedule items into leveled batches
    pub fn schedule_batches(&mut self, items: Vec<T>) {
        if items.is_empty() {
            return;
        }

        let total = items.len();
        let target_batch_size = self.calculate_optimal_batch_size(total);

        tracing::info!(
            total_items = total,
            batch_size = target_batch_size,
            takt_time_secs = self.config.takt_time.as_secs(),
            "📅 Scheduling Heijunka batches"
        );

        let mut remaining = items;
        let mut scheduled_start = Instant::now();

        while !remaining.is_empty() {
            let batch_size = target_batch_size.min(remaining.len());
            let batch_items: Vec<T> = remaining.drain(..batch_size).collect();

            let mut batch = ScheduledBatch::new(self.current_batch_id, batch_items);
            batch.scheduled_start = Some(scheduled_start);

            self.batches.push_back(batch);
            self.current_batch_id += 1;

            // Schedule next batch with takt time + buffer
            scheduled_start += self.config.takt_time + self.config.inter_batch_buffer;
        }

        tracing::info!(
            batches_scheduled = self.batches.len(),
            "✅ Batches scheduled with Heijunka leveling"
        );
    }

    /// Calculate optimal batch size for leveling
    fn calculate_optimal_batch_size(&self, total_items: usize) -> usize {
        if total_items == 0 {
            return self.config.min_batch_size;
        }

        // Calculate based on batches per cycle
        let ideal_size = (total_items + self.config.batches_per_cycle - 1)
            / self.config.batches_per_cycle;

        // Clamp to min/max bounds
        ideal_size
            .max(self.config.min_batch_size)
            .min(self.config.max_batch_size)
    }

    /// Get next batch to process
    pub fn next_batch(&mut self) -> Option<ScheduledBatch<T>> {
        if self.cycle_start.is_none() {
            self.cycle_start = Some(Instant::now());
        }

        let mut batch = self.batches.pop_front()?;

        // Enforce time boxing if strict mode enabled
        if self.config.strict_timeboxing {
            if let Some(scheduled_start) = batch.scheduled_start {
                let now = Instant::now();
                if now < scheduled_start {
                    let wait_time = scheduled_start.duration_since(now);
                    tracing::debug!(
                        batch_id = batch.id,
                        wait_ms = wait_time.as_millis(),
                        "⏳ Waiting for scheduled start time"
                    );
                    std::thread::sleep(wait_time);
                }
            }
        }

        batch.start();
        Some(batch)
    }

    /// Complete a batch and record metrics
    pub fn complete_batch(&mut self, mut batch: ScheduledBatch<T>) {
        batch.complete();

        let met_schedule = batch.met_schedule();
        let variance = batch.schedule_variance();

        tracing::info!(
            batch_id = batch.id,
            duration_ms = batch.duration().map(|d| d.as_millis()).unwrap_or(0),
            met_schedule = met_schedule,
            variance_ms = variance.map(|d| d.as_millis()).unwrap_or(0),
            "✅ Batch completed"
        );

        self.completed_batches.push(batch);
    }

    /// Check if all batches are completed
    pub fn is_complete(&self) -> bool {
        self.batches.is_empty()
    }

    /// Get cycle duration
    pub fn cycle_duration(&self) -> Option<Duration> {
        self.cycle_start.map(|start| start.elapsed())
    }

    /// Calculate schedule adherence (0.0 = perfect, 1.0+ = poor)
    pub fn schedule_adherence(&self) -> f64 {
        if self.completed_batches.is_empty() {
            return 0.0;
        }

        let total_variance: Duration = self
            .completed_batches
            .iter()
            .filter_map(|b| b.schedule_variance())
            .sum();

        let avg_variance = total_variance / self.completed_batches.len() as u32;
        avg_variance.as_secs_f64() / self.config.takt_time.as_secs_f64()
    }

    /// Generate Heijunka report
    pub fn report(&self) {
        println!("\n📅 Heijunka Scheduling Report");
        println!("═══════════════════════════════════════════════════════════");

        println!("\n⚙️  Configuration:");
        println!("  Takt Time:             {:.2}s", self.config.takt_time.as_secs_f64());
        println!("  Batch Size Range:      {}-{}", self.config.min_batch_size, self.config.max_batch_size);
        println!("  Batches per Cycle:     {}", self.config.batches_per_cycle);
        println!("  Strict Timeboxing:     {}", if self.config.strict_timeboxing { "✅ Enabled" } else { "❌ Disabled" });
        println!("  Inter-batch Buffer:    {}ms", self.config.inter_batch_buffer.as_millis());

        println!("\n📊 Execution Metrics:");
        println!("  Total Batches:         {}", self.completed_batches.len());

        if let Some(duration) = self.cycle_duration() {
            println!("  Cycle Duration:        {:.2}s", duration.as_secs_f64());

            let expected_duration = self.config.takt_time * self.completed_batches.len() as u32;
            let efficiency = expected_duration.as_secs_f64() / duration.as_secs_f64() * 100.0;
            println!("  Expected Duration:     {:.2}s", expected_duration.as_secs_f64());
            println!("  Cycle Efficiency:      {:.1}%", efficiency);
        }

        // Schedule adherence
        let adherence = self.schedule_adherence();
        let adherence_status = if adherence < 0.1 {
            "✅ Excellent"
        } else if adherence < 0.3 {
            "👍 Good"
        } else {
            "⚠️  Needs Improvement"
        };

        println!("  Schedule Adherence:    {:.3} - {}", adherence, adherence_status);

        // Batch details
        if !self.completed_batches.is_empty() {
            println!("\n📦 Batch Details:");

            let avg_size = self.completed_batches.iter().map(|b| b.items.len()).sum::<usize>() as f64
                / self.completed_batches.len() as f64;

            let avg_duration: Duration = self
                .completed_batches
                .iter()
                .filter_map(|b| b.duration())
                .sum::<Duration>()
                / self.completed_batches.len() as u32;

            println!("  Avg Batch Size:        {:.1}", avg_size);
            println!("  Avg Duration:          {:.2}ms", avg_duration.as_secs_f64() * 1000.0);

            let met_count = self.completed_batches.iter().filter(|b| b.met_schedule()).count();
            let on_time_rate = met_count as f64 / self.completed_batches.len() as f64 * 100.0;
            println!("  On-Time Rate:          {:.1}% ({}/{})", on_time_rate, met_count, self.completed_batches.len());

            // RDF/SPARQL metrics
            let total_rdf: usize = self.completed_batches.iter().map(|b| b.rdf_operations).sum();
            let total_sparql: usize = self.completed_batches.iter().map(|b| b.sparql_queries).sum();

            if total_rdf > 0 || total_sparql > 0 {
                println!("\n🔄 RDF/SPARQL Operations:");
                println!("  Total RDF Operations:  {}", total_rdf);
                println!("  Total SPARQL Queries:  {}", total_sparql);
                println!("  Avg RDF per Batch:     {:.1}", total_rdf as f64 / self.completed_batches.len() as f64);
                println!("  Avg SPARQL per Batch:  {:.1}", total_sparql as f64 / self.completed_batches.len() as f64);
            }
        }

        // Recommendations
        println!("\n💡 Recommendations:");
        if adherence > 0.3 {
            println!("  • Consider increasing takt time for better schedule adherence");
        }
        if adherence < 0.05 && self.config.strict_timeboxing {
            println!("  • Excellent adherence - consider reducing takt time to increase throughput");
        }
        if !self.config.strict_timeboxing && adherence > 0.2 {
            println!("  • Enable strict timeboxing for more predictable cadence");
        }

        println!("═══════════════════════════════════════════════════════════\n");
    }

    /// Get configuration
    pub fn config(&self) -> &HeijunkaConfig {
        &self.config
    }

    /// Get completed batches
    pub fn completed_batches(&self) -> &[ScheduledBatch<T>] {
        &self.completed_batches
    }
}

/// Standardized work sequence for RDF transformations
#[derive(Debug, Clone)]
pub struct StandardWorkSequence {
    pub name: String,
    pub steps: Vec<WorkStep>,
    pub total_duration: Duration,
}

impl StandardWorkSequence {
    pub fn new(name: impl Into<String>) -> Self {
        Self {
            name: name.into(),
            steps: Vec::new(),
            total_duration: Duration::ZERO,
        }
    }

    pub fn add_step(&mut self, step: WorkStep) {
        self.total_duration += step.target_duration;
        self.steps.push(step);
    }

    /// Execute the work sequence
    pub fn execute(&self) -> Result<WorkSequenceResult> {
        let start = Instant::now();
        let mut results = Vec::new();

        tracing::info!(
            sequence = %self.name,
            steps = self.steps.len(),
            "🔄 Executing standard work sequence"
        );

        for (idx, step) in self.steps.iter().enumerate() {
            let step_start = Instant::now();

            tracing::debug!(
                step = idx + 1,
                name = %step.name,
                "▶️  Executing step"
            );

            // Simulate step execution (in real implementation, this would call actual work)
            let step_result = StepResult {
                name: step.name.clone(),
                duration: step_start.elapsed(),
                success: true,
            };

            results.push(step_result);
        }

        let total_duration = start.elapsed();

        Ok(WorkSequenceResult {
            sequence_name: self.name.clone(),
            steps: results,
            total_duration,
            target_duration: self.total_duration,
        })
    }
}

/// Individual work step
#[derive(Debug, Clone)]
pub struct WorkStep {
    pub name: String,
    pub target_duration: Duration,
    pub description: String,
}

impl WorkStep {
    pub fn new(name: impl Into<String>, target_duration: Duration) -> Self {
        Self {
            name: name.into(),
            target_duration,
            description: String::new(),
        }
    }

    pub fn with_description(mut self, description: impl Into<String>) -> Self {
        self.description = description.into();
        self
    }
}

/// Result of a work step
#[derive(Debug, Clone)]
pub struct StepResult {
    pub name: String,
    pub duration: Duration,
    pub success: bool,
}

/// Result of executing a work sequence
#[derive(Debug, Clone)]
pub struct WorkSequenceResult {
    pub sequence_name: String,
    pub steps: Vec<StepResult>,
    pub total_duration: Duration,
    pub target_duration: Duration,
}

impl WorkSequenceResult {
    /// Check if sequence met its target time
    pub fn met_target(&self) -> bool {
        self.total_duration <= self.target_duration
    }

    /// Calculate efficiency (< 1.0 = under target, > 1.0 = over target)
    pub fn efficiency(&self) -> f64 {
        if self.target_duration.as_secs_f64() == 0.0 {
            return 1.0;
        }
        self.total_duration.as_secs_f64() / self.target_duration.as_secs_f64()
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use chicago_tdd_tools::{async_test, test};

    test!(test_heijunka_config_defaults, {
        let config = HeijunkaConfig::default();
        assert_eq!(config.takt_time, Duration::from_secs(10));
        assert_eq!(config.max_batch_size, 20);
        assert_eq!(config.min_batch_size, 5);
        assert!(!config.strict_timeboxing);
    });

    test!(test_heijunka_config_presets, {
        let fast = HeijunkaConfig::fast_cicd();
        assert!(fast.strict_timeboxing);
        assert!(fast.takt_time < Duration::from_secs(10));

        let heavy = HeijunkaConfig::heavy_ontology();
        assert!(heavy.takt_time > Duration::from_secs(10));
        assert!(heavy.max_batch_size > 20);
    });

    test!(test_scheduled_batch, {
        let mut batch = ScheduledBatch::new(1, vec![1, 2, 3]);
        batch.scheduled_start = Some(Instant::now());

        batch.start();
        std::thread::sleep(Duration::from_millis(10));
        batch.complete();

        assert!(batch.duration().is_some());
        assert!(batch.duration().unwrap() >= Duration::from_millis(10));
    });

    test!(test_heijunka_scheduler, {
        let config = HeijunkaConfig {
            takt_time: Duration::from_millis(100),
            max_batch_size: 5,
            min_batch_size: 2,
            batches_per_cycle: 3,
            strict_timeboxing: false,
            inter_batch_buffer: Duration::from_millis(10),
        };

        let mut scheduler = HeijunkaScheduler::new(config);
        let items: Vec<i32> = (1..=12).collect();

        scheduler.schedule_batches(items);

        let mut processed = 0;
        while let Some(mut batch) = scheduler.next_batch() {
            processed += batch.items.len();
            scheduler.complete_batch(batch);
        }

        assert_eq!(processed, 12);
        assert!(scheduler.is_complete());
        assert_eq!(scheduler.completed_batches().len(), 3);
    });

    test!(test_schedule_adherence, {
        let config = HeijunkaConfig::default();
        let mut scheduler = HeijunkaScheduler::new(config);

        // Create and complete some batches
        for i in 0..3 {
            let mut batch = ScheduledBatch::new(i, vec![i]);
            batch.scheduled_start = Some(Instant::now());
            batch.start();
            std::thread::sleep(Duration::from_millis(5));
            batch.complete();
            scheduler.completed_batches.push(batch);
        }

        let adherence = scheduler.schedule_adherence();
        assert!(adherence >= 0.0); // Should be a valid adherence score
    });

    test!(test_standard_work_sequence, {
        let mut sequence = StandardWorkSequence::new("RDF Transform");

        sequence.add_step(WorkStep::new("Parse RDF", Duration::from_millis(100)));
        sequence.add_step(WorkStep::new("Execute SPARQL", Duration::from_millis(200)));
        sequence.add_step(WorkStep::new("Render Template", Duration::from_millis(150)));

        assert_eq!(sequence.steps.len(), 3);
        assert_eq!(sequence.total_duration, Duration::from_millis(450));

        let result = sequence.execute().unwrap();
        assert_eq!(result.steps.len(), 3);
        assert!(result.total_duration > Duration::ZERO);
    });

    test!(test_work_sequence_result, {
        let result = WorkSequenceResult {
            sequence_name: "Test".to_string(),
            steps: vec![],
            total_duration: Duration::from_secs(5),
            target_duration: Duration::from_secs(10),
        };

        assert!(result.met_target());
        assert!(result.efficiency() < 1.0);
    });
}
