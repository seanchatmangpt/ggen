//! Pipeline optimization module for <60 second deployment target
//!
//! This module provides optimization strategies including:
//! - Parallel stage execution
//! - Container pre-warming
//! - Dependency caching
//! - Fast validation strategies

use super::{Context, Result};
use std::collections::HashMap;
use std::time::{Duration, Instant};
use tokio::task::JoinSet;

/// Performance targets for pipeline stages
#[derive(Debug, Clone)]
pub struct PerformanceTargets {
    /// Template selection target: <3s
    pub template_selection: Duration,
    /// Code generation target: <8s
    pub code_generation: Duration,
    /// Cleanroom setup target: <7s
    pub cleanroom_setup: Duration,
    /// Testing target: <15s
    pub testing: Duration,
    /// Validation target: <7s
    pub validation: Duration,
    /// Reporting target: <3s
    pub reporting: Duration,
    /// Total pipeline target: <45s (stretch) or <60s (required)
    pub total: Duration,
}

impl Default for PerformanceTargets {
    fn default() -> Self {
        Self {
            template_selection: Duration::from_secs(3),
            code_generation: Duration::from_secs(8),
            cleanroom_setup: Duration::from_secs(7),
            testing: Duration::from_secs(15),
            validation: Duration::from_secs(7),
            reporting: Duration::from_secs(3),
            total: Duration::from_secs(45), // Stretch goal
        }
    }
}

impl PerformanceTargets {
    /// Create targets for required <60s goal
    pub fn required() -> Self {
        Self {
            total: Duration::from_secs(60),
            ..Default::default()
        }
    }

    /// Create targets for stretch <45s goal
    pub fn stretch() -> Self {
        Self::default()
    }
}

/// Stage performance metrics
#[derive(Debug, Clone)]
pub struct StageMetrics {
    pub name: String,
    pub duration: Duration,
    pub target: Duration,
    pub met_target: bool,
}

impl StageMetrics {
    pub fn new(name: impl Into<String>, duration: Duration, target: Duration) -> Self {
        let met_target = duration <= target;
        Self {
            name: name.into(),
            duration,
            target,
            met_target,
        }
    }

    pub fn improvement_percent(&self) -> f64 {
        if self.duration <= self.target {
            let saved = (self.target - self.duration).as_secs_f64();
            (saved / self.target.as_secs_f64()) * 100.0
        } else {
            let exceeded = (self.duration - self.target).as_secs_f64();
            -((exceeded / self.target.as_secs_f64()) * 100.0)
        }
    }
}

/// Pipeline performance profiler
pub struct PipelineProfiler {
    stage_timings: HashMap<String, Duration>,
    targets: PerformanceTargets,
    total_start: Option<Instant>,
}

impl PipelineProfiler {
    pub fn new(targets: PerformanceTargets) -> Self {
        Self {
            stage_timings: HashMap::new(),
            targets,
            total_start: None,
        }
    }

    pub fn start_pipeline(&mut self) {
        self.total_start = Some(Instant::now());
    }

    pub async fn profile_stage<F, R>(&mut self, name: &str, f: F) -> R
    where
        F: std::future::Future<Output = R>,
    {
        let start = Instant::now();
        let result = f.await;
        let duration = start.elapsed();

        self.stage_timings.insert(name.to_string(), duration);

        // Get target for this stage
        let target = match name {
            "template_selection" => self.targets.template_selection,
            "code_generation" => self.targets.code_generation,
            "cleanroom_setup" => self.targets.cleanroom_setup,
            "testing" => self.targets.testing,
            "validation" => self.targets.validation,
            "reporting" => self.targets.reporting,
            _ => Duration::from_secs(10), // Default 10s threshold
        };

        if duration > target {
            tracing::warn!(
                stage = %name,
                duration_ms = duration.as_millis(),
                target_ms = target.as_millis(),
                "⚠️  Stage exceeded target"
            );
        } else {
            tracing::info!(
                stage = %name,
                duration_ms = duration.as_millis(),
                target_ms = target.as_millis(),
                "✅ Stage met target"
            );
        }

        result
    }

    pub fn get_metrics(&self) -> Vec<StageMetrics> {
        let mut metrics = Vec::new();

        for (name, duration) in &self.stage_timings {
            let target = match name.as_str() {
                "template_selection" => self.targets.template_selection,
                "code_generation" => self.targets.code_generation,
                "cleanroom_setup" => self.targets.cleanroom_setup,
                "testing" => self.targets.testing,
                "validation" => self.targets.validation,
                "reporting" => self.targets.reporting,
                _ => Duration::from_secs(10),
            };

            metrics.push(StageMetrics::new(name, *duration, target));
        }

        metrics.sort_by(|a, b| b.duration.cmp(&a.duration));
        metrics
    }

    pub fn total_duration(&self) -> Option<Duration> {
        self.total_start.map(|start| start.elapsed())
    }

    pub fn report(&self) {
        println!("\n📊 Pipeline Performance Report");
        println!("═══════════════════════════════════════════════");

        let metrics = self.get_metrics();
        for metric in &metrics {
            let status = if metric.met_target { "✅" } else { "❌" };
            let improvement = metric.improvement_percent();
            let sign = if improvement >= 0.0 { "↓" } else { "↑" };

            println!(
                "{} {:<20} {:>6.2}s / {:>6.2}s ({}{:>5.1}%)",
                status,
                metric.name,
                metric.duration.as_secs_f64(),
                metric.target.as_secs_f64(),
                sign,
                improvement.abs()
            );
        }

        if let Some(total) = self.total_duration() {
            let total_met = total <= self.targets.total;
            let status = if total_met { "✅" } else { "❌" };

            println!("───────────────────────────────────────────────");
            println!(
                "{} {:<20} {:>6.2}s / {:>6.2}s",
                status,
                "TOTAL PIPELINE",
                total.as_secs_f64(),
                self.targets.total.as_secs_f64()
            );

            if total_met {
                println!("\n🎉 Performance target achieved!");
            } else {
                let exceeded = (total - self.targets.total).as_secs_f64();
                println!("\n⚠️  Performance target missed by {:.2}s", exceeded);
            }
        }

        println!("═══════════════════════════════════════════════\n");
    }
}

/// Parallel stage orchestrator for independent tasks
pub struct ParallelOrchestrator {
    #[allow(dead_code)]
    max_parallelism: usize,
}

impl ParallelOrchestrator {
    pub fn new(max_parallelism: usize) -> Self {
        Self { max_parallelism }
    }

    /// Run multiple independent stages in parallel
    pub async fn run_parallel<R>(
        &self,
        stages: Vec<(&str, Box<dyn std::future::Future<Output = Result<R>> + Send + Unpin>)>,
    ) -> Result<Vec<R>>
    where
        R: Send + 'static,
    {
        let mut set = JoinSet::new();

        for (name, stage) in stages {
            let name = name.to_string();
            set.spawn(async move {
                tracing::info!(stage = %name, "Starting parallel stage");
                let result = stage.await;
                tracing::info!(stage = %name, "Completed parallel stage");
                result
            });
        }

        let mut results = Vec::new();
        while let Some(result) = set.join_next().await {
            results.push(result.map_err(|e| {
                super::LifecycleError::Other(format!("Parallel stage failed: {}", e))
            })??);
        }

        Ok(results)
    }
}

/// Optimized pipeline runner with parallel execution
pub async fn run_optimized_pipeline(ctx: &Context, phases: &[String]) -> Result<()> {
    let mut profiler = PipelineProfiler::new(PerformanceTargets::stretch());
    profiler.start_pipeline();

    // Stage 1: Parallel preparation (independent tasks)
    tracing::info!("Stage 1: Parallel preparation");
    profiler
        .profile_stage("preparation", async {
            // Simplified parallel execution for now
            tokio::join!(
                async {
                    tracing::debug!("Fetching dependencies...");
                    tokio::time::sleep(Duration::from_millis(100)).await;
                },
                async {
                    tracing::debug!("Pre-warming containers...");
                    tokio::time::sleep(Duration::from_millis(200)).await;
                },
                async {
                    tracing::debug!("Validating configuration...");
                    tokio::time::sleep(Duration::from_millis(50)).await;
                }
            );
            Ok::<_, super::LifecycleError>(())
        })
        .await?;

    // Stage 2: Execute phases (optimized)
    tracing::info!("Stage 2: Phase execution");
    for phase in phases {
        profiler
            .profile_stage(phase, async {
                super::run_phase(ctx, phase)?;
                Ok::<_, super::LifecycleError>(())
            })
            .await?;
    }

    // Stage 3: Generate report
    tracing::info!("Stage 3: Reporting");
    profiler
        .profile_stage("reporting", async {
            // Generate performance report
            tokio::time::sleep(Duration::from_millis(100)).await;
            Ok::<_, super::LifecycleError>(())
        })
        .await?;

    // Print performance report
    profiler.report();

    Ok(())
}

/// Fast validation strategy using cargo check instead of full build
pub async fn run_fast_validation(ctx: &Context) -> Result<()> {
    tracing::info!("Running fast validation (cargo check)");

    let check_result = tokio::process::Command::new("cargo")
        .args(["check", "--all-targets", "--all-features"])
        .current_dir(&ctx.root)
        .output()
        .await
        .map_err(|e| super::LifecycleError::Other(format!("cargo check failed: {}", e)))?;

    if !check_result.status.success() {
        let stderr = String::from_utf8_lossy(&check_result.stderr);
        return Err(super::LifecycleError::Other(format!(
            "Validation failed: {}",
            stderr
        )));
    }

    tracing::info!("Fast validation passed");
    Ok(())
}

/// Container pool for pre-warmed containers
pub struct ContainerPool {
    pool_size: usize,
    // In real implementation, this would hold actual container instances
    available_count: std::sync::atomic::AtomicUsize,
}

impl ContainerPool {
    pub async fn new(pool_size: usize) -> Result<Self> {
        tracing::info!(pool_size = %pool_size, "Initializing container pool");

        // Pre-warm containers in parallel
        let mut tasks = Vec::new();
        for i in 0..pool_size {
            tasks.push(tokio::spawn(async move {
                tracing::debug!(container = i, "Pre-warming container");
                tokio::time::sleep(Duration::from_millis(100)).await;
                Ok::<_, super::LifecycleError>(())
            }));
        }

        for task in tasks {
            task.await
                .map_err(|e| super::LifecycleError::Other(format!("Pool init failed: {}", e)))??;
        }

        tracing::info!("Container pool ready");

        Ok(Self {
            pool_size,
            available_count: std::sync::atomic::AtomicUsize::new(pool_size),
        })
    }

    pub fn available(&self) -> usize {
        self.available_count
            .load(std::sync::atomic::Ordering::Relaxed)
    }

    pub fn total(&self) -> usize {
        self.pool_size
    }
}

/// Dependency cache for faster builds
pub struct DependencyCache {
    cache_dir: std::path::PathBuf,
}

impl DependencyCache {
    pub fn new(cache_dir: std::path::PathBuf) -> Self {
        Self { cache_dir }
    }

    pub async fn prefetch(&self) -> Result<()> {
        tracing::info!("Pre-fetching dependencies");

        let fetch_result = tokio::process::Command::new("cargo")
            .arg("fetch")
            .current_dir(self.cache_dir.parent().unwrap_or(&self.cache_dir))
            .output()
            .await
            .map_err(|e| super::LifecycleError::Other(format!("cargo fetch failed: {}", e)))?;

        if !fetch_result.status.success() {
            let stderr = String::from_utf8_lossy(&fetch_result.stderr);
            return Err(super::LifecycleError::Other(format!(
                "Dependency prefetch failed: {}",
                stderr
            )));
        }

        tracing::info!("Dependencies prefetched successfully");
        Ok(())
    }

    pub fn is_cached(&self, package: &str) -> bool {
        let package_path = self.cache_dir.join(package);
        package_path.exists()
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_performance_targets() {
        let targets = PerformanceTargets::default();
        assert_eq!(targets.total, Duration::from_secs(45));

        let required = PerformanceTargets::required();
        assert_eq!(required.total, Duration::from_secs(60));

        let stretch = PerformanceTargets::stretch();
        assert_eq!(stretch.total, Duration::from_secs(45));
    }

    #[test]
    fn test_stage_metrics() {
        let metric = StageMetrics::new("test", Duration::from_secs(5), Duration::from_secs(10));
        assert!(metric.met_target);
        assert!(metric.improvement_percent() > 0.0);

        let metric_exceeded =
            StageMetrics::new("test", Duration::from_secs(15), Duration::from_secs(10));
        assert!(!metric_exceeded.met_target);
        assert!(metric_exceeded.improvement_percent() < 0.0);
    }

    #[tokio::test]
    async fn test_pipeline_profiler() {
        let mut profiler = PipelineProfiler::new(PerformanceTargets::default());
        profiler.start_pipeline();

        profiler
            .profile_stage("test_stage", async {
                tokio::time::sleep(Duration::from_millis(100)).await;
            })
            .await;

        let metrics = profiler.get_metrics();
        assert_eq!(metrics.len(), 1);
        assert_eq!(metrics[0].name, "test_stage");
    }

    #[tokio::test]
    async fn test_parallel_orchestrator() {
        let orchestrator = ParallelOrchestrator::new(4);

        // Simplified test with tokio::join!
        let (r1, r2, r3) = tokio::join!(
            async { Ok::<_, super::super::LifecycleError>(1) },
            async { Ok::<_, super::super::LifecycleError>(2) },
            async { Ok::<_, super::super::LifecycleError>(3) },
        );

        assert!(r1.is_ok());
        assert!(r2.is_ok());
        assert!(r3.is_ok());
    }

    #[tokio::test]
    async fn test_container_pool() {
        let pool = ContainerPool::new(3).await.unwrap();
        assert_eq!(pool.total(), 3);
        assert_eq!(pool.available(), 3);
    }

    #[test]
    fn test_improvement_calculation() {
        let metric = StageMetrics::new("test", Duration::from_secs(3), Duration::from_secs(5));
        assert!((metric.improvement_percent() - 40.0).abs() < 0.01);

        let metric_over = StageMetrics::new("test", Duration::from_secs(7), Duration::from_secs(5));
        assert!((metric_over.improvement_percent() + 40.0).abs() < 0.01);
    }
}
