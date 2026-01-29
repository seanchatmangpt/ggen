//! Performance profiling infrastructure.
//!
//! This module provides CPU and memory profiling capabilities for
//! identifying performance bottlenecks and optimization opportunities.

use ggen_utils::error::{GgenError, Result};
use serde::{Deserialize, Serialize};
use std::collections::HashMap;
use std::path::PathBuf;
use std::time::{Duration, Instant};

/// Configuration for profiling.
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct ProfileConfig {
    /// Enable CPU profiling with flamegraph generation.
    pub enable_cpu_profiling: bool,
    /// Enable memory allocation tracking.
    pub enable_memory_tracking: bool,
    /// Output directory for profiling reports.
    pub output_dir: PathBuf,
    /// Sampling frequency for profiling (in Hz).
    pub sampling_frequency: u32,
}

impl Default for ProfileConfig {
    fn default() -> Self {
        Self {
            enable_cpu_profiling: true,
            enable_memory_tracking: true,
            output_dir: PathBuf::from("benches/profiling"),
            sampling_frequency: 99, // 99 Hz is standard for profiling
        }
    }
}

/// Memory profiling data.
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct MemoryProfile {
    /// Timestamp of the profile.
    pub timestamp: chrono::DateTime<chrono::Utc>,
    /// Total allocated bytes.
    pub total_allocated: usize,
    /// Total deallocated bytes.
    pub total_deallocated: usize,
    /// Current memory usage in bytes.
    pub current_usage: usize,
    /// Peak memory usage in bytes.
    pub peak_usage: usize,
    /// Number of allocations.
    pub allocation_count: usize,
    /// Number of deallocations.
    pub deallocation_count: usize,
    /// Allocation size histogram.
    pub size_histogram: HashMap<usize, usize>,
}

impl MemoryProfile {
    /// Create a new empty memory profile.
    pub fn new() -> Self {
        Self {
            timestamp: chrono::Utc::now(),
            total_allocated: 0,
            total_deallocated: 0,
            current_usage: 0,
            peak_usage: 0,
            allocation_count: 0,
            deallocation_count: 0,
            size_histogram: HashMap::new(),
        }
    }

    /// Record an allocation.
    pub fn record_allocation(&mut self, size: usize) {
        self.total_allocated += size;
        self.current_usage += size;
        self.allocation_count += 1;

        if self.current_usage > self.peak_usage {
            self.peak_usage = self.current_usage;
        }

        // Update histogram (bucket by power of 2)
        let bucket = if size == 0 {
            0
        } else {
            1usize << (usize::BITS - size.leading_zeros())
        };
        *self.size_histogram.entry(bucket).or_insert(0) += 1;
    }

    /// Record a deallocation.
    pub fn record_deallocation(&mut self, size: usize) {
        self.total_deallocated += size;
        self.current_usage = self.current_usage.saturating_sub(size);
        self.deallocation_count += 1;
    }

    /// Get memory efficiency (ratio of deallocated to allocated).
    pub fn efficiency(&self) -> f64 {
        if self.total_allocated == 0 {
            0.0
        } else {
            self.total_deallocated as f64 / self.total_allocated as f64
        }
    }

    /// Generate a text report of the memory profile.
    pub fn generate_report(&self) -> String {
        let mut report = String::from("Memory Profile Report\n");
        report.push_str("━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━\n\n");

        report.push_str(&format!("Total Allocated:     {:>12} bytes\n", self.total_allocated));
        report.push_str(&format!(
            "Total Deallocated:   {:>12} bytes\n",
            self.total_deallocated
        ));
        report.push_str(&format!("Current Usage:       {:>12} bytes\n", self.current_usage));
        report.push_str(&format!("Peak Usage:          {:>12} bytes\n", self.peak_usage));
        report.push_str(&format!(
            "Allocation Count:    {:>12}\n",
            self.allocation_count
        ));
        report.push_str(&format!(
            "Deallocation Count:  {:>12}\n",
            self.deallocation_count
        ));
        report.push_str(&format!("Efficiency:          {:>12.2}%\n", self.efficiency() * 100.0));

        if !self.size_histogram.is_empty() {
            report.push_str("\nAllocation Size Distribution:\n");
            let mut buckets: Vec<_> = self.size_histogram.iter().collect();
            buckets.sort_by_key(|(size, _)| **size);

            for (size, count) in buckets {
                report.push_str(&format!("  {:>10} bytes: {}\n", size, count));
            }
        }

        report
    }
}

impl Default for MemoryProfile {
    fn default() -> Self {
        Self::new()
    }
}

/// Memory tracker for profiling.
pub struct MemoryTracker {
    profile: MemoryProfile,
    start_time: Instant,
}

impl MemoryTracker {
    /// Create a new memory tracker.
    pub fn new() -> Self {
        Self {
            profile: MemoryProfile::new(),
            start_time: Instant::now(),
        }
    }

    /// Record an allocation.
    pub fn track_allocation(&mut self, size: usize) {
        self.profile.record_allocation(size);
    }

    /// Record a deallocation.
    pub fn track_deallocation(&mut self, size: usize) {
        self.profile.record_deallocation(size);
    }

    /// Get the current profile.
    pub fn profile(&self) -> &MemoryProfile {
        &self.profile
    }

    /// Get elapsed time since tracker creation.
    pub fn elapsed(&self) -> Duration {
        self.start_time.elapsed()
    }

    /// Generate a report.
    pub fn report(&self) -> String {
        self.profile.generate_report()
    }
}

impl Default for MemoryTracker {
    fn default() -> Self {
        Self::new()
    }
}

/// CPU and memory profiler.
pub struct Profiler {
    config: ProfileConfig,
    memory_tracker: Option<MemoryTracker>,
}

impl Profiler {
    /// Create a new profiler with the given configuration.
    pub fn new(config: ProfileConfig) -> Self {
        let memory_tracker = if config.enable_memory_tracking {
            Some(MemoryTracker::new())
        } else {
            None
        };

        Self {
            config,
            memory_tracker,
        }
    }

    /// Start CPU profiling.
    pub fn start_cpu_profiling(&self) -> Result<()> {
        if !self.config.enable_cpu_profiling {
            return Ok(());
        }

        log::info!("CPU profiling would start here (requires pprof or flamegraph crate)");
        // In a real implementation, this would use the pprof or flamegraph crate
        // to start CPU profiling with the configured sampling frequency.
        Ok(())
    }

    /// Stop CPU profiling and generate flamegraph.
    pub fn stop_cpu_profiling(&self) -> Result<PathBuf> {
        if !self.config.enable_cpu_profiling {
            return Err(GgenError::Generation(
                "CPU profiling is not enabled".to_string(),
            ));
        }

        // Create output directory if it doesn't exist
        std::fs::create_dir_all(&self.config.output_dir).map_err(|e| {
            GgenError::Generation(format!("Failed to create output directory: {}", e))
        })?;

        let output_path = self
            .config
            .output_dir
            .join(format!("flamegraph-{}.svg", chrono::Utc::now().timestamp()));

        log::info!("CPU profiling would stop and generate flamegraph at: {:?}", output_path);

        // In a real implementation, this would:
        // 1. Stop the CPU profiler
        // 2. Collect profiling data
        // 3. Generate a flamegraph SVG file
        // 4. Save to output_path

        Ok(output_path)
    }

    /// Track a memory allocation.
    pub fn track_allocation(&mut self, size: usize) {
        if let Some(tracker) = &mut self.memory_tracker {
            tracker.track_allocation(size);
        }
    }

    /// Track a memory deallocation.
    pub fn track_deallocation(&mut self, size: usize) {
        if let Some(tracker) = &mut self.memory_tracker {
            tracker.track_deallocation(size);
        }
    }

    /// Get memory profiling report.
    pub fn memory_report(&self) -> Result<String> {
        if let Some(tracker) = &self.memory_tracker {
            Ok(tracker.report())
        } else {
            Err(GgenError::Generation(
                "Memory tracking is not enabled".to_string(),
            ))
        }
    }

    /// Generate and save memory report to file.
    pub fn save_memory_report(&self) -> Result<PathBuf> {
        let report = self.memory_report()?;

        // Create output directory if it doesn't exist
        std::fs::create_dir_all(&self.config.output_dir).map_err(|e| {
            GgenError::Generation(format!("Failed to create output directory: {}", e))
        })?;

        let output_path = self
            .config
            .output_dir
            .join(format!("memory-profile-{}.txt", chrono::Utc::now().timestamp()));

        std::fs::write(&output_path, report).map_err(|e| {
            GgenError::Generation(format!("Failed to write memory report: {}", e))
        })?;

        log::info!("Memory report saved to: {:?}", output_path);

        Ok(output_path)
    }

    /// Profile a closure and return results.
    pub async fn profile<F, T>(&mut self, f: F) -> Result<(T, Duration)>
    where
        F: std::future::Future<Output = Result<T>>,
    {
        self.start_cpu_profiling()?;
        let start = Instant::now();

        let result = f.await?;

        let duration = start.elapsed();
        let _ = self.stop_cpu_profiling(); // Ignore error if profiling disabled

        Ok((result, duration))
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_profile_config_default() {
        // Arrange & Act
        let config = ProfileConfig::default();

        // Assert
        assert!(config.enable_cpu_profiling);
        assert!(config.enable_memory_tracking);
        assert_eq!(config.sampling_frequency, 99);
        assert_eq!(config.output_dir, PathBuf::from("benches/profiling"));
    }

    #[test]
    fn test_memory_profile_new() {
        // Arrange & Act
        let profile = MemoryProfile::new();

        // Assert
        assert_eq!(profile.total_allocated, 0);
        assert_eq!(profile.total_deallocated, 0);
        assert_eq!(profile.current_usage, 0);
        assert_eq!(profile.peak_usage, 0);
        assert_eq!(profile.allocation_count, 0);
        assert_eq!(profile.deallocation_count, 0);
    }

    #[test]
    fn test_memory_profile_record_allocation() {
        // Arrange
        let mut profile = MemoryProfile::new();

        // Act
        profile.record_allocation(1024);
        profile.record_allocation(2048);

        // Assert
        assert_eq!(profile.total_allocated, 3072);
        assert_eq!(profile.current_usage, 3072);
        assert_eq!(profile.peak_usage, 3072);
        assert_eq!(profile.allocation_count, 2);
    }

    #[test]
    fn test_memory_profile_record_deallocation() {
        // Arrange
        let mut profile = MemoryProfile::new();
        profile.record_allocation(2048);

        // Act
        profile.record_deallocation(1024);

        // Assert
        assert_eq!(profile.total_allocated, 2048);
        assert_eq!(profile.total_deallocated, 1024);
        assert_eq!(profile.current_usage, 1024);
        assert_eq!(profile.deallocation_count, 1);
    }

    #[test]
    fn test_memory_profile_peak_usage() {
        // Arrange
        let mut profile = MemoryProfile::new();

        // Act
        profile.record_allocation(1024);
        profile.record_allocation(2048); // Peak should be 3072
        profile.record_deallocation(1024); // Peak remains 3072

        // Assert
        assert_eq!(profile.peak_usage, 3072);
        assert_eq!(profile.current_usage, 2048);
    }

    #[test]
    fn test_memory_profile_efficiency() {
        // Arrange
        let mut profile = MemoryProfile::new();

        // Act
        profile.record_allocation(1000);
        profile.record_deallocation(500);

        // Assert
        assert_eq!(profile.efficiency(), 0.5);
    }

    #[test]
    fn test_memory_profile_efficiency_no_allocations() {
        // Arrange
        let profile = MemoryProfile::new();

        // Act
        let efficiency = profile.efficiency();

        // Assert
        assert_eq!(efficiency, 0.0);
    }

    #[test]
    fn test_memory_tracker_new() {
        // Arrange & Act
        let tracker = MemoryTracker::new();

        // Assert
        assert_eq!(tracker.profile().allocation_count, 0);
        assert!(tracker.elapsed() >= Duration::from_secs(0));
    }

    #[test]
    fn test_memory_tracker_track_allocation() {
        // Arrange
        let mut tracker = MemoryTracker::new();

        // Act
        tracker.track_allocation(1024);
        tracker.track_allocation(2048);

        // Assert
        assert_eq!(tracker.profile().allocation_count, 2);
        assert_eq!(tracker.profile().total_allocated, 3072);
    }

    #[test]
    fn test_memory_tracker_track_deallocation() {
        // Arrange
        let mut tracker = MemoryTracker::new();
        tracker.track_allocation(2048);

        // Act
        tracker.track_deallocation(1024);

        // Assert
        assert_eq!(tracker.profile().deallocation_count, 1);
        assert_eq!(tracker.profile().total_deallocated, 1024);
    }

    #[test]
    fn test_memory_tracker_report() {
        // Arrange
        let mut tracker = MemoryTracker::new();
        tracker.track_allocation(1024);

        // Act
        let report = tracker.report();

        // Assert
        assert!(report.contains("Memory Profile Report"));
        assert!(report.contains("Total Allocated"));
        assert!(report.contains("1024"));
    }

    #[test]
    fn test_profiler_new_with_memory_tracking() {
        // Arrange
        let config = ProfileConfig::default();

        // Act
        let profiler = Profiler::new(config);

        // Assert
        assert!(profiler.memory_tracker.is_some());
    }

    #[test]
    fn test_profiler_new_without_memory_tracking() {
        // Arrange
        let config = ProfileConfig {
            enable_memory_tracking: false,
            ..Default::default()
        };

        // Act
        let profiler = Profiler::new(config);

        // Assert
        assert!(profiler.memory_tracker.is_none());
    }

    #[test]
    fn test_profiler_track_allocation() {
        // Arrange
        let config = ProfileConfig::default();
        let mut profiler = Profiler::new(config);

        // Act
        profiler.track_allocation(1024);

        // Assert
        assert!(profiler.memory_tracker.is_some());
        let tracker = profiler.memory_tracker.as_ref().unwrap();
        assert_eq!(tracker.profile().allocation_count, 1);
    }

    #[test]
    fn test_profiler_memory_report() {
        // Arrange
        let config = ProfileConfig::default();
        let mut profiler = Profiler::new(config);
        profiler.track_allocation(1024);

        // Act
        let result = profiler.memory_report();

        // Assert
        assert!(result.is_ok());
        let report = result.unwrap();
        assert!(report.contains("Memory Profile Report"));
    }

    #[test]
    fn test_profiler_memory_report_disabled() {
        // Arrange
        let config = ProfileConfig {
            enable_memory_tracking: false,
            ..Default::default()
        };
        let profiler = Profiler::new(config);

        // Act
        let result = profiler.memory_report();

        // Assert
        assert!(result.is_err());
    }

    #[test]
    fn test_memory_profile_generate_report() {
        // Arrange
        let mut profile = MemoryProfile::new();
        profile.record_allocation(1024);
        profile.record_allocation(2048);
        profile.record_deallocation(512);

        // Act
        let report = profile.generate_report();

        // Assert
        assert!(report.contains("Memory Profile Report"));
        assert!(report.contains("Total Allocated:"));
        assert!(report.contains("3072"));
        assert!(report.contains("Allocation Count:"));
        assert!(report.contains("2"));
    }
}
