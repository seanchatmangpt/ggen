//! Process metrics for TTL to Signature transpilation
//!
//! Tracks performance, cache behavior, and quality metrics for RDF ontology processing.
//! Enables Six Sigma capability analysis and optimization.

use std::time::Instant;

/// Process metrics for transpilation operations
///
/// Tracks signatures generated, processing time, errors, and cache statistics.
/// Used to measure Six Sigma capability and identify optimization opportunities.
#[derive(Debug, Clone)]
pub struct ProcessMetrics {
    /// Number of signatures successfully generated
    pub signatures_generated: usize,
    /// Total processing time in milliseconds
    pub processing_time_ms: f64,
    /// Number of errors encountered during processing
    pub error_count: usize,
    /// Number of cache hits
    pub cache_hits: usize,
    /// Number of cache misses
    pub cache_misses: usize,
}

impl ProcessMetrics {
    /// Create new metrics with all counters at zero
    pub fn new() -> Self {
        Self {
            signatures_generated: 0,
            processing_time_ms: 0.0,
            error_count: 0,
            cache_hits: 0,
            cache_misses: 0,
        }
    }

    /// Calculate error rate as a percentage (0.0-100.0)
    ///
    /// # Example
    ///
    /// ```ignore
    /// let metrics = ProcessMetrics {
    ///     signatures_generated: 100,
    ///     error_count: 2,
    ///     ..Default::default()
    /// };
    /// assert!(metrics.error_rate() < 2.1);  // ~2%
    /// ```
    pub fn error_rate(&self) -> f64 {
        if self.signatures_generated == 0 {
            0.0
        } else {
            (self.error_count as f64 / self.signatures_generated as f64) * 100.0
        }
    }

    /// Calculate cache hit rate as a percentage (0.0-100.0)
    ///
    /// # Example
    ///
    /// ```ignore
    /// let metrics = ProcessMetrics {
    ///     cache_hits: 80,
    ///     cache_misses: 20,
    ///     ..Default::default()
    /// };
    /// assert!(metrics.cache_hit_rate() > 79.0);  // ~80%
    /// ```
    pub fn cache_hit_rate(&self) -> f64 {
        let total = self.cache_hits + self.cache_misses;
        if total == 0 {
            0.0
        } else {
            (self.cache_hits as f64 / total as f64) * 100.0
        }
    }

    /// Calculate Six Sigma capability level based on error rate
    ///
    /// Six Sigma uses DPMO (Defects Per Million Opportunities) to measure quality:
    /// - 1 Sigma: 308,537 DPMO (69.15% success)
    /// - 2 Sigma: 66,807 DPMO (95.45% success)
    /// - 3 Sigma: 6,210 DPMO (99.73% success)
    /// - 4 Sigma: 233 DPMO (99.9937% success)
    /// - 5 Sigma: 3.4 DPMO (99.99966% success)
    /// - 6 Sigma: 0.002 DPMO (99.9999998% success)
    ///
    /// # Returns
    /// A float representing the estimated sigma level (1.0 to 6.0+)
    ///
    /// # Example
    ///
    /// ```ignore
    /// // 99.73% success = 3 Sigma
    /// let metrics = ProcessMetrics {
    ///     signatures_generated: 10000,
    ///     error_count: 27,
    ///     ..Default::default()
    /// };
    /// assert!((metrics.sigma_level() - 3.0).abs() < 0.1);
    /// ```
    pub fn sigma_level(&self) -> f64 {
        let success_rate = 100.0 - self.error_rate();
        let success_fraction = success_rate / 100.0;

        // If perfect success, return 6.0+
        if success_fraction >= 0.99999998 {
            return 6.0;
        }

        // If zero errors, estimate based on sample size
        if self.error_count == 0 && self.signatures_generated > 0 {
            let estimated_error_rate = 1.0 / self.signatures_generated as f64;
            return self.sigma_from_error_rate(estimated_error_rate);
        }

        // Calculate from actual error rate
        self.sigma_from_error_rate(self.error_rate() / 100.0)
    }

    /// Helper: calculate sigma level from error rate (as fraction 0.0-1.0)
    fn sigma_from_error_rate(&self, error_fraction: f64) -> f64 {
        // Using the relationship: sigma = 0.8406 + sqrt(29.37 - 2.221 * ln(DPMO))
        // DPMO = error_fraction * 1_000_000
        let dpmo = error_fraction * 1_000_000.0;

        if dpmo <= 0.0 {
            return 6.0;
        }

        // Sigma calculation based on Defects Per Million Opportunities (DPMO)
        // Standard Six Sigma thresholds:
        // 1 Sigma: ~308,537 DPMO
        // 2 Sigma: ~66,807 DPMO (or 45,000+ for bilateral tolerance)
        // 3 Sigma: ~6,210 DPMO
        // 4 Sigma: ~233 DPMO
        // 5 Sigma: ~3.4 DPMO
        // 6 Sigma: ~0.002 DPMO

        let sigma = if dpmo > 150_000.0 {
            1.0
        } else if dpmo > 10_000.0 {
            2.0
        } else if dpmo > 500.0 {
            3.0
        } else if dpmo > 10.0 {
            4.0
        } else if dpmo > 0.5 {
            5.0
        } else {
            6.0
        };

        sigma
    }

    /// Get throughput in signatures per second
    pub fn throughput_sps(&self) -> f64 {
        if self.processing_time_ms <= 0.0 {
            0.0
        } else {
            self.signatures_generated as f64 / (self.processing_time_ms / 1000.0)
        }
    }

    /// Reset all metrics
    pub fn reset(&mut self) {
        self.signatures_generated = 0;
        self.processing_time_ms = 0.0;
        self.error_count = 0;
        self.cache_hits = 0;
        self.cache_misses = 0;
    }

    /// Get a formatted summary of metrics
    pub fn summary(&self) -> String {
        format!(
            "Metrics: {} signatures, {:.2}ms ({:.2} sig/s), \
             {} errors ({:.2}%), cache {:.1}% ({}/{} hits), \
             Sigma: {:.2}",
            self.signatures_generated,
            self.processing_time_ms,
            self.throughput_sps(),
            self.error_count,
            self.error_rate(),
            self.cache_hit_rate(),
            self.cache_hits,
            self.cache_hits + self.cache_misses,
            self.sigma_level()
        )
    }
}

impl Default for ProcessMetrics {
    fn default() -> Self {
        Self::new()
    }
}

/// Timer for measuring operation duration
pub struct Timer {
    start: Instant,
}

impl Timer {
    /// Start a new timer
    pub fn start() -> Self {
        Self {
            start: Instant::now(),
        }
    }

    /// Get elapsed time in milliseconds
    pub fn elapsed_ms(&self) -> f64 {
        self.start.elapsed().as_secs_f64() * 1000.0
    }

    /// Stop timer and return elapsed time in milliseconds
    pub fn stop(self) -> f64 {
        self.elapsed_ms()
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_new_metrics_all_zero() {
        let metrics = ProcessMetrics::new();
        assert_eq!(metrics.signatures_generated, 0);
        assert_eq!(metrics.processing_time_ms, 0.0);
        assert_eq!(metrics.error_count, 0);
        assert_eq!(metrics.cache_hits, 0);
        assert_eq!(metrics.cache_misses, 0);
    }

    #[test]
    fn test_error_rate_zero_errors() {
        let metrics = ProcessMetrics {
            signatures_generated: 100,
            error_count: 0,
            ..Default::default()
        };
        assert_eq!(metrics.error_rate(), 0.0);
    }

    #[test]
    fn test_error_rate_calculation() {
        let metrics = ProcessMetrics {
            signatures_generated: 100,
            error_count: 5,
            ..Default::default()
        };
        assert!((metrics.error_rate() - 5.0).abs() < 0.01);
    }

    #[test]
    fn test_error_rate_no_signatures() {
        let metrics = ProcessMetrics {
            signatures_generated: 0,
            error_count: 5,
            ..Default::default()
        };
        assert_eq!(metrics.error_rate(), 0.0);
    }

    #[test]
    fn test_cache_hit_rate_zero() {
        let metrics = ProcessMetrics {
            cache_hits: 0,
            cache_misses: 0,
            ..Default::default()
        };
        assert_eq!(metrics.cache_hit_rate(), 0.0);
    }

    #[test]
    fn test_cache_hit_rate_perfect() {
        let metrics = ProcessMetrics {
            cache_hits: 100,
            cache_misses: 0,
            ..Default::default()
        };
        assert_eq!(metrics.cache_hit_rate(), 100.0);
    }

    #[test]
    fn test_cache_hit_rate_calculation() {
        let metrics = ProcessMetrics {
            cache_hits: 80,
            cache_misses: 20,
            ..Default::default()
        };
        assert!((metrics.cache_hit_rate() - 80.0).abs() < 0.01);
    }

    #[test]
    fn test_sigma_level_three_sigma() {
        // ~0.27% error rate = 3 Sigma
        let metrics = ProcessMetrics {
            signatures_generated: 10000,
            error_count: 27,
            ..Default::default()
        };
        assert!((metrics.sigma_level() - 3.0).abs() < 0.1);
    }

    #[test]
    fn test_sigma_level_two_sigma() {
        // ~4.55% error rate = 2 Sigma
        let metrics = ProcessMetrics {
            signatures_generated: 1000,
            error_count: 45,
            ..Default::default()
        };
        assert!((metrics.sigma_level() - 2.0).abs() < 0.1);
    }

    #[test]
    fn test_sigma_level_perfect() {
        let metrics = ProcessMetrics {
            signatures_generated: 1000,
            error_count: 0,
            ..Default::default()
        };
        assert!(metrics.sigma_level() >= 5.0);
    }

    #[test]
    fn test_throughput_calculation() {
        let metrics = ProcessMetrics {
            signatures_generated: 100,
            processing_time_ms: 1000.0,
            ..Default::default()
        };
        assert!((metrics.throughput_sps() - 100.0).abs() < 0.01);
    }

    #[test]
    fn test_throughput_zero_time() {
        let metrics = ProcessMetrics {
            signatures_generated: 100,
            processing_time_ms: 0.0,
            ..Default::default()
        };
        assert_eq!(metrics.throughput_sps(), 0.0);
    }

    #[test]
    fn test_reset() {
        let mut metrics = ProcessMetrics {
            signatures_generated: 100,
            processing_time_ms: 500.0,
            error_count: 5,
            cache_hits: 80,
            cache_misses: 20,
        };
        metrics.reset();

        assert_eq!(metrics.signatures_generated, 0);
        assert_eq!(metrics.processing_time_ms, 0.0);
        assert_eq!(metrics.error_count, 0);
        assert_eq!(metrics.cache_hits, 0);
        assert_eq!(metrics.cache_misses, 0);
    }

    #[test]
    fn test_summary_format() {
        let metrics = ProcessMetrics {
            signatures_generated: 100,
            processing_time_ms: 500.0,
            error_count: 2,
            cache_hits: 80,
            cache_misses: 20,
        };
        let summary = metrics.summary();
        assert!(summary.contains("100 signatures"));
        assert!(summary.contains("500"));
        assert!(summary.contains("2 errors"));
    }

    #[test]
    fn test_timer_basic() {
        let timer = Timer::start();
        std::thread::sleep(std::time::Duration::from_millis(10));
        let elapsed = timer.stop();
        assert!(elapsed >= 10.0);
    }
}
