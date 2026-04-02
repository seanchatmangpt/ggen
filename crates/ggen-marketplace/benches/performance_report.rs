//! Performance Report Generator
//!
//! Generates comprehensive performance reports with:
//! - Benchmark results visualization
//! - SLO validation status
//! - V1 vs V2 comparison
//! - Optimization recommendations
//! - Deployment sizing guidance

use std::fs;
use std::path::Path;
use std::time::Duration;

/// Performance metrics for a single benchmark
#[derive(Debug, Clone)]
pub struct BenchmarkMetric {
    pub name: String,
    pub mean_ns: u64,
    pub std_dev_ns: u64,
    pub p50_ns: u64,
    pub p95_ns: u64,
    pub p99_ns: u64,
    pub throughput_ops_sec: f64,
}

/// SLO validation result
#[derive(Debug, Clone)]
pub struct SloResult {
    pub name: String,
    pub threshold: String,
    pub actual: String,
    pub met: bool,
    pub margin: String,
}

/// V1 vs V2 comparison data
#[derive(Debug, Clone)]
pub struct ComparisonMetric {
    pub operation: String,
    pub v1_mean_ms: f64,
    pub v2_mean_ms: f64,
    pub improvement_percent: f64,
}

/// Performance report
pub struct PerformanceReport {
    pub benchmarks: Vec<BenchmarkMetric>,
    pub slo_results: Vec<SloResult>,
    pub comparisons: Vec<ComparisonMetric>,
    pub cache_hit_rate: f64,
    pub memory_footprint_mb: f64,
    pub scalability_notes: Vec<String>,
}

impl PerformanceReport {
    /// Create a new performance report
    pub fn new() -> Self {
        Self {
            benchmarks: Vec::new(),
            slo_results: Vec::new(),
            comparisons: Vec::new(),
            cache_hit_rate: 0.0,
            memory_footprint_mb: 0.0,
            scalability_notes: Vec::new(),
        }
    }

    /// Add a benchmark metric
    pub fn add_benchmark(&mut self, metric: BenchmarkMetric) {
        self.benchmarks.push(metric);
    }

    /// Add SLO validation result
    pub fn add_slo_result(&mut self, result: SloResult) {
        self.slo_results.push(result);
    }

    /// Add comparison metric
    pub fn add_comparison(&mut self, comparison: ComparisonMetric) {
        self.comparisons.push(comparison);
    }

    /// Generate markdown report
    pub fn generate_markdown(&self) -> String {
        let mut report = String::new();

        // Header
        report.push_str("# Marketplace V2 Performance Report\n\n");
        report.push_str(&format!(
            "**Report Generated:** {}\n\n",
            chrono::Local::now().format("%Y-%m-%d %H:%M:%S")
        ));

        // Executive Summary
        report.push_str("## Executive Summary\n\n");
        let slo_met = self.slo_results.iter().filter(|r| r.met).count();
        let slo_total = self.slo_results.len();
        report.push_str(&format!(
            "- **SLOs Met:** {}/{} ({:.1}%)\n",
            slo_met,
            slo_total,
            (slo_met as f64 / slo_total as f64) * 100.0
        ));
        report.push_str(&format!(
            "- **Cache Hit Rate:** {:.2}%\n",
            self.cache_hit_rate * 100.0
        ));
        report.push_str(&format!(
            "- **Memory Footprint:** {:.2} MB\n",
            self.memory_footprint_mb
        ));

        let avg_improvement = self
            .comparisons
            .iter()
            .map(|c| c.improvement_percent)
            .sum::<f64>()
            / self.comparisons.len() as f64;
        report.push_str(&format!(
            "- **Average V2 Improvement:** {:.1}%\n\n",
            avg_improvement
        ));

        // SLO Validation Results
        report.push_str("## SLO Validation Results\n\n");
        report.push_str("| Metric | Threshold | Actual | Status | Margin |\n");
        report.push_str("|--------|-----------|--------|--------|--------|\n");
        for slo in &self.slo_results {
            let status = if slo.met { "✅ PASS" } else { "❌ FAIL" };
            report.push_str(&format!(
                "| {} | {} | {} | {} | {} |\n",
                slo.name, slo.threshold, slo.actual, status, slo.margin
            ));
        }
        report.push_str("\n");

        // V1 vs V2 Comparison
        report.push_str("## V1 vs V2 Performance Comparison\n\n");
        report.push_str("| Operation | V1 Mean | V2 Mean | Improvement |\n");
        report.push_str("|-----------|---------|---------|-------------|\n");
        for comp in &self.comparisons {
            report.push_str(&format!(
                "| {} | {:.2}ms | {:.2}ms | {:.1}% |\n",
                comp.operation, comp.v1_mean_ms, comp.v2_mean_ms, comp.improvement_percent
            ));
        }
        report.push_str("\n");

        // Detailed Benchmark Results
        report.push_str("## Detailed Benchmark Results\n\n");
        report.push_str("| Benchmark | Mean | Std Dev | p50 | p95 | p99 | Throughput |\n");
        report.push_str("|-----------|------|---------|-----|-----|-----|------------|\n");
        for bench in &self.benchmarks {
            report.push_str(&format!(
                "| {} | {:.2}ms | {:.2}ms | {:.2}ms | {:.2}ms | {:.2}ms | {:.0} ops/s |\n",
                bench.name,
                bench.mean_ns as f64 / 1_000_000.0,
                bench.std_dev_ns as f64 / 1_000_000.0,
                bench.p50_ns as f64 / 1_000_000.0,
                bench.p95_ns as f64 / 1_000_000.0,
                bench.p99_ns as f64 / 1_000_000.0,
                bench.throughput_ops_sec
            ));
        }
        report.push_str("\n");

        // Scalability Analysis
        report.push_str("## Scalability Analysis\n\n");
        for note in &self.scalability_notes {
            report.push_str(&format!("- {}\n", note));
        }
        report.push_str("\n");

        // Optimization Recommendations
        report.push_str("## Optimization Recommendations\n\n");
        report.push_str(&self.generate_recommendations());

        // Deployment Sizing Guidance
        report.push_str("## Deployment Sizing Guidance\n\n");
        report.push_str(&self.generate_sizing_guidance());

        report
    }

    /// Generate optimization recommendations
    fn generate_recommendations(&self) -> String {
        let mut recommendations = String::new();

        // Cache configuration
        if self.cache_hit_rate < 0.85 {
            recommendations.push_str("### Cache Configuration\n\n");
            recommendations.push_str(&format!(
                "- **Current hit rate:** {:.2}%\n",
                self.cache_hit_rate * 100.0
            ));
            recommendations.push_str("- **Recommendation:** Increase cache size or adjust TTL\n");
            recommendations.push_str("- **Expected improvement:** 10-15% latency reduction\n\n");
        }

        // Memory optimization
        if self.memory_footprint_mb > 500.0 {
            recommendations.push_str("### Memory Optimization\n\n");
            recommendations.push_str(&format!(
                "- **Current footprint:** {:.2} MB\n",
                self.memory_footprint_mb
            ));
            recommendations.push_str(
                "- **Recommendation:** Enable memory pooling and compact representations\n",
            );
            recommendations.push_str("- **Expected improvement:** 20-30% memory reduction\n\n");
        }

        // Query optimization
        for slo in &self.slo_results {
            if !slo.met && slo.name.contains("Search") {
                recommendations.push_str("### Search Optimization\n\n");
                recommendations.push_str("- **Issue:** Search latency exceeds SLO\n");
                recommendations.push_str("- **Recommendations:**\n");
                recommendations.push_str("  - Enable SPARQL query caching\n");
                recommendations.push_str("  - Add indexes for frequent query patterns\n");
                recommendations.push_str("  - Consider query result pagination\n");
                recommendations
                    .push_str("- **Expected improvement:** 30-40% latency reduction\n\n");
                break;
            }
        }

        if recommendations.is_empty() {
            recommendations.push_str("✅ All systems operating optimally. No recommendations.\n\n");
        }

        recommendations
    }

    /// Generate deployment sizing guidance
    fn generate_sizing_guidance(&self) -> String {
        let mut guidance = String::new();

        guidance.push_str("### Small Deployment (< 1,000 packages)\n\n");
        guidance.push_str("- **Memory:** 512 MB\n");
        guidance.push_str("- **Cache Size:** 100 entries\n");
        guidance.push_str("- **Expected Latency:** p95 < 50ms\n\n");

        guidance.push_str("### Medium Deployment (1,000 - 10,000 packages)\n\n");
        guidance.push_str("- **Memory:** 2 GB\n");
        guidance.push_str("- **Cache Size:** 500 entries\n");
        guidance.push_str("- **Expected Latency:** p95 < 75ms\n\n");

        guidance.push_str("### Large Deployment (> 10,000 packages)\n\n");
        guidance.push_str("- **Memory:** 4 GB+\n");
        guidance.push_str("- **Cache Size:** 1000+ entries\n");
        guidance.push_str("- **Expected Latency:** p95 < 100ms\n");
        guidance.push_str("- **Recommendation:** Enable horizontal scaling\n\n");

        guidance
    }

    /// Save report to file
    pub fn save_to_file<P: AsRef<Path>>(&self, path: P) -> std::io::Result<()> {
        let markdown = self.generate_markdown();
        fs::write(path, markdown)
    }
}

impl Default for PerformanceReport {
    fn default() -> Self {
        Self::new()
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_report_generation() {
        let mut report = PerformanceReport::new();

        // Add sample data
        report.add_slo_result(SloResult {
            name: "Lookup Latency".to_string(),
            threshold: "<100ms".to_string(),
            actual: "85ms".to_string(),
            met: true,
            margin: "15ms".to_string(),
        });

        report.add_comparison(ComparisonMetric {
            operation: "Lookup".to_string(),
            v1_mean_ms: 150.0,
            v2_mean_ms: 85.0,
            improvement_percent: 43.3,
        });

        report.cache_hit_rate = 0.87;
        report.memory_footprint_mb = 256.0;

        let markdown = report.generate_markdown();

        assert!(markdown.contains("# Marketplace V2 Performance Report"));
        assert!(markdown.contains("SLO Validation Results"));
        assert!(markdown.contains("V1 vs V2 Performance Comparison"));
        assert!(markdown.contains("43.3%"));
    }
}
