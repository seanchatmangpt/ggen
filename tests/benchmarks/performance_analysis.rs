// Performance Analysis and Reporting Tool
// Analyzes benchmark results and generates optimization recommendations

use serde::{Deserialize, Serialize};
use std::collections::HashMap;
use std::fs;
use std::path::Path;

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct BenchmarkResult {
    pub command: String,
    pub dataset_size: usize,
    pub mean_time_ms: f64,
    pub median_time_ms: f64,
    pub p95_time_ms: f64,
    pub p99_time_ms: f64,
    pub throughput: f64,
    pub memory_usage_mb: f64,
    pub success_rate: f64,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct PerformanceGoals {
    pub interactive_max_ms: f64,      // Target < 500ms for interactive commands
    pub report_max_ms: f64,            // Target < 5000ms for report generation
    pub memory_max_mb: f64,            // Target < 100MB memory usage
    pub min_success_rate: f64,         // Target > 99% success rate
}

impl Default for PerformanceGoals {
    fn default() -> Self {
        Self {
            interactive_max_ms: 500.0,
            report_max_ms: 5000.0,
            memory_max_mb: 100.0,
            min_success_rate: 0.99,
        }
    }
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct OptimizationRecommendation {
    pub severity: Severity,
    pub command: String,
    pub issue: String,
    pub current_performance: String,
    pub target_performance: String,
    pub recommendations: Vec<String>,
    pub estimated_improvement: String,
}

#[derive(Debug, Clone, Serialize, Deserialize, PartialEq, Eq, PartialOrd, Ord)]
pub enum Severity {
    Critical,  // > 2x over goal
    High,      // 1.5-2x over goal
    Medium,    // 1.2-1.5x over goal
    Low,       // 1.0-1.2x over goal
    Info,      // Meeting goals
}

pub struct PerformanceAnalyzer {
    goals: PerformanceGoals,
    results: Vec<BenchmarkResult>,
}

impl PerformanceAnalyzer {
    pub fn new(goals: PerformanceGoals) -> Self {
        Self {
            goals,
            results: Vec::new(),
        }
    }

    pub fn add_result(&mut self, result: BenchmarkResult) {
        self.results.push(result);
    }

    pub fn analyze(&self) -> AnalysisReport {
        let mut recommendations = Vec::new();
        let mut summary_stats = HashMap::new();

        // Analyze each benchmark result
        for result in &self.results {
            // Determine command type (interactive vs report)
            let is_interactive = self.is_interactive_command(&result.command);
            let time_goal = if is_interactive {
                self.goals.interactive_max_ms
            } else {
                self.goals.report_max_ms
            };

            // Check latency performance
            if result.mean_time_ms > time_goal {
                let severity = self.calculate_severity(result.mean_time_ms, time_goal);
                recommendations.push(self.generate_latency_recommendation(result, time_goal, severity));
            }

            // Check memory performance
            if result.memory_usage_mb > self.goals.memory_max_mb {
                let severity = self.calculate_severity(
                    result.memory_usage_mb,
                    self.goals.memory_max_mb,
                );
                recommendations.push(self.generate_memory_recommendation(result, severity));
            }

            // Check success rate
            if result.success_rate < self.goals.min_success_rate {
                recommendations.push(self.generate_reliability_recommendation(result));
            }

            // Update summary stats
            summary_stats
                .entry(result.command.clone())
                .or_insert_with(Vec::new)
                .push(result.clone());
        }

        // Sort recommendations by severity
        recommendations.sort_by(|a, b| a.severity.cmp(&b.severity));

        AnalysisReport {
            summary: self.generate_summary(&summary_stats),
            goals: self.goals.clone(),
            recommendations,
            baseline_metrics: self.calculate_baseline_metrics(),
            performance_trends: self.analyze_trends(&summary_stats),
        }
    }

    fn is_interactive_command(&self, command: &str) -> bool {
        matches!(
            command,
            "search" | "compare" | "text_search" | "category_search" | "tag_search"
        )
    }

    fn calculate_severity(&self, actual: f64, target: f64) -> Severity {
        let ratio = actual / target;
        if ratio >= 2.0 {
            Severity::Critical
        } else if ratio >= 1.5 {
            Severity::High
        } else if ratio >= 1.2 {
            Severity::Medium
        } else if ratio >= 1.0 {
            Severity::Low
        } else {
            Severity::Info
        }
    }

    fn generate_latency_recommendation(
        &self,
        result: &BenchmarkResult,
        goal: f64,
        severity: Severity,
    ) -> OptimizationRecommendation {
        let mut recommendations = vec![
            "Consider implementing caching for frequently accessed data".to_string(),
            "Optimize database queries with proper indexing".to_string(),
            "Use lazy loading for large datasets".to_string(),
        ];

        // Add specific recommendations based on command type
        if result.command.contains("search") {
            recommendations.push("Implement full-text search indexing (e.g., tantivy)".to_string());
            recommendations.push("Add search result pagination".to_string());
            recommendations.push("Cache search results for common queries".to_string());
        } else if result.command.contains("export") {
            recommendations.push("Stream data instead of loading all in memory".to_string());
            recommendations.push("Use buffered writes for large exports".to_string());
            recommendations.push("Consider parallel processing for format conversion".to_string());
        } else if result.command.contains("assessment") {
            recommendations.push("Parallelize maturity calculations using rayon".to_string());
            recommendations.push("Cache intermediate assessment results".to_string());
            recommendations.push("Batch API calls to reduce network overhead".to_string());
        }

        // Add dataset size specific recommendations
        if result.dataset_size >= 1000 {
            recommendations.push("Implement database-backed storage for large datasets".to_string());
            recommendations.push("Use memory-mapped files for efficient data access".to_string());
        }

        OptimizationRecommendation {
            severity,
            command: result.command.clone(),
            issue: format!("Latency exceeds target by {:.1}%", (result.mean_time_ms / goal - 1.0) * 100.0),
            current_performance: format!("{:.2}ms (p95: {:.2}ms, p99: {:.2}ms)",
                result.mean_time_ms, result.p95_time_ms, result.p99_time_ms),
            target_performance: format!("{:.2}ms", goal),
            recommendations,
            estimated_improvement: format!("{:.0}% latency reduction",
                ((result.mean_time_ms - goal) / result.mean_time_ms) * 100.0),
        }
    }

    fn generate_memory_recommendation(
        &self,
        result: &BenchmarkResult,
        severity: Severity,
    ) -> OptimizationRecommendation {
        OptimizationRecommendation {
            severity,
            command: result.command.clone(),
            issue: format!(
                "Memory usage exceeds target by {:.1}%",
                (result.memory_usage_mb / self.goals.memory_max_mb - 1.0) * 100.0
            ),
            current_performance: format!("{:.2}MB", result.memory_usage_mb),
            target_performance: format!("{:.2}MB", self.goals.memory_max_mb),
            recommendations: vec![
                "Use iterators instead of collecting into vectors".to_string(),
                "Implement streaming for large data processing".to_string(),
                "Release unused allocations eagerly".to_string(),
                "Consider using smaller data structures (e.g., SmallVec)".to_string(),
                "Profile memory allocations to identify hotspots".to_string(),
            ],
            estimated_improvement: format!(
                "{:.0}% memory reduction",
                ((result.memory_usage_mb - self.goals.memory_max_mb) / result.memory_usage_mb) * 100.0
            ),
        }
    }

    fn generate_reliability_recommendation(
        &self,
        result: &BenchmarkResult,
    ) -> OptimizationRecommendation {
        OptimizationRecommendation {
            severity: Severity::High,
            command: result.command.clone(),
            issue: format!(
                "Success rate {:.2}% below target {:.2}%",
                result.success_rate * 100.0,
                self.goals.min_success_rate * 100.0
            ),
            current_performance: format!("{:.2}% success rate", result.success_rate * 100.0),
            target_performance: format!("{:.2}% success rate", self.goals.min_success_rate * 100.0),
            recommendations: vec![
                "Add retry logic for transient failures".to_string(),
                "Improve error handling and recovery".to_string(),
                "Add circuit breakers for external dependencies".to_string(),
                "Validate inputs more thoroughly".to_string(),
                "Add comprehensive logging for failure analysis".to_string(),
            ],
            estimated_improvement: "Improved reliability and user trust".to_string(),
        }
    }

    fn generate_summary(&self, stats: &HashMap<String, Vec<BenchmarkResult>>) -> PerformanceSummary {
        let mut command_summaries = Vec::new();

        for (command, results) in stats {
            let mean_times: Vec<f64> = results.iter().map(|r| r.mean_time_ms).collect();
            let memory_usages: Vec<f64> = results.iter().map(|r| r.memory_usage_mb).collect();

            command_summaries.push(CommandSummary {
                command: command.clone(),
                samples: results.len(),
                avg_latency_ms: mean_times.iter().sum::<f64>() / mean_times.len() as f64,
                min_latency_ms: mean_times.iter().cloned().fold(f64::INFINITY, f64::min),
                max_latency_ms: mean_times.iter().cloned().fold(f64::NEG_INFINITY, f64::max),
                avg_memory_mb: memory_usages.iter().sum::<f64>() / memory_usages.len() as f64,
                avg_throughput: results.iter().map(|r| r.throughput).sum::<f64>() / results.len() as f64,
            });
        }

        PerformanceSummary {
            total_benchmarks: self.results.len(),
            commands_tested: stats.len(),
            command_summaries,
        }
    }

    fn calculate_baseline_metrics(&self) -> BaselineMetrics {
        let all_times: Vec<f64> = self.results.iter().map(|r| r.mean_time_ms).collect();
        let all_memory: Vec<f64> = self.results.iter().map(|r| r.memory_usage_mb).collect();
        let all_throughput: Vec<f64> = self.results.iter().map(|r| r.throughput).collect();

        BaselineMetrics {
            median_latency_ms: Self::median(&all_times),
            p95_latency_ms: Self::percentile(&all_times, 95.0),
            p99_latency_ms: Self::percentile(&all_times, 99.0),
            median_memory_mb: Self::median(&all_memory),
            median_throughput: Self::median(&all_throughput),
        }
    }

    fn analyze_trends(&self, stats: &HashMap<String, Vec<BenchmarkResult>>) -> PerformanceTrends {
        let mut scaling_analysis = Vec::new();

        for (command, results) in stats {
            if results.len() >= 2 {
                // Sort by dataset size
                let mut sorted_results = results.clone();
                sorted_results.sort_by_key(|r| r.dataset_size);

                // Calculate scaling factor
                let small = &sorted_results[0];
                let large = &sorted_results[sorted_results.len() - 1];

                let size_ratio = large.dataset_size as f64 / small.dataset_size as f64;
                let time_ratio = large.mean_time_ms / small.mean_time_ms;
                let scaling_factor = time_ratio / size_ratio;

                let complexity = if scaling_factor < 1.2 {
                    "O(1) - Constant time (excellent!)".to_string()
                } else if scaling_factor < 2.0 {
                    "O(log n) - Logarithmic scaling (very good)".to_string()
                } else if scaling_factor < 3.0 {
                    "O(n) - Linear scaling (good)".to_string()
                } else if scaling_factor < 5.0 {
                    "O(n log n) - Linearithmic scaling (acceptable)".to_string()
                } else {
                    "O(n²) or worse - Poor scaling (needs optimization)".to_string()
                };

                scaling_analysis.push(ScalingAnalysis {
                    command: command.clone(),
                    min_dataset_size: small.dataset_size,
                    max_dataset_size: large.dataset_size,
                    scaling_factor,
                    estimated_complexity: complexity,
                });
            }
        }

        PerformanceTrends { scaling_analysis }
    }

    fn median(values: &[f64]) -> f64 {
        let mut sorted = values.to_vec();
        sorted.sort_by(|a, b| a.partial_cmp(b).unwrap());
        let mid = sorted.len() / 2;
        if sorted.len() % 2 == 0 {
            (sorted[mid - 1] + sorted[mid]) / 2.0
        } else {
            sorted[mid]
        }
    }

    fn percentile(values: &[f64], p: f64) -> f64 {
        let mut sorted = values.to_vec();
        sorted.sort_by(|a, b| a.partial_cmp(b).unwrap());
        let index = ((p / 100.0) * sorted.len() as f64) as usize;
        sorted[index.min(sorted.len() - 1)]
    }
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct AnalysisReport {
    pub summary: PerformanceSummary,
    pub goals: PerformanceGoals,
    pub recommendations: Vec<OptimizationRecommendation>,
    pub baseline_metrics: BaselineMetrics,
    pub performance_trends: PerformanceTrends,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct PerformanceSummary {
    pub total_benchmarks: usize,
    pub commands_tested: usize,
    pub command_summaries: Vec<CommandSummary>,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct CommandSummary {
    pub command: String,
    pub samples: usize,
    pub avg_latency_ms: f64,
    pub min_latency_ms: f64,
    pub max_latency_ms: f64,
    pub avg_memory_mb: f64,
    pub avg_throughput: f64,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct BaselineMetrics {
    pub median_latency_ms: f64,
    pub p95_latency_ms: f64,
    pub p99_latency_ms: f64,
    pub median_memory_mb: f64,
    pub median_throughput: f64,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct PerformanceTrends {
    pub scaling_analysis: Vec<ScalingAnalysis>,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct ScalingAnalysis {
    pub command: String,
    pub min_dataset_size: usize,
    pub max_dataset_size: usize,
    pub scaling_factor: f64,
    pub estimated_complexity: String,
}

impl AnalysisReport {
    pub fn print_summary(&self) {
        println!("\n{}", "=".repeat(80));
        println!("MARKETPLACE PERFORMANCE BENCHMARK RESULTS");
        println!("{}\n", "=".repeat(80));

        // Summary
        println!("📊 Summary");
        println!("  Total Benchmarks: {}", self.summary.total_benchmarks);
        println!("  Commands Tested: {}", self.summary.commands_tested);
        println!();

        // Performance Goals
        println!("🎯 Performance Goals");
        println!("  Interactive Commands: < {:.0}ms", self.goals.interactive_max_ms);
        println!("  Report Generation: < {:.0}ms", self.goals.report_max_ms);
        println!("  Memory Usage: < {:.0}MB", self.goals.memory_max_mb);
        println!("  Success Rate: > {:.1}%", self.goals.min_success_rate * 100.0);
        println!();

        // Baseline Metrics
        println!("📈 Baseline Metrics");
        println!("  Median Latency: {:.2}ms", self.baseline_metrics.median_latency_ms);
        println!("  P95 Latency: {:.2}ms", self.baseline_metrics.p95_latency_ms);
        println!("  P99 Latency: {:.2}ms", self.baseline_metrics.p99_latency_ms);
        println!("  Median Memory: {:.2}MB", self.baseline_metrics.median_memory_mb);
        println!("  Median Throughput: {:.2} ops/sec", self.baseline_metrics.median_throughput);
        println!();

        // Command Summaries
        println!("📋 Command Performance");
        println!("  {:<30} {:>12} {:>12} {:>12}", "Command", "Avg (ms)", "Memory (MB)", "Throughput");
        println!("  {}", "-".repeat(78));
        for cmd in &self.summary.command_summaries {
            println!(
                "  {:<30} {:>12.2} {:>12.2} {:>12.2}",
                cmd.command, cmd.avg_latency_ms, cmd.avg_memory_mb, cmd.avg_throughput
            );
        }
        println!();

        // Scaling Analysis
        if !self.performance_trends.scaling_analysis.is_empty() {
            println!("📊 Scaling Analysis");
            for analysis in &self.performance_trends.scaling_analysis {
                println!("  {} ({} → {} items)",
                    analysis.command,
                    analysis.min_dataset_size,
                    analysis.max_dataset_size
                );
                println!("    Scaling Factor: {:.2}x", analysis.scaling_factor);
                println!("    Complexity: {}", analysis.estimated_complexity);
            }
            println!();
        }

        // Recommendations
        if !self.recommendations.is_empty() {
            println!("⚠️  Optimization Recommendations ({} total)", self.recommendations.len());
            println!();

            for (i, rec) in self.recommendations.iter().enumerate() {
                let severity_icon = match rec.severity {
                    Severity::Critical => "🔴",
                    Severity::High => "🟠",
                    Severity::Medium => "🟡",
                    Severity::Low => "🔵",
                    Severity::Info => "ℹ️",
                };

                println!("{}. {} [{:?}] {}", i + 1, severity_icon, rec.severity, rec.command);
                println!("   Issue: {}", rec.issue);
                println!("   Current: {} | Target: {}", rec.current_performance, rec.target_performance);
                println!("   Recommendations:");
                for (j, suggestion) in rec.recommendations.iter().enumerate() {
                    println!("     {}. {}", j + 1, suggestion);
                }
                println!("   Expected Impact: {}", rec.estimated_improvement);
                println!();
            }
        } else {
            println!("✅ All performance goals met! No optimization recommendations.");
            println!();
        }

        println!("{}\n", "=".repeat(80));
    }

    pub fn save_to_file<P: AsRef<Path>>(&self, path: P) -> Result<(), Box<dyn std::error::Error>> {
        let json = serde_json::to_string_pretty(self)?;
        fs::write(path, json)?;
        Ok(())
    }

    pub fn save_markdown<P: AsRef<Path>>(&self, path: P) -> Result<(), Box<dyn std::error::Error>> {
        let mut md = String::new();

        md.push_str("# Marketplace Performance Benchmark Results\n\n");

        md.push_str("## Summary\n\n");
        md.push_str(&format!("- **Total Benchmarks**: {}\n", self.summary.total_benchmarks));
        md.push_str(&format!("- **Commands Tested**: {}\n\n", self.summary.commands_tested));

        md.push_str("## Performance Goals\n\n");
        md.push_str(&format!("- Interactive Commands: < {:.0}ms\n", self.goals.interactive_max_ms));
        md.push_str(&format!("- Report Generation: < {:.0}ms\n", self.goals.report_max_ms));
        md.push_str(&format!("- Memory Usage: < {:.0}MB\n", self.goals.memory_max_mb));
        md.push_str(&format!("- Success Rate: > {:.1}%\n\n", self.goals.min_success_rate * 100.0));

        md.push_str("## Baseline Metrics\n\n");
        md.push_str(&format!("- Median Latency: {:.2}ms\n", self.baseline_metrics.median_latency_ms));
        md.push_str(&format!("- P95 Latency: {:.2}ms\n", self.baseline_metrics.p95_latency_ms));
        md.push_str(&format!("- P99 Latency: {:.2}ms\n", self.baseline_metrics.p99_latency_ms));
        md.push_str(&format!("- Median Memory: {:.2}MB\n", self.baseline_metrics.median_memory_mb));
        md.push_str(&format!("- Median Throughput: {:.2} ops/sec\n\n", self.baseline_metrics.median_throughput));

        md.push_str("## Command Performance\n\n");
        md.push_str("| Command | Avg Latency (ms) | Memory (MB) | Throughput (ops/s) |\n");
        md.push_str("|---------|------------------|-------------|--------------------|\n");
        for cmd in &self.summary.command_summaries {
            md.push_str(&format!(
                "| {} | {:.2} | {:.2} | {:.2} |\n",
                cmd.command, cmd.avg_latency_ms, cmd.avg_memory_mb, cmd.avg_throughput
            ));
        }
        md.push_str("\n");

        if !self.recommendations.is_empty() {
            md.push_str("## Optimization Recommendations\n\n");
            for (i, rec) in self.recommendations.iter().enumerate() {
                md.push_str(&format!("### {}. {} [{:?}]\n\n", i + 1, rec.command, rec.severity));
                md.push_str(&format!("**Issue**: {}\n\n", rec.issue));
                md.push_str(&format!("**Current**: {} | **Target**: {}\n\n", rec.current_performance, rec.target_performance));
                md.push_str("**Recommendations**:\n\n");
                for suggestion in &rec.recommendations {
                    md.push_str(&format!("- {}\n", suggestion));
                }
                md.push_str(&format!("\n**Expected Impact**: {}\n\n", rec.estimated_improvement));
            }
        }

        fs::write(path, md)?;
        Ok(())
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_performance_analyzer() {
        let goals = PerformanceGoals::default();
        let mut analyzer = PerformanceAnalyzer::new(goals);

        analyzer.add_result(BenchmarkResult {
            command: "search".to_string(),
            dataset_size: 100,
            mean_time_ms: 250.0,
            median_time_ms: 240.0,
            p95_time_ms: 300.0,
            p99_time_ms: 350.0,
            throughput: 4.0,
            memory_usage_mb: 50.0,
            success_rate: 0.99,
        });

        let report = analyzer.analyze();
        assert!(report.recommendations.is_empty()); // Should meet goals
    }

    #[test]
    fn test_severity_calculation() {
        let analyzer = PerformanceAnalyzer::new(PerformanceGoals::default());

        assert_eq!(analyzer.calculate_severity(100.0, 50.0), Severity::Critical);
        assert_eq!(analyzer.calculate_severity(80.0, 50.0), Severity::High);
        assert_eq!(analyzer.calculate_severity(65.0, 50.0), Severity::Medium);
        assert_eq!(analyzer.calculate_severity(55.0, 50.0), Severity::Low);
        assert_eq!(analyzer.calculate_severity(45.0, 50.0), Severity::Info);
    }
}
