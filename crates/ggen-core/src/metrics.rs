//! Quality Metrics System for ggen
//!
//! Comprehensive metrics collection and analysis covering:
//! - Code metrics (lines, complexity, coverage)
//! - Process metrics (cycle time, throughput, yield)
//! - Defect metrics (Six Sigma: DPU, DPO, DPMO, Sigma level)
//! - Waste metrics (TPS: 7 wastes, Mura, Muri)
//! - Flow metrics (lead time, WIP, queue time)
//! - OEE metrics (availability, performance, quality)
//! - Kaizen metrics (improvement rate, suggestion rate)

use serde::{Deserialize, Serialize};
use std::collections::HashMap;
use std::time::{Duration, SystemTime};

///////////////////////////////////////////////////////////////////
//    Code Metrics
///////////////////////////////////////////////////////////////////

/// Metrics for code quality and complexity
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct CodeMetrics {
    /// Total lines of code
    pub total_lines: usize,
    /// Lines of actual code (excluding comments/blank)
    pub code_lines: usize,
    /// Comment lines
    pub comment_lines: usize,
    /// Blank lines
    pub blank_lines: usize,
    /// Cyclomatic complexity (average)
    pub avg_complexity: f64,
    /// Maximum complexity in any function
    pub max_complexity: usize,
    /// Test coverage percentage (0-100)
    pub test_coverage: f64,
    /// Number of public functions
    pub public_functions: usize,
    /// Number of types (structs, enums)
    pub types_count: usize,
    /// Number of unsafe blocks
    pub unsafe_blocks: usize,
    /// Number of unwrap()/expect() calls
    pub unwrap_calls: usize,
}

impl CodeMetrics {
    /// Create empty code metrics
    pub fn new() -> Self {
        Self {
            total_lines: 0,
            code_lines: 0,
            comment_lines: 0,
            blank_lines: 0,
            avg_complexity: 0.0,
            max_complexity: 0,
            test_coverage: 0.0,
            public_functions: 0,
            types_count: 0,
            unsafe_blocks: 0,
            unwrap_calls: 0,
        }
    }

    /// Calculate code quality score (0-100)
    pub fn quality_score(&self) -> f64 {
        let coverage_score = self.test_coverage;
        let complexity_score = if self.avg_complexity <= 5.0 {
            100.0
        } else if self.avg_complexity <= 10.0 {
            100.0 - (self.avg_complexity - 5.0) * 10.0
        } else {
            50.0
        };
        let safety_score = if self.unwrap_calls == 0 {
            100.0
        } else {
            (100.0 - (self.unwrap_calls as f64 * 10.0)).max(0.0)
        };

        coverage_score * 0.5 + complexity_score * 0.3 + safety_score * 0.2
    }
}

///////////////////////////////////////////////////////////////////
//    Process Metrics
///////////////////////////////////////////////////////////////////

/// Metrics for development process efficiency
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct ProcessMetrics {
    /// Total cycle time (idea to production)
    pub cycle_time: Duration,
    /// Active development time
    pub development_time: Duration,
    /// Wait time (reviews, approvals, queueing)
    pub wait_time: Duration,
    /// Throughput (features per time period)
    pub throughput: f64,
    /// First-time yield (percentage passing all gates first time)
    pub first_time_yield: f64,
    /// Rework percentage
    pub rework_percentage: f64,
    /// Number of quality gates
    pub quality_gates: usize,
    /// Number of gates passed
    pub gates_passed: usize,
}

impl ProcessMetrics {
    /// Create empty process metrics
    pub fn new() -> Self {
        Self {
            cycle_time: Duration::ZERO,
            development_time: Duration::ZERO,
            wait_time: Duration::ZERO,
            throughput: 0.0,
            first_time_yield: 100.0,
            rework_percentage: 0.0,
            quality_gates: 11,
            gates_passed: 0,
        }
    }

    /// Calculate process efficiency (active time / total time)
    pub fn efficiency(&self) -> f64 {
        let total = self.cycle_time.as_secs_f64();
        if total == 0.0 {
            0.0
        } else {
            (self.development_time.as_secs_f64() / total) * 100.0
        }
    }

    /// Calculate process velocity (features per week)
    pub fn velocity(&self) -> f64 {
        self.throughput * 7.0 // Assume throughput is per day
    }
}

///////////////////////////////////////////////////////////////////
//    Defect Metrics (Six Sigma)
///////////////////////////////////////////////////////////////////

/// Six Sigma defect metrics
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct DefectMetrics {
    /// Total number of units produced
    pub total_units: usize,
    /// Number of defective units
    pub defective_units: usize,
    /// Total number of defects found
    pub total_defects: usize,
    /// Total number of opportunities for defect per unit
    pub opportunities_per_unit: usize,
    /// Total defect opportunities
    pub total_opportunities: usize,
}

impl DefectMetrics {
    /// Create empty defect metrics
    pub fn new() -> Self {
        Self {
            total_units: 0,
            defective_units: 0,
            total_defects: 0,
            opportunities_per_unit: 11, // 11 quality gates
            total_opportunities: 0,
        }
    }

    /// Calculate Defects Per Unit (DPU)
    pub fn dpu(&self) -> f64 {
        if self.total_units == 0 {
            0.0
        } else {
            self.total_defects as f64 / self.total_units as f64
        }
    }

    /// Calculate Defects Per Opportunity (DPO)
    pub fn dpo(&self) -> f64 {
        let total_ops = self.total_units * self.opportunities_per_unit;
        if total_ops == 0 {
            0.0
        } else {
            self.total_defects as f64 / total_ops as f64
        }
    }

    /// Calculate Defects Per Million Opportunities (DPMO)
    pub fn dpmo(&self) -> f64 {
        self.dpo() * 1_000_000.0
    }

    /// Calculate Sigma level (0-6)
    pub fn sigma_level(&self) -> f64 {
        let dpmo = self.dpmo();
        // Add small epsilon for floating point comparison
        let epsilon = 0.01;
        // Approximate sigma level from DPMO
        if dpmo <= 3.4 + epsilon {
            6.0
        } else if dpmo <= 233.0 + epsilon {
            5.0
        } else if dpmo <= 6210.0 + epsilon {
            4.0
        } else if dpmo <= 66807.0 + epsilon {
            3.0
        } else if dpmo <= 308537.0 + epsilon {
            2.0
        } else if dpmo <= 691462.0 + epsilon {
            1.0
        } else {
            0.0
        }
    }

    /// Calculate yield percentage
    pub fn yield_percentage(&self) -> f64 {
        if self.total_units == 0 {
            100.0
        } else {
            ((self.total_units - self.defective_units) as f64 / self.total_units as f64) * 100.0
        }
    }
}

///////////////////////////////////////////////////////////////////
//    Waste Metrics (Toyota Production System)
///////////////////////////////////////////////////////////////////

/// The 7 Wastes of TPS (Muda)
#[derive(Debug, Clone, Serialize, Deserialize, Hash, Eq, PartialEq)]
pub enum WasteType {
    /// Overproduction (making more than needed)
    Overproduction,
    /// Waiting (delays, idle time)
    Waiting,
    /// Transportation (moving materials unnecessarily)
    Transportation,
    /// Overprocessing (doing more than required)
    Overprocessing,
    /// Inventory (excess stock/work-in-progress)
    Inventory,
    /// Motion (unnecessary movement of people)
    Motion,
    /// Defects (rework, scrap)
    Defects,
    /// Mura (unevenness, irregularity)
    Mura,
    /// Muri (overburden, unreasonable strain)
    Muri,
}

/// Waste metrics for TPS
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct WasteMetrics {
    /// Waste count by type
    pub waste_counts: HashMap<WasteType, usize>,
    /// Total waste events
    pub total_waste: usize,
    /// Waste reduction percentage (from baseline)
    pub waste_reduction: f64,
}

impl WasteMetrics {
    /// Create empty waste metrics
    pub fn new() -> Self {
        Self {
            waste_counts: HashMap::new(),
            total_waste: 0,
            waste_reduction: 0.0,
        }
    }

    /// Add waste event
    pub fn add_waste(&mut self, waste_type: WasteType) {
        *self.waste_counts.entry(waste_type).or_insert(0) += 1;
        self.total_waste += 1;
    }

    /// Get most common waste type
    pub fn most_common_waste(&self) -> Option<&WasteType> {
        self.waste_counts
            .iter()
            .max_by_key(|(_, &count)| count)
            .map(|(waste_type, _)| waste_type)
    }

    /// Calculate waste score (lower is better, 0-100)
    pub fn waste_score(&self) -> f64 {
        // Base score starts at 100, subtract for each waste
        let mut score = 100.0;
        for (waste_type, count) in &self.waste_counts {
            let penalty = match waste_type {
                WasteType::Defects => 15.0, // Most serious
                WasteType::Overproduction => 10.0,
                WasteType::Inventory => 8.0,
                WasteType::Overprocessing => 5.0,
                WasteType::Waiting => 3.0,
                WasteType::Transportation => 2.0,
                WasteType::Motion => 1.0,
                WasteType::Mura => 5.0,
                WasteType::Muri => 10.0,
            };
            score -= penalty * *count as f64;
        }
        score.max(0.0)
    }
}

///////////////////////////////////////////////////////////////////
//    Flow Metrics
///////////////////////////////////////////////////////////////////

/// Metrics for flow efficiency
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct FlowMetrics {
    /// Lead time (start to finish)
    pub lead_time: Duration,
    /// Active work time
    pub active_time: Duration,
    /// Wait time (blocked, waiting)
    pub wait_time: Duration,
    /// Work In Progress count
    pub wip: usize,
    /// Queue time (time waiting to be worked on)
    pub queue_time: Duration,
    /// Batch size
    pub batch_size: usize,
}

impl FlowMetrics {
    /// Create empty flow metrics
    pub fn new() -> Self {
        Self {
            lead_time: Duration::ZERO,
            active_time: Duration::ZERO,
            wait_time: Duration::ZERO,
            wip: 0,
            queue_time: Duration::ZERO,
            batch_size: 1,
        }
    }

    /// Calculate flow efficiency (active time / lead time)
    pub fn flow_efficiency(&self) -> f64 {
        let lead = self.lead_time.as_secs_f64();
        if lead == 0.0 {
            0.0
        } else {
            (self.active_time.as_secs_f64() / lead) * 100.0
        }
    }

    /// Calculate Little's Law expected throughput
    pub fn expected_throughput(&self) -> f64 {
        let lead = self.lead_time.as_secs_f64();
        if lead == 0.0 {
            0.0
        } else {
            self.wip as f64 / lead
        }
    }
}

///////////////////////////////////////////////////////////////////
//    OEE Metrics (Overall Equipment Effectiveness)
///////////////////////////////////////////////////////////////////

/// OEE metrics for production efficiency
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct OEEMetrics {
    /// Availability percentage (0-100)
    pub availability: f64,
    /// Performance percentage (0-100)
    pub performance: f64,
    /// Quality percentage (0-100)
    pub quality: f64,
    /// OEE score (availability * performance * quality / 10000)
    pub oee: f64,
}

impl OEEMetrics {
    /// Create empty OEE metrics
    pub fn new() -> Self {
        Self {
            availability: 100.0,
            performance: 100.0,
            quality: 100.0,
            oee: 100.0,
        }
    }

    /// Calculate OEE score
    pub fn calculate(&mut self) {
        self.oee = (self.availability * self.performance * self.quality) / 10_000.0;
    }

    /// Check if world-class OEE (>= 85%)
    pub fn is_world_class(&self) -> bool {
        self.oee >= 85.0
    }
}

///////////////////////////////////////////////////////////////////
//    Kaizen Metrics
///////////////////////////////////////////////////////////////////

/// Metrics for continuous improvement
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct KaizenMetrics {
    /// Number of improvement suggestions
    pub suggestions: usize,
    /// Number of suggestions implemented
    pub implemented: usize,
    /// Number of kaizen events held
    pub kaizen_events: usize,
    /// Improvement rate (implemented / suggestions)
    pub improvement_rate: f64,
    /// Time since last kaizen event
    pub days_since_last_kaizen: usize,
}

impl KaizenMetrics {
    /// Create empty kaizen metrics
    pub fn new() -> Self {
        Self {
            suggestions: 0,
            implemented: 0,
            kaizen_events: 0,
            improvement_rate: 0.0,
            days_since_last_kaizen: 0,
        }
    }

    /// Calculate improvement rate
    pub fn calculate_improvement_rate(&mut self) {
        if self.suggestions == 0 {
            self.improvement_rate = 0.0;
        } else {
            self.improvement_rate = (self.implemented as f64 / self.suggestions as f64) * 100.0;
        }
    }
}

///////////////////////////////////////////////////////////////////
//    Comprehensive Metrics Report
///////////////////////////////////////////////////////////////////

/// Complete quality metrics report
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct MetricsReport {
    /// Timestamp of report
    pub timestamp: SystemTime,
    /// Code metrics
    pub code: CodeMetrics,
    /// Process metrics
    pub process: ProcessMetrics,
    /// Defect metrics (Six Sigma)
    pub defects: DefectMetrics,
    /// Waste metrics (TPS)
    pub waste: WasteMetrics,
    /// Flow metrics
    pub flow: FlowMetrics,
    /// OEE metrics
    pub oee: OEEMetrics,
    /// Kaizen metrics
    pub kaizen: KaizenMetrics,
}

impl MetricsReport {
    /// Create empty metrics report
    pub fn new() -> Self {
        Self {
            timestamp: SystemTime::now(),
            code: CodeMetrics::new(),
            process: ProcessMetrics::new(),
            defects: DefectMetrics::new(),
            waste: WasteMetrics::new(),
            flow: FlowMetrics::new(),
            oee: OEEMetrics::new(),
            kaizen: KaizenMetrics::new(),
        }
    }

    /// Calculate overall quality score (0-100)
    pub fn overall_score(&self) -> f64 {
        let code_score = self.code.quality_score();
        let process_score = self.process.efficiency();
        let sigma_score = self.defects.sigma_level() * 100.0 / 6.0;
        let waste_score = self.waste.waste_score();
        let flow_score = self.flow.flow_efficiency();
        let oee_score = self.oee.oee;
        let kaizen_score = self.kaizen.improvement_rate;

        // Weighted average
        code_score * 0.25
            + process_score * 0.15
            + sigma_score * 0.20
            + waste_score * 0.15
            + flow_score * 0.10
            + oee_score * 0.10
            + kaizen_score * 0.05
    }

    /// Generate markdown report
    pub fn to_markdown(&self) -> String {
        use std::time::UNIX_EPOCH;
        let timestamp_secs = self
            .timestamp
            .duration_since(UNIX_EPOCH)
            .unwrap_or_default()
            .as_secs();

        format!(
            "# Quality Metrics Report\n\
             Generated: {timestamp_secs}\n\
             \n\
             ## Overall Score: {overall:.1}/100\n\
             \n\
             ## Code Metrics\n\
             - Total Lines: {total_lines}\n\
             - Code Lines: {code_lines}\n\
             - Test Coverage: {coverage:.1}%\n\
             - Avg Complexity: {complexity:.1}\n\
             - Unwrap Calls: {unwraps}\n\
             - Quality Score: {code_score:.1}/100\n\
             \n\
             ## Process Metrics\n\
             - Cycle Time: {cycle_time:.2}s\n\
             - Efficiency: {efficiency:.1}%\n\
             - First Time Yield: {fty:.1}%\n\
             - Quality Gates: {gates_passed}/{gates_total}\n\
             \n\
             ## Six Sigma Metrics\n\
             - DPU: {dpu:.4}\n\
             - DPMO: {dpmo:.0}\n\
             - Sigma Level: {sigma:.1}/6.0\n\
             - Yield: {yield_pct:.1}%\n\
             \n\
             ## TPS Waste Metrics\n\
             - Total Waste Events: {total_waste}\n\
             - Waste Score: {waste_score:.1}/100\n\
             - Waste Reduction: {reduction:.1}%\n\
             \n\
             ## Flow Metrics\n\
             - Lead Time: {lead_time:.2}s\n\
             - Flow Efficiency: {flow_eff:.1}%\n\
             - WIP: {wip}\n\
             \n\
             ## OEE Metrics\n\
             - Availability: {avail:.1}%\n\
             - Performance: {perf:.1}%\n\
             - Quality: {qual:.1}%\n\
             - OEE: {oee:.1}%\n\
             \n\
             ## Kaizen Metrics\n\
             - Suggestions: {suggestions}\n\
             - Implemented: {implemented}\n\
             - Improvement Rate: {imp_rate:.1}%\n",
            timestamp_secs = timestamp_secs,
            overall = self.overall_score(),
            total_lines = self.code.total_lines,
            code_lines = self.code.code_lines,
            coverage = self.code.test_coverage,
            complexity = self.code.avg_complexity,
            unwraps = self.code.unwrap_calls,
            code_score = self.code.quality_score(),
            cycle_time = self.process.cycle_time.as_secs_f64(),
            efficiency = self.process.efficiency(),
            fty = self.process.first_time_yield,
            gates_passed = self.process.gates_passed,
            gates_total = self.process.quality_gates,
            dpu = self.defects.dpu(),
            dpmo = self.defects.dpmo(),
            sigma = self.defects.sigma_level(),
            yield_pct = self.defects.yield_percentage(),
            total_waste = self.waste.total_waste,
            waste_score = self.waste.waste_score(),
            reduction = self.waste.waste_reduction,
            lead_time = self.flow.lead_time.as_secs_f64(),
            flow_eff = self.flow.flow_efficiency(),
            wip = self.flow.wip,
            avail = self.oee.availability,
            perf = self.oee.performance,
            qual = self.oee.quality,
            oee = self.oee.oee,
            suggestions = self.kaizen.suggestions,
            implemented = self.kaizen.implemented,
            imp_rate = self.kaizen.improvement_rate
        )
    }
}

impl Default for MetricsReport {
    fn default() -> Self {
        Self::new()
    }
}

///////////////////////////////////////////////////////////////////
//    Metrics Collector
///////////////////////////////////////////////////////////////////

/// Collector for gathering metrics from code and process
#[derive(Debug, Clone)]
pub struct MetricsCollector {
    report: MetricsReport,
}

impl MetricsCollector {
    /// Create new metrics collector
    pub fn new() -> Self {
        Self {
            report: MetricsReport::new(),
        }
    }

    /// Collect code metrics from source files
    pub fn collect_code_metrics(&mut self, source_files: &[String]) -> Result<(), String> {
        for file_path in source_files {
            if let Ok(content) = std::fs::read_to_string(file_path) {
                self.analyze_source_file(&content);
            }
        }
        Ok(())
    }

    /// Analyze a single source file for code metrics
    pub fn analyze_source_file(&mut self, content: &str) {
        let lines: Vec<&str> = content.lines().collect();
        let mut code_lines = 0;
        let mut comment_lines = 0;
        let mut blank_lines = 0;
        let mut in_comment = false;

        for line in &lines {
            let trimmed = line.trim();
            if trimmed.is_empty() {
                blank_lines += 1;
            } else if trimmed.starts_with("//") || trimmed.starts_with("/*") {
                comment_lines += 1;
                if trimmed.contains("/*") && !trimmed.contains("*/") {
                    in_comment = true;
                }
            } else if in_comment {
                comment_lines += 1;
                if trimmed.contains("*/") {
                    in_comment = false;
                }
            } else {
                code_lines += 1;
            }
        }

        self.report.code.total_lines += lines.len();
        self.report.code.code_lines += code_lines;
        self.report.code.comment_lines += comment_lines;
        self.report.code.blank_lines += blank_lines;

        // Count unsafe blocks
        self.report.code.unsafe_blocks += content.matches("unsafe").count();

        // Count unwrap/expect calls
        self.report.code.unwrap_calls += content.matches(".unwrap()").count();
        self.report.code.unwrap_calls += content.matches(".expect(").count();
    }

    /// Add defect event
    pub fn add_defect(&mut self) {
        self.report.defects.total_defects += 1;
        self.report.defects.defective_units += 1;
        self.report.defects.total_opportunities =
            self.report.defects.total_units * self.report.defects.opportunities_per_unit;
    }

    /// Add waste event
    pub fn add_waste(&mut self, waste_type: WasteType) {
        self.report.waste.add_waste(waste_type);
    }

    /// Get current report
    pub fn report(&self) -> &MetricsReport {
        &self.report
    }

    /// Consume and return report
    pub fn into_report(self) -> MetricsReport {
        self.report
    }
}

impl Default for MetricsCollector {
    fn default() -> Self {
        Self::new()
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_code_metrics_quality_score() {
        let mut metrics = CodeMetrics::new();
        metrics.test_coverage = 80.0;
        metrics.avg_complexity = 4.0;
        metrics.unwrap_calls = 0;

        let score = metrics.quality_score();
        assert!(score >= 80.0, "Score should be >= 80 for good metrics");
    }

    #[test]
    fn test_defect_metrics_sigma_level() {
        let mut metrics = DefectMetrics::new();
        metrics.total_units = 1000;
        metrics.total_defects = 0;
        metrics.defective_units = 0;

        // 0 defects = 0 DPMO = 6 Sigma
        let sigma = metrics.sigma_level();
        assert_eq!(sigma, 6.0, "Defect-free should achieve 6 Sigma level");
    }

    #[test]
    fn test_waste_metrics_tracking() {
        let mut metrics = WasteMetrics::new();
        metrics.add_waste(WasteType::Defects);
        metrics.add_waste(WasteType::Waiting);
        metrics.add_waste(WasteType::Defects);

        assert_eq!(metrics.total_waste, 3);
        assert_eq!(*metrics.waste_counts.get(&WasteType::Defects).unwrap(), 2);
        assert_eq!(metrics.most_common_waste(), Some(&WasteType::Defects));
    }

    #[test]
    fn test_oee_calculation() {
        let mut metrics = OEEMetrics::new();
        metrics.availability = 90.0;
        metrics.performance = 85.0;
        metrics.quality = 95.0;

        metrics.calculate();

        assert!((metrics.oee - 72.675).abs() < 0.01);
        assert!(!metrics.is_world_class());
    }

    #[test]
    fn test_flow_efficiency() {
        let mut metrics = FlowMetrics::new();
        metrics.lead_time = Duration::from_secs(100);
        metrics.active_time = Duration::from_secs(40);

        let efficiency = metrics.flow_efficiency();
        assert_eq!(efficiency, 40.0);
    }

    #[test]
    fn test_metrics_collector() {
        let mut collector = MetricsCollector::new();
        let source_code = r#"
// This is a comment
fn main() {
    let x = 42;
    println!("{}", x);
}

/* Multi-line
   comment */
fn test() {
    let y = x.unwrap();
}
"#;

        collector.analyze_source_file(source_code);

        assert_eq!(collector.report().code.total_lines, 12);
        assert_eq!(collector.report().code.unwrap_calls, 1);
    }

    #[test]
    fn test_overall_score_calculation() {
        let mut report = MetricsReport::new();
        report.code.test_coverage = 85.0;
        report.process.development_time = Duration::from_secs(40);
        report.process.cycle_time = Duration::from_secs(50);
        report.defects.total_units = 1000;
        report.defects.total_defects = 10;

        let score = report.overall_score();
        assert!(score > 0.0 && score <= 100.0);
    }

    #[test]
    fn test_markdown_report_generation() {
        let report = MetricsReport::new();
        let markdown = report.to_markdown();

        assert!(markdown.contains("# Quality Metrics Report"));
        assert!(markdown.contains("## Overall Score"));
        assert!(markdown.contains("## Code Metrics"));
    }

    #[test]
    fn test_waste_score_calculation() {
        let mut metrics = WasteMetrics::new();
        // Start with 100, defects remove 15 each
        metrics.add_waste(WasteType::Defects);
        metrics.add_waste(WasteType::Waiting);

        let score = metrics.waste_score();
        assert_eq!(score, 82.0); // 100 - 15 - 3
    }
}
