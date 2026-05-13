//! Quality Assurance Framework
//!
//! Implements Lean Manufacturing principles (FMEA, POKA-YOKE, MURA, MUDA) for ontology systems:
//! - FMEA: Failure Mode and Effects Analysis
//! - POKA-YOKE: Mistake-proofing (defect prevention)
//! - MURA: Eliminate unevenness (standardization)
//! - MUDA: Eliminate waste (optimization)

use serde::{Deserialize, Serialize};
use std::collections::BTreeMap;
use std::fmt;

/// Failure Mode and Effects Analysis (FMEA)
///
/// Systematically identifies potential failures and their impacts
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct FMEA {
    /// Process/component being analyzed
    pub subject: String,

    /// Identified failure modes
    pub failure_modes: Vec<FailureMode>,

    /// RPN (Risk Priority Number) threshold
    pub rpn_threshold: u32,

    /// Analysis timestamp
    pub analyzed_at: String,

    /// Critical failures (RPN >= threshold)
    pub critical_failures: Vec<(FailureMode, u32)>,
}

/// A potential failure mode
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct FailureMode {
    /// Failure description
    pub name: String,

    /// What could go wrong
    pub potential_cause: String,

    /// Impact on system
    pub potential_effect: String,

    /// Severity (1-10)
    pub severity: u32,

    /// How often it occurs (1-10)
    pub occurrence: u32,

    /// How often it's detected (1-10, inverse scoring)
    pub detection: u32,

    /// Mitigation/prevention actions
    pub preventive_actions: Vec<String>,

    /// Current controls
    pub current_controls: Vec<String>,

    /// Risk Priority Number (Severity × Occurrence × Detection)
    pub rpn: u32,
}

/// POKA-YOKE: Mistake-Proofing
///
/// Prevents or detects errors before they propagate
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct PokaYoke {
    /// Error prevention rules
    pub prevention_rules: Vec<PreventionRule>,

    /// Detection mechanisms
    pub detection_mechanisms: Vec<DetectionMechanism>,

    /// Statistics
    pub stats: PokaYokeStats,
}

/// A mistake-proofing rule (preventive)
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct PreventionRule {
    /// Rule identifier
    pub id: String,

    /// What error does it prevent
    pub prevents: String,

    /// Rule implementation
    pub implementation: String,

    /// Type of prevention
    pub rule_type: PreventionType,

    /// Is this rule enforced
    pub enforced: bool,

    /// Cost to implement
    pub implementation_cost: String,
}

/// Prevention types
#[derive(Debug, Clone, Copy, Serialize, Deserialize, PartialEq, Eq)]
pub enum PreventionType {
    /// Physically impossible to err
    Physical,

    /// System warns before error
    Warning,

    /// System requires confirmation
    Confirmation,

    /// System auto-corrects
    Correction,

    /// System forces correct usage
    Forced,
}

/// Error detection mechanism
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct DetectionMechanism {
    /// Mechanism identifier
    pub id: String,

    /// What it detects
    pub detects: String,

    /// Detection method
    pub method: String,

    /// Detection rate (%)
    pub detection_rate: f32,

    /// Response time
    pub response_time_ms: u32,

    /// Is automated
    pub automated: bool,
}

/// POKA-YOKE statistics
#[derive(Debug, Clone, Default, Serialize, Deserialize)]
pub struct PokaYokeStats {
    /// Errors prevented
    pub errors_prevented: u32,

    /// Errors detected
    pub errors_detected: u32,

    /// Defects caught (not in production)
    pub defects_caught: u32,

    /// False positives
    pub false_positives: u32,
}

/// MURA: Eliminate Unevenness
///
/// Standardizes processes for consistency
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct MURA {
    /// Standard procedures
    pub standards: Vec<Standard>,

    /// Consistency metrics
    pub consistency_metrics: BTreeMap<String, f32>,

    /// Unevenness violations
    pub violations: Vec<MuraViolation>,
}

/// A standard procedure
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct Standard {
    /// Standard identifier
    pub id: String,

    /// What it standardizes
    pub name: String,

    /// Standard definition
    pub definition: String,

    /// Expected variations (%)
    pub acceptable_variance: f32,

    /// Is this critical
    pub is_critical: bool,

    /// How to verify compliance
    pub verification_method: String,
}

/// Unevenness violation
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct MuraViolation {
    /// Violated standard
    pub standard_id: String,

    /// Actual variance (%)
    pub actual_variance: f32,

    /// Severity (LOW, MEDIUM, HIGH)
    pub severity: ViolationSeverity,

    /// Description
    pub description: String,

    /// Corrective action
    pub corrective_action: String,
}

#[derive(Debug, Clone, Copy, Serialize, Deserialize, PartialEq, Eq, PartialOrd, Ord)]
pub enum ViolationSeverity {
    Low,
    Medium,
    High,
}

/// MUDA: Eliminate Waste
///
/// Identifies and eliminates non-value-adding activities
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct MUDA {
    /// Types of waste identified
    pub waste_types: Vec<WasteType>,

    /// Waste in current process
    pub waste_items: Vec<WasteItem>,

    /// Optimization opportunities
    pub opportunities: Vec<OptimizationOpportunity>,

    /// Metrics
    pub metrics: WasteMetrics,
}

/// Type of waste in Lean
#[derive(Debug, Clone, Copy, Serialize, Deserialize, PartialEq, Eq)]
pub enum WasteType {
    /// Defects requiring rework
    Defects,

    /// Unnecessary processing
    OverProcessing,

    /// More than needed produced
    Overproduction,

    /// Moving/transporting unnecessarily
    Transportation,

    /// Excessive inventory
    Inventory,

    /// Waiting for dependencies
    Waiting,

    /// Unnecessary movement
    MotionWaste,

    /// Unused employee creativity
    UnusedTalent,
}

/// Identified waste item
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct WasteItem {
    /// Waste identifier
    pub id: String,

    /// Type of waste
    pub waste_type: WasteType,

    /// Description
    pub description: String,

    /// Current cost/impact
    pub current_impact: String,

    /// Frequency
    pub frequency: String,

    /// Elimination difficulty (EASY, MEDIUM, HARD)
    pub elimination_difficulty: DifficultyLevel,
}

#[derive(Debug, Clone, Copy, Serialize, Deserialize, PartialEq, Eq)]
pub enum DifficultyLevel {
    Easy,
    Medium,
    Hard,
}

/// Optimization opportunity
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct OptimizationOpportunity {
    /// Opportunity identifier
    pub id: String,

    /// What waste it addresses
    pub addresses: String,

    /// Proposed improvement
    pub proposal: String,

    /// Expected impact
    pub expected_impact: String,

    /// Implementation effort
    pub implementation_effort: String,

    /// Priority (1-5, 5 = highest)
    pub priority: u32,
}

/// Waste metrics
#[derive(Debug, Clone, Default, Serialize, Deserialize)]
pub struct WasteMetrics {
    /// Total waste identified
    pub total_waste_items: u32,

    /// Waste by type
    pub waste_by_type: BTreeMap<String, u32>,

    /// Critical waste items (high impact)
    pub critical_items: u32,

    /// Improvement potential (%)
    pub improvement_potential: f32,
}

impl FailureMode {
    /// Calculate Risk Priority Number
    pub fn calculate_rpn(&mut self) {
        self.rpn = self.severity * self.occurrence * self.detection;
    }

    /// Determine severity level
    pub fn severity_level(&self) -> SeverityLevel {
        match self.severity {
            1..=3 => SeverityLevel::Low,
            4..=7 => SeverityLevel::Medium,
            8..=10 => SeverityLevel::High,
            _ => SeverityLevel::Critical,
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
pub enum SeverityLevel {
    Low,
    Medium,
    High,
    Critical,
}

impl FMEA {
    /// Create new FMEA analysis
    pub fn new(subject: String, rpn_threshold: u32) -> Self {
        Self {
            subject,
            failure_modes: Vec::new(),
            rpn_threshold,
            analyzed_at: chrono::Utc::now().to_rfc3339(),
            critical_failures: Vec::new(),
        }
    }

    /// Add a failure mode
    pub fn add_failure_mode(&mut self, mut mode: FailureMode) {
        mode.calculate_rpn();

        // Track critical failures
        if mode.rpn >= self.rpn_threshold {
            self.critical_failures.push((mode.clone(), mode.rpn));
        }

        self.failure_modes.push(mode);
    }

    /// Get high-risk failure modes
    pub fn high_risk_failures(&self) -> Vec<&FailureMode> {
        self.failure_modes
            .iter()
            .filter(|fm| fm.rpn >= self.rpn_threshold)
            .collect()
    }

    /// Analyze for trends
    pub fn analyze_trends(&self) -> FMEATrends {
        let mut severity_distribution = BTreeMap::new();
        let mut occurrence_distribution = BTreeMap::new();
        let mut detection_distribution = BTreeMap::new();

        for mode in &self.failure_modes {
            *severity_distribution
                .entry(mode.severity_level())
                .or_insert(0) += 1;
            *occurrence_distribution.entry(mode.occurrence).or_insert(0) += 1;
            *detection_distribution.entry(mode.detection).or_insert(0) += 1;
        }

        FMEATrends {
            total_failures_analyzed: self.failure_modes.len(),
            high_risk_count: self.high_risk_failures().len(),
            highest_rpn: self
                .failure_modes
                .iter()
                .map(|fm| fm.rpn)
                .max()
                .unwrap_or(0),
            average_rpn: if self.failure_modes.is_empty() {
                0
            } else {
                self.failure_modes.iter().map(|fm| fm.rpn).sum::<u32>()
                    / self.failure_modes.len() as u32
            },
        }
    }
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct FMEATrends {
    pub total_failures_analyzed: usize,
    pub high_risk_count: usize,
    pub highest_rpn: u32,
    pub average_rpn: u32,
}

impl PokaYoke {
    /// Create new POKA-YOKE system
    pub fn new() -> Self {
        Self {
            prevention_rules: Vec::new(),
            detection_mechanisms: Vec::new(),
            stats: PokaYokeStats::default(),
        }
    }

    /// Add a prevention rule
    pub fn add_prevention_rule(&mut self, rule: PreventionRule) {
        self.prevention_rules.push(rule);
    }

    /// Add a detection mechanism
    pub fn add_detection_mechanism(&mut self, mechanism: DetectionMechanism) {
        self.detection_mechanisms.push(mechanism);
    }

    /// Get prevention effectiveness
    pub fn prevention_effectiveness(&self) -> f32 {
        let total_errors = self.stats.errors_prevented + self.stats.errors_detected;
        if total_errors == 0 {
            100.0
        } else {
            (self.stats.errors_prevented as f32 / total_errors as f32) * 100.0
        }
    }

    /// Get detection accuracy (avoiding false positives)
    pub fn detection_accuracy(&self) -> f32 {
        let total_detections = self.stats.errors_detected + self.stats.false_positives;
        if total_detections == 0 {
            100.0
        } else {
            (self.stats.errors_detected as f32 / total_detections as f32) * 100.0
        }
    }

    /// Get overall quality improvement
    pub fn overall_improvement(&self) -> f32 {
        let total_caught = self.stats.defects_caught + self.stats.errors_prevented;
        (total_caught as f32 * self.detection_accuracy() / 100.0) / 10.0 // Normalized to 0-100%
    }
}

impl Default for PokaYoke {
    fn default() -> Self {
        Self::new()
    }
}

impl Default for MURA {
    fn default() -> Self {
        Self::new()
    }
}

impl MURA {
    /// Create new MURA standardization system
    pub fn new() -> Self {
        Self {
            standards: Vec::new(),
            consistency_metrics: BTreeMap::new(),
            violations: Vec::new(),
        }
    }

    /// Add a standard
    pub fn add_standard(&mut self, standard: Standard) {
        self.standards.push(standard);
    }

    /// Record a violation
    pub fn record_violation(&mut self, violation: MuraViolation) {
        self.violations.push(violation);
    }

    /// Get consistency score
    pub fn consistency_score(&self) -> f32 {
        if self.consistency_metrics.is_empty() {
            100.0
        } else {
            let sum: f32 = self.consistency_metrics.values().sum();
            sum / self.consistency_metrics.len() as f32
        }
    }

    /// Get critical violations
    pub fn critical_violations(&self) -> Vec<&MuraViolation> {
        self.violations
            .iter()
            .filter(|v| v.severity == ViolationSeverity::High)
            .collect()
    }
}

impl Default for MUDA {
    fn default() -> Self {
        Self::new()
    }
}

impl MUDA {
    /// Create new MUDA waste elimination system
    pub fn new() -> Self {
        Self {
            waste_types: vec![
                WasteType::Defects,
                WasteType::OverProcessing,
                WasteType::Overproduction,
                WasteType::Transportation,
                WasteType::Inventory,
                WasteType::Waiting,
                WasteType::MotionWaste,
                WasteType::UnusedTalent,
            ],
            waste_items: Vec::new(),
            opportunities: Vec::new(),
            metrics: WasteMetrics::default(),
        }
    }

    /// Add identified waste
    pub fn add_waste_item(&mut self, item: WasteItem) {
        let waste_type_str = format!("{:?}", item.waste_type);
        *self
            .metrics
            .waste_by_type
            .entry(waste_type_str)
            .or_insert(0) += 1;
        self.metrics.total_waste_items += 1;

        if item.elimination_difficulty == DifficultyLevel::Easy {
            self.metrics.improvement_potential += 5.0;
        } else if item.elimination_difficulty == DifficultyLevel::Medium {
            self.metrics.improvement_potential += 3.0;
        }

        self.waste_items.push(item);
    }

    /// Add optimization opportunity
    pub fn add_opportunity(&mut self, opportunity: OptimizationOpportunity) {
        self.opportunities.push(opportunity);
    }

    /// Get high-priority opportunities
    pub fn high_priority_opportunities(&self) -> Vec<&OptimizationOpportunity> {
        self.opportunities
            .iter()
            .filter(|o| o.priority >= 4)
            .collect()
    }

    /// Get quick wins (easy elimination)
    pub fn quick_wins(&self) -> Vec<&WasteItem> {
        self.waste_items
            .iter()
            .filter(|w| w.elimination_difficulty == DifficultyLevel::Easy)
            .collect()
    }
}

impl fmt::Display for WasteType {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            WasteType::Defects => write!(f, "Defects"),
            WasteType::OverProcessing => write!(f, "Over-processing"),
            WasteType::Overproduction => write!(f, "Over-production"),
            WasteType::Transportation => write!(f, "Transportation"),
            WasteType::Inventory => write!(f, "Inventory"),
            WasteType::Waiting => write!(f, "Waiting"),
            WasteType::MotionWaste => write!(f, "Motion Waste"),
            WasteType::UnusedTalent => write!(f, "Unused Talent"),
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_fmea_rpn_calculation() {
        let mut mode = FailureMode {
            name: "Pack version conflict".to_string(),
            potential_cause: "Incompatible versions".to_string(),
            potential_effect: "Generation failure".to_string(),
            severity: 8,
            occurrence: 6,
            detection: 4,
            preventive_actions: vec![],
            current_controls: vec![],
            rpn: 0,
        };

        mode.calculate_rpn();
        assert_eq!(mode.rpn, 8 * 6 * 4); // 192
        assert_eq!(mode.severity_level(), SeverityLevel::High);
    }

    #[test]
    fn test_poka_yoke_effectiveness() {
        let mut poka = PokaYoke::new();

        poka.stats.errors_prevented = 95;
        poka.stats.errors_detected = 5;

        assert_eq!(poka.prevention_effectiveness(), 95.0);
    }

    #[test]
    fn test_muda_quick_wins() {
        let mut muda = MUDA::new();

        muda.add_waste_item(WasteItem {
            id: "waste-1".to_string(),
            waste_type: WasteType::Defects,
            description: "Redundant validation".to_string(),
            current_impact: "10% overhead".to_string(),
            frequency: "Every build".to_string(),
            elimination_difficulty: DifficultyLevel::Easy,
        });

        let quick_wins = muda.quick_wins();
        assert_eq!(quick_wins.len(), 1);
    }
}
