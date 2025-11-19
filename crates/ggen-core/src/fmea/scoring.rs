//! SOD (Severity-Occurrence-Detection) scoring and RPN calculation

use super::types::{Detection, Occurrence, RiskPriorityNumber, Severity};
use serde::{Deserialize, Serialize};

/// SOD (Severity-Occurrence-Detection) scorer
///
/// Provides automated scoring based on various metrics and heuristics
#[derive(Debug, Clone)]
pub struct SodScorer;

impl SodScorer {
    /// Create a new SOD scorer
    pub fn new() -> Self {
        Self
    }

    /// Score severity based on impact metrics
    pub fn score_severity(&self, metrics: &ImpactMetrics) -> Severity {
        let mut score = 1u8;

        // Data loss or corruption
        if metrics.causes_data_loss {
            score = score.max(10);
        }

        // Security vulnerability
        if metrics.security_impact {
            score = score.max(9);
        }

        // Breaking changes
        if metrics.breaking_change {
            score = score.max(7);
        }

        // Affects downstream systems
        score += metrics.affected_systems.min(3);

        // User impact
        score += match metrics.user_impact {
            UserImpact::None => 0,
            UserImpact::Minimal => 1,
            UserImpact::Moderate => 2,
            UserImpact::Significant => 3,
            UserImpact::Severe => 4,
        };

        Severity::new(score.min(10)).unwrap()
    }

    /// Score occurrence based on historical and predictive metrics
    pub fn score_occurrence(&self, metrics: &OccurrenceMetrics) -> Occurrence {
        let mut score = 1u8;

        // Historical failure rate
        score += match metrics.historical_failures {
            0 => 0,
            1..=5 => 2,
            6..=20 => 4,
            21..=50 => 6,
            _ => 8,
        };

        // Complexity indicator
        if metrics.complexity_score > 0.8 {
            score += 2;
        } else if metrics.complexity_score > 0.5 {
            score += 1;
        }

        // Change frequency
        score += match metrics.change_frequency {
            ChangeFrequency::Rare => 0,
            ChangeFrequency::Occasional => 1,
            ChangeFrequency::Frequent => 2,
            ChangeFrequency::VeryFrequent => 3,
        };

        Occurrence::new(score.min(10)).unwrap()
    }

    /// Score detection capability
    pub fn score_detection(&self, metrics: &DetectionMetrics) -> Detection {
        let mut score = 10u8; // Start pessimistic

        // Automated testing coverage
        if metrics.test_coverage > 0.9 {
            score = score.saturating_sub(5);
        } else if metrics.test_coverage > 0.7 {
            score = score.saturating_sub(3);
        } else if metrics.test_coverage > 0.5 {
            score = score.saturating_sub(2);
        }

        // Static analysis
        if metrics.has_static_analysis {
            score = score.saturating_sub(2);
        }

        // Runtime monitoring
        if metrics.has_runtime_monitoring {
            score = score.saturating_sub(2);
        }

        // Manual review
        if metrics.requires_manual_review {
            score = score.saturating_add(2);
        }

        Detection::new(score.max(1).min(10)).unwrap()
    }

    /// Calculate complete SOD score and RPN
    pub fn calculate_sod(
        &self,
        impact: &ImpactMetrics,
        occurrence: &OccurrenceMetrics,
        detection: &DetectionMetrics,
    ) -> SodScore {
        let severity = self.score_severity(impact);
        let occurrence_score = self.score_occurrence(occurrence);
        let detection_score = self.score_detection(detection);
        let rpn = RiskPriorityNumber::calculate(severity, occurrence_score, detection_score);

        SodScore {
            severity,
            occurrence: occurrence_score,
            detection: detection_score,
            rpn,
        }
    }
}

impl Default for SodScorer {
    fn default() -> Self {
        Self::new()
    }
}

/// Complete SOD score
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct SodScore {
    pub severity: Severity,
    pub occurrence: Occurrence,
    pub detection: Detection,
    pub rpn: RiskPriorityNumber,
}

/// Impact metrics for severity scoring
#[derive(Debug, Clone, Default)]
pub struct ImpactMetrics {
    /// Causes data loss or corruption
    pub causes_data_loss: bool,

    /// Has security implications
    pub security_impact: bool,

    /// Is a breaking change
    pub breaking_change: bool,

    /// Number of affected downstream systems
    pub affected_systems: u8,

    /// User impact level
    pub user_impact: UserImpact,
}

/// Occurrence metrics
#[derive(Debug, Clone)]
pub struct OccurrenceMetrics {
    /// Number of historical failures
    pub historical_failures: u32,

    /// Complexity score (0.0-1.0)
    pub complexity_score: f64,

    /// Change frequency
    pub change_frequency: ChangeFrequency,
}

impl Default for OccurrenceMetrics {
    fn default() -> Self {
        Self {
            historical_failures: 0,
            complexity_score: 0.0,
            change_frequency: ChangeFrequency::Rare,
        }
    }
}

/// Detection metrics
#[derive(Debug, Clone)]
pub struct DetectionMetrics {
    /// Test coverage (0.0-1.0)
    pub test_coverage: f64,

    /// Has static analysis
    pub has_static_analysis: bool,

    /// Has runtime monitoring
    pub has_runtime_monitoring: bool,

    /// Requires manual review
    pub requires_manual_review: bool,
}

impl Default for DetectionMetrics {
    fn default() -> Self {
        Self {
            test_coverage: 0.0,
            has_static_analysis: false,
            has_runtime_monitoring: false,
            requires_manual_review: true,
        }
    }
}

/// User impact level
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum UserImpact {
    None,
    Minimal,
    Moderate,
    Significant,
    Severe,
}

impl Default for UserImpact {
    fn default() -> Self {
        Self::None
    }
}

/// Change frequency
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum ChangeFrequency {
    Rare,
    Occasional,
    Frequent,
    VeryFrequent,
}

/// RPN calculator with configurable thresholds
#[derive(Debug, Clone)]
pub struct RpnCalculator {
    /// RPN threshold for high priority
    pub high_priority_threshold: u16,

    /// RPN threshold for medium priority
    pub medium_priority_threshold: u16,
}

impl RpnCalculator {
    /// Create with default thresholds
    pub fn new() -> Self {
        Self {
            high_priority_threshold: 200,
            medium_priority_threshold: 100,
        }
    }

    /// Calculate RPN
    pub fn calculate(&self, severity: Severity, occurrence: Occurrence, detection: Detection) -> RiskPriorityNumber {
        RiskPriorityNumber::calculate(severity, occurrence, detection)
    }

    /// Determine if RPN is high priority
    pub fn is_high_priority(&self, rpn: &RiskPriorityNumber) -> bool {
        rpn.value() >= self.high_priority_threshold
    }

    /// Determine if RPN is medium priority
    pub fn is_medium_priority(&self, rpn: &RiskPriorityNumber) -> bool {
        rpn.value() >= self.medium_priority_threshold && rpn.value() < self.high_priority_threshold
    }

    /// Get priority level
    pub fn priority_level(&self, rpn: &RiskPriorityNumber) -> PriorityLevel {
        if self.is_high_priority(rpn) {
            PriorityLevel::High
        } else if self.is_medium_priority(rpn) {
            PriorityLevel::Medium
        } else {
            PriorityLevel::Low
        }
    }
}

impl Default for RpnCalculator {
    fn default() -> Self {
        Self::new()
    }
}

/// Priority level
#[derive(Debug, Clone, Copy, PartialEq, Eq, Serialize, Deserialize)]
pub enum PriorityLevel {
    Low,
    Medium,
    High,
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_severity_scoring_data_loss() {
        let scorer = SodScorer::new();
        let metrics = ImpactMetrics {
            causes_data_loss: true,
            ..Default::default()
        };

        let severity = scorer.score_severity(&metrics);
        assert_eq!(severity.value(), 10);
    }

    #[test]
    fn test_severity_scoring_security() {
        let scorer = SodScorer::new();
        let metrics = ImpactMetrics {
            security_impact: true,
            ..Default::default()
        };

        let severity = scorer.score_severity(&metrics);
        assert_eq!(severity.value(), 9);
    }

    #[test]
    fn test_occurrence_scoring() {
        let scorer = SodScorer::new();
        let metrics = OccurrenceMetrics {
            historical_failures: 25,
            complexity_score: 0.9,
            change_frequency: ChangeFrequency::Frequent,
        };

        let occurrence = scorer.score_occurrence(&metrics);
        assert!(occurrence.value() >= 8);
    }

    #[test]
    fn test_detection_scoring_high_coverage() {
        let scorer = SodScorer::new();
        let metrics = DetectionMetrics {
            test_coverage: 0.95,
            has_static_analysis: true,
            has_runtime_monitoring: true,
            requires_manual_review: false,
        };

        let detection = scorer.score_detection(&metrics);
        assert!(detection.value() <= 3);
    }

    #[test]
    fn test_detection_scoring_poor_coverage() {
        let scorer = SodScorer::new();
        let metrics = DetectionMetrics {
            test_coverage: 0.2,
            has_static_analysis: false,
            has_runtime_monitoring: false,
            requires_manual_review: true,
        };

        let detection = scorer.score_detection(&metrics);
        assert!(detection.value() >= 8);
    }

    #[test]
    fn test_complete_sod_calculation() {
        let scorer = SodScorer::new();

        let impact = ImpactMetrics {
            breaking_change: true,
            affected_systems: 3,
            user_impact: UserImpact::Significant,
            ..Default::default()
        };

        let occurrence = OccurrenceMetrics {
            historical_failures: 10,
            complexity_score: 0.6,
            change_frequency: ChangeFrequency::Occasional,
        };

        let detection = DetectionMetrics {
            test_coverage: 0.5,
            has_static_analysis: true,
            has_runtime_monitoring: false,
            requires_manual_review: true,
        };

        let sod = scorer.calculate_sod(&impact, &occurrence, &detection);
        assert!(sod.rpn.value() > 0);
    }

    #[test]
    fn test_rpn_calculator_priority_levels() {
        let calc = RpnCalculator::new();

        let high_rpn = RiskPriorityNumber::calculate(
            Severity::new(10).unwrap(),
            Occurrence::new(5).unwrap(),
            Detection::new(5).unwrap(),
        );
        assert_eq!(calc.priority_level(&high_rpn), PriorityLevel::High);

        let medium_rpn = RiskPriorityNumber::calculate(
            Severity::new(5).unwrap(),
            Occurrence::new(5).unwrap(),
            Detection::new(5).unwrap(),
        );
        assert_eq!(calc.priority_level(&medium_rpn), PriorityLevel::Medium);

        let low_rpn = RiskPriorityNumber::calculate(
            Severity::new(2).unwrap(),
            Occurrence::new(2).unwrap(),
            Detection::new(5).unwrap(),
        );
        assert_eq!(calc.priority_level(&low_rpn), PriorityLevel::Low);
    }
}
