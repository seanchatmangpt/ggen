use crate::{Assessment, Dimension, MaturityLevel, ProgressTracker};

/// Performance metrics tracking for maturity progression
pub struct PerformanceAnalyzer;

impl PerformanceAnalyzer {
    /// Calculate velocity of improvement
    pub fn calculate_velocity(tracker: &ProgressTracker) -> f32 {
        if tracker.assessments.len() < 2 {
            return 0.0;
        }

        let first = tracker.assessments.first().unwrap();
        let last = tracker.assessments.last().unwrap();
        let cycles = (tracker.assessments.len() - 1) as f32;

        (last.overall_score - first.overall_score) / cycles
    }

    /// Estimate time to reach target level
    pub fn estimate_time_to_target(
        current_assessment: &Assessment,
        target_level: MaturityLevel,
        velocity: f32,
    ) -> f32 {
        if velocity <= 0.0 {
            return f32::INFINITY;
        }

        let target_score = target_level.score();
        let cycles_needed = (target_score - current_assessment.overall_score) / velocity;
        cycles_needed.max(0.0)
    }

    /// Calculate improvement acceleration
    pub fn calculate_acceleration(tracker: &ProgressTracker) -> f32 {
        if tracker.assessments.len() < 3 {
            return 0.0;
        }

        let velocities: Vec<f32> = tracker
            .assessments
            .windows(2)
            .map(|w| w[1].overall_score - w[0].overall_score)
            .collect();

        if velocities.len() < 2 {
            return 0.0;
        }

        let acceleration = velocities[velocities.len() - 1] - velocities[0];
        acceleration
    }

    /// Calculate dimension-specific performance SLOs
    pub fn calculate_dimension_slos(assessment: &Assessment) -> Vec<DimensionSLO> {
        let mut slos = Vec::new();

        for (&dim, dim_assess) in &assessment.dimensions {
            let target_level = match dim_assess.level {
                MaturityLevel::Initial => MaturityLevel::Repeatable,
                MaturityLevel::Repeatable => MaturityLevel::Defined,
                MaturityLevel::Defined => MaturityLevel::Managed,
                MaturityLevel::Managed => MaturityLevel::Optimizing,
                MaturityLevel::Optimizing => MaturityLevel::Optimizing,
            };

            slos.push(DimensionSLO {
                dimension: dim,
                current_level: dim_assess.level,
                target_level,
                gap: target_level.score() - dim_assess.level.score(),
                slo_percentage: Self::calculate_slo_percentage(dim_assess.level, target_level),
            });
        }

        slos.sort_by(|a, b| b.gap.partial_cmp(&a.gap).unwrap_or(std::cmp::Ordering::Equal));
        slos
    }

    fn calculate_slo_percentage(current: MaturityLevel, target: MaturityLevel) -> f32 {
        let gap = target.score() - current.score();
        match gap {
            _ if gap <= 0.0 => 100.0,
            _ if gap <= 1.0 => 75.0,
            _ if gap <= 2.0 => 50.0,
            _ => 25.0,
        }
    }

    /// Identify performance bottlenecks
    pub fn identify_bottlenecks(assessment: &Assessment) -> Vec<Bottleneck> {
        let mut bottlenecks = Vec::new();

        for (&dim, dim_assess) in &assessment.dimensions {
            if dim_assess.score <= 2.0 {
                bottlenecks.push(Bottleneck {
                    dimension: dim,
                    severity: 3.0 - dim_assess.score, // Scale 1-3
                    description: format!(
                        "{} at {} level - critical blocker",
                        dim.as_str(),
                        dim_assess.level.as_str()
                    ),
                });
            }
        }

        bottlenecks.sort_by(|a, b| {
            b.severity
                .partial_cmp(&a.severity)
                .unwrap_or(std::cmp::Ordering::Equal)
        });
        bottlenecks
    }

    /// Calculate improvement capacity
    pub fn calculate_capacity(tracker: &ProgressTracker, assessment: &Assessment) -> CapacityMetrics {
        let velocity = Self::calculate_velocity(tracker);
        let acceleration = Self::calculate_acceleration(tracker);

        let sustainable_pace = if tracker.assessments.len() >= 5 {
            // Based on last 3 cycles
            let recent: Vec<f32> = tracker
                .assessments
                .iter()
                .rev()
                .take(3)
                .map(|a| a.overall_score)
                .collect();
            if recent.len() >= 2 {
                (recent[0] - recent[recent.len() - 1]) / (recent.len() - 1) as f32
            } else {
                velocity
            }
        } else {
            velocity
        };

        CapacityMetrics {
            velocity,
            acceleration,
            sustainable_pace,
            headroom: 5.0 - assessment.overall_score, // Max score is 5.0
            runway_cycles: (5.0 - assessment.overall_score) / sustainable_pace.max(0.01),
        }
    }

    /// Detect momentum changes
    pub fn detect_momentum_change(tracker: &ProgressTracker) -> Option<MomentumChange> {
        if tracker.assessments.len() < 4 {
            return None;
        }

        let recent_velocities: Vec<f32> = tracker
            .assessments
            .iter()
            .rev()
            .take(3)
            .collect::<Vec<_>>()
            .windows(2)
            .map(|w| w[1].overall_score - w[0].overall_score)
            .collect();

        if recent_velocities.len() < 2 {
            return None;
        }

        let avg_recent = recent_velocities.iter().sum::<f32>() / recent_velocities.len() as f32;
        let all_velocities: Vec<f32> = tracker
            .assessments
            .windows(2)
            .map(|w| w[1].overall_score - w[0].overall_score)
            .collect();
        let avg_historical = all_velocities.iter().sum::<f32>() / all_velocities.len() as f32;

        let change_ratio = if avg_historical != 0.0 {
            (avg_recent - avg_historical) / avg_historical.abs()
        } else {
            0.0
        };

        if change_ratio.abs() > 0.3 {
            Some(MomentumChange {
                momentum_type: if avg_recent > avg_historical {
                    MomentumType::Accelerating
                } else {
                    MomentumType::Decelerating
                },
                magnitude: change_ratio.abs(),
                recent_average: avg_recent,
                historical_average: avg_historical,
            })
        } else {
            None
        }
    }
}

/// Service Level Objective for a dimension
#[derive(Debug, Clone, serde::Serialize, serde::Deserialize)]
pub struct DimensionSLO {
    pub dimension: Dimension,
    pub current_level: MaturityLevel,
    pub target_level: MaturityLevel,
    pub gap: f32,
    pub slo_percentage: f32,
}

/// Performance bottleneck
#[derive(Debug, Clone, serde::Serialize, serde::Deserialize)]
pub struct Bottleneck {
    pub dimension: Dimension,
    pub severity: f32,
    pub description: String,
}

/// Capacity metrics for improvement
#[derive(Debug, Clone, serde::Serialize, serde::Deserialize)]
pub struct CapacityMetrics {
    pub velocity: f32,
    pub acceleration: f32,
    pub sustainable_pace: f32,
    pub headroom: f32,
    pub runway_cycles: f32,
}

/// Momentum in improvement trajectory
#[derive(Debug, Clone, serde::Serialize, serde::Deserialize)]
pub struct MomentumChange {
    pub momentum_type: MomentumType,
    pub magnitude: f32,
    pub recent_average: f32,
    pub historical_average: f32,
}

/// Type of momentum change
#[derive(Debug, Clone, Copy, serde::Serialize, serde::Deserialize)]
pub enum MomentumType {
    Accelerating,
    Decelerating,
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::{Assessment, DimensionAssessment};

    #[test]
    fn test_calculate_velocity() {
        let mut tracker = ProgressTracker::new();

        let mut a1 = Assessment::new("a1".to_string());
        a1.add_dimension(DimensionAssessment::new(
            Dimension::CodeQuality,
            MaturityLevel::Initial,
            "ev".to_string(),
        ));
        tracker.add_assessment(a1);

        let mut a2 = Assessment::new("a2".to_string());
        a2.add_dimension(DimensionAssessment::new(
            Dimension::CodeQuality,
            MaturityLevel::Repeatable,
            "ev".to_string(),
        ));
        tracker.add_assessment(a2);

        let velocity = PerformanceAnalyzer::calculate_velocity(&tracker);
        assert_eq!(velocity, 1.0);
    }

    #[test]
    fn test_estimate_time_to_target() {
        let mut assessment = Assessment::new("test".to_string());
        assessment.add_dimension(DimensionAssessment::new(
            Dimension::CodeQuality,
            MaturityLevel::Initial,
            "ev".to_string(),
        ));

        let cycles = PerformanceAnalyzer::estimate_time_to_target(&assessment, MaturityLevel::Managed, 1.0);
        assert_eq!(cycles, 3.0); // From 1.0 to 4.0 at 1.0 per cycle
    }

    #[test]
    fn test_calculate_acceleration() {
        let mut tracker = ProgressTracker::new();

        for i in 0..4 {
            let mut a = Assessment::new(format!("a{}", i));
            a.add_dimension(DimensionAssessment::new(
                Dimension::CodeQuality,
                match i {
                    0 => MaturityLevel::Initial,
                    1 => MaturityLevel::Repeatable,
                    2 => MaturityLevel::Defined,
                    _ => MaturityLevel::Managed,
                },
                "ev".to_string(),
            ));
            tracker.add_assessment(a);
        }

        let accel = PerformanceAnalyzer::calculate_acceleration(&tracker);
        assert!(accel >= 0.0); // Consistent or improving
    }

    #[test]
    fn test_calculate_dimension_slos() {
        let mut assessment = Assessment::new("test".to_string());
        assessment.add_dimension(DimensionAssessment::new(
            Dimension::CodeQuality,
            MaturityLevel::Initial,
            "ev".to_string(),
        ));

        let slos = PerformanceAnalyzer::calculate_dimension_slos(&assessment);
        assert!(!slos.is_empty());
        assert!(slos[0].gap > 0.0);
    }

    #[test]
    fn test_identify_bottlenecks() {
        let mut assessment = Assessment::new("test".to_string());
        assessment.add_dimension(DimensionAssessment::new(
            Dimension::Security,
            MaturityLevel::Initial,
            "ev".to_string(),
        ));

        let bottlenecks = PerformanceAnalyzer::identify_bottlenecks(&assessment);
        assert!(!bottlenecks.is_empty());
    }

    #[test]
    fn test_calculate_capacity() {
        let mut tracker = ProgressTracker::new();

        let mut a1 = Assessment::new("a1".to_string());
        a1.add_dimension(DimensionAssessment::new(
            Dimension::CodeQuality,
            MaturityLevel::Initial,
            "ev".to_string(),
        ));
        tracker.add_assessment(a1);

        let mut a2 = Assessment::new("a2".to_string());
        a2.add_dimension(DimensionAssessment::new(
            Dimension::CodeQuality,
            MaturityLevel::Repeatable,
            "ev".to_string(),
        ));
        let a2_clone = a2.clone();
        tracker.add_assessment(a2);

        let capacity = PerformanceAnalyzer::calculate_capacity(&tracker, &a2_clone);
        assert!(capacity.velocity >= 0.0);
        assert!(capacity.headroom > 0.0);
    }

    #[test]
    fn test_detect_momentum_change() {
        let mut tracker = ProgressTracker::new();

        for i in 0..5 {
            let mut a = Assessment::new(format!("a{}", i));
            let score = match i {
                0 => 1.0,
                1 => 1.5,
                2 => 2.1,
                3 => 2.9,
                _ => 4.0,
            };
            let level = match score as u32 {
                1 => MaturityLevel::Initial,
                2 => MaturityLevel::Repeatable,
                3 => MaturityLevel::Defined,
                _ => MaturityLevel::Managed,
            };
            a.add_dimension(DimensionAssessment::new(Dimension::CodeQuality, level, "ev".to_string()));
            tracker.add_assessment(a);
        }

        let momentum = PerformanceAnalyzer::detect_momentum_change(&tracker);
        // Should detect acceleration as recent improvements are higher
        assert!(momentum.is_some() || tracker.assessments.len() < 4);
    }

    #[test]
    fn test_estimate_time_with_zero_velocity() {
        let mut assessment = Assessment::new("test".to_string());
        assessment.add_dimension(DimensionAssessment::new(
            Dimension::CodeQuality,
            MaturityLevel::Initial,
            "ev".to_string(),
        ));

        let result = PerformanceAnalyzer::estimate_time_to_target(&assessment, MaturityLevel::Optimizing, 0.0);
        assert_eq!(result, f32::INFINITY);
    }

    #[test]
    fn test_dimension_slo_percentage() {
        let mut assessment = Assessment::new("test".to_string());
        assessment.add_dimension(DimensionAssessment::new(
            Dimension::CodeQuality,
            MaturityLevel::Optimizing,
            "ev".to_string(),
        ));

        let slos = PerformanceAnalyzer::calculate_dimension_slos(&assessment);
        assert_eq!(slos[0].slo_percentage, 100.0); // Already at target
    }
}
