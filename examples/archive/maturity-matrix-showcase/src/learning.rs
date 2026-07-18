use crate::{Assessment, Dimension, MaturityLevel, ProgressTracker, Recommendation};
use std::collections::HashMap;

/// Self-improvement agent using reinforcement learning patterns
pub struct LearningAgent;

impl LearningAgent {
    /// Learn from executed improvements to refine future recommendations
    pub fn learn_from_improvements(
        tracker: &ProgressTracker,
    ) -> Option<LearningPattern> {
        if tracker.improvement_history.len() < 2 {
            return None;
        }

        let mut dimension_effectiveness: HashMap<Dimension, (f32, i32)> = HashMap::new();

        // Calculate effectiveness for each dimension
        for i in 0..tracker.assessments.len() - 1 {
            let prev = &tracker.assessments[i];
            let next = &tracker.assessments[i + 1];

            for (&dim, prev_assess) in &prev.dimensions {
                if let Some(next_assess) = next.dimensions.get(&dim) {
                    let improvement = next_assess.score - prev_assess.score;
                    let entry = dimension_effectiveness.entry(dim).or_insert((0.0, 0));
                    entry.0 += improvement;
                    entry.1 += 1;
                }
            }
        }

        // Calculate average improvements
        let mut patterns = Vec::new();
        for (dim, (total_improvement, count)) in dimension_effectiveness {
            let avg_improvement = total_improvement / count as f32;
            patterns.push(DimensionPattern {
                dimension: dim,
                average_improvement: avg_improvement,
                total_cycles: count as usize,
            });
        }

        // Sort by effectiveness
        patterns.sort_by(|a, b| {
            b.average_improvement
                .partial_cmp(&a.average_improvement)
                .unwrap_or(std::cmp::Ordering::Equal)
        });

        Some(LearningPattern { patterns })
    }

    /// Predict next maturity level based on trends
    pub fn predict_next_level(
        assessment: &Assessment,
        historical_trend: f32,
    ) -> HashMap<Dimension, MaturityLevel> {
        let mut predictions = HashMap::new();

        for (&dim, dim_assess) in &assessment.dimensions {
            let current_score = dim_assess.score;
            let predicted_score = (current_score + historical_trend).min(5.0).max(1.0);

            let predicted_level = match predicted_score as u32 {
                5 => MaturityLevel::Optimizing,
                4 => MaturityLevel::Managed,
                3 => MaturityLevel::Defined,
                2 => MaturityLevel::Repeatable,
                _ => MaturityLevel::Initial,
            };

            predictions.insert(dim, predicted_level);
        }

        predictions
    }

    /// Identify which recommendations were most effective
    pub fn rank_recommendation_effectiveness(
        tracker: &ProgressTracker,
    ) -> Vec<RecommendationScore> {
        let mut scores: HashMap<String, RecommendationScore> = HashMap::new();

        for plan in &tracker.improvement_history {
            for rec in &plan.recommendations {
                let key = format!("{:?}_{}", rec.dimension, rec.action);
                let entry = scores
                    .entry(key)
                    .or_insert(RecommendationScore {
                        action: rec.action.clone(),
                        dimension: rec.dimension,
                        expected_impact: rec.impact,
                        actual_impact: 0.0,
                        execution_count: 0,
                    });
                entry.execution_count += 1;
            }
        }

        // Calculate actual impact from assessments
        if tracker.assessments.len() >= 2 {
            for (_, score) in &mut scores {
                let latest = tracker.assessments.last().unwrap();
                let previous = &tracker.assessments[tracker.assessments.len() - 2];

                if let (Some(latest_dim), Some(prev_dim)) =
                    (latest.dimensions.get(&score.dimension), previous.dimensions.get(&score.dimension))
                {
                    score.actual_impact = latest_dim.score - prev_dim.score;
                }
            }
        }

        let mut result: Vec<_> = scores.into_values().collect();
        result.sort_by(|a, b| {
            b.actual_impact
                .partial_cmp(&a.actual_impact)
                .unwrap_or(std::cmp::Ordering::Equal)
        });
        result
    }
}

/// Pattern discovered through learning cycles
#[derive(Debug, Clone, serde::Serialize, serde::Deserialize)]
pub struct LearningPattern {
    pub patterns: Vec<DimensionPattern>,
}

impl LearningPattern {
    pub fn best_improving_dimension(&self) -> Option<Dimension> {
        self.patterns
            .first()
            .map(|p| p.dimension)
    }

    pub fn worst_improving_dimension(&self) -> Option<Dimension> {
        self.patterns
            .last()
            .map(|p| p.dimension)
    }
}

/// Pattern for a single dimension
#[derive(Debug, Clone, serde::Serialize, serde::Deserialize)]
pub struct DimensionPattern {
    pub dimension: Dimension,
    pub average_improvement: f32,
    pub total_cycles: usize,
}

/// Effectiveness score for a recommendation
#[derive(Debug, Clone, serde::Serialize, serde::Deserialize)]
pub struct RecommendationScore {
    pub action: String,
    pub dimension: Dimension,
    pub expected_impact: f32,
    pub actual_impact: f32,
    pub execution_count: usize,
}

impl RecommendationScore {
    pub fn accuracy(&self) -> f32 {
        if self.expected_impact == 0.0 {
            0.0
        } else {
            (self.actual_impact / self.expected_impact).min(1.0).max(0.0)
        }
    }
}

/// Adaptive strategy selector based on learning
pub struct StrategyAdaptor;

impl StrategyAdaptor {
    /// Select best strategy based on historical performance
    pub fn select_strategy(
        current_assessment: &Assessment,
        tracker: &ProgressTracker,
    ) -> AdaptiveStrategy {
        let pattern = LearningAgent::learn_from_improvements(tracker);
        let effectiveness = LearningAgent::rank_recommendation_effectiveness(tracker);

        AdaptiveStrategy {
            based_on_pattern: pattern,
            top_effective_actions: effectiveness.into_iter().take(5).collect(),
            recommended_focus: Self::identify_focus_areas(current_assessment),
        }
    }

    fn identify_focus_areas(assessment: &Assessment) -> Vec<Dimension> {
        let mut dims: Vec<_> = assessment
            .dimensions
            .values()
            .map(|d| (d.dimension, d.score))
            .collect();

        dims.sort_by(|a, b| a.1.partial_cmp(&b.1).unwrap_or(std::cmp::Ordering::Equal));
        dims.into_iter().take(3).map(|(d, _)| d).collect()
    }
}

/// Adaptive strategy based on learning
#[derive(Debug, Clone, serde::Serialize, serde::Deserialize)]
pub struct AdaptiveStrategy {
    pub based_on_pattern: Option<LearningPattern>,
    pub top_effective_actions: Vec<RecommendationScore>,
    pub recommended_focus: Vec<Dimension>,
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::{Assessment, DimensionAssessment, Gap, ImprovementPlan, ProgressTracker, Recommendation};

    #[test]
    fn test_learning_agent_learns_from_improvements() {
        let mut tracker = ProgressTracker::new();

        let mut a1 = Assessment::new("a1".to_string());
        a1.add_dimension(DimensionAssessment::new(
            Dimension::CodeQuality,
            MaturityLevel::Initial,
            "ev".to_string(),
        ));
        tracker.add_assessment(a1);

        let gap = Gap::new(Dimension::CodeQuality, MaturityLevel::Initial, MaturityLevel::Managed);
        let mut plan = ImprovementPlan::new("plan1".to_string());
        plan.add_recommendation(Recommendation::new(
            Dimension::CodeQuality,
            gap,
            "Add tests".to_string(),
            0.5,
            2.0,
        ));
        tracker.add_improvement(plan);

        let mut a2 = Assessment::new("a2".to_string());
        a2.add_dimension(DimensionAssessment::new(
            Dimension::CodeQuality,
            MaturityLevel::Managed,
            "ev".to_string(),
        ));
        tracker.add_assessment(a2);

        // Need at least 2 improvements for learning
        let gap2 = Gap::new(Dimension::CodeQuality, MaturityLevel::Managed, MaturityLevel::Optimizing);
        let mut plan2 = ImprovementPlan::new("plan2".to_string());
        plan2.add_recommendation(Recommendation::new(
            Dimension::CodeQuality,
            gap2,
            "Further testing".to_string(),
            0.6,
            2.5,
        ));
        tracker.add_improvement(plan2);

        let pattern = LearningAgent::learn_from_improvements(&tracker);
        assert!(pattern.is_some());
        let p = pattern.unwrap();
        assert!(!p.patterns.is_empty());
        assert!(p.patterns[0].average_improvement > 0.0);
    }

    #[test]
    fn test_predict_next_level() {
        let mut assessment = Assessment::new("test".to_string());
        assessment.add_dimension(DimensionAssessment::new(
            Dimension::CodeQuality,
            MaturityLevel::Defined,
            "ev".to_string(),
        ));

        let predictions = LearningAgent::predict_next_level(&assessment, 0.5);
        assert!(predictions.contains_key(&Dimension::CodeQuality));
    }

    #[test]
    fn test_rank_recommendation_effectiveness() {
        let mut tracker = ProgressTracker::new();

        let gap = Gap::new(Dimension::CodeQuality, MaturityLevel::Initial, MaturityLevel::Managed);
        let mut plan = ImprovementPlan::new("plan".to_string());
        plan.add_recommendation(Recommendation::new(
            Dimension::CodeQuality,
            gap,
            "Test action".to_string(),
            0.5,
            2.0,
        ));
        tracker.add_improvement(plan);

        let effectiveness = LearningAgent::rank_recommendation_effectiveness(&tracker);
        assert!(!effectiveness.is_empty());
    }

    #[test]
    fn test_recommendation_score_accuracy() {
        let score = RecommendationScore {
            action: "test".to_string(),
            dimension: Dimension::CodeQuality,
            expected_impact: 2.0,
            actual_impact: 1.8,
            execution_count: 1,
        };
        assert!(score.accuracy() > 0.8);
        assert!(score.accuracy() <= 1.0);
    }

    #[test]
    fn test_strategy_adaptor_selects_focus() {
        let mut assessment = Assessment::new("test".to_string());
        assessment.add_dimension(DimensionAssessment::new(
            Dimension::CodeQuality,
            MaturityLevel::Initial,
            "ev".to_string(),
        ));
        assessment.add_dimension(DimensionAssessment::new(
            Dimension::Performance,
            MaturityLevel::Repeatable,
            "ev".to_string(),
        ));

        let tracker = ProgressTracker::new();
        let strategy = StrategyAdaptor::select_strategy(&assessment, &tracker);
        assert!(!strategy.recommended_focus.is_empty());
    }

    #[test]
    fn test_learning_pattern_best_dimension() {
        let pattern = LearningPattern {
            patterns: vec![
                DimensionPattern {
                    dimension: Dimension::CodeQuality,
                    average_improvement: 2.0,
                    total_cycles: 3,
                },
                DimensionPattern {
                    dimension: Dimension::Performance,
                    average_improvement: 0.5,
                    total_cycles: 2,
                },
            ],
        };

        assert_eq!(pattern.best_improving_dimension(), Some(Dimension::CodeQuality));
        assert_eq!(pattern.worst_improving_dimension(), Some(Dimension::Performance));
    }
}
