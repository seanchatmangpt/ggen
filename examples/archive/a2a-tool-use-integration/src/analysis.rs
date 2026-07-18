//! Result analysis and learning system
//!
//! Analyzes execution results, validates goal achievement,
//! and extracts patterns for agent learning.

use crate::execution::ExecutionResult;
use crate::goals::Goal;
use serde::{Deserialize, Serialize};
use uuid::Uuid;
use chrono::{DateTime, Utc};

/// Result of analyzing an execution
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct AnalysisResult {
    pub id: String,
    pub execution_result_id: String,
    pub goal_achieved: bool,
    pub success_criteria_met: Vec<(String, bool)>,
    pub effectiveness_score: f32,
    pub insights: Vec<String>,
    pub recommendations: Vec<String>,
    pub learned_patterns: Vec<LearnedPattern>,
    pub analyzed_at: DateTime<Utc>,
}

/// Pattern learned from successful execution
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct LearnedPattern {
    pub pattern_id: String,
    pub pattern_type: PatternType,
    pub description: String,
    pub success_rate: f32,
    pub frequency: usize,
    pub applicable_goal_types: Vec<String>,
    pub tool_sequence: Vec<String>,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Serialize, Deserialize)]
pub enum PatternType {
    ToolCombination,
    SuccessSequence,
    ErrorRecovery,
}

impl std::fmt::Display for PatternType {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::ToolCombination => write!(f, "ToolCombination"),
            Self::SuccessSequence => write!(f, "SuccessSequence"),
            Self::ErrorRecovery => write!(f, "ErrorRecovery"),
        }
    }
}

/// Analyzer for execution results
pub struct Analyzer;

impl Analyzer {
    /// Analyze an execution result
    pub fn analyze(
        execution: &ExecutionResult,
        goal: &Goal,
    ) -> AnalysisResult {
        let mut analysis = AnalysisResult {
            id: Uuid::new_v4().to_string(),
            execution_result_id: execution.id.clone(),
            goal_achieved: false,
            success_criteria_met: Vec::new(),
            effectiveness_score: 0.0,
            insights: Vec::new(),
            recommendations: Vec::new(),
            learned_patterns: Vec::new(),
            analyzed_at: Utc::now(),
        };

        // Check success criteria
        Self::check_success_criteria(&mut analysis, goal, execution);

        // Calculate effectiveness score
        analysis.effectiveness_score = Self::calculate_effectiveness(execution, &analysis);

        // Extract insights
        Self::generate_insights(&mut analysis, execution);

        // Extract patterns
        if execution.goal_achieved {
            analysis.learned_patterns = Self::extract_patterns(execution, goal);
        }

        // Generate recommendations
        let analysis_copy = analysis.clone();
        Self::generate_recommendations(&mut analysis, execution, &analysis_copy);

        analysis.goal_achieved = execution.goal_achieved;
        analysis
    }

    /// Check success criteria
    fn check_success_criteria(
        analysis: &mut AnalysisResult,
        goal: &Goal,
        execution: &ExecutionResult,
    ) {
        for criterion in &goal.success_criteria {
            let met = match criterion.as_str() {
                "goal_achieved" => execution.goal_achieved,
                "no_errors" => execution.step_results.iter().all(|s| {
                    use crate::execution::ExecutionStatus;
                    s.status != ExecutionStatus::Failure
                }),
                "execution_time" => execution.total_execution_time_ms < 30000,
                "all_steps_completed" => !execution.step_results.is_empty(),
                _ => false,
            };
            analysis.success_criteria_met.push((criterion.clone(), met));
        }
    }

    /// Calculate effectiveness score (0.0-1.0)
    fn calculate_effectiveness(
        execution: &ExecutionResult,
        analysis: &AnalysisResult,
    ) -> f32 {
        let mut score = 0.0;

        // Achievement (50%)
        if execution.goal_achieved {
            score += 0.5;
        }

        // Criteria met (30%)
        let criteria_met = analysis.success_criteria_met.iter()
            .filter(|(_, met)| *met)
            .count() as f32;
        let criteria_ratio = if analysis.success_criteria_met.is_empty() {
            0.0
        } else {
            criteria_met / analysis.success_criteria_met.len() as f32
        };
        score += criteria_ratio * 0.3;

        // Efficiency (20%) - faster is better
        let time_score = 1.0 - (execution.total_execution_time_ms as f32 / 30000.0).min(1.0);
        score += time_score * 0.2;

        score
    }

    /// Generate insights from execution
    fn generate_insights(analysis: &mut AnalysisResult, execution: &ExecutionResult) {
        if execution.goal_achieved {
            analysis.insights.push(
                format!("Goal achieved successfully in {} ms",
                    execution.total_execution_time_ms)
            );
        }

        let failed_steps = execution.step_results.iter()
            .filter(|s| {
                use crate::execution::ExecutionStatus;
                s.status == ExecutionStatus::Failure
            })
            .count();

        if failed_steps > 0 {
            analysis.insights.push(
                format!("{} steps failed during execution", failed_steps)
            );
        }

        if execution.total_execution_time_ms > 20000 {
            analysis.insights.push(
                "Execution took longer than expected".to_string()
            );
        }

        if execution.step_results.len() > 1 {
            analysis.insights.push(
                format!("Multi-step execution used {} tools", execution.step_results.len())
            );
        }
    }

    /// Extract patterns from successful execution
    fn extract_patterns(execution: &ExecutionResult, goal: &Goal) -> Vec<LearnedPattern> {
        let mut patterns = Vec::new();

        // Extract tool combination pattern
        let tool_sequence: Vec<String> = execution.step_results
            .iter()
            .map(|s| s.tool_name.clone())
            .collect();

        if !tool_sequence.is_empty() {
            patterns.push(LearnedPattern {
                pattern_id: Uuid::new_v4().to_string(),
                pattern_type: PatternType::ToolCombination,
                description: format!("Tool combination for {}: {}",
                    goal.goal_type, tool_sequence.join(" -> ")),
                success_rate: 0.85,
                frequency: 1,
                applicable_goal_types: vec![goal.goal_type.to_string()],
                tool_sequence,
            });
        }

        // Extract success sequence pattern
        if execution.step_results.iter().all(|s| {
            use crate::execution::ExecutionStatus;
            s.status == ExecutionStatus::Success
        }) {
            patterns.push(LearnedPattern {
                pattern_id: Uuid::new_v4().to_string(),
                pattern_type: PatternType::SuccessSequence,
                description: format!("Successful execution pattern for {}",
                    goal.goal_type),
                success_rate: 1.0,
                frequency: 1,
                applicable_goal_types: vec![goal.goal_type.to_string()],
                tool_sequence: execution.step_results
                    .iter()
                    .map(|s| s.tool_name.clone())
                    .collect(),
            });
        }

        patterns
    }

    /// Generate recommendations
    fn generate_recommendations(
        analysis: &mut AnalysisResult,
        execution: &ExecutionResult,
        _analysis_data: &AnalysisResult,
    ) {
        if !execution.goal_achieved {
            analysis.recommendations.push(
                "Review goal definition and success criteria".to_string()
            );
        }

        if execution.step_results.iter().any(|s| {
            use crate::execution::ExecutionStatus;
            s.status == ExecutionStatus::Failure
        }) {
            analysis.recommendations.push(
                "Consider adding retry policies to failed steps".to_string()
            );
        }

        if execution.total_execution_time_ms > 20000 {
            analysis.recommendations.push(
                "Profile step execution times and optimize slow tools".to_string()
            );
        }

        if _analysis_data.effectiveness_score < 0.7 {
            analysis.recommendations.push(
                "Review and improve plan efficiency".to_string()
            );
        }
    }
}

/// Learning system for agent improvement
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct LearningSystem {
    pub patterns: Vec<LearnedPattern>,
    pub pattern_usage: std::collections::BTreeMap<String, usize>,
    pub pattern_success_rates: std::collections::BTreeMap<String, f32>,
}

impl LearningSystem {
    /// Create a new learning system
    pub fn new() -> Self {
        Self {
            patterns: Vec::new(),
            pattern_usage: std::collections::BTreeMap::new(),
            pattern_success_rates: std::collections::BTreeMap::new(),
        }
    }

    /// Record a learned pattern
    pub fn learn(&mut self, pattern: LearnedPattern) {
        let key = pattern.pattern_id.clone();
        *self.pattern_usage.entry(key.clone()).or_insert(0) += 1;
        self.pattern_success_rates.insert(key, pattern.success_rate);
        self.patterns.push(pattern);
    }

    /// Get patterns for a goal type
    pub fn get_patterns_for_goal(&self, goal_type: &str) -> Vec<&LearnedPattern> {
        self.patterns
            .iter()
            .filter(|p| p.applicable_goal_types.contains(&goal_type.to_string()))
            .collect()
    }

    /// Get most successful pattern
    pub fn get_best_pattern(&self, goal_type: &str) -> Option<&LearnedPattern> {
        self.get_patterns_for_goal(goal_type)
            .into_iter()
            .max_by(|a, b| a.success_rate.partial_cmp(&b.success_rate)
                .unwrap_or(std::cmp::Ordering::Equal))
    }
}

impl Default for LearningSystem {
    fn default() -> Self {
        Self::new()
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::execution::{ExecutionResult, StepResult};
    use crate::goals::GoalType;

    fn create_test_goal() -> Goal {
        Goal::new(GoalType::GenerateCode, "Test goal".to_string())
            .with_success_criteria(vec![
                "goal_achieved".to_string(),
                "no_errors".to_string(),
            ])
    }

    fn create_test_execution() -> ExecutionResult {
        let mut result = ExecutionResult::new("plan-1".to_string());
        let step = StepResult::new("step-1".to_string(), 1, "tool-1".to_string());
        result.add_step_result(step);
        result.goal_achieved = true;
        result
    }

    #[test]
    fn test_analysis_creation() {
        let goal = create_test_goal();
        let execution = create_test_execution();
        let analysis = Analyzer::analyze(&execution, &goal);

        assert_eq!(analysis.goal_achieved, true);
    }

    #[test]
    fn test_effectiveness_score() {
        let goal = create_test_goal();
        let execution = create_test_execution();
        let analysis = Analyzer::analyze(&execution, &goal);

        assert!(analysis.effectiveness_score > 0.0);
        assert!(analysis.effectiveness_score <= 1.0);
    }

    #[test]
    fn test_learning_system() {
        let mut system = LearningSystem::new();
        let pattern = LearnedPattern {
            pattern_id: "p1".to_string(),
            pattern_type: PatternType::ToolCombination,
            description: "test".to_string(),
            success_rate: 0.9,
            frequency: 1,
            applicable_goal_types: vec!["GenerateCode".to_string()],
            tool_sequence: vec!["gen".to_string()],
        };

        system.learn(pattern);
        assert_eq!(system.patterns.len(), 1);
    }

    #[test]
    fn test_get_patterns_for_goal() {
        let mut system = LearningSystem::new();
        let pattern = LearnedPattern {
            pattern_id: "p1".to_string(),
            pattern_type: PatternType::ToolCombination,
            description: "test".to_string(),
            success_rate: 0.9,
            frequency: 1,
            applicable_goal_types: vec!["GenerateCode".to_string()],
            tool_sequence: vec!["gen".to_string()],
        };

        system.learn(pattern);
        let patterns = system.get_patterns_for_goal("GenerateCode");
        assert_eq!(patterns.len(), 1);
    }
}
