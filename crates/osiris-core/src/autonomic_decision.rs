//! Autonomic Decision Making Framework
//!
//! Implements intelligent decision-making for OSIRIS autonomic life management

use serde_json::Value;
use std::collections::HashMap;
use std::time::Instant;
use tracing::{debug, info, warn};

use crate::{OSIRISError, Result};
use crate::domain::DomainType;
use crate::autonomic::AutonomicState;

/// Context for autonomic decisions
#[derive(Debug, Clone)]
pub struct AutonomicContext {
    pub domain: DomainType,
    pub performance_threshold: f64,
    pub quality_threshold: f64,
    pub operational_state: AutonomicState,
    pub environmental_factors: HashMap<String, f64>,
    pub historical_data: Vec<DecisionData>,
}

/// Decision data for learning
#[derive(Debug, Clone)]
pub struct DecisionData {
    pub timestamp: Instant,
    pub decision: AutonomicDecision,
    pub outcome: DecisionOutcome,
    pub context: HashMap<String, Value>,
}

/// Outcome of a decision
#[derive(Debug, Clone, PartialEq)]
pub enum DecisionOutcome {
    Success,
    PartialSuccess,
    Failure,
    NoChange,
}

/// Autonomic decision with confidence and rationale
#[derive(Debug, Clone)]
pub struct AutonomicDecision {
    pub action: AutonomicAction,
    pub confidence: f64,
    pub rationale: String,
    pub priority: DecisionPriority,
    pub expected_impact: f64,
    pub implementation_plan: Value,
}

/// Types of autonomic actions
#[derive(Debug, Clone, PartialEq)]
pub enum AutonomicAction {
    /// Adjust configuration parameters
    Adjust,
    /// Continue current operations
    Continue,
    /// Intervene manually
    Intervene,
    /// Escalate to higher authority
    Escalate,
    /// Scale resources up or down
    Scale,
    /// Reconfigure system
    Reconfigure,
    /// Optimize process
    Optimize,
    /// Initiate recovery
    Recover,
}

/// Decision priority levels
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord)]
pub enum DecisionPriority {
    Low = 0,
    Medium = 1,
    High = 2,
    Critical = 3,
    Emergency = 4,
}

/// Autonomic decision engine
pub struct AutonomicEngine {
    decision_history: Vec<DecisionData>,
    decision_models: HashMap<String, DecisionModel>,
    learning_enabled: bool,
}

impl AutonomicEngine {
    pub fn new() -> Self {
        Self {
            decision_history: Vec::new(),
            decision_models: Self::initialize_models(),
            learning_enabled: true,
        }
    }

    pub fn with_learning(mut self, enabled: bool) -> Self {
        self.learning_enabled = enabled;
        self
    }

    /// Initialize decision models
    fn initialize_models() -> HashMap<String, DecisionModel> {
        let mut models = HashMap::new();

        // Performance optimization model
        models.insert("performance".to_string(), DecisionModel::new(
            "performance_optimization".to_string(),
            vec![
                "cpu_usage".to_string(),
                "memory_usage".to_string(),
                "response_time".to_string(),
                "throughput".to_string(),
            ],
            DecisionPriority::Medium,
        ));

        // Quality assurance model
        models.insert("quality".to_string(), DecisionModel::new(
            "quality_assurance".to_string(),
            vec![
                "error_rate".to_string(),
                "defect_count".to_string(),
                "test_coverage".to_string(),
                "customer_satisfaction".to_string(),
            ],
            DecisionPriority::High,
        ));

        // Safety management model
        models.insert("safety".to_string(), DecisionModel::new(
            "safety_management".to_string(),
            vec![
                "safety_threshold".to_string(),
                "incident_count".to_string(),
                "compliance_level".to_string(),
                "risk_score".to_string(),
            ],
            DecisionPriority::Critical,
        ));

        models
    }

    /// Make an autonomic decision
    pub async fn make_decision(&self, context: &AutonomicContext) -> Result<AutonomicDecision> {
        debug!("Making autonomic decision for context: {:?}", context.domain);

        // Select appropriate decision model
        let model = self.select_decision_model(context);

        // Analyze situation
        let analysis = self.analyze_situation(context, &model).await?;

        // Generate possible actions
        let possible_actions = self.generate_possible_actions(&analysis).await?;

        // Select best action
        let action = self.select_best_action(&possible_actions, &analysis).await?;

        // Calculate confidence
        let confidence = self.calculate_confidence(&action, &analysis, context);

        // Generate rationale
        let rationale = self.generate_rationale(&action, &analysis, context);

        // Create implementation plan
        let implementation_plan = self.create_implementation_plan(&action, &analysis).await?;

        let decision = AutonomicDecision {
            action,
            confidence,
            rationale,
            priority: analysis.priority,
            expected_impact: analysis.expected_impact,
            implementation_plan,
        };

        // Record decision for learning
        if self.learning_enabled {
            self.record_decision(context, &decision).await;
        }

        info!("Autonomic decision made: {:?} (confidence: {:.2})", decision.action, decision.confidence);
        Ok(decision)
    }

    /// Select appropriate decision model
    fn select_decision_model(&self, context: &AutonomicContext) -> &DecisionModel {
        // Prioritize based on context
        if context.quality_threshold > 0.99 {
            self.decision_models.get("quality").unwrap()
        } else if context.performance_threshold > 0.95 {
            self.decision_models.get("performance").unwrap()
        } else if context.domain == DomainType::Security {
            self.decision_models.get("safety").unwrap()
        } else {
            self.decision_models.get("performance").unwrap()
        }
    }

    /// Analyze current situation
    async fn analyze_situation(&self, context: &AutonomicContext, model: &DecisionModel) -> Result<AnalysisResult> {
        let mut factors = HashMap::new();

        // Extract relevant metrics
        for metric in &model.metrics {
            if let Some(value) = context.environmental_factors.get(metric) {
                factors.insert(metric.clone(), *value);
            }
        }

        // Determine priority
        let priority = self.determine_priority(&factors, context);

        // Calculate expected impact
        let expected_impact = self.calculate_expected_impact(&factors, context);

        // Identify critical issues
        let critical_issues = self.identify_critical_issues(&factors, context);

        Ok(AnalysisResult {
            factors,
            priority,
            expected_impact,
            critical_issues,
        })
    }

    /// Generate possible actions
    async fn generate_possible_actions(&self, analysis: &AnalysisResult) -> Result<Vec<AutonomicAction>> {
        let mut actions = Vec::new();

        // Generate actions based on analysis
        if analysis.critical_issues.is_empty() {
            actions.push(AutonomicAction::Continue);
            actions.push(AutonomicAction::Optimize);
        } else {
            actions.push(AutonomicAction::Intervene);
            if analysis.expected_impact > 0.8 {
                actions.push(AutonomicAction::Escalate);
            }
        }

        // Add domain-specific actions
        if !analysis.factors.is_empty() {
            actions.push(AutonomicAction::Adjust);
            actions.push(AutonomicAction::Scale);
        }

        Ok(actions)
    }

    /// Select best action
    async fn select_best_action(&self, actions: &[AutonomicAction], analysis: &AnalysisResult) -> Result<AutonomicAction> {
        // Score each action
        let mut scored_actions = Vec::new();

        for action in actions {
            let score = self.score_action(action, analysis).await?;
            scored_actions.push((action.clone(), score));
        }

        // Sort by score (highest first)
        scored_actions.sort_by(|a, b| b.1.partial_cmp(&a.1).unwrap_or(std::cmp::Ordering::Equal));

        // Return best action
        if let Some((best_action, _)) = scored_actions.first() {
            Ok(best_action.clone())
        } else {
            Ok(AutonomicAction::Continue)
        }
    }

    /// Score an action
    async fn score_action(&self, action: &AutonomicAction, analysis: &AnalysisResult) -> Result<f64> {
        let mut score = 0.0;

        // Base score for action type
        match action {
            AutonomicAction::Continue => score += 0.5,
            AutonomicAction::Optimize => score += 0.8,
            AutonomicAction::Adjust => score += 0.7,
            AutonomicAction::Scale => score += 0.6,
            AutonomicAction::Intervene => score += 0.9,
            AutonomicAction::Escalate => score += 1.0,
            AutonomicAction::Reconfigure => score += 0.8,
            AutonomicAction::Recover => score += 0.85,
        }

        // Adjust for priority
        match analysis.priority {
            DecisionPriority::Low => score *= 0.5,
            DecisionPriority::Medium => score *= 0.7,
            DecisionPriority::High => score *= 0.9,
            DecisionPriority::Critical => score *= 1.2,
            DecisionPriority::Emergency => score *= 1.5,
        }

        // Adjust for expected impact
        score *= (analysis.expected_impact * 2.0).min(1.0);

        Ok(score)
    }

    /// Calculate decision confidence
    fn calculate_confidence(&self, action: &AutonomicAction, analysis: &AnalysisResult, context: &AutonomicContext) -> f64 {
        let mut confidence = 0.5; // Base confidence

        // Increase confidence based on historical success
        if let Some(success_rate) = self.get_historical_success_rate(action, context) {
            confidence += (success_rate - 0.5) * 0.5;
        }

        // Increase confidence if we have complete data
        let completeness = analysis.factors.len() as f64 / 5.0; // Assuming 5 key metrics
        confidence += completeness * 0.3;

        // Adjust for critical issues
        if !analysis.critical_issues.is_empty() {
            confidence -= 0.2;
        }

        confidence.min(1.0).max(0.0)
    }

    /// Generate decision rationale
    fn generate_rationale(&self, action: &AutonomicAction, analysis: &AnalysisResult, context: &AutonomicContext) -> String {
        let mut rationale = String::new();

        rationale.push_str(format!("Decision: {:?}\n", action).as_str());

        rationale.push_str("Rationale:\n");
        rationale.push_str(&format!("- Expected impact: {:.2}\n", analysis.expected_impact));
        rationale.push_str(&format!("- Priority: {:?}\n", analysis.priority));

        if !analysis.critical_issues.is_empty() {
            rationale.push_str("- Critical issues identified:\n");
            for issue in &analysis.critical_issues {
                rationale.push_str(&format!("  * {}\n", issue));
            }
        }

        rationale.push_str(&format!("- Confidence score: {:.2}\n", self.calculate_confidence(action, analysis, context)));

        rationale
    }

    /// Create implementation plan
    async fn create_implementation_plan(&self, action: &AutonomicAction, analysis: &AnalysisResult) -> Result<Value> {
        let mut plan = serde_json::json!({
            "action": format!("{:?}", action),
            "priority": analysis.priority,
            "expected_impact": analysis.expected_impact,
            "steps": []
        });

        // Add implementation steps
        let steps = self.generate_implementation_steps(action, analysis).await?;
        plan["steps"] = serde_json::to_value(steps)?;

        Ok(plan)
    }

    /// Generate implementation steps
    async fn generate_implementation_steps(&self, action: &AutonomicAction, analysis: &AnalysisResult) -> Result<Vec<String>> {
        let mut steps = Vec::new();

        match action {
            AutonomicAction::Adjust => {
                steps.push("Analyze current configuration".to_string());
                steps.push("Identify optimal adjustment parameters".to_string());
                steps.push("Apply configuration changes".to_string());
                steps.push("Validate changes are effective".to_string());
            }
            AutonomicAction::Scale => {
                steps.push("Monitor resource utilization".to_string());
                steps.push("Determine scaling requirements".to_string());
                steps.push("Scale resources up/down".to_string());
                steps.push("Verify scaling effectiveness".to_string());
            }
            AutonomicAction::Optimize => {
                steps.push("Identify optimization opportunities".to_string());
                steps.push("Implement optimizations".to_string());
                steps.push("Measure performance improvements".to_string());
                steps.push("Document changes".to_string());
            }
            AutonomicAction::Recover => {
                steps.push("Assess scope of failure".to_string());
                steps.push("Initiate recovery procedures".to_string());
                steps.push("Restore affected services".to_string());
                steps.push("Prevent recurrence".to_string());
            }
            _ => {
                steps.push("Evaluate situation".to_string());
                steps.push("Execute action".to_string());
                steps.push("Monitor results".to_string());
            }
        }

        Ok(steps)
    }

    /// Record decision for learning
    async fn record_decision(&mut self, context: &AutonomicContext, decision: &AutonomicDecision) {
        let data = DecisionData {
            timestamp: Instant::now(),
            decision: decision.clone(),
            outcome: DecisionOutcome::Success, // Will be updated later
            context: serde_json::json!({
                "domain": format!("{:?}", context.domain),
                "performance_threshold": context.performance_threshold,
                "quality_threshold": context.quality_threshold,
            }),
        };

        self.decision_history.push(data);

        // Keep only last 1000 decisions
        if self.decision_history.len() > 1000 {
            self.decision_history.remove(0);
        }
    }

    /// Get historical success rate for an action
    fn get_historical_success_rate(&self, action: &AutonomicAction, context: &AutonomicContext) -> Option<f64> {
        let mut total = 0;
        let mut successes = 0;

        for decision in &self.decision_history {
            if decision.decision.action == *action {
                total += 1;
                if matches!(decision.outcome, DecisionOutcome::Success | DecisionOutcome::PartialSuccess) {
                    successes += 1;
                }
            }
        }

        if total > 0 {
            Some(successes as f64 / total as f64)
        } else {
            None
        }
    }

    /// Determine decision priority
    fn determine_priority(&self, factors: &HashMap<String, f64>, context: &AutonomicContext) -> DecisionPriority {
        // Check for critical indicators
        if let Some(value) = factors.get("error_rate") {
            if *value > 0.1 {
                return DecisionPriority::Critical;
            }
        }

        if let Some(value) = factors.get("risk_score") {
            if *value > 0.9 {
                return DecisionPriority::Emergency;
            }
        }

        // Check quality threshold
        if context.quality_threshold > 0.98 {
            if let Some(value) = factors.get("defect_count") {
                if *value > 0 {
                    return DecisionPriority::High;
                }
            }
        }

        // Check performance threshold
        if context.performance_threshold > 0.95 {
            if let Some(value) = factors.get("response_time") {
                if *value > 200.0 {
                    return DecisionPriority::Medium;
                }
            }
        }

        DecisionPriority::Low
    }

    /// Calculate expected impact
    fn calculate_expected_impact(&self, factors: &HashMap<String, f64>, context: &AutonomicContext) -> f64 {
        let mut impact = 0.0;
        let mut count = 0;

        // Factor in error rate
        if let Some(error_rate) = factors.get("error_rate") {
            impact += *error_rate * 0.4;
            count += 1;
        }

        // Factor in response time
        if let Some(response_time) = factors.get("response_time") {
            let normalized_response = (*response_time / 1000.0).min(1.0);
            impact += normalized_response * 0.3;
            count += 1;
        }

        // Factor in quality
        impact += context.quality_threshold * 0.3;
        count += 1;

        if count > 0 {
            impact / count as f64
        } else {
            0.0
        }
    }

    /// Identify critical issues
    fn identify_critical_issues(&self, factors: &HashMap<String, f64>, context: &AutonomicContext) -> Vec<String> {
        let mut issues = Vec::new();

        if let Some(error_rate) = factors.get("error_rate") {
            if *error_rate > 0.1 {
                issues.push("High error rate detected".to_string());
            }
        }

        if let Some(response_time) = factors.get("response_time") {
            if *response_time > 500.0 {
                issues.push("Slow response time".to_string());
            }
        }

        if let Some(value) = factors.get("risk_score") {
            if *value > 0.8 {
                issues.push("High risk score".to_string());
            }
        }

        issues
    }
}

/// Decision model for autonomic decision making
#[derive(Debug, Clone)]
pub struct DecisionModel {
    pub name: String,
    pub metrics: Vec<String>,
    pub priority: DecisionPriority,
}

impl DecisionModel {
    pub fn new(name: String, metrics: Vec<String>, priority: DecisionPriority) -> Self {
        Self {
            name,
            metrics,
            priority,
        }
    }
}

/// Analysis result for decision making
#[derive(Debug, Clone)]
pub struct AnalysisResult {
    pub factors: HashMap<String, f64>,
    pub priority: DecisionPriority,
    pub expected_impact: f64,
    pub critical_issues: Vec<String>,
}

impl Default for AutonomicEngine {
    fn default() -> Self {
        Self::new()
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[tokio::test]
    async fn test_autonomic_decision_making() {
        let engine = AutonomicEngine::new();

        let context = AutonomicContext {
            domain: DomainType::Production,
            performance_threshold: 0.95,
            quality_threshold: 0.98,
            operational_state: AutonomicState::Monitoring,
            environmental_factors: HashMap::new(),
            historical_data: Vec::new(),
        };

        let decision = engine.make_decision(&context).await;
        assert!(decision.is_ok());
        assert!(decision.unwrap().confidence >= 0.0 && decision.unwrap().confidence <= 1.0);
    }

    #[test]
    fn test_priority_determination() {
        let engine = AutonomicEngine::new();

        let mut context = AutonomicContext {
            domain: DomainType::Production,
            performance_threshold: 0.95,
            quality_threshold: 0.98,
            operational_state: AutonomicState::Monitoring,
            environmental_factors: HashMap::new(),
            historical_data: Vec::new(),
        };

        let mut factors = HashMap::new();
        factors.insert("error_rate", 0.05);

        let priority = engine.determine_priority(&factors, &context);
        assert_eq!(priority, DecisionPriority::Low);
    }

    #[test]
    fn test_impact_calculation() {
        let engine = AutonomicEngine::new();

        let context = AutonomicContext {
            domain: DomainType::Production,
            performance_threshold: 0.95,
            quality_threshold: 0.98,
            operational_state: AutonomicState::Monitoring,
            environmental_factors: HashMap::new(),
            historical_data: Vec::new(),
        };

        let factors = HashMap::new();
        let impact = engine.calculate_expected_impact(&factors, &context);
        assert!(impact >= 0.0 && impact <= 1.0);
    }
}