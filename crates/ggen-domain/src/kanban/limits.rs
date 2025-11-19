// WIP (Work-in-Progress) limits for concurrent template modifications

use super::{WorkflowStage, KanbanError, KanbanResult};
use serde::{Deserialize, Serialize};
use std::collections::HashMap;

/// Policy for enforcing WIP limits
#[derive(Debug, Clone, Copy, PartialEq, Eq, Serialize, Deserialize)]
#[serde(rename_all = "snake_case")]
pub enum WipPolicy {
    /// Strictly enforce limits (reject new work)
    Strict,
    /// Warn when limit is exceeded but allow
    Warn,
    /// Soft limit - allow controlled overflow
    Soft { overflow_percent: u8 },
}

/// WIP limits configuration
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct WipLimits {
    /// Limits per stage
    stage_limits: HashMap<WorkflowStage, usize>,
    /// Policy for enforcement
    policy: WipPolicy,
    /// Maximum concurrent template modifications across all stages
    max_concurrent_modifications: usize,
    /// Enable dynamic limit adjustment
    dynamic_adjustment: bool,
    /// Throughput history for dynamic adjustment
    throughput_history: Vec<f64>,
}

impl WipLimits {
    /// Create new WIP limits with default values
    pub fn new(policy: WipPolicy) -> Self {
        let mut stage_limits = HashMap::new();
        stage_limits.insert(WorkflowStage::Backlog, 100);
        stage_limits.insert(WorkflowStage::Analysis, 5);
        stage_limits.insert(WorkflowStage::Transformation, 3);
        stage_limits.insert(WorkflowStage::Validation, 5);
        stage_limits.insert(WorkflowStage::Generation, 2);
        stage_limits.insert(WorkflowStage::Done, 1000);

        Self {
            stage_limits,
            policy,
            max_concurrent_modifications: 3,
            dynamic_adjustment: false,
            throughput_history: Vec::new(),
        }
    }

    /// Set limit for a specific stage
    pub fn set_limit(&mut self, stage: WorkflowStage, limit: usize) {
        self.stage_limits.insert(stage, limit);
    }

    /// Get limit for a stage
    pub fn get_limit(&self, stage: WorkflowStage) -> usize {
        self.stage_limits.get(&stage).copied().unwrap_or(10)
    }

    /// Set maximum concurrent modifications
    pub fn set_max_concurrent_modifications(&mut self, max: usize) {
        self.max_concurrent_modifications = max;
    }

    /// Enable dynamic limit adjustment based on throughput
    pub fn enable_dynamic_adjustment(&mut self) {
        self.dynamic_adjustment = true;
    }

    /// Check if adding work to a stage would violate WIP limits
    pub fn can_add_work(&self, stage: WorkflowStage, current_wip: usize) -> bool {
        let limit = self.get_limit(stage);

        match self.policy {
            WipPolicy::Strict => current_wip < limit,
            WipPolicy::Warn => true, // Always allow, just warn
            WipPolicy::Soft { overflow_percent } => {
                let max_allowed = limit + (limit * overflow_percent as usize / 100);
                current_wip < max_allowed
            }
        }
    }

    /// Record throughput for dynamic adjustment
    pub fn record_throughput(&mut self, throughput: f64) {
        self.throughput_history.push(throughput);
        if self.throughput_history.len() > 100 {
            self.throughput_history.remove(0);
        }

        if self.dynamic_adjustment && self.throughput_history.len() >= 10 {
            self.adjust_limits();
        }
    }

    /// Dynamically adjust limits based on throughput trends
    fn adjust_limits(&mut self) {
        if self.throughput_history.len() < 10 {
            return;
        }

        // Calculate moving average
        let recent: Vec<_> = self.throughput_history.iter().rev().take(10).collect();
        let avg = recent.iter().copied().sum::<f64>() / recent.len() as f64;

        // If throughput is consistently low, reduce WIP limits
        if avg < 1.0 {
            for (stage, limit) in self.stage_limits.iter_mut() {
                if *stage != WorkflowStage::Backlog && *stage != WorkflowStage::Done {
                    *limit = (*limit).max(1) - 1; // Reduce but keep at least 1
                }
            }
        }
        // If throughput is high, cautiously increase limits
        else if avg > 5.0 {
            for (stage, limit) in self.stage_limits.iter_mut() {
                if *stage != WorkflowStage::Backlog && *stage != WorkflowStage::Done {
                    *limit = (*limit + 1).min(10); // Increase but cap at 10
                }
            }
        }
    }

    /// Get all stage limits as a map
    pub fn as_map(&self) -> HashMap<String, usize> {
        self.stage_limits
            .iter()
            .map(|(stage, limit)| (stage.as_str().to_string(), *limit))
            .collect()
    }
}

/// Enforcer for WIP limits with violation tracking
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct LimitEnforcer {
    limits: WipLimits,
    violations: Vec<WipViolation>,
    warnings_issued: usize,
}

/// Record of a WIP limit violation
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct WipViolation {
    pub stage: WorkflowStage,
    pub attempted_wip: usize,
    pub limit: usize,
    pub timestamp: chrono::DateTime<chrono::Utc>,
    pub allowed: bool, // Whether violation was allowed under policy
}

impl LimitEnforcer {
    /// Create a new limit enforcer
    pub fn new(limits: WipLimits) -> Self {
        Self {
            limits,
            violations: Vec::new(),
            warnings_issued: 0,
        }
    }

    /// Check and enforce WIP limit for a stage
    pub fn enforce(
        &mut self,
        stage: WorkflowStage,
        current_wip: usize,
    ) -> KanbanResult<EnforcementResult> {
        let limit = self.limits.get_limit(stage);
        let can_add = self.limits.can_add_work(stage, current_wip);

        if current_wip >= limit {
            let violation = WipViolation {
                stage,
                attempted_wip: current_wip + 1,
                limit,
                timestamp: chrono::Utc::now(),
                allowed: can_add,
            };

            self.violations.push(violation);

            match self.limits.policy {
                WipPolicy::Strict => {
                    return Err(KanbanError::WipLimitExceeded(
                        current_wip,
                        stage.as_str().to_string(),
                        limit,
                    ));
                }
                WipPolicy::Warn => {
                    self.warnings_issued += 1;
                    return Ok(EnforcementResult::Warning {
                        message: format!(
                            "WIP limit warning: {} items in {}, limit is {}",
                            current_wip + 1,
                            stage.as_str(),
                            limit
                        ),
                    });
                }
                WipPolicy::Soft { overflow_percent } => {
                    if can_add {
                        return Ok(EnforcementResult::SoftLimitExceeded {
                            current: current_wip + 1,
                            limit,
                            overflow_percent,
                        });
                    } else {
                        return Err(KanbanError::WipLimitExceeded(
                            current_wip,
                            stage.as_str().to_string(),
                            limit,
                        ));
                    }
                }
            }
        }

        Ok(EnforcementResult::Allowed)
    }

    /// Get recent violations
    pub fn recent_violations(&self, count: usize) -> &[WipViolation] {
        let start = self.violations.len().saturating_sub(count);
        &self.violations[start..]
    }

    /// Get total violations
    pub fn total_violations(&self) -> usize {
        self.violations.len()
    }

    /// Get total warnings issued
    pub fn total_warnings(&self) -> usize {
        self.warnings_issued
    }

    /// Check if stage is at risk of limit violation
    pub fn is_at_risk(&self, stage: WorkflowStage, current_wip: usize) -> bool {
        let limit = self.limits.get_limit(stage);
        let threshold = (limit as f64 * 0.8) as usize;
        current_wip >= threshold
    }

    /// Get limit for a stage
    pub fn get_limit(&self, stage: WorkflowStage) -> usize {
        self.limits.get_limit(stage)
    }

    /// Update limits configuration
    pub fn update_limits(&mut self, limits: WipLimits) {
        self.limits = limits;
    }

    /// Record throughput for dynamic adjustment
    pub fn record_throughput(&mut self, throughput: f64) {
        self.limits.record_throughput(throughput);
    }
}

/// Result of WIP limit enforcement
#[derive(Debug, Clone, PartialEq)]
pub enum EnforcementResult {
    /// Work is allowed
    Allowed,
    /// Warning issued but work is allowed
    Warning { message: String },
    /// Soft limit exceeded but within overflow
    SoftLimitExceeded {
        current: usize,
        limit: usize,
        overflow_percent: u8,
    },
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_wip_limits_strict_policy() {
        let limits = WipLimits::new(WipPolicy::Strict);

        // Should allow within limit
        assert!(limits.can_add_work(WorkflowStage::Transformation, 2));

        // Should reject at limit
        assert!(!limits.can_add_work(WorkflowStage::Transformation, 3));
    }

    #[test]
    fn test_wip_limits_soft_policy() {
        let limits = WipLimits::new(WipPolicy::Soft { overflow_percent: 20 });

        // Transformation limit is 3, so with 20% overflow, max is 3
        assert!(limits.can_add_work(WorkflowStage::Transformation, 3));

        // Should reject beyond overflow
        assert!(!limits.can_add_work(WorkflowStage::Transformation, 4));
    }

    #[test]
    fn test_wip_limits_warn_policy() {
        let limits = WipLimits::new(WipPolicy::Warn);

        // Should always allow under warn policy
        assert!(limits.can_add_work(WorkflowStage::Transformation, 100));
    }

    #[test]
    fn test_limit_enforcer_strict() {
        let limits = WipLimits::new(WipPolicy::Strict);
        let mut enforcer = LimitEnforcer::new(limits);

        // Should allow within limit
        let result = enforcer.enforce(WorkflowStage::Transformation, 2);
        assert!(result.is_ok());
        assert_eq!(result.unwrap(), EnforcementResult::Allowed);

        // Should reject at limit
        let result = enforcer.enforce(WorkflowStage::Transformation, 3);
        assert!(result.is_err());
        assert_eq!(enforcer.total_violations(), 1);
    }

    #[test]
    fn test_limit_enforcer_warn() {
        let limits = WipLimits::new(WipPolicy::Warn);
        let mut enforcer = LimitEnforcer::new(limits);

        // Should warn but allow
        let result = enforcer.enforce(WorkflowStage::Transformation, 3);
        assert!(result.is_ok());

        match result.unwrap() {
            EnforcementResult::Warning { .. } => {},
            _ => panic!("Expected warning"),
        }

        assert_eq!(enforcer.total_warnings(), 1);
        assert_eq!(enforcer.total_violations(), 1);
    }

    #[test]
    fn test_dynamic_limit_adjustment() {
        let mut limits = WipLimits::new(WipPolicy::Strict);
        limits.enable_dynamic_adjustment();

        let original_limit = limits.get_limit(WorkflowStage::Transformation);

        // Record low throughput
        for _ in 0..10 {
            limits.record_throughput(0.5);
        }

        let new_limit = limits.get_limit(WorkflowStage::Transformation);
        assert!(new_limit < original_limit);
    }

    #[test]
    fn test_at_risk_detection() {
        let limits = WipLimits::new(WipPolicy::Strict);
        let enforcer = LimitEnforcer::new(limits);

        // At 80% of limit (3), should be at risk
        assert!(enforcer.is_at_risk(WorkflowStage::Transformation, 2));
        assert!(!enforcer.is_at_risk(WorkflowStage::Transformation, 1));
    }

    #[test]
    fn test_custom_limits() {
        let mut limits = WipLimits::new(WipPolicy::Strict);
        limits.set_limit(WorkflowStage::Analysis, 10);

        assert_eq!(limits.get_limit(WorkflowStage::Analysis), 10);
    }
}
