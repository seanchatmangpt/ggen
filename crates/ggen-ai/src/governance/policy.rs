//! Policy Engine for Governance
//!
//! Defines and enforces policies that constrain autonomous system behavior.

use chrono::{DateTime, Utc};
use serde::{Deserialize, Serialize};
use std::collections::HashMap;
use std::sync::Arc;
use tokio::sync::RwLock;

use super::error::{GovernanceError, Result};
use super::types::Decision;
use crate::types::{PolicyId, RuleId};

/// A policy rule that constrains autonomous behavior
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct Policy {
    pub id: PolicyId,
    pub name: String,
    pub description: String,
    pub enabled: bool,
    pub priority: i32,
    pub rules: Vec<PolicyRule>,
    pub constraints: HashMap<String, Constraint>,
    pub created_at: DateTime<Utc>,
    pub updated_at: DateTime<Utc>,
    pub metadata: HashMap<String, String>,
}

/// Individual policy rule
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct PolicyRule {
    pub id: RuleId,
    pub condition: RuleCondition,
    pub action: RuleAction,
    pub severity: Severity,
}

/// Rule condition types
#[derive(Debug, Clone, Serialize, Deserialize)]
pub enum RuleCondition {
    /// Check if rate limit exceeded
    RateLimit {
        window_seconds: u64,
        max_operations: u64,
    },
    /// Check if mutation count exceeded
    MutationLimit { max_mutations: u64 },
    /// Check if field matches pattern
    FieldMatch { field: String, pattern: String },
    /// Check if value in allowed list
    AllowList {
        field: String,
        allowed_values: Vec<String>,
    },
    /// Check if value not in blocked list
    BlockList {
        field: String,
        blocked_values: Vec<String>,
    },
    /// Custom condition (evaluated via extension)
    Custom {
        name: String,
        parameters: HashMap<String, String>,
    },
}

/// Action to take when rule matches
#[derive(Debug, Clone, Serialize, Deserialize)]
pub enum RuleAction {
    Approve,
    Reject,
    RequireApproval,
    Log,
    Alert,
}

/// Severity levels
#[derive(Debug, Clone, Serialize, Deserialize, PartialEq, PartialOrd)]
pub enum Severity {
    Info,
    Warning,
    Error,
    Critical,
}

/// Constraint types for policies
#[derive(Debug, Clone, Serialize, Deserialize)]
pub enum Constraint {
    Numeric { min: Option<f64>, max: Option<f64> },
    String { pattern: String },
    List { allowed: Vec<String> },
    Boolean { required_value: bool },
}

/// Policy violation
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct PolicyViolation {
    pub policy_id: PolicyId,
    pub rule_id: RuleId,
    pub severity: Severity,
    pub message: String,
    pub details: HashMap<String, String>,
}

/// Configuration for policy engine
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct PolicyConfig {
    pub strict_mode: bool,
    pub log_all_evaluations: bool,
    pub default_action: RuleAction,
}

impl Default for PolicyConfig {
    fn default() -> Self {
        Self {
            strict_mode: true,
            log_all_evaluations: true,
            // Default to Approve to allow low-risk decisions through when no policies match
            default_action: RuleAction::Approve,
        }
    }
}

/// Policy engine that evaluates decisions against policies
pub struct PolicyEngine {
    policies: Arc<RwLock<HashMap<PolicyId, Policy>>>,
    config: PolicyConfig,
    violation_history: Arc<RwLock<Vec<PolicyViolation>>>,
}

impl PolicyEngine {
    /// Create a new policy engine
    pub fn new(config: PolicyConfig) -> Self {
        Self {
            policies: Arc::new(RwLock::new(HashMap::new())),
            config,
            violation_history: Arc::new(RwLock::new(Vec::new())),
        }
    }

    /// Register a new policy
    pub async fn register_policy(&self, policy: Policy) -> Result<()> {
        let mut policies = self.policies.write().await;
        policies.insert(policy.id.clone(), policy);
        Ok(())
    }

    /// Remove a policy
    pub async fn remove_policy(&self, policy_id: &PolicyId) -> Result<()> {
        let mut policies = self.policies.write().await;
        policies
            .remove(policy_id)
            .ok_or_else(|| GovernanceError::PolicyNotFound(policy_id.to_string()))?;
        Ok(())
    }

    /// Get a policy by ID
    pub async fn get_policy(&self, policy_id: &PolicyId) -> Result<Policy> {
        let policies = self.policies.read().await;
        policies
            .get(policy_id)
            .cloned()
            .ok_or_else(|| GovernanceError::PolicyNotFound(policy_id.to_string()))
    }

    /// List all policies
    pub async fn list_policies(&self) -> Result<Vec<Policy>> {
        let policies = self.policies.read().await;
        Ok(policies.values().cloned().collect())
    }

    /// Validate a decision against all policies
    pub async fn validate(&self, decision: &Decision) -> Result<bool> {
        let policies = self.policies.read().await;

        for policy in policies.values() {
            if !policy.enabled {
                continue;
            }

            for rule in &policy.rules {
                if self.evaluate_rule(rule, decision).await? {
                    match &rule.action {
                        RuleAction::Reject => {
                            self.record_violation(policy, rule, decision).await?;
                            return Ok(false);
                        }
                        RuleAction::RequireApproval => {
                            // Approval required, but not outright rejection
                            return Ok(true);
                        }
                        RuleAction::Alert => {
                            self.record_violation(policy, rule, decision).await?;
                            // Continue evaluation
                        }
                        RuleAction::Log => {
                            tracing::info!("Policy rule matched: {} - {}", policy.name, rule.id);
                        }
                        RuleAction::Approve => {
                            return Ok(true);
                        }
                    }
                }
            }
        }

        // Default to configured action if no rules matched
        Ok(matches!(self.config.default_action, RuleAction::Approve))
    }

    /// Evaluate a single rule against a decision
    async fn evaluate_rule(&self, rule: &PolicyRule, decision: &Decision) -> Result<bool> {
        match &rule.condition {
            RuleCondition::RateLimit {
                window_seconds,
                max_operations,
            } => {
                self.check_rate_limit(*window_seconds, *max_operations, decision)
                    .await
            }
            RuleCondition::MutationLimit { max_mutations } => {
                self.check_mutation_limit(*max_mutations, decision).await
            }
            RuleCondition::FieldMatch { field, pattern } => {
                self.check_field_match(field, pattern, decision).await
            }
            RuleCondition::AllowList {
                field,
                allowed_values,
            } => self.check_allow_list(field, allowed_values, decision).await,
            RuleCondition::BlockList {
                field,
                blocked_values,
            } => self.check_block_list(field, blocked_values, decision).await,
            RuleCondition::Custom { name, parameters } => {
                self.evaluate_custom_condition(name, parameters, decision)
                    .await
            }
        }
    }

    /// Check rate limit using sliding window algorithm
    async fn check_rate_limit(
        &self, window_seconds: u64, max_operations: u64, decision: &Decision,
    ) -> Result<bool> {
        // Extract decision context for rate limiting
        let decision_key = decision
            .metadata
            .get("rate_limit_key")
            .unwrap_or(&decision.action)
            .clone();

        // Get current timestamp
        let now = Utc::now().timestamp() as u64;
        let window_start = now.saturating_sub(window_seconds);

        // In a production system, this would use a distributed cache (Redis, etc.)
        // For now, we'll use metadata to track recent operations
        let history_key = format!("rate_limit_history_{}", decision_key);

        // Parse operation history from metadata
        let mut operation_times: Vec<u64> =
            if let Some(history) = decision.metadata.get(&history_key) {
                serde_json::from_str(history).unwrap_or_default()
            } else {
                Vec::new()
            };

        // Filter operations within sliding window
        operation_times.retain(|&timestamp| timestamp >= window_start);

        // Check if adding this operation would exceed the limit
        let operations_in_window = operation_times.len() as u64;
        let would_exceed = operations_in_window >= max_operations;

        if would_exceed {
            tracing::warn!(
                decision_id = %decision.id,
                operations = operations_in_window,
                max = max_operations,
                window_seconds = window_seconds,
                "Rate limit exceeded"
            );
        } else {
            // Add current operation to history
            operation_times.push(now);

            tracing::debug!(
                decision_id = %decision.id,
                operations = operations_in_window + 1,
                max = max_operations,
                "Rate limit check passed"
            );
        }

        Ok(would_exceed)
    }

    /// Check mutation limit
    async fn check_mutation_limit(&self, max_mutations: u64, decision: &Decision) -> Result<bool> {
        if let Some(mutations) = decision.metadata.get("mutation_count") {
            if let Ok(count) = mutations.parse::<u64>() {
                return Ok(count > max_mutations);
            }
        }
        Ok(false)
    }

    /// Check field match against pattern
    async fn check_field_match(
        &self, field: &str, pattern: &str, decision: &Decision,
    ) -> Result<bool> {
        if let Some(value) = decision.metadata.get(field) {
            let regex = regex::Regex::new(pattern)
                .map_err(|e| GovernanceError::InvalidPattern(e.to_string()))?;
            return Ok(regex.is_match(value));
        }
        Ok(false)
    }

    /// Check if field value is in allow list
    async fn check_allow_list(
        &self, field: &str, allowed_values: &[String], decision: &Decision,
    ) -> Result<bool> {
        if let Some(value) = decision.metadata.get(field) {
            return Ok(!allowed_values.contains(value));
        }
        Ok(false)
    }

    /// Check if field value is in block list
    async fn check_block_list(
        &self, field: &str, blocked_values: &[String], decision: &Decision,
    ) -> Result<bool> {
        if let Some(value) = decision.metadata.get(field) {
            return Ok(blocked_values.contains(value));
        }
        Ok(false)
    }

    /// Evaluate custom condition (extensibility point)
    async fn evaluate_custom_condition(
        &self, _name: &str, _parameters: &HashMap<String, String>, _decision: &Decision,
    ) -> Result<bool> {
        // TODO: Implement plugin system for custom conditions
        Ok(false)
    }

    /// Record a policy violation
    async fn record_violation(
        &self, policy: &Policy, rule: &PolicyRule, decision: &Decision,
    ) -> Result<()> {
        let violation = PolicyViolation {
            policy_id: policy.id.clone(),
            rule_id: rule.id.clone(),
            severity: rule.severity.clone(),
            message: format!(
                "Policy '{}' violated by decision '{}'",
                policy.name, decision.id
            ),
            details: decision.metadata.clone(),
        };

        let mut history = self.violation_history.write().await;
        history.push(violation);
        Ok(())
    }

    /// Get violations for a decision
    pub async fn get_violations(&self, decision: &Decision) -> Result<Vec<PolicyViolation>> {
        let history = self.violation_history.read().await;
        Ok(history
            .iter()
            .filter(|_v| {
                decision
                    .metadata
                    .get("decision_id")
                    .map(|id| id == &decision.id)
                    .unwrap_or(false)
            })
            .cloned()
            .collect())
    }
}

/// Builder for creating policies
pub struct PolicyBuilder {
    name: String,
    description: String,
    rules: Vec<PolicyRule>,
    constraints: HashMap<String, Constraint>,
    priority: i32,
    metadata: HashMap<String, String>,
}

impl PolicyBuilder {
    pub fn new(name: impl Into<String>) -> Self {
        Self {
            name: name.into(),
            description: String::new(),
            rules: Vec::new(),
            constraints: HashMap::new(),
            priority: 0,
            metadata: HashMap::new(),
        }
    }

    pub fn description(mut self, description: impl Into<String>) -> Self {
        self.description = description.into();
        self
    }

    pub fn rule(mut self, rule: PolicyRule) -> Self {
        self.rules.push(rule);
        self
    }

    pub fn constraint(mut self, key: impl Into<String>, constraint: Constraint) -> Self {
        self.constraints.insert(key.into(), constraint);
        self
    }

    pub fn priority(mut self, priority: i32) -> Self {
        self.priority = priority;
        self
    }

    pub fn metadata(mut self, key: impl Into<String>, value: impl Into<String>) -> Self {
        self.metadata.insert(key.into(), value.into());
        self
    }

    pub fn build(self) -> Result<Policy> {
        Ok(Policy {
            id: PolicyId::new(),
            name: self.name,
            description: self.description,
            enabled: true,
            priority: self.priority,
            rules: self.rules,
            constraints: self.constraints,
            created_at: Utc::now(),
            updated_at: Utc::now(),
            metadata: self.metadata,
        })
    }
}

impl Policy {
    pub fn builder(name: impl Into<String>) -> PolicyBuilder {
        PolicyBuilder::new(name)
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[tokio::test]
    async fn test_policy_creation() {
        let policy = Policy::builder("test-policy")
            .description("Test policy")
            .priority(10)
            .build()
            .expect("Failed to build policy");

        assert_eq!(policy.name, "test-policy");
        assert_eq!(policy.priority, 10);
    }

    #[tokio::test]
    async fn test_policy_engine() {
        let engine = PolicyEngine::new(PolicyConfig::default());
        let policy = Policy::builder("test")
            .description("Test")
            .build()
            .expect("Failed to build policy");

        engine
            .register_policy(policy.clone())
            .await
            .expect("Failed to register policy");
        let retrieved = engine
            .get_policy(&policy.id)
            .await
            .expect("Failed to retrieve policy");
        assert_eq!(retrieved.name, "test");
    }
}
