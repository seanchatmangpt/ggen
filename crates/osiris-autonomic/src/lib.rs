//! OSIRIS Autonomic - Autonomic Refusal System
//!
//! This crate implements autonomic refusal mechanisms for life management,
//! allowing intelligent decision-making based on context, policies, and learned preferences.

use anyhow::Result;
use serde::{Deserialize, Serialize};
use std::collections::HashMap;
use tokio::sync::RwLock;
use tracing::{debug, info, warn};
use uuid::Uuid;

/// Refusal decision types
#[derive(Debug, Clone, Serialize, Deserialize, PartialEq)]
pub enum RefusalType {
    // Positive decisions
    Accept,
    AcceptWithConditions(Vec<String>),

    // Refusal decisions
    Decline,
    DeclineWithReason(String),
    Postpone(Uuid, chrono::DateTime<chrono::Utc>), // ID and suggested time

    // System decisions
    Override,       // System override for critical operations
    Escalate,       // Escalate to higher authority
    Delegate(Uuid), // Delegate to another agent
}

/// Refusal context
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct RefusalContext {
    pub request_id: Uuid,
    pub request_type: String,
    pub requested_by: String,
    pub timestamp: chrono::DateTime<chrono::Utc>,
    pub priority: Priority,
    pub urgency: Urgency,
    pub risk_assessment: RiskLevel,
    pub user_context: HashMap<String, serde_json::Value>,
    pub system_state: HashMap<String, serde_json::Value>,
}

/// Priority levels
#[derive(Debug, Clone, Serialize, Deserialize, PartialEq)]
pub enum Priority {
    Low,
    Normal,
    High,
    Critical,
}

/// Urgency levels
#[derive(Debug, Clone, Serialize, Deserialize, PartialEq)]
pub enum Urgency {
    Planned,
    Routine,
    Urgent,
    Emergency,
}

/// Risk levels
#[derive(Debug, Clone, Serialize, Deserialize, PartialEq)]
pub enum RiskLevel {
    Minimal,
    Low,
    Medium,
    High,
    Critical,
}

/// Refusal policy
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct RefusalPolicy {
    pub id: String,
    pub name: String,
    pub description: String,
    pub conditions: Vec<PolicyCondition>,
    pub actions: Vec<PolicyAction>,
    pub active: bool,
    pub priority: i32,
}

/// Policy condition
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct PolicyCondition {
    pub field: String,
    pub operator: ConditionOperator,
    pub value: serde_json::Value,
}

/// Policy action
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct PolicyAction {
    pub action_type: String,
    pub parameters: HashMap<String, serde_json::Value>,
}

/// Condition operators
#[derive(Debug, Clone, Serialize, Deserialize, PartialEq)]
pub enum ConditionOperator {
    Equals,
    NotEquals,
    GreaterThan,
    LessThan,
    Contains,
    NotContains,
    In,
    NotIn,
    Regex,
}

/// Autonomic refusal system
pub struct AutonomicRefusalSystem {
    policies: RwLock<Vec<RefusalPolicy>>,
    history: RwLock<Vec<RefusalDecision>>,
    learned_preferences: RwLock<HashMap<String, serde_json::Value>>,
    is_running: RwLock<bool>,
}

impl AutonomicRefusalSystem {
    /// Create a new autonomic refusal system
    pub fn new() -> Result<Self> {
        Ok(Self {
            policies: RwLock::new(Vec::new()),
            history: RwLock::new(Vec::new()),
            learned_preferences: RwLock::new(HashMap::new()),
            is_running: RwLock::new(false),
        })
    }

    /// Start the autonomic system
    pub async fn start(&self) -> Result<()> {
        let mut running = self.is_running.write().await;
        if *running {
            warn!("Autonomic system already running");
            return Ok(());
        }

        info!("Starting OSIRIS autonomic refusal system");

        // Load default policies
        self.load_default_policies().await?;

        *running = true;
        Ok(())
    }

    /// Stop the autonomic system
    pub async fn stop(&self) -> Result<()> {
        let mut running = self.is_running.write().await;
        if !*running {
            warn!("Autonomic system not running");
            return Ok(());
        }

        info!("Stopping OSIRIS autonomic refusal system");
        *running = false;
        Ok(())
    }

    /// Process a request and make a decision
    pub async fn process_request(&self, context: RefusalContext) -> Result<RefusalDecision> {
        let running = self.is_running.read().await;
        if !*running {
            return Err(anyhow::anyhow!("Autonomic system not running"));
        }

        info!("Processing request: {}", context.request_id);

        // Evaluate policies
        let policies = self.policies.read().await;
        let matching_policies = self.match_policies(&context, &policies).await?;

        // Make decision based on policies
        let decision = if matching_policies.is_empty() {
            RefusalDecision::new(context.clone(), RefusalType::Accept, None)
        } else {
            let best_policy = self.select_best_policy(&matching_policies).await?;
            self.execute_policy_actions(&context, &best_policy).await?
        };

        // Store in history
        let mut history = self.history.write().await;
        history.push(decision.clone());

        // Update learned preferences
        self.update_preferences(&context, &decision).await?;

        debug!("Decision made: {:?}", decision.refusal_type);
        Ok(decision)
    }

    /// Add a new policy
    pub async fn add_policy(&self, policy: RefusalPolicy) -> Result<()> {
        let mut policies = self.policies.write().await;
        policies.push(policy);
        policies.sort_by(|a, b| b.priority.cmp(&a.priority));
        if let Some(last_policy) = policies.last() {
            info!("Added new policy: {}", last_policy.name);
        } else {
            warn!("Policy list empty after adding policy");
        }
        Ok(())
    }

    /// Remove a policy by ID
    pub async fn remove_policy(&self, policy_id: &str) -> Result<()> {
        let mut policies = self.policies.write().await;
        policies.retain(|p| p.id != policy_id);
        info!("Removed policy: {}", policy_id);
        Ok(())
    }

    /// Get all policies
    pub async fn get_policies(&self) -> Vec<RefusalPolicy> {
        let policies = self.policies.read().await;
        policies.clone()
    }

    /// Get decision history
    pub async fn get_history(&self, limit: Option<usize>) -> Vec<RefusalDecision> {
        let history = self.history.read().await;
        match limit {
            Some(n) => history.iter().take(n).cloned().collect(),
            None => history.clone(),
        }
    }

    /// Process sensor data (from osiris-sensors)
    #[cfg(feature = "sensors")]
    pub async fn process_sensor_data(
        &self, sensor_id: &str, data: &[osiris_sensors::SensorDataType],
    ) -> Result<()> {
        info!("Processing sensor data from sensor: {}", sensor_id);

        // Analyze sensor data for patterns
        let patterns = self.analyze_sensor_patterns(sensor_id, data).await?;

        // Update risk assessment based on sensor data
        self.update_risk_assessment(patterns).await?;

        Ok(())
    }

    /// Load default policies
    async fn load_default_policies(&self) -> Result<()> {
        let default_policies = vec![
            // High-risk activity refusal policy
            RefusalPolicy {
                id: "high-risk-activity".to_string(),
                name: "High Risk Activity Refusal".to_string(),
                description: "Refuse high-risk activities based on context".to_string(),
                conditions: vec![PolicyCondition {
                    field: "risk_assessment".to_string(),
                    operator: ConditionOperator::Equals,
                    value: serde_json::Value::String("Critical".to_string()),
                }],
                actions: vec![PolicyAction {
                    action_type: "refuse".to_string(),
                    parameters: HashMap::new(),
                }],
                active: true,
                priority: 100,
            },
            // After-hours work refusal policy
            RefusalPolicy {
                id: "after-hours-work".to_string(),
                name: "After Hours Work Policy".to_string(),
                description: "Limit after-hours work except for emergencies".to_string(),
                conditions: vec![
                    PolicyCondition {
                        field: "timestamp".to_string(),
                        operator: ConditionOperator::GreaterThan,
                        value: serde_json::Value::Number(18.into()), // 6 PM
                    },
                    PolicyCondition {
                        field: "urgency".to_string(),
                        operator: ConditionOperator::NotEquals,
                        value: serde_json::Value::String("Emergency".to_string()),
                    },
                ],
                actions: vec![PolicyAction {
                    action_type: "postpone".to_string(),
                    parameters: HashMap::new(),
                }],
                active: true,
                priority: 50,
            },
        ];

        let mut policies = self.policies.write().await;
        policies.extend(default_policies);
        policies.sort_by(|a, b| b.priority.cmp(&a.priority));

        info!("Loaded {} default policies", policies.len());
        Ok(())
    }

    /// Match policies against context
    async fn match_policies(
        &self, context: &RefusalContext, policies: &[RefusalPolicy],
    ) -> Result<Vec<RefusalPolicy>> {
        let mut matching = Vec::new();

        for policy in policies {
            if !policy.active {
                continue;
            }

            let mut matches_all = true;
            for condition in &policy.conditions {
                if !self.evaluate_condition(context, condition).await? {
                    matches_all = false;
                    break;
                }
            }

            if matches_all {
                matching.push(policy.clone());
            }
        }

        Ok(matching)
    }

    /// Evaluate a policy condition
    async fn evaluate_condition(
        &self, context: &RefusalContext, condition: &PolicyCondition,
    ) -> Result<bool> {
        let field_value = self.get_field_value(context, &condition.field).await?;

        match condition.operator {
            ConditionOperator::Equals => Ok(field_value == condition.value),
            ConditionOperator::NotEquals => Ok(field_value != condition.value),
            ConditionOperator::GreaterThan => {
                match (field_value.as_f64(), condition.value.as_f64()) {
                    (Some(a), Some(b)) => Ok(a > b),
                    _ => Ok(false),
                }
            }
            ConditionOperator::LessThan => match (field_value.as_f64(), condition.value.as_f64()) {
                (Some(a), Some(b)) => Ok(a < b),
                _ => Ok(false),
            },
            _ => {
                debug!("Unimplemented operator: {:?}", condition.operator);
                Ok(false)
            }
        }
    }

    /// Get field value from context
    async fn get_field_value(
        &self, context: &RefusalContext, field: &str,
    ) -> Result<serde_json::Value> {
        match field {
            "priority" => Ok(serde_json::Value::String(format!("{:?}", context.priority))),
            "urgency" => Ok(serde_json::Value::String(format!("{:?}", context.urgency))),
            "risk_assessment" => Ok(serde_json::Value::String(format!(
                "{:?}",
                context.risk_assessment
            ))),
            _ => {
                // Check user context first
                if let Some(value) = context.user_context.get(field) {
                    Ok(value.clone())
                } else if let Some(value) = context.system_state.get(field) {
                    Ok(value.clone())
                } else {
                    Ok(serde_json::Value::Null)
                }
            }
        }
    }

    /// Select the best matching policy
    async fn select_best_policy(&self, policies: &[RefusalPolicy]) -> Result<RefusalPolicy> {
        Ok(policies[0].clone()) // Policies are sorted by priority
    }

    /// Execute policy actions
    async fn execute_policy_actions(
        &self, context: &RefusalContext, policy: &RefusalPolicy,
    ) -> Result<RefusalDecision> {
        let mut refusal_type = RefusalType::Accept;
        let mut reason = None;

        for action in &policy.actions {
            match action.action_type.as_str() {
                "refuse" => {
                    refusal_type = RefusalType::Decline;
                    reason = Some(format!("Refused by policy: {}", policy.name));
                }
                "postpone" => {
                    let suggested_time = chrono::Utc::now() + chrono::Duration::hours(1);
                    refusal_type = RefusalType::Postpone(context.request_id, suggested_time);
                }
                "decline" => {
                    refusal_type = RefusalType::DeclineWithReason(policy.name.clone());
                }
                _ => {
                    debug!("Unknown action type: {}", action.action_type);
                }
            }
        }

        Ok(RefusalDecision::new(context.clone(), refusal_type, reason))
    }

    /// Update learned preferences
    async fn update_preferences(
        &self, context: &RefusalContext, decision: &RefusalDecision,
    ) -> Result<()> {
        let mut preferences = self.learned_preferences.write().await;

        // Simple preference learning - track rejections
        if matches!(
            decision.refusal_type,
            RefusalType::Decline | RefusalType::DeclineWithReason(_)
        ) {
            let key = format!("rejected_{}", context.request_type);
            let count = preferences
                .entry(key.clone())
                .or_insert(serde_json::Value::Number(0.into()));
            if let serde_json::Value::Number(n) = count {
                *count = serde_json::Value::Number((n.as_u64().unwrap_or(0) + 1).into());
            }
        }

        Ok(())
    }

    /// Analyze sensor data patterns
    #[cfg(feature = "sensors")]
    async fn analyze_sensor_patterns(
        &self, sensor_id: &str, data: &[osiris_sensors::SensorDataType],
    ) -> Result<Vec<String>> {
        let mut patterns = Vec::new();

        // Simple pattern detection
        if sensor_id == "accelerometer" {
            // Detect sudden movements
            let movements: Vec<_> = data
                .iter()
                .filter(|d| matches!(d, SensorDataType::Accelerometer { .. }))
                .collect();

            if movements.len() > 10 {
                patterns.push("high_movement_detected".to_string());
            }
        }

        Ok(patterns)
    }

    /// Update risk assessment based on patterns
    async fn update_risk_assessment(&self, patterns: Vec<String>) -> Result<()> {
        // Update system state based on patterns
        let _ = patterns; // In a real implementation, this would update risk levels
        debug!("Updated risk assessment based on patterns");
        Ok(())
    }
}

/// Refusal decision record
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct RefusalDecision {
    pub id: Uuid,
    pub context: RefusalContext,
    pub refusal_type: RefusalType,
    pub reason: Option<String>,
    pub timestamp: chrono::DateTime<chrono::Utc>,
    pub confidence: f64,
}

impl RefusalDecision {
    pub fn new(context: RefusalContext, refusal_type: RefusalType, reason: Option<String>) -> Self {
        Self {
            id: Uuid::new_v4(),
            context,
            refusal_type,
            reason,
            timestamp: chrono::Utc::now(),
            confidence: 0.8, // Default confidence
        }
    }
}

impl Default for AutonomicRefusalSystem {
    fn default() -> Self {
        Self::new().expect("Failed to create autonomic system")
    }
}
