//! Configuration Agent - Dynamic Configuration and Feature Flags
//!
//! This agent manages dynamic configuration and feature flags for the MCP server:
//! - Runtime configuration updates
//! - Feature flag management
//! - Environment-specific configurations
//! - Configuration validation and schema enforcement
//! - Hot reloading of configuration changes
//!
//! # Configuration Patterns
//!
//! ## Dynamic Configuration
//! - **Runtime Updates** - Change configuration without restart
//! - **Validation** - Ensure configuration changes are valid
//! - **Rollback** - Revert to previous configuration if needed
//! - **Audit Trail** - Track all configuration changes
//!
//! ## Feature Flags
//! - **Boolean Flags** - Simple on/off features
//! - **Percentage Rollouts** - Gradual feature rollouts
//! - **User Targeting** - Feature flags for specific users
//! - **A/B Testing** - Compare feature variations
//!
//! ## Configuration Sources
//! - **File-based** - Configuration files
//! - **Environment Variables** - Environment-specific settings
//! - **Database** - Persistent configuration storage
//! - **API** - Remote configuration management

use crate::agents::{Agent, AgentMetadata, AgentStatus, AgentId};
use crate::error::{GgenMcpError, Result};
use serde::{Deserialize, Serialize};
use std::collections::HashMap;
use std::sync::RwLock;
use uuid::Uuid;
use chrono::Utc;

/// Configuration entry
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct ConfigurationEntry {
    pub key: String,
    pub value: serde_json::Value,
    pub data_type: ConfigurationType,
    pub description: String,
    pub default_value: Option<serde_json::Value>,
    pub validation_rules: Vec<ValidationRule>,
    pub last_updated: chrono::DateTime<Utc>,
    pub updated_by: String,
    pub version: u32,
}

/// Configuration type
#[derive(Debug, Clone, Serialize, Deserialize, PartialEq)]
pub enum ConfigurationType {
    String,
    Integer,
    Float,
    Boolean,
    Array,
    Object,
    Json,
}

/// Validation rule
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct ValidationRule {
    pub rule_type: ValidationRuleType,
    pub value: serde_json::Value,
    pub message: String,
}

/// Validation rule types
#[derive(Debug, Clone, Serialize, Deserialize, PartialEq)]
pub enum ValidationRuleType {
    MinValue,
    MaxValue,
    MinLength,
    MaxLength,
    Pattern,
    Required,
    Custom,
}

/// Feature flag definition
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct FeatureFlag {
    pub id: Uuid,
    pub name: String,
    pub description: String,
    pub flag_type: FeatureFlagType,
    pub enabled: bool,
    pub rollout_percentage: f64,
    pub target_users: Vec<String>,
    pub target_groups: Vec<String>,
    pub conditions: Vec<FeatureCondition>,
    pub created_at: chrono::DateTime<Utc>,
    pub last_updated: chrono::DateTime<Utc>,
    pub created_by: String,
    pub version: u32,
}

/// Feature flag types
#[derive(Debug, Clone, Serialize, Deserialize, PartialEq)]
pub enum FeatureFlagType {
    Boolean,
    String,
    Number,
    Json,
}

/// Feature condition
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct FeatureCondition {
    pub condition_type: ConditionType,
    pub field: String,
    pub operator: ConditionOperator,
    pub value: serde_json::Value,
    pub description: String,
}

/// Condition types
#[derive(Debug, Clone, Serialize, Deserialize, PartialEq)]
pub enum ConditionType {
    User,
    Environment,
    Time,
    Custom,
}

/// Condition operators
#[derive(Debug, Clone, Serialize, Deserialize, PartialEq)]
pub enum ConditionOperator {
    Equals,
    NotEquals,
    GreaterThan,
    LessThan,
    Contains,
    StartsWith,
    EndsWith,
    In,
    NotIn,
}

/// Configuration change record
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct ConfigurationChange {
    pub id: Uuid,
    pub key: String,
    pub old_value: Option<serde_json::Value>,
    pub new_value: serde_json::Value,
    pub change_type: ChangeType,
    pub timestamp: chrono::DateTime<Utc>,
    pub changed_by: String,
    pub reason: String,
    pub rollback_id: Option<Uuid>,
}

/// Change types
#[derive(Debug, Clone, Serialize, Deserialize, PartialEq)]
pub enum ChangeType {
    Create,
    Update,
    Delete,
    Rollback,
}

/// Configuration Agent implementation
pub struct ConfigurationAgent {
    id: AgentId,
    configurations: RwLock<HashMap<String, ConfigurationEntry>>,
    feature_flags: RwLock<HashMap<Uuid, FeatureFlag>>,
    change_history: RwLock<Vec<ConfigurationChange>>,
    configuration_schemas: HashMap<String, ConfigurationSchema>,
}

/// Configuration schema
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct ConfigurationSchema {
    pub name: String,
    pub version: String,
    pub fields: Vec<ConfigurationField>,
    pub required_fields: Vec<String>,
    pub validation_rules: Vec<ValidationRule>,
    pub last_updated: chrono::DateTime<Utc>,
}

/// Configuration field
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct ConfigurationField {
    pub name: String,
    pub field_type: ConfigurationType,
    pub description: String,
    pub required: bool,
    pub default_value: Option<serde_json::Value>,
    pub validation_rules: Vec<ValidationRule>,
}

impl ConfigurationAgent {
    pub fn new() -> Self {
        let mut agent = Self {
            id: Uuid::new_v4(),
            configurations: RwLock::new(HashMap::new()),
            feature_flags: RwLock::new(HashMap::new()),
            change_history: RwLock::new(Vec::new()),
            configuration_schemas: HashMap::new(),
        };

        // Initialize default configurations
        agent.initialize_default_configurations();
        
        // Initialize default feature flags
        agent.initialize_default_feature_flags();
        
        // Initialize configuration schemas
        agent.initialize_configuration_schemas();

        agent
    }

    /// Initialize default configurations
    fn initialize_default_configurations(&mut self) {
        let mut configurations = self.configurations.write().unwrap();

        // Performance configurations
        configurations.insert("performance.max_concurrent_operations".to_string(), ConfigurationEntry {
            key: "performance.max_concurrent_operations".to_string(),
            value: serde_json::Value::Number(serde_json::Number::from(10)),
            data_type: ConfigurationType::Integer,
            description: "Maximum number of concurrent operations".to_string(),
            default_value: Some(serde_json::Value::Number(serde_json::Number::from(10))),
            validation_rules: vec![
                ValidationRule {
                    rule_type: ValidationRuleType::MinValue,
                    value: serde_json::Value::Number(serde_json::Number::from(1)),
                    message: "Must be at least 1".to_string(),
                },
                ValidationRule {
                    rule_type: ValidationRuleType::MaxValue,
                    value: serde_json::Value::Number(serde_json::Number::from(100)),
                    message: "Must be at most 100".to_string(),
                },
            ],
            last_updated: Utc::now(),
            updated_by: "system".to_string(),
            version: 1,
        });

        // Security configurations
        configurations.insert("security.rate_limit_requests_per_minute".to_string(), ConfigurationEntry {
            key: "security.rate_limit_requests_per_minute".to_string(),
            value: serde_json::Value::Number(serde_json::Number::from(100)),
            data_type: ConfigurationType::Integer,
            description: "Rate limit for requests per minute".to_string(),
            default_value: Some(serde_json::Value::Number(serde_json::Number::from(100))),
            validation_rules: vec![
                ValidationRule {
                    rule_type: ValidationRuleType::MinValue,
                    value: serde_json::Value::Number(serde_json::Number::from(1)),
                    message: "Must be at least 1".to_string(),
                },
            ],
            last_updated: Utc::now(),
            updated_by: "system".to_string(),
            version: 1,
        });

        // Logging configurations
        configurations.insert("logging.level".to_string(), ConfigurationEntry {
            key: "logging.level".to_string(),
            value: serde_json::Value::String("info".to_string()),
            data_type: ConfigurationType::String,
            description: "Logging level".to_string(),
            default_value: Some(serde_json::Value::String("info".to_string())),
            validation_rules: vec![
                ValidationRule {
                    rule_type: ValidationRuleType::Pattern,
                    value: serde_json::Value::String("^(trace|debug|info|warn|error)$".to_string()),
                    message: "Must be one of: trace, debug, info, warn, error".to_string(),
                },
            ],
            last_updated: Utc::now(),
            updated_by: "system".to_string(),
            version: 1,
        });
    }

    /// Initialize default feature flags
    fn initialize_default_feature_flags(&mut self) {
        let mut feature_flags = self.feature_flags.write().unwrap();

        // Performance monitoring feature flag
        feature_flags.insert(Uuid::new_v4(), FeatureFlag {
            id: Uuid::new_v4(),
            name: "performance_monitoring".to_string(),
            description: "Enable performance monitoring and metrics collection".to_string(),
            flag_type: FeatureFlagType::Boolean,
            enabled: true,
            rollout_percentage: 100.0,
            target_users: vec![],
            target_groups: vec![],
            conditions: vec![],
            created_at: Utc::now(),
            last_updated: Utc::now(),
            created_by: "system".to_string(),
            version: 1,
        });

        // Advanced security features
        feature_flags.insert(Uuid::new_v4(), FeatureFlag {
            id: Uuid::new_v4(),
            name: "advanced_security".to_string(),
            description: "Enable advanced security features".to_string(),
            flag_type: FeatureFlagType::Boolean,
            enabled: false,
            rollout_percentage: 0.0,
            target_users: vec!["admin".to_string()],
            target_groups: vec!["beta_users".to_string()],
            conditions: vec![
                FeatureCondition {
                    condition_type: ConditionType::User,
                    field: "role".to_string(),
                    operator: ConditionOperator::Equals,
                    value: serde_json::Value::String("admin".to_string()),
                    description: "Admin users only".to_string(),
                },
            ],
            created_at: Utc::now(),
            last_updated: Utc::now(),
            created_by: "system".to_string(),
            version: 1,
        });
    }

    /// Initialize configuration schemas
    fn initialize_configuration_schemas(&mut self) {
        self.configuration_schemas.insert("performance".to_string(), ConfigurationSchema {
            name: "performance".to_string(),
            version: "1.0.0".to_string(),
            fields: vec![
                ConfigurationField {
                    name: "max_concurrent_operations".to_string(),
                    field_type: ConfigurationType::Integer,
                    description: "Maximum number of concurrent operations".to_string(),
                    required: true,
                    default_value: Some(serde_json::Value::Number(serde_json::Number::from(10))),
                    validation_rules: vec![
                        ValidationRule {
                            rule_type: ValidationRuleType::MinValue,
                            value: serde_json::Value::Number(serde_json::Number::from(1)),
                            message: "Must be at least 1".to_string(),
                        },
                    ],
                },
            ],
            required_fields: vec!["max_concurrent_operations".to_string()],
            validation_rules: vec![],
            last_updated: Utc::now(),
        });
    }

    /// Get configuration value
    pub fn get_configuration(&self, key: &str) -> Option<serde_json::Value> {
        let configurations = self.configurations.read().unwrap();
        configurations.get(key).map(|entry| entry.value.clone())
    }

    /// Set configuration value
    pub fn set_configuration(&self, key: String, value: serde_json::Value, updated_by: String, reason: String) -> Result<()> {
        let mut configurations = self.configurations.write().unwrap();
        let mut change_history = self.change_history.write().unwrap();

        // Get old value for change tracking
        let old_value = configurations.get(&key).map(|entry| entry.value.clone());

        // Validate the new value
        if let Some(entry) = configurations.get(&key) {
            for rule in &entry.validation_rules {
                self.validate_configuration_value(&value, rule)?;
            }
        }

        // Create new configuration entry
        let new_entry = ConfigurationEntry {
            key: key.clone(),
            value: value.clone(),
            data_type: self.infer_configuration_type(&value),
            description: "Updated configuration".to_string(),
            default_value: None,
            validation_rules: vec![],
            last_updated: Utc::now(),
            updated_by: updated_by.clone(),
            version: configurations.get(&key).map(|e| e.version + 1).unwrap_or(1),
        };

        // Record the change
        let change = ConfigurationChange {
            id: Uuid::new_v4(),
            key: key.clone(),
            old_value,
            new_value: value,
            change_type: if old_value.is_some() { ChangeType::Update } else { ChangeType::Create },
            timestamp: Utc::now(),
            changed_by: updated_by,
            reason,
            rollback_id: None,
        };

        configurations.insert(key, new_entry);
        change_history.push(change);

        // Keep only last 10000 changes
        if change_history.len() > 10000 {
            change_history.remove(0);
        }

        Ok(())
    }

    /// Validate configuration value against rule
    fn validate_configuration_value(&self, value: &serde_json::Value, rule: &ValidationRule) -> Result<()> {
        match rule.rule_type {
            ValidationRuleType::MinValue => {
                if let Some(number_value) = value.as_f64() {
                    let min_value = rule.value.as_f64().unwrap_or(f64::NEG_INFINITY);
                    if number_value < min_value {
                        return Err(GgenMcpError::InvalidParameter(rule.message.clone()));
                    }
                }
            }
            ValidationRuleType::MaxValue => {
                if let Some(number_value) = value.as_f64() {
                    let max_value = rule.value.as_f64().unwrap_or(f64::INFINITY);
                    if number_value > max_value {
                        return Err(GgenMcpError::InvalidParameter(rule.message.clone()));
                    }
                }
            }
            ValidationRuleType::MinLength => {
                if let Some(string_value) = value.as_str() {
                    let min_length = rule.value.as_i64().unwrap_or(0);
                    if string_value.len() < min_length as usize {
                        return Err(GgenMcpError::InvalidParameter(rule.message.clone()));
                    }
                }
            }
            ValidationRuleType::MaxLength => {
                if let Some(string_value) = value.as_str() {
                    let max_length = rule.value.as_i64().unwrap_or(i64::MAX);
                    if string_value.len() > max_length as usize {
                        return Err(GgenMcpError::InvalidParameter(rule.message.clone()));
                    }
                }
            }
            ValidationRuleType::Pattern => {
                if let Some(string_value) = value.as_str() {
                    if let Some(pattern_str) = rule.value.as_str() {
                        if let Ok(pattern) = regex::Regex::new(pattern_str) {
                            if !pattern.is_match(string_value) {
                                return Err(GgenMcpError::InvalidParameter(rule.message.clone()));
                            }
                        }
                    }
                }
            }
            ValidationRuleType::Required => {
                if value.is_null() {
                    return Err(GgenMcpError::InvalidParameter(rule.message.clone()));
                }
            }
            ValidationRuleType::Custom => {
                // Custom validation would be implemented here
            }
        }
        Ok(())
    }

    /// Infer configuration type from value
    fn infer_configuration_type(&self, value: &serde_json::Value) -> ConfigurationType {
        match value {
            serde_json::Value::String(_) => ConfigurationType::String,
            serde_json::Value::Number(n) => {
                if n.is_i64() {
                    ConfigurationType::Integer
                } else {
                    ConfigurationType::Float
                }
            }
            serde_json::Value::Bool(_) => ConfigurationType::Boolean,
            serde_json::Value::Array(_) => ConfigurationType::Array,
            serde_json::Value::Object(_) => ConfigurationType::Object,
            serde_json::Value::Null => ConfigurationType::String, // Default to string
        }
    }

    /// Check if feature flag is enabled
    pub fn is_feature_enabled(&self, flag_name: &str, user_id: Option<&str>, user_groups: Option<&[String]>) -> bool {
        let feature_flags = self.feature_flags.read().unwrap();
        
        for flag in feature_flags.values() {
            if flag.name == flag_name {
                if !flag.enabled {
                    return false;
                }

                // Check rollout percentage
                if flag.rollout_percentage < 100.0 {
                    // Simulate percentage rollout
                    use std::collections::hash_map::DefaultHasher;
                    use std::hash::{Hash, Hasher};
                    
                    let mut hasher = DefaultHasher::new();
                    flag_name.hash(&mut hasher);
                    if let Some(user_id) = user_id {
                        user_id.hash(&mut hasher);
                    }
                    let hash = hasher.finish();
                    let percentage = (hash % 100) as f64;
                    
                    if percentage > flag.rollout_percentage {
                        return false;
                    }
                }

                // Check target users
                if let Some(user_id) = user_id {
                    if flag.target_users.contains(&user_id.to_string()) {
                        return true;
                    }
                }

                // Check target groups
                if let Some(user_groups) = user_groups {
                    for group in user_groups {
                        if flag.target_groups.contains(&group.to_string()) {
                            return true;
                        }
                    }
                }

                // Check conditions
                for condition in &flag.conditions {
                    if self.evaluate_feature_condition(condition, user_id, user_groups) {
                        return true;
                    }
                }

                return flag.rollout_percentage == 100.0;
            }
        }

        false
    }

    /// Evaluate feature condition
    fn evaluate_feature_condition(&self, condition: &FeatureCondition, user_id: Option<&str>, user_groups: Option<&[String]>) -> bool {
        match condition.condition_type {
            ConditionType::User => {
                if let Some(user_id) = user_id {
                    match condition.operator {
                        ConditionOperator::Equals => {
                            user_id == condition.value.as_str().unwrap_or("")
                        }
                        ConditionOperator::NotEquals => {
                            user_id != condition.value.as_str().unwrap_or("")
                        }
                        _ => false,
                    }
                } else {
                    false
                }
            }
            ConditionType::Environment => {
                // Environment-based conditions would be implemented here
                false
            }
            ConditionType::Time => {
                // Time-based conditions would be implemented here
                false
            }
            ConditionType::Custom => {
                // Custom conditions would be implemented here
                false
            }
        }
    }

    /// Get all configurations
    pub fn get_all_configurations(&self) -> HashMap<String, serde_json::Value> {
        let configurations = self.configurations.read().unwrap();
        configurations.iter()
            .map(|(key, entry)| (key.clone(), entry.value.clone()))
            .collect()
    }

    /// Get all feature flags
    pub fn get_all_feature_flags(&self) -> Vec<FeatureFlag> {
        let feature_flags = self.feature_flags.read().unwrap();
        feature_flags.values().cloned().collect()
    }

    /// Get configuration change history
    pub fn get_change_history(&self) -> Vec<ConfigurationChange> {
        let change_history = self.change_history.read().unwrap();
        change_history.clone()
    }
}

#[async_trait::async_trait]
impl Agent for ConfigurationAgent {
    async fn initialize(&mut self) -> Result<(), Box<dyn std::error::Error>> {
        tracing::info!("Configuration Agent initialized with ID: {}", self.id);
        tracing::info!("Loaded {} configurations", self.configurations.read().unwrap().len());
        tracing::info!("Loaded {} feature flags", self.feature_flags.read().unwrap().len());
        Ok(())
    }

    async fn execute(&self, input: serde_json::Value) -> Result<serde_json::Value, Box<dyn std::error::Error>> {
        let operation = input.get("operation")
            .and_then(|v| v.as_str())
            .ok_or("Missing operation")?;

        let agent = ConfigurationAgent::new();
        
        let result = match operation {
            "get_configuration" => {
                let key = input.get("key")
                    .and_then(|v| v.as_str())
                    .ok_or("Missing key")?;
                serde_json::to_value(agent.get_configuration(key))?
            }
            "set_configuration" => {
                let key = input.get("key")
                    .and_then(|v| v.as_str())
                    .ok_or("Missing key")?;
                let value = input.get("value")
                    .ok_or("Missing value")?;
                let updated_by = input.get("updated_by")
                    .and_then(|v| v.as_str())
                    .unwrap_or("system");
                let reason = input.get("reason")
                    .and_then(|v| v.as_str())
                    .unwrap_or("Configuration update");
                
                agent.set_configuration(key.to_string(), value.clone(), updated_by.to_string(), reason.to_string())?;
                serde_json::json!({"success": true})
            }
            "is_feature_enabled" => {
                let flag_name = input.get("flag_name")
                    .and_then(|v| v.as_str())
                    .ok_or("Missing flag_name")?;
                let user_id = input.get("user_id")
                    .and_then(|v| v.as_str());
                let user_groups = input.get("user_groups")
                    .and_then(|v| v.as_array())
                    .map(|arr| arr.iter().filter_map(|v| v.as_str().map(|s| s.to_string())).collect::<Vec<_>>());
                
                let enabled = agent.is_feature_enabled(flag_name, user_id, user_groups.as_deref());
                serde_json::json!({"enabled": enabled})
            }
            "get_all_configurations" => {
                serde_json::to_value(agent.get_all_configurations())?
            }
            "get_all_feature_flags" => {
                serde_json::to_value(agent.get_all_feature_flags())?
            }
            _ => return Err("Unknown operation".into()),
        };

        Ok(result)
    }

    fn metadata(&self) -> AgentMetadata {
        AgentMetadata {
            id: self.id,
            name: "ConfigurationAgent".to_string(),
            version: "1.0.0".to_string(),
            status: AgentStatus::Healthy,
            capabilities: vec![
                "dynamic_configuration".to_string(),
                "feature_flags".to_string(),
                "configuration_validation".to_string(),
                "hot_reloading".to_string(),
            ],
            last_heartbeat: Utc::now(),
        }
    }

    async fn health_check(&self) -> AgentStatus {
        // Configuration agent is always healthy unless explicitly failed
        AgentStatus::Healthy
    }

    async fn shutdown(&mut self) -> Result<(), Box<dyn std::error::Error>> {
        tracing::info!("Configuration Agent shutting down");
        tracing::info!("Managed {} configurations", self.configurations.read().unwrap().len());
        tracing::info!("Managed {} feature flags", self.feature_flags.read().unwrap().len());
        Ok(())
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use serde_json::json;

    #[test]
    fn test_configuration_agent_creation() {
        let agent = ConfigurationAgent::new();
        
        assert!(!agent.configurations.read().unwrap().is_empty());
        assert!(!agent.feature_flags.read().unwrap().is_empty());
    }

    #[test]
    fn test_configuration_get_set() {
        let agent = ConfigurationAgent::new();
        
        // Get existing configuration
        let value = agent.get_configuration("performance.max_concurrent_operations");
        assert!(value.is_some());
        assert_eq!(value.unwrap(), json!(10));
        
        // Set new configuration
        let result = agent.set_configuration(
            "test.config".to_string(),
            json!("test_value"),
            "test_user".to_string(),
            "Test configuration".to_string()
        );
        assert!(result.is_ok());
        
        // Verify the new configuration
        let new_value = agent.get_configuration("test.config");
        assert_eq!(new_value, Some(json!("test_value")));
    }

    #[test]
    fn test_configuration_validation() {
        let agent = ConfigurationAgent::new();
        
        // Try to set invalid value (negative number)
        let result = agent.set_configuration(
            "performance.max_concurrent_operations".to_string(),
            json!(-1),
            "test_user".to_string(),
            "Test validation".to_string()
        );
        assert!(result.is_err());
    }

    #[test]
    fn test_feature_flag_evaluation() {
        let agent = ConfigurationAgent::new();
        
        // Check enabled feature flag
        let enabled = agent.is_feature_enabled("performance_monitoring", None, None);
        assert!(enabled);
        
        // Check disabled feature flag
        let disabled = agent.is_feature_enabled("advanced_security", None, None);
        assert!(!disabled);
        
        // Check feature flag for admin user
        let admin_enabled = agent.is_feature_enabled("advanced_security", Some("admin"), None);
        assert!(admin_enabled);
    }

    #[test]
    fn test_change_history() {
        let agent = ConfigurationAgent::new();
        
        // Set a configuration
        agent.set_configuration(
            "test.history".to_string(),
            json!("test"),
            "test_user".to_string(),
            "Test history".to_string()
        ).unwrap();
        
        // Check change history
        let history = agent.get_change_history();
        assert!(!history.is_empty());
        assert_eq!(history[0].key, "test.history");
        assert_eq!(history[0].changed_by, "test_user");
    }

    #[tokio::test]
    async fn test_agent_execution() {
        let mut agent = ConfigurationAgent::new();
        agent.initialize().await.unwrap();
        
        let input = json!({
            "operation": "get_configuration",
            "key": "performance.max_concurrent_operations"
        });
        
        let result = agent.execute(input).await.unwrap();
        assert_eq!(result, json!(10));
    }
}
