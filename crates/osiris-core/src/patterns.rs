//! Life Pattern Management
//!
//! Manages and executes life patterns for autonomic life management

use crate::{OSIRISSignal, SignalLevel};
use serde_json::{json, Value};
use std::collections::HashMap;
use std::sync::Arc;
use tokio::sync::RwLock;
use tracing::{debug, info, warn};

/// Represents a life pattern in the OSIRIS system
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct LifePattern {
    /// Pattern identifier
    pub id: String,
    /// Pattern name
    pub name: String,
    /// Pattern description
    pub description: Option<String>,
    /// Pattern category
    pub category: PatternCategory,
    /// Pattern version
    pub version: String,
    /// Pattern configuration
    pub config: Value,
    /// Pattern status
    pub status: PatternStatus,
    /// Execution statistics
    pub stats: PatternStats,
    /// Creation timestamp
    pub created_at: chrono::DateTime<chrono::Utc>,
    /// Last execution timestamp
    pub last_executed: Option<chrono::DateTime<chrono::Utc>>,
}

/// Pattern categories
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum PatternCategory {
    /// Health monitoring patterns
    Health,
    /// Process optimization patterns
    Optimization,
    /// Quality assurance patterns
    Quality,
    /// Safety patterns
    Safety,
    /// Efficiency patterns
    Efficiency,
    /// Innovation patterns
    Innovation,
    /// Custom patterns
    Custom,
}

/// Pattern execution status
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum PatternStatus {
    /// Pattern is active and ready to execute
    Active,
    /// Pattern is disabled
    Disabled,
    /// Pattern is under development
    Development,
    /// Pattern is deprecated
    Deprecated,
    /// Pattern is retired
    Retired,
}

/// Pattern execution statistics
#[derive(Debug, Clone, PartialEq, Eq, Default)]
pub struct PatternStats {
    /// Number of successful executions
    pub success_count: u64,
    /// Number of failed executions
    pub failure_count: u64,
    /// Total execution time (nanoseconds)
    pub total_execution_time_ns: u64,
    /// Average execution time (nanoseconds)
    pub avg_execution_time_ns: u64,
    /// Last execution result
    pub last_result: Option<Value>,
    /// Last execution error
    pub last_error: Option<String>,
}

impl LifePattern {
    /// Create a new life pattern
    pub fn new(id: String, name: String) -> Self {
        Self {
            id,
            name,
            description: None,
            category: PatternCategory::Custom,
            version: "1.0.0".to_string(),
            config: json!({}),
            status: PatternStatus::Active,
            stats: PatternStats::default(),
            created_at: chrono::Utc::now(),
            last_executed: None,
        }
    }

    /// Create a new life pattern with all fields
    pub fn with_fields(
        id: String,
        name: String,
        description: String,
        category: PatternCategory,
        version: String,
        config: Value,
    ) -> Self {
        Self {
            id,
            name,
            description: Some(description),
            category,
            version,
            config,
            status: PatternStatus::Active,
            stats: PatternStats::default(),
            created_at: chrono::Utc::now(),
            last_executed: None,
        }
    }

    /// Update pattern status
    pub fn update_status(&mut self, status: PatternStatus) {
        self.status = status;
    }

    /// Update pattern configuration
    pub fn update_config(&mut self, config: Value) {
        self.config = config;
    }

    /// Execute the pattern
    pub async fn execute(&self, input: Value) -> Result<Value, String> {
        if self.status != PatternStatus::Active {
            return Err(format!("Pattern {} is not active", self.id));
        }

        // Simulate pattern execution
        let start_time = std::time::Instant::now();
        let result = self.execute_pattern_logic(input).await;
        let execution_time = start_time.elapsed();

        match result {
            Ok(output) => {
                // Update statistics
                self.stats.success_count += 1;
                self.stats.total_execution_time_ns += execution_time.as_nanos() as u64;
                self.stats.avg_execution_time_ns = self.stats.total_execution_time_ns / self.stats.success_count;
                self.stats.last_result = Some(output.clone());
                self.stats.last_error = None;
                self.last_executed = Some(chrono::Utc::now());

                Ok(output)
            }
            Err(error) => {
                // Update failure statistics
                self.stats.failure_count += 1;
                self.stats.last_error = Some(error.clone());
                self.last_executed = Some(chrono::Utc::now());

                Err(error)
            }
        }
    }

    /// Pattern-specific execution logic (to be implemented by pattern)
    async fn execute_pattern_logic(&self, input: Value) -> Result<Value, String> {
        // This is a placeholder - real patterns would implement their logic here
        debug!("Executing pattern {} with input: {}", self.id, input);

        match self.category {
            PatternCategory::Health => {
                // Health pattern logic
                Ok(json!({
                    "status": "health_check_completed",
                    "pattern": self.id,
                    "input": input,
                    "result": {
                        "overall_health": "good",
                        "metrics": {
                            "heart_rate": 72,
                            "blood_pressure": "120/80",
                            "energy_level": "high"
                        }
                    }
                }))
            }
            PatternCategory::Optimization => {
                // Optimization pattern logic
                Ok(json!({
                    "status": "optimization_completed",
                    "pattern": self.id,
                    "input": input,
                    "result": {
                        "optimizations_applied": 3,
                        "efficiency_improvement": "15%",
                        "cost_savings": 1500
                    }
                }))
            }
            PatternCategory::Quality => {
                // Quality pattern logic
                Ok(json!({
                    "status": "quality_check_completed",
                    "pattern": self.id,
                    "input": input,
                    "result": {
                        "quality_score": 95,
                        "defects_found": 2,
                        "recommendations": ["Improve test coverage", "Add validation"]
                    }
                }))
            }
            PatternCategory::Safety => {
                // Safety pattern logic
                Ok(json!({
                    "status": "safety_check_completed",
                    "pattern": self.id,
                    "input": input,
                    "result": {
                        "safety_rating": "A",
                        "hazards_identified": 1,
                        "mitigation_plan": "Add guardrails"
                    }
                }))
            }
            PatternCategory::Efficiency => {
                // Efficiency pattern logic
                Ok(json!({
                    "status": "efficiency_analysis_completed",
                    "pattern": self.id,
                    "input": input,
                    "result": {
                        "current_efficiency": 78,
                        "potential_efficiency": 92,
                        "improvement_actions": [
                            "Automate manual processes",
                            "Optimize resource allocation"
                        ]
                    }
                }))
            }
            PatternCategory::Innovation => {
                // Innovation pattern logic
                Ok(json!({
                    "status": "innovation_session_completed",
                    "pattern": self.id,
                    "input": input,
                    "result": {
                        "ideas_generated": 5,
                        "feasible_ideas": 2,
                        "implementation_plan": "Q1 2026"
                    }
                }))
            }
            PatternCategory::Custom => {
                // Custom pattern logic
                Ok(json!({
                    "status": "custom_pattern_completed",
                    "pattern": self.id,
                    "input": input,
                    "result": {
                        "output": "Custom pattern executed successfully",
                        "custom_data": input
                    }
                }))
            }
        }
    }

    /// Get pattern success rate
    pub fn success_rate(&self) -> f64 {
        let total = self.stats.success_count + self.stats.failure_count;
        if total == 0 {
            0.0
        } else {
            self.stats.success_count as f64 / total as f64
        }
    }

    /// Get average execution time in milliseconds
    pub fn avg_execution_time_ms(&self) -> f64 {
        self.stats.avg_execution_time_ns as f64 / 1_000_000.0
    }

    /// Check if pattern is healthy
    pub fn is_healthy(&self) -> bool {
        self.status == PatternStatus::Active && self.success_rate() >= 0.8
    }
}

/// Pattern registry for managing all patterns
pub struct PatternRegistry {
    patterns: Arc<RwLock<HashMap<String, LifePattern>>>,
}

impl PatternRegistry {
    /// Create a new pattern registry
    pub fn new() -> Self {
        Self {
            patterns: Arc::new(RwLock::new(HashMap::new())),
        }
    }

    /// Register a pattern
    pub async fn register(&mut self, pattern: LifePattern) -> Result<(), String> {
        let mut patterns = self.patterns.write().await;

        if patterns.contains_key(&pattern.id) {
            return Err(format!("Pattern {} already exists", pattern.id));
        }

        patterns.insert(pattern.id.clone(), pattern);

        info!("Pattern registered: {}", pattern.id);

        // Emit pattern registration signal
        let signal = OSIRISSignal::new(
            "pattern_registered",
            format!("Pattern {} registered successfully", pattern.id),
            SignalLevel::Info,
        );

        // Signal would be emitted here in full implementation
        debug!("Pattern registration signal: {}", signal.message);

        Ok(())
    }

    /// Unregister a pattern
    pub async fn unregister(&mut self, pattern_id: &str) -> Result<(), String> {
        let mut patterns = self.patterns.write().await;

        if patterns.remove(pattern_id).is_none() {
            return Err(format!("Pattern {} not found", pattern_id));
        }

        info!("Pattern unregistered: {}", pattern_id);

        // Emit pattern unregistration signal
        let signal = OSIRISSignal::new(
            "pattern_unregistered",
            format!("Pattern {} unregistered", pattern_id),
            SignalLevel::Info,
        );

        // Signal would be emitted here in full implementation
        debug!("Pattern unregistration signal: {}", signal.message);

        Ok(())
    }

    /// Get a pattern by ID
    pub async fn get(&self, pattern_id: &str) -> Option<LifePattern> {
        let patterns = self.patterns.read().await;
        patterns.get(pattern_id).cloned()
    }

    /// Get all patterns
    pub async fn list(&self) -> Vec<LifePattern> {
        let patterns = self.patterns.read().await;
        patterns.values().cloned().collect()
    }

    /// Get patterns by category
    pub async fn get_by_category(&self, category: PatternCategory) -> Vec<LifePattern> {
        let patterns = self.patterns.read().await;
        patterns
            .values()
            .filter(|pattern| pattern.category == category)
            .cloned()
            .collect()
    }

    /// Get active patterns
    pub async fn get_active_patterns(&self) -> Vec<LifePattern> {
        let patterns = self.patterns.read().await;
        patterns
            .values()
            .filter(|pattern| pattern.status == PatternStatus::Active)
            .cloned()
            .collect()
    }

    /// Get pattern count
    pub async fn count(&self) -> usize {
        let patterns = self.patterns.read().await;
        patterns.len()
    }

    /// Execute a pattern by ID
    pub async fn execute_pattern(&self, pattern_id: &str, input: Value) -> Result<Value, String> {
        let pattern = self.get(pattern_id).await
            .ok_or_else(|| format!("Pattern {} not found", pattern_id))?;

        pattern.execute(input).await
    }

    /// Update pattern status
    pub async fn update_status(&mut self, pattern_id: &str, status: PatternStatus) -> Result<(), String> {
        let mut patterns = self.patterns.write().await;

        if let Some(pattern) = patterns.get_mut(pattern_id) {
            pattern.update_status(status);

            // Emit pattern status change signal
            let signal = OSIRISSignal::new(
                "pattern_status_changed",
                format!("Pattern {} status changed to {:?}", pattern_id, status),
                SignalLevel::Info,
            );

            // Signal would be emitted here in full implementation
            debug!("Pattern status change signal: {}", signal.message);

            Ok(())
        } else {
            Err(format!("Pattern {} not found", pattern_id))
        }
    }

    /// Get patterns that need attention (low success rate)
    pub async fn get_needing_attention(&self) -> Vec<LifePattern> {
        let patterns = self.patterns.read().await;
        patterns
            .values()
            .filter(|pattern| pattern.is_healthy() && pattern.success_rate() < 0.8)
            .cloned()
            .collect()
    }

    /// Clear all patterns (for testing/reset)
    pub async fn clear(&mut self) {
        let mut patterns = self.patterns.write().await;
        patterns.clear();
        info!("All patterns cleared");
    }
}

/// Pattern execution context for pattern execution
pub struct PatternExecution {
    pattern: LifePattern,
    input: Value,
}

impl PatternExecution {
    /// Create a new pattern execution
    pub fn new(pattern: LifePattern, input: Value) -> Self {
        Self { pattern, input }
    }

    /// Execute the pattern
    pub async fn execute(self) -> Result<Value, String> {
        self.pattern.execute(self.input).await
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use chrono::Utc;

    #[test]
    fn test_life_pattern_creation() {
        let pattern = LifePattern::new("health-check".to_string(), "Health Check".to_string());
        assert_eq!(pattern.id, "health-check");
        assert_eq!(pattern.name, "Health Check");
        assert_eq!(pattern.category, PatternCategory::Custom);
        assert_eq!(pattern.status, PatternStatus::Active);
        assert_eq!(pattern.stats.success_count, 0);
        assert_eq!(pattern.stats.failure_count, 0);
    }

    #[test]
    fn test_life_pattern_with_fields() {
        let pattern = LifePattern::with_fields(
            "optimization".to_string(),
            "Process Optimization".to_string(),
            "Optimize business processes".to_string(),
            PatternCategory::Optimization,
            "1.1.0".to_string(),
            json!({"timeout_ms": 5000}),
        );

        assert_eq!(pattern.id, "optimization");
        assert_eq!(pattern.name, "Process Optimization");
        assert_eq!(pattern.category, PatternCategory::Optimization);
        assert_eq!(pattern.version, "1.1.0");
        assert_eq!(pattern.config, json!({"timeout_ms": 5000}));
    }

    #[tokio::test]
    async fn test_pattern_execution_success() {
        let pattern = LifePattern::new("test-pattern".to_string(), "Test Pattern".to_string());

        let input = json!({"test": "input"});
        let result = pattern.execute(input).await;

        assert!(result.is_ok());
        let output = result.unwrap();
        assert_eq!(output["status"], "custom_pattern_completed");
        assert_eq!(output["pattern"], "test-pattern");

        // Check statistics
        assert_eq!(pattern.stats.success_count, 1);
        assert_eq!(pattern.stats.failure_count, 0);
        assert!(pattern.last_executed.is_some());
    }

    #[tokio::test]
    async fn test_pattern_execution_failure() {
        let pattern = LifePattern::new("test-pattern".to_string(), "Test Pattern".to_string());

        // Execute a pattern that will fail (disabled pattern)
        pattern.update_status(PatternStatus::Disabled);

        let input = json!({"test": "input"});
        let result = pattern.execute(input).await;

        assert!(result.is_err());
        assert_eq!(pattern.stats.success_count, 0);
        assert_eq!(pattern.stats.failure_count, 1);
    }

    #[test]
    fn test_pattern_success_rate() {
        let mut pattern = LifePattern::new("test".to_string(), "Test".to_string());

        // Initially no executions
        assert_eq!(pattern.success_rate(), 0.0);

        // Add some successes and failures
        pattern.stats.success_count = 8;
        pattern.stats.failure_count = 2;
        assert_eq!(pattern.success_rate(), 0.8);
    }

    #[test]
    fn test_pattern_health_check() {
        let mut pattern = LifePattern::new("test".to_string(), "Test".to_string());

        // Active pattern with no executions is healthy
        assert!(pattern.is_healthy());

        // Pattern with low success rate is not healthy
        pattern.stats.success_count = 5;
        pattern.stats.failure_count = 5;
        assert!(!pattern.is_healthy());
    }

    #[tokio::test]
    async fn test_pattern_registry_operations() {
        let mut registry = PatternRegistry::new();

        let pattern = LifePattern::new("test-pattern".to_string(), "Test Pattern".to_string());
        assert!(registry.register(pattern).await.is_ok());

        // Check pattern exists
        assert_eq!(registry.count().await, 1);

        // Get pattern
        let retrieved = registry.get("test-pattern").await;
        assert!(retrieved.is_some());

        // Execute pattern
        let input = json!({"test": "input"});
        let result = registry.execute_pattern("test-pattern", input).await;
        assert!(result.is_ok());

        // Unregister pattern
        assert!(registry.unregister("test-pattern").await.is_ok());
        assert_eq!(registry.count().await, 0);
    }

    #[tokio::test]
    async fn test_pattern_registry_categories() {
        let mut registry = PatternRegistry::new();

        let health_pattern = LifePattern::with_fields(
            "health-1".to_string(),
            "Health Pattern 1".to_string(),
            "Description".to_string(),
            PatternCategory::Health,
            "1.0.0".to_string(),
            json!({}),
        );

        let optimization_pattern = LifePattern::with_fields(
            "optimization-1".to_string(),
            "Optimization Pattern 1".to_string(),
            "Description".to_string(),
            PatternCategory::Optimization,
            "1.0.0".to_string(),
            json!({}),
        );

        registry.register(health_pattern).await.unwrap();
        registry.register(optimization_pattern).await.unwrap();

        let health_patterns = registry.get_by_category(PatternCategory::Health).await;
        assert_eq!(health_patterns.len(), 1);
        assert_eq!(health_patterns[0].id, "health-1");

        let optimization_patterns = registry.get_by_category(PatternCategory::Optimization).await;
        assert_eq!(optimization_patterns.len(), 1);
        assert_eq!(optimization_patterns[0].id, "optimization-1");
    }
}