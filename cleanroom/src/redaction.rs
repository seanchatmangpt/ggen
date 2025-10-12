//! Sensitive data redaction for cleanroom testing
//!
//! This module provides sensitive data redaction following core team best practices:
//! - Pattern-based redaction
//! - Configurable redaction rules
//! - Redaction validation
//! - Redaction reporting

use crate::error::{Result, CleanroomError};
use regex::Regex;
use serde::{Deserialize, Serialize};
use std::collections::HashMap;
use std::sync::Arc;
use std::time::Instant;
use tokio::sync::RwLock;
use uuid::Uuid;

/// Redaction manager for cleanroom testing
#[derive(Debug)]
pub struct RedactionManager {
    /// Session ID
    session_id: Uuid,
    /// Redaction rules
    redaction_rules: Arc<RwLock<Vec<RedactionRule>>>,
    /// Redaction data
    redaction_data: Arc<RwLock<RedactionData>>,
    /// Enabled flag
    enabled: bool,
}

/// Redaction rule
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct RedactionRule {
    /// Rule name
    pub name: String,
    /// Rule description
    pub description: String,
    /// Pattern to match
    pub pattern: String,
    /// Compiled regex
    #[serde(skip)]
    pub compiled_regex: Option<Regex>,
    /// Replacement text
    pub replacement: String,
    /// Rule priority
    pub priority: u32,
    /// Rule enabled
    pub enabled: bool,
}

/// Redaction data structure
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct RedactionData {
    /// Session ID
    pub session_id: Uuid,
    /// Start time
    pub start_time: Instant,
    /// End time
    pub end_time: Option<Instant>,
    /// Redaction operations
    pub redaction_operations: Vec<RedactionOperation>,
    /// Redaction statistics
    pub statistics: RedactionStatistics,
}

/// Redaction operation
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct RedactionOperation {
    /// Operation ID
    pub operation_id: Uuid,
    /// Rule name
    pub rule_name: String,
    /// Original content
    pub original_content: String,
    /// Redacted content
    pub redacted_content: String,
    /// Matches found
    pub matches_found: u32,
    /// Operation timestamp
    pub timestamp: Instant,
    /// Operation duration
    pub duration_ms: u64,
}

/// Redaction statistics
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct RedactionStatistics {
    /// Total operations
    pub total_operations: u32,
    /// Total matches
    pub total_matches: u32,
    /// Total bytes processed
    pub total_bytes_processed: u64,
    /// Average operation duration
    pub average_operation_duration_ms: f64,
    /// Rules applied
    pub rules_applied: HashMap<String, u32>,
}

impl RedactionManager {
    /// Create a new redaction manager
    pub fn new(redaction_patterns: Vec<String>) -> Self {
        let session_id = Uuid::new_v4();
        let mut rules = Vec::new();
        
        // Create default rules from patterns
        for (index, pattern) in redaction_patterns.iter().enumerate() {
            let rule = RedactionRule {
                name: format!("pattern_{}", index),
                description: format!("Redact pattern: {}", pattern),
                pattern: pattern.clone(),
                compiled_regex: Regex::new(pattern).ok(),
                replacement: "[REDACTED]".to_string(),
                priority: index as u32,
                enabled: true,
            };
            rules.push(rule);
        }
        
        Self {
            session_id,
            redaction_rules: Arc::new(RwLock::new(rules)),
            redaction_data: Arc::new(RwLock::new(RedactionData::new(session_id))),
            enabled: true,
        }
    }
    
    /// Create a disabled redaction manager
    pub fn disabled() -> Self {
        Self {
            session_id: Uuid::new_v4(),
            redaction_rules: Arc::new(RwLock::new(Vec::new())),
            redaction_data: Arc::new(RwLock::new(RedactionData::new(Uuid::new_v4()))),
            enabled: false,
        }
    }
    
    /// Check if redaction is enabled
    pub fn is_enabled(&self) -> bool {
        self.enabled
    }
    
    /// Add a redaction rule
    pub async fn add_rule(&self, rule: RedactionRule) -> Result<()> {
        if !self.enabled {
            return Ok(());
        }
        
        let mut rules = self.redaction_rules.write().await;
        
        // Compile regex
        let compiled_regex = Regex::new(&rule.pattern)
            .map_err(|e| CleanroomError::redaction_error(format!("Invalid regex pattern: {}", e)))?;
        
        let mut rule_with_regex = rule;
        rule_with_regex.compiled_regex = Some(compiled_regex);
        
        rules.push(rule_with_regex);
        
        // Sort by priority
        rules.sort_by_key(|r| r.priority);
        
        Ok(())
    }
    
    /// Remove a redaction rule
    pub async fn remove_rule(&self, rule_name: &str) -> Result<()> {
        if !self.enabled {
            return Ok(());
        }
        
        let mut rules = self.redaction_rules.write().await;
        rules.retain(|r| r.name != rule_name);
        Ok(())
    }
    
    /// Get all redaction rules
    pub async fn get_rules(&self) -> Result<Vec<RedactionRule>> {
        if !self.enabled {
            return Ok(Vec::new());
        }
        
        let rules = self.redaction_rules.read().await;
        Ok(rules.clone())
    }
    
    /// Redact sensitive data
    pub async fn redact(&self, content: &str) -> Result<String> {
        if !self.enabled {
            return Ok(content.to_string());
        }
        
        let start_time = Instant::now();
        let mut redacted_content = content.to_string();
        let mut total_matches = 0;
        let mut rules_applied = HashMap::new();
        
        let rules = self.redaction_rules.read().await;
        
        for rule in rules.iter().filter(|r| r.enabled) {
            if let Some(ref regex) = rule.compiled_regex {
                let matches = regex.find_iter(&redacted_content).count();
                if matches > 0 {
                    redacted_content = regex.replace_all(&redacted_content, &rule.replacement).to_string();
                    total_matches += matches as u32;
                    *rules_applied.entry(rule.name.clone()).or_insert(0) += matches as u32;
                }
            }
        }
        
        let duration = start_time.elapsed();
        
        // Record operation
        let operation = RedactionOperation {
            operation_id: Uuid::new_v4(),
            rule_name: "multiple_rules".to_string(),
            original_content: content.to_string(),
            redacted_content: redacted_content.clone(),
            matches_found: total_matches,
            timestamp: start_time,
            duration_ms: duration.as_millis() as u64,
        };
        
        let mut data = self.redaction_data.write().await;
        data.redaction_operations.push(operation);
        self.update_statistics(&mut data, rules_applied).await;
        
        Ok(redacted_content)
    }
    
    /// Redact sensitive data with specific rule
    pub async fn redact_with_rule(&self, content: &str, rule_name: &str) -> Result<String> {
        if !self.enabled {
            return Ok(content.to_string());
        }
        
        let start_time = Instant::now();
        let mut redacted_content = content.to_string();
        let mut matches_found = 0;
        
        let rules = self.redaction_rules.read().await;
        
        if let Some(rule) = rules.iter().find(|r| r.name == rule_name && r.enabled) {
            if let Some(ref regex) = rule.compiled_regex {
                matches_found = regex.find_iter(&redacted_content).count() as u32;
                redacted_content = regex.replace_all(&redacted_content, &rule.replacement).to_string();
            }
        }
        
        let duration = start_time.elapsed();
        
        // Record operation
        let operation = RedactionOperation {
            operation_id: Uuid::new_v4(),
            rule_name: rule_name.to_string(),
            original_content: content.to_string(),
            redacted_content: redacted_content.clone(),
            matches_found,
            timestamp: start_time,
            duration_ms: duration.as_millis() as u64,
        };
        
        let mut data = self.redaction_data.write().await;
        data.redaction_operations.push(operation);
        self.update_statistics(&mut data, HashMap::new()).await;
        
        Ok(redacted_content)
    }
    
    /// Check if content contains sensitive data
    pub async fn contains_sensitive_data(&self, content: &str) -> Result<bool> {
        if !self.enabled {
            return Ok(false);
        }
        
        let rules = self.redaction_rules.read().await;
        
        for rule in rules.iter().filter(|r| r.enabled) {
            if let Some(ref regex) = rule.compiled_regex {
                if regex.is_match(content) {
                    return Ok(true);
                }
            }
        }
        
        Ok(false)
    }
    
    /// Get redaction data
    pub async fn get_redaction_data(&self) -> RedactionData {
        let data = self.redaction_data.read().await;
        data.clone()
    }
    
    /// Get redaction operations
    pub async fn get_redaction_operations(&self) -> Result<Vec<RedactionOperation>> {
        if !self.enabled {
            return Ok(Vec::new());
        }
        
        let data = self.redaction_data.read().await;
        Ok(data.redaction_operations.clone())
    }
    
    /// Clear redaction operations
    pub async fn clear_operations(&self) -> Result<()> {
        if !self.enabled {
            return Ok(());
        }
        
        let mut data = self.redaction_data.write().await;
        data.redaction_operations.clear();
        self.update_statistics(&mut data, HashMap::new()).await;
        Ok(())
    }
    
    /// Generate redaction report
    pub async fn generate_redaction_report(&self) -> Result<RedactionReport> {
        if !self.enabled {
            return Err(CleanroomError::redaction_error("Redaction is disabled"));
        }
        
        let data = self.redaction_data.read().await;
        let rules = self.redaction_rules.read().await;
        
        let mut report = RedactionReport {
            session_id: data.session_id,
            start_time: data.start_time,
            end_time: data.end_time,
            statistics: data.statistics.clone(),
            rules: rules.clone(),
            operations: data.redaction_operations.clone(),
            recommendations: Vec::new(),
        };
        
        // Generate recommendations
        self.generate_recommendations(&data, &mut report).await;
        
        Ok(report)
    }
    
    /// Update statistics
    async fn update_statistics(&self, data: &mut RedactionData, rules_applied: HashMap<String, u32>) {
        let mut stats = RedactionStatistics {
            total_operations: 0,
            total_matches: 0,
            total_bytes_processed: 0,
            average_operation_duration_ms: 0.0,
            rules_applied: HashMap::new(),
        };
        
        for operation in &data.redaction_operations {
            stats.total_operations += 1;
            stats.total_matches += operation.matches_found;
            stats.total_bytes_processed += operation.original_content.len() as u64;
        }
        
        if stats.total_operations > 0 {
            let total_duration: u64 = data.redaction_operations.iter()
                .map(|op| op.duration_ms)
                .sum();
            stats.average_operation_duration_ms = total_duration as f64 / stats.total_operations as f64;
        }
        
        // Merge rules applied
        for (rule_name, count) in rules_applied {
            *stats.rules_applied.entry(rule_name).or_insert(0) += count;
        }
        
        data.statistics = stats;
    }
    
    /// Generate recommendations
    async fn generate_recommendations(&self, data: &RedactionData, report: &mut RedactionReport) {
        // High match count recommendation
        if data.statistics.total_matches > 100 {
            report.recommendations.push(format!(
                "{} sensitive data matches found, consider reviewing data handling practices",
                data.statistics.total_matches
            ));
        }
        
        // Long operation duration recommendation
        if data.statistics.average_operation_duration_ms > 100.0 {
            report.recommendations.push(format!(
                "Average redaction operation takes {:.1}ms, consider optimizing patterns",
                data.statistics.average_operation_duration_ms
            ));
        }
        
        // Large data processing recommendation
        if data.statistics.total_bytes_processed > 1024 * 1024 * 1024 { // 1GB
            report.recommendations.push(format!(
                "{} bytes of sensitive data processed, consider implementing data minimization",
                data.statistics.total_bytes_processed
            ));
        }
        
        // Unused rules recommendation
        let used_rules: std::collections::HashSet<String> = data.statistics.rules_applied.keys().cloned().collect();
        let all_rules: std::collections::HashSet<String> = report.rules.iter().map(|r| r.name.clone()).collect();
        let unused_rules: Vec<String> = all_rules.difference(&used_rules).cloned().collect();
        
        if !unused_rules.is_empty() {
            report.recommendations.push(format!(
                "{} redaction rules were not used, consider removing unused rules",
                unused_rules.len()
            ));
        }
    }
}

impl RedactionData {
    /// Create new redaction data
    pub fn new(session_id: Uuid) -> Self {
        Self {
            session_id,
            start_time: Instant::now(),
            end_time: None,
            redaction_operations: Vec::new(),
            statistics: RedactionStatistics {
                total_operations: 0,
                total_matches: 0,
                total_bytes_processed: 0,
                average_operation_duration_ms: 0.0,
                rules_applied: HashMap::new(),
            },
        }
    }
}

/// Redaction report
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct RedactionReport {
    /// Session ID
    pub session_id: Uuid,
    /// Start time
    pub start_time: Instant,
    /// End time
    pub end_time: Option<Instant>,
    /// Statistics
    pub statistics: RedactionStatistics,
    /// Rules
    pub rules: Vec<RedactionRule>,
    /// Operations
    pub operations: Vec<RedactionOperation>,
    /// Recommendations
    pub recommendations: Vec<String>,
}

#[cfg(test)]
mod tests {
    use super::*;
    
    #[tokio::test]
    async fn test_redaction_manager_creation() {
        let patterns = vec![
            r"password\s*=\s*[^\s]+".to_string(),
            r"token\s*=\s*[^\s]+".to_string(),
        ];
        let manager = RedactionManager::new(patterns);
        assert!(manager.is_enabled());
    }
    
    #[tokio::test]
    async fn test_redaction_manager_disabled() {
        let manager = RedactionManager::disabled();
        assert!(!manager.is_enabled());
    }
    
    #[tokio::test]
    async fn test_add_rule() {
        let manager = RedactionManager::new(vec![]);
        
        let rule = RedactionRule {
            name: "test_rule".to_string(),
            description: "Test rule".to_string(),
            pattern: r"test\s*=\s*[^\s]+".to_string(),
            compiled_regex: None,
            replacement: "[TEST]".to_string(),
            priority: 1,
            enabled: true,
        };
        
        manager.add_rule(rule).await.unwrap();
        
        let rules = manager.get_rules().await.unwrap();
        assert_eq!(rules.len(), 1);
        assert_eq!(rules[0].name, "test_rule");
    }
    
    #[tokio::test]
    async fn test_redact_content() {
        let patterns = vec![
            r"password\s*=\s*[^\s]+".to_string(),
            r"token\s*=\s*[^\s]+".to_string(),
        ];
        let manager = RedactionManager::new(patterns);
        
        let content = "password=secret123 token=abc456";
        let redacted = manager.redact(content).await.unwrap();
        
        assert!(redacted.contains("[REDACTED]"));
        assert!(!redacted.contains("secret123"));
        assert!(!redacted.contains("abc456"));
    }
    
    #[tokio::test]
    async fn test_redact_with_rule() {
        let manager = RedactionManager::new(vec![]);
        
        let rule = RedactionRule {
            name: "password_rule".to_string(),
            description: "Password rule".to_string(),
            pattern: r"password\s*=\s*[^\s]+".to_string(),
            compiled_regex: None,
            replacement: "[PASSWORD]".to_string(),
            priority: 1,
            enabled: true,
        };
        
        manager.add_rule(rule).await.unwrap();
        
        let content = "password=secret123";
        let redacted = manager.redact_with_rule(content, "password_rule").await.unwrap();
        
        assert_eq!(redacted, "[PASSWORD]");
    }
    
    #[tokio::test]
    async fn test_contains_sensitive_data() {
        let patterns = vec![
            r"password\s*=\s*[^\s]+".to_string(),
        ];
        let manager = RedactionManager::new(patterns);
        
        assert!(manager.contains_sensitive_data("password=secret123").await.unwrap());
        assert!(!manager.contains_sensitive_data("username=test").await.unwrap());
    }
    
    #[tokio::test]
    async fn test_redaction_operations() {
        let patterns = vec![
            r"password\s*=\s*[^\s]+".to_string(),
        ];
        let manager = RedactionManager::new(patterns);
        
        let content = "password=secret123";
        manager.redact(content).await.unwrap();
        
        let operations = manager.get_redaction_operations().await.unwrap();
        assert_eq!(operations.len(), 1);
        assert_eq!(operations[0].matches_found, 1);
    }
    
    #[tokio::test]
    async fn test_redaction_report() {
        let patterns = vec![
            r"password\s*=\s*[^\s]+".to_string(),
        ];
        let manager = RedactionManager::new(patterns);
        
        let content = "password=secret123";
        manager.redact(content).await.unwrap();
        
        let report = manager.generate_redaction_report().await.unwrap();
        assert_eq!(report.statistics.total_operations, 1);
        assert_eq!(report.statistics.total_matches, 1);
    }
}