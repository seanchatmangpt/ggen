//! Failure knowledge base for learning from past failures
//!
//! Provides a thread-safe knowledge base that records historical failures,
//! tracks recovery strategy effectiveness, and enables pattern-based failure prediction.

use serde::{Deserialize, Serialize};
use std::collections::HashMap;
use std::sync::{Arc, RwLock};
use std::time::{Duration, Instant};
use thiserror::Error;

/// Knowledge base error types
#[derive(Debug, Error)]
pub enum KnowledgeBaseError {
    /// Lock acquisition failed
    #[error("Failed to acquire lock: {0}")]
    LockError(String),

    /// Serialization failed
    #[error("Serialization failed: {0}")]
    SerializationError(String),

    /// Deserialization failed
    #[error("Deserialization failed: {0}")]
    DeserializationError(String),

    /// Query failed
    #[error("Query failed: {0}")]
    QueryError(String),

    /// Invalid configuration
    #[error("Invalid configuration: {0}")]
    InvalidConfig(String),
}

/// Result type for knowledge base operations
pub type Result<T> = std::result::Result<T, KnowledgeBaseError>;

/// Configuration for failure knowledge base
#[derive(Debug, Clone)]
pub struct KnowledgeBaseConfig {
    /// Maximum number of historical failures to retain
    pub max_history_size: usize,
    /// Minimum similarity threshold for querying similar failures (0.0-1.0)
    pub similarity_threshold: f64,
}

impl Default for KnowledgeBaseConfig {
    fn default() -> Self {
        KnowledgeBaseConfig {
            max_history_size: 10_000,
            similarity_threshold: 0.7,
        }
    }
}

/// Classification of failure types
#[derive(Debug, Clone, PartialEq, Eq, Hash, Serialize, Deserialize)]
pub enum FailureType {
    /// Container crash or termination
    ContainerFailure,
    /// Network connectivity issues
    NetworkFailure,
    /// Resource exhaustion (memory, CPU, disk)
    ResourceExhaustion,
    /// Service degradation
    ServiceDegradation,
    /// Data corruption or inconsistency
    DataCorruption,
    /// Configuration errors
    ConfigurationError,
    /// External dependency failure
    DependencyFailure,
    /// Unknown failure type
    Unknown,
}

/// Root cause analysis result
#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub enum RootCause {
    /// Memory leak or excessive allocation
    MemoryLeak { allocation_rate_mb_per_sec: f64 },
    /// CPU saturation
    CpuSaturation { avg_cpu_percent: f64 },
    /// Network partition or timeout
    NetworkIssue { timeout_ms: u64 },
    /// Disk space exhaustion
    DiskFull { available_bytes: u64 },
    /// Invalid configuration value
    InvalidConfig { field: String, value: String },
    /// External service unavailable
    ServiceUnavailable { service: String },
    /// Unknown root cause
    Unknown { description: String },
}

/// Recovery strategy applied to failure
#[derive(Debug, Clone, PartialEq, Eq, Hash, Serialize, Deserialize)]
pub enum RecoveryStrategy {
    /// Restart the failed component
    Restart,
    /// Scale up resources
    ScaleUp,
    /// Failover to backup
    Failover,
    /// Apply configuration fix
    ConfigFix,
    /// Circuit breaker activation
    CircuitBreaker,
    /// Manual intervention required
    ManualIntervention,
    /// No recovery attempted
    None,
}

/// Historical failure record
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct HistoricalFailure {
    /// Type of failure
    pub failure_type: FailureType,
    /// Root cause analysis
    pub root_cause: RootCause,
    /// Observable symptoms
    pub symptoms: Vec<String>,
    /// Recovery strategy applied
    pub recovery_strategy: RecoveryStrategy,
    /// Time taken to recover
    pub recovery_time: Duration,
    /// Whether recovery was successful
    pub success: bool,
    /// Timestamp when failure occurred (not serialized - reconstructed on deserialization)
    #[serde(skip, default = "default_instant")]
    pub timestamp: Instant,
}

/// Default Instant for deserialization
fn default_instant() -> Instant {
    Instant::now()
}

/// Effectiveness metrics for a recovery strategy
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct StrategyEffectiveness {
    /// Number of successful recoveries
    pub success_count: usize,
    /// Number of failed recoveries
    pub failure_count: usize,
    /// Average time to recover
    pub avg_recovery_time: Duration,
    /// Success rate (0.0-1.0)
    pub success_rate: f64,
}

impl StrategyEffectiveness {
    /// Create new effectiveness tracker
    fn new() -> Self {
        StrategyEffectiveness {
            success_count: 0,
            failure_count: 0,
            avg_recovery_time: Duration::from_secs(0),
            success_rate: 0.0,
        }
    }

    /// Update effectiveness metrics with new recovery result
    fn update(&mut self, success: bool, recovery_time: Duration) {
        if success {
            self.success_count += 1;
        } else {
            self.failure_count += 1;
        }

        let total = self.success_count + self.failure_count;
        self.success_rate = self.success_count as f64 / total as f64;

        // Update rolling average recovery time
        let current_avg_secs = self.avg_recovery_time.as_secs_f64();
        let new_time_secs = recovery_time.as_secs_f64();
        let new_avg_secs = (current_avg_secs * (total - 1) as f64 + new_time_secs) / total as f64;
        self.avg_recovery_time = Duration::from_secs_f64(new_avg_secs);
    }
}

/// Internal storage for knowledge base
#[derive(Debug, Clone, Serialize, Deserialize)]
struct KnowledgeStorage {
    /// Historical failure records
    failures: Vec<HistoricalFailure>,
    /// Success rate per failure type
    recovery_success_rates: HashMap<FailureType, f64>,
    /// Effectiveness metrics per strategy
    strategy_effectiveness: HashMap<RecoveryStrategy, StrategyEffectiveness>,
}

impl KnowledgeStorage {
    fn new() -> Self {
        KnowledgeStorage {
            failures: Vec::new(),
            recovery_success_rates: HashMap::new(),
            strategy_effectiveness: HashMap::new(),
        }
    }
}

/// Thread-safe failure knowledge base
pub struct FailureKnowledgeBase {
    /// Thread-safe storage
    storage: Arc<RwLock<KnowledgeStorage>>,
    /// Configuration
    config: KnowledgeBaseConfig,
}

impl FailureKnowledgeBase {
    /// Create a new failure knowledge base
    ///
    /// # Arguments
    ///
    /// * `config` - Configuration for the knowledge base
    ///
    /// # Errors
    ///
    /// Returns error if configuration is invalid
    pub fn new(config: KnowledgeBaseConfig) -> Result<Self> {
        if config.similarity_threshold < 0.0 || config.similarity_threshold > 1.0 {
            return Err(KnowledgeBaseError::InvalidConfig(
                "Similarity threshold must be between 0.0 and 1.0".to_string(),
            ));
        }

        if config.max_history_size == 0 {
            return Err(KnowledgeBaseError::InvalidConfig(
                "Max history size must be greater than 0".to_string(),
            ));
        }

        Ok(FailureKnowledgeBase {
            storage: Arc::new(RwLock::new(KnowledgeStorage::new())),
            config,
        })
    }

    /// Record a new failure in the knowledge base
    ///
    /// # Arguments
    ///
    /// * `failure` - Historical failure to record
    ///
    /// # Errors
    ///
    /// Returns error if lock acquisition fails
    pub fn record_failure(&self, failure: HistoricalFailure) -> Result<()> {
        let mut storage = self
            .storage
            .write()
            .map_err(|e| KnowledgeBaseError::LockError(e.to_string()))?;

        // Calculate success rate for this failure type
        let failures_of_type_count = storage
            .failures
            .iter()
            .filter(|f| f.failure_type == failure.failure_type)
            .count();

        let successful_of_type = storage
            .failures
            .iter()
            .filter(|f| f.failure_type == failure.failure_type && f.success)
            .count();

        let total = failures_of_type_count + 1;
        let successful = successful_of_type + if failure.success { 1 } else { 0 };
        let success_rate = successful as f64 / total as f64;

        // Update failure type success rate
        storage
            .recovery_success_rates
            .insert(failure.failure_type.clone(), success_rate);

        // Update strategy effectiveness
        let effectiveness = storage
            .strategy_effectiveness
            .entry(failure.recovery_strategy.clone())
            .or_insert_with(StrategyEffectiveness::new);

        effectiveness.update(failure.success, failure.recovery_time);

        // Add failure to history
        storage.failures.push(failure);

        // Enforce max history size (FIFO eviction)
        while storage.failures.len() > self.config.max_history_size {
            storage.failures.remove(0);
        }

        Ok(())
    }

    /// Query similar failures based on symptoms
    ///
    /// # Arguments
    ///
    /// * `symptoms` - List of symptoms to match
    ///
    /// # Errors
    ///
    /// Returns error if lock acquisition fails
    pub fn query_similar(&self, symptoms: &[String]) -> Result<Vec<HistoricalFailure>> {
        let storage = self
            .storage
            .read()
            .map_err(|e| KnowledgeBaseError::LockError(e.to_string()))?;

        let mut results = Vec::new();

        for failure in &storage.failures {
            let similarity = self.calculate_similarity(symptoms, &failure.symptoms);
            if similarity >= self.config.similarity_threshold {
                results.push(failure.clone());
            }
        }

        // Sort by similarity (descending)
        results.sort_by(|a, b| {
            let sim_a = self.calculate_similarity(symptoms, &a.symptoms);
            let sim_b = self.calculate_similarity(symptoms, &b.symptoms);
            sim_b.partial_cmp(&sim_a).unwrap_or(std::cmp::Ordering::Equal)
        });

        Ok(results)
    }

    /// Calculate Jaccard similarity between two symptom sets
    fn calculate_similarity(&self, symptoms_a: &[String], symptoms_b: &[String]) -> f64 {
        if symptoms_a.is_empty() && symptoms_b.is_empty() {
            return 1.0;
        }

        if symptoms_a.is_empty() || symptoms_b.is_empty() {
            return 0.0;
        }

        let set_a: std::collections::HashSet<_> = symptoms_a.iter().collect();
        let set_b: std::collections::HashSet<_> = symptoms_b.iter().collect();

        let intersection_size = set_a.intersection(&set_b).count();
        let union_size = set_a.union(&set_b).count();

        intersection_size as f64 / union_size as f64
    }

    /// Get success rate for a specific failure type
    ///
    /// # Arguments
    ///
    /// * `failure_type` - Type of failure to query
    ///
    /// # Errors
    ///
    /// Returns error if lock acquisition fails
    pub fn get_success_rate(&self, failure_type: &FailureType) -> Result<f64> {
        let storage = self
            .storage
            .read()
            .map_err(|e| KnowledgeBaseError::LockError(e.to_string()))?;

        Ok(storage
            .recovery_success_rates
            .get(failure_type)
            .copied()
            .unwrap_or(0.0))
    }

    /// Get effectiveness metrics for a recovery strategy
    ///
    /// # Arguments
    ///
    /// * `strategy` - Recovery strategy to query
    ///
    /// # Errors
    ///
    /// Returns error if lock acquisition fails
    pub fn get_strategy_effectiveness(&self, strategy: &RecoveryStrategy) -> Result<StrategyEffectiveness> {
        let storage = self
            .storage
            .read()
            .map_err(|e| KnowledgeBaseError::LockError(e.to_string()))?;

        Ok(storage
            .strategy_effectiveness
            .get(strategy)
            .cloned()
            .unwrap_or_else(StrategyEffectiveness::new))
    }

    /// Export knowledge base as JSON
    ///
    /// # Errors
    ///
    /// Returns error if serialization fails
    pub fn export_knowledge(&self) -> Result<String> {
        let storage = self
            .storage
            .read()
            .map_err(|e| KnowledgeBaseError::LockError(e.to_string()))?;

        serde_json::to_string_pretty(&*storage)
            .map_err(|e| KnowledgeBaseError::SerializationError(e.to_string()))
    }

    /// Import knowledge base from JSON
    ///
    /// # Arguments
    ///
    /// * `json` - JSON string containing knowledge base data
    ///
    /// # Errors
    ///
    /// Returns error if deserialization fails
    pub fn import_knowledge(&self, json: &str) -> Result<()> {
        let imported: KnowledgeStorage = serde_json::from_str(json)
            .map_err(|e| KnowledgeBaseError::DeserializationError(e.to_string()))?;

        let mut storage = self
            .storage
            .write()
            .map_err(|e| KnowledgeBaseError::LockError(e.to_string()))?;

        *storage = imported;

        Ok(())
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_knowledge_base_new_with_default_config() {
        // Arrange
        let config = KnowledgeBaseConfig::default();

        // Act
        let kb = FailureKnowledgeBase::new(config);

        // Assert
        assert!(kb.is_ok());
    }

    #[test]
    fn test_knowledge_base_new_with_invalid_similarity_threshold() {
        // Arrange
        let config = KnowledgeBaseConfig {
            max_history_size: 1000,
            similarity_threshold: 1.5, // Invalid: > 1.0
        };

        // Act
        let result = FailureKnowledgeBase::new(config);

        // Assert
        assert!(result.is_err());
        match result {
            Err(KnowledgeBaseError::InvalidConfig(msg)) => {
                assert!(msg.contains("Similarity threshold"));
            }
            _ => panic!("Expected InvalidConfig error"),
        }
    }

    #[test]
    fn test_record_failure_updates_success_rate() {
        // Arrange
        let kb = FailureKnowledgeBase::new(KnowledgeBaseConfig::default()).unwrap();
        let failure = HistoricalFailure {
            failure_type: FailureType::ContainerFailure,
            root_cause: RootCause::MemoryLeak {
                allocation_rate_mb_per_sec: 10.5,
            },
            symptoms: vec!["high_memory".to_string(), "oom_killed".to_string()],
            recovery_strategy: RecoveryStrategy::Restart,
            recovery_time: Duration::from_secs(30),
            success: true,
            timestamp: Instant::now(),
        };

        // Act
        let result = kb.record_failure(failure.clone());

        // Assert
        assert!(result.is_ok());
        let success_rate = kb.get_success_rate(&FailureType::ContainerFailure).unwrap();
        assert_eq!(success_rate, 1.0);
    }

    #[test]
    fn test_query_similar_returns_matching_failures() {
        // Arrange
        let kb = FailureKnowledgeBase::new(KnowledgeBaseConfig::default()).unwrap();
        let failure1 = HistoricalFailure {
            failure_type: FailureType::NetworkFailure,
            root_cause: RootCause::NetworkIssue { timeout_ms: 5000 },
            symptoms: vec!["timeout".to_string(), "connection_refused".to_string()],
            recovery_strategy: RecoveryStrategy::Restart,
            recovery_time: Duration::from_secs(10),
            success: true,
            timestamp: Instant::now(),
        };
        kb.record_failure(failure1).unwrap();

        // Act
        let symptoms = vec!["timeout".to_string(), "connection_refused".to_string()];
        let results = kb.query_similar(&symptoms).unwrap();

        // Assert
        assert_eq!(results.len(), 1);
        assert_eq!(results[0].failure_type, FailureType::NetworkFailure);
    }

    #[test]
    fn test_strategy_effectiveness_tracks_success_rate() {
        // Arrange
        let kb = FailureKnowledgeBase::new(KnowledgeBaseConfig::default()).unwrap();
        let failure1 = HistoricalFailure {
            failure_type: FailureType::ResourceExhaustion,
            root_cause: RootCause::CpuSaturation { avg_cpu_percent: 95.0 },
            symptoms: vec!["high_cpu".to_string()],
            recovery_strategy: RecoveryStrategy::ScaleUp,
            recovery_time: Duration::from_secs(60),
            success: true,
            timestamp: Instant::now(),
        };
        let failure2 = HistoricalFailure {
            failure_type: FailureType::ResourceExhaustion,
            root_cause: RootCause::CpuSaturation { avg_cpu_percent: 98.0 },
            symptoms: vec!["high_cpu".to_string()],
            recovery_strategy: RecoveryStrategy::ScaleUp,
            recovery_time: Duration::from_secs(45),
            success: false,
            timestamp: Instant::now(),
        };

        // Act
        kb.record_failure(failure1).unwrap();
        kb.record_failure(failure2).unwrap();
        let effectiveness = kb.get_strategy_effectiveness(&RecoveryStrategy::ScaleUp).unwrap();

        // Assert
        assert_eq!(effectiveness.success_count, 1);
        assert_eq!(effectiveness.failure_count, 1);
        assert_eq!(effectiveness.success_rate, 0.5);
    }

    #[test]
    fn test_export_and_import_knowledge() {
        // Arrange
        let kb1 = FailureKnowledgeBase::new(KnowledgeBaseConfig::default()).unwrap();
        let failure = HistoricalFailure {
            failure_type: FailureType::ConfigurationError,
            root_cause: RootCause::InvalidConfig {
                field: "port".to_string(),
                value: "invalid".to_string(),
            },
            symptoms: vec!["config_error".to_string()],
            recovery_strategy: RecoveryStrategy::ConfigFix,
            recovery_time: Duration::from_secs(5),
            success: true,
            timestamp: Instant::now(),
        };
        kb1.record_failure(failure).unwrap();

        // Act
        let exported = kb1.export_knowledge().unwrap();
        let kb2 = FailureKnowledgeBase::new(KnowledgeBaseConfig::default()).unwrap();
        let result = kb2.import_knowledge(&exported);

        // Assert
        assert!(result.is_ok());
        let success_rate = kb2.get_success_rate(&FailureType::ConfigurationError).unwrap();
        assert_eq!(success_rate, 1.0);
    }

    #[test]
    fn test_max_history_size_enforced() {
        // Arrange
        let config = KnowledgeBaseConfig {
            max_history_size: 2,
            similarity_threshold: 0.7,
        };
        let kb = FailureKnowledgeBase::new(config).unwrap();

        // Act - Add 3 failures when max is 2
        for i in 0..3 {
            let failure = HistoricalFailure {
                failure_type: FailureType::Unknown,
                root_cause: RootCause::Unknown {
                    description: format!("failure_{}", i),
                },
                symptoms: vec![format!("symptom_{}", i)],
                recovery_strategy: RecoveryStrategy::None,
                recovery_time: Duration::from_secs(1),
                success: true,
                timestamp: Instant::now(),
            };
            kb.record_failure(failure).unwrap();
        }

        // Assert - Only 2 failures should remain
        let storage = kb.storage.read().unwrap();
        assert_eq!(storage.failures.len(), 2);
        // First failure should be evicted (FIFO)
        assert!(!storage.failures.iter().any(|f| {
            if let RootCause::Unknown { description } = &f.root_cause {
                description == "failure_0"
            } else {
                false
            }
        }));
    }

    #[test]
    fn test_similarity_calculation_empty_sets() {
        // Arrange
        let kb = FailureKnowledgeBase::new(KnowledgeBaseConfig::default()).unwrap();

        // Act
        let similarity = kb.calculate_similarity(&[], &[]);

        // Assert
        assert_eq!(similarity, 1.0);
    }
}
