//! Root cause analysis for autonomic cluster resilience
//!
//! Provides comprehensive root cause analysis capabilities for distributed systems,
//! including symptom matching, historical pattern recognition, and confidence scoring.

use super::knowledge_base::{FailureKnowledgeBase, FailureType, HistoricalFailure, RootCause};
use std::collections::HashMap;
use std::sync::Arc;
use std::time::{Duration, Instant};
use thiserror::Error;

/// Error types for analysis operations
#[derive(Debug, Error)]
pub enum AnalyzeError {
    /// No failures detected in cluster state
    #[error("No failures detected in cluster state")]
    NoFailuresDetected,

    /// Insufficient data for analysis
    #[error("Insufficient data for analysis: {0}")]
    InsufficientData(String),

    /// Knowledge base query failed
    #[error("Knowledge base query failed: {0}")]
    KnowledgeBaseError(String),

    /// Symptom matching failed
    #[error("Symptom matching failed: {0}")]
    SymptomMatchingFailed(String),

    /// Root cause determination failed
    #[error("Root cause determination failed: {0}")]
    RootCauseFailed(String),

    /// Invalid cluster state
    #[error("Invalid cluster state: {0}")]
    InvalidState(String),
}

/// Result type for analysis operations
pub type Result<T> = std::result::Result<T, AnalyzeError>;

/// Cluster state representation
///
/// Captures the current state of the distributed cluster for analysis.
#[derive(Debug, Clone)]
pub struct ClusterState {
    /// Node states indexed by node ID
    pub nodes: HashMap<String, NodeState>,
    /// Detected failures in the cluster
    pub failures: Vec<DetectedFailure>,
    /// Timestamp when state was captured
    pub timestamp: Instant,
    /// Network connectivity matrix
    pub network_connectivity: HashMap<(String, String), bool>,
}

impl ClusterState {
    /// Create a new cluster state
    #[must_use]
    pub fn new() -> Self {
        Self {
            nodes: HashMap::new(),
            failures: Vec::new(),
            timestamp: Instant::now(),
            network_connectivity: HashMap::new(),
        }
    }

    /// Add a node to the cluster state
    pub fn add_node(&mut self, node_id: String, state: NodeState) {
        self.nodes.insert(node_id, state);
    }

    /// Add a detected failure
    pub fn add_failure(&mut self, failure: DetectedFailure) {
        self.failures.push(failure);
    }

    /// Check if cluster has any failures
    #[must_use]
    pub fn has_failures(&self) -> bool {
        !self.failures.is_empty()
    }

    /// Get the number of nodes in the cluster
    #[must_use]
    pub fn node_count(&self) -> usize {
        self.nodes.len()
    }
}

impl Default for ClusterState {
    fn default() -> Self {
        Self::new()
    }
}

/// Node state information
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct NodeState {
    /// Node identifier
    pub node_id: String,
    /// Whether node is healthy
    pub healthy: bool,
    /// CPU utilization percentage (0-100)
    pub cpu_usage: u8,
    /// Memory utilization percentage (0-100)
    pub memory_usage: u8,
    /// Network latency in milliseconds
    pub network_latency_ms: u64,
}

/// Detected failure information
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct DetectedFailure {
    /// Failure identifier
    pub failure_id: String,
    /// Type of failure detected
    pub failure_type: FailureType,
    /// Node where failure was detected
    pub affected_node: String,
    /// Timestamp of detection
    pub detected_at: Duration,
    /// Failure severity (0-100, higher is more severe)
    pub severity: u8,
    /// Additional metadata
    pub metadata: HashMap<String, String>,
}

/// Resource type for resource limit failures
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum ResourceType {
    /// CPU resource
    Cpu,
    /// Memory resource
    Memory,
    /// Disk I/O resource
    Disk,
    /// Network bandwidth
    Network,
}

impl std::fmt::Display for ResourceType {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            ResourceType::Cpu => write!(f, "CPU"),
            ResourceType::Memory => write!(f, "Memory"),
            ResourceType::Disk => write!(f, "Disk"),
            ResourceType::Network => write!(f, "Network"),
        }
    }
}

/// Analysis result
#[derive(Debug, Clone, PartialEq)]
pub struct AnalysisResult {
    /// Type of failure identified
    pub failure_type: FailureType,
    /// Root cause of the failure
    pub root_cause: RootCause,
    /// Confidence in the analysis (0.0-1.0)
    pub confidence: f64,
    /// Historical failures that match
    pub historical_matches: Vec<HistoricalFailure>,
    /// Timestamp of analysis
    pub timestamp: Instant,
}

impl AnalysisResult {
    /// Check if analysis has high confidence (>= 0.8)
    #[must_use]
    pub fn has_high_confidence(&self) -> bool {
        self.confidence >= 0.8
    }

    /// Check if analysis has historical precedent
    #[must_use]
    pub fn has_historical_precedent(&self) -> bool {
        !self.historical_matches.is_empty()
    }
}

/// Symptom match result
#[derive(Debug, Clone, PartialEq)]
pub struct SymptomMatch {
    /// Failure type matched
    pub failure_type: FailureType,
    /// Confidence score (0.0-1.0)
    pub confidence: f64,
    /// Matched historical failures
    pub historical_matches: Vec<HistoricalFailure>,
    /// Symptom patterns that matched
    pub matched_patterns: Vec<String>,
}

/// Symptom matcher for identifying failure patterns
#[derive(Debug, Clone)]
pub struct SymptomMatcher {
    /// Minimum confidence threshold for matches
    confidence_threshold: f64,
}

impl SymptomMatcher {
    /// Create a new symptom matcher
    ///
    /// # Arguments
    ///
    /// * `confidence_threshold` - Minimum confidence for valid matches (0.0-1.0)
    #[must_use]
    pub fn new(confidence_threshold: f64) -> Self {
        Self {
            confidence_threshold: confidence_threshold.clamp(0.0, 1.0),
        }
    }

    /// Match symptoms against known failure patterns
    ///
    /// # Arguments
    ///
    /// * `failures` - Detected failures to analyze
    ///
    /// # Errors
    ///
    /// Returns error if symptom matching fails or produces invalid results
    pub fn match_symptoms(&self, failures: &[DetectedFailure]) -> Result<Vec<SymptomMatch>> {
        if failures.is_empty() {
            return Err(AnalyzeError::SymptomMatchingFailed(
                "No failures provided for matching".to_string(),
            ));
        }

        let mut matches = Vec::new();

        for failure in failures {
            // Calculate confidence based on severity and failure type
            let confidence = self.calculate_symptom_confidence(failure);

            if confidence >= self.confidence_threshold {
                matches.push(SymptomMatch {
                    failure_type: failure.failure_type.clone(),
                    confidence,
                    historical_matches: Vec::new(), // Populated by analyzer
                    matched_patterns: vec![format!("{:?}", failure.failure_type)],
                });
            }
        }

        if matches.is_empty() {
            return Err(AnalyzeError::SymptomMatchingFailed(
                "No symptoms matched confidence threshold".to_string(),
            ));
        }

        Ok(matches)
    }

    /// Calculate confidence score for a detected failure
    fn calculate_symptom_confidence(&self, failure: &DetectedFailure) -> f64 {
        // Base confidence from severity (0-100 -> 0.0-0.7)
        let severity_score = (f64::from(failure.severity) / 100.0) * 0.7;

        // Bonus confidence for specific failure types (0.0-0.3)
        let type_bonus = match failure.failure_type {
            FailureType::ContainerFailure => 0.3,
            FailureType::NetworkFailure => 0.25,
            FailureType::ResourceExhaustion => 0.2,
            FailureType::ServiceDegradation => 0.15,
            FailureType::DataCorruption => 0.2,
            FailureType::ConfigurationError => 0.25,
            FailureType::DependencyFailure => 0.2,
            FailureType::Unknown => 0.1,
        };

        (severity_score + type_bonus).clamp(0.0, 1.0)
    }
}

impl Default for SymptomMatcher {
    fn default() -> Self {
        Self::new(0.5) // 50% confidence threshold by default
    }
}

/// Resilience analyzer for root cause analysis
pub struct ResilienceAnalyzer {
    /// Knowledge base of historical failures
    knowledge_base: Arc<FailureKnowledgeBase>,
    /// Symptom matcher for pattern recognition
    symptom_matcher: SymptomMatcher,
}

impl ResilienceAnalyzer {
    /// Create a new resilience analyzer
    ///
    /// # Errors
    ///
    /// Returns error if initialization fails (reserved for future validation)
    pub fn new(knowledge_base: Arc<FailureKnowledgeBase>) -> Result<Self> {
        Ok(Self {
            knowledge_base,
            symptom_matcher: SymptomMatcher::default(),
        })
    }

    /// Analyze cluster state and determine root cause
    ///
    /// # Arguments
    ///
    /// * `state` - Current cluster state to analyze
    ///
    /// # Errors
    ///
    /// Returns error if analysis fails or produces invalid results
    pub fn analyze(&self, state: &ClusterState) -> Result<AnalysisResult> {
        if !state.has_failures() {
            return Err(AnalyzeError::NoFailuresDetected);
        }

        // Match symptoms to failure patterns
        let matches = self.match_symptoms(&state.failures)?;

        // Determine root cause from matches
        let root_cause = self.determine_root_cause(&matches)?;

        // Get primary failure type (highest confidence match)
        let primary_match = matches
            .iter()
            .max_by(|a, b| a.confidence.partial_cmp(&b.confidence).unwrap())
            .ok_or_else(|| AnalyzeError::InsufficientData("No valid matches".to_string()))?;

        // Query historical failures from knowledge base
        let symptom_strings: Vec<String> = state
            .failures
            .iter()
            .map(|f| format!("{:?}", f.failure_type))
            .collect();

        let historical_matches = self
            .knowledge_base
            .query_similar(&symptom_strings)
            .unwrap_or_default();

        // Calculate overall confidence
        let confidence = self.calculate_confidence(&historical_matches);

        Ok(AnalysisResult {
            failure_type: primary_match.failure_type.clone(),
            root_cause,
            confidence,
            historical_matches,
            timestamp: Instant::now(),
        })
    }

    /// Match symptoms against known patterns
    fn match_symptoms(&self, failures: &[DetectedFailure]) -> Result<Vec<SymptomMatch>> {
        let mut matches = self.symptom_matcher.match_symptoms(failures)?;

        // Enrich matches with historical data from knowledge base
        for symptom_match in &mut matches {
            let symptoms = vec![format!("{:?}", symptom_match.failure_type)];
            if let Ok(historical) = self.knowledge_base.query_similar(&symptoms) {
                symptom_match.historical_matches = historical;
            }
        }

        Ok(matches)
    }

    /// Determine root cause from symptom matches
    fn determine_root_cause(&self, matches: &[SymptomMatch]) -> Result<RootCause> {
        if matches.is_empty() {
            return Err(AnalyzeError::RootCauseFailed(
                "No symptom matches provided".to_string(),
            ));
        }

        // Get highest confidence match
        let primary_match = matches
            .iter()
            .max_by(|a, b| a.confidence.partial_cmp(&b.confidence).unwrap())
            .ok_or_else(|| AnalyzeError::RootCauseFailed("No valid matches".to_string()))?;

        // Determine root cause based on failure type
        let root_cause = match primary_match.failure_type {
            FailureType::ContainerFailure => RootCause::MemoryLeak {
                allocation_rate_mb_per_sec: 10.0,
            },
            FailureType::NetworkFailure => RootCause::NetworkIssue { timeout_ms: 5000 },
            FailureType::ResourceExhaustion => RootCause::CpuSaturation {
                avg_cpu_percent: 95.0,
            },
            FailureType::ServiceDegradation => RootCause::CpuSaturation {
                avg_cpu_percent: 85.0,
            },
            FailureType::DataCorruption => RootCause::Unknown {
                description: "Data corruption detected".to_string(),
            },
            FailureType::ConfigurationError => RootCause::InvalidConfig {
                field: "Unknown".to_string(),
                value: "Unknown".to_string(),
            },
            FailureType::DependencyFailure => RootCause::ServiceUnavailable {
                service: "Unknown".to_string(),
            },
            FailureType::Unknown => RootCause::Unknown {
                description: "Unknown failure".to_string(),
            },
        };

        Ok(root_cause)
    }

    /// Calculate overall confidence from historical matches
    fn calculate_confidence(&self, historical: &[HistoricalFailure]) -> f64 {
        if historical.is_empty() {
            return 0.5; // Baseline confidence with no historical data
        }

        // Calculate success rate from historical recoveries
        let successful_recoveries = historical.iter().filter(|f| f.success).count();

        let success_rate = (successful_recoveries as f64) / (historical.len() as f64);

        // Confidence increases with historical precedent and success rate
        let precedent_bonus = (historical.len() as f64).min(10.0) / 10.0 * 0.3;
        let success_bonus = success_rate * 0.2;

        (0.5 + precedent_bonus + success_bonus).clamp(0.0, 1.0)
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use super::super::knowledge_base::KnowledgeBaseConfig;

    // AAA Pattern: Arrange-Act-Assert with state-based testing

    #[test]
    fn test_cluster_state_creation() {
        // Arrange & Act
        let state = ClusterState::new();

        // Assert
        assert_eq!(state.node_count(), 0);
        assert!(!state.has_failures());
    }

    #[test]
    fn test_cluster_state_add_node() {
        // Arrange
        let mut state = ClusterState::new();
        let node = NodeState {
            node_id: "node1".to_string(),
            healthy: true,
            cpu_usage: 50,
            memory_usage: 60,
            network_latency_ms: 10,
        };

        // Act
        state.add_node("node1".to_string(), node);

        // Assert
        assert_eq!(state.node_count(), 1);
        assert!(state.nodes.contains_key("node1"));
    }

    #[test]
    fn test_cluster_state_add_failure() {
        // Arrange
        let mut state = ClusterState::new();
        let failure = DetectedFailure {
            failure_id: "f1".to_string(),
            failure_type: FailureType::ContainerFailure,
            affected_node: "node1".to_string(),
            detected_at: Duration::from_secs(100),
            severity: 90,
            metadata: HashMap::new(),
        };

        // Act
        state.add_failure(failure);

        // Assert
        assert!(state.has_failures());
        assert_eq!(state.failures.len(), 1);
    }

    #[test]
    fn test_symptom_matcher_creation() {
        // Arrange & Act
        let matcher = SymptomMatcher::new(0.7);

        // Assert
        assert_eq!(matcher.confidence_threshold, 0.7);
    }

    #[test]
    fn test_symptom_matcher_with_high_severity_failure() {
        // Arrange
        let matcher = SymptomMatcher::new(0.5);
        let failure = DetectedFailure {
            failure_id: "f1".to_string(),
            failure_type: FailureType::ContainerFailure,
            affected_node: "node1".to_string(),
            detected_at: Duration::from_secs(100),
            severity: 100, // High severity
            metadata: HashMap::new(),
        };

        // Act
        let result = matcher.match_symptoms(&[failure]);

        // Assert
        assert!(result.is_ok());
        let matches = result.unwrap();
        assert_eq!(matches.len(), 1);
        assert!(matches[0].confidence >= 0.5);
    }

    #[test]
    fn test_symptom_matcher_empty_failures() {
        // Arrange
        let matcher = SymptomMatcher::new(0.5);

        // Act
        let result = matcher.match_symptoms(&[]);

        // Assert
        assert!(result.is_err());
        match result {
            Err(AnalyzeError::SymptomMatchingFailed(msg)) => {
                assert!(msg.contains("No failures provided"));
            }
            _ => panic!("Expected SymptomMatchingFailed error"),
        }
    }

    #[test]
    fn test_resilience_analyzer_no_failures() {
        // Arrange
        let kb = Arc::new(
            FailureKnowledgeBase::new(KnowledgeBaseConfig::default())
                .expect("Failed to create knowledge base"),
        );
        let analyzer = ResilienceAnalyzer::new(kb).expect("Failed to create analyzer");
        let state = ClusterState::new(); // No failures

        // Act
        let result = analyzer.analyze(&state);

        // Assert
        assert!(result.is_err());
        match result {
            Err(AnalyzeError::NoFailuresDetected) => {
                // Expected error
            }
            _ => panic!("Expected NoFailuresDetected error"),
        }
    }

    #[test]
    fn test_resilience_analyzer_with_single_failure() {
        // Arrange
        let kb = Arc::new(
            FailureKnowledgeBase::new(KnowledgeBaseConfig::default())
                .expect("Failed to create knowledge base"),
        );
        let analyzer = ResilienceAnalyzer::new(kb).expect("Failed to create analyzer");

        let mut state = ClusterState::new();
        let failure = DetectedFailure {
            failure_id: "f1".to_string(),
            failure_type: FailureType::NetworkFailure,
            affected_node: "node1".to_string(),
            detected_at: Duration::from_secs(100),
            severity: 85,
            metadata: HashMap::new(),
        };
        state.add_failure(failure);

        // Act
        let result = analyzer.analyze(&state);

        // Assert
        assert!(result.is_ok());
        let analysis = result.unwrap();
        assert_eq!(analysis.failure_type, FailureType::NetworkFailure);
        assert!(analysis.confidence >= 0.5);
    }

    #[test]
    fn test_analysis_result_high_confidence_check() {
        // Arrange
        let result = AnalysisResult {
            failure_type: FailureType::ResourceExhaustion,
            root_cause: RootCause::CpuSaturation {
                avg_cpu_percent: 95.0,
            },
            confidence: 0.85,
            historical_matches: Vec::new(),
            timestamp: Instant::now(),
        };

        // Act & Assert
        assert!(result.has_high_confidence());
        assert!(!result.has_historical_precedent());
    }

    #[test]
    fn test_resource_type_display() {
        // Arrange, Act & Assert
        assert_eq!(ResourceType::Cpu.to_string(), "CPU");
        assert_eq!(ResourceType::Memory.to_string(), "Memory");
        assert_eq!(ResourceType::Disk.to_string(), "Disk");
        assert_eq!(ResourceType::Network.to_string(), "Network");
    }
}
