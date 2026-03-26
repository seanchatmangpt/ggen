//! Evidence Tracker for Byzantine-Aware Replication
//!
//! Correlates Byzantine evidence with region health, enabling automatic isolation
//! of malicious regions and evidence-based conflict resolution.

use crate::byzantine::evidence::{Evidence, EvidenceLog, EvidenceSeverity};
use crate::error::{OSIRISError, Result};
use serde::{Deserialize, Serialize};
use std::collections::HashMap;
use std::sync::Arc;
use tokio::sync::RwLock;
use uuid::Uuid;

use super::manager::RegionHealth;

/// Correlation between a replication event and Byzantine evidence
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct EvidenceLedgerEntry {
    pub id: String,
    pub region_id: String,
    pub evidence_id: String,
    pub replication_event: ReplicationEvent,
    pub timestamp: u64,
    pub resolved: bool,
}

/// Types of replication events that can be correlated with evidence
#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
pub enum ReplicationEvent {
    /// Write operation replicated to region
    WriteReplicated { operation_id: String, vector_clock_version: u64 },

    /// Region failed to acknowledge replication
    AckTimeout { operation_id: String, attempt: u32 },

    /// Region returned conflicting data
    DataConflict { expected_hash: String, received_hash: String },

    /// Region health check failed
    HealthCheckFailed { reason: String },

    /// Region attempted to become primary during failover
    FailoverAttempt { priority: u32 },
}

/// Health score for a region based on evidence
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
pub struct HealthScore(pub i32);

impl HealthScore {
    pub const MAX: i32 = 100;
    pub const MIN: i32 = 0;
    pub const ISOLATION_THRESHOLD: i32 = 30;

    pub fn new(score: i32) -> Self {
        Self(score.clamp(Self::MIN, Self::MAX))
    }

    pub fn is_healthy(&self) -> bool {
        self.0 >= 70
    }

    pub fn is_degraded(&self) -> bool {
        self.0 >= 40 && self.0 < 70
    }

    pub fn is_unhealthy(&self) -> bool {
        self.0 < Self::ISOLATION_THRESHOLD
    }

    pub fn should_isolate(&self) -> bool {
        self.0 < Self::ISOLATION_THRESHOLD
    }
}

/// Evidence tracker linking replication events to Byzantine evidence
pub struct EvidenceTracker {
    /// Ledger of all evidence correlations
    ledger: Arc<RwLock<Vec<EvidenceLedgerEntry>>>,

    /// Health scores per region
    health_scores: Arc<RwLock<HashMap<String, HealthScore>>>,

    /// Reference to Byzantine evidence log
    evidence_log: Arc<RwLock<EvidenceLog>>,

    /// Configuration
    config: EvidenceTrackerConfig,
}

/// Configuration for evidence tracker
#[derive(Debug, Clone)]
pub struct EvidenceTrackerConfig {
    /// Penalty for low-severity evidence (timeout, etc.)
    pub low_severity_penalty: i32,

    /// Penalty for medium-severity evidence
    pub medium_severity_penalty: i32,

    /// Penalty for high-severity evidence
    pub high_severity_penalty: i32,

    /// Penalty for critical-severity evidence
    pub critical_severity_penalty: i32,

    /// Health recovery per successful operation
    pub recovery_per_success: i32,

    /// Whether to automatically isolate unhealthy regions
    pub auto_isolate: bool,
}

impl Default for EvidenceTrackerConfig {
    fn default() -> Self {
        Self {
            low_severity_penalty: 5,
            medium_severity_penalty: 15,
            high_severity_penalty: 30,
            critical_severity_penalty: 50,
            recovery_per_success: 2,
            auto_isolate: true,
        }
    }
}

impl EvidenceTracker {
    /// Create a new evidence tracker
    pub fn new(config: EvidenceTrackerConfig) -> Self {
        Self {
            ledger: Arc::new(RwLock::new(Vec::new())),
            health_scores: Arc::new(RwLock::new(HashMap::new())),
            evidence_log: Arc::new(RwLock::new(EvidenceLog::new())),
            config,
        }
    }

    /// Create with default configuration
    pub fn default_with_regions(region_ids: &[String]) -> Self {
        let mut health_scores = HashMap::new();
        for region_id in region_ids {
            health_scores.insert(region_id.clone(), HealthScore::new(100));
        }

        Self {
            ledger: Arc::new(RwLock::new(Vec::new())),
            health_scores: Arc::new(RwLock::new(health_scores)),
            evidence_log: Arc::new(RwLock::new(EvidenceLog::new())),
            config: EvidenceTrackerConfig::default(),
        }
    }

    /// Record a replication event and correlate with evidence
    pub async fn record_event(
        &self,
        region_id: &str,
        event: ReplicationEvent,
    ) -> Result<EvidenceLedgerEntry> {
        let entry = EvidenceLedgerEntry {
            id: Uuid::new_v4().to_string(),
            region_id: region_id.to_string(),
            evidence_id: String::new(), // Will be set if evidence is created
            replication_event: event.clone(),
            timestamp: std::time::SystemTime::now()
                .duration_since(std::time::UNIX_EPOCH)
                .unwrap()
                .as_secs(),
            resolved: false,
        };

        // Update health score based on event type
        self.update_health_for_event(region_id, &event).await?;

        // Add to ledger
        let mut ledger = self.ledger.write().await;
        ledger.push(entry.clone());

        Ok(entry)
    }

    /// Record Byzantine evidence and correlate with region
    pub async fn record_evidence(&self, region_id: &str, evidence: Evidence) -> Result<()> {
        // Add to evidence log
        let mut log = self.evidence_log.write().await;
        log.add(evidence.clone());
        drop(log);

        // Apply penalty to health score
        let penalty = match evidence.severity {
            EvidenceSeverity::Low => self.config.low_severity_penalty,
            EvidenceSeverity::Medium => self.config.medium_severity_penalty,
            EvidenceSeverity::High => self.config.high_severity_penalty,
            EvidenceSeverity::Critical => self.config.critical_severity_penalty,
        };

        self.apply_penalty(region_id, penalty).await?;

        // Create ledger entry linking evidence to region
        let entry = EvidenceLedgerEntry {
            id: Uuid::new_v4().to_string(),
            region_id: region_id.to_string(),
            evidence_id: evidence.id.clone(),
            replication_event: ReplicationEvent::HealthCheckFailed {
                reason: format!("Evidence recorded: {:?}", evidence.misbehavior),
            },
            timestamp: evidence.timestamp,
            resolved: false,
        };

        let mut ledger = self.ledger.write().await;
        ledger.push(entry);

        Ok(())
    }

    /// Get health score for a region
    pub async fn get_health_score(&self, region_id: &str) -> Option<HealthScore> {
        let scores = self.health_scores.read().await;
        scores.get(region_id).copied()
    }

    /// Get health status for a region based on score
    pub async fn get_region_health(&self, region_id: &str) -> RegionHealth {
        match self.get_health_score(region_id).await {
            Some(score) if score.is_healthy() => RegionHealth::Healthy,
            Some(score) if score.is_degraded() => RegionHealth::Degraded,
            Some(score) if score.is_unhealthy() => RegionHealth::Unhealthy,
            _ => RegionHealth::Unknown,
        }
    }

    /// Check if a region should be isolated based on evidence
    pub async fn should_isolate_region(&self, region_id: &str) -> bool {
        match self.get_health_score(region_id).await {
            Some(score) => score.should_isolate(),
            None => false,
        }
    }

    /// Get all regions that should be isolated
    pub async fn get_regions_to_isolate(&self) -> Vec<String> {
        let scores = self.health_scores.read().await;
        scores
            .iter()
            .filter(|(_, &score)| score.should_isolate())
            .map(|(region_id, _)| region_id.clone())
            .collect()
    }

    /// Get evidence ledger for a region
    pub async fn get_region_ledger(&self, region_id: &str) -> Vec<EvidenceLedgerEntry> {
        let ledger = self.ledger.read().await;
        ledger
            .iter()
            .filter(|entry| entry.region_id == region_id)
            .cloned()
            .collect()
    }

    /// Get all unresolved conflicts
    pub async fn get_unresolved_conflicts(&self) -> Vec<EvidenceLedgerEntry> {
        let ledger = self.ledger.read().await;
        ledger
            .iter()
            .filter(|entry| !entry.resolved)
            .filter(|entry| {
                matches!(
                    &entry.replication_event,
                    ReplicationEvent::DataConflict { .. }
                )
            })
            .cloned()
            .collect()
    }

    /// Resolve a conflict in the ledger
    pub async fn resolve_conflict(&self, entry_id: &str) -> Result<()> {
        let mut ledger = self.ledger.write().await;
        if let Some(entry) = ledger.iter_mut().find(|e| e.id == entry_id) {
            entry.resolved = true;
            Ok(())
        } else {
            Err(OSIRISError::ServiceUnavailable(format!(
                "Ledger entry {} not found",
                entry_id
            )))
        }
    }

    /// Replay evidence for conflict resolution
    pub async fn replay_evidence(&self, _region_id: &str) -> Vec<Evidence> {
        let log = self.evidence_log.read().await;
        // In a real implementation, this would filter by region and replay relevant evidence
        log.get_all().iter().cloned().collect()
    }

    /// Recover health score for a region (after successful operation)
    pub async fn recover_health(&self, region_id: &str) -> Result<()> {
        let mut scores = self.health_scores.write().await;
        let current_score = scores.get(region_id).copied().unwrap_or(HealthScore::new(50));
        let new_score = HealthScore::new(current_score.0 + self.config.recovery_per_success);
        scores.insert(region_id.to_string(), new_score);
        Ok(())
    }

    /// Update health score based on replication event
    async fn update_health_for_event(&self, region_id: &str, event: &ReplicationEvent) -> Result<()> {
        let penalty = match event {
            ReplicationEvent::WriteReplicated { .. } => {
                // Successful operation - recover health
                self.recover_health(region_id).await?;
                return Ok(());
            }
            ReplicationEvent::AckTimeout { .. } => 10,
            ReplicationEvent::DataConflict { .. } => 25,
            ReplicationEvent::HealthCheckFailed { .. } => 15,
            ReplicationEvent::FailoverAttempt { .. } => 5,
        };

        self.apply_penalty(region_id, penalty).await
    }

    /// Apply penalty to region's health score
    async fn apply_penalty(&self, region_id: &str, penalty: i32) -> Result<()> {
        let mut scores = self.health_scores.write().await;
        let current_score = scores.get(region_id).copied().unwrap_or(HealthScore::new(100));
        let new_score = HealthScore::new(current_score.0 - penalty);
        scores.insert(region_id.to_string(), new_score);

        // Check if we should auto-isolate
        if self.config.auto_isolate && new_score.should_isolate() {
            drop(scores);
            // Trigger isolation (would notify manager in real implementation)
            tracing::warn!(
                "Region {} has unhealthy score {} and should be isolated",
                region_id,
                new_score.0
            );
        }

        Ok(())
    }

    /// Get evidence log statistics
    pub async fn get_evidence_stats(&self, region_id: &str) -> EvidenceStats {
        let log = self.evidence_log.read().await;
        let region_evidence = log.get_all().iter().filter(|e| {
            // Convert NodeId to string for comparison
            format!("{:?}", e.accused_node).contains(region_id)
        });

        let total = region_evidence.clone().count();
        let critical = region_evidence
            .clone()
            .filter(|e| e.severity == EvidenceSeverity::Critical)
            .count();
        let high = region_evidence
            .clone()
            .filter(|e| e.severity == EvidenceSeverity::High)
            .count();

        EvidenceStats {
            total_evidence: total,
            critical_evidence: critical,
            high_severity_evidence: high,
        }
    }
}

/// Statistics about evidence for a region
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct EvidenceStats {
    pub total_evidence: usize,
    pub critical_evidence: usize,
    pub high_severity_evidence: usize,
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::byzantine::NodeId;
    use crate::byzantine::consensus::ProposalValue;

    #[tokio::test]
    async fn test_evidence_tracker_creation() {
        let regions = vec
![
            "us-east".to_string()
,
            "us-west".to_string()
,
            "eu".to_string()
        ];

        let tracker = EvidenceTracker::default_with_regions(&regions);

        let score = tracker.get_health_score("us-east").await;
        assert_eq!(score, Some(HealthScore::new(100)));

        let health = tracker.get_region_health("us-east").await;
        assert_eq!(health, RegionHealth::Healthy);
    }

    #[tokio::test]
    async fn test_record_successful_event() {
        let regions = vec
!["us-east".to_string()
];
        let tracker = EvidenceTracker::default_with_regions(&regions);

        tracker
            .record_event(
                "us-east",
                ReplicationEvent::WriteReplicated {
                    operation_id: "op-1".to_string()
,
                    vector_clock_version: 1,
                },
            )
            .await
            .unwrap();

        // Health should remain high after successful event
        let score = tracker.get_health_score("us-east").await;
        assert!(score.is_some()
);
        assert!(score.unwrap().is_healthy());
    }

    #[tokio::test]
    async fn test_record_failed_event() {
        let regions = vec
!["us-east".to_string()
];
        let tracker = EvidenceTracker::default_with_regions(&regions);

        tracker
            .record_event(
                "us-east",
                ReplicationEvent::AckTimeout {
                    operation_id: "op-1".to_string()
,
                    attempt: 1,
                },
            )
            .await
            .unwrap();

        // Health should decrease after timeout
        let score = tracker.get_health_score("us-east").await;
        assert_eq!(score, Some(HealthScore::new(90)));

        let health = tracker.get_region_health("us-east").await;
        assert_eq!(health, RegionHealth::Healthy);
    }

    #[tokio::test]
    async fn test_multiple_failures_lead_to_degraded() {
        let regions = vec
!["us-east".to_string()
];
        let tracker = EvidenceTracker::default_with_regions(&regions);

        // Record multiple timeouts
        for i in 0..5 {
            tracker
                .record_event(
                    "us-east",
                    ReplicationEvent::AckTimeout {
                        operation_id: format!("op-{}", i),
                        attempt: 1,
                    },
                )
                .await
                .unwrap();
        }

        let score = tracker.get_health_score("us-east").await;
        assert_eq!(score, Some(HealthScore::new(50)));

        let health = tracker.get_region_health("us-east").await;
        assert_eq!(health, RegionHealth::Degraded);
    }

    #[tokio::test]
    async fn test_data_conflict_severe_penalty() {
        let regions = vec
!["us-east".to_string()
];
        let tracker = EvidenceTracker::default_with_regions(&regions);

        tracker
            .record_event(
                "us-east",
                ReplicationEvent::DataConflict {
                    expected_hash: "abc123".to_string()
,
                    received_hash: "def456".to_string()
,
                },
            )
            .await
            .unwrap();

        let score = tracker.get_health_score("us-east").await;
        assert_eq!(score, Some(HealthScore::new(75)));

        let health = tracker.get_region_health("us-east").await;
        assert_eq!(health, RegionHealth::Healthy);
    }

    #[tokio::test]
    async fn test_should_isolate_after_critical_evidence() {
        let regions = vec
!["us-east".to_string()
];
        let tracker = EvidenceTracker::default_with_regions(&regions);

        // Record critical Byzantine evidence
        let node_id = NodeId::new(1);
        let evidence = Evidence::new(
            node_id,
            Misbehavior::ConflictingVotes {
                value_a: ProposalValue::new("data_a".to_string()
),
                value_b: ProposalValue::new("data_b".to_string()
),
            },
        );

        tracker
            .record_evidence("us-east", evidence)
            .await
            .unwrap();

        // Critical evidence applies 50 point penalty
        let score = tracker.get_health_score("us-east").await;
        assert_eq!(score, Some(HealthScore::new(50)));

        let health = tracker.get_region_health("us-east").await;
        assert_eq!(health, RegionHealth::Degraded);

        // Second critical evidence should trigger isolation
        let evidence2 = Evidence::new(
            node_id,
            Misbehavior::DoubleProposal {
                round: 1,
                value_a: ProposalValue::new("data_a".to_string()
),
                value_b: ProposalValue::new("data_b".to_string()
),
            },
        );

        tracker
            .record_evidence("us-east", evidence2)
            .await
            .unwrap();

        let score = tracker.get_health_score("us-east").await;
        assert_eq!(score, Some(HealthScore::new(0)));

        assert!(tracker.should_isolate_region("us-east").await);
    }

    #[tokio::test]
    async fn test_get_regions_to_isolate() {
        let regions = vec
!["us-east".to_string()
, "us-west".to_string()
, "eu".to_string()
];
        let tracker = EvidenceTracker::default_with_regions(&regions);

        // Make us-east unhealthy
        let node_id = NodeId::new(1);
        for _ in 0..2 {
            let evidence = Evidence::new(
                node_id,
                Misbehavior::ConflictingVotes {
                    value_a: ProposalValue::new("a".to_string()
),
                    value_b: ProposalValue::new("b".to_string()
),
                },
            );
            tracker
                .record_evidence("us-east", evidence)
                .await
                .unwrap();
        }

        let to_isolate = tracker.get_regions_to_isolate().await;
        assert_eq!(to_isolate.len()
, 1);
        assert!(to_isolate.contains(&"us-east".to_string()
));
    }

    #[tokio::test]
    async fn test_get_region_ledger() {
        let regions = vec
!["us-east".to_string()
];
        let tracker = EvidenceTracker::default_with_regions(&regions);

        tracker
            .record_event(
                "us-east",
                ReplicationEvent::WriteReplicated {
                    operation_id: "op-1".to_string()
,
                    vector_clock_version: 1,
                },
            )
            .await
            .unwrap();

        let ledger = tracker.get_region_ledger("us-east").await;
        assert_eq!(ledger.len()
, 1);
        assert_eq!(ledger[0].region_id, "us-east");
    }

    #[tokio::test]
    async fn test_unresolved_conflicts() {
        let regions = vec
!["us-east".to_string()
];
        let tracker = EvidenceTracker::default_with_regions(&regions);

        tracker
            .record_event(
                "us-east",
                ReplicationEvent::DataConflict {
                    expected_hash: "abc123".to_string()
,
                    received_hash: "def456".to_string()
,
                },
            )
            .await
            .unwrap();

        let conflicts = tracker.get_unresolved_conflicts().await;
        assert_eq!(conflicts.len()
, 1);
        assert!(!conflicts[0].resolved);
    }

    #[tokio::test]
    async fn test_resolve_conflict() {
        let regions = vec
!["us-east".to_string()
];
        let tracker = EvidenceTracker::default_with_regions(&regions);

        let entry = tracker
            .record_event(
                "us-east",
                ReplicationEvent::DataConflict {
                    expected_hash: "abc123".to_string()
,
                    received_hash: "def456".to_string()
,
                },
            )
            .await
            .unwrap();

        let conflicts_before = tracker.get_unresolved_conflicts().await;
        assert_eq!(conflicts_before.len()
, 1);

        tracker.resolve_conflict(&entry.id).await.unwrap();

        let conflicts_after = tracker.get_unresolved_conflicts().await;
        assert_eq!(conflicts_after.len()
, 0);
    }

    #[tokio::test]
    async fn test_health_score_boundaries() {
        let score = HealthScore::new(150);
        assert_eq!(score.0, HealthScore::MAX);

        let score = HealthScore::new(-10);
        assert_eq!(score.0, HealthScore::MIN);

        let score = HealthScore::new(50);
        assert_eq!(score.0, 50);
    }

    #[tokio::test]
    async fn test_evidence_stats() {
        let regions = vec
!["us-east".to_string()
];
        let tracker = EvidenceTracker::default_with_regions(&regions);

        let node_id = NodeId::new(1);
        let evidence = Evidence::new(
            node_id,
            Misbehavior::ConflictingVotes {
                value_a: ProposalValue::new("a".to_string()
),
                value_b: ProposalValue::new("b".to_string()
),
            },
        );

        tracker
            .record_evidence("us-east", evidence)
            .await
            .unwrap();

        let stats = tracker.get_evidence_stats("us-east").await;
        assert_eq!(stats.total_evidence, 1);
        assert_eq!(stats.critical_evidence, 1);
    }

    #[tokio::test]
    async fn test_recovery_after_failure() {
        let regions = vec
!["us-east".to_string()
];
        let tracker = EvidenceTracker::default_with_regions(&regions);

        // Apply penalty
        tracker
            .record_event(
                "us-east",
                ReplicationEvent::AckTimeout {
                    operation_id: "op-1".to_string()
,
                    attempt: 1,
                },
            )
            .await
            .unwrap();

        assert_eq!(
            tracker.get_health_score("us-east").await,
            Some(HealthScore::new(90))
        );

        // Successful operation should recover
        tracker
            .record_event(
                "us-east",
                ReplicationEvent::WriteReplicated {
                    operation_id: "op-2".to_string()
,
                    vector_clock_version: 2,
                },
            )
            .await
            .unwrap();

        assert_eq!(
            tracker.get_health_score("us-east").await,
            Some(HealthScore::new(92))
        );
    }
}
