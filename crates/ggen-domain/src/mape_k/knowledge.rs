//! Phase 8b: Knowledge - Persistent state management and historical queries
//!
//! Maintains the complete history of observations, findings, overlays, and snapshots.
//! Provides query interface and knowledge compaction for long-running systems.

use super::types::{
    Finding, MAPEMetrics, Observation, OntologyOverlay, SnapshotMetadata, ValidationResult,
};
use std::collections::HashMap;

/// Query parameters for historical searches
#[derive(Debug, Clone)]
pub struct HistoryQuery {
    /// Filter by component name
    pub component: Option<String>,

    /// Filter by severity level
    pub min_severity: Option<String>,

    /// Time range: start timestamp
    pub from_time: Option<u64>,

    /// Time range: end timestamp
    pub to_time: Option<u64>,

    /// Maximum results to return
    pub limit: usize,
}

impl Default for HistoryQuery {
    fn default() -> Self {
        Self {
            component: None,
            min_severity: None,
            from_time: None,
            to_time: None,
            limit: 100,
        }
    }
}

/// Compaction policy for archiving old data
#[derive(Debug, Clone)]
pub struct CompactionPolicy {
    /// Keep observations older than this many days
    pub observation_retention_days: u32,

    /// Keep findings older than this many days
    pub finding_retention_days: u32,

    /// Archive frequency (hours)
    pub archive_frequency_hours: u32,

    /// Enabled
    pub enabled: bool,
}

impl Default for CompactionPolicy {
    fn default() -> Self {
        Self {
            observation_retention_days: 30,
            finding_retention_days: 90,
            archive_frequency_hours: 24,
            enabled: false,
        }
    }
}

/// Knowledge store - persistent state for MAPE-K loop
pub struct KnowledgeStore {
    /// All snapshots (readonly after promotion)
    snapshots: Vec<SnapshotMetadata>,

    /// All observations ingested
    observations: Vec<Observation>,

    /// All findings generated
    findings: Vec<Finding>,

    /// All overlays (proposals + executed)
    overlays: Vec<OntologyOverlay>,

    /// All validation results
    validations: Vec<ValidationResult>,

    /// Currently active snapshot ID
    active_snapshot_id: String,

    /// MAPE-K metrics
    mape_metrics: MAPEMetrics,

    /// Compaction policy
    compaction_policy: CompactionPolicy,

    /// Archive of compacted data
    archived_data: HashMap<String, Vec<u8>>,
}

impl KnowledgeStore {
    /// Create new knowledge store
    pub fn new() -> Self {
        Self {
            snapshots: vec![SnapshotMetadata {
                id: "snapshot-0".to_string(),
                description: "Initial snapshot".to_string(),
                created_at: get_timestamp(),
                promoted_at: Some(get_timestamp()),
                overlay_count: 0,
                is_active: true,
            }],
            observations: Vec::new(),
            findings: Vec::new(),
            overlays: Vec::new(),
            validations: Vec::new(),
            active_snapshot_id: "snapshot-0".to_string(),
            mape_metrics: MAPEMetrics {
                observations_ingested: 0,
                findings_generated: 0,
                overlays_proposed: 0,
                overlays_promoted: 0,
                total_execution_time_ms: 0,
                last_execution_timestamp: 0,
            },
            compaction_policy: CompactionPolicy::default(),
            archived_data: HashMap::new(),
        }
    }

    /// Record observation
    pub fn record_observation(&mut self, obs: Observation) {
        self.observations.push(obs);
        self.mape_metrics.observations_ingested += 1;
    }

    /// Record finding
    pub fn record_finding(&mut self, finding: Finding) {
        self.findings.push(finding);
        self.mape_metrics.findings_generated += 1;
    }

    /// Record overlay proposal
    pub fn record_overlay(&mut self, overlay: OntologyOverlay) {
        self.overlays.push(overlay);
        self.mape_metrics.overlays_proposed += 1;
    }

    /// Record validation result
    pub fn record_validation(&mut self, result: ValidationResult) {
        self.validations.push(result);
    }

    /// Mark overlay as promoted (creates new snapshot)
    pub fn record_promotion(&mut self, overlay_id: String, new_snapshot_id: String) {
        // Mark overlay as promoted
        if let Some(overlay) = self.overlays.iter_mut().find(|o| o.id == overlay_id) {
            overlay.related_finding = Some("promoted".to_string());
        }

        // Deactivate old snapshot
        if let Some(old) = self.snapshots.iter_mut().find(|s| s.is_active) {
            old.is_active = false;
        }

        // Add new active snapshot
        self.snapshots.push(SnapshotMetadata {
            id: new_snapshot_id.clone(),
            description: format!("Snapshot after overlay: {}", overlay_id),
            created_at: get_timestamp(),
            promoted_at: Some(get_timestamp()),
            overlay_count: self.overlays.len(),
            is_active: true,
        });

        self.active_snapshot_id = new_snapshot_id;
        self.mape_metrics.overlays_promoted += 1;
    }

    /// Query findings by filter
    pub fn query_findings(&self, query: &HistoryQuery) -> Vec<&Finding> {
        self.findings
            .iter()
            .filter(|f| {
                // Component filter
                if let Some(ref comp) = query.component {
                    if f.component != *comp {
                        return false;
                    }
                }

                // Severity filter
                if let Some(ref severity) = query.min_severity {
                    if !self.severity_ge(&f.severity, severity) {
                        return false;
                    }
                }

                // Time range filter
                if let Some(from) = query.from_time {
                    if f.timestamp < from {
                        return false;
                    }
                }
                if let Some(to) = query.to_time {
                    if f.timestamp > to {
                        return false;
                    }
                }

                true
            })
            .take(query.limit)
            .collect()
    }

    /// Query observations by filter
    pub fn query_observations(&self, query: &HistoryQuery) -> Vec<&Observation> {
        self.observations
            .iter()
            .filter(|o| {
                // Time range filter
                if let Some(from) = query.from_time {
                    if o.timestamp < from {
                        return false;
                    }
                }
                if let Some(to) = query.to_time {
                    if o.timestamp > to {
                        return false;
                    }
                }

                true
            })
            .take(query.limit)
            .collect()
    }

    /// Get findings for a component
    pub fn findings_for_component(&self, component: &str) -> Vec<&Finding> {
        self.findings
            .iter()
            .filter(|f| f.component == component)
            .collect()
    }

    /// Get overlays for a snapshot
    pub fn overlays_for_snapshot(&self, snapshot_id: &str) -> Vec<&OntologyOverlay> {
        self.overlays
            .iter()
            .filter(|o| o.base_snapshot_id == snapshot_id)
            .collect()
    }

    /// Get snapshot history
    pub fn snapshot_history(&self) -> Vec<&SnapshotMetadata> {
        self.snapshots.iter().collect()
    }

    /// Get active snapshot
    pub fn active_snapshot(&self) -> Option<&SnapshotMetadata> {
        self.snapshots.iter().find(|s| s.is_active)
    }

    /// Get MAPE-K metrics
    pub fn mape_metrics(&self) -> &MAPEMetrics {
        &self.mape_metrics
    }

    /// Update MAPE-K execution time
    pub fn update_execution_time(&mut self, duration_ms: u64) {
        self.mape_metrics.total_execution_time_ms += duration_ms;
        self.mape_metrics.last_execution_timestamp = get_timestamp();
    }

    /// Set compaction policy
    pub fn set_compaction_policy(&mut self, policy: CompactionPolicy) {
        self.compaction_policy = policy;
    }

    /// Compact old data (archive and remove)
    pub fn compact(&mut self) -> CompactionResult {
        if !self.compaction_policy.enabled {
            return CompactionResult {
                archived_observations: 0,
                archived_findings: 0,
                archived_overlays: 0,
            };
        }

        let current_time = get_timestamp();
        let obs_cutoff =
            current_time - (self.compaction_policy.observation_retention_days as u64 * 86_400_000);
        let finding_cutoff =
            current_time - (self.compaction_policy.finding_retention_days as u64 * 86_400_000);

        let mut _archived_obs = 0;
        let mut _archived_findings = 0;

        // Archive old observations
        let (keep_obs, archive_obs): (Vec<_>, Vec<_>) = self
            .observations
            .drain(..)
            .partition(|o| o.timestamp > obs_cutoff);
        _archived_obs = archive_obs.len();
        self.observations = keep_obs;

        // Archive old findings
        let (keep_findings, archive_findings): (Vec<_>, Vec<_>) = self
            .findings
            .drain(..)
            .partition(|f| f.timestamp > finding_cutoff);
        _archived_findings = archive_findings.len();
        self.findings = keep_findings;

        // Store archive
        if !archive_obs.is_empty() {
            self.archived_data.insert(
                format!("observations-{}", current_time),
                serde_json::to_vec(&archive_obs).unwrap_or_default(),
            );
        }

        if !archive_findings.is_empty() {
            self.archived_data.insert(
                format!("findings-{}", current_time),
                serde_json::to_vec(&archive_findings).unwrap_or_default(),
            );
        }

        CompactionResult {
            archived_observations: _archived_obs,
            archived_findings: _archived_findings,
            archived_overlays: 0,
        }
    }

    /// Get all observations
    pub fn all_observations(&self) -> &[Observation] {
        &self.observations
    }

    /// Get all findings
    pub fn all_findings(&self) -> &[Finding] {
        &self.findings
    }

    /// Get all overlays
    pub fn all_overlays(&self) -> &[OntologyOverlay] {
        &self.overlays
    }

    /// Get statistics about stored data
    pub fn statistics(&self) -> KnowledgeStatistics {
        let findings_by_severity = self.findings.iter().fold(HashMap::new(), |mut acc, f| {
            *acc.entry(f.severity.clone()).or_insert(0) += 1;
            acc
        });

        let overlays_by_proposer = self.overlays.iter().fold(HashMap::new(), |mut acc, o| {
            let proposer = format!("{:?}", o.proposer);
            *acc.entry(proposer).or_insert(0) += 1;
            acc
        });

        KnowledgeStatistics {
            total_observations: self.observations.len(),
            total_findings: self.findings.len(),
            total_overlays: self.overlays.len(),
            findings_by_severity,
            overlays_by_proposer,
            snapshot_count: self.snapshots.len(),
            archived_records: self.archived_data.len(),
        }
    }

    /// Check severity ordering (Critical > High > Medium > Low)
    fn severity_ge(&self, a: &str, b: &str) -> bool {
        let severity_order = ["Critical", "High", "Medium", "Low"];
        let a_idx = severity_order.iter().position(|&s| s == a).unwrap_or(3);
        let b_idx = severity_order.iter().position(|&s| s == b).unwrap_or(3);
        a_idx <= b_idx
    }
}

impl Default for KnowledgeStore {
    fn default() -> Self {
        Self::new()
    }
}

/// Result of compaction operation
#[derive(Debug, Clone)]
pub struct CompactionResult {
    /// Number of observations archived
    pub archived_observations: usize,

    /// Number of findings archived
    pub archived_findings: usize,

    /// Number of overlays archived
    pub archived_overlays: usize,
}

/// Statistics about knowledge store contents
#[derive(Debug, Clone)]
pub struct KnowledgeStatistics {
    /// Total observations stored
    pub total_observations: usize,

    /// Total findings stored
    pub total_findings: usize,

    /// Total overlays stored
    pub total_overlays: usize,

    /// Count of findings by severity
    pub findings_by_severity: HashMap<String, usize>,

    /// Count of overlays by proposer type
    pub overlays_by_proposer: HashMap<String, usize>,

    /// Number of snapshots
    pub snapshot_count: usize,

    /// Number of archived records
    pub archived_records: usize,
}

/// Get current timestamp
fn get_timestamp() -> u64 {
    std::time::SystemTime::now()
        .duration_since(std::time::UNIX_EPOCH)
        .map(|d| d.as_millis() as u64)
        .unwrap_or(0)
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::mape_k::types::{FindingKind, ObservationType};

    #[test]
    fn test_create_knowledge_store() {
        let store = KnowledgeStore::new();
        assert_eq!(store.snapshot_history().len(), 1);
        assert_eq!(store.active_snapshot().unwrap().id, "snapshot-0");
    }

    #[test]
    fn test_record_observation() {
        let mut store = KnowledgeStore::new();
        let obs = Observation {
            id: "obs-1".to_string(),
            obs_type: ObservationType::Metric,
            timestamp: get_timestamp(),
            data: serde_json::json!({"value": 42}),
            source: "monitor".to_string(),
        };

        store.record_observation(obs);
        assert_eq!(store.all_observations().len(), 1);
        assert_eq!(store.mape_metrics().observations_ingested, 1);
    }

    #[test]
    fn test_record_finding() {
        let mut store = KnowledgeStore::new();
        let finding = Finding {
            id: "finding-1".to_string(),
            kind: FindingKind::TickBudgetViolation,
            severity: "High".to_string(),
            description: "Test finding".to_string(),
            component: "test-component".to_string(),
            evidence: vec![],
            suggested_action: None,
            timestamp: get_timestamp(),
            metadata: HashMap::new(),
        };

        store.record_finding(finding);
        assert_eq!(store.all_findings().len(), 1);
        assert_eq!(store.mape_metrics().findings_generated, 1);
    }

    #[test]
    fn test_query_findings_by_component() {
        let mut store = KnowledgeStore::new();
        let finding1 = Finding {
            id: "finding-1".to_string(),
            kind: FindingKind::GuardFailureRate,
            severity: "Medium".to_string(),
            description: "Test".to_string(),
            component: "comp1".to_string(),
            evidence: vec![],
            suggested_action: None,
            timestamp: get_timestamp(),
            metadata: HashMap::new(),
        };

        let finding2 = Finding {
            id: "finding-2".to_string(),
            kind: FindingKind::DriftDetected,
            severity: "Low".to_string(),
            description: "Test".to_string(),
            component: "comp2".to_string(),
            evidence: vec![],
            suggested_action: None,
            timestamp: get_timestamp(),
            metadata: HashMap::new(),
        };

        store.record_finding(finding1);
        store.record_finding(finding2);

        let results = store.findings_for_component("comp1");
        assert_eq!(results.len(), 1);
        assert_eq!(results[0].id, "finding-1");
    }

    #[test]
    fn test_statistics() {
        let mut store = KnowledgeStore::new();
        let finding = Finding {
            id: "finding-1".to_string(),
            kind: FindingKind::SLOBreach,
            severity: "Critical".to_string(),
            description: "Test".to_string(),
            component: "test".to_string(),
            evidence: vec![],
            suggested_action: None,
            timestamp: get_timestamp(),
            metadata: HashMap::new(),
        };

        store.record_finding(finding);
        let stats = store.statistics();
        assert_eq!(stats.total_findings, 1);
        assert_eq!(stats.findings_by_severity.get("Critical"), Some(&1));
    }

    #[test]
    fn test_compaction_disabled() {
        let mut store = KnowledgeStore::new();
        let result = store.compact();
        assert_eq!(result.archived_observations, 0);
        assert_eq!(result.archived_findings, 0);
    }

    #[test]
    fn test_active_snapshot() {
        let store = KnowledgeStore::new();
        let active = store.active_snapshot();
        assert!(active.is_some());
        assert!(active.unwrap().is_active);
    }

    #[test]
    fn test_severity_ordering() {
        let store = KnowledgeStore::new();
        assert!(store.severity_ge("Critical", "High"));
        assert!(store.severity_ge("High", "Medium"));
        assert!(store.severity_ge("Medium", "Low"));
        assert!(!store.severity_ge("Low", "Medium"));
    }

    #[test]
    fn test_record_promotion() {
        let mut store = KnowledgeStore::new();
        let old_snapshot_id = store.active_snapshot().unwrap().id.clone();

        store.record_promotion("overlay-001".to_string(), "snapshot-1".to_string());

        assert_eq!(store.snapshot_history().len(), 2);
        assert_eq!(store.active_snapshot().unwrap().id, "snapshot-1");
        assert_eq!(store.mape_metrics().overlays_promoted, 1);

        // Old snapshot should be inactive
        let snapshot_history = store.snapshot_history();
        let old_snapshot = snapshot_history.iter().find(|s| s.id == old_snapshot_id);
        assert!(old_snapshot.is_some());
        assert!(!old_snapshot.unwrap().is_active);
    }
}
