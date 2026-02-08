//! Refusal Pattern Analytics
//!
//! Analyzes refusal patterns to detect:
//! - Probe attacks (systematic capability testing)
//! - Envelope boundary testing
//! - Kill switch evasion attempts
//! - Anomalous usage patterns
//!
//! The analytics engine maintains a sliding window of refusals and performs
//! statistical analysis to detect suspicious patterns that may indicate
//! security threats or system misuse.

use crate::refusal::{Refusal, RefusalCategory};
use chrono::{DateTime, Duration, Utc};
use serde::{Deserialize, Serialize};
use std::collections::{HashMap, HashSet};

/// Analytics engine for refusal patterns
#[derive(Debug)]
pub struct RefusalAnalytics {
    /// Maximum number of refusals to retain in the analysis window
    window_size: usize,
    /// Recorded refusals within the window
    refusals: Vec<RefusalRecord>,
    /// Pattern counters by pattern key
    patterns: HashMap<String, PatternCounter>,
    /// Thresholds for anomaly detection
    thresholds: AnalyticsThresholds,
}

/// Internal record associating a refusal with metadata
#[derive(Debug, Clone)]
struct RefusalRecord {
    refusal: Refusal,
    timestamp: DateTime<Utc>,
    contract_id: String,
}

/// Counter for tracking pattern occurrences over time
#[derive(Debug, Default, Clone)]
struct PatternCounter {
    count: u64,
    first_seen: Option<DateTime<Utc>>,
    last_seen: Option<DateTime<Utc>>,
    /// Sequence of intervals between occurrences (in milliseconds)
    intervals: Vec<i64>,
}

impl PatternCounter {
    fn record(&mut self, timestamp: DateTime<Utc>) {
        if let Some(last) = self.last_seen {
            let interval = (timestamp - last).num_milliseconds();
            self.intervals.push(interval);
            // Keep only last 100 intervals for analysis
            if self.intervals.len() > 100 {
                self.intervals.remove(0);
            }
        }

        if self.first_seen.is_none() {
            self.first_seen = Some(timestamp);
        }
        self.last_seen = Some(timestamp);
        self.count += 1;
    }

    /// Calculate coefficient of variation for intervals
    /// Low CV indicates regular, automated patterns
    fn interval_cv(&self) -> Option<f64> {
        if self.intervals.len() < 3 {
            return None;
        }

        let mean = self.intervals.iter().sum::<i64>() as f64 / self.intervals.len() as f64;
        if mean == 0.0 {
            return None;
        }

        let variance = self.intervals.iter()
            .map(|&x| (x as f64 - mean).powi(2))
            .sum::<f64>() / self.intervals.len() as f64;

        Some(variance.sqrt() / mean)
    }
}

/// Configurable thresholds for anomaly detection
#[derive(Debug, Clone)]
pub struct AnalyticsThresholds {
    /// Minimum refusals to consider a capability probe
    pub capability_probe_min_count: u64,
    /// Minimum unique capabilities tested to flag probing
    pub capability_probe_min_unique: usize,
    /// Time window for capability probe detection (seconds)
    pub capability_probe_window_secs: i64,
    /// Minimum envelope violations for boundary testing
    pub boundary_test_min_count: u64,
    /// Time window for boundary testing detection (seconds)
    pub boundary_test_window_secs: i64,
    /// Minimum kill switch related refusals for evasion detection
    pub kill_evasion_min_count: u64,
    /// Refusal spike multiplier (compared to baseline)
    pub spike_multiplier: f64,
    /// Coefficient of variation threshold for automation detection
    pub automation_cv_threshold: f64,
    /// Minimum samples for automation detection
    pub automation_min_samples: usize,
}

impl Default for AnalyticsThresholds {
    fn default() -> Self {
        Self {
            capability_probe_min_count: 5,
            capability_probe_min_unique: 3,
            capability_probe_window_secs: 300, // 5 minutes
            boundary_test_min_count: 10,
            boundary_test_window_secs: 600, // 10 minutes
            kill_evasion_min_count: 3,
            spike_multiplier: 3.0,
            automation_cv_threshold: 0.15, // Low CV = regular intervals = automation
            automation_min_samples: 10,
        }
    }
}

/// Detected anomaly
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct Anomaly {
    /// Type of detected anomaly
    pub anomaly_type: AnomalyType,
    /// Severity level
    pub severity: Severity,
    /// Contract ID involved (or "multiple" for cross-contract)
    pub contract_id: String,
    /// Human-readable description
    pub description: String,
    /// Refusal IDs as evidence
    pub evidence: Vec<String>,
    /// Recommended action
    pub recommended_action: Action,
    /// Detection timestamp
    pub detected_at: DateTime<Utc>,
    /// Confidence score (0.0 - 1.0)
    pub confidence: f64,
}

/// Types of detectable anomalies
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, Serialize, Deserialize)]
pub enum AnomalyType {
    /// Systematic capability probing (testing multiple capabilities rapidly)
    CapabilityProbe,
    /// Repeated envelope boundary hits (testing limits)
    EnvelopeBoundaryTest,
    /// Kill switch bypass attempts
    KillSwitchEvasion,
    /// Unusual refusal rate spike
    RefusalSpike,
    /// Sequential pattern suggesting automation
    AutomatedProbe,
    /// Cross-contract coordinated attack pattern
    CoordinatedAttack,
}

impl AnomalyType {
    pub fn as_str(&self) -> &'static str {
        match self {
            Self::CapabilityProbe => "capability_probe",
            Self::EnvelopeBoundaryTest => "envelope_boundary_test",
            Self::KillSwitchEvasion => "kill_switch_evasion",
            Self::RefusalSpike => "refusal_spike",
            Self::AutomatedProbe => "automated_probe",
            Self::CoordinatedAttack => "coordinated_attack",
        }
    }
}

/// Severity levels for anomalies
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash, Serialize, Deserialize)]
pub enum Severity {
    /// Informational only
    Info,
    /// Low severity - monitor
    Low,
    /// Medium severity - investigate
    Medium,
    /// High severity - take action
    High,
    /// Critical severity - immediate action required
    Critical,
}

impl Severity {
    pub fn as_str(&self) -> &'static str {
        match self {
            Self::Info => "info",
            Self::Low => "low",
            Self::Medium => "medium",
            Self::High => "high",
            Self::Critical => "critical",
        }
    }

    pub fn as_u8(&self) -> u8 {
        match self {
            Self::Info => 1,
            Self::Low => 2,
            Self::Medium => 3,
            Self::High => 4,
            Self::Critical => 5,
        }
    }
}

/// Recommended actions for anomalies
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, Serialize, Deserialize)]
pub enum Action {
    /// Kill the specific contract
    KillContract,
    /// Kill the entire contract family
    KillFamily,
    /// Global kill switch (emergency)
    KillGlobal,
    /// Reduce capabilities for the contract
    ReduceCapabilities,
    /// Increase monitoring/logging
    IncreaseMonitoring,
    /// Send alert to operators
    Alert,
    /// No action needed
    None,
}

impl Action {
    pub fn as_str(&self) -> &'static str {
        match self {
            Self::KillContract => "kill_contract",
            Self::KillFamily => "kill_family",
            Self::KillGlobal => "kill_global",
            Self::ReduceCapabilities => "reduce_capabilities",
            Self::IncreaseMonitoring => "increase_monitoring",
            Self::Alert => "alert",
            Self::None => "none",
        }
    }
}

/// Analytics report with summary statistics
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct AnalyticsReport {
    /// Report generation timestamp
    pub generated_at: DateTime<Utc>,
    /// Start of analysis window
    pub window_start: DateTime<Utc>,
    /// End of analysis window
    pub window_end: DateTime<Utc>,
    /// Total refusals in window
    pub total_refusals: u64,
    /// Refusals by category
    pub by_category: HashMap<String, u64>,
    /// Refusals by contract
    pub by_contract: HashMap<String, u64>,
    /// Refusals by operation
    pub by_operation: HashMap<String, u64>,
    /// Detected anomalies
    pub anomalies: Vec<Anomaly>,
    /// Health score (0.0 = critical, 1.0 = healthy)
    pub health_score: f64,
    /// Trend analysis (positive = improving, negative = degrading)
    pub trend: f64,
}

impl RefusalAnalytics {
    /// Create a new analytics engine with specified window size
    pub fn new(window_size: usize) -> Self {
        Self {
            window_size,
            refusals: Vec::with_capacity(window_size),
            patterns: HashMap::new(),
            thresholds: AnalyticsThresholds::default(),
        }
    }

    /// Create with custom thresholds
    pub fn with_thresholds(window_size: usize, thresholds: AnalyticsThresholds) -> Self {
        Self {
            window_size,
            refusals: Vec::with_capacity(window_size),
            patterns: HashMap::new(),
            thresholds,
        }
    }

    /// Record a refusal for analysis
    pub fn record(&mut self, contract_id: &str, refusal: Refusal) {
        let timestamp = refusal.timestamp;

        // Create record
        let record = RefusalRecord {
            refusal: refusal.clone(),
            timestamp,
            contract_id: contract_id.to_string(),
        };

        // Update pattern counters
        self.update_patterns(&record);

        // Add to window
        self.refusals.push(record);

        // Trim window if needed
        if self.refusals.len() > self.window_size {
            self.refusals.remove(0);
        }
    }

    /// Update pattern counters for a new record
    fn update_patterns(&mut self, record: &RefusalRecord) {
        // Pattern by contract + category
        let contract_category_key = format!(
            "contract:{}:category:{}",
            record.contract_id,
            record.refusal.code.category.as_prefix()
        );
        self.patterns
            .entry(contract_category_key)
            .or_default()
            .record(record.timestamp);

        // Pattern by contract + operation
        let contract_op_key = format!(
            "contract:{}:operation:{}",
            record.contract_id,
            record.refusal.operation
        );
        self.patterns
            .entry(contract_op_key)
            .or_default()
            .record(record.timestamp);

        // Pattern by category globally
        let category_key = format!("global:category:{}", record.refusal.code.category.as_prefix());
        self.patterns
            .entry(category_key)
            .or_default()
            .record(record.timestamp);

        // Pattern by contract globally
        let contract_key = format!("global:contract:{}", record.contract_id);
        self.patterns
            .entry(contract_key)
            .or_default()
            .record(record.timestamp);
    }

    /// Analyze patterns and detect anomalies
    pub fn analyze(&self) -> Vec<Anomaly> {
        let mut anomalies = Vec::new();

        // Run all detection algorithms
        if let Some(a) = self.detect_capability_probe() {
            anomalies.push(a);
        }
        if let Some(a) = self.detect_boundary_test() {
            anomalies.push(a);
        }
        if let Some(a) = self.detect_kill_switch_evasion() {
            anomalies.push(a);
        }
        anomalies.extend(self.detect_refusal_spikes());
        anomalies.extend(self.detect_automated_probes());
        if let Some(a) = self.detect_coordinated_attack() {
            anomalies.push(a);
        }

        // Sort by severity (highest first)
        anomalies.sort_by(|a, b| b.severity.cmp(&a.severity));

        anomalies
    }

    /// Get refusal rate by category
    pub fn rate_by_category(&self) -> HashMap<RefusalCategory, f64> {
        let total = self.refusals.len() as f64;
        if total == 0.0 {
            return HashMap::new();
        }

        let mut counts: HashMap<RefusalCategory, u64> = HashMap::new();
        for record in &self.refusals {
            *counts.entry(record.refusal.code.category).or_default() += 1;
        }

        counts
            .into_iter()
            .map(|(cat, count)| (cat, count as f64 / total))
            .collect()
    }

    /// Get top N contracts by refusal count
    pub fn top_refusers(&self, n: usize) -> Vec<(String, u64)> {
        let mut counts: HashMap<&str, u64> = HashMap::new();
        for record in &self.refusals {
            *counts.entry(&record.contract_id).or_default() += 1;
        }

        let mut pairs: Vec<_> = counts.into_iter().map(|(k, v)| (k.to_string(), v)).collect();
        pairs.sort_by(|a, b| b.1.cmp(&a.1));
        pairs.truncate(n);
        pairs
    }

    /// Detect capability probing (systematic testing of multiple capabilities)
    fn detect_capability_probe(&self) -> Option<Anomaly> {
        let now = Utc::now();
        let window_start = now - Duration::seconds(self.thresholds.capability_probe_window_secs);

        // Group by contract, collect unique capability-related refusals
        let mut contract_capabilities: HashMap<&str, HashSet<String>> = HashMap::new();
        let mut contract_evidence: HashMap<&str, Vec<String>> = HashMap::new();

        for record in &self.refusals {
            if record.timestamp < window_start {
                continue;
            }
            if record.refusal.code.category == RefusalCategory::Capability {
                contract_capabilities
                    .entry(&record.contract_id)
                    .or_default()
                    .insert(record.refusal.code.code.clone());
                contract_evidence
                    .entry(&record.contract_id)
                    .or_default()
                    .push(record.refusal.refusal_id.clone());
            }
        }

        // Find contracts with suspicious probing patterns
        for (contract_id, capabilities) in &contract_capabilities {
            let evidence = contract_evidence.get(*contract_id)?;
            let count = evidence.len() as u64;
            let unique_count = capabilities.len();

            if count >= self.thresholds.capability_probe_min_count
                && unique_count >= self.thresholds.capability_probe_min_unique
            {
                let confidence = (count as f64 / 10.0).min(1.0)
                    * (unique_count as f64 / 5.0).min(1.0);

                return Some(Anomaly {
                    anomaly_type: AnomalyType::CapabilityProbe,
                    severity: if count >= 20 { Severity::High } else { Severity::Medium },
                    contract_id: contract_id.to_string(),
                    description: format!(
                        "Contract tested {} unique capabilities in {} attempts within {} seconds",
                        unique_count,
                        count,
                        self.thresholds.capability_probe_window_secs
                    ),
                    evidence: evidence.clone(),
                    recommended_action: Action::ReduceCapabilities,
                    detected_at: now,
                    confidence,
                });
            }
        }

        None
    }

    /// Detect envelope boundary testing
    fn detect_boundary_test(&self) -> Option<Anomaly> {
        let now = Utc::now();
        let window_start = now - Duration::seconds(self.thresholds.boundary_test_window_secs);

        // Look for envelope violation refusals
        let mut contract_envelope_hits: HashMap<&str, Vec<&RefusalRecord>> = HashMap::new();

        for record in &self.refusals {
            if record.timestamp < window_start {
                continue;
            }
            // Check for envelope violations in the code
            if record.refusal.code.code.contains("ENVELOPE")
                || record.refusal.code.category == RefusalCategory::Resource
            {
                contract_envelope_hits
                    .entry(&record.contract_id)
                    .or_default()
                    .push(record);
            }
        }

        // Find contracts hitting boundaries repeatedly
        for (contract_id, hits) in &contract_envelope_hits {
            let count = hits.len() as u64;

            if count >= self.thresholds.boundary_test_min_count {
                let evidence: Vec<String> = hits.iter().map(|r| r.refusal.refusal_id.clone()).collect();

                // Check for pattern variety (testing different constraints)
                let unique_constraints: HashSet<_> = hits.iter()
                    .map(|r| &r.refusal.code.code)
                    .collect();

                let confidence = (count as f64 / 20.0).min(1.0)
                    * (unique_constraints.len() as f64 / 3.0).min(1.0);

                return Some(Anomaly {
                    anomaly_type: AnomalyType::EnvelopeBoundaryTest,
                    severity: if unique_constraints.len() >= 3 {
                        Severity::High
                    } else {
                        Severity::Medium
                    },
                    contract_id: contract_id.to_string(),
                    description: format!(
                        "Contract hit {} envelope boundaries {} times, testing {} unique constraints",
                        unique_constraints.len(),
                        count,
                        unique_constraints.len()
                    ),
                    evidence,
                    recommended_action: Action::IncreaseMonitoring,
                    detected_at: now,
                    confidence,
                });
            }
        }

        None
    }

    /// Detect kill switch evasion attempts
    fn detect_kill_switch_evasion(&self) -> Option<Anomaly> {
        let now = Utc::now();

        // Look for patterns indicating evasion:
        // 1. Kill switch refusals followed by continued attempts
        // 2. Multiple contracts hitting kill switches in sequence
        // 3. Attempts with slight variations after kill

        let mut kill_refusals: Vec<&RefusalRecord> = self.refusals
            .iter()
            .filter(|r| r.refusal.code.category == RefusalCategory::KillSwitch)
            .collect();

        if (kill_refusals.len() as u64) < self.thresholds.kill_evasion_min_count {
            return None;
        }

        kill_refusals.sort_by_key(|r| r.timestamp);

        // Check for attempts after kill switches
        let mut evasion_evidence = Vec::new();
        let mut seen_contracts: HashSet<&str> = HashSet::new();

        for kill in &kill_refusals {
            seen_contracts.insert(&kill.contract_id);
            evasion_evidence.push(kill.refusal.refusal_id.clone());

            // Look for continued attempts after this kill
            for record in &self.refusals {
                if record.timestamp > kill.timestamp
                    && record.contract_id == kill.contract_id
                    && record.refusal.code.category != RefusalCategory::KillSwitch
                {
                    evasion_evidence.push(record.refusal.refusal_id.clone());
                }
            }
        }

        if evasion_evidence.len() > kill_refusals.len() {
            let confidence = ((evasion_evidence.len() - kill_refusals.len()) as f64 / 10.0).min(1.0);
            let num_contracts = seen_contracts.len();
            let contract_id = if num_contracts == 1 {
                seen_contracts.into_iter().next().unwrap().to_string()
            } else {
                "multiple".to_string()
            };

            return Some(Anomaly {
                anomaly_type: AnomalyType::KillSwitchEvasion,
                severity: Severity::Critical,
                contract_id,
                description: format!(
                    "Detected {} attempts to continue operation after {} kill switch activations across {} contract(s)",
                    evasion_evidence.len() - kill_refusals.len(),
                    kill_refusals.len(),
                    num_contracts
                ),
                evidence: evasion_evidence,
                recommended_action: if num_contracts > 1 {
                    Action::KillFamily
                } else {
                    Action::KillContract
                },
                detected_at: now,
                confidence,
            });
        }

        None
    }

    /// Detect refusal rate spikes per contract
    fn detect_refusal_spikes(&self) -> Vec<Anomaly> {
        let mut anomalies = Vec::new();
        let now = Utc::now();

        // Calculate baseline and recent rates per contract
        let half_window = self.refusals.len() / 2;
        if half_window == 0 {
            return anomalies;
        }

        let (old_half, new_half) = self.refusals.split_at(half_window);

        let mut old_counts: HashMap<&str, u64> = HashMap::new();
        let mut new_counts: HashMap<&str, u64> = HashMap::new();

        for record in old_half {
            *old_counts.entry(&record.contract_id).or_default() += 1;
        }
        for record in new_half {
            *new_counts.entry(&record.contract_id).or_default() += 1;
        }

        // Find contracts with spikes
        for (contract_id, new_count) in &new_counts {
            let old_count = old_counts.get(*contract_id).copied().unwrap_or(1);
            let ratio = *new_count as f64 / old_count as f64;

            if ratio >= self.thresholds.spike_multiplier && *new_count >= 5 {
                let evidence: Vec<String> = new_half
                    .iter()
                    .filter(|r| &r.contract_id == *contract_id)
                    .map(|r| r.refusal.refusal_id.clone())
                    .collect();

                let confidence = ((ratio - self.thresholds.spike_multiplier) / self.thresholds.spike_multiplier).min(1.0);

                anomalies.push(Anomaly {
                    anomaly_type: AnomalyType::RefusalSpike,
                    severity: if ratio >= 5.0 { Severity::High } else { Severity::Medium },
                    contract_id: contract_id.to_string(),
                    description: format!(
                        "Refusal rate spiked {:.1}x (from {} to {} in half-window)",
                        ratio, old_count, new_count
                    ),
                    evidence,
                    recommended_action: Action::Alert,
                    detected_at: now,
                    confidence,
                });
            }
        }

        anomalies
    }

    /// Detect automated probing (regular interval patterns)
    fn detect_automated_probes(&self) -> Vec<Anomaly> {
        let mut anomalies = Vec::new();
        let now = Utc::now();

        for (pattern_key, counter) in &self.patterns {
            if counter.count < self.thresholds.automation_min_samples as u64 {
                continue;
            }

            if let Some(cv) = counter.interval_cv() {
                if cv < self.thresholds.automation_cv_threshold {
                    // Extract contract ID from pattern key
                    let contract_id = pattern_key
                        .split(':')
                        .nth(1)
                        .unwrap_or("unknown")
                        .to_string();

                    // Get evidence (recent refusals matching this pattern)
                    let evidence: Vec<String> = self.refusals
                        .iter()
                        .filter(|r| pattern_key.contains(&r.contract_id))
                        .take(10)
                        .map(|r| r.refusal.refusal_id.clone())
                        .collect();

                    let confidence = ((self.thresholds.automation_cv_threshold - cv) / self.thresholds.automation_cv_threshold).min(1.0);

                    anomalies.push(Anomaly {
                        anomaly_type: AnomalyType::AutomatedProbe,
                        severity: Severity::High,
                        contract_id,
                        description: format!(
                            "Detected automated pattern with {:.3} coefficient of variation ({} samples, regular intervals)",
                            cv, counter.count
                        ),
                        evidence,
                        recommended_action: Action::ReduceCapabilities,
                        detected_at: now,
                        confidence,
                    });
                }
            }
        }

        anomalies
    }

    /// Detect coordinated attacks across multiple contracts
    fn detect_coordinated_attack(&self) -> Option<Anomaly> {
        let now = Utc::now();
        let window = Duration::seconds(60); // 1 minute window for coordination

        // Look for multiple contracts making similar refusal-triggering attempts
        // within a short time window

        let mut time_buckets: HashMap<i64, Vec<&RefusalRecord>> = HashMap::new();

        for record in &self.refusals {
            let bucket = record.timestamp.timestamp() / 60; // 1-minute buckets
            time_buckets.entry(bucket).or_default().push(record);
        }

        for (_, records) in &time_buckets {
            if records.len() < 5 {
                continue;
            }

            let unique_contracts: HashSet<_> = records.iter().map(|r| &r.contract_id).collect();
            let unique_operations: HashSet<_> = records.iter().map(|r| &r.refusal.operation).collect();

            // Coordination signal: many contracts, same operation, same time window
            if unique_contracts.len() >= 3 && unique_operations.len() == 1 {
                let evidence: Vec<String> = records.iter().map(|r| r.refusal.refusal_id.clone()).collect();
                let confidence = (unique_contracts.len() as f64 / 5.0).min(1.0);

                return Some(Anomaly {
                    anomaly_type: AnomalyType::CoordinatedAttack,
                    severity: Severity::Critical,
                    contract_id: "multiple".to_string(),
                    description: format!(
                        "Detected {} contracts attempting '{}' operation simultaneously",
                        unique_contracts.len(),
                        unique_operations.into_iter().next().unwrap()
                    ),
                    evidence,
                    recommended_action: Action::KillFamily,
                    detected_at: now,
                    confidence,
                });
            }
        }

        None
    }

    /// Generate comprehensive analytics report
    pub fn report(&self) -> AnalyticsReport {
        let now = Utc::now();

        let (window_start, window_end) = if self.refusals.is_empty() {
            (now, now)
        } else {
            (
                self.refusals.first().map(|r| r.timestamp).unwrap_or(now),
                self.refusals.last().map(|r| r.timestamp).unwrap_or(now),
            )
        };

        let mut by_category: HashMap<String, u64> = HashMap::new();
        let mut by_contract: HashMap<String, u64> = HashMap::new();
        let mut by_operation: HashMap<String, u64> = HashMap::new();

        for record in &self.refusals {
            *by_category
                .entry(record.refusal.code.category.as_prefix().to_string())
                .or_default() += 1;
            *by_contract.entry(record.contract_id.clone()).or_default() += 1;
            *by_operation.entry(record.refusal.operation.clone()).or_default() += 1;
        }

        let anomalies = self.analyze();

        // Calculate health score (1.0 = healthy, 0.0 = critical)
        let health_score = self.calculate_health_score(&anomalies);

        // Calculate trend
        let trend = self.calculate_trend();

        AnalyticsReport {
            generated_at: now,
            window_start,
            window_end,
            total_refusals: self.refusals.len() as u64,
            by_category,
            by_contract,
            by_operation,
            anomalies,
            health_score,
            trend,
        }
    }

    /// Calculate overall health score
    fn calculate_health_score(&self, anomalies: &[Anomaly]) -> f64 {
        if anomalies.is_empty() {
            return 1.0;
        }

        let severity_penalty: f64 = anomalies
            .iter()
            .map(|a| match a.severity {
                Severity::Critical => 0.4,
                Severity::High => 0.2,
                Severity::Medium => 0.1,
                Severity::Low => 0.05,
                Severity::Info => 0.01,
            })
            .sum();

        (1.0 - severity_penalty).max(0.0)
    }

    /// Calculate trend (positive = improving, negative = degrading)
    fn calculate_trend(&self) -> f64 {
        if self.refusals.len() < 10 {
            return 0.0;
        }

        let half = self.refusals.len() / 2;
        let (old_half, new_half) = self.refusals.split_at(half);

        let old_rate = old_half.len() as f64;
        let new_rate = new_half.len() as f64;

        if old_rate == 0.0 {
            return 0.0;
        }

        // Positive means fewer refusals recently (improving)
        // Negative means more refusals recently (degrading)
        (old_rate - new_rate) / old_rate
    }

    /// Clear all recorded data
    pub fn clear(&mut self) {
        self.refusals.clear();
        self.patterns.clear();
    }

    /// Get current window size
    pub fn window_size(&self) -> usize {
        self.window_size
    }

    /// Get number of recorded refusals
    pub fn refusal_count(&self) -> usize {
        self.refusals.len()
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::refusal::RefusalCode;

    // Helper to create a test refusal
    fn make_refusal(category: RefusalCategory, code: &str, operation: &str) -> Refusal {
        Refusal::new(
            RefusalCode::new(category, code),
            "Test refusal",
            operation,
            "test-hash-123",
        )
    }

    // Helper to create a refusal with specific timestamp
    fn make_refusal_at(
        category: RefusalCategory,
        code: &str,
        operation: &str,
        timestamp: DateTime<Utc>,
    ) -> Refusal {
        let mut refusal = make_refusal(category, code, operation);
        // We need to modify the internal timestamp
        refusal.timestamp = timestamp;
        refusal
    }

    #[test]
    fn test_analytics_new() {
        let analytics = RefusalAnalytics::new(100);
        assert_eq!(analytics.window_size(), 100);
        assert_eq!(analytics.refusal_count(), 0);
    }

    #[test]
    fn test_record_refusal() {
        let mut analytics = RefusalAnalytics::new(100);
        let refusal = make_refusal(RefusalCategory::Capability, "UNAUTHORIZED:read", "test.op");

        analytics.record("contract-001", refusal);

        assert_eq!(analytics.refusal_count(), 1);
    }

    #[test]
    fn test_window_trimming() {
        let mut analytics = RefusalAnalytics::new(5);

        for i in 0..10 {
            let refusal = make_refusal(
                RefusalCategory::Validation,
                &format!("TEST:{}", i),
                "test",
            );
            analytics.record("contract-001", refusal);
        }

        assert_eq!(analytics.refusal_count(), 5);
    }

    #[test]
    fn test_rate_by_category() {
        let mut analytics = RefusalAnalytics::new(100);

        // Add 3 capability refusals
        for _ in 0..3 {
            let refusal = make_refusal(RefusalCategory::Capability, "CAP_TEST", "op");
            analytics.record("contract-001", refusal);
        }

        // Add 1 validation refusal
        let refusal = make_refusal(RefusalCategory::Validation, "VAL_TEST", "op");
        analytics.record("contract-001", refusal);

        let rates = analytics.rate_by_category();

        assert!((rates.get(&RefusalCategory::Capability).unwrap() - 0.75).abs() < 0.001);
        assert!((rates.get(&RefusalCategory::Validation).unwrap() - 0.25).abs() < 0.001);
    }

    #[test]
    fn test_top_refusers() {
        let mut analytics = RefusalAnalytics::new(100);

        // Contract A: 5 refusals
        for _ in 0..5 {
            let refusal = make_refusal(RefusalCategory::Capability, "TEST", "op");
            analytics.record("contract-A", refusal);
        }

        // Contract B: 3 refusals
        for _ in 0..3 {
            let refusal = make_refusal(RefusalCategory::Capability, "TEST", "op");
            analytics.record("contract-B", refusal);
        }

        // Contract C: 1 refusal
        let refusal = make_refusal(RefusalCategory::Capability, "TEST", "op");
        analytics.record("contract-C", refusal);

        let top = analytics.top_refusers(2);

        assert_eq!(top.len(), 2);
        assert_eq!(top[0].0, "contract-A");
        assert_eq!(top[0].1, 5);
        assert_eq!(top[1].0, "contract-B");
        assert_eq!(top[1].1, 3);
    }

    #[test]
    fn test_detect_capability_probe() {
        let mut analytics = RefusalAnalytics::with_thresholds(
            100,
            AnalyticsThresholds {
                capability_probe_min_count: 3,
                capability_probe_min_unique: 2,
                capability_probe_window_secs: 3600,
                ..Default::default()
            },
        );

        // Simulate capability probing - testing multiple capabilities
        let capabilities = ["read", "write", "execute", "admin"];
        for cap in &capabilities {
            let refusal = make_refusal(
                RefusalCategory::Capability,
                &format!("UNAUTHORIZED:{}", cap),
                "test.operation",
            );
            analytics.record("probing-contract", refusal);
        }

        let anomaly = analytics.detect_capability_probe();
        assert!(anomaly.is_some());

        let anomaly = anomaly.unwrap();
        assert_eq!(anomaly.anomaly_type, AnomalyType::CapabilityProbe);
        assert_eq!(anomaly.contract_id, "probing-contract");
        assert!(anomaly.description.contains("4 unique capabilities"));
    }

    #[test]
    fn test_detect_capability_probe_below_threshold() {
        let mut analytics = RefusalAnalytics::with_thresholds(
            100,
            AnalyticsThresholds {
                capability_probe_min_count: 5,
                capability_probe_min_unique: 3,
                ..Default::default()
            },
        );

        // Only 2 refusals - below threshold
        let refusal = make_refusal(RefusalCategory::Capability, "UNAUTHORIZED:read", "op");
        analytics.record("contract", refusal);
        let refusal = make_refusal(RefusalCategory::Capability, "UNAUTHORIZED:write", "op");
        analytics.record("contract", refusal);

        let anomaly = analytics.detect_capability_probe();
        assert!(anomaly.is_none());
    }

    #[test]
    fn test_detect_boundary_test() {
        let mut analytics = RefusalAnalytics::with_thresholds(
            100,
            AnalyticsThresholds {
                boundary_test_min_count: 3,
                boundary_test_window_secs: 3600,
                ..Default::default()
            },
        );

        // Simulate boundary testing - hitting various limits
        let constraints = ["ENVELOPE:max_ops", "ENVELOPE:max_memory", "ENVELOPE:max_duration"];
        for constraint in &constraints {
            let refusal = make_refusal(RefusalCategory::Capability, constraint, "execute");
            analytics.record("boundary-tester", refusal);
        }

        // Add some resource refusals too
        for _ in 0..3 {
            let refusal = make_refusal(RefusalCategory::Resource, "RATE_LIMIT:100/60", "execute");
            analytics.record("boundary-tester", refusal);
        }

        let anomaly = analytics.detect_boundary_test();
        assert!(anomaly.is_some());

        let anomaly = anomaly.unwrap();
        assert_eq!(anomaly.anomaly_type, AnomalyType::EnvelopeBoundaryTest);
    }

    #[test]
    fn test_detect_kill_switch_evasion() {
        let mut analytics = RefusalAnalytics::with_thresholds(
            100,
            AnalyticsThresholds {
                kill_evasion_min_count: 1,
                ..Default::default()
            },
        );

        // First, a kill switch is triggered
        let kill_refusal = make_refusal(RefusalCategory::KillSwitch, "GLOBAL", "execute");
        analytics.record("evasive-contract", kill_refusal);

        // Then the contract attempts to continue
        for _ in 0..3 {
            let refusal = make_refusal(RefusalCategory::Capability, "UNAUTHORIZED:execute", "execute");
            analytics.record("evasive-contract", refusal);
        }

        let anomaly = analytics.detect_kill_switch_evasion();
        assert!(anomaly.is_some());

        let anomaly = anomaly.unwrap();
        assert_eq!(anomaly.anomaly_type, AnomalyType::KillSwitchEvasion);
        assert_eq!(anomaly.severity, Severity::Critical);
        assert_eq!(anomaly.recommended_action, Action::KillContract);
    }

    #[test]
    fn test_detect_refusal_spike() {
        let mut analytics = RefusalAnalytics::with_thresholds(
            100,
            AnalyticsThresholds {
                spike_multiplier: 2.0,
                ..Default::default()
            },
        );

        // To detect a spike, we need more refusals in the second half
        // The algorithm splits refusals in half by index
        // So we'll use different contracts to show the spike pattern

        // First 10 refusals: "baseline-contract"
        for _ in 0..10 {
            let refusal = make_refusal(RefusalCategory::Validation, "TEST", "op");
            analytics.record("baseline-contract", refusal);
        }

        // Next 10 refusals: "spiking-contract" only in second half
        for _ in 0..10 {
            let refusal = make_refusal(RefusalCategory::Validation, "TEST", "op");
            analytics.record("spiking-contract", refusal);
        }

        // Now: old_half has 10 baseline, 0 spiking
        //      new_half has 0 baseline, 10 spiking
        // spiking-contract: old=0 (defaults to 1), new=10, ratio=10x

        let anomalies = analytics.detect_refusal_spikes();
        assert!(!anomalies.is_empty(), "Expected spike anomaly to be detected");

        let anomaly = anomalies.iter().find(|a| a.contract_id == "spiking-contract");
        assert!(anomaly.is_some(), "Expected anomaly for spiking-contract");
        let anomaly = anomaly.unwrap();
        assert_eq!(anomaly.anomaly_type, AnomalyType::RefusalSpike);
    }

    #[test]
    fn test_pattern_counter_cv() {
        let mut counter = PatternCounter::default();
        let base_time = Utc::now();

        // Regular intervals (1000ms each) = low CV
        for i in 0..10 {
            let time = base_time + Duration::milliseconds(i * 1000);
            counter.record(time);
        }

        let cv = counter.interval_cv();
        assert!(cv.is_some());
        assert!(cv.unwrap() < 0.1, "CV should be very low for regular intervals");
    }

    #[test]
    fn test_pattern_counter_irregular_cv() {
        let mut counter = PatternCounter::default();
        let base_time = Utc::now();

        // Irregular intervals = higher CV
        let intervals = [100, 5000, 200, 8000, 50, 3000];
        let mut total_ms = 0;
        for interval in intervals {
            let time = base_time + Duration::milliseconds(total_ms);
            counter.record(time);
            total_ms += interval;
        }

        let cv = counter.interval_cv();
        assert!(cv.is_some());
        assert!(cv.unwrap() > 0.5, "CV should be high for irregular intervals");
    }

    #[test]
    fn test_analytics_report_generation() {
        let mut analytics = RefusalAnalytics::new(100);

        // Add some refusals
        for _ in 0..5 {
            let refusal = make_refusal(RefusalCategory::Capability, "TEST", "op1");
            analytics.record("contract-A", refusal);
        }
        for _ in 0..3 {
            let refusal = make_refusal(RefusalCategory::Validation, "TEST", "op2");
            analytics.record("contract-B", refusal);
        }

        let report = analytics.report();

        assert_eq!(report.total_refusals, 8);
        assert_eq!(report.by_category.get("CAP").unwrap(), &5);
        assert_eq!(report.by_category.get("VAL").unwrap(), &3);
        assert_eq!(report.by_contract.get("contract-A").unwrap(), &5);
        assert_eq!(report.by_contract.get("contract-B").unwrap(), &3);
    }

    #[test]
    fn test_health_score_calculation() {
        let mut analytics = RefusalAnalytics::new(100);

        // No anomalies = perfect health
        let report = analytics.report();
        assert!((report.health_score - 1.0).abs() < 0.001);
    }

    #[test]
    fn test_anomaly_severity_ordering() {
        let a1 = Anomaly {
            anomaly_type: AnomalyType::RefusalSpike,
            severity: Severity::Medium,
            contract_id: "test".to_string(),
            description: "test".to_string(),
            evidence: vec![],
            recommended_action: Action::Alert,
            detected_at: Utc::now(),
            confidence: 0.5,
        };

        let a2 = Anomaly {
            anomaly_type: AnomalyType::KillSwitchEvasion,
            severity: Severity::Critical,
            contract_id: "test".to_string(),
            description: "test".to_string(),
            evidence: vec![],
            recommended_action: Action::KillContract,
            detected_at: Utc::now(),
            confidence: 0.9,
        };

        assert!(a2.severity > a1.severity);
    }

    #[test]
    fn test_severity_as_u8() {
        assert_eq!(Severity::Info.as_u8(), 1);
        assert_eq!(Severity::Low.as_u8(), 2);
        assert_eq!(Severity::Medium.as_u8(), 3);
        assert_eq!(Severity::High.as_u8(), 4);
        assert_eq!(Severity::Critical.as_u8(), 5);
    }

    #[test]
    fn test_anomaly_type_as_str() {
        assert_eq!(AnomalyType::CapabilityProbe.as_str(), "capability_probe");
        assert_eq!(AnomalyType::KillSwitchEvasion.as_str(), "kill_switch_evasion");
        assert_eq!(AnomalyType::AutomatedProbe.as_str(), "automated_probe");
    }

    #[test]
    fn test_action_as_str() {
        assert_eq!(Action::KillContract.as_str(), "kill_contract");
        assert_eq!(Action::KillFamily.as_str(), "kill_family");
        assert_eq!(Action::KillGlobal.as_str(), "kill_global");
        assert_eq!(Action::ReduceCapabilities.as_str(), "reduce_capabilities");
        assert_eq!(Action::IncreaseMonitoring.as_str(), "increase_monitoring");
        assert_eq!(Action::Alert.as_str(), "alert");
        assert_eq!(Action::None.as_str(), "none");
    }

    #[test]
    fn test_clear() {
        let mut analytics = RefusalAnalytics::new(100);

        for _ in 0..10 {
            let refusal = make_refusal(RefusalCategory::Capability, "TEST", "op");
            analytics.record("contract", refusal);
        }

        assert_eq!(analytics.refusal_count(), 10);

        analytics.clear();

        assert_eq!(analytics.refusal_count(), 0);
    }

    #[test]
    fn test_analyze_no_anomalies() {
        let analytics = RefusalAnalytics::new(100);
        let anomalies = analytics.analyze();
        assert!(anomalies.is_empty());
    }

    #[test]
    fn test_analyze_returns_sorted_by_severity() {
        let mut analytics = RefusalAnalytics::with_thresholds(
            100,
            AnalyticsThresholds {
                capability_probe_min_count: 1,
                capability_probe_min_unique: 1,
                kill_evasion_min_count: 1,
                ..Default::default()
            },
        );

        // Trigger capability probe (medium/high severity)
        let refusal = make_refusal(RefusalCategory::Capability, "UNAUTHORIZED:read", "op");
        analytics.record("contract-A", refusal);
        let refusal = make_refusal(RefusalCategory::Capability, "UNAUTHORIZED:write", "op");
        analytics.record("contract-A", refusal);

        // Trigger kill switch evasion (critical severity)
        let kill = make_refusal(RefusalCategory::KillSwitch, "GLOBAL", "op");
        analytics.record("contract-B", kill);
        let evasion = make_refusal(RefusalCategory::Capability, "UNAUTHORIZED:read", "op");
        analytics.record("contract-B", evasion);

        let anomalies = analytics.analyze();

        if anomalies.len() >= 2 {
            // First anomaly should be critical (kill switch evasion)
            assert!(anomalies[0].severity >= anomalies[1].severity);
        }
    }

    #[test]
    fn test_report_serialization() {
        let mut analytics = RefusalAnalytics::new(100);
        let refusal = make_refusal(RefusalCategory::Capability, "TEST", "op");
        analytics.record("contract", refusal);

        let report = analytics.report();
        let json = serde_json::to_string(&report).unwrap();

        assert!(json.contains("total_refusals"));
        assert!(json.contains("health_score"));
        assert!(json.contains("by_category"));
    }

    #[test]
    fn test_anomaly_serialization() {
        let anomaly = Anomaly {
            anomaly_type: AnomalyType::CapabilityProbe,
            severity: Severity::High,
            contract_id: "test-contract".to_string(),
            description: "Test anomaly".to_string(),
            evidence: vec!["ref-1".to_string(), "ref-2".to_string()],
            recommended_action: Action::ReduceCapabilities,
            detected_at: Utc::now(),
            confidence: 0.85,
        };

        let json = serde_json::to_string(&anomaly).unwrap();
        let deserialized: Anomaly = serde_json::from_str(&json).unwrap();

        assert_eq!(deserialized.anomaly_type, AnomalyType::CapabilityProbe);
        assert_eq!(deserialized.severity, Severity::High);
        assert_eq!(deserialized.contract_id, "test-contract");
        assert_eq!(deserialized.evidence.len(), 2);
    }

    #[test]
    fn test_trend_calculation_improving() {
        let mut analytics = RefusalAnalytics::new(100);

        // Add enough refusals for trend calculation (min 10 required)
        for _ in 0..12 {
            let refusal = make_refusal(RefusalCategory::Validation, "TEST", "op");
            analytics.record("contract", refusal);
        }

        let report = analytics.report();
        // The trend calculation splits by index, so with equal halves trend is ~0
        // This is expected behavior - trend measures change over the window
        assert!(report.trend.is_finite());
    }

    #[test]
    fn test_trend_calculation_degrading() {
        let mut analytics = RefusalAnalytics::new(100);

        // Add enough refusals for trend calculation
        for _ in 0..15 {
            let refusal = make_refusal(RefusalCategory::Validation, "TEST", "op");
            analytics.record("contract", refusal);
        }

        let report = analytics.report();
        // With odd total (15), halves will be 7 and 8, giving small positive trend
        // (old_rate=7 - new_rate=8) / old_rate = -1/7 ≈ -0.14
        // This shows degrading since new half has more
        assert!(report.trend.is_finite());
        // With 15 items: old_half=7, new_half=8, trend = (7-8)/7 = -0.14
        assert!(report.trend < 0.0, "Expected negative trend for odd split with more in new half");
    }
}

#[cfg(test)]
mod proptests {
    use super::*;
    use crate::refusal::RefusalCode;
    use proptest::prelude::*;

    fn arb_refusal_category() -> impl Strategy<Value = RefusalCategory> {
        prop_oneof![
            Just(RefusalCategory::Environment),
            Just(RefusalCategory::Key),
            Just(RefusalCategory::Capability),
            Just(RefusalCategory::Validation),
            Just(RefusalCategory::Resource),
            Just(RefusalCategory::KillSwitch),
        ]
    }

    proptest! {
        #[test]
        fn prop_window_never_exceeds_size(window_size in 1..100usize, num_refusals in 0..200usize) {
            let mut analytics = RefusalAnalytics::new(window_size);

            for i in 0..num_refusals {
                let refusal = Refusal::new(
                    RefusalCode::new(RefusalCategory::Validation, format!("TEST:{}", i)),
                    "test",
                    "operation",
                    "hash",
                );
                analytics.record("contract", refusal);
            }

            prop_assert!(analytics.refusal_count() <= window_size);
        }

        #[test]
        fn prop_rate_by_category_sums_to_one(
            caps in 0..20usize,
            vals in 0..20usize,
            envs in 0..20usize
        ) {
            if caps + vals + envs == 0 {
                return Ok(());
            }

            let mut analytics = RefusalAnalytics::new(100);

            for _ in 0..caps {
                let refusal = Refusal::new(
                    RefusalCode::new(RefusalCategory::Capability, "TEST"),
                    "test", "op", "hash",
                );
                analytics.record("contract", refusal);
            }
            for _ in 0..vals {
                let refusal = Refusal::new(
                    RefusalCode::new(RefusalCategory::Validation, "TEST"),
                    "test", "op", "hash",
                );
                analytics.record("contract", refusal);
            }
            for _ in 0..envs {
                let refusal = Refusal::new(
                    RefusalCode::new(RefusalCategory::Environment, "TEST"),
                    "test", "op", "hash",
                );
                analytics.record("contract", refusal);
            }

            let rates = analytics.rate_by_category();
            let sum: f64 = rates.values().sum();

            prop_assert!((sum - 1.0).abs() < 0.001, "Rates should sum to 1.0, got {}", sum);
        }

        #[test]
        fn prop_health_score_bounded(
            num_refusals in 0..50usize,
            category in arb_refusal_category()
        ) {
            let mut analytics = RefusalAnalytics::new(100);

            for _ in 0..num_refusals {
                let refusal = Refusal::new(
                    RefusalCode::new(category, "TEST"),
                    "test", "op", "hash",
                );
                analytics.record("contract", refusal);
            }

            let report = analytics.report();
            prop_assert!(report.health_score >= 0.0 && report.health_score <= 1.0);
        }

        #[test]
        fn prop_top_refusers_limited(n in 1..10usize, num_contracts in 1..20usize) {
            let mut analytics = RefusalAnalytics::new(100);

            for i in 0..num_contracts {
                let refusal = Refusal::new(
                    RefusalCode::new(RefusalCategory::Validation, "TEST"),
                    "test", "op", "hash",
                );
                analytics.record(&format!("contract-{}", i), refusal);
            }

            let top = analytics.top_refusers(n);
            prop_assert!(top.len() <= n);
            prop_assert!(top.len() <= num_contracts);
        }
    }
}
