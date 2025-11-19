//! Kaizen - Continuous Improvement Culture
//!
//! Tracks and codifies continuous improvement in code generation:
//! - Improvement tracking over time
//! - PDCA cycle (Plan-Do-Check-Act)
//! - Kaizen events and blitzes
//! - Standard work evolution

use super::*;
use std::collections::VecDeque;

/// Kaizen (continuous improvement) tracker
pub struct KaizenTracker {
    /// Improvement events
    events: Vec<KaizenEvent>,

    /// PDCA cycles
    pdca_cycles: Vec<PdcaCycle>,

    /// Standard work definitions
    standard_work: Vec<StandardWork>,

    /// Improvement metrics over time
    metrics_history: VecDeque<KaizenMetrics>,
}

/// A Kaizen improvement event
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct KaizenEvent {
    /// Event ID
    pub id: String,

    /// Event name
    pub name: String,

    /// Event type
    pub event_type: KaizenEventType,

    /// Problem being addressed
    pub problem: String,

    /// Current state before improvement
    pub before_state: StateSnapshot,

    /// Target state after improvement
    pub target_state: StateSnapshot,

    /// Actual state achieved
    pub after_state: Option<StateSnapshot>,

    /// Participants
    pub participants: Vec<String>,

    /// Start date
    pub started_at: String,

    /// Completion date
    pub completed_at: Option<String>,

    /// Status
    pub status: KaizenStatus,

    /// Lessons learned
    pub lessons_learned: Vec<String>,
}

/// Type of Kaizen event
#[derive(Debug, Clone, Serialize, Deserialize, PartialEq, Eq)]
pub enum KaizenEventType {
    /// Quick improvement (hours to days)
    Blitz,

    /// Structured improvement event (3-5 days)
    Event,

    /// Daily improvement suggestion
    DailyKaizen,

    /// System-level improvement
    Kaikaku,
}

/// Status of Kaizen event
#[derive(Debug, Clone, Serialize, Deserialize, PartialEq, Eq)]
pub enum KaizenStatus {
    /// Planned but not started
    Planned,

    /// In progress
    InProgress,

    /// Completed successfully
    Completed,

    /// Did not achieve target
    PartialSuccess,

    /// Cancelled
    Cancelled,
}

/// Snapshot of state for comparison
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct StateSnapshot {
    /// Timestamp
    pub timestamp: String,

    /// Key metrics
    pub metrics: BTreeMap<String, f64>,

    /// Description
    pub description: String,

    /// Process documentation
    pub process_documentation: Option<String>,
}

/// PDCA (Plan-Do-Check-Act) cycle
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct PdcaCycle {
    /// Cycle ID
    pub id: String,

    /// Problem/Opportunity
    pub problem: String,

    /// Plan phase
    pub plan: PlanPhase,

    /// Do phase
    pub do_phase: Option<DoPhase>,

    /// Check phase
    pub check: Option<CheckPhase>,

    /// Act phase
    pub act: Option<ActPhase>,

    /// Current phase
    pub current_phase: PdcaPhase,

    /// Created at
    pub created_at: String,
}

/// PDCA phases
#[derive(Debug, Clone, Serialize, Deserialize, PartialEq, Eq)]
pub enum PdcaPhase {
    /// Planning phase
    Plan,

    /// Doing/Implementation phase
    Do,

    /// Checking/Verification phase
    Check,

    /// Acting/Standardization phase
    Act,

    /// Cycle complete
    Complete,
}

/// Plan phase of PDCA
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct PlanPhase {
    /// Root cause analysis
    pub root_cause: String,

    /// Hypothesis for improvement
    pub hypothesis: String,

    /// Planned countermeasures
    pub countermeasures: Vec<String>,

    /// Success criteria
    pub success_criteria: Vec<String>,

    /// Target metrics
    pub target_metrics: BTreeMap<String, f64>,

    /// Timeline
    pub timeline: String,
}

/// Do phase of PDCA
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct DoPhase {
    /// Implementation steps taken
    pub steps_taken: Vec<String>,

    /// Start date
    pub started_at: String,

    /// Completion date
    pub completed_at: Option<String>,

    /// Issues encountered
    pub issues: Vec<String>,

    /// Adjustments made
    pub adjustments: Vec<String>,
}

/// Check phase of PDCA
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct CheckPhase {
    /// Actual metrics achieved
    pub actual_metrics: BTreeMap<String, f64>,

    /// Comparison with targets
    pub targets_met: Vec<(String, bool)>,

    /// Analysis of results
    pub analysis: String,

    /// Success rate
    pub success_rate: f64,

    /// Unexpected outcomes
    pub unexpected_outcomes: Vec<String>,
}

/// Act phase of PDCA
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct ActPhase {
    /// Decision: standardize, adjust, or abandon
    pub decision: ActDecision,

    /// Reason for decision
    pub reason: String,

    /// If standardizing, reference to new standard work
    pub standard_work_id: Option<String>,

    /// Next steps
    pub next_steps: Vec<String>,

    /// Knowledge captured
    pub knowledge_captured: Vec<String>,
}

/// Decision in Act phase
#[derive(Debug, Clone, Serialize, Deserialize, PartialEq, Eq)]
pub enum ActDecision {
    /// Standardize the improvement
    Standardize,

    /// Adjust and try again
    Adjust,

    /// Abandon this approach
    Abandon,

    /// Scale to other areas
    Scale,
}

/// Standard work definition
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct StandardWork {
    /// Standard work ID
    pub id: String,

    /// Name
    pub name: String,

    /// Description
    pub description: String,

    /// Process area (e.g., "SPARQL Execution", "Template Rendering")
    pub process_area: String,

    /// Standard steps
    pub steps: Vec<StandardStep>,

    /// Expected cycle time (ms)
    pub expected_cycle_time_ms: u64,

    /// Quality standards
    pub quality_standards: Vec<String>,

    /// Version
    pub version: String,

    /// Created from Kaizen event
    pub created_from_kaizen: Option<String>,

    /// Last updated
    pub updated_at: String,
}

/// A step in standard work
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct StandardStep {
    /// Step number
    pub step: usize,

    /// Description
    pub description: String,

    /// Expected time (ms)
    pub expected_time_ms: u64,

    /// Key points (safety, quality, efficiency)
    pub key_points: Vec<String>,

    /// Reasons for the step
    pub reasons: Vec<String>,
}

/// Kaizen metrics snapshot
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct KaizenMetrics {
    /// Timestamp
    pub timestamp: String,

    /// Number of improvements implemented
    pub improvements_count: usize,

    /// Average value-added ratio
    pub avg_value_added_ratio: f64,

    /// Average flow efficiency
    pub avg_flow_efficiency: f64,

    /// Total waste eliminated (ms)
    pub waste_eliminated_ms: u64,

    /// Employee engagement score (0.0-1.0)
    pub engagement_score: f64,

    /// Ideas submitted
    pub ideas_submitted: usize,

    /// Ideas implemented
    pub ideas_implemented: usize,

    /// Implementation rate
    pub implementation_rate: f64,
}

impl KaizenTracker {
    /// Create new Kaizen tracker
    pub fn new() -> Self {
        Self {
            events: Vec::new(),
            pdca_cycles: Vec::new(),
            standard_work: Vec::new(),
            metrics_history: VecDeque::new(),
        }
    }

    /// Start a new Kaizen event
    pub fn start_event(
        &mut self,
        name: String,
        event_type: KaizenEventType,
        problem: String,
        before_state: StateSnapshot,
        target_state: StateSnapshot,
    ) -> String {
        let id = format!("kaizen-{}", uuid::Uuid::new_v4());

        let event = KaizenEvent {
            id: id.clone(),
            name,
            event_type,
            problem,
            before_state,
            target_state,
            after_state: None,
            participants: Vec::new(),
            started_at: chrono::Utc::now().to_rfc3339(),
            completed_at: None,
            status: KaizenStatus::InProgress,
            lessons_learned: Vec::new(),
        };

        self.events.push(event);
        id
    }

    /// Complete a Kaizen event
    pub fn complete_event(&mut self, event_id: &str, after_state: StateSnapshot) {
        if let Some(event) = self.events.iter_mut().find(|e| e.id == event_id) {
            event.after_state = Some(after_state);
            event.completed_at = Some(chrono::Utc::now().to_rfc3339());

            // Determine success
            event.status = if Self::targets_achieved(event) {
                KaizenStatus::Completed
            } else {
                KaizenStatus::PartialSuccess
            };
        }
    }

    /// Check if targets were achieved
    fn targets_achieved(event: &KaizenEvent) -> bool {
        if let Some(after) = &event.after_state {
            // Check if all target metrics were met
            for (key, target_value) in &event.target_state.metrics {
                if let Some(actual_value) = after.metrics.get(key) {
                    // Assuming higher is better for all metrics
                    if actual_value < target_value {
                        return false;
                    }
                } else {
                    return false;
                }
            }
            true
        } else {
            false
        }
    }

    /// Start PDCA cycle
    pub fn start_pdca(&mut self, problem: String, plan: PlanPhase) -> String {
        let id = format!("pdca-{}", uuid::Uuid::new_v4());

        let cycle = PdcaCycle {
            id: id.clone(),
            problem,
            plan,
            do_phase: None,
            check: None,
            act: None,
            current_phase: PdcaPhase::Plan,
            created_at: chrono::Utc::now().to_rfc3339(),
        };

        self.pdca_cycles.push(cycle);
        id
    }

    /// Move PDCA to Do phase
    pub fn pdca_do(&mut self, cycle_id: &str, do_phase: DoPhase) {
        if let Some(cycle) = self.pdca_cycles.iter_mut().find(|c| c.id == cycle_id) {
            cycle.do_phase = Some(do_phase);
            cycle.current_phase = PdcaPhase::Do;
        }
    }

    /// Move PDCA to Check phase
    pub fn pdca_check(&mut self, cycle_id: &str, check: CheckPhase) {
        if let Some(cycle) = self.pdca_cycles.iter_mut().find(|c| c.id == cycle_id) {
            cycle.check = Some(check);
            cycle.current_phase = PdcaPhase::Check;
        }
    }

    /// Move PDCA to Act phase
    pub fn pdca_act(&mut self, cycle_id: &str, act: ActPhase) {
        if let Some(cycle) = self.pdca_cycles.iter_mut().find(|c| c.id == cycle_id) {
            cycle.act = Some(act);
            cycle.current_phase = PdcaPhase::Act;
        }
    }

    /// Create standard work from successful improvement
    pub fn create_standard_work(&mut self, work: StandardWork) -> String {
        let id = work.id.clone();
        self.standard_work.push(work);
        id
    }

    /// Record metrics snapshot
    pub fn record_metrics(&mut self, metrics: KaizenMetrics) {
        // Keep last 100 snapshots
        if self.metrics_history.len() >= 100 {
            self.metrics_history.pop_front();
        }
        self.metrics_history.push_back(metrics);
    }

    /// Get improvement trend
    pub fn get_improvement_trend(&self) -> Option<ImprovementTrend> {
        if self.metrics_history.len() < 2 {
            return None;
        }

        let first = self.metrics_history.front()?;
        let last = self.metrics_history.back()?;

        let value_added_trend =
            ((last.avg_value_added_ratio - first.avg_value_added_ratio) / first.avg_value_added_ratio)
                * 100.0;

        let flow_efficiency_trend =
            ((last.avg_flow_efficiency - first.avg_flow_efficiency) / first.avg_flow_efficiency)
                * 100.0;

        let implementation_rate_trend =
            ((last.implementation_rate - first.implementation_rate) / first.implementation_rate)
                * 100.0;

        Some(ImprovementTrend {
            value_added_improvement: value_added_trend,
            flow_efficiency_improvement: flow_efficiency_trend,
            implementation_rate_improvement: implementation_rate_trend,
            total_waste_eliminated_ms: last.waste_eliminated_ms,
            engagement_trend: last.engagement_score - first.engagement_score,
        })
    }

    /// Get all events
    pub fn get_events(&self) -> &[KaizenEvent] {
        &self.events
    }

    /// Get all PDCA cycles
    pub fn get_pdca_cycles(&self) -> &[PdcaCycle] {
        &self.pdca_cycles
    }

    /// Get all standard work
    pub fn get_standard_work(&self) -> &[StandardWork] {
        &self.standard_work
    }
}

impl Default for KaizenTracker {
    fn default() -> Self {
        Self::new()
    }
}

/// Improvement trend over time
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct ImprovementTrend {
    /// Value-added ratio improvement (%)
    pub value_added_improvement: f64,

    /// Flow efficiency improvement (%)
    pub flow_efficiency_improvement: f64,

    /// Implementation rate improvement (%)
    pub implementation_rate_improvement: f64,

    /// Total waste eliminated (ms)
    pub total_waste_eliminated_ms: u64,

    /// Engagement score trend
    pub engagement_trend: f64,
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_kaizen_event() {
        let mut tracker = KaizenTracker::new();

        let before = StateSnapshot {
            timestamp: chrono::Utc::now().to_rfc3339(),
            metrics: BTreeMap::from([("flow_efficiency".to_string(), 0.3)]),
            description: "Before".to_string(),
            process_documentation: None,
        };

        let target = StateSnapshot {
            timestamp: chrono::Utc::now().to_rfc3339(),
            metrics: BTreeMap::from([("flow_efficiency".to_string(), 0.6)]),
            description: "Target".to_string(),
            process_documentation: None,
        };

        let id = tracker.start_event(
            "Improve caching".to_string(),
            KaizenEventType::Blitz,
            "Slow SPARQL queries".to_string(),
            before,
            target,
        );

        assert_eq!(tracker.events.len(), 1);
        assert_eq!(tracker.events[0].status, KaizenStatus::InProgress);

        let after = StateSnapshot {
            timestamp: chrono::Utc::now().to_rfc3339(),
            metrics: BTreeMap::from([("flow_efficiency".to_string(), 0.7)]),
            description: "After".to_string(),
            process_documentation: None,
        };

        tracker.complete_event(&id, after);

        assert_eq!(tracker.events[0].status, KaizenStatus::Completed);
    }

    #[test]
    fn test_pdca_cycle() {
        let mut tracker = KaizenTracker::new();

        let plan = PlanPhase {
            root_cause: "No caching".to_string(),
            hypothesis: "Adding cache will improve performance".to_string(),
            countermeasures: vec!["Implement LRU cache".to_string()],
            success_criteria: vec!["50% reduction in query time".to_string()],
            target_metrics: BTreeMap::from([("query_time_ms".to_string(), 100.0)]),
            timeline: "1 week".to_string(),
        };

        let id = tracker.start_pdca("Slow queries".to_string(), plan);

        assert_eq!(tracker.pdca_cycles.len(), 1);
        assert_eq!(tracker.pdca_cycles[0].current_phase, PdcaPhase::Plan);
    }
}
