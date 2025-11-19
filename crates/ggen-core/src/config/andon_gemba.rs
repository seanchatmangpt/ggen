//! ANDON (Alert System) & GEMBA WALK (Operational Audit)
//!
//! ANDON: Real-time alert and escalation system for anomalies
//! GEMBA WALK: Go-see-yourself operational audits to identify problems at source

use serde::{Deserialize, Serialize};
use std::collections::{BTreeMap, VecDeque};

/// ANDON: Real-time Alert System
///
/// Immediately stops and alerts when abnormalities detected
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct Andon {
    /// Active alerts
    pub alerts: VecDeque<AndonAlert>,

    /// Alert rules
    pub rules: Vec<AndonRule>,

    /// Alert callbacks/handlers
    pub handlers: Vec<AndonHandler>,

    /// Statistics
    pub stats: AndonStats,

    /// Maximum alerts to keep in history
    pub max_history: usize,
}

/// An ANDON alert
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct AndonAlert {
    /// Unique alert ID
    pub id: String,

    /// Severity level
    pub severity: AndonSeverity,

    /// What triggered the alert
    pub trigger: String,

    /// Detailed message
    pub message: String,

    /// Current status
    pub status: AlertStatus,

    /// When it was triggered
    pub triggered_at: String,

    /// When it was resolved (if resolved)
    pub resolved_at: Option<String>,

    /// Root cause (if identified)
    pub root_cause: Option<String>,

    /// Actions taken
    pub actions: Vec<String>,

    /// Context/metadata
    pub context: BTreeMap<String, String>,
}

/// Alert severity levels
#[derive(Debug, Clone, Copy, Serialize, Deserialize, PartialEq, Eq, PartialOrd, Ord)]
pub enum AndonSeverity {
    /// Minor issue (doesn't stop operation)
    Minor,

    /// Moderate issue (affects performance)
    Major,

    /// Critical issue (should stop operation)
    Critical,

    /// System failure (immediate action required)
    Emergency,
}

/// Alert status
#[derive(Debug, Clone, Copy, Serialize, Deserialize, PartialEq, Eq)]
pub enum AlertStatus {
    /// Just triggered
    Active,

    /// Being investigated
    Investigating,

    /// Temporarily suppressed
    Suppressed,

    /// Fixed/resolved
    Resolved,

    /// Acknowledged but not fixed
    Acknowledged,
}

/// ANDON rule (what triggers an alert)
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct AndonRule {
    /// Rule identifier
    pub id: String,

    /// What condition triggers this rule
    pub condition: String,

    /// Severity when triggered
    pub severity: AndonSeverity,

    /// Auto-stop operation
    pub auto_stop: bool,

    /// Notify who (email, Slack, etc.)
    pub notify_channels: Vec<String>,

    /// Is this rule active
    pub enabled: bool,

    /// How many times triggered
    pub trigger_count: u32,
}

/// Alert handler/responder
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct AndonHandler {
    /// Handler identifier
    pub id: String,

    /// What it responds to
    pub handles: String,

    /// Handler action
    pub action: String,

    /// Response time (ms)
    pub response_time_ms: u32,

    /// Is automatic
    pub automatic: bool,

    /// Requires approval
    pub requires_approval: bool,
}

/// ANDON statistics
#[derive(Debug, Clone, Default, Serialize, Deserialize)]
pub struct AndonStats {
    /// Total alerts triggered
    pub total_alerts: u32,

    /// Currently active
    pub active_alerts: u32,

    /// Alerts by severity
    pub alerts_by_severity: BTreeMap<String, u32>,

    /// Average resolution time (ms)
    pub avg_resolution_time_ms: u32,

    /// Most common triggers
    pub top_triggers: Vec<(String, u32)>,
}

/// GEMBA WALK: Go-See Operational Audit
///
/// Direct observation of actual operations to identify problems at source
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct GembaWalk {
    /// Walks conducted
    pub walks: Vec<GembaWalkSession>,

    /// Observations aggregated
    pub observations: Vec<GembaObservation>,

    /// Problem areas identified
    pub problem_areas: Vec<ProblemArea>,

    /// Improvement actions
    pub actions: Vec<ImprovementAction>,

    /// Statistics
    pub stats: GembaStats,
}

/// A GEMBA walk session
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct GembaWalkSession {
    /// Session ID
    pub id: String,

    /// When the walk occurred
    pub date: String,

    /// Duration (minutes)
    pub duration_minutes: u32,

    /// Area/process walked
    pub area: String,

    /// Observer(s)
    pub observers: Vec<String>,

    /// Observations made
    pub observations: Vec<String>,

    /// Problems identified
    pub problems_found: u32,

    /// Improvements suggested
    pub improvements_suggested: u32,

    /// Summary
    pub summary: String,
}

/// An observation from a GEMBA walk
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct GembaObservation {
    /// Observation ID
    pub id: String,

    /// What was observed
    pub description: String,

    /// Location/context
    pub location: String,

    /// Frequency (how often observed)
    pub frequency: ObservationFrequency,

    /// Impact
    pub impact: ObservationImpact,

    /// Is this a problem
    pub is_problem: bool,

    /// Related improvement
    pub related_improvement: Option<String>,
}

/// How often an observation is made
#[derive(Debug, Clone, Copy, Serialize, Deserialize, PartialEq, Eq)]
pub enum ObservationFrequency {
    /// Rare/one-time
    Rare,

    /// Occasional
    Occasional,

    /// Frequent
    Frequent,

    /// Constant
    Constant,
}

/// Impact of an observation
#[derive(Debug, Clone, Copy, Serialize, Deserialize, PartialEq, Eq, PartialOrd, Ord)]
pub enum ObservationImpact {
    /// Low impact
    Low,

    /// Medium impact
    Medium,

    /// High impact
    High,

    /// Critical impact
    Critical,
}

/// Problem area identified during GEMBA walk
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct ProblemArea {
    /// Problem ID
    pub id: String,

    /// Where the problem is
    pub location: String,

    /// What the problem is
    pub description: String,

    /// Root cause (if known)
    pub root_cause: Option<String>,

    /// Severity
    pub severity: ProblemSeverity,

    /// Attempts to solve
    pub attempts: Vec<String>,

    /// Current status
    pub status: ProblemStatus,
}

#[derive(Debug, Clone, Copy, Serialize, Deserialize, PartialEq, Eq, PartialOrd, Ord)]
pub enum ProblemSeverity {
    Minor,
    Moderate,
    Major,
    Critical,
}

#[derive(Debug, Clone, Copy, Serialize, Deserialize, PartialEq, Eq)]
pub enum ProblemStatus {
    /// Just identified
    Identified,

    /// Being worked on
    InProgress,

    /// Pending approval
    Awaiting,

    /// Fixed
    Resolved,
}

/// Improvement action
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct ImprovementAction {
    /// Action ID
    pub id: String,

    /// What improvement
    pub improvement: String,

    /// Owner/responsible person
    pub owner: String,

    /// Target completion date
    pub due_date: Option<String>,

    /// Current status
    pub status: ActionStatus,

    /// Progress (%)
    pub progress: u32,

    /// Blockers
    pub blockers: Vec<String>,

    /// Expected benefit
    pub expected_benefit: String,
}

#[derive(Debug, Clone, Copy, Serialize, Deserialize, PartialEq, Eq)]
pub enum ActionStatus {
    /// Not started
    NotStarted,

    /// In progress
    InProgress,

    /// Blocked
    Blocked,

    /// Completed
    Completed,
}

/// GEMBA walk statistics
#[derive(Debug, Clone, Default, Serialize, Deserialize)]
pub struct GembaStats {
    /// Total walks conducted
    pub total_walks: u32,

    /// Total observations
    pub total_observations: u32,

    /// Problems identified
    pub problems_identified: u32,

    /// Problems resolved
    pub problems_resolved: u32,

    /// Improvements implemented
    pub improvements_implemented: u32,

    /// Avg problems per walk
    pub avg_problems_per_walk: f32,

    /// Effectiveness (%)
    pub effectiveness: f32,
}

impl Andon {
    /// Create new ANDON system
    pub fn new(max_history: usize) -> Self {
        Self {
            alerts: VecDeque::new(),
            rules: Vec::new(),
            handlers: Vec::new(),
            stats: AndonStats::default(),
            max_history,
        }
    }

    /// Add an alert rule
    pub fn add_rule(&mut self, rule: AndonRule) {
        self.rules.push(rule);
    }

    /// Trigger an alert
    pub fn trigger_alert(&mut self, alert: AndonAlert) {
        // Update statistics
        self.stats.total_alerts += 1;
        self.stats.active_alerts += 1;

        let severity_str = format!("{:?}", alert.severity);
        *self
            .stats
            .alerts_by_severity
            .entry(severity_str)
            .or_insert(0) += 1;

        // Add to alerts queue
        self.alerts.push_back(alert.clone());

        // Maintain history limit
        while self.alerts.len() > self.max_history {
            self.alerts.pop_front();
        }

        // If critical, call handlers
        if alert.severity == AndonSeverity::Critical || alert.severity == AndonSeverity::Emergency {
            self.invoke_handlers(&alert);
        }
    }

    /// Resolve an alert
    pub fn resolve_alert(&mut self, alert_id: &str, root_cause: String) {
        for alert in self.alerts.iter_mut().rev() {
            if alert.id == alert_id {
                alert.status = AlertStatus::Resolved;
                alert.root_cause = Some(root_cause);
                alert.resolved_at = Some(chrono::Utc::now().to_rfc3339());
                self.stats.active_alerts = self.stats.active_alerts.saturating_sub(1);
                break;
            }
        }
    }

    /// Invoke handlers for alert
    fn invoke_handlers(&self, alert: &AndonAlert) {
        for handler in &self.handlers {
            if handler.automatic && handler.handles == alert.trigger {
                // Execute handler (notify, log, etc.)
                // Implementation would depend on handler type
            }
        }
    }

    /// Get active critical alerts
    pub fn critical_alerts(&self) -> Vec<&AndonAlert> {
        self.alerts
            .iter()
            .filter(|a| a.severity >= AndonSeverity::Critical && a.status == AlertStatus::Active)
            .collect()
    }

    /// Check if system should stop
    pub fn should_stop(&self) -> bool {
        !self.critical_alerts().is_empty()
            && self
                .rules
                .iter()
                .any(|r| r.enabled && r.auto_stop && r.severity == AndonSeverity::Emergency)
    }
}

impl GembaWalk {
    /// Create new GEMBA walk system
    pub fn new() -> Self {
        Self {
            walks: Vec::new(),
            observations: Vec::new(),
            problem_areas: Vec::new(),
            actions: Vec::new(),
            stats: GembaStats::default(),
        }
    }

    /// Conduct a new GEMBA walk
    pub fn conduct_walk(&mut self, session: GembaWalkSession) {
        // Add observations
        for obs_desc in &session.observations {
            self.observations.push(GembaObservation {
                id: format!("obs-{}", self.observations.len()),
                description: obs_desc.clone(),
                location: session.area.clone(),
                frequency: ObservationFrequency::Occasional,
                impact: ObservationImpact::Medium,
                is_problem: false,
                related_improvement: None,
            });
        }

        self.stats.total_walks += 1;
        self.stats.total_observations += self.observations.len() as u32;
        self.stats.problems_identified += session.problems_found;

        self.walks.push(session);
        self.recalculate_stats();
    }

    /// Add a problem area
    pub fn add_problem(&mut self, problem: ProblemArea) {
        self.problem_areas.push(problem);
    }

    /// Add an improvement action
    pub fn add_improvement(&mut self, action: ImprovementAction) {
        self.actions.push(action);
    }

    /// Mark problem as resolved
    pub fn resolve_problem(&mut self, problem_id: &str) {
        for problem in &mut self.problem_areas {
            if problem.id == problem_id {
                problem.status = ProblemStatus::Resolved;
                self.stats.problems_resolved += 1;
                break;
            }
        }
    }

    /// Mark improvement as completed
    pub fn complete_improvement(&mut self, action_id: &str) {
        for action in &mut self.actions {
            if action.id == action_id {
                action.status = ActionStatus::Completed;
                action.progress = 100;
                self.stats.improvements_implemented += 1;
                break;
            }
        }
    }

    /// Get unresolved problems
    pub fn unresolved_problems(&self) -> Vec<&ProblemArea> {
        self.problem_areas
            .iter()
            .filter(|p| p.status != ProblemStatus::Resolved)
            .collect()
    }

    /// Get blocked improvements
    pub fn blocked_improvements(&self) -> Vec<&ImprovementAction> {
        self.actions
            .iter()
            .filter(|a| a.status == ActionStatus::Blocked)
            .collect()
    }

    /// Calculate effectiveness
    fn recalculate_stats(&mut self) {
        if self.stats.total_walks > 0 {
            self.stats.avg_problems_per_walk =
                self.stats.problems_identified as f32 / self.stats.total_walks as f32;
        }

        if self.stats.problems_identified > 0 {
            self.stats.effectiveness = (self.stats.problems_resolved as f32
                / self.stats.problems_identified as f32)
                * 100.0;
        }
    }
}

impl Default for GembaWalk {
    fn default() -> Self {
        Self::new()
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_andon_alert_trigger() {
        let mut andon = Andon::new(100);

        let alert = AndonAlert {
            id: "alert-1".to_string(),
            severity: AndonSeverity::Critical,
            trigger: "config-invalid".to_string(),
            message: "Invalid configuration detected".to_string(),
            status: AlertStatus::Active,
            triggered_at: chrono::Utc::now().to_rfc3339(),
            resolved_at: None,
            root_cause: None,
            actions: vec![],
            context: BTreeMap::new(),
        };

        andon.trigger_alert(alert.clone());
        assert_eq!(andon.stats.total_alerts, 1);
        assert_eq!(andon.stats.active_alerts, 1);
    }

    #[test]
    fn test_andon_critical_alerts() {
        let mut andon = Andon::new(100);

        let alert = AndonAlert {
            id: "alert-1".to_string(),
            severity: AndonSeverity::Critical,
            trigger: "test".to_string(),
            message: "Test".to_string(),
            status: AlertStatus::Active,
            triggered_at: chrono::Utc::now().to_rfc3339(),
            resolved_at: None,
            root_cause: None,
            actions: vec![],
            context: BTreeMap::new(),
        };

        andon.trigger_alert(alert);
        assert_eq!(andon.critical_alerts().len(), 1);
    }

    #[test]
    fn test_gemba_walk_conduction() {
        let mut gemba = GembaWalk::new();

        let session = GembaWalkSession {
            id: "walk-1".to_string(),
            date: chrono::Utc::now().to_rfc3339(),
            duration_minutes: 30,
            area: "config-system".to_string(),
            observers: vec!["engineer-1".to_string()],
            observations: vec!["Redundant checks".to_string()],
            problems_found: 1,
            improvements_suggested: 2,
            summary: "Found opportunity to optimize".to_string(),
        };

        gemba.conduct_walk(session);
        assert_eq!(gemba.stats.total_walks, 1);
        assert_eq!(gemba.stats.problems_identified, 1);
    }
}
