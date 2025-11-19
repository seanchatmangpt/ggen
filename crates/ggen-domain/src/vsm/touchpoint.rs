//! Touchpoint tracking for stakeholder interactions
//!
//! Models handoffs, reviews, approvals, and collaborations between stakeholders.

use chrono::{DateTime, Utc};
use serde::{Deserialize, Serialize};

use super::swim_lane::StakeholderRole;

/// Types of touchpoints in the value stream
#[derive(Debug, Clone, Copy, PartialEq, Eq, Serialize, Deserialize)]
#[serde(rename_all = "snake_case")]
pub enum TouchpointType {
    /// Work handoff between stakeholders
    Handoff,
    /// Review or inspection
    Review,
    /// Approval decision
    Approval,
    /// Collaborative work session
    Collaboration,
    /// Information notification
    Notification,
    /// Feedback or iteration request
    Feedback,
}

impl TouchpointType {
    /// Get human-readable name
    pub fn name(&self) -> &'static str {
        match self {
            TouchpointType::Handoff => "Handoff",
            TouchpointType::Review => "Review",
            TouchpointType::Approval => "Approval",
            TouchpointType::Collaboration => "Collaboration",
            TouchpointType::Notification => "Notification",
            TouchpointType::Feedback => "Feedback",
        }
    }

    /// Get RDF URI suffix
    pub fn rdf_uri(&self) -> &'static str {
        match self {
            TouchpointType::Handoff => "Handoff",
            TouchpointType::Review => "Review",
            TouchpointType::Approval => "Approval",
            TouchpointType::Collaboration => "Collaboration",
            TouchpointType::Notification => "Notification",
            TouchpointType::Feedback => "Feedback",
        }
    }
}

impl std::fmt::Display for TouchpointType {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.name())
    }
}

/// A touchpoint interaction between stakeholders
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct Touchpoint {
    /// Unique identifier
    pub id: String,
    /// Type of touchpoint
    pub touchpoint_type: TouchpointType,
    /// Originating stakeholder
    pub from_role: StakeholderRole,
    /// Receiving stakeholder
    pub to_role: StakeholderRole,
    /// When touchpoint occurred
    pub timestamp: DateTime<Utc>,
    /// Duration of interaction (ms)
    pub duration_ms: f64,
    /// Stage this touchpoint belongs to
    pub stage_id: Option<String>,
    /// Outcome of touchpoint
    pub outcome: TouchpointOutcome,
    /// Additional context
    pub description: String,
    /// Metadata
    pub metadata: std::collections::HashMap<String, String>,
}

impl Touchpoint {
    /// Create a new touchpoint
    pub fn new(
        touchpoint_type: TouchpointType,
        from_role: StakeholderRole,
        to_role: StakeholderRole,
    ) -> Self {
        Self {
            id: uuid::Uuid::new_v4().to_string(),
            touchpoint_type,
            from_role,
            to_role,
            timestamp: Utc::now(),
            duration_ms: 0.0,
            stage_id: None,
            outcome: TouchpointOutcome::Pending,
            description: String::new(),
            metadata: std::collections::HashMap::new(),
        }
    }

    /// Set duration
    pub fn with_duration(mut self, duration_ms: f64) -> Self {
        self.duration_ms = duration_ms;
        self
    }

    /// Set stage ID
    pub fn with_stage(mut self, stage_id: String) -> Self {
        self.stage_id = Some(stage_id);
        self
    }

    /// Set description
    pub fn with_description(mut self, description: impl Into<String>) -> Self {
        self.description = description.into();
        self
    }

    /// Set outcome
    pub fn with_outcome(mut self, outcome: TouchpointOutcome) -> Self {
        self.outcome = outcome;
        self
    }

    /// Mark as completed successfully
    pub fn complete(&mut self) {
        self.outcome = TouchpointOutcome::Completed;
    }

    /// Mark as blocked
    pub fn block(&mut self, reason: impl Into<String>) {
        self.outcome = TouchpointOutcome::Blocked;
        self.metadata.insert("block_reason".to_string(), reason.into());
    }

    /// Check if touchpoint was successful
    pub fn is_successful(&self) -> bool {
        matches!(self.outcome, TouchpointOutcome::Completed)
    }

    /// Check if touchpoint is still pending
    pub fn is_pending(&self) -> bool {
        matches!(self.outcome, TouchpointOutcome::Pending)
    }

    /// Get formatted summary
    pub fn summary(&self) -> String {
        format!(
            "{} from {} to {} ({:.2}ms) - {}",
            self.touchpoint_type,
            self.from_role,
            self.to_role,
            self.duration_ms,
            self.outcome
        )
    }
}

/// Outcome of a touchpoint interaction
#[derive(Debug, Clone, Copy, PartialEq, Eq, Serialize, Deserialize)]
#[serde(rename_all = "snake_case")]
pub enum TouchpointOutcome {
    /// Still in progress
    Pending,
    /// Completed successfully
    Completed,
    /// Approved (for approval touchpoints)
    Approved,
    /// Rejected (for approval touchpoints)
    Rejected,
    /// Blocked or waiting
    Blocked,
    /// Failed or error occurred
    Failed,
}

impl std::fmt::Display for TouchpointOutcome {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            TouchpointOutcome::Pending => write!(f, "Pending"),
            TouchpointOutcome::Completed => write!(f, "Completed"),
            TouchpointOutcome::Approved => write!(f, "Approved"),
            TouchpointOutcome::Rejected => write!(f, "Rejected"),
            TouchpointOutcome::Blocked => write!(f, "Blocked"),
            TouchpointOutcome::Failed => write!(f, "Failed"),
        }
    }
}

/// Collection of touchpoints with analysis
#[derive(Debug, Clone, Default, Serialize, Deserialize)]
pub struct TouchpointCollection {
    /// All touchpoints
    pub touchpoints: Vec<Touchpoint>,
}

impl TouchpointCollection {
    /// Create new empty collection
    pub fn new() -> Self {
        Self {
            touchpoints: Vec::new(),
        }
    }

    /// Add a touchpoint
    pub fn add(&mut self, touchpoint: Touchpoint) {
        self.touchpoints.push(touchpoint);
    }

    /// Get touchpoints by type
    pub fn by_type(&self, touchpoint_type: TouchpointType) -> Vec<&Touchpoint> {
        self.touchpoints
            .iter()
            .filter(|t| t.touchpoint_type == touchpoint_type)
            .collect()
    }

    /// Get touchpoints involving a stakeholder
    pub fn by_stakeholder(&self, role: StakeholderRole) -> Vec<&Touchpoint> {
        self.touchpoints
            .iter()
            .filter(|t| t.from_role == role || t.to_role == role)
            .collect()
    }

    /// Get touchpoints for a stage
    pub fn by_stage(&self, stage_id: &str) -> Vec<&Touchpoint> {
        self.touchpoints
            .iter()
            .filter(|t| t.stage_id.as_deref() == Some(stage_id))
            .collect()
    }

    /// Calculate average touchpoint duration
    pub fn average_duration(&self) -> f64 {
        if self.touchpoints.is_empty() {
            return 0.0;
        }
        let total: f64 = self.touchpoints.iter().map(|t| t.duration_ms).sum();
        total / self.touchpoints.len() as f64
    }

    /// Calculate average duration by type
    pub fn average_duration_by_type(&self, touchpoint_type: TouchpointType) -> f64 {
        let touchpoints = self.by_type(touchpoint_type);
        if touchpoints.is_empty() {
            return 0.0;
        }
        let total: f64 = touchpoints.iter().map(|t| t.duration_ms).sum();
        total / touchpoints.len() as f64
    }

    /// Get touchpoint success rate
    pub fn success_rate(&self) -> f64 {
        if self.touchpoints.is_empty() {
            return 0.0;
        }
        let successful = self.touchpoints.iter().filter(|t| t.is_successful()).count();
        successful as f64 / self.touchpoints.len() as f64
    }

    /// Get blocked touchpoints
    pub fn blocked(&self) -> Vec<&Touchpoint> {
        self.touchpoints
            .iter()
            .filter(|t| matches!(t.outcome, TouchpointOutcome::Blocked))
            .collect()
    }

    /// Count handoffs between specific stakeholders
    pub fn count_handoffs(&self, from: StakeholderRole, to: StakeholderRole) -> usize {
        self.touchpoints
            .iter()
            .filter(|t| {
                t.touchpoint_type == TouchpointType::Handoff
                    && t.from_role == from
                    && t.to_role == to
            })
            .count()
    }

    /// Get longest touchpoint
    pub fn longest_touchpoint(&self) -> Option<&Touchpoint> {
        self.touchpoints
            .iter()
            .max_by(|a, b| a.duration_ms.partial_cmp(&b.duration_ms).unwrap())
    }

    /// Get summary statistics
    pub fn summary(&self) -> String {
        format!(
            "{} touchpoints, avg duration: {:.2}ms, success rate: {:.1}%, {} blocked",
            self.touchpoints.len(),
            self.average_duration(),
            self.success_rate() * 100.0,
            self.blocked().len()
        )
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_touchpoint_creation() {
        let touchpoint = Touchpoint::new(
            TouchpointType::Handoff,
            StakeholderRole::Developer,
            StakeholderRole::QualityAssurance,
        )
        .with_duration(250.0)
        .with_description("Code review handoff");

        assert_eq!(touchpoint.touchpoint_type, TouchpointType::Handoff);
        assert_eq!(touchpoint.from_role, StakeholderRole::Developer);
        assert_eq!(touchpoint.to_role, StakeholderRole::QualityAssurance);
        assert_eq!(touchpoint.duration_ms, 250.0);
        assert!(touchpoint.is_pending());
    }

    #[test]
    fn test_touchpoint_lifecycle() {
        let mut touchpoint = Touchpoint::new(
            TouchpointType::Approval,
            StakeholderRole::Developer,
            StakeholderRole::GovernanceBoard,
        );

        assert!(touchpoint.is_pending());
        assert!(!touchpoint.is_successful());

        touchpoint.complete();
        assert!(touchpoint.is_successful());
        assert!(!touchpoint.is_pending());
    }

    #[test]
    fn test_touchpoint_collection() {
        let mut collection = TouchpointCollection::new();

        collection.add(
            Touchpoint::new(
                TouchpointType::Handoff,
                StakeholderRole::Developer,
                StakeholderRole::QualityAssurance,
            )
            .with_duration(100.0)
            .with_outcome(TouchpointOutcome::Completed),
        );

        collection.add(
            Touchpoint::new(
                TouchpointType::Review,
                StakeholderRole::QualityAssurance,
                StakeholderRole::Developer,
            )
            .with_duration(200.0)
            .with_outcome(TouchpointOutcome::Completed),
        );

        assert_eq!(collection.touchpoints.len(), 2);
        assert_eq!(collection.average_duration(), 150.0);
        assert_eq!(collection.success_rate(), 1.0);
    }

    #[test]
    fn test_touchpoint_filtering() {
        let mut collection = TouchpointCollection::new();

        collection.add(
            Touchpoint::new(
                TouchpointType::Handoff,
                StakeholderRole::Developer,
                StakeholderRole::QualityAssurance,
            )
            .with_duration(100.0),
        );

        collection.add(
            Touchpoint::new(
                TouchpointType::Review,
                StakeholderRole::QualityAssurance,
                StakeholderRole::Developer,
            )
            .with_duration(200.0),
        );

        let handoffs = collection.by_type(TouchpointType::Handoff);
        assert_eq!(handoffs.len(), 1);

        let qa_touchpoints = collection.by_stakeholder(StakeholderRole::QualityAssurance);
        assert_eq!(qa_touchpoints.len(), 2);

        assert_eq!(
            collection.count_handoffs(
                StakeholderRole::Developer,
                StakeholderRole::QualityAssurance
            ),
            1
        );
    }
}
