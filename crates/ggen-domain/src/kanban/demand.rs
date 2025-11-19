// Demand-driven code generation system preventing overproduction

use super::{WorkflowCard, WorkflowStage, KanbanBoard, KanbanError, KanbanResult};
use super::card::{Priority, RdfChangeType};
use chrono::{DateTime, Utc, Duration};
use serde::{Deserialize, Serialize};
use std::collections::{HashMap, VecDeque};

/// Signal indicating demand for code generation
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct DemandSignal {
    pub id: String,
    pub signal_type: DemandSignalType,
    pub priority: Priority,
    pub requested_at: DateTime<Utc>,
    pub required_by: Option<DateTime<Utc>>,
    pub source: DemandSource,
    pub metadata: HashMap<String, String>,
}

/// Type of demand signal
#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
#[serde(rename_all = "snake_case")]
pub enum DemandSignalType {
    /// Explicit request for ontology change
    ExplicitRequest {
        description: String,
        affected_templates: Vec<String>,
    },
    /// Change detected in RDF schema
    SchemaChange {
        change_type: String,
        ontology_uri: String,
    },
    /// Dependency on another system
    DependencyPull {
        system: String,
        version: String,
    },
    /// Template usage detected
    TemplateUsage {
        template_path: String,
        usage_count: usize,
    },
    /// Scheduled maintenance or update
    ScheduledUpdate {
        schedule: String,
        description: String,
    },
}

/// Source of demand signal
#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
#[serde(rename_all = "snake_case")]
pub enum DemandSource {
    User,
    System,
    External,
    Automated,
}

impl DemandSignal {
    /// Create a new demand signal
    pub fn new(
        signal_type: DemandSignalType,
        priority: Priority,
        source: DemandSource,
    ) -> Self {
        Self {
            id: uuid::Uuid::new_v4().to_string(),
            signal_type,
            priority,
            requested_at: Utc::now(),
            required_by: None,
            source,
            metadata: HashMap::new(),
        }
    }

    /// Set a deadline for this demand
    pub fn with_deadline(mut self, deadline: DateTime<Utc>) -> Self {
        self.required_by = Some(deadline);
        self
    }

    /// Check if demand is overdue
    pub fn is_overdue(&self) -> bool {
        self.required_by
            .map(|deadline| Utc::now() > deadline)
            .unwrap_or(false)
    }

    /// Get urgency score (0.0 to 1.0)
    pub fn urgency_score(&self) -> f64 {
        let mut score = match self.priority {
            Priority::Low => 0.25,
            Priority::Medium => 0.5,
            Priority::High => 0.75,
            Priority::Critical => 1.0,
        };

        // Increase score if approaching deadline
        if let Some(deadline) = self.required_by {
            let time_remaining = deadline.signed_duration_since(Utc::now());
            let hours_remaining = time_remaining.num_hours() as f64;

            if hours_remaining < 24.0 {
                score += 0.1;
            }
            if hours_remaining < 4.0 {
                score += 0.15;
            }
        }

        score.min(1.0)
    }
}

/// Pull system for demand-driven generation
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct PullSystem {
    /// Queue of pending demand signals
    demand_queue: VecDeque<DemandSignal>,
    /// Active work items (card ID -> demand signal)
    active_work: HashMap<String, DemandSignal>,
    /// Maximum concurrent work items
    max_concurrent_work: usize,
    /// Demand signal history
    signal_history: Vec<DemandSignal>,
    /// Statistics
    stats: PullSystemStats,
}

impl PullSystem {
    /// Create a new pull system
    pub fn new(max_concurrent_work: usize) -> Self {
        Self {
            demand_queue: VecDeque::new(),
            active_work: HashMap::new(),
            max_concurrent_work,
            signal_history: Vec::new(),
            stats: PullSystemStats::default(),
        }
    }

    /// Add a demand signal to the queue
    pub fn signal_demand(&mut self, signal: DemandSignal) {
        self.stats.total_signals_received += 1;
        self.demand_queue.push_back(signal);

        // Sort queue by urgency
        let mut signals: Vec<_> = self.demand_queue.drain(..).collect();
        signals.sort_by(|a, b| {
            b.urgency_score()
                .partial_cmp(&a.urgency_score())
                .unwrap_or(std::cmp::Ordering::Equal)
        });

        self.demand_queue = signals.into();
    }

    /// Check if system can pull new work
    pub fn can_pull(&self) -> bool {
        self.active_work.len() < self.max_concurrent_work && !self.demand_queue.is_empty()
    }

    /// Pull the next demand signal and create a workflow card
    pub fn pull_next(&mut self) -> Option<(DemandSignal, WorkflowCard)> {
        if !self.can_pull() {
            return None;
        }

        let signal = self.demand_queue.pop_front()?;
        let card = self.create_card_from_signal(&signal);

        self.active_work.insert(card.id.clone(), signal.clone());
        self.stats.total_work_pulled += 1;

        Some((signal, card))
    }

    /// Complete work for a card
    pub fn complete_work(&mut self, card_id: &str) -> KanbanResult<()> {
        let signal = self.active_work.remove(card_id).ok_or_else(|| {
            KanbanError::CardNotFound(card_id.to_string())
        })?;

        self.signal_history.push(signal);
        self.stats.total_work_completed += 1;

        Ok(())
    }

    /// Create a workflow card from a demand signal
    fn create_card_from_signal(&self, signal: &DemandSignal) -> WorkflowCard {
        let (title, description, rdf_changes, affected_templates) = match &signal.signal_type {
            DemandSignalType::ExplicitRequest {
                description,
                affected_templates,
            } => (
                "Explicit Request".to_string(),
                description.clone(),
                Vec::new(),
                affected_templates.clone(),
            ),
            DemandSignalType::SchemaChange {
                change_type,
                ontology_uri,
            } => (
                format!("Schema Change: {}", change_type),
                format!("Change in ontology: {}", ontology_uri),
                vec![self.parse_rdf_change(change_type, ontology_uri)],
                Vec::new(),
            ),
            DemandSignalType::DependencyPull { system, version } => (
                format!("Dependency Pull: {}", system),
                format!("Update dependency {} to version {}", system, version),
                Vec::new(),
                Vec::new(),
            ),
            DemandSignalType::TemplateUsage {
                template_path,
                usage_count,
            } => (
                format!("Template Usage: {}", template_path),
                format!("Template used {} times", usage_count),
                Vec::new(),
                vec![template_path.clone()],
            ),
            DemandSignalType::ScheduledUpdate {
                schedule,
                description,
            } => (
                format!("Scheduled Update: {}", schedule),
                description.clone(),
                Vec::new(),
                Vec::new(),
            ),
        };

        let mut card = WorkflowCard::new(title, description, signal.priority);

        for change in rdf_changes {
            card.add_rdf_change(change);
        }

        for template in affected_templates {
            card.add_affected_template(template);
        }

        card
    }

    /// Parse RDF change from type string
    fn parse_rdf_change(&self, change_type: &str, uri: &str) -> RdfChangeType {
        if change_type.contains("class_added") || change_type.contains("ClassAdded") {
            RdfChangeType::ClassAdded {
                class_uri: uri.to_string(),
            }
        } else if change_type.contains("property_added") || change_type.contains("PropertyAdded") {
            RdfChangeType::PropertyAdded {
                property_uri: uri.to_string(),
            }
        } else {
            RdfChangeType::ClassModified {
                class_uri: uri.to_string(),
            }
        }
    }

    /// Get current active work count
    pub fn active_work_count(&self) -> usize {
        self.active_work.len()
    }

    /// Get pending demand count
    pub fn pending_demand_count(&self) -> usize {
        self.demand_queue.len()
    }

    /// Get statistics
    pub fn stats(&self) -> &PullSystemStats {
        &self.stats
    }

    /// Get all pending signals
    pub fn pending_signals(&self) -> Vec<&DemandSignal> {
        self.demand_queue.iter().collect()
    }

    /// Get overdue signals
    pub fn overdue_signals(&self) -> Vec<&DemandSignal> {
        self.demand_queue
            .iter()
            .filter(|s| s.is_overdue())
            .collect()
    }
}

/// Statistics for pull system
#[derive(Debug, Clone, Default, Serialize, Deserialize)]
pub struct PullSystemStats {
    pub total_signals_received: usize,
    pub total_work_pulled: usize,
    pub total_work_completed: usize,
}

/// Demand-driven generator coordinating the pull system with Kanban board
pub struct DemandDrivenGenerator {
    pull_system: PullSystem,
    board: KanbanBoard,
    auto_pull_enabled: bool,
    pull_interval: Duration,
    last_pull: Option<DateTime<Utc>>,
}

impl DemandDrivenGenerator {
    /// Create a new demand-driven generator
    pub fn new(
        board: KanbanBoard,
        max_concurrent_work: usize,
        auto_pull_enabled: bool,
    ) -> Self {
        Self {
            pull_system: PullSystem::new(max_concurrent_work),
            board,
            auto_pull_enabled,
            pull_interval: Duration::seconds(30),
            last_pull: None,
        }
    }

    /// Signal demand for generation
    pub fn signal_demand(&mut self, signal: DemandSignal) {
        self.pull_system.signal_demand(signal);

        // Trigger immediate pull if auto-pull is enabled
        if self.auto_pull_enabled {
            let _ = self.try_pull();
        }
    }

    /// Try to pull work from demand queue
    pub fn try_pull(&mut self) -> KanbanResult<Option<String>> {
        if !self.pull_system.can_pull() {
            return Ok(None);
        }

        // Check pull interval
        if let Some(last) = self.last_pull {
            let elapsed = Utc::now().signed_duration_since(last);
            if elapsed < self.pull_interval {
                return Ok(None);
            }
        }

        let (signal, card) = match self.pull_system.pull_next() {
            Some(x) => x,
            None => return Ok(None),
        };

        let card_id = card.id.clone();

        // Add card to board's backlog
        let backlog_state = self.board.stages.get_mut(&WorkflowStage::Backlog)
            .ok_or_else(|| {
                KanbanError::StageNotFound("backlog".to_string())
            })?;

        backlog_state.add_card(card_id.clone())?;

        self.last_pull = Some(Utc::now());

        Ok(Some(card_id))
    }

    /// Complete work for a card
    pub fn complete_work(&mut self, card_id: &str) -> KanbanResult<()> {
        self.pull_system.complete_work(card_id)
    }

    /// Get the Kanban board
    pub fn board(&self) -> &KanbanBoard {
        &self.board
    }

    /// Get mutable Kanban board
    pub fn board_mut(&mut self) -> &mut KanbanBoard {
        &mut self.board
    }

    /// Get the pull system
    pub fn pull_system(&self) -> &PullSystem {
        &self.pull_system
    }

    /// Check for overproduction (work being done without demand)
    pub fn detect_overproduction(&self) -> Option<OverproductionWarning> {
        let total_wip = self.board.total_wip();
        let pending_demand = self.pull_system.pending_demand_count();
        let active_work = self.pull_system.active_work_count();

        // If WIP is significantly higher than demand + active work
        if total_wip > (pending_demand + active_work + 5) {
            Some(OverproductionWarning {
                current_wip: total_wip,
                pending_demand,
                active_work,
                excess_wip: total_wip - (pending_demand + active_work),
                recommendation: "Reduce WIP limits or stop pulling new work until demand increases".to_string(),
            })
        } else {
            None
        }
    }

    /// Set auto-pull interval
    pub fn set_pull_interval(&mut self, interval: Duration) {
        self.pull_interval = interval;
    }

    /// Enable or disable auto-pull
    pub fn set_auto_pull(&mut self, enabled: bool) {
        self.auto_pull_enabled = enabled;
    }
}

/// Warning about overproduction detected
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct OverproductionWarning {
    pub current_wip: usize,
    pub pending_demand: usize,
    pub active_work: usize,
    pub excess_wip: usize,
    pub recommendation: String,
}

#[cfg(test)]
mod tests {
    use super::*;
    use super::super::KanbanConfig;

    #[test]
    fn test_demand_signal_creation() {
        let signal = DemandSignal::new(
            DemandSignalType::ExplicitRequest {
                description: "Add new feature".to_string(),
                affected_templates: vec!["template.hbs".to_string()],
            },
            Priority::High,
            DemandSource::User,
        );

        assert_eq!(signal.priority, Priority::High);
        assert!(!signal.is_overdue());
    }

    #[test]
    fn test_demand_signal_urgency() {
        let signal = DemandSignal::new(
            DemandSignalType::ExplicitRequest {
                description: "Test".to_string(),
                affected_templates: vec![],
            },
            Priority::Critical,
            DemandSource::User,
        );

        let urgency = signal.urgency_score();
        assert!(urgency >= 0.75); // Critical priority
    }

    #[test]
    fn test_pull_system_signal_demand() {
        let mut pull_system = PullSystem::new(3);

        let signal = DemandSignal::new(
            DemandSignalType::SchemaChange {
                change_type: "class_added".to_string(),
                ontology_uri: "http://example.org/User".to_string(),
            },
            Priority::Medium,
            DemandSource::System,
        );

        pull_system.signal_demand(signal);
        assert_eq!(pull_system.pending_demand_count(), 1);
        assert!(pull_system.can_pull());
    }

    #[test]
    fn test_pull_system_pull_next() {
        let mut pull_system = PullSystem::new(3);

        let signal = DemandSignal::new(
            DemandSignalType::ExplicitRequest {
                description: "Test".to_string(),
                affected_templates: vec![],
            },
            Priority::High,
            DemandSource::User,
        );

        pull_system.signal_demand(signal);

        let result = pull_system.pull_next();
        assert!(result.is_some());

        let (signal, card) = result.unwrap();
        assert_eq!(signal.priority, Priority::High);
        assert_eq!(pull_system.active_work_count(), 1);
    }

    #[test]
    fn test_pull_system_max_concurrent() {
        let mut pull_system = PullSystem::new(2);

        // Add 3 signals
        for i in 0..3 {
            let signal = DemandSignal::new(
                DemandSignalType::ExplicitRequest {
                    description: format!("Test {}", i),
                    affected_templates: vec![],
                },
                Priority::Medium,
                DemandSource::User,
            );
            pull_system.signal_demand(signal);
        }

        // Should pull 2 (max concurrent)
        assert!(pull_system.pull_next().is_some());
        assert!(pull_system.pull_next().is_some());

        // Should not pull 3rd (at limit)
        assert!(!pull_system.can_pull());
        assert!(pull_system.pull_next().is_none());
    }

    #[test]
    fn test_demand_driven_generator() {
        let config = KanbanConfig::default();
        let board = KanbanBoard::new(config.wip_limits);
        let mut generator = DemandDrivenGenerator::new(board, 3, true);

        let signal = DemandSignal::new(
            DemandSignalType::TemplateUsage {
                template_path: "templates/user.hbs".to_string(),
                usage_count: 5,
            },
            Priority::Medium,
            DemandSource::System,
        );

        generator.signal_demand(signal);

        // Auto-pull should have triggered
        let backlog_state = generator.board().get_stage(WorkflowStage::Backlog).unwrap();
        assert!(backlog_state.wip_count() > 0);
    }

    #[test]
    fn test_overproduction_detection() {
        let config = KanbanConfig::default();
        let mut board = KanbanBoard::new(config.wip_limits);

        // Add excessive WIP without corresponding demand
        for i in 0..15 {
            board.stages.get_mut(&WorkflowStage::Analysis)
                .unwrap()
                .add_card(format!("card-{}", i))
                .unwrap();
        }

        let generator = DemandDrivenGenerator::new(board, 3, false);

        let warning = generator.detect_overproduction();
        assert!(warning.is_some());

        let warning = warning.unwrap();
        assert!(warning.excess_wip > 0);
    }

    #[test]
    fn test_signal_priority_sorting() {
        let mut pull_system = PullSystem::new(5);

        // Add signals with different priorities
        let low = DemandSignal::new(
            DemandSignalType::ExplicitRequest {
                description: "Low priority".to_string(),
                affected_templates: vec![],
            },
            Priority::Low,
            DemandSource::User,
        );

        let high = DemandSignal::new(
            DemandSignalType::ExplicitRequest {
                description: "High priority".to_string(),
                affected_templates: vec![],
            },
            Priority::High,
            DemandSource::User,
        );

        let critical = DemandSignal::new(
            DemandSignalType::ExplicitRequest {
                description: "Critical priority".to_string(),
                affected_templates: vec![],
            },
            Priority::Critical,
            DemandSource::User,
        );

        // Add in random order
        pull_system.signal_demand(low);
        pull_system.signal_demand(high);
        pull_system.signal_demand(critical);

        // Should pull critical first
        let (signal, _) = pull_system.pull_next().unwrap();
        assert_eq!(signal.priority, Priority::Critical);
    }
}
