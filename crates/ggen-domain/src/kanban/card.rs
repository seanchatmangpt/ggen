// Workflow cards for tracking RDF schema changes

use super::{WorkflowStage, KanbanError, KanbanResult};
use chrono::{DateTime, Utc, Duration};
use serde::{Deserialize, Serialize};
use std::collections::HashMap;
use uuid::Uuid;

/// Status of a workflow card
#[derive(Debug, Clone, Copy, PartialEq, Eq, Serialize, Deserialize)]
#[serde(rename_all = "snake_case")]
pub enum CardStatus {
    Active,
    Blocked,
    Completed,
    Abandoned,
}

/// Priority level for workflow cards
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Serialize, Deserialize)]
#[serde(rename_all = "snake_case")]
pub enum Priority {
    Low,
    Medium,
    High,
    Critical,
}

/// Type of RDF schema change
#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
#[serde(rename_all = "snake_case")]
pub enum RdfChangeType {
    /// Adding a new class to the ontology
    ClassAdded { class_uri: String },
    /// Removing a class from the ontology
    ClassRemoved { class_uri: String },
    /// Modifying an existing class
    ClassModified { class_uri: String },
    /// Adding a new property
    PropertyAdded { property_uri: String },
    /// Removing a property
    PropertyRemoved { property_uri: String },
    /// Modifying a property
    PropertyModified { property_uri: String },
    /// Adding a relationship between classes
    RelationshipAdded { from: String, to: String, predicate: String },
    /// Removing a relationship
    RelationshipRemoved { from: String, to: String, predicate: String },
    /// Changing cardinality constraints
    CardinalityChanged { class_uri: String, property: String },
    /// Adding SPARQL query template
    QueryTemplateAdded { name: String },
    /// Modifying namespace
    NamespaceModified { old_ns: String, new_ns: String },
}

/// Workflow card tracking an ontology evolution task
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct WorkflowCard {
    pub id: String,
    pub title: String,
    pub description: String,
    pub status: CardStatus,
    pub priority: Priority,
    pub stage: WorkflowStage,
    pub rdf_changes: Vec<RdfChangeType>,
    pub affected_templates: Vec<String>,
    pub affected_files: Vec<String>,
    pub created_at: DateTime<Utc>,
    pub updated_at: DateTime<Utc>,
    pub started_at: Option<DateTime<Utc>>,
    pub completed_at: Option<DateTime<Utc>>,
    pub blocked_reason: Option<String>,
    pub assignee: Option<String>,
    pub tags: Vec<String>,
    pub metadata: HashMap<String, String>,
    pub cycle_times: HashMap<String, f64>, // Stage -> seconds
}

impl WorkflowCard {
    /// Create a new workflow card
    pub fn new(title: String, description: String, priority: Priority) -> Self {
        let now = Utc::now();
        Self {
            id: Uuid::new_v4().to_string(),
            title,
            description,
            status: CardStatus::Active,
            priority,
            stage: WorkflowStage::Backlog,
            rdf_changes: Vec::new(),
            affected_templates: Vec::new(),
            affected_files: Vec::new(),
            created_at: now,
            updated_at: now,
            started_at: None,
            completed_at: None,
            blocked_reason: None,
            assignee: None,
            tags: Vec::new(),
            metadata: HashMap::new(),
            cycle_times: HashMap::new(),
        }
    }

    /// Add an RDF schema change to track
    pub fn add_rdf_change(&mut self, change: RdfChangeType) {
        self.rdf_changes.push(change);
        self.updated_at = Utc::now();
    }

    /// Add an affected template
    pub fn add_affected_template(&mut self, template_path: String) {
        if !self.affected_templates.contains(&template_path) {
            self.affected_templates.push(template_path);
            self.updated_at = Utc::now();
        }
    }

    /// Add an affected file
    pub fn add_affected_file(&mut self, file_path: String) {
        if !self.affected_files.contains(&file_path) {
            self.affected_files.push(file_path);
            self.updated_at = Utc::now();
        }
    }

    /// Move card to a new stage
    pub fn move_to_stage(&mut self, new_stage: WorkflowStage) -> KanbanResult<()> {
        if !self.stage.can_transition_to(&new_stage) {
            return Err(KanbanError::InvalidTransition(
                self.stage.as_str().to_string(),
                new_stage.as_str().to_string(),
            ));
        }

        // Record cycle time for previous stage
        if let Some(started) = self.started_at {
            let elapsed = Utc::now().signed_duration_since(started);
            self.cycle_times.insert(
                self.stage.as_str().to_string(),
                elapsed.num_seconds() as f64,
            );
        }

        self.stage = new_stage;
        self.started_at = Some(Utc::now());
        self.updated_at = Utc::now();

        // Update status if moving to Done
        if new_stage == WorkflowStage::Done {
            self.status = CardStatus::Completed;
            self.completed_at = Some(Utc::now());
        }

        Ok(())
    }

    /// Block the card with a reason
    pub fn block(&mut self, reason: String) {
        self.status = CardStatus::Blocked;
        self.blocked_reason = Some(reason);
        self.updated_at = Utc::now();
    }

    /// Unblock the card
    pub fn unblock(&mut self) {
        self.status = CardStatus::Active;
        self.blocked_reason = None;
        self.updated_at = Utc::now();
    }

    /// Calculate total cycle time (from creation to completion)
    pub fn total_cycle_time(&self) -> Option<Duration> {
        self.completed_at.map(|completed| {
            completed.signed_duration_since(self.created_at)
        })
    }

    /// Calculate lead time (from start to completion)
    pub fn lead_time(&self) -> Option<Duration> {
        match (self.started_at, self.completed_at) {
            (Some(start), Some(end)) => Some(end.signed_duration_since(start)),
            _ => None,
        }
    }

    /// Get age of the card in hours
    pub fn age_hours(&self) -> f64 {
        let now = Utc::now();
        let duration = now.signed_duration_since(self.created_at);
        duration.num_seconds() as f64 / 3600.0
    }

    /// Check if card is overdue based on SLA (hours)
    pub fn is_overdue(&self, sla_hours: f64) -> bool {
        self.status != CardStatus::Completed && self.age_hours() > sla_hours
    }

    /// Get a summary of RDF changes
    pub fn change_summary(&self) -> String {
        let mut summary = Vec::new();
        let mut class_added = 0;
        let mut class_removed = 0;
        let mut property_added = 0;
        let mut property_removed = 0;
        let mut other = 0;

        for change in &self.rdf_changes {
            match change {
                RdfChangeType::ClassAdded { .. } => class_added += 1,
                RdfChangeType::ClassRemoved { .. } => class_removed += 1,
                RdfChangeType::PropertyAdded { .. } => property_added += 1,
                RdfChangeType::PropertyRemoved { .. } => property_removed += 1,
                _ => other += 1,
            }
        }

        if class_added > 0 {
            summary.push(format!("+{} classes", class_added));
        }
        if class_removed > 0 {
            summary.push(format!("-{} classes", class_removed));
        }
        if property_added > 0 {
            summary.push(format!("+{} properties", property_added));
        }
        if property_removed > 0 {
            summary.push(format!("-{} properties", property_removed));
        }
        if other > 0 {
            summary.push(format!("{} other changes", other));
        }

        if summary.is_empty() {
            "No RDF changes".to_string()
        } else {
            summary.join(", ")
        }
    }
}

/// Tracker for RDF schema changes across multiple cards
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct RdfChangeTracker {
    cards: HashMap<String, WorkflowCard>,
    changes_by_type: HashMap<String, Vec<String>>, // Change type -> card IDs
}

impl RdfChangeTracker {
    pub fn new() -> Self {
        Self {
            cards: HashMap::new(),
            changes_by_type: HashMap::new(),
        }
    }

    /// Add a card to track
    pub fn add_card(&mut self, card: WorkflowCard) {
        for change in &card.rdf_changes {
            let change_key = format!("{:?}", change);
            self.changes_by_type
                .entry(change_key)
                .or_insert_with(Vec::new)
                .push(card.id.clone());
        }
        self.cards.insert(card.id.clone(), card);
    }

    /// Get a card by ID
    pub fn get_card(&self, id: &str) -> Option<&WorkflowCard> {
        self.cards.get(id)
    }

    /// Get a mutable card by ID
    pub fn get_card_mut(&mut self, id: &str) -> Option<&mut WorkflowCard> {
        self.cards.get_mut(id)
    }

    /// Find cards affecting a specific template
    pub fn cards_affecting_template(&self, template_path: &str) -> Vec<&WorkflowCard> {
        self.cards
            .values()
            .filter(|card| card.affected_templates.contains(&template_path.to_string()))
            .collect()
    }

    /// Find cards with a specific type of RDF change
    pub fn cards_with_change_type(&self, change_type: &str) -> Vec<&WorkflowCard> {
        self.changes_by_type
            .get(change_type)
            .map(|card_ids| {
                card_ids
                    .iter()
                    .filter_map(|id| self.cards.get(id))
                    .collect()
            })
            .unwrap_or_default()
    }

    /// Get all active cards
    pub fn active_cards(&self) -> Vec<&WorkflowCard> {
        self.cards
            .values()
            .filter(|card| card.status == CardStatus::Active)
            .collect()
    }

    /// Get all blocked cards
    pub fn blocked_cards(&self) -> Vec<&WorkflowCard> {
        self.cards
            .values()
            .filter(|card| card.status == CardStatus::Blocked)
            .collect()
    }

    /// Get statistics about tracked changes
    pub fn statistics(&self) -> ChangeStatistics {
        let total_cards = self.cards.len();
        let active = self.active_cards().len();
        let blocked = self.blocked_cards().len();
        let completed = self.cards
            .values()
            .filter(|c| c.status == CardStatus::Completed)
            .count();

        let avg_cycle_time = {
            let completed_cards: Vec<_> = self.cards
                .values()
                .filter(|c| c.completed_at.is_some())
                .collect();

            if completed_cards.is_empty() {
                0.0
            } else {
                let total: f64 = completed_cards
                    .iter()
                    .filter_map(|c| c.total_cycle_time())
                    .map(|d| d.num_seconds() as f64)
                    .sum();
                total / completed_cards.len() as f64
            }
        };

        ChangeStatistics {
            total_cards,
            active,
            blocked,
            completed,
            avg_cycle_time_seconds: avg_cycle_time,
            total_rdf_changes: self.cards.values().map(|c| c.rdf_changes.len()).sum(),
        }
    }
}

impl Default for RdfChangeTracker {
    fn default() -> Self {
        Self::new()
    }
}

/// Statistics about tracked RDF changes
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct ChangeStatistics {
    pub total_cards: usize,
    pub active: usize,
    pub blocked: usize,
    pub completed: usize,
    pub avg_cycle_time_seconds: f64,
    pub total_rdf_changes: usize,
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_workflow_card_creation() {
        let card = WorkflowCard::new(
            "Add User class".to_string(),
            "Add User class to ontology".to_string(),
            Priority::High,
        );

        assert_eq!(card.stage, WorkflowStage::Backlog);
        assert_eq!(card.status, CardStatus::Active);
        assert_eq!(card.priority, Priority::High);
    }

    #[test]
    fn test_rdf_change_tracking() {
        let mut card = WorkflowCard::new(
            "Add User class".to_string(),
            "Description".to_string(),
            Priority::Medium,
        );

        card.add_rdf_change(RdfChangeType::ClassAdded {
            class_uri: "http://example.org/User".to_string(),
        });

        assert_eq!(card.rdf_changes.len(), 1);
        let summary = card.change_summary();
        assert!(summary.contains("+1 classes"));
    }

    #[test]
    fn test_card_stage_transition() {
        let mut card = WorkflowCard::new(
            "Test".to_string(),
            "Test".to_string(),
            Priority::Low,
        );

        assert!(card.move_to_stage(WorkflowStage::Analysis).is_ok());
        assert_eq!(card.stage, WorkflowStage::Analysis);

        // Invalid transition
        assert!(card.move_to_stage(WorkflowStage::Generation).is_err());
    }

    #[test]
    fn test_card_blocking() {
        let mut card = WorkflowCard::new(
            "Test".to_string(),
            "Test".to_string(),
            Priority::Low,
        );

        card.block("Waiting for dependency".to_string());
        assert_eq!(card.status, CardStatus::Blocked);
        assert!(card.blocked_reason.is_some());

        card.unblock();
        assert_eq!(card.status, CardStatus::Active);
        assert!(card.blocked_reason.is_none());
    }

    #[test]
    fn test_rdf_change_tracker() {
        let mut tracker = RdfChangeTracker::new();

        let mut card1 = WorkflowCard::new(
            "Card 1".to_string(),
            "Test".to_string(),
            Priority::High,
        );
        card1.add_affected_template("templates/user.hbs".to_string());
        tracker.add_card(card1);

        let mut card2 = WorkflowCard::new(
            "Card 2".to_string(),
            "Test".to_string(),
            Priority::Low,
        );
        card2.add_affected_template("templates/user.hbs".to_string());
        tracker.add_card(card2);

        let affected = tracker.cards_affecting_template("templates/user.hbs");
        assert_eq!(affected.len(), 2);

        let stats = tracker.statistics();
        assert_eq!(stats.total_cards, 2);
        assert_eq!(stats.active, 2);
    }
}
