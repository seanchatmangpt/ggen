// Workflow state machine and Kanban board management

use super::{KanbanError, KanbanResult, KanbanEvent, KanbanEventType};
use chrono::{DateTime, Utc};
use serde::{Deserialize, Serialize};
use std::collections::{HashMap, VecDeque};

/// Stages in the ontology evolution workflow
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, Serialize, Deserialize)]
#[serde(rename_all = "snake_case")]
pub enum WorkflowStage {
    /// Backlog of pending ontology changes
    Backlog,
    /// Analyzing RDF schema impact
    Analysis,
    /// Semantic transformation in progress
    Transformation,
    /// Validating generated code and schema
    Validation,
    /// Code generation phase
    Generation,
    /// Completed and deployed
    Done,
}

impl WorkflowStage {
    /// Get the next stage in the workflow
    pub fn next(&self) -> Option<WorkflowStage> {
        match self {
            WorkflowStage::Backlog => Some(WorkflowStage::Analysis),
            WorkflowStage::Analysis => Some(WorkflowStage::Transformation),
            WorkflowStage::Transformation => Some(WorkflowStage::Validation),
            WorkflowStage::Validation => Some(WorkflowStage::Generation),
            WorkflowStage::Generation => Some(WorkflowStage::Done),
            WorkflowStage::Done => None,
        }
    }

    /// Get the previous stage in the workflow
    pub fn previous(&self) -> Option<WorkflowStage> {
        match self {
            WorkflowStage::Backlog => None,
            WorkflowStage::Analysis => Some(WorkflowStage::Backlog),
            WorkflowStage::Transformation => Some(WorkflowStage::Analysis),
            WorkflowStage::Validation => Some(WorkflowStage::Transformation),
            WorkflowStage::Generation => Some(WorkflowStage::Validation),
            WorkflowStage::Done => Some(WorkflowStage::Generation),
        }
    }

    /// Check if transition to target stage is valid
    pub fn can_transition_to(&self, target: &WorkflowStage) -> bool {
        match (self, target) {
            // Forward transitions
            (WorkflowStage::Backlog, WorkflowStage::Analysis) => true,
            (WorkflowStage::Analysis, WorkflowStage::Transformation) => true,
            (WorkflowStage::Transformation, WorkflowStage::Validation) => true,
            (WorkflowStage::Validation, WorkflowStage::Generation) => true,
            (WorkflowStage::Generation, WorkflowStage::Done) => true,

            // Backward transitions (rework)
            (_, WorkflowStage::Backlog) => true,
            (WorkflowStage::Transformation, WorkflowStage::Analysis) => true,
            (WorkflowStage::Validation, WorkflowStage::Transformation) => true,
            (WorkflowStage::Generation, WorkflowStage::Validation) => true,

            // Same stage (no-op)
            (a, b) if a == b => true,

            _ => false,
        }
    }

    pub fn as_str(&self) -> &str {
        match self {
            WorkflowStage::Backlog => "backlog",
            WorkflowStage::Analysis => "analysis",
            WorkflowStage::Transformation => "transformation",
            WorkflowStage::Validation => "validation",
            WorkflowStage::Generation => "generation",
            WorkflowStage::Done => "done",
        }
    }
}

/// State of a workflow stage
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct WorkflowState {
    pub stage: WorkflowStage,
    pub card_ids: Vec<String>,
    pub wip_limit: usize,
    pub cycle_time_avg: f64, // Average time in seconds
    pub throughput: f64,      // Items per hour
    pub blocked_count: usize,
}

impl WorkflowState {
    pub fn new(stage: WorkflowStage, wip_limit: usize) -> Self {
        Self {
            stage,
            card_ids: Vec::new(),
            wip_limit,
            cycle_time_avg: 0.0,
            throughput: 0.0,
            blocked_count: 0,
        }
    }

    /// Check if WIP limit is reached
    pub fn is_at_limit(&self) -> bool {
        self.card_ids.len() >= self.wip_limit
    }

    /// Get current WIP count
    pub fn wip_count(&self) -> usize {
        self.card_ids.len()
    }

    /// Add a card to this stage
    pub fn add_card(&mut self, card_id: String) -> KanbanResult<()> {
        if self.is_at_limit() {
            return Err(KanbanError::WipLimitExceeded(
                self.card_ids.len(),
                self.stage.as_str().to_string(),
                self.wip_limit,
            ));
        }
        self.card_ids.push(card_id);
        Ok(())
    }

    /// Remove a card from this stage
    pub fn remove_card(&mut self, card_id: &str) -> bool {
        if let Some(pos) = self.card_ids.iter().position(|id| id == card_id) {
            self.card_ids.remove(pos);
            true
        } else {
            false
        }
    }
}

/// Kanban board managing the entire workflow
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct KanbanBoard {
    pub stages: HashMap<WorkflowStage, WorkflowState>,
    pub events: VecDeque<KanbanEvent>,
    pub max_events: usize,
    pub created_at: DateTime<Utc>,
    pub last_updated: DateTime<Utc>,
}

impl KanbanBoard {
    /// Create a new Kanban board with default WIP limits
    pub fn new(wip_limits: HashMap<String, usize>) -> Self {
        let mut stages = HashMap::new();

        for stage in [
            WorkflowStage::Backlog,
            WorkflowStage::Analysis,
            WorkflowStage::Transformation,
            WorkflowStage::Validation,
            WorkflowStage::Generation,
            WorkflowStage::Done,
        ] {
            let limit = wip_limits
                .get(stage.as_str())
                .copied()
                .unwrap_or(10);
            stages.insert(stage, WorkflowState::new(stage, limit));
        }

        let now = Utc::now();
        Self {
            stages,
            events: VecDeque::new(),
            max_events: 1000,
            created_at: now,
            last_updated: now,
        }
    }

    /// Move a card to a new stage (pull system)
    pub fn move_card(
        &mut self,
        card_id: String,
        from: WorkflowStage,
        to: WorkflowStage,
    ) -> KanbanResult<()> {
        // Validate transition
        if !from.can_transition_to(&to) {
            return Err(KanbanError::InvalidTransition(
                from.as_str().to_string(),
                to.as_str().to_string(),
            ));
        }

        // Check WIP limit on destination
        let dest_state = self.stages.get(&to).ok_or_else(|| {
            KanbanError::StageNotFound(to.as_str().to_string())
        })?;

        if to != WorkflowStage::Done && dest_state.is_at_limit() {
            return Err(KanbanError::WipLimitExceeded(
                dest_state.wip_count(),
                to.as_str().to_string(),
                dest_state.wip_limit,
            ));
        }

        // Remove from source stage
        let source_state = self.stages.get_mut(&from).ok_or_else(|| {
            KanbanError::StageNotFound(from.as_str().to_string())
        })?;

        if !source_state.remove_card(&card_id) {
            return Err(KanbanError::CardNotFound(card_id));
        }

        // Add to destination stage
        let dest_state = self.stages.get_mut(&to).unwrap();
        dest_state.add_card(card_id.clone())?;

        // Record event
        self.add_event(KanbanEvent {
            timestamp: Utc::now(),
            event_type: KanbanEventType::CardMoved,
            card_id,
            stage: to.as_str().to_string(),
            metadata: {
                let mut map = HashMap::new();
                map.insert("from".to_string(), from.as_str().to_string());
                map.insert("to".to_string(), to.as_str().to_string());
                map
            },
        });

        self.last_updated = Utc::now();
        Ok(())
    }

    /// Pull a card from the previous stage (demand-driven)
    pub fn pull_card(&mut self, stage: WorkflowStage) -> KanbanResult<Option<String>> {
        let prev_stage = match stage.previous() {
            Some(s) => s,
            None => return Ok(None), // No previous stage
        };

        // Check if current stage has capacity
        let current_state = self.stages.get(&stage).ok_or_else(|| {
            KanbanError::StageNotFound(stage.as_str().to_string())
        })?;

        if current_state.is_at_limit() {
            return Ok(None); // At capacity, can't pull
        }

        // Get first card from previous stage
        let prev_state = self.stages.get(&prev_stage).ok_or_else(|| {
            KanbanError::StageNotFound(prev_stage.as_str().to_string())
        })?;

        let card_id = match prev_state.card_ids.first() {
            Some(id) => id.clone(),
            None => return Ok(None), // No cards to pull
        };

        // Move the card
        self.move_card(card_id.clone(), prev_stage, stage)?;

        Ok(Some(card_id))
    }

    /// Get the current state of a stage
    pub fn get_stage(&self, stage: WorkflowStage) -> Option<&WorkflowState> {
        self.stages.get(&stage)
    }

    /// Add an event to the event log
    fn add_event(&mut self, event: KanbanEvent) {
        self.events.push_back(event);
        if self.events.len() > self.max_events {
            self.events.pop_front();
        }
    }

    /// Get recent events
    pub fn recent_events(&self, count: usize) -> Vec<&KanbanEvent> {
        self.events.iter().rev().take(count).collect()
    }

    /// Calculate total WIP across all stages
    pub fn total_wip(&self) -> usize {
        self.stages.values().map(|s| s.wip_count()).sum()
    }

    /// Get stages that are at or near their WIP limit
    pub fn bottleneck_stages(&self) -> Vec<(WorkflowStage, f64)> {
        self.stages
            .iter()
            .filter_map(|(stage, state)| {
                let utilization = state.wip_count() as f64 / state.wip_limit as f64;
                if utilization >= 0.8 {
                    Some((*stage, utilization))
                } else {
                    None
                }
            })
            .collect()
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_workflow_stage_transitions() {
        assert!(WorkflowStage::Backlog.can_transition_to(&WorkflowStage::Analysis));
        assert!(WorkflowStage::Analysis.can_transition_to(&WorkflowStage::Transformation));
        assert!(!WorkflowStage::Backlog.can_transition_to(&WorkflowStage::Generation));

        // Backward transitions (rework) should be allowed
        assert!(WorkflowStage::Validation.can_transition_to(&WorkflowStage::Transformation));
    }

    #[test]
    fn test_workflow_state_wip_limit() {
        let mut state = WorkflowState::new(WorkflowStage::Transformation, 2);

        assert!(state.add_card("card-1".to_string()).is_ok());
        assert!(state.add_card("card-2".to_string()).is_ok());
        assert!(state.add_card("card-3".to_string()).is_err()); // Should hit WIP limit
    }

    #[test]
    fn test_kanban_board_move_card() {
        let mut wip_limits = HashMap::new();
        wip_limits.insert("backlog".to_string(), 10);
        wip_limits.insert("analysis".to_string(), 3);

        let mut board = KanbanBoard::new(wip_limits);

        // Add card to backlog
        board.stages.get_mut(&WorkflowStage::Backlog)
            .unwrap()
            .add_card("card-1".to_string())
            .unwrap();

        // Move to analysis
        let result = board.move_card(
            "card-1".to_string(),
            WorkflowStage::Backlog,
            WorkflowStage::Analysis,
        );
        assert!(result.is_ok());

        // Verify card is in analysis stage
        let analysis_state = board.get_stage(WorkflowStage::Analysis).unwrap();
        assert_eq!(analysis_state.wip_count(), 1);
        assert!(analysis_state.card_ids.contains(&"card-1".to_string()));
    }

    #[test]
    fn test_pull_system() {
        let mut wip_limits = HashMap::new();
        wip_limits.insert("backlog".to_string(), 10);
        wip_limits.insert("analysis".to_string(), 3);

        let mut board = KanbanBoard::new(wip_limits);

        // Add cards to backlog
        board.stages.get_mut(&WorkflowStage::Backlog)
            .unwrap()
            .add_card("card-1".to_string())
            .unwrap();

        // Pull card to analysis
        let pulled = board.pull_card(WorkflowStage::Analysis).unwrap();
        assert_eq!(pulled, Some("card-1".to_string()));

        // Verify card moved
        let analysis_state = board.get_stage(WorkflowStage::Analysis).unwrap();
        assert_eq!(analysis_state.wip_count(), 1);
    }

    #[test]
    fn test_bottleneck_detection() {
        let mut wip_limits = HashMap::new();
        wip_limits.insert("transformation".to_string(), 3);

        let mut board = KanbanBoard::new(wip_limits);

        // Fill transformation stage to 90% capacity
        let stage = board.stages.get_mut(&WorkflowStage::Transformation).unwrap();
        stage.add_card("card-1".to_string()).unwrap();
        stage.add_card("card-2".to_string()).unwrap();
        stage.add_card("card-3".to_string()).unwrap();

        let bottlenecks = board.bottleneck_stages();
        assert!(!bottlenecks.is_empty());
        assert!(bottlenecks.iter().any(|(s, _)| *s == WorkflowStage::Transformation));
    }
}
