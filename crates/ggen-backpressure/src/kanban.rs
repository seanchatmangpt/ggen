//! Kanban board with pull-only release and WIP limits
//!
//! Implements flow control where work is pulled from upstream stages
//! only when downstream capacity is available.

use crate::{BackpressureError, Result, WIPToken};
use std::collections::HashMap;
use std::sync::Arc;
use tokio::sync::RwLock;

/// Kanban stage identifier
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum Stage {
    Backlog,
    Ready,
    InProgress,
    Review,
    Done,
}

impl Stage {
    /// Get the next stage in the workflow
    pub fn next(&self) -> Option<Stage> {
        match self {
            Stage::Backlog => Some(Stage::Ready),
            Stage::Ready => Some(Stage::InProgress),
            Stage::InProgress => Some(Stage::Review),
            Stage::Review => Some(Stage::Done),
            Stage::Done => None,
        }
    }

    /// Get the previous stage in the workflow
    pub fn prev(&self) -> Option<Stage> {
        match self {
            Stage::Backlog => None,
            Stage::Ready => Some(Stage::Backlog),
            Stage::InProgress => Some(Stage::Ready),
            Stage::Review => Some(Stage::InProgress),
            Stage::Done => Some(Stage::Review),
        }
    }
}

/// Work item on the kanban board
#[derive(Debug, Clone)]
pub struct WorkItem {
    pub id: String,
    pub stage: Stage,
    pub token: Option<u64>, // Token ID if holding capacity
}

/// Kanban board with strict WIP limits per stage
#[derive(Debug)]
pub struct KanbanBoard {
    stages: Arc<RwLock<HashMap<Stage, StageInfo>>>,
}

#[derive(Debug)]
struct StageInfo {
    wip_limit: usize,
    items: Vec<WorkItem>,
    pool: crate::token::TokenPool,
}

impl StageInfo {
    fn new(wip_limit: usize) -> Self {
        Self {
            wip_limit,
            items: Vec::new(),
            pool: crate::token::TokenPool::new(wip_limit),
        }
    }

    fn count(&self) -> usize {
        self.items.len()
    }

    fn has_capacity(&self) -> bool {
        self.count() < self.wip_limit
    }
}

/// Configuration for kanban board
#[derive(Debug, Clone)]
pub struct KanbanConfig {
    pub ready_limit: usize,
    pub in_progress_limit: usize,
    pub review_limit: usize,
}

impl Default for KanbanConfig {
    fn default() -> Self {
        Self {
            ready_limit: 5,
            in_progress_limit: 3,
            review_limit: 2,
        }
    }
}

impl KanbanBoard {
    /// Create a new kanban board with the given configuration
    pub fn new(config: KanbanConfig) -> Self {
        let mut stages = HashMap::new();

        // Backlog and Done have very large capacity (effectively unlimited)
        const LARGE_CAPACITY: usize = 1_000_000;
        stages.insert(Stage::Backlog, StageInfo::new(LARGE_CAPACITY));
        stages.insert(Stage::Ready, StageInfo::new(config.ready_limit));
        stages.insert(Stage::InProgress, StageInfo::new(config.in_progress_limit));
        stages.insert(Stage::Review, StageInfo::new(config.review_limit));
        stages.insert(Stage::Done, StageInfo::new(LARGE_CAPACITY));

        Self {
            stages: Arc::new(RwLock::new(stages)),
        }
    }

    /// Create with default configuration
    pub fn with_defaults() -> Self {
        Self::new(KanbanConfig::default())
    }

    /// Pull work from one stage to the next
    ///
    /// This is the only way to move work - push is not allowed.
    /// Returns error if downstream stage is at capacity.
    pub async fn pull(&self, item_id: &str) -> Result<WIPToken> {
        let mut stages = self.stages.write().await;

        // Find the item
        let current_stage = stages
            .values()
            .find(|s| s.items.iter().any(|i| i.id == item_id))
            .map(|s| s.items.iter().find(|i| i.id == item_id).unwrap().stage)
            .ok_or_else(|| BackpressureError::CapacityExceeded(format!("item not found: {}", item_id)))?;

        let next_stage = current_stage
            .next()
            .ok_or_else(|| BackpressureError::CapacityExceeded("already at final stage".to_string()))?;

        // Check downstream capacity
        let next_info = stages.get(&next_stage)
            .ok_or_else(|| BackpressureError::CapacityExceeded("stage not found".to_string()))?;

        if !next_info.has_capacity() {
            return Err(BackpressureError::CapacityExceeded(
                format!("stage {:?} at capacity", next_stage)
            ));
        }

        // Acquire token for next stage
        let token = stages.get(&next_stage)
            .ok_or_else(|| BackpressureError::CapacityExceeded("stage not found".to_string()))?
            .pool.try_acquire()?
            .ok_or_else(|| BackpressureError::CapacityExceeded(format!("no capacity in {:?}", next_stage)))?;

        let token_id = token.metadata().id;

        // Remove from current stage
        let current_info = stages.get_mut(&current_stage)
            .ok_or_else(|| BackpressureError::CapacityExceeded("stage not found".to_string()))?;
        current_info.items.retain(|i| i.id != item_id);

        // Add to next stage
        let next_info = stages.get_mut(&next_stage)
            .ok_or_else(|| BackpressureError::CapacityExceeded("stage not found".to_string()))?;
        next_info.items.push(WorkItem {
            id: item_id.to_string(),
            stage: next_stage,
            token: Some(token_id),
        });

        Ok(token)
    }

    /// Add a new item to the backlog
    pub async fn add_to_backlog(&self, item_id: String) -> Result<()> {
        let mut stages = self.stages.write().await;

        let backlog = stages.get_mut(&Stage::Backlog)
            .ok_or_else(|| BackpressureError::CapacityExceeded("backlog not found".to_string()))?;

        backlog.items.push(WorkItem {
            id: item_id,
            stage: Stage::Backlog,
            token: None,
        });

        Ok(())
    }

    /// Get count of items in a stage
    pub async fn count(&self, stage: Stage) -> usize {
        let stages = self.stages.read().await;
        stages.get(&stage).map(|s| s.count()).unwrap_or(0)
    }

    /// Get WIP limit for a stage
    pub async fn wip_limit(&self, stage: Stage) -> usize {
        let stages = self.stages.read().await;
        stages.get(&stage).map(|s| s.wip_limit).unwrap_or(0)
    }

    /// Check if a stage has capacity
    pub async fn has_capacity(&self, stage: Stage) -> bool {
        let stages = self.stages.read().await;
        stages.get(&stage).map(|s| s.has_capacity()).unwrap_or(false)
    }

    /// Get all items in a stage
    pub async fn items_in_stage(&self, stage: Stage) -> Vec<WorkItem> {
        let stages = self.stages.read().await;
        stages.get(&stage)
            .map(|s| s.items.clone())
            .unwrap_or_default()
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[tokio::test]
    async fn test_stage_transitions() {
        assert_eq!(Stage::Backlog.next(), Some(Stage::Ready));
        assert_eq!(Stage::Ready.next(), Some(Stage::InProgress));
        assert_eq!(Stage::InProgress.next(), Some(Stage::Review));
        assert_eq!(Stage::Review.next(), Some(Stage::Done));
        assert_eq!(Stage::Done.next(), None);

        assert_eq!(Stage::Done.prev(), Some(Stage::Review));
        assert_eq!(Stage::Review.prev(), Some(Stage::InProgress));
        assert_eq!(Stage::InProgress.prev(), Some(Stage::Ready));
        assert_eq!(Stage::Ready.prev(), Some(Stage::Backlog));
        assert_eq!(Stage::Backlog.prev(), None);
    }

    #[tokio::test]
    async fn test_kanban_add_to_backlog() {
        let board = KanbanBoard::with_defaults();

        board.add_to_backlog("item1".to_string()).await.unwrap();
        board.add_to_backlog("item2".to_string()).await.unwrap();

        assert_eq!(board.count(Stage::Backlog).await, 2);
    }

    #[tokio::test]
    async fn test_kanban_pull_respects_wip() {
        let config = KanbanConfig {
            ready_limit: 2,
            in_progress_limit: 1,
            review_limit: 1,
        };

        let board = KanbanBoard::new(config);

        // Add items to backlog
        board.add_to_backlog("item1".to_string()).await.unwrap();
        board.add_to_backlog("item2".to_string()).await.unwrap();
        board.add_to_backlog("item3".to_string()).await.unwrap();

        // Pull to Ready (should succeed - limit is 2)
        let _t1 = board.pull("item1").await.unwrap();
        let _t2 = board.pull("item2").await.unwrap();

        assert_eq!(board.count(Stage::Ready).await, 2);

        // Third pull should fail - at capacity
        let result = board.pull("item3").await;
        assert!(result.is_err());
    }

    #[tokio::test]
    async fn test_kanban_pull_flow() {
        let board = KanbanBoard::with_defaults();

        board.add_to_backlog("item1".to_string()).await.unwrap();

        // Pull through entire flow
        let _t1 = board.pull("item1").await.unwrap();
        assert_eq!(board.count(Stage::Ready).await, 1);

        let _t2 = board.pull("item1").await.unwrap();
        assert_eq!(board.count(Stage::InProgress).await, 1);
        assert_eq!(board.count(Stage::Ready).await, 0);

        let _t3 = board.pull("item1").await.unwrap();
        assert_eq!(board.count(Stage::Review).await, 1);
        assert_eq!(board.count(Stage::InProgress).await, 0);

        let _t4 = board.pull("item1").await.unwrap();
        assert_eq!(board.count(Stage::Done).await, 1);
        assert_eq!(board.count(Stage::Review).await, 0);
    }

    #[tokio::test]
    async fn test_kanban_has_capacity() {
        let config = KanbanConfig {
            ready_limit: 1,
            in_progress_limit: 1,
            review_limit: 1,
        };

        let board = KanbanBoard::new(config);

        assert!(board.has_capacity(Stage::Ready).await);

        board.add_to_backlog("item1".to_string()).await.unwrap();
        let _t = board.pull("item1").await.unwrap();

        assert!(!board.has_capacity(Stage::Ready).await);
    }

    #[tokio::test]
    async fn test_pull_only_no_push() {
        let board = KanbanBoard::with_defaults();

        board.add_to_backlog("item1".to_string()).await.unwrap();

        // Only way to move items is via pull
        let _token = board.pull("item1").await.unwrap();

        // No push method exists - enforces pull-only
        assert_eq!(board.count(Stage::Ready).await, 1);
        assert_eq!(board.count(Stage::Backlog).await, 0);
    }

    #[tokio::test]
    async fn test_items_in_stage() {
        let board = KanbanBoard::with_defaults();

        board.add_to_backlog("item1".to_string()).await.unwrap();
        board.add_to_backlog("item2".to_string()).await.unwrap();

        let items = board.items_in_stage(Stage::Backlog).await;
        assert_eq!(items.len(), 2);
        assert!(items.iter().any(|i| i.id == "item1"));
        assert!(items.iter().any(|i| i.id == "item2"));
    }
}
