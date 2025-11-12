//! Checkpoint module - Fault tolerance

use serde::{Deserialize, Serialize};

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct Checkpoint {
    pub batch_id: String,
    pub position: usize,
    pub timestamp: chrono::DateTime<chrono::Utc>,
}

pub struct CheckpointManager {
    checkpoints: Vec<Checkpoint>,
}

impl CheckpointManager {
    pub fn new() -> Self {
        Self {
            checkpoints: Vec::new(),
        }
    }

    pub fn save(&mut self, checkpoint: Checkpoint) {
        self.checkpoints.push(checkpoint);
    }

    pub fn load_latest(&self) -> Option<&Checkpoint> {
        self.checkpoints.last()
    }
}

impl Default for CheckpointManager {
    fn default() -> Self {
        Self::new()
    }
}
