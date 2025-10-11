//! Ultrathink - 80/20 Autonomous Intelligence System
//!
//! This module implements the ultrathink system following the 80/20 rule:
//! - 80% of value comes from 20% of core functionality
//! - Focus on essential WIP integration and autonomous task processing
//! - Simplified architecture for maximum impact

pub mod core;

use std::sync::Arc;

use crate::error::GgenAiError;
use crate::error::Result;
use uuid::Uuid;

/// Re-export core ultrathink types
pub use core::*;

/// Initialize the ultrathink system
pub async fn initialize_ultrathink() -> Result<()> {
    core::initialize_ultrathink_core().await
}

/// Create a new ultrathink task
pub fn create_task(
    task_type: core::TaskType, description: String, priority: core::TaskPriority,
) -> core::UltrathinkTask {
    core::create_ultrathink_task(task_type, description, priority)
}

/// Main ultrathink system instance
pub struct UltrathinkSystem {
    core: Arc<UltrathinkCore>,
}

impl UltrathinkSystem {
    /// Create a new ultrathink system
    pub async fn new() -> Result<Self> {
        let config = UltrathinkConfig::default();
        let core = Arc::new(UltrathinkCore::new(config).await?);

        Ok(Self { core })
    }

    /// Submit a task for autonomous processing
    pub async fn submit_task(&self, task: UltrathinkTask) -> Result<Uuid> {
        self.core.submit_task(task).await
    }

    /// Synchronize with WIP systems
    pub async fn sync_with_wip(&self) -> Result<()> {
        self.core.sync_with_wip().await
    }

    /// Get system status
    pub async fn get_status(&self) -> Result<CoreMetrics> {
        self.core.get_status().await
    }

    /// Process WIP entries
    pub async fn process_wip_entries(&self) -> Result<Vec<WipOperation>> {
        self.core.process_wip_entries().await
    }
}

/// Global ultrathink system instance
static mut ULTRATHINK_SYSTEM: Option<UltrathinkSystem> = None;

/// Initialize the global ultrathink system
pub async fn init_ultrathink_system() -> Result<()> {
    unsafe {
        if ULTRATHINK_SYSTEM.is_none() {
            ULTRATHINK_SYSTEM = Some(UltrathinkSystem::new().await?);
        }
    }
    Ok(())
}

/// Get the global ultrathink system instance
pub fn get_ultrathink_system() -> Option<&'static UltrathinkSystem> {
    unsafe { ULTRATHINK_SYSTEM.as_ref() }
}

/// Submit a task to the ultrathink system
pub async fn submit_ultrathink_task(
    task_type: core::TaskType, description: String, priority: core::TaskPriority,
) -> Result<Uuid> {
    let system = get_ultrathink_system()
        .ok_or_else(|| GgenAiError::configuration("Ultrathink system not initialized"))?;

    let task = create_task(task_type, description, priority);
    system.submit_task(task).await
}

/// Synchronize with WIP systems
pub async fn sync_ultrathink_wip() -> Result<()> {
    let system = get_ultrathink_system()
        .ok_or_else(|| GgenAiError::configuration("Ultrathink system not initialized"))?;

    system.sync_with_wip().await
}

/// Get ultrathink system status
pub async fn get_ultrathink_status() -> Result<CoreMetrics> {
    let system = get_ultrathink_system()
        .ok_or_else(|| GgenAiError::configuration("Ultrathink system not initialized"))?;

    system.get_status().await
}

/// Process WIP entries
pub async fn process_ultrathink_wip_entries() -> Result<Vec<WipOperation>> {
    let system = get_ultrathink_system()
        .ok_or_else(|| GgenAiError::configuration("Ultrathink system not initialized"))?;

    system.process_wip_entries().await
}
