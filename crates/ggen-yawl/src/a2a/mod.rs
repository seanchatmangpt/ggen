//! YAWL to A2A task conversion and workflow execution.
//!
//! This module provides integration between YAWL workflow generation and A2A
//! (Agent-to-Agent) task execution. It converts YAWL workflow specifications
//! into executable A2A tasks with proper state management and dependency handling.
//!
//! # Architecture
//!
//! ```text
//! YAWL Workflow (RDF/XML)
//!         ↓
//! TaskConverter (YAWL → A2A Tasks)
//!         ↓
//! WorkflowExecutor (orchestration)
//!         ↓
//! A2A TaskManager (execution)
//!         ↓
//! State Synchronizer (YAWL ↔ A2A states)
//! ```

pub mod converter;
pub mod coordinator;
pub mod error;
pub mod gateway;
pub mod state;
pub mod workflow;

// Re-export commonly used types
pub use converter::{TaskConverter, YawlToA2AConfig};
pub use coordinator::{WorkflowCoordinator, WorkflowExecutor};
pub use error::{A2AIntegrationError, IntegrationResult};
pub use gateway::{GatewayExecutor, GatewayType, SplitJoinBehavior};
pub use state::{StateMapping, StateSynchronizer, TaskStateSync, WorkflowState};
pub use workflow::{A2AWorkflow, WorkflowExecutionContext, WorkflowStatus};
