//! YAWL-A2A bridge for workflow execution
//!
//! This module provides the bridge between YAWL (Yet Another Workflow Language)
//! workflows and A2A (Agent-to-Agent) task execution. It handles state mapping,
//! task conversion, and event publishing between the two systems.

pub mod state_mapper;
pub mod task_mapper;
pub mod event_publisher;

pub use state_mapper::YawlStateMapper;
pub use task_mapper::{TaskMapper, YawlTask, YawlTaskType, YawlSplitType, YawlJoinType};
pub use event_publisher::{YawlEventPublisher, YawlEventType};
