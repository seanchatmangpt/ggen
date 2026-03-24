//! YAWL-A2A bridge for workflow execution
//!
//! This module provides the bridge between YAWL (Yet Another Workflow Language)
//! workflows and A2A (Agent-to-Agent) task execution. It handles state mapping,
//! task conversion, and event publishing between the two systems.

pub mod event_publisher;
pub mod state_mapper;
pub mod task_mapper;

pub use event_publisher::{YawlEventPublisher, YawlEventType};
pub use state_mapper::YawlStateMapper;
pub use task_mapper::{TaskMapper, YawlJoinType, YawlSplitType, YawlTask, YawlTaskType};
