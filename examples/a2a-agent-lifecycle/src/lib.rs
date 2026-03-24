//! Agent Lifecycle State Machine Example
//!
//! Demonstrates:
//! - Agent state machine with guards and transitions
//! - Task management with priorities and dependencies
//! - Message routing with FIFO guarantees
//! - Agent bridging as MCP tools
//! - Fault recovery mechanisms

pub mod agent;
pub mod bridging;
pub mod messaging;
pub mod task;

pub use agent::{Agent, AgentState};
pub use messaging::{Message, MessageRouter, MessageType};
pub use task::{Task, TaskManager, TaskPriority};
pub use bridging::AgentBridge;

pub const VERSION: &str = "0.1.0";

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_version() {
        assert_eq!(VERSION, "0.1.0");
    }
}
