//! Agent Lifecycle State Machine Example
//!
//! Demonstrates:
//! - Agent state machine with guards and transitions
//! - Task management with priorities and dependencies
//! - Message routing with FIFO guarantees
//! - Agent bridging as MCP tools
//! - Fault recovery mechanisms

pub mod agent;
// pub mod bridging;  // Temporarily disabled - depends on ggen-a2a-mcp
pub mod messaging;
pub mod supervisor;
pub mod task;

pub use agent::{Agent, AgentState};
// pub use bridging::AgentBridge;  // Temporarily disabled - depends on ggen-a2a-mcp
pub use messaging::{Message, MessageRouter, MessageType};
pub use supervisor::{CrashReason, ManagedAgent, Supervisor, SupervisorState};
pub use task::{Task, TaskManager, TaskPriority};

pub const VERSION: &str = "0.1.0";

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_version() {
        assert_eq!(VERSION, "0.1.0");
    }
}
