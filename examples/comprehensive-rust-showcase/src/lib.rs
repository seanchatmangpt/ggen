pub mod agents;
pub mod fault_tolerance;
pub mod consensus;
pub mod coordination;
pub mod mcp_tools;
pub mod error;

pub use agents::{Agent, AgentState, AgentMessage};
pub use fault_tolerance::{CircuitBreaker, CircuitBreakerState, SupervisorTree};
pub use consensus::{Consensus, ConsensusResult};
pub use coordination::{Coordinator, CoordinatorState};
pub use mcp_tools::{ToolRegistry, ToolCall, ToolResult};
pub use error::{AgentError, Result};

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_module_exports() {
        // Verify all public modules are accessible
        let _ = std::any::type_name::<Agent>();
        let _ = std::any::type_name::<CircuitBreaker>();
        let _ = std::any::type_name::<Consensus>();
        let _ = std::any::type_name::<Coordinator>();
        let _ = std::any::type_name::<ToolRegistry>();
    }
}
