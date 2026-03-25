/// E2E Complete System: Joe Armstrong Fault Tolerance Principles
///
/// A comprehensive example demonstrating all five fault tolerance principles:
/// 1. Autonomous Agents with Isolation
/// 2. Distributed Consensus (PBFT)
/// 3. Tool Use Integration (MCP)
/// 4. Crash Recovery (Supervisor Trees)
/// 5. Cryptographic Accountability (Signed Receipts)
pub mod agents;
pub mod consensus;
pub mod orchestrator;
pub mod plans;
pub mod receipts;
pub mod supervisor;
pub mod tools;

pub use orchestrator::{LifeDomain, OSIRISOrchestrator, OSIRISState};
