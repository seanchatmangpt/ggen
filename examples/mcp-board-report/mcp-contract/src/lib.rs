//! MCP+ Contract Execution Engine
//!
//! Implements sealed operating contracts with:
//! - Envelope enforcement (bounded behavior)
//! - Cryptographic receipts for every operation
//! - Deterministic refusals for violations
//! - Kill switch integration

pub mod engine;
pub mod executor;
pub mod state;

pub use engine::ContractEngine;
pub use executor::{ContractExecutor, ExecutionResult};
pub use state::ContractState;
