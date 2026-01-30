//! MCP+ Contract Execution Engine
//!
//! Implements sealed operating contracts with:
//! - Envelope enforcement (bounded behavior)
//! - Cryptographic receipts for every operation
//! - Deterministic refusals for violations
//! - Kill switch integration
//! - Dead man's switch for automatic kill on heartbeat timeout
//! - Hot envelope reload (runtime constraint updates)
//! - Contract composition into type-safe pipelines

pub mod compose;
pub mod deadman;
pub mod engine;
pub mod executor;
pub mod hotreload;
pub mod state;

pub use compose::{Pipeline, PipelineBuilder, PipelineNode, PipelineResult, StageResult, ValidationResult};
pub use deadman::{DeadManConfig, DeadManSwitch, KillEvent, KillScope, SwitchStatus};
pub use engine::ContractEngine;
pub use executor::{ContractExecutor, ExecutionResult};
pub use hotreload::{ChangeType, EnvelopeChange, EnvelopeModifier, HotEnvelope, VersionedEnvelope};
pub use state::ContractState;
