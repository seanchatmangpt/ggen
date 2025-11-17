//! Autonomic subsystem: Self-improving ggen without human intervention
//!
//! The autonomic subsystem implements the complete MAPE-K loop,
//! enabling ggen to continuously improve itself through observation,
//! analysis, planning, and controlled execution.

pub mod mape_k;

pub use mape_k::{
    MAPEKLoop, ObservationPhase, AnalysisPhase, PlanningPhase, ExecutionPhase, KnowledgePhase,
    Finding, SchemaProposal, ExecutionResult,
};
