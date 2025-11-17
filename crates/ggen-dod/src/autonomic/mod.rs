//! Autonomic subsystem: Self-improving ggen without human intervention
//!
//! The autonomic subsystem implements the complete MAPE-K loop,
//! enabling ggen to continuously improve itself through observation,
//! analysis, planning, and controlled execution.

pub mod mape_k;

pub use mape_k::{
    AnalysisPhase, ExecutionPhase, ExecutionResult, Finding, KnowledgePhase, MAPEKLoop,
    ObservationPhase, PlanningPhase, SchemaProposal,
};
