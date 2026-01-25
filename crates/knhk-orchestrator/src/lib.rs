#![warn(missing_docs)]
#![deny(unsafe_code)]

//! # knhk-orchestrator: ETL-KGC-4D-Workflow Engine Integration Bridge
//!
//! Holographic orchestration of three Rust systems:
//! 1. **knhk-etl**: Extract-Transform-Load pipeline (RDF ingestion & processing)
//! 2. **hknk-kgc-4d**: Temporal knowledge graph layer (4D causality & event sourcing)
//! 3. **knhk-workflow-engine**: BPMN 2.0 orchestration (process execution & state management)
//!
//! The orchestrator implements the Chatman Equation: **A = μ(O)** where:
//! - **O** = ETL output (RDF triples with receipts)
//! - **μ** = Orchestrator's five-stage transformation pipeline
//! - **A** = Workflow trigger events ready for execution
//!
//! ## Architecture
//!
//! ```text
//! ETL Pipeline (knhk-etl)
//!    ↓ (TripleEvent with receipt)
//! Orchestrator Stage 1: ETL Event Intake
//!    ↓
//! Orchestrator Stage 2: KGC-4D Context Injection (temporal coordinates)
//!    ↓
//! Orchestrator Stage 3: Process Variable Aggregation
//!    ↓
//! Orchestrator Stage 4: Workflow Trigger Generation
//!    ↓
//! Orchestrator Stage 5: Workflow Execution & Receipt
//!    ↓ (WorkflowTriggerEvent)
//! Workflow Engine (knhk-workflow-engine)
//! ```
//!
//! ## Optimization Strategies
//!
//! 1. **Zero-Copy Data Flow**: Uses `Arc<T>` for shared state, avoids cloning large graphs
//! 2. **Batch Processing**: Leverages ETL's SoA arrays (64-byte alignment) for SIMD vectorization
//! 3. **Lazy Context Injection**: Only computes BLAKE3 snapshots on workflow entry
//! 4. **Hot-Path Delegation**: Uses C FFI for variable map merging
//! 5. **Event Deduplication**: Idempotent processing via transaction ID tracking
//!
//! ## Constitutional Rules
//!
//! - **Async Boundaries**: All inter-system calls are non-blocking (Tokio channels)
//! - **Error Isolation**: Failures don't cascade; each layer logs and continues
//! - **Deterministic Ordering**: Events ordered by (Git hash, nanotime, transactionId)
//! - **Receipt Generation**: Each layer proves its transformation
//! - **Mandatory Traceability**: OpenTelemetry span correlation across all systems
//!
//! ## Andon Signals
//!
//! - 🔴 **RED**: Event queue overflow (>10k pending), workflow failure rate >5%
//! - 🟡 **YELLOW**: KGC snapshot latency >100ms, span correlation loss >1%
//! - 🟢 **GREEN**: All SLOs met, steady state operation

pub mod bus;
pub mod events;
pub mod kgc;
pub mod orchestrator;
pub mod tracing;

pub use bus::EventBus;
pub use events::{
    EtlTripleEvent, ProcessInstanceEvent, ProcessInstanceState, WorkflowTriggerEvent,
};
pub use kgc::TemporalContext;
pub use orchestrator::{Orchestrator, OrchestratorConfig};

/// Result type for orchestrator operations
pub type Result<T> = std::result::Result<T, OrchestratorError>;

/// Orchestrator error types
#[derive(Debug, thiserror::Error)]
pub enum OrchestratorError {
    /// Event queue error
    #[error("Event queue error: {0}")]
    QueueError(String),

    /// KGC context injection error
    #[error("KGC context injection failed: {0}")]
    ContextError(String),

    /// Variable aggregation error
    #[error("Variable aggregation failed: {0}")]
    AggregationError(String),

    /// Workflow trigger generation error
    #[error("Workflow trigger generation failed: {0}")]
    TriggerGenerationError(String),

    /// Serialization error
    #[error("Serialization error: {0}")]
    SerializationError(#[from] serde_json::Error),

    /// Internal error
    #[error("Internal error: {0}")]
    Internal(String),
}
