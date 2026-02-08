//! Converged (Unified) Interfaces
//!
//! This module exports the unified interfaces that eliminate redundancy
//! across the A2A protocol implementation.

pub mod agent;
pub mod message;

// Re-export agent types
pub use agent::{
    UnifiedAgent, UnifiedAgentBuilder, AgentIdentity, AgentCapabilities,
    AgentLifecycle, AgentCommunication, AgentExecution, AgentSecurity,
    Capability, AgentProtocol, DataFormat, QoSLevel, ResourceConstraints,
    AgentState, AgentHealth, HealthStatus, AgentMetrics,
    ExecutionMode, ExecutionStrategy, AgentError,
};

// Re-export message types
pub use message::{
    ConvergedMessage, ConvergedMessageType, ConvergedPayload, UnifiedContent,
    MessageEnvelope, MessageRouting, MessageLifecycle, MessageState,
    UnifiedContext, TaskContext, ConversationContext, DomainContext, TemporalContext,
    MessageHints, ProcessingHints, SecurityHints, ResourceRequirements,
    SecurityClassification, EncryptionRequirements,
    MessageIntegrity, QoSRequirements, ReliabilityLevel,
    LatencyRequirements, ThroughputRequirements,
    MessageTimeout, TimeoutType, MessagePriority, TaskStatus,
    MessageStateTransition, ConvergedMessageBuilder,
    UnifiedFileContent,
};
