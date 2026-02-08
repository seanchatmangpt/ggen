//! Converged (Unified) Interfaces
//!
//! This module exports the unified interfaces that eliminate redundancy
//! across the A2A protocol implementation.

pub mod agent;
pub mod message;

// Re-export agent types
pub use agent::{
    AgentCapabilities, AgentCommunication, AgentError, AgentExecution, AgentHealth, AgentIdentity,
    AgentLifecycle, AgentMetrics, AgentProtocol, AgentSecurity, AgentState, Capability, DataFormat,
    ExecutionMode, ExecutionStrategy, HealthStatus, QoSLevel, ResourceConstraints, UnifiedAgent,
    UnifiedAgentBuilder,
};

// Re-export message types
pub use message::{
    ConvergedMessage, ConvergedMessageBuilder, ConvergedMessageType, ConvergedPayload,
    ConversationContext, DomainContext, EncryptionRequirements, LatencyRequirements,
    MessageEnvelope, MessageHints, MessageIntegrity, MessageLifecycle, MessagePriority,
    MessageRouting, MessageState, MessageStateTransition, MessageTimeout, ProcessingHints,
    QoSRequirements, ReliabilityLevel, ResourceRequirements, SecurityClassification, SecurityHints,
    TaskContext, TaskStatus, TemporalContext, ThroughputRequirements, TimeoutType, UnifiedContent,
    UnifiedContext, UnifiedFileContent,
};
