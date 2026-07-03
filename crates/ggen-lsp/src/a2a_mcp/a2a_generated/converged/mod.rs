//! Converged (Unified) Interfaces
//!
//! This module exports the unified interfaces that eliminate redundancy
//! across the A2A protocol implementation.

pub mod agent;
pub mod message;

// Re-export agent types
pub use agent::{
    AgentCapabilities,
    AgentCommunication,
    AgentError,
    AgentExecution,
    AgentHealth,
    AgentIdentity,
    AgentLifecycle,
    AgentMetrics,
    AgentProtocol,
    AgentSecurity,
    AgentState,
    // Security types from agent module
    AuditConfig,
    AuditEvent,
    AuthenticationConfig,
    AuthenticationMethod,
    AuthenticationProvider,
    AuthorizationConfig,
    AuthorizationModel,
    AuthorizationRole,
    Capability,
    ComparisonOperator,
    ComplianceConfig,
    ComplianceControl,
    ComplianceFramework,
    ComplianceRequirement,
    ComplianceStandard,
    DataFormat,
    DestinationType,
    EncryptionAlgorithm,
    EncryptionConfig,
    EncryptionKey,
    EncryptionMode,
    ExecutionMode,
    ExecutionStrategy,
    HealthStatus,
    Permission,
    PermissionScope,
    PermissionType,
    PolicyEffect,
    PolicyPriority,
    PolicyType,
    ProviderType,
    QoSLevel,
    RequirementType,
    ResourceConstraints,
    RetentionPolicy,
    SecurityPolicies,
    SecurityPolicy,
    SecurityRule,
    UnifiedAgent,
    UnifiedAgentBuilder,
    ValidationAction,
    ValidationRule,
    ValidationSeverity,
};

// Re-export message types
pub use message::{
    ConvergedMessage, ConvergedMessageBuilder, ConvergedMessageType, ConvergedPayload,
    ConversationContext, DomainContext, EncryptionRequirements, HandlerAction, HandlerPriority,
    HandlerStatus, LatencyRequirements, MessageEnvelope, MessageHints, MessageIntegrity,
    MessageLifecycle, MessagePriority, MessageRouter, MessageRouting, MessageState,
    MessageStateTransition, MessageTimeout, ProcessingHints, QoSRequirements, ReliabilityLevel,
    ResourceRequirements, Route, RouteAction, RoutingCondition, RoutingRule,
    SecurityClassification, SecurityHints, TaskContext, TaskStatus, TemporalContext,
    ThroughputRequirements, TimeoutType, UnifiedContent, UnifiedContext, UnifiedFileContent,
};
