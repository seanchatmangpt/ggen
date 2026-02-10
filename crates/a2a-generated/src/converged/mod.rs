//! Converged (Unified) Interfaces
//!
//! This module exports the unified interfaces that eliminate redundancy
//! across the A2A protocol implementation.

pub mod agent;
pub mod message;

// Re-export agent types
pub use agent::{
    AgentCapabilities, AgentCommunication, AgentError, AgentExecution, AgentHealth, AgentIdentity,
    AgentLifecycle, AgentMetrics, AgentProtocol, AgentSecurity, AgentState, Capability, ComparisonOperator,
    DataFormat, ExecutionMode, ExecutionStrategy, HealthStatus, QoSLevel, ResourceConstraints,
    UnifiedAgent, UnifiedAgentBuilder, ValidationAction, ValidationRule, ValidationSeverity,
    // Security types from agent module
    AuditConfig, AuditEvent, AuthenticationConfig, AuthenticationMethod, AuthenticationProvider,
    AuthorizationConfig, AuthorizationModel, AuthorizationRole, ComplianceConfig,
    ComplianceControl, ComplianceFramework, ComplianceRequirement, ComplianceStandard,
    DestinationType, EncryptionAlgorithm, EncryptionConfig, EncryptionKey, EncryptionMode,
    Permission, PermissionScope, PermissionType, PolicyEffect,
    PolicyPriority, PolicyType, ProviderType, RequirementType, RetentionPolicy,
    SecurityPolicies, SecurityPolicy, SecurityRule,
};

// Re-export message types
pub use message::{
    ConvergedMessage, ConvergedMessageBuilder, ConvergedMessageType, ConvergedPayload,
    ConversationContext, DomainContext, EncryptionRequirements, HandlerAction, HandlerPriority,
    HandlerStatus, LatencyRequirements, MessageEnvelope, MessageHints, MessageIntegrity,
    MessageLifecycle, MessagePriority, MessageRouter, MessageRouting, MessageState,
    MessageStateTransition, MessageTimeout, ProcessingHints, QoSRequirements,
    ReliabilityLevel, ResourceRequirements, SecurityClassification, SecurityHints, TaskContext,
    TaskStatus, TemporalContext, ThroughputRequirements, TimeoutType, UnifiedContent,
    UnifiedContext, UnifiedFileContent, Route, RouteAction, RoutingCondition, RoutingRule,
};

