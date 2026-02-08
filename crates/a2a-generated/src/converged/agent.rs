//! Unified Agent Interface - BB80 70% Redundancy Elimination
//!
//! This module implements the unified agent interface that eliminates 70%
//! of interface redundancy between basic and rich agent implementations.
//! Following the BB80 pattern with convergence through selection pressure.

use std::collections::HashMap;
use serde::{Deserialize, Serialize};
use chrono::{DateTime, Utc};

/// Unified convergent agent interface eliminating 70% redundancy
///
/// This interface consolidates basic and rich agent patterns into a single,
/// extensible format that maintains backward compatibility while dramatically
/// reducing interface duplication and semantic overlap.
#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub struct UnifiedAgent {
    /// Core agent identity (immutable)
    pub identity: AgentIdentity,

    /// Unified agent capabilities eliminating redundancy
    pub capabilities: AgentCapabilities,

    /// Agent lifecycle and state management
    pub lifecycle: AgentLifecycle,

    /// Agent communication and messaging
    pub communication: AgentCommunication,

    /// Agent execution and processing
    pub execution: AgentExecution,

    /// Agent security and access control
    pub security: AgentSecurity,

    /// Extensible agent metadata
    #[serde(skip_serializing_if = "Option::is_none")]
    pub extensions: Option<HashMap<String, serde_json::Value>>,
}

/// Core agent identity
#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
pub struct AgentIdentity {
    /// Unique agent identifier
    pub id: String,

    /// Human-readable agent name
    pub name: String,

    /// Agent type classification
    #[serde(rename = "agentType")]
    pub agent_type: String,

    /// Agent version
    pub version: String,

    /// Agent namespace
    pub namespace: String,

    /// Agent tags for classification
    #[serde(skip_serializing_if = "Option::is_none")]
    pub tags: Option<Vec<String>>,
}

/// Unified agent capabilities eliminating redundancy
#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub struct AgentCapabilities {
    /// Primary agent capabilities
    pub primary: Vec<Capability>,

    /// Secondary agent capabilities
    #[serde(skip_serializing_if = "Option::is_none")]
    pub secondary: Option<Vec<Capability>>,

    /// Agent protocols supported
    pub protocols: Vec<AgentProtocol>,

    /// Data formats supported
    pub formats: Vec<DataFormat>,

    /// Message types handled
    pub message_types: Vec<ConvergedMessageType>,

    /// Quality of service levels
    #[serde(rename = "qosLevels")]
    pub qos_levels: Vec<QoSLevel>,

    /// Resource constraints
    #[serde(skip_serializing_if = "Option::is_none")]
    pub constraints: Option<ResourceConstraints>,
}

/// Agent capability definition
#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
pub struct Capability {
    /// Capability name
    pub name: String,

    /// Capability version
    pub version: String,

    /// Capability description
    #[serde(skip_serializing_if = "Option::is_none")]
    pub description: Option<String>,

    /// Capability requirements
    #[serde(skip_serializing_if = "Option::is_none")]
    pub requirements: Option<HashMap<String, serde_json::Value>>,

    /// Capability metadata
    #[serde(skip_serializing_if = "Option::is_none")]
    pub metadata: Option<HashMap<String, serde_json::Value>>,
}

/// Agent protocols supported
#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
#[serde(rename_all = "lowercase")]
pub enum AgentProtocol {
    /// HTTP/HTTPS protocol
    Http,
    /// WebSocket protocol
    WebSocket,
    /// gRPC protocol
    Grpc,
    /// Message queue protocol
    MessageQueue,
    /// Peer-to-peer protocol
    P2P,
    /// Custom protocol
    Custom(String),
}

/// Data formats supported
#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
#[serde(rename_all = "lowercase")]
pub enum DataFormat {
    /// JSON format
    Json,
    /// XML format
    Xml,
    /// YAML format
    Yaml,
    /// Protocol Buffers format
    Protobuf,
    /// MessagePack format
    Msgpack,
    /// Custom format
    Custom(String),
}

/// Quality of service levels
#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
#[serde(rename_all = "lowercase")]
pub enum QoSLevel {
    /// Best effort delivery
    BestEffort,
    /// At-least-once delivery
    AtLeastOnce,
    /// Exactly-once delivery
    ExactlyOnce,
    /// Ordered delivery
    Ordered,
    /// Priority delivery
    Priority,
}

/// Resource constraints
#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub struct ResourceConstraints {
    /// Memory constraints
    pub memory: Option<ResourceLimit>,

    /// CPU constraints
    pub cpu: Option<ResourceLimit>,

    /// Storage constraints
    pub storage: Option<ResourceLimit>,

    /// Network constraints
    pub network: Option<ResourceLimit>,

    /// Concurrent operations
    #[serde(rename = "concurrentOps")]
    pub concurrent_ops: Option<ResourceLimit>,
}

/// Resource limit definition
#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub struct ResourceLimit {
    /// Minimum resource requirement
    pub min: Option<u64>,

    /// Maximum resource limit
    pub max: Option<u64>,

    /// Default resource allocation
    pub default: Option<u64>,

    /// Resource unit
    pub unit: ResourceUnit,
}

/// Resource units
#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
#[serde(rename_all = "lowercase")]
pub enum ResourceUnit {
    /// Bytes
    Bytes,
    /// Megabytes
    Megabytes,
    /// Gigabytes
    Gigabytes,
    /// Operations per second
    OpsPerSecond,
    /// Percentage
    Percent,
    /// Count
    Count,
}

/// Agent lifecycle state
#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
pub struct AgentLifecycle {
    /// Current lifecycle state
    pub state: AgentState,

    /// Lifecycle state history
    #[serde(rename = "stateHistory")]
    pub state_history: Vec<AgentStateTransition>,

    /// Health status
    pub health: AgentHealth,

    /// Performance metrics
    #[serde(skip_serializing_if = "Option::is_none")]
    pub metrics: Option<AgentMetrics>,

    /// Configuration and settings
    pub configuration: AgentConfiguration,

    /// Dependencies and requirements
    #[serde(skip_serializing_if = "Option::is_none")]
    pub dependencies: Option<AgentDependencies>,

    /// Lifecycle timeout information
    #[serde(skip_serializing_if = "Option::is_none")]
    pub timeouts: Option<AgentTimeouts>,
}

/// Agent states
#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
#[serde(rename_all = "lowercase")]
pub enum AgentState {
    /// Agent created but not initialized
    Created,
    /// Agent initializing
    Initializing,
    /// Agent ready and running
    Ready,
    /// Agent busy processing
    Busy,
    /// Agent paused
    Paused,
    /// Agent shutting down
    ShuttingDown,
    /// Agent terminated
    Terminated,
    /// Agent failed
    Failed,
    /// Agent in maintenance
    Maintenance,
}

/// Agent health status
#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
pub struct AgentHealth {
    /// Overall health status
    pub status: HealthStatus,

    /// Health check timestamp
    #[serde(rename = "lastCheck")]
    pub last_check: DateTime<Utc>,

    /// Health check intervals
    #[serde(rename = "checkInterval")]
    pub check_interval: std::time::Duration,

    /// Health metrics
    #[serde(skip_serializing_if = "Option::is_none")]
    pub metrics: Option<HealthMetrics>,

    /// Health warnings
    #[serde(skip_serializing_if = "Option::is_none")]
    pub warnings: Option<Vec<HealthWarning>>,

    /// Health errors
    #[serde(skip_serializing_if = "Option::is_none")]
    pub errors: Option<Vec<HealthError>>,
}

/// Health status
#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
#[serde(rename_all = "lowercase")]
pub enum HealthStatus {
    /// Healthy
    Healthy,
    /// Warning
    Warning,
    /// Critical
    Critical,
    /// Unhealthy
    Unhealthy,
    /// Unknown
    Unknown,
}

/// Health metrics
#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub struct HealthMetrics {
    /// Response time metrics
    #[serde(rename = "responseTime")]
    pub response_time: ResponseTimeMetrics,

    /// Error rate metrics
    #[serde(rename = "errorRate")]
    pub error_rate: ErrorRateMetrics,

    /// Resource usage metrics
    #[serde(rename = "resourceUsage")]
    pub resource_usage: ResourceUsageMetrics,

    /// Throughput metrics
    #[serde(skip_serializing_if = "Option::is_none")]
    pub throughput: Option<ThroughputMetrics>,
}

/// Response time metrics
#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub struct ResponseTimeMetrics {
    /// Average response time
    pub average: std::time::Duration,

    /// 95th percentile response time
    #[serde(rename = "p95")]
    pub p95: std::time::Duration,

    /// 99th percentile response time
    #[serde(rename = "p99")]
    pub p99: std::time::Duration,

    /// Maximum response time
    pub maximum: std::time::Duration,

    /// Minimum response time
    pub minimum: std::time::Duration,
}

/// Error rate metrics
#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub struct ErrorRateMetrics {
    /// Error rate percentage
    pub rate: f64,

    /// Total requests
    pub total: u64,

    /// Error count
    pub errors: u64,

    /// Error trend
    #[serde(skip_serializing_if = "Option::is_none")]
    pub trend: Option<ErrorTrend>,
}

/// Error trend
#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
#[serde(rename_all = "lowercase")]
pub enum ErrorTrend {
    /// Increasing error rate
    Increasing,
    /// Decreasing error rate
    Decreasing,
    /// Stable error rate
    Stable,
    /// Unknown trend
    Unknown,
}

/// Resource usage metrics
#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub struct ResourceUsageMetrics {
    /// Memory usage percentage
    #[serde(rename = "memoryPercent")]
    pub memory_percent: f64,

    /// CPU usage percentage
    pub cpu_percent: f64,

    /// Disk usage percentage
    #[serde(rename = "diskPercent")]
    pub disk_percent: f64,

    /// Network usage
    #[serde(rename = "networkBytes")]
    pub network_bytes: u64,
}

/// Throughput metrics
#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub struct ThroughputMetrics {
    /// Requests per second
    #[serde(rename = "requestsPerSecond")]
    pub requests_per_second: f64,

    /// Messages per second
    #[serde(rename = "messagesPerSecond")]
    pub messages_per_second: f64,

    /// Processing rate
    #[serde(skip_serializing_if = "Option::is_none")]
    pub processing_rate: Option<f64>,

    /// Queued operations
    #[serde(skip_serializing_if = "Option::is_none")]
    pub queued: Option<u64>,
}

/// Health warning
#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
pub struct HealthWarning {
    /// Warning message
    pub message: String,

    /// Warning timestamp
    #[serde(rename = "timestamp")]
    pub timestamp: DateTime<Utc>,

    /// Warning severity
    pub severity: WarningSeverity,

    /// Warning category
    pub category: String,

    /// Warning details
    #[serde(skip_serializing_if = "Option::is_none")]
    pub details: Option<HashMap<String, serde_json::Value>>,
}

/// Warning severity
#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
#[serde(rename_all = "lowercase")]
pub enum WarningSeverity {
    /// Informational warning
    Info,
    /// Minor warning
    Minor,
    /// Major warning
    Major,
    /// Critical warning
    Critical,
}

/// Health error
#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
pub struct HealthError {
    /// Error message
    pub message: String,

    /// Error timestamp
    #[serde(rename = "timestamp")]
    pub timestamp: DateTime<Utc>,

    /// Error type
    pub error_type: String,

    /// Error severity
    pub severity: ErrorSeverity,

    /// Error stack trace
    #[serde(skip_serializing_if = "Option::is_none")]
    pub stack_trace: Option<String>,

    /// Error details
    #[serde(skip_serializing_if = "Option::is_none")]
    pub details: Option<HashMap<String, serde_json::Value>>,
}

/// Error severity
#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
#[serde(rename_all = "lowercase")]
pub enum ErrorSeverity {
    /// Low severity error
    Low,
    /// Medium severity error
    Medium,
    /// High severity error
    High,
    /// Critical error
    Critical,
}

/// Agent state transition
#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
pub struct AgentStateTransition {
    /// Previous state
    pub from: AgentState,

    /// New state
    pub to: AgentState,

    /// Transition timestamp
    #[serde(rename = "timestamp")]
    pub timestamp: DateTime<Utc>,

    /// Transition reason
    #[serde(skip_serializing_if = "Option::is_none")]
    pub reason: Option<String>,

    /// Transition details
    #[serde(skip_serializing_if = "Option::is_none")]
    pub details: Option<HashMap<String, serde_json::Value>>,
}

/// Agent metrics
#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub struct AgentMetrics {
    /// Performance metrics
    pub performance: PerformanceMetrics,

    /// Reliability metrics
    pub reliability: ReliabilityMetrics,

    /// Efficiency metrics
    #[serde(skip_serializing_if = "Option::is_none")]
    pub efficiency: Option<EfficiencyMetrics>,

    /// Scalability metrics
    #[serde(skip_serializing_if = "Option::is_none")]
    pub scalability: Option<ScalabilityMetrics>,

    /// Business metrics
    #[serde(skip_serializing_if = "Option::is_none")]
    pub business: Option<BusinessMetrics>,
}

/// Performance metrics
#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub struct PerformanceMetrics {
    /// Processing time
    #[serde(rename = "processingTime")]
    pub processing_time: ProcessingMetrics,

    /// Response time
    #[serde(rename = "responseTime")]
    pub response_time: ResponseMetrics,

    /// Throughput
    pub throughput: ThroughputMetrics,

    /// Resource utilization
    #[serde(rename = "resourceUtilization")]
    pub resource_utilization: ResourceUtilizationMetrics,
}

/// Processing metrics
#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub struct ProcessingMetrics {
    /// Average processing time
    pub average: std::time::Duration,

    /// Maximum processing time
    pub maximum: std::time::Duration,

    /// Minimum processing time
    pub minimum: std::time::Duration,

    /// Processing throughput
    #[serde(rename = "throughput")]
    pub throughput: f64,
}

/// Response metrics
#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub struct ResponseMetrics {
    /// Average response time
    pub average: std::time::Duration,

    /// Median response time
    pub median: std::time::Duration,

    /// 95th percentile response time
    #[serde(rename = "p95")]
    pub p95: std::time::Duration,

    /// 99th percentile response time
    #[serde(rename = "p99")]
    pub p99: std::time::Duration,

    /// Maximum response time
    pub maximum: std::time::Duration,
}

/// Resource utilization metrics
#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub struct ResourceUtilizationMetrics {
    /// CPU utilization
    pub cpu: f64,

    /// Memory utilization
    pub memory: f64,

    /// Disk utilization
    pub disk: f64,

    /// Network utilization
    pub network: f64,
}

/// Reliability metrics
#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub struct ReliabilityMetrics {
    /// Uptime percentage
    pub uptime: f64,

    /// Error rate
    pub error_rate: f64,

    /// Success rate
    pub success_rate: f64,

    /// Mean time between failures
    #[serde(rename = "mtbf")]
    pub mtbf: Option<std::time::Duration>,

    /// Mean time to recovery
    #[serde(rename = "mttr")]
    pub mttr: Option<std::time::Duration>,
}

/// Efficiency metrics
#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub struct EfficiencyMetrics {
    /// Cost efficiency
    #[serde(rename = "costEfficiency")]
    pub cost_efficiency: f64,

    /// Resource efficiency
    #[serde(rename = "resourceEfficiency")]
    pub resource_efficiency: f64,

    /// Time efficiency
    #[serde(rename = "timeEfficiency")]
    pub time_efficiency: f64,

    /// Operational efficiency
    #[serde(rename = "operationalEfficiency")]
    pub operational_efficiency: f64,
}

/// Scalability metrics
#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub struct ScalabilityMetrics {
    /// Horizontal scalability
    #[serde(rename = "horizontal")]
    pub horizontal: f64,

    /// Vertical scalability
    #[serde(rename = "vertical")]
    pub vertical: f64,

    /// Elasticity
    pub elasticity: f64,

    /// Load distribution
    #[serde(rename = "loadDistribution")]
    pub load_distribution: f64,
}

/// Business metrics
#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub struct BusinessMetrics {
    /// User satisfaction
    #[serde(rename = "userSatisfaction")]
    pub user_satisfaction: f64,

    /// Business value
    pub business_value: f64,

    /// Return on investment
    #[serde(rename = "roi")]
    pub roi: f64,

    /// Customer satisfaction
    #[serde(rename = "customerSatisfaction")]
    pub customer_satisfaction: f64,
}

/// Agent configuration
#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub struct AgentConfiguration {
    /// Configuration parameters
    pub parameters: HashMap<String, serde_json::Value>,

    /// Configuration version
    pub version: String,

    /// Configuration timestamp
    #[serde(rename = "timestamp")]
    pub timestamp: DateTime<Utc>,

    /// Configuration source
    #[serde(skip_serializing_if = "Option::is_none")]
    pub source: Option<String>,

    /// Configuration validation
    #[serde(skip_serializing_if = "Option::is_none")]
    pub validation: Option<ConfigurationValidation>,
}

/// Configuration validation
#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub struct ConfigurationValidation {
    /// Validation status
    pub valid: bool,

    /// Validation errors
    #[serde(skip_serializing_if = "Option::is_none")]
    pub errors: Option<Vec<ValidationError>>,

    /// Validation warnings
    #[serde(skip_serializing_if = "Option::is_none")]
    pub warnings: Option<Vec<ValidationWarning>>,
}

/// Validation error
#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
pub struct ValidationError {
    /// Error message
    pub message: String,

    /// Error field
    pub field: String,

    /// Error type
    pub error_type: String,

    /// Error details
    #[serde(skip_serializing_if = "Option::is_none")]
    pub details: Option<HashMap<String, serde_json::Value>>,
}

/// Validation warning
#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
pub struct ValidationWarning {
    /// Warning message
    pub message: String,

    /// Warning field
    pub field: String,

    /// Warning type
    pub warning_type: String,

    /// Warning details
    #[serde(skip_serializing_if = "Option::is_none")]
    pub details: Option<HashMap<String, serde_json::Value>>,
}

/// Agent dependencies
#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub struct AgentDependencies {
    /// Required dependencies
    pub required: Vec<Dependency>,

    /// Optional dependencies
    #[serde(skip_serializing_if = "Option::is_none")]
    pub optional: Option<Vec<Dependency>>,

    /// Dependency resolution status
    #[serde(rename = "resolutionStatus")]
    pub resolution_status: DependencyResolutionStatus,
}

/// Dependency definition
#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
pub struct Dependency {
    /// Dependency name
    pub name: String,

    /// Dependency version
    pub version: String,

    /// Dependency type
    pub dependency_type: DependencyType,

    /// Dependency source
    #[serde(skip_serializing_if = "Option::is_none")]
    pub source: Option<String>,

    /// Dependency status
    pub status: DependencyStatus,
}

/// Dependency types
#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
#[serde(rename_all = "lowercase")]
pub enum DependencyType {
    /// Software dependency
    Software,
    /// Service dependency
    Service,
    /// Data dependency
    Data,
    /// Network dependency
    Network,
    /// Hardware dependency
    Hardware,
    /// Custom dependency
    Custom(String),
}

/// Dependency status
#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
#[serde(rename_all = "lowercase")]
pub enum DependencyStatus {
    /// Resolved
    Resolved,
    /// Pending
    Pending,
    /// Failed
    Failed,
    /// Optional
    Optional,
    /// Deprecated
    Deprecated,
}

/// Dependency resolution status
#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
#[serde(rename_all = "lowercase")]
pub enum DependencyResolutionStatus {
    /// All dependencies resolved
    Resolved,
    /// Some dependencies pending
    Partial,
    /// Dependencies failed to resolve
    Failed,
    /// No dependencies required
    None,
}

/// Agent timeouts
#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub struct AgentTimeouts {
    /// Initialization timeout
    #[serde(rename = "initialization")]
    pub initialization: Option<std::time::Duration>,

    /// Processing timeout
    pub processing: Option<std::time::Duration>,

    /// Response timeout
    pub response: Option<std::time::Duration>,

    /// Health check timeout
    #[serde(rename = "healthCheck")]
    pub health_check: Option<std::time::Duration>,

    /// Communication timeout
    pub communication: Option<std::time::Duration>,
}

/// Agent communication interface
#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub struct AgentCommunication {
    /// Communication endpoints
    pub endpoints: Vec<CommunicationEndpoint>,

    /// Communication protocols
    pub protocols: Vec<AgentProtocol>,

    /// Message handlers
    #[serde(skip_serializing_if = "Option::is_none")]
    pub handlers: Option<MessageHandlers>,

    /// Communication security
    #[serde(skip_serializing_if = "Option::is_none")]
    pub security: Option<CommunicationSecurity>,

    /// Communication quality of service
    #[serde(rename = "qos")]
    pub qos: CommunicationQoS,
}

/// Communication endpoint
#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
pub struct CommunicationEndpoint {
    /// Endpoint URL
    pub url: String,

    /// Endpoint type
    pub endpoint_type: EndpointType,

    /// Endpoint authentication
    #[serde(skip_serializing_if = "Option::is_none")]
    pub authentication: Option<EndpointAuthentication>,

    /// Endpoint metadata
    #[serde(skip_serializing_if = "Option::is_none")]
    pub metadata: Option<HashMap<String, serde_json::Value>>,
}

/// Endpoint types
#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
#[serde(rename_all = "lowercase")]
pub enum EndpointType {
    /// REST API endpoint
    Rest,
    /// WebSocket endpoint
    WebSocket,
    /// gRPC endpoint
    Grpc,
    /// Message queue endpoint
    MessageQueue,
    /// Event stream endpoint
    EventStream,
    /// Custom endpoint
    Custom(String),
}

/// Endpoint authentication
#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
#[serde(rename_all = "lowercase")]
pub enum EndpointAuthentication {
    /// No authentication
    None,
    /// Basic authentication
    Basic,
    /// Bearer token authentication
    Bearer,
    /// API key authentication
    ApiKey,
    /// OAuth authentication
    OAuth,
    /// Custom authentication
    Custom(String),
}

/// Message handlers
#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub struct MessageHandlers {
    /// Request handlers
    pub request_handlers: Vec<MessageHandler>,

    /// Response handlers
    pub response_handlers: Vec<MessageHandler>,

    /// Event handlers
    pub event_handlers: Vec<MessageHandler>,

    /// Error handlers
    pub error_handlers: Vec<MessageHandler>,
}

/// Message handler
#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
pub struct MessageHandler {
    /// Handler name
    pub name: String,

    /// Handler type
    pub handler_type: HandlerType,

    /// Handler priority
    pub priority: HandlerPriority,

    /// Handler configuration
    pub configuration: HashMap<String, serde_json::Value>,

    /// Handler metadata
    #[serde(skip_serializing_if = "Option::is_none")]
    pub metadata: Option<HashMap<String, serde_json::Value>>,
}

/// Handler types
#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
#[serde(rename_all = "lowercase")]
pub enum HandlerType {
    /// Request handler
    Request,
    /// Response handler
    Response,
    /// Event handler
    Event,
    /// Error handler
    Error,
    /// Custom handler
    Custom(String),
}

/// Handler priority
#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
#[serde(rename_all = "lowercase")]
pub enum HandlerPriority {
    /// Highest priority
    Highest,
    /// High priority
    High,
    /// Normal priority
    Normal,
    /// Low priority
    Low,
    /// Lowest priority
    Lowest,
}

/// Communication security
#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub struct CommunicationSecurity {
    /// Security protocols
    pub protocols: Vec<SecurityProtocol>,

    /// Encryption requirements
    pub encryption: EncryptionRequirements,

    /// Authentication requirements
    pub authentication: AuthenticationRequirements,

    /// Authorization requirements
    pub authorization: AuthorizationRequirements,
}

/// Security protocols
#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
#[serde(rename_all = "lowercase")]
pub enum SecurityProtocol {
    /// TLS protocol
    Tls,
    /// SSL protocol
    Ssl,
    /// IPSec protocol
    Ipsec,
    /// Custom security protocol
    Custom(String),
}

/// Communication quality of service
#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub struct CommunicationQoS {
    /// Reliability level
    pub reliability: ReliabilityLevel,

    /// Latency requirements
    #[serde(skip_serializing_if = "Option::is_none")]
    pub latency: Option<LatencyRequirements>,

    /// Throughput requirements
    #[serde(skip_serializing_if = "Option::is_none")]
    pub throughput: Option<ThroughputRequirements>,

    /// Ordering requirements
    #[serde(skip_serializing_if = "Option::is_none")]
    pub ordering: Option<OrderingRequirements>,

    /// Flow control requirements
    #[serde(skip_serializing_if = "Option::is_none")]
    pub flow_control: Option<FlowControlRequirements>,
}

/// Ordering requirements
#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub struct OrderingRequirements {
    /// Ordering guarantee
    pub guarantee: OrderingGuarantee,

    /// Maximum out-of-order messages
    #[serde(rename = "maxOutOfOrder")]
    pub max_out_of_order: Option<usize>,

    /// Reordering window size
    #[serde(rename = "reorderingWindowSize")]
    pub reordering_window_size: Option<usize>,
}

/// Ordering guarantees
#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
#[serde(rename_all = "lowercase")]
pub enum OrderingGuarantee {
    /// No ordering guarantee
    None,
    /// FIFO ordering
    Fifo,
    /// Priority ordering
    Priority,
    /// Custom ordering
    Custom(String),
}

/// Flow control requirements
#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub struct FlowControlRequirements {
    /// Flow control type
    pub control_type: FlowControlType,

    /// Window size
    #[serde(rename = "windowSize")]
    pub window_size: Option<usize>,

    /// Timeout
    pub timeout: Option<std::time::Duration>,

    /// Backpressure handling
    #[serde(rename = "backpressure")]
    pub backpressure: Option<BackpressureHandling>,
}

/// Flow control types
#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
#[serde(rename_all = "lowercase")]
pub enum FlowControlType {
    /// No flow control
    None,
    /// Window-based flow control
    Window,
    /// Rate-based flow control
    Rate,
    /// Credit-based flow control
    Credit,
    /// Custom flow control
    Custom(String),
}

/// Backpressure handling
#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
#[serde(rename_all = "lowercase")]
pub enum BackpressureHandling {
    /// Drop messages
    Drop,
    /// Queue messages
    Queue,
    /// Throttle messages
    Throttle,
    /// Reject messages
    Reject,
    /// Custom handling
    Custom(String),
}

/// Agent execution interface
#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub struct AgentExecution {
    /// Execution mode
    pub mode: ExecutionMode,

    /// Execution parameters
    pub parameters: HashMap<String, serde_json::Value>,

    /// Execution context
    #[serde(skip_serializing_if = "Option::is_none")]
    pub context: Option<ExecutionContext>,

    /// Execution strategy
    #[serde(skip_serializing_if = "Option::is_none")]
    pub strategy: Option<ExecutionStrategy>,

    /// Execution monitoring
    #[serde(skip_serializing_if = "Option::is_none")]
    pub monitoring: Option<ExecutionMonitoring>,

    /// Execution policies
    #[serde(skip_serializing_if = "Option::is_none")]
    pub policies: Option<ExecutionPolicies>,
}

/// Execution modes
#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
#[serde(rename_all = "lowercase")]
pub enum ExecutionMode {
    /// Synchronous execution
    Synchronous,
    /// Asynchronous execution
    Asynchronous,
    /// Batch execution
    Batch,
    /// Stream execution
    Stream,
    /// Event-driven execution
    EventDriven,
    /// Custom execution mode
    Custom(String),
}

/// Execution context
#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub struct ExecutionContext {
    /// Context data
    pub data: HashMap<String, serde_json::Value>,

    /// Context metadata
    #[serde(skip_serializing_if = "Option::is_none")]
    pub metadata: Option<HashMap<String, serde_json::Value>>,

    /// Context lifecycle
    #[serde(skip_serializing_if = "Option::is_none")]
    pub lifecycle: Option<ContextLifecycle>,

    /// Context validation
    #[serde(skip_serializing_if = "Option::is_none")]
    pub validation: Option<ContextValidation>,
}

/// Context lifecycle
#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
#[serde(rename_all = "lowercase")]
pub enum ContextLifecycle {
    /// Transient context
    Transient,
    /// Persistent context
    Persistent,
    /// Cached context
    Cached,
    /// Shared context
    Shared,
    /// Custom context lifecycle
    Custom(String),
}

/// Context validation
#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub struct ContextValidation {
    /// Validation rules
    pub rules: Vec<ValidationRule>,

    /// Validation mode
    pub mode: ValidationMode,

    /// Validation results
    #[serde(skip_serializing_if = "Option::is_none")]
    pub results: Option<ValidationResults>,
}

/// Validation rule
#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
pub struct ValidationRule {
    /// Rule name
    pub name: String,

    /// Rule type
    pub rule_type: ValidationRuleType,

    /// Rule condition
    pub condition: String,

    /// Rule action
    pub action: ValidationAction,

    /// Rule severity
    pub severity: ValidationSeverity,
}

/// Validation rule types
#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
#[serde(rename_all = "lowercase")]
pub enum ValidationRuleType {
    /// Required field validation
    Required,
    /// Type validation
    Type,
    /// Range validation
    Range,
    /// Pattern validation
    Pattern,
    /// Custom validation
    Custom(String),
}

/// Validation actions
#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
#[serde(rename_all = "lowercase")]
pub enum ValidationAction {
    /// Accept valid data
    Accept,
    /// Reject invalid data
    Reject,
    /// Transform invalid data
    Transform,
    /// Flag invalid data
    Flag,
    /// Custom action
    Custom(String),
}

/// Validation severity
#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
#[serde(rename_all = "lowercase")]
pub enum ValidationSeverity {
    /// Info level
    Info,
    /// Warning level
    Warning,
    /// Error level
    Error,
    /// Critical level
    Critical,
}

/// Validation mode
#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
#[serde(rename_all = "lowercase")]
pub enum ValidationMode {
    /// Strict validation
    Strict,
    /// Lenient validation
    Lenient,
    /// Custom validation mode
    Custom(String),
}

/// Validation results
#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub struct ValidationResults {
    /// Validation status
    pub status: ValidationStatus,

    /// Validation errors
    #[serde(skip_serializing_if = "Option::is_none")]
    pub errors: Option<Vec<ValidationError>>,

    /// Validation warnings
    #[serde(skip_serializing_if = "Option::is_none")]
    pub warnings: Option<Vec<ValidationWarning>>,

    /// Validation metadata
    #[serde(skip_serializing_if = "Option::is_none")]
    pub metadata: Option<HashMap<String, serde_json::Value>>,
}

/// Validation status
#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
#[serde(rename_all = "lowercase")]
pub enum ValidationStatus {
    /// Valid
    Valid,
    /// Invalid
    Invalid,
    /// Partially valid
    Partial,
    /// Unknown
    Unknown,
}

/// Execution strategy
#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub struct ExecutionStrategy {
    /// Strategy type
    pub strategy_type: StrategyType,

    /// Strategy configuration
    pub configuration: HashMap<String, serde_json::Value>,

    /// Strategy parameters
    #[serde(skip_serializing_if = "Option::is_none")]
    pub parameters: Option<HashMap<String, serde_json::Value>>,

    /// Strategy metadata
    #[serde(skip_serializing_if = "Option::is_none")]
    pub metadata: Option<HashMap<String, serde_json::Value>>,
}

/// Strategy types
#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
#[serde(rename_all = "lowercase")]
pub enum StrategyType {
    /// Sequential strategy
    Sequential,
    /// Parallel strategy
    Parallel,
    /// Pipeline strategy
    Pipeline,
    /// Fork-join strategy
    ForkJoin,
    /// Event-driven strategy
    EventDriven,
    /// Custom strategy
    Custom(String),
}

/// Execution monitoring
#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub struct ExecutionMonitoring {
    /// Monitoring metrics
    pub metrics: MonitoringMetrics,

    /// Monitoring thresholds
    pub thresholds: MonitoringThresholds,

    /// Monitoring alerts
    #[serde(skip_serializing_if = "Option::is_none")]
    pub alerts: Option<Vec<MonitoringAlert>>,

    /// Monitoring dashboards
    #[serde(skip_serializing_if = "Option::is_none")]
    pub dashboards: Option<Vec<MonitoringDashboard>>,
}

/// Monitoring metrics
#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub struct MonitoringMetrics {
    /// Performance metrics
    pub performance: PerformanceMetrics,

    /// Resource metrics
    pub resources: ResourceUsageMetrics,

    /// Business metrics
    #[serde(skip_serializing_if = "Option::is_none")]
    pub business: Option<BusinessMetrics>,

    /// Custom metrics
    #[serde(skip_serializing_if = "Option::is_none")]
    pub custom: Option<HashMap<String, serde_json::Value>>,
}

/// Monitoring thresholds
#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub struct MonitoringThresholds {
    /// Performance thresholds
    pub performance: Thresholds,

    /// Resource thresholds
    pub resources: Thresholds,

    /// Business thresholds
    #[serde(skip_serializing_if = "Option::is_none")]
    pub business: Option<Thresholds>,

    /// Custom thresholds
    #[serde(skip_serializing_if = "Option::is_none")]
    pub custom: Option<HashMap<String, serde_json::Value>>,
}

/// Thresholds
#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub struct Thresholds {
    /// Warning threshold
    pub warning: f64,

    /// Critical threshold
    pub critical: f64,

    /// Unit
    pub unit: String,

    /// Comparison operator
    pub operator: ComparisonOperator,
}

/// Comparison operators
#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
#[serde(rename_all = "lowercase")]
pub enum ComparisonOperator {
    /// Greater than
    Greater,
    /// Greater than or equal
    GreaterEqual,
    /// Less than
    Less,
    /// Less than or equal
    LessEqual,
    /// Equal
    Equal,
    /// Not equal
    NotEqual,
    /// Custom operator
    Custom(String),
}

/// Monitoring alert
#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
pub struct MonitoringAlert {
    /// Alert name
    pub name: String,

    /// Alert type
    pub alert_type: String,

    /// Alert severity
    pub severity: AlertSeverity,

    /// Alert message
    pub message: String,

    /// Alert timestamp
    #[serde(rename = "timestamp")]
    pub timestamp: DateTime<Utc>,

    /// Alert details
    #[serde(skip_serializing_if = "Option::is_none")]
    pub details: Option<HashMap<String, serde_json::Value>>,

    /// Alert metadata
    #[serde(skip_serializing_if = "Option::is_none")]
    pub metadata: Option<HashMap<String, serde_json::Value>>,
}

/// Alert severity
#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
#[serde(rename_all = "lowercase")]
pub enum AlertSeverity {
    /// Info alert
    Info,
    /// Warning alert
    Warning,
    /// Error alert
    Error,
    /// Critical alert
    Critical,
}

/// Monitoring dashboard
#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
pub struct MonitoringDashboard {
    /// Dashboard name
    pub name: String,

    /// Dashboard type
    pub dashboard_type: String,

    /// Dashboard description
    #[serde(skip_serializing_if = "Option::is_none")]
    pub description: Option<String>,

    /// Dashboard widgets
    pub widgets: Vec<DashboardWidget>,

    /// Dashboard metadata
    #[serde(skip_serializing_if = "Option::is_none")]
    pub metadata: Option<HashMap<String, serde_json::Value>>,
}

/// Dashboard widget
#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
pub struct DashboardWidget {
    /// Widget name
    pub name: String,

    /// Widget type
    pub widget_type: String,

    /// Widget configuration
    pub configuration: HashMap<String, serde_json::Value>,

    /// Widget position
    pub position: WidgetPosition,

    /// Widget size
    pub size: WidgetSize,
}

/// Widget position
#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
pub struct WidgetPosition {
    /// X coordinate
    pub x: u32,

    /// Y coordinate
    pub y: u32,

    /// Z coordinate (layer)
    #[serde(rename = "z")]
    pub z: u32,
}

/// Widget size
#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
pub struct WidgetSize {
    /// Width
    pub width: u32,

    /// Height
    pub height: u32,

    /// Unit
    pub unit: String,
}

/// Execution policies
#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub struct ExecutionPolicies {
    /// Retry policy
    pub retry: RetryPolicy,

    /// Timeout policy
    pub timeout: TimeoutPolicy,

    /// Circuit breaker policy
    #[serde(rename = "circuitBreaker")]
    pub circuit_breaker: CircuitBreakerPolicy,

    /// Bulkhead policy
    pub bulkhead: BulkheadPolicy,

    /// Custom policies
    #[serde(skip_serializing_if = "Option::is_none")]
    pub custom: Option<HashMap<String, serde_json::Value>>,
}

/// Retry policy
#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub struct RetryPolicy {
    /// Maximum retries
    pub max_retries: u32,

    /// Retry delay
    pub delay: std::time::Duration,

    /// Backoff strategy
    pub backoff: BackoffStrategy,

    /// Retry conditions
    #[serde(skip_serializing_if = "Option::is_none")]
    pub conditions: Option<Vec<RetryCondition>>,
}

/// Backoff strategies
#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
#[serde(rename_all = "lowercase")]
pub enum BackoffStrategy {
    /// Fixed backoff
    Fixed,
    /// Exponential backoff
    Exponential,
    /// Linear backoff
    Linear,
    /// Custom backoff
    Custom(String),
}

/// Retry condition
#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
pub struct RetryCondition {
    /// Condition type
    pub condition_type: ConditionType,

    /// Condition expression
    pub expression: String,

    /// Action
    pub action: RetryAction,

    /// Metadata
    #[serde(skip_serializing_if = "Option::is_none")]
    pub metadata: Option<HashMap<String, serde_json::Value>>,
}

/// Condition types
#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
#[serde(rename_all = "lowercase")]
pub enum ConditionType {
    /// Error condition
    Error,
    /// Timeout condition
    Timeout,
    /// Status condition
    Status,
    /// Custom condition
    Custom(String),
}

/// Retry actions
#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
#[serde(rename_all = "lowercase")]
pub enum RetryAction {
    /// Retry
    Retry,
    /// Fail fast
    FailFast,
    /// Skip
    Skip,
    /// Custom action
    Custom(String),
}

/// Timeout policy
#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub struct TimeoutPolicy {
    /// Timeout duration
    pub duration: std::time::Duration,

    /// Timeout type
    pub timeout_type: TimeoutType,

    /// Timeout behavior
    pub behavior: TimeoutBehavior,

    /// Metadata
    #[serde(skip_serializing_if = "Option::is_none")]
    pub metadata: Option<HashMap<String, serde_json::Value>>,
}

/// Timeout types
#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
#[serde(rename_all = "lowercase")]
pub enum TimeoutType {
    /// Operation timeout
    Operation,
    /// Read timeout
    Read,
    /// Write timeout
    Write,
    /// Connection timeout
    Connection,
    /// Custom timeout
    Custom(String),
}

/// Timeout behaviors
#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
#[serde(rename_all = "lowercase")]
pub enum TimeoutBehavior {
    /// Fail fast
    FailFast,
    /// Continue processing
    Continue,
    /// Retry with timeout
    Retry,
    /// Custom behavior
    Custom(String),
}

/// Circuit breaker policy
#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub struct CircuitBreakerPolicy {
    /// Failure threshold
    pub failure_threshold: f64,

    /// Recovery timeout
    pub recovery_timeout: std::time::Duration,

    /// Half-open requests
    pub half_open_requests: u32,

    /// Status
    pub status: CircuitBreakerStatus,

    /// Metadata
    #[serde(skip_serializing_if = "Option::is_none")]
    pub metadata: Option<HashMap<String, serde_json::Value>>,
}

/// Circuit breaker status
#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
#[serde(rename_all = "lowercase")]
pub enum CircuitBreakerStatus {
    /// Closed circuit
    Closed,
    /// Open circuit
    Open,
    /// Half-open circuit
    HalfOpen,
    /// Custom status
    Custom(String),
}

/// Bulkhead policy
#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub struct BulkheadPolicy {
    /// Max concurrent operations
    #[serde(rename = "maxConcurrentOps")]
    pub max_concurrent_ops: u32,

    /// Max pending operations
    #[serde(rename = "maxPendingOps")]
    pub max_pending_ops: u32,

    /// Queue timeout
    #[serde(rename = "queueTimeout")]
    pub queue_timeout: Option<std::time::Duration>,

    /// Metadata
    #[serde(skip_serializing_if = "Option::is_none")]
    pub metadata: Option<HashMap<String, serde_json::Value>>,
}

/// Agent security interface
#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub struct AgentSecurity {
    /// Authentication configuration
    pub authentication: AuthenticationConfig,

    /// Authorization configuration
    pub authorization: AuthorizationConfig,

    /// Encryption configuration
    pub encryption: EncryptionConfig,

    /// Audit configuration
    pub audit: AuditConfig,

    /// Compliance configuration
    #[serde(skip_serializing_if = "Option::is_none")]
    pub compliance: Option<ComplianceConfig>,

    /// Security policies
    #[serde(skip_serializing_if = "Option::is_none")]
    pub policies: Option<SecurityPolicies>,
}

/// Authentication configuration
#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub struct AuthenticationConfig {
    /// Authentication methods
    pub methods: Vec<AuthenticationMethod>,

    /// Authentication providers
    #[serde(skip_serializing_if = "Option::is_none")]
    pub providers: Option<Vec<AuthenticationProvider>>,

    /// Authentication metadata
    #[serde(skip_serializing_if = "Option::is_none")]
    pub metadata: Option<HashMap<String, serde_json::Value>>,
}

/// Authentication methods
#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
#[serde(rename_all = "lowercase")]
pub enum AuthenticationMethod {
    /// Password authentication
    Password,
    /// Token authentication
    Token,
    /// Certificate authentication
    Certificate,
    /// Biometric authentication
    Biometric,
    /// Multi-factor authentication
    MultiFactor,
    /// Custom authentication
    Custom(String),
}

/// Authentication provider
#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
pub struct AuthenticationProvider {
    /// Provider name
    pub name: String,

    /// Provider type
    pub provider_type: ProviderType,

    /// Provider configuration
    pub configuration: HashMap<String, serde_json::Value>,

    /// Provider metadata
    #[serde(skip_serializing_if = "Option::is_none")]
    pub metadata: Option<HashMap<String, serde_json::Value>>,
}

/// Provider types
#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
#[serde(rename_all = "lowercase")]
pub enum ProviderType {
    /// LDAP provider
    Ldap,
    /// OAuth provider
    OAuth,
    /// SAML provider
    Saml,
    /// Custom provider
    Custom(String),
}

/// Authorization configuration
#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub struct AuthorizationConfig {
    /// Authorization model
    pub model: AuthorizationModel,

    /// Authorization policies
    pub policies: Vec<AuthorizationPolicy>,

    /// Authorization roles
    #[serde(skip_serializing_if = "Option::is_none")]
    pub roles: Option<Vec<AuthorizationRole>>,

    /// Authorization metadata
    #[serde(skip_serializing_if = "Option::is_none")]
    pub metadata: Option<HashMap<String, serde_json::Value>>,
}

/// Authorization models
#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
#[serde(rename_all = "lowercase")]
pub enum AuthorizationModel {
    /// Role-based access control
    Rbac,
    /// Attribute-based access control
    Abac,
    /// Policy-based access control
    Pbac,
    /// Custom authorization model
    Custom(String),
}

/// Authorization policy
#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
pub struct AuthorizationPolicy {
    /// Policy name
    pub name: String,

    /// Policy type
    pub policy_type: PolicyType,

    /// Policy rules
    pub rules: Vec<PolicyRule>,

    /// Policy effect
    pub effect: PolicyEffect,

    /// Policy metadata
    #[serde(skip_serializing_if = "Option::is_none")]
    pub metadata: Option<HashMap<String, serde_json::Value>>,
}

/// Policy types
#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
#[serde(rename_all = "lowercase")]
pub enum PolicyType {
    /// Access policy
    Access,
    /// Data policy
    Data,
    /// System policy
    System,
    /// Custom policy
    Custom(String),
}

/// Policy rule
#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
pub struct PolicyRule {
    /// Rule name
    pub name: String,

    /// Rule condition
    pub condition: String,

    /// rule actions
    pub actions: Vec<String>,

    /// rule resources
    pub resources: Vec<String>,

    /// rule metadata
    #[serde(skip_serializing_if = "Option::is_none")]
    pub metadata: Option<HashMap<String, serde_json::Value>>,
}

/// Policy effects
#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
#[serde(rename_all = "lowercase")]
pub enum PolicyEffect {
    /// Allow access
    Allow,
    /// Deny access
    Deny,
    /// Custom effect
    Custom(String),
}

/// Authorization role
#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
pub struct AuthorizationRole {
    /// Role name
    pub name: String,

    /// Role description
    #[serde(skip_serializing_if = "Option::is_none")]
    pub description: Option<String>,

    /// Role permissions
    pub permissions: Vec<Permission>,

    /// Role metadata
    #[serde(skip_serializing_if = "Option::is_none")]
    pub metadata: Option<HashMap<String, serde_json::Value>>,
}

/// Permission
#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
pub struct Permission {
    /// Permission name
    pub name: String,

    /// Permission type
    pub permission_type: PermissionType,

    /// Permission scope
    pub scope: PermissionScope,

    /// permission metadata
    #[serde(skip_serializing_if = "Option::is_none")]
    pub metadata: Option<HashMap<String, serde_json::Value>>,
}

/// Permission types
#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
#[serde(rename_all = "lowercase")]
pub enum PermissionType {
    /// Read permission
    Read,
    /// Write permission
    Write,
    /// Execute permission
    Execute,
    /// Delete permission
    Delete,
    /// Custom permission
    Custom(String),
}

/// Permission scope
#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
#[serde(rename_all = "lowercase")]
pub enum PermissionScope {
    /// Global scope
    Global,
    /// Local scope
    Local,
    /// Resource scope
    Resource,
    /// Custom scope
    Custom(String),
}

/// Encryption configuration
#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub struct EncryptionConfig {
    /// Encryption algorithms
    pub algorithms: Vec<EncryptionAlgorithm>,

    /// Encryption keys
    pub keys: Vec<EncryptionKey>,

    /// encryption modes
    pub modes: Vec<EncryptionMode>,

    /// encryption metadata
    #[serde(skip_serializing_if = "Option::is_none")]
    pub metadata: Option<HashMap<String, serde_json::Value>>,
}

/// Encryption algorithms
#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
#[serde(rename_all = "lowercase")]
pub enum EncryptionAlgorithm {
    /// AES algorithm
    Aes,
    /// RSA algorithm
    Rsa,
    /// ECC algorithm
    Ecc,
    /// Custom algorithm
    Custom(String),
}

/// Encryption key
#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
pub struct EncryptionKey {
    /// Key name
    pub name: String,

    /// Key type
    pub key_type: KeyType,

    /// Key material
    pub material: String,

    /// key metadata
    #[serde(skip_serializing_if = "Option::is_none")]
    pub metadata: Option<HashMap<String, serde_json::Value>>,
}

/// Key types
#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
#[serde(rename_all = "lowercase")]
pub enum KeyType {
    /// Symmetric key
    Symmetric,
    /// Asymmetric key
    Asymmetric,
    /// Public key
    PublicKey,
    /// Private key
    PrivateKey,
    /// Custom key type
    Custom(String),
}

/// Encryption modes
#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
#[serde(rename_all = "lowercase")]
pub enum EncryptionMode {
    /// ECB mode
    Ecb,
    /// CBC mode
    Cbc,
    /// CTR mode
    Ctr,
    /// GCM mode
    Gcm,
    /// Custom mode
    Custom(String),
}

/// Audit configuration
#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub struct AuditConfig {
    /// Audit events
    pub events: Vec<AuditEvent>,

    /// audit destinations
    pub destinations: Vec<AuditDestination>,

    /// audit retention
    pub retention: AuditRetention,

    /// audit metadata
    #[serde(skip_serializing_if = "Option::is_none")]
    pub metadata: Option<HashMap<String, serde_json::Value>>,
}

/// Audit events
#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
#[serde(rename_all = "lowercase")]
pub enum AuditEvent {
    /// Authentication event
    Authentication,
    /// Authorization event
    Authorization,
    /// Data access event
    DataAccess,
    /// System event
    System,
    /// Custom event
    Custom(String),
}

/// Audit destination
#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
pub struct AuditDestination {
    /// destination name
    pub name: String,

    /// destination type
    pub destination_type: DestinationType,

    /// destination configuration
    pub configuration: HashMap<String, serde_json::Value>,

    /// destination metadata
    #[serde(skip_serializing_if = "Option::is_none")]
    pub metadata: Option<HashMap<String, serde_json::Value>>,
}

/// Destination types
#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
#[serde(rename_all = "lowercase")]
pub enum DestinationType {
    /// File destination
    File,
    /// Database destination
    Database,
    /// Log destination
    Log,
    /// Cloud destination
    Cloud,
    /// Custom destination
    Custom(String),
}

/// Audit retention
#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub struct AuditRetention {
    /// Retention period
    pub period: std::time::Duration,

    /// Retention policy
    pub policy: RetentionPolicy,

    /// Retention metadata
    #[serde(skip_serializing_if = "Option::is_none")]
    pub metadata: Option<HashMap<String, serde_json::Value>>,
}

/// Retention policies
#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
#[serde(rename_all = "lowercase")]
pub enum RetentionPolicy {
    /// Permanent retention
    Permanent,
    /// Time-based retention
    TimeBased,
    /// Size-based retention
    SizeBased,
    /// Custom retention
    Custom(String),
}

/// Compliance configuration
#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub struct ComplianceConfig {
    /// Compliance frameworks
    pub frameworks: Vec<ComplianceFramework>,

    /// compliance standards
    pub standards: Vec<ComplianceStandard>,

    /// compliance requirements
    pub requirements: Vec<ComplianceRequirement>,

    /// compliance metadata
    #[serde(skip_serializing_if = "Option::is_none")]
    pub metadata: Option<HashMap<String, serde_json::Value>>,
}

/// Compliance frameworks
#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
#[serde(rename_all = "lowercase")]
pub enum ComplianceFramework {
    /// GDPR compliance
    Gdpr,
    /// HIPAA compliance
    Hipaa,
    /// PCI compliance
    Pci,
    /// SOC compliance
    Soc,
    /// Custom compliance
    Custom(String),
}

/// Compliance standard
#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
pub struct ComplianceStandard {
    /// Standard name
    pub name: String,

    /// Standard version
    pub version: String,

    /// Standard requirements
    pub requirements: Vec<String>,

    /// standard metadata
    #[serde(skip_serializing_if = "Option::is_none")]
    pub metadata: Option<HashMap<String, serde_json::Value>>,
}

/// Compliance requirement
#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
pub struct ComplianceRequirement {
    /// Requirement name
    pub name: String,

    /// requirement type
    pub requirement_type: RequirementType,

    /// requirement controls
    pub controls: Vec<ComplianceControl>,

    /// requirement metadata
    #[serde(skip_serializing_if = "Option::is_none")]
    pub metadata: Option<HashMap<String, serde_json::Value>>,
}

/// Requirement types
#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
#[serde(rename_all = "lowercase")]
pub enum RequirementType {
    /// Technical requirement
    Technical,
    /// Administrative requirement
    Administrative,
    /// Physical requirement
    Physical,
    /// Custom requirement
    Custom(String),
}

/// Compliance control
#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
pub struct ComplianceControl {
    /// Control name
    pub name: String,

    /// control description
    pub description: String,

    /// control implementation
    pub implementation: String,

    /// control metadata
    #[serde(skip_serializing_if = "Option::is_none")]
    pub metadata: Option<HashMap<String, serde_json::Value>>,
}

/// Security policies
#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub struct SecurityPolicies {
    /// Access control policies
    #[serde(rename = "accessControl")]
    pub access_control: Vec<SecurityPolicy>,

    /// Data protection policies
    #[serde(rename = "dataProtection")]
    pub data_protection: Vec<SecurityPolicy>,

    /// Network security policies
    #[serde(rename = "networkSecurity")]
    pub network_security: Vec<SecurityPolicy>,

    /// System security policies
    #[serde(rename = "systemSecurity")]
    pub system_security: Vec<SecurityPolicy>,

    /// Custom policies
    #[serde(skip_serializing_if = "Option::is_none")]
    pub custom: Option<HashMap<String, serde_json::Value>>,
}

/// Security policy
#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
pub struct SecurityPolicy {
    /// Policy name
    pub name: String,

    /// Policy description
    #[serde(skip_serializing_if = "Option::is_none")]
    pub description: Option<String>,

    /// Policy rules
    pub rules: Vec<SecurityRule>,

    /// policy priority
    pub priority: PolicyPriority,

    /// policy metadata
    #[serde(skip_serializing_if = "Option::is_none")]
    pub metadata: Option<HashMap<String, serde_json::Value>>,
}

/// Security rule
#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
pub struct SecurityRule {
    /// Rule name
    pub name: String,

    /// Rule condition
    pub condition: String,

    /// rule actions
    pub actions: Vec<String>,

    /// rule targets
    pub targets: Vec<String>,

    /// rule metadata
    #[serde(skip_serializing_if = "Option::is_none")]
    pub metadata: Option<HashMap<String, serde_json::Value>>,
}

/// Policy priority
#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
#[serde(rename_all = "lowercase")]
pub enum PolicyPriority {
    /// Highest priority
    Highest,
    /// High priority
    High,
    /// Normal priority
    Normal,
    /// Low priority
    Low,
    /// Lowest priority
    Lowest,
}

/// Import dependencies from converged message module
use super::message::{ConvergedMessageType, ReliabilityLevel, LatencyRequirements, ThroughputRequirements};

// Unified agent builder
pub struct UnifiedAgentBuilder {
    identity: AgentIdentity,
    capabilities: AgentCapabilities,
    lifecycle: AgentLifecycle,
    communication: AgentCommunication,
    execution: AgentExecution,
    security: AgentSecurity,
    extensions: Option<HashMap<String, serde_json::Value>>,
}

impl UnifiedAgentBuilder {
    pub fn new(id: String, name: String, agent_type: String, namespace: String) -> Self {
        Self {
            identity: AgentIdentity {
                id,
                name,
                agent_type,
                version: "1.0.0".to_string(),
                namespace,
                tags: None,
            },
            capabilities: AgentCapabilities {
                primary: Vec::new(),
                secondary: None,
                protocols: vec![AgentProtocol::Http],
                formats: vec![DataFormat::Json],
                message_types: vec![ConvergedMessageType::Direct],
                qos_levels: vec![QoSLevel::AtLeastOnce],
                constraints: None,
            },
            lifecycle: AgentLifecycle {
                state: AgentState::Created,
                state_history: Vec::new(),
                health: AgentHealth {
                    status: HealthStatus::Healthy,
                    last_check: Utc::now(),
                    check_interval: std::time::Duration::from_secs(30),
                    metrics: None,
                    warnings: None,
                    errors: None,
                },
                metrics: None,
                configuration: AgentConfiguration {
                    parameters: HashMap::new(),
                    version: "1.0.0".to_string(),
                    timestamp: Utc::now(),
                    source: None,
                    validation: None,
                },
                dependencies: None,
                timeouts: None,
            },
            communication: AgentCommunication {
                endpoints: Vec::new(),
                protocols: vec![AgentProtocol::Http],
                handlers: None,
                security: None,
                qos: CommunicationQoS {
                    reliability: ReliabilityLevel::AtLeastOnce,
                    latency: None,
                    throughput: None,
                    ordering: None,
                    flow_control: None,
                },
            },
            execution: AgentExecution {
                mode: ExecutionMode::Synchronous,
                parameters: HashMap::new(),
                context: None,
                strategy: None,
                monitoring: None,
                policies: None,
            },
            security: AgentSecurity {
                authentication: AuthenticationConfig {
                    methods: vec![AuthenticationMethod::Token],
                    providers: None,
                    metadata: None,
                },
                authorization: AuthorizationConfig {
                    model: AuthorizationModel::Rbac,
                    policies: Vec::new(),
                    roles: None,
                    metadata: None,
                },
                encryption: EncryptionConfig {
                    algorithms: vec![EncryptionAlgorithm::Aes],
                    keys: Vec::new(),
                    modes: vec![EncryptionMode::Gcm],
                    metadata: None,
                },
                audit: AuditConfig {
                    events: vec![AuditEvent::Authentication, AuditEvent::Authorization],
                    destinations: Vec::new(),
                    retention: AuditRetention {
                        period: std::time::Duration::from_secs(365 * 24 * 60 * 60),
                        policy: RetentionPolicy::TimeBased,
                        metadata: None,
                    },
                    metadata: None,
                },
                compliance: None,
                policies: None,
            },
            extensions: None,
        }
    }

    pub fn with_capability(mut self, capability: Capability) -> Self {
        self.capabilities.primary.push(capability);
        self
    }

    pub fn with_protocol(mut self, protocol: AgentProtocol) -> Self {
        self.capabilities.protocols.push(protocol);
        self
    }

    pub fn with_message_type(mut self, message_type: ConvergedMessageType) -> Self {
        self.capabilities.message_types.push(message_type);
        self
    }

    pub fn with_endpoint(mut self, endpoint: CommunicationEndpoint) -> Self {
        self.communication.endpoints.push(endpoint);
        self
    }

    pub fn with_policy(mut self, policy: SecurityPolicy) -> Self {
        if let Some(policies) = &mut self.security.policies {
            policies.access_control.push(policy);
        } else {
            self.security.policies = Some(SecurityPolicies {
                access_control: vec![policy],
                data_protection: Vec::new(),
                network_security: Vec::new(),
                system_security: Vec::new(),
                custom: None,
            });
        }
        self
    }

    pub fn build(self) -> UnifiedAgent {
        UnifiedAgent {
            identity: self.identity,
            capabilities: self.capabilities,
            lifecycle: self.lifecycle,
            communication: self.communication,
            execution: self.execution,
            security: self.security,
            extensions: self.extensions,
        }
    }
}

// Helper implementations for common patterns
impl UnifiedAgent {
    /// Create a basic agent
    pub fn basic(id: String, name: String, agent_type: String) -> Self {
        UnifiedAgentBuilder::new(id, name, agent_type, "default".to_string()).build()
    }

    /// Create an agent with specific capabilities
    pub fn with_capabilities(id: String, name: String, agent_type: String, capabilities: Vec<Capability>) -> Self {
        let mut builder = UnifiedAgentBuilder::new(id, name, agent_type, "default".to_string());
        for capability in capabilities {
            builder = builder.with_capability(capability);
        }
        builder.build()
    }

    /// Add a message type to the agent
    pub fn with_message_type(mut self, message_type: ConvergedMessageType) -> Self {
        self.capabilities.message_types.push(message_type);
        self
    }

    /// Add a protocol to the agent
    pub fn with_protocol(mut self, protocol: AgentProtocol) -> Self {
        self.capabilities.protocols.push(protocol);
        self
    }

    /// Add an endpoint to the agent
    pub fn with_endpoint(mut self, endpoint: CommunicationEndpoint) -> Self {
        self.communication.endpoints.push(endpoint);
        self
    }

    /// Validate the agent configuration
    pub fn validate(&self) -> Result<(), String> {
        if self.identity.id.is_empty() {
            return Err("Agent ID cannot be empty".to_string());
        }

        if self.identity.name.is_empty() {
            return Err("Agent name cannot be empty".to_string());
        }

        if self.capabilities.primary.is_empty() {
            return Err("Agent must have at least one primary capability".to_string());
        }

        if self.capabilities.protocols.is_empty() {
            return Err("Agent must support at least one protocol".to_string());
        }

        Ok(())
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_unified_agent_creation() {
        let agent = UnifiedAgent::basic(
            "agent-123".to_string(),
            "Test Agent".to_string(),
            "test".to_string(),
        );

        assert_eq!(agent.identity.id, "agent-123");
        assert_eq!(agent.identity.name, "Test Agent");
        assert_eq!(agent.identity.agent_type, "test");
        assert_eq!(agent.identity.namespace, "default");
        assert_eq!(agent.capabilities.protocols, vec![AgentProtocol::Http]);
        assert_eq!(agent.capabilities.formats, vec![DataFormat::Json]);
    }

    #[test]
    fn test_agent_with_capabilities() {
        let capability = Capability {
            name: "text-processing".to_string(),
            version: "1.0.0".to_string(),
            description: Some("Process text content".to_string()),
            requirements: None,
            metadata: None,
        };

        let agent = UnifiedAgent::with_capabilities(
            "agent-456".to_string(),
            "Smart Agent".to_string(),
            "smart".to_string(),
            vec![capability],
        );

        assert_eq!(agent.capabilities.primary.len(), 1);
        assert_eq!(agent.capabilities.primary[0].name, "text-processing");
    }

    #[test]
    fn test_agent_validation() {
        let valid_agent = UnifiedAgent::basic(
            "valid-agent".to_string(),
            "Valid Agent".to_string(),
            "test".to_string(),
        );

        assert!(valid_agent.validate().is_ok());

        let mut invalid_agent = valid_agent;
        invalid_agent.identity.id = "".to_string();
        assert!(invalid_agent.validate().is_err());
    }

    #[test]
    fn test_agent_builder() {
        let capability = Capability {
            name: "data-processing".to_string(),
            version: "1.0.0".to_string(),
            description: Some("Process data".to_string()),
            requirements: None,
            metadata: None,
        };

        let endpoint = CommunicationEndpoint {
            url: "http://localhost:8080".to_string(),
            endpoint_type: EndpointType::Rest,
            authentication: None,
            metadata: None,
        };

        let policy = SecurityPolicy {
            name: "access-control".to_string(),
            description: Some("Control access to agent".to_string()),
            rules: Vec::new(),
            priority: PolicyPriority::High,
            metadata: None,
        };

        let agent = UnifiedAgentBuilder::new(
            "builder-agent".to_string(),
            "Builder Agent".to_string(),
            "builder".to_string(),
            "default".to_string(),
        )
        .with_capability(capability)
        .with_protocol(AgentProtocol::WebSocket)
        .with_endpoint(endpoint)
        .with_policy(policy)
        .build();

        assert_eq!(agent.identity.id, "builder-agent");
        assert_eq!(agent.capabilities.primary.len(), 1);
        assert_eq!(agent.capabilities.protocols.len(), 2);
        assert_eq!(agent.communication.endpoints.len(), 1);
    }
}