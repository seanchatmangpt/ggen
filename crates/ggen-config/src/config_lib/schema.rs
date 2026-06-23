//! Schema definitions for ggen.toml configuration
//!
//! This module defines the complete structure of ggen.toml files
//! using serde-compatible Rust structs.

use serde::{Deserialize, Serialize};
use std::collections::HashMap;

/// Root configuration structure for ggen.toml
/// `PartialEq` without Eq: Contains nested config structs via composition
#[allow(clippy::derive_partial_eq_without_eq)]
#[derive(Debug, Clone, Serialize, Deserialize, PartialEq)]
#[serde(deny_unknown_fields)]
pub struct GgenConfig {
    /// Project metadata
    pub project: ProjectConfig,

    /// AI configuration (optional)
    #[serde(skip_serializing_if = "Option::is_none")]
    pub ai: Option<AiConfig>,

    /// Templates configuration (optional)
    #[serde(skip_serializing_if = "Option::is_none")]
    pub templates: Option<TemplatesConfig>,

    /// RDF configuration (optional)
    #[serde(skip_serializing_if = "Option::is_none")]
    pub rdf: Option<RdfConfig>,

    /// SPARQL configuration (optional)
    #[serde(skip_serializing_if = "Option::is_none")]
    pub sparql: Option<SparqlConfig>,

    /// Lifecycle configuration (optional)
    #[serde(skip_serializing_if = "Option::is_none")]
    pub lifecycle: Option<LifecycleConfig>,

    /// Security settings (optional)
    #[serde(skip_serializing_if = "Option::is_none")]
    pub security: Option<SecurityConfig>,

    /// Performance settings (optional)
    #[serde(skip_serializing_if = "Option::is_none")]
    pub performance: Option<PerformanceConfig>,

    /// Logging configuration (optional)
    #[serde(skip_serializing_if = "Option::is_none")]
    pub logging: Option<LoggingConfig>,

    /// Telemetry configuration (optional)
    #[serde(skip_serializing_if = "Option::is_none")]
    pub telemetry: Option<TelemetryConfig>,

    /// Feature flags (optional)
    #[serde(skip_serializing_if = "Option::is_none")]
    pub features: Option<HashMap<String, bool>>,

    /// Environment-specific overrides (optional)
    #[serde(skip_serializing_if = "Option::is_none")]
    pub env: Option<HashMap<String, serde_json::Value>>,

    /// Build configuration (optional)
    #[serde(skip_serializing_if = "Option::is_none")]
    pub build: Option<BuildConfig>,

    /// Test configuration (optional)
    #[serde(skip_serializing_if = "Option::is_none")]
    pub test: Option<TestConfig>,

    /// Package metadata (for marketplace packages)
    #[serde(skip_serializing_if = "Option::is_none")]
    pub package: Option<PackageMetadata>,

    /// Inference configuration (optional)
    #[serde(skip_serializing_if = "Option::is_none")]
    pub inference: Option<InferenceConfig>,

    /// Generation configuration (optional)
    #[serde(skip_serializing_if = "Option::is_none")]
    pub generation: Option<GenerationConfig>,

    /// MCP configuration (optional)
    #[serde(skip_serializing_if = "Option::is_none")]
    pub mcp: Option<McpConfig>,

    /// A2A configuration (optional)
    #[serde(skip_serializing_if = "Option::is_none")]
    pub a2a: Option<A2AConfig>,
}

/// Project metadata configuration
#[derive(Debug, Clone, Serialize, Deserialize, PartialEq, Eq)]
pub struct ProjectConfig {
    /// Project name
    pub name: String,

    /// Project version
    pub version: String,

    /// Project description (optional)
    #[serde(skip_serializing_if = "Option::is_none")]
    pub description: Option<String>,

    /// Project authors (optional)
    #[serde(skip_serializing_if = "Option::is_none")]
    pub authors: Option<Vec<String>>,

    /// Project license (optional)
    #[serde(skip_serializing_if = "Option::is_none")]
    pub license: Option<String>,

    /// Project repository URL (optional)
    #[serde(skip_serializing_if = "Option::is_none")]
    pub repository: Option<String>,
}

/// AI provider configuration
/// `PartialEq` without Eq: temperature (f32) does not implement Eq
#[allow(clippy::derive_partial_eq_without_eq)]
#[derive(Debug, Clone, Serialize, Deserialize, PartialEq)]
pub struct AiConfig {
    /// AI provider (openai, ollama, anthropic, etc.)
    pub provider: String,

    /// Model name
    pub model: String,

    /// Temperature for generation (0.0 - 1.0)
    #[serde(default = "default_temperature")]
    pub temperature: f32,

    /// Maximum tokens for generation
    #[serde(default = "default_max_tokens")]
    pub max_tokens: u32,

    /// Request timeout in seconds
    #[serde(default = "default_timeout")]
    pub timeout: u32,

    /// System and user prompts (optional)
    #[serde(skip_serializing_if = "Option::is_none")]
    pub prompts: Option<AiPrompts>,

    /// Validation settings (optional)
    #[serde(skip_serializing_if = "Option::is_none")]
    pub validation: Option<AiValidation>,
}

/// AI prompt configuration
#[derive(Debug, Clone, Serialize, Deserialize, PartialEq, Eq)]
pub struct AiPrompts {
    /// System prompt
    #[serde(skip_serializing_if = "Option::is_none")]
    pub system: Option<String>,

    /// User prompt prefix
    #[serde(skip_serializing_if = "Option::is_none")]
    pub user_prefix: Option<String>,
}

/// AI validation configuration
/// `PartialEq` without Eq: `quality_threshold` (f32) does not implement Eq
#[allow(clippy::derive_partial_eq_without_eq)]
#[derive(Debug, Clone, Serialize, Deserialize, PartialEq)]
pub struct AiValidation {
    /// Whether validation is enabled
    #[serde(default)]
    pub enabled: bool,

    /// Quality threshold (0.0 - 1.0)
    #[serde(default = "default_quality_threshold")]
    pub quality_threshold: f32,

    /// Maximum validation iterations
    #[serde(default = "default_max_iterations")]
    pub max_iterations: u32,
}

/// Templates configuration
#[derive(Debug, Clone, Serialize, Deserialize, PartialEq, Eq)]
pub struct TemplatesConfig {
    /// Template source directory
    #[serde(skip_serializing_if = "Option::is_none")]
    pub directory: Option<String>,

    /// Output directory for generated files
    #[serde(skip_serializing_if = "Option::is_none")]
    pub output_directory: Option<String>,

    /// Enable backup before overwriting
    #[serde(default)]
    pub backup_enabled: bool,

    /// Idempotent generation (only update if changed)
    #[serde(default)]
    pub idempotent: bool,
}

/// RDF configuration
#[derive(Debug, Clone, Serialize, Deserialize, PartialEq, Eq)]
pub struct RdfConfig {
    /// Base IRI/URI for RDF entities
    #[serde(skip_serializing_if = "Option::is_none")]
    pub base_uri: Option<String>,

    /// Base IRI (alternative name)
    #[serde(skip_serializing_if = "Option::is_none")]
    pub base_iri: Option<String>,

    /// RDF namespace prefixes
    #[serde(skip_serializing_if = "Option::is_none")]
    pub prefixes: Option<HashMap<String, String>>,

    /// Default RDF format (turtle, rdfxml, etc.)
    #[serde(skip_serializing_if = "Option::is_none")]
    pub default_format: Option<String>,

    /// Enable query caching
    #[serde(default)]
    pub cache_queries: bool,
}

/// SPARQL configuration
#[derive(Debug, Clone, Serialize, Deserialize, PartialEq, Eq)]
pub struct SparqlConfig {
    /// Query timeout in seconds
    #[serde(default = "default_sparql_timeout")]
    pub timeout: u32,

    /// Maximum results per query
    #[serde(default = "default_max_results")]
    pub max_results: u32,

    /// Enable query caching
    #[serde(default)]
    pub cache_enabled: bool,
}

/// Lifecycle configuration
#[derive(Debug, Clone, Serialize, Deserialize, PartialEq, Eq)]
pub struct LifecycleConfig {
    /// Enable lifecycle management
    #[serde(default)]
    pub enabled: bool,

    /// Lifecycle config file (e.g., make.toml)
    #[serde(skip_serializing_if = "Option::is_none")]
    pub config_file: Option<String>,

    /// Cache directory
    #[serde(skip_serializing_if = "Option::is_none")]
    pub cache_directory: Option<String>,

    /// State file path
    #[serde(skip_serializing_if = "Option::is_none")]
    pub state_file: Option<String>,

    /// Lifecycle phases
    #[serde(skip_serializing_if = "Option::is_none")]
    pub phases: Option<HashMap<String, Vec<String>>>,
}

/// Security configuration
#[derive(Debug, Clone, Serialize, Deserialize, PartialEq, Eq)]
pub struct SecurityConfig {
    /// Enable path traversal protection
    #[serde(default = "default_true")]
    pub path_traversal_protection: bool,

    /// Enable shell injection protection
    #[serde(default = "default_true")]
    pub shell_injection_protection: bool,

    /// Enable template sandboxing
    #[serde(default = "default_true")]
    pub template_sandboxing: bool,

    /// Validate file paths
    #[serde(default = "default_true")]
    pub validate_paths: bool,

    /// Require user confirmation for destructive operations
    #[serde(default)]
    pub require_confirmation: bool,

    /// Audit all operations
    #[serde(default)]
    pub audit_operations: bool,

    /// Backup before write operations
    #[serde(default)]
    pub backup_before_write: bool,
}

/// Performance configuration
#[derive(Debug, Clone, Serialize, Deserialize, PartialEq, Eq)]
pub struct PerformanceConfig {
    /// Enable parallel execution
    #[serde(default)]
    pub parallel_execution: bool,

    /// Maximum parallel workers
    #[serde(default = "default_max_workers")]
    pub max_workers: u32,

    /// Cache size (as string, e.g., "1GB")
    #[serde(skip_serializing_if = "Option::is_none")]
    pub cache_size: Option<String>,

    /// Enable profiling
    #[serde(default)]
    pub enable_profiling: bool,

    /// Memory limit in MB
    #[serde(skip_serializing_if = "Option::is_none")]
    pub memory_limit_mb: Option<u32>,
}

/// Logging configuration
#[derive(Debug, Clone, Serialize, Deserialize, PartialEq, Eq)]
pub struct LoggingConfig {
    /// Log level (debug, info, warn, error)
    #[serde(default = "default_log_level")]
    pub level: String,

    /// Log format (json, text)
    #[serde(default = "default_log_format")]
    pub format: String,

    /// Log file path (optional)
    #[serde(skip_serializing_if = "Option::is_none")]
    pub file: Option<String>,

    /// Log rotation (daily, size-based, etc.)
    #[serde(skip_serializing_if = "Option::is_none")]
    pub rotation: Option<String>,
}

/// Telemetry configuration
#[derive(Debug, Clone, Serialize, Deserialize, PartialEq, Eq)]
pub struct TelemetryConfig {
    /// OTLP endpoint (default: <http://localhost:4317>)
    #[serde(default = "default_telemetry_endpoint")]
    pub endpoint: String,

    /// Service name for traces (default: "ggen")
    #[serde(default = "default_telemetry_service_name")]
    pub service_name: String,

    /// Whether to enable console output
    #[serde(default = "default_telemetry_console_output")]
    pub console_output: bool,
}

/// Build configuration
#[derive(Debug, Clone, Serialize, Deserialize, PartialEq, Eq)]
pub struct BuildConfig {
    /// Build target (release, debug)
    #[serde(skip_serializing_if = "Option::is_none")]
    pub target: Option<String>,

    /// Features to enable
    #[serde(skip_serializing_if = "Option::is_none")]
    pub features: Option<Vec<String>>,

    /// Build profile
    #[serde(skip_serializing_if = "Option::is_none")]
    pub profile: Option<String>,

    /// Number of parallel build jobs
    #[serde(skip_serializing_if = "Option::is_none")]
    pub parallel_jobs: Option<u32>,
}

/// Test configuration
#[derive(Debug, Clone, Serialize, Deserialize, PartialEq, Eq)]
pub struct TestConfig {
    /// Test framework
    #[serde(skip_serializing_if = "Option::is_none")]
    pub framework: Option<String>,

    /// Enable parallel test execution
    #[serde(default)]
    pub parallel: bool,

    /// Test timeout in seconds
    #[serde(skip_serializing_if = "Option::is_none")]
    pub timeout_seconds: Option<u32>,

    /// Enable code coverage
    #[serde(default)]
    pub coverage_enabled: bool,

    /// Coverage threshold percentage
    #[serde(skip_serializing_if = "Option::is_none")]
    pub coverage_threshold: Option<u32>,
}

/// Package metadata (for marketplace packages)
#[derive(Debug, Clone, Serialize, Deserialize, PartialEq, Eq)]
pub struct PackageMetadata {
    /// Package name
    pub name: String,

    /// Package version
    pub version: String,

    /// Description
    #[serde(skip_serializing_if = "Option::is_none")]
    pub description: Option<String>,

    /// Authors
    #[serde(skip_serializing_if = "Option::is_none")]
    pub authors: Option<Vec<String>>,

    /// License
    #[serde(skip_serializing_if = "Option::is_none")]
    pub license: Option<String>,

    /// Repository URL
    #[serde(skip_serializing_if = "Option::is_none")]
    pub repository: Option<String>,

    /// Keywords
    #[serde(skip_serializing_if = "Option::is_none")]
    pub keywords: Option<Vec<String>>,

    /// Categories
    #[serde(skip_serializing_if = "Option::is_none")]
    pub categories: Option<Vec<String>>,

    /// Package-specific metadata
    #[serde(skip_serializing_if = "Option::is_none")]
    pub metadata: Option<HashMap<String, serde_json::Value>>,
}

/// MCP (Model Context Protocol) configuration
///
/// Configuration for MCP server and client integration.
/// Enables LLM-driven agents to discover and invoke tools through
/// a standardized JSON-RPC interface.
#[derive(Debug, Clone, Serialize, Deserialize, PartialEq, Eq)]
pub struct McpConfig {
    /// Server name
    #[serde(skip_serializing_if = "Option::is_none")]
    pub name: Option<String>,

    /// Server version
    #[serde(skip_serializing_if = "Option::is_none")]
    pub version: Option<String>,

    /// Maximum time to wait for tool execution (milliseconds)
    #[serde(default = "default_mcp_tool_timeout")]
    pub tool_timeout_ms: u64,

    /// Maximum concurrent requests
    #[serde(default = "default_mcp_max_concurrent")]
    pub max_concurrent_requests: usize,

    /// Server transport configuration
    #[serde(skip_serializing_if = "Option::is_none")]
    pub transport: Option<McpTransportConfig>,

    /// Tool registry configuration
    #[serde(skip_serializing_if = "Option::is_none")]
    pub tools: Option<McpToolsConfig>,

    /// ZAI integration settings
    #[serde(skip_serializing_if = "Option::is_none")]
    pub zai: Option<McpZaiConfig>,

    /// Enable MCP server
    #[serde(default = "default_mcp_enabled")]
    pub enabled: bool,

    /// Server discovery settings
    #[serde(skip_serializing_if = "Option::is_none")]
    pub discovery: Option<McpDiscoveryConfig>,
}

/// MCP transport configuration
#[derive(Debug, Clone, Serialize, Deserialize, PartialEq, Eq, Default)]
pub struct McpTransportConfig {
    /// Transport type (stdio, http, websocket)
    #[serde(default = "default_mcp_transport_type")]
    pub transport_type: String,

    /// HTTP server port (for http/websocket transport)
    #[serde(skip_serializing_if = "Option::is_none")]
    pub port: Option<u16>,

    /// HTTP server host
    #[serde(default = "default_mcp_host")]
    pub host: String,

    /// TLS configuration
    #[serde(skip_serializing_if = "Option::is_none")]
    pub tls: Option<McpTlsConfig>,

    /// Request timeout (seconds)
    #[serde(default = "default_mcp_request_timeout")]
    pub request_timeout_seconds: u64,
}

/// MCP TLS configuration
#[derive(Debug, Clone, Serialize, Deserialize, PartialEq, Eq)]
pub struct McpTlsConfig {
    /// Enable TLS
    #[serde(default)]
    pub enabled: bool,

    /// Path to certificate file
    #[serde(skip_serializing_if = "Option::is_none")]
    pub cert_path: Option<String>,

    /// Path to private key file
    #[serde(skip_serializing_if = "Option::is_none")]
    pub key_path: Option<String>,

    /// Path to CA certificate file
    #[serde(skip_serializing_if = "Option::is_none")]
    pub ca_path: Option<String>,
}

/// MCP tools configuration
#[derive(Debug, Clone, Serialize, Deserialize, PartialEq, Eq)]
pub struct McpToolsConfig {
    /// Tool discovery path (directory to scan for tools)
    #[serde(skip_serializing_if = "Option::is_none")]
    pub discovery_path: Option<String>,

    /// Tool registration is required
    #[serde(default)]
    pub require_registration: bool,

    /// Validate tool signatures
    #[serde(default = "default_true")]
    pub validate_signatures: bool,

    /// Allowed tool prefixes (whitelist)
    #[serde(skip_serializing_if = "Option::is_none")]
    pub allowed_prefixes: Option<Vec<String>>,
}

/// MCP ZAI integration configuration
#[derive(Debug, Clone, Serialize, Deserialize, PartialEq, Eq)]
pub struct McpZaiConfig {
    /// Enable ZAI integration
    #[serde(default)]
    pub enabled: bool,

    /// ZAI provider URL
    #[serde(skip_serializing_if = "Option::is_none")]
    pub provider_url: Option<String>,

    /// ZAI model for tool processing
    #[serde(skip_serializing_if = "Option::is_none")]
    pub model: Option<String>,

    /// Cache ZAI responses
    #[serde(default = "default_mcp_zai_cache")]
    pub cache_enabled: bool,

    /// Cache TTL (seconds)
    #[serde(default = "default_mcp_zai_cache_ttl")]
    pub cache_ttl_seconds: u64,
}

/// MCP discovery configuration
#[derive(Debug, Clone, Serialize, Deserialize, PartialEq, Eq)]
pub struct McpDiscoveryConfig {
    /// Enable server discovery
    #[serde(default)]
    pub enabled: bool,

    /// Discovery method (local, remote, hybrid)
    #[serde(default = "default_mcp_discovery_method")]
    pub method: String,

    /// Remote discovery URL
    #[serde(skip_serializing_if = "Option::is_none")]
    pub registry_url: Option<String>,

    /// Discovery cache TTL (seconds)
    #[serde(default = "default_mcp_discovery_cache_ttl")]
    pub cache_ttl_seconds: u64,
}

/// A2A (Agent-to-Agent) configuration
///
/// Configuration for A2A message passing and agent coordination.
/// Enables agents to communicate, coordinate, and orchestrate workflows.
#[derive(Debug, Clone, Serialize, Deserialize, PartialEq, Eq)]
pub struct A2AConfig {
    /// Agent identifier
    #[serde(skip_serializing_if = "Option::is_none")]
    pub agent_id: Option<String>,

    /// Agent name
    #[serde(skip_serializing_if = "Option::is_none")]
    pub agent_name: Option<String>,

    /// Agent type (coordinator, worker, specialist, etc.)
    #[serde(skip_serializing_if = "Option::is_none")]
    pub agent_type: Option<String>,

    /// Transport configuration
    #[serde(skip_serializing_if = "Option::is_none")]
    pub transport: Option<A2ATransportConfig>,

    /// Message handling configuration
    #[serde(skip_serializing_if = "Option::is_none")]
    pub messaging: Option<A2AMessagingConfig>,

    /// Orchestration settings
    #[serde(skip_serializing_if = "Option::is_none")]
    pub orchestration: Option<A2AOrchestrationConfig>,

    /// Agent capabilities
    #[serde(skip_serializing_if = "Option::is_none")]
    pub capabilities: Option<Vec<String>>,

    /// Enable A2A
    #[serde(default = "default_a2a_enabled")]
    pub enabled: bool,
}

/// A2A transport configuration
#[derive(Debug, Clone, Serialize, Deserialize, PartialEq, Eq, Default)]
pub struct A2ATransportConfig {
    /// Transport type (memory, http, websocket, amqp)
    #[serde(default = "default_a2a_transport_type")]
    pub transport_type: String,

    /// Server bind address
    #[serde(skip_serializing_if = "Option::is_none")]
    pub bind_address: Option<String>,

    /// Server port
    #[serde(skip_serializing_if = "Option::is_none")]
    pub port: Option<u16>,

    /// Connection timeout (milliseconds)
    #[serde(default = "default_a2a_timeout")]
    pub timeout_ms: u64,

    /// Maximum concurrent connections
    #[serde(skip_serializing_if = "Option::is_none")]
    pub max_connections: Option<usize>,

    /// Retry configuration
    #[serde(skip_serializing_if = "Option::is_none")]
    pub retry: Option<A2ARetryConfig>,
}

/// A2A retry configuration
#[derive(Debug, Clone, Serialize, Deserialize, PartialEq, Eq)]
pub struct A2ARetryConfig {
    /// Maximum retry attempts
    #[serde(default = "default_a2a_max_retries")]
    pub max_attempts: u32,

    /// Initial retry delay (milliseconds)
    #[serde(default = "default_a2a_retry_delay")]
    pub initial_delay_ms: u64,

    /// Maximum retry delay (milliseconds)
    #[serde(default = "default_a2a_max_retry_delay")]
    pub max_delay_ms: u64,

    /// Enable exponential backoff
    #[serde(default)]
    pub exponential_backoff: bool,
}

/// A2A messaging configuration
#[derive(Debug, Clone, Serialize, Deserialize, PartialEq, Eq)]
pub struct A2AMessagingConfig {
    /// Message queue size
    #[serde(default = "default_a2a_queue_size")]
    pub queue_size: usize,

    /// Message TTL (seconds)
    #[serde(default = "default_a2a_message_ttl")]
    pub message_ttl_seconds: u64,

    /// Enable message persistence
    #[serde(default)]
    pub persistence_enabled: bool,

    /// Persistence path
    #[serde(skip_serializing_if = "Option::is_none")]
    pub persistence_path: Option<String>,

    /// Enable message signing
    #[serde(default)]
    pub signing_enabled: bool,

    /// Signature algorithm (ed25519, rsa, ecdsa)
    #[serde(skip_serializing_if = "Option::is_none")]
    pub signature_algorithm: Option<String>,
}

/// A2A orchestration configuration
#[derive(Debug, Clone, Serialize, Deserialize, PartialEq, Eq, Default)]
pub struct A2AOrchestrationConfig {
    /// Orchestration mode (centralized, decentralized, hierarchical)
    #[serde(default = "default_a2a_orchestration_mode")]
    pub mode: String,

    /// Coordinator address (for centralized mode)
    #[serde(skip_serializing_if = "Option::is_none")]
    pub coordinator_address: Option<String>,

    /// Heartbeat interval (seconds)
    #[serde(default = "default_a2a_heartbeat_interval")]
    pub heartbeat_interval_seconds: u64,

    /// Agent timeout (seconds)
    #[serde(default = "default_a2a_agent_timeout")]
    pub agent_timeout_seconds: u64,

    /// Enable consensus
    #[serde(default)]
    pub consensus_enabled: bool,

    /// Consensus algorithm (raft, pbft, naive)
    #[serde(skip_serializing_if = "Option::is_none")]
    pub consensus_algorithm: Option<String>,
}

/// Inference configuration for graph modifications
#[derive(Debug, Clone, Serialize, Deserialize, PartialEq, Eq)]
pub struct InferenceConfig {
    /// List of inference rules
    #[serde(default)]
    pub rules: Vec<InferenceRule>,
}

/// A single inference rule
#[derive(Debug, Clone, Serialize, Deserialize, PartialEq, Eq)]
pub struct InferenceRule {
    /// Rule name
    pub name: String,
    /// CONSTRUCT query
    pub construct: String,
    /// Optional execution order
    #[serde(default)]
    pub order: i32,
    /// Optional ASK query condition
    #[serde(skip_serializing_if = "Option::is_none")]
    pub when: Option<String>,
}

/// Generation configuration
#[derive(Debug, Clone, Serialize, Deserialize, PartialEq, Eq)]
pub struct GenerationConfig {
    /// Output directory
    pub output_dir: String,
    /// Generation rules
    #[serde(default)]
    pub rules: Vec<serde_json::Value>,
}

// Default value functions for serde
const fn default_temperature() -> f32 {
    0.7
}

const fn default_max_tokens() -> u32 {
    2000
}

const fn default_timeout() -> u32 {
    30
}

const fn default_quality_threshold() -> f32 {
    0.8
}

const fn default_max_iterations() -> u32 {
    3
}

const fn default_sparql_timeout() -> u32 {
    10
}

const fn default_max_results() -> u32 {
    1000
}

fn default_max_workers() -> u32 {
    num_cpus()
}

fn default_log_level() -> String {
    "info".to_string()
}

fn default_log_format() -> String {
    "text".to_string()
}

const fn default_true() -> bool {
    true
}

fn num_cpus() -> u32 {
    std::thread::available_parallelism().map_or(4, |n| n.get() as u32)
}

impl Default for GgenConfig {
    fn default() -> Self {
        Self {
            project: ProjectConfig {
                name: "unnamed".to_string(),
                version: "0.1.0".to_string(),
                description: None,
                authors: None,
                license: None,
                repository: None,
            },
            ai: None,
            templates: None,
            rdf: None,
            sparql: None,
            lifecycle: None,
            security: None,
            performance: None,
            logging: None,
            telemetry: None,
            features: None,
            env: None,
            build: None,
            test: None,
            package: None,
            inference: None,
            generation: None,
            mcp: None,
            a2a: None,
        }
    }
}

// MCP and A2A default value functions for serde
const fn default_mcp_tool_timeout() -> u64 {
    30000
}

const fn default_mcp_max_concurrent() -> usize {
    100
}

fn default_mcp_transport_type() -> String {
    "stdio".to_string()
}

fn default_mcp_host() -> String {
    "127.0.0.1".to_string()
}

const fn default_mcp_request_timeout() -> u64 {
    30
}

fn default_mcp_enabled() -> bool {
    false
}

const fn default_mcp_zai_cache() -> bool {
    true
}

const fn default_mcp_zai_cache_ttl() -> u64 {
    3600
}

fn default_mcp_discovery_method() -> String {
    "local".to_string()
}

const fn default_mcp_discovery_cache_ttl() -> u64 {
    7200
}

fn default_a2a_enabled() -> bool {
    false
}

fn default_a2a_transport_type() -> String {
    "memory".to_string()
}

const fn default_a2a_timeout() -> u64 {
    5000
}

const fn default_a2a_max_retries() -> u32 {
    3
}

const fn default_a2a_retry_delay() -> u64 {
    1000
}

const fn default_a2a_max_retry_delay() -> u64 {
    30000
}

const fn default_a2a_queue_size() -> usize {
    1000
}

const fn default_a2a_message_ttl() -> u64 {
    3600
}

fn default_a2a_orchestration_mode() -> String {
    "decentralized".to_string()
}

const fn default_a2a_heartbeat_interval() -> u64 {
    30
}

const fn default_a2a_agent_timeout() -> u64 {
    300
}

fn default_telemetry_endpoint() -> String {
    "http://localhost:4317".to_string()
}

fn default_telemetry_service_name() -> String {
    "ggen".to_string()
}

fn default_telemetry_console_output() -> bool {
    false
}

// =========================================================================
// Validate Trait Implementations
// =========================================================================

use star_toml::{Validate, Validator};

impl Validate for GgenConfig {
    fn validate(&self, v: &mut Validator) {
        v.field("project", |v| self.project.validate(v));
        if let Some(ai) = &self.ai {
            v.field("ai", |v| ai.validate(v));
        }
        if let Some(templates) = &self.templates {
            v.field("templates", |v| templates.validate(v));
        }
        if let Some(rdf) = &self.rdf {
            v.field("rdf", |v| rdf.validate(v));
        }
        if let Some(sparql) = &self.sparql {
            v.field("sparql", |v| sparql.validate(v));
        }
        if let Some(lifecycle) = &self.lifecycle {
            v.field("lifecycle", |v| lifecycle.validate(v));
        }
        if let Some(security) = &self.security {
            v.field("security", |v| security.validate(v));
        }
        if let Some(performance) = &self.performance {
            v.field("performance", |v| performance.validate(v));
        }
        if let Some(logging) = &self.logging {
            v.field("logging", |v| logging.validate(v));
        }
        if let Some(telemetry) = &self.telemetry {
            v.field("telemetry", |v| telemetry.validate(v));
        }
        if let Some(build) = &self.build {
            v.field("build", |v| build.validate(v));
        }
        if let Some(test) = &self.test {
            v.field("test", |v| test.validate(v));
        }
        if let Some(package) = &self.package {
            v.field("package", |v| package.validate(v));
        }
        if let Some(mcp) = &self.mcp {
            v.field("mcp", |v| mcp.validate(v));
        }
        if let Some(a2a) = &self.a2a {
            v.field("a2a", |v| a2a.validate(v));
        }
    }
}

impl Validate for ProjectConfig {
    fn validate(&self, v: &mut Validator) {
        v.check_non_empty("name", &self.name);
        v.check_semver("version", &self.version);
    }
}

impl Validate for AiConfig {
    fn validate(&self, v: &mut Validator) {
        let providers = ["openai", "ollama", "anthropic", "cohere", "huggingface"];
        v.check_one_of("provider", &self.provider, &providers);
        v.check_range("temperature", self.temperature, 0.0..=1.0);
        v.check_predicate(
            "max_tokens",
            self.max_tokens > 0,
            "out_of_range",
            "AI max_tokens must be greater than 0",
        );
        v.check_predicate(
            "timeout",
            self.timeout > 0,
            "out_of_range",
            "AI timeout must be greater than 0",
        );
        if let Some(prompts) = &self.prompts {
            v.field("prompts", |v| prompts.validate(v));
        }
        if let Some(validation) = &self.validation {
            v.field("validation", |v| validation.validate(v));
        }
    }
}

impl Validate for AiPrompts {
    fn validate(&self, _v: &mut Validator) {}
}

impl Validate for AiValidation {
    fn validate(&self, v: &mut Validator) {
        v.check_range("quality_threshold", self.quality_threshold, 0.0..=1.0);
    }
}

impl Validate for TemplatesConfig {
    fn validate(&self, v: &mut Validator) {
        if let Some(dir) = &self.directory {
            v.check_path("directory", dir, None);
        }
        if let Some(out_dir) = &self.output_directory {
            v.check_path("output_directory", out_dir, None);
        }
    }
}

impl Validate for RdfConfig {
    fn validate(&self, _v: &mut Validator) {}
}

impl Validate for SparqlConfig {
    fn validate(&self, _v: &mut Validator) {}
}

impl Validate for LifecycleConfig {
    fn validate(&self, _v: &mut Validator) {}
}

impl Validate for SecurityConfig {
    fn validate(&self, _v: &mut Validator) {}
}

impl Validate for PerformanceConfig {
    fn validate(&self, v: &mut Validator) {
        v.check_consistent(
            "max_workers",
            &["parallel_execution"],
            !self.parallel_execution || self.max_workers > 0,
            "out_of_range",
            "Performance max_workers must be greater than 0 when parallel_execution is enabled",
        );
        if let Some(cache_size) = &self.cache_size {
            v.check_size_format("cache_size", cache_size);
        }
    }
}

impl Validate for LoggingConfig {
    fn validate(&self, v: &mut Validator) {
        let level = self.level.to_lowercase();
        v.check_one_of(
            "level",
            &level,
            &["trace", "debug", "info", "warn", "error"],
        );
        let format = self.format.to_lowercase();
        v.check_one_of("format", &format, &["json", "text", "pretty"]);
        if let Some(file) = &self.file {
            v.check_path("file", file, None);
        }
    }
}

impl Validate for TelemetryConfig {
    fn validate(&self, _v: &mut Validator) {}
}

impl Validate for BuildConfig {
    fn validate(&self, _v: &mut Validator) {}
}

impl Validate for TestConfig {
    fn validate(&self, _v: &mut Validator) {}
}

impl Validate for PackageMetadata {
    fn validate(&self, _v: &mut Validator) {}
}

impl Validate for McpConfig {
    fn validate(&self, v: &mut Validator) {
        v.check_predicate(
            "tool_timeout_ms",
            self.tool_timeout_ms > 0,
            "out_of_range",
            "MCP tool_timeout_ms must be greater than 0",
        );
        v.check_predicate(
            "max_concurrent_requests",
            self.max_concurrent_requests > 0,
            "out_of_range",
            "MCP max_concurrent_requests must be greater than 0",
        );
        if let Some(transport) = &self.transport {
            v.field("transport", |v| transport.validate(v));
        }
        if let Some(tools) = &self.tools {
            v.field("tools", |v| tools.validate(v));
        }
        if let Some(zai) = &self.zai {
            v.field("zai", |v| zai.validate(v));
        }
        if let Some(discovery) = &self.discovery {
            v.field("discovery", |v| discovery.validate(v));
        }
    }
}

impl Validate for McpTransportConfig {
    fn validate(&self, v: &mut Validator) {
        let valid_transports = ["stdio", "http", "websocket"];
        v.check_one_of("transport_type", &self.transport_type, &valid_transports);
        if let Some(port) = self.port {
            v.check_range("port", port, 1..=65535);
        }
        if let Some(tls) = &self.tls {
            v.field("tls", |v| tls.validate(v));
        }
    }
}

impl Validate for McpTlsConfig {
    fn validate(&self, v: &mut Validator) {
        if let Some(cert_path) = &self.cert_path {
            v.check_path("cert_path", cert_path, None);
        }
        if let Some(key_path) = &self.key_path {
            v.check_path("key_path", key_path, None);
        }
        if let Some(ca_path) = &self.ca_path {
            v.check_path("ca_path", ca_path, None);
        }
    }
}

impl Validate for McpToolsConfig {
    fn validate(&self, v: &mut Validator) {
        if let Some(discovery_path) = &self.discovery_path {
            v.check_path("discovery_path", discovery_path, None);
        }
    }
}

impl Validate for McpZaiConfig {
    fn validate(&self, _v: &mut Validator) {}
}

impl Validate for McpDiscoveryConfig {
    fn validate(&self, _v: &mut Validator) {}
}

impl Validate for A2AConfig {
    fn validate(&self, v: &mut Validator) {
        if let Some(transport) = &self.transport {
            v.field("transport", |v| transport.validate(v));
        }
        if let Some(messaging) = &self.messaging {
            v.field("messaging", |v| messaging.validate(v));
        }
        if let Some(orchestration) = &self.orchestration {
            v.field("orchestration", |v| orchestration.validate(v));
        }
    }
}

impl Validate for A2ATransportConfig {
    fn validate(&self, v: &mut Validator) {
        let valid_transports = ["memory", "http", "websocket", "amqp"];
        v.check_one_of("transport_type", &self.transport_type, &valid_transports);
        if let Some(port) = self.port {
            v.check_range("port", port, 1..=65535);
        }
        if let Some(retry) = &self.retry {
            v.field("retry", |v| retry.validate(v));
        }
    }
}

impl Validate for A2ARetryConfig {
    fn validate(&self, _v: &mut Validator) {}
}

impl Validate for A2AMessagingConfig {
    fn validate(&self, v: &mut Validator) {
        if let Some(persistence_path) = &self.persistence_path {
            v.check_path("persistence_path", persistence_path, None);
        }
    }
}

impl Validate for A2AOrchestrationConfig {
    fn validate(&self, v: &mut Validator) {
        let valid_modes = ["centralized", "decentralized", "hierarchical"];
        v.check_one_of("mode", &self.mode, &valid_modes);
        if self.consensus_enabled {
            if let Some(algo) = &self.consensus_algorithm {
                let valid_algos = ["raft", "pbft", "naive"];
                v.check_one_of("consensus_algorithm", algo, &valid_algos);
            } else {
                v.check_predicate(
                    "consensus_algorithm",
                    false,
                    "missing",
                    "A2A consensus algorithm must be specified when consensus is enabled",
                );
            }
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use star_toml::Validate;

    #[test]
    fn test_ggen_config_validate_trait() {
        let mut config = GgenConfig::default();
        config.project.name = "".to_string(); // Invalid: empty
        config.project.version = "1.0".to_string(); // Invalid: bad semver

        let errs = config.check().unwrap_err();
        let error_msgs: Vec<String> = errs.errors().iter().map(|e| e.loc.to_string()).collect();
        assert!(error_msgs.contains(&"project.name".to_string()));
        assert!(error_msgs.contains(&"project.version".to_string()));

        config.project.name = "my-project".to_string();
        config.project.version = "1.0.0".to_string();
        assert!(config.check().is_ok());

        // Test AI config validation
        config.ai = Some(AiConfig {
            provider: "unknown".to_string(),
            model: "gpt-4".to_string(),
            temperature: 1.5, // Invalid: > 1.0
            max_tokens: 0,    // Invalid: must be > 0
            timeout: 0,       // Invalid: must be > 0
            prompts: None,
            validation: Some(AiValidation {
                enabled: true,
                quality_threshold: -0.5, // Invalid: < 0.0
                max_iterations: 3,
            }),
        });

        let errs = config.check().unwrap_err();
        let error_locs: Vec<String> = errs.errors().iter().map(|e| e.loc.to_string()).collect();
        assert!(error_locs.contains(&"ai.provider".to_string()));
        assert!(error_locs.contains(&"ai.temperature".to_string()));
        assert!(error_locs.contains(&"ai.max_tokens".to_string()));
        assert!(error_locs.contains(&"ai.timeout".to_string()));
        assert!(error_locs.contains(&"ai.validation.quality_threshold".to_string()));
    }

    #[test]
    fn test_ggen_config_optional_subconfigs_none() {
        let mut config = GgenConfig::default();
        config.project.name = "my-project".to_string();
        config.project.version = "1.0.0".to_string();
        // Set all optional fields to None
        config.ai = None;
        config.templates = None;
        config.rdf = None;
        config.sparql = None;
        config.lifecycle = None;
        config.security = None;
        config.performance = None;
        config.logging = None;
        config.telemetry = None;
        config.build = None;
        config.test = None;
        config.package = None;
        config.mcp = None;
        config.a2a = None;

        assert!(config.check().is_ok());
    }

    #[test]
    fn test_ggen_config_extreme_values() {
        let mut config = GgenConfig::default();
        config.project.name = "my-project".to_string();
        config.project.version = "1.0.0".to_string();

        // 1. Performance Config max_workers extreme values
        config.performance = Some(PerformanceConfig {
            parallel_execution: true,
            max_workers: 0, // Invalid: must be > 0 when parallel_execution is true
            cache_size: Some("invalid_size".to_string()), // Invalid size format
            enable_profiling: false,
            memory_limit_mb: None,
        });

        let errs = config.check().unwrap_err();
        let error_locs: Vec<String> = errs.errors().iter().map(|e| e.loc.to_string()).collect();
        assert!(error_locs.contains(&"performance.max_workers".to_string()));
        assert!(error_locs.contains(&"performance.cache_size".to_string()));

        // 2. Logging Config invalid values
        config.performance = None;
        config.logging = Some(LoggingConfig {
            level: "SUPER_FATAL".to_string(), // Invalid level
            format: "xml".to_string(),        // Invalid format
            file: None,
            rotation: None,
        });

        let errs = config.check().unwrap_err();
        let error_locs: Vec<String> = errs.errors().iter().map(|e| e.loc.to_string()).collect();
        assert!(error_locs.contains(&"logging.level".to_string()));
        assert!(error_locs.contains(&"logging.format".to_string()));

        // 3. MCP Config validation
        config.logging = None;
        config.mcp = Some(McpConfig {
            name: Some("test-mcp".to_string()),
            version: Some("1.0.0".to_string()),
            enabled: true,
            tool_timeout_ms: 0,         // Invalid: must be > 0
            max_concurrent_requests: 0, // Invalid: must be > 0
            transport: Some(McpTransportConfig {
                transport_type: "ftp".to_string(), // Invalid transport
                port: Some(0),                     // Invalid port
                host: "localhost".to_string(),
                tls: None,
                request_timeout_seconds: 30,
                ..Default::default()
            }),
            tools: None,
            zai: None,
            discovery: None,
        });

        let errs = config.check().unwrap_err();
        let error_locs: Vec<String> = errs.errors().iter().map(|e| e.loc.to_string()).collect();
        assert!(error_locs.contains(&"mcp.tool_timeout_ms".to_string()));
        assert!(error_locs.contains(&"mcp.max_concurrent_requests".to_string()));
        assert!(error_locs.contains(&"mcp.transport.transport_type".to_string()));
        assert!(error_locs.contains(&"mcp.transport.port".to_string()));

        // 4. A2A Config validation
        config.mcp = None;
        config.a2a = Some(A2AConfig {
            agent_id: Some("agent-1".to_string()),
            agent_name: Some("test-agent".to_string()),
            agent_type: Some("worker".to_string()),
            capabilities: None,
            enabled: true,
            transport: Some(A2ATransportConfig {
                transport_type: "gRPC".to_string(), // Invalid transport
                bind_address: Some("127.0.0.1".to_string()),
                port: Some(9999), // Invalid port (>65535) is handled by u16 type, let's use 0 to trigger port range validation
                timeout_ms: 5000,
                max_connections: Some(10),
                retry: None,
                ..Default::default()
            }),
            messaging: None,
            orchestration: Some(A2AOrchestrationConfig {
                mode: "dynamic".to_string(), // Invalid mode
                coordinator_address: Some("127.0.0.1".to_string()),
                heartbeat_interval_seconds: 5,
                agent_timeout_seconds: 30,
                consensus_enabled: true,
                consensus_algorithm: Some("paxos".to_string()), // Invalid consensus algo
                ..Default::default()
            }),
        });

        // Actually A2ATransportConfig's port is Option<u16>, so 99999 won't fit, let's use Some(0) to trigger range violation.
        config
            .a2a
            .as_mut()
            .unwrap()
            .transport
            .as_mut()
            .unwrap()
            .port = Some(0);

        let errs = config.check().unwrap_err();
        let error_locs: Vec<String> = errs.errors().iter().map(|e| e.loc.to_string()).collect();
        assert!(error_locs.contains(&"a2a.transport.transport_type".to_string()));
        assert!(error_locs.contains(&"a2a.transport.port".to_string()));
        assert!(error_locs.contains(&"a2a.orchestration.mode".to_string()));
        assert!(error_locs.contains(&"a2a.orchestration.consensus_algorithm".to_string()));
    }

    #[test]
    fn test_ggen_config_path_validation_gaps() {
        let mut config = GgenConfig::default();
        config.project.name = "my-project".to_string();
        config.project.version = "1.0.0".to_string();

        // Setup various traversal path strings and null bytes
        let malicious_path = "path/../../to/malicious/file".to_string();
        let backslash_path = "path\\..\\to\\malicious\\file".to_string();
        let null_byte_path = "path/with\0null/byte".to_string();

        // 1. Templates Config
        config.templates = Some(TemplatesConfig {
            directory: Some(malicious_path.clone()),
            output_directory: Some(backslash_path.clone()),
            backup_enabled: false,
            idempotent: false,
        });

        let errs = config.check().unwrap_err();
        let locs: Vec<String> = errs.errors().iter().map(|e| e.loc.to_string()).collect();
        assert!(locs.contains(&"templates.directory".to_string()));
        assert!(locs.contains(&"templates.output_directory".to_string()));

        // 2. Logging Config
        config.templates = None;
        config.logging = Some(LoggingConfig {
            level: "info".to_string(),
            format: "json".to_string(),
            file: Some(null_byte_path.clone()),
            rotation: None,
        });

        let errs = config.check().unwrap_err();
        let locs: Vec<String> = errs.errors().iter().map(|e| e.loc.to_string()).collect();
        assert!(locs.contains(&"logging.file".to_string()));

        // 3. MCP Config
        config.logging = None;
        config.mcp = Some(McpConfig {
            name: Some("test-mcp".to_string()),
            version: Some("1.0.0".to_string()),
            enabled: true,
            tool_timeout_ms: 1000,
            max_concurrent_requests: 10,
            transport: Some(McpTransportConfig {
                transport_type: "stdio".to_string(),
                port: None,
                host: "localhost".to_string(),
                tls: Some(McpTlsConfig {
                    enabled: false,
                    cert_path: Some(malicious_path.clone()),
                    key_path: Some(backslash_path.clone()),
                    ca_path: Some(null_byte_path.clone()),
                }),
                request_timeout_seconds: 30,
                ..Default::default()
            }),
            tools: Some(McpToolsConfig {
                discovery_path: Some(malicious_path.clone()),
                require_registration: false,
                validate_signatures: false,
                allowed_prefixes: None,
            }),
            zai: None,
            discovery: None,
        });

        let errs = config.check().unwrap_err();
        let locs: Vec<String> = errs.errors().iter().map(|e| e.loc.to_string()).collect();
        assert!(locs.contains(&"mcp.transport.tls.cert_path".to_string()));
        assert!(locs.contains(&"mcp.transport.tls.key_path".to_string()));
        assert!(locs.contains(&"mcp.transport.tls.ca_path".to_string()));
        assert!(locs.contains(&"mcp.tools.discovery_path".to_string()));

        // 4. A2A Config
        config.mcp = None;
        config.a2a = Some(A2AConfig {
            agent_id: Some("agent-1".to_string()),
            agent_name: Some("test-agent".to_string()),
            agent_type: Some("worker".to_string()),
            capabilities: None,
            enabled: true,
            transport: None,
            messaging: Some(A2AMessagingConfig {
                queue_size: 100,
                message_ttl_seconds: 60,
                persistence_enabled: true,
                persistence_path: Some(malicious_path.clone()),
                signing_enabled: false,
                signature_algorithm: None,
            }),
            orchestration: None,
        });

        let errs = config.check().unwrap_err();
        let locs: Vec<String> = errs.errors().iter().map(|e| e.loc.to_string()).collect();
        assert!(locs.contains(&"a2a.messaging.persistence_path".to_string()));
    }
}
