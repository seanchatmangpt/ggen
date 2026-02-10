//! Schema definitions for ggen.toml configuration
//!
//! This module defines the complete structure of ggen.toml files
//! using serde-compatible Rust structs.

use serde::{Deserialize, Serialize};
use std::collections::HashMap;

/// Root configuration structure for ggen.toml
#[derive(Debug, Clone, Serialize, Deserialize, PartialEq)]
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
#[derive(Debug, Clone, Serialize, Deserialize, PartialEq, Eq)]
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
#[derive(Debug, Clone, Serialize, Deserialize, PartialEq, Eq)]
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
#[derive(Debug, Clone, Serialize, Deserialize, PartialEq, Eq)]
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
    std::thread::available_parallelism()
        .map(|n| n.get() as u32)
        .unwrap_or(4)
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
            features: None,
            env: None,
            build: None,
            test: None,
            package: None,
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
