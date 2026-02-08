//! MCP (Model Context Protocol) Configuration Module
//!
//! This module provides configuration management for MCP servers and A2A protocol
//! integration with the following features:
//!
//! - Multi-layered configuration priority: CLI args > env vars > project config > user config > system config > defaults
//! - JSON-based MCP server configuration (.mcp.json)
//! - TOML-based A2A configuration (a2a.toml)
//! - Configuration validation with helpful error messages
//! - Environment variable support (GGEN_MCP_*, GGEN_A2A_*)

use ggen_utils::error::Error as GgenError;
use serde::{Deserialize, Serialize};
use std::collections::HashMap;
use std::env;
use std::fs;
use std::path::{Path, PathBuf};
use std::time::{Duration, SystemTime, UNIX_EPOCH};

// ============================================================================
// Configuration File Paths
// ============================================================================

/// Default project-level MCP configuration file
pub const PROJECT_MCP_CONFIG: &str = ".mcp.json";

/// Default user-level MCP configuration directory
pub const USER_MCP_CONFIG_DIR: &str = ".ggen/mcp";

/// Default user-level MCP configuration file
pub const USER_MCP_CONFIG: &str = ".ggen/mcp/config.json";

/// Default system-level MCP configuration file
pub const SYSTEM_MCP_CONFIG: &str = "/etc/ggen/mcp.json";

/// Default project-level A2A configuration file
pub const PROJECT_A2A_CONFIG: &str = "a2a.toml";

/// Default user-level A2A configuration file
pub const USER_A2A_CONFIG: &str = ".ggen/a2a.toml";

/// Default system-level A2A configuration file
pub const SYSTEM_A2A_CONFIG: &str = "/etc/ggen/a2a.toml";

/// PID file for running MCP server
pub const MCP_SERVER_PID_FILE: &str = ".ggen/mcp/server.pid";

/// Lock file for MCP server operations
pub const MCP_SERVER_LOCK_FILE: &str = ".ggen/mcp/server.lock";

// ============================================================================
// MCP Server Configuration Types
// ============================================================================

/// Complete MCP configuration file structure
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct McpConfigFile {
    /// MCP server configurations keyed by server name
    #[serde(default)]
    pub mcp_servers: HashMap<String, McpServerConfig>,
    /// Configuration metadata
    #[serde(default)]
    pub metadata: McpMetadata,
    /// Optional description
    #[serde(default)]
    pub description: Option<String>,
    /// Configuration version
    #[serde(default = "default_mcp_version")]
    pub version: String,
}

impl Default for McpConfigFile {
    fn default() -> Self {
        Self {
            mcp_servers: HashMap::new(),
            metadata: McpMetadata::default(),
            description: Some(
                "MCP (Model Context Protocol) servers for enhanced Claude Code capabilities"
                    .to_string(),
            ),
            version: default_mcp_version(),
        }
    }
}

fn default_mcp_version() -> String {
    "1.0.0".to_string()
}

/// Metadata for MCP configuration
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct McpMetadata {
    /// Project name
    #[serde(default)]
    pub project: Option<String>,
    /// Project version
    #[serde(default)]
    pub version: Option<String>,
    /// Purpose description
    #[serde(default)]
    pub purpose: Option<String>,
    /// Configuration creation/update timestamp
    #[serde(default)]
    pub updated_at: Option<String>,
}

impl Default for McpMetadata {
    fn default() -> Self {
        Self {
            project: None,
            version: None,
            purpose: None,
            updated_at: None,
        }
    }
}

/// Configuration for a single MCP server
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct McpServerConfig {
    /// Command to execute (e.g., "npx", "uvx", "python")
    pub command: String,
    /// Arguments to pass to the command
    #[serde(default)]
    pub args: Vec<String>,
    /// Environment variables for the server process
    #[serde(default)]
    pub env: HashMap<String, String>,
    /// Server working directory
    #[serde(default)]
    pub cwd: Option<String>,
    /// Server timeout in seconds
    #[serde(default = "default_server_timeout")]
    pub timeout: u64,
    /// Whether the server is enabled
    #[serde(default = "default_server_enabled")]
    pub enabled: bool,
    /// Maximum restart attempts
    #[serde(default = "default_max_restarts")]
    pub max_restarts: u32,
    /// Server type (std, ssh, or custom)
    #[serde(default)]
    pub server_type: Option<String>,
}

fn default_server_timeout() -> u64 {
    30
}

fn default_server_enabled() -> bool {
    true
}

fn default_max_restarts() -> u32 {
    3
}

impl McpServerConfig {
    /// Create a new MCP server configuration
    pub fn new(command: impl Into<String>) -> Self {
        Self {
            command: command.into(),
            args: Vec::new(),
            env: HashMap::new(),
            cwd: None,
            timeout: default_server_timeout(),
            enabled: default_server_enabled(),
            max_restarts: default_max_restarts(),
            server_type: None,
        }
    }

    /// Add an argument to the command
    pub fn with_arg(mut self, arg: impl Into<String>) -> Self {
        self.args.push(arg.into());
        self
    }

    /// Add multiple arguments
    pub fn with_args(mut self, args: Vec<String>) -> Self {
        self.args.extend(args);
        self
    }

    /// Add an environment variable
    pub fn with_env(mut self, key: impl Into<String>, value: impl Into<String>) -> Self {
        self.env.insert(key.into(), value.into());
        self
    }

    /// Set working directory
    pub fn with_cwd(mut self, cwd: impl Into<String>) -> Self {
        self.cwd = Some(cwd.into());
        self
    }

    /// Set timeout
    pub fn with_timeout(mut self, timeout: Duration) -> Self {
        self.timeout = timeout.as_secs();
        self
    }

    /// Disable the server
    pub fn disabled(mut self) -> Self {
        self.enabled = false;
        self
    }

    /// Validate the configuration
    pub fn validate(&self) -> Result<(), McpValidationError> {
        if self.command.is_empty() {
            return Err(McpValidationError::EmptyCommand);
        }

        if self.timeout == 0 {
            return Err(McpValidationError::InvalidTimeout(
                "Timeout must be greater than 0".to_string(),
            ));
        }

        // Check for suspicious commands
        let dangerous_commands = ["rm -rf", "mkfs", "format", "del /f"];
        let command_lower = self.command.to_lowercase();
        for dangerous in dangerous_commands {
            if command_lower.contains(dangerous) {
                return Err(McpValidationError::DangerousCommand(format!(
                    "Command contains potentially dangerous pattern: {}",
                    dangerous
                )));
            }
        }

        Ok(())
    }
}

// ============================================================================
// A2A Configuration Types
// ============================================================================

/// A2A (Agent-to-Agent) protocol configuration
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct A2aConfig {
    /// A2A server configuration
    #[serde(default)]
    pub server: A2aServerConfig,
    /// Agent configurations
    #[serde(default)]
    pub agents: HashMap<String, A2aAgentConfig>,
    /// Workflow configurations
    #[serde(default)]
    pub workflows: HashMap<String, A2aWorkflowConfig>,
    /// Configuration metadata
    #[serde(default)]
    pub metadata: A2aMetadata,
}

impl Default for A2aConfig {
    fn default() -> Self {
        Self {
            server: A2aServerConfig::default(),
            agents: HashMap::new(),
            workflows: HashMap::new(),
            metadata: A2aMetadata::default(),
        }
    }
}

/// A2A server configuration
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct A2aServerConfig {
    /// Server host address
    #[serde(default = "default_a2a_host")]
    pub host: String,
    /// Server port
    #[serde(default = "default_a2a_port")]
    pub port: u16,
    /// Enable TLS
    #[serde(default)]
    pub tls_enabled: bool,
    /// TLS certificate path
    #[serde(default)]
    pub tls_cert_path: Option<String>,
    /// TLS key path
    #[serde(default)]
    pub tls_key_path: Option<String>,
    /// Request timeout in seconds
    #[serde(default = "default_a2a_timeout")]
    pub timeout: u64,
    /// Maximum concurrent connections
    #[serde(default = "default_max_connections")]
    pub max_connections: usize,
}

fn default_a2a_host() -> String {
    "127.0.0.1".to_string()
}

fn default_a2a_port() -> u16 {
    8080
}

fn default_a2a_timeout() -> u64 {
    30
}

fn default_max_connections() -> usize {
    100
}

impl Default for A2aServerConfig {
    fn default() -> Self {
        Self {
            host: default_a2a_host(),
            port: default_a2a_port(),
            tls_enabled: false,
            tls_cert_path: None,
            tls_key_path: None,
            timeout: default_a2a_timeout(),
            max_connections: default_max_connections(),
        }
    }
}

/// A2A agent configuration
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct A2aAgentConfig {
    /// Agent type
    pub agent_type: String,
    /// Agent name
    pub name: String,
    /// Agent description
    #[serde(default)]
    pub description: Option<String>,
    /// Agent enabled
    #[serde(default = "default_agent_enabled")]
    pub enabled: bool,
    /// Agent-specific configuration
    #[serde(default)]
    pub config: HashMap<String, toml::Value>,
}

fn default_agent_enabled() -> bool {
    true
}

/// A2A workflow configuration
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct A2aWorkflowConfig {
    /// Workflow specification file
    pub spec_file: String,
    /// Workflow name
    pub name: String,
    /// Auto-start on initialization
    #[serde(default)]
    pub auto_start: bool,
}

/// A2A metadata
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct A2aMetadata {
    /// Configuration version
    #[serde(default = "default_a2a_config_version")]
    pub version: String,
    /// Environment name
    #[serde(default)]
    pub environment: Option<String>,
    /// Last updated timestamp
    #[serde(default)]
    pub updated_at: Option<String>,
}

fn default_a2a_config_version() -> String {
    "1.0.0".to_string()
}

impl Default for A2aMetadata {
    fn default() -> Self {
        Self {
            version: default_a2a_config_version(),
            environment: None,
            updated_at: None,
        }
    }
}

impl A2aConfig {
    /// Create a new A2A configuration
    pub fn new() -> Self {
        Self::default()
    }

    /// Validate the A2A configuration
    pub fn validate(&self) -> Result<(), A2aValidationError> {
        // Validate server configuration
        if self.server.port == 0 {
            return Err(A2aValidationError::InvalidPort(
                "Port cannot be zero".to_string(),
            ));
        }

        if self.server.timeout == 0 {
            return Err(A2aValidationError::InvalidTimeout(
                "Timeout must be greater than 0".to_string(),
            ));
        }

        // Validate TLS configuration consistency
        if self.server.tls_enabled {
            if self.server.tls_cert_path.is_none() || self.server.tls_key_path.is_none() {
                return Err(A2aValidationError::TlsMisconfigured(
                    "TLS is enabled but certificate or key path is missing".to_string(),
                ));
            }
        }

        Ok(())
    }

    /// Get the server URL
    pub fn server_url(&self) -> String {
        let scheme = if self.server.tls_enabled {
            "https"
        } else {
            "http"
        };
        format!("{}://{}:{}", scheme, self.server.host, self.server.port)
    }
}

// ============================================================================
// Validation Error Types
// ============================================================================

/// MCP configuration validation error
#[derive(Debug, Clone, thiserror::Error)]
pub enum McpValidationError {
    #[error("Empty command in server configuration")]
    EmptyCommand,

    #[error("Invalid timeout: {0}")]
    InvalidTimeout(String),

    #[error("Dangerous command detected: {0}")]
    DangerousCommand(String),

    #[error("Server not found: {0}")]
    ServerNotFound(String),

    #[error("Configuration file not found: {0}")]
    ConfigNotFound(String),

    #[error("Invalid JSON in configuration: {0}")]
    InvalidJson(String),

    #[error("File IO error: {0}")]
    FileIo(String),
}

/// A2A configuration validation error
#[derive(Debug, Clone, thiserror::Error)]
pub enum A2aValidationError {
    #[error("Invalid port: {0}")]
    InvalidPort(String),

    #[error("Invalid timeout: {0}")]
    InvalidTimeout(String),

    #[error("TLS misconfigured: {0}")]
    TlsMisconfigured(String),

    #[error("Agent not found: {0}")]
    AgentNotFound(String),

    #[error("Configuration file not found: {0}")]
    ConfigNotFound(String),

    #[error("Invalid TOML in configuration: {0}")]
    InvalidToml(String),

    #[error("File IO error: {0}")]
    FileIo(String),
}

// ============================================================================
// Configuration Loader with Priority Resolution
// ============================================================================

/// Configuration priority levels (highest to lowest)
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
pub enum ConfigPriority {
    /// CLI arguments (highest priority)
    CliArgs,
    /// Environment variables
    EnvVars,
    /// Project-level configuration
    Project,
    /// User-level configuration
    User,
    /// System-level configuration
    System,
    /// Default values (lowest priority)
    Defaults,
}

/// Configuration resolution result
#[derive(Debug, Clone)]
pub struct ResolvedConfig {
    /// The effective MCP configuration
    pub mcp: Option<McpConfigFile>,
    /// The effective A2A configuration
    pub a2a: Option<A2aConfig>,
    /// Sources of each configuration value
    pub sources: HashMap<String, ConfigPriority>,
}

/// Load and resolve configuration from all sources
pub fn load_config(
    project_dir: Option<&Path>, cli_mcp_file: Option<&Path>, cli_a2a_file: Option<&Path>,
) -> Result<ResolvedConfig, GgenError> {
    let mut sources = HashMap::new();
    let mut mcp_config: Option<McpConfigFile> = None;
    let mut a2a_config: Option<A2aConfig> = None;

    // Start with environment variables
    let env_mcp_config = load_mcp_from_env()?;
    if env_mcp_config.is_some() {
        sources.insert("mcp".to_string(), ConfigPriority::EnvVars);
        mcp_config = env_mcp_config;
    }

    let env_a2a_config = load_a2a_from_env()?;
    if env_a2a_config.is_some() {
        sources.insert("a2a".to_string(), ConfigPriority::EnvVars);
        a2a_config = env_a2a_config;
    }

    // Check project-level configuration
    if let Some(project_dir) = project_dir {
        let project_mcp_path = project_dir.join(PROJECT_MCP_CONFIG);
        if project_mcp_path.exists() {
            let config = load_mcp_from_file(&project_mcp_path)?;
            if sources
                .get("mcp")
                .map_or(true, |p| *p < ConfigPriority::Project)
            {
                sources.insert("mcp".to_string(), ConfigPriority::Project);
                mcp_config = Some(config);
            }
        }

        let project_a2a_path = project_dir.join(PROJECT_A2A_CONFIG);
        if project_a2a_path.exists() {
            let config = load_a2a_from_file(&project_a2a_path)?;
            if sources
                .get("a2a")
                .map_or(true, |p| *p < ConfigPriority::Project)
            {
                sources.insert("a2a".to_string(), ConfigPriority::Project);
                a2a_config = Some(config);
            }
        }
    }

    // Check user-level configuration
    let user_mcp_path = dirs::home_dir().map(|p| p.join(USER_MCP_CONFIG));
    if let Some(ref path) = user_mcp_path {
        if path.exists() {
            let config = load_mcp_from_file(path)?;
            if sources
                .get("mcp")
                .map_or(true, |p| *p < ConfigPriority::User)
            {
                sources.insert("mcp".to_string(), ConfigPriority::User);
                mcp_config = Some(config);
            }
        }
    }

    let user_a2a_path = dirs::home_dir().map(|p| p.join(USER_A2A_CONFIG));
    if let Some(ref path) = user_a2a_path {
        if path.exists() {
            let config = load_a2a_from_file(path)?;
            if sources
                .get("a2a")
                .map_or(true, |p| *p < ConfigPriority::User)
            {
                sources.insert("a2a".to_string(), ConfigPriority::User);
                a2a_config = Some(config);
            }
        }
    }

    // Check system-level configuration
    let system_mcp_path = PathBuf::from(SYSTEM_MCP_CONFIG);
    if system_mcp_path.exists() {
        let config = load_mcp_from_file(&system_mcp_path)?;
        if sources
            .get("mcp")
            .map_or(true, |p| *p < ConfigPriority::System)
        {
            sources.insert("mcp".to_string(), ConfigPriority::System);
            mcp_config = Some(config);
        }
    }

    let system_a2a_path = PathBuf::from(SYSTEM_A2A_CONFIG);
    if system_a2a_path.exists() {
        let config = load_a2a_from_file(&system_a2a_path)?;
        if sources
            .get("a2a")
            .map_or(true, |p| *p < ConfigPriority::System)
        {
            sources.insert("a2a".to_string(), ConfigPriority::System);
            a2a_config = Some(config);
        }
    }

    // Apply CLI-specified config files (highest priority after CLI args)
    if let Some(cli_file) = cli_mcp_file {
        let config = load_mcp_from_file(cli_file)?;
        sources.insert("mcp".to_string(), ConfigPriority::CliArgs);
        mcp_config = Some(config);
    }

    if let Some(cli_file) = cli_a2a_file {
        let config = load_a2a_from_file(cli_file)?;
        sources.insert("a2a".to_string(), ConfigPriority::CliArgs);
        a2a_config = Some(config);
    }

    Ok(ResolvedConfig {
        mcp: mcp_config,
        a2a: a2a_config,
        sources,
    })
}

/// Load MCP configuration from a file
pub fn load_mcp_from_file(path: &Path) -> Result<McpConfigFile, GgenError> {
    let content = fs::read_to_string(path).map_err(|e| {
        GgenError::invalid_input(&format!("Failed to read MCP config from {:?}: {}", path, e))
    })?;

    serde_json::from_str(&content).map_err(|e| {
        GgenError::invalid_input(&format!(
            "Failed to parse MCP config from {:?}: {}\nSuggestion: Check JSON syntax and structure",
            path, e
        ))
    })
}

/// Load A2A configuration from a file
pub fn load_a2a_from_file(path: &Path) -> Result<A2aConfig, GgenError> {
    let content = fs::read_to_string(path).map_err(|e| {
        GgenError::invalid_input(&format!("Failed to read A2A config from {:?}: {}", path, e))
    })?;

    toml::from_str(&content).map_err(|e| {
        GgenError::invalid_input(&format!(
            "Failed to parse A2A config from {:?}: {}\nSuggestion: Check TOML syntax",
            path, e
        ))
    })
}

/// Load MCP configuration from environment variables
pub fn load_mcp_from_env() -> Result<Option<McpConfigFile>, GgenError> {
    let config_file = env::var("GGEN_MCP_CONFIG").ok();
    if let Some(path) = config_file {
        let path_buf = PathBuf::from(&path);
        if path_buf.exists() {
            return load_mcp_from_file(&path_buf).map(Some);
        }
    }

    // Check for individual server configs from env
    // Format: GGEN_MCP_SERVER_<name>_command, GGEN_MCP_SERVER_<name>_args
    let mut servers = HashMap::new();

    for (key, value) in env::vars() {
        if key.starts_with("GGEN_MCP_SERVER_") && key.ends_with("_COMMAND") {
            let server_name = key
                .strip_prefix("GGEN_MCP_SERVER_")
                .and_then(|s| s.strip_suffix("_COMMAND"))
                .map(|s| s.to_lowercase());

            if let Some(name) = server_name {
                let mut server = McpServerConfig::new(&value);

                // Look for args
                let args_key = format!("GGEN_MCP_SERVER_{}_ARGS", name.to_uppercase());
                if let Ok(args_str) = env::var(&args_key) {
                    server.args = args_str.split_whitespace().map(|s| s.to_string()).collect();
                }

                // Look for timeout
                let timeout_key = format!("GGEN_MCP_SERVER_{}_TIMEOUT", name.to_uppercase());
                if let Ok(timeout_str) = env::var(&timeout_key) {
                    if let Ok(secs) = timeout_str.parse::<u64>() {
                        server.timeout = secs;
                    }
                }

                servers.insert(name, server);
            }
        }
    }

    if servers.is_empty() {
        Ok(None)
    } else {
        Ok(Some(McpConfigFile {
            mcp_servers: servers,
            ..Default::default()
        }))
    }
}

/// Load A2A configuration from environment variables
pub fn load_a2a_from_env() -> Result<Option<A2aConfig>, GgenError> {
    let config_file = env::var("GGEN_A2A_CONFIG").ok();
    if let Some(path) = config_file {
        let path_buf = PathBuf::from(&path);
        if path_buf.exists() {
            return load_a2a_from_file(&path_buf).map(Some);
        }
    }

    // Build A2A config from individual env vars
    let mut config = A2aConfig::new();

    if let Ok(host) = env::var("GGEN_A2A_HOST") {
        config.server.host = host;
    }

    if let Ok(port) = env::var("GGEN_A2A_PORT") {
        if let Ok(p) = port.parse::<u16>() {
            config.server.port = p;
        }
    }

    if let Ok(timeout) = env::var("GGEN_A2A_TIMEOUT") {
        if let Ok(t) = timeout.parse::<u64>() {
            config.server.timeout = t;
        }
    }

    if env::var("GGEN_A2A_TLS_ENABLED").is_ok() {
        config.server.tls_enabled = true;
    }

    if let Ok(cert_path) = env::var("GGEN_A2A_TLS_CERT") {
        config.server.tls_cert_path = Some(cert_path);
    }

    if let Ok(key_path) = env::var("GGEN_A2A_TLS_KEY") {
        config.server.tls_key_path = Some(key_path);
    }

    // Return None if no env vars were set
    let has_custom_config = config.server.host != default_a2a_host()
        || config.server.port != default_a2a_port()
        || config.server.timeout != default_a2a_timeout()
        || config.server.tls_enabled;

    Ok(if has_custom_config {
        Some(config)
    } else {
        None
    })
}

// ============================================================================
// Configuration File Operations
// ============================================================================

/// Initialize a new MCP configuration file
pub fn init_mcp_config(path: &Path, include_examples: bool) -> Result<McpConfigFile, GgenError> {
    let mut config = McpConfigFile::default();

    if include_examples {
        // Add example server configurations
        config.mcp_servers.insert(
            "claude-code-guide".to_string(),
            McpServerConfig::new("npx")
                .with_args(vec!["@anthropic-ai/claude-code-guide".to_string()]),
        );

        config.mcp_servers.insert(
            "git".to_string(),
            McpServerConfig::new("git").with_args(vec!["mcp-server".to_string()]),
        );

        config.mcp_servers.insert(
            "bash".to_string(),
            McpServerConfig::new("bash")
                .with_args(vec![
                    "--init-file".to_string(),
                    ".claude/helpers/bash-init.sh".to_string(),
                ])
                .with_env("CARGO_TERM_COLOR".to_string(), "always".to_string()),
        );
    }

    // Update metadata
    config.metadata.updated_at = Some(timestamp_now());
    if let Some(project_name) = path
        .parent()
        .and_then(|p| p.file_name())
        .and_then(|n| n.to_str())
    {
        config.metadata.project = Some(project_name.to_string());
    }

    // Write to file
    write_mcp_config(path, &config)?;

    Ok(config)
}

/// Initialize a new A2A configuration file
pub fn init_a2a_config(path: &Path) -> Result<A2aConfig, GgenError> {
    let config = A2aConfig::default();

    // Write to file
    write_a2a_config(path, &config)?;

    Ok(config)
}

/// Write MCP configuration to file
pub fn write_mcp_config(path: &Path, config: &McpConfigFile) -> Result<(), GgenError> {
    // Create parent directory if needed
    if let Some(parent) = path.parent() {
        fs::create_dir_all(parent).map_err(|e| {
            GgenError::invalid_input(&format!("Failed to create directory {:?}: {}", parent, e))
        })?;
    }

    let content = serde_json::to_string_pretty(config)
        .map_err(|e| GgenError::invalid_input(&format!("Failed to serialize MCP config: {}", e)))?;

    fs::write(path, content).map_err(|e| {
        GgenError::invalid_input(&format!("Failed to write MCP config to {:?}: {}", path, e))
    })?;

    Ok(())
}

/// Write A2A configuration to file
pub fn write_a2a_config(path: &Path, config: &A2aConfig) -> Result<(), GgenError> {
    // Create parent directory if needed
    if let Some(parent) = path.parent() {
        fs::create_dir_all(parent).map_err(|e| {
            GgenError::invalid_input(&format!("Failed to create directory {:?}: {}", parent, e))
        })?;
    }

    let content = toml::to_string_pretty(config)
        .map_err(|e| GgenError::invalid_input(&format!("Failed to serialize A2A config: {}", e)))?;

    fs::write(path, content).map_err(|e| {
        GgenError::invalid_input(&format!("Failed to write A2A config to {:?}: {}", path, e))
    })?;

    Ok(())
}

/// Validate MCP configuration
pub fn validate_mcp_config(config: &McpConfigFile) -> Result<Vec<ValidationResult>, GgenError> {
    let mut results = Vec::new();

    // Validate each server configuration
    for (name, server) in &config.mcp_servers {
        match server.validate() {
            Ok(()) => {
                results.push(ValidationResult {
                    server_name: name.clone(),
                    is_valid: true,
                    errors: Vec::new(),
                    warnings: Vec::new(),
                });
            }
            Err(e) => {
                results.push(ValidationResult {
                    server_name: name.clone(),
                    is_valid: false,
                    errors: vec![e.to_string()],
                    warnings: Vec::new(),
                });
            }
        }
    }

    // Check for required servers
    if config.mcp_servers.is_empty() {
        results.push(ValidationResult {
            server_name: "_config".to_string(),
            is_valid: false,
            errors: vec!["No MCP servers configured".to_string()],
            warnings: vec!["Consider adding at least one MCP server".to_string()],
        });
    }

    Ok(results)
}

/// Validation result for a single server or the entire configuration
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct ValidationResult {
    /// Server name being validated
    pub server_name: String,
    /// Whether the configuration is valid
    pub is_valid: bool,
    /// List of errors found
    pub errors: Vec<String>,
    /// List of warnings (non-critical issues)
    pub warnings: Vec<String>,
}

// ============================================================================
// Server Management
// ============================================================================

/// Server status information
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct ServerStatus {
    /// Whether the server is running
    pub is_running: bool,
    /// Server process ID (if running)
    pub pid: Option<u32>,
    /// Server uptime in seconds (if running)
    pub uptime_secs: Option<u64>,
    /// Server configuration file path
    pub config_file: Option<String>,
    /// Server address
    pub address: Option<String>,
    /// Last start time
    pub last_start_time: Option<String>,
}

/// Get the status of the MCP server
pub fn get_server_status(project_dir: Option<&Path>) -> Result<ServerStatus, GgenError> {
    let pid_file = project_dir
        .unwrap_or_else(|| Path::new("."))
        .join(MCP_SERVER_PID_FILE);

    if !pid_file.exists() {
        return Ok(ServerStatus {
            is_running: false,
            pid: None,
            uptime_secs: None,
            config_file: None,
            address: None,
            last_start_time: None,
        });
    }

    let pid_str = fs::read_to_string(&pid_file).map_err(|e| {
        GgenError::invalid_input(&format!("Failed to read PID file {:?}: {}", pid_file, e))
    })?;

    let pid: u32 = pid_str
        .trim()
        .parse()
        .map_err(|_| GgenError::invalid_input(&format!("Invalid PID in file: {}", pid_str)))?;

    // Check if process is running
    let is_running = is_process_running(pid);

    if !is_running {
        // Clean up stale PID file
        let _ = fs::remove_file(&pid_file);
        return Ok(ServerStatus {
            is_running: false,
            pid: None,
            uptime_secs: None,
            config_file: None,
            address: None,
            last_start_time: None,
        });
    }

    // Get process start time
    let uptime_secs = get_process_uptime(pid).ok();
    let last_start = uptime_secs.map(|u| {
        let now = SystemTime::now()
            .duration_since(UNIX_EPOCH)
            .unwrap()
            .as_secs();
        format_timestamp(now - u)
    });

    Ok(ServerStatus {
        is_running: true,
        pid: Some(pid),
        uptime_secs,
        config_file: pid_file.to_str().map(|s| s.to_string()),
        address: Some("127.0.0.1:0".to_string()), // TODO: Get actual address
        last_start_time: last_start,
    })
}

/// Check if a process is running
#[cfg(unix)]
fn is_process_running(pid: u32) -> bool {
    use std::process::Command;
    Command::new("kill")
        .arg("-0")
        .arg(pid.to_string())
        .output()
        .map(|o| o.status.success())
        .unwrap_or(false)
}

/// Check if a process is running
#[cfg(windows)]
fn is_process_running(pid: u32) -> bool {
    use std::process::Command;
    Command::new("tasklist")
        .args(&["/FI", &format!("PID eq {}", pid)])
        .output()
        .map(|o| {
            let output = String::from_utf8_lossy(&o.stdout);
            output.contains(&pid.to_string())
        })
        .unwrap_or(false)
}

/// Get process uptime in seconds
#[cfg(unix)]
fn get_process_uptime(pid: u32) -> Result<u64, GgenError> {
    use std::process::Command;
    let output = Command::new("ps")
        .args(&["-o", "etime=", "-p", &pid.to_string()])
        .output()
        .map_err(|e| GgenError::invalid_input(&format!("Failed to query process: {}", e)))?;

    let elapsed = String::from_utf8_lossy(&output.stdout).trim().to_string();

    // Parse elapsed time format (MM:SS or HH:MM:SS or DD-HH:MM:SS)
    let parts: Vec<&str> = elapsed.split(':').collect();
    let seconds = match parts.len() {
        2 => {
            // MM:SS
            let mins: u64 = parts[0].parse().unwrap_or(0);
            let secs: u64 = parts[1].parse().unwrap_or(0);
            mins * 60 + secs
        }
        3 => {
            // HH:MM:SS
            let hours: u64 = parts[0].parse().unwrap_or(0);
            let mins: u64 = parts[1].parse().unwrap_or(0);
            let secs: u64 = parts[2].parse().unwrap_or(0);
            hours * 3600 + mins * 60 + secs
        }
        _ => 0,
    };

    Ok(seconds)
}

#[cfg(windows)]
fn get_process_uptime(_pid: u32) -> Result<u64, GgenError> {
    // Windows implementation would use WMI or other methods
    // For now, return error
    Err(GgenError::feature_not_enabled(
        "Process uptime not available on Windows",
        "",
    ))
}

/// Stop a running MCP server
pub fn stop_server(project_dir: Option<&Path>, force: bool) -> Result<bool, GgenError> {
    let status = get_server_status(project_dir)?;

    if !status.is_running {
        return Ok(false);
    }

    let pid = status
        .pid
        .ok_or_else(|| GgenError::invalid_input("Server is running but PID is unknown"))?;

    // Terminate the process
    terminate_process(pid, force)?;

    // Clean up PID file
    let pid_file = project_dir
        .unwrap_or_else(|| Path::new("."))
        .join(MCP_SERVER_PID_FILE);
    let _ = fs::remove_file(&pid_file);

    Ok(true)
}

/// Terminate a process
#[cfg(unix)]
fn terminate_process(pid: u32, force: bool) -> Result<(), GgenError> {
    use std::process::Command;
    let signal = if force { "9" } else { "15" }; // SIGKILL or SIGTERM

    let output = Command::new("kill")
        .arg(&format!("-{}", signal))
        .arg(pid.to_string())
        .output()
        .map_err(|e| GgenError::invalid_input(&format!("Failed to terminate process: {}", e)))?;

    if !output.status.success() {
        return Err(GgenError::invalid_input(&format!(
            "Failed to stop process {}: {}",
            pid,
            String::from_utf8_lossy(&output.stderr)
        )));
    }

    Ok(())
}

#[cfg(windows)]
fn terminate_process(pid: u32, _force: bool) -> Result<(), GgenError> {
    use std::process::Command;
    let output = Command::new("taskkill")
        .args(&["/PID", &pid.to_string(), "/F"])
        .output()
        .map_err(|e| GgenError::invalid_input(&format!("Failed to terminate process: {}", e)))?;

    if !output.status.success() {
        return Err(GgenError::invalid_input(&format!(
            "Failed to stop process {}",
            pid
        )));
    }

    Ok(())
}

/// Write server PID file
pub fn write_pid_file(project_dir: Option<&Path>, pid: u32) -> Result<(), GgenError> {
    let pid_file = project_dir
        .unwrap_or_else(|| Path::new("."))
        .join(MCP_SERVER_PID_FILE);

    if let Some(parent) = pid_file.parent() {
        fs::create_dir_all(parent).map_err(|e| {
            GgenError::invalid_input(&format!("Failed to create directory {:?}: {}", parent, e))
        })?;
    }

    fs::write(&pid_file, pid.to_string()).map_err(|e| {
        GgenError::invalid_input(&format!("Failed to write PID file {:?}: {}", pid_file, e))
    })?;

    Ok(())
}

// ============================================================================
// Utility Functions
// ============================================================================

/// Get current timestamp as ISO 8601 string
fn timestamp_now() -> String {
    format_timestamp(
        SystemTime::now()
            .duration_since(UNIX_EPOCH)
            .unwrap()
            .as_secs(),
    )
}

/// Format a Unix timestamp as ISO 8601 string
fn format_timestamp(secs: u64) -> String {
    use chrono::{DateTime, Utc};
    let dt = DateTime::<Utc>::from_timestamp(secs as i64, 0).unwrap();
    dt.to_rfc3339_opts(chrono::SecondsFormat::Secs, true)
}

/// Get a friendly description of a configuration priority level
impl std::fmt::Display for ConfigPriority {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            ConfigPriority::CliArgs => write!(f, "CLI arguments"),
            ConfigPriority::EnvVars => write!(f, "Environment variables"),
            ConfigPriority::Project => write!(f, "Project configuration"),
            ConfigPriority::User => write!(f, "User configuration"),
            ConfigPriority::System => write!(f, "System configuration"),
            ConfigPriority::Defaults => write!(f, "Default values"),
        }
    }
}

// ============================================================================
// Tests
// ============================================================================

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_mcp_server_config_builder() {
        let config = McpServerConfig::new("test-command")
            .with_arg("--verbose")
            .with_arg("--output")
            .with_env("TEST_VAR", "test_value")
            .with_timeout(Duration::from_secs(60))
            .with_cwd("/tmp");

        assert_eq!(config.command, "test-command");
        assert_eq!(config.args.len(), 2);
        assert_eq!(config.env.get("TEST_VAR"), Some(&"test_value".to_string()));
        assert_eq!(config.timeout, 60);
        assert_eq!(config.cwd, Some("/tmp".to_string()));
    }

    #[test]
    fn test_mcp_server_validation() {
        // Valid config
        let config = McpServerConfig::new("npx")
            .with_args(vec!["@anthropic-ai/claude-code-guide".to_string()]);
        assert!(config.validate().is_ok());

        // Empty command
        let invalid = McpServerConfig::new("");
        assert!(invalid.validate().is_err());

        // Zero timeout
        let invalid_timeout = McpServerConfig::new("test").with_timeout(Duration::from_secs(0));
        assert!(invalid_timeout.validate().is_err());
    }

    #[test]
    fn test_a2a_config_default() {
        let config = A2aConfig::default();
        assert_eq!(config.server.host, "127.0.0.1");
        assert_eq!(config.server.port, 8080);
        assert_eq!(config.server.timeout, 30);
        assert!(!config.server.tls_enabled);
    }

    #[test]
    fn test_a2a_config_validation() {
        let config = A2aConfig::default();
        assert!(config.validate().is_ok());

        // Invalid port
        let mut invalid = config.clone();
        invalid.server.port = 0;
        assert!(invalid.validate().is_err());

        // TLS misconfiguration
        let mut tls_invalid = config.clone();
        tls_invalid.server.tls_enabled = true;
        assert!(tls_invalid.validate().is_err());
    }

    #[test]
    fn test_a2a_server_url() {
        let config = A2aConfig::default();
        assert_eq!(config.server_url(), "http://127.0.0.1:8080");

        let mut tls_config = A2aConfig::default();
        tls_config.server.tls_enabled = true;
        tls_config.server.host = "example.com".to_string();
        tls_config.server.port = 8443;
        assert_eq!(tls_config.server_url(), "https://example.com:8443");
    }

    #[test]
    fn test_mcp_config_file_default() {
        let config = McpConfigFile::default();
        assert!(config.mcp_servers.is_empty());
        assert_eq!(config.version, "1.0.0");
        assert!(config.description.is_some());
    }

    #[test]
    fn test_config_priority_display() {
        assert_eq!(format!("{}", ConfigPriority::CliArgs), "CLI arguments");
        assert_eq!(
            format!("{}", ConfigPriority::EnvVars),
            "Environment variables"
        );
        assert_eq!(
            format!("{}", ConfigPriority::Project),
            "Project configuration"
        );
    }

    #[test]
    fn test_validation_result_serialization() {
        let result = ValidationResult {
            server_name: "test-server".to_string(),
            is_valid: true,
            errors: vec![],
            warnings: vec!["Warning message".to_string()],
        };

        let json = serde_json::to_string(&result).unwrap();
        assert!(json.contains("test-server"));
        assert!(json.contains("true"));
    }
}
