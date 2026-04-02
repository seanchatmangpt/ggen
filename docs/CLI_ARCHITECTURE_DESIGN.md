# ggen CLI Architecture Design

## Overview

This document details the architecture design for the new CLI commands (`agent` and `mcp` subcommands) in the ggen system. The design extends the existing clap-noun-verb system to provide unified interfaces for agent management and MCP (Model Context Protocol) integration.

## Core Architecture

### Command System Overview

```rust
/// Command Router - Extends existing clap-noun-verb system
pub struct CommandRouter {
    /// Agent command handlers
    agent_command: AgentCommand,
    /// MCP command handlers
    mcp_command: MCPCommand,
    /// Subcommand router for delegation
    subcommand_router: SubcommandRouter,
    /// Shared command utilities
    utils: CommandUtils,
    /// Configuration management
    config: ConfigManager,
    /// Error handling
    error_handler: ErrorHandler,
}

/// Main CLI Entrypoint
pub struct CLI {
    /// Command router
    router: CommandRouter,
    /// Global configuration
    config: GlobalConfig,
    /// Plugin system
    plugins: PluginRegistry,
    /// Output formatters
    formatters: OutputFormatterRegistry,
}
```

### Agent Command Architecture

```rust
/// Agent Command - Handler for `ggen agent` subcommands
pub struct AgentCommand {
    /// Create agent subcommand
    create: AgentCreate,
    /// Start agent subcommand
    start: AgentStart,
    /// Stop agent subcommand
    stop: AgentStop,
    /// Status subcommand
    status: AgentStatus,
    /// Logs subcommand
    logs: AgentLogs,
    /// Workflow integration subcommand
    workflow: AgentWorkflow,
    /// Configuration subcommand
    config: AgentConfig,
}

/// Agent Create Command
pub struct AgentCreate {
    /// Argument parser
    args: AgentCreateArgs,
    /// Agent factory
    factory: AgentFactory,
    /// Validator
    validator: AgentValidator,
    /// Template engine
    template_engine: TemplateEngine,
}

/// Agent Create Arguments
#[derive(Debug, clap::Parser)]
pub struct AgentCreateArgs {
    /// Agent name (required)
    #[clap(short, long, required = true)]
    name: String,

    /// Workflow template to use
    #[clap(short, long, default_value = "default")]
    workflow: String,

    /// Number of agent instances
    #[clap(short, long, default_value = "1")]
    instances: u32,

    /// Timeout in seconds
    #[clap(short, long, default_value = "30")]
    timeout: u64,

    /// Agent configuration file
    #[clap(short = 'c', long)]
    config: Option<PathBuf>,

    /// Enable debug mode
    #[clap(short, long)]
    debug: bool,

    /// Security profile
    #[clap(long, default_value = "default")]
    security_profile: String,

    /// Resource constraints
    #[clap(long)]
    memory_mb: Option<u64>,

    /// CPU limit
    #[clap(long)]
    cpu_limit: Option<u64>,

    /// Network policy
    #[clap(long)]
    network_policy: Option<String>,

    /// Environment variables
    #[clap(long, multiple_occurrences = true)]
    env: Vec<String>,
}

impl AgentCreateArgs {
    /// Validate arguments
    pub fn validate(&self) -> Result<(), CLIError> {
        // Validate name
        if self.name.is_empty() {
            return Err(CLIError::InvalidArgument("Agent name cannot be empty".to_string()));
        }

        // Validate timeout
        if self.timeout == 0 {
            return Err(CLIError::InvalidArgument("Timeout must be greater than 0".to_string()));
        }

        // Validate memory constraint
        if let Some(memory) = self.memory_mb {
            if memory == 0 {
                return Err(CLIError::InvalidArgument("Memory limit must be greater than 0".to_string()));
            }
        }

        Ok(())
    }

    /// Parse environment variables
    pub fn parse_env_vars(&self) -> Result<BTreeMap<String, String>, CLIError> {
        let mut env_vars = BTreeMap::new();

        for env_var in &self.env {
            let parts: Vec<&str> = env_var.splitn(2, '=').collect();
            if parts.len() != 2 {
                return Err(CLIError::InvalidArgument(format!("Invalid environment variable format: {}", env_var)));
            }

            env_vars.insert(parts[0].to_string(), parts[1].to_string());
        }

        Ok(env_vars)
    }
}

/// Agent Create Implementation
impl AgentCreate {
    /// Execute create command
    pub async fn execute(&self, args: AgentCreateArgs) -> CLIResult<AgentCreateResult> {
        // Validate arguments
        args.validate()?;

        // Parse environment variables
        let env_vars = args.parse_env_vars()?;

        // Load configuration
        let config = self.load_config(&args)?;

        // Validate agent configuration
        self.validator.validate(&config)?;

        // Generate agent specification from template
        let spec = self.generate_spec(&args, &config, &env_vars)?;

        // Create agent
        let agent = self.factory.create(&spec)?;

        // Start agent
        let started_agent = self.start_agent(&agent)?;

        // Generate output
        let result = AgentCreateResult {
            agent_id: started_agent.id.clone(),
            name: started_agent.name.clone(),
            workflow_id: started_agent.workflow_id.clone(),
            status: started_agent.status,
            url: started_agent.url,
            created_at: started_agent.created_at,
        };

        Ok(result)
    }

    /// Load configuration
    fn load_config(&self, args: &AgentCreateArgs) -> Result<AgentConfig, CLIError> {
        if let Some(config_path) = &args.config {
            let config_content = std::fs::read_to_string(config_path)
                .map_err(|e| CLIError::FileReadError(e.to_string()))?;

            let config: AgentConfig = serde_yaml::from_str(&config_content)
                .map_err(|e| CLIError::ConfigParseError(e.to_string()))?;

            Ok(config)
        } else {
            // Use default configuration
            Ok(AgentConfig::default())
        }
    }

    /// Generate agent specification
    fn generate_spec(&self, args: &AgentCreateArgs, config: &AgentConfig, env_vars: &BTreeMap<String, String>) -> Result<AgentSpec, CLIError> {
        let mut context = tera::Context::new();

        // Add variables from arguments
        context.insert("name", &args.name);
        context.insert("workflow", &args.workflow);
        context.insert("instances", &args.instances);
        context.insert("timeout", &args.timeout);
        context.insert("security_profile", &args.security_profile);

        // Add environment variables
        context.insert("env_vars", env_vars);

        // Add resource constraints
        if let Some(memory) = args.memory_mb {
            context.insert("memory_mb", &memory);
        }
        if let Some(cpu) = args.cpu_limit {
            context.insert("cpu_limit", &cpu);
        }
        if let Some(network_policy) = &args.network_policy {
            context.insert("network_policy", network_policy);
        }

        // Render template
        let spec_content = self.template_engine.render("agent-spec.yaml", &context)
            .map_err(|e| CLIError::TemplateError(e.to_string()))?;

        let spec: AgentSpec = serde_yaml::from_str(&spec_content)
            .map_err(|e| CLIError::ConfigParseError(e.to_string()))?;

        Ok(spec)
    }
}

/// Agent Start Command
pub struct AgentStart {
    /// Agent manager
    manager: AgentManager,
    /// Timeout configuration
    timeout: Duration,
    /// Health check interval
    health_check_interval: Duration,
}

impl AgentStart {
    /// Execute start command
    pub async fn execute(&self, agent_id: &str) -> CLIResult<AgentStartResult> {
        // Validate agent ID
        if agent_id.is_empty() {
            return Err(CLIError::InvalidArgument("Agent ID cannot be empty".to_string()));
        }

        // Start agent
        let agent = self.manager.start_agent(agent_id).await?;

        // Wait for health check
        self.wait_for_health_check(&agent).await?;

        // Generate result
        let result = AgentStartResult {
            agent_id: agent.id.clone(),
            status: agent.status,
            url: agent.url,
            started_at: agent.started_at,
        };

        Ok(result)
    }

    /// Wait for health check
    async fn wait_for_health_check(&self, agent: &Agent) -> CLIResult<()> {
        let mut interval = tokio::time::interval(self.health_check_interval);
        let timeout = tokio::time::timeout(self.timeout, async {
            loop {
                interval.tick().await;
                if agent.is_healthy() {
                    break;
                }
            }
        }).await.map_err(|_| CLIError::TimeoutError("Agent health check timeout".to_string()))?;

        Ok(())
    }
}
```

### MCP Command Architecture

```rust
/// MCP Command - Handler for `ggen mcp` subcommands
pub struct MCPCommand {
    /// MCP server subcommand
    server: MCPServerCommand,
    /// MCP client subcommand
    client: MCPClientCommand,
    /// MCP tools subcommand
    tools: MCPToolsCommand,
    /// MCP models subcommand
    models: MCPModelsCommand,
    /// MCP configuration subcommand
    config: MCPConfigCommand,
}

/// MCP Server Command
pub struct MCPServerCommand {
    /// Server configuration
    config: MCPConfig,
    /// Server instance
    server: Option<MCPServer>,
    /// Request router
    router: MCPRequestRouter,
    /// Middleware stack
    middleware: Vec<Box<dyn Middleware>>,
}

impl MCPServerCommand {
    /// Execute server command
    pub async fn execute(&mut self, args: MCPServerArgs) -> CLIResult<MCPStartResult> {
        // Load configuration
        self.config = self.load_config(&args)?;

        // Create server
        self.server = Some(MCPServer::new(&self.config));

        // Start server
        let server = self.server.as_mut().unwrap();
        server.start().await?;

        // Register middleware
        for middleware in &mut self.middleware {
            server.register_middleware(middleware);
        }

        // Wait for startup
        server.wait_for_startup().await?;

        // Generate result
        let result = MCPStartResult {
            pid: server.pid(),
            address: server.address(),
            port: server.port(),
            protocol: server.protocol(),
            started_at: server.started_at(),
        };

        Ok(result)
    }

    /// Load configuration
    fn load_config(&self, args: &MCPServerArgs) -> Result<MCPConfig, CLIError> {
        let mut config = MCPConfig::default();

        // Override with command line arguments
        config.address = args.address.clone().unwrap_or_else(|| "127.0.0.1".to_string());
        config.port = args.port.unwrap_or_else(|| 8080);
        config.protocol = args.protocol.clone().unwrap_or_else(|| "http".to_string());
        config.max_connections = args.max_connections.unwrap_or_else(|| 1000);

        // Load from file if specified
        if let Some(config_path) = &args.config {
            let file_config = self.load_config_from_file(config_path)?;
            config.merge(file_config);
        }

        Ok(config)
    }

    /// Load configuration from file
    fn load_config_from_file(&self, path: &Path) -> Result<MCPConfig, CLIError> {
        let content = std::fs::read_to_string(path)
            .map_err(|e| CLIError::FileReadError(e.to_string()))?;

        let config: MCPConfig = serde_yaml::from_str(&content)
            .map_err(|e| CLIError::ConfigParseError(e.to_string()))?;

        Ok(config)
    }
}

/// MCP Client Command
pub struct MCPClientCommand {
    /// Client configuration
    config: MCPClientConfig,
    /// Client instance
    client: Option<MCPClient>,
    /// Command registry
    command_registry: CommandRegistry,
}

impl MCPClientCommand {
    /// Execute client command
    pub async fn execute(&mut self, args: MCPClientArgs) -> CLIResult<MCPCommandResult> {
        // Load configuration
        self.config = self.load_config(&args)?;

        // Create client
        self.client = Some(MCPClient::new(&self.config));

        // Execute command
        let result = self.execute_command(&args).await?;

        Ok(result)
    }

    /// Execute command
    async fn execute_command(&self, args: &MCPClientArgs) -> CLIResult<MCPCommandResult> {
        let client = self.client.as_ref().unwrap();

        match args.command.as_str() {
            "list" => {
                let tools = client.list_tools().await?;
                Ok(MCPCommandResult::Tools(tools))
            }
            "execute" => {
                let result = client.execute_tool(&args.tool_name, &args.arguments).await?;
                Ok(MCPCommandResult::ToolResult(result))
            }
            "health" => {
                let health = client.check_health().await?;
                Ok(MCPCommandResult::Health(health))
            }
            _ => Err(CLIError::InvalidArgument(format!("Unknown command: {}", args.command)))
        }
    }
}
```

### Integration with Existing CLI System

```rust
/// Extended Command Router - Extends existing clap-noun-verb system
pub struct ExtendedCommandRouter {
    /// Original command router
    original: CommandRouter,
    /// New agent command
    agent_command: AgentCommand,
    /// New MCP command
    mcp_command: MCPCommand,
    /// Plugin system
    plugins: PluginRegistry,
}

impl ExtendedCommandRouter {
    /// Route commands to appropriate handlers
    pub async fn route_command(&mut self, command: &str, args: &[String]) -> CLIResult<()> {
        match command {
            "agent" => {
                self.route_agent_command(args).await
            }
            "mcp" => {
                self.route_mcp_command(args).await
            }
            _ => {
                // Delegate to original router
                self.original.route_command(command, args).await
            }
        }
    }

    /// Route agent subcommands
    async fn route_agent_command(&mut self, args: &[String]) -> CLIResult<()> {
        if args.is_empty() {
            return Err(CLIError::MissingSubcommand("agent".to_string()));
        }

        let subcommand = &args[0];
        let remaining_args = &args[1..];

        match subcommand {
            "create" => {
                let create_args = AgentCreateArgs::try_parse_from(remaining_args)?;
                let result = self.agent_command.create.execute(create_args).await?;
                self.output_result(result)
            }
            "start" => {
                if remaining_args.len() != 1 {
                    return Err(CLIError::InvalidArgument("Usage: agent start <agent_id>".to_string()));
                }
                let result = self.agent_command.start.execute(&remaining_args[0]).await?;
                self.output_result(result)
            }
            "stop" => {
                if remaining_args.len() != 1 {
                    return Err(CLIError::InvalidArgument("Usage: agent stop <agent_id>".to_string()));
                }
                let result = self.agent_command.stop.execute(&remaining_args[0]).await?;
                self.output_result(result)
            }
            "status" => {
                if remaining_args.len() != 1 {
                    return Err(CLIError::InvalidArgument("Usage: agent status <agent_id>".to_string()));
                }
                let result = self.agent_command.status.execute(&remaining_args[0]).await?;
                self.output_result(result)
            }
            "logs" => {
                if remaining_args.is_empty() {
                    return Err(CLIError::MissingSubcommand("agent logs".to_string()));
                }
                self.route_agent_logs_command(remaining_args).await
            }
            "config" => {
                if remaining_args.is_empty() {
                    return Err(CLIError::MissingSubcommand("agent config".to_string()));
                }
                self.route_agent_config_command(remaining_args).await
            }
            _ => {
                Err(CLIError::InvalidArgument(format!("Unknown agent subcommand: {}", subcommand)))
            }
        }
    }

    /// Route MCP subcommands
    async fn route_mcp_command(&mut self, args: &[String]) -> CLIResult<()> {
        if args.is_empty() {
            return Err(CLIError::MissingSubcommand("mcp".to_string()));
        }

        let subcommand = &args[0];
        let remaining_args = &args[1..];

        match subcommand {
            "server" => {
                let server_args = MCPServerArgs::try_parse_from(remaining_args)?;
                let result = self.mcp_command.server.execute(server_args).await?;
                self.output_result(result)
            }
            "client" => {
                let client_args = MCPClientArgs::try_parse_from(remaining_args)?;
                let result = self.mcp_command.client.execute(client_args).await?;
                self.output_result(result)
            }
            "tools" => {
                if remaining_args.len() != 1 {
                    return Err(CLIError::InvalidArgument("Usage: mcp tools <command> [options]".to_string()));
                }
                self.route_mcp_tools_command(remaining_args).await
            }
            "models" => {
                if remaining_args.is_empty() {
                    return Err(CLIError::MissingSubcommand("mcp models".to_string()));
                }
                self.route_mcp_models_command(remaining_args).await
            }
            "config" => {
                if remaining_args.is_empty() {
                    return Err(CLIError::MissingSubcommand("mcp config".to_string()));
                }
                self.route_mcp_config_command(remaining_args).await
            }
            _ => {
                Err(CLIError::InvalidArgument(format!("Unknown MCP subcommand: {}", subcommand)))
            }
        }
    }
}
```

### Error Handling

```rust
/// CLI Error Types
#[derive(Debug)]
pub enum CLIError {
    /// Invalid argument
    InvalidArgument(String),
    /// Missing subcommand
    MissingSubcommand(String),
    /// File read error
    FileReadError(String),
    /// Configuration parse error
    ConfigParseError(String),
    /// Template error
    TemplateError(String),
    /// Network error
    NetworkError(String),
    /// Agent error
    AgentError(AgentError),
    /// MCP error
    MCPError(MCPError),
    /// Timeout error
    TimeoutError(String),
    /// Configuration error
    ConfigurationError(String),
    /// Authentication error
    AuthenticationError(String),
    /// Permission error
    PermissionError(String),
    /// Unknown error
    Unknown(String),
}

/// CLI Result Type
pub type CLIResult<T> = Result<T, CLIError>;

/// CLI Error Display
impl std::fmt::Display for CLIError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            CLIError::InvalidArgument(msg) => write!(f, "Invalid argument: {}", msg),
            CLIError::MissingSubcommand(cmd) => write!(f, "Missing subcommand for: {}", cmd),
            CLIError::FileReadError(msg) => write!(f, "File read error: {}", msg),
            CLIError::ConfigParseError(msg) => write!(f, "Configuration parse error: {}", msg),
            CLIError::TemplateError(msg) => write!(f, "Template error: {}", msg),
            CLIError::NetworkError(msg) => write!(f, "Network error: {}", msg),
            CLIError::AgentError(e) => write!(f, "Agent error: {}", e),
            CLIError::MCPError(e) => write!(f, "MCP error: {}", e),
            CLIError::TimeoutError(msg) => write!(f, "Timeout error: {}", msg),
            CLIError::ConfigurationError(msg) => write!(f, "Configuration error: {}", msg),
            CLIError::AuthenticationError(msg) => write!(f, "Authentication error: {}", msg),
            CLIError::PermissionError(msg) => write!(f, "Permission error: {}", msg),
            CLIError::Unknown(msg) => write!(f, "Unknown error: {}", msg),
        }
    }
}

/// CLI Error Handling
pub struct ErrorHandler {
    /// Error logger
    logger: ErrorLogger,
    /// Error reporter
    reporter: ErrorReporter,
    /// Recovery strategies
    recovery: RecoveryStrategies,
}

impl ErrorHandler {
    /// Handle CLI error
    pub fn handle_error(&self, error: &CLIError) -> CLIResult<()> {
        // Log error
        self.logger.log_error(error);

        // Report error
        self.reporter.report_error(error);

        // Attempt recovery if possible
        if let Some(recovery) = self.recovery.try_recovery(error) {
            return recovery.execute();
        }

        // No recovery available
        Err(error.clone())
    }
}
```

### Output Formatting

```rust
/// Output Formatters
pub struct OutputFormatterRegistry {
    /// JSON formatter
    json_formatter: JSONFormatter,
    /// YAML formatter
    yaml_formatter: YAMLFormatter,
    /// Table formatter
    table_formatter: TableFormatter,
    /// Plain text formatter
    plain_formatter: PlainTextFormatter,
}

/// Output Result
pub enum OutputResult {
    JSON(serde_json::Value),
    YAML(serde_yaml::Value),
    Table(Vec<Vec<String>>),
    PlainText(String),
}

/// Output Formatter Trait
pub trait OutputFormatter {
    fn format(&self, result: &CLIResult<()>) -> OutputResult;
}

/// JSON Formatter
pub struct JSONFormatter;

impl OutputFormatter for JSONFormatter {
    fn format(&self, result: &CLIResult<()>) -> OutputResult {
        match result {
            Ok(()) => OutputResult::JSON(serde_json::json!({
                "status": "success",
                "message": "Command executed successfully"
            })),
            Err(e) => OutputResult::JSON(serde_json::json!({
                "status": "error",
                "message": e.to_string(),
                "error": serde_json::to_value(e).unwrap_or_default()
            })),
        }
    }
}

/// Table Formatter
pub struct TableFormatter;

impl OutputFormatter for TableFormatter {
    fn format(&self, result: &CLIResult<()>) -> OutputResult {
        match result {
            Ok(()) => OutputResult::Table(vec![
                vec!["Status".to_string(), "Success".to_string()],
                vec!["Message".to_string(), "Command executed successfully".to_string()],
            ]),
            Err(e) => OutputResult::Table(vec![
                vec!["Status".to_string(), "Error".to_string()],
                vec!["Message".to_string(), e.to_string()],
            ]),
        }
    }
}
```

### Plugin System

```rust
/// Plugin Registry
pub struct PluginRegistry {
    /// Registered plugins
    plugins: BTreeMap<String, Box<dyn Plugin>>,
    /// Plugin loader
    loader: PluginLoader,
    /// Security manager
    security: PluginSecurityManager,
}

/// Plugin Trait
pub trait Plugin: Send + Sync {
    /// Plugin name
    fn name(&self) -> &str;
    /// Plugin version
    fn version(&self) -> &str;
    /// Plugin description
    fn description(&self) -> &str;
    /// Initialize plugin
    fn initialize(&mut self, config: PluginConfig) -> CLIResult<()>;
    /// Execute plugin command
    fn execute(&self, command: &str, args: &[String]) -> CLIResult<()>;
    /// Cleanup plugin
    fn cleanup(&mut self) -> CLIResult<()>;
}

/// CLI Plugin
pub struct CLIPlugin {
    /// Plugin name
    name: String,
    /// Plugin implementation
    implementation: Box<dyn PluginImplementation>,
    /// Configuration
    config: PluginConfig,
    /// Enabled state
    enabled: bool,
}

impl CLIPlugin {
    /// Create new plugin
    pub fn new(name: String, implementation: Box<dyn PluginImplementation>) -> Self {
        Self {
            name,
            implementation,
            config: PluginConfig::default(),
            enabled: true,
        }
    }

    /// Initialize plugin
    pub fn initialize(&mut self, config: PluginConfig) -> CLIResult<()> {
        self.config = config;
        self.implementation.initialize(self.config.clone())
    }

    /// Execute plugin command
    pub fn execute(&self, command: &str, args: &[String]) -> CLIResult<()> {
        if !self.enabled {
            return Err(CLIError::PluginDisabled(self.name.clone()));
        }
        self.implementation.execute(command, args)
    }
}
```

## Usage Examples

```rust
/// Example: Using agent commands
#[tokio::main]
async fn example_agent_commands() -> CLIResult<()> {
    let mut router = ExtendedCommandRouter::new();

    // Create agent
    let args = vec!["agent".to_string(), "create".to_string(),
                   "my-agent".to_string(), "--workflow", "workflow-1".to_string()];
    router.route_command(&args[0], &args[1..]).await?;

    // Start agent
    let args = vec!["agent".to_string(), "start".to_string(), "my-agent".to_string()];
    router.route_command(&args[0], &args[1..]).await?;

    // Get status
    let args = vec!["agent".to_string(), "status".to_string(), "my-agent".to_string()];
    router.route_command(&args[0], &args[1..]).await?;

    Ok(())
}

/// Example: Using MCP commands
async fn example_mcp_commands() -> CLIResult<()> {
    let mut router = ExtendedCommandRouter::new();

    // Start MCP server
    let args = vec!["mcp".to_string(), "server".to_string(),
                   "--port", "8080".to_string(), "--address", "127.0.0.1".to_string()];
    router.route_command(&args[0], &args[1..]).await?;

    // List tools
    let args = vec!["mcp".to_string(), "tools".to_string(), "list".to_string()];
    router.route_command(&args[0], &args[1..]).await?;

    // Execute tool
    let args = vec!["mcp".to_string(), "tools".to_string(), "execute".to_string(),
                   "file-read".to_string(), "--path", "/path/to/file".to_string()];
    router.route_command(&args[0], &args[1..]).await?;

    Ok(())
}

/// Example: Error handling
async fn example_error_handling() -> CLIResult<()> {
    let router = ExtendedCommandRouter::new();

    // Invalid command
    let args = vec!["agent".to_string(), "invalid".to_string()];
    match router.route_command(&args[0], &args[1..]).await {
        Ok(_) => println!("Command succeeded"),
        Err(e) => {
            println!("Error: {}", e);
            // Handle specific error types
            match e {
                CLIError::InvalidArgument(msg) => println!("Invalid argument: {}", msg),
                CLIError::MissingSubcommand(cmd) => println!("Missing subcommand for: {}", cmd),
                _ => println!("Other error"),
            }
        }
    }

    Ok(())
}
```

## Implementation Roadmap

### Phase 1: Foundation (Week 1)
- Extend clap-noun-verb system with new command structure
- Implement basic agent command framework
- Create MCP command infrastructure
- Set up error handling and output formatting

### Phase 2: Core Commands (Week 2)
- Implement agent create/start/stop commands
- Implement MCP server/client commands
- Add configuration management
- Create plugin system foundation

### Phase 3: Advanced Features (Week 3)
- Add agent workflow integration
- Implement MCP tools and models commands
- Add comprehensive monitoring and logging
- Create advanced output formatters

### Phase 4: Validation and Testing (Week 4)
- Comprehensive testing of all commands
- Integration testing with existing system
- Performance optimization
- Documentation and user guides

## Conclusion

The CLI architecture provides a comprehensive extension to the existing ggen command system, adding powerful agent management and MCP integration capabilities. The design maintains compatibility with the existing clap-noun-verb system while adding new functionality through a clean, modular architecture. The error handling, plugin system, and output formatting ensure a professional user experience suitable for enterprise deployment.