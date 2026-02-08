# A2A Configuration Reference

Complete reference for A2A (Agent-to-Agent) configuration files.

## Configuration Overview

A2A configuration is stored in TOML format (`a2a.toml`) and defines A2A servers, agents, workflows, and runtime settings.

### Configuration Priority

Configuration is resolved from multiple sources in order of priority (highest to lowest):

1. **CLI arguments** - `--config` flag
2. **Environment variables** - `GGEN_A2A_*` variables
3. **Project configuration** - `a2a.toml` in current directory
4. **User configuration** - `~/.ggen/a2a.toml`
5. **System configuration** - `/etc/ggen/a2a.toml`
6. **Default values** - Built-in defaults

---

## Configuration File Schema

### Root Structure

```toml
[server]
host = "127.0.0.1"
port = 8080
tls_enabled = false
timeout = 30
max_connections = 100

[agents.<agent_name>]
agent_type = "generation"
name = "Text Generator"
description = "Generates text content"
enabled = true

[agents.<agent_name>.config]
key = "value"

[workflows.<workflow_name>]
spec_file = "workflow.yawl"
name = "Data Pipeline"
auto_start = true

[metadata]
version = "1.0.0"
environment = "development"
updated_at = "2024-01-15T10:30:00Z"
```

---

## Server Configuration

### `[server]` Section

Defines the A2A server configuration.

| Property | Type | Required | Default | Description |
|----------|------|----------|---------|-------------|
| `host` | string | No | `"127.0.0.1"` | Server host address |
| `port` | integer | No | `8080` | Server port (must be > 0) |
| `tls_enabled` | boolean | No | `false` | Enable TLS |
| `tls_cert_path` | string | No | `null` | Path to TLS certificate |
| `tls_key_path` | string | No | `null` | Path to TLS private key |
| `timeout` | integer | No | `30` | Request timeout in seconds (must be > 0) |
| `max_connections` | integer | No | `100` | Maximum concurrent connections |

### Example

```toml
[server]
host = "127.0.0.1"
port = 8080
tls_enabled = false
timeout = 30
max_connections = 100
```

### TLS Configuration

```toml
[server]
host = "0.0.0.0"
port = 8443
tls_enabled = true
tls_cert_path = "/etc/ssl/certs/a2a-server.crt"
tls_key_path = "/etc/ssl/private/a2a-server.key"
timeout = 60
max_connections = 500
```

---

## Agent Configuration

### `[agents.<name>]` Section

Defines individual A2A agents.

| Property | Type | Required | Default | Description |
|----------|------|----------|---------|-------------|
| `agent_type` | string | **Yes** | - | Type of agent (generation, analysis, workflow, etc.) |
| `name` | string | **Yes** | - | Display name for the agent |
| `description` | string | No | `null` | Agent description |
| `enabled` | boolean | No | `true` | Whether the agent is enabled |
| `config` | table | No | `{}` | Agent-specific configuration |

### Agent Types

| Type | Description |
|------|-------------|
| `generation` | Text/content generation agents |
| `analysis` | Code/data analysis agents |
| `workflow` | Workflow orchestration agents |
| `monitoring` | Monitoring and observability agents |
| `custom` | Custom agent types |

### Example

```toml
[agents.text-generator]
agent_type = "generation"
name = "Text Generator"
description = "Generates text content using LLM"
enabled = true

[agents.text-generator.config]
model = "gpt-4"
max_tokens = 2000
temperature = 0.7

[agents.code-analyzer]
agent_type = "analysis"
name = "Code Analyzer"
description = "Analyzes code for quality and security"
enabled = true

[agents.code-analyzer.config]
languages = ["rust", "python", "javascript"]
check_security = true
check_performance = true
```

---

## Workflow Configuration

### `[workflows.<name>]` Section

Defines workflow configurations.

| Property | Type | Required | Default | Description |
|----------|------|----------|---------|-------------|
| `spec_file` | string | **Yes** | - | Path to workflow specification file |
| `name` | string | **Yes** | - | Workflow name |
| `auto_start` | boolean | No | `false` | Auto-start on initialization |

### Example

```toml
[workflows.data-pipeline]
spec_file = "workflows/data-pipeline.yawl"
name = "Data Processing Pipeline"
auto_start = true

[workflows.code-review]
spec_file = "workflows/review.yawl"
name = "Automated Code Review"
auto_start = false
```

---

## Metadata Configuration

### `[metadata]` Section

Configuration metadata.

| Property | Type | Required | Default | Description |
|----------|------|----------|---------|-------------|
| `version` | string | No | `"1.0.0"` | Configuration version |
| `environment` | string | No | `null` | Environment name (development, staging, production) |
| `updated_at` | string | No | `null` | ISO 8601 timestamp of last update |

### Example

```toml
[metadata]
version = "1.0.0"
environment = "development"
updated_at = "2024-01-15T10:30:00Z"
```

---

## Complete Example

```toml
# A2A Server Configuration
[server]
host = "127.0.0.1"
port = 8080
tls_enabled = false
timeout = 30
max_connections = 100

# Agent Configurations
[agents.text-generator]
agent_type = "generation"
name = "Text Generator"
description = "Generates text content using LLM"
enabled = true

[agents.text-generator.config]
model = "gpt-4"
max_tokens = 2000
temperature = 0.7

[agents.code-analyzer]
agent_type = "analysis"
name = "Code Analyzer"
description = "Analyzes code for quality and security"
enabled = true

[agents.code-analyzer.config]
languages = ["rust", "python", "javascript"]
check_security = true
check_performance = true
max_file_size = 1048576

[agents.workflow-coordinator]
agent_type = "workflow"
name = "Workflow Coordinator"
description = "Coordinates multi-agent workflows"
enabled = true

[agents.workflow-coordinator.config]
max_parallel_tasks = 10
timeout = 300
retry_attempts = 3

# Workflow Configurations
[workflows.data-pipeline]
spec_file = "workflows/data-pipeline.yawl"
name = "Data Processing Pipeline"
auto_start = true

[workflows.code-review]
spec_file = "workflows/review.yawl"
name = "Automated Code Review"
auto_start = false

# Metadata
[metadata]
version = "1.0.0"
environment = "development"
updated_at = "2024-01-15T10:30:00Z"
```

---

## Environment Variable Configuration

Individual properties can be overridden using environment variables.

### Variable Format

```
GGEN_A2A_<PROPERTY>=<value>
```

### Supported Properties

| Property | Environment Variable | Example |
|----------|---------------------|---------|
| Host | `GGEN_A2A_HOST` | `GGEN_A2A_HOST=192.168.1.100` |
| Port | `GGEN_A2A_PORT` | `GGEN_A2A_PORT=9000` |
| Timeout | `GGEN_A2A_TIMEOUT` | `GGEN_A2A_TIMEOUT=60` |
| TLS Enabled | `GGEN_A2A_TLS_ENABLED` | `GGEN_A2A_TLS_ENABLED=1` |
| TLS Cert | `GGEN_A2A_TLS_CERT` | `GGEN_A2A_TLS_CERT=/path/to/cert.pem` |
| TLS Key | `GGEN_A2A_TLS_KEY` | `GGEN_A2A_TLS_KEY=/path/to/key.pem` |
| Config File | `GGEN_A2A_CONFIG` | `GGEN_A2A_CONFIG=/path/to/a2a.toml` |

### Environment Configuration Example

```bash
# Set server address
export GGEN_A2A_HOST="192.168.1.100"
export GGEN_A2A_PORT="9000"

# Enable TLS
export GGEN_A2A_TLS_ENABLED="1"
export GGEN_A2A_TLS_CERT="/etc/ssl/certs/a2a-server.crt"
export GGEN_A2A_TLS_KEY="/etc/ssl/private/a2a-server.key"

# Set timeout
export GGEN_A2A_TIMEOUT="60"

# Or use a custom config file
export GGEN_A2A_CONFIG="/custom/path/a2a.toml"
```

---

## Validation Rules

### Server Validation

| Rule | Description | Error |
|------|-------------|-------|
| Valid port | Port must be > 0 | `InvalidPort` |
| Valid timeout | Timeout must be > 0 | `InvalidTimeout` |
| TLS consistency | If TLS enabled, cert and key must be specified | `TlsMisconfigured` |

---

## Default Values

### Default Server Configuration

```toml
[server]
host = "127.0.0.1"
port = 8080
tls_enabled = false
tls_cert_path = null
tls_key_path = null
timeout = 30
max_connections = 100
```

### Default Metadata

```toml
[metadata]
version = "1.0.0"
environment = null
updated_at = null
```

---

## File Locations

| Location | Platform | Path |
|----------|----------|------|
| Project | All | `a2a.toml` |
| User | All | `~/.ggen/a2a.toml` |
| System | Unix/Linux | `/etc/ggen/a2a.toml` |
| System | Windows | `%PROGRAMDATA%\ggen\a2a.toml` |

---

## Common Configuration Patterns

### Development Environment

```toml
[server]
host = "127.0.0.1"
port = 8080
tls_enabled = false
timeout = 30
max_connections = 50

[metadata]
environment = "development"
```

### Production Environment

```toml
[server]
host = "0.0.0.0"
port = 8443
tls_enabled = true
tls_cert_path = "/etc/ssl/certs/a2a-server.crt"
tls_key_path = "/etc/ssl/private/a2a-server.key"
timeout = 60
max_connections = 500

[metadata]
environment = "production"
```

### Testing Environment

```toml
[server]
host = "127.0.0.1"
port = 0  # Random port
tls_enabled = false
timeout = 10
max_connections = 10

[metadata]
environment = "testing"
```

### Multi-Agent Setup

```toml
[server]
host = "127.0.0.1"
port = 8080
timeout = 30

[agents.generator]
agent_type = "generation"
name = "Content Generator"
enabled = true

[agents.analyzer]
agent_type = "analysis"
name = "Data Analyzer"
enabled = true

[agents.coordinator]
agent_type = "workflow"
name = "Task Coordinator"
enabled = true

[agents.coordinator.config]
max_parallel_tasks = 5
```

---

## Configuration Validation

### Validate with CLI

```bash
# Validate default configuration
ggen mcp config validate --a2a

# Validate specific file
ggen mcp config validate --a2a --file /path/to/a2a.toml

# Show detailed results
ggen mcp config validate --a2a --verbose
```

### Programmatic Validation

```rust
use ggen_domain::mcp_config::{load_a2a_from_file, A2aConfig};

// Load configuration
let config: A2aConfig = load_a2a_from_file(&PathBuf::from("a2a.toml"))?;

// Validate configuration
match config.validate() {
    Ok(_) => println!("Configuration is valid"),
    Err(e) => eprintln!("Validation error: {}", e),
}

// Get server URL
let url = config.server_url();  // e.g., "http://127.0.0.1:8080"
```

---

## See Also

- [A2A Command Reference](../commands/a2a.md)
- [MCP Configuration Reference](./mcp-config.md)
- [MCP A2A Integration Guide](../MCP_A2A_INTEGRATION.md)
