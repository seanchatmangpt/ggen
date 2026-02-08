<!-- START doctoc generated TOC please keep comment here to allow auto update -->
<!-- DON'T EDIT THIS SECTION, INSTEAD RE-RUN doctoc TO UPDATE -->
**Table of Contents**

- [How to Configure MCP Server Settings](#how-to-configure-mcp-server-settings)
  - [Initializing Configuration](#initializing-configuration)
  - [Configuration File Structure](#configuration-file-structure)
  - [Server Settings](#server-settings)
  - [Transport Configuration](#transport-configuration)
  - [Tool Settings](#tool-settings)
  - [Logging Configuration](#logging-configuration)
  - [Environment Variables](#environment-variables)
  - [Validating Configuration](#validating-configuration)
  - [Reloading Configuration](#reloading-configuration)
  - [Troubleshooting](#troubleshooting)
    - [Configuration Not Loading](#configuration-not-loading)
    - [Invalid Configuration](#invalid-configuration)
    - [Changes Not Taking Effect](#changes-not-taking-effect)
  - [Next Steps](#next-steps)

<!-- END doctoc generated TOC -->

# How to Configure MCP Server Settings

Guide to configuring the MCP server for optimal performance and security.

## Initializing Configuration

**Problem:** You need to create a new MCP configuration.

**Solution:** Use the `ggen mcp config init` command.

```bash
# Initialize with default configuration
ggen mcp config init

# Initialize at custom path
ggen mcp config init --file /path/to/mcp-config.json

# Initialize with example servers
ggen mcp config init --examples
```

**Expected Output:**
```bash
$ ggen mcp config init

Initializing MCP configuration at: /Users/sac/ggen/.mcp.json

MCP configuration initialized successfully
  Version: 1.0
  Servers configured: 3
  Default server: stdio
```

## Configuration File Structure

The MCP configuration file (`.mcp.json`) follows this structure:

```json
{
  "version": "1.0",
  "description": "MCP server configuration",
  "default_server": "stdio",
  "mcp_servers": {
    "stdio": {
      "command": "ggen",
      "args": ["mcp", "server", "stdio"],
      "enabled": true
    },
    "http": {
      "command": "ggen",
      "args": ["mcp", "server", "http", "--port", "3000"],
      "enabled": true,
      "env": {
        "GGEN_MCP_PORT": "3000"
      }
    }
  }
}
```

## Server Settings

**Problem:** You need to configure server-specific settings.

**Solution:** Edit the `mcp_servers` section.

```json
{
  "mcp_servers": {
    "production": {
      "command": "ggen",
      "args": [
        "mcp",
        "server",
        "http",
        "--port", "8080",
        "--host", "0.0.0.0",
        "--workers", "4"
      ],
      "enabled": true,
      "cwd": "/app/ggen",
      "timeout": 30000,
      "env": {
        "GGEN_MCP_PORT": "8080",
        "GGEN_MCP_HOST": "0.0.0.0",
        "RUST_LOG": "info"
      }
    }
  }
}
```

**Available server options:**

| Option | Type | Description |
|--------|------|-------------|
| `command` | string | Command to start the server |
| `args` | array | Command arguments |
| `enabled` | boolean | Whether the server is enabled |
| `cwd` | string | Working directory |
| `timeout` | number | Timeout in milliseconds |
| `env` | object | Environment variables |

## Transport Configuration

**Problem:** You need to configure different transport protocols.

**Solution:** Configure each transport separately.

### HTTP Transport

```json
{
  "transports": {
    "http": {
      "enabled": true,
      "port": 3000,
      "host": "127.0.0.1",
      "timeout_ms": 10000,
      "max_retries": 3,
      "ssl_verify": true
    }
  }
}
```

### WebSocket Transport

```json
{
  "transports": {
    "websocket": {
      "enabled": true,
      "url": "ws://localhost:3000",
      "reconnect_interval_ms": 5000,
      "max_reconnect_attempts": 10,
      "message_timeout_ms": 30000,
      "compression": true
    }
  }
}
```

### QUIC Transport

```json
{
  "transports": {
    "quic": {
      "enabled": true,
      "server_name": "localhost",
      "port": 4433,
      "idle_timeout_ms": 30000,
      "max_concurrent_streams": 100,
      "enable_zero_rtt": true
    }
  }
}
```

## Tool Settings

**Problem:** You need to configure tool-specific behavior.

**Solution:** Add tool configuration.

```json
{
  "tools": {
    "defaults": {
      "timeout_ms": 5000,
      "max_retries": 2
    },
    "agent-start": {
      "timeout_ms": 10000,
      "max_concurrent": 5
    },
    "workflow-start": {
      "timeout_ms": 30000,
      "max_retries": 1
    }
  }
}
```

## Logging Configuration

**Problem:** You need to configure server logging.

**Solution:** Add logging settings.

```json
{
  "logging": {
    "level": "info",
    "format": "json",
    "output": "stdout",
    "file": {
      "enabled": true,
      "path": "/var/log/ggen/mcp.log",
      "rotation": {
        "max_size_mb": 100,
        "max_files": 10
      }
    },
    "modules": {
      "mcp_server": "debug",
      "transport": "info",
      "auth": "warn"
    }
  }
}
```

## Environment Variables

**Problem:** You need to override configuration with environment variables.

**Solution:** Set `GGEN_MCP_*` variables.

```bash
# Server configuration
export GGEN_MCP_PORT=3000
export GGEN_MCP_HOST=0.0.0.0
export GGEN_MCP_TIMEOUT_MS=10000

# Transport configuration
export GGEN_MCP_TRANSPORT=http
export GGEN_MCP_WS_URL=ws://localhost:3000

# Authentication
export GGEN_MCP_API_KEY=your-api-key
export GGEN_MCP_JWT_SECRET=your-secret

# Logging
export RUST_LOG=debug,ggen_mcp=trace

# Enable features
export GGEN_MCP_ENABLE_QUIC=true
export GGEN_MCP_ENABLE_TLS=true
```

**Priority order:** Environment variables > Config file > Defaults

## Validating Configuration

**Problem:** You need to verify your configuration is valid.

**Solution:** Use the `ggen mcp config validate` command.

```bash
# Validate default configuration
ggen mcp config validate

# Validate specific file
ggen mcp config validate --file /path/to/config.json

# Validate with verbose output
ggen mcp config validate --verbose
```

**Expected Output:**
```bash
$ ggen mcp config validate

Validating MCP configuration: /Users/sac/ggen/.mcp.json

Validation Results:
  Config version: 1.0

  Server: stdio
    ✓ Command found
    ✓ Enabled

  Server: http
    ✓ Command found
    ✓ Enabled
    ! Warning: Port 3000 may be in use

Configuration is valid!
```

## Reloading Configuration

**Problem:** You need to apply configuration changes without restarting.

**Solution:** Use the reload command.

```bash
# Reload configuration
ggen mcp server reload

# Reload specific server
ggen mcp server reload --server http

# Reload with validation
ggen mcp server reload --validate
```

## Troubleshooting

### Configuration Not Loading

**Problem:** Server uses default settings instead of configured values.

**Solutions:**
```bash
# Check config file exists
ls -la ~/.mcp.json

# Verify config file location
ggen mcp config show

# Check file permissions
chmod 644 ~/.mcp.json

# Validate syntax
ggen mcp config validate --verbose
```

### Invalid Configuration

**Problem:** Server fails to start with configuration errors.

**Solutions:**
```bash
# Validate configuration
ggen mcp config validate

# Check JSON syntax
cat ~/.mcp.json | jq .

# Show effective configuration
ggen mcp config show --effective

# Reset to defaults
ggen mcp config init --force
```

### Changes Not Taking Effect

**Problem:** Modified configuration doesn't change server behavior.

**Solutions:**
```bash
# Reload server
ggen mcp server reload

# Restart server
ggen mcp server restart

# Clear configuration cache
ggen mcp cache clear

# Check environment variables
env | grep GGEN_MCP
```

## Next Steps

- **Setup authentication:** [How to Setup Authentication](setup-authentication.md)
- **List tools:** [How to List and Filter MCP Tools](list-tools.md)
- **Bridge agents:** [How to Bridge A2A Agents as MCP Tools](bridge-agents.md)
- **Server commands:** [CLI Reference](../reference/cli.md#mcp-server-commands)
- **Integration guide:** [MCP A2A Integration Guide](../MCP_A2A_INTEGRATION.md)
