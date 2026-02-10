<!-- START doctoc generated TOC please keep comment here to allow auto update -->
<!-- DON'T EDIT THIS SECTION, INSTEAD RE-RUN doctoc TO UPDATE -->
**Table of Contents**

- [MCP Configuration Reference](#mcp-configuration-reference)
  - [Configuration Overview](#configuration-overview)
    - [Configuration Priority](#configuration-priority)
  - [Configuration File Schema](#configuration-file-schema)
    - [Root Structure](#root-structure)
    - [Root Properties](#root-properties)
  - [Server Configuration](#server-configuration)
    - [ServerConfig Schema](#serverconfig-schema)
    - [Server Properties](#server-properties)
  - [Metadata Schema](#metadata-schema)
    - [Metadata Properties](#metadata-properties)
  - [Complete Example](#complete-example)
  - [Environment Variable Configuration](#environment-variable-configuration)
    - [Variable Format](#variable-format)
    - [Supported Properties](#supported-properties)
    - [Environment Configuration Example](#environment-configuration-example)
  - [Validation Rules](#validation-rules)
    - [Server Validation](#server-validation)
    - [Dangerous Command Detection](#dangerous-command-detection)
  - [Default Values](#default-values)
    - [Default Server Configuration](#default-server-configuration)
    - [Default Root Configuration](#default-root-configuration)
  - [File Locations](#file-locations)
    - [PID and Lock Files](#pid-and-lock-files)
  - [Common Server Examples](#common-server-examples)
    - [Claude Code Guide](#claude-code-guide)
    - [Git Server](#git-server)
    - [Bash Server](#bash-server)
    - [Filesystem Server](#filesystem-server)
    - [SSH Remote Server](#ssh-remote-server)
    - [Python Server](#python-server)
  - [Configuration Validation](#configuration-validation)
    - [Validate with CLI](#validate-with-cli)
    - [Programmatic Validation](#programmatic-validation)
  - [See Also](#see-also)

<!-- END doctoc generated TOC please keep comment here to allow auto update -->

# MCP Configuration Reference

Complete reference for MCP (Model Context Protocol) configuration files.

## Configuration Overview

MCP configuration is stored in JSON format (`.mcp.json`) and defines MCP servers, their commands, and runtime settings.

### Configuration Priority

Configuration is resolved from multiple sources in order of priority (highest to lowest):

1. **CLI arguments** - `--config` flag
2. **Environment variables** - `GGEN_MCP_*` variables
3. **Project configuration** - `.mcp.json` in current directory
4. **User configuration** - `~/.ggen/mcp/config.json`
5. **System configuration** - `/etc/ggen/mcp.json`
6. **Default values** - Built-in defaults

---

## Configuration File Schema

### Root Structure

```json
{
  "version": "1.0.0",
  "description": "MCP servers for enhanced Claude Code capabilities",
  "mcp_servers": {
    "<server_name>": { /* ServerConfig */ }
  },
  "metadata": {
    "project": "my-project",
    "version": "1.0.0",
    "purpose": "Development tools",
    "updated_at": "2024-01-15T10:30:00Z"
  }
}
```

### Root Properties

| Property | Type | Required | Default | Description |
|----------|------|----------|---------|-------------|
| `version` | string | No | `"1.0.0"` | Configuration version |
| `description` | string | No | See default | Description of this configuration |
| `mcp_servers` | object | No | `{}` | Map of server configurations |
| `metadata` | object | No | `{}` | Configuration metadata |

---

## Server Configuration

### ServerConfig Schema

```json
{
  "command": "npx",
  "args": ["@anthropic-ai/claude-code-guide"],
  "env": {
    "NODE_ENV": "production"
  },
  "cwd": "/path/to/working/directory",
  "timeout": 30,
  "enabled": true,
  "max_restarts": 3,
  "server_type": "std"
}
```

### Server Properties

| Property | Type | Required | Default | Description |
|----------|------|----------|---------|-------------|
| `command` | string | **Yes** | - | Command to execute |
| `args` | array | No | `[]` | Arguments to pass to the command |
| `env` | object | No | `{}` | Environment variables for the server |
| `cwd` | string | No | `null` | Working directory for the server |
| `timeout` | integer | No | `30` | Server timeout in seconds (must be > 0) |
| `enabled` | boolean | No | `true` | Whether the server is enabled |
| `max_restarts` | integer | No | `3` | Maximum restart attempts |
| `server_type` | string | No | `null` | Server type (`std`, `ssh`, or custom) |

---

## Metadata Schema

```json
{
  "project": "my-project",
  "version": "1.0.0",
  "purpose": "Development tools",
  "updated_at": "2024-01-15T10:30:00Z"
}
```

### Metadata Properties

| Property | Type | Required | Default | Description |
|----------|------|----------|---------|-------------|
| `project` | string | No | `null` | Project name |
| `version` | string | No | `null` | Project version |
| `purpose` | string | No | `null` | Purpose description |
| `updated_at` | string | No | `null` | ISO 8601 timestamp of last update |

---

## Complete Example

```json
{
  "version": "1.0.0",
  "description": "MCP servers for enhanced Claude Code capabilities",
  "mcp_servers": {
    "claude-code-guide": {
      "command": "npx",
      "args": ["@anthropic-ai/claude-code-guide"],
      "env": {
        "NODE_ENV": "production"
      },
      "timeout": 30,
      "enabled": true,
      "max_restarts": 3
    },
    "git": {
      "command": "npx",
      "args": ["@modelcontextprotocol/server-git", "--repository", "."],
      "cwd": "/project",
      "timeout": 60,
      "enabled": true,
      "max_restarts": 3
    },
    "bash": {
      "command": "bash",
      "args": ["--init-file", ".claude/helpers/bash-init.sh"],
      "env": {
        "CARGO_TERM_COLOR": "always"
      },
      "timeout": 30,
      "enabled": false,
      "max_restarts": 3
    },
    "filesystem": {
      "command": "npx",
      "args": ["-y", "@modelcontextprotocol/server-filesystem", "/allowed/path"],
      "timeout": 30,
      "enabled": true
    },
    "ssh-server": {
      "command": "ssh",
      "args": ["user@remote", "mcp-server"],
      "server_type": "ssh",
      "timeout": 120,
      "enabled": true
    }
  },
  "metadata": {
    "project": "ggen",
    "version": "1.0.0",
    "purpose": "Development MCP servers",
    "updated_at": "2024-01-15T10:30:00Z"
  }
}
```

---

## Environment Variable Configuration

Individual server properties can be overridden using environment variables.

### Variable Format

```
GGEN_MCP_SERVER_<NAME>_<PROPERTY>=<value>
```

### Supported Properties

| Property | Environment Variable | Example |
|----------|---------------------|---------|
| Command | `GGEN_MCP_SERVER_<NAME>_COMMAND` | `GGEN_MCP_SERVER_MY_SERVER_COMMAND=npx` |
| Args | `GGEN_MCP_SERVER_<NAME>_ARGS` | `GGEN_MCP_SERVER_MY_SERVER_ARGS=--verbose` |
| Timeout | `GGEN_MCP_SERVER_<NAME>_TIMEOUT` | `GGEN_MCP_SERVER_MY_SERVER_TIMEOUT=60` |

### Environment Configuration Example

```bash
# Configure a server via environment variables
export GGEN_MCP_SERVER_CLAUDE_COMMAND="npx"
export GGEN_MCP_SERVER_CLAUDE_ARGS="@anthropic-ai/claude-code-guide --verbose"
export GGEN_MCP_SERVER_CLAUDE_TIMEOUT="60"

# Alternative: Use a config file path
export GGEN_MCP_CONFIG="/custom/path/config.json"
```

---

## Validation Rules

### Server Validation

Each server configuration is validated against the following rules:

| Rule | Description | Error |
|------|-------------|-------|
| Non-empty command | Command cannot be empty | `EmptyCommand` |
| Valid timeout | Timeout must be > 0 | `InvalidTimeout` |
| Safe command | Command cannot contain dangerous patterns | `DangerousCommand` |

### Dangerous Command Detection

Commands containing the following patterns are rejected:

- `rm -rf`
- `mkfs`
- `format`
- `del /f`

---

## Default Values

### Default Server Configuration

```json
{
  "command": "",
  "args": [],
  "env": {},
  "cwd": null,
  "timeout": 30,
  "enabled": true,
  "max_restarts": 3,
  "server_type": null
}
```

### Default Root Configuration

```json
{
  "version": "1.0.0",
  "description": "MCP (Model Context Protocol) servers for enhanced Claude Code capabilities",
  "mcp_servers": {},
  "metadata": {}
}
```

---

## File Locations

| Location | Platform | Path |
|----------|----------|------|
| Project | All | `.mcp.json` |
| User | All | `~/.ggen/mcp/config.json` |
| System | Unix/Linux | `/etc/ggen/mcp.json` |
| System | Windows | `%PROGRAMDATA%\ggen\mcp.json` |

### PID and Lock Files

| File | Purpose |
|------|---------|
| `.ggen/mcp/server.pid` | Running server process ID |
| `.ggen/mcp/server.lock` | Server operation lock |

---

## Common Server Examples

### Claude Code Guide

```json
{
  "claude-code-guide": {
    "command": "npx",
    "args": ["@anthropic-ai/claude-code-guide"],
    "timeout": 30,
    "enabled": true
  }
}
```

### Git Server

```json
{
  "git": {
    "command": "npx",
    "args": ["@modelcontextprotocol/server-git", "--repository", "."],
    "cwd": "/project",
    "timeout": 60,
    "enabled": true
  }
}
```

### Bash Server

```json
{
  "bash": {
    "command": "bash",
    "args": ["--init-file", ".claude/helpers/bash-init.sh"],
    "env": {
      "CARGO_TERM_COLOR": "always"
    },
    "timeout": 30,
    "enabled": true
  }
}
```

### Filesystem Server

```json
{
  "filesystem": {
    "command": "npx",
    "args": ["-y", "@modelcontextprotocol/server-filesystem", "/allowed/path"],
    "timeout": 30,
    "enabled": true
  }
}
```

### SSH Remote Server

```json
{
  "remote-server": {
    "command": "ssh",
    "args": ["user@remote", "mcp-server"],
    "server_type": "ssh",
    "timeout": 120,
    "enabled": true
  }
}
```

### Python Server

```json
{
  "python-server": {
    "command": "uvx",
    "args": ["mcp-server-python"],
    "env": {
      "PYTHONPATH": "/project/src"
    },
    "timeout": 60,
    "enabled": true
  }
}
```

---

## Configuration Validation

### Validate with CLI

```bash
# Validate default configuration
ggen mcp config validate

# Validate specific file
ggen mcp config validate --file /path/to/config.json

# Show detailed results
ggen mcp config validate --verbose
```

### Programmatic Validation

```rust
use ggen_domain::mcp_config::{load_mcp_from_file, validate_mcp_config};

// Load configuration
let config = load_mcp_from_file(&PathBuf::from(".mcp.json"))?;

// Validate configuration
let results = validate_mcp_config(&config)?;

// Check results
for result in results {
    if !result.is_valid {
        eprintln!("Server '{}' has errors:", result.server_name);
        for error in &result.errors {
            eprintln!("  - {}", error);
        }
    }
}
```

---

## See Also

- [MCP Command Reference](../commands/mcp.md)
- [A2A Configuration Reference](./a2a-config.md)
- [MCP A2A Integration Guide](../MCP_A2A_INTEGRATION.md)
