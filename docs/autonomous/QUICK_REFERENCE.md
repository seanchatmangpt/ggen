<!-- START doctoc generated TOC please keep comment here to allow auto update -->
<!-- DON'T EDIT THIS SECTION, INSTEAD RE-RUN doctoc TO UPDATE -->
**Table of Contents**

- [ggen Autonomous System - Quick Reference](#ggen-autonomous-system---quick-reference)
  - [Common Commands](#common-commands)
    - [Setup & Configuration](#setup--configuration)
    - [Template Generation](#template-generation)
    - [Graph Operations](#graph-operations)
    - [Project Management](#project-management)
    - [Monitoring & Debugging](#monitoring--debugging)
  - [Configuration Templates](#configuration-templates)
    - [Basic Configuration](#basic-configuration)
    - [Production Configuration](#production-configuration)
    - [Development Configuration](#development-configuration)
  - [Troubleshooting Checklist](#troubleshooting-checklist)
    - [Template Generation Issues](#template-generation-issues)
    - [MCP Server Issues](#mcp-server-issues)
    - [Graph Loading Issues](#graph-loading-issues)
    - [Performance Issues](#performance-issues)
  - [Error Code Reference](#error-code-reference)
  - [Key Keyboard Shortcuts](#key-keyboard-shortcuts)
  - [Environment Variables](#environment-variables)
  - [Performance Tips](#performance-tips)
  - [Support Resources](#support-resources)
  - [Quick Links](#quick-links)

<!-- END doctoc generated TOC please keep comment here to allow auto update -->

# ggen Autonomous System - Quick Reference

## Common Commands

### Setup & Configuration

```bash
# Install
cargo install --path .

# Set up environment
export ANTHROPIC_API_KEY="your-key"
export OPENAI_API_KEY="your-key"

# Start MCP server
ggen-mcp --port 8080 --agents 12

# Check status
ggen-mcp status
```

### Template Generation

```bash
# Basic generation
ggen ai generate -d "REST API endpoint" -o api.rs

# With validation
ggen ai generate -d "User authentication" --validate -o auth.rs

# Multiple iterations
ggen ai generate -d "Complex feature" --validate --max-iterations 5

# Different formats
ggen ai generate -d "Database schema" --format sql -o schema.sql
```

### Graph Operations

```bash
# Generate ontology
ggen ai graph -d "E-commerce domain" -o ecommerce.ttl --verify

# Generate SPARQL query
ggen ai sparql -d "Find all users" -g users.ttl -o query.sparql

# Validate graph
ggen graph validate ontologies/domain.ttl

# Execute query
ggen graph query -q queries/users.sparql -g data/users.ttl
```

### Project Management

```bash
# Create project
ggen ai project -d "Microservice" -n my-service --validate

# List templates
ggen market search --tags rest,api

# Install template
ggen market install rest-api-template

# Use template
ggen gen --template rest-api-template --output my-api/
```

### Monitoring & Debugging

```bash
# View logs
ggen-mcp logs --tail 100 --follow

# Check agent health
ggen-mcp agent status --all

# Performance metrics
ggen-mcp metrics --last 24h

# Audit trail
ggen-mcp audit tail --lines 50
```

## Configuration Templates

### Basic Configuration

```toml
# ~/.config/ggen/config.toml

[ai]
provider = "anthropic"
model = "claude-3-opus-20240229"
timeout_seconds = 30

[validation]
enabled = true
min_score = 0.80
max_iterations = 3

[cache]
enabled = true
max_size_mb = 1024
ttl_seconds = 3600
```

### Production Configuration

```toml
# config/production.toml

[policies]
enforcement_mode = "strict"
audit_all_operations = true

[agents]
count = 12
topology = "mesh"
enable_byzantine_tolerance = true

[monitoring]
enabled = true
health_check_interval_seconds = 30

[security]
input_validation = true
secret_scanning = true
require_approval_for_destructive = true
```

### Development Configuration

```toml
# config/development.toml

[ai]
provider = "ollama"
model = "qwen2.5-coder"

[policies]
enforcement_mode = "permissive"

[validation]
enabled = false  # Faster iteration

[cache]
enabled = false  # Always fresh
```

## Troubleshooting Checklist

### Template Generation Issues

- [ ] API key set in environment
- [ ] Provider accessible (check `ggen ai config validate`)
- [ ] Sufficient disk space for cache
- [ ] No rate limiting (check provider dashboard)
- [ ] Description is clear and specific

### MCP Server Issues

- [ ] Port not already in use
- [ ] Sufficient system resources
- [ ] Configuration file valid
- [ ] All dependencies installed
- [ ] Logs checked for errors

### Graph Loading Issues

- [ ] File path correct
- [ ] Format specified correctly
- [ ] RDF syntax valid
- [ ] Sufficient memory
- [ ] No file permission issues

### Performance Issues

- [ ] Cache enabled
- [ ] Appropriate number of agents
- [ ] Resource limits not exceeded
- [ ] Network latency acceptable
- [ ] Database optimized

## Error Code Reference

| Code | Error | Solution |
|------|-------|----------|
| 1001 | API_KEY_MISSING | Set API key in environment |
| 1003 | RATE_LIMIT | Wait or upgrade plan |
| 1101 | VALIDATION_FAILED | Increase max iterations |
| 1201 | GRAPH_LOAD_ERROR | Check file format |
| 1304 | CONSENSUS_FAILED | Check agent health |
| 1403 | INVALID_PROJECT_NAME | Use valid characters |

## Key Keyboard Shortcuts

```bash
# In interactive mode
Ctrl+C  - Cancel operation
Ctrl+D  - Exit
Ctrl+R  - Retry last
Ctrl+L  - Clear screen
```

## Environment Variables

```bash
# AI Providers
ANTHROPIC_API_KEY       # Anthropic Claude API key
OPENAI_API_KEY          # OpenAI GPT API key
OLLAMA_BASE_URL         # Ollama server URL (default: http://localhost:11434)

# Configuration
GGEN_CONFIG_PATH        # Config file path
GGEN_CACHE_DIR          # Cache directory
GGEN_CACHE_ENABLED      # Enable cache (true/false)

# Logging
RUST_LOG                # Log level (debug, info, warn, error)
GGEN_LOG_FILE           # Log file path

# MCP
GGEN_MCP_PORT           # MCP server port
GGEN_MCP_HOST           # MCP server host
```

## Performance Tips

1. **Use cache**: Enable cache for 3-5x speedup
2. **Local models**: Use Ollama for faster iteration
3. **Batch operations**: Process multiple items together
4. **Appropriate validation**: Balance quality vs speed
5. **Resource limits**: Set appropriate limits for your system

## Support Resources

- Documentation: [https://github.com/seanchatmangpt/ggen/tree/master/docs](https://github.com/seanchatmangpt/ggen/tree/master/docs)
- Issues: [https://github.com/seanchatmangpt/ggen/issues](https://github.com/seanchatmangpt/ggen/issues)
- Discussions: [https://github.com/seanchatmangpt/ggen/discussions](https://github.com/seanchatmangpt/ggen/discussions)

## Quick Links

- [User Guide](USER_GUIDE.md) - Comprehensive user documentation
- [Architecture Deep Dive](ARCHITECTURE_DEEP_DIVE.md) - System architecture
- [API Reference](API_REFERENCE.md) - API documentation
- [Governance Guide](GOVERNANCE_GUIDE.md) - Policy and security
- [Migration Guide](MIGRATION_GUIDE.md) - Migration procedures
