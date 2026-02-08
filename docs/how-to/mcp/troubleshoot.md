<!-- START doctoc generated TOC please keep comment here to allow auto update -->
<!-- DON'T EDIT THIS SECTION, INSTEAD RERUN doctoc TO UPDATE -->
**Table of Contents**

- [MCP & A2A Troubleshooting Reference](#mcp--a2a-troubleshooting-reference)
  - [Quick Diagnostics](#quick-diagnostics)
    - [Health Check Commands](#health-check-commands)
    - [Log Locations](#log-locations)
    - [Configuration Validation](#configuration-validation)
    - [Connectivity Testing](#connectivity-testing)
  - [Common Issues by Component](#common-issues-by-component)
    - [MCP Server Issues](#mcp-server-issues)
      - [Server Won't Start](#server-wont-start)
      - [Server Starts But Tools Not Available](#server-starts-but-tools-not-available)
      - [Server Stops Unexpectedly](#server-stops-unexpectedly)
      - [Stdio Transport Issues](#stdio-transport-issues)
      - [HTTP Transport Issues](#http-transport-issues)
      - [WebSocket Connection Drops](#websocket-connection-drops)
    - [A2A Agent Issues](#a2a-agent-issues)
      - [Agent Registration Fails](#agent-registration-fails)
      - [Agent Not Responding](#agent-not-responding)
      - [Agent Task Timeout](#agent-task-timeout)
      - [Agent Message Conversion Fails](#agent-message-conversion-fails)
      - [Agent Bridge to MCP Fails](#agent-bridge-to-mcp-fails)
    - [Configuration Issues](#configuration-issues)
      - [Config File Not Found](#config-file-not-found)
      - [Config Priority Confusion](#config-priority-confusion)
      - [Environment Variables Not Applied](#environment-variables-not-applied)
      - [TLS Certificate Errors](#tls-certificate-errors)
      - [Hot Reload Not Working](#hot-reload-not-working)
    - [Network Issues](#network-issues)
      - [Connection Refused](#connection-refused)
      - [Connection Timeout](#connection-timeout)
      - [DNS Resolution Failures](#dns-resolution-failures)
      - [Firewall Blocking](#firewall-blocking)
      - [Proxy Configuration](#proxy-configuration)
    - [Performance Issues](#performance-issues)
      - [Slow Tool Discovery](#slow-tool-discovery)
      - [High Memory Usage](#high-memory-usage)
      - [Message Latency](#message-latency)
      - [Tool Execution Timeout](#tool-execution-timeout)
      - [Connection Pool Exhaustion](#connection-pool-exhaustion)
  - [Debugging Techniques](#debugging-techniques)
    - [Verbose Logging](#verbose-logging)
    - [Debug Mode](#debug-mode)
    - [Tracing](#tracing)
    - [Message Inspection](#message-inspection)
    - [Protocol Analysis](#protocol-analysis)
    - [Performance Profiling](#performance-profiling)
  - [Error Reference](#error-reference)
    - [JSON-RPC Standard Errors (-32700 to -32603)](#json-rpc-standard-errors--32700-to--32603)
    - [MCP Transport Errors (-32100 to -32106)](#mcp-transport-errors--32100-to--32106)
    - [A2A Conversion Errors](#a2a-conversion-errors)
    - [Registration Errors](#registration-errors)
    - [Configuration Errors](#configuration-errors)
    - [CLI Exit Codes](#cli-exit-codes)

<!-- END doctoc generated TOC -->

# MCP & A2A Troubleshooting Reference

Comprehensive troubleshooting guide for MCP (Model Context Protocol) and A2A (Agent-to-Agent) integration issues.

## Quick Diagnostics

### Health Check Commands

**Problem:** You need to quickly assess the health of MCP and A2A systems.

**Solution:** Run these diagnostic commands to get an overview.

```bash
# Overall system health
ggen mcp doctor

# MCP server status
ggen mcp server status

# A2A agent status
ggen agent status --all

# Configuration validation
ggen mcp config validate
ggen a2a config validate

# Connectivity test
ggen mcp test-connection
ggen a2a test-connection

# Tool discovery health
ggen mcp tools --health
```

**Expected healthy output:**
```
$ ggen mcp doctor

MCP System Health Check
=======================

MCP Servers: 2/2 healthy
  rig-mcp (stdio)    : OK
  http-server (http) : OK

A2A Agents: 3/3 healthy
  text-generator     : ready (http://localhost:8080)
  code-analyzer      : ready (http://localhost:8081)
  workflow-agent     : ready (http://localhost:8082)

Configuration: Valid
  Config file: /Users/sac/ggen/.mcp.json
  Sources: CLI args, Environment, Project config

Connectivity: All transports operational
Issues: None detected
```

### Log Locations

**Problem:** You need to find the logs to investigate issues.

**Solution:** Logs are stored in these locations.

| Component | Log Location | Description |
|-----------|--------------|-------------|
| MCP Server | `~/.ggen/logs/mcp/` | MCP server operations |
| A2A Agents | `~/.ggen/logs/agents/` | Agent activity logs |
| Configuration | `~/.ggen/logs/config/` | Config loading/resolution |
| Transport | `~/.ggen/logs/transport/` | Transport layer logs |
| Integration | `~/.ggen/logs/integration/` | MCP-A2A bridge logs |

**View recent logs:**
```bash
# MCP server logs
tail -100 ~/.ggen/logs/mcp/mcp-server.log

# A2A agent logs
tail -100 ~/.ggen/logs/agents/*.log

# All logs with errors
grep -r "error\|Error\|ERROR" ~/.ggen/logs/

# Logs from last hour
find ~/.ggen/logs/ -mmin -60 -type f -exec tail -20 {} +
```

### Configuration Validation

**Problem:** Configuration may be invalid or conflicting.

**Solution:** Validate your configuration files.

```bash
# Validate MCP configuration
ggen mcp config validate --verbose

# Validate A2A configuration
ggen a2a config validate --verbose

# Show effective configuration (after priority resolution)
ggen mcp config show --sources
ggen a2a config show --sources

# Test configuration without applying
ggen mcp config test --file /path/to/config.json
```

**Common validation errors:**
```json
// Error: Empty command
{
  "error": "Empty command in server configuration",
  "file": ".mcp.json",
  "line": 15,
  "suggestion": "Ensure 'command' field is non-empty"
}

// Error: Invalid timeout
{
  "error": "Invalid timeout: 0",
  "file": ".mcp.json",
  "suggestion": "Timeout must be greater than 0"
}

// Error: TLS misconfiguration
{
  "error": "TLS is enabled but certificate or key path is missing",
  "file": "a2a.toml",
  "suggestion": "Set tls_cert_path and tls_key_path when tls_enabled is true"
}
```

### Connectivity Testing

**Problem:** You need to test if MCP/A2A endpoints are reachable.

**Solution:** Use these connectivity tests.

```bash
# Test MCP server connectivity
ggen mcp test-connection --server http://localhost:3000

# Test A2A server connectivity
ggen a2a test-connection --server http://localhost:8080

# Test with specific transport
ggen mcp test-connection --transport stdio
ggen mcp test-connection --transport websocket

# Test protocol compliance
ggen mcp test-protocol --server http://localhost:3000

# Ping test (for HTTP/WebSocket)
curl -f http://localhost:3000/health
wscat -c ws://localhost:3000/ws
```

## Common Issues by Component

### MCP Server Issues

#### Server Won't Start

**Symptom:** `ggen mcp server start` fails with an error.

**Diagnostic Steps:**
```bash
# Check if another instance is running
ggen mcp server status

# Check port availability
netstat -an | grep 3000  # macOS
lsof -i :3000            # Alternative

# Check PID file
cat ~/.ggen/mcp/server.pid

# Try with verbose output
ggen mcp server start --verbose --debug
```

**Common Causes and Solutions:**

1. **Port already in use**
   ```bash
   # Find process using the port
   lsof -ti :3000

   # Kill the process
   kill -9 $(lsof -ti :3000)

   # Or use a different port
   ggen mcp server start --port 3001
   ```

2. **Configuration file has errors**
   ```bash
   # Validate configuration
   ggen mcp config validate

   # Check for JSON syntax
   cat .mcp.json | jq .

   # Fix common issues:
   # - Missing commas
   # - Trailing commas
   # - Unquoted keys
   # - Invalid escape sequences
   ```

3. **Missing dependencies**
   ```bash
   # Check if command exists
   which npx
   which uvx

   # Install missing dependencies
   npm install -g @anthropic-ai/claude-code-guide
   ```

4. **Permission denied**
   ```bash
   # Check file permissions
   ls -la ~/.ggen/mcp/

   # Fix permissions
   chmod 755 ~/.ggen/mcp/
   chmod 644 ~/.ggen/mcp/config.json
   ```

**Prevention Tips:**
- Always validate configuration before starting
- Use unique ports for different environments
- Set appropriate file permissions
- Use process managers (systemd, launchd) for production

#### Server Starts But Tools Not Available

**Symptom:** Server starts successfully but `ggen mcp list` returns no tools.

**Diagnostic Steps:**
```bash
# Check server status
ggen mcp server status --verbose

# Test tool discovery
ggen mcp tools --discovery

# Check server logs
tail -100 ~/.ggen/logs/mcp/mcp-server.log | grep -i tool

# Verify tool registration
ggen mcp tools --registered
```

**Common Causes and Solutions:**

1. **Tool discovery timeout**
   ```bash
   # Increase timeout
   ggen mcp config set discovery.timeout_secs=60

   # Check slow servers
   ggen mcp test-connection --all
   ```

2. **Invalid tool schema**
   ```bash
   # Validate tool schemas
   ggen mcp tools --validate

   # Check schema format
   ggen mcp tools --schema tool-name
   ```

3. **Permission issues**
   ```bash
   # Check tool permissions
   ggen mcp tools --permissions

   # Grant required permissions
   ggen mcp config set tools.permissions=full
   ```

4. **Server not advertising tools**
   ```bash
   # Force tool refresh
   ggen mcp tools --refresh

   # Clear tool cache
   ggen mcp tools --clear-cache
   ```

**Prevention Tips:**
- Set appropriate discovery timeouts for your network
- Validate tool schemas before deployment
- Use tool caching for improved performance
- Monitor tool availability with health checks

#### Server Stops Unexpectedly

**Symptom:** Server starts but stops after a short time.

**Diagnostic Steps:**
```bash
# Check server logs for crashes
tail -200 ~/.ggen/logs/mcp/mcp-server.log

# Check system logs
dmesg | tail -100    # Linux
log show --predicate 'process == "ggen"' --last 10m  # macOS

# Check for core dumps
ls -la /cores/        # macOS
ls -la /var/crash/    # Linux
```

**Common Causes and Solutions:**

1. **Out of memory**
   ```bash
   # Check memory usage
   ggen mcp server status --metrics

   # Increase memory limit
   ggen mcp server start --memory-limit 4G
   ```

2. **Unhandled panic/error**
   ```bash
   # Check for Rust panics
   RUST_BACKTRACE=1 ggen mcp server start --debug

   # Report with backtrace
   ggen issue report --attach ~/.ggen/logs/mcp/mcp-server.log
   ```

3. **Signal received**
   ```bash
   # Check for termination signals
   grep -i "signal\|terminated" ~/.ggen/logs/mcp/mcp-server.log

   # Ensure proper signal handling
   ggen mcp server start --graceful-shutdown-timeout 30
   ```

**Prevention Tips:**
- Set appropriate resource limits
- Enable graceful shutdown
- Monitor server health
- Use process supervisors for auto-restart

#### Stdio Transport Issues

**Symptom:** Stdio-based MCP servers not working correctly.

**Diagnostic Steps:**
```bash
# Test stdio transport
echo '{"jsonrpc":"2.0","id":1,"method":"tools/list"}' | npx @anthropic-ai/claude-code-guide

# Check for buffering issues
stdbuf -o0 -e0 npx @anthropic-ai/claude-code-guide

# Verify command
which npx
npx --version
```

**Common Causes and Solutions:**

1. **Buffering issues**
   ```bash
   # Disable buffering
   ggen mcp config set stdio.buffered=false

   # Use unbuffered I/O
   ggen mcp server start --stdio-unbuffered
   ```

2. **Path issues**
   ```bash
   # Use absolute paths
   ggen mcp config set servers.myserver.command=/usr/local/bin/node

   # Verify executable
   ls -la $(which npx)
   ```

3. **Environment not passed**
   ```bash
   # Pass environment variables
   ggen mcp config set servers.myserver.env.PATH=/usr/local/bin:$PATH
   ```

#### HTTP Transport Issues

**Symptom:** HTTP-based MCP servers not accessible.

**Diagnostic Steps:**
```bash
# Test HTTP endpoint
curl -v http://localhost:3000/health

# Check with verbose output
ggen mcp server status --verbose

# Test tools endpoint
curl -X POST http://localhost:3000/tools/list \
  -H "Content-Type: application/json" \
  -d '{"jsonrpc":"2.0","id":1,"method":"tools/list"}'
```

**Common Causes and Solutions:**

1. **Wrong URL or port**
   ```bash
   # Verify server URL
   ggen mcp config show | grep -A5 http

   # Update configuration
   ggen mcp config set transports.http.port=3000
   ggen mcp config set transports.http.host=0.0.0.0
   ```

2. **CORS issues**
   ```bash
   # Enable CORS
   ggen mcp config set transports.http.cors_enabled=true
   ggen mcp config set transports.http.cors_origin="*"
   ```

3. **SSL/TLS errors**
   ```bash
   # Disable SSL verification (for testing)
   ggen mcp config set transports.http.ssl_verify=false

   # Or fix certificates
   ggen mcp config set transports.http.ssl_cert=/path/to/cert.pem
   ```

#### WebSocket Connection Drops

**Symptom:** WebSocket connections frequently disconnect.

**Diagnostic Steps:**
```bash
# Monitor WebSocket connections
ggen mcp server status --websockets

# Test WebSocket stability
wscat -c ws://localhost:3000/ws --ping-interval 10

# Check keepalive settings
ggen mcp config show | grep -A5 websocket
```

**Common Causes and Solutions:**

1. **No keepalive**
   ```bash
   # Enable ping/pong
   ggen mcp config set transports.websocket.ping_interval_secs=30
   ggen mcp config set transports.websocket.ping_timeout_secs=10
   ```

2. **Message size too large**
   ```bash
   # Increase max message size
   ggen mcp config set transports.websocket.max_message_size=10485760
   ```

3. **Proxy timeout**
   ```bash
   # Increase proxy timeout
   ggen mcp config set transports.websocket.proxy_timeout_secs=300
   ```

### A2A Agent Issues

#### Agent Registration Fails

**Symptom:** Cannot register A2A agent with MCP.

**Diagnostic Steps:**
```bash
# Check agent status
ggen agent status agent-name

# Test MCP bridge
ggen mcp bridge agent-name --test

# Check bridge logs
tail -100 ~/.ggen/logs/integration/bridge.log
```

**Common Causes and Solutions:**

1. **Agent not running**
   ```bash
   # Start agent first
   ggen agent start agent-name

   # Wait for ready state
   ggen agent start agent-name --wait
   ```

2. **Agent endpoint not accessible**
   ```bash
   # Test agent endpoint
   curl http://localhost:8080/health

   # Update agent URL in config
   ggen mcp config set agents.agent-name.url=http://localhost:8080
   ```

3. **Duplicate registration**
   ```bash
   # Check existing registrations
   ggen mcp tools --registered | grep agent-name

   # Unregister first
   ggen mcp bridge agent-name --unregister
   ```

#### Agent Not Responding

**Symptom:** Agent is registered but doesn't respond to requests.

**Diagnostic Steps:**
```bash
# Check agent health
ggen agent health agent-name

# Check agent metrics
ggen agent status agent-name --metrics

# Test agent directly
curl -X POST http://localhost:8080/tasks \
  -H "Content-Type: application/json" \
  -d '{"task":"test"}'
```

**Common Causes and Solutions:**

1. **Agent overloaded**
   ```bash
   # Check agent load
   ggen agent status agent-name --metrics | grep load

   # Scale agent
   ggen agent start agent-name-2 --clone-of agent-name
   ```

2. **Message conversion error**
   ```bash
   # Check conversion logs
   tail -100 ~/.ggen/logs/integration/conversion.log | grep error

   # Test conversion
   ggen mcp test-conversion --agent agent-name
   ```

3. **Network issue**
   ```bash
   # Test connectivity
   ggen a2a test-connection --server http://localhost:8080

   # Check firewall
   sudo ufw status
   ```

#### Agent Task Timeout

**Symptom:** Agent accepts tasks but times out before completion.

**Diagnostic Steps:**
```bash
# Check task status
ggen agent tasks agent-name

# Check timeout settings
ggen agent config agent-name | grep timeout

# Monitor long-running tasks
ggen agent tasks agent-name --watch
```

**Common Causes and Solutions:**

1. **Timeout too short**
   ```bash
   # Increase agent timeout
   ggen agent config agent-name --timeout 120

   # Set per-task timeout
   ggen agent config agent-name --task-timeout 300
   ```

2. **Task actually stuck**
   ```bash
   # Cancel stuck task
   ggen agent task-cancel agent-name task-id

   # Restart agent
   ggen agent restart agent-name
   ```

3. **Resource exhaustion**
   ```bash
   # Check resources
   ggen agent status agent-name --resources

   # Increase limits
   ggen agent start agent-name --memory-limit 8G --cpu-limit 4
   ```

#### Agent Message Conversion Fails

**Symptom:** Errors when converting between MCP and A2A formats.

**Diagnostic Steps:**
```bash
# Check conversion logs
tail -100 ~/.ggen/logs/integration/conversion.log

# Test conversion with example
ggen mcp test-conversion \
  --from mcp \
  --to a2a \
  --message '{"method":"tools/call","params":{"name":"test"}}'

# Validate schemas
ggen mcp tools --validate-schemas
```

**Common Causes and Solutions:**

1. **Schema mismatch**
   ```bash
   # Show tool schema
   ggen mcp tools --schema tool-name

   # Update conversion rules
   ggen mcp config set conversion.strict=false
   ```

2. **Missing required fields**
   ```bash
   # Show conversion rules
   ggen mcp config show | grep -A20 conversion

   # Add field mapping
   ggen mcp config set conversion.field_mappings.extra_field=target_field
   ```

3. **Type incompatibility**
   ```bash
   # Enable type coercion
   ggen mcp config set conversion.type_coercion=true

   # Set strict mode off for testing
   ggen mcp config set conversion.strict=false
   ```

#### Agent Bridge to MCP Fails

**Symptom:** Cannot bridge A2A agent as MCP tool.

**Diagnostic Steps:**
```bash
# Test bridge
ggen mcp bridge agent-name --test

# Check bridge status
ggen mcp bridge --status

# Verify agent capabilities
ggen agent capabilities agent-name
```

**Common Causes and Solutions:**

1. **Agent doesn't expose capabilities**
   ```bash
   # Define agent capabilities
   ggen agent config agent-name --capabilities "tool1,tool2"

   # Set default capability
   ggen agent config agent-name --default-capability "generic"
   ```

2. **Bridge configuration error**
   ```bash
   # Reset bridge configuration
   ggen mcp bridge agent-name --reset

   # Reconfigure bridge
   ggen mcp bridge agent-name --tool-name "custom-tool-name"
   ```

3. **Permission denied**
   ```bash
   # Check bridge permissions
   ggen mcp bridge --permissions

   # Grant permission
   ggen mcp config set bridge.allow_agent_bridging=true
   ```

### Configuration Issues

#### Config File Not Found

**Symptom:** `Configuration file not found` error.

**Diagnostic Steps:**
```bash
# Show config search paths
ggen mcp config --show-paths

# Check which config is being used
ggen mcp config show --sources

# List existing config files
find . -name ".mcp.json" -o -name "a2a.toml"
```

**Common Causes and Solutions:**

1. **Config in wrong location**
   ```bash
   # Create config in project directory
   cp ~/.ggen/mcp/config.json .mcp.json

   # Or specify config file
   ggen mcp --config /path/to/config.json
   ```

2. **Wrong file name**
   ```bash
   # Valid file names:
   # - .mcp.json (project level)
   # - a2a.toml (project level)
   # - ~/.ggen/mcp/config.json (user level)

   # Rename file if needed
   mv mcp-config.json .mcp.json
   ```

3. **Not in project directory**
   ```bash
   # Change to project directory
   cd /path/to/project

   # Or use --project-dir flag
   ggen mcp --project-dir /path/to/project
   ```

#### Config Priority Confusion

**Symptom:** Configuration values not what you expected.

**Diagnostic Steps:**
```bash
# Show effective configuration with sources
ggen mcp config show --sources

# Show all config files being loaded
ggen mcp config --all-files

# Trace configuration resolution
ggen mcp config --trace-resolution
```

**Configuration Priority (highest to lowest):**
1. CLI arguments (`--config`, `--port`, etc.)
2. Environment variables (`GGEN_MCP_*`, `GGEN_A2A_*`)
3. Project configuration (`./.mcp.json`, `./a2a.toml`)
4. User configuration (`~/.ggen/mcp/config.json`, `~/.ggen/a2a.toml`)
5. System configuration (`/etc/ggen/mcp.json`, `/etc/ggen/a2a.toml`)
6. Built-in defaults

**Common Issues:**

1. **Environment variable overriding config**
   ```bash
   # Check env vars
   env | grep GGEN_MCP
   env | grep GGEN_A2A

   # Unset conflicting env var
   unset GGEN_MCP_PORT
   ```

2. **Multiple config files**
   ```bash
   # See which file is being used
   ggen mcp config show --sources | grep "Source:"

   # Disable lower priority configs
   ggen mcp --no-user-config --no-system-config
   ```

#### Environment Variables Not Applied

**Symptom:** Environment variables not affecting configuration.

**Diagnostic Steps:**
```bash
# Check if env vars are set
env | grep GGEN_MCP
env | grep GGEN_A2A

# Test env var loading
ggen mcp config --from-env

# Show env-derived config
ggen mcp config show --env-only
```

**Common Causes and Solutions:**

1. **Wrong variable name**
   ```bash
   # Correct format:
   # GGEN_MCP_SERVER_<NAME>_COMMAND
   # GGEN_MCP_SERVER_<NAME>_ARGS
   # GGEN_MCP_SERVER_<NAME>_TIMEOUT
   # GGEN_A2A_HOST
   # GGEN_A2A_PORT

   # Example:
   export GGEN_MCP_SERVER_MYSERVER_COMMAND="npx"
   export GGEN_MCP_SERVER_MYSERVER_ARGS="@anthropic-ai/claude-code-guide"
   ```

2. **Variable not exported**
   ```bash
   # Wrong (not exported):
   GGEN_MCP_PORT=3000

   # Correct:
   export GGEN_MCP_PORT=3000
   ```

3. **Type mismatch**
   ```bash
   # Numbers must be valid
   export GGEN_MCP_TIMEOUT=30    # Correct
   export GGEN_MCP_TIMEOUT="30"  # Also correct

   # Booleans use "true"/"false"
   export GGEN_MCP_TLS_ENABLED="true"
   ```

#### TLS Certificate Errors

**Symptom:** SSL/TLS certificate validation failures.

**Diagnostic Steps:**
```bash
# Test SSL connection
openssl s_client -connect localhost:3443 -showcerts

# Check certificate validity
openssl x509 -in cert.pem -noout -dates

# Verify certificate chain
openssl verify -CAfile ca.pem cert.pem
```

**Common Causes and Solutions:**

1. **Self-signed certificate**
   ```bash
   # Disable verification (for testing only)
   ggen mcp config set transports.http.ssl_verify=false

   # Or add to trust store
   sudo security add-trusted-cert -d -r trustRoot -k /Library/Keychains/System.keychain cert.pem
   ```

2. **Expired certificate**
   ```bash
   # Check expiration
   openssl x509 -in cert.pem -noout -dates

   # Generate new certificate
   ggen mcp generate-cert --host localhost
   ```

3. **Hostname mismatch**
   ```bash
   # Regenerate with correct hostname
   ggen mcp generate-cert --host $(hostname)

   # Or disable hostname verification
   ggen mcp config set transports.http.ssl_verify_host=false
   ```

#### Hot Reload Not Working

**Symptom:** Configuration changes not applied without restart.

**Diagnostic Steps:**
```bash
# Check if watcher is running
ggen mcp config --watcher-status

# Test config reload
ggen mcp config --reload

# Check watcher logs
tail -100 ~/.ggen/logs/config/watcher.log
```

**Common Causes and Solutions:**

1. **File watcher not enabled**
   ```bash
   # Enable hot reload
   ggen mcp config set hot_reload.enabled=true

   # Set poll interval (if inotify not available)
   ggen mcp config set hot_reload.poll_interval_secs=5
   ```

2. **File not in watched directory**
   ```bash
   # Show watched paths
   ggen mcp config --watched-paths

   # Add path to watch list
   ggen mcp config add watch-path /custom/path
   ```

3. **Debounce too long**
   ```bash
   # Reduce debounce time
   ggen mcp config set hot_reload.debounce_ms=1000
   ```

### Network Issues

#### Connection Refused

**Symptom:** `Connection refused` when connecting to MCP/A2A.

**Diagnostic Steps:**
```bash
# Check if server is running
ggen mcp server status
ggen agent status --all

# Check if port is listening
netstat -an | grep 3000
lsof -i :3000

# Test with curl
curl -v http://localhost:3000/health

# Check firewall
sudo pfctl -sr | grep 3000  # macOS
sudo iptables -L -n | grep 3000  # Linux
```

**Common Causes and Solutions:**

1. **Server not running**
   ```bash
   # Start server
   ggen mcp server start

   # Start agent
   ggen agent start agent-name
   ```

2. **Wrong port**
   ```bash
   # Find correct port
   ggen mcp server status | grep Port

   # Update config
   ggen mcp config set transports.http.port=3001
   ```

3. **Firewall blocking**
   ```bash
   # macOS: Allow port
   sudo pfctl -e
   echo "pass in quick proto tcp from any to any port 3000" | sudo pfctl -f -

   # Linux: Allow port
   sudo ufw allow 3000/tcp
   ```

#### Connection Timeout

**Symptom:** Connections time out before completing.

**Diagnostic Steps:**
```bash
# Test with timeout
curl --max-time 30 http://localhost:3000/health

# Check network latency
ping -c 10 localhost

# Measure time to connect
time curl http://localhost:3000/health
```

**Common Causes and Solutions:**

1. **Timeout too short**
   ```bash
   # Increase timeout
   ggen mcp config set transports.http.timeout_secs=60
   ggen mcp config set transports.http.connection_timeout_secs=30
   ```

2. **Server overloaded**
   ```bash
   # Check server load
   ggen mcp server status --metrics

   # Scale up
   ggen mcp server start --workers 4
   ```

3. **Network issues**
   ```bash
   # Check routing
   netstat -rn

   # Check DNS
   nslookup localhost

   # Test with larger timeout
   ggen mcp test-connection --timeout 120
   ```

#### DNS Resolution Failures

**Symptom:** Cannot resolve server hostname.

**Diagnostic Steps:**
```bash
# Check DNS resolution
nslookup server.example.com
dig server.example.com

# Check /etc/hosts
cat /etc/hosts | grep server

# Test with IP directly
curl http://127.0.0.1:3000/health
```

**Common Causes and Solutions:**

1. **Wrong hostname**
   ```bash
   # Use IP instead
   ggen mcp config set servers.myserver.host=127.0.0.1

   # Add to hosts
   echo "127.0.0.1 server.example.com" | sudo tee -a /etc/hosts
   ```

2. **DNS server issues**
   ```bash
   # Check DNS config
   cat /etc/resolv.conf

   # Use different DNS
   ggen mcp config set transports.http.dns_server=8.8.8.8
   ```

#### Firewall Blocking

**Symptom:** Connections blocked by firewall.

**Diagnostic Steps:**
```bash
# Check firewall status
sudo ufw status  # Linux
sudo pfctl -s rules  # macOS

# Test with allowed port
curl http://localhost:80/health

# Check firewall logs
sudo log show --predicate 'process == "pfctl"' --last 10m  # macOS
sudo journalctl -u firewalld --since "10 minutes ago"  # Linux
```

**Common Causes and Solutions:**

1. **Port not allowed**
   ```bash
   # Allow port (Linux UFW)
   sudo ufw allow 3000/tcp
   sudo ufw enable

   # Allow port (macOS)
   sudo pfctl -e
   echo "pass in quick proto tcp from any to any port 3000" | sudo pfctl -f -
   ```

2. **Application blocked**
   ```bash
   # Allow ggen through firewall
   sudo ufw allow from any to any app ggen
   ```

#### Proxy Configuration

**Symptom:** Cannot connect through proxy.

**Diagnostic Steps:**
```bash
# Check proxy settings
env | grep -i proxy

# Test direct connection
curl --noproxy "*" http://localhost:3000/health

# Test proxy connection
curl -x http://proxy:8080 http://localhost:3000/health
```

**Common Causes and Solutions:**

1. **Proxy not configured**
   ```bash
   # Set proxy
   export HTTP_PROXY=http://proxy:8080
   export HTTPS_PROXY=http://proxy:8080
   export NO_PROXY=localhost,127.0.0.1
   ```

2. **Proxy authentication**
   ```bash
   # Set proxy with auth
   export HTTP_PROXY=http://user:pass@proxy:8080

   # Or configure in ggen
   ggen mcp config set transports.http.proxy=http://user:pass@proxy:8080
   ```

### Performance Issues

#### Slow Tool Discovery

**Symptom:** Tool discovery takes too long.

**Diagnostic Steps:**
```bash
# Measure discovery time
time ggen mcp tools

# Check discovery cache
ggen mcp tools --cache-stats

# Test individual servers
ggen mcp test-connection --all
```

**Common Causes and Solutions:**

1. **Network latency**
   ```bash
   # Increase timeout
   ggen mcp config set discovery.timeout_secs=60

   # Use caching
   ggen mcp config set discovery.cache_enabled=true
   ggen mcp config set discovery.cache_ttl_secs=300
   ```

2. **Sequential discovery**
   ```bash
   # Enable concurrent discovery
   ggen mcp config set discovery.concurrent=true
   ggen mcp config set discovery.max_concurrent=10
   ```

3. **Large tool lists**
   ```bash
   # Use pagination
   ggen mcp tools --page-size 50

   # Filter results
   ggen mcp tools --filter "code"
   ```

#### High Memory Usage

**Symptom:** MCP/A2A processes using too much memory.

**Diagnostic Steps:**
```bash
# Check memory usage
ggen mcp server status --metrics

# Monitor over time
watch -n 5 'ps aux | grep ggen'

# Check for leaks
valgrind --leak-check=full ggen mcp server start
```

**Common Causes and Solutions:**

1. **Memory leak**
   ```bash
   # Restart periodically
   ggen mcp server restart --graceful

   # Enable memory limits
   ggen mcp server start --memory-limit 2G
   ```

2. **Large message buffers**
   ```bash
   # Reduce buffer size
   ggen mcp config set transports.buffer_size=8192

   # Enable message batching
   ggen mcp config set message.batching.enabled=true
   ```

3. **Tool cache too large**
   ```bash
   # Clear cache
   ggen mcp tools --clear-cache

   # Reduce cache size
   ggen mcp config set discovery.cache_max_items=100
   ```

#### Message Latency

**Symptom:** High latency in message processing.

**Diagnostic Steps:**
```bash
# Measure latency
ggen mcp server status --metrics | grep latency

# Trace message processing
ggen mcp --trace-message --id test-message-id

# Profile transport
ggen mcp profile --transport
```

**Common Causes and Solutions:**

1. **Serialization overhead**
   ```bash
   # Use faster serialization
   ggen mcp config set serialization.format=cbor

   # Enable compression
   ggen mcp config set transports.compression=true
   ```

2. **Network latency**
   ```bash
   # Use local servers when possible
   ggen mcp config set servers.local.prefer_local=true

   # Enable keepalive
   ggen mcp config set transports.http.keepalive=true
   ```

3. **Processing bottleneck**
   ```bash
   # Increase workers
   ggen mcp server start --workers 4

   # Enable async processing
   ggen mcp config set processing.async=true
   ```

#### Tool Execution Timeout

**Symptom:** Tools timing out during execution.

**Diagnostic Steps:**
```bash
# Check timeout settings
ggen mcp config show | grep timeout

# Test specific tool
ggen mcp test tool-name --timeout 120

# Check tool execution time
ggen mcp tools --metrics | grep duration
```

**Common Causes and Solutions:**

1. **Timeout too short**
   ```bash
   # Increase global timeout
   ggen mcp config set tools.timeout_ms=60000

   # Set per-tool timeout
   ggen mcp config set tools.tool-name.timeout_ms=120000
   ```

2. **Tool actually slow**
   ```bash
   # Profile tool
   ggen mcp profile tool-name

   # Consider async execution
   ggen mcp config set tools.tool-name.async=true
   ```

#### Connection Pool Exhaustion

**Symptom:** No available connections in pool.

**Diagnostic Steps:**
```bash
# Check pool status
ggen mcp server status --pools

# Monitor pool usage
watch -n 1 'ggen mcp server status --pools'

# Check for connection leaks
ggen mcp server status --pools --leak-check
```

**Common Causes and Solutions:**

1. **Pool too small**
   ```bash
   # Increase pool size
   ggen mcp config set transports.http.pool_size=50

   # Set max connections
   ggen mcp config set transports.http.max_connections=100
   ```

2. **Connections not released**
   ```bash
   # Enable connection auto-release
   ggen mcp config set transports.http.auto_release=true

   # Set idle timeout
   ggen mcp config set transports.http.idle_timeout_secs=60
   ```

## Debugging Techniques

### Verbose Logging

**Problem:** You need more detailed information to diagnose issues.

**Solution:** Enable verbose logging at various levels.

```bash
# Enable verbose output
ggen mcp server start --verbose

# Enable debug logging
RUST_LOG=debug ggen mcp server start

# Enable trace logging (most verbose)
RUST_LOG=trace ggen mcp server start

# Enable specific module logging
RUST_LOG=ggen_mcp=debug,ggen_transport=trace ggen mcp server start

# Log to file
RUST_LOG=debug ggen mcp server start 2>&1 | tee debug.log
```

**Logging levels:**
- `error` - Errors only
- `warn` - Warnings and errors
- `info` - Informational (default)
- `debug` - Detailed debugging
- `trace` - Most verbose

### Debug Mode

**Problem:** You need comprehensive debugging information.

**Solution:** Enable debug mode for enhanced diagnostics.

```bash
# Enable debug mode
ggen mcp server start --debug

# Debug with backtrace
RUST_BACKTRACE=1 ggen mcp server start

# Full backtrace
RUST_BACKTRACE=full ggen mcp server start

# Debug with panic backtrace
RUST_BACKTRACE=1 PANIC_BACKTRACE=1 ggen mcp server start
```

**Debug mode features:**
- Enhanced error messages
- Stack traces on errors
- Memory usage tracking
- Performance metrics
- Request/response logging

### Tracing

**Problem:** You need to trace request/response flows.

**Solution:** Enable tracing for detailed flow analysis.

```bash
# Enable tracing
ggen mcp --trace server start

# Trace specific request
ggen mcp --trace-request --id request-id

# Trace with OpenTelemetry
ggen mcp server start --otel-exporter console

# Trace to file
ggen mcp --trace-file trace.log server start
```

**Trace analysis:**
```bash
# View trace file
cat trace.log | jq .

# Filter traces
cat trace.log | jq '.[] | select(.method == "tools/call")'

# Analyze timing
cat trace.log | jq '.[] | select(.duration > 1000)'
```

### Message Inspection

**Problem:** You need to inspect message content.

**Solution:** Use message inspection tools.

```bash
# Enable message logging
ggen mcp config set logging.messages=true
ggen mcp config set logging.message_content=true

# Inspect specific message
ggen mcp inspect-message --id message-id

# Capture messages to file
ggen mcp --message-log messages.json server start

# Pretty print messages
cat messages.json | jq .
```

### Protocol Analysis

**Problem:** You need to analyze protocol-level issues.

**Solution:** Use protocol analysis tools.

```bash
# Enable protocol logging
ggen mcp config set logging.protocol=true

# Analyze JSON-RPC messages
ggen mcp analyze-protocol --file messages.json

# Validate protocol compliance
ggen mcp validate-protocol --server http://localhost:3000

# Compare protocol versions
ggen mcp compare-protocol --version 1.0 --version 2.0
```

### Performance Profiling

**Problem:** You need to identify performance bottlenecks.

**Solution:** Use profiling tools.

```bash
# Enable profiling
ggen mcp server start --profile

# CPU profiling
ggen mcp profile --cpu --duration 30

# Memory profiling
ggen mcp profile --memory --duration 30

# Heap profiling
ggen mcp profile --heap --duration 30

# Flame graph
ggen mcp profile --flamegraph --output flamegraph.svg
```

**Profiling analysis:**
```bash
# View profile
ggen mcp profile --view profile.pb

# Compare profiles
ggen mcp profile compare baseline.pb current.pb

# Find hotspots
ggen mcp profile --hotspots profile.pb
```

## Error Reference

### JSON-RPC Standard Errors (-32700 to -32603)

| Code | Name | Description | Resolution |
|------|------|-------------|------------|
| -32700 | Parse Error | Invalid JSON | Check JSON syntax, validate with `jq` |
| -32600 | Invalid Request | JSON not a valid request | Verify request format matches JSON-RPC 2.0 spec |
| -32601 | Method Not Found | Method does not exist | Check method name, verify server capabilities |
| -32602 | Invalid Params | Invalid method parameters | Validate parameter types and required fields |
| -32603 | Internal Error | Internal server error | Check server logs, report bug if persistent |

**Example:**
```json
{
  "jsonrpc": "2.0",
  "error": {
    "code": -32601,
    "message": "Method not found: unknown_method",
    "data": null
  },
  "id": 1
}
```

### MCP Transport Errors (-32100 to -32106)

| Code | Name | Description | Resolution |
|------|------|-------------|------------|
| -32100 | Transport Error | General transport failure | Check transport configuration, restart server |
| -32101 | Connection Failed | Could not establish connection | Verify server URL, check network connectivity |
| -32102 | Timeout | Operation timed out | Increase timeout, check for slow operations |
| -32103 | Not Connected | Operation attempted while disconnected | Connect first, check connection status |
| -32104 | Shutdown | Transport has been shut down | Restart transport/server |
| -32105 | Protocol Error | Protocol violation detected | Verify protocol compliance, check message format |
| -32106 | Stream Closed | Stream closed unexpectedly | Check for network issues, verify stream handling |

**Example:**
```json
{
  "jsonrpc": "2.0",
  "error": {
    "code": -32102,
    "message": "Operation timed out",
    "data": {
      "timeout_ms": 30000,
      "operation": "tools/call"
    }
  },
  "id": 1
}
```

### A2A Conversion Errors

| Error | Description | Resolution |
|-------|-------------|------------|
| `InvalidMcpRequest` | MCP request format invalid | Validate JSON-RPC request format |
| `InvalidA2aTask` | A2A task format invalid | Check task structure, verify required fields |
| `UnsupportedMethod` | Method not supported by converter | Use supported method or extend converter |
| `MissingField` | Required field missing | Add missing field to message/task |
| `TypeConversion` | Type conversion failed | Check type compatibility, enable coercion |
| `A2AError` | A2A protocol error | Verify A2A server status, check logs |

**Example:**
```rust
// Conversion error
ConversionError::MissingField("tool name".to_string())

// Resolution: Ensure tool name is provided in request
```

### Registration Errors

| Error | Description | Resolution |
|-------|-------------|------------|
| `Conversion` | Tool conversion failed | Check tool schema, validate format |
| `AlreadyRegistered` | Tool already registered | Unregister first or use different ID |
| `Registry` | Registry operation failed | Check registry status, verify permissions |
| `SourceNotFound` | Source not found for cleanup | Verify source URL, check registered sources |

**Example:**
```bash
# Error: tool already registered
ggen mcp: tool 'my_tool' already registered from source 'http://localhost:3000'

# Resolution: Unregister first
ggen mcp tools --unregister my_tool
```

### Configuration Errors

| Error | Description | Resolution |
|-------|-------------|------------|
| `EmptyCommand` | Command field is empty | Provide valid command |
| `InvalidTimeout` | Timeout must be greater than 0 | Set timeout > 0 |
| `DangerousCommand` | Command contains dangerous pattern | Use safe command only |
| `ServerNotFound` | Server configuration not found | Add server to config |
| `ConfigNotFound` | Configuration file not found | Create config file or specify path |
| `InvalidJson` | Invalid JSON in configuration | Fix JSON syntax |
| `InvalidToml` | Invalid TOML in configuration | Fix TOML syntax |
| `TlsMisconfigured` | TLS enabled but certs missing | Add certificate paths or disable TLS |

### CLI Exit Codes

| Code | Name | Description |
|------|------|-------------|
| 0 | Success | Operation completed successfully |
| 1 | ValidationError | RDF/SHACL/type validation failed |
| 2 | SparqlError | SPARQL query syntax/execution error |
| 3 | TemplateError | Template rendering failed |
| 4 | OutputInvalid | Generated code failed validation |
| 5 | Timeout | Operation exceeded time limit |
| 127 | Unknown | Unexpected error |

**Using exit codes:**
```bash
# Check exit code
ggen mcp server start
echo $?  # Prints exit code

# Handle errors
ggen mcp server start || {
    case $? in
        1) echo "Validation failed" ;;
        2) echo "SPARQL error" ;;
        5) echo "Timeout occurred" ;;
        *) echo "Unknown error" ;;
    esac
}
```

## Next Steps

- **Configuration:** [How to Configure MCP Server Settings](configure-server.md)
- **Authentication:** [How to Setup Authentication](setup-authentication.md)
- **List tools:** [How to List and Filter MCP Tools](list-tools.md)
- **Bridge agents:** [How to Bridge A2A Agents as MCP Tools](bridge-agents.md)
- **Integration guide:** [MCP A2A Integration Guide](../../MCP_A2A_INTEGRATION.md)
