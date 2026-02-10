<!-- START doctoc generated TOC please keep comment here to allow auto update -->
<!-- DON'T EDIT THIS SECTION, INSTEAD RE-RUN doctoc TO UPDATE -->
**Table of Contents**

- [How to Troubleshoot Connection Issues](#how-to-troubleshoot-connection-issues)
  - [Diagnosing Connection Problems](#diagnosing-connection-problems)
  - [Server Not Running](#server-not-running)
  - [Connection Refused](#connection-refused)
  - [Timeout Issues](#timeout-issues)
  - [Authentication Failures](#authentication-failures)
  - [Transport-Specific Issues](#transport-specific-issues)
    - [HTTP/HTTPS Issues](#httphttps-issues)
    - [WebSocket Issues](#websocket-issues)
    - [QUIC Issues](#quic-issues)
  - [Network and Firewall Issues](#network-and-firewall-issues)
  - [Performance Issues](#performance-issues)
  - [Debug Mode](#debug-mode)
  - [Common Error Messages](#common-error-messages)
  - [Getting Help](#getting-help)
  - [Next Steps](#next-steps)

<!-- END doctoc generated TOC please keep comment here to allow auto update -->

# How to Troubleshoot Connection Issues

Guide to diagnosing and resolving MCP server connection problems.

## Diagnosing Connection Problems

**Problem:** You need to identify why MCP connections are failing.

**Solution:** Use the diagnostic tools available.

```bash
# Run connection diagnostics
ggen mcp doctor

# Check server status
ggen mcp server status

# Verify configuration
ggen mcp config validate

# Test connectivity
ggen mcp test-connection
```

**Expected Output:**
```bash
$ ggen mcp doctor

MCP Connection Diagnostics
==========================

Server Status: Running
  PID: 12345
  Uptime: 2h 34m
  Address: 127.0.0.1:3000

Configuration: Valid
  Config file: /Users/sac/ggen/.mcp.json
  Version: 1.0

Connectivity: OK
  HTTP: Connected
  WebSocket: Connected
  Tools: 12 available

Issues: None found
```

## Server Not Running

**Problem:** Connection attempts fail because the server is not running.

**Solutions:**

```bash
# Check server status
ggen mcp server status

# Start the server
ggen mcp server start

# Start with verbose output
ggen mcp server start --verbose

# Start specific server
ggen mcp server start --server http
```

**Expected Output:**
```bash
$ ggen mcp server start

Starting MCP server...
MCP server started successfully
  PID: 12345
  Port: 3000
```

## Connection Refused

**Problem:** `Connection refused` error when connecting to MCP server.

**Solutions:**

```bash
# Check if server is listening
netstat -an | grep 3000
# or
lsof -i :3000

# Verify server configuration
ggen mcp config show

# Check correct port
ggen mcp server status --verbose

# Test with curl
curl -v http://localhost:3000/health
```

**Common causes:**

1. **Wrong port:** Server running on different port
   ```bash
   # Find correct port
   ggen mcp server status | grep "Port"

   # Update configuration
   ggen mcp config set transports.http.port=3001
   ```

2. **Wrong host:** Server bound to different interface
   ```bash
   # Check binding
   ggen mcp server status | grep "Address"

   # Update to bind to all interfaces
   ggen mcp config set transports.http.host="0.0.0.0"
   ```

3. **Firewall blocking:** Firewall preventing connection
   ```bash
   # Check firewall rules (macOS)
   sudo pfctl -sr | grep 3000

   # Check firewall rules (Linux)
   sudo iptables -L -n | grep 3000

   # Allow port
   sudo ufw allow 3000
   ```

## Timeout Issues

**Problem:** Connections timeout before completing.

**Solutions:**

```bash
# Increase timeout
ggen mcp config set transports.http.timeout_ms=30000

# Test with timeout
curl --max-time 30 http://localhost:3000/tools

# Check for slow operations
ggen mcp server status --verbose | grep "Slow operations"
```

**Timeout values:**

| Setting | Default | Recommended for Slow Networks |
|---------|---------|-------------------------------|
| Connection timeout | 10s | 30s |
| Request timeout | 60s | 120s |
| WebSocket timeout | 30s | 60s |

## Authentication Failures

**Problem:** Connections rejected due to authentication issues.

**Solutions:**

```bash
# Verify credentials
ggen mcp auth verify-token "$TOKEN"

# Check API key
ggen mcp auth list-api-keys

# Test without auth
curl http://localhost:3000/health

# Test with auth
curl -H "Authorization: Bearer $TOKEN" \
  http://localhost:3000/tools
```

**Common authentication errors:**

| Error | Cause | Solution |
|-------|-------|----------|
| `Invalid API key` | Wrong or expired key | Generate new key |
| `Token expired` | JWT token expired | Refresh token |
| `Invalid signature` | JWT secret mismatch | Update secret |
| `Certificate verify failed` | mTLS certificate issue | Verify certificates |

## Transport-Specific Issues

### HTTP/HTTPS Issues

```bash
# Test HTTP connection
curl -v http://localhost:3000/health

# Test HTTPS connection
curl -v -k https://localhost:3443/health

# Check SSL certificate
openssl s_client -connect localhost:3443 -showcerts

# Verify SSL configuration
ggen mcp config show | grep -A 10 ssl
```

### WebSocket Issues

```bash
# Test WebSocket connection
wscat -c ws://localhost:3000/ws

# Test with authentication
wscat -c "ws://localhost:3000/ws?token=$TOKEN"

# Check WebSocket logs
ggen mcp server logs --grep "WebSocket"

# Verify WebSocket configuration
ggen mcp config show | grep -A 10 websocket
```

### QUIC Issues

```bash
# Test QUIC connection
# (requires quiche or similar tool)
quiche-client https://localhost:4433/

# Check QUIC logs
ggen mcp server logs --grep "QUIC"

# Verify QUIC configuration
ggen mcp config show | grep -A 10 quic
```

## Network and Firewall Issues

```bash
# Test local connectivity
ping -c 3 127.0.0.1

# Test port connectivity
nc -zv localhost 3000

# Check DNS resolution
nslookup localhost

# Check routing
netstat -rn

# Test from remote host
curl http://your-server:3000/health
```

**Firewall configuration:**

```bash
# macOS - Allow port
sudo pfctl -e
echo "pass in quick proto tcp from any to any port 3000" | sudo pfctl -f -

# Linux - UFW
sudo ufw allow 3000/tcp
sudo ufw enable

# Linux - iptables
sudo iptables -A INPUT -p tcp --dport 3000 -j ACCEPT
```

## Performance Issues

**Problem:** Connections succeed but are slow.

**Solutions:**

```bash
# Measure latency
time curl http://localhost:3000/health

# Check server load
ggen mcp server status --metrics

# Enable compression
ggen mcp config set transports.http.compression=true

# Increase workers
ggen mcp config set server.workers=4

# Monitor performance
ggen mcp server monitor
```

**Performance tuning:**

| Setting | Default | High Performance |
|---------|---------|------------------|
| Workers | 2 | 4-8 |
| Connection pool | 10 | 50-100 |
| Buffer size | 8KB | 64KB |
| Compression | false | true |

## Debug Mode

**Problem:** You need detailed information to diagnose issues.

**Solution:** Enable debug mode.

```bash
# Enable debug logging
export RUST_LOG=debug,ggen_mcp=trace

# Start server with debug
ggen mcp server start --debug --verbose

# Enable debug for CLI
ggen mcp list --debug

# Capture debug output
ggen mcp server start --debug 2>&1 | tee mcp-debug.log
```

**Debug categories:**

- `mcp_server` - Server operations
- `transport` - Transport layer
- `auth` - Authentication
- `tools` - Tool execution
- `protocol` - Protocol translation

## Common Error Messages

| Error | Meaning | Solution |
|-------|---------|----------|
| `Connection refused` | Server not running | Start server |
| `Connection timeout` | Server not responding | Check firewall, increase timeout |
| `Authentication failed` | Invalid credentials | Verify API key/token |
| `Tool not found` | Tool doesn't exist | Check tool name, refresh list |
| `Invalid request` | Malformed request | Check request format |
| `Server error` | Internal server error | Check server logs |
| `Rate limit exceeded` | Too many requests | Wait, increase rate limit |
| `Certificate verify failed` | SSL/TLS issue | Update certificates |

## Getting Help

When all else fails, gather diagnostic information:

```bash
# Collect diagnostics
ggen mcp doctor > diagnostics.txt

# Export configuration
ggen mcp config show > config.json

# Export logs
ggen mcp server logs --tail 1000 > server.log

# Export metrics
ggen mcp server status --metrics > metrics.json
```

**Report issue with:**
1. Diagnostic output
2. Configuration (with secrets redacted)
3. Relevant log excerpts
4. Steps to reproduce
5. ggen version (`ggen --version`)

## Next Steps

- **Configure server:** [How to Configure MCP Server Settings](configure-server.md)
- **Setup authentication:** [How to Setup Authentication](setup-authentication.md)
- **List tools:** [How to List and Filter MCP Tools](list-tools.md)
- **CLI reference:** [CLI Reference](../reference/cli.md#mcp-commands)
- **Integration guide:** [MCP A2A Integration Guide](../MCP_A2A_INTEGRATION.md)
