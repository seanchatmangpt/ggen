<!-- START doctoc generated TOC please keep comment here to allow auto update -->
<!-- DON'T EDIT THIS SECTION, INSTEAD RE-RUN doCTOC TO UPDATE -->
**Table of Contents**

- [How to Configure Transport Protocols](#how-to-configure-transport-protocols)
  - [Transport Overview](#transport-overview)
  - [HTTP Transport](#http-transport)
  - [WebSocket Transport](#websocket-transport)
  - [QUIC Transport](#quic-transport)
  - [Transport Comparison](#transport-comparison)
  - [Transport Selection](#transport-selection)
  - [Configuration Management](#configuration-management)
  - [TLS Configuration](#tls-configuration)
  - [Troubleshooting](#troubleshooting)
    - [Transport Not Connecting](#transport-not-connecting)
    - [Slow Transport](#slow-transport)
    - [Connection Drops](#connection-drops)
  - [Next Steps](#next-steps)

<!-- END doctoc generated TOC -->

# How to Configure Transport Protocols

Guide to configuring HTTP, WebSocket, and QUIC transports for A2A communication.

## Transport Overview

ggen A2A supports three transport protocols:

| Transport | Use Case | Latency | Reliability | Features |
|-----------|----------|---------|-------------|----------|
| HTTP | Simple requests | Medium | High | Simple, widely supported |
| WebSocket | Real-time bidirectional | Low | High | Full-duplex, low overhead |
| QUIC | High-performance streaming | Very Low | High | Multiplexing, congestion control |

## HTTP Transport

**Problem:** You need simple request-response communication.

**Solution:** Configure HTTP transport.

### Basic Configuration

```bash
# Set HTTP transport
ggen a2a config set transport=http

# Set HTTP server address
ggen a2a config set transport.http.host=0.0.0.0
ggen a2a config set transport.http.port=8080
```

**Configuration file (a2a-config.toml):**
```toml
[transport]
type = "http"

[transport.http]
host = "0.0.0.0"
port = 8080
timeout_ms = 30000
max_retries = 3
keep_alive = true
compression = true
```

### Advanced HTTP Configuration

```toml
[transport.http]
# Server settings
host = "0.0.0.0"
port = 8080
timeout_ms = 30000

# Connection pool
max_connections = 100
connection_timeout_ms = 10000
idle_timeout_ms = 60000

# Retry settings
max_retries = 3
retry_delay_ms = 1000
retry_backoff = "exponential"

# Performance
keep_alive = true
compression = true
max_request_size_mb = 10

# TLS (optional)
tls_enabled = false
tls_cert = "/path/to/cert.pem"
tls_key = "/path/to/key.pem"
```

### Testing HTTP Transport

```bash
# Test HTTP endpoint
curl http://localhost:8080/health

# Test with agent
curl http://localhost:8080/agents/agent-id/health

# Test A2A message
curl -X POST http://localhost:8080/a2a/send \
  -H "Content-Type: application/json" \
  -d '{"to": "agent-name", "message": "Hello"}'
```

## WebSocket Transport

**Problem:** You need real-time bidirectional communication.

**Solution:** Configure WebSocket transport.

### Basic Configuration

```bash
# Set WebSocket transport
ggen a2a config set transport=websocket

# Set WebSocket URL
ggen a2a config set transport.websocket.url=ws://localhost:8080
```

**Configuration file (a2a-config.toml):**
```toml
[transport]
type = "websocket"

[transport.websocket]
url = "ws://localhost:8080"
reconnect_interval_ms = 5000
max_reconnect_attempts = 10
message_timeout_ms = 30000
compression = true
```

### Advanced WebSocket Configuration

```toml
[transport.websocket]
# Connection settings
url = "ws://localhost:8080"
subprotocols = ["a2a-v1", "a2a-v2"]

# Reconnection
reconnect_interval_ms = 5000
max_reconnect_attempts = 10
reconnect_backoff = "exponential"

# Message handling
message_timeout_ms = 30000
max_message_size_mb = 10
compression = true

# Ping/pong for keep-alive
ping_interval_ms = 30000
pong_timeout_ms = 5000

# TLS (optional)
tls_enabled = false
tls_verify = true
```

### Testing WebSocket Transport

```bash
# Test with websocat
websocat ws://localhost:8080/ws

# Test with wscat
wscat -c ws://localhost:8080/ws

# Send test message
echo '{"type":"ping"}' | websocat ws://localhost:8080/ws
```

### WebSocket Client Example

**Rust client:**
```rust
use ggen_a2a::WebSocketTransport;

let transport = WebSocketTransport::builder()
    .url("ws://localhost:8080/ws")
    .reconnect_interval(Duration::from_secs(5))
    .max_reconnect_attempts(10)
    .build()?;

transport.connect().await?;

// Send message
transport.send(Message::text("Hello")).await?;

// Receive message
let msg = transport.receive().await?;
```

## QUIC Transport

**Problem:** You need high-performance streaming with multiplexing.

**Solution:** Configure QUIC transport.

### Basic Configuration

```bash
# Set QUIC transport
ggen a2a config set transport=quic

# Set QUIC server
ggen a2a config set transport.quic.server_name=localhost
ggen a2a config set transport.quic.port=4433
```

**Configuration file (a2a-config.toml):**
```toml
[transport]
type = "quic"

[transport.quic]
server_name = "localhost"
port = 4433
alpn_protocols = ["a2a-quic"]
idle_timeout_ms = 30000
max_concurrent_streams = 100
congestion_control = "bbr"
enable_zero_rtt = true
```

### Advanced QUIC Configuration

```toml
[transport.quic]
# Server settings
server_name = "localhost"
port = 4433
alpn_protocols = ["a2a-quic", "a2a-quic-v2"]

# Connection settings
idle_timeout_ms = 30000
max_concurrent_streams = 100
initial_mtu = 1200
max_mtu = 1500

# Performance
congestion_control = "bbr"  # or "cubic"
enable_zero_rtt = true
enable_hystart = true

# TLS (required for QUIC)
tls_enabled = true
tls_cert = "/path/to/cert.pem"
tls_key = "/path/to/key.pem"
tls_client_ca = "/path/to/ca.pem"
```

### Testing QUIC Transport

```bash
# Test with quiche client
quiche-client https://localhost:4433/health

# Test with ngtcp2
ngtcp2-client --host localhost --port 4433

# Test A2A over QUIC
ggen a2a test-transport --type quic
```

### QUIC Client Example

**Rust client:**
```rust
use ggen_a2a::QuicTransport;

let transport = QuicTransport::builder()
    .server_name("localhost")
    .port(4433)
    .alpn_protocols(&["a2a-quic"])
    .ca_cert("/path/to/ca.pem")
    .build()?;

transport.connect().await?;

// Open stream
let mut stream = transport.open_stream().await?;
stream.write_all(b"Hello").await?;

// Receive data
let mut buf = vec![0u8; 1024];
let n = stream.read(&mut buf).await?;
```

## Transport Comparison

| Feature | HTTP | WebSocket | QUIC |
|---------|------|-----------|------|
| Bidirectional | No | Yes | Yes |
| Multiplexing | HTTP/2 only | Yes | Yes |
| Low latency | Medium | Low | Very Low |
| Head-of-line blocking | Yes | No | No |
| Server push | Yes | No | No |
| Connection migration | No | No | Yes |
| Firewall friendly | High | Medium | Low |
| Browser support | Universal | Universal | Limited |

## Transport Selection

**Use HTTP when:**
- Simple request-response is sufficient
- Maximum compatibility is needed
- Firewall traversal is important
- Stateless communication is preferred

**Use WebSocket when:**
- Real-time bidirectional communication is needed
- Low latency is important
- Server push is required
- Stateful communication is acceptable

**Use QUIC when:**
- Maximum performance is required
- Network conditions are variable
- Connection migration is needed
- Advanced features like 0-RTT are desired

## Configuration Management

### Switching Transports

```bash
# Switch from HTTP to WebSocket
ggen a2a config set transport=websocket
ggen a2a restart

# Switch from WebSocket to QUIC
ggen a2a config set transport=quic
ggen a2a restart
```

### Multi-Transport Configuration

```toml
[transport]
# Primary transport
type = "websocket"

# Fallback transport
fallback = "http"

# Enable both
[transport.websocket]
url = "ws://localhost:8080"
enabled = true

[transport.http]
port = 8081
enabled = true
```

### Transport-Specific Settings

```bash
# Show current transport
ggen a2a config show transport

# Show all transport options
ggen a2a config show transport --all

# Validate transport configuration
ggen a2a config validate transport
```

## TLS Configuration

### TLS for HTTP/WebSocket

```toml
[transport.tls]
enabled = true
cert = "/path/to/cert.pem"
key = "/path/to/key.pem"
ca_cert = "/path/to/ca.pem"
verify_client = false
min_version = "1.2"
max_version = "1.3"
ciphers = ["TLS_AES_128_GCM_SHA256"]
```

### TLS for QUIC (Required)

```toml
[transport.quic.tls]
enabled = true
cert = "/path/to/cert.pem"
key = "/path/to/key.pem"
ca_cert = "/path/to/ca.pem"
verify_client = true
```

### Generating Certificates

```bash
# Generate self-signed certificate
openssl req -x509 -newkey rsa:2048 \
  -keyout key.pem \
  -out cert.pem \
  -days 365 \
  -nodes

# Generate CA-signed certificate
ggen a2a generate-cert \
  --ca ca.pem \
  --cn "a2a-server" \
  --out cert.pem \
  --key-out key.pem
```

## Troubleshooting

### Transport Not Connecting

**Problem:** Transport fails to establish connection.

**Solutions:**
```bash
# Check transport status
ggen a2a status

# Verify configuration
ggen a2a config validate transport

# Test connectivity
ggen a2a test-connection

# Check firewall
telnet localhost 8080

# Review logs
ggen a2a logs --transport
```

### Slow Transport

**Problem:** Transport has high latency.

**Solutions:**
```bash
# Measure latency
ggen a2a ping --count 10

# Check network conditions
ggen a2a network-diag

# Optimize settings
ggen a2a config set transport.http.keep_alive=true
ggen a2a config set transport.websocket.compression=true
ggen a2a config set transport.quic.congestion_control=bbr

# Restart with new settings
ggen a2a restart
```

### Connection Drops

**Problem:** Connections drop unexpectedly.

**Solutions:**
```bash
# Check keep-alive settings
ggen a2a config show transport | grep keep_alive

# Enable keep-alive
ggen a2a config set transport.http.keep_alive=true
ggen a2a config set transport.websocket.ping_interval_ms=30000

# Check timeout settings
ggen a2a config show transport | grep timeout

# Increase timeouts
ggen a2a config set transport.http.idle_timeout_ms=120000

# Check network stability
ping -c 100 localhost
```

## Next Steps

- **Start agents:** [How to Start A2A Agents](start-agent.md)
- **Send messages:** [How to Send Messages Between Agents](send-messages.md)
- **Monitor agents:** [How to Monitor Agent Health and Metrics](monitor-agents.md)
- **Bridge to MCP:** [How to Bridge A2A Agents as MCP Tools](../mcp/bridge-agents.md)
- **Integration guide:** [MCP A2A Integration Guide](../MCP_A2A_INTEGRATION.md)
