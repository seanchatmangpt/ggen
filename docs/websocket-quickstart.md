# WebSocket Transport Quickstart Guide

## Overview
All A2A agents now support WebSocket transport for real-time, bidirectional communication alongside existing HTTP endpoints.

## WebSocket Endpoints

| Language | Endpoint | Protocol |
|----------|----------|----------|
| Rust | `/a2a/ws` | WebSocket (RFC 6455) |
| Go | `/a2a/ws` | WebSocket (RFC 6455) |
| TypeScript | `/a2a/ws` | WebSocket (RFC 6455) |
| Java | `HTTP_PORT + 1` | WebSocket (RFC 6455) |
| Elixir | `/a2a/socket` | Phoenix Channels |

## Quick Test

### Using websocat (command-line WebSocket client)

```bash
# Install websocat
cargo install websocat

# Test Rust agent
websocat ws://localhost:8090/a2a/ws

# Test Go agent
websocat ws://localhost:8089/a2a/ws

# Test TypeScript agent
websocat ws://localhost:8089/a2a/ws

# Test Java agent
websocat ws://localhost:8090/a2a/ws

# Test Elixir agent
websocat ws://localhost:4000/a2a/socket
```

### Sending Messages

Once connected, send JSON-RPC messages:

```json
{
  "jsonrpc": "2.0",
  "method": "your_skill_name",
  "params": {
    "key": "value"
  },
  "id": 1
}
```

### Expected Response

```json
{
  "jsonrpc": "2.0",
  "result": {
    "taskId": "uuid-here",
    "status": "created"
  },
  "id": 1
}
```

## Python Client Example

```python
import asyncio
import websockets
import json

async def test_agent():
    uri = "ws://localhost:8090/a2a/ws"
    async with websockets.connect(uri) as websocket:
        # Send message
        message = {
            "jsonrpc": "2.0",
            "method": "your_skill",
            "params": {"key": "value"},
            "id": 1
        }
        await websocket.send(json.dumps(message))

        # Receive response
        response = await websocket.recv()
        print(f"Received: {response}")

asyncio.run(test_agent())
```

## JavaScript Client Example

```javascript
const ws = new WebSocket('ws://localhost:8090/a2a/ws');

ws.onopen = () => {
  console.log('Connected to agent');

  // Send message
  ws.send(JSON.stringify({
    jsonrpc: '2.0',
    method: 'your_skill',
    params: { key: 'value' },
    id: 1
  }));
};

ws.onmessage = (event) => {
  console.log('Received:', JSON.parse(event.data));
};

ws.onerror = (error) => {
  console.error('WebSocket error:', error);
};

ws.onclose = () => {
  console.log('Connection closed');
};
```

## Go Client Example

```go
package main

import (
    "log"
    "net/url"
    "github.com/gorilla/websocket"
)

func main() {
    u := url.URL{Scheme: "ws", Host: "localhost:8090", Path: "/a2a/ws"}
    log.Printf("Connecting to %s", u.String())

    c, _, err := websocket.DefaultDialer.Dial(u.String(), nil)
    if err != nil {
        log.Fatal("dial:", err)
    }
    defer c.Close()

    // Send message
    message := []byte(`{"jsonrpc":"2.0","method":"your_skill","params":{},"id":1}`)
    err = c.WriteMessage(websocket.TextMessage, message)
    if err != nil {
        log.Fatal("write:", err)
    }

    // Read response
    _, response, err := c.ReadMessage()
    if err != nil {
        log.Fatal("read:", err)
    }
    log.Printf("Received: %s", response)
}
```

## Connection Features

### Automatic Reconnection
All clients support automatic reconnection with exponential backoff:
- **Max retries:** 3 attempts
- **Backoff strategy:** Linear (1s, 2s, 3s)
- **Timeout:** 30 seconds for receive operations

### Error Handling
WebSocket connections handle:
- Connection failures (retry with backoff)
- Parse errors (return JSON-RPC -32700)
- Method not found (return JSON-RPC -32601)
- Invalid parameters (return JSON-RPC -32602)
- Timeouts (return error after 30s)

### Message Types
Supported message formats:
- **Text messages:** JSON strings (JSON-RPC)
- **Binary messages:** Binary data (language-specific)

## Monitoring

### Connection Status
Check WebSocket connection health:

```bash
# View active connections (varies by language)
# Rust: Check logs for "WebSocket connection established"
# Go: Check logs for "WebSocket connection established"
# TypeScript: Check logs for "WebSocket connection established"
# Java: Check logs for "WebSocket server started"
# Elixir: Check logs for "WebSocket client joined"
```

### Message Throughput
Monitor message flow:
```python
import time

start = time.time()
messages_sent = 0

async def send_messages(websocket):
    global messages_sent
    while True:
        await websocket.send(json.dumps({
            "jsonrpc": "2.0",
            "method": "ping",
            "id": messages_sent
        }))
        messages_sent += 1
        await asyncio.sleep(0.1)
```

## Security

### Production Deployment
Always use WSS (WebSocket Secure) in production:

```javascript
// Development
const ws = new WebSocket('ws://localhost:8090/a2a/ws');

// Production
const ws = new WebSocket('wss://your-domain.com/a2a/ws');
```

### Authentication
Implement token-based authentication on connection:

```python
headers = {"Authorization": "Bearer your-token"}
async with websockets.connect(uri, extra_headers=headers) as ws:
    # ...
```

## Troubleshooting

### Connection Refused
- **Check:** Agent is running
- **Check:** Correct port number
- **Check:** Firewall settings

### Connection Timeout
- **Check:** Network latency
- **Check:** Agent load
- **Increase:** Timeout value in client

### Message Not Received
- **Check:** JSON format validity
- **Check:** Method name exists
- **Check:** Parameters match skill input schema

### Frequent Disconnections
- **Check:** Network stability
- **Check:** Agent resource usage
- **Implement:** Client-side reconnection logic

## Performance Tips

1. **Reuse Connections:** Maintain persistent connections
2. **Batch Messages:** Send multiple messages in single frame
3. **Compression:** Enable permessage-deflate extension
4. **Binary Protocol:** Use MessagePack for smaller payloads
5. **Connection Pooling:** Use multiple connections for high throughput

## Next Steps

1. **Start your agent:** See language-specific documentation
2. **Test WebSocket:** Use websocat or browser console
3. **Implement skills:** Replace TODO handlers with real logic
4. **Deploy:** Configure reverse proxy (nginx, traefik) for WSS
5. **Monitor:** Track connection metrics and message throughput

## Support

- **Issues:** https://github.com/seanchatmangpt/ggen/issues
- **Docs:** https://github.com/seanchatmangpt/ggen/tree/main/docs
- **Examples:** https://github.com/seanchatmangpt/ggen/tree/main/examples
