<!-- START doctoc generated TOC please keep comment here to allow auto update -->
<!-- DON'T EDIT THIS SECTION, INSTEAD RE-RUN doCTOC TO UPDATE -->
**Table of Contents**

- [How to Send Messages Between Agents](#how-to-send-messages-between-agents)
  - [Understanding A2A Messaging](#understanding-a2a-messaging)
  - [Basic Message Sending](#basic-message-sending)
  - [Message Formats](#message-formats)
  - [Direct Agent Communication](#direct-agent-communication)
  - [Broadcast Messages](#broadcast-messages)
  - [Request-Response Pattern](#request-response-pattern)
  - [Streaming Messages](#streaming-messages)
  - [Message Queues](#message-queues)
  - [Handling Errors](#handling-errors)
  - [Troubleshooting](#troubleshooting)
    - [Message Not Delivered](#message-not-delivered)
    - [No Response](#no-response)
    - [Serialization Errors](#serialization-errors)
  - [Next Steps](#next-steps)

<!-- END doctoc generated TOC -->

# How to Send Messages Between Agents

Guide to sending and receiving messages via the A2A protocol.

## Understanding A2A Messaging

A2A (Agent-to-Agent) messaging enables:
- Direct communication between agents
- Asynchronous request-response patterns
- Message broadcasting to multiple agents
- Streaming data between agents

## Basic Message Sending

**Problem:** You need to send a message to an agent.

**Solution:** Use the `ggen a2a send` command.

```bash
# Send a simple message
ggen a2a send "text-generator" \
  --message "Generate a hello world message"

# Send with message ID
ggen a2a send "code-analyzer" \
  --id "msg-001" \
  --message "Analyze this code: fn main() {}"

# Send and wait for response
ggen a2a send "agent" --message "..." --wait
```

**Expected Output:**
```bash
$ ggen a2a send "text-generator" \
  --message "Generate a hello world message"

Message sent successfully
  Message ID: msg_abc123xyz789
  From: system
  To: text-generator
  Status: delivered

Waiting for response...
Response received:
  Hello! This is a generated message from the text-generator agent.
```

## Message Formats

### Text Messages

```bash
ggen a2a send "agent" \
  --message "Plain text message"
```

### JSON Messages

```bash
ggen a2a send "agent" \
  --json '{"task": "analyze", "data": "..."}'
```

### Structured Messages

```bash
ggen a2a send "agent" \
  --type "request" \
  --payload '{"action": "process", "input": "..."}' \
  --metadata '{"priority": "high", "timeout": 30}'
```

## Direct Agent Communication

**Problem:** You need agents to communicate directly with each other.

**Solution:** Use direct addressing.

```bash
# Send from one agent to another
ggen a2a send \
  --from "workflow-agent" \
  --to "code-analyzer" \
  --message "Analyze the workflow code"

# Send with agent authentication
ggen a2a send \
  --from "workflow-agent" \
  --to "code-analyzer" \
  --message "..." \
  --auth-token "$AGENT_TOKEN"
```

**From within an agent (Rust example):**
```rust
use ggen_a2a::{Message, MessageSender};

let sender = MessageSender::new("workflow-agent")?;
let message = Message::builder()
    .to("code-analyzer")
    .payload("Analyze code")
    .build()?;

sender.send(message).await?;
```

## Broadcast Messages

**Problem:** You need to send a message to multiple agents.

**Solution:** Use broadcast addressing.

```bash
# Broadcast to all agents
ggen a2a broadcast \
  --message "System maintenance in 5 minutes"

# Broadcast to specific agent types
ggen a2a broadcast \
  --type "generative" \
  --message "Update model weights"

# Broadcast to specific agents
ggen a2a broadcast \
  --to "text-generator,code-analyzer" \
  --message "New task available"
```

**Expected Output:**
```bash
$ ggen a2a broadcast --to "text-generator,code-analyzer" \
  --message "New task available"

Broadcast sent successfully
  Message ID: msg_xyz789abc123
  Recipients: 2
  Delivered: 2
  Failed: 0

Delivery status:
  ✓ text-generator (delivered in 5ms)
  ✓ code-analyzer (delivered in 3ms)
```

## Request-Response Pattern

**Problem:** You need a response from an agent.

**Solution:** Use the request-response pattern.

```bash
# Send request and wait for response
ggen a2a request "text-generator" \
  --payload '{"prompt": "Write a poem"}' \
  --timeout 30

# Send with callback
ggen a2a request "agent" \
  --payload '{"task": "..."}' \
  --callback "http://my-server/callback"

# Send with async response
RESPONSE_ID=$(ggen a2a request "agent" --payload '{...}' --async)
# Later...
ggen a2a response "$RESPONSE_ID"
```

**Expected Output:**
```bash
$ ggen a2a request "text-generator" \
  --payload '{"prompt": "Write a poem"}' \
  --timeout 30

Request sent: req_abc123
Waiting for response...

Response received:
  ID: res_xyz789
  From: text-generator
  Status: success
  Payload: |
    {
      "poem": "In circuits deep where data flows...",
      "tokens": 45
    }
  Latency: 1.2s
```

## Streaming Messages

**Problem:** You need to stream data between agents.

**Solution:** Use the streaming API.

```bash
# Start a stream
ggen a2a stream start "text-generator" \
  --payload '{"prompt": "Tell a long story"}'

# Subscribe to stream
ggen a2a stream subscribe "stream-abc123"

# Send streaming data
echo "Line 1" | ggen a2a stream send "stream-abc123"
echo "Line 2" | ggen a2a stream send "stream-abc123"

# Stop stream
ggen a2a stream stop "stream-abc123"
```

**From within an agent (Rust example):**
```rust
use ggen_a2a::{StreamSender, StreamMessage};

let mut stream = StreamSender::connect("agent-name").await?;

for chunk in data_chunks {
    stream.send(StreamMessage::chunk(chunk)).await?;
}

stream.close().await?;
```

## Message Queues

**Problem:** You need reliable message delivery with queuing.

**Solution:** Use message queues.

```bash
# Send to queue
ggen a2a queue send "task-queue" \
  --payload '{"task": "process", "data": "..."}'

# Create queue
ggen a2a queue create "high-priority" \
  --max-size 1000 \
  --retention 1h

# List queues
ggen a2a queue list

# Consume from queue
ggen a2a queue receive "task-queue" \
  --wait \
  --timeout 60
```

**Queue configuration:**
```yaml
queues:
  task-queue:
    max_size: 10000
    retention: 24h
    max_retries: 3
    retry_delay: 60s
    dead_letter: task-queue-dlq
```

## Handling Errors

### Retry Failed Messages

```bash
# Send with retry
ggen a2a send "agent" \
  --message "..." \
  --retries 3 \
  --retry-delay 5s

# Retry failed message
ggen a2a retry "msg-failed-123"
```

### Dead Letter Queue

```bash
# List dead letter messages
ggen a2a dlq list

# Inspect dead letter
ggen a2a dlq show "msg-dead-456"

# Retry from dead letter
ggen a2a dlq retry "msg-dead-456"
```

### Error Handling in Code

```rust
use ggen_a2a::{MessageSender, SendError};

match sender.send(message).await {
    Ok(_) => println!("Message sent"),
    Err(SendError::Timeout(msg_id)) => {
        eprintln!("Timeout sending {}", msg_id);
        // Retry logic
    }
    Err(SendError::AgentUnavailable(msg_id)) => {
        eprintln!("Agent unavailable for {}", msg_id);
        // Queue for later
    }
    Err(e) => eprintln!("Send error: {:?}", e),
}
```

## Troubleshooting

### Message Not Delivered

**Problem:** Message sent but never delivered.

**Solutions:**
```bash
# Check message status
ggen a2a status "msg-id"

# Check agent is running
ggen agent status "agent-name"

# Check network connectivity
ggen a2a test-connection "agent-name"

# Check queue depth
ggen a2a queue stats "queue-name"
```

### No Response

**Problem:** Request sent but no response received.

**Solutions:**
```bash
# Check request status
ggen a2a request-status "req-id"

# Increase timeout
ggen a2a request "agent" \
  --payload '{...}' \
  --timeout 120

# Check agent logs
ggen agent logs "agent-name" --tail 100

# Verify agent capability
ggen agent capabilities "agent-name"
```

### Serialization Errors

**Problem:** Message payload cannot be serialized/deserialized.

**Solutions:**
```bash
# Validate JSON before sending
echo '{...}' | jq .

# Use explicit type
ggen a2a send "agent" \
  --type "application/json" \
  --payload '{...}'

# Check message schema
ggen a2a schema show "agent-name"
```

## Next Steps

- **Start agents:** [How to Start A2A Agents](start-agent.md)
- **Monitor agents:** [How to Monitor Agent Health and Metrics](monitor-agents.md)
- **Configure transport:** [How to Configure Transport Protocols](configure-transport.md)
- **Bridge to MCP:** [How to Bridge A2A Agents as MCP Tools](../mcp/bridge-agents.md)
- **Integration guide:** [MCP A2A Integration Guide](../MCP_A2A_INTEGRATION.md)
