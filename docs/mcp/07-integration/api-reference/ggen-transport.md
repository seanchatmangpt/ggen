# API Reference: ggen-transport

Transport layer with session management and streaming support for A2A messaging.

## Structs

### A2aTransport

**Purpose**: Message-level transport with handler registration and agent routing.

**Fields** (private):
- `agent_id: String` - This transport's agent identifier
- `handlers: Arc<RwLock<HashMap<String, Arc<dyn A2aMessageHandler>>>>` - Registered handlers
- `session_manager: SessionManager` - Session lifecycle management
- `origin_validator: OriginValidator` - Origin validation

**Constructor**:
```rust
// Use TransportBuilder
let transport = TransportBuilder::new()
    .with_session_ttl(7200)
    .with_buffer_size(200)
    .build_a2a("agent-id");
```

**Key Methods**:
```rust
pub async fn handle_message(&self, message: A2aMessage) -> A2aResponse
pub async fn handle_stream(&self, message: A2aStreamMessage) -> Result<(StreamSender, MessageStream)>
pub async fn send_message(&self, message: A2aMessage) -> Result<A2aResponse>
pub fn register_handler(&self, message_type: &str, handler: Arc<dyn A2aMessageHandler>)
pub fn register_agent_route(&self, agent_id: &str, endpoint: &str)
```

**Example**:
```rust
let transport = TransportBuilder::new().build_a2a("my-agent");

// Register handler
transport.register_handler("Request", Arc::new(MyHandler));

// Handle message
let response = transport.handle_message(message).await?;
```

---

### SessionManager

**Purpose**: Thread-safe session lifecycle management with TTL and expiration.

**Fields** (private):
- `sessions: Arc<RwLock<HashMap<SessionId, Session>>>` - Session storage
- `default_ttl: Duration` - Default session TTL (seconds)

**Constructor**:
```rust
pub fn new(default_ttl_seconds: i64) -> Self
```

**Key Methods**:
```rust
pub async fn create_session(&self) -> Session
pub async fn get_session(&self, id: &SessionId) -> Result<Session, TransportError>
pub async fn update_session(&self, id: &SessionId, session: Session)
pub async fn touch_session(&self, id: &SessionId) -> Result<(), TransportError>
pub async fn delete_session(&self, id: &SessionId)
pub async fn cleanup_expired(&self) -> usize
pub async fn set_cursor(&self, id: &SessionId, cursor: ResumeCursor)
pub async fn get_cursor(&self, id: &SessionId) -> Option<ResumeCursor>
```

**Example**:
```rust
let manager = SessionManager::new(3600); // 1 hour TTL

// Create session
let session = manager.create_session().await;
let id = session.id;

// Use session
manager.touch_session(&id).await?;

// Cleanup expired
let cleaned = manager.cleanup_expired().await;
println!("Cleaned {} sessions", cleaned);
```

---

### StreamBuilder

**Purpose**: Factory for creating (StreamSender, MessageStream) pairs.

**Key Methods**:
```rust
pub fn new(session_id: SessionId) -> Self
pub fn with_buffer_size(self, size: usize) -> Self
pub fn resume_from(self, position: u64) -> Self
pub fn build(self) -> (StreamSender, MessageStream)
```

**Example**:
```rust
let (sender, mut stream) = StreamBuilder::new(session_id)
    .with_buffer_size(100)
    .resume_from(50)
    .build();

// Send messages
sender.send("Hello".into()).await?;
sender.send_final("World".into()).await?;

// Receive messages
while let Some(result) = stream.next().await {
    match result? {
        StreamMessage { payload, is_final, .. } => {
            println!("Received: {:?}", String::from_utf8(payload)?);
            if is_final { break; }
        }
    }
}
```

---

### StreamSender

**Purpose**: Producer end of a message stream.

**Key Methods**:
```rust
pub async fn send(&self, payload: Bytes) -> Result<(), TransportError>
pub async fn send_with_metadata(&self, payload: Bytes, metadata: HashMap<String, String>)
pub async fn send_final(&self, payload: Bytes) -> Result<(), TransportError>
pub async fn send_error(&self, error: TransportError)
pub fn get_current_sequence(&self) -> u64
```

**Example**:
```rust
// Regular message
sender.send(b"chunk 1".to_vec().into()).await?;

// Message with metadata
let mut meta = HashMap::new();
meta.insert("type".to_string(), "progress".to_string());
sender.send_with_metadata(b"chunk 2".to_vec().into(), meta).await?;

// Final message
sender.send_final(b"done".to_vec().into()).await?;
```

---

### MessageStream

**Purpose**: Consumer end of a message stream with cursor support.

**Key Methods**:
```rust
pub async fn next(&mut self) -> Option<Result<StreamMessage, TransportError>>
pub fn get_cursor(&self) -> ResumeCursor
```

**Example**:
```rust
let mut stream = /* ... */;

while let Some(result) = stream.next().await {
    match result {
        Ok(msg) => {
            println!("Sequence {}: {:?}", msg.sequence, msg.payload);
            if msg.is_final { break; }
        }
        Err(e) => {
            eprintln!("Stream error: {:?}", e);
            break;
        }
    }
}

// Get cursor for resumption
let cursor = stream.get_cursor();
```

---

## Structs (Data Types)

### Session

**Purpose**: Individual session state with expiration and cursor tracking.

**Fields**:
```rust
pub struct Session {
    pub id: SessionId,
    pub created_at: DateTime<Utc>,
    pub last_accessed: DateTime<Utc>,
    pub expires_at: DateTime<Utc>,
    pub cursor: Option<ResumeCursor>,
    pub metadata: HashMap<String, String>,
}
```

**Methods**:
```rust
pub fn is_expired(&self) -> bool
pub fn touch(&mut self, ttl: Duration)
pub fn set_cursor(&mut self, cursor: ResumeCursor)
pub fn get_cursor(&self) -> Option<&ResumeCursor>
```

---

### ResumeCursor

**Purpose**: Serializable position marker for stream resumption.

**Fields**:
```rust
pub struct ResumeCursor {
    pub session_id: SessionId,
    pub position: u64,
    pub timestamp: DateTime<Utc>,
    pub metadata: HashMap<String, String>,
}
```

**Methods**:
```rust
pub fn to_string(&self) -> String
pub fn from_string(s: &str) -> Result<Self, TransportError>
```

---

### A2aMessage

**Purpose**: Non-streaming A2A message with routing metadata.

**Fields**:
```rust
pub struct A2aMessage {
    pub id: String,
    pub from_agent: String,
    pub to_agent: String,
    pub message_type: A2aMessageType,
    pub payload: serde_json::Value,
    pub session_id: Option<SessionId>,
    pub origin: Option<String>,
    pub correlation_id: Option<String>,
}
```

---

### A2aStreamMessage

**Purpose**: Streaming A2A message for opening a stream channel.

**Fields**:
```rust
pub struct A2aStreamMessage {
    pub id: String,
    pub from_agent: String,
    pub to_agent: String,
    pub session_id: SessionId,
    pub stream_type: A2aStreamType,
    pub metadata: HashMap<String, String>,
}
```

---

## Enums

### A2aMessageType

**Purpose**: Message type classification.

**Variants**:
```rust
pub enum A2aMessageType {
    Request,
    Response,
    Event,
    Command,
    Query,
    Notification,
}
```

### A2aStreamType

**Purpose**: Streaming direction classification.

**Variants**:
```rust
pub enum A2aStreamType {
    Bidirectional,
    ServerStream,
    ClientStream,
}
```

### TransportError

**Purpose**: Transport-layer error types.

**Variants** (15 total):
- `ConnectionError` - Connection failed
- `ProtocolError` - Protocol violation
- `MessageFormatError` - Invalid message format
- `SessionNotFound` - Session does not exist
- `SessionExpired` - Session TTL exceeded
- `OriginValidationError` - Invalid origin
- `StreamError` - Stream operation failed
- `SerializationError` - Serialize/deserialize failed
- `IoError` - I/O operation failed
- `TimeoutError` - Operation timed out
- `CursorError` - Invalid cursor
- `AuthenticationError` - Auth failed
- `AuthorizationError` - Not authorized
- `RateLimitError` - Rate limit exceeded
- `InternalError` - Internal error

---

## See Also

- [Transport Source](../../../../../crates/ggen-transport/src/)
- [Session Management](../../../../../crates/ggen-transport/src/session.rs)
- [Streaming Implementation](../../../../../crates/ggen-transport/src/streaming.rs)
