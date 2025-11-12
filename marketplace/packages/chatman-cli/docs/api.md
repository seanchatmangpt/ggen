# API Reference

## Core Types

### `ChatManager`

Main conversation manager.

```rust
pub struct ChatManager {
    config: Config,
    history: Vec<Message>,
}
```

**Methods**:

- `new(config: Config) -> Result<Self>` - Create new manager
- `send_message(&mut self, message: Message) -> Result<Message>` - Send message
- `history(&self) -> &[Message]` - Get conversation history
- `clear_history(&mut self)` - Clear history
- `export_json(&self) -> Result<String>` - Export to JSON
- `add_hook(&mut self, hook: KnowledgeHook) -> Result<()>` - Add knowledge hook

### `Config`

Configuration structure.

```rust
pub struct Config {
    pub provider: String,
    pub model: String,
    pub max_history: usize,
    pub timeout_secs: u64,
    pub retry_attempts: u32,
    pub ontology_path: Option<PathBuf>,
}
```

### `Message`

Chat message structure.

```rust
pub struct Message {
    pub role: String,
    pub content: String,
    pub timestamp: Option<i64>,
}
```

**Constructors**:

- `Message::user(content: impl Into<String>) -> Self`
- `Message::assistant(content: impl Into<String>) -> Self`
- `Message::system(content: impl Into<String>) -> Self`

### `KnowledgeHook`

RDF ontology integration.

```rust
pub struct KnowledgeHook {
    ontology_path: PathBuf,
}
```

**Methods**:

- `from_ontology(path: impl Into<PathBuf>) -> Result<Self>`

## CLI Commands

### `chatman chat`

Start interactive chat session.

**Options**:
- `-p, --provider <PROVIDER>` - AI provider
- `-m, --model <MODEL>` - Model name
- `-e, --export <FILE>` - Export conversation

### `chatman ask`

Ask single question.

**Arguments**:
- `<QUESTION>` - Question to ask

**Options**:
- `-p, --provider <PROVIDER>` - AI provider
- `-m, --model <MODEL>` - Model name

### `chatman info`

Show version and system information.

## Examples

See [examples/](../examples/) directory for complete examples.
