# AI Provider Integration

## Supported Providers

ChatMan CLI supports multiple AI providers through a unified interface.

### 1. OpenAI

```rust
let config = Config {
    provider: "openai".to_string(),
    model: "gpt-4".to_string(),
    ..Default::default()
};
```

**Environment Variables**:
```bash
export OPENAI_API_KEY=sk-...
export CHATMAN_PROVIDER=openai
export CHATMAN_MODEL=gpt-4
```

**Models**:
- `gpt-3.5-turbo` - Fast, cost-effective
- `gpt-4` - Most capable
- `gpt-4-turbo` - Latest improvements

### 2. Anthropic

```rust
let config = Config {
    provider: "anthropic".to_string(),
    model: "claude-3-opus".to_string(),
    ..Default::default()
};
```

**Environment Variables**:
```bash
export ANTHROPIC_API_KEY=sk-ant-...
export CHATMAN_PROVIDER=anthropic
export CHATMAN_MODEL=claude-3-opus
```

**Models**:
- `claude-3-opus` - Most intelligent
- `claude-3-sonnet` - Balanced
- `claude-3-haiku` - Fastest

### 3. Local LLM

```rust
let config = Config {
    provider: "local".to_string(),
    model: "llama-2".to_string(),
    ..Default::default()
};
```

**Supported Engines**:
- llama.cpp
- Ollama
- Custom endpoints

## Custom Provider

Implement the provider trait:

```rust
pub trait AIProvider {
    async fn send_message(&self, message: &Message) -> Result<Message>;
    fn supports_streaming(&self) -> bool;
}
```

## Configuration

### Rate Limiting

```rust
let config = Config {
    retry_attempts: 3,
    timeout_secs: 30,
    ..Default::default()
};
```

### Streaming (Future Feature)

```rust
let stream = manager.send_message_stream(message).await?;
while let Some(chunk) = stream.next().await {
    print!("{}", chunk?);
}
```

## Error Handling

All providers use consistent error types:

```rust
match manager.send_message(msg).await {
    Ok(response) => println!("{}", response.content),
    Err(e) => eprintln!("Error: {}", e),
}
```

## Best Practices

1. **API Key Security**: Never commit API keys
2. **Rate Limits**: Respect provider limits
3. **Error Handling**: Always handle errors gracefully
4. **Timeouts**: Set appropriate timeouts
5. **Retries**: Use exponential backoff
