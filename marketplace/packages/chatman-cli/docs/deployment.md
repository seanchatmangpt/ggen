# Deployment Guide

## Production Deployment

### 1. Build for Release

```bash
cargo build --release --package chatman-cli
```

Binary location: `target/release/chatman`

### 2. Installation

#### System-wide Installation

```bash
cargo install --path .
# Binary installed to ~/.cargo/bin/chatman
```

#### Docker Deployment

```dockerfile
FROM rust:1.75 as builder
WORKDIR /app
COPY . .
RUN cargo build --release --package chatman-cli

FROM debian:bookworm-slim
COPY --from=builder /app/target/release/chatman /usr/local/bin/
CMD ["chatman", "info"]
```

### 3. Configuration

#### Environment Variables

```bash
# Required
export OPENAI_API_KEY=sk-...

# Optional
export CHATMAN_PROVIDER=openai
export CHATMAN_MODEL=gpt-4
export CHATMAN_MAX_HISTORY=100
export CHATMAN_TIMEOUT=30
export RUST_LOG=chatman_cli=info
```

#### Config File

Create `~/.config/chatman/config.yaml`:

```yaml
provider: openai
model: gpt-4
max_history: 100
timeout: 30
retry_attempts: 3

knowledge_hooks:
  - path: /etc/chatman/ontology.ttl
    enabled: true

output:
  format: colored
  theme: dark
```

### 4. Systemd Service (Linux)

```ini
[Unit]
Description=ChatMan CLI Service
After=network.target

[Service]
Type=simple
User=chatman
EnvironmentFile=/etc/chatman/env
ExecStart=/usr/local/bin/chatman chat
Restart=on-failure

[Install]
WantedBy=multi-user.target
```

### 5. Health Checks

```bash
# Check installation
chatman info

# Verify configuration
chatman ask "test" --provider openai
```

## Security Best Practices

1. **API Keys**: Use environment variables or secret management
2. **Permissions**: Run with minimal required permissions
3. **Network**: Restrict outbound connections if possible
4. **Logging**: Sanitize logs to avoid leaking sensitive data
5. **Updates**: Keep dependencies updated

## Monitoring

### Logging

```bash
# Enable debug logging
export RUST_LOG=chatman_cli=debug

# Log to file
chatman chat 2>&1 | tee chatman.log
```

### Metrics

Future enhancement: Prometheus metrics endpoint

## Troubleshooting

### Common Issues

1. **API Key Error**:
   ```bash
   export OPENAI_API_KEY=your-key
   ```

2. **Timeout**:
   ```bash
   export CHATMAN_TIMEOUT=60
   ```

3. **Rate Limit**:
   - Reduce request frequency
   - Implement backoff logic

## Performance Tuning

1. **History Limit**: Reduce for lower memory usage
2. **Timeout**: Adjust based on network latency
3. **Concurrency**: Tokio handles this automatically

## Backup and Recovery

### Export Conversations

```bash
chatman chat --export backup.json
```

### Restore from Backup

Future enhancement: Import functionality
