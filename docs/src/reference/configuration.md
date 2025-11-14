# Configuration Reference

Complete reference for ggen configuration options.

## Configuration Files

### Global Configuration

Location: `~/.config/ggen/config.toml`

```toml
[cache]
dir = "~/.cache/ggen"
ttl = 3600

[marketplace]
registry_url = "https://registry.ggen.io"
timeout = 30

[ai]
provider = "anthropic"
model = "claude-3-5-sonnet-20241022"
api_key_env = "ANTHROPIC_API_KEY"
```

### Project Configuration

Location: `.ggen/config.toml` (project root)

```toml
[project]
name = "my-project"
ontology = "domain.ttl"

[templates]
dir = "templates"
cache = true

[generation]
output_dir = "src"
deterministic = true
```

## Environment Variables

### Cache

```bash
export GGEN_CACHE_DIR="$HOME/.cache/ggen"
```

### Marketplace

```bash
export GGEN_REGISTRY_URL="https://registry.ggen.io"
export GGEN_MARKETPLACE_TIMEOUT="30"
```

### AI Providers

```bash
# Anthropic
export ANTHROPIC_API_KEY="sk-ant-..."

# OpenAI
export OPENAI_API_KEY="sk-..."

# Ollama (local)
export OLLAMA_BASE_URL="http://localhost:11434"
```

### Logging

```bash
export RUST_LOG="ggen=debug"
export GGEN_LOG_LEVEL="debug"
```

## Configuration Precedence

1. Command-line arguments (highest priority)
2. Project configuration (`.ggen/config.toml`)
3. Global configuration (`~/.config/ggen/config.toml`)
4. Environment variables
5. Default values (lowest priority)

## Configuration Sections

### Cache Configuration

```toml
[cache]
dir = "~/.cache/ggen"      # Cache directory
ttl = 3600                 # Time-to-live in seconds
max_size = "1GB"           # Maximum cache size
```

### Marketplace Configuration

```toml
[marketplace]
registry_url = "https://registry.ggen.io"
timeout = 30               # Request timeout in seconds
retry_count = 3            # Retry attempts
```

### AI Configuration

```toml
[ai]
provider = "anthropic"     # Provider: anthropic, openai, ollama
model = "claude-3-5-sonnet-20241022"
api_key_env = "ANTHROPIC_API_KEY"
temperature = 0.7
max_tokens = 4096
```

### Generation Configuration

```toml
[generation]
output_dir = "src"          # Default output directory
deterministic = true        # Enable deterministic generation
seed = null                 # Random seed (null = auto)
```

## Command-Line Overrides

All configuration can be overridden via command-line:

```bash
# Override cache directory
ggen template generate-rdf --cache-dir /tmp/cache ...

# Override registry URL
ggen marketplace search --registry-url https://custom.registry.io ...

# Override AI provider
ggen ai generate-ontology --provider ollama --model llama2 ...
```

## See Also

- [Installation Guide](../how-to-guides/installation.md)
- [CLI Reference](cli.md)
- [Troubleshooting Guide](../how-to-guides/troubleshoot.md)

