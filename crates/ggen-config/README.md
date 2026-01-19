# ggen-config

[![Crates.io](https://img.shields.io/crates/v/ggen-config.svg)](https://crates.io/crates/ggen-config)
[![Documentation](https://docs.rs/ggen-config/badge.svg)](https://docs.rs/ggen-config)

Configuration parser and validator for `ggen.toml` files.

## Features

- **Type-safe parsing**: Strongly-typed Rust structs with serde
- **Schema validation**: Validates configuration against expected schema
- **Environment overrides**: Support for environment-specific configs (development, staging, production)
- **Workspace support**: Mono-repo and workspace configuration
- **Automatic discovery**: Searches upward through directories to find `ggen.toml`
- **Comprehensive error handling**: Detailed error types with context
- **Well-tested**: 24 tests with 100% pass rate
- **Documented**: Full rustdoc coverage for all public APIs

## Installation

Add this to your `Cargo.toml`:

```toml
[dependencies]
ggen-config = { version = "3.2.0", path = "../ggen-config" }
```

## Usage

### Basic Loading

```rust
use ggen_config::{ConfigLoader, ConfigValidator};

// Load configuration from file
let config = ConfigLoader::from_file("ggen.toml")?;

// Validate the configuration
ConfigValidator::validate(&config)?;

// Access configuration
println!("Project: {}", config.project.name);
if let Some(ai) = &config.ai {
    println!("AI Provider: {}", ai.provider);
}
```

### Auto-Discovery

```rust
// Automatically find and load ggen.toml from current or parent directories
let config = ConfigLoader::find_and_load()?;
```

### Environment-Specific Configuration

```rust
// Load with environment overrides
let loader = ConfigLoader::new("ggen.toml")?;
let dev_config = loader.load_with_env("development")?;
let prod_config = loader.load_with_env("production")?;
```

### Parsing from String

```rust
let toml = r#"
    [project]
    name = "my-project"
    version = "1.0.0"

    [ai]
    provider = "openai"
    model = "gpt-4"
"#;

let config = ConfigLoader::from_str(toml)?;
```

## Configuration Schema

### Minimal Configuration

```toml
[project]
name = "my-project"
version = "1.0.0"
```

### Full Configuration Example

```toml
[project]
name = "advanced-project"
version = "2.0.0"
description = "A comprehensive example"
authors = ["Developer <dev@example.com>"]

[ai]
provider = "openai"
model = "gpt-4"
temperature = 0.7
max_tokens = 2000
timeout = 30

[ai.prompts]
system = "You are an expert developer"
user_prefix = "Generate code with the following requirements:"

[ai.validation]
enabled = true
quality_threshold = 0.8
max_iterations = 3

[templates]
directory = "templates"
output_directory = "generated"
backup_enabled = true
idempotent = true

[rdf]
base_uri = "https://example.com/project/"
prefixes = { ex = "https://example.com/project/", schema = "http://schema.org/" }

[sparql]
timeout = 10
max_results = 1000
cache_enabled = true

[lifecycle]
enabled = true
config_file = "make.toml"
cache_directory = ".ggen/cache"
state_file = ".ggen/state.json"

[lifecycle.phases]
default = ["init", "build", "test"]
production = ["build", "test", "deploy"]

[security]
path_traversal_protection = true
shell_injection_protection = true
template_sandboxing = true

[performance]
parallel_execution = true
max_workers = 16
cache_size = "1GB"

[logging]
level = "info"
format = "json"
file = "logs/ggen.log"
rotation = "daily"

[features]
ai_generation = true
sparql_queries = true
lifecycle_management = true

# Environment-specific overrides
[env.development]
"ai.model" = "gpt-3.5-turbo"
"ai.temperature" = 0.9
"logging.level" = "debug"

[env.production]
"ai.temperature" = 0.3
"logging.level" = "warn"
"security.require_confirmation" = true
```

## Supported Configuration Sections

### Project (`[project]`)

- `name` - Project name (required)
- `version` - Semantic version (required)
- `description` - Project description
- `authors` - List of authors
- `license` - License identifier
- `repository` - Repository URL

### AI (`[ai]`)

- `provider` - AI provider (openai, ollama, anthropic, etc.)
- `model` - Model name
- `temperature` - Generation temperature (0.0-1.0, default: 0.7)
- `max_tokens` - Maximum tokens (default: 2000)
- `timeout` - Request timeout in seconds (default: 30)

### Templates (`[templates]`)

- `directory` - Template source directory
- `output_directory` - Output directory for generated files
- `backup_enabled` - Enable backups before overwriting
- `idempotent` - Only update if content changed

### RDF (`[rdf]`)

- `base_uri` / `base_iri` - Base IRI for RDF entities
- `prefixes` - Namespace prefix mappings
- `default_format` - RDF serialization format
- `cache_queries` - Enable query caching

### SPARQL (`[sparql]`)

- `timeout` - Query timeout in seconds
- `max_results` - Maximum query results
- `cache_enabled` - Enable query caching

### Lifecycle (`[lifecycle]`)

- `enabled` - Enable lifecycle management
- `config_file` - Lifecycle config file path
- `cache_directory` - Cache directory
- `state_file` - State file path
- `phases` - Named phase sequences

### Security (`[security]`)

- `path_traversal_protection` - Prevent path traversal attacks
- `shell_injection_protection` - Prevent shell injection
- `template_sandboxing` - Sandbox template execution
- `require_confirmation` - Require user confirmation
- `audit_operations` - Audit all operations

### Performance (`[performance]`)

- `parallel_execution` - Enable parallel execution
- `max_workers` - Maximum parallel workers
- `cache_size` - Cache size limit
- `enable_profiling` - Enable performance profiling

### Logging (`[logging]`)

- `level` - Log level (trace, debug, info, warn, error)
- `format` - Log format (json, text, pretty)
- `file` - Log file path
- `rotation` - Log rotation strategy

## Validation

The validator checks for:

- Valid semantic versioning
- AI temperature within range (0.0-1.0)
- Valid AI providers
- Valid log levels and formats
- Non-empty required fields
- Valid size formats (e.g., "1GB", "512MB")

```rust
use ggen_config::{ConfigValidator, ConfigError};

match ConfigValidator::validate(&config) {
    Ok(_) => println!("Configuration is valid"),
    Err(ConfigError::Validation(msg)) => {
        eprintln!("Validation errors: {}", msg);
    }
    Err(e) => eprintln!("Error: {}", e),
}
```

## Error Handling

The crate provides comprehensive error types:

- `ConfigError::FileNotFound` - Configuration file not found
- `ConfigError::Io` - I/O error reading file
- `ConfigError::TomlParse` - TOML parsing error
- `ConfigError::Validation` - Schema validation error
- `ConfigError::InvalidValue` - Invalid field value
- `ConfigError::MissingField` - Required field missing

## Features

### Optional Features

- `graph` - Enable graph integration with `ggen-core`
- `ontology` - Enable ontology integration with `ggen-domain`
- `full` - Enable all optional features

```toml
[dependencies]
ggen-config = { version = "3.2.0", features = ["full"] }
```

## Testing

The crate includes comprehensive tests:

- **10 unit tests** - Core parsing and validation logic
- **9 integration tests** - End-to-end scenarios with real configs
- **5 doc tests** - Documentation examples

Run tests with:

```bash
cargo test -p ggen-config
```

## Examples

See the `tests/integration_test.rs` file for comprehensive examples including:

- Loading real-world configs from examples
- Environment override handling
- Validation error scenarios
- Default value population

## License

MIT

## Contributing

This crate is part of the `ggen` workspace. Contributions are welcome!

## Related Crates

- `ggen-core` - Core functionality for graph operations
- `ggen-domain` - Domain models and ontology support
- `ggen-cli` - Command-line interface using this config parser
