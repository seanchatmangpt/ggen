# Configuration Patterns

Guide to configuration file and environment variable patterns for CLI applications.

## Configuration Hierarchy

CLI applications typically support three levels of configuration (in priority order):

1. **Command-line flags** (highest priority)
2. **Environment variables**
3. **Configuration files** (lowest priority)

## Configuration Files

### TOML Configuration

**Specification:**
```turtle
:App cli:hasConfig [
    a cli:ConfigFile ;
    cli:configFormat "toml" ;
    cli:configPath "~/.myapp/config.toml"
] .
```

**Generated config file:**
```toml
[general]
verbose = false
output_dir = "/tmp/output"

[server]
host = "localhost"
port = 8080
workers = 4

[database]
url = "postgresql://localhost/mydb"
pool_size = 10
```

**Rust code:**
```rust
use serde::Deserialize;

#[derive(Debug, Deserialize)]
struct Config {
    general: GeneralConfig,
    server: ServerConfig,
    database: DatabaseConfig,
}

#[derive(Debug, Deserialize)]
struct GeneralConfig {
    verbose: bool,
    output_dir: String,
}

impl Config {
    fn load(path: &Path) -> Result<Self> {
        let content = fs::read_to_string(path)?;
        toml::from_str(&content)
    }
}
```

### YAML Configuration

**Specification:**
```turtle
:App cli:hasConfig [
    a cli:ConfigFile ;
    cli:configFormat "yaml" ;
    cli:configPath "./config.yaml"
] .
```

**Generated config file:**
```yaml
general:
  verbose: false
  output_dir: /tmp/output

server:
  host: localhost
  port: 8080
  workers: 4

database:
  url: postgresql://localhost/mydb
  pool_size: 10
```

**TypeScript code:**
```typescript
import yaml from 'js-yaml';
import fs from 'fs/promises';

interface Config {
  general: {
    verbose: boolean;
    output_dir: string;
  };
  server: {
    host: string;
    port: number;
    workers: number;
  };
}

async function loadConfig(path: string): Promise<Config> {
  const content = await fs.readFile(path, 'utf-8');
  return yaml.load(content) as Config;
}
```

### JSON Configuration

**Specification:**
```turtle
:App cli:hasConfig [
    a cli:ConfigFile ;
    cli:configFormat "json" ;
    cli:configPath "./config.json"
] .
```

**Generated config file:**
```json
{
  "general": {
    "verbose": false,
    "output_dir": "/tmp/output"
  },
  "server": {
    "host": "localhost",
    "port": 8080,
    "workers": 4
  },
  "database": {
    "url": "postgresql://localhost/mydb",
    "pool_size": 10
  }
}
```

**Python code:**
```python
import json
from pathlib import Path
from typing import Dict, Any

def load_config(path: Path) -> Dict[str, Any]:
    with open(path, 'r') as f:
        return json.load(f)
```

## Environment Variables

### Prefixed Environment Variables

**Specification:**
```turtle
:App cli:hasConfig [
    a cli:EnvVar ;
    cli:envVarPrefix "MYAPP_"
] .
```

**Usage:**
```bash
export MYAPP_VERBOSE=true
export MYAPP_OUTPUT_DIR=/tmp/output
export MYAPP_SERVER_HOST=localhost
export MYAPP_SERVER_PORT=8080
```

**Rust code:**
```rust
use std::env;

fn load_from_env(prefix: &str) -> Result<Config> {
    let verbose = env::var(format!("{prefix}VERBOSE"))
        .ok()
        .and_then(|v| v.parse().ok())
        .unwrap_or(false);

    let output_dir = env::var(format!("{prefix}OUTPUT_DIR"))
        .unwrap_or_else(|_| "/tmp/output".to_string());

    Ok(Config {
        verbose,
        output_dir,
    })
}
```

### Specific Environment Variables

**Specification:**
```turtle
:VerboseOpt cli:optionLong "verbose" ;
            cli:hasConfig [
                a cli:EnvVar ;
                cli:envVarName "DEBUG"
            ] .
```

Maps `--verbose` option to `DEBUG` environment variable.

## Configuration Merging

### Priority Example (Rust)

```rust
pub struct AppConfig {
    pub verbose: bool,
    pub output_dir: String,
}

impl AppConfig {
    pub fn load(cli: &Cli) -> Result<Self> {
        // 1. Start with defaults
        let mut config = Self::default();

        // 2. Load from config file if specified
        if let Some(config_path) = &cli.config {
            let file_config = Self::from_file(config_path)?;
            config.merge(file_config);
        }

        // 3. Override with environment variables
        if let Ok(value) = env::var("MYAPP_VERBOSE") {
            config.verbose = value.parse()?;
        }
        if let Ok(value) = env::var("MYAPP_OUTPUT_DIR") {
            config.output_dir = value;
        }

        // 4. Override with CLI flags (highest priority)
        if cli.verbose {
            config.verbose = true;
        }
        if let Some(ref dir) = cli.output_dir {
            config.output_dir = dir.clone();
        }

        Ok(config)
    }
}
```

### Priority Example (TypeScript)

```typescript
interface AppConfig {
  verbose: boolean;
  outputDir: string;
}

async function loadConfig(options: any): Promise<AppConfig> {
  // 1. Defaults
  const config: AppConfig = {
    verbose: false,
    outputDir: '/tmp/output',
  };

  // 2. Config file
  if (options.config) {
    const fileConfig = await loadConfigFile(options.config);
    Object.assign(config, fileConfig);
  }

  // 3. Environment variables
  if (process.env.MYAPP_VERBOSE) {
    config.verbose = process.env.MYAPP_VERBOSE === 'true';
  }
  if (process.env.MYAPP_OUTPUT_DIR) {
    config.outputDir = process.env.MYAPP_OUTPUT_DIR;
  }

  // 4. CLI flags
  if (options.verbose) {
    config.verbose = true;
  }
  if (options.outputDir) {
    config.outputDir = options.outputDir;
  }

  return config;
}
```

## Configuration Patterns

### 1. User vs Project Config

```turtle
:App cli:hasConfig [
    a cli:ConfigFile ;
    cli:configFormat "toml" ;
    cli:configPath "~/.myapp/config.toml"  # User config
] , [
    a cli:ConfigFile ;
    cli:configFormat "toml" ;
    cli:configPath "./myapp.toml"  # Project config
] .
```

Load both and merge:
```rust
let user_config = Config::load("~/.myapp/config.toml")?;
let project_config = Config::load("./myapp.toml")?;
let merged = user_config.merge(project_config);
```

### 2. Config File Auto-Discovery

Search for config files in multiple locations:

```rust
fn find_config() -> Option<PathBuf> {
    let candidates = vec![
        Path::new("./myapp.toml"),
        Path::new("./.myapp.toml"),
        Path::new("~/.myapp/config.toml"),
        Path::new("~/.config/myapp/config.toml"),
    ];

    candidates.into_iter()
        .find(|p| p.exists())
        .map(|p| p.to_path_buf())
}
```

### 3. Config Validation

**Specification:**
```turtle
:ConfigFile cli:hasValidator [
    cli:validatorType "schema" ;
    cli:validatorPattern "path/to/schema.json"
] .
```

**Rust validation:**
```rust
use jsonschema::JSONSchema;

fn validate_config(config: &Config) -> Result<()> {
    let schema = include_str!("schema.json");
    let compiled = JSONSchema::compile(&serde_json::from_str(schema)?)?;

    let config_json = serde_json::to_value(config)?;
    if let Err(errors) = compiled.validate(&config_json) {
        for error in errors {
            eprintln!("Config validation error: {}", error);
        }
        bail!("Invalid configuration");
    }

    Ok(())
}
```

### 4. Config Generation

Generate default config file:

```rust
#[derive(Subcommand)]
enum Commands {
    /// Generate default configuration file
    InitConfig {
        #[arg(short, long)]
        output: Option<PathBuf>,
    },
}

fn init_config(output: Option<PathBuf>) -> Result<()> {
    let config = Config::default();
    let toml = toml::to_string_pretty(&config)?;

    let path = output.unwrap_or_else(|| {
        Path::new("~/.myapp/config.toml").to_path_buf()
    });

    fs::create_dir_all(path.parent().unwrap())?;
    fs::write(&path, toml)?;

    println!("Created config file: {}", path.display());
    Ok(())
}
```

### 5. Secrets Management

**Never store secrets in config files!**

Use environment variables or secret management:

```turtle
:DBUrl cli:optionLong "database-url" ;
       cli:hasConfig [
           a cli:EnvVar ;
           cli:envVarName "DATABASE_URL"
       ] .
```

```bash
# Good: Use env vars for secrets
export DATABASE_URL=postgresql://user:pass@localhost/db

# Bad: Don't put in config.toml
# database_url = "postgresql://user:pass@localhost/db"
```

### 6. Config Profiles

Support multiple profiles (dev, staging, prod):

```toml
[profiles.dev]
verbose = true
database_url = "postgresql://localhost/mydb_dev"

[profiles.staging]
verbose = false
database_url = "postgresql://staging.example.com/mydb"

[profiles.prod]
verbose = false
database_url = "postgresql://prod.example.com/mydb"
```

```bash
myapp --profile prod run
```

### 7. Interactive Config Wizard

```turtle
:InitCmd cli:hasPrompt [
    cli:promptType "input" ;
    cli:promptMessage "Database host:" ;
    cli:promptName "db_host"
] , [
    cli:promptType "password" ;
    cli:promptMessage "Database password:" ;
    cli:promptName "db_password"
] , [
    cli:promptType "select" ;
    cli:promptMessage "Environment:" ;
    cli:promptChoices "dev,staging,prod" ;
    cli:promptName "environment"
] .
```

## Best Practices

1. **Defaults:**
   - Provide sensible defaults for all options
   - Document defaults in help text
   - Make zero-config possible for simple use cases

2. **File Locations:**
   - User config: `~/.config/myapp/config.toml`
   - Project config: `./myapp.toml` or `./.myapp.toml`
   - System config: `/etc/myapp/config.toml`

3. **Environment Variables:**
   - Use consistent prefix (e.g., `MYAPP_`)
   - Use SCREAMING_SNAKE_CASE
   - Document all supported env vars

4. **Secrets:**
   - Never commit secrets to version control
   - Use environment variables or secret managers
   - Warn if secrets found in config files

5. **Validation:**
   - Validate config on load
   - Provide helpful error messages
   - Use JSON Schema or similar for validation

6. **Migration:**
   - Support config file migration
   - Warn about deprecated options
   - Provide migration commands

7. **Documentation:**
   - Document all config options
   - Provide example config files
   - Show config precedence clearly

## Example: Complete Config System

```rust
use serde::{Deserialize, Serialize};
use std::path::{Path, PathBuf};
use anyhow::Result;

#[derive(Debug, Serialize, Deserialize)]
pub struct Config {
    pub general: GeneralConfig,
    pub server: ServerConfig,
}

#[derive(Debug, Serialize, Deserialize)]
pub struct GeneralConfig {
    pub verbose: bool,
    pub output_dir: String,
}

#[derive(Debug, Serialize, Deserialize)]
pub struct ServerConfig {
    pub host: String,
    pub port: u16,
}

impl Config {
    pub fn load(cli: &Cli) -> Result<Self> {
        // Start with defaults
        let mut config = Self::default();

        // Load from config file
        if let Some(path) = Self::find_config_file() {
            let file_config = Self::from_file(&path)?;
            config.merge(file_config);
        }

        // Override with environment
        config.merge_env("MYAPP_")?;

        // Override with CLI
        config.merge_cli(cli);

        // Validate
        config.validate()?;

        Ok(config)
    }

    fn find_config_file() -> Option<PathBuf> {
        ["./myapp.toml", "~/.config/myapp/config.toml"]
            .iter()
            .map(Path::new)
            .find(|p| p.exists())
            .map(|p| p.to_path_buf())
    }

    fn from_file(path: &Path) -> Result<Self> {
        let content = std::fs::read_to_string(path)?;
        Ok(toml::from_str(&content)?)
    }

    fn merge(&mut self, other: Self) {
        // Merge logic
    }

    fn merge_env(&mut self, prefix: &str) -> Result<()> {
        // Environment variable merging
        Ok(())
    }

    fn merge_cli(&mut self, cli: &Cli) {
        // CLI flag merging
    }

    fn validate(&self) -> Result<()> {
        // Validation logic
        Ok(())
    }
}

impl Default for Config {
    fn default() -> Self {
        Self {
            general: GeneralConfig {
                verbose: false,
                output_dir: "/tmp/output".to_string(),
            },
            server: ServerConfig {
                host: "localhost".to_string(),
                port: 8080,
            },
        }
    }
}
```

This comprehensive configuration system handles all common patterns and provides a solid foundation for CLI applications.
