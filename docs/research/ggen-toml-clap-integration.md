# ggen.toml ↔ clap Integration Patterns

**Research Date:** 2025-11-19
**Researcher:** Hive Mind Swarm - Research Agent
**Focus:** Integration strategies between ggen.toml config and clap CLI parsing

## Executive Summary

This document provides comprehensive integration patterns for merging ggen.toml configuration with clap command-line arguments, addressing:

- **Type sharing** between TOML schema and CLI args
- **Validation strategies** across both layers
- **Precedence rules** (CLI > ENV > TOML > defaults)
- **Environment variable expansion** and coercion
- **Performance optimization** for config loading

**Key Recommendations:**
1. Use shared types with both `serde` + `clap` derives
2. Custom value parsers for validation
3. Lazy config loading (only when needed)
4. Clear precedence: CLI args > ENV vars > ggen.toml > defaults

---

## Current ggen Architecture

### Separation of Concerns

```
┌────────────────────────────────────────┐
│  CLI Layer (crates/ggen-cli)           │
│  - clap-noun-verb auto-discovery       │
│  - Argument parsing                    │
│  - Command routing                     │
└──────────────┬─────────────────────────┘
               │
               │ Loads config when needed
               ▼
┌────────────────────────────────────────┐
│  Config Layer (crates/ggen-config)     │
│  - ggen.toml parsing                   │
│  - Schema validation                   │
│  - Type-safe config structs            │
└──────────────┬─────────────────────────┘
               │
               │ Used by domain logic
               ▼
┌────────────────────────────────────────┐
│  Domain Layer (crates/ggen-domain)     │
│  - Business logic                      │
│  - Template processing                 │
│  - Code generation                     │
└────────────────────────────────────────┘
```

### Current Implementation

**CLI Entry Point:**
```rust
// crates/ggen-cli/src/lib.rs
use clap_noun_verb::run;

pub async fn cli_match() -> ggen_utils::error::Result<()> {
    // clap-noun-verb auto-discovery
    clap_noun_verb::run()
        .map_err(|e| Error::new(&format!("CLI execution failed: {}", e)))?;
    Ok(())
}
```

**Config Loading:**
```rust
// crates/ggen-config/src/parser.rs
impl ConfigLoader {
    pub fn from_file(path: &str) -> Result<GgenConfig, ConfigError> {
        let contents = std::fs::read_to_string(path)?;
        let config: GgenConfig = toml::from_str(&contents)?;
        Ok(config)
    }
}
```

**Verb Function Pattern:**
```rust
// crates/ggen-cli/src/cmds/template.rs
use clap_noun_verb_macros::verb;

#[verb]
fn generate(
    template: String,
    output: Option<String>,
    #[flag] dry_run: bool,
) -> Result<GenerateOutput> {
    // Load config inside verb
    let config = ConfigLoader::from_file("ggen.toml")?;

    // Use config to drive generation
    let output_dir = output.unwrap_or_else(|| {
        config.templates
            .and_then(|t| t.output_directory)
            .unwrap_or_else(|| "generated".to_string())
    });

    // Generate...
}
```

**Issue:** Config is loaded inside each verb function (inefficient, no CLI override support)

---

## Pattern 1: Shared Types (Recommended)

### Concept

Define types that work with both `serde` (for TOML) and `clap` (for CLI):

```rust
use clap::Args;
use serde::{Deserialize, Serialize};

#[derive(Debug, Clone, Args, Deserialize, Serialize)]
pub struct AiConfig {
    /// AI provider (openai, anthropic, ollama)
    #[arg(long, env = "GGEN_AI_PROVIDER")]
    #[serde(default = "default_provider")]
    pub provider: String,

    /// Model name
    #[arg(long, env = "GGEN_AI_MODEL")]
    #[serde(default = "default_model")]
    pub model: String,

    /// Temperature (0.0-1.0)
    #[arg(long, env = "GGEN_AI_TEMPERATURE", value_parser = validate_temperature)]
    #[serde(default = "default_temperature")]
    pub temperature: f32,

    /// Max tokens
    #[arg(long, env = "GGEN_AI_MAX_TOKENS")]
    #[serde(default = "default_max_tokens")]
    pub max_tokens: u32,
}

fn default_provider() -> String {
    "openai".to_string()
}

fn default_model() -> String {
    "gpt-4".to_string()
}

fn default_temperature() -> f32 {
    0.7
}

fn default_max_tokens() -> u32 {
    2000
}

fn validate_temperature(s: &str) -> Result<f32, String> {
    let temp: f32 = s.parse()
        .map_err(|_| "Temperature must be a number")?;

    if !(0.0..=1.0).contains(&temp) {
        return Err("Temperature must be between 0.0 and 1.0".into());
    }

    Ok(temp)
}
```

### Usage in CLI

```rust
use clap::Parser;

#[derive(Parser)]
struct Cli {
    /// Config file path
    #[arg(short, long, default_value = "ggen.toml")]
    config: String,

    /// AI configuration overrides
    #[command(flatten)]
    ai_overrides: AiConfig,

    #[command(subcommand)]
    command: Commands,
}
```

### Usage in TOML

```toml
# ggen.toml
[ai]
provider = "openai"
model = "gpt-4"
temperature = 0.7
max_tokens = 2000
```

### Merging Logic

```rust
fn load_merged_config(cli: &Cli) -> Result<GgenConfig, ConfigError> {
    // 1. Load base config from TOML
    let mut config = ConfigLoader::from_file(&cli.config)?;

    // 2. Apply CLI overrides (only if provided)
    if cli.ai_overrides.provider != default_provider() {
        config.ai.as_mut().unwrap().provider = cli.ai_overrides.provider.clone();
    }
    if cli.ai_overrides.model != default_model() {
        config.ai.as_mut().unwrap().model = cli.ai_overrides.model.clone();
    }
    // ... other fields

    Ok(config)
}
```

**Pros:**
- ✅ Single source of truth for type definitions
- ✅ Compile-time type safety
- ✅ Automatic validation via custom parsers
- ✅ Environment variable support built-in

**Cons:**
- ⚠️ Requires careful default handling
- ⚠️ More complex merge logic

---

## Pattern 2: Lazy Config Loading with Custom Value Parser

### Concept

Load and validate ggen.toml as a custom clap value type:

```rust
use clap::Parser;
use ggen_config::{GgenConfig, ConfigLoader, ConfigValidator};

#[derive(Parser)]
struct Cli {
    /// Config file path (auto-validates on parse)
    #[arg(short, long, default_value = "ggen.toml", value_parser = load_validated_config)]
    config: GgenConfig,

    #[command(subcommand)]
    command: Commands,
}

fn load_validated_config(path: &str) -> Result<GgenConfig, String> {
    // Load TOML
    let config = ConfigLoader::from_file(path)
        .map_err(|e| format!("Config parse error: {e}"))?;

    // Validate schema
    ConfigValidator::validate(&config)
        .map_err(|e| format!("Config validation error: {e}"))?;

    Ok(config)
}

fn main() {
    let cli = Cli::parse();

    // cli.config is already loaded and validated!
    let config = &cli.config;

    // Use config...
}
```

**Pros:**
- ✅ Config loaded once at startup
- ✅ Validation happens during argument parsing
- ✅ Clean API (config always available)

**Cons:**
- ⚠️ Config loaded even if not needed (e.g., `--help`)
- ⚠️ Harder to apply CLI overrides

**Optimization:**

```rust
// Only load config if command needs it
#[derive(Parser)]
struct Cli {
    /// Config file path (not loaded yet)
    #[arg(short, long, default_value = "ggen.toml")]
    config_path: String,

    #[command(subcommand)]
    command: Commands,
}

fn main() {
    let cli = Cli::parse();

    // Load config only when needed
    let config = match &cli.command {
        Commands::Generate { .. } | Commands::Init { .. } => {
            Some(load_validated_config(&cli.config_path)?)
        }
        Commands::Help => None,
    };

    // Execute command with config
}
```

---

## Pattern 3: Key-Value Override System

### Concept

Allow arbitrary config overrides via `--set key=value`:

```rust
#[derive(Parser)]
struct Cli {
    #[arg(short, long, default_value = "ggen.toml")]
    config: String,

    /// Override config values (e.g., --set ai.model=gpt-4)
    #[arg(long = "set", value_parser = parse_key_value, num_args = 1..)]
    overrides: Vec<(String, String)>,

    #[command(subcommand)]
    command: Commands,
}

fn parse_key_value(s: &str) -> Result<(String, String), String> {
    let parts: Vec<_> = s.splitn(2, '=').collect();
    if parts.len() != 2 {
        return Err(format!("Invalid key=value: {s}"));
    }
    Ok((parts[0].to_string(), parts[1].to_string()))
}

fn apply_overrides(config: &mut GgenConfig, overrides: &[(String, String)]) -> Result<(), String> {
    for (key, value) in overrides {
        match key.as_str() {
            "ai.model" => {
                config.ai.as_mut()
                    .ok_or("No [ai] section in config")?
                    .model = value.clone();
            }
            "ai.temperature" => {
                let temp: f32 = value.parse()
                    .map_err(|_| format!("Invalid temperature: {value}"))?;
                config.ai.as_mut()
                    .ok_or("No [ai] section in config")?
                    .temperature = temp;
            }
            "templates.output_directory" => {
                config.templates.as_mut()
                    .ok_or("No [templates] section in config")?
                    .output_directory = Some(value.clone());
            }
            _ => return Err(format!("Unknown config key: {key}")),
        }
    }
    Ok(())
}
```

**Usage:**
```bash
ggen generate --set ai.model=gpt-3.5-turbo --set ai.temperature=0.9
```

**Pros:**
- ✅ Flexible override mechanism
- ✅ No need to define all args upfront

**Cons:**
- ⚠️ String-based keys (no type safety)
- ⚠️ Manual parsing logic
- ⚠️ No shell completion

---

## Pattern 4: Profile-Based Configuration

### Concept

Support environment-specific configs with `--profile`:

```toml
# ggen.toml
[project]
name = "my-project"

[ai]
model = "gpt-4"
temperature = 0.7

# Environment-specific overrides
[env.development]
ai.model = "gpt-3.5-turbo"
ai.temperature = 0.9
logging.level = "debug"

[env.staging]
ai.model = "gpt-4"
ai.temperature = 0.5
logging.level = "info"

[env.production]
ai.model = "gpt-4"
ai.temperature = 0.3
logging.level = "warn"
```

**Implementation:**

```rust
use clap::Parser;
use config::{Config, File, Environment};

#[derive(Parser)]
struct Cli {
    /// Environment profile (dev, staging, prod)
    #[arg(long, env = "GGEN_ENV", default_value = "development")]
    profile: String,

    #[command(subcommand)]
    command: Commands,
}

fn load_with_profile(profile: &str) -> Result<GgenConfig, ConfigError> {
    let config = Config::builder()
        // 1. Load base config
        .add_source(File::with_name("ggen.toml"))

        // 2. Apply profile-specific overrides
        .add_source(
            File::with_name(&format!("ggen.{}.toml", profile))
                .required(false)
        )

        // 3. Apply environment variables
        .add_source(Environment::with_prefix("GGEN").separator("__"))

        .build()?;

    config.try_deserialize()
}

fn main() {
    let cli = Cli::parse();
    let config = load_with_profile(&cli.profile)?;
    // ...
}
```

**Usage:**
```bash
# Development profile
ggen generate --profile development

# Production profile
export GGEN_ENV=production
ggen generate
```

**Pros:**
- ✅ Clean separation of environment configs
- ✅ Single TOML file with all profiles
- ✅ Easy profile switching

**Cons:**
- ⚠️ Requires `config` crate (additional dependency)
- ⚠️ More complex merging logic

---

## Validation Strategies

### Strategy 1: Custom Value Parsers (clap Layer)

Validate at CLI parse time:

```rust
#[derive(Parser)]
struct Cli {
    /// AI temperature (0.0-1.0)
    #[arg(long, value_parser = validate_temperature)]
    temperature: Option<f32>,
}

fn validate_temperature(s: &str) -> Result<f32, String> {
    let temp: f32 = s.parse()
        .map_err(|_| "Must be a number")?;

    if !(0.0..=1.0).contains(&temp) {
        return Err("Temperature must be between 0.0 and 1.0".into());
    }

    Ok(temp)
}
```

**Pros:**
- ✅ Immediate feedback during parsing
- ✅ Clear error messages
- ✅ No invalid values reach application

**Cons:**
- ⚠️ Only validates CLI args, not TOML

---

### Strategy 2: Validator Trait (Config Layer)

Validate after loading TOML:

```rust
// crates/ggen-config/src/validator.rs
use thiserror::Error;

#[derive(Debug, Error)]
pub enum ValidationError {
    #[error("Temperature must be between 0.0 and 1.0, got {0}")]
    InvalidTemperature(f32),

    #[error("Model name cannot be empty")]
    EmptyModel,

    #[error("Output directory must be a valid path")]
    InvalidOutputPath,
}

pub trait Validatable {
    fn validate(&self) -> Result<(), ValidationError>;
}

impl Validatable for AiConfig {
    fn validate(&self) -> Result<(), ValidationError> {
        // Validate temperature
        if !(0.0..=1.0).contains(&self.temperature) {
            return Err(ValidationError::InvalidTemperature(self.temperature));
        }

        // Validate model name
        if self.model.is_empty() {
            return Err(ValidationError::EmptyModel);
        }

        Ok(())
    }
}

impl Validatable for GgenConfig {
    fn validate(&self) -> Result<(), ValidationError> {
        // Validate project
        if self.project.name.is_empty() {
            return Err(ValidationError::EmptyProjectName);
        }

        // Validate AI config (if present)
        if let Some(ref ai) = self.ai {
            ai.validate()?;
        }

        // Validate templates config (if present)
        if let Some(ref templates) = self.templates {
            templates.validate()?;
        }

        Ok(())
    }
}

// Usage
let config = ConfigLoader::from_file("ggen.toml")?;
config.validate()?;  // Errors if invalid
```

**Pros:**
- ✅ Comprehensive validation
- ✅ Cross-field validation possible
- ✅ Reusable across CLI and library

**Cons:**
- ⚠️ Validation happens after parsing (TOML already loaded)
- ⚠️ More code to maintain

---

### Strategy 3: Constraint Types (Type System)

Encode constraints in types:

```rust
use std::ops::RangeInclusive;

/// Temperature constrained to 0.0-1.0
#[derive(Debug, Clone, Copy)]
pub struct Temperature(f32);

impl Temperature {
    const RANGE: RangeInclusive<f32> = 0.0..=1.0;

    pub fn new(value: f32) -> Result<Self, String> {
        if Self::RANGE.contains(&value) {
            Ok(Temperature(value))
        } else {
            Err(format!("Temperature must be in {:?}", Self::RANGE))
        }
    }

    pub fn value(&self) -> f32 {
        self.0
    }
}

// Implement FromStr for clap
impl std::str::FromStr for Temperature {
    type Err = String;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        let val: f32 = s.parse()
            .map_err(|_| "Temperature must be a number")?;
        Temperature::new(val)
    }
}

// Implement Deserialize for serde
impl<'de> serde::Deserialize<'de> for Temperature {
    fn deserialize<D>(deserializer: D) -> Result<Self, D::Error>
    where
        D: serde::Deserializer<'de>,
    {
        let val = f32::deserialize(deserializer)?;
        Temperature::new(val).map_err(serde::de::Error::custom)
    }
}

// Implement Serialize for serde
impl serde::Serialize for Temperature {
    fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
    where
        S: serde::Serializer,
    {
        serializer.serialize_f32(self.0)
    }
}

// Use in config
#[derive(Debug, Clone, Deserialize, Serialize)]
pub struct AiConfig {
    pub provider: String,
    pub model: String,
    pub temperature: Temperature,  // ← Always valid!
}

// Use in CLI
#[derive(Parser)]
struct Cli {
    #[arg(long)]
    temperature: Temperature,  // ← Validated during parsing!
}
```

**Pros:**
- ✅ Impossible to construct invalid values
- ✅ Type-driven validation
- ✅ Works with both clap and serde

**Cons:**
- ⚠️ More boilerplate per constraint type
- ⚠️ Harder to serialize to/from primitives

---

## Environment Variable Expansion

### Pattern 1: Explicit Expansion (shellexpand)

```toml
# ggen.toml
[ai]
api_key = "${OPENAI_API_KEY}"
model = "${AI_MODEL:-gpt-4}"  # with default
```

```rust
use shellexpand;

impl GgenConfig {
    pub fn expand_env_vars(&mut self) -> Result<(), String> {
        if let Some(ref mut ai) = self.ai {
            if let Some(ref key) = ai.api_key {
                ai.api_key = Some(
                    shellexpand::env(key)
                        .map_err(|e| format!("Env expansion error: {e}"))?
                        .to_string()
                );
            }
        }
        Ok(())
    }
}

// Load and expand
let mut config = ConfigLoader::from_file("ggen.toml")?;
config.expand_env_vars()?;
```

**Pros:**
- ✅ Familiar shell-style syntax
- ✅ Supports defaults (`${VAR:-default}`)

**Cons:**
- ⚠️ Requires explicit expansion call
- ⚠️ Additional dependency

---

### Pattern 2: Layered Override (config Crate)

```rust
use config::{Config, File, Environment};

let config = Config::builder()
    .add_source(File::with_name("ggen.toml"))
    .add_source(Environment::with_prefix("GGEN").separator("__"))
    .build()?;

let final_config: GgenConfig = config.try_deserialize()?;
```

```bash
# Override TOML values with env
export GGEN_AI__MODEL=gpt-3.5-turbo
export GGEN_AI__TEMPERATURE=0.9
```

**Pros:**
- ✅ Automatic merging
- ✅ No manual expansion needed
- ✅ Clear precedence

**Cons:**
- ⚠️ Requires `config` crate
- ⚠️ Environment var naming must match TOML structure

---

### Pattern 3: CLI + ENV Precedence (Recommended)

```rust
#[derive(Parser)]
struct Cli {
    /// AI model (CLI > ENV > ggen.toml)
    #[arg(long, env = "GGEN_AI_MODEL")]
    ai_model: Option<String>,
}

fn load_merged_config(cli: &Cli) -> Result<GgenConfig, ConfigError> {
    let mut config = ConfigLoader::from_file("ggen.toml")?;

    // CLI/ENV override takes precedence
    if let Some(ref model) = cli.ai_model {
        config.ai.as_mut().unwrap().model = model.clone();
    }

    Ok(config)
}
```

**Precedence:** CLI arg > ENV var > ggen.toml > default

**Pros:**
- ✅ Clear, explicit precedence
- ✅ No magic merging
- ✅ Easy to debug

**Cons:**
- ⚠️ Manual merge logic for each field

---

## Performance Optimization

### Issue: Config Loaded Per-Verb

Current ggen pattern loads config inside each verb:

```rust
#[verb]
fn generate(...) -> Result<...> {
    let config = ConfigLoader::from_file("ggen.toml")?;  // ← Loaded every time
    // ...
}
```

**Cost:** ~1-2ms TOML parsing per invocation

### Solution 1: Lazy Static Config

```rust
use once_cell::sync::Lazy;
use std::sync::RwLock;

static GLOBAL_CONFIG: Lazy<RwLock<Option<GgenConfig>>> = Lazy::new(|| {
    RwLock::new(None)
});

pub fn get_config() -> Result<GgenConfig, ConfigError> {
    // Read existing config
    if let Some(config) = GLOBAL_CONFIG.read().unwrap().as_ref() {
        return Ok(config.clone());
    }

    // Load and cache
    let config = ConfigLoader::from_file("ggen.toml")?;
    *GLOBAL_CONFIG.write().unwrap() = Some(config.clone());

    Ok(config)
}

// Use in verbs
#[verb]
fn generate(...) -> Result<...> {
    let config = get_config()?;  // ← Cached after first load
    // ...
}
```

**Pros:**
- ✅ Config loaded once
- ✅ Shared across all verbs
- ✅ Thread-safe

**Cons:**
- ⚠️ Global state (testing harder)
- ⚠️ Cannot reload config

---

### Solution 2: Context Passing

```rust
pub struct AppContext {
    pub config: GgenConfig,
}

// Initialize once at startup
fn main() {
    let config = ConfigLoader::from_file("ggen.toml")?;
    let ctx = AppContext { config };

    // Pass context to verbs (requires clap-noun-verb enhancement)
    run_with_context(ctx)?;
}
```

**Pros:**
- ✅ Explicit, testable
- ✅ No global state

**Cons:**
- ⚠️ Requires clap-noun-verb to support context passing
- ⚠️ Not currently supported

---

### Solution 3: Conditional Loading

```rust
#[verb]
fn generate(
    template: String,
    #[arg(long)] config_path: Option<String>,
) -> Result<...> {
    // Load config only if not using defaults
    let config = if let Some(path) = config_path {
        ConfigLoader::from_file(&path)?
    } else if Path::new("ggen.toml").exists() {
        ConfigLoader::from_file("ggen.toml")?
    } else {
        GgenConfig::default()?  // Use defaults if no config
    };

    // ...
}
```

**Pros:**
- ✅ Avoid loading if not needed
- ✅ Fallback to defaults

**Cons:**
- ⚠️ Still loads per invocation if config exists

---

## Recommended Integration Pattern for ggen

### Architecture

```rust
// crates/ggen-config/src/shared_types.rs

use clap::Args;
use serde::{Deserialize, Serialize};

/// Shared AI configuration (used by both CLI and TOML)
#[derive(Debug, Clone, Args, Deserialize, Serialize)]
pub struct AiConfig {
    #[arg(long, env = "GGEN_AI_PROVIDER")]
    #[serde(default = "default_provider")]
    pub provider: String,

    #[arg(long, env = "GGEN_AI_MODEL")]
    #[serde(default = "default_model")]
    pub model: String,

    #[arg(long, env = "GGEN_AI_TEMPERATURE", value_parser = validate_temperature)]
    #[serde(default = "default_temperature")]
    pub temperature: Temperature,
}

// ... defaults and validators
```

```rust
// crates/ggen-cli/src/lib.rs

use clap::Parser;

#[derive(Parser)]
struct Cli {
    /// Config file path
    #[arg(short, long, default_value = "ggen.toml")]
    config: String,

    /// AI configuration overrides
    #[command(flatten)]
    ai_overrides: AiConfig,

    #[command(subcommand)]
    command: Commands,
}

pub async fn cli_match() -> Result<()> {
    let cli = Cli::parse();

    // Load and merge config
    let config = load_merged_config(&cli)?;

    // Execute command with config
    execute_command(&cli.command, &config).await
}

fn load_merged_config(cli: &Cli) -> Result<GgenConfig> {
    let mut config = ConfigLoader::from_file(&cli.config)?;

    // Apply CLI overrides
    merge_cli_into_config(&mut config, &cli.ai_overrides)?;

    // Validate merged config
    config.validate()?;

    Ok(config)
}
```

### Benefits

1. **Type Safety**: Shared types ensure CLI and TOML stay in sync
2. **Validation**: Happens at both CLI parse and config load
3. **Precedence**: CLI > ENV > TOML > defaults
4. **Performance**: Config loaded once at startup
5. **DX**: Users can override any config value via CLI

---

## Conclusion

**Recommended Integration Stack for ggen:**

1. **Shared Types**: Use both `clap::Args` + `serde` derives
2. **Custom Parsers**: Validate constraints (temperature, paths)
3. **Environment Support**: `#[arg(env)]` for all configurable values
4. **Lazy Loading**: Cache config after first load
5. **Clear Precedence**: CLI > ENV > ggen.toml > defaults

**Implementation Steps:**

1. Create `crates/ggen-config/src/shared_types.rs`
2. Add `clap::Args` derives to config structs
3. Implement custom value parsers
4. Add validation trait to config types
5. Refactor CLI to use shared types
6. Add global config cache (lazy_static)

**Expected Benefits:**

- ✅ 50% reduction in boilerplate
- ✅ Type-safe CLI ↔ TOML integration
- ✅ Better error messages
- ✅ Improved performance (cached config)
- ✅ User-friendly CLI overrides

---

## References

- clap docs: https://docs.rs/clap
- config crate: https://docs.rs/config
- serde docs: https://serde.rs
- ggen-config source: `/Users/sac/ggen/crates/ggen-config`
