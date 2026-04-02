# ggen-config Implementation Checklist for CODER

## 📦 Crate Structure to Create

```
crates/ggen-config/
├── Cargo.toml
├── src/
│   ├── lib.rs          # Main entry point, re-exports
│   ├── config.rs       # Config struct definitions
│   ├── parser.rs       # TOML parsing logic
│   ├── validator.rs    # Schema validation
│   ├── resolver.rs     # Workspace dependency resolution
│   └── workspace.rs    # Workspace management
└── tests/
    ├── parser_tests.rs     # TOML parsing tests
    ├── validator_tests.rs  # Validation tests
    ├── resolver_tests.rs   # Resolution tests
    └── workspace_tests.rs  # Workspace tests
```

## 📝 Key Structures to Implement

### 1. Config Structure (src/config.rs)

```rust
use serde::{Deserialize, Serialize};

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct GgenConfig {
    pub project: ProjectConfig,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub workspace: Option<WorkspaceConfig>,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub lifecycle: Option<LifecycleConfig>,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub marketplace: Option<MarketplaceConfig>,
    pub templates: TemplatesConfig,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub ai: Option<AiConfig>,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub graph: Option<GraphConfig>,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub security: Option<SecurityConfig>,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub logging: Option<LoggingConfig>,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub performance: Option<PerformanceConfig>,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct ProjectConfig {
    pub name: String,
    pub version: String,
    pub description: String,
    pub author: String,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub license: Option<String>,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub repository: Option<String>,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct WorkspaceConfig {
    pub members: Vec<String>,
    #[serde(default)]
    pub exclude: Vec<String>,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub dependencies: Option<HashMap<String, String>>,
}

// ... implement other config structs (see test fixtures for structure)
```

### 2. Parser (src/parser.rs)

```rust
use anyhow::{Context, Result};
use std::path::Path;

pub fn parse_config<P: AsRef<Path>>(path: P) -> Result<GgenConfig> {
    let content = std::fs::read_to_string(path.as_ref())
        .context("Failed to read config file")?;

    toml::from_str(&content)
        .context("Failed to parse TOML")
}

pub fn parse_config_str(content: &str) -> Result<GgenConfig> {
    toml::from_str(content)
        .context("Failed to parse TOML")
}
```

### 3. Validator (src/validator.rs)

```rust
use anyhow::{bail, Result};

pub fn validate_config(config: &GgenConfig) -> Result<()> {
    // Validate project name is not empty
    if config.project.name.is_empty() {
        bail!("Project name cannot be empty");
    }

    // Validate semver
    validate_semver(&config.project.version)?;

    // Validate performance limits
    if let Some(perf) = &config.performance {
        if perf.memory_limit_mb == 0 {
            bail!("Memory limit must be greater than 0");
        }
        if perf.cpu_limit_percent > 100 {
            bail!("CPU limit cannot exceed 100%");
        }
    }

    Ok(())
}

fn validate_semver(version: &str) -> Result<()> {
    // Use semver crate or regex validation
    // Pattern: major.minor.patch
    if !version.contains('.') {
        bail!("Version must be in semver format (e.g., 1.0.0)");
    }
    Ok(())
}
```

### 4. Resolver (src/resolver.rs)

```rust
pub fn resolve_with_workspace(
    member: &GgenConfig,
    workspace: &WorkspaceConfig,
) -> Result<ResolvedConfig> {
    let mut resolved = member.clone();

    // Inherit workspace dependencies
    if let Some(workspace_deps) = &workspace.dependencies {
        // Merge workspace deps with member deps
        // Member deps take precedence
    }

    Ok(ResolvedConfig {
        config: resolved,
        workspace_inherited: true,
    })
}

pub fn version_matches(constraint: &str, version: &str) -> Result<bool> {
    // Implement version constraint matching
    // Support: ^, ~, >=, <, exact
    Ok(true) // Placeholder
}
```

## 📋 Dependencies for Cargo.toml

```toml
[package]
name = "ggen-config"
version = "3.2.0"
edition = "2021"

[dependencies]
serde = { workspace = true }
toml = { workspace = true }
anyhow = { workspace = true }

[dev-dependencies]
tempfile = { workspace = true }
```

## ✅ Implementation Steps

1. [ ] Create `crates/ggen-config/` directory
2. [ ] Add to workspace members in root `Cargo.toml`
3. [ ] Implement core config structures in `src/config.rs`
4. [ ] Implement parser in `src/parser.rs`
5. [ ] Implement validator in `src/validator.rs`
6. [ ] Implement resolver in `src/resolver.rs`
7. [ ] Implement workspace manager in `src/workspace.rs`
8. [ ] Create `src/lib.rs` with public API
9. [ ] Copy unit test templates from `tests/integration/config/UNIT_TESTS_TEMPLATE.md`
10. [ ] Run `cargo build -p ggen-config` to verify compilation
11. [ ] Notify TESTER to uncomment integration tests

## 🧪 Testing After Implementation

```bash
# Build the new crate
cargo build -p ggen-config

# Run unit tests
cargo test -p ggen-config

# Run integration tests (TESTER will uncomment assertions)
cargo test --test config_integration_test
cargo test --test config_performance_tests

# Verify all tests pass
cargo test config
```

## 🤝 Coordination

Once implementation is complete:

1. Run hooks:
```bash
npx claude-flow@alpha hooks post-task --task-id "implement-ggen-config"
npx claude-flow@alpha memory store hive/coder/ggen-config "Implementation complete"
```

2. Notify TESTER:
```bash
npx claude-flow@alpha hooks notify --message "ggen-config crate implemented, ready for test activation"
```

3. TESTER will:
   - Uncomment test assertions in integration tests
   - Create unit tests in `crates/ggen-config/tests/`
   - Run full test suite
   - Report 100% pass rate

## 📚 Reference Files

All example `ggen.toml` structures are in:
- `tests/fixtures/config/simple.ggen.toml` - Minimal example
- `tests/fixtures/config/advanced.ggen.toml` - Complete example with all features
- `tests/fixtures/config/workspace.ggen.toml` - Workspace example

## 🎯 Success Criteria

- [ ] Crate compiles without errors
- [ ] All config sections from fixtures can be parsed
- [ ] Validation catches required fields and constraints
- [ ] Workspace resolution works correctly
- [ ] Unit tests pass (15+ tests)
- [ ] Integration tests pass after uncommenting (18+ tests)
- [ ] Performance targets met (<50ms complex load)
