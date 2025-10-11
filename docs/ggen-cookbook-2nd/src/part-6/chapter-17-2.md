<!-- START doctoc generated TOC please keep comment here to allow auto update -->
<!-- DON'T EDIT THIS SECTION, INSTEAD RE-RUN doctoc TO UPDATE -->
**Table of Contents**

- [Chapter 17.2 - Placeholder](#chapter-172---placeholder)

<!-- END doctoc generated TOC please keep comment here to allow auto update -->

# Chapter 17.2: Plugin Architecture

## Context

As GGen evolves, users need ways to extend its functionality without modifying the core codebase. Traditional extension mechanisms are either too rigid (hardcoded integrations) or too loose (unreliable third-party packages).

## Problem

**How can we allow users to extend GGen's functionality while maintaining reliability, security, and consistency?**

## Forces

- **Extensibility**: Users need custom processors, validators, and generators
- **Security**: Third-party code must not compromise GGen's deterministic guarantees
- **Reliability**: Extensions should not break core functionality or each other
- **Performance**: Extensions should not significantly impact generation speed
- **Maintainability**: Extension API must be stable and well-documented
- **Discovery**: Users need to find and install extensions easily

## Solution

**Implement a plugin architecture with sandboxed execution, declarative configuration, and strong isolation boundaries.**

Create a plugin system that:

1. **Sandboxes extensions** in isolated processes or contexts
2. **Uses declarative configuration** via TOML/JSON schemas
3. **Enforces strict interfaces** with compile-time type checking
4. **Provides lifecycle management** for plugin loading/unloading
5. **Implements security boundaries** to prevent malicious extensions

## Diagram

```
┌─────────────────────────────────────────────────────────────┐
│                    GGen Core Engine                         │
│  ┌─────────────────────────────────────────────────────┐   │
│  │              Plugin Registry                        │   │
│  │  ┌─────────────────────────────────────────────┐   │   │
│  │  │           Plugin Sandbox                    │   │   │
│  │  │  ┌─────────────────────────────────────┐   │   │   │
│  │  │  │        Extension Code              │   │   │   │
│  │  │  │  • Processors                       │   │   │   │
│  │  │  │  • Validators                       │   │   │   │
│  │  │  │  • Generators                       │   │   │   │
│  │  │  └─────────────────────────────────────┘   │   │   │
│  │  └─────────────────────────────────────────────┘   │   │
│  └─────────────────────────────────────────────────────┘   │
└─────────────────────────────────────────────────────────────┘
```

## Implementation

### Plugin Manifest Format

```toml
# plugin.toml
[plugin]
name = "custom-validator"
version = "1.0.0"
description = "Custom validation for enterprise schemas"
author = "Your Name"

[permissions]
filesystem = ["read"]  # read-only access
network = false        # no network access
memory = "100MB"       # memory limit

[hooks]
pre_generation = "validate_schema"
post_generation = "format_output"

[dependencies]
serde = "1.0"
regex = "1.0"
```

### Plugin Interface Trait

```rust
use async_trait::async_trait;
use anyhow::Result;

#[async_trait]
pub trait GGenPlugin: Send + Sync {
    /// Plugin metadata
    fn name(&self) -> &str;
    fn version(&self) -> &str;
    fn description(&self) -> &str;

    /// Lifecycle hooks
    async fn on_load(&mut self) -> Result<()>;
    async fn on_unload(&mut self) -> Result<()>;

    /// Generation hooks
    async fn pre_generation(
        &self,
        template: &Template,
        context: &mut Context
    ) -> Result<()>;

    async fn post_generation(
        &self,
        template: &Template,
        output: &mut GeneratedFile
    ) -> Result<()>;
}
```

### Sandbox Implementation

```rust
pub struct PluginSandbox {
    plugin: Box<dyn GGenPlugin>,
    memory_limit: usize,
    filesystem_access: Vec<PathBuf>,
}

impl PluginSandbox {
    pub fn new(
        plugin: Box<dyn GGenPlugin>,
        config: &PluginConfig
    ) -> Result<Self> {
        Ok(Self {
            plugin,
            memory_limit: config.memory_limit,
            filesystem_access: config.allowed_paths.clone(),
        })
    }

    pub async fn execute_safely<F, R>(
        &self,
        operation: F
    ) -> Result<R>
    where
        F: FnOnce() -> R + Send + 'static,
        R: Send + 'static,
    {
        // Check memory usage
        let current_memory = self.get_memory_usage();
        if current_memory > self.memory_limit {
            return Err(anyhow!("Memory limit exceeded"));
        }

        // Check filesystem access
        if let Err(e) = self.validate_filesystem_access() {
            return Err(anyhow!("Filesystem access denied: {}", e));
        }

        // Execute with timeout and resource limits
        tokio::time::timeout(
            Duration::from_secs(30),
            tokio::task::spawn_blocking(operation)
        ).await??
    }
}
```

### Plugin Registry

```rust
pub struct PluginRegistry {
    plugins: HashMap<String, PluginSandbox>,
    config: PluginConfig,
}

impl PluginRegistry {
    pub fn new(config: PluginConfig) -> Self {
        Self {
            plugins: HashMap::new(),
            config,
        }
    }

    pub async fn load_plugin(&mut self, path: &Path) -> Result<()> {
        // Load plugin manifest
        let manifest: PluginManifest = toml::from_str(
            &fs::read_to_string(path.join("plugin.toml"))?
        )?;

        // Validate permissions
        self.validate_permissions(&manifest)?;

        // Load and sandbox the plugin
        let plugin = self.create_plugin_instance(&manifest, path).await?;
        let sandbox = PluginSandbox::new(plugin, &self.config)?;

        self.plugins.insert(manifest.name.clone(), sandbox);
        Ok(())
    }

    pub async fn execute_hooks(&self, hook: &str, args: HookArgs) -> Result<()> {
        for (name, sandbox) in &self.plugins {
            if let Some(hook_impl) = self.get_hook_implementation(name, hook) {
                sandbox.execute_safely(|| async {
                    hook_impl.execute(args.clone()).await
                }).await?;
            }
        }
        Ok(())
    }
}
```

## Result

**A secure, extensible plugin architecture that allows third-party extensions while maintaining GGen's core guarantees.**

### Benefits Achieved

- **🔒 Security**: Sandboxed execution prevents malicious plugins
- **🚀 Performance**: Resource limits prevent runaway plugins
- **🔧 Extensibility**: Easy to add new processors and validators
- **📦 Maintainability**: Clean separation between core and extensions
- **🔍 Discoverability**: Plugin registry makes extensions easy to find

### Related Patterns

- **021_knowledge_hooks.md** - Plugin hooks integrate with knowledge hooks
- **017_graph_driven_paths.md** - Plugins can extend path resolution
- **024_git_as_runtime.md** - Plugins can hook into Git operations

### Example: Custom RDF Validator Plugin

```rust
use async_trait::async_trait;
use ggen_core::{Template, Context, GeneratedFile};
use anyhow::Result;

pub struct CustomRDFValidator;

#[async_trait]
impl GGenPlugin for CustomRDFValidator {
    fn name(&self) -> &str { "custom-rdf-validator" }
    fn version(&self) -> &str { "1.0.0" }
    fn description(&self) -> &str {
        "Validates RDF graphs against enterprise schemas"
    }

    async fn on_load(&mut self) -> Result<()> {
        println!("Loading custom RDF validator");
        Ok(())
    }

    async fn post_generation(
        &self,
        _template: &Template,
        output: &mut GeneratedFile
    ) -> Result<()> {
        if output.path.ends_with(".ttl") {
            self.validate_rdf(&output.content)?;
        }
        Ok(())
    }
}

impl CustomRDFValidator {
    fn validate_rdf(&self, content: &str) -> Result<()> {
        // Custom validation logic
        if !content.contains("@prefix") {
            return Err(anyhow!("RDF must include prefix declarations"));
        }
        Ok(())
    }
}
```

## Verification

1. **Install a plugin**:
   ```bash
   ggen plugin install ./my-custom-validator
   ```

2. **Generate with plugin**:
   ```bash
   ggen generate my-template.tmpl --plugin custom-rdf-validator
   ```

3. **Verify plugin executed**:
   ```bash
   # Check plugin logs
   ggen plugin logs custom-rdf-validator

   # Verify output includes plugin modifications
   cat output.ttl | grep -q "@prefix"
   ```

4. **Test security boundaries**:
   ```bash
   # Plugin should not be able to write outside sandbox
   ggen plugin test-security custom-rdf-validator
   ```

## Next Steps

- **Plugin Marketplace**: Create a registry for sharing plugins
- **Plugin Dependencies**: Allow plugins to depend on other plugins
- **Plugin Updates**: Automatic plugin updates with rollback capability
- **Plugin Analytics**: Track plugin usage and performance metrics
