# Ggen Marketplace: Rig+MCP Integration Package

**Date**: 2025-10-11
**Status**: DESIGN PHASE
**Priority**: CRITICAL - Build marketplace infrastructure BEFORE LLM integration

---

## Executive Summary

Instead of directly integrating Rig framework into ggen-ai, we're creating a **marketplace system** where the Rig+MCP integration is a reusable package that ANY Rust project can install.

**Key Decision**: Build the CLI marketplace infrastructure FIRST, THEN use it to integrate LLMs into ggen.

---

## Strategic Vision

### Current State
```
vendors/rig-integration/  ← Downloaded official example
ggen-ai/                  ← Has 16,341 lines of custom agent/MCP code
ggen-mcp/                 ← Custom MCP implementations
```

### Proposed State
```
marketplace/
  packages/
    rig-mcp/              ← Reusable package (based on vendor example)

ggen-ai/                  ← Uses marketplace package
  Cargo.toml              ← Dependencies: rig-mcp-integration = "0.1.0"
```

### Any External Project
```
my-rust-project/
  Cargo.toml              ← Can also use: rig-mcp-integration = "0.1.0"

# Or install via CLI:
$ ggen market install rig-mcp
$ ggen market search agent
$ ggen market list
```

---

## Phase 1: Marketplace CLI (DO THIS FIRST)

### Commands to Implement

```bash
# Search marketplace
ggen market search <query>
ggen market search "agent framework"

# List packages
ggen market list
ggen market list --category ai

# Install package
ggen market install <package-name>
ggen market install rig-mcp

# Package info
ggen market info <package-name>
ggen market info rig-mcp

# Publish (for contributors)
ggen market publish <package-dir>

# Update packages
ggen market update
ggen market update rig-mcp
```

### Marketplace Structure

**File**: `marketplace/registry/packages.toml`
```toml
# Ggen Marketplace Package Registry
version = "1.0.0"

[[package]]
name = "rig-mcp"
version = "0.1.0"
description = "Rig framework + MCP protocol integration"
category = "ai"
author = "ggen-team"
repository = "https://github.com/seanchatmangpt/ggen/tree/master/marketplace/packages/rig-mcp"
dependencies = [
    "rig-core = 0.15.1",
    "rmcp = 0.8"
]
features = [
    "Dynamic tool loading with embeddings",
    "Multi-transport MCP (stdio, SSE, HTTP)",
    "Vector store for intelligent tool selection",
    "Streaming chat interface"
]
tags = ["llm", "agent", "mcp", "rig", "ai"]

[[package]]
name = "sparql-generator"
version = "0.2.0"
description = "AI-powered SPARQL query generation"
category = "graph"
# ... more packages
```

---

## Phase 2: Package Creation (`rig-mcp-integration`)

### Package Structure

```
marketplace/packages/rig-mcp/
├── Cargo.toml
├── README.md
├── LICENSE (MIT)
├── config.toml.example
├── src/
│   ├── lib.rs              # Public API
│   ├── adaptor.rs          # McpToolAdaptor (from vendor)
│   ├── config.rs           # Config + McpConfig
│   ├── manager.rs          # McpManager
│   ├── client.rs           # RigClient wrapper
│   ├── agent.rs            # Agent builder helpers
│   └── chat.rs             # Optional CLI chatbot
└── examples/
    ├── basic.rs            # Simple usage
    ├── multi_provider.rs   # Multiple LLM providers
    └── custom_tools.rs     # Adding custom MCP tools
```

### Public API Design

**File**: `marketplace/packages/rig-mcp/src/lib.rs`
```rust
//! Rig + MCP Integration
//!
//! Production-ready integration of the Rig LLM framework with the Model Context Protocol (MCP).
//! Based on the official MCP Rust SDK example.
//!
//! # Quick Start
//!
//! ```no_run
//! use rig_mcp_integration::{RigMcpClient, Config};
//!
//! #[tokio::main]
//! async fn main() -> anyhow::Result<()> {
//!     let config = Config::from_file("config.toml")?;
//!     let client = RigMcpClient::new(config).await?;
//!
//!     let agent = client
//!         .agent("gpt-4")
//!         .preamble("You are a helpful assistant")
//!         .build();
//!
//!     let response = agent.prompt("Generate a Rust function").await?;
//!     println!("{}", response);
//!     Ok(())
//! }
//! ```

pub mod adaptor;
pub mod config;
pub mod manager;
pub mod client;
pub mod agent;
pub mod chat;

// Re-exports
pub use adaptor::{McpToolAdaptor, McpManager};
pub use config::{Config, McpConfig, McpServerConfig, McpServerTransportConfig};
pub use client::RigMcpClient;
pub use agent::AgentBuilder;

/// Version of this integration package
pub const VERSION: &str = env!("CARGO_PKG_VERSION");

/// Prelude for convenient imports
pub mod prelude {
    pub use crate::{
        Config, RigMcpClient, AgentBuilder,
        McpManager, McpToolAdaptor,
    };
    pub use rig::prelude::*;
}
```

### Config API

**File**: `marketplace/packages/rig-mcp/src/config.rs`
```rust
use serde::{Deserialize, Serialize};
use std::collections::HashMap;

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct Config {
    /// Default LLM model to use
    pub default_model: String,

    /// OpenAI API key (optional, uses env if not set)
    pub openai_api_key: Option<String>,

    /// Anthropic API key (optional, uses env if not set)
    pub anthropic_api_key: Option<String>,

    /// Deepseek API key (optional, uses env if not set)
    pub deepseek_api_key: Option<String>,

    /// Cohere API key (optional, uses env if not set)
    pub cohere_api_key: Option<String>,

    /// MCP server configuration
    pub mcp: McpConfig,

    /// Additional provider-specific settings
    #[serde(default)]
    pub provider_settings: HashMap<String, serde_json::Value>,
}

impl Config {
    /// Load configuration from TOML file
    pub fn from_file(path: &str) -> anyhow::Result<Self> {
        let content = std::fs::read_to_string(path)?;
        Ok(toml::from_str(&content)?)
    }

    /// Create default config (uses environment variables)
    pub fn from_env() -> Self {
        Self {
            default_model: "gpt-4".to_string(),
            openai_api_key: None,
            anthropic_api_key: None,
            deepseek_api_key: None,
            cohere_api_key: None,
            mcp: McpConfig { servers: vec![] },
            provider_settings: HashMap::new(),
        }
    }
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct McpConfig {
    #[serde(rename = "server")]
    pub servers: Vec<McpServerConfig>,
}

// (Include full McpServerConfig from vendor example)
```

### Client API

**File**: `marketplace/packages/rig-mcp/src/client.rs`
```rust
use crate::{Config, McpManager};
use rig::{
    client::{CompletionClient, ProviderClient},
    embeddings::EmbeddingsBuilder,
    providers::{openai, anthropic, cohere, deepseek},
    vector_store::in_memory_store::InMemoryVectorStore,
};

/// Main client for Rig + MCP integration
pub struct RigMcpClient {
    config: Config,
    mcp_manager: McpManager,
    // Cache providers
    openai_client: Option<openai::Client>,
    anthropic_client: Option<anthropic::Client>,
    cohere_client: Option<cohere::Client>,
    deepseek_client: Option<deepseek::Client>,
}

impl RigMcpClient {
    /// Create new client from config
    pub async fn new(config: Config) -> anyhow::Result<Self> {
        let mcp_manager = config.mcp.create_manager().await?;

        Ok(Self {
            config,
            mcp_manager,
            openai_client: None,
            anthropic_client: None,
            cohere_client: None,
            deepseek_client: None,
        })
    }

    /// Get or create OpenAI client
    pub fn openai(&mut self) -> &openai::Client {
        self.openai_client.get_or_insert_with(|| {
            if let Some(key) = &self.config.openai_api_key {
                openai::Client::new(key)
            } else {
                openai::Client::from_env()
            }
        })
    }

    /// Build an agent with MCP tools
    pub async fn agent(&mut self, model: &str) -> anyhow::Result<AgentBuilder> {
        let tool_set = self.mcp_manager.get_tool_set().await?;

        // Create embeddings for tool selection
        let cohere_client = self.cohere();
        let embedding_model = cohere_client.embedding_model(
            cohere::EMBED_MULTILINGUAL_V3,
            "search_document"
        );

        let embeddings = EmbeddingsBuilder::new(embedding_model.clone())
            .documents(tool_set.schemas()?)?
            .build()
            .await?;

        let store = InMemoryVectorStore::from_documents_with_id_f(
            embeddings,
            |f| f.name.clone()
        );
        let index = store.index(embedding_model);

        Ok(AgentBuilder {
            model: model.to_string(),
            tool_set,
            tool_index: index,
            client: self,
        })
    }

    // Similar methods for other providers...
}
```

---

## Phase 3: CLI Implementation

### File Structure

```
ggen/src/
├── cli/
│   ├── mod.rs
│   ├── market.rs           # NEW: Marketplace commands
│   ├── generate.rs
│   └── validate.rs
└── marketplace/
    ├── mod.rs              # NEW: Marketplace logic
    ├── registry.rs         # Package registry management
    ├── installer.rs        # Package installation
    └── publisher.rs        # Package publishing
```

### Command Implementation

**File**: `ggen/src/cli/market.rs`
```rust
use clap::{Args, Subcommand};

#[derive(Debug, Args)]
pub struct MarketArgs {
    #[command(subcommand)]
    pub command: MarketCommand,
}

#[derive(Debug, Subcommand)]
pub enum MarketCommand {
    /// Search for packages in the marketplace
    Search {
        /// Search query
        query: String,

        /// Filter by category
        #[arg(long)]
        category: Option<String>,
    },

    /// List all available packages
    List {
        /// Filter by category
        #[arg(long)]
        category: Option<String>,
    },

    /// Install a package
    Install {
        /// Package name
        package: String,

        /// Specific version (default: latest)
        #[arg(long)]
        version: Option<String>,
    },

    /// Show package information
    Info {
        /// Package name
        package: String,
    },

    /// Update installed packages
    Update {
        /// Specific package (default: all)
        package: Option<String>,
    },

    /// Publish a package (for contributors)
    Publish {
        /// Package directory
        path: String,
    },
}

pub async fn execute(args: MarketArgs) -> anyhow::Result<()> {
    match args.command {
        MarketCommand::Search { query, category } => {
            search_packages(&query, category.as_deref()).await
        }
        MarketCommand::List { category } => {
            list_packages(category.as_deref()).await
        }
        MarketCommand::Install { package, version } => {
            install_package(&package, version.as_deref()).await
        }
        MarketCommand::Info { package } => {
            show_package_info(&package).await
        }
        MarketCommand::Update { package } => {
            update_packages(package.as_deref()).await
        }
        MarketCommand::Publish { path } => {
            publish_package(&path).await
        }
    }
}

async fn search_packages(query: &str, category: Option<&str>) -> anyhow::Result<()> {
    let registry = crate::marketplace::Registry::load().await?;
    let results = registry.search(query, category)?;

    println!("Found {} package(s):", results.len());
    for pkg in results {
        println!("\n📦 {}", pkg.name);
        println!("   Version: {}", pkg.version);
        println!("   {}", pkg.description);
        println!("   Category: {}", pkg.category);
    }
    Ok(())
}

async fn install_package(name: &str, version: Option<&str>) -> anyhow::Result<()> {
    let installer = crate::marketplace::Installer::new().await?;

    println!("📥 Installing {}...", name);
    installer.install(name, version).await?;

    println!("✅ Successfully installed {}", name);
    println!("\nTo use in your project:");
    println!("  Add to Cargo.toml: {} = \"{}\"", name, version.unwrap_or("*"));
    Ok(())
}

// Implement other commands...
```

---

## Phase 4: Registry Management

**File**: `ggen/src/marketplace/registry.rs`
```rust
use serde::{Deserialize, Serialize};
use std::path::PathBuf;

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct Package {
    pub name: String,
    pub version: String,
    pub description: String,
    pub category: String,
    pub author: String,
    pub repository: String,
    pub dependencies: Vec<String>,
    pub features: Vec<String>,
    pub tags: Vec<String>,
}

#[derive(Debug, Serialize, Deserialize)]
pub struct Registry {
    pub version: String,
    pub packages: Vec<Package>,
}

impl Registry {
    /// Load registry from marketplace/registry/packages.toml
    pub async fn load() -> anyhow::Result<Self> {
        let path = Self::registry_path()?;
        let content = tokio::fs::read_to_string(path).await?;
        Ok(toml::from_str(&content)?)
    }

    /// Search packages by query
    pub fn search(&self, query: &str, category: Option<&str>) -> anyhow::Result<Vec<&Package>> {
        let query_lower = query.to_lowercase();
        let results: Vec<_> = self.packages.iter()
            .filter(|pkg| {
                let matches_query = pkg.name.to_lowercase().contains(&query_lower)
                    || pkg.description.to_lowercase().contains(&query_lower)
                    || pkg.tags.iter().any(|tag| tag.to_lowercase().contains(&query_lower));

                let matches_category = category.map_or(true, |cat| pkg.category == cat);

                matches_query && matches_category
            })
            .collect();
        Ok(results)
    }

    /// Get package by name
    pub fn get(&self, name: &str) -> Option<&Package> {
        self.packages.iter().find(|pkg| pkg.name == name)
    }

    fn registry_path() -> anyhow::Result<PathBuf> {
        let project_root = std::env::current_dir()?;
        Ok(project_root.join("marketplace/registry/packages.toml"))
    }
}
```

---

## Phase 5: Integration with ggen-ai

Once marketplace is ready, ggen-ai becomes a simple consumer:

**File**: `ggen-ai/Cargo.toml`
```toml
[dependencies]
# Use marketplace package instead of custom code
rig-mcp-integration = { path = "../marketplace/packages/rig-mcp" }

# Or from crates.io eventually:
# rig-mcp-integration = "0.1.0"

# DELETE these:
# genai = "0.4"  ❌
# Custom agent code ❌
```

**File**: `ggen-ai/src/lib.rs`
```rust
// Simply re-export the marketplace package
pub use rig_mcp_integration::prelude::*;

// Add ggen-specific helpers
pub mod ggen {
    use super::*;

    /// Create ggen-configured agent
    pub async fn template_agent() -> anyhow::Result<Agent> {
        let config = Config::from_file("ggen-config.toml")?;
        let mut client = RigMcpClient::new(config).await?;

        Ok(client
            .agent("gpt-4")?
            .preamble("You are a code template generation expert")
            .build())
    }

    pub async fn sparql_agent() -> anyhow::Result<Agent> {
        // Similar pattern
    }
}
```

---

## Implementation Timeline

### Week 1: Marketplace Foundation
- [ ] Day 1-2: CLI commands (`ggen market`)
- [ ] Day 3-4: Registry system
- [ ] Day 5: Installer logic

### Week 2: Package Creation
- [ ] Day 1-2: Extract vendor example into package
- [ ] Day 3: Public API design
- [ ] Day 4: Documentation
- [ ] Day 5: Examples

### Week 3: Integration
- [ ] Day 1-2: Test marketplace installation
- [ ] Day 3: Integrate into ggen-ai
- [ ] Day 4: Delete old code (16,341 lines)
- [ ] Day 5: Final testing

---

## Success Metrics

### Before
```
ggen-ai:  2,874 lines (with custom agents)
ggen-mcp: 12,506 lines (custom MCP)
Total:    15,380 lines of custom LLM/agent code
```

### After
```
marketplace/packages/rig-mcp/: ~400 lines (reusable)
ggen-ai:                       ~100 lines (just uses the package)
Total custom code:             ~500 lines (96.7% reduction)
```

### Additional Benefits
- ✅ Any Rust project can use `rig-mcp-integration`
- ✅ Marketplace enables community contributions
- ✅ ggen becomes a platform, not just a tool
- ✅ Clean separation of concerns
- ✅ Better testing in isolation

---

## Next Steps

1. **Initialize marketplace structure**:
   ```bash
   mkdir -p marketplace/{packages,registry}
   touch marketplace/registry/packages.toml
   ```

2. **Implement `ggen market` CLI commands**

3. **Extract vendor example into reusable package**

4. **Test installation flow**

5. **Integrate into ggen-ai** (after marketplace works)

---

## Questions to Resolve

1. **Package hosting**: Start with local paths, move to crates.io later?
2. **Versioning strategy**: SemVer with marketplace registry?
3. **Update mechanism**: Auto-check for updates on `ggen` startup?
4. **Authentication**: Needed for publishing packages?
5. **CI/CD**: Automated package testing and publishing?

---

**Decision**: Build the marketplace infrastructure FIRST, before integrating LLMs into ggen-ai.

**Rationale**: This approach creates a sustainable, scalable platform rather than a one-off integration.
