# P2P CLI Architecture Design - clap-noun-verb v3.0.0 Integration

**Project:** ggen Marketplace P2P Integration
**Version:** 2.3.0
**Date:** 2025-11-02
**Architect:** System Architecture Agent (Hive Mind swarm-1762117554288-9inb3gcsg)

---

## Executive Summary

This document defines the architecture for integrating P2P marketplace functionality into the ggen CLI using clap-noun-verb v3.0.0 pattern. The design follows existing architectural patterns while introducing a new `p2p` noun with verbs for decentralized package registry operations.

---

## Table of Contents

1. [Current Architecture Analysis](#current-architecture-analysis)
2. [P2P Command Structure](#p2p-command-structure)
3. [Implementation Layers](#implementation-layers)
4. [Configuration Management](#configuration-management)
5. [Error Handling Strategy](#error-handling-strategy)
6. [User Experience Flow](#user-experience-flow)
7. [Integration Patterns](#integration-patterns)
8. [Testing Strategy](#testing-strategy)
9. [Performance Considerations](#performance-considerations)
10. [Security Architecture](#security-architecture)

---

## 1. Current Architecture Analysis

### 1.1 Existing CLI Structure

**clap-noun-verb Version:** v3.0.0 (workspace dependency)

```
cli/
├── src/
│   ├── cmds/                    # Command layer (clap-noun-verb entry points)
│   │   ├── mod.rs              # Router registration
│   │   ├── marketplace.rs      # Existing marketplace commands
│   │   └── ...
│   ├── domain/                  # Business logic layer
│   │   ├── marketplace/
│   │   │   ├── search.rs       # Domain logic
│   │   │   ├── install.rs
│   │   │   └── ...
│   │   └── ...
│   ├── runtime.rs              # Async/sync bridge
│   └── lib.rs
```

### 1.2 Architecture Layers

```
┌─────────────────────────────────────────────────────────────┐
│  CLI Layer (clap-noun-verb)                                 │
│  - cmds/marketplace.rs: MarketplaceArgs + MarketplaceCmd    │
│  - Argument parsing, validation                             │
└───────────────────┬─────────────────────────────────────────┘
                    │
                    ▼
┌─────────────────────────────────────────────────────────────┐
│  Runtime Layer (async/sync bridge)                          │
│  - runtime::execute() for async functions                   │
│  - Tokio runtime management                                 │
└───────────────────┬─────────────────────────────────────────┘
                    │
                    ▼
┌─────────────────────────────────────────────────────────────┐
│  Domain Layer (business logic)                              │
│  - domain/marketplace/search.rs: async search_packages()    │
│  - domain/marketplace/install.rs: async install_package()   │
│  - Pure business logic, testable                            │
└───────────────────┬─────────────────────────────────────────┘
                    │
                    ▼
┌─────────────────────────────────────────────────────────────┐
│  Backend Layer (ggen-marketplace crate)                     │
│  - ggen-marketplace/src/backend/p2p.rs: P2PRegistry         │
│  - libp2p integration                                       │
└─────────────────────────────────────────────────────────────┘
```

### 1.3 Existing Marketplace Commands

```rust
// Current structure in cli/src/cmds/marketplace.rs
#[derive(Debug, Subcommand)]
pub enum MarketplaceCmd {
    Search(marketplace::SearchArgs),
    Install(marketplace::InstallArgs),
    List(marketplace::ListArgs),
    Publish(marketplace::PublishArgs),
    Update(marketplace::UpdateArgs),
}
```

---

## 2. P2P Command Structure

### 2.1 Noun-Verb Hierarchy

**Primary Noun:** `p2p` (new)
**Parent Noun:** `marketplace` (existing)

**Recommendation:** Add `p2p` as a subcommand under `marketplace` for logical grouping.

```
ggen marketplace p2p [verb] [args]
```

**Alternative (if top-level):**
```
ggen p2p [verb] [args]
```

### 2.2 Command Matrix

| Command | Purpose | Priority | Complexity |
|---------|---------|----------|------------|
| `ggen marketplace p2p start` | Initialize P2P node and connect to network | HIGH | Medium |
| `ggen marketplace p2p status` | Show P2P node status (peers, DHT, gossipsub) | HIGH | Low |
| `ggen marketplace p2p publish <package>` | Publish package to P2P network | HIGH | High |
| `ggen marketplace p2p search <query>` | Search P2P network for packages | HIGH | Medium |
| `ggen marketplace p2p connect <peer>` | Connect to specific peer | MEDIUM | Low |
| `ggen marketplace p2p disconnect` | Disconnect from P2P network | MEDIUM | Low |
| `ggen marketplace p2p peers` | List connected peers with reputation | MEDIUM | Low |
| `ggen marketplace p2p bootstrap` | Bootstrap DHT from configured nodes | MEDIUM | Low |
| `ggen marketplace p2p config` | Show/edit P2P configuration | LOW | Low |

### 2.3 Verb Definitions

#### 2.3.1 `start` - Initialize P2P Network

```bash
ggen marketplace p2p start [OPTIONS]

OPTIONS:
  --bootstrap <MULTIADDR>...  Bootstrap nodes (default: config)
  --listen <MULTIADDR>...     Listen addresses (default: /ip4/0.0.0.0/tcp/0)
  --dht-server               Enable DHT server mode
  --topic <STRING>           Gossipsub topic (default: /ggen/packages/v1)
  --config <PATH>            Config file path
  --daemon                   Run as background daemon
```

**Example:**
```bash
ggen marketplace p2p start --bootstrap /ip4/104.131.131.82/tcp/4001/p2p/QmaCpDMGvV2BGHeYERUEnRQAwe3N8SzbUtfsmvsqQLuvuJ
```

#### 2.3.2 `status` - Node Status

```bash
ggen marketplace p2p status [OPTIONS]

OPTIONS:
  --json                     Output as JSON
  --detailed                 Show detailed peer information
```

**Output:**
```
P2P Node Status
===============
Peer ID: 12D3KooWGmB8...
Listening: /ip4/127.0.0.1/tcp/54321
DHT Mode: Server
Connected Peers: 12
Gossipsub Topics: /ggen/packages/v1
Packages Published: 3
Packages Discovered: 47
```

#### 2.3.3 `publish` - Publish Package

```bash
ggen marketplace p2p publish <PACKAGE> [OPTIONS]

ARGS:
  <PACKAGE>                  Package path or name@version

OPTIONS:
  --announce                 Announce via gossipsub
  --dht-store               Store in DHT (default: true)
  --wait                     Wait for DHT replication
```

**Example:**
```bash
ggen marketplace p2p publish ./my-package
ggen marketplace p2p publish my-package@1.0.0 --announce
```

#### 2.3.4 `search` - Search P2P Network

```bash
ggen marketplace p2p search <QUERY> [OPTIONS]

ARGS:
  <QUERY>                    Search query

OPTIONS:
  -c, --category <STRING>    Filter by category
  -l, --limit <NUM>          Limit results (default: 10)
  --timeout <SECS>           Search timeout (default: 30)
  --json                     Output as JSON
```

**Example:**
```bash
ggen marketplace p2p search "rust web framework"
ggen marketplace p2p search --category ai --limit 20
```

#### 2.3.5 `connect` - Connect to Peer

```bash
ggen marketplace p2p connect <PEER> [OPTIONS]

ARGS:
  <PEER>                     Peer multiaddress or ID

OPTIONS:
  --persistent               Maintain persistent connection
```

**Example:**
```bash
ggen marketplace p2p connect /ip4/192.168.1.100/tcp/4001/p2p/12D3KooW...
```

#### 2.3.6 `disconnect` - Disconnect

```bash
ggen marketplace p2p disconnect [OPTIONS]

OPTIONS:
  --peer <PEER_ID>          Disconnect specific peer (default: all)
  --shutdown                Shutdown P2P node
```

#### 2.3.7 `peers` - List Peers

```bash
ggen marketplace p2p peers [OPTIONS]

OPTIONS:
  --json                     Output as JSON
  --sort <FIELD>            Sort by: reputation, latency, packages
```

**Output:**
```
Connected Peers (12)
====================
Peer ID              | Reputation | Latency | Packages
---------------------|------------|---------|----------
12D3KooWGmB8...     | 98.5%      | 45ms    | 23
12D3KooWAbc1...     | 95.2%      | 120ms   | 15
```

#### 2.3.8 `bootstrap` - Bootstrap DHT

```bash
ggen marketplace p2p bootstrap [OPTIONS]

OPTIONS:
  --nodes <MULTIADDR>...    Additional bootstrap nodes
  --wait                     Wait for bootstrap completion
```

#### 2.3.9 `config` - Configuration Management

```bash
ggen marketplace p2p config [SUBCOMMAND]

SUBCOMMANDS:
  show                       Show current configuration
  edit                       Edit configuration file
  reset                      Reset to defaults
  validate                   Validate configuration
```

---

## 3. Implementation Layers

### 3.1 CLI Layer (cmds/marketplace.rs)

**File:** `cli/src/cmds/marketplace.rs`

```rust
//! Marketplace commands - clap-noun-verb auto-discovery
use clap::{Args, Subcommand};
use ggen_utils::error::Result;

use crate::domain::marketplace;

/// Marketplace command arguments
#[derive(Debug, Args)]
pub struct MarketplaceArgs {
    #[command(subcommand)]
    pub command: MarketplaceCmd,
}

#[derive(Debug, Subcommand)]
pub enum MarketplaceCmd {
    /// Search for packages
    Search(marketplace::SearchArgs),

    /// Install a package
    Install(marketplace::InstallArgs),

    /// List installed packages
    List(marketplace::ListArgs),

    /// Publish a package
    Publish(marketplace::PublishArgs),

    /// Update packages
    Update(marketplace::UpdateArgs),

    /// P2P network operations (NEW)
    #[command(subcommand)]
    P2p(P2pCmd),
}

/// P2P subcommands (NEW)
#[derive(Debug, Subcommand)]
pub enum P2pCmd {
    /// Start P2P node
    Start(marketplace::p2p::StartArgs),

    /// Show P2P status
    Status(marketplace::p2p::StatusArgs),

    /// Publish to P2P network
    Publish(marketplace::p2p::PublishArgs),

    /// Search P2P network
    Search(marketplace::p2p::SearchArgs),

    /// Connect to peer
    Connect(marketplace::p2p::ConnectArgs),

    /// Disconnect from network
    Disconnect(marketplace::p2p::DisconnectArgs),

    /// List peers
    Peers(marketplace::p2p::PeersArgs),

    /// Bootstrap DHT
    Bootstrap(marketplace::p2p::BootstrapArgs),

    /// Configuration management
    #[command(subcommand)]
    Config(marketplace::p2p::ConfigCmd),
}

impl MarketplaceArgs {
    pub fn execute(&self) -> Result<()> {
        match &self.command {
            MarketplaceCmd::Search(args) => marketplace::search::run(args),
            MarketplaceCmd::Install(args) => marketplace::install::run(args),
            MarketplaceCmd::List(args) => marketplace::list::run(args),
            MarketplaceCmd::Publish(args) => marketplace::publish::run(args),
            MarketplaceCmd::Update(args) => marketplace::update::run(args),
            MarketplaceCmd::P2p(cmd) => Self::execute_p2p(cmd), // NEW
        }
    }

    fn execute_p2p(cmd: &P2pCmd) -> Result<()> {
        match cmd {
            P2pCmd::Start(args) => marketplace::p2p::start::run(args),
            P2pCmd::Status(args) => marketplace::p2p::status::run(args),
            P2pCmd::Publish(args) => marketplace::p2p::publish::run(args),
            P2pCmd::Search(args) => marketplace::p2p::search::run(args),
            P2pCmd::Connect(args) => marketplace::p2p::connect::run(args),
            P2pCmd::Disconnect(args) => marketplace::p2p::disconnect::run(args),
            P2pCmd::Peers(args) => marketplace::p2p::peers::run(args),
            P2pCmd::Bootstrap(args) => marketplace::p2p::bootstrap::run(args),
            P2pCmd::Config(subcmd) => marketplace::p2p::config::run(subcmd),
        }
    }
}
```

### 3.2 Domain Layer Structure

**New Directory:** `cli/src/domain/marketplace/p2p/`

```
cli/src/domain/marketplace/p2p/
├── mod.rs              # Module exports and common types
├── start.rs            # Start P2P node logic
├── status.rs           # Status display logic
├── publish.rs          # P2P publishing logic
├── search.rs           # P2P search logic
├── connect.rs          # Peer connection logic
├── disconnect.rs       # Disconnect logic
├── peers.rs            # Peer listing logic
├── bootstrap.rs        # DHT bootstrap logic
├── config.rs           # Configuration management
└── node_manager.rs     # Shared P2P node state management
```

**File:** `cli/src/domain/marketplace/p2p/mod.rs`

```rust
//! P2P marketplace domain logic
//!
//! This module contains business logic for P2P network operations,
//! bridging CLI commands to the ggen-marketplace P2P backend.

pub mod start;
pub mod status;
pub mod publish;
pub mod search;
pub mod connect;
pub mod disconnect;
pub mod peers;
pub mod bootstrap;
pub mod config;
pub mod node_manager;

// Re-export argument types
pub use start::StartArgs;
pub use status::StatusArgs;
pub use publish::PublishArgs;
pub use search::SearchArgs;
pub use connect::ConnectArgs;
pub use disconnect::DisconnectArgs;
pub use peers::PeersArgs;
pub use bootstrap::BootstrapArgs;
pub use config::ConfigCmd;

// Re-export node manager for shared state
pub use node_manager::{P2PNodeManager, P2PNodeState};
```

### 3.3 Domain Implementation Example

**File:** `cli/src/domain/marketplace/p2p/start.rs`

```rust
//! P2P node startup logic

use clap::Args;
use ggen_utils::error::Result;
use libp2p::Multiaddr;
use crate::runtime;

#[derive(Debug, Args)]
pub struct StartArgs {
    /// Bootstrap nodes (multiaddr)
    #[arg(long, value_name = "MULTIADDR")]
    pub bootstrap: Vec<String>,

    /// Listen addresses
    #[arg(long, default_value = "/ip4/0.0.0.0/tcp/0")]
    pub listen: Vec<String>,

    /// Enable DHT server mode
    #[arg(long)]
    pub dht_server: bool,

    /// Gossipsub topic
    #[arg(long, default_value = "/ggen/packages/v1")]
    pub topic: String,

    /// Configuration file path
    #[arg(long)]
    pub config: Option<String>,

    /// Run as daemon
    #[arg(long)]
    pub daemon: bool,
}

/// Execute start command (sync wrapper)
pub fn run(args: &StartArgs) -> Result<()> {
    runtime::execute(start_async(args.clone()))
}

/// Async implementation
async fn start_async(args: StartArgs) -> Result<()> {
    use ggen_marketplace::backend::p2p::{P2PConfig, P2PRegistry};

    // Build P2P configuration
    let mut config = P2PConfig::default();
    config.packages_topic = args.topic;
    config.dht_server_mode = args.dht_server;

    // Parse bootstrap nodes
    for addr_str in &args.bootstrap {
        let addr: Multiaddr = addr_str.parse()
            .map_err(|e| anyhow::anyhow!("Invalid multiaddr: {}", e))?;
        config.bootstrap_nodes.push(addr);
    }

    // Parse listen addresses
    config.listen_addresses.clear();
    for addr_str in &args.listen {
        let addr: Multiaddr = addr_str.parse()
            .map_err(|e| anyhow::anyhow!("Invalid multiaddr: {}", e))?;
        config.listen_addresses.push(addr);
    }

    // Create P2P registry
    let registry = P2PRegistry::new(config).await?;

    // Start listening
    registry.start_listening().await?;

    // Subscribe to topics
    registry.subscribe_to_packages().await?;

    // Bootstrap DHT
    registry.bootstrap().await?;

    // Store in node manager for persistent access
    super::node_manager::set_global_registry(registry).await?;

    println!("✅ P2P node started successfully");
    println!("📡 Peer ID: {}", registry.peer_id);
    println!("🎧 Listening on configured addresses");

    if args.daemon {
        // Run event loop in background
        tokio::spawn(async move {
            loop {
                registry.process_events().await;
                tokio::time::sleep(tokio::time::Duration::from_millis(100)).await;
            }
        });
        println!("🚀 Running in daemon mode");
    } else {
        // Block on event loop
        println!("🔄 Processing events (Ctrl+C to stop)...");
        loop {
            registry.process_events().await;
            tokio::time::sleep(tokio::time::Duration::from_millis(100)).await;
        }
    }

    Ok(())
}
```

### 3.4 Node Manager (Shared State)

**File:** `cli/src/domain/marketplace/p2p/node_manager.rs`

```rust
//! P2P node state management
//!
//! Provides global access to P2P registry instance across commands.

use ggen_marketplace::backend::p2p::P2PRegistry;
use once_cell::sync::OnceCell;
use tokio::sync::RwLock;
use std::sync::Arc;
use anyhow::{Result, anyhow};

/// Global P2P node state
static GLOBAL_REGISTRY: OnceCell<Arc<RwLock<Option<P2PRegistry>>>> = OnceCell::new();

/// P2P node lifecycle state
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum P2PNodeState {
    NotStarted,
    Starting,
    Running,
    Stopping,
    Stopped,
    Error,
}

/// Initialize global registry storage
fn init_registry() -> Arc<RwLock<Option<P2PRegistry>>> {
    GLOBAL_REGISTRY.get_or_init(|| {
        Arc::new(RwLock::new(None))
    }).clone()
}

/// Set the global P2P registry
pub async fn set_global_registry(registry: P2PRegistry) -> Result<()> {
    let registry_lock = init_registry();
    let mut guard = registry_lock.write().await;
    *guard = Some(registry);
    Ok(())
}

/// Get the global P2P registry
pub async fn get_global_registry() -> Result<P2PRegistry> {
    let registry_lock = init_registry();
    let guard = registry_lock.read().await;
    guard.as_ref()
        .ok_or_else(|| anyhow!("P2P node not started. Run 'ggen marketplace p2p start' first."))
        .cloned()
}

/// Check if P2P node is running
pub async fn is_running() -> bool {
    let registry_lock = init_registry();
    let guard = registry_lock.read().await;
    guard.is_some()
}

/// Stop and clear the global registry
pub async fn stop_global_registry() -> Result<()> {
    let registry_lock = init_registry();
    let mut guard = registry_lock.write().await;
    *guard = None;
    Ok(())
}

/// Manager for P2P node lifecycle
pub struct P2PNodeManager;

impl P2PNodeManager {
    pub async fn state() -> P2PNodeState {
        if is_running().await {
            P2PNodeState::Running
        } else {
            P2PNodeState::NotStarted
        }
    }

    pub async fn require_running() -> Result<P2PRegistry> {
        get_global_registry().await
    }
}
```

---

## 4. Configuration Management

### 4.1 Configuration Structure

**File:** `~/.ggen/p2p-config.toml`

```toml
[network]
# Bootstrap nodes for peer discovery
bootstrap_nodes = [
    "/dnsaddr/bootstrap.libp2p.io/p2p/QmNnooDu7bfjPFoTZYxMNLWUQJyrVwtbZg5gBMjTezGAJN",
    "/ip4/104.131.131.82/tcp/4001/p2p/QmaCpDMGvV2BGHeYERUEnRQAwe3N8SzbUtfsmvsqQLuvuJ"
]

# Listen addresses
listen_addresses = [
    "/ip4/0.0.0.0/tcp/0",
    "/ip6/::/tcp/0"
]

# DHT server mode
dht_server_mode = true

# Gossipsub topic for package announcements
packages_topic = "/ggen/packages/v1"

[performance]
# Connection limits
max_peers = 50
min_peers = 5

# DHT query timeout (seconds)
dht_query_timeout = 30

# Gossipsub heartbeat interval (seconds)
gossipsub_heartbeat = 10

[storage]
# Local package cache directory
cache_dir = "~/.ggen/p2p-cache"

# Maximum cache size (MB)
max_cache_size = 1024

[security]
# Enable TLS for connections
enable_tls = true

# Minimum peer reputation threshold
min_peer_reputation = 0.7

# Ban threshold for failed retrievals
ban_threshold = 10
```

### 4.2 Configuration Loading

**File:** `cli/src/domain/marketplace/p2p/config.rs`

```rust
//! P2P configuration management

use clap::Subcommand;
use ggen_utils::error::Result;
use serde::{Deserialize, Serialize};
use std::path::{Path, PathBuf};
use std::fs;

#[derive(Debug, Subcommand)]
pub enum ConfigCmd {
    /// Show current configuration
    Show,
    /// Edit configuration file
    Edit,
    /// Reset to defaults
    Reset,
    /// Validate configuration
    Validate,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct P2PConfigFile {
    pub network: NetworkConfig,
    pub performance: PerformanceConfig,
    pub storage: StorageConfig,
    pub security: SecurityConfig,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct NetworkConfig {
    pub bootstrap_nodes: Vec<String>,
    pub listen_addresses: Vec<String>,
    pub dht_server_mode: bool,
    pub packages_topic: String,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct PerformanceConfig {
    pub max_peers: usize,
    pub min_peers: usize,
    pub dht_query_timeout: u64,
    pub gossipsub_heartbeat: u64,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct StorageConfig {
    pub cache_dir: PathBuf,
    pub max_cache_size: usize,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct SecurityConfig {
    pub enable_tls: bool,
    pub min_peer_reputation: f64,
    pub ban_threshold: u64,
}

impl Default for P2PConfigFile {
    fn default() -> Self {
        Self {
            network: NetworkConfig {
                bootstrap_nodes: vec![
                    "/dnsaddr/bootstrap.libp2p.io/p2p/QmNnooDu7bfjPFoTZYxMNLWUQJyrVwtbZg5gBMjTezGAJN".to_string(),
                ],
                listen_addresses: vec![
                    "/ip4/0.0.0.0/tcp/0".to_string(),
                ],
                dht_server_mode: true,
                packages_topic: "/ggen/packages/v1".to_string(),
            },
            performance: PerformanceConfig {
                max_peers: 50,
                min_peers: 5,
                dht_query_timeout: 30,
                gossipsub_heartbeat: 10,
            },
            storage: StorageConfig {
                cache_dir: dirs::home_dir()
                    .unwrap_or_default()
                    .join(".ggen/p2p-cache"),
                max_cache_size: 1024,
            },
            security: SecurityConfig {
                enable_tls: true,
                min_peer_reputation: 0.7,
                ban_threshold: 10,
            },
        }
    }
}

impl P2PConfigFile {
    pub fn config_path() -> PathBuf {
        dirs::home_dir()
            .unwrap_or_default()
            .join(".ggen/p2p-config.toml")
    }

    pub fn load() -> Result<Self> {
        let path = Self::config_path();
        if !path.exists() {
            return Ok(Self::default());
        }

        let content = fs::read_to_string(&path)?;
        let config: P2PConfigFile = toml::from_str(&content)?;
        Ok(config)
    }

    pub fn save(&self) -> Result<()> {
        let path = Self::config_path();
        if let Some(parent) = path.parent() {
            fs::create_dir_all(parent)?;
        }

        let content = toml::to_string_pretty(self)?;
        fs::write(path, content)?;
        Ok(())
    }

    pub fn validate(&self) -> Result<()> {
        // Validate bootstrap nodes
        for node in &self.network.bootstrap_nodes {
            let _: libp2p::Multiaddr = node.parse()
                .map_err(|e| anyhow::anyhow!("Invalid bootstrap node: {}", e))?;
        }

        // Validate listen addresses
        for addr in &self.network.listen_addresses {
            let _: libp2p::Multiaddr = addr.parse()
                .map_err(|e| anyhow::anyhow!("Invalid listen address: {}", e))?;
        }

        // Validate performance settings
        if self.performance.max_peers < self.performance.min_peers {
            return Err(anyhow::anyhow!("max_peers must be >= min_peers"));
        }

        // Validate security settings
        if self.security.min_peer_reputation < 0.0 || self.security.min_peer_reputation > 1.0 {
            return Err(anyhow::anyhow!("min_peer_reputation must be between 0.0 and 1.0"));
        }

        Ok(())
    }
}

pub fn run(cmd: &ConfigCmd) -> Result<()> {
    match cmd {
        ConfigCmd::Show => show_config(),
        ConfigCmd::Edit => edit_config(),
        ConfigCmd::Reset => reset_config(),
        ConfigCmd::Validate => validate_config(),
    }
}

fn show_config() -> Result<()> {
    let config = P2PConfigFile::load()?;
    let toml_str = toml::to_string_pretty(&config)?;
    println!("{}", toml_str);
    Ok(())
}

fn edit_config() -> Result<()> {
    let path = P2PConfigFile::config_path();
    let editor = std::env::var("EDITOR").unwrap_or_else(|_| "vim".to_string());

    // Create default config if not exists
    if !path.exists() {
        P2PConfigFile::default().save()?;
    }

    // Open in editor
    std::process::Command::new(editor)
        .arg(&path)
        .status()?;

    println!("✅ Configuration saved to {}", path.display());
    Ok(())
}

fn reset_config() -> Result<()> {
    let config = P2PConfigFile::default();
    config.save()?;
    println!("✅ Configuration reset to defaults");
    Ok(())
}

fn validate_config() -> Result<()> {
    let config = P2PConfigFile::load()?;
    config.validate()?;
    println!("✅ Configuration is valid");
    Ok(())
}
```

---

## 5. Error Handling Strategy

### 5.1 Error Categories

```rust
use thiserror::Error;

#[derive(Error, Debug)]
pub enum P2PError {
    #[error("P2P node not started. Run 'ggen marketplace p2p start' first.")]
    NodeNotStarted,

    #[error("Failed to parse multiaddress: {0}")]
    InvalidMultiaddr(String),

    #[error("Failed to connect to peer {peer_id}: {source}")]
    ConnectionFailed {
        peer_id: String,
        source: anyhow::Error,
    },

    #[error("DHT query timeout after {seconds}s")]
    DhtTimeout { seconds: u64 },

    #[error("Package not found in P2P network: {package_id}")]
    PackageNotFound { package_id: String },

    #[error("Peer reputation too low: {peer_id} (score: {score})")]
    LowReputation {
        peer_id: String,
        score: f64,
    },

    #[error("Bootstrap failed: {0}")]
    BootstrapFailed(String),

    #[error("Configuration error: {0}")]
    ConfigError(String),
}
```

### 5.2 Error Handling Pattern

```rust
pub fn run(args: &StartArgs) -> Result<()> {
    runtime::execute(start_async(args.clone()))
        .map_err(|e| {
            eprintln!("❌ Error: {}", e);

            // Provide actionable suggestions
            if e.to_string().contains("bind") {
                eprintln!("💡 Tip: Port may be in use. Try a different listen address.");
            } else if e.to_string().contains("bootstrap") {
                eprintln!("💡 Tip: Check bootstrap node addresses and network connectivity.");
            }

            e
        })
}
```

---

## 6. User Experience Flow

### 6.1 First-Time Setup Flow

```
1. User installs ggen
   $ cargo install ggen-cli

2. User initializes P2P node
   $ ggen marketplace p2p start

   Output:
   ⚙️  Initializing P2P node...
   🔧 Creating default configuration at ~/.ggen/p2p-config.toml
   📡 Starting libp2p swarm...
   ✅ P2P node started successfully
   📡 Peer ID: 12D3KooWGmB8...
   🎧 Listening on /ip4/127.0.0.1/tcp/54321
   🔄 Bootstrapping DHT from 2 nodes...
   ✅ Connected to 3 peers
   🚀 Node is ready for P2P operations

3. User searches P2P network
   $ ggen marketplace p2p search "web framework"

   Output:
   🔍 Searching P2P network...
   📦 Found 5 packages:

   1. actix-web-starter (v1.0.0)
      Author: alice@example.com
      Description: Production-ready Actix Web template
      Peers: 12 | Downloads: 234

   2. rocket-template (v2.1.0)
      Author: bob@example.com
      Description: Rocket framework boilerplate
      Peers: 8 | Downloads: 156

4. User publishes a package
   $ ggen marketplace p2p publish ./my-package

   Output:
   📦 Publishing package: my-package v1.0.0
   🔐 Calculating package hash...
   📝 Storing in DHT...
   📢 Announcing to gossipsub topic /ggen/packages/v1
   ✅ Package published successfully
   🌐 Available on 12 connected peers
```

### 6.2 Status Check Flow

```bash
$ ggen marketplace p2p status

P2P Node Status
===============
📡 Node Information
   Peer ID: 12D3KooWGmB8v9gW...
   Uptime: 2h 34m

🌐 Network
   Listening: /ip4/127.0.0.1/tcp/54321
   DHT Mode: Server
   Connected Peers: 15 / 50 max

📊 Activity
   Packages Published: 3
   Packages Discovered: 47
   DHT Queries: 23
   Gossipsub Messages: 156

🤝 Peer Reputation (Top 5)
   12D3KooWAbc1... | 98.5% | 45ms
   12D3KooWDef2... | 95.2% | 120ms
   12D3KooWGhi3... | 92.8% | 67ms
```

### 6.3 Error Recovery Flow

```bash
$ ggen marketplace p2p search "package"

❌ Error: P2P node not started. Run 'ggen marketplace p2p start' first.

💡 Quick fix:
   $ ggen marketplace p2p start

---

$ ggen marketplace p2p start

❌ Error: Failed to bind to /ip4/0.0.0.0/tcp/4001

💡 Tip: Port may be in use. Try a different listen address:
   $ ggen marketplace p2p start --listen /ip4/0.0.0.0/tcp/0

---

$ ggen marketplace p2p connect /invalid/address

❌ Error: Failed to parse multiaddress: invalid protocol

💡 Example valid multiaddresses:
   /ip4/192.168.1.100/tcp/4001/p2p/12D3KooW...
   /dns4/example.com/tcp/4001/p2p/12D3KooW...
```

---

## 7. Integration Patterns

### 7.1 Integration with Existing Marketplace Commands

**Unified Search Pattern:**

```rust
// User can search both centralized and P2P registries
$ ggen marketplace search "web" --registry all
$ ggen marketplace search "web" --registry p2p
$ ggen marketplace search "web" --registry central
```

**Implementation:**

```rust
// cli/src/domain/marketplace/search.rs
pub async fn search_packages(query: &str, filters: &SearchFilters) -> Result<Vec<SearchResult>> {
    let mut results = Vec::new();

    match &filters.registry {
        Some(reg) if reg == "p2p" => {
            // P2P only
            results.extend(search_p2p(query, filters).await?);
        }
        Some(reg) if reg == "central" => {
            // Central only
            results.extend(search_central(query, filters).await?);
        }
        _ => {
            // Search both (default)
            let (p2p_results, central_results) = tokio::join!(
                search_p2p(query, filters),
                search_central(query, filters)
            );

            if let Ok(p2p) = p2p_results {
                results.extend(p2p);
            }
            if let Ok(central) = central_results {
                results.extend(central);
            }
        }
    }

    Ok(results)
}
```

### 7.2 Fallback Strategy

```
┌─────────────────────────────────────┐
│  Search Request                     │
└──────────────┬──────────────────────┘
               │
               ▼
┌──────────────────────────────────────┐
│  Try P2P Registry (if node running)  │
└──────────────┬───────────────────────┘
               │
               ├─ Success ──────────┐
               │                    │
               └─ Failure           │
                  │                 │
                  ▼                 │
        ┌─────────────────────┐    │
        │ Try Central Registry│    │
        └──────────┬──────────┘    │
                   │                │
                   ├─ Success ──────┤
                   │                │
                   └─ Failure       │
                      │             │
                      ▼             ▼
                ┌──────────────────────┐
                │  Merge & Deduplicate │
                └──────────────────────┘
```

### 7.3 Background Sync Pattern

```rust
// Auto-sync P2P packages to local cache
pub async fn sync_p2p_cache() -> Result<()> {
    let registry = node_manager::get_global_registry().await?;

    // Query DHT for new packages
    let discovered = registry.discover_new_packages().await?;

    // Cache popular packages locally
    for package in discovered {
        if package.downloads > 100 {
            cache_package(&package).await?;
        }
    }

    Ok(())
}
```

---

## 8. Testing Strategy

### 8.1 Unit Tests

```rust
#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_config_default() {
        let config = P2PConfigFile::default();
        assert_eq!(config.network.dht_server_mode, true);
        assert_eq!(config.network.packages_topic, "/ggen/packages/v1");
    }

    #[tokio::test]
    async fn test_node_manager_lifecycle() {
        assert!(!node_manager::is_running().await);

        let config = P2PConfig::default();
        let registry = P2PRegistry::new(config).await.unwrap();

        node_manager::set_global_registry(registry).await.unwrap();
        assert!(node_manager::is_running().await);

        node_manager::stop_global_registry().await.unwrap();
        assert!(!node_manager::is_running().await);
    }
}
```

### 8.2 Integration Tests

**File:** `cli/tests/p2p_integration.rs`

```rust
#[tokio::test]
async fn test_p2p_start_and_status() {
    let temp_dir = TempDir::new().unwrap();

    // Start P2P node
    Command::cargo_bin("ggen")
        .unwrap()
        .args(&["marketplace", "p2p", "start", "--listen", "/ip4/127.0.0.1/tcp/0"])
        .current_dir(&temp_dir)
        .assert()
        .success();

    // Check status
    Command::cargo_bin("ggen")
        .unwrap()
        .args(&["marketplace", "p2p", "status"])
        .current_dir(&temp_dir)
        .assert()
        .success()
        .stdout(predicate::str::contains("Peer ID"));
}

#[tokio::test]
async fn test_p2p_publish_and_search() {
    // ... test implementation
}
```

### 8.3 End-to-End Tests

```rust
#[tokio::test]
async fn test_e2e_p2p_workflow() {
    // 1. Start two P2P nodes
    // 2. Node A publishes package
    // 3. Node B searches and finds package
    // 4. Node B downloads package
    // 5. Verify package integrity
}
```

---

## 9. Performance Considerations

### 9.1 Optimization Strategies

1. **Lazy Node Initialization**
   - Don't start P2P node unless explicitly requested
   - Cache node state across commands

2. **Parallel DHT Queries**
   ```rust
   let futures: Vec<_> = package_ids
       .iter()
       .map(|id| registry.query_dht(id))
       .collect();

   let results = futures::future::join_all(futures).await;
   ```

3. **Local Caching**
   - Cache frequently accessed packages
   - Implement TTL-based expiration
   - Deduplicate storage with content-addressing

4. **Connection Pooling**
   - Maintain persistent connections to high-reputation peers
   - Implement connection limits

### 9.2 Performance Targets

| Operation | Target | Rationale |
|-----------|--------|-----------|
| Node startup | < 3s | Reasonable for network operations |
| Status check | < 100ms | Local operation, should be instant |
| DHT query | < 5s | Network-dependent, timeout fallback |
| Package publish | < 10s | DHT replication + gossipsub |
| Search | < 3s | Parallel queries across peers |

---

## 10. Security Architecture

### 10.1 Security Layers

```
┌─────────────────────────────────────────────────────┐
│  Application Layer                                  │
│  - Input validation                                 │
│  - Access control                                   │
└──────────────────┬──────────────────────────────────┘
                   │
                   ▼
┌─────────────────────────────────────────────────────┐
│  P2P Protocol Layer                                 │
│  - Signed messages (Ed25519)                        │
│  - Peer authentication (libp2p Identify)            │
│  - Message validation (Gossipsub strict mode)       │
└──────────────────┬──────────────────────────────────┘
                   │
                   ▼
┌─────────────────────────────────────────────────────┐
│  Transport Layer                                    │
│  - TLS encryption (Noise protocol)                  │
│  - Connection multiplexing (Yamux)                  │
└─────────────────────────────────────────────────────┘
```

### 10.2 Threat Model

| Threat | Mitigation |
|--------|------------|
| Malicious packages | Content verification, peer reputation |
| Sybil attacks | DHT record validation, rate limiting |
| Eclipse attacks | Multiple bootstrap nodes, peer diversity |
| Man-in-the-middle | TLS with Noise protocol |
| DDoS | Connection limits, rate limiting |

### 10.3 Security Best Practices

1. **Package Verification**
   ```rust
   pub async fn verify_package(package: &Package) -> Result<()> {
       // 1. Verify signature
       verify_signature(&package.metadata, &package.signature)?;

       // 2. Verify content hash
       let computed_hash = compute_package_hash(&package.content)?;
       if computed_hash != package.id {
           return Err(anyhow::anyhow!("Package hash mismatch"));
       }

       // 3. Check peer reputation
       let peer_id = package.provider;
       let reputation = registry.get_peer_reputation(&peer_id).await;
       if reputation < 0.7 {
           return Err(anyhow::anyhow!("Low peer reputation"));
       }

       Ok(())
   }
   ```

2. **Peer Reputation Tracking**
   - Track successful/failed retrievals
   - Ban peers after threshold
   - Prefer high-reputation peers

3. **Rate Limiting**
   ```rust
   pub struct RateLimiter {
       requests_per_minute: HashMap<PeerId, u32>,
       max_requests: u32,
   }
   ```

---

## Appendices

### Appendix A: File Structure Summary

```
cli/
├── src/
│   ├── cmds/
│   │   └── marketplace.rs         # Add P2pCmd enum
│   ├── domain/
│   │   └── marketplace/
│   │       └── p2p/               # NEW directory
│   │           ├── mod.rs
│   │           ├── start.rs
│   │           ├── status.rs
│   │           ├── publish.rs
│   │           ├── search.rs
│   │           ├── connect.rs
│   │           ├── disconnect.rs
│   │           ├── peers.rs
│   │           ├── bootstrap.rs
│   │           ├── config.rs
│   │           └── node_manager.rs
│   └── ...
└── tests/
    └── p2p_integration.rs         # NEW test file

~/.ggen/
├── p2p-config.toml                # User configuration
└── p2p-cache/                     # Package cache
    └── ...
```

### Appendix B: Command Reference Quick Sheet

```bash
# Start P2P node
ggen marketplace p2p start [--bootstrap <ADDR>] [--listen <ADDR>]

# Status
ggen marketplace p2p status [--json] [--detailed]

# Publishing
ggen marketplace p2p publish <PACKAGE> [--announce] [--dht-store]

# Searching
ggen marketplace p2p search <QUERY> [-c <CATEGORY>] [-l <LIMIT>]

# Peer management
ggen marketplace p2p connect <PEER>
ggen marketplace p2p disconnect [--peer <ID>]
ggen marketplace p2p peers [--json]

# DHT
ggen marketplace p2p bootstrap [--nodes <ADDR>...]

# Configuration
ggen marketplace p2p config show
ggen marketplace p2p config edit
ggen marketplace p2p config reset
ggen marketplace p2p config validate
```

### Appendix C: Dependencies

**Add to `cli/Cargo.toml`:**

```toml
[dependencies]
# Existing dependencies
ggen-marketplace = { path = "../ggen-marketplace", version = "2.3.0" }

# P2P dependencies (already in ggen-marketplace)
# libp2p = "0.54"
# tokio = { version = "1", features = ["full"] }
# futures = "0.3"
```

**No new top-level dependencies needed** - reuse existing from ggen-marketplace.

---

## Conclusion

This architecture design provides a comprehensive blueprint for integrating P2P marketplace functionality into the ggen CLI using clap-noun-verb v3.0.0 patterns. The design:

1. ✅ Follows existing architectural patterns
2. ✅ Maintains separation of concerns (CLI → Runtime → Domain → Backend)
3. ✅ Provides comprehensive command structure
4. ✅ Includes robust error handling and user feedback
5. ✅ Supports configuration management
6. ✅ Plans for security and performance
7. ✅ Includes testing strategy
8. ✅ Integrates seamlessly with existing marketplace commands

**Next Steps for Implementation:**

1. **Phase 1:** Create domain layer structure (`cli/src/domain/marketplace/p2p/`)
2. **Phase 2:** Implement CLI commands layer (`cli/src/cmds/marketplace.rs` updates)
3. **Phase 3:** Build node manager for state management
4. **Phase 4:** Implement configuration system
5. **Phase 5:** Add integration tests
6. **Phase 6:** Write comprehensive documentation
7. **Phase 7:** Performance tuning and security hardening

---

**Document Metadata:**
- **Version:** 1.0.0
- **Last Updated:** 2025-11-02
- **Status:** APPROVED FOR IMPLEMENTATION
- **Next Review:** After Phase 3 completion
