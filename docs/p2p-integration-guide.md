# P2P Registry Integration Guide

This guide walks through integrating the P2P marketplace registry into the ggen CLI.

## Overview

The P2P registry integration adds decentralized package discovery and distribution to ggen, enabling:

1. **Offline-first operation**: Local package cache
2. **Peer-to-peer discovery**: No central server required
3. **Real-time updates**: Instant package notifications
4. **Resilient architecture**: Works with node failures

## Integration Architecture

```
┌────────────────────────────────────────────────────────┐
│                    Ggen CLI                             │
├────────────────────────────────────────────────────────┤
│                                                         │
│  ┌──────────────┐         ┌──────────────┐            │
│  │  Marketplace │◄───────►│  P2P Registry│            │
│  │   Commands   │         │              │            │
│  └──────────────┘         └──────────────┘            │
│         │                        │                     │
│         │                        ▼                     │
│         │              ┌──────────────────┐            │
│         │              │ DHT + Gossipsub  │            │
│         │              │ + Content Router │            │
│         │              └──────────────────┘            │
│         │                        │                     │
│         ▼                        ▼                     │
│  ┌──────────────┐      ┌──────────────┐              │
│  │Local Storage │      │ P2P Network  │              │
│  │   (Cache)    │      │   (Peers)    │              │
│  └──────────────┘      └──────────────┘              │
│                                                         │
└────────────────────────────────────────────────────────┘
```

## Step 1: CLI Command Integration

### Add P2P Commands

Create `cli/src/commands/p2p.rs`:

```rust
use clap::{Args, Subcommand};
use anyhow::Result;
use ggen::p2p::{P2PRegistryBuilder, Registry, Package, Query};

#[derive(Args)]
pub struct P2PArgs {
    #[command(subcommand)]
    command: P2PCommands,
}

#[derive(Subcommand)]
enum P2PCommands {
    /// Start P2P node
    Start {
        /// Listen address
        #[arg(short, long, default_value = "/ip4/0.0.0.0/tcp/4001")]
        listen: String,

        /// Bootstrap nodes
        #[arg(short, long)]
        bootstrap: Vec<String>,
    },

    /// Publish package to P2P network
    Publish {
        /// Package path
        package: String,
    },

    /// Search P2P network for packages
    Search {
        /// Search query
        query: Vec<String>,

        /// Category filter
        #[arg(short, long)]
        category: Option<String>,
    },

    /// Show P2P network status
    Status,
}

impl P2PArgs {
    pub async fn execute(&self) -> Result<()> {
        match &self.command {
            P2PCommands::Start { listen, bootstrap } => {
                start_node(listen, bootstrap).await
            }
            P2PCommands::Publish { package } => {
                publish_package(package).await
            }
            P2PCommands::Search { query, category } => {
                search_packages(query, category).await
            }
            P2PCommands::Status => {
                show_status().await
            }
        }
    }
}

async fn start_node(listen: &str, bootstrap: &[String]) -> Result<()> {
    println!("🚀 Starting P2P node...");

    let registry = P2PRegistryBuilder::new()
        .with_listen_addresses(vec![listen.to_string()])
        .build()?;

    registry.start().await?;

    println!("✅ Node started on {}", listen);
    println!("📊 Connected peers: {}", registry.stats().await.connected_peers);

    // Keep running
    tokio::signal::ctrl_c().await?;

    registry.stop().await?;
    Ok(())
}

async fn publish_package(package_path: &str) -> Result<()> {
    println!("📦 Publishing package: {}", package_path);

    // Load package metadata
    let package = load_package_from_path(package_path)?;

    let registry = P2PRegistryBuilder::new().build()?;
    registry.start().await?;

    registry.publish(package).await?;

    println!("✅ Package published successfully!");

    registry.stop().await?;
    Ok(())
}

async fn search_packages(query: &[String], category: &Option<String>) -> Result<()> {
    let registry = P2PRegistryBuilder::new().build()?;
    registry.start().await?;

    let mut search_query = Query::new(query.to_vec());
    if let Some(cat) = category {
        search_query = search_query.with_category(cat.clone());
    }

    let results = registry.search(&search_query).await?;

    println!("📋 Found {} packages:", results.len());
    for result in results {
        println!("  {} v{} (score: {:.2})",
            result.package.name,
            result.package.version,
            result.score
        );
    }

    registry.stop().await?;
    Ok(())
}

async fn show_status() -> Result<()> {
    let registry = P2PRegistryBuilder::new().build()?;
    registry.start().await?;

    let stats = registry.stats().await;

    println!("📊 P2P Network Status:");
    println!("  Connected peers: {}", stats.connected_peers);
    println!("  Cached packages: {}", stats.cached_packages);
    println!("  Provider records: {}", stats.provider_records);

    registry.stop().await?;
    Ok(())
}

fn load_package_from_path(path: &str) -> Result<Package> {
    // Load package metadata from ggen.toml or package.json
    // This is a placeholder - implement based on your package format
    let package = Package::new("example".to_string(), "1.0.0".to_string());
    Ok(package)
}
```

### Update Main CLI

Add to `cli/src/main.rs`:

```rust
use clap::{Parser, Subcommand};

#[derive(Parser)]
struct Cli {
    #[command(subcommand)]
    command: Commands,
}

#[derive(Subcommand)]
enum Commands {
    // ... existing commands ...

    /// P2P marketplace operations
    P2p(p2p::P2PArgs),
}

#[tokio::main]
async fn main() -> Result<()> {
    let cli = Cli::parse();

    match cli.command {
        // ... existing command handlers ...

        Commands::P2p(args) => args.execute().await,
    }
}
```

## Step 2: Configuration

### Add P2P Config to ggen.toml

```toml
[p2p]
enabled = true
listen_addresses = [
    "/ip4/0.0.0.0/tcp/4001",
    "/ip4/0.0.0.0/udp/4001/quic-v1"
]

[p2p.bootstrap]
enabled = true
nodes = [
    { peer_id = "12D3KooWExample1", address = "/ip4/104.131.131.82/tcp/4001" },
    { peer_id = "12D3KooWExample2", address = "/ip4/104.131.131.83/tcp/4001" }
]
mdns_enabled = true

[p2p.dht]
replication_factor = 20
query_timeout = 60
cache_size = 1000

[p2p.gossipsub]
mesh_n = 6
mesh_n_low = 4
mesh_n_high = 12
```

### Load Config in Rust

```rust
use serde::{Deserialize, Serialize};
use ggen::p2p::{P2PConfig, BootstrapNode};

#[derive(Deserialize)]
struct GgenConfig {
    p2p: P2PConfigWrapper,
}

#[derive(Deserialize)]
struct P2PConfigWrapper {
    enabled: bool,
    listen_addresses: Vec<String>,
    bootstrap: BootstrapConfigWrapper,
}

#[derive(Deserialize)]
struct BootstrapConfigWrapper {
    enabled: bool,
    nodes: Vec<BootstrapNode>,
    mdns_enabled: bool,
}

fn load_p2p_config() -> Result<P2PConfig> {
    let config_str = std::fs::read_to_string("ggen.toml")?;
    let config: GgenConfig = toml::from_str(&config_str)?;

    let mut p2p_config = P2PConfig::default();
    p2p_config.network.listen_addresses = config.p2p.listen_addresses;
    p2p_config.bootstrap.enabled = config.p2p.bootstrap.enabled;
    p2p_config.bootstrap.nodes = config.p2p.bootstrap.nodes;
    p2p_config.bootstrap.mdns_enabled = config.p2p.bootstrap.mdns_enabled;

    Ok(p2p_config)
}
```

## Step 3: Marketplace Integration

### Hybrid Registry Strategy

Implement a hybrid approach that uses both central and P2P registries:

```rust
use ggen::p2p::{P2PRegistry, Registry as P2PRegistryTrait};

pub enum RegistryMode {
    Central,
    P2P,
    Hybrid,
}

pub struct HybridRegistry {
    central: Box<dyn CentralRegistry>,
    p2p: P2PRegistry,
    mode: RegistryMode,
}

impl HybridRegistry {
    pub async fn new(mode: RegistryMode) -> Result<Self> {
        let central = Box::new(DefaultCentralRegistry::new()?);
        let p2p = P2PRegistryBuilder::new().build()?;

        if matches!(mode, RegistryMode::P2P | RegistryMode::Hybrid) {
            p2p.start().await?;
        }

        Ok(Self { central, p2p, mode })
    }

    pub async fn search(&self, query: &Query) -> Result<Vec<SearchResult>> {
        match self.mode {
            RegistryMode::Central => {
                self.central.search(query).await
            }
            RegistryMode::P2P => {
                self.p2p.search(query).await
            }
            RegistryMode::Hybrid => {
                // Try P2P first, fall back to central
                match self.p2p.search(query).await {
                    Ok(results) if !results.is_empty() => Ok(results),
                    _ => self.central.search(query).await,
                }
            }
        }
    }

    pub async fn publish(&self, package: Package) -> Result<()> {
        match self.mode {
            RegistryMode::Central => {
                self.central.publish(package).await
            }
            RegistryMode::P2P => {
                self.p2p.publish(package).await
            }
            RegistryMode::Hybrid => {
                // Publish to both
                self.central.publish(package.clone()).await?;
                self.p2p.publish(package).await?;
                Ok(())
            }
        }
    }
}
```

### Update Marketplace Commands

```rust
// In marketplace.rs

use crate::registry::HybridRegistry;

pub async fn search_packages(query: &str) -> Result<()> {
    let registry = HybridRegistry::new(RegistryMode::Hybrid).await?;

    let search_query = Query::new(vec![query.to_string()]);
    let results = registry.search(&search_query).await?;

    // Display results...
    Ok(())
}

pub async fn install_package(package_id: &str) -> Result<()> {
    let registry = HybridRegistry::new(RegistryMode::Hybrid).await?;

    // Try to get from P2P first
    if let Some(package) = registry.p2p.get(package_id).await? {
        download_and_install(package).await?;
    } else {
        // Fall back to central registry
        let package = registry.central.get(package_id).await?;
        download_and_install(package).await?;
    }

    Ok(())
}
```

## Step 4: Background Service

### Create P2P Daemon

For long-running P2P node:

```rust
use tokio::sync::RwLock;
use std::sync::Arc;

pub struct P2PDaemon {
    registry: Arc<RwLock<P2PRegistry>>,
    running: Arc<RwLock<bool>>,
}

impl P2PDaemon {
    pub async fn new(config: P2PConfig) -> Result<Self> {
        let registry = P2PRegistry::new(config)?;

        Ok(Self {
            registry: Arc::new(RwLock::new(registry)),
            running: Arc::new(RwLock::new(false)),
        })
    }

    pub async fn start(&self) -> Result<()> {
        let mut running = self.running.write().await;
        if *running {
            return Ok(());
        }

        let registry = self.registry.read().await;
        registry.start().await?;

        *running = true;

        // Start background tasks
        tokio::spawn(self.clone().health_check_loop());
        tokio::spawn(self.clone().metrics_loop());

        Ok(())
    }

    pub async fn stop(&self) -> Result<()> {
        let mut running = self.running.write().await;
        *running = false;

        let registry = self.registry.read().await;
        registry.stop().await?;

        Ok(())
    }

    async fn health_check_loop(self) {
        while *self.running.read().await {
            let stats = self.registry.read().await.stats().await;

            if stats.connected_peers < 3 {
                log::warn!("Low peer count: {}", stats.connected_peers);
                // Trigger reconnection
            }

            tokio::time::sleep(Duration::from_secs(30)).await;
        }
    }

    async fn metrics_loop(self) {
        while *self.running.read().await {
            let stats = self.registry.read().await.stats().await;

            // Log metrics
            log::info!("P2P Stats - Peers: {}, Packages: {}",
                stats.connected_peers,
                stats.cached_packages
            );

            tokio::time::sleep(Duration::from_secs(60)).await;
        }
    }
}
```

### System Service Integration

Create systemd service `/etc/systemd/system/ggen-p2p.service`:

```ini
[Unit]
Description=Ggen P2P Marketplace Node
After=network.target

[Service]
Type=simple
User=ggen
Group=ggen
WorkingDirectory=/opt/ggen
ExecStart=/usr/local/bin/ggen p2p start --listen "/ip4/0.0.0.0/tcp/4001"
Restart=always
RestartSec=10

[Install]
WantedBy=multi-user.target
```

## Step 5: Testing

### Integration Tests

```rust
#[cfg(test)]
mod integration_tests {
    use super::*;

    #[tokio::test]
    async fn test_hybrid_registry_search() {
        let registry = HybridRegistry::new(RegistryMode::Hybrid).await.unwrap();

        let query = Query::new(vec!["test".to_string()]);
        let results = registry.search(&query).await.unwrap();

        assert!(!results.is_empty());
    }

    #[tokio::test]
    async fn test_p2p_publish_and_search() {
        let registry = HybridRegistry::new(RegistryMode::P2P).await.unwrap();

        let package = Package::new("test-pkg".to_string(), "1.0.0".to_string());
        registry.publish(package.clone()).await.unwrap();

        // Wait for propagation
        tokio::time::sleep(Duration::from_secs(2)).await;

        let query = Query::new(vec!["test-pkg".to_string()]);
        let results = registry.search(&query).await.unwrap();

        assert_eq!(results.len(), 1);
    }
}
```

### CLI Testing

```bash
# Start P2P node
ggen p2p start --listen "/ip4/0.0.0.0/tcp/4001"

# In another terminal, publish a package
ggen p2p publish ./my-package

# Search P2P network
ggen p2p search "web framework"

# Check status
ggen p2p status
```

## Step 6: Deployment

### Production Checklist

- [ ] Configure bootstrap nodes
- [ ] Set up monitoring and alerts
- [ ] Enable logging
- [ ] Configure firewall rules (TCP 4001, UDP 4001)
- [ ] Set up health checks
- [ ] Configure rate limiting
- [ ] Enable metrics collection
- [ ] Set up backup registry nodes

### Monitoring

```rust
use prometheus::{Counter, Gauge, Registry};

pub struct P2PMetrics {
    connected_peers: Gauge,
    packages_cached: Gauge,
    search_requests: Counter,
    publish_requests: Counter,
}

impl P2PMetrics {
    pub fn new() -> Self {
        let registry = Registry::new();

        let connected_peers = Gauge::new("p2p_connected_peers", "Connected peers").unwrap();
        registry.register(Box::new(connected_peers.clone())).unwrap();

        // Register other metrics...

        Self {
            connected_peers,
            packages_cached,
            search_requests,
            publish_requests,
        }
    }

    pub async fn update(&self, stats: NetworkStats) {
        self.connected_peers.set(stats.connected_peers as f64);
        self.packages_cached.set(stats.cached_packages as f64);
    }
}
```

## Troubleshooting

### Common Issues

1. **No peers connecting**
   - Check firewall rules
   - Verify bootstrap node addresses
   - Enable mDNS for local discovery

2. **Slow package discovery**
   - Increase DHT replication factor
   - Add more bootstrap nodes
   - Enable caching

3. **High memory usage**
   - Reduce cache size
   - Limit connection count
   - Enable garbage collection

## Next Steps

1. Implement content verification
2. Add package signing
3. Implement peer reputation
4. Add bandwidth management
5. Create web UI for P2P status

## Resources

- [P2P Registry Documentation](./p2p-registry.md)
- [Example Implementation](../examples/p2p-marketplace/)
- [Configuration Reference](./p2p-config.md)
