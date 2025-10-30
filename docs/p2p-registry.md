# P2P Marketplace Registry

A distributed, peer-to-peer marketplace registry implementation using libp2p-inspired architecture.

## Overview

The P2P registry provides decentralized package discovery and distribution without requiring central servers. It uses:

- **Kademlia DHT** for distributed package lookup
- **Gossipsub** for real-time package update notifications
- **Request-Response** for direct package retrieval
- **Content Routing** to find providers for package content
- **Peer Discovery** via mDNS and bootstrap nodes

## Architecture

```
┌─────────────────────────────────────────────────┐
│           P2P Marketplace Registry              │
├─────────────────────────────────────────────────┤
│                                                 │
│  ┌──────────────┐      ┌──────────────┐       │
│  │   Kademlia   │      │  Gossipsub   │       │
│  │     DHT      │      │   (Updates)  │       │
│  └──────────────┘      └──────────────┘       │
│                                                 │
│  ┌──────────────┐      ┌──────────────┐       │
│  │  Request/    │      │   Content    │       │
│  │  Response    │      │   Routing    │       │
│  └──────────────┘      └──────────────┘       │
│                                                 │
│  ┌──────────────┐      ┌──────────────┐       │
│  │     Peer     │      │   Security   │       │
│  │  Discovery   │      │   (Noise)    │       │
│  └──────────────┘      └──────────────┘       │
│                                                 │
└─────────────────────────────────────────────────┘
```

## Features

### 1. DHT for Package Discovery

Uses a distributed hash table (Kademlia) for decentralized package lookup:

```rust
// Publish package to DHT
registry.publish(package).await?;

// Search packages via DHT
let query = Query::new(vec!["rust", "web"]);
let results = registry.search(&query).await?;
```

### 2. Gossipsub for Real-time Updates

Subscribe to package updates across the network:

```rust
// Subscribe to updates
registry.subscribe("ggen-marketplace/updates").await?;

// Updates are automatically received and processed
```

### 3. Request-Response Protocol

Direct package retrieval from peers:

```rust
// Request specific package
let package = registry.get("package-id").await?;
```

### 4. Content Routing

Find optimal providers for package content:

```rust
// System automatically finds best provider based on:
// - Proximity
// - Reputation
// - Availability
```

### 5. Peer Discovery

Automatic peer discovery via:
- **mDNS**: Local network discovery
- **Bootstrap nodes**: Initial network entry points
- **DHT**: Ongoing peer discovery

## Usage

### Basic Setup

```rust
use ggen::p2p::{P2PRegistryBuilder, Registry, Package, Query};

#[tokio::main]
async fn main() -> Result<()> {
    // Create registry
    let registry = P2PRegistryBuilder::new()
        .with_listen_addresses(vec![
            "/ip4/0.0.0.0/tcp/4001".to_string(),
            "/ip4/0.0.0.0/udp/4001/quic-v1".to_string(),
        ])
        .build()?;

    // Start P2P network
    registry.start().await?;

    // Use registry...

    // Stop when done
    registry.stop().await?;
    Ok(())
}
```

### Publishing Packages

```rust
let mut package = Package::new(
    "rust-web-framework".to_string(),
    "1.0.0".to_string()
);
package.category = "web".to_string();
package.tags = vec!["rust".to_string(), "web".to_string()];
package.description = "Fast web framework".to_string();

registry.publish(package).await?;
```

### Searching for Packages

```rust
// Basic search
let query = Query::new(vec!["rust".to_string()]);
let results = registry.search(&query).await?;

// Advanced search with filters
let query = Query::new(vec!["web".to_string()])
    .with_category("frameworks".to_string())
    .with_tags(vec!["rust".to_string()])
    .with_limit(20);

let results = registry.search(&query).await?;

// Results are ranked by relevance
for result in results {
    println!("Package: {} (score: {})",
        result.package.name,
        result.score
    );
}
```

### Bootstrap Configuration

```rust
use ggen::p2p::{P2PRegistryBuilder, BootstrapNode};

let nodes = vec![
    BootstrapNode {
        peer_id: "12D3KooWExample1".to_string(),
        address: "/ip4/104.131.131.82/tcp/4001".to_string(),
    },
    BootstrapNode {
        peer_id: "12D3KooWExample2".to_string(),
        address: "/ip4/104.131.131.83/tcp/4001".to_string(),
    },
];

let registry = P2PRegistryBuilder::new()
    .with_bootstrap_nodes(nodes)
    .build()?;
```

## Configuration

### Network Configuration

```rust
use ggen::p2p::{P2PConfig, NetworkConfig, Protocol};

let mut config = P2PConfig::default();

// Configure network protocols
config.network.protocols = vec![
    Protocol::TCP,
    Protocol::QUIC,
    Protocol::WebSocket,
];

// Set connection limits
config.network.max_connections = 100;
config.network.connection_timeout = 30;
```

### Security Configuration

```rust
use ggen::p2p::{SecurityConfig, KeyType};
use std::path::PathBuf;

let mut config = P2PConfig::default();

// Enable Noise protocol encryption
config.security.noise_enabled = true;
config.security.key_type = KeyType::Ed25519;

// Use persistent identity
config.security.identity_path = Some(
    PathBuf::from("/path/to/identity.key")
);
```

### DHT Configuration

```rust
let mut config = P2PConfig::default();

// Configure DHT parameters
config.dht.replication_factor = 20;
config.dht.query_timeout = 60;
config.dht.provider_record_ttl = 86400; // 24 hours
config.dht.cache_size = 1000;
```

### Gossipsub Configuration

```rust
let mut config = P2PConfig::default();

// Configure gossipsub mesh
config.gossipsub.mesh_n = 6;           // Target mesh size
config.gossipsub.mesh_n_low = 4;       // Minimum mesh size
config.gossipsub.mesh_n_high = 12;     // Maximum mesh size
config.gossipsub.heartbeat_interval = 1000; // 1 second
```

## Network Protocols

### Supported Transports

1. **TCP**: Reliable, widely supported
   ```
   /ip4/0.0.0.0/tcp/4001
   ```

2. **QUIC**: Fast, modern, UDP-based
   ```
   /ip4/0.0.0.0/udp/4001/quic-v1
   ```

3. **WebSocket**: Browser-compatible
   ```
   /ip4/0.0.0.0/tcp/4001/ws
   ```

### Security

All connections use the Noise Protocol Framework:
- **XX handshake pattern**
- **Ed25519 keys** for authentication
- **ChaCha20-Poly1305** for encryption

## Monitoring

### Network Statistics

```rust
let stats = registry.stats().await;

println!("Connected peers: {}", stats.connected_peers);
println!("Cached packages: {}", stats.cached_packages);
println!("Provider records: {}", stats.provider_records);
println!("Gossip topics: {}", stats.gossip_topics);
```

## Best Practices

### 1. Use Bootstrap Nodes

Always configure bootstrap nodes for reliable network entry:

```rust
let registry = P2PRegistryBuilder::new()
    .with_bootstrap_nodes(bootstrap_nodes)
    .build()?;
```

### 2. Enable mDNS for Local Discovery

For local development, enable mDNS:

```rust
let mut config = P2PConfig::default();
config.bootstrap.mdns_enabled = true;
```

### 3. Set Reasonable Timeouts

Configure timeouts based on your network:

```rust
config.network.connection_timeout = 30;  // seconds
config.network.idle_timeout = 300;       // 5 minutes
config.dht.query_timeout = 60;           // 1 minute
```

### 4. Monitor Network Health

Regularly check network statistics:

```rust
let stats = registry.stats().await;
if stats.connected_peers < 3 {
    // Trigger reconnection logic
}
```

## Examples

### Complete Example: Package Publisher

```rust
use ggen::p2p::{P2PRegistryBuilder, Registry, Package};

#[tokio::main]
async fn main() -> Result<()> {
    // Setup registry
    let registry = P2PRegistryBuilder::new()
        .with_listen_addresses(vec!["/ip4/0.0.0.0/tcp/0".to_string()])
        .build()?;

    registry.start().await?;

    // Create package
    let package = Package::new(
        "my-awesome-package".to_string(),
        "1.0.0".to_string()
    );

    // Publish to network
    registry.publish(package).await?;

    // Wait for propagation
    tokio::time::sleep(tokio::time::Duration::from_secs(5)).await;

    println!("Package published successfully!");

    registry.stop().await?;
    Ok(())
}
```

### Complete Example: Package Consumer

```rust
use ggen::p2p::{P2PRegistryBuilder, Registry, Query};

#[tokio::main]
async fn main() -> Result<()> {
    // Setup registry
    let registry = P2PRegistryBuilder::new().build()?;
    registry.start().await?;

    // Search for packages
    let query = Query::new(vec!["web".to_string(), "rust".to_string()])
        .with_limit(10);

    let results = registry.search(&query).await?;

    // Display results
    for result in results {
        println!("Found: {} v{}",
            result.package.name,
            result.package.version
        );
        println!("  Score: {}", result.score);
        println!("  Providers: {}", result.peer_count);
    }

    registry.stop().await?;
    Ok(())
}
```

## Troubleshooting

### No Peers Found

1. Check bootstrap node connectivity
2. Verify firewall rules allow P2P traffic
3. Enable mDNS for local discovery
4. Check network configuration

### Package Not Found

1. Allow time for DHT propagation (5-10 seconds)
2. Verify package was successfully published
3. Check provider records
4. Try searching with broader keywords

### Slow Performance

1. Increase connection limits
2. Add more bootstrap nodes
3. Enable QUIC protocol for faster connections
4. Increase cache size

## Future Enhancements

- [ ] IPFS integration for content storage
- [ ] Reputation system for peers
- [ ] Bandwidth management
- [ ] NAT traversal improvements
- [ ] WebRTC support for browsers
- [ ] Enhanced search ranking algorithms
- [ ] Package signing and verification

## References

- [libp2p Documentation](https://docs.libp2p.io/)
- [Kademlia DHT](https://en.wikipedia.org/wiki/Kademlia)
- [Gossipsub Protocol](https://github.com/libp2p/specs/tree/master/pubsub/gossipsub)
- [Noise Protocol Framework](https://noiseprotocol.org/)
