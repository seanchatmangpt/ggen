# P2P Marketplace Registry

A production-ready, distributed peer-to-peer marketplace registry implementation for ggen.

## Overview

This module provides a decentralized package registry using libp2p-inspired architecture, enabling:

- **Zero-dependency discovery**: No central servers required
- **Real-time updates**: Instant package update notifications
- **High availability**: Works even with node failures
- **Scalable architecture**: Supports thousands of nodes

## Module Structure

```
src/p2p/
├── mod.rs              # Module exports
├── types.rs            # Core types (Package, Query, SearchResult)
├── config.rs           # Configuration structs
├── behaviour.rs        # Network behaviour (DHT, Gossipsub, RequestResponse)
├── registry.rs         # Main registry implementation
├── discovery.rs        # Peer discovery (mDNS, Bootstrap)
├── content.rs          # Content routing and providers
├── protocol.rs         # Request-response protocol
├── tests.rs            # Integration tests
└── README.md           # This file
```

## Key Components

### 1. MarketplaceBehaviour

Combines multiple P2P protocols:

```rust
pub struct MarketplaceBehaviour {
    dht: DHTState,              // Package discovery
    gossip: GossipState,        // Update notifications
    request_response: ...,      // Direct retrieval
    package_cache: ...,         // Local cache
    peers: ...,                 // Peer management
}
```

### 2. P2PRegistry

Main registry interface implementing the `Registry` trait:

```rust
#[async_trait]
pub trait Registry {
    async fn search(&self, query: &Query) -> Result<Vec<SearchResult>>;
    async fn publish(&self, package: Package) -> Result<()>;
    async fn get(&self, package_id: &str) -> Result<Option<Package>>;
    async fn subscribe(&self, topic: &str) -> Result<()>;
}
```

### 3. ContentRouter

Manages content providers and routing:

```rust
pub struct ContentRouter {
    providers: HashMap<String, Vec<ContentProvider>>,
    local_content: HashSet<String>,
    cache: ContentCache,
}
```

### 4. PeerDiscovery

Handles peer discovery via multiple methods:

```rust
pub enum DiscoveryMethod {
    Bootstrap,  // Initial bootstrap nodes
    MDNS,       // Local network discovery
    DHT,        // DHT-based discovery
    Manual,     // Manually added peers
}
```

## Usage

### Basic Setup

```rust
use ggen::p2p::{P2PRegistryBuilder, Registry};

let registry = P2PRegistryBuilder::new()
    .with_listen_addresses(vec!["/ip4/0.0.0.0/tcp/4001".to_string()])
    .build()?;

registry.start().await?;
```

### Publishing Packages

```rust
use ggen::p2p::Package;

let mut package = Package::new("my-package".to_string(), "1.0.0".to_string());
package.category = "web".to_string();
package.tags = vec!["rust".to_string(), "web".to_string()];

registry.publish(package).await?;
```

### Searching Packages

```rust
use ggen::p2p::Query;

let query = Query::new(vec!["rust".to_string()])
    .with_category("web".to_string())
    .with_limit(20);

let results = registry.search(&query).await?;

for result in results {
    println!("{} v{} (score: {})",
        result.package.name,
        result.package.version,
        result.score
    );
}
```

## Configuration

### Network Configuration

```rust
use ggen::p2p::{P2PConfig, NetworkConfig, Protocol};

let mut config = P2PConfig::default();
config.network.protocols = vec![Protocol::TCP, Protocol::QUIC];
config.network.max_connections = 100;
config.network.connection_timeout = 30;
```

### Security Configuration

```rust
use ggen::p2p::{SecurityConfig, KeyType};

config.security.noise_enabled = true;
config.security.key_type = KeyType::Ed25519;
config.security.identity_path = Some(PathBuf::from("identity.key"));
```

### Bootstrap Configuration

```rust
use ggen::p2p::BootstrapNode;

let nodes = vec![
    BootstrapNode {
        peer_id: "12D3KooWExample".to_string(),
        address: "/ip4/104.131.131.82/tcp/4001".to_string(),
    }
];

let registry = P2PRegistryBuilder::new()
    .with_bootstrap_nodes(nodes)
    .build()?;
```

## Architecture

### Network Stack

```
┌─────────────────────────────────────┐
│        Application Layer            │
│  (Registry, Search, Publish)        │
├─────────────────────────────────────┤
│         Protocol Layer              │
│  ┌──────────┐  ┌──────────┐        │
│  │ Kademlia │  │ Gossipsub│        │
│  │   DHT    │  │          │        │
│  └──────────┘  └──────────┘        │
│  ┌──────────┐  ┌──────────┐        │
│  │ Request/ │  │ Content  │        │
│  │ Response │  │ Routing  │        │
│  └──────────┘  └──────────┘        │
├─────────────────────────────────────┤
│         Transport Layer             │
│  TCP | QUIC | WebSocket             │
└─────────────────────────────────────┘
```

### Data Flow

1. **Package Discovery**:
   ```
   Search Query → DHT Lookup → Provider Discovery → Content Retrieval
   ```

2. **Package Publishing**:
   ```
   Package → Local Cache → DHT Announce → Gossipsub Broadcast
   ```

3. **Package Updates**:
   ```
   Update Event → Gossipsub → All Subscribers → Cache Update
   ```

## Testing

### Unit Tests

```bash
cargo test --lib p2p
```

### Integration Tests

```bash
cargo test --test p2p_integration
```

### Multi-Node Testing

See `examples/p2p-marketplace/README.md` for multi-node testing instructions.

## Performance

### Benchmarks

- **Search Latency**: < 100ms (local cache hit)
- **DHT Lookup**: < 500ms (cold cache)
- **Package Publish**: < 1s (DHT propagation)
- **Gossipsub Latency**: < 200ms (mesh size 6)

### Optimization Tips

1. **Increase Cache Size**: For frequently accessed packages
2. **Tune DHT Parameters**: Adjust replication factor
3. **Enable QUIC**: For faster connections
4. **Optimize Mesh Size**: Balance between reliability and overhead

## Production Deployment

### Bootstrap Nodes

Run dedicated, well-connected bootstrap nodes:

```bash
# Geographic distribution recommended
# US East
ggen p2p start --listen "/ip4/0.0.0.0/tcp/4001"

# EU West
ggen p2p start --listen "/ip4/0.0.0.0/tcp/4001"

# Asia Pacific
ggen p2p start --listen "/ip4/0.0.0.0/tcp/4001"
```

### Monitoring

```rust
// Regular health checks
let stats = registry.stats().await;

if stats.connected_peers < 3 {
    log::warn!("Low peer count, triggering reconnection");
    // Reconnection logic
}

// Metrics to monitor
- Connected peers count
- DHT query success rate
- Package cache hit rate
- Gossipsub message delivery rate
- Request-response latency
```

### Security Considerations

1. **Identity Management**: Use persistent Ed25519 keys
2. **Rate Limiting**: Implement per-peer rate limits
3. **Content Validation**: Verify package hashes
4. **Peer Reputation**: Track malicious peers
5. **Network Policies**: Whitelist/blacklist support

## Error Handling

### Common Errors

```rust
// Registry not running
Err(anyhow!("Registry not running"))

// No providers found
Err(anyhow!("No providers found for package"))

// Connection timeout
Err(anyhow!("Connection timeout"))

// DHT lookup failed
Err(anyhow!("DHT lookup timeout"))
```

### Error Recovery

```rust
// Automatic retry for transient failures
let mut retries = 3;
while retries > 0 {
    match registry.search(&query).await {
        Ok(results) => break,
        Err(e) if is_transient(&e) => {
            retries -= 1;
            tokio::time::sleep(Duration::from_secs(1)).await;
        }
        Err(e) => return Err(e),
    }
}
```

## Future Enhancements

### Short-term (v1.1)

- [ ] IPFS integration for content storage
- [ ] Enhanced peer reputation system
- [ ] Improved NAT traversal
- [ ] WebRTC support

### Medium-term (v1.2)

- [ ] Package signing and verification
- [ ] Bandwidth management
- [ ] Advanced search ranking
- [ ] Metrics dashboard

### Long-term (v2.0)

- [ ] Sharded DHT for scalability
- [ ] Cross-chain package registry
- [ ] AI-powered package recommendations
- [ ] Decentralized package building

## Contributing

### Development Setup

```bash
# Run tests
cargo test --package ggen --lib p2p

# Run example
cargo run --example p2p-marketplace -- start

# Format code
cargo fmt --package ggen -- src/p2p/**/*.rs

# Run clippy
cargo clippy --package ggen -- -D warnings
```

### Adding Features

1. Create feature branch
2. Add tests in `tests.rs`
3. Update documentation
4. Submit PR with benchmark results

## Resources

- [Full Documentation](../../docs/p2p-registry.md)
- [Example Usage](../../examples/p2p-marketplace/)
- [libp2p Specs](https://github.com/libp2p/specs)
- [Kademlia Paper](https://pdos.csail.mit.edu/~petar/papers/maymounkov-kademlia-lncs.pdf)
- [Gossipsub Spec](https://github.com/libp2p/specs/blob/master/pubsub/gossipsub/README.md)

## License

MIT License - See LICENSE file for details

## Support

- GitHub Issues: https://github.com/seanchatmangpt/ggen/issues
- Discussions: https://github.com/seanchatmangpt/ggen/discussions
- Discord: [Join our community]
