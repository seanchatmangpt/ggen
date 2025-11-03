# P2P Registry Implementation

## Overview

The P2P Registry provides a decentralized package registry using libp2p for peer-to-peer networking. This implementation enables distributed package discovery and retrieval without relying on a central server.

## Architecture

### Components

1. **Kademlia DHT (Distributed Hash Table)**
   - Decentralized peer discovery
   - Package metadata storage and retrieval
   - Content addressing for packages

2. **Gossipsub**
   - Pub/Sub messaging for package announcements
   - Real-time package update notifications
   - Efficient broadcast to interested peers

3. **Identify Protocol**
   - Peer identification and capability discovery
   - Protocol version negotiation

### Data Flow

```
Publisher                  Network                  Consumer
   |                          |                         |
   | 1. Publish Package       |                         |
   |------------------------->|                         |
   |                          |                         |
   | 2. Store in DHT          |                         |
   |------------------------->|                         |
   |                          |                         |
   | 3. Announce via Gossipsub|                         |
   |------------------------->|                         |
   |                          | 4. Gossip to peers      |
   |                          |------------------------>|
   |                          |                         |
   |                          | 5. Query DHT            |
   |                          |<------------------------|
   |                          |                         |
   |                          | 6. Retrieve package     |
   |                          |------------------------>|
```

## Features

### Implemented

- ✅ P2P network initialization with configurable topology
- ✅ Peer discovery via Kademlia DHT
- ✅ Package announcement via Gossipsub
- ✅ Package search (local first, then DHT)
- ✅ Package retrieval from peers
- ✅ Peer reputation tracking
- ✅ Registry trait implementation

### Planned

- ⏳ Content-addressed package storage (IPFS integration)
- ⏳ Package signature verification across peers
- ⏳ Bandwidth-efficient content transfer (BitSwap)
- ⏳ NAT traversal and hole punching
- ⏳ Byzantine fault tolerance
- ⏳ Sybil attack resistance

## Usage

### Basic Setup

```rust
use ggen_marketplace::prelude::*;
use ggen_marketplace::P2PRegistry;
use ggen_marketplace::backend::p2p::P2PConfig;

#[tokio::main]
async fn main() -> Result<()> {
    // Create configuration
    let config = P2PConfig {
        bootstrap_nodes: vec![
            "/ip4/104.131.131.82/tcp/4001/p2p/QmaCpDMGvV2BGHeYERUEnRQAwe3N8SzbUtfsmvsqQLuvuJ"
                .parse()
                .unwrap(),
        ],
        packages_topic: "/ggen/packages/v1".to_string(),
        dht_server_mode: true,
        listen_addresses: vec![
            "/ip4/0.0.0.0/tcp/0".parse().unwrap(),
        ],
    };

    // Initialize registry
    let registry = P2PRegistry::new(config).await?;

    // Start listening
    registry.start_listening().await?;

    // Subscribe to package announcements
    registry.subscribe_to_packages().await?;

    // Bootstrap DHT
    registry.bootstrap().await?;

    // Search for packages
    let query = Query::new("rust web framework");
    let results = registry.search(&query).await?;

    for package in results {
        println!("Found: {} v{}", package.id, package.version);
    }

    Ok(())
}
```

### Publishing Packages

```rust
use ggen_marketplace::prelude::*;

async fn publish_package(registry: &P2PRegistry) -> Result<()> {
    let package_id = PackageId::new("myorg", "awesome-lib");
    let version = Version::new(1, 0, 0);

    let package = Package::builder(package_id, version)
        .title("Awesome Library")
        .description("A truly awesome library")
        .license("MIT")
        .tag("rust")
        .tag("library")
        .content_id(ContentId::new(
            "QmX7Zd...",  // IPFS CID
            HashAlgorithm::Sha256,
        ))
        .build()?;

    // Publish to P2P network
    registry.publish(package).await?;

    println!("Package published to P2P network!");
    Ok(())
}
```

### Peer Reputation

```rust
use libp2p::PeerId;

async fn check_peer_reputation(registry: &P2PRegistry, peer_id: &PeerId) {
    let reputation = registry.get_peer_reputation(peer_id).await;
    println!("Peer {} reputation: {:.2}", peer_id, reputation);

    if reputation < 0.5 {
        println!("WARNING: Low reputation peer!");
    }
}
```

## Configuration

### P2PConfig Options

| Field | Type | Description | Default |
|-------|------|-------------|---------|
| `bootstrap_nodes` | `Vec<Multiaddr>` | Initial bootstrap nodes for DHT | `[]` |
| `packages_topic` | `String` | Gossipsub topic for package announcements | `/ggen/packages/v1` |
| `dht_server_mode` | `bool` | Act as DHT server node | `true` |
| `listen_addresses` | `Vec<Multiaddr>` | Local addresses to listen on | `[/ip4/0.0.0.0/tcp/0]` |

### Environment Variables

```bash
# Bootstrap nodes (comma-separated multiaddrs)
GGEN_P2P_BOOTSTRAP="/ip4/104.131.131.82/tcp/4001/p2p/QmaCpDMG...,/ip4/192.168.1.1/tcp/4001/p2p/QmXYZ..."

# Listen address
GGEN_P2P_LISTEN="/ip4/0.0.0.0/tcp/9000"

# Enable DHT server mode
GGEN_P2P_DHT_SERVER=true
```

## Testing

### Running Tests

```bash
# Run all P2P tests
cargo test --features p2p --test backend_p2p

# Run specific test
cargo test --features p2p test_discover_peers_via_dht

# With debug output
RUST_LOG=debug cargo test --features p2p -- --nocapture
```

### Test Coverage

The test suite includes:

1. **Network Initialization** - Verify P2P network starts correctly
2. **Peer Discovery** - Test DHT-based peer discovery
3. **Package Announcement** - Test Gossipsub package broadcasts
4. **Search Functionality** - Test package search across peers
5. **Package Retrieval** - Test content retrieval from remote peers
6. **Multi-Peer Discovery** - Test discovery with multiple peers
7. **Duplicate Prevention** - Test republishing behavior
8. **Reputation Tracking** - Test peer reputation system
9. **DHT Operations** - Test put/get operations
10. **Topic Subscription** - Test Gossipsub topic handling
11. **Version Updates** - Test multiple package versions

## Performance Considerations

### Scalability

- **DHT Lookups**: O(log N) where N is number of peers
- **Gossipsub Broadcast**: O(D) where D is gossip degree (typically 6-12)
- **Package Search**: Local cache first, then DHT (minimizes latency)

### Optimization Tips

1. **Local Caching**: Always check local cache before DHT queries
2. **Bootstrap Nodes**: Use geographically distributed bootstrap nodes
3. **DHT Server Mode**: Enable for well-connected, stable peers
4. **Reputation Filtering**: Skip low-reputation peers for retrievals
5. **Connection Limits**: Configure libp2p connection limits appropriately

```rust
// Example optimized configuration
let config = P2PConfig {
    bootstrap_nodes: load_bootstrap_nodes(),  // Multiple nodes
    dht_server_mode: is_stable_node(),         // Only if stable
    ..Default::default()
};
```

## Security Considerations

### Implemented

- ✅ Peer identity verification via libp2p Noise protocol
- ✅ Reputation tracking to identify malicious peers
- ✅ Content-addressed storage (tamper-proof)

### Recommended

- 🔒 Package signature verification before trust
- 🔒 Sybil attack mitigation (e.g., proof-of-work for publish)
- 🔒 Rate limiting for package announcements
- 🔒 Whitelist/blacklist for known good/bad peers
- 🔒 Eclipse attack prevention (diverse peer connections)

## Troubleshooting

### Common Issues

#### Cannot Connect to Bootstrap Nodes

```bash
# Check network connectivity
ping 104.131.131.82

# Verify multiaddr format
/ip4/104.131.131.82/tcp/4001/p2p/QmaCpDMGvV2BGHeYERUEnRQAwe3N8SzbUtfsmvsqQLuvuJ

# Try alternative bootstrap nodes
export GGEN_P2P_BOOTSTRAP="/dnsaddr/bootstrap.libp2p.io"
```

#### DHT Queries Timeout

- Increase query timeout in Kademlia config
- Ensure DHT has sufficient peers (min 20 recommended)
- Check firewall/NAT configuration

#### Package Not Found

- Package may not be propagated yet (wait 30-60s)
- Query multiple peers for redundancy
- Check local cache first
- Verify package ID format

### Debug Logging

```bash
# Enable detailed P2P logging
RUST_LOG=ggen_marketplace::backend::p2p=debug cargo run

# Enable libp2p internal logging
RUST_LOG=libp2p=debug cargo run
```

## Contributing

When contributing to P2P implementation:

1. **Follow London TDD**: Write tests first with mocks
2. **Document Network Behavior**: Explain DHT/Gossipsub interactions
3. **Performance Testing**: Benchmark with 100+ simulated peers
4. **Security Review**: Consider attack vectors
5. **Integration Tests**: Test with real libp2p network

## References

- [libp2p Documentation](https://docs.libp2p.io/)
- [Kademlia DHT Paper](https://pdos.csail.mit.edu/~petar/papers/maymounkov-kademlia-lncs.pdf)
- [Gossipsub Specification](https://github.com/libp2p/specs/blob/master/pubsub/gossipsub/README.md)
- [IPFS Specs](https://github.com/ipfs/specs)

## License

MIT OR Apache-2.0
