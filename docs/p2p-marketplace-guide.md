# P2P Marketplace Guide

## Overview

The ggen P2P marketplace provides decentralized package distribution using libp2p technology. This enables:

- **Decentralized Discovery**: Find packages without a central registry
- **Peer-to-Peer Distribution**: Download packages directly from peers
- **Content Addressing**: Verify package integrity cryptographically
- **Reputation System**: Track peer reliability

## Architecture

### Core Components

1. **P2P Registry** (`ggen-marketplace/src/backend/p2p.rs`)
   - Implements `Registry` trait for decentralized operations
   - Uses Kademlia DHT for distributed storage
   - Uses Gossipsub for package announcements
   - Tracks peer reputation

2. **CLI Commands** (`cli/src/domain/marketplace/p2p.rs`)
   - User-facing commands for P2P operations
   - Integrates with clap-noun-verb structure
   - Manages node lifecycle

3. **State Management** (`cli/src/domain/marketplace/p2p_state.rs`)
   - Global P2P node singleton
   - Configuration persistence
   - Thread-safe access

### Network Protocol

```
┌─────────────────┐
│   Kademlia DHT  │ ← Distributed package metadata storage
├─────────────────┤
│   Gossipsub     │ ← Package announcements and updates
├─────────────────┤
│   Identify      │ ← Peer identification and versioning
└─────────────────┘
```

## Usage

### Starting a P2P Node

```bash
# Start with default settings
ggen marketplace p2p start

# Start with custom listen address
ggen marketplace p2p start --listen /ip4/0.0.0.0/tcp/4001

# Start with bootstrap nodes
ggen marketplace p2p start \
  --bootstrap /ip4/104.131.131.82/tcp/4001/p2p/QmaCpDMGvV2BGHeYERUEnRQAwe3N8SzbUtfsmvsqQLuvuJ \
  --bootstrap /dnsaddr/bootstrap.libp2p.io

# Run in daemon mode (background)
ggen marketplace p2p start --daemon

# Use custom configuration file
ggen marketplace p2p start --config ~/.ggen/p2p-config.toml
```

### Publishing Packages

```bash
# Publish a package to the P2P network
ggen marketplace p2p publish ./my-package

# Publish with explicit version
ggen marketplace p2p publish ./my-package --version 1.2.3

# Skip verification checks
ggen marketplace p2p publish ./my-package --skip-verify
```

**Package Requirements:**
- Must be a directory containing `gpack.toml`
- `gpack.toml` must specify `name`, `version`, and `description`

### Searching Packages

```bash
# Basic search
ggen marketplace p2p search "database"

# Search with category filter
ggen marketplace p2p search "database" --category middleware

# Search with tags
ggen marketplace p2p search "auth" --tags security --tags oauth

# Limit results
ggen marketplace p2p search "web" --limit 10

# Filter by minimum peer reputation
ggen marketplace p2p search "crypto" --min-reputation 0.8
```

### Managing Peers

```bash
# List all connected peers
ggen marketplace p2p peer-list

# List peers with verbose information
ggen marketplace p2p peer-list --verbose

# Filter by reputation
ggen marketplace p2p peer-list --min-reputation 0.7

# Output as JSON
ggen marketplace p2p peer-list --format json

# Get information about specific peer
ggen marketplace p2p peer-info QmaCpDMGvV2BGHeYERUEnRQAwe3N8SzbUtfsmvsqQLuvuJ

# Get full peer history
ggen marketplace p2p peer-info QmaCpDMGvV2BGHeYERUEnRQAwe3N8SzbUtfsmvsqQLuvuJ --full
```

### Bootstrapping

```bash
# Bootstrap DHT with known peers
ggen marketplace p2p bootstrap \
  /ip4/104.131.131.82/tcp/4001/p2p/QmaCpDMGvV2BGHeYERUEnRQAwe3N8SzbUtfsmvsqQLuvuJ \
  /dnsaddr/bootstrap.libp2p.io

# Bootstrap with custom timeout
ggen marketplace p2p bootstrap <nodes...> --timeout 60
```

### Node Status

```bash
# Check P2P node status
ggen marketplace p2p status
```

## Configuration

### P2P Configuration File

Create `~/.ggen/p2p-config.toml`:

```toml
# Listen addresses
listen_addresses = [
    "/ip4/0.0.0.0/tcp/4001",
    "/ip6/::/tcp/4001"
]

# Bootstrap nodes for initial peer discovery
bootstrap_nodes = [
    "/ip4/104.131.131.82/tcp/4001/p2p/QmaCpDMGvV2BGHeYERUEnRQAwe3N8SzbUtfsmvsqQLuvuJ",
    "/dnsaddr/bootstrap.libp2p.io"
]

# Enable DHT server mode
dht_server_mode = true

# Gossipsub topic for package announcements
packages_topic = "/ggen/packages/v1"
```

## Reputation System

The P2P marketplace tracks peer reputation based on:

- **Successful Retrievals**: Package downloads that complete successfully
- **Failed Retrievals**: Package downloads that fail or timeout
- **Last Seen**: When the peer was last active
- **Packages Provided**: Number of unique packages offered

### Reputation Score

```
reputation = successful_retrievals / (successful_retrievals + failed_retrievals)
```

- New peers start with reputation 1.0 (optimistic)
- Reputation ranges from 0.0 (unreliable) to 1.0 (perfect)
- Use `--min-reputation` to filter search results

## Security Considerations

### Package Verification

All packages should be:
1. **Content-addressed**: SHA-256 hash verification
2. **Signed**: Cryptographic signature by author
3. **Validated**: Metadata and dependencies checked

### Network Security

- All connections use libp2p's encrypted transports (Noise protocol)
- Peer IDs are derived from public keys
- DHT uses secure routing protocols

### Best Practices

1. **Bootstrap from trusted nodes**: Use well-known bootstrap peers
2. **Verify signatures**: Always check package signatures before installation
3. **Monitor reputation**: Filter low-reputation peers
4. **Regular updates**: Keep bootstrap nodes list current
5. **Firewall rules**: Configure appropriate port forwarding

## Troubleshooting

### Node Won't Start

```bash
# Check if port is already in use
lsof -i :4001

# Try different listen address
ggen marketplace p2p start --listen /ip4/0.0.0.0/tcp/0
```

### Can't Find Packages

```bash
# Ensure node is bootstrapped
ggen marketplace p2p bootstrap <bootstrap-nodes>

# Check peer connections
ggen marketplace p2p peer-list

# Increase search limit
ggen marketplace p2p search "query" --limit 50
```

### Poor Performance

```bash
# Filter by high-reputation peers
ggen marketplace p2p search "query" --min-reputation 0.8

# Connect to more bootstrap nodes
ggen marketplace p2p start --bootstrap <multiple-nodes>
```

## Advanced Topics

### Running a Bootstrap Node

```bash
# Start in DHT server mode with fixed address
ggen marketplace p2p start \
  --listen /ip4/0.0.0.0/tcp/4001 \
  --dht-server \
  --daemon

# Announce your peer ID for others to use
ggen marketplace p2p status
```

### Custom Network Topology

The P2P marketplace supports different network topologies:

- **Mesh**: Full peer-to-peer, all nodes equal
- **Hub-and-spoke**: Central bootstrap nodes with edge peers
- **Hybrid**: Mix of permanent and ephemeral nodes

Configure via `p2p-config.toml` or command-line flags.

## API Integration

### Using P2P Registry Programmatically

```rust
use ggen_marketplace::backend::p2p::{P2PConfig, P2PRegistry};
use ggen_marketplace::traits::Registry;

#[tokio::main]
async fn main() -> Result<()> {
    // Create config
    let config = P2PConfig::default();

    // Initialize registry
    let registry = P2PRegistry::new(config).await?;

    // Start listening
    registry.start_listening().await?;
    registry.subscribe_to_packages().await?;

    // Publish package
    registry.publish(my_package).await?;

    // Search packages
    let results = registry.search(&query).await?;

    Ok(())
}
```

## Future Enhancements

- [ ] IPFS integration for content storage
- [ ] Distributed reputation consensus
- [ ] Bandwidth accounting and incentives
- [ ] NAT traversal improvements
- [ ] Mobile node support
- [ ] Cross-shard package discovery

## References

- [libp2p Documentation](https://docs.libp2p.io/)
- [Kademlia DHT Spec](https://pdos.csail.mit.edu/~petar/papers/maymounkov-kademlia-lncs.pdf)
- [Gossipsub Spec](https://github.com/libp2p/specs/blob/master/pubsub/gossipsub/gossipsub-v1.0.md)
