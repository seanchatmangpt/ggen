# P2P Marketplace Example

This example demonstrates how to use the P2P marketplace registry for decentralized package discovery and distribution.

## Features

- **Decentralized**: No central server required
- **Real-time**: Package updates propagate via gossipsub
- **Resilient**: DHT-based discovery works even with node failures
- **Scalable**: Designed to handle thousands of nodes

## Usage

### Start a Node

```bash
# Start with default settings
cargo run --example p2p-marketplace -- start

# Start with custom listen address
cargo run --example p2p-marketplace -- start --listen "/ip4/0.0.0.0/tcp/4001"

# Start with bootstrap nodes
cargo run --example p2p-marketplace -- start \
    --bootstrap "12D3KooWPeer1@/ip4/104.131.131.82/tcp/4001" \
    --bootstrap "12D3KooWPeer2@/ip4/104.131.131.83/tcp/4001"
```

### Publish a Package

```bash
# Basic publish
cargo run --example p2p-marketplace -- publish \
    --name "rust-web-framework" \
    --version "1.0.0"

# Publish with metadata
cargo run --example p2p-marketplace -- publish \
    --name "rust-web-framework" \
    --version "1.0.0" \
    --category "web" \
    --tags "rust,web,framework"
```

### Search for Packages

```bash
# Basic search
cargo run --example p2p-marketplace -- search rust web

# Search with filters
cargo run --example p2p-marketplace -- search rust \
    --category "web" \
    --tags "framework" \
    --limit 10
```

### Get Package Details

```bash
cargo run --example p2p-marketplace -- get "rust-web-framework@1.0.0"
```

### Show Network Statistics

```bash
cargo run --example p2p-marketplace -- stats
```

## Multi-Node Testing

### Terminal 1 - Bootstrap Node

```bash
cargo run --example p2p-marketplace -- start --listen "/ip4/0.0.0.0/tcp/4001"
# Note the peer ID from output
```

### Terminal 2 - Publishing Node

```bash
cargo run --example p2p-marketplace -- start \
    --listen "/ip4/0.0.0.0/tcp/4002" \
    --bootstrap "<peer-id-from-terminal-1>@/ip4/127.0.0.1/tcp/4001"

# In another terminal, publish a package
cargo run --example p2p-marketplace -- publish \
    --name "test-package" \
    --version "1.0.0"
```

### Terminal 3 - Consumer Node

```bash
cargo run --example p2p-marketplace -- start \
    --listen "/ip4/0.0.0.0/tcp/4003" \
    --bootstrap "<peer-id-from-terminal-1>@/ip4/127.0.0.1/tcp/4001"

# In another terminal, search for packages
cargo run --example p2p-marketplace -- search test
```

## Architecture

```
┌──────────────┐      ┌──────────────┐      ┌──────────────┐
│  Publisher   │      │  Bootstrap   │      │   Consumer   │
│    Node      │◄────►│     Node     │◄────►│     Node     │
└──────────────┘      └──────────────┘      └──────────────┘
       │                      │                      │
       └──────────────────────┴──────────────────────┘
                         DHT Network
```

## Configuration

Default configuration can be customized via environment variables:

```bash
# Network settings
export GGEN_P2P_LISTEN="/ip4/0.0.0.0/tcp/4001"
export GGEN_P2P_MAX_CONNECTIONS="100"

# DHT settings
export GGEN_DHT_REPLICATION="20"
export GGEN_DHT_QUERY_TIMEOUT="60"

# Gossipsub settings
export GGEN_GOSSIP_MESH_SIZE="6"
```

## Troubleshooting

### "No peers found"

1. Ensure bootstrap nodes are accessible
2. Check firewall rules
3. Enable mDNS for local discovery
4. Verify listen address is not blocked

### "Package not found"

1. Wait 5-10 seconds for DHT propagation
2. Check package was successfully published
3. Verify bootstrap nodes are connected
4. Try searching with broader keywords

### "Connection timeout"

1. Increase connection timeout in config
2. Check network connectivity
3. Verify bootstrap node addresses
4. Try different transport protocol (TCP/QUIC)

## Production Considerations

### Bootstrap Nodes

For production, run dedicated bootstrap nodes:

```bash
# Run multiple bootstrap nodes for redundancy
cargo run --example p2p-marketplace -- start \
    --listen "/ip4/0.0.0.0/tcp/4001"

cargo run --example p2p-marketplace -- start \
    --listen "/ip4/0.0.0.0/tcp/4002" \
    --bootstrap "<first-node>@/ip4/<ip>/tcp/4001"
```

### Monitoring

Monitor network health:

```bash
# Check stats regularly
watch -n 5 'cargo run --example p2p-marketplace -- stats'
```

### Security

1. Use persistent identities for production nodes
2. Enable Noise protocol encryption (default)
3. Implement rate limiting
4. Monitor for malicious peers

## Next Steps

1. **Integration**: Integrate P2P registry with ggen CLI
2. **Content Storage**: Add IPFS integration for package content
3. **Reputation**: Implement peer reputation system
4. **Discovery**: Enhance peer discovery mechanisms
5. **Performance**: Add caching and optimization

## Resources

- [P2P Registry Documentation](../../docs/p2p-registry.md)
- [libp2p Documentation](https://docs.libp2p.io/)
- [Ggen Marketplace Guide](../../docs/marketplace.md)
