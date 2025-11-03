# P2P Marketplace Usage Examples

This document provides practical examples for using the P2P marketplace commands in ggen 2.4.0.

## Prerequisites

Build ggen with P2P support:
```bash
cargo build --release --features p2p
```

## Quick Start

### 1. Start a P2P Node

Start a basic P2P node:
```bash
ggen marketplace p2p start
```

Start with custom listen address:
```bash
ggen marketplace p2p start --listen "/ip4/0.0.0.0/tcp/4001"
```

Start in daemon mode (background):
```bash
ggen marketplace p2p start --daemon
```

### 2. Connect to Existing Network

Bootstrap with known peers:
```bash
ggen marketplace p2p start \
  --bootstrap "/ip4/104.131.131.82/tcp/4001/p2p/QmaCpDMGvV2BGHeYERUEnRQAwe3N8SzbUtfsmvsqQLuvuJ" \
  --bootstrap "/ip4/104.236.179.241/tcp/4001/p2p/QmSoLV4Bbm51jM9C4gDYZQ9Cy3U6aXMJDAbzgu2fzaDs64"
```

### 3. Publish a Package

Publish a package to the P2P network:
```bash
ggen marketplace p2p publish ./my-package
```

Publish with specific version:
```bash
ggen marketplace p2p publish ./my-package --version "1.2.3"
```

Skip verification checks (not recommended):
```bash
ggen marketplace p2p publish ./my-package --skip-verify
```

### 4. Search for Packages

Basic search:
```bash
ggen marketplace p2p search "rust template"
```

Search with filters:
```bash
ggen marketplace p2p search "react" \
  --category "web" \
  --tags "typescript" \
  --tags "frontend" \
  --limit 10
```

Search with reputation filter:
```bash
ggen marketplace p2p search "cli tool" --min-reputation 0.8
```

### 5. List Connected Peers

List all peers:
```bash
ggen marketplace p2p peer-list
```

List with detailed information:
```bash
ggen marketplace p2p peer-list --verbose
```

Filter by minimum reputation:
```bash
ggen marketplace p2p peer-list --min-reputation 0.7
```

Output as JSON:
```bash
ggen marketplace p2p peer-list --format json
```

Output as YAML:
```bash
ggen marketplace p2p peer-list --format yaml
```

### 6. Get Peer Information

Get detailed info about a specific peer:
```bash
ggen marketplace p2p peer-info "QmaCpDMGvV2BGHeYERUEnRQAwe3N8SzbUtfsmvsqQLuvuJ"
```

Get full peer history:
```bash
ggen marketplace p2p peer-info "QmaCpDMGvV2BGHeYERUEnRQAwe3N8SzbUtfsmvsqQLuvuJ" --full
```

### 7. Bootstrap DHT

Bootstrap DHT with multiple nodes:
```bash
ggen marketplace p2p bootstrap \
  "/ip4/104.131.131.82/tcp/4001/p2p/QmaCpDMGvV2BGHeYERUEnRQAwe3N8SzbUtfsmvsqQLuvuJ" \
  "/ip4/104.236.179.241/tcp/4001/p2p/QmSoLV4Bbm51jM9C4gDYZQ9Cy3U6aXMJDAbzgu2fzaDs64"
```

Bootstrap with timeout:
```bash
ggen marketplace p2p bootstrap \
  "/ip4/104.131.131.82/tcp/4001/p2p/QmaCpDMGvV2BGHeYERUEnRQAwe3N8SzbUtfsmvsqQLuvuJ" \
  --timeout 60
```

### 8. Check Node Status

Check local node status:
```bash
ggen marketplace p2p status
```

## Advanced Usage Scenarios

### Running a Long-Lived Node

Start a node as a daemon and keep it running:
```bash
# Start node in background
ggen marketplace p2p start --daemon \
  --listen "/ip4/0.0.0.0/tcp/4001" \
  --bootstrap "/ip4/104.131.131.82/tcp/4001/p2p/QmaCpDMGvV2BGHeYERUEnRQAwe3N8SzbUtfsmvsqQLuvuJ" \
  --dht-server true
```

### Publishing Multiple Packages

```bash
# Publish a collection of packages
for dir in ./packages/*; do
  echo "Publishing $dir..."
  ggen marketplace p2p publish "$dir"
done
```

### Searching and Installing

```bash
# Search for packages
ggen marketplace p2p search "rust cli" --format json > results.json

# Parse results and install (requires jq)
jq -r '.packages[].id' results.json | while read pkg_id; do
  echo "Found package: $pkg_id"
  # Install logic here
done
```

### Monitoring Peer Reputation

```bash
# Monitor high-reputation peers
ggen marketplace p2p peer-list \
  --min-reputation 0.9 \
  --format json | jq '.peers[] | {peer_id, reputation, packages_provided}'
```

### Setting Up a Private Network

```bash
# Node 1 (bootstrap node)
ggen marketplace p2p start \
  --listen "/ip4/0.0.0.0/tcp/4001" \
  --dht-server true \
  --daemon

# Note the peer ID from output: QmYourPeerID123...

# Node 2 (connect to node 1)
ggen marketplace p2p start \
  --listen "/ip4/0.0.0.0/tcp/4002" \
  --bootstrap "/ip4/127.0.0.1/tcp/4001/p2p/QmYourPeerID123..." \
  --daemon
```

## Configuration File

Create a config file `~/.ggen/p2p-config.toml`:

```toml
[p2p]
listen_addresses = [
  "/ip4/0.0.0.0/tcp/4001",
  "/ip6/::/tcp/4001"
]

bootstrap_nodes = [
  "/ip4/104.131.131.82/tcp/4001/p2p/QmaCpDMGvV2BGHeYERUEnRQAwe3N8SzbUtfsmvsqQLuvuJ",
  "/ip4/104.236.179.241/tcp/4001/p2p/QmSoLV4Bbm51jM9C4gDYZQ9Cy3U6aXMJDAbzgu2fzaDs64"
]

dht_server_mode = true

[search]
min_reputation = 0.5
default_limit = 20
```

Use the config:
```bash
ggen marketplace p2p start --config ~/.ggen/p2p-config.toml
```

## Troubleshooting

### Feature Not Enabled Error

If you see:
```
Error: Feature 'p2p' not enabled
```

Rebuild with P2P support:
```bash
cargo build --release --features p2p
```

### Connection Issues

Check node status:
```bash
ggen marketplace p2p status
```

Verify bootstrap nodes are reachable:
```bash
ping 104.131.131.82
```

### Peer Discovery Problems

Try bootstrapping manually:
```bash
ggen marketplace p2p bootstrap \
  "/ip4/104.131.131.82/tcp/4001/p2p/QmaCpDMGvV2BGHeYERUEnRQAwe3N8SzbUtfsmvsqQLuvuJ"
```

## Performance Tips

1. **Enable DHT Server Mode**: For better network participation
   ```bash
   ggen marketplace p2p start --dht-server true
   ```

2. **Use Multiple Bootstrap Nodes**: Improves network resilience
   ```bash
   ggen marketplace p2p start \
     --bootstrap "node1" \
     --bootstrap "node2" \
     --bootstrap "node3"
   ```

3. **Filter by Reputation**: Avoid low-quality peers
   ```bash
   ggen marketplace p2p search "package" --min-reputation 0.8
   ```

4. **JSON Output for Automation**: Use structured output
   ```bash
   ggen marketplace p2p peer-list --format json | jq '.peers'
   ```

## See Also

- [P2P Architecture](./P2P_CLI_ARCHITECTURE.md)
- [Marketplace Documentation](./MARKETPLACE-ARCHITECTURE-INDEX.md)
- [P2P Backend Implementation](../ggen-marketplace/src/backend/p2p.rs)
