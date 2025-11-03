# P2P Quick Reference - ggen 2.4.0

**Version:** 2.4.0
**Format:** Cheat sheet for fast reference

## Installation

```bash
# Install with P2P support
cargo install ggen --version 2.4.0 --features p2p

# Verify installation
ggen --version  # Output: ggen 2.4.0
ggen marketplace p2p --help
```

---

## Common Commands

### Start P2P Node

```bash
# Basic start
ggen marketplace p2p start

# With bootstrap
ggen marketplace p2p start \
  --bootstrap /ip4/104.131.131.82/tcp/4001/p2p/QmaCpDMGvV2BGHeYERUEnRQAwe3N8SzbUtfsmvsqQLuvuJ

# Daemon mode
ggen marketplace p2p start --daemon

# Custom listen address
ggen marketplace p2p start --listen /ip4/0.0.0.0/tcp/7777
```

### Search Packages

```bash
# Basic search
ggen marketplace p2p search "rust cli"

# With filters
ggen marketplace p2p search "api" \
  --category rust \
  --tags async \
  --min-reputation 0.8 \
  --limit 10
```

### Publish Package

```bash
# Publish
ggen marketplace p2p publish ./my-package

# With version
ggen marketplace p2p publish ./my-package --version 1.0.0

# Skip verification
ggen marketplace p2p publish ./my-package --skip-verify
```

### Peer Management

```bash
# List peers
ggen marketplace p2p peer-list

# Detailed peers
ggen marketplace p2p peer-list --verbose

# JSON output
ggen marketplace p2p peer-list --format json

# Peer info
ggen marketplace p2p peer-info <peer-id>

# Bootstrap
ggen marketplace p2p bootstrap <node-address>

# Status
ggen marketplace p2p status
```

---

## Configuration

### Environment Variables

```bash
export GGEN_REGISTRY_URL="https://seanchatmangpt.github.io/ggen/registry/"
export GGEN_P2P_BOOTSTRAP="node1,node2,node3"
export GGEN_P2P_LISTEN="/ip4/0.0.0.0/tcp/7777"
export GGEN_P2P_DHT_SERVER="true"
export RUST_LOG="info"
```

### Config File (~/.ggen/config.toml)

```toml
[registry]
url = "https://seanchatmangpt.github.io/ggen/registry/"

[p2p]
bootstrap_nodes = [
  "/ip4/104.131.131.82/tcp/4001/p2p/QmaCpDMGvV2BGHeYERUEnRQAwe3N8SzbUtfsmvsqQLuvuJ"
]
listen_addresses = ["/ip4/0.0.0.0/tcp/0"]
dht_server_mode = true

[location]
latitude = 37.7749
longitude = -122.4194
region = "US-CA"

[cache]
max_size_mb = 100
ttl_minutes = 5
```

---

## Rust API

### Create P2P Registry

```rust
use ggen_marketplace::backend::p2p::{P2PConfig, P2PRegistry};

let config = P2PConfig {
    bootstrap_nodes: vec![
        "/ip4/104.131.131.82/tcp/4001/p2p/QmaCpDMGvV2BGHeYERUEnRQAwe3N8SzbUtfsmvsqQLuvuJ".parse()?
    ],
    dht_server_mode: true,
    ..Default::default()
};

let registry = P2PRegistry::new(config).await?;
registry.start_listening().await?;
registry.subscribe_to_packages().await?;
registry.bootstrap().await?;
```

### Search Packages

```rust
use ggen_marketplace::traits::Registry;
use ggen_marketplace::models::Query;

let query = Query {
    text: "rust cli".to_string(),
    categories: vec![],
    tags: vec![],
    limit: Some(10),
};

let results = registry.search(&query).await?;
```

### Geo-Proximity

```rust
use ggen_marketplace::backend::p2p::GeoLocation;

let location = GeoLocation {
    latitude: 37.7749,
    longitude: -122.4194,
    region: Some("US-CA".to_string()),
};

registry.set_location(location.clone()).await;

let peers = registry
    .select_best_peers(0.7, 10, Some(&location))
    .await;
```

### Parallel DHT Queries

```rust
// Automatic (uses parallel internally)
let package = registry.query_dht(&package_id).await?;

// Explicit fan-out
let package = registry
    .query_dht_parallel(&package_id, 3)
    .await?;
```

### Reputation

```rust
// Simple success rate (compatible)
let reputation = registry
    .get_peer_reputation(&peer_id)
    .await;

// Comprehensive score (v2.4.0)
let reputation = registry
    .get_comprehensive_reputation(&peer_id, Some(&location))
    .await;
```

---

## HTTP API

### Endpoints

```bash
# Server info
curl http://localhost:8080/

# List packages
curl http://localhost:8080/packages

# Package metadata
curl http://localhost:8080/packages/io.ggen.rust.cli

# Download info
curl http://localhost:8080/packages/io.ggen.rust.cli/info

# Download package
curl -O http://localhost:8080/packages/io.ggen.rust.cli/download
```

---

## Performance Targets

| Operation | Target | Actual |
|-----------|--------|--------|
| Node startup | <5s | ~3s |
| Package search | <2s | ~1.5s |
| DHT query (parallel) | <200ms | ~180ms |
| Cache hit rate | >80% | ~85% |
| Peer discovery | <3s | ~2s |

---

## Troubleshooting

### P2P Feature Not Enabled

```bash
cargo build --features p2p
cargo install --path . --features p2p
```

### Can't Connect to Bootstrap

```bash
# Try alternative nodes
ggen marketplace p2p start \
  --bootstrap /ip4/104.131.131.83/tcp/4001/p2p/QmYwAPJzv5CZsnA625s3Xf2nemtYgPpHdWEz79ojWnPbdG

# Or start without bootstrap
ggen marketplace p2p start
```

### No Peers Found

```bash
# Bootstrap manually
ggen marketplace p2p bootstrap <node-address>

# Wait for discovery
sleep 60

# Check again
ggen marketplace p2p peer-list
```

### Debug Mode

```bash
export RUST_LOG=debug
ggen marketplace p2p start
```

---

## Common Workflows

### Setup P2P Node

```bash
ggen marketplace p2p start \
  --bootstrap /ip4/104.131.131.82/tcp/4001/p2p/QmaCpDMGvV2BGHeYERUEnRQAwe3N8SzbUtfsmvsqQLuvuJ \
  --daemon

ggen marketplace p2p status
ggen marketplace p2p peer-list
```

### Search and Install

```bash
ggen marketplace p2p search "rust cli" --min-reputation 0.8
ggen marketplace install io.ggen.rust.cli-subcommand
ggen marketplace list
```

### Publish Package

```bash
mkdir my-package && cd my-package
cat > gpack.toml << 'EOF'
[gpack]
id = "io.example.my-package"
name = "My Package"
version = "1.0.0"
description = "Example package"
license = "MIT"
ggen_compat = ">=2.4.0"
EOF

ggen marketplace p2p publish .
ggen marketplace p2p search "my-package"
```

### Monitor Network

```bash
# Terminal 1
RUST_LOG=debug ggen marketplace p2p start --daemon

# Terminal 2
watch -n 5 'ggen marketplace p2p peer-list'

# Terminal 3
watch -n 10 'ggen marketplace p2p status'
```

---

## Bootstrap Nodes

### Public IPFS Bootstrap Nodes

```bash
/ip4/104.131.131.82/tcp/4001/p2p/QmaCpDMGvV2BGHeYERUEnRQAwe3N8SzbUtfsmvsqQLuvuJ
/ip4/104.131.131.83/tcp/4001/p2p/QmYwAPJzv5CZsnA625s3Xf2nemtYgPpHdWEz79ojWnPbdG
/ip4/104.131.131.84/tcp/4001/p2p/QmSoLV4Bbm51jM9C4gDYZQ9Cy3U6aXMJDAbzgu2fzaDs64
```

---

## Key Concepts

### Reputation Scoring

```
Score = 0.50 × success_rate +
        0.25 × response_time_score +
        0.15 × availability_score +
        0.10 × recency_score +
        geo_bonus (up to +10%)
```

### Geo-Proximity Bonus

```
if distance < 100km:
    bonus = 0.1 × (1.0 - distance/100.0)
```

### Cache Layers

1. **Hot cache** - 5-minute TTL, frequently accessed packages
2. **Local store** - Permanently cached packages
3. **DHT** - Distributed hash table queries

### Parallel Queries

- **Fan-out:** Query 3 peers concurrently
- **First response:** Use fastest peer's result
- **Fallback:** Try other peers on failure

---

## Links

- **Docs:** https://github.com/seanchatmangpt/ggen/tree/main/docs
- **API Reference:** [API_REFERENCE_V2.4.0.md](API_REFERENCE_V2.4.0.md)
- **CLI Reference:** [CLI_REFERENCE_V2.4.0.md](CLI_REFERENCE_V2.4.0.md)
- **Migration Guide:** [MIGRATION_GUIDE_V2.4.0.md](MIGRATION_GUIDE_V2.4.0.md)
- **Issues:** https://github.com/seanchatmangpt/ggen/issues

---

**Last Updated:** 2025-11-02
**Version:** 2.4.0
**License:** MIT
