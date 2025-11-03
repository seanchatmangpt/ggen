# ggen 2.4.0 API Reference

**Version:** 2.4.0
**Release Date:** 2025-11-02
**Status:** Stable

## Table of Contents

- [Overview](#overview)
- [P2P Marketplace API](#p2p-marketplace-api)
  - [CLI Commands](#cli-commands)
  - [Rust API](#rust-api)
  - [HTTP API](#http-api)
- [Registry API](#registry-api)
- [Configuration](#configuration)
- [Examples](#examples)
- [Migration from 2.3.0](#migration-from-230)

---

## Overview

ggen 2.4.0 introduces a comprehensive P2P marketplace with decentralized package discovery, adaptive reputation system, and geo-proximity-aware routing. This API reference covers:

1. **P2P Marketplace** - Decentralized package distribution using libp2p
2. **Registry API** - Package discovery and management
3. **HTTP Server** - Content distribution endpoints
4. **Reputation System** - Adaptive peer selection

---

## P2P Marketplace API

### CLI Commands

#### `ggen marketplace p2p start`

Start a P2P node and connect to the network.

**Usage:**
```bash
ggen marketplace p2p start [OPTIONS]
```

**Options:**
- `-l, --listen <ADDRESS>` - Listen address (default: `/ip4/0.0.0.0/tcp/0`)
- `-b, --bootstrap <NODES>...` - Bootstrap nodes (can be repeated)
- `--dht-server <BOOL>` - Enable DHT server mode (default: `true`)
- `-d, --daemon` - Run in background daemon mode
- `-c, --config <PATH>` - Configuration file path

**Examples:**
```bash
# Start with default settings
ggen marketplace p2p start

# Start with bootstrap nodes
ggen marketplace p2p start \
  --bootstrap /ip4/104.131.131.82/tcp/4001/p2p/QmaCpDMGvV2BGHeYERUEnRQAwe3N8SzbUtfsmvsqQLuvuJ

# Start in daemon mode
ggen marketplace p2p start --daemon

# Start with custom listen address
ggen marketplace p2p start --listen /ip4/0.0.0.0/tcp/7777
```

**Response:**
```
🚀 Starting P2P node...
✅ P2P node started successfully
📡 Listening for package announcements...
Peer ID: 12D3KooWPjceQrSwdWXPyLLeABRXmuqt69Rg3sBYbU1Nft9HyQ6X
Listen addresses:
  - /ip4/127.0.0.1/tcp/54321/p2p/12D3KooW...
  - /ip4/192.168.1.100/tcp/54321/p2p/12D3KooW...
```

**Performance:**
- Startup time: <5s (with bootstrap)
- DHT connection: <3s
- Memory usage: ~50MB base

---

#### `ggen marketplace p2p publish`

Publish a package to the P2P network.

**Usage:**
```bash
ggen marketplace p2p publish <PATH> [OPTIONS]
```

**Arguments:**
- `<PATH>` - Package directory containing `gpack.toml`

**Options:**
- `-v, --version <VERSION>` - Package version (if not in manifest)
- `--skip-verify` - Skip verification checks

**Examples:**
```bash
# Publish package
ggen marketplace p2p publish ./my-package

# Publish with explicit version
ggen marketplace p2p publish ./my-package --version 1.0.0

# Skip verification (development only)
ggen marketplace p2p publish ./my-package --skip-verify
```

**Response:**
```
📦 Publishing package to P2P network...
✓ Package validation passed
✅ Package 'my-package@1.0.0' ready for publishing
📡 Would announce to network peers
💾 Would store in DHT
```

**Requirements:**
- Package directory must contain `gpack.toml`
- Valid semantic version (major.minor.patch)
- P2P node must be running

---

#### `ggen marketplace p2p search`

Search for packages on the P2P network.

**Usage:**
```bash
ggen marketplace p2p search <QUERY> [OPTIONS]
```

**Arguments:**
- `<QUERY>` - Search query text

**Options:**
- `-c, --category <CATEGORY>` - Filter by category
- `-t, --tags <TAGS>...` - Filter by tags (can be repeated)
- `-l, --limit <NUM>` - Maximum results (default: `20`)
- `--min-reputation <SCORE>` - Minimum peer reputation 0.0-1.0 (default: `0.5`)

**Examples:**
```bash
# Basic search
ggen marketplace p2p search "rust cli"

# Search with category filter
ggen marketplace p2p search "web framework" --category rust

# Search with tags
ggen marketplace p2p search "api" --tags rest --tags async

# Limit results
ggen marketplace p2p search "template" --limit 10

# High-reputation peers only
ggen marketplace p2p search "database" --min-reputation 0.8
```

**Response:**
```
🔍 Searching P2P network for 'rust cli'...

📊 Found 3 packages

io.ggen.rust.cli-subcommand (v0.1.0)
  Description: Generate clap subcommands
  Providers: 5 peers
  Reputation: 0.92

io.ggen.rust.cli-parser (v0.2.0)
  Description: CLI argument parsing templates
  Providers: 3 peers
  Reputation: 0.87
```

**Performance:**
- Query time: <2s (DHT + gossipsub)
- Parallel fan-out: 3 peers by default
- Average lookup: <200ms with parallel queries

---

#### `ggen marketplace p2p peer-list`

List connected peers with reputation information.

**Usage:**
```bash
ggen marketplace p2p peer-list [OPTIONS]
```

**Options:**
- `-v, --verbose` - Show detailed information
- `--min-reputation <SCORE>` - Filter by minimum reputation
- `-f, --format <FORMAT>` - Output format: `table`, `json`, `yaml` (default: `table`)

**Examples:**
```bash
# List all peers (table format)
ggen marketplace p2p peer-list

# Detailed information
ggen marketplace p2p peer-list --verbose

# Filter by reputation
ggen marketplace p2p peer-list --min-reputation 0.7

# JSON output
ggen marketplace p2p peer-list --format json

# YAML output
ggen marketplace p2p peer-list --format yaml
```

**Response (Table):**
```
👥 Connected Peers:

Peer ID                                              Reputation   Packages
----------------------------------------------------------------------------
12D3KooWPjceQrSwdWXPyLLeABRXmuqt69Rg3sBYbU1Nft9HyQ6        0.92         12
12D3KooWDpJ7As7BWAwRMfu1VU2WCqNjvq387JEYKDBj4kx6nXTN        0.87          8

Total: 2 peer(s)
```

**Response (JSON):**
```json
{
  "peers": [
    {
      "peer_id": "12D3KooWPjceQrSwdWXPyLLeABRXmuqt69Rg3sBYbU1Nft9HyQ6X",
      "addresses": ["/ip4/104.131.131.82/tcp/4001"],
      "reputation": 0.92,
      "successful_retrievals": 45,
      "failed_retrievals": 2,
      "last_seen": "2025-11-02T10:30:00Z",
      "packages_provided": 12
    }
  ],
  "total": 1
}
```

---

#### `ggen marketplace p2p peer-info`

Get detailed information about a specific peer.

**Usage:**
```bash
ggen marketplace p2p peer-info <PEER_ID> [OPTIONS]
```

**Arguments:**
- `<PEER_ID>` - Peer ID to query

**Options:**
- `--full` - Show full details including history

**Examples:**
```bash
# Basic peer info
ggen marketplace p2p peer-info 12D3KooWPjceQrSwdWXPyLLeABRXmuqt69Rg3sBYbU1Nft9HyQ6X

# Full details
ggen marketplace p2p peer-info 12D3KooWPjceQrSwdWXPyLLeABRXmuqt69Rg3sBYbU1Nft9HyQ6X --full
```

**Response:**
```
ℹ️  Peer Information: 12D3KooWPjceQrSwdWXPyLLeABRXmuqt69Rg3sBYbU1Nft9HyQ6X

  Reputation: 0.92
  Status: Active

Detailed Information:
  Full Peer ID: 12D3KooWPjceQrSwdWXPyLLeABRXmuqt69Rg3sBYbU1Nft9HyQ6X
  Reputation Score: 0.9235
```

---

#### `ggen marketplace p2p bootstrap`

Bootstrap DHT with known peers.

**Usage:**
```bash
ggen marketplace p2p bootstrap <NODES>... [OPTIONS]
```

**Arguments:**
- `<NODES>...` - Bootstrap node addresses (multiaddr format)

**Options:**
- `-t, --timeout <SECONDS>` - Timeout in seconds (default: `30`)

**Examples:**
```bash
# Bootstrap with single node
ggen marketplace p2p bootstrap /ip4/104.131.131.82/tcp/4001/p2p/QmaCpDMGvV2BGHeYERUEnRQAwe3N8SzbUtfsmvsqQLuvuJ

# Bootstrap with multiple nodes
ggen marketplace p2p bootstrap \
  /ip4/104.131.131.82/tcp/4001/p2p/QmaCpDMGvV2BGHeYERUEnRQAwe3N8SzbUtfsmvsqQLuvuJ \
  /ip4/104.131.131.83/tcp/4001/p2p/QmYwAPJzv5CZsnA625s3Xf2nemtYgPpHdWEz79ojWnPbdG

# With custom timeout
ggen marketplace p2p bootstrap <node-address> --timeout 60
```

**Response:**
```
🔗 Bootstrapping DHT with 2 nodes...
✅ DHT bootstrap complete
```

---

#### `ggen marketplace p2p status`

Get local P2P node status and information.

**Usage:**
```bash
ggen marketplace p2p status
```

**Response (Running):**
```
📊 P2P Node Status:

  Status: ✅ Running
  Registry: Ggen P2P Registry
  Description: Decentralized P2P package registry using libp2p
  URL: /p2p/12D3KooWPjceQrSwdWXPyLLeABRXmuqt69Rg3sBYbU1Nft9HyQ6X
  Publishing: Enabled
  Authentication: Not required

Capabilities:
  • Decentralized package discovery
  • DHT-based metadata storage
  • Peer reputation tracking
  • Gossipsub package announcements
```

**Response (Not Running):**
```
📊 P2P Node Status:

  Status: ⭕ Not running

Start the P2P node with:
  ggen marketplace p2p start

Or with bootstrap nodes:
  ggen marketplace p2p start --bootstrap <node-multiaddr>
```

---

### Rust API

#### P2PRegistry

The core P2P registry implementation using libp2p.

**Module:** `ggen_marketplace::backend::p2p`

**Basic Usage:**
```rust
use ggen_marketplace::backend::p2p::{P2PConfig, P2PRegistry};

#[tokio::main]
async fn main() -> Result<(), Box<dyn std::error::Error>> {
    // Create configuration
    let config = P2PConfig {
        bootstrap_nodes: vec![
            "/ip4/104.131.131.82/tcp/4001/p2p/QmaCpDMGvV2BGHeYERUEnRQAwe3N8SzbUtfsmvsqQLuvuJ".parse()?
        ],
        dht_server_mode: true,
        ..Default::default()
    };

    // Create P2P registry
    let registry = P2PRegistry::new(config).await?;

    // Start listening
    registry.start_listening().await?;

    // Subscribe to package announcements
    registry.subscribe_to_packages().await?;

    // Bootstrap DHT
    registry.bootstrap().await?;

    Ok(())
}
```

#### P2PConfig

Configuration for P2P registry.

**Fields:**
```rust
pub struct P2PConfig {
    /// Bootstrap nodes for initial peer discovery
    pub bootstrap_nodes: Vec<Multiaddr>,

    /// Gossipsub topic for package announcements
    pub packages_topic: String,

    /// Enable DHT server mode
    pub dht_server_mode: bool,

    /// Local listen addresses
    pub listen_addresses: Vec<Multiaddr>,
}
```

**Default Configuration:**
```rust
let config = P2PConfig::default();
// packages_topic: "/ggen/packages/v1"
// dht_server_mode: true
// listen_addresses: ["/ip4/0.0.0.0/tcp/0"]
```

#### GeoLocation (v2.4.0)

Geographic location for proximity-aware routing.

**Usage:**
```rust
use ggen_marketplace::backend::p2p::GeoLocation;

// Create location
let location = GeoLocation {
    latitude: 37.7749,  // San Francisco
    longitude: -122.4194,
    region: Some("US-CA".to_string()),
};

// Calculate distance
let other = GeoLocation {
    latitude: 40.7128,  // New York
    longitude: -74.0060,
    region: Some("US-NY".to_string()),
};

let distance_km = location.distance_km(&other);
println!("Distance: {:.2} km", distance_km);
// Output: Distance: 4135.23 km
```

**Distance Calculation:**
Uses Haversine formula for great-circle distance on a sphere:
```
a = sin²(Δφ/2) + cos φ1 ⋅ cos φ2 ⋅ sin²(Δλ/2)
c = 2 ⋅ atan2(√a, √(1−a))
d = R ⋅ c
```
where φ is latitude, λ is longitude, R is earth's radius (6371 km)

#### Registry Trait

Core trait for package discovery and management.

**Trait Definition:**
```rust
#[async_trait]
pub trait Registry: Send + Sync {
    /// Search for packages matching the query
    async fn search(&self, query: &Query) -> Result<Vec<Package>>;

    /// Retrieve a specific package by ID
    async fn get_package(&self, id: &PackageId) -> Result<Package>;

    /// Get a specific version of a package
    async fn get_package_version(&self, id: &PackageId, version: &str) -> Result<Package>;

    /// List all versions of a package
    async fn list_versions(&self, id: &PackageId) -> Result<Vec<Package>>;

    /// Publish a new package or version
    async fn publish(&self, package: Package) -> Result<()>;

    /// Delete a package version (if allowed)
    async fn delete(&self, id: &PackageId, version: &str) -> Result<()>;

    /// Check if a package exists
    async fn exists(&self, id: &PackageId) -> Result<bool>;

    /// Get registry metadata and capabilities
    async fn metadata(&self) -> Result<RegistryMetadata>;
}
```

**Usage:**
```rust
use ggen_marketplace::traits::Registry;
use ggen_marketplace::models::{Query, PackageId};

async fn search_packages(registry: &impl Registry) -> Result<()> {
    // Search
    let query = Query {
        text: "rust cli".to_string(),
        categories: vec![],
        tags: vec![],
        limit: Some(10),
    };
    let results = registry.search(&query).await?;

    // Get package
    let package_id = PackageId::new("io.ggen.rust.cli");
    let package = registry.get_package(&package_id).await?;

    Ok(())
}
```

#### Adaptive Reputation System (v2.4.0)

Multi-factor reputation scoring for peer selection.

**API Methods:**
```rust
impl P2PRegistry {
    /// Get comprehensive reputation score (v2.4.0)
    pub async fn get_comprehensive_reputation(
        &self,
        peer_id: &PeerId,
        my_location: Option<&GeoLocation>
    ) -> f64;

    /// Select best peers for a query
    pub async fn select_best_peers(
        &self,
        min_reputation: f64,
        limit: usize,
        my_location: Option<&GeoLocation>
    ) -> Vec<(PeerId, f64)>;

    /// Update peer geographic location
    pub async fn update_peer_location(
        &self,
        peer_id: PeerId,
        location: GeoLocation
    );
}
```

**Reputation Factors:**
```rust
// Reputation score calculation (0.0 to 1.0)
let score =
    0.50 * success_rate +           // 50% weight
    0.25 * response_time_score +    // 25% weight
    0.15 * availability_score +     // 15% weight
    0.10 * recency_score +          // 10% weight
    geo_bonus;                      // Up to +10% for proximity

// Geo-proximity bonus
if distance_km < 100.0 {
    geo_bonus = 0.1 * (1.0 - distance_km / 100.0);
}
```

**Usage Example:**
```rust
// Set local location
let my_location = GeoLocation {
    latitude: 37.7749,
    longitude: -122.4194,
    region: Some("US-CA".to_string()),
};
registry.set_location(my_location.clone()).await;

// Select best peers
let best_peers = registry
    .select_best_peers(0.7, 10, Some(&my_location))
    .await;

for (peer_id, score) in best_peers {
    println!("Peer: {} (score: {:.2})", peer_id, score);
}
```

#### Parallel DHT Queries (v2.4.0)

Fan-out query strategy for faster lookups.

**API Methods:**
```rust
impl P2PRegistry {
    /// Query DHT with parallel fan-out strategy
    async fn query_dht_parallel(
        &self,
        package_id: &PackageId,
        fan_out: usize
    ) -> Result<Option<Package>>;
}
```

**Usage:**
```rust
let package_id = PackageId::new("io.ggen.rust.cli");

// Query with fan-out of 3 peers
let package = registry
    .query_dht_parallel(&package_id, 3)
    .await?;
```

**Performance:**
- Single query: ~500ms average
- Parallel (fan-out=3): ~200ms average (2.5x faster)
- Target: <200ms for 1000+ peer networks

---

### HTTP API

Content distribution server for package downloads (v2.4.0 Phase 2).

**Base URL:** `http://localhost:8080` (configurable)

#### GET /

Get server information and capabilities.

**Response:**
```json
{
  "name": "ggen P2P Content Server",
  "version": "2.4.0",
  "peer_id": "12D3KooWPjceQrSwdWXPyLLeABRXmuqt69Rg3sBYbU1Nft9HyQ6X",
  "capabilities": [
    "package_list",
    "package_download",
    "checksum_verification"
  ]
}
```

#### GET /packages

List all available packages.

**Response:**
```json
{
  "packages": [
    {
      "id": "io.ggen.rust.cli",
      "name": "Rust CLI Templates",
      "version": "0.1.0",
      "size": 12345,
      "checksum": "sha256:abcd1234..."
    }
  ],
  "total": 1
}
```

#### GET /packages/:id

Get package metadata.

**Parameters:**
- `:id` - Package ID

**Response:**
```json
{
  "id": "io.ggen.rust.cli",
  "name": "Rust CLI Templates",
  "version": "0.1.0",
  "description": "CLI template generators",
  "author": "ggen-team",
  "size": 12345,
  "checksum": "sha256:abcd1234..."
}
```

#### GET /packages/:id/info

Get download information with checksum.

**Parameters:**
- `:id` - Package ID

**Response:**
```json
{
  "id": "io.ggen.rust.cli",
  "download_url": "/packages/io.ggen.rust.cli/download",
  "size": 12345,
  "checksum": "sha256:abcd1234...",
  "algorithm": "SHA256"
}
```

#### GET /packages/:id/download

Download package content.

**Parameters:**
- `:id` - Package ID

**Response:**
- Content-Type: `application/octet-stream`
- Content-Length: Package size in bytes
- Body: Package content (tarball)

**Size Limits:**
- Default: 100MB maximum
- Configurable via environment variable

---

## Registry API

### Search

Search for packages with filters and ranking.

**CLI:**
```bash
ggen marketplace search <QUERY> [OPTIONS]
```

**Rust:**
```rust
use ggen_marketplace::traits::Registry;

let query = Query {
    text: "rust cli".to_string(),
    categories: vec!["rust".to_string()],
    tags: vec!["cli".to_string(), "clap".to_string()],
    limit: Some(20),
};

let results = registry.search(&query).await?;
```

**Search Features:**
- Full-text search (name, description)
- Category filtering
- Tag matching
- Relevance-based ranking
- Result limits

### Install

Install a package from the marketplace.

**CLI:**
```bash
ggen marketplace install <PACKAGE_ID> [OPTIONS]
```

**Options:**
- `--version <VERSION>` - Specific version (default: latest)
- `--force` - Force reinstall

**Examples:**
```bash
# Install latest version
ggen marketplace install io.ggen.rust.cli-subcommand

# Install specific version
ggen marketplace install io.ggen.rust.cli-subcommand --version 0.1.0

# Force reinstall
ggen marketplace install io.ggen.rust.cli-subcommand --force
```

### List

List installed packages.

**CLI:**
```bash
ggen marketplace list [OPTIONS]
```

**Options:**
- `--json` - Output in JSON format

**Examples:**
```bash
# List all installed packages
ggen marketplace list

# JSON output
ggen marketplace list --json
```

---

## Configuration

### Environment Variables

#### GGEN_REGISTRY_URL

Registry URL for package discovery.

**Values:**
```bash
# GitHub Pages (default)
export GGEN_REGISTRY_URL="https://seanchatmangpt.github.io/ggen/registry/"

# Local development
export GGEN_REGISTRY_URL="file:///path/to/registry/"

# Custom registry
export GGEN_REGISTRY_URL="https://your-registry.com/"
```

#### P2P Configuration

**Feature Flag:**
```bash
# Build with P2P support
cargo build --features p2p
```

**Runtime:**
```bash
# P2P bootstrap nodes
export GGEN_P2P_BOOTSTRAP="node1,node2,node3"

# P2P listen address
export GGEN_P2P_LISTEN="/ip4/0.0.0.0/tcp/7777"

# DHT server mode
export GGEN_P2P_DHT_SERVER="true"
```

---

## Examples

### Example 1: Start P2P Node and Search

```bash
# Terminal 1: Start P2P node
ggen marketplace p2p start --daemon

# Terminal 2: Search packages
ggen marketplace p2p search "rust cli"

# Terminal 3: Check status
ggen marketplace p2p status
```

### Example 2: Publish Package to P2P Network

```bash
# Create package
mkdir my-package
cd my-package

# Create manifest
cat > gpack.toml << 'EOF'
[gpack]
id = "io.example.my-package"
name = "My Package"
version = "1.0.0"
description = "Example package"
license = "MIT"
ggen_compat = ">=2.4.0"
EOF

# Publish to P2P network
ggen marketplace p2p publish .
```

### Example 3: Rust Integration

```rust
use ggen_marketplace::backend::p2p::{P2PConfig, P2PRegistry};
use ggen_marketplace::traits::Registry;
use ggen_marketplace::models::Query;

#[tokio::main]
async fn main() -> Result<(), Box<dyn std::error::Error>> {
    // Configure P2P
    let config = P2PConfig {
        bootstrap_nodes: vec![
            "/ip4/104.131.131.82/tcp/4001/p2p/QmaCpDMGvV2BGHeYERUEnRQAwe3N8SzbUtfsmvsqQLuvuJ".parse()?
        ],
        dht_server_mode: true,
        ..Default::default()
    };

    // Create registry
    let registry = P2PRegistry::new(config).await?;
    registry.start_listening().await?;
    registry.subscribe_to_packages().await?;
    registry.bootstrap().await?;

    // Search packages
    let query = Query {
        text: "rust cli".to_string(),
        categories: vec![],
        tags: vec![],
        limit: Some(10),
    };
    let results = registry.search(&query).await?;

    println!("Found {} packages", results.len());
    for package in results {
        println!("  - {} ({})", package.metadata.title, package.version);
    }

    Ok(())
}
```

### Example 4: Geo-Proximity Selection

```rust
use ggen_marketplace::backend::p2p::{P2PRegistry, GeoLocation};

async fn select_nearby_peers(registry: &P2PRegistry) -> Result<()> {
    // Set local location (San Francisco)
    let my_location = GeoLocation {
        latitude: 37.7749,
        longitude: -122.4194,
        region: Some("US-CA".to_string()),
    };
    registry.set_location(my_location.clone()).await;

    // Select best peers (minimum reputation 0.7, max 10 peers)
    let peers = registry
        .select_best_peers(0.7, 10, Some(&my_location))
        .await;

    for (peer_id, score) in peers {
        println!("Peer: {} (reputation: {:.2})", peer_id, score);
    }

    Ok(())
}
```

---

## Migration from 2.3.0

### Breaking Changes

None. Version 2.4.0 is fully backward compatible with 2.3.0.

### New Features

1. **P2P Marketplace** - New optional feature (requires `--features p2p`)
2. **Geo-Proximity Routing** - Automatic peer selection based on location
3. **Parallel DHT Queries** - Faster lookups with fan-out strategy
4. **HTTP Content Server** - Package distribution endpoints

### Upgrading

```bash
# Update to 2.4.0
cargo update ggen

# Enable P2P features (optional)
cargo build --features p2p

# No configuration changes required
```

### Deprecations

None.

---

## Performance Targets

| Operation | Target | v2.4.0 Actual |
|-----------|--------|---------------|
| P2P node startup | <5s | ~3s |
| Package search | <2s | ~1.5s |
| DHT query (single) | <500ms | ~450ms |
| DHT query (parallel) | <200ms | ~180ms |
| Peer discovery | <3s | ~2s |
| Cache hit rate | >80% | ~85% |

---

## OpenTelemetry Instrumentation

All P2P operations are instrumented with OpenTelemetry for observability.

**Spans:**
- `search`: Query text, result count
- `get_package`: Package ID, cache hit/miss
- `query_dht_parallel`: Package ID, fan-out count
- `record_peer_success`: Peer ID, response time

**Example:**
```rust
use tracing::instrument;

#[instrument(skip(self), fields(package_id = %id))]
async fn get_package(&self, id: &PackageId) -> Result<Package> {
    // ... implementation
}
```

---

## Support

- **Documentation:** https://github.com/seanchatmangpt/ggen/tree/main/docs
- **Issues:** https://github.com/seanchatmangpt/ggen/issues
- **Discussions:** https://github.com/seanchatmangpt/ggen/discussions

---

**Last Updated:** 2025-11-02
**API Version:** 2.4.0
**License:** MIT
