# ggen 2.4.0 CLI Reference

**Version:** 2.4.0
**Release Date:** 2025-11-02
**Command Pattern:** Noun-Verb (clap-based)

## Table of Contents

- [Quick Start](#quick-start)
- [Marketplace Commands](#marketplace-commands)
- [P2P Commands](#p2p-commands)
- [Configuration](#configuration)
- [Common Workflows](#common-workflows)
- [Troubleshooting](#troubleshooting)

---

## Quick Start

```bash
# Install ggen 2.4.0
cargo install ggen --version 2.4.0

# Build with P2P support (optional)
cargo build --features p2p

# Verify installation
ggen --version
# Output: ggen 2.4.0

# Get help
ggen --help
ggen marketplace --help
ggen marketplace p2p --help
```

---

## Marketplace Commands

### ggen marketplace search

Search for packages in the marketplace.

```bash
# Basic search
ggen marketplace search rust

# Search with multiple terms
ggen marketplace search "rust cli"

# Search by category
ggen marketplace search web --category rust

# Search with tags
ggen marketplace search api --tags rest --tags async

# Limit results
ggen marketplace search template --limit 10
```

**Output:**
```
🔍 Searching marketplace for 'rust cli'...

Found 3 packages:

io.ggen.rust.cli-subcommand (v0.1.0)
  Description: Generate clap subcommands
  Category: rust
  Tags: rust, cli, clap, subcommand

io.ggen.rust.cli-parser (v0.2.0)
  Description: CLI argument parsing templates
  Category: rust
  Tags: rust, cli, parser
```

---

### ggen marketplace install

Install a package from the marketplace.

```bash
# Install latest version
ggen marketplace install io.ggen.rust.cli-subcommand

# Install specific version
ggen marketplace install io.ggen.rust.cli-subcommand --version 0.1.0

# Force reinstall
ggen marketplace install io.ggen.rust.cli-subcommand --force

# Install from local path
ggen marketplace install ./local-package
```

**Output:**
```
📦 Installing io.ggen.rust.cli-subcommand...
⬇️  Downloading package...
✓ Downloaded 15.2 KB
✓ Verified checksum
✓ Extracted to ~/.ggen/packs/io.ggen.rust.cli-subcommand
✅ Installation complete
```

---

### ggen marketplace list

List installed packages.

```bash
# List all packages
ggen marketplace list

# JSON output
ggen marketplace list --json

# Filter by category
ggen marketplace list --category rust
```

**Output:**
```
📦 Installed Packages:

io.ggen.rust.cli-subcommand (v0.1.0)
  Location: ~/.ggen/packs/io.ggen.rust.cli-subcommand
  Size: 15.2 KB

io.ggen.web.api-route (v0.2.0)
  Location: ~/.ggen/packs/io.ggen.web.api-route
  Size: 22.1 KB

Total: 2 packages installed
```

---

### ggen marketplace publish

Publish a package to the marketplace.

```bash
# Publish package
ggen marketplace publish ./my-package

# Publish with version
ggen marketplace publish ./my-package --version 1.0.0

# Dry run (validation only)
ggen marketplace publish ./my-package --dry-run
```

**Requirements:**
- Package must contain `gpack.toml` manifest
- Valid semantic version (major.minor.patch)
- All required fields in manifest

---

### ggen marketplace update

Update installed packages.

```bash
# Update all packages
ggen marketplace update

# Update specific package
ggen marketplace update io.ggen.rust.cli-subcommand

# Check for updates only
ggen marketplace update --check
```

**Output:**
```
🔄 Checking for updates...

Updates available:
  io.ggen.rust.cli-subcommand: 0.1.0 → 0.2.0
  io.ggen.web.api-route: 0.2.0 (up to date)

Updating 1 package...
✅ Updated io.ggen.rust.cli-subcommand to 0.2.0
```

---

## P2P Commands

### ggen marketplace p2p start

Start a P2P node and connect to the network.

```bash
# Start with defaults
ggen marketplace p2p start

# Start with bootstrap nodes
ggen marketplace p2p start \
  --bootstrap /ip4/104.131.131.82/tcp/4001/p2p/QmaCpDMGvV2BGHeYERUEnRQAwe3N8SzbUtfsmvsqQLuvuJ

# Start in daemon mode
ggen marketplace p2p start --daemon

# Custom listen address
ggen marketplace p2p start --listen /ip4/0.0.0.0/tcp/7777

# With configuration file
ggen marketplace p2p start --config ~/.ggen/p2p.toml

# Multiple bootstrap nodes
ggen marketplace p2p start \
  --bootstrap /ip4/104.131.131.82/tcp/4001/p2p/QmaCpDMGvV2BGHeYERUEnRQAwe3N8SzbUtfsmvsqQLuvuJ \
  --bootstrap /ip4/104.131.131.83/tcp/4001/p2p/QmYwAPJzv5CZsnA625s3Xf2nemtYgPpHdWEz79ojWnPbdG
```

**Output:**
```
🚀 Starting P2P node...
✅ P2P node started successfully
📡 Listening for package announcements...

Peer ID: 12D3KooWPjceQrSwdWXPyLLeABRXmuqt69Rg3sBYbU1Nft9HyQ6X

Listen addresses:
  /ip4/127.0.0.1/tcp/54321/p2p/12D3KooWPjceQrSwdWXPyLLeABRXmuqt69Rg3sBYbU1Nft9HyQ6X
  /ip4/192.168.1.100/tcp/54321/p2p/12D3KooWPjceQrSwdWXPyLLeABRXmuqt69Rg3sBYbU1Nft9HyQ6X

Connected to 2 bootstrap nodes
```

---

### ggen marketplace p2p publish

Publish a package to the P2P network.

```bash
# Publish package
ggen marketplace p2p publish ./my-package

# Publish with explicit version
ggen marketplace p2p publish ./my-package --version 1.0.0

# Skip verification (development)
ggen marketplace p2p publish ./my-package --skip-verify
```

**Output:**
```
📦 Publishing package to P2P network...
✓ Package validation passed
✓ Package metadata extracted
✓ Content hash calculated: sha256:abcd1234...
✅ Package 'my-package@1.0.0' ready for publishing
📡 Announced to 12 network peers
💾 Stored in DHT with 3 replicas
```

---

### ggen marketplace p2p search

Search for packages on the P2P network.

```bash
# Basic search
ggen marketplace p2p search "rust cli"

# Search with category
ggen marketplace p2p search "web framework" --category rust

# Search with tags
ggen marketplace p2p search "api" --tags rest --tags async

# Limit results
ggen marketplace p2p search "template" --limit 10

# High-reputation peers only
ggen marketplace p2p search "database" --min-reputation 0.8

# Combine filters
ggen marketplace p2p search "cli" \
  --category rust \
  --tags async \
  --limit 5 \
  --min-reputation 0.7
```

**Output:**
```
🔍 Searching P2P network for 'rust cli'...

📊 Found 3 packages (query time: 1.2s)

io.ggen.rust.cli-subcommand (v0.1.0)
  Description: Generate clap subcommands
  Providers: 5 peers (avg reputation: 0.92)
  Download size: 15.2 KB

io.ggen.rust.cli-parser (v0.2.0)
  Description: CLI argument parsing templates
  Providers: 3 peers (avg reputation: 0.87)
  Download size: 12.8 KB

io.ggen.rust.cli-validation (v0.1.0)
  Description: Input validation for CLI apps
  Providers: 4 peers (avg reputation: 0.79)
  Download size: 10.5 KB
```

---

### ggen marketplace p2p peer-list

List connected peers with reputation information.

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

# Combine filters
ggen marketplace p2p peer-list \
  --min-reputation 0.8 \
  --verbose \
  --format json
```

**Output (Table):**
```
👥 Connected Peers:

Peer ID                                              Reputation   Packages
----------------------------------------------------------------------------
12D3KooWPjceQrSwdWXPyLLeABRXmuqt69Rg3sBYbU1Nft9HyQ6        0.92         12
12D3KooWDpJ7As7BWAwRMfu1VU2WCqNjvq387JEYKDBj4kx6nXTN        0.87          8
12D3KooWQYbGH7Tv9YCKvv1SqhwcGJc7B1Qb8YE3mAcLdRh2kXmP        0.79          5

Total: 3 peer(s)
```

**Output (Verbose):**
```
👥 Connected Peers:

Peer ID                                              Reputation   Packages
----------------------------------------------------------------------------
12D3KooWPjceQrSwdWXPyLLeABRXmuqt69Rg3sBYbU1Nft9HyQ6        0.92         12
12D3KooWDpJ7As7BWAwRMfu1VU2WCqNjvq387JEYKDBj4kx6nXTN        0.87          8

Total: 2 peer(s)

Detailed Information:

  Peer: 12D3KooWPjceQrSwdWXPyLLeABRXmuqt69Rg3sBYbU1Nft9HyQ6X
    Addresses: ["/ip4/104.131.131.82/tcp/4001"]
    Successful: 45 | Failed: 2
    Last Seen: 2025-11-02T10:30:00Z
    Avg Response: 120ms

  Peer: 12D3KooWDpJ7As7BWAwRMfu1VU2WCqNjvq387JEYKDBj4kx6nXTN
    Addresses: ["/ip4/104.131.131.83/tcp/4001"]
    Successful: 38 | Failed: 5
    Last Seen: 2025-11-02T10:25:00Z
    Avg Response: 180ms
```

**Output (JSON):**
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
      "packages_provided": 12,
      "avg_response_time_ms": 120
    }
  ],
  "total": 1
}
```

---

### ggen marketplace p2p peer-info

Get detailed information about a specific peer.

```bash
# Basic peer info
ggen marketplace p2p peer-info 12D3KooWPjceQrSwdWXPyLLeABRXmuqt69Rg3sBYbU1Nft9HyQ6X

# Full details with history
ggen marketplace p2p peer-info 12D3KooWPjceQrSwdWXPyLLeABRXmuqt69Rg3sBYbU1Nft9HyQ6X --full
```

**Output:**
```
ℹ️  Peer Information: 12D3KooWPjceQrSwdWXPyLLeABRXmuqt69Rg3sBYbU1Nft9HyQ6X

  Reputation: 0.92
  Status: Active
  Success Rate: 95.7% (45 successful / 2 failed)
  Avg Response: 120ms
  Packages: 12
  Last Seen: 2025-11-02T10:30:00Z

Detailed Information:
  Full Peer ID: 12D3KooWPjceQrSwdWXPyLLeABRXmuqt69Rg3sBYbU1Nft9HyQ6X
  Reputation Score: 0.9235
  Geo-Location: US-CA (San Francisco)
  Distance: ~15 km
```

---

### ggen marketplace p2p bootstrap

Bootstrap DHT with known peers.

```bash
# Bootstrap with single node
ggen marketplace p2p bootstrap \
  /ip4/104.131.131.82/tcp/4001/p2p/QmaCpDMGvV2BGHeYERUEnRQAwe3N8SzbUtfsmvsqQLuvuJ

# Bootstrap with multiple nodes
ggen marketplace p2p bootstrap \
  /ip4/104.131.131.82/tcp/4001/p2p/QmaCpDMGvV2BGHeYERUEnRQAwe3N8SzbUtfsmvsqQLuvuJ \
  /ip4/104.131.131.83/tcp/4001/p2p/QmYwAPJzv5CZsnA625s3Xf2nemtYgPpHdWEz79ojWnPbdG

# With custom timeout
ggen marketplace p2p bootstrap \
  /ip4/104.131.131.82/tcp/4001/p2p/QmaCpDMGvV2BGHeYERUEnRQAwe3N8SzbUtfsmvsqQLuvuJ \
  --timeout 60
```

**Output:**
```
🔗 Bootstrapping DHT with 2 nodes...
✓ Connected to node 1/2
✓ Connected to node 2/2
✅ DHT bootstrap complete
📊 DHT routing table: 15 entries
```

---

### ggen marketplace p2p status

Get local P2P node status and information.

```bash
# Get status
ggen marketplace p2p status
```

**Output (Running):**
```
📊 P2P Node Status:

  Status: ✅ Running
  Uptime: 2h 34m
  Peer ID: 12D3KooWPjceQrSwdWXPyLLeABRXmuqt69Rg3sBYbU1Nft9HyQ6X

Registry:
  Name: Ggen P2P Registry
  Description: Decentralized P2P package registry using libp2p
  URL: /p2p/12D3KooWPjceQrSwdWXPyLLeABRXmuqt69Rg3sBYbU1Nft9HyQ6X
  Publishing: Enabled
  Authentication: Not required

Network:
  Connected Peers: 12
  DHT Entries: 45
  Local Packages: 8
  Cache Size: 2.3 MB

Capabilities:
  • Decentralized package discovery
  • DHT-based metadata storage
  • Peer reputation tracking
  • Gossipsub package announcements
  • Geo-proximity routing
  • Multi-tier caching
```

**Output (Not Running):**
```
📊 P2P Node Status:

  Status: ⭕ Not running

Start the P2P node with:
  ggen marketplace p2p start

Or with bootstrap nodes:
  ggen marketplace p2p start --bootstrap <node-multiaddr>
```

---

## Configuration

### Environment Variables

```bash
# Registry URL
export GGEN_REGISTRY_URL="https://seanchatmangpt.github.io/ggen/registry/"

# P2P bootstrap nodes
export GGEN_P2P_BOOTSTRAP="node1,node2,node3"

# P2P listen address
export GGEN_P2P_LISTEN="/ip4/0.0.0.0/tcp/7777"

# DHT server mode
export GGEN_P2P_DHT_SERVER="true"

# Log level
export RUST_LOG="info"
export RUST_LOG="ggen=debug"  # Debug mode
```

### Configuration File

Create `~/.ggen/config.toml`:

```toml
[registry]
url = "https://seanchatmangpt.github.io/ggen/registry/"

[p2p]
bootstrap_nodes = [
  "/ip4/104.131.131.82/tcp/4001/p2p/QmaCpDMGvV2BGHeYERUEnRQAwe3N8SzbUtfsmvsqQLuvuJ"
]
listen_addresses = ["/ip4/0.0.0.0/tcp/0"]
dht_server_mode = true
packages_topic = "/ggen/packages/v1"

[location]
latitude = 37.7749
longitude = -122.4194
region = "US-CA"

[cache]
max_size_mb = 100
ttl_minutes = 5
```

---

## Common Workflows

### Workflow 1: Set Up P2P Node

```bash
# Step 1: Start P2P node with bootstrap
ggen marketplace p2p start \
  --bootstrap /ip4/104.131.131.82/tcp/4001/p2p/QmaCpDMGvV2BGHeYERUEnRQAwe3N8SzbUtfsmvsqQLuvuJ \
  --daemon

# Step 2: Check status
ggen marketplace p2p status

# Step 3: List connected peers
ggen marketplace p2p peer-list
```

### Workflow 2: Search and Install Package

```bash
# Step 1: Search P2P network
ggen marketplace p2p search "rust cli" --min-reputation 0.8

# Step 2: Install from P2P network
ggen marketplace install io.ggen.rust.cli-subcommand

# Step 3: Verify installation
ggen marketplace list
```

### Workflow 3: Publish Package

```bash
# Step 1: Create package structure
mkdir my-package
cd my-package

# Step 2: Create manifest
cat > gpack.toml << 'EOF'
[gpack]
id = "io.example.my-package"
name = "My Package"
version = "1.0.0"
description = "Example package"
license = "MIT"
ggen_compat = ">=2.4.0"
EOF

# Step 3: Validate package
ggen marketplace publish . --dry-run

# Step 4: Publish to P2P network
ggen marketplace p2p publish .

# Step 5: Verify publication
ggen marketplace p2p search "my-package"
```

### Workflow 4: Monitor P2P Network

```bash
# Terminal 1: Start node with verbose logging
RUST_LOG=debug ggen marketplace p2p start --daemon

# Terminal 2: Monitor peers
watch -n 5 'ggen marketplace p2p peer-list'

# Terminal 3: Monitor status
watch -n 10 'ggen marketplace p2p status'
```

---

## Troubleshooting

### P2P Node Won't Start

**Problem:**
```
⚠️  Feature not enabled: p2p
Rebuild with --features p2p to enable P2P functionality
```

**Solution:**
```bash
# Rebuild with P2P feature
cargo build --features p2p
cargo install --path . --features p2p
```

---

### Can't Connect to Bootstrap Nodes

**Problem:**
```
❌ Bootstrap failed: Connection timeout
```

**Solution:**
```bash
# Try alternative bootstrap nodes
ggen marketplace p2p start \
  --bootstrap /ip4/104.131.131.83/tcp/4001/p2p/QmYwAPJzv5CZsnA625s3Xf2nemtYgPpHdWEz79ojWnPbdG

# Or start without bootstrap (local-only mode)
ggen marketplace p2p start
```

---

### Package Search Returns No Results

**Problem:**
```
🔍 Searching P2P network for 'rust cli'...
📊 Found 0 packages
```

**Solution:**
```bash
# Check P2P node status
ggen marketplace p2p status

# Bootstrap if not connected
ggen marketplace p2p bootstrap <node-address>

# Lower reputation threshold
ggen marketplace p2p search "rust cli" --min-reputation 0.3
```

---

### Peer List Shows No Peers

**Problem:**
```
👥 Connected Peers:
No peers connected yet
```

**Solution:**
```bash
# Bootstrap with known nodes
ggen marketplace p2p bootstrap \
  /ip4/104.131.131.82/tcp/4001/p2p/QmaCpDMGvV2BGHeYERUEnRQAwe3N8SzbUtfsmvsqQLuvuJ

# Wait for peer discovery (30-60 seconds)
sleep 60

# Check again
ggen marketplace p2p peer-list
```

---

### Debug Mode

Enable debug logging for detailed troubleshooting:

```bash
# Full debug output
export RUST_LOG=debug
ggen marketplace p2p start

# Component-specific debug
export RUST_LOG="ggen=debug,libp2p=info"
ggen marketplace p2p start

# Trace-level logging
export RUST_LOG=trace
ggen marketplace p2p start
```

---

## Performance Tips

### Optimize Search Performance

```bash
# Use parallel queries (default fan-out: 3)
ggen marketplace p2p search "rust" --limit 10

# Increase reputation threshold for faster peers
ggen marketplace p2p search "rust" --min-reputation 0.8
```

### Reduce Latency with Geo-Proximity

```bash
# Set location in config file
cat > ~/.ggen/config.toml << 'EOF'
[location]
latitude = 37.7749
longitude = -122.4194
region = "US-CA"
EOF

# Restart P2P node to apply
ggen marketplace p2p start
```

### Enable Caching

Caching is enabled by default. To configure:

```toml
# ~/.ggen/config.toml
[cache]
max_size_mb = 100    # Maximum cache size
ttl_minutes = 5      # Time to live for cached entries
```

---

## Advanced Usage

### Custom Bootstrap Nodes

Create a bootstrap node list file:

```bash
# bootstrap-nodes.txt
/ip4/104.131.131.82/tcp/4001/p2p/QmaCpDMGvV2BGHeYERUEnRQAwe3N8SzbUtfsmvsqQLuvuJ
/ip4/104.131.131.83/tcp/4001/p2p/QmYwAPJzv5CZsnA625s3Xf2nemtYgPpHdWEz79ojWnPbdG
/ip4/104.131.131.84/tcp/4001/p2p/QmSoLV4Bbm51jM9C4gDYZQ9Cy3U6aXMJDAbzgu2fzaDs64

# Use with start command
while read node; do
  ggen marketplace p2p start --bootstrap "$node"
done < bootstrap-nodes.txt
```

### Scripted Monitoring

```bash
#!/bin/bash
# monitor-p2p.sh - Monitor P2P network health

while true; do
  clear
  echo "=== P2P Network Monitor ==="
  echo
  ggen marketplace p2p status
  echo
  echo "=== Connected Peers ==="
  ggen marketplace p2p peer-list
  echo
  sleep 10
done
```

---

**Last Updated:** 2025-11-02
**CLI Version:** 2.4.0
**License:** MIT
