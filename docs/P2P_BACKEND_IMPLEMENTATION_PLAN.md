# P2P Backend Implementation Plan for ggen Marketplace

## Executive Summary

**Current Status**: ggen has P2P infrastructure scaffolding but NO actual P2P networking library integration.

**Critical Gap**: The codebase references `libp2p` in comments but has ZERO libp2p dependencies in Cargo.toml.

**80/20 Solution**: Integrate rust-libp2p with minimal protocols to enable decentralized package distribution.

## Analysis Findings

### 1. Existing Infrastructure (Good Foundation)
✅ **Traits Defined** (`ggen-marketplace/src/traits/mod.rs`):
- `Registry` - Package discovery and management
- `PackageStore` - Content-addressable storage
- `SearchEngine` - Advanced search functionality
- `CryptoVerifier` - Signature verification

✅ **P2P Module Structure** (`src/p2p/`):
- `registry.rs` - P2P registry implementation (trait-based, no actual networking)
- `behaviour.rs` - MarketplaceBehaviour with peer management
- `config.rs` - P2P configuration structs
- `discovery.rs` - Peer discovery logic
- `content.rs` - Content routing and providers
- `protocol.rs` - Request/response protocol definitions
- `types.rs` - Package, Query, SearchResult types

✅ **Registry Infrastructure** (`cli/src/domain/marketplace/registry.rs`):
- File-based registry index (JSON)
- LRU cache manager for package metadata
- Async filesystem operations
- Version resolution

### 2. Critical Missing Components

❌ **NO libp2p dependency**:
```toml
# Missing from Cargo.toml
libp2p = { version = "0.54", features = ["tcp", "dns", "noise", "yamux", "gossipsub", "kad", "identify"] }
```

❌ **NO actual networking code**:
- `P2PRegistry::start()` only sets `running = true` - no network initialization
- `MarketplaceBehaviour` manages state but doesn't communicate over network
- `PeerDiscovery::discover()` returns hardcoded bootstrap nodes - no mDNS/DHT

❌ **NO content distribution**:
- `PackageStore` trait exists but no IPFS/BitTorrent-like implementation
- `ContentRouter` has provider selection logic but no actual content transfer

❌ **NO gossipsub integration**:
- `subscribe_to_updates()` exists but doesn't actually subscribe to pubsub topics

## Recommended P2P Backend: rust-libp2p

### Why libp2p?

1. **Rust-native**: Excellent async/await support with Tokio
2. **Modular**: Pick only the protocols you need
3. **Battle-tested**: Used by Polkadot, IPFS, Filecoin
4. **Feature-rich**: DHT (Kademlia), PubSub (Gossipsub), mDNS, TCP/QUIC transports
5. **Active development**: Well-maintained by Protocol Labs

### Alternative Considered: quinn (QUIC only)

**Rejected because**:
- Low-level transport only (no DHT, pubsub, discovery)
- Would require building entire application layer
- Not worth it for this use case

## 80/20 Implementation Plan

### Phase 1: Core P2P Networking (20% effort, 80% value)

**Goal**: Enable basic peer discovery and package announcements

#### 1.1 Add libp2p Dependencies

```toml
[dependencies]
libp2p = { version = "0.54", features = [
    "tcp",           # TCP transport
    "dns",           # DNS resolution
    "noise",         # Encryption
    "yamux",         # Multiplexing
    "gossipsub",     # Pub/Sub messaging
    "kad",           # Kademlia DHT for discovery
    "identify",      # Peer identification
    "mdns",          # Local network discovery
] }
libp2p-swarm = "0.44"
libp2p-core = "0.41"
```

#### 1.2 Implement Network Behavior

**File**: `cli/src/domain/marketplace/network_behaviour.rs`

```rust
use libp2p::{
    gossipsub, kad, mdns, noise, tcp, yamux,
    swarm::{NetworkBehaviour, SwarmEvent},
    PeerId, Multiaddr,
};

#[derive(NetworkBehaviour)]
pub struct MarketplaceNetworkBehaviour {
    pub gossipsub: gossipsub::Behaviour,  // Package announcements
    pub kademlia: kad::Behaviour<kad::store::MemoryStore>,  // DHT for peer discovery
    pub mdns: mdns::tokio::Behaviour,  // Local peer discovery
    pub identify: libp2p::identify::Behaviour,  // Peer info exchange
}

impl MarketplaceNetworkBehaviour {
    pub fn new(local_peer_id: PeerId) -> Self {
        // Initialize each protocol
        let gossipsub_config = gossipsub::ConfigBuilder::default()
            .heartbeat_interval(Duration::from_secs(1))
            .validation_mode(gossipsub::ValidationMode::Strict)
            .build()
            .expect("Valid gossipsub config");

        let gossipsub = gossipsub::Behaviour::new(
            gossipsub::MessageAuthenticity::Signed(local_key),
            gossipsub_config,
        ).expect("Valid gossipsub");

        let kademlia = kad::Behaviour::new(
            local_peer_id,
            kad::store::MemoryStore::new(local_peer_id),
        );

        let mdns = mdns::tokio::Behaviour::new(
            mdns::Config::default(),
            local_peer_id,
        ).expect("Valid mDNS");

        let identify = libp2p::identify::Behaviour::new(
            libp2p::identify::Config::new(
                "/ggen-marketplace/1.0.0".to_string(),
                local_key.public(),
            ),
        );

        Self { gossipsub, kademlia, mdns, identify }
    }
}
```

#### 1.3 Update P2PRegistry with Real Networking

**File**: `cli/src/domain/marketplace/p2p_registry.rs`

```rust
use libp2p::{Swarm, SwarmBuilder};
use crate::domain::marketplace::network_behaviour::MarketplaceNetworkBehaviour;

pub struct P2PRegistry {
    swarm: Arc<RwLock<Swarm<MarketplaceNetworkBehaviour>>>,
    local_peer_id: PeerId,
    running: Arc<RwLock<bool>>,
}

impl P2PRegistry {
    pub async fn start(&self) -> Result<()> {
        let mut swarm = self.swarm.write().await;

        // Listen on all interfaces
        swarm.listen_on("/ip4/0.0.0.0/tcp/0".parse()?)?;

        // Bootstrap DHT
        for bootstrap_node in &self.config.bootstrap.nodes {
            let peer_id: PeerId = bootstrap_node.peer_id.parse()?;
            let addr: Multiaddr = bootstrap_node.address.parse()?;
            swarm.behaviour_mut().kademlia.add_address(&peer_id, addr);
        }
        swarm.behaviour_mut().kademlia.bootstrap()?;

        // Subscribe to marketplace topic
        let topic = gossipsub::IdentTopic::new("ggen-marketplace");
        swarm.behaviour_mut().gossipsub.subscribe(&topic)?;

        *self.running.write().await = true;

        // Spawn event loop
        tokio::spawn(self.clone().run_event_loop());

        Ok(())
    }

    async fn run_event_loop(self) {
        let mut swarm = self.swarm.write().await;
        loop {
            match swarm.select_next_some().await {
                SwarmEvent::Behaviour(event) => {
                    // Handle gossipsub messages (package announcements)
                    // Handle Kademlia events (peer discovery)
                    // Handle mDNS discoveries
                }
                _ => {}
            }
        }
    }
}
```

#### 1.4 Package Announcement Protocol

**File**: `cli/src/domain/marketplace/package_announcement.rs`

```rust
#[derive(Serialize, Deserialize)]
pub struct PackageAnnouncement {
    pub package_id: String,
    pub version: String,
    pub checksum: String,
    pub provider_peer_id: String,
    pub timestamp: i64,
}

impl P2PRegistry {
    pub async fn announce_package(&self, pkg: &PackageMetadata) -> Result<()> {
        let announcement = PackageAnnouncement {
            package_id: pkg.name.clone(),
            version: pkg.versions[0].version.clone(),
            checksum: pkg.versions[0].checksum.clone(),
            provider_peer_id: self.local_peer_id.to_string(),
            timestamp: chrono::Utc::now().timestamp(),
        };

        let message = serde_json::to_vec(&announcement)?;
        let topic = gossipsub::IdentTopic::new("ggen-marketplace");

        self.swarm.write().await
            .behaviour_mut()
            .gossipsub
            .publish(topic, message)?;

        Ok(())
    }
}
```

### Phase 2: Content Distribution (Next 20% effort)

#### 2.1 Simple HTTP Content Serving

**File**: `cli/src/domain/marketplace/content_server.rs`

```rust
use axum::{Router, routing::get, extract::Path, http::StatusCode};

pub async fn start_content_server(port: u16) -> Result<()> {
    let app = Router::new()
        .route("/packages/:name/:version", get(serve_package));

    axum::Server::bind(&format!("0.0.0.0:{}", port).parse()?)
        .serve(app.into_make_service())
        .await?;

    Ok(())
}

async fn serve_package(
    Path((name, version)): Path<(String, String)>
) -> Result<Vec<u8>, StatusCode> {
    // Read package from local registry cache
    // Return tarball bytes
}
```

#### 2.2 Integrate with P2P

When announcing a package, include HTTP endpoint:

```rust
pub struct PackageAnnouncement {
    pub package_id: String,
    pub version: String,
    pub checksum: String,
    pub provider_peer_id: String,
    pub http_endpoint: String,  // "http://peer-ip:port/packages/name/version"
    pub timestamp: i64,
}
```

### Phase 3: DHT-based Package Discovery (Nice-to-have)

```rust
// Store package metadata in DHT
swarm.behaviour_mut().kademlia.put_record(
    kad::Record::new(package_key, metadata_json),
    kad::Quorum::One,
)?;

// Query DHT for package
swarm.behaviour_mut().kademlia.get_record(package_key);
```

## Integration Points

### 1. Trait Implementation

**File**: `ggen-marketplace/src/p2p/p2p_registry.rs`

Implement marketplace traits using libp2p backend:

```rust
#[async_trait]
impl Registry for P2PRegistry {
    async fn search(&self, query: &Query) -> Result<Vec<Package>> {
        // 1. Query local cache first
        // 2. Broadcast query over gossipsub
        // 3. Wait for responses
        // 4. Aggregate and rank results
    }

    async fn publish(&self, package: Package) -> Result<()> {
        // 1. Store package content locally
        // 2. Announce via gossipsub
        // 3. Add to DHT (optional)
    }
}
```

### 2. Search Integration

**File**: `cli/src/domain/marketplace/search.rs`

```rust
pub async fn search_packages(query: &str, filters: &SearchFilters) -> Result<Vec<SearchResult>> {
    // Try P2P registry first
    if let Ok(p2p_registry) = P2PRegistry::global().await {
        if p2p_registry.is_running().await {
            return p2p_registry.search(&Query::new(query)).await;
        }
    }

    // Fallback to local registry
    search_local_registry(query, filters).await
}
```

### 3. Install Integration

**File**: `cli/src/domain/marketplace/install.rs`

```rust
async fn download_package(name: &str, version: &str) -> Result<Vec<u8>> {
    // 1. Query P2P network for providers
    let providers = p2p_registry.find_providers(name, version).await?;

    // 2. Download from best provider (lowest latency, highest reputation)
    for provider in providers {
        if let Ok(content) = provider.download().await {
            // 3. Verify checksum
            if verify_checksum(&content, &expected_checksum) {
                return Ok(content);
            }
        }
    }

    // 4. Fallback to central registry (if configured)
    Err(anyhow!("Package not found on P2P network"))
}
```

## Performance Considerations

### Caching Strategy

```rust
pub struct P2PCache {
    /// Peer addresses (PeerId -> Vec<Multiaddr>)
    peer_addresses: Arc<RwLock<HashMap<PeerId, Vec<Multiaddr>>>>,

    /// Package providers (PackageId -> Vec<ProviderId>)
    package_providers: Arc<RwLock<HashMap<String, Vec<PeerId>>>>,

    /// Package metadata cache
    metadata_cache: Arc<RwLock<LruCache<String, PackageMetadata>>>,
}
```

### Timeouts

- Peer discovery: 5 seconds
- Package query: 10 seconds
- Content download: 60 seconds per MB

## Security

### 1. Package Verification

```rust
impl CryptoVerifier for P2PRegistry {
    fn verify(&self, content: &[u8], signature: &Signature) -> Result<bool> {
        // Verify Ed25519 signature
        use ed25519_dalek::{Verifier, VerifyingKey};

        let public_key = VerifyingKey::from_bytes(&signature.public_key)?;
        let signature = ed25519_dalek::Signature::from_bytes(&signature.signature)?;

        Ok(public_key.verify(content, &signature).is_ok())
    }
}
```

### 2. Peer Reputation

```rust
pub struct PeerReputation {
    pub successful_downloads: u64,
    pub failed_downloads: u64,
    pub average_latency_ms: f64,
    pub last_seen: SystemTime,
}
```

## Testing Strategy

### Unit Tests

```rust
#[tokio::test]
async fn test_peer_discovery() {
    let registry = P2PRegistry::new_test().await;
    registry.start().await.unwrap();

    // Wait for mDNS to discover local peers
    tokio::time::sleep(Duration::from_secs(2)).await;

    let stats = registry.stats().await;
    assert!(stats.connected_peers > 0);
}

#[tokio::test]
async fn test_package_announcement() {
    let registry = P2PRegistry::new_test().await;
    let pkg = create_test_package("test-pkg", "1.0.0");

    registry.announce_package(&pkg).await.unwrap();

    // Verify announcement was published
    let announcements = registry.get_recent_announcements().await;
    assert_eq!(announcements.len(), 1);
}
```

### Integration Tests (Chicago TDD)

**File**: `tests/chicago_tdd/marketplace/p2p_integration_tests.rs`

```rust
#[tokio::test]
async fn test_p2p_package_discovery() {
    // Start two P2P registries
    let registry1 = P2PRegistry::new_test().await;
    let registry2 = P2PRegistry::new_test().await;

    registry1.start().await.unwrap();
    registry2.start().await.unwrap();

    // Registry 1 publishes a package
    let pkg = create_test_package("p2p-test", "1.0.0");
    registry1.publish(pkg.clone()).await.unwrap();

    // Wait for gossipsub propagation
    tokio::time::sleep(Duration::from_secs(2)).await;

    // Registry 2 searches for the package
    let results = registry2.search(&Query::new("p2p-test")).await.unwrap();

    // Chicago TDD: Verify REAL discovery happened
    assert_eq!(results.len(), 1);
    assert_eq!(results[0].name, "p2p-test");
}
```

## Dependencies to Add

```toml
[dependencies]
# P2P networking
libp2p = { version = "0.54", features = [
    "tcp", "dns", "noise", "yamux",
    "gossipsub", "kad", "identify", "mdns"
] }
libp2p-swarm = "0.44"
libp2p-core = "0.41"

# Cryptography
ed25519-dalek = "2.1"
sha2 = "0.10"

# HTTP content serving (simple alternative to complex P2P transfer)
axum = { version = "0.7", optional = true }
tower = { version = "0.4", optional = true }

# LRU cache for peer/package metadata
lru = "0.12"
```

## Deployment Considerations

### Bootstrap Nodes

```toml
# config.toml
[p2p.bootstrap]
nodes = [
    { peer_id = "12D3KooWBootstrap1...", address = "/ip4/bootstrap1.ggen.io/tcp/4001" },
    { peer_id = "12D3KooWBootstrap2...", address = "/ip4/bootstrap2.ggen.io/tcp/4001" },
]
```

### Firewall/NAT

- Use libp2p's `autonat` protocol for NAT detection
- Enable QUIC for better NAT traversal
- Provide STUN/TURN servers for enterprise environments

## Migration Path

### Stage 1: Hybrid Mode (Recommended First Step)
- Local file-based registry as primary
- P2P announcements as secondary (notifications only)
- No content distribution via P2P yet

### Stage 2: P2P Content Distribution
- Download packages from peers
- Fallback to central registry

### Stage 3: Fully Decentralized
- Remove dependency on central registry
- DHT-only package discovery

## Cost-Benefit Analysis

### Costs
- **Development**: ~2-3 weeks for Phase 1
- **Complexity**: P2P debugging is harder than HTTP
- **Dependencies**: ~50MB additional binary size (libp2p)

### Benefits
- **Resilience**: No single point of failure
- **Bandwidth**: Peer-to-peer reduces server costs
- **Speed**: Local peers = faster downloads
- **Privacy**: No central tracking

## Recommendation

✅ **Implement Phase 1 (Core P2P Networking) NOW**
- Minimal effort for substantial value
- Enables decentralized package announcements
- Provides foundation for future expansion

🔄 **Defer Phase 2 (Content Distribution) for v2.4.0**
- More complex, requires HTTP server + transfer protocol
- Can use IPFS integration as alternative

❌ **Skip DHT-based discovery for now**
- Gossipsub provides sufficient discovery
- DHT adds complexity without immediate benefit

## Next Steps for Implementation

1. Add libp2p dependencies to `Cargo.toml`
2. Create `cli/src/domain/marketplace/network_behaviour.rs`
3. Update `P2PRegistry` in `registry.rs` with real networking
4. Write integration tests
5. Update documentation

## References

- [rust-libp2p documentation](https://docs.rs/libp2p/latest/libp2p/)
- [libp2p specs](https://github.com/libp2p/specs)
- [Kademlia DHT](https://docs.libp2p.io/concepts/fundamentals/protocols/#kad-dht)
- [Gossipsub](https://docs.libp2p.io/concepts/pubsub/overview/)
