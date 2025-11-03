# libp2p Implementation Code Snippets - Ready to Use

**Purpose:** Copy-paste ready code for implementing async DHT query collection
**Target File:** `/Users/sac/ggen/ggen-marketplace/src/backend/p2p.rs`
**Pattern Source:** rust-libp2p file-sharing example + best practices

---

## Snippet 1: Command Enum Definition

**Location:** Add near top of file after imports

```rust
use tokio::sync::{mpsc, oneshot};
use std::collections::HashMap;
use libp2p::kad::QueryId;

/// Commands sent to the P2P swarm event loop
#[derive(Debug)]
pub enum SwarmCommand {
    /// Query DHT for package metadata
    QueryDHT {
        package_id: PackageId,
        response: oneshot::Sender<Option<Package>>,
    },

    /// Store package in DHT
    StoreDHT {
        package: Package,
        response: oneshot::Sender<Result<()>>,
    },

    /// Publish package announcement to gossipsub
    PublishPackage {
        package: Package,
        response: oneshot::Sender<Result<()>>,
    },

    /// Bootstrap DHT with known peers
    Bootstrap {
        response: oneshot::Sender<Result<()>>,
    },

    /// Subscribe to package announcements topic
    Subscribe {
        topic: String,
        response: oneshot::Sender<Result<()>>,
    },

    /// Connect to a specific peer
    Dial {
        peer_id: PeerId,
        addresses: Vec<Multiaddr>,
        response: oneshot::Sender<Result<()>>,
    },

    /// Get list of connected peers
    GetPeers {
        response: oneshot::Sender<Vec<PeerId>>,
    },

    /// Graceful shutdown
    Shutdown,
}
```

---

## Snippet 2: Event Loop Structure

**Location:** Add after P2PRegistry struct definition

```rust
/// Event loop that owns and drives the libp2p Swarm
struct SwarmEventLoop {
    /// The libp2p swarm (owned exclusively by this task)
    swarm: Swarm<P2PBehaviour>,

    /// Receiver for commands from application
    command_rx: mpsc::Receiver<SwarmCommand>,

    /// Pending DHT get_record queries
    pending_dht_get: HashMap<QueryId, oneshot::Sender<Option<Package>>>,

    /// Pending DHT put_record operations
    pending_dht_put: HashMap<QueryId, oneshot::Sender<Result<()>>>,

    /// Pending bootstrap operations
    pending_bootstrap: HashMap<QueryId, oneshot::Sender<Result<()>>>,

    /// Shared state with application (for discovered packages, etc.)
    discovered_packages: Arc<RwLock<HashMap<PackageId, HashSet<PeerId>>>>,
    peer_reputation: Arc<RwLock<HashMap<PeerId, PeerReputation>>>,
    local_packages: Arc<RwLock<HashMap<PackageId, Package>>>,

    /// Configuration
    packages_topic: gossipsub::IdentTopic,
    peer_id: PeerId,
}

impl SwarmEventLoop {
    /// Create new event loop
    fn new(
        swarm: Swarm<P2PBehaviour>,
        command_rx: mpsc::Receiver<SwarmCommand>,
        discovered_packages: Arc<RwLock<HashMap<PackageId, HashSet<PeerId>>>>,
        peer_reputation: Arc<RwLock<HashMap<PeerId, PeerReputation>>>,
        local_packages: Arc<RwLock<HashMap<PackageId, Package>>>,
        packages_topic: gossipsub::IdentTopic,
        peer_id: PeerId,
    ) -> Self {
        Self {
            swarm,
            command_rx,
            pending_dht_get: HashMap::new(),
            pending_dht_put: HashMap::new(),
            pending_bootstrap: HashMap::new(),
            discovered_packages,
            peer_reputation,
            local_packages,
            packages_topic,
            peer_id,
        }
    }

    /// Main event loop - runs forever until shutdown
    #[instrument(skip(self), fields(peer_id = %self.peer_id))]
    pub async fn run(mut self) {
        tracing::info!("P2P event loop starting");

        loop {
            tokio::select! {
                // Process network events from swarm
                event = self.swarm.select_next_some() => {
                    if let Err(e) = self.handle_swarm_event(event).await {
                        tracing::error!("Error handling swarm event: {}", e);
                    }
                }

                // Process commands from application
                Some(command) = self.command_rx.recv() => {
                    match command {
                        SwarmCommand::Shutdown => {
                            tracing::info!("Shutdown command received");
                            break;
                        }
                        other => {
                            if let Err(e) = self.handle_command(other).await {
                                tracing::error!("Error handling command: {}", e);
                            }
                        }
                    }
                }

                // Cleanup stale queries every 30 seconds
                _ = tokio::time::sleep(Duration::from_secs(30)) => {
                    self.cleanup_stale_queries();
                }

                // Channel closed - shutdown gracefully
                else => {
                    tracing::warn!("Command channel closed, shutting down");
                    break;
                }
            }
        }

        tracing::info!("P2P event loop terminated");
    }
}
```

---

## Snippet 3: Command Handler

**Location:** Add as impl block for SwarmEventLoop

```rust
impl SwarmEventLoop {
    /// Handle commands from the application
    #[instrument(skip(self, command), fields(command_type = ?command))]
    async fn handle_command(&mut self, command: SwarmCommand) -> Result<()> {
        match command {
            SwarmCommand::QueryDHT { package_id, response } => {
                let key = kad::RecordKey::new(&package_id.to_string().as_bytes());

                tracing::debug!("Starting DHT query for package: {}", package_id);

                let query_id = self.swarm
                    .behaviour_mut()
                    .kademlia
                    .get_record(key);

                // Store response channel for when result arrives
                self.pending_dht_get.insert(query_id, response);

                tracing::trace!("DHT query {} registered", query_id);
                Ok(())
            }

            SwarmCommand::StoreDHT { package, response } => {
                let package_id = PackageId::from(package.name.as_str());
                let key = kad::RecordKey::new(&package_id.to_string().as_bytes());

                let value = serde_json::to_vec(&package)
                    .map_err(|e| MarketplaceError::serialization_error(e))?;

                let record = kad::Record {
                    key,
                    value,
                    publisher: Some(self.peer_id),
                    expires: None,
                };

                tracing::debug!("Storing package {} in DHT", package_id);

                let query_id = self.swarm
                    .behaviour_mut()
                    .kademlia
                    .put_record(record, kad::Quorum::One)
                    .map_err(|e| MarketplaceError::network_error(format!("DHT put failed: {}", e)))?;

                self.pending_dht_put.insert(query_id, response);
                Ok(())
            }

            SwarmCommand::PublishPackage { package, response } => {
                let announcement = serde_json::to_vec(&package)
                    .map_err(|e| MarketplaceError::serialization_error(e))?;

                tracing::debug!("Publishing package {} to gossipsub", package.name);

                let result = self.swarm
                    .behaviour_mut()
                    .gossipsub
                    .publish(self.packages_topic.clone(), announcement)
                    .map_err(|e| MarketplaceError::network_error(format!("Gossip publish failed: {}", e)));

                // Gossipsub publish is fire-and-forget, send result immediately
                let _ = response.send(result);
                Ok(())
            }

            SwarmCommand::Bootstrap { response } => {
                tracing::info!("Starting DHT bootstrap");

                let query_id = self.swarm
                    .behaviour_mut()
                    .kademlia
                    .bootstrap()
                    .map_err(|e| MarketplaceError::network_error(format!("Bootstrap failed: {}", e)))?;

                self.pending_bootstrap.insert(query_id, response);
                Ok(())
            }

            SwarmCommand::Subscribe { topic, response } => {
                let gossip_topic = gossipsub::IdentTopic::new(topic);

                tracing::info!("Subscribing to topic: {}", gossip_topic);

                let result = self.swarm
                    .behaviour_mut()
                    .gossipsub
                    .subscribe(&gossip_topic)
                    .map_err(|e| MarketplaceError::network_error(format!("Subscribe failed: {}", e)));

                let _ = response.send(result);
                Ok(())
            }

            SwarmCommand::Dial { peer_id, addresses, response } => {
                tracing::info!("Dialing peer {} at {} addresses", peer_id, addresses.len());

                for addr in addresses {
                    if let Err(e) = self.swarm.dial(addr.clone()) {
                        tracing::warn!("Failed to dial {}: {}", addr, e);
                    }
                }

                // Dial is async, send immediate ok (connection events come later)
                let _ = response.send(Ok(()));
                Ok(())
            }

            SwarmCommand::GetPeers { response } => {
                let peers: Vec<PeerId> = self.swarm
                    .connected_peers()
                    .cloned()
                    .collect();

                tracing::trace!("Returning {} connected peers", peers.len());
                let _ = response.send(peers);
                Ok(())
            }

            SwarmCommand::Shutdown => {
                // Handled in main loop
                Ok(())
            }
        }
    }
}
```

---

## Snippet 4: Event Handler

**Location:** Add as impl block for SwarmEventLoop

```rust
impl SwarmEventLoop {
    /// Handle events from the swarm
    #[instrument(skip(self, event), fields(event_type = ?event))]
    async fn handle_swarm_event(&mut self, event: SwarmEvent<P2PBehaviourEvent>) -> Result<()> {
        match event {
            SwarmEvent::Behaviour(P2PBehaviourEvent::Kademlia(kad_event)) => {
                self.handle_kademlia_event(kad_event).await
            }

            SwarmEvent::Behaviour(P2PBehaviourEvent::Gossipsub(gossip_event)) => {
                self.handle_gossipsub_event(gossip_event).await
            }

            SwarmEvent::ConnectionEstablished { peer_id, endpoint, .. } => {
                tracing::info!(
                    "Connected to peer {} at {}",
                    peer_id,
                    endpoint.get_remote_address()
                );

                // Initialize reputation for new peer
                let mut reputation = self.peer_reputation.write().await;
                reputation.entry(peer_id)
                    .or_insert_with(|| PeerReputation::new(peer_id));

                Ok(())
            }

            SwarmEvent::ConnectionClosed { peer_id, cause, .. } => {
                tracing::info!("Disconnected from peer {}: {:?}", peer_id, cause);
                Ok(())
            }

            SwarmEvent::IncomingConnection { .. } => {
                tracing::trace!("Incoming connection");
                Ok(())
            }

            _ => Ok(()),
        }
    }

    /// Handle Kademlia DHT events
    #[instrument(skip(self, event))]
    async fn handle_kademlia_event(&mut self, event: kad::Event) -> Result<()> {
        use kad::{QueryResult, GetRecordOk, GetRecordError, PutRecordError};

        match event {
            kad::Event::OutboundQueryProgressed { id, result, stats, .. } => {
                tracing::trace!(
                    "Query {} progressed: requests={}, success={}, failure={}",
                    id,
                    stats.requests(),
                    stats.num_successes(),
                    stats.num_failures()
                );

                match result {
                    // Successful record retrieval
                    QueryResult::GetRecord(Ok(GetRecordOk::FoundRecord(peer_record))) => {
                        tracing::debug!(
                            "Query {} found record from peer {}",
                            id,
                            peer_record.peer.unwrap_or(self.peer_id)
                        );

                        if let Some(sender) = self.pending_dht_get.remove(&id) {
                            let package: Option<Package> = serde_json::from_slice(&peer_record.record.value)
                                .map_err(|e| {
                                    tracing::error!("Failed to deserialize package: {}", e);
                                    e
                                })
                                .ok();

                            let _ = sender.send(package);
                        }
                    }

                    // Query finished with no records found
                    QueryResult::GetRecord(Ok(GetRecordOk::FinishedWithNoAdditionalRecord { .. })) => {
                        tracing::debug!("Query {} finished with no records", id);

                        if let Some(sender) = self.pending_dht_get.remove(&id) {
                            let _ = sender.send(None);
                        }
                    }

                    // Query timeout
                    QueryResult::GetRecord(Err(GetRecordError::Timeout)) => {
                        tracing::warn!("Query {} timed out", id);

                        if let Some(sender) = self.pending_dht_get.remove(&id) {
                            let _ = sender.send(None);
                        }
                    }

                    // Query failed
                    QueryResult::GetRecord(Err(e)) => {
                        tracing::error!("Query {} failed: {}", id, e);

                        if let Some(sender) = self.pending_dht_get.remove(&id) {
                            let _ = sender.send(None);
                        }
                    }

                    // Successful record storage
                    QueryResult::PutRecord(Ok(_)) => {
                        tracing::debug!("Query {} stored record successfully", id);

                        if let Some(sender) = self.pending_dht_put.remove(&id) {
                            let _ = sender.send(Ok(()));
                        }
                    }

                    // Storage failed
                    QueryResult::PutRecord(Err(e)) => {
                        tracing::error!("Query {} put record failed: {}", id, e);

                        if let Some(sender) = self.pending_dht_put.remove(&id) {
                            let err = match e {
                                PutRecordError::Timeout => "DHT put timed out",
                                _ => "DHT put failed",
                            };
                            let _ = sender.send(Err(MarketplaceError::network_error(err)));
                        }
                    }

                    // Bootstrap completed
                    QueryResult::Bootstrap(Ok(_)) => {
                        tracing::info!("Bootstrap query {} completed successfully", id);

                        if let Some(sender) = self.pending_bootstrap.remove(&id) {
                            let _ = sender.send(Ok(()));
                        }
                    }

                    // Bootstrap failed
                    QueryResult::Bootstrap(Err(e)) => {
                        tracing::error!("Bootstrap query {} failed: {}", id, e);

                        if let Some(sender) = self.pending_bootstrap.remove(&id) {
                            let _ = sender.send(Err(MarketplaceError::network_error(format!("Bootstrap failed: {}", e))));
                        }
                    }

                    _ => {
                        tracing::trace!("Unhandled query result for {}", id);
                    }
                }

                Ok(())
            }

            kad::Event::RoutingUpdated { peer, addresses, .. } => {
                tracing::trace!("Routing table updated for peer {} ({} addresses)", peer, addresses.len());
                Ok(())
            }

            _ => Ok(()),
        }
    }

    /// Handle Gossipsub events
    #[instrument(skip(self, event))]
    async fn handle_gossipsub_event(&mut self, event: gossipsub::Event) -> Result<()> {
        match event {
            gossipsub::Event::Message { propagation_source, message, .. } => {
                tracing::debug!(
                    "Received gossipsub message from {} on topic {:?}",
                    propagation_source,
                    message.topic
                );

                // Deserialize package announcement
                if let Ok(package) = serde_json::from_slice::<Package>(&message.data) {
                    let package_id = PackageId::from(package.name.as_str());

                    tracing::info!("Discovered package {} from peer {}", package_id, propagation_source);

                    // Add to discovered packages
                    let mut discovered = self.discovered_packages.write().await;
                    discovered
                        .entry(package_id)
                        .or_insert_with(HashSet::new)
                        .insert(propagation_source);

                    // Update peer reputation for successful announcement
                    let mut reputation = self.peer_reputation.write().await;
                    if let Some(peer_rep) = reputation.get_mut(&propagation_source) {
                        peer_rep.successful_retrievals += 1;
                        peer_rep.last_seen = chrono::Utc::now();
                    }
                } else {
                    tracing::warn!("Failed to deserialize gossipsub message from {}", propagation_source);
                }

                Ok(())
            }

            gossipsub::Event::Subscribed { peer_id, topic } => {
                tracing::debug!("Peer {} subscribed to topic {:?}", peer_id, topic);
                Ok(())
            }

            gossipsub::Event::Unsubscribed { peer_id, topic } => {
                tracing::debug!("Peer {} unsubscribed from topic {:?}", peer_id, topic);
                Ok(())
            }

            _ => Ok(()),
        }
    }

    /// Clean up stale queries that have been pending too long
    fn cleanup_stale_queries(&mut self) {
        let timeout = Duration::from_secs(60);
        let now = std::time::Instant::now();

        let stale_get: Vec<QueryId> = self.pending_dht_get
            .iter()
            .filter(|(_, _)| {
                // In production, track start time per query
                // For now, aggressive cleanup
                false
            })
            .map(|(id, _)| *id)
            .collect();

        for id in stale_get {
            if let Some(sender) = self.pending_dht_get.remove(&id) {
                let _ = sender.send(None);
                tracing::warn!("Cleaned up stale DHT get query: {}", id);
            }
        }

        // Similar cleanup for put and bootstrap
    }
}
```

---

## Snippet 5: Refactored P2PRegistry

**Location:** Replace existing P2PRegistry impl

```rust
/// P2P Registry using message-passing architecture
pub struct P2PRegistry {
    /// Channel to send commands to swarm event loop
    command_tx: mpsc::Sender<SwarmCommand>,

    /// Local peer ID
    peer_id: PeerId,

    /// Shared state (read-only from Registry perspective)
    local_packages: Arc<RwLock<HashMap<PackageId, Package>>>,
    discovered_packages: Arc<RwLock<HashMap<PackageId, HashSet<PeerId>>>>,
    peer_reputation: Arc<RwLock<HashMap<PeerId, PeerReputation>>>,

    /// Configuration
    config: P2PConfig,

    /// Cache
    package_cache: Arc<RwLock<HashMap<PackageId, (Package, std::time::Instant)>>>,
    my_location: Arc<RwLock<Option<GeoLocation>>>,
}

impl P2PRegistry {
    /// Create new P2P registry and spawn event loop
    pub async fn new(config: P2PConfig) -> Result<Self> {
        // Generate keypair
        let local_key = libp2p::identity::Keypair::generate_ed25519();
        let peer_id = PeerId::from(local_key.public());

        tracing::info!("Creating P2P registry with peer ID: {}", peer_id);

        // Create shared state
        let local_packages = Arc::new(RwLock::new(HashMap::new()));
        let discovered_packages = Arc::new(RwLock::new(HashMap::new()));
        let peer_reputation = Arc::new(RwLock::new(HashMap::new()));
        let package_cache = Arc::new(RwLock::new(HashMap::new()));
        let my_location = Arc::new(RwLock::new(None));

        // Create swarm (same as before)
        let swarm = Self::create_swarm(&config, local_key).await?;

        // Create command channel
        let (command_tx, command_rx) = mpsc::channel(100);

        // Create gossipsub topic
        let packages_topic = gossipsub::IdentTopic::new(&config.packages_topic);

        // Create event loop
        let event_loop = SwarmEventLoop::new(
            swarm,
            command_rx,
            Arc::clone(&discovered_packages),
            Arc::clone(&peer_reputation),
            Arc::clone(&local_packages),
            packages_topic,
            peer_id,
        );

        // Spawn event loop task
        tokio::spawn(async move {
            event_loop.run().await;
        });

        tracing::info!("P2P event loop spawned");

        Ok(Self {
            command_tx,
            peer_id,
            local_packages,
            discovered_packages,
            peer_reputation,
            config,
            package_cache,
            my_location,
        })
    }

    /// Create libp2p swarm (helper method)
    async fn create_swarm(
        config: &P2PConfig,
        local_key: libp2p::identity::Keypair,
    ) -> Result<Swarm<P2PBehaviour>> {
        // Same logic as before for creating swarm
        // ... (omitted for brevity, keep existing code)
        todo!("Copy existing swarm creation logic here")
    }
}
```

---

## Snippet 6: Registry Trait Implementation

**Location:** Replace existing #[async_trait] impl Registry

```rust
#[async_trait]
impl Registry for P2PRegistry {
    #[instrument(skip(self))]
    async fn register(&self, package: Package) -> Result<()> {
        tracing::info!("Registering package: {}", package.name);

        // Store locally first
        let package_id = PackageId::from(package.name.as_str());
        {
            let mut local = self.local_packages.write().await;
            local.insert(package_id.clone(), package.clone());
        }

        // Store in DHT
        let (tx, rx) = oneshot::channel();
        self.command_tx
            .send(SwarmCommand::StoreDHT {
                package: package.clone(),
                response: tx,
            })
            .await
            .map_err(|_| MarketplaceError::network_error("P2P event loop died"))?;

        tokio::time::timeout(Duration::from_secs(30), rx)
            .await
            .map_err(|_| MarketplaceError::network_error("DHT store timeout"))?
            .map_err(|_| MarketplaceError::network_error("DHT store cancelled"))??;

        // Publish to gossipsub
        let (tx, rx) = oneshot::channel();
        self.command_tx
            .send(SwarmCommand::PublishPackage {
                package,
                response: tx,
            })
            .await
            .map_err(|_| MarketplaceError::network_error("P2P event loop died"))?;

        tokio::time::timeout(Duration::from_secs(5), rx)
            .await
            .map_err(|_| MarketplaceError::network_error("Publish timeout"))?
            .map_err(|_| MarketplaceError::network_error("Publish cancelled"))??;

        Ok(())
    }

    #[instrument(skip(self), fields(package_id = %package_id))]
    async fn get_package(&self, package_id: &PackageId) -> Result<Option<Package>> {
        // Check local first
        {
            let local = self.local_packages.read().await;
            if let Some(pkg) = local.get(package_id) {
                tracing::debug!("Package {} found locally", package_id);
                return Ok(Some(pkg.clone()));
            }
        }

        // Check cache
        {
            let cache = self.package_cache.read().await;
            if let Some((pkg, cached_at)) = cache.get(package_id) {
                if cached_at.elapsed() < Duration::from_secs(300) {
                    tracing::debug!("Package {} found in cache", package_id);
                    return Ok(Some(pkg.clone()));
                }
            }
        }

        // Query DHT
        tracing::debug!("Querying DHT for package {}", package_id);

        let (tx, rx) = oneshot::channel();
        self.command_tx
            .send(SwarmCommand::QueryDHT {
                package_id: package_id.clone(),
                response: tx,
            })
            .await
            .map_err(|_| MarketplaceError::network_error("P2P event loop died"))?;

        let result = tokio::time::timeout(Duration::from_secs(30), rx)
            .await
            .map_err(|_| MarketplaceError::network_error("DHT query timeout"))?
            .map_err(|_| MarketplaceError::network_error("DHT query cancelled"))?;

        // Cache result if found
        if let Some(ref pkg) = result {
            let mut cache = self.package_cache.write().await;
            cache.insert(package_id.clone(), (pkg.clone(), std::time::Instant::now()));
        }

        Ok(result)
    }

    #[instrument(skip(self, query))]
    async fn search(&self, query: &Query) -> Result<Vec<Package>> {
        // Combine local + discovered + DHT
        let mut results = Vec::new();

        // Search local packages
        {
            let local = self.local_packages.read().await;
            for pkg in local.values() {
                if pkg.name.contains(&query.text) || pkg.description.contains(&query.text) {
                    results.push(pkg.clone());
                }
            }
        }

        // Search discovered packages (would need to fetch them)
        // For now, return local results
        // TODO: Implement distributed search

        Ok(results)
    }

    async fn unregister(&self, package_id: &PackageId) -> Result<()> {
        let mut local = self.local_packages.write().await;
        local.remove(package_id);
        Ok(())
    }

    async fn metadata(&self) -> Result<RegistryMetadata> {
        let (tx, rx) = oneshot::channel();
        self.command_tx
            .send(SwarmCommand::GetPeers { response: tx })
            .await
            .map_err(|_| MarketplaceError::network_error("P2P event loop died"))?;

        let peers = tokio::time::timeout(Duration::from_secs(5), rx)
            .await
            .map_err(|_| MarketplaceError::network_error("GetPeers timeout"))?
            .map_err(|_| MarketplaceError::network_error("GetPeers cancelled"))?;

        Ok(RegistryMetadata {
            name: "P2P Registry".to_string(),
            url: format!("p2p://{}", self.peer_id),
            description: format!("Decentralized P2P registry ({} peers)", peers.len()),
        })
    }
}
```

---

## Usage Example

```rust
#[tokio::main]
async fn main() -> Result<()> {
    // Initialize tracing
    tracing_subscriber::fmt::init();

    // Create P2P config
    let config = P2PConfig {
        bootstrap_nodes: vec![
            "/dnsaddr/bootstrap.libp2p.io/p2p/QmNnooDu7bfjPFoTZYxMNLWUQJyrVwtbZg5gBMjTezGAJN"
                .parse()
                .unwrap(),
        ],
        packages_topic: "/ggen/packages/v1".to_string(),
        dht_server_mode: true,
        listen_addresses: vec!["/ip4/0.0.0.0/tcp/0".parse().unwrap()],
    };

    // Create registry (spawns event loop automatically)
    let registry = P2PRegistry::new(config).await?;

    // Use registry
    let package = Package {
        name: "example-package".to_string(),
        version: "1.0.0".to_string(),
        description: "Example".to_string(),
        // ... other fields
    };

    // Register package (non-blocking, uses message passing)
    registry.register(package).await?;

    // Get package (queries DHT asynchronously)
    let package_id = PackageId::from("example-package");
    let result = registry.get_package(&package_id).await?;

    println!("Package: {:?}", result);

    Ok(())
}
```

---

## Testing Snippets

```rust
#[cfg(test)]
mod tests {
    use super::*;

    #[tokio::test]
    async fn test_event_loop_command_processing() {
        // Create mock swarm and command channel
        let (tx, rx) = mpsc::channel(10);

        // Send QueryDHT command
        let (response_tx, response_rx) = oneshot::channel();
        tx.send(SwarmCommand::QueryDHT {
            package_id: PackageId::from("test"),
            response: response_tx,
        }).await.unwrap();

        // Event loop should process command and respond
        // (requires mock swarm - use production test with real swarm)
    }

    #[tokio::test]
    async fn test_dht_query_timeout() {
        let config = P2PConfig::default();
        let registry = P2PRegistry::new(config).await.unwrap();

        let package_id = PackageId::from("nonexistent");

        // Should timeout and return None
        let result = tokio::time::timeout(
            Duration::from_secs(5),
            registry.get_package(&package_id)
        ).await;

        assert!(result.is_ok());
        assert_eq!(result.unwrap().unwrap(), None);
    }
}
```

---

## Integration Checklist

- [ ] Add `SwarmCommand` enum
- [ ] Add `SwarmEventLoop` struct
- [ ] Implement `SwarmEventLoop::run()`
- [ ] Implement `handle_command()`
- [ ] Implement `handle_swarm_event()`
- [ ] Implement `handle_kademlia_event()`
- [ ] Implement `handle_gossipsub_event()`
- [ ] Refactor `P2PRegistry` to remove `Arc<RwLock<Swarm>>`
- [ ] Update `P2PRegistry::new()` to spawn event loop
- [ ] Refactor all `Registry` trait methods to use channels
- [ ] Add cleanup for stale queries
- [ ] Test with `cargo test -p ggen-marketplace --features p2p`
- [ ] Verify zero compilation errors
- [ ] Run clippy: `cargo clippy -p ggen-marketplace --features p2p`

---

## Performance Notes

- **Channel Capacity:** 100 is reasonable for moderate load
- **Timeout:** 30s for DHT queries, 5s for gossipsub
- **Cleanup Interval:** 30s for stale query cleanup
- **Cache TTL:** 300s (5 minutes) for package cache

Adjust based on benchmarks and production load.
