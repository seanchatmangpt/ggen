//! P2P Registry with complete DHT event handling

use crate::error::MarketplaceError;
use crate::models::{Package, PackageId, Query, RegistryMetadata};
use crate::traits::Registry;
use async_trait::async_trait;
use futures::StreamExt;
use libp2p::{
    gossipsub, identify, kad,
    swarm::{NetworkBehaviour, SwarmEvent},
    Multiaddr, PeerId, Swarm,
};
use std::collections::{HashMap, HashSet};
use std::sync::Arc;
use std::time::Instant;
use tokio::sync::{mpsc, oneshot, RwLock};
use tokio::time::{timeout, Duration};
use tracing::{debug, info, instrument, warn, Span};

#[derive(NetworkBehaviour)]
pub struct P2PBehaviour {
    pub kademlia: kad::Behaviour<kad::store::MemoryStore>,
    pub gossipsub: gossipsub::Behaviour,
    pub identify: identify::Behaviour,
}

#[derive(Debug, Clone)]
pub struct P2PConfig {
    pub bootstrap_nodes: Vec<Multiaddr>,
    pub packages_topic: String,
    pub dht_server_mode: bool,
    pub listen_addresses: Vec<Multiaddr>,
    pub query_timeout_secs: u64,
}

impl Default for P2PConfig {
    fn default() -> Self {
        let default_addr = "/ip4/0.0.0.0/tcp/0"
            .parse()
            .unwrap_or_else(|_| "/ip4/127.0.0.1/tcp/0".parse().unwrap());
        Self {
            bootstrap_nodes: Vec::new(),
            packages_topic: "/ggen/packages/v1".to_string(),
            dht_server_mode: true,
            listen_addresses: vec![default_addr],
            query_timeout_secs: 30,
        }
    }
}

#[derive(Debug, Clone)]
pub struct GeoLocation {
    pub latitude: f64,
    pub longitude: f64,
    pub region: Option<String>,
}

impl GeoLocation {
    pub fn distance_km(&self, other: &GeoLocation) -> f64 {
        const EARTH_RADIUS_KM: f64 = 6371.0;
        let lat1 = self.latitude.to_radians();
        let lat2 = other.latitude.to_radians();
        let delta_lat = (self.latitude - other.latitude).to_radians();
        let delta_lon = (self.longitude - other.longitude).to_radians();
        let a = (delta_lat / 2.0).sin().powi(2)
            + lat1.cos() * lat2.cos() * (delta_lon / 2.0).sin().powi(2);
        let c = 2.0 * a.sqrt().asin();
        EARTH_RADIUS_KM * c
    }
}

#[derive(Debug, Clone)]
struct PeerReputation {
    peer_id: PeerId,
    successful_retrievals: u64,
    failed_retrievals: u64,
    last_seen: chrono::DateTime<chrono::Utc>,
    avg_response_time_ms: u64,
    location: Option<GeoLocation>,
    packages_provided: usize,
}

impl PeerReputation {
    fn new(peer_id: PeerId) -> Self {
        Self {
            peer_id,
            successful_retrievals: 0,
            failed_retrievals: 0,
            last_seen: chrono::Utc::now(),
            avg_response_time_ms: 0,
            location: None,
            packages_provided: 0,
        }
    }

    fn success_rate(&self) -> f64 {
        let total = self.successful_retrievals + self.failed_retrievals;
        if total == 0 {
            return 1.0;
        }
        self.successful_retrievals as f64 / total as f64
    }

    pub fn reputation_score(&self, my_location: Option<&GeoLocation>) -> f64 {
        let success_rate = self.success_rate();
        let response_time_score = if self.avg_response_time_ms == 0 {
            1.0
        } else {
            (1000.0 / (self.avg_response_time_ms as f64 + 1.0)).min(1.0)
        };
        let availability_score = (self.packages_provided as f64 / 10.0).min(1.0);
        let hours_since_seen = chrono::Utc::now()
            .signed_duration_since(self.last_seen)
            .num_hours() as f64;
        let recency_score = (1.0 / (hours_since_seen + 1.0)).min(1.0);
        let geo_bonus =
            if let (Some(my_loc), Some(peer_loc)) = (my_location, self.location.as_ref()) {
                let distance_km = my_loc.distance_km(peer_loc);
                if distance_km < 100.0 {
                    0.1 * (1.0 - distance_km / 100.0)
                } else {
                    0.0
                }
            } else {
                0.0
            };
        let base_score = 0.5 * success_rate
            + 0.25 * response_time_score
            + 0.15 * availability_score
            + 0.10 * recency_score;
        (base_score + geo_bonus).min(1.0)
    }

    pub fn record_response_time(&mut self, response_time_ms: u64) {
        if self.avg_response_time_ms == 0 {
            self.avg_response_time_ms = response_time_ms;
        } else {
            self.avg_response_time_ms =
                ((0.7 * self.avg_response_time_ms as f64) + (0.3 * response_time_ms as f64)) as u64;
        }
    }

    pub fn update_location(&mut self, location: GeoLocation) {
        self.location = Some(location);
    }
}

type QueryResultSender = oneshot::Sender<std::result::Result<Option<Package>, MarketplaceError>>;

#[derive(Debug)]
struct ActiveQuery {
    package_id: PackageId,
    started_at: Instant,
    result_sender: QueryResultSender,
    collected_results: Vec<Package>,
}

enum EventLoopCommand {
    Shutdown,
}

pub struct P2PRegistry {
    swarm: Arc<RwLock<Swarm<P2PBehaviour>>>,
    peer_id: PeerId,
    local_packages: Arc<RwLock<HashMap<PackageId, Package>>>,
    discovered_packages: Arc<RwLock<HashMap<PackageId, HashSet<PeerId>>>>,
    peer_reputation: Arc<RwLock<HashMap<PeerId, PeerReputation>>>,
    packages_topic: gossipsub::IdentTopic,
    config: P2PConfig,
    my_location: Arc<RwLock<Option<GeoLocation>>>,
    package_cache: Arc<RwLock<HashMap<PackageId, (Package, std::time::Instant)>>>,
    active_queries: Arc<RwLock<HashMap<kad::QueryId, ActiveQuery>>>,
    event_tx: mpsc::UnboundedSender<EventLoopCommand>,
}

impl P2PRegistry {
    pub async fn new(config: P2PConfig) -> std::result::Result<Self, MarketplaceError> {
        let local_key = libp2p::identity::Keypair::generate_ed25519();
        let peer_id = PeerId::from(local_key.public());
        let store = kad::store::MemoryStore::new(peer_id);
        let mut kademlia = kad::Behaviour::new(peer_id, store);
        if config.dht_server_mode {
            kademlia.set_mode(Some(kad::Mode::Server));
        }
        for addr in &config.bootstrap_nodes {
            let _ = kademlia.add_address(&peer_id, addr.clone());
        }
        let gossipsub_config = gossipsub::ConfigBuilder::default()
            .heartbeat_interval(std::time::Duration::from_secs(10))
            .validation_mode(gossipsub::ValidationMode::Strict)
            .build()
            .map_err(|e| {
                MarketplaceError::network_error(format!("Gossipsub config error: {}", e))
            })?;
        let gossipsub = gossipsub::Behaviour::new(
            gossipsub::MessageAuthenticity::Signed(local_key.clone()),
            gossipsub_config,
        )
        .map_err(|e| {
            MarketplaceError::network_error(format!("Failed to create gossipsub: {}", e))
        })?;
        let identify = identify::Behaviour::new(identify::Config::new(
            "/ggen/1.0.0".to_string(),
            local_key.public(),
        ));
        let behaviour = P2PBehaviour {
            kademlia,
            gossipsub,
            identify,
        };
        let swarm = libp2p::SwarmBuilder::with_existing_identity(local_key)
            .with_tokio()
            .with_tcp(
                libp2p::tcp::Config::default(),
                libp2p::noise::Config::new,
                libp2p::yamux::Config::default,
            )
            .map_err(|e| {
                MarketplaceError::network_error(format!("Failed to configure TCP: {}", e))
            })?
            .with_behaviour(|_| behaviour)
            .map_err(|e| {
                MarketplaceError::network_error(format!("Failed to create behavior: {}", e))
            })?
            .build();
        let packages_topic = gossipsub::IdentTopic::new(&config.packages_topic);
        let (event_tx, event_rx) = mpsc::unbounded_channel();
        let registry = Self {
            swarm: Arc::new(RwLock::new(swarm)),
            peer_id,
            local_packages: Arc::new(RwLock::new(HashMap::new())),
            discovered_packages: Arc::new(RwLock::new(HashMap::new())),
            peer_reputation: Arc::new(RwLock::new(HashMap::new())),
            packages_topic,
            config,
            my_location: Arc::new(RwLock::new(None)),
            package_cache: Arc::new(RwLock::new(HashMap::new())),
            active_queries: Arc::new(RwLock::new(HashMap::new())),
            event_tx,
        };
        registry.spawn_event_loop(event_rx);
        Ok(registry)
    }

    fn spawn_event_loop(&self, mut event_rx: mpsc::UnboundedReceiver<EventLoopCommand>) {
        let swarm = Arc::clone(&self.swarm);
        let active_queries = Arc::clone(&self.active_queries);
        let peer_reputation = Arc::clone(&self.peer_reputation);
        let discovered_packages = Arc::clone(&self.discovered_packages);
        let local_packages = Arc::clone(&self.local_packages);
        let packages_topic = self.packages_topic.clone();

        tokio::spawn(async move {
            loop {
                tokio::select! {
                    cmd = event_rx.recv() => {
                        match cmd {
                            Some(EventLoopCommand::Shutdown) | None => {
                                info!("P2P event loop shutting down");
                                break;
                            }
                        }
                    }
                    _ = async {
                        let mut swarm_lock = swarm.write().await;
                        if let Some(event) = swarm_lock.next().await {
                            drop(swarm_lock);
                            Self::handle_swarm_event(
                                event,
                                Arc::clone(&active_queries),
                                Arc::clone(&peer_reputation),
                                Arc::clone(&discovered_packages),
                                Arc::clone(&local_packages),
                                &packages_topic,
                            ).await;
                        }
                    } => {}
                }
            }
        });
    }

    async fn handle_swarm_event(
        event: SwarmEvent<P2PBehaviourEvent>,
        active_queries: Arc<RwLock<HashMap<kad::QueryId, ActiveQuery>>>,
        peer_reputation: Arc<RwLock<HashMap<PeerId, PeerReputation>>>,
        discovered_packages: Arc<RwLock<HashMap<PackageId, HashSet<PeerId>>>>,
        local_packages: Arc<RwLock<HashMap<PackageId, Package>>>,
        packages_topic: &gossipsub::IdentTopic,
    ) {
        match event {
            SwarmEvent::Behaviour(P2PBehaviourEvent::Kademlia(kad_event)) => match kad_event {
                kad::Event::OutboundQueryProgressed { id, result, .. } => match result {
                    kad::QueryResult::GetRecord(Ok(kad::GetRecordOk::FoundRecord(record))) => {
                        if let Ok(package) = serde_json::from_slice::<Package>(&record.record.value)
                        {
                            let mut queries = active_queries.write().await;
                            if let Some(query) = queries.get_mut(&id) {
                                debug!("DHT: Found package {} for query {:?}", package.id, id);
                                query.collected_results.push(package.clone());
                                if let Some(peer_id) = record.record.publisher {
                                    let mut discovered = discovered_packages.write().await;
                                    discovered
                                        .entry(package.id.clone())
                                        .or_insert_with(HashSet::new)
                                        .insert(peer_id);
                                    let mut reputation = peer_reputation.write().await;
                                    let entry = reputation
                                        .entry(peer_id)
                                        .or_insert_with(|| PeerReputation::new(peer_id));
                                    entry.packages_provided += 1;
                                    entry.last_seen = chrono::Utc::now();
                                }
                            }
                        }
                    }
                    kad::QueryResult::GetRecord(Err(_)) => {
                        let mut queries = active_queries.write().await;
                        if let Some(query) = queries.remove(&id) {
                            let result = if !query.collected_results.is_empty() {
                                Ok(Some(query.collected_results[0].clone()))
                            } else {
                                Ok(None)
                            };
                            let _ = query.result_sender.send(result);
                        }
                    }
                    kad::QueryResult::GetProviders(Ok(kad::GetProvidersOk::FoundProviders {
                        providers,
                        ..
                    })) => {
                        let queries = active_queries.write().await;
                        if let Some(query) = queries.get(&id) {
                            let mut discovered = discovered_packages.write().await;
                            let entry = discovered
                                .entry(query.package_id.clone())
                                .or_insert_with(HashSet::new);
                            for peer_id in providers {
                                entry.insert(peer_id);
                            }
                        }
                    }
                    _ => {}
                },
                kad::Event::RoutingUpdated { peer, .. } => {
                    let mut reputation = peer_reputation.write().await;
                    reputation
                        .entry(peer)
                        .or_insert_with(|| PeerReputation::new(peer));
                }
                _ => {}
            },
            SwarmEvent::Behaviour(P2PBehaviourEvent::Gossipsub(gossipsub::Event::Message {
                propagation_source: peer_id,
                message,
                ..
            })) => {
                if message.topic == packages_topic.hash() {
                    match serde_json::from_slice::<Package>(&message.data) {
                        Ok(package) => {
                            info!(
                                "Received package announcement: {} from peer {}",
                                package.id, peer_id
                            );
                            let mut discovered = discovered_packages.write().await;
                            discovered
                                .entry(package.id.clone())
                                .or_insert_with(HashSet::new)
                                .insert(peer_id);
                            let mut local = local_packages.write().await;
                            local.insert(package.id.clone(), package);
                            let mut reputation = peer_reputation.write().await;
                            let entry = reputation
                                .entry(peer_id)
                                .or_insert_with(|| PeerReputation::new(peer_id));
                            entry.packages_provided += 1;
                            entry.last_seen = chrono::Utc::now();
                        }
                        Err(e) => {
                            warn!("Failed to deserialize package announcement: {}", e);
                        }
                    }
                }
            }
            SwarmEvent::ConnectionEstablished { peer_id, .. } => {
                info!("Connection established with peer: {}", peer_id);
                let mut reputation = peer_reputation.write().await;
                reputation
                    .entry(peer_id)
                    .or_insert_with(|| PeerReputation::new(peer_id))
                    .last_seen = chrono::Utc::now();
            }
            _ => {}
        }
    }

    pub async fn set_location(&self, location: GeoLocation) {
        *self.my_location.write().await = Some(location);
    }

    pub async fn get_location(&self) -> Option<GeoLocation> {
        self.my_location.read().await.clone()
    }

    pub async fn start_listening(&self) -> std::result::Result<(), MarketplaceError> {
        let mut swarm = self.swarm.write().await;
        for addr in &self.config.listen_addresses {
            swarm
                .listen_on(addr.clone())
                .map_err(|e| MarketplaceError::network_error(format!("Failed to listen: {}", e)))?;
        }
        Ok(())
    }

    pub async fn subscribe_to_packages(&self) -> std::result::Result<(), MarketplaceError> {
        let mut swarm = self.swarm.write().await;
        swarm
            .behaviour_mut()
            .gossipsub
            .subscribe(&self.packages_topic)
            .map_err(|e| MarketplaceError::network_error(format!("Failed to subscribe: {}", e)))?;
        Ok(())
    }

    #[instrument(skip(self), fields(peer_id = %self.peer_id))]
    pub async fn bootstrap(&self) -> std::result::Result<(), MarketplaceError> {
        let bootstrap_start = Instant::now();
        let mut swarm = self.swarm.write().await;
        swarm
            .behaviour_mut()
            .kademlia
            .bootstrap()
            .map_err(|e| MarketplaceError::network_error(format!("Bootstrap failed: {}", e)))?;

        let bootstrap_time_ms = bootstrap_start.elapsed().as_millis() as u64;
        Span::current().record("p2p.bootstrap.duration_ms", bootstrap_time_ms);
        Span::current().record("p2p.bootstrap.status", "success");

        Ok(())
    }

    async fn announce_package(
        &self, package: &Package,
    ) -> std::result::Result<(), MarketplaceError> {
        let announcement =
            serde_json::to_vec(package).map_err(|e| MarketplaceError::serialization_error(e))?;
        let mut swarm = self.swarm.write().await;
        swarm
            .behaviour_mut()
            .gossipsub
            .publish(self.packages_topic.clone(), announcement)
            .map_err(|e| MarketplaceError::network_error(format!("Failed to publish: {}", e)))?;
        Ok(())
    }

    async fn store_in_dht(
        &self, package_id: &PackageId, package: &Package,
    ) -> std::result::Result<(), MarketplaceError> {
        let key = kad::RecordKey::new(&package_id.to_string().as_bytes());
        let value =
            serde_json::to_vec(package).map_err(|e| MarketplaceError::serialization_error(e))?;
        let record = kad::Record {
            key,
            value,
            publisher: Some(self.peer_id),
            expires: None,
        };
        let mut swarm = self.swarm.write().await;
        swarm
            .behaviour_mut()
            .kademlia
            .put_record(record, kad::Quorum::One)
            .map_err(|e| MarketplaceError::network_error(format!("DHT put failed: {}", e)))?;
        Ok(())
    }

    #[instrument(skip(self), fields(package_id = %package_id))]
    async fn query_dht(
        &self, package_id: &PackageId,
    ) -> std::result::Result<Option<Package>, MarketplaceError> {
        let query_start = Instant::now();
        let key = kad::RecordKey::new(&package_id.to_string().as_bytes());
        let (result_tx, result_rx) = oneshot::channel();
        let query_id = {
            let mut swarm = self.swarm.write().await;
            swarm.behaviour_mut().kademlia.get_record(key)
        };
        {
            let mut queries = self.active_queries.write().await;
            queries.insert(
                query_id,
                ActiveQuery {
                    package_id: package_id.clone(),
                    started_at: Instant::now(),
                    result_sender: result_tx,
                    collected_results: Vec::new(),
                },
            );
        }
        let timeout_duration = Duration::from_secs(self.config.query_timeout_secs);
        let result = match timeout(timeout_duration, result_rx).await {
            Ok(Ok(result)) => result,
            Ok(Err(_)) => {
                self.active_queries.write().await.remove(&query_id);
                Span::current().record("p2p.dht.query.status", "error");
                Ok(None)
            }
            Err(_) => {
                let mut queries = self.active_queries.write().await;
                if let Some(query) = queries.remove(&query_id) {
                    if !query.collected_results.is_empty() {
                        Span::current().record("p2p.dht.query.status", "partial");
                        Ok(Some(query.collected_results[0].clone()))
                    } else {
                        Span::current().record("p2p.dht.query.status", "timeout");
                        Ok(None)
                    }
                } else {
                    Span::current().record("p2p.dht.query.status", "timeout");
                    Ok(None)
                }
            }
        };

        let query_time_ms = query_start.elapsed().as_millis() as u64;
        Span::current().record("p2p.dht.query.duration_ms", query_time_ms);
        if let Ok(Some(_)) = &result {
            Span::current().record("p2p.dht.query.found", true);
        } else {
            Span::current().record("p2p.dht.query.found", false);
        }

        result
    }

    #[instrument(skip(self), fields(package_id = %package_id, fan_out = fan_out))]
    async fn query_dht_parallel(
        &self, package_id: &PackageId, fan_out: usize,
    ) -> std::result::Result<Option<Package>, MarketplaceError> {
        let query_start = Instant::now();
        if fan_out <= 1 {
            return self.query_dht(package_id).await;
        }
        let mut query_futures = Vec::new();
        for i in 0..fan_out {
            let package_id = package_id.clone();
            let registry_ref = self;
            query_futures.push(async move {
                let span = tracing::Span::current();
                span.record("p2p.dht.query.parallel_query_id", i);
                registry_ref.query_dht(&package_id).await
            });
        }
        let results = futures::future::join_all(query_futures).await;
        let mut found_count = 0;
        for result in results {
            match result {
                Ok(Some(package)) => {
                    found_count += 1;
                    let query_time_ms = query_start.elapsed().as_millis() as u64;
                    Span::current().record("p2p.dht.query.parallel.duration_ms", query_time_ms);
                    Span::current().record("p2p.dht.query.parallel.found", true);
                    Span::current().record("p2p.dht.query.parallel.found_count", found_count);
                    return Ok(Some(package));
                }
                Ok(None) => continue,
                Err(_) => continue,
            }
        }

        let query_time_ms = query_start.elapsed().as_millis() as u64;
        Span::current().record("p2p.dht.query.parallel.duration_ms", query_time_ms);
        Span::current().record("p2p.dht.query.parallel.found", false);
        Span::current().record("p2p.dht.query.parallel.found_count", found_count);

        Ok(None)
    }

    pub async fn get_peer_reputation(&self, peer_id: &PeerId) -> f64 {
        let reputation = self.peer_reputation.read().await;
        reputation
            .get(peer_id)
            .map(|r| r.success_rate())
            .unwrap_or(1.0)
    }

    #[instrument(skip(self, my_location), fields(min_reputation = min_reputation, limit = limit))]
    pub async fn select_best_peers(
        &self, min_reputation: f64, limit: usize, my_location: Option<&GeoLocation>,
    ) -> Vec<(PeerId, f64)> {
        let selection_start = Instant::now();
        let reputation = self.peer_reputation.read().await;
        let mut peers: Vec<_> = reputation
            .iter()
            .map(|(peer_id, rep)| {
                let score = rep.reputation_score(my_location);
                (*peer_id, score)
            })
            .filter(|(_, score)| *score >= min_reputation)
            .collect();
        peers.sort_by(|a, b| b.1.partial_cmp(&a.1).unwrap_or(std::cmp::Ordering::Equal));
        peers.truncate(limit);

        let selection_time_ms = selection_start.elapsed().as_millis() as u64;
        Span::current().record("p2p.peer.selection.duration_ms", selection_time_ms);
        Span::current().record("p2p.peer.selection.count", peers.len());
        Span::current().record("p2p.peer.selection.filtered_count", peers.len());

        peers
    }

    pub async fn update_peer_location(&self, peer_id: PeerId, location: GeoLocation) {
        let mut reputation = self.peer_reputation.write().await;
        if let Some(rep) = reputation.get_mut(&peer_id) {
            rep.update_location(location);
        }
    }

    pub async fn shutdown(&self) -> std::result::Result<(), MarketplaceError> {
        let _ = self.event_tx.send(EventLoopCommand::Shutdown);
        Ok(())
    }
}

#[async_trait]
impl Registry for P2PRegistry {
    #[instrument(skip(self, query), fields(query = %query.text, limit = query.limit))]
    async fn search(&self, query: &Query) -> std::result::Result<Vec<Package>, MarketplaceError> {
        let local_packages = self.local_packages.read().await;
        let mut results: Vec<Package> = local_packages
            .values()
            .filter(|package| {
                let text_match = query.text.is_empty()
                    || package
                        .metadata
                        .title
                        .to_lowercase()
                        .contains(&query.text.to_lowercase())
                    || package
                        .metadata
                        .description
                        .to_lowercase()
                        .contains(&query.text.to_lowercase());
                let category_match = query.categories.is_empty()
                    || package
                        .metadata
                        .categories
                        .iter()
                        .any(|c| query.categories.contains(c));
                let tag_match = query.tags.is_empty()
                    || package.metadata.tags.iter().any(|t| query.tags.contains(t));
                text_match && category_match && tag_match
            })
            .cloned()
            .collect();
        let discovered = self.discovered_packages.read().await;
        let my_location = self.get_location().await;
        let best_peers = self.select_best_peers(0.5, 10, my_location.as_ref()).await;
        let fan_out = (best_peers.len().min(3)).max(1);
        for package_id in discovered.keys() {
            if let Ok(Some(package)) = self.query_dht_parallel(package_id, fan_out).await {
                let text_match = query.text.is_empty()
                    || package
                        .metadata
                        .title
                        .to_lowercase()
                        .contains(&query.text.to_lowercase())
                    || package
                        .metadata
                        .description
                        .to_lowercase()
                        .contains(&query.text.to_lowercase());
                let category_match = query.categories.is_empty()
                    || package
                        .metadata
                        .categories
                        .iter()
                        .any(|c| query.categories.contains(c));
                let tag_match = query.tags.is_empty()
                    || package.metadata.tags.iter().any(|t| query.tags.contains(t));
                if text_match && category_match && tag_match {
                    if !results.iter().any(|p| p.id == package.id) {
                        results.push(package);
                    }
                }
            }
        }
        if let Some(limit) = query.limit {
            results.truncate(limit);
        }
        Span::current().record("result_count", results.len());
        Ok(results)
    }

    #[instrument(skip(self), fields(package_id = %id))]
    async fn get_package(&self, id: &PackageId) -> std::result::Result<Package, MarketplaceError> {
        {
            let cache = self.package_cache.read().await;
            if let Some((package, cached_at)) = cache.get(id) {
                if cached_at.elapsed() < std::time::Duration::from_secs(300) {
                    Span::current().record("cache_hit", true);
                    return Ok(package.clone());
                }
            }
        }
        {
            let local_packages = self.local_packages.read().await;
            if let Some(package) = local_packages.get(id) {
                let mut cache = self.package_cache.write().await;
                cache.insert(id.clone(), (package.clone(), std::time::Instant::now()));
                return Ok(package.clone());
            }
        }
        if let Some(package) = self.query_dht_parallel(id, 3).await? {
            self.local_packages
                .write()
                .await
                .insert(id.clone(), package.clone());
            let mut cache = self.package_cache.write().await;
            cache.insert(id.clone(), (package.clone(), std::time::Instant::now()));
            Span::current().record("cache_hit", false);
            return Ok(package);
        }
        Err(MarketplaceError::package_not_found(
            id.to_string(),
            "p2p registry",
        ))
    }

    async fn get_package_version(
        &self, id: &PackageId, version: &str,
    ) -> std::result::Result<Package, MarketplaceError> {
        let package = self.get_package(id).await?;
        if package.version.to_string() == version {
            Ok(package)
        } else {
            Err(MarketplaceError::package_not_found(
                format!("{}@{}", id, version),
                "p2p registry",
            ))
        }
    }

    async fn list_versions(
        &self, id: &PackageId,
    ) -> std::result::Result<Vec<Package>, MarketplaceError> {
        match self.get_package(id).await {
            Ok(package) => Ok(vec![package]),
            Err(_) => Ok(Vec::new()),
        }
    }

    async fn publish(&self, package: Package) -> std::result::Result<(), MarketplaceError> {
        let package_id = package.id.clone();
        self.local_packages
            .write()
            .await
            .insert(package_id.clone(), package.clone());
        self.store_in_dht(&package_id, &package).await?;
        self.announce_package(&package).await?;
        Ok(())
    }

    async fn delete(
        &self, id: &PackageId, _version: &str,
    ) -> std::result::Result<(), MarketplaceError> {
        self.local_packages.write().await.remove(id);
        Ok(())
    }

    async fn exists(&self, id: &PackageId) -> std::result::Result<bool, MarketplaceError> {
        {
            let local_packages = self.local_packages.read().await;
            if local_packages.contains_key(id) {
                return Ok(true);
            }
        }
        Ok(self.query_dht(id).await?.is_some())
    }

    async fn metadata(&self) -> std::result::Result<RegistryMetadata, MarketplaceError> {
        let local_packages = self.local_packages.read().await;
        Ok(RegistryMetadata {
            name: "Ggen P2P Registry".to_string(),
            description: "Decentralized P2P package registry using libp2p".to_string(),
            version: "2.4.0".to_string(),
            package_count: local_packages.len(),
            api_version: "v1".to_string(),
            features: vec![
                "p2p".to_string(),
                "dht".to_string(),
                "gossipsub".to_string(),
                "reputation".to_string(),
            ],
        })
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[tokio::test]
    async fn test_p2p_config_default() {
        let config = P2PConfig::default();
        assert_eq!(config.packages_topic, "/ggen/packages/v1");
        assert!(config.dht_server_mode);
        assert_eq!(config.bootstrap_nodes.len(), 0);
        assert_eq!(config.query_timeout_secs, 30);
    }

    #[tokio::test]
    async fn test_peer_reputation_new() {
        let keypair = libp2p::identity::Keypair::generate_ed25519();
        let peer_id = PeerId::from(keypair.public());
        let reputation = PeerReputation::new(peer_id);
        assert_eq!(reputation.successful_retrievals, 0);
        assert_eq!(reputation.failed_retrievals, 0);
        assert_eq!(reputation.success_rate(), 1.0);
    }

    #[tokio::test]
    async fn test_peer_reputation_success_rate() {
        let keypair = libp2p::identity::Keypair::generate_ed25519();
        let peer_id = PeerId::from(keypair.public());
        let mut reputation = PeerReputation::new(peer_id);
        reputation.successful_retrievals = 8;
        reputation.failed_retrievals = 2;
        assert_eq!(reputation.success_rate(), 0.8);
    }
}
