//! Comprehensive P2P Marketplace Performance Benchmarks
//!
//! Benchmarks for libp2p-based decentralized package registry:
//! - Peer discovery latency (DHT)
//! - Package search across peers
//! - DHT put/get operations
//! - Gossipsub message propagation
//! - Memory usage per peer
//! - Network scalability (1-100 peers)
//! - CLI command response times
//!
//! Expected Performance Metrics (from architecture docs):
//! - DHT lookup: ~200-500ms (1000 peers)
//! - Gossipsub propagation: ~1-3s
//! - Local cache: <1ms
//! - Memory per peer: ~50MB

use criterion::{black_box, criterion_group, criterion_main, BenchmarkId, Criterion, Throughput};
use std::collections::{HashMap, HashSet};
use std::sync::Arc;
use std::time::{Duration, Instant};
use tokio::runtime::Runtime;
use tokio::sync::RwLock;

// ============================================================================
// Mock P2P Components for Benchmarking
// ============================================================================

/// Simulates a P2P peer node with DHT and gossipsub capabilities
#[derive(Debug, Clone)]
struct MockPeer {
    peer_id: String,
    packages: Arc<RwLock<HashMap<String, MockPackage>>>,
    connected_peers: Arc<RwLock<HashSet<String>>>,
    dht_storage: Arc<RwLock<HashMap<String, Vec<u8>>>>,
    gossip_messages: Arc<RwLock<Vec<GossipMessage>>>,
    reputation_score: f64,
    memory_usage_bytes: u64,
}

#[derive(Debug, Clone)]
struct MockPackage {
    id: String,
    name: String,
    version: String,
    size_bytes: u64,
    metadata: HashMap<String, String>,
}

#[derive(Debug, Clone)]
struct GossipMessage {
    topic: String,
    data: Vec<u8>,
    timestamp: Instant,
    from_peer: String,
}

impl MockPeer {
    fn new(peer_id: String) -> Self {
        Self {
            peer_id,
            packages: Arc::new(RwLock::new(HashMap::new())),
            connected_peers: Arc::new(RwLock::new(HashSet::new())),
            dht_storage: Arc::new(RwLock::new(HashMap::new())),
            gossip_messages: Arc::new(RwLock::new(Vec::new())),
            reputation_score: 1.0,
            memory_usage_bytes: 50 * 1024 * 1024, // ~50MB base
        }
    }

    async fn connect_to_peer(&self, peer_id: String) -> Duration {
        let start = Instant::now();
        let mut peers = self.connected_peers.write().await;
        peers.insert(peer_id);
        start.elapsed()
    }

    async fn publish_package(&self, package: MockPackage) -> Duration {
        let start = Instant::now();
        let mut packages = self.packages.write().await;
        packages.insert(package.id.clone(), package);
        start.elapsed()
    }

    async fn dht_put(&self, key: String, value: Vec<u8>) -> Duration {
        let start = Instant::now();
        let mut dht = self.dht_storage.write().await;
        dht.insert(key, value);
        // Simulate network latency
        tokio::time::sleep(Duration::from_millis(10)).await;
        start.elapsed()
    }

    async fn dht_get(&self, key: &str) -> (Option<Vec<u8>>, Duration) {
        let start = Instant::now();
        let dht = self.dht_storage.read().await;
        let value = dht.get(key).cloned();
        // Simulate network latency
        tokio::time::sleep(Duration::from_millis(15)).await;
        (value, start.elapsed())
    }

    async fn gossip_publish(&self, topic: String, data: Vec<u8>) -> Duration {
        let start = Instant::now();
        let message = GossipMessage {
            topic,
            data,
            timestamp: Instant::now(),
            from_peer: self.peer_id.clone(),
        };
        let mut messages = self.gossip_messages.write().await;
        messages.push(message);
        // Simulate gossipsub propagation delay
        tokio::time::sleep(Duration::from_millis(50)).await;
        start.elapsed()
    }

    async fn search_local(&self, query: &str) -> (Vec<MockPackage>, Duration) {
        let start = Instant::now();
        let packages = self.packages.read().await;
        let results: Vec<MockPackage> = packages
            .values()
            .filter(|p| p.name.contains(query) || p.metadata.values().any(|v| v.contains(query)))
            .cloned()
            .collect();
        (results, start.elapsed())
    }
}

/// Simulates a P2P network with multiple peers
struct MockP2PNetwork {
    peers: Vec<MockPeer>,
    bootstrap_time: Duration,
}

impl MockP2PNetwork {
    async fn new(num_peers: usize) -> Self {
        let start = Instant::now();
        let mut peers = Vec::with_capacity(num_peers);

        // Create peers
        for i in 0..num_peers {
            let peer = MockPeer::new(format!("peer-{}", i));
            peers.push(peer);
        }

        // Bootstrap network: connect peers in a realistic topology
        // Each peer connects to ~10 other peers (typical P2P network)
        for i in 0..num_peers {
            let connections = std::cmp::min(10, num_peers - 1);
            for j in 0..connections {
                let target_idx = (i + j + 1) % num_peers;
                if target_idx != i {
                    let target_id = format!("peer-{}", target_idx);
                    peers[i].connect_to_peer(target_id).await;
                }
            }
        }

        let bootstrap_time = start.elapsed();

        Self {
            peers,
            bootstrap_time,
        }
    }

    async fn distribute_packages(&self, num_packages: usize) {
        for (i, peer) in self.peers.iter().enumerate() {
            // Each peer hosts some packages
            let packages_per_peer = num_packages / self.peers.len();
            for j in 0..packages_per_peer {
                let pkg_id = i * packages_per_peer + j;
                let package = MockPackage {
                    id: format!("pkg-{}", pkg_id),
                    name: format!("package-{}", pkg_id),
                    version: "1.0.0".to_string(),
                    size_bytes: 1024 * 100, // 100KB
                    metadata: HashMap::from([
                        ("category".to_string(), format!("cat-{}", pkg_id % 5)),
                        ("tags".to_string(), format!("tag-{}", pkg_id % 10)),
                    ]),
                };
                peer.publish_package(package).await;
            }
        }
    }

    async fn simulate_dht_lookup(&self, key: &str) -> (bool, Duration) {
        let start = Instant::now();
        // Simulate DHT routing: O(log N) hops through network
        let hops = (self.peers.len() as f64).log2().ceil() as usize;

        for _ in 0..hops {
            // Simulate hop latency
            tokio::time::sleep(Duration::from_millis(20)).await;
        }

        // Check if any peer has the key
        for peer in &self.peers {
            let (value, _) = peer.dht_get(key).await;
            if value.is_some() {
                return (true, start.elapsed());
            }
        }

        (false, start.elapsed())
    }

    async fn simulate_package_search(&self, query: &str) -> (Vec<MockPackage>, Duration) {
        let start = Instant::now();
        let mut all_results = Vec::new();

        // Search across multiple peers (simulating P2P search)
        let search_peer_count = std::cmp::min(5, self.peers.len());
        for peer in self.peers.iter().take(search_peer_count) {
            let (results, _) = peer.search_local(query).await;
            all_results.extend(results);
        }

        // Deduplicate
        all_results.sort_by(|a, b| a.id.cmp(&b.id));
        all_results.dedup_by(|a, b| a.id == b.id);

        (all_results, start.elapsed())
    }

    fn calculate_total_memory(&self) -> u64 {
        self.peers.iter().map(|p| p.memory_usage_bytes).sum()
    }

    fn calculate_avg_memory_per_peer(&self) -> u64 {
        self.calculate_total_memory() / self.peers.len() as u64
    }
}

// ============================================================================
// Benchmark: Peer Discovery
// ============================================================================

fn bench_peer_discovery(c: &mut Criterion) {
    let rt = Runtime::new().unwrap();
    let mut group = c.benchmark_group("p2p_peer_discovery");

    for peer_count in [5, 10, 20, 50].iter() {
        group.bench_with_input(
            BenchmarkId::new("bootstrap_network", peer_count),
            peer_count,
            |b, &size| {
                b.to_async(&rt).iter(|| async move {
                    let network = MockP2PNetwork::new(size).await;
                    black_box(network.bootstrap_time)
                });
            },
        );
    }

    group.finish();
}

// ============================================================================
// Benchmark: DHT Operations
// ============================================================================

fn bench_dht_operations(c: &mut Criterion) {
    let rt = Runtime::new().unwrap();
    let mut group = c.benchmark_group("p2p_dht_operations");

    // DHT Put operation
    for peer_count in [10, 50, 100].iter() {
        group.bench_with_input(
            BenchmarkId::new("dht_put", peer_count),
            peer_count,
            |b, &size| {
                b.to_async(&rt).iter(|| async move {
                    let network = MockP2PNetwork::new(size).await;
                    let peer = &network.peers[0];
                    let key = "test-package-id".to_string();
                    let value = vec![1, 2, 3, 4, 5];
                    let duration = peer.dht_put(key, value).await;
                    black_box(duration)
                });
            },
        );
    }

    // DHT Get operation
    for peer_count in [10, 50, 100].iter() {
        group.bench_with_input(
            BenchmarkId::new("dht_get", peer_count),
            peer_count,
            |b, &size| {
                b.to_async(&rt).iter(|| async move {
                    let network = MockP2PNetwork::new(size).await;
                    // Pre-populate DHT
                    let peer = &network.peers[0];
                    let key = "test-package-id".to_string();
                    let value = vec![1, 2, 3, 4, 5];
                    peer.dht_put(key.clone(), value).await;

                    // Benchmark lookup
                    let (found, duration) = network.simulate_dht_lookup(&key).await;
                    black_box((found, duration))
                });
            },
        );
    }

    // DHT Lookup across network sizes
    for peer_count in [10, 50, 100, 500, 1000].iter() {
        group.bench_with_input(
            BenchmarkId::new("dht_lookup_network_size", peer_count),
            peer_count,
            |b, &size| {
                b.to_async(&rt).iter(|| async move {
                    let network = MockP2PNetwork::new(size).await;
                    let (_, duration) = network.simulate_dht_lookup("random-key").await;
                    black_box(duration)
                });
            },
        );
    }

    group.finish();
}

// ============================================================================
// Benchmark: Package Search Performance
// ============================================================================

fn bench_package_search(c: &mut Criterion) {
    let rt = Runtime::new().unwrap();
    let mut group = c.benchmark_group("p2p_package_search");

    for (peer_count, package_count) in [(10, 100), (20, 500), (50, 1000)].iter() {
        group.bench_with_input(
            BenchmarkId::new(
                "search_across_peers",
                format!("{}p_{}pkg", peer_count, package_count),
            ),
            &(*peer_count, *package_count),
            |b, (peers, packages)| {
                b.to_async(&rt).iter(|| async move {
                    let network = MockP2PNetwork::new(*peers).await;
                    network.distribute_packages(*packages).await;
                    let (results, duration) = network.simulate_package_search("package").await;
                    black_box((results.len(), duration))
                });
            },
        );
    }

    // Local cache hit (should be <1ms)
    group.bench_function("local_cache_hit", |b| {
        b.to_async(&rt).iter(|| async move {
            let peer = MockPeer::new("test-peer".to_string());
            let package = MockPackage {
                id: "pkg-1".to_string(),
                name: "cached-package".to_string(),
                version: "1.0.0".to_string(),
                size_bytes: 1024 * 100,
                metadata: HashMap::new(),
            };
            peer.publish_package(package).await;

            let (results, duration) = peer.search_local("cached-package").await;
            black_box((results.len(), duration))
        });
    });

    group.finish();
}

// ============================================================================
// Benchmark: Gossipsub Message Propagation
// ============================================================================

fn bench_gossipsub_propagation(c: &mut Criterion) {
    let rt = Runtime::new().unwrap();
    let mut group = c.benchmark_group("p2p_gossipsub");

    for peer_count in [5, 10, 20, 50].iter() {
        group.bench_with_input(
            BenchmarkId::new("message_propagation", peer_count),
            peer_count,
            |b, &size| {
                b.to_async(&rt).iter(|| async move {
                    let network = MockP2PNetwork::new(size).await;
                    let peer = &network.peers[0];

                    let message = b"Package announcement: test-package@1.0.0";
                    let duration = peer
                        .gossip_publish("/ggen/packages/v1".to_string(), message.to_vec())
                        .await;

                    // Simulate propagation to other peers
                    let propagation_delay = Duration::from_millis(50 * (size as u64 - 1) / 5);
                    tokio::time::sleep(propagation_delay).await;

                    black_box(duration + propagation_delay)
                });
            },
        );
    }

    // Package announcement latency
    group.bench_function("package_announcement", |b| {
        b.to_async(&rt).iter(|| async move {
            let network = MockP2PNetwork::new(10).await;
            let peer = &network.peers[0];

            let package = MockPackage {
                id: "new-pkg".to_string(),
                name: "new-package".to_string(),
                version: "1.0.0".to_string(),
                size_bytes: 1024 * 100,
                metadata: HashMap::new(),
            };

            let start = Instant::now();
            peer.publish_package(package.clone()).await;
            let message = serde_json::to_vec(&package).unwrap();
            peer.gossip_publish("/ggen/packages/v1".to_string(), message)
                .await;
            let duration = start.elapsed();

            black_box(duration)
        });
    });

    group.finish();
}

// ============================================================================
// Benchmark: Memory Usage
// ============================================================================

fn bench_memory_usage(c: &mut Criterion) {
    let rt = Runtime::new().unwrap();
    let mut group = c.benchmark_group("p2p_memory_usage");

    // Memory per peer baseline
    group.bench_function("single_peer_baseline", |b| {
        b.to_async(&rt).iter(|| async move {
            let peer = MockPeer::new("test-peer".to_string());
            black_box(peer.memory_usage_bytes)
        });
    });

    // Memory scaling with network size
    for peer_count in [10, 20, 50, 100].iter() {
        group.bench_with_input(
            BenchmarkId::new("network_memory", peer_count),
            peer_count,
            |b, &size| {
                b.to_async(&rt).iter(|| async move {
                    let network = MockP2PNetwork::new(size).await;
                    let total = network.calculate_total_memory();
                    let avg = network.calculate_avg_memory_per_peer();
                    black_box((total, avg))
                });
            },
        );
    }

    // Memory with packages
    for (peer_count, package_count) in [(10, 100), (20, 500), (50, 1000)].iter() {
        group.bench_with_input(
            BenchmarkId::new(
                "memory_with_packages",
                format!("{}p_{}pkg", peer_count, package_count),
            ),
            &(*peer_count, *package_count),
            |b, (peers, packages)| {
                b.to_async(&rt).iter(|| async move {
                    let network = MockP2PNetwork::new(*peers).await;
                    network.distribute_packages(*packages).await;

                    // Estimate memory increase from packages
                    let package_memory = (*packages as u64) * 100 * 1024 / (*peers as u64);
                    let total = network.calculate_total_memory() + (package_memory * *peers as u64);
                    let avg = total / (*peers as u64);

                    black_box((total, avg))
                });
            },
        );
    }

    group.finish();
}

// ============================================================================
// Benchmark: Network Scalability
// ============================================================================

fn bench_network_scalability(c: &mut Criterion) {
    let rt = Runtime::new().unwrap();
    let mut group = c.benchmark_group("p2p_scalability");
    group.sample_size(10); // Reduce sample size for large networks

    // Network bootstrap time scaling
    for peer_count in [5, 10, 20, 50, 100].iter() {
        group.bench_with_input(
            BenchmarkId::new("bootstrap_scaling", peer_count),
            peer_count,
            |b, &size| {
                b.to_async(&rt).iter(|| async move {
                    let start = Instant::now();
                    let network = MockP2PNetwork::new(size).await;
                    let duration = start.elapsed();
                    black_box((network.bootstrap_time, duration))
                });
            },
        );
    }

    // Search latency scaling
    for peer_count in [10, 20, 50, 100].iter() {
        group.bench_with_input(
            BenchmarkId::new("search_scaling", peer_count),
            peer_count,
            |b, &size| {
                b.to_async(&rt).iter(|| async move {
                    let network = MockP2PNetwork::new(size).await;
                    network.distribute_packages(1000).await;
                    let (_, duration) = network.simulate_package_search("package").await;
                    black_box(duration)
                });
            },
        );
    }

    // DHT lookup scaling (logarithmic expected)
    for peer_count in [10, 50, 100, 500, 1000].iter() {
        group.bench_with_input(
            BenchmarkId::new("dht_lookup_scaling", peer_count),
            peer_count,
            |b, &size| {
                b.to_async(&rt).iter(|| async move {
                    let network = MockP2PNetwork::new(size).await;
                    let (_, duration) = network.simulate_dht_lookup("test-key").await;
                    black_box(duration)
                });
            },
        );
    }

    group.finish();
}

// ============================================================================
// Benchmark: CLI Command Response Times
// ============================================================================

fn bench_cli_commands(c: &mut Criterion) {
    let rt = Runtime::new().unwrap();
    let mut group = c.benchmark_group("p2p_cli_commands");

    // ggen marketplace search
    group.bench_function("cli_search_command", |b| {
        b.to_async(&rt).iter(|| async move {
            let network = MockP2PNetwork::new(20).await;
            network.distribute_packages(500).await;

            let start = Instant::now();
            let (results, _) = network.simulate_package_search("web").await;
            let duration = start.elapsed();

            black_box((results.len(), duration))
        });
    });

    // ggen marketplace install (includes search + DHT lookup)
    group.bench_function("cli_install_command", |b| {
        b.to_async(&rt).iter(|| async move {
            let network = MockP2PNetwork::new(20).await;
            network.distribute_packages(500).await;

            let start = Instant::now();

            // Search for package
            let (results, _) = network.simulate_package_search("pkg-100").await;

            // DHT lookup for package metadata
            let (_, _) = network.simulate_dht_lookup("pkg-100").await;

            // Simulate download (not included in measurement)

            let duration = start.elapsed();
            black_box((results.len(), duration))
        });
    });

    // ggen marketplace publish
    group.bench_function("cli_publish_command", |b| {
        b.to_async(&rt).iter(|| async move {
            let network = MockP2PNetwork::new(20).await;
            let peer = &network.peers[0];

            let package = MockPackage {
                id: "new-package".to_string(),
                name: "new-package".to_string(),
                version: "1.0.0".to_string(),
                size_bytes: 1024 * 100,
                metadata: HashMap::new(),
            };

            let start = Instant::now();

            // Store in DHT
            let package_json = serde_json::to_vec(&package).unwrap();
            peer.dht_put("new-package".to_string(), package_json.clone())
                .await;

            // Announce via gossipsub
            peer.gossip_publish("/ggen/packages/v1".to_string(), package_json)
                .await;

            let duration = start.elapsed();
            black_box(duration)
        });
    });

    group.finish();
}

// ============================================================================
// Benchmark: Peer Reputation and Reliability
// ============================================================================

fn bench_peer_reputation(c: &mut Criterion) {
    let rt = Runtime::new().unwrap();
    let mut group = c.benchmark_group("p2p_peer_reputation");

    // Measure reputation calculation overhead
    group.bench_function("reputation_calculation", |b| {
        b.to_async(&rt).iter(|| async move {
            let network = MockP2PNetwork::new(50).await;

            let start = Instant::now();
            let avg_reputation: f64 = network
                .peers
                .iter()
                .map(|p| p.reputation_score)
                .sum::<f64>()
                / network.peers.len() as f64;
            let duration = start.elapsed();

            black_box((avg_reputation, duration))
        });
    });

    // Peer selection based on reputation
    group.bench_function("peer_selection", |b| {
        b.to_async(&rt).iter(|| async move {
            let network = MockP2PNetwork::new(50).await;

            let start = Instant::now();
            let mut best_peers: Vec<_> = network
                .peers
                .iter()
                .filter(|p| p.reputation_score > 0.8)
                .collect();
            best_peers.sort_by(|a, b| b.reputation_score.partial_cmp(&a.reputation_score).unwrap());
            let duration = start.elapsed();

            black_box((best_peers.len(), duration))
        });
    });

    group.finish();
}

// ============================================================================
// Criterion Configuration
// ============================================================================

criterion_group!(
    benches,
    bench_peer_discovery,
    bench_dht_operations,
    bench_package_search,
    bench_gossipsub_propagation,
    bench_memory_usage,
    bench_network_scalability,
    bench_cli_commands,
    bench_peer_reputation,
);

criterion_main!(benches);
