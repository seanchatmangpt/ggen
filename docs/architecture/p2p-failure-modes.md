# P2P Marketplace Failure Modes & Resilience Analysis

**Version:** 2.4.0
**Date:** 2025-11-02
**Risk Assessment:** Medium

## Executive Summary

This document analyzes failure scenarios for the P2P marketplace integration and defines mitigation strategies to ensure system resilience. The analysis follows the principle of **graceful degradation**: the system should continue functioning (possibly with reduced capabilities) rather than failing completely.

---

## 1. Network Failures

### 1.1 Bootstrap Nodes Unreachable

**Scenario:** All configured bootstrap nodes are offline or unreachable.

| Aspect | Details |
|--------|---------|
| **Detection** | TCP connection timeout (30s) + retry failures |
| **Probability** | Low (with 3+ geographically distributed nodes) |
| **Impact** | Cannot join P2P network, isolated node |
| **User Experience** | "P2P unavailable" warning, fallback to file registry |

**Mitigation Strategy:**
```rust
pub async fn start_node(config: P2PConfig) -> Result<P2PRegistry> {
    // Try to connect to bootstrap nodes with timeout
    let timeout = Duration::from_secs(30);
    let bootstrap_result = timeout_future(
        connect_to_bootstrap(&config.bootstrap_nodes),
        timeout
    ).await;

    match bootstrap_result {
        Ok(_) => {
            info!("Connected to P2P network via bootstrap nodes");
            // Proceed with P2P registry
        }
        Err(TimeoutError) => {
            warn!("Bootstrap timeout, operating in isolated mode");
            // Create P2P node without bootstrap
            // Will auto-connect if peers discovered later
        }
        Err(e) => {
            warn!("Bootstrap failed: {}, falling back to file registry", e);
            return fallback_to_file_registry();
        }
    }
}
```

**Fallback Behavior:**
1. Warn user: "⚠️  P2P network unreachable, using local registry"
2. Switch to file-based registry automatically
3. Continue searching local `~/.ggen/registry/`
4. Retry P2P connection in background every 5 minutes

**Recovery:**
- Automatic reconnection when bootstrap nodes come back online
- No user intervention required
- Cached packages remain available

---

### 1.2 DHT Query Timeout

**Scenario:** Kademlia DHT query takes too long or never completes.

| Aspect | Details |
|--------|---------|
| **Detection** | No response after 10 seconds |
| **Probability** | Medium (in congested networks) |
| **Impact** | Incomplete search results, missing packages |
| **User Experience** | Partial results with "(incomplete)" indicator |

**Mitigation Strategy:**
```rust
pub async fn search(&self, query: &Query) -> Result<Vec<Package>> {
    // Phase 1: Search local packages (fast, always succeeds)
    let mut results = search_local_packages(query).await;

    // Phase 2: Search DHT with timeout
    let dht_timeout = Duration::from_secs(10);
    let dht_results = match timeout_future(
        search_dht(query),
        dht_timeout
    ).await {
        Ok(pkgs) => pkgs,
        Err(TimeoutError) => {
            warn!("DHT query timeout, returning local results only");
            Vec::new()
        }
    };

    // Merge and deduplicate
    results.extend(dht_results);
    results.dedup_by_key(|p| p.id.clone());

    if results.is_empty() {
        info!("No packages found for query: {:?}", query);
    }

    Ok(results)
}
```

**Fallback Behavior:**
1. Return local packages immediately
2. DHT results arrive late → ignore (query already completed)
3. Display message: "Showing local results (network slow)"

**Recovery:**
- Next search will retry DHT query
- DHT queries cached for 5 minutes
- Background DHT refresh every 60 seconds

---

### 1.3 Network Partition (Split Brain)

**Scenario:** Node isolated from main P2P network but connected to a small partition.

| Aspect | Details |
|--------|---------|
| **Detection** | No new peer connections for 60 seconds, peer count drops below 5 |
| **Probability** | Low (requires network infrastructure failure) |
| **Impact** | Reduced package availability, stale data |
| **User Experience** | "⚠️  Limited P2P connectivity" warning |

**Mitigation Strategy:**
```rust
// Background health monitoring
pub async fn monitor_network_health(registry: Arc<P2PRegistry>) {
    let mut interval = tokio::time::interval(Duration::from_secs(60));

    loop {
        interval.tick().await;

        let peer_count = registry.connected_peers().await;
        let last_connection_age = registry.last_peer_connection_time().await;

        if peer_count < 5 && last_connection_age > Duration::from_secs(60) {
            warn!("Network partition suspected (peers: {}, last connection: {:?}s ago)",
                peer_count, last_connection_age);

            // Attempt re-bootstrap
            if let Err(e) = registry.bootstrap().await {
                error!("Re-bootstrap failed: {}", e);
            }

            // Notify user
            eprintln!("⚠️  Limited P2P connectivity, some packages may be unavailable");
        }
    }
}
```

**Fallback Behavior:**
1. Continue operating with available peers
2. Prefer local cache over stale P2P data
3. Retry bootstrap every 5 minutes
4. Display connectivity status in `ggen marketplace p2p status`

---

### 1.4 Malicious Peer (Byzantine Fault)

**Scenario:** Peer provides incorrect package metadata or corrupted files.

| Aspect | Details |
|--------|---------|
| **Detection** | Checksum mismatch, invalid signature (future) |
| **Probability** | Low (with reputation system) |
| **Impact** | Installation failure, potential security risk |
| **User Experience** | "❌ Package verification failed, trying alternate source" |

**Mitigation Strategy:**
```rust
pub async fn install_package_verified(
    package_id: &PackageId,
    providers: Vec<PeerId>
) -> Result<InstallResult> {
    // Sort providers by reputation (descending)
    let mut sorted_providers = providers.clone();
    sorted_providers.sort_by_key(|peer| {
        let reputation = self.get_peer_reputation(peer).await;
        (reputation * 1000.0) as i64  // Convert to sortable integer
    });
    sorted_providers.reverse();

    // Try providers in order until success
    for provider in sorted_providers {
        match try_install_from_peer(package_id, provider).await {
            Ok(result) => {
                // Verify checksum
                if verify_checksum(&result, &expected_checksum) {
                    // Success - update peer reputation
                    self.record_peer_success(provider).await;
                    return Ok(result);
                } else {
                    // Checksum failure - mark peer as malicious
                    error!("Checksum mismatch from peer {}, banning", provider);
                    self.record_peer_failure(provider).await;
                    self.ban_peer(provider, Duration::from_secs(3600)).await;
                    continue;  // Try next provider
                }
            }
            Err(e) => {
                warn!("Install from peer {} failed: {}", provider, e);
                self.record_peer_failure(provider).await;
                continue;
            }
        }
    }

    Err(MarketplaceError::AllProvidersFailed)
}
```

**Fallback Behavior:**
1. Try next provider in reputation-sorted list
2. Ban malicious peer for 1 hour
3. Report to user: "Package verification failed, trying alternate source"
4. After all P2P providers fail, fallback to file registry

**Reputation System:**
```rust
pub async fn ban_peer(&self, peer_id: PeerId, duration: Duration) {
    let mut banned_peers = self.banned_peers.write().await;
    banned_peers.insert(peer_id, Instant::now() + duration);

    warn!("Peer {} banned for {:?}", peer_id, duration);
}

pub async fn is_peer_banned(&self, peer_id: &PeerId) -> bool {
    let banned_peers = self.banned_peers.read().await;
    if let Some(&ban_expiry) = banned_peers.get(peer_id) {
        if Instant::now() < ban_expiry {
            return true;  // Still banned
        }
    }
    false
}
```

---

## 2. Storage Failures

### 2.1 Registry Index Corrupted

**Scenario:** `~/.ggen/registry/index.json` is corrupted or invalid JSON.

| Aspect | Details |
|--------|---------|
| **Detection** | JSON parse error during load |
| **Probability** | Low (requires disk corruption or manual edit) |
| **Impact** | Cannot load local registry, fallback to P2P |
| **User Experience** | "⚠️  Local registry corrupted, recreating..." |

**Mitigation Strategy:**
```rust
pub async fn load_registry_index(path: &Path) -> Result<RegistryIndex> {
    match fs::read_to_string(path).await {
        Ok(contents) => {
            match serde_json::from_str::<RegistryIndex>(&contents) {
                Ok(index) => Ok(index),
                Err(e) => {
                    error!("Registry index corrupted: {}", e);

                    // Backup corrupted file
                    let backup_path = path.with_extension("json.corrupted");
                    if let Err(be) = fs::rename(path, &backup_path).await {
                        warn!("Failed to backup corrupted index: {}", be);
                    } else {
                        info!("Backed up corrupted index to: {}", backup_path.display());
                    }

                    // Create new empty index
                    let new_index = RegistryIndex::new();
                    save_registry_index(path, &new_index).await?;

                    eprintln!("⚠️  Registry index was corrupted and has been reset");
                    eprintln!("   Backup saved to: {}", backup_path.display());

                    Ok(new_index)
                }
            }
        }
        Err(e) if e.kind() == std::io::ErrorKind::NotFound => {
            // File doesn't exist - create new
            let new_index = RegistryIndex::new();
            save_registry_index(path, &new_index).await?;
            Ok(new_index)
        }
        Err(e) => Err(e.into()),
    }
}
```

**Recovery:**
1. Backup corrupted file to `.json.corrupted`
2. Create new empty index
3. Notify user to run `ggen marketplace update` to repopulate

---

### 2.2 Disk Full During Installation

**Scenario:** Insufficient disk space to extract package.

| Aspect | Details |
|--------|---------|
| **Detection** | `std::io::Error` with kind `StorageFull` |
| **Probability** | Medium (especially on constrained systems) |
| **Impact** | Installation fails, partial files may remain |
| **User Experience** | "❌ Insufficient disk space: need 50MB, have 10MB" |

**Mitigation Strategy:**
```rust
pub async fn install_package(options: &InstallOptions) -> Result<InstallResult> {
    // Step 1: Check available disk space BEFORE downloading
    let package_size = get_package_size(&options.package_name).await?;
    let available_space = get_available_disk_space(&packages_dir)?;

    // Require 2x package size (for download + extraction)
    let required_space = package_size * 2;

    if available_space < required_space {
        return Err(MarketplaceError::InsufficientDiskSpace {
            required: required_space,
            available: available_space,
        });
    }

    // Step 2: Extract to temporary directory first (atomic operation)
    let temp_dir = tempfile::tempdir_in(&packages_dir)?;

    match extract_tarball(&tarball_path, temp_dir.path()).await {
        Ok(_) => {
            // Success - move to final location
            atomic_move(temp_dir.path(), &install_path)?;
        }
        Err(e) if is_disk_full_error(&e) => {
            // Cleanup partial extraction
            let _ = fs::remove_dir_all(temp_dir.path()).await;

            return Err(MarketplaceError::InsufficientDiskSpace {
                required: package_size,
                available: get_available_disk_space(&packages_dir)?,
            });
        }
        Err(e) => {
            // Other error - cleanup and propagate
            let _ = fs::remove_dir_all(temp_dir.path()).await;
            return Err(e.into());
        }
    }

    Ok(InstallResult { /* ... */ })
}

fn is_disk_full_error(err: &std::io::Error) -> bool {
    matches!(err.kind(), std::io::ErrorKind::StorageFull)
        || err.raw_os_error() == Some(28)  // ENOSPC on Unix
}
```

**User-Facing Error:**
```
❌ Installation failed: Insufficient disk space

Package: my-pkg@1.0.0
Required: 50 MB
Available: 10 MB

Suggestions:
  • Free up disk space: rm -rf ~/.ggen/cache/*
  • Remove unused packages: ggen marketplace list --installed
  • Clean temporary files: ggen marketplace clean
```

---

### 2.3 Concurrent Installations (Race Condition)

**Scenario:** Two `ggen marketplace install` processes running simultaneously.

| Aspect | Details |
|--------|---------|
| **Detection** | File lock acquisition fails |
| **Probability** | Low (user-initiated, rare in practice) |
| **Impact** | One installation fails or waits |
| **User Experience** | "⏳ Another installation in progress, waiting..." |

**Mitigation Strategy:**
```rust
use fs2::FileExt;
use std::fs::File;
use std::time::Duration;

pub async fn install_package_with_lock(options: &InstallOptions) -> Result<InstallResult> {
    // Acquire global installation lock
    let lock_path = packages_dir().join(".ggen-install.lock");
    let lock_file = File::create(&lock_path)?;

    // Try to acquire lock with retries
    let max_retries = 3;
    let retry_delay = Duration::from_secs(5);

    for attempt in 0..max_retries {
        match lock_file.try_lock_exclusive() {
            Ok(_) => {
                // Lock acquired - proceed with installation
                let result = install_package_impl(options).await;

                // Release lock
                lock_file.unlock()?;
                fs::remove_file(&lock_path)?;

                return result;
            }
            Err(e) if e.kind() == std::io::ErrorKind::WouldBlock => {
                // Lock held by another process
                if attempt < max_retries - 1 {
                    eprintln!("⏳ Another installation in progress, waiting... (attempt {}/{})",
                        attempt + 1, max_retries);
                    tokio::time::sleep(retry_delay).await;
                } else {
                    return Err(MarketplaceError::LockTimeout {
                        path: lock_path,
                        waited: retry_delay * max_retries,
                    });
                }
            }
            Err(e) => return Err(e.into()),
        }
    }

    unreachable!()
}
```

**Fallback Behavior:**
1. Wait up to 15 seconds (3 retries × 5s)
2. If lock not acquired, fail with clear error message
3. Suggest: "Wait for other installation to complete or run `ps aux | grep ggen` to check for stuck processes"

---

## 3. Dependency Resolution Failures

### 3.1 Circular Dependency Detected

**Scenario:** Package A depends on B, B depends on C, C depends on A.

| Aspect | Details |
|--------|---------|
| **Detection** | DFS cycle detection during graph building |
| **Probability** | Low (publisher error) |
| **Impact** | Cannot install package |
| **User Experience** | "❌ Circular dependency: pkg-a → pkg-b → pkg-c → pkg-a" |

**Detection Algorithm:**
```rust
impl DependencyGraph {
    /// Detect circular dependencies using DFS
    pub fn detect_circular(&self) -> Result<()> {
        let mut visited = HashSet::new();
        let mut rec_stack = HashSet::new();

        for key in self.nodes.keys() {
            if !visited.contains(key) {
                self.dfs_cycle_check(key, &mut visited, &mut rec_stack)?;
            }
        }

        Ok(())
    }

    fn dfs_cycle_check(
        &self,
        node: &str,
        visited: &mut HashSet<String>,
        rec_stack: &mut HashSet<String>,
    ) -> Result<()> {
        visited.insert(node.to_string());
        rec_stack.insert(node.to_string());

        if let Some(pkg) = self.nodes.get(node) {
            for (dep_name, dep_version) in &pkg.dependencies {
                let dep_key = format!("{}@{}", dep_name, dep_version);

                if !visited.contains(&dep_key) {
                    self.dfs_cycle_check(&dep_key, visited, rec_stack)?;
                } else if rec_stack.contains(&dep_key) {
                    // Cycle detected! Build cycle path
                    let cycle_path = build_cycle_path(rec_stack, &dep_key);
                    return Err(MarketplaceError::CircularDependency(cycle_path));
                }
            }
        }

        rec_stack.remove(node);
        Ok(())
    }
}

fn build_cycle_path(rec_stack: &HashSet<String>, start: &str) -> String {
    // Build human-readable cycle path: A → B → C → A
    format!("{} → {}", rec_stack.iter().collect::<Vec<_>>().join(" → "), start)
}
```

**User-Facing Error:**
```
❌ Circular dependency detected

Dependency cycle:
  web-framework@1.0.0 → database-orm@2.0.0 → query-builder@3.0.0 → web-framework@1.0.0

Packages with circular dependencies cannot be installed.
Please report this to the package publisher.
```

**Mitigation:**
- Early detection before any downloads
- Clear error message with full cycle path
- No partial installation (atomic failure)

---

### 3.2 Missing Dependency Version

**Scenario:** Package requires `dep@^1.0.0` but only `dep@2.0.0` is available.

| Aspect | Details |
|--------|---------|
| **Detection** | Version resolution fails (no matching version) |
| **Probability** | Medium (breaking changes in dependencies) |
| **Impact** | Cannot install package |
| **User Experience** | "❌ No version of dep satisfies ^1.0.0 (available: 2.0.0)" |

**Mitigation Strategy:**
```rust
pub fn resolve_version(
    package_name: &str,
    version_spec: &str,
    available_versions: &[String]
) -> Result<String> {
    // Try to find matching version
    let matching = available_versions.iter()
        .find(|v| matches_version_spec(v, version_spec));

    match matching {
        Some(version) => Ok(version.clone()),
        None => {
            // No matching version - provide helpful error
            let available_list = available_versions.join(", ");

            Err(MarketplaceError::NoMatchingVersion {
                package: package_name.to_string(),
                requirement: version_spec.to_string(),
                available: available_list,
            })
        }
    }
}
```

**User-Facing Error:**
```
❌ Dependency resolution failed

Package: my-app@1.0.0
Missing dependency: database-orm@^1.0.0

Available versions: 0.9.0, 2.0.0, 2.1.0

Suggestions:
  • Update my-app to use database-orm@^2.0.0
  • Check if an older version of my-app supports database-orm@^1.0.0
  • Contact package publisher to update dependencies
```

---

## 4. P2P-Specific Failures

### 4.1 Gossipsub Message Flood

**Scenario:** Malicious peer floods network with package announcements.

| Aspect | Details |
|--------|---------|
| **Detection** | Message rate exceeds 1000/second from single peer |
| **Probability** | Low (requires intentional attack) |
| **Impact** | High CPU usage, delayed legitimate messages |
| **User Experience** | System slowdown (transparent to user) |

**Mitigation Strategy:**
```rust
pub struct RateLimiter {
    /// Messages per peer in sliding window
    peer_messages: HashMap<PeerId, VecDeque<Instant>>,
    /// Window duration
    window: Duration,
    /// Max messages per window
    max_messages: usize,
}

impl RateLimiter {
    pub fn new(window: Duration, max_messages: usize) -> Self {
        Self {
            peer_messages: HashMap::new(),
            window,
            max_messages,
        }
    }

    pub fn check_and_update(&mut self, peer: PeerId) -> bool {
        let now = Instant::now();
        let messages = self.peer_messages
            .entry(peer)
            .or_insert_with(VecDeque::new);

        // Remove expired timestamps
        while let Some(&ts) = messages.front() {
            if now.duration_since(ts) > self.window {
                messages.pop_front();
            } else {
                break;
            }
        }

        // Check rate limit
        if messages.len() >= self.max_messages {
            warn!("Rate limit exceeded for peer {}", peer);
            return false;  // Reject message
        }

        // Accept message and record timestamp
        messages.push_back(now);
        true
    }
}

// Usage in P2P registry event handler
match swarm_event {
    SwarmEvent::Behaviour(BehaviourEvent::Gossipsub(event)) => {
        if let gossipsub::Event::Message { propagation_source, .. } = event {
            if !self.rate_limiter.check_and_update(propagation_source) {
                // Drop message and potentially ban peer
                self.record_peer_violation(propagation_source).await;

                if self.get_violation_count(propagation_source).await > 10 {
                    self.ban_peer(propagation_source, Duration::from_secs(3600)).await;
                }
                return;  // Drop message
            }

            // Process legitimate message
            // ...
        }
    }
    _ => {}
}
```

---

### 4.2 DHT Poisoning Attack

**Scenario:** Attacker stores incorrect package metadata in DHT.

| Aspect | Details |
|--------|---------|
| **Detection** | Checksum verification fails on download |
| **Probability** | Low (requires Sybil attack to control k nodes) |
| **Impact** | Wrong package installed, potential security risk |
| **User Experience** | "❌ Package verification failed" (transparent retry) |

**Mitigation Strategy:**
1. **Always verify checksums** (implemented in install pipeline)
2. **Require signatures** (future: v2.5.0)
3. **Cross-verify with multiple DHT nodes**:

```rust
pub async fn get_package_from_dht_verified(
    package_id: &PackageId
) -> Result<Package> {
    // Query multiple DHT nodes (redundancy)
    let mut handles = Vec::new();

    for _ in 0..3 {  // Query 3 different nodes
        let id = package_id.clone();
        let handle = tokio::spawn(async move {
            query_dht_single_node(&id).await
        });
        handles.push(handle);
    }

    // Wait for all queries
    let mut results = Vec::new();
    for handle in handles {
        if let Ok(Ok(package)) = handle.await {
            results.push(package);
        }
    }

    // Majority voting: require at least 2/3 matching checksums
    if results.len() < 2 {
        return Err(MarketplaceError::InsufficientDHTResponses);
    }

    let checksum_counts: HashMap<String, usize> = results.iter()
        .fold(HashMap::new(), |mut counts, pkg| {
            *counts.entry(pkg.metadata.checksum.clone()).or_insert(0) += 1;
            counts
        });

    let majority_checksum = checksum_counts.iter()
        .max_by_key(|(_, &count)| count)
        .map(|(checksum, _)| checksum.clone())
        .ok_or(MarketplaceError::DHTConsensusFailure)?;

    // Return package with majority checksum
    results.into_iter()
        .find(|pkg| pkg.metadata.checksum == majority_checksum)
        .ok_or(MarketplaceError::DHTConsensusFailure)
}
```

---

## 5. Failure Recovery Procedures

### Automatic Recovery

| Failure | Detection Time | Auto-Recovery | Recovery Time |
|---------|---------------|---------------|---------------|
| Bootstrap timeout | 30s | Fallback to file registry | Immediate |
| DHT query timeout | 10s | Return cached/local results | Immediate |
| Network partition | 60s | Re-bootstrap, retry | 5 minutes (periodic) |
| Checksum mismatch | Immediate | Try alternate peer | <5s per peer |
| Corrupted index | On load | Create new index | Immediate |
| Disk full | Pre-download | Abort with error | Manual (user frees space) |

### Manual Recovery

**Clear corrupt cache:**
```bash
$ ggen marketplace clean
✓ Cleared package cache
✓ Cleared registry cache
✓ Removed temporary files
Freed: 150 MB
```

**Reset P2P state:**
```bash
$ rm -rf ~/.ggen/p2p/
$ ggen marketplace p2p start --bootstrap <nodes>
```

**Rebuild registry index:**
```bash
$ ggen marketplace update --force
Rebuilding registry index...
✓ Scanned 150 packages
✓ Index saved to ~/.ggen/registry/index.json
```

---

## 6. Monitoring and Alerting

### Health Checks

```rust
pub struct SystemHealth {
    pub p2p_connected: bool,
    pub peer_count: usize,
    pub dht_responsive: bool,
    pub registry_loaded: bool,
    pub disk_space_available: u64,
}

pub async fn check_system_health() -> SystemHealth {
    SystemHealth {
        p2p_connected: check_p2p_connection().await,
        peer_count: get_peer_count().await,
        dht_responsive: test_dht_query().await.is_ok(),
        registry_loaded: check_registry_loaded().await,
        disk_space_available: get_available_disk_space(&packages_dir()).unwrap_or(0),
    }
}
```

**CLI Status Command:**
```bash
$ ggen marketplace p2p status

P2P Node Status:
  ● Running
  Peer ID: 12D3KooWABC...
  Connected peers: 47
  DHT status: Healthy
  Last bootstrap: 2 minutes ago

Registry Status:
  ● Loaded
  Packages indexed: 150
  Cache size: 35 / 100

Disk Space:
  Available: 25.3 GB
  Packages: 1.2 GB
```

---

## 7. Risk Matrix

| Risk | Likelihood | Impact | Mitigation Priority | Status |
|------|-----------|---------|---------------------|--------|
| Bootstrap nodes down | Low | Medium | High | ✅ Implemented |
| DHT query timeout | Medium | Low | Medium | ✅ Implemented |
| Network partition | Low | Medium | Medium | ✅ Implemented |
| Malicious peer | Low | High | High | ✅ Implemented |
| Corrupted index | Low | Medium | High | ✅ Implemented |
| Disk full | Medium | High | High | ✅ Implemented |
| Circular dependency | Low | Low | Low | ✅ Implemented |
| Missing dependency | Medium | Medium | Medium | ✅ Implemented |
| Gossipsub flood | Low | Medium | Medium | ✅ Implemented |
| DHT poisoning | Low | High | High | 🔄 Partial (checksum only) |

**Legend:**
- ✅ Fully mitigated
- 🔄 Partially mitigated
- ❌ Not yet mitigated

---

## 8. Testing Failure Scenarios

### Chaos Engineering Tests

```rust
#[tokio::test]
async fn test_bootstrap_timeout_fallback() {
    // Simulate bootstrap timeout
    let config = P2PConfig {
        bootstrap_nodes: vec![
            "/ip4/127.0.0.1/tcp/9999/p2p/fake".parse().unwrap()
        ],
        ..Default::default()
    };

    let start = Instant::now();
    let result = start_node_with_timeout(config, Duration::from_secs(5)).await;

    // Should fail within 5 seconds
    assert!(start.elapsed() < Duration::from_secs(6));

    // Should have fallen back to file registry
    assert!(result.is_err());
    assert!(matches!(result, Err(P2PError::BootstrapTimeout)));
}

#[tokio::test]
async fn test_checksum_mismatch_recovery() {
    // Create mock peer that returns corrupted data
    let bad_peer = create_mock_peer_with_bad_checksum().await;

    // Create good peer
    let good_peer = create_mock_peer_with_good_checksum().await;

    // Install should succeed by trying good peer after bad peer fails
    let result = install_from_peers(
        "test-pkg",
        vec![bad_peer.peer_id(), good_peer.peer_id()]
    ).await;

    assert!(result.is_ok());

    // Verify bad peer was marked with low reputation
    let bad_reputation = registry.get_peer_reputation(&bad_peer.peer_id()).await;
    assert!(bad_reputation < 0.5);
}
```

---

## 9. Incident Response Playbook

### User Reports "Package Not Found"

**Investigation Steps:**
1. Check if package exists in file registry: `ls ~/.ggen/registry/`
2. Check P2P connectivity: `ggen marketplace p2p status`
3. Check network connectivity: `ping 8.8.8.8`
4. Retry with verbose logging: `RUST_LOG=debug ggen marketplace search <pkg>`

**Resolution:**
- If file registry: Package doesn't exist (user error)
- If P2P down: `ggen marketplace p2p start --bootstrap <nodes>`
- If network down: User fixes network, automatic recovery

---

### User Reports "Installation Hangs"

**Investigation Steps:**
1. Check for lock file: `ls ~/.ggen/packages/.ggen-install.lock`
2. Check running processes: `ps aux | grep ggen`
3. Check disk space: `df -h ~/.ggen`

**Resolution:**
- If lock file exists: Remove stale lock `rm ~/.ggen/packages/.ggen-install.lock`
- If process stuck: Kill process `kill <pid>`
- If disk full: User frees space `ggen marketplace clean`

---

**Document Version:** 1.0
**Last Updated:** 2025-11-02
**Next Review:** 2025-12-02
