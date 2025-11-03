# P2P Marketplace Integration Flow Diagram

## Package Discovery and Installation Flow

```
┌─────────────────────────────────────────────────────────────────────────────┐
│                     User: ggen marketplace install pkg@1.0.0                 │
└─────────────────────────────────────────────────────────────────────────────┘
                                    │
                                    ▼
┌─────────────────────────────────────────────────────────────────────────────┐
│                          CLI Layer (clap parsing)                            │
│  cli/src/cmds/marketplace.rs                                                 │
│  ┌───────────────────────────────────────────────────────────────────────┐  │
│  │  MarketplaceCmd::Install(InstallArgs)                                 │  │
│  │  • package: "pkg@1.0.0"                                               │  │
│  │  • target: None                                                       │  │
│  │  • force: false                                                       │  │
│  │  • no_dependencies: false                                             │  │
│  └───────────────────────────────────────────────────────────────────────┘  │
└─────────────────────────────────────────────────────────────────────────────┘
                                    │
                                    │ calls domain function
                                    ▼
┌─────────────────────────────────────────────────────────────────────────────┐
│                       Domain Layer (business logic)                          │
│  cli/src/domain/marketplace/install.rs                                       │
│  ┌───────────────────────────────────────────────────────────────────────┐  │
│  │  install_package(&InstallOptions) -> Result<InstallResult>           │  │
│  │                                                                       │  │
│  │  Step 1: Parse package spec                                          │  │
│  │    "pkg@1.0.0" → (name: "pkg", version: Some("1.0.0"))              │  │
│  │                                                                       │  │
│  │  Step 2: Resolve version from registry                               │  │
│  │    resolve_version("pkg", "1.0.0", registry_path)                    │  │
│  │      │                                                                │  │
│  │      ├─ Check if "1.0.0" is "latest" → get_latest_version()         │  │
│  │      ├─ Handle semver ranges (^1.0.0, ~1.0.0, >=1.0.0)              │  │
│  │      └─ Return exact version: "1.0.0"                                │  │
│  │                                                                       │  │
│  │  Step 3: Load package manifest                                       │  │
│  │    load_package_manifest("pkg", "1.0.0", registry_path)              │  │
│  │      │                                                                │  │
│  │      └─ Read: ~/.ggen/registry/pkg/1.0.0/package.json               │  │
│  │         Returns: PackageManifest { name, version, dependencies }     │  │
│  │                                                                       │  │
│  │  Step 4: Build dependency graph                                      │  │
│  │    ┌──────────────────────────────────────────────────────────────┐ │  │
│  │    │  DependencyGraph::new()                                       │ │  │
│  │    │  • Add main package to graph                                  │ │  │
│  │    │  • BFS traversal of dependencies                              │ │  │
│  │    │    - For each dependency:                                     │ │  │
│  │    │      * Resolve version                                        │ │  │
│  │    │      * Load manifest                                          │ │  │
│  │    │      * Add to graph                                           │ │  │
│  │    │      * Queue child dependencies                               │ │  │
│  │    │  • Detect circular dependencies (DFS cycle check)             │ │  │
│  │    │  • Topological sort for install order                         │ │  │
│  │    └──────────────────────────────────────────────────────────────┘ │  │
│  │                                                                       │  │
│  │  Step 5: Install packages in order                                   │  │
│  │    For each package in topological order:                            │  │
│  │      ┌────────────────────────────────────────────────────────────┐ │  │
│  │      │  1. Get tarball path                                        │ │  │
│  │      │     ~/.ggen/registry/pkg/1.0.0/pkg-1.0.0.tar.gz           │ │  │
│  │      │                                                             │ │  │
│  │      │  2. Extract tarball to target                              │ │  │
│  │      │     extract_tarball(tarball_path, ~/.ggen/packages/pkg)    │ │  │
│  │      │       │                                                     │ │  │
│  │      │       ├─ Open tarball with flate2::GzDecoder              │ │  │
│  │      │       ├─ Create tar::Archive                               │ │  │
│  │      │       └─ Unpack to target directory                        │ │  │
│  │      │                                                             │ │  │
│  │      │  3. Calculate checksum (MD5)                               │ │  │
│  │      │     calculate_checksum(tarball_path)                       │ │  │
│  │      │                                                             │ │  │
│  │      │  4. Update lockfile                                        │ │  │
│  │      │     ggen.lock: {                                           │ │  │
│  │      │       "pkg@1.0.0": {                                       │ │  │
│  │      │         name: "pkg",                                       │ │  │
│  │      │         version: "1.0.0",                                  │ │  │
│  │      │         resolved: "/path/to/tarball",                     │ │  │
│  │      │         integrity: "md5-...",                              │ │  │
│  │      │         dependencies: {...}                                │ │  │
│  │      │       }                                                     │ │  │
│  │      │     }                                                       │ │  │
│  │      │                                                             │ │  │
│  │      │  5. Rollback on failure                                    │ │  │
│  │      │     If extraction fails:                                   │ │  │
│  │      │       - Remove all partially installed packages            │ │  │
│  │      │       - Revert lockfile changes                            │ │  │
│  │      │       - Return error to user                               │ │  │
│  │      └────────────────────────────────────────────────────────────┘ │  │
│  │                                                                       │  │
│  │  Step 6: Return result                                                │  │
│  │    InstallResult {                                                    │  │
│  │      package_name: "pkg",                                             │  │
│  │      version: "1.0.0",                                                │  │
│  │      install_path: ~/.ggen/packages/pkg,                             │  │
│  │      dependencies_installed: ["dep1@2.0.0", "dep2@3.1.0"]            │  │
│  │    }                                                                  │  │
│  └───────────────────────────────────────────────────────────────────────┘  │
└─────────────────────────────────────────────────────────────────────────────┘
                                    │
                                    │ uses backend (future: Registry trait)
                                    ▼
┌─────────────────────────────────────────────────────────────────────────────┐
│                     Backend Layer (storage & networking)                     │
│  ggen-marketplace/src/                                                       │
│  ┌───────────────────────────────────────────────────────────────────────┐  │
│  │  Registry Trait (abstraction for backends)                            │  │
│  │                                                                        │  │
│  │  trait Registry {                                                     │  │
│  │    async fn search(&self, query: &Query) -> Result<Vec<Package>>;    │  │
│  │    async fn get_package(&self, id: &PackageId) -> Result<Package>;   │  │
│  │    async fn publish(&self, package: Package) -> Result<()>;          │  │
│  │    // ... other methods                                               │  │
│  │  }                                                                     │  │
│  └───────────────────────────────────────────────────────────────────────┘  │
│                                    │                                         │
│                    ┌───────────────┴───────────────┐                        │
│                    ▼                               ▼                        │
│  ┌─────────────────────────────┐   ┌──────────────────────────────────┐   │
│  │  FileRegistry (implicit)    │   │  P2PRegistry (libp2p)             │   │
│  │                             │   │                                   │   │
│  │  • Load index.json          │   │  • Kademlia DHT queries           │   │
│  │  • Parse package metadata   │   │  • Gossipsub announcements        │   │
│  │  • Cache with LRU           │   │  • Peer reputation tracking       │   │
│  │  • Filesystem operations    │   │  • Local package cache            │   │
│  └─────────────────────────────┘   └──────────────────────────────────┘   │
└─────────────────────────────────────────────────────────────────────────────┘
```

## P2P Package Discovery Flow

```
┌─────────────────────────────────────────────────────────────────────────────┐
│              User: ggen marketplace p2p search "web framework"               │
└─────────────────────────────────────────────────────────────────────────────┘
                                    │
                                    ▼
┌─────────────────────────────────────────────────────────────────────────────┐
│                          CLI Layer (P2P command)                             │
│  cli/src/domain/marketplace/p2p.rs                                           │
│  ┌───────────────────────────────────────────────────────────────────────┐  │
│  │  P2PCommand::Search(SearchArgs {                                      │  │
│  │    query: "web framework",                                            │  │
│  │    category: None,                                                    │  │
│  │    tags: [],                                                          │  │
│  │    limit: 20,                                                         │  │
│  │    min_reputation: 0.5                                                │  │
│  │  })                                                                   │  │
│  └───────────────────────────────────────────────────────────────────────┘  │
└─────────────────────────────────────────────────────────────────────────────┘
                                    │
                                    │ execute_p2p_command()
                                    ▼
┌─────────────────────────────────────────────────────────────────────────────┐
│                       P2P Backend (libp2p integration)                       │
│  ggen-marketplace/src/backend/p2p.rs                                         │
│  ┌───────────────────────────────────────────────────────────────────────┐  │
│  │  P2PRegistry::search(&Query) -> Result<Vec<Package>>                  │  │
│  │                                                                        │  │
│  │  ┌──────────────────────────────────────────────────────────────────┐ │  │
│  │  │  Phase 1: Search Local Packages                                  │ │  │
│  │  │  ┌────────────────────────────────────────────────────────────┐  │ │  │
│  │  │  │  let local_packages = self.local_packages.read().await;    │  │ │  │
│  │  │  │  let mut results = local_packages.values()                 │  │ │  │
│  │  │  │    .filter(|pkg| {                                         │  │ │  │
│  │  │  │      // Text matching                                      │  │ │  │
│  │  │  │      let text_match = pkg.metadata.title                   │  │ │  │
│  │  │  │        .to_lowercase()                                     │  │ │  │
│  │  │  │        .contains(&query.text.to_lowercase());              │  │ │  │
│  │  │  │      // Category filtering                                 │  │ │  │
│  │  │  │      let category_match = query.categories.is_empty() ||   │  │ │  │
│  │  │  │        pkg.metadata.categories.iter()                      │  │ │  │
│  │  │  │          .any(|c| query.categories.contains(c));           │  │ │  │
│  │  │  │      text_match && category_match                          │  │ │  │
│  │  │  │    })                                                       │  │ │  │
│  │  │  │    .cloned()                                               │  │ │  │
│  │  │  │    .collect();                                             │  │ │  │
│  │  │  └────────────────────────────────────────────────────────────┘  │ │  │
│  │  └──────────────────────────────────────────────────────────────────┘ │  │
│  │                                                                        │  │
│  │  ┌──────────────────────────────────────────────────────────────────┐ │  │
│  │  │  Phase 2: Query Kademlia DHT                                     │ │  │
│  │  │  ┌────────────────────────────────────────────────────────────┐  │ │  │
│  │  │  │  let discovered = self.discovered_packages.read().await;   │  │ │  │
│  │  │  │  for (package_id, providers) in discovered.iter() {        │  │ │  │
│  │  │  │    // Query DHT for package metadata                       │  │ │  │
│  │  │  │    let key = RecordKey::new(package_id.to_string());       │  │ │  │
│  │  │  │    swarm.behaviour_mut().kademlia.get_record(key);         │  │ │  │
│  │  │  │                                                             │  │ │  │
│  │  │  │    // Wait for DHT response (async event)                  │  │ │  │
│  │  │  │    if let Some(package) = query_dht_result {               │  │ │  │
│  │  │  │      // Apply same filters as local search                 │  │ │  │
│  │  │  │      if matches_query(&package, query) {                   │  │ │  │
│  │  │  │        // Check provider reputation                        │  │ │  │
│  │  │  │        let reputation = get_peer_reputation(provider);     │  │ │  │
│  │  │  │        if reputation >= query.min_reputation {             │  │ │  │
│  │  │  │          results.push(package);                            │  │ │  │
│  │  │  │        }                                                    │  │ │  │
│  │  │  │      }                                                      │  │ │  │
│  │  │  │    }                                                        │  │ │  │
│  │  │  │  }                                                          │  │ │  │
│  │  │  └────────────────────────────────────────────────────────────┘  │ │  │
│  │  └──────────────────────────────────────────────────────────────────┘ │  │
│  │                                                                        │  │
│  │  ┌──────────────────────────────────────────────────────────────────┐ │  │
│  │  │  Phase 3: Merge and Deduplicate Results                          │ │  │
│  │  │  ┌────────────────────────────────────────────────────────────┐  │ │  │
│  │  │  │  // Remove duplicates by PackageId                         │  │ │  │
│  │  │  │  let mut seen_ids = HashSet::new();                        │  │ │  │
│  │  │  │  results.retain(|pkg| {                                    │  │ │  │
│  │  │  │    if seen_ids.contains(&pkg.id) {                         │  │ │  │
│  │  │  │      false  // Duplicate, remove                           │  │ │  │
│  │  │  │    } else {                                                 │  │ │  │
│  │  │  │      seen_ids.insert(pkg.id.clone());                      │  │ │  │
│  │  │  │      true   // Keep                                         │  │ │  │
│  │  │  │    }                                                        │  │ │  │
│  │  │  │  });                                                        │  │ │  │
│  │  │  │                                                             │  │ │  │
│  │  │  │  // Apply limit                                            │  │ │  │
│  │  │  │  if let Some(limit) = query.limit {                        │  │ │  │
│  │  │  │    results.truncate(limit);                                │  │ │  │
│  │  │  │  }                                                          │  │ │  │
│  │  │  └────────────────────────────────────────────────────────────┘  │ │  │
│  │  └──────────────────────────────────────────────────────────────────┘ │  │
│  │                                                                        │  │
│  │  Return: Vec<Package>                                                 │  │
│  └───────────────────────────────────────────────────────────────────────┘  │
└─────────────────────────────────────────────────────────────────────────────┘
                                    │
                                    │ network interaction
                                    ▼
┌─────────────────────────────────────────────────────────────────────────────┐
│                           P2P Network (libp2p)                               │
│  ┌───────────────────────────────────────────────────────────────────────┐  │
│  │  Kademlia DHT                                                         │  │
│  │  • Key-value store: PackageId → Package metadata                     │  │
│  │  • Distributed routing table (XOR distance metric)                   │  │
│  │  • Query propagation to k-closest peers                              │  │
│  └───────────────────────────────────────────────────────────────────────┘  │
│  ┌───────────────────────────────────────────────────────────────────────┐  │
│  │  Gossipsub                                                            │  │
│  │  • Topic: /ggen/packages/v1                                          │  │
│  │  • Broadcast package announcements to all subscribers                │  │
│  │  • Message deduplication (by message ID)                             │  │
│  └───────────────────────────────────────────────────────────────────────┘  │
│  ┌───────────────────────────────────────────────────────────────────────┐  │
│  │  Peer Reputation                                                      │  │
│  │  • Track success/failure per peer                                    │  │
│  │  • Calculate reputation: successes / (successes + failures)          │  │
│  │  • Filter peers below min_reputation threshold                       │  │
│  └───────────────────────────────────────────────────────────────────────┘  │
└─────────────────────────────────────────────────────────────────────────────┘
```

## P2P Package Publishing Flow

```
┌─────────────────────────────────────────────────────────────────────────────┐
│           User: ggen marketplace p2p publish ./my-package                    │
└─────────────────────────────────────────────────────────────────────────────┘
                                    │
                                    ▼
┌─────────────────────────────────────────────────────────────────────────────┐
│                          CLI Layer (P2P command)                             │
│  cli/src/domain/marketplace/p2p.rs                                           │
│  ┌───────────────────────────────────────────────────────────────────────┐  │
│  │  P2PCommand::Publish(PublishArgs {                                    │  │
│  │    path: "./my-package",                                              │  │
│  │    version: None,                                                     │  │
│  │    skip_verify: false                                                 │  │
│  │  })                                                                   │  │
│  └───────────────────────────────────────────────────────────────────────┘  │
└─────────────────────────────────────────────────────────────────────────────┘
                                    │
                                    │ publish_package()
                                    ▼
┌─────────────────────────────────────────────────────────────────────────────┐
│                       P2P Backend (package publication)                      │
│  ggen-marketplace/src/backend/p2p.rs                                         │
│  ┌───────────────────────────────────────────────────────────────────────┐  │
│  │  P2PRegistry::publish(Package) -> Result<()>                          │  │
│  │                                                                        │  │
│  │  ┌──────────────────────────────────────────────────────────────────┐ │  │
│  │  │  Phase 1: Store Locally                                          │ │  │
│  │  │  ┌────────────────────────────────────────────────────────────┐  │ │  │
│  │  │  │  let package_id = package.id.clone();                      │  │ │  │
│  │  │  │  self.local_packages                                        │  │ │  │
│  │  │  │    .write().await                                           │  │ │  │
│  │  │  │    .insert(package_id.clone(), package.clone());            │  │ │  │
│  │  │  └────────────────────────────────────────────────────────────┘  │ │  │
│  │  └──────────────────────────────────────────────────────────────────┘ │  │
│  │                                                                        │  │
│  │  ┌──────────────────────────────────────────────────────────────────┐ │  │
│  │  │  Phase 2: Store in Kademlia DHT                                  │ │  │
│  │  │  ┌────────────────────────────────────────────────────────────┐  │ │  │
│  │  │  │  // Serialize package metadata                             │  │ │  │
│  │  │  │  let key = RecordKey::new(package_id.to_string());         │  │ │  │
│  │  │  │  let value = serde_json::to_vec(&package)?;                │  │ │  │
│  │  │  │                                                             │  │ │  │
│  │  │  │  // Create DHT record                                      │  │ │  │
│  │  │  │  let record = Record {                                     │  │ │  │
│  │  │  │    key,                                                     │  │ │  │
│  │  │  │    value,                                                   │  │ │  │
│  │  │  │    publisher: Some(self.peer_id),                          │  │ │  │
│  │  │  │    expires: None,  // Permanent storage                    │  │ │  │
│  │  │  │  };                                                         │  │ │  │
│  │  │  │                                                             │  │ │  │
│  │  │  │  // Store in DHT (replicated to k closest peers)           │  │ │  │
│  │  │  │  swarm.behaviour_mut()                                     │  │ │  │
│  │  │  │    .kademlia                                               │  │ │  │
│  │  │  │    .put_record(record, Quorum::One)?;                      │  │ │  │
│  │  │  └────────────────────────────────────────────────────────────┘  │ │  │
│  │  └──────────────────────────────────────────────────────────────────┘ │  │
│  │                                                                        │  │
│  │  ┌──────────────────────────────────────────────────────────────────┐ │  │
│  │  │  Phase 3: Announce via Gossipsub                                 │ │  │
│  │  │  ┌────────────────────────────────────────────────────────────┐  │ │  │
│  │  │  │  // Serialize announcement                                 │  │ │  │
│  │  │  │  let announcement = serde_json::to_vec(&package)?;         │  │ │  │
│  │  │  │                                                             │  │ │  │
│  │  │  │  // Publish to /ggen/packages/v1 topic                     │  │ │  │
│  │  │  │  swarm.behaviour_mut()                                     │  │ │  │
│  │  │  │    .gossipsub                                              │  │ │  │
│  │  │  │    .publish(self.packages_topic.clone(), announcement)?;   │  │ │  │
│  │  │  │                                                             │  │ │  │
│  │  │  │  // Message propagates to all peers subscribed to topic    │  │ │  │
│  │  │  └────────────────────────────────────────────────────────────┘  │ │  │
│  │  └──────────────────────────────────────────────────────────────────┘ │  │
│  │                                                                        │  │
│  │  Success: Package published                                           │  │
│  │  • Stored locally: ✓                                                  │  │
│  │  • Stored in DHT: ✓                                                   │  │
│  │  • Announced to network: ✓                                            │  │
│  └───────────────────────────────────────────────────────────────────────┘  │
└─────────────────────────────────────────────────────────────────────────────┘
                                    │
                                    │ network propagation
                                    ▼
┌─────────────────────────────────────────────────────────────────────────────┐
│                           P2P Network (libp2p)                               │
│  ┌───────────────────────────────────────────────────────────────────────┐  │
│  │  DHT Replication                                                      │  │
│  │  • Record stored on k=20 closest peers by XOR distance               │  │
│  │  • Each peer re-announces periodically (DHT maintenance)             │  │
│  │  • Survives individual peer failures                                 │  │
│  └───────────────────────────────────────────────────────────────────────┘  │
│  ┌───────────────────────────────────────────────────────────────────────┐  │
│  │  Gossipsub Propagation                                                │  │
│  │  • Message floods to all subscribed peers (flood-subscribe)          │  │
│  │  • Peers forward to their neighbors (up to D=6 hops)                 │  │
│  │  • Message ID deduplication prevents loops                           │  │
│  │  • Typical propagation time: <2 seconds for 100 peers                │  │
│  └───────────────────────────────────────────────────────────────────────┘  │
│  ┌───────────────────────────────────────────────────────────────────────┐  │
│  │  Remote Peer Reception                                                │  │
│  │  • Receives Gossipsub message                                         │  │
│  │  • Deserializes Package metadata                                      │  │
│  │  • Adds to discovered_packages cache                                  │  │
│  │  • Updates provider list for PackageId                                │  │
│  └───────────────────────────────────────────────────────────────────────┘  │
└─────────────────────────────────────────────────────────────────────────────┘
```

## Error Handling and Fallback Flow

```
┌─────────────────────────────────────────────────────────────────────────────┐
│                     User: ggen marketplace install pkg@1.0.0                 │
└─────────────────────────────────────────────────────────────────────────────┘
                                    │
                                    ▼
                  ┌─────────────────────────────────────┐
                  │  Feature check: P2P enabled?        │
                  └─────────────────────────────────────┘
                            │                │
                     #[cfg(feature = "p2p")]  │
                            │                │ #[cfg(not(feature = "p2p"))]
                            ▼                ▼
         ┌──────────────────────┐    ┌─────────────────────────┐
         │  Try P2P Registry    │    │  Use File Registry      │
         └──────────────────────┘    └─────────────────────────┘
                  │                              │
                  │                              │
       ┌──────────▼──────────┐                  │
       │  P2PRegistry::new() │                  │
       └──────────┬──────────┘                  │
                  │                              │
         ┌────────▼────────┐                    │
         │  Success?       │                    │
         └────────┬────────┘                    │
                  │                              │
         Yes ─────┤                              │
                  │                              │
                  ▼                              │
  ┌───────────────────────────────────┐         │
  │  P2P Registry Active              │         │
  └───────────────────────────────────┘         │
                  │                              │
                  ▼                              │
  ┌───────────────────────────────────┐         │
  │  Query P2P network                │         │
  │  • Check local_packages           │         │
  │  • Query Kademlia DHT             │         │
  │  • Wait up to 10s for response    │         │
  └───────────────────────────────────┘         │
                  │                              │
         ┌────────▼────────┐                    │
         │  Package found? │                    │
         └────────┬────────┘                    │
                  │                              │
         Yes ─────┤                              │
                  │                              │
                  ▼                              │
  ┌───────────────────────────────────┐         │
  │  Verify checksum                  │         │
  │  • Download tarball               │         │
  │  • Calculate SHA256               │         │
  │  • Compare with metadata          │         │
  └───────────────────────────────────┘         │
                  │                              │
         ┌────────▼────────┐                    │
         │  Checksum OK?   │                    │
         └────────┬────────┘                    │
                  │                              │
         Yes ─────┤                              │
                  │                              │
                  ▼                              │
  ┌───────────────────────────────────┐         │
  │  Extract and install              │         │
  └───────────────────────────────────┘         │
                  │                              │
                  ▼                              │
        SUCCESS ✓✓✓                             │
                                                 │
         No (any failure) ───────────────────────┤
                                                 │
                                                 ▼
                        ┌────────────────────────────────────┐
                        │  Fallback to File Registry         │
                        │  warn!("P2P failed, using local")  │
                        └────────────────────────────────────┘
                                        │
                                        ▼
                        ┌────────────────────────────────────┐
                        │  Load ~/.ggen/registry/index.json │
                        └────────────────────────────────────┘
                                        │
                                        ▼
                        ┌────────────────────────────────────┐
                        │  Search for package locally        │
                        └────────────────────────────────────┘
                                        │
                               ┌────────▼────────┐
                               │  Package found? │
                               └────────┬────────┘
                                        │
                               Yes ─────┤
                                        │
                                        ▼
                        ┌────────────────────────────────────┐
                        │  Extract from local tarball        │
                        └────────────────────────────────────┘
                                        │
                                        ▼
                              SUCCESS (fallback) ✓

                               No ──────┤
                                        │
                                        ▼
                                ERROR: Package not found ✗
```

---

**Version:** 1.0
**Last Updated:** 2025-11-02
