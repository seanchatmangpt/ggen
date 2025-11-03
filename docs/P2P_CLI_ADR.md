# Architecture Decision Records - P2P CLI Integration

**Project:** ggen Marketplace P2P Integration
**Architect:** System Architecture Agent
**Date:** 2025-11-02

---

## ADR-001: Use clap-noun-verb v3.0.0 for CLI Structure

**Status:** ✅ ACCEPTED

**Date:** 2025-11-02

### Context

The ggen CLI currently uses clap-noun-verb v3.0.0 for command routing, providing a consistent `noun verb [args]` pattern. We need to integrate P2P marketplace functionality into this existing architecture.

### Decision

Adopt `marketplace p2p [verb]` command structure following clap-noun-verb v3.0.0 patterns:
- **Noun:** `marketplace` (existing)
- **Sub-noun:** `p2p` (new)
- **Verbs:** `start`, `status`, `search`, `publish`, etc.

### Alternatives Considered

1. **Top-level `p2p` noun:**
   ```bash
   ggen p2p start
   ggen p2p search "query"
   ```
   - ❌ Less clear relationship to marketplace
   - ❌ Potential confusion with network tools

2. **Marketplace flag:**
   ```bash
   ggen marketplace search --p2p "query"
   ```
   - ❌ Less discoverable
   - ❌ Doesn't fit noun-verb pattern

3. **Separate binary:**
   ```bash
   ggen-p2p start
   ```
   - ❌ Additional installation complexity
   - ❌ Breaks unified CLI experience

### Consequences

**Positive:**
- ✅ Consistent with existing CLI patterns
- ✅ Clear namespace: marketplace → p2p
- ✅ Discoverable via `ggen marketplace --help`
- ✅ Follows clap-noun-verb best practices
- ✅ Reuses existing marketplace infrastructure

**Negative:**
- ⚠️ Longer command paths (3 levels)
- ⚠️ More typing for users

**Mitigation:**
- Provide shell aliases in documentation
- Support command abbreviations
- Create bash completion scripts

---

## ADR-002: Embed P2P Node in CLI Process

**Status:** ✅ ACCEPTED

**Date:** 2025-11-02

### Context

P2P node can run as:
1. Embedded in CLI process
2. Separate daemon process
3. System service (systemd)

### Decision

Embed libp2p swarm directly in the CLI process, managed by a global NodeManager with optional daemon mode.

### Implementation

```rust
// Global state management
static GLOBAL_REGISTRY: OnceCell<Arc<RwLock<Option<P2PRegistry>>>> = OnceCell::new();

// Start node
pub async fn start_node(config: P2PConfig) -> Result<()> {
    let registry = P2PRegistry::new(config).await?;
    set_global_registry(registry).await?;

    // Event loop runs in tokio runtime
    loop {
        registry.process_events().await;
    }
}
```

### Alternatives Considered

1. **Separate daemon:**
   ```bash
   ggen-p2p-daemon &
   ggen marketplace p2p search "query"  # Connects to daemon
   ```
   - ❌ Complex IPC (Unix sockets, gRPC)
   - ❌ Lifecycle management (start, stop, restart)
   - ❌ Additional deployment complexity
   - ✅ Persistent node across commands

2. **System service:**
   ```bash
   systemctl start ggen-p2p
   ```
   - ❌ Requires root privileges
   - ❌ Platform-specific (systemd, launchd, etc.)
   - ❌ Overkill for single-user tool
   - ✅ Always available

### Consequences

**Positive:**
- ✅ Simple deployment (single binary)
- ✅ No IPC complexity
- ✅ Direct in-process communication
- ✅ Easier to debug
- ✅ Automatic cleanup on exit

**Negative:**
- ⚠️ Node stops when CLI exits
- ⚠️ Each command session starts fresh node (mitigated by daemon mode)
- ⚠️ State not persistent across invocations

**Mitigation:**
- Add `--daemon` flag for long-running node
- Cache package metadata locally
- Implement fast startup (<3s)

---

## ADR-003: Global Node Manager for State Sharing

**Status:** ✅ ACCEPTED

**Date:** 2025-11-02

### Context

Multiple CLI commands need access to the same P2P node instance within a single session. We need thread-safe global state management.

### Decision

Use `OnceCell<Arc<RwLock<Option<P2PRegistry>>>>` for global state:

```rust
pub mod node_manager {
    static GLOBAL_REGISTRY: OnceCell<Arc<RwLock<Option<P2PRegistry>>>> = OnceCell::new();

    pub async fn set_global_registry(registry: P2PRegistry) -> Result<()> {
        let lock = init_registry();
        let mut guard = lock.write().await;
        *guard = Some(registry);
        Ok(())
    }

    pub async fn get_global_registry() -> Result<P2PRegistry> {
        let lock = init_registry();
        let guard = lock.read().await;
        guard.as_ref()
            .ok_or_else(|| anyhow!("P2P node not started"))
            .cloned()
    }
}
```

### Alternatives Considered

1. **Pass registry as parameter:**
   ```rust
   pub fn run(args: &Args, registry: &P2PRegistry) -> Result<()>
   ```
   - ❌ Threading registry through all layers
   - ❌ Breaking API changes
   - ❌ Tight coupling

2. **Thread-local storage:**
   ```rust
   thread_local! {
       static REGISTRY: RefCell<Option<P2PRegistry>> = RefCell::new(None);
   }
   ```
   - ❌ Doesn't work with async runtime
   - ❌ Thread affinity issues

3. **Singleton pattern with lazy_static:**
   ```rust
   lazy_static! {
       static ref REGISTRY: Mutex<Option<P2PRegistry>> = Mutex::new(None);
   }
   ```
   - ❌ Blocking mutex (bad for async)
   - ✅ Simple API

### Consequences

**Positive:**
- ✅ Thread-safe access from multiple commands
- ✅ Works with async/await
- ✅ Single source of truth
- ✅ Clear lifecycle (start → use → stop)

**Negative:**
- ⚠️ Global mutable state
- ⚠️ Possible deadlocks if misused
- ⚠️ Must ensure initialization before use

**Best Practices:**
- Always check `is_running()` before accessing
- Provide helpful error messages if not started
- Document initialization requirements

---

## ADR-004: Hybrid Registry Strategy (Central + P2P)

**Status:** ✅ ACCEPTED

**Date:** 2025-11-02

### Context

Users need access to both centralized registries (crates.io, npm) and P2P networks. We need a strategy for querying multiple sources.

### Decision

Implement **parallel search with intelligent fallback**:

```rust
pub async fn search_packages(query: &str, filters: &SearchFilters) -> Result<Vec<SearchResult>> {
    match &filters.registry {
        Some(r) if r == "p2p" => search_p2p(query, filters).await,
        Some(r) if r == "central" => search_central(query, filters).await,
        _ => {
            // Parallel search both
            let (p2p_fut, central_fut) = tokio::join!(
                search_p2p(query, filters),
                search_central(query, filters)
            );

            let mut results = Vec::new();
            if let Ok(p2p) = p2p_fut { results.extend(p2p); }
            if let Ok(central) = central_fut { results.extend(central); }

            deduplicate_and_rank(&mut results);
            Ok(results)
        }
    }
}
```

### Alternatives Considered

1. **P2P only:**
   - ❌ Smaller package ecosystem initially
   - ❌ Misses centralized packages

2. **Central only with P2P fallback:**
   ```rust
   search_central().or_else(|| search_p2p())
   ```
   - ❌ P2P becomes second-class
   - ❌ Doesn't leverage P2P advantages

3. **Sequential search (central first, then P2P):**
   - ❌ Slower total time
   - ❌ Misses best results if timeout

### Consequences

**Positive:**
- ✅ Best of both worlds
- ✅ Graceful degradation
- ✅ Fast results (parallel queries)
- ✅ User can opt-in to specific registry

**Negative:**
- ⚠️ Duplicate packages need deduplication
- ⚠️ Ranking logic complexity
- ⚠️ Higher resource usage

**Deduplication Strategy:**
1. Group by package name
2. Prefer P2P if both have same version
3. Merge metadata (combine sources)
4. Sort by relevance score

---

## ADR-005: TOML Configuration Format

**Status:** ✅ ACCEPTED

**Date:** 2025-11-02

### Context

P2P node needs user-configurable settings (bootstrap nodes, listen addresses, security settings). We need a format that's:
- Human-readable
- Well-supported in Rust
- Consistent with Rust ecosystem

### Decision

Use **TOML** format for configuration file at `~/.ggen/p2p-config.toml`:

```toml
[network]
bootstrap_nodes = ["/ip4/104.131.131.82/tcp/4001/p2p/Qm..."]
listen_addresses = ["/ip4/0.0.0.0/tcp/0"]
dht_server_mode = true
packages_topic = "/ggen/packages/v1"

[performance]
max_peers = 50
dht_query_timeout = 30

[security]
enable_tls = true
min_peer_reputation = 0.7
```

### Alternatives Considered

1. **YAML:**
   ```yaml
   network:
     bootstrap_nodes:
       - /ip4/104.131.131.82/tcp/4001/p2p/Qm...
   ```
   - ❌ Whitespace-sensitive
   - ✅ More flexible
   - ⚠️ Less common in Rust

2. **JSON:**
   ```json
   {
     "network": {
       "bootstrap_nodes": ["..."]
     }
   }
   ```
   - ❌ No comments
   - ❌ Less human-friendly
   - ✅ Universal format

3. **Command-line flags:**
   ```bash
   ggen marketplace p2p start --bootstrap "..." --listen "..."
   ```
   - ❌ Too verbose for complex config
   - ❌ Not persistent
   - ✅ Good for overrides

### Consequences

**Positive:**
- ✅ Consistent with Cargo.toml
- ✅ Human-readable and editable
- ✅ Excellent Rust crate support (serde, toml)
- ✅ Supports comments for documentation
- ✅ Type-safe deserialization

**Negative:**
- ⚠️ Less flexible than YAML
- ⚠️ Another file to manage

**Configuration Hierarchy:**
1. Command-line flags (highest priority)
2. `~/.ggen/p2p-config.toml` (user config)
3. Built-in defaults (lowest priority)

---

## ADR-006: Async Runtime Bridge Pattern

**Status:** ✅ ACCEPTED

**Date:** 2025-11-02

### Context

CLI commands are synchronous (clap requires sync `fn`), but P2P operations are async (libp2p uses tokio). We need to bridge this gap.

### Decision

Use **runtime bridge utilities** at domain layer:

```rust
// cli/src/runtime.rs
pub fn execute<F>(future: F) -> Result<()>
where
    F: Future<Output = Result<()>>,
{
    let runtime = tokio::runtime::Runtime::new()?;
    runtime.block_on(future)
}

// cli/src/domain/marketplace/p2p/start.rs
pub fn run(args: &StartArgs) -> Result<()> {
    runtime::execute(start_async(args.clone()))
}

async fn start_async(args: StartArgs) -> Result<()> {
    // Async P2P operations here
}
```

### Alternatives Considered

1. **Make CLI commands async:**
   ```rust
   #[tokio::main]
   async fn main() -> Result<()> {
       let cli = Cli::parse();
       cli.execute().await
   }
   ```
   - ❌ clap doesn't support async command handlers
   - ❌ Breaking change to existing architecture

2. **Spawn threads for async work:**
   ```rust
   std::thread::spawn(|| {
       tokio::runtime::Runtime::new().unwrap().block_on(async_fn())
   }).join()
   ```
   - ❌ Awkward API
   - ❌ Error handling complexity

3. **Convert P2P to sync:**
   - ❌ libp2p is fundamentally async
   - ❌ Would require major refactoring

### Consequences

**Positive:**
- ✅ Clean separation of concerns
- ✅ Minimal changes to CLI layer
- ✅ Reusable runtime utilities
- ✅ Works with existing architecture

**Negative:**
- ⚠️ Creates new runtime per command
- ⚠️ Slight overhead

**Optimization:**
- Reuse runtime when possible
- Cache node state across commands (via NodeManager)

---

## ADR-007: Content-Addressed Storage for Packages

**Status:** ✅ ACCEPTED

**Date:** 2025-11-02

### Context

P2P networks need unique, verifiable identifiers for packages. Traditional name+version can have conflicts.

### Decision

Use **SHA-256 content hash** as package ID in P2P network:

```rust
pub struct PackageId(String);  // SHA-256 hex string

impl PackageId {
    pub fn from_content(content: &[u8]) -> Self {
        let hash = sha256::digest(content);
        PackageId(hash)
    }
}

// DHT key generation
let key = kad::RecordKey::new(&package_id.to_string().as_bytes());
```

### Alternatives Considered

1. **Name + version:**
   ```rust
   PackageId("my-package@1.0.0")
   ```
   - ❌ Collision possible
   - ❌ Mutable (same ID, different content)
   - ✅ Human-readable

2. **UUID:**
   ```rust
   PackageId(Uuid::new_v4())
   ```
   - ❌ Not verifiable
   - ❌ Not content-addressed
   - ✅ Guaranteed unique

3. **Git-style SHA-1:**
   - ⚠️ SHA-1 has known weaknesses
   - ✅ Proven in Git

### Consequences

**Positive:**
- ✅ Content-addressed (verifiable integrity)
- ✅ Immutable (same content = same ID)
- ✅ Deduplication built-in
- ✅ No central authority needed

**Negative:**
- ⚠️ Not human-readable
- ⚠️ Need mapping from name@version → hash

**Hybrid Approach:**
- Use content hash internally for P2P
- Maintain name@version → hash mapping
- Display friendly names to users

---

## ADR-008: Peer Reputation System

**Status:** ✅ ACCEPTED

**Date:** 2025-11-02

### Context

P2P networks can have malicious or unreliable peers. We need a trust system to:
- Prefer reliable peers
- Ban malicious peers
- Recover from failures

### Decision

Implement **simple reputation scoring**:

```rust
struct PeerReputation {
    peer_id: PeerId,
    successful_retrievals: u64,
    failed_retrievals: u64,
    last_seen: DateTime<Utc>,
}

impl PeerReputation {
    fn success_rate(&self) -> f64 {
        let total = self.successful_retrievals + self.failed_retrievals;
        if total == 0 { return 1.0; }
        self.successful_retrievals as f64 / total as f64
    }
}

// Usage
pub async fn get_package(&self, id: &PackageId) -> Result<Package> {
    let peers = self.find_providers(id).await?;

    // Sort by reputation
    let mut peers = peers.into_iter()
        .filter(|p| self.get_reputation(p).await > 0.7)
        .collect::<Vec<_>>();
    peers.sort_by_key(|p| self.get_reputation(p));

    // Try peers in order
    for peer in peers {
        match self.retrieve_from(peer, id).await {
            Ok(package) => {
                self.record_success(peer).await;
                return Ok(package);
            }
            Err(_) => self.record_failure(peer).await,
        }
    }

    Err(anyhow!("Package not found"))
}
```

### Alternatives Considered

1. **No reputation (trust all peers equally):**
   - ❌ Vulnerable to malicious peers
   - ❌ No learning from failures
   - ✅ Simplest implementation

2. **EigenTrust algorithm:**
   - ✅ Proven P2P reputation system
   - ❌ Complex implementation
   - ❌ Requires global view

3. **Blockchain-based reputation:**
   - ❌ Overkill for package registry
   - ❌ Slow consensus
   - ❌ Additional infrastructure

### Consequences

**Positive:**
- ✅ Simple to implement
- ✅ Local decision-making (no consensus)
- ✅ Adaptive (learns over time)
- ✅ Configurable thresholds

**Negative:**
- ⚠️ Slow peers penalized unfairly
- ⚠️ Cold start problem (new peers unknown)
- ⚠️ Sybil attack vulnerability

**Security Measures:**
- Set minimum reputation threshold (0.7)
- Ban peers after N failures (configurable)
- Gradual reputation recovery
- Rate limiting per peer

---

## Summary Table

| ADR | Title | Status | Key Decision |
|-----|-------|--------|--------------|
| ADR-001 | CLI Structure | ✅ Accepted | `marketplace p2p [verb]` pattern |
| ADR-002 | Node Deployment | ✅ Accepted | Embedded in CLI process with daemon mode |
| ADR-003 | State Management | ✅ Accepted | Global NodeManager with RwLock |
| ADR-004 | Registry Strategy | ✅ Accepted | Hybrid parallel search (central + P2P) |
| ADR-005 | Configuration | ✅ Accepted | TOML format at ~/.ggen/p2p-config.toml |
| ADR-006 | Runtime Bridge | ✅ Accepted | Sync CLI → async domain via runtime::execute() |
| ADR-007 | Package IDs | ✅ Accepted | SHA-256 content hash (content-addressed) |
| ADR-008 | Peer Trust | ✅ Accepted | Simple reputation scoring with banning |

---

## Future ADRs to Consider

- **ADR-009:** Package Versioning in P2P (how to handle multiple versions)
- **ADR-010:** Garbage Collection for P2P Cache
- **ADR-011:** Cross-Platform Bootstrap Nodes (discovery mechanism)
- **ADR-012:** Package Signing and Verification
- **ADR-013:** Bandwidth Limits and Rate Limiting
- **ADR-014:** Metrics and Telemetry Collection

---

**Document Version:** 1.0.0
**Last Updated:** 2025-11-02
**Status:** APPROVED
**Next Review:** After implementation of Phase 1-3
