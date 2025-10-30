# Marketplace Technology Stack - Quick Reference

## Recommended Stack at a Glance

### Core Infrastructure
```toml
libp2p = "0.56"              # P2P networking foundation
tantivy = "0.25"             # Full-text search engine
redb = "2.1"                 # Embedded database
```

### Performance & Security
```toml
moka = "0.12"                # High-performance caching
ed25519-dalek = "2.1"        # Digital signatures
postcard = "1.0"             # Efficient serialization
```

### API & Extensions
```toml
async-graphql = "7.0"        # GraphQL API
wasmtime = "26.0"            # WebAssembly plugins
opentelemetry = "0.27"       # Observability
```

### Content Addressing
```toml
multihash = "0.19"           # Self-describing hashes
cid = "0.11"                 # Content identifiers
iroh = "0.26"                # P2P content distribution
```

---

## Decision Matrix

| Need | Library | Why |
|------|---------|-----|
| P2P networking | **libp2p** | Most mature, proven in IPFS/Polkadot |
| Search | **tantivy** | Embeddable, fast, BM25 scoring |
| Caching | **moka** | 85%+ hit rate in production (crates.io) |
| Signatures | **ed25519-dalek** | Pure Rust, constant-time, batch verification |
| GraphQL | **async-graphql** | Full async, Apollo Federation, subscriptions |
| WASM runtime | **wasmtime** | Bytecode Alliance, best performance |
| Database | **redb** | Pure Rust, fastest writes, ACID |
| Serialization | **postcard** | Best size/speed balance (70% size of bincode) |
| Content addressing | **multihash + cid** | IPFS standard, future-proof |
| Metrics | **opentelemetry** | Unified observability, vendor-neutral |

---

## Performance Highlights

| Component | Benchmark | Result |
|-----------|-----------|--------|
| libp2p TCP | Throughput | 353 Mbit/s download |
| tantivy | Indexing | Multi-threaded, BM25 |
| moka | Cache hit rate | 85%+ (production) |
| ed25519-dalek | Signing | Constant-time |
| wasmtime | Execution | Near-native (JIT) |
| redb | Bulk writes | 395ms (2.9x faster than rocksdb) |
| postcard | Serialization | 60ns, 30% smaller than bincode |

---

## Integration Complexity

### 🟢 Simple (< 1 day)
- moka (caching)
- postcard (serialization)
- ed25519-dalek (basic signing)

### 🟡 Moderate (2-3 days)
- tantivy (search engine)
- redb (database)
- async-graphql (API)
- multihash + cid (content addressing)

### 🔴 Complex (1-2 weeks)
- libp2p (P2P networking)
- wasmtime (plugin system)
- opentelemetry (full observability)

---

## Production Checklist

### Must-Have (MVP)
- [ ] libp2p P2P networking
- [ ] redb embedded database
- [ ] ed25519-dalek signatures
- [ ] postcard serialization
- [ ] Basic metrics

### Should-Have (v1.0)
- [ ] tantivy full-text search
- [ ] moka caching layer
- [ ] async-graphql API
- [ ] Content addressing (multihash + cid)
- [ ] opentelemetry metrics

### Nice-to-Have (v1.1+)
- [ ] wasmtime plugin system
- [ ] iroh content distribution
- [ ] OpenTelemetry tracing
- [ ] quinn QUIC transport

---

## Key Design Patterns

### 1. Layered Architecture
```
┌─────────────────────────────┐
│   GraphQL API (async-graphql)│
├─────────────────────────────┤
│   Business Logic Layer       │
├─────────────────────────────┤
│   Cache Layer (moka)         │
├─────────────────────────────┤
│   Search (tantivy) + DB (redb)│
├─────────────────────────────┤
│   P2P Networking (libp2p)    │
└─────────────────────────────┘
```

### 2. Content-Addressed Storage
```rust
Content → BLAKE3 → Multihash → CID
                                 ↓
                           Store in redb
                                 ↓
                      Distribute via libp2p
```

### 3. Plugin Architecture
```rust
User Plugin (WASM)
       ↓
Wasmtime Sandbox (fuel-limited)
       ↓
Host Functions (marketplace API)
       ↓
Marketplace Core
```

---

## License Summary

**All libraries use MIT or Apache-2.0** ✅

Safe for commercial use with no restrictions.

---

## Production Users

- **libp2p**: IPFS, Polkadot, Filecoin, Ethereum 2.0
- **tantivy**: Quickwit
- **moka**: crates.io
- **wasmtime**: Cloudflare Workers, Fastly Compute@Edge
- **async-graphql**: Growing adoption from Juniper migrations

---

## Alternative Considerations

### If you need...

**Simpler P2P**: Use `iroh` instead of libp2p (smaller API surface)

**Faster serialization**: Use `bincode` instead of postcard (1.5x faster, 30% larger)

**Proven database**: Use `rocksdb` instead of redb (more mature, C++ deps)

**Minimal metrics**: Use `metrics` crate instead of OpenTelemetry (lower overhead)

**Embedded WASM**: Use `wasm3` instead of wasmtime (10KB RAM vs moderate)

---

## Next Steps

1. **Review** full research document: `library-research.md`
2. **Prototype** Phase 1 libraries (redb, postcard, ed25519-dalek)
3. **Benchmark** on target hardware
4. **Integrate** Phase 2 libraries (moka, tantivy, metrics)
5. **Scale** with Phase 3-4 (GraphQL, P2P, observability)

---

**Document Version:** 1.0
**Last Updated:** October 2025
**Full Research:** See `library-research.md`
