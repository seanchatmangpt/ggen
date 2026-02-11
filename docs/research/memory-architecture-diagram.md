<!-- START doctoc generated TOC please keep comment here to allow auto update -->
<!-- DON'T EDIT THIS SECTION, INSTEAD RE-RUN doctoc TO UPDATE -->
**Table of Contents**

- [ggen AI Memory Architecture (2026)](#ggen-ai-memory-architecture-2026)
  - [System Architecture Overview](#system-architecture-overview)
  - [4-Level Memory Hierarchy (Priority Order)](#4-level-memory-hierarchy-priority-order)
  - [Three-Tier Memory Lifespan](#three-tier-memory-lifespan)
  - [Memory Compression Pipeline (Hierarchical)](#memory-compression-pipeline-hierarchical)
  - [Hybrid Search Architecture](#hybrid-search-architecture)
  - [SQLite-vec Integration](#sqlite-vec-integration)
  - [Memory Manager API (Unified)](#memory-manager-api-unified)
  - [μ₁-μ₅ Pipeline Integration](#%CE%BC%E2%82%81-%CE%BC%E2%82%85-pipeline-integration)
  - [Security Architecture (Post-Quantum)](#security-architecture-post-quantum)
  - [File System Layout](#file-system-layout)
  - [Data Flow Diagram](#data-flow-diagram)
  - [Performance Metrics (2026 Targets)](#performance-metrics-2026-targets)
  - [Comparison: Before vs After Memory System](#comparison-before-vs-after-memory-system)
  - [Decision Tree: When to Use What](#decision-tree-when-to-use-what)
  - [Implementation Timeline](#implementation-timeline)
  - [Key Takeaways](#key-takeaways)

<!-- END doctoc generated TOC please keep comment here to allow auto update -->

# ggen AI Memory Architecture (2026)

**Visual reference for implementing memory system in ggen v6.0.0**

---

## System Architecture Overview

```
┌─────────────────────────────────────────────────────────────────────────┐
│                        ggen AI Memory System                            │
│                          (Based on 2026 Research)                       │
└─────────────────────────────────────────────────────────────────────────┘
                                    │
                    ┌───────────────┴───────────────┐
                    ▼                               ▼
        ┌───────────────────────┐       ┌───────────────────────┐
        │   File-Based Memory   │       │   Database Memory     │
        │   (.claude/memory/)   │       │   (SQLite + vec)      │
        └───────────────────────┘       └───────────────────────┘
                    │                               │
        ┌───────────┴────────────┐     ┌───────────┴───────────┐
        ▼           ▼            ▼     ▼           ▼           ▼
    MEMORY.md  patterns.md  arch.md  Vectors  Sessions  Patterns
    (Entry)    (Learned)    (ADRs)   (384d)   (Compressed)(Indexed)
```

---

## 4-Level Memory Hierarchy (Priority Order)

```
┌────────────────────────────────────────────────────────┐
│  Level 1: Enterprise Policy                           │  ← Highest Priority
│  (Organizational standards, compliance)                │
└────────────────────────────────────────────────────────┘
                         ↓
┌────────────────────────────────────────────────────────┐
│  Level 2: Project Memory (CLAUDE.md)                  │
│  - Git-tracked, shared with team                      │
│  - Project-specific instructions                      │
│  - Stack: Rust, Tokio, Oxigraph, etc.                 │
└────────────────────────────────────────────────────────┘
                         ↓
┌────────────────────────────────────────────────────────┐
│  Level 3: Project Rules (.claude/rules/)              │
│  ├── rust/           (Rust-specific patterns)         │
│  ├── testing/        (Chicago TDD rules)              │
│  ├── performance/    (SLO enforcement)                │
│  └── security/       (PQC, encryption)                │
└────────────────────────────────────────────────────────┘
                         ↓
┌────────────────────────────────────────────────────────┐
│  Level 4: User Memory (~/.claude/CLAUDE.md)           │  ← Lowest Priority
│  - Global preferences across all projects             │
│  - Personal coding style, preferred patterns          │
└────────────────────────────────────────────────────────┘
```

---

## Three-Tier Memory Lifespan

```
┌────────────────────────────────────────────────────────────────────┐
│                        Memory Tiers                                │
├────────────────────────────────────────────────────────────────────┤
│                                                                    │
│  ┌─────────────────────────────────────────────────────┐          │
│  │  SHORT-TERM (In-Memory Context)                     │          │
│  │  • Lifetime: Current conversation only              │          │
│  │  • Storage: In-memory (context window)              │          │
│  │  • Size: Up to 170,000 tokens (2026)                │          │
│  │  • Use: Immediate context, current task             │          │
│  └─────────────────────────────────────────────────────┘          │
│                           ↓ (Session end)                          │
│  ┌─────────────────────────────────────────────────────┐          │
│  │  SESSION MEMORY (Temp Files)                        │          │
│  │  • Lifetime: Workflow duration (hours/days)         │          │
│  │  • Storage: .claude/memory/*.md                     │          │
│  │  • Size: Compressed summaries (~1-10MB)             │          │
│  │  • Use: Cross-conversation continuity               │          │
│  └─────────────────────────────────────────────────────┘          │
│                           ↓ (After compression)                    │
│  ┌─────────────────────────────────────────────────────┐          │
│  │  LONG-TERM MEMORY (Database)                        │          │
│  │  • Lifetime: Weeks/months (retention policy)        │          │
│  │  • Storage: SQLite + SQLite-vec                     │          │
│  │  • Size: Vector embeddings (~1.5GB/1M vectors)      │          │
│  │  • Use: Semantic search, pattern learning           │          │
│  └─────────────────────────────────────────────────────┘          │
│                                                                    │
└────────────────────────────────────────────────────────────────────┘
```

---

## Memory Compression Pipeline (Hierarchical)

```
Recent Sessions (0-7 days)
    │
    │ [Keep Verbatim]
    ▼
┌────────────────────────────────┐
│  "ggen sync completed         │
│   Generated 15 files          │
│   Tests: 347/347 passed       │
│   Build time: 12.3s"          │
└────────────────────────────────┘
                │
                │ [Age > 7 days]
                ▼
Old Sessions (7-30 days)
    │
    │ [Compress to Summary]
    ▼
┌────────────────────────────────┐
│  "Summary: 12 operations      │
│   - 8× ggen sync (success)    │
│   - 3× template generate      │
│   - 1× marketplace search"    │
└────────────────────────────────┘
                │
                │ [Age > 30 days]
                ▼
Archive Sessions (30-90 days)
    │
    │ [Compress to Key Insights]
    ▼
┌────────────────────────────────┐
│  "Archive: 45 entries         │
│   Agents: coder, tester       │
│   Peak performance: 8.2s"     │
└────────────────────────────────┘
                │
                │ [Age > 90 days]
                ▼
           [Delete]
```

**Compression Ratio**: 3-4× (KVzip-inspired)

---

## Hybrid Search Architecture

```
User Query: "Find CLI template with Clap integration"
    │
    ├────────────────────┬────────────────────┬─────────────────┐
    ▼                    ▼                    ▼                 ▼
┌─────────┐      ┌──────────┐      ┌──────────┐      ┌────────────┐
│ Vector  │      │ Keyword  │      │ Recency  │      │  Context   │
│ Search  │      │  (BM25)  │      │  Score   │      │  Filter    │
│         │      │          │      │          │      │            │
│ Semantic│      │ "CLI"    │      │ Recent   │      │ crates/    │
│ Embeddin│      │ "Clap"   │      │ updates  │      │ ggen-cli   │
└─────────┘      └──────────┘      └──────────┘      └────────────┘
    │                 │                 │                  │
    └─────────────────┴─────────────────┴──────────────────┘
                          │
                          ▼
                  ┌───────────────┐
                  │   Relevance   │
                  │    Scorer     │
                  │               │
                  │ Weighted sum: │
                  │ 0.5×vector +  │
                  │ 0.3×keyword + │
                  │ 0.2×recency   │
                  └───────────────┘
                          │
                          ▼
                   Ranked Results
```

---

## SQLite-vec Integration

```
┌─────────────────────────────────────────────────────────────┐
│                     SQLite Database                         │
├─────────────────────────────────────────────────────────────┤
│                                                             │
│  Regular Tables:                                            │
│  ┌─────────────────────────────────────────┐               │
│  │ sessions (id, timestamp, content, meta) │               │
│  │ patterns (id, stage, pattern, usage)    │               │
│  │ configurations (key, value, updated_at) │               │
│  └─────────────────────────────────────────┘               │
│                                                             │
│  Vector Tables (SQLite-vec extension):                      │
│  ┌─────────────────────────────────────────┐               │
│  │ CREATE VIRTUAL TABLE embeddings         │               │
│  │ USING vec0(                             │               │
│  │   id TEXT PRIMARY KEY,                  │               │
│  │   vector FLOAT[384],                    │               │
│  │   metadata TEXT                         │               │
│  │ );                                      │               │
│  └─────────────────────────────────────────┘               │
│                                                             │
│  Queries:                                                   │
│  • vec_distance_cosine(v1, v2) - Similarity                │
│  • ORDER BY distance - Nearest neighbors                   │
│  • Indexed for O(log n) search                             │
│                                                             │
└─────────────────────────────────────────────────────────────┘
```

**Memory Footprint**: 1M vectors @ 384d = ~1.5GB RAM

---

## Memory Manager API (Unified)

```rust
┌────────────────────────────────────────────────────────┐
│              MemoryManager (Public API)                │
├────────────────────────────────────────────────────────┤
│                                                        │
│  pub fn new(config: MemoryConfig)                     │
│  pub fn store_embedding(emb: Embedding)               │
│  pub fn search(vector: &[f32], limit: usize)          │
│  pub fn compress(entries: Vec<Entry>)                 │
│  pub fn encrypt(data: &[u8])                          │
│                                                        │
└────────────────────────────────────────────────────────┘
                         │
         ┌───────────────┼───────────────┐
         ▼               ▼               ▼
┌──────────────┐  ┌──────────┐  ┌───────────────┐
│SemanticSearch│  │Compressor│  │   Encryptor   │
│Engine        │  │          │  │   (ML-DSA)    │
│(SQLite-vec)  │  │(Hierarch)│  │   (PQC)       │
└──────────────┘  └──────────┘  └───────────────┘
```

---

## μ₁-μ₅ Pipeline Integration

```
ggen Holographic Factory Pipeline
                                                    Memory Layer
┌──────────────────────────────────────────────┐  Integration
│                                              │
│  μ₁: Normalize (Validate RDF)               │ ──→ Learn validation patterns
│      ↓                                       │     Store ontology structure
│  μ₂: Extract (SPARQL queries)               │ ──→ Optimize common queries
│      ↓                                       │     Cache query results
│  μ₃: Emit (Template rendering)              │ ──→ Remember successful templates
│      ↓                                       │     Suggest similar patterns
│  μ₄: Canonicalize (Format, hash)            │ ──→ Track formatting rules
│      ↓                                       │     Deterministic state
│  μ₅: Receipt (Cryptographic proof)          │ ──→ Audit trail persistence
│                                              │     Memory snapshot hash
└──────────────────────────────────────────────┘
        │
        └──→ Cross-Session Continuity:
             Resume interrupted workflows
             Load last μ-stage state
             Recover from failures
```

---

## Security Architecture (Post-Quantum)

```
┌──────────────────────────────────────────────────────────┐
│                  Security Layers                         │
├──────────────────────────────────────────────────────────┤
│                                                          │
│  Layer 1: Data at Rest                                  │
│  ┌────────────────────────────────────┐                 │
│  │  Encryption: ML-DSA (Post-Quantum) │                 │
│  │  • .claude/memory/*.md encrypted   │                 │
│  │  • SQLite WAL encrypted            │                 │
│  │  • Key derivation: Argon2          │                 │
│  └────────────────────────────────────┘                 │
│                                                          │
│  Layer 2: Access Control                                │
│  ┌────────────────────────────────────┐                 │
│  │  • Session isolation (prevent mix) │                 │
│  │  • Agent-specific memory spaces    │                 │
│  │  • Retention policy enforcement    │                 │
│  └────────────────────────────────────┘                 │
│                                                          │
│  Layer 3: Privacy Controls                              │
│  ┌────────────────────────────────────┐                 │
│  │  • Memory-free mode (sensitive ops)│                 │
│  │  • Auto-cleanup (>90 days)         │                 │
│  │  • User consent for persistence    │                 │
│  │  • GDPR-compliant deletion         │                 │
│  └────────────────────────────────────┘                 │
│                                                          │
└──────────────────────────────────────────────────────────┘

EU PQC Roadmap: Migration deadline Q4 2026 ✓
```

---

## File System Layout

```
ggen/
├── CLAUDE.md                           # Project memory (Level 2)
├── .claude/
│   ├── rules/                          # Level 3
│   │   ├── rust/
│   │   │   ├── ownership.md           # Ownership patterns
│   │   │   ├── async.md               # Tokio patterns
│   │   │   └── testing.md             # Chicago TDD
│   │   ├── testing/
│   │   │   ├── unit.md                # Unit test rules
│   │   │   └── integration.md         # Integration tests
│   │   ├── performance/
│   │   │   ├── slos.md                # SLO definitions
│   │   │   └── benchmarks.md          # Benchmark rules
│   │   └── security/
│   │       ├── encryption.md          # PQC requirements
│   │       └── auditing.md            # Audit trail
│   │
│   └── memory/                         # Session/Long-term
│       ├── MEMORY.md                  # Entrypoint
│       ├── POLICY.md                  # Retention policies
│       ├── sessions/
│       │   ├── 2026-02-08.md         # Today's session
│       │   ├── 2026-02-07.md         # Yesterday
│       │   └── summary-2026-02.md    # Monthly summary
│       ├── patterns/
│       │   ├── μ1-normalize.md       # μ₁ patterns
│       │   ├── μ2-extract.md         # μ₂ patterns
│       │   └── templates.md          # Template patterns
│       ├── architecture/
│       │   ├── decisions.md          # ADRs
│       │   └── diagrams.md           # Architecture docs
│       ├── performance/
│       │   ├── slos.md               # SLO tracking
│       │   └── benchmarks.md         # Benchmark results
│       └── memory.db                  # SQLite database
│           ├── Regular tables
│           └── Vector tables (vec0)
│
├── crates/
│   └── ggen-core/
│       └── src/
│           └── memory/
│               ├── mod.rs                    # Public API
│               ├── semantic_search.rs        # SQLite-vec
│               ├── compression.rs            # Hierarchical
│               ├── encryption.rs             # ML-DSA
│               └── pipeline_integration.rs   # μ₁-μ₅
│
└── ~/.claude/
    └── CLAUDE.md                      # User memory (Level 4)
```

---

## Data Flow Diagram

```
┌─────────────────────────────────────────────────────────────┐
│                      User Interaction                       │
└─────────────────────────────────────────────────────────────┘
                            │
                            ▼
                    ┌───────────────┐
                    │  ggen Command │
                    │  (CLI/API)    │
                    └───────────────┘
                            │
        ┌───────────────────┼───────────────────┐
        ▼                   ▼                   ▼
┌──────────────┐    ┌──────────────┐    ┌──────────────┐
│ Load Context │    │ Execute Task │    │ Store Result │
│ (4-level)    │    │ (μ₁-μ₅)      │    │ (Memory)     │
└──────────────┘    └──────────────┘    └──────────────┘
        │                   │                   │
        ▼                   ▼                   ▼
┌──────────────┐    ┌──────────────┐    ┌──────────────┐
│ Enterprise   │    │ RDF Process  │    │ Session File │
│ Project      │    │ Template Gen │    │ (.md)        │
│ Rules        │    │ Code Emit    │    │              │
│ User         │    │ Test Run     │    │ Vector Emb   │
└──────────────┘    └──────────────┘    │ (.db)        │
                                        │              │
                                        │ Encrypted    │
                                        │ (ML-DSA)     │
                                        └──────────────┘
                                               │
                    ┌──────────────────────────┤
                    ▼                          ▼
            ┌──────────────┐          ┌──────────────┐
            │ Compression  │          │ Retrieval    │
            │ (3-4× ratio) │          │ (Search)     │
            └──────────────┘          └──────────────┘
                    │                          │
                    ▼                          ▼
            ┌──────────────┐          ┌──────────────┐
            │ Summary      │          │ Ranked       │
            │ Archive      │          │ Results      │
            │ Delete       │          │ (Hybrid)     │
            └──────────────┘          └──────────────┘
```

---

## Performance Metrics (2026 Targets)

```
┌──────────────────────────────────────────────────────────┐
│                    Performance SLOs                      │
├──────────────────────────────────────────────────────────┤
│                                                          │
│  Search Operations:                                      │
│  ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━  │
│  • Latency (p50):        <5ms                           │
│  • Latency (p95):        <20ms                          │
│  • Latency (p99):        <50ms                          │
│  • Throughput:           1000+ qps                      │
│                                                          │
│  Memory Operations:                                      │
│  ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━  │
│  • Compression ratio:    3-4×                           │
│  • Accuracy retention:   89-95%                         │
│  • Token savings:        90%                            │
│  • Context support:      Up to 170K tokens              │
│                                                          │
│  Storage:                                                │
│  ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━  │
│  • Vector footprint:     ~1.5GB / 1M vectors @ 384d     │
│  • Session files:        ~1-10MB compressed             │
│  • Database growth:      <100MB/month (with cleanup)    │
│                                                          │
│  Encryption:                                             │
│  ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━  │
│  • Overhead:             <10% latency increase          │
│  • Algorithm:            ML-DSA (post-quantum)          │
│  • Key size:             4864 bytes (ML-DSA-87)         │
│                                                          │
└──────────────────────────────────────────────────────────┘
```

---

## Comparison: Before vs After Memory System

```
┌──────────────────────────────────────────────────────────────┐
│                    WITHOUT Memory System                     │
├──────────────────────────────────────────────────────────────┤
│  • Each session starts from scratch                          │
│  • Manual context building every time                        │
│  • No pattern learning or optimization                       │
│  • Repeat same mistakes across sessions                      │
│  • Cannot resume interrupted workflows                       │
│  • Slow template discovery (manual search)                   │
│  • No cross-session knowledge sharing                        │
└──────────────────────────────────────────────────────────────┘
                            VS
┌──────────────────────────────────────────────────────────────┐
│                     WITH Memory System                       │
├──────────────────────────────────────────────────────────────┤
│  ✓ Context preserved across sessions                         │
│  ✓ Automatic pattern learning and application                │
│  ✓ Avoid repeating known issues                              │
│  ✓ Resume workflows from any μ-stage                         │
│  ✓ Semantic search for instant template discovery            │
│  ✓ Knowledge compounds over time                             │
│  ✓ 26% accuracy boost, 91% lower latency                     │
│  ✓ 90% token savings through compression                     │
└──────────────────────────────────────────────────────────────┘

Productivity Improvement: ~40-60% (industry average for 2026)
```

---

## Decision Tree: When to Use What

```
Need memory persistence?
    │
    ├─ No → Use in-memory context only
    │       (Current conversation)
    │
    └─ Yes → How long?
            │
            ├─ Hours/Days → Session Memory
            │               (.claude/memory/*.md)
            │               Compressed summaries
            │
            └─ Weeks/Months → Long-term Memory
                              (SQLite + SQLite-vec)
                              │
                              ├─ <1M vectors → SQLite-vec
                              │                (Embedded, local)
                              │
                              ├─ 1M-10M vectors → SQLite-vec + optimization
                              │                   (Still local, indexed)
                              │
                              └─ >10M vectors → Consider Qdrant
                                               (Managed, scalable)

Need semantic search?
    │
    ├─ Prototype → FAISS (in-memory)
    │
    ├─ Production (local) → SQLite-vec
    │
    └─ Production (scale) → Qdrant (Rust-native)

Need encryption?
    │
    ├─ Not sensitive → No encryption
    │
    ├─ Sensitive → ML-DSA (post-quantum)
    │              Required by Q4 2026 (EU PQC)
    │
    └─ Highly sensitive → ML-DSA + memory-free mode
```

---

## Implementation Timeline

```
┌─────────────────────────────────────────────────────────────┐
│                      Implementation Phases                  │
├─────────────────────────────────────────────────────────────┤
│                                                             │
│  Week 1: Foundation                                         │
│  ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━ │
│  [ ] Create .claude/rules/ structure                       │
│  [ ] Create .claude/memory/ with MEMORY.md                 │
│  [ ] Document architectural patterns                       │
│  [ ] Define retention policies (POLICY.md)                 │
│                                                             │
│  Month 1: Lightweight Integration                          │
│  ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━ │
│  [ ] Add SQLite-vec to ggen-core                           │
│  [ ] Implement semantic search module                      │
│  [ ] Add hierarchical compression                          │
│  [ ] Encrypt sensitive memory files                        │
│  [ ] Chicago TDD tests for all operations                  │
│                                                             │
│  Quarter 1: Advanced Features                              │
│  ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━ │
│  [ ] Evaluate Qdrant (if scaling to 10M+ templates)        │
│  [ ] Build Mem0-style hybrid memory for ggen-ai            │
│  [ ] μ₁-μ₅ pipeline integration for resumable workflows    │
│  [ ] Memory versioning with cryptographic receipts         │
│  [ ] Performance benchmarks (Criterion)                    │
│                                                             │
│  Q4 2026: Production Hardening                             │
│  ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━ │
│  [ ] Post-quantum crypto migration complete (EU deadline)  │
│  [ ] Production monitoring and alerting                    │
│  [ ] GDPR-compliant deletion workflows                     │
│  [ ] Documentation and team training                       │
│  [ ] SLO enforcement and performance validation            │
│                                                             │
└─────────────────────────────────────────────────────────────┘
```

---

## Key Takeaways

1. **Start Lightweight**: SQLite-vec for prototyping, scale to Qdrant when proven need
2. **4-Level Hierarchy**: Enterprise → Project → Rules → User (priority order)
3. **3-Tier Lifespan**: Short-term → Session → Long-term (with compression)
4. **Hybrid Search**: Vector + BM25 + Recency = Best results (2026 standard)
5. **Compression**: 3-4× reduction with hierarchical summarization
6. **Security**: Post-quantum crypto (ML-DSA) by Q4 2026 (EU requirement)
7. **Performance**: <20ms p95 latency, 90% token savings, 26% accuracy boost
8. **Privacy**: On-device with SQLite-vec, MCP for local data access
9. **Determinism**: Version memory snapshots, cryptographic receipts
10. **Chicago TDD**: Test all memory operations with observable outputs

---

**This architecture aligns with ggen's philosophy:**
- ✅ Type-first design
- ✅ Zero-cost abstractions (where possible)
- ✅ Chicago TDD (real dependencies, observable outputs)
- ✅ DfLSS (prevent waste from start)
- ✅ Performance-aware (SLOs, benchmarks)
- ✅ Security-first (post-quantum ready)

**Ready for implementation in ggen v6.0.0!**
