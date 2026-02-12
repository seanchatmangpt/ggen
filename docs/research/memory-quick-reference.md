<!-- START doctoc generated TOC please keep comment here to allow auto update -->
<!-- DON'T EDIT THIS SECTION, INSTEAD RE-RUN doctoc TO UPDATE -->
**Table of Contents**

- [AI Memory Management - Quick Reference (2026)](#ai-memory-management---quick-reference-2026)
  - [TL;DR - Top Recommendations for ggen](#tldr---top-recommendations-for-ggen)
    - [1. Directory Structure (Adopt Now)](#1-directory-structure-adopt-now)
    - [2. Technology Stack Recommendations](#2-technology-stack-recommendations)
    - [3. Key Metrics (2026 Industry Standards)](#3-key-metrics-2026-industry-standards)
    - [4. Architecture Patterns](#4-architecture-patterns)
      - [Three-Tier Memory](#three-tier-memory)
      - [Hybrid Search (2026 Standard)](#hybrid-search-2026-standard)
      - [Memory Types](#memory-types)
    - [5. Security Checklist](#5-security-checklist)
    - [6. Implementation Priority](#6-implementation-priority)
    - [7. Critical Insights](#7-critical-insights)
    - [8. Common Pitfalls to Avoid](#8-common-pitfalls-to-avoid)
    - [9. Quick Wins for ggen](#9-quick-wins-for-ggen)
    - [10. Decision Matrix](#10-decision-matrix)
    - [11. ggen-Specific Considerations](#11-ggen-specific-considerations)
  - [Resources](#resources)

<!-- END doctoc generated TOC please keep comment here to allow auto update -->

# AI Memory Management - Quick Reference (2026)

**Last Updated:** 2026-02-08

## TL;DR - Top Recommendations for ggen

### 1. Directory Structure (Adopt Now)
```
ggen/
├── CLAUDE.md                    # Project memory (Git-tracked)
├── .claude/
│   ├── rules/                   # Domain-specific rules
│   │   ├── rust/
│   │   ├── testing/
│   │   ├── performance/
│   │   └── security/
│   └── memory/                  # Session/long-term memory
│       ├── MEMORY.md            # Entrypoint
│       ├── architecture.md      # Design decisions
│       ├── patterns.md          # Code patterns
│       └── performance.md       # SLO insights
```

### 2. Technology Stack Recommendations

| Use Case | Technology | Why |
|----------|-----------|-----|
| **Semantic search** | SQLite-vec | Rust-friendly, on-device, privacy-preserving |
| **Vector DB (if scaling)** | Qdrant | Rust-native, 26% accuracy boost |
| **Memory layer** | Mem0-inspired custom | 91% lower latency, 90% token savings |
| **Compression** | Hierarchical summarization | 3-4× reduction, KVzip approach |
| **Security** | Post-quantum crypto | EU deadline: end 2026 |

### 3. Key Metrics (2026 Industry Standards)

- **Memory Compression**: 3-4× with KVzip
- **Performance**: 26% accuracy boost, 91% lower p95 latency (Mem0)
- **Context Support**: Up to 170,000 tokens
- **Retention**: Semantic (90d), Preference (365d), Session (30d)
- **Vector Dimensions**: 1536d-3072d (OpenAI latest models)

### 4. Architecture Patterns

#### Three-Tier Memory
```
Short-Term (in-memory) → Session (temp files) → Long-Term (DB)
```

#### Hybrid Search (2026 Standard)
```
Vector Search + BM25 Keyword + Relevance Scorer = Context
```

#### Memory Types
- **Semantic**: Factual knowledge (APIs, patterns)
- **Preference**: User preferences (style, naming)
- **Summarization**: Compressed historical context

### 5. Security Checklist

- [ ] Encrypt `.claude/memory/` if sensitive
- [ ] Add `.claude/memory/.gitignore` for private files
- [ ] Implement retention policies (auto-delete >90d)
- [ ] Post-quantum crypto migration (by Q4 2026)
- [ ] Session isolation to prevent cross-contamination
- [ ] Memory-free mode for sensitive operations

### 6. Implementation Priority

**Week 1:**
1. Create `.claude/rules/` and `.claude/memory/` structure
2. Initialize MEMORY.md with session tracking
3. Document architectural patterns

**Month 1:**
4. Add SQLite-vec to ggen-core
5. Implement semantic template search
6. Add hierarchical compression for sessions

**Quarter 1:**
7. Evaluate Qdrant (if marketplace scales to 10M+ templates)
8. Custom Mem0-style memory layer for ggen-ai
9. Cross-session continuity for interrupted workflows

### 7. Critical Insights

**"2026 = Year of Context"**
- Shift from RAG (summaries) → Context Engines (direct data access)
- MCP (Model Context Protocol) replacing traditional RAG
- Universal memory extensions across AI platforms

**"Smarter Memory > Bigger Models"**
- Industry investing in context curation over parameter scaling
- Memory evolution more impactful than model size (2026-2030)

**"Local-First Revival"**
- Privacy concerns driving on-device inference
- SQLite-AI for offline, privacy-preserving AI
- No cloud upload with MCP local servers

### 8. Common Pitfalls to Avoid

❌ **Don't**: Use dedicated vector DB for prototypes (overkill)
✅ **Do**: Start with FAISS or NumPy + scikit-learn

❌ **Don't**: Store raw conversation history indefinitely
✅ **Do**: Implement hierarchical compression and retention policies

❌ **Don't**: Assume memory is always available (it's asynchronous)
✅ **Do**: Use short-term memory for immediate needs

❌ **Don't**: Mix session data across agents (Echoleak vulnerability)
✅ **Do**: Isolate memory per session/agent

❌ **Don't**: Ignore post-quantum threat (harvest now, decrypt later)
✅ **Do**: Plan PQC migration for Q4 2026

### 9. Quick Wins for ggen

1. **Template Discovery**: SQLite-vec semantic search across 30 crates
2. **Pattern Learning**: Track successful patterns in `.claude/memory/patterns.md`
3. **SLO Monitoring**: Store performance insights in `.claude/memory/performance.md`
4. **Resumable Workflows**: Save μ₁-μ₅ pipeline state for interrupted `ggen sync`
5. **Smart Recommendations**: Learn from past generations, improve suggestions

### 10. Decision Matrix

**When to use SQLite-vec:**
- Local-first applications
- Privacy-sensitive data
- < 1M vectors
- Offline capability required

**When to use Qdrant:**
- Production RAG at scale
- > 10M vectors
- Hybrid search needed
- Real-time updates

**When to use FAISS:**
- Prototypes/research
- In-memory only
- Maximum control needed
- Temporary/experimental

**When to use Mem0:**
- Multi-agent coordination
- Persistent memory across sessions
- Automatic extraction and summarization
- AWS/GCP integration

### 11. ggen-Specific Considerations

**Determinism:**
- Memory introduces non-determinism
- Version memory snapshots with hashes
- Cryptographic receipts per memory state
- Option to disable memory for reproducible builds

**RDF Ontology:**
- Memory can learn ontology patterns
- SPARQL query optimization via learned patterns
- Template recommendations based on ontology structure

**Chicago TDD:**
- Test memory operations (store, retrieve, compress)
- Verify retention policies work
- Check encryption/decryption
- Validate cross-session continuity

**DfLSS (Prevent Waste):**
- Don't store redundant information
- Compress early and often
- Auto-cleanup stale memories
- Efficient retrieval (no scanning)

---

## Resources

- **Full Research Document**: `/home/user/ggen/docs/research/ai-memory-management-2026.md`
- **Claude Code Docs**: https://code.claude.com/docs/en/memory
- **Mem0**: https://github.com/mem0ai/mem0
- **Qdrant**: https://qdrant.tech/
- **SQLite-vec**: https://github.com/asg017/sqlite-vec

---

**Quick Action:** Create `.claude/memory/MEMORY.md` now with this template:

```markdown
# ggen Session Memory

## Current Session
- **Date**: 2026-02-08
- **Focus**: AI memory management research
- **Pipeline Stage**: μ₁ (Normalize) - research phase

## Patterns Learned
- [Document patterns as you discover them]

## Performance Insights
- [Track SLO metrics, build times, test durations]

## Architectural Decisions
- [Record key decisions and rationale]

## TODO
- [ ] Implement SQLite-vec for template search
- [ ] Add compression logic for old sessions
- [ ] Define retention policies
```

---

**Remember:** Start lightweight (SQLite-vec), scale when proven need (Qdrant), and always prioritize security (PQC by Q4 2026).
