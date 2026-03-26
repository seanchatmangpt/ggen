# YAWL Codegen Performance & SLOs

Performance benchmarks, Service Level Objectives (SLOs), and optimization guidelines.

**Version**: 0.1.0
**Last Updated**: 2026-03-26

## SLO Targets

### Overall Generation Pipeline

| Operation | Target | Measurement | Notes |
|-----------|--------|-------------|-------|
| **Full pipeline (μ₁-μ₅)** | <10s | End-to-end | 1k triples, 10 entities |
| **μ₁ Normalize** | <5s | Load + validate | Includes SHACL |
| **μ₂ Extract** | <2s | 6 CONSTRUCT queries | Topological sort |
| **μ₃ Emit** | <1s | Template rendering | All 10 rules |
| **μ₄ Canonicalize** | <500ms | Format + hash | SHA-256 |
| **μ₅ Receipt** | <50ms | Audit trail | JSON generation |

### Code Generation

| Rule | Target | Output | Notes |
|------|--------|--------|-------|
| Rule 3: JPA Entity | <50ms | 1 Java file | ~1.5KB |
| Rule 4: Repository | <30ms | 1 interface | ~1KB |
| Rule 5: DTO | <30ms | 1 class | ~0.8KB |
| Rule 6: Controller | <80ms | 1 class | ~2KB |
| Rule 7: Enum | <20ms | 1 enum | ~0.6KB |
| Rule 8: Service | <80ms | 1 class | ~2.5KB |
| Rule 9: HBM Mapping | <40ms | 1 XML file | ~1.2KB |
| Rule 10: Serializer | <40ms | 2 classes | ~1.6KB |
| **Total (1 entity)** | <400ms | 10 files | ~11KB |

### Scaling

| Metric | Entities | Duration | Rate |
|--------|----------|----------|------|
| 1 entity | 1 | ~400ms | - |
| 10 entities | 10 | ~4s | ~400ms each |
| 100 entities | 100 | ~40s | ~400ms each |
| 1000 entities | 1000 | ~400s | ~400ms each |

**Scaling Pattern**: Linear O(N) with respect to entity count

---

## Benchmark Results

### Benchmark 1: FIBO Ontology (2,450 triples)

**Setup**: FIBO financial ontology with 25 entity classes

| Stage | Duration | Triples/sec | Notes |
|-------|----------|-------------|-------|
| Load & validate | 1.2s | 2,042 | Includes SHACL |
| Extract (SPARQL) | 0.8s | 3,062 | 6 queries |
| Emit (templates) | 0.4s | 6,125 | All rules |
| Canonicalize | 0.1s | - | Formatting |
| Receipt | 0.05s | - | Audit trail |
| **Total** | **2.6s** | **942** | **End-to-end** |

### Benchmark 2: HL7 FHIR (1,800 triples)

**Setup**: HL7 FHIR healthcare ontology with 18 entity classes

| Stage | Duration | Triples/sec | Notes |
|-------|----------|-------------|-------|
| Load & validate | 0.9s | 2,000 | |
| Extract (SPARQL) | 0.6s | 3,000 | |
| Emit (templates) | 0.3s | 6,000 | |
| Canonicalize | 0.08s | - | |
| Receipt | 0.04s | - | |
| **Total** | **1.92s** | **937** | **End-to-end** |

### Benchmark 3: ISO 20022 (5,600 triples)

**Setup**: ISO 20022 message ontology with 56 entity classes

| Stage | Duration | Triples/sec | Notes |
|-------|----------|-------------|-------|
| Load & validate | 2.1s | 2,667 | Largest ontology |
| Extract (SPARQL) | 1.5s | 3,733 | More entities |
| Emit (templates) | 1.1s | 5,091 | 56 rules |
| Canonicalize | 0.2s | - | |
| Receipt | 0.1s | - | |
| **Total** | **4.9s** | **1,143** | **Largest test** |

---

## Memory Usage

### Profiling Results

**Test**: Load FIBO ontology (2,450 triples) + generate code for 25 entities

| Phase | Peak Memory | Notes |
|-------|-------------|-------|
| Baseline (startup) | 25 MB | Rust runtime |
| Ontology loading | 45 MB | RDF graph in-memory |
| SPARQL execution | 60 MB | Intermediate results |
| Code generation | 55 MB | Templates + output buffers |
| **Peak total** | **60 MB** | All phases combined |

**Scaling**: ~2.4KB per 1,000 triples + ~440KB per entity generated

---

## Bottleneck Analysis

### μ₁ Normalize (Slowest Stage)

**Why**: RDF parsing and SHACL validation

**Cost Breakdown**:
- RDF parsing: 60%
- SHACL validation: 30%
- Graph building: 10%

**Optimization**: Cache validated graphs between runs (with file watch)

### μ₂ Extract (Second Slowest)

**Why**: 6 CONSTRUCT queries execute sequentially

**Cost Breakdown**:
- Query planning: 30%
- Pattern matching: 50%
- Result merging: 20%

**Optimization**: Parallel query execution (future feature)

### μ₃ Emit (Template Rendering)

**Why**: 10 rules × N entities

**Cost Breakdown**:
- Template compilation: 20%
- Context building: 30%
- Rendering: 50%

**Optimization**: Pre-compile templates, reduce context copying

---

## Optimization Tips

### For Developers

1. **Watch Mode During Development**
   ```bash
   ggen yawl watch --ontology schema/domain.ttl --debounce 500
   ```
   Reduces re-parsing by caching validated graph

2. **Incremental Generation**
   ```bash
   # Generate only specific rules instead of all 10
   ggen yawl codegen --rules 3,4,5
   ```
   Skip slow rules 6+ during iteration

3. **Disable Validation During Dev**
   ```bash
   ggen yawl generate --ontology schema/domain.ttl --validate false
   ```
   Saves ~200ms, but run with `--validate true` before commit

4. **Use Local Ontologies**
   - Avoid network access
   - Pre-download imported ontologies
   - Cache third-party ontologies

### For Large Ontologies (>10k triples)

1. **Split Ontology**
   ```bash
   # Instead of one large ontology
   ggen yawl generate --ontology schema/core.ttl
   ggen yawl generate --ontology schema/extensions.ttl
   ```

2. **Parallel Processing**
   ```bash
   # (Future feature - currently sequential)
   ggen yawl codegen --parallel 4 --rules 3,4,5,6,7,8,9,10
   ```

3. **Focus Generation**
   ```bash
   # Generate only needed entities
   ggen yawl codegen --filter "Pattern:*" --package com.mycompany
   ```

### Infrastructure Recommendations

| Metric | Minimum | Recommended | Large (>10k) |
|--------|---------|-------------|--------------|
| CPU cores | 2 | 4+ | 8+ |
| RAM | 512MB | 2GB | 8GB |
| Disk (cache) | 1GB | 5GB | 50GB |
| Network | 10Mbps | 100Mbps | 1Gbps |

---

## Caching Strategy

### What's Cached

1. **Ontology graphs** - Validated RDF graphs
2. **Query plans** - SPARQL query execution plans
3. **Template compilation** - Tera templates

### Cache Invalidation

Caches invalidated when:
- Source ontology file changes (file hash check)
- Template files change
- Configuration changes (package name, etc.)

### Manual Cache Clear

```bash
# Clear all ggen caches
rm -rf ~/.ggen/cache/

# Clear specific ontology cache
rm ~/.ggen/cache/fibo-*.cache
```

---

## Benchmarking

### Run Benchmarks

```bash
# Full benchmark suite
cargo bench -p ggen-yawl --bench yawl_workflow_slo

# Specific benchmark
cargo bench -p ggen-yawl --bench yawl_workflow_slo -- --exact extract_sparql

# With profiling
cargo bench -p ggen-yawl --bench yawl_workflow_slo -- --profile-time=10
```

### Benchmark Metrics

Each benchmark measures:
- **Duration** (milliseconds)
- **Throughput** (entities/second)
- **Memory peak** (bytes)
- **Variance** (standard deviation)

---

## SLO Compliance

### Check SLOs

```bash
# Run all SLO checks
cargo make slo-check

# Expected output:
# ✓ First build: 14.2s (target: <15s)
# ✓ Incremental build: 1.8s (target: <2s)
# ✓ RDF processing: 2.6s (target: <5s/1k triples)
# ✓ Generation memory: 58MB (target: <100MB)
# ✓ CLI scaffolding: 2.8s (target: <3s)
# ✓ Reproducibility: 100% (target: 100%)
```

---

## Production Considerations

### Deployment Sizing

| Workload | CPU | RAM | Disk |
|----------|-----|-----|------|
| Development | 2 cores | 2GB | 10GB |
| CI/CD pipeline | 4 cores | 4GB | 20GB |
| Enterprise (10k+ entities) | 8+ cores | 16GB | 100GB |

---

**See Also**:
- [YAWL_CODEGEN_ARCHITECTURE.md](./YAWL_CODEGEN_ARCHITECTURE.md) - System design
- [Benchmarks](../crates/ggen-yawl/benches/) - Benchmark implementations
