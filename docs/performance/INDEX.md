# Hive Mind Swarm Performance Benchmarking Suite

## Overview

Comprehensive performance benchmarking system for the ggen Hive Mind swarm coordination platform, focusing on the critical 20% of operations that represent 80% of system usage.

## Benchmark Suites

### 1. Hive Coordination (`ggen-core`)
**Location**: `crates/ggen-core/benches/hive_coordination.rs`

**Focus Areas**:
- ✅ Consensus mechanism latency (HiveQueen orchestration)
- ✅ Agent spawning and initialization overhead
- ✅ Conflict detection performance (O(n²) pairwise comparison)
- ✅ Configuration analysis phase
- ✅ Parallel agent execution
- ✅ Memory allocation patterns
- ✅ End-to-end orchestration throughput

**Key Metrics**:
- Consensus latency (p50, p95, p99) for 1-20 packs
- Agent spawning time for 4-32 agents
- Conflict detection for 5-50 packs
- Parallel execution scalability (2-16 concurrent agents)

### 2. Swarm Coordination (`ggen-ai`)
**Location**: `crates/ggen-ai/benches/swarm_coordination.rs`

**Focus Areas**:
- ✅ Pipeline execution latency
- ✅ Stage dependency resolution
- ✅ Concurrent swarm execution
- ✅ Semaphore coordination overhead
- ✅ Timeout handling performance
- ✅ Artifact conversion and serialization
- ✅ Memory allocation for large swarms

**Key Metrics**:
- Pipeline execution for 3-15 stages
- Dependency resolution for 2-8 depth
- Concurrent execution scalability (2-20 swarms)
- Timeout overhead (10-500ms)

### 3. Swarm Primitives (`ggen-domain`)
**Location**: `crates/ggen-domain/benches/swarm_primitives.rs`

**Focus Areas**:
- ✅ Lock-free snapshot reads (critical path)
- ✅ Snapshot staging and commit
- ✅ Commutative proposal aggregation
- ✅ Conditional proposal with conflict detection
- ✅ Priority scheduling (enqueue/dequeue)
- ✅ Queue depth calculation
- ✅ Pairwise conflict detection
- ✅ Sequential proposal merging
- ✅ Scheduling hint sort key calculation

**Key Metrics**:
- Snapshot reads (10-1000 concurrent readers)
- Lock-free performance (< 1μs target)
- Proposal aggregation (10-500 proposals)
- Priority scheduling (10-1000 tasks)

## Quick Start

### Run All Benchmarks
```bash
# From repository root
cargo bench --workspace

# View results
open target/criterion/report/index.html
```

### Run Specific Suite
```bash
# Hive coordination
cargo bench --bench hive_coordination

# Swarm coordination
cargo bench --bench swarm_coordination

# Low-level primitives
cargo bench --bench swarm_primitives
```

### Run Specific Benchmark
```bash
# Only consensus latency
cargo bench --bench hive_coordination -- consensus_latency

# Only snapshot reads
cargo bench --bench swarm_primitives -- snapshot_reads
```

## Performance Targets (80/20 Critical Path)

| Metric | Target | Priority | Benchmark |
|--------|--------|----------|-----------|
| **Consensus p50** | < 100ms | 🔥 CRITICAL | `consensus_latency/10` |
| **Consensus p99** | < 500ms | 🔥 CRITICAL | `consensus_latency/10` |
| **Agent Spawning** | < 50ms | 🔥 CRITICAL | `agent_spawning/8` |
| **Snapshot Reads** | < 1μs | 🔥 CRITICAL | `snapshot_reads/100` |
| **Throughput** | > 100 ops/sec | 🔥 CRITICAL | `orchestration_throughput/50` |
| Pipeline p50 | < 200ms | 🟡 HIGH | `pipeline_execution/6` |
| Conflict Detection | < 200ms | 🟡 HIGH | `conflict_detection/20` |
| Memory/100 agents | < 10MB | 🟡 MEDIUM | `swarm_memory/100` |

## Documentation

- **[BENCHMARK_RESULTS.md](./BENCHMARK_RESULTS.md)** - Detailed benchmark results and analysis
- **[OPTIMIZATION_RECOMMENDATIONS.md](./OPTIMIZATION_RECOMMENDATIONS.md)** - Priority-ordered optimization guide
- **[QUICK_REFERENCE.md](./QUICK_REFERENCE.md)** - Command reference and troubleshooting

## Benchmark Implementation Details

### Technologies
- **Criterion.rs** - Statistical benchmarking framework
- **Tokio** - Async runtime for concurrent benchmarks
- **Rayon** - Data parallelism for optimization testing

### Coverage
- ✅ **24 benchmark scenarios** across 3 crates
- ✅ **Consensus mechanisms** (7 benchmarks)
- ✅ **Coordination primitives** (7 benchmarks)
- ✅ **Lock-free operations** (9 benchmarks)
- ✅ **Memory and resource profiling**
- ✅ **Scalability testing** (1-1000 concurrent operations)

## Results Storage

Benchmark results are stored in:
- `target/criterion/` - Criterion HTML reports
- `.swarm/memory.db` - Coordination memory
- Memory keys:
  - `hive/benchmarker/results`
  - `hive/benchmarker/optimizations`

---

**Status**: ✅ Benchmark suite complete and ready to run
**Coverage**: 24 benchmarks across 3 crates
**Focus**: 80/20 critical path operations
**Next Steps**: Run baseline benchmarks and identify top 3 bottlenecks
