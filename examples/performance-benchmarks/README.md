# Performance Benchmarks Example

Wave 5 example validating system performance against Service Level Objectives (SLOs).

## Purpose

Measures and validates performance of key system operations:
- Agent creation and startup
- Message throughput
- MCP tool discovery
- Plan generation
- Tool execution
- Consensus operations
- Domain balance calculations

## SLO Targets

All times in milliseconds (lower is better):

| Operation | Target | Category |
|-----------|--------|----------|
| Agent creation | ≤100ms | Agent lifecycle |
| Agent startup to ready | ≤500ms | Agent lifecycle |
| Message throughput | ≥10,000 msgs/sec | Messaging |
| Tool discovery | ≤200ms | Tool integration |
| Plan generation (10 steps) | ≤1,000ms | Planning |
| Tool execution per tool | ≤100ms | Execution |
| Consensus (3 agents) | ≤2,000ms | Coordination |
| Domain balance calculation | ≤500ms | Domain mgmt |

## Running Benchmarks

### All Benchmarks
```bash
cargo bench --example performance-benchmarks
```

### Individual Benchmarks
```bash
# Agent creation benchmarks
cargo bench --bench agent_creation

# Message throughput benchmarks
cargo bench --bench message_throughput

# Tool discovery benchmarks
cargo bench --bench tool_discovery

# Plan generation benchmarks
cargo bench --bench plan_generation
```

### SLO Validation Tests
```bash
# Run all validation tests
cargo test --example performance-benchmarks

# Run only SLO validation
cargo test --test slo_validation

# Run benchmark correctness validation
cargo test --test benchmark_validation

# Run performance regression detection
cargo test --test performance_regression
```

## Benchmark Results Interpretation

### BenchmarkResult Structure
```rust
pub struct BenchmarkResult {
    pub name: String,           // Test name
    pub elapsed_ms: u64,        // Actual time
    pub slo_ms: u64,           // Target time
    pub passed: bool,          // SLO met?
    pub throughput: Option<u64>, // msgs/sec (optional)
}
```

### Key Metrics
- **Status**: PASS/FAIL based on SLO
- **Slack**: Percentage of headroom (positive = margin, negative = breach)
- **Throughput**: Messages/sec for throughput tests

Example output:
```
Test: agent_creation
  Elapsed: 85ms
  SLO: 100ms
  Slack: 15.0%
  Status: PASS
```

## Test Coverage

### SLO Validation (8 tests)
- Agent creation ≤100ms
- Agent startup ≤500ms
- Tool discovery ≤200ms
- Plan generation ≤1,000ms (10-step)
- Tool execution ≤100ms
- Consensus ≤2,000ms (3 agents)
- Domain balance ≤500ms
- SLO configuration validation

### Benchmark Validation (7 tests)
- Pass/fail determination
- Slack calculation
- Negative slack (breach) detection
- Throughput measurement
- Multiple result handling
- Slack percentage calculation

### Regression Detection (8 tests)
- Agent creation regression (10% tolerance)
- Tool discovery regression (15% tolerance)
- Throughput maintenance/improvement
- Consensus performance consistency
- Linear scaling (plan generation)
- Memory efficiency
- Performance cliff detection
- Tool execution consistency

## Performance Characteristics

### Expected Performance on Modern Hardware

| Operation | Expected | Target | Headroom |
|-----------|----------|--------|----------|
| Agent creation | ~75ms | 100ms | 25% |
| Agent startup | ~400ms | 500ms | 20% |
| Tool discovery | ~150ms | 200ms | 25% |
| Plan generation | ~850ms | 1000ms | 15% |
| Tool execution | ~85ms | 100ms | 15% |
| Consensus | ~1800ms | 2000ms | 10% |
| Domain balance | ~450ms | 500ms | 10% |

### Scalability

- **Agent count**: O(1) creation, O(n) health check
- **Message throughput**: >10k msgs/sec on modern CPUs
- **Plan size**: Linear scaling up to 20 steps
- **Consensus**: O(n) for n agents, safety proof: 3f+1

## Performance Regression Detection

The regression tests detect:

1. **Absolute Regression**: Performance degradation in individual operations
2. **Consistency Regression**: Increased variance in measurements
3. **Throughput Regression**: Decreased message throughput
4. **Scaling Regression**: Non-linear scaling behavior
5. **Memory Regression**: Increased memory usage patterns

### Tolerance Levels

- Agent creation: 10% tolerance
- Tool discovery: 15% tolerance
- Consensus: 5% tolerance
- Throughput: Must maintain or improve

## Optimization Recommendations

If SLOs are not met:

### Agent Creation (target: ≤100ms)
- Use object pooling for frequent agent creation
- Pre-allocate UUID generation
- Consider lazy initialization of non-essential fields

### Tool Discovery (target: ≤200ms)
- Cache tool registry
- Use HashMap for O(1) lookup
- Consider async tool loading

### Plan Generation (target: ≤1000ms for 10 steps)
- Parallelize step generation where possible
- Use iterative refinement instead of global optimization
- Cache common plan templates

### Consensus (target: ≤2000ms for 3 agents)
- Reduce voting overhead
- Parallelize vote collection
- Consider asynchronous consensus

### Domain Balance (target: ≤500ms)
- Cache balance calculations
- Incremental updates instead of full recalculation
- Parallel domain scoring

## Architecture

```
┌─────────────────────────────────────────────┐
│    Performance Benchmarks Suite             │
├─────────────────────────────────────────────┤
│                                             │
│  ┌──────────────────────────────────────┐  │
│  │ Criterion Benchmarks (4 categories)  │  │
│  │ - Agent creation                     │  │
│  │ - Message throughput                 │  │
│  │ - Tool discovery                     │  │
│  │ - Plan generation                    │  │
│  └──────────────────────────────────────┘  │
│                ↓                            │
│  ┌──────────────────────────────────────┐  │
│  │ SLO Validation Tests (8 tests)       │  │
│  │ - Verify all operations vs targets   │  │
│  │ - Report slack/breach                │  │
│  └──────────────────────────────────────┘  │
│                ↓                            │
│  ┌──────────────────────────────────────┐  │
│  │ Benchmark Correctness (7 tests)      │  │
│  │ - Result calculation accuracy        │  │
│  │ - Pass/fail determination            │  │
│  └──────────────────────────────────────┘  │
│                ↓                            │
│  ┌──────────────────────────────────────┐  │
│  │ Regression Detection (8 tests)       │  │
│  │ - Compare vs baseline                │  │
│  │ - Detect performance cliffs          │  │
│  │ - Verify consistency                 │  │
│  └──────────────────────────────────────┘  │
│                                             │
└─────────────────────────────────────────────┘
```

## Interpreting Results

### Passed SLO
```
agent_creation: 85ms ≤ 100ms ✓
Slack: 15%
```

### Breached SLO
```
consensus: 2150ms > 2000ms ✗
Breach: 150ms (7.5%)
```

### Regression Detected
```
tool_discovery: 190ms (baseline: 180ms)
Increase: 10ms (5.6%) - WITHIN TOLERANCE
```

## CI/CD Integration

For continuous monitoring:

```bash
# Run full benchmark suite
cargo bench --example performance-benchmarks > bench_results.txt

# Run SLO validation only
cargo test --test slo_validation

# Check for regressions
cargo test --test performance_regression
```

All tests must pass for release. Failed SLOs block deployment.

## References

- Performance testing best practices
- SLO/SLI definitions and measurement
- Criterion benchmarking framework
- Statistical analysis of performance data
