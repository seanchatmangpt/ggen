# Chicago TDD: Full CONSTRUCT8 Pipeline Tests

## Overview

Comprehensive Chicago TDD test suite for the complete CONSTRUCT8 pipeline: **Rust → C → Rust**

## Pipeline Flow

```
1. Rust Warm Path (Warm)
   └─> Parse Turtle (rio_turtle)
   └─> Prepare CONSTRUCT8 IR
   └─> Hash IRIs to u64 IDs
   └─> Validate guards (run.len ≤ 8)
   └─> Prepare SoA arrays (64-byte aligned)
   │
2. C Hot Path (≤8 ticks / ≤2ns) ⚡
   └─> Execute CONSTRUCT8 (knhks_eval_construct8)
   └─> SIMD operations (load-mask-blend-store)
   └─> Emit triples to output arrays
   └─> Generate receipt (span_id, a_hash)
   │
3. Rust Warm Path (Warm)
   └─> Process results
   └─> Verify output triples
   └─> Validate receipt
   └─> Return constructed triples
```

## Test Files

### 1. C Test: `tests/chicago_construct8_pipeline.c`

**Purpose**: End-to-end pipeline test using C test framework

**Tests**:
- `test_pipeline_turtle_parsing()`: Full pipeline with Turtle file parsing
- `test_pipeline_manual_triples()`: Full pipeline with manual triple setup
- `test_pipeline_prefix_resolution()`: Prefix resolution → C hot path → result processing
- `test_pipeline_performance()`: Performance validation (1000 iterations, ≤8 ticks)
- `test_pipeline_error_handling()`: Error handling (empty runs, invalid inputs)
- `test_pipeline_idempotence()`: Idempotence test (μ∘μ = μ)

**Run**: `make test-construct8-pipeline`

### 2. Rust Test: `rust/knhks-integration-tests/tests/construct8_pipeline.rs`

**Purpose**: Full pipeline test using Rust FFI to C hot path

**Tests**:
- `test_construct8_pipeline_rust_to_c_to_rust()`: Complete pipeline using Rust FFI
- `test_construct8_pipeline_performance()`: Performance validation (1000 iterations)
- `test_construct8_pipeline_idempotence()`: Idempotence verification

**Run**: `cd rust/knhks-integration-tests && cargo test construct8_pipeline`

## Chicago TDD Principles Applied

### ✅ No Mocks, Real Implementations Only
- Uses real `rio_turtle` parser (not mocked)
- Uses real C hot path (`knhks_eval_construct8`)
- Uses real FFI layer (`knhks_hot::Engine`)

### ✅ Direct Assertions on Behavior
- Asserts triple count matches expected
- Asserts output triples match template
- Asserts receipt fields are populated
- Asserts performance constraints (≤8 ticks)

### ✅ Performance Validation
- Measures timing around C hot path call
- Validates ≤8 ticks (Chatman Constant)
- Validates ≤2ns (2.0 nanoseconds)
- Cache warming before measurement
- 1000 iterations for statistical significance

### ✅ End-to-End Verification
- Tests complete pipeline (Rust → C → Rust)
- Tests data flow correctness
- Tests error handling
- Tests idempotence (μ∘μ = μ)

## Test Scenarios

### Scenario 1: Basic Pipeline
```c
// 1. Parse Turtle
knhks_rdf_load("tests/data/enterprise_authorization.ttl", S, P, O, NROWS, &count);

// 2. Prepare IR
knhks_pin_run(&ctx, (knhks_pred_run_t){.pred = pred, .off = 0, .len = count});

// 3. Execute CONSTRUCT8 (C hot path)
knhks_eval_construct8(&ctx, &ir, &rcpt);

// 4. Verify results
assert(written > 0);
assert(rcpt.ticks <= 8);
```

### Scenario 2: Prefix Resolution
```c
// Turtle with prefixes
@prefix ex: <http://example.org/> .
ex:alice ex:role ex:admin .

// Rust warm path resolves prefixes → full IRIs
// C hot path receives hashed IRIs
// Result: Correct triples emitted
```

### Scenario 3: Performance Validation
```c
// Cache warming
for (int i = 0; i < 100; i++) {
    knhks_eval_construct8(&ctx, &ir, &rcpt);
}

// Measure 1000 iterations
for (int i = 0; i < 1000; i++) {
    uint64_t t0 = knhks_rd_ticks();
    knhks_eval_construct8(&ctx, &ir, &rcpt);
    uint64_t t1 = knhks_rd_ticks();
    // Validate ≤8 ticks
}
```

## Key Validations

### 1. Data Flow Correctness
- ✅ Input triples parsed correctly
- ✅ IRIs hashed correctly (FNV-1a)
- ✅ SoA arrays prepared correctly
- ✅ Output triples match template
- ✅ Receipt fields populated correctly

### 2. Performance Constraints
- ✅ C hot path ≤8 ticks (Chatman Constant)
- ✅ C hot path ≤2ns (2.0 nanoseconds)
- ✅ Cache warming applied
- ✅ Prefetch hints used (where applicable)

### 3. Error Handling
- ✅ Empty runs handled correctly
- ✅ Invalid inputs rejected
- ✅ Error messages include context

### 4. Idempotence
- ✅ Same input → same output (μ∘μ = μ)
- ✅ Deterministic execution
- ✅ Receipt consistency

## Build & Run

### C Tests
```bash
# Build
make test-construct8-pipeline

# Run
./tests/chicago_construct8_pipeline

# Or via Makefile
make test-construct8-pipeline
```

### Rust Tests
```bash
# Build and run
cd rust/knhks-integration-tests
cargo test construct8_pipeline

# With output
cargo test construct8_pipeline -- --nocapture
```

## Expected Output

### C Test Output
```
========================================
Chicago TDD: Full CONSTRUCT8 Pipeline
Rust → C → Rust Integration Tests
========================================

[TEST] Full Pipeline: Turtle Parsing → C Hot Path → Result Processing
  ✓ Parsed 8 triples from Turtle file
  ✓ Pipeline executed: 8 triples emitted, ticks=6, ns=1.50
  ✓ Receipt: lanes=8, span_id=0x1234..., a_hash=0x5678...

[TEST] Full Pipeline: Manual Triples → C Hot Path → Result Processing
  ✓ Pipeline executed: 3 triples emitted, ticks=4, ns=1.00

[TEST] Full Pipeline: Prefix Resolution → C Hot Path → Result Processing
  ✓ Pipeline executed with prefix resolution: 2 triples, ticks=4, ns=1.00

[TEST] Full Pipeline: Performance Validation (1000 iterations)
  Max ticks observed: 6 (budget = 8)
  Max nanoseconds observed: 1.50 (budget = 2.00)
  ✓ Performance validation passed: max_ticks=6, max_ns=1.50

[TEST] Full Pipeline: Error Handling
  ✓ Empty run handled correctly
  ✓ Error handling validated

[TEST] Full Pipeline: Idempotence (μ∘μ = μ)
  ✓ Pipeline is idempotent (μ∘μ = μ)

========================================
Results: 6/6 tests passed
========================================
```

### Rust Test Output
```
running 3 tests
test construct8_pipeline::test_construct8_pipeline_rust_to_c_to_rust ... ok
test construct8_pipeline::test_construct8_pipeline_performance ... ok
test construct8_pipeline::test_construct8_pipeline_idempotence ... ok

test result: ok. 3 passed; 0 failed; 0 ignored; 0 measured; 0 filtered out
```

## Integration Points

### Rust → C FFI
- `knhks_hot::Engine`: Safe Rust wrapper around C hot path
- `knhks_hot::Ir`: FFI-safe IR structure
- `knhks_hot::Receipt`: FFI-safe receipt structure

### C → Rust Data Flow
- Rust prepares SoA arrays (64-byte aligned)
- Rust calls C hot path via FFI
- C executes CONSTRUCT8 (≤8 ticks)
- C fills output arrays and receipt
- Rust processes results

## Performance Targets

| Component | Target | Validation |
|-----------|--------|------------|
| **C Hot Path** | ≤8 ticks | Assert in test |
| **C Hot Path** | ≤2ns | Assert in test |
| **Rust Warm Path** | <500ms | Not validated (warm path) |
| **Total Pipeline** | <500ms | Not validated (warm path) |

## Notes

- **C hot path only**: Performance validation applies to C hot path execution only
- **Rust warm path**: Parsing and preparation are warm path (no tick budget)
- **Cache warming**: Tests include cache warming before measurement
- **Real implementations**: No mocks, all real components

## Future Enhancements

1. **Rust FFI Integration**: Full Rust → C → Rust test using FFI
2. **Streaming Tests**: Test with large Turtle files (streaming parser)
3. **Error Recovery**: Test error recovery scenarios
4. **Batch Processing**: Test batch CONSTRUCT8 operations
5. **Receipt Merging**: Test receipt merging (⊕ operation)

