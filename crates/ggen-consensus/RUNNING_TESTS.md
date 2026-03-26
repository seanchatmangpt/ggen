# Running Byzantine Consensus Schema Validation Tests

## Quick Start

```bash
# Run all schema consensus validation tests
cargo test --test schema_consensus_validation --package ggen-consensus

# Run with detailed output (shows consensus report)
cargo test --test schema_consensus_validation --package ggen-consensus test_schema_consensus_full_validation_report -- --nocapture
```

## Full Test Suite

### Run All Tests

```bash
cd /Users/sac/ggen/crates/ggen-consensus

# Run all 9 tests
cargo test --test schema_consensus_validation

# Expected output:
# running 9 tests
# test test_schema_consensus_pbft_quorum_calculation ... ok
# test test_schema_consensus_validates_multiple_data_scenarios ... ok
# test test_schema_consensus_honest_nodes_agree_on_invalid_data ... ok
# test test_schema_consensus_all_nodes_load_same_schemas ... ok
# test test_schema_consensus_byzantine_tolerance_4_nodes_1_fault ... ok
# test test_schema_consensus_honest_nodes_agree_on_valid_data ... ok
# test test_schema_consensus_all_nodes_consistent ... ok
# test test_schema_consensus_byzantine_tolerance_7_nodes_2_faults ... ok
# test test_schema_consensus_full_validation_report ... ok
# test result: ok. 9 passed; 0 failed
```

## Individual Test Execution

### Test 1: Schema Loading

```bash
cargo test --test schema_consensus_validation test_schema_consensus_all_nodes_load_same_schemas -- --nocapture
```

**What it tests**: Verifies all consensus nodes initialize with identical validation schemas

---

### Test 2: Honest Consensus on Valid Data

```bash
cargo test --test schema_consensus_validation test_schema_consensus_honest_nodes_agree_on_valid_data -- --nocapture
```

**What it tests**: All nodes should unanimously agree valid data is valid

**Expected behavior**: All 4 nodes vote `Valid`

---

### Test 3: Honest Consensus on Invalid Data

```bash
cargo test --test schema_consensus_validation test_schema_consensus_honest_nodes_agree_on_invalid_data -- --nocapture
```

**What it tests**: All nodes should unanimously agree invalid data is invalid

**Expected behavior**: All 4 nodes vote `Invalid`

---

### Test 4: Byzantine Tolerance (4 nodes, 1 dishonest)

```bash
cargo test --test schema_consensus_validation test_schema_consensus_byzantine_tolerance_4_nodes_1_fault -- --nocapture
```

**What it tests**: Byzantine node voting opposite should be overruled

**Expected behavior**:
```
Nodes 0-2: Valid (honest)
Node 3: Invalid (Byzantine - inverted)
Consensus: VALID (3 >= threshold of 3)
```

---

### Test 5: Byzantine Tolerance (7 nodes, 2 dishonest)

```bash
cargo test --test schema_consensus_validation test_schema_consensus_byzantine_tolerance_7_nodes_2_faults -- --nocapture
```

**What it tests**: Multiple Byzantine nodes cannot change consensus with 7-node network

**Expected behavior**:
```
Nodes 0-4: Invalid (honest, 5 votes)
Nodes 5-6: Valid (Byzantine, 2 votes)
Consensus: INVALID (5 >= threshold of 5)
```

---

### Test 6: Schema Validation Consistency

```bash
cargo test --test schema_consensus_validation test_schema_consensus_validates_multiple_data_scenarios -- --nocapture
```

**What it tests**: Schema validates consistently across different scenarios

**Scenarios**:
- Valid user (all fields correct) → Valid
- Missing email → Invalid
- Bad email format → Invalid
- Username exceeds max length → Invalid

---

### Test 7: PBFT Quorum Calculation

```bash
cargo test --test schema_consensus_validation test_schema_consensus_pbft_quorum_calculation -- --nocapture
```

**What it tests**: PBFT quorum math is correct for networks of various sizes

**Verifies**:
- 4 nodes: f=1, need 3 votes
- 5 nodes: f=1, need 3 votes
- 6 nodes: f=1, need 3 votes
- 7 nodes: f=2, need 5 votes
- 10 nodes: f=3, need 7 votes

---

### Test 8: Cross-Node Consistency

```bash
cargo test --test schema_consensus_validation test_schema_consensus_all_nodes_consistent -- --nocapture
```

**What it tests**: 5 honest nodes independently reach identical consensus

**Expected behavior**: All 5 nodes vote `Valid` for valid data

---

### Test 9: Full Validation Report (RECOMMENDED)

```bash
cargo test --test schema_consensus_validation test_schema_consensus_full_validation_report -- --nocapture
```

**What it tests**: Complete demonstration of all capabilities

**Output includes**:
- Schema loading verification
- Honest consensus (valid data)
- Honest consensus (invalid data)
- Byzantine tolerance demonstration
- Schema validation consistency check
- Final results summary

**Example output**:
```
========================================
SCHEMA CONSENSUS VALIDATION REPORT
========================================

✅ TEST 1: All nodes loaded same generated schemas
   Nodes created: 4
   Validators initialized: 4

✅ TEST 2: Honest consensus on valid data
   Node 0: Valid
   Node 1: Valid
   Node 2: Valid
   Node 3: Valid
   Result: CONSENSUS REACHED (Valid)

[... more output ...]

========================================
RESULTS
========================================
✅ All nodes loaded same generated schemas
✅ Honest consensus on valid data
✅ Honest consensus on invalid data
✅ Byzantine tolerance (dishonest node overruled)
✅ Consensus matches schema validation
✅ TESTS: 6/6 passed
========================================
```

## Running Tests from Root Directory

```bash
# From /Users/sac/ggen
cargo test --test schema_consensus_validation --package ggen-consensus

# Or navigate to the crate directory
cd crates/ggen-consensus
cargo test --test schema_consensus_validation
```

## Running with Additional Options

### Run tests sequentially (instead of parallel)

```bash
cargo test --test schema_consensus_validation -- --test-threads=1
```

### Run with backtrace on panic

```bash
RUST_BACKTRACE=1 cargo test --test schema_consensus_validation
```

### Run specific test function with full output

```bash
cargo test --test schema_consensus_validation test_schema_consensus_byzantine_tolerance_4_nodes_1_fault -- --nocapture --exact
```

### Run all ggen-consensus tests

```bash
cargo test --package ggen-consensus
```

## Verifying Test Results

### All tests should pass

```bash
test result: ok. 9 passed; 0 failed; 0 ignored; 0 measured; 0 filtered out
```

### If any test fails:

1. Run the failing test with output:
   ```bash
   cargo test --test schema_consensus_validation test_NAME -- --nocapture
   ```

2. Check for assertion errors or panic messages

3. Review the test code in:
   ```
   /Users/sac/ggen/crates/ggen-consensus/tests/schema_consensus_validation.rs
   ```

## Performance Expectations

- **Compilation time**: ~1 second (incremental build)
- **Full test execution**: <1 second (all 9 tests)
- **Memory usage**: Minimal (single process, no concurrency)
- **CPU usage**: Minimal (simple consensus simulation)

## Troubleshooting

### "test target not found"

Make sure you're running from the correct directory or use the full package specification:

```bash
cargo test --test schema_consensus_validation --package ggen-consensus
```

### Compilation errors

Ensure your Rust version is up to date:

```bash
rustc --version  # Should be 1.91.1 or later
```

### Tests hang or timeout

The tests should complete in <1 second. If they don't:
1. Check for external processes blocking the test
2. Try running with `--test-threads=1`
3. Check system resources (CPU, memory)

## Documentation

- **Architecture**: See `CONSENSUS_SCHEMA_VALIDATION.md`
- **Detailed Results**: See `TEST_EXECUTION_REPORT.md`
- **Test Code**: See `tests/schema_consensus_validation.rs`

## Integration Notes

The test uses simulated schemas that mirror ggen-generated patterns:
- Email validation (must contain '@')
- Field length constraints (username ≤ 255, bio ≤ 500)
- Required field validation
- Optional field handling

To integrate with real ggen-generated schemas:
1. Generate schemas with `ggen sync`
2. Import from `examples/openapi-variants/golden/schemas.js`
3. Add TypeScript validation nodes to the consensus system
4. Extend tests to validate across multiple languages

---

**Last Updated**: March 24, 2026
**Test Suite Version**: v1.0.0
