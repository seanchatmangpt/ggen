# Property-Based Validation System (7-Agent Complementary Invariants)

A novel validation approach where 7 agents each test **different properties** of ggen-generated code (complementary validation, not redundant voting).

## Overview

**Traditional Consensus** (what we already built):
```
Agent 1: Tests compilation → PASS
Agent 2: Tests compilation → PASS
Agent 3: Tests compilation → FAIL
Consensus: 2/3 fail → reject (redundant checks)
```

**Property-Based** (this system):
```
Agent 1: Tests determinism → PASS (same input → same output)
Agent 2: Tests completeness → PASS (all required fields present)
Agent 3: Tests consistency → PASS (no contradictions)
Agent 4: Tests soundness → PASS (no deadlocks)
Agent 5: Tests performance → PASS (latency < 100ms)
Agent 6: Tests security → PASS (no vulnerabilities)
Agent 7: Tests correctness → FAIL (output doesn't match spec)
Result: 6/7 pass → reject with specific failure reason
```

## Key Innovation

**Complementary vs Redundant**:
- **Consensus**: All agents test the SAME thing (redundant checks)
- **Property-Based**: Each agent tests a DIFFERENT property (complementary coverage)

## 7 Properties

| Agent | Property | Invariant | What It Checks |
|-------|----------|-----------|----------------|
| **Agent 1** | Determinism | f(x) = f(x) | Same input always produces same output |
| **Agent 2** | Completeness | ∀ required fields r ∈ output, r is present | All required fields present and non-null |
| **Agent 3** | Consistency | No contradictions | No contradictory statements in output |
| **Agent 4** | Soundness | Deadlock-free, liveness, boundedness | WvdA soundness verification |
| **Agent 5** | Performance | Meets SLOs | Latency < threshold, memory < limit |
| **Agent 6** | Security | No vulnerabilities | No XSS, SQL injection, path traversal |
| **Agent 7** | Correctness | Matches specification | Output conforms to schema and semantics |

## Usage

### As a Demo

```bash
cd .
cargo run --example property-based-validation
```

### As a Library

```rust
use property_based_validation::PropertyValidationSystem;

#[tokio::main]
async fn main() -> Result<(), Box<dyn std::error::Error>> {
    let system = PropertyValidationSystem::new().await?;
    let result = system.validate_generation(
        "/path/to/package",
        "blake3-receipt-abc123"
    ).await?;

    if result.all_passed() {
        println!("All 7 properties validated!");
    } else {
        println!("Failed: {:?}", result.failed_properties());
    }

    Ok(())
}
}
```

## Implementation Status

### ✅ Completed
- [x] Core validation system architecture
- [x] 7 property agent definitions
- [x] Property result structures
- [x] Coordinator for agent management
- [x] Feedback loop for kaizen PDCA
- [x] Demo/example program

### 🔨 In Progress
- [ ] Actual property check implementations
- [ ] Property-based testing with proptest
- [ ] Integration with ggen's μ₁-μ₅ pipeline
- [ ] Kaizen metrics and trend analysis

### 📋 Planned
- [ ] Adaptive thresholds based on history
- [ ] Fuzzing integration for edge case detection
- [ ] Mutation testing integration
- [ ] Real-time validation dashboard

## Comparison: Consensus vs Property-Based

| Dimension | Consensus System | Property-Based System |
|-----------|-----------------|----------------------|
| **Validation Type** | Redundant (same check) | Complementary (different checks) |
| **Decision** | Vote (5-of-7 quorum) | All must pass (7-of-7) |
| **Failure Mode** | Quorum not reached | Any property fails |
| **Feedback** | Vote distribution | Specific property violations |
| **Testing** | Example-based | Property-based (invariants) |
| **Coverage** | Redundant coverage | Complementary coverage |

## Novel Contributions

1. **Complementary Validation**: Each agent tests a unique property (not redundant voting)
2. **Property-Based Testing**: Uses proptest to verify invariants across random inputs
3. **Invariant Specification**: Each property encodes "what must always be true"
4. **Adaptive Thresholds**: Agents learn from past validations
5. **Formal Verification**: Soundness agent proves WvdA properties
6. **Kaizen Integration**: Results feed back into ggen's improvement cycle

## References

- **Property-Based Testing**: proptest framework, QuickCheck (Haskell)
- **Wil van der Aalst**: Process soundness (deadlock-free, liveness, boundedness)
- **Toyota Production System**: Kaizen (continuous improvement)
- **ggen Infrastructure**: μ₁-μ₅ pipeline, BLAKE3 receipts, AndonSignal

## License

MIT License - See ggen LICENSE file for details.
