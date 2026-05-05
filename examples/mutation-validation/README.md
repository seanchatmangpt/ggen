# Mutation-Based Validation System (7-Agent Adversarial Testing)

A novel validation approach where 7 agents perform **active adversarial testing** — they try to BREAK the code rather than validate it.

## Overview

**Traditional Validation** (what we already built):
```
Check if code compiles → PASS
Check if tests pass → PASS
Result: Code is valid (passive checking)
```

**Mutation-Based** (this system):
```
Agent 1: Inject syntax mutation (delete line) → Run tests → SURVIVED
Agent 2: Inject logic mutation (flip boolean) → Run tests → KILLED
Agent 3: Inject boundary mutation (off-by-one) → Run tests → SURVIVED
...
Result: Mutation Score = 33% (tests caught 1/3 mutations)
```

## Key Innovation

**Active vs Passive**:
- **Consensus/Property-Based**: Passive validation (check if code is correct)
- **Mutation-Based**: Active adversarial testing (try to BREAK the code)

## 7 Agents (Two-Phase Workflow)

### PHASE 1: Inject Mutations (Agents 1-3)

| Agent | Role | What It Does |
|-------|------|--------------|
| **Agent 1** | Syntax Injector | Change operators, delete statements |
| **Agent 2** | Logic Injector | Flip conditionals, negate expressions |
| **Agent 3** | Boundary Injector | Off-by-one errors, loop bounds |

### PHASE 2: Detect and Analyze (Agents 4-7)

| Agent | Role | What It Does |
|-------|------|--------------|
| **Agent 4** | Unit Test Runner | Run unit tests to detect mutations |
| **Agent 5** | Integration Test Runner | Run integration tests to detect mutations |
| **Agent 6** | Score Calculator | Calculate mutation score (killed/survived) |
| **Agent 7** | Test Improver | Suggest tests for surviving mutations |

## Usage

### As a Demo

```bash
cd .
cargo run --example mutation-validation
```

### As a Library

```rust
use mutation_validation::MutationValidationSystem;

#[tokio::main]
async fn main() -> Result<(), Box<dyn std::error::Error>> {
    let system = MutationValidationSystem::new().await?;

    let report = system.run_mutation_testing(
        "/path/to/package",
        "fn main() { println!(\"Hello\"); }",
        "cargo test"
    ).await?;

    println!("Mutation Score: {:.1}%", report.result.mutation_score * 100.0);
    println!("Assessment: {}", report.assessment);

    for suggestion in report.suggestions {
        println!("Add test: {}", suggestion.suggested_test);
    }

    Ok(())
}
```

## Mutation Score

The **mutation score** is a quantitative measure of test quality:

```
Mutation Score = (Killed + Errors) / Total Mutations

Where:
  Killed = Mutation detected by tests (good)
  Survived = Mutation not detected (test gap)
  Error = Mutation caused compilation error (neutral)

Target Score:
  80%+ = Excellent (strong test suite)
  50-80% = Good (room for improvement)
  30-50% = Fair (significant gaps)
  <30% = Poor (critical weaknesses)
```

## Comparison: Three Approaches

| Dimension | Consensus | Property-Based | Mutation-Based |
|-----------|-----------|----------------|----------------|
| **What agents do** | Vote on SAME check | Test DIFFERENT properties | Try to BREAK code |
| **Decision logic** | 5-of-7 quorum | All 7 must pass | Mutation score |
| **Primary goal** | Fault tolerance | Comprehensive coverage | Test quality |
| **Testing style** | Example-based | Property-based (invariants) | Adversarial |
| **Feedback** | Vote distribution | Property violations | Test suggestions |
| **Use case** | Distributed agreement | Specification compliance | Test suite strength |

## Novel Contributions

1. **Active Adversarial Testing**: Agents try to BREAK code, not validate it
2. **Mutation Score**: Quantitative measure of test quality (0-100%)
3. **Test Gap Detection**: Surviving mutations reveal test weaknesses
4. **Targeted Suggestions**: Agent 7 suggests specific tests to add
5. **Two-Phase Workflow**: Clear separation (inject → detect)
6. **Competitive Testing**: Tests compete against mutations

## When To Use

### Use Mutation-Based When:

1. **Test quality assessment**: Need to measure how good tests are
2. **Test gap detection**: Want to find missing test cases
3. **Adversarial validation**: Need to harden code against bugs
4. **Regression testing**: Ensure new tests catch old bugs

### Don't Use Mutation-Based When:

1. **Quick validation needed**: Mutation testing is SLOW
2. **Fault tolerance critical**: Use consensus instead
3. **Specification compliance**: Use property-based instead
4. **Large codebase**: Too many mutations to test

## Example Output

```
=== Mutation Testing Result ===

📊 Mutation Score: 65.2%
   Total mutations: 150
   Killed (detected): 98
   Survived (missed): 52
   Errors: 0

📋 Assessment: Good: Tests are catching most mutations, but room for improvement

⚠️  Surviving Mutations (Test Gaps):
   Line 15: 'true' → 'false'
   Line 23: 'x == 10' → 'x != 10'
   Line 47: '[0]' → '[1]'

💡 Test Improvement Suggestions:
   1. [Critical] test_logic_kills_mutation_abc123
      Rationale: Add test that checks for 'true -> false' mutation at line 15
   2. [Medium] test_relational_kills_mutation_def456
      Rationale: Add test that checks for '== -> != ' mutation at line 23
   3. [Medium] test_boundary_kills_mutation_ghi789
      Rationale: Add test that checks for '[0] -> [1]' mutation at line 47
```

## References

- **Mutation Testing**: Jia, Harrold (2011), "An Analysis and Survey of the Evaluation of Mutation Testing"
- **Property-Based Testing**: proptest framework, QuickCheck (Haskell)
- **Consensus-Based**: `./examples/7-agent-validation/`
- **Property-Based**: `./examples/property-based-validation/`

## License

MIT License - See ggen LICENSE file for details.
