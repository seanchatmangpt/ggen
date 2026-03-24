# Divergence Reporter

## Overview

The Divergence Reporter is a critical component of the ln_ctrl verification system that generates actionable repair suggestions when execution verification fails. It compares expected vs actual execution state to identify the first divergence point and provides structured, prioritized repair recommendations.

## Features

### Core Functionality

1. **Divergence Detection**
   - Finds first divergence point (step number, redex)
   - Compares receipt chains
   - Validates cryptographic hash chains
   - Checks frontier states
   - Verifies effect execution
   - Monitors budget compliance

2. **State Comparison**
   - Frontier term comparison (expected vs actual)
   - Effect list comparison (type, data, results)
   - Budget state comparison (steps, memory, time)
   - Receipt chain integrity validation

3. **Diagnosis Generation**
   - Human-readable diagnosis string
   - Root cause identification
   - Impact assessment
   - Context-aware explanations

4. **Repair Suggestions**
   - Prioritized action items (critical, high, medium, low)
   - Actionable commands
   - Rationale for each suggestion
   - Estimated impact assessment

### Supported Divergence Types

| Type | Description | Severity |
|------|-------------|----------|
| `receipt_chain_divergence` | Hash chain integrity broken | High |
| `trace_hash_mismatch` | Execution trace hash differs | High |
| `frontier_difference` | Frontier state diverged | Medium |
| `effect_mismatch` | Side effects differ | High |
| `budget_violation` | Resource limits exceeded | High |
| `operation_mismatch` | Different operation executed | Medium |
| `causal_chain_broken` | Causal chain discontinuity | Critical |
| `signature_invalid` | Cryptographic signature invalid | Critical |

## Usage

### Basic Usage

```javascript
import { analyzeDivergence, printDivergenceReport } from './divergence_reporter.mjs';

// Compare expected vs actual execution
const report = analyzeDivergence(expectedState, actualState);

// Print formatted report to console
printDivergenceReport(report);
```

### Save Report to File

```javascript
import { generateDivergenceReport } from './divergence_reporter.mjs';

// Generate and save report
const report = await generateDivergenceReport(
  expectedState,
  actualState,
  './divergence.json'
);

console.log(`Report saved: ${report.report_id}`);
```

### Programmatic Analysis

```javascript
import { analyzeDivergence, DivergenceType, Severity } from './divergence_reporter.mjs';

const report = analyzeDivergence(expected, actual);

if (report.divergence_detected) {
  console.log(`Divergence at step ${report.divergence_point.step_index}`);
  console.log(`Type: ${report.divergence_type}`);
  console.log(`Severity: ${report.severity}`);

  // Process repair suggestions
  for (const suggestion of report.repair_suggestions) {
    if (suggestion.priority === 'critical') {
      console.log(`🔴 ${suggestion.action}`);
      if (suggestion.command) {
        console.log(`   Run: ${suggestion.command}`);
      }
    }
  }
}
```

## Input Format

### Expected State Structure

```javascript
{
  workflow_id: "550e8400-e29b-41d4-a716-446655440000",
  receipts: [
    {
      step_index: 0,
      operation: "reduce",
      hash_chain: "a1b2c3...",
      redex_executed: {
        redex_type: "beta",
        redex_expression: "(λx.x) 42"
      },
      frontier_after: {
        frontier_terms: ["42"],
        frontier_size: 1,
        frontier_hash: "d4e5f6..."
      },
      effects_performed: [],
      budget_remaining: {
        budget_steps: 10000,
        budget_memory: 100000000,
        budget_time: 30000,
        budget_exceeded: false
      }
    }
    // ... more receipts
  ]
}
```

### Actual State Structure

Same structure as expected state. The analyzer compares the two and identifies divergence points.

## Output Format

### Divergence Report Schema

```javascript
{
  report_id: "uuid",
  timestamp: "2026-02-11T21:45:00Z",
  workflow_id: "uuid",
  divergence_detected: true,

  divergence_point: {
    step_index: 42,
    operation: "reduce",
    redex_type: "beta",
    redex_expression: "(λx.x+1) 5"
  },

  divergence_type: "frontier_difference",

  expected_state: {
    hash_chain: "...",
    frontier: { ... },
    effects: [ ... ],
    budget: { ... }
  },

  actual_state: {
    hash_chain: "...",
    frontier: { ... },
    effects: [ ... ],
    budget: { ... }
  },

  diagnosis: "Human-readable explanation...",

  repair_suggestions: [
    {
      priority: "critical",
      action: "What to do",
      rationale: "Why do it",
      command: "ggen validate --check-config",
      estimated_impact: "Expected result"
    }
  ],

  affected_receipts: ["hash1", "hash2"],
  severity: "high",
  reproducible: true,

  metadata: {
    detector_version: "1.0.0",
    analysis_duration_ms: 127,
    context: { ... }
  }
}
```

## Repair Suggestion Priorities

### Critical Priority

- Causal chain broken
- Cryptographic signature invalid
- Early termination or infinite loop
- Budget violations

**Action Required:** Stop and fix immediately before proceeding.

### High Priority

- Hash chain mismatch
- Trace hash divergence
- Receipt count mismatch
- Non-deterministic operations detected

**Action Required:** Fix before deployment or production use.

### Medium Priority

- Frontier state differences
- Normalizer configuration issues
- Reduction strategy mismatches

**Action Required:** Review and address, may require regenerating expected traces.

### Low Priority

- Minor effect differences
- Small budget variances
- Configuration drift

**Action Required:** Monitor and address during regular maintenance.

## Example Scenarios

### Scenario 1: Frontier Divergence (Incomplete Normalization)

**Symptoms:**
- Expected: `["6", "42"]`
- Actual: `["5+1", "42"]`

**Diagnosis:**
Arithmetic reduction did not complete, suggesting normalizer interruption or configuration mismatch.

**Repair Suggestions:**
1. **[CRITICAL]** Verify normalizer configuration
2. **[HIGH]** Check reduction strategy settings
3. **[MEDIUM]** Regenerate expected trace with current settings

### Scenario 2: Receipt Chain Divergence

**Symptoms:**
- Hash chain mismatch at step 15
- All subsequent receipts invalid

**Diagnosis:**
Non-deterministic operation introduced randomness, breaking cryptographic chain.

**Repair Suggestions:**
1. **[CRITICAL]** Verify cryptographic chain integrity
2. **[HIGH]** Check for non-deterministic operations (RNG, timestamps, I/O)
3. **[HIGH]** Ensure RNG seeding is correctly configured

### Scenario 3: Budget Violation

**Symptoms:**
- Expected: `budget_exceeded: false`
- Actual: `budget_exceeded: true` at step 142

**Diagnosis:**
Execution exceeded resource limits, suggesting infinite loop or performance regression.

**Repair Suggestions:**
1. **[CRITICAL]** Investigate budget violation root cause
2. **[HIGH]** Analyze step count divergence (142+ unexpected steps)
3. **[MEDIUM]** Review algorithm changes or optimizations

## Integration with Verification Pipeline

```bash
# 1. Run execution with actual state
node execute.mjs --replay-pack scenario.json > actual.json

# 2. Compare with expected state
node verify.mjs --expected expected.json --actual actual.json

# 3. If verification fails, generate divergence report
node divergence_reporter.mjs --expected expected.json --actual actual.json \
  --output divergence.json

# 4. Review repair suggestions
cat divergence.json | jq '.repair_suggestions[] | select(.priority=="critical")'
```

## API Reference

### `analyzeDivergence(expected, actual)`

Analyzes divergence between expected and actual execution.

**Parameters:**
- `expected` (Object): Expected execution state with receipts
- `actual` (Object): Actual execution state with receipts

**Returns:** Divergence report object

### `generateDivergenceReport(expected, actual, outputPath)`

Generates and saves divergence report to file.

**Parameters:**
- `expected` (Object): Expected execution state
- `actual` (Object): Actual execution state
- `outputPath` (string): Path to save JSON report (default: './divergence.json')

**Returns:** Promise<Object> - Generated report

### `printDivergenceReport(report)`

Prints formatted divergence report to console.

**Parameters:**
- `report` (Object): Divergence report to print

**Returns:** void

## Implementation Details

### Divergence Detection Algorithm

1. **Length Check**: Compare receipt chain lengths
2. **Step-by-Step Comparison**: Iterate through receipts sequentially
3. **Multi-Level Validation**:
   - Step index match
   - Operation type match
   - Hash chain integrity
   - Frontier state comparison
   - Budget violation check
4. **First Divergence**: Return immediately upon finding first mismatch

### Comparison Strategies

**Frontier Comparison:**
- Size check (number of terms)
- Hash check (frontier_hash)
- Term-by-term comparison

**Effect Comparison:**
- Count check
- Type matching
- Data matching
- Result validation

**Budget Comparison:**
- Steps consumed
- Memory used
- Time elapsed
- Exceeded flag

### Performance Characteristics

- **Time Complexity:** O(n) where n = min(expected, actual) receipt count
- **Space Complexity:** O(1) for analysis (streaming comparison)
- **Typical Analysis Time:** <200ms for 1000+ receipts

## Validation Schema

The output conforms to `divergence.schema.json.tera`:

```bash
# Validate generated report
ajv validate -s divergence.schema.json -d divergence.json
```

## Testing

### Unit Tests

Test individual comparison functions:
- `findDivergencePoint()`
- `compareFrontiers()`
- `compareEffects()`
- `compareBudgets()`

### Integration Tests

Test full analysis pipeline:
- No divergence (matching execution)
- Frontier divergence
- Effect mismatch
- Budget violation
- Receipt chain break

### Property Tests

Verify invariants:
- Analysis always completes
- First divergence is truly first
- Severity matches divergence type
- All suggestions are actionable

## Performance Tuning

### Large Receipt Chains

For chains >10,000 receipts:
- Use streaming comparison
- Implement early termination
- Cache frontier hashes

### Memory Optimization

- Don't load entire chains into memory
- Process receipts incrementally
- Stream results to file

## Troubleshooting

### "No divergence detected" but verification failed

**Cause:** Comparison may be too coarse-grained.

**Solution:** Increase comparison granularity, check floating-point precision.

### Incorrect divergence point identified

**Cause:** Multiple simultaneous divergences mask root cause.

**Solution:** Analyze causality chain, review previous steps.

### Repair suggestions not actionable

**Cause:** Diagnosis lacks context or specificity.

**Solution:** Enhance diagnosis with more state information, add domain knowledge.

## Future Enhancements

- [ ] Machine learning for root cause classification
- [ ] Automated repair script generation
- [ ] Diff visualization (expected vs actual)
- [ ] Historical divergence pattern analysis
- [ ] Integration with CI/CD pipelines
- [ ] Real-time divergence alerting

## References

- **Receipt Schema:** `receipt.schema.json.tera`
- **Replay Pack Schema:** `replay_pack.schema.json.tera`
- **Divergence Schema:** `divergence.schema.json.tera`
- **Verification Script:** `verify_receipts.mjs.tera`

## License

Generated by ggen from ln_ctrl wizard.
Formula: A = μ(O) - Code precipitates from RDF ontologies.

---

**Version:** 1.0.0
**Last Updated:** 2026-02-11
**Maintainer:** ggen ln_ctrl wizard
