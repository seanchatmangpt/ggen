# Divergence Reporter - Quick Start Guide

## 5-Minute Getting Started

### 1. Basic Usage (CLI)

```bash
# Run divergence analysis
node divergence_reporter_cli.mjs \
  --expected expected.json \
  --actual actual.json \
  --output divergence.json

# Exit codes:
# 0 = No divergence (verification passed)
# 1 = Divergence detected (see report)
# 2 = Error (invalid input)
```

### 2. Programmatic Usage

```javascript
import { analyzeDivergence, printDivergenceReport } from './divergence_reporter.mjs';

// Load your execution states
const expected = { workflow_id: "...", receipts: [...] };
const actual = { workflow_id: "...", receipts: [...] };

// Analyze
const report = analyzeDivergence(expected, actual);

// Print formatted report
printDivergenceReport(report);

// Check result
if (report.divergence_detected) {
  console.log(`Divergence at step ${report.divergence_point.step_index}`);
  console.log(`Fix: ${report.repair_suggestions[0].action}`);
}
```

### 3. Input Format

```javascript
{
  "workflow_id": "550e8400-e29b-41d4-a716-446655440000",
  "receipts": [
    {
      "step_index": 0,
      "operation": "reduce",
      "hash_chain": "a1b2c3...",
      "redex_executed": {
        "redex_type": "beta",
        "redex_expression": "(λx.x) 42"
      },
      "frontier_after": {
        "frontier_terms": ["42"],
        "frontier_size": 1,
        "frontier_hash": "d4e5f6..."
      },
      "effects_performed": [],
      "budget_remaining": {
        "budget_steps": 10000,
        "budget_memory": 100000000,
        "budget_time": 30000,
        "budget_exceeded": false
      }
    }
  ]
}
```

### 4. Output Format

```javascript
{
  "report_id": "uuid",
  "timestamp": "2026-02-11T21:45:00Z",
  "workflow_id": "uuid",
  "divergence_detected": true,
  "divergence_point": {
    "step_index": 42,
    "operation": "reduce"
  },
  "divergence_type": "frontier_difference",
  "diagnosis": "Frontier divergence at step 42...",
  "repair_suggestions": [
    {
      "priority": "critical",
      "action": "Verify normalizer configuration",
      "rationale": "Term '6' was expected but '5+1' was found",
      "command": "ggen validate --check-normalizer-config"
    }
  ],
  "severity": "high"
}
```

### 5. Common Scenarios

#### Scenario A: Frontier Divergence
**Problem:** Terms don't match
```
Expected: ["6", "42"]
Actual:   ["5+1", "42"]
```
**Fix:** Check normalizer configuration
```bash
ggen validate --check-normalizer-config
```

#### Scenario B: Hash Chain Mismatch
**Problem:** Cryptographic chain broken
```
Expected hash: abc123...
Actual hash:   abc124...
```
**Fix:** Check for non-deterministic operations
```bash
# Ensure RNG seeding
export RNG_SEED=42
```

#### Scenario C: Budget Violation
**Problem:** Resource limits exceeded
```
Expected: budget_exceeded = false
Actual:   budget_exceeded = true
```
**Fix:** Investigate infinite loop or performance regression

## Priority Levels

| Priority | Symbol | Meaning | Action Required |
|----------|--------|---------|-----------------|
| Critical | 🔴 | Stop immediately | Fix before proceeding |
| High | 🟡 | Action needed | Fix before deployment |
| Medium | 🟢 | Should address | Review and fix |
| Low | ⚪ | Monitor | Address in maintenance |

## Quick Diagnostics

### No Divergence
```
✅ No divergence detected
→ Verification passed
→ Expected and actual match
```

### Divergence Detected
```
🔴 DIVERGENCE DETECTED
Type: frontier_difference
Step: 42
→ Check repair suggestions
→ Start with critical priority
```

## Integration Examples

### Shell Script
```bash
#!/bin/bash
node execute.mjs --replay scenario.json > actual.json
if ! node verify.mjs --expected expected.json --actual actual.json; then
  node divergence_reporter_cli.mjs \
    --expected expected.json \
    --actual actual.json
  exit 1
fi
```

### CI/CD (GitHub Actions)
```yaml
- name: Verify Execution
  run: |
    node divergence_reporter_cli.mjs \
      --expected expected.json \
      --actual actual.json || exit 1
```

### Makefile
```makefile
verify:
	@node divergence_reporter_cli.mjs \
		--expected expected.json \
		--actual actual.json \
		--quiet && echo "✅ PASS" || echo "❌ FAIL"
```

## Testing

```bash
# Run tests
node test_divergence_reporter.mjs

# Expected output:
# ✅ All divergence reporter tests passed!
```

## Common Commands

```bash
# Full analysis with report
node divergence_reporter_cli.mjs \
  --expected expected.json \
  --actual actual.json

# Quiet mode (CI/CD)
node divergence_reporter_cli.mjs \
  --expected expected.json \
  --actual actual.json \
  --quiet

# Custom output path
node divergence_reporter_cli.mjs \
  --expected expected.json \
  --actual actual.json \
  --output custom-report.json

# Show help
node divergence_reporter_cli.mjs --help
```

## API Quick Reference

```javascript
// Main functions
analyzeDivergence(expected, actual)           // Returns report object
generateDivergenceReport(exp, act, path)      // Saves to file
printDivergenceReport(report)                 // Console output

// Enums
DivergenceType.FRONTIER                       // frontier_difference
DivergenceType.RECEIPT_CHAIN                  // receipt_chain_divergence
DivergenceType.BUDGET                         // budget_violation

Severity.CRITICAL                             // critical
Severity.HIGH                                 // high
Severity.MEDIUM                               // medium
Severity.LOW                                  // low

Priority.CRITICAL                             // critical
Priority.HIGH                                 // high
Priority.MEDIUM                               // medium
Priority.LOW                                  // low
```

## Troubleshooting

### Error: File not found
```bash
# Check file paths
ls -la expected.json actual.json
```

### Error: Invalid JSON
```bash
# Validate JSON syntax
node -e "JSON.parse(require('fs').readFileSync('expected.json'))"
```

### Error: Module not found
```bash
# Check file is in correct directory
ls -la divergence_reporter.mjs
```

## Next Steps

1. Read full documentation: `DIVERGENCE_REPORTER_README.md`
2. Review implementation: `DIVERGENCE_IMPLEMENTATION_SUMMARY.md`
3. Check schema: `schemas/divergence.schema.json.tera`
4. Run tests: `node test_divergence_reporter.mjs`

---

**Quick Reference Card v1.0.0**
**Generated by ggen ln_ctrl wizard**
**Date: 2026-02-11**
