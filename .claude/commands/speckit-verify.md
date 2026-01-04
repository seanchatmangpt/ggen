---
description: Verify specification closure before implementation
---

# /speckit-verify

Validate that specification is sufficiently closed before beginning implementation.

## Usage

```bash
/speckit-verify [feature-number]
```

## Parameters

- `[feature-number]` (optional): Feature spec to verify (e.g., "004" for .specify/specs/004-test-audit/). If omitted, checks current feature branch.

## What It Does

Runs the `bb80-specification-validator` agent to:

1. **Verify specification closure** using formal checklist
2. **Identify incomplete areas** (specific, actionable)
3. **Report blocking issues** preventing EPIC 9 fan-out
4. **Recommend clarifications** for closure

## Output

Returns structured pass/fail verdict with:
- Closure score (0-100%)
- List of incomplete areas
- Recommendations for clarification
- Ready-for-implementation flag

## Example

```bash
/speckit-verify 004

Output:
═══════════════════════════════════════════════════════════
SPECIFICATION CLOSURE REPORT: 004-test-audit
═══════════════════════════════════════════════════════════

Status: INCOMPLETE ⚠️
Closure Score: 72%

✓ Complete Areas:
  - Input characterization (test crates defined)
  - Output specification (metrics defined)
  - Acceptance criteria (3 scenarios)

✗ Incomplete Areas:
  1. Non-functional requirements
     Issue: Performance targets for mutation testing not specified
     Recommendation: Add SLO - "Mutation run <10 min for 1000 LOC"

  2. Error handling
     Issue: What happens if test file is invalid?
     Recommendation: Document error scenarios in feature.ttl

  3. Scope boundaries
     Issue: Do we support custom test frameworks or pytest only?
     Recommendation: Decide in feature.ttl "supported_frameworks: [pytest]"

═══════════════════════════════════════════════════════════
Ready for implementation: FALSE

Next steps:
1. Resolve the 3 incomplete areas above
2. Run `/speckit-verify 004` again
3. When score = 100%: proceed to EPIC 9 fan-out
```

## When to Use

✅ **Use before EPIC 9**: Always run before fan-out
✅ **Use during clarification**: Structure discussion with checklist
✅ **Use to gate implementation**: Block work until closed

❌ **Don't skip**: Incomplete spec → iteration → defects

## Integration

```
User: "Start feature 004"
  ↓
/speckit-verify 004
  ↓ Incomplete: 72%
Request clarifications
  ↓
User: [clarifies in .specify/specs/004-test-audit/feature.ttl]
  ↓
/speckit-verify 004
  ↓ Complete: 100% ✓
Proceed to /bb80-parallel [full specification]
  ↓
EPIC 9 atomic cycle begins
```

## Technical Details

- Reads `.specify/specs/[feature]/feature.ttl`
- Validates against SHACL constraints
- Checks for vague terms (TBD, TODO, maybe, etc.)
- Performs SPARQL queries for relationship completeness
- Returns deterministic pass/fail (no human judgment)

## See Also

- `/bb80-parallel` - After verification, start EPIC 9 cycle
- `bb80-specification-closure` skill - Learn closure patterns
- `.specify/specs/` - Specification files to verify
