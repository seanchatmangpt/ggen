---
description: "Run comprehensive test audit using ggen-test-audit. Analyzes mutation testing, assertion coverage, and false positive detection. Use when improving test quality or verifying test strength."
allowed_tools: "Bash(cargo make:*), Bash(cargo test:*), Read, Grep"
argument_hint: "[crate-name] [audit-type]"
---

# Test Audit Command

Analyze test quality using mutation testing and assertion analysis.

## Supported Audit Types

- **mutations**: Run cargo-mutants to verify tests catch changes
- **assertions**: Measure assertion density and coverage
- **false-positives**: Detect tests passing when code broken
- **coverage**: Show assertion density by function
- **all**: Run all analyses (default)

## Workflow

1. **Select Target**
   - If $1 specified: audit crate/$1
   - Otherwise: audit entire workspace

2. **Run Mutation Tests** (`cargo make test`)
   - Executes all tests with timeouts enforced
   - Captures mutation survival rates
   - Identifies weak assertions

3. **Analyze Test Quality**
   - Measure assertions per function
   - Identify untested error paths
   - Find false-positive patterns

4. **Generate Report**
   - Save results to `.specify/specs/004-test-audit/evidence/`
   - Include recommendations for improvement
   - Track test quality metrics over time

## Chicago TDD Principles

Following chicago-tdd-tools 1.4.0 (state-based testing):
- Tests verify observable behavior (return values, state changes)
- Use real collaborators (not mocks, except in #[cfg(test)])
- AAA pattern: Arrange-Act-Assert

## Output

Test audit results include:
- Mutation survival rate by file
- Assertion density heatmap
- Recommended fixes for weak tests
- Performance impact summary

## Success Criteria

✓ Mutation score > 90% (surviving mutations < 10%)
✓ Assertion density > 1 assertion per function
✓ No false-positive patterns detected
✓ All tests pass with SLO timeouts
