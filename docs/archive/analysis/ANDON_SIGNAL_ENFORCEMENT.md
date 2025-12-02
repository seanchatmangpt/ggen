# Andon Signal Enforcement in ggen

## The Pre-Push Hook Bug (FIXED v2.0)

### What Was the Bug?
Old pre-push hook line 109 skipped ENTIRE FILES if they contained `#[cfg(test)]` modules. This allowed 150+ `expect()` violations to slip through.

### The Fix
Check EACH `expect()` call individually for `#[allow(clippy::expect_used)]` attribute. No more file skipping.

### Impact
- Exposed 150+ pre-existing violations across 51 files in 12 crates
- Violations were hidden by the hook bug, not actually clean code
- Requires systematic remediation with proper `#[allow]` attributes
- Aligns with "Stop the Line" principle from Lean manufacturing

## Andon Signals

Andon signals enforce quality by "stopping the line" when issues appear:
- **CRITICAL**: Compiler errors, test failures
- **HIGH**: Linting errors, format violations
- **MEDIUM**: TODOs, unwrap/expect without allow

## Hook Gates

### Pre-Commit (6 gates)
1. Compilation check
2. Format check
3. Linting check
4. Unit tests
5. Security audit
6. Debug print detection

### Pre-Push (5 gates - v2.0)
1. Cargo check
2. Clippy (strict)
3. TODO/expect() validation (IMPROVED)
4. Formatting
5. Tests
6. Security audit

## Using --no-verify

**NEVER use `git push --no-verify` unless explicitly authorized.**

It defeats the entire purpose of the hook system and allows defects to propagate.

## Technical Debt

150+ pre-existing `expect()` violations need remediation:
- Add `#[allow(clippy::expect_used)]` to test functions
- Replace production `expect()` with proper error handling
- Commit systematically across 51 files

---

**Remember**: Andon signals exist to protect code quality. When they appear, fix the root cause immediately.
