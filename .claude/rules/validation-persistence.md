# Validation Persistence Rule

**CRITICAL**: When validation fails, keep working until it passes. Never give up.

## Required Behavior

When ANY validation check fails:
1. **Read the error message carefully** - understand what's blocking
2. **Fix the specific issue** - don't just report it, fix it
3. **Retry validation** - run the check again after fixing
4. **Iterate until success** - keep fixing and retrying
5. **Only report success** - when all checks pass

## Forbidden Behaviors

❌ "Validation failed, cannot proceed" — then stop working
❌ "Here are the errors" — then wait for user to fix them
❌ "Blocked by gate" — then give up
❌ Report failures without attempting fixes
❌ Skip validation checks
❌ Proceed despite failures

## Required Behaviors

✅ Read validation error → Identify root cause
✅ Identify root cause → Apply targeted fix
✅ Apply fix → Run validation again
✅ Validation fails → Analyze new errors
✅ Validation fails → Fix and retry again
✅ Validation passes → Report success and continue

## Example Workflow

```bash
# Step 1: Check gate
$ echo '{}' | bash .claude/hooks/stop_release_gate.sh
# Result: ETHOS_CONFORMANCE_FAILURE

# Step 2: Read error details
$ cat artifacts/ethos-validation/ethos-proof-*.json | jq '.results[] | select(.pass == false)'
# Result: Private namespace URNs detected

# Step 3: Fix the issue (migrate urn:ethos: to public URIs)
# Step 4: Re-run ETHOS validation
$ python scripts/ethos_validation.py
# Result: PASSED

# Step 5: Retry gate
$ echo '{}' | bash .claude/hooks/stop_release_gate.sh
# Result: stop_allowed: true
```

## Constitutional Principle

> **The easiest way to pass validation is to fix the issues.**
> **Keep working until validation passes. Do not stop until success.**

This is not a neverending loop — it's persistent iteration until the system is in a valid state. Each iteration should make measurable progress toward passing all checks.

## Gate Responses

The gate will tell you exactly what to fix:

```json
{
  "stop_allowed": false,
  "reason": "ethos_conformance_failed",
  "instruction": "Keep working until ETHOS validation passes. Retry after fixing."
}
```

Your response: Fix the ETHOS issues, then retry.

```json
{
  "stop_allowed": false,
  "missing": ["readme", "tests"],
  "instruction": "Fix missing items and retry. Do not proceed until validation passes."
}
```

Your response: Add the README and tests, then retry.

## Success Condition

Only when `stop_allowed: true` should you report completion and move to the next task. Until then: **keep working**.
