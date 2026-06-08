---
paths:
  - "tests/**"
---
# Testing Rules

## Test Runner

Always use `.venv/bin/pytest` (system python3 has no pytest).

## Test Organization

- Tests in `tests/` (unit/integration) or co-located in `src/` (module-internal)
- Follow `test_*.py` naming convention
- Shared fixtures in `tests/*/conftest.py`

## Pytest Patterns

- **Clear stale cache after fixing tests:** `pytest --cache-clear` — avoids re-running old failures via `--lf`
- **Skip integration tests with `@pytest.mark.skipif`** for modules requiring pm4py, SpiffWorkflow, or GROQ_API_KEY
- **Mock at the source, not the consumer:** `@patch("ostar.lm.configure_groq")`, not `@patch("ostar.workflow.agent.configure_groq")`

## Mocking Pitfalls

- **`sys.version_info`:** Never `@patch("module.sys")` — replaces the module with a MagicMock that can't compare. Use `@patch.object(sys, "version_info")` and set `.major`/`.minor` on the mock.
- **`@patch("ostar.lm.configure_groq")`** works because `configure_groq` is exported from `ostar.lm.__init__`. But if the code does `from ostar.lm import configure_groq` inside `__init__`, the patch still works because it patches at the source module.
- **DSPy ChainOfThought:** When a class does `self.method = dspy.ChainOfThought(...)`, this shadows the original method. Use `MethodType` to restore it in tests.

## Coverage

- Minimum 50% coverage enforced by CI
- Run with `poe test` or `pytest tests/ -v --cov=ostar --cov-report=term-missing`

## Test Quality Principle (Replaces Tier-Based Organization)

**Warning:** Do NOT use test naming/categorization as a substitute for enforcing behavior. Naming creates a loophole.

**Better approach: Classify tests by what they prove, not what they're called.**

### Tests Must Prove One of These (or test is low-value):

1. **Boundary Crossing** — Real LLM call, MCP invocation, OTel emission, or process transition
2. **Causality Chain** — X caused Y (traced through spans, receipts, or state)
3. **Falsifiability** — This test fails if core behavior is broken or faked
4. **Externalizable Evidence** — Output is in OTel, files, DB, or logs (not in-memory)
5. **Anti-Cheating** — Faking this test is harder than real execution

**DELETE tests that:**
- Only assert internal state without evidence
- Pass with mocks (no real boundary)
- Cannot catch fraud or drift
- Are decorative (document code, don't validate behavior)

**KEEP tests that:**
- Require real boundary-crossing
- Produce externalizable evidence
- Prove causality, not assertions
- Are resilient to agent cheating
- Would fail if code lies
