# 03 — WvdA Test 6: Docstring Claims a Mechanism the Test Does Not Implement

Part of [00-OVERVIEW](00-OVERVIEW.md). Small in lines, but it is a constitution violation
(fabricated evidence in prose), so it blocks committing its file in
[01-COMMIT-BOUNDARY](01-COMMIT-BOUNDARY.md) group 3.

## Finding (adversarial verifier, round 2, 2026-08-16)

`/Users/sac/chatmangpt/ostar/tests/process/test_wvda_agent_placeholders.py` was rewritten
during the sweep to real-collaborator Chicago style. The verifier confirmed **5 of 6 tests
are genuinely real** and reproduce as claimed. Test 6,
`test_forward_against_unreachable_tempo_returns_invalid_with_reason`, does not:

- The module docstring and the test's own docstring claim it exercises "the real
  TempoClient HTTP path" via "a real socket connection attempt to a port bound-then-closed
  on 127.0.0.1".
- The verifier could not reproduce that mechanism from the test body — the claimed
  bind-then-close socket choreography is not what the code does.

A test whose prose asserts evidence its body does not produce is the same defect class as
a hardcoded success banner: the reader is told a boundary was crossed when it was not.

## Fix (either direction is acceptable; pick one)

1. **Make the claim true**: actually bind an ephemeral port on 127.0.0.1, close it, point
   a real `TempoClient` at it, and assert on the real returned invalid/reason state. This
   is cheap, deterministic, and crosses the real HTTP-client boundary (connection refused
   is a real outcome of a real socket attempt).
2. **Make the prose true**: if the test's actual mechanism is legitimate on its own terms,
   rewrite both docstrings to describe exactly what the body does — no more.

Do not delete the test. Also re-check the two Pyright hits in the same file
(`start_time`/`end_time` typed `str` receiving `None` at lines 79-80 and 106-107) —
either the annotation or the call is wrong; fix whichever is.

## Acceptance

- Docstrings describe only what the body performs, verified by reading the diff.
- `.venv/bin/python -m pytest tests/process/test_wvda_agent_placeholders.py -v` — real
  output pasted, all collected tests pass or fail honestly (no skips without a stated
  infeasibility reason per the one-legitimate-double rule).
- `grep -nE "unittest.mock|@patch|MagicMock|monkeypatch" <file>` → zero hits.

## See Also

- [01-COMMIT-BOUNDARY](01-COMMIT-BOUNDARY.md) — this file commits after this ticket closes
- `/Users/sac/chatmangpt/ostar/.claude/rules/testing-anti-cheating.md` — the evidence
  doctrine this test violated in prose form
