# Wave 2 Phase 1 — Quick Start Guide

**Read This First:** 2 minutes  
**Full Brief:** [WAVE2_PHASE1_KICKOFF_BRIEF.md](./WAVE2_PHASE1_KICKOFF_BRIEF.md) (395 lines, 16KB)

---

## What You're Doing

Fixing **4 critical P0 blockers** that cause wrong behavior or data loss. These are not feature requests — they break correctness.

| Blocker | Impact | Hours | Priority |
|---------|--------|-------|----------|
| **P0-01 SHACL** | Validation no-op (shapes ignored) | 14h | ⚠️ CRITICAL |
| **P0-02 Pipeline** | Wrong stages, receipts incomplete | 7h | 🟡 MEDIUM |
| **P0-03 Namespace** | SPARQL silent data loss | 5h | 🔴 HIGHEST |
| **P0-04 Error** | Error context lost | 3.5h | 🟢 LOW |

**Total: 29.5 hours** over 5 business days

---

## Your Task (Pick One)

1. **P0-01 SHACL Engineer** → Read "P0-01: SHACL Validation" section
2. **P0-03 Namespace Engineer** → Read "P0-03: Namespace Conflicts" section
3. **P0-02 Pipeline Engineer** → Read "P0-02: Pipeline Architecture" section
4. **P0-04 Error Engineer** → Read "P0-04: Error Type Chaos" section
5. **Test Harness Lead** → Read "Test Harness References" + "Success Criteria" sections

---

## What Success Looks Like

For your blocker:
1. ✅ Implementation written (4-6 hours)
2. ✅ Chicago TDD tests pass (≥80% coverage)
3. ✅ OTEL spans captured and verified
4. ✅ `cargo make check` + `cargo make test` green
5. ✅ Committed with evidence (OTEL output in commit message)

---

## Checklist for Your Blocker

### Before You Start
- [ ] Read full brief: [WAVE2_PHASE1_KICKOFF_BRIEF.md](./WAVE2_PHASE1_KICKOFF_BRIEF.md)
- [ ] Read testing rules: [TESTING.md](./TESTING.md)
- [ ] Read Chicago TDD forbidden patterns: [testing-forbidden.md](./../.claude/rules/rust/testing-forbidden.md)
- [ ] Read OTEL validation: [otel-validation.md](./../.claude/rules/otel-validation.md)

### While Implementing
- [ ] Create test file first (RED)
- [ ] Implement fix (GREEN)
- [ ] Verify OTEL spans: `RUST_LOG=trace cargo test -- <blocker>`
- [ ] Write negative-path test (sabotage test)
- [ ] Run full build: `cargo make pre-commit`

### Before Committing
- [ ] Coverage ≥80% for your blocker
- [ ] All tests pass
- [ ] OTEL spans in output
- [ ] No compiler warnings
- [ ] Commit message includes OTEL evidence

---

## OTEL Span Checklist

For each blocker, verify these spans appear in output:

**P0-01 SHACL:**
```
quality_gate.validate
  gate.name="shacl"
  gate.result="pass" or "fail"
```

**P0-02 Pipeline:**
```
pipeline.load
pipeline.extract
pipeline.generate
pipeline.validate
pipeline.emit
  (each with pipeline.stage and pipeline.duration_ms)
```

**P0-03 Namespace:**
```
sparql.query
  namespace.count=1  (not 3)
  sparql.result_count>0  (not silent 0)
```

**P0-04 Error:**
```
error.*
  error.type="..."
  error.message="..."
  error.code="..."
```

Run this to verify:
```bash
export RUST_LOG=trace,ggen_core=trace,ggen_marketplace=trace,ggen_cli=trace
cargo test -- <your_blocker_name> 2>&1 | grep -E "(quality_gate|pipeline|sparql|error)"
```

---

## Red Flags (Andon Signals)

STOP immediately if you see:
1. `error[E...]` — compiler error
2. `FAILED` — test failure
3. `warning:` — clippy warning
4. No OTEL spans in log output

Fix these before proceeding. Do not commit with signals.

---

## FAQ

**Q: Do I write the test first or the implementation?**  
A: Test first (RED), then implement (GREEN). Chicago TDD mandatory.

**Q: Can I use mocks or test doubles?**  
A: No. Mocks forbidden. Use real collaborators (real files, real databases, real SPARQL queries).

**Q: What if my OTEL spans don't appear?**  
A: Check `RUST_LOG=trace`. If still missing, the feature didn't call the external service. Investigate.

**Q: How do I know if my coverage is 80%?**  
A: Run `cargo tarpaulin --out Html` and check the HTML report.

**Q: What's a "sabotage test"?**  
A: Inject impossible state (e.g., delete shape file, violate constraint, query with wrong namespace) and verify the system fails correctly, not silently.

---

## Key Documents

- **Full Brief:** [WAVE2_PHASE1_KICKOFF_BRIEF.md](./WAVE2_PHASE1_KICKOFF_BRIEF.md)
- **Audit Context:** [crate-audits/AUDIT_DASHBOARD.md](./crate-audits/AUDIT_DASHBOARD.md)
- **Testing Rules:** [TESTING.md](./TESTING.md)
- **Chicago TDD Rules:** [../.claude/rules/rust/testing.md](./../.claude/rules/rust/testing.md)
- **Forbidden Patterns:** [../.claude/rules/rust/testing-forbidden.md](./../.claude/rules/rust/testing-forbidden.md)
- **OTEL Validation:** [../.claude/rules/otel-validation.md](./../.claude/rules/otel-validation.md)

---

**Status:** Phase 1 READY FOR KICKOFF  
**Next Step:** Pick your blocker and read the corresponding section in the full brief
