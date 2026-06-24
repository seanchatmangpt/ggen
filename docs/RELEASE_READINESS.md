# ggen Release Readiness — 10-Dimension Definition of Done

**Version:** 26.6.23 | **Last Updated:** 2026-06-23 | **Status:** NORMATIVE

> **GOLDEN RULE:** All 10 dimensions must pass before shipping. No partial success. One failure = do not deploy.

---

## Quick Reference Checklist

```bash
# 1. Gate Validation (5 core gates)
just timeout-check && just check && just lint && just test && just slo-check

# 2. Artifact Emission (code generation pipeline)
ggen sync --audit true && find . -name "*.rs" -newer .ggen/receipts/latest.json | wc -l

# 3. Checksum Validation
ggen receipt verify .ggen/receipts/latest.json --check-hashes

# 4. Signature Verification
ggen receipt verify .ggen/receipts/latest.json --public-key .ggen/keys/verifying.key

# 5. Lockfile Finality
jq -e '.packages[] | select(.digest | length == 64)' .ggen/packs.lock && echo "✅ Lockfile valid"

# 6. Version Tagging
grep 'version =' Cargo.toml | head -1 && git tag -l v$(grep 'version =' Cargo.toml | head -1 | cut -d '=' -f2 | tr -d '\"' | xargs)

# 7. Changelog Recording
head -5 CHANGELOG.md && grep -A 2 '## \[' CHANGELOG.md | head -3

# 8. Smoke Testing
cargo build --release && cargo test --release --lib

# 9. Deployment Checklist
git status --short && git branch -vv && git log --oneline -1

# 10. Rollback Plan Documented
ls -la ROLLBACK.md RUNBOOK.md 2>/dev/null || echo "⚠️  Rollback documentation missing"
```

---

## The 10 Dimensions (In Order)

### 1️⃣ **Gate Validation** — All 5 Core Quality Gates Pass

| Gate | Timeout | Purpose |
|------|---------|---------|
| `timeout-check` | 5s | Verify `timeout` command available |
| `check` | 60s | Compilation: all 15 crates compile |
| `lint` | 90s | Code quality: zero clippy warnings, formatted |
| `test` | 120s | Functional: all 347+ tests pass (Chicago TDD) |
| `slo-check` | 120s | Performance: all SLOs met |

**Success:** `just timeout-check && just check && just lint && just test && just slo-check` exits 0  
**Failure:** STOP. Do NOT proceed. Fix the gate and re-run.

---

### 2️⃣ **Artifact Emission** — Code Generation Pipeline Produces Output

**What it proves:** `ggen sync` actually ran and created files (not just exited 0).

**Validation:**
```bash
ggen sync --audit true
ls -la $(grep -r 'output_file' ggen.toml | cut -d '=' -f2 | tr -d '"')
wc -l $(find . -name '*.rs' -newer .ggen/receipts/latest.json)
```

**Success:** Output files exist, have timestamps >= sync start time, contain real content  
**Failure:** No files created, or files empty, or timestamps stale → contract drift, do NOT deploy

---

### 3️⃣ **Checksum Validation** — SHA-256 Hashes Verified

**What it proves:** Generated artifacts match hashes stored in receipt (no tampering or corruption).

**Validation:**
```bash
ggen receipt verify .ggen/receipts/latest.json --check-hashes
sha256sum --check <(jq -r '.output_hashes | to_entries[] | .value + "  " + .key' .ggen/receipts/latest.json)
```

**Success:** All hashes match (exit 0)  
**Failure:** Hash mismatch → artifact corrupted or modified, restore from backup

---

### 4️⃣ **Signature Verification** — Receipt Cryptographically Signed

**What it proves:** Receipt came from authoritative build system (not forged).

**Validation:**
```bash
ggen receipt verify .ggen/receipts/latest.json --public-key .ggen/keys/verifying.key
```

**Success:** Signature valid (exit 0)  
**Failure:** Signature invalid or missing → receipt tampered, audit trail broken, do NOT deploy

---

### 5️⃣ **Lockfile Finality** — Package Lock Frozen & Verified

**What it proves:** Same packs (versions, digests) used as in testing; deployment is reproducible.

**Validation:**
```bash
jq -e '.packages[] | select(.digest | length != 64 or (.digest | test("[^0-9a-f]")))' .ggen/packs.lock && echo "❌ Invalid digest" || echo "✅ Lockfile valid"
ggen sync --locked --dry_run true
```

**Success:** All packages have valid SHA-256 digests, timestamps are recent  
**Failure:** Empty digest, stale timestamp, or lockfile missing → reinstall packs

---

### 6️⃣ **Version Tagging** — Semantic Versioning Tagged in Git

**What it proves:** Release is tied to specific git commit; enables rollback.

**Validation:**
```bash
VERSION=$(grep 'version =' Cargo.toml | head -1 | cut -d '=' -f2 | tr -d '"')
git tag -l v$VERSION && git show v$VERSION --format=fuller
```

**Success:** git tag v<version> exists, annotated, signed with GPG, points to current commit  
**Failure:** Tag missing or unsigned → rollback path compromised, create tag before deploying

---

### 7️⃣ **Changelog Recording** — Changes Documented

**What it proves:** Users can see what changed between versions; breaking changes are listed.

**Validation:**
```bash
head -10 CHANGELOG.md
grep -E '## \[26\.6\.11\]|### Breaking Changes|### Features|### Fixes' CHANGELOG.md
```

**Success:** CHANGELOG.md has entry for current version with sections  
**Failure:** No entry or wrong version → add entry before releasing

---

### 8️⃣ **Smoke Testing** — End-to-End Validation

**What it proves:** Generated code compiles, tests pass, runs without errors.

**Validation:**
```bash
cargo build --release
cargo test --release --lib
cargo bench --bench cli_startup_performance -- --test
```

**Success:** All builds pass, tests pass, benchmarks meet SLOs  
**Failure:** Compile error, test failure, or SLO exceeded → fix and re-run smoke tests

---

### 9️⃣ **Deployment Checklist** — Pre-Deployment Safety Gate

**What it proves:** Working tree is clean, branch is correct, CI is green, rollback plan exists.

**Validation:**
```bash
git status --short                          # must be empty (no uncommitted changes)
git branch -vv | grep main                  # must show "up to date with origin/main"
git log --oneline -1 | head -1              # shows current commit
ls -la ROLLBACK.md                          # rollback procedure documented
```

**Success:** Working tree clean, on main, all commits pushed, CHANGELOG updated, ROLLBACK.md exists  
**Failure:** Any item fails → fix before deploying

---

### 🔟 **Rollback Plan** — Recovery Documented & Tested

**What it proves:** If deployment fails, we can revert to prior working state in <5 min.

**Validation:**
```bash
cat ROLLBACK.md | grep -E "git checkout|ggen sync --locked"
git tag -l | tail -2  # show prior two releases
ls -la .ggen/receipts/ | grep -E "\.json$" | wc -l  # at least 3 prior receipts
```

**Success:** ROLLBACK.md documents prior git tag, ggen sync --locked command, tested in staging  
**Failure:** No documentation or untested → document and test in staging before production

---

## Validation Sequence

```
START
  ↓
1. Gate Validation (all 5 gates pass) ────→ FAIL: FIX & RETRY
  ↓
2. Artifact Emission (output files exist) ─→ FAIL: RE-RUN ggen sync
  ↓
3. Checksum Validation (hashes verified) ──→ FAIL: RESTORE FROM BACKUP
  ↓
4. Signature Verification (signed) ───────→ FAIL: AUDIT BREACH, DO NOT DEPLOY
  ↓
5. Lockfile Finality (frozen & verified) ─→ FAIL: REINSTALL PACKS
  ↓
6. Version Tagging (semver tagged) ───────→ FAIL: CREATE TAG
  ↓
7. Changelog Recording (changes documented) ─→ FAIL: ADD CHANGELOG ENTRY
  ↓
8. Smoke Testing (end-to-end passes) ─────→ FAIL: FIX CODE
  ↓
9. Deployment Checklist (final checks) ───→ FAIL: RESOLVE ITEM
  ↓
10. Rollback Plan (documented & tested) ──→ FAIL: DOCUMENT & TEST IN STAGING
  ↓
READY TO SHIP ✅
```

---

## Andon Protocol (Stop the Line)

When a dimension fails:

1. **STOP** immediately. Do not proceed.
2. **READ** the failure message carefully.
3. **IDENTIFY** root cause (not symptom).
4. **FIX** the issue (code, config, docs, etc.).
5. **RETRY** that dimension.
6. **ITERATE** steps 2-5 until dimension passes.
7. Only when ALL dimensions pass, proceed to deployment.

**Forbidden responses:**
- ❌ "I'll deploy anyway and monitor"
- ❌ "This gate is probably wrong"
- ❌ "Let me suppress this warning"
- ❌ "This can be fixed after deployment"
- ❌ "Nobody looks at receipts anyway"

---

## Failure Modes to Avoid

### Decorative Completion
❌ Command exits 0 but no durable state changed
- ggen sync exits 0 but no output files created (dimension 2)
- Receipt created with empty signature field (dimension 4)
- .ggen/packs.lock has empty digest (dimension 5)

### Contract Drift
❌ Proof objects (receipt, lockfile) don't accurately describe what ran
- input_hashes stale from prior run (dimension 3)
- installed_at is test fixture '2000-01-01T00:00:00Z' (dimension 5)
- operation_id is hardcoded UUID (dimension 4)

### Fail-Open Behavior
❌ System continues when it should halt
- Missing pack emits warning instead of error (dimension 2)
- ggen receipt verify returns true with empty signature (dimension 4)
- Lockfile mismatch ignored during sync --locked (dimension 5)

### Legacy Path Contamination
❌ Old code path still reachable alongside new authoritative path
- load_packs_legacy() still called when lockfile absent (dimension 5)
- write_raw_receipt() still exists and called from production code (dimension 4)

### Epistemic Bypass
❌ Logic hardcoded instead of derived from RDF/SPARQL/templates
- Output file paths hardcoded instead of from CONSTRUCT query (dimension 2)
- Pack names matched with inline `match` arm instead of PackRegistry (dimension 5)

---

## Evidence Required (What Must Be Captured)

| Dimension | Evidence |
|-----------|----------|
| 1 — Gates | Exit code 0 from each gate; test count from output |
| 2 — Artifacts | Output files on disk with correct timestamps and size |
| 3 — Checksums | sha256sum output matches receipt output_hashes |
| 4 — Signature | ggen receipt verify exit 0; GPG signature present |
| 5 — Lockfile | .ggen/packs.lock JSON with all digests (64 hex chars) |
| 6 — Version | git tag -l output, git show output (annotated, signed) |
| 7 — Changelog | CHANGELOG.md entry with version, date, sections |
| 8 — Smoke Tests | cargo build, cargo test exit 0; bench metrics captured |
| 9 — Checklist | git status clean, git branch on main, ROLLBACK.md exists |
| 10 — Rollback | ROLLBACK.md with prior tag, command, tested result |

---

## False Positives (Not Ready to Ship)

- ✗ "Code compiles" (dimension 1 only)
- ✗ "Tests pass" (dimensions 1 + 8 only)
- ✗ "CI is green" (dimensions 1-3 only)
- ✗ "Receipt exists" (dimension 4 only)
- ✗ "Version is tagged" (dimension 6 only)

**Only when ALL 10 dimensions pass:** READY TO SHIP ✅

---

## References

- **Machine-readable spec:** `docs/DEFINITION_OF_DONE_RELEASE.json`
- **Quality gates:** `docs/DEFINITION_OF_DONE.md`
- **Mistake classes:** `.claude/rules/coding-agent-mistakes.md`
- **Andon protocol:** `.claude/rules/andon/signals.md`
- **Architecture:** `docs/architecture/COMPRESSED_REFERENCE.md`

---

**Last Updated:** 2026-06-23 | **Project:** ggen v26.6.23 | **Repository:** https://github.com/seanchatmangpt/ggen
