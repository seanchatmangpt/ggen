# Reference: Lean Vocabulary

**Mura, Muda, Muri, Kaizen, Gemba, Andon definitions with software examples**

---

## The 3M Framework

### Mura (斑) - Inconsistency

**Japanese:** 斑 (むら)
**Pronunciation:** moo-rah
**Definition:** Unevenness, irregularity, inconsistency in processes.

**In Manufacturing:** Uneven production pace (100 units/hour, then 20, then 150)
**In Software:** Inconsistent code patterns, mixed naming conventions

**Example:**
```rust
// Mura: Inconsistent naming
fn getUserData() { }      // camelCase
fn process_order() { }    // snake_case
fn FetchConfig() { }      // PascalCase
```

**Elimination:** Standardize on snake_case for Rust functions.

---

### Muda (無駄) - Waste

**Japanese:** 無駄 (むだ)
**Pronunciation:** moo-dah
**Definition:** Waste, non-value-adding activity.

**7 Types of Muda:**
1. **Overproduction:** Making more than needed
2. **Waiting:** Idle time
3. **Transportation:** Moving items unnecessarily
4. **Overprocessing:** Doing more work than required
5. **Inventory:** Excess stock
6. **Motion:** Unnecessary movement
7. **Defects:** Rework due to errors

**In Software:**
- Duplicate tests (Overproduction)
- Slow tests with sleeps (Waiting)
- File I/O in unit tests (Transportation)
- Testing trivial getters (Overprocessing)
- Dead code (Inventory)
- Context switching (Motion)
- Flaky tests (Defects)

---

### Muri (無理) - Overburden

**Japanese:** 無理 (むり)
**Pronunciation:** moo-ree
**Definition:** Overburdening people or equipment beyond capacity.

**In Manufacturing:** Machine running above rated speed, breaking frequently
**In Software:** 200-line test doing too much, impossible to debug

**Example:**
```rust
// Muri: Mega-test doing everything
#[test]
fn test_entire_application_lifecycle() {
    // 300 lines testing:
    // - User creation
    // - Authentication
    // - Profile updates
    // - Data export
    // - Account deletion
    // (Takes 30 seconds, hard to debug)
}
```

**Elimination:** Split into 10 focused tests, each <5 seconds.

---

## Continuous Improvement

### Kaizen (改善) - Continuous Improvement

**Japanese:** 改善 (かいぜん)
**Pronunciation:** ky-zen
**Definition:** Continuous, incremental improvement involving everyone.

**Philosophy:** Small, daily improvements compound over time.

**In Software:** Weekly code reviews, refactor 1 function/day, reduce CI time by 10s/week

**Example:**
```
Week 1: Refactor 5 tests (30 min saved)
Week 2: Remove 10 unused imports (5 min saved)
Week 3: Inline 3 test fixtures (15 min saved)
Week 4: Parallelize 2 slow tests (60 min saved)
────────────────────────────────────────
Total: 110 min/week saved (annualized: 95 hours)
```

**Key:** Everyone participates, not just managers.

---

## Observation & Alerting

### Gemba (現場) - The Real Place

**Japanese:** 現場 (げんば)
**Pronunciation:** gem-bah
**Definition:** Go to where work happens to observe reality, not rely on reports.

**In Manufacturing:** Manager walks factory floor to see actual process
**In Software:** Read actual test code, don't just trust CI metrics

**Example:**
```
❌ Remote: "Coverage is 85%, tests must be good"
✅ Gemba: Read 10 random tests, find no edge cases
```

**Gemba Walk:** Systematic inspection of work in progress.

---

### Andon (行灯) - Visual Alert

**Japanese:** 行灯 (あんどん)
**Pronunciation:** ahn-dohn
**Definition:** Visual management system to signal problems immediately.

**In Manufacturing:** Rope workers pull to stop assembly line when defects detected
**In Software:** CI alerts on test failures (Yellow: 1-5, Red: 6+)

**Example:**
```
🟢 Green Andon: 0 failures → Normal operations
🟡 Yellow Andon: 3 failures → Fix before next feature
🔴 Red Andon: 15 failures → STOP, team meeting
```

**Purpose:** Make problems visible instantly, prevent cascading failures.

---

### Poka-Yoke (ポカヨケ) - Mistake-Proofing

**Japanese:** ポカヨケ
**Pronunciation:** poh-kah yoh-keh
**Definition:** Error-proofing design so mistakes are impossible.

**Example:**
```rust
// ❌ Error possible (string typo)
export(&graph, "rdff");  // Runtime error

// ✅ Poka-yoke (enum, compile-time)
enum Format { Rdf, Owl }
export(&graph, Format::Rdf);  // Typo = compile error
```

**5 Patterns:** Guide pin, limit switch, fail-safe, counter, sequencing.

---

## Quality Standards

### Jidoka (自働化) - Autonomation

**Japanese:** 自働化 (じどうか)
**Pronunciation:** jee-doh-kah
**Definition:** Automation with human intelligence (machines detect defects).

**In Software:** Linters, formatters, type checkers catching errors automatically

**Example:**
```bash
# Jidoka: Automated quality gates
cargo clippy --deny warnings  # Auto-detect code smells
cargo fmt --check             # Auto-detect formatting issues
cargo test                    # Auto-detect regressions
```

---

### Kanban (看板) - Signboard

**Japanese:** 看板 (かんばん)
**Pronunciation:** kahn-bahn
**Definition:** Visual workflow management (cards moving across board).

**In Software:** GitHub Projects, Jira boards

**Columns:** To Do → In Progress → Review → Done

---

## Root Cause Analysis

### 5 Whys (五回のなぜ)

**Method:** Ask "why?" 5 times to find root cause.

**Example:**
```
Problem: Test suite takes 12 minutes
Why? 50 tests take >1s each
Why? Tests sleep for fixed durations
Why? Original author didn't know about timeouts
Why? No code review caught it
Why? Code review checklist doesn't include performance
→ Root cause: Missing performance criteria in reviews
```

**Fix:** Add "Test duration <100ms" to code review checklist.

---

## Quick Reference

| Term | Meaning | Software Example |
|------|---------|------------------|
| Mura | Inconsistency | Mixed coding styles |
| Muda | Waste | Duplicate tests |
| Muri | Overburden | 200-line mega-test |
| Kaizen | Continuous improvement | Weekly refactoring |
| Gemba | Go and see | Read actual code |
| Andon | Visual alert | CI failure notifications |
| Poka-Yoke | Mistake-proofing | Type system constraints |
| Jidoka | Autonomation | Linters, CI checks |

---

## Related Resources

- [Tutorial: Lean Manufacturing Intro](../tutorials/04-lean-manufacturing-intro.md)
- [How-to: Refactor Tests with Lean](../how-to/refactor-tests-with-lean.md)

---

**Last Updated:** 2025-11-18
