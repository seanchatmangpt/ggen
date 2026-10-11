# Cheat-Scan Triage — v26.10.10 (2026-10-10)

Lane: cheat-triage (read-only). HEAD `dcbf33333`, branch `main`.

## Current count (real run, 2026-10-10)

Command (from `justfile:680`, `guard-cheat-scan`):

```
cargo run --quiet -p ggen-cheat-scanner --bin ggen-cheat-scanner
```

Real output (run twice, 1172 then 1174 files — count jitter from transient build artifacts):

```
ALIVE: no cheat patterns detected across 1174 scanned file(s), 0 parse errors.
exit=0
```

**Current count: 0. The "~464 findings" premise of TECH-DEBT-001 is stale.**

## History (from docs/jira/2026-07-17-JTBD-VERIFICATION-DISCOVERED-BUGS.md, TECH-DEBT-001)

The 464 were measured 2026-07-17/18 and **retired 2026-07-20** on
`feat/cheat-scan-debt-retirement` (464 → 0, `guard-cheat-scan` green at 1152 files).
Class breakdown at the time:

| Class | Count | Disposition |
|---|---|---|
| CHEAT-T03 no-assertion-test | 456 | ~305 scanner false positives (fixed as precision improvements: `#[should_panic]`, `?`-returning tests, `assert_*` helpers now recognized); ~151 real debt fixed or deleted (real assertions added; 50 sham all-`Ok(())` tests deleted) |
| CHEAT-T01 vacuous-assert | 7 | `assert!(true)` in `chicago-tdd-tools` gate tests — fixed |
| CHEAT-T04 mock-import | 1 | False positive on `FakeDataGenerator: Default` — fixed via detector precision |

Top crates then: `chicago-tdd-tools`, `ggen-cli/tests`, `bcinr-mfw-ir`, `bcinr-pddl`, plus root `tests/`.

## Session-introduced vs pre-existing

0% session-introduced, 0% remaining pre-existing. Everything was pre-existing and was
eliminated 2026-07-20; this session introduced nothing and found no regressions.

## Recommended elimination order

None needed — backlog is empty. Recommendation: update TECH-DEBT-001's standing reference in
`.claude/rules/README.md`/CLAUDE.md contexts ("~464 pre-existing findings" is stale by ~3
months and two full refactors of the count's basis). `guard-cheat-scan` should stay in the
pre-commit gate list; its precision fixtures (`crates/ggen-cheat-scanner/tests/`) are the
anti-vacuity court for future regressions.

## Verification

- Recipe located: `justfile:680` (`guard-cheat-scan`), wired into `pre-commit` at `justfile:522`.
- Scan roots confirmed in `crates/ggen-cheat-scanner/src/main.rs` (`crates/*/{src,tests}`,
  root `tests/`, `src/`, `examples/`, `tools/`, `benches/`).
- 10-finding sample not possible: zero findings exist to sample.
