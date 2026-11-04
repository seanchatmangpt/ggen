# DS-6 Receipt: sjira Control Plan (Detect)

Lane DS-6, DMEDI control-plan phase for `crates/ggen-sjira`. What DS-5 measures,
this lane makes mechanically detectable. Never ran git; landing is left to the
coordinator.

## DEFINE — drift classes and their detection rules

| class | drift | detection rule |
|---|---|---|
| D1 | refusal-class mix shift (new class dominating, class vanishing) | live census of `pub enum *Refusal*` variants in `src/` diffed against baseline `class_names`; per-class proportion of test-corpus mentions checked against control limits (see SPC below) |
| D2 | suite-count regression (tests silently removed) | `tests/*.rs` count < baseline `suites.count` |
| D3 | per-suite test-count regression | `#[test]` count per suite < baseline `tests_per_suite` |
| D4 | API-surface drift (pub fns vanishing) | `^pub (async )?fn` count per module < baseline; module vanishing from `src/` |
| D5 | chain-verification failures | reserved baseline slot (`chain_verification_failures`, int >= 0, currently 0); local check asserts slot shape only — CI wiring asserts the count from CI logs (see Wiring) |

Standing-flip rate (PARTIAL→ALIVE without receipts) was chartered but is owned by
the promotion kernel's own courts (`promote.UnwitnessedCourt`,
`promote.AcceptanceUnwitnessed` are already typed refusal classes counted under
D1); no separate metric implemented in this lane.

## MEASURE — current values (as-of final census, this session)

- Refusal enums: `Refusal` (admit), `PromoteRefusal`, `LeaseGateRefusal`,
  `ShaclRefusal`, `BridgeRefusal`, `DescriptorRefusal`, `ReceiptRefusal`,
  `CourtMapRefusal`, `SovereignRefusal`, `LocatorRefusal`, `OriginRefusal`,
  `Refusal` (epoch) plus concurrent-lane modules (`generated`, `reconcile`,
  `target_pack`, `pack_health`) → **93 distinct refusal classes**
- Test-corpus refusal-class mentions: **114** across 16 suites
- Suites: **16** files; per-suite `#[test]` counts in baseline
- Pub fns: **51** across src modules (per-module counts in baseline)
- Chain-verification failures slot: **0** (CI-enforced; no local evidence)

## EXPLORE — SPC rule choice

With deterministic suites there is no sampling variation in the *code*, only in
which classes the test corpus exercises. The lawful chart is attribute-based:
a **p-chart on refusal-class proportions** over the corpus mention counts
(n = total mentions, real n = 114 at baseline). 3-sigma limits
`p ± 3·sqrt(p(1-p)/n)` applied only when `total >= 30` and the class count
`>= 5`; otherwise **fixed ±50% bands labeled `FIXED`** — never fake sigma. A
zero-mention baseline class has `hi = 0`: any later mention of it is mix shift,
which is correct (it forces a baseline re-mint, see Drift-under-concurrency).

## DEVELOP + IMPLEMENT

- `scripts/sjira_control_baseline.py` — re-mints the baseline from a live census
  (filesystem-only; never git). The committed baseline must equal its output.
- `scripts/sjira_control_plan.py` + `scripts/sjira_control_plan.sh` (wrapper) —
  the gate. Exit 0 = IN CONTROL; exit 1 = one typed `VIOLATION class=D*` line
  per finding.
- Baseline (this lane owns it, new file):
  `docs/jira/v26.11.3/baselines/sjira-control-baseline.json`

## Verification ladder (actual output)

1. Gate on current tree: `python3 scripts/sjira_control_plan.py .` →
   `sjira-control-plan: IN CONTROL (93 refusal classes, 114 corpus mentions,
   16 suites, 51 pub fns)` — **exit 0**
2. Anti-vacuity mutation (charter): fake `admit.FakeNewRefusalClass` injected
   into a temp copy of the baseline →
   `VIOLATION class=D1-vanished-refusal-class ... admit.FakeNewRefusalClass`,
   **exit 1**
3. Secondary mutation D2 (baseline suites 16→17, simulating a removed suite) →
   `VIOLATION class=D2-suite-count-regression suites 16 < baseline 17`, **exit 1**
4. Secondary mutation D4 (baseline `admit` pub fns →99) →
   `VIOLATION class=D4-pubfn-vanishing module admit: 5 < baseline 99`, **exit 1**
5. Wrapper recheck: `./scripts/sjira_control_plan.sh .` →
   `IN CONTROL (93 refusal classes, 114 corpus mentions, 17 suites, 51 pub fns)`
   — **exit 0** (suite count grew 16→17 between runs from a concurrent lane's
   new suite; growth is not drift, D2 asserts only `>= baseline`)

A court that flips is the point; all three mutations flipped the gate.

## Intended wiring (recorded, not applied — justfile not edited per charter)

```
# ggen justfile, when the coordinator admits it:
sjira-control:
    python3 scripts/sjira_control_baseline.py . --check 2>/dev/null || true
    python3 scripts/sjira_control_plan.sh .
```

`--check` mode for the baseline regenerator (baseline drift = re-mint required)
is future work; today the gate is `sjira_control_plan.sh` alone, and CI should
additionally assert the baseline file equals `sjira_control_baseline.py` output
(diff) to catch stale baselines.

## Standing / caveats

- The checkout was hot during this lane: concurrent lanes landed `shacl.rs`,
  `generated.rs`, `reconcile*`, `target_pack*`, `pack_health*` and 5 new suites
  mid-census (measured: 86→95→114 mentions, 11→15→16 suites across runs). All
  census numbers are as-of the final mint immediately before the final gate run;
  the committed baseline matches the tree at that instant only.
- Baseline regeneration is the *admitted* response to legitimate mix shift (new
  refusal classes landing with tests); the gate enforces that the shift is
  conscious (baseline diff = reviewable), not silent.
- D5 remains a reserved slot: local exit-0 does not certify CI chain
  verification; that requires the CI wiring named above.

## Standing: PARTIAL_ALIVE

Gate ALIVE on the exact tree at final census (exit 0 + 3 flipping mutations).
Not ALIVE end-to-end: no justfile/CI hook executed (charter forbids editing
ggen's justfile); D5 CI enforcement unwired.
