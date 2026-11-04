# GA-7 — sjira control wiring (DS-6 inheritance)

Lane: GA-7, wave v26.11.3. DS-6 landed the control-plan machinery
(`scripts/sjira_control_plan.py`, `scripts/sjira_control_baseline.py`,
`scripts/sjira_control_plan.sh`, `scripts/sjira_mutation_battery.py`,
`docs/jira/v26.11.3/baselines/sjira-control-baseline.json`) but its justfile
wiring was outside its ownership — no ds-6 receipt file existed at GA-7 start;
scripts + baseline were read directly and found complete (no completion needed).

## Diff (this lane)

- `justfile` — append-only block `── GA-7: sjira control-plan wiring ──`:
  `sjira-control` (delegates to `scripts/sjira_control_plan.sh`, zero inline
  logic) and `sjira-control-court` (delegates to the new court). No existing
  recipe touched; not wired into `pre-commit` (promotion is an operator call,
  v26.10.20 courts precedent).
- `scripts/sjira_control_court.sh` — NEW: anti-vacuity court for the checker.
  Two independent mutations on a `mktemp` copy of `crates/ggen-sjira`
  (M1: baseline `pub_fns` inflated +5 → checker must exit 1 with typed
  `VIOLATION class=D4-pubfn-vanishing`; M2: one `#[test]` deleted from a suite
  copy → must exit 1 with `VIOLATION class=D3-test-count-regression`). Court
  exits 1 on any vacuous outcome; `trap rm -rf` cleans the temp root; canonical
  tree is read-only; never runs git.

## Gates (run this session, real exits)

| Gate | Command | Exit | Witness |
|---|---|---|---|
| control in-control | `just sjira-control` | 0 | `IN CONTROL (93 refusal classes, 114 corpus mentions, 16 suites, 51 pub fns)` |
| anti-vacuity flip | `just sjira-control-court` | 0 | `[FLIP] M1 ... D4-pubfn-vanishing` + `[FLIP] M2 ... D3-test-count-regression`, `PASS (2/2 flips witnessed)` |

`just` parsing of the modified justfile is itself witnessed by both runs
(invoked via `just`). The heavyweight DS-7 mutation battery
(`scripts/sjira_mutation_battery.py`, 18 cargo-test mutations) is untouched and
NOT wired into these recipes — it remains standalone by DS-6/DS-7 design.

## Standing

ALIVE — both recipes executed on the canonical checkout this session with the
exits above. Falsifier for the wiring: `just sjira-control-court` returning
exit 1 (a flip lost = checker or court regressed).
