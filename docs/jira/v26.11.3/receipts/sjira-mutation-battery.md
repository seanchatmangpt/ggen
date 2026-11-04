# Receipt — sjira_mutation_battery.py (DS-7 battery run + flip-set record)

Date: 2026-11-3 · Lane: DS-10 (PF-2 obligation 2) · Script:
`/Users/sac/ggen/scripts/sjira_mutation_battery.py` (pre-existing, unmodified)

## Execution

- Full run: `python3 scripts/sjira_mutation_battery.py -q` (background,
  canonical checkout, in-place shims with sha256-exact restore; git never
  invoked). Script exit 1 (gate FAIL — see registered divergences).
- Rerun of the 11 compile-aborted criteria on the quiet tree:
  `python3 scripts/sjira_mutation_battery.py --only LEAS-C08 LEAS-C08b
  EPOC-C09 EPOC-C10 LEDG-C11 LEDG-C12 LEDG-C13 RECP-C14 CTRL-C15 CTRL-C16
  DESC-C18 -q` → exit 1 (LEAS-C08 VACUOUS).

## Flip-set record (21 criteria, combined across the two runs)

FLIPPED (17): EXEC-C01, EXEC-C02c, LEAS-C04, LEAS-C05, LEAS-C06, LEAS-C07,
LEAS-C08b, EPOC-C09, EPOC-C10, LEDG-C11, LEDG-C12, LEDG-C13,
RECP-C14, CTRL-C15, CTRL-C16, PROM-C17, DESC-C18.

VACUOUS (4), each registered with analysis:
- **EXEC-C02** (`if !progressed` → `if false && !progressed`): the shim is
  semantically absorbed — a no-progress pass still falls through to the
  final `Err(exhausted(...))` after the loop, so the pinned test observes
  the same typed refusal either way. The exhaustion law survives through
  the loop-exit refusal; the shim targets a redundant guard. Nearest-flip
  witness: EXEC-C02c (mutating the typed reason string flips
  `max_iterations_refuses_typed_exhaustion`).
- **EXEC-C02b** (loop pinned to 1 iteration): the pinned test's budget is
  exactly 1, so pinning the loop to 1 is indistinguishable. Fix shape:
  a budget-2-over-3-chain test would flip it (named follow-on).
  Nearest-flip witness: EXEC-C02c.
- **EXEC-C03** (`status == Appended` → `if true`): the rerun test's second
  run has an EMPTY frontier (the order is standing-blocked on replay), so
  the append idempotency branch is never reached under the shim. The
  branch is only reachable when a candidate is eligible yet its event is
  already recorded — reachable only via a standing-projection/receipt-
  digest edge (named follow-on test).
- **LEAS-C08** (`granted_at` removed from canonical bytes → its own pinned
  test `unsigned_field_change_breaks_verification` still passes): the
  test name promises a law the test does not pin. The law itself IS
  witnessed — LEAS-C08b runs the identical shim against
  `canonical_json_is_sorted_key_compact_minus_signatures` and FLIPS. So
  this is a test-strength divergence, not a kernel gap; fix shape: make
  C08's test assert canonical-JSON equality (C08b's assertion), or
  repoint C08's battery row at C08b's test (named follow-on).

COMPILE-ABORT (0 final): the first run aborted 8 criteria (LEAS-C08 onward)
on mid-run lane landings; all 11 rerun clean on the quiet tree.

## Gate status

Gate FAIL (4 registered VACUOUS rows) — honest-fail landing per doctrine:
thresholds not relaxed, rows registered with analysis and named fix shapes
rather than deleted or marked pass. Script unmodified (finish + run +
record was the obligation; test-strengthening is the follow-on, touching
execute_test.rs/lease_epoch_test.rs — shared court files, so it needs lane
ownership coordination).