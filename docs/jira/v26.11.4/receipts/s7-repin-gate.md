# S-7 — E-2 self-host observation re-pin gate (HEAD → observation commit)

Date: 2026-11-05. Repo: /Users/sac/ggen @ 3c2ee02e7 (shared checkout, no
worktrees, no commits made). Closes the mechanicalization half of
`docs/jira/v26.11.4/SEED.md` §E-2: the `--check` gate is now bound to the
observation commit recorded in the committed receipt, not literal HEAD
(C21-class out-of-subject receipt binding: a commit cannot contain its own
receipt).

Standing: ALIVE

## Re-verification (2026-11-05, lane L7 re-read at use time)

All receipt-cited claims re-verified on disk this session, plus two new runs:

1. Real-tree gate: `python3 self-host/scripts/observe_self_host.py --check`
   at live HEAD → **exit 0** (passes without re-breaking on post-landing
   commits — the E-2 defect class).
2. Non-vacuity fixture landed in-repo:
   `self-host/tests/test_observation_repin_gate.py` — Chicago-style court
   over a real scratch git repo: (a) gate passes across an unrelated HEAD
   shift, (b) planted stale pin (`0`*40 in the committed receipt) is refused
   (exit != 0), (c) byte-exact receipt restore returns PASS. Run:
   **1 passed in 0.77s**; full battery after adding the fixture:
   **21 passed in 5.79s** (was 20).
3. Ad-hoc shell court also executed out-of-band with the same flip pattern
   (CLEAN_EXIT=0 / STALE_EXIT=1 / RESTORE_EXIT=0) before the fixture landed.

Diff unchanged from the 3 script files listed below; the fixture test is the
only new file.

## Defect (pre-existing)

`observe_self_host.py --check` regenerated the observation at literal
`rev-parse HEAD` and compared against committed outputs minted at an earlier
observation commit — so every post-landing commit re-broke the binding by
construction (live state at lane open: recorded revision `244eed7f9`, HEAD
`3c2ee02e7`).

## Diff (3 files, working tree only — uncommitted per lane law)

- `self-host/scripts/observe_repository.py` — `tracked_paths(root, revision)`
  (revision-bound form uses `ls-tree -r` at the revision, unbound keeps
  `ls-files`); `observe(root, revision=None)`; new `recorded_revision(root)`
  reading the revision from `self-host/observed/observation-receipt.json`;
  `main()` pins `--check` to `recorded_revision` (write mode stays HEAD).
- `self-host/scripts/observe_exact_tree.py` — `head_snapshot(root, revision)`
  + module `_OBSERVATION_REVISION` via `set_observation_revision()`;
  revision-aware `observe()` wrapper; re-exports
  `recorded_revision = MODEL.recorded_revision`; `main()` pins `--check`.
- `self-host/scripts/observe_self_host.py` — revision threaded through
  `observe(root, revision)`; `main()` pins `--check` to
  `EXACT.recorded_revision(root)`.

## Gates (real output, this session)

1. Compile: `python3 -m py_compile` ×3 → OK.
2. Existing battery: `python3 -m pytest self-host/tests/ -x -q` →
   **20 passed in 5.74s**.
3. Falsifier (scratch repo from fetched objects, tree extracted at
   observation commit `244eed7f9`, HEAD shifted to `403143038` by an
   unrelated commit; content untouched):
   - OLD gate (pre-fix scripts from the committed tree), `--check`:
     `GGEN_SELF_HOST_DRIFT: self-host/ontology.ttl, self-host/observed/repository.json, self-host/observed/observation-receipt.json`,
     `OLD_GATE_EXIT=2` — wrong flip purely from the HEAD shift.
   - NEW gate, live ggen at HEAD `3c2ee02e7` (recorded `244eed7f9`),
     `--check`: `LIVE_NEWGATE_EXIT=0` — stable across the same shift class.
   - NEW gate anti-vacuity (same scratch, committed receipt tampered):
     `GGEN_SELF_HOST_DRIFT: self-host/ontology.ttl, self-host/observed/repository.json, self-host/observed/observation-receipt.json`,
     `NEWGATE_TAMPER_EXIT=2` — the re-pinned gate still flips on real drift.
4. Timings: each full observation run ~15-17 min CPU (matches ZB-21's
   ~17.5 min); all runs executed to completion, no sleeps-as-methodology.

## Not done / residue

- `verify_observation.py` still checks against literal HEAD by design
  (it is the post-re-pin verifier at terminal HEAD, per ZB-21's sequence);
  left untouched.
- Scratch evidence tree left at /tmp/s7-repin (includes tampered receipt);
  not integrated into the repo. One mid-flight defect introduced and
  repaired in-session: a garbled Edit briefly truncated
  `observe_self_host.py`; repaired before any gate ran (py_compile + full
  diff review).
- Old-gate scratch run used the committed pre-fix scripts; new-gate runs
  used the working-tree scripts copied into scratch — same subject tree,
  same shifted HEAD for both old-flip and tamper runs.
