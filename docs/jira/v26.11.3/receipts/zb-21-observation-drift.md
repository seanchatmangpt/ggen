# ZB-21 — self-host observation drift check

Date: 2026-11-04. Lane: read-only census (Explore), coordinator-authored
receipt (lane contract was report-only). Subject: ggen branch
`v26-11-3-wave` @ 58a3b4108.

## Findings (all from ZB-21's live runs this session)

- `observe_self_host.py --check` → exit 2,
  `GGEN_SELF_HOST_DRIFT` naming ontology.ttl +
  observed/repository.json + observation-receipt.json — the expected
  typed refusal; deterministic ×2.
- Full re-observation cost measured: ~17.5 min (12:43-13:01).
- Zero side effects: `git status self-host/` unchanged; ontology.ttl
  sha256 `42d745d6…` identical before/after both runs.
- Self-host sync dry-run: exit 0, all outputs
  `unchanged: content identical`.
- Observation outputs fully committed and clean
  (`git ls-files self-host/observed/`: receipt/repository/
  root-regeneration). Content is NOT stale — only the revision
  pointer is: committed observation revision `ed3f1dfb1` vs HEAD
  `58a3b4108` (3 commits behind: 72b743d23, bda447441, 58a3b4108).

## Re-pin plan (for the terminal sweep)

1. At the terminal HEAD (no further commits pending): run the real
   observer (no --check), budget ~18 min.
2. Commit the rewritten outputs as the closing re-pin commit;
   `--check` then exits 0 at that HEAD.
3. C21-class note (carried in v26.11.4/SEED.md §E-2, UNVERIFIED until
   a court lands): the receipt cannot contain its own commit hash —
   the `--check` gate must compare against the observation commit
   (HEAD~1 at the re-pin commit), never literal HEAD, or every
   post-landing commit re-breaks the binding by construction.

Standing: PARTIAL — census and plan only; the re-pin itself runs at
terminal HEAD per the sweep sequence.
