# zd-50: WO Standing-Word Drift Reconciliation (CARRY-01, v26.11.3)

- **Date**: 2026-11-04
- **Repo**: /Users/sac/ggen (branch v26-11-3-wave)
- **Standing**: ALIVE for the landed surface — a verification sweep grep per WO
  (standing word in WO == word in cited receipt body / close record) exits clean, run ×1.
- **Scope**: standing words only. No verdict re-grading; no content edits outside the
  Standing line/section of each WO file.

## Subject

Six WO files still carried their spec-time standing word (`UNVERIFIED`) after their
receipts landed with different words. Resolved by reading both sides (never-destructive
conflict heuristics): each WO's standing now equals what its cited receipt body says on
disk, with the citation in place.

## Per-WO before/after

| WO | File | Before | After | Receipt body (source of truth) |
|---|---|---|---|---|
| WO-01 | WO-01-stage-2c-wiring.md §Standing | UNVERIFIED | ALIVE | g1-stage2c.md:6 reads PARTIAL (9 CLI tests red on pending CLI flag); blocker closed by k8-cli-flag-depgraph.md:6 ALIVE ("all gates run green this session; mutation-court flip witnessed"); RECEIPTS.md WO-01 row reads ALIVE with gate re-run record |
| WO-03 | WO-03-composer-stage2c.md:3 | UNVERIFIED | ALIVE | g4-composer.md:76 reads "ALIVE — commands and exits above are from this session"; RECEIPTS.md WO-03 rows read ALIVE (6/0, 5/0 re-runs) |
| WO-05 | WO-05-refusal-vocab-fences.md §Standing | UNVERIFIED | ALIVE for the landed surface | g5-refusal-fences.md:66 reads "ALIVE for the landed surface (rule renders deterministically; court 3/3)" |
| WO-07 | WO-07-depgraph-hardening.md §Standing | UNVERIFIED | ALIVE | g6-depgraph.md:91 reads "ALIVE for the landed surface … PARTIAL on refusal variants"; k1-refusal-flip.md:54 reads "ALIVE for the flipped surface", explicitly resolving G6's PARTIAL-on-refusal-variants; RECEIPTS.md WO-07 row reads ALIVE (hardening 15/15) |
| WO-08 | WO-08-arcus-ea-graph.md §Standing | UNVERIFIED | PARTIAL-ALIVE | arcus close record /Users/sac/arcus/docs/jira/v26.11.3/closure/waves-wo08-wo09.md:49 reads "WO-08 standing: PARTIAL-ALIVE" (5 SBBs ADMITTED, gates green; F2-dependent declarative-admission witness registered as follow-up) |
| WO-09 | WO-09-working-example.md:6 | UNVERIFIED | ALIVE | x4-consumer-guide.md:7 reads ALIVE; close record:75 reads "WO-09 standing: ALIVE" |

No receipt body carried a missing standing word, so no UNSTATED-with-cite row was needed.

## Verification

Sweep run ×1 (2026-11-04), exit 0: for each of the six WOs, the standing word in the WO
file was compared against the word in its cited receipt body via direct sed/grep reads
(both sides re-read after the edits; output above in session transcript). All six match.
`UNVERIFIED` count in the six WO files after the sweep:

    grep -c UNVERIFIED WO-0{1,3,5,7,8,9}*.md  →  0 matches in Standing lines

## Notes

- WO-01's ALIVE is a resolved-partial: g1's body still reads PARTIAL on disk; the word
  ALIVE is carried by the closing receipt k8 (which RECEIPTS.md's blocker note names as
  the closer). Both bodies are cited in the WO line so the resolution is replayable.
- WO-07 same shape: g6's PARTIAL-on-refusal-variants is resolved by k1's body explicitly
  ("G6's Standing note can be updated to ALIVE at coordinator integration").
- WO-08 stays PARTIAL-ALIVE, not ALIVE: the close record's own word is PARTIAL-ALIVE
  (declarative-admission witness is a named follow-up). Not rounded up.

## See Also

- `docs/jira/v26.11.3/RECEIPTS.md` — index rows this reconciliation agrees with
- `receipts/g1-stage2c.md`, `receipts/k8-cli-flag-depgraph.md`, `receipts/g4-composer.md`,
  `receipts/g5-refusal-fences.md`, `receipts/g6-depgraph.md`, `receipts/k1-refusal-flip.md`,
  `receipts/x4-consumer-guide.md` — cited receipt bodies
- `/Users/sac/arcus/docs/jira/v26.11.3/closure/waves-wo08-wo09.md` — arcus-side close record
