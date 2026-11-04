# RECEIPTS

Register of truth for v26.11.3 lane receipts. Every row cites a receipt
file on disk in `receipts/`; status re-read from receipts at each update
(never from memory). Standing vocabulary:
ALIVE / PARTIAL / BLOCKED / UNVERIFIED / REFUSED. ALIVE requires a green
gate re-run THIS session (2026-11-03, lane-close run).

## Landed lane receipts (WO-01 through WO-07)

| WO | Receipt(s) | Standing | Gate re-run this session (exit 0) |
|---|---|---|---|
| WO-01 | g1-stage2c, k8-cli-flag | ALIVE | engine cli_boundary 32/0; stage2c wiring 7/0 |
| WO-02 | g8-cli.md | ALIVE | `-p ggen-cli-lib --test ea_verbs_test` 7/0 |
| WO-03 | g4-composer.md | ALIVE | `-p ggen-cli-lib --test pack_compose_test` 6/0 |
| WO-03 | g4-composer.md | ALIVE | `-p ggen-marketplace --lib packs_registry::composer` 5/0 |
| WO-04 | g7-schema.md | ALIVE | `-p ggen-config --test ea_graph_schema_parity_test` 4/0 |
| WO-04 | x3-projection.md | ALIVE | `-p ggen-abb-sbb --test ea_projection_test` 5/0 |
| WO-05 | g5-refusal-fences.md | ALIVE | `-p ggen-abb-sbb` 129+3dt, 0 failed (crosswalk 3/3) |
| WO-06 | i5-wo06-reconcile.md | ALIVE | `-p ggen-engine --lib envelope` 8/0 |
| WO-07 | g6-depgraph.md, k1-refusal-flip.md | ALIVE | `-p ggen-abb-sbb` 129+3dt (hardening 15/15) |

## Orphan-recovery lanes (CL-P / CL-R)

Standing words re-read from each receipt body on disk (2026-11-03, cl-close
lane). Exit record law: A gate is witnessed by an exit record: either (a) the process exit status, or (b) cargo's "N passed; 0 failed" summary line. A passed-count without a failed-count is not an exit record.

| Lane | Receipt | Standing | Exit record |
|---|---|---|---|
| CL-P | cl-p-g2-recovery.md | VERIFY-ALREADY-LANDED (no code change; G2's own receipt g2-typed-refusals.md reads ALIVE; `cargo test -p ggen-engine` every binary `test result: ok`, lib 235 passed / 0 failed) | 0 failed |
| CL-R | cl-r-transactional-recovery.md | ALIVE | transactional_edits_test 17 passed; 0 failed; `cargo test -p ggen-engine --lib` 235 passed; 0 failed |

WO-01 blocker note: g1's PARTIAL standing (9 red CLI tests) was closed by
the k8 receipt (`--skip-ea-admission` flag + fail-closed Stage 2c block 1);
k8's court list includes the flip witness (SHIM-COURT, restore byte-exact).

## Open work orders (NOT closed by this lane)

| WO | Receipts | Standing |
|---|---|---|
| WO-08 | ea-run-transcript.md names the WO-08 interim route; arcus-side receipts + close record at /Users/sac/arcus/docs/jira/v26.11.3/closure/waves-wo08-wo09.md read PARTIAL-ALIVE | landed on disk — PARTIAL-ALIVE per the close record; closure decision not made by this index lane |
| WO-09 | x4-consumer-guide.md (ALIVE), ea-run-transcript (ALIVE) | landed on disk — receipts' own rows read ALIVE; closure decision not made by this index lane |

WO-09 receipts landed mid-close (13:59, during this lane's run) and their
own standing rows read ALIVE; the arcus-side close record
(closure/waves-wo08-wo09.md) re-ran all gates green. Receipt citations above
are as-of-listing; re-read before use.
