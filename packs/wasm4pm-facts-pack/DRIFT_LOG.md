# Upstream drift log — wasm4pm-facts-pack

Dated, evidenced checks of `ontology.ttl` against the upstream wasm4pm
checkout (`~/wasm4pm`, commit pinned per entry below), run via
`scripts/check_upstream_drift.sh`. This is a real per-individual diff, not a
whole-file byte comparison, so a reordering or an unrelated upstream comment
change does not itself count as drift.

## 2026-07-18 — upstream commit `b6fedcbef8d5fbdab4dbb9827226e802fe961a71`

Ran `scripts/check_upstream_drift.sh /Users/sac/wasm4pm`. Initial run found
**2 of 60** `pi:ProcessIntelligenceAlgorithm` individuals had drifted from
upstream on the `pi:wasmExport` field (breeds: 0/55 drift):

| Subject | Field | Pack (before fix) | Upstream (correct) |
|---|---|---|---|
| `pi:Algo_optimized_dfg` | `pi:wasmExport` | `discover_optimized_dfg` | `discover_dfg` |
| `pi:Algo_streaming_log` | `pi:wasmExport` | `discover_streaming_log` | `discover_dfg` |

Both corrected in `ontology.ttl` in this pass to match upstream exactly
(upstream itself maps both individuals to the same `discover_dfg` wasm
export — that is upstream's own fact, reproduced verbatim here, not this
pack's error to second-guess). Re-ran the script after the fix:

```
OK: breed individuals match upstream exactly (55 rows)
OK: algorithm individuals match upstream exactly (60 rows)
```

Exit code 0. All 115 individuals now byte-for-byte match (modulo whitespace)
their upstream source blocks.

## 2026-07-19 — re-run found the SAME 2 individuals re-drifted

Re-ran `scripts/check_upstream_drift.sh /Users/sac/wasm4pm` at the start of the
L5-push round-3 session (before any edits this round). Despite the 2026-07-18
entry above claiming both individuals were corrected, this pack's committed
`ontology.ttl` on `feat/l5-push-round3` (branched from a merged main) again
carried `pi:wasmExport "discover_optimized_dfg"` /
`"discover_streaming_log"` for `pi:Algo_optimized_dfg` /
`pi:Algo_streaming_log` — upstream's own file is unchanged, still
`"discover_dfg"` for both. `git log`/`git diff HEAD` show the tree is clean
(this is the actually-committed state, not an uncommitted edit), so the
2026-07-18 fix either never made it into the commit that landed on this
branch, or was reverted by an intervening merge — the mechanism is not
determined here, only the observable fact that the drift was back. Re-fixed
both to `"discover_dfg"` in this round. Re-ran the script:

```
OK: breed individuals match upstream exactly (55 rows)
OK: algorithm individuals match upstream exactly (60 rows)
```

**Known cross-pack consequence, disclosed but NOT fixed (out of scope for
this pack — editing another pack's directory is off-limits this round):**
`packs/wasm4pm-algorithms-pack/ontology.ttl` asserts the identical
`pi:Algo_optimized_dfg`/`pi:Algo_streaming_log` subject IRIs with the SAME
stale `"discover_optimized_dfg"`/`"discover_streaming_log"` values (verified
2026-07-19, lines 267/344 of that pack's `ontology.ttl`). Because both packs
assert `pi:wasmExport` on the identical IRIs, a consumer that wires both
packs together (as `examples/receiptctl` does) gets a multi-valued
`pi:wasmExport` for these 2 subjects once again — this pack's copy is now
upstream-correct (`discover_dfg`) but the sibling's copy is not, so the union
graph is presently **inconsistent between these two packs**, the same class
of bug the L5_PUSH_RESULTS.md round-2 log (bug #2) already found and fixed
once. This needs a `wasm4pm-algorithms-pack` maintainer/agent to apply the
identical one-line fix in its own `ontology.ttl`; flagging here rather than
editing that pack's files directly.

## 2026-07-19 (later same day) — L5-push round-4: re-drifted a THIRD time, fixed, and now cross-verified by a fresh scratch consumer (not just the drift script)

Re-ran `scripts/check_upstream_drift.sh ~/wasm4pm` (upstream still pinned at
`b6fedcbef8d5fbdab4dbb9827226e802fe961a71`) at the start of this round, before
any edits. Result: the *same* 2 individuals from the two prior entries above
(`pi:Algo_optimized_dfg`, `pi:Algo_streaming_log`) had drifted back to
`pi:wasmExport "discover_optimized_dfg"` / `"discover_streaming_log"` again,
despite the 2026-07-19 entry immediately above claiming both were corrected
earlier today. Exact same failure signature as the prior round; root cause
of the *recurrence* (why a committed fix keeps reappearing stale — branch
rebase, concurrent-session overwrite, or something else) is still not
determined here, only the observable fact reconfirmed a third time.

Re-fixed both lines in `ontology.ttl` to `pi:wasmExport "discover_dfg"`
(upstream's actual value, confirmed byte-for-byte against
`~/wasm4pm/ggen/ontology/algorithms.ttl` lines 190 and 273). Re-ran the
script:

```
OK: breed individuals match upstream exactly (55 rows)
OK: algorithm individuals match upstream exactly (60 rows)
```

Exit code 0.

This round additionally verified the fix downstream of the ontology, not
just in the drift script: built `ggen` (`cargo build -p ggen-cli-lib --bin
ggen`), wired this pack alone into a fresh scratch consumer
(`ggen.toml` with only `wasm4pm-facts-pack`, no sibling packs), ran
`ggen sync run` for a real generation, then `cargo test` in that consumer.
All 18 generated proof tests passed (6 in
`wasm4pm_facts_pack_full_coverage_proof.rs`, 12 in
`wasm4pm_facts_pack_registry_proof.rs`), and the generated
`src/wasm4pm_facts_registry.rs` was grepped directly to confirm both
`Algo_optimized_dfg`/`Algo_streaming_log` rows now carry
`wasm_export: "discover_dfg"` — the fix reaches the actual generated
artifact a consumer would compile against, not only the ontology source.

**Still true, unchanged from the prior entry:** `packs/wasm4pm-algorithms-pack/ontology.ttl`
still asserts the same two subject IRIs with the stale
`discover_optimized_dfg`/`discover_streaming_log` values (not re-checked this
round, but no evidence it was touched) — the cross-pack union-graph
inconsistency for any consumer wiring both packs together remains open.
Out of scope for this pack's own directory; flagged again for whichever
session next touches `wasm4pm-algorithms-pack`.

**Cannot verify against a live `examples/receiptctl` sync this round:** that
consumer's `ggen.lock` was stale against unrelated concurrent changes to
`chicago-tdd-tools-pack` in this working tree (a different pack's
`gates/*.rq` now reports a violation against the receiptctl union graph),
which blocked a full `ggen sync run` there independent of anything in this
pack. Not fixed here — editing another pack's files is out of scope for this
round; the scratch-consumer verification above is the substitute evidence.

## Known limitation

This script requires a local `~/wasm4pm` checkout and is not wired into CI —
running it is a manual, dated act by whoever maintains this pack, not an
automatic gate. It is a genuine improvement over "manually copied once with
no re-check mechanism" (the L2 bar), but does not reach the L5 bar of
fidelity being *definitional* (structurally impossible to drift) — that
would require wasm4pm to publish these facts as a resolvable ontology this
pack could programmatically pull and diff at sync time, or for
`ggen sync` itself to support an upstream-ontology-URL fetch+diff hook. That
capability does not exist in `ggen-engine` today; adding it is an
engine-level change outside this pack's own files.
