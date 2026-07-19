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
