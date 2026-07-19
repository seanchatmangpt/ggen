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
