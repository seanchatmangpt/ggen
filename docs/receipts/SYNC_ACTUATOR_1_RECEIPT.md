# SYNC-ACTUATOR-1 Receipt

## Final State

**SYNC-ACTUATOR-1 COMPLETE** — `ggen sync` actuates one Gall foundation artifact from
the smallest public-ontology-aligned `O*`, and the run is replayable/inspectable.

> `A = μ(O*)`, `μ ≜ ggen sync`. The actuator can move load — not just refuse it.

## The boundary

`playground/sync-foundation/` — the smallest capable `O*`:

| File | Role | Public footing |
|------|------|----------------|
| `ggen.toml` | manifest: ontology → inference → query → template → output | — |
| `ontology/command-foundation.ttl` | seed facts (actuator/sensing roles, the pier) | RDF/RDFS + PROV-O + DCTERMS |
| inline `[inference]` CONSTRUCT | normalization (satisfies DMAIC *Measure*) | SPARQL |
| inline SELECT | extracts the Gall test spec | SPARQL |
| `templates/gall_command_foundation.rs.tera` | renders the Rust test | Tera |
| `expected/gall_command_foundation.rs` | deterministic expected artifact | — |

## Acceptance — all met (one honest gap)

| Criterion | Result |
|-----------|--------|
| `ggen sync` runs μ₁–μ₅ | ✓ 11 quality gates + 5 DMAIC phases all PASSED |
| consumes real ontology + CONSTRUCT + query + template | ✓ |
| emits one Gall test artifact | ✓ `generated/gall_command_foundation.rs` (1349 bytes, valid Rust) |
| artifact is deterministic | ✓ hash stable across re-sync; matches `expected/` |
| emits one receipt | ✓ `.ggen/receipts/<ts>.json` — UUID v4, RFC-3339, **non-empty Ed25519 signature**, input + output hashes |
| replay can inspect | ✓ `ggen lsp replay --root` reachable, honest (`no promoted-route artifact`) |
| metrics can inspect | ✓ `ggen lsp metrics --root` → `route_hit_rate=1.0`, `receipt_density=1.0`, `verdict=insufficient_evidence` (honest) |
| **receipt references ontology/template** | ✓ **RESOLVED** by O-STAR-RECEIPT-CLOSURE-1 — `input_hashes` now binds the manifest + ontology + external query/template files + actuator identity. See [O_STAR_RECEIPT_CLOSURE_1_RECEIPT.md](O_STAR_RECEIPT_CLOSURE_1_RECEIPT.md). |

## The actuation command

```bash
cd playground/sync-foundation
ggen sync --manifest ggen.toml
```

```
[Quality Gate: Manifest Schema] ✓ … [DMAIC Phase 5: Control] ✓
All Gates: ✅ PASSED → Proceeding to generation phase
✓ Generated 1 files in 52ms  (1 inference rule, 1 generation rule, 1349 bytes)
{"files_synced":1,"receipt_path":".ggen/receipts/latest.json","status":"success"}
```

## OCEL correctness + feedback (the "fed back when appropriate" loop)

`ggen lsp check` (sensing) writes well-formed OCEL to `.ggen/ocel/agent-edit-events.ocel.jsonl`:

```json
{ "id": "...", "activity": "DiagnosticRaised", "timestamp": "2026-05-29T05:50:04Z",
  "objects": [ {"id":"q.rq","type":"file"}, {"id":"E0011","type":"diagnostic_code"},
               {"id":"...","type":"episode"}, {"id":"local","type":"agent"} ],
  "attributes": { "severity":"warning", "transport":"headless", ... } }
```

Object-centric (typed `objects[]`), with `activity` + `timestamp` — minable, not just
loggable. `ggen lsp metrics` consumes it and reports real values where evidence exists and
`insufficient_evidence` where it does not — it cannot be gamed into a false verdict.

## Order this unlocks

```
1. SYNC-ACTUATOR-1               ← COMPLETE (the actuator moves load)
2. gall_sync_actuation triad     ← capable dry-run + capable real sync (next)
3. GALL-COMMAND-FOUNDATION-1      ← use sync to manufacture the command-foundation pack
4. capability-map                ← resumes only after the floor holds
```

## Remaining work

- Complete the actuation triad in `gall_sync_actuation.rs` (capable dry-run previews
  without a receipt; capable real sync emits artifact + receipt) using this project.
- Fix `ggen sync` receipt `input_hashes` to include the ontology + template (so the
  receipt fully describes what was consumed).
