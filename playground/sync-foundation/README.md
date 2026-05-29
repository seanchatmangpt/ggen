# SYNC-ACTUATOR-1 — the smallest capable `O*`

This is the foundation *before* the foundation factory: the smallest
public-ontology-aligned boundary from which **`ggen sync` actuates once** —
emitting one Gall foundation-test artifact and one receipt.

> `A = μ(O*)`, where `μ ≜ ggen sync` (the only actuation). LSP/MCP/A2A sense and
> route; only `ggen sync` reaches artifact-or-honest-refusal + receipt territory.

## The boundary (`O*`)

| File | Role |
|------|------|
| `ggen.toml` | the manifest wiring ontology → inference → query → template → output |
| `ontology/command-foundation.ttl` | the seed facts, on public footing (RDF/RDFS + PROV-O + DCTERMS) |
| (inline `[inference]`) | CONSTRUCT normalization — satisfies the DMAIC *Measure* gate |
| (inline `[[generation.rules]].query`) | SELECT that extracts the Gall test spec |
| `templates/gall_command_foundation.rs.tera` | renders the Rust Gall test from the bindings |
| `expected/gall_command_foundation.rs` | the deterministic expected artifact |

## Run it

```bash
cd playground/sync-foundation
ggen sync --manifest ggen.toml
```

`ggen sync` runs all 11 quality gates + 5 DMAIC phases, then manufactures:

- `generated/gall_command_foundation.rs` — the artifact (deterministic; re-sync is hash-stable)
- `.ggen/receipts/<ts>.json` — the receipt (real UUID, RFC-3339, Ed25519 signature,
  input + output hashes), with `latest.json` as the chain head
- `.ggen/keys/{signing,verifying}.key` — the signing material
- `.ggen/cache/*.sha256` — input-hash cache for incremental re-sync

## What it proves

The actuator can move load from a public-ontology boundary: facts in the `.ttl`
(`E0011`, `template.values-inline`, the fixture, the three transports) are
precipitated through `ggen sync` into a Rust test artifact, and the receipt
proves the run produced it. This is the actuator-side complement to
GALL-INTEGRATION-1 (sensing parity) and `gall_sync_actuation` (honest refusal).

## Known limitation (honest)

The receipt's `input_hashes` currently covers `ggen.toml` only — not the
ontology `.ttl` or the external template file. Changing either changes the
output, so a complete receipt should hash them too. Tracked as a contract-drift
fix in `ggen sync` (ggen-core), separate from this actuation proof.
