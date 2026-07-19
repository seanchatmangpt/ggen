# Consumer wiring for wasm4pm-facts-pack

## Baseline (unchanged): add the pack

```toml
[packs]
wasm4pm-facts-pack = { path = "../../packs/wasm4pm-facts-pack" }
```

`ggen sync run` then generates, into the consumer project:

- `docs/releases/v26.7.6/BREED_ALGORITHM_REGISTRY.md` (SPARQL-projected report)
- `src/wasm4pm_facts_registry.rs` (typed `BREEDS`/`ALGORITHMS` catalog + `lookup_breed`/`lookup_algorithm`/`algorithms_by_category`)
- `tests/wasm4pm_facts_pack_registry_proof.rs` (spot-check proof, 8 individuals)
- `tests/wasm4pm_facts_pack_full_coverage_proof.rs` (mechanical proof, all 115 individuals)
- `src/wasm4pm_facts_lib_wiring.rs` -- **not** injected into the consumer's `src/lib.rs` directly. An earlier version of this template used `to: src/lib.rs` with `inject: true`, which works for exactly one pack in isolation but hits this engine's own `FM-WRITE-008` duplicate-output guard the moment a second pack (e.g. `wasm4pm-cognition-pack`) targets the same file in the same real consumer -- confirmed live in `examples/receiptctl`, which wires both. The template now targets this pack-uniquely-named file instead. **The consumer must add one line by hand** to their own `src/lib.rs`:

  ```rust
  include!("wasm4pm_facts_lib_wiring.rs");
  ```

  This mounts `mod wasm4pm_facts_registry;` (the file's sole content) without colliding with any other pack's own wiring file. This is a real, disclosed, non-zero consumer-effort step -- not the "consumer wires `ggen.toml`, done" bar of L5.

## Optional: activate law-derived breed standing

The pack bundles `ontology/rules/breed_standing.n3`, an N3 rule that
CONSTRUCT-derives `law:standing "EvidenceBound"` for any
`compat:CognitionBreed` that carries a `compat:citation` (all 55 do). To
activate it, add to the **consumer's own** `ggen.toml`:

```toml
[law]
rules = ["../../packs/wasm4pm-facts-pack/ontology/rules/breed_standing.n3"]
```

This is a genuine consumer-effort gap this pack cannot fully close by
itself: `[law]` is a top-level table on the consumer's `ggen.toml`
(`ggen_engine::config::GgenConfig::law` / `ggen_config::manifest::GgenManifest`
-- see `.claude/rules/architecture.md`'s "ggen.toml has two schemas" note),
not something any pack template can inject into a sibling file via
`ggen-engine`'s current `inject`/`skip_if` machinery (that machinery only
inserts raw text at a marker in an existing file -- it cannot merge TOML
tables). Until `ggen-engine` gains a pack-declared-law-requirement
mechanism (an engine-level change outside this pack's own files), a human
must add this one-line `[law]` table by hand. Without it, `ggen sync` still
succeeds and every breed row renders the `ADMITTED` placeholder rather than
failing loudly -- this is a known, named, unclosed gap (see
`docs/packs/L5_VALIDATION_REPORT.md`'s "Consumer effort" finding for this
pack), not a silent one: the registry template's own header comment and this
file both say so.

## Verifying the shapes

```bash
ggen graph validate --files packs/wasm4pm-facts-pack/ontology.ttl \
  --shapes packs/wasm4pm-facts-pack/shapes.ttl
```

## Verifying upstream fidelity

```bash
packs/wasm4pm-facts-pack/scripts/check_upstream_drift.sh ~/wasm4pm
```

See `DRIFT_LOG.md` for the dated history of this check.
