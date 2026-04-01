# Pack query contract (μ₂ / v6 extraction)

**Version:** 1.0  
**Last updated:** 2026-04-01

The v6 **μ₂ extraction** pass is **CONSTRUCT-only**: it materializes IR triples into the working graph. Pack-contributed queries loaded from `<pack_cache>/<atomic-pack-id>/queries/*.rq` MUST follow the same contract as project tensor queries.

## Rules

1. **CONSTRUCT required** — Each file must be a SPARQL query containing a `CONSTRUCT` clause. `SELECT`, `ASK`, `DESCRIBE`, and `INSERT`/`DELETE` (update) are rejected by the μ₂ Andon gate.
2. **Execution order** — Pack queries run **after** built-in tensor queries from `ExtractionPass::with_standard_rules()`, in deterministic file order, with distinct `order` indices and unique synthetic target predicates to avoid unsafe parallel overlap.
3. **Graph effects** — CONSTRUCT results are inserted into the same `Graph` as project IR; packs must not rely on hidden global state outside that graph.
4. **Naming** — Internally, pack queries are registered as `pack::<atomic-pack-id>::<file-stem>` for receipts and debugging.

## Non-goals

- Pack `SELECT` queries that only populate JSON bindings (legacy pattern) are **not** supported in v6 μ₂; rewrite as `CONSTRUCT` that shapes IR triples, or keep logic in the project ontology + standard extraction rules.

## References

- Implementation: [crates/ggen-core/src/v6/passes/extraction.rs](../../crates/ggen-core/src/v6/passes/extraction.rs) (`extend_with_pack_construct_queries`).
- Loader: [crates/ggen-core/src/pack_resolver.rs](../../crates/ggen-core/src/pack_resolver.rs) (`get_pack_queries`).
