# ggen-engine

The live code-generation engine behind [`ggen sync`](https://github.com/seanchatmangpt/ggen):
RDF/SPARQL ontologies in, Tera-rendered code out, with BLAKE3 receipt chains over every
sync run.

## Five-stage pipeline

`ggen sync` runs a sequential five-stage pipeline
(`crates/ggen-engine/src/sync.rs`), each stage carrying its own OpenTelemetry span
(`pipeline.load`, `pipeline.extract`, `pipeline.validate`, `pipeline.generate`,
`pipeline.emit`):

1. **Load** — load the manifest (`ggen.toml`), ontology, and referenced packs.
2. **Extract** — run SPARQL SELECT/CONSTRUCT queries over the RDF graph.
3. **Validate** — law/admission checks over extracted facts.
4. **Generate** — render Tera templates from query results.
5. **Emit** — write generated files; mint the sync receipt (`.ggen-v2/receipt.json`).

## Graph backend

The default graph backend is the [`graphlaw`](https://crates.io/crates/graphlaw) crate
(registry, pinned in the workspace), replacing the retired vendored `praxis-core`/
`praxis-graphlaw` members (SJIRA-15, 2026-10-10).

## Admission gate

`ggen-abb-sbb` is wired in as the pipeline's Stage 2c admission kernel: IO-free
ABB/SBB `admit()`/`plan()`/`depgraph` with transitive closure, cycle, and unbound-port
refusals.

## Related crates

- [`ggen`](https://crates.io/crates/ggen) — CLI that routes `sync`/`doctor`/`graph`/
  `receipt` here.
- `ggen-config` — the `ggen.toml` manifest schema.
- `ggen-marketplace` — pack registry/lockfile.

## License

MIT. See [LICENSE-MIT](../../LICENSE-MIT) at the repository root.
