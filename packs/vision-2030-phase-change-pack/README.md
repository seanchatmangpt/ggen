# vision-2030-phase-change-pack

**Status: ORPHANED — zero consumers.** This pack is not wired into any `ggen.toml`
generation rule, any Rust code path, or any CI workflow, anywhere in this repository.

## Evidence (verified 2026-08-03)

- `find packs/vision-2030-phase-change-pack -type f` returns only `pack.toml`,
  `catalog/vision-2030-capabilities.json`, `catalog/vision-2030-maximalist-capabilities.json`,
  `schema/vision-2030-maximalism.schema.json`, `schema/vision-2030-program.schema.json`, and
  `templates/vision-2030-report.md.tera` — no `ontology.ttl`, no `queries/*.rq`, no `ggen.toml`.
- `grep -rl "vision-2030-phase-change-pack" --include="ggen.toml" .` (repo-wide) returns only
  this pack's own directory is absent — **zero** `ggen.toml` files anywhere reference it.
- `grep -rl "vision-2030-phase-change-pack" crates/` (repo-wide, all Rust source) now returns
  **one** file: `crates/ggen-config/tests/vision_2030_pack_orphan_test.rs` — a governance
  regression test (added after this note was first written) that asserts this pack stays
  orphaned. It excludes itself from its own assertion (it necessarily names the pack in prose)
  and still finds **zero** *other* Rust consumers, so the orphan status this README documents
  is unchanged; only the literal grep-output count above was stale.

## Do not confuse this with the live `ggen vision2030` CLI

There is a real, live, tested `ggen vision2030` command surface at
`crates/ggen-cli/src/cmds/vision2030/` (see `mod.rs`, `evaluation.rs`, `receipts.rs`,
`tests.rs`). It is a **separate, independent implementation** from this pack:

- The live CLI's `inspect`/`validate`/`roadmap`/`blue_ocean`/`dx`/`qol`/`doctor`/`receipt`/
  `replay` verbs all take a `manifest: String` CLI argument — a path to a JSON file matching
  the `Manifest` struct in `crates/ggen-cli/src/cmds/vision2030/mod.rs`, whose required
  `schema` field must equal the constant `MANIFEST_SCHEMA = "ggen.vision2030.program.v1"`.
- This pack's `schema/vision-2030-program.schema.json` **does** carry that same
  `"schema": {"const": "ggen.vision2030.program.v1"}` — so that one file is at least
  schema-compatible in principle.
- However, this pack's actual data files —
  `catalog/vision-2030-capabilities.json` and `catalog/vision-2030-maximalist-capabilities.json`
  — declare `"schema": "ggen.vision2030.catalog.v1"` (a **different** schema), are missing the
  live `Manifest`'s required `program`/`required_domains`/`horizons` fields entirely, and use a
  `depends_on` key on each capability where the live code's `Capability` struct requires
  `dependencies` plus an `evidence` map. **Neither catalog file in this pack can be passed
  as-is to the live `ggen vision2030` CLI as a `--manifest` argument; it will fail to parse.**
- The document `docs/architecture/VISION-2030-PHASE-CHANGE-ARD-PRD-v26.8.3.md` lists this
  pack's schema/catalog files in its "Implementation map" (§9) alongside the live CLI files,
  which can read as though they cooperate today. As of this note, they do not: nothing in the
  live CLI reads from this pack, and this pack's catalog JSON is not a valid input to it.

## If you are picking this pack back up

Before wiring it in, decide (and update this README when you do):

1. Either make the catalog files conform to the live `Manifest` schema
   (`ggen.vision2030.program.v1`, with `program`/`required_domains`/`horizons`/`dependencies`/
   `evidence`), or make the live CLI accept the pack's `ggen.vision2030.catalog.v1` shape.
2. Add a `ggen.toml` (or a generation rule in an existing one) that actually consumes
   `templates/vision-2030-report.md.tera` against real query results, following the pattern
   used by other packs with their own consumer `ggen.toml` (e.g.
   `packs/fortune5-testing-bblock-pack/ggen.toml`).
3. Remove or update the orphan notice in this file once a real consumer exists.

Per this repo's Evidence-First principle, no claim above is asserted without a citable
file or command; re-run the greps in "Evidence" above if this file's claims might have gone
stale.
