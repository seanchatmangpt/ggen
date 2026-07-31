# Source precedence

When repository sources disagree, the v26.8.1 program resolves authority in this order:

1. enforced constitutional rules and CI guards;
2. active Cargo workspace and exact dependency graph;
3. admitted ontology, manifest, schema, and policy loaded by production code;
4. production implementation and generated source at the exact head;
5. executable tests, verifiers, negative fixtures, and real-boundary evidence;
6. receipts and replay evidence bound to that head;
7. generated documentation and indexes;
8. manually authored narrative;
9. historical snapshots and git history.

Historical material remains necessary for Chesterton analysis but cannot override active behavior. Conversely, active behavior cannot erase a historical capability without recording its disposition and migration consequence.

## Real evidence for rung 3 ("admitted ontology, manifest, schema, and policy loaded by production code")

`ggen.toml` — this repo's own top-level project manifest format — is a direct, checkable
counter-example to treating "the manifest schema" as one authority: it is parsed by **two**
independently hand-written struct hierarchies with no shared type and no automated cross-check
until this pass. `ggen_engine::generation_rules::has_generation_rules`
(`crates/ggen-engine/src/generation_rules.rs`) does a raw-text pre-parse to decide which typed
parser runs: `ggen_config::manifest::types::GgenManifest` (this pass's owned crate,
`crates/ggen-config`) if `[[generation.rules]]` is present, else
`ggen_engine::config::GgenConfig`. Both declare `[project]`/`[ontology]`/`[packs]`/`[templates]`/`[law]`
tables with genuinely divergent internal shapes (flat array-of-tables `PackRef` vs. an untagged
enum table-of-tables `PackRef`). `crates/ggen-config/tests/schema_parity_test.rs` (pre-existing,
not added this pass) is the one enforced guard against silent drift between the two — it parses
both structs' real source with `syn` and diffs their shared field names against a frozen
constant. This is rung 3's authority claim made concrete: production code, not this doc, decides
which schema wins for a given `ggen.toml`, and only a test (rung 5) currently keeps the two
schemas from silently diverging further.
