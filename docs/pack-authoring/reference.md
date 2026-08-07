# Reference: ggen pack file layout

| Path | Required | Contract |
|---|---|---|
| `pack.toml` | yes | `[pack]` table: `name`, `version` (semver), `description`. No other top-level tables. |
| `ontology.ttl` | yes | Turtle. One or more `@prefix` declarations plus RDF classes/individuals — the pack's facts, unioned with every other pack's and the consumer's own `ontology.ttl` at sync time. |
| `templates/*.tmpl` | at least 1 | YAML frontmatter (`---`/`---`) then a Tera body. Frontmatter: `to:` (output path, project-relative, may interpolate `{{ var }}` from a SPARQL row) and either `sparql.row:` (exactly one row expected) or `sparql.rows:` (zero or more rows, looped with `{% for row in rows %}`). |
| `gates/*.rq` | no | SPARQL `SELECT`. Non-empty result set refuses the sync before Extract/Render/Write. Files run in filename sort order (`010_`, `020_`, ... convention). |

## Template `to:` fan-out rule

If `to:` contains a `{{ variable }}`, the engine writes **one output file
per query row** — a query returning N rows for the same resolved `to:`
path is a collision (`FM-WRITE-008`), not an aggregation. To collapse
multiple facts into one file per key, `GROUP BY` the key and
`GROUP_CONCAT` the varying field in the SPARQL — see
`resource_doc.tmpl`/`backend_catalog_doc.tmpl` in `gh-terraform-pack` for
the real, working idiom.

## Gate file shape

```sparql
# MESSAGE: <human-readable statement of the invariant, surfaced verbatim
# in the refusal error>
PREFIX <prefix>: <...>
SELECT ?s ?missing WHERE {
  { ?s a <Class> . BIND(<requiredProp> AS ?missing) } UNION { ... }
  FILTER NOT EXISTS { ?s ?missing ?any }
}
ORDER BY ?s ?missing
```

## Chicago-TDD test shape

One `tests/<pack>_pack_e2e.rs` per pack, using
`crates/ggen-engine/tests/support::{scaffold_pack, assert_idempotent,
assert_gate_refuses}` — see `how-to-write-a-chicago-e2e-test.md`.
