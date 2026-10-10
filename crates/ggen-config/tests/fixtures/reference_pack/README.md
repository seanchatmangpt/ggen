# reference_pack fixture

Minimal-but-complete reference `ggen.toml` for the DeclarativeRules schema
(v26.10.10 §4.1), exercising every top-level section a pack author needs.

- `[project]` / `[ontology]` — required; `source` points at `ontology.ttl`.
- `[[generation.rules]]` — inline SPARQL CONSTRUCT query + template pair
  driving code generation; `output_file` is the codegen output root path.
- `[rules]` — OPTIONAL references to external rule files:
  `n3` (executable) and `datalog` (schema-accepted, engine-refused).
  Distinct from `[law].rules` and `[[generation.rules]]`.
- `[pack_sources]` — OPTIONAL map, pack name -> `{ source, location }`,
  where `source` is `"path"` (local dir) or `"git"` (repo URL).

Copy-paste: duplicate this directory, edit `project.name`, the ontology,
and the generation rule. Keep or drop `rules.datalog`.

Refusal contract: a `.datalog` entry parses and validates fine, but the
sync engine refuses it with FM-LAW-019 (UNSUPPORTED) at execution time —
the schema layer never rejects the path itself.
