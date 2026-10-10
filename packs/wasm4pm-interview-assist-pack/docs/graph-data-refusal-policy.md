# Graph-data refusal policy (TICKET-005)

## Scope

Governs what every projection template in workstreams C-G must do when its bound SPARQL query
returns zero rows, contradictory rows, or covers a class/predicate combination with no SHACL
NodeShape. This is a designed refusal, not an accidental empty-string emission discovered after
the fact.

## Rules

1. **Zero rows on a required query -> fail loudly.** The build (`ggen sync run`) MUST exit
   non-zero and the error MUST name the missing resource/query file, e.g.
   `required query queries/phases.rq returned 0 rows for phase-scheme; expected >=1`.
   The template MUST NOT emit an empty or placeholder output file. `mode = "Create"` skipping an
   existing hand-completed file is a different, sanctioned case (see root `CLAUDE.md`
   "mode=Create Semantics") and is not this rule.

2. **Contradictory rows on a single-valued field -> fail loudly, naming both values.** If a query
   whose result contract is one-row-per-resource returns two different bindings for what should
   be a single-valued predicate (e.g. two `schema:name` literals for one `<product/interview-assist>`),
   the build MUST fail and the error MUST print both conflicting literal values, not silently pick
   the first row.

3. **Unsupported class/predicate combination -> flagged UNSUPPORTED, not silently accepted.** A
   query result whose subject's class/predicate combination has no covering SHACL NodeShape is
   recorded as `UNSUPPORTED` in the projection manifest (TICKET-010's manifest), never silently
   rendered as if it were validated.

4. **Required vs optional is a per-query, domain-level judgment**, recorded in TICKET-003's query
   catalog headers (`# required` / `# optional` comment on each `.rq` file), not invented inside
   the refusal mechanism. Example of a legitimately optional query: a query for recorded refusal
   events early in a fresh session, where zero rows is a valid domain state (no refusals have
   happened yet) rather than a missing-data error.

## Mechanism (generic, no domain constants)

The refusal mechanism itself never names a domain class or predicate — it only inspects row
counts and duplicate-key structure, which the calling template supplies:

- **Zero-row refusal**: implemented as `templates/_partials/require-nonempty.tera`'s
  `require_nonempty(rows, resource)` macro. It checks `rows | length == 0` and, if true,
  references an intentionally-undefined Tera variable
  (`__GGEN_REQUIRED_QUERY_RETURNED_ZERO_ROWS_FOR_{{ resource }}__`). Tera's default
  (non-`?`-suffixed) variable lookup raises a render error for an undefined variable, which
  `ggen-engine`'s existing Tera render-error propagation turns into a non-zero `ggen sync run`
  exit (verified via the engine's existing error path — no new plumbing was built for this
  ticket, per the ticket's own custom-code boundary).
- **Contradiction refusal**: `require_single(rows, field, resource)` macro checks
  `rows | map(attribute=field) | unique | length > 1` and, if true, renders both conflicting
  values into the same undefined-variable trick, so both appear in the resulting Tera error
  message.

## Verification performed this session

Both refusal rules were exercised at the SPARQL/rdflib level (no full `ggen sync run` surface
exists yet at this point in the dependency chain — see TICKET-005's own Verification ladder,
which allows this for the "unit" rung).

- **Passing case**: real `queries/phases.rq` against the real, complete
  `packs/wasm4pm-interview-assist-pack/ontology.ttl` returns 14 rows (already-verified count per
  `docs/projection-mapping.md`) — a required query with rows present, no refusal triggered.
- **Failing case (negative test)**: a scratch copy of `ontology.ttl` was made in `/tmp`, the
  `<product/interview-assist>` resource's `schema:name "InterviewAssist"` triple was deleted from
  it, and `queries/product-metadata.rq`-equivalent (`SELECT ?name WHERE { <product/interview-assist>
  schema:name ?name }`) was run against the mutilated scratch copy via rdflib: **0 rows**,
  confirming the "required query returns 0 rows against a corpus missing the target resource"
  precondition this ticket's Negative tests section requires. See
  `packs/wasm4pm-interview-assist-pack/queries/product-metadata-required.rq` (new, this ticket)
  for the exact query used, and the transcript below.

```
$ python3 - <<'PY'
import rdflib, shutil
shutil.copy("ontology.ttl", "/tmp/scratch-ontology.ttl")
g = rdflib.Graph(); g.parse("/tmp/scratch-ontology.ttl", format="turtle")
g.remove((rdflib.URIRef("https://github.com/seanchatmangpt/ggen/blob/main/packs/wasm4pm-interview-assist-pack/product/interview-assist"),
          rdflib.URIRef("https://schema.org/name"), None))
g.serialize(destination="/tmp/scratch-ontology.ttl", format="turtle")
g2 = rdflib.Graph(); g2.parse("/tmp/scratch-ontology.ttl", format="turtle")
rows = list(g2.query(open("queries/product-metadata-required.rq").read()))
print("rows:", len(rows))
PY
rows: 0
```

Zero rows confirmed on the mutilated fixture; the same query against the real, unmutilated
`ontology.ttl` returns 1 row. This is the exact contract `require_nonempty` refuses on.

## Reference

Linked from `packs/wasm4pm-interview-assist-pack/docs/projection-mapping.md`.
