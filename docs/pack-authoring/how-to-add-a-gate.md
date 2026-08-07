# How to: add an admission gate

A gate is a `gates/*.rq` SPARQL `SELECT` that returns rows for every
violation. A non-empty result set refuses the sync — before Extract,
Render, or Write ever run, so a refused sync leaves no partial output,
`ggen.lock`, or receipt on disk.

## The required-properties idiom

Every existing pack (`gh-terraform-pack`, `praxis-core-pack`, ...) uses this
exact shape for `gates/010_required.rq`:

```sparql
# MESSAGE: every hp:Greeting must have a non-empty hp:text. Any row = a
# subject missing its required property.
PREFIX hp: <http://example.org/hello-pack#>
SELECT ?s ?missing WHERE {
  {
    ?s a hp:GreetingClass . BIND(hp:text AS ?missing)
  }
  FILTER NOT EXISTS { ?s ?missing ?any }
}
ORDER BY ?s ?missing
```

- The leading `# MESSAGE:` comment is not decorative — it is surfaced
  verbatim in the refusal error, so write it for the human who trips the
  gate, not for yourself.
- One `UNION` branch per (class, required-property) pair. Add a branch, not
  a new file, as your ontology grows — one `010_required.rq` per pack keeps
  the "what must every individual carry" contract in one place.
- Gates run against the UNION graph (this pack's facts plus every other
  pack's plus the consumer project's own `ontology.ttl`), so a violation can
  come from any source — the refusal message says so explicitly; don't
  narrow your gate's `FILTER` to assume only your own individuals exist.

## Policy gates (cross-individual, not just "is this field present")

A second gate file, conventionally `gates/030_policy.rq` (see
`gh-terraform-pack/gates/030_policy.rq`), expresses rules that reference
MORE than one individual — "critical-tier repos need >=2 reviewers",
"secrets must be vault-backed", etc. Same `SELECT ?s ?detail` /
non-empty-refuses shape; just a richer `WHERE` clause.

## Proving a gate actually fires

Documentation and structural review are not proof a gate refuses what it
claims to. Use `support::assert_gate_refuses` (see
`how-to-write-a-chicago-e2e-test.md`) to feed the gate a hand-crafted bad
fact and assert the sync is refused BY NAME — citing your gate's filename
fragment in the error.
