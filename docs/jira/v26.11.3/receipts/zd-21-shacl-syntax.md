# ZD-21 — SHACL Shapes Turtle Syntax Fix

## Subject

- Repo: `/Users/sac/ggen`, branch `v26-11-3-wave`
- Failing test (ZD-14 disclosed pre-existing red):
  `book/tests/test_gap_closure_chicago.py::TurtleCorpusStateTests::test_active_turtle_is_valid_and_archive_quarantine_is_exact`
- Failing file: `examples/industry/manufacturing-dflss/generated/shacl-manufacturing-shapes.ttl`
  (`Bad syntax (Prefix "https:" not bound)` at line 8)

## Root cause (GGM-SRC-002: fix the source, not the artifact)

The generator template
`/Users/sac/ggen-marketplace/packs/industry-manufacturing-dflss/templates/shacl-manufacturing-shapes.ttl.tmpl`
emitted SPARQL result IRIs bare, without Turtle `<...>` delimiters:

    sh:targetClass {{ r["targetClass"] }} ;

which rendered `https://...#WorkOrder` un-delimited. A Turtle parser tokenizes
`https` as a prefixed-name prefix, which is unbound → the reported parse error.

## Fix

Wrapped all three emitted IRI columns in angle brackets in the template
(generator input; the generated file was NOT hand-patched):

    sh:targetClass <{{ r["targetClass"] }}> ;
    sh:path <{{ r["requiredPath"] }}> ;
    sh:class <{{ r["requiredClass"] }}> ;

## Regeneration

    cd /Users/sac/ggen/examples/industry/manufacturing-dflss && ggen sync run
    → exit 0; decisions: generated/shacl-manufacturing-shapes.ttl "written";
      generated/client-walkthrough.md "skipped: unchanged: content identical";
      actuator ggen@26.10.11; graph_hash d2497d51...

## Verification

- Direct rdflib parse of the regenerated file: OK, 21 triples.
- `python3 -m pytest book/tests/test_gap_closure_chicago.py::TurtleCorpusStateTests::test_active_turtle_is_valid_and_archive_quarantine_is_exact -q`
  - run 1: `1 passed in 5.03s`
  - run 2: `1 passed in 4.84s` (green ×2)
- Full file suite: `4 passed, 1 skipped in 3.02s` (skip pre-existing).

## Standing

ALIVE — exact subject regenerated and test-verified twice this session.
