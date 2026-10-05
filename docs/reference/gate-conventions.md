# Gate Conventions

Two gate-scoring conventions coexist across the ggen ecosystem. This page
reconciles them so pack authors do not silently invert a gate by copying a
query between runtimes.

## ggen `[validation].gates` — offender-reporting (violation rows)

In `ggen sync`, every query listed under `[validation] gates` in
`ggen.toml` is evaluated post-inference, before any file write. A gate
**fails** when its `SELECT` returns **>= 1 row**: each row is an offender,
a concrete violation the query surfaces. A gate **passes** on exactly zero
rows. Multi-row results are fine; each row becomes a violation entry in the
refusal (`REFUSED:*` standing, `andon_signal` in the sync JSON). A
`# MESSAGE:` comment header on the query supplies the human-readable
violation text.

Because rows mean "violation", the queries are written
offender-reporting style: bind a row for each thing that is wrong, using
`FILTER NOT EXISTS`, negation, or aggregate HAVING clauses.

## ggen_igniter `gates/` vs `verify/` — directory-is-convention

ggen_igniter (ADR 0010:
`docs/architecture/adr/0010-gate-convention-directory-is-convention.md`)
splits the corpus by directory, and the directory IS the scoring:

| Directory | Orientation | PASS condition | Scoring |
|---|---|---|---|
| `gates/*.rq` | witness-reporting | >= 1 row | `GateVerify` |
| `verify/*.unbound.rq` | offender-reporting | 0 rows | verify task + `cardinality.json` |

`gates/*.rq` are witness-reporting: rows are witnesses the ontology must
produce, so **>= 1 row = PASS**, 0 rows = FAIL. `verify/*.unbound.rq` are
offender-reporting: rows are violations, **0 rows = PASS**, >= 1 row =
FAIL, scored through the pack's `verify/cardinality.json` contract.

Known residual hole (accepted in ADR 0010): an offender-shaped query
misfiled in `gates/` scores `:pass` exactly when the ontology is broken.
Nothing types a gate; misfiling is silent. Nothing in the directory
convention changes how ggen scores `[validation].gates`.

## Rule for pack authors

In `ggen sync`, ALL gate queries in `[validation].gates` are
violation-row (offender-reporting) orientation — no exceptions, no
per-gate flag. ggen_igniter's witness-reporting `gates/*.rq` files MUST
NOT be copied into a `[validation].gates` list without re-orientation.

A witness query scores PASS on >= 1 row; a ggen gate scores FAIL on
>= 1 row. Copied verbatim into `[validation].gates`, a healthy ontology
produces witnesses, every witness row is read as a violation, and the
sync is refused — the gate fails closed. Worse, when the ontology is
broken and produces no witnesses, the copied gate scores PASS: it fails
open on exactly the broken case. Re-orient instead: negate the query
(bind rows only when the witness is absent), or move it to igniter's
`verify/` with a cardinality contract.
