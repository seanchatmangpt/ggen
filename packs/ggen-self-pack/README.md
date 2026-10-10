# ggen-self-pack

The canonical constructor for ggen packs.

## Why this exists

Every pack under `packs/*` shares one shape: `pack.toml` + `ontology.ttl` +
`gates/*.rq` + `templates/*`. Until this pack existed, nothing enforced that
shape at creation time — each pack was hand-assembled, and a missing piece
(no gate, no ontology, an unclaimed namespace) was only ever discovered by
someone scanning existing packs afterward, not prevented at construction.

This pack's ontology (`ontology.ttl`) describes pack structure itself —
`sp:Pack`, with `sp:name`/`sp:description`/`sp:namespace`/`sp:hasTemplateRole`
— so a future structural requirement on packs (e.g. "packs must declare
ownership") is one edit to this ontology plus this pack's own templates, not
new Rust code, and is inherited by every pack created after that edit. This
mirrors the project's `ggen-first: generate, don't hand-write` principle,
applied recursively to pack creation itself.

## Usage

```bash
# One-time per project: materialize this pack into a project-local,
# hand-editable copy (the copy `pack new` actually reads).
ggen init-self

# Create a new pack. --description and --namespace are required; --version
# and --category default to 0.1.0 / uncategorized.
ggen pack new my-new-pack \
  --description "What this pack generates and why" \
  --namespace "http://seanchatmangpt.github.io/packs/my-new-pack#"
```

`ggen pack new` writes one `sp:Pack` individual to `input.ttl`, binding the
CLI args, then runs this pack's own `ggen sync` pipeline exactly like any
other project — no bespoke logic in the CLI verb beyond that binding step.
`packs/<name>/pack.toml`, `ontology.ttl`, `gates/010_required.rq`, and
`README.md` land under `packs/<name>/`, and a `.ggen-v2/receipt.json` is
chained for the run exactly as `ggen sync run` produces for any project.

## Structurally-correct-by-construction

`gates/010_required_shape.rq` (a SPARQL SELECT gate — any returned row is a
violation, per this repo's `[law].gates` convention) refuses to generate a
pack whose `sp:Pack` individual lacks a name, description, namespace, or at
least one declared template role. This is not a post-hoc lint: `ggen sync`
fails closed (Andon-style, non-zero exit) before any file is written, so
"no pack without an ontology namespace" is a guarantee of the constructor,
not a rule someone has to remember to check afterward.

## Files

| Path | Purpose |
|---|---|
| `ontology.ttl` | The `sp:` vocabulary — what a Pack *is* |
| `input.ttl` | Where `ggen pack new` writes the one `sp:Pack` individual for this run |
| `queries/pack_new.rq` | Projects the bound `sp:Pack` individual's scalar fields |
| `gates/010_required_shape.rq` | Refuses an incomplete `sp:Pack` individual (including zero declared template roles) before any write |
| `templates/*.tmpl` | Render `packs/<name>/{pack.toml,ontology.ttl,gates/010_required.rq,README.md}` |
| `ggen.toml` | Wires the above into the ordinary five-stage sync pipeline |
