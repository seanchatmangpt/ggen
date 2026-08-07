# Explanation: why packs are shaped this way

## RDF is the source of truth, not the generated files

`ggen`'s core discipline (see the repo's own `CLAUDE.md`: "RDF is Truth —
edit `.specify/*.ttl`, never the generated `.md`") extends to every pack: an
`ontology.ttl` fact is what's real; a `.tf`/`.md`/`.rs` file under `docs/`
or `infra/` is a *projection* of that fact, regenerated on demand. If a
generated file says something the ontology doesn't, the file is wrong, not
the ontology — this is why `ggen sync` is idempotent and why a second sync
producing a diff is treated as a bug, not a feature.

## Templates echo, they never synthesize

Every template in this pack (and in `gh-terraform-pack`, which this
convention is modeled on) either echoes a literal `hclBody`/`content`
string already stored in the ontology, or fills a small number of named
holes (`{{ name }}`, `{{ className }}`) from a spec individual's own
properties. No template computes new content the ontology doesn't already
assert. This is a deliberate constraint, not a missing feature: it keeps
"what will this pack generate" answerable by reading the RDF, without
running the template engine to find out.

## Gates are admission control, not test assertions

A `gates/*.rq` file runs at sync time, against real consumer data, and
refuses BEFORE any file is written — it protects every future sync, not
just the one your test happens to run. A Chicago-TDD test then separately
PROVES the gate fires (via `assert_gate_refuses`), because a gate that
silently never matches anything is indistinguishable, by inspection alone,
from a gate that works — the same "decorative completion" risk this repo's
`coding-agent-mistakes.md` names for any patch.

## Why a shared test harness, not one bespoke scaffold per pack

`crates/ggen-engine/tests/support/mod.rs` exists because
`gh_terraform_pack_e2e.rs` and the older `pack_e2e.rs` had independently
reinvented the same `TempDir`/`copy_tree`/scaffold code. Two copies of the
same logic drift the moment one is edited and the other isn't — the
`coding-agent-mistakes.md` "reduce drift" half of this repo's strongest
rule. One shared, tested implementation is the version every new pack's
`<pack>_pack_e2e.rs` should build on, including the ones this pack itself
scaffolds.
