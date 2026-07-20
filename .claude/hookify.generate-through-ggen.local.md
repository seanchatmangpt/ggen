---
name: generate-through-ggen
enabled: true
event: file
conditions:
  - field: file_path
    operator: regex_match
    pattern: (docs/packs/(PACK_MATURITY_MODEL|TCPS-STATUS)\.md|\.claude/rules/architecture\.md|book/src/|examples/[^/]+/src/.*\.rs$|examples/[^/]+/infra/|\.claude/(agents|skills|workflows)/|VERIFICATION\.md$|EVIDENCE_SNAPSHOT\.md$)
---

⚠️ **This path is a ggen-GENERATED surface — ggen is the generation engine, not the Write tool.**

You are hand-writing a file that is (or is becoming) a projection of RDF:

- `docs/packs/PACK_MATURITY_MODEL.md` / `TCPS-STATUS.md` → edit `.specify/maturity.ttl`, then root `ggen sync run`
- `.claude/rules/architecture.md` → edit `.specify/repo-facts.ttl`, then root `ggen sync run`
- `book/src/**` → edit `packs/level-five-book-pack/ontology.ttl`, then sync the `book/` consumer
- `examples/*/src/*.rs`, `examples/*/infra/**` → edit the owning pack's ontology/templates, then re-sync the example
- `.claude/agents|skills|workflows/**` → edit `packs/claude-code-pack/ontology.ttl`, then sync its consumer

Hand-edits to these files are drift by construction: the next `ggen sync` clobbers them (force: true), and until then the file lies about its ontology. If this edit is legitimately part of BOOTSTRAPPING a new generated surface (writing the template/marker scaffolding itself, or a one-time migration), proceed — but say so explicitly in your narration so the exception is visible, and make the ontology the authority before the round ends.
