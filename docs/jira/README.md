# Jira-style implementation programs

This directory indexes versioned implementation backlogs and migration programs maintained as repository-native Markdown.

| Version | Program | Standing | Scope |
|---|---|---|---|
| [`v26.7.16/`](./v26.7.16/) | ggen-core replacement and consolidation | Historical implementation program | Migration from the retired `ggen-core` path to the current ggen-engine/Praxis architecture. Individual files record their own completion or supersession state. |
| [`v26.7.23/`](./v26.7.23/) | InterviewAssist admitted-ontology implementation backlog | PLANNED | 57 deterministic tickets derived from the public-ontology PRD/ARD corpus in `packs/wasm4pm-interview-assist-pack/`. No ticket implementation is included in the backlog PR. |

## Standing law

A directory entry is an index, not proof that its program is complete. Read the program's own overview or epic and the receipts named there before assigning `ALIVE` standing.

## InterviewAssist handoff

The InterviewAssist sequence is:

```text
admitted public-ontology RDF
→ deterministic tickets
→ SPARQL selection
→ ggen templates
→ projected canonical source
→ thin custom adapters
→ Chicago-TDD verification
→ receipts and replay
```

Projected code is first-class source. No `generated/` directory is introduced by this backlog.
