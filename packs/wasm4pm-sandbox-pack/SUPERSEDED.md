# Superseded InterviewAssist sandbox prototype

## Standing

`SUPERSEDED`

This pack is retained only as reviewable history of the first Phase 0 sandbox-catalog experiment. It models compile, execute, and test operations through the private `sbx:` vocabulary.

That modeling choice was explicitly rejected for the canonical InterviewAssist specification. The admitted replacement is:

```text
packs/wasm4pm-interview-assist-pack/
```

The replacement represents the PRD/ARD with existing public ontologies and supplies the domain source for `docs/jira/v26.7.23/`.

## Allowed use

- Review the earlier catalog and generated-port experiment.
- Reuse generic implementation techniques only after deriving them from the public-ontology graph.
- Compare prototype behavior against later ticket implementations.

## Excluded use

- Do not treat `sbx:SandboxCapability` or any `sbx:` predicate as admitted InterviewAssist domain truth.
- Do not add this ontology to the canonical InterviewAssist union graph.
- Do not generate TICKET-001 through TICKET-057 outputs from this pack.
- Do not extend the private vocabulary.

## Disposition

Archived in place rather than deleted because the implementation experiment remains useful evidence and is already coupled to historical receiptctl fixtures. The explicit supersession marker prevents it from silently competing with the public-ontology corpus.
