# CS2 projections

Deterministic multi-projection witness for `RFC-CS2-001`.

```
canonical.ttl
  -> queries/consumers.rq
  -> templates/consumers.json.tera             -> generated/consumers.json
  -> templates/consumers.exs.tera              -> generated/consumers.exs
  -> templates/fleet-contract.json.tera        -> generated/fleet-contract.json
  -> templates/marketplace-pack.ttl.tera       -> generated/marketplace-pack.ttl
  -> templates/ash-a2a-fleet-contract.ex.tera  -> generated/ash_a2a/generated_fleet_contract.ex
  -> templates/xaas-fleet-contract.ex.tera     -> generated/xaas/generated_fleet_contract.ex
  -> semantic-jira.rq
  -> templates/semantic-jira.json.tera         -> generated/semantic-jira.json
```

The canonical consumer relation now owns distribution identity as data:
`work_id + target_repository + target_path + artifact_kind + contract_version`.
That lets downstream repositories consume generated projections instead of
reconstructing CS2 consumer semantics locally.

The generated fleet JSON contract is schema-bound by
`schema/fleet-contract.schema.json`. The marketplace projection is an RDF pack
manifest over the same query rows. Ash A2A and XaaS receive generated Elixir
modules from the same source relation, so all three surfaces share the exact
`RFC-CS2-001` subject and `CONSTRUCT` authority ceiling.

All outputs are projections of the same exact subject and authority ceiling.
Generated output is not truth, authority, DO, admission, or standing.

The semantic Jira projection remains schema-bound by
`schema/semantic-jira.schema.json` and
`schema/semantic-jira-batch.schema.json`.

## Manufacture

From the repository root:

```bash
cargo run --quiet -p ggen-cli -- sync \
  --manifest examples/cs2-projections/ggen.toml
```

If `ggen` is already installed:

```bash
ggen sync --manifest examples/cs2-projections/ggen.toml
```

## Distribution contract

The generated fleet contract declares these current consumer targets:

- `CS2-WRK-003` → `seanchatmangpt/ggen-marketplace`
- `CS2-WRK-012` → `seanchatmangpt/ash_a2a`
- `CS2-WRK-013` → `seanchatmangpt/xaas`

Downstream integration should copy or package the generated artifact identified
for its repository and retire hand-maintained equivalents rather than editing the
generated projection.
