# Committed GitHub enterprise consumer

This clean consumer composes both GitHub architecture layers:

- `infra/terraform/github/` — the existing institutional self-management pack;
- `infra/terraform/github-enterprise/` — the reusable multi-repository factory.

Run from this directory:

```bash
ggen sync run
ggen sync run
```

The second sync must be byte-identical. Generated outputs are intentionally not
committed here; the example is a reproducible consumer boundary rather than a
second authority for generated files.
