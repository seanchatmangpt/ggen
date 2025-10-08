# Troubleshooting

- Missing output: check `to:` and matrix query.
- Unbound var: pass `--vars` or add `sparql.vars`.
- SHACL failure: fix data to satisfy shape.
- Nondeterminism: ensure matrix query has `ORDER BY` and seed is fixed.
- No writes: same `K`; use `--dry-run` to inspect.
