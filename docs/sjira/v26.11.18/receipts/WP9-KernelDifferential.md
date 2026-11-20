# WP9 KernelDifferential
Standing: VERIFIED

Mirror of the fnox sjira receipt for the cross-kernel admit gate. Canonical
long form: `docs/jira/v26.11.18/receipts/wp9-kernel-differential.md` and
`/Users/sac/fnox/docs/sjira/v26.11.18/receipts/WP9-KernelDifferential.md`.

## Evidence (2026-11-19)

- Mutual admit digests: `sha256:94e00b6ed077be2d5e9e8b799ccbc480d0a1d5350b01ee86b15118b951ae4184`
- `cargo test -p ggen-sjira --test projection_test` → 12, EXIT=0
- `cargo test -p ggen-sjira --test shared_fixture_admission_test` → 1, EXIT=0
- `cargo test -p ggen-sjira --test work_order_admission_test` → 15, EXIT=0
- `projection_test` `order()`: `falsifiers: []`, `path_scope: ["lib/"]`
