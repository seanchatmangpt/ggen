# zero-code-sjira

clap-noun-verb six-pack consumer (packs pinned to `~/ggen-marketplace`) for a
Semantic Jira CLI surface:

| Noun / verb | Behavior | Handler seam |
|---|---|---|
| `system ping` | `StaticJsonBehavior` | (none — GENERATED shell) |
| `system echo` | `EchoBehavior` | (none — GENERATED shell) |
| `system refuse` | `RefusalBehavior` | (none — GENERATED shell) |
| `issue admit` | `CustomBehavior` | `custom_handlers::issue_admit` |
| `project frontier` | `CustomBehavior` | `custom_handlers::project_frontier` |
| `project execute` | `CustomBehavior` | `custom_handlers::project_execute` |

Domain logic lives **only** in `src/custom_handlers.rs` (scaffolded once with
`unless_exists: true`, then ported over `ggen sjira`). Generated
`src/generated_*.rs` files are thin routing/behavior/boundary shells — they
must not grow process I/O or sjira kernel calls.

## EA admission (FM-LAW-015)

This example ships an empty but schema-valid `ea.graph.json` (same shape as
`ggen init`'s scaffold). That satisfies the fail-closed EA gate so plain
`ggen sync run` works without `--skip-ea-admission`.

Sibling contrast:

- `zero-code-inventoryctl` — fuller qualified EA graph
- `zero-code-sjira-hands-free` — no EA file; documents `ggen sync run --skip-ea-admission`

## Sync / build

```sh
cd examples/zero-code-sjira
ggen sync run
# equivalent opt-out if ea.graph.json is removed:
# ggen sync run --skip-ea-admission
cargo test
```

Expected artifacts after sync: `Cargo.toml`, `src/generated_*.rs`,
`src/custom_handlers.rs` (preserved if already present — domain bodies stay).
