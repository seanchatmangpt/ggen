# zero-code-sjira-hands-free

Minimal clap-noun-verb six-pack consumer (packs pinned to `~/ggen-marketplace`)
whose `cnv:CustomBehavior` handlers shell out to real `ggen sjira` for
admit / frontier / execute. Domain logic stays in `src/custom_handlers.rs`
only; `generated_*` files remain thin GENERATED shells. Closed
`system ping` stays on the six-primitive path (no Rust). CustomBehavior is
only for that irreducible subprocess I/O.

## Handlers

| Command | Handler | Spawns |
|---|---|---|
| `issue admit <file>` | `src/custom_handlers.rs::issue_admit` | `ggen sjira admit <file>` |
| `system frontier --orders-path …` | `src/custom_handlers.rs::system_frontier` | `ggen sjira frontier …` |
| `system execute --orders-path … --ledger-path … --authority-path …` | `src/custom_handlers.rs::system_execute` | `ggen sjira execute …` |

Override binary:

- `GGEN_BIN=/path/to/ggen` → `{GGEN_BIN} sjira <verb> …`
- `SJIRA_BIN=/path/to/ggen-sjira` → `{SJIRA_BIN} <verb> …` (no `sjira` noun prefix)

## EA admission

No `ea.graph.json` — sync with the documented opt-out:

```sh
ggen sync run --skip-ea-admission
```

Sibling `zero-code-sjira` ships an empty EA graph so plain `ggen sync run` works
and uses `project frontier` / `project execute` nouns.

## Sync / build / test

```sh
cd examples/zero-code-sjira-hands-free
ggen sync run --skip-ea-admission
cargo test
cargo run -- issue admit fixtures/order.json
cargo run -- system frontier --orders-path fixtures/orders.json
```
