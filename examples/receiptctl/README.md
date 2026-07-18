# receiptctl — multi-pack ggen example

Combines **6 ggen packs**, spanning 3 real external projects, into one working CLI:

| Pack | Project | Generates |
|---|---|---|
| `clap-noun-verb-pack` | [clap-noun-verb](https://github.com/seanchatmangpt/clap-noun-verb) | `src/clap_noun_verb_routes.rs` — thin `#[verb]` CLI routes |
| `chicago-tdd-tools-pack` | [chicago-tdd-tools](https://crates.io/crates/chicago-tdd-tools) | `tests/chicago_tdd_tools_boundary.rs` — real subprocess CLI boundary proofs |
| `wasm4pm-algorithms-pack` | [wasm4pm](https://crates.io/crates/wasm4pm-compat) | `src/w4pm_algorithms_catalog.rs` — typed algorithm catalog |
| `wasm4pm-cognition-pack` | wasm4pm | `src/w4pm_cognition_{catalog,dispatch}.rs` — cognition breed catalog + dispatch skeleton |
| `wasm4pm-compat-pack` | wasm4pm-compat | `src/wasm4pm_compat_events.rs` — real `OCELEvent` emission helpers |
| `wasm4pm-facts-pack` | wasm4pm | `docs/releases/*/BREED_ALGORITHM_REGISTRY.md` — admitted-fact doc |

Every pack is wired via ggen's real `[packs]` mechanism (`ggen.toml`'s `{ path = "..." }` — not
manual file copying), and the project's own `schema/domain.ttl` adds 3 project-specific
`cnv:Command` individuals (`algorithm list`, `cognition list`, `receipt emit`) that ggen unions
with `clap-noun-verb-pack`'s own sample commands (`session login/verify`, `user create`) — no
pack modification needed to extend the CLI surface.

Verified end-to-end, live: `ggen sync run` generates real code from all 6 packs; `cargo build`
succeeds; `cargo test` passes (3 real subprocess boundary tests); all 5 CLI nouns run with real
exit codes, real JSON output, and one command (`receipt emit`) constructs and serializes a real
`wasm4pm_compat::ocel::OCELEvent`.

## Try it

```bash
ggen sync run --dry-run   # preview
ggen sync run             # generate
cargo build
cargo test                # runs the 3 generated CLI boundary proofs
./target/debug/receiptctl --help
./target/debug/receiptctl algorithm list
./target/debug/receiptctl cognition list
./target/debug/receiptctl receipt emit --sync-receipt-id abc123
./target/debug/receiptctl session login --token abc123
./target/debug/receiptctl user create --name Alice --email alice@example.com
```

## Real issues found and fixed while building this

- **`chicago-tdd-tools`'s `cli_proof` module isn't in the published crate yet** (`cli-proof`
  feature not shipped to crates.io) — worked around with a path dependency on this repo's own
  vendored copy (`../../crates/chicago-tdd-tools`), the same reason ggen's own workspace vendors
  it.
- **`clap-noun-verb`'s `--version` prints a generic `"cli <version>"` string**, not the actual
  binary name — a real framework quirk, not something a consumer controls. Corrected
  `chicago-tdd-tools-pack`'s sample ontology to assert what's actually true instead of an
  unverified assumption.
- **Unknown-subcommand parsing exits 1, not 2** — same correction.
- **`wasm4pm-algorithms-pack` and `wasm4pm-facts-pack` both define `pi:Algo_optimized_dfg` and
  `pi:Algo_streaming_log`** (facts-pack is a "verbatim copy" of algorithms-pack's data) with a
  duplicate, non-unique `pi:wasmExport "discover_dfg"` value on both. Combining the two packs in
  one project exposed this as duplicate SPARQL rows -> duplicate Rust enum variants -> a compile
  error. Fixed both packs' ontologies to use unique export names (`discover_optimized_dfg`,
  `discover_streaming_log`), keeping the two copies in sync per facts-pack's own stated intent.
